module Mdmaster.Web.DemoSpec (spec) where

import Control.Exception (bracket)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft)
import Data.List (find, sort)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime (..), fromGregorian)
import Mdmaster.App.Pipeline (runBatch)
import Mdmaster.App.Types
import Mdmaster.Core.Operation
import Mdmaster.Core.Options (defaultValues)
import Mdmaster.Plugins.Registry (allOperations)
import Mdmaster.Web.Config (Config (..), defaultConfig)
import Mdmaster.Web.Demo
import Mdmaster.Web.ServerSpec (appWith, contains)
import Mdmaster.Web.View.Demo (sampleFileClass, sampleOperationAttribute)
import Network.HTTP.Types (status200)
import Network.Wai (Application)
import qualified Network.Wai.Test as WaiTest
import System.Directory (createDirectory, removeDirectory, removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Posix.Temp (mkdtemp)
import Test.Hspec
import Network.Wai.Test (SResponse (..))

static :: FilePath
static = cfgStaticDir defaultConfig

shipped :: IO [DemoSet]
shipped = fromMaybe [] <$> loadDemo (Just True) static

operationOf :: DemoSet -> Operation
operationOf set =
  fromMaybe (error ("no operation " <> show (demoOperation set)))
            (find ((== demoOperation set) . opId) allOperations)

runSet :: DemoSet -> IO [BatchFile]
runSet set = do
  documents <- mapM document (demoFiles set)
  pure (brFiles (runBatch at (BatchId "demo") operation
                   (defaultValues (opSchema operation)) documents))
  where
    operation = operationOf set
    at = UTCTime (fromGregorian 2031 1 1) 0
    document path = do
      bytes <- BS.readFile (static </> demoFolder
                              </> T.unpack (unOperationId (demoOperation set)) </> path)
      pure $ Document path $ case opInput operation of
        MarkdownInput -> MarkdownSource (TE.decodeUtf8 bytes)
        DocxInput     -> DocxSource bytes

isChanged :: BatchFile -> Bool
isChanged file = case file of
  DocRow _ Changed _ _ _ -> True
  DocRow {}              -> False
  MediaRow {}            -> True

withMarkedStatic :: Bool -> (FilePath -> IO a) -> IO a
withMarkedStatic marked = bracket make discard
  where
    folder dir = dir </> demoFolder </> "toc"
    make = do
      base <- fromMaybe "/tmp" <$> lookupEnv "TMPDIR"
      dir <- mkdtemp (base </> "mdmaster-demo-")
      createDirectory (dir </> demoFolder)
      createDirectory (folder dir)
      writeFile (folder dir </> "a.md") "# A\n"
      if marked then writeFile (dir </> demoMarker) "" else pure ()
      pure dir
    discard dir = do
      if marked then removeFile (dir </> demoMarker) else pure ()
      removeFile (folder dir </> "a.md")
      removeDirectory (folder dir)
      removeDirectory (dir </> demoFolder)
      removeDirectory dir

fetched :: BS.ByteString -> Application -> IO SResponse
fetched path = WaiTest.runSession
  (WaiTest.request (WaiTest.setPath WaiTest.defaultRequest path))

sampleHrefs :: SResponse -> [BS.ByteString]
sampleHrefs = go . BL.toStrict . simpleBody
  where
    prefix = "/static/demo/"
    go haystack =
      let (_, rest) = BS.breakSubstring prefix haystack
       in if BS.null rest
            then []
            else BS8.takeWhile (/= '"') rest : go (BS.drop (BS.length prefix) rest)

spec :: Spec
spec = do
  describe "the switch" $ do
    it "is left to the marker when unset" $
      demoSwitch [] `shouldBe` Right Nothing

    it "reads yes and no in any case" $ do
      map (\v -> demoSwitch [("MDMASTER_DEMO", v)]) ["1", "TRUE", "yes", "On"]
        `shouldBe` replicate 4 (Right (Just True))
      map (\v -> demoSwitch [("MDMASTER_DEMO", v)]) ["0", "False", "no", "OFF"]
        `shouldBe` replicate 4 (Right (Just False))

    it "refuses anything else" $
      map (\v -> isLeft (demoSwitch [("MDMASTER_DEMO", v)])) ["", "maybe", "2"]
        `shouldBe` [True, True, True]

    it "is on by the marker, off without it, and the variable outranks both" $ do
      on <- withMarkedStatic True (loadDemo Nothing)
      on `shouldBe` Just [DemoSet (OperationId "toc") ["a.md"]]
      withMarkedStatic False (loadDemo Nothing) >>= (`shouldBe` Nothing)
      withMarkedStatic True (loadDemo (Just False)) >>= (`shouldBe` Nothing)
      forced <- withMarkedStatic False (loadDemo (Just True))
      forced `shouldBe` on

    it "refuses to start with no samples to offer" $
      loadDemo (Just True) "test/fixtures" `shouldThrow` anyIOException

  describe "the shipped samples" $ do
    it "cover every operation in the registry, and nothing else" $ do
      sets <- shipped
      sort (map (unOperationId . demoOperation) sets)
        `shouldBe` sort (map (unOperationId . opId) allOperations)

    it "are all accepted by the operation they are filed under" $ do
      sets <- shipped
      [ path | set <- sets, path <- demoFiles set
             , not (any (`T.isSuffixOf` T.pack path)
                        (map T.pack (inputExtensions (opInput (operationOf set))))) ]
        `shouldBe` []

    it "each come out changed, none failed, under the default options" $ do
      sets <- shipped
      rows <- concat <$> mapM runSet sets
      [ rowPath row | row <- rows, not (isChanged row) ] `shouldBe` []

    it "carry Word comments and images through the conversion" $ do
      sets <- shipped
      rows <- concat <$> mapM runSet [ s | s <- sets, opInput (operationOf s) == DocxInput ]
      length [ () | MediaRow {} <- rows ] `shouldBe` 2
      any (\row -> case row of
             DocRow _ _ (Just text) _ _ -> "Almost never?" `T.isInfixOf` text
             _                          -> False) rows
        `shouldBe` True

  describe "the page" $ do
    it "shows no notice and no samples with demo mode off" $ do
      isNothing (cfgDemo defaultConfig) `shouldBe` True
      app <- appWith defaultConfig
      res <- fetched "/" app
      contains "Demo mode" res `shouldBe` False
      contains (TE.encodeUtf8 sampleOperationAttribute) res `shouldBe` False

    it "shows the notice and a set for every operation with demo mode on" $ do
      sets <- shipped
      app <- appWith defaultConfig { cfgDemo = Just sets }
      res <- fetched "/" app
      contains "Demo mode" res `shouldBe` True
      length (sampleHrefs res) `shouldBe` sum (map (length . demoFiles) sets)
      contains (TE.encodeUtf8 ("class=\"" <> sampleFileClass <> "\"")) res `shouldBe` True

    it "links only samples the server serves" $ do
      sets <- shipped
      app <- appWith defaultConfig { cfgDemo = Just sets }
      res <- fetched "/" app
      samples <- mapM (`fetched` app) (sampleHrefs res)
      map simpleStatus samples `shouldBe` map (const status200) samples
