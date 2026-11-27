module Mdmaster.Web.StaticSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Mdmaster.Web.Api (staticHref)
import Mdmaster.Web.Config (Config (..), defaultConfig)
import Mdmaster.Web.ServerSpec (appWith)
import Mdmaster.Web.View.Demo (sampleFileClass, sampleLoadClass, sampleOperationAttribute)
import Mdmaster.Web.View.Diff (copyClass, copyHrefAttribute, copyLabel, diffTitleClass)
import Mdmaster.Web.View.Layout
  ( acceptExtensionsAttribute, acceptListClass, acceptedFilesId, clearFilesId
  , dropOverlayId, dropZoneId, expansionNoteAttribute, expansionNoteClass
  , fileCountId, filePickerId, fileSummaryId, folderPickerId
  , maxFileBytesAttribute, maxFilesAttribute )
import Mdmaster.Web.View.Results (errorBannerTemplateId, expiryAttribute, fileFilterInputId)
import Network.HTTP.Types (status200, status404)
import Test.Hspec
import Test.Hspec.Wai

linkedAssets :: SResponse -> [BS.ByteString]
linkedAssets = go . BL.toStrict . simpleBody
  where
    prefix = TE.encodeUtf8 (staticHref "")
    go haystack =
      let (_, rest) = BS.breakSubstring prefix haystack
       in if BS.null rest
            then []
            else BS8.takeWhile (/= '"') rest
                   : go (BS.drop (BS.length prefix) rest)

at :: T.Text -> BS.ByteString
at = TE.encodeUtf8 . staticHref

datasetKey :: T.Text -> T.Text
datasetKey attr = "dataset." <> datasetName attr

datasetName :: T.Text -> T.Text
datasetName attr = case T.splitOn "-" (T.drop 5 attr) of
  [] -> ""
  (w : ws) -> w <> T.concat (map T.toTitle ws)

appJs :: IO T.Text
appJs = TE.decodeUtf8 <$> BS.readFile (cfgStaticDir defaultConfig <> "/app.js")

contentType :: SResponse -> Maybe BS.ByteString
contentType = fmap (BS8.takeWhile (/= ';')) . lookup "Content-Type" . simpleHeaders

spec :: Spec
spec = do
  clientContract
  served

clientContract :: Spec
clientContract = describe "static/app.js" $ do
  let quoted name = "\"" <> name <> "\""
      joins =
        [ ("dropOverlayId", quoted dropOverlayId)
        , ("errorBannerTemplateId", quoted errorBannerTemplateId)
        , ("fileFilterInputId", quoted fileFilterInputId)
        , ("expiryAttribute", datasetKey expiryAttribute)
        , ("copyHrefAttribute", datasetKey copyHrefAttribute)
        , ("copyLabel", quoted copyLabel)
        , ("copyClass", "." <> copyClass <> "\"")
        , ("diffTitleClass", "." <> diffTitleClass <> "\"")
        , ("dropZoneId", quoted dropZoneId)
        , ("filePickerId", quoted filePickerId)
        , ("folderPickerId", quoted folderPickerId)
        , ("fileSummaryId", quoted fileSummaryId)
        , ("fileCountId", quoted fileCountId)
        , ("clearFilesId", quoted ("#" <> clearFilesId))
        , ("acceptedFilesId", quoted acceptedFilesId)
        , ("maxFilesAttribute", quoted (datasetName maxFilesAttribute))
        , ("maxFileBytesAttribute", quoted (datasetName maxFileBytesAttribute))
        , ("acceptExtensionsAttribute", datasetKey acceptExtensionsAttribute)
        , ("expansionNoteAttribute", datasetKey expansionNoteAttribute)
        , ("acceptListClass", quoted ("." <> acceptListClass))
        , ("expansionNoteClass", quoted ("." <> expansionNoteClass))
        , ("sampleOperationAttribute", quoted ("[" <> sampleOperationAttribute <> "]"))
        , ("sampleOperationAttribute's dataset key", datasetKey sampleOperationAttribute)
        , ("sampleLoadClass", quoted ("button." <> sampleLoadClass))
        , ("sampleFileClass", quoted ("a." <> sampleFileClass))
        ]
  mapM_
    (\(name, spelled) ->
       it ("spells " <> name <> " the way the view renders it") $ do
         js <- appJs
         spelled `shouldSatisfy` (`T.isInfixOf` js))
    joins

served :: Spec
served = with (appWith defaultConfig) $ do
  describe "GET /static" $ do
    it "is linked by the page, so there is something to serve" $ do
      page <- get "/"
      liftIO $ length (linkedAssets page) `shouldSatisfy` (>= 2)

    it "serves every asset the page links" $ do
      page <- get "/"
      mapM_
        (\path -> do
           res <- get path
           liftIO $ do
             simpleStatus res `shouldBe` status200
             BL.null (simpleBody res) `shouldBe` False)
        (linkedAssets page)

    it "serves the script as JavaScript" $ do
      res <- get (at "app.js")
      liftIO $ do
        simpleStatus res `shouldBe` status200
        contentType res `shouldSatisfy`
          (`elem` map Just ["application/javascript", "text/javascript"])

    it "serves the stylesheet as CSS" $ do
      res <- get (at "app.css")
      liftIO $ do
        simpleStatus res `shouldBe` status200
        contentType res `shouldBe` Just "text/css"

    it "serves the icon as SVG" $ do
      res <- get (at "favicon.svg")
      liftIO $ do
        simpleStatus res `shouldBe` status200
        contentType res `shouldBe` Just "image/svg+xml"

    it "serves the file's bytes as they are on disk" $ do
      onDisk <- liftIO (BS.readFile (cfgStaticDir defaultConfig <> "/app.js"))
      res <- get (at "app.js")
      liftIO $ BL.toStrict (simpleBody res) `shouldBe` onDisk

    it "answers 404 for an asset that does not exist" $ do
      res <- get (at "no-such-asset.js")
      liftIO $ simpleStatus res `shouldBe` status404

    it "answers 404 for the directory itself" $ do
      res <- get (BS.dropWhileEnd (== 47) (at ""))
      liftIO $ simpleStatus res `shouldBe` status404

    it "does not serve a path that climbs out of the asset directory" $ do
      res <- get (at "../mdmaster-web.cabal")
      liftIO $ simpleStatus res `shouldNotBe` status200
