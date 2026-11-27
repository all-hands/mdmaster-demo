module Mdmaster.Web.RoundTripSpec (spec) where

import qualified Data.ByteString as BS
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime (..), fromGregorian)
import Mdmaster.App.Pipeline (runBatch)
import Mdmaster.App.Types
import Mdmaster.Core.Operation
import Mdmaster.Core.Options (validateOptions)
import Mdmaster.Plugins.Registry (allOperations)
import Mdmaster.Web.Config (Config (..), defaultConfig)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Test.Hspec

operation :: Text -> Operation
operation name =
  fromMaybe (error ("no operation " <> T.unpack name))
            (find ((== OperationId name) . opId) allOperations)

runOn :: Text -> [(Text, Text)] -> Text -> (FileStatus, Either Text Text)
runOn name kvs input = case brFiles batch of
  [DocRow _ status (Just rendered) _ _]   -> (status, Right rendered)
  [DocRow _ (Failed message) _ _ _]       -> (Failed message, Left message)
  other                                   -> error ("unexpected rows " <> show (length other))
  where
    op = operation name
    values = either (error . show) id (validateOptions (opSchema op) (M.fromList kvs))
    at = UTCTime (fromGregorian 2031 1 1) 0
    batch = runBatch at (BatchId "rt") op values
              [Document "a.md" (MarkdownSource input)]

out :: Text -> [(Text, Text)] -> Text -> Text
out name kvs input = either (error . T.unpack) id (snd (runOn name kvs input))

idempotent :: Text -> [(Text, Text)] -> Text -> Expectation
idempotent name kvs input =
  let once = out name kvs input in out name kvs once `shouldBe` once

isFailed :: FileStatus -> Bool
isFailed (Failed _) = True
isFailed _          = False

markdownOps :: [(Text, [[(Text, Text)]])]
markdownOps =
  [ ("toc", [ [], [("anchors", "explicit")], [("anchors", "pandoc")]
            , [("anchors", "explicit"), ("style", "numbered"), ("skipTitle", "never")] ])
  , ("normalize", [[]])
  , ("strip-comments", [[]])
  ]

samples :: IO [Text]
samples = do
  files <- walk (cfgStaticDir defaultConfig </> "demo")
  mapM (fmap TE.decodeUtf8 . BS.readFile) [f | f <- files, ".md" `T.isSuffixOf` T.pack f]
  where
    walk dir = do
      entries <- map (dir </>) <$> listDirectory dir
      concat <$> mapM (\e -> do
                          isDir <- doesDirectoryExist e
                          if isDir then walk e else pure [e]) entries

spec :: Spec
spec = do
  describe "a second run changes nothing" $ do
    it "on every shipped Markdown sample, for every operation and option set" $ do
      texts <- samples
      length texts `shouldSatisfy` (>= 10)
      sequence_
        [ idempotent name kvs text
        | text <- texts, (name, sets) <- markdownOps, kvs <- sets ]

    it "with explicit anchors (anchors are found again, not added again)" $ do
      let once = out "toc" [("anchors", "explicit")] "# T\n\n## A\n\nText.\n"
      T.count "data-mdmaster" once `shouldBe` 1
      idempotent "toc" [("anchors", "explicit")] "# T\n\n## A\n\nText.\n"

    it "on a heading that ends in #" $ do
      idempotent "normalize" [] "Rule one ##\n-----------\n\ntext\n"
      out "normalize" [] (out "normalize" [] "Rule one ##\n-----------\n")
        `shouldSatisfy` ("## Rule one \\#\\#\n" `T.isPrefixOf`)

  describe "Remove" $
    it "removes the explicit anchors an earlier run added" $ do
      let once = out "toc" [("anchors", "explicit")] "# T\n\n## A\n\nText.\n"
      out "toc" [("mode", "remove")] once `shouldBe` "# T\n\n## A\n\nText.\n"

  describe "markers" $ do
    it "find an end marker on the line straight after a list item" $ do
      let input = "# T\n\n<!-- mdmaster:toc start -->\n- old\n<!-- mdmaster:toc end -->\n\n## A\n"
          result = out "toc" [] input
      T.isInfixOf "old" result `shouldBe` False
      T.count "mdmaster:toc start" result `shouldBe` 1
      T.isInfixOf "[A](#a)" result `shouldBe` True

    it "refuse a marker pair that shares its block with other HTML" $ do
      let input = "# T\n\n<div class=\"x\">\n<!-- mdmaster:toc start -->\n- old\n\
                  \<!-- mdmaster:toc end -->\n</div>\n\n## A\n"
      fst (runOn "toc" [] input) `shouldSatisfy` isFailed

  describe "slugs" $
    it "never issue the same anchor twice" $ do
      let result = out "toc" [("skipTitle", "never")] "# Foo\n\n# Foo\n\n# Foo 1\n"
      T.count "(#foo-1)" result `shouldBe` 1

  describe "YAML front matter" $
    it "keeps comment lines and key order when the metadata is not changed" $ do
      let fm = "---\n# published: true\ntitle: Notes\nauthor: A. Person\n---\n"
      mapM_ (\name -> T.isPrefixOf fm (out name [] (fm <> "\n# Notes\n\ntext\n"))
                        `shouldBe` True)
            ["normalize", "strip-comments", "toc"]

  describe "the status" $
    it "is Unchanged when only the final newline differs" $ do
      let once = out "toc" [] "# T\n\n## A\n"
      fst (runOn "toc" [] (T.dropWhileEnd (== '\n') once)) `shouldBe` Unchanged
