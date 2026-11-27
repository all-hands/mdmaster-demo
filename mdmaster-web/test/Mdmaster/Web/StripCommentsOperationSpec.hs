module Mdmaster.Web.StripCommentsOperationSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian)
import Mdmaster.App.Pipeline (runBatch)
import Mdmaster.App.Types
import Mdmaster.Core.Marker (markerEnd, markerStart)
import Mdmaster.Core.Options (OptionValues, validateOptions)
import Mdmaster.Plugins.StripComments (stripCommentsOperation)
import Mdmaster.Plugins.StripComments.Options (stripCommentsSchema)
import Mdmaster.Web.TestSupport (firstOf, frResult, frStatus)
import Test.Hspec

bid :: BatchId
bid = BatchId "b1"

started :: UTCTime
started = UTCTime (fromGregorian 2026 11 6) (10 * 3600)

values :: Text -> OptionValues
values keep = case validateOptions stripCommentsSchema (M.fromList [("keepMarkers", keep)]) of
  Left errs -> error ("bad options in test: " <> show errs)
  Right vs  -> vs

run :: Text -> Text -> BatchFile
run keep input = firstOf "a file result" (brFiles (runBatch started bid stripCommentsOperation (values keep)
                               [Document "notes.md" (MarkdownSource input)]))

result :: BatchFile -> Text
result fr = case frResult fr of
  Nothing -> error ("unexpected failure: " <> show fr)
  Just t  -> t

marked :: Text
marked = markerStart "toc" <> "\n\n- [A](#a)\n\n" <> markerEnd "toc" <> "\n\nPara.\n"

spec :: Spec
spec = do
  describe "block comments" $ do
    it "removes a comment on its own line and closes the gap it leaves" $
      result (run "true" "para one\n\n<!-- a note -->\n\npara two\n")
        `shouldBe` "para one\n\npara two\n"

    it "reports an already clean document as Unchanged" $
      frStatus (run "true" "# Title\n\nBody text.\n") `shouldBe` Unchanged

  describe "comments inside a paragraph" $
    it "removes the comment and collapses the spaces it sat between" $
      result (run "true" "text <!-- c --> more\n")
        `shouldBe` "text more\n"

  describe "marker comments" $ do
    it "keeps the markers by default, leaving the document byte-stable" $ do
      result (run "true" marked) `shouldBe` marked
      frStatus (run "true" marked) `shouldBe` Unchanged

    it "strips the markers when keepMarkers is false" $
      result (run "false" marked) `shouldBe` "- [A](#a)\n\nPara.\n"

  describe "idempotency" $
    it "a second run over the output is byte-identical, with markers kept and stripped" $
      mapM_
        (\input -> mapM_
            (\keep ->
                let once_ = result (run keep input)
                 in result (run keep once_) `shouldBe` once_)
            ["true", "false"])
        [ "para one\n\n<!-- a note -->\n\npara two\n"
        , "text <!-- c --> more\n"
        , marked
        , "# Title\n\n" <> markerStart "toc" <> "\n\n" <> markerEnd "toc"
            <> "\n\nBody <!-- c --> text\n"
        ]
