module Mdmaster.Web.NormalizeOperationSpec (spec) where

import Control.Exception (evaluate)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian)
import Mdmaster.App.Pipeline (runBatch)
import Mdmaster.App.Types
import Mdmaster.Core.Options (OptionValues, validateOptions)
import Mdmaster.Plugins.Normalize (normalizeOperation)
import Mdmaster.Plugins.Normalize.Options (normalizeSchema)
import System.Timeout (timeout)
import Mdmaster.Web.TestSupport (firstOf, frResult, frStatus)
import Test.Hspec

bid :: BatchId
bid = BatchId "b1"

started :: UTCTime
started = UTCTime (fromGregorian 2026 11 6) (10 * 3600)

values :: Text -> OptionValues
values smart = case validateOptions normalizeSchema (M.fromList [("smart", smart)]) of
  Left errs -> error ("bad options in test: " <> show errs)
  Right vs  -> vs

run :: Text -> Text -> BatchFile
run smart input = firstOf "a file result" (brFiles (runBatch started bid normalizeOperation (values smart)
                                   [Document "notes.md" (MarkdownSource input)]))

result :: BatchFile -> Text
result fr = case frResult fr of
  Nothing -> error ("unexpected failure: " <> show fr)
  Just t  -> t

ldq, rdq, lsq, rsq, enDash, emDash, ellipsis :: Text
ldq      = "\x201C"
rdq      = "\x201D"
lsq      = "\x2018"
rsq      = "\x2019"
enDash   = "\x2013"
emDash   = "\x2014"
ellipsis = "\x2026"

samples :: [Text]
samples =
  [ "Title\n=====\n\n* one\n* two\n"
  , "he said \"hi\" -- ok... ok\n"
  , "'quoted' and don't --- fine\n"
  , "# Fine\n\nan _important_ word\n"
  , "&quot;entity&quot; and &#39;x&#39;\n"
  , "\\\"escaped\\\"\n"
  , "see https://example.com/a--b...c and <https://example.com/'x'>\n"
  , "wait... and . . . and ....\n"
  ]

finishesQuickly :: Text -> IO Bool
finishesQuickly input = do
  done <- timeout 1000000 (evaluate (T.length (result (run "on" input))))
  pure (isJust done)

unclosedOpeners :: Int -> Text
unclosedOpeners n = T.concat (replicate n "\"a 'b ") <> "\n"

spec :: Spec
spec = do
  describe "canonical form" $ do
    it "turns a setext heading into ATX" $
      result (run "off" "Title\n=====\n\nBody text.\n")
        `shouldBe` "# Title\n\nBody text.\n"

    it "turns asterisk bullets into dashes" $
      result (run "off" "* one\n* two\n")
        `shouldBe` "- one\n- two\n"

    it "turns underscore emphasis into asterisks" $
      result (run "off" "an _important_ word\n")
        `shouldBe` "an *important* word\n"

    it "reports an already-canonical document as Unchanged" $
      frStatus (run "off" "# Title\n\nBody text.\n") `shouldBe` Unchanged

  describe "smart punctuation" $ do
    it "leaves straight quotes and dashes alone when off" $
      result (run "off" "he said \"hi\" -- ok... ok\n")
        `shouldBe` "he said \"hi\" -- ok... ok\n"

    it "curlyises quotes and dashes when on" $
      result (run "on" "he said \"hi\" -- ok... ok\n")
        `shouldBe` "he said " <> ldq <> "hi" <> rdq <> " " <> enDash
                    <> " ok" <> ellipsis <> " ok\n"

    it "curlyises a lone apostrophe when on" $
      result (run "on" "don't stop\n")
        `shouldBe` "don" <> rsq <> "t stop\n"

    it "curlyises a single-quoted phrase when on" $
      result (run "on" "'quoted' text\n")
        `shouldBe` lsq <> "quoted" <> rsq <> " text\n"

    it "keeps an interior apostrophe inside a single-quoted phrase" $
      result (run "on" "'don't' he said\n")
        `shouldBe` lsq <> "don" <> rsq <> "t" <> rsq <> " he said\n"

    it "nests single quotes inside double quotes" $
      result (run "on" "he said \"a 'fine' day\" ok\n")
        `shouldBe` "he said " <> ldq <> "a " <> lsq <> "fine" <> rsq <> " day"
                    <> rdq <> " ok\n"

    it "makes an unclosed single-quote opener an apostrophe, not a left quote" $
      result (run "on" "'tis a trail\n")
        `shouldBe` rsq <> "tis a trail\n"

    it "makes an unclosed double-quote opener a left quote, the reader's fallback" $
      result (run "on" "a \"dialogue line\n")
        `shouldBe` "a " <> ldq <> "dialogue line\n"

    it "makes a quote with nothing to open a right quote" $
      result (run "on" "\" end\"\n")
        `shouldBe` rdq <> " end" <> rdq <> "\n"

    it "curlyises the quotes around inline code but leaves the code's own alone" $
      result (run "on" "use \"`don't -- ...`\" often\n")
        `shouldBe` "use " <> ldq <> "`don't -- ...`" <> rdq <> " often\n"

    it "reads two hyphens as an en dash and three as an em dash" $
      result (run "on" "a -- b --- c\n")
        `shouldBe` "a " <> enDash <> " b " <> emDash <> " c\n"

    it "reads four hyphens as an em dash and a leftover hyphen" $
      result (run "on" "a ---- b\n")
        `shouldBe` "a " <> emDash <> "- b\n"

    it "reads three dots as an ellipsis and four as an ellipsis and a dot" $ do
      result (run "on" "wait...\n") `shouldBe` "wait" <> ellipsis <> "\n"
      result (run "on" "wait....\n") `shouldBe` "wait" <> ellipsis <> ".\n"

    it "reads the spaced dot form as one ellipsis" $
      result (run "on" "wait . . . more\n") `shouldBe` "wait " <> ellipsis <> " more\n"

    it "leaves the space after an abbreviation an ordinary space" $
      result (run "on" "Mr. Smith -- e.g. this\n")
        `shouldBe` "Mr. Smith " <> enDash <> " e.g. this\n"

    it "leaves autolinks as autolinks, smartening only the prose around them" $
      result (run "on" "see https://example.com/a--b...c and <https://example.com/'x'>\n")
        `shouldBe` "see <https://example.com/a--b>" <> ellipsis
                    <> "c and <https://example.com/'x'>\n"

    it "leaves an email autolink as an autolink" $
      result (run "on" "mail <a--b@example.com> now\n")
        `shouldBe` "mail <a--b@example.com> now\n"

    it "keeps a decoded entity's quotes curly, not straight" $
      result (run "on" "&quot;entity&quot; and &#39;x&#39;\n")
        `shouldBe` ldq <> "entity" <> rdq <> " and " <> lsq <> "x" <> rsq <> "\n"

    it "escapes a literal backslash and curls the quote after it" $
      result (run "on" "\\\"escaped\\\"\n")
        `shouldBe` "\\\\" <> ldq <> "escaped\\\\" <> rdq <> "\n"

  describe "cost" $ do
    it "smartens 60 unclosed nested quote openers well within a second" $
      finishesQuickly (unclosedOpeners 60) `shouldReturn` True

    it "smartens 2,000 unclosed nested quote openers within a second" $
      finishesQuickly (unclosedOpeners 2000) `shouldReturn` True

  describe "idempotency" $
    it "a second run over the output is byte-identical, with smart on and off" $
      mapM_
        (\input -> mapM_
            (\smart ->
                let once_ = result (run smart input)
                 in result (run smart once_) `shouldBe` once_)
            ["off", "on"])
        samples
