module Mdmaster.Plugins.NormalizeSpec (spec) where

import Control.Exception (evaluate)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Mdmaster.Core.Operation (OpError (..))
import Mdmaster.Core.Options (OptionValues, validateOptions)
import Mdmaster.Plugins.Normalize (runNormalize)
import Mdmaster.Plugins.Normalize.Gen (SpicyDoc (..))
import Mdmaster.Plugins.Normalize.Options (normalizeSchema)
import Mdmaster.Plugins.Normalize.Smart
  (emDash, enDash, ellipsis, leftDoubleQuote, leftSingleQuote, rightDoubleQuote
  , rightSingleQuote, smartify)
import System.Timeout (timeout)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Text.Pandoc.Definition
import Text.Pandoc.Walk (query, walk)

opts :: [(Text, Text)] -> OptionValues
opts kvs = case validateOptions normalizeSchema (M.fromList kvs) of
  Left errs -> error ("bad options in test: " <> show errs)
  Right vs  -> vs

run :: [(Text, Text)] -> Pandoc -> Either OpError Pandoc
run kvs = runNormalize (opts kvs)

doc :: Text -> Pandoc
doc text = Pandoc nullMeta [Para [Str text]]

prose :: Pandoc -> Text
prose = query strText . walk dropAutolinkText
  where
    strText inline = case inline of
      Str t -> t
      _     -> ""
    dropAutolinkText inline = case inline of
      Link attr@(_, classes, _) [Str _] target
        | classes `elem` [["uri"], ["email"]] -> Link attr [] target
      _ -> inline

autolinks :: Pandoc -> [Inline]
autolinks = query $ \inline -> case inline of
  Link (_, classes, _) _ _ | classes `elem` [["uri"], ["email"]] -> [inline]
  _ -> []

unclosedOpeners :: Int -> Pandoc
unclosedOpeners n = doc (T.concat (replicate n "\"a 'b "))

finishesQuickly :: Pandoc -> IO Bool
finishesQuickly d = do
  done <- timeout 1000000 (evaluate (T.length (prose (smartify d))))
  pure (isJust done)

ch :: Char -> Text
ch = T.singleton

straightQuoteFree :: Text -> Bool
straightQuoteFree = T.all (\c -> c /= '"' && c /= '\'')

spec :: Spec
spec = do
  describe "smart off" $
    it "leaves the document's inline text exactly as it is" $
      let d = doc "he said \"hi\" -- ok... ok"
       in run [] d `shouldBe` Right d

  describe "smart on: quotes" $ do
    it "curlyises a quoted phrase" $
      run [("smart", "on")] (doc "he said \"hi\" there")
        `shouldBe` Right (doc ("he said " <> ch leftDoubleQuote <> "hi"
                                <> ch rightDoubleQuote <> " there"))

    it "turns an apostrophe after a word into a right single quote" $
      run [("smart", "on")] (doc "don't stop")
        `shouldBe` Right (doc ("don" <> ch rightSingleQuote <> "t stop"))

    it "curlyises a single-quoted phrase" $
      run [("smart", "on")] (doc "'quoted' text")
        `shouldBe` Right (doc (ch leftSingleQuote <> "quoted"
                                <> ch rightSingleQuote <> " text"))

    it "keeps an interior apostrophe inside a single-quoted phrase" $
      run [("smart", "on")] (doc "'don't' he said")
        `shouldBe` Right (doc (ch leftSingleQuote <> "don" <> ch rightSingleQuote
                                <> "t" <> ch rightSingleQuote <> " he said"))

    it "makes an unclosed single-quote opener an apostrophe, not a left quote" $
      run [("smart", "on")] (doc "'tis a trail")
        `shouldBe` Right (doc (ch rightSingleQuote <> "tis a trail"))

    it "makes a quote with nothing to open a right quote" $
      run [("smart", "on")] (doc "\" end\"")
        `shouldBe` Right (doc (ch rightDoubleQuote <> " end"
                                <> ch rightDoubleQuote))

    it "closes the double-quoted span over a single quote left open inside it" $
      run [("smart", "on")] (doc "\"say 'hi\" then 'ok'")
        `shouldBe` Right (doc (ch leftDoubleQuote <> "say " <> ch leftSingleQuote <> "hi"
                                <> ch rightDoubleQuote <> " then " <> ch leftSingleQuote
                                <> "ok" <> ch rightSingleQuote))

    it "starts a fresh double-quoted span after closing over an open single" $
      run [("smart", "on")] (doc "\"a 'b\" c \"d\" 'e'")
        `shouldBe` Right (doc (ch leftDoubleQuote <> "a " <> ch leftSingleQuote <> "b"
                                <> ch rightDoubleQuote <> " c " <> ch leftDoubleQuote <> "d"
                                <> ch rightDoubleQuote <> " " <> ch leftSingleQuote <> "e"
                                <> ch rightSingleQuote))

    it "leaves a single-quoted span that holds a double-quoted one open around it" $
      run [("smart", "on")] (doc "'a \"b\" c' d")
        `shouldBe` Right (doc (ch leftSingleQuote <> "a " <> ch leftDoubleQuote <> "b"
                                <> ch rightDoubleQuote <> " c" <> ch rightSingleQuote <> " d"))

    it "carries the quote across a nested container, which it never rewrites" $
      run [("smart", "on")]
          (Pandoc nullMeta [Para [Str "said \"", Emph [Str "hi"], Str "\" ok"]])
        `shouldBe` Right (Pandoc nullMeta
          [Para [ Str ("said " <> ch leftDoubleQuote)
                , Emph [Str "hi"]
                , Str (ch rightDoubleQuote <> " ok") ]])

    it "starts each paragraph as its own quoting context" $
      run [("smart", "on")]
          (Pandoc nullMeta [Para [Str "\"one\""], Para [Str "\"two\""]])
        `shouldBe` Right (Pandoc nullMeta
          [ Para [Str (ch leftDoubleQuote <> "one" <> ch rightDoubleQuote)]
          , Para [Str (ch leftDoubleQuote <> "two" <> ch rightDoubleQuote)] ])

    it "decides quotes around inline code but leaves the code's own alone" $
      run [("smart", "on")]
          (Pandoc nullMeta [Para [ Str "x \""
                                 , Code ("", [], []) "\"coded\""
                                 , Str "\" z" ]])
        `shouldBe` Right (Pandoc nullMeta
          [Para [ Str ("x " <> ch leftDoubleQuote)
                , Code ("", [], []) "\"coded\""
                , Str (ch rightDoubleQuote <> " z") ]])

  describe "smart on: dashes and ellipses" $ do
    it "reads two hyphens as an en dash and three as an em dash" $
      run [("smart", "on")] (doc "a -- b --- c")
        `shouldBe` Right (doc ("a " <> ch enDash <> " b " <> ch emDash <> " c"))

    it "reads four hyphens as an em dash and a leftover hyphen" $
      run [("smart", "on")] (doc "a ---- b")
        `shouldBe` Right (doc ("a " <> ch emDash <> "- b"))

    it "reads three dots as an ellipsis and four as an ellipsis and a dot" $ do
      run [("smart", "on")] (doc "wait...")
        `shouldBe` Right (doc ("wait" <> ch ellipsis))
      run [("smart", "on")] (doc "wait....")
        `shouldBe` Right (doc ("wait" <> ch ellipsis <> "."))

    it "reads the spaced form as one ellipsis" $
      run [("smart", "on")] (Pandoc nullMeta [Para [Str ".", Space, Str ".", Space, Str "."]])
        `shouldBe` Right (Pandoc nullMeta [Para [Str (ch ellipsis)]])

  describe "smart on: links whose text is their URL" $ do
    let para inlines = Pandoc nullMeta [Para inlines]
        uri u = Link ("", ["uri"], []) [Str u] (u, "")

    it "leaves a bare autolink's text alone, and still smartens around it" $
      run [("smart", "on")]
          (para [Str "see", Space, uri "https://example.com/a--b", Str "...c"])
        `shouldBe` Right
          (para [Str "see", Space, uri "https://example.com/a--b", Str (ch ellipsis <> "c")])

    it "leaves the quotes inside an angle-bracket autolink alone" $
      run [("smart", "on")] (para [uri "https://example.com/'x'"])
        `shouldBe` Right (para [uri "https://example.com/'x'"])

    it "leaves an email autolink alone" $
      let email = Link ("", ["email"], []) [Str "a--b@example.com"]
                       ("mailto:a--b@example.com", "")
       in run [("smart", "on")] (para [email]) `shouldBe` Right (para [email])

    it "leaves an explicit link written as its own URL alone" $
      let same = Link nullAttr [Str "https://example.com/a--b"]
                      ("https://example.com/a--b", "")
       in run [("smart", "on")] (para [same]) `shouldBe` Right (para [same])

    it "still smartens the text of an ordinary link" $
      run [("smart", "on")]
          (para [Link nullAttr [Str "a--b"] ("https://example.com/a--b", "")])
        `shouldBe` Right
          (para [Link nullAttr [Str ("a" <> ch enDash <> "b")] ("https://example.com/a--b", "")])

    prop "never changes an autolink" $ \(SpicyDoc d) ->
      fmap autolinks (run [("smart", "on")] d) == Right (autolinks d)

  describe "cost" $ do
    it "smartens 60 unclosed nested openers well within a second" $
      finishesQuickly (unclosedOpeners 60) `shouldReturn` True

    it "smartens 20,000 unclosed nested openers within a second" $
      finishesQuickly (unclosedOpeners 20000) `shouldReturn` True

    it "smartens a 60,000-character hyphen run within a second" $
      finishesQuickly (doc ("a" <> T.replicate 60000 "-")) `shouldReturn` True

  describe "idempotency" $ do
    prop "smartify twice equals once" $ \(SpicyDoc d) ->
      smartify (smartify d) == smartify d

    prop "a second run with smart on changes nothing" $ \(SpicyDoc d) ->
      (run [("smart", "on")] d >>= run [("smart", "on")]) == run [("smart", "on")] d

    prop "no straight quote survives in prose" $ \(SpicyDoc d) ->
      straightQuoteFree (prose (either (error . show) id (run [("smart", "on")] d)))
