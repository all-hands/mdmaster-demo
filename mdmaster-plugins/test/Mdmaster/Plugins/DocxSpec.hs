module Mdmaster.Plugins.DocxSpec (spec) where

import Data.List (isSubsequenceOf)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Mdmaster.Core.CommentSpan (dateKey, isCommentMarker)
import Mdmaster.Core.Marker (markerEnd)
import Mdmaster.Core.Operation (InputFormat (..), Operation (..), OperationId (..))
import Mdmaster.Core.Options
  (EnumChoice (..), FieldType (EnumField), OptionField (..), OptionValues
  , schemaFields, validateOptions)
import Mdmaster.Plugins.Docx (docxOperation, runDocx)
import Mdmaster.Plugins.Docx.Collect (Comment (..), Thread (..), collect)
import Mdmaster.Plugins.Docx.Gen (EnrichedDoc (..), end, start)
import Mdmaster.Plugins.Docx.Options (docxSchema)
import Mdmaster.Plugins.Docx.Render (inlineText, markerName)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (conjoin, counterexample, property, (===))
import Text.Pandoc.Definition
import Text.Pandoc.Walk (query)

opts :: Text -> OptionValues
opts style = case validateOptions docxSchema (M.fromList [("comments", style)]) of
  Left errs -> error ("bad options in test: " <> show errs)
  Right vs  -> vs

doc :: [Block] -> Pandoc
doc = Pandoc nullMeta

allStyles :: [Text]
allStyles =
  [ choiceValue c
  | field <- schemaFields docxSchema
  , EnumField choices _ <- [fieldType field]
  , c <- choices
  ]

sample :: Pandoc
sample = doc
  [ Header 1 nullAttr [Str "Title"]
  , Para
      [ start "0" "Reviewer A" [(dateKey, "2024-01-01T09:00:00Z")] "please clarify"
      , Str "anchored"
      , end ["0"]
      , Str " tail"
      ]
  ]

shared :: Pandoc
shared = doc
  [ Para
      [ start "0" "Reviewer A" [(dateKey, "2024-01-01T09:00:00Z")] "first note"
      , start "1" "Reviewer B" [(dateKey, "2024-01-02T09:00:00Z")] "second note"
      , Str "shared range"
      , end ["0"]
      , end ["1"]
      ]
  ]

run :: Text -> Pandoc -> [Block]
run style input = case runDocx (opts style) input of
  Left err            -> error ("runDocx failed: " <> show err)
  Right (Pandoc _ bs) -> bs

spec :: Spec
spec = do
  describe "the operation record" $ do
    it "has the docx id" $
      opId docxOperation `shouldBe` OperationId "docx"

    it "declares docx input" $
      opInput docxOperation `shouldBe` DocxInput

    it "has a non-empty name and description" $
      (opName docxOperation /= "", opDescription docxOperation /= "")
        `shouldBe` (True, True)

  describe "the body, in every mode" $ do
    it "strips comment spans from the prose" $
      mapM_ (\style -> run style sample `shouldSatisfy` all noCommentSpan) allStyles

    it "renders identical prose blocks in every mode" $
      map (\style -> take 2 (run style sample)) allStyles `shouldSatisfy` allEqual

    it "leaves the prose as the document's own, with the comment markup gone" $
      take 2 (run "review" sample)
        `shouldBe` [ Header 1 nullAttr [Str "Title"]
                   , Para [Str "anchored", Str " tail"]
                   ]

  describe "placement" $ do
    it "appends the block at the end for diff-stable" $
      last (run "diff-stable" sample) `shouldSatisfy` isMarkerEnd

    it "inserts after the annotated block for review" $
      case run "review" sample of
        (_ : _ : BlockQuote _ : _) -> pure ()
        other -> expectationFailure ("expected a quote after block 1: " <> show (length other))

  describe "two comments anchored in the same block" $ do
    it "merges both after that block, in the order their ranges opened" $
      case map blockText (run "review" shared) of
        [_, one, two] -> do
          one `shouldSatisfy` T.isInfixOf "first note"
          two `shouldSatisfy` T.isInfixOf "second note"
        other ->
          expectationFailure ("expected prose plus two quotes, got " <> show (length other))

    it "loses neither comment in either inline mode" $
      mapM_
        (\style ->
           let rendered = T.concat (map blockText (run style shared))
            in ( "first note"  `T.isInfixOf` rendered
               , "second note" `T.isInfixOf` rendered
               ) `shouldBe` (True, True))
        ["review", "archive"]

  describe "agent front matter" $
    it "records the counts in Meta, which the renderer turns into YAML" $
      case runDocx (opts "agent") sample of
        Left err -> expectationFailure (show err)
        Right (Pandoc (Meta m) _) -> M.keys m `shouldMatchList` ["comments", "threads", "unresolved"]

  describe "documents with no comments" $ do
    it "adds nothing" $
      run "review" (doc [Para [Str "plain"]]) `shouldBe` [Para [Str "plain"]]

    it "adds no front matter in agent mode either" $
      case runDocx (opts "agent") (doc [Para [Str "plain"]]) of
        Left err -> expectationFailure (show err)
        Right (Pandoc (Meta m) _) -> M.null m `shouldBe` True

  modifyMaxSuccess (const 50) $
    describe "over arbitrary documents, in every mode" $
      mapM_ (\style -> describe (T.unpack style) $ do
        prop "keeps every comment body, root and replies, in the output" $
          \(EnrichedDoc input) ->
            let output = outputText (run style input)
                bodies = [ inlineText (cBody c)
                         | t <- snd (collect input)
                         , c <- tRoot t : tReplies t ]
             in conjoin [ counterexample ("lost comment body: " <> show b)
                                          (b `T.isInfixOf` output)
                         | b <- bodies ]

        prop "keeps the document's prose, in order, as the output's blocks" $
          \(EnrichedDoc input) ->
            let Pandoc _ prose = fst (collect input)
             in counterexample "prose is not a subsequence of the output"
                  (prose `isSubsequenceOf` run style input)

        prop "leaves no comment markup anywhere in the output" $
          \(EnrichedDoc input) ->
            counterexample "a comment span survived"
              (null (query markerOf (run style input)))

        prop "returns a document untouched when it has no comments" $
          \(EnrichedDoc input) ->
            let (Pandoc meta prose, threads) = collect input
             in if null threads
                  then property (runDocx (opts style) input === Right (Pandoc meta prose))
                  else property True

        prop "is deterministic" $
          \(EnrichedDoc input) ->
            runDocx (opts style) input === runDocx (opts style) input
        ) allStyles

outputText :: [Block] -> Text
outputText = T.concat . query leaf
  where
    leaf inline = case inline of
      Str s    -> [s]
      Space    -> [" "]
      Code _ s -> [s]
      _        -> []

markerOf :: Inline -> [()]
markerOf inline = [() | isCommentMarker inline]

noCommentSpan :: Block -> Bool
noCommentSpan block = case block of
  Para inlines -> all ok inlines
  _            -> True
  where
    ok inline = case inline of
      Span _ _ -> not (isCommentMarker inline)
      _        -> True

allEqual :: Eq a => [a] -> Bool
allEqual xs = case xs of
  []       -> True
  (y : ys) -> all (== y) ys

isMarkerEnd :: Block -> Bool
isMarkerEnd block = case block of
  RawBlock _ t -> markerEnd markerName `T.isInfixOf` t
  _            -> False

blockText :: Block -> Text
blockText block = case block of
  Plain inlines      -> inlineText inlines
  Para inlines       -> inlineText inlines
  Header _ _ inlines -> inlineText inlines
  BlockQuote bs      -> T.concat (map blockText bs)
  BulletList itemss  -> T.concat (map blockText (concat itemss))
  RawBlock _ t       -> t
  _                  -> ""
