module Mdmaster.Web.DocxOperationSpec (spec) where

import Codec.Archive.Zip (findEntryByPath, fromEntry, toArchive)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (isSubsequenceOf)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian)
import Mdmaster.App.Docx (parseDocx)
import Mdmaster.App.Docx.Media (mediaDir)
import Mdmaster.App.Markdown (renderMarkdown)
import Mdmaster.App.Pipeline (runBatch)
import Mdmaster.App.Types
  (BatchId (..), BatchResult (..), DocSource (..), Document (..))
import Mdmaster.Core.CommentSpan
  ( authorKey
  , commentEndClass
  , commentStartClass
  , idKey
  , isCommentMarker
  , isCommentStart
  )
import Mdmaster.Core.Options
  (EnumChoice (..), FieldType (EnumField), OptionField (..), OptionValues
  , schemaFields, validateOptions)
import Mdmaster.Plugins.Docx (docxOperation, runDocx)
import Mdmaster.Plugins.Docx.Collect
  (Comment (..), Thread (..), collect, placeholderNoteTail)
import Mdmaster.Plugins.Docx.Options (docxSchema)
import Mdmaster.Web.TestSupport (docxFixture, firstOf, frMedia, frPath, frResult)
import System.FilePath ((</>))
import Test.Hspec
import Text.Pandoc.Definition
import Text.Pandoc.Walk (query)

parsed :: IO Pandoc
parsed = do
  bytes <- BL.readFile docxFixture
  case parseDocx bytes of
    Left err     -> error ("fixture unreadable: " <> T.unpack err)
    Right (doc, _) -> pure doc

opts :: Text -> OptionValues
opts style = case validateOptions docxSchema (M.fromList [("comments", style)]) of
  Left errs -> error ("bad options in test: " <> show errs)
  Right vs  -> vs

allStyles :: [Text]
allStyles =
  [ choiceValue c
  | field <- schemaFields docxSchema
  , EnumField choices _ <- [fieldType field]
  , c <- choices
  ]

runStyle :: Text -> Pandoc -> Pandoc
runStyle style input = case runDocx (opts style) input of
  Left err  -> error ("runDocx failed: " <> show err)
  Right out -> out

blocksOf :: Pandoc -> [Block]
blocksOf (Pandoc _ blocks) = blocks

commentStartIds :: Pandoc -> [Text]
commentStartIds = query go
  where
    go inline = case inline of
      Span (_, _, kvs) _ | isCommentStart inline -> [v | (k, v) <- kvs, k == idKey]
      _                                            -> []

commentSpanClasses :: Pandoc -> [Text]
commentSpanClasses = query go
  where
    go inline = case inline of
      Span (_, cls, _) _ ->
        [c | c <- cls, isCommentMarker (Span ("", [c], []) [])]
      _                  -> []

harvestedIds :: [Thread] -> [Text]
harvestedIds threads =
  [cId c | t <- threads, c <- tRoot t : tReplies t]

documentText :: Pandoc -> Text
documentText doc = T.concat (query rawChunk doc <> query inlineChunk doc)
  where
    rawChunk block = case block of
      RawBlock _ t -> [t]
      _            -> []
    inlineChunk inline = case inline of
      Str s     -> [s]
      Space     -> [" "]
      SoftBreak -> [" "]
      LineBreak -> [" "]
      Code _ s  -> [s]
      _         -> []

metaStrings :: Pandoc -> [(Text, Text)]
metaStrings (Pandoc (Meta m) _) = [(k, v) | (k, MetaString v) <- M.toList m]

blockKind :: Block -> String
blockKind block = case block of
  Plain _      -> "Plain"
  Para _       -> "Para"
  Header{}     -> "Header"
  BlockQuote _ -> "BlockQuote"
  BulletList _ -> "BulletList"
  RawBlock _ _ -> "RawBlock"
  Div _ _      -> "Div"
  _            -> "other"

commentPhrases :: [Text]
commentPhrases =
  [ "please clarify this phrase"
  , "agreed, rewording now"
  , "separate thread on the same range"
  , "fixed in revision two"
  , "Third and final paragraph"
  ]

imagePath :: FilePath
imagePath = mediaDir </> "image1.png"

imageReference :: Text
imageReference = "![](" <> T.pack imagePath <> ")"

fixtureBytes :: IO BS.ByteString
fixtureBytes = BS.readFile docxFixture

fixturePng :: IO BS.ByteString
fixturePng = do
  bytes <- BL.readFile docxFixture
  case findEntryByPath "word/media/image1.png" (toArchive bytes) of
    Nothing    -> error "fixture lost its embedded image"
    Just entry -> pure (BL.toStrict (fromEntry entry))

docxBatch :: BS.ByteString -> BatchResult
docxBatch bytes =
  runBatch started (BatchId "docx-spec") docxOperation (opts "review")
    [Document "notes.docx" (DocxSource bytes)]

started :: UTCTime
started = UTCTime (fromGregorian 2026 11 6) (10 * 3600)

commented :: Text -> Text -> Block
commented cid word = Para
  [ Span ("", [commentStartClass], [(idKey, cid), (authorKey, "A")]) [Str "note"]
  , Str word
  , Span ("", [commentEndClass], [(idKey, cid)]) []
  ]

tableAndFootnote :: Pandoc
tableAndFootnote = Pandoc nullMeta
  [ Para [Str "ordinary", Note [commented "note" "footnoted"]]
  , Table nullAttr (Caption Nothing []) []
      (TableHead nullAttr [])
      [ TableBody nullAttr (RowHeadColumns 0) []
          [Row nullAttr [Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1)
                          [commented "cell" "tabled"]]]
      ]
      (TableFoot nullAttr [])
  ]

interruptedList :: Pandoc
interruptedList = Pandoc nullMeta
  [ OrderedList (1, Decimal, Period) [[Plain [Str "one"]]]
  , Para
      [ Span ("", [commentStartClass], [(idKey, "0"), (authorKey, "A")]) [Str "note"]
      , Str "interruption"
      , Span ("", [commentEndClass], [(idKey, "0")]) []
      ]
  , OrderedList (2, Decimal, Period) [[Plain [Str "two"]], [Plain [Str "three"]]]
  ]

listMarkers :: Text -> [Text]
listMarkers rendered =
  [ marker
  | line <- T.lines rendered
  , (marker, rest) <- [T.breakOn "." line]
  , not (T.null marker)
  , T.all (`elem` ("0123456789" :: String)) marker
  , " " `T.isPrefixOf` T.drop 1 rest
  ]

spec :: Spec
spec = do
  describe "a numbered list interrupted mid-way" $
    it "counts through the interruption instead of restarting, in every mode" $
      mapM_
        (\style ->
           case renderMarkdown (runStyle style interruptedList) of
             Left err       -> expectationFailure (T.unpack err)
             Right rendered -> listMarkers rendered `shouldBe` ["1", "2", "3"])
        allStyles

  describe "harvesting a real document" $ do
    it "accounts for every comment span the reader emits" $ do
      doc <- parsed
      let (_, threads) = collect doc
      harvestedIds threads `shouldMatchList` commentStartIds doc

    it "accounts for them in a table cell and a footnote too" $ do
      let (_, threads) = collect tableAndFootnote
      harvestedIds threads `shouldMatchList` commentStartIds tableAndFootnote

    it "anchors those two on the words their ranges cover" $ do
      let (_, threads) = collect tableAndFootnote
      map (\t -> (cId (tRoot t), tBlockIndex t, tAnchor t)) threads
        `shouldBe` [("note", 0, [Str "footnoted"]), ("cell", 1, [Str "tabled"])]

    it "groups the five comments into four threads, the reply under its root" $ do
      doc <- parsed
      let (_, threads) = collect doc
      map (\t -> (cId (tRoot t), map cId (tReplies t))) threads
        `shouldBe` [("0", ["1"]), ("2", []), ("3", []), ("4", [])]

  describe "the prose, in every mode" $ do
    it "keeps the document's own blocks, unchanged and in order, whatever the mode" $ do
      doc <- parsed
      let prose = blocksOf (fst (collect doc))
      prose `shouldSatisfy` (not . null)
      mapM_
        (\style -> blocksOf (runStyle style doc) `shouldSatisfy` (prose `isSubsequenceOf`))
        allStyles

    it "leaves no comment span behind, in any mode" $ do
      doc <- parsed
      commentSpanClasses doc `shouldSatisfy` (not . null)
      mapM_ (\style -> commentSpanClasses (runStyle style doc) `shouldBe` []) allStyles

  describe "the comments, in every mode" $
    it "writes all five out, whichever mode is chosen" $ do
      doc <- parsed
      mapM_
        (\style ->
           let rendered = documentText (runStyle style doc)
            in mapM_ (\phrase -> rendered `shouldSatisfy` T.isInfixOf phrase) commentPhrases)
        allStyles

  describe "two comments anchored in the same paragraph" $
    it "keeps the paragraph whole and stacks both notes after it" $ do
      doc <- parsed
      case map blockKind (blocksOf (runStyle "review" doc)) of
        ["Para", "BlockQuote", "BlockQuote", "Para", "BlockQuote", "BlockQuote", "Para"]
          -> pure ()
        other -> expectationFailure ("unexpected block shape: " <> show other)

  describe "agent front matter over the real document" $
    it "counts five comments in four threads, three of them unresolved" $ do
      doc <- parsed
      metaStrings (runStyle "agent" doc)
        `shouldBe` [("comments", "5"), ("threads", "4"), ("unresolved", "3")]

  describe "the embedded image, through the pipeline" $ do
    it "extracts the image as a file of the batch, bytes verbatim" $ do
      bytes <- fixtureBytes
      png <- fixturePng
      let files = brFiles (docxBatch bytes)
      map frPath files `shouldBe` ["notes.md", imagePath]
      frMedia (files !! 1) `shouldBe` Just png

    it "rewrites the reference to the media path, with no placeholder left" $ do
      bytes <- fixtureBytes
      let converted = firstOf "a file result" (brFiles (docxBatch bytes))
      case frResult converted of
        Nothing    -> expectationFailure "no result"
        Just prose -> do
          prose `shouldSatisfy` T.isInfixOf imageReference
          prose `shouldSatisfy` (not . T.isInfixOf placeholderNoteTail)

    it "spells the reference identically in every comment style" $ do
      bytes <- fixtureBytes
      png <- fixturePng
      mapM_
        (\style ->
           let batch = runBatch started (BatchId "docx-spec") docxOperation (opts style)
                         [Document "notes.docx" (DocxSource bytes)]
               files = brFiles batch
            in case frResult (firstOf "a file result" files) of
                 Nothing -> expectationFailure "no result"
                 Just prose -> do
                   prose `shouldSatisfy` T.isInfixOf imageReference
                   frMedia (files !! 1) `shouldBe` Just png)
        allStyles
