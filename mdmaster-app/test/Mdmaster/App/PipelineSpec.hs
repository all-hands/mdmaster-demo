module Mdmaster.App.PipelineSpec (spec) where

import Codec.Archive.Zip (eRelativePath, findEntryByPath, fromEntry, toArchive, zEntries)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian)
import Mdmaster.App.Docx.Media (mediaDir)
import Mdmaster.App.Pipeline (runBatch)
import Mdmaster.App.TestSupport
  ( SyntheticDocx (..), drawing, firstOf, imageRel, syntheticDocx
  , frPath, frStatus, frResult, frBefore, frHunks, frMedia )
import Mdmaster.App.Types
import Mdmaster.App.Zip (zipBatch)
import Mdmaster.Core.DocxImage (extractedKey)
import Mdmaster.Core.Operation
import Mdmaster.Core.Options (OptionSchema (..), OptionValue (..), OptionValues (..), defaultValues)
import System.FilePath (normalise, takeDirectory, (</>))
import Test.Hspec
import Text.Pandoc.Definition

noOptions :: OptionValues
noOptions = defaultValues (OptionSchema [])

someValues :: OptionValues
someValues = OptionValues (M.fromList [("depth", VInt 2)])

started :: UTCTime
started = UTCTime (fromGregorian 2026 11 6) (10 * 3600)

stub :: Text -> (Pandoc -> Either OpError Pandoc) -> Operation
stub name f = Operation
  { opId          = OperationId name
  , opName        = name
  , opDescription = "stub"
  , opInput       = MarkdownInput
  , opSchema      = OptionSchema []
  , opRun         = const f
  }

identityOp :: Operation
identityOp = stub "id" Right

docxIdentityOp :: Operation
docxIdentityOp = (stub "docx-id" Right) { opInput = DocxInput }

docxEmptyOp :: Operation
docxEmptyOp =
  (stub "docx-empty" (\(Pandoc m _) -> Right (Pandoc m []))) { opInput = DocxInput }

docxFailOp :: Operation
docxFailOp = (stub "docx-fail" (const (Left (OpError "operation refused"))))
  { opInput = DocxInput }

appendOp :: Operation
appendOp = stub "append" (\(Pandoc m bs) -> Right (Pandoc m (bs <> [Para [Str "added"]])))

failOp :: Operation
failOp = stub "fail" (const (Left (OpError "operation refused")))

pickyOp :: Operation
pickyOp = stub "picky" $ \d@(Pandoc _ bs) -> case bs of
  (Header 6 _ _ : _) -> Left (OpError "too deep")
  _                  -> Right d

doc :: FilePath -> Text -> Document
doc path content = Document path (MarkdownSource content)

fixtureBytes :: IO BS.ByteString
fixtureBytes = BS.readFile "test/fixtures/comments.docx"

fixturePng :: IO BS.ByteString
fixturePng = do
  bytes <- fixtureBytes
  case findEntryByPath "word/media/image1.png" (toArchive (BL.fromStrict bytes)) of
    Nothing  -> error "fixture lost its embedded image"
    Just entry -> pure (BL.toStrict (fromEntry entry))

docx :: FilePath -> IO Document
docx path = do
  bytes <- fixtureBytes
  pure (Document path (DocxSource bytes))

imagePath :: FilePath
imagePath = mediaDir </> "image1.png"

suffixedPath :: FilePath
suffixedPath = mediaDir </> "image1-2.png"

reference :: FilePath -> Text
reference path = "![](" <> T.pack path <> ")"

imageLinks :: Text -> [Text]
imageLinks prose =
  [ T.takeWhile (/= ')') (T.drop 2 rest)
  | piece <- drop 1 (T.splitOn "![" prose)
  , let rest = snd (T.breakOn "](" piece)
  , not (T.null rest)
  ]

bid :: BatchId
bid = BatchId "b1"

spec :: Spec
spec = do
  describe "batch identity" $ do
    it "carries the batch id through" $
      brId (runBatch started bid identityOp noOptions []) `shouldBe` bid

    it "records the run's start time, which the zip manifest stamps" $
      brAt (runBatch started bid identityOp noOptions []) `shouldBe` started

    it "records which operation ran" $
      brOperation (runBatch started bid appendOp noOptions []) `shouldBe` OperationId "append"

    it "records the decoded option values the operation ran with" $
      brOptions (runBatch started bid identityOp someValues [])
        `shouldBe` someValues

    it "handles an empty batch" $
      brFiles (runBatch started bid identityOp noOptions []) `shouldBe` []

    it "records a Markdown operation's input format" $
      brInput (runBatch started bid identityOp noOptions []) `shouldBe` MarkdownInput

    it "preserves file order" $
      map frPath (brFiles (runBatch started bid identityOp noOptions
        [doc "a.md" "# A\n", doc "b.md" "# B\n", doc "c.md" "# C\n"]))
        `shouldBe` ["a.md", "b.md", "c.md"]

    it "leaves a Markdown result's name alone, extension included" $
      map frPath (brFiles (runBatch started bid identityOp noOptions
        [doc "notes.markdown" "# A\n", doc "docs/deep.md" "# B\n"]))
        `shouldBe` ["notes.markdown", "docs/deep.md"]

  describe "unchanged files" $ do
    it "reports Unchanged when output is byte-identical" $
      map frStatus (brFiles (runBatch started bid identityOp noOptions [doc "a.md" "# A\n"]))
        `shouldBe` [Unchanged]

    it "produces no hunks for an unchanged file" $
      map frHunks (brFiles (runBatch started bid identityOp noOptions [doc "a.md" "# A\n"]))
        `shouldBe` [Just []]

    it "still fills in the result content" $
      map frResult (brFiles (runBatch started bid identityOp noOptions [doc "a.md" "# A\n"]))
        `shouldBe` [Just "# A\n"]

  describe "changed files" $ do
    it "reports Changed and produces hunks" $
      let r = firstOf "a file result" (brFiles (runBatch started bid appendOp noOptions [doc "a.md" "# A\n"]))
       in (frStatus r, fmap null (frHunks r)) `shouldBe` (Changed, Just False)

    it "renders the transformed content" $
      let r = firstOf "a file result" (brFiles (runBatch started bid appendOp noOptions [doc "a.md" "# A\n"]))
       in frResult r `shouldBe` Just "# A\n\nadded\n"

    it "keeps the before-state the diff route re-diffs from" $
      let r = firstOf "a file result" (brFiles (runBatch started bid appendOp noOptions [doc "a.md" "# A\n"]))
       in frBefore r `shouldBe` Just "# A\n"

    it "keeps it for an unchanged file too, whose diff is simply empty" $
      map frBefore (brFiles (runBatch started bid identityOp noOptions [doc "a.md" "# A\n"]))
        `shouldBe` [Just "# A\n"]

  describe "failing files" $ do
    it "reports the operation's message" $
      map frStatus (brFiles (runBatch started bid failOp noOptions [doc "a.md" "# A\n"]))
        `shouldBe` [Failed "operation refused"]

    it "leaves the result empty" $
      map frResult (brFiles (runBatch started bid failOp noOptions [doc "a.md" "# A\n"]))
        `shouldBe` [Nothing]

    it "produces no hunks" $
      map frHunks (brFiles (runBatch started bid failOp noOptions [doc "a.md" "# A\n"]))
        `shouldBe` [Nothing]

    it "keeps no before-state, which the route would have nothing to diff" $
      map frBefore (brFiles (runBatch started bid failOp noOptions [doc "a.md" "# A\n"]))
        `shouldBe` [Nothing]

  describe "batch isolation" $
    it "one failing file does not sink the batch" $
      map frStatus (brFiles (runBatch started bid pickyOp noOptions
        [doc "ok1.md" "# A\n", doc "bad.md" "###### D\n", doc "ok2.md" "# B\n"]))
        `shouldBe` [Unchanged, Failed "too deep", Unchanged]

  describe "typography" $
    it "reads a run's Markdown with its straight quotes and dashes as written" $
      let r = firstOf "a file result" (brFiles (runBatch started bid identityOp noOptions
                             [doc "a.md" "don't -- \"stop\"\n"]))
       in frResult r `shouldBe` Just "don't -- \"stop\"\n"

  describe "parse failures" $
    it "are reported per file, not as a batch failure" $
      let results = brFiles (runBatch started bid identityOp noOptions
                      [doc "good.md" "# A\n", doc "bad.md" "---\ntitle: [unclosed\n---\n"])
       in (frStatus (firstOf "a file result" results), isFailed (frStatus (results !! 1)))
            `shouldBe` (Unchanged, True)

  describe "counts" $ do
    let batch = runBatch started bid pickyOp noOptions
          [ doc "a.md" "# A\n"
          , doc "b.md" "###### D\n"
          , doc "c.md" "# C\n"
          ]
    it "counts unchanged files" $ batchUnchangedCount batch `shouldBe` 2
    it "counts failed files" $ batchFailedCount batch `shouldBe` 1
    it "counts changed files" $ batchChangedCount batch `shouldBe` 0

  describe "large content" $
    it "handles a document with many lines" $
      let big = T.intercalate "\n" ["line " <> T.pack (show i) | i <- [1 :: Int .. 500]] <> "\n"
       in map frStatus (brFiles (runBatch started bid identityOp noOptions [doc "big.md" big]))
            `shouldBe` [Unchanged]

  describe "docx input" $ do
    it "parses a docx source instead of Markdown" $ do
      document <- docx "notes.docx"
      let batch = runBatch started bid docxIdentityOp noOptions [document]
      frStatus (firstOf "a file result" (brFiles batch)) `shouldBe` Changed

    it "produces Markdown containing the document's prose" $ do
      document <- docx "notes.docx"
      let batch = runBatch started bid docxIdentityOp noOptions [document]
      case frResult (firstOf "a file result" (brFiles batch)) of
        Nothing  -> expectationFailure "no result"
        Just out -> out `shouldSatisfy` T.isInfixOf "Intro sentence before any comment."

    it "records the input format so the view can pick its wording" $
      brInput (runBatch started bid docxIdentityOp noOptions []) `shouldBe` DocxInput

    it "names the result .md, not the .docx it was converted from" $ do
      bytes <- fixtureBytes
      let batch = runBatch started bid docxIdentityOp noOptions
                    [ Document "notes.docx" (DocxSource bytes)
                    , Document "docs/report.docx" (DocxSource bytes)
                    ]
      map frPath (brFiles batch)
        `shouldBe` ["notes.md", imagePath, "docs/report.md", "docs" </> imagePath]

    it "names a failed conversion the same way, so one file cannot break the rule" $
      map frPath (brFiles (runBatch started bid docxIdentityOp noOptions
        [Document "broken.docx" (DocxSource "not a zip")]))
        `shouldBe` ["broken.md"]

    it "reports even an empty conversion as Changed, never as Unchanged" $ do
      bytes <- fixtureBytes
      map frStatus (brFiles (runBatch started bid docxEmptyOp noOptions
        [Document "empty.docx" (DocxSource bytes)]))
        `shouldBe` [Changed, Unchanged]

    it "produces no hunks for a docx, since there is nothing to diff against" $ do
      bytes <- fixtureBytes
      let batch = runBatch started bid docxIdentityOp noOptions
                    [Document "notes.docx" (DocxSource bytes)]
      map frHunks (brFiles batch) `shouldBe` [Nothing, Nothing]

    it "keeps no before-state for a docx" $ do
      bytes <- fixtureBytes
      let batch = runBatch started bid docxIdentityOp noOptions
                    [Document "notes.docx" (DocxSource bytes)]
      map frBefore (brFiles batch) `shouldBe` [Nothing, Nothing]

    it "fails one unreadable docx without sinking the batch" $ do
      bytes <- fixtureBytes
      let batch = runBatch started bid docxIdentityOp noOptions
                    [ Document "good.docx" (DocxSource bytes)
                    , Document "bad.docx"  (DocxSource "not a zip")
                    ]
      map (isFailed . frStatus) (brFiles batch) `shouldBe` [False, False, True]

  describe "docx media" $ do
    it "carries the embedded image out as a file of the batch, bytes verbatim" $ do
      document <- docx "notes.docx"
      png <- fixturePng
      let files = brFiles (runBatch started bid docxIdentityOp noOptions [document])
      map frPath files `shouldBe` ["notes.md", imagePath]
      frMedia (files !! 1) `shouldBe` Just png

    it "builds the media file as a media row, with its path and bytes" $ do
      document <- docx "notes.docx"
      png <- fixturePng
      case brFiles (runBatch started bid docxIdentityOp noOptions [document]) !! 1 of
        MediaRow path bytes -> (path, bytes) `shouldBe` (imagePath, png)
        DocRow {}           -> expectationFailure "the image came out as a document row"

    it "keeps no before-state on a media row" $ do
      document <- docx "notes.docx"
      let mediaRow = brFiles (runBatch started bid docxIdentityOp noOptions [document]) !! 1
      frBefore mediaRow `shouldBe` Nothing

    it "rewrites the prose reference to the media path" $ do
      document <- docx "notes.docx"
      let converted = firstOf "a file result" (brFiles (runBatch started bid docxIdentityOp noOptions [document]))
      case frResult converted of
        Nothing    -> expectationFailure "no result"
        Just prose -> do
          prose `shouldSatisfy` T.isInfixOf (reference imagePath)
          prose `shouldSatisfy` not . T.isInfixOf "not extracted"
          prose `shouldSatisfy` not . T.isInfixOf ("data-" <> extractedKey)

    it "suffixes a second document's colliding image and points at the suffix" $ do
      bytes <- fixtureBytes
      let batch = runBatch started bid docxIdentityOp noOptions
                    [ Document "notes.docx" (DocxSource bytes)
                    , Document "report.docx" (DocxSource bytes)
                    ]
          files = brFiles batch
      map frPath files
        `shouldBe` ["notes.md", imagePath, "report.md", suffixedPath]
      case frResult (files !! 2) of
        Nothing    -> expectationFailure "no result"
        Just prose -> prose `shouldSatisfy` T.isInfixOf (reference suffixedPath)
      frMedia (files !! 3) `shouldBe` frMedia (files !! 1)

    it "places a nested document's image beside it, linked relative to it" $ do
      document <- docx "FolderA/level1/comments.docx"
      png <- fixturePng
      let files = brFiles (runBatch started bid docxIdentityOp noOptions [document])
      map frPath files
        `shouldBe` ["FolderA/level1/comments.md", "FolderA/level1" </> imagePath]
      frMedia (files !! 1) `shouldBe` Just png
      case frResult (firstOf "a file result" files) of
        Nothing    -> expectationFailure "no result"
        Just prose -> prose `shouldSatisfy` T.isInfixOf (reference imagePath)

    it "gives documents in different folders each their own unsuffixed image" $ do
      bytes <- fixtureBytes
      let files = brFiles (runBatch started bid docxIdentityOp noOptions
                    [ Document "A/notes.docx" (DocxSource bytes)
                    , Document "B/notes.docx" (DocxSource bytes)
                    ])
      map frPath files
        `shouldBe` ["A/notes.md", "A" </> imagePath, "B/notes.md", "B" </> imagePath]
      map frResult [files !! 0, files !! 2]
        `shouldSatisfy` all (maybe False (T.isInfixOf (reference imagePath)))

    it "suffixes the second of two documents in one folder, link and file alike" $ do
      bytes <- fixtureBytes
      let files = brFiles (runBatch started bid docxIdentityOp noOptions
                    [ Document "A/notes.docx" (DocxSource bytes)
                    , Document "A/report.docx" (DocxSource bytes)
                    ])
      map frPath files
        `shouldBe` ["A/notes.md", "A" </> imagePath, "A/report.md", "A" </> suffixedPath]
      case frResult (files !! 2) of
        Nothing    -> expectationFailure "no result"
        Just prose -> prose `shouldSatisfy` T.isInfixOf (reference suffixedPath)

    it "packs a zip in which every document's image link resolves" $ do
      bytes <- fixtureBytes
      let batch = runBatch started bid docxIdentityOp noOptions
                    [ Document "FolderA/level1/comments.docx" (DocxSource bytes)
                    , Document "FolderA/level1/other.docx" (DocxSource bytes)
                    , Document "B/notes.docx" (DocxSource bytes)
                    , Document "loose.docx" (DocxSource bytes)
                    ]
          entries = map eRelativePath (zEntries (toArchive (zipBatch batch)))
          links =
            [ normalise (takeDirectory (frPath file) </> T.unpack link)
            | file <- brFiles batch
            , Just prose <- [frResult file]
            , link <- imageLinks prose
            ]
      length links `shouldBe` 4
      filter (`notElem` entries) links `shouldBe` []

    it "keeps a root document's media path and link exactly as before" $ do
      document <- docx "notes.docx"
      let files = brFiles (runBatch started bid docxIdentityOp noOptions [document])
      map frPath files `shouldBe` ["notes.md", "media/image1.png"]
      fmap (T.isInfixOf "![](media/image1.png)") (frResult (firstOf "a file result" files))
        `shouldBe` Just True

    it "writes links that decode to the stored names of awkwardly named images" $ do
      let names = ["image%201.png", "image 2.png", "a#b.png"]
          relIds = ["rId" <> T.pack (show n) | n <- [1 :: Int ..]]
          source = syntheticDocx SyntheticDocx
            { sdBody         = map drawing (take (length names) relIds)
            , sdDocumentRels = zipWith (\r n -> imageRel r ("media/" <> n)) relIds names
            , sdFootnotes    = []
            , sdFootnoteRels = []
            , sdParts        = [("word/media/" <> T.unpack n, "png") | n <- names]
            }
          files = brFiles (runBatch started bid docxIdentityOp noOptions
                    [Document "odd.docx" (DocxSource (BL.toStrict source))])
      map frPath files
        `shouldBe` ["odd.md", "media/image%201.png", "media/image 2.png", "media/a#b.png"]
      frResult (firstOf "a file result" files) `shouldBe` Just
        "![](media/image%25201.png)\n\n![](media/image%202.png)\n\n![](media/a%23b.png)\n"

    describe "against the batch's own results" $ do
      let namedImage name = BL.toStrict $ syntheticDocx SyntheticDocx
            { sdBody         = [drawing "rId1"]
            , sdDocumentRels = [imageRel "rId1" ("media/" <> name)]
            , sdFootnotes    = []
            , sdFootnoteRels = []
            , sdParts        = [("word/media/" <> T.unpack name, "png")]
            }
          paths files = map frPath files

      it "suffixes an image that lands on the path of a later document's result" $ do
        plain <- fixtureBytes
        let files = brFiles (runBatch started bid docxIdentityOp noOptions
                      [ Document "a.docx" (DocxSource (namedImage "z.md"))
                      , Document "media/z.docx" (DocxSource plain)
                      ])
        paths files `shouldSatisfy` \ps -> length ps == length (nub ps)
        take 2 (paths files) `shouldBe` ["a.md", mediaDir </> "z-2.md"]
        case frResult (files !! 0) of
          Nothing    -> expectationFailure "no result"
          Just prose -> prose `shouldSatisfy` T.isInfixOf (reference (mediaDir </> "z-2.md"))

      it "suffixes the same image when the document it would collide with comes first" $ do
        plain <- fixtureBytes
        let files = brFiles (runBatch started bid docxIdentityOp noOptions
                      [ Document "media/z.docx" (DocxSource plain)
                      , Document "a.docx" (DocxSource (namedImage "z.md"))
                      ])
        paths files `shouldSatisfy` \ps -> length ps == length (nub ps)

    it "places an image referenced from a footnote, with that footnote's bytes" $ do
      let source = syntheticDocx SyntheticDocx
            { sdBody         =
                [ "<w:r><w:t xml:space=\"preserve\">body </w:t></w:r>\
                  \<w:r><w:footnoteReference w:id=\"1\"/></w:r>"
                , drawing "rId1"
                ]
            , sdDocumentRels = [imageRel "rId1" "media/image1.png"]
            , sdFootnotes    = ["<w:r><w:t xml:space=\"preserve\">note </w:t></w:r>" <> drawing "rId1"]
            , sdFootnoteRels = [imageRel "rId1" "media/image2.png"]
            , sdParts        = [ ("word/media/image1.png", "body image")
                               , ("word/media/image2.png", "footnote image") ]
            }
          files = brFiles (runBatch started bid docxIdentityOp noOptions
                    [Document "fn.docx" (DocxSource (BL.toStrict source))])
      [(frPath f, frMedia f) | f <- drop 1 files]
        `shouldMatchList` [ ("media/image1.png", Just "body image")
                          , ("media/image2.png", Just "footnote image") ]
      frResult (firstOf "a file result" files) `shouldBe` Just
        "body [^1]\n\n![](media/image1.png)\n\n[^1]: note ![](media/image2.png)\n"

    it "places an image whose target is rooted at the package" $ do
      let source = syntheticDocx SyntheticDocx
            { sdBody         = [drawing "rId7"]
            , sdDocumentRels = [imageRel "rId7" "/media/image3.png"]
            , sdFootnotes    = []
            , sdFootnoteRels = []
            , sdParts        = [("media/image3.png", "rooted image")]
            }
          files = brFiles (runBatch started bid docxIdentityOp noOptions
                    [Document "rooted.docx" (DocxSource (BL.toStrict source))])
      [(frPath f, frMedia f) | f <- files]
        `shouldBe` [("rooted.md", Nothing), ("media/image3.png", Just "rooted image")]
      frResult (firstOf "a file result" files) `shouldBe` Just "![](media/image3.png)\n"

    it "leaves no media rows for a document whose conversion failed" $ do
      document <- docx "broken.docx"
      let files = brFiles (runBatch started bid docxFailOp noOptions [document])
      map frPath files `shouldBe` ["broken.md"]
      map frMedia files `shouldBe` [Nothing]

    it "gives a Markdown operation no media rows at all" $
      map frPath (brFiles (runBatch started bid identityOp noOptions
        [doc "a.md" "# A\n"]))
        `shouldBe` ["a.md"]

isFailed :: FileStatus -> Bool
isFailed s = case s of
  Failed _ -> True
  _        -> False
