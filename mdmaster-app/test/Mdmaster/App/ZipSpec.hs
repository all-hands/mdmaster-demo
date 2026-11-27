module Mdmaster.App.ZipSpec (spec) where

import Codec.Archive.Zip (eRelativePath, fromEntry, toArchive, zEntries)
import Control.Exception (evaluate)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime (..), fromGregorian)
import Mdmaster.App.Types
import Data.Either (isLeft, isRight)
import Mdmaster.App.Zip (maxEntries, zipBatch, zipBatchChecked, zipEntryPaths)
import Mdmaster.App.Zip.Manifest (manifestEntryPath, manifestFor)
import Mdmaster.Core.Operation (InputFormat (..), OperationId (..))
import Mdmaster.Core.Options (OptionSchema (..), defaultValues)
import Test.Hspec

result :: FilePath -> FileStatus -> Maybe Text -> BatchFile
result path status content =
  DocRow path status content (Just "before\n") (Just [])

media :: FilePath -> BS.ByteString -> BatchFile
media path bytes = MediaRow path bytes

at :: UTCTime
at = UTCTime (fromGregorian 2026 11 6) (10 * 3600)

batch :: [BatchFile] -> BatchResult
batch files = BatchResult
  { brId        = BatchId "b1"
  , brAt        = at
  , brOperation = OperationId "spec"
  , brOptions   = defaultValues (OptionSchema [])
  , brInput     = MarkdownInput
  , brFiles     = files
  }

entryContents :: BL.ByteString -> [(FilePath, Text)]
entryContents bytes =
  [ (eRelativePath e, TE.decodeUtf8 (BL.toStrict (fromEntry e)))
  | e <- zEntries (toArchive bytes)
  ]

entryBytes :: BL.ByteString -> [(FilePath, BS.ByteString)]
entryBytes bytes =
  [ (eRelativePath e, BL.toStrict (fromEntry e))
  | e <- zEntries (toArchive bytes)
  ]

fileContents :: BatchResult -> [(FilePath, Text)]
fileContents b =
  [ entry
  | entry@(path, _) <- entryContents (zipBatch b)
  , path /= manifestEntryPath (zipEntryPaths b)
  ]

fileEntryBytes :: BatchResult -> [(FilePath, BS.ByteString)]
fileEntryBytes b =
  [ entry
  | entry@(path, _) <- entryBytes (zipBatch b)
  , path /= manifestEntryPath (zipEntryPaths b)
  ]

spec :: Spec
spec = do
  it "includes changed files with their transformed content" $
    fileContents (batch [result "a.md" Changed (Just "new\n")])
      `shouldBe` [("a.md", "new\n")]

  it "includes unchanged files too, so the download is the whole set" $
    map fst (fileContents (batch
      [ result "a.md" Changed (Just "new\n")
      , result "b.md" Unchanged (Just "same\n")
      ]))
      `shouldBe` ["a.md", "b.md"]

  it "omits failed files" $
    map fst (fileContents (batch
      [ result "a.md" Changed (Just "new\n")
      , result "b.md" (Failed "boom") Nothing
      ]))
      `shouldBe` ["a.md"]

  it "preserves folder structure in entry paths" $
    map fst (fileContents (batch
      [result "docs/guide/intro.md" Changed (Just "x\n")]))
      `shouldBe` ["docs/guide/intro.md"]

  it "round-trips UTF-8 content" $
    fileContents (batch [result "a.md" Changed (Just "καλημέρα\n")])
      `shouldBe` [("a.md", "καλημέρα\n")]

  it "round-trips a media file's bytes verbatim" $
    fileEntryBytes (batch
      [ result "a.md" Changed (Just "text\n")
      , media "media/image1.png" "\x89PNG\r\n\x1a\nnot-really-a-png\xff"
      ])
      `shouldBe` [ ("a.md", "text\n")
                 , ("media/image1.png", "\x89PNG\r\n\x1a\nnot-really-a-png\xff")
                 ]

  describe "the manifest entry" $ do
    it "is the only entry when every file failed" $
      let b = batch [result "a.md" (Failed "boom") Nothing]
       in entryContents (zipBatch b)
            `shouldBe` [(manifestEntryPath (zipEntryPaths b), manifestFor b)]

    it "carries the batch's manifest beside the files" $
      let b = batch [result "a.md" Changed (Just "new\n")]
       in lookup (manifestEntryPath (zipEntryPaths b)) (entryContents (zipBatch b))
            `shouldBe` Just (manifestFor b)

    it "rides first, the files following in batch order" $
      let b = batch
                [ result "a.md" Changed (Just "x\n")
                , result "b.md" Unchanged (Just "y\n")
                ]
       in map fst (entryContents (zipBatch b))
            `shouldBe` manifestEntryPath (zipEntryPaths b) : ["a.md", "b.md"]

  it "refuses an entry name a 16-bit length cannot carry" $
    let b = batch [result (replicate 70000 'a' <> ".md") Changed (Just "x\n")]
     in evaluate (BL.length (zipBatch b)) `shouldThrow` anyErrorCall

  let tiny n = batch [media ("m/" <> show i <> ".png") "x" | i <- [1 .. n :: Int]]

  it "refuses a batch with more entries than a zip without ZIP64 can carry" $
    isLeft (zipBatchChecked (tiny maxEntries)) `shouldBe` True

  it "admits a batch at the most entries a zip without ZIP64 can carry" $
    isRight (zipBatchChecked (tiny (maxEntries - 1))) `shouldBe` True

  it "builds an archive at the most entries a zip without ZIP64 can carry" $
    length (zEntries (toArchive (zipBatch (tiny (maxEntries - 1))))) `shouldBe` maxEntries

  it "appears in no entry of the archive" $
    let batchKey = T.replicate 2 "0123456789abcdef"
        b = (batch [result "a.md" Changed (Just "new\n")]) { brId = BatchId batchKey }
     in [ path | (path, content) <- entryContents (zipBatch b)
                , T.isInfixOf batchKey (T.pack path) || T.isInfixOf batchKey content ]
          `shouldBe` []

  it "is deterministic: the same batch produces the same bytes" $
    let b = batch [result "a.md" Changed (Just "new\n")]
     in zipBatch b `shouldBe` zipBatch b

  describe "zipEntryPaths" $
    it "lists the batch's files exactly as the archive packs them, manifest aside" $
      zipEntryPaths (batch
        [ result "a.md" Changed (Just "new\n")
        , result "b.md" (Failed "boom") Nothing
        , result "c.md" Unchanged (Just "same\n")
        ])
        `shouldBe` ["a.md", "c.md"]

  describe "zipEntryPaths with nothing to pack" $
    it "stays empty for a batch whose every file failed" $
      zipEntryPaths (batch [result "a.md" (Failed "boom") Nothing]) `shouldBe` []

  describe "zipEntryPaths with media" $
    it "lists media files among the entries" $
      zipEntryPaths (batch
        [ result "a.md" Changed (Just "new\n")
        , media "media/image1.png" "png"
        ])
        `shouldBe` ["a.md", "media/image1.png"]
