module Mdmaster.App.Zip.ManifestSpec (spec) where

import Data.List (nub)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian)
import Mdmaster.App.Diff (DiffHunk (..), DiffKind (..), DiffLine (..))
import Mdmaster.App.Types
import Mdmaster.App.Zip.Manifest
  ( manifestCandidates
  , manifestEntryPath
  , manifestFor
  , manifestName
  , mdmasterManifestName
  , statusWord
  )
import Mdmaster.Core.Operation (InputFormat (..), OperationId (..))
import Mdmaster.Core.Options (OptionValue (..), OptionValues (..))
import Test.Hspec

at :: UTCTime
at = UTCTime (fromGregorian 2026 11 6) (10 * 3600 + 30 * 60)

options :: OptionValues
options = OptionValues (M.fromList [("depth", VInt 3), ("mode", VEnum "update")])

changedFile :: FilePath -> BatchFile
changedFile path = DocRow
  path
  Changed
  (Just "rewritten\n")
  (Just "before\n")
  (Just [ DiffHunk 1 4
                        [ DiffLine Removed "old"
                        , DiffLine Added "new one"
                        , DiffLine Added "new two"
                        , DiffLine Context "kept"
                        ]
                    ])

unchangedFile :: FilePath -> BatchFile
unchangedFile path = DocRow
  path
  Unchanged
  (Just "same\n")
  (Just "same\n")
  (Just [])

mediaFile :: FilePath -> BatchFile
mediaFile path = MediaRow
  path
  "png"

failedFile :: FilePath -> BatchFile
failedFile path = DocRow
  path
  (Failed "boom")
  Nothing
  Nothing
  Nothing

batch :: [BatchFile] -> BatchResult
batch = batchWith options

batchWith :: OptionValues -> [BatchFile] -> BatchResult
batchWith chosen files = BatchResult
  { brId        = BatchId "b1"
  , brAt        = at
  , brOperation = OperationId "toc"
  , brOptions   = chosen
  , brInput     = MarkdownInput
  , brFiles     = files
  }

spec :: Spec
spec = do
  describe "the document's shape" $
    it "records what ran, when, with what options, and what became of each file" $
      manifestFor (batch
        [ changedFile "guide.md"
        , unchangedFile "same.md"
        , mediaFile "media/image1.png"
        , failedFile "broken.md"
        ])
        `shouldBe` T.unlines
          [ "# mdmaster batch manifest"
          , ""
          , "- Operation: toc"
          , "- Ran at: 2026-11-06T10:30:00Z"
          , "- Options: depth: 3, mode: update"
          , ""
          , "| Path | Status | Added | Removed |"
          , "|---|---|---|---|"
          , "| guide.md | changed | 2 | 1 |"
          , "| same.md | unchanged |  |  |"
          , "| media/image1.png | unchanged |  |  |"
          , "| broken.md | failed |  |  |"
          ]

  describe "the counts" $
    it "exist exactly when the file has hunks, never as a zero-change" $ do
      manifestFor (batch [unchangedFile "same.md"])
        `shouldSatisfy` T.isInfixOf "| same.md | unchanged |  |  |"
      manifestFor (batch [mediaFile "media/image1.png"])
        `shouldSatisfy` T.isInfixOf "| media/image1.png | unchanged |  |  |"

  describe "the batch id" $
    it "is written nowhere in the manifest" $
      let batchKey = T.replicate 2 "0123456789abcdef"
          b = (batch [changedFile "a.md", mediaFile "media/image1.png"])
                { brId = BatchId batchKey }
       in manifestFor b `shouldSatisfy` (not . T.isInfixOf batchKey)

  describe "the options line" $
    it "says none when the operation takes no options" $
      manifestFor (batchWith (OptionValues M.empty) [])
        `shouldSatisfy` T.isInfixOf "- Options: none"

  describe "a path carrying a pipe" $
    it "is escaped, so the row survives it" $
      manifestFor (batch [changedFile "we|ird.md"])
        `shouldSatisfy` T.isInfixOf "| we\\|ird.md | changed |"

  describe "a path carrying a backslash before the pipe" $
    it "escapes the backslash first, so the pipe stays escaped" $
      manifestFor (batch [changedFile "we\\|ird.md"])
        `shouldSatisfy` T.isInfixOf "| we\\\\\\|ird.md | changed |"

  describe "the status words" $
    it "spell every outcome a row can report" $ do
      statusWord Changed       `shouldBe` "changed"
      statusWord Unchanged     `shouldBe` "unchanged"
      statusWord (Failed "x")  `shouldBe` "failed"

  describe "the entry name" $ do
    it "claims MANIFEST.md when the batch leaves it free" $
      manifestEntryPath ["guide.md"] `shouldBe` manifestName

    it "steps aside when the batch packs its own file under the default name" $
      manifestEntryPath [manifestName, "guide.md"] `shouldBe` mdmasterManifestName

    it "tries MANIFEST.md first, then mdmaster-MANIFEST.md" $
      take 2 manifestCandidates `shouldBe` [manifestName, mdmasterManifestName]

    it "keeps walking when the batch claims more than one candidate" $
      manifestEntryPath (take 3 manifestCandidates)
        `shouldBe` (manifestCandidates !! 3)

    it "never offers the same name twice" $
      length (nub (take 100 manifestCandidates)) `shouldBe` 100
