module Mdmaster.App.GoldenSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Mdmaster.App.Markdown (parseMarkdown, renderMarkdown)
import System.Directory (listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeExtension)
import Test.Hspec

corpusDir :: FilePath
corpusDir = "test/corpus"

corpusFiles :: IO [FilePath]
corpusFiles = do
  names <- listDirectory corpusDir
  pure (sort [corpusDir </> n | n <- names, takeExtension n == ".md"])

readCorpusFile :: FilePath -> IO Text
readCorpusFile file = TE.decodeUtf8 <$> BS.readFile file

writeCorpusFile :: FilePath -> Text -> IO ()
writeCorpusFile file = BS.writeFile file . TE.encodeUtf8

spec :: Spec
spec = do
  describe "round-trip fidelity" $
    it "is byte-identical for every corpus file" $ do
      files <- corpusFiles
      files `shouldSatisfy` (not . null)
      accept <- lookupEnv "MDMASTER_GOLDEN_ACCEPT"
      forM_ files $ \file -> do
        original <- readCorpusFile file
        case parseMarkdown original >>= renderMarkdown of
          Left err -> expectationFailure (file <> ": " <> T.unpack err)
          Right rendered
            | rendered == original -> pure ()
            | accept == Just "1" -> writeCorpusFile file rendered
            | otherwise ->
                expectationFailure $
                  file <> " is not canonical.\n"
                    <> "--- stored ---\n" <> T.unpack original
                    <> "--- rendered ---\n" <> T.unpack rendered
                    <> "Review the difference. If it is pure formatting, rerun with "
                    <> "MDMASTER_GOLDEN_ACCEPT=1 and commit the updated corpus."

  describe "corpus coverage" $
    it "covers frontmatter, tables, code, nested lists, footnotes, marker blocks and non-ASCII text" $ do
      files <- corpusFiles
      map (drop (length corpusDir + 1)) files
        `shouldMatchList`
          [ "existing-toc.md"
          , "footnotes.md"
          , "frontmatter.md"
          , "nested-lists.md"
          , "readme.md"
          , "tables-and-code.md"
          , "unicode.md"
          ]
