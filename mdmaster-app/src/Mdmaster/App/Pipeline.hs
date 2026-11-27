module Mdmaster.App.Pipeline
  ( diffContext
  , runBatch
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Mdmaster.App.Diff (diffHunks)
import Mdmaster.App.Docx (parseDocx)
import Mdmaster.App.Docx.Media (placeImages, unmarkImages)
import Mdmaster.App.Markdown (parseMarkdown, renderMarkdown, renderMarkdownFor)
import Mdmaster.App.Types
import Mdmaster.Core.Operation (InputFormat (..), OpError (..), Operation (..))
import Mdmaster.Core.Options (OptionValues)
import System.FilePath (replaceExtension, takeDirectory)
import Text.Pandoc.Definition (Pandoc)

diffContext :: Int
diffContext = 3

runBatch :: UTCTime -> BatchId -> Operation -> OptionValues -> [Document] -> BatchResult
runBatch started batchId operation values documents = BatchResult
  { brId        = batchId
  , brAt        = started
  , brOperation = opId operation
  , brOptions   = values
  , brInput     = opInput operation
  , brFiles     = fst (foldl' step ([], results) documents)
  }
  where
    results = S.fromList [outputPath (opInput operation) (docPath d) | d <- documents]

    step (files, taken) document =
      let (row, media, taken') = documentFiles taken document
       in (files <> (row : map mediaRow media), taken')

    documentFiles taken document =
      let original = case docSource document of
            MarkdownSource content -> content
            DocxSource _           -> ""
          resultPath = outputPath (opInput operation) (docPath document)
          outcome = do
            (parsed, embedded) <- parseSource (docSource document)
            let (resolved, media) = case docSource document of
                  MarkdownSource _ -> (parsed, [])
                  DocxSource _     ->
                    placeImages (takeDirectory resultPath) embedded taken parsed
            transformed <- first opErrorMessage (opRun operation values resolved)
            rendered <- case docSource document of
              MarkdownSource content -> renderMarkdownFor content parsed (unmarkImages transformed)
              DocxSource _           -> renderMarkdown (unmarkImages transformed)
            pure (rendered, media)
       in case outcome of
            Left message ->
              (DocRow resultPath (Failed message) Nothing Nothing Nothing, [], taken)
            Right (rendered, media) ->
              let diff = case opInput operation of
                    MarkdownInput -> Just (original, diffHunks diffContext original rendered)
                    DocxInput     -> Nothing
                  status = if T.lines rendered == T.lines original then Unchanged else Changed
               in ( DocRow resultPath status (Just rendered) (fmap fst diff) (fmap snd diff)
              , media
              , taken <> S.fromList (map fst media)
              )

    mediaRow (path, bytes) = MediaRow path bytes

outputPath :: InputFormat -> FilePath -> FilePath
outputPath format path = case format of
  MarkdownInput -> path
  DocxInput     -> replaceExtension path ".md"

parseSource :: DocSource -> Either Text (Pandoc, Map Text ByteString)
parseSource source = case source of
  MarkdownSource content -> (\doc -> (doc, M.empty)) <$> parseMarkdown content
  DocxSource bytes       -> parseDocx (BL.fromStrict bytes)

