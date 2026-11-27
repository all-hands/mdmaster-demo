module Mdmaster.Plugins.Docx
  ( docxOperation
  , runDocx
  ) where

import qualified Data.Map.Strict as M
import Mdmaster.Core.Operation
  (InputFormat (DocxInput), OpError, Operation (..), OperationId (..))
import Mdmaster.Core.Options (OptionValues)
import Mdmaster.Plugins.Docx.Collect (collect)
import Mdmaster.Plugins.Docx.Options (CommentStyle (Agent), decodeDocxOptions, docxSchema)
import Mdmaster.Plugins.Docx.Render (Placement (..), agentMeta, renderComments)
import Text.Pandoc.Definition

docxOperation :: Operation
docxOperation = Operation
  { opId          = OperationId "docx"
  , opName        = "DOCX to Markdown"
  , opDescription =
      "Convert a Word document to Markdown, including its comments. Threads and \
      \replies are kept; choose how comments are written into the output \
      \(the comment styles differ in which details they keep). Comments anchored inside a table, a figure or \
      \a footnote are carried across too."
  , opInput       = DocxInput
  , opSchema      = docxSchema
  , opRun         = runDocx
  }

runDocx :: OptionValues -> Pandoc -> Either OpError Pandoc
runDocx values input = Right (Pandoc meta' blocks')
  where
    style = decodeDocxOptions values
    (Pandoc meta blocks, threads) = collect input

    blocks' = case renderComments style threads of
      Trailing extra -> blocks <> extra
      Inline placed  -> splice placed blocks

    meta' = case (style, threads) of
      (Agent, (_ : _)) -> foldr addMeta meta (agentMeta threads)
      _                -> meta

    addMeta (k, v) (Meta m) = Meta (M.insert k (MetaString v) m)

splice :: [(Int, [Block])] -> [Block] -> [Block]
splice placed blocks = concat
  [ block : concat [extra | (i, extra) <- placed, i == ix]
  | (ix, block) <- zip [0 ..] blocks
  ]
