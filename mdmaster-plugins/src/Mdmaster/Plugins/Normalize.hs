module Mdmaster.Plugins.Normalize
  ( normalizeOperation
  , runNormalize
  ) where

import Mdmaster.Core.Operation
  (InputFormat (MarkdownInput), OpError, Operation (..), OperationId (..))
import Mdmaster.Core.Options (OptionValues)
import Mdmaster.Plugins.Normalize.Options (Smart (..), decodeNormalizeOptions, normalizeSchema)
import Mdmaster.Plugins.Normalize.Smart (smartify)
import Text.Pandoc.Definition (Pandoc)

normalizeOperation :: Operation
normalizeOperation = Operation
  { opId          = OperationId "normalize"
  , opName        = "Normalize"
  , opDescription =
      "Rewrite the document in mdmaster's canonical form: parsed and \
      \re-rendered, so constructs with more than one spelling — setext \
      \headings, asterisk bullets, underscore emphasis — land on one. With \
      \Smart punctuation on, straight quotes become curly and dashes and \
      \ellipses are normalised too; with it off, the document's own \
      \typography is left alone."
  , opInput       = MarkdownInput
  , opSchema      = normalizeSchema
  , opRun         = runNormalize
  }

runNormalize :: OptionValues -> Pandoc -> Either OpError Pandoc
runNormalize values doc = case decodeNormalizeOptions values of
  SmartOff -> Right doc
  SmartOn  -> Right (smartify doc)
