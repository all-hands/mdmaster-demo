{-# LANGUAGE DeriveGeneric #-}

module Mdmaster.Core.Operation
  ( OperationId (..)
  , OpError (..)
  , InputFormat (..)
  , inputExtensions
  , Operation (..)
  , lookupOperation
  ) where

import Control.DeepSeq (NFData)
import Data.List (find)
import Data.Text (Text)
import GHC.Generics (Generic)
import Mdmaster.Core.Options (OptionSchema, OptionValues)
import Text.Pandoc.Definition (Pandoc)

newtype OperationId = OperationId { unOperationId :: Text }
  deriving (Eq, Ord, Show, Generic)

instance NFData OperationId

newtype OpError = OpError { opErrorMessage :: Text }
  deriving (Eq, Show)

data InputFormat
  = MarkdownInput
  | DocxInput
  deriving (Eq, Show, Generic)

instance NFData InputFormat

inputExtensions :: InputFormat -> [String]
inputExtensions format = case format of
  MarkdownInput -> [".md", ".markdown"]
  DocxInput     -> [".docx"]

data Operation = Operation
  { opId          :: OperationId
  , opName        :: Text
  , opDescription :: Text
  , opInput       :: InputFormat
  , opSchema      :: OptionSchema
  , opRun         :: OptionValues -> Pandoc -> Either OpError Pandoc
  }

lookupOperation :: OperationId -> [Operation] -> Maybe Operation
lookupOperation oid = find ((== oid) . opId)
