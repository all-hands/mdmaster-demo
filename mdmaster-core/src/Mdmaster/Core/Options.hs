{-# LANGUAGE DeriveGeneric #-}

module Mdmaster.Core.Options
  ( EnumChoice (..)
  , FieldType (..)
  , OptionField (..)
  , OptionSchema (..)
  , OptionValue (..)
  , OptionValues (..)
  , OptionError (..)
  , validateOptions
  , defaultValues
  , renderDefault
  , EnumTable
  , enumField
  , decodeEnum
  , optInt
  ) where

import Control.DeepSeq (NFData)
import Data.Either (partitionEithers)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Char (isDigit)
import Text.Read (readMaybe)

data EnumChoice = EnumChoice
  { choiceValue :: Text
  , choiceLabel :: Text
  }
  deriving (Eq, Show)

data FieldType
  = IntField Int Int Int
  | EnumField [EnumChoice] Text
  deriving (Eq, Show)

data OptionField = OptionField
  { fieldKey   :: Text
  , fieldLabel :: Text
  , fieldHelp  :: Text
  , fieldType  :: FieldType
  }
  deriving (Eq, Show)

newtype OptionSchema = OptionSchema { schemaFields :: [OptionField] }
  deriving (Eq, Show)

data OptionValue
  = VInt Int
  | VEnum Text
  deriving (Eq, Show, Generic)

instance NFData OptionValue

newtype OptionValues = OptionValues (Map Text OptionValue)
  deriving (Eq, Show, Generic)

instance NFData OptionValues

data OptionError = OptionError
  { errField   :: Text
  , errMessage :: Text
  }
  deriving (Eq, Show)

validateOptions :: OptionSchema -> Map Text Text -> Either [OptionError] OptionValues
validateOptions (OptionSchema fields) raw =
  case partitionEithers (map validateOne fields) of
    ([], oks)  -> Right (OptionValues (M.fromList oks))
    (errs, _)  -> Left errs
  where
    validateOne field =
      let key = fieldKey field
          fallback = Right (key, defaultValue (fieldType field))
      in case M.lookup key raw of
           Nothing -> fallback
           Just txt
             | T.null (T.strip txt) -> fallback
             | otherwise -> fmap ((,) key) (validateField field (T.strip txt))

validateField :: OptionField -> Text -> Either OptionError OptionValue
validateField field txt = case fieldType field of
  IntField lo hi _ ->
    case (if T.all isDigit txt then readMaybe (T.unpack txt) else Nothing) of
      Just n | n >= lo && n <= hi -> Right (VInt n)
      _ -> Left . err $
        "must be a whole number between " <> tshow lo <> " and " <> tshow hi
  EnumField choices _
    | txt `elem` map choiceValue choices -> Right (VEnum txt)
    | otherwise -> Left . err $
      "must be one of: " <> T.intercalate ", " (map choiceValue choices)
  where
    err = OptionError (fieldKey field)

tshow :: Int -> Text
tshow = T.pack . show

defaultValue :: FieldType -> OptionValue
defaultValue ft = case ft of
  IntField _ _ d  -> VInt d
  EnumField _ d   -> VEnum d

renderDefault :: FieldType -> Text
renderDefault ft = case ft of
  IntField _ _ d -> tshow d
  EnumField _ d  -> d

defaultValues :: OptionSchema -> OptionValues
defaultValues (OptionSchema fields) =
  OptionValues (M.fromList [ (fieldKey f, defaultValue (fieldType f)) | f <- fields ])

optInt :: Text -> OptionValues -> Maybe Int
optInt k (OptionValues m) = case M.lookup k m of
  Just (VInt n) -> Just n
  _             -> Nothing

optEnum :: Text -> OptionValues -> Maybe Text
optEnum k (OptionValues m) = case M.lookup k m of
  Just (VEnum t) -> Just t
  _              -> Nothing

type EnumTable a = [(a, Text, Text)]

enumField :: Eq a => EnumTable a -> a -> FieldType
enumField rows def =
  EnumField [ EnumChoice wire label | (_, wire, label) <- rows ] (wireOf rows def)

decodeEnum :: Eq a => EnumTable a -> a -> Text -> OptionValues -> a
decodeEnum rows def key values =
  case optEnum key values of
    Just wire -> fromMaybe def (lookup wire [ (w, v) | (v, w, _) <- rows ])
    Nothing   -> def

wireOf :: Eq a => EnumTable a -> a -> Text
wireOf rows v = fromMaybe "" (lookup v [ (v', w) | (v', w, _) <- rows ])


