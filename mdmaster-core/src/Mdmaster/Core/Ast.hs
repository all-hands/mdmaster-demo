module Mdmaster.Core.Ast
  ( Heading (..)
  , documentHeadings
  , headingText
  , githubSlug
  , nextSlug
  , linkSafeInlines
  ) where

import Data.Char (isAlphaNum, isSpace)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Definition
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk (query, walk)

data Heading = Heading
  { hLevel   :: Int
  , hInlines :: [Inline]
  }
  deriving (Eq, Show)

documentHeadings :: Pandoc -> [Heading]
documentHeadings (Pandoc _ blocks) = query collect blocks
  where
    collect (Header lvl _ inlines) = [Heading lvl inlines]
    collect _ = []

headingText :: Heading -> Text
headingText = stringify . hInlines

githubSlug :: Text -> Text
githubSlug = T.map spaceToHyphen . T.filter keep . T.map normaliseSpace . T.toLower
  where
    normaliseSpace c = if isSpace c then ' ' else c
    keep c = isAlphaNum c || c == '_' || c == '-' || c == ' '
    spaceToHyphen c = if c == ' ' then '-' else c

nextSlug :: M.Map Text Int -> Text -> (M.Map Text Int, Text)
nextSlug seen raw
  | raw `M.notMember` seen = (M.insert raw 0 seen, raw)
  | otherwise = go (M.findWithDefault 0 raw seen + 1)
  where
    go n =
      let candidate = raw <> "-" <> T.pack (show n)
       in if candidate `M.member` seen
            then go (n + 1)
            else (M.insert candidate 0 (M.insert raw n seen), candidate)

linkSafeInlines :: [Inline] -> [Inline]
linkSafeInlines = walk (concatMap flatten)
  where
    flatten :: Inline -> [Inline]
    flatten (Link _ label _) = label
    flatten (Note _)         = []
    flatten i                = [i]
