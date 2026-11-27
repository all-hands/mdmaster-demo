module Mdmaster.App.Docx.Comments
  ( commentMetadata
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Mdmaster.App.Docx.Archive (DocxParts (..))
import Mdmaster.Core.CommentSpan (CommentMeta (..))
import Text.XML (Document, Name (..), def, parseLBS)
import Text.XML.Cursor (Cursor, ($/), ($//), attribute, element, fromDocument)

wNS, w14NS, w15NS, w16cidNS, w16cexNS :: Text
wNS      = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
w14NS    = "http://schemas.microsoft.com/office/word/2010/wordml"
w15NS    = "http://schemas.microsoft.com/office/word/2012/wordml"
w16cidNS = "http://schemas.microsoft.com/office/word/2016/wordml/cid"
w16cexNS = "http://schemas.microsoft.com/office/word/2018/wordml/cex"

named :: Text -> Text -> Name
named ns local = Name local (Just ns) Nothing

commentMetadata :: DocxParts -> Map Text CommentMeta
commentMetadata parts = M.fromList
    [ (cmtId c, entry c)
    | c <- comments
    ]
  where
    comments = fromMaybe [] (parseComments <$> dpComments parts >>= eitherToMaybe)

    extended = M.fromList
      [ (ceParaId e, e)
      | e <- fromMaybe [] (parseExtended <$> dpCommentsExtended parts >>= eitherToMaybe)
      ]

    durableOf = M.fromList
      (fromMaybe [] (parseIds <$> dpCommentsIds parts >>= eitherToMaybe))

    dateOf = M.fromList
      (fromMaybe [] (parseExtensible <$> dpCommentsExtensible parts >>= eitherToMaybe))

    ownerOf = M.fromList
      [ (pid, cmtId c) | c <- comments, pid <- cmtParaIds c ]

    entry c = CommentMeta
      { cmDone      = maybe False ceDone joined
      , cmParentId  = do
          e      <- joined
          parent <- ceParent e
          owner  <- M.lookup parent ownerOf
          if owner == cmtId c then Nothing else Just owner
      , cmDurableId = durable
      , cmDateUtc   = durable >>= \d -> M.lookup d dateOf
      , cmInitials  = cmtInitials c
      }
      where
        joinKey = listToMaybe (reverse (cmtParaIds c))
        joined  = joinKey >>= \k -> M.lookup k extended
        durable = joinKey >>= \k -> M.lookup k durableOf

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

parse :: BL.ByteString -> Either Text Document
parse lbs = case parseLBS def lbs of
  Left _    -> Left "malformed XML"
  Right doc -> Right doc

data Comment = Comment
  { cmtId       :: Text
  , cmtParaIds  :: [Text]
  , cmtInitials :: Maybe Text
  }

parseComments :: BL.ByteString -> Either Text [Comment]
parseComments lbs = do
  doc <- parse lbs
  pure $ mapMaybe toComment (fromDocument doc $// element (named wNS "comment"))
  where
    toComment c = do
      cid <- listToMaybe (attribute (named wNS "id") c)
      pure Comment
        { cmtId       = cid
        , cmtParaIds  = paraIdsOf c
        , cmtInitials = listToMaybe (attribute (named wNS "initials") c)
        }

data CommentEx = CommentEx
  { ceParaId :: Text
  , ceParent :: Maybe Text
  , ceDone   :: Bool
  }

parseExtended :: BL.ByteString -> Either Text [CommentEx]
parseExtended lbs = do
  doc <- parse lbs
  pure $ mapMaybe toEx (fromDocument doc $// element (named w15NS "commentEx"))
  where
    toEx c = do
      pid <- listToMaybe (attribute (named w15NS "paraId") c)
      pure CommentEx
        { ceParaId = pid
        , ceParent = listToMaybe (attribute (named w15NS "paraIdParent") c)
        , ceDone   = listToMaybe (attribute (named w15NS "done") c)
                       `elem` [Just "1", Just "true", Just "on"]
        }

parseIds :: BL.ByteString -> Either Text [(Text, Text)]
parseIds lbs = do
  doc <- parse lbs
  pure $ mapMaybe pair (fromDocument doc $// element (named w16cidNS "commentId"))
  where
    pair c = (,) <$> listToMaybe (attribute (named w16cidNS "paraId") c)
                 <*> listToMaybe (attribute (named w16cidNS "durableId") c)

parseExtensible :: BL.ByteString -> Either Text [(Text, Text)]
parseExtensible lbs = do
  doc <- parse lbs
  pure $ mapMaybe pair (fromDocument doc $// element (named w16cexNS "commentExtensible"))
  where
    pair c = (,) <$> listToMaybe (attribute (named w16cexNS "durableId") c)
                 <*> listToMaybe (attribute (named w16cexNS "dateUtc") c)

paraIdsOf :: Cursor -> [Text]
paraIdsOf c =
  concatMap (attribute (named w14NS "paraId")) (c $/ element (named wNS "p"))
