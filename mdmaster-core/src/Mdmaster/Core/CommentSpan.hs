module Mdmaster.Core.CommentSpan
  ( commentStartClass
  , commentEndClass
  , insertionClass
  , deletionClass
  , isCommentStart
  , isCommentEnd
  , isCommentMarker
  , spanTransparent
  , idKey
  , authorKey
  , dateKey
  , doneKey
  , parentKey
  , dateUtcKey
  , durableIdKey
  , initialsKey
  , doneValue
  , CommentMeta (..)
  , enrichAttr
  ) where

import Data.Maybe (maybeToList)
import Data.Text (Text)
import Text.Pandoc.Definition (Attr, Inline (..))

commentStartClass, commentEndClass, insertionClass, deletionClass :: Text
commentStartClass = "comment-start"
commentEndClass = "comment-end"
insertionClass = "insertion"
deletionClass = "deletion"

hasClass :: Text -> Inline -> Bool
hasClass cls inline = case inline of
  Span (_, classes, _) _ -> cls `elem` classes
  _                      -> False

isCommentStart :: Inline -> Bool
isCommentStart = hasClass commentStartClass

isCommentEnd :: Inline -> Bool
isCommentEnd = hasClass commentEndClass

isCommentMarker :: Inline -> Bool
isCommentMarker i = isCommentStart i || isCommentEnd i

spanTransparent :: Inline -> Bool
spanTransparent inline
  | isCommentMarker inline = False
  | hasClass deletionClass inline = False
  | otherwise = True

idKey, authorKey, dateKey :: Text
idKey = "comment-id"
authorKey = "author"
dateKey = "date"

doneKey, parentKey, dateUtcKey, durableIdKey, initialsKey, doneValue :: Text
doneKey = "done"
parentKey = "parent"
dateUtcKey = "dateUtc"
durableIdKey = "durableId"
initialsKey = "initials"
doneValue = "true"

data CommentMeta = CommentMeta
  { cmDone      :: Bool
  , cmParentId  :: Maybe Text
  , cmDurableId :: Maybe Text
  , cmDateUtc   :: Maybe Text
  , cmInitials  :: Maybe Text
  }
  deriving (Eq, Show)

enrichAttr :: Attr -> Maybe CommentMeta -> Attr
enrichAttr attr Nothing = attr
enrichAttr (ident, classes, kvs) (Just meta) =
  (ident, classes, kvs <> extras)
  where
    extras =
      [ (doneKey, doneValue) | cmDone meta ]
        <> concat
           [ [ (k, v) | v <- maybeToList mv ]
           | (k, mv) <-
               [ (parentKey, cmParentId meta)
               , (dateUtcKey, cmDateUtc meta)
               , (durableIdKey, cmDurableId meta)
               , (initialsKey, cmInitials meta)
               ]
           ]
