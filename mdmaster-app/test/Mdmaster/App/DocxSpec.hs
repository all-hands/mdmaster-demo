module Mdmaster.App.DocxSpec (spec) where

import Codec.Archive.Zip (findEntryByPath, fromEntry, toArchive)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Mdmaster.App.Docx (parseDocx)
import Mdmaster.App.TestSupport (firstOf)
import Mdmaster.Core.CommentSpan
  (authorKey, commentEndClass, commentStartClass, dateKey, dateUtcKey, doneKey
  , doneValue, durableIdKey, idKey, initialsKey, parentKey
  )
import Test.Hspec
import Text.Pandoc.Definition
import Text.Pandoc.Walk (query)

parsed :: IO Pandoc
parsed = do
  bytes <- BL.readFile "test/fixtures/comments.docx"
  case parseDocx bytes of
    Left err     -> error ("fixture unreadable: " <> T.unpack err)
    Right (doc, _) -> pure doc

fixturePng :: IO BS.ByteString
fixturePng = do
  bytes <- BL.readFile "test/fixtures/comments.docx"
  case findEntryByPath "word/media/image1.png" (toArchive bytes) of
    Nothing   -> error "fixture lost its embedded image"
    Just entry -> pure (BL.toStrict (fromEntry entry))

starts :: Pandoc -> [[(Text, Text)]]
starts = query go
  where
    go inline = case inline of
      Span (_, cls, kvs) _ | commentStartClass `elem` cls -> [kvs]
      _                                                 -> []

closedIds :: Pandoc -> [Text]
closedIds = query go
  where
    go inline = case inline of
      Span (_, cls, kvs) _ | commentEndClass `elem` cls ->
        [v | (k, v) <- kvs, k == idKey]
      _ -> []

imageTargets :: Pandoc -> [Text]
imageTargets = query go
  where
    go inline = case inline of
      Image _ _ (target, _) -> [target]
      _                     -> []

attrOf :: Text -> [(Text, Text)] -> Maybe Text
attrOf k kvs = lookup k kvs

spec :: Spec
spec = do
  describe "reading the body" $ do
    it "surfaces every comment, which requires AllChanges" $ do
      doc <- parsed
      length (starts doc) `shouldBe` 5

    it "keeps pandoc's own comment attributes" $ do
      doc <- parsed
      let first = firstOf "a comment" (starts doc)
      map (`attrOf` first) [idKey, authorKey, dateKey]
        `shouldBe` [Just "0", Just "Reviewer A", Just "2024-01-01T09:00:00Z"]

    it "closes every range, including ids nested inside another comment-end" $ do
      doc <- parsed
      closedIds doc `shouldMatchList` ["0", "1", "2", "3", "4"]

  describe "reading the embedded image" $ do
    it "surfaces the drawing with its relationship target" $ do
      doc <- parsed
      imageTargets doc `shouldBe` ["media/image1.png"]

    it "carries the image's bytes out of the reader's media bag, keyed by that target" $ do
      bytes <- BL.readFile "test/fixtures/comments.docx"
      png <- fixturePng
      case parseDocx bytes of
        Left err      -> error ("fixture unreadable: " <> T.unpack err)
        Right (_, media) ->
          M.lookup "media/image1.png" media `shouldBe` Just png

  describe "enrichment" $ do
    it "adds the resolved thread's done flag" $ do
      doc <- parsed
      let resolved = firstOf "comment 3" [kvs | kvs <- starts doc, attrOf idKey kvs == Just "3"]
      attrOf doneKey resolved `shouldBe` Just doneValue

    it "adds the parent comment id for a reply" $ do
      doc <- parsed
      let reply = firstOf "comment 1" [kvs | kvs <- starts doc, attrOf idKey kvs == Just "1"]
      attrOf parentKey reply `shouldBe` Just "0"

    it "leaves parent absent for a root comment" $ do
      doc <- parsed
      let root = firstOf "comment 0" [kvs | kvs <- starts doc, attrOf idKey kvs == Just "0"]
      attrOf parentKey root `shouldBe` Nothing

    it "adds the true UTC date, which disagrees with pandoc's date" $ do
      doc <- parsed
      let long = firstOf "comment 4" [kvs | kvs <- starts doc, attrOf idKey kvs == Just "4"]
      (attrOf dateKey long, attrOf dateUtcKey long)
        `shouldBe` (Just "2024-01-04T12:00:00Z", Just "2024-01-04T17:45:30.5Z")

    it "adds the stable durable id and the author initials" $ do
      doc <- parsed
      let long = firstOf "comment 4" [kvs | kvs <- starts doc, attrOf idKey kvs == Just "4"]
      (attrOf durableIdKey long, attrOf initialsKey long)
        `shouldBe` (Just "10000004", Just "RA")

    it "appends the enriched keys after pandoc's own, omitting absent ones" $ do
      doc <- parsed
      let reply = firstOf "comment 1" [kvs | kvs <- starts doc, attrOf idKey kvs == Just "1"]
      map fst reply
        `shouldBe` [ idKey, authorKey, dateKey, parentKey, dateUtcKey
                   , durableIdKey, initialsKey
                   ]

  describe "failure" $
    it "reports unreadable input as a value" $
      parseDocx "not a zip" `shouldSatisfy` isLeft
