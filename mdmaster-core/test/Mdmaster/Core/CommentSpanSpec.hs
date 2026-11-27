module Mdmaster.Core.CommentSpanSpec (spec) where

import Data.Text (Text)
import Mdmaster.Core.CommentSpan
import Test.Hspec
import Text.Pandoc.Definition

spec :: Spec
spec = do
  describe "isCommentStart" $ do
    it "matches a span carrying the start class" $
      spanOf [commentStartClass] `shouldSatisfy` isCommentStart
    it "matches even with extra classes present" $
      spanOf [deletionClass, commentStartClass, "some-other"]
        `shouldSatisfy` isCommentStart
    it "does not match a span carrying only other classes" $
      spanOf [commentEndClass] `shouldNotSatisfy` isCommentStart
    it "does not match a non-span" $
      Str "comment-start" `shouldNotSatisfy` isCommentStart

  describe "isCommentEnd" $ do
    it "matches a span carrying the end class, including the nested form" $
      spanOf [commentEndClass] `shouldSatisfy` isCommentEnd
    it "matches even with extra classes present" $
      spanOf [commentEndClass, insertionClass] `shouldSatisfy` isCommentEnd
    it "does not match a comment-start span" $
      spanOf [commentStartClass] `shouldNotSatisfy` isCommentEnd
    it "does not match a non-span" $
      Emph [Str "x"] `shouldNotSatisfy` isCommentEnd

  describe "isCommentMarker" $
    it "accepts either marker and nothing else" $ do
      spanOf [commentStartClass] `shouldSatisfy` isCommentMarker
      spanOf [commentEndClass] `shouldSatisfy` isCommentMarker
      spanOf [insertionClass] `shouldNotSatisfy` isCommentMarker
      Str "x" `shouldNotSatisfy` isCommentMarker

  describe "spanTransparent" $ do
    it "is opaque for a comment marker, whose content is comment text" $ do
      spanOf [commentStartClass] `shouldNotSatisfy` spanTransparent
      spanOf [commentEndClass] `shouldNotSatisfy` spanTransparent
    it "is opaque for a deletion span, whose content the accept-all policy drops" $
      spanOf [deletionClass] `shouldNotSatisfy` spanTransparent
    it "is transparent for an insertion span, whose content survives unwrapped" $
      spanOf [insertionClass] `shouldSatisfy` spanTransparent
    it "is transparent for a plain span" $
      spanOf [] `shouldSatisfy` spanTransparent
    it "is transparent for any non-span inline" $ do
      Emph [Str "x"] `shouldSatisfy` spanTransparent
      Str "x" `shouldSatisfy` spanTransparent

  describe "enrichAttr" $ do
    let meta m = Just CommentMeta
          { cmDone = done m
          , cmParentId = lookup "parent" m
          , cmDurableId = lookup "durableId" m
          , cmDateUtc = lookup "dateUtc" m
          , cmInitials = lookup "initials" m
          }
        done m = lookup "done" m == Just "true"
        enrich :: [(Text, Text)] -> [(Text, Text)] -> (Text, [Text], [(Text, Text)])
        enrich extra m = attrOf extra `enrichAttr` meta m
        attrOf :: [(Text, Text)] -> (Text, [Text], [(Text, Text)])
        attrOf extra = ("", [], extra)

    it "passes the attribute through untouched when there is no metadata" $
      enrichAttr (attrOf [(idKey, "0")]) Nothing
        `shouldBe` ("", [], [(idKey, "0")])

    it "adds no key at all for a field the file does not carry" $
      enrich [(idKey, "0")] [] `shouldBe` ("", [], [(idKey, "0")])

    it "appends the enrichment after pandoc's own keys, in writer order" $
      enrich
        [(idKey, "0"), (authorKey, "A"), (dateKey, "2024-01-01T09:00:00Z")]
        [ ("done", "true")
        , ("parent", "9")
        , ("dateUtc", "2024-01-01T09:00:00Z")
        , ("durableId", "D1")
        , ("initials", "AL")
        ]
        `shouldBe` ( "", []
                   , [ (idKey, "0")
                     , (authorKey, "A")
                     , (dateKey, "2024-01-01T09:00:00Z")
                     , (doneKey, "true")
                     , (parentKey, "9")
                     , (dateUtcKey, "2024-01-01T09:00:00Z")
                     , (durableIdKey, "D1")
                     , (initialsKey, "AL")
                     ]
                   )

    it "writes done only when the comment is resolved" $ do
      let (_, _, kvs) = enrich [(idKey, "0")] [("done", "true")]
      lookup doneKey kvs `shouldBe` Just doneValue
      let (_, _, kvs') = enrich [(idKey, "0")] []
      lookup doneKey kvs' `shouldBe` Nothing

    it "skips only the absent keys, keeping the order of the rest" $ do
      enrich [(idKey, "0")] [("parent", "9"), ("initials", "AL")]
        `shouldBe` ("", [], [(idKey, "0"), ("parent", "9"), ("initials", "AL")])

    it "never overwrites a key already present" $
      enrich [(idKey, "0"), (parentKey, "keep")] [("parent", "drop")]
        `shouldBe` ("", [], [(idKey, "0"), (parentKey, "keep"), ("parent", "drop")])
  where
    spanOf classes = Span ("", classes :: [Text], []) [Str "x"]
