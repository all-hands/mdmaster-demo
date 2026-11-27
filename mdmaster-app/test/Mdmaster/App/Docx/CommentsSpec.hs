module Mdmaster.App.Docx.CommentsSpec (spec) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Mdmaster.App.Docx.Archive (DocxParts (..), docxParts)
import Mdmaster.App.Docx.Comments (commentMetadata)
import Mdmaster.Core.CommentSpan (CommentMeta (..))
import Test.Hspec

parts :: IO DocxParts
parts = do
  bytes <- BL.readFile "test/fixtures/comments.docx"
  case docxParts bytes of
    Left err -> error ("fixture unreadable: " <> T.unpack err)
    Right ps -> pure ps

meta :: IO (M.Map T.Text CommentMeta)
meta = commentMetadata <$> parts

commentsXml :: BL.ByteString -> BL.ByteString
commentsXml body =
  "<w:comments\
  \ xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\"\
  \ xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\">"
    <> body
    <> "</w:comments>"

extendedXml :: BL.ByteString -> BL.ByteString
extendedXml body =
  "<w15:commentsEx\
  \ xmlns:w15=\"http://schemas.microsoft.com/office/word/2012/wordml\">"
    <> body
    <> "</w15:commentsEx>"

spec :: Spec
spec = do
  describe "the four-part join" $ do
    it "keys metadata by the comment id pandoc reports, not by paraId" $ do
      m <- meta
      M.keys m `shouldMatchList` ["0", "1", "2", "3", "4"]

    it "resolves a reply to its parent's comment id" $ do
      m <- meta
      (cmParentId <$> M.lookup "1" m) `shouldBe` Just (Just "0")

    it "leaves a root comment with no parent" $ do
      m <- meta
      (cmParentId <$> M.lookup "0" m) `shouldBe` Just Nothing

    it "reads resolved state" $ do
      m <- meta
      map (\k -> cmDone <$> M.lookup k m) ["0", "3"]
        `shouldBe` [Just False, Just True]

    it "joins a multi-paragraph comment through its LAST paragraph's paraId" $ do
      m <- meta
      (cmDurableId <$> M.lookup "4" m) `shouldBe` Just (Just "10000004")

    it "prefers the true UTC date, which disagrees with w:date" $ do
      m <- meta
      (cmDateUtc <$> M.lookup "4" m) `shouldBe` Just (Just "2024-01-04T17:45:30.5Z")

    it "reads initials" $ do
      m <- meta
      (cmInitials <$> M.lookup "1" m) `shouldBe` Just (Just "RB")

  describe "absent parts" $ do
    it "yields no metadata at all when comments.xml is missing" $
      commentMetadata (DocxParts Nothing Nothing Nothing Nothing) `shouldBe` M.empty

    it "defaults done to False and ids to Nothing when only comments.xml exists" $ do
      ps <- parts
      let onlyComments = DocxParts (dpComments ps) Nothing Nothing Nothing
          m = commentMetadata onlyComments
      map (\k -> (\c -> (cmDone c, cmParentId c, cmDateUtc c)) <$> M.lookup k m) ["0", "3"]
        `shouldBe` [Just (False, Nothing, Nothing), Just (False, Nothing, Nothing)]

  describe "resolving a parent" $ do
    it "resolves a parent named by a paragraph that is not the parent's last" $ do
      let cs = commentsXml
            ( "<w:comment w:id=\"0\" w:initials=\"RA\">\
              \<w:p w14:paraId=\"PP01\"/><w:p w14:paraId=\"PP02\"/>\
              \<w:p w14:paraId=\"PP03\"/></w:comment>\
              \<w:comment w:id=\"1\" w:initials=\"RB\">\
              \<w:p w14:paraId=\"QQ01\"/></w:comment>" )
          ex = extendedXml
            ( "<w15:commentEx w15:paraId=\"PP03\" w15:done=\"0\"/>\
              \<w15:commentEx w15:paraId=\"QQ01\" w15:paraIdParent=\"PP01\"\
              \ w15:done=\"0\"/>" )
          m = commentMetadata (DocxParts (Just cs) (Just ex) Nothing Nothing)
      (cmParentId <$> M.lookup "1" m) `shouldBe` Just (Just "0")

    it "does not make a comment its own parent" $ do
      let cs = commentsXml
            ( "<w:comment w:id=\"0\">\
              \<w:p w14:paraId=\"PP01\"/><w:p w14:paraId=\"PP02\"/></w:comment>" )
          ex = extendedXml
            ( "<w15:commentEx w15:paraId=\"PP02\" w15:paraIdParent=\"PP01\"\
              \ w15:done=\"0\"/>" )
          m = commentMetadata (DocxParts (Just cs) (Just ex) Nothing Nothing)
      (cmParentId <$> M.lookup "0" m) `shouldBe` Just Nothing

  describe "every spelling ST_OnOff allows for done" $ do
    let doneWith value =
          let cs = commentsXml "<w:comment w:id=\"0\"><w:p w14:paraId=\"P1\"/></w:comment>"
              ex = extendedXml
                ("<w15:commentEx w15:paraId=\"P1\" w15:done=\"" <> value <> "\"/>")
           in cmDone <$> M.lookup "0" (commentMetadata (DocxParts (Just cs) (Just ex) Nothing Nothing))

    it "reads all three true spellings as resolved" $
      map doneWith ["1", "true", "on"] `shouldBe` [Just True, Just True, Just True]

    it "reads all three false spellings as unresolved" $
      map doneWith ["0", "false", "off"] `shouldBe` [Just False, Just False, Just False]

  describe "paragraphs nested in the comment body" $
    it "picks the comment's own last paragraph, not one inside a table" $ do
      let cs = commentsXml
            ( "<w:comment w:id=\"0\">\
              \<w:p w14:paraId=\"TOP1\"/><w:p w14:paraId=\"TOP2\"/>\
              \<w:tbl><w:tr><w:tc><w:p w14:paraId=\"CELL1\"/></w:tc></w:tr>\
              \</w:tbl></w:comment>" )
          ex = extendedXml "<w15:commentEx w15:paraId=\"TOP2\" w15:done=\"1\"/>"
          m = commentMetadata (DocxParts (Just cs) (Just ex) Nothing Nothing)
      (cmDone <$> M.lookup "0" m) `shouldBe` Just True

  describe "a producer that writes no paraId" $
    it "keeps the comment, with the initials the file plainly carries" $ do
      let cs = commentsXml
            "<w:comment w:id=\"7\" w:initials=\"ZZ\"><w:p/></w:comment>"
          m = commentMetadata (DocxParts (Just cs) Nothing Nothing Nothing)
      M.lookup "7" m
        `shouldBe` Just CommentMeta
          { cmDone      = False
          , cmParentId  = Nothing
          , cmDurableId = Nothing
          , cmDateUtc   = Nothing
          , cmInitials  = Just "ZZ"
          }

  describe "malformed parts" $ do
    it "yields no metadata when comments.xml is not well-formed XML" $ do
      ps <- parts
      let torn = ps { dpComments = Just "<w:comments><w:comment w:id=\"0\">" }
      commentMetadata torn `shouldBe` M.empty

    it "keeps the comments it can read when a sidecar is not well-formed XML" $ do
      ps <- parts
      let torn = ps { dpCommentsExtended = Just "<w15:commentsEx>" }
          m = commentMetadata torn
      (\c -> (cmDone c, cmParentId c, cmDurableId c, cmDateUtc c))
        <$> M.lookup "1" m
        `shouldBe` Just (False, Nothing, Just "10000001", Just "2024-01-01T10:00:00Z")
