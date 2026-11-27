module Mdmaster.App.TestSupport
  ( zipOf
  , SyntheticDocx (..)
  , syntheticDocx
  , drawing
  , imageRel
  , firstOf
  , frPath
  , frStatus
  , frResult
  , frBefore
  , frHunks
  , frMedia
  ) where

import Codec.Archive.Zip (addEntryToArchive, emptyArchive, fromArchive, toEntry)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Stack (HasCallStack)
import Mdmaster.App.Diff (DiffHunk)
import Mdmaster.App.Types (BatchFile (..), FileStatus (..), rowPath)

zipOf :: [(FilePath, BL.ByteString)] -> BL.ByteString
zipOf = fromArchive . foldr add emptyArchive
  where
    add (path, content) = addEntryToArchive (toEntry path 0 content)

data SyntheticDocx = SyntheticDocx {
    sdBody          :: [Text]
  , sdDocumentRels  :: [Text]
  , sdFootnotes     :: [Text]
  , sdFootnoteRels  :: [Text]
  , sdParts         :: [(FilePath, BL.ByteString)]
  }

syntheticDocx :: SyntheticDocx -> BL.ByteString
syntheticDocx sd = zipOf $
  [ ("[Content_Types].xml", xml contentTypes)
  , ("_rels/.rels", xml (rels [rel "rIdDoc" "officeDocument" "word/document.xml"]))
  , ("word/document.xml", xml document)
  , ("word/_rels/document.xml.rels", xml (rels documentRels))
  ]
    ++ footnoteParts
    ++ sdParts sd
  where
    xml = BL.fromStrict . TE.encodeUtf8
    hasFootnotes = not (null (sdFootnotes sd))
    documentRels =
      sdDocumentRels sd
        ++ [ rel "rIdFootnotes" "footnotes" "footnotes.xml" | hasFootnotes ]
    footnoteParts
      | hasFootnotes =
          [ ("word/footnotes.xml", xml footnotes)
          , ("word/_rels/footnotes.xml.rels", xml (rels (sdFootnoteRels sd)))
          ]
      | otherwise = []
    document = T.concat
      [ "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
      , "<w:document ", namespaces, "><w:body>"
      , T.concat [ "<w:p>" <> runs <> "</w:p>" | runs <- sdBody sd ]
      , "</w:body></w:document>"
      ]
    footnotes = T.concat
      [ "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
      , "<w:footnotes ", namespaces, ">"
      , T.concat
          [ "<w:footnote w:id=\"" <> T.pack (show n) <> "\"><w:p>" <> runs <> "</w:p></w:footnote>"
          | (n, runs) <- zip [1 :: Int ..] (sdFootnotes sd)
          ]
      , "</w:footnotes>"
      ]
    contentTypes = T.concat
      [ "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
      , "<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">"
      , "<Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/>"
      , "<Default Extension=\"xml\" ContentType=\"application/xml\"/>"
      , "<Default Extension=\"png\" ContentType=\"image/png\"/>"
      , "<Override PartName=\"/word/document.xml\" ContentType=\""
      , "application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml\"/>"
      , "</Types>"
      ]
    rels entries = T.concat
      [ "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
      , "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
      , T.concat entries
      , "</Relationships>"
      ]

rel :: Text -> Text -> Text -> Text
rel relId kind target =
  "<Relationship Id=\"" <> relId <> "\" Type=\""
    <> "http://schemas.openxmlformats.org/officeDocument/2006/relationships/"
    <> kind <> "\" Target=\"" <> target <> "\"/>"

imageRel :: Text -> Text -> Text
imageRel relId = rel relId "image"

drawing :: Text -> Text
drawing relId = T.concat
  [ "<w:r><w:drawing><wp:inline>"
  , "<wp:extent cx=\"190500\" cy=\"190500\"/><wp:docPr id=\"1\" name=\"Picture\"/>"
  , "<a:graphic><a:graphicData uri=\"http://schemas.openxmlformats.org/drawingml/2006/picture\">"
  , "<pic:pic><pic:nvPicPr><pic:cNvPr id=\"1\" name=\"picture\"/><pic:cNvPicPr/></pic:nvPicPr>"
  , "<pic:blipFill><a:blip r:embed=\"", relId, "\"/></pic:blipFill>"
  , "<pic:spPr/></pic:pic>"
  , "</a:graphicData></a:graphic></wp:inline></w:drawing></w:r>"
  ]

namespaces :: Text
namespaces = T.unwords
  [ "xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\""
  , "xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\""
  , "xmlns:wp=\"http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing\""
  , "xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\""
  , "xmlns:pic=\"http://schemas.openxmlformats.org/drawingml/2006/picture\""
  ]

firstOf :: HasCallStack => String -> [a] -> a
firstOf what xs = case xs of
  x : _ -> x
  []    -> error ("expected " <> what <> ", found none")

frPath :: BatchFile -> FilePath
frPath = rowPath

frStatus :: BatchFile -> FileStatus
frStatus row = case row of
  DocRow _ status _ _ _ -> status
  MediaRow {}           -> Unchanged

frResult :: BatchFile -> Maybe Text
frResult row = case row of
  DocRow _ _ result _ _ -> result
  MediaRow {}           -> Nothing

frBefore :: BatchFile -> Maybe Text
frBefore row = case row of
  DocRow _ _ _ before _ -> before
  MediaRow {}           -> Nothing

frHunks :: BatchFile -> Maybe [DiffHunk]
frHunks row = case row of
  DocRow _ _ _ _ hunks -> hunks
  MediaRow {}          -> Nothing

frMedia :: BatchFile -> Maybe BS.ByteString
frMedia row = case row of
  MediaRow _ bytes -> Just bytes
  DocRow {}        -> Nothing
