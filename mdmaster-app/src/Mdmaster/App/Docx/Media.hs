module Mdmaster.App.Docx.Media
  ( mediaDir
  , docxMedia
  , placeImages
  , unmarkImages
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, isAscii, isControl, ord)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Mdmaster.Core.DocxImage (markExtracted, unmarkExtracted)
import System.FilePath (dropExtension, takeExtension, takeFileName, (</>))
import Text.Printf (printf)
import Text.Pandoc.Definition (Inline (..), Pandoc (..), nullAttr)
import Text.Pandoc.MediaBag (MediaBag, lookupMedia, mediaContents)
import Text.Pandoc.Walk (query, walk)

mediaDir :: FilePath
mediaDir = "media"

docxMedia :: Pandoc -> MediaBag -> Map Text ByteString
docxMedia doc bag = M.fromList
  [ (target, BL.toStrict (mediaContents item))
  | target <- query imageTarget doc
  , Just item <- [lookupMedia (T.unpack target) bag]
  ]

placeImages :: FilePath -> Map Text ByteString -> Set FilePath -> Pandoc
            -> (Pandoc, [(FilePath, ByteString)])
placeImages dir media taken doc = (Pandoc meta (walk retarget blocks), files)
  where
    Pandoc meta blocks = doc

    referenced = nub [t | t <- query imageTarget doc, M.member t media]

    assigned = snd (foldl' assign (taken, []) referenced)
    assign (used, acc) target =
      let path = freePath used (inDir (mediaDir </> mediaName target))
          link = linkTo (mediaDir </> takeFileName path)
       in (S.insert path used, acc <> [(target, (path, link))])

    inDir p
      | dir == "." || null dir = p
      | otherwise              = dir </> p

    files =
      [ (path, bytes)
      | (target, (path, _)) <- assigned
      , Just bytes <- [M.lookup target media]
      ]

    assignedOf = M.fromList assigned

    retarget inline = case inline of
      Image _ alt (target, title)
        | Just (_, link) <- M.lookup target assignedOf ->
            Image (markExtracted nullAttr) alt (link, title)
      _ -> inline

mediaName :: Text -> FilePath
mediaName target = fitName "" (named (T.unpack (T.map unControl lastSegment)))
  where
    lastSegment = T.takeWhileEnd (\c -> c /= '/' && c /= '\\') target
    unControl c = if isControl c then '_' else c
    named name
      | name `elem` ["", ".", ".."] = "image"
      | otherwise                   = name

maxMediaNameBytes :: Int
maxMediaNameBytes = 255

fitName :: String -> FilePath -> FilePath
fitName tailText name
  | utf8Length whole <= maxMediaNameBytes = whole
  | utf8Length ext + utf8Length tailText < maxMediaNameBytes =
      takeUtf8 (maxMediaNameBytes - utf8Length ext - utf8Length tailText) stem
        <> tailText <> ext
  | otherwise =
      takeUtf8 (maxMediaNameBytes - utf8Length tailText) (stem <> ext) <> tailText
  where
    stem = dropExtension name
    ext = takeExtension name
    whole = stem <> tailText <> ext

utf8Length :: String -> Int
utf8Length = sum . map charBytes
  where
    charBytes c
      | n < 0x80    = 1
      | n < 0x800   = 2
      | n < 0x10000 = 3
      | otherwise   = 4
      where n = ord c

takeUtf8 :: Int -> String -> String
takeUtf8 budget = go 0
  where
    go _ [] = []
    go used (c : cs)
      | used + size > budget = []
      | otherwise            = c : go (used + size) cs
      where size = utf8Length [c]

linkTo :: FilePath -> Text
linkTo = T.pack . concatMap escape
  where
    escape c
      | not (isAscii c) || isAlphaNum c || c `elem` ("-._~!$&'*+,;=:@/" :: String) = [c]
      | otherwise = printf "%%%02X" (ord c)

imageTarget :: Inline -> [Text]
imageTarget inline = case inline of
  Image _ _ (target, _) -> [target]
  _                     -> []

unmarkImages :: Pandoc -> Pandoc
unmarkImages = walk unimage
  where
    unimage inline = case inline of
      Image attr alt target -> Image (unmarkExtracted attr) alt target
      _                     -> inline

freePath :: Set FilePath -> FilePath -> FilePath
freePath used path
  | path `S.notMember` used = path
  | otherwise               = firstFree 2
  where
    firstFree :: Int -> FilePath
    firstFree n
      | suffixed n `S.notMember` used = suffixed n
      | otherwise                     = firstFree (n + 1)
    base = takeFileName path
    folder = take (length path - length base) path
    suffixed n = folder <> fitName ("-" <> show n) base
