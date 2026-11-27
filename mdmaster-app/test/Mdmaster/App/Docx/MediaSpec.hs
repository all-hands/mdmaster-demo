module Mdmaster.App.Docx.MediaSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Mdmaster.App.Docx.Media (docxMedia, mediaDir, placeImages, unmarkImages)
import Mdmaster.App.Markdown (parseMarkdown, renderMarkdown)
import Mdmaster.Core.DocxImage
  (extractedKey, extractedValue, isExtracted)
import Data.List (nub)
import System.FilePath ((</>), takeExtension, takeFileName)
import Test.Hspec
import Text.Pandoc.Definition
import Text.Pandoc.MediaBag (MediaBag, insertMedia)
import Text.Pandoc.Walk (query)

bagOf :: [(Text, BS.ByteString)] -> MediaBag
bagOf = foldl' add mempty
  where
    add bag (target, bytes) =
      insertMedia (T.unpack target) Nothing (BL.fromStrict bytes) bag

docOf :: [Inline] -> Pandoc
docOf inlines = Pandoc nullMeta [Para inlines]

image :: Text -> Attr -> Inline
image target attr = Image attr [Str "alt"] (target, "fig")

firstImage :: Pandoc -> Inline
firstImage p = case [i | i <- query (: []) p, isImage i] of
  (i : _) -> i
  []      -> error "document has no image inline"
  where
    isImage i = case i of
      Image {} -> True
      _        -> False

imageTargets :: Pandoc -> [Text]
imageTargets = query go
  where
    go inline = case inline of
      Image _ _ (target, _) -> [target]
      _                     -> []

spec :: Spec
spec = do
  describe "docxMedia" $ do
    it "keys the bytes by the target the document's images spell" $
      docxMedia (docOf [image "media/pic.png" nullAttr])
                (bagOf [("media/pic.png", "bytes")])
        `shouldBe` M.fromList [("media/pic.png", "bytes")]

    it "skips a target the bag holds nothing for" $
      docxMedia (docOf [image "media/other.png" nullAttr])
                (bagOf [("media/pic.png", "bytes")])
        `shouldBe` M.empty

    it "leaves bag entries no image references out" $
      docxMedia (docOf [Str "prose"]) (bagOf [("media/header.png", "bytes")])
        `shouldBe` M.empty

    it "resolves a percent-encoded target by its own spelling" $
      docxMedia (docOf [image "media/image%201.png" nullAttr])
                (bagOf [("media/image%201.png", "bytes")])
        `shouldBe` M.fromList [("media/image%201.png", "bytes")]

    it "collapses references that share a target into one entry" $
      docxMedia (docOf [ image "media/pic.png" nullAttr
                       , image "media/pic.png" nullAttr ])
                (bagOf [("media/pic.png", "bytes")])
        `shouldBe` M.fromList [("media/pic.png", "bytes")]

  describe "placeImages" $ do
    let media :: Map Text BS.ByteString
        media = M.fromList [("media/pic.png", "bytes")]

    it "rewrites a resolvable target to its batch path and marks it" $ do
      let (placed, files) =
            placeImages "." media S.empty (docOf [image "media/pic.png" nullAttr])
      case firstImage placed of
        Image attr _ (target, _) ->
          (target, isExtracted attr) `shouldBe` (T.pack "media/pic.png", True)
        other -> expectationFailure ("not an image: " <> show other)
      files `shouldBe` [("media/pic.png", "bytes")]

    it "strips the reader's extent attributes, so the writer spells Markdown" $ do
      let sized = image "media/pic.png" ("", [], [("width", "1in"), ("height", "1in")])
          (placed, _) = placeImages "." media S.empty (docOf [sized])
      case firstImage placed of
        Image attr _ _ -> attr `shouldBe` ("", [], [(extractedKey, extractedValue)])
        other          -> expectationFailure ("not an image: " <> show other)

    it "leaves a target the map cannot resolve alone, unmarked" $ do
      let d = docOf [image "media/other.png" nullAttr]
          (placed, files) = placeImages "." media S.empty d
      (placed, files) `shouldBe` (d, [])

    it "suffices a colliding path before its extension, numbered until free" $ do
      let taken :: Set FilePath
          taken = S.singleton (mediaDir </> "pic.png")
          (placed, files) =
            placeImages "." media taken (docOf [image "media/pic.png" nullAttr])
      imageTargets placed `shouldBe` [T.pack (mediaDir </> "pic-2.png")]
      map fst files `shouldBe` [mediaDir </> "pic-2.png"]
      let (placed', files') = placeImages "."
            media (S.fromList [mediaDir </> "pic.png", mediaDir </> "pic-2.png"])
            (docOf [image "media/pic.png" nullAttr])
      map fst files' `shouldBe` [mediaDir </> "pic-3.png"]
      imageTargets placed' `shouldBe` [T.pack (mediaDir </> "pic-3.png")]

    it "keeps a document with no resolvable images untouched" $ do
      let d = docOf [image "media/other.png" nullAttr, Str "prose"]
      placeImages "." media S.empty d `shouldBe` (d, [])

    it "shares one file between references that share a target" $ do
      let (placed, files) = placeImages "." media S.empty
                              (docOf [image "media/pic.png" nullAttr, image "media/pic.png" nullAttr])
      length files `shouldBe` 1
      imageTargets placed `shouldBe` [T.pack "media/pic.png", T.pack "media/pic.png"]

  describe "placeImages: names the link must spell carefully" $ do
    let placeOne target =
          let (placed, files) = placeImages "." (M.fromList [(target, "bytes")]) S.empty
                                  (docOf [image target nullAttr])
           in (map fst files, imageTargets placed)

    it "leaves an ordinary name's link as its path" $
      placeOne "media/image1.png"
        `shouldBe` (["media/image1.png"], ["media/image1.png"])

    it "escapes a percent sign, so the decoded link is the stored name" $
      placeOne "media/image%201.png"
        `shouldBe` (["media/image%201.png"], ["media/image%25201.png"])

    it "escapes a space" $
      placeOne "media/image 2.png"
        `shouldBe` (["media/image 2.png"], ["media/image%202.png"])

    it "escapes a hash and a question mark, which would start a fragment or a query" $ do
      placeOne "media/a#b.png" `shouldBe` (["media/a#b.png"], ["media/a%23b.png"])
      placeOne "media/q?.png" `shouldBe` (["media/q?.png"], ["media/q%3F.png"])

    it "escapes parentheses, which would end or unbalance the link destination" $ do
      placeOne "media/a(1.png" `shouldBe` (["media/a(1.png"], ["media/a%281.png"])
      placeOne "media/a)b.png" `shouldBe` (["media/a)b.png"], ["media/a%29b.png"])
      placeOne "media/Picture (1).png"
        `shouldBe` (["media/Picture (1).png"], ["media/Picture%20%281%29.png"])

    it "round-trips through the Markdown writer and reader to the same target" $
      mapM_
        (\name -> do
            let target = "media/" <> name
                (placed, _) = first unmarkImages
                                (placeImages "." (M.fromList [(target, "bytes")]) S.empty
                                  (docOf [image target nullAttr]))
            rendered <- either (expectationFailure . T.unpack >=> const (pure "")) pure
                          (renderMarkdown placed)
            fmap imageTargets (parseMarkdown rendered) `shouldBe` Right (imageTargets placed))
        ["a(1.png", "a)b.png", "Picture (1).png", "a)(b.png", "((.png"]

    it "leaves non-ASCII letters in the link as written" $
      placeOne "media/bild-\228.png"
        `shouldBe` (["media/bild-\228.png"], ["media/bild-\228.png"])

    it "takes only the last segment of a name holding backslashes" $
      placeOne "media/..\\..\\evil.png"
        `shouldBe` (["media/evil.png"], ["media/evil.png"])

    it "names a target whose last segment is no file name 'image'" $
      placeOne "media/.."
        `shouldBe` (["media/image"], ["media/image"])

    it "replaces a control character in the name" $
      placeOne "media/a\tb.png"
        `shouldBe` (["media/a_b.png"], ["media/a_b.png"])

  describe "placeImages under a folder" $ do
    let media :: Map Text BS.ByteString
        media = M.fromList [("media/pic.png", "bytes")]

    it "places the file in the document's folder and links it relative to the document" $ do
      let (placed, files) = placeImages "A/b" media S.empty
                              (docOf [image "media/pic.png" nullAttr])
      map fst files `shouldBe` ["A/b" </> mediaDir </> "pic.png"]
      imageTargets placed `shouldBe` [T.pack (mediaDir </> "pic.png")]

    it "does not collide with the same name in another folder" $ do
      let (_, files) = placeImages "B" media (S.singleton ("A" </> mediaDir </> "pic.png"))
                         (docOf [image "media/pic.png" nullAttr])
      map fst files `shouldBe` ["B" </> mediaDir </> "pic.png"]

    it "suffixes a collision in its own folder, and the link follows the suffix" $ do
      let (placed, files) = placeImages "A" media (S.singleton ("A" </> mediaDir </> "pic.png"))
                              (docOf [image "media/pic.png" nullAttr])
      map fst files `shouldBe` ["A" </> mediaDir </> "pic-2.png"]
      imageTargets placed `shouldBe` [T.pack (mediaDir </> "pic-2.png")]

    it "keeps the bare media path for a document at the batch root" $ do
      let (placed, files) = placeImages "." media S.empty
                              (docOf [image "media/pic.png" nullAttr])
      map fst files `shouldBe` [mediaDir </> "pic.png"]
      imageTargets placed `shouldBe` [T.pack (mediaDir </> "pic.png")]

  describe "a placed name is bounded to one path segment" $ do
    let long = replicate 70000 'a'
        longTarget = T.pack ("media/" <> long <> ".png")
        otherTarget = T.pack ("docs/" <> long <> ".png")
        bothMedia = M.fromList [(longTarget, "one"), (otherTarget, "two")]

    it "keeps a 70,000-character segment within 255 bytes, its extension intact" $
      case placeImages "." (M.fromList [(longTarget, "bytes")]) S.empty
             (docOf [image longTarget nullAttr]) of
        (_, [(path, _)]) -> do
          utf8Len (takeFileName path) `shouldSatisfy` (<= 255)
          takeExtension path `shouldBe` ".png"
        (_, other) -> expectationFailure ("expected one placed file, got " <> show (map fst other))

    it "gives two colliding long segments distinct names, both within 255 bytes" $
      let (_, files) = placeImages "." bothMedia S.empty
                         (docOf [image longTarget nullAttr, image otherTarget nullAttr])
       in do
            length files `shouldBe` 2
            map fst files `shouldSatisfy` \paths -> length (nub paths) == length paths
            map (utf8Len . takeFileName . fst) files `shouldSatisfy` all (<= 255)

    it "links each placed name, the collision suffix included" $
      let (placed, files) = placeImages "." bothMedia S.empty
                              (docOf [image longTarget nullAttr, image otherTarget nullAttr])
       in imageTargets placed
            `shouldBe` [T.pack (mediaDir </> takeFileName path) | (path, _) <- files]

    it "truncates a multibyte segment on a character boundary, filling the budget" $
      case placeImages "." (M.fromList [(accented, "bytes")]) S.empty
             (docOf [image accented nullAttr]) of
        (_, [(path, _)]) -> utf8Len (takeFileName path) `shouldBe` 254
        (_, other) -> expectationFailure ("expected one placed file, got " <> show (map fst other))

    it "cuts a name whose extension alone is too long to 255 bytes" $
      let target = T.pack ("media/x." <> replicate 70000 'e')
       in case placeImages "." (M.fromList [(target, "bytes")]) S.empty
                 (docOf [image target nullAttr]) of
            (_, [(path, _)]) -> utf8Len (takeFileName path) `shouldSatisfy` (<= 255)
            (_, other) -> expectationFailure ("expected one placed file, got " <> show (map fst other))
  where
    accented = T.pack ("media/" <> replicate 200 'é' <> ".png")

    utf8Len :: String -> Int
    utf8Len = BS.length . TE.encodeUtf8 . T.pack
