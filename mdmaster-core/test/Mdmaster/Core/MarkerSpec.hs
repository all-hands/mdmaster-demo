module Mdmaster.Core.MarkerSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Mdmaster.Core.Marker
import Test.Hspec
import Text.Pandoc.Definition

para :: Text -> Block
para t = Para [Str t]

raw :: Text -> Block
raw t = RawBlock (Format "html") t

toc :: [Block]
toc = [BulletList [[Plain [Str "entry"]]]]

spec :: Spec
spec = do
  describe "marker literals" $ do
    it "builds the exact start literal" $
      markerStart "toc" `shouldBe` "<!-- mdmaster:toc start -->"
    it "builds the exact end literal" $
      markerEnd "toc" `shouldBe` "<!-- mdmaster:toc end -->"
    it "is parameterised by name" $
      markerStart "fmt" `shouldBe` "<!-- mdmaster:fmt start -->"
    it "builds both literals on the shared prefix" $ do
      T.isPrefixOf markerPrefix (markerStart "toc") `shouldBe` True
      T.isPrefixOf markerPrefix (markerEnd "toc") `shouldBe` True

  describe "wrapInMarker" $ do
    it "brackets the content with raw HTML comment blocks" $
      wrapInMarker "toc" toc
        `shouldBe` [raw "<!-- mdmaster:toc start -->"]
                <> toc
                <> [raw "<!-- mdmaster:toc end -->"]
    it "produces an empty but well-formed block for empty content" $
      wrapInMarker "toc" []
        `shouldBe` [raw "<!-- mdmaster:toc start -->", raw "<!-- mdmaster:toc end -->"]

  describe "scanMarker" $ do
    it "finds a marker block in the spelling before the rename" $
      scanMarker "toc"
        [ para "before", raw "<!-- mdaster:toc start -->", para "old"
        , raw "<!-- mdaster:toc end -->", para "after" ]
        `shouldBe` MarkerFound [para "before"] [para "after"]

    it "closes a legacy start on a current end" $
      scanMarker "toc" [raw "<!-- mdaster:toc start -->", raw "<!-- mdmaster:toc end -->"]
        `shouldBe` MarkerFound [] []

    it "still writes only the current spelling" $
      T.isInfixOf legacyMarkerPrefix (markerStart "toc") `shouldBe` False

    it "reports no marker in a plain document" $
      scanMarker "toc" [para "a", para "b"] `shouldBe` NoMarker

    it "reports no marker for a differently named marker" $
      scanMarker "toc" [raw "<!-- mdmaster:fmt start -->", raw "<!-- mdmaster:fmt end -->"]
        `shouldBe` NoMarker

    it "splits around a marker block, discarding markers and contents" $
      scanMarker "toc"
        ( [para "before"]
            <> wrapInMarker "toc" toc
            <> [para "after"] )
        `shouldBe` MarkerFound [para "before"] [para "after"]

    it "handles a marker block at the very start" $
      scanMarker "toc" (wrapInMarker "toc" toc <> [para "after"])
        `shouldBe` MarkerFound [] [para "after"]

    it "handles a marker block at the very end" $
      scanMarker "toc" ([para "before"] <> wrapInMarker "toc" toc)
        `shouldBe` MarkerFound [para "before"] []

    it "handles an empty marker block" $
      scanMarker "toc" ([para "x"] <> wrapInMarker "toc" [] <> [para "y"])
        `shouldBe` MarkerFound [para "x"] [para "y"]

    it "handles both markers inside one raw block" $
      scanMarker "toc"
        [ para "x"
        , raw "<!-- mdmaster:toc start --><!-- mdmaster:toc end -->"
        , para "y" ]
        `shouldBe` MarkerFound [para "x"] [para "y"]

    it "reports an unterminated marker" $
      scanMarker "toc" [para "x", raw "<!-- mdmaster:toc start -->", para "y"]
        `shouldBe` UnterminatedMarker

    it "ignores the literals when they are not raw HTML" $
      scanMarker "toc" [para "<!-- mdmaster:toc start -->"] `shouldBe` NoMarker

    it "ignores a stray end marker with no start" $
      scanMarker "toc" [para "x", raw "<!-- mdmaster:toc end -->"] `shouldBe` NoMarker

    it "uses the first start marker when there are several" $
      scanMarker "toc"
        ( wrapInMarker "toc" [para "one"]
            <> wrapInMarker "toc" [para "two"] )
        `shouldBe` MarkerFound [] (wrapInMarker "toc" [para "two"])
