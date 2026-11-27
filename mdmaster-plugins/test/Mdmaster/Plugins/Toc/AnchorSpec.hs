module Mdmaster.Plugins.Toc.AnchorSpec (spec) where

import Data.Text (Text)
import Mdmaster.Plugins.Toc.Anchor
import Test.Hspec
import Text.Pandoc.Definition

hdr :: Int -> Text -> Text -> Block
hdr lvl ident title = Header lvl (ident, [], []) [Str title]

doc :: [Block] -> Pandoc
doc = Pandoc nullMeta

spec :: Spec
spec = do
  describe "anchorBlock" $
    it "renders the exact literal from the spec" $
      anchorBlock "intro"
        `shouldBe` RawBlock (Format "html") "<a id=\"intro\" data-mdmaster></a>"

  describe "isMdmasterAnchor" $ do
    it "recognises an anchor we produced" $
      isMdmasterAnchor (anchorBlock "x") `shouldBe` True
    it "tolerates surrounding whitespace" $
      isMdmasterAnchor (RawBlock (Format "html") "  <a id=\"x\" data-mdmaster></a>\n")
        `shouldBe` True
    it "recognises an anchor an earlier release wrote (data-mdaster)" $
      isMdmasterAnchor (RawBlock (Format "html") "<a id=\"x\" data-mdaster></a>")
        `shouldBe` True
    it "rejects a user-authored anchor without the attribute" $
      isMdmasterAnchor (RawBlock (Format "html") "<a id=\"x\"></a>") `shouldBe` False
    it "rejects a non-anchor raw block that mentions the attribute" $
      isMdmasterAnchor (RawBlock (Format "html") "<div data-mdmaster></div>") `shouldBe` False
    it "rejects non-HTML raw blocks" $
      isMdmasterAnchor (RawBlock (Format "latex") "<a id=\"x\" data-mdmaster></a>")
        `shouldBe` False
    it "rejects ordinary blocks" $
      isMdmasterAnchor (Para [Str "x"]) `shouldBe` False

  describe "removeMdmasterAnchors" $ do
    it "removes our anchors" $
      removeMdmasterAnchors (doc [anchorBlock "a", hdr 1 "a" "A"])
        `shouldBe` doc [hdr 1 "a" "A"]

    it "leaves user-authored anchors alone" $
      let userAnchor = RawBlock (Format "html") "<a id=\"mine\"></a>"
       in removeMdmasterAnchors (doc [userAnchor, hdr 1 "a" "A"])
            `shouldBe` doc [userAnchor, hdr 1 "a" "A"]

    it "removes anchors nested inside a div" $
      removeMdmasterAnchors (doc [Div nullAttr [anchorBlock "a", hdr 2 "a" "A"]])
        `shouldBe` doc [Div nullAttr [hdr 2 "a" "A"]]

    it "is a no-op on a document with no anchors" $
      removeMdmasterAnchors (doc [Para [Str "x"]]) `shouldBe` doc [Para [Str "x"]]

