module Mdmaster.Plugins.Toc.OptionsSpec (spec) where

import Data.Either (isRight)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Mdmaster.Core.Options
import Mdmaster.Plugins.Toc.Options
import Test.Hspec

decodeRaw :: [(Text, Text)] -> TocOptions
decodeRaw kvs = case validateOptions tocSchema (M.fromList kvs) of
  Left errs -> error ("unexpected validation failure: " <> show errs)
  Right vs  -> decodeTocOptions vs

unsafeValues :: [(Text, OptionValue)] -> OptionValues
unsafeValues = OptionValues . M.fromList

spec :: Spec
spec = do
  describe "tocSchema" $ do
    it "declares exactly mode, depth, skipTitle, anchors and style" $
      map fieldKey (schemaFields tocSchema)
        `shouldBe` ["mode", "depth", "skipTitle", "anchors", "style"]

    it "offers the two mode choices with update as the default" $
      [ (map choiceValue cs, d)
      | EnumField cs d <- map fieldType (filter ((== "mode") . fieldKey) (schemaFields tocSchema)) ]
        `shouldBe` [(["update", "remove"], "update")]

    it "constrains depth to 1..6 with a default of 3" $
      map fieldType (filter ((== "depth") . fieldKey) (schemaFields tocSchema))
        `shouldBe` [IntField 1 6 3]

    it "offers the three skipTitle choices with auto as the default" $
      [ (map choiceValue cs, d)
      | EnumField cs d <- map fieldType (filter ((== "skipTitle") . fieldKey) (schemaFields tocSchema)) ]
        `shouldBe` [(["auto", "always", "never"], "auto")]

    it "offers the three anchor choices with github as the default" $
      [ (map choiceValue cs, d)
      | EnumField cs d <- map fieldType (filter ((== "anchors") . fieldKey) (schemaFields tocSchema)) ]
        `shouldBe` [(["github", "pandoc", "explicit"], "github")]

    it "offers the two style choices with bullets as the default" $
      [ (map choiceValue cs, d)
      | EnumField cs d <- map fieldType (filter ((== "style") . fieldKey) (schemaFields tocSchema)) ]
        `shouldBe` [(["bullets", "numbered"], "bullets")]

    it "gives every field a label and a help string" $
      all (\f -> fieldLabel f /= "" && fieldHelp f /= "") (schemaFields tocSchema)
        `shouldBe` True

    it "validates its own declared defaults" $
      isRight (validateOptions tocSchema
                 (M.fromList [ (fieldKey f, renderDefault (fieldType f))
                             | f <- schemaFields tocSchema ]))
        `shouldBe` True

  describe "decodeTocOptions" $ do
    it "uses the documented defaults when nothing is supplied" $
      decodeRaw [] `shouldBe` TocOptions TocUpdate 3 SkipAuto AnchorGithub StyleBullets

    it "reads every mode" $ do
      tocMode (decodeRaw [("mode", "update")]) `shouldBe` TocUpdate
      tocMode (decodeRaw [("mode", "remove")]) `shouldBe` TocRemove

    it "reads depth" $
      tocDepth (decodeRaw [("depth", "6")]) `shouldBe` 6

    it "reads every skipTitle value" $ do
      tocSkipTitle (decodeRaw [("skipTitle", "auto")])   `shouldBe` SkipAuto
      tocSkipTitle (decodeRaw [("skipTitle", "always")]) `shouldBe` SkipAlways
      tocSkipTitle (decodeRaw [("skipTitle", "never")])  `shouldBe` SkipNever

    it "reads every anchor mode" $ do
      tocAnchor (decodeRaw [("anchors", "github")])   `shouldBe` AnchorGithub
      tocAnchor (decodeRaw [("anchors", "pandoc")])   `shouldBe` AnchorPandoc
      tocAnchor (decodeRaw [("anchors", "explicit")]) `shouldBe` AnchorExplicit

    it "reads every style" $ do
      tocStyle (decodeRaw [("style", "bullets")])  `shouldBe` StyleBullets
      tocStyle (decodeRaw [("style", "numbered")]) `shouldBe` StyleNumbered

    it "is total: values that never passed validation still decode to defaults" $
      decodeTocOptions (defaultValues (OptionSchema []))
        `shouldBe` TocOptions TocUpdate 3 SkipAuto AnchorGithub StyleBullets

    it "clamps an out-of-range depth rather than failing" $ do
      tocDepth (decodeTocOptions (unsafeValues [("depth", VInt 99)])) `shouldBe` 6
      tocDepth (decodeTocOptions (unsafeValues [("depth", VInt 0)]))  `shouldBe` 1
