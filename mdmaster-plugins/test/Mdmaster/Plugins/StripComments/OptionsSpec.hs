module Mdmaster.Plugins.StripComments.OptionsSpec (spec) where

import Data.Either (isRight)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Mdmaster.Core.Options
import Mdmaster.Plugins.StripComments.Options
import Test.Hspec

decodeRaw :: [(Text, Text)] -> KeepMarkers
decodeRaw kvs = case validateOptions stripCommentsSchema (M.fromList kvs) of
  Left errs -> error ("unexpected validation failure: " <> show errs)
  Right vs  -> decodeStripCommentsOptions vs

unsafeValues :: [(Text, OptionValue)] -> OptionValues
unsafeValues = OptionValues . M.fromList

spec :: Spec
spec = do
  describe "stripCommentsSchema" $ do
    it "declares exactly keepMarkers" $
      map fieldKey (schemaFields stripCommentsSchema) `shouldBe` ["keepMarkers"]

    it "offers true and false with true as the default" $
      [ (map choiceValue cs, d)
      | EnumField cs d <- map fieldType (filter ((== "keepMarkers") . fieldKey) (schemaFields stripCommentsSchema)) ]
        `shouldBe` [(["true", "false"], "true")]

    it "gives every field a label and a help string" $
      all (\f -> fieldLabel f /= "" && fieldHelp f /= "") (schemaFields stripCommentsSchema)
        `shouldBe` True

    it "validates its own declared defaults" $
      isRight (validateOptions stripCommentsSchema
                 (M.fromList [ (fieldKey f, renderDefault (fieldType f))
                             | f <- schemaFields stripCommentsSchema ]))
        `shouldBe` True

  describe "decodeStripCommentsOptions" $ do
    it "uses the documented default when nothing is supplied" $
      decodeRaw [] `shouldBe` KeepMarkersOn

    it "reads every value" $ do
      decodeRaw [("keepMarkers", "true")]  `shouldBe` KeepMarkersOn
      decodeRaw [("keepMarkers", "false")] `shouldBe` KeepMarkersOff

    it "is total: values that never passed validation still decode to defaults" $
      decodeStripCommentsOptions (defaultValues (OptionSchema []))
        `shouldBe` KeepMarkersOn

    it "decodes an unrecognised value to the default rather than failing" $
      decodeStripCommentsOptions (unsafeValues [("keepMarkers", VEnum "maybe")])
        `shouldBe` KeepMarkersOn
