module Mdmaster.Plugins.Normalize.OptionsSpec (spec) where

import Data.Either (isRight)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Mdmaster.Core.Options
import Mdmaster.Plugins.Normalize.Options
import Test.Hspec

decodeRaw :: [(Text, Text)] -> Smart
decodeRaw kvs = case validateOptions normalizeSchema (M.fromList kvs) of
  Left errs -> error ("unexpected validation failure: " <> show errs)
  Right vs  -> decodeNormalizeOptions vs

unsafeValues :: [(Text, OptionValue)] -> OptionValues
unsafeValues = OptionValues . M.fromList

spec :: Spec
spec = do
  describe "normalizeSchema" $ do
    it "declares exactly smart" $
      map fieldKey (schemaFields normalizeSchema) `shouldBe` ["smart"]

    it "offers on and off with off as the default" $
      [ (map choiceValue cs, d)
      | EnumField cs d <- map fieldType (filter ((== "smart") . fieldKey) (schemaFields normalizeSchema)) ]
        `shouldBe` [(["off", "on"], "off")]

    it "gives every field a label and a help string" $
      all (\f -> fieldLabel f /= "" && fieldHelp f /= "") (schemaFields normalizeSchema)
        `shouldBe` True

    it "validates its own declared defaults" $
      isRight (validateOptions normalizeSchema
                 (M.fromList [ (fieldKey f, renderDefault (fieldType f))
                             | f <- schemaFields normalizeSchema ]))
        `shouldBe` True

  describe "decodeNormalizeOptions" $ do
    it "uses the documented default when nothing is supplied" $
      decodeRaw [] `shouldBe` SmartOff

    it "reads every value" $ do
      decodeRaw [("smart", "off")] `shouldBe` SmartOff
      decodeRaw [("smart", "on")]  `shouldBe` SmartOn

    it "is total: values that never passed validation still decode to defaults" $
      decodeNormalizeOptions (defaultValues (OptionSchema []))
        `shouldBe` SmartOff

    it "decodes an unrecognised value to the default rather than failing" $
      decodeNormalizeOptions (unsafeValues [("smart", VEnum "curly")])
        `shouldBe` SmartOff
