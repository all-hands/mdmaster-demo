module Mdmaster.Core.OptionsSpec (spec) where

import Data.Text (Text)
import qualified Data.Map.Strict as M
import Mdmaster.Core.Options
import Test.Hspec

schema :: OptionSchema
schema = OptionSchema
  [ OptionField "depth" "Depth" "Deepest heading level" (IntField 1 6 3)
  , OptionField "mode" "Mode" "How to link"
      (EnumField [EnumChoice "github" "GitHub", EnumChoice "pandoc" "Pandoc"] "github")
  ]

modes :: EnumTable Text
modes = [("github", "github", "GitHub"), ("pandoc", "pandoc", "Pandoc")]

spec :: Spec
spec = do
  describe "defaultValues" $
    it "fills every declared field from its default" $ do
      let vs = defaultValues schema
      optInt "depth" vs `shouldBe` Just 3
      decodeEnum modes "github" "mode" vs `shouldBe` "github"

  describe "renderDefault" $ do
    it "renders an int default" $ renderDefault (IntField 1 6 3) `shouldBe` "3"
    it "renders an enum default" $
      renderDefault (EnumField [EnumChoice "a" "A"] "a") `shouldBe` "a"

  describe "validateOptions" $ do
    it "accepts values in range" $
      (validateOptions schema (M.fromList [("depth", "5")])
        >>= (Right . optInt "depth")) `shouldBe` Right (Just 5)

    it "trims surrounding whitespace" $
      (optInt "depth" <$> validateOptions schema (M.fromList [("depth", " 4 ")]))
        `shouldBe` Right (Just 4)

    it "falls back to the default for an omitted field" $
      (optInt "depth" <$> validateOptions schema M.empty) `shouldBe` Right (Just 3)

    it "falls back to the default for an empty field" $
      (optInt "depth" <$> validateOptions schema (M.fromList [("depth", "  ")]))
        `shouldBe` Right (Just 3)

    it "ignores keys the schema does not declare" $
      (optInt "depth" <$> validateOptions schema (M.fromList [("csrf", "xyz")]))
        `shouldBe` Right (Just 3)

    it "rejects an int below the minimum with a schema-derived message" $
      validateOptions schema (M.fromList [("depth", "0")])
        `shouldBe` Left [OptionError "depth" "must be a whole number between 1 and 6"]

    it "rejects an int above the maximum" $
      validateOptions schema (M.fromList [("depth", "7")])
        `shouldBe` Left [OptionError "depth" "must be a whole number between 1 and 6"]

    it "rejects a non-numeric int" $
      validateOptions schema (M.fromList [("depth", "deep")])
        `shouldBe` Left [OptionError "depth" "must be a whole number between 1 and 6"]

    it "rejects an unknown enum value listing the choices" $
      validateOptions schema (M.fromList [("mode", "gitlab")])
        `shouldBe` Left [OptionError "mode" "must be one of: github, pandoc"]

    it "collects every error rather than stopping at the first" $
      validateOptions schema (M.fromList [("depth", "9"), ("mode", "gitlab")])
        `shouldBe` Left [ OptionError "depth" "must be a whole number between 1 and 6"
                        , OptionError "mode" "must be one of: github, pandoc"
                        ]

  describe "decodeEnum" $ do
    it "reads the submitted value by its wire string" $
      decodeEnum modes "github" "mode"
        (OptionValues (M.fromList [("mode", VEnum "pandoc")]))
        `shouldBe` "pandoc"

    it "falls back to the default when the stored value has the wrong shape" $
      decodeEnum modes "github" "mode"
        (OptionValues (M.fromList [("mode", VInt 2)]))
        `shouldBe` "github"

