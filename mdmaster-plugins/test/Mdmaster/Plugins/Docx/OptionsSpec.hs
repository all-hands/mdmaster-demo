module Mdmaster.Plugins.Docx.OptionsSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Mdmaster.Core.Options
import Mdmaster.Plugins.Docx.Options (CommentStyle (..), decodeDocxOptions, docxSchema)
import Test.Hspec

decoded :: [(Text, Text)] -> CommentStyle
decoded kvs = case validateOptions docxSchema (M.fromList kvs) of
  Left errs -> error ("bad options in test: " <> show errs)
  Right vs  -> decodeDocxOptions vs

unsafeValues :: [(Text, OptionValue)] -> OptionValues
unsafeValues = OptionValues . M.fromList

styles :: [(Text, CommentStyle)]
styles =
  [ ("diff-stable", DiffStable)
  , ("review",      Review)
  , ("agent",       Agent)
  , ("archive",     Archive)
  ]

spec :: Spec
spec = do
  describe "the schema" $ do
    it "declares exactly one field" $
      map fieldKey (schemaFields docxSchema) `shouldBe` ["comments"]

    it "offers all four styles" $
      case map fieldType (schemaFields docxSchema) of
        [EnumField choices _] ->
          map choiceValue choices `shouldBe` map fst styles
        other -> expectationFailure ("unexpected field types: " <> show other)

    it "gives every choice a human label" $
      case map fieldType (schemaFields docxSchema) of
        [EnumField choices _] ->
          all ((/= "") . choiceLabel) choices `shouldBe` True
        other -> expectationFailure ("unexpected field types: " <> show other)

    it "defaults to a style that is one of the offered choices" $
      case map fieldType (schemaFields docxSchema) of
        [EnumField choices d] -> d `elem` map choiceValue choices `shouldBe` True
        other -> expectationFailure ("unexpected field types: " <> show other)

    it "names review as that default" $
      case map fieldType (schemaFields docxSchema) of
        [EnumField _ d] -> d `shouldBe` "review"
        other -> expectationFailure ("unexpected field types: " <> show other)

  describe "decoding" $ do
    it "reads each style" $
      map (\(wire, _) -> decoded [("comments", wire)]) styles
        `shouldBe` map snd styles

    it "falls back to the default when the field is absent" $
      decoded [] `shouldBe` Review

    it "is total: an unrecognised value that never passed validation falls back" $
      decodeDocxOptions (unsafeValues [("comments", VEnum "sideways")])
        `shouldBe` Review

    it "is total: a value of the wrong shape falls back" $
      decodeDocxOptions (unsafeValues [("comments", VInt 2)])
        `shouldBe` Review
