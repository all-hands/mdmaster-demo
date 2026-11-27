module Mdmaster.Core.OperationSpec (spec) where

import Data.Text (Text)
import Mdmaster.Core.Operation
import Mdmaster.Core.Options
import Test.Hspec
import Text.Pandoc.Definition (Block (..), Pandoc (..), nullMeta)

identityOp :: Text -> Operation
identityOp name = Operation
  { opId          = OperationId name
  , opName        = name
  , opDescription = "does nothing"
  , opInput       = MarkdownInput
  , opSchema      = OptionSchema []
  , opRun         = \_ doc -> Right doc
  }

failingOp :: Operation
failingOp = Operation
  { opId          = OperationId "boom"
  , opName        = "Boom"
  , opDescription = "always fails"
  , opInput       = MarkdownInput
  , opSchema      = OptionSchema []
  , opRun         = \_ _ -> Left (OpError "kaboom")
  }

spec :: Spec
spec = do
  describe "lookupOperation" $ do
    let ops = [identityOp "toc", identityOp "fmt", failingOp]

    it "finds an operation by id" $
      fmap opId (lookupOperation (OperationId "fmt") ops)
        `shouldBe` Just (OperationId "fmt")

    it "returns Nothing for an unknown id" $
      fmap opId (lookupOperation (OperationId "nope") ops) `shouldBe` Nothing

    it "returns Nothing for an empty registry" $
      fmap opId (lookupOperation (OperationId "toc") []) `shouldBe` Nothing

  describe "opRun" $ do
    let doc = Pandoc nullMeta [Para []]

    it "returns Right for a successful operation" $
      opRun (identityOp "toc") (defaultValues (OptionSchema [])) doc
        `shouldBe` Right doc

    it "returns Left with the error message for a failing operation" $
      opRun failingOp (defaultValues (OptionSchema [])) doc
        `shouldBe` Left (OpError "kaboom")

  describe "input formats" $ do
    it "accepts both Markdown extensions" $
      inputExtensions MarkdownInput `shouldMatchList` [".md", ".markdown"]

    it "accepts only .docx for Word input" $
      inputExtensions DocxInput `shouldBe` [".docx"]

    it "gives every format at least one extension, so no operation is unusable" $
      all (not . null . inputExtensions) [MarkdownInput, DocxInput] `shouldBe` True
