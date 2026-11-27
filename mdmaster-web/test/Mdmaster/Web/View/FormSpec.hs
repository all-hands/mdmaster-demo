module Mdmaster.Web.View.FormSpec (spec) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Lucid (Html, renderText)
import Mdmaster.Core.Operation (InputFormat (..), Operation (..), OperationId (..))
import Mdmaster.Core.Options
import Mdmaster.Web.Api (runAction)
import Mdmaster.Web.Config (defaultConfig)
import Mdmaster.Web.Limits (Limits, limitsFromConfig)
import Mdmaster.Web.View.Layout (acceptedExtensions, expansionNote)
import Mdmaster.Web.View.Form (operationForm, optionFields, optionPanel)
import Test.Hspec

render :: Html () -> Text
render = TL.toStrict . renderText

limits :: Limits
limits = limitsFromConfig defaultConfig

demoSchema :: OptionSchema
demoSchema = OptionSchema
  [ OptionField "depth" "Depth" "How deep" (IntField 1 6 3)
  , OptionField "mode" "Mode" "Which mode"
      (EnumField [EnumChoice "a" "Alpha", EnumChoice "b" "Beta"] "a")
  ]


demoOp :: Operation
demoOp = Operation
  { opId          = OperationId "demo"
  , opName        = "Demo Operation"
  , opDescription = "A demonstration"
  , opInput       = MarkdownInput
  , opSchema      = demoSchema
  , opRun         = \_ d -> Right d
  }

convertOp :: Operation
convertOp = demoOp
  { opId    = OperationId "convert"
  , opName  = "Convert Something"
  , opInput = DocxInput
  }

fields :: Map Text Text -> [OptionError] -> Text
fields values errs = render (optionFields demoSchema values errs)

spec :: Spec
spec = do
  describe "int fields" $ do
    it "render a number input with the schema bounds" $ do
      let out = fields M.empty []
      out `shouldSatisfy` T.isInfixOf "type=\"number\""
      out `shouldSatisfy` T.isInfixOf "min=\"1\""
      out `shouldSatisfy` T.isInfixOf "max=\"6\""

    it "pre-fill the schema default" $
      fields M.empty [] `shouldSatisfy` T.isInfixOf "value=\"3\""

    it "echo a submitted value instead of the default" $
      fields (M.fromList [("depth", "5")]) [] `shouldSatisfy` T.isInfixOf "value=\"5\""

  describe "enum fields" $ do
    it "render one option per choice, using the human label" $ do
      let out = fields M.empty []
      out `shouldSatisfy` T.isInfixOf "value=\"a\""
      out `shouldSatisfy` T.isInfixOf "Alpha"
      out `shouldSatisfy` T.isInfixOf "Beta"

    it "mark the default as selected" $
      fields M.empty [] `shouldSatisfy` T.isInfixOf "selected"

  describe "labels and help" $ do
    it "render every field's label" $ do
      let out = fields M.empty []
      out `shouldSatisfy` T.isInfixOf "Depth"
      out `shouldSatisfy` T.isInfixOf "Mode"

    it "render every field's help text" $
      fields M.empty [] `shouldSatisfy` T.isInfixOf "How deep"

  describe "errors" $ do
    it "render a field's error message next to it" $
      fields M.empty [OptionError "depth" "must be a whole number between 1 and 6"]
        `shouldSatisfy` T.isInfixOf "must be a whole number between 1 and 6"

    it "render nothing extra when there are no errors" $
      fields M.empty [] `shouldSatisfy` (not . T.isInfixOf "field-error")

  describe "escaping" $
    it "escapes a submitted value rather than emitting markup" $ do
      let out = fields (M.fromList [("depth", "\"><script>alert(1)</script>")]) []
      out `shouldSatisfy` (not . T.isInfixOf "<script>")

  describe "operationForm" $ do
    it "lists every registered operation" $
      render (operationForm limits [demoOp] (OperationId "demo") M.empty [])
        `shouldSatisfy` T.isInfixOf "Demo Operation"

    it "includes the operation id as a submitted field" $
      render (operationForm limits [demoOp] (OperationId "demo") M.empty [])
        `shouldSatisfy` T.isInfixOf "name=\"operation\""

    it "renders the selected operation's fields" $
      render (operationForm limits [demoOp] (OperationId "demo") M.empty [])
        `shouldSatisfy` T.isInfixOf "Depth"

    it "renders nothing for an unknown operation id" $
      render (operationForm limits [demoOp] (OperationId "nope") M.empty [])
        `shouldSatisfy` (not . T.isInfixOf "Depth")

    it "renders a submit button to start the operation" $
      render (operationForm limits [demoOp] (OperationId "demo") M.empty [])
        `shouldSatisfy` T.isInfixOf "<button type=\"submit\""

    it "renders the run label the client counts into and restores" $
      render (operationForm limits [demoOp] (OperationId "demo") M.empty [])
        `shouldSatisfy` T.isInfixOf "<span class=\"run-label\">Run</span>"

    it "renders the run route as its action" $ do
      let out = render (operationForm limits [demoOp] (OperationId "demo") M.empty [])
      out `shouldSatisfy` T.isInfixOf ("action=\"" <> runAction <> "\"")

    it "puts the operation's description inside the region the picker swaps" $ do
      let out = render (operationForm limits [demoOp] (OperationId "demo") M.empty [])
          (aboveOptions, fromOptions) = T.breakOn "id=\"operation-options\"" out
      aboveOptions `shouldSatisfy` (not . T.isInfixOf "A demonstration")
      fromOptions `shouldSatisfy` T.isInfixOf "A demonstration"

    it "renders the same panel the picker asks for on its own" $
      render (optionPanel demoOp M.empty []) `shouldSatisfy` T.isInfixOf "A demonstration"

    it "carries each operation's accepted extensions on its own option" $ do
      let out = render (operationForm limits [demoOp, convertOp] (OperationId "demo") M.empty [])
      out `shouldSatisfy` T.isInfixOf
        ("data-accept-extensions=\"" <> acceptedExtensions MarkdownInput <> "\"")
      out `shouldSatisfy` T.isInfixOf
        ("data-accept-extensions=\"" <> acceptedExtensions DocxInput <> "\"")

    it "carries the expansion note on the options whose format has one, and no other" $ do
      let out = render (operationForm limits [demoOp, convertOp] (OperationId "demo") M.empty [])
          note = maybe "" id (expansionNote limits DocxInput)
      note `shouldSatisfy` (not . T.null)
      T.count "data-expansion-note=" out `shouldBe` 1
      out `shouldSatisfy` T.isInfixOf ("data-expansion-note=\"" <> note <> "\"")
      expansionNote limits MarkdownInput `shouldBe` Nothing
