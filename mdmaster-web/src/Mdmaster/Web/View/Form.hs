module Mdmaster.Web.View.Form
  ( operationForm
  , optionPanel
  , optionFields
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Lucid
import Mdmaster.Core.Operation (Operation (..), OperationId (..), lookupOperation)
import Mdmaster.Core.Options
import Mdmaster.Web.Api (operationsHref, runAction)
import Mdmaster.Web.Limits (Limits)
import Mdmaster.Web.View.Layout
  (acceptExtensionsAttribute, acceptedExtensions, expansionNote, expansionNoteAttribute)

operationForm :: Limits -> [Operation] -> OperationId -> Map Text Text -> [OptionError] -> Html ()
operationForm limits operations selected submitted errors =
  form_ [id_ "run-form", method_ "post", action_ runAction, enctype_ "multipart/form-data"] $ do
    div_ [class_ "field"] $ do
      label_ [for_ "operation"] "Operation"
      select_
        [ id_ "operation"
        , name_ "operation"
        , term "hx-get" operationsHref
        , term "hx-target" "#operation-options"
        , term "hx-trigger" "change"
        , term "hx-include" "this"
        ]
        $ mapM_ operationOption operations

    div_ [id_ "operation-options"] $
      case lookupOperation selected operations of
        Nothing -> mempty
        Just operation -> optionPanel operation submitted errors

    button_ [type_ "submit", class_ "button run-button"] $ do
      span_ [class_ "run-spinner", term "aria-hidden" "true"] mempty
      span_ [class_ "run-label"] "Run"
  where
    operationOption :: Operation -> Html ()
    operationOption operation =
      option_
        ([ value_ (unOperationId (opId operation))
         , term acceptExtensionsAttribute (acceptedExtensions (opInput operation))
         ]
          <> [ term expansionNoteAttribute note
             | Just note <- [expansionNote limits (opInput operation)] ]
          <> [selected_ "selected" | opId operation == selected])
        (toHtml (opName operation))

optionPanel :: Operation -> Map Text Text -> [OptionError] -> Html ()
optionPanel operation submitted errors = do
  p_ [class_ "operation-description"] (toHtml (opDescription operation))
  optionFields (opSchema operation) submitted errors

optionFields :: OptionSchema -> Map Text Text -> [OptionError] -> Html ()
optionFields schema submitted errors =
  mapM_ renderField (schemaFields schema)
  where
    renderField :: OptionField -> Html ()
    renderField field = div_ [class_ "field"] $ do
      label_ [for_ (fieldKey field)] (toHtml (fieldLabel field))
      control field
      p_ [class_ "field-help"] (toHtml (fieldHelp field))
      mapM_ (p_ [class_ "field-error"] . toHtml) (errorFor (fieldKey field))

    errorFor key = [errMessage e | e <- errors, errField e == key]

    current field = fromMaybe (renderDefault (fieldType field))
                              (M.lookup (fieldKey field) submitted)

    control field = case fieldType field of
      IntField lo hi _ ->
        input_
          [ type_ "number"
          , id_ (fieldKey field)
          , name_ (fieldKey field)
          , min_ (tshow lo)
          , max_ (tshow hi)
          , value_ (current field)
          ]
      EnumField choices _ ->
        select_ [id_ (fieldKey field), name_ (fieldKey field)] $
          mapM_ (choiceOption (current field)) choices

    choiceOption :: Text -> EnumChoice -> Html ()
    choiceOption chosen choice =
      option_
        ([value_ (choiceValue choice)]
           <> [selected_ "selected" | choiceValue choice == chosen])
        (toHtml (choiceLabel choice))

    tshow :: Int -> Text
    tshow = T.pack . show
