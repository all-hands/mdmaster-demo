module Mdmaster.Web.View.DiffContext
  ( diffContextCap
  , diffContextChoices
  , parseDiffContext
  , clampDiffContext
  , badDiffContextMessage
  , contextActiveClass
  , contextControl
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Lucid
import Mdmaster.App.Pipeline (diffContext)
import Mdmaster.App.Types (BatchId)
import Mdmaster.Web.Api (batchFileDiffHref)
import Data.Char (isDigit)
import Text.Read (readMaybe)

diffContextCap :: Int
diffContextCap = 50

diffContextChoices :: [(Int, Text)]
diffContextChoices =
  [ (0,            "less")
  , (diffContext,  "standard")
  , (10,           "more")
  , (diffContextCap, "50 lines")
  ]

parseDiffContext :: Text -> Maybe Int
parseDiffContext txt = case T.stripPrefix "-" stripped of
    Just digits | decimal digits -> readMaybe (T.unpack stripped)
    Nothing     | decimal stripped -> readMaybe (T.unpack stripped)
    _                              -> Nothing
  where
    stripped = T.strip txt
    decimal t = not (T.null t) && T.all isDigit t

clampDiffContext :: Int -> Int
clampDiffContext = max 0 . min diffContextCap

badDiffContextMessage :: Text
badDiffContextMessage = "The context parameter must be a whole number of lines."

contextActiveClass :: Text
contextActiveClass = "is-active"

contextControl :: BatchId -> Int -> Int -> Html ()
contextControl batchId index active =
  nav_ [class_ "diff-context", term "aria-label" "Context shown around each change"] $
    mapM_ choice diffContextChoices
  where
    choice :: (Int, Text) -> Html ()
    choice (value, label)
      | value == active = span_
          [ class_ ("diff-context-choice " <> contextActiveClass)
          , term "aria-current" "true"
          ]
          (toHtml label)
      | otherwise =
          let target = batchFileDiffHref batchId index (Just value)
           in a_ [ href_ target
                 , class_ "diff-context-choice"
                 , term "hx-get" target
                 , term "hx-target" "#diff-pane"
                 ]
                 (toHtml label)
