module Mdmaster.Web.View.Demo
  ( demoNotice
  , samplePanel
  , sampleOperationAttribute
  , sampleLoadClass
  , sampleFileClass
  ) where

import qualified Data.ByteString.Char8 as BS8
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Lucid
import Mdmaster.Core.Operation (Operation (..), OperationId (..))
import Mdmaster.Web.Api (staticHref)
import Mdmaster.Web.Demo (DemoSet (..), demoFolder)
import Network.HTTP.Types (urlEncode)

demoNotice :: Html ()
demoNotice = p_ [class_ "demo-notice"] $ do
  span_ [term "aria-hidden" "true"] "🎈 "
  "Demo mode: every sample document is fictional"

samplePanel :: [Operation] -> [DemoSet] -> Html ()
samplePanel operations sets = section_ [class_ "card sample-panel"] $ do
  h2_ [class_ "sample-title"] "Try it with made-up files"
  p_ [class_ "drop-hint"]
    "Pick an operation to load its samples, then press Run. Your own files work too."
  ul_ [class_ "sample-sets"] $ mapM_ sampleSet
    [ (operation, set)
    | operation <- operations
    , Just set <- [find ((== opId operation) . demoOperation) sets]
    ]
  where
    sampleSet :: (Operation, DemoSet) -> Html ()
    sampleSet (operation, set) =
      li_ [ class_ "sample-set"
          , term sampleOperationAttribute (unOperationId (opId operation))
          ] $ do
        button_ [type_ "button", class_ ("picker " <> sampleLoadClass)]
          (toHtml (opName operation <> " · " <> count (length (demoFiles set))))
        ul_ [class_ "sample-files"] $ mapM_ (sampleLink operation) (demoFiles set)

    sampleLink :: Operation -> FilePath -> Html ()
    sampleLink operation path = li_ $
      a_ [ class_ sampleFileClass
         , href_ (sampleHref (opId operation) path)
         , download_ ""
         ] (toHtml path)

    count :: Int -> Text
    count 1 = "1 file"
    count n = T.pack (show n) <> " files"

sampleHref :: OperationId -> FilePath -> Text
sampleHref operation path = staticHref . T.intercalate "/" $
  T.pack demoFolder : map encoded (unOperationId operation : T.splitOn "/" (T.pack path))
  where
    encoded = T.pack . BS8.unpack . urlEncode False . TE.encodeUtf8

sampleOperationAttribute :: Text
sampleOperationAttribute = "data-sample-operation"

sampleLoadClass :: Text
sampleLoadClass = "sample-load"

sampleFileClass :: Text
sampleFileClass = "sample-file"
