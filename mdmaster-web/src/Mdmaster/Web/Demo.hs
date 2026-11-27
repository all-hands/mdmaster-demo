module Mdmaster.Web.Demo
  ( DemoSet (..)
  , demoSwitch
  , demoMarker
  , demoFolder
  , demoSourceUrl
  , loadDemo
  ) where

import Data.Char (toLower)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Mdmaster.Core.Operation (OperationId (..))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))

data DemoSet = DemoSet
  { demoOperation :: OperationId
  , demoFiles     :: [FilePath]
  }
  deriving (Eq, Show)

demoSwitch :: [(String, String)] -> Either Text (Maybe Bool)
demoSwitch env = case lookup "MDMASTER_DEMO" env of
  Nothing -> Right Nothing
  Just raw
    | map toLower raw `elem` ["1", "true", "yes", "on"]  -> Right (Just True)
    | map toLower raw `elem` ["0", "false", "no", "off"] -> Right (Just False)
    | otherwise -> Left ("MDMASTER_DEMO must be 1, true, yes, on, 0, false, no or off, got "
                           <> T.pack (show raw))

demoFolder :: FilePath
demoFolder = "demo"

demoMarker :: FilePath
demoMarker = demoFolder </> "DEMO_MODE"

demoSourceUrl :: Text
demoSourceUrl = "https://github.com/all-hands/mdmaster-demo"

loadDemo :: Maybe Bool -> FilePath -> IO (Maybe [DemoSet])
loadDemo switch staticDir = do
  marked <- doesFileExist (staticDir </> demoMarker)
  if not (maybe marked id switch)
    then pure Nothing
    else do
      sets <- demoSets (staticDir </> demoFolder)
      if null sets
        then fail ("demo mode is on, but there are no samples under "
                     <> (staticDir </> demoFolder))
        else pure (Just sets)

demoSets :: FilePath -> IO [DemoSet]
demoSets root = do
  exists <- doesDirectoryExist root
  names <- if exists then sort <$> listDirectory root else pure []
  concat <$> mapM set names
  where
    set name = do
      isFolder <- doesDirectoryExist (root </> name)
      files <- if isFolder then filesUnder (root </> name) else pure []
      pure [ DemoSet (OperationId (T.pack name)) files | not (null files) ]

filesUnder :: FilePath -> IO [FilePath]
filesUnder root = sort <$> go ""
  where
    go prefix = do
      names <- filter ((/= ".") . take 1) <$> listDirectory (root </> prefix)
      concat <$> mapM (entry prefix) names
    entry prefix name = do
      let path = if null prefix then name else prefix <> "/" <> name
      isFolder <- doesDirectoryExist (root </> path)
      if isFolder then go path else pure [path]
