module Mdmaster.App.FileStore
  ( FileStore (..)
  , Held (..)
  , newInMemoryStore
  , newInMemoryStoreWith
  ) where

import Control.Concurrent.STM
import Control.Exception (evaluate)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Mdmaster.App.Types (BatchId, BatchResult, batchBytes)

data FileStore m = FileStore {
    fsPut  :: BatchId -> BatchResult -> m UTCTime
  , fsGet  :: BatchId -> m (Maybe BatchResult)
  , fsSweep :: m ()
  , fsHeld :: m Held
  }

data Held = Held
  { heldEntries :: Int
  , heldBytes   :: Int
  }
  deriving (Eq, Show)

data Entry = Entry
  { enExpiry :: UTCTime
  , enBytes  :: Int
  , enResult :: BatchResult
  }

type Entries = Map BatchId Entry

newInMemoryStore :: NominalDiffTime -> Int -> Int -> IO (FileStore IO)
newInMemoryStore = newInMemoryStoreWith getCurrentTime

newInMemoryStoreWith :: IO UTCTime -> NominalDiffTime -> Int -> Int -> IO (FileStore IO)
newInMemoryStoreWith clock ttl cap byteCap = do
  var <- newTVarIO M.empty
  pure FileStore
    { fsPut = \batchId result -> do
        now <- clock
        size <- evaluate (batchBytes result)
        let expiresAt = addUTCTime ttl now
            entry = Entry { enExpiry = expiresAt, enBytes = size, enResult = result }
        atomically $ modifyTVar' var
          ( M.insert batchId entry
              . makeRoom cap byteCap now batchId size
          )
        pure expiresAt
    , fsGet = \batchId -> do
        now <- clock
        entries <- readTVarIO var
        pure $ case M.lookup batchId entries of
          Just entry | enExpiry entry > now -> Just (enResult entry)
          _                                 -> Nothing
    , fsSweep = do
        now <- clock
        atomically $ modifyTVar' var (sweep now)
    , fsHeld = do
        entries <- readTVarIO var
        pure Held { heldEntries = M.size entries, heldBytes = storedBytes entries }
    }

makeRoom :: Int -> Int -> UTCTime -> BatchId -> Int -> Entries -> Entries
makeRoom cap byteCap now batchId incoming =
  underCaps cap byteCap incoming . M.delete batchId . sweep now

underCaps :: Int -> Int -> Int -> Entries -> Entries
underCaps cap byteCap incoming = go
  where
    go m
      | fits m    = m
      | otherwise = case soonestExpiring m of
          Nothing   -> m
          Just next -> go (M.delete next m)
    fits m = M.size m < cap && storedBytes m + incoming <= byteCap

storedBytes :: Entries -> Int
storedBytes = sum . map enBytes . M.elems

soonestExpiring :: Entries -> Maybe BatchId
soonestExpiring m =
  case sortOn (enExpiry . snd) (M.toList m) of
    ((bid, _) : _) -> Just bid
    []             -> Nothing

sweep :: UTCTime -> Entries -> Entries
sweep now = M.filter ((> now) . enExpiry)
