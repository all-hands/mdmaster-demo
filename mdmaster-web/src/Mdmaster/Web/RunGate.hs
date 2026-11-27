module Mdmaster.Web.RunGate
  ( RunGate
  , newRunGate
  , withRunPermit
  ) where

import Control.Exception (finally, mask)
import Data.IORef (IORef, atomicModifyIORef', newIORef)

newtype RunGate = RunGate (IORef Int)

newRunGate :: Int -> IO RunGate
newRunGate n = RunGate <$> newIORef n

withRunPermit :: RunGate -> IO a -> IO a -> IO a
withRunPermit (RunGate free) refused action = mask $ \restore -> do
  taken <- atomicModifyIORef' free takePermit
  if taken
    then restore action `finally` atomicModifyIORef' free givePermit
    else restore refused
  where
    takePermit n
      | n > 0     = (n - 1, True)
      | otherwise = (n, False)

    givePermit n = (n + 1, ())
