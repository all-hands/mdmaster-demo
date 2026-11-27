module Mdmaster.App.FileStoreSpec (spec) where

import qualified Data.ByteString as B
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Mdmaster.App.Diff (DiffHunk (..), DiffKind (..), DiffLine (..))
import Mdmaster.App.FileStore
import Mdmaster.App.Types
import Mdmaster.Core.Operation (InputFormat (..), OperationId (..))
import Mdmaster.Core.Options (OptionSchema (..), OptionValues, defaultValues)
import Test.Hspec

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2026 7 28) 0

fakeClock :: IO (IORef UTCTime, IO UTCTime)
fakeClock = do
  ref <- newIORef epoch
  pure (ref, readIORef ref)

advance :: IORef UTCTime -> Double -> IO ()
advance ref seconds = modifyIORef' ref (addUTCTime (realToFrac seconds))

batch :: BatchId -> BatchResult
batch bid = BatchResult
  { brId        = bid
  , brAt        = epoch
  , brOperation = OperationId "spec"
  , brOptions   = noOptions
  , brInput     = MarkdownInput
  , brFiles     = []
  }

noOptions :: OptionValues
noOptions = defaultValues (OptionSchema [])

unlimited :: Int
unlimited = 1024 * 1024 * 1024

sized :: Int -> BatchId -> BatchResult
sized n bid = (batch bid)
  { brFiles = [ MediaRow "" (B.replicate n 0) ] }

unit :: Int
unit = batchBytes (sized 100 (BatchId "u"))

utf8Length :: T.Text -> Int
utf8Length = B.length . TE.encodeUtf8

spec :: Spec
spec = do
  describe "round-trip" $ do
    it "returns what was stored" $ do
      store <- newInMemoryStore 1800 100 unlimited
      let bid = BatchId "abc"
      _ <- fsPut store bid (batch bid)
      fsGet store bid `shouldReturn` Just (batch bid)

    it "returns Nothing for an id that was never stored" $ do
      store <- newInMemoryStore 1800 100 unlimited
      fsGet store (BatchId "missing") `shouldReturn` Nothing

    it "keeps several batches apart" $ do
      store <- newInMemoryStore 1800 100 unlimited
      _ <- fsPut store (BatchId "a") (batch (BatchId "a"))
      _ <- fsPut store (BatchId "b") (batch (BatchId "b"))
      a <- fsGet store (BatchId "a")
      b <- fsGet store (BatchId "b")
      (fmap brId a, fmap brId b) `shouldBe` (Just (BatchId "a"), Just (BatchId "b"))

    it "overwrites an existing id" $ do
      store <- newInMemoryStore 1800 100 unlimited
      let bid = BatchId "a"
          updated = (batch bid) { brAt = addUTCTime 1 epoch }
      _ <- fsPut store bid (batch bid)
      _ <- fsPut store bid updated
      fsGet store bid `shouldReturn` Just updated

  describe "expiry" $ do
    it "hands back the absolute instant the entry dies" $ do
      (ref, clock) <- fakeClock
      store <- newInMemoryStoreWith clock 100 10 unlimited
      expiresAt <- fsPut store (BatchId "a") (batch (BatchId "a"))
      advance ref 99
      fsGet store (BatchId "a") `shouldReturn` Just (batch (BatchId "a"))
      expiresAt `shouldBe` addUTCTime 100 epoch

    it "renews what it hands back when an overwrite renews the entry" $ do
      (ref, clock) <- fakeClock
      store <- newInMemoryStoreWith clock 100 10 unlimited
      _ <- fsPut store (BatchId "a") (batch (BatchId "a"))
      advance ref 60
      renewed <- fsPut store (BatchId "a") (batch (BatchId "a"))
      renewed `shouldBe` addUTCTime 160 epoch

    it "still returns an entry just before the TTL elapses" $ do
      (ref, clock) <- fakeClock
      store <- newInMemoryStoreWith clock 100 10 unlimited
      _ <- fsPut store (BatchId "a") (batch (BatchId "a"))
      advance ref 99
      fsGet store (BatchId "a") `shouldReturn` Just (batch (BatchId "a"))

    it "drops an entry once the TTL has elapsed" $ do
      (ref, clock) <- fakeClock
      store <- newInMemoryStoreWith clock 100 10 unlimited
      _ <- fsPut store (BatchId "a") (batch (BatchId "a"))
      advance ref 101
      fsGet store (BatchId "a") `shouldReturn` Nothing

    it "sweeping drops an expired entry without a put" $ do
      (ref, clock) <- fakeClock
      store <- newInMemoryStoreWith clock 100 10 unlimited
      _ <- fsPut store (BatchId "a") (sized 100 (BatchId "a"))
      advance ref 101
      fsHeld store `shouldReturn` Held { heldEntries = 1, heldBytes = unit }
      fsSweep store
      fsHeld store `shouldReturn` Held { heldEntries = 0, heldBytes = 0 }
      fsGet store (BatchId "a") `shouldReturn` Nothing

    it "sweeping keeps the entries whose expiry has not passed" $ do
      (ref, clock) <- fakeClock
      store <- newInMemoryStoreWith clock 100 10 unlimited
      _ <- fsPut store (BatchId "a") (sized 100 (BatchId "a"))
      advance ref 60
      _ <- fsPut store (BatchId "b") (sized 200 (BatchId "b"))
      advance ref 50
      fsSweep store
      fsHeld store `shouldReturn`
        Held { heldEntries = 1, heldBytes = batchBytes (sized 200 (BatchId "b")) }
      fsGet store (BatchId "a") `shouldReturn` Nothing
      fsGet store (BatchId "b") `shouldReturn` Just (sized 200 (BatchId "b"))

    it "holds what was put, and an expired entry it no longer serves" $ do
      (ref, clock) <- fakeClock
      store <- newInMemoryStoreWith clock 100 10 unlimited
      fsHeld store `shouldReturn` Held { heldEntries = 0, heldBytes = 0 }
      _ <- fsPut store (BatchId "a") (sized 100 (BatchId "a"))
      _ <- fsPut store (BatchId "b") (sized 100 (BatchId "b"))
      advance ref 101
      fsGet store (BatchId "a") `shouldReturn` Nothing
      fsHeld store `shouldReturn` Held { heldEntries = 2, heldBytes = 2 * unit }

    it "measures the TTL from the write, so a later write extends it" $ do
      (ref, clock) <- fakeClock
      store <- newInMemoryStoreWith clock 100 10 unlimited
      _ <- fsPut store (BatchId "a") (batch (BatchId "a"))
      advance ref 60
      _ <- fsPut store (BatchId "a") (batch (BatchId "a"))
      advance ref 60
      fsGet store (BatchId "a") `shouldReturn` Just (batch (BatchId "a"))

  describe "eviction" $ do
    it "holds exactly the cap's worth of entries" $ do
      (_, clock) <- fakeClock
      store <- newInMemoryStoreWith clock 100 3 unlimited
      _ <- fsPut store (BatchId "a") (batch (BatchId "a"))
      _ <- fsPut store (BatchId "b") (batch (BatchId "b"))
      _ <- fsPut store (BatchId "c") (batch (BatchId "c"))
      fsGet store (BatchId "a") `shouldReturn` Just (batch (BatchId "a"))
      fsGet store (BatchId "b") `shouldReturn` Just (batch (BatchId "b"))
      fsGet store (BatchId "c") `shouldReturn` Just (batch (BatchId "c"))

    it "evicts the soonest-expiring entry to admit one more" $ do
      (ref, clock) <- fakeClock
      store <- newInMemoryStoreWith clock 100 3 unlimited
      _ <- fsPut store (BatchId "a") (batch (BatchId "a"))
      advance ref 10
      _ <- fsPut store (BatchId "b") (batch (BatchId "b"))
      advance ref 10
      _ <- fsPut store (BatchId "c") (batch (BatchId "c"))
      advance ref 10
      _ <- fsPut store (BatchId "d") (batch (BatchId "d"))
      fsGet store (BatchId "a") `shouldReturn` Nothing
      fsGet store (BatchId "b") `shouldReturn` Just (batch (BatchId "b"))
      fsGet store (BatchId "c") `shouldReturn` Just (batch (BatchId "c"))
      fsGet store (BatchId "d") `shouldReturn` Just (batch (BatchId "d"))

    it "evicts by expiry, not insertion order" $ do
      (ref, clock) <- fakeClock
      store <- newInMemoryStoreWith clock 100 2 unlimited
      _ <- fsPut store (BatchId "a") (batch (BatchId "a"))
      advance ref 10
      _ <- fsPut store (BatchId "b") (batch (BatchId "b"))
      advance ref 10
      _ <- fsPut store (BatchId "a") (batch (BatchId "a"))
      advance ref 10
      _ <- fsPut store (BatchId "c") (batch (BatchId "c"))
      fsGet store (BatchId "a") `shouldReturn` Just (batch (BatchId "a"))
      fsGet store (BatchId "b") `shouldReturn` Nothing
      fsGet store (BatchId "c") `shouldReturn` Just (batch (BatchId "c"))

    it "breaks equal expiries on the smaller id" $ do
      (ref, clock) <- fakeClock
      store <- newInMemoryStoreWith clock 100 2 unlimited
      _ <- fsPut store (BatchId "b") (batch (BatchId "b"))
      _ <- fsPut store (BatchId "a") (batch (BatchId "a"))
      advance ref 1
      _ <- fsPut store (BatchId "c") (batch (BatchId "c"))
      fsGet store (BatchId "a") `shouldReturn` Nothing
      fsGet store (BatchId "b") `shouldReturn` Just (batch (BatchId "b"))
      fsGet store (BatchId "c") `shouldReturn` Just (batch (BatchId "c"))

    it "sweeps an expired entry before evicting a live one" $ do
      (ref, clock) <- fakeClock
      store <- newInMemoryStoreWith clock 100 2 unlimited
      _ <- fsPut store (BatchId "a") (batch (BatchId "a"))
      advance ref 60
      _ <- fsPut store (BatchId "b") (batch (BatchId "b"))
      advance ref 50
      _ <- fsPut store (BatchId "c") (batch (BatchId "c"))
      fsGet store (BatchId "a") `shouldReturn` Nothing
      fsGet store (BatchId "b") `shouldReturn` Just (batch (BatchId "b"))
      fsGet store (BatchId "c") `shouldReturn` Just (batch (BatchId "c"))

    it "overwrites at the cap without evicting anything" $ do
      (_, clock) <- fakeClock
      store <- newInMemoryStoreWith clock 100 2 unlimited
      _ <- fsPut store (BatchId "a") (batch (BatchId "a"))
      _ <- fsPut store (BatchId "b") (batch (BatchId "b"))
      _ <- fsPut store (BatchId "a") (batch (BatchId "a"))
      fsGet store (BatchId "a") `shouldReturn` Just (batch (BatchId "a"))
      fsGet store (BatchId "b") `shouldReturn` Just (batch (BatchId "b"))

  describe "byte cap" $ do
    let putAt store ref seconds n name = do
          advance ref seconds
          fsPut store (BatchId name) (sized (n * unit) (BatchId name))
        present store name n =
          fsGet store (BatchId name)
            `shouldReturn` Just (sized (n * unit) (BatchId name))
        gone store name = fsGet store (BatchId name) `shouldReturn` Nothing

    it "evicts the soonest-expiring entry to admit one that would pass the cap" $ do
      (ref, clock) <- fakeClock
      store <- newInMemoryStoreWith clock 100 10 (2 * unit)
      _ <- putAt store ref 0 1 "a"
      _ <- putAt store ref 10 1 "b"
      _ <- putAt store ref 10 1 "c"
      gone store "a"
      present store "b" 1
      present store "c" 1

    it "holds exactly the cap's worth of bytes" $ do
      (ref, clock) <- fakeClock
      store <- newInMemoryStoreWith clock 100 10 (2 * unit)
      _ <- putAt store ref 0 1 "a"
      _ <- putAt store ref 10 1 "b"
      present store "a" 1
      present store "b" 1

    it "evicts as many entries as the incoming batch needs" $ do
      (ref, clock) <- fakeClock
      store <- newInMemoryStoreWith clock 100 10 (3 * unit)
      _ <- putAt store ref 0 1 "a"
      _ <- putAt store ref 10 1 "b"
      _ <- putAt store ref 10 1 "c"
      _ <- putAt store ref 10 2 "d"
      gone store "a"
      gone store "b"
      present store "c" 1
      present store "d" 2

    it "evicts by expiry, not insertion order" $ do
      (ref, clock) <- fakeClock
      store <- newInMemoryStoreWith clock 100 10 (2 * unit)
      _ <- putAt store ref 0 1 "a"
      _ <- putAt store ref 10 1 "b"
      _ <- putAt store ref 10 1 "a"
      _ <- putAt store ref 10 1 "c"
      present store "a" 1
      gone store "b"
      present store "c" 1

    it "sweeps an expired entry before evicting a live one" $ do
      (ref, clock) <- fakeClock
      store <- newInMemoryStoreWith clock 100 10 (2 * unit)
      _ <- putAt store ref 0 1 "a"
      _ <- putAt store ref 60 1 "b"
      _ <- putAt store ref 50 1 "c"
      gone store "a"
      present store "b" 1
      present store "c" 1

    it "overwrites at the cap without evicting anything" $ do
      (ref, clock) <- fakeClock
      store <- newInMemoryStoreWith clock 100 10 (2 * unit)
      _ <- putAt store ref 0 1 "a"
      _ <- putAt store ref 0 1 "b"
      _ <- putAt store ref 0 1 "a"
      present store "a" 1
      present store "b" 1

    it "does not count the bytes an overwrite replaces against the new ones" $ do
      (ref, clock) <- fakeClock
      store <- newInMemoryStoreWith clock 100 10 (3 * unit)
      _ <- putAt store ref 0 1 "a"
      _ <- putAt store ref 10 1 "b"
      _ <- putAt store ref 10 2 "a"
      present store "a" 2
      present store "b" 1

    it "stores a batch larger than the cap alone, evicting everything else" $ do
      (ref, clock) <- fakeClock
      store <- newInMemoryStoreWith clock 100 10 (2 * unit)
      _ <- putAt store ref 0 1 "a"
      _ <- putAt store ref 10 1 "b"
      _ <- putAt store ref 10 5 "huge"
      gone store "a"
      gone store "b"
      present store "huge" 5
      _ <- putAt store ref 10 1 "c"
      gone store "huge"
      present store "c" 1

    it "stores an oversized batch into an empty store" $ do
      (ref, clock) <- fakeClock
      store <- newInMemoryStoreWith clock 100 10 unit
      _ <- putAt store ref 0 3 "huge"
      present store "huge" 3

    it "still enforces the count cap when the bytes are well under theirs" $ do
      (ref, clock) <- fakeClock
      store <- newInMemoryStoreWith clock 100 2 (10 * unit)
      _ <- putAt store ref 0 1 "a"
      _ <- putAt store ref 10 1 "b"
      _ <- putAt store ref 10 1 "c"
      gone store "a"
      present store "b" 1
      present store "c" 1

  describe "batchBytes" $ do
    let withRow row = (batch (BatchId "x")) { brFiles = [row] }

    it "is zero for a batch with no files" $
      batchBytes (batch (BatchId "x")) `shouldBe` 0

    it "counts a media row at its byte length" $
      batchBytes (withRow (MediaRow "" (B.replicate 7 1)))
        `shouldBe` 7

    it "counts text at its UTF-8 length, not its character count" $ do
      let text = "caf\233 \8212 \128512"
      utf8Length text `shouldSatisfy` (> T.length text)
      batchBytes (withRow (DocRow "" Unchanged (Just text) Nothing Nothing))
        `shouldBe` utf8Length text

    it "counts the rendered output, the before-state, the diff lines, a failure and the path" $ do
      let out    = "rendered \233"
          original = "original text"
          hunks  = [ DiffHunk 1 1 [DiffLine Removed "old line", DiffLine Added "new \233 line"]
                   , DiffHunk 9 9 [DiffLine Context "kept"] ]
          failure = "could not parse"
          path   = "notes/a.md"
          row = DocRow
            path
            (Failed failure)
            (Just out)
            (Just original)
            (Just hunks)
      batchBytes (withRow row) `shouldBe`
        length path + sum (map utf8Length
          [out, original, failure, "old line", "new \233 line", "kept"])

    it "sums across files" $
      batchBytes ((batch (BatchId "x"))
        { brFiles = [ MediaRow "" (B.replicate 3 0)
                    , MediaRow "" (B.replicate 4 0) ] })
        `shouldBe` 7
