module Mdmaster.Web.HandlersSpec (spec) where

import Control.Monad (replicateM)
import Data.Char (isDigit)
import qualified Data.Set as S
import qualified Data.Text as T
import Mdmaster.App.Types (BatchId (..))
import Mdmaster.Web.Handlers (newBatchId)
import Test.Hspec

isLowerHex :: Char -> Bool
isLowerHex c = isDigit c || c `elem` ['a' .. 'f']

spec :: Spec
spec = describe "newBatchId" $ do
  it "is 128 bits as 32 hex digits" $ do
    ids <- replicateM 200 newBatchId
    mapM_ (\(BatchId t) -> T.length t `shouldBe` 32) ids

  it "is lower-case hex and nothing else, safe in a URL path" $ do
    ids <- replicateM 200 newBatchId
    mapM_ (\(BatchId t) -> t `shouldSatisfy` T.all isLowerHex) ids

  it "is exactly 32 lower-case hex characters" $ do
    ids <- replicateM 200 newBatchId
    mapM_ (\(BatchId t) -> do
             T.length t `shouldBe` 32
             t `shouldSatisfy` T.all isLowerHex)
          ids

  it "differs between calls" $ do
    a <- newBatchId
    b <- newBatchId
    a `shouldNotBe` b

  it "does not repeat across many calls" $ do
    ids <- replicateM 500 newBatchId
    S.size (S.fromList (map unBatchId ids)) `shouldBe` 500
