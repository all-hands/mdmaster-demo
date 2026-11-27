module Mdmaster.Plugins.RegistrySpec (spec) where

import Data.Either (isRight)
import Data.List (nub)
import qualified Data.Map.Strict as M
import Mdmaster.Core.Operation
import Mdmaster.Core.Options
import Mdmaster.Plugins.Registry (allOperations)
import Test.Hspec

spec :: Spec
spec = do
  it "is not empty" $
    null allOperations `shouldBe` False

  it "registers toc" $
    map opId allOperations `shouldContain` [OperationId "toc"]

  it "registers docx" $
    map opId allOperations `shouldContain` [OperationId "docx"]

  it "registers normalize" $
    map opId allOperations `shouldContain` [OperationId "normalize"]

  it "registers strip-comments" $
    map opId allOperations `shouldContain` [OperationId "strip-comments"]

  it "gives every operation a unique id" $
    let ids = map opId allOperations in length (nub ids) `shouldBe` length ids

  it "gives every operation a non-empty name and description" $
    all (\o -> opName o /= "" && opDescription o /= "") allOperations `shouldBe` True

  it "gives every operation a schema whose declared defaults validate" $
    all schemaDefaultsValidate allOperations `shouldBe` True

  it "gives every schema field a unique key" $
    all uniqueKeys allOperations `shouldBe` True

  it "is findable by id" $
    all (\o -> isFound (opId o)) allOperations `shouldBe` True

isFound :: OperationId -> Bool
isFound oid = case lookupOperation oid allOperations of
  Just op -> opId op == oid
  Nothing -> False

schemaDefaultsValidate :: Operation -> Bool
schemaDefaultsValidate op =
  isRight (validateOptions (opSchema op) rendered)
  where
    rendered = M.fromList
      [ (fieldKey f, renderDefault (fieldType f)) | f <- schemaFields (opSchema op) ]

uniqueKeys :: Operation -> Bool
uniqueKeys op =
  let keys = map fieldKey (schemaFields (opSchema op))
   in length (nub keys) == length keys
