module Mdmaster.Web.ConfigSpec (spec) where

import Data.Either (isLeft, isRight)
import Data.List (stripPrefix)
import qualified Data.Text as T
import Mdmaster.Core.Operation (OperationId (..))
import Mdmaster.Web.Config
import Test.Hspec

spec :: Spec
spec = do
  describe "defaults" $ do
    it "uses the documented defaults when the environment is empty" $
      configFromEnv [] `shouldBe` Right defaultConfig

    it "defaults the port to 8965" $
      fmap cfgPort (configFromEnv []) `shouldBe` Right 8965

    it "defaults the max file size to 10 MB" $
      fmap cfgMaxFileBytes (configFromEnv []) `shouldBe` Right 10485760

    it "defaults the max files per batch to 500" $
      fmap cfgMaxFiles (configFromEnv []) `shouldBe` Right 500

    it "defaults the max total batch size to 100 MB" $
      fmap cfgMaxBatchBytes (configFromEnv []) `shouldBe` Right 104857600

    it "defaults the docx expanded-size limit to 50 MiB" $
      fmap cfgMaxDocxUncompressedBytes (configFromEnv []) `shouldBe` Right 52428800

    it "defaults the docx batch expanded-size limit to 200 MiB" $
      fmap cfgMaxDocxBatchUncompressedBytes (configFromEnv []) `shouldBe` Right 209715200

    it "defaults the result TTL to 30 minutes" $
      fmap cfgResultTtl (configFromEnv []) `shouldBe` Right 1800

    it "defaults the run time limit to one minute" $
      fmap cfgRunTimeout (configFromEnv []) `shouldBe` Right 60

    it "defaults the run concurrency to 2" $
      fmap cfgMaxConcurrentRuns (configFromEnv []) `shouldBe` Right 2

    it "defaults the result count to 100" $
      fmap cfgMaxResults (configFromEnv []) `shouldBe` Right 100

    it "defaults the store byte cap to 1 GiB" $
      fmap cfgMaxStoreBytes (configFromEnv []) `shouldBe` Right 1073741824

    it "defaults the static directory to static" $
      fmap cfgStaticDir (configFromEnv []) `shouldBe` Right "static"

    it "defaults the operation to toc" $
      fmap cfgDefaultOperation (configFromEnv []) `shouldBe` Right (OperationId "toc")

  describe "overrides" $ do
    it "reads the port" $
      fmap cfgPort (configFromEnv [("MDMASTER_PORT", "9000")]) `shouldBe` Right 9000

    it "reads the max file size" $
      fmap cfgMaxFileBytes (configFromEnv [("MDMASTER_MAX_FILE_BYTES", "1024")])
        `shouldBe` Right 1024

    it "reads the max file count" $
      fmap cfgMaxFiles (configFromEnv [("MDMASTER_MAX_FILES", "10")]) `shouldBe` Right 10

    it "reads the max batch size" $
      fmap cfgMaxBatchBytes (configFromEnv [("MDMASTER_MAX_BATCH_BYTES", "2048")])
        `shouldBe` Right 2048

    it "reads the docx expanded-size limit" $
      fmap cfgMaxDocxUncompressedBytes
        (configFromEnv [("MDMASTER_MAX_DOCX_UNCOMPRESSED_BYTES", "1234")])
        `shouldBe` Right 1234

    it "reads the docx batch expanded-size limit" $
      fmap cfgMaxDocxBatchUncompressedBytes
        (configFromEnv [("MDMASTER_MAX_DOCX_BATCH_UNCOMPRESSED_BYTES", "4321")])
        `shouldBe` Right 4321

    it "reads the TTL in seconds" $
      fmap cfgResultTtl (configFromEnv [("MDMASTER_RESULT_TTL_SECONDS", "60")])
        `shouldBe` Right 60

    it "reads the run time limit in seconds" $
      fmap cfgRunTimeout (configFromEnv [("MDMASTER_RUN_TIMEOUT_SECONDS", "5")])
        `shouldBe` Right 5

    it "reads the run concurrency" $
      fmap cfgMaxConcurrentRuns (configFromEnv [("MDMASTER_MAX_CONCURRENT_RUNS", "8")])
        `shouldBe` Right 8

    it "reads the max result count" $
      fmap cfgMaxResults (configFromEnv [("MDMASTER_MAX_RESULTS", "20")])
        `shouldBe` Right 20

    it "reads the store byte cap" $
      fmap cfgMaxStoreBytes (configFromEnv [ ("MDMASTER_MAX_STORE_BYTES", "4096")
                                           , ("MDMASTER_MAX_BATCH_BYTES", "512")
                                           , ("MDMASTER_MAX_DOCX_BATCH_UNCOMPRESSED_BYTES", "2048") ])
        `shouldBe` Right 4096

    it "reads the static directory" $
      fmap cfgStaticDir (configFromEnv [("MDMASTER_STATIC_DIR", "/srv/static")])
        `shouldBe` Right "/srv/static"

    it "reads the default operation" $
      fmap cfgDefaultOperation (configFromEnv [("MDMASTER_DEFAULT_OPERATION", "fmt")])
        `shouldBe` Right (OperationId "fmt")

    it "ignores unrelated environment variables" $
      configFromEnv [("PATH", "/usr/bin"), ("HOME", "/root")] `shouldBe` Right defaultConfig

  describe "the source URL" $ do
    it "defaults to the project's repository" $
      fmap cfgSourceUrl (configFromEnv [])
        `shouldBe` Right "https://github.com/all-hands/mdmaster-demo"

    it "reads an https URL" $
      fmap cfgSourceUrl
        (configFromEnv [("MDMASTER_SOURCE_URL", "https://example.org/src/mdmaster-0.0.1.tar.gz")])
        `shouldBe` Right "https://example.org/src/mdmaster-0.0.1.tar.gz"

    it "reads an http URL with a port and a query" $
      fmap cfgSourceUrl
        (configFromEnv [("MDMASTER_SOURCE_URL", "http://source.example.org:8080/mdmaster?ref=v0.0.1")])
        `shouldBe` Right "http://source.example.org:8080/mdmaster?ref=v0.0.1"

    it "rejects what is not an absolute http or https URL, naming the variable" $ do
      let refused value = configFromEnv [("MDMASTER_SOURCE_URL", value)]
      refused "" `shouldSatisfy` isLeft
      refused "/source" `shouldSatisfy` isLeft
      refused "example.org/source" `shouldSatisfy` isLeft
      refused "javascript:alert(1)" `shouldSatisfy` isLeft
      refused "ftp://example.org/mdmaster.tar.gz" `shouldSatisfy` isLeft
      refused "https://" `shouldSatisfy` isLeft
      refused "https:///path-without-host" `shouldSatisfy` isLeft
      refused "https://example.org/a b" `shouldSatisfy` isLeft
      refused "https://example.org/\"onmouseover=x" `shouldSatisfy` isLeft
      refused "/source" `shouldBe`
        Left "MDMASTER_SOURCE_URL must be an absolute http:// or https:// URL, got \"/source\""

  describe "rejection" $ do
    it "rejects a non-numeric port with a message naming the variable" $
      configFromEnv [("MDMASTER_PORT", "http")]
        `shouldBe` Left "MDMASTER_PORT must be a positive whole number, got \"http\""

    it "rejects a zero port" $
      isLeft (configFromEnv [("MDMASTER_PORT", "0")]) `shouldBe` True

    it "rejects a negative limit" $
      isLeft (configFromEnv [("MDMASTER_MAX_FILES", "-1")]) `shouldBe` True

    it "rejects a non-positive docx expanded-size limit" $
      configFromEnv [("MDMASTER_MAX_DOCX_UNCOMPRESSED_BYTES", "0")]
        `shouldSatisfy` isLeft

    it "rejects a non-positive docx batch expanded-size limit" $
      configFromEnv [("MDMASTER_MAX_DOCX_BATCH_UNCOMPRESSED_BYTES", "0")]
        `shouldSatisfy` isLeft

    it "rejects a non-numeric TTL" $
      isLeft (configFromEnv [("MDMASTER_RESULT_TTL_SECONDS", "half an hour")])
        `shouldBe` True

    it "rejects a non-numeric run time limit, naming the variable" $
      configFromEnv [("MDMASTER_RUN_TIMEOUT_SECONDS", "a while")]
        `shouldBe` Left "MDMASTER_RUN_TIMEOUT_SECONDS must be a positive whole number, got \"a while\""

    it "rejects a zero or negative run time limit" $ do
      configFromEnv [("MDMASTER_RUN_TIMEOUT_SECONDS", "0")] `shouldSatisfy` isLeft
      configFromEnv [("MDMASTER_RUN_TIMEOUT_SECONDS", "-5")] `shouldSatisfy` isLeft

    it "rejects a run time limit past the cap, and accepts the cap" $ do
      configFromEnv [("MDMASTER_RUN_TIMEOUT_SECONDS", show (maxRunTimeoutSeconds + 1))]
        `shouldBe` Left ("MDMASTER_RUN_TIMEOUT_SECONDS must be between 1 and "
                          <> T.pack (show maxRunTimeoutSeconds) <> ", got "
                          <> T.pack (show (maxRunTimeoutSeconds + 1)))
      fmap cfgRunTimeout
        (configFromEnv [("MDMASTER_RUN_TIMEOUT_SECONDS", show maxRunTimeoutSeconds)])
        `shouldBe` Right (fromIntegral maxRunTimeoutSeconds)

    it "rejects a non-numeric run concurrency, naming the variable" $
      configFromEnv [("MDMASTER_MAX_CONCURRENT_RUNS", "several")]
        `shouldBe` Left "MDMASTER_MAX_CONCURRENT_RUNS must be a positive whole number, got \"several\""

    it "rejects a zero run concurrency" $
      isLeft (configFromEnv [("MDMASTER_MAX_CONCURRENT_RUNS", "0")]) `shouldBe` True

    it "rejects a run concurrency past the cap, and accepts the cap" $ do
      configFromEnv [("MDMASTER_MAX_CONCURRENT_RUNS", show (maxConcurrentRuns + 1))]
        `shouldBe` Left ("MDMASTER_MAX_CONCURRENT_RUNS must be between 1 and "
                         <> T.pack (show maxConcurrentRuns) <> ", got "
                         <> T.pack (show (maxConcurrentRuns + 1)))
      fmap cfgMaxConcurrentRuns
        (configFromEnv [("MDMASTER_MAX_CONCURRENT_RUNS", show maxConcurrentRuns)])
        `shouldBe` Right maxConcurrentRuns

    it "rejects a non-numeric result count" $
      configFromEnv [("MDMASTER_MAX_RESULTS", "plenty")]
        `shouldBe` Left "MDMASTER_MAX_RESULTS must be a positive whole number, got \"plenty\""

    it "rejects a zero result count" $
      isLeft (configFromEnv [("MDMASTER_MAX_RESULTS", "0")]) `shouldBe` True

    it "rejects a non-numeric store byte cap, naming the variable" $
      configFromEnv [("MDMASTER_MAX_STORE_BYTES", "lots")]
        `shouldBe` Left "MDMASTER_MAX_STORE_BYTES must be a positive whole number, got \"lots\""

    it "rejects a zero or negative store byte cap" $ do
      configFromEnv [("MDMASTER_MAX_STORE_BYTES", "0")] `shouldSatisfy` isLeft
      configFromEnv [("MDMASTER_MAX_STORE_BYTES", "-1")] `shouldSatisfy` isLeft

    it "rejects an empty default operation" $
      isLeft (configFromEnv [("MDMASTER_DEFAULT_OPERATION", "")]) `shouldBe` True

    it "reports the first failure when several variables are wrong" $
      configFromEnv [("MDMASTER_PORT", "x"), ("MDMASTER_MAX_FILES", "y")]
        `shouldBe` Left "MDMASTER_PORT must be a positive whole number, got \"x\""

    it "rejects a port outside the TCP range" $
      configFromEnv [("MDMASTER_PORT", "70000")]
        `shouldBe` Left "MDMASTER_PORT must be between 1 and 65535, got 70000"

    it "accepts the highest legal port" $
      fmap cfgPort (configFromEnv [("MDMASTER_PORT", "65535")]) `shouldBe` Right 65535

  describe "the store cap against the batch caps" $ do
    let small = ("MDMASTER_MAX_DOCX_BATCH_UNCOMPRESSED_BYTES", "1024")

    it "refuses a store under eight batches, naming both variables and the numbers" $
      configFromEnv [ ("MDMASTER_MAX_STORE_BYTES", "134217728")
                    , ("MDMASTER_MAX_BATCH_BYTES", "33554432") ]
        `shouldBe` Left ("MDMASTER_MAX_STORE_BYTES must be at least 8 times "
                         <> "MDMASTER_MAX_BATCH_BYTES, got 134217728 for a batch cap of "
                         <> "33554432: a Markdown batch can store about four times its "
                         <> "upload, and the store must hold two of the largest batch, "
                         <> "or one batch evicts everything else")

    it "refuses a store one byte under eight batches" $
      configFromEnv [ ("MDMASTER_MAX_STORE_BYTES", "8388607")
                    , ("MDMASTER_MAX_BATCH_BYTES", "1048576"), small ]
        `shouldSatisfy` isLeft

    it "accepts a store of exactly eight batches" $
      fmap cfgMaxStoreBytes (configFromEnv [ ("MDMASTER_MAX_STORE_BYTES", "8388608")
                                           , ("MDMASTER_MAX_BATCH_BYTES", "1048576"), small ])
        `shouldBe` Right 8388608

    it "refuses the default store against a batch cap past an eighth of it" $
      configFromEnv [("MDMASTER_MAX_BATCH_BYTES", "209715200")]
        `shouldSatisfy` isLeft

    it "does not let a huge batch cap wrap past the check" $
      configFromEnv [ ("MDMASTER_MAX_STORE_BYTES", "1073741824")
                    , ("MDMASTER_MAX_BATCH_BYTES", show (maxBound `div` 2 :: Int)) ]
        `shouldSatisfy` isLeft

    it "refuses a store under two .docx batches, naming both variables and the numbers" $
      configFromEnv [ ("MDMASTER_MAX_STORE_BYTES", "134217728")
                    , ("MDMASTER_MAX_BATCH_BYTES", "16777216") ]
        `shouldBe` Left ("MDMASTER_MAX_STORE_BYTES must be at least 2 times "
                         <> "MDMASTER_MAX_DOCX_BATCH_UNCOMPRESSED_BYTES, got 134217728 "
                         <> "for a .docx batch cap of 209715200: a .docx batch can store "
                         <> "about what it expands to, and the store must hold two of the "
                         <> "largest batch, or one batch evicts everything else")

    it "refuses a store one byte under two .docx batches" $
      configFromEnv [ ("MDMASTER_MAX_STORE_BYTES", "8388607")
                    , ("MDMASTER_MAX_BATCH_BYTES", "1024")
                    , ("MDMASTER_MAX_DOCX_BATCH_UNCOMPRESSED_BYTES", "4194304") ]
        `shouldSatisfy` isLeft

    it "accepts a store of exactly two .docx batches" $
      fmap cfgMaxStoreBytes
        (configFromEnv [ ("MDMASTER_MAX_STORE_BYTES", "8388608")
                       , ("MDMASTER_MAX_BATCH_BYTES", "1024")
                       , ("MDMASTER_MAX_DOCX_BATCH_UNCOMPRESSED_BYTES", "4194304") ])
        `shouldBe` Right 8388608

    it "refuses the default store against a .docx batch cap past half of it" $
      configFromEnv [("MDMASTER_MAX_DOCX_BATCH_UNCOMPRESSED_BYTES", "629145600")]
        `shouldSatisfy` isLeft

    it "does not let a huge .docx batch cap wrap past the check" $
      configFromEnv [ ("MDMASTER_MAX_DOCX_BATCH_UNCOMPRESSED_BYTES"
                      , show (maxBound `div` 2 :: Int)) ]
        `shouldSatisfy` isLeft

    it "accepts the caps the shipped deploy unit sets" $ do
      unit <- readFile "../deploy/mdmaster.container"
      let env = [ (key, drop 1 value)
                | line <- lines unit
                , Just assignment <- [stripPrefix "Environment=" line]
                , let (key, value) = break (== '=') assignment
                ]
      lookup "MDMASTER_MAX_STORE_BYTES" env `shouldSatisfy` (/= Nothing)
      lookup "MDMASTER_MAX_BATCH_BYTES" env `shouldSatisfy` (/= Nothing)
      lookup "MDMASTER_MAX_DOCX_BATCH_UNCOMPRESSED_BYTES" env `shouldSatisfy` (/= Nothing)
      configFromEnv env `shouldSatisfy` isRight

  describe "the default operation against the registry" $ do
    let known = [OperationId "toc", OperationId "fmt"]

    it "passes a configured operation that exists" $
      fmap cfgDefaultOperation (checkDefaultOperation known defaultConfig)
        `shouldBe` Right (OperationId "toc")

    it "refuses one nothing implements, and says what it knows" $
      checkDefaultOperation known defaultConfig { cfgDefaultOperation = OperationId "nope" }
        `shouldBe` Left "MDMASTER_DEFAULT_OPERATION must name a known operation, \
                        \got \"nope\". Known operations: toc, fmt"

    it "refuses every operation when the registry is empty" $
      checkDefaultOperation [] defaultConfig `shouldSatisfy` isLeft
