module Mdmaster.Web.ServerSpec
  ( spec
  , appWith
  , appWithStore
  , postRun
  , operationField
  , sampleMarkdown
  , contains
  , countOf
  , batchIdIn
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.IORef (atomicModifyIORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime (..), fromGregorian)
import Mdmaster.App.FileStore (FileStore (..), newInMemoryStore)
import Mdmaster.App.Pipeline (diffContext)
import Mdmaster.App.Types
  (BatchId (..), BatchResult (..), BatchFile (..), FileStatus (..))
import Mdmaster.Core.Operation
  (InputFormat (..), Operation (..), OperationId (..), lookupOperation)
import Mdmaster.Core.Options
  (FieldType (..), OptionField (..), OptionSchema (..), defaultValues)
import Mdmaster.Plugins.Registry (allOperations)
import Mdmaster.Web.Api (batchFileHref)
import Mdmaster.Web.Config (Config (..), defaultConfig)
import Mdmaster.Web.Handlers (AppEnv (..), contentDisposition, healthzBody, unknownOperationMessage)
import Mdmaster.Web.Headers (contentSecurityPolicy)
import Mdmaster.Web.Identity (anonymousProvider)
import Mdmaster.Web.Limits
  ( UploadError (NoAcceptableFiles), limitsFromConfig, maxRequestBytes
  , parseFailureMessage, uploadErrorMessage )
import Mdmaster.Web.RunGate (newRunGate)
import Mdmaster.Web.Server (application)
import Mdmaster.Web.TestSupport
  ( Part (..), heaviestBody, heaviestContentType, multipartBody, multipartContentType
  , paddedFileBody )
import Mdmaster.Web.View.Diff (copyClass, copyHrefAttribute)
import Mdmaster.Web.View.DiffContext
  (badDiffContextMessage, diffContextCap, diffContextChoices)
import Mdmaster.Web.View.Layout (privacyNote, sourceLabel)
import Mdmaster.Web.View.Results
  ( downloadAllLabel
  , errorBannerTemplateId
  , expiredHeadline
  , expiryAttribute
  , expirySentence
  , nothingToZipHeadline
  )
import Network.HTTP.Types
  (methodPost, status200, status400, status409, status410)
import Network.Wai (Application)
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as WaiTest
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.Internal (withApplication)

appWith :: Config -> IO Application
appWith config = do
  store <- newInMemoryStore (cfgResultTtl config) (cfgMaxResults config) (cfgMaxStoreBytes config)
  appWithStore config store

appWithStore :: Config -> FileStore IO -> IO Application
appWithStore config store = do
  gate <- newRunGate (cfgMaxConcurrentRuns config)
  pure $ application AppEnv
    { envConfig     = config
    , envStore      = store
    , envOperations = allOperations
    , envIdentity   = anonymousProvider
    , envRunGate    = gate
    }

tiny :: Config
tiny = defaultConfig
  { cfgMaxFiles      = 3
  , cfgMaxFileBytes  = 64
  , cfgMaxBatchBytes = 100
  }

heavy :: Config
heavy = defaultConfig
  { cfgMaxFiles      = 3
  , cfgMaxFileBytes  = 40000
  , cfgMaxBatchBytes = 120000
  }

nearCaps :: Config
nearCaps = defaultConfig
  { cfgMaxFiles      = 3
  , cfgMaxFileBytes  = 40000
  , cfgMaxBatchBytes = 70000
  }


postRun :: [Part] -> WaiSession st SResponse
postRun parts =
  request methodPost "/run"
    [("Content-Type", multipartContentType)]
    (multipartBody parts)

chunkedPost :: BL.ByteString -> IO Wai.Request
chunkedPost = postWith multipartContentType Wai.ChunkedBody

declaredPost :: BL.ByteString -> IO Wai.Request
declaredPost = declaredPostAs multipartContentType

declaredPostAs :: BS.ByteString -> BL.ByteString -> IO Wai.Request
declaredPostAs contentType body =
  postWith contentType (Wai.KnownLength (fromIntegral (BL.length body))) body

paddedTo :: Int -> BL.ByteString
paddedTo over = BL.fromChunks (reads4k (front <> marker <> BS.concat (padLines pad) <> back))
  where
    reads4k bs
      | BS.null bs = []
      | otherwise  = let (a, b) = BS.splitAt 4096 bs in a : reads4k b
    whole = BL.toStrict (heaviestBody (limitsFromConfig heavy))
    marker = "Content-Disposition: form-data; name=\"operation\"\r\n"
    (front, rest) = BS.breakSubstring marker whole
    back = BS.drop (BS.length marker) rest
    pad = maxRequestBytes (limitsFromConfig heavy) - BS.length whole + over
    padLines n
      | n <= 0       = []
      | n <= 8100    = [line n]
      | otherwise    = line 8000 : padLines (n - 8000)
    line n = "X-Pad: " <> BS.replicate (n - 9) 112 <> "\r\n"

postWith :: BS.ByteString -> Wai.RequestBodyLength -> BL.ByteString -> IO Wai.Request
postWith contentType length_ body = fst <$> postCounting contentType length_ body

postCounting
  :: BS.ByteString -> Wai.RequestBodyLength -> BL.ByteString -> IO (Wai.Request, IO Int)
postCounting contentType length_ body = do
  chunks <- newIORef (BL.toChunks body)
  served <- newIORef 0
  let next = do
        chunk <- atomicModifyIORef chunks step
        atomicModifyIORef' served (\n -> (n + BS.length chunk, ()))
        pure chunk
  pure
    ( WaiTest.setPath
        (Wai.setRequestBodyChunks next Wai.defaultRequest
           { Wai.requestMethod     = methodPost
           , Wai.requestHeaders    = [("Content-Type", contentType)]
           , Wai.requestBodyLength = length_
           })
        "/run"
    , readIORef served
    )
  where
    step []       = ([], BS.empty)
    step (c : cs) = (cs, c)

sendTo
  :: Config
  -> Wai.RequestBodyLength
  -> BL.ByteString
  -> IO (SResponse, Int)
sendTo config length_ body = do
  app <- appWith config
  (req, served) <- postCounting multipartContentType length_ body
  res <- WaiTest.runSession (WaiTest.request req) app
  (,) res <$> served

declared :: BL.ByteString -> Wai.RequestBodyLength
declared = Wai.KnownLength . fromIntegral . BL.length

lowCeiling, highCeiling :: Config
lowCeiling  = tiny { cfgMaxFiles = 1 }
highCeiling = tiny { cfgMaxFiles = 100 }

ceilingBody :: BL.ByteString
ceilingBody = multipartBody [operationField, PaddedUpload 20 "guide.md" sampleMarkdown]

defaultOperation :: Operation
defaultOperation =
  fromMaybe (error "the default operation is missing from the registry")
            (lookupOperation (cfgDefaultOperation defaultConfig) allOperations)

operationIdBytes :: BS.ByteString
operationIdBytes = TE.encodeUtf8 (unOperationId (opId defaultOperation))

operationField :: Part
operationField = Field "operation" operationIdBytes

intOption :: (BS.ByteString, BS.ByteString)
intOption =
  case [ (TE.encodeUtf8 (fieldKey f), BS8.pack (show hi))
       | f <- schemaFields (opSchema defaultOperation)
       , IntField _ hi _ <- [fieldType f]
       ] of
    (x : _) -> x
    []      -> error "the default operation declares no integer option"

sampleMarkdown :: BS.ByteString
sampleMarkdown = "# Title\n\n## One\n\nbody\n\n## Two\n\nbody\n"

limitsBanner :: BS.ByteString
limitsBanner = TE.encodeUtf8 parseFailureMessage

unknownOperationBytes :: BS.ByteString
unknownOperationBytes = TE.encodeUtf8 unknownOperationMessage

expiredBytes, nothingToZipBytes :: BS.ByteString
expiredBytes = TE.encodeUtf8 expiredHeadline
nothingToZipBytes = TE.encodeUtf8 nothingToZipHeadline

downloadAllBytes :: BS.ByteString
downloadAllBytes = TE.encodeUtf8 downloadAllLabel

errorBannerTemplateIdBytes :: BS.ByteString
errorBannerTemplateIdBytes =
  TE.encodeUtf8 ("id=\"" <> errorBannerTemplateId <> "\"")

noUsableFilesBytes :: BS.ByteString
noUsableFilesBytes = TE.encodeUtf8 (uploadErrorMessage (NoAcceptableFiles MarkdownInput))

healthzBodyBytes :: BL.ByteString
healthzBodyBytes = BL.fromStrict (TE.encodeUtf8 healthzBody)

contains :: BS.ByteString -> SResponse -> Bool
contains needle = BS.isInfixOf needle . BL.toStrict . simpleBody

contextLabel :: Int -> Text
contextLabel value = fromMaybe "" (lookup value diffContextChoices)

countOf :: BS.ByteString -> SResponse -> Int
countOf needle = go . BL.toStrict . simpleBody
  where
    go haystack =
      let (_, rest) = BS.breakSubstring needle haystack
       in if BS.null rest then 0 else 1 + go (BS.drop (BS.length needle) rest)

fixtureFailure :: Text
fixtureFailure = "the marker block was never closed"

failedOnlyApp :: IO Application
failedOnlyApp = do
  store <- newInMemoryStore (cfgResultTtl defaultConfig) (cfgMaxResults defaultConfig)
    (cfgMaxStoreBytes defaultConfig)
  _ <- fsPut store (BatchId "allfailed") BatchResult
    { brId        = BatchId "allfailed"
    , brAt        = UTCTime (fromGregorian 2026 11 6) 0
    , brOperation = opId defaultOperation
    , brOptions   = defaultValues (opSchema defaultOperation)
    , brInput     = MarkdownInput
    , brFiles =
        [ DocRow
          "broken.md"
          (Failed fixtureFailure)
          Nothing
          Nothing
          Nothing
        ]
    }
  appWithStore defaultConfig store

batchIdIn :: SResponse -> BS.ByteString
batchIdIn res =
    BS8.takeWhile (/= '/') (BS.drop (BS.length marker) (snd (BS.breakSubstring marker body)))
  where
    marker = "/batch/"
    body   = BL.toStrict (simpleBody res)

spec :: Spec
spec = do
  with (appWith defaultConfig) $ do
    describe "GET /" $ do
      it "serves the page with the drop zone and the form" $ do
        res <- get "/"
        liftIO $ do
          simpleStatus res `shouldBe` status200
          contains "id=\"drop-zone\"" res `shouldBe` True
          contains "id=\"run-form\"" res `shouldBe` True
          contains "type=\"submit\"" res `shouldBe` True
          contains "id=\"form-panel\"" res `shouldBe` True
          contains "id=\"results\"" res `shouldBe` True
          contains errorBannerTemplateIdBytes res `shouldBe` True
          countOf "id=\"run-form\"" res `shouldBe` 1
          contains "data-accept-extensions=" res `shouldBe` True

      it "offers the source and states what happens to the files" $ do
        res <- get "/"
        liftIO $ do
          contains (TE.encodeUtf8 ("href=\"" <> cfgSourceUrl defaultConfig <> "\"")) res
            `shouldBe` True
          contains (TE.encodeUtf8 sourceLabel) res `shouldBe` True
          contains (TE.encodeUtf8 (privacyNote defaultConfig)) res `shouldBe` True

    describe "GET /operations" $ do
      it "serves the option fields of a known operation" $ do
        res <- get ("/operations?operation=" <> operationIdBytes)
        liftIO $ do
          simpleStatus res `shouldBe` status200
          contains (fst intOption) res `shouldBe` True

      it "reports an unknown operation rather than failing" $ do
        res <- get "/operations?operation=no-such-operation"
        liftIO $ contains unknownOperationBytes res `shouldBe` True

    describe "GET /healthz" $
      it "answers the liveness body as text/plain" $ do
        res <- get "/healthz"
        liftIO $ do
          simpleStatus res `shouldBe` status200
          simpleBody res `shouldBe` healthzBodyBytes
          lookup "Content-Type" (simpleHeaders res)
            `shouldBe` Just "text/plain;charset=utf-8"

    describe "the security headers" $ do
      it "sends the four headers with the page" $ do
        res <- get "/"
        liftIO $ do
          simpleStatus res `shouldBe` status200
          lookup "X-Content-Type-Options" (simpleHeaders res)
            `shouldBe` Just "nosniff"
          lookup "X-Frame-Options" (simpleHeaders res) `shouldBe` Just "DENY"
          lookup "Referrer-Policy" (simpleHeaders res)
            `shouldBe` Just "no-referrer"
          lookup "Content-Security-Policy" (simpleHeaders res)
            `shouldBe` Just contentSecurityPolicy

      it "sends them with the liveness probe too" $ do
        res <- get "/healthz"
        liftIO $ do
          simpleStatus res `shouldBe` status200
          lookup "X-Content-Type-Options" (simpleHeaders res)
            `shouldBe` Just "nosniff"
          lookup "X-Frame-Options" (simpleHeaders res) `shouldBe` Just "DENY"
          lookup "Referrer-Policy" (simpleHeaders res)
            `shouldBe` Just "no-referrer"
          lookup "Content-Security-Policy" (simpleHeaders res)
            `shouldBe` Just contentSecurityPolicy

    describe "POST /run" $ do
      it "runs the batch synchronously and lists the files" $ do
        res <- postRun [operationField, Upload "guide.md" sampleMarkdown]
        liftIO $ do
          simpleStatus res `shouldBe` status200
          contains "id=\"results\"" res `shouldBe` True
          contains "guide.md" res `shouldBe` True

      it "offers the zip link when a run leaves something to pack" $ do
        res <- postRun [operationField, Upload "guide.md" sampleMarkdown]
        liftIO $ contains downloadAllBytes res `shouldBe` True

      it "stamps the results with the expiry and the countdown sentence" $ do
        res <- postRun [operationField, Upload "guide.md" sampleMarkdown]
        liftIO $ do
          contains "id=\"results\"" res `shouldBe` True
          contains (TE.encodeUtf8 (expiryAttribute <> "=\"")) res `shouldBe` True
          contains (TE.encodeUtf8 (expirySentence 30)) res `shouldBe` True

      it "withholds the zip link when every file in the run failed" $ do
        res <- postRun
          [operationField, Upload "broken.md" "<!-- mdmaster:toc start -->\n"]
        liftIO $ do
          contains "id=\"results\"" res `shouldBe` True
          contains downloadAllBytes res `shouldBe` False

      it "ignores non-Markdown parts that are within the limits" $ do
        res <- postRun
          [ operationField
          , Upload "logo.png" "\xff\xd8"
          , Upload "guide.md" sampleMarkdown
          ]
        liftIO $ simpleStatus res `shouldBe` status200

      it "accepts under the default limits what the small config rejects" $ do
        res <- postRun
          [operationField, Upload "big.md" (BS.replicate 200 65 <> "\n\n# Title\n")]
        liftIO $ simpleStatus res `shouldBe` status200

      it "serves the diff, the single file and the zip for the stored batch" $ do
        res <- postRun [operationField, Upload "docs/guide.md" sampleMarkdown]
        liftIO $ simpleStatus res `shouldBe` status200
        let bid = batchIdIn res
        liftIO $ BS.null bid `shouldBe` False

        diff <- get ("/batch/" <> bid <> "/file/0/diff")
        liftIO $ do
          simpleStatus diff `shouldBe` status200
          contains (TE.encodeUtf8 (copyHrefAttribute <> "=\""
                      <> batchFileHref (BatchId (TE.decodeUtf8 bid)) 0 <> "\""))
                   diff `shouldBe` True

        one <- get ("/batch/" <> bid <> "/file/0/download")
        liftIO $ do
          simpleStatus one `shouldBe` status200
          lookup "Content-Disposition" (simpleHeaders one)
            `shouldBe` Just "attachment; filename=\"docs/guide.md\""

        archive <- get ("/batch/" <> bid <> "/download")
        liftIO $ do
          simpleStatus archive `shouldBe` status200
          lookup "Content-Disposition" (simpleHeaders archive)
            `shouldBe` Just "attachment; filename=\"mdmaster.zip\""
          BS.isPrefixOf "PK" (BL.toStrict (simpleBody archive)) `shouldBe` True

      it "reports an out-of-range file index as expired rather than crashing" $ do
        res <- postRun [operationField, Upload "guide.md" sampleMarkdown]
        let bid = batchIdIn res
        gone <- get ("/batch/" <> bid <> "/file/9/diff")
        liftIO $ simpleStatus gone `shouldBe` status410

      it "reports a negative file index as expired rather than serving file 0" $ do
        res <- postRun [operationField, Upload "guide.md" sampleMarkdown]
        let bid = batchIdIn res
        file <- get ("/batch/" <> bid <> "/file/-1/download")
        diff <- get ("/batch/" <> bid <> "/file/-1/diff")
        liftIO $ do
          simpleStatus file `shouldBe` status410
          contains expiredBytes file `shouldBe` True
          simpleStatus diff `shouldBe` status410
          contains expiredBytes diff `shouldBe` True

    describe "the diff's ?context= parameter" $ do
      it "answers without the parameter at the standard context, as it always has" $ do
        res <- postRun [operationField, Upload "guide.md" sampleMarkdown]
        let bid = batchIdIn res
        diff <- get ("/batch/" <> bid <> "/file/0/diff")
        liftIO $ do
          simpleStatus diff `shouldBe` status200
          contains (TE.encodeUtf8 ("aria-current=\"true\">" <> contextLabel diffContext <> "</span>")) diff `shouldBe` True

      it "shows bare changes at context 0, the unchanged lines elided" $ do
        res <- postRun [operationField, Upload "guide.md" sampleMarkdown]
        let bid = batchIdIn res
        bare <- get ("/batch/" <> bid <> "/file/0/diff?context=0")
        liftIO $ do
          simpleStatus bare `shouldBe` status200
          contains "body" bare `shouldBe` False
          contains (TE.encodeUtf8 ("aria-current=\"true\">" <> contextLabel 0 <> "</span>")) bare `shouldBe` True

      it "shows the whole document at the cap" $ do
        res <- postRun [operationField, Upload "guide.md" sampleMarkdown]
        let bid = batchIdIn res
        whole <- get ("/batch/" <> bid <> "/file/0/diff?context="
                                        <> BS8.pack (show diffContextCap))
        liftIO $ do
          simpleStatus whole `shouldBe` status200
          contains "body" whole `shouldBe` True
          contains (TE.encodeUtf8 ("aria-current=\"true\">" <> contextLabel diffContextCap <> "</span>")) whole `shouldBe` True

      it "clamps a value above the cap to the cap rather than erroring" $ do
        res <- postRun [operationField, Upload "guide.md" sampleMarkdown]
        let bid = batchIdIn res
        huge <- get ("/batch/" <> bid <> "/file/0/diff?context=99999")
        capped <- get ("/batch/" <> bid <> "/file/0/diff?context="
                                         <> BS8.pack (show diffContextCap))
        liftIO $ do
          simpleStatus huge `shouldBe` status200
          simpleBody huge `shouldBe` simpleBody capped

      it "clamps a negative value to 0, the barest view there is" $ do
        res <- postRun [operationField, Upload "guide.md" sampleMarkdown]
        let bid = batchIdIn res
        negative <- get ("/batch/" <> bid <> "/file/0/diff?context=-2")
        bare <- get ("/batch/" <> bid <> "/file/0/diff?context=0")
        liftIO $ do
          simpleStatus negative `shouldBe` status200
          simpleBody negative `shouldBe` simpleBody bare

      it "refuses a value that does not parse with the rejection every route answers" $ do
        res <- postRun [operationField, Upload "guide.md" sampleMarkdown]
        let bid = batchIdIn res
        rejected <- get ("/batch/" <> bid <> "/file/0/diff?context=standard")
        liftIO $ do
          simpleStatus rejected `shouldBe` status400
          contains (TE.encodeUtf8 badDiffContextMessage) rejected `shouldBe` True

    describe "expired or unknown batches" $ do
      it "renders the expiry page with 410 for a diff" $ do
        res <- get "/batch/nosuchbatch/file/0/diff"
        liftIO $ do
          simpleStatus res `shouldBe` status410
          contains expiredBytes res `shouldBe` True
      it "renders the expiry page with 410 for a zip" $ do
        res <- get "/batch/nosuchbatch/download"
        liftIO $ do
          simpleStatus res `shouldBe` status410
          contains expiredBytes res `shouldBe` True
    describe "request-level rejections" $ do
      it "re-renders the form for an unknown operation" $ do
        res <- postRun
          [Field "operation" "no-such-operation", Upload "guide.md" sampleMarkdown]
        liftIO $ do
          simpleStatus res `shouldBe` status400
          contains unknownOperationBytes res `shouldBe` True
          contains "id=\"run-form\"" res `shouldBe` True

      it "keeps the user's input when an option fails validation" $ do
        res <- postRun
          [ operationField
          , Field (fst intOption) "999"
          , Upload "guide.md" sampleMarkdown
          ]
        liftIO $ do
          simpleStatus res `shouldBe` status400
          contains "field-error" res `shouldBe` True
          contains "value=\"999\"" res `shouldBe` True

      it "answers a rejection with the panel that replaces the form" $ do
        res <- postRun
          [ operationField
          , Field (fst intOption) "999"
          , Upload "guide.md" sampleMarkdown
          ]
        liftIO $ do
          contains "id=\"form-panel\"" res `shouldBe` True
          countOf "id=\"run-form\"" res `shouldBe` 1

      it "answers a rejected upload with the same panel" $ do
        res <- postRun [operationField, Upload "logo.png" "\xff\xd8"]
        liftIO $ do
          contains "id=\"form-panel\"" res `shouldBe` True
          countOf "id=\"run-form\"" res `shouldBe` 1

      it "keeps the user's input when no Markdown file was uploaded" $ do
        res <- postRun
          [ operationField
          , Field (fst intOption) (snd intOption)
          , Upload "logo.png" "\xff\xd8"
          ]
        liftIO $ do
          simpleStatus res `shouldBe` status400
          contains noUsableFilesBytes res `shouldBe` True

  with (appWith tiny) $
    describe "multipart limits (adversarial)" $ do
      it "accepts an upload inside the limits" $ do
        res <- postRun [operationField, Upload "a.md" "# Title\n"]
        liftIO $ simpleStatus res `shouldBe` status200

      it "stops reading past the caps, answering 400 and the parse-time banner" $ do
        res <- postRun [operationField, Upload "logo.png" (BS.replicate 200 65)]
        liftIO $ do
          simpleStatus res `shouldBe` status400
          contains limitsBanner res `shouldBe` True

      it "renders an error banner rather than leaking a stack trace" $ do
        res <- postRun [operationField, Upload "logo.png" (BS.replicate 200 65)]
        liftIO $ do
          contains "error-banner" res `shouldBe` True
          contains "CallStack" res `shouldBe` False
          contains "PayloadTooLarge" res `shouldBe` False

      it "caps a urlencoded flood instead of buffering it" $ do
        res <- request methodPost "/run"
                 [("Content-Type", "application/x-www-form-urlencoded")]
                 (BL.fromStrict ("operation=" <> BS.replicate 200000 65))
        liftIO $ do
          simpleStatus res `shouldBe` status400
          contains limitsBanner res `shouldBe` True

      it "rejects a body that is not a form at all without reading it" $ do
        res <- request methodPost "/run"
                 [("Content-Type", "application/json")]
                 (BL.fromStrict (BS.replicate 200000 65))
        liftIO $ do
          simpleStatus res `shouldBe` status400
          contains "id=\"run-form\"" res `shouldBe` True

      it "answers even the parse-time rejection with a usable form" $ do
        res <- postRun [operationField, Upload "logo.png" (BS.replicate 200 65)]
        liftIO $ do
          contains "id=\"form-panel\"" res `shouldBe` True
          countOf "id=\"run-form\"" res `shouldBe` 1

  with (appWith defaultConfig
          { cfgResultTtl = 7200
          , cfgSourceUrl = "https://example.org/mdmaster-src.tar.gz" }) $
    describe "the footer of an instance with its own TTL and source URL" $ do
      it "offers the source where the instance says it is" $ do
        res <- get "/"
        liftIO $ do
          contains "href=\"https://example.org/mdmaster-src.tar.gz\"" res `shouldBe` True
          contains "https://github.com/all-hands/mdmaster-demo" res `shouldBe` False

      it "states that instance's retention, on the page and on an error page" $ do
        page' <- get "/"
        gone <- get "/batch/no-such-batch/download"
        liftIO $ do
          contains "downloaded for up to 2 hours (" page' `shouldBe` True
          simpleStatus gone `shouldBe` status410
          contains "downloaded for up to 2 hours (" gone `shouldBe` True

  describe "the whole-body ceiling" $ do
    it "stops a chunked framing flood the parser caps do not weigh" $ do
      app <- appWith tiny
      req <- chunkedPost (paddedFileBody 10 ["a.md"])
      res <- WaiTest.runSession (WaiTest.request req) app
      simpleStatus res `shouldBe` status400
      contains limitsBanner res `shouldBe` True

    it "refuses a length-declared body over the ceiling on the length alone" $ do
      app <- appWith tiny
      req <- declaredPost (paddedFileBody 10 ["a.md"])
      res <- WaiTest.runSession (WaiTest.request req) app
      simpleStatus res `shouldBe` status400
      contains limitsBanner res `shouldBe` True

    describe "paired with an acceptance of the same bytes under a higher ceiling" $ do
      let bodyBytes = BL.length ceilingBody

      it "is set by the two configurations either side of the test body" $ do
        fromIntegral (maxRequestBytes (limitsFromConfig lowCeiling)) `shouldSatisfy` (< bodyBytes)
        fromIntegral (maxRequestBytes (limitsFromConfig highCeiling)) `shouldSatisfy` (> bodyBytes)

      it "runs a chunked body under a ceiling above it" $ do
        (res, _) <- sendTo highCeiling Wai.ChunkedBody ceilingBody
        simpleStatus res `shouldBe` status200
        contains "id=\"results\"" res `shouldBe` True

      it "stops a chunked body mid-read under a ceiling below it" $ do
        (res, served) <- sendTo lowCeiling Wai.ChunkedBody ceilingBody
        simpleStatus res `shouldBe` status400
        contains limitsBanner res `shouldBe` True
        served `shouldBe` fromIntegral bodyBytes

      it "runs a length-declared body under a ceiling above it" $ do
        (res, _) <- sendTo highCeiling (declared ceilingBody) ceilingBody
        simpleStatus res `shouldBe` status200
        contains "id=\"results\"" res `shouldBe` True

      it "refuses a length-declared body under a ceiling below it, and drains it unread by the parser" $ do
        (res, served) <- sendTo lowCeiling (declared ceilingBody) ceilingBody
        simpleStatus res `shouldBe` status400
        contains limitsBanner res `shouldBe` True
        served `shouldBe` fromIntegral bodyBytes

    it "accepts an upload sitting close under the caps" $ do
      app <- appWith nearCaps
      res <- withApplication app $ postRun
        [ operationField
        , Upload "a.md" (BS.replicate 33000 97)
        , Upload "b.md" (BS.replicate 33000 97)
        ]
      simpleStatus res `shouldBe` status200

    it "stops a nameless part the parser never measures, at the ceiling" $ do
      let skipped = BL.fromChunks
            [ "--mdmasterTestBoundary\r\nContent-Disposition: form-data\r\n\r\n"
            , BS.replicate (maxRequestBytes (limitsFromConfig tiny) + 1) 65
            , "\r\n--mdmasterTestBoundary--\r\n" ]
      app <- appWith tiny
      req <- chunkedPost skipped
      res <- WaiTest.runSession (WaiTest.request req) app
      simpleStatus res `shouldBe` status400
      contains limitsBanner res `shouldBe` True

    it "runs the heaviest legal request, padded to the ceiling exactly" $ do
      app <- appWith heavy
      req <- declaredPostAs heaviestContentType (paddedTo 0)
      res <- WaiTest.runSession (WaiTest.request req) app
      simpleStatus res `shouldBe` status200

    it "refuses the same request one byte heavier, on its length" $ do
      app <- appWith heavy
      req <- declaredPostAs heaviestContentType (paddedTo 1)
      res <- WaiTest.runSession (WaiTest.request req) app
      simpleStatus res `shouldBe` status400
      contains limitsBanner res `shouldBe` True

  with failedOnlyApp $
    describe "a batch with nothing left to pack" $ do
      it "answers the zip route with an explanation, not an empty archive" $ do
        res <- get "/batch/allfailed/download"
        liftIO $ do
          simpleStatus res `shouldBe` status409
          contains nothingToZipBytes res `shouldBe` True

      it "answers a failed file's download with 410 and the expiry page" $ do
        res <- get "/batch/allfailed/file/0/download"
        liftIO $ do
          simpleStatus res `shouldBe` status410
          contains expiredBytes res `shouldBe` True
          lookup "Content-Disposition" (simpleHeaders res) `shouldBe` Nothing

      it "answers a negative index on a stored batch with 410 too" $ do
        file <- get "/batch/allfailed/file/-1/download"
        diff <- get "/batch/allfailed/file/-1/diff"
        liftIO $ map simpleStatus [file, diff] `shouldBe` [status410, status410]

      it "still serves the file's diff, so the failure can be read" $ do
        res <- get "/batch/allfailed/file/0/diff"
        liftIO $ do
          simpleStatus res `shouldBe` status200
          contains (TE.encodeUtf8 fixtureFailure) res `shouldBe` True
          contains (TE.encodeUtf8 copyClass) res `shouldBe` False


  describe "the download filename" $ do
    it "quotes an ASCII path unchanged" $
      contentDisposition "docs/guide.md"
        `shouldBe` "attachment; filename=\"docs/guide.md\""

    it "spells a name outside ASCII the way RFC 6266 asks" $
      contentDisposition "notes-café.md"
        `shouldBe` "attachment; filename=\"notes-caf_.md\"; \
                   \filename*=UTF-8''notes-caf%C3%A9.md"

    it "strips quotes so the header cannot be split" $
      contentDisposition "a\"b.md" `shouldBe` "attachment; filename=\"ab.md\""
