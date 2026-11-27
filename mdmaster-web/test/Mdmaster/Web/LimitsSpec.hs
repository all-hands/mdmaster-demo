module Mdmaster.Web.LimitsSpec (spec) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit, isSpace, toLower)
import Data.IORef (atomicModifyIORef, newIORef)
import Data.Word (Word64)
import Mdmaster.App.Docx.Admission (DocxLimits (..))
import Mdmaster.Core.Operation (InputFormat (..))
import Mdmaster.Web.Config (Config (..), configFromEnv, defaultConfig)
import Mdmaster.Web.Limits
import Mdmaster.Web.TestSupport
  ( Part (..), fileBody, firstOf, heaviestBody, heaviestContentType, md, multipartBody
  , multipartContentType, parserShortfall )
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Parse
import Test.Hspec

body :: BS.ByteString
body = "# Title\n"

small :: Limits
small = (limitsFromConfig defaultConfig)
  { limMaxFiles      = 3
  , limMaxFileBytes  = 100
  , limMaxBatchBytes = 200
  }

smallCount, smallPerFile, smallTotal :: Limits
smallCount   = (limitsFromConfig defaultConfig) { limMaxFiles      = 3  }
smallPerFile = (limitsFromConfig defaultConfig) { limMaxFileBytes  = 64 }
smallTotal   = (limitsFromConfig defaultConfig) { limMaxBatchBytes = 100 }

heavyLimits :: Limits
heavyLimits = (limitsFromConfig defaultConfig)
  { limMaxFiles      = 3
  , limMaxFileBytes  = 40000
  , limMaxBatchBytes = 120000
  }

rawPost :: BS.ByteString -> BL.ByteString -> IO Wai.Request
rawPost contentType whole = do
  next <- newIORef (BL.toChunks whole)
  pure $ Wai.setRequestBodyChunks (atomicModifyIORef next step) Wai.defaultRequest
    { Wai.requestMethod  = "POST"
    , Wai.requestHeaders = [("Content-Type", contentType)]
    }
  where
    step []       = ([], BS.empty)
    step (c : cs) = (cs, c)



runParser
  :: Limits
  -> Wai.Request
  -> IO (Either SomeException ( [(BS.ByteString, BS.ByteString)]
                              , [(BS.ByteString, Parse.FileInfo BL.ByteString)] ))
runParser limits request = try (parseUpload limits request)

runBareParser
  :: Limits
  -> Wai.Request
  -> IO (Either SomeException ( [(BS.ByteString, BS.ByteString)]
                              , [(BS.ByteString, Parse.FileInfo BL.ByteString)] ))
runBareParser limits request =
  try (Parse.parseRequestBodyEx (parseOptions limits) Parse.lbsBackEnd request)

inChunks :: Int -> BL.ByteString -> BL.ByteString
inChunks n = BL.fromChunks . go . BL.toStrict
  where
    go bs | BS.null bs = []
          | otherwise  = let (a, b) = BS.splitAt n bs in a : go b

lineFeedsOnly :: BL.ByteString -> BL.ByteString
lineFeedsOnly = BL.filter (/= 13)

threw :: Show a => String -> Either SomeException a -> IO ()
threw what outcome = case outcome of
  Left _  -> pure ()
  Right v -> expectationFailure (what <> " instead of throwing: " <> show v)

spec :: Spec
spec = do
  describe "limitsFromConfig" $ do
    it "copies the default numbers unchanged" $
      limitsFromConfig defaultConfig `shouldBe`
        Limits { limMaxFiles      = 500
               , limMaxFileBytes  = 10 * 1024 * 1024
               , limMaxBatchBytes = 100 * 1024 * 1024
               , limDocx          = DocxLimits
                   { dlFileBytes  = 50 * 1024 * 1024
                   , dlBatchBytes = 200 * 1024 * 1024
                   }
               }
    it "maps each Config field one-to-one" $ do
      let doctored = defaultConfig
            { cfgMaxFiles      = 7
            , cfgMaxFileBytes  = 8
            , cfgMaxBatchBytes = 9
            , cfgMaxDocxUncompressedBytes      = 10
            , cfgMaxDocxBatchUncompressedBytes = 11
            }
      limitsFromConfig doctored `shouldBe`
        Limits { limMaxFiles      = 7
               , limMaxFileBytes  = 8
               , limMaxBatchBytes = 9
               , limDocx          = DocxLimits { dlFileBytes = 10, dlBatchBytes = 11 }
               }

  describe "parseFailureMessage" $
    it "is the banner the handler answers parse failures with" $
      parseFailureMessage `shouldBe` "The upload exceeded the configured limits."

  describe "parseOptions, straight against the parser" $ do
    it "stops reading past limMaxFiles file parts" $ do
      request <- rawPost multipartContentType
                   (fileBody (replicate 4 ("logo.png", "12345678")))
      threw "parsed the flood" =<< runParser smallCount request

    it "stops reading a part past limMaxFileBytes" $ do
      request <- rawPost multipartContentType
                   (fileBody [("logo.png", BS.replicate 200 65)])
      threw "parsed the oversized part" =<< runParser smallPerFile request

    it "stops reading past limMaxBatchBytes" $ do
      request <- rawPost multipartContentType
                   (fileBody [ ("a.png", BS.replicate 60 65)
                                  , ("b.png", BS.replicate 60 65) ])
      threw "parsed the flood" =<< runParser smallTotal request

    it "parses the same floods under the default limits" $ do
      count <- runParser (limitsFromConfig defaultConfig)
                 =<< rawPost multipartContentType
                       (fileBody (replicate 4 ("logo.png", "12345678")))
      case count of
        Left exc    -> expectationFailure ("threw: " <> show exc)
        Right files -> length (snd files) `shouldBe` 4

      one <- runParser (limitsFromConfig defaultConfig)
              =<< rawPost multipartContentType
                    (fileBody [("logo.png", BS.replicate 200 65)])
      case one of
        Left exc    -> expectationFailure ("threw: " <> show exc)
        Right files -> BL.length (Parse.fileContent (snd (firstOf "a parsed file" (snd files))))
                         `shouldBe` 200

      two <- runParser (limitsFromConfig defaultConfig)
              =<< rawPost multipartContentType
                    (fileBody [ ("a.png", BS.replicate 60 65)
                                   , ("b.png", BS.replicate 60 65) ])
      case two of
        Left exc    -> expectationFailure ("threw: " <> show exc)
        Right files -> length (snd files) `shouldBe` 2

  describe "parseUpload's bounds on the body" $ do
    it "stops reading past maxParts parts, though empty fields cost the parser nothing" $ do
      let flood = multipartBody (replicate 200 (Field "" ""))
      bare <- runBareParser smallCount =<< rawPost multipartContentType flood
      case bare of
        Left exc -> expectationFailure ("the bare parser already refused it: " <> show exc)
        Right (params, _) -> length params `shouldBe` 200
      threw "parsed the flood" =<< runParser smallCount =<< rawPost multipartContentType flood

    it "stops reading past maxParts parts of a body framed with bare line feeds" $ do
      let flood = lineFeedsOnly (multipartBody (replicate 200 (Field "" "")))
      BL.elem 13 flood `shouldBe` False
      bare <- runBareParser smallCount =<< rawPost multipartContentType flood
      case bare of
        Left exc -> expectationFailure ("the bare parser already refused it: " <> show exc)
        Right (params, _) -> length params `shouldBe` 200
      outcome <- runParser smallCount =<< rawPost multipartContentType flood
      either show (const "parsed") outcome `shouldBe` show (TooManyParts (maxParts smallCount))

    it "counts the same flood when its delimiters fall across chunks" $ do
      let flood = inChunks 5 (lineFeedsOnly (multipartBody (replicate 200 (Field "" ""))))
      threw "parsed the flood" =<< runParser smallCount =<< rawPost multipartContentType flood

    it "counts parts whose delimiter falls across two chunks" $ do
      let flood = inChunks 7 (multipartBody (replicate 200 (Field "" "")))
      threw "parsed the flood" =<< runParser smallCount =<< rawPost multipartContentType flood

    it "parses as many parts as one path and one file each, plus the form fields" $ do
      let parts = [Field "operation" "toc"]
            <> concat [ [Upload (BS8.pack ("f" <> show i <> ".md")) body, Field "paths" "p.md"]
                      | i <- [1 .. limMaxFiles smallCount] ]
            <> replicate (maxFormFields - 1) (Field "opt" "v")
      outcome <- runParser smallCount =<< rawPost multipartContentType (multipartBody parts)
      case outcome of
        Left exc -> expectationFailure ("threw: " <> show exc)
        Right (params, files) -> do
          length files `shouldBe` limMaxFiles smallCount
          length params `shouldBe` limMaxFiles smallCount + maxFormFields

    it "fits one path field of maxPathBytes for every file the limits allow" $ do
      let limits = limitsFromConfig defaultConfig
          longPath = BS.replicate maxPathBytes 97
          parts = Field "operation" "toc" : replicate (limMaxFiles limits) (Field "paths" longPath)
      outcome <- runParser limits =<< rawPost multipartContentType (multipartBody parts)
      case outcome of
        Left exc -> expectationFailure ("threw: " <> show exc)
        Right (params, _) -> length params `shouldBe` limMaxFiles limits + 1
      threw "the bare default budget" =<< runBareParser limits { limMaxFiles = 0 }
        =<< rawPost multipartContentType (multipartBody parts)

    it "still bounds the path fields' bytes, by the files the limits allow" $ do
      let parts = replicate 70 (Field "paths" (BS.replicate maxPathBytes 97))
      threw "parsed the paths" =<< runParser smallCount
        =<< rawPost multipartContentType (multipartBody parts)

  describe "pairPaths" $ do
    it "gives each file the path field in its place" $
      pairPaths ["docs/say \"hi\".md", "back\\slash.md"]
                [md "say %22hi%22.md" "a", md "backslash.md" "b"]
        `shouldBe` Right [md "docs/say \"hi\".md" "a", md "back\\slash.md" "b"]

    it "keeps the multipart filenames when no path field was sent" $
      pairPaths [] [md "a.md" "a", md "b/c.md" "c"]
        `shouldBe` Right [md "a.md" "a", md "b/c.md" "c"]

    it "refuses fewer paths than files rather than guessing" $
      pairPaths ["a.md"] [md "a.md" "a", md "b.md" "b"]
        `shouldBe` Left (PathCountMismatch 1 2)

    it "refuses more paths than files rather than guessing" $
      pairPaths ["a.md", "b.md", "c.md"] [md "a.md" "a"]
        `shouldBe` Left (PathCountMismatch 3 1)

    it "refuses paths for an upload with no files" $
      pairPaths ["a.md"] [] `shouldBe` Left (PathCountMismatch 1 0)

  describe "maxRequestBytes" $ do
    it "sits above the batch cap it backs" $
      maxRequestBytes (limitsFromConfig defaultConfig)
        `shouldSatisfy` (> cfgMaxBatchBytes defaultConfig)

    it "budgets each allowed file room for a folder-dropped name's framing" $ do
      let limits = limitsFromConfig defaultConfig
          perFile = (maxRequestBytes limits - limMaxBatchBytes limits - fieldFramingBytes)
                      `div` limMaxFiles limits
          folderName = BS8.pack "notes/harbour-logs/2026/quarter3/ferry-delays.txt"
          rendered = BL.length (multipartBody [Upload folderName BS.empty, Field "paths" folderName])
                       - BL.length (multipartBody [])
      perFile `shouldBe` fileFramingBytes
      fromIntegral rendered `shouldSatisfy` (<= perFile)

    it "grows with the file count, so more files mean more framing room" $
      maxRequestBytes (limitsFromConfig defaultConfig { cfgMaxFiles = cfgMaxFiles defaultConfig + 1 })
        `shouldSatisfy` (> maxRequestBytes (limitsFromConfig defaultConfig))

    it "fits the proxy pairing the example deployment documents" $ do
      maxRequestBytes (limitsFromConfig defaultConfig { cfgMaxBatchBytes = 16777216 })
        `shouldBe` 19435832
      19435832 `shouldSatisfy` (<= (20 * 1024 * 1024 :: Int))
      2 * 19435832 `shouldSatisfy` (> (20 * 1024 * 1024 :: Int))
      maxRequestBytes (limitsFromConfig defaultConfig) `shouldBe` 107516216
      107516216 `shouldSatisfy` \total ->
        total > (100 * 1024 * 1024 :: Int) && total < 110 * 1024 * 1024

    describe "against the example deployment in deploy/" $ do
      it "fits under the proxy's body limit, over the unit's environment" $ do
        unit <- BS.readFile "../deploy/mdmaster.container"
        conf <- BS.readFile "../deploy/nginx-mdmaster.conf"
        let env = unitEnvironment unit
        lookup "MDMASTER_MAX_BATCH_BYTES" env `shouldSatisfy` (/= Nothing)
        case (configFromEnv env, nginxBodyLimits conf) of
          (Left err, _) -> expectationFailure ("the unit's environment: " <> show err)
          (Right config, [limit]) ->
            fromIntegral (maxRequestBytes (limitsFromConfig config))
              `shouldSatisfy` \top -> top <= limit && 2 * top > limit
          (_, limits) -> expectationFailure
            ("expected one client_max_body_size, found " <> show limits)

      it "needs the larger proxy limit the conf names at the default batch cap" $ do
        conf <- BS.readFile "../deploy/nginx-mdmaster.conf"
        conf `shouldSatisfy` BS.isInfixOf "use 110m"
        case nginxBodyLimits conf of
          [limit] -> fromIntegral (maxRequestBytes (limitsFromConfig defaultConfig))
            `shouldSatisfy` \total -> total > limit && total <= 110 * 1024 * 1024
          limits -> expectationFailure
            ("expected one client_max_body_size, found " <> show limits)

    it "fits the heaviest request the limits accept, which the parser and checks accept" $ do
      let limits = heavyLimits
          whole = heaviestBody limits
          full = fromIntegral (BL.length whole) + limMaxFiles limits * parserShortfall
      full `shouldSatisfy` (<= maxRequestBytes limits)
      outcome <- runParser limits =<< rawPost heaviestContentType (inChunks 4096 whole)
      case outcome of
        Left exc -> expectationFailure ("threw: " <> show exc)
        Right (params, files) -> do
          length files `shouldBe` limMaxFiles limits
          length params `shouldBe` limMaxFiles limits + maxFormFields
          let paths = [ BS8.unpack v | (k, v) <- params, k == pathFieldName ]
              uploads = [ (BS8.unpack (Parse.fileName info), BL.toStrict (Parse.fileContent info))
                        | (_, info) <- files ]
          case pairPaths paths uploads >>= checkUpload limits MarkdownInput of
            Left err -> expectationFailure ("refused: " <> show err)
            Right accepted -> sum (map (BS.length . snd) accepted)
                                `shouldBe` limMaxBatchBytes limits - limMaxFiles limits * parserShortfall

    it "leaves the heaviest legal request no more than its framing allowance short" $ do
      let limits = heavyLimits
          slack = maxRequestBytes limits - fromIntegral (BL.length (heaviestBody limits))
                    - limMaxFiles limits * parserShortfall
      slack `shouldSatisfy`
        (< limMaxFiles limits * (fileFramingBytes `div` 6) + fieldFramingBytes `div` 3)

  describe "checkUpload" $ do
    it "rejects a path longer than maxPathBytes, counted in UTF-8 bytes" $ do
      let long = replicate (maxPathBytes - 3) 'a' <> ".md"
          overByUtf8 = replicate (maxPathBytes `div` 2) 'é' <> ".md"
      checkUpload small MarkdownInput [md long body] `shouldBe` Right [md long body]
      checkUpload small MarkdownInput [md (long <> "x") body]
        `shouldBe` Left (PathTooLong (maxPathBytes + 1) maxPathBytes)
      checkUpload small MarkdownInput [md overByUtf8 body]
        `shouldBe` Left (PathTooLong (maxPathBytes + 3) maxPathBytes)

    it "rejects a path with a line break as unsafe, rather than renaming it" $
      checkUpload small MarkdownInput [md "FolderD/two\r\nlines.md" body]
        `shouldBe` Left (UnsafePath "FolderD/two\r\nlines.md")

    it "keeps a quote, which is legal in a name" $
      checkUpload small MarkdownInput [md "FolderD/say \"hi\".md" body]
        `shouldBe` Right [md "FolderD/say \"hi\".md" body]

    it "returns the accepted files with their paths cleaned" $
      checkUpload small MarkdownInput [md "a.md" body] `shouldBe` Right [md "a.md" body]

    it "keeps one file for a path, the later upload, in the place the path first held" $
      checkUpload small MarkdownInput
        [md "a.md" "old", md "b.md" body, md "a.md" "new"]
        `shouldBe` Right [md "a.md" "new", md "b.md" body]

    it "sees two spellings of one path as the same path" $
      checkUpload small MarkdownInput [md "a/./b.md" "old", md "a/b.md" "new"]
        `shouldBe` Right [md "a/b.md" "new"]

    it "counts a repeated path once towards the limits" $
      checkUpload small MarkdownInput (replicate 4 (md "a.md" body))
        `shouldBe` Right [md "a.md" body]

    it "rejects an unsafe path before anything else" $
      checkUpload small MarkdownInput [md "../secrets.md" body]
        `shouldBe` Left (UnsafePath "../secrets.md")

    it "rejects a batch with no Markdown files" $
      checkUpload small MarkdownInput [md "logo.png" "\xff\xd8"]
        `shouldBe` Left (NoAcceptableFiles MarkdownInput)

    it "rejects an empty upload" $
      checkUpload small MarkdownInput [] `shouldBe` Left (NoAcceptableFiles MarkdownInput)

    it "rejects a file over the per-file limit" $
      checkUpload small MarkdownInput [md "big.md" (BS.replicate 200 65)]
        `shouldBe` Left (FileTooLarge "big.md" 200 100)

    it "rejects too many files" $
      checkUpload small MarkdownInput [md (show i <> ".md") body | i <- [1 .. 4 :: Int]]
        `shouldBe` Left (TooManyFiles 4 3)

    it "rejects a batch over the total size limit" $
      checkUpload small MarkdownInput [md (show i <> ".md") (BS.replicate 90 65) | i <- [1 .. 3 :: Int]]
        `shouldBe` Left (BatchTooLarge 270 200)

    it "counts only accepted files towards the limits" $
      checkUpload small MarkdownInput
        [md "a.md" body, md "x.png" (BS.replicate 5000 0), md "b.md" body]
        `shouldBe` Right [md "a.md" body, md "b.md" body]

    it "reports an unsafe path over the oversized files behind it" $
      checkUpload small MarkdownInput
        [md "../secrets.md" body, md "big.md" (BS.replicate 200 65)]
        `shouldBe` Left (UnsafePath "../secrets.md")

    it "reports a per-file failure before the count is consulted" $
      checkUpload small MarkdownInput
        (md "big.md" (BS.replicate 200 65) : [md (show i <> ".md") body | i <- [1 .. 4 :: Int]])
        `shouldBe` Left (FileTooLarge "big.md" 200 100)

    it "reports too many files before the total is consulted" $
      checkUpload small MarkdownInput [md (show i <> ".md") (BS.replicate 90 65) | i <- [1 .. 4 :: Int]]
        `shouldBe` Left (TooManyFiles 4 3)

  describe "acceptsPath" $ do
    it "accepts .md" $ acceptsPath MarkdownInput "a.md" `shouldBe` True
    it "accepts .markdown" $ acceptsPath MarkdownInput "a.markdown" `shouldBe` True
    it "is case-insensitive" $ acceptsPath MarkdownInput "README.MD" `shouldBe` True
    it "accepts nested paths" $ acceptsPath MarkdownInput "docs/a.md" `shouldBe` True
    it "rejects .txt" $ acceptsPath MarkdownInput "a.txt" `shouldBe` False
    it "rejects no extension" $ acceptsPath MarkdownInput "README" `shouldBe` False
    it "rejects a name that merely contains md" $
      acceptsPath MarkdownInput "amd.txt" `shouldBe` False

  describe "accepted extensions per format" $ do
    it "accepts Markdown for a Markdown operation" $
      map (acceptsPath MarkdownInput) ["a.md", "a.markdown", "a.docx"]
        `shouldBe` [True, True, False]

    it "accepts only .docx for a docx operation" $
      map (acceptsPath DocxInput) ["a.docx", "a.md", "a.markdown"]
        `shouldBe` [True, False, False]

    it "ignores case, as the Markdown check already does" $
      map (acceptsPath DocxInput) ["A.DOCX", "a.DocX"] `shouldBe` [True, True]

  describe "safeRelativePath" $ do
    it "accepts a plain name" $ safeRelativePath "a.md" `shouldBe` Just "a.md"
    it "accepts a nested path" $
      safeRelativePath "docs/guide/a.md" `shouldBe` Just "docs/guide/a.md"
    it "strips a leading ./" $ safeRelativePath "./a.md" `shouldBe` Just "a.md"
    it "rejects an absolute path" $ safeRelativePath "/etc/passwd.md" `shouldBe` Nothing
    it "rejects a parent traversal" $ safeRelativePath "../a.md" `shouldBe` Nothing
    it "rejects a traversal in the middle" $
      safeRelativePath "docs/../../a.md" `shouldBe` Nothing
    it "rejects a backslash path" $
      safeRelativePath "docs\\..\\a.md" `shouldBe` Nothing
    it "rejects an empty path" $ safeRelativePath "" `shouldBe` Nothing
    it "rejects a path that is only a separator" $ safeRelativePath "/" `shouldBe` Nothing

    it "rejects a NUL smuggled into a traversal segment" $
      safeRelativePath "..\0/a.md" `shouldBe` Nothing
    it "rejects a bare NUL anywhere in the path" $
      safeRelativePath "a\0.md" `shouldBe` Nothing
    it "rejects control characters that could inject a header" $ do
      safeRelativePath "\n/a.md" `shouldBe` Nothing
      safeRelativePath "a\r\n.md" `shouldBe` Nothing
    it "rejects a traversal hidden behind a ./ segment" $
      safeRelativePath "docs/./../a.md" `shouldBe` Nothing
    it "rejects a doubled leading separator" $
      safeRelativePath "//etc/a.md" `shouldBe` Nothing
    it "rejects a Windows drive path" $ safeRelativePath "C:\\a.md" `shouldBe` Nothing
    it "rejects a bare dot" $ safeRelativePath "." `shouldBe` Nothing
    it "rejects a bare parent" $ safeRelativePath ".." `shouldBe` Nothing
    it "collapses a doubled inner separator" $
      safeRelativePath "docs//a.md" `shouldBe` Just "docs/a.md"
    it "treats a four-dot segment as a literal name, not a traversal" $
      safeRelativePath "....//a.md" `shouldBe` Just "..../a.md"

unitEnvironment :: BS.ByteString -> [(String, String)]
unitEnvironment unit =
  [ (BS8.unpack key, BS8.unpack (BS.drop 1 value))
  | line <- BS8.lines unit
  , Just assignment <- [BS.stripPrefix "Environment=" line]
  , let (key, value) = BS8.break (== '=') assignment
  ]

nginxBodyLimits :: BS.ByteString -> [Word64]
nginxBodyLimits conf =
  [ bytes
  | line <- BS8.lines conf
  , Just rest <- [BS.stripPrefix "client_max_body_size" (BS8.dropWhile isSpace line)]
  , Just bytes <- [size (BS8.takeWhile (/= ';') (BS8.dropWhile isSpace rest))]
  ]
  where
    size value = case BS8.span isDigit value of
      (digits, suffix)
        | BS.null digits -> Nothing
        | otherwise -> (* read (BS8.unpack digits)) <$> unit (BS8.map toLower suffix)
    unit suffix = lookup suffix
      [("", 1), ("k", 1024), ("m", 1024 * 1024), ("g", 1024 * 1024 * 1024)]
