module Mdmaster.Plugins.Docx.RenderSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Mdmaster.Core.CommentSpan
  (authorKey, doneKey, durableIdKey, initialsKey, parentKey)
import Mdmaster.Core.Marker (markerEnd, markerPrefix, markerStart)
import Mdmaster.Plugins.Docx.Collect (Comment (..), Thread (..))
import Mdmaster.Plugins.Docx.Options (CommentStyle (..))
import Mdmaster.Plugins.Docx.Render
  (Placement (..), agentMeta, inlineText, markerName, renderComments)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), elements, listOf, (===))
import Text.Pandoc.Definition (Block (..), Inline (..), nullAttr)

comment :: Text -> Text -> Text -> Bool -> Text -> Comment
comment cid author date done body = Comment
  { cId        = cid
  , cDurableId = "1000000" <> cid
  , cAuthor    = author
  , cDate      = date
  , cDateLocal = date
  , cInitials  = T.take 2 author
  , cDone      = done
  , cParent    = Nothing
  , cBody      = [Str body]
  }

threaded :: Thread
threaded = Thread
  { tBlockIndex = 0
  , tAnchor     = [Str "Minimum ferry crossings"]
  , tRoot       = (comment "0" "Reviewer A" "2024-01-01T09:00:00Z" False "please clarify")
                    { cDateLocal = "2024-01-01T10:00:00Z" }
  , tReplies    =
      [ (comment "1" "Reviewer B" "2024-01-02T10:00:00Z" False "rewording now")
          { cParent = Just "0" }
      ]
  }

resolved :: Thread
resolved = threaded
  { tBlockIndex = 2
  , tAnchor     = [Str "other anchor"]
  , tRoot       = comment "3" "Reviewer A" "2024-01-03T08:15:00Z" True "fixed already"
  , tReplies    = []
  }

unanchored :: Thread
unanchored = threaded { tAnchor = [], tReplies = [] }

formatted :: Thread
formatted = threaded
  { tAnchor =
      [ Underline [Str "ferry"]
      , Space
      , Span nullAttr [Str "timetable"]
      , Space
      , Link nullAttr [Str "policy"] ("https://example.test/policy", "")
      ]
  , tReplies = []
  }

hostile :: Thread
hostile = threaded
  { tAnchor  = [Str "say \"no\" --> to <!-- C:\\docs"]
  , tReplies = []
  }

trailingSpace :: Thread
trailingSpace = threaded { tAnchor = [Str "the minimum crossings "], tReplies = [] }

texts :: [Block] -> [Text]
texts = concatMap go
  where
    go block = case block of
      Para inlines       -> [inlineText inlines]
      Plain inlines      -> [inlineText inlines]
      Header _ _ inlines -> [inlineText inlines]
      BlockQuote bs      -> texts bs
      BulletList itemss  -> concatMap texts itemss
      RawBlock _ t       -> [t]
      _                  -> []

spec :: Spec
spec = do
  describe "diff-stable" $ do
    it "puts everything after the document, so the prose never moves" $
      case renderComments DiffStable [threaded] of
        Trailing _ -> pure ()
        Inline _   -> expectationFailure "diff-stable must not touch the body"

    it "wraps the block in mdmaster markers so the region is recognisable" $
      case renderComments DiffStable [threaded] of
        Trailing bs -> do
          take 1 (texts bs) `shouldBe` [markerStart markerName]
          take 1 (reverse (texts bs)) `shouldBe` [markerEnd markerName]
        Inline _ -> expectationFailure "expected Trailing"

    it "names the anchor and lists the whole thread" $
      case renderComments DiffStable [threaded] of
        Trailing bs -> do
          texts bs `shouldSatisfy` any (T.isInfixOf "Minimum ferry crossings")
          texts bs `shouldSatisfy` any (T.isInfixOf "please clarify")
          texts bs `shouldSatisfy` any (T.isInfixOf "rewording now")
        Inline _ -> expectationFailure "expected Trailing"

    it "heads the whole block with one Comments heading, which agent mode has not" $
      case (renderComments DiffStable [threaded], renderComments Agent [threaded]) of
        (Trailing stable, Trailing machine) -> do
          filter (== "Comments") (texts stable) `shouldBe` ["Comments"]
          texts machine `shouldSatisfy` notElem "Comments"
        _ -> expectationFailure "expected Trailing from both"

    it "credits each comment by author and day, dropping the time" $
      case renderComments DiffStable [threaded] of
        Trailing bs ->
          texts bs `shouldSatisfy` elem "Reviewer A (2024-01-01): please clarify"
        Inline _ -> expectationFailure "expected Trailing"

  describe "review" $ do
    it "places each thread after the block it annotates" $
      case renderComments Review [threaded, resolved] of
        Inline placed -> map fst placed `shouldBe` [0, 2]
        Trailing _    -> expectationFailure "review must place threads inline"

    it "uses a plain blockquote, not an Obsidian callout" $
      case renderComments Review [threaded] of
        Inline [(_, bs)] -> do
          case bs of
            (BlockQuote _ : _) -> pure ()
            other -> expectationFailure ("expected a BlockQuote, got " <> show (take 1 other))
          T.concat (texts bs) `shouldSatisfy` (not . T.isInfixOf "[!note]")
        other -> expectationFailure ("expected one placement, got " <> show (length' other))

    it "nests a reply inside the thread's quote" $
      case renderComments Review [threaded] of
        Inline [(_, [BlockQuote outer])] ->
          case [nested | BlockQuote nested <- outer] of
            [nested] -> texts nested `shouldSatisfy` any (T.isInfixOf "rewording now")
            other    ->
              expectationFailure ("expected one nested quote, got " <> show (length other))
        _ -> expectationFailure "expected one placement holding one quote"

    it "marks a resolved thread" $
      case renderComments Review [resolved] of
        Inline [(_, bs)] -> T.concat (texts bs) `shouldSatisfy` T.isInfixOf "resolved"
        _ -> expectationFailure "expected one placement"

    it "leaves an unresolved thread unmarked, so the mark means something" $
      case renderComments Review [threaded] of
        Inline [(_, bs)] -> T.concat (texts bs) `shouldSatisfy` (not . T.isInfixOf "resolved")
        _ -> expectationFailure "expected one placement"

  describe "agent" $ do
    it "keeps the prose untouched and lists threads at the end" $
      case renderComments Agent [threaded] of
        Trailing bs -> texts bs `shouldSatisfy` any (T.isInfixOf "anchor:")
        Inline _    -> expectationFailure "agent must not touch the body"

    it "identifies a thread by the durable id, not by Word's volatile w:id" $
      case renderComments Agent [threaded] of
        Trailing bs -> do
          texts bs `shouldSatisfy` any (T.isInfixOf "c:10000000")
          texts bs `shouldSatisfy` notElem "c:0"
        Inline _    -> expectationFailure "expected Trailing"

    it "falls back to the w:id when the file carries no durable id" $
      case renderComments Agent [threaded { tRoot = (tRoot threaded) { cDurableId = "" } }] of
        Trailing bs -> texts bs `shouldSatisfy` elem "c:0"
        Inline _    -> expectationFailure "expected Trailing"

    it "states resolved state explicitly, not by omission" $
      case renderComments Agent [threaded] of
        Trailing bs -> texts bs `shouldSatisfy` any (T.isInfixOf "resolved: false")
        Inline _    -> expectationFailure "expected Trailing"

    it "counts comments, threads and unresolved threads in the front matter" $
      agentMeta [threaded, resolved]
        `shouldBe` [("comments", "3"), ("threads", "2"), ("unresolved", "1")]

    it "counts the unresolved threads rather than the resolved ones" $
      agentMeta [threaded, threaded, resolved]
        `shouldBe` [("comments", "5"), ("threads", "3"), ("unresolved", "2")]

    it "counts zero of everything for an empty thread list" $
      agentMeta [] `shouldBe` [("comments", "0"), ("threads", "0"), ("unresolved", "0")]

  describe "archive" $ do
    it "emits raw HTML metadata at the anchor, followed by the body" $
      case renderComments Archive [threaded] of
        Inline [(0, RawBlock _ metadata : Para body : _)] -> do
          metadata `shouldSatisfy` T.isInfixOf (markerPrefix <> "comment id=\"0\"")
          body `shouldBe` [Str "please clarify"]
        _ -> expectationFailure "expected raw metadata then the body at block 0"

    it "records every recovered field, including the ones other modes drop" $
      case renderComments Archive [threaded] of
        Inline [(_, bs)] -> do
          let raw = T.concat (texts bs)
          mapM_ (\k -> raw `shouldSatisfy` T.isInfixOf (k <> "=\""))
            [ "id", durableIdKey, authorKey, initialsKey
            , "date", "dateLocal", doneKey, parentKey, "anchor"
            ]
        _ -> expectationFailure "expected one placement"

    it "keeps the durable id and the file's own local date, not just the resolved one" $
      case renderComments Archive [threaded] of
        Inline [(_, bs)] -> do
          let record = T.concat (rawTexts bs)
          record `shouldSatisfy` T.isInfixOf "durableId=\"10000000\""
          record `shouldSatisfy` T.isInfixOf "date=\"2024\\-01\\-01T09:00:00Z\""
          record `shouldSatisfy` T.isInfixOf "dateLocal=\"2024\\-01\\-01T10:00:00Z\""
        _ -> expectationFailure "expected one placement"

    it "emits a reply with its parent recorded" $
      case renderComments Archive [threaded] of
        Inline [(_, bs)] -> T.concat (texts bs) `shouldSatisfy` T.isInfixOf "parent=\"0\""
        _ -> expectationFailure "expected one placement"

  describe "an anchor left empty by an accepted deletion" $ do
    it "still gives diff-stable a named heading, never a bare one" $
      case renderComments DiffStable [unanchored] of
        Trailing bs -> texts bs `shouldSatisfy` elem "(no anchored text)"
        Inline _    -> expectationFailure "expected Trailing"

    it "leaves review's anchor line out rather than quoting nothing" $
      case (renderComments Review [threaded], renderComments Review [unanchored]) of
        (Inline [(_, anchored)], Inline [(_, blank)]) -> do
          T.concat (texts anchored)
            `shouldSatisfy` T.isInfixOf "on \"Minimum ferry crossings\""
          T.concat (texts blank) `shouldSatisfy` (not . T.isInfixOf "\"")
        _ -> expectationFailure "expected one placement from each"

    it "names the absence for agent, which states every field explicitly" $
      case renderComments Agent [unanchored] of
        Trailing bs -> texts bs `shouldSatisfy` elem "anchor: none"
        Inline _    -> expectationFailure "expected Trailing"

    it "keeps archive's field, empty, as it already does for an absent parent" $
      case renderComments Archive [unanchored] of
        Inline [(_, bs)] -> do
          let raw = T.concat (texts bs)
          raw `shouldSatisfy` T.isInfixOf "anchor=\"\""
          raw `shouldSatisfy` T.isInfixOf "parent=\"\""
        _ -> expectationFailure "expected one placement"

  describe "an anchor wearing Word's own formatting" $ do
    it "names an underlined, styled, hyperlinked anchor rather than calling it absent" $
      case renderComments DiffStable [formatted] of
        Trailing bs -> do
          texts bs `shouldSatisfy` notElem "(no anchored text)"
          texts bs `shouldSatisfy` elem "ferry timetable policy"
        Inline _    -> expectationFailure "expected Trailing"

    it "reads the text out of all three wrappers, in order" $
      case renderComments Agent [formatted] of
        Trailing bs -> texts bs `shouldSatisfy` elem "anchor: \"ferry timetable policy\""
        Inline _    -> expectationFailure "expected Trailing"

  describe "the archive record's escaping" $ do
    it "leaves no way for anchored prose to end the record early" $
      case renderComments Archive [hostile] of
        Inline [(_, bs)] -> case rawTexts bs of
          [record] -> do
            record `shouldSatisfy` T.isPrefixOf (markerPrefix <> "comment ")
            record `shouldSatisfy` T.isSuffixOf " -->"
            T.count "-->"  record `shouldBe` 1
            T.count "<!--" record `shouldBe` 1
            T.count "--"   record `shouldBe` 2
          other -> expectationFailure ("expected one record, got " <> show (length other))
        _ -> expectationFailure "expected one placement"

    it "keeps a quotation mark and a backslash instead of rewriting them away" $
      case renderComments Archive [hostile] of
        Inline [(_, bs)] -> do
          let record = T.concat (rawTexts bs)
          record `shouldSatisfy` T.isInfixOf "say \\\"no\\\""
          record `shouldSatisfy` T.isInfixOf "C:\\\\docs"
          record `shouldSatisfy` (not . T.isInfixOf "say 'no'")
        _ -> expectationFailure "expected one placement"

    it "records the anchor untrimmed, where the modes a person reads trim it" $
      case (renderComments Archive [trailingSpace], renderComments Agent [trailingSpace]) of
        (Inline [(_, bs)], Trailing ag) -> do
          T.concat (rawTexts bs)
            `shouldSatisfy` T.isInfixOf "anchor=\"the minimum crossings \""
          texts ag `shouldSatisfy` elem "anchor: \"the minimum crossings\""
        _ -> expectationFailure "expected one placement and one trailing block"

  describe "the archive record's escaping, over arbitrary anchors" $ do
    prop "round-trips any anchor through the decoder: drop each backslash, keep the next character" $
      \(AnchorText t) -> archiveAnchor t === Just t

    prop "escapes every anchor so that no two dashes end up adjacent" $
      \(AnchorText t) -> fmap (T.isInfixOf "--") (escapedAnchor t) === Just False

    prop "leaves the record's own delimiters the only dashes that pair up" $
      \(AnchorText t) -> fmap (T.count "--") (archiveRecord t) === Just 2

  describe "no comments" $
    it "produces nothing at all in every mode" $
      map (\style -> isEmpty (renderComments style []))
        [DiffStable, Review, Agent, Archive]
        `shouldBe` [True, True, True, True]

rawTexts :: [Block] -> [Text]
rawTexts = concatMap go
  where
    go block = case block of
      RawBlock _ t -> [t]
      _            -> []

newtype AnchorText = AnchorText Text
  deriving Show

instance Arbitrary AnchorText where
  arbitrary = AnchorText . T.pack <$> listOf (elements "--\\\"<>!/ abZ\n\8627")
  shrink (AnchorText t) = [AnchorText (T.pack s) | s <- shrink (T.unpack t)]

archiveRecord :: Text -> Maybe Text
archiveRecord t =
  case renderComments Archive [threaded { tAnchor = [Str t], tReplies = [] }] of
    Inline [(_, bs)] -> case rawTexts bs of
      [record] -> Just record
      _        -> Nothing
    _ -> Nothing

escapedAnchor :: Text -> Maybe Text
escapedAnchor t = do
  record <- archiveRecord t
  let key = "anchor=\""
  stripped <- T.stripPrefix key (snd (T.breakOn key record))
  T.stripSuffix "\" -->" stripped

decodeValue :: Text -> Text
decodeValue = T.pack . go . T.unpack
  where
    go s = case s of
      ('\\' : c : rest) -> c : go rest
      (c : rest)        -> c : go rest
      []                -> []

archiveAnchor :: Text -> Maybe Text
archiveAnchor = fmap decodeValue . escapedAnchor

isEmpty :: Placement -> Bool
isEmpty p = case p of
  Trailing bs -> null bs
  Inline xs   -> null xs

length' :: Placement -> Int
length' p = case p of
  Trailing bs -> length bs
  Inline xs   -> length xs
