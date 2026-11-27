module Mdmaster.Plugins.Docx.CollectSpec (spec) where

import Data.Text (Text)
import Mdmaster.Core.CommentSpan
  ( authorKey
  , commentStartClass
  , dateKey
  , dateUtcKey
  , deletionClass
  , doneKey
  , doneValue
  , idKey
  , initialsKey
  , insertionClass
  , isCommentMarker
  , parentKey
  )
import Mdmaster.Core.DocxImage (extractedKey, extractedValue)
import Mdmaster.Plugins.Docx.Collect
  (Comment (..), Thread (..), collect, placeholderNote)
import Mdmaster.Plugins.Docx.Gen (EnrichedDoc (..), commented, end, start)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===), (.&&.))
import Text.Pandoc.Definition
import Text.Pandoc.Walk (query)

doc :: [Block] -> Pandoc
doc = Pandoc nullMeta


cellRow :: [Block] -> Row
cellRow bs = Row nullAttr [Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1) bs]

tableOf :: [Block] -> [Row] -> [Row] -> [Row] -> Block
tableOf caption headRows bodyRows footRows =
  Table nullAttr (Caption Nothing caption) []
    (TableHead nullAttr headRows)
    [TableBody nullAttr (RowHeadColumns 0) [] bodyRows]
    (TableFoot nullAttr footRows)

oneCell :: [Block] -> Block
oneCell bs = tableOf [] [] [cellRow bs] []

summary :: Thread -> (Int, Text, [Inline])
summary t = (tBlockIndex t, cId (tRoot t), tAnchor t)

spec :: Spec
spec = do
  describe "anchors" $ do
    it "takes the anchored text from the inlines between the spans" $ do
      let d = doc [Para [Str "before", start "0" "A" [] "note", Str "anchored", end ["0"], Str "after"]]
      map tAnchor (snd (collect d)) `shouldBe` [[Str "anchored"]]

    it "records which block a thread belongs to" $ do
      let d = doc
            [ Para [Str "first"]
            , Para [start "0" "A" [] "note", Str "x", end ["0"]]
            ]
      map tBlockIndex (snd (collect d)) `shouldBe` [1]

    it "closes ids nested inside another comment-end span" $ do
      let d = doc
            [ Para [ start "0" "A" [] "n0", start "1" "B" [] "n1"
                   , Str "shared", end ["0", "1"], Str "tail" ]
            ]
      map tAnchor (snd (collect d)) `shouldBe` [[Str "shared"], [Str "shared"]]

    it "closes an id nested three deep" $ do
      let d = doc
            [ Para [ start "0" "A" [] "n0", start "1" "B" [] "n1"
                   , start "2" "C" [] "n2"
                   , Str "shared", end ["0", "1", "2"], Str "tail" ]
            ]
      map tAnchor (snd (collect d))
        `shouldBe` [[Str "shared"], [Str "shared"], [Str "shared"]]

    it "gives overlapping ranges each their own anchor" $ do
      let d = doc
            [ Para [ start "0" "A" [] "n0", Str "one"
                   , start "1" "B" [] "n1", Str "two", end ["0"]
                   , Str "three", end ["1"], Str "tail" ]
            ]
      map tAnchor (snd (collect d))
        `shouldBe` [ [Str "one", Str "two"]
                   , [Str "two", Str "three"]
                   ]

    it "resolves tracked changes and images inside the anchor" $ do
      let d = doc
            [ Para [ start "0" "A" [] "n"
                   , Span ("", [deletionClass], [(authorKey, "A")]) [Str "gone"]
                   , Span ("", [insertionClass], [(authorKey, "A")]) [Str "new"]
                   , Image ("", [], []) [Str "alt"] ("media/pic.png", "t")
                   , Str "kept"
                   , end ["0"] ] ]
      map tAnchor (snd (collect d))
        `shouldBe` [[ Str "new"
                    , placeholderNote "media/pic.png"
                    , Str "kept" ]]

    it "resolves an image nested inside an anchor's formatting" $ do
      let d = doc
            [ Para [ start "0" "A" [] "n"
                   , Emph [Image ("", [], []) [Str "alt"] ("media/pic.png", "t")]
                   , end ["0"] ] ]
      map tAnchor (snd (collect d))
        `shouldBe` [[Emph [placeholderNote "media/pic.png"]]]

  describe "threads" $ do
    it "nests a reply under its parent" $ do
      let d = doc
            [ Para [ start "0" "A" [] "root"
                   , start "1" "B" [(parentKey, "0")] "reply"
                   , Str "x", end ["0", "1"] ] ]
          threads = snd (collect d)
      (length threads, map (cId . tRoot) threads, map (map cId . tReplies) threads)
        `shouldBe` (1, ["0"], [["1"]])

    it "keeps two separate threads on the same range separate" $ do
      let d = doc
            [ Para [ start "0" "A" [] "t0", start "2" "B" [] "t2"
                   , Str "x", end ["0", "2"] ] ]
      map (cId . tRoot) (snd (collect d)) `shouldBe` ["0", "2"]

    it "reads resolved state and the parent attribute" $ do
      let d = doc [Para [start "3" "A" [(doneKey, doneValue)] "r", Str "x", end ["3"]]]
          roots = map tRoot (snd (collect d))
      map (\c -> (cDone c, cParent c)) roots `shouldBe` [(True, Nothing)]

    it "reports an absent done attribute as unresolved" $ do
      let d = doc [Para [start "3" "A" [] "r", Str "x", end ["3"]]]
      map (cDone . tRoot) (snd (collect d)) `shouldBe` [False]

    it "prefers dateUtc over date" $ do
      let d = doc
            [ Para [ start "4" "A" [(dateKey, "2024-01-04T12:00:00Z")
                                   ,(dateUtcKey, "2024-01-04T17:45:30.5Z")] "n"
                   , Str "x", end ["4"] ] ]
      map (cDate . tRoot) (snd (collect d)) `shouldBe` ["2024-01-04T17:45:30.5Z"]

    it "falls back to date when dateUtc is absent" $ do
      let d = doc [Para [start "0" "A" [(dateKey, "2024-01-01T09:00:00Z")] "n", Str "x", end ["0"]]]
      map (cDate . tRoot) (snd (collect d)) `shouldBe` ["2024-01-01T09:00:00Z"]

    it "carries the comment body, which is the span's own content" $ do
      let d = doc [Para [start "0" "A" [] "the note", Str "x", end ["0"]]]
      map (cBody . tRoot) (snd (collect d)) `shouldBe` [[Str "the note"]]

    it "resolves an image in the comment body" $ do
      let d = doc
            [ Para [ Span ("", [commentStartClass], [(idKey, "0"), (authorKey, "A")])
                       [Str "see ", Image ("", [], []) [Str "alt"] ("media/pic.png", "t")]
                   , Str "x", end ["0"] ] ]
      map (cBody . tRoot) (snd (collect d))
        `shouldBe` [[Str "see ", placeholderNote "media/pic.png"]]

    it "carries the author and initials" $ do
      let d = doc [Para [start "0" "Ada L" [(initialsKey, "AL")] "n", Str "x", end ["0"]]]
          roots = map tRoot (snd (collect d))
      map (\c -> (cAuthor c, cInitials c)) roots `shouldBe` [("Ada L", "AL")]

    it "promotes a reply whose parent is nowhere in the document to a root" $ do
      let d = doc [Para [start "1" "B" [(parentKey, "9")] "orphan", Str "x", end ["1"]]]
          threads = snd (collect d)
      (map (cId . tRoot) threads, map (map cId . tReplies) threads)
        `shouldBe` (["1"], [[]])

    it "attaches a reply of a reply to the thread root" $ do
      let d = doc
            [ Para [ start "0" "A" [] "root"
                   , start "1" "B" [(parentKey, "0")] "reply"
                   , start "2" "C" [(parentKey, "1")] "reply of reply"
                   , Str "x", end ["0", "1", "2"] ] ]
          threads = snd (collect d)
      (map (cId . tRoot) threads, map (map cId . tReplies) threads)
        `shouldBe` (["0"], [["1", "2"]])

    it "keeps a mutually parented pair as two roots instead of losing both" $ do
      let d = doc
            [ Para [ start "0" "A" [(parentKey, "1")] "n0"
                   , start "1" "B" [(parentKey, "0")] "n1"
                   , Str "x", end ["0", "1"] ] ]
          threads = snd (collect d)
      (map (cId . tRoot) threads, map (map cId . tReplies) threads)
        `shouldBe` (["0", "1"], [[], []])

    it "keeps a self-parented comment as its own root" $ do
      let d = doc [Para [start "0" "A" [(parentKey, "0")] "n", Str "x", end ["0"]]]
          threads = snd (collect d)
      (map (cId . tRoot) threads, map (map cId . tReplies) threads)
        `shouldBe` (["0"], [[]])

  describe "comments in tables, figures, footnotes and definition lists" $ do
    it "harvests a comment anchored in a table cell" $ do
      let d = doc [Para [Str "before"], oneCell [commented "0" "celltext"]]
      map summary (snd (collect d)) `shouldBe` [(1, "0", [Str "celltext"])]

    it "reaches a table's caption, head, body and foot, in document order" $ do
      let d = doc
            [ tableOf [commented "c" "cap"]
                [cellRow [commented "h" "head"]]
                [cellRow [commented "b" "body"]]
                [cellRow [commented "f" "foot"]]
            ]
      map summary (snd (collect d))
        `shouldBe` [ (0, "c", [Str "cap"])
                   , (0, "h", [Str "head"])
                   , (0, "b", [Str "body"])
                   , (0, "f", [Str "foot"])
                   ]

    it "harvests a comment anchored in a figure's caption and in its content" $ do
      let d = doc
            [ Figure nullAttr (Caption Nothing [commented "0" "cap"])
                [commented "1" "content"]
            ]
      map summary (snd (collect d))
        `shouldBe` [(0, "0", [Str "cap"]), (0, "1", [Str "content"])]

    it "harvests a comment anchored inside a footnote" $ do
      let d = doc [Para [Str "x", Note [commented "0" "inside"], Str "y"]]
      map summary (snd (collect d)) `shouldBe` [(0, "0", [Str "inside"])]

    it "harvests a comment inside a footnote inside a table cell" $ do
      let d = doc [oneCell [Para [Str "x", Note [commented "0" "deep"]]]]
      map summary (snd (collect d)) `shouldBe` [(0, "0", [Str "deep"])]

    it "harvests a comment anchored in a definition-list definition" $ do
      let d = doc [DefinitionList [([Str "term"], [[commented "0" "meaning"]])]]
      map summary (snd (collect d)) `shouldBe` [(0, "0", [Str "meaning"])]

    it "puts a footnote's own words into an anchor that spans it" $ do
      let d = doc
            [ Para [ start "0" "A" [] "n", Str "before"
                   , Note [Para [Str "aside"]], Str "after", end ["0"] ] ]
      map tAnchor (snd (collect d))
        `shouldBe` [[Str "before", Str "aside", Str "after"]]

    it "harvests a comment inside a footnote inside emphasis" $ do
      let d = doc [Para [Emph [Note [commented "0" "hidden"]]]]
      map summary (snd (collect d)) `shouldBe` [(0, "0", [Str "hidden"])]

    it "harvests a comment inside a footnote inside a link" $ do
      let d = doc
            [ Para [Link nullAttr [Note [commented "0" "linked"]] ("t", "")] ]
      map summary (snd (collect d)) `shouldBe` [(0, "0", [Str "linked"])]

    it "harvests a comment inside a footnote inside nested emphasis" $ do
      let d = doc [Para [Strong [Emph [Note [commented "0" "deep"]]]]]
      map summary (snd (collect d)) `shouldBe` [(0, "0", [Str "deep"])]

    it "puts a footnote inside emphasis into an anchor in document order" $ do
      let d = doc
            [ Para [ start "0" "A" [] "n"
                   , Emph [Str "before", Note [Para [Str "aside"]], Str "after"]
                   , Str "tail", end ["0"] ] ]
      map tAnchor (snd (collect d))
        `shouldBe` [[Emph [Str "before", Str "aside", Str "after"], Str "tail"]]

    it "does not lift a footnote out of a deletion span into the anchor" $ do
      let d = doc
            [ Para [ start "0" "A" [] "n"
                   , Span ("", [deletionClass], [(authorKey, "A")])
                       [Str "gone", Note [Para [Str "aside"]]]
                   , Str "kept", end ["0"] ] ]
      map tAnchor (snd (collect d)) `shouldBe` [[Str "kept"]]

    it "drops a container the lifted footnote leaves empty" $ do
      let d = doc
            [ Para [ start "0" "A" [] "n", Str "x"
                   , Emph [Note [Para [Str "aside"]]], end ["0"] ] ]
      map tAnchor (snd (collect d)) `shouldBe` [[Str "x", Str "aside"]]

    it "does not lift a footnote out of a comment body" $ do
      let d = doc
            [ Para [ Span ("", [commentStartClass], [(idKey, "0"), (authorKey, "A")])
                       [Emph [Note [Para [Str "in the body"]]]]
                   , Str "anchored", end ["0"] ] ]
      map tAnchor (snd (collect d)) `shouldBe` [[Str "anchored"]]

    it "leaves the table itself in the prose, with its spans stripped" $ do
      let d = doc [oneCell [commented "0" "celltext"]]
      fst (collect d) `shouldBe` doc [oneCell [Para [Str "celltext"]]]

  describe "the cleaned document" $ do
    it "removes every comment span from the prose" $ do
      let d = doc [Para [Str "a", start "0" "A" [] "n", Str "b", end ["0"], Str "c"]]
      fst (collect d) `shouldBe` doc [Para [Str "a", Str "b", Str "c"]]

    it "removes a nested comment-end span along with its outer one" $ do
      let d = doc [Para [Str "a", start "0" "A" [] "n", Str "b", end ["0", "1"], Str "c"]]
      fst (collect d) `shouldBe` doc [Para [Str "a", Str "b", Str "c"]]

    it "accepts tracked insertions by unwrapping them" $ do
      let d = doc [Para [Str "a", Span ("", [insertionClass], [(authorKey, "A")]) [Str "new"]]]
      fst (collect d) `shouldBe` doc [Para [Str "a", Str "new"]]

    it "accepts tracked deletions by dropping them" $ do
      let d = doc [Para [Str "a", Span ("", [deletionClass], [(authorKey, "A")]) [Str "gone"]]]
      fst (collect d) `shouldBe` doc [Para [Str "a"]]

    it "replaces an image with a placeholder note" $ do
      let d = doc [Para [Image ("", [], []) [Str "alt"] ("media/pic.png", "t")]]
      fst (collect d) `shouldBe` doc [Para [Emph [Str "[image: media/pic.png, not extracted]"]]]

    it "keeps a resolved image as a reference to its media file" $ do
      let marked = Image ("", [], [(extractedKey, extractedValue)])
                         [Str "alt"] ("media/pic.png", "t")
          d = doc [Para [Str "a", marked, Str "b"]]
      fst (collect d)
        `shouldBe` doc [Para [Str "a", Image ("", [], []) [Str "alt"] ("media/pic.png", "t"), Str "b"]]

    it "keeps a resolved image the cleaner lifted into an anchor or body" $ do
      let marked = Image ("", [], [(extractedKey, extractedValue)])
                         [Str "alt"] ("media/pic.png", "t")
          unmarked = Image ("", [], []) [Str "alt"] ("media/pic.png", "t")
          d = doc [Para [start "0" "A" [] "note", marked, end ["0"]]]
          threads = snd (collect d)
      map tAnchor threads `shouldBe` [[unmarked]]
      map (cBody . tRoot) threads `shouldBe` [[Str "note"]]

    it "drops a resolved image whose deletion dropped it" $ do
      let d = doc
            [ Para [ Str "a"
                   , Span ("", [deletionClass], [(authorKey, "A")])
                       [Image ("", [], [(extractedKey, extractedValue)])
                              [Str "alt"] ("media/pic.png", "t")] ] ]
      fst (collect d) `shouldBe` doc [Para [Str "a"]]

    it "drops a deleted image rather than leaving a placeholder" $ do
      let d = doc
            [ Para [ Str "a"
                   , Span ("", [deletionClass], [(authorKey, "A")])
                       [Image ("", [], []) [Str "alt"] ("media/pic.png", "t")] ] ]
      fst (collect d) `shouldBe` doc [Para [Str "a"]]

    it "leaves a document with no comments untouched" $ do
      let d = doc [Para [Str "plain"], Header 1 nullAttr [Str "H"]]
      collect d `shouldBe` (d, [])

    it "keeps a comment span that never closes from swallowing the document" $ do
      let d = doc [Para [start "0" "A" [] "n", Str "x"], Para [Str "y"]]
      map tAnchor (snd (collect d)) `shouldBe` [[Str "x"]]

    prop "cleans to a fixed point: a second collect changes nothing and finds no threads" $
      \(EnrichedDoc d) ->
        let (cleaned, _)       = collect d
            (recleaned, more)  = collect cleaned
         in recleaned === cleaned .&&. more === []

    prop "leaves no comment markup anywhere: not in the document, not in any thread" $
      \(EnrichedDoc d) ->
        let (cleaned, threads) = collect d
            ofThread t =
              query isResidue (tAnchor t)
                : map (query isResidue . cBody) (tRoot t : tReplies t)
            residue = concat (query isResidue cleaned : concatMap ofThread threads)
         in residue `shouldBe` []

isResidue :: Inline -> [Inline]
isResidue inline = case inline of
  Span (_, classes, _) _
    | isCommentMarker inline
        || deletionClass `elem` classes
        || insertionClass `elem` classes -> [inline]
  _ -> []
