{-# LANGUAGE DeriveGeneric #-}

module Mdmaster.App.Diff
  ( DiffKind (..)
  , DiffLine (..)
  , DiffHunk (..)
  , DiffSegment (..)
  , diffHunks
  , hunkOldLength
  , hunkCounts
  , lineTokens
  , wordDiff
  , wordSegments
  ) where

import Control.DeepSeq (NFData)
import Data.Algorithm.Diff (Diff, PolyDiff (..), getDiff, getGroupedDiff)
import Data.Char (isSpace)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

data DiffKind = Context | Added | Removed
  deriving (Eq, Show, Generic)

instance NFData DiffKind

data DiffLine = DiffLine
  { dlKind :: DiffKind
  , dlText :: Text
  }
  deriving (Eq, Show, Generic)

instance NFData DiffLine

data DiffHunk = DiffHunk
  { dhOldStart :: Int
  , dhNewStart :: Int
  , dhLines    :: [DiffLine]
  }
  deriving (Eq, Show, Generic)

instance NFData DiffHunk

hunkOldLength :: DiffHunk -> Int
hunkOldLength = length . filter ((/= Added) . dlKind) . dhLines

hunkCounts :: [DiffHunk] -> (Int, Int)
hunkCounts hunks = (countOf Added, countOf Removed)
  where
    countOf kind = length [() | hunk <- hunks, line <- dhLines hunk, dlKind line == kind]

data DiffSegment = DiffSegment
  { dsText    :: Text
  , dsChanged :: Bool
  }
  deriving (Eq, Show)

lineTokens :: Text -> [Text]
lineTokens t
  | T.null t  = []
  | otherwise =
      let (word, rest)    = T.break isSpace t
          (spaces, rest') = T.span isSpace rest
       in (word <> spaces) : lineTokens rest'

wordDiff :: DiffLine -> DiffLine -> ([DiffSegment], [DiffSegment])
wordDiff old new = (side oldOf, side newOf)
  where
    groups = getGroupedDiff (lineTokens (dlText old)) (lineTokens (dlText new))

    oldOf (Both l _) = Just (False, l)
    oldOf (First l)  = Just (True, l)
    oldOf (Second _) = Nothing

    newOf (Both _ r) = Just (False, r)
    newOf (Second r) = Just (True, r)
    newOf (First _)  = Nothing

    side pick = [DiffSegment (T.concat ts) changed | (changed, ts) <- mapMaybe pick groups]

wordSegments :: DiffHunk -> [[DiffSegment]]
wordSegments hunk = go (dhLines hunk)
  where
    go [] = []
    go (l : ls) = case dlKind l of
      Context -> plain l : go ls
      _ -> let (run, rest) = break ((== Context) . dlKind) ls
            in runSegments (l : run) ++ go rest

    plain l = [DiffSegment (dlText l) False]

    runSegments run = map lineSegments (numbered 0 0 run)
      where
        numbered _ _ [] = []
        numbered ri ai (l : ls) = case dlKind l of
          Removed -> (ri, l) : numbered (ri + 1) ai ls
          _       -> (ai, l) : numbered ri (ai + 1) ls

        removed = [l | l <- run, dlKind l == Removed]
        added   = [l | l <- run, dlKind l == Added]

        counterpart k l = case dlKind l of
          Removed -> nth k added
          _       -> nth k removed

        lineSegments (k, l) = case (dlKind l, counterpart k l) of
          (Removed, Just other) -> fst (wordDiff l other)
          (_,       Just other) -> snd (wordDiff other l)
          (_,       Nothing)    -> [DiffSegment (dlText l) True]

    nth k = listToMaybe . drop k

diffHunks :: Int -> Text -> Text -> [DiffHunk]
diffHunks context old new
  | old == new = []
  | otherwise  = collect (numbered (getDiff (T.lines old) (T.lines new)))
  where
    numbered :: [Diff Text] -> [(Int, Int, DiffLine)]
    numbered = go 1 1
      where
        go _ _ [] = []
        go o n (d : ds) = case d of
          Both l _ -> (o, n, DiffLine Context l) : go (o + 1) (n + 1) ds
          First l  -> (o, n, DiffLine Removed l) : go (o + 1) n ds
          Second l -> (o, n, DiffLine Added l)   : go o (n + 1) ds

    isChange (_, _, DiffLine k _) = k /= Context

    collect :: [(Int, Int, DiffLine)] -> [DiffHunk]
    collect entries =
      concatMap toHunk (markedRuns (zip (nearAChange (map isChange entries)) entries))

    nearAChange :: [Bool] -> [Bool]
    nearAChange flags = zipWith (\b a -> min b a <= context) before after
      where
        far = context + 1
        closer d changeHere = if changeHere then 0 else min far (d + 1)
        before = drop 1 (scanl closer far flags)
        after  = init (scanr (flip closer) far flags)

    markedRuns xs = case break fst xs of
      (_, [])   -> []
      (_, rest) -> let (run, rest') = span fst rest
                    in map snd run : markedRuns rest'

    toHunk grp = case grp of
      []              -> []
      ((o, n, _) : _) -> [DiffHunk o n [line | (_, _, line) <- grp]]
