module Mdmaster.Plugins.Normalize.Smart
  ( smartify
  , leftDoubleQuote
  , rightDoubleQuote
  , leftSingleQuote
  , rightSingleQuote
  , enDash
  , emDash
  , ellipsis
  ) where

import Data.Char (isAlphaNum, isSpace)
import Data.Function (on)
import Data.List (groupBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Definition (Attr, Inline (..), Pandoc)
import Text.Pandoc.Walk (walk)

leftDoubleQuote, rightDoubleQuote, leftSingleQuote, rightSingleQuote :: Char
leftDoubleQuote  = '\x201C'
rightDoubleQuote = '\x201D'
leftSingleQuote  = '\x2018'
rightSingleQuote = '\x2019'

enDash, emDash, ellipsis :: Char
enDash   = '\x2013'
emDash   = '\x2014'
ellipsis = '\x2026'

smartify :: Pandoc -> Pandoc
smartify = walk unshieldAutolink . walk smartifyInlines . walk shieldAutolink

isAutolink :: Inline -> Bool
isAutolink inline = case inline of
  Link (_, classes, _) [Str s] (src, _) ->
    s == fromMaybe src (T.stripPrefix "mailto:" src)
      || classes `elem` [["uri"], ["email"]]
  _ -> False

shieldAttr :: Attr
shieldAttr = ("", ["mdmaster-autolink-text"], [])

shieldAutolink :: Inline -> Inline
shieldAutolink inline = case inline of
  Link attr [Str s] target
    | isAutolink inline -> Link attr [Code shieldAttr s] target
  _ -> inline

unshieldAutolink :: Inline -> Inline
unshieldAutolink inline = case inline of
  Link attr [Code codeAttr s] target
    | codeAttr == shieldAttr -> Link attr [Str s] target
  _ -> inline

data Piece = Piece
  { pieceInline     :: Inline
  , pieceRewritable :: Bool
  , pieceText       :: Text
  }

boundaryChar :: Char
boundaryChar = '\xFFFE'

piece :: Inline -> Piece
piece inline = case inline of
  Str t     -> Piece inline True t
  Space     -> Piece inline False " "
  SoftBreak -> Piece inline False " "
  LineBreak -> Piece inline False " "
  _         -> Piece inline False (T.singleton boundaryChar)

data Cell = Cell Int Char Bool

cellChar :: Cell -> Char
cellChar (Cell _ c _) = c

smartifyInlines :: [Inline] -> [Inline]
smartifyInlines inlines =
    [ rebuild index p
    | (index, p) <- zip [0 ..] pieces
    ]
  where
    merged = spacedEllipses inlines
    pieces = map piece merged
    stream = cells (concat
      [ map ((,) index) (T.unpack (pieceText p))
      | (index, p) <- zip [0 ..] pieces
      ])
    scanned = scan noQuotes boundaryChar stream
    texts = M.fromList
      [ (i, T.pack (map snd group_)) | group_@((i, _) : _) <- grouped ]
    grouped = groupBy ((==) `on` fst) scanned

    rebuild index p
      | pieceRewritable p = Str (M.findWithDefault (pieceText p) index texts)
      | otherwise         = pieceInline p

cells :: [(Int, Char)] -> [Cell]
cells chars = zipWith cell chars (drop 2 closerFrom ++ repeat False)
  where
    cell (index, c) = Cell index c
    closesHere = zipWith closes chars (map Just (drop 1 chars) ++ [Nothing])
    closes (_, c) next = c == '\'' && not (maybe False (isAlphaNum . snd) next)
    closerFrom = scanr (||) False closesHere

scan :: QuoteCtx -> Char -> [Cell] -> [(Int, Char)]
scan _ _ [] = []
scan ctx prev stream@(Cell index c closer : rest) = case c of
  '-'  | hyphens >= 3 -> (index, emDash) : scan ctx '-' (drop 3 stream)
       | hyphens >= 2 -> (index, enDash) : scan ctx '-' (drop 2 stream)
  '.'  | dots >= 3    -> (index, ellipsis) : scan ctx '.' (drop 3 stream)
  '"'  -> let (ctx', q) = doubleQuote ctx prev rest
           in (index, q) : scan ctx' c rest
  '\'' -> let (ctx', q) = singleQuote ctx prev closer rest
           in (index, q) : scan ctx' c rest
  _ -> (index, c) : scan ctx c rest
  where
    hyphens = runOf '-'
    dots    = runOf '.'
    runOf x = length (takeWhile ((== x) . cellChar) (take 3 stream))

newtype QuoteCtx = QuoteCtx [Int]

noQuotes :: QuoteCtx
noQuotes = QuoteCtx [0]

doubleQuote :: QuoteCtx -> Char -> [Cell] -> (QuoteCtx, Char)
doubleQuote (QuoteCtx (_ : outer@(_ : _))) _ _ = (QuoteCtx outer, rightDoubleQuote)
doubleQuote ctx@(QuoteCtx frames) prev rest
  | afterWord prev   = (ctx, rightDoubleQuote)
  | nextIsSpace rest = (ctx, rightDoubleQuote)
  | otherwise        = (QuoteCtx (0 : frames), leftDoubleQuote)

singleQuote :: QuoteCtx -> Char -> Bool -> [Cell] -> (QuoteCtx, Char)
singleQuote inner@(QuoteCtx (open : frames)) _ _ rest
  | open > 0 = if nextIsAlphaNum rest
      then (inner, rightSingleQuote)
      else (QuoteCtx (open - 1 : frames), rightSingleQuote)
singleQuote ctx@(QuoteCtx frames) prev hasCloser rest
  | afterWord prev   = (ctx, rightSingleQuote)
  | nextIsSpace rest = (ctx, rightSingleQuote)
  | hasCloser        = (QuoteCtx (opened frames), leftSingleQuote)
  | otherwise        = (ctx, rightSingleQuote)
  where
    opened (n : outer) = n + 1 : outer
    opened []          = [1]

afterWord :: Char -> Bool
afterWord = isAlphaNum

nextIsSpace :: [Cell] -> Bool
nextIsSpace []      = True
nextIsSpace (x : _) = isSpace (cellChar x)

nextIsAlphaNum :: [Cell] -> Bool
nextIsAlphaNum []      = False
nextIsAlphaNum (x : _) = isAlphaNum (cellChar x)

spacedEllipses :: [Inline] -> [Inline]
spacedEllipses (Str "." : Space : Str "." : Space : Str "." : rest) =
  Str (T.singleton ellipsis) : spacedEllipses rest
spacedEllipses (x : rest) = x : spacedEllipses rest
spacedEllipses [] = []
