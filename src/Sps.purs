module Sps where

import Effect (Effect)
import Effect.Console (log)
import Data.Foldable
import Data.Functor
import Data.List.Types
import Data.Maybe
-- import Data.String as S (length, fromString)
import Data.String as S
import Data.Int (fromString)
import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List as L
-- import Data.Set
import Data.Array (filter, (..), (!!), index, length, toUnfoldable, fromFoldable, concat, concatMap, cons, snoc, uncons)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.Either (fromRight)
import Data.String.Regex (Regex, regex, split, replace) as RE
import Data.String.Regex.Flags (RegexFlags(..), global, multiline, noFlags) as REF
-- import Data.String.Regex.Flags (RegexFlags, RegexFlagsRec)
import Partial.Unsafe (unsafePartial)

--------------
--
--  Data Defs
--
--------------
-- instance showEffect :: Show Effect Unit where
--   show = genericShow
-- data SudokuChar = SudokuChar "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "_"
-- type SudokuChar = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "_"
-- data SudokuChar = SudokuChar "1" | SudokuChar "2"
-- data ToneIndex = Zero | One | Two | Three | Four | Five
-- data SudokuChar = One | Two

-- newtype Note = Note { freq :: Number , durRatio :: Int }
-- derive instance genericNote :: Generic Note _
-- instance showNote :: Show Note where
--   show = genericShow
-- newtype  Row = Row
-- type Row = List Int
-- type Puzzle = List Row
type Row = Array Int
type Puzzle = Array Row


--------------
--
-- Functions
--
--------------
spsMain :: Effect Unit
spsMain = do
  -- r <- readSudokuInput "input.json"
  -- r <- readSudokuInput "data/puzzles/ez_6843808492_v2.tsv"
  -- log $ r <> "-xyz"
  -- r do
  --   log $ "r=" <> r
  --   pure unit
  -- let r = readPuzzleFile "data/puzzles/ez_6843808492_v3.tsv"
  let r = removeComments $ removeCr $ readPuzzleFile "data/puzzles/ez_6843808492.csv"
  log $ r <> "-js"
  let puz = seedPuzzle r
  log $ "puz=" <> show puz
  pure unit

doSomething :: Int
doSomething = 7

-- "unixize" a file by removing cr's (\r or 0x'0d')
-- dos2unix :: String -> String
-- dos2unix s = S.replace (S.Pattern "\r") (S.Replacement "") s

-- "unixize" a file by removing cr's (\r or 0x'0d')
removeCr :: String -> String
removeCr s = RE.replace noCrRe "" s

-- We need to use a regex and not S.replace because we need the global flag.
noCrRe :: RE.Regex
noCrRe = unsafePartial $ fromRight $ RE.regex "\r" REF.global

removeComments :: String -> String
removeComments s = RE.replace noCommentsRe "" s

noCommentsRe :: RE.Regex
-- noComments = unsafePartial $ fromRight $ RE.regex "[^]--" REF.noFlags
-- noComments = unsafePartial $ fromRight $ RE.regex """^--""" REF.noFlags
-- noComments = unsafePartial $ fromRight $ RE.regex """^--.*\n""" globalMultiline
noCommentsRe = unsafePartial $ fromRight $ RE.regex """^--.*\n""" globalMultiline
-- noComments = unsafePartial $ fromRight $ RE.regex "\\^--" REF.noFlags

globalMultiline :: REF.RegexFlags
globalMultiline = REF.RegexFlags
  { global: true
  , ignoreCase: false
  , multiline: true
  , sticky: false
  , unicode: false
  }
-- global :: RegexFlags
-- global = RegexFlags
--   { global: true
--     , ignoreCase: false
--     , multiline: false
--     , sticky: false
--     , unicode: false
--   }


-- SudokuChar :: String -> String
-- SudokuChar s = case s of

readSudokuInput :: String -> Effect String
readSudokuInput f = readTextFile UTF8 f

-- readSudokuInput :: String -> String
-- readSudokuInput f = do
--   do  (readTextFile UTF8 f)
--     pure unit
--   pure unit
-- parseTsv :: Effect String -> String
-- parseTsv s =
-- seedPuzzle :: String -> Puzzle
-- seedPuzzle
-- parsePuzzleRow :: String -> List
-- parsePuzzleRow r =

-- doIt :: Maybe Int -> Array Int -> Int
-- doIt n _ = maybe 0 (\x -> x) n
doIt :: Maybe Int -> Int
doIt mn = maybe 0 (\x -> x) mn

-- unbox an Int from a Maybe
unMaybeInt :: Maybe Int -> Int
unMaybeInt mn = maybe 69 (\x -> x) mn
-- toInt :: Maybe String -> Int
-- toInt mx = maybe 0 (\x -> )
-- toMaybeInt :: String -> Maybe Int
-- toMaybeInt s = fromString s

toInt :: Array String -> Array Int
-- toInt x [] =
-- toInt x xs =
-- toInt xs = foldr (\y a -> cons (doIt (fromString y)) a) [] xs
toInt xs = foldr (\y a -> cons (unMaybeInt $ fromString y) a) [] xs

doIt2 :: Array (Maybe Int) -> Array Int
-- doIt2 x = foldr doIt [] x
doIt2 x = foldr (\y a -> cons (doIt y) a) [] x

seedPuzzle :: String -> Puzzle
seedPuzzle x = map toInt elems
  where rows = S.split (S.Pattern "\n") x
        -- elems = map (S.split (S.Pattern "\t")) rows
        elems = map (S.split (S.Pattern ",")) rows

--------------------
--
-- Foreign Functions
--
--------------------
foreign import readPuzzleFile :: String -> String
