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

{-

       8 0 1 2 3 4 5 6 7  ^
       7 8 0 1 2 3 4 5 6  |
       6 7 8 0 1 2 3 4 5
       5 6 7 8 0 1 2 3 4
       4 5 6 7 8 0 1 2 3
       3 4 5 6 7 8 0 1 2  GridSizeY
       2 3 4 5 6 7 8 0 1
       1 2 3 4 5 6 7 8 0
       0 1 2 3 4 5 6 7 8 |
       <-  gridSizeX  -> \/

-- lower bottom corner is (0,0)
-- Top right corner is (8,8)
Top left corner is (0,0)
Bottom right corner is (8,8)

Abstractions:
Cell
Grid, GridCell
SubGrid, SubGridCell: a 3x3 subMatrix

points and offsets are zero based.
The *content* of the gridCells (and subgridCells) are, of course, 1-based to
correspond with the rules of suduko where a user is presented with numbers
1 through 9.  But since this program is primarily concerned with manipulating
and referencing the cells themselves (and not their content), most logic
in this code is 0-8 based.

-- subGrid numbering:
--       6 7 8
--       3 4 5
--       0 1 2
--
-- i.e. everything is ordered "to the right and then up".
subGrid numbering:
      0 1 2
      3 4 5
      6 7 8

i.e. everything is ordered "from top left to the left and then down".

-}
doIt :: Int -> String
doIt n = "hello from doIt"
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
foo :: forall r. { first :: String, last :: String | r } -> String
foo {first, last} = "first=" <> first <>",last=" <> last

type Cell2 = { val :: Int}

-- newtype Cell = Cell { val :: Int, status :: String}
newtype Cell = Cell { val :: Int, status :: String}
-- forall r. newtype Cell = Cell { val :: Int | r}
-- newtype Cell = forall r. Cell { val :: Int | r}
derive instance genericCell :: Generic Cell _
instance showCell :: Show Cell where
  show = genericShow

cellDefault2 = Cell { val: -1, status: ""}

-- cellDefault :: String -> String -> Cell
-- cellDefault "val" s = Cell {val: unMaybeInt $ fromString s, status: ""}
-- cellDefault _ _ = cellDefault2

-- multiProduct req1 opt1 opt2 opt3 = req1 * opt1' * opt2' * opt3'
--     where opt1' = fromMaybe 10 opt1
--           opt2' = fromMaybe 20 opt2
--           opt3' = fromMaybe 30 opt3
cellDefault3 :: Maybe Int -> Maybe String -> Cell
cellDefault3 val status = Cell {val: val', status: status'}
    where val' = fromMaybe (-1) val
          status' = fromMaybe "" status

-- cellDefault4 ::



type GridCell = Cell
type SubGridCell = Cell
type GridRow = Array Cell
type SubGridRow = Array Cell

type Grid = Array GridRow
type SubGrid = Array GridRow
-- newtype Grid = Grid (Array GridRow)
-- derive instance genericGrid :: Generic Grid _
-- instance showGrid :: Show Grid where
--   show = genericShow

-- newtype SubGrid = SubGrid (Array SubGridCell)

-- newtype Note = Note { freq :: Number , durRatio :: Int }
-- derive instance genericNote :: Generic Note _
-- instance showNote :: Show Note where
--   show = genericShow
-- newtype  Row = Row
-- type Row = List Int
-- type Puzzle = List Row
type Row = Array Int
type Puzzle = Array Row

-- showNote :: NoteType -> String
-- showNote n = "note.freq=" <> show n.freq
-- instance showRow :: Show Row where
--   show r = "row2=" <> show r
showRow :: Row -> String
showRow r = "row=" <> show r

showPuzzle :: Puzzle -> String
showPuzzle p = foldr (\x a -> a <> showRow x <> "\n") "" p

showAbcLoop :: Effect Unit
showAbcLoop = do
  -- log $ (showPuzzle [[1,2] [3,4]])
  log $ show "abc" <> "\n" <> "def"
  pure unit

showAbc :: String
showAbc = show "abc"

-------
-- Constants and test variables
--------

-- instance showPuzzle :: Show Puzzle where
--   show p =
testGrid1 = [
  [cellDefault {val: 0}, cellDefault {val: 1}, cellDefault {val: 2}, cellDefault {val: 3}, cellDefault {val: 4}, cellDefault {val: 5}, cellDefault {val: 6}, cellDefault {val: 7}, cellDefault {val: 8} ],
  [cellDefault {val: 1}, cellDefault {val: 2}, cellDefault {val: 3}, cellDefault {val: 4}, cellDefault {val: 5}, cellDefault {val: 6}, cellDefault {val: 7}, cellDefault {val: 8}, cellDefault {val: 0} ]
]

tc = cellDefault {val: 9}

-- https://www.websudoku.com/?level=1&set_id=1096937905
fullGrid1 :: Grid
fullGrid1 = [
  gridRowFromString "000051460",
  gridRowFromString "050600000",
  gridRowFromString "103804007",
  gridRowFromString "000082001",
  gridRowFromString "572000638",
  gridRowFromString "400360000",
  gridRowFromString "600108204",
  gridRowFromString "000007080",
  gridRowFromString "017240000"
]

gr0 = fromMaybe [] $ fullGrid1 !! 0


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
  -- log $ "puz=" <> show puz
  log $ "puz=" <> showPuzzle puz
  pure unit

doSomething :: Int
doSomething = 8

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
-- doIt :: Maybe Int -> Int
-- doIt mn = maybe 0 (\x -> x) mn

-- unbox an Int from a Maybe
-- unMaybeInt :: Maybe Int -> Int
-- unMaybeInt mn = maybe 0 (\x -> x) mn
-- toInt :: Maybe String -> Int
-- toInt mx = maybe 0 (\x -> )
-- toMaybeInt :: String -> Maybe Int
-- toMaybeInt s = fromString s
-- unMaybeGridRow :: Maybe GridRow -> GridRow
-- unMaybeGridRow mx = maybe [] (\x -> x) mx
-- unMaybe :: Maybe a -> a -> a
-- unMaybe ma a = maybe a (\x -> x) ma

-- unMaybeCell2 :: forall r . Maybe { val :: Int | r} -> Cell2
-- unMaybeCell2 mc = maybe ( {val: -1}) (\x -> x) mc

fromMaybeCell :: Maybe Cell -> Cell
fromMaybeCell mc = fromMaybe (cellDefault {}) mc
-- unMaybeCell :: Maybe Cell -> Cell
-- unMaybeCell mc = maybe (Cell {val: -1, status: ""}) (\x -> x) mc
-- unMaybeCell :: forall r. Maybe Cell {val :: Int | r } -> Cell
-- unMaybeCell mc = maybe (Cell {val: -1, status: ""}) (\x -> x) mc
-- unMaybeCell :: forall r. Maybe Cell (val :: Int | r ) -> Cell
-- unMaybeCell mc = maybe (Cell {val: -1, status: ""}) (\x -> x) mc

toInt :: Array String -> Array Int
-- toInt x [] =
-- toInt x xs =
-- toInt xs = foldr (\y a -> cons (doIt (fromString y)) a) [] xs
-- toInt xs = foldr (\y a -> cons (unMaybeInt $ fromString y) a) [] xs
toInt xs = foldr (\y a -> cons (fromMaybe (-1) $ fromString y) a) [] xs

-- doIt2 :: Array (Maybe Int) -> Array Int
-- -- doIt2 x = foldr doIt [] x
-- doIt2 x = foldr (\y a -> cons (doIt y) a) [] x

seedPuzzle :: String -> Puzzle
seedPuzzle x = map toInt elems
  where rows = S.split (S.Pattern "\n") x
        -- elems = map (S.split (S.Pattern "\t")) rows
        elems = map (S.split (S.Pattern ",")) rows

--------
-- SubGrid Functions
--------
-- return a linear array of all the cells in a subgrid.  SubGrid 0 is the upper
-- left corner, subgrid 2 is the top right, and subgrid 8 in bottmost right.
subGridVect :: Grid -> Int -> Array Cell
-- subGridVect g n = [cellDefault {val : 7}]
subGridVect g n = [ gridCell g row col]
  where
        -- initOffset =  case n of
        --                 0 -> 0
        --                 1 -> 4
        --                 2 -> 7
        --                 3 -> 27
        --                 4 -> 30
        --                 5 -> 33
        --                 6 -> 54
        --                 7 -> 57
        --                 8 -> 60
        row = mod n 3
        col = n

------------------
--- Getters etc
------------------
-- cellField :: Cell -> String ->
cellVal :: Cell -> Int
cellVal (Cell {val, status }) = val

-- get a GridRow from a Grid
gridRow :: Grid -> Int ->  GridRow
-- gridRow (Grid g) n = g !! n
-- gridRow g r = unMaybeGridRow $ g !! r
gridRow g r = fromMaybe [] $ g !! r

gridCell :: Grid -> Int -> Int -> GridCell
-- gridCell g r c = (gridRow r) !! c
-- gridCell g r c = unMaybeCell $ (unMaybeGridRow $ g !! r) !! c
gridCell g r c = fromMaybe (cellDefault {val: -1}) $ (fromMaybe [] $ g !! r) !! c

gridRowFromString :: String -> GridRow
-- gridRowFromString s = [cellDefault {val: 1}]
-- gridRowFromString s = [cellDefault {val: firstInt}]
-- gridRowFromString s = foldl (\x a -> snoc a $ cellDefault {val: 1 }) [] $ (S.split (S.Pattern "") s)
-- gridRowFromString s = foldl (\x a -> snoc a $ cellDefault {val: x }) [] ?what
-- gridRowFromString s = foldl (\x a -> accumGridRow a firstInt) [] charArray
-- gridRowFromString s = foldl accumGridRow [] [1,2,3]
gridRowFromString s = foldr (\x a -> cons (cellDefault {val : x}) a) [] intArray
  where charArray = S.split (S.Pattern "") s
        intArray = foldr (\x a -> cons (fromMaybe 0 (fromString x)) a) [] charArray
        -- firstChar = fromMaybe "" $ charArray !! 0
        -- firstInt = fromMaybe 0  (fromString firstChar)

-- accumGridRow :: GridRow -> Int -> GridRow
-- accumGridRow a n = snoc a $ cellDefault {val: n}

-- accumGridRow :: Int -> GridRow -> GridRow
-- accumGridRow n a = snoc a $ cellDefault {val: n}

-- subGrid :: Grid -> Int -> SubGrid
-- subGrid 0 = []

--------------------
--
-- Foreign Functions
--
--------------------
foreign import readPuzzleFile :: String -> String
foreign import cellDefault :: forall r. {|r} -> Cell
