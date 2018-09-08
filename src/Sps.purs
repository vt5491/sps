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
import Data.Array (filter, (..), (!!), index, length, toUnfoldable, fromFoldable, concat, concatMap, cons, snoc, uncons, reverse)

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
foo :: forall r. { first :: String, last :: String | r } -> String
foo {first,
   last} = "first=" <> first <>",last=" <> last

newtype Cell = Cell { val :: Int, row :: Int, col :: Int}
derive instance genericCell :: Generic Cell _
instance showCell :: Show Cell where
  show = genericShow

type GridCell = Cell
type SubGridCell = Cell
type GridRow = Array Cell
type GridCol = Array Cell
type SubGridRow = Array Cell

type Grid = Array GridRow
-- type SubGrid = Array GridRow
type SubGrid = Array Cell
type Row = Array Int
type Puzzle = Array Row

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
  gridRowFromString 0 "000051460",
  gridRowFromString 1 "050600000",
  gridRowFromString 2 "103804007",
  gridRowFromString 3 "000082001",
  gridRowFromString 5 "572000638",
  gridRowFromString 5 "400360000",
  gridRowFromString 6 "600108204",
  gridRowFromString 7 "000007080",
  gridRowFromString 8 "017240000"
]

fg :: Grid
fg = fullGrid1


gr0 = fromMaybe [] $ fullGrid1 !! 0

gridWidth :: Int
gridWidth = 9

gridHeight :: Int
gridHeight = gridWidth

--------------
--
-- Functions
--
--------------
spsMain :: Effect Unit
spsMain = do
  let r = removeComments $ removeCr $ readPuzzleFile "data/puzzles/ez_6843808492.csv"
  log $ r <> "-js"
  let puz = seedPuzzle r
  log $ "puz=" <> showPuzzle puz
  pure unit

doSomething :: Int
doSomething = 8

-- "unixize" a file by removing cr's (\r or 0x'0d')
removeCr :: String -> String
removeCr s = RE.replace noCrRe "" s

-- We need to use a regex and not S.replace because we need the global flag.
noCrRe :: RE.Regex
noCrRe = unsafePartial $ fromRight $ RE.regex "\r" REF.global

removeComments :: String -> String
removeComments s = RE.replace noCommentsRe "" s

noCommentsRe :: RE.Regex
noCommentsRe = unsafePartial $ fromRight $ RE.regex """^--.*\n""" globalMultiline

globalMultiline :: REF.RegexFlags
globalMultiline = REF.RegexFlags
  { global: true
  , ignoreCase: false
  , multiline: true
  , sticky: false
  , unicode: false
  }

readSudokuInput :: String -> Effect String
readSudokuInput f = readTextFile UTF8 f

fromMaybeCell :: Maybe Cell -> Cell
fromMaybeCell mc = fromMaybe (cellDefault {}) mc

toInt :: Array String -> Array Int
toInt xs = foldr (\y a -> cons (fromMaybe (-1) $ fromString y) a) [] xs

seedPuzzle :: String -> Puzzle
seedPuzzle x = map toInt elems
  where rows = S.split (S.Pattern "\n") x
        elems = map (S.split (S.Pattern ",")) rows

-- rowHasVal fullGrid1 2 7
-- 2 is the row, 7 is the val
rowHasVal :: Grid -> Int -> Int -> Boolean
rowHasVal g rowNum val = (length $ filter (\x -> cellVal x == val) row ) > 0
  where row = gridRow g rowNum

colHasVal :: Grid -> Int -> Int -> Boolean
colHasVal g colNum val = (length $ filter (\x -> cellVal x == val) col ) > 0
  -- where row = gridRow g rowNum
  where col = gridCol g colNum
--------
-- SubGrid Functions
--------
-- return the cell index that indicates the start of where the 3x3 subGrid
-- starts. e.g. subGrid 3 is at index 27.
subGridIndex :: Int -> Int
subGridIndex n = ((n / 3) * (gridWidth * 3)) + (mod n 3) * 3

-- return a linear array of all the cells in a subgrid.  SubGrid 0 is the upper
-- left corner, subgrid 2 is the top right, and subgrid 8 in bottmost right.
subGridVect :: Grid -> Int -> Array Cell
-- subGridVect g n = [cellDefault {val : 7}]
-- subGridVect g n = [ gridCell g row col]
subGridVect g n = [
  gridCellByIndex g sgStart, gridCellByIndex g $ sgStart + 1, gridCellByIndex g $ sgStart + 2,
  gridCellByIndex g $ sgStart + gridWidth, gridCellByIndex g $ sgStart + gridWidth + 1, gridCellByIndex g $ sgStart + gridWidth + 2,
  gridCellByIndex g $ sgStart + 2 * gridWidth, gridCellByIndex g $ sgStart + 2 * gridWidth + 1, gridCellByIndex g $ sgStart + 2 * gridWidth + 2
]
  where
        sgStart = subGridIndex n

-- return the list of values that are "closed".  This is primarly here, so we
-- calculate 'subGridOpenVals', which is dependent on this function.
subGridClosedVals :: SubGrid -> Array Int
subGridClosedVals sg = map (\x -> cellVal x) closedCells
  where closedCells = filter (\x -> (cellVal x) /= 0) sg

-- return the list of values that are "open" (e.g not "satisfied" with a value )
-- in a SubGrid.  This is so we know what value we need to try to fill.
-- This is basically just the inversion of 'subGridClosedVals'
subGridOpenVals :: SubGrid -> Array Int
-- subGridOpenVals sg = []
subGridOpenVals sg = filter (\x -> not $ elem x closedVals) $ 1..gridWidth
  where closedVals = subGridClosedVals sg
-- subGridOpenVals sg = map (\x -> not $ elem x closedVals) closedVals
-- subGridOpenVals sg = map (\x -> cellVal x) openCells
--   where
--         closedCells = filter (\x -> (cellVal x) /= 0) sg
--         closedVals = map (\x -> cellVal x) closedCells
--         openCells = filter (\cell -> not $ elem (cellVal cell) closedVals) sg
-- subGridCell_rowColTest :: SubGridCell ->
-- given a subGrid cell, determine what values cannot be here by doing a
-- grid level row and column check.
subGridCell_ineligibilityTest :: SubGridCell -> Array Int
-- subGridCell_ineligibilityTest c = [4,6]
subGridCell_ineligibilityTest c = [4,6]


------------------
-- Utility Functions
------------------
indexToRowCol :: Int -> Array Int
indexToRowCol n = [n / gridWidth, mod n gridWidth]

emptyCellArray :: Array Cell
emptyCellArray = []
------------------
--- Getters etc
------------------
-- cellField :: Cell -> String ->
cellVal :: Cell -> Int
cellVal (Cell {val, row, col }) = val

-- get a GridRow from a Grid
gridRow :: Grid -> Int ->  GridRow
gridRow g r = fromMaybe [] $ g !! r

-- get a GridCol from a Grid
gridCol :: Grid -> Int ->  GridCol
-- gridCol g c = foldr (\r a  -> cons $ (fromMaybe (cellDefault {}) $ r !! c) a)
gridCol g c = foldr (\r a -> cons (fromMaybe (cellDefault {}) $ r !! c) $ a)
  -- emptyCellArray g
  [] g

-- get gridCell by row and col.
gridCell :: Grid -> Int -> Int -> GridCell
gridCell g r c = fromMaybe (cellDefault {val: -1}) $ (fromMaybe [] $ g !! r) !! c

gridCellByRowCol :: Grid -> Int -> Int -> GridCell
gridCellByRowCol g r c = gridCell g r c

gridCellByIndex :: Grid -> Int -> GridCell
-- gridCellByIndex g n = fromMaybe (cellDefault {val: -1}) $ g !! n
gridCellByIndex g n = gridCellByRowCol g row col
  where rowCol = indexToRowCol n
        row = (fromMaybe (-1) (rowCol !! 0))
        col = (fromMaybe (-1) (rowCol !! 1))

gridRowFromString :: Int -> String -> GridRow
-- gridRowFromString2 n s = [cellDefault {val: 7}]
-- gridRowFromString row s = reverse $ infoArray.rowAccum
gridRowFromString row s =  infoArray.rowAccum
  where charArray = S.split (S.Pattern "") s
        intArray = foldr (\x a -> cons (fromMaybe 0 (fromString x)) a) [] charArray
        infoArray = foldr (\x a -> {info: {row: a.info.row, col: a.info.col - 1},
          rowAccum: cons (cellDefault {val: x, row: a.info.row, col: a.info.col}) a.rowAccum})
          {info: {row: row, col: gridWidth - 1}, rowAccum: []} intArray

-- actually, I don't need this as long as subGrid is in a Vect form
subGridCellByIndex :: SubGrid -> Int -> SubGridCell
-- subGridCellByIndex sg n = cellDefault {}
--TODO: implement this
subGridCellByIndex sg n = cellDefault {}

--------------------
--
-- Foreign Functions
--
--------------------
foreign import readPuzzleFile :: String -> String
foreign import cellDefault :: forall r. {|r} -> Cell
