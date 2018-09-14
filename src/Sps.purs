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

cells are typically identied by row first and the col e.g. (row, col).  This is
the opposite of (x,y) positioning where col would be first, since it corresponds
to 'x'.  However, since the grid is row based, it makes sense to drill down by
row first.

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
-- Note: equality is by pos only, independent of val
instance eqCell :: Eq Cell where
  -- eq (Cell {val, row, col} x) ( Cell {val, row, col} y) = (x.row == y.row)
  eq (Cell a) ( Cell b) = (a.row == b.row && a.col == b.col)
  -- eq x y = (x.row == y.row)

type GridCell = Cell
type SubGridCell = Cell
type GridRow = Array Cell
type GridCol = Array Cell
type SubGridRow = Array Cell

type Grid = Array GridRow
-- type SubGrid = Array GridRow
type SubGrid = Array Cell
-- TODO: Row should be called ValRow, to distinguish between GridRow more clearly
-- type Row = Array Int
type ValRow = Array Int
-- type Puzzle = Array GridRow
type Puzzle = Array ValRow
type SubGridNum = Int
type ColNum = Int
type RowNum = Int
type Val = Int
-- type Col = 0..8
-- data Col2 = 1 | 2
-- data Row2 = A | B
-- data CellVector = SubGrid | GridRow

-- showRow :: Row -> String
showRow :: ValRow -> String
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
  gridRowFromString 4 "572000638",
  gridRowFromString 5 "400360000",
  gridRowFromString 6 "600108204",
  gridRowFromString 7 "000007080",
  gridRowFromString 8 "017240000"
]

unitGrid :: Grid
unitGrid = [
  gridRowFromString 0 "000000000",
  gridRowFromString 1 "000000000",
  gridRowFromString 2 "000000000",
  gridRowFromString 3 "000000000",
  gridRowFromString 4 "000000000",
  gridRowFromString 5 "000000000",
  gridRowFromString 6 "000000000",
  gridRowFromString 7 "000000000",
  gridRowFromString 8 "000000000"
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

-- Create a new grid with the new cell
-- Basically loop over every cell in the grid, and if the cell matches the
-- location of the passed cell, substitute the new cell into the output stream.
-- newGrid :: Grid -> Cell -> Grid
-- newGrid g c = foldr topProcesser [] g
--   where topProcesser = (\x a -> cons (rowProcessor x) a)
--         rowProcessor = (\row -> foldr (\y a -> cons (cellProcessor y) a) [] row)
--         cellProcessor = (\x -> if cellRow x == cellRow c &&  cellCol x == cellCol c
--                                  then c
--                                  else x)

-----------------
--
-- Grid Functions
--
-----------------
newGrid :: Grid -> Array Cell -> Grid
-- newGrid  g cs = g
newGrid g candCells = foldr gridProcesser [] g
  where gridProcesser = (\x a -> cons (rowProcessor x) a)
        rowProcessor = (\row -> foldr (\baselineCell a -> cons (cellProcessor baselineCell) a) [] row)
        -- cellProcessor = (\x -> if cellRow x == cellRow c &&  cellCol x == cellCol c
        -- cellProcessor = (\x -> if any (\cand -> cellRow x == cellRow cand) cs &&  any (\cand -> cellCol x == cellCol cand))
        -- cellProcessor = (\c ->  do
        cellProcessor = (\baselineCell -> fromMaybe (baselineCell) $ do
              -- is baseline cell in the array of candcells?
              findCell baselineCell candCells
        )
        -- cellProcessor = (\c -> fromMaybe (cellDefault {}) $ do
        --                           let mr = findCell c candCells
        --                           r <- findCell c candCells
        --                           -- pure r
        --                           case mr of
        --                             Nothing -> do
        --                               let tmp = printMsg "path1"
        --                               pure c
        --                             -- Just Cell -> pure r
        --                             -- _ -> pure r
        --                             Just (Cell a) -> pure r
        --                             -- Just (Cell) -> pure r
        --                           -- if r == Nothing
        --                           --   then c
        --                           --   else r)
        --                           -- pure unit
        --                 )
gridSize :: Grid -> Int
gridSize g = fromMaybe 0 $ (*) <$> (Just $ length g) <*> (length <$> (g !! 0))

-- do one arbitrage test for each subgrid, accumulating the results in
-- the passed grid.
gridArbitrageRound :: Grid -> Grid
-- gridArbitrageRound g = foldr (\i a -> subGridArbitrage a i) g $ 0..3
gridArbitrageRound g = foldr (\i a -> subGridArbitrage a i) g $ 0..(gridWidth - 1)
-- this is just a dummy for entry into repl.  ok to delete
-- cellP :: Cell -> Array Cell -> Cell
-- -- cellP baselineCell candCells = fromMaybe (cellDefault {val: -2}) $ do
-- cellP baselineCell candCells = fromMaybe (baselineCell) $ do
--   findCell baselineCell candCells

-- newGrid2 :: Grid -> Array Cell -> Grid
-- newGrid2 g cs = foldr topProcesser [] g
--   where topProcesser = (\x a -> cons (rowProcessor x) a)
--   -- where topProcesser = (\x a ->  cons $ fromMaybeCell ((rowProcessor x) !! 0)  a)
--   -- where topProcesser = (\x a -> concat (rowProcessor x) a)
--         rowProcessor = (\row -> foldr (\y a -> cons (candCellsProcessor y) a) [] row)
--         candCellsProcessor = (\origCell ->
--           foldr (\candCell a -> cons (cellProcessor origCell candCell) a) [] cs)
--         cellProcessor = (\origCell candCell -> if cellRow origCell == cellRow candCell &&  cellCol origCell == cellCol candCell
--                                  then candCell
--                                  else origCell)
--         concat = (\x y -> foldr (\x a -> cons x a) x y)


-- rowHasVal fullGrid1 2 7
-- 2 is the row, 7 is the val
rowHasVal :: Grid -> RowNum -> Int -> Boolean
rowHasVal g rowNum val = (length $ filter (\x -> cellVal x == val) row ) > 0
  where row = gridRow g rowNum

colHasVal :: Grid -> ColNum -> Int -> Boolean
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
subGridVect :: Grid -> SubGridNum -> Array Cell
-- subGridVect g n = [cellDefault {val : 7}]
-- subGridVect g n = [ gridCell g row col]
subGridVect g n = [
  gridCellByIndex g sgStart, gridCellByIndex g $ sgStart + 1, gridCellByIndex g $ sgStart + 2,
  gridCellByIndex g $ sgStart + gridWidth, gridCellByIndex g $ sgStart + gridWidth + 1, gridCellByIndex g $ sgStart + gridWidth + 2,
  gridCellByIndex g $ sgStart + 2 * gridWidth, gridCellByIndex g $ sgStart + 2 * gridWidth + 1, gridCellByIndex g $ sgStart + 2 * gridWidth + 2
]
  where
        sgStart = subGridIndex n

-- return a linear array of cells in the subGrid that are open, e.g do not
-- have a definite value.
subGridVectOpen :: Grid -> SubGridNum -> Array Cell
subGridVectOpen g sgn = filter (\c -> cellVal c == 0) sg
  where sg = subGridVect g sgn

-- return the list of values that are "closed".  This is primarly here, so we
-- calculate 'subGridOpenVals', which is dependent on this function.
subGridClosedVals :: SubGrid -> Array Int
-- subGridClosedVals sg = map (\x -> cellVal x) closedCells
--   where closedCells = filter (\x -> (cellVal x) /= 0) sg
subGridClosedVals sg = closedVals sg

-- return the list of values that are "open" (e.g not "satisfied" with a value )
-- in a SubGrid.  This is so we know what value we need to try to fill.
-- This is basically just the inversion of 'subGridClosedVals'
subGridOpenVals :: SubGrid -> Array Int
-- subGridOpenVals sg = []
subGridOpenVals sg = filter (\x -> not $ elem x closedVals) $ 1..gridWidth
  where closedVals = subGridClosedVals sg

-- basically a front-end to 'subGridArbitrageForVal'.  Call that function
-- for each of the possible open values.
-- subGridArbitrage :: Grid -> SubGridNum -> {found :: Int, grid :: Grid}
-- subGridArbitrage g sgn = {found: 1, grid: g}
-- subGridArbitrage g sgn = foldr (\x a -> )
--   where openVals = subGridOpenVals $ subGridVect fg sgn

subGridArbitrage :: Grid -> SubGridNum -> Grid
subGridArbitrage g sgn = foldr (\x a -> subGridArbitrageForVal a sgn x) g openVals
  where openVals = subGridOpenVals $ subGridVect g sgn

-- traverse through the open slots of a subgrid to see if we can find
-- a placement "arbitrage", e.g. be able to determine for sure that a
-- value must belong to a particular (row, col).
-- valSubGridTraversal :: Grid -> SubGridNum -> Val -> Grid
subGridArbitrageForVal :: Grid -> SubGridNum -> Val -> Grid
-- valSubGridTraversal g sgn v = g
-- TODO: update the val of the single cell to have the passed value (if this)
-- is an arbitrage case)
subGridArbitrageForVal g sgn v = if (length r) == 1
                              -- then newGrid g (fromMaybe (cellDefault {}) $ r !! 0)
                              then replaceCell g (fromMaybe (cellDefault {}) $ r !! 0)
                              else g
  where sgvo =  subGridVectOpen g sgn
        r = foldr (\cell a -> if (not $ cellCrossRowExistenceTest g cell v)
          then cons cell a
          else a) [] sgvo
        -- replaceCell = (\x y -> newGrid x $ cellDefault {val: v, row: cellRow y, col: cellCol y})
        replaceCell = (\x y -> newGrid x $ [cellDefault {val: v, row: cellRow y, col: cellCol y}] )


-- subGridCell_rowColTest :: SubGridCell ->
-- given a subGrid cell, determine what values cannot be here by doing a
-- grid level row and column check.
subGridCell_ineligibilityTest :: SubGridCell -> Array Int
-- subGridCell_ineligibilityTest c = [4,6]
subGridCell_ineligibilityTest c = [4,6]

-- do a grid-level row and column check to determine if, at the specified cell's
-- position, if the passed value is present anywhere in row or column span.
-- Basically, a "true" return type means the specified cell cannot be that
-- value.  A false, doesn't mean it *is* that value, just that that value cannot
-- be eliminated as a possibility.
cellCrossRowExistenceTest :: Grid -> Cell -> Val -> Boolean
cellCrossRowExistenceTest g c n = (rowHasVal g rowNum n) || (colHasVal g colNum n)
  where rowNum = cellRow c
        colNum = cellCol c
        -- val = cellVal c

cellColExistenceTest :: Grid -> Cell -> Val -> Boolean
-- cellColExistenceTest g c v = true
cellColExistenceTest g c n = colHasVal g colNum n
  where colNum = cellCol c

-- checkColForVal :: Grid -> ColNum -> Int -> Boolean
-- checkColForVal g c n = found > 0
--   where f = (\row a -> if ((cellVal <$> (row !! c)) == Just n) then (a + 1) else a)
--         found = foldr f 0 g
-----------------
-- Row and Col level functions
-----------------
rowArbitrage :: Grid -> RowNum -> Grid
rowArbitrage g n = g
-- rowArbitrage g n = foldr (\cell a -> ) [] row
--   where row = gridRow g n

rowArbitrageForVal :: Grid -> RowNum -> Val -> Grid
rowArbitrageForVal g r v = g
-- rowArbitrageForVal g r v = foldr (\x a -> )
--   where
--     row = gridRow g r
--     openCells = openCells row

rowClosedVals :: GridRow -> Array Int
-- rowClosedVals r = [1]
-- rowClosedVals r = map (\x -> cellVal x) closedCells
--   where closedCells = filter (\x -> (cellVal x) /= 0) r
rowClosedVals r = closedVals r

rowOpenVals :: GridRow -> Array Int
rowOpenVals r = openVals r


------------------
-- Utility (utils) Functions
------------------
indexToRowCol :: Int -> Array Int
indexToRowCol n = [n / gridWidth, mod n gridWidth]

emptyCellArray :: Array Cell
emptyCellArray = []

deltaGrid :: Grid -> Grid -> Grid
-- deltaGrid g1 g2 = g1
deltaGrid g1 g2 = newGrid unitGrid deltaCells
  where deltaCells = foldr gridIterator [] $ 0..((gridSize fg) - 1)
        gridIterator = (\i a ->
          if (cellVal (gridCellByIndex g1 i)) /= (cellVal (gridCellByIndex g2 i))
            then cons (gridCellByIndex g2 i) a
            else a
          )

-- find a cell with the same row and col from an array of cells (if any).
-- If nothing found, return an empty array.
-- findCell :: Cell -> Array Cell
findCell :: Cell -> Array Cell -> Maybe Cell
-- findCell baselineCell candCells = if ((matchRow == Nothing) || (matchCol == Nothing))
--                   then Nothing
--                   else matchRow
findCell baselineCell candCells = do
  find (\cand ->
    (cellRow cand == cellRow baselineCell)
    && (cellCol cand == cellCol baselineCell))
    candCells
  -- where matchRow = find (\cand -> (cellRow cand) == (cellRow baselineCell)) candCells
  --       matchCol = find (\cand -> (cellCol cand) == (cellCol baselineCell)) candCells
  --       match = find (\cand -> cellRow cand == cellRow baselineCell && cellCol cand == cellCol baselineCell)

-- closedVals :: CellVector -> Array Int
closedVals :: Array Cell -> Array Int
closedVals v = map (\x -> cellVal x) closedCells
  where closedCells = filter (\x -> (cellVal x) /= 0) v

openVals :: Array Cell -> Array Int
openVals v = filter (\x -> not $ elem x closed) $ 1..gridWidth
  where closed = closedVals v
-- gridFormat :: Grid -> String
-- gridFormat g = "123" \n\ "456"

closedCells :: Array Cell -> Array Cell
closedCells v = filter (\cell -> cellVal cell /= 0) v

openCells :: Array Cell -> Array Cell
openCells v = filter (\cell -> cellVal cell == 0) v

------------------
--- Getters etc
------------------
-- cellField :: Cell -> String ->
cellVal :: Cell -> Int
cellVal (Cell {val, row, col }) = val

cellRow :: Cell -> Int
cellRow (Cell {val, row, col }) = row

cellCol :: Cell -> Int
cellCol (Cell {val, row, col }) = col

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
gridCell :: Grid -> RowNum -> ColNum -> GridCell
gridCell g r c = fromMaybe (cellDefault {val: -1}) $ (fromMaybe [] $ g !! r) !! c

gridCellByRowCol :: Grid -> RowNum -> ColNum -> GridCell
gridCellByRowCol g r c = gridCell g r c

gridCellByIndex :: Grid -> Int -> GridCell
-- gridCellByIndex g n = fromMaybe (cellDefault {val: -1}) $ g !! n
gridCellByIndex g n = gridCellByRowCol g row col
  where rowCol = indexToRowCol n
        row = (fromMaybe (-1) (rowCol !! 0))
        col = (fromMaybe (-1) (rowCol !! 1))

gridRowFromString :: RowNum -> String -> GridRow
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
foreign import printGrid :: Grid -> String
foreign import printMsg :: String -> String
