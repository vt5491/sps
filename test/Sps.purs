module Test.Sps (spsMainTest) where

import Data.Maybe
import Prelude
import Sps

import Data.Array (filter, (..), (!!), index, length, toUnfoldable, fromFoldable, concat, concatMap, cons, snoc, uncons)
import Effect (Effect)
import Node.Stdout (log) as NODE
import Test.Unit (suite, test, timeout)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
--
-- -- import Node.FS.Aff as FS
-- -- import Node.Encoding (Encoding(..))
--
-- main = runTest do

-- testGrid1 = [
--   [Cell {val: 0}, Cell {val: 1}, Cell {val: 2}, Cell {val: 3}, Cell {val: 4}, Cell {val: 5}, Cell {val: 6}, Cell {val: 7}, Cell {val: 8} ],
--   [Cell {val: 1}, Cell {val: 2}, Cell {val: 3}, Cell {val: 4}, Cell {val: 5}, Cell {val: 6}, Cell {val: 7}, Cell {val: 8}, Cell {val: 0} ]
-- ]
testGrid1 :: Grid
-- testGrid1 = Grid [
testGrid1 = [
  [cellDefault {val: 0}, cellDefault {val: 1}, cellDefault {val: 2}, cellDefault {val: 3}, cellDefault {val: 4}, cellDefault {val: 5}, cellDefault {val: 6}, cellDefault {val: 7}, cellDefault {val: 8} ],
  [cellDefault {val: 1}, cellDefault {val: 2}, cellDefault {val: 3}, cellDefault {val: 4}, cellDefault {val: 5}, cellDefault {val: 6}, cellDefault {val: 7}, cellDefault {val: 8}, cellDefault {val: 0} ]
]

tg2 = [
  [cellDefault]
]
-- https://www.websudoku.com/?level=1&set_id=1096937905
-- 000051460
-- 050600000
-- 103804007
-- 000082001
-- 572000638
-- 400360000
-- 600108204
-- 000007080
-- 017240000
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

spsMainTest = runTest do
  -- let tmp = debugMe
  -- abc = do :: Effect Unit
  --   log "hello"
  -- suite "sync code" do
    -- test "arithmetic" do
    --   Assert.assert "2 + 2 should be 4" $ (2 + 2) == 4
    --   Assert.assertFalse "2 + 2 shouldn't be 5" $ (2 + 2) == 5
    --   Assert.equal 4 (2 + 2)
    --   Assert.expectFailure "2 + 2 shouldn't be 5" $ Assert.equal 5 (2 + 2)
    -- test "arithmetic2" do
    --   Assert.assert "2 + 2 should be 4" $ (2 + 2) == 4
  suite "Grid Level ops" do
    test "newGrid" do
      let updateCell = gridCell fullGrid1 2 4
      let newCell = cellDefault {val: 8, row: cellRow updateCell, col: cellCol updateCell}
      let r = newGrid fullGrid1 newCell
      Assert.assert "newGrid works" $ (cellVal $ gridCellByRowCol r 2 4) == 8
    test "newGrid2" do
      let updateCell = gridCell fullGrid1 2 4
      let newCell = cellDefault {val: 8, row: cellRow updateCell, col: cellCol updateCell}
      let r = newGrid2 fullGrid1 [newCell]
      Assert.assert "newGrid2 works with a single cell" $ (cellVal $ gridCellByRowCol r 2 4) == 8

      let newCell2 = cellDefault {val: 4, row: 7, col: 7}
      let r2 = newGrid2 fullGrid1 [newCell, newCell2]
      Assert.assert "newGrid2 works with multiple cell" $ (cellVal $ gridCellByRowCol r2 2 4) == 8
      Assert.assert "newGrid2 works with multiple cell" $ (cellVal $ gridCellByRowCol r2 7 7) == 4
    test "rowHasVal" do
      Assert.assert "rowHasVal finds 5 on row 0" $ rowHasVal fullGrid1 0 5 == true
      Assert.assert "rowHasVal does not find 7 on row 1" $ rowHasVal fullGrid1 1 7 == false
    test "colHasVal" do
      Assert.assert "colHasVal finds 5 on col 1" $ colHasVal fullGrid1 1 5 == true
      Assert.assert "colHasVal does not find 2 on col 8" $ colHasVal fullGrid1 7 2 == false
    -- test "checkColForVal" do
    --   Assert.assert "col 0 contains a 1" $ checkColForVal fullGrid1 0 1 == true
    --   Assert.assert "col 0 contains a 1" $ colHasVal fullGrid1 0 1 == true
    --   Assert.assert "col 0 does not contain a 3" $ checkColForVal fullGrid1 0 3 == false
  suite "GridCell ops" do
    test "gridCell" do
      Assert.assert "gridRow works" $ length (gridRow testGrid1 0) == 9
      -- Assert.assert "gridCell works" $ (gridCell testGrid1 0 0) == 0
      Assert.assert "gridCell works" $ (cellVal $ gridCell testGrid1 0 1) == 1
      Assert.assert "gridCell works on fullGrid1" $ cellVal (gridCell fullGrid1 1 3) == 6
      -- pure
    test "cellCrossRowExistenceTest" do
      -- let sgVect = subGridVect fullGrid1 1
      -- let sgCell0 = fromMaybe (cellDefault {}) $ sgVect !! 0
      let cell = gridCell fullGrid1 0 0
      Assert.assert "cellCrossRowExistenceTest works for cell at 0,0" $ (cellCrossRowExistenceTest fullGrid1 cell 6)  == true
      Assert.assert "cellCrossRowExistenceTest works for cell at 0,0" $ (cellCrossRowExistenceTest fullGrid1 cell 3)  == false

      -- Assert.assert "gridCell gets correct GridCell val" $ cellVal $ gridCell 0 0 == 0
  suite "GridRow ops" do
    test "gridRowFromString" do
      Assert.assert "gridRowFromString works" $ length (gridRowFromString 1 "000051460") == 9
  suite "getters ops" do
    test "gridCol" do
      Assert.assert "gridCol returns array of proper length" $ length (gridCol fullGrid1 1)  == gridHeight
      Assert.assert "gridCol 1 has a 5 in second pos (pos=1)" $ (cellVal $ fromMaybe (cellDefault {}) (gridCol fullGrid1 1 !! 1))  == 5
        --
  suite "SubGrid ops" do
    test "subGridIndex" do
      Assert.assert "subGridIndex 0 is correct" $ subGridIndex 0 == 0
      Assert.assert "subGridIndex 1 is correct" $ subGridIndex 1 == 3
      Assert.assert "subGridIndex 3 is correct" $ subGridIndex 3 == 27
      Assert.assert "subGridIndex 8 is correct" $ subGridIndex 8 == 60
    test "subGridVect" do
      let sgVect1 = subGridVect fullGrid1 1
      -- Assert.assert "result is proper length" $ length sgVect1 == 9
      Assert.assert "subgrid row 0, col 1 is correct" $ cellVal (fromMaybeCell $ sgVect1 !! 1) ==  5
      Assert.assert "subgrid row 1, col 0 is correct" $ cellVal (fromMaybeCell $ sgVect1 !! 3) ==  6
      Assert.assert "subgrid row 2, col 2 is correct" $ cellVal (fromMaybeCell $ sgVect1 !! 8) ==  4
      Assert.assert "second row, second col is correct" $ cellVal (fromMaybeCell $ sgVect1 !! 4) ==  0
      let sgVect3 = subGridVect fullGrid1 3
      -- let tmp = printObj {o: sgVect3}
      -- let tmp = printArray sgVect3
      Assert.assert "subgrid number 3 has correct row for second subgrid row" $ cellRow (fromMaybeCell $ sgVect3 !! 3) == 4
      -- pure unit
    test "indexToRowCol" do
      Assert.assert "indexToRowCol 0 works" $ fromMaybe 0 (indexToRowCol 0 !! 0 )== 0
      Assert.assert "indexToRowCol 1 returns proper row" $ (fromMaybe 0 $ (indexToRowCol 1 !! 0)) == 0
      Assert.assert "indexToRowCol 1 returns proper col " $ (fromMaybe 0 $ (indexToRowCol 1 !! 0)) == 0
      Assert.assert "indexToRowCol 9 returns properl row" $ (fromMaybe 0 $ (indexToRowCol 9 !! 0)) == 1
      Assert.assert "indexToRowCol 80 return proper row" $ (fromMaybe 0 $ (indexToRowCol 80 !! 0)) == 8
      Assert.assert "indexToRowCol 80 return proper col" $ (fromMaybe 0 $ (indexToRowCol 80 !! 1)) == 8
    test "subGridOpenList" do
      let sgVect1 = subGridVect fullGrid1 0
      Assert.assert "subGridOpenVals is correct for subGrid 0" $ (subGridOpenVals sgVect1) == [2, 4, 6, 7, 8, 9]
    -- test "subGridCellByIndex" do
    --   let sgVect0 = subGridVect fullGrid1 0
    --   let sgVect1 = subGridVect fullGrid1 1
    --   Assert.assert "subGridCellByIndex 0 at index 0 is correct" $ (cellVal $ subGridCellByIndex sgVect0 0) == 0
    --   Assert.assert "subGridCellByIndex 1 at subGridIndex 3 is correct" $ (cellVal $ subGridCellByIndex sgVect1 3) == 6

      -- cellCrossRowExistenceTest
    test "subGridCell_ineligibilityTest" do
      let sgVect = subGridVect fullGrid1 1
      let sgCell0 = fromMaybe (cellDefault {}) $ sgVect !! 0
      Assert.assert "subGridCell_ineligibilityTest works for subgrid 0, subGridCell 0"
        $ subGridCell_ineligibilityTest sgCell0 == [4, 6]
    test "subGridArbitrageForVal" do
      Assert.assert "subGridArbitrageForVal pre-cond" $ (cellVal <$> (subGridVect fullGrid1 3) !! 8) == Just 0
      let gridResult = subGridArbitrageForVal fullGrid1 3 1
      let sgVect = subGridVect gridResult 3
      Assert.assert "subGridArbitrageForVal in sg 3 for val=1 is an arbitrage"
        $ (cellVal <$> (sgVect !! 8)) == Just 1
  -- suite "general ops" do
  --   test "rowHasVal" do
  --     Assert.assert "rowHasVal finds 5 on row 0" $ rowHasVal fullGrid1 0 5 == true
  --     Assert.assert "rowHasVal does not find 7 on row 1" $ rowHasVal fullGrid1 1 7 == false
  --   test "colHasVal" do
  --     Assert.assert "colHasVal finds 5 on col 1" $ colHasVal fullGrid1 1 5 == true
  --     Assert.assert "colHasVal does not find 2 on col 8" $ colHasVal fullGrid1 7 2 == false
  -- suite "utils" do
  --   test "deltaGrid" do
  --     let deltaCell = cellDefault {val: 1, row: 3, col: 2}
  --     let fg2 = newGrid fullGrid1 deltaCell
  --     Assert.assert "pre-cond for gridDelta" $ (cellVal $ gridCellByRowCol fg2 3 2) == 1
  --     let gdelta = deltaGrid fullGrid1 fg2
  --     Assert.assert "gridDelta has delta in proper place" $ (cellVal $ gridCellByRowCol gdelta 3 2) == 1
  --     Assert.assert "gridDelta has zero in proper place" $ (cellVal $ gridCellByRowCol gdelta 2 0) == 0

debugMe :: Effect Unit
debugMe = do
  NODE.log "hello"

foreign import printObj :: {o :: {}} -> String
-- foreign import printArray :: {a :: Array Cell } -> String
foreign import printArray :: Array Cell  -> String
