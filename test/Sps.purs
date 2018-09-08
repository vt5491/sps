module Test.Sps (spsMainTest) where

import Prelude
-- import Prelude (Unit, discard, pure, unit)
import Data.Maybe


import Effect (Effect)
-- import Effect.Console (log)

import Test.Unit (suite, test, timeout)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import Data.Array (filter, (..), (!!), index, length, toUnfoldable, fromFoldable, concat, concatMap, cons, snoc, uncons)
import Node.Stdout (log) as NODE


import Sps
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
  suite "sync code" do
    test "arithmetic" do
      Assert.assert "2 + 2 should be 4" $ (2 + 2) == 4
      Assert.assertFalse "2 + 2 shouldn't be 5" $ (2 + 2) == 5
      Assert.equal 4 (2 + 2)
      Assert.expectFailure "2 + 2 shouldn't be 5" $ Assert.equal 5 (2 + 2)
    test "arithmetic2" do
      Assert.assert "2 + 2 should be 4" $ (2 + 2) == 4
  suite "GridCell ops" do
    test "gridCell" do
      Assert.assert "gridRow works" $ length (gridRow testGrid1 0) == 9
      -- Assert.assert "gridCell works" $ (gridCell testGrid1 0 0) == 0
      Assert.assert "gridCell works" $ (cellVal $ gridCell testGrid1 0 1) == 1
      Assert.assert "gridCell works on fullGrid1" $ cellVal (gridCell fullGrid1 1 3) == 6
      -- pure

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
      -- Assert.assert "second row, second col is correct" $ cellVal (fromMaybeCell $ sgVect1 !! 4) ==  5
      pure unit
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
    test "subGridCellByIndex" do
      let sgVect0 = subGridVect fullGrid1 0
      let sgVect1 = subGridVect fullGrid1 1
      Assert.assert "subGridCellByIndex 0 at index 0 is correct" $ (cellVal $ subGridCellByIndex sgVect0 0) == 0
      Assert.assert "subGridCellByIndex 1 at subGridIndex 3 is correct" $ (cellVal $ subGridCellByIndex sgVect1 3) == 6

    test "subGridCell_ineligibilityTest" do
      let sgVect = subGridVect fullGrid1 1
      let sgCell0 = fromMaybe (cellDefault {}) $ sgVect !! 0
      Assert.assert "subGridCell_ineligibilityTest works for subgrid 0, subGridCell 0"
        $ subGridCell_ineligibilityTest sgCell0 == [4, 6]
  suite "general ops" do
    test "rowHasVal" do
      Assert.assert "rowHasVal finds 5 on row 0" $ rowHasVal fullGrid1 0 5 == true
      Assert.assert "rowHasVal does not find 7 on row 1" $ rowHasVal fullGrid1 1 7 == false
    test "colHasVal" do
      Assert.assert "colHasVal finds 5 on col 1" $ colHasVal fullGrid1 1 5 == true
      Assert.assert "colHasVal does not find 2 on col 8" $ colHasVal fullGrid1 7 2 == false

debugMe :: Effect Unit
debugMe = do
  NODE.log "hello"
