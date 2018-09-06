module Test.Sps where

import Prelude
import Data.Maybe


import Effect (Effect)
import Effect.Console (log)

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
      Assert.assert "gridRowFromString works" $ length (gridRowFromString "000051460") == 9
  suite "SubGrid ops" do
    test "subGridVect" do
      let sgVect1 = subGridVect fullGrid1 0
      Assert.assert "result is proper length" $ length sgVect1 == 9
      Assert.assert "second row, second col is correct" $ cellVal (fromMaybeCell $ sgVect1 !! 4) ==  5

debugMe :: Effect Unit
debugMe = do
  NODE.log "hello"
