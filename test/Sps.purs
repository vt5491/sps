module Test.Sps where

import Prelude

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
      -- pure

      -- Assert.assert "gridCell gets correct GridCell val" $ cellVal $ gridCell 0 0 == 0

debugMe :: Effect Unit
debugMe = do
  NODE.log "hello"
