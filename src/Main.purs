module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Sps

main :: Effect Unit
main = do
  let a = doSomething
  log $ "Hello sailor!" <> show a
  -- log =<< readTextFile UTF8 "input.json"
  spsMain
