module Test where

import Test.HUnit
import Graphics.Gloss.Interface.Pure.Game
import TronBot

movetest1 :: Test
movetest1 = "move" ~: "should move down when down key pressed"
    ~: move KeyDown (newGrid (10, 10)) ~=? newGrid (10, 9)

tests = test [movetest1]

runtests :: IO Counts
runtests = runTestTT tests
