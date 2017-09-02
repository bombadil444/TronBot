{-# LANGUAGE TemplateHaskell #-}
module Test where

import Test.HUnit
import Graphics.Gloss.Interface.Pure.Game
import TronBot
import Control.Lens

ig = initialGrid

movetest1 :: Test
movetest1 = "move" ~: "should move active tile down when down key pressed"
    ~: move KeyDown ig^.active ~=? (ig^.active & y -~ 1)

movetest2 = "move" ~: "should move active tile up when up key pressed"
    ~: move KeyUp ig^.active ~=? (ig^.active & y +~ 1)

movetest3 = "move" ~: "should move active tile left when left key pressed"
    ~: move KeyLeft ig^.active ~=? (ig^.active & x -~ 1)

movetest4 = "move" ~: "should move active tile right when right key pressed"
    ~: move KeyRight ig^.active ~=? (ig^.active & x +~ 1)

movetest5 = "move" ~: "should set lastPos to the previous active tile"
    ~: move KeyDown (newGrid (15, 10) (14, 10))^.lastPos ~=? newTile(15, 10)

tests = test [movetest1, movetest2, movetest3, movetest4, movetest5]

runtests :: IO Counts
runtests = runTestTT tests
