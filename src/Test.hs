{-# LANGUAGE TemplateHaskell #-}
module Test where

import Test.HUnit
import Graphics.Gloss.Interface.Pure.Game
import TronBot
import Control.Lens

ig = initialGrid

-----------------------------------------------------------------------------------

movetest1 = "move" ~: "should set nextPos down when down key pressed"
    ~: move KeyDown ig^.nextPos ~=? (ig^.nextPos & y -~ 1)

movetest2 = "move" ~: "should set nextPos up when up key pressed"
    ~: move KeyUp ig^.nextPos ~=? (ig^.nextPos & y +~ 1)

movetest3 = "move" ~: "should set nextPos left when left key pressed"
    ~: move KeyLeft ig^.nextPos ~=? (ig^.nextPos & x -~ 1)

movetest4 = "move" ~: "should set nextPos right when right key pressed"
    ~: move KeyRight ig^.nextPos ~=? (ig^.nextPos & x +~ 1)

-----------------------------------------------------------------------------------

fhtest1 = "framehandler" ~: "should move active to next tile"
    ~: frameHandler 1 (newGrid initialTiles (10, 10) (10, 9))^.active ~=? newTile(10, 11) True

fhtest2 = "framehandler" ~: "should set lastPos to the previous active tile"
    ~: frameHandler 1 (newGrid initialTiles (10, 10) (10, 9))^.lastPos ~=? newTile(10, 10) True

fhtest3 = "framehandler" ~: "should reset nextPos to the current active tile"
    ~: frameHandler 1 (newGrid initialTiles (10, 10) (10, 9))^.nextPos ~=? newTile(10, 11) True

-----------------------------------------------------------------------------------

tests = test [movetest1, movetest2, movetest3, movetest4,
              fhtest1, fhtest2, fhtest3]

runtests :: IO Counts
runtests = runTestTT tests
