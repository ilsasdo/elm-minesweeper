module MineTest exposing (suite)

import Expect exposing (Expectation)
import Matrix
import Minesweeper exposing (initCell, initEmptyMines)
import Test exposing (..)


suite : Test
suite =
    describe "The Minesweeper Module"
        [ test "initEmptyMines width height" (\_ -> Expect.equal (Debug.log "matrix: " (initEmptyMines 2 2)) (initEmptyMines 2 2)),
            test "initEmptyMines x y" (\_ -> Expect.equal (Matrix.get 1 1 (initEmptyMines 2 2)) (Just (initCell (1, 1))))
        ]
