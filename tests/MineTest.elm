module MineTest exposing (suite)

import Expect exposing (Expectation)
import Mine exposing (Mine, emptyMine)
import Minesweeper exposing (initEmptyMineField, initEmptyRow)
import Test exposing (..)


suite : Test
suite =
    describe "The Minesweeper Module"
        [ test "empty Mine" (\_ -> Expect.equal (emptyMine 4).i 4)
        , test "empty map initialized" (\_ -> Expect.equal (List.length (initEmptyRow 4)) 4)
        ]
