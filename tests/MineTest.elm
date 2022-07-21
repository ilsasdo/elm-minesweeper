module MineTest exposing (suite)

import Expect exposing (Expectation)
import Matrix
import Minesweeper exposing (initCell, initEmptyMines)
import Test exposing (..)


suite : Test
suite =
    describe "The Minesweeper Module"
        [ test "initEmptyMines x y" (\_ -> Expect.equal (Matrix.get 1 1 (initEmptyMines 2 2)) (Just (initCell ( 1, 1 ))))
        , test "matrixSet x y" (\_ ->
            let
                matrix = Matrix.initialize 1 10 (\(a, b) -> a + b)
            in
                Expect.equal (Matrix.get 1 5 matrix) (Just 6)
        )
        ]
