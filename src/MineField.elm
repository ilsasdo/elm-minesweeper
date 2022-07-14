module MineField exposing (..)

import Array exposing (Array, fromList, get, set, toList)
import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick)
import Maybe exposing (withDefault)
import Random exposing (Generator)


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


subscriptions : State -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = PutFlag Cell
    | RevealCell Cell
    | SpreadCell Cell
    | NewGame Int Int Int
    | EndGame Cell
    | PlaceMine ( Int, Int )


type alias State =
    { width : Int
    , height : Int
    , mines : Array Cell
    , mineCount : Int
    }


type alias Cell =
    { x : Int
    , y : Int
    , index : Int
    , nearby : Int
    , mine : Bool
    , hidden : Bool
    }


init : () -> ( State, Cmd Msg )
init _ =
    initModel 9 9 9


initModel : Int -> Int -> Int -> ( State, Cmd Msg )
initModel width height mineCount =
    ( newGame width height mineCount, Cmd.none )


newGame : Int -> Int -> Int -> State
newGame width height mineCount =
    State width height (initEmptyMines width height) mineCount


initEmptyMines : Int -> Int -> Array Cell
initEmptyMines width height =
    List.range 0 ((width * height) - 1)
        |> List.map (initCell width height)
        |> fromList


emptyCell =
    initCell 1 1 0


initCell : Int -> Int -> Int -> Cell
initCell width height index =
    Cell (modBy width index) (index // height) index 0 False True


view : State -> Html Msg
view model =
    div []
        [ button [ onClick (NewGame 9 9 10) ] [ text "New Game" ]
        , viewMap model
        ]


viewMap : State -> Html Msg
viewMap state =
    div [ class "map" ] (List.map (viewRow state) (List.range 0 (state.height - 1)))


viewRow : State -> Int -> Html Msg
viewRow state y =
    state.mines
        |> Array.slice (y * state.width) ((y + 1) * state.width)
        |> Array.map viewCell
        |> toList
        |> div [ class "row" ]


viewCell : Cell -> Html Msg
viewCell cell =
    if cell.hidden then
        div [ class "cell hidden", onClick (RevealCell cell) ] [ text " " ]

    else if cell.mine then
        div [ class "cell mine" ] [ text "💣" ]

    else if cell.nearby == 0 then
        div [ class "cell revealed" ] [ text "" ]

    else
        div [ class ("cell revealed nearby-" ++ String.fromInt cell.nearby) ] [ text (String.fromInt cell.nearby) ]


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        NewGame width height mineCount ->
            ( newGame width height mineCount
            , Random.generate PlaceMine (randomPairGenerator width height)
            )

        RevealCell cell ->
            revealFirstCell cell model

        PlaceMine xyTuple ->
            placeMine (tupleToCell xyTuple model) model

        EndGame cell ->
            endGame cell model

        _ ->
            ( model, Cmd.none )


endGame: Cell -> State -> (State, Cmd Msg)
endGame cell state =
    ({ state | mines = revealAllCells state.mines }, Cmd.none)


revealAllCells mines =
    Array.map (\cell -> {cell | hidden = False }) mines


revealFirstCell : Cell -> State -> ( State, Cmd Msg )
revealFirstCell cell state =
    if cell.mine then
        update (EndGame cell) state

    else
        ( { state
            | mines =
                state.mines
                    |> set cell.index { cell | hidden = False }
          }
        , Cmd.none
        )


revealCell : Int -> Array Cell -> Array Cell
revealCell index mines =
    let
        cell =
            mines
                |> get index
                |> withDefault emptyCell
    in
    set index { cell | hidden = False } mines


tupleToCell : ( Int, Int ) -> State -> Cell
tupleToCell cell model =
    arrayGetValue (Tuple.first cell) (Tuple.second cell) model.width emptyCell model.mines


placeMine : Cell -> State -> ( State, Cmd Msg )
placeMine cell model =
    if model.mineCount == 0 then
        ( model, Cmd.none )

    else if cell.mine then
        ( model, Random.generate PlaceMine (randomPairGenerator model.width model.height) )

    else
        ( { model
            | mineCount = model.mineCount - 1
            , mines = setMine cell model
          }
        , Random.generate PlaceMine (randomPairGenerator model.width model.height)
        )


incrementNeighbourMine : Int -> Array Cell -> Array Cell
incrementNeighbourMine index mines =
    let
        cell =
            mines
                |> get index
                |> withDefault emptyCell
    in
    set index { cell | nearby = cell.nearby + 1 } mines


arrayGetValue x y width defaultValue array =
    get (x + (y * width)) array
        |> withDefault defaultValue


setMine : Cell -> State -> Array Cell
setMine cell state =
    state.mines
        |> set cell.index { cell | mine = True }
        |> incrementNeighbourMine ((cell.x - 1) + ((cell.y - 1) * state.width))
        |> incrementNeighbourMine ((cell.x - 1) + ((cell.y + 0) * state.width))
        |> incrementNeighbourMine ((cell.x - 1) + ((cell.y + 1) * state.width))
        |> incrementNeighbourMine ((cell.x + 0) + ((cell.y - 1) * state.width))
        |> incrementNeighbourMine ((cell.x + 0) + ((cell.y + 1) * state.width))
        |> incrementNeighbourMine ((cell.x + 1) + ((cell.y - 1) * state.width))
        |> incrementNeighbourMine ((cell.x + 1) + ((cell.y + 0) * state.width))
        |> incrementNeighbourMine ((cell.x + 1) + ((cell.y + 1) * state.width))


randomPairGenerator : Int -> Int -> Generator ( Int, Int )
randomPairGenerator width height =
    Random.pair (Random.int 0 (width - 1)) (Random.int 0 (height - 1))
