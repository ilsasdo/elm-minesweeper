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


type alias Cell =
    { x : Int, y : Int }


type Msg
    = PutFlag Cell
    | DiscoverCell Cell
    | SpreadCell Cell
    | NewGame Int Int Int
    | PlaceMine Cell


type alias State =
    { width : Int
    , height : Int
    , mines : Array Bool
    , mineCount : Int
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

initEmptyMines : Int -> Int -> Array Bool
initEmptyMines width height =
    Array.repeat (height * width) False


view : State -> Html Msg
view model =
    div []
        [ button [ onClick (NewGame 9 9 9) ] [ text "Nuovo Gioco" ]
        , viewMap model
        ]

viewMap : State -> Html Msg
viewMap state =
    div [ class "map" ] (List.map (viewRow state) (List.range 0 ((Array.length state.mines) // state.width)))

viewRow : State -> Int -> Html Msg
viewRow state r =
    div [ class "row" ] (toList (Array.map viewCell row))


viewCell : Bool -> Html Msg
viewCell cell =
    if cell then
        div [ class "cell mine" ] []

    else
        div [ class "cell" ] []


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        NewGame width height mineCount ->
            ( newGame width height mineCount
            , Random.generate PlaceMine (mineGenerator width height)
            )

        PlaceMine point ->
            placeMine point model

        _ ->
            ( model, Cmd.none )


placeMine : Cell -> State -> ( State, Cmd Msg )
placeMine point model =
    if model.mineCount == 0 then
        ( model, Cmd.none )

    else if isMine point model then
        ( model, Random.generate PlaceMine (mineGenerator model.width model.height) )
    else
        ( { model
            | mineCount = model.mineCount - 1
            , mines = setMine point model
          }
        , Random.generate PlaceMine (mineGenerator model.width model.height)
        )

isMine : Cell -> State -> Bool
isMine cell state =
    get (cell.x + (cell.y * state.width)) state.mines
        |> withDefault False

setMine : Cell -> State -> Array Bool
setMine cell state =
    set (cell.x + (cell.y * state.width)) True state.mines


setMineInCol : Int -> Array Bool -> Array Bool
setMineInCol y row =
    set y True row


mineGenerator : Int -> Int -> Generator Cell
mineGenerator width height =
    Random.map tupleToCell (randomPair width height)


randomPair : Int -> Int -> Generator ( Int, Int )
randomPair width height =
    Random.pair (Random.int 0 (width - 1)) (Random.int 0 (height - 1))


tupleToCell : ( Int, Int ) -> Cell
tupleToCell t =
    Cell (Tuple.first t) (Tuple.second t)
