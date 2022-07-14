module MineField exposing (..)

import Array exposing (Array, fromList, get, set, toList)
import Browser
import Html exposing (Attribute, Html, button, div, p, text)
import Html.Attributes exposing (class, contextmenu, value)
import Html.Events exposing (custom, on, onClick)
import Json.Decode as Decode
import Maybe exposing (withDefault)
import Random exposing (Generator)


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


subscriptions : State -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = PutFlag Cell
    | RemoveFlag Cell
    | RevealCell Cell
    | SpreadCell Cell
    | NewGame Int Int Int
    | CheckVictory
    | GameOver Cell
    | PlaceMine ( Int, Int )


type GameStatus
    = Defeat
    | Victory
    | NotStarted
    | OnGoing

type alias State =
    { width : Int
    , height : Int
    , mines : Array Cell
    , mineCount : Int
    , flagCount : Int
    , gameStatus: GameStatus
    }


type alias Cell =
    { x : Int
    , y : Int
    , index : Int
    , nearby : Int
    , mine : Bool
    , hidden : Bool
    , flag : Bool
    }

onRightClick : Msg -> Attribute Msg
onRightClick message =
    custom "contextmenu" (Decode.succeed { message = message, stopPropagation = True, preventDefault = True })


init : () -> ( State, Cmd Msg )
init _ =
    initModel 9 9 9


initModel : Int -> Int -> Int -> ( State, Cmd Msg )
initModel width height mineCount =
    ( newGame width height mineCount, Cmd.none )


newGame : Int -> Int -> Int -> State
newGame width height mineCount =
    State width height (initEmptyMines width height) mineCount 0 OnGoing


initEmptyMines : Int -> Int -> Array Cell
initEmptyMines width height =
    List.range 0 ((width * height) - 1)
        |> List.map (initCell width height)
        |> fromList


emptyCell =
    initCell 1 1 0


initCell : Int -> Int -> Int -> Cell
initCell width height index =
    Cell (modBy width index) (index // height) index 0 False True False


view : State -> Html Msg
view model =
    div []
        [ button [ onClick (NewGame 9 9 10) ] [ text "New Game" ]
        , p [] [text ("Flags: "++(String.fromInt model.flagCount))]
        , viewGameStatus model.gameStatus
        , viewMap model
        ]


viewGameStatus: GameStatus -> Html Msg
viewGameStatus status =
    case status of
        NotStarted ->
            p [] [text "Click To Start"]
        Defeat ->
            p [] [text "You Lost"]
        Victory ->
            p [] [text "You WIN!"]
        OnGoing ->
            p [] [text ""]


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
    if cell.flag then
        div [ class "cell hidden", onRightClick (RemoveFlag cell)] [ text "🚩" ]

    else if cell.hidden then
        div [ class "cell hidden", onClick (RevealCell cell), onRightClick (PutFlag cell)] [ text " " ]

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

        PutFlag cell ->
            putFlag cell model

        RemoveFlag cell ->
            removeFlag cell model

        RevealCell cell ->
            revealFirstCell cell model

        PlaceMine xyTuple ->
            placeMine (tupleToCell xyTuple model) model

        CheckVictory ->
            checkVictory model

        GameOver cell ->
            gameOver cell model

        _ ->
            ( model, Cmd.none )

checkVictory: State -> (State, Cmd Msg)
checkVictory state =
    ({state | gameStatus = isVictory state}, Cmd.none)


isVictory: State -> GameStatus
isVictory state =
    if (Debug.log "state.flagCount" (state.flagCount)) == (Debug.log "state.mineCount" (state.mineCount)) then
        Victory
    else
        OnGoing


putFlag: Cell -> State -> (State, Cmd Msg)
putFlag cell state =
    update CheckVictory { state | mines = set cell.index {cell | flag = True } state.mines
             , flagCount = state.flagCount + 1 }

removeFlag: Cell -> State -> (State, Cmd Msg)
removeFlag cell state =
    update CheckVictory { state | mines = set cell.index {cell | flag = False } state.mines
             , flagCount = state.flagCount - 1 }


gameOver: Cell -> State -> (State, Cmd Msg)
gameOver cell state =
    ({ state | mines = revealAllCells state.mines
     , gameStatus = Defeat
     }, Cmd.none)


revealAllCells mines =
    Array.map (\cell -> {cell | hidden = False }) mines


revealFirstCell : Cell -> State -> ( State, Cmd Msg )
revealFirstCell cell state =
    if cell.mine then
        update (GameOver cell) state

    else
        update CheckVictory { state | mines = revealCell cell.x cell.y state.width state.mines}


revealCell : Int -> Int -> Int -> Array Cell -> Array Cell
revealCell x y width mines =
    let
        c = mines
            |> get (x + (y * width))
            |> withDefault {emptyCell | hidden = False}
    in
    if x < 0 || y < 0 || x >= width then
        mines

    else if not c.hidden || c.flag then
        mines

    else if c.nearby > 0 then
        mines
        |> set c.index { c | hidden = False }

    else
        mines
        |> set c.index { c | hidden = False }
        |> revealCell (c.x - 1) (c.y - 1) width
        |> revealCell (c.x - 1) (c.y + 0) width
        |> revealCell (c.x - 1) (c.y + 1) width
        |> revealCell (c.x + 0) (c.y - 1) width
        |> revealCell (c.x + 0) (c.y + 1) width
        |> revealCell (c.x + 1) (c.y - 1) width
        |> revealCell (c.x + 1) (c.y + 0) width
        |> revealCell (c.x + 1) (c.y + 1) width


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


incrementNeighbourMine : Int -> Int -> Int -> Array Cell -> Array Cell
incrementNeighbourMine x y width mines =
    let
        index = (x + (y * width))
        cell =
            mines
                |> get index
                |> withDefault emptyCell
    in
    if x < 0 || y < 0 || x >= width then
        mines
    else
        set index { cell | nearby = cell.nearby + 1 } mines


arrayGetValue x y width defaultValue array =
    get (x + (y * width)) array
        |> withDefault defaultValue


setMine : Cell -> State -> Array Cell
setMine cell state =
    state.mines
        |> set cell.index { cell | mine = True }
        |> incrementNeighbourMine (cell.x - 1) (cell.y - 1) state.width
        |> incrementNeighbourMine (cell.x - 1) (cell.y + 0) state.width
        |> incrementNeighbourMine (cell.x - 1) (cell.y + 1) state.width
        |> incrementNeighbourMine (cell.x + 0) (cell.y - 1) state.width
        |> incrementNeighbourMine (cell.x + 0) (cell.y + 1) state.width
        |> incrementNeighbourMine (cell.x + 1) (cell.y - 1) state.width
        |> incrementNeighbourMine (cell.x + 1) (cell.y + 0) state.width
        |> incrementNeighbourMine (cell.x + 1) (cell.y + 1) state.width


randomPairGenerator : Int -> Int -> Generator ( Int, Int )
randomPairGenerator width height =
    Random.pair (Random.int 0 (width - 1)) (Random.int 0 (height - 1))
