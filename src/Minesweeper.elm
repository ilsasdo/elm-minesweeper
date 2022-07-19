module Minesweeper exposing (..)

import Array exposing (Array, fromList, get, set, toList)
import Browser
import Html exposing (Attribute, Html, button, div, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (custom, onClick)
import Json.Decode as Decode
import Maybe exposing (withDefault)
import Random exposing (Generator)
import Random.Extra as Random
import Random.Set as Random
import Set


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


subscriptions : GameModel -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = PutFlag Cell
    | RemoveFlag Cell
    | RevealCell Cell
    | ResetGame
    | UpdateGameState
    | PlaceMines Cell (Set.Set ( Int, Int ))


type GameState
    = Defeat
    | Victory
    | NotStarted
    | OnGoing


type alias GameModel =
    { width : Int
    , height : Int
    , mines : Array Cell
    , mineCount : Int
    , totalMineCount : Int
    , flagCount : Int
    , gameState : GameState
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


init : () -> ( GameModel, Cmd Msg )
init _ =
    initModel 9 9 9


initModel : Int -> Int -> Int -> ( GameModel, Cmd Msg )
initModel width height mineCount =
    ( newGame width height mineCount, Cmd.none )


newGame : Int -> Int -> Int -> GameModel
newGame width height mineCount =
    GameModel width height (initEmptyMines width height) mineCount mineCount 0 NotStarted


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


view : GameModel -> Html Msg
view model =
    div []
        [ button [ onClick ResetGame ] [ text "New Game" ]
        , p [] [ text ("Flags: " ++ String.fromInt model.flagCount) ]
        , viewGameStatus model.gameState
        , viewMap model
        ]


viewGameStatus : GameState -> Html Msg
viewGameStatus status =
    case status of
        NotStarted ->
            p [] [ text "Click To Start" ]

        Defeat ->
            p [] [ text "You Lost" ]

        Victory ->
            p [] [ text "You WON!" ]

        OnGoing ->
            p [] [ text "Playing..." ]


viewMap : GameModel -> Html Msg
viewMap state =
    div [ class "map" ] (List.map (viewRow state) (List.range 0 (state.height - 1)))


viewRow : GameModel -> Int -> Html Msg
viewRow state y =
    state.mines
        |> Array.slice (y * state.width) ((y + 1) * state.width)
        |> Array.map viewCell
        |> toList
        |> div [ class "row" ]


viewCell : Cell -> Html Msg
viewCell cell =
    if cell.flag then
        div
            [ class "cell hidden"
            , onRightClick (RemoveFlag cell)
            ]
            [ text "🚩" ]

    else if cell.hidden then
        div
            [ class "cell hidden"
            , onClick (RevealCell cell)
            , onRightClick (PutFlag cell)
            ]
            [ text " " ]

    else if cell.mine then
        div [ class "cell mine" ] [ text "💣" ]

    else if cell.nearby == 0 then
        div [ class "cell revealed" ] [ text "" ]

    else
        div [ class ("cell revealed nearby-" ++ String.fromInt cell.nearby) ]
            [ text (String.fromInt cell.nearby) ]


update : Msg -> GameModel -> ( GameModel, Cmd Msg )
update msg model =
    case msg of
        ResetGame ->
            initModel 9 9 9

        PutFlag cell ->
            putFlag cell model

        RemoveFlag cell ->
            removeFlag cell model

        RevealCell cell ->
            if model.gameState == NotStarted then
                ( { model | gameState = OnGoing }
                  , Random.generate (PlaceMines cell) (randomPairGenerator cell model.width model.height model.totalMineCount))

            else
                revealFirstCell cell model

        PlaceMines cell mines ->
            ({model | mines = revealCell cell.x cell.y model.width (placeMines mines model)}, Cmd.none)

        UpdateGameState ->
            updateGameState model


updateGameState : GameModel -> ( GameModel, Cmd Msg )
updateGameState model =
    let
        gameState =
            determineGameState model
    in
    case gameState of
        Defeat ->
            gameOver model

        _ ->
            ( { model | gameState = gameState }, Cmd.none )


determineGameState : GameModel -> GameState
determineGameState model =
    if isMineRevealed model.mines then
        Defeat

    else if isVictory model then
        Victory

    else
        OnGoing


isVictory: GameModel -> Bool
isVictory model =
    let
        hiddenCellCount = Array.foldr (\cell -> \result -> if cell.hidden then result + 1 else result) 0 model.mines
    in
        hiddenCellCount == model.flagCount && model.flagCount == model.totalMineCount


isMineRevealed : Array Cell -> Bool
isMineRevealed mines =
    mines
        |> Array.filter (\cell -> cell.mine && not cell.hidden)
        |> Array.length
        |> greaterThan 0


greaterThan : Int -> Int -> Bool
greaterThan a b =
    a < b


putFlag : Cell -> GameModel -> ( GameModel, Cmd Msg )
putFlag cell state =
    update UpdateGameState
        { state
            | mines = set cell.index { cell | flag = True } state.mines
            , flagCount = state.flagCount + 1
        }


removeFlag : Cell -> GameModel -> ( GameModel, Cmd Msg )
removeFlag cell state =
    update UpdateGameState
        { state
            | mines = set cell.index { cell | flag = False } state.mines
            , flagCount = state.flagCount - 1
        }


gameOver : GameModel -> ( GameModel, Cmd Msg )
gameOver state =
    ( { state
        | mines = revealAllCells state.mines
        , gameState = Defeat
      }
    , Cmd.none
    )


revealAllCells mines =
    Array.map (\cell -> { cell | hidden = False }) mines


revealFirstCell : Cell -> GameModel -> ( GameModel, Cmd Msg )
revealFirstCell cell state =
    if cell.mine then
        update UpdateGameState { state | mines = set cell.index { cell | hidden = False } state.mines }

    else
        update UpdateGameState { state | mines = revealCell cell.x cell.y state.width state.mines }


revealCell : Int -> Int -> Int -> Array Cell -> Array Cell
revealCell x y width mines =
    let
        c =
            mines
                |> get (x + (y * width))
                |> withDefault { emptyCell | hidden = False }
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


placeMines : Set.Set ( Int, Int ) -> GameModel -> Array Cell
placeMines mines model =
    mineSetToArray mines model


mineSetToArray : Set.Set ( Int, Int ) -> GameModel -> Array Cell
mineSetToArray minesToPlace model =
    model.mines
        |> Array.map (setMines minesToPlace)
        |> Array.map (\cell -> { cell | nearby = countNearbyMines cell minesToPlace })


countNearbyMines : Cell -> Set.Set ( Int, Int ) -> Int
countNearbyMines cell minesToPlace =
    Set.fromList
        [ ( cell.x - 1, cell.y - 1 )
        , ( cell.x - 1, cell.y + 0 )
        , ( cell.x - 1, cell.y + 1 )
        , ( cell.x + 0, cell.y - 1 )
        , ( cell.x + 0, cell.y + 1 )
        , ( cell.x + 1, cell.y - 1 )
        , ( cell.x + 1, cell.y + 0 )
        , ( cell.x + 1, cell.y + 1 )
        ]
        |> Set.intersect minesToPlace
        |> Set.size


setMines : Set.Set ( Int, Int ) -> Cell -> Cell
setMines minesToPlace cell =
    { cell | mine = Set.member ( cell.x, cell.y ) minesToPlace }


randomPairGenerator : Cell -> Int -> Int -> Int -> Generator (Set.Set ( Int, Int ))
randomPairGenerator reserverdCell width height nMines =
    Random.pair (Random.int 0 (width - 1)) (Random.int 0 (height - 1))
        |> Random.filter (\pair -> reserverdCell.x /= Tuple.first pair && reserverdCell.y /= Tuple.second pair)
        |> Random.set nMines
