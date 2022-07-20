module Minesweeper exposing (..)

import Array exposing (Array, toList)
import Browser
import Html exposing (Attribute, Html, button, div, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (custom, onClick, onDoubleClick)
import Json.Decode as Decode
import Matrix exposing (Matrix)
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
    | ExpandCell Cell


type GameState
    = Defeat
    | Victory
    | NotStarted
    | OnGoing


type alias GameModel =
    { width : Int
    , height : Int
    , mines : Matrix Cell
    , mineCount : Int
    , flagCount : Int
    , gameState : GameState
    }


type alias Cell =
    { x : Int
    , y : Int
    , nearby : Int
    , mine : Bool
    , hidden : Bool
    , flag : Bool
    }


init : () -> ( GameModel, Cmd Msg )
init _ =
    initModel 9 9 9


initModel : Int -> Int -> Int -> ( GameModel, Cmd Msg )
initModel width height mineCount =
    ( newGame width height mineCount, Cmd.none )


newGame : Int -> Int -> Int -> GameModel
newGame width height mineCount =
    GameModel width height (initEmptyMines width height) mineCount 0 NotStarted


initEmptyMines : Int -> Int -> Matrix Cell
initEmptyMines width height =
    Matrix.initialize width height initCell


emptyCell =
    initCell ( 0, 0 )


initCell : ( Int, Int ) -> Cell
initCell ( x, y ) =
    Cell x y 0 False True False


update : Msg -> GameModel -> ( GameModel, Cmd Msg )
update msg model =
    case Debug.log "msg: " msg of
        ResetGame ->
            initModel 9 9 10

        PutFlag cell ->
            let
                cmd =
                    if model.gameState == NotStarted then
                        Random.generate (PlaceMines cell) (randomPairGenerator cell model.width model.height model.mineCount)

                    else
                        Cmd.none
            in
            { model
                | gameState = OnGoing
                , flagCount = model.flagCount + 1
                , mines = Matrix.map (putFlag cell) model.mines
            }
                |> update UpdateGameState

        RemoveFlag cell ->
            ( removeFlag cell model, triggerUpdateGameState )

        RevealCell cell ->
            if model.gameState == NotStarted then
                ( { model | gameState = OnGoing }
                , Random.generate (PlaceMines cell) (randomPairGenerator cell model.width model.height model.mineCount)
                )

            else
                { model | mines = revealFirstCell cell model.mines }
                    |> update UpdateGameState

        PlaceMines cell mines ->
            ( { model | mines = revealCell cell.x cell.y (placeMines mines model) }, Cmd.none )

        UpdateGameState ->
            updateGameState model

        ExpandCell cell ->
            { model | mines = expandCell cell model.mines }
                |> update UpdateGameState


triggerUpdateGameState =
    Cmd.map (always UpdateGameState) Cmd.none


expandCell : Cell -> Matrix Cell -> Matrix Cell
expandCell cell minefield =
    let
        nearbyMines =
            countNearbyFlags cell minefield
    in
    if cell.nearby == nearbyMines then
        minefield
            |> revealCell (cell.x - 1) (cell.y - 1)
            |> revealCell (cell.x - 1) (cell.y + 0)
            |> revealCell (cell.x - 1) (cell.y + 1)
            |> revealCell (cell.x + 0) (cell.y - 1)
            |> revealCell (cell.x + 0) (cell.y + 1)
            |> revealCell (cell.x + 1) (cell.y - 1)
            |> revealCell (cell.x + 1) (cell.y + 0)
            |> revealCell (cell.x + 1) (cell.y + 1)

    else
        minefield


countNearbyFlags : Cell -> Matrix Cell -> Int
countNearbyFlags cell minefield =
    minefield
        |> Matrix.toList
        |> List.filter (\c -> c.flag)
        |> List.map (\c -> ( c.x, c.y ))
        |> Set.fromList
        |> Set.intersect
            (Set.fromList
                [ ( cell.x - 1, cell.y - 1 )
                , ( cell.x - 1, cell.y + 0 )
                , ( cell.x - 1, cell.y + 1 )
                , ( cell.x + 0, cell.y - 1 )
                , ( cell.x + 0, cell.y + 1 )
                , ( cell.x + 1, cell.y - 1 )
                , ( cell.x + 1, cell.y + 0 )
                , ( cell.x + 1, cell.y + 1 )
                ]
            )
        |> Set.size


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


isVictory : GameModel -> Bool
isVictory model =
    let
        hiddenCellCount =
            countHiddenCell model.mines
    in
    hiddenCellCount == model.flagCount && model.flagCount == model.mineCount


countHiddenCell : Matrix Cell -> Int
countHiddenCell cells =
    cells
        |> Matrix.toList
        |> List.foldr
            (\cell ->
                \result ->
                    if cell.hidden then
                        result + 1

                    else
                        result
            )
            0


isMineRevealed : Matrix Cell -> Bool
isMineRevealed mines =
    mines
        |> Matrix.toList
        |> List.filter (\cell -> cell.mine && not cell.hidden)
        |> List.length
        |> greaterThan 0


greaterThan : Int -> Int -> Bool
greaterThan a b =
    a < b


putFlag : Cell -> Cell -> Cell
putFlag selectedCell cell =
    if selectedCell.x == cell.x && selectedCell.y == cell.y then
        { cell | flag = True }

    else
        cell


removeFlag : Cell -> GameModel -> GameModel
removeFlag cell state =
    { state
        | mines = matrixSet cell.x cell.y { cell | flag = False } state.mines
        , flagCount = state.flagCount - 1
    }


matrixSet x y value matrix =
    Matrix.map
        (\cell ->
            if cell.x == x && cell.y == y then
                value

            else
                cell
        )
        matrix


gameOver : GameModel -> ( GameModel, Cmd Msg )
gameOver state =
    ( { state
        | mines = revealAllCells state.mines
        , gameState = Defeat
      }
    , Cmd.none
    )


revealAllCells : Matrix Cell -> Matrix Cell
revealAllCells mines =
    Matrix.map (\cell -> { cell | hidden = False }) mines


revealFirstCell : Cell -> Matrix Cell -> Matrix Cell
revealFirstCell cell minefield =
    if cell.mine then
        matrixSet cell.x cell.y { cell | hidden = False } minefield

    else
        revealCell cell.x cell.y minefield


revealCell : Int -> Int -> Matrix Cell -> Matrix Cell
revealCell x y minefield =
    let
        c =
            Matrix.get x y minefield
                |> withDefault { emptyCell | hidden = False }
    in
    if x < 1 || y < 1 || x > Matrix.width minefield || y > Matrix.height minefield then
        minefield

    else if not c.hidden || c.flag then
        minefield

    else if c.nearby > 0 || c.mine then
        matrixSet c.x c.y { c | hidden = False } minefield

    else
        minefield
            |> matrixSet c.x c.y { c | hidden = False }
            |> revealCell (c.x - 1) (c.y - 1)
            |> revealCell (c.x - 1) (c.y + 0)
            |> revealCell (c.x - 1) (c.y + 1)
            |> revealCell (c.x + 0) (c.y - 1)
            |> revealCell (c.x + 0) (c.y + 1)
            |> revealCell (c.x + 1) (c.y - 1)
            |> revealCell (c.x + 1) (c.y + 0)
            |> revealCell (c.x + 1) (c.y + 1)


placeMines : Set.Set ( Int, Int ) -> GameModel -> Matrix Cell
placeMines mines model =
    mineSetToArray mines model


mineSetToArray : Set.Set ( Int, Int ) -> GameModel -> Matrix Cell
mineSetToArray minesToPlace model =
    model.mines
        |> Matrix.map (setMines minesToPlace)
        |> Matrix.map (\cell -> { cell | nearby = countNearbyMines cell minesToPlace })


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
    Random.pair (Random.int 1 width) (Random.int 1 height)
        |> Random.filter (\pair -> reserverdCell.x /= Tuple.first pair && reserverdCell.y /= Tuple.second pair)
        |> Random.set nMines


-- VIEWS --


view : GameModel -> Html Msg
view model =
    div []
        [ button [ onClick ResetGame ] [ text "New Game" ]
        , p [] [ text ("Flags: " ++ String.fromInt (model.mineCount - model.flagCount)) ]
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
viewMap model =
    div [ class "map" ] (List.map (viewRow model) (List.range 1 model.height))


viewRow : GameModel -> Int -> Html Msg
viewRow state y =
    state.mines
        |> Matrix.toList
        |> Array.fromList
        |> Array.slice ((y - 1) * state.width) (y * state.width)
        |> Array.map (viewCell state.gameState)
        |> toList
        |> div [ class "row" ]


viewCell : GameState -> Cell -> Html Msg
viewCell gameState cell =
    if cell.mine && not cell.hidden then
        div [ class "cell mine" ] [ text "💣" ]

    else if gameState == Defeat && cell.flag && not cell.mine then
        div [ class "cell mine" ]
            [ div [ class "wrong-flag" ]
                [ text "❌" ]
            , text "🚩"
            ]

    else if cell.flag then
        div
            [ class "cell"
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

    else if cell.nearby == 0 then
        div [ class "cell revealed" ] [ text "" ]

    else
        div
            [ class ("cell revealed nearby-" ++ String.fromInt cell.nearby)
            , onDoubleClick (ExpandCell cell)
            ]
            [ text (String.fromInt cell.nearby) ]


onRightClick : Msg -> Attribute Msg
onRightClick message =
    custom "contextmenu" (Decode.succeed { message = message, stopPropagation = True, preventDefault = True })