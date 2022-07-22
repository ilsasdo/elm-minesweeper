module Minesweeper exposing (..)

import Array exposing (Array, toList)
import Browser
import Html exposing (Attribute, Html, button, div, option, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (custom, on, onClick, onDoubleClick, onInput)
import Json.Decode as Decode
import Matrix exposing (Matrix)
import Maybe exposing (withDefault)
import Random exposing (Generator)
import Random.Extra as Random
import Random.Set as Random
import Set
import Time


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


subscriptions : GameModel -> Sub Msg
subscriptions model =
    Time.every 1000 Tick


type Msg
    = ResetGame GameDifficulty
    | RevealCell Cell
    | ExpandCell Cell
    | PutFlag Cell
    | RemoveFlag Cell
    | PlaceMines Cell (Set.Set ( Int, Int ))
    | UpdateGameState
    | Tick Time.Posix


type GameState
    = Defeat
    | Victory
    | NotStarted
    | OnGoing


type GameDifficulty
    = Easy
    | Medium
    | Advanced


type alias GameModel =
    { minefield : Matrix Cell
    , mineCount : Int
    , flagCount : Int
    , gameState : GameState
    , elapsedTime : Int
    , gameType : GameDifficulty
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
    initGame Easy


initGame : GameDifficulty -> ( GameModel, Cmd Msg )
initGame difficulty =
    case difficulty of
        Easy ->
            ( newGame 9 9 10 difficulty, Cmd.none )

        Medium ->
            ( newGame 16 16 40 difficulty, Cmd.none )

        Advanced ->
            ( newGame 30 16 99 difficulty, Cmd.none )


newGame : Int -> Int -> Int -> GameDifficulty -> GameModel
newGame width height mineCount difficulty =
    GameModel (initEmptyMines width height) mineCount 0 NotStarted 0 difficulty


initEmptyMines : Int -> Int -> Matrix Cell
initEmptyMines width height =
    Matrix.initialize height width initCell


emptyCell =
    initCell ( 0, 0 )


initCell : ( Int, Int ) -> Cell
initCell ( y, x ) =
    Cell x y 0 False True False


update : Msg -> GameModel -> ( GameModel, Cmd Msg )
update msg model =
    -- if game is over, disable any event except ResetGame
    if
        (model.gameState == Victory || model.gameState == Defeat)
            && (msg /= ResetGame Easy && msg /= ResetGame Medium && msg /= ResetGame Advanced)
    then
        ( model, Cmd.none )

    else
        case msg of
            ResetGame size ->
                initGame size

            Tick newTime ->
                if model.gameState == OnGoing then
                    ( { model | elapsedTime = model.elapsedTime + 1 }, Cmd.none )

                else
                    ( model, Cmd.none )

            PutFlag cell ->
                if model.gameState == NotStarted then
                    ( putFlag cell model, generateMines cell model )

                else
                    putFlag cell model
                        |> update UpdateGameState

            RemoveFlag cell ->
                ( removeFlag cell model, triggerUpdateGameState )

            RevealCell cell ->
                if model.gameState == NotStarted then
                    ( { model | gameState = OnGoing }, generateMines cell model )

                else
                    { model | minefield = revealFirstCell cell model.minefield }
                        |> update UpdateGameState

            PlaceMines cell mines ->
                ( { model | minefield = revealCell cell.x cell.y (placeMines mines model) }, Cmd.none )

            UpdateGameState ->
                updateGameState model

            ExpandCell cell ->
                { model | minefield = expandCell cell model.minefield }
                    |> update UpdateGameState



-- INIT MINEFIELD BY PLACING RANDOM MINES --


placeMines : Set.Set ( Int, Int ) -> GameModel -> Matrix Cell
placeMines minesToPlace model =
    model.minefield
        -- place mines
        |> Matrix.map (\cell -> { cell | mine = Set.member ( cell.x, cell.y ) minesToPlace })
        -- init nearby mines
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


generateMines : Cell -> GameModel -> Cmd Msg
generateMines reservedCell model =
    Random.generate (PlaceMines reservedCell) (randomPairGenerator reservedCell (Matrix.width model.minefield) (Matrix.height model.minefield) model.mineCount)


randomPairGenerator : Cell -> Int -> Int -> Int -> Generator (Set.Set ( Int, Int ))
randomPairGenerator reserverdCell width height nMines =
    Random.pair (Random.int 1 width) (Random.int 1 height)
        |> Random.filter (\pair -> reserverdCell.x /= Tuple.first pair && reserverdCell.y /= Tuple.second pair)
        |> Random.set nMines



-- CELL REVEALING --


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
            Matrix.get y x minefield
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



-- PUT / REMOVE FLAG --


putFlag : Cell -> GameModel -> GameModel
putFlag cell model =
    { model
        | gameState = OnGoing
        , flagCount = model.flagCount + 1
        , minefield = Matrix.map (putFlagOnCell cell) model.minefield
    }


putFlagOnCell : Cell -> Cell -> Cell
putFlagOnCell selectedCell cell =
    if selectedCell.x == cell.x && selectedCell.y == cell.y then
        { cell | flag = True }

    else
        cell


removeFlag : Cell -> GameModel -> GameModel
removeFlag cell state =
    { state
        | minefield = matrixSet cell.x cell.y { cell | flag = False } state.minefield
        , flagCount = state.flagCount - 1
    }



-- GAME STATE UPDATE --


triggerUpdateGameState =
    Cmd.map (always UpdateGameState) Cmd.none


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
    if isMineRevealed model.minefield then
        Defeat

    else if isVictory model then
        Victory

    else
        OnGoing


isVictory : GameModel -> Bool
isVictory model =
    let
        hiddenCellCount =
            countHiddenCell model.minefield
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


gameOver : GameModel -> ( GameModel, Cmd Msg )
gameOver state =
    ( { state
        | minefield = revealAllCells state.minefield
        , gameState = Defeat
      }
    , Cmd.none
    )


revealAllCells : Matrix Cell -> Matrix Cell
revealAllCells mines =
    Matrix.map (\cell -> { cell | hidden = False }) mines



-- VIEWS --


parseDifficulty : String -> Msg
parseDifficulty message =
    case message of
        "Easy" ->
            ResetGame Easy

        "Medium" ->
            ResetGame Medium

        "Advanced" ->
            ResetGame Advanced

        _ ->
            ResetGame Easy


view : GameModel -> Html Msg
view model =
    div []
        [ div [ class "choose" ]
            [ Html.select [ onInput parseDifficulty ]
                [ Html.option [] [ text "Easy" ]
                , Html.option [] [ text "Medium" ]
                , Html.option [] [ text "Advanced" ]
                ]
            ]
        , div [ class "container" ]
            [ div [ class "header" ]
                [ div []
                    [ div [ class "header-flags" ] [ text (String.fromInt (model.mineCount - model.flagCount)) ] ]
                , div [ class "header-middle" ]
                    [ div [ class "header-newgame" ] [ viewGameStatus model ] ]
                , div [] [ div [ class "header-time" ] [ text (String.fromInt model.elapsedTime) ] ]
                ]
            , viewMap model
            ]
        ]


viewGameStatus : GameModel -> Html Msg
viewGameStatus model =
    button [ onClick (ResetGame model.gameType) ]
        [ case model.gameState of
            NotStarted ->
                text "🙂"

            Defeat ->
                text "🤯"

            Victory ->
                text "😎"

            OnGoing ->
                text "🙂"
        ]


viewMap : GameModel -> Html Msg
viewMap model =
    div [ class "map" ] (List.map (viewRow model) (List.range 1 (Matrix.height model.minefield)))


viewRow : GameModel -> Int -> Html Msg
viewRow model y =
    model.minefield
        |> Matrix.toList
        |> Array.fromList
        |> Array.slice ((y - 1) * Matrix.width model.minefield) (y * Matrix.width model.minefield)
        |> Array.map (viewCell model.gameState)
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



-- UTILS


matrixSet : Int -> Int -> Cell -> Matrix Cell -> Matrix Cell
matrixSet x y value matrix =
    Matrix.map
        (\cell ->
            if cell.x == x && cell.y == y then
                value

            else
                cell
        )
        matrix
