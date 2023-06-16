module Pages.Home_ exposing (Model, Msg, page)

import Browser.Events
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Html.Keyed
import Json.Decode
import List.Extra
import Page exposing (Page)
import Process
import Random
import Route exposing (Route)
import Set exposing (Set)
import Shared
import Task
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { tiles : List Tile
    , phase : Phase
    , score : Int
    }


type Phase
    = ReadyForInput
    | SlidingTiles
    | SpawningNewTile Id
    | PlayerWon
    | GameOver


type alias Coordinate =
    ( Int, Int )


type alias Tile =
    { id : Id
    , coordinate : Coordinate
    , value : Value
    }


type alias Id =
    Int


type alias Value =
    Int


init : () -> ( Model, Effect Msg )
init () =
    startNewGame


startNewGame : ( Model, Effect Msg )
startNewGame =
    ( { phase = ReadyForInput
      , tiles = []
      , score = 0
      }
    , spawnTileInGrid []
    )


spawnTileInGrid : List Tile -> Effect Msg
spawnTileInGrid existingTiles =
    let
        allCoordinates : Set Coordinate
        allCoordinates =
            List.range 0 3
                |> List.concatMap (\x -> List.range 0 3 |> List.map (Tuple.pair x))
                |> Set.fromList

        occupiedCoordinates : Set Coordinate
        occupiedCoordinates =
            existingTiles
                |> List.map .coordinate
                |> Set.fromList

        emptyCoordinates : Set Coordinate
        emptyCoordinates =
            Set.diff
                allCoordinates
                occupiedCoordinates
    in
    case Set.toList emptyCoordinates of
        [] ->
            Effect.sendMsg TileCouldNotBeSpawned

        coordinate :: otherCoordinates ->
            let
                newTileGenerator : Random.Generator Tile
                newTileGenerator =
                    Random.map3 Tile
                        (Random.int 0 Random.maxInt)
                        (Random.uniform coordinate otherCoordinates)
                        (Random.weighted
                            ( 90, 2 )
                            [ ( 10, 4 ) ]
                        )
            in
            newTileGenerator
                |> Random.generate TileSpawned
                |> Effect.sendCmd



-- UPDATE


type Msg
    = PressedArrowKey Direction
    | TileSlideAnimationFinished
    | TileSpawned Tile
    | TileSpawnAnimationReady
    | TileCouldNotBeSpawned
    | ClickedNewGame


type Direction
    = Up
    | Down
    | Left
    | Right


{-| How long the tile slide animation lasts (in milliseconds)
-}
duration : Int
duration =
    200


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        PressedArrowKey direction ->
            if model.phase == ReadyForInput then
                let
                    newTiles : List Tile
                    newTiles =
                        slide direction model.tiles

                    noTilesHaveMoved : Bool
                    noTilesHaveMoved =
                        List.sortBy .id newTiles
                            == List.sortBy .id model.tiles
                in
                if noTilesHaveMoved then
                    ( model, Effect.none )

                else
                    ( { model
                        | tiles = newTiles
                        , phase = SlidingTiles
                      }
                    , sendDelayedMessage duration TileSlideAnimationFinished
                    )

            else
                ( model
                , Effect.none
                )

        TileSlideAnimationFinished ->
            let
                ( points, newTiles ) =
                    combineValuesForStackedTiles model.tiles
            in
            ( { model | tiles = newTiles, score = model.score + points }
            , spawnTileInGrid newTiles
            )

        TileSpawned tile ->
            ( { model
                | tiles = tile :: model.tiles
                , phase = SpawningNewTile tile.id
              }
            , sendDelayedMessage 150 TileSpawnAnimationReady
            )

        TileSpawnAnimationReady ->
            if List.any is2048 model.tiles then
                ( { model | phase = PlayerWon }
                , Effect.none
                )

            else
                ( { model | phase = ReadyForInput }
                , Effect.none
                )

        TileCouldNotBeSpawned ->
            ( { model | phase = GameOver }
            , Effect.none
            )

        ClickedNewGame ->
            startNewGame


is2048 : Tile -> Bool
is2048 tile =
    tile.value == 2048


sendDelayedMessage : Int -> Msg -> Effect Msg
sendDelayedMessage duration_ msg =
    Process.sleep (Basics.toFloat duration_)
        |> Task.perform (\_ -> msg)
        |> Effect.sendCmd


combineValuesForStackedTiles : List Tile -> ( Int, List Tile )
combineValuesForStackedTiles tiles =
    let
        tilesAndScores : List ( Int, Tile )
        tilesAndScores =
            List.Extra.gatherEqualsBy .coordinate tiles
                |> List.map
                    (\( first, others ) ->
                        if List.isEmpty others then
                            ( 0, first )

                        else
                            ( first.value * 2, { first | value = first.value * 2 } )
                    )
    in
    ( List.sum (List.map Tuple.first tilesAndScores)
    , List.map Tuple.second tilesAndScores
    )


type Stack
    = Single Tile
    | Double Tile Tile


slide : Direction -> List Tile -> List Tile
slide direction tiles =
    let
        dict : Dict Coordinate Tile
        dict =
            tiles
                |> List.map (\tile -> ( tile.coordinate, tile ))
                |> Dict.fromList

        rows : List Tile
        rows =
            List.range 0 3
                |> List.map
                    (\y ->
                        List.range 0 3
                            |> List.filterMap (\x -> Dict.get ( x, y ) dict)
                            |> stackTiles
                            |> updateCoordinatesForRow y
                    )
                |> List.concat

        columns : List Tile
        columns =
            List.range 0 3
                |> List.map
                    (\x ->
                        List.range 0 3
                            |> List.filterMap (\y -> Dict.get ( x, y ) dict)
                            |> stackTiles
                            |> updateCoordinatesForColumn x
                    )
                |> List.concat

        stackTiles : List Tile -> List Stack
        stackTiles tiles_ =
            if direction == Up || direction == Left then
                List.foldl combineIfSameValue
                    []
                    tiles_

            else
                List.foldr combineIfSameValue
                    []
                    tiles_

        updateCoordinatesForColumn : Int -> List Stack -> List Tile
        updateCoordinatesForColumn x stacks =
            let
                offset =
                    if direction == Down then
                        4 - List.length stacks

                    else
                        0

                updateCoordinateForStack : Int -> Stack -> List Tile
                updateCoordinateForStack y stack =
                    case stack of
                        Single tile1 ->
                            [ { tile1 | coordinate = ( x, y + offset ) } ]

                        Double tile1 tile2 ->
                            [ { tile1 | coordinate = ( x, y + offset ) }
                            , { tile2 | coordinate = ( x, y + offset ) }
                            ]
            in
            stacks
                |> (if direction == Up then
                        List.reverse

                    else
                        identity
                   )
                |> List.indexedMap updateCoordinateForStack
                |> List.concat

        updateCoordinatesForRow : Int -> List Stack -> List Tile
        updateCoordinatesForRow y stacks =
            let
                offset =
                    if direction == Right then
                        4 - List.length stacks

                    else
                        0

                updateCoordinateForStack : Int -> Stack -> List Tile
                updateCoordinateForStack x stack =
                    case stack of
                        Single tile1 ->
                            [ { tile1 | coordinate = ( x + offset, y ) } ]

                        Double tile1 tile2 ->
                            [ { tile1 | coordinate = ( x + offset, y ) }
                            , { tile2 | coordinate = ( x + offset, y ) }
                            ]
            in
            stacks
                |> (if direction == Left then
                        List.reverse

                    else
                        identity
                   )
                |> List.indexedMap updateCoordinateForStack
                |> List.concat

        combineIfSameValue : Tile -> List Stack -> List Stack
        combineIfSameValue tile listSoFar =
            case listSoFar of
                (Single previous) :: rest ->
                    if previous.value == tile.value then
                        Double tile previous :: rest

                    else
                        Single tile :: Single previous :: rest

                _ ->
                    Single tile :: listSoFar

        moveTile : Tile -> Tile
        moveTile tile =
            let
                moveTileOneUnit : Coordinate -> Coordinate
                moveTileOneUnit ( x, y ) =
                    Tuple.mapBoth
                        (Basics.clamp 0 3)
                        (Basics.clamp 0 3)
                        (case direction of
                            Up ->
                                ( x
                                , y - 1
                                )

                            Left ->
                                ( x - 1
                                , y
                                )

                            Down ->
                                ( x
                                , y + 1
                                )

                            Right ->
                                ( x + 1
                                , y
                                )
                        )
            in
            { tile | coordinate = moveTileOneUnit tile.coordinate }
    in
    if direction == Up || direction == Down then
        columns

    else
        rows



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyDown keyPressDecoder


keyPressDecoder : Json.Decode.Decoder Msg
keyPressDecoder =
    let
        fromKeyCode : String -> Json.Decode.Decoder Direction
        fromKeyCode code =
            case code of
                "ArrowUp" ->
                    Json.Decode.succeed Up

                "ArrowLeft" ->
                    Json.Decode.succeed Left

                "ArrowRight" ->
                    Json.Decode.succeed Right

                "ArrowDown" ->
                    Json.Decode.succeed Down

                _ ->
                    Json.Decode.fail "Unknown key"
    in
    Json.Decode.field "code" Json.Decode.string
        |> Json.Decode.andThen fromKeyCode
        |> Json.Decode.map PressedArrowKey



-- VIEW


view : Model -> View Msg
view model =
    { title = "Homepage"
    , body =
        [ div [ style "text-align" "center" ]
            [ h1 [ style "font-size" "3rem" ] [ text "2048" ]
            , p [ style "opacity" "0.75" ] [ text ("Score: " ++ String.fromInt model.score) ]
            ]
        , div [ class "relative" ]
            [ div [ class "grid" ]
                (div [ class "grid__slot" ] []
                    |> List.repeat 16
                )
            , Html.Keyed.node "div"
                []
                (model.tiles
                    |> List.sortBy .id
                    |> List.map (viewKeyedTile model)
                )
            ]
        , case model.phase of
            PlayerWon ->
                viewDialog
                    { title = "You won!"
                    , buttonLabel = "New game"
                    }

            GameOver ->
                viewDialog
                    { title = "Game over"
                    , buttonLabel = "Try again?"
                    }

            _ ->
                text ""
        ]
    }


viewDialog :
    { title : String
    , buttonLabel : String
    }
    -> Html Msg
viewDialog props =
    div []
        [ div [ class "dialog__overlay" ] []
        , div [ class "dialog" ]
            [ h3 [] [ text props.title ]
            , button
                [ class "button"
                , Html.Events.onClick ClickedNewGame
                ]
                [ text props.buttonLabel ]
            ]
        ]


viewKeyedTile : Model -> Tile -> ( String, Html msg )
viewKeyedTile model tile =
    let
        valueStr : String
        valueStr =
            String.fromInt tile.value

        ( x, y ) =
            Tuple.mapBoth
                String.fromInt
                String.fromInt
                tile.coordinate

        fg : String
        fg =
            if tile.value <= 4 then
                "var(--color_fgDark)"

            else
                "var(--color_fgLight)"

        bg : String
        bg =
            "var(--color_bg${value})"
                |> String.replace "${value}" valueStr

        size : String
        size =
            if tile.value >= 1024 then
                "4em"

            else
                "5em"
    in
    ( String.fromInt tile.id
    , div
        [ class "tile"
        , class ("tile--" ++ valueStr)
        , style "transition"
            ("transform ${duration}ms ease-in-out"
                |> String.replace "${duration}" (String.fromInt duration)
            )
        , style "color" fg
        , style "background-color" bg
        , style "transform"
            ("translate(calc(${x} * 15.5em), calc(${y} * 15.5em)) scale(${scale})"
                |> String.replace "${x}" x
                |> String.replace "${y}" y
                |> String.replace "${scale}"
                    (case model.phase of
                        SpawningNewTile id ->
                            if tile.id == id then
                                "0"

                            else
                                "1"

                        _ ->
                            "1"
                    )
            )
        ]
        [ span
            [ class "tile__text"
            , style "font-size" size
            ]
            [ text valueStr ]
        ]
    )
