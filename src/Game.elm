module Game exposing (Game, decoder, encode)

import Json.Decode
import Json.Encode


type alias Game =
    { tiles : List Tile
    , score : Int
    }


decoder : Json.Decode.Decoder Game
decoder =
    Json.Decode.map2 Game
        (Json.Decode.field "tiles" (Json.Decode.list tileDecoder))
        (Json.Decode.field "score" Json.Decode.int)


encode : Game -> Json.Encode.Value
encode game =
    Json.Encode.object
        [ ( "tiles", Json.Encode.list encodeTile game.tiles )
        , ( "score", Json.Encode.int game.score )
        ]


type alias Tile =
    { id : Int
    , value : Int
    , coordinate : ( Int, Int )
    }


tileDecoder : Json.Decode.Decoder Tile
tileDecoder =
    Json.Decode.map3 Tile
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "value" Json.Decode.int)
        (Json.Decode.field "coordinate"
            (Json.Decode.map2 Tuple.pair
                (Json.Decode.index 0 Json.Decode.int)
                (Json.Decode.index 1 Json.Decode.int)
            )
        )


encodeTile : Tile -> Json.Encode.Value
encodeTile tile =
    Json.Encode.object
        [ ( "id", Json.Encode.int tile.id )
        , ( "value", Json.Encode.int tile.value )
        , ( "coordinate"
          , Json.Encode.list Json.Encode.int
                [ Tuple.first tile.coordinate
                , Tuple.second tile.coordinate
                ]
          )
        ]
