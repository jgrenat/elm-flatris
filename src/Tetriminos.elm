module Tetriminos exposing (Tetrimino(..), decode, encode, random, toColorString)

import Color exposing (Color)
import Grid exposing (Grid)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Random


type Tetrimino
    = Shape Color
    | Bomb


random : Random.Seed -> ( Grid Tetrimino, Random.Seed )
random seed =
    let
        number =
            Random.int 0 (List.length tetriminos - 1)

        tetrimino n =
            Maybe.withDefault Grid.empty (List.head (List.drop n tetriminos))
    in
    Random.step (Random.map tetrimino number) seed


decode : Decoder Tetrimino
decode =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "shape" ->
                        Decode.field "color" Color.decode
                            |> Decode.map Shape

                    "bomb" ->
                        Decode.succeed Bomb

                    _ ->
                        Decode.fail ("Unknown type " ++ type_)
            )


encode : Tetrimino -> Encode.Value
encode tetrimino =
    case tetrimino of
        Shape color ->
            Encode.object [ ( "type", Encode.string "shape" ), ( "color", Color.encode color ) ]

        Bomb ->
            Encode.object [ ( "type", Encode.string "bomb" ) ]


tetriminos : List (Grid Tetrimino)
tetriminos =
    List.map
        (\( a, b ) -> Grid.fromList a b)
        [ ( Color.rgb 60 199 214 |> Shape, [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 3, 0 ) ] )
        , ( Color.rgb 251 180 20 |> Shape, [ ( 0, 0 ), ( 1, 0 ), ( 0, 1 ), ( 1, 1 ) ] )
        , ( Color.rgb 176 68 151 |> Shape, [ ( 1, 0 ), ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) ] )
        , ( Color.rgb 57 147 208 |> Shape, [ ( 0, 0 ), ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) ] )
        , ( Color.rgb 237 101 47 |> Shape, [ ( 2, 0 ), ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) ] )
        , ( Color.rgb 149 196 61 |> Shape, [ ( 1, 0 ), ( 2, 0 ), ( 0, 1 ), ( 1, 1 ) ] )
        , ( Color.rgb 232 65 56 |> Shape, [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 2, 1 ) ] )
        , ( Bomb, [ ( 0, 0 ) ] )
        ]


toColorString : Tetrimino -> String
toColorString tetrimino =
    case tetrimino of
        Shape color ->
            Color.toString color

        Bomb ->
            Color.toString (Color.rgb 0 0 0)
