module Elm.Parser.Numbers exposing (number)

import Combine exposing (Parser)
import Combine.Char
import Combine.Extra as Combine
import Combine.Num as Num
import Elm.Parser.State exposing (State)
import Hex


number : (Float -> a) -> (Int -> a) -> (Int -> a) -> Parser State a
number floatf intf hexf =
    Combine.choice
        [ Combine.map hexf hex
        , Combine.map floatf Num.float
        , Combine.map intf Num.int
        ]


hex : Parser State Int
hex =
    Combine.string "0x"
        |> Combine.continueWith (Combine.many1 Combine.Char.hexDigit)
        |> Combine.map hexValueFromChars


hexValueFromChars : List Char -> Int
hexValueFromChars =
    String.fromList >> Hex.fromString >> Result.withDefault 0
