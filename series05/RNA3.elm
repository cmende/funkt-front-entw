module RNA3 exposing (..)

import List exposing (..)
import Char
import RNA exposing (Base(..), complementBase)
import RNA2 exposing (parseBase)


type alias RNA =
    List Base


empty : RNA
empty =
    []


extend : RNA -> Base -> RNA
extend r b =
    r ++ [ b ]


complement : RNA -> RNA
complement r =
    map complementBase r


inverse : RNA -> RNA
inverse =
    reverse


printBase : Base -> Char
printBase b =
    case b of
        U ->
            'U'

        A ->
            'A'

        C ->
            'C'

        G ->
            'G'


printRNA : Bool -> RNA -> String
printRNA b r =
    String.fromList
        (map
            ((if b then
                Char.toLower
              else
                identity
             )
                << printBase
            )
            r
        )


parse : String -> Result String RNA
parse s =
    case String.uncons s of
        Nothing ->
            Ok []

        Just ( c, cs ) ->
            Result.map2 (::) (parseBase c) (parse cs)
