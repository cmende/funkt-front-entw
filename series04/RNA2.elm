module RNA2 exposing (..)

import RNA exposing (..)


parseBase : Char -> Result String Base
parseBase c =
    if c == 'U' || c == 'u' then
        Ok U
    else if c == 'A' || c == 'a' then
        Ok A
    else if c == 'C' || c == 'c' then
        Ok C
    else if c == 'G' || c == 'g' then
        Ok G
    else
        Err "keine Base"



-- uncons : String -> Maybe ( Char, String )


parse : String -> Result String RNA
parse s =
    case String.uncons s of
        Nothing ->
            Ok Nil

        Just ( c, cs ) ->
            case parseBase c of
                Err e ->
                    Err e

                Ok b ->
                    case parse cs of
                        Err e ->
                            Err e

                        Ok r ->
                            Ok (Cons b r)
