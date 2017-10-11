module RNA exposing (..)


type Base
    = U
    | A
    | C
    | G


type RNA
    = Cons Base RNA
    | Nil


empty : RNA
empty =
    Nil


extend : RNA -> Base -> RNA
extend r b =
    case r of
        Nil ->
            Cons b Nil

        Cons x xs ->
            Cons x (extend xs b)


complement : RNA -> RNA
complement r =
    let
        complementBase b =
            case b of
                U ->
                    A

                A ->
                    U

                C ->
                    G

                G ->
                    C
    in
        case r of
            Nil ->
                Nil

            Cons x xs ->
                Cons (complementBase x) (complement xs)


inverse : RNA -> RNA
inverse r =
    case r of
        Nil ->
            Nil

        Cons x xs ->
            extend (inverse xs) x
