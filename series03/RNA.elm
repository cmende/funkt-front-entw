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


complementBase : Base -> Base
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


complement : RNA -> RNA
complement r =
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


printBase : Bool -> Base -> String
printBase c b =
    case b of
        U ->
            if c then
                "u"
            else
                "U"

        A ->
            if c then
                "a"
            else
                "A"

        C ->
            if c then
                "c"
            else
                "C"

        G ->
            if c then
                "g"
            else
                "G"


printRNA : Bool -> RNA -> String
printRNA c r =
    case r of
        Nil ->
            ""

        Cons b bs ->
            printBase c b ++ printRNA c bs
