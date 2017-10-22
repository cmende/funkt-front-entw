module Series04 exposing (..)


reverse : List a -> List a
reverse l =
    case l of
        [] ->
            []

        x :: xs ->
            (reverse xs) ++ [ x ]


reverseEnd : List a -> List a
reverseEnd l =
    let
        h x y =
            case x of
                [] ->
                    y

                z :: zs ->
                    h zs (z :: y)
    in
        h l []


last : List a -> Maybe a
last l =
    case l of
        [] ->
            Nothing

        x :: [] ->
            Just x

        x :: xs ->
            last xs


indexOf : List String -> String -> Maybe Int
indexOf l s =
    let
        h c l s =
            case l of
                [] ->
                    Nothing

                x :: xs ->
                    if x == s then
                        Just c
                    else
                        h (c + 1) xs s
    in
        h 0 l s
