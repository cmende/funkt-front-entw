module Series05 exposing (..)

import Maybe exposing (..)


mapMaybe : (a -> b) -> Maybe a -> Maybe b
mapMaybe f m =
    case m of
        Nothing ->
            Nothing

        Just x ->
            Just (f x)


type Tree a
    = Empty
    | Node (Tree a) a (Tree a)


mapTree : (a -> b) -> Tree a -> Tree b
mapTree f t =
    case t of
        Empty ->
            Empty

        Node t1 x t2 ->
            Node (mapTree f t1) (f x) (mapTree f t2)


mapResult : (a -> b) -> Result c a -> Result c b
mapResult f r =
    case r of
        Err e ->
            Err e

        Ok x ->
            Ok (f x)


mapFirst : (a -> c) -> ( a, b ) -> ( c, b )
mapFirst f ( x, y ) =
    ( f x, y )


mapSecond : (b -> c) -> ( a, b ) -> ( a, c )
mapSecond f ( x, y ) =
    ( x, f y )
