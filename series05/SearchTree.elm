module SearchTree exposing (..)


type SearchTree a
    = Empty
    | Node Int (SearchTree a) a (SearchTree a)


insert : SearchTree a -> Int -> a -> SearchTree a
insert t i x =
    case t of
        Empty ->
            Node i Empty x Empty

        Node j t1 y t2 ->
            if i < j then
                Node j (insert t1 i x) y t2
            else if i > j then
                Node j t1 y (insert t2 i x)
            else
                Node i t1 x t2


lookupSearchTree : SearchTree a -> Int -> Maybe a
lookupSearchTree t i =
    case t of
        Empty ->
            Nothing

        Node j t1 x t2 ->
            if i < j then
                lookupSearchTree t1 i
            else if i > j then
                lookupSearchTree t2 i
            else
                Just x


delete : SearchTree a -> Int -> SearchTree a
delete t i =
    case t of
        Empty ->
            Empty

        Node j t1 x t2 ->
            Empty


listToSearchTree : List ( Int, a ) -> SearchTree a
listToSearchTree l =
    case l of
        [] ->
            Empty

        ( i, x ) :: xs ->
            insert (listToSearchTree xs) i x


searchTreeToList : SearchTree a -> List ( Int, a )
searchTreeToList t =
    case t of
        Empty ->
            []

        Node i t1 x t2 ->
            searchTreeToList t1 ++ (( i, x ) :: searchTreeToList t2)


mapSearchTree : (a -> b) -> SearchTree a -> SearchTree b
mapSearchTree f t =
    case t of
        Empty ->
            Empty

        Node i t1 x t2 ->
            Node i (mapSearchTree f t1) (f x) (mapSearchTree f t2)


t1 : SearchTree Char
t1 =
    Node 5 Empty 'a' Empty


t2 =
    insert t1 4 'b'
