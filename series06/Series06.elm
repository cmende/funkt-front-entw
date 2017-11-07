module Series06 exposing (..)

-- Aufgabe 1


reverse : List a -> List a
reverse =
    List.foldl (::) []


concat : List a -> List a -> List a
concat l1 l2 =
    List.foldr (::) l2 l1


map : (a -> b) -> List a -> List b
map f =
    List.foldr (\x xs -> f x :: xs) []


filter : (a -> Bool) -> List a -> List a
filter p =
    List.foldr
        (\x xs ->
            if p x then
                x :: xs
            else
                xs
        )
        []



-- Aufgabe 2


type alias IntSet =
    Int -> Bool


empty : IntSet
empty =
    \_ ->
        False


isElem : IntSet -> Int -> Bool
isElem =
    identity


singleton : Int -> IntSet
singleton x =
    \y -> x == y


insert : Int -> IntSet -> IntSet
insert x s =
    \y -> y == x || isElem s y



-- insert = union << singleton


remove : Int -> IntSet -> IntSet
remove x s =
    \y -> y /= x && isElem s y


union : IntSet -> IntSet -> IntSet
union s1 s2 =
    \x -> s1 x || s2 x


intersect : IntSet -> IntSet -> IntSet
intersect s1 s2 =
    \x -> s1 x && s2 x


listToSet : List Int -> IntSet
listToSet =
    List.foldr insert empty
