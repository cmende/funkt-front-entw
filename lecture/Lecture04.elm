module Lecture04 exposing (..)

import Lecture03 exposing (IntList(..))


-- Polymorphismus
-- Adhoc
-- Parametrischen
-- Generics


type Maybe a
    = Just a
    | Nothing


m1 : Maybe Int
m1 =
    Nothing


m2 : Maybe Int
m2 =
    Just 1


m3 : Maybe String
m3 =
    Just "a"


type Result error value
    = Ok value
    | Err error


r1 : Result Bool IntList
r1 =
    Ok Nil


r2 : Result Bool IntList
r2 =
    Err True



-- type List
--     = Nil
--     | Cons a (List a)
--
--
-- l1 : List Int
-- l1 =
--     Nil
--
--
-- l2 : List Int
-- l2 =
--     Cons 1 (Cons 2 Nil)
--
--
-- l3 : List String
-- l3 =
--     Cons "Hello" (Cons "World" (Cons "!" Nil))


l1 : List Int
l1 =
    []


l2 : List String
l2 =
    "a" :: []


l3 : List String
l3 =
    "b" :: "a" :: []


l4 : List String
l4 =
    [ "c", "b", "a" ]
