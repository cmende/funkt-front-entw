module Lecture03 exposing (..)

-- Algebraische Datentypen
-- Summentyp


type Weekday
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday



-- Produkttyp


type Date
    = Date Weekday Int Int Int


date : Date
date =
    Date Monday 1 2 3


type alias Type11 =
    Int


type alias Type12 =
    Int


type alias Type21 =
    Int


type AlgDataType
    = Constr1 Type11 Type12
    | Constr2 Type21
    | Constr3



-- foo : AlgDataType -> Bool
-- foo a =
--     case a of
--         Constr1 x _ ->
--             True
--
--         Constr2 x ->
--             False
--
--         Constr3 ->
--             True


dateWorkday : Date -> Bool
dateWorkday (Date d _ _ _) =
    case d of
        Sunday ->
            False

        Saturday ->
            False

        _ ->
            True


foo : ( Int, Char ) -> Int
foo t =
    case t of
        ( 1, 'a' ) ->
            3

        _ ->
            4


bar : ( Int, Char, String ) -> Int
bar ( i, c, s ) =
    i



-- baz : ( Int, Char, String ) -> Int


buz t =
    let
        ( i, c, s ) =
            t
    in
        i


lets : Int
lets =
    let
        id x =
            x

        x =
            2

        y =
            4

        z =
            5
    in
        x + y + z


type IntList
    = Nil
    | Cons Int IntList


list1 : IntList
list1 =
    Nil


list2 : IntList
list2 =
    Cons 1 Nil


list3 : IntList
list3 =
    Cons 1 (Cons 2 Nil)


list4 : IntList
list4 =
    Cons 3 list2


length : IntList -> Int
length xs =
    case xs of
        Nil ->
            0

        Cons _ ys ->
            1 + length ys


everySecond : IntList -> IntList
everySecond xs =
    case xs of
        Nil ->
            Nil

        Cons x Nil ->
            Nil

        Cons _ (Cons y ys) ->
            Cons y (everySecond ys)


type IntTree
    = Node IntTree Int IntTree
    | Leaf Int


tree1 : IntTree
tree1 =
    Leaf 1


tree2 : IntTree
tree2 =
    Node (Leaf 1) 2 (Leaf 3)


find : Int -> IntTree -> Bool
find n t =
    case t of
        Leaf m ->
            n == m

        Node lt m rt ->
            if n == m then
                True
            else if n < m then
                find n lt
            else
                find n rt



-- Records


hasFullAge : User -> Bool
hasFullAge user =
    -- user.age >= 18
    .age user >= 18


type alias User =
    { firstName : String, lastName : String, age : Int }


maturing : User -> User
maturing user =
    { user | age = 18 }


increaseAge : User -> User
increaseAge user =
    { user | age = user.age + 1 }


japanese : User -> User
japanese user =
    { user | firstName = user.lastName, lastName = user.firstName }
