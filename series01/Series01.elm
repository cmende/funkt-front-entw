module Series01 exposing (..)

-- Aufgabe 2


ggT : Int -> Int -> Int
ggT n m =
    case m of
        0 ->
            n

        _ ->
            ggT m (n % m)


kgV : Int -> Int -> Int
kgV n m =
    (n * m) // (ggT n m)



-- Aufgabe 3


sumN : Int -> Int
sumN n =
    if n < 0 then
        -1
    else if n == 0 then
        0
    else
        n + sumN (n - 1)


fac : Int -> Int
fac n =
    if n < 0 then
        -1
    else if n == 0 then
        1
    else
        n * fac (n - 1)



-- Aufgabe 4


sumN2 : Int -> Int
sumN2 n =
    sumN_end n 0


sumN_end : Int -> Int -> Int
sumN_end n m =
    if n < 0 then
        -1
    else if n == 0 then
        m
    else
        sumN_end (n - 1)
            (m + n)


fac2 : Int -> Int
fac2 n =
    fac_end n 1


fac_end : Int -> Int -> Int
fac_end n m =
    if n < 0 then
        -1
    else if n == 0 then
        m
    else
        fac_end (n - 1) (n * m)
