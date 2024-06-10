module Uebung0 where

{-
ghci uebung0.hs
-}

add :: Int -> Int -> Int
add x y = x + y

{-
ghc uebung0.hs -> ./uebung0.exe
-}

main :: IO()
main = putStrLn "Hello World!"

{-
Partielle Funktionsanwendung / partial function application:
$ operator is used to reduce the need for parentheses in expressions:
f (g (h x)) -> f $ g $ h x
-}

successor :: Int -> Int
successor = (+) 1

add4 :: Int -> Int
add4 x = successor $ successor $ successor $ successor x

{-
b) Die Funktionsanwendung in Haskell ist per Definition bekanntermaßen
linksassoziativ.
Erweitern Sie die Definition der konstanten Funktion ten :: Int um die
impliziten Klammern.
-}

add4Ints :: Int -> Int -> Int -> Int -> Int
add4Ints a b c d = a + b + c + d

ten :: Int
ten = (((add4Ints 1) 2) 3) 4

{-
Aufgabe 0.* - konstante und nullstellige Funktionen
-}

{-
Definieren Sie eine Funktion funPlus, die isomorph zur Funktion add ist
und anstatt von zwei ganzen Zahlen, zwei einstellige Funktionen (z.B. funEins)
als Argumente nimmt.
-}

funEins :: () -> Int
funEins _ = 1

funZwei :: () -> Int
funZwei x = 2

funDrei :: () -> Int
funDrei () = 3

funPlus :: (() -> Int) -> (() -> Int) -> Int
funPlus a b = a () + b ()