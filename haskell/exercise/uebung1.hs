module Uebung1 where

{-
Aufgabe 1.1  - Rekursive Funktionen
-}

{-
a) :i fakt1 -> fakt1 :: Integer -> Integer     -- Defined at uebung1.hs:18:1
The difference between Int -> Int & Integer -> Integer: 
- Integer is restricted obly by memory => Should be used with huge numbers
- Int has performance advantages since it's fixed-size
-}

fakt1 :: Integer -> Integer
fakt1 n = if n == 0
          then 1
          else n * fakt1 (n-1)

{-
b) Die Türme von Hanoi:

           { 1                  , wenn n = 1
hanoi(n) = {
           { 2 * hanoi(n-1) + 1 , sonst
-}

{-
n = number of disks
output = the minimum number of steps required to move all n disks 
from the source peg to the destination peg
-}
hanoi :: Int -> Int -- type signature
-- function body
hanoi n 
  | n <= 0    = 0 -- base case
  | otherwise = 2 * hanoi (n - 1) + 1 -- recursive case


{-
c) Implementieren Sie eine Haskell Funktion
applyNtimes :: (a -> a) -> Int -> a -> a
erstes Argument eine einstellige Funktion f (a -> a);
zweites Argument: eine positive ganze Zahl n Int;
drittes Argument: ein Element x auf das die Funktion aus dem ersten Argument
angewendet werden kann.

Die Funktion applyNtimes soll dann die Funktion f aus dem ersten Argument n mal auf
das Element x anwenden.
-}

-- f -> n -> x -> result
applyNtimes :: (a -> a) -> Int -> a -> a
applyNtimes f n x 
  | n <= 0    = x -- base case
  | otherwise = applyNtimes f (n - 1) (f x) -- recursive case


-- Test
exponentTest :: Int -> Int -> Int -- receives two integers n & x
exponentTest n x
  | n <= 0    = 1 -- base case for exponentiation
  | otherwise = applyNtimes (* x) (n - 1) x
  
{-
Aufgabe 1.2 - Lambda Ausdrücke
-}

{-
a) flip :: (a -> b -> c) -> b -> a -> c
Die Funktion flip nimmt eine Funktion f :: a -> b -> c und gibt eine Funktion
f' :: b -> a -> c zurück. Bedenken Sie, dass der Funktionspfeil rechtsassoziativ ist.
Nennen Sie den Lambda Ausdruck flip'.
-}

flip' :: (a -> b -> c) -> b -> a -> c -- receives a function which takes func f
flip' f =  \x y -> f y x -- swap order of arguments using anonymous function

{-
b) curry :: ((a, b) -> c) -> a -> b -> c

Sie nimmt eine Funktion f :: (a, b) -> c als Argument und gibt eine Funktion
f' :: a -> b -> c zurück. 
-}

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \x y -> f (x, y)

{-
c) Geben Sie einen Lambda Ausdruck für eine Funktion an, der zwei Argumente b und e
nimmt und durch sinnvollen Aufruf von applyNtimes b^e berechnet.
Nennen sie den Lambda Ausdruck exponential.
Also z.B.
exponential 2 0 ~> 1
exponential 2 4 ~> 16
exponential 3 2 ~> 9
usw. ...
-}

exponential :: Int -> Int -> Int
exponential = \b e -> applyNtimes (\x -> x * b) e 1
