module Uebung1 where

{-
Übung zur Vorlesung
Funktionale Programmierung
Sommersemester 2024
Übungsblatt 1
-}

{-
Block: 1
Ausgabe: 19.04.2024
Abgabe: keine
-}

{-
Aufgabe 1.1  - Rekursive Funktionen
-}

{-
a) Gegeben sei die Fakultätsfunktion fakt1 :: Int -> Int von den Vorlesungsfolien.
Laden Sie uebung1.hs im GHCi und finden Sie das größte Argument vom Typ Int,
für das die Funktion fakt1 ein sinnvolles Ergebnis liefert.
Ändern Sie den Typ von Int -> Int zu Integer -> Integer und probieren Sie ein paar
größere Eingaben aus.
Vergleichen Sie die Informationen, die Ihnen der GHCi mittels des Kommandos :i
zu den Typen Int und Integer liefert.
-}

fakt1 :: Int -> Int
fakt1 n = if n == 0
          then 1
          else n * fakt1 (n-1)

{-
b) Die Türme von Hanoi (https://de.wikipedia.org/wiki/Türme_von_Hanoi) sind ein
bei Informatikern beliebtes Puzzle, um rekursive Algorithmen zu motivieren.
Man kann die Anzahl an Schritten, die benötigt wird um n Scheiben von einem Stab
zu einem anderen zu bewegen, nach folgendem Rekursionsprinzip berechnen:

           { 1                  , wenn n = 1
hanoi(n) = {
           { 2 * hanoi(n-1) + 1 , sonst

Implementieren Sie eine Haskell Funktion hanoi :: Int -> Int, die für alle
Eingaben n > 0 die Anzahl an Schritten nach dem obigen Rekursionsprinzip berechnet,
um n Scheiben von einem Stab zum anderen zu bewegen.
Für alle Eingaben n <= 0 soll 0 zurückgegeben werden.
-}

--hanoi :: Int -> Int



{-
c) Implementieren Sie eine Haskell Funktion
applyNtimes :: (a -> a) -> Int -> a -> a
die als erstes Argument eine einstellige Funktion f nimmt,
als zweites Argument eine positive ganze Zahl n und
als drittes Argument ein Element x auf das die Funktion aus dem ersten Argument
angewendet werden kann.
Die Funktion applyNtimes soll dann die Funktion f aus dem ersten Argument n mal auf
das Element aus dem dritten Argument anwenden.

Die Funktion soll also z.B.  wie folgt ausgewertet werden:
applyNtimes f 0 x ~> x
applyNtimes f 2 x ~> f (f x)
applyNtimes f 5 x ~> f (f (f (f (f x))))
usw. ...
-}

--applyNtimes :: (a -> a) -> Int -> a -> a



{-
Aufgabe 1.2 - Lambda Ausdrücke
-}

{-
a) Geben Sie einen Lambda Ausdruck für die Funktion
flip :: (a -> b -> c) -> b -> a -> c
aus dem Prelude an. 
Die Funktion flip nimmt eine Funktion f :: a -> b -> c und gibt eine Funktion
f' :: b -> a -> c zurück. Bedenken Sie, dass der Funktionspfeil rechtsassoziativ ist.
Nennen Sie den Lambda Ausdruck flip'.
-}

--flip' = 

{-
b) Geben Sie einen Lambda Ausdruck für die Funktion
curry :: ((a, b) -> c) -> a -> b -> c
aus dem Prelude an.
In den Doberkat-Folien wurde bereits auf Folie 14 die sogenannte Curryfizierung
angesprochen.
Die Funktion curry implementiert die Curryfizierung (Kaskadierung) einer Funktion.
Sie nimmt eine Funktion f :: (a, b) -> c als Argument und gibt eine Funktion
f' :: a -> b -> c zurück. Bedenken Sie, dass der Funktionspfeil rechtsassoziativ ist.
Nennen Sie den Lambda Ausdruck curry'.
-}

--curry' = 

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

--exponential = 