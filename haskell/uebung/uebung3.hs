module Uebung3 where

{-
Übung zur Vorlesung
Funktionale Programmierung
Sommersemester 2024
Übungsblatt 3
-}

{-
Ausgabe: 03.05.2024
Abgabe: keine
-}

{-
Aufgabe 3.1 - Fibonacci Zahlen
-}

{-
Auf den Doberkat-Folien 94-97 finden Sie folgende Haskell-Funktionen zur Berechnung
der n-ten Fibonacci Zahl, bzw. die Liste aller Fibonacci Zahlen fibs :: [Integer].
-}

{-
a)
Machen Sie sich die unterschiedlichen Funktionsweisen und Rekursionsprinzipien
der unterschiedlichen Implementierungen klar.
Untersuchen Sie diese auch hinsichtlich ihrer Effizienz, indem Sie Laufzeiten und
Speicherbedarf analysieren. Hierzu können Sie im GHCi ":set +s" eingeben, um sich
Laufzeit und Speicherbedarf von Aufrufen anzeigen zu lassen.
-}

{-
Der "naive" Ansatz.
Betrachten Sie z.B. die Laufzeit des Aufrufs
fib 35
-}
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

{-
Ab hier können Sie problemlos deutlich größere Eingaben testen,
z.B. fib2 100000
-}
fib1 :: Integer -> (Integer, Integer) -> (Integer, Integer)
fib1 0 (a, b) = (a, b)
fib1 n (a, b) = fib1 (n-1) (b, a + b)

fib2 :: Integer -> Integer
fib2 n = fst (fib1 n (0, 1))

fib3 :: Integer -> Integer
fib3 n = fst (fibLokal n (0, 1))
  where
    fibLokal :: Integer -> (Integer, Integer) -> (Integer, Integer)
    fibLokal 0 (a, b) = (a, b)
    fibLokal n (a, b) = fibLokal (n-1) (b, a + b)

{-
Man kann auch Listen statt Paaren nutzen...
-}
fib4 n = (fibs' 0 1) !! n where
  fibs' :: Integer -> Integer -> [Integer]
  fibs' a b = a : fibs' b (a+b)

fibs:: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

{-
Die Liste fibs kann zur Definition der Funktion fib5 zur Berechnung der
n-ten Fibonacci Zahl genutzt werden.
-}

fib5 n = fibs !! n

{-
b)
Geben Sie Gründe für die Unterschiede in Laufzeit und Speicherbedarf an.

Erläutern Sie den Unterschied zwischen fib2 und fib3 und betrachten Sie auch
zusätzlich die Funktion fib4, die einem logischen "Zwischenschritt" der Implementierungen
von fib3 und fib5 entspricht.
-}

{-
c)
Implementieren Sie die Funktion fib3 erneut indem Sie die lokale Hilfsfunktion
fibLokal sinnvoll curryfiziert implementieren, sodass Sie den Typ
fibLokal :: Integer -> Integer -> Integer -> Integer hat.
Nennen Sie die Funktion fib3' :: Integer -> Integer

Implementieren Sie auch die Funktion fib4 erneut indem Sie die lokale Hilfsfunktion
fibs' sinnvoll uncurryfiziert implementieren, sodass Sie den Typ
fibs' :: (Integer, Integer) -> [Integer] hat.
Nennen Sie die Funktion fib4' :: Int -> Integer.

Sie sollen sich die curryfizierten und uncurryfizierten Varianten der Hilfsfunktionen explizit durch
sinvolle Definitionen klar machen. Die Funktionen curry und uncurry dürfen also
nicht verwendet werden!
-}

--fib3' :: Integer -> Integer

--fib4' :: Integer -> Integer



{-
Aufgabe 3.2 - Binärzahlen
-}

{-
Wir können Binärzahlen als Listen von Bools modellieren.
Der Einfachheit halber betrachten wir die Binärzahlen gemäß LSB 0
(Least Significant Bit first) nummeriert.
(Vgl. https://de.wikipedia.org/wiki/Bitwertigkeit)

Ein paar Beispiele, wie Dezimalzahlen als Binärzahlen in unserem Modell
dargestellt werden können:

0 ~ []
0 ~ [False]
1 ~ [True]
8 ~ [False,False,False,True]
42 ~ [False,True,False,True,False,True]

Wir wollen keine Vorzeichenbits betrachten. Wir modellieren also ausschließlich
positive ganze Zahlen.
-}

{-
a) Implementieren Sie eine Haskell-Funktion int2Bin :: Int -> [Bool], die eine
positive ganze Zahl in eine Liste von Bools übersetzt, die der zuvor beschriebenen
Modellierung von Binärzahlen entspricht.

Tipp: Die Funktion div :: Integral a => a -> a -> a aus dem Prelude darf benutzt werden
und entspricht der ganzzahligen Division.
-}

--int2bin :: Int -> [Bool]


{-
b) Implementieren Sie eine Haskell-Funktion bin2Int :: [Bool] -> Int, die eine
Liste von Bools, wie zuvor beschrieben, als Binärzahl interpretiert und den Wert als
Dezimalzahl, bzw. Int zurückgibt.

Nutzen Sie zur Definition die Linksfaltung
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
auf nicht-triviale, sinnvolle Weise.

Tipp: Wenn man sich die Funktionsweise von foldl klar macht,
bezeichnen manche das zweite Argument (im obigen Typen mit der Typvariable b getypt)
als "Akkumulator".
Beim Übersetzen einer Binär- in eine Dezimalzahl kann es helfen zwei Informationen im Blick haben:
-Die aktuelle Stelle, bzw. dessen Wertigkeit und
-das Zwischenergebnis aller bisher betrachteten Stellen.
Als Akkumulator der Faltung könnte sich also ein Tupel dieser beiden Informationen anbieten.
Um bin2int zu implementieren, muss man dann natürlich noch die entsprechende Projektion anwenden.
-}

--bin2int :: [Bool] -> Int


{-
c) Implementieren Sie eine Funktion binAdd :: [Bool] -> [Bool] -> [Bool] die zwei als
Liste von Bools kodierte Binärzahlen als Argumente nimmt und entsprechend der Logik der
schriftlichen Addition von Binärzahlen stellenweise addiert.

Tipp: Die Funktion lässt sich am einfachsten Implementieren, indem man Sie in sinnvolle
Hilfsfunktionen unterteilt.
Hilfsfunktionen könnten z.B. ein Halbaddierer zur Definition eines Volladdierers sein, sowie die
Addition zweier Binärzahlen unter Beachtung eines Übertrags (hier könnte der Volladdierer helfen)...
-}

--binAdd :: [Bool] -> [Bool] -> [Bool]


{-
Aufgabe 3.* - Fibonacci mal anders...
-}

{-
Wir können 2x2-Matrizen statt als Liste von Listen auch als Tupel von Tupeln modellieren.
Implementieren Sie eine Funktion
matrixSquare :: ((Integer, Integer), (Integer, Integer)) -> ((Integer, Integer), (Integer, Integer))
die eine 2x2-Matrix mit sich selbst multipliziert.

Die Matrix

  a b
  c d

würde also als Tupel ((a, b), (c, d)) interpretiert werden.

Interessanterweise kann man die 2^n-te Fibonacci Zahl berechnen, indem man die Matrix

  1 1
  1 0

n-mal mit sich selber multipliziert. Die 2^n-te Fibonacci Zahl steht dann entweder in der
2. Spalte der ersten Zeile, oder der ersten Spalte der zweiten Zeile
(also auf der Diagonalen von links unten nach rechts oben).

Implementieren Sie die Haskell-Funktion fib2HochN :: Integer -> Integer, die die 2^n-te
Fibonacci Zahl durch sinnvollen Aufruf von matrixSquare berechnet.

Vergleichen Sie Laufzeit und Speicherbedarf mit den Implementierungen aus 3.1.
-}

--matrixSquare :: ((Integer, Integer), (Integer, Integer)) -> ((Integer, Integer), (Integer, Integer))
--matrixSquare ((a , b) , (c , d)) = 

applyNtimes f 0 x = x
applyNtimes f n x = f (applyNtimes f (n-1) x)

--fib2HochN :: Integer -> Integer

