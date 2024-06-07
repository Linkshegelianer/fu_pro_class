module Uebung4 where

{-
Übung zur Vorlesung
Funktionale Programmierung
Sommersemester 2024
Übungsblatt 4
-}

{-
Ausgabe: 10.05.2024
Abgabe: keine
-}

{-
Ab diesem Blatt werden die Übungen um QuickCheck-Tests ergänzt.
Sie finden im Moodle ein Video, wie Sie die Tests nutzen können.
Um QuickCheck lokal zu installieren, empfehlen wir in dem Pfad/Ordner,
in dem Sie die Übungsblätter speichern und bearbeiten, folgenden Befehl 
auszuführen:

cabal install --lib --package-env . QuickCheck

Stellen Sie gegebenenfalls sicher, dass Sie eine aktuelle cabal Version installiert
haben. Hilfreich hierfür ist der im Video gezeigte Befehl:

ghcup tui

Sie können sich natürlich selbstständig in den Haskell-Toolstack einarbeiten
und für die Übungsblätter ein cabal-Project anlegen. Wir wollen die Übung
möglichst leichtgewichtig halten und uns auf die Konzepte der 
funktionalen Programmierung konzentrieren, weshalb nicht weiter auf den 
Haskell-Toolstack eingegangen wird.

Die Übungen können komplett ohne QuickCheck gelöst werden und QuickCheck, 
bzw. das Schreiben von Tests mittels QuickCheck, ist NICHT klausurrelevant!
Die Tests sollen Ihnen lediglich ein weiteres Tool an die Hand geben, um
ihre Lösungen bereits beim Bearbeiten des Blattes zu überprüfen und evtl.
Fehler zu finden.

Denken Sie immer daran: Tests zeigen nur, dass ihr Code für die getesteten Eingaben
gewisse Eigenschaften erfüllt.
Tests sagen Ihnen nicht, dass ihre Lösung korrekt ist.
Wenn aber die Tests schief gehen, dann ist ihre Lösung sehr wahrscheinlich falsch...
-}

{-
Die folgenden beiden Importe sind nur relevant, wenn Sie
- QuickCheck installiert haben
- und die Tests am Ende der Datei nutzen wollen.
Dann sollten Sie diese auskommentieren.

Das Blatt soll ohne die Importe gelöst werden!
Wie immer dürfen alle Funktionen aus dem Prelude verwendet werden.
-}
{-
import Test.QuickCheck
import Data.List (nub, elemIndices)
-}

{-
Aufgabe 4.1 - Lexikographisches Produkt
-}

{-
a)
Implementieren Sie eine Haskell-Funktion
bag :: Eq a => [a] -> [(a, Int)],
die die Vorkommen einzelner Elemente in der Liste zählt und eine Liste
von Tupeln zurückgibt, die in der ersten Projektion das Element hat und in
der zweiten Projektion die Anzahl dieses Elements in der Argumentliste.

Nutzen Sie zur Implementierung foldl auf sinnvolle,
nicht-triviale Weise.

Beispielaufrufe:
bag [1,1,1,2,2,3,4,4,4,5]
~>
[(1,3),(2,2),(3,1),(4,3),(5,1)]

bag "Hello World!"
~>
[('H',1),('e',1),('l',3),('o',2),(' ',1),('W',1),('r',1),('d',1),('!',1)]
-}

--bag :: Eq a => [a] -> [(a, Int)]



{-
b)
Implementieren Sie eine Haskell-Funktion
lexOrd :: Ord a => [a] -> [a] -> Ordering,
die zwei Listen lexikographisch ordnet
(https://de.wikipedia.org/wiki/Lexikographische_Ordnung).
-}

--lexOrd :: Ord a => [a] -> [a] -> Ordering



{-
c)
Implementieren Sie eine Haskell-Funktion
bagOrd :: Ord a => [a] -> [a] -> Ordering,
die zwei Listen wie folgt ordnet:
Zähle die Vorkommen aller Elemente und finde das
maximale Vorkommen eines Elements für jede Liste.
Vergleiche die Listen anhand dieser Werte.

Beispielaufrufe:
bagOrd [1,1,1,2,2,3,4,4,4,5] [3,3,3,3] ~> LT

bagOrd [1,1,1,2,2,3,4,4,4,5] [1000,10] ~> GT

bagOrd [1,1,1] [5,5,5,6,7,8] ~> EQ
-}

--bagOrd :: Ord a => [a] -> [a] -> Ordering



{-
d)
Implementieren Sie eine Haskell-Funktion
lexProd :: Ord a => [a] -> [a] -> Ordering,
die dem lexikographische Produkt von der Längenordnung auf Listen,
bagOrd und lexOrd entspricht.

Das heißt, dass die Listen zuerst anhand ihrer Länge verglichen werden sollen.
Ist ihre Länge gleich, sollen Sie mittels bagOrd verglichen werden und falls Sie unter
bagOrd ebenfalls gleich sind, soll das Ergebnis von lexOrd zurückgegeben werden.

Beispielaufrufe:

lexProd [1,1,1,2,2,3,4,4,4,5] [1,1,1,2,2,3,4,4,4,5] ~> EQ

lexProd [1,1,1,2,2,3,4,4,4,5] [1,1,2,1,2,3,4,4,4,5] ~> LT

lexProd [1,1,1,1,2,3,4,4,4,5] [2,1,1,1,2,3,4,4,4,5] ~> GT
-}

-- 1. length 2. bagOrd 3. lexOrd
--lexProd :: Ord a => [a] -> [a] -> Ordering



{-
Aufgabe 4.2 - Matrizen
-}

{-
Wir können Vektoren als Listen von Integern modellieren.
Eine Matrix kann dann als Liste von Spaltenvektoren modelliert werden.

Sie dürfen der Einfachheit halber für alle Matrizen voraussetzen,
dass alle Spaltenvektoren dieselbe Länge haben und nicht leer sind.
-}

type Spalte = [Integer]
type Matrix = [Spalte]

{-
a)
Implementieren Sie eine Haskell-Funktion
skalarProdukt :: [Integer] -> [Integer] -> Integer,
die das Skalarprodukt zweier Vektoren berechnet.
Vektoren werden hier als Listen von Integern modelliert und Sie dürfen
der Einfachheit halber voraussetzen, dass die beiden Argumentlisten
dieselbe Länge haben.
-}

--skalarProdukt :: [Integer] -> [Integer] -> Integer



{-
b)
Implementieren Sie eine Haskell-Funktion
transpose :: Matrix -> Matrix,
die eine Matrix transponiert.

Tipp: Eine Matrix ohne Einträge (mit leeren Spaltenvektoren) kann gleichbedeutend
zur leeren Liste behandelt werden. Es kann Sinn machen, diesen Fall explizit mittels
Pattern Matching zu behandeln.

Beispielaufruf:
transpose [[1,2,3], [4,5,6], [7,8,9]] ~> [[1,4,7],[2,5,8],[3,6,9]]

transpose [[1,4,7],[2,5,8],[3,6,9]] ~> [[1,2,3],[4,5,6],[7,8,9]]
-}

--transpose :: Matrix -> Matrix



{-
c)
Implementieren Sie eine Haskell-Funktion
matrixMult :: Matrix -> Matrix -> Matrix,
die der Multiplikation zweier Matrizen entspricht.
Sie dürfen der Einfachheit halber voraussetzen,
dass die Dimensionen der Argumentmatrizen korrekt sind.
-}

--matrixMult :: Matrix -> Matrix -> Matrix



{-
Aufgabe 4.* - Fibonacci mal ganz anders...
-}

{-
Sie können die n-te Fibonacci-Zahl berechnen, indem Sie die Matrix

1 1
1 0

mit n exponenzieren ((n-1)-mal mit sich selbst multiplizieren).

Mathematisch ausgedrückt kann die Fibonacci-Sequenz also über folgendes
lineares Gleichungssystem beschrieben werden:

(F_{n+2} F_{n+1})       (1  1)    (F_{n+1} F_{n}  )
(               )   =   (    ) *  (               )
(F_{n+1} F_{n}  )       (1  0)    (F_{n}   F_{n-1})

Implementieren Sie eine Haskell-Funktion
fib :: Int -> Integer,
die die n-te Fibonacci-Zahl nach diesem Prinzip berechnet und vergleichen
Sie die Laufzeit und den Speicherbedarf zu den Implementierungen von Blatt 3.
-}

--fib :: Int -> Integer




{-
#########################################################################################
############################## QuickCheck - Tests #######################################
#########################################################################################
-}

{-
Die Aufgaben können einzeln getestet werden.
Für einen Test brauchen wir auch immer mindestens eine Eigenschaft, die wir testen wollen.
Das Generieren der tatsächlichen Testinstanzen übernimmt QuickCheck.
Wenn Sie in einer Aufgabe eine Funktion f implementieren müssen, dann gilt folgendes 
Namensschema:
Die zu testenden Eigenschaften werden mit
prop_f
oder falls mehrere Eigenschaften getestet werden
prop_f_eigenschaft
benannt.
Die Tests testen immer auf alle definierten Eigenschaften einer Funktion f und werden als
test_f benannt.

Um eine Aufgabe zu testen, müssen Sie den entsprechenden Code auskommentieren.
-}

{-
Aufgabe 4.1 a)
-}
{-
prop_bag as = length as > 5 && nub as /= as ==> bag as == map (\x -> (x, length $ elemIndices x as)) (nub as) where
  types = as :: String

test_bag = putStrLn "Test bag:" >> quickCheck prop_bag
-}

{-
Aufgabe 4.1 b)
-}
{-
prop_lexOrd xs ys = length xs > 5 && nub ys /= ys ==> if xs == ys then lexOrd xs ys == lexOrd ys xs else lexOrd xs ys == compare xs ys where
  types = (xs :: [Int], ys :: [Int])

test_lexOrd = putStrLn "Test lexOrd:" >> quickCheck prop_lexOrd
-}

{-
Aufgabe 4.1 c)
-}
{-
prop_bagOrd xs ys = length xs > 5 && length ys > 8 && nub ys /= ys && nub xs /= xs ==> bagOrd xs ys == compare (f xs) (f ys)  where
  f as = maximum $ map (length . (flip elemIndices as)) (nub as)
  types = (xs :: [Int], ys :: [Int])

test_bagOrd = putStrLn "Test bagOrd:" >> quickCheck prop_bagOrd
-}

{-
Aufgabe 4.1 d)
-}
{-
prop_lexProd xs ys = length xs > 5 && length ys > 8 && nub ys /= ys && nub xs /= xs ==> case lexProd xs ys of
  EQ -> length xs == length ys || bagOrd xs ys == EQ || xs == ys
  LT -> length xs < length ys || (length xs == length ys && bagOrd xs ys == LT) || (length xs == length ys && bagOrd xs ys == EQ && xs < ys)
  GT -> length xs > length ys || (length xs == length ys && bagOrd xs ys == GT) || (length xs == length ys && bagOrd xs ys == EQ && xs > ys) where
    types = (xs :: [Int], ys :: [Int])

test_lexProd = putStrLn "Test lexProd:" >> quickCheck prop_lexProd
-}

{-
Aufgabe 4.2 a)
-}
{-
prop_skalarProdukt_symmetrisch (NonEmpty xs) = forAll (vector (length xs)) $ \ys -> skalarProdukt xs ys == skalarProdukt ys xs where
  types = xs :: [Integer]

prop_skalarProdukt_homogen (NonEmpty xs) n = forAll (vector (length xs)) $ \ys -> skalarProdukt (map (*n) xs) ys == skalarProdukt ys (map (*n) xs) && skalarProdukt ys (map (*n) xs) == n * skalarProdukt xs ys where
  types = (xs :: [Integer], n :: Integer)

prop_skalarProdukt_additiv (NonEmpty as) = forAll (vector (length as)) $ \bs -> forAll (vector (length bs)) $ \cs -> 
  skalarProdukt as (zipWith (+) bs cs) == skalarProdukt as bs + skalarProdukt as cs && 
  skalarProdukt (zipWith (+) as bs) cs == skalarProdukt as cs + skalarProdukt bs cs where
    types = as :: [Integer]

test_skalarProdukt = putStrLn "Test skalarProdukt:" >> 
  putStrLn "Test symmetrisch:" >> quickCheck prop_skalarProdukt_symmetrisch >> 
  putStrLn "Test homogen:" >> quickCheck prop_skalarProdukt_homogen >> 
  putStrLn "Test additiv:" >> quickCheck prop_skalarProdukt_additiv
-}

{-
Aufgabe 4.2 b)
-}
{-
prop_transpose n m = n > 0 && m > 0 ==> forAll (vectorOf m (vector n)) $ \x -> transpose (transpose x) == x where --generator für n X m - Matrizen
  types = (n :: Int, m :: Int)

test_transpose = putStrLn "Test transpose:" >> quickCheck prop_transpose
-}

{-
Aufgabe 4.2 c)
-}
{-
prop_matrixMult n m l = n > 0 && m > 0 && l > 0 ==> 
  forAll (vectorOf n (vector m)) $ \mXn -> 
  forAll (vectorOf l (vector n)) $ \nXl -> 
  length (matrixMult mXn nXl) == l && all ((== m) . length) (matrixMult mXn nXl) where
    types = (n :: Int, m :: Int, l :: Int)

test_matrixMult = putStrLn "Test matrixMult:" >> quickCheck prop_matrixMult
-}

{-
Aufgabe 4.*
-}
{-
prop_fib (NonNegative n) = n < 10000 ==> fib n == fibs !! n where
  fibs:: [Integer]
  fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
  types = n :: Int

test_fib = putStrLn "Test fib:" >> quickCheck prop_fib
-}

{-
Wenn Sie alle Aufgaben gelöst und getestet haben, dann können Sie alle Tests mittels
test_all auf einmal ausführen.
-}
{-
test_all = do
  test_bag
  test_lexOrd
  test_bagOrd
  test_lexProd
  test_skalarProdukt
  test_transpose
  test_matrixMult
  test_fib
-}