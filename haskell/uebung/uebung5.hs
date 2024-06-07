module Uebung5 where

{-
Übung zur Vorlesung
Funktionale Programmierung
Sommersemester 2024
Übungsblatt 5
-}

{-
Block: 1
Ausgabe: 17.05.2024
Abgabe: keine
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
import Control.Monad
-}

{-
!!!!!!!!!!!
Dieses Blatt führt einen neuen Datentyp Nat für natürliche Zahlen ein.
Viele der Aufgaben können natürlich gelöst werden, indem man z.B. Funktionen
nat2Int und int2Nat implementiert.
Das ist aber nicht Sinn der Aufgaben und würde in der Klausur explizit
ausgeschlossen werden (keine Punkte geben).
Sie sollen hier lernen/üben Faltungen für beliebige algebraische Datentypen
zu implementieren und anzuwenden. Nutzen Sie also die Gelegenheit.
Falls Sie die Faltungen tatsächlich nicht implementiert bekommen, können Sie
die Aufgaben trotzdem per Pattern Matching lösen. Dann raten wir Ihnen aber
dringend die Übungen wahrzunehmen und Fragen zu stellen.
!!!!!!!!!!!
-}

{-
Aufgabe 5.1 - Nat
-}

data Nat = Z | S Nat deriving (Show, Eq)

{-
Gegeben sei der Haskell-Datentyp Nat für natürliche Zahlen.
Die natürlichen Zahlen 0,1,2,3 entsprechen dann z.B. den folgenden Nat-Termen:
-}

nullN :: Nat
nullN = Z

einsN :: Nat
einsN = S Z

zweiN :: Nat
zweiN = S $ S Z

dreiN :: Nat
dreiN = S $ S $ S Z

{-
a)
Implementieren Sie eine Haskell-Funktion
foldNat :: b -> (b -> b) -> Nat -> b,
die als erstes Argument eine Interpretation des Z-Konstruktors und
als zweites Argument eine Interpretation des S-Konstruktors nimmt und
eine Funktion zurückgibt, die einen Nat-Term unter dieser Interpretation
auswertet.
Die Faltung foldNat soll also alle Vorkommen des Z-Konstruktors durch das erste
Argument und alle Vorkommen des S-Konstruktors durch das zweite Argument
ersetzen.

Beispielauswertungen:

foldNat f g (Z) ~> f
foldNat f g (S $ Z) ~> g f
foldNat f g (S $ S Z) ~> g $ g f
...

-}

--foldNat :: b -> (b -> b) -> Nat -> b


{-
b)
Implementieren Sie eine Haskell-Funktion
add :: Nat -> Nat -> Nat,
die zwei natürliche Zahlen addiert.
Nutzen Sie hierfür die Faltung foldNat auf sinnvolle und nicht triviale Weise.
-}

--add :: Nat -> Nat -> Nat


{-
c)
Implementieren Sie eine Haskell-Funktion
nat2binär :: Nat -> [Bool],
die eine natürliche Zahl in die Binärzahldarstellung aus Übung 3
(Listen von Bools, LSB 0) übersetzt.
Nutzen Sie hierfür die Faltung foldNat auf sinnvolle und nicht triviale Weise.
-}

--nat2binär :: Nat -> [Bool]



{-
Aufgabe 5.2 - Maybe & Either
-}

{-
a)
Implementieren Sie eine Haskell-Funktion
predNat :: Nat -> Maybe Nat,
die den Vorgägner einer natürlichen Zahl berechnet, sofern dieser existiert.
Wenn kein Vorgänger existiert soll Nothing zurückgegeben werden.

Anmerkung: predNat darf hier gerne mittels Pattern Matching definiert werden.
           Außer Sie wollen die *-Aufgabe vorziehen.
-}

--predNat :: Nat -> Maybe Nat



{-
b)
Implementieren Sie eine Haskell-Funktion
minus :: Nat -> Nat -> Maybe Nat,
die zwei natürliche Zahlen subtrahiert. Sollte das zweite Argument größer
als das erste sein, so soll Nothing zurückgegeben werden.
Nutzen Sie hierfür die Faltung foldNat auf sinnvolle und nicht triviale Weise.

Tipp:
Guards oder ähnliches sind nicht notwendig, der "Fehlerfall" kann durch
foldNat korrekt gehandhabt werden.
-}

--minus :: Nat -> Nat -> Maybe Nat


{-
c)
Implementieren Sie eine Haskell-Funktion
elemAtIndex :: Int -> [a] -> Either String a,
die als erstes Argument einen Index und als zweites Argument eine Liste nimmt
und das Element der Liste an dem Index zurückgibt.

Hierbei können Fehler auftreten, die durch die Verwendung von (Either String)
abgefangen und durch sinnvolle Fehlermeldungen beschrieben werden sollen.
Sollte der Index größer als die Länge der Liste sein, so soll der String
"Fehler: Index zu groß" im Either zurückgegeben werden.
Sollte der Index negativ sein, so soll der String
"Fehler: Negativer Index" im Either zurückgegeben werden.
-}

--elemAtIndex :: Int -> [a] -> Either String a


{-
Aufgabe 5.3 - Binäre Bäume
-}

{-
In der Vorlesung wurde auf den Doberkat-Folien S.224ff ein Datentyp für
binäre Bäume vorgestellt und genutzt.
Wir nennen den Datentyp entgegen der Folien hier BinBaum und die Konstruktoren
BinLeer und BinKnoten.

Des weiteren sei der Datentyp Baum für Bäume beliebigen Ausgrads gegeben.
-}

data BinBaum a = BinLeer | BinKnoten a (BinBaum a) (BinBaum a) deriving (Show, Eq)

data Baum a = Leer | Knoten a [Baum a] deriving (Show, Eq)

binBaum :: BinBaum Int
binBaum = BinKnoten 4
  (BinKnoten 2 (BinKnoten 1 (BinLeer) (BinLeer)) (BinKnoten 3 (BinLeer) (BinLeer)))
  (BinKnoten 6 (BinKnoten 5 (BinLeer) (BinLeer)) (BinKnoten 7 (BinLeer) (BinLeer)))

{-
a)
Implementieren Sie eine Haskell-Funktion
convert :: BinBaum a -> Baum a,
die einen binären Baum in einen Baum beliebigen Ausgrads übersetzt.
-}

--convert :: BinBaum a -> Baum a


{-
b)
Implementieren Sie eine Haskell-Funktion
preFold :: b -> (a -> b -> b -> b) -> BinBaum a -> b,
die als erstes Argument eine Interpretation des BinLeer-Konstruktors und als
zweites Argumgent eine Interpretation des BinKnoten-Konstruktors nimmt und
eine Funktion zurückgibt, die einen BinBaum-Term unter dieser Interpretation
auswertet.

Beispielauswertung:
preFold f g BinLeer ~> f
preFold f g (BinKnoten x BinLeer BinLeer) ~> g x f f
...
-}

--preFold :: b -> (a -> b -> b -> b) -> BinBaum a -> b



{-
c)
Implementieren Sie eine Haskell-Funktion
preorder :: BinBaum a -> [a],
die einen binären Baum in preorder-Reihenfolge druchläuft und die Knotenelemente
in eine Liste schreibt.
Nutzen Sie hierfür die Faltung preFold auf sinnvolle und nicht triviale Weise.

Beispielaufruf:
preorder binBaum ~>
[4,2,1,3,6,5,7]
-}

--preorder :: BinBaum a -> [a]



{-
d)
Implementieren Sie eine Haskell-Funktion
knotenSumme :: Num a => BinBaum a -> a,
die die Summe aller Knoten eines binären Baums berechnet.
Nutzen Sie hierfür die Faltung preFold auf sinnvolle und nicht triviale Weise.

Beispielaufruf:
knotenSumme binBaum ~>
28
-}

--knotenSumme :: Num a => BinBaum a -> a



{-
Aufgabe 5.* - Destruktor
-}

{-
Implementieren Sie die Funktion
predNat :: Nat -> Maybe Nat
aus Aufgabe 5.2 a) erneut, nutzen Sie aber dieses Mal
foldNat auf sinnvolle und nicht triviale Weise und nennen Sie die Funktion
predNat'.
-}

--predNat' :: Nat -> Maybe Nat






{-
#########################################################################################
############################## QuickCheck - Tests #######################################
#########################################################################################
-}

{-
Die Aufgaben können einzeln getestet werden.
Für einen Test brauchen wir auch immer mindestens eine Eigenschaft, die wir testen wollen.
Das Generieren der tatsächlichen Testinstanzen übernimmt QuickCheck.
Wenn ihr in einer Aufgabe eine Funktion f implementieren müsst, dann gilt folgendes Namensschema:
Die Eigenschaften werden mit
prop_f
oder falls mehrere Eigenschaften getestet werden
prop_f_eigenschaft
benannt.
Die Tests testen immer auf alle Eigenschaften einer Funktion f und werden als
test_f benannt.

Um eine Aufgabe zu testen, müsst ihr den entsprechenden Code auskommentieren.
-}


{-
Wenn QuickCheck genutzt wird, müssen die folgenden Instanzen auskommentiert werden.
-}
{-
instance Arbitrary Nat where
  arbitrary = frequency [(1, return Z), (7, liftM S arbitrary)]

instance Arbitrary a => Arbitrary (BinBaum a) where
  arbitrary = sized arbTree where
    arbTree 0 = return BinLeer
    arbTree n = frequency [(1, return BinLeer), (7,  liftM3 BinKnoten arbitrary (arbTree (n `div` 2)) (arbTree (n `div` 2)))]
-}


{-
Aufgabe 5.1 a)
-}
{-
prop_foldNat n = foldNat Z S n == n

test_foldNat = putStrLn "Test foldNat:" >> quickCheck prop_foldNat
-}

{-
Aufgabe 5.1 b)
-}
{-
prop_add_symmetrisch n m = add n m == add m n

prop_add_assoziativ a b c = add (add a b) c == add a (add b c)

prop_add_Z_neutral n = add n Z == n && add Z n == n

test_add = do
  putStrLn "Test add:" 
  putStrLn "Test symmetrisch:" 
  quickCheck prop_add_symmetrisch 
  putStrLn "Test assoziativ:"
  quickCheck prop_add_assoziativ
  putStrLn "Test Z_neutral:"
  quickCheck prop_add_Z_neutral
-}

{-
Aufgabe 5.1 c)
-}
{-
prop_nat2binär n = bin2int (nat2binär n) == nat2int n where
  bin2int = fst . foldl f (0, 1) 
  f (result, arity) True = (result + arity, 2*arity)
  f (result, arity) False = (result, 2*arity)
  nat2int Z = 0
  nat2int (S n) = 1 + nat2int n

test_nat2binär = putStrLn "Test nat2binär:" >> quickCheck prop_nat2binär
-}

{-
Aufgabe 5.2 a)
-}
{-
prop_predNat n = fmap nat2int (predNat n) == case nat2int n - 1 of {(-1) -> Nothing; n -> Just n} where
  nat2int Z = 0
  nat2int (S n) = 1 + nat2int n

test_predNat = putStrLn "Test predNat:" >> quickCheck prop_predNat
-}

{-
Aufgabe 5.2 b)
-}
{-
prop_minus_invers n = minus n n == Just Z

prop_minus_Z_links_neutral n = minus n Z == Just n 

prop_minus_nicht_negativ n = minus n (S n) == Nothing

test_minus = do
  putStrLn "Test minus:"
  putStrLn "Test invers"
  quickCheck prop_minus_invers
  putStrLn "Test Z_neutral"
  quickCheck prop_minus_Z_links_neutral
  putStrLn "Test nicht_negativ:"
  quickCheck prop_minus_nicht_negativ
-}

{-
Aufgabe 5.2 c)
-}
{-
prop_elemAtIndex_Right (NonNegative n) = elemAtIndex n [0..] == Right ([0..] !! n) 

prop_elemAtIndex_Left (NonNegative n) = n > 10 ==> f (elemAtIndex (-n) [0..]) && f (elemAtIndex n [0..5]) where
  f (Left _) = True
  f _ = False

test_elemAtIndex = do
  putStrLn "Test elemAtIndex:"
  putStrLn "Test Right:"
  quickCheck prop_elemAtIndex_Right
  putStrLn "Test Left:"
  quickCheck prop_elemAtIndex_Left
-}

{-
Aufgabe 5.3 a)
-}
{-
prop_convert b = prop $ convert b where
  prop Leer = True
  prop (Knoten _ xs) = length xs == 2 && all prop xs
  types = b :: BinBaum Int

test_convert = putStrLn "Test convert:" >> quickCheck prop_convert
-}

{-
Aufgabe 5.3 b)
-}
{-
prop_preFold b = preFold BinLeer BinKnoten b == b where
  types = b :: BinBaum Int

test_preFold = putStrLn "Test preFold:" >> quickCheck prop_preFold
-}

{-
Aufgabe 5.3 c)
-}
{-
prop_preorder b = fst $ prop b 0 where
  types = b :: BinBaum Int
  prop BinLeer 0 = (preorder b == [], 0)
  prop BinLeer n = (True, n)
  prop (BinKnoten x l r) n = let p = prop l (n+1) in let p' = prop r (snd p) in ((preorder b !! n == x) && fst p && fst p', snd p')

test_preorder = putStrLn "Test preorder:" >> quickCheck prop_preorder
-}

{-
Aufgabe 5.3 d)
-}
{-
prop_knotenSumme b = knotenSumme b == summe b where
  types = b :: BinBaum Int
  summe BinLeer = 0
  summe (BinKnoten x l r) = x + summe l + summe r

test_knotenSumme = putStrLn "Test knotenSumme:" >> quickCheck prop_knotenSumme
-}

{-
Aufgabe 5.*
-}
{-
prop_predNat' n = fmap nat2int (predNat' n) == case nat2int n - 1 of {(-1) -> Nothing; n -> Just n} where
  nat2int Z = 0
  nat2int (S n) = 1 + nat2int n

test_predNat' = putStrLn "Test predNat':" >> quickCheck prop_predNat'
-}

{-
Wenn ihr alle Aufgaben gelöst und getestet habt, dann könnt ihr alle Tests mittels
test_all auf einmal ausführen.
-}
{-
test_all = do
  test_foldNat
  test_add
  test_nat2binär
  test_predNat
  test_minus
  test_elemAtIndex
  test_convert
  test_preFold
  test_preorder
  test_knotenSumme
  test_predNat'
-}