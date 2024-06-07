module Uebung6 where

{-
Die Funktion guard aus Control.Monad ist leider nicht im Prelude, darf aber
in der Übung, im Midterm-Test und der Klausur vorausgesetzt werden,
sofern die konkrete Aufgabenstellung dies nicht ausschließt.
-}
import Control.Monad ( guard )

{-
Übung zur Vorlesung
Funktionale Programmierung
Sommersemester 2023
Übungsblatt 6
-}

{-
Ausgabe: 24.05.2024
Abgabe: keine
-}

{-
Die folgenden Importe sind nur relevant, wenn Sie
- QuickCheck installiert haben
- und die Tests am Ende der Datei nutzen wollen.
Dann sollten Sie diese auskommentieren.

Das Blatt soll ohne die Importe gelöst werden!
Wie immer dürfen alle Funktionen aus dem Prelude verwendet werden.
-}

{-
import Test.QuickCheck
import Control.Monad (liftM, liftM2)
import Data.Either (isRight, isLeft)
-}

{-
Aufgabe 6.1 - Listenmonade
-}

{-
a)
Implementieren Sie die Funktion
map :: (a -> b) -> [a] -> [b]
aus dem Prelude jeweils mittels Listenkomprehension,
Do- und Bind-Notation.
Nennen Sie die Funktionen mapLK, mapDo und mapBind.
-}

--mapLK :: (a -> b) -> [a] -> [b]

--mapDo :: (a -> b) -> [a] -> [b]

--mapBind :: (a -> b) -> [a] -> [b]

{-
b)
Übersetzen Sie die Liste
pyTriples :: [(Int, Int, Int)]
in die Do- und Bind-Notation.
Nennen Sie die Listen pyTriplesDo und pyTriplesBind.
-}

pyTriples :: [(Int, Int, Int)]
pyTriples = [(a,b,c) |c <- [0..], b <- [0.. c], a <- [0.. b], (a < b) && (b < c), (a^2 + b^2) == c^2]

--pyTriplesDo :: [(Int, Int, Int)]

--pyTriplesBind :: [(Int, Int, Int)]


{-
Aufgabe 6.2 - Monadeninstanzen
-}

{-
Gegeben sei der Datentyp Baum für nicht leere binäre Bäume, ohne innere Knoten.
-}

data Baum a = Blatt a | Knoten (Baum a) (Baum a) deriving (Show, Eq)

{-
a)
Machen Sie Baum zu einer Instanz der Typklassen Functor, Applicative und Monad.
-}



{-
b)
Implementieren Sie die Faltung
foldBaum :: (a -> b) -> (b -> b -> b) -> Baum a -> b
für den Datentyp Baum.

Anmerkung: Wenn in einer Aufgabenstellung gefordert wird die Faltung für einen
Datentyp zu implementieren, dann ist immer das Schema von Übungsblatt 5
gemeint, sofern explizit nichts anderes in der Aufgabenstellung steht.
-}

--foldBaum :: (a -> b) -> (b -> b -> b) -> Baum a -> b


{-
Aufgabe 6.3 - Maybe & Either Monade
-}

{-
a)
Die Either-Monade kann als "optimierte" Maybe-Monade angesehen werden, wenn man im ersten
Typargument Fehlernachrichten abbildet.
So kann (Either String Float) bei der Implementierung von partiellen Funktionen im Gegensatz
zu (Maybe Float) in dem Sinne als Optimierung verstanden werden, dass
Left "Man darf nicht durch 0 teilen!"
Informationen über die nicht-definierte Eingabe enthalten kann. Im Gegensatz zu
Nothing.

Implementieren Sie eine Funktion
try :: (Show a, Show b) => Either a b -> String,
sodass Sie Either sinnvoll zum debuggen
nutzen könnten. Z.B.:
try (Left "Hier ging etwas schief!") ~> "FEHLER: Hier ging etwas schief!"
try (Right 5.5) ~> "5.5"

Implementieren Sie die Funktionen
logarithmus :: Float -> Either String Float
quadratwurzel :: Float -> Either String Float
kehrwert :: Float -> Either String Float
noch einmal mit sinnvollen Fehlermeldungen.

Implementieren Sie folgende Funktionen mittels (>>=):
f = logarithmus . kehrwert . quadratwurzel
g = quadratwurzel . logarithmus . kehrwert
(Die Komposition (.) ist hier natürlich nicht Typkorrekt, sondern soll nur "die Idee"
wiedergeben. Ansonsten bräuchte man ja auch kein (>>=) ;) ...) 
-}

--try :: (Show a, Show b) => Either a b -> String

--logarithmus :: Float -> Either String Float


--quadratwurzel :: Float -> Either String Float


--kehrwert :: Float -> Either String Float


--f :: Float -> Either String Float


--g :: Float -> Either String Float


{-
b)
Implementieren Sie eine Funktion
foo :: [Maybe a] -> Maybe [a],
die Nothing zurückgibt, wenn die Argumentliste ein Nothing enthält und sonst
alle Elemente der Liste aus dem Maybe "auspackt" und diese Liste wieder in ein
Maybe "einpackt".

Beispielaufrufe:
foo [Just 1, Just 4, Nothing, Just 3] ~> Nothing
foo [Just 1, Just 4, Just 3] ~> Just [1,4,3]
foo [] ~> Just []

Nutzen Sie zur Implementierung die do-Notation auf sinnvolle Weise.
-}

--foo :: [Maybe a] -> Maybe [a]


{-
Aufgabe 6.* - Kartesisches Produkt
-}

{-
Implementieren Sie eine Funktion
cartesianProduct :: [[a]] -> [[a]],
die das kartesische Produkt aller Elementlisten der Argumentliste berechnet.
Anstatt von n-Tupeln sollen aber Listen verwendet werden, um die Elemente des
n-stelligen kartesischen Produkts einfach abbilden zu können.

Beispielaufrufe:
cartesianProduct [] ~> [[]]
cartesianProduct [[1,2,3]] ~> [[1],[2],[3]]

cartesianProduct [[1,2,3], [4,5,6]] ~>
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]

cartesianProduct [[1,2], [3,4], [5,6]] ~>
[[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]

cartesianProduct [[1,2], [3,4], [5,6], []] ~>
[]

cartesianProduct [[1,2], [3,4], [5,6], [7]] ~>
[[1,3,5,7],[1,3,6,7],[1,4,5,7],[1,4,6,7],[2,3,5,7],[2,3,6,7],[2,4,5,7],[2,4,6,7]]
-}

--cartesianProduct :: [[a]] -> [[a]]






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
Wenn QuickCheck genutzt wird, muss die folgenden Instanz auskommentiert werden.
-}

{-
instance Arbitrary a => Arbitrary (Baum a) where
  arbitrary = sized arbTree where
    arbTree 0 = liftM Blatt arbitrary
    arbTree n = frequency [(1, liftM Blatt arbitrary), (4,  liftM2 Knoten (arbTree (n `div` 2)) (arbTree (n `div` 2)))]
-}

{-
Aufgabe 6.1 a)
-}

{-
prop_mapLK :: NonEmptyList String -> Fun String Int -> Bool
prop_mapLK (NonEmpty ls) (Fn f) = map f ls == mapLK f ls

test_mapLK = putStrLn "Test mapLK:" >> quickCheck prop_mapLK
-}

{-
prop_mapDo :: NonEmptyList String -> Fun String Int -> Bool
prop_mapDo (NonEmpty ls) (Fn f) = map f ls == mapDo f ls

test_mapDo = putStrLn "Test mapDo:" >> quickCheck prop_mapDo
-}

{-
prop_mapBind :: NonEmptyList String -> Fun String Int -> Bool
prop_mapBind (NonEmpty ls) (Fn f) = map f ls == mapBind f ls

test_mapBind = putStrLn "Test mapBind:" >> quickCheck prop_mapBind
-}

{-
Aufgabe 6.1 b)
-}

{-
prop_pyTriples (NonNegative n) (NonNegative m) = n > 10 ==>
  (take n (drop m pyTriples) == take n (drop m pyTriplesDo)) &&
  (take n (drop m pyTriples) == take n (drop m pyTriplesBind))

test_pyTriples = putStrLn "Test pyTriples:" >> quickCheck prop_pyTriples
-}

{-
Aufgabe 6.2 a)
-}

{-
prop_Functor_id b = fmap id b == b where
  types = b :: Baum Int

prop_Functor_composition :: Baum Int ->  Fun String [Bool] -> Fun Int String -> Bool
prop_Functor_composition b (Fn f) (Fn g) = fmap (f . g) b == (fmap f . fmap g) b

test_Functor = do
  putStrLn ""
  putStrLn ""
  putStrLn "Testen auf Funktor-Gesetze (Doberkat-Folie 183):"
  putStrLn ""
  putStrLn ""
  putStrLn "Test Functor identity law:"
  quickCheck prop_Functor_id
  putStrLn "Test Functor composition law:"
  quickCheck prop_Functor_composition
-}

{-
prop_Applicative_fmap :: Baum Int -> Fun Int String -> Bool
prop_Applicative_fmap b (Fn f) = (fmap f b) == (pure f <*> b)

prop_Applicative_id b = (pure id <*> b) == b where
  types = b :: Baum Int

prop_Applicative_hom :: Int -> Fun Int String -> Bool
prop_Applicative_hom n (Fn f) = (pure f <*> b) == pure (f n) where
  b :: Baum Int
  b = pure n

prop_Applicative_composition :: Baum Int ->  Fun String [Bool] -> Fun Int String -> Bool
prop_Applicative_composition b (Fn f) (Fn g) = (pure (.) <*> pure f <*> pure g <*> b) == (pure f <*> (pure g <*> b))

test_Applicative = do
  putStrLn ""
  putStrLn ""
  putStrLn "Testen auf Applicative-Gesetze (z.B. https://en.wikibooks.org/wiki/Haskell/Applicative_functors):"
  putStrLn ""
  putStrLn ""
  putStrLn "Test Applicative fmap:"
  quickCheck prop_Applicative_fmap
  putStrLn "Test Applicative id:"
  quickCheck prop_Applicative_id
  putStrLn "Test Applicative homomorphism:"
  quickCheck prop_Applicative_hom
  putStrLn "Test Applicative composition:"
  quickCheck prop_Applicative_composition
-}

{-
prop_Monad_left_id :: Int -> Fun Int (Baum Int) -> Bool
prop_Monad_left_id n (Fn f) = (return n >>= f) == f n

prop_Monad_right_id b = (b >>= return) == b where
  types = b :: Baum Int

prop_Monad_assoc :: Baum Int -> Fun Int (Baum String) -> Fun String (Baum [Bool]) -> Bool
prop_Monad_assoc b (Fn f) (Fn g) = ((b >>= f) >>= g) == (b >>= (\x -> f x >>= g))

test_Monad = do
  putStrLn ""
  putStrLn ""
  putStrLn "Testen auf Monaden-Gesetze (Doberkat-Folien 325 - 327):"
  putStrLn ""
  putStrLn ""
  putStrLn "Test Monad left identity:"
  quickCheck prop_Monad_left_id
  putStrLn "Test Monad right identity:"
  quickCheck prop_Monad_right_id
  putStrLn "Test Monad associativity:"
  quickCheck prop_Monad_assoc
-}

{-
Aufgabe 6.2 b)
-}

{-
prop_foldBaum b = foldBaum Blatt Knoten b == b where
  types = b :: Baum Int

test_foldBaum = putStrLn "Test foldBaum:" >> quickCheck prop_foldBaum
-}

{-
Aufgabe 6.3 a)
-}

{-
Für die Funktion try stellen wir keine 
prop_try
und
test_try
Implementierungen zur Verfügung, 
da die Aufgabenstellung Ihnen (bewusst) viele Freiheiten lässt.
-}

{-
prop_logarithmus (Positive n) = case (logarithmus $ negate n, logarithmus n) of
  (Left _, Right m) -> (m == log n) && isLeft (logarithmus 0)
  _ -> False

test_logarithmus = putStrLn "Test logarithmus:" >> quickCheck prop_logarithmus
-}

{-
prop_quadratwurzel (Positive n) = case (quadratwurzel $ negate n, quadratwurzel n) of
  (Left _, Right m) -> (m == sqrt n) && isRight (quadratwurzel 0)
  _ -> False

test_quadratwurzel = putStrLn "Test quadratwurzel:" >> quickCheck prop_quadratwurzel
-}

{-
prop_kehrwert (NonZero n) = case (kehrwert 0, kehrwert n) of
  (Left _, Right m) -> m == 1 / n
  _ -> False

test_kehrwert = putStrLn "Test kehrwert:" >> quickCheck prop_kehrwert
-}

{-
prop_f (Positive n) = case (f 0, f $ negate n, f n) of
  (Left _, Left _, Right m) -> m == log ((1/) $ sqrt n)
  _ -> False

test_f = putStrLn "Test f:" >> quickCheck prop_f
-}

{-
prop_g (Positive n) (Positive k) = n < 1 && k > 1 ==> case (g 0, g $ negate n, g $ k + n, g n) of
  (Left _, Left _, Left _, Right m) -> m == sqrt (log $ 1 / n)
  _ -> False

test_g = putStrLn "Test g:" >> quickCheck prop_g
-}

{-
Aufgabe 6.3 b)
-}

{-
prop_foo xs = foo xs == sequence xs where
  types = xs :: [Maybe String]

test_foo = putStrLn "Test foo:" >> quickCheck prop_foo
-}

{-
Aufgabe 6.*
-}

{-
prop_cartesianProduct (Small m) (Small n) = m < 7 && n < 7 ==> cartesianProduct xs == sequence xs where
  xs = replicate n [0..m]

test_cartesianProduct = putStrLn "Test cartesianProduct:" >> quickCheck prop_cartesianProduct
-}

{-
Wenn Sie alle Aufgaben gelöst und getestet haben, dann können Sie alle Tests mittels
test_all auf einmal ausführen.
-}
{-
test_all = do
  test_mapLK
  test_mapDo
  test_mapBind
  test_pyTriples
  test_Functor
  test_Applicative
  test_Monad
  test_foldBaum
  test_logarithmus
  test_quadratwurzel
  test_kehrwert
  test_f
  test_g
  test_foo
  test_cartesianProduct
-}