module Uebung7 where

{-
Die Funktion guard aus Control.Monad ist leider nicht im Prelude, darf aber
in der Übung, im Midterm-Test und der Klausur vorausgesetzt werden,
sofern die konkrete Aufgabenstellung dies nicht ausschließt.
-}
import Control.Monad ( guard )

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
import Data.List (isInfixOf)
-}

{-
Übung zur Vorlesung
Funktionale Programmierung
Sommersemester 2024
Übungsblatt 7
-}

{-
Ausgabe: 31.05.2024
Abgabe: keine
-}

{-
Aufgabe 7.1 - Monaden anwenden
-}

{-
a)
Implementieren Sie eine Funktion
mapFi :: (a -> b) -> (b -> Bool) -> [a] -> [b],
die zuerst die übergebene Funktion über die Liste mapped und dann das Resultat
nach dem übergebenen Prädikat filtert.
Nutzen Sie zur Implementierung die (>>=)-Notation.
-}

--mapFi :: (a -> b) -> (b -> Bool) -> [a] -> [b]


{-
b)
Implementieren Sie die Funktion mapFi aus 7.1 a) erneut mittels Do-Notation.
(Bzw. übersetzen Sie die (>>=)-Notation in die Do-Notation.)
Nennen Sie die Funktion
mapFiDo :: (a -> b) -> (b -> Bool) -> [a] -> [b].
-}

--mapFiDo :: (a -> b) -> (b -> Bool) -> [a] -> [b]


{-
c)
Implementieren Sie eine Funktion
zipApp :: [a -> Maybe b] -> [a] -> Either String [b],
die eine Liste von Funktionen mit einer Liste von Argumenten "zipped" und
auftretende Fehler in der Either-Monade abhandelt.
Sollten die Listen nicht gleichlang sein, soll entweder der Fehler
"Linke Liste zu kurz."
oder
"Rechte Liste zu kurz."
zurückgegeben werden.
Sollte bei einer Funktionsapplikation Nothing als Ergebnis
herauskommen, soll der Fehler
"Nothing bei Funktionsanwendung."
zurückgegeben werden.
Ansonsten sollen die Funktionen aus der ersten Liste jeweils auf das Element
mit demselben Index in der zweiten Liste angewendet werden und das Ergebnis
in der Either-Monade, statt der Maybe-Monade zurückgegeben werden.
-}

--zipApp :: [a -> Maybe b] -> [a] -> Either String [b]


{-
Aufgabe 7.2 - Eine funktionale Bank
-}

newtype State s a = State {runS :: s -> (a, s)}

{-
Auf den Folien gegeben:
-}

instance Monad (State s) where
  return a = State (\s -> (a, s))
  State h >>= f = State ((\(a,s) -> runS (f a) s) . h)

instance Functor (State s) where
  fmap f (State h) = State ((\(x,s) -> (f x, s)) . h)

{-
Die Definitionen der Leser- und Schreibermonade finden sich in
FuPro_2024_VL8.pdf
-}

type Writer s a = (s, a)

type Reader s a = s -> a

{-
Fehlende Instanz auf Basis der gegebenen Definition:
-}

instance Applicative (State s) where
  pure a = State (\s -> (a, s))
  --zf <*> za = zf >>= \f -> za >>= \a -> return (f a)
  State f <*> h = State ((\(g,s) -> let (x, s') = runS h s in (g x, s')) . f)

{-
Gegeben seien die folgenden Datentypen zur Modellierung von (Bank-)Konten und
(bankinternen) Überweisungen zwischen Kontoinhabern.
-}
type ID = Int

type Bank = [(ID,Account)]

data Account = Account { balance :: Int, owner :: Client } deriving (Show, Eq)

data Client = Client
  { name :: String
  , surname :: String
  , address :: String
  } deriving (Show, Eq)

own1, own2, own3 :: Client
own1 = Client "Max" "Mustermann" "Musterhausen"
own2 = Client "John" "Doe" "Somewhere"
own3 = Client "Erika" "Mustermann" "Musterhausen"

acc1, acc2, acc3 :: Account
acc1 = Account 100 own1
acc2 = Account 0 own2
acc3 = Account 50 own3

bank1 :: Bank
bank1 = [(1,acc1), (2,acc2), (3,acc3)]

updRel :: Eq a => [(a,b)] -> a -> b -> [(a,b)]
updRel ((a,b):r) c d = if a == c then (a,d):r else (a,b):updRel r c d
updRel _ a b = [(a,b)]

putAccount :: Bank ->  ID -> Account -> Bank
putAccount bank id acc = updRel bank id acc

getAccount :: Bank -> ID -> Maybe Account
getAccount bank id = lookup id bank

credit :: Int -> ID -> Bank -> Bank
credit n i b = putAccount b i entry{ balance = oldBalance + n } where
  Just entry = getAccount b i
  oldBalance = balance entry

debit :: Int -> ID -> Bank -> Bank
debit n = credit (-n)

transfer :: Int -> ID -> ID -> Bank -> Bank
transfer amount id1 id2 bank = let b = credit amount id2 bank in debit amount id1 b

{-
Wenn wir uns die Typen von
putAccount :: Bank ->  ID -> Account -> Bank,
getAccount :: Bank -> ID ->  Maybe Account,
credit :: Int -> ID -> Bank -> Bank,
debit :: Int -> ID -> Bank -> Bank
und
transfer :: Int -> ID -> ID -> Bank -> Bank
anschauen, dann stellen wir fest, dass alle Funktion auf derselben
"Umgebung" aus ID und Bank arbeiten.
Sprich alle Funktionen haben mindestens ein Argument vom Typ ID und vom Typ Bank.

Das liegt daran, dass diese Funktionen immer in Bezug zu einem Konto
auf einer Bank arbeiten.

Die Lesermonade ermöglicht es uns Funktionen "in" dieser Umgebung zu
definieren, sprich die Umgebung als implizit gegeben zu behandeln.

Hierzu führen wir einen Typen für unsere Umgebung aus Kontonummer und Bank ein:
-}

data Environment = Env {
                        idB :: ID,
                        bank :: Bank
                       } deriving (Show, Eq)

{-
a)
Nutzen Sie die Lesermonade (Reader Environment) um die Funktionen
putAccountR :: Account -> Reader Environment Bank,
getAccountR :: Reader Environment (Maybe Account),
creditR  :: Int -> Reader Environment Bank,
debitR :: Int -> Reader Environment Bank
und
transferR :: Int -> ID -> Reader Environment Bank
analog zu den zuvor definierten Funktionen zu implementieren.

Verwenden Sie die Do-Notation.
-}

--putAccountR :: Account -> Reader Environment Bank

--getAccountR :: Reader Environment (Maybe Account)

--creditR  :: Int -> Reader Environment Bank

--debitR :: Int -> Reader Environment Bank

--transferR :: Int -> ID -> Reader Environment Bank


{-
Sie können transferR mittels der folgenden Funktion
transactions :: Bank -> Writer String Bank
testen.
Folgender Beispielaufruf sollte mit ihrer Implementierung
übereinstimmen:
transactions bank1
~>
[
(1,Account {balance = 25, owner = Client {name = "Max", surname = "Mustermann", address = "Musterhausen"}}),
(2,Account {balance = 25, owner = Client {name = "John", surname = "Doe", address = "Somewhere"}}),
(3,Account {balance = 100, owner = Client {name = "Erika", surname = "Mustermann", address = "Musterhausen"}})
]
-}

{-
transactions bank = transferR 25 2 $ Env 3 $ transferR 25 1 $ Env 3 $ transferR 50 1 $ Env 2 bank
-}

{-
b)
Nutzen Sie die Schreibermonade (Writer String) um eine Funktion
transferLog :: Int -> ID -> ID -> Bank -> Writer String Bank
zu implementieren, die den angegebenen Betrag vom ersten Konto auf das zweite überweist
und einen Eintrag in das Protokoll schreibt. Der Eintrag soll folgendes Format besitzen:

"Der Betrag <amount> wurde von Konto <id1> auf Konto <id2> übertragen."

Verwenden Sie die do-Notation. Der Tupelkonstruktor (,) soll nicht benutzt werden!

Tipp: Es lohnt sich eine Loggerfunktion
logMsg :: String -> (String,())
zu implementieren, welche einen beliebigen String an das Protokoll anhängt.
(Für logMsg kann natürlich der Tupelkonstruktor (,) anstatt der Do-Notation
benutzt werden.)
-}

--transferLog :: Int -> ID -> ID -> Bank -> Writer String Bank


{-
Sie können transferLog mittels der folgenden Funktion
transactionsLog :: Bank -> Writer String Bank
testen.
Folgende Beispielaufrufe sollte mit ihrer Implementierung
übereinstimmen:
putStrLn $ fst $ transactionsLog bank1
~>
Der Betrag 50 wurde von Konto 1 auf Konto 2 übertragen.
Der Betrag 25 wurde von Konto 1 auf Konto 3 übertragen.
Der Betrag 25 wurde von Konto 2 auf Konto 3 übertragen.

snd $ transactionsLog bank1
~>
[
(1,Account {balance = 25, owner = Client {name = "Max", surname = "Mustermann", address = "Musterhausen"}}),
(2,Account {balance = 25, owner = Client {name = "John", surname = "Doe", address = "Somewhere"}}),
(3,Account {balance = 100, owner = Client {name = "Erika", surname = "Mustermann", address = "Musterhausen"}})
]
-}

{-
transactionsLog :: Bank -> Writer String Bank
transactionsLog bank = do
  bank2 <- transferLog 50 1 2 bank
  bank3 <- transferLog 25 1 3 bank2
  transferLog 25 2 3 bank3
-}

{-
c)
Nutzen Sie die Zustandsmonade (State Bank) um die Funktionen
putAccountS :: ID -> Account -> State Bank (),
getAccountS :: ID -> State Bank (Maybe Account)
creditS :: Int -> ID -> State Bank (),
debitS :: Int -> ID -> State Bank ()
und
transferS :: Int -> ID -> ID -> State Bank ()
analog zu den zuvor definierten Funktionen zu implementieren.

putAccountS und getAccountS lassen sich am einfachsten über den Konstruktor
der Zustandsmonade definieren.
Für alle anderen Funktionen sollen Sie zur Implementierung die Do-Notation nutzen.
-}

--putAccountS :: ID -> Account -> State Bank ()

--getAccountS :: ID -> State Bank (Maybe Account)

--creditS :: Int -> ID -> State Bank ()

--debitS :: Int -> ID -> State Bank ()

--transferS :: Int -> ID -> ID -> State Bank ()


{-
Folgender Beispielaufruf kann zum Verständnis der Aufgabe und Überprüfen ihrer
Lösungen hilfreich sein:

 fmap (fmap balance)$ snd $ runS transactionsS bank1
~>
[(1,25),(2,25),(3,100)]

-}

{-
transactionsS = do
  transferS 50 1 2
  transferS 25 1 3
  transferS 25 2 3
-}

{-
Aufgabe 7.* - reverse
-}

{-
Implementieren Sie die Funktion
reverse' :: [a] -> [a],
die eine gegebene Liste "umdreht" mittels einer Listenfaltung.
-}

--reverse' :: [a] -> [a]






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
instance Arbitrary Client where
   arbitrary = Client <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Account where
   arbitrary = Account <$> arbitrary <*> arbitrary 
-}

{-
Aufgabe 7.1 a)
-}

{-
prop_mapFi :: Fun String Int -> Fun Int Bool -> [String] -> Bool
prop_mapFi (Fn f) (Fn p) xs = mapFi f p xs == filter p (map f xs)

test_mapFi = putStrLn "Test mapFi:" >> quickCheck prop_mapFi
-}

{-
Aufgabe 7.1 b)
-}

{-
prop_mapFiDo :: Fun String Int -> Fun Int Bool -> [String] -> Bool
prop_mapFiDo (Fn f) (Fn p) xs = mapFiDo f p xs == filter p (map f xs)

test_mapFiDo = putStrLn "Test mapFiDo:" >> quickCheck prop_mapFiDo
-}

{-
Aufgabe 7.1 c)
-}

{-
prop_zipApp :: NonEmptyList (Fun String (Maybe Int)) -> NonEmptyList String -> Bool
prop_zipApp (NonEmpty gs) (NonEmpty xs) = let fs = (map applyFun gs) in case zipApp fs xs of
  Left _ -> (length fs /= length xs) || (or $ zipWith (\g x -> case g x of {Nothing -> True; _ -> False}) fs xs)
  Right l -> and $ zipWith (\a b -> case b of {Nothing -> False; Just b' -> a == b'}) l $ zipWith (\g x -> g x) fs xs

test_zipApp = putStrLn "Test zipApp:" >> quickCheck prop_zipApp
-}

{-
Aufgabe 7.2 a)
-}

{-
prop_putAccountR bank id account = (putAccountR account (Env id bank)) == (putAccount bank id account)

test_putAccountR = putStrLn "Test putAccountR:" >> quickCheck prop_putAccountR
-}

{-
prop_getAccountR bank id = getAccountR (Env id bank) == getAccount bank id

test_getAccountR = putStrLn "Test getAccountR:" >> quickCheck prop_getAccountR
-}

{-
prop_creditR n (NonEmpty bank) = all (\(id, _) -> creditR n (Env id bank) == credit n id bank) bank

test_creditR = putStrLn "Test creditR:" >> quickCheck prop_creditR
-}

{-
prop_debitR n (NonEmpty bank) = all (\(id, _) -> debitR n (Env id bank) == debit n id bank) bank

test_debitR = putStrLn "Test debitR:" >> quickCheck prop_debitR
-}

{-
prop_transferR n (NonEmpty bank) = all (\(id1, id2) -> transferR n id1 (Env id2 bank) == transfer n id1 id2 bank) [(a,b) | (a, _) <- bank, (b, _) <- bank]

test_transferR = putStrLn "Test transferR:" >> quickCheck prop_transferR
-}

{-
Aufgabe 7.2 b)
-}

{-
prop_transferLog n (NonEmpty bank) = all 
  (\(id1, id2) -> let (s, b) = transferLog n id1 id2 bank in 
    b == transfer n id1 id2 bank && 
    isInfixOf "Der Betrag" s &&
    isInfixOf "wurde von Konto" s &&
    isInfixOf (show n) s 
    ) 
  [(a,b) | (a, _) <- bank, (b, _) <- bank]

test_transferLog = putStrLn "Test transferLog:" >> quickCheck prop_transferLog
-}

{-
Aufgabe 7.2 c)
-}

{-
prop_putAccountS bank id account = snd (runS (putAccountS id account) bank) == putAccount bank id account

test_putAccountS = putStrLn "Test putAccountS:" >> quickCheck prop_putAccountS
-}

{-
prop_getAccountS bank id = fst (runS (getAccountS id) bank) == getAccount bank id

test_getAccountS = putStrLn "Test getAccountS:" >> quickCheck prop_getAccountS
-}

{-
prop_creditS n (NonEmpty bank) = all (\(id, _) -> snd (runS (creditS n id) bank) == credit n id bank) bank

test_creditS = putStrLn "Test creditS:" >> quickCheck prop_creditS
-}

{-
prop_debitS n (NonEmpty bank) = all (\(id, _) -> snd (runS (debitS n id) bank) == debit n id bank) bank

test_debitS = putStrLn "Test debitS:" >> quickCheck prop_debitS
-}

{-
prop_transferS n (NonEmpty bank) = all (\(id1, id2) -> snd (runS (transferS n id1 id2) bank)  == transfer n id1 id2 bank) [(a,b) | (a, _) <- bank, (b, _) <- bank]

test_transferS = putStrLn "Test transferS:" >> quickCheck prop_transferS
-}

{-
Aufgabe 7.*
-}

{-
prop_reverse' xs = reverse xs == reverse' xs where
  types = xs :: [Int]

test_reverse' = putStrLn "Test reverse':" >> quickCheck prop_reverse'
-}

{-
Wenn Sie alle Aufgaben gelöst und getestet haben, dann können Sie alle Tests mittels
test_all auf einmal ausführen.
-}

{-
test_all = do
  test_mapFi
  test_mapFiDo
  test_zipApp
  test_putAccountR
  test_getAccountR
  test_creditR
  test_debitR
  test_transferR
  test_transferLog
  test_putAccountS
  test_getAccountS
  test_creditS
  test_debitS
  test_transferS
  test_reverse'
-}