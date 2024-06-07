module Uebung8 where

{-
Die Funktion guard aus Control.Monad ist leider nicht im Prelude, darf aber
in der Übung, im Midterm-Test und der Klausur vorausgesetzt werden,
sofern die konkrete Aufgabenstellung dies nicht ausschließt.
-}
import Control.Monad (guard)

{-
Die folgenden Importe sind nur für dieses Blatt gedacht und
nicht relevant für spätere Übungen, den Midterm-Test oder
die Klausur.
-}
import System.Random (randomIO)
import Data.Char (toUpper)


{-
Übung zur Vorlesung
Funktionale Programmierung
Sommersemester 2024
Übungsblatt 8
-}

{-
Ausgabe: 07.06.2024
Abgabe: keine
-}

{-
Aufgabe 8 - Hangman
-}


{-
Implementieren Sie das Spiel Hangman. Eine Beschreibung des Spiels finden Sie auf
folgender Webseite: http://de.wikipedia.org/wiki/Galgenmännchen.
Das Spiel soll interaktiv in der Konsole spielbar sein und selbstständig auf
Tastatureingaben vom Benutzer reagieren.
Beim Programmstart soll eine Datei mit den möglichen Wörtern eingelesen werden.

Sie können die Funktion randomIO :: IO a aus dem Modul System.Random nutzen,
um eine Zufallszahl zu erhalten, mit der Sie zufällig ein Wort aus der Datei auswählen.

Die Funktion toUpper :: Char -> Char aus dem Modul Data.Char steht Ihnen für diese Aufgabe 
ebenfalls zur Verfügung.

Sollten Sie weitere nützliche Funktionen importieren wollen, so steht Ihnen das auf diesem 
Blatt frei. Bedenken Sie jedoch, dass dies nicht für spätere Übungen, den Midterm-Test
oder die Klausur gilt.
-}

{-
Anmerkung: Das Paket System.Random ist abhängig vom System und der Installation
von Haskell manchmal nicht direkt verfügbar.
Diese Aufgabe ist also eine sehr "praxisnahe" Programmieraufgabe :)
Sollte System.Random bei Ihnen nicht direkt verfügbar sein, dann sehen Sie es bitte als
Teil der Aufgabe es auf Ihrem System entsprechend verfügbar zu machen.

Im Zweifelsfall kommentieren sie "import System.Random" aus und lösen Sie diese
Aufgabe einfach mit einem festen Wert.
-}

{-
Hinweis:
Anstatt einen Lösungsvorschlag von den Tutoren präsentieren/besprechen zu lassen,
wäre es toll, wenn Sie in der Übung Ihre eigene Implementierung vorstellen.
Sie haben mittlerweile alles notwendige gelernt, um ein tolles kleines
Spiel in Haskell zu implementieren. Nutzen Sie die Chance, um das
gelernte anzuwenden und anderen ihre Lösung zu präsentieren!
-}