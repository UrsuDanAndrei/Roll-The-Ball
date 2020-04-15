{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState

import Data.Array as A

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
{-
data Cell = Cell
            { getPos :: Position
            , getC :: Char }
-}

data Cell = Cell
            { getC :: Char } deriving(Eq, Ord)


{-
    Tip de date pentru reprezentarea nivelului curent
-}

instance Show Cell where
    show (Cell c) = show c

data Level = Level (A.Array (Int, Int) Cell)
    deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}

instance Show Level where
    show (Level arr) = [if j == m+1 then '\n' else getC $ arr A.! (i, j) | i <- [0..n], j <- [0..m+1]] 
                    where
                        lrc = snd . A.bounds $ arr
                        n = fst lrc
                        m = snd lrc 

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel pos = Level (A.array ((0, 0), pos) [((i, j), Cell emptySpace) | i <- [0..(fst pos)], j <- [0..(snd pos)]])

-- Level [(Cell emptySpace) | x <- [0..(snd pos)]]

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat:
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! nu verifica daca e libera
addCell :: (Char, Position) -> Level -> Level
addCell (c, pos) (Level arr) = Level $ arr A.// [(pos, Cell c)]

{-
addCell (c, pos) lvl@(Level arr)
    | (getC $ arr A.! pos) == emptySpace = Level $ arr A.// [(pos, Cell c)]
    | otherwise = lvl
-}

{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel pos l = foldr addCell initLevel l
                where
                    initLevel = emptyLevel pos


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

-- !!! poate iese de pe tabla
moveCell :: Position -> Directions -> Level -> Level
moveCell pos@(i, j) dir lvl@(Level arr)
    | foldr (\c acc -> c == posChar || acc) False startWinCells = lvl 
    | otherwise = case dir of
                    North -> if isEmpty (i + 1, j) then addCell (emptySpace, pos) $ addCell (posChar, (i + 1, j)) lvl else lvl
                    South -> if isEmpty (i - 1, j) then addCell (emptySpace, pos) $ addCell (posChar, (i - 1, j)) lvl else lvl
                    West -> if isEmpty (i, j - 1) then addCell (emptySpace, pos) $ addCell (posChar, (i, j - 1)) lvl else lvl
                    East -> if isEmpty (i, j + 1) then addCell (emptySpace, pos) $ addCell (posChar, (i, j + 1)) lvl else lvl
    where
        posChar = (getC $ arr A.! pos)
        startWinCells = startCells ++ winningCells
        isEmpty poss = (getC $ arr A.! poss) == emptySpace
{-
    *** TODO ***

    Verifică dacă două celule se pot conecta.
    Atenție: Această funcție se aplică de la stânga la 
    dreapta(nu este comutativă).

    ex: connection botLeft horPipe = True (╚═)
        connection horPipe botLeft = False (═╚)
-}

-- !!! si se aplica de sus in jos

connection :: Cell -> Cell -> Bool
connection (Cell c1) (Cell c2) = undefined
{-
    case (c1, c2) of
    (horPipe, horPipe) -> True
    (horPipe, botRight) -> True
    (horPipe, topRight) -> True
    (horPipe, startLeft) -> True
    (horPipe, winLeft) -> True
    -}

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}
wonLevel :: Level -> Bool
wonLevel = undefined

instance ProblemState Level (Position, Directions) where
    successors = undefined
    isGoal = undefined
    reverseAction = undefined
