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

data Directions = North | South | West | East | NoDir
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}

data Cell = Cell { getC :: Char }
    deriving(Eq, Ord)

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
    show (Level arr) = '\n':[if j == m+1 then '\n' else getC $ arr A.! (i, j) | i <- [0..n], j <- [0..m+1]] 
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
emptyLevel pos = Level $ A.array ((0, 0), pos) [((i, j), Cell emptySpace) | i <- [0..fst pos], j <- [0..snd pos]]

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

addCell :: (Char, Position) -> Level -> Level
addCell (c, pos@(i, j)) lvl@(Level arr)
    | (i >= 0 && i <= n && j >= 0 && j <= m) = Level $ arr A.// [(pos, Cell c)]
    | otherwise = lvl
    where
        lrc = snd . A.bounds $ arr
        n = fst lrc
        m = snd lrc

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
createLevel pos cells = foldr addCell (emptyLevel pos) cells

{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

moveCell :: Position -> Directions -> Level -> Level
moveCell pos@(i, j) dir lvl@(Level arr)
    | dir == North && i == 0 = lvl
    | dir == South && i == n = lvl
    | dir == East && j == m = lvl
    | dir == West && j == 0 = lvl
    | foldr (\c acc -> c == posChar || acc) False startWinCells = lvl 
    | otherwise = case dir of
                    North -> if isEmpty (i - 1, j) then addCell (emptySpace, pos) $ addCell (posChar, (i - 1, j)) lvl else lvl
                    South -> if isEmpty (i + 1, j) then addCell (emptySpace, pos) $ addCell (posChar, (i + 1, j)) lvl else lvl
                    West -> if isEmpty (i, j - 1) then addCell (emptySpace, pos) $ addCell (posChar, (i, j - 1)) lvl else lvl
                    East -> if isEmpty (i, j + 1) then addCell (emptySpace, pos) $ addCell (posChar, (i, j + 1)) lvl else lvl
                    NoDir -> lvl
    where
        lrc = snd . A.bounds $ arr
        n = fst lrc
        m = snd lrc

        posChar = getC $ arr A.! pos
        startWinCells = startCells ++ winningCells
        isEmpty nextPos = (getC $ arr A.! nextPos) == emptySpace

{-
    *** TODO ***

    Verifică dacă două celule se pot conecta.
    Atenție: Această funcție se aplică de la stânga la 
    dreapta(nu este comutativă).

    ex: connection botLeft horPipe = True (╚═)
        connection horPipe botLeft = False (═╚)
-}

connection :: Cell -> Cell -> Directions -> Bool 
connection (Cell c1) (Cell c2) dir
    | c1 == horPipe && c2 == horPipe && dir == East = True
    | c1 == horPipe && c2 == botRight && dir == East = True
    | c1 == horPipe && c2 == topRight && dir == East = True
    | c1 == horPipe && c2 == startLeft && dir == East = True
    | c1 == horPipe && c2 == winLeft && dir == East = True
    | c1 == horPipe && c2 == horPipe && dir == West = True
    | c1 == horPipe && c2 == botLeft && dir == West = True
    | c1 == horPipe && c2 == topLeft && dir == West = True
    | c1 == horPipe && c2 == startRight && dir == West = True
    | c1 == horPipe && c2 == winRight && dir == West = True
    | c1 == verPipe && c2 == verPipe && dir == North = True
    | c1 == verPipe && c2 == topLeft && dir == North = True
    | c1 == verPipe && c2 == topRight && dir == North = True
    | c1 == verPipe && c2 == startDown && dir == North = True
    | c1 == verPipe && c2 == winDown && dir == North = True
    | c1 == verPipe && c2 == verPipe && dir == South = True
    | c1 == verPipe && c2 == botLeft && dir == South = True
    | c1 == verPipe && c2 == botRight && dir == South = True
    | c1 == verPipe && c2 == startUp && dir == South = True
    | c1 == verPipe && c2 == winUp && dir == South = True
    | c1 == topLeft && c2 == horPipe && dir == East = True
    | c1 == topLeft && c2 == botRight && dir == East = True
    | c1 == topLeft && c2 == topRight && dir == East = True
    | c1 == topLeft && c2 == startLeft && dir == East = True
    | c1 == topLeft && c2 == winLeft && dir == East = True
    | c1 == topLeft && c2 == verPipe && dir == South = True
    | c1 == topLeft && c2 == botLeft && dir == South = True
    | c1 == topLeft && c2 == botRight && dir == South = True
    | c1 == topLeft && c2 == startUp && dir == South = True
    | c1 == topLeft && c2 == winUp && dir == South = True
    | c1 == botLeft && c2 == horPipe && dir == East = True
    | c1 == botLeft && c2 == botRight && dir == East = True
    | c1 == botLeft && c2 == topRight && dir == East = True
    | c1 == botLeft && c2 == startLeft && dir == East = True
    | c1 == botLeft && c2 == winLeft && dir == East = True
    | c1 == botLeft && c2 == horPipe && dir == North = True
    | c1 == botLeft && c2 == topLeft && dir == North = True
    | c1 == botLeft && c2 == topRight && dir == North = True
    | c1 == botLeft && c2 == startDown && dir == North = True
    | c1 == botLeft && c2 == winDown && dir == North = True
    | c1 == botRight && c2 == horPipe && dir == West = True
    | c1 == botRight && c2 == topLeft && dir == West = True
    | c1 == botRight && c2 == botLeft && dir == West = True
    | c1 == botRight && c2 == startRight && dir == West = True
    | c1 == botRight && c2 == winRight && dir == West = True
    | c1 == botRight && c2 == verPipe && dir == North = True
    | c1 == botRight && c2 == topLeft && dir == North = True
    | c1 == botRight && c2 == topRight && dir == North = True
    | c1 == botRight && c2 == startDown && dir == North = True
    | c1 == botRight && c2 == winDown && dir == North = True
    | c1 == topRight && c2 == horPipe && dir == West = True
    | c1 == topRight && c2 == topLeft && dir == West = True
    | c1 == topRight && c2 == botLeft && dir == West = True
    | c1 == topRight && c2 == startRight && dir == West = True
    | c1 == topRight && c2 == winRight && dir == West = True
    | c1 == topRight && c2 == verPipe && dir == South = True
    | c1 == topRight && c2 == botLeft && dir == South = True
    | c1 == topRight && c2 == botRight && dir == South = True
    | c1 == topRight && c2 == startUp && dir == South = True
    | c1 == topRight && c2 == winUp && dir == South = True
    | c1 == startUp && c2 == verPipe && dir == North = True
    | c1 == startUp && c2 == topLeft && dir == North = True
    | c1 == startUp && c2 == topRight && dir == North = True
    | c1 == startUp && c2 == winDown && dir == North = True
    | c1 == startDown && c2 == verPipe && dir == South = True
    | c1 == startDown && c2 == botLeft && dir == South = True
    | c1 == startDown && c2 == botRight && dir == South = True
    | c1 == startDown && c2 == winUp && dir == South = True
    | c1 == startLeft && c2 == horPipe && dir == West = True
    | c1 == startLeft && c2 == topLeft && dir == West = True
    | c1 == startLeft && c2 == botLeft && dir == West = True
    | c1 == startLeft && c2 == winRight && dir == West = True
    | c1 == startRight && c2 == horPipe && dir == East = True
    | c1 == startRight && c2 == botRight && dir == East = True
    | c1 == startRight && c2 == topRight && dir == East = True
    | c1 == startRight && c2 == winLeft && dir == East = True
    | c1 == winUp && c2 == verPipe && dir == North = True
    | c1 == winUp && c2 == topLeft && dir == North = True
    | c1 == winUp && c2 == topRight && dir == North = True
    | c1 == winUp && c2 == startDown && dir == North = True
    | c1 == winDown && c2 == verPipe && dir == South = True
    | c1 == winDown && c2 == botLeft && dir == South = True
    | c1 == winDown && c2 == botRight && dir == South = True
    | c1 == winDown && c2 == startUp && dir == South = True
    | c1 == winLeft && c2 == horPipe && dir == West = True
    | c1 == winLeft && c2 == topLeft && dir == West = True
    | c1 == winLeft && c2 == botLeft && dir == West = True
    | c1 == winLeft && c2 == startRight && dir == West = True
    | c1 == winRight && c2 == horPipe && dir == East = True
    | c1 == winRight && c2 == botRight && dir == East = True
    | c1 == winRight && c2 == topRight && dir == East = True
    | c1 == winRight && c2 == startLeft && dir == East = True
    | otherwise = False

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}

wonLevel :: Level -> Bool
wonLevel (Level arr) = pathEnd NoDir $ getStart arr (0, 0)
    where
        lrc = snd . A.bounds $ arr
        n = fst lrc
        m = snd lrc

        isStart (Cell x) = elem x startCells
        isWin (Cell x) = elem x winningCells

        posConnection pos1 pos2 dir = connection (arr A.! pos1) (arr A.! pos2) dir
        getStart localArr pos@(i, j)
            | isStart $ localArr A.! pos = (i, j)
            | j == m = getStart localArr (i+1, 0)
            | otherwise = getStart localArr (i, j+1)

        pathEnd notDir pos@(i, j)
            | isWin $ arr A.! pos = True
            | notDir /= North && i /= 0 && posConnection (i, j) (i-1, j) North = pathEnd South (i-1, j)
            | notDir /= South && i /= n && posConnection (i, j) (i+1, j) South = pathEnd North (i+1, j)
            | notDir /= West && j /= 0 && posConnection (i, j) (i, j-1) West = pathEnd East (i, j-1)
            | notDir /= East && j /= m && posConnection (i, j) (i, j+1) East = pathEnd West (i, j+1)
            | otherwise = False

instance ProblemState Level (Position, Directions) where
    successors lvl@(Level arr) = filter ((/= lvl) . snd) allStates
        where
            lrc = snd . A.bounds $ arr
            n = fst lrc
            m = snd lrc
            allStates = [(((i, j), dir), moveCell (i, j) dir lvl) | i <- [0..n], j <- [0..m], dir <- [North, South, West, East]]

    isGoal = wonLevel

    reverseAction (((i, j), dir), lvl)
        | dir == North = (((i-1, j), South), moveCell (i-1, j) South lvl)
        | dir == South = (((i+1, j), North), moveCell (i+1, j) North lvl)
        | dir == East = (((i, j+1), West), moveCell (i, j+1) West lvl)
        | dir == West = (((i, j-1), East), moveCell (i, j-1) East lvl)
