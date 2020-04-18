{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState

{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = Node s (Maybe a) (Maybe (Node s a)) Int [Node s a]
    deriving (Show, Eq)
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!11111 daca pun asta iau 57 de puncte aparent o folosesc undeva in bfs
{-
instance (Eq s) => Eq (Node s a) where
    (==) (Node state1 _ _ _ _) (Node state2 _ _ _ _) = state1 == state2
    -}

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}


-- !!! paote introduci modulul RollTheBall aici si schimbi conditia de maybe in dir == NoDir 
nodeState :: Node s a -> s
nodeState (Node state _ _ _ _) = state

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (Node _ _ dad _ _) = dad
 {- | d == 0 = Nothing
    | otherwise = Just dad
    -}

nodeDepth :: Node s a -> Int
nodeDepth (Node _ _ _ d _) = d

nodeAction :: Node s a -> Maybe a
nodeAction (Node _ action _ _ _) = action
 {- | d == 0 = Nothing
    | otherwise = Just action
    -}

nodeChildren :: Node s a -> [Node s a]
nodeChildren (Node _ _ _ _ chil) = chil

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1111 s-ar putea ca getChildren sa trebuiasca plasat
-- inafara lui createStateSpace            

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace state = initialNode
    where
        initialNode = Node state Nothing Nothing 0 (createChildren initialNode)
        createChildren currNode = map childFromActionState (successors (nodeState currNode))
            where
                d = nodeDepth currNode
                childFromActionState (nextAction, nextState) = nextChild
                    where
                        nextChild = Node nextState (Just nextAction) (Just currNode) (d+1) (createChildren nextChild)

{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}

bfsHelper :: (Eq a, Ord s) => [Node s a] -> [Node s a] -> [([Node s a], [Node s a])]
bfsHelper inQueue allNodes = (nextInQueue, (tail inQueue) ++ nextInQueue):(bfsHelper ((tail inQueue) ++ nextInQueue) (allNodes ++ nextInQueue))
    where
        node = head inQueue
        nextInQueue = filter (not . (\child -> elem child allNodes)) (nodeChildren node)

bfs :: (Eq a, Ord s) => Node s a -> [([Node s a], [Node s a])]
bfs node = ([node], [node]):(bfsHelper [node] [node])

{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}
{-
bidirBFSHelper :: Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFSHelper = undefined
-}

bidirBFS :: Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS start finish = undefined

{-
    where
        union = head $ filter contact zipped
        zipped = zip (bfs start) (bfs finish)
        contact ((lastS, frontS), (lastF, frontF)) = filter (\x -> (isInFront x frontF) || (childrenAreInFront x frontF)) False lastS
        isInFront y front = foldr (\z acc -> ((nodeState y) == (nodeState z)) || acc) False front
        childrenAreInFront y front = foldr (\z acc -> (isInFront z front) || acc) False (nodeChildren y)
-}

{-
        startRecent = map fst bfsStart
        finishRecent = map fst bfsFinish
        startFront = map snd bfsStart
        finishFront = map snd bfsFinish
        -}


{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

extractPath :: Node s a -> [(Maybe a, s)]
extractPath = undefined



{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve = undefined
