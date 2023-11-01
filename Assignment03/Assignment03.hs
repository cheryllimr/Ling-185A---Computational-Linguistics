module Assignment03 where

-- Imports everything from the FiniteState module
import FiniteState

-- A type we'll use as symbols for some FSAs
data SegmentPKIU = P | K | I | U | MB deriving (Show,Eq)

-- A list-like type that will be useful for computing forward values
data SnocList a = ESL | (SnocList a) ::: a deriving Show

-- The word ``hello'' encoded as a snoc list of characters
sl :: SnocList Char
sl = ((((ESL ::: 'h') ::: 'e') ::: 'l') ::: 'l') ::: 'o'

-- Checks that all states and symbols mentioned in the transition 
-- table (i.e. delta) come from the provided lists of states and symbols.
fsaSanityCheck :: (Eq a) => Automaton a -> Bool
fsaSanityCheck m =
    let (states, syms, i, f, delta) = m in
    let validTransition (q1,x,q2) = elem q1 states && elem x syms && elem q2 states in
    and (map validTransition delta)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

-- Section 1 checked
-- A)
fsa_countVs :: Automaton SegmentCV
fsa_countVs = ([0, 1, 2, 3], [C, V], [0], [3], [(0, C, 0), (0, V, 1), (1, C, 1), 
            (1, V, 2), (2, C, 2), (2, V, 0), (2, V, 3), (3, C, 3), (3, C, 3)])

-- Section 2 checked
-- B)
addToFront :: a -> SnocList a -> SnocList a
addToFront x ESL = ESL ::: x
addToFront x (rest ::: y) = (addToFront x rest) ::: y

-- C)
toSnoc :: [a] -> SnocList a
toSnoc [] = ESL
toSnoc (x:xs) = addToFront x (toSnoc xs)

-- D)
forward :: (Eq a) => Automaton a -> SnocList a -> State -> Bool
forward m w q =
    let (states, syms, i, f, delta) = m in
        case w of
            ESL -> elem q i
            rest ::: x -> or [elem (q0, x, q) delta && forward m rest q0 | q0 <- states]

--Section 3 checked
-- E)
fsa_twoVs :: Automaton SegmentCV
fsa_twoVs = ([0, 1, 2, 3], [C, V], [0], [3],
    [(0, C, 0), (0, V, 1), (1, C, 1), (1, V, 2), (2, V, 3), (2, C, 3), (3, V, 3)])

-- F)
fsa_thirdlastC :: Automaton SegmentCV
fsa_thirdlastC = ([0, 1, 2, 3, 4], [C, V], [0], [4],
    [(0, C, 1), (1, C, 2), (2, C, 3), (2, V, 4), (3, V, 3), (3, C, 4), (4, C, 4)])

-- G)
fsa_oddEven :: Automaton SegmentCV
fsa_oddEven = ([0, 1], [C, V], [0], [1], [(0, C, 1), (0, V, 0),
    (1, C, 0), (1, V, 1)])

-- H)
fsa_harmony :: Automaton SegmentPKIU
fsa_harmony = ([0, 1, 2, 3], [P, K, I, U, MB], [0], [3], [(0, P, 0), (0, K, 0), (0, MB, 0),
 (0, I, 1), (0, U, 2), (1, P, 1), (1, K, 1), (1, I, 1), (1, MB, 1), (1, MB, 2), (2, U, 2), (2, MB, 2),
  (2, K, 2), (2, P, 2), (2, MB, 1), (2, P, 3), (2, K, 3), (2, U, 3), (2, MB, 3), (1, P, 3), (1, K, 3),
   (1, I, 3), (1, MB, 3), (0, P, 3), (0, K, 3), (0, I, 3), (0, U, 3), (0, MB, 3)])


-- I)
fsa_MBU :: Automaton SegmentPKIU
fsa_MBU = ([0, 1, 2], [P, K, I, U, MB], [0], [2], [(0, K, 0), (0, I, 0),
    (0, MB, 0), (0,P,0),(0,MB,1),(1,U,1),(1,P,1),(1,K,1),(1,I,1),(1,MB,1),(1,U,2),
    (1,P,2),(1,K,2),(1,I,2),(1,MB,2),(0,P,2),(0,MB,2),(0,I,2),(0,K,2)])

-- Section 4 checked
-- J)
requireVs :: Int -> Automaton SegmentCV
requireVs n = 
        let states = [0 .. n] in
        let syms = [C,V] in
        let i = [0] in
        let f = [n] in
        let ctransitions = [(s, C, s) | s <- states] in
        let vtransitions = if n == 0 then [] else [(s, V, s+1) | s <- [0 .. n-1]] in
        (states, syms, i, f, ctransitions ++ vtransitions)