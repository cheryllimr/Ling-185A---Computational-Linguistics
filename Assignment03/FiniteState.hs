module FiniteState where

-- A type we'll use for the symbols of some FSAs. 
-- By ``deriving Eq'' we are linking the default/obvious 
-- equality function on this type to the name (==).
data SegmentCV = C | V deriving (Show, Eq)

-- This line defines `State' as a synonym for `Int', just for readability
type State = Int

-- This is a parametrized type synonym: it defines `Automaton a' as a 
-- type that we'll use to represent an FSA with symbols of type a. 
-- So `Automaton SegmentCV' is a type for FSAs with symbols of type SegmentCV, 
-- `Automaton Char' is a type for FSAs with symbols of type Char, etc.
type Automaton a = ([State], [a], [State], [State], [(State,a,State)])

-- Here's the FSA from (7) on the Intro FLT FSA handout
fsa_handout7 :: Automaton SegmentCV
fsa_handout7 = ([40,41,42,43], [C,V], [40], [43], [(40, C, 40),
                                                   (40, V, 40),
                                                   (40, C, 41),
                                                   (40, V, 42),
                                                   (41, C, 43),
                                                   (42, V, 43),
                                                   (43, C, 43),
                                                   (43, V, 43)])

-- Here's the FSA from (8) on the handout
fsa_handout8 :: Automaton SegmentCV
fsa_handout8 = ([1,2,3], [C,V], [1], [3], [(1, C, 1),
                                           (1, V, 1),
                                           (1, V, 2),
                                           (2, C, 3)])

-- Here's the FSA from (9) on the handout
fsa_handout9 :: Automaton SegmentCV
fsa_handout9 = ([1,2,3], [C,V], [1], [1], [(1, V, 1), (1, C, 2), (1, V, 3), 
                                           (2, V, 1), (2, V, 3), 
                                           (3, C, 1)])

-- Some existing functions that will be useful as helpers:
--      elem :: (Eq a) => a -> [a] -> Bool
--      map :: (a -> b) -> [a] -> [b]
--      sum :: [Int] -> Int          (+) :: Int -> Int -> Int
--      product :: [Int] -> Int      (*) :: Int -> Int -> Int
--      or :: [Bool] -> Bool         (||) :: Bool -> Bool -> Bool
--      and :: [Bool] -> Bool        (&&) :: Bool -> Bool -> Bool
-- The `sum', `product,' `or,' and `and' functions extend the two-at-a-time 
-- (+), (*), (||), and (&&) operations to act on lists. We can use them 
-- in combination with `map' to implement the kind of thing that's 
-- often expressed on paper with ``big sigma'' notation for summation 
-- ``big pi'' notation for products, etc.


-- This corresponds transparently to (24) on the handout. 
-- This function can only be used with automata whose symbol 
-- type has an equality function defined on it, because it 
-- calls the function `elem' which has this restriction. 
-- (The type `(Int,a,Int)' is a member of Eq iff `a' is.)
backward :: (Eq a) => Automaton a -> [a] -> State -> Bool
backward m w q = let (states,syms,i,f,delta) = m in
                 case w of
                 [] -> elem q f     -- F(q) on the handout
                 x:rest -> or (map (\q' -> elem (q,x,q') delta && backward m rest q') states)

-- This corresponds transparently to (21) on the handout. 
-- (And this function can only be used with symbol types 
-- that belong to the class Eq, because it calls `backward' 
-- which has this restriction.)
generates :: (Eq a) => Automaton a -> [a] -> Bool
generates m w = let (states,syms,i,f,delta) = m in
                or (map (\q0 -> elem q0 i && backward m w q0) states)



