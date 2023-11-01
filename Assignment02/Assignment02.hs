module Assignment02 where

-- Imports everything from the Recursion module. 
import Recursion

-- Imports just a few things that we have seen from the standard Prelude module. 
-- (If there is no explicit 'import Prelude' line, then the entire Prelude 
-- module is imported. I'm restricting things here to a very bare-bones system.)
import Prelude((+), (-), (*), (<), (>), (++), not, (||), (&&), Bool(..), Int, Show, Char, Eq)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

--Section 1
-- a) checked
mult :: Numb -> (Numb -> Numb)
mult n = \m -> case n of
    Z -> Z
    (S n') -> add m (mult n' m)

-- b) checked
sumUpTo :: Numb -> Numb
sumUpTo n = case n of
    Z -> Z 
    S n' -> add n (sumUpTo n')

-- c) checked
isEqual :: Numb -> (Numb -> Bool)
isEqual Z = \m -> case m of
    Z -> True
    (S m') -> False
isEqual (S n') = \m -> case m of
    Z -> False
    (S m') -> isEqual n' m'

-- d) checked
numbToInt :: Numb -> Int
numbToInt num = case num of
    Z -> 0
    S n -> 1 + numbToInt n

-- Section 2
-- a) checked
count :: (a -> Bool) -> ([a] -> Numb)
count p lst = case lst of
    [] -> Z  
    x:rest -> if p x
              then S (count p rest)  
              else count p rest  

-- b) checked
addToEnd :: a -> ([a] -> [a])
addToEnd x l = case l of
    [] -> [x]  
    y:ys -> y : addToEnd x ys  

-- c) checked
remove :: (a -> Bool) -> ([a] -> [a])
remove f l = case l of
    [] -> []  
    x:xs -> if f x
            then remove f xs  
            else x : remove f xs  

-- d) checked
prefix :: Numb -> [a] -> [a]
prefix n list = case n of
    Z -> []  
    (S n') -> case list of
        [] -> []  
        (x:xs) -> x : prefix n' xs  

-- e) checked
reverse :: [a] -> [a] 
reverse list = case list of
    [] -> []  
    (x:xs) -> reverse xs ++ [x]  

-- Section 3
-- a) checked
countStars :: RegExp a -> Numb
countStars re = case re of
    Lit _ -> Z  
    Alt r1 r2 -> add (countStars r1) (countStars r2)  
    Concat r1 r2 -> add (countStars r1) (countStars r2)  
    Star r -> S (countStars r)  
    ZeroRE -> Z  
    OneRE -> Z  

-- b) checked
depth :: RegExp a -> Numb
depth = \re -> case re of
    Lit _ -> S Z
    ZeroRE -> S Z
    OneRE -> S Z
    Alt r1 r2 -> S (bigger (depth r1) (depth r2))
    Concat r1 r2-> S (bigger (depth r1) (depth r2))
    Star r -> S (depth r)

-- c) checked
reToString :: RegExp Char -> [Char]
reToString re = case re of
    Lit c -> [c]  
    Alt r1 r2 -> "(" ++ reToString r1 ++ "|" ++ reToString r2 ++ ")"  
    Concat r1 r2 ->"(" ++ reToString r1 ++ "." ++ reToString r2 ++ ")"  
    Star r -> "" ++ reToString r ++ "*"  
    ZeroRE -> "0"  
    OneRE -> "1"   

-- Section 4
-- a) φ
-- b) F
-- c) φ
-- d) φ
-- e) r
-- f) 0
-- g) r
-- h) r
-- i)  I think that the parallels between formulas and regular expressions in
    -- these examples are in the concept of equivalence and simplification.
    -- By simplifying complex expressions and recognizing that certain operations
    -- have no effect on the overall result. I think that in the case of logical formulas
    -- (a, b, c, d), it can be simplified by recognizing that operations with certain constants
    -- (True, False) or duplicate expressions can be reduced to a simpler form while preserving
    -- the overall truth value. I think that in terms of cases with regular expressions
    -- (e, f, g, h), we simplify by recognizing that operations involving certain constants
    -- (empty set, language with an empty string) or duplicate expressions can be reduced to a
    -- simpler form while preserving the language defined by the regular expression. I believe
    -- that in both cases, the concept of equivalence allows us to simplify expressions without
    -- changing the meaning.
