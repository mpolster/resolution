-- Implementation of the resolution algorithm in propositional logic
module Resolution (Literal (..), resX, resN) where
-- needed imports
import Data.List (union, sort)

-- Fundamental Data Structures and type aliases
data Literal = Literal {neg :: Bool, name :: String}
type Clause = [Literal]
type ClauseSet = [Clause]

instance Show Literal where
    show (Literal a b) = if a then b else "¬" ++ b

instance Eq Literal where
    (Literal a b) == (Literal x y) = a == x && b == y  

instance Ord Literal where
    compare (Literal a b) (Literal x y) = if compare b y == EQ then compare a x else compare b y

-- Returns the Complement of a Literal
complement :: Literal -> Literal
complement x = Literal (not $ neg x) (name x)

-- Returns True if complement of a literal is in the clause, else False
contains_complement :: Literal -> Clause -> Bool
contains_complement l c = (Literal (not $ neg l) (name l)) `elem`c

-- Returns the sorted resolvent of two clauses
resolvent :: Literal -> Clause -> Clause -> Clause
resolvent l x y = sort ((filter(\z -> z /= l) x) `union` (filter(\z -> z /= complement l) y))

-- Initialize resolvation for two Clauses
resolve :: Clause -> Clause -> ClauseSet
resolve x y = resolveBE 0 x y []

-- Make all resolvents of two clauses
resolveBE :: Int -> Clause -> Clause -> ClauseSet -> ClauseSet
resolveBE n xs y acc
    | n == length xs = acc
    | contains_complement (xs !! n) y && not ((resolvent (xs !! n) xs y) `elem` acc) = resolveBE (n+1) xs y ((resolvent (xs !! n) xs y):acc)  
    | otherwise = resolveBE (n+1) xs y acc

-- Initialize the resolvation
resN :: ClauseSet -> ClauseSet
resN x = resNBE 0 (head x) (tail x) []

-- Make all resolvents of Res(n-1)
resNBE :: Int -> Clause -> ClauseSet -> ClauseSet -> ClauseSet
resNBE n x y acc
    | null y = acc
    | n == length y = resNBE 0 (head y) (tail y) acc
    | otherwise = resNBE (n+1) x y (acc `union` (resolve x (y !! n)))

-- Init calculating Res(*)
resX :: ClauseSet -> (String, Int, ClauseSet)
resX x = if a == True then ("Satisfiable!", b, c) else ("Not Satisfiable!", b, c) where (a, b, c) = resXBE x 0 []

-- Calculate Res(n) until [] € Res(n) or Res(n) = Res(n-1)
resXBE :: ClauseSet -> Int -> ClauseSet -> (Bool, Int, ClauseSet)
resXBE x n acc
    | [] `elem` acc = (False, n, acc)
    | acc `union`(resN (x `union` acc)) == acc = (True, n, acc `union` resn_x)
    | otherwise = resXBE (acc `union` x) (n+1) (acc `union` resn_x)
    where resn_x = resN x