-- Implementation of the resolution algorithm in propositional logic
module Resolution (Literal (..), resX, resN) where
-- needed imports
import Data.List (union, sort)

-- Fundamental Data Structures and type aliases
data Literal = Literal {sig :: Bool, name :: String}
type Clause = [Literal]
type ClauseSet = [Clause]

instance Show Literal where
    show (Literal a b) = if a then b else "¬" ++ b

instance Eq Literal where
    (Literal a b) == (Literal x y) = a == x && b == y

instance Ord Literal where
    compare (Literal a b) (Literal x y) = if compare b y == EQ then compare a x else compare b y

-- Returns the complementary literal
complement :: Literal -> Literal
complement x = Literal (not $ sig x) (name x)

-- Returns the sorted resolvent of two clauses
resolvent :: Literal -> Clause -> Clause -> Clause
resolvent l x y = sort((filter(\z -> z /= l) x) `union` (filter(\z -> z /= complement l) y))

-- Initialize resolvation for two Clauses
resolve :: Clause -> Clause -> ClauseSet
resolve x y = resolveBE 0 x y []

-- Make all resolvents of two clauses
resolveBE :: Int -> Clause -> Clause -> ClauseSet -> ClauseSet
resolveBE n xs y acc 
    | n == length xs = acc
    | (complement (xs !! n)) `elem` y && not ((resolvent (xs !! n) xs y) `elem` acc) = resolveBE (n+1) xs y ((resolvent (xs !! n) xs y):acc)
    | otherwise = resolveBE (n+1) xs y acc

-- Initialize the resolvation
resN :: ClauseSet -> ClauseSet
resN x = resNBE 0 (head x) (tail x) x

-- Make all resolvents for Res(n)
resNBE :: Int -> Clause -> ClauseSet -> ClauseSet -> ClauseSet
resNBE n x y acc 
    | null y = acc
    | n == length y = resNBE 0 (head y) (tail y) acc
    | otherwise = resNBE (n+1) x y (acc `union` (resolve x (y !! n)))

-- Init calculating Res(*), error handling
resX :: ClauseSet -> (String, Int, ClauseSet)
resX x = case resXBE (map (\c -> sort c) x) 0 [] of
    Just (a, b, c) -> if a == True then ("Satisfiable!", b, c) else ("Not Satisfiable!", b, c)
    Nothing -> ("Error!", 0, [])

-- Calculate Res(n) until [] \in Res(n) or Res(n) = Res(n-1)
resXBE :: ClauseSet -> Int -> ClauseSet -> Maybe (Bool, Int, ClauseSet)
resXBE x n acc 
    | null x = Nothing
    | [] `elem` x = Just (False, n, x)
    | x == acc = Just (True, (n-1), x)
    | otherwise = resXBE (acc `union` (resN x)) (n+1) (acc `union` x)
