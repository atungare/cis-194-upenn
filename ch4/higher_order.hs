import Data.List (nub)

-- E1
fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (\item acc -> (item - 2) * acc) 1 . filter even

--fun2' :: Integer -> Integer
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- E2

-- E3
xor :: [Bool] -> Bool
xor = foldr step False
  where
    step item acc
      | item == True = not acc
      | otherwise    = acc

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\i acc -> (:) (f i) acc) []


-- E4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [1,2] ++ nub [2*x+1 | x <- [1..n], y <- [1..x], z <- [1..y], y + z + 2*y*z < n]

