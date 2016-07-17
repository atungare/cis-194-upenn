module Golf where

import Data.List (tails)

-- E1
every :: Int -> [a] -> [a]
every n xs = case drop (n-1) xs of
  (y:ys) -> y:(every n ys)
  [] -> []

skips :: [a] -> [[a]]
skips xs = map (\n -> every n xs) [1..(length xs)]

-- E2
localMaxima :: (Ord a) => [a] -> [a]
localMaxima xs = [y | (x:y:z:_) <- tails xs, y > x && y > z]

-- E3
--histogram :: [Integer] -> String