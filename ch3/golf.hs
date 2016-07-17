module Golf where

import Data.List (tails)

-- E1
every :: Int -> [a] -> [a]
every n xs = case drop (n-1) xs of
  (y:ys) -> y:(every n ys)
  [] -> []

skips :: [a] -> [[a]]
skips xs = map (flip every xs) [1..(length xs)]

-- E2
localMaxima :: (Ord a) => [a] -> [a]
localMaxima xs = [y | (x:y:z:_) <- tails xs, y > x && y > z]

-- E3
count :: (Eq a) => a -> [a] -> Int
count x = length . (filter (==x))

histogram :: [Integer] -> String
histogram xs = unlines ([ buildRow n | n <- height] ++ [['=' | m <- range]] ++ [concat [show m | m <- range]])
  where
    range = [0..9]
    nums = map (flip count xs) range
    mx = maximum nums
    height = [mx, mx-1..1]
    buildRow n = map (\x -> if x >= n then '*' else ' ') nums