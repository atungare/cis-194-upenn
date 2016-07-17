-- E1
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n = (n `mod` 10) : toDigitsRev (n `div` 10)

-- E2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther  = reverse . (zipWith (*) (cycle [1, 2])) . reverse

-- E3
sumDigits :: [Integer] -> Integer
sumDigits = sum . (map (sum . toDigits))

-- E4
validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

-- E5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n start end buffer = (hanoi (n - 1) start buffer end) ++ [(start, end)] ++ (hanoi (n - 1) buffer end start)

-- TODO: E6 optional - hanoi with 2 buffers, find optimal solution path
