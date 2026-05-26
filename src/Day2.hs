module Day2 where

import AoCPrelude ( runDay )

type Range = (Int, Int)

egInput :: String
egInput = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

-- this is less readable in pointfree but it's a fun exercise
surround :: String -> String
surround = ("(" ++) . (++ ")")

replace :: Char -> Char -> String -> String
replace from to = fmap (\ch -> if ch == from then to else ch)

toRange :: String -> Range
toRange = read . surround . replace '-' ','

initParse :: String -> [Range]
initParse = fmap toRange . words . replace ',' ' '

invalids :: (String -> Bool) -> Range -> Int
invalids invalidf (a,b) = sum $ filter (invalidf . show) [a..b]

-- split num down the middle then compare left and right side
invalid :: String -> Bool
invalid = liftA2 (==) fst snd . liftA2 splitAt ((`div` 2) . length) id

-- >>> task1 egInput
-- 1227775554

task1 :: String -> Int
task1 = sum . fmap (invalids invalid) . initParse

groups :: Int -> String -> [String]
groups _ "" = []
groups n xs = if length xs < n then [xs] else take n xs : groups n (drop n xs)

allRepeat :: String -> Int -> Bool
allRepeat x n = (`groups` x) n == replicate (length x `div` n) (take n x)

-- just check all possible groupings for invalid
invalid' :: String -> Bool
invalid' s = any (allRepeat s) [1..length s - 1]

-- >>> task2 egInput
-- 4174379265

task2 :: String -> Int
task2 = sum . fmap (invalids invalid') . initParse

runTask1 :: IO Int
runTask1 = runDay 2 task1

runTask2 :: IO Int
runTask2 = runDay 2 task2
