module Day3 where

import AoCPrelude

egInput :: String
egInput = "987654321111111\n\
          \811111111111119\n\
          \234234234234278\n\
          \818181911112111"

toListInts :: String -> [Int]
toListInts = fmap (read . (: []))

stripLeft :: [Int] -> Int -> [Int]
stripLeft [] _ = [] -- unreachable for given inputs
stripLeft [x,y] _ = [x,y] -- disallow singletons
stripLeft (x:xs) largest = if x < largest then stripLeft xs largest else x:xs

firstDigit :: [Int] -> [Int]
firstDigit = stripLeft <*> (maximum . init) -- don't need brackets here

secondDigit :: [Int] -> Int
secondDigit [] = 1
secondDigit (x:xs) = x * 10 + maximum xs

-- >>> task1 egInput
-- 357

-- remove all values left of the max
-- then find largest of the remaining values (excluding max)
-- max can't be the last element as there is always a better 2-digit joltage
task1 :: String -> Int
task1 = sum . fmap (secondDigit . firstDigit . toListInts) . words

removeIndex :: [a] -> Int -> [a]
removeIndex xs n = take n xs ++ drop (n+1) xs

-- try removing every index and pick best
bestRemoval :: String -> String
bestRemoval x = show $ maximum (read . removeIndex x <$> [0..length x - 1] :: [Integer])

-- repeat bestRemoval until we have 12 elements
-- (not sure if there is some iterateUntil)
bestTwelve :: String -> Int
bestTwelve = read . last . liftA2 take (\x -> length x - 11) (iterate bestRemoval)

-- >>> task2 egInput
-- 3121910778619

task2 :: String -> Int
task2 = sum . fmap bestTwelve . words

runTask1 :: IO Int
runTask1 = runDay 3 task1

runTask2 :: IO Int
runTask2 = runDay 3 task2
