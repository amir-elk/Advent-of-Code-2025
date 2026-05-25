module Day1 where

import AoCPrelude ( Parser, (<|>), char, int, parseInput, runDay )

data Direction = L | R deriving (Show, Enum, Eq)
type Turn = Int

dirParse :: Parser Direction
dirParse = toDir <$> (char 'L' <|> char 'R')
    where
        toDir :: Char -> Direction
        toDir 'L' = L
        toDir 'R' = R
        toDir _ = error "should be unreachable"

instrParse :: Parser Turn
instrParse = do
    dir <- dirParse
    val <- int
    if dir == L then pure $ -val else pure val

initParse :: String -> [Turn]
initParse = fmap (parseInput instrParse) . words

egInput :: String
egInput = "L68 L30 R48 L5 R60 L55 L1 L99 R14 L82"

rotate :: Int -> Turn -> Int
rotate acc turn
    | turn < 0 = (100 + acc + turn) `mod` 100
    | otherwise =      (acc + turn) `mod` 100

dials :: [Turn] -> [Int]
dials = scanl rotate 50

task1 :: String -> Int
task1 = length . filter (== 0) . dials . initParse

zeroes :: [Turn] -> [Int]
zeroes = liftA2 (zipWith crosses) dials id

crosses :: Int -> Turn -> Int
crosses dial turn
    | dial == 0 && turn >= -100 && turn < 0 = 0 -- this edgecase cost me an hour
    | dial + turn < 0 = 1 + crosses dial (100 + turn)
    | dial + turn > 100 = 1 + crosses dial (turn - 100)
    | otherwise = 0

task2 :: String -> Int
task2 = liftA2 (+) (sum . zeroes . initParse) task1

runTask1 :: IO Int
runTask1 = runDay 1 task1

runTask2 :: IO Int
runTask2 = runDay 1 task2
