module Day4 where

import AoCPrelude ( runDay )
import Control.Monad ( guard, join, (>=>) )
import Control.Monad.Writer ( execWriter, MonadWriter(tell), Writer )
import Data.Monoid ( Sum, getSum )

egInput :: String
egInput = "..@@.@@@@.\n\
          \@@@.@.@.@@\n\
          \@@@@@.@.@@\n\
          \@.@@@@..@.\n\
          \@@.@@@@.@@\n\
          \.@@@@@@@.@\n\
          \.@.@.@.@@@\n\
          \@.@@@.@@@@\n\
          \.@@@@@@@@.\n\
          \@.@.@@@.@."

getNeighbours :: [[a]] -> (Int, Int) -> [a]
getNeighbours xss (row, col) = do
    row' <- [row-1..row+1]
    guard $ row' >= 0 && row' < length xss
    col' <- [col-1..col+1]
    guard $ col' >= 0 && col' < length (head xss) && not (row == row' && col == col')
    pure $ xss !! row' !! col'

-- >>> getNeighbours (words egInput) (1,1)
-- "..@@@@@@"

numAts :: [[Char]] -> [Int]
numAts xss = do
    row <- [0..length xss - 1]
    col <- [0..length (head xss) - 1]
    guard $ xss !! row !! col == '@'
    pure $ length $ filter (== '@') $ getNeighbours xss (row, col)

task1 :: String -> Int
task1 = length . filter (<4) . numAts . words

-- >>> task1 egInput
-- 13

numAts' :: [[Char]] -> [Int]
numAts' xss = do
    row <- [0..length xss - 1]
    col <- [0..length (head xss) - 1]
    if xss !! row !! col == '@'
        then pure $ length $ filter (== '@') $ getNeighbours xss (row, col)
        else pure 0

groups :: Int -> [a] -> [[a]]
groups _ [] = []
groups n xs = if length xs < n then [xs] else take n xs : groups n (drop n xs)

numChanges :: [[(Char, Int)]] -> Int
numChanges = length . filter (\(c, x) -> c == '@' && x < 4) . join

neighbourMap :: [[Char]] -> [[(Char, Int)]]
neighbourMap xss = liftA2 (zipWith zip) id (groups (length xss) . numAts') xss

updateRemovals :: [[Char]] -> Writer (Sum Int) [[Char]]
updateRemovals xss = do
    tell $ pure $ numChanges $ neighbourMap xss
    return $ fmap (\(c, x) -> if c == '@' && x >= 4 then '@' else '.') <$> neighbourMap xss

untilWriter :: [[Char]] -> Writer (Sum Int) [[Char]]
untilWriter xss = if (==0) . numChanges . neighbourMap $ xss then pure xss
                  else (updateRemovals >=> untilWriter) xss

-- >>> task2 egInput
-- 43

task2 :: String -> Int
task2 = getSum . execWriter . untilWriter . words

runTask1 :: IO Int
runTask1 = runDay 4 task1

runTask2 :: IO Int
runTask2 = runDay 4 task2
