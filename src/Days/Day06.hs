module Days.Day06 (runDay) where

import Data.Attoparsec.Text
import qualified Program.RunDay as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser =
  (truncate <$> double) `sepBy` char ','

------------ TYPES ------------
type Input = [Word]

type OutputA = Word

type OutputB = Word

------------ PART A ------------
lanternfish :: Word -> [Word] -> [Word]
lanternfish durationInDays fishes =
  if durationInDays > 0
    then lanternfish (durationInDays - 1) nextFishes
    else fishes
 where
  nextFishes = foldr thePassageOfTime [] fishes

  thePassageOfTime :: Word -> [Word] -> [Word]
  thePassageOfTime 0 nextFishesSorFar = 8 : 6 : nextFishesSorFar
  thePassageOfTime fish nextFishesSorFar = (fish - 1) : nextFishesSorFar

partA :: Input -> OutputA
partA = fromIntegral . length . lanternfish 80

------------ PART B ------------
partB :: Input -> OutputB
partB = fromIntegral . length . lanternfish 256

