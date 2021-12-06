module Days.Day03 (runDay, binToDec) where

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import qualified Data.Maybe as Maybe
import Data.Vector (Vector, (!?), (//))
import qualified Data.Vector as Vec
import qualified Program.RunDay as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser =
  Vec.fromList
    <$> Parser.sepBy binParser Parser.endOfLine
 where
  binParser =
    fmap Vec.fromList $
      Parser.many' $
        Parser.choice
          [ False <$ Parser.char '0'
          , True <$ Parser.char '1'
          ]

------------ TYPES ------------
type Binary = Vector Bool

type Input = Vector Binary

type OutputA = Int

type OutputB = Int

------------ PART A ------------

binToDec :: Binary -> Int
binToDec = Vec.ifoldr convert 0 . Vec.reverse
 where
  convert ix bit acc =
    acc + ((2 ^ ix) * fromEnum bit)

calculateMostCommonBits :: Vector Binary -> Binary
calculateMostCommonBits bins =
  Vec.ifoldl' groupBitsByIndex Vec.empty bins
    <&> Vec.partition id
    <&> (\(ones, zeros) -> length ones >= length zeros)
 where
  groupBitsByIndex :: Vector Binary -> Int -> Binary -> Vector Binary
  groupBitsByIndex acc binsIx bin
    | null acc = bin <&> Vec.singleton
    | otherwise = Vec.ifoldl' appendBitsByIx acc bin

  appendBitsByIx :: Vector Binary -> Int -> Bool -> Vector Binary
  appendBitsByIx accum binIx bit =
    accum // [(binIx, appended)]
   where
    appended =
      fromMaybe Vec.empty (accum !? binIx)
        `Vec.snoc` bit

partA :: Input -> OutputA
partA bins =
  binToDec gammaRate * binToDec epsilonRate
 where
  gammaRate =
    calculateMostCommonBits bins
  epsilonRate =
    not <$> gammaRate

------------ PART B ------------
partB :: Input -> OutputB
partB bins =
  fromMaybe 0 $ Just (*) <*> oxyGenRating <*> c02ScrubRating
 where
  oxyGenRating =
    binToDec
      <$> findRating calculateMostCommonBits bins 0

  c02ScrubRating =
    binToDec
      <$> findRating (fmap not . calculateMostCommonBits) bins 0

  findRating :: (Vector Binary -> Binary) -> Vector Binary -> Int -> Maybe Binary
  findRating method bins ix =
    let mostCommonBits = method bins
        remainingBins =
          case Vec.filter (\bin -> bin !? ix == mostCommonBits !? ix) bins of
            filtered | null filtered -> Vec.catMaybes $ Vec.singleton (bins !? (length bins - 1))
            filtered -> filtered
     in if length remainingBins == 1
          then remainingBins !? 0
          else findRating method remainingBins (ix + 1)
