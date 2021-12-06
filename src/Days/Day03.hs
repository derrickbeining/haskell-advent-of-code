module Days.Day03 (runDay, binToDec) where

import Data.List ()
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector, (!?), (//))
import qualified Data.Vector as Vec
import qualified Util.Util as U

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Maybe as Maybe
import Data.Void (Void)
import qualified Program.RunDay as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser =
  Parser.sepBy binParser Parser.endOfLine
    <&> Vec.fromList
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

calculateMostCommonBits :: Vector Binary -> Vector Bool
calculateMostCommonBits bins =
  Vec.ifoldl' reduce Vec.empty bins
    <&> Vec.partition id
    <&> (\(ones, zeros) -> length ones >= length zeros)
 where
  reduce :: Vector Binary -> Int -> Binary -> Vector Binary
  reduce acc binsIx bin
    | null acc = bin <&> Vec.singleton
    | otherwise = Vec.ifoldl' go acc bin

  go :: Vector Binary -> Int -> Bool -> Vector Binary
  go accum binIx bit =
    accum // [(binIx, appended)]
   where
    appended =
      fromMaybe Vec.empty (accum !? binIx)
        `Vec.snoc` bit

partA :: Input -> OutputA
partA bins =
  binToDec gammaRate * binToDec epsilonRate
 where
  gammaRate = calculateMostCommonBits bins
  epsilonRate = not <$> gammaRate

------------ PART B ------------
partB :: Input -> OutputB
partB bins =
  fromMaybe 0 $ Just (*) <*> oxyGenRating <*> c02ScrubRating
 where
  oxyGenRating =
    binToDec <$> findRating calculateMostCommonBits bins 0
  c02ScrubRating =
    binToDec <$> findRating (fmap not . calculateMostCommonBits) bins 0

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
