module Days.Day01 (runDay) where

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (foldl')
import qualified Program.RunDay as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser =
  Parser.sepBy Parser.double Parser.endOfLine
    <&> fmap truncate

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- partA :: Input -> OutputA
-- partA = go 0
--  where
--   go count depths =
--     case depths of
--       [] -> count
--       [x] -> count
--       x : y : tail ->
--         if y > x
--           then go (count + 1) (y : tail)
--           else go count (y : tail)

partA :: Input -> OutputA
partA =
  snd . foldl' countSuccessiveIncreases (Nothing, 0)
 where
  countSuccessiveIncreases :: (Maybe Int, Int) -> Int -> (Maybe Int, Int)
  countSuccessiveIncreases (prev, count) val =
    ( Just val
    , maybe 0 (\prev' -> if val > prev' then count + 1 else count) prev
    )

------------ PART B ------------
partB :: Input -> OutputB
partB = go 0
 where
  go count depths =
    case depths of
      a : b : c : d : rest ->
        if b + c + d > a + b + c
          then go (count + 1) (b : c : d : rest)
          else go count (b : c : d : rest)
      _ -> count
