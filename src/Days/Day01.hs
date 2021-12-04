module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U
import qualified Data.Monoid as Monoid
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text ( Parser )
import qualified Data.Attoparsec.Text as Parser
import Data.Void
import Data.Function ((&))
import Data.Functor ((<&>))
{- ORMOLU_ENABLE -}

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
      a : b : c : d : tail ->
        if b + c + d > a + b + c
          then go (count + 1) (b : c : d : tail)
          else go count (b : c : d : tail)
      _ -> count
