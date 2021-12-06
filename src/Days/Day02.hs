module Days.Day02 (runDay) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe ()
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import Data.Bifunctor (Bifunctor (bimap))
import Data.Void (Void)
import qualified Program.RunDay as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser =
  Parser.sepBy commandParser Parser.endOfLine
 where
  cmdParser =
    Parser.choice
      [ Forward <$ Parser.string "forward"
      , Up <$ Parser.string "up"
      , Down <$ Parser.string "down"
      ]
      <* Parser.char ' '
      <*> (truncate <$> Parser.double)
  commandParser = do
    mkCmd <-
      Parser.choice
        [ Forward <$ Parser.string "forward"
        , Up <$ Parser.string "up"
        , Down <$ Parser.string "down"
        ]
    _ <- Parser.char ' '
    depth <- truncate <$> Parser.double
    pure $ mkCmd depth

------------ TYPES ------------
data Command
  = Forward Int
  | Down Int
  | Up Int
  deriving
    ( Show
    )

newtype Depth = Depth {unDepth :: Int}
  deriving
    ( Show
    , Num
    , Eq
    , Enum
    , Ord
    , Real
    , Integral
    )

newtype HPosition = HPosition {unHPosition :: Int}
  deriving
    ( Show
    , Num
    , Eq
    , Enum
    , Ord
    , Real
    , Integral
    )

newtype Aim = Aim {unAim :: Int}
  deriving
    ( Show
    , Num
    , Eq
    , Enum
    , Ord
    , Real
    , Integral
    )

type Input = [Command]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

partA :: Input -> OutputA
partA =
  uncurry (*)
    . bimap unDepth unHPosition
    . foldl' reduce (Depth 0, HPosition 0)
 where
  reduce (depth, hPosition) command =
    case command of
      Forward n ->
        (depth, hPosition + HPosition n)
      Up n ->
        (depth - Depth n, hPosition)
      Down n ->
        (depth + Depth n, hPosition)

------------ PART B ------------
partB :: Input -> OutputB
partB input =
  case foldl' reduce (Aim 0, Depth 0, HPosition 0) input of
    (_, Depth d, HPosition h) -> d * h
 where
  reduce :: (Aim, Depth, HPosition) -> Command -> (Aim, Depth, HPosition)
  reduce (aim, depth, hPosition) command =
    case command of
      Forward n ->
        ( aim
        , depth + fromIntegral (n * fromIntegral aim)
        , hPosition + fromIntegral n
        )
      Up n ->
        (aim - Aim n, depth, hPosition)
      Down n ->
        (aim + Aim n, depth, hPosition)
