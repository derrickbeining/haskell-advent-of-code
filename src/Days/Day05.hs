module Days.Day05 (runDay) where

import Data.Attoparsec.Text (
  Parser,
  char,
  double,
  endOfLine,
  many',
  sepBy,
  space,
  string,
 )
import Data.List (foldl')
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import qualified Program.RunDay as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser =
  lineSegmentParser `sepBy` endOfLine
    >>= (maybe (fail "Not NonEmpty!") pure . NEL.nonEmpty)
 where
  lineSegmentParser = do
    fromCoord <- coordParser
    _ <- many' space
    _ <- string "->"
    _ <- many' space
    LineSegment fromCoord <$> coordParser

  uintParser = truncate <$> double :: Parser Word

  coordParser :: Parser (Coord Word)
  coordParser = do
    coordX <- uintParser
    _ <- char ','
    Coord coordX <$> uintParser

------------ TYPES ------------
type Input = NEL.NonEmpty LineSegment

type OutputA = Word

type OutputB = Word

data Coord a = Coord {pointX :: a, pointY :: a}
  deriving
    ( Show
    , Eq
    , Ord
    )

data LineSegment = LineSegment
  { lsFrom :: Coord Word
  , lsTo :: Coord Word
  }
  deriving
    ( Show
    )

------------ UTILS ------------

-- Enumeration operator for `Coord`. Creates a list of all `Coord`s between
-- two `Coord`s.
(....) :: (Num a, Ord a, Enum a) => Coord a -> Coord a -> [Coord a]
Coord fromX fromY .... Coord toX toY =
  uncurry Coord <$> zip xs ys
 where
  thenX = case compare fromX toX of
    EQ -> fromX + 1
    LT -> fromX + 1
    GT -> fromX - 1
  thenY = case compare fromY toY of
    EQ -> fromY + 1
    LT -> fromY + 1
    GT -> fromY - 1
  (xs, ys) = case (enumFromThenTo fromX thenX toX, enumFromThenTo fromY thenY toY) of
    (xs', ys') | length xs' > length ys' -> (xs', ys' ++ replicate (length xs' - length ys') toY)
    (xs', ys') | length xs' < length ys' -> (xs' ++ replicate (length ys' - length xs') toX, ys')
    unchanged -> unchanged

------------ PART A ------------

partA :: Input -> OutputA
partA =
  partA' . NEL.filter isHorizontalOrVertical
 where
  isHorizontalOrVertical (LineSegment (Coord fromX fromY) (Coord toX toY)) =
    fromX == toX || fromY == toY

partA' :: [LineSegment] -> OutputA
partA' lines =
  fromIntegral $ Map.size $ Map.filter (>= 2) coordCounts
 where
  coordCounts =
    foldl' countCoords Map.empty lines

  countCoords counts (LineSegment fromCoord toCoord) =
    let coords = fromCoord .... toCoord
        nextCoordCounts = foldl' upsertCoord counts coords
     in nextCoordCounts

  upsertCoord counts coord =
    Map.alter upsert coord counts

  upsert Nothing = Just (1 :: Word)
  upsert (Just count) = Just (count + 1)

------------ PART B ------------
partB :: Input -> OutputB
partB = partA' . NEL.toList
