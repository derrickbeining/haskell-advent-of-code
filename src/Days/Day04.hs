{-# LANGUAGE NamedFieldPuns #-}

module Days.Day04 (runDay) where

import Control.Monad.State (State)
import qualified Control.Monad.State as MonadState
import Data.Attoparsec.Text (
  Parser,
  char,
  double,
  endOfLine,
  many',
  manyTill',
  sepBy,
  string,
 )
import Data.List (find)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NEL
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vec
import Data.Word (Word)
import qualified GHC.Base as NEL
import qualified Program.RunDay as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  lottoNums <-
    lottoNumsParser
      >>= maybe (fail "No numbers found") pure . NEL.nonEmpty
  boards <-
    boardParser `sepBy` endOfLine
      >>= maybe (fail "No boards found") pure . NEL.nonEmpty
  pure (lottoNums, boards)
 where
  twoNewlines = endOfLine >> endOfLine

  lottoNumsParser = flip manyTill' twoNewlines $ do
    n <- truncate <$> double :: Parser Word
    _ <- many' $ string ","
    pure n

  boardParser = do
    a <- rowParser <* endOfLine
    b <- rowParser <* endOfLine
    c <- rowParser <* endOfLine
    d <- rowParser <* endOfLine
    e <- rowParser <* endOfLine
    pure $ Board a b c d e

  rowParser = do
    a <- many' (char ' ') *> (truncate <$> double) <* many' (char ' ')
    b <- many' (char ' ') *> (truncate <$> double) <* many' (char ' ')
    c <- many' (char ' ') *> (truncate <$> double) <* many' (char ' ')
    d <- many' (char ' ') *> (truncate <$> double) <* many' (char ' ')
    e <- many' (char ' ') *> (truncate <$> double) <* many' (char ' ')
    pure $
      Row
        (UnmarkedCell a)
        (UnmarkedCell b)
        (UnmarkedCell c)
        (UnmarkedCell d)
        (UnmarkedCell e)

------------ TYPES ------------
type BingoNums = NEL.NonEmpty Word

type BingoBoard = Board (Row Cell)

type BingoBoards = NEL.NonEmpty BingoBoard

data Board a = Board a a a a a
  deriving
    ( Show
    , Functor
    , Foldable
    )

data Row a = Row a a a a a
  deriving
    ( Show
    , Functor
    , Foldable
    )

data Cell
  = UnmarkedCell Word
  | MarkedCell Word
  deriving
    ( Show
    )

isMarkedCell :: Cell -> Bool
isMarkedCell (UnmarkedCell _) = False
isMarkedCell (MarkedCell _) = True

newtype FinalScore = FinalScore {unFinalScore :: Word}
  deriving
    ( Show
    )

type Input = (BingoNums, BingoBoards)

type OutputA = FinalScore

type OutputB = FinalScore

data BingoState = BingoState
  { winners :: Vector (Word, BingoBoard)
  , numbers :: [Word]
  , boards :: Maybe BingoBoards
  }

type BingoApp =
  State BingoState FinalScore

updateBoards :: Word -> BingoBoards -> BingoBoards
updateBoards numberDrawn =
  fmap (updateRows numberDrawn)

updateRows :: Word -> BingoBoard -> BingoBoard
updateRows n =
  fmap (updateRow n)

updateRow :: Word -> Row Cell -> Row Cell
updateRow n =
  fmap (updateCell n)

updateCell :: Word -> Cell -> Cell
updateCell n cell =
  case cell of
    UnmarkedCell m | n == m -> MarkedCell m
    unchanged -> unchanged

isWinner :: Foldable t => t (Row Cell) -> Bool
isWinner board =
  any (all isMarkedCell) board
    || all (isMarkedCellAt 0) board
    || all (isMarkedCellAt 1) board
    || all (isMarkedCellAt 2) board
    || all (isMarkedCellAt 3) board
    || all (isMarkedCellAt 4) board

isMarkedCellAt :: (Eq a, Num a) => a -> Row Cell -> Bool
isMarkedCellAt 0 (Row a b c d e) = isMarkedCell a
isMarkedCellAt 1 (Row a b c d e) = isMarkedCell b
isMarkedCellAt 2 (Row a b c d e) = isMarkedCell c
isMarkedCellAt 3 (Row a b c d e) = isMarkedCell d
isMarkedCellAt 4 (Row a b c d e) = isMarkedCell e
isMarkedCellAt _ _ = False

calculateFinalScore :: (Foldable t1, Foldable t2) => Word -> t1 (t2 Cell) -> FinalScore
calculateFinalScore n board =
  FinalScore $ foldr foldRow 0 board * n
 where
  foldRow row sumSoFar =
    foldr sumUnmarkedCells sumSoFar row

sumUnmarkedCells :: Cell -> Word -> Word
sumUnmarkedCells (UnmarkedCell m) sumSoFar = sumSoFar + m
sumUnmarkedCells (MarkedCell _) sumSoFar = sumSoFar

------------ PART A ------------
partA :: Input -> OutputA
partA (bingoNums, bingoBoards) =
  case find isWinner updatedBoards of
    Nothing ->
      case NEL.nonEmpty remainingNums of
        Nothing -> FinalScore 0
        Just nums -> partA (nums, updatedBoards)
    Just winner ->
      calculateFinalScore num winner
 where
  num :| remainingNums = bingoNums

  updatedBoards = updateBoards num bingoBoards

------------ PART B ------------
partB :: Input -> OutputB
partB (bingoNums, bingoBoards) =
  MonadState.evalState bingoApp initialState
 where
  initialState =
    BingoState Vec.empty (NEL.toList bingoNums) (Just bingoBoards)

  bingoApp :: BingoApp
  bingoApp = do
    state@BingoState{winners, numbers, boards} <- MonadState.get

    case (numbers, boards) of
      -- No remaining numbers, game over, check if winner
      ([], _) -> case winners !? (length winners - 1) of
        Nothing -> pure $ FinalScore 0
        Just (num, lastWinner) -> pure $ calculateFinalScore num lastWinner
      -- No remaining boards, same logic as no remaining numbers; recur
      (_, Nothing) -> do
        MonadState.put $ state{numbers = []}
        bingoApp
      -- Draw next number, update board, find winners, update state, and recur
      (numberDrawn : remainingNumbers, Just boards') -> do
        let updatedBoards = updateBoards numberDrawn boards'
            (newWinners, notWinners) = NEL.partition isWinner updatedBoards

        MonadState.put $
          state
            { winners = winners <> ((numberDrawn,) <$> Vec.fromList newWinners)
            , numbers = remainingNumbers
            , boards = NEL.nonEmpty notWinners
            }

        bingoApp
