{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Days.Day06 (runDay) where

import qualified Control.Monad.ST as ST
import Data.Array.Repa (Array, Z (..), (:.) ((:.)))
import qualified Data.Array.Repa as Arr
import qualified Data.Array.Repa.Eval as Arr (Target, fromList)
import Data.Array.Repa.Repr.Unboxed (U)
import Data.Attoparsec.Text

import qualified Control.Monad as Monad
import Control.Monad.Primitive (PrimMonad (PrimState))
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.Reader as MonadReader
import Data.HashTable.ST.Cuckoo (HashTable)
import Control.Monad.ST (ST, runST)
import qualified Program.RunDay as R (Day, runDay)
import Data.Word (Word64, Word8)
import Data.Bifunctor (bimap)
import Data.Bool (bool)
import qualified Data.HashTable.Class as HT
import Data.Int (Int32, Int64)
import Data.List.Index as IList
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.STRef (newSTRef)
import qualified Data.STRef as STRef
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as MVec
import Data.Vector.Unboxed (MVector, Vector, forM)
import qualified Data.Vector.Unboxed as UVec
import Data.Vector.Unboxed.Mutable (Unbox)
import qualified Data.Vector.Unboxed.Mutable as UMVec

runDay :: R.Day
runDay = R.runDay inputParser partA partB

---------- PARSER ------------
inputParser :: Parser Input
inputParser = do
  ls <- (truncate <$> double) `sepBy` char ','
  pure $ Arr.fromList (Z :. length ls) ls

------------ TYPES ------------
type Input = Array U (Z :. Int) Word8

type OutputA = Word64

type OutputB = Word64

------------ PART A ------------
theLifeOfAFish :: Word -> Word8 -> Word64
theLifeOfAFish 0 fish = 1
theLifeOfAFish days 0 = theLifeOfAFish (days - 1) 8 + theLifeOfAFish (days - 1) 6
theLifeOfAFish days fish = theLifeOfAFish (days - 1) (fish - 1)

solution ::   Word -> Input -> Word64
solution days fishes =
  runST $ Arr.sumAllP $ Arr.map (theLifeOfAFish days) fishes

partA :: Input -> OutputA
partA = solution 80

------------ PART B ------------
partB :: Input -> OutputB
partB = solution 256




-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
------------------------------GRAVEYARD BELOW---------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- ---------- PARSER ------------
-- inputParser :: Parser Input
-- inputParser =
--   (truncate <$> double) `sepBy` char ','

-- ------------ TYPES ------------
-- type Input = [Word8]

-- type OutputA = Integer

-- type OutputB = Integer

-- ------------ PART A ------------
-- theLifeOfAFish :: Int -> Word8 -> Integer
-- theLifeOfAFish 0 fish = 1
-- theLifeOfAFish days 0 = theLifeOfAFish (days - 1) 8 + theLifeOfAFish (days - 1) 6
-- theLifeOfAFish days fish = theLifeOfAFish (days - 1) (fish - 1)

-- solution :: (Foldable t, Functor t) => Int -> t Word8 -> Integer
-- solution days fishes = sum (theLifeOfAFish days <$> fishes)

-- partA :: Input -> OutputA
-- partA = solution 80

-- ------------ PART B ------------
-- partB :: Input -> OutputB
-- partB = solution 256


-- ---------- PARSER ------------
-- inputParser :: Parser Input
-- inputParser =
--   fmap (Map.fromList . UVec.toList . UVec.imap ((,) . toInteger) . UVec.fromList) $
--     (truncate <$> double) `sepBy` char ','

-- ------------ TYPES ------------
-- type Input = Map Integer Word8

-- type OutputA = Integer

-- type OutputB = Integer

-- ------------ PART A ------------
-- lanternfish :: Word -> Map Integer Word8 -> Map Integer Word8
-- lanternfish durationInDays fishes =
--   if durationInDays > 0
--     then lanternfish nextDuration nextFishes
--     else fishes
--  where
--   nextDuration = durationInDays - 1 :: Word
--   nextFishes = Map.foldrWithKey thePassageOfTime fishes fishes

--   thePassageOfTime :: Integer -> Word8 -> Map Integer Word8 -> Map Integer Word8
--   thePassageOfTime key 0 fishes' =
--     fishes'
--       & Map.insert key 6
--       & Map.insert (toInteger $ Map.size fishes') 8
--   thePassageOfTime key fish fishes' = fishes' & Map.insert key (fish - 1)

-- partA :: Input -> OutputA
-- partA = fromIntegral . length . lanternfish 80

-- ------------ PART B ------------
-- partB :: Input -> OutputB
-- partB = fromIntegral . length . lanternfish 256




--
--
-- GAVE UP TRYING TO USE MUTABLE VECTORS

-------- PARSER ------------
-- inputParser :: Parser Input
-- inputParser =
--   fmap UVec.fromList $ (truncate <$> double) `sepBy` char ','

-- ------------ TYPES ------------
-- type Input = Vector Word8

-- type OutputA = Word64

-- type OutputB = Word64

-- ------------ PART A ------------

-- mutableVectorSolution :: Word -> Vector Word8-> Word64
-- mutableVectorSolution durationInDays fishes = runST $ do
--   mFishes <- UVec.thaw fishes
--   (mNewFishes :: MVector s Word8) <- UMVec.new . fromIntegral $ (maxBound :: Int32)
--   newFishesCountRef <- newSTRef 0
--   UVec.forM_ (UVec.enumFromN (1 :: Word) (fromIntegral durationInDays)) $ \_ ->
--     UMVec.iforM_ mFishes $ \ix fish ->
--       if fish == 0
--         then do
--           newFishesCount <- STRef.readSTRef newFishesCountRef
--           UMVec.write mFishes ix 6
--           UMVec.write mNewFishes newFishesCount 8
--           STRef.writeSTRef newFishesCountRef (newFishesCount + 1)
--         else do
--           UMVec.write mFishes ix (fish - 1)
--   newFishesCount <- STRef.readSTRef newFishesCountRef
--   pure $ fromIntegral $ UVec.length fishes + newFishesCount

-- mutableVectorSolution :: Word -> Vector Word8 -> Word64
-- mutableVectorSolution durationInDays fishes = runST $ do
--   mFishes <- UVec.thaw fishes
--   mNewFishes <- flip UMVec.replicate (False, 0 :: Word8) . fromIntegral $ (maxBound :: Int32)

--   newFishesCountRef <- newSTRef 0

--   UVec.forM_ (UVec.enumFromN (1 :: Int) (fromIntegral durationInDays)) $ \_ ->
--     UMVec.iforM_ mNewFishes $ \ix (exists, fish) ->
--       Monad.when exists $
--         if fish == 0
--           then do
--             newFishesCount <- STRef.readSTRef newFishesCountRef
--             UMVec.write mNewFishes ix (True, 6)
--             UMVec.write mNewFishes newFishesCount (True, 8)
--             STRef.writeSTRef newFishesCountRef (newFishesCount + 1)
--           else do
--             UMVec.write mNewFishes ix (True, fish - 1)

--   UVec.forM_ (UVec.enumFromN (1 :: Int) (fromIntegral durationInDays)) $ \_ ->
--     UMVec.iforM_ mFishes $ \ix fish ->
--       if fish == 0
--         then do
--           newFishesCount <- STRef.readSTRef newFishesCountRef
--           UMVec.write mFishes ix 6
--           UMVec.write mNewFishes newFishesCount (True, 8)
--           STRef.writeSTRef newFishesCountRef (newFishesCount + 1)
--         else do
--           UMVec.write mFishes ix (fish - 1)
--   newFishesCount <- STRef.readSTRef newFishesCountRef
--   pure $ fromIntegral $ UVec.length fishes + newFishesCount

-- partA :: Input -> OutputA
-- partA = mutableVectorSolution 80

-- ------------ PART B ------------
-- partB :: Input -> OutputB
-- partB = mutableVectorSolution 256
