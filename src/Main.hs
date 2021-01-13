{-# LANGUAGE ConstraintKinds #-}

import Control.Concurrent
import Control.Monad.Catch (catchAll)
import Data.Foldable (traverse_)
import qualified Data.Map as M

type Terminal a = Ord a
type Message n = (Foldable n, Functor n)

-- | Quantities of elements of type `a`.
newtype CountMap a = CountMap { unCountMap :: M.Map a Integer }
    deriving (Eq, Ord, Read, Show)

instance Ord a => Semigroup (CountMap a) where
    CountMap x <> CountMap y = CountMap $ M.unionWith (+) x y

instance Ord a => Monoid (CountMap a) where
    mempty = CountMap M.empty

singletonCountMap :: a -> CountMap a
singletonCountMap x = CountMap $ M.singleton x 1

-- | Create a `CountMap` for a message of `Terminal`s `a`.
countTerminals :: (Terminal a, Message n) => n a -> CountMap a
countTerminals = foldMap singletonCountMap

-- | Remove a single matching `Terminal` from a `CountMap` or return the given `CountMap` when it
-- doesn't match.
-- Returns `Nothing` when the `CountMap` is completely used up.
consume :: Terminal a => a -> CountMap a -> Maybe (CountMap a)
consume a (CountMap m) = justOrEmpty $
    case M.lookup a m of
      Nothing -> CountMap m
      Just 1 -> CountMap $ M.delete a m
      Just i -> CountMap $ M.insert a (pred i) m
    where justOrEmpty countMap
            | countMap == mempty = Nothing
            | otherwise = Just countMap

----------------------------------------------------------------------------------------------------
    -- parsing the message

-- | Check if there are as many `Terminal`s in `[a]` as given in the `CountMap`.
-- Returns either a Unit (OK) or a `CountMap` of the still missing `Terminal`s.
--
-- Worst case: O(length provided terminals)
-- Best case: O(length message * log(size countmap))
testCountMapM :: (Terminal a, Monad m)
              => CountMap a -- ^ CountMap to check
              -> [a]        -- ^ provided terminals
              -> m (Either (CountMap a) ())
testCountMapM countMap [] = pure $ Left countMap
testCountMapM countMap (t:ts) = do
    let mbCountMap' = consume t countMap
    case mbCountMap' of
      Nothing -> pure $ Right ()
      Just countMap' -> testCountMapM countMap' ts

-- | Wraps `testCountMapM` and `countTerminals` for the given problem.
testMessageM :: (Terminal a, Message n, Monad m)
             => n a -- ^ message
             -> [a] -- ^ river
             -> m (Either (CountMap a) ())
testMessageM mes = testCountMapM (countTerminals mes)

----------------------------------------------------------------------------------------------------
    -- handling the traffic light

data TrafficLight = Green
                  | Red
                  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | This function only returns when the `TrafficLight` turns `Red`.
trafficLightBecameRed :: Chan TrafficLight
                      -> ThreadId -- ^ thread to kill, when light turns red
                      -> IO ()
trafficLightBecameRed chan threadToBeKilled = do
    light <- readChan chan
    case light of
      Green -> trafficLightBecameRed chan threadToBeKilled
      Red -> killThread threadToBeKilled

-- | Uses `trafficLightBecameRed` to get the traffic light state until it is `Red` and returns the
-- lazy list of `Terminal`s up to that point.
terminalStream :: Chan TrafficLight
               -> Chan a
               -> IO [a]
terminalStream chanLight chanTerminal = flip catchAll (const $ pure []) $ do -- TODO: don't use catchAll
    thisThread <- myThreadId
    trafficLightThread <- forkIO $ trafficLightBecameRed chanLight thisThread
    nextTerminal <- readChan chanTerminal
    threadDelay 10 -- TODO: This is not necessary unless the river flows with lightspeed, maybe it could be improved still.
                   --       One should also think about whether consuming a few additional terminals actually hurts.
                   --       Really depends on the use case.
                   --       EDIT: After reading the puzzle again, it is clear that this can be completely removed.
    killThread trafficLightThread
    (nextTerminal :) <$> terminalStream chanLight chanTerminal

-- | The whole program.
-- For a description read the puzzle.
testMessageAtRiver :: (Terminal a, Message n)
                   => Chan TrafficLight
                   -> Chan a
                   -> n a -- ^ message
                   -> IO (Either (CountMap a) ())
testMessageAtRiver chanLight chanTerminal message = do
    terminals <- terminalStream chanLight chanTerminal
    testMessageM message terminals

----------------------------------------------------------------------------------------------------
    -- debugging

myMessage :: String
myMessage = "Stell mich??? ein!"

someRiverStream, someRiverStream' :: [Char]
someRiverStream = "Sfsdlkj@hl?Ltak!lsfie ndle las?dasdllasdasda m asdai ac lkhsldf ?"
someRiverStream' = "Sfsdlkj@hlLtaklsfie ndle lasdasdllasdasda m asdai ac lkhsldf "

tryProgram :: (Terminal a, Message n)
           => n a -- ^ message
           -> [a] -- ^ river
           -> IO (Either (CountMap a) ())
tryProgram message river = do
    chanLight <- newChan
    writeChan chanLight Green

    chanRiver <- newChan
    writeList2Chan chanRiver river

    testMessageAtRiver chanLight chanRiver message

tryProgramException :: IO (Either (CountMap Char) ())
tryProgramException = do
    chanLight <- newChan
    writeChan chanLight Green

    chanRiver <- newChan
    _ <- forkIO $ do
        let f t = threadDelay 100 >> writeChan chanRiver t
        threadDelay 100
        traverse_ f "???!!!"
        writeChan chanLight Red
        traverse_ f "Stell mich"

    testMessageAtRiver chanLight chanRiver myMessage

main :: IO ()
main = do
    print =<< tryProgram myMessage someRiverStream
    print =<< tryProgram myMessage someRiverStream'
    print =<< tryProgramException
