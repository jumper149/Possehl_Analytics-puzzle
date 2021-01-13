{-# LANGUAGE ConstraintKinds #-}

import qualified Data.Map as M
import Control.Monad.Identity
import Control.Monad.IO.Class

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

-- | For debugging.
testMessageAtRiver' :: (Terminal a, Message n, Monad m)
                    => n a   -- ^ message
                    -> m [a] -- ^ stream of terminals
                    -> m (Either (CountMap a) ())
testMessageAtRiver' mes str = testMessageM mes =<< str

----------------------------------------------------------------------------------------------------

data TrafficLight = Green
                  | Red
                  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | What does the traffic light show?
-- The monad here would probably have `MonadIO`.
getTrafficLightState :: Monad m => m TrafficLight
getTrafficLightState = undefined

-- | Uses `getTrafficLightState` to get the traffic light state until it is `Red` and returns the
-- lazy list of `Terminal`s up to that point.
-- The monad here would probably have `MonadIO`.
terminalStream :: (Terminal a, Monad m) => m [a]
terminalStream = undefined

testMessageAtRiver :: (Terminal a, Foldable f, Functor f, Monad m)
                   => f a   -- ^ message
                   -> m (Either (CountMap a) ())
testMessageAtRiver = flip testMessageAtRiver' terminalStream

----------------------------------------------------------------------------------------------------

myMessage :: [Char]
myMessage = "Stell mich ein!"

-- | Why am I wrapping this in `IdentityT`?
-- This is just a placeholder, but in a real world scenario, there would be `IO` involved to read the
-- `Terminal`s from the river. The `TrafficLight` would also be wrapped in `IO`.
-- So why not just use `IO [Char]` then?
-- This allows the programmer that fills in the functions above (`getTrafficLightState` and
-- `terminalStream`) to use some monad like:
--   `ReaderT (TVar TrafficLight) IO`
someRiverStream, someRiverStream' :: Monad m => IdentityT m [Char]
someRiverStream = pure "Sfsdlkj@hlLtak!lsfie ndle lasdasdllasdasda m asdai ac lkhsldf "
someRiverStream' = pure "Sfsdlkj@hlLtaklsfie ndle lasdasdllasdasda m asdai ac lkhsldf "

----------------------------------------------------------------------------------------------------

main :: IO ()
main = runIdentityT $ do

    messageOK <- testMessageAtRiver' myMessage someRiverStream
    liftIO $ print messageOK

    messageFail <- testMessageAtRiver' myMessage someRiverStream'
    liftIO $ print messageFail
