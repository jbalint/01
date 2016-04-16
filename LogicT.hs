-- experiments in LogicT
import Control.Monad

import Control.Monad.Trans.Cont

-- placeholder
type L a = [a]

runL :: Maybe Int -> L a -> [a]
runL Nothing _ = []
runL (Just n) l = take n l

x = 1

odds :: MonadPlus m => m Int
odds = (return 1) `mplus` (odds >>= \a -> return (2 + a))

-- this is unfair in that it never evaluates t3 because odds is
-- infinite (backtracks abitrarily)
unfairDisjunction :: [Int] -> [Int]
unfairDisjunction t3 = runL (Just 1) $ do
  x <- odds `mplus` t3
  if even x then return x else mzero

oddsPlus :: MonadPlus m => Int -> m Int
oddsPlus n = odds >>= \a -> return (a + n)

--Arrow
--Kleisli
--Functor


posOf :: Eq a => a -> [a] -> Maybe Int
posOf a as = (`runCont` id) $ do
  callCC (\k -> find 0 a as k)
  where
    find :: Eq a => Int -> a -> [a] -> (Maybe Int -> Cont (Maybe Int) (Maybe Int)) -> Cont (Maybe Int) (Maybe Int)
    find _ _ [] k = k Nothing
    find n q (a:as) k = case q == a of
                         True -> k (Just n)
                         False -> find (n + 1) q as k
