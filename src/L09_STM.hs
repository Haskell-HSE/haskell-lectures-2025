{-# LANGUAGE BlockArguments #-}

module L09_STM where

import Control.Concurrent (forkIO, newQSem, signalQSem, waitQSem)
import GHC.Conc (par, STM, TVar, readTVar, writeTVar, retry, newTVar, atomically)
import Numeric.Natural (Natural)
import System.Environment (getArgs)
import Data.Foldable (for_)
import Control.Monad (replicateM_, guard)
import Control.Applicative (Alternative(..))

-- 1. Parallel computations

sumUpTo :: Natural -> Natural -> Natural
sumUpTo k n = impl 0 0
  where
    step = n `div` k
    impl i acc
      | i > n = acc
      | otherwise =
          let p = sum [i .. min (i + step) n]
           in p `par` impl (i + step + 1) (acc + p)

-- async
-- mapConcurrently, ...

appC :: IO ()
appC = do
  [k, n] <- map read <$> getArgs
  print (sumUpTo k n)

-- 2. Parallel serving

appS :: IO ()
appS = do
  files <- getArgs
  sem <- newQSem 0
  for_ files \file -> forkIO do
    contents <- readFile file
    print (file, length contents)
    signalQSem sem
  replicateM_ (length files) (waitQSem sem)

-- 3. Explicit synchronisation
-- see MVars, Chans, Sems,...

exec :: (Eq a, Num a) => a -> TVar a -> STM ()
exec k v = do
  k' <- readTVar v
  guard (k == k')
  writeTVar v (k + 1)

exec' :: STM ()
exec' = do
  v <- newTVar (5 :: Int)
  retry <|> exec 5 v <|> retry

exec'' :: STM ()
exec'' = do
  v <- newTVar (5 :: Int)
  exec 5 v
  exec 5 v

execA :: IO ()
execA = atomically exec''










