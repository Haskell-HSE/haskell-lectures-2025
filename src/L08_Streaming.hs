{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}



module L08_Streaming where

import Prelude (Int)
import System.IO (IO, print, getContents, getLine, putStrLn)
import Control.Monad (replicateM, replicateM_, (>>=), return, (>>), ap, Monad)
import Data.Foldable (for_, Foldable (..))
import Text.Show (Show)
import Data.Traversable (for)
import Data.Function (id, ($))
import Data.String (String)
import Data.List (replicate)
import Data.Functor (Functor (..))
import Control.Applicative (Applicative (..))
import Data.Semigroup (Semigroup (..))
import Data.Monoid (Monoid (..))

-- list?
data List a = Nil | Cons a (List a)

-- Haskell is lazy
ones :: List Int
ones = Cons 1 ones

main0 :: IO ()
main0 = do
  file <- getContents
  -- hGetContents
  print file

main1 :: IO ()
main1 = do
  strs <- replicateM 10 getLine
  for_ strs putStrLn

main2 :: IO ()
main2 = do
  replicateM_ 10 (getLine >>= putStrLn)

logging :: Show info => [elem] -> (elem -> info) -> IO [elem]
logging elems getInfo = for elems \el -> print (getInfo el) >> return el

main3 :: IO ()
main3 = do
  elems <- replicateM 10 getLine
  _ <- logging elems id
  return ()

logging1 :: Show info => [IO elem] -> (elem -> info) -> IO [elem]
logging1 acts getInfo = for acts \act -> do
  element <- act
  print (getInfo element)
  return element

main4 :: IO ()
main4 = do
  let acts :: [IO String] = replicate 10 getLine
  _ <- logging1 acts id
  return ()

newtype Stream0 a = Stream0 {runStream0 :: [IO a]}
  deriving (Functor)

--instance Applicative Stream0 where
--  pure :: a -> Stream0 a
--  pure x = Stream0 [pure x]

--  (<*>) :: Stream0 (a -> b) -> Stream0 a -> Stream0 b
--  (<*>) = ap

--instance Monad Stream0 where
--  (>>=) :: Stream0 a -> (a -> Stream0 b) -> Stream0 b
--  Stream0 acts >>= cont =
--    let r = [ fmap (runStream0 . cont) act | act <- acts ]
--     in Stream0 _

data StreamIOImpl a
  = Done
  | Next a (StreamIO a)
  deriving (Functor)

newtype StreamIO a = StreamIO { runStreamIO :: IO (StreamIOImpl a) }
  deriving (Functor)

singleIO :: IO a -> StreamIO a
-- Monad m => m a -> StreamT m a
-- ^ lift from MonadTrans
singleIO x = StreamIO $ fmap (`Next` mempty) x

instance Semigroup (StreamIO a) where
  StreamIO actx <> StreamIO acty = StreamIO $ actx >>= \case
    Done -> acty
    Next x xs -> pure $ Next x (xs <> StreamIO acty)

instance Monoid (StreamIO a) where
  mempty = StreamIO (pure Done)

instance Applicative StreamIO where
  pure :: a -> StreamIO a
  pure x = StreamIO $ pure (Next x mempty)
  (<*>) = ap

instance Monad StreamIO where
  (>>=) :: StreamIO a -> (a -> StreamIO b) -> StreamIO b
  StreamIO act >>= k = StreamIO $ act >>= \case
    Done -> return Done
    Next x xs -> runStreamIO (k x <> (xs >>= k))

replicateStream :: Int -> a -> StreamIO a
replicateStream n x = fold $ replicate n (pure x)

replicateStreamIO :: Int -> IO a -> StreamIO a
replicateStreamIO n x = fold $ replicate n (singleIO x)

loggingStream :: Show info => StreamIO a -> (a -> info) -> StreamIO a
loggingStream s i = s >>= \x -> singleIO (print (i x)) >> pure x

-- conduit, ...
-- streamly
-- foldl
