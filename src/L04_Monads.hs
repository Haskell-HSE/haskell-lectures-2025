{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}

module L04_Monads where

import Control.Monad (ap)
import Prelude hiding (Either (..), Maybe (..))

-- template<typename a> struct Identity { ... }
newtype Identity a = Identity {runIdentity :: a}
  deriving (Functor)

-- instance Functor Identity where
--  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure :: a -> Identity a
  pure = Identity

  (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  -- Identity f <*> Identity x = Identity (f x)
  -- mf <*> mx = mf >>= (\f -> (mx >>= (\x -> (pure (f x)))))
  (<*>) = ap

instance Monad Identity where
  (>>=) :: Identity a -> (a -> Identity b) -> Identity b
  Identity x >>= f = f x

-- | Not really a partial function
throw :: a
throw = error "ERROR!!!"

data Maybe a = Just a | Nothing
  deriving (Functor)

type Partial a b = a -> Maybe b

instance Applicative Maybe where
  pure = Just
  (<*>) = ap

instance Monad Maybe where
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Just x >>= f = f x
  Nothing >>= _ = Nothing

safeDiv :: (Integral a) => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

algo :: (Integral a) => a -> Maybe (a, a)
algo x =
  safeDiv x (x - 1)
    >>= ( \d ->
            safeDiv d (d + 1)
              >>= ( \e ->
                      pure (d, e)
                  )
        ) -- callback hell!

algo' :: (Integral a) => a -> Maybe (a, a)
algo' x = do
  d <- safeDiv x (x - 1) -- let d = safeDiv(x, x-1)?;
  e <- safeDiv d (d + 1) -- let e = safeDiv(d, d+1)?;
  pure (d, e) -- Some((d, e))

-- data Maybe a = Nothing | Just a
data Either e a = Left e | Right a
  deriving (Functor)

instance Applicative (Either e) where
  pure :: a -> Either e a
  pure = Right

  (<*>) :: Either e (a -> b) -> Either e a -> Either e b
  Right f <*> Right x = Right (f x)
  Right _ <*> Left e = Left e
  Left e <*> Right _ = Left e
  Left e <*> Left _ = Left e

-- Semigroup e => (e <> e')
-- ap?
-- ap mf mx = do { f <- mf; x <- mx; pure (f x) }

instance Monad (Either e) where
  (>>=) :: Either e a -> (a -> Either e b) -> Either e b
  Left e >>= _ = Left e
  Right x >>= f = f x

data ErrAccum e a = ELeft e | ERight a
  deriving (Functor)

instance (Semigroup e) => Applicative (ErrAccum e) where
  pure = error "TODO"
  (<*>) = error "TODO"

type PureFun a b = a -> b

type AllElse a = IO a

main :: IO ()
main = pure () -- putStrLn "Hello, world!"

newtype State s a = State {runState :: s -> (a, s)}
  deriving (Functor)

instance Applicative (State s) where
  pure :: a -> State s a
  pure x = State (\s -> (x, s))
  (<*>) = ap

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  State mx >>= clb =
    State
      ( \s0 ->
          let (x, s1) = mx s0 in runState (clb x) s1
      )

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s' = State (\_ -> ((), s'))

counter :: State Int ()
counter = do
  cnt <- get
  put (cnt + 1)
  return ()
