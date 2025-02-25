

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module L07_Parametricity where
import Control.Monad (ap)

newtype ReaderT e m a = ReaderT { runReaderT :: e -> m a }
  deriving (Functor)
instance Applicative m => Applicative (ReaderT e m)
instance Monad m => Monad (ReaderT e m)

newtype StateT s m a = StateT { runStateT :: s -> m (s, a) }
  deriving (Functor)
instance Monad m => Applicative (StateT s m) where
  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  StateT sf <*> StateT sx = StateT \s0 -> do
    (s1, f) <- sf s0
    (s2, x) <- sx s1
    return (s2, f x)
instance Monad m => Monad (StateT s m)

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
  deriving (Functor)
instance Applicative m => Applicative (MaybeT m)
instance Monad m => Monad (MaybeT m)

data Env
data State
type MyMonad = ReaderT Env (StateT State (MaybeT IO))
-- ^ Monad transformers

-- mtl
class Monad m => MonadReader e m | m -> e where
  ask :: m e
  ask = reader return

  local :: (e -> e) -> m a -> m a
  reader :: (e -> m a) -> m a

readsEnv :: Monad m => ReaderT e m e
readsEnv = ReaderT \e -> return e

readsEnv' :: MonadReader e m => m e
readsEnv' = ask

class Monad m => MonadState s m | m -> s where
  get :: m s
  get = state \s -> (s, return s)

  put :: s -> m ()
  put s = modify \_ -> s

  modify :: (s -> s) -> m ()
  modify f = state \s -> (f s, return ())

  state :: (s -> (s, m a)) -> m a

class Monad m => MonadMaybe m where
  fail :: m a
  runMaybe :: m (Maybe a) -> m a

instance Monad m => MonadReader e (ReaderT e m) where
  local f (ReaderT mx) = ReaderT \e -> mx (f e)
  reader k = ReaderT \e -> runReaderT (k e) e

instance Monad m => MonadState s (StateT s m) where
  state k = StateT \s0 -> let (s1, StateT f) = k s0 in f s1

readsAndGets :: (MonadReader e m, MonadState s m) => m (e, s)
readsAndGets = do
  e <- ask
  s <- get
  return (e, s)

instance MonadState s m => MonadState s (ReaderT e m) where
  state k = ReaderT \e -> state \s0 -> let (s1, ReaderT f) = k s0 in (s1, f e)

instance MonadReader e m => MonadReader e (StateT s m) where
  local f (StateT mx) = StateT \s0 -> local f (mx s0)
  reader k = StateT \s0 -> reader \e -> runStateT (k e) s0

-- n transformers, m classes => (n * m) instances
-- mtl
-- L1 -> mtl, L2 -> mtl
-- L1 & L2 might not be compatible, needs orphan instances

data ReadsAndGets e s a
  = Leaf a
  | Ask (e -> ReadsAndGets e s a)
  | Get (s -> ReadsAndGets e s a)

readsAndGets' :: ReadsAndGets e s (e, s)
readsAndGets' = Ask \e -> Get \s -> Leaf (e, s)

interpretRG :: Monad m => m e -> m s -> ReadsAndGets e s a -> m a
interpretRG _  _  (Leaf x) = return x
interpretRG ak gt (Ask k)  = ak >>= \e -> interpretRG ak gt (k e)
interpretRG ak gt (Get k)  = gt >>= \s -> interpretRG ak gt (k s)

data Free f a -- free monad!
  = Return a
  | Free (f (Free f a))
  deriving (Functor)

data ReadsAndGetsF e s a
  = AskF (e -> a)
  | GetF (s -> a)
  deriving (Functor)

type ReadsAndGets' e s a = Free (ReadsAndGetsF e s) a

instance Functor f => Applicative (Free f) where
  pure :: a -> Free f a
  pure = Return
  (<*>) = ap

instance Functor f => Monad (Free f) where
  (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  Return x >>= k = k x
  Free fx >>= k = Free (fmap (>>= k) fx)

data WriteF s a = PutF s a deriving (Functor)
type Writes s a = Free (WriteF s) a

data (:+:) f g a = InL (f a) | InR (g a) deriving (Functor)
type ReadsAndGetsAndWrites e s a = Free (ReadsAndGetsF e s :+: WriteF s) a

interpret :: Monad m => (forall b . f b -> m b) -> Free f a -> m a
interpret _   (Return x) = return x
interpret ops (Free f)   = ops f >>= interpret ops

-- Monad m => (f b -> m b) -> Free f a -> m a?
incorrect :: Monad m => (f b -> m b) -> Free f a -> m a
incorrect _ (Return x) = return x
incorrect _ (Free _) = error "INCORRECT" -- ops f >>= interpret ops
incorrect' :: forall m f b a. Monad m => (f b -> m b) -> Free f a -> m a
incorrect' = incorrect

interpret' :: forall m f a. Monad m => (forall b . f b -> m b) -> Free f a -> m a
interpret' = interpret

-- forall x. (forall y. P(x, y)) -> Q(x)
-- forall x, y. P(x, y) -> Q(x)

identity :: forall x. x -> x
identity x = x
-- only ONE function with this type!
first :: forall x y. (x, y) -> x
first (x, _) = x

two :: forall x. x -> x -> x
two x _ = x
two' :: forall x. x -> x -> x
two' _ x = x

