{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use =<<" #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE LambdaCase #-}




module L06_Transformers where
import Control.Monad (join, ap, MonadPlus)
import Control.Applicative (Alternative (..))


-- data Maybe a = Nothing | Just
-- data Either e a = Left e | Right a
-- newtype State s a = State (s -> (s, a))
-- data IO a = ...

-- newtype Parser a = Parser (String -> Maybe (String, a))

-- f, g
-- f (g a)

newtype Comp f g a = Comp { runComp :: f (g a) }

type EitherIO a = Comp (Either String) IO a

instance (Functor f, Functor g) => Functor (Comp f g) where
  fmap :: (a -> b) -> Comp f g a -> Comp f g b
  fmap f = Comp . fmap (fmap f) . runComp
  -- ^ Can be checked that satisfies functor laws

instance (Applicative f, Applicative g) => Applicative (Comp f g) where
  pure :: a -> Comp f g a
  pure = Comp . pure . pure

  liftA2 :: (a -> b -> c) -> Comp f g a -> Comp f g b -> Comp f g c
  liftA2 f (Comp fga) (Comp fgb) = Comp (liftA2 (liftA2 f) fga fgb)
  -- ^ Also satisfies laws!

--instance (Monad f, Monad g) => Monad (Comp f g) where
--  (>>=) :: Comp f g a -> (a -> Comp f g b) -> Comp f g b
--  Comp fga >>= k = Comp (fga >>= \ga -> let r = ga >>= _ in _)

-- join :: f (f a) -> f a
-- joinComp :: Comp f g (Comp f g a) -> Comp f g a
-- joinComp :: (Monad f, Monad g) => f (g (f (g a))) -> f (g a)
-- joinComp = _ -- ?
--
-- MONADS DO NOT COMPOSE!!!
-- => Effect systems

joinSwap ::
  (Monad f, Monad g) => (forall x. g (f x) -> f (g x)) ->
  f (g (f (g a))) -> f (g a)
joinSwap s = fmap join . join . fmap s

newtype StateT s m a = StateT { runStateT :: s -> m (s, a) }
  deriving (Functor)

instance Monad m => Applicative (StateT s m) where
  pure x = StateT \s -> pure (s, x)
  (<*>) = ap

instance MonadPlus m => Alternative (StateT s m) where
  empty :: StateT s m a
  empty = StateT \_ -> empty
  StateT sx <|> StateT sy = StateT \s -> sx s <|> sy s

instance Monad m => Monad (StateT s m) where
  StateT sx >>= k = StateT \s0 -> do
     (s1, x) <- sx s0
     runStateT (k x) s1

instance MonadPlus m => MonadPlus (StateT s m)

type Parser a = StateT String Maybe a

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
  deriving (Functor)
                                          -- Maybe (m a) is not quite right

instance Applicative m => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure = MaybeT . pure . pure

  liftA2 :: (a -> b -> c) -> MaybeT m a -> MaybeT m b -> MaybeT m c
  liftA2 f (MaybeT ma) (MaybeT mb) = MaybeT (liftA2 (liftA2 f) ma mb)

instance Monad m => Monad (MaybeT m) where
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  MaybeT mx >>= k = MaybeT $ mx >>= \case
    Just a -> runMaybeT (k a)
    Nothing -> pure Nothing

newtype Reader e a = Reader { runReader :: e -> a }
-- instance Monad ((->) e)

newtype ReaderT e m a = ReaderT { runReaderT :: e -> m a }
  deriving (Functor)

instance Applicative m => Applicative (ReaderT e m) where
  pure :: a -> ReaderT e m a
  pure x = ReaderT \_ -> pure x

  (<*>) :: ReaderT e m (a -> b) -> ReaderT e m a -> ReaderT e m b
  ReaderT mf <*> ReaderT mx = ReaderT \e -> mf e <*> mx e

instance Monad m => Monad (ReaderT e m) where
  (>>=) :: ReaderT e m a -> (a -> ReaderT e m b) -> ReaderT e m b
  ReaderT mx >>= k = ReaderT \e ->
    mx e >>= \x -> runReaderT (k x) e

-- MONAD TRANSFORMERS

type Lol a = ReaderT [String] (StateT Int IO) a

kek :: Lol a -> (a -> Lol b) -> Lol b
kek = (>>=)

-- Typeclassopedia has section on monad transformers:
-- https://wiki.haskell.org/Typeclassopedia#Monad_transformers





