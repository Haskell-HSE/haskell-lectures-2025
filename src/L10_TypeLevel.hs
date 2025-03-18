

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}


module L10_TypeLevel where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Kind (Type)
import GHC.Generics ((:*:) ((:*:)))
import GHC.TypeLits (Symbol, type (+), type (-))
import Numeric.Natural (Natural)


-- Associated types

-- why not (c :: Type -> Type -> Type)?
class Collection c k v | c -> k, c -> v where
  empty :: c -- []
  singleton :: k -> v -> c -- [(k, v)]
  get :: c -> k -> Maybe v
  insert :: k -> v -> c -> c -- + (k, v)

-- instance Collection a b c

instance Collection (IntMap v) Int v where
  empty = IM.empty
  singleton = IM.singleton
  get = (IM.!?)
  insert = IM.insert

------------------------------------------------

c :: ((a, b) -> c) -> a -> b -> c
c = curry

u :: (a -> b -> c) -> ((a, b) -> c)
u = uncurry

-- what we want:
-- uncurryN (\x y z w a -> ...) => (\(x, y, z, w, a) -> ...)

class Curried b a where
  curryN :: a -> b
  uncurryN :: b -> a

-- instance Curried (a -> b -> c) ((a, b) -> c) where
-- instance Curried (a -> b -> c -> d) ((a, b, c) -> d) where
-- ...

-- instance Curried a b => Curried (e -> a) (... b, but one more argument in tuple ...)
instance Curried a (b -> c) => Curried (e -> a) ((e, b) -> c) where
  curryN g e = curryN (\b -> g (e, b))
  uncurryN f (e, b) = uncurryN (f e) b

instance Curried (a -> b) (a -> b) where
  curryN = id
  uncurryN = id

x :: (Int, Int) -> Int
x = uncurryN ((+) :: Int -> Int -> Int)

--------------------------------------------------------------------------------

five :: Num a => a
five = 5

--class From a where
--  type Const a :: Type
--  from :: Const a -> a

--instance {-# OVERLAPPABLE #-} Num a => From a where
--  type Const a = Integer
--  from = fromInteger

--instance {-# OVERLAPPING #-} From Text where
--  type Const Text = String
--  from = T.pack

--instance {-# OVERLAPPING #-} From String where
--  type Const String = String
--  from = id

-----------------------------------------------------------

class Generic' a where
  type Rep' a :: Type -> Type
  fromRep :: Rep' a x -> a
  toRep :: a -> Rep' a ()

instance (Generic' x, Generic' y) => Generic' (x, y) where
  type Rep' (x, y) = Rep' x :*: Rep' y
  fromRep (rx :*: ry) = (fromRep rx, fromRep ry)
  toRep (a, b) = toRep a :*: toRep b

--class GenericF f a | a -> f where
--  fromRepF :: f x -> a
--  toRepF :: a -> f ()

-- instance (GenericF f a, GenericF g b) => GenericF (f :*: g) (a, b) where

------------------------------------------------------------

type family ArgumentsTuple (f :: Type) :: Type where
  ArgumentsTuple (a -> b) = (a, ArgumentsTuple b)
  ArgumentsTuple a = ()

type Fun = Int -> Int -> String -> IntMap Int -> IO ()

args :: ArgumentsTuple Fun
args = (1, (0, ("", (IM.empty, ()))))

type family ArgList (f :: Type) :: [Type] where
  ArgList (a -> b) = a : ArgList b
  ArgList a = '[] -- punning

type family ListToTuple (xs :: [Type]) :: Type where
  ListToTuple '[] = ()
  ListToTuple '[x] = x
  ListToTuple '[x, y] = (x, y)
  ListToTuple '[x, y, z] = (x, y, z)
  ListToTuple '[x, y, z, w] = (x, y, z, w)
  ListToTuple (x : xs) = (x, ListToTuple xs)

args' :: ListToTuple (ArgList Fun)
args' = (1, 0, "", IM.empty)

newtype Kek = MkKek String

type family OnKek (k :: Kek) :: Type where
  OnKek (MkKek '[]) = Int
  OnKek (MkKek '[c]) = String

type Sym = "12345" :: Symbol

type One = 1 :: Natural

data Vec (n :: Natural) (a :: Type) where
  Empty :: Vec 0 a
  Cons :: a -> Vec (n - 1) a -> Vec n a

safeHead :: Vec (n + 1) a -> a
safeHead Empty = error "impossible"
safeHead (Cons h _) = h

safeTail :: Vec n a -> Vec (n - 1) a
safeTail Empty = error "impossible"
safeTail (Cons _ t) = t

vec :: Vec 2 Char
vec = Cons 'a' (Cons 'b' Empty)

safeZip :: Vec n a -> Vec n b -> Vec n (a, b)
safeZip Empty Empty = Empty
safeZip (Cons h t) (Cons h' t') = Cons (h, h') (safeZip t t')
safeZip _ _ = error "impossible"

data PeanoNat = Zero | Succ PeanoNat

data Vec' (n :: PeanoNat) (a :: Type) where
  Empty' :: Vec' Zero a
  Cons' :: a -> Vec' n a -> Vec' (Succ n) a

safeHead' :: Vec' (Succ n) a -> a
safeHead' (Cons' h _) = h

safeTail' :: Vec' (Succ n) a -> Vec' n a
safeTail' (Cons' _ t) = t

safeZip' :: Vec' n a -> Vec' n b -> Vec' n (a, b)
safeZip' Empty' Empty' = Empty'
safeZip' (Cons' h t) (Cons' h' t') = Cons' (h, h') (safeZip' t t')

-- example = safeHead' Empty'
example' :: Vec' Zero (a, b)
example' = safeZip' Empty' Empty'
-- example'' = safeZip' Empty' (Cons' 1 Empty')

onEmpty' :: Vec' Zero a -> ()
onEmpty' Empty' = ()
