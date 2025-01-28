```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module L2_Laws where
```

# Typeclass laws

In the previous lecture, we introduced typeclasses as Haskell's analogue of
interfaces. Sometimes, good documentation of an interface explicitly describes a
set of invariants any implementation should uphold; in the case of Haskell, it
is a common rule of good manners to provide _typeclass laws_ for any typeclass
you define. For example, `Semigroup` and `Monoid` classes from Haskell's
standard library are defined like this:

```haskell ignore
-- | The class of semigroups (types with an associative binary operation).
--
-- Instances should satisfy the following:
--
-- [Associativity] @x '<>' (y '<>' z) = (x '<>' y) '<>' z@
class Semigroup a where
        -- | An associative operation.
        --
        -- >>> [1,2,3] <> [4,5,6]
        -- [1,2,3,4,5,6]
        (<>) :: a -> a -> a

-- | The class of monoids (types with an associative binary operation that
-- has an identity). Instances should satisfy the following:
--
-- [Right identity] @x '<>' 'mempty' = x@
-- [Left identity]  @'mempty' '<>' x = x@
class Semigroup a => Monoid a where
        -- | Identity of '<>'
        --
        -- >>> "Hello world" <> mempty
        -- "Hello world"
        mempty :: a
```

An instance of a typeclass which abides its laws is called a "lawful instance".
While nothing stops you from making a non-lawful instance, you should
nevertheless try to make your instances lawful because typeclass laws usually
capture an important intuition about an interaction between operations inside a
class: for example, we can safely omit parentheses while working with a lawful
semigroup instance, but with non-lawful instance we cannot.

__Exercise 1__: define a function `foldList :: Monoid a => [a] -> a` which
reduces a list using methods from `Monoid` typeclass. Which laws did you
implicitly rely on during the implementation?

__Exercise 2__: some types can be viewed as a monoid in more than one way, e.g.
both addition and multiplication on numbers. In such cases we often define
`newtype`s and make those instances of `Monoid`. Implement `Monoid` instances
for these newtypes defined below. What is needed of `Num a` instance for Monoid
laws to hold?

```haskell
newtype Sum a = Sum { getSum :: a }
  deriving newtype Num

instance Num a => Semigroup (Sum a) where
  (<>) :: Sum a -> Sum a -> Sum a
  Sum x <> Sum y = Sum (x + y)
  -- x <> y = x + y

instance Num a => Monoid (Sum a) where
  mempty = 0

newtype Mul a = Mul { getMul :: a }
  deriving newtype Num

instance Num a => Semigroup (Mul a) where
  (<>) = (*)

instance Num a => Monoid (Mul a) where
  mempty = 1
```

__Exercise 3__. Using newtypes above and the instances you've written, define
functions `sumList :: Num a => [a] -> a` and `productList :: Num a => [a] -> a`
via `foldList` defined before.

Solutions to all exercises are given at the end of this file.

## Endofunctor class hierarchy

### Foldable

Typeclasses are actually more powerful than their counterparts in other
languages. Most notably, we can define typeclasses which describe an interface
of a generic type instead of a concrete type. For example, a `Foldable` class
describes a type of generic collections whose contents can be folded with a
monoid operation:

```haskell ignore
class Foldable (t :: Type -> Type) where
  foldMap :: Monoid b => (a -> b) -> t a -> b
  foldr :: (a -> b -> b) -> b -> t a -> b

instance Foldable [] where
  foldMap :: Monoid b => (a -> b) -> [a] -> b
  foldMap f xs = foldList (map f xs)

toList :: Foldable f => f a -> [a]
```

__Exercise 4__. Make the following tree type into `Foldable` by writing down an
instance for it:

```haskell
data BinTree a = Leaf | Branch (BinTree a) a (BinTree a)
```

### Functor

Just like with folding, a capability of mapping over elements of a container is
captured in a typeclass:

```haskell ignore
class Functor (f :: Type -> Type) where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap = map
```

The Functor laws capture an important property that mapping over elements should
not change an overall internal structure of a container. In other words,

1. `fmap id x = x` (Identity);
2. `fmap g (fmap f x) = fmap (g . f) x` (Composition).

__Exercise 5__. Write out a lawful instance of `Functor BinTree`.

However, Functor laws are so strict that, if you can write out a lawful
instance, every other possible instance would behave the same. Luckily for us,
we can derive such an instance automatically. For example, for `BinTree` we
could do the following:

```haskell ignore
data BinTree a = Leaf | Branch (BinTree a) a (BinTree a) deriving (Functor)
```

### Applicative

Actually, there are things more powerful than functors: applicative functors.

```haskell ignore
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
```

As notation and types are a little tricky, let us explain them bit by bit.

* At first sight, applicative functors can be seen as a generalization from
  lifting unary functions (`fmap`) to lifting functions of arbitrary arity:
  `pure x` is a lifting of an 0-ary function `x`; `liftA2` (read as lift
  applicative binary) lifts binary functions, and each and every `liftAN` can
  be written out in terms of `pure` and `(<*>)`:

```haskell ignore
liftA1 :: Applicative f => (a -> b) -> f a -> f b
liftA1 g fa = pure g <*> fa

(<$>) :: Applicative f => (a -> b) -> f a -> f b
(<$>) = liftA1

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 g fa fb = g <$> fa <*> fb

liftA3 ::
  Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 g fa fb fc = g <$> fa <*> fb <*> fc
```

  Note that in doing so we showed that every applicative functor (that is,
  a type implementing `pure` and `(<*>)`) is a functor.

* On the other hand, applicative functor is a functor which can __apply__
  functions from inside of it via `(<*>)`, here's why it is called an
  __Applicative__.

If `liftA2` can be implemented using `pure` and `(<*>)`, why is it included in
the class declaration? The reason for this is that `(<*>)` can be implemented
using `liftA2`:

```haskell ignore
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
(<*>) = liftA2 (\g x -> g x)
```

Applicative laws:

1. `pure id <*> v = v` (Identity);
2. `pure f <*> pure x = pure (v x)` (Homomorphism);
3. `u <*> pure y = pure (\f -> f y) <*> u` (Interchange);
4. `u <*> (v <*> w) = ((pure (.) <*> u) <*> v) <*> w` (Composition);
5. `liftA2 g u v = pure g <*> u <*> v` (relation with `liftA2`);
6. `fmap = liftA1` (relation with Functor).

Intuitively:

* `pure` must not provide any additional information meaningful to `(<*>)` (so
  here's the source of the name: it should encapsulate a __pure__ value and do
  nothing more);
* the merging of internal structure performed in `(<*>)` should be associative.

__Exercise 6__: show that functor laws hold automatically for every lawful
applicative.

### Monad

Now, the most notable typeclass of all is a Monad. What is this?

```haskell ignore
class Applicative m => Monad m where
  return :: a -> m a
  return = pure

  (>>=) :: m a -> (a -> m b) -> m b

  (>>) :: m a -> m b -> m b
  mx >> my = mx >>= (\_ -> my)
```

As we mentioned before, this is an interface most useful to express effectful
operations: `return x` is a pure result of otherwise effectful computation;
`(>>=)` and `(>>)` both execute operations in order, where in `(>>=)` (also
called "bind") (in other languages called `then`, `and_then`, `flatMap`,...)
a second computation can depend on the outcome of the first.

Turns out that it is a proper subclass of Applicative, meaning that `(<*>)` can
be written out in terms of `return` and `(>>=)`:

```haskell
-- import Control.Monad (ap)
ap :: Monad m => m (a -> b) -> m a -> m b
ap mf mx = mf >>= \f -> mx >>= \x -> return (f x)
```

(This is already defined in standard library, in module `Control.Monad`. Don't
be afraid to use Hoogle to find such combinators!)

A little insight into the way these typeclasses could be discovered. Starting
from `Functor f`, every class introduces new capabilities for lifting of
functions into `f`:

* `fmap` is `(a -> b)` to `(f a -> f b)`
* `(<*>)` is `f (a -> b)` to `(f a -> f b)`
* `flip (>>=)` is `(a -> f b)` to `(f a -> f b)`

`(f a -> f b)` to `(f a -> f b)` is `id`; what about this?

```haskell ignore
whatIsThis :: (f a -> b) -> (f a -> f b)
```

Actually, this is a method from `Comonad` interface, which has its own
interesting applications. However, they are out of scope for now.

Monad laws:

1. `return a >>= k = k a` (Left identity);
2. `m >>= return = m` (Right identity);
3. `(m >>= k) >>= h = m >>= (\x -> k x >>= h)` (Associativity).

After squinting a little, this looks like monoid laws. With additional
technical work, Monad actually looks like a proper "monoid in the category of
endofunctors", whatever this category-theoretic talk means. However, you do not
need this arcane knowledge to program in Haskell: just learn the laws and the
interface itself.

# Exercise solutions

```haskell
foldList :: Monoid a => [a] -> a
foldList [] = mempty
foldList (x : xs) = x <> foldList xs

--instance Num a => Semigroup (Sum a) where
--  Sum x <> Sum y = Sum (x + y)

--instance Num a => Monoid (Sum a) where
--  mempty = Sum 0

--instance Num a => Semigroup (Mul a) where
--  Mul x <> Mul y = Mul (x * y)

--instance Num a => Monoid (Mul a) where
--  mempty = Mul 1

sumList :: Num a => [a] -> a
sumList = getSum . foldList . map Sum

productList :: Num a => [a] -> a
productList = getMul . foldList . map Mul

instance Foldable BinTree where
  foldMap _ Leaf = mempty
  foldMap f (Branch l x r) = foldMap f l <> f x <> foldMap f r

instance Functor BinTree where
  fmap _ Leaf = Leaf
  fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)
```

Solution for __Exercise 6__:

```haskell ignore
fmap id x = liftA1 id x = pure id <*> x = x

fmap g (fmap f x) = liftA1 g (liftA1 f x)
  = pure g <*> (pure f <*> x)
  = ((pure (.) <*> pure g) <*> pure f) <*> x
  = (pure ((.) g) <*> pure f) <*> x
  = pure ((.) g f) <*> x
  = pure (g . f) <*> x
  = liftA1 (g . f) x
  = fmap (g . f) x
```

# Sources

[Typeclassopedia](https://wiki.haskell.org/Typeclassopedia) contains a thorough
description of the most ubiquitous typeclasses together with explanation of
their laws and with more exercises.
