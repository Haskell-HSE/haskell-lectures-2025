# Haskell 2024 - Intro

## What is Haskell?

As written in Wikipedia:

Haskell (/ˈhæskəl/) is a general-purpose, statically-typed, purely functional
programming language with type inference and lazy evaluation. Designed for
teaching, research, and industrial applications, Haskell has pioneered a number
of programming language features such as type classes, which enable type-safe
operator overloading, and monadic input/output (IO). It is named after logician
Haskell Curry. Haskell's main implementation is the Glasgow Haskell Compiler
(GHC).

## Where is Haskell used?

Haskell is used in academia and in industry:

* The proof assistant Agda is written in Haskell.
* Darcs is a revision control system written in Haskell, with several innovative
  features, such as more precise control of patches to apply.
* GHC is also often a testbed for advanced functional programming features and
  optimizations in other programming languages.
* Git-annex is a tool to manage (big) data files under Git version control. It
  also provides a distributed file synchronization system (git-annex assistant).
* Linspire Linux chose Haskell for system tools development.
* Pandoc is a tool to convert one markup format into another.
* Pugs is a compiler and interpreter for the Raku programming language
  (formerly Perl 6).
* TidalCycles is a domain special language for live coding musical patterns,
  embedded in Haskell.
* Xmonad is a window manager for the X Window System written fully in Haskell.
* GarganText is a collaborative tool to map through semantic analysis texts on
  any web browser, written fully in Haskell and PureScript, which is used for
  instance in the research community to draw up state-of-the-art reports and
  roadmaps.

### Industry

* Bluespec SystemVerilog (BSV) is a language for semiconductor design that is an
  extension of Haskell. Also, Bluespec, Inc.'s tools are implemented in Haskell.
  Note this is an example of a DSL embedded into Haskell.
* Cryptol, a language and toolchain for developing and verifying cryptography
  algorithms, is implemented in Haskell.
* Facebook implements its anti-spam programs in Haskell, maintaining the
  underlying data access library as open-source software.
* The Cardano blockchain platform is implemented in Haskell.
* GitHub implemented Semantic, an open-source library for analysis, diffing, and
  interpretation of untrusted source code, in Haskell.
* Standard Chartered's financial modelling language Mu is syntactic Haskell
  running on a strict runtime.
* seL4, the first formally verified microkernel, used Haskell as a prototyping
  language for the OS developer. At the same time, the Haskell code defined an
  executable specification with which to reason, for automatic translation by
  the theorem-proving tool. The Haskell code thus served as an intermediate
  prototype before final C refinement.
* Target stores' supply chain optimization software is written in Haskell.
* Co–Star

### Some more examples from Serokell

* Hasura is an open-source GraphQL engine that gives you instant access to a
  GraphQL API for your data. In the GitHub repository of Hasura, Haskell is the
  most used language, followed by languages like TypeScript, JavaScript, Python,
  and Go. At the start of 2022, Hasura raised \$100 million.
* Microsoft uses Haskell in Bond, a cross-platform framework for working with
  schematized data. The Haskell part of Bond is mainly gbc, a command-line code
  generation tool that can generate C++ and C# code using Bond. For a long time,
  Microsoft Research also significantly supported the Glasgow Haskell Compiler
  (GHC) by employing Simon Peyton Jones to work on it.
* Tesla has been regularly hiring Haskell engineers and interns for some years
  already. There isn’t a lot of information out there about the exact specifics
  of their work. According to a comment in a job post made on Reddit, they use
  Haskell to generate C code that is then compiled into vehicle firmware.
* Freckle is an online learning platform that adjusts to the skills of students.
  They use Haskell for all of their backend services and most of their tooling.
* Galois is a company that specializes in the research and development of
  trustworthy software systems for fields where failure is unacceptable. One of
  their most interesting projects is Copilot, a stream-based DSL for writing and
  monitoring embedded C programs.
* Serokell uses Haskell to great success for most of their day-to-day software
  development projects. They have used it to build the settlement layer of the
  Cardano blockchain, programming languages for the Tezos blockchain,
  cryptocurrency exchanges, a multi-currency wallet with its own DSL and more.

## What are the benefits of using Haskell?

Companies above list three things as the most significant benefits of Haskell:

1. Expressive type system.
2. Strong correctness guarantees.
3. Easy refactoring.

In addition, while Haskell is a great programming language for everyday
purposes, we can separate two popular use cases:

1. Complex systems where strong guarantees are required (such as blockchains
   like Cardano and fintech projects like Klarna).
2. The second is compilers and DSLs. On our list, the examples for this use case
   are projects like Bond, Semantic, Copilot.

Both of these use cases heavily benefit from the strengths of Haskell and manage
to dodge most of its cons. They can rightfully be called the “killer apps” of
Haskell.

If you are considering a project in these two areas, it’s hard to go wrong with
picking Haskell.

# Basic language constructs

This file is actually a markdown file with a postprocessor run on it to extract
haskell code blocks like these:

```haskell ignore
a = 5
```

Normal code is written in normal source code files with extension `.hs` called
**Haskell modules**. This one is also a module, although a **Literate Haskell**
one: it has an extension `.lhs`. Each module starts with compiler pragmas
(if they're used) and a module name:

```haskell
{-# OPTIONS_GHC -Wno-empty-enumerations #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module L1_Intro where
```

(Compiler pragmas above disable some warnings as we're first going to show some
antipatterns here, sadly)

To write a comment, a double dash is used:

```haskell
-- This is a comment inside code block in a literate Haskell file.
{- And this is a multiline comment.
   A little useless, isn't it? -}
```

## Primitive Datatypes and Operators

Of course, Haskell has numbers, booleans, characters, strings and whatnot:

```haskell
a = 5
b = a + 1 -- 6
c = 3 - 2 -- 1
d = 10 * 2 -- 20
e = 35 / 5
```

However, if you load this file into a Haskell interpreter called `ghci`, you'll
see the following:

```
~/haskell-lectures-2024 $ ghci 1.Intro.lhs
Loaded package environment from /home/turtle/.ghc/x86_64-linux-9.4.8/environments/default
GHCi, version 9.4.8: https://www.haskell.org/ghc/  :? for help
package flags have changed, resetting and loading new packages...
Loaded GHCi configuration from /home/turtle/haskell-2024/haskell-lectures-2024/.ghci
[1 of 2] Compiling Main             ( 1.Intro.lhs, interpreted )
Ok, one module loaded.
ghci> e
7.0
```

So division is not integer division by default. Actual integer division:

```haskell
this_is_eight = 35 `div` 4
```

Boolean operations:

```haskell
f = not True
g = not False
h = True && False
i = True || False
j = 1 == 1
k = 1 /= 1
l = 1 < 10
```

In the above examples, `not` is a function that takes one value. Haskell doesn't
need parentheses for function calls --- all the arguments are just listed after
the function. So the general pattern is: `func arg1 arg2 arg3...` See the
section on functions for information on how to write your own.

Strings and characters:

```haskell
m = "This is a string."
n = 'a'
```

```haskell ignore
o = 'You cant use single quotes for strings.' -- error!
```

Strings can be concatenated:

```haskell
p = "Hello " ++ "world!"
```

A string is a list of characters:

```haskell
q = ['H', 'e', 'l', 'l', 'o']
```

Lists can be indexed with the `!!` operator followed by an index

```haskell
s = "This is a string" !! 0
```

## Lists

Every element in a list must have the same type. These two lists are equal:

```haskell
l1 = [1, 2, 3, 4, 5]
l2 = [1..5]
```

Ranges are versatile.

```haskell
charRange = ['A'..'F'] -- "ABCDEF"
orderingRange = [LT .. GT] -- [LT, EQ, GT]
```

You can create a step in a range.

```haskell
evenStep = [0,2..10] -- [0, 2, 4, 6, 8, 10]
notStepped = [5..1] -- [] (Haskell defaults to incrementing)
decrementStep = [5,4..1] -- [5, 4, 3, 2, 1]
```

Indexing into a list

```haskell
thirdGlyph = [1..10] !! 3 -- 4 (zero-based indexing)
```

You can also have infinite lists in Haskell!

```haskell
nats = [1..] -- a list of all the natural numbers
```

Infinite lists work because Haskell has "lazy evaluation". This means that
Haskell only evaluates things when it needs to. So you can ask for the 1000th
element of your list and Haskell will give it to you:

```haskell
someNat = [1..] !! 999 -- 1000
```

And now Haskell has evaluated elements 1 - 1000 of this list... but the rest of
the elements of this "infinite" list don't exist yet! Haskell won't actually
evaluate them until it needs to.

Joining two lists:

```haskell
joinedLists = [1..5] ++ [6..10]
```

Adding to the head of a list:

```haskell
headCat = 0:[1..5] -- [0, 1, 2, 3, 4, 5]
```

More list operations

```haskell
someHead = head [1..5] -- 1
someTail = tail [1..5] -- [2, 3, 4, 5]
someInit = init [1..5] -- [1, 2, 3, 4]
someLast = last [1..5] -- 5
```

Actually, you can lookup them on [hoogle](https://hoogle.haskell.org/) or, even
better, find them there by their type signature!

List comprehensions

```haskell
comp = [x*2 | x <- [1..5]] -- [2, 4, 6, 8, 10]
```

With a conditional

```haskell
compCond = [x*2 | x <- [1..5], x*2 > 4] -- [6, 8, 10]
```

With several iteratees

```haskell
twoD = take 10 [x ^ 2 * y | x <- [1..], even x, x `mod` 3 == 0, y <- [1, -1]]
```

## Tuples

Every element in a tuple can be a different type, but a tuple has a fixed
length. A tuple:

```haskell
tupleExample = ("haskell", 1)
```

Accessing elements of a pair (i.e. a tuple of length 2)

```haskell
someFst = fst ("haskell", 1) -- "haskell"
someSnd = snd ("haskell", 1) -- 1
```

Pair element accessing does not work on n-tuples (i.e. triple, quadruple, etc)

```haskell ignore
snd ("snd", "can't touch this", "da na na na") -- error! see function below
```

## Functions

A simple function that takes two variables

```haskell
add a b = a + b
```

Using the function

```haskell
three = add 1 2 -- 3
```

You can also put the function name between the two arguments with backticks:

```haskell
alsoThree = 1 `add` 2 -- 3
```

You can also define functions that have no letters! This lets you define your
own operators! Here's an operator that does integer division

```haskell
(//) a b = a `div` b
eight = 35 // 4 -- 8
```

### Guards

An easy way to do branching in functions

```haskell
fib x
  | x < 2 = 1
  | otherwise = fib (x - 1) + fib (x - 2)
```

### Pattern matching

Pattern matching is similar. Here we have given three different equations that
define `fib'`. Haskell will automatically use the first equation whose left hand
side pattern matches the value.

```haskell
fib' 0 = 1
fib' 1 = 1
fib' 2 = 2
fib' x = fib' (x - 1) + fib' (x - 2)
```

### Pattern matching on tuples

```haskell
sndOfTriple (_, y, _) = y -- use a wild card (_) to bypass naming unused value
```

### Pattern matching on lists

Here `x` is the first element in the list, and `xs` is the rest of the list. We
can write our own map function:

```haskell
myMap func [] = []
myMap func (x:xs) = func x:(myMap func xs)
```

Anonymous functions are created with a backslash followed by all the arguments.

```haskell
mapped = myMap (\x -> x + 2) [1..5] -- [3, 4, 5, 6, 7]
```

### Using fold (called `inject` in some languages) with an anonymous function

`foldl1` means fold left, and use the first value in the list as the initial
value for the accumulator.

```haskell
usingFold = foldl1 (\acc x -> acc + x) [1..5] -- 15
```

### Partial application

If you don't pass in all the arguments to a function, it gets "partially
applied". That means it returns a function that takes the rest of the arguments.

```haskell
foo = add 10 -- foo is now a function that takes a number and adds 10 to it
fifteen = foo 5 -- 15
```

Another way to write the same thing. It is called a "section":

```haskell
foo' = (10 +)
alsoFifteen = foo' 5 -- 15
```

### Function composition

The operator `.` chains functions together. For example, here `foo` is a
function that takes a value. It adds 10 to it, multiplies the result of that by
4, and then returns the final value.

```haskell
foo'' = (4 *) . (10 +)
sixty = foo'' 5 -- 4*(10+5) = 60
```

### Fixing precedence

Haskell has an operator called `$`. This operator applies a function to a given
parameter. In contrast to standard function application, which has highest
possible priority of 10 and is left-associative, the `$` operator has priority
of 0 and is right-associative. Such a low priority means that the expression on
its right is applied as a parameter to the function on its left.

Before:

```haskell
evenSeven = even (fib 7) -- false
```

Equivalently:

```haskell
evenSeven' = even $ fib 7 -- false
```

Composing functions:

```haskell
evenSeven'' = even . fib $ 7 -- false
evenSeven''' = even $ fib $ 7
```

## Type signatures

Haskell has a very strong type system, and every valid expression has a type.
Some basic types:

```haskell
five = 5 :: Integer
hello = "hello" :: String
truth = True :: Bool
```

Functions have types too. `not` takes a boolean and returns a boolean:

```haskell
not' = not :: Bool -> Bool
```

Here's a function that takes two arguments:

```haskell
add' = add :: Integer -> Integer -> Integer
```

When you define a value, it's good practice to write its type above it:

```haskell
double :: Integer -> Integer
double x = x * 2
```

### Typed holes

Haskell's type system allows one to use "typed holes" -- you can replace an
expression with an underscore and a compiler would tell you what is the expected
type of a missing expression:

```haskell ignore
double' x = _ * 2 :: Integer
```

## Control Flow and If Expressions

if-expressions

```haskell
haskell = if 1 == 1 then "awesome" else "awful" -- haskell = "awesome"
```

if-expressions can be on multiple lines too, indentation is important

```haskell
haskell' = if 1 == 1
             then "awesome"
             else "awful"
```

Case expressions: Here's how you could parse command line arguments

```haskell
parseArgs args =
  case args of
    "help" -> printHelp
    "start" -> startProgram
    _ -> putStrLn "bad args"

printHelp = undefined -- see Hoogle to learn what this is
startProgram = undefined
```

Haskell doesn't have loops; it uses recursion instead. map applies a function
over every element in a list

```haskell
someEvens = map (*2) [1..5] -- [2, 4, 6, 8, 10]
```

You can make a for function using map

```haskell
for array func = map func array
```

And then use it

```haskell
strings = for [0..5] $ \i -> show i
```

We could've written that like this too:

```haskell
strs = for [0..5] show
```

You can use `foldl` or `foldr` to reduce a list:
`foldl <fn> <initial value> <list>`

```haskell
fortyThree = foldl (\x y -> 2*x + y) 4 [1,2,3] -- 43
```

(This is the same as (2 * (2 * (2 * 4 + 1) + 2) + 3))

While `foldl` is left-handed, `foldr` is right-handed

```haskell
sixteen = foldr (\x y -> 2*x + y) 4 [1,2,3] -- 16
```

(This is now the same as (2 * 1 + (2 * 2 + (2 * 3 + 4))))

## Data Types

A data type is declared with a 'type constructor' on the left and one or more
'data constructors' on the right, separated by the pipe | symbol. This is a
sum/union type. Each data constructor is a (possibly nullary) function that
creates an object of the type named by the type constructor.

This is essentially an enum

```haskell
data Color = Red | Blue | Green -- would be an enum in other languages
```

Now you can use it in a function:

```haskell
say :: Color -> String
say Red   = "You are Red!"
say Blue  = "You are Blue!"
say Green = "You are Green!"
```

Note that the type constructor is used in the type signature and the data
constructors are used in the body of the function. Data constructors are
primarily pattern-matched against.

This next one is a traditional container type holding two fields. In a type
declaration, data constructors take types as parameters. Data constructors can
have the same name as type constructors: this is common where the type only has
a single data constructor, although here we do not do this to reduce confusion
for newcomers.

```haskell
data Point = Cartesian Float Float
```

This can be used in a function like:

```haskell
distance :: Point -> Point -> Float
distance (Cartesian x y) (Cartesian x' y') = sqrt $ dx + dy
     where dx = (x - x') ** 2
           dy = (y - y') ** 2
```

Types can have multiple data constructors with arguments, too

```haskell
data Name = Mononym String
          | FirstLastName String String
          | FullName String String String
```

To make things clearer, we can use record syntax

```haskell
data PointRecord = CartesianPoint2D { x :: Float, y :: Float }
                 | PolarPoint2D { r :: Float, theta :: Float }

myPoint = CartesianPoint2D { x = 7.0, y = 10.0 }
```

Using record syntax automatically creates accessor functions:

```haskell
xOfMyPoint = x myPoint -- 7.0
```

Record syntax also allows a simple form of update

```haskell
myPoint' = myPoint { x = 9.0 } -- CartesianPoint2D { x = 9.0, y = 10.0 }
```

Even if a type is defined with record syntax, it can be declared like a simple
data constructor. This is fine:

```haskell
myPoint'' = CartesianPoint2D 3.3 4.0
```

It's also useful to pattern match data constructors in `case` expressions

```haskell
distanceFromOrigin x =
   case x of (CartesianPoint2D x y) -> sqrt $ x ** 2 + y ** 2
             (PolarPoint2D r _) -> r
```

Your data types can have type parameters too:

```haskell ignore
data Maybe a = Nothing | Just a
```

These are all `Maybe`s, but with different `a`s:

```haskell
justHello = Just "hello"  -- of type `Maybe String`
justOne = Just 1          -- of type `Maybe Int`
nothing = Nothing         -- of type `Maybe a` for any `a`
```

For convenience we can also create type synonyms with the `type` keyword

```haskell
type String' = [Char]
```

Unlike `data` types, type synonyms need no constructor, and can be used anywhere
a synonymous data type could be used. Say we have the following type synonyms
and items with the following type signatures:

```haskell
-- For demonstration purposes, these types have no constructors
data Person
data Circle

-- Types in which we are interested
type Weight = Float
newtype Weight' = Weight' Float
type Height = Float
type Point2D = (Float, Float)

getMyHeightAndWeight :: Person -> (Height, Weight)
getMyHeightAndWeight = undefined

findCenter :: Circle -> Point2D
findCenter = undefined

somePerson :: Person
somePerson = undefined

someCircle :: Circle
someCircle = undefined

distance2D :: Point2D -> Point2D -> Float
distance2D = undefined
```

Then the following would compile and run without issue even though it does not
make sense semantically:

```haskell
nonsense = distance2D (getMyHeightAndWeight somePerson) (findCenter someCircle)
```

## Typeclasses

Typeclasses are one way Haskell does polymorphism. They are similar to
interfaces in other languages: a typeclass defines a set of functions that must
work on any type that is in that typeclass.

The
[Eq](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#t:Eq)
typeclass is for types whose instances can be tested for equality with one
another.

```haskell ignore
class Eq a where
    {-# MINIMAL (==) | (/=) #-}
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
```

This defines a typeclass that requires two functions, `(==)` and `(/=)`. It also
declares that one function can be declared in terms of another, so it is enough
that __either__ the `(==)` function or the `(/=)` is defined and the other will
be "filled in" based on the typeclass definition.

To make a type a member of a type class, the `instance` keyword is used

```haskell
data TrafficLight = TLRed | TLYellow | TLGreen

instance Eq TrafficLight where
    TLRed == TLRed = True
    TLGreen == TLGreen = True
    TLYellow == TLYellow = True
    _ == _ = False
```

(Note that, while declaring `TrafficLight` constructors, we had to name
constructors differently in order for them to not clash with `Color`
constructors we defined earlier. It is common to either prefix constructor
names with type name abbreviation or define types in different modules
altogether)

Now we can use `(==)` and `(/=)` with `TrafficLight` objects:

```haskell
canProceedThrough :: TrafficLight -> Bool
canProceedThrough t = t /= TLRed
```

You can NOT create an instance definition for a type synonym, though.

Functions can be written to take typeclasses with type parameters, rather than
types, assuming that the function only relies on features of the typeclass:

```haskell
isEqual :: Eq a => a -> a -> Bool
isEqual x y = x == y
```

Note that `x` and `y` MUST be the same type, as they are both defined as being
of type parameter `a`. A typeclass does not state that different types in the
typeclass can be mixed together. So `isEqual Red 2` is invalid even though `2`
is an `Int` which is an instance of `Eq`, and `Red` is a `TrafficLight` which is
also an instance of `Eq`.

Other common typeclasses are:

* `Ord` for types that can be ordered, allowing you to use `>`, `<=`, etc.
* `Read` for types that can be created from a string representation
* `Show` for types that can be converted to a string for display
* `Num`, `Real`, `Integral`, `Fractional` for types that can do math
* `Enum` for types that can be stepped through
* `Bounded` for types with a maximum and minimum

### Literals

We used character, string and list literals back from the very start. It is
simple to understand what they mean and how to use them. However, numeric
literals are not so simple. If we ask GHCi: "what is the type of 2?", we get:

```
ghci> :t 2
2 :: Num a => a
```

So numeric literals are actually polymorphic and do not have a concrete type.
Not that this is not the same as casts because the following is not allowed:

```haskell
two = 2 :: Integer
```
```haskell ignore
float = two * 0.5 -- would not pass the type check
```

### Deriving

Haskell can automatically make types part of `Eq`, `Ord`, `Read`, `Show`, `Enum`
and `Bounded` with the `deriving` keyword at the end of the type declaration

```haskell
data PointDerives = Point Float Float deriving (Eq, Read, Show)
```

In this case it is NOT necessary to create an `instance` definition.

## Haskell IO

While IO can't be explained fully without explaining monads, it is not hard to
explain enough to get going.

When a Haskell program is executed, `main` is called. It must return a value of
type `IO a` for some type `a`. For example:

```haskell
main :: IO ()
main = putStrLn $ "Hello, sky! " ++ (say Blue)
```

(`putStrLn` has type `String -> IO ()`)

It is easiest to do IO if you can implement your program as a function from
String to String. The function

```haskell
interact' = interact :: (String -> String) -> IO ()
```

Inputs some text, runs a function on it, and prints out the output.

```haskell
countLines :: String -> String
countLines = show . length . lines

main' = interact countLines
```

You can think of a value of type `IO ()` as representing a sequence of actions
for the computer to do, much like a computer program written in an imperative
language. We can use the `do` notation to chain actions together. For example:

```haskell
sayHello :: IO ()
sayHello = do
  putStrLn "What is your name?"
  name <- getLine -- this gets a line and gives it the name "name"
  putStrLn $ "Hello, " ++ name
```

Exercise: write your own version of `interact` that only reads one line of
input.

If you are running this module in GHCi, it can run any IO action on demand:

```
ghci> sayHello
What is your name?
Pavel
Hello, Pavel
```

Let's understand better how the function `getLine` we just used works. Its type
is:

```haskell
getLine' = getLine :: IO String
```

You can think of a value of type `IO a` as representing a computer program that
will generate a value of type `a` when executed (in addition to anything else it
does). We can name and reuse this value using `<-`. We can also make our own
action of type `IO String`:

```haskell
action :: IO String
action = do
  putStrLn "This is a line. Duh"
  input1 <- getLine
  input2 <- getLine
  -- The type of the `do` statement is that of its last line.
  -- `return` is not a keyword, but merely a function
  return (input1 ++ "\n" ++ input2) -- return :: String -> IO String
```

However, do-notation is not some compiler magic, this is just a syntactic sugar
over two operators called `>>` and `>>=`. The action below is the same as the
action above:

```haskell
action' = putStrLn "This is a line. Duh" >> (getLine >>= (\input1 ->
  getLine >>= (\input2 -> return (input1 ++ "\n" ++ input2))))
```

We can use this just like we used `getLine`:

```haskell
main'' = do
    putStrLn "I will echo two lines!"
    result <- action
    putStrLn result
    putStrLn "This was all, folks!"
```

The type `IO` is an example of a "monad". The way Haskell uses a monad to do IO
allows it to be a purely functional language. Any function that interacts with
the outside world (i.e. does IO) gets marked as `IO` in its type signature. This
lets us reason about which functions are "pure" (don't interact with the outside
world or modify state) and which functions aren't.

This is a powerful feature, because it's easy to run pure functions
concurrently; so, concurrency in Haskell is very easy.

## Monads

Actually, `do`-notation has wider application than only constructing IO values.
Is is applicable for every _monad_:

```
ghci> :i Monad
type Monad :: (* -> *) -> Constraint
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
```

In other words, `m` is a monad if it allows to construct "pure" values of type
`m a` from any value of type `a` and if it allows to sequence actions of type
`m a` and `m b` and yield a result of type `m b`, where the second action either
can (`(>>=)`) or cannot (`(>>)`) depend on the result of the first action.

This is discussed more in detail in the next lecture.

# Sources & References

* [Wikipedia page on Haskell](https://en.wikipedia.org/wiki/Haskell)
* [Serokell blog post on Haskell in industry](https://serokell.io/blog/top-software-written-in-haskell)
* [Memo for basic Haskell constructions](https://learnxinyminutes.com/docs/haskell/)
* [Hoogle, Haskell code search](https://hoogle.haskell.org/)
