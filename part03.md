[Contents](index.md) | [P1](part01.md) | [P2](part02.md) | **P3** | [P4](part04.md)

# I/O

* We want to write useful programs
* Practical programs typically need to interact with the outside world
* Otherwise all they're doing is warming up your CPU
* Most other programming languages allow subroutines to directly perform arbitrary input and output or interaction with the outside world
* But what about purely functional programming languages?
    * Mathematical functions cannot read files from disc or write to the screen
    * Mathematical functions can only return values that are functions of their arguments or other pure functions
* There are several different solutions to this problem
* Haskell uses what I will refer to as the continuation-passing style, though other people will use other terms

# The Prelude

* Haskell defines many standard functions in what is known as the "Prelude"
* This is a module that is implicitly imported into every Haskell source file, unless this is explicitly disabled
* We'll use a few Prelude functions to explore I/O in Haskell

# Read a number and do something to it

## The `print` function

* We've already seen this once or twice
* Let's look at the details:

```ghci
λ> :t print
print :: Show a => a -> IO ()
```

* This has a type constraint of `Show a`
* `a` is a type variable again
* `Show` is a type class
* For the sake of this discussion, `Show` is a type class with a single "method" `show`:

```ghci
λ> :t show
show :: Show a => a -> String
```

* A function on a type class is commonly referred to as a "method" by analogy with "method" on a class or interface in object-oriented programming
* `show` takes an object of some type `a` and returns a `String`
* Haskell's primitive types implement `show` and typically return a human-readable representation of a value
* So, what's `IO`?
* Well, clearly it's a type class much like `Num` etc.
* What's `()`?
* It's pronounced "unit"
* It's the single inhabitant of the [unit type][unittype]
* The unit type is a type that allows only a single value which conveys no information
* Not to be confused with the zero or bottom type which has no values or inhabitants
* So, this is a function that takes something of type `a` (subject to the `Show a` constraint) and returns `IO ()`

## `>>=` a.k.a. "bind"

* Let's look at the `IO` type class some more
* It's not a type, so let's use `:i` in GHCI

```ghci
λ> :i IO
newtype IO a
  = ghc-prim-0.5.0.0:GHC.Types.IO (ghc-prim-0.5.0.0:GHC.Prim.State#
                                     ghc-prim-0.5.0.0:GHC.Prim.RealWorld
                                   -> (# ghc-prim-0.5.0.0:GHC.Prim.State#
                                           ghc-prim-0.5.0.0:GHC.Prim.RealWorld,
                                         a #))
  	-- Defined in ‘ghc-prim-0.5.0.0:GHC.Types’
instance Monad IO -- Defined in ‘GHC.Base’
instance Functor IO -- Defined in ‘GHC.Base’
instance Applicative IO -- Defined in ‘GHC.Base’
instance Monoid a => Monoid (IO a) -- Defined in ‘GHC.Base’
```

* This tells you that `IO` has instances for four other type classes, namely `Monad`, `Functor`, `Applicative` and `Monoid`

```ghci
λ> :i Monad
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  fail :: String -> m a
  {-# MINIMAL (>>=) #-}
  	-- Defined in ‘GHC.Base’
instance Monad (Either e) -- Defined in ‘Data.Either’
instance Monad [] -- Defined in ‘GHC.Base’
instance Monad Maybe -- Defined in ‘GHC.Base’
instance Monad IO -- Defined in ‘GHC.Base’
instance Monad ((->) r) -- Defined in ‘GHC.Base’
```

* Since `IO` has an instance for `Monad`, it provides an implementation of "method" `>>=`, pronounced "bind":

```ghci
λ> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

* Note that functions incorporating symbols in their names will, under certain circumstances, require surrounding parentheses both in GHCI and Haskell source code
* Specializing from `Monad` to `IO`, `>>=` is a function that takes `IO a`, where `a` is a type variable, a function from `a` to `IO b` and evaluates to an `IO b`
* Try out `print` by itself:

```ghci
λ> print 5
5
λ> :t print 5
print 5 :: IO ()
λ> print "hello"
"hello"
λ> :t print "hello"
print "hello" :: IO ()
```

* It works on primitive types
* Note that these are two separate statements in GHCI
* Let's see if we can combine them into a single expression using `>>=`
* Given a `IO ()`, we need a function that takes unit and returns `IO b`
* `b` can be unit too
* So let's build such a function using `print "hello"` as the return value and call it `f` for now:

```ghci
λ> f :: () -> IO (); f x = print "hello"
λ> :t f
f :: () -> IO ()
```

* Now, we'll combine it with `print 5`:

```ghci
λ> print 5 >>= f
5
"hello"
```

## Anonymous functions a.k.a. lambdas

* Our function `f` is great
* We use it exactly once
* So do we really have to give it a name?
* Absolutely not!
* This is known as a "lambda" and we can assign them since they are values just like `5` or `"hello"`:

```ghci
λ> g :: () -> IO (); g = \x -> print "hello"
λ> :t g
g :: () -> IO ()
```

* This is pretty much indistinguishable from `f` defined previously
* Instead of assigning to a name `g` we can use the right-hand side directly:

```ghci
λ> print 5 >>= \x -> print "hello"
5
"hello"
λ> :t print 5 >>= \x -> print "hello"
print 5 >>= \x -> print "hello" :: IO ()
```

* Now we have single expression whose type is `IO ()`
* Let's get rid of all the extraneous names
* `_` is a "throwaway" name and you'll see it used a lot

```ghci
λ> print 5 >>= \_ -> print "hello"
5
"hello"
```

* What they called [lambdas][lambdas]?

## Let's put it in a source file

```haskell
module Main where

main :: IO ()
main = print 5 >>= \_ -> print "hello"
```

```console
> stack runhaskell Scratch.hs
5
"hello"
```

## The `read` function

* Let's do some more exploring using GHCI:

```ghci
λ> :t read
read :: Read a => String -> a
```

* This takes `String` and returns `a` where is subject to the `Read a` constraint
* [`Read`][readdoc] is a type class that supports reading a value from a `String`
* Most primitive types in Haskell have instances of `Read`
* Since this is a polymorphic function, we need a type annotation to choose a specific instance of it:

```ghci
λ> :t read :: String -> Integer
read :: String -> Integer :: String -> Integer
```

* This is a function that takes `String` and returns `Integer`
* We can assign this specific instance of `read` to another name and test it out:

```ghci
λ> readInteger :: String -> Integer; readInteger = read
λ> :t readInteger
readInteger :: String -> Integer
λ> readInteger "123"
123
```

## The `getLine` function

* Here's `getline` in all its glory:

```ghci
λ> :t getLine
getLine :: IO String
```

## The `putStr` function

* This one's pretty straightforward

```ghci
λ> :t putStr
putStr :: String -> IO ()
```

## Combine them all

* Here's what we're going to do
    * Print a prompt to the terminal (using `putStrLn`)
    * Get a string from the keyboard (using `getLine`)
    * Read it as an integer (using `read`/`readInteger`)
    * Multiply the integer by 2 (using `*`)
    * Print out the result (using `print`)
* And it will all be a single expression:

```haskell
readInteger :: String -> Integer
readInteger = read

main :: IO ()
main = putStr "Enter a number and I'll double it: "
    >>= \_ -> getLine
    >>= \l -> print (2 * readInteger l)
```

* And let's run it:

```console
> stack runhaskell Scratch.hs
Enter a number and I'll double it: 5
10
```

## Wait a minute!

* Didn't you say that Haskell was purely functional?
* Surely, `putStr`, `getLine` and `print` have side effects
* Well, no not at all
    * They're actions
    * They're strung together using `>>=`
    * At no point do they mutate global state or interact with the outside world
    * We build up an expression of these actions
    * They are "executed" when the program is run
* Did you mention something about continuation passing?
    * Why, yes, I did!
    * That's what `>>=` is doing
    * You provide an action as its first argument
    * And the continuation as the second argument
    * This is the thing that is to be evaluated when the first value is executed at program execution time
* `>>=` is great as it provides the ability to sequence actions (at least when it's used with `IO`)
* All this `>>=` is kinda ugly, though, isn't it?
* Well, yes it is

# Summary

* Explored some common functions from Haskell's Prelude
* Briefly some type classes, particularly `IO`
* Learnt what a "method" is
* Learnt how to string actions together
* Hinted at things to come

[lambdas]: https://en.wikipedia.org/wiki/Anonymous_function
[readdoc]: https://hackage.haskell.org/package/base-4.9.0.0/docs/Text-Read.html
[unittype]: https://en.wikipedia.org/wiki/Unit_type
