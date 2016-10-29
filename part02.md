[Index](index.md) | [Part 1](part01.md) | **Part 2**

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

# Let's get a number from the keyboard

## The `getLine` function

* Let's do some more exploring using GHCI:

```ghci
λ> :t getLine
getLine :: IO String
```

* What's `IO`?
* Well, clearly it's a type class much like `Num` etc.
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
* The one the we care about the most right now is `Monad`:

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

* A function on a type class is typically referred to as a "method"
* Since `IO` has an instance for `Monad`, it provides an instance of "method" `>>=`, pronounced "bind":

```ghci
λ> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

* Note that functions incorporating symbols in their names will, under certain circumstances, require surrounding parentheses
* Specializing from `Monad` to `IO`, `>>=` is a function that takes `IO a`, where `a` is a type variable, a function from `a` to `IO b` and evaluates to an `IO b`

## The `read` function

* Let's do some more exploring using GHCI:

```ghci
λ> :t read
read :: Read a => String -> a
```

* This is a function that takes `String` and returns `a`
* `a` is a type variable which can be any type that satisfies the type constraints on the left of `=>`
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

[readdoc]: https://hackage.haskell.org/package/base-4.9.0.0/docs/Text-Read.html