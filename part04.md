# I/O

Given the title of this course, I'll assume you came here to learn how to write useful programs. Practical programs typically need to interact with the outside world. In the absence of input/output, all your programs can do is warm up your CPU.

Most other programming languages allow subroutines to directly perform arbitrary input and output or interaction with the outside world. However, this is fundamentally incompatible with a language such as Haskell which maintains a strong correspondence between its functions and mathematical functions.

Mathematical functions:

* Cannot read files from disc or write to the screen
* Cannot read or mutate global state
* Can only return values that are functions of their arguments or other pure functions

Fortunately, there are several different solutions to this problem. Modern Haskell's approach makes use of an abstract `IO` type. The model is functionally equivalent to the continuation-passing style, though few people actually explicitly use this particular terminology. We'll build up step-by-step to full-blown I/O.

## The Prelude

Haskell's prelude is a module provided by the implementation that defines many standard functions.
It is implicitly imported into every Haskell source file, unless this is explicitly disabled. We'll use a few functions from `Prelude` to explore I/O.

Our mission is to read a number in from the user, do something to it and then output the result to the terminal.

### The `print` function

We've already seen this function, but now we'll look at in a little more detail:

```ghci
λ> :t print
print :: Show a => a -> IO ()
```

 The type signature has a type constraint of `Show a` where `a` is a type variable again. For the sake of this discussion, `Show` is a type class with a single "method" `show` (it's not quite this simple but, whatever!):

```ghci
λ> :t show
show :: Show a => a -> String
```

A function on a type class is commonly referred to as a "method" by analogy with methods on a class or interface in object-oriented languages. Thus, `show` is a method taking a value of some type `a` and returning a `String`. All of Haskell's primitive types implement `Show` which typically return a human-readable representation of a value. You can provide instances for user-defined types&mdash;which we'll discuss eventually&mdash;or have them derived automatically for you using `deriving Show` where possible.

So, what's `IO`? Well, it's a type class much like `Num` and we already saw `()` or the unit type. Therefore, this is a function that takes something of type `a` in the type (subject to the `Show a` constraint) and returns `IO ()`

### `>>=` a.k.a. "bind"

Here are more details of `IO` in GHCi:

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

This tells us that `IO` has instances for four other type classes, namely `Monad`, `Functor`, `Applicative` and `Monoid`. Let's look at `Monad`:

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

Since `IO` has an instance for `Monad`, it provides an implementation of method `>>=`, pronounced "bind":

```ghci
λ> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

Note that functions with special symbols in their names will, under certain circumstances, require surrounding parentheses both in GHCi and Haskell source code.

Specializing from `Monad` to `IO`, `>>=` is a function that takes `IO a`, where `a` is a type variable, a function from `a` to `IO b` and evaluates to an `IO b`. We can try out `print` by itself:

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

Note that `print 5` and `print "hello"` are two separate statements in GHCi. In "full" Haskell in a source file, programs must consist of a single expression. Let's see if we can combine them into a single expression using `>>=`.

Given an `IO ()`, we need a function that takes unit and returns `IO b` where `b` can be unit too. We can build such a function that ignores its first argument and evaluates to `print "hello"`. We'll call it `f` for now:

```ghci
λ> f :: () -> IO (); f _ = print "hello"
```

We can then combine it with `print 5` by passing it as the second operand to `>>=`:

```ghci
λ> print 5 >>= f
5
"hello"
```

Our function `f` is great. However, we use it exactly once and should, therefore, never need to refer to it by name. So, let's use a lambda instead. Let's start by defining a alternative version of `f`, named `f'`, to which we'll assign the equivalent anonymous function:

```ghci
λ> f' :: () -> IO (); f' = \_ -> print "hello"
```

Function `f'` is semantically identical to `f` and, so, we can write:

```ghci
λ> print 5 >>= f'
5
"hello"
```

Since `f'` is referentially transparent, we can replace it with its value instead:


```ghci
λ> print 5 >>= \_ -> print "hello"
5
"hello"
λ> :t print 5 >>= \_ -> print "hello"
print 5 >>= \_ -> print "hello" :: IO ()
```

Now we have single expression whose type is `IO ()` with no unnecessary names. We can then put this in a source file:

```haskell
module Main where

main :: IO ()
main = print 5 >>= \_ -> print "hello"
```

And compile and run as follows:

```console
> stack runhaskell Scratch.hs
5
"hello"
```

We have successfully demonstrated several functionally equivalent ways of using a functional dependency to provide an explicit ordering of execution.

### The `read` function

Let's do some more exploring using GHCi:

```ghci
λ> :t read
read :: Read a => String -> a
```

This takes `String` and returns `a` which is subject to the `Read a` constraint. [`Read`][readdoc] is a type class that supports reading a value from a `String`. Haskell's primitive types have instances of `Read`. Since this is function is polymorphic in its return type, we'll need a type annotation to choose a specific instance of it:

```ghci
λ> :t read :: String -> Integer
read :: String -> Integer :: String -> Integer
```

This is a function that takes `String` and returns an `Integer`. We can assign a (monomorphic) name to this specific instance of `read` and test it out:

```ghci
λ> readInteger :: String -> Integer; readInteger = read
λ> :t readInteger
readInteger :: String -> Integer
λ> readInteger "123"
123
```

### The `getLine` function

Here's `getline` in all its glory:

```ghci
λ> :t getLine
getLine :: IO String
```

### The `putStr` function

This one is straightforward:

```ghci
λ> :t putStr
putStr :: String -> IO ()
```

This is simply `putStrLn` without the extra line ending.

### The `hFlush` function

By itself, `putStr` outputs the given characters to the standard output stream on the terminal but does not, by default, flush the stream. Just like using standard output from a C program, we'll need to flush the buffer prior to requesting input from the user. In C/C++ we'd have to do the following:

```c
printf("Buffered output: ");
fflush(stdout);
```

Similarly, in Haskell we'll need to use `hFlush`:


```ghci
λ> :t hFlush
hFlush :: Handle -> IO ()
```

The handle itself, `stdout`, is in the `System.IO` namespace, so you'll need to
import this namespace. This is our first use of the `import` keyword in Haskell
which we'll run into more in the future:

```haskell
import System.IO
```

### Combine them all

This is what we're going to do:
* Print a prompt to the terminal (using `putStr`)
* Flush the standard output buffer (using `hFlush`)
* Get a string from the keyboard (using `getLine`)
* Convert the string to an integer (using `read`/`readInteger`)
* Multiply the integer by 2 (using `*`)
* Print out the result (using `print`)

The resulting program will consist of a `main` function itself consisting of a
single expression:

```haskell
import System.IO

readInteger :: String -> Integer
readInteger = read

main :: IO ()
main = putStr "Enter a number and I'll double it: "
    >>= \_ -> hFlush stdout
    >>= \_ -> getLine
    >>= \l -> print (2 * readInteger l)
```

Let's run it:

```console
> stack runhaskell Scratch.hs
Enter a number and I'll double it: 5
10
```

### Wait a minute!

But, didn't you say that Haskell was purely functional? Surely, `putStr`, `getLine` and `print` have side effects?

Well, no, not at all:

* They're "actions"
* They're strung together using `>>=`
* At no point do they mutate global state or interact with the outside world
* We build up an expression tree or graph of these actions
* They are "executed" when the program is run

Did you mention something about continuation passing?

* Why, yes, I did!
* That's what `>>=` is doing
* You provide an action as its first argument
* And the continuation as the second argument
* This second argument is the thing that is to be evaluated after the first action has run at program execution time: the value generated by the first action is passed as an argument to the continuation

`>>=` is great as it provides the ability to sequence actions (at least when it's used with `IO`). However, all of these `>>=`s will become ugly as we sequence more and more subexpressions in our program. We will talk about how to clean this up soon.

[cps]: https://en.wikipedia.org/wiki/Continuation-passing_style
[readdoc]: https://hackage.haskell.org/package/base-4.9.0.0/docs/Text-Read.html
[stdoutbuffering]: http://stackoverflow.com/questions/1716296/why-does-printf-not-flush-after-the-call-unless-a-newline-is-in-the-format-strin
