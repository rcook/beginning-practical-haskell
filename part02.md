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

## The Prelude

* Haskell defines many standard functions in what is known as the "Prelude"
* This is a module that is implicitly imported into every Haskell source file, unless this is explicitly disabled
* We'll use a few Prelude functions to explore I/O in Haskell

## Let's get a number from the keyboard

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