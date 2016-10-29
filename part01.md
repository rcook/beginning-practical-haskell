[Index](index.md) | **Part 1** | [Part 2](part02.md)

# What is Haskell?

*[Sources: [1][haskellwikifp], [2][wikipediahaskell]]*

* Purely functional programming language
* Non-strict
* Statically typed

## Purely functional programming language

* Functional
	* Functions are first-class objects
	* Effectively values that can be passed around
* Pure
	* Haskell functions more closely resemble mathematical functions
	* Given any input value, they return the same output
	* This is _referential transparency_
	* Typically operate on immutable data
	* No side effects

## Strictness

* Function arguments not evaluated unless they're actually used

## Static typing

* Catch many kinds of programmer error at _compile time_
* Expressive type system allows programmer lots of power and flexibility

## What else?

* Haskell has a clean, minimal syntax
* Much of this is a consequence of some of these other characteristics
* Example: non-strict evaluation allows us to _build_ certain flow
control constructs where other languages require language-level syntax
* To a first approximation, Haskell programs consist of two elements:
    * Definitions
    * Expressions
* Nontrivial Haskell programs will also likely include some extras:
    * Type annotations
    * Pragmas
    * Import statements

# Let's do something

## Interactive (REPL) Haskell

* Fire up your terminal or command prompt
* Create a new project named `hello-world`

```bash
stack new hello-world --resolver=lts-7.5
cd hello-world
```

* Fire up GHCI

```bash
stack ghci
```

* GHC is the Glasgow Haskell Compiler
* GHCI is GHC _Interactive_
* It's GHC's read-evaluate-print-loop (REPL)

```ghci
λ> x = 5
λ> y = 6
λ> z = x + y
λ> z
11
λ> :t z
z :: Num a => a
λ> :t 5
5 :: Num t => t
λ> z = "hello"
λ> z
"hello"
λ> :t z
z :: [Char]
λ> :q
```

## Your first Haskell source file

* Let's do some similar stuff in a source file
* Create `Hello.hs`:

```haskell
x = 5
y = 6
z = x + y

main = print z
```

* Run it as follows:

```bash
stack runhaskell Hello.hs
```

* Change `Hello.hs` to the following to mimic the GHCI example:

```haskell
x = 5
y = 6
z = x + y
z = "hello"

main = print z
```

* Run it again:

```console
> stack runhaskell Hello.hs

Hello.hs:4:1: error:
    Multiple declarations of ‘z’
    Declared at: Hello.hs:3:1
                 Hello.hs:4:1
```

## Differences between GHC and GHCI

* Names can be _shadowed_ in GHCI: i.e. we can introduce a new `z` that _hides_ the previous definition with name `z`
* In "full" Haskell, a top-level name can be used exactly once
* Though "full" Haskell allows shadowing within nested lexical scopes

# A more realistic example

*[Sources: [1][haskellnumbers]]*

* Let's add some type annotations
* Start up GHCI again

```ghci
λ> x :: Integer; x = 5
λ> y :: Integer; x = 6
λ> z :: Integer; z = x + y
λ> z
11
λ> :t x
x :: Integer
λ> :t y
y :: Integer
λ> :t z
z :: Integer
λ> a = 5
λ> :t a
a :: Num t => t
```

* Let's do that in our source file:

```haskell
x :: Integer
x = 5

y :: Integer
y = 6

z :: Integer
z = x + y

main :: IO ()
main = print z
```

* Consider the type of `a`:
    * Items to the left of `=>` are _type constraints_
    * Lower-case `t` is a _type variable_ and can be any type that fulfils the type constraints
    * `Num t` constrains `t` to be an instance of the `Num` _type class_
    * For now, it suffices to say that `Num` is a _type class_ which has an instance for (or "is implemented by") all numeric types in Haskell
* Consider the type of `x`, `y` and `z`:
    * These have no `=>` and, therefore, no type constraints
    * Upper-case `Integer` is a _concrete type_ corresponding to arbitrary-precision integers: this is an _instance_ of `Num`
* We'll talk about `IO ()` next lesson

## When to use them

* Haskell has powerful type inference
* Haskell designed in such a way that usually you won't need them
* Sometimes ambiguities arise
* Some more [advanced language features][dependenttypes] make ambiguities more likely
* Even so, type annotations are useful as documentation and for type-driven development
* Most experienced Haskell developers recommend that all _top-level_ definitions should carry a type annotation

# Our first function

* `z` was a value which was the result of applying the `+` operator to `x` and `y`
* Let's generalize this to a function which adds it two arguments:

```ghci
λ> addIntegers :: Integer -> Integer -> Integer; addIntegers x y = x + y
λ> :t addIntegers
addIntegers :: Integer -> Integer -> Integer
```

* Now, let's use it:

```ghci
λ> addIntegers 5 6
11
λ> addIntegers 10 11
21
```

* Some languages use parentheses `(` and `)` for function application
* However, applying functions is Haskell's raison d'&ecirc;tre
* In the spirit of minimal, clean syntax, function application eschews parentheses
* In a source file, this would look like:

```haskell
addIntegers :: Integer -> Integer -> Integer
addIntegers x y = x + y

main :: IO ()
main = print (addIntegers 5 6)
```


[dependenttypes]: https://wiki.haskell.org/Dependent_type
[haskellnumbers]: https://www.haskell.org/tutorial/numbers.html
[haskellwikifp]: https://wiki.haskell.org/Functional_programming
[wikipediahaskell]: https://en.wikipedia.org/wiki/Haskell_(programming_language)