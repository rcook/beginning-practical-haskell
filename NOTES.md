# Beginning Practical Haskell

## Overview

* Will teach the essentials required to write real programs in Haskell
* Will only dig into mathematical underpinnings as needed to understand real-world programming problems
* Will enable the student to use the tools needed to build real programs and consume Haskell libraries from third parties
* Intend to complement other courses offered by [Seattle Area Haskell Users' Group][seahug]

## Prerequisites

* This course will use [Stack][stack]
* Please follow [setup instructions][stackhowto] to install Stack
* Make sure the example in the setup guide works:

```bash
stack new my-project
cd my-project
stack setup
stack build
stack exec my-project-exe
```

* This course will use the [LTS 7.5][lts75] snapshot of Stack which uses [GHC 8.0.1][ghc801]

## Part 1

*[Sources: [1][haskellwikifp], [2][wikipediahaskell]]*

### 1.1 What is Haskell?

* Purely functional programming language
* Non-strict
* Statically typed

#### Purely functional programming language

* Functional
	* Functions are first-class objects
	* Effectively values that can be passed around
* Pure
	* Haskell functions more closely resemble mathematical functions
	* Given any input value, they return the same output
	* This is _referential transparency_
	* Typically operate on immutable data
	* No side effects

#### Strictness

* Function arguments not evaluated unless they're actually used

#### Static typing

* Catch many kinds of programmer error at _compile time_
* Expressive type system allows programmer lots of power and flexibility

### What else?

* Haskell has a clean, minimal syntax
* Much of this is a consequence of some of these other characteristics
* Example: non-strict evaluation allows us to _build_ certain flow
control constructs where other languages require language-level syntax

### 1.2 Let's do something

#### Interactive (REPL) Haskell

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

#### Your first Haskell source file

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

#### Differences between GHC and GHCI

* Names can be _shadowed_ in GHCI: i.e. we can introduce a new `z` that _hides_ the previous definition with name `z`
* In "full" Haskell, a top-level name can be used exactly once
* Though "full" Haskell allows shadowing within nested lexical scopes

[ghc801]: https://downloads.haskell.org/~ghc/master/users-guide/8.0.1-notes.html
[haskellwikifp]: https://wiki.haskell.org/Functional_programming
[lts75]: https://www.stackage.org/lts-7.5
[seahug]: http://seattlehaskell.org/
[stack]: https://docs.haskellstack.org/
[stackhowto]: https://docs.haskellstack.org/en/stable/README/#how-to-install
[wikipediahaskell]: https://en.wikipedia.org/wiki/Haskell_(programming_language)