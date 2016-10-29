# Prerequisites

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
* This course will use the [LTS 7.5][lts75] snapshot of Stack which uses
[GHC 8.0.1][ghc801]

# Part 1

## 1.1: What is Haskell?

Sources: [1][haskellwikifp], [2][wikipediahaskell]

* Purely functional programming language
* Non-strict
* Statically typed

### Purely functional programming language

* Functional
	* Functions are first-class objects
	* Effectively values that can be passed around
* Pure
	* Haskell functions more closely resemble mathematical functions
	* Given any input value, they return the same output
	* This is _referential transparency_
	* Typically operate on immutable data
	* No side effects

### Strictness

* Function arguments not evaluated unless they're actually used

### Static typing

* Catch many kinds of programmer error at _compile time_
* Expressive type system allows programmer lots of power and flexibility

### What else?

* Haskell has a clean, minimal syntax
* Much of this is a consequence of some of these other characteristics
* Example: non-strict evaluation allows us to _build_ certain flow
control constructs where other languages require language-level syntax

## 1.2: Let's do something

[ghc801]: https://downloads.haskell.org/~ghc/master/users-guide/8.0.1-notes.html
[haskellwikifp]: https://wiki.haskell.org/Functional_programming
[lts75]: https://www.stackage.org/lts-7.5
[stack]: https://docs.haskellstack.org/
[stackhowto]: https://docs.haskellstack.org/en/stable/README/#how-to-install
[wikipediahaskell]: https://en.wikipedia.org/wiki/Haskell_(programming_language)