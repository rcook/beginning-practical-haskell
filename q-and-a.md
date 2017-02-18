---
title: Questions and answers
---

This will eventually become Frequently Asked Questions once the questions have been asked more than once each!

# If a thunk is forced from more than one thread, are there any guarantees that a value will not be evaluated more than once?

My hand-wavy answer to this was "It doesn't matter" since referential transparency guarantees that whether the inner computation was performed more than once cannot be observed by outside users. But, of course, this is not really true since more than one computation could incur a time or performance overhead that could be observed by the end user.

My claim is that Haskell programming language specification does not go into this and an implementation is free to deal with this however it sees fit.

I will look into the details of this and get back to you when I have a more satisfactory answer.

[Here][mainisusuallyafunction]'s a good source.

[mainisusuallyafunction]: http://mainisusuallyafunction.blogspot.com/2011/10/thunks-and-lazy-blackholes-introduction.html

# Why does the `build-depends` section place an upper bound on `base`?

[Hackage][hackage] (the Haskell package repository) prefers to use [package versioning policy][pvp] for packages, but does not attempt to enforce it. Anyway, the gist of this policy is that versions of packages, including `base`, should be compatible if they have the same major version number. Moving forward to `base` with a newer major version is likely to require nontrivial changes to a project's dependencies and/or source code.

# Is Haskell a compiled or interpreted language? What about GHCi?

I believe that the Haskell [specification][haskell2010languagereport] doesn't, to the best of my knowledge, mandate whether a conformant Haskell implementation should compile or interpret code. Neither does mandate whether the implementation should generate native code or an intermediate language. We'll be using [GHC][ghc]&mdash;the Glasgow Haskell Compiler&mdash;which is a compiler and emits native code (and other kinds of output with different backends). This is true even of GHCi, the REPL, which compiles code incrementally using the [GHC API][ghcapi].

# Line continuation in GHCi

While performing [interactive evaluation][interactiveevaluation], input can split across multiple lines using the `:{` and `:}` delimiters:

```ghci
λ> :{
λ| let { g op n [] = n
λ|     ; g op n (h:t) = h `op` g op n t
λ|     }
λ| :}
λ> g (*) 1 [1..3]
6
```

# Can I tell within GHCi if a thunk has been evaluated etc.?

This can be done using the `:sprint` command:

```ghci
λ> a = 5 + 6
λ> :t a
a :: Num a => a
λ> :sprint a
a = _
λ> a
11
λ> :sprint a
a = _
λ> b = (5 + 6) :: Int
λ> :t b
b :: Int
λ> :sprint b
b = _
λ> b
11
λ> :sprint b
b = 11
λ> xs = [0 ..]
λ> :t xs
xs :: (Num t, Enum t) => [t]
λ> :sprint xs
xs = _
λ> head xs
0
λ> :sprint xs
xs = _
λ> ys = [0 ..] :: [Int]
λ> :t ys
ys :: [Int]
λ> :sprint ys
ys = _
λ> head ys
0
λ> :sprint ys
ys = 0 : _
```

Notes:

* Unevaluated terms or subterms are indicated by `_`
* Both `a` and `xs` are polymorphic types and will always show up as `_`
* Only concrete instantiations of `a` and `xs` will demonstrate partial or full evaluation using `:sprint`
* Both `b` and `ys` are monomorphic (i.e. concrete) types and demonstrate partial and full evaluation

# What is `Int` and what is `Integer`?

We'll talk about this in later session. For now, all you need to know is that `Int` is a machine integer (akin to `int` in C++) while `Integer` is an arbitrary precision or "mathematical" integer. Both provide instances of the `Num` type class.

# What happens if a valid `Float` value cannot be represented by `Int`?

Automatic conversions between numeric types [is unsupported][convertingnumbers]. This is likely to remain the case forever. Here's what happens when we try to convert floating-point values that are unrepresentable as integers:

Let's define `infinity` be reading its string represention:

```ghci
λ> infinity = read "Infinity" :: Float
λ> infinity
Infinity
λ> :t infinity
infinity :: Float
```

And, similarly, `nan`:

```ghci
λ> nan = read "NaN" :: Float
λ> nan
NaN
> :t nan
nan :: Float
```

Let's convert these to `Integer` and `Int`:

```ghci
> round infinity :: Integer
340282366920938463463374607431768211456
λ> round nan :: Integer
-510423550381407695195061911147652317184
λ> round infinity :: Int
0
λ> round nan :: Int
0
```

I was somewhat disappointed by some of these behaviours to be honest. However, I expect that this is roughly equivalent to how a C++ compiler would behave. If these special values, and other unrepresentable values, are important to the correct running of the program, the [`RealFloat`][realfloat] type class offers the full array of helper functions such as `isNaN` and `isInfinite` that you'd expect.

# Can I view the history of GHCi commands?

GHCi offers `:back`, `:forward`, `:trace`, `:history` commands. There is also the `~/.ghc/ghci_history` file that retains a log of your GHCi commands.

# TODO

* Shadowing of GHCi bindings
* Whitespace for scopes etc.

[convertingnumbers]: https://wiki.haskell.org/Converting_numbers
[ghc]: https://www.haskell.org/ghc/
[ghcapi]: https://wiki.haskell.org/GHC/As_a_library
[hackage]: http://hackage.haskell.org/
[haskell2010languagereport]: https://www.haskell.org/onlinereport/haskell2010/
[interactiveevaluation]: https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/interactive-evaluation.html
[pvp]: http://pvp.haskell.org/
[realfloat]: http://hackage.haskell.org/package/base-4.9.0.0/docs/Prelude.html#t:Num
