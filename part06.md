---
title: "Part 6: Other concepts and syntax"
...

# Recursion and higher-order function abstractions

In Haskell, we will typically use recursion instead of loops since mutating loop variables are not allowed in most cases. We will not go into laborious detail on recursion and, instead, will consider a couple of basic examples and look quickly at how we can use higher-level abstractions with higher-order functions.

Consider `Main.hs`:

```haskell
sumElements :: [Int] -> Int
sumElements [] = 0
sumElements (h : t) = h + sumElements t

lengthElements :: [String] -> [Int]
lengthElements [] = []
lengthElements (h : t) = length h : lengthElements t

main :: IO ()
main = do
    print $ sumElements [10, 20, 30]
    print $ lengthElements ["aaa", "aa", "a]
```

Which can run as follows:

```console
> stack runhaskell Hello.hs
60
[3,2,1]
```

This example illustrates two of the most basic types of recursion patterns, namely folding and mapping. Since these patterns are so common, we will typically use higher-order functions instead:

Abstraction | Function | Example
:-----|:-------------------------|:-------------------------
Left-fold   | `foldl ::`<br>`(a -> b -> a) -> a -> [b] -> a` | `λ> foldl (+) 0 [10, 20, 30]`<br>`60` |
Right-fold  | `foldr ::`<br>`(a -> b -> b) -> b -> [a] -> b` | `λ> foldr (+) 0 [10, 20, 30]`<br>`60` |
Map         | `map ::`<br>`(a -> b) -> [a] -> [b]`           | `λ> map length ["aaa", "aa", "a"]`<br>`[3,2,1]` |

There are many others, of course. In general, we should prefer the higher-order functions to explicit recursion.

# `if`, `then`, `else`

> ***TODO:***
> Do it!
