# Functions

But, wait, isn't Haskell supposed to be a functional programming language? It's already part 2 of the course and we haven't encountered any functions yet.

Since Haskell is a functional programming language, functions are first-class values. You will recall an earlier example with a value named `z` which was the result of applying the `+` operator to `x` and `y`. Let's generalize this to a function that adds its two arguments together:

```ghci
λ> addIntegers :: Integer -> Integer -> Integer; addIntegers x y = x + y
λ> :t addIntegers
addIntegers :: Integer -> Integer -> Integer
```

The `:t` command yields the type signature for `addIntegers`: in this case a function taking two `Integer`s and evaluating to a third `Integer`. The function's "return" type is the rightmost type in the signature. The `->` operator is read as "to" or "maps to".

We can use this function as follows:

```ghci
λ> addIntegers 5 6
11
λ> addIntegers 10 11
21
```

Many languages use parentheses `(` and `)` to delimit the arguments of a function application or call. Applying functions, however, is Haskell's raison d'&ecirc;tre and, in the spirit of minimal, clean syntax, Haskells eschews extraneous punctuation for this most fundamental of syntactic elements.

Moving this function to a source file would look like:

```haskell
addIntegers :: Integer -> Integer -> Integer
addIntegers x y = x + y

main :: IO ()
main = print (addIntegers 5 6)
```

Since `addIntegers` is a value much like `z`, albeit one with arguments, it can be passed as an argument to other functions. In this respect, `addIntegers` is much like `z` or  `5`, `"hello"` or any other value:

```haskell
addIntegers :: Integer -> Integer -> Integer
addIntegers x y = x + y

functionTakingAFunction :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer
functionTakingAFunction f a b = f a b

main :: IO ()
main = print (functionTakingAFunction addIntegers 5 6)
```
