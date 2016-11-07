# Our first function

* But, isn't Haskell a functional programming language?
* What about the functions?
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

* What makes Haskell a functional programming language is that functions are first-class values just like the things that we would traditionally consider to be values, such as `5`, `"hello"` etc.
