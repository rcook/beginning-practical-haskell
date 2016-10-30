[Contents](index.md) | [P1](part01.md) | [P2](part03.md) | **P3**

# Let's get ugly

Let's look at a more complicated example:

```haskell
readInteger :: String -> Integer
readInteger = read

main :: IO ()
main = putStr "Enter x: "
    >>= \_ -> getLine
    >>= \xStr -> putStr "Enter y: "
    >>= \_ -> getLine
    >>= \yStr ->
        let x = readInteger xStr
            y = readInteger yStr
            z = x + y
        in putStrLn ("z = " ++ show z)
```

This program displays a prompt and reads in a line of input twice. It then converts each of the two lines into an `Integer`, computes their sum and then writes the output to the terminal. Let's consider the new things we see here:

* `let` and `in`: let-bindings introduce one or more new names with associated expressions into the expression after `in`
*  `putStrLn`: a variant of `putStr` which outputs a `String` followed by a line break
*  `++`: pronounced "append"; joins two lists together; `String` is `[Char]`, so this joins two strings together
