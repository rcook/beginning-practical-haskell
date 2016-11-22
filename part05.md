# Ugly code

Let's look at a more complicated example:

```haskell
import System.IO

readInteger :: String -> Integer
readInteger = read

main :: IO ()
main = putStr "Enter x: "
    >>= \_ -> hFlush stdout
    >>= \_ -> getLine
    >>= \xStr -> putStr "Enter y: "
    >>= \_ -> hFlush stdout
    >>= \_ -> getLine
    >>= \yStr ->
        let x = readInteger xStr
            y = readInteger yStr
            z = x + y
        in putStrLn $ "z = " ++ show z
```

This program displays a prompt and reads in a line of input twice. It then converts each of the two lines into an `Integer`, computes their sum and then writes the output to the terminal. Let's consider the new things we see here:

* `let` and `in`: let-bindings introduce one or more new names with associated expressions into the expression after `in`
*  `++`: pronounced "append"; joins two lists together; `String` is `[Char]`, so this joins two strings together

I told you that Haskell had clean, minimal syntax. You've seen nearly everything here. However, this does not look pleasant. Let's try to do something about that. First, let's reflow the code:

```haskell
import System.IO

readInteger :: String -> Integer
readInteger = read

main :: IO ()
main =
    putStr "Enter x: " >>= \_ ->
    hFlush stdout >>= \_ ->
    getLine >>= \xStr ->
    putStr "Enter y: " >>= \_ ->
    hFlush stdout >>= \_ ->
    getLine >>= \yStr ->
    let x = readInteger xStr
        y = readInteger yStr
        z = x + y
    in
    putStrLn $ "z = " ++ show z
```

That's marginally better. There's still a proliferation of `>>=` operators and lambdas. What next? Let's take a look at another method provided by `Monad`, namely `>>`, pronounced "then". This is the second "monadic sequencing operator" with the following type signature:

```ghci
λ> :t (>>)
(>>) :: Monad m => m a -> m b -> m b
```

This is a simpler version of `>>=` which can be used when the value returned by the previous action is to be ignored. In this program, we twice use `putStr` followed by `getLine` which ignores the value yielded by `putStr` using `_`. We can, therefore, rewrite the program as follows:

```haskell
import System.IO

readInteger :: String -> Integer
readInteger = read

main :: IO ()
main =
    putStr "Enter x: " >>
    hFlush stdout >>
    getLine >>= \xStr ->
    putStr "Enter y: " >>
    hFlush stdout >>
    getLine >>= \yStr ->
    let x = readInteger xStr
        y = readInteger yStr
        z = x + y
    in
    putStrLn $ "z = " ++ show z
```

That's another step in the right direction.

This code shape is so common, however, that Haskell introduces special syntax to make it even cleaner. This is known as `do`-notation named after the `do`-keyword used to introduce it.

Consider:

```haskell
do action1
   action2
   action3
```

This is transformed or [desugared][syntacticsugar] to:

```haskell
action1 >>
do action2
   action3
```

And then to:

```haskell
action1 >>
action2 >>
do action3
```

And, finally, to:

```haskell
action1 >>
action2 >>
action3
```

The second form that can be included in `do`-notation includes a binding of the action's result to a name:

```haskell
do x1 <- action1
   x2 <- action2
   action3 x1 x2
```

becomes

```haskell
action1 >>= \x1 ->
do x2 <- action2
   action3 x1 x2
```

becomes

```haskell
action1 >>= \x1 ->
action2 >>= \x2 ->
do action3 x1 x2
```

becomes

```haskell
action1 >>= \x1 ->
action2 >>= \x2 ->
action3 x1 x2
```

Furthermore, these two forms can be combined. With this new knowledge, we can rewrite our original program as follows:

```haskell
import System.IO

readInteger :: String -> Integer
readInteger = read

main :: IO ()
main = do
    putStr "Enter x: "
    hFlush stdout
    xStr <- getLine
    putStr "Enter y: "
    hFlush stdout
    yStr <- getLine
    let x = readInteger xStr
        y = readInteger yStr
        z = x + y
    putStrLn $ "z = " ++ show z
```

Inside `do`-notation, Haskell also drops the `in` from let-bindings.

[syntacticsugar]: https://en.wikipedia.org/wiki/Syntactic_sugar
