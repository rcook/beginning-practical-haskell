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

Many languages use parentheses `(` and `)` to delimit the arguments of a function application or call. Functions are so central to the Haskell Way that the language designers intentionally chose the tersest syntax possible for function definitions and function application. Thus, instead of:

$\operatorname{f}(x, y) = x ^ 2 + y ^ 2$<br>
$\operatorname{g}(x, y) = x ^ 3 + y ^ 3$<br>
$\operatorname{h}(x, y) = \operatorname{f}(x, y) + \operatorname{g}(x, y)$

Haskell uses:

```haskell
f x y = x ^ 2 + y ^ 2
g x y = x ^ 3 + y ^ 3
h x y = f x y + g x y
```

To further reduce the use of parentheses, Haskell also assigns the highest precedence of all infix operators to function application.

Moving our `addIntegers` function to a source file, we get:

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

# Anonymous functions and lambda abstraction

Functions are so important in Haskell that we get to refer to them by their own individual names or with no name at all. They also get their own letter of the Greek alphabet: lambda, so-called because of [the lambda calculus][lambdacalculus]. Lambda calculus is a universal model of computation equivalent in power to the Turing machine. It's based on function abstraction and function application.

Consider the named (mathematical) function $\operatorname{square\_sum}$:

$\operatorname{square\_sum}(x, y) = x ^ 2 + y ^ 2$

Based on an initial simplification, rewriting in _anonymous form_, we obtain:

$(x,y) \mapsto x ^ 2 + y ^ 2$

i.e. the pair of $x$ and $y$ maps to $x ^ 2 + y ^ 2$. Similarly, the identity function, $\operatorname{id}$, is given by:

$\operatorname{id}(x) = x$

or

$x \mapsto x$

in anonymous form.

A second simplification is to refactor multiple-argument functions, such as $\operatorname{square\_sum}$, into equivalent functions in a single argument, known as [_currying_][haskellcurry] (or [_schönfinkelization_][mosesschoenfinkel]):

$(x,y) \mapsto x ^ 2 + y ^ 2$

is equivalent to

$x \mapsto (y \mapsto x ^ 2 + y ^ 2)$

Application of the function $\operatorname{square\_sum}$ to the arguments $(5, 2)$ yields:

$((x,y) \mapsto x ^ 2 + y ^ 2)(5, 2)$<br>
$= 5 ^ 2 + 2 ^ 2$<br>
$= 29$

while applying our curried version looks like:

$((x \mapsto (y \mapsto x ^ 2 + y ^ 2))(5))(2)$<br>
$= (y \mapsto 5 ^ 2 + y ^ 2)(2)$<br>
$= 5 ^ 2 + 2 ^ 2$<br>
$= 29$

It can be shown that single-argument functions obtained by currying multiple-argument functions in this way are equivalent in expressive power and only differ in the number of steps required to evaluate them.

Finally, lambda forms are represented as follows:

$x \mapsto x ^ 2$ also written $\lambda x . x ^ 2$

becomes

```haskell
\x -> x ^ 2
```

Similarly, the anonymous form of our $\operatorname{square\_sum}$ function becomes:

```haskell
\x y -> x ^ 2 + y ^ 2
```

or, in curried form:

```haskell
\x -> \y -> x ^ 2 + y ^ 2
```

[haskellcurry]: https://en.wikipedia.org/wiki/Haskell_Curry
[lambdacalculus]: https://en.wikipedia.org/wiki/Lambda_calculus
[mosesschoenfinkel]: https://en.wikipedia.org/wiki/Moses_Sch%C3%B6nfinkel
