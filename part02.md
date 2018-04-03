---
title: "Part 2: Functions"
---

# Where are the functions?

But, wait, isn't Haskell supposed to be a functional programming language? It's already part 2 of the course and we haven't encountered any functions yet.

Since Haskell is a functional programming language, functions are first-class values. Recall our earlier example with a value named `z` which was the result of applying the `+` operator to `x` and `y`.


```ghci
λ> x = 5
λ> y = 6
λ> z = x + y
λ> z
11
```

Let's generalize this to a function that adds its two arguments together:

```ghci
λ> z x y = x + y
λ> :t z
z :: Num a => a
λ> z 6 7
11
```

Now let's rename our function `addIntegers` and provide a type annotation:

```ghci
λ> addIntegers :: Integer -> Integer -> Integer; addIntegers x y = x + y
λ> :t addIntegers
addIntegers :: Integer -> Integer -> Integer
λ> addIntegers 6 7
11
```

The `:t` command yields the type signature for `addIntegers`: in this case a function taking two `Integer`s and evaluating to a third `Integer`. The function's "return" type is the rightmost type in the signature. The `->` operator is read as "to" or "maps to".

Many languages use parentheses, `(` and `)`, to delimit the arguments of a function application or call. Functions are so central to the Haskell Way that the language designers intentionally chose the tersest syntax possible for function definitions and function application. Thus, instead of:

$\operatorname{f}(x, y) = x ^ 2 + y ^ 2$

$\operatorname{g}(x, y) = x ^ 3 + y ^ 3$

$\operatorname{h}(x, y) = \operatorname{f}(x, y) + \operatorname{g}(x, y)$

Haskell uses:

```haskell
f x y = x ^ 2 + y ^ 2
g x y = x ^ 3 + y ^ 3
h x y = f x y + g x y
```

Note that `f` and `g` are in scope for `h`.

To further reduce the use of parentheses, Haskell also assigns the highest precedence of all infix operators to function application.

Moving our `addIntegers` function to a source file and adding its type signature, we get:

```haskell
addIntegers :: Integer -> Integer -> Integer
addIntegers x y = x + y

main :: IO ()
main = print (addIntegers 5 6)
```

To reiterate, this defines a function `addIntegers` that takes an `Integer`, a second `Integer` and evaluates to an `Integer` (analogous to "returning" an `Integer` in imperative programming languages).

Since `addIntegers` is a value much like `z`, albeit one with arguments, it can be passed as an argument to other functions. In this respect, `addIntegers` is much like `z` or  `5`, `"hello"` or any other value.

```haskell
addIntegers :: Integer -> Integer -> Integer
addIntegers x y = x + y

functionTakingAFunction :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer
functionTakingAFunction f a b = f a b

main :: IO ()
main = print (functionTakingAFunction addIntegers 5 6)
```

# Anonymous functions and lambda abstraction

Functions are so important in Haskell that we get to refer to them by name or with no name at all. They also get their own letter of the Greek alphabet: lambda, so-called because of [the lambda calculus][lambdacalculus]. Lambda calculus is a universal model of computation, equivalent in power to the Turing machine. It's based on function abstraction and function application and this is the bare minimum you need to know to get started with Haskell.

Consider the named (mathematical) function $\operatorname{square\_sum}$:

$\operatorname{square\_sum}(x, y) = x ^ 2 + y ^ 2$

Based on an initial simplification, rewriting in _anonymous form_, we obtain:

$(x,y) \mapsto x ^ 2 + y ^ 2$

i.e. the pair of $x$ and $y$ maps to $x ^ 2 + y ^ 2$. In essence, this is a definition of a "thing" that maps $x$ and $y$ to $x ^ 2 + y ^ 2$.

Similarly, the identity function, $\operatorname{id}$, is given by:

$\operatorname{id}(x) = x$

or

$x \mapsto x$

in anonymous form. This anonymous form is a another "thing": this time it's something that maps $x$ to itself, i.e. $x$.

Under the notion of _alpha-equivalence_, the exact name of the _bound variable_, in this case $x$, does not matter as long as there are no name collisions since the name $x$ does not "leak" out of the anonymous function's body. With this intuition, it's straightforward to see that the anonymous form of $\operatorname{id}$ is a "thing" that maps something to itself.

A second simplification is to transform multiple-argument functions, such as $\operatorname{square\_sum}$, into equivalent functions in a single argument, known as [_currying_][haskellcurry] (or [_schönfinkelization_][mosesschoenfinkel]):

$(x,y) \mapsto x ^ 2 + y ^ 2$

is equivalent to

$x \mapsto (y \mapsto x ^ 2 + y ^ 2)$

i.e. a function that maps $x$ to another function in $y$ that evaluates to $x ^ 2 + y ^ 2$.

Application of the function $\operatorname{square\_sum}$ to the arguments $(5, 2)$ yields:

$((x,y) \mapsto x ^ 2 + y ^ 2)(5, 2)$<br>
$= 5 ^ 2 + 2 ^ 2$<br>
$= 29$

while applying our curried version looks like:

$((x \mapsto (y \mapsto x ^ 2 + y ^ 2))(5))(2)$<br>
$= (y \mapsto 5 ^ 2 + y ^ 2)(2)$<br>
$= 5 ^ 2 + 2 ^ 2$<br>
$= 29$

It can be shown that single-argument functions obtained by currying multiple-argument functions in this way are equivalent in expressive power and only differ in the number of steps required to evaluate them. The transformation is also obvious and completely mechanical. Furthermore, the additional steps required to evaluate curried functions can be trivially optimized away by the compiler.

Finally, these anonymous functions can be rewritten as _lambda forms_:

$x \mapsto x ^ 2$ is identical to $\lambda x . x ^ 2$

In Haskell this becomes:

```haskell
\x -> x ^ 2
```

The backslash is chosen due its (questionable) resemblance to the lambda character.

Similarly, the anonymous form of our $\operatorname{square\_sum}$ function becomes:

```haskell
\x y -> x ^ 2 + y ^ 2
```

or, in curried form:

```haskell
\x -> \y -> x ^ 2 + y ^ 2
```

At this point, it should not surprise you that
these anonymous functions are values in their own right. Thus, names can be assigned to them. This brings us full circle and illustrate the equivalence of functions and anonymous functions modulo the name itself:

```ghci
λ> squareSum = \x -> \y -> x ^ 2 + y ^ 2
λ> :t squareSum
squareSum :: Num a => a -> a -> a
λ> squareSum 3 4
25
```

# Function composition

So far we've applied only single functions to single values. In practice, we're likely to want to do much more than this. Consider computing the hypotenuse of a right-angled triangle. We already have our `squareSum` function which we can reuse. We also happen to know that there is a standard `sqrt` function that we can reuse. Let's see what we can do with our (contrived) example:

```ghci
λ> squareSum x y = x ^ 2 + y ^ 2
λ> squareSumWith3 = squareSum 3
λ> sqrt (squareSumWith3 4)
5.0
```

We're applying a function `squareSumWith3` to value `4` and then feeding its value into the input of the function `sqrt`. This kind of chaining or _composition_ of functions is so common that it gets the second most unassuming operator in the language after whitespace, e.g. `.`. We can rewrite this last line as follows:

```ghci
λ> (sqrt . squareSumWith3) 4
5.0
```

Notionally, `.` is equivalent to the function `compose` of type `(b -> c) -> (a -> b) -> (a -> c)`: i.e. a function taking two functions and yielding a third function:

```ghci
λ> compose f g x = f (g x)
λ> :t compose
compose :: (t -> t1) -> (t2 -> t) -> t2 -> t1
λ> f = compose sqrt squareSumWith3
λ> :t sqrt
sqrt :: Floating a => a -> a
λ> :t squareSumWith3
squareSumWith3 :: Num a => a -> a
λ> :t f
f :: Floating t1 => t1 -> t1
λ> f 4
5.0
```

Of course, we can assign our silly `sqrt . squareSumWith3` expression to a name too:

```ghci
λ> f = sqrt . squareSumWith3
λ> f 4
5.0
```

Again, this example is totally contrived and we'll build up to more realistic uses soon.

# More function application

We've already discussed function application. At this point it's worth mentioning Haskell's other function application operator: `$`. This is semantically identical to our existing function application model, using whitespace, but with different, much lower, precedence. It is used to reduce the need for parentheses and is commonly used to reduce code's visual clutter. Consider our toy functions again:

```ghci
λ> (sqrt . squareSumWith3) 4
5.0
λ> sqrt . squareSumWith3 $ 4
5.0
```

This doesn't look like much, but when dealing with multiple levels of nested parentheses, `$` can eliminate much of this line noise and go a long towards making the code look less [Lisp-like][seaofparentheses]. When you use parentheses, `$` or a combination of the two, is entirely a matter of taste, though you'll need to get used to it in order to be able read other people's code.

# Functions, expressions, scope and binding additional names

Let's consider a minimal Haskell source file:

```haskell
double x = 2 * x
treble x = 3 * x
doubleThenTreble = treble . double
main = print $ doubleThenTreble 5
```

Some important notes:

* There are four "top-level bindings" namely, `double`, `treble`, `doubleThenTreble` and `main`
* They are all "visible" to one another
* The order in which they are declared does not matter: merely that they are in the same scope
* The definition of each can refer to every other top-level name, including itself, i.e. they can be recursive and _mutually recursive_

It is also possible to introduce child lexical scopes using either `let`&hellip;`in` or `where`. For example:

```haskell
doubleThenTreble = treble . double
    where double x = 2 * x
          treble x = 3 * x
```

This introduces the names `double` and `treble` into the scope of the expression on the right-hand of the `=`-sign. Thus, the expression `treble . double` "sees" these names. However, they _cannot_ be seen by other top-level bindings.

Here is are two equivalent uses `let`&hellip;`in`, also known as `let`-bindings:

```haskell
doubleThenTreble' =
    let double x = 2 * x
        treble x = 3 * x
    in treble . double

doubleThenTreble'' =
    let double x = 2 * x; treble x = 3 * x
    in treble . double
```

This illustrates that `let` introduces one or more new names bound to expressions `in` the scope of another expression. The value of the overall expression is that of the `in`-expression. The `doubleThenTreble''` version illustrates that multiple expressions can be collapsed onto a single line using a semicolon. They are otherwise identical.

There is yet another option:

```haskell
doubleThenTreble''' =
    let double x = 2 * x
    in let treble x = 3 * x
    in treble . double
```

This is semantically slightly different in that `treble` is not visible at the point at which the name `double` is assign. This is a _nested_ `let`-binding: `double x = 2 * x` is introduced into the scope of the expression `let treble x = 3 * x in treble . double` which itself introduces `treble x` into the scope of the expression `treble . double`.

It should be clear, therefore, that `let`&hellip;`in` is generally more powerful than `where` but that, in the simple case, they are equivalent. In this scenario, it is really a matter of personal preference: Should the names be (visually) introduced _before_ they're referenced or _after_ they're referenced.

[haskellcurry]: https://en.wikipedia.org/wiki/Haskell_Curry
[lambdacalculus]: https://en.wikipedia.org/wiki/Lambda_calculus
[mosesschoenfinkel]: https://en.wikipedia.org/wiki/Moses_Sch%C3%B6nfinkel
[seaofparentheses]: http://wiki.c2.com/?LostInaSeaofParentheses
