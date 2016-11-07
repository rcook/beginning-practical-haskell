[Contents](index.md) | [P1](part01.md) | **P2** | [P3](part03.md) | [P4](part04.md)

> ***TODO:***
>
> * List Haskell's primitive types, especially `Int` so that the `Colour` example below makes more sense.
>* More on functions so that `Int` $\rightarrow$ `Int` $\rightarrow$ `Int` etc. makes more sense.

# Algebraic data types

*[Sources: [1][datadecl]]*

Consider the following `data` declaration:

```haskell
data Colour = Red | Green | Blue
```

`Colour` is the _type constructor_ and appears on the left-hand side of the `=` sign. This type has three _data constructors_, named `Red`, `Green` and `Blue` which appear on the right-hand side of the `=` sign. You use type constructors where you'd expect a type (e.g. in type annotations) and data constructors where you'd expect a value. `Colour` is a type while `Red`, `Green` and `Blue` contain values of type `Colour`. This is known as a [_sum type_][taggedunion].

Let's try a more interesting version of this type:

```haskell
data Colour = RGB Int Int Int
```

`Colour` is still a type. However, `RGB` is not a value. It is a function taking three `Int`s and returning a value of type `Colour`:

Input                                     | Output                                                            | Comment
:-----------------------------------------|:------------------------------------------------------------------|:-------
`λ> data Colour = RGB Int Int Int`        |                                                                   | Defines type `Colour`
`λ> :info Colour`<br>or<br>`λ> :i Colour` | `data Colour = RGB Int Int Int   -- Defined at <interactive>:2:1` | Shows information about type `Colour`
`λ> :t RGB`                               | `RGB :: Int -> Int -> Int -> Colour`                              | Shows type of `RGB`
`λ> let c = RGB 10 20 30`                 |                                                                   | Assigns name `c` to a `Colour` value
`λ> :t c`                                 | `c :: Colour`                                                     | Shows type of `c`
`λ> c` | `<interactive>:13:1:`<br><code>&nbsp;&nbsp;&nbsp;&nbsp;No instance for (Show Colour) arising from a use of &#96;print'</code><br><code>&nbsp;&nbsp;&nbsp;&nbsp;In a stmt of an interactive GHCi command: print it</code> | What?
`λ> data Colour = RGB Int Int Int deriving Show`<br>`λ> let c = RGB 10 20 30`<br>`λ> c` | `RGB 10 20 30`      | That's better!

This version of `Colour` is a [_product type_][producttype]. It is isometric to a 3-tuple, or triple, of `Int`s.

## Sum types

* Equivalent to tagged union, variant, discriminated union etc. in other languages

## Product types

*[Sources: [1][cardinalityproof]]*

* Isomorphic to strongly-typed tuples
* So-called because the _cardinality_ (i.e. size or number of inhabitants) of a type is equal to the product of the cardinality of each of its component types
* Thus equivalent to a Cartesian product of finite sets
* For the Cartesian product $S \times T$ of two finite sets $S$ and $T$, $\left|{S \times T}\right| = \left|{S}\right| \times \left|{T}\right|$ where $\left|{S}\right|$ denotes cardinality
* Equivalent to records and structures in other languages

## <a name="recordsyntax"></a> Records

Another way of defining a product type with the convenience of automatically-generated accessor functions, Haskell has _record_ syntax:

```haskell
data Colour = RGB { red :: Int, green :: Int, blue :: Int }
```

Just like the previous product type definition of `Colour`, this definition consists of a triple of `Int`s. Similarly, `RGB` is a data constructor of type `Int` $\rightarrow$ `Int` $\rightarrow$ `Int` $\rightarrow$ `Colour`. However, this definition also names the three components and generates accessor functions for them, as you can convince yourself by defining `Colour` in GHCI and using `:t` on `red`, `green`, `blue`. Each has type `Colour` $\rightarrow$ `Int`: i.e. each is a function taking a `Colour` and returning an `Int`.

Similarly, we might define a "pair" type as follows:

Input | Output | Comment
:-----|:-------|:-------
data Pair a b = Pair { first :: a, second :: b }

> ***TODO:***
> Complete `Pair` walkthrough

## Pattern matching

Given `Colour` defined as a product type without using record syntax, how do we extract the component values? This is where "pattern matching" comes in. Pattern matching is a mechanism for _deconstructing_ Haskell values, so-called because the patterns mimic the _data constructor_ invocation used to construct the value initially. Consequently, the runtime representation of values of product types retain sufficient information to allow code to determine _how_ a value was constructed at runtime.

There are two distinct places where you'll see pattern matching:

* In function definitions, used to deconstruct function arguments
* In `case` expressions, used to deconstruct arbitrary values

### Pattern matching in function definitions

This is equivalent to the code that the Haskell compiler generates for record accessor functions described [previously](#recordsyntax). Here's an example using our trusty `Colour` data type:

```haskell
data Colour = RGB Int Int Int

red :: Colour -> Int
red (RGB r _ _) = r

green :: Colour -> Int
green (RGB _ g _) = g

blue :: Colour -> Int
blue (RGB _ _ b) = b
```

* `_` ("unknown") is a "throwaway" name: it matches a value but does not assign a name to it which is useful when we don't care about that specific value
* You'll see it used a lot
* It can also be used to represent a [typed hole][typedholes]
* `RGB r _ _` matches the value of type `Colour` on its `RGB` data constructor of type `Int` $\rightarrow$ `Int` $\rightarrow$ `Int` $\rightarrow$ `Colour`, matching `r` to the first value of the triple and ignoring the second and third values
* Interestingly, "regular" function argument names are really just a degenerate case of pattern matching

### Pattern matching using `case`

An alternative implementation can make use of a `case` expression:

```haskell
data Colour =
    RGB Int Int Int |
    CMYK Float Float Float Float

colourModelV1 :: Colour -> String
colourModelV1 (RGB _ _ _) = "RGB"
colourModelV1 (CMYK _ _ _ _) = "CMYK"

colourModelV2 :: Colour -> String
colourModelV2 c =
    case c of RGB _ _ _ -> "RGB"
              CMYK _ _ _ _ -> "CMYK"

main :: IO ()
main =
    let c1 = CMYK 0.5 0.5 0.5 0.0
        cm1 = colourModelV1 c1
        c2 = RGB 50 100 150
        cm2 = colourModelV2 c2
    in putStrLn ("cm1=" ++ cm1 ++ ", cm2=" ++ cm2)
```

This will yield `cm1=CMYK, cm2=RGB`.

Patterns can be matched both in function argument position and in `case` expressions. Here's a contrived example using our `RGB`/`CMYK` definition of `Colour`:

```haskell
data Point = Point { x :: Int, y :: Int }

data Line = Line
  { start :: Point
  , end :: Point
  , thickness :: Int
  , colour :: Colour
  }

lineRedness :: Line -> Int
lineRedness (Line _ _ _ (RGB r _ _)) = r

main :: IO ()
main =
    let l = Line
                (Point 10 10)
                (Point 50 50)
                1
                (RGB 255 0 0)
    in print (lineRedness l)
```

This will output `255`.

This example illustrates another important aspect of pattern matching, namely _exhaustiveness_ of our pattern matching. Let's change our `main` function to the following to illustrate this:

```haskell
main :: IO ()
main =
    let l = Line
                (Point 10 10)
                (Point 50 50)
                1
                (CMYK 0.5 0.5 0.5 0.0)
    in print (lineRedness l)
```

Let's compile and run this. Here's the output:

```text
scratch: src/Main.hs:15:1-40: Non-exhaustive patterns in function lineRedness
```

Well, that's interesting but makes sense. The pattern in the definition of `lineRedness` cannot match the value `CMYK 0.5 0.5 0.5 0.0` since it was not constructed using the `RGB` data constructor.

Such a problem can be addressed in one of [several ways][haskellerrorreporting]:

* Provide a pattern match on the `CMYK` data constructor: this would require a valid conversion from the `CMYK` representation of a colour to `RGB` in order to provide a red component
* Provide a fall-through case using `_` to match any `Colour` value after the `RGB` match: this would require the existence of some "default" notion of line redness
* Augment the return type to include the notion of failure

The option you will choose will depend primarily on the semantics of the function. To make this determination, you'd need to ask questions such as:

* Is there a valid "conversion" from the other data constructors?
* Is there a semantically reasonable default value?

I'll illustrate these three alternatives to the existing behaviour. Note that there are also other ways to provide different runtime error messages through Haskell's various [exception mechanisms][haskellisexceptionallyunsafe] etc. We will discuss these more when we get to I/O.

Here we pattern-match on `CMYK` and apply a [formula][cmytorgb]:

```haskell
lineRedness :: Line -> Int
lineRedness (Line _ _ _ (RGB r _ _)) = r
lineRedness (Line _ _ _ (CMYK c _ _ _)) = round ((1.0 - c) * 255.0)
```

Look sideways and ignore the fact that we're ignoring the K-component!

Here we provide a catch-all or fall-through case:

```haskell
defaultRed :: Int
defaultRed = 0

lineRedness :: Line -> Int
lineRedness (Line _ _ _ (RGB r _ _)) = r
lineRedness _ = defaultRed
```

Here we augment the return type:

```haskell
lineRedness :: Line -> Maybe Int
lineRedness (Line _ _ _ (RGB r _ _)) = Just r
lineRedness _ = Nothing
```

We'll definitely talk more about `Maybe` later. Suffice it to say for now that it's a type constructor taking one type variable and is analogous to [option types][optiontypes] and [nullable types][nullabletypes] in other languages.

> ***TODO:***
> Discuss catching non-exhaustive matches at compile time
> This is a bit of wart on Haskell

[cardinalityproof]: https://proofwiki.org/wiki/Cardinality_of_Cartesian_Product
[cmytorgb]: http://www.easyrgb.com/index.php?X=MATH&H=12#text12
[datadecl]: http://stackoverflow.com/questions/18204308/haskell-type-vs-data-constructor
[haskellerrorreporting]: http://www.randomhacks.net/2007/03/10/haskell-8-ways-to-report-errors/
[haskellisexceptionallyunsafe]: https://existentialtype.wordpress.com/2012/08/14/haskell-is-exceptionally-unsafe/
[optiontypes]: https://en.wikipedia.org/wiki/Option_type
[nullabletypes]: https://en.wikipedia.org/wiki/Nullable_type
[producttype]: https://en.wikipedia.org/wiki/Product_type
[taggedunion]: https://en.wikipedia.org/wiki/Tagged_union
[typedholes]: https://wiki.haskell.org/GHC/Typed_holes
