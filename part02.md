[Contents](index.md) | [P1](part01.md) | **P2** | [P3](part03.md) | [P4](part04.md)

> ***TODO:***
>
> * List Haskell's primitive types, especially `Int` so that the `Colour` example below makes more sense.
>* More on functions so that `Int -> Int -> Int` etc. makes more sense.

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

This version of `Colour` is a [_product type_][producttype].

## Sum types

* Equivalent to tagged union, variant, discriminated union etc. in other languages

## Product types

*[Sources: [1][cardinalityproof]]*

* Isomorphic to strongly-typed tuples
* So-called because the _cardinality_ (i.e. size or number of inhabitants) of a type is equal to the product of the cardinality of each of its component types
* Thus equivalent to a Cartesian product of finite sets
* For the Cartesian product $S \times T$ of two finite sets $S$ and $T$, $\left|{S \times T}\right| = \left|{S}\right| \times \left|{T}\right|$ where $\left|{S}\right|$ denotes cardinality
* Equivalent to records and structures in other languages

[cardinalityproof]: https://proofwiki.org/wiki/Cardinality_of_Cartesian_Product
[datadecl]: http://stackoverflow.com/questions/18204308/haskell-type-vs-data-constructor
[producttype]: https://en.wikipedia.org/wiki/Product_type
[taggedunion]: https://en.wikipedia.org/wiki/Tagged_union
