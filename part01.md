---
title: "Part 1: Introduction"
---

In this introduction, we'll get acqainted with Haskell in three indispensable ways. First, through a _very brief_ overview of its features to orient us in our study. Then we'll jump into a Haskell REPL to see some simple Haskell expressions, but also to practice using the REPL as a development tool. Finally, we'll work with a source file, and create a minimalist project with Stack.

# What is Haskell?

*[Sources: [1][haskellwikifp], [2][wikipediahaskell], [3][evaluation]]*

## Purely functional

Functions are first-class citizens that can be passed as arguments to other functions, "returned" from other functions, and stored in data structures, e.g. lists. (The notion of returning a value belongs to imperative languages, the appropriate term is _evaluates to_.)

In Haskell, functions closely resemble [mathematical functions][mathematicalfunctions]: given any input value, they always return the same output; this is called [referential transparency][referentialtransparency]. Functions typically operate on _immutable_ data, and do not have side effects.

Referential transparency means that the compiler is free to do all kinds of optimization such as interleaving and [inlining][inlining] etc. which typically require additional annotations or data flow analysis in compilers for languages such as C++ and Java.

## Non-strict

Function arguments not evaluated unless they're actually used (a strict function is one which always evaluates all of its arguments). Non-strictness allows for _lazy evaluation_ (note the [difference][nonstrictversuslazy] between non-strictness and lazy evaluation).  Haskell employs lazy evaluation by default, but has annotations for strict evaluation where necessary. Lazy evaluation allows control structures to be built from user-defined functions.

## Statically typed

Haskell is a staticaly typed language, which allows the compiler to catch many kinds of programmer error at _compile time_.  But Haskell's type system is highly expressive, and allows programmer lots of power and flexibility.  The expressivity of the type system itself -- the same machanism which provides safety and certain guarantees about the program -- is one of the features which sets Haskell apart from other languages.

![Simon Peyton Jones's "Region of Abysmal Pain" Venn diagram][spjvenn]

## Call-by-need

Haskell uses a call-by-need evaluation strategy, which is effectively [call-by-name][callbyname] with memoization.  Call-by-name is an evaluation strategy where a function's arguments aren't evaluated prior to the function's call, but are substituted into the body of the function itself. With call-by-need, _if a function argument is evaluated_ its value is stored for future uses.

## Minimalist syntax

Haskell's syntax largely follows from the privileged role of functions, thus functions assume the simplest and least-decorated place in Haskell's syntax.  A mathematical function such as `f(x) = x` is rendered `f x = x` in Haskell; no elipses wrapping arguments, and no braces closing the function body.

In addition, Haskell uses an [off-side rule language][offsiderule] syntax, much like Python; most of the time the required indentation is what feels right.

## Garbage collected

Haskell employs its own garbage collection to manage memory. Haskell computations can produce a lot of memory garbage, partly as a consequence of non-strict evaluation which involves accumulation of _thunks_ in memory, and partly a result of immutability. However, the GHC runtime's GC is highly tuned for this behaviour.   Haskell does afford manual management of external resources such as externally allocated memory or resource handles.

# Our first Haskell code

Let's open a Haskell REPL and a project with a source file to work with.

## GHCi

Next  we'll start up GHCi, the interactive Haskell interpreter.

Run `stack ghci` and you'll see a prompt `Prelude>`.  GHC stands for the Glasgow Haskell Compiler, GHCi is GHC _Interactive_, GHC's read-evaluate-print-loop (REPL).

You can configure the GHCi prompt by entering `:set prompt "ghci> "` into GHCi. You can also enter this line into a `.ghci` dotfile in your home directory. Here we've configured the prompt to display `λ>` for pertinence and brevity.

For our first steps using Haskell, let's assign some values and evaluate some _expressions_.

```bash
λ> x = 5
```

This assigns name `x` to value `5`.

Note that we say "assigns name `foo` to `bar`" as opposed to "assigns value `bar` to `foo`. In imperative programming languages `=` or equivalent operators typically perform _assignment_ and the different values can be assigned to existing names from time to time. In Haskell, the name `foo` is _defined to be_ the `value` in the equational sense of `=`: it's a definition and this is at the root of [equational reasoning][equationalreasoning].

```bash
λ> x = 5
λ> y = 6
λ> z = x + y
```
Now we've assigend the name `z` to the expression  `x + y`, but we still haven't seen any evaluation.

```bash
λ> z
11
```
Here the expression `z` is evaluated.

We can use GHCi to do more than assignments and evaluations. We can also retrieve information about values and expressions:

```bash
λ> :type z
z :: Num a => a
```

We just asked GHC to tell us the `type` of the expression `z` and it printed out the type signature for `z`.  `Num a` is the type class `Num` with one type variable `a`. More on this later. Note, however, that we did not specify a type for `z`, the compiler inferred it. In the absence of type annotations&mdash;which we'll cover later&mdash;GHCi will typically assign the most general type possible to an expression, subject to certain rules.

GHCi will assign exactly one type to a given expression. Despite the absence of explicit type annotations in this example, the expressions are still strongly and statically typed.

A type signature consists of:

* One or more constraints to the left of `=>` (such constraints are optional)
* One or more types separated by `->`

<span class="marginnote">Types and [_type classes_][typeclasses] always spelt with initial upper-case letter. Type variables always spelt with initial lower-case letter.</span>

Let's look at another type:

```bash
λ> z = "hello"
λ> z
hello
λ> :t z
z :: [Char]
```

Here we've assigned `z` to "hello", evaluated it, and used the shorthand `:t` for `:type` to show its type signature.  The type `[Char]` is a list of `Char`. 

```bash
λ> :t (+)
(+) :: Num a => a -> a -> a
```

Here are other GHCi commands:

| Command | Alias | Use    | 
|---------|---------|-------|
| `:type` | `:t`  | type signature of the given value or  expression |
| `:info` | `:i` | information about the given name |
| `:kind` | `:k` | information about the kind of the given type |
| `:quit` | `:q` | quit ghci |

GHCi has many other commands, which you can peruse [here][ghcicommands].

### Exercises 

#### Use GHCi with to explore different values and expressions

Familiarize yourself with GHCi by investigating the following expressions in GHCi with `:type` and `:info` and `:kind`

1. `4`, `8 / 4`, and `4.0`
2. `x = 42 + 3 ^ 2`
3. `(+)` and `(-)`
4. `(*)` and `(/)`
5. `(^)`
6. `True` and `False`
7. `&&` and `||`
8. `f a = a + 1`
9. `g a b = a + b`
10. `Num`
11. `Integer`

## Create a project with Stack

Now that we have some familiarity with the REPL, let's look at how to work with a source file.

As mentioned earlier, the examples in this course use Stack to run Haskell.  Here are [setup instructions][stacksetup].

Stack provides templates for bootstrapping projects. In a terminal or command prompt create a brand-new Stack project named `hello-world`:

```bash
stack new hello-world simple --resolver=lts-7.8
cd hello-world
```

The `simple` template is one of the simplest-possible Haskell projects: a project with a single executable target with the same name as the project itself, i.e. `hello-world` in this case. It consists of the following:

* `LICENSE`: a licence file (BSD-compatible, by default)
* `Setup.hs`: a Haskell program used to pull in and build external project dependencies such as native libraries etc.
* `hello-world.cabal`: the Cabal file, which is akin to a project file in Visual Studio etc.: this defines various metadata for the project including an `executable` target
* `src/Main.hs`: a simple starter source file
* `stack.yaml`: a Stack-specific configuration file

Because this project has only one source file, you can run that source file directly with `stack runhaskell src/Main.hs`, and get "hello world" triumphantly printed to the terminal. A more standard procedure for a project would be to `stack install` to resolve dependencies, then `stack build` to compile, and finally `stack exec hello-world`.

## Your first Haskell source file

Now we'll create a source file and write similar code. We'll then run this through a compiler and execute it. In your favourite editor, open a new file `Hello.hs` in the existing `hello-world` project directory and type the following text into it:

```haskell
x = 5
y = 6
z = x + y

main = print z
```

Most things you type into GHCi are valid lines of code in a Haskell source file.  In order to be able to run a program, a Haskell program must have exactly one function named `main` in the `Main` module (or unnamed module) and must have an `IO` type. `print` is a function that takes as an argument any value that has an instance of the `Show` type class: we'll talk about type classes more later.

Now we can run the program as follows:

```bash
stack runhaskell Hello.hs
```

Now we'll change `Hello.hs` to the following to mimic our GHCi example:

```haskell
x = 5
y = 6
z = x + y
z = "hello"

main = print z
```

And we'll try to run it again:

```console
> stack runhaskell Hello.hs

Hello.hs:4:1: error:
    Multiple declarations of ‘z’
    Declared at: Hello.hs:3:1
                 Hello.hs:4:1
```

## Differences between GHC and GHCi

There are naturally many differences between the interactive and non-interactive Haskell environments. The most important ones for our immediate purposes are:

* Names can be _shadowed_ in GHCi: i.e. we can introduce a new `z` that _hides_ the previous definition with name `z`
* In Haskell source files, a top-level name can be used exactly once
* [Shadowing][shadowing] is allowed within Haskell source files, specifically within nested lexical scopes
* In fact, that is exactly what's happening in GHCi: each line entered at the prompt is effectively a new lexical scope nested within the previous lexical scope

# A more realistic example

*[Sources: [1][haskellnumbers]]*

So far we've only seen type signatures when querying GHCi, but we can provide type annotations to expressions to explicitly declare types.  Let's do this in GHCi and then in a source file.

Start up GHCi again:

```ghci
λ> x :: Integer; x = 5
λ> y :: Integer; y = 6
λ> z :: Integer; z = x + y
λ> z
11
λ> :t x
x :: Integer
λ> :t y
y :: Integer
λ> :t z
z :: Integer
λ> a = 5
λ> :t a
a :: Num t => t
```

Now do the same in our source file:

```haskell
x :: Integer
x = 5

y :: Integer
y = 6

z :: Integer
z = x + y

main :: IO ()
main = print z
```

Consider the contrasting result, in our GHCi example, for type of `a` as `a :: Num t = t`:

* Lower-case `t` is a _type variable_ and can be any type that fulfils the type constraints
* `Num t` _constrains_ `t` to be some type which is an instance of the `Num` type class
* `Num` has instances for (or "is implemented by") all primitive numeric types in Haskell

Consider the type of `x`, `y` and `z`:

* These have no `=>` and, therefore, no type constraints
* Upper-case `Integer` is a _concrete type_ corresponding to arbitrary-precision integers: this is an _instance_ of `Num`

We'll talk about the `IO ()` type in the next lesson.

## When to use type annotations

Haskell has powerful type inference, designed so that usually you won't need them.  But sometimes ambiguities arise, and some more [advanced language features][dependenttypes] make ambiguities more likely.  Even so, type annotations are useful as documentation and for type-driven development, and most experienced Haskell developers recommend that all _top-level_ definitions should carry a type annotation.

[dependenttypes]: https://wiki.haskell.org/Dependent_type
[equationalreasoning]: http://www.haskellforall.com/2013/12/equational-reasoning.html
[evaluation]: http://dev.stephendiehl.com/fun/005_evaluation.html
[haskellnumbers]: https://www.haskell.org/tutorial/numbers.html
[haskellwikifp]: https://wiki.haskell.org/Functional_programming
[offsiderule]: https://en.wikipedia.org/wiki/Off-side_rule
[pronunciation]: https://wiki.haskell.org/Pronunciation
[spjvenn]: images/region-of-abysmal-pain.png
[typeclasses]: https://www.haskell.org/tutorial/classes.html
[wikipediahaskell]: https://en.wikipedia.org/wiki/Haskell_(programming_language)
[callbyname]: https://en.wikipedia.org/wiki/Evaluation_strategy#Call_by_name
[mathematicalfunctions]: addenda#mathematical-functions
[inlining]: https://en.wikipedia.org/wiki/Inline_expansion
[referentialtransparency]: https://en.wikipedia.org/wiki/Referential_transparency
[stacksetup]: https://docs.haskellstack.org/en/stable/README/#how-to-install
[ghcicommands]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#ghci-commands
[shadowing]: https://en.wikipedia.org/wiki/Variable_shadowing
[nonstrictversuslazy]: https://stackoverflow.com/questions/7140978/haskell-how-does-non-strict-and-lazy-differ#7141537
