---
  title: "Addenda"
---

# Naming conventions

* Values and type variables start with initial lower-case letter
* Types and type classes start with initial capital letter
* Names typically employ medial capitalization, e.g. `MyType` and `myFunction` instead of `My_Type` and `my_function`, though this is not enforced
* Punctuation such as `'` allowed
* Some common, but not mandatory, suffixes exist including `M` and `M_` etc., e.g. `foldM`, `mapM` and `forM_`
* Operators are no different from functions except their names are spelt with symbol characters and can be used infix
* Backticks can be used to use regular functions infix while parentheses can be employed to use operators in function-style prefix position
* Type and data constructors are separate namespaces and can, therefore, share names
* It is not uncommon to see type constructors with identically named data constructors, which we'll see later

# Pronunciation

| Symbol | Pronunciation |
|--------|:-------------:|
| `=>`   | implies       |
| `->`   | to, maps to   |


# Further Discussion

## Mathematical Functions

Mathematical functions define relationships between input and output.  Accordingly, all mathematical functions can be represented as a table of pairs of values.  For example `f(x) = x + 1` can be represented:

|x|f(x)|
|---|---|
|0|1|
|1|2|
|2|3|



