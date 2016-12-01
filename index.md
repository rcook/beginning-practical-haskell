# Beginning Practical Haskell

An introductory Haskell programming course by [Richard Cook][rcookdotorg]

This course will build up enough knowledge and understanding of the Haskell programming language to tackle a real-world problem&mdash;something not difficult but not completely trivial. The problem I've chosen is one that I've often tackled, specifically web scraping: pulling web pages off the Internet and extract and manipulating numerical and textual data.

I wrote the original web scraper in Python in about two hours. The Haskell version took a wee while longer, clocking it at around eight, but is superior in nearly every respect.

## Overview

* Will teach the essentials required to write real programs in Haskell
* Will only dig into mathematical underpinnings as needed to understand real-world programming problems
* Will enable the student to use the tools needed to build real programs and consume Haskell libraries from third parties
* Intend to complement other courses offered by [Seattle Area Haskell Users' Group][seahug]

## <a name="prerequisites"></a>Prerequisites

* This course will use [Stack][stack]
* Please follow [setup instructions][stackhowto] to install Stack
* Make sure the following example based on the setup guide works:

```bash
stack new my-project simple --resolver=lts-7.8
cd my-project
stack setup
stack build
stack exec my-project-exe
```

If everything is working as expected, the last line should yield the output
`hello world`.

* This course will use the [LTS 7.8][lts78] snapshot of Stack which uses [GHC 8.0.1][ghc801]

## Contents

* [Part 1](part01.md)
    * What is Haskell?
    * Interactive Haskell
    * Type annotations
* [Part 2](part02.md)
    * Functions
* [Part 3](part03.md)
    * Algebraic data types
* [Part 4](part04.md)
    * I/O
* [Part 5](part05.md)
    * `do`-notation
* [Part 6](part06.md)
    * Other

> ***TODO:***
>
> ### Library selection
>
> * Making web requests (`wreq`)
> * Parsing HTML
> * Looping over things
> * Conversion from strings to int/double/%/CI
> * Handle noise/extraneous characters in parsing
> * Handling Unicode
> * Generating CSV or Excel files
> * File I/O
>
> ### Packages you'll learn about
>
> * Strings and text:
>   * `bytestring`
>   * `text`
>   * `text-icu-translit`
>   * `string-conv`
>   * `strings`
> * Specialized containers:
>   * `containers`
>   * `vector`
> * Parsing and serialization:
>   * `cassava`
>   * `taggy`
>   * `taggy-lens`
> * Network and file system:
>   * `directory`
>   * `filepath`
>   * `wreq`
> * Others:
>   * `extra`
>   * `lens`

[ghc801]: https://downloads.haskell.org/~ghc/master/users-guide/8.0.1-notes.html
[lts78]: https://www.stackage.org/lts-7.8
[rcookdotorg]: http://rcook.org/
[seahug]: http://seattlehaskell.org/
[stack]: https://docs.haskellstack.org/
[stackhowto]: https://docs.haskellstack.org/en/stable/README/#how-to-install
