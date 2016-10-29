# Beginning Practical Haskell

## Overview

* Will teach the essentials required to write real programs in Haskell
* Will only dig into mathematical underpinnings as needed to understand real-world programming problems
* Will enable the student to use the tools needed to build real programs and consume Haskell libraries from third parties
* Intend to complement other courses offered by [Seattle Area Haskell Users' Group][seahug]

## Prerequisites

* This course will use [Stack][stack]
* Please follow [setup instructions][stackhowto] to install Stack
* Make sure the example in the setup guide works:

```bash
stack new my-project
cd my-project
stack setup
stack build
stack exec my-project-exe
```

* This course will use the [LTS 7.5][lts75] snapshot of Stack which uses [GHC 8.0.1][ghc801]

[ghc801]: https://downloads.haskell.org/~ghc/master/users-guide/8.0.1-notes.html
[lts75]: https://www.stackage.org/lts-7.5
[seahug]: http://seattlehaskell.org/
[stack]: https://docs.haskellstack.org/
[stackhowto]: https://docs.haskellstack.org/en/stable/README/#how-to-install