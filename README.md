# Beginning Practical Haskell

An introductory Haskell programming course by [Richard Cook][rcookdotorg]

* [View Beginning Practical Haskell course online][course]

This is the source code for the course notes and code examples.

## Generating the course notes

The course notes are rendered using [Pandoc][pandoc] in the [`gh-pages`][gh-pages] branch of this repo. Please view the [`README.md`][gh-pages-readme] file in this branch for more information.

You can also [view the content directly in GitHub](index.md), but note that this is only an approximation to the Pandoc output. GitHub does not, for example, render MathJax/LaTeX markup and so some of the mathematics and other advanced markup features in this course may only be viewable in its intended form in the Pandoc output.

## Live preview

Live preview of this content is supported using [Pansite][pansite]. Pansite is not currently on [Stackage][stackage], so you'll need to build it yourself for now:

```
git clone https://github.com/rcook/pansite.git
cd pansite
stack install
```

This will install the `pansite-app` executable into your path. Then you can `cd` into this repo and run `pansite-app`:

```
cd beginning-practical-haskell
pansite-app
```

This will run a live preview on port 3000 by default: load `http://localhost:3000` into your browser to see what it looks like.

## Licence

[![Creative Commons Licence][cclicenceimage]][cclicence]

This work is licensed under a [Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International Licence][cclicence]. Code projects are separately released under the [MIT License][mitlicense].

Copyright &copy; 2016&ndash;2017, Richard Cook

[cclicence]: http://creativecommons.org/licenses/by-nc-nd/4.0/
[cclicenceimage]: https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png
[course]: https://rcook.github.io/beginning-practical-haskell
[gh-pages]: https://github.com/rcook/beginning-practical-haskell/tree/gh-pages
[gh-pages-readme]: https://github.com/rcook/beginning-practical-haskell/blob/gh-pages/README.md
[mitlicense]: https://opensource.org/licenses/MIT
[pandoc]: http://pandoc.org/
[pansite]: https://github.com/rcook/pansite
[rcookdotorg]: http://rcook.org/
[stackage]: https://www.stackage.org/
