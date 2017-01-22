# Beginning Practical Haskell

An introductory Haskell programming course by [Richard Cook][rcookdotorg]

This [branch][gh-pages] contains the [Pandoc][pandoc]-rendered output from the course's [source code][github].

## Generating the course notes

I generally rebuild my notes on a machine running Mac OS X or some Linux. These steps can be made to work with Windows using an [MSYS2][msys2] environment if you're really determined!

### Install GNU Make

Course note generation is driven by a `Makefile`. Make sure you have [GNU Make][gnumake] installed using your standard package manager.

### Install Pandoc

This project uses Pandoc to generate notes in various formats from Markdown sources. You'll need to [install Pandoc][pandocinstall].

### Clone and switch branch

You'll need a clone of the repo checked out in the `gh-pages` branch with submodules initialized:

```bash
git clone https://github.com/rcook/beginning-practical-haskell.git
cd beginning-practical-haskell/
git checkout gh-pages
git submodule update --init
```

### Build the web site

To regenerate the web version of the course notes:

```bash
make web
```

### Build "print" versions of the notes

To regenerate the course notes in Microsoft Word, PDF or LaTeX format:

```bash
make out/notes.docx
make out/notes.pdf
make out/notes.tex
```

### Build and clean everything

To clean and build everything:

```bash
make rebuild
```

## Utilities

### `grip`

Run `grip` to preview changes to `README.md` as it will be rendered in GitHub:

```bash
script/grip
```

### `server`

Run `server` to preview the HTML output as it will appear on GitHub Pages:

```bash
script/server
```

### `update`

The `update` script will sync the latest changes from the main source repo and rebuild all targets.

```bash
script/update
```

## Licence

[![Creative Commons Licence][cclicenceimage]][cclicence]

This work is licensed under a [Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International Licence][cclicence]. Code projects are separately released under the [MIT License][mitlicense].

Copyright &copy; 2016&ndash;2017, Richard Cook

[cclicence]: http://creativecommons.org/licenses/by-nc-nd/4.0/
[cclicenceimage]: https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png
[gh-pages]: https://github.com/rcook/beginning-practical-haskell/tree/gh-pages
[github]: https://github.com/rcook/beginning-practical-haskell
[gnumake]: https://www.gnu.org/software/make/
[mitlicense]: https://opensource.org/licenses/MIT
[msys2]: https://msys2.github.io/
[pandoc]: http://pandoc.org/
[pandocinstall]: http://pandoc.org/installing.html
[rcookdotorg]: http://rcook.org/
