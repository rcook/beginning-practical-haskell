# Beginning Practical Haskell

An introductory Haskell programming course by [Richard Cook][rcookdotorg]

This is the source code for the course notes. To view the latest version of the course online, please navigate to the following link:

* [View course notes online][notes]

## Course contents

The course notes are intended to be rendered into a final output format using [Pandoc][pandoc] as described [below](#generatingcoursenotes). You can also [view the content directly in GitHub](index.md), but note that this is only an approximation to the Pandoc output. GitHub does not, for example, render MathJax/LaTeX markup and so some of the mathematics and other advanced markup features in this course may only be viewable in its intended form in the Pandoc output.

## <a name="generatingcoursenotes"><a> Generating the course notes

To regenerate the course notes in HTML, PDF, LaTeX or Microsoft Word format, you'll need to [install Pandoc][pandocinstall]. This is probably the easiest method, though I haven't tried it myself.

If you have a working [Stack][stack] installation, you can install Pandoc this way. This is what I do:

```bash
stack install pandoc
```

### Linux/OS X

To build HTML/PDF/LaTeX/Word output files:

```bash
make
```

### Windows

To build HTML/PDF/LaTeX/Word output files:

```cmd
.\make.cmd
```

## Licence

[![Creative Commons Licence][cclicenceimage]][cclicence]

This work is licensed under a [Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International Licence][cclicence]. Code projects are separately released under the [MIT License][mitlicense].

Copyright &copy; 2016, Richard Cook

[cclicence]: http://creativecommons.org/licenses/by-nc-nd/4.0/
[cclicenceimage]: https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png
[mitlicense]: https://opensource.org/licenses/MIT
[notes]: https://rcook.github.io/beginning-practical-haskell
[pandoc]: http://pandoc.org/
[pandocinstall]: http://pandoc.org/installing.html
[rcookdotorg]: http://rcook.org/
[repo]: https://github.com/rcook/beginning-practical-haskell
[stack]: https://docs.haskellstack.org/
