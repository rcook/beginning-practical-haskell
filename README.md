# Beginning Practical Haskell

An introductory Haskell programming course by [Richard Cook][rcookdotorg]

## Course contents

[Start here](index.md): this links to the course material as well as the setup
instructions to get a working Haskell development environment

## Generating the course notes

You'll only care about this section if you want to regenerate the course notes
in HTML, PDF, LaTeX or Microsoft Word format. For this you'll need
[Stack][stack] and [Pandoc][pandoc]:

```bash
stack install pandoc
```

### Linux/OS X

```bash
make
```

### Windows

```cmd
.\make.cmd
```

## Licence

[![Creative Commons Licence][cclicenceimage]][cclicence]

This work is licensed under a [Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International Licence][cclicence]. Code projects are separately released under the
[MIT License][mitlicense].

Copyright &copy; 2016, Richard Cook

[cclicence]: http://creativecommons.org/licenses/by-nc-nd/4.0/
[cclicenceimage]: https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png
[mitlicense]: https://opensource.org/licenses/MIT
[pandoc]: http://pandoc.org/
[rcookdotorg]: http://rcook.org/
[stack]: https://docs.haskellstack.org/
