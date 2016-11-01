# child-health-data

## Development

This project uses [Stack][stack].

## Prerequisites

### ICU

Linux (Centos):

```bash
yum install libicu-devel
```

Linux (Ubuntu):

```bash
apt-get install libicu-dev
```

OS X (using [Homebrew][homebrew]):

```bash
brew install icu4c
```

Windows (using [pacman][pacman] in Stack):

```cmd
stack exec -- pacman -Syu
stack exec -- pacman -Sy mingw64/mingw-w64-x86_64-icu
```

## Licence

Released under MIT License

Copyright (c) 2016 Richard Cook

[homebrew]: http://brew.sh/
[pacman]: https://wiki.archlinux.org/index.php/pacman
[stack]: https://haskellstack.org/
