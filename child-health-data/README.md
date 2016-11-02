# child-health-data

## Development

This project uses [Stack][stack].

## Prerequisites

### ICU

#### Centos

Install the `libicu-devel` package system-wide:

```bash
yum install libicu-devel
```

Assuming the library and headers are installed into a sensible default location, that's it. If not, you'll need to modify your Stack user configuration file as described under [OS X](#osx) to set `extra-lib-dirs` and `extra-include-dirs` as appropriate.

#### Ubuntu

Install the `libicu-dev` package system-wide:

```bash
apt-get install libicu-dev
```

Assuming the library and headers are installed into a sensible default location, that's it. If not, you'll need to modify your Stack user configuration file as described under [OS X](#osx) to set `extra-lib-dirs` and `extra-include-dirs` as appropriate.

#### <a name="osx"></a> OS X (using [Homebrew][homebrew])

Install the `icu4c` package system-wide:

```bash
brew install icu4c
```

Modify your Stack user configuration file (usually to add the library and include directories for ICU to `extra-lib-dirs` and `extra-include-dirs`. This is typically done by adding the following to the end of your `~/.stack/config.yaml` file:

```yaml
extra-lib-dirs:
- /usr/local/opt/icu4c/lib

extra-include-dirs:
- /usr/local/opt/icu4c/include
```

#### Windows (using [pacman][pacman] in Stack)

Install the `mingw-w64-x86_64-icu` package into Stack's msys2 environment:

```cmd
stack exec -- pacman -Syu
stack exec -- pacman -Sy mingw64/mingw-w64-x86_64-icu
```

## Running the program

Linux/OS X:

```bash
stack exec child-health-data-app
```

Windows:

```cmd
.\child-health-data-app.cmd
```

## Licence

Released under MIT License

Copyright (c) 2016 Richard Cook

[homebrew]: http://brew.sh/
[pacman]: https://wiki.archlinux.org/index.php/pacman
[stack]: https://haskellstack.org/
