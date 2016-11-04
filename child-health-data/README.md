# Child Health Data Scraping Tool

* A simple web-scraping tool written in [Haskell][haskell]
* Written [Richard Cook][rcookdotorg]

## Prerequisites

### Stack

Install [Stack][stack]. This'll take care of the Haskell bits.

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

## Setting up runtime environment

On Windows, you'll need to run `script\env.cmd` (Windows command prompt) or `script\env.ps1` (Powershell) to set up the `PATH` environment variable so that various libraries (e.g. ICU) can be located at runtime:

```cmd
script\env.cmd
```

or

```ps
script\env.ps1
```

On other platforms, assuming your installations of ICU etc. are system-wide, you won't need to adjust your `PATH`.

## Building

```bash
stack setup
stack build
```

## Running the program

```bash
stack exec child-health-data-app
```

## Rebuilding automatically on file changes

```bash
stack build --file-watch --exec child-health-data-app
```

## Licence

Released under MIT License

Copyright &copy; 2016 Richard Cook

[haskell]: https://www.haskell.org/
[homebrew]: http://brew.sh/
[pacman]: https://wiki.archlinux.org/index.php/pacman
[rcookdotorg]: http://rcook.org/
[stack]: https://haskellstack.org/
