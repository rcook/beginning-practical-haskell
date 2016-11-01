#!/bin/bash
cygpath -w $(dirname $(find /mingw64 -name 'libicu*' | head -1))