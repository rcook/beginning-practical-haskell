#!/bin/bash
icu_bin_path=$(dirname $(find /mingw64 -name 'libicu*' | head -1))
PATH=$icu_bin_path:$PATH child-health-data-app.exe $*