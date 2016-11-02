@echo off
setlocal
set OUTDIR=doc
set PANDOCOPTS=--highlight-style=tango
set INPUTPATHS=index.md part01.md part02.md part03.md part04.md
set PANDOCDIR=%APPDATA%\local\bin
set PANDOCPATH=%PANDOCDIR%\pandoc.exe
%PANDOCPATH% %PANDOCOPTS% -s -o %OUTDIR%\notes.html %INPUTPATHS%
