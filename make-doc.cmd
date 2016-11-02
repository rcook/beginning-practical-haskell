@echo off
setlocal
set OUTDIR=doc
set PANDOCOPTS=--highlight-style=tango --latex-engine=xelatex --variable mainfont="Times New Roman"
set INPUTPATHS=index.md part01.md part02.md part03.md part04.md
set PANDOCDIR=%APPDATA%\local\bin
set PANDOCPATH=%PANDOCDIR%\pandoc.exe
%PANDOCPATH% %PANDOCOPTS% -s -o %OUTDIR%\notes.html %INPUTPATHS%
%PANDOCPATH% %PANDOCOPTS% -s -o %OUTDIR%\notes.pdf %INPUTPATHS%
