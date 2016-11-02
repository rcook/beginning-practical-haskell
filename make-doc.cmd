@echo off
setlocal
set OUTDIR=doc
set PANDOCOPTS=--highlight-style=tango --latex-engine=xelatex --variable mainfont="Times New Roman"
set INPUTPATHS=index.md part01.md part02.md part03.md part04.md
set PANDOCDIR=%APPDATA%\local\bin
set PANDOCPATH=%PANDOCDIR%\pandoc.exe

del %OUTDIR%\notes.html >nul 2>&1
del %OUTDIR%\notes.pdf >nul 2>&1
del %OUTDIR%\notes.tex >nul 2>&1

%PANDOCPATH% %PANDOCOPTS% -s -o %OUTDIR%\notes.html %INPUTPATHS%
%PANDOCPATH% %PANDOCOPTS% -s -o %OUTDIR%\notes.pdf %INPUTPATHS%
%PANDOCPATH% %PANDOCOPTS% -s -o %OUTDIR%\notes.tex %INPUTPATHS%
