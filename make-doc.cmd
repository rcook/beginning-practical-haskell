@echo off
setlocal
set OUTDIR=doc
set PANDOCOPTSBASE=--highlight-style=tango
set PANDOCOPTSPDF=--latex-engine=xelatex --variable mainfont="Times New Roman" --template=my.latex
set INPUTPATHS=index.md part01.md part02.md part03.md part04.md
set PANDOCDIR=%APPDATA%\local\bin
set PANDOCPATH=%PANDOCDIR%\pandoc.exe

del %OUTDIR%\notes.html >nul 2>&1
del %OUTDIR%\notes.pdf >nul 2>&1
del %OUTDIR%\notes.tex >nul 2>&1

%PANDOCPATH% %PANDOCOPTSBASE% -s -o %OUTDIR%\notes.html %INPUTPATHS%
%PANDOCPATH% %PANDOCOPTSBASE% %PANDOCOPTSPDF% -s -o %OUTDIR%\notes.pdf %INPUTPATHS%
%PANDOCPATH% %PANDOCOPTSBASE% -s -o %OUTDIR%\notes.tex %INPUTPATHS%
