@echo off
setlocal
set OUTDIR=doc
set PANDOCOPTS=
set PANDOCOPTS=%PANDOCOPTS% --highlight-style=tango
set PANDOCOPTS=%PANDOCOPTS% --latex-engine=xelatex
set PANDOCOPTS=%PANDOCOPTS% --variable mainfont="Times New Roman"
set PANDOCOPTS=%PANDOCOPTS% --variable monofont=Menlo
set BASENAME=notes
set INPUTPATHS=
set INPUTPATHS=%INPUTPATHS% index.md
set INPUTPATHS=%INPUTPATHS% part01.md
set INPUTPATHS=%INPUTPATHS% part02.md
set INPUTPATHS=%INPUTPATHS% part03.md
set INPUTPATHS=%INPUTPATHS% part04.md
set PANDOCDIR=%APPDATA%\local\bin
set PANDOCPATH=%PANDOCDIR%\pandoc.exe

call :CleanAndMake %OUTDIR%\%BASENAME%.html
call :CleanAndMake %OUTDIR%\%BASENAME%.pdf
call :CleanAndMake %OUTDIR%\%BASENAME%.tex
echo Done
goto :eof

:CleanAndMake
echo Creating %~f1...
del "%~f1" >nul 2>&1
%PANDOCPATH% %PANDOCOPTS% -s -o "%~f1" %INPUTPATHS%
echo Created
goto :eof
