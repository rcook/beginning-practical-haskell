@echo off
setlocal

if defined CHILDHEALTHDATAENV (
  goto :eof
)

call :SetICUBINPATHFILE "%~dp0../.stack-work/icu-bin-path.txt"

pushd "%~dp0"
if not exist "%ICUBINPATHFILE%" (
  stack exec -- sh get-icu-bin-path.sh > "%ICUBINPATHFILE%"
)

set /p ICUBINPATH=<"%ICUBINPATHFILE%"

rem Export PATH to global scope
for /f "delims=" %%i in ('echo "%ICUBINPATH%;%PATH%"') do endlocal & set CHILDHEALTHDATAENV=1 & set PATH=%%~i

goto :eof

:SetICUBINPATHFILE
set ICUBINPATHFILE=%~f1
goto :eof
