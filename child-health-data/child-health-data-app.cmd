@echo off
setlocal
set ICUBINPATHFILE=.stack-work/icu-bin-path.txt
pushd %~dp0
if not exist %ICUBINPATHFILE% (
  stack exec -- sh script/get-icu-bin-path.sh > %ICUBINPATHFILE%
)
set /p ICUBINPATH=<%ICUBINPATHFILE%
PATH=%ICUBINPATH%;%PATH%
stack exec child-health-data-app.exe %*
