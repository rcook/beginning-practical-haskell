@echo off
setlocal
pushd %~dp0
for /f %%i in ('stack exec -- sh script/get-icu-bin-path.sh') do set ICUBINPATH=%%i
PATH=%ICUBINPATH%;%PATH%
stack exec child-health-data-app.exe %*
