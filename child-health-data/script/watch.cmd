@echo off
setlocal
pushd %~dp0..
stack build --file-watch --exec "sh script/exec.sh %*"
