@echo off

cd install\build
call pretest.bat
if ERRORLEVEL 1 GOTO FINI

SET DELPHIVERSION=%1
if "%1" == "" SET DELPHIVERSION=newest

build.exe %DELPHIVERSION% "--make=installer"

:FINI
cd ..\..
