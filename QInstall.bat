@echo off

cd install\build
call pretest.bat
if ERRORLEVEL 1 GOTO FINI

SET DELPHIVERSION=%1
if "%1" == "" SET DELPHIVERSION=d7

build.exe %DELPHIVERSION% "--make=qinstaller"

:FINI
cd ..\..
