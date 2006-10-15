@echo off

cd install\build
call pretest.bat
if ERRORLEVEL 1 GOTO FINI

SET DELPHIVERSION=%1
if "%1" == "" SET DELPHIVERSION=newest

build.exe %DELPHIVERSION% "--make=installer"
if ERRORLEVEL 1 GOTO FINI

echo Launching JCL installer ...

start ..\..\bin\JediInstaller.exe
if ERRORLEVEL 1 goto FailStart
goto FINI

:FailStart
..\..\bin\JediInstaller.exe
goto FINI

:FINI
cd ..\..
