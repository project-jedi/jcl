@echo off

SETLOCAL
pushd "%~dp0"

if not exist "..\.git" goto StartInstall
::if exist "source\include\jedi\jedi.inc" goto StartInstall

:: Check if git if available
call git --version 2>NUL >NUL
if ERRORLEVEL 1 goto CannotInitializeSubModules
:: Initialize git submodules
echo Initializing/Updating git submodules...
pushd .
cd ..
call git submodule update --init
if ERRORLEVEL 1 goto CannotInitializeSubModules
popd
goto StartInstall

:CannotInitializeSubModules
if exist "source\include\jedi\jedi.inc" goto StartInstall
echo.
echo The jcl\source\include\jedi git submodule can't be initialized. jedi.inc not found.
echo.
goto FailedCompile

:StartInstall
SET DELPHIVERSION=%1

cd install

:: compile installer

build\dcc32ex.exe --runtime-package-rtl --runtime-package-vcl --preserve-config -q -w -$O- -dJCLINSTALL -E..\bin -I..\source\include -U..\source\common;..\source\windows JediInstaller.dpr
if ERRORLEVEL 1 goto FailedCompile

echo.
echo Launching JCL installer...

start ..\bin\JediInstaller.exe %*
if ERRORLEVEL 1 goto FailStart
goto FINI

:FailStart
..\bin\JediInstaller.exe %*
goto FINI

:FailedCompile
echo.
echo.
echo An error occured while compiling the installer. Installation aborted.
echo.
pause

:FINI
cd ..
SET DELPHIVERSION=

popd
ENDLOCAL