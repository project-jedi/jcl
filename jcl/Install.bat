SETLOCAL
pushd "%~dp0"

SET DELPHIVERSION=%1

cd install

:: compile installer

build\dcc32ex.exe --runtime-package-rtl --runtime-package-vcl --preserve-config -q -w -dJCLINSTALL -E..\bin -I..\source\include -U..\source\common;..\source\windows JediInstaller.dpr
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