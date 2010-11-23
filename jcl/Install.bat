SET DELPHIVERSION=%1

cd install

:: compile installer

build\dcc32ex.exe --runtime-package-rtl --runtime-package-vcl --preserve-config -q -w -dJCLINSTALL -E..\bin -I..\source\include -U..\source\common;..\source\windows JediInstaller.dpr
if ERRORLEVEL 1 goto FailedCompile

echo.
echo Launching JCL installer...

start ..\bin\JediInstaller.exe %2 %3 %4 %5 %6 %7 %8 %9
if ERRORLEVEL 1 goto FailStart
goto FINI

:FailStart
..\bin\JediInstaller.exe %2 %3 %4 %5 %6 %7 %8 %9
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
