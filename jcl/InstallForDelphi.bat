@echo off

cd install

::jpp prototypes

..\devtools\jpp.exe -c -dVCL -dMSWINDOWS -uVisualCLX -uUnix -uKYLIX -xVclGui VclGui\JediGUIMain.pas
if ERRORLEVEL 1 goto FailedCompile
..\devtools\jpp.exe -c -dVCL -dMSWINDOWS -uVisualCLX -uUnix -uKYLIX -xVclGui VClGui\JediGUIReadme.pas
if ERRORLEVEL 1 goto FailedCompile
..\devtools\jpp.exe -c -dVCL -dMSWINDOWS -uVisualCLX -uUnix -uKYLIX -xVclGui VclGui\JediGUIInstall.pas
if ERRORLEVEL 1 goto FailedCompile


:: compile installer

build\dcc32ex.exe --use-search-paths -q -w -dJCLINSTALL -E..\bin -I..\source -U..\source\common;..\source\windows JediInstaller.dpr
if ERRORLEVEL 1 goto FailedCompile


echo Launching JCL installer...

start ..\bin\JediInstaller.exe
if ERRORLEVEL 1 goto FailStart
goto FINI

:FailStart
..\bin\JediInstaller.exe
goto FINI

:FailedCompile
echo.
echo.
echo An error occured while compiling the installer. Installation aborted.
echo.

:FINI
cd ..
