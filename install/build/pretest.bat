@echo off

if EXIST build.exe GOTO FOUND

dcc32.exe -$D- -DJCL -Q build.dpr >NUL
if ERRORLEVEL 1 GOTO FAILED

rem ======= COMPILED =======
echo build.exe compiled. Pretest: ok

goto LEAVE

:FAILED
rem ======= FAILED =======
echo.
echo.
echo Delphi Compiler for Win32 (dcc32.exe) was not found. Please add the 
echo Delphi\Bin directory to the PATH environment variable.
echo.
echo You can do this by executing 
echo   'SET PATH=C:\Program Files\Borland\Delphi7\Bin;%%PATH%%'
echo.
echo (Adjust the directories to your installation path)
echo.


goto LEAVE

:FOUND
rem ======= FOUND =======
echo build.exe found. Pretest: ok

:LEAVE
