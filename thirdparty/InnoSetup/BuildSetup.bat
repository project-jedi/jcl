@echo off
SETLOCAL
SET SETUPDIR=%CD%

:: ==========================================================
:: rsvars.bat check
:: ==========================================================
if not "-%BDS%" == "-" goto RsVarsCalled
call rsvars.bat
if "-%BDS%" == "-" goto Leave

:RsVarsCalled
SET JCLROOT=%SETUPDIR%\..\jcl
SET JCLBUILTDIR=%SETUPDIR%\setupbuild
SET InnoSetupDir=%SETUPDIR%\InnoSetup

:: == Sanity checks ==
if not exist "%JCLROOT%\source\common\JclBase.pas" goto NoRootDirFound
if not exist "%SETUPDIR%\Install.iss" goto NoInstallDir


:: ==========================================================
:: Compile JCL
:: ==========================================================

:: == Create output directories ==
md "%SETUPDIR%\setupbuild" 2>NUL >NUL
md "%JCLBUILTDIR%" 2>NUL >NUL
md "%JCLBUILTDIR%\hpp" 2>NUL >NUL
md "%JCLBUILTDIR%\lib" 2>NUL >NUL
md "%JCLBUILTDIR%\bpl" 2>NUL >NUL

:: == Delete all files in the output directories, we always want to rebuild them ==
del /Q /S "%JCLBUILTDIR%\*.*" 2>NUL >NUL

:: == Compile the files
SET JclLib=%JCLBUILTDIR%\lib\win32

cd %JCLROOT%
msbuild make.proj "/p:Platform=win32" "/p:HppOutDir=%JCLBUILTDIR%\hpp" "/p:DcuOutDir=%JCLBUILTDIR%\lib\win32" "/p:BplOutDir=%JCLBUILTDIR%\bpl"
if ERRORLEVEL 1 goto Failed
if not exist "%BDS%\bin\dcc64.exe" goto NoWin64
msbuild make.proj "/p:Platform=win64" "/p:HppOutDir=%JCLBUILTDIR%\hpp64" "/p:DcuOutDir=%JCLBUILTDIR%\lib\win64" "/p:BplOutDir=%JCLBUILTDIR%\bpl\Win64"
if ERRORLEVEL 1 goto Failed
:: For 64bit we have to install both win32 and lib\win64
SET JclLib=%JCLBUILTDIR%\lib
:NoWin64
cd %SETUPDIR%

:: ==========================================================
:: Compile Setup
:: ==========================================================
:Setup
"%InnoSetupDir%\ISCC.exe" Install.iss /dCmdLineBuild "/dJclRoot=%JCLROOT%" "/dJclLib=%JclLib%" "/dJclBpl=%JCLBUILTDIR%\bpl" "/dJclHpp="%JCLBUILTDIR%\hpp"
if ERRORLEVEL 1 goto Failed


goto Leave

:NoInstalLDirFound
echo You must start BuildSetup.bat from the JclInnoSetup directory.
goto Failed

:NoRootDirFound
echo "%JCLROOT%" is not the JCL root directory.

:Failed
echo.
pause

:Leave
cd %SETUPDIR%
ENDLOCAL