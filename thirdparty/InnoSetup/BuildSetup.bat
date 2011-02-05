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
cd %JCLROOT%
msbuild make.proj "/p:HppOutDir=%JCLBUILTDIR%\hpp" "/p:DcuOutDir=%JCLBUILTDIR%\lib" "/p:BplOutDir=%JCLBUILTDIR%\bpl"
if ERRORLEVEL 1 goto Failed
cd %SETUPDIR%

:: ==========================================================
:: Compile Setup
:: ==========================================================
:Setup
"%InnoSetupDir%\ISCC.exe" Install.iss /dCmdLineBuild "/dJclRoot=%JCLROOT%" "/dJclLib=%JCLBUILTDIR%\lib" "/dJclBpl=%JCLBUILTDIR%\bpl" "/dJclHpp="%JCLBUILTDIR%\hpp"
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