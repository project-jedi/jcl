@echo off
echo CommandInterpreter: %COMSPEC%

IF NOT %JCL%!==! SET JCL=

SET MAKE=make
IF NOT %1!==! SET MAKE=%1\Bin\make.exe

REM check for compiler version 13.0 (Delphi/BCB 5)
REM eventually skip examples which require at least D6/BCB6 
dcc32 | grep -q+ "Version 13"
if not errorlevel 1 goto compiler5

REM cd vcl\clr
REM %MAKE%
REM cd ..\..

cd visclx\filesearch
%MAKE%
cd ..\..
cd visclx\numformat
%MAKE%
cd ..\..

:compiler5

cd vcl\appinst
%MAKE%
cd ..\..
cd vcl\asuser
%MAKE%
cd ..\..
cd vcl\delphitools
%MAKE%
cd ..\..
cd vcl\debugextension\tools
%MAKE%
cd ..\..\..
cd vcl\debug\framestrack
%MAKE%
cd ..\..\..
cd vcl\debug\sourceloc
%MAKE%
cd ..\..\..
cd vcl\debug\stacktrack
%MAKE%
cd ..\..\..
cd vcl\debug\threadexcept
%MAKE%
cd ..\..\..
cd vcl\fileversion
%MAKE%
cd ..\..
cd vcl\graphics
%MAKE%
cd ..\..
cd vcl\lanman
%MAKE%
cd ..\..
cd vcl\locales
%MAKE%
cd ..\..
cd vcl\mapi
%MAKE%
cd ..\..
cd vcl\multimedia
%MAKE%
cd ..\..
cd vcl\ntservice
%MAKE%
cd ..\..
cd vcl\peimage
%MAKE%
cd ..\..
cd vcl\registry
%MAKE%
cd ..\..
cd vcl\rtti
%MAKE%
cd ..\..
cd vcl\sysinfo
%MAKE%
cd ..\..

REM cd vcl\tasks
REM %MAKE%
REM cd ..\..

cd vcl\textreader
%MAKE%
cd ..\..

:exit