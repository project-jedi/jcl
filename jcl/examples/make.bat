@echo off
echo CommandInterpreter: %COMSPEC%

IF NOT %JCL%!==! SET JCL=

REM check for compiler version 13.0 (Delphi/BCB 5)
REM eventually skip examples which require at least D6/BCB6 
dcc32 | grep -q+ "Version 13"
if not errorlevel 1 goto compiler5

REM cd vcl\clr
REM make
REM cd ..\..

cd visclx\filesearch
make
cd ..\..
cd visclx\numformat
make
cd ..\..

:compiler5

cd vcl\appinst
make
cd ..\..
cd vcl\asuser
make
cd ..\..
cd vcl\delphitools
make
cd ..\..
cd vcl\debugextension\tools
make
cd ..\..\..
cd vcl\debug\framestrack
make
cd ..\..\..
cd vcl\debug\sourceloc
make
cd ..\..\..
cd vcl\debug\stacktrack
make
cd ..\..\..
cd vcl\debug\threadexcept
make
cd ..\..\..
cd vcl\fileversion
make
cd ..\..
cd vcl\graphics
make
cd ..\..
cd vcl\lanman
make
cd ..\..
cd vcl\locales
make
cd ..\..
cd vcl\mapi
make
cd ..\..
cd vcl\multimedia
make
cd ..\..
cd vcl\ntservice
make
cd ..\..
cd vcl\peimage
make
cd ..\..
cd vcl\registry
make
cd ..\..
cd vcl\rtti
make
cd ..\..
cd vcl\sysinfo
make
cd ..\..

REM cd vcl\tasks
REM make
REM cd ..\..

cd vcl\textreader
make
cd ..\..

:exit