@echo off
echo CommandInterpreter: %COMSPEC%

IF NOT %JCL%!==! SET JCL=

REM check for compiler version 13.0 (Delphi/BCB 5)
REM eventually skip examples which require at least D6/BCB6 
dcc32 | grep -q+ "Version 13"
if not errorlevel 1 goto compiler5

REM pushd vcl\clr
REM make
REM popd

pushd visclx\filesearch
make
popd
pushd visclx\numformat
make
popd

:compiler5

pushd vcl\appinst
make
popd
pushd vcl\asuser
make
popd
pushd vcl\delphitools
make
popd
pushd vcl\debugextension\tools
make
popd
pushd vcl\debug\framestrack
make
popd
pushd vcl\debug\sourceloc
make
popd
pushd vcl\debug\stacktrack
make
popd
pushd vcl\debug\threadexcept
make
popd
pushd vcl\fileversion
make
popd
pushd vcl\graphics
make
popd
pushd vcl\lanman
make
popd
pushd vcl\locales
make
popd
pushd vcl\mapi
make
popd
pushd vcl\multimedia
make
popd
pushd vcl\ntservice
make
popd
pushd vcl\peimage
make
popd
pushd vcl\registry
make
popd
pushd vcl\rtti
make
popd
pushd vcl\sysinfo
make
popd

REM pushd vcl\tasks
REM make
REM popd

pushd vcl\textreader
make
popd

:exit