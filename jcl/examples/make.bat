REM CommandInterpreter: $(COMSPEC)

echo %JCL%
IF NOT %JCL%!==! SET JCL=
echo %JCL%

pushd vcl\appinst
make
popd
pushd vcl\asuser
make
popd
pushd vcl\clr
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
pushd visclx\filesearch
make
popd
pushd visclx\numformat
make
popd
