REM CommandInterpreter: $(COMSPEC)
pushd vcl\delphitools
make
popd
pushd vcl\debugextension\tools
make
popd
pushd vcl\debug\stacktrack
make
popd

