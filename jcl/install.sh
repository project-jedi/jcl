#!/bin/sh
#
# shell script to build and execute QJediInstaller
#
# Robert Rossmair, 2004-06-11
#

DCC="dcc -I../source -R"$DelphiRoot/lib" -U../source/common"

eval `grep 'DelphiRoot=' ~/.borland/delphi69rc`
source "$DelphiRoot/bin/kylixpath"
cd install
$DCC QJediInstaller.dpr         # build...
../bin/QJediInstaller           # ...and run installer
rm *.dcu                        # clean up source directories
