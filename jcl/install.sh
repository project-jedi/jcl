#!/bin/sh
#
# shell script to build and execute QJediInstaller
#
# Robert Rossmair, 2004-06-12
#

eval `grep 'DelphiRoot=' ~/.borland/delphi69rc`
DCC=$DelphiRoot/bin/dcc\ -E../bin\ -I../source\ -R$DelphiRoot/lib\ -U../source/common
source "$DelphiRoot/bin/kylixpath"
cd install
if [ -f ../devtools/jpp ]; then
  cd prototypes
  ./jpp.sh
  cd ..
fi
$DCC QJediInstaller.dpr         # build...
../bin/QJediInstaller           # ...and run installer
rm *.dcu                        # clean up source directories
