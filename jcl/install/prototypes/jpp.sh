#!/bin/sh
#
# shell script to generate installer units from prototypes
#
# Robert Rossmair, 2004-02-16
#

JPP=../../source/prototypes/jpp
OPTIONS="-c -dVisualCLX -dHAS_UNIT_TYPES -uDevelop -uVCL -x../Q"
FILES="JclInstall.pas JediInstallIntf.pas ProductFrames.pas JediInstallerMain.pas JediInstaller.dpr"

$JPP $OPTIONS $FILES
if [ -f ../QJediInstaller.pas ]; then
  mv ../QJediInstaller.pas ../QJediInstaller.dpr
fi
