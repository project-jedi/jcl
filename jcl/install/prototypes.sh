#!/bin/sh

#
# shell script to generate installer units from prototypes
#
# Robert Rossmair, 2004-02-16
#
# $Id$

JPP=../devtools/jpp
CLXOPTIONS="-c -dVisualCLX -dHAS_UNIT_TYPES -uDevelop -uVCL -xClxGui/Q"
VCLOPTIONS="-c -dVCL -dMSWINDOWS -uDevelop -uVisualCLX -uHAS_UNIT_LIBC -uUnix -uLinux -uKYLIX -x-xVclGui/"
FILES="prototypes\JediGUIInstall.pas prototypes\JediGUIMain.pas prototypes\JediGUIReadme.pas"

chmod a+x $JPP >/dev/null 2>/dev/null
$JPP $CLXOPTIONS $FILES
$JPP $VCLOPTIONS $FILES
