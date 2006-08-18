#--------------------------------------------------------------------------------------------------#
#                                                                                                  #
# JCL Examples and Tools                                                                           #
#                                                                                                  #
#--------------------------------------------------------------------------------------------------#

!ifndef ROOT
ROOT = $(MAKEDIR)\..
!endif
#---------------------------------------------------------------------------------------------------
SRC = ..\Source
DCU = ..\Dcu
BIN = ..\Bin
MAP = $(BIN)\$&.map
DRC = $&.drc
#---------------------------------------------------------------------------------------------------
MAKE = $(ROOT)\bin\make.exe -$(MAKEFLAGS)
DCC = $(ROOT)\bin\dcc32.exe -e$(BIN) -i$(SRC) -n$(DCU) -q -w -u$(SRC) $**
DCCU = $(ROOT)\bin\dcc32.exe -e$(BIN) -i$(SRC) -n$(DCU) -q -w $**
BRCC = $(ROOT)\bin\brcc32.exe $**
MAKEJCLDBG = $(BIN)\makejcldbg.exe -j
#---------------------------------------------------------------------------------------------------
default: \
  Examples \
  DebugExtensionTools \
  DelphiTools
#---------------------------------------------------------------------------------------------------

Examples: examples\makefile.mak
  cd examples
  $(MAKE) -f$(**F)
  cd ..

DebugExtensionTools: examples\debugextension\tools\makefile.mak
  cd examples\debugextension\tools
  $(MAKE) -f$(**F)
  cd ..\..\..

DelphiTools: examples\DelphiTools\makefile.mak
  cd examples\DelphiTools
  $(MAKE) -f$(**F)
  cd ..\..

