#--------------------------------------------------------------------------------------------------#
#                                                                                                  #
# Delphi Tools 0.5.4                                                                               #
#                                                                                                  #
#--------------------------------------------------------------------------------------------------#

!ifndef ROOT
ROOT = $(MAKEDIR)\..
!endif

!ifndef JCL
JCL = ..\..\..\..
!endif

#---------------------------------------------------------------------------------------------------
VCLEXAMP = $(JCL)\examples\vcl
INC = $(JCL)\source
SRC = $(JCL)\lib\d7;$(JCL)\lib\d6;$(JCL)\lib\d5
BIN = $(JCL)\bin
MAP = $(BIN)\$&.map
DRC = $&.drc
#---------------------------------------------------------------------------------------------------
MAKE = $(ROOT)\bin\make.exe -$(MAKEFLAGS)
DCC = $(ROOT)\bin\dcc32.exe -e$(BIN) -i$(INC) -w -u$(SRC)
BRCC = $(ROOT)\bin\brcc32.exe $**
MAKEJCLDBG = $(BIN)\makejcldbg.exe -e
#---------------------------------------------------------------------------------------------------
default: \
  MakeJclDbg.exe \
  DependView.exe \
  PeViewer.exe \
  ToolHelpViewer.exe \
  ResFix.exe \
  ScreenJPG.exe
#---------------------------------------------------------------------------------------------------

MakeJclDbg.exe: ..\debugextension\tools\MakeJclDbg.dpr
  cd ..\debugextension\tools
  $(DCC) $&.dpr
  cd ..\..\delphitools

DependView.exe: DependencyViewer\DependView.dpr
  cd DependencyViewer
  $(DCC) -gd $&.dpr
  $(MAKEJCLDBG) $(MAP)
  del $(MAP)
  del $(DRC)
  cd ..

PeViewer.exe: PeViewer\PeViewer.dpr
  cd PeViewer
  $(DCC) -gd $&.dpr
  $(MAKEJCLDBG) $(MAP)
  del $(MAP)
  del $(DRC)
  cd ..

ToolHelpViewer.exe: ToolHelpView\ToolHelpViewer.dpr
  cd ToolHelpView
  $(DCC) -gd $&.dpr
  $(MAKEJCLDBG) $(MAP)
  del $(MAP)
  del $(DRC)
  cd ..

ResFix.exe: ResFix\ResFix.dpr
  cd ResFix
  $(DCC) -gd $&.dpr
  $(MAKEJCLDBG) $(MAP)
  del $(MAP)
  del $(DRC)
  cd ..

ScreenJPG.exe: ScreenJPG\ScreenJPG.dpr
  cd ScreenJPG
  $(DCC) -gd $&.dpr
  $(MAKEJCLDBG) $(MAP)
  del $(MAP)
  del $(DRC)
  cd ..
