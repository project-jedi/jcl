#--------------------------------------------------------------------------------------------------#
#                                                                                                  #
# JCL Stack Track Examples                                                                         #
#                                                                                                  #
#--------------------------------------------------------------------------------------------------#

!ifndef ROOT
ROOT = $(MAKEDIR)\..
!endif

!ifndef JCL
JCL = ..\..\..\..
!endif

#---------------------------------------------------------------------------------------------------
VclExamples = $(JCL)\examples\vcl
INC = $(JCL)\source
SRC = $(JCL)\lib\d7;$(JCL)\lib\d6;$(JCL)\lib\d5;..\bin;vcl\peimage
BIN = $(JCL)\bin
MAP = $(BIN)\$&.map
DRC = $&.drc
#---------------------------------------------------------------------------------------------------
MAKE = $(ROOT)\bin\make.exe -$(MAKEFLAGS)
DCC = $(ROOT)\bin\dcc32.exe -e$(BIN) -i$(INC) -w -u$(SRC)
BRCC = $(ROOT)\bin\brcc32.exe $**
MAKEJCLDBG = $(BIN)\makejcldbg.exe -j
#---------------------------------------------------------------------------------------------------
default: \
  $(BIN)\MakeJclDbg.exe \
  $(BIN)\StackTrackExample.exe \
  $(BIN)\StackTrackDLLsExample.exe \
  $(BIN)\StackTrackDLLsStaticLibrary.dll \
  $(BIN)\StackTrackDLLsDynamicLibrary.dll \
  $(BIN)\StackTrackDLLsComLibrary.dll

#--------------------------------------------------------------------------------------------------#
#                                                                                                  #
# JCL Debug Examples                                                                               #
#                                                                                                  #
#--------------------------------------------------------------------------------------------------#

.path.dpr = $(VclExamples)\debug\stacktrack;$(VclExamples)\debugextension\tools
.path.pas = $(VclExamples)\debug\stacktrack;$(VclExamples)\debugextension\tools

.dpr.exe:
  $(DCC) $<

.dpr.dll:
  $(DCC) -gd -dHOOK_DLL_EXCEPTIONS $&
  $(MAKEJCLDBG) $(MAP)
  del $(MAP)
  del $(DRC)

$(BIN)\StackTrackExample.exe: StackTrackExample.dpr
  $(DCC) -gd $&
  $(MAKEJCLDBG) $(MAP)
  del $(MAP)
  del $(DRC)

$(BIN)\StackTrackDLLsExample.exe: StackTrackDLLsExample.dpr
  $(DCC) -gd -dHOOK_DLL_EXCEPTIONS $&
  $(MAKEJCLDBG) $(MAP)
  del $(MAP)
  del $(DRC)
