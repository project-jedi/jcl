#--------------------------------------------------------------------------------------------------#
#                                                                                                  #
# JCL Examples                                                                                     #
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
SRC = $(JCL)\lib\d7;..\bin;vcl\peimage
BIN = $(JCL)\bin
DCU = $(JCL)\bin
MAP = $(BIN)\$&.map
DRC = $&.drc
#---------------------------------------------------------------------------------------------------
MAKE = $(ROOT)\bin\make.exe -$(MAKEFLAGS)
DCC = $(ROOT)\bin\dcc32.exe -e$(BIN) -i$(INC) -w -u$(SRC) $&
DCCU = $(ROOT)\bin\dcc32.exe -e$(BIN) -i$(INC) -n$(DCU) -q -w $**
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

.dpr.dll:
  $(DCC) -gd -dHOOK_DLL_EXCEPTIONS
  $(MAKEJCLDBG) $(MAP)
  del $(MAP)
  del $(DRC)

$(BIN)\MakeJclDbg.exe: $(VCLEXAMP)\debugextension\tools\MakeJclDbg.dpr
  pushd $(VCLEXAMP)\debugextension\tools
  cd $(VCLEXAMP)\debugextension\tools
  dir
  popd

#  $(MAKE) -fmakefile.mak
  # cd $(VCLEXAMP)\debugextension\tools

$(BIN)\StackTrackExample.exe: StackTrackExample.dpr
  $(DCC) -gd
  $(MAKEJCLDBG) $(MAP)
  del $(MAP)
  del $(DRC)

$(BIN)\StackTrackDLLsExample.exe: StackTrackDLLsExample.dpr
  $(DCC) -gd -dHOOK_DLL_EXCEPTIONS
  $(MAKEJCLDBG) $(MAP)
  del $(MAP)
  del $(DRC)

$(BIN)\StackTrackDLLsStaticLibrary.dll: StackTrackDLLsStaticLibrary.dpr

$(BIN)\StackTrackDLLsDynamicLibrary.dll: StackTrackDLLsDynamicLibrary.dpr

$(BIN)\StackTrackDLLsComLibrary.dll: StackTrackDLLsComLibrary.dpr

