#--------------------------------------------------------------------------------------------------#
#                                                                                                  #
# JCL Examples                                                                                     #
#                                                                                                  #
#--------------------------------------------------------------------------------------------------#

!ifndef ROOT
ROOT = $(MAKEDIR)\..
!endif
#---------------------------------------------------------------------------------------------------
INC = ..\source\common
SRC = ..\lib\d7;.\vcl\peimage
BIN = ..\bin
DCU = ..\bin
MAP = $(BIN)\$&.map
DRC = $&.drc
#---------------------------------------------------------------------------------------------------
MAKE = $(ROOT)\bin\make.exe -$(MAKEFLAGS) -f$**
DCC = $(ROOT)\bin\dcc32.exe -e$(BIN) -i$(INC) -n$(DCU) -w -u$(SRC) $**
DCCU = $(ROOT)\bin\dcc32.exe -e$(BIN) -i$(INC) -n$(DCU) -q -w $**
BRCC = $(ROOT)\bin\brcc32.exe $**
MAKEJCLDBG = $(BIN)\makejcldbg.exe -j
#---------------------------------------------------------------------------------------------------
default: \
  MakeJclDbg.exe \
  ApiHookExample.exe \
  AppInstExample.exe \
  CreateProcAsUserExample.exe \
  FramesTrackExample.exe \
  LanManExample.exe \
  LocalesExample.exe \
  MapiExample.exe \
  MultiMediaExample.exe \
  NtSvcExample.exe \
  PeFuncExample.exe \
  ReadMailExample.exe \
  RegistryExample.exe \
  RTTIExample.exe \
  SingleInstExample.exe \
  SourceLocExample.exe \
  SysInfoExample.exe \
  TextReaderExample.exe \
  UnmangleNameExample.exe \
  VerInfoExample.exe \
  StackTrackExample.exe \
  ThreadExceptExample.exe \
  StackTrackDLLsExample.exe \
  StackTrackDLLsStaticLibrary.dll \
  StackTrackDLLsDynamicLibrary.dll \
  StackTrackDLLsComLibrary.dll
#---------------------------------------------------------------------------------------------------

MakeJclDbg.exe: vcl\debugextension\tools\MakeJclDbg.dpr
  $(DCC)

ApiHookExample.exe: vcl\peimage\ApiHookExample.dpr
  $(DCC)

AppInstExample.exe: vcl\appinst\AppInstExample.dpr
  $(DCC)

CreateProcAsUserExample.exe: vcl\asuser\CreateProcAsUserExample.dpr
  $(DCC)

FramesTrackExample.exe: vcl\debug\framestrack\FramesTrackExample.dpr
  $(DCC) -gd
  $(MAKEJCLDBG) $(MAP)
  del $(MAP)
  del $(DRC)

LanManExample.exe: vcl\lanman\LanManExample.dpr
  $(DCC)

LocalesExample.exe: vcl\locales\LocalesExample.dpr
  $(DCC)

MapiExample.exe: vcl\mapi\MapiExample.dpr
  $(DCC)

MultiMediaExample.exe: vcl\multimedia\MultiMediaExample.dpr
  $(DCC)

NtSvcExample.exe: vcl\ntservice\NtSvcExample.dpr
  $(DCC)

PeFuncExample.exe: vcl\peimage\PeFuncExample.dpr
  $(DCC)

ReadMailExample.exe: vcl\mapi\ReadMailExample.dpr
  $(DCC)

RegistryExample.exe: vcl\registry\RegistryExample.dpr
  $(DCC)

RTTIExample.exe: vcl\rtti\RTTIExample.dpr
  $(DCC)

SingleInstExample.exe: vcl\appinst\SingleInstExample.dpr
  $(DCC)

SourceLocExample.exe: vcl\debug\sourceloc\SourceLocExample.dpr
  $(DCC) -gd
  $(MAKEJCLDBG) $(MAP)
  del $(MAP)
  del $(DRC)

SysInfoExample.exe: vcl\sysinfo\SysInfoExample.dpr
  $(DCC)

TextReaderExample.exe: vcl\textreader\TextReaderExample.dpr
  $(DCC)

UnmangleNameExample.exe: vcl\peimage\UnmangleNameExample.dpr
  $(DCC)

VerInfoExample.exe: vcl\fileversion\VerInfoExample.dpr
  $(DCC)

#--------------------------------------------------------------------------------------------------#
#                                                                                                  #
# JCL Debug Examples                                                                               #
#                                                                                                  #
#--------------------------------------------------------------------------------------------------#

StackTrackExample.exe: vcl\debug\stacktrack\StackTrackExample.dpr
  $(DCC) -gd
  $(MAKEJCLDBG) $(MAP)
  del $(MAP)
  del $(DRC)

ThreadExceptExample.exe: vcl\debug\threadexcept\ThreadExceptExample.dpr
  $(DCCU) -gd -u$(SRC);debugextension\threadnames
  $(MAKEJCLDBG) $(MAP)
  del $(MAP)
  del $(DRC)

StackTrackDLLsExample.exe: vcl\debug\stacktrack\StackTrackDLLsExample.dpr
  $(DCC) -gd -dHOOK_DLL_EXCEPTIONS
  $(MAKEJCLDBG) $(MAP)
  del $(MAP)
  del $(DRC)

StackTrackDLLsStaticLibrary.dll: vcl\debug\stacktrack\StackTrackDLLsStaticLibrary.dpr
  $(DCC) -gd -dHOOK_DLL_EXCEPTIONS
  $(MAKEJCLDBG) $(MAP)
  del $(MAP)
  del $(DRC)

StackTrackDLLsDynamicLibrary.dll: vcl\debug\stacktrack\StackTrackDLLsDynamicLibrary.dpr
  $(DCC) -gd -dHOOK_DLL_EXCEPTIONS
  $(MAKEJCLDBG) $(MAP)
  del $(MAP)
  del $(DRC)

StackTrackDLLsComLibrary.dll: vcl\debug\stacktrack\StackTrackDLLsComLibrary.dpr
  $(DCC) -gd -dHOOK_DLL_EXCEPTIONS
  $(MAKEJCLDBG) $(MAP)
  del $(MAP)
  del $(DRC)

