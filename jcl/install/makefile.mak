#--------------------------------------------------------------------------------------------------#
#                                                                                                  #
# JCL Install Helper                                                                               #
#                                                                                                  #
#--------------------------------------------------------------------------------------------------#

!ifndef ROOT
ROOT = $(MAKEDIR)\..
!endif
#---------------------------------------------------------------------------------------------------
VClxOptions     = -c -dVisualCLX -dHAS_UNIT_TYPES -uDevelop -uVCL -x.\Q
VclOptions      = -c -dVCL -dVCL -dMSWINDOWS -uDevelop -uVisualCLX -uUnix -uKYLIX -x.\\
#---------------------------------------------------------------------------------------------------
SRC = ..\source
UNIT = $(ROOT)\Lib;$(ROOT)\Lib\Obj;$(SRC)\common;$(SRC)\windows
RES =
BIN = ..\bin
MAP = $(BIN)\$&.map
DRC = $&.drc
#---------------------------------------------------------------------------------------------------
MAKE = $(ROOT)\bin\make.exe -$(MAKEFLAGS) -f$**
DCC = $(ROOT)\bin\dcc32.exe -dJCLINSTALL -e$(BIN) -i$(SRC) -q -r$(RES) -u$(UNIT) -w $<
BRCC = $(ROOT)\bin\brcc32.exe $**
jpp = ..\source\prototypes\jpp.exe
#---------------------------------------------------------------------------------------------------
default:	clean prototypes install
#---------------------------------------------------------------------------------------------------

.dpr.exe:
  @if exist "$(ROOT)\Lib\Obj\vcl.dcp" $(DCC) -LUvcl -LUrtl
  @if exist "$(ROOT)\Lib\Obj\vcl50.dcp" $(DCC) -LUvcl50
  @if not exist "$(ROOT)\Lib\Obj" $(DCC)
	@if exist *.dcu del *.dcu

$(BIN)\JediInstaller.exe: \
		VclUnits \
		JediInstaller.dpr

$(BIN)\QJediInstaller.exe: \
		ClxUnits \
		QJediInstaller.dpr

install:        $(BIN)\JediInstaller.exe
	cd ..
	bin\JediInstaller.exe
	cd install

qinstall:       $(BIN)\QJediInstaller.exe
	cd ..
	bin\QJediInstaller.exe
	cd install

.PHONY: clean prototypes

clean:
	cd ..
	@echo cleaning up first...
	-@del /f /s *.~* bin\*.exe bin\*.dll *.a *.bpi *.dcp *.dcu *.dpu *.hpp *.jdbg *.map *.o 
	cd lib
	-@del /f /s *.obj *.res
	cd ..\install

prototypes: VclUnits ClxUnits

VclUnits:
	@if exist prototypes $(MAKEDIR)\make.exe -fprototypes.mak VclUnits

ClxUnits:
	@if exist prototypes $(MAKEDIR)\make.exe -fprototypes.mak ClxUnits
