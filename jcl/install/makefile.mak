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
UNIT = $(ROOT)\Lib;$(SRC)\common;$(SRC)\windows
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
default:	clean install
#---------------------------------------------------------------------------------------------------

.dpr.exe:
  $(DCC)
  if exist *.dcu del *.dcu

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
	-@del /q /f /s *.~* bin\*.exe bin\*.dll *.a *.bpi *.dcp *.dcu *.dpu *.hpp *.jdbg *.map *.o *.obj
        cd install

prototypes: VclUnits ClxUnits

VclUnits:       JclInstall.pas \
                JediInstallIntf.pas \
                ProductFrames.pas \
                JediInstallerMain.pas

ClxUnits:       QJclInstall.pas \
                QJediInstallIntf.pas \
                QProductFrames.pas \
                QJediInstallerMain.pas
	if exist prototypes $(MAKEDIR)\make.exe -fprototypes.mak

{prototypes}.pas{.}.pas:
	$(jpp) $(VclOptions) $<

