#--------------------------------------------------------------------------------------------------#
#                                                                                                  #
# JCL Install Helper                                                                               #
#                                                                                                  #
#--------------------------------------------------------------------------------------------------#

!ifndef ROOT
ROOT = $(MAKEDIR)\..
!endif
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
#---------------------------------------------------------------------------------------------------
default:	clean install
#---------------------------------------------------------------------------------------------------

.dpr.exe:
  $(DCC)
  if exist *.dcu del *.dcu

$(BIN)\JediInstaller.exe: \
                JediInstaller.dpr

$(BIN)\QJediInstaller.exe: \
                QJediInstaller.dpr

install:        $(BIN)\JediInstaller.exe
        cd ..
        bin\JediInstaller.exe
        cd install

qinstall:  $(BIN)\QJediInstaller.exe
        cd ..
        bin\QJediInstaller.exe
        cd install

.PHONY: clean prototypes

clean:
        cd ..
        @echo cleaning up first...
        -@del /q /f /s *.~* bin\*.exe bin\*.dll *.a *.bpi *.dcp *.dcu *.dpu *.hpp *.o *.obj
        cd install

prototypes:
        cd prototypes
        make
        cd ..
