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
VclOptions      = -c -dVCL -dMSWINDOWS -uDevelop -uVisualCLX -uUnix -uKYLIX -x.\\
#---------------------------------------------------------------------------------------------------
MAKE = $(ROOT)\bin\make.exe -$(MAKEFLAGS) -f$**
jpp = ..\source\prototypes\jpp.exe
#---------------------------------------------------------------------------------------------------
default:	VclUnits ClxUnits
#---------------------------------------------------------------------------------------------------

VclUnits:       .\JclInstall.pas \
                JediInstallIntf.pas \
                ProductFrames.pas \
                JediInstallerMain.pas

ClxUnits:       QJclInstall.pas \
                QJediInstallIntf.pas \
                QProductFrames.pas \
                QJediInstallerMain.pas

{prototypes}.pas{.}.pas:
	$(jpp) $(VclOptions) $<


QJclInstall.pas: \
        prototypes\JclInstall.pas
        $(jpp) $(VClxOptions) $?

QJediInstallerMain.pas: \
        prototypes\JediInstallerMain.pas
        $(jpp) $(VClxOptions) $?

QJediInstallIntf.pas: \
        prototypes\JediInstallIntf.pas
        $(jpp) $(VClxOptions) $?

QProductFrames.pas: \
        prototypes\ProductFrames.pas
        $(jpp) $(VClxOptions) $?
