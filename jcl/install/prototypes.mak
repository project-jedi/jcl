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

VclUnits:       ProductFrames.pas \
                JediInstallerMain.pas

ClxUnits:       QProductFrames.pas \
                QJediInstallerMain.pas

{prototypes}.pas{.}.pas:
	$(jpp) $(VclOptions) $<


QJediInstallerMain.pas: \
        prototypes\JediInstallerMain.pas
        $(jpp) $(VClxOptions) $?

QProductFrames.pas: \
        prototypes\ProductFrames.pas
        $(jpp) $(VClxOptions) $?
