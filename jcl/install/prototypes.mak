#--------------------------------------------------------------------------------------------------#
#                                                                                                  #
# JCL Install Helper                                                                               #
#                                                                                                  #
#--------------------------------------------------------------------------------------------------#

!ifndef ROOT
ROOT = $(MAKEDIR)\..
!endif
#---------------------------------------------------------------------------------------------------
VClxOptions     = -c -dVisualCLX -dHAS_UNIT_TYPES -uVCL -xClxGui\Q
VclOptions      = -c -dVCL -dMSWINDOWS -uVisualCLX -uUnix -uKYLIX -xVclGui\\
#---------------------------------------------------------------------------------------------------
MAKE = $(ROOT)\bin\make.exe -$(MAKEFLAGS) -f$**
jpp = ..\devtools\jpp.exe
#---------------------------------------------------------------------------------------------------
default:	VclGuiUnits ClxGuiUnits
#---------------------------------------------------------------------------------------------------

VclGuiUnits:    VclGui\JediGUIMain.pas \
                VClGui\JediGUIReadme.pas \
                VclGui\JediGUIInstall.pas

ClxGuiUnits:    ClxGui\QJediGUIMain.pas \
                ClxGui\QJediGUIReadme.pas \
                ClxGui\QJediGUIInstall.pas

{prototypes}.pas{VclGui}.pas:
        $(jpp) $(VclOptions) $<

ClxGui\QJediGUIMain.pas: \
        prototypes\JediGUIMain.pas
        $(jpp) $(VClxOptions) $?

ClxGui\QJediGUIReadme.pas: \
        prototypes\JediGUIReadme.pas
        $(jpp) $(VClxOptions) $?

ClxGui\QJediGUIInstall.pas: \
        prototypes\JediGUIInstall.pas
        $(jpp) $(VClxOptions) $?
