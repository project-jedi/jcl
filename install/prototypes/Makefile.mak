#
# Generates VisualCLX / VCL dependent units from common code base
#
# Robert Rossmair, 2004-02-13
#

jpp		= ..\..\source\prototypes\jpp.exe
VClxOptions	= -c -dVisualCLX -dHAS_UNIT_TYPES -uDevelop -uBitmap32 -uVCL -x..\Q
VclOptions	= -c -dVCL -dVCL -dBitmap32 -dMSWINDOWS -uDevelop -uVisualCLX -uUnix -x..\\

release:	VCL VisualCLX

VCL:            JclInstall.pas \
                JediInstallIntf.pas \
		ProductFrames.pas \
		JediInstallerMain.pas \
		JediInstaller.dpr
	$(jpp) $(VclOptions) $**
	move ..\JediInstaller.pas ..\JediInstaller.dpr

VisualCLX:    	JclInstall.pas \
                JediInstallIntf.pas \
		ProductFrames.pas \
		JediInstallerMain.pas \
		JediInstaller.dpr
	$(jpp) $(VClxOptions) $**
	move ..\QJediInstaller.pas ..\QJediInstaller.dpr

