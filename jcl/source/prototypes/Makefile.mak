#
# Generates VisualCLX / VCL dependent units from common code base
#
# Robert Rossmair, 2003-10-30
#

jpp		= jpp.exe
VClxOptions	= -c -dVisualCLX -dCOMPILER6_UP -uDevelop -uBitmap32 -uVCL -x1:..\VisCLX\JclQ
VclOptions	= -c -dVCL -dVCL -dBitmap32 -dMSWINDOWS -uDevelop -uVisualCLX -uUnix -x1:..\VCL\Jcl



release:	VCL VisualCLX

VCL:    	_Graphics.pas \
		_GraphUtils.pas
	$(jpp) $(VclOptions) $**

VisualCLX:    	_Graphics.pas \
		_GraphUtils.pas
	$(jpp) $(VClxOptions) $**

