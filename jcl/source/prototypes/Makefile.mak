#
# Generates VisualCLX / VCL dependent units from common code base
#
# Robert Rossmair, 2003-10-30
#

jpp		= jpp.exe
VClxOptions	= -c -dVisualCLX -dHAS_UNIT_TYPES -uDevelop -uBitmap32 -uVCL -x1:..\visclx\JclQ
VclOptions	= -c -dVCL -dVCL -dBitmap32 -dMSWINDOWS -uDevelop -uVisualCLX -uUnix -x1:..\vcl\Jcl



release:	VCL VisualCLX

VCL:    	_Graphics.pas \
		_GraphUtils.pas
	$(jpp) $(VclOptions) $**

VisualCLX:    	_Graphics.pas \
		_GraphUtils.pas
	$(jpp) $(VClxOptions) $**

