#
# Generates platform dependent units from common code base
#
# $Id$
#

jpp		= jpp.exe
VClxOptions	= -c -uPrototype -dVisualCLX -dHAS_UNIT_TYPES -uDevelop -uBitmap32 -uVCL -x1:..\visclx\JclQ
VclOptions      = -c -uPrototype -dVCL -dBitmap32 -dMSWINDOWS -uDevelop -uVisualCLX -uUnix -x1:..\vcl\Jcl
WinOptions      = -c -uPrototype -dJCL -dMSWINDOWS -uDevelop -uUnix -x..\windows\\
UnixOptions     = -c -uPrototype -dJCL -dUNIX -uDevelop -uMSWINDOWS -x..\unix\\



release:	VCL VisualCLX

VCL:    	_Graphics.pas \
		_GraphUtils.pas
	$(jpp) $(VclOptions) $**

VisualCLX:    	_Graphics.pas \
		_GraphUtils.pas
	$(jpp) $(VClxOptions) $**

Windows:	..\windows\zlibh.pas

Unix:		..\unix\zlibh.pas

{.}.pas{..\windows}.pas:
	$(jpp) $(WinOptions) $<

{.}.pas{..\unix}.pas:
	$(jpp) $(UnixOptions) $<
