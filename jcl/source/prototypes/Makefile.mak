#
# Generates platform dependent units from common code base
#
# $Id$
#

jpp		= jpp.exe

Options         = -c -dJCL
CommonOptions   = $(Options) -x..\common\\
VclOptions      = $(Options) -dVCL -uVisualCLX -dMSWINDOWS -uUnix -dBitmap32 -x1:..\vcl\Jcl
VClxOptions	= $(Options) -uVCL -dVisualCLX -dHAS_UNIT_TYPES -uBitmap32 -x1:..\visclx\JclQ
WinOptions      = $(Options) -dMSWINDOWS -uUNIX -x..\windows\\
UnixOptions     = $(Options) -uMSWINDOWS -dUNIX -x..\unix\\


release:	Common Windows Unix VCL VisualCLX

VCL:    	..\vcl\JclGraphics.pas \
		..\vcl\JclGraphUtils.pas

VisualCLX:    	..\visclx\JclQGraphics.pas \
		..\visclx\JclQGraphUtils.pas

..\vcl\JclGraphics.pas: \
		_Graphics.pas
	$(jpp) $(VclOptions) $?

..\vcl\JclGraphUtils.pas: \
		_GraphUtils.pas
	$(jpp) $(VclOptions) $?

..\visclx\JclQGraphics.pas: \
		_Graphics.pas
	$(jpp) $(VClxOptions) $?

..\visclx\JclQGraphUtils.pas: \
		_GraphUtils.pas
	$(jpp) $(VClxOptions) $?

Common:         ..\common\JclZLib.pas \
                ..\common\JclDITs.pas

Windows:	..\windows\zlibh.pas

Unix:		..\unix\zlibh.pas

{.}.pas{..\common}.pas:
	$(jpp) $(CommonOptions) $<

{.}.pas{..\windows}.pas:
	$(jpp) $(WinOptions) $<

{.}.pas{..\unix}.pas:
	$(jpp) $(UnixOptions) $<
