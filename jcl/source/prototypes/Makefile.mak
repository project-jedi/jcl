#
# Generates platform dependent units from common code base
#
# $Id$
#

jpp		= jpp.exe

Options         = -c -dJCL -dSUPPORTS_DEFAULTPARAMS -dSUPPORTS_INT64
CommonOptions   = $(Options) -x..\common\\
VclOptions      = $(Options) -dVCL -uVisualCLX -dMSWINDOWS -uUnix -dBitmap32 -x1:..\vcl\Jcl
VClxOptions	= $(Options) -uVCL -dVisualCLX -dHAS_UNIT_TYPES -uBitmap32 -x1:..\visclx\JclQ
WinOptions      = $(Options) -dMSWINDOWS -uUNIX -uHAS_UNIT_LIBC -x..\windows\\
UnixOptions     = $(Options) -uMSWINDOWS -dUNIX -x..\unix\\
ZlibOptions	= -uPLATFORM_SPECIFIC_COMMENT -uZLIB_WIN32DLL


release:	VCL VisualCLX Windows Unix

VCL:    	..\vcl\JclGraphics.pas \
		..\vcl\JclGraphUtils.pas

VisualCLX:    	..\visclx\JclQGraphics.pas \
		..\visclx\JclQGraphUtils.pas

Windows:        ..\windows\Hardlinks.pas \
                ..\windows\zlibh.pas

Unix:		..\unix\zlibh.pas

zlib:		..\windows\zlibh.pas ..\unix\zlibh.pas

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

..\unix\zlibh.pas: \
		zlibh.pas
        echo Unix-zlib
	$(jpp) $(UnixOptions) $(ZlibOptions) $?

..\windows\zlibh.pas: \
		zlibh.pas
        echo Win-zlib
	$(jpp) $(WinOptions) $(ZlibOptions) $?

{.}.pas{..\common}.pas:
	$(jpp) $(CommonOptions) $<

{.}.pas{..\windows}.pas:
	$(jpp) $(WinOptions) $<

{.}.pas{..\unix}.pas:
	$(jpp) $(UnixOptions) $<
