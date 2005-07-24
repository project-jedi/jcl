//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JclDebugIdeC50.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("JclOtaUtils.pas");
USEFORMNS("JclDebugIdeResult.pas", Jcldebugideresult, JclDebugResultForm);
USEUNIT("JclDebugIdeImpl.pas");
USEPACKAGE("CJcl50.bpi");
USEPACKAGE("dsnide50.bpi");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Source du paquet.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
