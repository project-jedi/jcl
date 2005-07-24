//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEPACKAGE("vcl50.bpi");
USEUNIT("..\JclOtaUtils.pas");
USEPACKAGE("CJcl50.bpi");
USEPACKAGE("dsnide50.bpi");
USEUNIT("ThreadExpertUnit.pas");
USEUNIT("ThreadExpertSharedNames.pas");
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
