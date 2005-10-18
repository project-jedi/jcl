//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEPACKAGE("vcl50.bpi");
USEPACKAGE("dsnide50.bpi");
USEPACKAGE("JclC50.bpi");
USEUNIT("..\..\experts\debug\JclOtaUtils.pas");
USEUNIT("..\..\experts\favfolders\OpenDlgFavAdapter.pas");
USEUNIT("..\..\experts\favfolders\IdeOpenDlgFavoriteUnit.pas");
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
