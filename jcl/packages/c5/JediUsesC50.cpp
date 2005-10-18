//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEPACKAGE("vcl50.bpi");
USEUNIT("..\debugextension\JclOtaUtils.pas");
USEPACKAGE("CJcl50.bpi");
USEPACKAGE("dsnide50.bpi");
USEUNIT("JCLUsesWizard.pas");
USEUNIT("JclParseUses.pas");
USEFORMNS("JclUsesDialog.pas", Jclusesdialog, FormUsesConfirm);
USEFORMNS("JCLOptionsFrame.pas", Jcloptionsframe, FrameJclOptions); /* TFrame: File Type */
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
