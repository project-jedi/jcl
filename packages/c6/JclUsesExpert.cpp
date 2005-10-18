//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEFORMNS("..\..\experts\useswizard\JclUsesDialog.pas", Jclusesdialog, FormUsesConfirm);
USEFORMNS("..\..\experts\useswizard\JCLOptionsFrame.pas", Jcloptionsframe, FrameJclOptions); /* TFrame: File Type */
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
