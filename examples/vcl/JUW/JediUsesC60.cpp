//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("JclUsesDialog.pas", Jclusesdialog, FormUsesConfirm);
USEFORMNS("JCLOptionsFrame.pas", Jcloptionsframe, FrameJclOptions); /* TFrame: File Type */
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//  Source du paquet.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
  return 1;
}
//---------------------------------------------------------------------------
 