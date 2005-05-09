//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("JclSIMDViewForm.pas", Jclsimdviewform, JvSIMDViewFrm);
USEFORMNS("JclSIMDModifyForm.pas", Jclsimdmodifyform, JvSIMDModifyFrm);
USEFORMNS("JclSIMDCpuInfo.pas", Jclsimdcpuinfo, FormCpuInfo);
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
