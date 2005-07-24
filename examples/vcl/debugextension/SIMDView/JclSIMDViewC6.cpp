//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEFORMNS("JclSIMDViewForm.pas", Jclsimdviewform, JclSIMDViewFrm);
USEFORMNS("JclSIMDModifyForm.pas", Jclsimdmodifyform, JclSIMDModifyFrm);
USEFORMNS("JclSIMDCpuInfo.pas", Jclsimdcpuinfo, JclFormCpuInfo);
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
