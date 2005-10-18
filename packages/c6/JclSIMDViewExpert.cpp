//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEFORMNS("..\..\experts\debug\simdview\JclSIMDViewForm.pas", Jclsimdviewform, JclSIMDViewFrm);
USEFORMNS("..\..\experts\debug\simdview\JclSIMDModifyForm.pas", Jclsimdmodifyform, JclSIMDModifyFrm);
USEFORMNS("..\..\experts\debug\simdview\JclSIMDCpuInfo.pas", Jclsimdcpuinfo, JclFormCpuInfo);
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
