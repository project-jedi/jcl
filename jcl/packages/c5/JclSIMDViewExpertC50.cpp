//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEPACKAGE("vcl50.bpi");
USEUNIT("..\..\experts\debug\JclOtaUtils.pas");
USEPACKAGE("JclC50.bpi");
USEPACKAGE("dsnide50.bpi");
USEFORMNS("..\..\experts\debug\simdview\JclSIMDViewForm.pas", Jclsimdviewform, JclSIMDViewFrm);
USEFORMNS("..\..\experts\debug\simdview\JclSIMDModifyForm.pas", Jclsimdmodifyform, JclSIMDModifyFrm);
USEUNIT("..\..\experts\debug\simdview\JclSIMDUtils.pas");
USEUNIT("..\..\experts\debug\simdview\JclSIMDView.pas");
USEFORMNS("..\..\experts\debug\simdview\JclSIMDCpuInfo.pas", Jclsimdcpuinfo, JclFormCpuInfo);
USERES("..\..\experts\debug\simdview\JclSIMDIcon.dcr");
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
