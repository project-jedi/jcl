//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEPACKAGE("vcl50.bpi");
USEUNIT("..\JclOtaUtils.pas");
USEPACKAGE("CJcl50.bpi");
USEPACKAGE("dsnide50.bpi");
USEFORMNS("JclSIMDViewForm.pas", Jclsimdviewform, JclSIMDViewFrm);
USEFORMNS("JclSIMDModifyForm.pas", Jclsimdmodifyform, JclSIMDModifyFrm);
USEUNIT("JclSIMDUtils.pas");
USEUNIT("JclSIMDView.pas");
USEFORMNS("JclSIMDCpuInfo.pas", Jclsimdcpuinfo, JclFormCpuInfo);
USERES("JclSIMDIcon.dcr");
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
