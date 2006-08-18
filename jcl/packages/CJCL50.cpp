//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("CJCL50.res");
USERC("..\source\JclUnicode.rc");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcljpg50.bpi");
USEUNIT("..\source\JclStrHashMap.pas");
USEUNIT("..\source\JclStrings.pas");
USEUNIT("..\source\JclSysInfo.pas");
USEUNIT("..\source\JclUnicode.pas");
USEUNIT("..\source\JclAppInst.pas");
USEUNIT("..\source\JclDebug.pas");
USEUNIT("..\source\JclGraphics.pas");
USEUNIT("..\source\JclGraphUtils.pas");
USEUNIT("..\source\JclHookExcept.pas");
USEUNIT("..\source\JclRTTI.pas");
USEUNIT("..\source\JclResources.pas");
USEUNIT("..\source\JclBase.pas");
USEUNIT("..\source\JclLogic.pas");
USEUNIT("..\source\JclWin32.pas");
USEUNIT("..\source\JclShell.pas");
USEUNIT("..\source\JclRegistry.pas");
USEUNIT("..\source\JclFileUtils.pas");
USEUNIT("..\source\JclSynch.pas");
USEUNIT("..\source\JclSysUtils.pas");
USEUNIT("..\source\JclTD32.pas");
USEUNIT("..\source\JclPeImage.pas");
USEUNIT("..\source\JclSecurity.pas");
USEUNIT("..\source\JclDateTime.pas");
USEUNIT("..\source\JclCOM.pas");
USEUNIT("..\source\JclComplex.pas");
USEUNIT("..\source\JclCounter.pas");
USEUNIT("..\source\JclIniFiles.pas");
USEUNIT("..\source\JclLocales.pas");
USEUNIT("..\source\JclMiscel.pas");
USEUNIT("..\source\JclMath.pas");
USEUNIT("..\source\JclMime.pas");
USEUNIT("..\source\JclMultimedia.pas");
USEUNIT("..\source\JclNTFS.pas");
USEUNIT("..\source\JclMapi.pas");
USEUNIT("..\source\JclPrint.pas");
USEUNIT("..\source\JclStatistics.pas");
USEUNIT("..\source\JclExprEval.pas");
USEUNIT("..\source\Jcl8087.pas");
USEUNIT("..\source\JclUnitConv.pas");
USEUNIT("..\source\JclSvcCtrl.pas");
USEUNIT("..\source\Snmp.pas");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
