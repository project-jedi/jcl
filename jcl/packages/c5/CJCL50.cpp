//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("CJCL50.res");
USERC("..\..\source\Windows\JclUnicode.rc");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcljpg50.bpi");
USEUNIT("..\..\source\Common\Jcl8087.pas");
USEUNIT("..\..\source\Common\JclBase.pas");
USEUNIT("..\..\source\Common\JclComplex.pas");
USEUNIT("..\..\source\Common\JclCounter.pas");
USEUNIT("..\..\source\Common\JclDateTime.pas");
USEUNIT("..\..\Source\Common\JclEDIXML.pas");
USEUNIT("..\..\Source\Common\JclEDI_ANSIX12.pas");
USEUNIT("..\..\Source\Common\JclEDI_UNEDIFACT.pas");
USEUNIT("..\..\Source\Common\JclEDI.pas");
USEUNIT("..\..\Source\Common\JclEDISEF.pas");
USEUNIT("..\..\Source\Common\JclEDITranslators.pas");
USEUNIT("..\..\source\Common\JclExprEval.pas");
USEUNIT("..\..\source\Common\JclFileUtils.pas");
USEUNIT("..\..\source\Common\JclIniFiles.pas");
USEUNIT("..\..\source\Common\JclLogic.pas");
USEUNIT("..\..\source\Common\JclMath.pas");
USEUNIT("..\..\source\Common\JclMIDI.pas");
USEUNIT("..\..\source\Common\JclMime.pas");
USEUNIT("..\..\source\Common\JclResources.pas");
USEUNIT("..\..\source\Common\JclRTTI.pas");
USEUNIT("..\..\Source\Common\JclSchedule.pas");
USEUNIT("..\..\source\Common\JclStatistics.pas");
USEUNIT("..\..\source\Common\JclStrHashMap.pas");
USEUNIT("..\..\source\Common\JclStrings.pas");
USEUNIT("..\..\source\Common\JclSysInfo.pas");
USEUNIT("..\..\source\Common\JclSysUtils.pas");
USEUNIT("..\..\source\Common\JclUnitConv.pas");
USEUNIT("..\..\source\Windows\JclAppInst.pas");
USEUNIT("..\..\source\Windows\JclCOM.pas");
USEUNIT("..\..\Source\Windows\JclConsole.pas");
USEUNIT("..\..\source\Windows\JclDebug.pas");
USEUNIT("..\..\source\Windows\JclHookExcept.pas");
USEUNIT("..\..\Source\Windows\JclLANMan.pas");
USEUNIT("..\..\source\Windows\JclLocales.pas");
USEUNIT("..\..\source\Windows\JclMapi.pas");
USEUNIT("..\..\source\Windows\JclMiscel.pas");
USEUNIT("..\..\source\Windows\JclMultimedia.pas");
USEUNIT("..\..\source\Windows\JclNTFS.pas");
USEUNIT("..\..\source\Windows\JclPeImage.pas");
USEUNIT("..\..\source\Windows\JclRegistry.pas");
USEUNIT("..\..\source\Windows\JclSecurity.pas");
USEUNIT("..\..\source\Windows\JclShell.pas");
USEUNIT("..\..\source\Windows\JclSvcCtrl.pas");
USEUNIT("..\..\source\Windows\JclSynch.pas");
USEUNIT("..\..\source\Windows\JclTD32.pas");
USEUNIT("..\..\source\Windows\JclUnicode.pas");
USEUNIT("..\..\source\Windows\JclWin32.pas");
USEUNIT("..\..\Source\Windows\JclWinMIDI.pas");
USEUNIT("..\..\Source\Windows\LM.pas");
USEUNIT("..\..\source\Windows\Snmp.pas");
USEUNIT("..\..\source\VCL\JclGraphics.pas");
USEUNIT("..\..\source\VCL\JclGraphUtils.pas");
USEUNIT("..\..\source\VCL\JclPrint.pas");
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
