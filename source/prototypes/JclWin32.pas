{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ Portions of this code are translated from DelayImp.h.                                            }
{ The Initial Developer of DelayImp.h is Inprise Corporation. Portions created by Inprise          }
{ Corporation are Copyright (C) 1999, 2000 by Inprise Corporation. All Rights Reserved.            }
{                                                                                                  }
{ The Original Code is JclWin32.pas.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel. Portions created by Marcel van  }
{ Brakel are Copyright (C) Marcel van Brakel. All Rights Reserved.                                 }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Marcel van Brakel                                                                              }
{   Peter Friese                                                                                   }
{   Andreas Hausladen (ahuser)                                                                     }
{   Flier Lu (flier)                                                                               }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit defines various Win32 API declarations which are either missing or incorrect in one or }
{ more of the supported Delphi versions. This unit is not intended for regular code, only API      }
{ declarations.                                                                                    }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$

unit JclWin32;

{$I jcl.inc}

{$DEFINE STRICT}
{$DEFINE WINVER_0400_UP}
{$DEFINE WINVER_0500_GREATER}
{$DEFINE WINVER_0400_GREATER}
{$DEFINE WINNT}
{$DEFINE WINNT_0400_UP}
{$DEFINE WINNT_0400_GREATER}
{$DEFINE WINNT_0500_GREATER}
{$DEFINE WINDOWS_0400_GREATER}

{$MINENUMSIZE 4}
{$ALIGN ON}
{$WARNINGS OFF}

interface

uses
  Windows, SysUtils,
  {$IFNDEF FPC}
  {$IFDEF CLR}
  System.Runtime.InteropServices, System.Security,
  {$ELSE}
  AccCtrl,
  {$ENDIF CLR}
  ActiveX,
  {$ENDIF ~FPC}
  JclBase;

{$HPPEMIT ''}
{$IFDEF COMPILER5}
{$HPPEMIT '// To lift ambiguity between LONG64 and System::LONG64'}
{$HPPEMIT '#define LONG64 System::LONG64'}
{$HPPEMIT ''}
{$ENDIF COMPILER5}
{$HPPEMIT '#include "WinDef.h"'}
{$HPPEMIT '#include "WinNT.h"'}
{$HPPEMIT '#include "WinBase.h"'}
{$HPPEMIT '#include "BaseTsd.h"'}
{$HPPEMIT '#include "ImageHlp.h"'}
{$HPPEMIT '#include "lm.h"'}
{$HPPEMIT '#include "Nb30.h"'}
{$HPPEMIT '#include "RasDlg.h"'}
{$IFDEF COMPILER6_UP}
{$HPPEMIT '#include "Reason.h"'}
{$ENDIF COMPILER6_UP}
{$HPPEMIT '#include "ShlWApi.h"'}
{$HPPEMIT '#include "WinError.h"'}
{$HPPEMIT '#include "WinIoCtl.h"'}
{$HPPEMIT '#include "WinUser.h"'}

{$HPPEMIT '#include <delayimp.h>'}
{$HPPEMIT ''}

{$IFDEF CLR}
type
  LPSTR = string;
  LPWSTR = string;
  LPCSTR = string;
  LPCWSTR = string;
  LPCTSTR = string;
  PLongWord = ^LongWord;
  PByte = IntPtr;
{$ENDIF CLR}

{$IFDEF FPC}
// include file for FPC compatibility
{$I win32api\fpc.inc}
{$ENDIF FPC}

{$I win32api\WinDef.int}
{$I win32api\WinNT.int}
{$I win32api\WinBase.int}
{$I win32api\BaseTsd.int}
{$I win32api\AclApi.int}
{$I win32api\ImageHlp.int}
{$I win32api\LmErr.int}
{$I win32api\LmCons.int}
{$I win32api\LmAccess.int}
{$I win32api\LmApiBuf.int}
{$I win32api\Nb30.int}
{$I win32api\RasDlg.int}
{$I win32api\Reason.int}
{$I win32api\ShlObj.int}
{$I win32api\ShlWApi.int}
{$I win32api\WinError.int}
{$I win32api\WinIoctl.int}
{$I win32api\WinNLS.int}
{$I win32api\WinUser.int}

{$I win32api\DelayImp.int}

{$IFDEF MSWINDOWS}

{$IFNDEF CLR}

const
  RtdlSetNamedSecurityInfoW: function(pObjectName: LPWSTR; ObjectType: SE_OBJECT_TYPE;
    SecurityInfo: SECURITY_INFORMATION; psidOwner, psidGroup: PSID;
    pDacl, pSacl: PACL): DWORD stdcall = SetNamedSecurityInfoW;

  RtdlSetWaitableTimer: function(hTimer: THandle; var lpDueTime: TLargeInteger;
    lPeriod: Longint; pfnCompletionRoutine: TFNTimerAPCRoutine;
    lpArgToCompletionRoutine: Pointer; fResume: BOOL): BOOL stdcall = SetWaitableTimer;

  RtdlNetUserAdd: function(servername: LPCWSTR; level: DWORD;
    buf: PByte; parm_err: PDWord): NET_API_STATUS stdcall = NetUserAdd;

  RtdlNetUserDel: function(servername: LPCWSTR;
    username: LPCWSTR): NET_API_STATUS stdcall = NetUserDel;

  RtdlNetGroupAdd: function(servername: LPCWSTR; level: DWORD; buf: PByte;
    parm_err: PDWord): NET_API_STATUS stdcall = NetGroupAdd;

  RtdlNetGroupEnum: function(servername: LPCWSTR; level: DWORD;
    out bufptr: PByte; prefmaxlen: DWORD; out entriesread, totalentries: DWORD;
    resume_handle: PDWORD_PTR): NET_API_STATUS stdcall = NetGroupEnum;

  RtdlNetGroupDel: function(servername: LPCWSTR;
    groupname: LPCWSTR): NET_API_STATUS stdcall = NetGroupDel;

  RtdlNetLocalGroupAdd: function(servername: LPCWSTR; level: DWORD;
    buf: PByte; parm_err: PDWord): NET_API_STATUS stdcall = NetLocalGroupAdd;

  RtdlNetLocalGroupEnum: function(servername: LPCWSTR; level: DWORD;
    out bufptr: PByte; prefmaxlen: DWORD; out entriesread, totalentries: DWORD;
    resumehandle: PDWORD_PTR): NET_API_STATUS stdcall = NetLocalGroupEnum;

  RtdlNetLocalGroupDel: function(servername: LPCWSTR;
    groupname: LPCWSTR): NET_API_STATUS stdcall = NetLocalGroupDel;

  RtdlNetLocalGroupAddMembers: function(servername: LPCWSTR; groupname: LPCWSTR;
    level: DWORD; buf: PByte;
    totalentries: DWORD): NET_API_STATUS stdcall = NetLocalGroupAddMembers;

  RtdlNetApiBufferFree: function(Buffer: Pointer): NET_API_STATUS stdcall = NetApiBufferFree;

  RtdlGetCalendarInfoA: function(Locale: LCID; Calendar: CALID; CalType: CALTYPE;
    lpCalData: PAnsiChar; cchData: Integer;
    lpValue: PDWORD): Integer stdcall = GetCalendarInfoA;

  RtdlGetCalendarInfoW: function(Locale: LCID; Calendar: CALID; CalType: CALTYPE;
    lpCalData: PWideChar; cchData: Integer;
    lpValue: PDWORD): Integer stdcall = GetCalendarInfoW;

  RtdlEnumCalendarInfoExA: function(lpCalInfoEnumProc: TCalInfoEnumProcExA;
    Locale: LCID; Calendar: CALID; CalType: CALTYPE): BOOL stdcall = EnumCalendarInfoExA;

  RtdlGetVolumeNameForVolumeMountPoint: function(lpszVolumeMountPoint: LPCSTR;
    lpszVolumeName: LPSTR; cchBufferLength: DWORD): BOOL stdcall = GetVolumeNameForVolumeMountPoint;

  RtdlSetVolumeMountPoint: function(lpszVolumeMountPoint: LPCSTR;
    lpszVolumeName: LPCSTR): BOOL stdcall = SetVolumeMountPoint;

  RtdlDeleteVolumeMountPoint: function(lpszVolumeMountPoint: LPCSTR): BOOL
    stdcall = DeleteVolumeMountPoint;

  RtdlNetBios: function(P: PNCB): UCHAR stdcall = NetBios;

{$ENDIF ~CLR}
{$ENDIF MSWINDOWS}

implementation

uses
  JclResources;

const
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE ~UNICODE}
  AWSuffix = 'A';
  {$ENDIF ~UNICODE}

{$IFNDEF CLR}
procedure GetProcedureAddress(var P: Pointer; const ModuleName, ProcName: string);
var
  ModuleHandle: HMODULE;
begin
  if not Assigned(P) then
  begin
    ModuleHandle := GetModuleHandle(PChar(ModuleName));
    if ModuleHandle = 0 then
    begin
      ModuleHandle := LoadLibrary(PChar(ModuleName));
      if ModuleHandle = 0 then
        raise EJclError.CreateResFmt(@RsELibraryNotFound, [ModuleName]);
    end;
    P := GetProcAddress(ModuleHandle, PChar(ProcName));
    if not Assigned(P) then
      raise EJclError.CreateResFmt(@RsEFunctionNotFound, [ModuleName, ProcName]);
  end;
end;
{$ENDIF ~CLR}

{$I win32api\AclApi.imp}
{$I win32api\ImageHlp.imp}
{$I win32api\LmAccess.imp}
{$I win32api\LmApiBuf.imp}
{$I win32api\Nb30.imp}
{$I win32api\WinBase.imp}
{$I win32api\WinNLS.imp}
{$I win32api\WinNT.imp}

// History of source\prototypes\JclWin32.pas:

{$IFDEF PROTOTYPE}
// $Log$
// Revision 1.5  2005/04/07 00:41:37  rrossmair
// - changed for FPC 1.9.8
//
{$ENDIF PROTOTYPE}
// Revision 1.4  2005/03/08 08:33:19  marquardt
// overhaul of exceptions and resourcestrings, minor style cleaning
//
// Revision 1.3  2005/03/07 07:49:12  marquardt
// made the generator not remove IFDEF MSWINDOWS and UNIX
//
// Revision 1.2  2004/12/23 04:31:43  rrossmair
// - check-in for JCL 1.94 RC 1
//
// Revision 1.1  2004/12/03 04:05:19  rrossmair
// JclWin32 a unit generated from prototype now
//
// History of source\windows\JclWin32.pas:
//
// Revision 1.32  2004/11/04 12:55:21  obones
// BCB compatibility fix: aclapi.h and shlobj.h must not be included.
//
// Revision 1.31  2004/10/30 08:20:09  rrossmair
// fixed BCB-related bugs
//
// Revision 1.30  2004/10/21 08:40:11  marquardt
// style cleaning
//
// Revision 1.29  2004/10/19 21:28:41  rrossmair
// - rewrite from scratch, cannibalizing MvB's Win32API distribution
//
// Revision 1.28  2004/10/09 13:58:52  marquardt
// style cleaning JclPrint
// remove WinSpool related functions from JclWin32
//
// Revision 1.27  2004/08/02 06:34:59  marquardt
// minor string literal improvements
//
// Revision 1.26  2004/08/01 05:50:00  marquardt
// fix JclFreeLibrary
//
// Revision 1.25  2004/07/31 06:21:03  marquardt
// fixing TStringLists, adding BeginUpdate/EndUpdate, finalization improved
//
// Revision 1.24  2004/07/28 18:00:55  marquardt
// various style cleanings, some minor fixes
//
// Revision 1.23  2004/06/14 13:05:22  marquardt
// style cleaning ENDIF, Tabs
//
// Revision 1.22  2004/06/14 11:05:53  marquardt
// symbols added to all ENDIFs and some other minor style changes like removing IFOPT
//
// Revision 1.21  2004/06/02 03:23:47  rrossmair
// cosmetic changes in several units (code formatting, help TODOs processed etc.)
//
// Revision 1.20  2004/05/28 14:00:46  obones
// BCB5 compatibility
//
// Revision 1.19  2004/05/06 05:09:55  rrossmair
// Changes for FPC v1.9.4 compatibility
//
// Revision 1.18  2004/05/05 05:38:38  rrossmair
// Changes for FPC compatibility; header updated according to new policy: initial developers, contributors listed
//
// Revision 1.17  2004/04/18 00:45:05
// add run-time dynamic linking support for GetOpenGLVersion
//
// Revision 1.16  2004/04/11 22:16:20  mthoma
// Modifications for GetDefaultPrinterName. Added GetDefaultPrinter API function.
//
// Revision 1.15  2004/04/08 19:59:11  ahuser
// BCB compatibility
//
// Revision 1.14  2004/04/08 10:27:15  rrossmair
// GetVersionEx overload added.
//
end.



