{******************************************************************************}
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JclWin32.pas.                                           }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This unit defines various Win32 API declarations which are either missing or }
{ incorrect in one or more of the supported Delphi versions. This unit is not  }
{ intended for regular code, only API declarations.                            }
{                                                                              }
{ Unit owner: Peter Friese                                                     }
{ Last modified: Februari 17, 2001                                             }
{                                                                              }
{******************************************************************************}

unit JclWin32;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

uses
  Windows, ActiveX, ImageHlp,
  {$IFDEF COMPILER5_UP}
  AccCtrl, AclApi,
  {$ENDIF COMPILER5_UP}
  ShlObj;

//------------------------------------------------------------------------------
// Locales related
//------------------------------------------------------------------------------

function LANGIDFROMLCID(const lcid: LCID): Word;
function MAKELANGID(const usPrimaryLanguage, usSubLanguage: Byte): Word;
function PRIMARYLANGID(const lgid: Word): Word;
function SUBLANGID(const lgid: Word): Word;
function MAKELCID(const wLanguageID, wSortID: Word): LCID;
function SORTIDFROMLCID(const lcid: LCID): Word;

const
  KLF_SETFORPROCESS = $00000100;
  DATE_YEARMONTH = $00000008;

//------------------------------------------------------------------------------
// Various Base Services declarations
//------------------------------------------------------------------------------

function InterlockedExchangePointer(var Target: Pointer; Value: Pointer): Pointer;
  stdcall; external 'kernel32.dll' name 'InterlockedExchangePointer';

function SignalObjectAndWait(hObjectToSignal: THandle; hObjectToWaitOn: THandle;
  dwMilliseconds: DWORD; bAlertable: BOOL): DWORD; stdcall; external kernel32
  name 'SignalObjectAndWait';

const
  VER_NT_WORKSTATION       = $0000001;
  VER_NT_DOMAIN_CONTROLLER = $0000002;
  VER_NT_SERVER            = $0000003;

type
  POSVersionInfoEx = ^TOSVersionInfoEx;
  TOSVersionInfoEx = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array [0..127] of Char;     // Maintenance string for PSS usage
    wServicePackMajor: Word;
    wServicePackMinor: Word;
    wSuiteMask: Word;
    wProductType: Byte;
    wReserved: Byte;
  end;

function GetVersionEx(lpVersionInformation: POSVersionInfoEx): BOOL; stdcall;

function CreateMutex(lpMutexAttributes: PSecurityAttributes; bInitialOwner: DWORD;
  lpName: PChar): THandle; stdcall; external kernel32 name 'CreateMutexA';

//------------------------------------------------------------------------------
// Shell related declarations (missing/incorrect in different delphi versions)
//------------------------------------------------------------------------------

{$IFNDEF COMPILER4_UP}
{$IFDEF SUPPORTS_INTERFACE}

type
  IContextMenu2 = interface (IContextMenu)
    [SID_IContextMenu2]
    function HandleMenuMsg(uMsg: UINT; WParam: WPARAM; LParam: LPARAM): HRESULT; stdcall;
  end;

function SHGetSpecialFolderPath(hwndOwner: HWND; lpszPath: PChar;
  nFolder: Integer; fCreate: BOOL): BOOL; stdcall; external
  'shell32.dll' name 'SHGetSpecialFolderPathA'

const
  SID_IQueryInfo = '{00021500-0000-0000-C000-000000000046}';

type
  IQueryInfo = interface (IUnknown)
    [SID_IQueryInfo]
    function GetInfoTip(dwFlags: DWORD; var ppwszTip: PWideChar): HRESULT; stdcall;
    function GetInfoFlags(out pdwFlags: DWORD): HRESULT; stdcall;
  end;

{$ENDIF SUPPORTS_INTERFACE}
{$ENDIF COMPILER4_UP}

//==============================================================================
// Miscellanuous (missing in delphi 3)
//==============================================================================

{$IFNDEF COMPILER4_UP}

const
  TIME_ZONE_ID_INVALID  = DWORD($FFFFFFFF);
  TIME_ZONE_ID_UNKNOWN  = 0;
  TIME_ZONE_ID_STANDARD = 1;
  TIME_ZONE_ID_DAYLIGHT = 2;

{$ENDIF COMPILER4_UP}

//==============================================================================
// COM related declarations
//==============================================================================

type
  TCoCreateInstanceExProc = function (const clsid: TGUID;
    unkOuter: IUnknown; dwClsCtx: Longint; ServerInfo: Pointer{PCoServerInfo};
    dwCount: Longint; rgmqResults: Pointer{PMultiQIArray}): HResult stdcall;

//==============================================================================
// Security related declarations from winnt.h
//==============================================================================

/////////////////////////////////////////////////////////////////////////////
//                                                                         //
// Universal well-known SIDs                                               //
//                                                                         //
//     Null SID                     S-1-0-0                                //
//     World                        S-1-1-0                                //
//     Local                        S-1-2-0                                //
//     Creator Owner ID             S-1-3-0                                //
//     Creator Group ID             S-1-3-1                                //
//     Creator Owner Server ID      S-1-3-2                                //
//     Creator Group Server ID      S-1-3-3                                //
//                                                                         //
//     (Non-unique IDs)             S-1-4                                  //
//                                                                         //
/////////////////////////////////////////////////////////////////////////////

const
  SECURITY_NULL_SID_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 0));
  SECURITY_WORLD_SID_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 1));
  SECURITY_LOCAL_SID_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 2));
  SECURITY_CREATOR_SID_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 3));
  SECURITY_NON_UNIQUE_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 4));

  SECURITY_NULL_RID                 = ($00000000);
  SECURITY_WORLD_RID                = ($00000000);
  SECURITY_LOCAL_RID                = ($00000000);

  SECURITY_CREATOR_OWNER_RID        = ($00000000);
  SECURITY_CREATOR_GROUP_RID        = ($00000001);

  SECURITY_CREATOR_OWNER_SERVER_RID = ($00000002);
  SECURITY_CREATOR_GROUP_SERVER_RID = ($00000003);

/////////////////////////////////////////////////////////////////////////////
//                                                                         //
// NT well-known SIDs                                                      //
//                                                                         //
//     NT Authority          S-1-5                                         //
//     Dialup                S-1-5-1                                       //
//                                                                         //
//     Network               S-1-5-2                                       //
//     Batch                 S-1-5-3                                       //
//     Interactive           S-1-5-4                                       //
//     Service               S-1-5-6                                       //
//     AnonymousLogon        S-1-5-7       (aka null logon session)        //
//     Proxy                 S-1-5-8                                       //
//     ServerLogon           S-1-5-9       (aka domain controller account) //
//     Self                  S-1-5-10      (self RID)                      //
//     Authenticated User    S-1-5-11      (Authenticated user somewhere)  //
//     Restricted Code       S-1-5-12      (Running restricted code)       //
//     Terminal Server       S-1-5-13      (Running on Terminal Server)    //
//                                                                         //
//     (Logon IDs)           S-1-5-5-X-Y                                   //
//                                                                         //
//     (NT non-unique IDs)   S-1-5-0x15-...                                //
//                                                                         //
//     (Built-in domain)     s-1-5-0x20                                    //
//                                                                         //
/////////////////////////////////////////////////////////////////////////////

const
  SECURITY_NT_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));

  SECURITY_DIALUP_RID                 = ($00000001);
  SECURITY_NETWORK_RID                = ($00000002);
  SECURITY_BATCH_RID                  = ($00000003);
  SECURITY_INTERACTIVE_RID            = ($00000004);
  SECURITY_SERVICE_RID                = ($00000006);
  SECURITY_ANONYMOUS_LOGON_RID        = ($00000007);
  SECURITY_PROXY_RID                  = ($00000008);
  SECURITY_ENTERPRISE_CONTROLLERS_RID = ($00000009);
  SECURITY_SERVER_LOGON_RID           = SECURITY_ENTERPRISE_CONTROLLERS_RID;
  SECURITY_PRINCIPAL_SELF_RID         = ($0000000A);
  SECURITY_AUTHENTICATED_USER_RID     = ($0000000B);
  SECURITY_RESTRICTED_CODE_RID        = ($0000000C);
  SECURITY_TERMINAL_SERVER_RID        = ($0000000D);

  SECURITY_LOGON_IDS_RID       = ($00000005);
  SECURITY_LOGON_IDS_RID_COUNT = (3);
  SECURITY_LOCAL_SYSTEM_RID    = ($00000012);
  SECURITY_NT_NON_UNIQUE       = ($00000015);
  SECURITY_BUILTIN_DOMAIN_RID  = ($00000020);

/////////////////////////////////////////////////////////////////////////////
//                                                                         //
// well-known domain relative sub-authority values (RIDs)...               //
//                                                                         //
/////////////////////////////////////////////////////////////////////////////

// Well-known users ...

  DOMAIN_USER_RID_ADMIN  = ($000001F4);
  DOMAIN_USER_RID_GUEST  = ($000001F5);
  DOMAIN_USER_RID_KRBTGT = ($000001F6);

// well-known groups ...

  DOMAIN_GROUP_RID_ADMINS            = ($00000200);
  DOMAIN_GROUP_RID_USERS             = ($00000201);
  DOMAIN_GROUP_RID_GUESTS            = ($00000202);
  DOMAIN_GROUP_RID_COMPUTERS         = ($00000203);
  DOMAIN_GROUP_RID_CONTROLLERS       = ($00000204);
  DOMAIN_GROUP_RID_CERT_ADMINS       = ($00000205);
  DOMAIN_GROUP_RID_SCHEMA_ADMINS     = ($00000206);
  DOMAIN_GROUP_RID_ENTERPRISE_ADMINS = ($00000207);
  DOMAIN_GROUP_RID_POLICY_ADMINS     = ($00000208);

// well-known aliases ...

  DOMAIN_ALIAS_RID_ADMINS           = ($00000220);
  DOMAIN_ALIAS_RID_USERS            = ($00000221);
  DOMAIN_ALIAS_RID_GUESTS           = ($00000222);
  DOMAIN_ALIAS_RID_POWER_USERS      = ($00000223);

  DOMAIN_ALIAS_RID_ACCOUNT_OPS      = ($00000224);
  DOMAIN_ALIAS_RID_SYSTEM_OPS       = ($00000225);
  DOMAIN_ALIAS_RID_PRINT_OPS        = ($00000226);
  DOMAIN_ALIAS_RID_BACKUP_OPS       = ($00000227);

  DOMAIN_ALIAS_RID_REPLICATOR       = ($00000228);
  DOMAIN_ALIAS_RID_RAS_SERVERS      = ($00000229);
  DOMAIN_ALIAS_RID_PREW2KCOMPACCESS = ($0000022A);


  SE_CREATE_TOKEN_NAME        = 'SeCreateTokenPrivilege';
  SE_ASSIGNPRIMARYTOKEN_NAME  = 'SeAssignPrimaryTokenPrivilege';
  SE_LOCK_MEMORY_NAME         = 'SeLockMemoryPrivilege';
  SE_INCREASE_QUOTA_NAME      = 'SeIncreaseQuotaPrivilege';
  SE_UNSOLICITED_INPUT_NAME   = 'SeUnsolicitedInputPrivilege';
  SE_MACHINE_ACCOUNT_NAME     = 'SeMachineAccountPrivilege';
  SE_TCB_NAME                 = 'SeTcbPrivilege';
  SE_SECURITY_NAME            = 'SeSecurityPrivilege';
  SE_TAKE_OWNERSHIP_NAME      = 'SeTakeOwnershipPrivilege';
  SE_LOAD_DRIVER_NAME         = 'SeLoadDriverPrivilege';
  SE_SYSTEM_PROFILE_NAME      = 'SeSystemProfilePrivilege';
  SE_SYSTEMTIME_NAME          = 'SeSystemtimePrivilege';
  SE_PROF_SINGLE_PROCESS_NAME = 'SeProfileSingleProcessPrivilege';
  SE_INC_BASE_PRIORITY_NAME   = 'SeIncreaseBasePriorityPrivilege';
  SE_CREATE_PAGEFILE_NAME     = 'SeCreatePagefilePrivilege';
  SE_CREATE_PERMANENT_NAME    = 'SeCreatePermanentPrivilege';
  SE_BACKUP_NAME              = 'SeBackupPrivilege';
  SE_RESTORE_NAME             = 'SeRestorePrivilege';
  SE_SHUTDOWN_NAME            = 'SeShutdownPrivilege';
  SE_DEBUG_NAME               = 'SeDebugPrivilege';
  SE_AUDIT_NAME               = 'SeAuditPrivilege';
  SE_SYSTEM_ENVIRONMENT_NAME  = 'SeSystemEnvironmentPrivilege';
  SE_CHANGE_NOTIFY_NAME       = 'SeChangeNotifyPrivilege';
  SE_REMOTE_SHUTDOWN_NAME     = 'SeRemoteShutdownPrivilege';
  SE_UNDOCK_NAME              = 'SeUndockPrivilege';
  SE_SYNC_AGENT_NAME          = 'SeSyncAgentPrivilege';
  SE_ENABLE_DELEGATION_NAME   = 'SeEnableDelegationPrivilege';

{$IFNDEF COMPILER5_UP}

type
  SE_OBJECT_TYPE = (
    SE_UNKNOWN_OBJECT_TYPE,
    SE_FILE_OBJECT,
    SE_SERVICE,
    SE_PRINTER,
    SE_REGISTRY_KEY,
    SE_LMSHARE,
    SE_KERNEL_OBJECT,
    SE_WINDOW_OBJECT,
    SE_DS_OBJECT,
    SE_DS_OBJECT_ALL,
    SE_PROVIDER_DEFINED_OBJECT,
    SE_WMIGUID_OBJECT
  );

{$ENDIF COMPILER5_UP}

{$IFNDEF COMPILER4_UP}
const
  SECURITY_DESCRIPTOR_REVISION = 1;
  SECURITY_DESCRIPTOR_REVISION1 = 1;
  SECURITY_DESCRIPTOR_MIN_LENGTH = 20;
{$ENDIF COMPILER4_UP}

// TODO SetNamedSecurityInfo is incorrectly declared, at least for Windows 2000
// it is. D5 unit tries to import from aclapi.dll but it is located in advapi3.dll
// Have to check whether this is also true for Windows NT 4.

type
  PPSID = ^PSID;

function SetNamedSecurityInfoW(pObjectName: PWideChar; ObjectType: SE_OBJECT_TYPE;
  SecurityInfo: SECURITY_INFORMATION; ppsidOwner, ppsidGroup: PPSID; ppDacl,
  ppSacl: PACL): DWORD; stdcall; external 'advapi32.dll' name 'SetNamedSecurityInfoW';

function AdjustTokenPrivileges(TokenHandle: THandle; DisableAllPrivileges: BOOL;
  const NewState: TTokenPrivileges; BufferLength: DWORD;
  PreviousState: PTokenPrivileges; ReturnLength: PDWORD): BOOL; stdcall;
  external 'advapi32.dll' name 'AdjustTokenPrivileges'

//==============================================================================
// NTFS related I/O control codes, types and constants from winnt.h, winioctl.h
//==============================================================================

type
  PFileAllocatedRangeBuffer = ^TFileAllocatedRangeBuffer;
  _FILE_ALLOCATED_RANGE_BUFFER = record
    FileOffset: TLargeInteger;
    Length: TLargeInteger;
  end;
  TFileAllocatedRangeBuffer = _FILE_ALLOCATED_RANGE_BUFFER;

  PFileZeroDataInformation = ^TFileZeroDataInformation;
  _FILE_ZERO_DATA_INFORMATION = record
    FileOffset: TLargeInteger;
    BeyondFinalZero: TLargeInteger;
  end;
  TFileZeroDataInformation = _FILE_ZERO_DATA_INFORMATION;

const
  COMPRESSION_FORMAT_NONE     = $0000;
  COMPRESSION_FORMAT_DEFAULT  = $0001;
  COMPRESSION_FORMAT_LZNT1    = $0002;

  FILE_SUPPORTS_SPARSE_FILES         = $00000040;
  FILE_SUPPORTS_REPARSE_POINTS       = $00000080;

  IO_REPARSE_TAG_MOUNT_POINT = DWORD($A0000003);
  IO_REPARSE_TAG_HSM         = DWORD($C0000004);
  IO_REPARSE_TAG_SIS         = DWORD($80000007);

  FILE_ATTRIBUTE_DEVICE              = $00000040;
  FILE_ATTRIBUTE_SPARSE_FILE         = $00000200;
  FILE_ATTRIBUTE_REPARSE_POINT       = $00000400;
  FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = $00002000;
  FILE_ATTRIBUTE_ENCRYPTED           = $00004000;

  FILE_DEVICE_FILE_SYSTEM = $00000009;

  METHOD_BUFFERED         = 0;
  METHOD_IN_DIRECT        = 1;
  METHOD_OUT_DIRECT       = 2;
  METHOD_NEITHER          = 3;

  FILE_ANY_ACCESS         = 0;
  FILE_SPECIAL_ACCESS     = FILE_ANY_ACCESS;
  FILE_READ_ACCESS        = $0001;
  FILE_WRITE_ACCESS       = $0002;

  FILE_WRITE_DATA         = $0002;
  FILE_READ_DATA          = $0001;

  FSCTL_GET_COMPRESSION   = (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            (15 shl 2) or METHOD_BUFFERED;

  FSCTL_SET_COMPRESSION   = (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            ((FILE_READ_DATA or FILE_WRITE_DATA) shl 14) or
                            (16 shl 2) or METHOD_BUFFERED;

  FSCTL_LOCK_VOLUME       = (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            (6 shl 2) or METHOD_BUFFERED;

  FSCTL_UNLOCK_VOLUME     = (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            (7 shl 2) or METHOD_BUFFERED;

  FSCTL_SET_SPARSE        = (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_SPECIAL_ACCESS shl 14) or
                            (49 shl 2) or METHOD_BUFFERED;

  FSCTL_SET_ZERO_DATA     = (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_WRITE_DATA shl 14) or
                            (50 shl 2) or METHOD_BUFFERED;

  FSCTL_QUERY_ALLOCATED_RANGES =
                            (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_READ_DATA shl 14) or
                            (51 shl 2) or METHOD_NEITHER;

  FSCTL_SET_REPARSE_POINT = (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_SPECIAL_ACCESS shl 14) or
                            (41 shl 2) or METHOD_BUFFERED;

  FSCTL_GET_REPARSE_POINT = (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            (42 shl 2) or METHOD_BUFFERED;

  FSCTL_DELETE_REPARSE_POINT =
                            (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_SPECIAL_ACCESS shl 14) or
                            (43 shl 2) or METHOD_BUFFERED;

  FSCTL_REQUEST_OPLOCK_LEVEL_1 =
                            (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            (0 shl 2) or METHOD_BUFFERED;

  FSCTL_REQUEST_OPLOCK_LEVEL_2 =
                            (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            (1 shl 2) or METHOD_BUFFERED;

  FSCTL_REQUEST_BATCH_OPLOCK =
                            (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            (2 shl 2) or METHOD_BUFFERED;

  FSCTL_REQUEST_FILTER_OPLOCK =
                            (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            (23 shl 2) or METHOD_BUFFERED;

  FSCTL_OPLOCK_BREAK_ACKNOWLEDGE =
                            (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            (3 shl 2) or METHOD_BUFFERED;

  FSCTL_OPBATCH_ACK_CLOSE_PENDING =
                            (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            (4 shl 2) or METHOD_BUFFERED;

  FSCTL_OPLOCK_BREAK_NOTIFY =
                            (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            (5 shl 2) or METHOD_BUFFERED;

  FSCTL_OPLOCK_BREAK_ACK_NO_2 =
                            (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            (20 shl 2) or METHOD_BUFFERED;

function GetVolumeNameForVolumeMountPoint(lpszVolumeMountPoint: LPCSTR; lpszVolumeName: LPSTR; cchBufferLength: DWORD): BOOL;
function SetVolumeMountPoint(lpszVolumeMountPoint: LPCSTR; lpszVolumeName: LPCSTR): BOOL;
function DeleteVolumeMountPoint(lpszVolumeMountPoint: LPCSTR): BOOL;

//------------------------------------------------------------------------------
// NTFS Reparse Points
//------------------------------------------------------------------------------

//
// The reparse structure is used by layered drivers to store data in a
// reparse point. The constraints on reparse tags are defined below.
// This version of the reparse data buffer is only for Microsoft tags.
//

type
  PReparseDataBuffer = ^TReparseDataBuffer;
  _REPARSE_DATA_BUFFER = record
    ReparseTag: DWORD;
    ReparseDataLength: Word;
    Reserved: Word;
    case Integer of
      0: ( // SymbolicLinkReparseBuffer and MountPointReparseBuffer
        SubstituteNameOffset: Word;
        SubstituteNameLength: Word;
        PrintNameOffset: Word;
        PrintNameLength: Word;
        PathBuffer: array [0..0] of WCHAR);
      1: ( // GenericReparseBuffer
        DataBuffer: array [0..0] of Byte);
  end;
  TReparseDataBuffer = _REPARSE_DATA_BUFFER;

const
  REPARSE_DATA_BUFFER_HEADER_SIZE = 8;
  REPARSE_GUID_DATA_BUFFER_HEADER_SIZE = 24;

type
  PReparsePointInformation = ^TReparsePointInformation;
  _REPARSE_POINT_INFORMATION = record
    ReparseDataLength: Word;
    UnparsedNameLength: Word;
  end;
  TReparsePointInformation = _REPARSE_POINT_INFORMATION;

const
  MAXIMUM_REPARSE_DATA_BUFFER_SIZE = 16 * 1024;

  IO_REPARSE_TAG_RESERVED_ZERO  = (0);
  IO_REPARSE_TAG_RESERVED_ONE   = (1);
  IO_REPARSE_TAG_RESERVED_RANGE = IO_REPARSE_TAG_RESERVED_ONE;
  IO_REPARSE_TAG_VALID_VALUES   = DWORD($E000FFFF);

type
  PReparseGuidDataBuffer = ^TReparseGuidDataBuffer;
  _REPARSE_GUID_DATA_BUFFER = record
    ReparseTag: DWORD;
    ReparseDataLength: Word;
    Reserved: WORD;
    ReparseGuid: TGUID;
    DataBuffer: array [0..0] of Byte;
  end;
  TReparseGuidDataBuffer = _REPARSE_GUID_DATA_BUFFER;

const
  FILE_FLAG_OPEN_REPARSE_POINT = $00200000;

//==============================================================================
// PSAPI (not included in delphi 3)
//==============================================================================

type
  PModuleInfo = ^TModuleInfo;
  _MODULEINFO = packed record
    lpBaseOfDll: Pointer;
    SizeOfImage: DWORD;
    EntryPoint: Pointer;
  end;
  TModuleInfo = _MODULEINFO;

procedure ExitPsapi;
function InitPsapi: Boolean;
function EnumProcesses(lpidProcess: LPDWORD; cb: DWORD; var cbNeeded: DWORD): BOOL;
function EnumProcessModules(hProcess: THandle; lphModule: LPDWORD; cb: DWORD;
  var lpcbNeeded: DWORD): BOOL;
function GetModuleBaseNameA(hProcess: THandle; hModule: HMODULE;
  lpBaseName: PAnsiChar; nSize: DWORD): DWORD;
function GetModuleFileNameEx(hProcess: THandle; hModule: HMODULE; lpFilename: PChar;
  nSize: DWORD): DWORD;
function GetModuleInformation(hProcess: THandle; hModule: HMODULE; lpmodinfo: PModuleInfo;
  cb: DWORD): BOOL;

//==============================================================================
// ToolHelp32 (static linkage in delphi 3)
//==============================================================================

const
  MAX_MODULE_NAME32 = 255;

type
  tagMODULEENTRY32 = record
    dwSize: DWORD;
    th32ModuleID: DWORD;  // This module
    th32ProcessID: DWORD; // owning process
    GlblcntUsage: DWORD;  // Global usage count on the module
    ProccntUsage: DWORD;  // Module usage count in th32ProcessID's context
    modBaseAddr: PByte;   // Base address of module in th32ProcessID's context
    modBaseSize: DWORD;   // Size in bytes of module starting at modBaseAddr
    hModule: HMODULE;     // The hModule of this module in th32ProcessID's context
    szModule: array [0..MAX_MODULE_NAME32] of Char;
    szExePath: array [0..MAX_PATH - 1] of Char;
  end;
  TModuleEntry32 = tagMODULEENTRY32;

const
  TH32CS_SNAPMODULE   = $00000008;

procedure ExitTLHelp;
function InitTLHelp: Boolean;
function CreateToolhelp32Snapshot(dwFlags, th32ProcessID: DWORD): THandle;
function Module32First(hSnapshot: THandle; var lpme: TModuleEntry32): BOOL;
function Module32Next(hSnapshot: THandle; var lpme: TModuleEntry32): BOOL;

//==============================================================================
// Netbios (incorrect/inconvenient declarations in rtl)
//==============================================================================

const
  NCBNAMSZ    = 16;        // absolute length of a net name
  MAX_LANA    = 254;       // lana's in range 0 to MAX_LANA inclusive

  NRC_GOODRET = $00;       // good return
  NCBASTAT    = $33;       // NCB ADAPTER STATUS
  NCBRESET    = $32;       // NCB RESET
  NCBENUM     = $37;       // NCB ENUMERATE LANA NUMBERS

type
  PNCB = ^TNCB;
  TNCBPostProc = procedure (P: PNCB); stdcall;
  TNCB = record
    ncb_command: Byte;
    ncb_retcode: Byte;
    ncb_lsn: Byte;
    ncb_num: Byte;
    ncb_buffer: PChar;
    ncb_length: Word;
    ncb_callname: array [0..NCBNAMSZ - 1] of Char;
    ncb_name: array [0..NCBNAMSZ - 1] of Char;
    ncb_rto: Byte;
    ncb_sto: Byte;
    ncb_post: TNCBPostProc;
    ncb_lana_num: Byte;
    ncb_cmd_cplt: Byte;
    ncb_reserve: array [0..9] of Char;
    ncb_event: THandle;
  end;

  PAdapterStatus = ^TAdapterStatus;
  TAdapterStatus = record
    adapter_address: array [0..5] of Char;
    // Remaining fields are unused so let's not declare them and save space
    filler: array [1..4*SizeOf(Char)+19*SizeOf(Word)+3*SizeOf(DWORD)] of Byte;
  end;

  PNameBuffer = ^TNameBuffer;
  TNameBuffer = record
    name: array [0..NCBNAMSZ - 1] of Char;
    name_num: Byte;
    name_flags: Byte;
  end;

  ASTAT = record
    adapt: TAdapterStatus;
    namebuf: array [0..29] of TNameBuffer;
  end;

  PLanaEnum = ^TLanaEnum;
  TLanaEnum = record
    length: Byte;
    lana: array [0..MAX_LANA] of Byte;
  end;

procedure ExitNetbios;
function InitNetbios: Boolean;
function NetBios(P: PNCB): Byte;

//==============================================================================
// JclPeImage
//==============================================================================

//------------------------------------------------------------------------------
// Missing winnt.h translations
//------------------------------------------------------------------------------

const
  IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT   = 13; // Delay load import descriptors
  IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR = 14; // COM run-time descriptor
  RT_HTML = MakeIntResource(23);

type

{$IFNDEF DELPHI5_UP}
  PImageExportDirectory = ^TImageExportDirectory;
  _IMAGE_EXPORT_DIRECTORY = packed record
    Characteristics: DWORD;
    TimeDateStamp: DWORD;
    MajorVersion: Word;
    MinorVersion: Word;
    Name: DWORD;
    Base: DWORD;
    NumberOfFunctions: DWORD;
    NumberOfNames: DWORD;
    AddressOfFunctions: DWORD;    // RVA from base of image
    AddressOfNames: DWORD;        // RVA from base of image
    AddressOfNameOrdinals: DWORD; // RVA from base of image
  end;
  TImageExportDirectory = _IMAGE_EXPORT_DIRECTORY;
  IMAGE_EXPORT_DIRECTORY = _IMAGE_EXPORT_DIRECTORY;
{$ENDIF DELPHI5_UP}

{ Non-COFF Object file header }

  PANonObjectHeader = ^TANonObjectHeader;
  ANON_OBJECT_HEADER = record
    Sig1: Word;            // Must be IMAGE_FILE_MACHINE_UNKNOWN
    Sig2: Word;            // Must be 0xffff
    Version: Word;         // >= 1 (implies the CLSID field is present)
    Machine: Word;
    TimeDateStamp: DWORD;
    ClassID: TCLSID;       // Used to invoke CoCreateInstance
    SizeOfData: DWORD;     // Size of data that follows the header
  end;
  TANonObjectHeader = ANON_OBJECT_HEADER;

{ Import format }

  PImageImportByName = ^TImageImportByName;
  _IMAGE_IMPORT_BY_NAME = packed record
    Hint: Word;
    Name: array [0..0] of Char;
  end;
  TImageImportByName = _IMAGE_IMPORT_BY_NAME;
  IMAGE_IMPORT_BY_NAME = _IMAGE_IMPORT_BY_NAME;

  PImageThunkData = ^TImageThunkData;
  _IMAGE_THUNK_DATA = packed record
    case Integer of
      0: (ForwarderString: DWORD;);      // PBYTE
      1: (Function_: DWORD;);            // PDWORD
      2: (Ordinal: DWORD;);
      3: (AddressOfData: DWORD;);        // PIMAGE_IMPORT_BY_NAME
  end;
  TImageThunkData = _IMAGE_THUNK_DATA;
  IMAGE_THUNK_DATA = _IMAGE_THUNK_DATA;

const
  IMAGE_ORDINAL_FLAG = $80000000;

function IMAGE_ORDINAL(Ordinal: DWORD): Word;

type
  PImageTlsDirectory = ^TImageTlsDirectory;
  _IMAGE_TLS_DIRECTORY = packed record
    StartAddressOfRawData: DWORD;
    EndAddressOfRawData: DWORD;
    AddressOfIndex: DWORD;                // PDWORD
    AddressOfCallBacks: DWORD;            // PIMAGE_TLS_CALLBACK *
    SizeOfZeroFill: DWORD;
    Characteristics: DWORD;
  end;
  TImageTlsDirectory = _IMAGE_TLS_DIRECTORY;
  IMAGE_TLS_DIRECTORY = _IMAGE_TLS_DIRECTORY;

  PImageImportDescriptor = ^TImageImportDescriptor;
  _IMAGE_IMPORT_DESCRIPTOR = record
    Characteristics: DWORD;  // 0 for terminating null import descriptor
                             // RVA to original unbound IAT (PIMAGE_THUNK_DATA)
    TimeDateStamp: DWORD;    // 0 if not bound,
                             // -1 if bound, and real date\time stamp
                             //     in IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT (new BIND)
                             // O.W. date/time stamp of DLL bound to (Old BIND)
    ForwarderChain: DWORD;   // -1 if no forwarders
    Name: DWORD;
    FirstThunk: DWORD;       // RVA to IAT (if bound this IAT has actual addresses)
  end;
  TImageImportDescriptor = _IMAGE_IMPORT_DESCRIPTOR;
  IMAGE_IMPORT_DESCRIPTOR = _IMAGE_IMPORT_DESCRIPTOR;

{ New format import descriptors pointed to by DataDirectory[ IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT ] }

  PImageBoundImportDescriptor = ^TImageBoundImportDescriptor;
  _IMAGE_BOUND_IMPORT_DESCRIPTOR = record
    TimeDateStamp: DWORD;
    OffsetModuleName: Word;
    NumberOfModuleForwarderRefs: Word;
    // Array of zero or more IMAGE_BOUND_FORWARDER_REF follows
  end;
  TImageBoundImportDescriptor = _IMAGE_BOUND_IMPORT_DESCRIPTOR;
  IMAGE_BOUND_IMPORT_DESCRIPTOR = _IMAGE_BOUND_IMPORT_DESCRIPTOR;

  PImageBoundForwarderRef = ^TImageBoundForwarderRef;
  _IMAGE_BOUND_FORWARDER_REF = record
    TimeDateStamp: DWORD;
    OffsetModuleName: Word;
    Reserved: Word;
  end;
  TImageBoundForwarderRef = _IMAGE_BOUND_FORWARDER_REF;
  IMAGE_BOUND_FORWARDER_REF = _IMAGE_BOUND_FORWARDER_REF;

{ Resource Format }

const
  IMAGE_RESOURCE_NAME_IS_STRING    = $80000000;
  IMAGE_RESOURCE_DATA_IS_DIRECTORY = $80000000;

type
  PImageResourceDirectory = ^TImageResourceDirectory;
  _IMAGE_RESOURCE_DIRECTORY = packed record
    Characteristics: DWORD;
    TimeDateStamp: DWORD;
    MajorVersion: Word;
    MinorVersion: Word;
    NumberOfNamedEntries: Word;
    NumberOfIdEntries: Word;
  end;
  TImageResourceDirectory = _IMAGE_RESOURCE_DIRECTORY;
  IMAGE_RESOURCE_DIRECTORY = _IMAGE_RESOURCE_DIRECTORY;

  PImageResourceDirectoryEntry = ^TImageResourceDirectoryEntry;
  _IMAGE_RESOURCE_DIRECTORY_ENTRY = packed record
    Name: DWORD;        // Or ID: Word (Union)
    OffsetToData: DWORD;
  end;
  TImageResourceDirectoryEntry = _IMAGE_RESOURCE_DIRECTORY_ENTRY;
  IMAGE_RESOURCE_DIRECTORY_ENTRY = _IMAGE_RESOURCE_DIRECTORY_ENTRY;

  PImageResourceDataEntry = ^TImageResourceDataEntry;
  _IMAGE_RESOURCE_DATA_ENTRY = packed record
    OffsetToData: DWORD;
    Size: DWORD;
    CodePage: DWORD;
    Reserved: DWORD;
  end;
  TImageResourceDataEntry = _IMAGE_RESOURCE_DATA_ENTRY;
  IMAGE_RESOURCE_DATA_ENTRY = _IMAGE_RESOURCE_DATA_ENTRY;

  PImageResourceDirStringU = ^TImageResourceDirStringU;
  _IMAGE_RESOURCE_DIR_STRING_U = packed record
    Length: Word;
    NameString: array [0..0] of WCHAR;
  end;
  TImageResourceDirStringU = _IMAGE_RESOURCE_DIR_STRING_U;
  IMAGE_RESOURCE_DIR_STRING_U = _IMAGE_RESOURCE_DIR_STRING_U;

{ Load Configuration Directory Entry }

  PImageLoadConfigDirectory = ^TImageLoadConfigDirectory;
  IMAGE_LOAD_CONFIG_DIRECTORY = packed record
    Characteristics: DWORD;
    TimeDateStamp: DWORD;
    MajorVersion: Word;
    MinorVersion: Word;
    GlobalFlagsClear: DWORD;
    GlobalFlagsSet: DWORD;
    CriticalSectionDefaultTimeout: DWORD;
    DeCommitFreeBlockThreshold: DWORD;
    DeCommitTotalFreeThreshold: DWORD;
    LockPrefixTable: DWORD; // VA
    MaximumAllocationSize: DWORD;
    VirtualMemoryThreshold: DWORD;
    ProcessHeapFlags: DWORD;
    ProcessAffinityMask: DWORD;
    CSDVersion: Word;
    Reserved1: Word;
    EditList: DWORD; // VA
    Reserved: array [0..0] of  DWORD;
  end;
  TImageLoadConfigDirectory = IMAGE_LOAD_CONFIG_DIRECTORY;

  PImgDelayDescr = ^TImgDelayDescr;
  ImgDelayDescr = packed record
    grAttrs: DWORD;                 // attributes
    szName: DWORD;                  // pointer to dll name
    phmod: PDWORD;                  // address of module handle
    pIAT: TImageThunkData;          // address of the IAT
    pINT: TImageThunkData;          // address of the INT
    pBoundIAT: TImageThunkData;     // address of the optional bound IAT
    pUnloadIAT: TImageThunkData;    // address of optional copy of original IAT
    dwTimeStamp: DWORD;             // 0 if not bound,
                                    // O.W. date/time stamp of DLL bound to (Old BIND)
  end;
  TImgDelayDescr = ImgDelayDescr;

{ Relocation }

  PImageBaseRelocation = ^TImageBaseRelocation;
  _IMAGE_BASE_RELOCATION = packed record
    VirtualAddress: DWORD;
    SizeOfBlock: DWORD;
  end;
  TImageBaseRelocation = _IMAGE_BASE_RELOCATION;
  IMAGE_BASE_RELOCATION =_IMAGE_BASE_RELOCATION;

const
  IMAGE_SIZEOF_BASE_RELOCATION   = 8;

  IMAGE_REL_BASED_ABSOLUTE       = 0;
  IMAGE_REL_BASED_HIGH           = 1;
  IMAGE_REL_BASED_LOW            = 2;
  IMAGE_REL_BASED_HIGHLOW        = 3;
  IMAGE_REL_BASED_HIGHADJ        = 4;
  IMAGE_REL_BASED_MIPS_JMPADDR   = 5;
  IMAGE_REL_BASED_SECTION        = 6;
  IMAGE_REL_BASED_REL32          = 7;

  IMAGE_REL_BASED_MIPS_JMPADDR16 = 9;
  IMAGE_REL_BASED_IA64_IMM64     = 9;
  IMAGE_REL_BASED_DIR64          = 10;
  IMAGE_REL_BASED_HIGH3ADJ       = 11;

{ Debug format }

  IMAGE_DEBUG_TYPE_BORLAND = 9;

//------------------------------------------------------------------------------
// Incorrect translations
//------------------------------------------------------------------------------

type
  // possibly Borland's header translation bug
  TLoadedImage = LoadedImage;

  PPImageSectionHeader = ^PImageSectionHeader;

  // wrong translation - LastRvaSection parameter is not var
  function ImageRvaToVa(NtHeaders: PImageNtHeaders; Base: Pointer;
    Rva: ULONG; LastRvaSection: PPImageSectionHeader): Pointer; stdcall;
    external 'imagehlp.dll' name 'ImageRvaToVa';

  // wrong translation - last parameter is incorrect
  function BindImageEx(Flags: DWORD; ImageName, DllPath, SymbolPath: LPSTR;
    StatusRoutine: TImagehlpStatusRoutine): Bool; stdcall;
    external 'imagehlp.dll' name 'BindImageEx';

  // wrong translation - last parameter is incorrect
  function ImageEnumerateCertificates(FileHandle: THandle; TypeFilter: Word;
    CertificateCount, Indices: PDWORD; IndexCount: DWORD): Bool; stdcall;
    external 'imagehlp.dll' name 'ImageEnumerateCertificates';

//------------------------------------------------------------------------------

{$IFDEF SUPPORTS_EXTSYM}

// centralized EXTERNALSYMs to keep this Delphi 3 compatible

{$EXTERNALSYM InterlockedExchangePointer}
{$EXTERNALSYM SignalObjectAndWait}
{$EXTERNALSYM GetVersionEx}
{$EXTERNALSYM CreateMutex}

{$IFNDEF COMPILER4_UP}

  {$EXTERNALSYM IContextMenu2}
  {$EXTERNALSYM SHGetSpecialFolderPath}
  {$EXTERNALSYM SID_IQueryInfo}
  {$EXTERNALSYM IQueryInfo}

{$ENDIF COMPILER4_UP}

{$IFNDEF COMPILER4_UP}

  {$EXTERNALSYM TIME_ZONE_ID_INVALID}
  {$EXTERNALSYM TIME_ZONE_ID_UNKNOWN}
  {$EXTERNALSYM TIME_ZONE_ID_STANDARD}
  {$EXTERNALSYM TIME_ZONE_ID_DAYLIGHT}

{$ENDIF COMPILER4_UP}

  {$EXTERNALSYM SECURITY_NULL_SID_AUTHORITY}
  {$EXTERNALSYM SECURITY_WORLD_SID_AUTHORITY}
  {$EXTERNALSYM SECURITY_LOCAL_SID_AUTHORITY}
  {$EXTERNALSYM SECURITY_CREATOR_SID_AUTHORITY}
  {$EXTERNALSYM SECURITY_NON_UNIQUE_AUTHORITY}

  {$EXTERNALSYM SECURITY_NULL_RID}
  {$EXTERNALSYM SECURITY_WORLD_RID}
  {$EXTERNALSYM SECURITY_LOCAL_RID}

  {$EXTERNALSYM SECURITY_CREATOR_OWNER_RID}
  {$EXTERNALSYM SECURITY_CREATOR_GROUP_RID}

  {$EXTERNALSYM SECURITY_CREATOR_OWNER_SERVER_RID}
  {$EXTERNALSYM SECURITY_CREATOR_GROUP_SERVER_RID}


  {$EXTERNALSYM SECURITY_NT_AUTHORITY}

  {$EXTERNALSYM SECURITY_DIALUP_RID}
  {$EXTERNALSYM SECURITY_NETWORK_RID}
  {$EXTERNALSYM SECURITY_BATCH_RID}
  {$EXTERNALSYM SECURITY_INTERACTIVE_RID}
  {$EXTERNALSYM SECURITY_SERVICE_RID}
  {$EXTERNALSYM SECURITY_ANONYMOUS_LOGON_RID}
  {$EXTERNALSYM SECURITY_PROXY_RID}
  {$EXTERNALSYM SECURITY_ENTERPRISE_CONTROLLERS_RID}
  {$EXTERNALSYM SECURITY_SERVER_LOGON_RID}
  {$EXTERNALSYM SECURITY_PRINCIPAL_SELF_RID}
  {$EXTERNALSYM SECURITY_AUTHENTICATED_USER_RID}
  {$EXTERNALSYM SECURITY_RESTRICTED_CODE_RID}
  {$EXTERNALSYM SECURITY_TERMINAL_SERVER_RID}

  {$EXTERNALSYM SECURITY_LOGON_IDS_RID}
  {$EXTERNALSYM SECURITY_LOGON_IDS_RID_COUNT}
  {$EXTERNALSYM SECURITY_LOCAL_SYSTEM_RID}
  {$EXTERNALSYM SECURITY_NT_NON_UNIQUE}
  {$EXTERNALSYM SECURITY_BUILTIN_DOMAIN_RID}

  {$EXTERNALSYM DOMAIN_USER_RID_ADMIN}
  {$EXTERNALSYM DOMAIN_USER_RID_GUEST}
  {$EXTERNALSYM DOMAIN_USER_RID_KRBTGT}

  {$EXTERNALSYM DOMAIN_GROUP_RID_ADMINS}
  {$EXTERNALSYM DOMAIN_GROUP_RID_USERS}
  {$EXTERNALSYM DOMAIN_GROUP_RID_GUESTS}
  {$EXTERNALSYM DOMAIN_GROUP_RID_COMPUTERS}
  {$EXTERNALSYM DOMAIN_GROUP_RID_CONTROLLERS}
  {$EXTERNALSYM DOMAIN_GROUP_RID_CERT_ADMINS}
  {$EXTERNALSYM DOMAIN_GROUP_RID_SCHEMA_ADMINS}
  {$EXTERNALSYM DOMAIN_GROUP_RID_ENTERPRISE_ADMINS}
  {$EXTERNALSYM DOMAIN_GROUP_RID_POLICY_ADMINS}


  {$EXTERNALSYM DOMAIN_ALIAS_RID_ADMINS}
  {$EXTERNALSYM DOMAIN_ALIAS_RID_USERS}
  {$EXTERNALSYM DOMAIN_ALIAS_RID_GUESTS}
  {$EXTERNALSYM DOMAIN_ALIAS_RID_POWER_USERS}

  {$EXTERNALSYM DOMAIN_ALIAS_RID_ACCOUNT_OPS}
  {$EXTERNALSYM DOMAIN_ALIAS_RID_SYSTEM_OPS}
  {$EXTERNALSYM DOMAIN_ALIAS_RID_PRINT_OPS}
  {$EXTERNALSYM DOMAIN_ALIAS_RID_BACKUP_OPS}

  {$EXTERNALSYM DOMAIN_ALIAS_RID_REPLICATOR}
  {$EXTERNALSYM DOMAIN_ALIAS_RID_RAS_SERVERS}
  {$EXTERNALSYM DOMAIN_ALIAS_RID_PREW2KCOMPACCESS}


  {$EXTERNALSYM SE_CREATE_TOKEN_NAME}
  {$EXTERNALSYM SE_ASSIGNPRIMARYTOKEN_NAME}
  {$EXTERNALSYM SE_LOCK_MEMORY_NAME}
  {$EXTERNALSYM SE_INCREASE_QUOTA_NAME}
  {$EXTERNALSYM SE_UNSOLICITED_INPUT_NAME}
  {$EXTERNALSYM SE_MACHINE_ACCOUNT_NAME}
  {$EXTERNALSYM SE_TCB_NAME}
  {$EXTERNALSYM SE_SECURITY_NAME}
  {$EXTERNALSYM SE_TAKE_OWNERSHIP_NAME}
  {$EXTERNALSYM SE_LOAD_DRIVER_NAME}
  {$EXTERNALSYM SE_SYSTEM_PROFILE_NAME}
  {$EXTERNALSYM SE_SYSTEMTIME_NAME}
  {$EXTERNALSYM SE_PROF_SINGLE_PROCESS_NAME}
  {$EXTERNALSYM SE_INC_BASE_PRIORITY_NAME}
  {$EXTERNALSYM SE_CREATE_PAGEFILE_NAME}
  {$EXTERNALSYM SE_CREATE_PERMANENT_NAME}
  {$EXTERNALSYM SE_BACKUP_NAME}
  {$EXTERNALSYM SE_RESTORE_NAME}
  {$EXTERNALSYM SE_SHUTDOWN_NAME}
  {$EXTERNALSYM SE_DEBUG_NAME}
  {$EXTERNALSYM SE_AUDIT_NAME}
  {$EXTERNALSYM SE_SYSTEM_ENVIRONMENT_NAME}
  {$EXTERNALSYM SE_CHANGE_NOTIFY_NAME}
  {$EXTERNALSYM SE_REMOTE_SHUTDOWN_NAME}
  {$EXTERNALSYM SE_UNDOCK_NAME}
  {$EXTERNALSYM SE_SYNC_AGENT_NAME}
  {$EXTERNALSYM SE_ENABLE_DELEGATION_NAME}

{$IFNDEF COMPILER5_UP}

  {$EXTERNALSYM SE_OBJECT_TYPE}
  {$EXTERNALSYM PPSID}

{$ENDIF COMPILER5_UP}

{$IFNDEF COMPILER4_UP}

  {$EXTERNALSYM SECURITY_DESCRIPTOR_REVISION}
  {$EXTERNALSYM SECURITY_DESCRIPTOR_REVISION1}
  {$EXTERNALSYM SECURITY_DESCRIPTOR_MIN_LENGTH}

{$ENDIF COMPILER4_UP}

{$EXTERNALSYM SetNamedSecurityInfoW}
{$EXTERNALSYM AdjustTokenPrivileges}

  {$EXTERNALSYM _FILE_ALLOCATED_RANGE_BUFFER}
  {$EXTERNALSYM _FILE_ZERO_DATA_INFORMATION}

  {$EXTERNALSYM COMPRESSION_FORMAT_NONE}
  {$EXTERNALSYM COMPRESSION_FORMAT_DEFAULT}
  {$EXTERNALSYM COMPRESSION_FORMAT_LZNT1}

  {$EXTERNALSYM FILE_SUPPORTS_SPARSE_FILES}
  {$EXTERNALSYM FILE_SUPPORTS_REPARSE_POINTS}

  {$EXTERNALSYM REPARSE_GUID_DATA_BUFFER_HEADER_SIZE}

  {$EXTERNALSYM IO_REPARSE_TAG_MOUNT_POINT}
  {$EXTERNALSYM IO_REPARSE_TAG_HSM}
  {$EXTERNALSYM IO_REPARSE_TAG_SIS}

  {$EXTERNALSYM FILE_ATTRIBUTE_DEVICE}
  {$EXTERNALSYM FILE_ATTRIBUTE_SPARSE_FILE}
  {$EXTERNALSYM FILE_ATTRIBUTE_REPARSE_POINT}
  {$EXTERNALSYM FILE_ATTRIBUTE_NOT_CONTENT_INDEXED}
  {$EXTERNALSYM FILE_ATTRIBUTE_ENCRYPTED}

  {$EXTERNALSYM FILE_DEVICE_FILE_SYSTEM}

  {$EXTERNALSYM METHOD_BUFFERED}
  {$EXTERNALSYM METHOD_IN_DIRECT}
  {$EXTERNALSYM METHOD_OUT_DIRECT}
  {$EXTERNALSYM METHOD_NEITHER}

  {$EXTERNALSYM FILE_ANY_ACCESS}
  {$EXTERNALSYM FILE_SPECIAL_ACCESS}
  {$EXTERNALSYM FILE_READ_ACCESS}
  {$EXTERNALSYM FILE_WRITE_ACCESS}

  {$EXTERNALSYM FILE_WRITE_DATA}
  {$EXTERNALSYM FILE_READ_DATA}

  {$EXTERNALSYM FSCTL_GET_COMPRESSION}
  {$EXTERNALSYM FSCTL_SET_COMPRESSION}
  {$EXTERNALSYM FSCTL_LOCK_VOLUME}
  {$EXTERNALSYM FSCTL_UNLOCK_VOLUME}
  {$EXTERNALSYM FSCTL_SET_SPARSE}
  {$EXTERNALSYM FSCTL_SET_ZERO_DATA}
  {$EXTERNALSYM FSCTL_QUERY_ALLOCATED_RANGES}
  {$EXTERNALSYM FSCTL_SET_REPARSE_POINT}
  {$EXTERNALSYM FSCTL_GET_REPARSE_POINT}
  {$EXTERNALSYM FSCTL_DELETE_REPARSE_POINT}
  {$EXTERNALSYM FSCTL_REQUEST_OPLOCK_LEVEL_1}
  {$EXTERNALSYM FSCTL_REQUEST_OPLOCK_LEVEL_2}
  {$EXTERNALSYM FSCTL_REQUEST_BATCH_OPLOCK}
  {$EXTERNALSYM FSCTL_REQUEST_FILTER_OPLOCK}
  {$EXTERNALSYM FSCTL_OPLOCK_BREAK_ACKNOWLEDGE}
  {$EXTERNALSYM FSCTL_OPBATCH_ACK_CLOSE_PENDING}
  {$EXTERNALSYM FSCTL_OPLOCK_BREAK_NOTIFY}
  {$EXTERNALSYM FSCTL_OPLOCK_BREAK_ACK_NO_2}

  {$EXTERNALSYM GetVolumeNameForVolumeMountPoint}
  {$EXTERNALSYM SetVolumeMountPoint}
  {$EXTERNALSYM DeleteVolumeMountPoint}

  {$EXTERNALSYM _MODULEINFO}

  {$EXTERNALSYM MAX_MODULE_NAME32}
  {$EXTERNALSYM tagMODULEENTRY32}
  {$EXTERNALSYM TH32CS_SNAPMODULE}

  {$EXTERNALSYM NCBNAMSZ}
  {$EXTERNALSYM MAX_LANA}

  {$EXTERNALSYM NRC_GOODRET}
  {$EXTERNALSYM NCBASTAT}
  {$EXTERNALSYM NCBRESET}
  {$EXTERNALSYM NCBENUM}

  {$EXTERNALSYM PNCB}
  {$EXTERNALSYM ASTAT}

  {$EXTERNALSYM IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT}
  {$EXTERNALSYM IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR}
  {$EXTERNALSYM RT_HTML}
{$IFNDEF DELPHI5_UP}
  {$EXTERNALSYM _IMAGE_EXPORT_DIRECTORY}
  {$EXTERNALSYM IMAGE_EXPORT_DIRECTORY}
{$ENDIF DELPHI5_UP}
  {$EXTERNALSYM ANON_OBJECT_HEADER}
  {$EXTERNALSYM _IMAGE_IMPORT_BY_NAME}
  {$EXTERNALSYM IMAGE_IMPORT_BY_NAME}
  {$EXTERNALSYM _IMAGE_THUNK_DATA}
  {$EXTERNALSYM IMAGE_THUNK_DATA}
  {$EXTERNALSYM IMAGE_ORDINAL_FLAG}
  {$EXTERNALSYM IMAGE_ORDINAL}
  {$EXTERNALSYM _IMAGE_TLS_DIRECTORY}
  {$EXTERNALSYM IMAGE_TLS_DIRECTORY}
  {$EXTERNALSYM _IMAGE_IMPORT_DESCRIPTOR}
  {$EXTERNALSYM IMAGE_IMPORT_DESCRIPTOR}
  {$EXTERNALSYM _IMAGE_BOUND_IMPORT_DESCRIPTOR}
  {$EXTERNALSYM IMAGE_BOUND_IMPORT_DESCRIPTOR}
  {$EXTERNALSYM _IMAGE_BOUND_FORWARDER_REF}
  {$EXTERNALSYM IMAGE_BOUND_FORWARDER_REF}
  {$EXTERNALSYM IMAGE_RESOURCE_NAME_IS_STRING}
  {$EXTERNALSYM IMAGE_RESOURCE_DATA_IS_DIRECTORY}
  {$EXTERNALSYM _IMAGE_RESOURCE_DIRECTORY}
  {$EXTERNALSYM IMAGE_RESOURCE_DIRECTORY}
  {$EXTERNALSYM _IMAGE_RESOURCE_DIRECTORY_ENTRY}
  {$EXTERNALSYM IMAGE_RESOURCE_DIRECTORY_ENTRY}
  {$EXTERNALSYM _IMAGE_RESOURCE_DATA_ENTRY}
  {$EXTERNALSYM IMAGE_RESOURCE_DATA_ENTRY}
  {$EXTERNALSYM _IMAGE_RESOURCE_DIR_STRING_U}
  {$EXTERNALSYM IMAGE_RESOURCE_DIR_STRING_U}
  {$EXTERNALSYM IMAGE_LOAD_CONFIG_DIRECTORY}
  {$EXTERNALSYM ImgDelayDescr}
  {$EXTERNALSYM _IMAGE_BASE_RELOCATION}
  {$EXTERNALSYM IMAGE_BASE_RELOCATION}
  {$EXTERNALSYM IMAGE_SIZEOF_BASE_RELOCATION}
  {$EXTERNALSYM IMAGE_REL_BASED_ABSOLUTE}
  {$EXTERNALSYM IMAGE_REL_BASED_HIGH}
  {$EXTERNALSYM IMAGE_REL_BASED_LOW}
  {$EXTERNALSYM IMAGE_REL_BASED_HIGHLOW}
  {$EXTERNALSYM IMAGE_REL_BASED_HIGHADJ}
  {$EXTERNALSYM IMAGE_REL_BASED_MIPS_JMPADDR}
  {$EXTERNALSYM IMAGE_REL_BASED_SECTION}
  {$EXTERNALSYM IMAGE_REL_BASED_REL32}
  {$EXTERNALSYM IMAGE_REL_BASED_MIPS_JMPADDR16}
  {$EXTERNALSYM IMAGE_REL_BASED_IA64_IMM64}
  {$EXTERNALSYM IMAGE_REL_BASED_DIR64}
  {$EXTERNALSYM IMAGE_REL_BASED_HIGH3ADJ}
  {$EXTERNALSYM IMAGE_DEBUG_TYPE_BORLAND}
  {$EXTERNALSYM ImageRvaToVa}
  {$EXTERNALSYM BindImageEx}
  {$EXTERNALSYM ImageEnumerateCertificates}

{$ENDIF SUPPORTS_EXTSYM}

implementation

//==============================================================================
// Locales related
//==============================================================================

function LANGIDFROMLCID(const lcid: LCID): Word;
begin
  Result := Word(lcid);
end;

//------------------------------------------------------------------------------

function MAKELANGID(const usPrimaryLanguage, usSubLanguage: Byte): Word;
begin
  Result := usPrimaryLanguage or (usSubLanguage shl 10);
end;

//------------------------------------------------------------------------------

function PRIMARYLANGID(const lgid: Word): Word;
begin
  Result := (lgid and $03FF);
end;

//------------------------------------------------------------------------------

function SUBLANGID(const lgid: Word): Word;
begin
  Result := (lgid shr 10);
end;

//------------------------------------------------------------------------------

function MAKELCID(const wLanguageID, wSortID: Word): LCID;
begin
  Result := wLanguageID or (wSortID shl 16);
end;

//------------------------------------------------------------------------------

function SORTIDFROMLCID(const lcid: LCID): Word;
begin
  Result := (lcid shr 16) and $0F;
end;

//------------------------------------------------------------------------------

var
  _GetVolumeNameForVolumeMountPoint: function (lpszVolumeMountPoint: LPCSTR; lpszVolumeName: LPSTR; cchBufferLength: DWORD): BOOL; stdcall;
  _SetVolumeMountPoint: function (lpszVolumeMountPoint: LPCSTR; lpszVolumeName: LPCSTR): BOOL; stdcall;
  _DeleteVolumeMountPoint: function (lpszVolumeMountPoint: LPCSTR): BOOL; stdcall;

function GetVolumeNameForVolumeMountPoint(lpszVolumeMountPoint: LPCSTR; lpszVolumeName: LPSTR; cchBufferLength: DWORD): BOOL;
var
  Kernel32: THandle;
begin
  if not Assigned(_GetVolumeNameForVolumeMountPoint) then
  begin
    Kernel32 := GetModuleHandle(PChar('kernel32.dll'));
    if Kernel32 <> 0 then @_GetVolumeNameForVolumeMountPoint := GetProcAddress(Kernel32, PChar('GetVolumeNameForVolumeMountPointA'));
  end;
  if Assigned(_GetVolumeNameForVolumeMountPoint) then
    Result := _GetVolumeNameForVolumeMountPoint(lpszVolumeMountPoint, lpszVolumeName, cchBufferLength)
  else
    Result := False;
end;

function SetVolumeMountPoint(lpszVolumeMountPoint: LPCSTR; lpszVolumeName: LPCSTR): BOOL;
var
  Kernel32: THandle;
begin
  if not Assigned(_SetVolumeMountPoint) then
  begin
    Kernel32 := GetModuleHandle(PChar('kernel32.dll'));
    if Kernel32 <> 0 then @_SetVolumeMountPoint := GetProcAddress(Kernel32, PChar('SetVolumeMountPointA'));
  end;
  if Assigned(_SetVolumeMountPoint) then
    Result := _SetVolumeMountPoint(lpszVolumeMountPoint, lpszVolumeName)
  else
    Result := False;
end;

function DeleteVolumeMountPoint(lpszVolumeMountPoint: LPCSTR): BOOL; 
var
  Kernel32: THandle;
begin
  if not Assigned(_DeleteVolumeMountPoint) then
  begin
    Kernel32 := GetModuleHandle(PChar('kernel32.dll'));
    if Kernel32 <> 0 then @_DeleteVolumeMountPoint := GetProcAddress(Kernel32, PChar('DeleteVolumeMountPointA'));
  end;
  if Assigned(_DeleteVolumeMountPoint) then
    Result := _DeleteVolumeMountPoint(lpszVolumeMountPoint)
  else
    Result := False;
end;

function GetVersionEx; external kernel32 name 'GetVersionExA';

//==============================================================================
// Netbios
//==============================================================================

type
  TNetBios = function (P: PNCB): Byte; stdcall;

var
  NetBiosLib: HINST = 0;
  _NetBios: TNetBios;

//------------------------------------------------------------------------------

procedure ExitNetbios;
begin
  if NetBiosLib <> 0 then
  begin
    FreeLibrary(NetBiosLib);
    NetBiosLib := 0;
  end;
end;

//------------------------------------------------------------------------------

function InitNetbios: Boolean;
begin
  Result := True;
  if NetBiosLib = 0 then
  begin
    NetBiosLib := LoadLibrary(PChar('netapi32.dll'));
    Result := NetBiosLib <> 0;
    if Result then
    begin
      @_NetBios := GetProcAddress(NetBiosLib, PChar('Netbios'));
      Result := @_NetBios <> nil;
      if not Result then
        ExitNetbios;
    end;
  end;
end;

//------------------------------------------------------------------------------

function NetBios(P: PNCB): Byte;
begin
  if InitNetbios then
    Result := _NetBios(P)
  else
    Result := 1; // anything other then NRC_GOODRET will do
end;

//==============================================================================
// PSAPI
//==============================================================================

type
  TEnumProcesses = function (lpidProcess: LPDWORD; cb: DWORD;
    var cbNeeded: DWORD): BOOL stdcall;
  TEnumProcessModules = function (hProcess: THandle; lphModule: LPDWORD; cb: DWORD;
    var lpcbNeeded: DWORD): BOOL; stdcall;
  TGetModuleFileNameEx = function (hProcess: THandle; hModule: HMODULE;
    lpFilename: PChar; nSize: DWORD): DWORD; stdcall;
  TGetModuleInformation = function (hProcess: THandle; hModule: HMODULE;
    lpmodinfo: PModuleInfo; cb: DWORD): BOOL; stdcall;
  TGetModuleBaseNameA = function (hProcess: THandle; hModule: HMODULE;
    lpBaseName: PAnsiChar; nSize: DWORD): DWORD; stdcall;

var
  PsapiLib: HINST = 0;
  _EnumProcesses: TEnumProcesses;
  _EnumProcessModules: TEnumProcessModules;
  _GetModuleFileNameEx: TGetModuleFileNameEx;
  _GetModuleInformation: TGetModuleInformation;
  _GetModuleBaseNameA: TGetModuleBaseNameA;

//------------------------------------------------------------------------------

procedure ExitPsapi;
begin
  if PsapiLib <> 0 then
  begin
    FreeLibrary(PsapiLib);
    PsapiLib := 0;
  end;
end;

//------------------------------------------------------------------------------

function InitPsapi: Boolean;
begin
  Result := True;
  if PsapiLib = 0 then
  begin
    PsapiLib := LoadLibrary('PSAPI.dll');
    Result := PsapiLib <> 0;
    if Result then
    begin
      @_EnumProcesses := GetProcAddress(PsapiLib, PChar('EnumProcesses'));
      @_EnumProcessModules := GetProcAddress(PsapiLib, PChar('EnumProcessModules'));
      @_GetModuleFileNameEx := GetProcAddress(PsapiLib, PChar('GetModuleFileNameExA'));
      @_GetModuleInformation := GetProcAddress(PsapiLib, PChar('GetModuleInformation'));
      @_GetModuleBaseNameA := GetProcAddress(PsapiLib, PChar('GetModuleBaseNameA'));
    end;
  end;
end;

//------------------------------------------------------------------------------

function EnumProcesses(lpidProcess: LPDWORD; cb: DWORD; var cbNeeded: DWORD): BOOL;
begin
  Result := InitPsapi;
  if Result then
    Result := _EnumProcesses(lpidProcess, cb, cbNeeded);
end;

//------------------------------------------------------------------------------

function EnumProcessModules(hProcess: THandle; lphModule: LPDWORD; cb: DWORD;
  var lpcbNeeded: DWORD): BOOL;
begin
  Result := InitPsapi;
  if Result then
    Result := _EnumProcessModules(hProcess, lphModule, cb, lpcbNeeded);
end;

//------------------------------------------------------------------------------

function GetModuleBaseNameA(hProcess: THandle; hModule: HMODULE;
  lpBaseName: PAnsiChar; nSize: DWORD): DWORD;
begin
  Result := 0;
  if InitPsapi then
    Result := _GetModuleBaseNameA(hProcess, hModule, lpBaseName, nSize);
end;

//------------------------------------------------------------------------------

function GetModuleFileNameEx(hProcess: THandle; hModule: HMODULE; lpFilename: PChar;
  nSize: DWORD): DWORD;
begin
  if InitPsapi then
    Result := _GetModuleFileNameEx(hProcess, hModule, lpFileName, nSize)
  else
    Result := 0;
end;

//------------------------------------------------------------------------------

function GetModuleInformation(hProcess: THandle; hModule: HMODULE; lpModInfo: PModuleInfo;
  cb: DWORD): BOOL;
begin
  Result := InitPsapi;
  if Result then
    Result := _GetModuleInformation(hProcess, hModule, lpModInfo, cb);
end;

//==============================================================================
// ToolHelp32
//==============================================================================

type
  TCreateToolhelp32Snapshot = function (dwFlags, th32ProcessID: DWORD): THandle; stdcall;
  TModule32First = function (hSnapshot: THandle; var lpme: TModuleEntry32): BOOL; stdcall;
  TModule32Next = function (hSnapshot: THandle; var lpme: TModuleEntry32): BOOL; stdcall;

var
  TLHelpLib: HINST = 0;
  _CreateToolhelp32Snapshot: TCreateToolhelp32Snapshot;
  _Module32First: TModule32First;
  _Module32Next: TModule32Next;

//------------------------------------------------------------------------------

procedure ExitTLHelp;
begin
  if TLHelpLib <> 0 then
  begin
    FreeLibrary(TLHelpLib);
    TLHelpLib := 0;
  end;
end;

//------------------------------------------------------------------------------

function InitTLHelp: Boolean;
begin
  Result := True;
  if TLHelpLib = 0 then
  begin
    TLHelpLib := GetModuleHandle('kernel32.dll');
    Result := TLHelpLib <> 0;
    if Result then
    begin
      @_CreateToolhelp32Snapshot := GetProcAddress(TLHelpLib, PChar('CreateToolhelp32Snapshot'));
      @_Module32First := GetProcAddress(TLHelpLib, PChar('Module32First'));
      @_Module32Next := GetProcAddress(TLHelpLib, PChar('Module32Next'));
    end;
  end;
end;

//------------------------------------------------------------------------------

function CreateToolhelp32Snapshot(dwFlags, th32ProcessID: DWORD): THandle;
begin
  if InitTLHelp then
    Result := _CreateToolhelp32Snapshot(dwFlags, th32ProcessID)
  else
    Result := 0;
end;

//------------------------------------------------------------------------------

function Module32First(hSnapshot: THandle; var lpme: TModuleEntry32): BOOL;
begin
  if InitTLHelp then
    Result := _Module32First(hSnapshot, lpme)
  else
    Result := False;
end;

//------------------------------------------------------------------------------

function Module32Next(hSnapshot: THandle; var lpme: TModuleEntry32): BOOL;
begin
  if InitTLHelp then
    Result := _Module32Next(hSnapshot, lpme)
  else
    Result := False;
end;

//==============================================================================
// JclPeImage
//==============================================================================

function IMAGE_ORDINAL(Ordinal: DWORD): Word;
begin
  Result := Ordinal and $FFFF;
end;


end.

