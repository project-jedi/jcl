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
{ The Original Code is JclSysInfo.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright (C) Marcel van Brakel. All rights reserved.  }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Alexander Radchenko                                                                            }
{   André Snepvangers (asnepvangers)                                                               }
{   Azret Botash                                                                                   }
{   Bryan Coutch                                                                                   }
{   Carl Clark                                                                                     }
{   Eric S. Fisher                                                                                 }
{   Florent Ouchet (outchy)                                                                        }
{   James Azarja                                                                                   }
{   Jean-Fabien Connault                                                                           }
{   John C Molyneux                                                                                }
{   Marcel van Brakel                                                                              }
{   Matthias Thoma (mthoma)                                                                        }
{   Mike Lischke                                                                                   }
{   Nick Hodges                                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Peter Friese                                                                                   }
{   Peter Thörnquist (peter3)                                                                      }
{   Petr Vones (pvones)                                                                            }
{   Rik Barker                                                                                     }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Scott Price                                                                                    }
{   Tom Hahn (tomhahn)                                                                             }
{   Wim de Cleen                                                                                   }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains routines and classes to retrieve various pieces of system information.        }
{ Examples are the location of standard folders, settings of environment variables, processor      }
{ details and the Windows version.                                                                 }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

// Windows NT 4 and earlier do not support GetSystemPowerStatus (while introduced
// in NT4 - it is a stub there - implemented in Windows 2000 and later.


unit JclSysInfo;

{$I jcl.inc}

interface

uses
  {$IFDEF HAS_UNIT_TYPES}
  Types,
  {$ENDIF HAS_UNIT_TYPES}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  {$IFDEF CLR}
  System.IO, System.Configuration, System.Diagnostics, System.Collections,
  System.Net,
  {$ELSE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$IFNDEF FPC}
  ShlObj,
  {$ENDIF ~FPC}
  {$ENDIF MSWINDOWS}
  {$ENDIF CLR}
  Classes,
  JclResources;

// Environment Variables
{$IFDEF MSWINDOWS}
type
  TEnvironmentOption = (eoLocalMachine, eoCurrentUser, eoAdditional);
  TEnvironmentOptions = set of TEnvironmentOption;
{$ENDIF MSWINDOWS}
{$IFDEF CLR}
type
  DWORD = LongWord;
{$ENDIF CLR}

function DelEnvironmentVar(const Name: string): Boolean;
function ExpandEnvironmentVar(var Value: string): Boolean;
function GetEnvironmentVar(const Name: string; var Value: string): Boolean; overload;
function GetEnvironmentVar(const Name: string; var Value: string; Expand: Boolean): Boolean; overload;
function GetEnvironmentVars(const Vars: TStrings): Boolean; overload;
function GetEnvironmentVars(const Vars: TStrings; Expand: Boolean): Boolean; overload;
function SetEnvironmentVar(const Name, Value: string): Boolean;
{$IFNDEF CLR}
{$IFDEF MSWINDOWS}
function CreateEnvironmentBlock(const Options: TEnvironmentOptions; const AdditionalVars: TStrings): PChar;
procedure DestroyEnvironmentBlock(var Env: PChar);
procedure SetGlobalEnvironmentVariable(VariableName, VariableContent: string);
{$ENDIF MSWINDOWS}
{$ENDIF ~CLR}

// Common Folder Locations
{$IFNDEF CLR}
{$IFDEF MSWINDOWS}
function GetCommonFilesFolder: string;
{$ENDIF MSWINDOWS}
{$ENDIF ~CLR}
function GetCurrentFolder: string;
{$IFDEF MSWINDOWS}
function GetProgramFilesFolder: string;
{$IFNDEF CLR}
function GetWindowsFolder: string;
{$ENDIF ~CLR}
function GetWindowsSystemFolder: string;
function GetWindowsTempFolder: string;

function GetDesktopFolder: string;
function GetProgramsFolder: string;
{$ENDIF MSWINDOWS}
function GetPersonalFolder: string;
{$IFDEF MSWINDOWS}
function GetFavoritesFolder: string;
function GetStartupFolder: string;
function GetRecentFolder: string;
function GetSendToFolder: string;
function GetStartmenuFolder: string;
function GetDesktopDirectoryFolder: string;
{$IFNDEF CLR}
function GetNethoodFolder: string;
function GetFontsFolder: string;
function GetCommonStartmenuFolder: string;
function GetCommonStartupFolder: string;
function GetPrinthoodFolder: string;
function GetProfileFolder: string;
{$ENDIF ~CLR}
function GetCommonProgramsFolder: string;
function GetCommonDesktopdirectoryFolder: string;
function GetCommonAppdataFolder: string;
function GetAppdataFolder: string;
function GetCommonFavoritesFolder: string;
function GetTemplatesFolder: string;
function GetInternetCacheFolder: string;
function GetCookiesFolder: string;
function GetHistoryFolder: string;

{$IFNDEF CLR}
// Advanced Power Management (APM)
type
  TAPMLineStatus = (alsOffline, alsOnline, alsUnknown);
  TAPMBatteryFlag = (abfHigh, abfLow, abfCritical, abfCharging, abfNoBattery, abfUnknown);
  TAPMBatteryFlags = set of TAPMBatteryFlag;

function GetAPMLineStatus: TAPMLineStatus;
function GetAPMBatteryFlag: TAPMBatteryFlag;
function GetAPMBatteryFlags: TAPMBatteryFlags;
function GetAPMBatteryLifePercent: Integer;
function GetAPMBatteryLifeTime: DWORD;
function GetAPMBatteryFullLifeTime: DWORD;

// Identification
type
  TFileSystemFlag =
   (
    fsCaseSensitive,            // The file system supports case-sensitive file names.
    fsCasePreservedNames,       // The file system preserves the case of file names when it places a name on disk.
    fsSupportsUnicodeOnDisk,    // The file system supports Unicode in file names as they appear on disk.
    fsPersistentACLs,           // The file system preserves and enforces ACLs. For example, NTFS preserves and enforces ACLs, and FAT does not.
    fsSupportsFileCompression,  // The file system supports file-based compression.
    fsSupportsVolumeQuotas,     // The file system supports disk quotas.
    fsSupportsSparseFiles,      // The file system supports sparse files.
    fsSupportsReparsePoints,    // The file system supports reparse points.
    fsSupportsRemoteStorage,    // ?
    fsVolumeIsCompressed,       // The specified volume is a compressed volume; for example, a DoubleSpace volume.
    fsSupportsObjectIds,        // The file system supports object identifiers.
    fsSupportsEncryption,       // The file system supports the Encrypted File System (EFS).
    fsSupportsNamedStreams,     // The file system supports named streams.
    fsVolumeIsReadOnly          // The specified volume is read-only.
                                //   Windows 2000/NT and Windows Me/98/95:  This value is not supported.
   );

  TFileSystemFlags = set of TFileSystemFlag;

function GetVolumeName(const Drive: string): string;
function GetVolumeSerialNumber(const Drive: string): string;
function GetVolumeFileSystem(const Drive: string): string;
function GetVolumeFileSystemFlags(const Volume: string): TFileSystemFlags;
{$ENDIF ~CLR}
{$ENDIF MSWINDOWS}
function GetIPAddress(const HostName: string): string;
{$IFDEF UNIX}
procedure GetIpAddresses(Results: TStrings);
{$ENDIF UNIX}
function GetLocalComputerName: string;
{$IFNDEF CLR}
function GetLocalUserName: string;
{$IFDEF MSWINDOWS}
function GetUserDomainName(const CurUser: string): string;
{$ENDIF MSWINDOWS}
function GetDomainName: string;
{$IFDEF MSWINDOWS}
function GetRegisteredCompany: string;
function GetRegisteredOwner: string;
function GetBIOSName: string;
function GetBIOSCopyright: string;
function GetBIOSExtendedInfo: string;
function GetBIOSDate: TDateTime;
{$ENDIF MSWINDOWS}

// Processes, Tasks and Modules
type
  TJclTerminateAppResult = (taError, taClean, taKill);
{$ENDIF ~CLR}

function RunningProcessesList(const List: TStrings; FullPath: Boolean = True): Boolean;

{$IFNDEF CLR}
{$IFDEF MSWINDOWS}
function LoadedModulesList(const List: TStrings; ProcessID: DWORD; HandlesOnly: Boolean = False): Boolean;
function GetTasksList(const List: TStrings): Boolean;

function ModuleFromAddr(const Addr: Pointer): HMODULE;
{$IFNDEF FPC}
function IsSystemModule(const Module: HMODULE): Boolean;
{$ENDIF ~FPC}

function IsMainAppWindow(Wnd: HWND): Boolean;
function IsWindowResponding(Wnd: HWND; Timeout: Integer): Boolean;

function GetWindowIcon(Wnd: HWND; LargeIcon: Boolean): HICON;
function GetWindowCaption(Wnd: HWND): string;
function TerminateTask(Wnd: HWND; Timeout: Integer): TJclTerminateAppResult;
{$ENDIF ~CLR}
function TerminateApp(ProcessID: DWORD; Timeout: Integer): TJclTerminateAppResult;

{$IFNDEF CLR}
{.$IFNDEF FPC}
function GetPidFromProcessName(const ProcessName: string): DWORD;
function GetProcessNameFromWnd(Wnd: HWND): string;
function GetProcessNameFromPid(PID: DWORD): string;
function GetMainAppWndFromPid(PID: DWORD): HWND;
{.$ENDIF ~FPC}

function GetShellProcessName: string;
{.$IFNDEF FPC}
function GetShellProcessHandle: THandle;
{.$ENDIF ~FPC}

// Version Information
type
  TWindowsVersion =
   (wvUnknown, wvWin95, wvWin95OSR2, wvWin98, wvWin98SE, wvWinME,
    wvWinNT31, wvWinNT35, wvWinNT351, wvWinNT4, wvWin2000, wvWinXP, wvWin2003);
  TNtProductType =
   (ptUnknown, ptWorkStation, ptServer, ptAdvancedServer,
    ptPersonal, ptProfessional, ptDatacenterServer);

var
  { in case of additions, don't forget to update initialization section! }
  IsWin95: Boolean = False;
  IsWin95OSR2: Boolean = False;
  IsWin98: Boolean = False;
  IsWin98SE: Boolean = False;
  IsWinME: Boolean = False;
  IsWinNT: Boolean = False;
  IsWinNT3: Boolean = False;
  IsWinNT31: Boolean = False;
  IsWinNT35: Boolean = False;
  IsWinNT351: Boolean = False;
  IsWinNT4: Boolean = False;
  IsWin2K: Boolean = False;
  IsWinXP: Boolean = False;
  IsWin2003: Boolean = False;

function GetWindowsVersion: TWindowsVersion;
function NtProductType: TNtProductType;
function GetWindowsVersionString: string;
function NtProductTypeString: string;
function GetWindowsServicePackVersion: Integer;
function GetWindowsServicePackVersionString: string;
function GetOpenGLVersion(const Win: HWND; out Version, Vendor: AnsiString): Boolean;
{$ENDIF MSWINDOWS}

function GetOSVersionString: string;

// Hardware
{$IFDEF MSWINDOWS}
function GetMacAddresses(const Machine: string; const Addresses: TStrings): Integer;
{$ENDIF MSWINDOWS}
function ReadTimeStampCounter: Int64;

type
  TTLBInformation = (tiEntries, tiAssociativity);
  TCacheInformation = (ciLineSize {in Bytes}, ciLinesPerTag, ciAssociativity, ciSize);

  TIntelSpecific = record
    L2Cache: Cardinal;
    CacheDescriptors: array [0..15] of Byte;
    BrandID: Byte;
    ExFeatures: Cardinal;
    Ex64Features: Cardinal;
  end;

  TCyrixSpecific = record
    L1CacheInfo: array [0..3] of Byte;
    TLBInfo: array [0..3] of Byte;
  end;

  TAMDSpecific = record
    ExFeatures: Cardinal;
    MByteDataTLB: array [TTLBInformation] of Byte;
    MByteInstructionTLB: array [TTLBInformation] of Byte;
    KByteDataTLB: array [TTLBInformation] of Byte;
    KByteInstructionTLB: array [TTLBInformation] of Byte;
    L1DataCache: array [TCacheInformation] of Byte;
    L1InstructionCache: array [TCacheInformation] of Byte;
    L2MByteDataTLB: array [TTLBInformation] of Byte;           // L2 TLB for 2-MByte and 4-MByte pages
    L2MByteInstructionTLB: array [TTLBInformation] of Byte;    // L2 TLB for 2-MByte and 4-MByte pages
    L2KByteDataTLB: array [TTLBInformation] of Byte;           // L2 TLB for 4-KByte pages
    L2KByteInstructionTLB: array [TTLBInformation] of Byte;    // L2 TLB for 4-KByte pages
    L2Cache: Cardinal;
    AdvancedPowerManagement: Cardinal;
    PhysicalAddressSize: Byte;
    VirtualAddressSize: Byte;
  end;

  TVIASpecific = record
    ExFeatures: Cardinal;
    DataTLB: array [TTLBInformation] of Byte;
    InstructionTLB: array [TTLBInformation] of Byte;
    L1DataCache: array [TCacheInformation] of Byte;
    L1InstructionCache: array [TCacheInformation] of Byte;
    L2DataCache: Cardinal;
  end;

  TTransmetaSpecific = record
    ExFeatures: Cardinal;
    DataTLB: array [TTLBInformation] of Byte;
    CodeTLB: array [TTLBInformation] of Byte;
    L1DataCache: array [TCacheInformation] of Byte;
    L1CodeCache: array [TCacheInformation] of Byte;
    L2Cache: Cardinal;
    RevisionABCD: Cardinal;
    RevisionXXXX: Cardinal;
    Frequency: Cardinal;
    CodeMorphingABCD: Cardinal;
    CodeMorphingXXXX: Cardinal;
    TransmetaFeatures: Cardinal;
    TransmetaInformations: array [0..64] of Char;
    CurrentVoltage: Cardinal;
    CurrentFrequency: Cardinal;
    CurrentPerformance: Cardinal;
  end;

  TCacheFamily = ( cfInstructionTLB, cfDataTLB,
                   cfL1InstructionCache, cfL1DataCache,
                   cfL2Cache, cfL3Cache, cfTrace, cfOther );

  TCacheInfo = record
    D: Byte;
    Family: TCacheFamily;
    Size: Cardinal;
    WaysOfAssoc: Byte;
    LineSize: Byte;       // for Normal Cache
    LinePerSector: Byte;  // for L3 Normal Cache
    Entries: Cardinal;        // for TLB
    I: string;
  end; 

  TFreqInfo = record
    RawFreq: Cardinal;
    NormFreq: Cardinal;
    InCycles: Cardinal;
    ExTicks: Cardinal;
  end;

const
  CPU_TYPE_INTEL     = 1;
  CPU_TYPE_CYRIX     = 2;
  CPU_TYPE_AMD       = 3;
  CPU_TYPE_TRANSMETA = 4;
  CPU_TYPE_VIA       = 5;

type
  TCpuInfo = record
    HasInstruction: Boolean;
    MMX: Boolean;
    ExMMX: Boolean;
    _3DNow: Boolean;
    Ex3DNow: Boolean;
    SSE: Byte;        // SSE version 0 = no SSE, 1 = SSE, 2 = SSE2, 3 = SSE3
    IsFDIVOK: Boolean;
    Is64Bits: Boolean;
    HasCacheInfo: Boolean;
    HasExtendedInfo: Boolean;
    PType: Byte;
    Family: Byte;
    ExtendedFamily: Byte;
    Model: Byte;
    ExtendedModel: Byte;
    Stepping: Byte;
    Features: Cardinal;
    FrequencyInfo: TFreqInfo;
    VendorIDString: array [0..11] of Char;
    Manufacturer: array [0..9] of Char;
    CpuName: array [0..47] of Char;
    L1DataCacheSize: Cardinal;             // in kByte
    L1DataCacheLineSize: Byte;             // in Byte
    L1DataCacheAssociativity: Byte;
    L1InstructionCacheSize: Cardinal;      // in kByte
    L1InstructionCacheLineSize: Byte;      // in Byte
    L1InstructionCacheAssociativity: Byte;
    L2CacheSize: Cardinal;                 // in kByte
    L2CacheLineSize: Byte;                 // in Byte
    L2CacheAssociativity: Byte;
    L3CacheSize: Cardinal;                 // in kByte
    L3CacheLineSize: Byte;                 // in Byte
    L3CacheAssociativity: Byte;
    L3LinesPerSector: Byte;
    // todo: TLB
    case CpuType: Byte of
      CPU_TYPE_INTEL     : (IntelSpecific: TIntelSpecific;);
      CPU_TYPE_CYRIX     : (CyrixSpecific: TCyrixSpecific;);
      CPU_TYPE_AMD       : (AMDSpecific: TAMDSpecific;);
      CPU_TYPE_TRANSMETA : (TransmetaSpecific: TTransmetaSpecific;);
      CPU_TYPE_VIA       : (ViaSpecific: TViaSpecific;);
  end;

const
  VendorIDIntel     : array [0..11] of Char = 'GenuineIntel';
  VendorIDCyrix     : array [0..11] of Char = 'CyrixInstead';
  VendorIDAMD       : array [0..11] of Char = 'AuthenticAMD';
  VendorIDTransmeta : array [0..11] of Char = 'GenuineTMx86';
  VendorIDVIA       : array [0..11] of Char = 'CentaurHauls';

// Constants to be used with Feature Flag set of a CPU
// eg. IF (Features and FPU_FLAG = FPU_FLAG) THEN CPU has Floating-Point unit on
// chip. However, Intel claims that in future models, a zero in the feature
// flags will mean that the chip has that feature, however, the following flags
// will work for any production 80x86 chip or clone.
// eg. IF (Features and FPU_FLAG = 0) then CPU has Floating-Point unit on chip.

const

{ 32 bits in a DWord Value }

  BIT_0       = $00000001;
  BIT_1       = $00000002;
  BIT_2       = $00000004;
  BIT_3       = $00000008;
  BIT_4       = $00000010;
  BIT_5       = $00000020;
  BIT_6       = $00000040;
  BIT_7       = $00000080;
  BIT_8       = $00000100;
  BIT_9       = $00000200;
  BIT_10      = $00000400;
  BIT_11      = $00000800;
  BIT_12      = $00001000;
  BIT_13      = $00002000;
  BIT_14      = $00004000;
  BIT_15      = $00008000;
  BIT_16      = $00010000;
  BIT_17      = $00020000;
  BIT_18      = $00040000;
  BIT_19      = $00080000;
  BIT_20      = $00100000;
  BIT_21      = $00200000;
  BIT_22      = $00400000;
  BIT_23      = $00800000;
  BIT_24      = $01000000;
  BIT_25      = $02000000;
  BIT_26      = $04000000;
  BIT_27      = $08000000;
  BIT_28      = $10000000;
  BIT_29      = $20000000;
  BIT_30      = $40000000;
  BIT_31      = DWORD($80000000);

{ Standard Feature Flags }

  FPU_FLAG    = BIT_0 ; // Floating-Point unit on chip
  VME_FLAG    = BIT_1 ; // Virtual Mode Extention
  DE_FLAG     = BIT_2 ; // Debugging Extention
  PSE_FLAG    = BIT_3 ; // Page Size Extention
  TSC_FLAG    = BIT_4 ; // Time Stamp Counter
  MSR_FLAG    = BIT_5 ; // Model Specific Registers
  PAE_FLAG    = BIT_6 ; // Physical Address Extention
  MCE_FLAG    = BIT_7 ; // Machine Check Exception
  CX8_FLAG    = BIT_8 ; // CMPXCHG8 Instruction
  APIC_FLAG   = BIT_9 ; // Software-accessible local APIC on Chip
  BIT_10_FLAG = BIT_10; // Reserved, do not count on value
  SEP_FLAG    = BIT_11; // Fast System Call
  MTRR_FLAG   = BIT_12; // Memory Type Range Registers
  PGE_FLAG    = BIT_13; // Page Global Enable
  MCA_FLAG    = BIT_14; // Machine Check Architecture
  CMOV_FLAG   = BIT_15; // Conditional Move Instruction
  PAT_FLAG    = BIT_16; // Page Attribute Table
  PSE36_FLAG  = BIT_17; // 36-bit Page Size Extention
  PSN_FLAG    = BIT_18; // Processor serial number is present and enabled
  CLFLSH_FLAG = BIT_19; // CLFLUSH intruction
  BIT_20_FLAG = BIT_20; // Reserved, do not count on value
  DS_FLAG     = BIT_21; // Debug store
  ACPI_FLAG   = BIT_22; // Thermal monitor and clock control
  MMX_FLAG    = BIT_23; // MMX technology
  FXSR_FLAG   = BIT_24; // Fast Floating Point Save and Restore
  SSE_FLAG    = BIT_25; // Streaming SIMD Extensions
  SSE2_FLAG   = BIT_26; // Streaming SIMD Extensions 2
  SS_FLAG     = BIT_27; // Self snoop
  HTT_FLAG    = BIT_28; // Hyper-threading technology
  TM_FLAG     = BIT_29; // Thermal monitor
  BIT_30_FLAG = BIT_30; // Reserved, do not count on value
  PBE_FLAG    = BIT_31; // Pending Break Enable

{ Standard Intel Feature Flags }

  INTEL_FPU    = BIT_0 ; // Floating-Point unit on chip
  INTEL_VME    = BIT_1 ; // Virtual Mode Extention
  INTEL_DE     = BIT_2 ; // Debugging Extention
  INTEL_PSE    = BIT_3 ; // Page Size Extention
  INTEL_TSC    = BIT_4 ; // Time Stamp Counter
  INTEL_MSR    = BIT_5 ; // Model Specific Registers
  INTEL_PAE    = BIT_6 ; // Physical Address Extention
  INTEL_MCE    = BIT_7 ; // Machine Check Exception
  INTEL_CX8    = BIT_8 ; // CMPXCHG8 Instruction
  INTEL_APIC   = BIT_9 ; // Software-accessible local APIC on Chip
  INTEL_BIT_10 = BIT_10; // Reserved, do not count on value
  INTEL_SEP    = BIT_11; // Fast System Call
  INTEL_MTRR   = BIT_12; // Memory Type Range Registers
  INTEL_PGE    = BIT_13; // Page Global Enable
  INTEL_MCA    = BIT_14; // Machine Check Architecture
  INTEL_CMOV   = BIT_15; // Conditional Move Instruction
  INTEL_PAT    = BIT_16; // Page Attribute Table
  INTEL_PSE36  = BIT_17; // 36-bit Page Size Extention
  INTEL_PSN    = BIT_18; // Processor serial number is present and enabled
  INTEL_CLFLSH = BIT_19; // CLFLUSH intruction
  INTEL_BIT_20 = BIT_20; // Reserved, do not count on value
  INTEL_DS     = BIT_21; // Debug store
  INTEL_ACPI   = BIT_22; // Thermal monitor and clock control
  INTEL_MMX    = BIT_23; // MMX technology
  INTEL_FXSR   = BIT_24; // Fast Floating Point Save and Restore
  INTEL_SSE    = BIT_25; // Streaming SIMD Extensions
  INTEL_SSE2   = BIT_26; // Streaming SIMD Extensions 2
  INTEL_SS     = BIT_27; // Self snoop
  INTEL_HTT    = BIT_28; // Hyper-threading technology
  INTEL_TM     = BIT_29; // Thermal monitor
  INTEL_BIT_30 = BIT_30; // Reserved, do not count on value
  INTEL_PBE    = BIT_31; // Pending Break Enable

{ Extended Intel Feature Flags }

  EINTEL_SSE3    = BIT_0 ; // Streaming SIMD Extensions 3
  EINTEL_BIT_1   = BIT_1 ; // Reserved, do not count on value
  EINTEL_BIT_2   = BIT_2 ; // Reserved, do not count on value
  EINTEL_MONITOR = BIT_3 ; // Monitor/MWAIT
  EINTEL_DSCPL   = BIT_4 ; // CPL Qualified debug Store
  EINTEL_BIT_5   = BIT_5 ; // Reserved, do not count on value
  EINTEL_BIT_6   = BIT_6 ; // Reserved, do not count on value
  EINTEL_EST     = BIT_7 ; // Enhanced Intel Speedstep technology
  EINTEL_TM2     = BIT_8 ; // Thermal monitor 2
  EINTEL_BIT_9   = BIT_9 ; // Reserved, do not count on value
  EINTEL_CNXTID  = BIT_10; // L1 Context ID
  EINTEL_BIT_11  = BIT_11; // Reserved, do not count on value
  EINTEL_BIT_12  = BIT_12; // Reserved, do not count on value
  EINTEL_BIT_13  = BIT_13; // Reserved, do not count on value
  EINTEL_XTPR    = BIT_14; // Send Task Priority messages
  EINTEL_BIT_15  = BIT_15; // Reserved, do not count on value
  EINTEL_BIT_16  = BIT_16; // Reserved, do not count on value
  EINTEL_BIT_17  = BIT_17; // Reserved, do not count on value
  EINTEL_BIT_18  = BIT_18; // Reserved, do not count on value
  EINTEL_BIT_19  = BIT_19; // Reserved, do not count on value
  EINTEL_BIT_20  = BIT_20; // Reserved, do not count on value
  EINTEL_BIT_21  = BIT_21; // Reserved, do not count on value
  EINTEL_BIT_22  = BIT_22; // Reserved, do not count on value
  EINTEL_BIT_23  = BIT_23; // Reserved, do not count on value
  EINTEL_BIT_24  = BIT_24; // Reserved, do not count on value
  EINTEL_BIT_25  = BIT_25; // Reserved, do not count on value
  EINTEL_BIT_26  = BIT_26; // Reserved, do not count on value
  EINTEL_BIT_27  = BIT_27; // Reserved, do not count on value
  EINTEL_BIT_28  = BIT_28; // Reserved, do not count on value
  EINTEL_BIT_29  = BIT_29; // Reserved, do not count on value
  EINTEL_BIT_30  = BIT_30; // Reserved, do not count on value
  EINTEL_BIT_31  = BIT_31; // Reserved, do not count on value

{ Extended Intel 64 Bits Feature Flags }

  EINTEL64_BIT_0  = BIT_0 ; // Reserved, do not count on value
  EINTEL64_BIT_1  = BIT_1 ; // Reserved, do not count on value
  EINTEL64_BIT_2  = BIT_2 ; // Reserved, do not count on value
  EINTEL64_BIT_3  = BIT_3 ; // Reserved, do not count on value
  EINTEL64_BIT_4  = BIT_4 ; // Reserved, do not count on value
  EINTEL64_BIT_5  = BIT_5 ; // Reserved, do not count on value
  EINTEL64_BIT_6  = BIT_6 ; // Reserved, do not count on value
  EINTEL64_BIT_7  = BIT_7 ; // Reserved, do not count on value
  EINTEL64_BIT_8  = BIT_8 ; // Reserved, do not count on value
  EINTEL64_BIT_9  = BIT_9 ; // Reserved, do not count on value
  EINTEL64_BIT_10 = BIT_10; // Reserved, do not count on value
  EINTEL64_SYS    = BIT_11; // 64 Bit - SYSCALL SYSRET
  EINTEL64_BIT_12 = BIT_12; // Reserved, do not count on value
  EINTEL64_BIT_13 = BIT_13; // Reserved, do not count on value
  EINTEL64_BIT_14 = BIT_14; // Reserved, do not count on value
  EINTEL64_BIT_15 = BIT_15; // Reserved, do not count on value
  EINTEL64_BIT_16 = BIT_16; // Reserved, do not count on value
  EINTEL64_BIT_17 = BIT_17; // Reserved, do not count on value
  EINTEL64_BIT_18 = BIT_18; // Reserved, do not count on value
  EINTEL64_BIT_19 = BIT_19; // Reserved, do not count on value
  EINTEL64_BIT_20 = BIT_20; // Reserved, do not count on value
  EINTEL64_BIT_21 = BIT_21; // Reserved, do not count on value
  EINTEL64_BIT_22 = BIT_22; // Reserved, do not count on value
  EINTEL64_BIT_23 = BIT_23; // Reserved, do not count on value
  EINTEL64_BIT_24 = BIT_24; // Reserved, do not count on value
  EINTEL64_BIT_25 = BIT_25; // Reserved, do not count on value
  EINTEL64_BIT_26 = BIT_26; // Reserved, do not count on value
  EINTEL64_BIT_27 = BIT_27; // Reserved, do not count on value
  EINTEL64_BIT_28 = BIT_28; // Reserved, do not count on value
  EINTEL64_EM64T  = BIT_29; // Intel® Extended Memory 64 Technology
  EINTEL64_BIT_30 = BIT_30; // Reserved, do not count on value
  EINTEL64_BIT_31 = BIT_31; // Reserved, do not count on value

{ AMD Standard Feature Flags }

  AMD_FPU     = BIT_0 ; // Floating-Point unit on chip
  AMD_VME     = BIT_1 ; // Virtual Mode Extention
  AMD_DE      = BIT_2 ; // Debugging Extention
  AMD_PSE     = BIT_3 ; // Page Size Extention
  AMD_TSC     = BIT_4 ; // Time Stamp Counter
  AMD_MSR     = BIT_5 ; // Model Specific Registers
  AMD_PAE     = BIT_6 ; // Physical address Extensions
  AMD_MCE     = BIT_7 ; // Machine Check Exception
  AMD_CX8     = BIT_8 ; // CMPXCHG8 Instruction
  AMD_APIC    = BIT_9 ; // Software-accessible local APIC on Chip
  AMD_BIT_10  = BIT_10; // Reserved, do not count on value
  AMD_SEP_BIT = BIT_11; // SYSENTER and SYSEXIT instructions
  AMD_MTRR    = BIT_12; // Memory Type Range Registers
  AMD_PGE     = BIT_13; // Page Global Enable
  AMD_MCA     = BIT_14; // Machine Check Architecture
  AMD_CMOV    = BIT_15; // Conditional Move Instruction
  AMD_PAT     = BIT_16; // Page Attribute Table
  AMD_PSE2    = BIT_17; // Page Size Extensions
  AMD_BIT_18  = BIT_18; // Reserved, do not count on value
  AMD_CLFLSH  = BIT_19; // CLFLUSH instruction
  AMD_BIT_20  = BIT_20; // Reserved, do not count on value
  AMD_BIT_21  = BIT_21; // Reserved, do not count on value
  AMD_BIT_22  = BIT_22; // Reserved, do not count on value
  AMD_MMX     = BIT_23; // MMX technology
  AMD_FX      = BIT_24; // FXSAVE and FXSTORE instructions
  AMD_SSE     = BIT_25; // SSE Extensions
  AMD_SSE2    = BIT_26; // SSE2 Extensions
  AMD_BIT_27  = BIT_27; // Reserved, do not count on value
  AMD_BIT_28  = BIT_28; // Reserved, do not count on value
  AMD_BIT_29  = BIT_29; // Reserved, do not count on value
  AMD_BIT_30  = BIT_30; // Reserved, do not count on value
  AMD_BIT_31  = BIT_31; // Reserved, do not count on value

{ AMD Enhanced Feature Flags }

  EAMD_FPU     = BIT_0 ; // Floating-Point unit on chip
  EAMD_VME     = BIT_1 ; // Virtual Mode Extention
  EAMD_DE      = BIT_2 ; // Debugging Extention
  EAMD_PSE     = BIT_3 ; // Page Size Extention
  EAMD_TSC     = BIT_4 ; // Time Stamp Counter
  EAMD_MSR     = BIT_5 ; // Model Specific Registers
  EAMD_PAE     = BIT_6 ; // Physical-address extensions
  EAMD_MCE     = BIT_7 ; // Machine Check Exception
  EAMD_CX8     = BIT_8 ; // CMPXCHG8 Instruction
  EAMD_APIC    = BIT_9 ; // Advanced Programmable Interrupt Controler
  EAMD_BIT_10  = BIT_10; // Reserved, do not count on value
  EAMD_SEP     = BIT_11; // Fast System Call
  EAMD_MTRR    = BIT_12; // Memory-Type Range Registers
  EAMD_PGE     = BIT_13; // Page Global Enable
  EAMD_MCA     = BIT_14; // Machine Check Architecture
  EAMD_CMOV    = BIT_15; // Conditional Move Intructions
  EAMD_PAT     = BIT_16; // Page Attributes Table
  EAMD_PSE2    = BIT_17; // Page Size Extensions
  EAMD_BIT_18  = BIT_18; // Reserved, do not count on value
  EAMD_BIT_19  = BIT_19; // Reserved, do not count on value
  EAMD_NEPP    = BIT_20; // No-Execute Page Protection
  EAMD_BIT_21  = BIT_21; // Reserved, do not count on value
  EAMD_EXMMX   = BIT_22; // AMD Extensions to MMX technology
  EAMD_MMX     = BIT_23; // MMX technology
  EAMD_FX      = BIT_24; // FXSAVE and FXSTORE instructions
  EAMD_FFX     = BIT_25; // Fast FXSAVE and FXSTORE instructions
  EAMD_BIT_26  = BIT_26; // Reserved, do not count on value
  EAMD_BIT_27  = BIT_27; // Reserved, do not count on value
  EAMD_BIT_28  = BIT_28; // Reserved, do not count on value
  EAMD_LONG    = BIT_29; // Long Mode (64-bit Core)
  EAMD_EX3DNOW = BIT_30; // AMD Extensions to 3DNow! intructions
  EAMD_3DNOW   = BIT_31; // AMD 3DNOW! Technology

{ AMD Power Management Features Flags }

  PAMD_TEMPSENSOR       = $00000001;  // Temperature Sensor
  PAMD_FREQUENCYID      = $00000002;  // Frequency ID Control
  PAMD_VOLTAGEID        = $00000004;  // Voltage ID Control
  PAMD_THERMALTRIP      = $00000008;  // Thermal Trip
  PAMD_THERMALMONITOR   = $00000010;  // Thermal Monitoring
  PAMD_SOFTTHERMCONTROL = $00000020;  // Software Thermal Control

{ AMD TLB and L1 Associativity constants }

  AMD_ASSOC_RESERVED = 0;
  AMD_ASSOC_DIRECT   = 1;
  // 2 to 254 = direct value to the associativity
  AMD_ASSOC_FULLY    = 255;

{ AMD L2 Cache Associativity constants }

  AMD_L2_ASSOC_DISABLED = 0;
  AMD_L2_ASSOC_DIRECT   = 1;
  AMD_L2_ASSOC_2WAY     = 2;
  AMD_L2_ASSOC_4WAY     = 4;
  AMD_L2_ASSOC_8WAY     = 6;
  AMD_L2_ASSOC_16WAY    = 8;
  AMD_L2_ASSOC_FULLY    = 15;

{ VIA Standard Feature Flags }

  VIA_FPU           = BIT_0 ; // FPU present
  VIA_VME           = BIT_1 ; // Virtual Mode Extension
  VIA_DE            = BIT_2 ; // Debugging extensions
  VIA_PSE           = BIT_3 ; // Page Size Extensions (4MB)
  VIA_TSC           = BIT_4 ; // Time Stamp Counter
  VIA_MSR           = BIT_5 ; // Model Specific Registers
  VIA_PAE           = BIT_6 ; // Physical Address Extension
  VIA_MCE           = BIT_7 ; // Machine Check Exception
  VIA_CX8           = BIT_8 ; // CMPXCHG8B instruction
  VIA_APIC          = BIT_9 ; // APIC supported
  VIA_BIT_10        = BIT_10; // Reserved, do not count on value
  VIA_SEP           = BIT_11; // Fast System Call
  VIA_MTRR          = BIT_12; // Memory Range Registers
  VIA_PTE           = BIT_13; // PTE Global Bit
  VIA_MCA           = BIT_14; // Machine Check Architecture
  VIA_CMOVE         = BIT_15; // Conditional Move
  VIA_PAT           = BIT_16; // Page Attribute Table
  VIA_PSE2          = BIT_17; // 36-bit Page Size Extension
  VIA_SNUM          = BIT_18; // Processor serial number
  VIA_BIT_19        = BIT_19; // Reserved, do not count on value
  VIA_BIT_20        = BIT_20; // Reserved, do not count on value
  VIA_BIT_21        = BIT_21; // Reserved, do not count on value
  VIA_BIT_22        = BIT_22; // Reserved, do not count on value
  VIA_MMX           = BIT_23; // MMX
  VIA_FX            = BIT_24; // FXSAVE and FXSTORE instructions
  VIA_SSE           = BIT_25; // Streaming SIMD Extension
  VIA_BIT_26        = BIT_26; // Reserved, do not count on value
  VIA_BIT_27        = BIT_27; // Reserved, do not count on value
  VIA_BIT_28        = BIT_28; // Reserved, do not count on value
  VIA_BIT_29        = BIT_29; // Reserved, do not count on value
  VIA_BIT_30        = BIT_30; // Reserved, do not count on value
  VIA_3DNOW         = BIT_31; // 3DNow! Technology

{ VIA Extended Feature Flags }

  EVIA_AIS    = BIT_0;  // Alternate Instruction Set
  EVIA_AISE   = BIT_1;  // Alternate Instruction Set Enabled
  EVIA_NO_RNG = BIT_2;  // NO Random Number Generator
  EVIA_RNGE   = BIT_3;  // Random Number Generator Enabled
  EVIA_MSR    = BIT_4;  // Longhaul MSR 0x110A available
  EVIA_FEMMS  = BIT_5;  // FEMMS instruction Present
  EVIA_NO_ACE = BIT_6;  // Advanced Cryptography Engine NOT Present
  EVIA_ACEE   = BIT_7;  // ACE Enabled
  EVIA_BIT_8  = BIT_8;  // Reserved, do not count on value
  EVIA_BIT_9  = BIT_9;  // Reserved, do not count on value
  EVIA_BIT_10 = BIT_10; // Reserved, do not count on value
  EVIA_BIT_11 = BIT_11; // Reserved, do not count on value
  EVIA_BIT_12 = BIT_12; // Reserved, do not count on value
  EVIA_BIT_13 = BIT_13; // Reserved, do not count on value
  EVIA_BIT_14 = BIT_14; // Reserved, do not count on value
  EVIA_BIT_15 = BIT_15; // Reserved, do not count on value
  EVIA_BIT_16 = BIT_16; // Reserved, do not count on value
  EVIA_BIT_17 = BIT_17; // Reserved, do not count on value
  EVIA_BIT_18 = BIT_18; // Reserved, do not count on value
  EVIA_BIT_19 = BIT_19; // Reserved, do not count on value
  EVIA_BIT_20 = BIT_20; // Reserved, do not count on value
  EVIA_BIT_21 = BIT_21; // Reserved, do not count on value
  EVIA_BIT_22 = BIT_22; // Reserved, do not count on value
  EVIA_BIT_23 = BIT_23; // Reserved, do not count on value
  EVIA_BIT_24 = BIT_24; // Reserved, do not count on value
  EVIA_BIT_25 = BIT_25; // Reserved, do not count on value
  EVIA_BIT_26 = BIT_26; // Reserved, do not count on value
  EVIA_BIT_27 = BIT_27; // Reserved, do not count on value
  EVIA_BIT_28 = BIT_28; // Reserved, do not count on value
  EVIA_BIT_29 = BIT_29; // Reserved, do not count on value
  EVIA_BIT_30 = BIT_30; // Reserved, do not count on value
  EVIA_BIT_31 = BIT_31; // Reserved, do not count on value

{ Cyrix Standard Feature Flags }

  CYRIX_FPU    = BIT_0 ; // Floating-Point unit on chip
  CYRIX_VME    = BIT_1 ; // Virtual Mode Extention
  CYRIX_DE     = BIT_2 ; // Debugging Extention
  CYRIX_PSE    = BIT_3 ; // Page Size Extention
  CYRIX_TSC    = BIT_4 ; // Time Stamp Counter
  CYRIX_MSR    = BIT_5 ; // Model Specific Registers
  CYRIX_PAE    = BIT_6 ; // Physical Address Extention
  CYRIX_MCE    = BIT_7 ; // Machine Check Exception
  CYRIX_CX8    = BIT_8 ; // CMPXCHG8 Instruction
  CYRIX_APIC   = BIT_9 ; // Software-accessible local APIC on Chip
  CYRIX_BIT_10 = BIT_10; // Reserved, do not count on value
  CYRIX_BIT_11 = BIT_11; // Reserved, do not count on value
  CYRIX_MTRR   = BIT_12; // Memory Type Range Registers
  CYRIX_PGE    = BIT_13; // Page Global Enable
  CYRIX_MCA    = BIT_14; // Machine Check Architecture
  CYRIX_CMOV   = BIT_15; // Conditional Move Instruction
  CYRIX_BIT_16 = BIT_16; // Reserved, do not count on value
  CYRIX_BIT_17 = BIT_17; // Reserved, do not count on value
  CYRIX_BIT_18 = BIT_18; // Reserved, do not count on value
  CYRIX_BIT_19 = BIT_19; // Reserved, do not count on value
  CYRIX_BIT_20 = BIT_20; // Reserved, do not count on value
  CYRIX_BIT_21 = BIT_21; // Reserved, do not count on value
  CYRIX_BIT_22 = BIT_22; // Reserved, do not count on value
  CYRIX_MMX    = BIT_23; // MMX technology
  CYRIX_BIT_24 = BIT_24; // Reserved, do not count on value
  CYRIX_BIT_25 = BIT_25; // Reserved, do not count on value
  CYRIX_BIT_26 = BIT_26; // Reserved, do not count on value
  CYRIX_BIT_27 = BIT_27; // Reserved, do not count on value
  CYRIX_BIT_28 = BIT_28; // Reserved, do not count on value
  CYRIX_BIT_29 = BIT_29; // Reserved, do not count on value
  CYRIX_BIT_30 = BIT_30; // Reserved, do not count on value
  CYRIX_BIT_31 = BIT_31; // Reserved, do not count on value

{ Cyrix Enhanced Feature Flags }

  ECYRIX_FPU    = BIT_0 ; // Floating-Point unit on chip
  ECYRIX_VME    = BIT_1 ; // Virtual Mode Extention
  ECYRIX_DE     = BIT_2 ; // Debugging Extention
  ECYRIX_PSE    = BIT_3 ; // Page Size Extention
  ECYRIX_TSC    = BIT_4 ; // Time Stamp Counter
  ECYRIX_MSR    = BIT_5 ; // Model Specific Registers
  ECYRIX_PAE    = BIT_6 ; // Physical Address Extention
  ECYRIX_MCE    = BIT_7 ; // Machine Check Exception
  ECYRIX_CX8    = BIT_8 ; // CMPXCHG8 Instruction
  ECYRIX_APIC   = BIT_9 ; // Software-accessible local APIC on Chip
  ECYRIX_SEP    = BIT_10; // Fast System Call
  ECYRIX_BIT_11 = BIT_11; // Reserved, do not count on value
  ECYRIX_MTRR   = BIT_12; // Memory Type Range Registers
  ECYRIX_PGE    = BIT_13; // Page Global Enable
  ECYRIX_MCA    = BIT_14; // Machine Check Architecture
  ECYRIX_ICMOV  = BIT_15; // Integer Conditional Move Instruction
  ECYRIX_FCMOV  = BIT_16; // Floating Point Conditional Move Instruction
  ECYRIX_BIT_17 = BIT_17; // Reserved, do not count on value
  ECYRIX_BIT_18 = BIT_18; // Reserved, do not count on value
  ECYRIX_BIT_19 = BIT_19; // Reserved, do not count on value
  ECYRIX_BIT_20 = BIT_20; // Reserved, do not count on value
  ECYRIX_BIT_21 = BIT_21; // Reserved, do not count on value
  ECYRIX_BIT_22 = BIT_22; // Reserved, do not count on value
  ECYRIX_MMX    = BIT_23; // MMX technology
  ECYRIX_EMMX   = BIT_24; // Extended MMX Technology
  ECYRIX_BIT_25 = BIT_25; // Reserved, do not count on value
  ECYRIX_BIT_26 = BIT_26; // Reserved, do not count on value
  ECYRIX_BIT_27 = BIT_27; // Reserved, do not count on value
  ECYRIX_BIT_28 = BIT_28; // Reserved, do not count on value
  ECYRIX_BIT_29 = BIT_29; // Reserved, do not count on value
  ECYRIX_BIT_30 = BIT_30; // Reserved, do not count on value
  ECYRIX_BIT_31 = BIT_31; // Reserved, do not count on value

{ Transmeta Features }

  TRANSMETA_FPU    = BIT_0 ; // Floating-Point unit on chip
  TRANSMETA_VME    = BIT_1 ; // Virtual Mode Extention
  TRANSMETA_DE     = BIT_2 ; // Debugging Extention
  TRANSMETA_PSE    = BIT_3 ; // Page Size Extention
  TRANSMETA_TSC    = BIT_4 ; // Time Stamp Counter
  TRANSMETA_MSR    = BIT_5 ; // Model Specific Registers
  TRANSMETA_BIT_6  = BIT_6 ; // Reserved, do not count on value
  TRANSMETA_BIT_7  = BIT_7 ; // Reserved, do not count on value
  TRANSMETA_CX8    = BIT_8 ; // CMPXCHG8 Instruction
  TRANSMETA_BIT_9  = BIT_9 ; // Reserved, do not count on value
  TRANSMETA_BIT_10 = BIT_10; // Reserved, do not count on value
  TRANSMETA_SEP    = BIT_11; // Fast system Call Extensions
  TRANSMETA_BIT_12 = BIT_12; // Reserved, do not count on value
  TRANSMETA_BIT_13 = BIT_13; // Reserved, do not count on value
  TRANSMETA_BIT_14 = BIT_14; // Reserved, do not count on value
  TRANSMETA_CMOV   = BIT_15; // Conditional Move Instruction
  TRANSMETA_BIT_16 = BIT_16; // Reserved, do not count on value
  TRANSMETA_BIT_17 = BIT_17; // Reserved, do not count on value
  TRANSMETA_PSN    = BIT_18; // Processor Serial Number
  TRANSMETA_BIT_19 = BIT_19; // Reserved, do not count on value
  TRANSMETA_BIT_20 = BIT_20; // Reserved, do not count on value
  TRANSMETA_BIT_21 = BIT_21; // Reserved, do not count on value
  TRANSMETA_BIT_22 = BIT_22; // Reserved, do not count on value
  TRANSMETA_MMX    = BIT_23; // MMX technology
  TRANSMETA_BIT_24 = BIT_24; // Reserved, do not count on value
  TRANSMETA_BIT_25 = BIT_25; // Reserved, do not count on value
  TRANSMETA_BIT_26 = BIT_26; // Reserved, do not count on value
  TRANSMETA_BIT_27 = BIT_27; // Reserved, do not count on value
  TRANSMETA_BIT_28 = BIT_28; // Reserved, do not count on value
  TRANSMETA_BIT_29 = BIT_29; // Reserved, do not count on value
  TRANSMETA_BIT_30 = BIT_30; // Reserved, do not count on value
  TRANSMETA_BIT_31 = BIT_31; // Reserved, do not count on value

{ Extended Transmeta Features }

  ETRANSMETA_FPU    = BIT_0 ; // Floating-Point unit on chip
  ETRANSMETA_VME    = BIT_1 ; // Virtual Mode Extention
  ETRANSMETA_DE     = BIT_2 ; // Debugging Extention
  ETRANSMETA_PSE    = BIT_3 ; // Page Size Extention
  ETRANSMETA_TSC    = BIT_4 ; // Time Stamp Counter
  ETRANSMETA_MSR    = BIT_5 ; // Model Specific Registers
  ETRANSMETA_BIT_6  = BIT_6 ; // Reserved, do not count on value
  ETRANSMETA_BIT_7  = BIT_7 ; // Reserved, do not count on value
  ETRANSMETA_CX8    = BIT_8 ; // CMPXCHG8 Instruction
  ETRANSMETA_BIT_9  = BIT_9 ; // Reserved, do not count on value
  ETRANSMETA_BIT_10 = BIT_10; // Reserved, do not count on value
  ETRANSMETA_BIT_11 = BIT_11; // Reserved, do not count on value
  ETRANSMETA_BIT_12 = BIT_12; // Reserved, do not count on value
  ETRANSMETA_BIT_13 = BIT_13; // Reserved, do not count on value
  ETRANSMETA_BIT_14 = BIT_14; // Reserved, do not count on value
  ETRANSMETA_CMOV   = BIT_15; // Conditional Move Instruction
  ETRANSMETA_FCMOV  = BIT_16; // Float Conditional Move Instruction
  ETRANSMETA_BIT_17 = BIT_17; // Reserved, do not count on value
  ETRANSMETA_BIT_18 = BIT_18; // Reserved, do not count on value
  ETRANSMETA_BIT_19 = BIT_19; // Reserved, do not count on value
  ETRANSMETA_BIT_20 = BIT_20; // Reserved, do not count on value
  ETRANSMETA_BIT_21 = BIT_21; // Reserved, do not count on value
  ETRANSMETA_BIT_22 = BIT_22; // Reserved, do not count on value
  ETRANSMETA_MMX    = BIT_23; // MMX technology
  ETRANSMETA_BIT_24 = BIT_24; // Reserved, do not count on value
  ETRANSMETA_BIT_25 = BIT_25; // Reserved, do not count on value
  ETRANSMETA_BIT_26 = BIT_26; // Reserved, do not count on value
  ETRANSMETA_BIT_27 = BIT_27; // Reserved, do not count on value
  ETRANSMETA_BIT_28 = BIT_28; // Reserved, do not count on value
  ETRANSMETA_BIT_29 = BIT_29; // Reserved, do not count on value
  ETRANSMETA_BIT_30 = BIT_30; // Reserved, do not count on value
  ETRANSMETA_BIT_31 = BIT_31; // Reserved, do not count on value

{ Transmeta Specific Features }

  STRANSMETA_RECOVERY = BIT_0 ; // Recovery Mode
  STRANSMETA_LONGRUN  = BIT_1 ; // Long Run
  STRANSMETA_BIT_2    = BIT_2 ; // Debugging Extention
  STRANSMETA_LRTI     = BIT_3 ; // Long Run Table Interface
  STRANSMETA_BIT_4    = BIT_4 ; // Reserved, do not count on value
  STRANSMETA_BIT_5    = BIT_5 ; // Reserved, do not count on value
  STRANSMETA_BIT_6    = BIT_6 ; // Reserved, do not count on value
  STRANSMETA_PTTI1    = BIT_7 ; // Persistent Translation Technology 1.x
  STRANSMETA_PTTI2    = BIT_8 ; // Persistent Translation Technology 2.0
  STRANSMETA_BIT_9    = BIT_9 ; // Reserved, do not count on value
  STRANSMETA_BIT_10   = BIT_10; // Reserved, do not count on value
  STRANSMETA_BIT_11   = BIT_11; // Reserved, do not count on value
  STRANSMETA_BIT_12   = BIT_12; // Reserved, do not count on value
  STRANSMETA_BIT_13   = BIT_13; // Reserved, do not count on value
  STRANSMETA_BIT_14   = BIT_14; // Reserved, do not count on value
  STRANSMETA_BIT_15   = BIT_15; // Reserved, do not count on value
  STRANSMETA_BIT_16   = BIT_16; // Reserved, do not count on value
  STRANSMETA_BIT_17   = BIT_17; // Reserved, do not count on value
  STRANSMETA_BIT_18   = BIT_18; // Reserved, do not count on value
  STRANSMETA_BIT_19   = BIT_19; // Reserved, do not count on value
  STRANSMETA_BIT_20   = BIT_20; // Reserved, do not count on value
  STRANSMETA_BIT_21   = BIT_21; // Reserved, do not count on value
  STRANSMETA_BIT_22   = BIT_22; // Reserved, do not count on value
  STRANSMETA_BIT_23   = BIT_23; // Reserved, do not count on value
  STRANSMETA_BIT_24   = BIT_24; // Reserved, do not count on value
  STRANSMETA_BIT_25   = BIT_25; // Reserved, do not count on value
  STRANSMETA_BIT_26   = BIT_26; // Reserved, do not count on value
  STRANSMETA_BIT_27   = BIT_27; // Reserved, do not count on value
  STRANSMETA_BIT_28   = BIT_28; // Reserved, do not count on value
  STRANSMETA_BIT_29   = BIT_29; // Reserved, do not count on value
  STRANSMETA_BIT_30   = BIT_30; // Reserved, do not count on value
  STRANSMETA_BIT_31   = BIT_31; // Reserved, do not count on value

const
  IntelCacheDescription: array [0..50] of TCacheInfo = (
    (D: $00; Family:cfOther;                                                                      I: RsIntelCacheDescr00),
    (D: $01; Family:cfInstructionTLB;     Size:4;    WaysOfAssoc:4; Entries:32;                   I: RsIntelCacheDescr01), // Instruction TLB: 4 KByte Pages, 4-way set associative, 32 entries
    (D: $02; Family:cfInstructionTLB;     Size:4096; WaysOfAssoc:4; Entries:2;                    I: RsIntelCacheDescr02), // Instruction TLB: 4 MByte Pages, 4-way set associative, 2 entries
    (D: $03; Family:cfDataTLB;            Size:4;    WaysOfAssoc:4; Entries:64;                   I: RsIntelCacheDescr03), // Data TLB: 4KByte Pages, 4-way set associative, 64 entries
    (D: $04; Family:cfDataTLB;            Size:4096; WaysOfAssoc:4; Entries:8;                    I: RsIntelCacheDescr04), // Data TLB: 4MByte Pages, 4-way set associative, 8 entries
    (D: $06; Family:cfL1InstructionCache; Size:8;    WaysOfAssoc:4; LineSize:32;                  I: RsIntelCacheDescr06), // 1st-level instruction cache: 8 KBytes, 4-way set associative, 32 byte line size
    (D: $08; Family:cfL1InstructionCache; Size:16;   WaysOfAssoc:4; LineSize:32;                  I: RsIntelCacheDescr08), // 1st-level instruction cache: 16 KBytes, 4-way set associative, 32 byte line size
    (D: $0A; Family:cfL1DataCache;        Size:8;    WaysOfAssoc:2; LineSize:32;                  I: RsIntelCacheDescr0A), // 1st-level data cache: 8 KBytes, 2-way set associative, 32 byte line size
    (D: $0C; Family:cfL1DataCache;        Size:16;   WaysOfAssoc:4; LineSize:32;                  I: RsIntelCacheDescr0C), // 1st-level data cache: 16 KBytes, 4-way set associative, 32 byte line size
    (D: $22; Family:cfL3Cache;            Size:512;  WaysOfAssoc:4; LineSize:64; LinePerSector:2; I: RsIntelCacheDescr22), // 3rd-level cache: 512 KBytes, 4-way set associative, 64 byte line size, 2 lines per sector
    (D: $23; Family:cfL3Cache;            Size:1024; WaysOfAssoc:8; LineSize:64; LinePerSector:2; I: RsIntelCacheDescr23), // 3rd-level cache: 1 MBytes, 8-way set associative, 64 byte line size, 2 lines per sector
    (D: $25; Family:cfL3Cache;            Size:2048; WaysOfAssoc:8; LineSize:64; LinePerSector:2; I: RsIntelCacheDescr25), // 3rd-level cache: 2 MBytes, 8-way set associative, 64 byte line size, 2 lines per sector
    (D: $29; Family:cfL3Cache;            Size:4096; WaysOfAssoc:8; LineSize:64; LinePerSector:2; I: RsIntelCacheDescr29), // 3rd-level cache: 4M Bytes, 8-way set associative, 64 byte line size, 2 lines per sector
    (D: $2C; Family:cfL1DataCache;        Size:32;   WaysOfAssoc:8; LineSize:64;                  I: RsIntelCacheDescr2C), // 1st-level data cache: 32K Bytes, 8-way set associative, 64 byte line size
    (D: $30; Family:cfL1InstructionCache; Size:32;   WaysOfAssoc:8; LineSize:64;                  I: RsIntelCacheDescr30), // 1st-level instruction cache: 32K Bytes, 8-way set associative, 64 byte line size
    (D: $40; Family:cfOther;                                                                      I: RsIntelCacheDescr40), // No 2nd-level cache or, if processor contains a valid 2nd-level cache, no 3rd-level cache
    (D: $41; Family:cfL2Cache;            Size:128;  WaysOfAssoc:4; LineSize:32;                  I: RsIntelCacheDescr41), // 2nd-level cache: 128 KBytes, 4-way set associative, 32 byte line size
    (D: $42; Family:cfL2Cache;            Size:256;  WaysOfAssoc:4; LineSize:32;                  I: RsIntelCacheDescr42), // 2nd-level cache: 256 KBytes, 4-way set associative, 32 byte line size
    (D: $43; Family:cfL2Cache;            Size:512;  WaysOfAssoc:4; LineSize:32;                  I: RsIntelCacheDescr43), // 2nd-level cache: 512 KBytes, 4-way set associative, 32 byte line size
    (D: $44; Family:cfL2Cache;            Size:1024; WaysOfAssoc:4; LineSize:32;                  I: RsIntelCacheDescr44), // 2nd-level cache: 1 MByte, 4-way set associative, 32 byte line size
    (D: $45; Family:cfL2Cache;            Size:2048; WaysOfAssoc:4; LineSize:32;                  I: RsIntelCacheDescr45), // 2nd-level cache: 2 MByte, 4-way set associative, 32 byte line size
    (D: $50; Family:cfInstructionTLB;     Size:4096;                Entries:64;                   I: RsIntelCacheDescr50), // Instruction TLB: 4 KByte and 2-MByte or 4-MByte pages, 64 entries
    (D: $51; Family:cfInstructionTLB;     Size:4096;                Entries:128;                  I: RsIntelCacheDescr51), // Instruction TLB: 4 KByte and 2-MByte or 4-MByte pages, 128 entries
    (D: $52; Family:cfInstructionTLB;     Size:4096;                Entries:256;                  I: RsIntelCacheDescr52), // Instruction TLB: 4 KByte and 2-MByte or 4-MByte pages, 256 entries
    (D: $5B; Family:cfDataTLB;            Size:4096;                Entries:64;                   I: RsIntelCacheDescr5B), // Data TLB: 4 KByte and 4 MByte pages, 64 entries
    (D: $5C; Family:cfDataTLB;            Size:4096;                Entries:128;                  I: RsIntelCacheDescr5C), // Data TLB: 4 KByte and 4 MByte pages,128 entries
    (D: $5D; Family:cfDataTLB;            Size:4096;                Entries:256;                  I: RsIntelCacheDescr5D), // Data TLB: 4 KByte and 4 MByte pages,256 entries
    (D: $60; Family:cfL1DataCache;        Size:16;   WaysOfAssoc:8; LineSize:64;                  I: RsIntelCacheDescr60), // 1st-level data cache: 16 KByte, 8-way set associative, 64 byte line size
    (D: $66; Family:cfL1DataCache;        Size:8;    WaysOfAssoc:4; LineSize:64;                  I: RsIntelCacheDescr66), // 1st-level data cache: 8 KByte, 4-way set associative, 64 byte line size
    (D: $67; Family:cfL1DataCache;        Size:16;   WaysOfAssoc:4; LineSize:64;                  I: RsIntelCacheDescr67), // 1st-level data cache: 16 KByte, 4-way set associative, 64 byte line size
    (D: $68; Family:cfL1DataCache;        Size:32;   WaysOfAssoc:4; LineSize:64;                  I: RsIntelCacheDescr68), // 1st-level data cache: 32 KByte, 4-way set associative, 64 byte line size
    (D: $70; Family:cfTrace;              Size:12;   WaysOfAssoc:8;                               I: RsIntelCacheDescr70), // Trace cache: 12 K-µop, 8-way set associative
    (D: $71; Family:cfTrace;              Size:16;   WaysOfAssoc:8;                               I: RsIntelCacheDescr71), // Trace cache: 16 K-µop, 8-way set associative
    (D: $72; Family:cfTrace;              Size:32;   WaysOfAssoc:8;                               I: RsIntelCacheDescr72), // Trace cache: 32 K-µop, 8-way set associative
    (D: $78; Family:cfL2Cache;            Size:1024; WaysOfAssoc:4; LineSize:64;                  I: RsIntelCacheDescr78), // 2nd-level cache: 1 MByte, 4-way set associative, 64byte line size
    (D: $79; Family:cfL2Cache;            Size:128;  WaysOfAssoc:8; LineSize:64; LinePerSector:2; I: RsIntelCacheDescr79), // 2nd-level cache: 128 KByte, 8-way set associative, 64 byte line size, 2 lines per sector
    (D: $7A; Family:cfL2Cache;            Size:256;  WaysOfAssoc:8; LineSize:64; LinePerSector:2; I: RsIntelCacheDescr7A), // 2nd-level cache: 256 KByte, 8-way set associative, 64 byte line size, 2 lines per sector
    (D: $7B; Family:cfL2Cache;            Size:512;  WaysOfAssoc:8; LineSize:64; LinePerSector:2; I: RsIntelCacheDescr7B), // 2nd-level cache: 512 KByte, 8-way set associative, 64 byte line size, 2 lines per sector
    (D: $7C; Family:cfL2Cache;            Size:1024; WaysOfAssoc:8; LineSize:64; LinePerSector:2; I: RsIntelCacheDescr7C), // 2nd-level cache: 1 MByte, 8-way set associative, 64 byte line size, 2 lines per sector
    (D: $7D; Family:cfL2Cache;            Size:2048; WaysOfAssoc:8; LineSize:64;                  I: RsIntelCacheDescr7D), // 2nd-level cache: 2 MByte, 8-way set associative, 64byte line size
    (D: $7F; Family:cfL2Cache;            Size:512;  WaysOfAssoc:2; LineSize:64;                  I: RsIntelCacheDescr7F), // 2nd-level cache: 512 KByte, 2-way set associative, 64-byte line size
    (D: $82; Family:cfL2Cache;            Size:256;  WaysOfAssoc:8; LineSize:32;                  I: RsIntelCacheDescr82), // 2nd-level cache: 256 KByte, 8-way set associative, 32 byte line size
    (D: $83; Family:cfL2Cache;            Size:512;  WaysOfAssoc:8; LineSize:32;                  I: RsIntelCacheDescr83), // 2nd-level cache: 512 KByte, 8-way set associative, 32 byte line size
    (D: $84; Family:cfL2Cache;            Size:1024; WaysOfAssoc:8; LineSize:32;                  I: RsIntelCacheDescr84), // 2nd-level cache: 1 MByte, 8-way set associative, 32 byte line size
    (D: $85; Family:cfL2Cache;            Size:2048; WaysOfAssoc:8; LineSize:32;                  I: RsIntelCacheDescr85), // 2nd-level cache: 2 MByte, 8-way set associative, 32 byte line size
    (D: $86; Family:cfL2Cache;            Size:512;  WaysOfAssoc:4; LineSize:64;                  I: RsIntelCacheDescr86), // 2nd-level cache: 512 KByte, 4-way set associative, 64 byte line size
    (D: $87; Family:cfL2Cache;            Size:1024; WaysOfAssoc:8; LineSize:64;                  I: RsIntelCacheDescr87), // 2nd-level cache: 1 MByte, 8-way set associative, 64 byte line size
    (D: $B0; Family:cfInstructionTLB;     Size:4;    WaysOfAssoc:4; Entries:128;                  I: RsIntelCacheDescrB0), // Instruction TLB: 4 KByte Pages, 4-way set associative, 128 entries
    (D: $B3; Family:cfDataTLB;            Size:4;    WaysOfAssoc:4; Entries:128;                  I: RsIntelCacheDescrB3), // Data TLB: 4 KByte Pages, 4-way set associative, 128 entries
    (D: $F0; Family:cfOther;                                                                      I: RsIntelCacheDescrF0), // 64-Byte Prefetching
    (D: $F1; Family:cfOther;                                                                      I: RsIntelCacheDescrF1)  // 128-Byte Prefetching
  );

procedure GetCpuInfo(var CpuInfo: TCpuInfo);

function GetIntelCacheDescription(const D: Byte): string;
function RoundFrequency(const Frequency: Integer): Integer;
{$IFDEF MSWINDOWS}
function GetCPUSpeed(var CpuSpeed: TFreqInfo): Boolean;
{$ENDIF MSWINDOWS}
function CPUID: TCpuInfo;
function TestFDIVInstruction: Boolean;

// Memory Information
{$IFDEF MSWINDOWS}
function GetMaxAppAddress: Integer;
function GetMinAppAddress: Integer;
{$ENDIF MSWINDOWS}
function GetMemoryLoad: Byte;
function GetSwapFileSize: Integer;
function GetSwapFileUsage: Integer;
function GetTotalPhysicalMemory: Integer;
function GetFreePhysicalMemory: Integer;
{$IFDEF MSWINDOWS}
function GetTotalPageFileMemory: Integer;
function GetFreePageFileMemory: Integer;
function GetTotalVirtualMemory: Integer;
function GetFreeVirtualMemory: Integer;
{$ENDIF MSWINDOWS}

// Alloc granularity
procedure RoundToAllocGranularity64(var Value: Int64; Up: Boolean);
procedure RoundToAllocGranularityPtr(var Value: Pointer; Up: Boolean);

{$IFDEF MSWINDOWS}
// Keyboard Information
function GetKeyState(const VirtualKey: Cardinal): Boolean;
function GetNumLockKeyState: Boolean;
function GetScrollLockKeyState: Boolean;
function GetCapsLockKeyState: Boolean;

// Windows 95/98/Me system resources information
type
  TFreeSysResKind = (rtSystem, rtGdi, rtUser);
  TFreeSystemResources = record
    SystemRes: Integer;
    GdiRes: Integer;
    UserRes: Integer;
  end;

function IsSystemResourcesMeterPresent: Boolean;

function GetFreeSystemResources(const ResourceType: TFreeSysResKind): Integer; overload;
function GetFreeSystemResources: TFreeSystemResources; overload;
{$ENDIF MSWINDOWS}

// Public global variables
var
  ProcessorCount: Cardinal = 0;
  AllocGranularity: Cardinal = 0;
  PageSize: Cardinal = 0;
{$ENDIF ~CLR}

implementation

uses
  SysUtils,
  {$IFNDEF CLR}
  {$IFDEF MSWINDOWS}
  Messages, Winsock, Snmp,
  {$IFDEF FPC}
  ActiveX, JwaTlHelp32, JwaPsApi,
  {$ELSE}
  TLHelp32, PsApi,
  JclShell,
  {$ENDIF FPC}
  JclRegistry, JclWin32,
  {$ENDIF MSWINDOWS}
  Jcl8087, JclIniFiles,
  {$ENDIF ~CLR}
  JclBase, JclFileUtils, JclStrings;

{$IFDEF FPC}
{$I JclSysInfo.fpc}
{$ENDIF FPC}

//=== Environment ============================================================

function DelEnvironmentVar(const Name: string): Boolean;
begin
  {$IFDEF CLR}
  System.Environment.GetEnvironmentVariables.Remove(Name);
  Result := True;
  {$ELSE}
  {$IFDEF UNIX}
  UnSetEnv(PChar(Name));
  Result := True;
  {$ENDIF UNIX}
  {$IFDEF MSWINDOWS}
  Result := SetEnvironmentVariable(PChar(Name), nil);
  {$ENDIF MSWINDOWS}
  {$ENDIF CLR}
end;

function ExpandEnvironmentVar(var Value: string): Boolean;
{$IFDEF CLR}
begin
  Value := System.Environment.ExpandEnvironmentVariables(Value);
  Result := True;
end;
{$ELSE}
{$IFDEF UNIX}
begin
  Result := True;
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
var
  R: Integer;
  Expanded: string;
begin
  SetLength(Expanded, 1);
  R := ExpandEnvironmentStrings(PChar(Value), PChar(Expanded), 0);
  SetLength(Expanded, R);
  Result := ExpandEnvironmentStrings(PChar(Value), PChar(Expanded), R) <> 0;
  if Result then
  begin
    StrResetLength(Expanded);
    Value := Expanded;
  end;
end;
{$ENDIF MSWINDOWS}
{$ENDIF CLR}

{$IFDEF UNIX}

function GetEnvironmentVar(const Name: string; var Value: string): Boolean;
begin
  Value := getenv(PChar(Name));
  Result := Value <> '';
end;

function GetEnvironmentVar(const Name: string; var Value: string; Expand: Boolean): Boolean;
begin
  Result := GetEnvironmentVar(Name, Value); // Expand is there just for x-platform compatibility
end;

{$ENDIF UNIX}

{$IFDEF MSWINDOWS}

function GetEnvironmentVar(const Name: string; var Value: string): Boolean;
begin
  {$IFDEF CLR}
  Value := System.Environment.GetEnvironmentVariable(Name);
  Result := TObject(Value) <> nil;
  {$ELSE}
  Result := GetEnvironmentVar(Name, Value, True);
  {$ENDIF CLR}
end;

function GetEnvironmentVar(const Name: string; var Value: string; Expand: Boolean): Boolean;
{$IFDEF CLR}
begin
  Result := GetEnvironmentVar(Name, Value);
  if Expand then
    ExpandEnvironmentVar(Value);
end;
{$ELSE}
var
  R: DWORD;
begin
  R := Windows.GetEnvironmentVariable(PChar(Name), nil, 0);
  SetLength(Value, R);
  R := Windows.GetEnvironmentVariable(PChar(Name), PChar(Value), R);
  Result := R <> 0;
  if not Result then
    Value := ''
  else
  begin
    SetLength(Value, R);
    if Expand then
      ExpandEnvironmentVar(Value);
  end;
end;
{$ENDIF CLR}

{$ENDIF MSWINDOWS}

{$IFDEF KYLIX}
function GetEnvironmentVars(const Vars: TStrings): Boolean;
var
  P: PPChar;
begin
  Vars.BeginUpdate;
  try
    Vars.Clear;
    P := System.envp;
    Result := P <> nil;
    while (P <> nil) and (P^ <> nil) do
    begin
      Vars.Add(P^);
      Inc(P);
    end;
  finally
    Vars.EndUpdate;
  end;
end;
{$ENDIF KYLIX}
{$IFDEF UNIX}
function GetEnvironmentVars(const Vars: TStrings; Expand: Boolean): Boolean;
begin
  Result := GetEnvironmentVars(Vars); // Expand is there just for x-platform compatibility
end;
{$ENDIF UNIX}

{$IFDEF MSWINDOWS}

function GetEnvironmentVars(const Vars: TStrings): Boolean;
begin
  Result := GetEnvironmentVars(Vars, True);
end;

function GetEnvironmentVars(const Vars: TStrings; Expand: Boolean): Boolean;
{$IFDEF CLR}
var
  Dic: IDictionaryEnumerator;
begin
  Vars.BeginUpdate;
  try
    Vars.Clear;
    for Dic in System.Environment.GetEnvironmentVariables do
      Vars.Add(string(Dic.Key) + '=' + string(Dic.Value));
  finally
    Vars.EndUpdate;
  end;
  Result := True;
end;
{$ELSE}
var
  Raw: PChar;
  Expanded: string;
  I: Integer;
begin
  Vars.BeginUpdate;
  try
    Vars.Clear;
    Raw := GetEnvironmentStrings;
    try
      MultiSzToStrings(Vars, Raw);
      Result := True;
    finally
      FreeEnvironmentStrings(Raw);
    end;
    if Expand then
    begin
      for I := 0 to Vars.Count - 1 do
      begin
        Expanded := Vars[I];
        if ExpandEnvironmentVar(Expanded) then
          Vars[I] := Expanded;
      end;
    end;
  finally
    Vars.EndUpdate;
  end;
end;
{$ENDIF CLR}

{$ENDIF MSWINDOWS}

function SetEnvironmentVar(const Name, Value: string): Boolean;
begin
  {$IFDEF CLR}
  if System.Environment.GetEnvironmentVariables.Contains(Name) then
    System.Environment.GetEnvironmentVariables.Item[Name] := Value
  else
    System.Environment.GetEnvironmentVariables.Add(Name, Value);
  Result := True;
  {$ELSE}
  {$IFDEF UNIX}
  SetEnv(PChar(Name), PChar(Value), 1);
  Result := True;
  {$ENDIF UNIX}
  {$IFDEF MSWINDOWS}
  Result := SetEnvironmentVariable(PChar(Name), PChar(Value));
  {$ENDIF MSWINDOWS}
  {$ENDIF CLR}
end;

{$IFNDEF CLR}
{$IFDEF MSWINDOWS}

function CreateEnvironmentBlock(const Options: TEnvironmentOptions; const AdditionalVars: TStrings): PChar;
const
  RegLocalEnvironment = 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment';
  RegUserEnvironment = '\Environment\';
var
  KeyNames, TempList: TStrings;
  Temp, Name, Value: string;
  I: Integer;
begin
  TempList := TStringList.Create;
  try
    // add additional environment variables
    if eoAdditional in Options then
      for I := 0 to AdditionalVars.Count - 1 do
      begin
        Temp := AdditionalVars[I];
        ExpandEnvironmentVar(Temp);
        TempList.Add(Temp);
      end;
    // get environment strings from local machine
    if eoLocalMachine in Options then
    begin
      KeyNames := TStringList.Create;
      try
        if RegGetValueNames(HKEY_LOCAL_MACHINE, RegLocalEnvironment, KeyNames) then
        begin
          for I := 0 to KeyNames.Count - 1 do
          begin
            Name := KeyNames[I];
            Value := RegReadString(HKEY_LOCAL_MACHINE, RegLocalEnvironment, Name);
            ExpandEnvironmentVar(Value);
            TempList.Add(Name + '=' + Value);
          end;
        end;
      finally
        FreeAndNil(KeyNames);
      end;
    end;
    // get environment strings from current user
    if eoCurrentUser in Options then
    begin
      KeyNames := TStringLIst.Create;
      try
        if RegGetValueNames(HKEY_CURRENT_USER, RegUserEnvironment, KeyNames) then
        begin
          for I := 0 to KeyNames.Count - 1 do
          begin
            Name := KeyNames[I];
            Value := RegReadString(HKEY_CURRENT_USER, RegUserEnvironment, Name);
            ExpandEnvironmentVar(Value);
            TempList.Add(Name + '=' + Value);
          end;
        end;
      finally
        KeyNames.Free;
      end;
    end;
    // transform stringlist into multi-PChar
    StringsToMultiSz(Result, TempList);
  finally
    FreeAndNil(TempList);
  end;
end;

// frees an environment block allocated by CreateEnvironmentBlock and
// sets Env to nil

procedure DestroyEnvironmentBlock(var Env: PChar);
begin
  FreeMultiSz(Env);
end;

procedure SetGlobalEnvironmentVariable(VariableName, VariableContent: string);
const
  cEnvironment = 'Environment';
begin
  if VariableName = '' then
    Exit;
  if VariableContent = '' then
  begin
    RegDeleteEntry(HKEY_CURRENT_USER, cEnvironment, VariableName);
    SetEnvironmentVariable(PChar(VariableName), nil);
  end
  else
  begin
    RegWriteAnsiString(HKEY_CURRENT_USER, cEnvironment, VariableName, VariableContent);
    SetEnvironmentVariable(PChar(VariableName), PChar(VariableContent));
  end;
  SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, LPARAM(PChar(cEnvironment)));
end;

//=== Common Folders =========================================================

// Utility function which returns the Windows independent CurrentVersion key
// inside HKEY_LOCAL_MACHINE

const
  HKLM_CURRENT_VERSION_WINDOWS = 'SOFTWARE\Microsoft\Windows\CurrentVersion';
  HKLM_CURRENT_VERSION_NT      = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion';

function REG_CURRENT_VERSION: string;
begin
  if IsWinNT then
    Result := HKLM_CURRENT_VERSION_NT
  else
    Result := HKLM_CURRENT_VERSION_WINDOWS;
end;

{ TODO : Check for documented solution }
function GetCommonFilesFolder: string;
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, HKLM_CURRENT_VERSION_WINDOWS,
    'CommonFilesDir', '');
end;

{$ENDIF MSWINDOWS}
{$ENDIF ~CLR}

function GetCurrentFolder: string;
{$IFDEF CLR}
begin
  Result := System.Environment.CurrentDirectory;
end;
{$ELSE}
{$IFDEF UNIX}
const
  InitialSize = 64;
var
  Size: Integer;
begin
  Size := InitialSize;
  while True do
  begin
    SetLength(Result, Size);
    if getcwd(PChar(Result), Size) <> nil then
    begin
      StrResetLength(Result);
      Exit;
    end;
    if GetLastError <> ERANGE then
      RaiseLastOSError;
    Size := Size * 2;
  end;
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
var
  Required: Cardinal;
begin
  Result := '';
  Required := GetCurrentDirectory(0, nil);
  if Required <> 0 then
  begin
    SetLength(Result, Required);
    GetCurrentDirectory(Required, PChar(Result));
    StrResetLength(Result);
  end;
end;
{$ENDIF MSWINDOWS}
{$ENDIF CLR}

{$IFDEF MSWINDOWS}
{ TODO : Check for documented solution }
function GetProgramFilesFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles);
  {$ELSE}
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, HKLM_CURRENT_VERSION_WINDOWS, 'ProgramFilesDir', '');
  {$ENDIF CLR}
end;

{$IFNDEF CLR}
{ TODO : Check for documented solution }
function GetWindowsFolder: string;
var
  Required: Cardinal;
begin
  Result := '';
  Required := GetWindowsDirectory(nil, 0);
  if Required <> 0 then
  begin
    SetLength(Result, Required);
    GetWindowsDirectory(PChar(Result), Required);
    StrResetLength(Result);
  end;
end;
{$ENDIF ~CLR}

{ TODO : Check for documented solution }
function GetWindowsSystemFolder: string;
{$IFDEF CLR}
begin
  Result := System.Environment.SystemDirectory;
end;
{$ELSE}
var
  Required: Cardinal;
begin
  Result := '';
  Required := GetSystemDirectory(nil, 0);
  if Required <> 0 then
  begin
    SetLength(Result, Required);
    GetSystemDirectory(PChar(Result), Required);
    StrResetLength(Result);
  end;
end;
{$ENDIF CLR}

function GetWindowsTempFolder: string;
{$IFDEF CLR}
begin
  Result := Path.GetTempPath;
end;
{$ELSE}
var
  Required: Cardinal;
begin
  Result := '';
  Required := GetTempPath(0, nil);
  if Required <> 0 then
  begin
    SetLength(Result, Required);
    GetTempPath(Required, PChar(Result));
    StrResetLength(Result);
    Result := PathRemoveSeparator(Result);
  end;
end;
{$ENDIF CLR}

function GetDesktopFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.Desktop);
  {$ELSE}
  Result := GetSpecialFolderLocation(CSIDL_DESKTOP);
  {$ENDIF CLR}
end;

{ TODO : Check GetProgramsFolder = GetProgramFilesFolder }
function GetProgramsFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.Programs);
  {$ELSE}
  Result := GetSpecialFolderLocation(CSIDL_PROGRAMS);
  {$ENDIF CLR}
end;

{$ENDIF MSWINDOWS}
function GetPersonalFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.Personal);
  {$ELSE}
  {$IFDEF UNIX}
  Result := GetEnvironmentVariable('HOME');
  {$ENDIF UNIX}
  {$IFDEF MSWINDOWS}
  Result := GetSpecialFolderLocation(CSIDL_PERSONAL);
  {$ENDIF MSWINDOWS}
  {$ENDIF CLR}
end;

{$IFDEF MSWINDOWS}
function GetFavoritesFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.Favorites);
  {$ELSE}
  Result := GetSpecialFolderLocation(CSIDL_FAVORITES);
  {$ENDIF CLR}
end;

function GetStartupFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.Startup);
  {$ELSE}
  Result := GetSpecialFolderLocation(CSIDL_STARTUP);
  {$ENDIF CLR}
end;

function GetRecentFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.Recent);
  {$ELSE}
  Result := GetSpecialFolderLocation(CSIDL_RECENT);
  {$ENDIF CLR}
end;

function GetSendToFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.SendTo);
  {$ELSE}
  Result := GetSpecialFolderLocation(CSIDL_SENDTO);
  {$ENDIF CLR}
end;

function GetStartmenuFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.StartMenu);
  {$ELSE}
  Result := GetSpecialFolderLocation(CSIDL_STARTMENU);
  {$ENDIF CLR}
end;

function GetDesktopDirectoryFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.DesktopDirectory);
  {$ELSE}
  Result := GetSpecialFolderLocation(CSIDL_DESKTOPDIRECTORY);
  {$ENDIF CLR}
end;

{$IFNDEF CLR}
function GetNethoodFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_NETHOOD);
end;
{$ENDIF ~CLR}

{$IFNDEF CLR}
function GetFontsFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_FONTS);
end;

function GetCommonStartmenuFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_COMMON_STARTMENU);
end;
{$ENDIF ~CLR}

function GetCommonProgramsFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.CommonProgramFiles);
  {$ELSE}
  Result := GetSpecialFolderLocation(CSIDL_COMMON_PROGRAMS);
  {$ENDIF CLR}
end;

{$IFNDEF CLR}
function GetCommonStartupFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_COMMON_STARTUP);
end;
{$ENDIF ~CLR}

function GetCommonDesktopdirectoryFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.DesktopDirectory);
  {$ELSE}
  Result := GetSpecialFolderLocation(CSIDL_COMMON_DESKTOPDIRECTORY);
  {$ENDIF CLR}
end;

function GetCommonAppdataFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.CommonApplicationData);
  {$ELSE}
  Result := GetSpecialFolderLocation(CSIDL_COMMON_APPDATA);
  {$ENDIF CLR}
end;

function GetAppdataFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData);
  {$ELSE}
  Result := GetSpecialFolderLocation(CSIDL_APPDATA);
  {$ENDIF CLR}
end;

{$IFNDEF CLR}
function GetPrinthoodFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_PRINTHOOD);
end;
{$ENDIF ~CLR}

function GetCommonFavoritesFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.Favorites);
  {$ELSE}
  Result := GetSpecialFolderLocation(CSIDL_COMMON_FAVORITES);
  {$ENDIF CLR}
end;

function GetTemplatesFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.Templates);
  {$ELSE}
  Result := GetSpecialFolderLocation(CSIDL_TEMPLATES);
  {$ENDIF CLR}
end;

function GetInternetCacheFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.InternetCache);
  {$ELSE}
  Result := GetSpecialFolderLocation(CSIDL_INTERNET_CACHE);
  {$ENDIF CLR}
end;

function GetCookiesFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.Cookies);
  {$ELSE}
  Result := GetSpecialFolderLocation(CSIDL_COOKIES);
  {$ENDIF CLR}
end;

function GetHistoryFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.History);
  {$ELSE}
  Result := GetSpecialFolderLocation(CSIDL_HISTORY);
  {$ENDIF CLR}
end;

{$IFNDEF CLR}
function GetProfileFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_PROFILE);
end;
{$ENDIF ~CLR}

// the following special folders are pure virtual and cannot be
// mapped to a directory path:
// CSIDL_INTERNET
// CSIDL_CONTROLS
// CSIDL_PRINTERS
// CSIDL_BITBUCKET
// CSIDL_DRIVES
// CSIDL_NETWORK
// CSIDL_ALTSTARTUP
// CSIDL_COMMON_ALTSTARTUP

{$IFNDEF CLR}
// Identification
type
  TVolumeInfoKind = (vikName, vikSerial, vikFileSystem);

function GetVolumeInfoHelper(const Drive: string; InfoKind: TVolumeInfoKind): string;
var
  VolumeSerialNumber: DWORD;
  MaximumComponentLength: DWORD;
  Flags: DWORD;
  Name: array [0..MAX_PATH] of Char;
  FileSystem: array [0..15] of Char;
  ErrorMode: Cardinal;
  DriveStr: string;
begin
  { TODO : Change to RootPath }
  { TODO : Perform better checking of Drive param or document that no checking
    is performed. RM Suggested:
    DriveStr := Drive;
    if (Length(Drive) < 2) or (Drive[2] <> ':') then
      DriveStr := GetCurrentFolder;
    DriveStr  := DriveStr[1] + ':\'; }
  Result := '';
  DriveStr := Drive + ':\';
  ErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    if GetVolumeInformation(PChar(DriveStr), Name, SizeOf(Name), @VolumeSerialNumber,
      MaximumComponentLength, Flags, FileSystem, SizeOf(FileSystem)) then
    case InfoKind of
      vikName:
        Result := StrPas(Name);
      vikSerial:
        begin
          Result := IntToHex(HiWord(VolumeSerialNumber), 4) + '-' +
          IntToHex(LoWord(VolumeSerialNumber), 4);
        end;
      vikFileSystem:
        Result := StrPas(FileSystem);
    end;
  finally
    SetErrorMode(ErrorMode);
  end;
end;

function GetVolumeName(const Drive: string): string;
begin
  Result := GetVolumeInfoHelper(Drive, vikName);
end;

function GetVolumeSerialNumber(const Drive: string): string;
begin
  Result := GetVolumeInfoHelper(Drive, vikSerial);
end;

function GetVolumeFileSystem(const Drive: string): string;
begin
  Result := GetVolumeInfoHelper(Drive, vikFileSystem);
end;

{ TODO -cHelp : Donator (incl. TFileSystemFlag[s]): Robert Rossmair }

function GetVolumeFileSystemFlags(const Volume: string): TFileSystemFlags;
var
  MaximumComponentLength, Flags: Cardinal;
  Flag: TFileSystemFlag;
begin
  if not GetVolumeInformation(PChar(PathAddSeparator(Volume)), nil, 0, nil,
    MaximumComponentLength, Flags, nil, 0) then
    RaiseLastOSError;
  Result := [];
  for Flag := Low(TFileSystemFlag) to High(TFileSystemFlag) do
    if (Flags and Ord(Flag)) <> 0 then
      Include(Result, Flag);
end;

{$ENDIF ~CLR}
{$ENDIF MSWINDOWS}

{ TODO -cDoc: Contributor: twm }

function GetIPAddress(const HostName: string): string;
{$IFDEF CLR}
var
  Host: IPHostEntry;
begin
  Host := System.Net.Dns.Resolve(HostName);
  if (Host <> nil) and (Length(Host.AddressList) > 0) then
    Result := Host.AddressList[0].ToString()
  else
    Result := '';
end;
{$ELSE}
var
  {$IFDEF MSWINDOWS}
  R: Integer;
  WSAData: TWSAData;
  {$ENDIF MSWINDOWS}
  HostEnt: PHostEnt;
  Host: string;
  SockAddr: TSockAddrIn;
begin
  Result := '';
  {$IFDEF MSWINDOWS}
  R := WSAStartup(MakeWord(1, 1), WSAData);
  if R = 0 then
    try
  {$ENDIF MSWINDOWS}
      Host := HostName;
      if Host = '' then
      begin
        SetLength(Host, MAX_PATH);
        GetHostName(PChar(Host), MAX_PATH);
      end;
      HostEnt := GetHostByName(PChar(Host));
      if HostEnt <> nil then
      begin
        SockAddr.sin_addr.S_addr := Longint(PLongint(HostEnt^.h_addr_list^)^);
        Result := inet_ntoa(SockAddr.sin_addr);
      end;
    {$IFDEF MSWINDOWS}
    finally
      WSACleanup;
    end;
    {$ENDIF MSWINDOWS}
end;
{$ENDIF CLR}

{$IFDEF UNIX}

{ TODO -cDoc: Donator: twm, Contributor rrossmair }

// Returns all IP addresses of the local machine in the form
// <interface>=<IP-Address> (which allows for access to the interface names
// by means of Results.Names and the addresses through Results.Values)
//
// Example:
//
// lo=127.0.0.1
// eth0=10.10.10.1
// ppp0=217.82.187.130
//
// note that this will append to Results!
//

procedure GetIpAddresses(Results: TStrings);
var
  Sock: Integer;
  IfReq: TIfReq;
  SockAddrPtr: PSockAddrIn;
  ListSave, IfList: PIfNameIndex;
begin
  //need a socket for ioctl()
  Sock := socket(AF_INET, SOCK_STREAM, 0);
  if Sock < 0 then
    RaiseLastOSError;

  try
    //returns pointer to dynamically allocated list of structs
    ListSave := if_nameindex();
    try
      IfList := ListSave;
      //walk thru the array returned and query for each
      //interface's address
      while IfList^.if_index <> 0 do
      begin
        //copy in the interface name to look up address of
        strncpy(IfReq.ifrn_name, IfList^.if_name, IFNAMSIZ);
        //get the address for this interface
        if ioctl(Sock, SIOCGIFADDR, @IfReq) <> 0 then
          RaiseLastOSError;
        //print out the address
        SockAddrPtr := PSockAddrIn(@IfReq.ifru_addr);
        Results.Add(Format('%s=%s', [IfReq.ifrn_name, inet_ntoa(SockAddrPtr^.sin_addr)]));
        Inc(IfList);
      end;
    finally
      //free the dynamic memory kernel allocated for us
      if_freenameindex(ListSave);
    end;
  finally
    Libc.__close(Sock)
  end;
end;

{$ENDIF UNIX}

function GetLocalComputerName: string;
{$IFDEF CLR}
begin
  Result := System.Environment.MachineName;
end;
{$ELSE}
// (rom) UNIX or LINUX?
{$IFDEF LINUX}
var
  MachineInfo: utsname;
begin
  uname(MachineInfo);
  Result := MachineInfo.nodename;
end;
{$ENDIF LINUX}
{$IFDEF MSWINDOWS}
var
  Count: DWORD;
begin
  Count := MAX_COMPUTERNAME_LENGTH + 1;
  // set buffer size to MAX_COMPUTERNAME_LENGTH + 2 characters for safety
  { TODO : Win2k solution }
  SetLength(Result, Count);
  if GetComputerName(PChar(Result), Count) then
    StrResetLength(Result)
  else
    Result := '';
end;
{$ENDIF MSWINDOWS}
{$ENDIF CLR}

{$IFNDEF CLR}

function GetLocalUserName: string;
{$IFDEF UNIX}
begin
  Result := GetEnv('USER');
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
var
  Count: DWORD;
begin
  Count := 256 + 1; // UNLEN + 1
  // set buffer size to 256 + 2 characters
  { TODO : Win2k solution }
  SetLength(Result, Count);
  if GetUserName(PChar(Result), Count) then
    StrResetLength(Result)
  else
    Result := '';
end;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
function GetRegisteredCompany: string;
begin
  { TODO : check for MSDN documentation }
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, REG_CURRENT_VERSION, 'RegisteredOrganization', '');
end;

function GetRegisteredOwner: string;
begin
  { TODO : check for MSDN documentation }
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, REG_CURRENT_VERSION, 'RegisteredOwner', '');
end;

{ TODO: Check supported platforms, maybe complete rewrite }

function GetUserDomainName(const CurUser: string): string;
var
  Count1, Count2: DWORD;
  Sd: PSID; // PSecurityDescriptor; // FPC requires PSID
  Snu: SID_Name_Use;
begin
  Count1 := 0;
  Count2 := 0;
  Sd := nil;
  Snu := SIDTypeUser;
  LookUpAccountName(nil, PChar(CurUser), Sd, Count1, PChar(Result), Count2, Snu);
  // set buffer size to Count2 + 2 characters for safety
  SetLength(Result, Count2 + 1);
  Sd := AllocMem(Count1);
  try
    if LookUpAccountName(nil, PChar(CurUser), Sd, Count1, PChar(Result), Count2, Snu) then
      StrResetLength(Result)
    else
      Result := EmptyStr;
  finally
    FreeMem(Sd);
  end;
end;

{$ENDIF MSWINDOWS}
function GetDomainName: string;
{$IFDEF UNIX}
var
  MachineInfo: utsname;
begin
  uname(MachineInfo);
  Result := MachineInfo.domainname;
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
begin
  Result := GetUserDomainName(GetLocalUserName);
end;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
// Reference: How to Obtain BIOS Information from the Registry
// http://support.microsoft.com/default.aspx?scid=kb;en-us;q195268

function GetBIOSName: string;
const
  Win9xBIOSInfoKey = 'Enum\Root\*PNP0C01\0000';
begin
  if IsWinNT then
    Result := ''
  else
    Result := RegReadStringDef(HKEY_LOCAL_MACHINE, Win9xBIOSInfoKey, 'BIOSName', '');
end;

function GetBIOSCopyright: string;
const
  ADR_BIOSCOPYRIGHT = $FE091;
begin
  Result := '';
  if not IsWinNT and not IsBadReadPtr(Pointer(ADR_BIOSCOPYRIGHT), 2) then
  try
    Result := PChar(ADR_BIOSCOPYRIGHT);
  except
    Result := '';
  end;
end;

function GetBIOSExtendedInfo: string;
const
  ADR_BIOSEXTENDEDINFO = $FEC71;
begin
  Result := '';
  if not IsWinNT and not IsBadReadPtr(Pointer(ADR_BIOSEXTENDEDINFO), 2) then
  try
    Result := PChar(ADR_BIOSEXTENDEDINFO);
  except
    Result := '';
  end;
end;

// Reference: How to Obtain BIOS Information from the Registry
// http://support.microsoft.com/default.aspx?scid=kb;en-us;q195268

{ TODO : the date string can be e.g. 00/00/00 }
function GetBIOSDate: TDateTime;
const
  WinNT_REG_PATH = 'HARDWARE\DESCRIPTION\System';
  WinNT_REG_KEY  = 'SystemBiosDate';
  Win9x_REG_PATH = 'Enum\Root\*PNP0C01\0000';
  Win9x_REG_KEY  = 'BiosDate';
var
  RegStr: string;
  {$IFDEF RTL150_UP}
  FormatSettings: TFormatSettings;
  {$ELSE RTL150_UP}
  RegFormat: string;
  RegSeparator: Char;
  {$ENDIF RTL150_UP}
begin
  if IsWinNT then
    RegStr := RegReadString(HKEY_LOCAL_MACHINE, WinNT_REG_PATH, WinNT_REG_KEY)
  else
    RegStr := RegReadString(HKEY_LOCAL_MACHINE, Win9x_REG_PATH, Win9x_REG_KEY);
  {$IFDEF RTL150_UP}
  FillChar(FormatSettings, SizeOf(FormatSettings), 0);
  FormatSettings.DateSeparator := '/';
  FormatSettings.ShortDateFormat := 'm/d/y';
  if not TryStrToDate(RegStr, Result, FormatSettings) then
  begin
    FormatSettings.ShortDateFormat := 'y/m/d';
    if not TryStrToDate(RegStr, Result, FormatSettings) then
      Result := 0;
  end;
  {$ELSE RTL150_UP}
  Result := 0;
  { TODO : change to a threadsafe solution }
  RegFormat := ShortDateFormat;
  RegSeparator := DateSeparator;
  try
    DateSeparator := '/';
    try
      ShortDateFormat := 'm/d/y';
      Result := StrToDate(RegStr);
    except
      try
        ShortDateFormat := 'y/m/d';
        Result := StrToDate(RegStr);
      except
      end;
    end;
  finally
    ShortDateFormat := RegFormat;
    DateSeparator := RegSeparator;
  end;
  {$ENDIF RTL150_UP}
end;

{$ENDIF MSWINDOWS}

//=== Processes, Tasks and Modules ===========================================

{$IFDEF UNIX}
const
  CommLen = 16;  // synchronize with size of comm in struct task_struct in
                 //     /usr/include/linux/sched.h
  SProcDirectory = '/proc';

function RunningProcessesList(const List: TStrings; FullPath: Boolean): Boolean;
var
  ProcDir: PDirectoryStream;
  PtrDirEnt: PDirEnt;
  Scratch: TDirEnt;
  ProcID: __pid_t;
  E: Integer;
  FileName: string;
  F: PIOFile;
begin
  Result := False;
  ProcDir := opendir(SProcDirectory);
  if ProcDir <> nil then
  begin
    PtrDirEnt := nil;
    if readdir_r(ProcDir, @Scratch, PtrDirEnt) <> 0 then
      Exit;
    List.BeginUpdate;
    try
      while PtrDirEnt <> nil do
      begin
        Val(PtrDirEnt^.d_name, ProcID, E);
        if E = 0 then // name was process id
        begin
          FileName := '';

          if FullPath then
            FileName := SymbolicLinkTarget(Format('/proc/%s/exe', [PtrDirEnt^.d_name]));

          if FileName = '' then // usually due to insufficient access rights
          begin
            // read stat
            FileName := Format('/proc/%s/stat', [PtrDirEnt^.d_name]);
            F := fopen(PChar(FileName), 'r');
            if F = nil then
              raise EJclError.CreateResFmt(@RsInvalidProcessID, [ProcID]);
            try
              SetLength(FileName, CommLen);
              if fscanf(F, PChar(Format('%%*d (%%%d[^)])', [CommLen])), PChar(FileName)) <> 1 then
                RaiseLastOSError;
              StrResetLength(FileName);
            finally
              fclose(F);
            end;
          end;

          List.AddObject(FileName, Pointer(ProcID));
        end;
        if readdir_r(ProcDir, @Scratch, PtrDirEnt) <> 0 then
          Break;
      end;
    finally
      List.EndUpdate;
    end;
  end;
end;

{$ENDIF UNIX}
{$ENDIF ~CLR}

{$IFDEF MSWINDOWS}

function RunningProcessesList(const List: TStrings; FullPath: Boolean): Boolean;
{$IFDEF CLR}
var
  Processes: array of Process;
  I: Integer;
begin
  Result := True;
  Processes := Process.GetProcesses;
  for I := 0 to High(Processes) do
    if FullPath then
      List.Add(Processes[I].MainModule.FileName)
    else
      List.Add(Processes[I].MainModule.ModuleName);
end;
{$ELSE}

  // This function always returns an empty string on Win9x
  function ProcessFileName(PID: DWORD): string;
  var
    Handle: THandle;
  begin
    Result := '';
    Handle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, PID);
    if Handle <> 0 then
    try
      SetLength(Result, MAX_PATH);
      if FullPath then
      begin
        if GetModuleFileNameEx(Handle, 0, PChar(Result), MAX_PATH) > 0 then
          StrResetLength(Result)
        else
          Result := '';
      end
      else
      begin
        if GetModuleBaseNameA(Handle, 0, PChar(Result), MAX_PATH) > 0 then
          StrResetLength(Result)
        else
          Result := '';
      end;
    finally
      CloseHandle(Handle);
    end;
  end;

  { TODO: Check return value of CreateToolhelp32Snapshot on Windows NT (0?) }
  function BuildListTH: Boolean;
  var
    SnapProcHandle: THandle;
    ProcEntry: TProcessEntry32;
    NextProc: Boolean;
    FileName: string;
  begin
    SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    Result := (SnapProcHandle <> INVALID_HANDLE_VALUE);
    if Result then
    try
      ProcEntry.dwSize := SizeOf(ProcEntry);
      NextProc := Process32First(SnapProcHandle, ProcEntry);
      while NextProc do
      begin
        if ProcEntry.th32ProcessID = 0 then
        begin
          // PID 0 is always the "System Idle Process" but this name cannot be
          // retrieved from the system and has to be fabricated.
          FileName := RsSystemIdleProcess;
        end
        else
        begin
          if IsWin2k or IsWinXP or IsWin2003 then
          begin
            FileName := ProcessFileName(ProcEntry.th32ProcessID);
            if FileName = '' then
              FileName := ProcEntry.szExeFile;
          end
          else
          begin
            FileName := ProcEntry.szExeFile;
            if not FullPath then
              FileName := ExtractFileName(FileName);
          end;
        end;
        List.AddObject(FileName, Pointer(ProcEntry.th32ProcessID));
        NextProc := Process32Next(SnapProcHandle, ProcEntry);
      end;
    finally
      CloseHandle(SnapProcHandle);
    end;
  end;

  function BuildListPS: Boolean;
  var
    PIDs: array [0..1024] of DWORD;
    Needed: DWORD;
    I: Integer;
    FileName: string;
  begin
    Result := EnumProcesses(@PIDs, SizeOf(PIDs), Needed);
    if Result then
    begin
      for I := 0 to (Needed div SizeOf(DWORD)) - 1 do
      begin
        case PIDs[I] of
          0:
            // PID 0 is always the "System Idle Process" but this name cannot be
            // retrieved from the system and has to be fabricated.
            FileName := RsSystemIdleProcess;
          2:
            // On NT 4 PID 2 is the "System Process" but this name cannot be
            // retrieved from the system and has to be fabricated.
            if IsWinNT4 then
              FileName := RsSystemProcess
            else
              FileName := ProcessFileName(PIDs[I]);
          8:
            // On Win2K PID 8 is the "System Process" but this name cannot be
            // retrieved from the system and has to be fabricated.
            if IsWin2k or IsWinXP then
              FileName := RsSystemProcess
            else
              FileName := ProcessFileName(PIDs[I]);
        else
          FileName := ProcessFileName(PIDs[I]);
        end;
        if FileName <> '' then
          List.AddObject(FileName, Pointer(PIDs[I]));
      end;
    end;
  end;

begin
  { TODO : safer solution? }
  List.BeginUpdate;
  try
    if GetWindowsVersion in [wvWinNT31, wvWinNT35, wvWinNT351, wvWinNT4] then
      Result := BuildListPS
    else
      Result := BuildListTH;
  finally
    List.EndUpdate;
  end;
end;
{$ENDIF CLR}

{$IFNDEF CLR}

{ TODO Windows 9x ? }

function LoadedModulesList(const List: TStrings; ProcessID: DWORD; HandlesOnly: Boolean): Boolean;

  procedure AddToList(ProcessHandle: THandle; Module: HMODULE);
  var
    FileName: array [0..MAX_PATH] of Char;
    ModuleInfo: TModuleInfo;
  begin
    {$IFDEF FPC}
    if GetModuleInformation(ProcessHandle, Module, ModuleInfo, SizeOf(ModuleInfo)) then
    {$ELSE}
    if GetModuleInformation(ProcessHandle, Module, @ModuleInfo, SizeOf(ModuleInfo)) then
    {$ENDIF FPC}
    begin
      if HandlesOnly then
        List.AddObject('', Pointer(ModuleInfo.lpBaseOfDll))
      else
      if GetModuleFileNameEx(ProcessHandle, Module, Filename, SizeOf(Filename)) > 0 then
        List.AddObject(FileName, Pointer(ModuleInfo.lpBaseOfDll));
    end;
  end;

  function EnumModulesVQ(ProcessHandle: THandle): Boolean;
  var
    MemInfo: TMemoryBasicInformation;
    Base: PChar;
    LastAllocBase: Pointer;
    Res: DWORD;
  begin
    Base := nil;
    LastAllocBase := nil;
    FillChar(MemInfo, SizeOf(MemInfo), #0);
    Res := VirtualQueryEx(ProcessHandle, Base, MemInfo, SizeOf(MemInfo));
    Result := (Res = SizeOf(MemInfo));
    while Res = SizeOf(MemInfo) do
    begin
      if MemInfo.AllocationBase <> LastAllocBase then
      begin
        {$IFDEF FPC}
        if MemInfo._Type = MEM_IMAGE then
        {$ELSE}
        if MemInfo.Type_9 = MEM_IMAGE then
        {$ENDIF FPC}
          AddToList(ProcessHandle, HMODULE(MemInfo.AllocationBase));
        LastAllocBase := MemInfo.AllocationBase;
      end;
      Inc(Base, MemInfo.RegionSize);
      Res := VirtualQueryEx(ProcessHandle, Base, MemInfo, SizeOf(MemInfo));
    end;
  end;

  function EnumModulesPS: Boolean;
  var
    ProcessHandle: THandle;
    Needed: DWORD;
    Modules: array of THandle;
    I, Cnt: Integer;
  begin
    Result := False;
    ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, ProcessID);
    if ProcessHandle <> 0 then
    try
      Result := EnumProcessModules(ProcessHandle, nil, 0, Needed);
      if Result then
      begin
        Cnt := Needed div SizeOf(HMODULE);
        SetLength(Modules, Cnt);
        if EnumProcessModules(ProcessHandle, @Modules[0], Needed, Needed) then
          for I := 0 to Cnt - 1 do
            AddToList(ProcessHandle, Modules[I]);
      end
      else
        Result := EnumModulesVQ(ProcessHandle);
    finally
      CloseHandle(ProcessHandle);
    end;
  end;

 { TODO: Check return value of CreateToolhelp32Snapshot on Windows NT (0?) }

  function EnumModulesTH: Boolean;
  var
    SnapProcHandle: THandle;
    Module: TModuleEntry32;
    Next: Boolean;
  begin
    SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, ProcessID);
    Result := (SnapProcHandle <> INVALID_HANDLE_VALUE);
    if Result then
    try
      FillChar(Module, SizeOf(Module), #0);
      Module.dwSize := SizeOf(Module);
      Next := Module32First(SnapProcHandle, Module);
      while Next do
      begin
        if HandlesOnly then
          List.AddObject('', Pointer(Module.hModule))
        else
          List.AddObject(Module.szExePath, Pointer(Module.hModule));
        Next := Module32Next(SnapProcHandle, Module);
      end;
    finally
      CloseHandle(SnapProcHandle);
    end;
  end;

begin
  List.BeginUpdate;
  try
    if IsWinNT then
      Result := EnumModulesPS
    else
      Result := EnumModulesTH;
  finally
    List.EndUpdate;
  end;
end;

function GetTasksList(const List: TStrings): Boolean;

  function EnumWindowsProc(Wnd: HWND; List: TStrings): Boolean; stdcall;
  var
    Caption: array [0..1024] of Char;
  begin
    if IsMainAppWindow(Wnd) and (GetWindowText(Wnd, Caption, SizeOf(Caption)) > 0) then
      List.AddObject(Caption, Pointer(Wnd));
    Result := True;
  end;

begin
  List.BeginUpdate;
  try
    Result := EnumWindows(@EnumWindowsProc, Integer(List));
  finally
    List.EndUpdate;
  end;
end;

function ModuleFromAddr(const Addr: Pointer): HMODULE;
var
  MI: TMemoryBasicInformation;
begin
  VirtualQuery(Addr, MI, SizeOf(MI));
  if MI.State <> MEM_COMMIT then
    Result := 0
  else
    Result := HMODULE(MI.AllocationBase);
end;

{$IFNDEF FPC}
function IsSystemModule(const Module: HMODULE): Boolean;
var
  CurModule: PLibModule;
begin
  Result := False;
  if Module <> 0 then
  begin
    CurModule := LibModuleList;
    while CurModule <> nil do
    begin
      if CurModule.Instance = Module then
      begin
        Result := True;
        Break;
      end;
      CurModule := CurModule.Next;
    end;
  end;
end;
{$ENDIF ~FPC}

// Reference: http://msdn.microsoft.com/library/periodic/period97/win321197.htm
{ TODO : wrong link }

function IsMainAppWindow(Wnd: HWND): Boolean;
var
  ParentWnd: HWND;
  ExStyle: DWORD;
begin
  if IsWindowVisible(Wnd) then
  begin
    ParentWnd := GetWindowLong(Wnd, GWL_HWNDPARENT);
    ExStyle := GetWindowLong(Wnd, GWL_EXSTYLE);
    Result := ((ParentWnd = 0) or (ParentWnd = GetDesktopWindow)) and
      ((ExStyle and WS_EX_TOOLWINDOW = 0) or (ExStyle and WS_EX_APPWINDOW <> 0));
  end
  else
    Result := False;
end;

function IsWindowResponding(Wnd: HWND; Timeout: Integer): Boolean;
var
  Res: DWORD;
begin
  Result := SendMessageTimeout(Wnd, WM_NULL, 0, 0, SMTO_ABORTIFHUNG, Timeout, Res) <> 0;
end;

function GetWindowIcon(Wnd: HWND; LargeIcon: Boolean): HICON;
var
  Width, Height: Integer;
  TempIcon: HICON;
  IconType: DWORD;
begin
  if LargeIcon then
  begin
    Width := GetSystemMetrics(SM_CXICON);
    Height := GetSystemMetrics(SM_CYICON);
    IconType := ICON_BIG;
    TempIcon := GetClassLong(Wnd, GCL_HICON);
  end
  else
  begin
    Width := GetSystemMetrics(SM_CXSMICON);
    Height := GetSystemMetrics(SM_CYSMICON);
    IconType := ICON_SMALL;
    TempIcon := GetClassLong(Wnd, GCL_HICONSM);
  end;
  if TempIcon = 0 then
    TempIcon := SendMessage(Wnd, WM_GETICON, IconType, 0);
  if (TempIcon = 0) and not LargeIcon then
    TempIcon := SendMessage(Wnd, WM_GETICON, ICON_BIG, 0);
  Result := CopyImage(TempIcon, IMAGE_ICON, Width, Height, 0);
end;

function GetWindowCaption(Wnd: HWND): string;
const
  BufferAllocStep = 256;
var
  Buffer: PChar;
  Size, TextLen: Integer;
begin
  { TODO : use string }
  Result := '';
  Buffer := nil;
  try
    Size := GetWindowTextLength(Wnd) + 2 - BufferAllocStep;
    repeat
      Inc(Size, BufferAllocStep);
      ReallocMem(Buffer, Size);
      TextLen := GetWindowText(Wnd, Buffer, Size);
    until TextLen < Size - 1;
    if TextLen > 0 then
      Result := Buffer;
  finally
    FreeMem(Buffer);
  end;
end;

// Q178893
// http://support.microsoft.com/default.aspx?scid=kb;en-us;178893

function TerminateApp(ProcessID: DWORD; Timeout: Integer): TJclTerminateAppResult;
var
  ProcessHandle: THandle;

  function EnumWindowsProc(Wnd: HWND; ProcessID: DWORD): Boolean; stdcall;
  var
    PID: DWORD;
  begin
    GetWindowThreadProcessId(Wnd, @PID);
    if ProcessID = PID then
      PostMessage(Wnd, WM_CLOSE, 0, 0);
    Result := True;
  end;

begin
  Result := taError;
  if ProcessID <> GetCurrentProcessId then
  begin
    ProcessHandle := OpenProcess(SYNCHRONIZE or PROCESS_TERMINATE, False, ProcessID);
    if ProcessHandle <> 0 then
    try
      EnumWindows(@EnumWindowsProc, LPARAM(ProcessID));
      if WaitForSingleObject(ProcessHandle, Timeout) = WAIT_OBJECT_0 then
        Result := taClean
      else
      if TerminateProcess(ProcessHandle, 0) then
        Result := taKill;
    finally
      CloseHandle(ProcessHandle);
    end;
  end;
end;

function TerminateTask(Wnd: HWND; Timeout: Integer): TJclTerminateAppResult;
var
  PID: DWORD;
begin
  if GetWindowThreadProcessId(Wnd, @PID) <> 0 then
    Result := TerminateApp(PID, Timeout)
  else
    Result := taError;  
end;

function GetProcessNameFromWnd(Wnd: HWND): string;
var
  List: TStringList;
  PID: DWORD;
  I: Integer;
begin
  Result := '';
  if IsWindow(Wnd) then
  begin
    PID := INVALID_HANDLE_VALUE;
    GetWindowThreadProcessId(Wnd, @PID);
    List := TStringList.Create;
    try
      if RunningProcessesList(List, True) then
      begin
        I := List.IndexOfObject(Pointer(PID));
        if I > -1 then
          Result := List[I];
      end;
    finally
      List.Free;
    end;
  end;
end;

function GetPidFromProcessName(const ProcessName: string): DWORD;
var
  List: TStringList;
  I: Integer;
  HasFullPath: Boolean;
begin
  Result := INVALID_HANDLE_VALUE;
  List := TStringList.Create;
  try
    HasFullPath := ExtractFilePath(ProcessName) <> '';
    if RunningProcessesList(List, HasFullPath) then
    begin
      I := List.IndexOf(ProcessName);
      if I > -1 then
        Result := DWORD(List.Objects[I]);
    end;
  finally
    List.Free;
  end;
end;

function GetProcessNameFromPid(PID: DWORD): string;
var
  List: TStringList;
  I: Integer;
begin
  // Note: there are other ways to retrieve the name of the process given it's
  // PID but this implementation seems to work best without making assumptions
  // although it may not be the most efficient implementation.
  Result := '';
  List := TStringList.Create;
  try
    if RunningProcessesList(List, True) then
    begin
      I := List.IndexOfObject(Pointer(PID));
      if I > -1 then
        Result := List[I];
    end;
  finally
    List.Free;
  end;
end;

function GetMainAppWndFromPid(PID: DWORD): HWND;
type
  PSearch = ^TSearch;
  TSearch = record
    PID: DWORD;
    Wnd: HWND;
  end;
var
  SearchRec: TSearch;

  function EnumWindowsProc(Wnd: HWND; Res: PSearch): Boolean; stdcall;
  var
    WindowPid: DWORD;
  begin
    WindowPid := 0;
    GetWindowThreadProcessId(Wnd, @WindowPid);
    if (WindowPid = Res^.PID) and IsMainAppWindow(Wnd) then
    begin
      Res^.Wnd := Wnd;
      Result := False;
    end
    else
      Result := True;
  end;

begin
  SearchRec.PID := PID;
  SearchRec.Wnd := 0;
  EnumWindows(@EnumWindowsProc, Integer(@SearchRec));
  Result := SearchRec.Wnd;
end;

function GetShellProcessName: string;
const
  cShellKey = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\WinLogon';
  cShellValue = 'Shell';
  cShellDefault = 'explorer.exe';
  cShellSystemIniFileName = 'system.ini';
  cShellBootSection = 'boot';
begin
  if IsWinNT then
    Result := RegReadStringDef(HKEY_LOCAL_MACHINE, cShellKey, cShellValue, '')
  else
    Result := IniReadString(PathAddSeparator(GetWindowsFolder) + cShellSystemIniFileName, cShellBootSection, cShellValue);
  if Result = '' then
    Result := cShellDefault;
end;

function GetShellProcessHandle: THandle;
var
  Pid: Longword;
begin
  Pid := GetPidFromProcessName(GetShellProcessName);
  Result := OpenProcess(PROCESS_ALL_ACCESS, False, Pid);
  if Result = 0 then
    RaiseLastOSError;
end;

//=== Version Information ====================================================

{ Q159/238

  Windows 95 retail, OEM    4.00.950                      7/11/95
  Windows 95 retail SP1     4.00.950A                     7/11/95-12/31/95
  OEM Service Release 2     4.00.1111* (4.00.950B)        8/24/96
  OEM Service Release 2.1   4.03.1212-1214* (4.00.950B)   8/24/96-8/27/97
  OEM Service Release 2.5   4.03.1214* (4.00.950C)        8/24/96-11/18/97
  Windows 98 retail, OEM    4.10.1998                     5/11/98
  Windows 98 Second Edition 4.10.2222A                    4/23/99
  Windows Millennium        4.90.3000
}
{ TODO : Distinquish between all these different releases? }

var
  KernelVersionHi: DWORD;

function GetWindowsVersion: TWindowsVersion;
var
  TrimmedWin32CSDVersion: string;
begin
  Result := wvUnknown;
  TrimmedWin32CSDVersion := Trim(Win32CSDVersion);
  case Win32Platform of
    VER_PLATFORM_WIN32_WINDOWS:
      case Win32MinorVersion of
        0..9:
          if (TrimmedWin32CSDVersion = 'B') or (TrimmedWin32CSDVersion = 'C') then
            Result := wvWin95OSR2
          else
            Result := wvWin95;
        10..89:
          // On Windows ME Win32MinorVersion can be 10 (indicating Windows 98
          // under certain circumstances (image name is setup.exe). Checking
          // the kernel version is one way of working around that.
          if KernelVersionHi = $0004005A then // 4.90.x.x
            Result := wvWinME
          else
          if TrimmedWin32CSDVersion = 'A' then
            Result := wvWin98SE
          else
            Result := wvWin98;
        90:
          Result := wvWinME;
      end;
    VER_PLATFORM_WIN32_NT:
      case Win32MajorVersion of
        3:
          case Win32MinorVersion of
            1:
              Result := wvWinNT31;
            5:
              Result := wvWinNT35;
            51:
              Result := wvWinNT351;
          end;
        4:
          Result := wvWinNT4;
        5:
          case Win32MinorVersion of
            0:
              Result := wvWin2000;
            1:
              Result := wvWinXP;
            2:
              Result := wvWin2003;
          end;
      end;
  end;
end;

function NtProductType: TNtProductType;
const
  ProductType = 'SYSTEM\CurrentControlSet\Control\ProductOptions';
var
  Product: string;
  VersionInfo: TOSVersionInfoEx;
begin
  Result := ptUnknown;
  FillChar(VersionInfo, SizeOf(VersionInfo), 0);
  VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);

  // Favor documented API over registry
  if IsWinNT4 and (GetWindowsServicePackVersion >= 6) then
  begin
    if GetVersionEx(VersionInfo) then
    begin
      if (VersionInfo.wProductType = VER_NT_WORKSTATION) then
        Result := ptWorkstation
      else
        Result := ptServer;
    end;
  end
  else
  if IsWin2K or IsWin2003 then
  begin
    if GetVersionEx(VersionInfo) then
    begin
      if (VersionInfo.wProductType = VER_NT_SERVER) then
      begin
        if (VersionInfo.wSuiteMask and VER_SUITE_DATACENTER) = VER_SUITE_DATACENTER then
          Result := ptDatacenterServer
        else
        if (VersionInfo.wSuiteMask and VER_SUITE_ENTERPRISE) = VER_SUITE_ENTERPRISE then
          Result := ptAdvancedServer
        else
          result := ptServer;
      end
      else
      if (VersionInfo.wProductType = VER_NT_WORKSTATION) then
        Result := ptProfessional;
    end;
  end
  else
  if IsWinXP then
  begin
    if GetVersionEx(VersionInfo) then
    begin
      if VersionInfo.wProductType = VER_NT_WORKSTATION then
      begin
        if (VersionInfo.wSuiteMask and VER_SUITE_PERSONAL) = VER_SUITE_PERSONAL then
          Result := ptPersonal
        else
          Result := ptProfessional;
      end;
    end;
  end;

  if Result = ptUnknown then
  begin
    // Non Windows 2000/XP system or the above method failed, try registry
    Product := RegReadStringDef(HKEY_LOCAL_MACHINE, ProductType, 'ProductType', '');
    if CompareText(Product, 'WINNT') = 0 then
      Result :=  ptWorkStation
    else
    if CompareText(Product, 'SERVERNT') = 0 then
      Result := {ptServer} ptAdvancedServer
    else
    if CompareText(Product, 'LANMANNT') = 0 then
      Result := {ptAdvancedServer} ptServer
    else
      Result := ptUnknown;
  end;
end;

function GetWindowsVersionString: string;
begin
  case GetWindowsVersion of
    wvWin95:
      Result := RsOSVersionWin95;
    wvWin95OSR2:
      Result := RsOSVersionWin95OSR2;
    wvWin98:
      Result := RsOSVersionWin98;
    wvWin98SE:
      Result := RsOSVersionWin98SE;
    wvWinME:
      Result := RsOSVersionWinME;
    wvWinNT31, wvWinNT35, wvWinNT351:
      Result := Format(RsOSVersionWinNT3, [Win32MinorVersion]);
    wvWinNT4:
      Result := Format(RsOSVersionWinNT4, [Win32MinorVersion]);
    wvWin2000:
      Result := RsOSVersionWin2000;
    wvWinXP:
      Result := RsOSVersionWinXP;
    wvWin2003:
      Result := RsOSVersionWin2003;
  else
    Result := '';
  end;
end;

function NtProductTypeString: string;
begin
  case NtProductType of
   ptWorkStation:
     Result := RsProductTypeWorkStation;
   ptServer:
     Result := RsProductTypeServer;
   ptAdvancedServer:
     Result := RsProductTypeAdvancedServer;
   ptPersonal:
     Result := RsProductTypePersonal;
   ptProfessional:
     Result := RsProductTypeProfessional;
   ptDatacenterServer:
     Result := RsProductTypeDatacenterServer;
  else
    Result := '';
  end;
end;

function GetWindowsServicePackVersion: Integer;
const
  RegWindowsControl = 'SYSTEM\CurrentControlSet\Control\Windows';
var
  SP: Integer;
  VersionInfo: TOSVersionInfoEx;
begin
  Result := 0;
  if IsWin2K or IsWinXP or IsWin2003 then
  begin
    FillChar(VersionInfo, SizeOf(VersionInfo), 0);
    VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
    if GetVersionEx(VersionInfo) then
      Result := VersionInfo.wServicePackMajor;
    end
  else
  begin
    SP := RegReadIntegerDef(HKEY_LOCAL_MACHINE, RegWindowsControl, 'CSDVersion', 0);
    Result := StrToInt(IntToHex(SP, 4)) div 100;
  end;
end;

function GetWindowsServicePackVersionString: string;
var
  SP: Integer;
begin
  SP := GetWindowsServicePackVersion;
  if SP > 0 then
    Result := Format(RsSPInfo, [SP])
  else
    Result := '';
end;

// Imports copied from OpenGL unit. Direct using of OpenGL unit might cause unexpected problems due
// setting 8087CW in the intialization section
function glGetString(name: Cardinal): PChar; stdcall; external opengl32; 
function glGetError: Cardinal; stdcall; external opengl32;
function gluErrorString(errCode: Cardinal): PChar; stdcall; external 'glu32.dll';

function GetOpenGLVersion(const Win: HWND; out Version, Vendor: AnsiString): Boolean;
const
  GL_NO_ERROR = 0;
  GL_VENDOR   = $1F00;
  GL_VERSION  = $1F02;
var
  pfd: TPixelFormatDescriptor;
  iFormatIndex: Integer;
  hGLContext: HGLRC;
  hGLDC: HDC;
  pcTemp: PChar;
  glErr: Cardinal;
  bError: Boolean;
  sOpenGLVersion, sOpenGLVendor: string;
  Save8087CW: Word;

  procedure FunctionFailedError(Name: string);
  begin
    raise EJclError.CreateResFmt(@RsEOpenGLInfo, [Name]);
  end;

begin
  { To call for the version information string we must first have an active
    context established for use.  We can, of course, close this after use }
  Save8087CW := Get8087ControlWord;
  try
    Set8087CW($133F);
    hGLContext := 0;
    Result := False;
    bError := False;

    if Win = 0 then
    begin
      Result := False;
      Vendor := RsOpenGLInfoError;
      Version := RsOpenGLInfoError;
      Exit;
    end;

    FillChar(pfd, SizeOf(pfd), 0);
    with pfd do
    begin
      nSize := SizeOf(pfd);
      nVersion := 1;  { The Current Version of the descriptor is 1 }
      dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL;
      iPixelType := PFD_TYPE_RGBA;
      cColorBits := 24;  { support 24-bit colour }
      cDepthBits := 32;  { Depth of the z-buffer }
      iLayerType := PFD_MAIN_PLANE;
    end;

    hGLDC := GetDC(Win);
    try
      iFormatIndex := ChoosePixelFormat(hGLDC, @pfd);
      if iFormatIndex = 0 then
        FunctionFailedError('ChoosePixelFormat');

      if not SetPixelFormat(hGLDC, iFormatIndex, @pfd) then
        FunctionFailedError('SetPixelFormat');

      hGLContext := wglCreateContext(hGLDC);
      if hGLContext = 0 then
        FunctionFailedError('wglCreateContext');

      if not wglMakeCurrent(hGLDC, hGLContext) then
        FunctionFailedError('wglMakeCurrent');

      { TODO : Review the following.  Not sure I am 100% happy with this code
               in its current structure. }
      pcTemp := glGetString(GL_VERSION);
      if pcTemp <> nil then
      begin
        { TODO : Store this information in a Global Variable, and return that??
                 This would save this work being performed again with later calls }
        sOpenGLVersion := StrPas(pcTemp);
      end
      else
      begin
        bError := True;
        glErr := glGetError;
        if glErr <> GL_NO_ERROR then
        begin
          sOpenGLVersion := gluErrorString(glErr);
          sOpenGLVendor := '';
        end;
      end;

      pcTemp := glGetString(GL_VENDOR);
      if pcTemp <> nil then
      begin
        { TODO : Store this information in a Global Variable, and return that??
                 This would save this work being performed again with later calls }
        sOpenGLVendor := StrPas(pcTemp);
      end
      else
      begin
        bError := True;
        glErr := glGetError;
        if glErr <> GL_NO_ERROR then
        begin
          sOpenGLVendor := gluErrorString(glErr);
          Exit;
        end;
      end;

      Result := (not bError);
      Version := sOpenGLVersion;
      Vendor := sOpenGLVendor;
    finally
      { Close all resources }
      wglMakeCurrent(hGLDC, 0);
      if hGLContext <> 0 then
        wglDeleteContext(hGLContext);
    end;
  finally
    Set8087CW(Save8087CW);
  end;
end;

{$ENDIF ~CLR}
{$ENDIF MSWINDOWS}
{$IFNDEF CLR}

function GetOSVersionString: string;
{$IFDEF UNIX}
var
  MachineInfo: utsname;
begin
  uname(MachineInfo);
  Result := Format('%s %s', [MachineInfo.sysname, MachineInfo.release]);
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
begin
  Result := Format('%s %s', [GetWindowsVersionString, GetWindowsServicePackVersionString]);
end;
{$ENDIF MSWINDOWS}

//=== Hardware ===============================================================

// Helper function for GetMacAddress()
// Converts the adapter_address array to a string

function AdapterToString(Adapter: PByteArray): string;
begin
  Result := Format('%2.2x-%2.2x-%2.2x-%2.2x-%2.2x-%2.2x',
   [Integer(Adapter[0]), Integer(Adapter[1]),
    Integer(Adapter[2]), Integer(Adapter[3]),
    Integer(Adapter[4]), Integer(Adapter[5])]);
end;

{ TODO: RTLD version of NetBios }
{$IFDEF MSWINDOWS}
type
  TNetBios = function(P: PNCB): Byte; stdcall;

var
  NetBiosLib: HINST = 0;
  _NetBios: TNetBios;
  {$IFDEF FPC}
  NullAdapterAddress: array [0..5] of Byte = ($00, $00, $00, $00, $00, $00);
  OID_ipMACEntAddr: array [0..9] of UINT = (1, 3, 6, 1, 2, 1, 2, 2, 1, 6);
  OID_ifEntryType: array [0..9] of UINT = (1, 3, 6, 1, 2, 1, 2, 2, 1, 3);
  OID_ifEntryNum: array [0..7] of UINT = (1, 3, 6, 1, 2, 1, 2, 1);
  {$ENDIF FPC}

function GetMacAddresses(const Machine: string; const Addresses: TStrings): Integer;

  procedure ExitNetbios;
    begin
    if NetBiosLib <> 0 then
    begin
      FreeLibrary(NetBiosLib);
      NetBiosLib := 0;
    end;
  end;

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

  function NetBios(P: PNCB): Byte;
  begin
    if InitNetbios then
      Result := _NetBios(P)
    else
      Result := 1; // anything other than NRC_GOODRET will do
  end;

  procedure GetMacAddressesNetBios;
  // Platform SDK
  // http://msdn.microsoft.com/library/default.asp?url=/library/en-us/netbios/netbios_1l82.asp

  // Microsoft Knowledge Base Article - 118623
  // HOWTO: Get the MAC Address for an Ethernet Adapter
  // http://support.microsoft.com/default.aspx?scid=kb;en-us;118623
  type
    AStat = packed record
      adapt: TAdapterStatus;
      NameBuff: array [0..29] of TNameBuffer;
    end;
  var
    NCB: TNCB;
    Enum: TLanaEnum;
    I, L, NameLen: Integer;
    Adapter: AStat;
    MachineName: string;
  begin
    MachineName := UpperCase(Machine);
    if MachineName = '' then
      MachineName := '*';
    NameLen := Length(MachineName);
    L := NCBNAMSZ - NameLen;
    if L > 0 then
    begin
      SetLength(MachineName, NCBNAMSZ);
      FillChar(MachineName[NameLen + 1], L, ' ');
    end;
    FillChar(NCB, SizeOf(NCB), #0);
    NCB.ncb_command := NCBENUM;
    NCB.ncb_buffer := Pointer(@Enum);
    NCB.ncb_length := SizeOf(Enum);
    if NetBios(@NCB) = NRC_GOODRET then
    begin
      Result := Enum.Length;
      for I := 0 to Ord(Enum.Length) - 1 do
      begin
        FillChar(NCB, SizeOf(NCB), #0);
        NCB.ncb_command := NCBRESET;
        NCB.ncb_lana_num := Enum.lana[I];
        if NetBios(@NCB) = NRC_GOODRET then
        begin
          FillChar(NCB, SizeOf(NCB), #0);
          NCB.ncb_command := NCBASTAT;
          NCB.ncb_lana_num := Enum.lana[I];
          Move(MachineName[1], NCB.ncb_callname, SizeOf(NCB.ncb_callname));
          NCB.ncb_buffer := PChar(@Adapter);
          NCB.ncb_length := SizeOf(Adapter);
          if NetBios(@NCB) = NRC_GOODRET then
            Addresses.Add(AdapterToString(@Adapter.adapt));
        end;
      end;
    end;
  end;

  procedure GetMacAddressesSnmp;
  const
    InetMib1 = 'inetmib1.dll';
    DunAdapterAddress: array [0..4] of Byte = ($44, $45, $53, $54, $00);
    {$IFNDEF FPC // can't resolve address of const }
    NullAdapterAddress: array [0..5] of Byte = ($00, $00, $00, $00, $00, $00);
    OID_ipMACEntAddr: array [0..9] of UINT = (1, 3, 6, 1, 2, 1, 2, 2, 1, 6);
    OID_ifEntryType: array [0..9] of UINT = (1, 3, 6, 1, 2, 1, 2, 2, 1, 3);
    OID_ifEntryNum: array [0..7] of UINT = (1, 3, 6, 1, 2, 1, 2, 1);
    {$ENDIF ~FPC}
  var
    PollForTrapEvent: THandle;
    SupportedView: PAsnObjectIdentifier;
    MIB_ifMACEntAddr: TAsnObjectIdentifier;
    MIB_ifEntryType: TAsnObjectIdentifier;
    MIB_ifEntryNum: TAsnObjectIdentifier;
    VarBindList: TSnmpVarBindList;
    VarBind: array [0..1] of TSnmpVarBind;
    ErrorStatus, ErrorIndex: TAsnInteger32;
    DTmp: Integer;
    Ret: Boolean;
    MAC: PByteArray;
  begin
    if LoadSnmp then
    try
      if LoadSnmpExtension(InetMib1) then
      try
        MIB_ifMACEntAddr.idLength := Length(OID_ipMACEntAddr);
        MIB_ifMACEntAddr.ids := @OID_ipMACEntAddr;
        MIB_ifEntryType.idLength := Length(OID_ifEntryType);
        MIB_ifEntryType.ids := @OID_ifEntryType;
        MIB_ifEntryNum.idLength := Length(OID_ifEntryNum);
        MIB_ifEntryNum.ids := @OID_ifEntryNum;
        if SnmpExtensionInit(GetTickCount, PollForTrapEvent, SupportedView) then
        begin
          VarBindList.list := @VarBind[0];
          VarBind[0].name := DEFINE_NULLOID;
          VarBind[1].name := DEFINE_NULLOID;
          VarBindList.len := 1;
          SnmpUtilOidCpy(@VarBind[0].name, @MIB_ifEntryNum);
          Ret := SnmpExtensionQuery(SNMP_PDU_GETNEXT, VarBindList, ErrorStatus, ErrorIndex);
          if Ret then
          begin
            Result := VarBind[0].value.number;
            VarBindList.len := 2;
            SnmpUtilOidCpy(@VarBind[0].name, @MIB_ifEntryType);
            SnmpUtilOidCpy(@VarBind[1].name, @MIB_ifMACEntAddr);
            while Ret do
            begin
              Ret := SnmpExtensionQuery(SNMP_PDU_GETNEXT, VarBindList, ErrorStatus, ErrorIndex);
              if Ret then
              begin
                Ret := SnmpUtilOidNCmp(@VarBind[0].name, @MIB_ifEntryType, MIB_ifEntryType.idLength) = SNMP_ERRORSTATUS_NOERROR;
                if Ret then
                begin
                  DTmp := VarBind[0].value.number;
                  if DTmp = 6 then
                  begin
                    Ret := SnmpUtilOidNCmp(@VarBind[1].name, @MIB_ifMACEntAddr, MIB_ifMACEntAddr.idLength) = SNMP_ERRORSTATUS_NOERROR;
                    if Ret and (VarBind[1].value.address.stream <> nil) then
                    begin
                      MAC := PByteArray(VarBind[1].value.address.stream);
                      if not CompareMem(MAC, @NullAdapterAddress, SizeOf(NullAdapterAddress)) then
                        Addresses.Add(AdapterToString(MAC));
                    end;
                  end;
                end;
              end;
            end;
          end;
          SnmpUtilVarBindFree(@VarBind[0]);
          SnmpUtilVarBindFree(@VarBind[1]);
        end;
      finally
        UnloadSnmpExtension;
      end;
    finally
      UnloadSnmp;
    end;
  end;

begin
  Result := -1;
  Addresses.BeginUpdate;
  try
    Addresses.Clear;
    GetMacAddressesNetBios;
    if (Result <= 0) and (Machine = '') then
      GetMacAddressesSnmp;
  finally
    Addresses.EndUpdate;
  end;
end;
{$ENDIF MSWINDOWS}
function ReadTimeStampCounter: Int64; assembler;
asm
        DW      $310F
end;

function GetIntelCacheDescription(const D: Byte): string;
var
  I: Integer;
begin
  Result := '';
  if D <> 0 then
    for I := Low(IntelCacheDescription) to High(IntelCacheDescription) do
      if IntelCacheDescription[I].D = D then
      begin
        Result := IntelCacheDescription[I].I;
        Break;
      end;
  // (outchy) added a return value for unknow D value
  if Result = '' then
    Result := Format(RsIntelUnknownCache,[D]);
end;

procedure GetCpuInfo(var CpuInfo: TCpuInfo);
begin
  CpuInfo := CPUID;
  CpuInfo.IsFDIVOK := TestFDIVInstruction;
  if CpuInfo.HasInstruction then
  begin
    {$IFDEF MSWINDOWS}
    if (CpuInfo.Features and TSC_FLAG) = TSC_FLAG then
      GetCpuSpeed(CpuInfo.FrequencyInfo);
    {$ENDIF MSWINDOWS}
  end;
end;

function RoundFrequency(const Frequency: Integer): Integer;
const
  NF: array [0..8] of Integer = (0, 20, 33, 50, 60, 66, 80, 90, 100);
var
  Freq, RF: Integer;
  I: Byte;
  Hi, Lo: Byte;
begin
  RF := 0;
  Freq := Frequency mod 100;
  for I := 0 to 8 do
  begin
    if Freq < NF[I] then
    begin
      Hi := I;
      Lo := I - 1;
      if (NF[Hi] - Freq) > (Freq - NF[Lo]) then
        RF := NF[Lo] - Freq
      else
        RF := NF[Hi] - Freq;
      Break;
    end;
  end;
  Result := Frequency + RF;
end;

function GetCPUSpeed(var CpuSpeed: TFreqInfo): Boolean;
{$IFDEF UNIX}
begin
  { TODO : GetCPUSpeed: Solution for Linux }
  Result := False;
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
var
  T0, T1: Int64;
  CountFreq: Int64;
  Freq, Freq2, Freq3, Total: Int64;
  TotalCycles, Cycles: Int64;
  Stamp0, Stamp1: Int64;
  TotalTicks, Ticks: Double;
  Tries, Priority: Integer;
  Thread: THandle;
begin
  Stamp0 := 0;
  Stamp1 := 0;
  Freq  := 0;
  Freq2 := 0;
  Freq3 := 0;
  Tries := 0;
  TotalCycles := 0;
  TotalTicks := 0;
  Total := 0;

  Thread := GetCurrentThread();
  Result := QueryPerformanceFrequency(CountFreq);
  if Result then
  begin
    while ((Tries < 3) or ((Tries < 20) and ((Abs(3 * Freq - Total) > 3) or
      (Abs(3 * Freq2 - Total) > 3) or (Abs(3 * Freq3 - Total) > 3)))) do
    begin
      Inc(Tries);
      Freq3 := Freq2;
      Freq2 := Freq;
      QueryPerformanceCounter(T0);
      T1 := T0;

      Priority := GetThreadPriority(Thread);
      if Priority <> THREAD_PRIORITY_ERROR_RETURN then
        SetThreadPriority(Thread, THREAD_PRIORITY_TIME_CRITICAL);
      try
        while T1 - T0 < 50 do
        begin
          QueryPerformanceCounter(T1);
          Stamp0 := ReadTimeStampCounter;
        end;
        T0 := T1;

        while T1 - T0 < 1000 do
        begin
          QueryPerformanceCounter(T1);
          Stamp1 := ReadTimeStampCounter;
        end;
      finally
        if Priority <> THREAD_PRIORITY_ERROR_RETURN then
          SetThreadPriority(Thread, Priority);
      end;

      Cycles := Stamp1 - Stamp0;
      Ticks := T1 - T0;
      Ticks := Ticks * 100000;

      // avoid division by zero
      if CountFreq = 0 then
        Ticks := High(Int64)
      else
        Ticks := Ticks / (CountFreq / 10);

      TotalTicks := TotalTicks + Ticks;
      TotalCycles := TotalCycles + Cycles;

      // avoid division by zero
      if Ticks = 0 then
        Freq := High(Freq)
      else
        Freq := Round(Cycles / Ticks);

      Total := Freq + Freq2 + Freq3;
    end;

    // avoid division by zero
    if TotalTicks = 0 then
    begin
      Freq3 := High(Freq3);
      Freq2 := High(Freq2);
      CpuSpeed.RawFreq := High(CpuSpeed.RawFreq);
    end
    else
    begin
      Freq3 := Round((TotalCycles *  10) / TotalTicks); // freq. in multiples of 10^5 Hz
      Freq2 := Round((TotalCycles * 100) / TotalTicks); // freq. in multiples of 10^4 Hz
      CpuSpeed.RawFreq := Round(TotalCycles / TotalTicks);
    end;

    CpuSpeed.NormFreq := CpuSpeed.RawFreq;

    if Freq2 - (Freq3 * 10) >= 6 then
      Inc(Freq3);


    Freq := CpuSpeed.RawFreq * 10;
    if (Freq3 - Freq) >= 6 then
      Inc(CpuSpeed.NormFreq);

    CpuSpeed.ExTicks := Round(TotalTicks);
    CpuSpeed.InCycles := TotalCycles;

    CpuSpeed.NormFreq := RoundFrequency(CpuSpeed.NormFreq);
    Result := True;
  end;
end;
{$ENDIF MSWINDOWS}

// Helper function for CPUID. Initializes Intel specific fields.

procedure IntelSpecific(var CpuInfo: TCpuInfo);
var
  I, J: Integer;
begin
  with CpuInfo do
  begin
    Manufacturer := 'Intel';
    if HasCacheInfo then
    begin
      if (IntelSpecific.L2Cache <> 0) then
      begin
        L2CacheSize := IntelSpecific.L2Cache shr 16;
        L2CacheLineSize := IntelSpecific.L2Cache and $FF;
        L2CacheAssociativity := (IntelSpecific.L2Cache shr 12) and $F;
      end;
      for I := Low(IntelSpecific.CacheDescriptors) to High(IntelSpecific.CacheDescriptors) do
        if IntelSpecific.CacheDescriptors[I]<>0 then
          for J := Low(IntelCacheDescription) to High(IntelCacheDescription) do
            if IntelCacheDescription[J].D = IntelSpecific.CacheDescriptors[I] then
              with IntelCacheDescription[J] do
        case Family of
          //cfInstructionTLB :
          //cfDataTLB :
          cfL1InstructionCache :
            begin
              Inc(L1InstructionCacheSize,Size);
              L1InstructionCacheLineSize := LineSize;
              L1InstructionCacheAssociativity := WaysOfAssoc;
            end;
          cfL1DataCache :
            begin
              Inc(L1DataCacheSize,Size);
              L1DataCacheLineSize := LineSize;
              L1DataCacheAssociativity := WaysOfAssoc;
            end;
          cfL2Cache :
            if (IntelSpecific.L2Cache = 0) then
            begin
              Inc(L2CacheSize,Size);
              L2CacheLineSize := LineSize;
              L2CacheAssociativity := WaysOfAssoc;
            end;
          cfL3Cache :
            begin
              Inc(L3CacheSize,Size);
              L3CacheLineSize := LineSize;
              L3CacheAssociativity := WaysOfAssoc;
              L3LinesPerSector := LinePerSector;
            end;
          //cfTrace :    // no numeric informations
          //cfOther :
        end;
    end;
    if not HasExtendedInfo then
    begin
      case Family of
        4:
          case Model of
            1:
              CpuName := 'Intel 486DX Processor';
            2:
              CpuName := 'Intel 486SX Processor';
            3:
              CpuName := 'Intel DX2 Processor';
            4:
              CpuName := 'Intel 486 Processor';
            5:
              CpuName := 'Intel SX2 Processor';
            7:
              CpuName := 'Write-Back Enhanced Intel DX2 Processor';
            8:
              CpuName := 'Intel DX4 Processor';
          else
            CpuName := 'Intel 486 Processor';
          end;
        5:
          CpuName := 'Pentium';
        6:
          case Model of
            1:
              CpuName := 'Pentium Pro';
            3:
              CpuName := 'Pentium II';
            5:
              case L2CacheSize of
                0:
                  CpuName := 'Celeron';
                1024:
                  CpuName := 'Pentium II Xeon';
                2048:
                  CpuName := 'Pentium II Xeon';
              else
                CpuName := 'Pentium II';
              end;
            6:
              case L2CacheSize of
                0:
                  CpuName := 'Celeron';
                128:
                  CpuName := 'Celeron';
              else
                CpuName := 'Pentium II';
              end;
            7:
              case L2CacheSize of
                1024:
                  CpuName := 'Pentium III Xeon';
                2048:
                  CpuName := 'Pentium III Xeon';
              else
                CpuName := 'Pentium III';
              end;
            8:
              case IntelSpecific.BrandID of
                1:
                  CpuName := 'Celeron';
                2:
                  CpuName := 'Pentium III';
                3:
                  CpuName := 'Pentium III Xeon';
                4:
                  CpuName := 'Pentium III';
              else
                CpuName := 'Pentium III';
              end;
            10:
              CpuName := 'Pentium III Xeon';
            11:
              CpuName := 'Pentium III';
          else
            StrPCopy(CpuName, Format('P6 (Model %d)', [Model]));
          end;
        15:
          case IntelSpecific.BrandID of
            1:
              CpuName := 'Celeron';
            8:
              CpuName := 'Pentium 4';
            14:
              CpuName := 'Xeon';
          else
            CpuName := 'Pentium 4';
          end;
      else
        StrPCopy(CpuName, Format('P%d', [Family]));
      end;
    end;

    MMX := (Features and MMX_FLAG) <> 0;
    if (Features and SSE_FLAG) <> 0 then
      if (Features and SSE2_FLAG) <> 0 then
        if (IntelSpecific.ExFeatures and EINTEL_SSE3) <> 0 then
          SSE := 3
        else
          SSE := 2
      else
        SSE := 1
    else
      SSE := 0;
    Is64Bits := HasExtendedInfo and ((IntelSpecific.Ex64Features and EINTEL64_EM64T)<>0);
  end;
 end;

// Helper function for CPUID. Initializes Cyrix specific fields.

procedure CyrixSpecific(var CpuInfo: TCpuInfo);
begin
  with CpuInfo do
  begin
    Manufacturer := 'Cyrix';
    if not HasExtendedInfo then
    begin
      case Family of
        4:
          CpuName := 'Cyrix MediaGX';
        5:
          case Model of
            2:
              CpuName := 'Cyrix 6x86';
            4:
              CpuName := 'Cyrix GXm';
          end;
        6:
          CpuName := '6x86MX';
      else
        StrPCopy(CpuName, Format('%dx86', [Family]));
      end;
    end;
  end;
end;

// Helper function for CPUID. Initializes AMD specific fields.

resourcestring
  RsUnknownAMDModel = 'Unknown AMD (Model %d)';
  
procedure AMDSpecific(var CpuInfo: TCpuInfo);
begin
  with CpuInfo do
  begin
    Manufacturer := 'AMD';
    if not HasExtendedInfo then
    begin
      case Family of
        4:
          CpuName := 'Am486(R) or Am5x86';
        5:
          case Model of
            0:
              CpuName := 'AMD-K5 (Model 0)';
            1:
              CpuName := 'AMD-K5 (Model 1)';
            2:
              CpuName := 'AMD-K5 (Model 2)';
            3:
              CpuName := 'AMD-K5 (Model 3)';
            6:
              CpuName := 'AMD-K6® (Model 6)';
            7:
              CpuName := 'AMD-K6® (Model 7)';
            8:
              CpuName := 'AMD-K6®-2 (Model 8)';
            9:
              CpuName := 'AMD-K6®-III (Model 9)';
            else
              StrFmt(CpuName,PChar(RsUnknownAMDModel),[Model]);
          end;
        6:
          case Model of
            1:
              CpuName := 'AMD Athlon™ (Model 1)';
            2:
              CpuName := 'AMD Athlon™ (Model 2)';
            3:
              CpuName := 'AMD Duron™ (Model 3)';
            4:
              CpuName := 'AMD Athlon™ (Model 4)';
            6:
              CpuName := 'AMD Athlon™ XP (Model 6)';
            7:
              CpuName := 'AMD Duron™ (Model 7)';
            8:
              CpuName := 'AMD Athlon™ XP (Model 8)';
            10:
              CpuName := 'AMD Athlon™ XP (Model 10)';
            else
              StrFmt(CpuName,PChar(RsUnknownAMDModel),[Model]);
          end;
        8:

        else
          CpuName := 'Unknown AMD Chip';
      end;
    end;
    if (HasCacheInfo) then
    begin
      L1DataCacheSize := AMDSpecific.L1DataCache[ciSize];
      L1DataCacheLineSize := AMDSpecific.L1DataCache[ciLineSize];
      L1DataCacheAssociativity := AMDSpecific.L1DataCache[ciAssociativity];
      L1InstructionCacheSize := AMDSpecific.L1InstructionCache[ciSize];
      L1InstructionCacheLineSize := AMDSpecific.L1InstructionCache[ciLineSize];
      L1InstructionCacheAssociativity := AMDSpecific.L1InstructionCache[ciAssociativity];
      L2CacheLineSize := AMDSpecific.L2Cache and $FF;
      L2CacheAssociativity := (AMDSpecific.L2Cache shr 12) and $F;
      L2CacheSize := AMDSpecific.L2Cache shr 16;
    end;
    MMX := (Features and AMD_MMX) <> 0;
    ExMMX := HasExtendedInfo and ((AMDSpecific.ExFeatures and EAMD_EXMMX) <> 0);
    _3DNow := HasExtendedInfo and ((AMDSpecific.ExFeatures and EAMD_3DNOW) <> 0);
    Ex3DNow := HasExtendedInfo and ((AMDSpecific.ExFeatures and EAMD_EX3DNOW) <> 0);
    if (Features and AMD_SSE) <> 0 then
      if (Features and AMD_SSE2) <> 0 then
        SSE := 2
      else
        SSE := 1
    else
      SSE := 0;
    Is64Bits := HasExtendedInfo and ((AMDSpecific.ExFeatures and EAMD_LONG) <> 0);
  end;
end;

// Helper function for CPUID. Initializes Transmeta specific fields.

procedure TransmetaSpecific(var CpuInfo: TCpuInfo);
begin
  with CpuInfo do
  begin
    Manufacturer := 'Transmeta';
    if not HasExtendedInfo then
      CpuName := 'Crusoe';
    if HasCacheInfo then
    begin
      L1DataCacheSize := TransmetaSpecific.L1DataCache[ciSize];
      L1DataCacheLineSize := TransmetaSpecific.L1DataCache[ciLineSize];
      L1DataCacheAssociativity := TransmetaSpecific.L1DataCache[ciAssociativity];
      L1InstructionCacheSize := TransmetaSpecific.L1CodeCache[ciSize];
      L1InstructionCacheLineSize := TransmetaSpecific.L1CodeCache[ciLineSize];
      L1InstructionCacheAssociativity := TransmetaSpecific.L1CodeCache[ciAssociativity];
      L2CacheLineSize := TransmetaSpecific.L2Cache and $FF;
      L2CacheAssociativity := (TransmetaSpecific.L2Cache shr 12) and $F;
      L2CacheSize := TransmetaSpecific.L2Cache shr 16;
    end;
    MMX := (Features and TRANSMETA_MMX) <> 0;
  end;
end;

// Helper function for CPUID. Initializes Via specific fields.

procedure ViaSpecific(var CpuInfo: TCpuInfo);
begin
  with CpuInfo do
  begin
    Manufacturer := 'Via';
    if not HasExtendedInfo then
      CpuName := 'C3';
    if HasCacheInfo then
    begin
      L1DataCacheSize := VIASpecific.L1DataCache[ciSize];
      L1DataCacheLineSize := VIASpecific.L1DataCache[ciLineSize];
      L1DataCacheAssociativity := VIASpecific.L1DataCache[ciAssociativity];
      L1InstructionCacheSize := VIASpecific.L1InstructionCache[ciSize];
      L1InstructionCacheLineSize := VIASpecific.L1InstructionCache[ciLineSize];
      L1InstructionCacheAssociativity := VIASpecific.L1InstructionCache[ciAssociativity];
      L2CacheLineSize := VIASpecific.L2DataCache and $FF;
      L2CacheAssociativity := (VIASpecific.L2DataCache shr 12) and $F;
      L2CacheSize := VIASpecific.L2DataCache shr 16;
    end;
    MMX := (Features and VIA_MMX) <> 0;
    if (Features and VIA_SSE) <> 0
      then SSE := 1
      else SSE := 0;
    _3DNow := (Features and VIA_3DNOW) <> 0;
  end;
end;

function CPUID: TCpuInfo;
var
  CPUInfo: TCpuInfo;
  HiVal: Cardinal;
  ExHiVal: Cardinal;
  TimesToExecute, CurrentLoop: Byte;
begin
  FillChar(CPUInfo, sizeof(CPUInfo), 0);
  asm
        PUSH    EAX
        PUSH    EBP
        PUSH    EBX
        PUSH    ECX                
        PUSH    EDI
        PUSH    EDX
        PUSH    ESI
{$IFDEF PIC} // position independent code for linux
        MOV     ESI, EBX       // get the GOT placed in ebx
{$ELSE}      // PIC
        XOR     ESI, ESI
{$ENDIF}     // PIC

  @@Check80486:
        MOV     [CPUInfo.Family], 4
        PUSHFD
        POP     EAX
        MOV     ECX, EAX
        XOR     EAX, 200000H
        PUSH    EAX
        POPFD
        PUSHFD
        POP     EAX
        XOR     EAX, ECX
        JE      @@DoneCpuType

  @@HasCPUIDInstruction:
        MOV     [CPUInfo.HasInstruction], 1
        MOV     EAX, 0
        DB      0FH
        DB      0A2H

        MOV     HiVal, EAX
        MOV     DWORD PTR [CPUInfo.VendorIDString], EBX
        MOV     DWORD PTR [CPUInfo.VendorIDString + 4], EDX
        MOV     DWORD PTR [CPUInfo.VendorIDString + 8], ECX

  @@CheckIntel:
        CMP     DWORD PTR [ESI].VendorIDIntel, EBX       //'uneG'
        JNE     @@CheckAMD
        CMP     DWORD PTR [ESI+4].VendorIDIntel, EDX     //'Ieni'
        JNE     @@CheckAMD
        CMP     DWORD PTR [ESI+8].VendorIDIntel, ECX     //'letn'
        JNE     @@CheckAMD
        MOV     [CPUInfo.CpuType], CPU_TYPE_INTEL
        JMP     @@CheckIntelExtended

  @@CheckAMD:
        CMP     DWORD PTR [ESI].VendorIDAMD, EBX         //'htuA'
        JNE     @@CheckCyrix
        CMP     DWORD PTR [ESI+4].VendorIDAMD, EDX       //'itne'
        JNE     @@CheckCyrix
        CMP     DWORD PTR [ESI+8].VendorIDAMD, ECX       //'DMAc'
        JNE     @@CheckCyrix
        MOV     [CPUInfo.CpuType], CPU_TYPE_AMD
        JMP     @@CheckAMDExtended

  @@CheckCyrix:
        CMP     DWORD PTR [ESI].VendorIDCyrix, EBX       //'iryC'
        JNE     @@CheckVIA
        CMP     DWORD PTR [ESI+4].VendorIDCyrix, EDX     //'snIx'
        JNE     @@CheckVIA
        CMP     DWORD PTR [ESI+8].VendorIDCyrix, ECX     //'daet'
        JNE     @@CheckVIA
        MOV     [CPUInfo.CpuType], CPU_TYPE_CYRIX
        JMP     @@CheckCyrixExtended

  @@CheckVIA:
        CMP     DWORD PTR [ESI].VendorIDVIA, EBX         //'tneC'
        JNE     @@CheckTransmeta
        CMP     DWORD PTR [ESI+4].VendorIDVIA, EDX       //'Hrua'
        JNE     @@CheckTransmeta
        CMP     DWORD PTR [ESI+8].VendorIDVIA, ECX       //'slua'
        JNE     @@CheckTransmeta
        MOV     [CPUInfo.CpuType], CPU_TYPE_VIA
        JMP     @@CheckVIAExtended

  @@CheckTransmeta:
        CMP     DWORD PTR [ESI].VendorIDTransmeta, EBX   //'uneG'
        JNE     @@StandardFunctions
        CMP     DWORD PTR [ESI+4].VendorIDTransmeta, EDX //'Teni'
        JNE     @@StandardFunctions
        CMP     DWORD PTR [ESI+8].VendorIDTransmeta, ECX //'68xM'
        JNE     @@StandardFunctions
        MOV     [CPUInfo.CpuType], CPU_TYPE_TRANSMETA
        JMP     @@CheckTransmetaExtended

  @@CheckIntelExtended:
        MOV     EAX, 80000000h
        DB      0Fh
        DB      0A2h
        TEST    EAX, 80000000h
        JZ      @@StandardFunctions
        JMP     @@IntelOnly

  @@CheckAMDExtended:
        MOV     EAX, 1
        CMP     HiVal, 1
        JL      @@StandardFunctions
        DB      0Fh
        DB      0A2h
        MOV     [CpuInfo.Features], EDX
        MOV     EAX, 80000000h
        DB      0Fh
        DB      0A2h
        CMP     EAX, 0
        JE      @@StandardFunctions
        JMP     @@AMDOnly

  @@CheckCyrixExtended:
        MOV     EAX, 80000000h
        DB      0Fh
        DB      0A2h
        CMP     EAX, 0
        JE      @@StandardFunctions
        JMP     @@CyrixOnly

  @@CheckVIAExtended:
        MOV     EAX, 80000000h
        DB      0Fh
        DB      0A2h
        CMP     EAX, 0
        JE      @@StandardFunctions
        JMP     @@VIAOnly

  @@CheckTransmetaExtended:
        JMP     @@TransmetaOnly

  @@StandardFunctions:
        CMP     HiVal, 1
        JL      @@DoneCPUType
        MOV     EAX, 1
        DB      0FH
        DB      0A2H
        MOV     [CPUInfo.Features], EDX
        MOV     [CPUInfo.IntelSpecific.BrandID], BL
        MOV     EBX, EAX
        AND     EAX, 3000H
        SHR     EAX, 12
        MOV     [CPUInfo.PType], AL
        MOV     EAX, EBX
        AND     EAX, 0F00H
        SHR     EAX, 8
        MOV     [CPUInfo.Family], AL
        MOV     EAX, EBX
        AND     EAX, 00F0H
        SHR     EAX, 4
        MOV     [CPUInfo.MODEL], AL
        MOV     EAX, EBX
        AND     EAX, 000FH
        MOV     [CPUInfo.Stepping], AL
        CMP     [CpuInfo.CpuType], CPU_TYPE_INTEL
        JNE     @@DoneCPUType
        MOV     [CPUInfo.IntelSpecific.ExFeatures], ECX  // (outchy) added extended features for intel processors
        
  @@IntelStandard:
        CMP     HiVal, 2
        JL      @@DoneCPUType
        MOV     CurrentLoop, 0
        MOV     [CPUInfo.HasCacheInfo], 1
        PUSH    ECX

  @@RepeatCacheQuery:
        POP     ECX
        MOV     EAX, 2
        DB      0FH
        DB      0A2H
        INC     CurrentLoop
        CMP     CurrentLoop, 1
        JNE     @@DoneCacheQuery
        MOV     TimesToExecute, AL
        CMP     AL, 0
        JE      @@DoneCPUType

  @@DoneCacheQuery:
        PUSH    ECX
        MOV     CL, CurrentLoop
        SUB     CL, TimesToExecute
        JNZ     @@RepeatCacheQuery
        POP     ECX
        MOV     DWORD PTR [CPUInfo.IntelSpecific.CacheDescriptors], EAX
        MOV     DWORD PTR [CPUInfo.IntelSpecific.CacheDescriptors + 4], EBX
        MOV     DWORD PTR [CPUInfo.IntelSpecific.CacheDescriptors + 8], ECX
        MOV     DWORD PTR [CPUInfo.IntelSpecific.CacheDescriptors + 12], EDX
        JMP     @@DoneCPUType

  @@IntelOnly:
        MOV     ExHiVal, EAX

        MOV     EAX, 80000001h
        CMP     ExHiVal, EAX
        JL      @@StandardFunctions
        MOV     [CPUInfo.HasExtendedInfo], 1
        DB      0Fh
        DB      0A2h
        MOV     [CPUInfo.IntelSpecific.Ex64Features], EDX

        MOV     EAX, 80000002h
        CMP     ExHiVal, EAX
        JL      @@StandardFunctions
        MOV     [CPUInfo.HasExtendedInfo], 1
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.CpuName], EAX
        MOV     DWORD PTR [CPUInfo.CpuName + 4], EBX
        MOV     DWORD PTR [CPUInfo.CpuName + 8], ECX
        MOV     DWORD PTR [CPUInfo.CpuName + 12], EDX

        MOV     EAX, 80000003h
        CMP     ExHiVal, EAX
        JL      @@StandardFunctions
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.CpuName + 16], EAX
        MOV     DWORD PTR [CPUInfo.CpuName + 20], EBX
        MOV     DWORD PTR [CPUInfo.CpuName + 24], ECX
        MOV     DWORD PTR [CPUInfo.CpuName + 28], EDX

        MOV     EAX, 80000004h
        CMP     ExHiVal, EAX
        JL      @@StandardFunctions
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.CpuName + 32], EAX
        MOV     DWORD PTR [CPUInfo.CpuName + 36], EBX
        MOV     DWORD PTR [CPUInfo.CpuName + 40], ECX
        MOV     DWORD PTR [CPUInfo.CpuName + 44], EDX

        MOV     EAX, 80000006h
        CMP     ExHiVal, EAX
        JL      @@StandardFunctions
        DB      0Fh
        DB      0A2h
        MOV     [CPUInfo.IntelSpecific.L2Cache], EDX
        JMP     @@StandardFunctions

  @@AMDOnly:
        MOV     ExHiVal, EAX
        MOV     EAX, 80000001h
        CMP     ExHiVal, EAX
        JL      @@DoneCPUType
        MOV     [CPUInfo.HasExtendedInfo], 1
        DB      0Fh
        DB      0A2h
        MOV     ECX, EAX
        //AND     EAX, 0F000H
        //SHR     EAX, 12
        //MOV     [CPUInfo.PType], AL        // (outchy) AMD processors don't support ProcessorType
        //MOV     EAX, ECX
        AND     EAX, 00000F00h
        SHR     EAX, 8
        MOV     [CPUInfo.Family], AL
        MOV     EAX, ECX
        AND     EAX, 0FF00000h
        SHR     EAX, 20
        MOV     [CpuInfo.ExtendedFamily], AL
        MOV     EAX, ECX
        AND     EAX, 000000F0h
        SHR     EAX, 4
        MOV     [CPUInfo.Model], AL
        MOV     EAX, ECX
        AND     EAX, 000F0000h
        SHR     EAX, 16
        MOV     [CpuInfo.ExtendedModel], AL
        MOV     EAX, ECX
        AND     EAX, 000FH
        MOV     [CPUInfo.Stepping], AL
        MOV     [CPUInfo.AMDSpecific.ExFeatures], EDX

        MOV     EAX, 80000002h
        CMP     ExHiVal, EAX
        JL      @@DoneCPUType
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.CpuName], EAX
        MOV     DWORD PTR [CPUInfo.CpuName + 4], EBX
        MOV     DWORD PTR [CPUInfo.CpuName + 8], ECX
        MOV     DWORD PTR [CPUInfo.CpuName + 12], EDX

        MOV     EAX, 80000003h
        CMP     ExHiVal, EAX
        JL      @@DoneCPUType
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.CpuName + 16], EAX
        MOV     DWORD PTR [CPUInfo.CpuName + 20], EBX
        MOV     DWORD PTR [CPUInfo.CpuName + 24], ECX
        MOV     DWORD PTR [CPUInfo.CpuName + 28], EDX

        MOV     EAX, 80000004h
        CMP     ExHiVal, EAX
        JL      @@DoneCPUType
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.CpuName + 32], EAX
        MOV     DWORD PTR [CPUInfo.CpuName + 36], EBX
        MOV     DWORD PTR [CPUInfo.CpuName + 40], ECX
        MOV     DWORD PTR [CPUInfo.CpuName + 44], EDX

        MOV     EAX, 80000005h
        CMP     ExHiVal, EAX
        JL      @@DoneCPUType
        MOV     [CPUInfo.HasCacheInfo], 1
        DB      0Fh
        DB      0A2h
        MOV     [CPUInfo.AMDSpecific.MByteInstructionTLB], AX
        SHR     EAX, 16
        MOV     [CPUInfo.AMDSpecific.MByteDataTLB], AX
        MOV     [CPUInfo.AMDSpecific.KByteInstructionTLB], BX
        SHR     EBX, 16
        MOV     [CPUInfo.AMDSpecific.KByteDataTLB], BX
        MOV     [CPUInfo.AMDSpecific.L1DataCache], ECX
        MOV     [CPUInfo.AMDSpecific.L1InstructionCache], EDX

        MOV     EAX, 80000006h
        CMP     ExHiVal, EAX
        JL      @@DoneCPUType
        DB      0Fh
        DB      0A2h
        MOV     [CpuInfo.AMDSpecific.L2MByteInstructionTLB], AX
        SHR     EAX, 16
        MOV     [CpuInfo.AMDSpecific.L2MByteDataTLB], AX
        MOV     [CpuInfo.AMDSpecific.L2KByteInstructionTLB], BX
        SHR     EBX, 16
        MOV     [CpuInfo.AMDSpecific.L2KByteDataTLB], BX
        MOV     [CpuInfo.AMDSpecific.L2Cache], ECX

        MOV     EAX, 80000007h
        CMP     ExHiVal, EAX
        JL      @@DoneCPUType
        DB      0Fh
        DB      0A2h
        MOV     [CpuInfo.AMDSpecific.AdvancedPowerManagement], EDX

        MOV     EAX, 80000008h
        CMP     ExHiVal, EAX
        JL      @@DoneCPUType
        DB      0Fh
        DB      0A2h
        MOV     [CPUInfo.AMDSpecific.PhysicalAddressSize], AL
        MOV     [CPUInfo.AMDSpecific.VirtualAddressSize], AH
        JMP     @@DoneCPUType

  @@CyrixOnly:
        MOV     ExHiVal, EAX
        MOV     EAX, 80000001h
        CMP     ExHiVal, EAX
        JL      @@DoneCPUType
        MOV     [CPUInfo.HasExtendedInfo], 1
        DB      0Fh
        DB      0A2h
        MOV     ECX, EAX
        AND     EAX, 0F000H
        SHR     EAX, 12
        MOV     [CPUInfo.PType], AL
        MOV     EAX, ECX
        AND     EAX, 0F00H
        SHR     EAX, 8
        MOV     [CPUInfo.Family], AL
        MOV     EAX, ECX
        AND     EAX, 00F0H
        SHR     EAX, 4
        MOV     [CPUInfo.Model], AL
        MOV     EAX, ECX
        AND     EAX, 000FH
        MOV     [CPUInfo.Stepping], AL
        MOV     [CPUInfo.Features], EDX

        MOV     EAX, 80000002h
        CMP     ExHiVal, EAX
        JL      @@DoneCPUType
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.CpuName], EAX
        MOV     DWORD PTR [CPUInfo.CpuName + 4], EBX
        MOV     DWORD PTR [CPUInfo.CpuName + 8], ECX
        MOV     DWORD PTR [CPUInfo.CpuName + 12], EDX

        MOV     EAX, 80000003h
        CMP     ExHiVal, EAX
        JL      @@DoneCPUType
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.CpuName + 16], EAX
        MOV     DWORD PTR [CPUInfo.CpuName + 20], EBX
        MOV     DWORD PTR [CPUInfo.CpuName + 24], ECX
        MOV     DWORD PTR [CPUInfo.CpuName + 28], EDX

        MOV     EAX, 80000004h
        CMP     ExHiVal, EAX
        JL      @@DoneCPUType
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.CpuName + 32], EAX
        MOV     DWORD PTR [CPUInfo.CpuName + 36], EBX
        MOV     DWORD PTR [CPUInfo.CpuName + 40], ECX
        MOV     DWORD PTR [CPUInfo.CpuName + 44], EDX

        MOV     EAX, 80000005h
        CMP     ExHiVal, EAX
        JL      @@DoneCPUType
        MOV     [CPUInfo.HasCacheInfo], 1
        DB      0Fh
        DB      0A2h
        MOV     [CPUInfo.CyrixSpecific.TLBInfo], EBX
        MOV     [CPUInfo.CyrixSpecific.L1CacheInfo], ECX
        JMP     @@DoneCPUType

  @@VIAOnly:
        MOV     ExHiVal, EAX
        MOV     EAX, 80000001h
        CMP     ExHiVal, EAX
        JL      @@VIAExtended
        MOV     [CPUInfo.HasExtendedInfo], 1
        DB      0Fh
        DB      0A2h
        MOV     [CpuInfo.Features], EDX
        MOV     ECX, EAX
        AND     EAX, 000Fh
        MOV     [CpuInfo.Stepping], AL
        MOV     EAX, ECX
        AND     EAX, 00F0h
        MOV     [CpuInfo.Model], AL
        MOV     EAX, ECX
        AND     EAX, 0F00h
        MOV     [CpuInfo.Family], AL
        MOV     EAX, ECX
        AND     EAX, 3000h
        MOV     [CpuInfo.Stepping], AL

        MOV     EAX, 80000002h
        CMP     ExHiVal, EAX
        JL      @@VIAExtended
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.CpuName], EAX
        MOV     DWORD PTR [CPUInfo.CpuName + 4], EBX
        MOV     DWORD PTR [CPUInfo.CpuName + 8], ECX
        MOV     DWORD PTR [CPUInfo.CpuName + 12], EDX

        MOV     EAX, 80000003h
        CMP     ExHiVal, EAX
        JL      @@VIAExtended
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.CpuName + 16], EAX
        MOV     DWORD PTR [CPUInfo.CpuName + 20], EBX
        MOV     DWORD PTR [CPUInfo.CpuName + 24], ECX
        MOV     DWORD PTR [CPUInfo.CpuName + 28], EDX

        MOV     EAX, 80000004h
        CMP     ExHiVal, EAX
        JL      @@VIAExtended
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.CpuName + 32], EAX
        MOV     DWORD PTR [CPUInfo.CpuName + 36], EBX
        MOV     DWORD PTR [CPUInfo.CpuName + 40], ECX
        MOV     DWORD PTR [CPUInfo.CpuName + 44], EDX

        MOV     EAX, 80000005h
        CMP     ExHiVal, EAX
        JL      @@VIAExtended
        DB      0Fh
        DB      0A2h

        MOV     [CPUInfo.VIASpecific.InstructionTLB], BX
        SHR     EBX, 16
        MOV     [CPUInfo.VIASpecific.DataTLB], BX
        MOV     [CPUInfo.VIASpecific.L1DataCache], ECX
        MOV     [CPUInfo.VIASpecific.L1InstructionCache], EDX
        MOV     [CPUInfo.HasCacheInfo], 1

        MOV     EAX, 80000006h
        CMP     ExHiVal, EAX
        JL      @@VIAExtended
        DB      0Fh
        DB      0A2h
        MOV     [CPUInfo.VIASpecific.L2DataCache], ECX

  @@VIAExtended:
        MOV     EAX, 0C0000000h
        DB      0Fh
        DB      0A2h
        CMP     EAX, 0
        JE      @@DoneCpuType
        MOV     ExHiVal, EAX

        MOV     EAX, 0C0000001h
        CMP     ExHiVal, EAX
        JL      @@DoneCpuType
        DB      0Fh
        DB      0A2h
        MOV     [CPUInfo.VIASpecific.ExFeatures], EDX
        JMP     @@DoneCpuType

  @@TransmetaOnly:
        MOV     EAX, 1
        CMP     HiVal, EAX
        JL      @@TransmetaExtended1
        DB      0Fh
        DB      0A2h
        MOV     [CpuInfo.Features], EDX
        MOV     EBX, EAX
        AND     EAX, 3000H
        SHR     EAX, 12
        MOV     [CPUInfo.PType], AL
        MOV     EAX, EBX
        AND     EAX, 0F00H
        SHR     EAX, 8
        MOV     [CPUInfo.Family], AL
        MOV     EAX, EBX
        AND     EAX, 00F0H
        SHR     EAX, 4
        MOV     [CPUInfo.MODEL], AL
        MOV     EAX, EBX
        AND     EAX, 000FH
        MOV     [CPUInfo.Stepping], AL
        // no information when eax is 2
        // eax is 3 means Serial Number, not detected there
  @@TransmetaExtended1:
        MOV     EAX, 80000000h
        DB      0Fh
        DB      0A2h
        CMP     EAX, 0
        JE      @@TransmetaExtended2
        MOV     ExHiVal, EAX
        MOV     [CPUInfo.HasExtendedInfo], 1
        MOV     DWORD PTR [CPUInfo.CpuName], EBX         // small CPU description, overriden if ExHiVal >=80000004
        MOV     DWORD PTR [CPUInfo.CpuName + 4], EDX
        MOV     DWORD PTR [CPUInfo.CpuName + 8], ECX

        MOV     EAX, 80000001h
        CMP     ExHiVal, EAX
        JL      @@TransmetaExtended2
        DB      0Fh
        DB      0A2h
        MOV     [CPUInfo.TransmetaSpecific.ExFeatures], EDX

        MOV     EAX, 80000002h
        CMP     ExHiVal, EAX
        JL      @@TransmetaExtended2
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.CpuName], EAX         // large CPU description
        MOV     DWORD PTR [CPUInfo.CpuName + 4], EBX
        MOV     DWORD PTR [CPUInfo.CpuName + 8], ECX
        MOV     DWORD PTR [CPUInfo.CpuName + 12], EDX

        MOV     EAX, 80000003h
        CMP     ExHiVal, EAX
        JL      @@TransmetaExtended2
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.CpuName + 16], EAX
        MOV     DWORD PTR [CPUInfo.CpuName + 20], EBX
        MOV     DWORD PTR [CPUInfo.CpuName + 24], ECX
        MOV     DWORD PTR [CPUInfo.CpuName + 28], EDX

        MOV     EAX, 80000004h
        CMP     ExHiVal, EAX
        JL      @@TransmetaExtended2
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.CpuName + 32], EAX
        MOV     DWORD PTR [CPUInfo.CpuName + 36], EBX
        MOV     DWORD PTR [CPUInfo.CpuName + 40], ECX
        MOV     DWORD PTR [CPUInfo.CpuName + 44], EDX

        MOV     EAX, 80000005h
        CMP     ExHiVal, EAX
        JL      @@TransmetaExtended2
        DB      0Fh
        DB      0A2h
        MOV     [CPUInfo.HasCacheInfo], 1
        MOV     [CPUInfo.TransmetaSpecific.CodeTLB], BX
        SHR     EBX, 16
        MOV     [CPUInfo.TransmetaSpecific.DataTLB], BX
        MOV     [CPUInfo.TransmetaSpecific.L1DataCache], ECX
        MOV     [CPUInfo.TransmetaSpecific.L1CodeCache], EDX

        MOV     EAX, 80000006h
        CMP     ExHiVal, EAX
        JL      @@TransmetaExtended2
        DB      0Fh
        DB      0A2h
        MOV     [CPUInfo.TransmetaSpecific.L2Cache], ECX

  @@TransmetaExtended2:
        MOV     EAX, 80860000h
        DB      0Fh
        DB      0A2h
        CMP     EAX, 0
        JE      @@DoneCPUType
        MOV     ExHiVal, EAX

        MOV     EAX, 80860001h
        CMP     ExHiVal, EAX
        JL      @@DoneCPUType
        DB      0Fh
        DB      0A2h
        MOV     [CPUInfo.TransmetaSpecific.RevisionABCD], EBX
        MOV     [CPUInfo.TransmetaSpecific.RevisionXXXX], ECX
        MOV     [CPUInfo.TransmetaSpecific.TransmetaFeatures], EDX

        MOV     EAX, 80860002h
        CMP     ExHiVal, EAX
        JL      @@DoneCPUType
        DB      0Fh
        DB      0A2h
        MOV     [CPUInfo.TransmetaSpecific.CodeMorphingABCD], EBX
        MOV     [CPUInfo.TransmetaSpecific.CodeMorphingXXXX], ECX

        MOV     EAX, 80860003h
        CMP     ExHiVal, EAX
        JL      @@DoneCPUType
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.TransmetaSpecific.TransmetaInformations], EAX
        MOV     DWORD PTR [CPUInfo.TransmetaSpecific.TransmetaInformations + 4], EBX
        MOV     DWORD PTR [CPUInfo.TransmetaSpecific.TransmetaInformations + 8], ECX
        MOV     DWORD PTR [CPUInfo.TransmetaSpecific.TransmetaInformations + 12], EDX

        MOV     EAX, 80860004h
        CMP     ExHiVal, EAX
        JL      @@DoneCPUType
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.TransmetaSpecific.TransmetaInformations + 16], EAX
        MOV     DWORD PTR [CPUInfo.TransmetaSpecific.TransmetaInformations + 20], EBX
        MOV     DWORD PTR [CPUInfo.TransmetaSpecific.TransmetaInformations + 24], ECX
        MOV     DWORD PTR [CPUInfo.TransmetaSpecific.TransmetaInformations + 28], EDX

        MOV     EAX, 80860005h
        CMP     ExHiVal, EAX
        JL      @@DoneCPUType
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.TransmetaSpecific.TransmetaInformations + 32], EAX
        MOV     DWORD PTR [CPUInfo.TransmetaSpecific.TransmetaInformations + 36], EBX
        MOV     DWORD PTR [CPUInfo.TransmetaSpecific.TransmetaInformations + 40], ECX
        MOV     DWORD PTR [CPUInfo.TransmetaSpecific.TransmetaInformations + 44], EDX

        MOV     EAX, 80860006h
        CMP     ExHiVal, EAX
        JL      @@DoneCPUType
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.TransmetaSpecific.TransmetaInformations + 48], EAX
        MOV     DWORD PTR [CPUInfo.TransmetaSpecific.TransmetaInformations + 52], EBX
        MOV     DWORD PTR [CPUInfo.TransmetaSpecific.TransmetaInformations + 56], ECX
        MOV     DWORD PTR [CPUInfo.TransmetaSpecific.TransmetaInformations + 60], EDX

        MOV     EAX, 80860007h
        CMP     ExHiVal, EAX
        JL      @@DoneCPUType
        MOV     EBX, [CPUInfo.TransmetaSpecific.TransmetaFeatures]
        TEST    EBX, STRANSMETA_LONGRUN
        JZ      @@DoneCPUType
        DB      0Fh
        DB      0A2h
        MOV     [CPUInfo.TransmetaSpecific.CurrentFrequency], EAX
        MOV     [CPUInfo.TransmetaSpecific.CurrentVoltage], EBX
        MOV     [CPUInfo.TransmetaSpecific.CurrentPerformance], ECX

  @@DoneCpuType:
        POP     ESI
        POP     EDX
        POP     EDI
        POP     ECX
        POP     EBX
        POP     EBP
        POP     EAX
  end;

  case CPUInfo.CpuType of
    CPU_TYPE_INTEL     : IntelSpecific(CpuInfo);
    CPU_TYPE_CYRIX     : CyrixSpecific(CpuInfo);
    CPU_TYPE_AMD       : AMDSpecific(CpuInfo);
    CPU_TYPE_TRANSMETA : TransmetaSpecific(CpuInfo);
    CPU_TYPE_VIA       : ViaSpecific(CpuInfo);
    else begin
      CpuInfo.Manufacturer := 'Unknown';
      CpuInfo.CpuName := 'Unknown';
    end;
  end;
  Result := CPUInfo;
end;

function TestFDIVInstruction: Boolean;
var
  TopNum: Double;
  BottomNum: Double;
  One: Double;
  ISOK: Boolean;
begin
  // The following code was found in Borlands fdiv.asm file in the
  // Delphi 3\Source\RTL\SYS directory, (I made some minor modifications)
  // therefore I cannot take credit for it.
  TopNum := 2658955;
  BottomNum := PI;
  One := 1;
  asm
        PUSH    EAX
        FLD     [TopNum]
        FDIV    [BottomNum]
        FMUL    [BottomNum]
        FSUBR   [TopNum]
        FCOMP   [One]
        FSTSW   AX
        SHR     EAX, 8
        AND     EAX, 01H
        MOV     ISOK, AL
        POP     EAX
  end;
  Result := ISOK;
end;

//=== Alloc granularity ======================================================

procedure RoundToAllocGranularity64(var Value: Int64; Up: Boolean);
begin
  if (Value mod AllocGranularity) <> 0 then
    if Up then
      Value := ((Value div AllocGranularity) + 1) * AllocGranularity
    else
      Value := (Value div AllocGranularity) * AllocGranularity;
end;

procedure RoundToAllocGranularityPtr(var Value: Pointer; Up: Boolean);
begin
  if (Cardinal(Value) mod AllocGranularity) <> 0 then
    if Up then
      Value := Pointer(((Cardinal(Value) div AllocGranularity) + 1) * AllocGranularity)
    else
      Value := Pointer((Cardinal(Value) div AllocGranularity) * AllocGranularity);
end;

//=== Advanced Power Management (APM) ========================================

{$IFDEF MSWINDOWS}
function GetAPMLineStatus: TAPMLineStatus;
var
  SystemPowerStatus: TSystemPowerStatus;
begin
  Result := alsUnknown;

  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion < 5) then // Windows NT doesn't support GetSystemPowerStatus
    Exit;                                                                     // so we return alsUnknown

  if not GetSystemPowerStatus(SystemPowerStatus) then
    RaiseLastOSError
  else
  begin
    case SystemPowerStatus.ACLineStatus  of
      0:
        Result := alsOffline;
      1:
        Result := alsOnline;
      255:
        Result := alsUnknown;
    end;
  end;
end;

function GetAPMBatteryFlag: TAPMBatteryFlag;
var
  SystemPowerStatus: TSystemPowerStatus;
begin
  Result := abfUnknown;

  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion < 5) then // Windows NT doesn't support GetSystemPowerStatus
    Exit;                                                                     // so we return abfUnknown

  if not GetSystemPowerStatus(SystemPowerStatus) then
    RaiseLastOSError
  else
  begin
    case SystemPowerStatus.BatteryFlag of
      1:
       Result := abfHigh;
      2:
        Result := abfLow;
      4:
        Result := abfCritical;
      8:
        Result := abfCharging;
      128:
        Result := abfNoBattery;
      255:
        Result := abfUnknown;
    end;
  end;
end;


function GetAPMBatteryFlags: TAPMBatteryFlags;
var
  SystemPowerStatus: TSystemPowerStatus;
begin
  Result := [];

  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion < 5) then // Windows NT doesn't support GetSystemPowerStatus
  begin
    Result := [abfUnknown];
    Exit;                                                                     // so we return [abfUnknown]
  end;

  if not GetSystemPowerStatus(SystemPowerStatus) then
    RaiseLastOSError
  else
  begin
    if (SystemPowerStatus.BatteryFlag and 1) <> 0 then
      Result := Result + [abfHigh];
    if (SystemPowerStatus.BatteryFlag and 2) <> 0 then
      Result := Result + [abfLow];
    if (SystemPowerStatus.BatteryFlag and 4) <> 0 then
      Result := Result + [abfCritical];
    if (SystemPowerStatus.BatteryFlag and 8) <> 0 then
      Result := Result + [abfCharging];
    if (SystemPowerStatus.BatteryFlag and 128) <> 0 then
      Result := Result + [abfNoBattery];
    if SystemPowerStatus.BatteryFlag = 255 then
      Result := Result + [abfUnknown];
  end;
end;

function GetAPMBatteryLifePercent: Integer;
var
  SystemPowerStatus: TSystemPowerStatus;
begin
  Result := 0;

  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion < 5) then // Windows NT doesn't support GetSystemPowerStatus
    Exit;

  if not GetSystemPowerStatus(SystemPowerStatus) then
    RaiseLastOSError
  else
    Result := SystemPowerStatus.BatteryLifePercent;
end;

function GetAPMBatteryLifeTime: DWORD;
var
  SystemPowerStatus: TSystemPowerStatus;
begin
  Result := 0;

  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion < 5) then // Windows NT doesn't support GetSystemPowerStatus
    Exit;

  if not GetSystemPowerStatus(SystemPowerStatus) then
    RaiseLastOSError
  else
    Result := SystemPowerStatus.BatteryLifeTime;
end;

function GetAPMBatteryFullLifeTime: DWORD;
var
  SystemPowerStatus: TSystemPowerStatus;
begin
  Result := 0;

  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion < 5) then // Windows NT doesn't support GetSystemPowerStatus
    Exit;

  if not GetSystemPowerStatus(SystemPowerStatus) then
    RaiseLastOSError
  else
    Result := SystemPowerStatus.BatteryFullLifeTime;
end;

//=== Memory Information =====================================================

function GetMaxAppAddress: Integer;
var
  SystemInfo: TSystemInfo;
begin
  FillChar(SystemInfo, SizeOf(SystemInfo), #0);
  GetSystemInfo(SystemInfo);
  Result := Integer(SystemInfo.lpMaximumApplicationAddress);
end;

function GetMinAppAddress: Integer;
var
  SystemInfo: TSystemInfo;
begin
  FillChar(SystemInfo, SizeOf(SystemInfo), #0);
  GetSystemInfo(SystemInfo);
  Result := Integer(SystemInfo.lpMinimumApplicationAddress);
end;
{$ENDIF MSWINDOWS}

function GetMemoryLoad: Byte;
{$IFDEF UNIX}
var
  SystemInf: TSysInfo ;
begin
  SysInfo(SystemInf);
  with SystemInf do
    Result := 100 - Round(100 * freeram / totalram);
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  Result := MemoryStatus.dwMemoryLoad;
end;
{$ENDIF MSWINDOWS}

function GetSwapFileSize: Integer;
{$IFDEF UNIX}
var
  SystemInf: TSysInfo;
begin
  SysInfo(SystemInf);
  Result := SystemInf.totalswap;
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  with MemoryStatus do
    Result := Trunc(dwTotalPageFile - dwAvailPageFile);
end;
{$ENDIF MSWINDOWS}

function GetSwapFileUsage: Integer;
{$IFDEF UNIX}
var
  SystemInf: TSysInfo;
begin
  SysInfo(SystemInf);
  with SystemInf do
    Result := 100 - Trunc(100 * FreeSwap / TotalSwap);
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  with MemoryStatus do
    if dwTotalPageFile > 0 then
      Result := 100 - Trunc(dwAvailPageFile / dwTotalPageFile * 100)
    else
      Result := 0;
end;
{$ENDIF MSWINDOWS}

function GetTotalPhysicalMemory: Integer;
{$IFDEF UNIX}
var
  SystemInf: TSysInfo;
begin
  SysInfo(SystemInf);
  Result := SystemInf.totalram;
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  Result := MemoryStatus.dwTotalPhys;
end;
{$ENDIF MSWINDOWS}

function GetFreePhysicalMemory: Integer;
{$IFDEF UNIX}
var
  SystemInf: TSysInfo;
begin
  SysInfo(SystemInf);
  Result := SystemInf.freeram;
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  Result := MemoryStatus.dwAvailPhys;
end;

function GetTotalPageFileMemory: Integer;
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  Result := MemoryStatus.dwTotalPageFile;
end;

function GetFreePageFileMemory: Integer;
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  Result := MemoryStatus.dwAvailPageFile;
end;

function GetTotalVirtualMemory: Integer;
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  Result := MemoryStatus.dwTotalVirtual;
end;

function GetFreeVirtualMemory: Integer;
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  Result := MemoryStatus.dwAvailVirtual;
end;

//=== Keyboard Information ===================================================

function GetKeybStateHelper(VirtualKey: Cardinal; Mask: Byte): Boolean;
var
  Keys: TKeyboardState;
begin
  Result := GetKeyBoardState(Keys) and (Keys[VirtualKey] and Mask <> 0);
end;

function GetKeyState(const VirtualKey: Cardinal): Boolean;
begin
  Result := GetKeybStateHelper(VirtualKey, $80);
end;

function GetNumLockKeyState: Boolean;
begin
  Result := GetKeybStateHelper(VK_NUMLOCK, $01);
end;

function GetScrollLockKeyState: Boolean;
begin
  Result := GetKeybStateHelper(VK_SCROLL, $01);
end;

function GetCapsLockKeyState: Boolean;
begin
  Result := GetKeybStateHelper(VK_CAPITAL, $01);
end;

//=== Windows 95/98/ME system resources information ==========================

{ TODO -oPJH : compare to Win9xFreeSysResources }
var
  ResmeterLibHandle: THandle;
  MyGetFreeSystemResources: function(ResType: UINT): UINT; stdcall;

procedure UnloadSystemResourcesMeterLib;
begin
  if ResmeterLibHandle <> 0 then
  begin
    FreeLibrary(ResmeterLibHandle);
    ResmeterLibHandle := 0;
    @MyGetFreeSystemResources := nil;
  end;
end;

function IsSystemResourcesMeterPresent: Boolean;

  procedure LoadResmeter;
  var
    OldErrorMode: UINT;
  begin
    OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
    try
      ResmeterLibHandle := LoadLibrary('rsrc32.dll');
    finally
      SetErrorMode(OldErrorMode);
    end;
    if ResmeterLibHandle <> 0 then
    begin
      @MyGetFreeSystemResources := GetProcAddress(ResmeterLibHandle, '_MyGetFreeSystemResources32@4');
      if not Assigned(MyGetFreeSystemResources) then
        UnloadSystemResourcesMeterLib;
    end;    
  end;

begin
  if not IsWinNT and (ResmeterLibHandle = 0) then
    LoadResmeter;
  Result := (ResmeterLibHandle <> 0);
end;

function GetFreeSystemResources(const ResourceType: TFreeSysResKind): Integer;
const
  ParamValues: array [TFreeSysResKind] of UINT = (0, 1, 2);
begin
  if IsSystemResourcesMeterPresent then
    Result := MyGetFreeSystemResources(ParamValues[ResourceType])
  else
    Result := -1;
end;

function GetFreeSystemResources: TFreeSystemResources;
begin
  with Result do
  begin
    SystemRes := GetFreeSystemResources(rtSystem);
    GdiRes := GetFreeSystemResources(rtGdi);
    UserRes := GetFreeSystemResources(rtUser);
  end;
end;

//=== Initialization/Finalization ============================================

procedure InitSysInfo;
var
  SystemInfo: TSystemInfo;
  Kernel32FileName: string;
  VerFixedFileInfo: TVSFixedFileInfo;
begin
  { processor information related initialization }

  FillChar(SystemInfo, SizeOf(SystemInfo), 0);
  GetSystemInfo(SystemInfo);
  ProcessorCount := SystemInfo.dwNumberOfProcessors;
  AllocGranularity := SystemInfo.dwAllocationGranularity;
  PageSize := SystemInfo.dwPageSize;

  { Windows version information }

  IsWinNT := Win32Platform = VER_PLATFORM_WIN32_NT;

  Kernel32FileName := GetModulePath(GetModuleHandle(kernel32));
  if (not IsWinNT) and VersionFixedFileInfo(Kernel32FileName, VerFixedFileInfo) then
    KernelVersionHi := VerFixedFileInfo.dwProductVersionMS
  else
    KernelVersionHi := 0;

  case GetWindowsVersion of
    wvUnknown:
      ;
    wvWin95:
      IsWin95 := True;
    wvWin95OSR2:
      IsWin95OSR2 := True;
    wvWin98:
      IsWin98 := True;
    wvWin98SE:
      IsWin98SE := True;
    wvWinME:
      IsWinME := True;
    wvWinNT31:
      begin
        IsWinNT3 := True;
        IsWinNT31 := True;
      end;
    wvWinNT35:
      begin
        IsWinNT3 := True;
        IsWinNT35 := True;
      end;
    wvWinNT351:
      begin
        IsWinNT3 := True;
        IsWinNT35 := True;
        IsWinNT351 := True;
      end;
    wvWinNT4:
      IsWinNT4 := True;
    wvWin2000:
      IsWin2K := True;
    wvWinXP:
      IsWinXP := True;
    wvWin2003:
      IsWin2003 := True;
  end;
end;

procedure FinalizeSysInfo;
begin
  UnloadSystemResourcesMeterLib;
end;

initialization
  InitSysInfo;

finalization
  FinalizeSysInfo;

{$ENDIF MSWINDOWS}
{$ENDIF ~CLR}

// History:

// $Log$
// Revision 1.43  2005/05/05 20:08:45  ahuser
// JCL.NET support
//
// Revision 1.42  2005/04/07 00:41:35  rrossmair
// - changed for FPC 1.9.8
//
// Revision 1.41  2005/03/12 01:32:50  outchy
// Update of the CPUID function. New processors detection, constants reworked and specifications upgraded.
//
// Revision 1.40  2005/03/08 08:33:17  marquardt
// overhaul of exceptions and resourcestrings, minor style cleaning
//
// Revision 1.39  2005/03/03 15:35:59  rikbarker
// Windows 2003 Fix for NTProductType and GetWindowsServicePackVersion
//
// Revision 1.38  2005/02/24 16:34:40  marquardt
// remove divider lines, add section lines (unfinished)
//
// Revision 1.37  2005/02/24 07:36:24  marquardt
// resolved the compiler warnings, style cleanup, removed code from JclContainerIntf.pas
//
// Revision 1.36  2005/02/20 04:37:09  rrossmair
// - added GetIPAddress() and GetIPAddresses() for Unix
//
// Revision 1.35  2004/12/19 20:16:31  rrossmair
// - added TCpuInfo improvements by Florent Ouchet
//
// Revision 1.34  2004/12/07 02:40:07  rrossmair
// - added GetVolumeFileSystemFlags function
//
// Revision 1.33  2004/10/21 08:40:10  marquardt
// style cleaning
//
// Revision 1.32  2004/10/17 23:48:22  mthoma
// Removed contributions... Reintroduced orignal GetOpenGLVersion.
//
// Revision 1.31  2004/10/17 20:25:21  mthoma
// style cleaning, adjusting contributors
//
// Revision 1.30  2004/10/10 12:52:12  marquardt
// DestroyEnvironmentBlock introduced
//
// Revision 1.29  2004/08/04 09:05:51  marquardt
// forgot to export SetGlobalEnvironmentVariable
//
// Revision 1.28  2004/08/04 06:11:49  marquardt
// added SetGlobalEnvironmentVariable
//
// Revision 1.27  2004/08/03 07:22:37  marquardt
// resourcestring cleanup
//
// Revision 1.26  2004/07/31 06:21:01  marquardt
// fixing TStringLists, adding BeginUpdate/EndUpdate, finalization improved
//
// Revision 1.25  2004/07/28 18:00:51  marquardt
// various style cleanings, some minor fixes
//
// Revision 1.24  2004/07/16 04:11:46  rrossmair
// fixed RunningProcessesList for Win2003
//
// Revision 1.23  2004/06/16 07:30:28  marquardt
// added tilde to all IFNDEF ENDIFs, inherited qualified
//
// Revision 1.22  2004/06/14 13:05:18  marquardt
// style cleaning ENDIF, Tabs
//
// Revision 1.21  2004/06/14 06:24:52  marquardt
// style cleaning IFDEF
//
// Revision 1.20  2004/06/02 03:23:46  rrossmair
// cosmetic changes in several units (code formatting, help TODOs processed etc.)
//
// Revision 1.19  2004/05/08 08:44:17  rrossmair
// introduced & applied symbol HAS_UNIT_LIBC
//
// Revision 1.18  2004/05/05 07:12:03  rrossmair
// changes for FPC compatibility
//
// Revision 1.17  2004/05/05 00:15:12  mthoma
// Updated headers: Added donors as contributors, adjusted the initial authors, added cvs names when they were not obvious. Changed $data to $date where necessary,
//
// Windows NT 4 and earlier do not support GetSystemPowerStatus. Modified the APM function accordingly.
//
// Revision 1.16  2004/04/19 06:14:43  rrossmair
// Help TODOs done
//
// Revision 1.15  2004/04/18 19:57:29
// - rename one of the GetOpenGLVersion to GetOpenGLVersionBitmapRendering
// - delete pre-loading of Glu32Handle
// - move the OpenGl32Handle call to directly before ChoosePixelFormat
//
// Revision 1.14  2004/04/18 05:14:11  rrossmair
// fixed GetOpenGLVersion (draw to bitmap overload); removed VCL dependency ("uses Graphics")
//
// Revision 1.13  2004/04/18 00:43:19
// modify und bugfix GetOpenGLVersion, add second function for bitmap rendering
//
// Revision 1.12  2004/04/09 15:05:09  mthoma
// Added new function GetAPMBatteryFlags.
//
// Revision 1.11  2004/04/07 13:55:09  peter3
// - var params cannot be passed by adress
//
// Revision 1.10  2004/04/07 07:33:39  marquardt
// fixes for GetVersionEx
//
// Revision 1.9  2004/04/06 04:53:18
// adapt compiler conditions, add log entry
//

end.
