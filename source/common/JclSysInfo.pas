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
{   James Azarja                                                                                   }
{   Jean-Fabien Connault                                                                           }
{   John C Molyneux                                                                                }
{   Marcel van Brakel                                                                              }
{   Matthias Thoma (mthoma)                                                                        }
{   Mike Lischke                                                                                   }
{   Nick Hodges                                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Peter Friese                                                                                   }
{   Peter J. Haas (peterjhaas)                                                                     }
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
  {$IFDEF MSWINDOWS}
  Windows,
  {$IFNDEF FPC}
  ShlObj,
  {$ENDIF ~FPC}
  {$ENDIF MSWINDOWS}
  Classes,
  JclResources;

//--------------------------------------------------------------------------------------------------
// Environment Variables
//--------------------------------------------------------------------------------------------------
{$IFDEF MSWINDOWS}
type
  TEnvironmentOption = (eoLocalMachine, eoCurrentUser, eoAdditional);
  TEnvironmentOptions = set of TEnvironmentOption;
{$ENDIF MSWINDOWS}

function DelEnvironmentVar(const Name: string): Boolean;
function ExpandEnvironmentVar(var Value: string): Boolean;
function GetEnvironmentVar(const Name: string; var Value: string): Boolean; overload;
function GetEnvironmentVar(const Name: string; var Value: string; Expand: Boolean): Boolean; overload;
function GetEnvironmentVars(const Vars: TStrings): Boolean; overload;
function GetEnvironmentVars(const Vars: TStrings; Expand: Boolean): Boolean; overload;
function SetEnvironmentVar(const Name, Value: string): Boolean;
{$IFDEF MSWINDOWS}
function CreateEnvironmentBlock(const Options: TEnvironmentOptions; const AdditionalVars: TStrings): PChar;
{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------
// Common Folder Locations
//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
function GetCommonFilesFolder: string;
{$ENDIF MSWINDOWS}
function GetCurrentFolder: string;
{$IFDEF MSWINDOWS}
function GetProgramFilesFolder: string;
function GetWindowsFolder: string;
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
function GetNethoodFolder: string;
function GetFontsFolder: string;
function GetCommonStartmenuFolder: string;
function GetCommonProgramsFolder: string;
function GetCommonStartupFolder: string;
function GetCommonDesktopdirectoryFolder: string;
function GetCommonAppdataFolder: string;
function GetAppdataFolder: string;
function GetPrinthoodFolder: string;
function GetCommonFavoritesFolder: string;
function GetTemplatesFolder: string;
function GetInternetCacheFolder: string;
function GetCookiesFolder: string;
function GetHistoryFolder: string;
function GetProfileFolder: string;

//--------------------------------------------------------------------------------------------------
// Advanced Power Management (APM)
//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------
// Identification
//--------------------------------------------------------------------------------------------------

function GetVolumeName(const Drive: string): string;
function GetVolumeSerialNumber(const Drive: string): string;
function GetVolumeFileSystem(const Drive: string): string;
function GetIPAddress(const HostName: string): string;
{$ENDIF MSWINDOWS}
function GetLocalComputerName: string;
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

//--------------------------------------------------------------------------------------------------
// Processes, Tasks and Modules
//--------------------------------------------------------------------------------------------------

type
  TJclTerminateAppResult = (taError, taClean, taKill);

function RunningProcessesList(const List: TStrings; FullPath: Boolean = True): Boolean;

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
function TerminateApp(ProcessID: DWORD; Timeout: Integer): TJclTerminateAppResult;

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

//--------------------------------------------------------------------------------------------------
// Version Information
//--------------------------------------------------------------------------------------------------

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
function GetOpenGLVersion(Win: HWND; out Version, Vendor: AnsiString): Boolean;
function GetOpenGLVersionBitmapRendering(out Version, Vendor: AnsiString): Boolean;
{$ENDIF MSWINDOWS}

function GetOSVersionString: string;

//--------------------------------------------------------------------------------------------------
// Hardware
//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
function GetMacAddresses(const Machine: string; const Addresses: TStrings): Integer;
{$ENDIF MSWINDOWS}
function ReadTimeStampCounter: Int64;

type
  TIntelSpecific = record
    L2Cache: Cardinal;
    CacheDescriptors: array [0..15] of Byte;
    BrandID : Byte;
  end;

  TCyrixSpecific = record
    L1CacheInfo: array [0..3] of Byte;
    TLBInfo: array [0..3] of Byte;
  end;

  TAMDSpecific = record
    DataTLB: array [0..1] of Byte;
    InstructionTLB: array [0..1] of Byte;
    L1DataCache: array [0..3] of Byte;
    L1ICache: array [0..3] of Byte;
  end;

  TCacheInfo = record
    D: Byte;
    I: string;
  end;

  TFreqInfo = record
    RawFreq: Cardinal;
    NormFreq: Cardinal;
    InCycles: Cardinal;
    ExTicks: Cardinal;
  end;

  TCpuInfo = record
    HasInstruction: Boolean;
    MMX: Boolean;
    IsFDIVOK: Boolean;
    HasCacheInfo: Boolean;
    HasExtendedInfo: Boolean;
    CpuType: Byte;
    PType: Byte;
    Family: Byte;
    Model: Byte;
    Stepping: Byte;
    Features: Cardinal;
    FrequencyInfo: TFreqInfo;
    VendorIDString: array [0..11] of Char;
    Manufacturer: array [0..9] of Char;
    CpuName: array [0..47] of Char;
    IntelSpecific: TIntelSpecific;
    CyrixSpecific: TCyrixSpecific;
    AMDSpecific: TAMDSpecific;
  end;

const
  CPU_TYPE_INTEL  = 1;
  CPU_TYPE_CYRIX  = 2;
  CPU_TYPE_AMD    = 3;
  CPU_TYPE_CRUSOE = 4;

// Constants to be used with Feature Flag set of a CPU
// eg. IF (Features and FPU_FLAG = FPU_FLAG) THEN CPU has Floating-Point unit on
// chip. However, Intel claims that in future models, a zero in the feature
// flags will mean that the chip has that feature, however, the following flags
// will work for any production 80x86 chip or clone.
// eg. IF (Features and FPU_FLAG = 0) then CPU has Floating-Point unit on chip.

const

{ Standard (Intel) Feature Flags }

  FPU_FLAG   = $00000001; // Floating-Point unit on chip
  VME_FLAG   = $00000002; // Virtual Mode Extention
  DE_FLAG    = $00000004; // Debugging Extention
  PSE_FLAG   = $00000008; // Page Size Extention
  TSC_FLAG   = $00000010; // Time Stamp Counter
  MSR_FLAG   = $00000020; // Model Specific Registers
  PAE_FLAG   = $00000040; // Physical Address Extention
  MCE_FLAG   = $00000080; // Machine Check Exception
  CX8_FLAG   = $00000100; // CMPXCHG8 Instruction
  APIC_FLAG  = $00000200; // Software-accessible local APIC on Chip
  BIT_10     = $00000400; // Reserved, do not count on value
  SEP_FLAG   = $00000800; // Fast System Call
  MTRR_FLAG  = $00001000; // Memory Type Range Registers
  PGE_FLAG   = $00002000; // Page Global Enable
  MCA_FLAG   = $00004000; // Machine Check Architecture
  CMOV_FLAG  = $00008000; // Conditional Move Instruction
  PAT_FLAG   = $00010000; // Page Attribute Table
  PSE36_FLAG = $00020000; // 36-bit Page Size Extention
  BIT_18     = $00040000; // Reserved, do not count on value
  BIT_19     = $00080000; // Reserved, do not count on value
  BIT_20     = $00100000; // Reserved, do not count on value
  BIT_21     = $00200000; // Reserved, do not count on value
  BIT_22     = $00400000; // Reserved, do not count on value
  MMX_FLAG   = $00800000; // MMX technology
  FXSR_FLAG  = $01000000; // Fast Floating Point Save and Restore
  BIT_25     = $02000000; // Reserved, do not count on value
  BIT_26     = $04000000; // Reserved, do not count on value
  BIT_27     = $08000000; // Reserved, do not count on value
  BIT_28     = $10000000; // Reserved, do not count on value
  BIT_29     = $20000000; // Reserved, do not count on value
  BIT_30     = $40000000; // Reserved, do not count on value
  BIT_31     = DWORD($80000000); // Reserved, do not count on value

{ AMD Standard Feature Flags }

  AMD_FPU_FLAG   = $00000001; // Floating-Point unit on chip
  AMD_VME_FLAG   = $00000002; // Virtual Mode Extention
  AMD_DE_FLAG    = $00000004; // Debugging Extention
  AMD_PSE_FLAG   = $00000008; // Page Size Extention
  AMD_TSC_FLAG   = $00000010; // Time Stamp Counter
  AMD_MSR_FLAG   = $00000020; // Model Specific Registers
  AMD_BIT_6      = $00000040; // Reserved, do not count on value
  AMD_MCE_FLAG   = $00000080; // Machine Check Exception
  AMD_CX8_FLAG   = $00000100; // CMPXCHG8 Instruction
  AMD_APIC_FLAG  = $00000200; // Software-accessible local APIC on Chip
  AMD_BIT_10     = $00000400; // Reserved, do not count on value
  AMD_BIT_11     = $00000800; // Reserved, do not count on value
  AMD_MTRR_FLAG  = $00001000; // Memory Type Range Registers
  AMD_PGE_FLAG   = $00002000; // Page Global Enable
  AMD_BIT_14     = $00004000; // Reserved, do not count on value
  AMD_CMOV_FLAG  = $00008000; // Conditional Move Instruction
  AMD_BIT_16     = $00010000; // Reserved, do not count on value
  AMD_BIT_17     = $00020000; // Reserved, do not count on value
  AMD_BIT_18     = $00040000; // Reserved, do not count on value
  AMD_BIT_19     = $00080000; // Reserved, do not count on value
  AMD_BIT_20     = $00100000; // Reserved, do not count on value
  AMD_BIT_21     = $00200000; // Reserved, do not count on value
  AMD_BIT_22     = $00400000; // Reserved, do not count on value
  AMD_MMX_FLAG   = $00800000; // MMX technology
  AMD_BIT_24     = $01000000; // Reserved, do not count on value
  AMD_BIT_25     = $02000000; // Reserved, do not count on value
  AMD_BIT_26     = $04000000; // Reserved, do not count on value
  AMD_BIT_27     = $08000000; // Reserved, do not count on value
  AMD_BIT_28     = $10000000; // Reserved, do not count on value
  AMD_BIT_29     = $20000000; // Reserved, do not count on value
  AMD_BIT_30     = $40000000; // Reserved, do not count on value
  AMD_BIT_31     = DWORD($80000000); // Reserved, do not count on value

{ AMD Enhanced Feature Flags }

  EAMD_FPU_FLAG   = $00000001; // Floating-Point unit on chip
  EAMD_VME_FLAG   = $00000002; // Virtual Mode Extention
  EAMD_DE_FLAG    = $00000004; // Debugging Extention
  EAMD_PSE_FLAG   = $00000008; // Page Size Extention
  EAMD_TSC_FLAG   = $00000010; // Time Stamp Counter
  EAMD_MSR_FLAG   = $00000020; // Model Specific Registers
  EAMD_BIT_6      = $00000040; // Reserved, do not count on value
  EAMD_MCE_FLAG   = $00000080; // Machine Check Exception
  EAMD_CX8_FLAG   = $00000100; // CMPXCHG8 Instruction
  EAMD_BIT_9      = $00000200; // Reserved, do not count on value
  EAMD_BIT_10     = $00000400; // Reserved, do not count on value
  EAMD_SEP_FLAG   = $00000800; // Fast System Call
  EAMD_BIT_12     = $00001000; // Reserved, do not count on value
  EAMD_PGE_FLAG   = $00002000; // Page Global Enable
  EAMD_BIT_14     = $00004000; // Reserved, do not count on value
  EAMD_ICMOV_FLAG = $00008000; // Integer Conditional Move Instruction
  EAMD_FCMOV_FLAG = $00010000; // Floating Point Conditional Move Instruction
  EAMD_BIT_17     = $00020000; // Reserved, do not count on value
  EAMD_BIT_18     = $00040000; // Reserved, do not count on value
  EAMD_BIT_19     = $00080000; // Reserved, do not count on value
  EAMD_BIT_20     = $00100000; // Reserved, do not count on value
  EAMD_BIT_21     = $00200000; // Reserved, do not count on value
  EAMD_BIT_22     = $00400000; // Reserved, do not count on value
  EAMD_MMX_FLAG   = $00800000; // MMX technology
  EAMD_BIT_24     = $01000000; // Reserved, do not count on value
  EAMD_BIT_25     = $02000000; // Reserved, do not count on value
  EAMD_BIT_26     = $04000000; // Reserved, do not count on value
  EAMD_BIT_27     = $08000000; // Reserved, do not count on value
  EAMD_BIT_28     = $10000000; // Reserved, do not count on value
  EAMD_BIT_29     = $20000000; // Reserved, do not count on value
  EAMD_BIT_30     = $40000000; // Reserved, do not count on value
  EAMD_3DNOW_FLAG = DWORD($80000000); // AMD 3DNOW! Technology

{ Cyrix Standard Feature Flags }

  CYRIX_FPU_FLAG   = $00000001; // Floating-Point unit on chip
  CYRIX_VME_FLAG   = $00000002; // Virtual Mode Extention
  CYRIX_DE_FLAG    = $00000004; // Debugging Extention
  CYRIX_PSE_FLAG   = $00000008; // Page Size Extention
  CYRIX_TSC_FLAG   = $00000010; // Time Stamp Counter
  CYRIX_MSR_FLAG   = $00000020; // Model Specific Registers
  CYRIX_PAE_FLAG   = $00000040; // Physical Address Extention
  CYRIX_MCE_FLAG   = $00000080; // Machine Check Exception
  CYRIX_CX8_FLAG   = $00000100; // CMPXCHG8 Instruction
  CYRIX_APIC_FLAG  = $00000200; // Software-accessible local APIC on Chip
  CYRIX_BIT_10     = $00000400; // Reserved, do not count on value
  CYRIX_BIT_11     = $00000800; // Reserved, do not count on value
  CYRIX_MTRR_FLAG  = $00001000; // Memory Type Range Registers
  CYRIX_PGE_FLAG   = $00002000; // Page Global Enable
  CYRIX_MCA_FLAG   = $00004000; // Machine Check Architecture
  CYRIX_CMOV_FLAG  = $00008000; // Conditional Move Instruction
  CYRIX_BIT_16     = $00010000; // Reserved, do not count on value
  CYRIX_BIT_17     = $00020000; // Reserved, do not count on value
  CYRIX_BIT_18     = $00040000; // Reserved, do not count on value
  CYRIX_BIT_19     = $00080000; // Reserved, do not count on value
  CYRIX_BIT_20     = $00100000; // Reserved, do not count on value
  CYRIX_BIT_21     = $00200000; // Reserved, do not count on value
  CYRIX_BIT_22     = $00400000; // Reserved, do not count on value
  CYRIX_MMX_FLAG   = $00800000; // MMX technology
  CYRIX_BIT_24     = $01000000; // Reserved, do not count on value
  CYRIX_BIT_25     = $02000000; // Reserved, do not count on value
  CYRIX_BIT_26     = $04000000; // Reserved, do not count on value
  CYRIX_BIT_27     = $08000000; // Reserved, do not count on value
  CYRIX_BIT_28     = $10000000; // Reserved, do not count on value
  CYRIX_BIT_29     = $20000000; // Reserved, do not count on value
  CYRIX_BIT_30     = $40000000; // Reserved, do not count on value
  CYRIX_BIT_31     = DWORD($80000000); // Reserved, do not count on value

{ Cyrix Enhanced Feature Flags }

  ECYRIX_FPU_FLAG   = $00000001; // Floating-Point unit on chip
  ECYRIX_VME_FLAG   = $00000002; // Virtual Mode Extention
  ECYRIX_DE_FLAG    = $00000004; // Debugging Extention
  ECYRIX_PSE_FLAG   = $00000008; // Page Size Extention
  ECYRIX_TSC_FLAG   = $00000010; // Time Stamp Counter
  ECYRIX_MSR_FLAG   = $00000020; // Model Specific Registers
  ECYRIX_PAE_FLAG   = $00000040; // Physical Address Extention
  ECYRIX_MCE_FLAG   = $00000080; // Machine Check Exception
  ECYRIX_CX8_FLAG   = $00000100; // CMPXCHG8 Instruction
  ECYRIX_APIC_FLAG  = $00000200; // Software-accessible local APIC on Chip
  ECYRIX_SEP_FLAG   = $00000400; // Fast System Call
  ECYRIX_BIT_11     = $00000800; // Reserved, do not count on value
  ECYRIX_MTRR_FLAG  = $00001000; // Memory Type Range Registers
  ECYRIX_PGE_FLAG   = $00002000; // Page Global Enable
  ECYRIX_MCA_FLAG   = $00004000; // Machine Check Architecture
  ECYRIX_ICMOV_FLAG = $00008000; // Integer Conditional Move Instruction
  ECYRIX_FCMOV_FLAG = $00010000; // Floating Point Conditional Move Instruction
  ECYRIX_BIT_17     = $00020000; // Reserved, do not count on value
  ECYRIX_BIT_18     = $00040000; // Reserved, do not count on value
  ECYRIX_BIT_19     = $00080000; // Reserved, do not count on value
  ECYRIX_BIT_20     = $00100000; // Reserved, do not count on value
  ECYRIX_BIT_21     = $00200000; // Reserved, do not count on value
  ECYRIX_BIT_22     = $00400000; // Reserved, do not count on value
  ECYRIX_MMX_FLAG   = $00800000; // MMX technology
  ECYRIX_EMMX_FLAG  = $01000000; // Extended MMX Technology
  ECYRIX_BIT_25     = $02000000; // Reserved, do not count on value
  ECYRIX_BIT_26     = $04000000; // Reserved, do not count on value
  ECYRIX_BIT_27     = $08000000; // Reserved, do not count on value
  ECYRIX_BIT_28     = $10000000; // Reserved, do not count on value
  ECYRIX_BIT_29     = $20000000; // Reserved, do not count on value
  ECYRIX_BIT_30     = $40000000; // Reserved, do not count on value
  ECYRIX_BIT_31     = DWORD($80000000); // Reserved, do not count on value

const
  IntelCacheDescription: array [0..13] of TCacheInfo = (
    (D: $01; I: RsIntelCacheDescr01),
    (D: $02; I: RsIntelCacheDescr02),
    (D: $03; I: RsIntelCacheDescr03),
    (D: $04; I: RsIntelCacheDescr04),
    (D: $06; I: RsIntelCacheDescr06),
    (D: $08; I: RsIntelCacheDescr08),
    (D: $0A; I: RsIntelCacheDescr0A),
    (D: $0C; I: RsIntelCacheDescr0C),
    (D: $40; I: RsIntelCacheDescr40),
    (D: $41; I: RsIntelCacheDescr41),
    (D: $42; I: RsIntelCacheDescr42),
    (D: $43; I: RsIntelCacheDescr43),
    (D: $44; I: RsIntelCacheDescr44),
    (D: $45; I: RsIntelCacheDescr45));

procedure GetCpuInfo(var CpuInfo: TCpuInfo);

function GetIntelCacheDescription(const D: Byte): string;
function RoundFrequency(const Frequency: Integer): Integer;
{$IFDEF MSWINDOWS}
function GetCPUSpeed(var CpuSpeed: TFreqInfo): Boolean;
{$ENDIF MSWINDOWS}
function CPUID: TCpuInfo;
function TestFDIVInstruction: Boolean;

//--------------------------------------------------------------------------------------------------
// Memory Information
//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------
// Alloc granularity
//--------------------------------------------------------------------------------------------------

procedure RoundToAllocGranularity64(var Value: Int64; Up: Boolean);
procedure RoundToAllocGranularityPtr(var Value: Pointer; Up: Boolean);

{$IFDEF MSWINDOWS}
//--------------------------------------------------------------------------------------------------
// Keyboard Information
//--------------------------------------------------------------------------------------------------

function GetKeyState(const VirtualKey: Cardinal): Boolean;
function GetNumLockKeyState: Boolean;
function GetScrollLockKeyState: Boolean;
function GetCapsLockKeyState: Boolean;

//--------------------------------------------------------------------------------------------------
// Windows 95/98/Me system resources information
//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------
// Public global variables
//--------------------------------------------------------------------------------------------------

var
  ProcessorCount: Cardinal = 0;
  AllocGranularity: Cardinal = 0;
  PageSize: Cardinal = 0;

implementation

uses
  SysUtils,
  {$IFDEF MSWINDOWS}
  Messages, Winsock, Snmp,
  JclRegistry, JclWin32,
  {$IFDEF FPC}
  ActiveX,
  JwaTlHelp32, JwaPsApi,
  {$ELSE}
  TLHelp32, PsApi,
  JclShell,
  {$ENDIF FPC}
  {$ENDIF MSWINDOWS}
  Jcl8087, JclBase, JclFileUtils, JclIniFiles, JclStrings;

{$IFDEF FPC}
{$I JclSysInfo.fpc}
{$ENDIF FPC}

//==================================================================================================
// Environment
//==================================================================================================

function DelEnvironmentVar(const Name: string): Boolean;
begin
  {$IFDEF UNIX}
  UnSetEnv(PChar(Name));
  Result := True;
  {$ENDIF UNIX}
  {$IFDEF MSWINDOWS}
  Result := SetEnvironmentVariable(PChar(Name), nil);
  {$ENDIF MSWINDOWS}
end;

//--------------------------------------------------------------------------------------------------

function ExpandEnvironmentVar(var Value: string): Boolean;
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

//--------------------------------------------------------------------------------------------------

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
  Result := GetEnvironmentVar(Name, Value, True);
end;

function GetEnvironmentVar(const Name: string; var Value: string; Expand: Boolean): Boolean;
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

{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------

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

{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------

function SetEnvironmentVar(const Name, Value: string): Boolean;
begin
  {$IFDEF UNIX}
  SetEnv(PChar(Name), PChar(Value), 1);
  Result := True;
  {$ENDIF UNIX}
  {$IFDEF MSWINDOWS}
  Result := SetEnvironmentVariable(PChar(Name), PChar(Value));
  {$ENDIF MSWINDOWS}
end;

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

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

//==================================================================================================
// Common Folders
//==================================================================================================

// Utility function which returns the Windows independent CurrentVersion key
// inside HKEY_LOCAL_MACHINE

const
  HKLM_CURRENT_VERSION_WINDOWS = 'Software\Microsoft\Windows\CurrentVersion';
  HKLM_CURRENT_VERSION_NT      = 'Software\Microsoft\Windows NT\CurrentVersion';

function REG_CURRENT_VERSION: string;
begin
  if IsWinNT then
    Result := HKLM_CURRENT_VERSION_NT
  else
    Result := HKLM_CURRENT_VERSION_WINDOWS;
end;

//--------------------------------------------------------------------------------------------------

{ TODO : Check for documented solution }
function GetCommonFilesFolder: string;
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, HKLM_CURRENT_VERSION_WINDOWS,
    'CommonFilesDir', '');
end;

{$ENDIF MSWINDOWS}
//--------------------------------------------------------------------------------------------------

function GetCurrentFolder: string;
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

{$IFDEF MSWINDOWS}
//--------------------------------------------------------------------------------------------------

{ TODO : Check for documented solution }
function GetProgramFilesFolder: string;
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, HKLM_CURRENT_VERSION_WINDOWS, 'ProgramFilesDir', '');
end;

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

{ TODO : Check for documented solution }
function GetWindowsSystemFolder: string;
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

//--------------------------------------------------------------------------------------------------

function GetWindowsTempFolder: string;
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

//--------------------------------------------------------------------------------------------------

function GetDesktopFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_DESKTOP);
end;

//--------------------------------------------------------------------------------------------------

{ TODO : Check GetProgramsFolder = GetProgramFilesFolder }
function GetProgramsFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_PROGRAMS);
end;

{$ENDIF MSWINDOWS}
//--------------------------------------------------------------------------------------------------

function GetPersonalFolder: string;
begin
  {$IFDEF UNIX}
  Result := GetEnvironmentVariable('HOME');
  {$ENDIF UNIX}
  {$IFDEF MSWINDOWS}
  Result := GetSpecialFolderLocation(CSIDL_PERSONAL);
  {$ENDIF MSWINDOWS}
end;

{$IFDEF MSWINDOWS}
//--------------------------------------------------------------------------------------------------

function GetFavoritesFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_FAVORITES);
end;

//--------------------------------------------------------------------------------------------------

function GetStartupFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_STARTUP);
end;

//--------------------------------------------------------------------------------------------------

function GetRecentFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_RECENT);
end;

//--------------------------------------------------------------------------------------------------

function GetSendToFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_SENDTO);
end;

//--------------------------------------------------------------------------------------------------

function GetStartmenuFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_STARTMENU);
end;

//--------------------------------------------------------------------------------------------------

function GetDesktopDirectoryFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_DESKTOPDIRECTORY);
end;

//--------------------------------------------------------------------------------------------------

function GetNethoodFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_NETHOOD);
end;

//--------------------------------------------------------------------------------------------------

function GetFontsFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_FONTS);
end;

//--------------------------------------------------------------------------------------------------

function GetCommonStartmenuFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_COMMON_STARTMENU);
end;

//--------------------------------------------------------------------------------------------------

function GetCommonProgramsFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_COMMON_PROGRAMS);
end;

//--------------------------------------------------------------------------------------------------

function GetCommonStartupFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_COMMON_STARTUP);
end;

//--------------------------------------------------------------------------------------------------

function GetCommonDesktopdirectoryFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_COMMON_DESKTOPDIRECTORY);
end;

//--------------------------------------------------------------------------------------------------

function GetCommonAppdataFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_COMMON_APPDATA);
end;

//--------------------------------------------------------------------------------------------------

function GetAppdataFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_APPDATA);
end;

//--------------------------------------------------------------------------------------------------

function GetPrinthoodFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_PRINTHOOD);
end;

//--------------------------------------------------------------------------------------------------

function GetCommonFavoritesFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_COMMON_FAVORITES);
end;

//--------------------------------------------------------------------------------------------------

function GetTemplatesFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_TEMPLATES);
end;

//--------------------------------------------------------------------------------------------------

function GetInternetCacheFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_INTERNET_CACHE);
end;

//--------------------------------------------------------------------------------------------------

function GetCookiesFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_COOKIES);
end;

//--------------------------------------------------------------------------------------------------

function GetHistoryFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_HISTORY);
end;

//--------------------------------------------------------------------------------------------------

function GetProfileFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_PROFILE);
end;  

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

//==================================================================================================
// Identification
//==================================================================================================

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

//--------------------------------------------------------------------------------------------------

function GetVolumeName(const Drive: string): string;
begin
  Result := GetVolumeInfoHelper(Drive, vikName);
end;

//--------------------------------------------------------------------------------------------------

function GetVolumeSerialNumber(const Drive: string): string;
begin
  Result := GetVolumeInfoHelper(Drive, vikSerial);
end;

//--------------------------------------------------------------------------------------------------

function GetVolumeFileSystem(const Drive: string): string;
begin
  Result := GetVolumeInfoHelper(Drive, vikFileSystem);
end;

//--------------------------------------------------------------------------------------------------

function GetIPAddress(const HostName: string): string;
var
  R: Integer;
  WSAData: TWSAData;
  HostEnt: PHostEnt;
  Host: string;
  SockAddr: TSockAddrIn;
begin
  Result := '';
  R := WSAStartup(MakeWord(1, 1), WSAData);
  if R = 0 then
  try
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
  finally
    WSACleanup;
  end;
end;

{$ENDIF MSWINDOWS}
//--------------------------------------------------------------------------------------------------

function GetLocalComputerName: string;
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

//--------------------------------------------------------------------------------------------------

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
//--------------------------------------------------------------------------------------------------

function GetRegisteredCompany: string;
begin
  { TODO : check for MSDN documentation }
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, REG_CURRENT_VERSION, 'RegisteredOrganization', '');
end;

//--------------------------------------------------------------------------------------------------

function GetRegisteredOwner: string;
begin
  { TODO : check for MSDN documentation }
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, REG_CURRENT_VERSION, 'RegisteredOwner', '');
end;

//--------------------------------------------------------------------------------------------------

{ TODO : Several functions that use the SECURITY_DESCRIPTOR structure require
  that this structure be on a valid pointer boundary in memory. These boundaries
  vary depending on the type of processor used. Memory allocation functions,
  such as malloc and LocalAlloc, return properly aligned pointers. }

// At least under Win98 SE LookupAccountName return always 0 and
// GetLastError = ERROR_CALL_NOT_IMPLEMENTED
{ TODO : Move to JclSecurity? }
{ TODO : Maybe a other, Win9x compatible solution }
function GetUserDomainName(const CurUser: string): string;
var
  Count1, Count2: DWORD;
  Sd: PSID; // PSecurityDescriptor; // FPC requires PSID
  Snu: SID_Name_Use;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
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
  end
  else
    Result := '';  // Win9x/ME
end;

{$ENDIF MSWINDOWS}
//--------------------------------------------------------------------------------------------------

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
//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

// Reference: How to Obtain BIOS Information from the Registry
// http://support.microsoft.com/default.aspx?scid=kb;en-us;q195268

{ TODO : the date string can be e.g. 00/00/00 }
function GetBIOSDate: TDateTime;
const
  WinNT_REG_PATH = '\HARDWARE\DESCRIPTION\System';
  WinNT_REG_KEY  = 'SystemBiosDate';
  Win9x_REG_PATH = '\Enum\Root\*PNP0C01\0000';
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

//==================================================================================================
// Processes, Tasks and Modules
//==================================================================================================

{$IFDEF UNIX}
const
  CommLen = 16;  // synchronize with size of comm in struct task_struct in
                 //     /usr/include/linux/sched.h
  SProcDirectory = '/proc';

resourcestring
  RsInvalidProcessID = 'Invalid process id %d';

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

{$IFDEF MSWINDOWS}

function RunningProcessesList(const List: TStrings; FullPath: Boolean): Boolean;

  // under Win9x this function always return ''
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
          Result := '';  // always valid for Win9x
      end
      else
      begin
        if GetModuleBaseNameA(Handle, 0, PChar(Result), MAX_PATH) > 0 then
          StrResetLength(Result)
        else
          Result := '';  // always valid for Win9x
      end;
    finally
      CloseHandle(Handle);
    end;
  end;

  // under WinNT this function always return False
  function BuildListTH: Boolean;
  var
    SnapProcHandle: THandle;
    ProcEntry: TProcessEntry32;
    NextProc: Boolean;
    FileName: string;
  begin
    SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    // INVALID_HANDLE_VALUE, if the function failed,
    // 0 for WinNT (bad design in TLHelp32.pas)
    Result := (SnapProcHandle <> INVALID_HANDLE_VALUE) and (SnapProcHandle <> 0);
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

  // under Win9x this function always return False
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

//--------------------------------------------------------------------------------------------------

{ TODO -cTest : Extremely likely this function don't work under Win9x }
{ TODO -cHelp : Extremely likely this function don't work under Win9x }
function LoadedModulesList(const List: TStrings; ProcessID: DWORD; HandlesOnly: Boolean): Boolean;

  // this function don't work under Win9x
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

  // this function don't work under Win9x, because it call AddToList
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

  // this function don't work under Win9x, because it call AddToList
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

  // under WinNT this function always return False
  function EnumModulesTH: Boolean;
  var
    SnapProcHandle: THandle;
    Module: TModuleEntry32;
    Next: Boolean;
  begin
    SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, ProcessID);
    // INVALID_HANDLE_VALUE, if the function failed,
    // 0 for WinNT (bad design in TLHelp32.pas)
    Result := (SnapProcHandle <> INVALID_HANDLE_VALUE) and (SnapProcHandle <> 0);
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

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

function IsWindowResponding(Wnd: HWND; Timeout: Integer): Boolean;
var
  Res: DWORD;
begin
  Result := SendMessageTimeout(Wnd, WM_NULL, 0, 0, SMTO_ABORTIFHUNG, Timeout, Res) <> 0;
end;

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

function TerminateTask(Wnd: HWND; Timeout: Integer): TJclTerminateAppResult;
var
  PID: DWORD;
begin
  if GetWindowThreadProcessId(Wnd, @PID) <> 0 then
    Result := TerminateApp(PID, Timeout)
  else
    Result := taError;  
end;

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

function GetShellProcessName: string;
const
  cShellKey = 'Software\Microsoft\Windows NT\CurrentVersion\WinLogon';
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

//--------------------------------------------------------------------------------------------------

function GetShellProcessHandle: THandle;
var
  Pid: Longword;
begin
  Pid := GetPidFromProcessName(GetShellProcessName);
  Result := OpenProcess(PROCESS_ALL_ACCESS, False, Pid);
  if Result = 0 then
    RaiseLastOSError;
end;

//==================================================================================================
// Version Information
//==================================================================================================

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

//--------------------------------------------------------------------------------------------------

function NtProductType: TNtProductType;
const
  ProductType = 'System\CurrentControlSet\Control\ProductOptions';
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
  if IsWin2K then
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
      if (VersionInfo.wProductType = VER_NT_WORKSTATION) then
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

//--------------------------------------------------------------------------------------------------

function GetWindowsVersionString: string;
const
  OSVersionStrings: array [Succ(Low(TWindowsVersion))..High(TWindowsVersion)] of string = (
    RsOSVersionWin95, RsOSVersionWin95OSR2, RsOSVersionWin98, RsOSVersionWin98SE,
    RsOSVersionWinME, RsOSVersionWinNT3, RsOSVersionWinNT3, RsOSVersionWinNT3,
    RsOSVersionWinNT4, RsOSVersionWin2000, RsOSVersionWinXP, RsOSVersionWin2003);
var
  WindowsVersion: TWindowsVersion;
begin
  WindowsVersion := GetWindowsVersion;
  case WindowsVersion of
    Low(OSVersionStrings)..High(OSVersionStrings):
      Result := Format(OSVersionStrings[WindowsVersion], [Win32MinorVersion]);
  else
    Result := '';
  end;
end;

//--------------------------------------------------------------------------------------------------

function NtProductTypeString: string;
const
  NtProductTypeStrings: array [Succ(Low(TNtProductType))..High(TNtProductType)] of string = (
    RsProductTypeWorkStation, RsProductTypeServer, RsProductTypeAdvancedServer,
    RsProductTypePersonal, RsProductTypeProfessional, RsProductTypeDatacenterServer);
var
  ProductType: TNtProductType;
begin
  ProductType := NtProductType;
  case ProductType of
    Low(NtProductTypeStrings)..High(NtProductTypeStrings):
      Result := NtProductTypeStrings[ProductType];
  else
    Result := '';
  end;
end;

//--------------------------------------------------------------------------------------------------

function GetWindowsServicePackVersion: Integer;
const
  RegWindowsControl = '\SYSTEM\CurrentControlSet\Control\Windows\';
var
  SP: Integer;
  VersionInfo: TOSVersionInfoEx;
begin
  Result := 0;
  if IsWin2K or IsWinXP then
  begin
    FillChar(VersionInfo, SizeOf(VersionInfo), 0);
    VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
    if GetVersionEx(VersionInfo) then Result := VersionInfo.wServicePackMajor;
  end
  else
  begin
    SP := RegReadIntegerDef(HKEY_LOCAL_MACHINE, RegWindowsControl, 'CSDVersion', 0);
    Result := StrToInt(IntToHex(SP, 4)) div 100;
  end;
end;

//--------------------------------------------------------------------------------------------------

function GetWindowsServicePackVersionString: string;
var
  SP: Integer;
begin
  SP := GetWindowsServicePackVersion;
  if SP > 0 then
    Result := 'SP' + IntToStr(SP)      { TODO : localize, that mean ResourceString? }
  else
    Result := '';
end;

//--------------------------------------------------------------------------------------------------

function InternalGetOpenGLVersion(out Version, Vendor: AnsiString; DCHandle: HDC;
  Flags: DWORD): Boolean;

function GetOpenGlString(Name: Cardinal; out Value: AnsiString): Boolean;
var
  Ptr: PAnsiChar;
begin
  Ptr := RtdlglGetString(Name);
  Result := Assigned(Ptr);
  if Result then
    Value := Ptr
  else
    { TODO : in case of missing dll empty string }
    Value := RtdlgluErrorString(RtdlglGetError);
end;

var
  Save8087CW: Word;
  PFDesc: TPixelFormatDescriptor;
  FormatIndex: Integer;
  RenderingContextHandle: HGLRC;
begin
  Result := False;
  Version := RsOpenGLInfoError;
  Vendor := RsOpenGLInfoError;
  if DCHandle = 0 then
    Exit;

  // To call for the version information string we must first have an active
  // context established for use.  We can, of course, close this after use
  Save8087CW := Set8087ControlWord($133F);
  try
    // We need to load the OpenGl32 library before calling ChoosePixelFormat
    OpenGl32Handle;

    FillChar(PFDesc, SizeOf(PFDesc), 0);
    with PFDesc do
    begin
      nSize := SizeOf(PFDesc);
      nVersion := 1;               // The Current Version of the descriptor is 1
      dwFlags := Flags or PFD_SUPPORT_OPENGL;
      iPixelType := PFD_TYPE_RGBA;
      cColorBits := 24;            // support 24-bit colour
      cDepthBits := 32;            // Depth of the z-buffer
      iLayerType := PFD_MAIN_PLANE;
    end;

    FormatIndex := ChoosePixelFormat(DCHandle, @PFDesc);
    if FormatIndex = 0 then
      RaiseLastOSError;
    if not SetPixelFormat(DCHandle, FormatIndex, @PFDesc) then
      RaiseLastOSError;

    RenderingContextHandle := RtdlwglCreateContext(DCHandle);
    if RenderingContextHandle = 0 then
      RaiseLastOSError;
    try
      if not RtdlwglMakeCurrent(DCHandle, RenderingContextHandle) then
        RaiseLastOSError;

      { TODO : Review the following.  Not sure I am 100% happy with this code
               in its current structure. }
      { TODO : Store this information in a Global Variable, and return that??
               This would save this work being performed again with later calls }
      Result := GetOpenGlString(GL_VERSION, Version) and GetOpenGlString(GL_VENDOR, Vendor);
    finally
      // Close all resources
      RtdlwglMakeCurrent(DCHandle, 0);
      if RenderingContextHandle <> 0 then
        RtdlwglDeleteContext(RenderingContextHandle);
    end;
  finally
    Set8087ControlWord(Save8087CW);
  end;
end;

function GetOpenGLVersionBitmapRendering(out Version, Vendor: AnsiString): Boolean;
var
  BmpInfoHdr: TBitmapInfoHeader;
  DCHandle: HDC;
  BmpHandle, OldBitmapHandle: HBitmap;
  Bits: Pointer;
begin
  FillChar(BmpInfoHdr, SizeOf(BmpInfoHdr), 0);
  BmpInfoHdr.biSize := SizeOf(BmpInfoHdr);
  BmpInfoHdr.biWidth := 1;
  BmpInfoHdr.biHeight := 1;
  BmpInfoHdr.biPlanes := 1;
  BmpInfoHdr.biBitCount := 24;
  BmpInfoHdr.biCompression := BI_RGB;
  BmpInfoHdr.biSizeImage := 0;
  DCHandle := CreateCompatibleDC(0);
  try
    BmpHandle := CreateDIBSection(DCHandle, PBitmapInfo(@BmpInfoHdr)^, DIB_RGB_COLORS, Bits, 0, 0);
    OldBitmapHandle := SelectObject(DCHandle, BmpHandle);
    try
      Result := InternalGetOpenGLVersion(Version, Vendor, DCHandle, PFD_DRAW_TO_BITMAP);
    finally
      SelectObject(DCHandle, OldBitmapHandle);
      DeleteObject(BmpHandle);
    end;
  finally
    ReleaseDC(0, DCHandle);
  end;
end;

function GetOpenGLVersion(Win: HWND; out Version, Vendor: AnsiString): Boolean;
var
  DCHandle: HDC;
begin
  Result := False;
  Version := RsOpenGLInfoError;
  Vendor := RsOpenGLInfoError;
  if Win = 0 then
    Exit;
  DCHandle := GetDC(Win);
  try
    Result := InternalGetOpenGLVersion(Version, Vendor, DCHandle, PFD_DRAW_TO_WINDOW);
  finally
    ReleaseDC(Win, DCHandle);
    // Redraw window
    InvalidateRect(Win, nil, False);
  end;
end;
{$ENDIF MSWINDOWS}

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

//==================================================================================================
// Hardware
//==================================================================================================

// Helper function for GetMacAddress()
// Converts the adapter_address array to a string

function AdapterToString(Adapter: PByteArray): string;
begin
  Result := Format('%2.2x-%2.2x-%2.2x-%2.2x-%2.2x-%2.2x', [
    Integer(Adapter[0]), Integer(Adapter[1]),
    Integer(Adapter[2]), Integer(Adapter[3]),
    Integer(Adapter[4]), Integer(Adapter[5])]);
end;

//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
function GetMacAddresses(const Machine: string; const Addresses: TStrings): Integer;

  procedure GetMacAddressesNetBios;
  // Platform SDK
  // http://msdn.microsoft.com/library/default.asp?url=/library/en-us/netbios/netbios_1l82.asp

  // Microsoft Knowledge Base Article - 118623
  // HOWTO: Get the MAC Address for an Ethernet Adapter
  // http://support.microsoft.com/default.aspx?scid=kb;en-us;118623
  type
    TAStat = packed record
      adapt: TAdapterStatus;
      NameBuff: array [0..29] of TNameBuffer;
    end;
  var
    NCB: TNCB;
    Enum: TLanaEnum;
    I, L, NameLen: Integer;
    Adapter: TAStat;
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
    if RtdlNetBios(@NCB) = NRC_GOODRET then
    begin
      Result := Enum.Length;
      for I := 0 to Ord(Enum.Length) - 1 do
      begin
        FillChar(NCB, SizeOf(NCB), #0);
        NCB.ncb_command := NCBRESET;
        NCB.ncb_lana_num := Enum.lana[I];
        if RtdlNetBios(@NCB) = NRC_GOODRET then
        begin
          FillChar(NCB, SizeOf(NCB), #0);
          NCB.ncb_command := NCBASTAT;
          NCB.ncb_lana_num := Enum.lana[I];
          Move(MachineName[1], NCB.ncb_callname, SizeOf(NCB.ncb_callname));
          NCB.ncb_buffer := PChar(@Adapter);
          NCB.ncb_length := SizeOf(Adapter);
          if RtdlNetBios(@NCB) = NRC_GOODRET then
            Addresses.Add(AdapterToString(@Adapter.adapt));
        end;
      end;
    end;
  end;

  procedure GetMacAddressesSnmp;
  const
    InetMib1 = 'inetmib1.dll';
    DunAdapterAddress: array [0..4] of Byte = ($44, $45, $53, $54, $00);
    NullAdapterAddress: array [0..5] of Byte = ($00, $00, $00, $00, $00, $00);
    OID_ipMACEntAddr: array [0..9] of UINT = (1, 3, 6, 1, 2, 1, 2, 2, 1, 6);
    OID_ifEntryType: array [0..9] of UINT = (1, 3, 6, 1, 2, 1, 2, 2, 1, 3);
    OID_ifEntryNum: array [0..7] of UINT = (1, 3, 6, 1, 2, 1, 2, 1);
  var
    PollForTrapEvent: THandle;
    SupportedView: PAsnObjectIdentifier;
    MIB_ifMACEntAddr: TAsnObjectIdentifier;
    MIB_ifEntryType: TAsnObjectIdentifier;
    MIB_ifEntryNum: TAsnObjectIdentifier;
    varBindList: TSnmpVarBindList;
    varBind: array [0..1] of TSnmpVarBind;
    ErrorStatus, ErrorIndex: TAsnInteger32;
    Dtmp: Integer;
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
          varBindList.list := @varBind[0];
          varBind[0].name := DEFINE_NULLOID;
          varBind[1].name := DEFINE_NULLOID;
          varBindList.len := 1;
          SnmpUtilOidCpy(@varBind[0].name, @MIB_ifEntryNum);
          Ret := SnmpExtensionQuery(SNMP_PDU_GETNEXT, varBindList, ErrorStatus, ErrorIndex);
          if Ret then
          begin
            Result := varBind[0].value.number;
            varBindList.len := 2;
            SnmpUtilOidCpy(@varBind[0].name, @MIB_ifEntryType);
            SnmpUtilOidCpy(@varBind[1].name, @MIB_ifMACEntAddr);
            while Ret do
            begin
              Ret := SnmpExtensionQuery(SNMP_PDU_GETNEXT, varBindList, ErrorStatus, ErrorIndex);
              if Ret then
              begin
                Ret := SnmpUtilOidNCmp(@varBind[0].name, @MIB_ifEntryType, MIB_ifEntryType.idLength) = SNMP_ERRORSTATUS_NOERROR;
                if Ret then
                begin
                  Dtmp := varBind[0].value.number;
                  if Dtmp = 6 then
                  begin
                    Ret := SnmpUtilOidNCmp(@varBind[1].name, @MIB_ifMACEntAddr, MIB_ifMACEntAddr.idLength) = SNMP_ERRORSTATUS_NOERROR;
                    if Ret and (varBind[1].value.address.stream <> nil) then
                    begin
                      MAC := PByteArray(varBind[1].value.address.stream);
                      if not CompareMem(MAC, @NullAdapterAddress, SizeOf(NullAdapterAddress)) then
                        Addresses.Add(AdapterToString(MAC));
                    end;
                  end;
                end;
              end;
            end;
          end;
          SnmpUtilVarBindFree(@varBind[0]);
          SnmpUtilVarBindFree(@varBind[1]);
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
//--------------------------------------------------------------------------------------------------

function ReadTimeStampCounter: Int64; assembler;
asm
        DW      $310F
end;

//--------------------------------------------------------------------------------------------------

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
end;

//--------------------------------------------------------------------------------------------------

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
    CpuInfo.MMX := (CpuInfo.Features and MMX_FLAG) = MMX_FLAG;
  end;
end;

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

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
    while ((Tries < 3 ) or ((Tries < 20) and ((Abs(3 * Freq - Total) > 3) or
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

//--------------------------------------------------------------------------------------------------

// Helper function for CPUID. Initializes Intel specific fields.

procedure IntelSpecific(var CpuInfo: TCpuInfo);
var
  I: Integer;
begin
  with CpuInfo do
  begin
    Manufacturer := 'Intel';
    CpuType := CPU_TYPE_INTEL;
    if HasCacheInfo then
      with CPUInfo.IntelSpecific do
      begin
        L2Cache := 0;
        for I := 1 to 15 do
          case CacheDescriptors[I] of
            $40:
              L2Cache := 0;
            $41:
              L2Cache := 128;
            $42:
              L2Cache := 256;
            $43:
              L2Cache := 512;
            $44:
              L2Cache := 1024;
            $45:
              L2Cache := 2048;
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
              case IntelSpecific.L2Cache of
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
              case IntelSpecific.L2Cache of
                0:
                  CpuName := 'Celeron';
                128:
                  CpuName := 'Celeron';
              else
                CpuName := 'Pentium II';
              end;
            7:
              case IntelSpecific.L2Cache of
                1024:
                  CpuName := 'Pentium III Xeon';
                2048:
                  CpuName := 'Pentium III Xeon';
              else
                CpuName := 'Pentium III';
              end;
            8:
              case IntelSpecific.BrandID of
                1: CpuName := 'Celeron';
                2: CpuName := 'Pentium III';
                3: CpuName := 'Pentium III Xeon';
                4: CpuName := 'Pentium III';
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
  end;
end;

//--------------------------------------------------------------------------------------------------

// Helper function for CPUID. Initializes Cyrix specific fields.

procedure CyrixSpecific(var CpuInfo: TCpuInfo);
begin
  with CpuInfo do
  begin
    Manufacturer := 'Cyrix';
    CpuType := CPU_TYPE_CYRIX;
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

//--------------------------------------------------------------------------------------------------

// Helper function for CPUID. Initializes AMD specific fields.

procedure AMDSpecific(var CpuInfo: TCpuInfo);
begin
  with CpuInfo do
  begin
    Manufacturer := 'AMD';
    CpuType := CPU_TYPE_AMD;
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
              CpuName := 'AMD-K6(R)';
            7:
              CpuName := 'AMD-K6';
            8:
              CpuName := 'AMD-K6(R) -2';
            9:
              CpuName := 'AMD-K6(R) -3';
          else
            CpuName := 'Unknown AMD Model';
          end;
      else
        CpuName := 'Unknown AMD Chip';
      end;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TransmetaSpecific(var CpuInfo: TCpuInfo);
begin
  with CpuInfo do
  begin
    Manufacturer := 'Transmeta';
    CpuType := CPU_TYPE_CRUSOE;
    CpuName := 'Crusoe';
  end;
end;
  
//--------------------------------------------------------------------------------------------------

function CPUID: TCpuInfo;
var
  CPUInfo: TCpuInfo;
  HiVal: Cardinal;
  TimesToExecute, CurrentLoop: Byte;
begin
  asm
        MOV     [CPUInfo.HasInstruction], 0
        MOV     [CPUInfo.HasExtendedInfo], 0
        MOV     [CPUInfo.HasCacheInfo], 0
        MOV     [CPUInfo.PType], 0
        MOV     [CPUInfo.Model], 0
        MOV     [CPUInfo.Stepping], 0
        MOV     [CPUInfo.Features], 0
        MOV     [CPUInfo.FrequencyInfo.RawFreq], 0
        MOV     [CPUInfo.FrequencyInfo.NormFreq], 0
        MOV     [CPUInfo.FrequencyInfo.InCycles], 0
        MOV     [CPUInfo.FrequencyInfo.ExTicks], 0
        MOV     [CPUInfo.IntelSpecific.BrandID],0

        PUSH    EAX
        PUSH    EBP
        PUSH    EBX
        PUSH    ECX
        PUSH    EDI
        PUSH    EDX
        PUSH    ESI

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
        CMP     DWORD PTR [CPUInfo.VendorIDString], 'uneG'
        JNE     @@CheckAMD
        CMP     DWORD PTR [CPUInfo.VendorIDString + 4], 'Ieni'
        JNE     @@CheckAMD
        CMP     DWORD PTR [CPUInfo.VendorIDString + 8], 'letn'
        JNE     @@CheckAMD
        MOV     [CPUInfo.CpuType], CPU_TYPE_INTEL
        JMP     @@StandardFunctions

  @@CheckAMD:
        CMP     DWORD PTR [CPUInfo.VendorIDString], 'htuA'
        JNE     @@CheckCitrix
        CMP     DWORD PTR [CPUInfo.VendorIDString + 4], 'itne'
        JNE     @@CheckCitrix
        CMP     DWORD PTR [CPUInfo.VendorIDString + 8], 'DMAc'
        JNE     @@CheckCitrix
        MOV     [CPUInfo.CpuType], CPU_TYPE_AMD
        JMP     @@CheckAMDExtended

  @@CheckCitrix:
        CMP     DWORD PTR [CPUInfo.VendorIDString], 'iryC'
        JNE     @@StandardFunctions
        CMP     DWORD PTR [CPUInfo.VendorIDString + 4], 'snIx'
        JNE     @@StandardFunctions
        CMP     DWORD PTR [CPUInfo.VendorIDString + 8], 'daet'
        JNE     @@StandardFunctions
        MOV     [CPUInfo.CpuType], CPU_TYPE_CYRIX
        JMP     @@CheckCitrixExtended

  @@CheckAMDExtended:
        MOV     EAX, 80000000h
        DB      0Fh
        DB      0A2h
        CMP     EAX, 0
        JE      @@StandardFunctions
        JMP     @@AMDOnly

  @@CheckCitrixExtended:
        MOV     EAX, 80000000h
        DB      0Fh
        DB      0A2h
        CMP     EAX, 0
        JE      @@StandardFunctions
        JMP     @@CitrixOnly

  @@StandardFunctions:
        CMP     HiVal, 1
        JL      @@DoneCPUType
        MOV     EAX, 1
        DB      0FH
        DB      0A2H
        MOV     [CPUInfo.Features], EDX
        MOV     [CPUInfo.IntelSpecific.BrandID], BL 
        MOV     ECX, EAX
        AND     EAX, 3000H
        SHR     EAX, 12
        MOV     [CPUInfo.PType], AL
        MOV     EAX, ECX
        AND     EAX, 0F00H
        SHR     EAX, 8
        MOV     [CPUInfo.Family], AL
        MOV     EAX, ECX
        AND     EAX, 00F0H
        SHR     EAX, 4
        MOV     [CPUInfo.MODEL], AL
        MOV     EAX, ECX
        AND     EAX, 000FH
        MOV     [CPUInfo.Stepping], AL
        CMP     DWORD PTR [CPUInfo.VendorIDString], 'uneG'
        JNE     @@DoneCPUType
        CMP     DWORD PTR [CPUInfo.VendorIDString + 4], 'Ieni'
        JNE     @@DoneCPUType
        CMP     DWORD PTR [CPUInfo.VendorIDString + 8], 'letn'
        JNE     @@DoneCPUType

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

  @@AMDOnly:
        MOV     HiVal, EAX
        MOV     EAX, 80000001h
        CMP     HiVal, EAX
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
        MOV     [CPUInfo.MODEL], AL
        MOV     EAX, ECX
        AND     EAX, 000FH
        MOV     [CPUInfo.Stepping], AL
        MOV     [CPUInfo.Features], EDX

        MOV     EAX, 80000002h
        CMP     HiVal, EAX
        JL      @@DoneCPUType
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.CpuName], EAX
        MOV     DWORD PTR [CPUInfo.CpuName + 4], EBX
        MOV     DWORD PTR [CPUInfo.CpuName + 8], ECX
        MOV     DWORD PTR [CPUInfo.CpuName + 12], EDX

        MOV     EAX, 80000003h
        CMP     HiVal, EAX
        JL      @@DoneCPUType
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.CpuName + 16], EAX
        MOV     DWORD PTR [CPUInfo.CpuName + 20], EBX
        MOV     DWORD PTR [CPUInfo.CpuName + 24], ECX
        MOV     DWORD PTR [CPUInfo.CpuName + 28], EDX

        MOV     EAX, 80000004h
        CMP     HiVal, EAX
        JL      @@DoneCPUType
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.CpuName + 32], EAX
        MOV     DWORD PTR [CPUInfo.CpuName + 36], EBX
        MOV     DWORD PTR [CPUInfo.CpuName + 40], ECX
        MOV     DWORD PTR [CPUInfo.CpuName + 44], EDX

        MOV     EAX, 80000005h
        CMP     HiVal, EAX
        JL      @@DoneCPUType
        MOV     [CPUInfo.HasCacheInfo], 1
        DB      0Fh
        DB      0A2h
        MOV     WORD PTR [CPUInfo.AMDSpecific.InstructionTLB], BX
        SHR     EBX, 16
        MOV     WORD PTR [CPUInfo.AMDSpecific.DataTLB], BX
        MOV     DWORD PTR [CPUInfo.AMDSpecific.L1DataCache], ECX
        MOV     DWORD PTR [CPUInfo.AMDSpecific.L1ICache], EDX
        JMP     @@DoneCPUType

  @@CitrixOnly:
        MOV     HiVal, EAX
        MOV     EAX, 80000001h
        CMP     HiVal, EAX
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
        MOV     [CPUInfo.MODEL], AL
        MOV     EAX, ECX
        AND     EAX, 000FH
        MOV     [CPUInfo.Stepping], AL
        MOV     [CPUInfo.Features], EDX

        MOV     EAX, 80000002h
        CMP     HiVal, EAX
        JL      @@DoneCPUType
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.CpuName], EAX
        MOV     DWORD PTR [CPUInfo.CpuName + 4], EBX
        MOV     DWORD PTR [CPUInfo.CpuName + 8], ECX
        MOV     DWORD PTR [CPUInfo.CpuName + 12], EDX

        MOV     EAX, 80000003h
        CMP     HiVal, EAX
        JL      @@DoneCPUType
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.CpuName + 16], EAX
        MOV     DWORD PTR [CPUInfo.CpuName + 20], EBX
        MOV     DWORD PTR [CPUInfo.CpuName + 24], ECX
        MOV     DWORD PTR [CPUInfo.CpuName + 28], EDX

        MOV     EAX, 80000004h
        CMP     HiVal, EAX
        JL      @@DoneCPUType
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.CpuName + 32], EAX
        MOV     DWORD PTR [CPUInfo.CpuName + 36], EBX
        MOV     DWORD PTR [CPUInfo.CpuName + 40], ECX
        MOV     DWORD PTR [CPUInfo.CpuName + 44], EDX

        MOV     EAX, 80000005h
        CMP     HiVal, EAX
        JL      @@DoneCPUType
        MOV     [CPUInfo.HasCacheInfo], 1
        DB      0Fh
        DB      0A2h
        MOV     DWORD PTR [CPUInfo.CyrixSpecific.TLBInfo], EBX
        MOV     DWORD PTR [CPUInfo.CyrixSpecific.L1CacheInfo], ECX

  @@DoneCpuType:
        POP     ESI
        POP     EDX
        POP     EDI
        POP     ECX
        POP     EBX
        POP     EBP
        POP     EAX
  end;

  if CPUInfo.VendorIDString = 'GenuineIntel' then
    IntelSpecific(CpuInfo)
  else
  if CPUInfo.VendorIDString = 'CyrixInstead' then
    CyrixSpecific(CpuInfo)
  else
  if CPUInfo.VendorIDString = 'AuthenticAMD' then
    AMDSpecific(CpuInfo)
  else
  if CPUInfo.VendorIDString = 'GenuineTMx86' then
    TransmetaSpecific(CpuInfo)
  else
  begin
    CpuInfo.Manufacturer := 'Unknown';
    CpuInfo.CpuName := 'Unknown';
  end;


  Result := CPUInfo;
end;

//--------------------------------------------------------------------------------------------------

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

//==================================================================================================
// Alloc granularity
//==================================================================================================

procedure RoundToAllocGranularity64(var Value: Int64; Up: Boolean);
begin
  if (Value mod AllocGranularity) <> 0 then
    if Up then
      Value := ((Value div AllocGranularity) + 1) * AllocGranularity
    else
      Value := (Value div AllocGranularity) * AllocGranularity;
end;

//--------------------------------------------------------------------------------------------------

procedure RoundToAllocGranularityPtr(var Value: Pointer; Up: Boolean);
begin
  if (Cardinal(Value) mod AllocGranularity) <> 0 then
    if Up then
      Value := Pointer(((Cardinal(Value) div AllocGranularity) + 1) * AllocGranularity)
    else
      Value := Pointer((Cardinal(Value) div AllocGranularity) * AllocGranularity);
end;

//==================================================================================================
// Advanced Power Management (APM)
//==================================================================================================

{$IFDEF MSWINDOWS}
function GetAPMLineStatus: TAPMLineStatus;
var
  SystemPowerstatus: TSystemPowerStatus;
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

//--------------------------------------------------------------------------------------------------

function GetAPMBatteryFlag: TAPMBatteryFlag;
var
  SystemPowerstatus: TSystemPowerStatus;
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

//--------------------------------------------------------------------------------------------------


function GetAPMBatteryFlags: TAPMBatteryFlags;
var
  SystemPowerstatus: TSystemPowerStatus;
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

//--------------------------------------------------------------------------------------------------

function GetAPMBatteryLifePercent: Integer;
var
  SystemPowerstatus: TSystemPowerStatus;
begin
  Result := 0;

  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion < 5) then // Windows NT doesn't support GetSystemPowerStatus
    Exit;

  if not GetSystemPowerStatus(SystemPowerStatus) then
    RaiseLastOSError
  else
    Result := SystemPowerStatus.BatteryLifePercent;
end;

//--------------------------------------------------------------------------------------------------

function GetAPMBatteryLifeTime: DWORD;
var
  SystemPowerstatus: TSystemPowerStatus;
begin
  Result := 0;

  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion < 5) then // Windows NT doesn't support GetSystemPowerStatus
    Exit;

  if not GetSystemPowerStatus(SystemPowerStatus) then
    RaiseLastOSError
  else
    Result := SystemPowerStatus.BatteryLifeTime;
end;

//--------------------------------------------------------------------------------------------------

function GetAPMBatteryFullLifeTime: DWORD;
var
  SystemPowerstatus: TSystemPowerStatus;
begin
  Result := 0;

  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion < 5) then // Windows NT doesn't support GetSystemPowerStatus
    Exit;

  if not GetSystemPowerStatus(SystemPowerStatus) then
    RaiseLastOSError
  else
    Result := SystemPowerStatus.BatteryFullLifeTime;
end;

//==================================================================================================
// Memory Information
//==================================================================================================

function GetMaxAppAddress: Integer;
var
  SystemInfo: TSystemInfo;
begin
  FillChar(SystemInfo, SizeOf(SystemInfo), #0);
  GetSystemInfo(SystemInfo);
  Result := Integer(SystemInfo.lpMaximumApplicationAddress);
end;

//--------------------------------------------------------------------------------------------------

function GetMinAppAddress: Integer;
var
  SystemInfo: TSystemInfo;
begin
  FillChar(SystemInfo, SizeOf(SystemInfo), #0);
  GetSystemInfo(SystemInfo);
  Result := Integer(SystemInfo.lpMinimumApplicationAddress);
end;
{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

function GetTotalPageFileMemory: Integer;
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  Result := MemoryStatus.dwTotalPageFile;
end;

//--------------------------------------------------------------------------------------------------

function GetFreePageFileMemory: Integer;
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  Result := MemoryStatus.dwAvailPageFile;
end;

//--------------------------------------------------------------------------------------------------

function GetTotalVirtualMemory: Integer;
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  Result := MemoryStatus.dwTotalVirtual;
end;

//--------------------------------------------------------------------------------------------------

function GetFreeVirtualMemory: Integer;
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  Result := MemoryStatus.dwAvailVirtual;
end;

//==================================================================================================
// Keyboard Information
//==================================================================================================

function GetKeybStateHelper(VirtualKey: Cardinal; Mask: Byte): Boolean;
var
  Keys: TKeyboardState;
begin
  Result := GetKeyBoardState(Keys) and (Keys[VirtualKey] and Mask <> 0);
end;

//--------------------------------------------------------------------------------------------------

function GetKeyState(const VirtualKey: Cardinal): Boolean;
begin
  Result := GetKeybStateHelper(VirtualKey, $80);
end;

//--------------------------------------------------------------------------------------------------

function GetNumLockKeyState: Boolean;
begin
  Result := GetKeybStateHelper(VK_NUMLOCK, $01);
end;

//--------------------------------------------------------------------------------------------------

function GetScrollLockKeyState: Boolean;
begin
  Result := GetKeybStateHelper(VK_SCROLL, $01);
end;

//--------------------------------------------------------------------------------------------------

function GetCapsLockKeyState: Boolean;
begin
  Result := GetKeybStateHelper(VK_CAPITAL, $01);
end;

//==================================================================================================
// Windows 95/98/Me system resources information
//==================================================================================================

{ TODO -oPJH : compare to Win9xFreeSysResources }
var
  ResmeterLibHandle: THandle;
  MyGetFreeSystemResources: function(ResType: UINT): UINT; stdcall;

//--------------------------------------------------------------------------------------------------

procedure UnloadSystemResourcesMeterLib;
begin
  if ResmeterLibHandle <> 0 then
  begin
    FreeLibrary(ResmeterLibHandle);
    ResmeterLibHandle := 0;
    @MyGetFreeSystemResources := nil;
  end;
end;

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

function GetFreeSystemResources(const ResourceType: TFreeSysResKind): Integer;
const
  ParamValues: array [TFreeSysResKind] of UINT = (0, 1, 2);
begin
  if IsSystemResourcesMeterPresent then
    Result := MyGetFreeSystemResources(ParamValues[ResourceType])
  else
    Result := -1;
end;

//--------------------------------------------------------------------------------------------------

function GetFreeSystemResources: TFreeSystemResources;
begin
  with Result do
  begin
    SystemRes := GetFreeSystemResources(rtSystem);
    GdiRes := GetFreeSystemResources(rtGdi);
    UserRes := GetFreeSystemResources(rtUser);
  end;
end;

//==================================================================================================
// Initialization
//==================================================================================================

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
        // (rom) bug? IsWinNT35 not set here
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

//==================================================================================================
// Finalization
//==================================================================================================

procedure FinalizeSysInfo;
begin
  UnloadSystemResourcesMeterLib;
end;

//--------------------------------------------------------------------------------------------------

initialization
  InitSysInfo;

finalization
  FinalizeSysInfo;

{$ENDIF MSWINDOWS}

// History:

// $Log$
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
// Revision 1.15  2004/04/18 19:57:29  peterjhaas
// - rename one of the GetOpenGLVersion to GetOpenGLVersionBitmapRendering
// - delete pre-loading of Glu32Handle
// - move the OpenGl32Handle call to directly before ChoosePixelFormat
//
// Revision 1.14  2004/04/18 05:14:11  rrossmair
// fixed GetOpenGLVersion (draw to bitmap overload); removed VCL dependency ("uses Graphics")
//
// Revision 1.13  2004/04/18 00:43:19  peterjhaas
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
// Revision 1.9  2004/04/06 04:53:18  peterjhaas
// adapt compiler conditions, add log entry
//

end.
