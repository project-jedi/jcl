{$IFDEF JCL}
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
{ The Original Code is Hardlink.pas.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Oliver Schneider (Assarbad att gmx dott info).     }
{ Portions created by Oliver Schneider are Copyright (C) 1995 - 2004 Oliver Schneider.             }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Obtained through:                                                                                }
{   Joint Endeavour of Delphi Innovators (Project JEDI)                                            }
{                                                                                                  }
{ You may retrieve the latest version of the original file at the Original Developer's homepage,   }
{ located at http://assarbad.net.                                                                  }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Oliver Schneider (assarbad)                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{  Windows NT 4.0 compatible implementation of the CreateHardLink() API introduced in Windows      }
{  2000.                                                                                           }
{                                                                                                  }
{**************************************************************************************************}
{$ELSE ~JCL}
(******************************************************************************
 ******************************************************************************
 ***                                                                        ***
 ***  Hardlinks. Implementation of the CreateHardLink() API introduced in   ***
 ***  Windows 2000 - BUT ALSO COMPATIBLE with Windows NT 4.0!               ***
 ***  This implementation should be fully compatible with the Windows 2000  ***
 ***  implementation (including last error and so on).                      ***
 ***                                                                        ***
 ***  Version [1.12a]                               {Last mod 2004-10-21}   ***
 ***                                                                        ***
 ******************************************************************************
 ******************************************************************************

                                 _\\|//_
                                (` * * ')
 ______________________________ooO_(_)_Ooo_____________________________________
 ******************************************************************************
 ******************************************************************************
 ***                                                                        ***
 ***                Copyright (c) 1995 - 2004 by -=Assarbad=-               ***
 ***                                                                        ***
 ***   CONTACT TO THE AUTHOR(S):                                            ***
 ***    ____________________________________                                ***
 ***   |                                    |                               ***
 ***   | -=Assarbad=- aka Oliver            |                               ***
 ***   |____________________________________|                               ***
 ***   |                                    |                               ***
 ***   | Assarbad @ gmx.info|.net|.com|.de  |                               ***
 ***   | ICQ: 281645                        |                               ***
 ***   | AIM: nixlosheute                   |                               ***
 ***   |      nixahnungnicht                |                               ***
 ***   | MSN: Assarbad@ePost.de             |                               ***
 ***   | YIM: sherlock_holmes_and_dr_watson |                               ***
 ***   |____________________________________|                               ***
 ***             ___                                                        ***
 ***            /   |                     ||              ||                ***
 ***           / _  |   ________ ___  ____||__    ___   __||                ***
 ***          / /_\ |  / __/ __//   |/  _/|   \  /   | /   |                ***
 ***         / ___  |__\\__\\  / /\ || |  | /\ \/ /\ |/ /\ | DOT NET        ***
 ***        /_/   \_/___/___/ /_____\|_|  |____/_____\\__/\|                ***
 ***       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~        ***
 ***              [http://assarbad.net | http://assarbad.org]               ***
 ***                                                                        ***
 ***   Notes:                                                               ***
 ***   - my first name is Oliver, you may well use this in your e-mails     ***
 ***   - for questions and/or proposals drop me a mail or instant message   ***
 ***                                                                        ***
 ***~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~***
 ***              May the source be with you, stranger ... ;)               ***
 ***    Snizhok, eto ne tolko fruktovij kefir, snizhok, eto stil zhizni.    ***
 ***                     Vsekh Privet iz Germanii                           ***
 ***                                                                        ***
 *** Greets from -=Assarbad=- fly to YOU =)                                 ***
 *** Special greets fly 2 Nico, Casper, SA, Pizza, Navarion, Eugen, Zhenja, ***
 *** Xandros, Melkij, Strelok etc pp.                                       ***
 ***                                                                        ***
 *** Thanks to:                                                             ***
 *** W.A. Mozart, Vivaldi, Beethoven, Poeta Magica, Kurtzweyl, Manowar,     ***
 *** Blind Guardian, Weltenbrand, In Extremo, Wolfsheim, Carl Orff, Zemfira ***
 *** ... most of my work was done with their music in the background ;)     ***
 ***                                                                        ***
 ******************************************************************************
 ******************************************************************************

 LEGAL STUFF:
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 NOTE: This source is triple-licensed. You may choose between:
       - the BSD-License (as seen below)
       - the LGPL [ http://www.opensource.org/licenses/lgpl-license.php ]
       - the MPL [ http://www.opensource.org/licenses/mozilla1.1.php ]
       Attention: if this unit comes bundled with the files of the project
       JEDI the project license (currently MPL) is mandatory!!!
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 Copyright (c) 1995-2004, -=Assarbad=- ["copyright holder(s)"]
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
 3. The name(s) of the copyright holder(s) may not be used to endorse or
    promote products derived from this software without specific prior written
    permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
 DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                             .oooO     Oooo.
 ____________________________(   )_____(   )___________________________________
                              \ (       ) /
                               \_)     (_/

 ******************************************************************************)
{$ENDIF ~JCL}

{$IFDEF JCL}
// Last modified: $Date$
// For history see end of file

{$ENDIF ~JCL}
unit Hardlinks;

{$ALIGN ON}
{$MINENUMSIZE 4}

interface
// ALL enabled by default for Project JEDI
{$DEFINE STDCALL}   // Make functions STDCALL always
{$DEFINE RTDL}      // Use runtime dynamic linking
{$DEFINE PREFERAPI} // Prefer the "real" Windows API on systems on which it exists
                    // If this is defined STDCALL is automatically needed and defined!

(*
  All possible combinations of the above DEFINEs have been tested and work fine.

   # | A  B  C
  ---|---------
   1 | 0  0  0                 A = STDCALL
   2 | 0  0  X                 B = RTDL
   3 | X  0  0                 C = PREFERAPI
   4 | X  0  X
   5 | X  X  0
   6 | X  X  X
*)
uses
  Windows;

{$IFDEF PREFERAPI}
  {$DEFINE STDCALL} // For the windows API we _require_ STDCALL calling convention
{$ENDIF PREFERAPI}

{$EXTERNALSYM CreateHardLinkW}
{$EXTERNALSYM CreateHardLinkA}

{$IFNDEF PREFERAPI}
// We prefer the homegrown version - use the static version
function CreateHardLinkW(szLinkName, szLinkTarget: PWideChar; lpSecurityAttributes: PSecurityAttributes): BOOL;
  {$IFDEF STDCALL}stdcall;{$ENDIF} // Makes the actual call STDCALL
function CreateHardLinkA(szLinkName, szLinkTarget: PAnsiChar; lpSecurityAttributes: PSecurityAttributes): BOOL;
  {$IFDEF STDCALL}stdcall;{$ENDIF} // Makes the actual call STDCALL
{$ELSE PREFERAPI}
// Well, we did not decide yet ;) - bind to either address, depending on whether
// the API could be found.
type
  TFNCreateHardLinkW = function(szLinkName, szLinkTarget: PWideChar; lpSecurityAttributes: PSecurityAttributes): BOOL;{$IFDEF STDCALL} stdcall;{$ENDIF}
  TFNCreateHardLinkA = function(szLinkName, szLinkTarget: PAnsiChar; lpSecurityAttributes: PSecurityAttributes): BOOL;{$IFDEF STDCALL} stdcall;{$ENDIF}
var
  CreateHardLinkW: TFNCreateHardLinkW = nil;
  CreateHardLinkA: TFNCreateHardLinkA = nil;
{$ENDIF PREFERAPI}

{$IFDEF RTDL}
var
  hNtDll: THandle = 0; // For runtime dynamic linking
  bRtdlFunctionsLoaded: Boolean = False; // To show whether the RTDL functions had been loaded
{$ENDIF RTDL}

implementation

const
  szNtDll           = 'NTDLL.DLL'; // Import native APIs from this DLL
{$IFDEF PREFERAPI}
  szCreateHardLinkA = 'CreateHardLinkA';
  szCreateHardLinkW = 'CreateHardLinkW';
{$ENDIF PREFERAPI}

(******************************************************************************

 Note, I only include function prototypes and constants here which are needed!
 For other prototypes or constants check out the related books of
 - Gary Nebbett
 - Sven B. Schreiber
 - Rajeev Nagar

 Note, one my homepage I have also some Native APIs listed in Delphi translated
 form. Not all of them might be translated correctly with respect to the fact
 whether or not they are pointer and whether or not the alignment of variables
 or types is always correct. This might be reviewed by me somewhen in future.

 ******************************************************************************)

// =================================================================
// Type definitions
// =================================================================
type
  NTSTATUS = Longint;
  PPWideChar = ^PWideChar;

// =================================================================
// Constants
// =================================================================
const
  FileDirectoryInformation = 1;
  FileLinkInformation      = 11;
  FILE_SYNCHRONOUS_IO_NONALERT = $00000020;
  FILE_OPEN_FOR_BACKUP_INTENT  = $00004000;
  FILE_OPEN_REPARSE_POINT      = $00200000;
  FILE_LIST_DIRECTORY = 1;
  FILE_DIRECTORY_FILE = $00000001;
  STATUS_SUCCESS       = NTSTATUS(0);
  STATUS_NO_MORE_FILES = NTSTATUS($80000006);
  OBJ_CASE_INSENSITIVE = $00000040;

  // Should be defined, but isn't
  HEAP_ZERO_MEMORY = $00000008;

  // Related constant(s) for RtlDetermineDosPathNameType_U()
  INVALID_PATH        = 0;
  UNC_PATH            = 1;
  ABSOLUTE_DRIVE_PATH = 2;
  RELATIVE_DRIVE_PATH = 3;
  ABSOLUTE_PATH       = 4;
  RELATIVE_PATH       = 5;
  DEVICE_PATH         = 6;
  UNC_DOT_PATH        = 7;

  MAXIMUM_REPARSE_DATA_BUFFER_SIZE = 16 * 1024;
  IO_REPARSE_TAG_MOUNT_POINT = $A0000003;
  FSCTL_SET_REPARSE_POINT = $900A4;
  REPARSE_DATA_BUFFER_HEADER_SIZE = 2 * SizeOf(DWORD);

// =================================================================
// Type definitions
// =================================================================
type
  LARGE_INTEGER = TLargeInteger;
  PLARGE_INTEGER = ^LARGE_INTEGER;

  UNICODE_STRING = record
    Length: WORD;
    MaximumLength: WORD;
    Buffer: PWideChar;
  end;
  PUNICODE_STRING = ^UNICODE_STRING;

  ANSI_STRING = record
    Length: WORD;
    MaximumLength: WORD;
    Buffer: PAnsiChar;
  end;
  PANSI_STRING = ^ANSI_STRING;


  REPARSE_DATA_BUFFER = record
    ReparseTag: DWORD;
    ReparseDataLength: WORD;
    Reserved: WORD;
    case Integer of
      0:
       (SubstituteNameOffset: WORD;
        SubstituteNameLength: WORD;
        PrintNameOffset: WORD;
        PrintNameLength: WORD;
        PathBuffer: array [0..0] of WideChar;);
      1:
        (DataBuffer: array [0..0] of Byte;);
  end;
  PREPARSE_DATA_BUFFER = ^REPARSE_DATA_BUFFER;

  OBJECT_ATTRIBUTES = record
    Length: ULONG;
    RootDirectory: THandle;
    ObjectName: PUNICODE_STRING;
    Attributes: ULONG;
    SecurityDescriptor: Pointer;       // Points to type SECURITY_DESCRIPTOR
    SecurityQualityOfService: Pointer; // Points to type SECURITY_QUALITY_OF_SERVICE
  end;
  POBJECT_ATTRIBUTES = ^OBJECT_ATTRIBUTES;

  IO_STATUS_BLOCK = record
    case integer of
      0:
       (Status: NTSTATUS);
      1:
       (Pointer: Pointer;
        Information: ULONG);
  end;
  PIO_STATUS_BLOCK = ^IO_STATUS_BLOCK;

  _FILE_LINK_RENAME_INFORMATION = record // File Information Classes 10 and 11
    ReplaceIfExists: BOOL;
    RootDirectory: THandle;
    FileNameLength: ULONG;
    FileName: array[0..0] of WideChar;
  end;
  FILE_LINK_INFORMATION = _FILE_LINK_RENAME_INFORMATION;
  PFILE_LINK_INFORMATION = ^FILE_LINK_INFORMATION;
  FILE_RENAME_INFORMATION = _FILE_LINK_RENAME_INFORMATION;
  PFILE_RENAME_INFORMATION = ^FILE_RENAME_INFORMATION;

  FILE_DIRECTORY_INFORMATION = record // File Information Class 1
    NextEntryOffset: ULONG;
    Unknown: ULONG;
    CreationTime: LARGE_INTEGER;
    LastAccessTime: LARGE_INTEGER;
    LastWriteTime: LARGE_INTEGER;
    ChangeTime: LARGE_INTEGER;
    EndOfFile: LARGE_INTEGER;
    AllocationSize: LARGE_INTEGER;
    FileAttributes: ULONG;
    FileNameLength: ULONG;
    FileName: array [0..0] of WideChar;
  end;
  PFILE_DIRECTORY_INFORMATION = ^FILE_DIRECTORY_INFORMATION;

// =================================================================
// PROCESS ENVIRONMENT BLOCK (PEB)
// =================================================================

  MODULE_HEADER = record
    d000: DWORD;        // 000
    d004: DWORD;        // 004
    List1: LIST_ENTRY;  // 008
    List2: LIST_ENTRY;  // 010
    List3: LIST_ENTRY;  // 018
  end;
  PMODULE_HEADER = ^MODULE_HEADER;
  PPMODULE_HEADER = ^PMODULE_HEADER;

// -----------------------------------------------------------------

  PROCESS_MODULE_INFO = record
     Size: DWORD;                 // 000
     ModuleHeader: MODULE_HEADER; // 004
  end;
  PPROCESS_MODULE_INFO = ^PROCESS_MODULE_INFO;
  PPPROCESS_MODULE_INFO = ^PPROCESS_MODULE_INFO;

// -----------------------------------------------------------------

  PROCESS_PARAMETERS = record
    Allocated: DWORD;                      // 000
    Size: DWORD;                           // 004
    Flags: DWORD;                          // 008   bit 0: all pointers normalized
    DebugFlags: DWORD;                     // 00C
    Console: THandle;                      // 010
    ProcessGroup: DWORD;                   // 014
    StdInput: THandle;                     // 018
    StdOutput: THandle;                    // 01C
    StdError: THandle;                     // 020
    WorkingDirectoryName: UNICODE_STRING;  // 024
    WorkingDirectoryHandle: THandle;       // 02C
    SearchPath: UNICODE_STRING;            // 030
    ImagePath: UNICODE_STRING;             // 038
    CommandLine: UNICODE_STRING;           // 040
    Environment: PWideChar;                // 048
    X: DWORD;                              // 04C
    Y: DWORD;                              // 050
    XSize: DWORD;                          // 054
    YSize: DWORD;                          // 058
    XCountChars: DWORD;                    // 05C
    YCountChars: DWORD;                    // 060
    FillAttribute: DWORD;                  // 064
    Flags2: DWORD;                         // 068
    ShowWindow: Word;                      // 06C
    Reserved2: Word;                       // 06E
    Title: UNICODE_STRING;                 // 070
    Desktop: UNICODE_STRING;               // 078
    Reserved3: UNICODE_STRING;             // 080
    Reserved4: UNICODE_STRING;             // 088
  end;
  PPROCESS_PARAMETERS = ^PROCESS_PARAMETERS;
  PPPROCESS_PARAMETERS = ^PPROCESS_PARAMETERS;

// -----------------------------------------------------------------

  SYSTEM_STRINGS = record
    SystemRoot: UNICODE_STRING;        // 000  %SystemRoot%
    System32Root: UNICODE_STRING;      // 008  %SystemRoot%\System32
    BaseNamedObjects: UNICODE_STRING;  // 010  \BaseNamedObjects
  end;
  PSYSTEM_STRINGS = ^SYSTEM_STRINGS;
  PPSYSTEM_STRINGS = ^PSYSTEM_STRINGS;

// -----------------------------------------------------------------

  TEXT_INFO = record
    Reserved: Pointer;               // 000
    SystemStrings: PSYSTEM_STRINGS;  // 004
  end;
  PTEXT_INFO = ^TEXT_INFO;
  PPTEXT_INFO = ^PTEXT_INFO;

// -----------------------------------------------------------------

  PPEB_FREE_BLOCK = ^PEB_FREE_BLOCK;
  PEB_FREE_BLOCK = record
    Next: PPEB_FREE_BLOCK;  // 000
    Size: ULONG;            // 004
  end;

  RTL_BITMAP = record
    SizeOfBitMap: DWORD;  // 000
    Buffer: PDWORD;       // 004
  end;
  PRTL_BITMAP = ^RTL_BITMAP;
  PPRTL_BITMAP = ^PRTL_BITMAP;

  KAFFINITY = DWORD;
  PKAFFINITY = ^KAFFINITY;

  CLIENT_ID = record
    UniqueProcess: THandle;  // 000
    UniqueThread: THandle;   // 004
  end;
  PCLIENT_ID = ^CLIENT_ID;

  PCRITICAL_SECTION = PRtlCriticalSection;
  PPEBLOCKROUTINE = procedure(PebLock: PCRITICAL_SECTION); stdcall;
  PFarProc = ^TFarProc;

// -----------------------------------------------------------------
// located at 0x7FFDF000

  PEB = record
    InheritedAddressSpace: Boolean;                      // 000
    ReadImageFileExecOptions: Boolean;                   // 001
    BeingDebugged: Boolean;                              // 002
    bReserved: Boolean;                                  // 003
    Mutant: Pointer;                                     // 004  THandle
    SectionBaseAddress: Pointer;                         // 008
    ProcessModuleInfo: PPROCESS_MODULE_INFO;             // 00C
    ProcessParameters: PPROCESS_PARAMETERS;              // 010
    SubSystemData: DWORD;                                // 014
    ProcessHeap: Pointer;                                // 018  THandle
    FastPebLock: PCRITICAL_SECTION;                      // 01C
    AcquireFastPebLock PPEBLOCKROUTINE;                  // 020  function
    ReleaseFastPebLock: PPEBLOCKROUTINE;                 // 024  function
    EnvironmentUpdateCount: DWORD;                       // 028
    KernelCallbackTable: PFarProc;                       // 02C  function
    EventLogSection: Pointer;                            // 030  THandle
    EventLog: Pointer;                                   // 034  THandle
    FreeList: PPEB_FREE_BLOCK;                           // 038
    TlsBitMapSize: DWORD;                                // 03C  number of bits
    TlsBitMap: PRTL_BITMAP;                              // 040  ntdll!TlsBitMap
    TlsBitMapData: array [0..1] of DWORD;                // 044  64 bits
    ReadOnlySharedMemoryBase: Pointer;                   // 04C
    ReadOnlySharedMemoryHeap: Pointer;                   // 050
    TextInfo: PTEXT_INFO;                                // 054
    InitAnsiCodePageData: Pointer;                       // 058
    InitOemCodePageData: Pointer;                        // 05C
    InitUnicodeCaseTableData: Pointer;                   // 060
    KeNumberProcessors: array [0..3] of Byte;            // 064
    NtGlobalFlag: array [0..3] of Byte;                  // 068
    Reserved: array [0..3] of Byte;                      // 06C
    MmCriticalSectionTimeout: LARGE_INTEGER;             // 070
    MmHeapSegmentReserve: DWORD;                         // 078
    MmHeapSegmentCommit: DWORD;                          // 07C
    MmHeapDeCommitTotalFreeThreshold: DWORD;             // 080
    MmHeapDeCommitFreeBlockThreshold: DWORD;             // 084
    NumberOfHeaps: DWORD;                                // 088
    AvailableHeaps: DWORD;                               // 08C  16, *2 if exhausted
    ProcessHeapsListBuffer: PHandle;                     // 090
    GdiSharedHandleTable: Pointer;                       // 094
    ProcessStarterHelper: Pointer;                       // 098
    GdiDCAttributeList: Pointer;                         // 09C
    LoaderLock: PCRITICAL_SECTION;                       // 0A0
    NtMajorVersion: DWORD;                               // 0A4
    NtMinorVersion: DWORD;                               // 0A8
    NtBuildNumber: WORD;                                 // 0AC
    CmNtCSDVersion: WORD;                                // 0AE
    PlatformId: DWORD;                                   // 0B0
    Subsystem: DWORD;                                    // 0B4
    MajorSubsystemVersion: DWORD;                        // 0B8
    MinorSubsystemVersion: DWORD;                        // 0BC
    AffinityMask: KAFFINITY;                             // 0C0
    ad0C4: array [0..33] of DWORD;                       // 0C4
    PostProcessInitRoutine: ^Pointer;                    // 14C
    TlsExpansionBitmap: ULONG;                           // 150
    TlsExpansionBitmapBits: array [0..$80 - 1] of Byte;  // 154
    Win32WindowStation: THandle;                         // 1D4  aka SessionId???
    d1D8: DWORD;                                         // 1D8
    d1DC: DWORD;                                         // 1DC
    CSDVersion: PWord;                                   // 1E0
    d1E4: DWORD;                                         // 1E4
  end;
  PPEB = ^PEB;
  PPPEB = ^PPEB;

// =================================================================
// THREAD ENVIRONMENT BLOCK (TEB)
// =================================================================

  PNT_TIB = ^NT_TIB;
  NT_TIB = record
    ExceptionList: Pointer;         // 000   ^_EXCEPTION_REGISTRATION_RECORD
    StackBase: Pointer;             // 004
    StackLimit: Pointer;            // 008
    SubSystemTib: Pointer;          // 00C
    _union: record                  // 010
    case Integer of
      0:
        (FiberData: Pointer);       // 010
      1:
        (Version: ULONG);           // 010
    end;
    ArbitraryUserPointer: Pointer;  // 014
    Self: PNT_TIB;                  // 018
  end;
  PPNT_TIB = ^PNT_TIB;

// -----------------------------------------------------------------
// located at 0x7FFDE000, 0x7FFDD000, ...

  TEB = record
    Tib: NT_TIB;                   // 000
    EnvironmentPointer: Pointer;   // 01C
    ClientId: CLIENT_ID;           // 020
    RpcHandle: THandle;            // 028
    ThreadLocalStorage: ^Pointer;  // 02C
    Peb: PPEB;                     // 030
    LastErrorValue: DWORD;         // 034
  end;
  PTEB = ^TEB;
  PPTEB = ^PTEB;

// =================================================================
// Function prototypes
// =================================================================

{$IFNDEF RTDL}
function RtlCreateUnicodeStringFromAsciiz(var destination: UNICODE_STRING;
  source: PChar): Boolean; stdcall; external szNtDll;

function ZwClose(Handle: THandle): NTSTATUS; stdcall; external szNtDll;

function ZwSetInformationFile(FileHandle: THandle; var IoStatusBlock: IO_STATUS_BLOCK;
  FileInformation: Pointer; FileInformationLength: ULONG;
  FileInformationClass: DWORD): NTSTATUS; stdcall; external szNtDll;

function RtlPrefixUnicodeString(const usPrefix: UNICODE_STRING;
  const usContainingString: UNICODE_STRING;
  ignore_case: Boolean): Boolean; stdcall; external szNtDll;

function ZwOpenSymbolicLinkObject(var LinkHandle: THandle; DesiredAccess: DWORD;
  const ObjectAttributes: OBJECT_ATTRIBUTES): NTSTATUS; stdcall; external szNtDll;

function ZwQuerySymbolicLinkObject(LinkHandle: THandle;
  var LinkTarget: UNICODE_STRING; ReturnedLength: PULONG): NTSTATUS; stdcall; external szNtDll;

function ZwOpenFile(var FileHandle: THandle; DesiredAccess: DWORD;
  const ObjectAttributes: OBJECT_ATTRIBUTES; var IoStatusBlock: IO_STATUS_BLOCK;
  ShareAccess: ULONG; OpenOptions: ULONG): NTSTATUS; stdcall; external szNtDll;

function RtlAllocateHeap(HeapHandle: Pointer;
  Flags, Size: ULONG): Pointer; stdcall; external szNtDll;

function RtlFreeHeap(HeapHandle: Pointer; Flags: ULONG;
  MemoryPointer: Pointer): Boolean; stdcall; external szNtDll;

function RtlDosPathNameToNtPathName_U(DosName: PWideChar;
  var NtName: UNICODE_STRING; DosFilePath: PPWideChar;
  var NtFilePath: UNICODE_STRING): Boolean; stdcall; external szNtDll;

function RtlInitUnicodeString(var DestinationString: UNICODE_STRING;
  const SourceString: PWideChar): NTSTATUS; stdcall; external szNtDll;

function RtlDetermineDosPathNameType_U(wcsPathNameType: PWideChar): DWORD; stdcall; external szNtDll;

function RtlNtStatusToDosError(status: NTSTATUS): ULONG; stdcall; external szNtDll;

function ZwQueryDirectoryFile(FileHandle, Event: THandle;
  ApcRoutine: Pointer; ApcContext: Pointer;
  var IoStatusBlock: IO_STATUS_BLOCK; FileInformation: Pointer;
  FileInformationLength: ULONG; FileInformationClass: DWORD;
  ReturnSingleEntry: Boolean; var FileName: UNICODE_STRING;
  RestartScan: Boolean): NTSTATUS; stdcall; external szNtDll;

function ZwQueryInformationFile(FileHandle: THandle;
  var IoStatusBlock: IO_STATUS_BLOCK; FileInformation: Pointer;
  FileInformationLength: ULONG; FileInformationClass: DWORD): NTSTATUS; stdcall; external szNtDll;
{$ELSE RTDL}
type
  TRtlCreateUnicodeStringFromAsciiz = function(var destination: UNICODE_STRING;
    source: PChar): Boolean; stdcall;

  TZwClose = function(Handle: THandle): NTSTATUS; stdcall;

  TZwSetInformationFile = function(FileHandle: THandle;
    var IoStatusBlock: IO_STATUS_BLOCK; FileInformation: Pointer;
    FileInformationLength: ULONG; FileInformationClass: DWORD): NTSTATUS; stdcall;

  TRtlPrefixUnicodeString = function(const usPrefix: UNICODE_STRING;
    const usContainingString: UNICODE_STRING; ignore_case: Boolean): Boolean; stdcall;

  TZwOpenSymbolicLinkObject = function(var LinkHandle: THandle;
    DesiredAccess: DWORD; const ObjectAttributes: OBJECT_ATTRIBUTES): NTSTATUS; stdcall;

  TZwQuerySymbolicLinkObject = function(LinkHandle: THandle;
    var LinkTarget: UNICODE_STRING; ReturnedLength: PULONG): NTSTATUS; stdcall;

  TZwOpenFile = function(var FileHandle: THandle; DesiredAccess: DWORD;
    const ObjectAttributes: OBJECT_ATTRIBUTES; var IoStatusBlock: IO_STATUS_BLOCK;
    ShareAccess: ULONG; OpenOptions: ULONG): NTSTATUS; stdcall;

  TRtlAllocateHeap = function(HeapHandle: Pointer; Flags, Size: ULONG): Pointer; stdcall;

  TRtlFreeHeap = function(HeapHandle: Pointer; Flags: ULONG;
    MemoryPointer: Pointer): Boolean; stdcall;

  TRtlDosPathNameToNtPathName_U = function(DosName: PWideChar;
    var NtName: UNICODE_STRING; DosFilePath: PPWideChar;
    var NtFilePath: UNICODE_STRING): Boolean; stdcall;

  TRtlInitUnicodeString = function(var DestinationString: UNICODE_STRING;
    const SourceString: PWideChar): NTSTATUS; stdcall;

  TRtlDetermineDosPathNameType_U = function(wcsPathNameType: PWideChar): DWORD; stdcall;

  TRtlNtStatusToDosError = function(status: NTSTATUS): ULONG; stdcall;

  TZwQueryDirectoryFile = function(FileHandle, Event: THandle;
    ApcRoutine: Pointer; ApcContext: Pointer; var IoStatusBlock: IO_STATUS_BLOCK;
    FileInformation: Pointer; FileInformationLength: ULONG; FileInformationClass: DWORD;
    ReturnSingleEntry: Boolean; var FileName: UNICODE_STRING;
    RestartScan: Boolean): NTSTATUS; stdcall;

  TZwQueryInformationFile = function(FileHandle: THandle;
    var IoStatusBlock: IO_STATUS_BLOCK; FileInformation: Pointer;
    FileInformationLength: ULONG; FileInformationClass: DWORD): NTSTATUS; stdcall;

// Declare all the _global_ function pointers for RTDL
var
  RtlCreateUnicodeStringFromAsciiz: TRtlCreateUnicodeStringFromAsciiz = nil;
  ZwClose: TZwClose = nil;
  ZwSetInformationFile: TZwSetInformationFile = nil;
  RtlPrefixUnicodeString: TRtlPrefixUnicodeString = nil;
  ZwOpenSymbolicLinkObject: TZwOpenSymbolicLinkObject = nil;
  ZwQuerySymbolicLinkObject: TZwQuerySymbolicLinkObject = nil;
  ZwOpenFile: TZwOpenFile = nil;
  RtlAllocateHeap: TRtlAllocateHeap = nil;
  RtlFreeHeap: TRtlFreeHeap = nil;
  RtlDosPathNameToNtPathName_U: TRtlDosPathNameToNtPathName_U = nil;
  RtlInitUnicodeString: TRtlInitUnicodeString = nil;
  RtlDetermineDosPathNameType_U: TRtlDetermineDosPathNameType_U = nil;
  RtlNtStatusToDosError: TRtlNtStatusToDosError = nil;
  ZwQueryDirectoryFile: TZwQueryDirectoryFile = nil;
  ZwQueryInformationFile: TZwQueryInformationFile = nil;
{$ENDIF RTDL}

function NtMyGetProcessHeap: Pointer; assembler;
asm
        // To understand this a deep understanding of the NT Native API and its
        // structures is required.
        MOV    EAX, FS:[0]._TEB.Peb // FS points to TEB/TIB which has a pointer to the PEB
        MOV    EAX, [EAX]._PEB.ProcessHeap // Get the process heap's handle
end;

(******************************************************************************

 Syntax:
 -------
  C-Prototype! (if STDCALL enabled)

  BOOL WINAPI CreateHardLink(
    LPCTSTR lpFileName,
    LPCTSTR lpExistingFileName,
    LPSECURITY_ATTRIBUTES lpSecurityAttributes // Reserved; Must be NULL!

 Compatibility:
 --------------
  The function can only work on file systems that support hard links through the
  underlying FS driver layer. Currently this only includes NTFS on the NT
  platform (as far as I know).
  The function works fine on Windows NT4/2000/XP and is considered to work on
  future Operating System versions derived from NT (including Windows 2003).

 Remarks:
 --------
  This function tries to resemble the original CreateHardLinkW() call from
  Windows 2000/XP/2003 Kernel32.DLL as close as possible. This is why many
  functions used are NT Native API, whereas one could use Delphi or Win32 API
  functions (e.g. memory management). BUT I included much more SEH code and
  omitted extra code to free buffers and close handles. This all is done during
  the FINALLY block (so there are no memory leaks anyway ;).

  Note, that neither Microsoft's code nor mine ignore the Security Descriptor
  from the SECURITY_ATTRIBUTES structure. In both cases the security descriptor
  is passed on to ZwOpenFile()!

  The limit of 1023 hardlinks to one file is probably related to the system or
  NTFS respectively. At least I saw no special hint, why there would be such a
  limit - the original CreateHardLink() does not check the number of links!
  Thus I consider the limit being the same for the original and my rewrite.

  For the ANSI version of this function see below ...

 Remarks from the  Platform SDK:
 -------------------------------
  Any directory entry for a file, whether created with CreateFile or
  CreateHardLink, is a hard link to the associated file. Additional hard links,
  created with the CreateHardLink function, allow you to have multiple directory
  entries for a file, that is, multiple hard links to the same file. These may
  be different names in the same directory, or they may be the same (or
  different) names in different directories. However, all hard links to a file
  must be on the same volume.
  Because hard links are just directory entries for a file, whenever an
  application modifies a file through any hard link, all applications using any
  other hard link to the file see the changes. Also, all of the directory
  entries are updated if the file changes. For example, if the file's size
  changes, all of the hard links to the file will show the new size.
  The security descriptor belongs to the file to which the hard link points.
  The link itself, being merely a directory entry, has no security descriptor.
  Thus, if you change the security descriptor of any hard link, you're actually
  changing the underlying file's security descriptor. All hard links that point
  to the file will thus allow the newly specified access. There is no way to
  give a file different security descriptors on a per-hard-link basis.
  This function does not modify the security descriptor of the file to be linked
  to, even if security descriptor information is passed in the
  lpSecurityAttributes parameter.
  Use DeleteFile to delete hard links. You can delete them in any order
  regardless of the order in which they were created.
  Flags, attributes, access, and sharing as specified in CreateFile operate on
  a per-file basis. That is, if you open a file with no sharing allowed, another
  application cannot share the file by creating a new hard link to the file.

  CreateHardLink does not work over the network redirector.

  Note that when you create a hard link on NTFS, the file attribute information
  in the directory entry is refreshed only when the file is opened or when
  GetFileInformationByHandle is called with the handle of the file of interest.

 ******************************************************************************)
function
{$IFNDEF PREFERAPI}
  CreateHardLinkW // This name is directly published if PREFERAPI is not defined
{$ELSE PREFERAPI}
  MyCreateHardLinkW // ... otherwise this one
{$ENDIF PREFERAPI}
  (szLinkName, szLinkTarget: PWideChar; lpSecurityAttributes: PSecurityAttributes): BOOL;
const
  wcsC_NtName: PWideChar = '\??\C:';
  wcsLanMan: PWideChar = '\Device\LanmanRedirector\';
  cbC_NtName = $10;
  dwUnknownAccess = $110000;
var
  usNtName_LinkName, usNtName_LinkTarget, usNtFilePath: UNICODE_STRING;
  usCheckDrive, usSymLinkDrive, usLanMan: UNICODE_STRING;
  wcsNtName_LinkTarget, wcsFilePart_LinkTarget: PWideChar;
  oaMisc: OBJECT_ATTRIBUTES;
  IOStats: IO_STATUS_BLOCK;
  hHeap: Pointer;
  NeededSize: DWORD;
  Status: NTSTATUS;
  hLinkTarget, hDrive: THandle;
  lpFileLinkInfo: PFILE_LINK_INFORMATION;
begin
  Result := False;
{$IFDEF RTDL}
  if not bRtdlFunctionsLoaded then
    Exit;
{$ENDIF RTDL}
  // Get process' heap
  hHeap := NtMyGetProcessHeap;
  // If both are assigned ...
  if (szLinkName <> nil) and (szLinkTarget <> nil) then
    (
      // Determine DOS path type for both link name and target
      if (RtlDetermineDosPathNameType_U(szLinkName) <> UNC_PATH) and
        (RtlDetermineDosPathNameType_U(szLinkTarget) <> UNC_PATH) then
        (
          // Convert the link target into a UNICODE_STRING
          if RtlDosPathNameToNtPathName_U(szLinkTarget, usNtName_LinkTarget, nil, usNtFilePath) then
          try
            // Initialise the length members
            RtlInitUnicodeString(usNtName_LinkTarget, usNtName_LinkTarget.Buffer);
            // Get needed buffer size (in TCHARs)
            NeededSize := GetFullPathNameW(szLinkTarget, 0, nil, PWideChar(nil^));
            if NeededSize <> 0 then
            begin
              // Calculate needed size (in TCHARs)
              NeededSize := NeededSize + 1; // times 2
              // Freed in FINALLY
              wcsNtName_LinkTarget := RtlAllocateHeap(hHeap, HEAP_ZERO_MEMORY, NeededSize shl 1);
              // If successfully allocated buffer ...
              if wcsNtName_LinkTarget <> nil then
              try
                // Get the full unicode path name
                if GetFullPathNameW(szLinkTarget, NeededSize, wcsNtName_LinkTarget, wcsFilePart_LinkTarget) <> 0 then
                begin
                  // Allocate memory to check the drive object
                  usCheckDrive.Buffer := RtlAllocateHeap(hHeap, HEAP_ZERO_MEMORY, cbC_NtName);
                  // On success ...
                  if usCheckDrive.Buffer <> nil then
                  try
                    // Copy to buffer and set length members
                    lstrcpynW(usCheckDrive.Buffer, wcsC_NtName, lstrlenW(wcsC_NtName) + 1);
                    RtlInitUnicodeString(usCheckDrive, usCheckDrive.Buffer);
                    // Replace drive letter by the drive letter we want
                    usCheckDrive.Buffer[4] := wcsNtName_LinkTarget[0];
                    // Init OBJECT_ATTRIBUTES
                    oaMisc.Length := SizeOf(oaMisc);
                    oaMisc.RootDirectory := 0;
                    oaMisc.ObjectName := @usCheckDrive;
                    oaMisc.Attributes := OBJ_CASE_INSENSITIVE;
                    oaMisc.SecurityDescriptor := nil;
                    oaMisc.SecurityQualityOfService := nil;
                    // Open symbolic link object
                    if ZwOpenSymbolicLinkObject(hDrive, 1, oaMisc) = STATUS_SUCCESS then
                    try
                      usSymLinkDrive.Buffer := RtlAllocateHeap(hHeap, HEAP_ZERO_MEMORY, MAX_PATH * SizeOf(WideChar));
                      if usSymLinkDrive.Buffer <> nil then
                      try
                        // Query the path the symbolic link points to ...
                        ZwQuerySymbolicLinkObject(hDrive, usSymLinkDrive, nil);
                        // Initialise the length members
                        RtlInitUnicodeString(usLanMan, wcsLanMan);
                        // The path must not be a mapped drive ... check this!
                        if not RtlPrefixUnicodeString(usLanMan, usSymLinkDrive, True) then
                        begin
                          // Initialise OBJECT_ATTRIBUTES
                          oaMisc.Length := SizeOf(oaMisc);
                          oaMisc.RootDirectory := 0;
                          oaMisc.ObjectName := @usNtName_LinkTarget;
                          oaMisc.Attributes := OBJ_CASE_INSENSITIVE;
                          // Set security descriptor in OBJECT_ATTRIBUTES if they were given
                          if lpSecurityAttributes <> nil then
                            oaMisc.SecurityDescriptor := lpSecurityAttributes.lpSecurityDescriptor
                          else
                            oaMisc.SecurityDescriptor := nil;
                          oaMisc.SecurityQualityOfService := nil;
                          // Try open the target file
                          Status := ZwOpenFile(hLinkTarget, dwUnknownAccess, oaMisc, IOStats,
                            FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
                            FILE_SYNCHRONOUS_IO_NONALERT or FILE_OPEN_FOR_BACKUP_INTENT or FILE_OPEN_REPARSE_POINT);
                          if Status = STATUS_SUCCESS then
                          try
                            // Wow ... target opened ... let's try to
                            if RtlDosPathNameToNtPathName_U(szLinkName, usNtName_LinkName, nil, usNtFilePath) then
                            try
                              // Initialise the length members
                              RtlInitUnicodeString(usNtName_LinkName, usNtName_LinkName.Buffer);
                              // Now almost everything is done to create a link!
                              NeededSize := usNtName_LinkName.Length + SizeOf(FILE_LINK_INFORMATION) + SizeOf(WideChar);
                              lpFileLinkInfo := RtlAllocateHeap(hHeap, HEAP_ZERO_MEMORY, NeededSize);
                              if lpFileLinkInfo <> nil then
                              try
                                lpFileLinkInfo^.ReplaceIfExists := False;
                                lpFileLinkInfo^.RootDirectory := 0;
                                lpFileLinkInfo^.FileNameLength := usNtName_LinkName.Length;
                                lstrcpynW(lpFileLinkInfo.FileName, usNtName_LinkName.Buffer, usNtName_LinkName.Length);
                                // Hard link the file as intended
                                Status := ZwSetInformationFile(hLinkTarget, IOStats, lpFileLinkInfo, NeededSize, FileLinkInformation);
                                // On success return TRUE
                                if Status >= 0 then
                                  Result := True;
                              finally
                                // Free the buffer
                                RtlFreeHeap(hHeap, 0, lpFileLinkInfo);
                                // Set last error code
                                SetLastError(RtlNtStatusToDosError(Status));
                              end
                              else // if lpFileLinkInfo <> nil then
                                SetLastError(ERROR_NOT_ENOUGH_MEMORY);
                            finally
                              RtlFreeHeap(hHeap, 0, usNtName_LinkName.Buffer);
                            end
                            else // if RtlDosPathNameToNtPathName_U(szLinkName, usNtName_LinkName, nil, usNtFilePath) then
                              SetLastError(ERROR_INVALID_NAME);
                          finally
                            ZwClose(hLinkTarget);
                          end
                          else // if Status = STATUS_SUCCESS then
                            SetLastError(RtlNtStatusToDosError(Status));
                        end
                        else // if not RtlPrefixUnicodeString(usLanMan, usSymLinkDrive, True) then
                          SetLastError(ERROR_INVALID_NAME);
                      finally
                        RtlFreeHeap(hHeap, 0, usSymLinkDrive.Buffer);
                      end
                      else // if usSymLinkDrive.Buffer <> nil then
                        SetLastError(ERROR_NOT_ENOUGH_MEMORY);
                    finally
                      ZwClose(hDrive);
                    end;
                  finally
                    RtlFreeHeap(hHeap, 0, usCheckDrive.Buffer);
                  end
                  else // if usCheckDrive.Buffer <> nil then
                    SetLastError(ERROR_NOT_ENOUGH_MEMORY);
                end
                else // if GetFullPathNameW(lpExistingFileName, NeededSize shr 2, pwsPathName, pwsFilePart)<>0 then
                  SetLastError(ERROR_INVALID_NAME);
              finally
                RtlFreeHeap(hHeap, 0, wcsNtName_LinkTarget);
              end
              else // if p <> nil then
                SetLastError(ERROR_NOT_ENOUGH_MEMORY);
            end
            else // if NeededSize <> 0 then
              SetLastError(ERROR_INVALID_NAME);
          finally
            RtlFreeHeap(hHeap, 0, usNtName_LinkTarget.Buffer);
          end
          else // if RtlDosPathNameToNtPathName_U(lpExistingFileName, NtName, nil, NtFilePath) then
            SetLastError(ERROR_PATH_NOT_FOUND))
      else // if (RtlDetermineDosPathNameType_U(szLinkName) <> UNC_PATH) and ...
        SetLastError(ERROR_INVALID_NAME))
  else // if (lpFileName <> nil) and (lpExistingFileName <> nil) then
    SetLastError(ERROR_INVALID_PARAMETER);
end;

(******************************************************************************
 Hint:
 -----
  For all closer information see the CreateHardLinkW function above.

 Specific to the ANSI-version:
 -----------------------------
  The ANSI-Version can be used as if it were used on Windows 2000. This holds
  for all supported systems for now.

 ******************************************************************************)

function
{$IFNDEF PREFERAPI}
  CreateHardLinkA // This name is directly published if PREFERAPI is not defined
{$ELSE PREFERAPI}
  MyCreateHardLinkA // ... otherwise this one
{$ENDIF PREFERAPI}
  (szLinkName, szLinkTarget: PAnsiChar; lpSecurityAttributes: PSecurityAttributes): BOOL;
var
  usLinkName: UNICODE_STRING;
  usLinkTarget: UNICODE_STRING;
  hHeap: Pointer;
begin
  Result := False;
{$IFDEF RTDL}
  if not bRtdlFunctionsLoaded then
    Exit;
{$ENDIF RTDL}
  // Get the process' heap
  hHeap := NtMyGetProcessHeap;
  // Create and allocate a UNICODE_STRING from the zero-terminated parameters
  if RtlCreateUnicodeStringFromAsciiz(usLinkName, szLinkName) and
    RtlCreateUnicodeStringFromAsciiz(usLinkTarget, szLinkTarget) then
  try
    // Call the Unicode version
    Result := CreateHardLinkW(usLinkName.Buffer, usLinkTarget.Buffer, lpSecurityAttributes);
  finally
    // free the allocated buffers
    RtlFreeHeap(hHeap, 0, usLinkName.Buffer);
    RtlFreeHeap(hHeap, 0, usLinkTarget.Buffer);
  end;
end;

{$IFDEF RTDL}
const
// Names of the functions to import
  szRtlCreateUnicodeStringFromAsciiz = 'RtlCreateUnicodeStringFromAsciiz';
  szZwClose                          = 'ZwClose';
  szZwSetInformationFile             = 'ZwSetInformationFile';
  szRtlPrefixUnicodeString           = 'RtlPrefixUnicodeString';
  szZwOpenSymbolicLinkObject         = 'ZwOpenSymbolicLinkObject';
  szZwQuerySymbolicLinkObject        = 'ZwQuerySymbolicLinkObject';
  szZwOpenFile                       = 'ZwOpenFile';
  szRtlAllocateHeap                  = 'RtlAllocateHeap';
  szRtlFreeHeap                      = 'RtlFreeHeap';
  szRtlDosPathNameToNtPathName_U     = 'RtlDosPathNameToNtPathName_U';
  szRtlInitUnicodeString             = 'RtlInitUnicodeString';
  szRtlDetermineDosPathNameType_U    = 'RtlDetermineDosPathNameType_U';
  szRtlNtStatusToDosError            = 'RtlNtStatusToDosError';
  szZwQueryDirectoryFile             = 'ZwQueryDirectoryFile';
  szZwQueryInformationFile           = 'ZwQueryInformationFile';
{$ENDIF RTDL}

{$IFDEF PREFERAPI}
var
  hKernel32: THandle = 0;
{$ENDIF PREFERAPI}

initialization
{$IFDEF PREFERAPI}
  // GetModuleHandle because this DLL is loaded into any Win32 subsystem process anyway
  // implicitly. And Delphi cannot create applications for other subsystems without
  // major changes in SysInit und System units.
  hKernel32 := GetModuleHandle(kernel32);
  // If we prefer the real Windows APIs try to get their addresses
  @CreateHardLinkA := GetProcAddress(hKernel32, szCreateHardLinkA);
  @CreateHardLinkW := GetProcAddress(hKernel32, szCreateHardLinkW);
  // If they could not be retrieved resort to our home-grown version
  if not (Assigned(@CreateHardLinkA) and Assigned(@CreateHardLinkW)) then
  begin
{$ENDIF}

{$IFDEF RTDL}
  // GetModuleHandle because this DLL is loaded into any Win32 subsystem process anyway
  // implicitly. And Delphi cannot create applications for other subsystems without
  // major changes in SysInit und System units.
  hNtDll := GetModuleHandle(szNtDll);
  if hNtDll <> 0 then
  begin
    // Get all the function addresses
    @RtlCreateUnicodeStringFromAsciiz := GetProcAddress(hNtDll, szRtlCreateUnicodeStringFromAsciiz);
    @ZwClose := GetProcAddress(hNtDll, szZwClose);
    @ZwSetInformationFile := GetProcAddress(hNtDll, szZwSetInformationFile);
    @RtlPrefixUnicodeString := GetProcAddress(hNtDll, szRtlPrefixUnicodeString);
    @ZwOpenSymbolicLinkObject := GetProcAddress(hNtDll, szZwOpenSymbolicLinkObject);
    @ZwQuerySymbolicLinkObject := GetProcAddress(hNtDll, szZwQuerySymbolicLinkObject);
    @ZwOpenFile := GetProcAddress(hNtDll, szZwOpenFile);
    @RtlAllocateHeap := GetProcAddress(hNtDll, szRtlAllocateHeap);
    @RtlFreeHeap := GetProcAddress(hNtDll, szRtlFreeHeap);
    @RtlDosPathNameToNtPathName_U := GetProcAddress(hNtDll, szRtlDosPathNameToNtPathName_U);
    @RtlInitUnicodeString := GetProcAddress(hNtDll, szRtlInitUnicodeString);
    @RtlDetermineDosPathNameType_U := GetProcAddress(hNtDll, szRtlDetermineDosPathNameType_U);
    @RtlNtStatusToDosError := GetProcAddress(hNtDll, szRtlNtStatusToDosError);
    @ZwQueryDirectoryFile := GetProcAddress(hNtDll, szZwQueryDirectoryFile);
    @ZwQueryInformationFile := GetProcAddress(hNtDll, szZwQueryInformationFile);
    // Check whether we could retrieve all of them
    bRtdlFunctionsLoaded := // Update the "loaded" status
      Assigned(@RtlCreateUnicodeStringFromAsciiz) and
      Assigned(@ZwClose) and
      Assigned(@ZwSetInformationFile) and
      Assigned(@RtlPrefixUnicodeString) and
      Assigned(@ZwOpenSymbolicLinkObject) and
      Assigned(@ZwQuerySymbolicLinkObject) and
      Assigned(@ZwOpenFile) and
      Assigned(@RtlAllocateHeap) and
      Assigned(@RtlFreeHeap) and
      Assigned(@RtlDosPathNameToNtPathName_U) and
      Assigned(@RtlInitUnicodeString) and
      Assigned(@RtlDetermineDosPathNameType_U) and
      Assigned(@RtlNtStatusToDosError) and
      Assigned(@ZwQueryDirectoryFile) and
      Assigned(@ZwQueryInformationFile);
  end;
{$ENDIF RTDL}

{$IFDEF PREFERAPI}
    @CreateHardLinkA := @MyCreateHardLinkA;
    @CreateHardLinkW := @MyCreateHardLinkW;
  end; // if not (Assigned(@CreateHardLinkA) and Assigned(@CreateHardLinkW)) then ...
{$ENDIF PREFERAPI}

//--------------------------------------------------------------------------------------------------

// History:

{$IFDEF PROTOTYPE}
// $Log$
// Revision 1.3  2004/10/21 21:58:03  assarbad
// - minimal changes in the prototype
//   (change of the filename for the release version on assarbad.net
//    Hardlink.pas -> Hardlinks.pas
//    The JCL prototype is now reference for "my" release version)
// - creation of new unit from style-cleaned prototype
//
// Revision 1.2  2004/10/21 17:53:03  marquardt
// style cleaning
//
// Revision 1.1  2004/10/20 19:49:00  rrossmair
// - added prototype unit Hardlinks (formerly known as Hardlink)
// - modified makefile accordingly
//
{$ENDIF PROTOTYPE}

{
   Version 1.12a - 2004-10-21
   + "Original" file renamed according to the change in the JCL prototype
     Hardlink.pas -> Hardlinks.pas
   + The original version is now being created using:
     jpp -c -uJCL -dMSWINDOWS -uUNIX -uHAS_UNIT_LIBC -x..\ Hardlinks.pas
   + Changes will first occur in this prototype and the output of the
     preprocessor undefining the "JCL" symbol will be mirrored to my site
     afterwards. The prototype at the JCL is the reference from now on.

   Version 1.12 - 2004-10-18
   + Code-cleaning (removal of the currently not working softlink stuff from 1.10)
   + Comments for Project JEDI (JCL)
   + Some extra declarations to be compatible with JclNTFS
   + Runtime dynamic linking
   + Checked into the JCL

   Version 1.11 - 2004-07-01
   + Bugfix from Nico Bendlin - Odd behavior of NtMyGetProcessHeap()

  ! Version 1.10 - 2004-04-16 [this was taken out again in 1.12]
  ! + Implemented softlinks for directories (junction points/reparse points)

   Version 1.01 - 2003-08-25
   + Implemented hardlinks
}

end.

