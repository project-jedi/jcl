{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}


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

// Last modified: $Date$
// For history see end of file

unit Hardlinks;

{$ALIGN ON}
{$MINENUMSIZE 4}

interface
// ALL enabled by default for Project JEDI
   // Make functions STDCALL always
      // Use runtime dynamic linking
 // Prefer the "real" Windows API on systems on which it exists
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

   // For the windows API we _require_ STDCALL calling convention

{$EXTERNALSYM CreateHardLinkW}
{$EXTERNALSYM CreateHardLinkA}

// Well, we did not decide yet ;) - bind to either address, depending on whether
// the API could be found.
type
  TFNCreateHardLinkW = function(szLinkName, szLinkTarget: PWideChar; lpSecurityAttributes: PSecurityAttributes): BOOL; stdcall; 
  TFNCreateHardLinkA = function(szLinkName, szLinkTarget: PAnsiChar; lpSecurityAttributes: PSecurityAttributes): BOOL; stdcall; 
var
  CreateHardLinkW : TFNCreateHardLinkW = nil;
  CreateHardLinkA : TFNCreateHardLinkA = nil;

var
  hNtDll : THandle = 0; // For runtime dynamic linking
  bRtdlFunctionsLoaded : Boolean = False; // To show whether the RTDL functions had been loaded

implementation

const
  szNtDll           = 'NTDLL.DLL'; // Import native APIs from this DLL
  szCreateHardLinkA = 'CreateHardLinkA';
  szCreateHardLinkW = 'CreateHardLinkW';

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

type
// =================================================================
// Type definitions
// =================================================================
  NTSTATUS = Longint;

const
// =================================================================
// Constants
// =================================================================
  FileDirectoryInformation = 1;
  FileLinkInformation = 11;
  FILE_SYNCHRONOUS_IO_NONALERT = $00000020;
  FILE_OPEN_FOR_BACKUP_INTENT = $00004000;
  FILE_OPEN_REPARSE_POINT = $00200000;
  FILE_LIST_DIRECTORY = 1;
  FILE_DIRECTORY_FILE = $00000001;
  STATUS_SUCCESS = NTSTATUS(0);
  STATUS_NO_MORE_FILES = NTSTATUS($80000006);
  OBJ_CASE_INSENSITIVE = $00000040;

// Should be defined, but isn't
  HEAP_ZERO_MEMORY = $00000008;

// Related constant(s) for RtlDetermineDosPathNameType_U()
  INVALID_PATH = 0;
  UNC_PATH = 1;
  ABSOLUTE_DRIVE_PATH = 2;
  RELATIVE_DRIVE_PATH = 3;
  ABSOLUTE_PATH = 4;
  RELATIVE_PATH = 5;
  DEVICE_PATH = 6;
  UNC_DOT_PATH = 7;

  MAXIMUM_REPARSE_DATA_BUFFER_SIZE = 16 * 1024;
  IO_REPARSE_TAG_MOUNT_POINT = $A0000003;
  FSCTL_SET_REPARSE_POINT = $900A4;
  REPARSE_DATA_BUFFER_HEADER_SIZE = 2 * sizeof(DWORD);

type
// =================================================================
// Type definitions
// =================================================================
  LARGE_INTEGER = TLargeInteger;
  PLARGE_INTEGER = ^LARGE_INTEGER;

  _UNICODE_STRING = record
    Length,
      MaximumLength: Word;
    Buffer: PWideChar;
  end;
  UNICODE_STRING = _UNICODE_STRING;
  PUNICODE_STRING = ^_UNICODE_STRING;

  _ANSI_STRING = record
    Length,
      MaximumLength: Word;
    Buffer: PAnsiChar;
  end;
  ANSI_STRING = _ANSI_STRING;
  PANSI_STRING = ^_ANSI_STRING;


  _REPARSE_DATA_BUFFER = record
    ReparseTag: DWORD;
    ReparseDataLength: WORD;
    Reserved: WORD;
    case integer of
      0: (
        SubstituteNameOffset: WORD;
        SubstituteNameLength: WORD;
        PrintNameOffset: WORD;
        PrintNameLength: WORD;
        PathBuffer: array[0..0] of WideChar;
        );
      1: (
        DataBuffer: array[0..0] of BYTE;
        );
  end;
  REPARSE_DATA_BUFFER = _REPARSE_DATA_BUFFER;
  PREPARSE_DATA_BUFFER = ^_REPARSE_DATA_BUFFER;


  _OBJECT_ATTRIBUTES = record
    Length: ULONG;
    RootDirectory: THandle;
    ObjectName: PUNICODE_STRING;
    Attributes: ULONG;
    SecurityDescriptor: Pointer; // Points to type SECURITY_DESCRIPTOR
    SecurityQualityOfService: Pointer; // Points to type SECURITY_QUALITY_OF_SERVICE
  end;
  OBJECT_ATTRIBUTES = _OBJECT_ATTRIBUTES;
  POBJECT_ATTRIBUTES = ^_OBJECT_ATTRIBUTES;

  _IO_STATUS_BLOCK = record
    case integer of
      0: (Status: NTSTATUS);
      1: (Pointer: Pointer;
        Information: ULONG);
  end;
  IO_STATUS_BLOCK = _IO_STATUS_BLOCK;
  PIO_STATUS_BLOCK = ^_IO_STATUS_BLOCK;

  _FILE_LINK_RENAME_INFORMATION = record // File Information Classes 10 and 11
    ReplaceIfExists: BOOL;
    RootDirectory: THandle;
    FileNameLength: ULONG;
    FileName: array[0..0] of WideChar;
  end;
  FILE_LINK_INFORMATION = _FILE_LINK_RENAME_INFORMATION;
  PFILE_LINK_INFORMATION = ^_FILE_LINK_RENAME_INFORMATION;
  FILE_RENAME_INFORMATION = _FILE_LINK_RENAME_INFORMATION;
  PFILE_RENAME_INFORMATION = ^_FILE_LINK_RENAME_INFORMATION;

  _FILE_DIRECTORY_INFORMATION = record // File Information Class 1
    NextEntryOffset,
      Unknown: ULONG;
    CreationTime,
      LastAccessTime,
      LastWriteTime,
      ChangeTime,
      EndOfFile,
      AllocationSize: LARGE_INTEGER;
    FileAttributes,
      FileNameLength: ULONG;
    FileName: array[0..0] of WideChar;
  end;
  FILE_DIRECTORY_INFORMATION = _FILE_DIRECTORY_INFORMATION;
  PFILE_DIRECTORY_INFORMATION = ^_FILE_DIRECTORY_INFORMATION;

// =================================================================
// PROCESS ENVIRONMENT BLOCK (PEB)
// =================================================================

  _MODULE_HEADER = record
{000} d000,
{004} d004: DWORD;
{008} List1,
{010} List2,
{018} List3: LIST_ENTRY;
  end;
  MODULE_HEADER = _MODULE_HEADER;
  PMODULE_HEADER = ^_MODULE_HEADER;
  PPMODULE_HEADER = ^PMODULE_HEADER;

// -----------------------------------------------------------------

  _PROCESS_MODULE_INFO = record
{000} Size: DWORD;
{004} ModuleHeader: MODULE_HEADER;
  end;
  PROCESS_MODULE_INFO = _PROCESS_MODULE_INFO;
  PPROCESS_MODULE_INFO = ^_PROCESS_MODULE_INFO;
  PPPROCESS_MODULE_INFO = ^PPROCESS_MODULE_INFO;

// -----------------------------------------------------------------

  _PROCESS_PARAMETERS = record
{000} Allocated,
{004} Size,
{008} Flags, // bit 0: all pointers normalized
{00C} DebugFlags: DWORD;
{010} Console: THandle;
{014} ProcessGroup: DWORD;
{018} StdInput,
{01C} StdOutput,
{020} StdError: THandle;
{024} WorkingDirectoryName: UNICODE_STRING;
{02C} WorkingDirectoryHandle: THandle;
{030} SearchPath,
{038} ImagePath,
{040} CommandLine: UNICODE_STRING;
{048} Environment: PWideChar;
{04C} X,
{050} Y,
{054} XSize,
{058} YSize,
{05C} XCountChars,
{060} YCountChars,
{064} FillAttribute,
{068} Flags2: DWORD;
{06C} ShowWindow,
{06E} Reserved2: Word;
{070} Title,
{078} Desktop,
{080} Reserved3,
{088} Reserved4: UNICODE_STRING;
  end;
  PROCESS_PARAMETERS = _PROCESS_PARAMETERS;
  PPROCESS_PARAMETERS = ^_PROCESS_PARAMETERS;
  PPPROCESS_PARAMETERS = ^PPROCESS_PARAMETERS;

// -----------------------------------------------------------------

  _SYSTEM_STRINGS = record
{000} SystemRoot, // %SystemRoot%
{008} System32Root, // %SystemRoot%\System32
{010} BaseNamedObjects // \BaseNamedObjects
      : UNICODE_STRING;
  end;
  SYSTEM_STRINGS = _SYSTEM_STRINGS;
  PSYSTEM_STRINGS = ^_SYSTEM_STRINGS;
  PPSYSTEM_STRINGS = ^PSYSTEM_STRINGS;

// -----------------------------------------------------------------

  _TEXT_INFO = record
{000} Reserved: Pointer;
{004} SystemStrings: PSYSTEM_STRINGS;
  end;
  TEXT_INFO = _TEXT_INFO;
  PTEXT_INFO = ^_TEXT_INFO;
  PPTEXT_INFO = ^PTEXT_INFO;

// -----------------------------------------------------------------

  PPEB_FREE_BLOCK = ^_PEB_FREE_BLOCK;
  _PEB_FREE_BLOCK = record
{000} Next: PPEB_FREE_BLOCK;
{004} Size: ULONG;
  end;
  PEB_FREE_BLOCK = _PEB_FREE_BLOCK;

  _RTL_BITMAP = record
{000} SizeOfBitMap: DWORD;
{004} Buffer: PDWORD;
  end;
  RTL_BITMAP = _RTL_BITMAP;
  PRTL_BITMAP = ^_RTL_BITMAP;
  PPRTL_BITMAP = ^PRTL_BITMAP;

  KAFFINITY = DWORD;
  PKAFFINITY = ^KAFFINITY;

  _CLIENT_ID = record
{000} UniqueProcess,
{004} UniqueThread: THandle;
  end;
  CLIENT_ID = _CLIENT_ID;
  PCLIENT_ID = ^_CLIENT_ID;


  PCRITICAL_SECTION = PRtlCriticalSection;
  PPEBLOCKROUTINE = procedure(PebLock: PCRITICAL_SECTION); stdcall;
  PFarProc = ^TFarProc;

// -----------------------------------------------------------------
// located at 0x7FFDF000

  _PEB = record
{000} InheritedAddressSpace,
{001} ReadImageFileExecOptions,
{002} BeingDebugged,
{003} bReserved: Boolean;
{004} Mutant: Pointer; // THandle
{008} SectionBaseAddress: Pointer;
{00C} ProcessModuleInfo: PPROCESS_MODULE_INFO;
{010} ProcessParameters: PPROCESS_PARAMETERS;
{014} SubSystemData: DWORD;
{018} ProcessHeap: Pointer; // THandle
{01C} FastPebLock: PCRITICAL_SECTION;
{020} AcquireFastPebLock, // function
{024} ReleaseFastPebLock: PPEBLOCKROUTINE; // function
{028} EnvironmentUpdateCount: DWORD;
{02C} KernelCallbackTable: PFarProc; // function
{030} EventLogSection: Pointer; // THandle
{034} EventLog: Pointer; // THandle
{038} FreeList: PPEB_FREE_BLOCK;
{03C} TlsBitMapSize: DWORD; // number of bits
{040} TlsBitMap: PRTL_BITMAP; // ntdll!TlsBitMap
{044} TlsBitMapData: array[0..1] of DWORD; // 64 bits
{04C} ReadOnlySharedMemoryBase,
{050} ReadOnlySharedMemoryHeap: Pointer;
{054} TextInfo: PTEXT_INFO;
{058} InitAnsiCodePageData,
{05C} InitOemCodePageData,
{060} InitUnicodeCaseTableData: Pointer;
{064} KeNumberProcessors,
{068} NtGlobalFlag,
{06C} Reserved: array[0..3] of Byte;
{070} MmCriticalSectionTimeout: LARGE_INTEGER;
{078} MmHeapSegmentReserve,
{07C} MmHeapSegmentCommit,
{080} MmHeapDeCommitTotalFreeThreshold,
{084} MmHeapDeCommitFreeBlockThreshold,
{088} NumberOfHeaps,
{08C} AvailableHeaps: DWORD; // 16, *2 if exhausted
{090} ProcessHeapsListBuffer: PHandle;
{094} GdiSharedHandleTable,
{098} ProcessStarterHelper,
{09C} GdiDCAttributeList: Pointer;
{0A0} LoaderLock: PCRITICAL_SECTION;
{0A4} NtMajorVersion,
{0A8} NtMinorVersion: DWORD;
{0AC} NtBuildNumber,
{0AE} CmNtCSDVersion: Word;
{0B0} PlatformId,
{0B4} Subsystem,
{0B8} MajorSubsystemVersion,
{0BC} MinorSubsystemVersion: DWORD;
{0C0} AffinityMask: KAFFINITY;
{0C4} ad0C4: array[0..33] of DWORD;
{14C} PostProcessInitRoutine: ^Pointer;
{150} TlsExpansionBitmap: ULONG;
{154} TlsExpansionBitmapBits: array[0..$80 - 1] of Byte;
{1D4} Win32WindowStation: THandle; // aka SessionId???
{1D8} d1D8,
{1DC} d1DC: DWORD;
{1E0} CSDVersion: PWord;
{1E4} d1E4: DWORD;
  end;
  PEB = _PEB;
  PPEB = ^_PEB;
  PPPEB = ^PPEB;

// =================================================================
// THREAD ENVIRONMENT BLOCK (TEB)
// =================================================================

  PNT_TIB = ^_NT_TIB;
  _NT_TIB = record
{000} ExceptionList: Pointer; // ^_EXCEPTION_REGISTRATION_RECORD
{004} StackBase,
{008} StackLimit,
{00C} SubSystemTib: Pointer;
{010} _union: record
      case Integer of
  {010} 0: (FiberData: Pointer);
  {010} 1: (Version: ULONG);
    end;
{014} ArbitraryUserPointer: Pointer;
{018} Self: PNT_TIB;
  end;
  NT_TIB = _NT_TIB;
  PPNT_TIB = ^PNT_TIB;

// -----------------------------------------------------------------
// located at 0x7FFDE000, 0x7FFDD000, ...

  _TEB = record
{000} Tib: NT_TIB;
{01C} EnvironmentPointer: Pointer;
{020} ClientId: CLIENT_ID;
{028} RpcHandle: THandle;
{02C} ThreadLocalStorage: ^Pointer;
{030} Peb: PPEB;
{034} LastErrorValue: DWORD;
  end;
  TEB = _TEB;
  PTEB = ^_TEB;
  PPTEB = ^PTEB;

// =================================================================
// Function prototypes
// =================================================================

TRtlCreateUnicodeStringFromAsciiz = function(
  var destination: UNICODE_STRING;
  source: PAnsiChar
  ): Boolean; stdcall;

TZwClose = function(
  Handle: THandle
  ): NTSTATUS; stdcall;

TZwSetInformationFile = function(
  FileHandle: THandle;
  IoStatusBlock: PIO_STATUS_BLOCK;
  FileInformation: Pointer;
  FileInformationLength: ULONG;
  FileInformationClass: DWORD
  ): NTSTATUS;
  stdcall;

TRtlPrefixUnicodeString = function(
  const usPrefix: UNICODE_STRING;
  const usContainingString: UNICODE_STRING;
  ignore_case: Boolean): Boolean; stdcall;

TZwOpenSymbolicLinkObject = function(
  var LinkHandle: THandle;
  DesiredAccess: DWORD;
  ObjectAttributes: POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;

TZwQuerySymbolicLinkObject = function(
  LinkHandle: THandle;
  var LinkTarget: UNICODE_STRING;
  var ReturnedLength: ULONG
  ): NTSTATUS; stdcall;

TZwOpenFile = function(
  var FileHandle: THandle;
  DesiredAccess: DWORD;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  var IoStatusBlock: IO_STATUS_BLOCK;
  ShareAccess: ULONG;
  OpenOptions: ULONG
  ): NTSTATUS; stdcall;

TRtlAllocateHeap = function(
  HeapHandle: Pointer;
  Flags,
  Size: ULONG
  ): Pointer; stdcall;

TRtlFreeHeap = function(
  HeapHandle: Pointer;
  Flags: ULONG;
  MemoryPointer: Pointer
  ): Boolean; stdcall;

TRtlDosPathNameToNtPathName_U = function(
  DosName: PWideChar;
  var NtName: UNICODE_STRING;
  var DosFilePath: PWideChar;
  var NtFilePath: UNICODE_STRING
  ): Boolean; stdcall;

TRtlInitUnicodeString = function(
  var DestinationString: UNICODE_STRING;
  const SourceString: PWideChar
  ): NTSTATUS; stdcall;

TRtlDetermineDosPathNameType_U = function(
  wcsPathNameType: PWideChar
  ): DWORD; stdcall;

TRtlNtStatusToDosError = function(
  status: NTSTATUS
  ): ULONG; stdcall;

TZwQueryDirectoryFile = function(
  FileHandle,
  Event: THandle;
  ApcRoutine: Pointer;
  ApcContext: Pointer;
  var IoStatusBlock: IO_STATUS_BLOCK;
  FileInformation: Pointer;
  FileInformationLength: ULONG;
  FileInformationClass: DWORD;
  ReturnSingleEntry: Boolean;
  var FileName: UNICODE_STRING;
  RestartScan: Boolean
  ): NTSTATUS; stdcall;

TZwQueryInformationFile = function(
  FileHandle: THandle;
  var IoStatusBlock: IO_STATUS_BLOCK;
  FileInformation: Pointer;
  FileInformationLength: ULONG;
  FileInformationClass: DWORD
  ): NTSTATUS; stdcall;

var
// Declare all the _global_ function pointers for RTDL
  RtlCreateUnicodeStringFromAsciiz : TRtlCreateUnicodeStringFromAsciiz = nil;
  ZwClose                          : TZwClose = nil;
  ZwSetInformationFile             : TZwSetInformationFile = nil;
  RtlPrefixUnicodeString           : TRtlPrefixUnicodeString = nil;
  ZwOpenSymbolicLinkObject         : TZwOpenSymbolicLinkObject = nil;
  ZwQuerySymbolicLinkObject        : TZwQuerySymbolicLinkObject = nil;
  ZwOpenFile                       : TZwOpenFile = nil;
  RtlAllocateHeap                  : TRtlAllocateHeap = nil;
  RtlFreeHeap                      : TRtlFreeHeap = nil;
  RtlDosPathNameToNtPathName_U     : TRtlDosPathNameToNtPathName_U = nil;
  RtlInitUnicodeString             : TRtlInitUnicodeString = nil;
  RtlDetermineDosPathNameType_U    : TRtlDetermineDosPathNameType_U = nil;
  RtlNtStatusToDosError            : TRtlNtStatusToDosError = nil;
  ZwQueryDirectoryFile             : TZwQueryDirectoryFile = nil;
  ZwQueryInformationFile           : TZwQueryInformationFile = nil;

function NtMyGetProcessHeap: Pointer; assembler;
asm
// To understand this a deep understanding of the NT Native API and its
// structures is required.
  mov   EAX, FS:[0]._TEB.Peb // FS points to TEB/TIB which has a pointer to the PEB
  mov   EAX, [EAX]._PEB.ProcessHeap // Get the process heap's handle
end;

function
  MyCreateHardLinkW // ... otherwise this one
  (
  szLinkName,
  szLinkTarget: PWideChar;
  lpSecurityAttributes: PSecurityAttributes
  ): BOOL;
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
var
  usNtName_LinkName,
    usNtName_LinkTarget,
    usNtFilePath,
    usCheckDrive,
    usSymLinkDrive,
    usLanMan: UNICODE_STRING;
  wcsNtName_LinkTarget,
    wcsFilePart_LinkTarget: PWideChar;
  oaMisc: OBJECT_ATTRIBUTES;

  iostats: IO_STATUS_BLOCK;

  hHeap: Pointer;
  neededsize: DWORD;
  status: NTSTATUS;

  hLinkTarget,
    hDrive: THandle;

  lpFileLinkInfo: PFILE_LINK_INFORMATION;
const
  wcsC_NtName: PWideChar = '\??\C:';
  wcsLanMan: PWideChar = '\Device\LanmanRedirector\';
  cbC_NtName = $10;
  dwUnknownAccess = $110000;
begin
  result := False;
  if not bRtdlFunctionsLoaded then
    Exit;
// Get process' heap
  hHeap := NtMyGetProcessHeap;
// If both are assigned ...
  if ((szLinkName <> nil) and (szLinkTarget <> nil)) then
    (
// Determine DOS path type for both link name and target
      if ((RtlDetermineDosPathNameType_U(szLinkName) <> UNC_PATH) and
        (RtlDetermineDosPathNameType_U(szLinkTarget) <> UNC_PATH)) then
        (
// Convert the link target into a UNICODE_STRING
          if RtlDosPathNameToNtPathName_U(szLinkTarget, usNtName_LinkTarget, PWideChar(nil^), usNtFilePath) then
          try
// Initialise the length members
            RtlInitUnicodeString(usNtName_LinkTarget, usNtName_LinkTarget.Buffer);
// Get needed buffer size (in TCHARs)
            neededsize := GetFullPathNameW(szLinktarget, 0, nil, PWideChar(nil^));
            if neededsize <> 0 then
            begin
// Calculate needed size (in TCHARs)
              neededsize := (neededsize + 1); // times 2
// Freed in FINALLY
              wcsNtName_LinkTarget := RtlAllocateHeap(hHeap, HEAP_ZERO_MEMORY, neededsize shl 1);
// If successfully allocated buffer ...
              if wcsNtName_LinkTarget <> nil then
              try
// Get the full unicode path name
                if GetFullPathNameW(szLinkTarget, neededsize, wcsNtName_LinkTarget, wcsFilePart_LinkTarget) <> 0 then
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
                    oaMisc.Length := sizeof(oaMisc);
                    oaMisc.RootDirectory := 0;
                    oaMisc.ObjectName := @usCheckDrive;
                    oaMisc.Attributes := OBJ_CASE_INSENSITIVE;
                    oaMisc.SecurityDescriptor := nil;
                    oaMisc.SecurityQualityOfService := nil;
// Open symbolic link object
                    if ZwOpenSymbolicLinkObject(hDrive, 1, @oaMisc) = STATUS_SUCCESS then
                    try
                      usSymLinkDrive.Buffer := RtlAllocateHeap(hHeap, HEAP_ZERO_MEMORY, MAX_PATH * sizeof(WideChar));
                      if usSymLinkDrive.Buffer <> nil then
                      try
// Query the path the symbolic link points to ...
                        ZwQuerySymbolicLinkObject(hDrive, usSymLinkDrive, DWORD(nil^));
// Initialise the length members
                        RtlInitUnicodeString(usLanMan, wcsLanMan);
// The path must not be a mapped drive ... check this!
                        if not RtlPrefixUnicodeString(usLanMan, usSymLinkDrive, True) then
                        begin
// Initialise OBJECT_ATTRIBUTES
                          oaMisc.Length := sizeof(oaMisc);
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
                          status := ZwOpenFile(
                            hLinkTarget,
                            dwUnknownAccess,
                            @oaMisc,
                            iostats,
                            FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
                            FILE_SYNCHRONOUS_IO_NONALERT or FILE_OPEN_FOR_BACKUP_INTENT or FILE_OPEN_REPARSE_POINT
                            );
                          if status = STATUS_SUCCESS then
                          try
// Wow ... target opened ... let's try to
                            if RtlDosPathNameToNtPathName_U(szLinkName, usNtName_LinkName, PWideChar(nil^), usNtFilePath) then
                            try
// Initialise the length members
                              RtlInitUnicodeString(usNtName_LinkName, usNtName_LinkName.Buffer);
// Now almost everything is done to create a link!
                              neededsize := usNtName_LinkName.Length + sizeof(FILE_LINK_INFORMATION) + sizeof(WideChar);
                              lpFileLinkInfo := RtlAllocateHeap(hHeap, HEAP_ZERO_MEMORY, neededsize);
                              if lpFileLinkInfo <> nil then
                              try
                                lpFileLinkInfo^.ReplaceIfExists := False;
                                lpFileLinkInfo^.RootDirectory := 0;
                                lpFileLinkInfo^.FileNameLength := usNtName_LinkName.Length;
                                lstrcpynW(lpFileLinkInfo.FileName, usNtName_LinkName.Buffer, usNtName_LinkName.Length);
// Hard link the file as intended
                                status := ZwSetInformationFile(hLinkTarget, @iostats, lpFileLinkInfo, neededsize, FileLinkInformation);
// On success return TRUE
                                if status >= 0 then
                                  result := True;
                              finally
// Free the buffer
                                RtlFreeHeap(hHeap, 0, lpFileLinkInfo);
// Set last error code
                                SetLastError(RtlNtStatusToDosError(status));
                              end
                              else // if lpFileLinkInfo <> nil then
                                SetLastError(ERROR_NOT_ENOUGH_MEMORY);
                            finally
                              RtlFreeHeap(hHeap, 0, usNtName_LinkName.Buffer);
                            end
                            else // if RtlDosPathNameToNtPathName_U(szLinkName, usNtName_LinkName, PWideChar(nil^), usNtFilePath) then
                              SetLastError(ERROR_INVALID_NAME);
                          finally
                            ZwClose(hLinkTarget);
                          end
                          else // if status = STATUS_SUCCESS then
                            SetLastError(RtlNtStatusToDosError(status));
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
                else // if GetFullPathNameW(lpExistingFileName, neededsize shr 2, pwsPathName, pwsFilePart)<>0 then
                  SetLastError(ERROR_INVALID_NAME);
              finally
                RtlFreeHeap(hHeap, 0, wcsNtName_LinkTarget);
              end
              else // if p <> nil then
                SetLastError(ERROR_NOT_ENOUGH_MEMORY);
            end
            else // if neededsize <> 0 then
              SetLastError(ERROR_INVALID_NAME);
          finally
            RtlFreeHeap(hHeap, 0, usNtName_LinkTarget.Buffer);
          end
          else // if RtlDosPathNameToNtPathName_U(lpExistingFileName, NtName, PWideChar(nil^), NtFilePath) then
            SetLastError(ERROR_PATH_NOT_FOUND))
      else // if ((RtlDetermineDosPathNameType_U(szLinkName) <> UNC_PATH) and ...
        SetLastError(ERROR_INVALID_NAME))
  else // if ((lpFileName <> nil) and (lpExistingFileName <> nil)) then
    SetLastError(ERROR_INVALID_PARAMETER);
end;

function 
  MyCreateHardLinkA // ... otherwise this one
  (
  szLinkName,
  szLinkTarget: PAnsiChar;
  lpSecurityAttributes: PSecurityAttributes
  ): BOOL;
(******************************************************************************
 Hint:
 -----
  For all closer information see the CreateHardLinnkW function above.

 Specific to the ANSI-version:
 -----------------------------
  The ANSI-Version can be used as if it were used on Windows 2000. This holds
  for all supported systems for now.

 ******************************************************************************)
var
  usLinkName: UNICODE_STRING;
  usLinkTarget: UNICODE_STRING;
  hHeap: Pointer;
begin
  result := False;
  if not bRtdlFunctionsLoaded then
    Exit;
// Get the process' heap
  hHeap := NtMyGetProcessHeap;
// Create and allocate a UNICODE_STRING from the zero-terminated parameters
  if RtlCreateUnicodeStringFromAsciiz(usLinkName, szLinkName) and
    RtlCreateUnicodeStringFromAsciiz(usLinkTarget, szLinkTarget) then
  try
// Call the Unicode version
    result := CreateHardLinkW(usLinkName.Buffer, usLinkTarget.Buffer, lpSecurityAttributes);
  finally
// free the allocated buffers
    RtlFreeHeap(hHeap, 0, usLinkName.Buffer);
    RtlFreeHeap(hHeap, 0, usLinkTarget.Buffer);
  end;
end;

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

var
  hKernel32 : THandle = 0;

initialization
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

// GetModuleHandle because this DLL is loaded into any Win32 subsystem process anyway
// implicitly. And Delphi cannot create applications for other subsystems without
// major changes in SysInit und System units.
  hNtDll := GetModuleHandle(szNtDll);
  if hNtDll <> 0 then
  begin
// Get all the function addresses
    @RtlCreateUnicodeStringFromAsciiz := GetProcAddress(hNtDll, szRtlCreateUnicodeStringFromAsciiz);
    @ZwClose                          := GetProcAddress(hNtDll, szZwClose);
    @ZwSetInformationFile             := GetProcAddress(hNtDll, szZwSetInformationFile);
    @RtlPrefixUnicodeString           := GetProcAddress(hNtDll, szRtlPrefixUnicodeString);
    @ZwOpenSymbolicLinkObject         := GetProcAddress(hNtDll, szZwOpenSymbolicLinkObject);
    @ZwQuerySymbolicLinkObject        := GetProcAddress(hNtDll, szZwQuerySymbolicLinkObject);
    @ZwOpenFile                       := GetProcAddress(hNtDll, szZwOpenFile);
    @RtlAllocateHeap                  := GetProcAddress(hNtDll, szRtlAllocateHeap);
    @RtlFreeHeap                      := GetProcAddress(hNtDll, szRtlFreeHeap);
    @RtlDosPathNameToNtPathName_U     := GetProcAddress(hNtDll, szRtlDosPathNameToNtPathName_U);
    @RtlInitUnicodeString             := GetProcAddress(hNtDll, szRtlInitUnicodeString);
    @RtlDetermineDosPathNameType_U    := GetProcAddress(hNtDll, szRtlDetermineDosPathNameType_U);
    @RtlNtStatusToDosError            := GetProcAddress(hNtDll, szRtlNtStatusToDosError);
    @ZwQueryDirectoryFile             := GetProcAddress(hNtDll, szZwQueryDirectoryFile);
    @ZwQueryInformationFile           := GetProcAddress(hNtDll, szZwQueryInformationFile);
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

    @CreateHardLinkA := @MyCreateHardLinkA;
    @CreateHardLinkW := @MyCreateHardLinkW;
  end; // if not (Assigned(@CreateHardLinkA) and Assigned(@CreateHardLinkW)) then ...

//--------------------------------------------------------------------------------------------------

// History:


{
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

