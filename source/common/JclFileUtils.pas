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
{ The Original Code is JclFileUtils.pas.                                                           }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright (C) Marcel van Brakel. All rights reserved.  }
{                                                                                                  }
{ Contributors:                                                                                    }
{   André Snepvangers (asnepvangers)                                                               }
{   Anthony Steele                                                                                 }
{   Azret Botash                                                                                   }
{   Charlie Calvert                                                                                }
{   David Hervieux                                                                                 }
{   Jeff                                                                                           }
{   John Molyneux                                                                                  }
{   Marcel Bestebroer                                                                              }
{   Marcel van Brakel                                                                              }
{   Massimo Maria Ghisalberti                                                                      }
{   Matthias Thoma (mthoma)                                                                        }
{   Olivier Sannier (obones)                                                                       }
{   Pelle F. S. Liljendal                                                                          }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Rudy Velthuis                                                                                  }
{   Scott Price                                                                                    }
{   Wim De Cleen                                                                                   }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains routines and classes for working with files, directories and path strings.    }
{ Additionally it contains wrapper classes for file mapping objects and version resources.         }
{ Generically speaking, everything that has to do with files and directories. Note that filesystem }
{ specific functionality has been extracted into external units, for example JclNTFS which         }
{ contains NTFS specific utility routines, and that the JclShell unit contains some file related   }
{ routines as well but they are specific to the Windows shell.                                     }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclFileUtils;

{$I jcl.inc}
{$I crossplatform.inc}

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
  {$ENDIF MSWINDOWS}
  Classes, SysUtils,
  JclBase;

{$IFDEF FPC}
type
  PBoolean = System.PBoolean;
{$ENDIF FPC}

//--------------------------------------------------------------------------------------------------
// replacements for defective Libc.pas declarations
//--------------------------------------------------------------------------------------------------

{$IFDEF KYLIX}

function stat64(FileName: PChar; var StatBuffer: TStatBuf64): Integer; cdecl;
{$EXTERNALSYM stat64}
function fstat64(FileDes: Integer; var StatBuffer: TStatBuf64): Integer; cdecl;
{$EXTERNALSYM fstat64}
function lstat64(FileName: PChar; var StatBuffer: TStatBuf64): Integer; cdecl;
{$EXTERNALSYM lstat64}

{$ENDIF KYLIX}

//--------------------------------------------------------------------------------------------------
// Path Manipulation
//
// Various support routines for working with path strings. For example, building a path from
// elements or extracting the elements from a path, interpretation of paths and transformations of
// paths.
//--------------------------------------------------------------------------------------------------

const
  {$IFDEF UNIX}
  PathSeparator    = '/';
  {$ENDIF UNIX}
  {$IFDEF MSWINDOWS}
  DriveLetters     = ['a'..'z', 'A'..'Z'];
  PathDevicePrefix = '\\.\';
  PathSeparator    = '\';
  PathUncPrefix    = '\\';
  {$ENDIF MSWINDOWS}

  faSymLink           = $00000040 {$IFDEF SUPPORTS_PLATFORM} platform {$ENDIF}; // defined since D7
  faNormalFile        = $00000080;
  faTemporary         = $00000100 {$IFDEF SUPPORTS_PLATFORM} platform {$ENDIF};
  faSparseFile        = $00000200 {$IFDEF SUPPORTS_PLATFORM} platform {$ENDIF};
  faReparsePoint      = $00000400 {$IFDEF SUPPORTS_PLATFORM} platform {$ENDIF};
  faCompressed        = $00000800 {$IFDEF SUPPORTS_PLATFORM} platform {$ENDIF};
  faOffline           = $00001000 {$IFDEF SUPPORTS_PLATFORM} platform {$ENDIF};
  faNotContentIndexed = $00002000 {$IFDEF SUPPORTS_PLATFORM} platform {$ENDIF};
  faEncrypted         = $00004000 {$IFDEF SUPPORTS_PLATFORM} platform {$ENDIF};

  faRejectedByDefault = faHidden + faSysFile + faVolumeID + faDirectory;
  faWindowsSpecific   = faVolumeID + faArchive + faTemporary + faSparseFile + faReparsePoint +
                        faCompressed + faOffline + faNotContentIndexed + faEncrypted;
  faUnixSpecific      = faSymLink;

type
  TCompactPath = ({cpBegin, }cpCenter, cpEnd);

function PathAddSeparator(const Path: string): string;
function PathAddExtension(const Path, Extension: string): string;
function PathAppend(const Path, Append: string): string;
function PathBuildRoot(const Drive: Byte): string;
function PathCanonicalize(const Path: string): string;
function PathCommonPrefix(const Path1, Path2: string): Integer;
{$IFDEF MSWINDOWS}
function PathCompactPath(const DC: HDC; const Path: string; const Width: Integer;
  CmpFmt: TCompactPath): string;
{$ENDIF MSWINDOWS}
procedure PathExtractElements(const Source: string; var Drive, Path, FileName, Ext: string);
function PathExtractFileDirFixed(const S: AnsiString): AnsiString;
function PathExtractFileNameNoExt(const Path: string): string;
function PathExtractPathDepth(const Path: string; Depth: Integer): string;
function PathGetDepth(const Path: string): Integer;
{$IFDEF MSWINDOWS}
function PathGetLongName(const Path: string): string;
function PathGetShortName(const Path: string): string;
{$ENDIF MSWINDOWS}
function PathGetRelativePath(Origin, Destination: string): string;
function PathGetTempPath: string;
function PathIsAbsolute(const Path: string): Boolean;
function PathIsChild(const Path, Base: AnsiString): Boolean;
function PathIsDiskDevice(const Path: string): Boolean;
function PathIsUNC(const Path: string): Boolean;
function PathRemoveSeparator(const Path: string): string;
function PathRemoveExtension(const Path: string): string;

//--------------------------------------------------------------------------------------------------
// Files and Directories
//
// Routines for working with files and directories. Includes routines to extract various file
// attributes or update them, volume locking and routines for creating temporary files.
//--------------------------------------------------------------------------------------------------

type
  TDelTreeProgress = function (const FileName: string; Attr: DWORD): Boolean;
  TFileListOption  = (flFullNames, flRecursive, flMaskedSubfolders);
  TFileListOptions = set of TFileListOption;
  TJclAttributeMatch = (amAny, amExact, amSubSetOf, amSuperSetOf, amCustom);
  TFileMatchFunc = function(const Attr: Integer; const FileInfo: TSearchRec): Boolean;
  TFileHandler = procedure (const FileName: string) of object;
  TFileHandlerEx = procedure (const Directory: string; const FileInfo: TSearchRec) of object;

function BuildFileList(const Path: string; const Attr: Integer; const List: TStrings): Boolean;
function AdvBuildFileList(const Path: string; const Attr: Integer; const Files: TStrings;
  const AttributeMatch: TJclAttributeMatch = amSuperSetOf; const Options: TFileListOptions = [];
  const SubfoldersMask: string = ''; const FileMatchFunc: TFileMatchFunc = nil): Boolean;
function VerifyFileAttributeMask(var RejectedAttributes, RequiredAttributes: Integer): Boolean;
function IsFileAttributeMatch(FileAttributes, RejectedAttributes,
  RequiredAttributes: Integer): Boolean;
function FileAttributesStr(const FileInfo: TSearchRec): string;
function IsFileNameMatch(FileName: string; const Mask: string;
  const CaseSensitive: Boolean = {$IFDEF MSWINDOWS} False {$ELSE} True {$ENDIF}): Boolean;
procedure EnumFiles(const Path: string; HandleFile: TFileHandlerEx;
  RejectedAttributes: Integer = faRejectedByDefault; RequiredAttributes: Integer = 0;
  Abort: PBoolean = nil);
procedure EnumDirectories(const Root: string; const HandleDirectory: TFileHandler;
  const IncludeHiddenDirectories: Boolean = False; const SubDirectoriesMask: string = '';
  Abort: PBoolean = nil {$IFDEF UNIX}; ResolveSymLinks: Boolean = True {$ENDIF});
{$IFDEF MSWINDOWS}
function CloseVolume(var Volume: THandle): Boolean;
procedure CreateEmptyFile(const FileName: string);
{$IFNDEF FPC}
function DeleteDirectory(const DirectoryName: string; MoveToRecycleBin: Boolean): Boolean;
{$ENDIF ~FPC}
function DelTree(const Path: string): Boolean;
function DelTreeEx(const Path: string; AbortOnFailure: Boolean; Progress: TDelTreeProgress): Boolean;
{$ENDIF MSWINDOWS}
function DirectoryExists(const Name: string {$IFDEF UNIX}; ResolveSymLinks: Boolean = True {$ENDIF}): Boolean;
{$IFDEF MSWINDOWS}
function DiskInDrive(Drive: Char): Boolean;
{$ENDIF MSWINDOWS}
function FileCreateTemp(var Prefix: string): THandle;
function FileBackup(const FileName: string; Move: Boolean = False): Boolean;
function FileCopy(const ExistingFileName, NewFileName: string; ReplaceExisting: Boolean = False): Boolean;
function FileDelete(const FileName: string; MoveToRecycleBin: Boolean = False): Boolean;
function FileExists(const FileName: string): Boolean;
function FileMove(const ExistingFileName, NewFileName: string; ReplaceExisting: Boolean = False): Boolean;
function FileRestore(const FileName: string): Boolean;
function GetBackupFileName(const FileName: string): string;
function FileGetDisplayName(const FileName: string): string;
function FileGetGroupName(const FileName: string): string;
function FileGetOwnerName(const FileName: string): string;
function FileGetSize(const FileName: string): Integer;
function FileGetTempName(const Prefix: string): string;
{$IFDEF MSWINDOWS}
function FileGetTypeName(const FileName: string): string;
{$ENDIF MSWINDOWS}
function FindUnusedFileName(const FileName, FileExt, Suffix: AnsiString): AnsiString;
function ForceDirectories(Name: string): Boolean;
function GetDirectorySize(const Path: string): Int64;
{$IFDEF MSWINDOWS}
function GetDriveTypeStr(const Drive: Char): string;
function GetFileAgeCoherence(const FileName: string): Boolean;
{$ENDIF MSWINDOWS}
procedure GetFileAttributeList(const Items: TStrings; const Attr: Integer);
{$IFDEF MSWINDOWS}
procedure GetFileAttributeListEx(const Items: TStrings; const Attr: Integer);
{$ENDIF MSWINDOWS}
function GetFileInformation(const FileName: string; out FileInfo: TSearchRec): Boolean; overload;
function GetFileInformation(const FileName: string): TSearchRec; overload;
{$IFDEF UNIX}
function GetFileStatus(const FileName: string; out StatBuf: TStatBuf64;
  const ResolveSymLinks: Boolean): Integer;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
function GetFileLastWrite(const FileName: string): TFileTime; overload;
function GetFileLastWrite(const FileName: string; out LocalTime: TDateTime): Boolean; overload;
function GetFileLastAccess(const FileName: string): TFileTime; overload;
function GetFileLastAccess(const FileName: string; out LocalTime: TDateTime): Boolean; overload;
function GetFileCreation(const FileName: string): TFileTime; overload;
function GetFileCreation(const FileName: string; out LocalTime: TDateTime): Boolean; overload;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
function GetFileLastWrite(const FileName: string; out TimeStamp: Integer; ResolveSymLinks: Boolean = True): Boolean; overload;
function GetFileLastWrite(const FileName: string; out LocalTime: TDateTime; ResolveSymLinks: Boolean = True): Boolean; overload;
function GetFileLastWrite(const FileName: string; ResolveSymLinks: Boolean = True): Integer; overload;
function GetFileLastAccess(const FileName: string; out TimeStamp: Integer; ResolveSymLinks: Boolean = True): Boolean; overload;
function GetFileLastAccess(const FileName: string; out LocalTime: TDateTime; ResolveSymLinks: Boolean = True): Boolean; overload;
function GetFileLastAccess(const FileName: string; ResolveSymLinks: Boolean = True): Integer; overload;
function GetFileLastAttrChange(const FileName: string; out TimeStamp: Integer; ResolveSymLinks: Boolean = True): Boolean; overload;
function GetFileLastAttrChange(const FileName: string; out LocalTime: TDateTime; ResolveSymLinks: Boolean = True): Boolean; overload;
function GetFileLastAttrChange(const FileName: string; ResolveSymLinks: Boolean = True): Integer; overload;
{$ENDIF UNIX}
function GetModulePath(const Module: HMODULE): string;
function GetSizeOfFile(const FileName: string): Int64; overload;
function GetSizeOfFile(const FileInfo: TSearchRec): Int64; overload;
{$IFDEF MSWINDOWS}
function GetSizeOfFile(Handle: THandle): Int64; overload;
function GetStandardFileInfo(const FileName: string): TWin32FileAttributeData;
{$ENDIF MSWINDOWS}
function IsDirectory(const FileName: string {$IFDEF UNIX}; ResolveSymLinks: Boolean = True {$ENDIF}): Boolean;
function IsRootDirectory(const CanonicFileName: string): Boolean;
{$IFDEF MSWINDOWS}
function LockVolume(const Volume: string; var Handle: THandle): Boolean;
function OpenVolume(const Drive: Char): THandle;
function SetDirLastWrite(const DirName: string; const DateTime: TDateTime): Boolean;
function SetDirLastAccess(const DirName: string; const DateTime: TDateTime): Boolean;
function SetDirCreation(const DirName: string; const DateTime: TDateTime): Boolean;
{$ENDIF MSWINDOWS}
function SetFileLastWrite(const FileName: string; const DateTime: TDateTime): Boolean;
function SetFileLastAccess(const FileName: string; const DateTime: TDateTime): Boolean;
{$IFDEF MSWINDOWS}
function SetFileCreation(const FileName: string; const DateTime: TDateTime): Boolean;
procedure ShredFile(const FileName: string; Times: Integer = 1);
function UnlockVolume(var Handle: THandle): Boolean;
{$ENDIF MSWINDOWS}

{$IFDEF UNIX}
function CreateSymbolicLink(const Name, Target: string): Boolean;
{ This function gets the value of the symbolic link filename. }
function SymbolicLinkTarget(const Name: string): string;
{$ENDIF UNIX}

//--------------------------------------------------------------------------------------------------
// TJclFileAttributeMask
//
// File search helper class, allows to specify required/rejected attributes
//--------------------------------------------------------------------------------------------------

type
  TAttributeInterest = (aiIgnored, aiRejected, aiRequired);

  TJclCustomFileAttrMask = class(TPersistent)
  private
    FRequiredAttr: Integer;
    FRejectedAttr: Integer;
    function GetAttr(Index: Integer): TAttributeInterest;
    procedure SetAttr(Index: Integer; const Value: TAttributeInterest);
    procedure ReadRequiredAttributes(Reader: TReader);
    procedure ReadRejectedAttributes(Reader: TReader);
    procedure WriteRequiredAttributes(Writer: TWriter);
    procedure WriteRejectedAttributes(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    property ReadOnly: TAttributeInterest index faReadOnly
      read GetAttr write SetAttr stored False;
    property Hidden: TAttributeInterest index faHidden
      read GetAttr write SetAttr stored False;
    property System: TAttributeInterest index faSysFile
      read GetAttr write SetAttr stored False;
    property VolumeID: TAttributeInterest index faVolumeID
      read GetAttr write SetAttr stored False;
    property Directory: TAttributeInterest index faDirectory
      read GetAttr write SetAttr stored False;
    property SymLink: TAttributeInterest index faSymLink
      read GetAttr write SetAttr stored False;
    property Normal: TAttributeInterest index faNormalFile
      read GetAttr write SetAttr stored False;
    property Archive: TAttributeInterest index faArchive
      read GetAttr write SetAttr stored False;
    property Temporary: TAttributeInterest index faTemporary
      read GetAttr write SetAttr stored False;
    property SparseFile: TAttributeInterest index faSparseFile
      read GetAttr write SetAttr stored False;
    property ReparsePoint: TAttributeInterest index faReparsePoint
      read GetAttr write SetAttr stored False;
    property Compressed: TAttributeInterest index faCompressed
      read GetAttr write SetAttr stored False;
    property OffLine: TAttributeInterest index faOffline
      read GetAttr write SetAttr stored False;
    property NotContentIndexed: TAttributeInterest index faNotContentIndexed
      read GetAttr write SetAttr stored False;
    property Encrypted: TAttributeInterest index faEncrypted
      read GetAttr write SetAttr stored False;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    function Match(FileAttributes: Integer): Boolean; overload;
    function Match(const FileInfo: TSearchRec): Boolean; overload;
    property Required: Integer read FRequiredAttr write FRequiredAttr;
    property Rejected: Integer read FRejectedAttr write FRejectedAttr;
    property Attribute[Index: Integer]: TAttributeInterest read GetAttr write SetAttr; default;
  end;

  TJclFileAttributeMask = class(TJclCustomFileAttrMask)
  published
    property ReadOnly;
    property Hidden;
    property System;
    property Directory;
    property Normal;
    {$IFDEF UNIX}
    property SymLink;
    {$ENDIF UNIX}
    {$IFDEF MSWINDOWS}
    property VolumeID;
    property Archive;
    property Temporary;
    property SparseFile;
    property ReparsePoint;
    property Compressed;
    property OffLine;
    property NotContentIndexed;
    property Encrypted;
    {$ENDIF MSWINDOWS}
  end;

//--------------------------------------------------------------------------------------------------
// IJclFileEnumerator / TJclFileEnumerator
//
// Interface / class for thread-based file search
//--------------------------------------------------------------------------------------------------

type
  TFileSearchOption = (fsIncludeSubDirectories, fsIncludeHiddenSubDirectories, fsLastChangeAfter,
    fsLastChangeBefore, fsMaxSize, fsMinSize);
  TFileSearchOptions = set of TFileSearchOption;
  TFileSearchTaskID = Integer;
  TFileSearchTerminationEvent = procedure (const ID: TFileSearchTaskID; const Aborted: Boolean) of object;
  TFileEnumeratorSyncMode = (smPerFile, smPerDirectory);

  IJclFileEnumerator = interface
    ['{F7E747ED-1C41-441F-B25B-BB314E00C4E9}']
    // property access methods
    function GetAttributeMask: TJclFileAttributeMask;
    function GetCaseSensitiveSearch: Boolean;
    function GetRootDirectory: string;
    function GetFileMask: string;
    function GetFileMasks: TStrings;
    function GetFileSizeMax: Int64;
    function GetFileSizeMin: Int64;
    function GetIncludeSubDirectories: Boolean;
    function GetIncludeHiddenSubDirectories: Boolean;
    function GetLastChangeAfter: TDateTime;
    function GetLastChangeBefore: TDateTime;
    function GetLastChangeAfterStr: string;
    function GetLastChangeBeforeStr: string;
    function GetRunningTasks: Integer;
    function GetSubDirectoryMask: string;
    function GetSynchronizationMode: TFileEnumeratorSyncMode;
    function GetOnEnterDirectory: TFileHandler;
    function GetOnTerminateTask: TFileSearchTerminationEvent;
    function GetOption(const Option: TFileSearchOption): Boolean;
    function GetOptions: TFileSearchoptions;
    procedure SetAttributeMask(const Value: TJclFileAttributeMask);
    procedure SetCaseSensitiveSearch(const Value: Boolean);
    procedure SetRootDirectory(const Value: string);
    procedure SetFileMask(const Value: string);
    procedure SetFileMasks(const Value: TStrings);
    procedure SetFileSizeMax(const Value: Int64);
    procedure SetFileSizeMin(const Value: Int64);
    procedure SetIncludeSubDirectories(const Value: Boolean);
    procedure SetIncludeHiddenSubDirectories(const Value: Boolean);
    procedure SetLastChangeAfter(const Value: TDateTime);
    procedure SetLastChangeBefore(const Value: TDateTime);
    procedure SetLastChangeAfterStr(const Value: string);
    procedure SetLastChangeBeforeStr(const Value: string);
    procedure SetOption(const Option: TFileSearchOption; const Value: Boolean);
    procedure SetOptions(const Value: TFileSearchOptions);
    procedure SetSubDirectoryMask(const Value: string);
    procedure SetSynchronizationMode(const Value: TFileEnumeratorSyncMode);
    procedure SetOnEnterDirectory(const Value: TFileHandler);
    procedure SetOnTerminateTask(const Value: TFileSearchTerminationEvent);
    // other methods
    function FillList(List: TStrings): TFileSearchTaskID;
    function ForEach(Handler: TFileHandler): TFileSearchTaskID; overload;
    function ForEach(Handler: TFileHandlerEx): TFileSearchTaskID; overload;
    procedure StopTask(ID: TFileSearchTaskID);
    procedure StopAllTasks(Silently: Boolean = False); // Silently: Don't call OnTerminateTask
    // properties
    property CaseSensitiveSearch: Boolean read GetCaseSensitiveSearch write SetCaseSensitiveSearch;
    property RootDirectory: string read GetRootDirectory write SetRootDirectory;
    property FileMask: string read GetFileMask write SetFileMask;
    property SubDirectoryMask: string read GetSubDirectoryMask write SetSubDirectoryMask;
    property AttributeMask: TJclFileAttributeMask read GetAttributeMask write SetAttributeMask;
    property FileSizeMin: Int64 read GetFileSizeMin write SetFileSizeMin;
    property FileSizeMax: Int64 read GetFileSizeMax write SetFileSizeMax; // default InvalidFileSize;
    property LastChangeAfter: TDateTime read GetLastChangeAfter write SetLastChangeAfter;
    property LastChangeBefore: TDateTime read GetLastChangeBefore write SetLastChangeBefore;
    property LastChangeAfterAsString: string read GetLastChangeAfterStr write SetLastChangeAfterStr;
    property LastChangeBeforeAsString: string read GetLastChangeBeforeStr write SetLastChangeBeforeStr;
    property IncludeSubDirectories: Boolean read GetIncludeSubDirectories
      write SetIncludeSubDirectories;
    property IncludeHiddenSubDirectories: Boolean read GetIncludeHiddenSubDirectories
      write SetIncludeHiddenSubDirectories;
    property RunningTasks: Integer read GetRunningTasks;
    property SynchronizationMode: TFileEnumeratorSyncMode read GetSynchronizationMode
      write SetSynchronizationMode;
    property OnEnterDirectory: TFileHandler read GetOnEnterDirectory write SetOnEnterDirectory;
    property OnTerminateTask: TFileSearchTerminationEvent read GetOnTerminateTask
      write SetOnTerminateTask;
  end;

  TJclFileEnumerator = class(TPersistent, IJclFileEnumerator)
  private
    FOwnerInterface: IInterface;
    FTasks: TList;
    FFileMasks: TStringList;
    FRootDirectory: string;
    FSubDirectoryMask: string;
    FOnEnterDirectory: TFileHandler;
    FOnTerminateTask: TFileSearchTerminationEvent;
    FNextTaskID: TFileSearchTaskID;
    FAttributeMask: TJclFileAttributeMask;
    FSynchronizationMode: TFileEnumeratorSyncMode;
    FFileSizeMin: Int64;
    FFileSizeMax: Int64;
    FLastChangeBefore: TDateTime;
    FLastChangeAfter: TDateTime;
    FOptions: TFileSearchOptions;
    FCaseSensitiveSearch: Boolean;
    function IsLastChangeAfterStored: Boolean;
    function IsLastChangeBeforeStored: Boolean;
    function GetNextTaskID: TFileSearchTaskID;
    function GetCaseSensitiveSearch: Boolean;
    procedure SetCaseSensitiveSearch(const Value: Boolean);
  protected
    FRefCount: Integer;
    function CreateTask: TThread;
    procedure TaskTerminated(Sender: TObject);
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // IJclFileEnumerator property access methods
    function GetAttributeMask: TJclFileAttributeMask;
    function GetRootDirectory: string;
    function GetFileMask: string;
    function GetFileMasks: TStrings;
    function GetFileSizeMax: Int64;
    function GetFileSizeMin: Int64;
    function GetIncludeSubDirectories: Boolean;
    function GetIncludeHiddenSubDirectories: Boolean;
    function GetLastChangeAfter: TDateTime;
    function GetLastChangeBefore: TDateTime;
    function GetLastChangeAfterStr: string;
    function GetLastChangeBeforeStr: string;
    function GetOption(const Option: TFileSearchOption): Boolean;
    function GetOptions: TFileSearchoptions;
    function GetRunningTasks: Integer;
    function GetSubDirectoryMask: string;
    function GetSynchronizationMode: TFileEnumeratorSyncMode;
    function GetOnEnterDirectory: TFileHandler;
    function GetOnTerminateTask: TFileSearchTerminationEvent;
    procedure SetAttributeMask(const Value: TJclFileAttributeMask);
    procedure SetRootDirectory(const Value: string);
    procedure SetFileMask(const Value: string);
    procedure SetFileMasks(const Value: TStrings);
    procedure SetFileSizeMax(const Value: Int64);
    procedure SetFileSizeMin(const Value: Int64);
    procedure SetIncludeSubDirectories(const Value: Boolean);
    procedure SetIncludeHiddenSubDirectories(const Value: Boolean);
    procedure SetLastChangeAfter(const Value: TDateTime);
    procedure SetLastChangeBefore(const Value: TDateTime);
    procedure SetLastChangeAfterStr(const Value: string);
    procedure SetLastChangeBeforeStr(const Value: string);
    procedure SetOption(const Option: TFileSearchOption; const Value: Boolean);
    procedure SetOptions(const Value: TFileSearchOptions);
    procedure SetSubDirectoryMask(const Value: string);
    procedure SetSynchronizationMode(const Value: TFileEnumeratorSyncMode);
    procedure SetOnEnterDirectory(const Value: TFileHandler);
    procedure SetOnTerminateTask(const Value: TFileSearchTerminationEvent);
    property NextTaskID: TFileSearchTaskID read GetNextTaskID;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Assign(Source: TPersistent); override;
    function FillList(List: TStrings): TFileSearchTaskID;
    function ForEach(Handler: TFileHandler): TFileSearchTaskID; overload;
    function ForEach(Handler: TFileHandlerEx): TFileSearchTaskID; overload;
    procedure StopTask(ID: TFileSearchTaskID);
    procedure StopAllTasks(Silently: Boolean = False); // Silently: Don't call OnTerminateTask
    property FileMask: string read GetFileMask write SetFileMask;
    property IncludeSubDirectories: Boolean
      read GetIncludeSubDirectories write SetIncludeSubDirectories;
    property IncludeHiddenSubDirectories: Boolean
      read GetIncludeHiddenSubDirectories write SetIncludeHiddenSubDirectories;
    property SearchOption[const Option: TFileSearchOption]: Boolean read GetOption write SetOption;
    property LastChangeAfterAsString: string read GetLastChangeAfterStr write SetLastChangeAfterStr;
    property LastChangeBeforeAsString: string read GetLastChangeBeforeStr write SetLastChangeBeforeStr;
  published
    property CaseSensitiveSearch: Boolean read GetCaseSensitiveSearch write SetCaseSensitiveSearch
      default {$IFDEF MSWINDOWS} False {$ELSE} True {$ENDIF};
    property FileMasks: TStrings read GetFileMasks write SetFileMasks;
    property RootDirectory: string read FRootDirectory write FRootDirectory;
    property SubDirectoryMask: string read FSubDirectoryMask write FSubDirectoryMask;
    property AttributeMask: TJclFileAttributeMask read FAttributeMask write SetAttributeMask;
    property FileSizeMin: Int64 read FFileSizeMin write FFileSizeMin;
    property FileSizeMax: Int64 read FFileSizeMax write FFileSizeMax;
    property LastChangeAfter: TDateTime read FLastChangeAfter write FLastChangeAfter
      stored IsLastChangeAfterStored;
    property LastChangeBefore: TDateTime read FLastChangeBefore write FLastChangeBefore
      stored IsLastChangeBeforeStored;
    property Options: TFileSearchOptions read FOptions write FOptions
      default [fsIncludeSubDirectories];
    property RunningTasks: Integer read GetRunningTasks;
    property SynchronizationMode: TFileEnumeratorSyncMode read FSynchronizationMode write FSynchronizationMode
      default smPerDirectory;
    property OnEnterDirectory: TFileHandler read FOnEnterDirectory write FOnEnterDirectory;
    property OnTerminateTask: TFileSearchTerminationEvent read FOnTerminateTask write FOnTerminateTask;
  end;

function FileSearch: IJclFileEnumerator;

//--------------------------------------------------------------------------------------------------
// TFileVersionInfo
//
// Class that enables reading the version information stored in a PE file.
//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

type
  TFileFlag = (ffDebug, ffInfoInferred, ffPatched, ffPreRelease, ffPrivateBuild, ffSpecialBuild);
  TFileFlags = set of TFileFlag;

  PLangIdRec = ^TLangIdRec;
  TLangIdRec = packed record
    case Integer of
    0: (
      LangId: Word;
      CodePage: Word);
    1: (
      Pair: DWORD);
  end;

  EJclFileVersionInfoError = class(EJclError);

  TJclFileVersionInfo = class(TObject)
  private
    FBuffer: string;
    FFixedInfo: PVSFixedFileInfo;
    FFileFlags: TFileFlags;
    FItemList: TStringList;
    FItems: TStringList;
    FLanguages: array of TLangIdRec;
    FLanguageIndex: Integer;
    FTranslations: array of TLangIdRec;
    function GetFixedInfo: TVSFixedFileInfo;
    function GetItems: TStrings;
    function GetLanguageCount: Integer;
    function GetLanguageIds(Index: Integer): string;
    function GetLanguageNames(Index: Integer): string;
    function GetLanguages(Index: Integer): TLangIdRec;
    function GetTranslationCount: Integer;
    function GetTranslations(Index: Integer): TLangIdRec;
    procedure SetLanguageIndex(const Value: Integer);
  protected
    procedure CreateItemsForLanguage;
    procedure CheckLanguageIndex(Value: Integer);
    procedure ExtractData;
    procedure ExtractFlags;
    function GetBinFileVersion: string;
    function GetBinProductVersion: string;
    function GetFileOS: DWORD;
    function GetFileSubType: DWORD;
    function GetFileType: DWORD;
    function GetVersionKeyValue(Index: Integer): string;
  public
    constructor Attach(VersionInfoData: Pointer; Size: Integer);
    constructor Create(const FileName: string);
    destructor Destroy; override;
    class function VersionLanguageId(const LangIdRec: TLangIdRec): string;
    class function VersionLanguageName(const LangId: Word): string;
    function TranslationMatchesLanguages(Exact: Boolean = True): Boolean;
    property BinFileVersion: string read GetBinFileVersion;
    property BinProductVersion: string read GetBinProductVersion;
    property Comments: string index 1 read GetVersionKeyValue;
    property CompanyName: string index 2 read GetVersionKeyValue;
    property FileDescription: string index 3 read GetVersionKeyValue;
    property FixedInfo: TVSFixedFileInfo read GetFixedInfo;
    property FileFlags: TFileFlags read FFileFlags;
    property FileOS: DWORD read GetFileOS;
    property FileSubType: DWORD read GetFileSubType;
    property FileType: DWORD read GetFileType;
    property FileVersion: string index 4 read GetVersionKeyValue;
    property Items: TStrings read GetItems;
    property InternalName: string index 5 read GetVersionKeyValue;
    property LanguageCount: Integer read GetLanguageCount;
    property LanguageIds[Index: Integer]: string read GetLanguageIds;
    property LanguageIndex: Integer read FLanguageIndex write SetLanguageIndex;
    property Languages[Index: Integer]: TLangIdRec read GetLanguages;
    property LanguageNames[Index: Integer]: string read GetLanguageNames;
    property LegalCopyright: string index 6 read GetVersionKeyValue;
    property LegalTradeMarks: string index 7 read GetVersionKeyValue;
    property OriginalFilename: string index 8 read GetVersionKeyValue;
    property PrivateBuild: string index 12 read GetVersionKeyValue;
    property ProductName: string index 9 read GetVersionKeyValue;
    property ProductVersion: string index 10 read GetVersionKeyValue;
    property SpecialBuild: string index 11 read GetVersionKeyValue;
    property TranslationCount: Integer read GetTranslationCount;
    property Translations[Index: Integer]: TLangIdRec read GetTranslations;
  end;

function OSIdentToString(const OSIdent: DWORD): string;
function OSFileTypeToString(const OSFileType: DWORD; const OSFileSubType: DWORD = 0): string;

function VersionResourceAvailable(const FileName: string): Boolean;

{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------
// Version Info formatting
//--------------------------------------------------------------------------------------------------

type
  TFileVersionFormat = (vfMajorMinor, vfFull);

function FormatVersionString(const HiV, LoV: Word): string; overload;
function FormatVersionString(const Major, Minor, Build, Revision: Word): string; overload;

{$IFDEF MSWINDOWS}

function FormatVersionString(const FixedInfo: TVSFixedFileInfo; VersionFormat: TFileVersionFormat = vfFull): string; overload;

//--------------------------------------------------------------------------------------------------
// Version Info extracting
//--------------------------------------------------------------------------------------------------

procedure VersionExtractFileInfo(const FixedInfo: TVSFixedFileInfo; var Major, Minor, Build, Revision: Word);
procedure VersionExtractProductInfo(const FixedInfo: TVSFixedFileInfo; var Major, Minor, Build, Revision: Word);

//--------------------------------------------------------------------------------------------------
// Fixed Version Info routines
//--------------------------------------------------------------------------------------------------

function VersionFixedFileInfo(const FileName: string; var FixedInfo: TVSFixedFileInfo): Boolean;
function VersionFixedFileInfoString(const FileName: string; VersionFormat: TFileVersionFormat = vfFull;
  const NotAvailableText: string = ''): string;

{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------
// Streams
//
// TStream descendent classes for dealing with temporary files and for using file mapping objects.
//--------------------------------------------------------------------------------------------------

type
  TJclTempFileStream = class(THandleStream)
  private
    FFileName: string;
  public
    constructor Create(const Prefix: string);
    destructor Destroy; override;
    property FileName: string read FFileName;
  end;

{$IFDEF MSWINDOWS}

  TJclCustomFileMapping = class;

  TJclFileMappingView = class(TCustomMemoryStream)
  private
    FFileMapping: TJclCustomFileMapping;
    FOffsetHigh: Cardinal;
    FOffsetLow: Cardinal;
    function GetIndex: Integer;
    function GetOffset: Int64;
  public
    constructor Create(const FileMap: TJclCustomFileMapping;
      Access, Size: Cardinal; ViewOffset: Int64);
    constructor CreateAt(FileMap: TJclCustomFileMapping; Access,
      Size: Cardinal; ViewOffset: Int64; Address: Pointer);
    destructor Destroy; override;
    function Flush(const Count: Cardinal): Boolean;
    procedure LoadFromStream(const Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    function Write(const Buffer; Count: Longint): Longint; override;
    property Index: Integer read GetIndex;
    property FileMapping: TJclCustomFileMapping read FFileMapping;
    property Offset: Int64 read GetOffset;
  end;

  TJclFileMappingRoundOffset = (rvDown, rvUp);

  TJclCustomFileMapping = class(TObject)
  private
    FExisted: Boolean;
    FHandle: THandle;
    FName: string;
    FRoundViewOffset: TJclFileMappingRoundOffset;
    FViews: TList;
    function GetCount: Integer;
    function GetView(Index: Integer): TJclFileMappingView;
  protected
    procedure ClearViews;
    procedure InternalCreate(const FileHandle: THandle; const Name: string;
      const Protect: Cardinal; MaximumSize: Int64; SecAttr: PSecurityAttributes);
    procedure InternalOpen(const Name: string; const InheritHandle: Boolean;
      const DesiredAccess: Cardinal);
    constructor Create;
  public
    constructor Open(const Name: string; const InheritHandle: Boolean; const DesiredAccess: Cardinal);
    destructor Destroy; override;
    function Add(const Access, Count: Cardinal; const Offset: Int64): Integer;
    function AddAt(const Access, Count: Cardinal; const Offset: Int64; const Address: Pointer): Integer;
    procedure Delete(const Index: Integer);
    function IndexOf(const View: TJclFileMappingView): Integer;
    property Count: Integer read GetCount;
    property Existed: Boolean read FExisted;
    property Handle: THandle read FHandle;
    property Name: string read FName;
    property RoundViewOffset: TJclFileMappingRoundOffset read FRoundViewOffset write FRoundViewOffset;
    property Views[index: Integer]: TJclFileMappingView read GetView;
  end;

  TJclFileMapping = class(TJclCustomFileMapping)
  private
    FFileHandle: THandle;
  public
    constructor Create(const FileName: string; FileMode: Cardinal;
      const Name: string; Protect: Cardinal; const MaximumSize: Int64;
      SecAttr: PSecurityAttributes); overload;
    constructor Create(const FileHandle: THandle; const Name: string;
      Protect: Cardinal; const MaximumSize: Int64;
      SecAttr: PSecurityAttributes); overload;
    destructor Destroy; override;
    property FileHandle: THandle read FFileHandle;
  end;

  TJclSwapFileMapping = class(TJclCustomFileMapping)
  public
    constructor Create(const Name: string; Protect: Cardinal;
      const MaximumSize: Int64; SecAttr: PSecurityAttributes);
  end;

//--------------------------------------------------------------------------------------------------
// TJclFileMappingStream
//--------------------------------------------------------------------------------------------------

  TJclFileMappingStream = class(TCustomMemoryStream)
  private
    FFileHandle: THandle;
    FMapping: THandle;
  protected
    procedure Close;
  public
    constructor Create(const FileName: string; FileMode: Word = fmOpenRead or fmShareDenyWrite);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

//--------------------------------------------------------------------------------------------------
// TJclMappedTextReader
//--------------------------------------------------------------------------------------------------

  TJclMappedTextReaderIndex = (tiNoIndex, tiFull);

  {$IFNDEF FPC}
  PPCharArray = ^TPCharArray;
  TPCharArray = array [0..0] of PChar;
  {$ENDIF ~FPC}

  TJclMappedTextReader = class(TPersistent)
  private
    FContent: PChar;
    FEnd: PChar;
    FIndex: PPCharArray;
    FIndexOption: TJclMappedTextReaderIndex;
    FFreeStream: Boolean;
    FLastLineNumber: Integer;
    FLastPosition: PChar;
    FLineCount: Integer;
    FMemoryStream: TCustomMemoryStream;
    FPosition: PChar;
    FSize: Integer;
    function GetAsString: string;
    function GetEof: Boolean;
    function GetChars(Index: Integer): Char;
    function GetLineCount: Integer;
    function GetLines(LineNumber: Integer): string;
    function GetPosition: Integer;
    function GetPositionFromLine(LineNumber: Integer): Integer;
    procedure SetPosition(const Value: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure CreateIndex;
    procedure Init;
    function PtrFromLine(LineNumber: Integer): PChar;
    function StringFromPosition(var StartPos: PChar): string;
  public
    constructor Create(MemoryStream: TCustomMemoryStream; FreeStream: Boolean = True;
      const AIndexOption: TJclMappedTextReaderIndex = tiNoIndex); overload;
    constructor Create(const FileName: string;
      const AIndexOption: TJclMappedTextReaderIndex = tiNoIndex); overload;
    destructor Destroy; override;
    procedure GoBegin;
    function Read: Char;
    function ReadLn: string;
    property AsString: string read GetAsString;
    property Chars[Index: Integer]: Char read GetChars;
    property Content: PChar read FContent;
    property Eof: Boolean read GetEof;
    property IndexOption: TJclMappedTextReaderIndex read FIndexOption;
    property Lines[LineNumber: Integer]: string read GetLines;
    property LineCount: Integer read GetLineCount;
    property PositionFromLine[LineNumber: Integer]: Integer read GetPositionFromLine;
    property Position: Integer read GetPosition write SetPosition;
    property Size: Integer read FSize;
  end;

{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------
// TJclFileMaskComparator
//--------------------------------------------------------------------------------------------------

{ TODO : UNTESTED/UNDOCUMENTED }

type
  TJclFileMaskComparator = class(TObject)
  private
    FFileMask: string;
    FExts: array of string;
    FNames: array of string;
    FWildChars: array of Byte;
    FSeparator: Char;
    procedure CreateMultiMasks;
    function GetCount: Integer;
    function GetExts(Index: Integer): string;
    function GetMasks(Index: Integer): string;
    function GetNames(Index: Integer): string;
    procedure SetFileMask(const Value: string);
    procedure SetSeparator(const Value: Char);
  public
    constructor Create;
    function Compare(const NameExt: string): Boolean;
    property Count: Integer read GetCount;
    property Exts[Index: Integer]: string read GetExts;
    property FileMask: string read FFileMask write SetFileMask;
    property Masks[Index: Integer]: string read GetMasks;
    property Names[Index: Integer]: string read GetNames;
    property Separator: Char read FSeparator write SetSeparator;
  end;

//--------------------------------------------------------------------------------------------------
// Exceptions
//--------------------------------------------------------------------------------------------------

  EJclPathError = class(EJclError);
  EJclFileUtilsError = class(EJclError);
  {$IFDEF UNIX}
  EJclTempFileStreamError = class(EJclFileUtilsError);
  {$ENDIF UNIX}
  {$IFDEF MSWINDOWS}
  EJclTempFileStreamError = class(EJclWin32Error);
  EJclFileMappingError = class(EJclWin32Error);
  EJclFileMappingViewError = class(EJclWin32Error);
  {$ENDIF MSWINDOWS}

{$IFNDEF DROP_OBSOLETE_CODE}
//--------------------------------------------------------------------------------------------------
// Deprecated, do not use
//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
function PathGetLongName2(const Path: string): string;
{$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}
{$IFNDEF FPC}
function Win32DeleteFile(const FileName: string; MoveToRecycleBin: Boolean): Boolean;
{$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}
{$ENDIF ~FPC}
function Win32MoveFileReplaceExisting(const SrcFileName, DstFileName: string): Boolean;
{$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}
function Win32BackupFile(const FileName: string; Move: Boolean): Boolean;
{$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}
function Win32RestoreFile(const FileName: string): Boolean;
{$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}
{$ENDIF MSWINDOWS}

{$ENDIF ~DROP_OBSOLETE_CODE}

implementation

uses
  {$IFDEF MSWINDOWS}
  ShellApi,
  {$IFDEF FPC}
  WinSysUt,
  {$ELSE ~FPC}
  ActiveX, ShlObj, JclShell,
  {$ENDIF ~FPC}
  JclSysUtils, JclWin32, JclDateTime, JclSecurity, JclSysInfo,
  {$ENDIF MSWINDOWS}
  JclResources, JclStrings;

{ Some general notes:

  This unit redeclares some functions from FileCtrl.pas to avoid a dependency on that unit in the
  JCL. The problem is that FileCtrl.pas uses some units (eg Forms.pas) which have ridiculous
  initialization requirements. They add 4KB (!) to the executable and roughly 1 second of startup.
  That initialization is only necessary for GUI applications and is unacceptable for high
  performance services or console apps.

  The routines which query files or directories for their attributes deliberately use FindFirst
  even though there may be easier ways to get at the required information. This is because FindFirst
  is about the only routine which doesn't cause the file's last modification/accessed time to be
  changed which is usually an undesired side-effect. }

{$IFNDEF RTL140_UP}
const
  MinDateTime: TDateTime = -657434.0;      { 0100-01-01T00:00:00.000 }
  MaxDateTime: TDateTime =  2958465.99999; { 9999-12-31T23:59:59.999 }
{$ENDIF ~RTL140_UP}

{$IFDEF UNIX}
const
  ERROR_NO_MORE_FILES  = -1;
  INVALID_HANDLE_VALUE = THandle(-1);
{$ENDIF UNIX}

//==================================================================================================
// replacements for defective Libc.pas declarations
//==================================================================================================

{$IFDEF KYLIX}

function fstat64(FileDes: Integer; var StatBuffer: TStatBuf64): Integer;
begin
  Result := __fxstat64(_STAT_VER, FileDes, StatBuffer);
end;

//--------------------------------------------------------------------------------------------------

function lstat64(FileName: PChar; var StatBuffer: TStatBuf64): Integer;
begin
  Result := __lxstat64(_STAT_VER, FileName, StatBuffer);
end;

//--------------------------------------------------------------------------------------------------

function stat64(FileName: PChar; var StatBuffer: TStatBuf64): Integer;
begin
  Result := __xstat64(_STAT_VER, FileName, StatBuffer);
end;

{$ENDIF KYLIX}

//==================================================================================================
// TJclTempFileStream
//==================================================================================================

constructor TJclTempFileStream.Create(const Prefix: string);
var
  FileHandle: THandle;
begin
  FFileName := Prefix;
  FileHandle := FileCreateTemp(FFileName);
  // (rom) is it really wise to throw an exception before calling inherited?
  if FileHandle = INVALID_HANDLE_VALUE then
    raise EJclTempFileStreamError.CreateResRec(@RsFileStreamCreate);
  inherited Create(FileHandle);
end;

//--------------------------------------------------------------------------------------------------

destructor TJclTempFileStream.Destroy;
begin
  if THandle(Handle) <> INVALID_HANDLE_VALUE then
    FileClose(Handle);
  inherited Destroy;
end;

//==================================================================================================
// TJclFileMappingView
//==================================================================================================

{$IFDEF MSWINDOWS}

constructor TJclFileMappingView.Create(const FileMap: TJclCustomFileMapping;
  Access, Size: Cardinal; ViewOffset: Int64);
var
  BaseAddress: Pointer;
  OffsetLow, OffsetHigh: Cardinal;
begin
  inherited Create;
  if FileMap = nil then
    raise EJclFileMappingViewError.CreateResRec(@RsViewNeedsMapping);
  FFileMapping := FileMap;
  // Offset must be a multiple of system memory allocation granularity
  RoundToAllocGranularity64(ViewOffset, FFileMapping.RoundViewOffset = rvUp);
  I64ToCardinals(ViewOffset, OffsetLow, OffsetHigh);
  FOffsetHigh := OffsetHigh;
  FOffsetLow := OffsetLow;
  BaseAddress := MapViewOfFile(FFileMapping.Handle, Access, FOffsetHigh, FOffsetLow, Size);
  if BaseAddress = nil then
    raise EJclFileMappingViewError.CreateResRec(@RsCreateFileMappingView);
  // If we are mapping a file and size = 0 then MapViewOfFile has mapped the entire file. We must
  // figure out the size ourselves before we can call SetPointer. Since in case of failure to
  // retrieve the size we raise an exception, we also have to explicitly unmap the view which
  // otherwise would have been done by the destructor.
  if (Size = 0) and (FileMap is TJclFileMapping) then
  begin
    Size := GetFileSize(TJclFileMapping(FileMap).FFileHandle, nil);
    if Size = DWORD(-1) then
    begin
      UnMapViewOfFile(BaseAddress);
      raise EJclFileMappingViewError.CreateResRec(@RsFailedToObtainSize);
    end;
  end;
  SetPointer(BaseAddress, Size);
  FFileMapping.FViews.Add(Self);
end;

//--------------------------------------------------------------------------------------------------

constructor TJclFileMappingView.CreateAt(FileMap: TJclCustomFileMapping;
  Access, Size: Cardinal; ViewOffset: Int64; Address: Pointer);
var
  BaseAddress: Pointer;
  OffsetLow, OffsetHigh: Cardinal;
begin
  inherited Create;
  if FileMap = nil then
    raise EJclFileMappingViewError.CreateResRec(@RsViewNeedsMapping);
  FFileMapping := FileMap;
  // Offset must be a multiple of system memory allocation granularity
  RoundToAllocGranularity64(ViewOffset, FFileMapping.RoundViewOffset = rvUp);
  RoundToAllocGranularityPtr(Address, FFileMapping.RoundViewOffset = rvUp);
  I64ToCardinals(ViewOffset, OffsetLow, OffsetHigh);
  FOffsetHigh := OffsetHigh;
  FOffsetLow := OffsetLow;
  BaseAddress := MapViewOfFileEx(FFileMapping.Handle, Access, FOffsetHigh,
    FOffsetLow, Size, Address);
  if BaseAddress = nil then
    raise EJclFileMappingViewError.CreateResRec(@RsCreateFileMappingView);
  // If we are mapping a file and size = 0 then MapViewOfFile has mapped the entire file. We must
  // figure out the size ourselves before we can call SetPointer. Since in case of failure to
  // retrieve the size we raise an exception, we also have to explicitly unmap the view which
  // otherwise would have been done by the destructor.
  if (Size = 0) and (FileMap is TJclFileMapping) then
  begin
    Size := GetFileSize(TJclFileMapping(FileMap).FFileHandle, nil);
    if Size = DWORD(-1) then
    begin
      UnMapViewOfFile(BaseAddress);
      raise EJclFileMappingViewError.CreateResRec(@RsFailedToObtainSize);
    end;
  end;
  SetPointer(BaseAddress, Size);
  FFileMapping.FViews.Add(Self);
end;

//--------------------------------------------------------------------------------------------------

destructor TJclFileMappingView.Destroy;
var
  IndexOfSelf: Integer;
begin
  if Memory <> nil then
  begin
    UnMapViewOfFile(Memory);
    SetPointer(nil, 0);
  end;
  if FFileMapping <> nil then
  begin
    IndexOfSelf := FFileMapping.IndexOf(Self);
    if IndexOfSelf <> -1 then
      FFileMapping.FViews.Delete(IndexOfSelf);
  end;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileMappingView.Flush(const Count: Cardinal): Boolean;
begin
  Result := FlushViewOfFile(Memory, Count);
end;

//--------------------------------------------------------------------------------------------------

function TJclFileMappingView.GetIndex: Integer;
begin
  Result := FFileMapping.IndexOf(Self);
end;

//--------------------------------------------------------------------------------------------------

function TJclFileMappingView.GetOffset: Int64;
begin
  CardinalsToI64(Result, FOffsetLow, FOffsetHigh);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileMappingView.LoadFromFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    FreeAndNil(Stream);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileMappingView.LoadFromStream(const Stream: TStream);
begin
  if Stream.Size > Size then
    raise EJclFileMappingViewError.CreateResRec(@RsLoadFromStreamSize);
  Stream.Position := 0;
  Stream.ReadBuffer(Memory^, Stream.Size);
end;

//--------------------------------------------------------------------------------------------------

function TJclFileMappingView.Write(const Buffer; Count: Integer): Longint;
begin
  Result := 0;
  if (Size - Position) >= Count then
  begin
    System.Move(Buffer, Pointer(Longint(Memory) + Longint(Position))^, Count);
    Position := Position + Count;
    Result := Count;
  end;
end;

//==================================================================================================
// TJclCustomFileMapping
//==================================================================================================

constructor TJclCustomFileMapping.Create;
begin
  inherited Create;
  FViews := TList.Create;
  FRoundViewOffset := rvDown;
end;

//--------------------------------------------------------------------------------------------------

constructor TJclCustomFileMapping.Open(const Name: string;
  const InheritHandle: Boolean; const DesiredAccess: Cardinal);
begin
  Create;
  InternalOpen(Name, InheritHandle, DesiredAccess);
end;

//--------------------------------------------------------------------------------------------------

destructor TJclCustomFileMapping.Destroy;
begin
  ClearViews;
  if FHandle <> 0 then
    CloseHandle(FHandle);
  FreeAndNil(FViews);
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TJclCustomFileMapping.Add(const Access, Count: Cardinal; const Offset: Int64): Integer;
var
  View: TJclFileMappingView;
begin
  // The view adds itself to the FViews list
  View := TJclFileMappingView.Create(Self, Access, Count, Offset);
  Result := View.Index;
end;

//--------------------------------------------------------------------------------------------------

function TJclCustomFileMapping.AddAt(const Access, Count: Cardinal;
  const Offset: Int64; const Address: Pointer): Integer;
var
  View: TJclFileMappingView;
begin
  // The view adds itself to the FViews list
  View := TJclFileMappingView.CreateAt(Self, Access, Count, Offset, Address);
  Result := View.Index;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclCustomFileMapping.ClearViews;
var
  I: Integer;
begin
  // Note that the view destructor removes the view object from the FViews list so we must loop
  // downwards from count to 0
  for I := FViews.Count - 1 downto 0 do
    TJclFileMappingView(FViews[I]).Free;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclCustomFileMapping.Delete(const Index: Integer);
begin
  // Note that the view destructor removes itself from FViews
  TJclFileMappingView(FViews[Index]).Free;
end;

//--------------------------------------------------------------------------------------------------

function TJclCustomFileMapping.GetCount: Integer;
begin
  Result := FViews.Count;
end;

//--------------------------------------------------------------------------------------------------

function TJclCustomFileMapping.GetView(Index: Integer): TJclFileMappingView;
begin
  Result := TJclFileMappingView(FViews.Items[index]);
end;

//--------------------------------------------------------------------------------------------------

function TJclCustomFileMapping.IndexOf(const View: TJclFileMappingView): Integer;
begin
  Result := FViews.IndexOf(View);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclCustomFileMapping.InternalCreate(const FileHandle: THandle;
  const Name: string; const Protect: Cardinal; MaximumSize: Int64;
  SecAttr: PSecurityAttributes);
var
  MaximumSizeLow, MaximumSizeHigh: Cardinal;
begin
  FName := Name;
  I64ToCardinals(MaximumSize, MaximumSizeLow, MaximumSizeHigh);
  FHandle := CreateFileMapping(FileHandle, SecAttr, Protect, MaximumSizeHigh,
    MaximumSizeLow, PChar(Name));
  if FHandle = 0 then
    raise EJclFileMappingError.CreateResRec(@RsCreateFileMapping);
  FExisted := GetLastError = ERROR_ALREADY_EXISTS;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclCustomFileMapping.InternalOpen(const Name: string;
  const InheritHandle: Boolean; const DesiredAccess: Cardinal);
begin
  FExisted := True;
  FName := Name;
  FHandle := OpenFileMapping(DesiredAccess, InheritHandle, PChar(Name));
  if FHandle = 0 then
    raise EJclFileMappingError.CreateResRec(@RsCreateFileMapping);
end;

//==================================================================================================
// TJclFileMapping
//==================================================================================================

constructor TJclFileMapping.Create(const FileName: string; FileMode: Cardinal;
  const Name: string; Protect: Cardinal; const MaximumSize: Int64;
  SecAttr: PSecurityAttributes);
begin
  FFileHandle := INVALID_HANDLE_VALUE;
  inherited Create;
  FFileHandle := THandle(FileOpen(FileName, FileMode));
  if FFileHandle = INVALID_HANDLE_VALUE then
    raise EJclFileMappingError.CreateResRec(@RsFileMappingOpenFile);
  InternalCreate(FFileHandle, Name, Protect, MaximumSize, SecAttr);
end;

//--------------------------------------------------------------------------------------------------

constructor TJclFileMapping.Create(const FileHandle: THandle; const Name: string;
  Protect: Cardinal; const MaximumSize: Int64; SecAttr: PSecurityAttributes);
begin
  FFileHandle := INVALID_HANDLE_VALUE;
  inherited Create;
  if FileHandle = INVALID_HANDLE_VALUE then
    raise EJclFileMappingError.CreateResRec(@RsFileMappingInvalidHandle);
  InternalCreate(FileHandle, Name, Protect, MaximumSize, SecAttr);
  // Duplicate the handle into FFileHandle as opposed to assigning it directly. This will cause
  // FFileHandle to retrieve a unique copy which is independent of FileHandle. This makes the
  // remainder of the class, especially the destructor, easier. The caller will have to close it's
  // own copy of the handle explicitly.
  DuplicateHandle(GetCurrentProcess, FileHandle, GetCurrentProcess,
    @FFileHandle, 0, False, DUPLICATE_SAME_ACCESS);
end;

//--------------------------------------------------------------------------------------------------

destructor TJclFileMapping.Destroy;
begin
  if FFileHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FFileHandle);
  inherited Destroy;
end;

//==================================================================================================
// TJclSwapFileMapping
//==================================================================================================

constructor TJclSwapFileMapping.Create(const Name: string; Protect: Cardinal;
  const MaximumSize: Int64; SecAttr: PSecurityAttributes);
begin
  inherited Create;
  InternalCreate(INVALID_HANDLE_VALUE, Name, Protect, MaximumSize, SecAttr);
end;

//==================================================================================================
// TJclFileMappingStream
//==================================================================================================

constructor TJclFileMappingStream.Create(const FileName: string; FileMode: Word);
var
  Protect, Access, Size: DWORD;
  BaseAddress: Pointer;
begin
  inherited Create;
  FFileHandle := THandle(FileOpen(FileName, FileMode));
  if FFileHandle = INVALID_HANDLE_VALUE then
    RaiseLastOSError;
  if (FileMode and $0F) = fmOpenReadWrite then
  begin
    Protect := PAGE_WRITECOPY;
    Access := FILE_MAP_COPY;
  end
  else
  begin
    Protect := PAGE_READONLY;
    Access := FILE_MAP_READ;
  end;
  FMapping := CreateFileMapping(FFileHandle, nil, Protect, 0, 0, nil);
  if FMapping = 0 then
  begin
    Close;
    raise EJclFileMappingError.CreateResRec(@RsCreateFileMapping);
  end;
  BaseAddress := MapViewOfFile(FMapping, Access, 0, 0, 0);
  if BaseAddress = nil then
  begin
    Close;
    raise EJclFileMappingViewError.CreateResRec(@RsCreateFileMappingView);
  end;
  Size := GetFileSize(FFileHandle, nil);
  if Size = DWORD(-1) then
  begin
    UnMapViewOfFile(BaseAddress);
    Close;
    raise EJclFileMappingViewError.CreateResRec(@RsFailedToObtainSize);
  end;
  SetPointer(BaseAddress, Size);
end;

//--------------------------------------------------------------------------------------------------

destructor TJclFileMappingStream.Destroy;
begin
  Close;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileMappingStream.Close;
begin
  if Memory <> nil then
  begin
    UnMapViewOfFile(Memory);
    SetPointer(nil, 0);
  end;
  if FMapping <> 0 then
  begin
    CloseHandle(FMapping);
    FMapping := 0;
  end;
  if FFileHandle <> INVALID_HANDLE_VALUE then
  begin
    FileClose(FFileHandle);
    FFileHandle := INVALID_HANDLE_VALUE;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileMappingStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := 0;
  if (Size - Position) >= Count then
  begin
    System.Move(Buffer, Pointer(Longint(Memory) + Longint(Position))^, Count);
    Position := Position + Count;
    Result := Count;
  end;
end;

//==================================================================================================
// TJclMappedTextReader
//==================================================================================================

constructor TJclMappedTextReader.Create(MemoryStream: TCustomMemoryStream; FreeStream: Boolean;
  const AIndexOption: TJclMappedTextReaderIndex);
begin
  inherited Create;
  FMemoryStream := MemoryStream;
  FFreeStream := FreeStream;
  FIndexOption := AIndexOption;
  Init;
end;

//--------------------------------------------------------------------------------------------------

constructor TJclMappedTextReader.Create(const FileName: string;
  const AIndexOption: TJclMappedTextReaderIndex);
begin
  inherited Create;
  FMemoryStream := TJclFileMappingStream.Create(FileName);
  FFreeStream := True;
  FIndexOption := AIndexOption;
  Init;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclMappedTextReader.Destroy;
begin
  if FFreeStream then
    FMemoryStream.Free;
  FreeMem(FIndex);
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMappedTextReader.AssignTo(Dest: TPersistent);
begin
  if Dest is TStrings then
  begin
    GoBegin;
    TStrings(Dest).BeginUpdate;
    try
      while not Eof do
        TStrings(Dest).Add(ReadLn);
    finally
      TStrings(Dest).EndUpdate;
    end;
  end
  else
    inherited AssignTo(Dest);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMappedTextReader.CreateIndex;
var
  P, LastLineStart: PChar;
  I: Integer;
begin
  {$RANGECHECKS OFF}
  P := FContent;
  I := 0;
  LastLineStart := P;
  while P < FEnd do
  begin
    if P^ = AnsiLineFeed then
    begin
      if I and $FFFF = 0 then
        ReallocMem(FIndex, (I + $10000) * SizeOf(Pointer));
      FIndex[I] := LastLineStart;
      Inc(I);
      LastLineStart := P + 1;
    end;
    Inc(P);
  end;
  if P > LastLineStart then
  begin
    ReallocMem(FIndex, (I + 1) * SizeOf(Pointer));
    FIndex[I] := LastLineStart;
    Inc(I);
  end
  else
    ReallocMem(FIndex, I * SizeOf(Pointer));
  FLineCount := I;
  {$IFDEF RANGECHECKS_ON}
  {$RANGECHECKS ON}
  {$ENDIF RANGECHECKS_ON}
end;

//--------------------------------------------------------------------------------------------------

function TJclMappedTextReader.GetEof: Boolean;
begin
  Result := FPosition >= FEnd;
end;

//--------------------------------------------------------------------------------------------------

function TJclMappedTextReader.GetAsString: string;
begin
  SetString(Result, Content, Size);
end;

//--------------------------------------------------------------------------------------------------

function TJclMappedTextReader.GetChars(Index: Integer): Char;
begin
  if (Index < 0) or (Index >= Size) then
    raise EJclError.CreateResRec(@RsFileIndexOutOfRange);
  Result := Char(PByte(FContent + Index)^);
end;

//--------------------------------------------------------------------------------------------------

function TJclMappedTextReader.GetLineCount: Integer;

  function CountLines(P: PChar; Len: Integer): Integer;
  {$IFDEF PUREPASCAL}
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to Len-1 do
    begin
      if P^ = AnsiLineFeed then
        Inc(Result);
      Inc(P);
    end;
  end;
  {$ELSE}
  asm
       PUSH    EDI
       MOV     EDI, EAX
       MOV     ECX, EDX
       MOV     EAX, AnsiLineFeed
       XOR     EDX, EDX
  @@1: REPNZ   SCASB
       INC     EDX
       OR      ECX, ECX
       JNZ     @@1
       MOV     EAX, EDX
       POP     EDI
  end;
  {$ENDIF PUREPASCAL}

begin
  if FLineCount = -1 then
    FLineCount := CountLines(FContent, FSize);
  Result := FLineCount;
end;

//--------------------------------------------------------------------------------------------------

function TJclMappedTextReader.GetLines(LineNumber: Integer): string;
var
  P: PChar;
begin
  P := PtrFromLine(LineNumber);
  Result := StringFromPosition(P);
end;

//--------------------------------------------------------------------------------------------------

function TJclMappedTextReader.GetPosition: Integer;
begin
  Result := FPosition - FContent;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMappedTextReader.GoBegin;
begin
  Position := 0;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMappedTextReader.Init;
begin
  FContent := FMemoryStream.Memory;
  FSize := FMemoryStream.Size;
  FEnd := FContent + FSize;
  FPosition := FContent;
  FLineCount := -1;
  FLastLineNumber := 0;
  FLastPosition := FContent;
  if IndexOption = tiFull then
    CreateIndex;
end;

//--------------------------------------------------------------------------------------------------

function TJclMappedTextReader.GetPositionFromLine(LineNumber: Integer): Integer;
var
  P: PChar;
begin
  P := PtrFromLine(LineNumber);
  if P = nil then
    Result := -1
  else
    Result := P - FContent;
end;

//--------------------------------------------------------------------------------------------------

function TJclMappedTextReader.PtrFromLine(LineNumber: Integer): PChar;
var
  LineOffset: Integer;
begin
  Result := nil;
  {$RANGECHECKS OFF}
  if (IndexOption <> tiNoIndex) and (LineNumber < FLineCount) and (FIndex[LineNumber] <> nil) then
    Result := FIndex[LineNumber]
  {$IFDEF RANGECHECKS_ON}
  {$RANGECHECKS ON}
  {$ENDIF RANGECHECKS_ON}
  else
  begin
    LineOffset := LineNumber - FLastLineNumber;
    if (FLineCount <> -1) and (LineNumber > 0) then
    begin
      if -LineOffset > LineNumber then
      begin
        FLastLineNumber := 0;
        FLastPosition := FContent;
        LineOffset := LineNumber;
      end
      else
      if LineOffset > FLineCount - LineNumber then
      begin
        FLastLineNumber := FLineCount;
        FLastPosition := FEnd;
        LineOffset := LineNumber - FLineCount;
      end;
    end;
    if LineNumber <= 0 then
      Result := FContent
    else
    if LineOffset = 0 then
      Result := FLastPosition
    else
    if LineOffset > 0 then
    begin
      Result := FLastPosition;
      while (Result < FEnd) and (LineOffset > 0) do
      begin
        if Result^ = AnsiLineFeed then
          Dec(LineOffset);
        Inc(Result);
      end;
    end
    else
    if LineOffset < 0 then
    begin
      Result := FLastPosition;
      while (Result >= FContent) and (LineOffset < 1) do
      begin
        if Result^ = AnsiLineFeed then
          Inc(LineOffset);
        Dec(Result);
      end;
      Inc(Result, 2);
    end;
    FLastLineNumber := LineNumber;
    FLastPosition := Result;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclMappedTextReader.Read: Char;
begin
  if FPosition >= FEnd then
    Result := #0
  else
  begin
    Result := FPosition^;
    Inc(FPosition);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclMappedTextReader.ReadLn: string;
begin
  Result := StringFromPosition(FPosition);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMappedTextReader.SetPosition(const Value: Integer);
begin
  FPosition := FContent + Value;
end;

//--------------------------------------------------------------------------------------------------

function TJclMappedTextReader.StringFromPosition(var StartPos: PChar): string;
var
  P, StringEnd: PChar;
begin
  if (StartPos = nil) or (StartPos >= FEnd) then
    Result := ''
  else
  begin
    P := StartPos;
    while (P < FEnd) and (P^ <> AnsiLineFeed) do
      Inc(P);
    if (P > FContent) and ((P - 1)^ = AnsiCarriageReturn) then
      StringEnd := P - 1
    else
      StringEnd := P;
    SetString(Result, StartPos, StringEnd - StartPos);
    StartPos := P + 1;
  end;
end;

{$ENDIF MSWINDOWS}

//==================================================================================================
// Path manipulation
//==================================================================================================

function PathAddSeparator(const Path: string): string;
begin
  Result := Path;
  if (Length(Path) = 0) or (AnsiLastChar(Path) <> PathSeparator) then
    Result := Path + PathSeparator;
end;

//--------------------------------------------------------------------------------------------------

function PathAddExtension(const Path, Extension: string): string;
begin
  Result := Path;
  if (Path <> '') and (ExtractFileExt(Path) = '') and (Extension <> '') then
  begin
    // Note that if we get here Path is guarenteed not to end in a '.' otherwise ExtractFileExt
    // would have returned '.' therefore there's no need to check it explicitly in the code below
    if Extension[1] = '.' then
      Result := Result + Extension
    else
      Result := Result + '.' + Extension;
  end;
end;

//--------------------------------------------------------------------------------------------------

function PathAppend(const Path, Append: string): string;
var
  PathLength: Integer;
  B1, B2: Boolean;
begin
  if Append = '' then
    Result := Path
  else
  begin
    PathLength := Length(Path);
    if PathLength = 0 then
      Result := Append
    else
    begin
      // The following code may look a bit complex but all it does is add Append to Path ensuring
      // that there is one and only one path separator character between them
      B1 := Path[PathLength] = PathSeparator;
      B2 := Append[1] = PathSeparator;
      if B1 and B2 then
        Result := Copy(Path, 1, PathLength - 1) + Append
      else
      begin
        if not (B1 or B2) then
          Result := Path + PathSeparator + Append
        else
          Result := Path + Append;
      end;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function PathBuildRoot(const Drive: Byte): string;
begin
  {$IFDEF UNIX}
  Result := PathSeparator;
  {$ENDIF UNIX}
  {$IFDEF MSWINDOWS}
  // Remember, Win32 only allows 'a' to 'z' as drive letters (mapped to 0..25)
  if Drive < 26 then
    Result := Char(Drive + 65) + ':\'
  else
    raise EJclPathError.CreateResRecFmt(@RsPathInvalidDrive, [IntToStr(Drive)]);
  {$ENDIF MSWINDOWS}
end;

//--------------------------------------------------------------------------------------------------

function PathCanonicalize(const Path: string): string;
var
  List: TStringList;
  S: string;
  I, K: Integer;
  IsAbsolute: Boolean;
begin
  I := Pos(':', Path); // for Windows' sake
  K := Pos(PathSeparator, Path);
  IsAbsolute := K - I = 1;
  if not IsAbsolute then
    K := I;
  if K = 0 then
    S := Path
  else
    S := Copy(Path, K + 1, Length(Path));
  List := TStringList.Create;
  try
    StrIToStrings(S, PathSeparator, List, False);
    I := 0;
    while I < List.Count do
    begin
      if List[I] = '.' then
        List.Delete(I)
      else
      if (IsAbsolute or (I > 0) and not (List[I-1] = '..')) and (List[I] = '..') then
      begin
        List.Delete(I);
        if I > 0 then
        begin
          Dec(I);
          List.Delete(I);
        end;
      end
      else Inc(I);
    end;
    Result := StringsToStr(List, PathSeparator, False);
  finally
    List.Free;
  end;
  if K > 0 then
    Result := Copy(Path, 1, K) + Result
  else
  if Result = '' then
    Result := '.';
end;

//--------------------------------------------------------------------------------------------------

function PathCommonPrefix(const Path1, Path2: string): Integer;
var
  P1, P2: PChar;
  LastSeparator: Integer;
begin
  Result := 0;
  if (Path1 <> '') and (Path2 <> '') then
  begin
    // Initialize P1 to the shortest of the two paths so that the actual comparison loop below can
    // use the terminating #0 of that string to terminate the loop.
    if Length(Path1) <= Length(Path2) then
    begin
      P1 := @Path1[1];
      P2 := @Path2[1];
    end
    else
    begin
      P1 := @Path2[1];
      P2 := @Path1[1];
    end;
    LastSeparator := 0;
    while (P1^ = P2^) and (P1^ <> #0) do
    begin
      Inc(Result);
      if P1^ in [PathSeparator, ':'] then
        LastSeparator := Result;
      Inc(P1);
      Inc(P2);
    end;
    if (LastSeparator < Result) and (P1^ <> #0) then
      Result := LastSeparator;
  end;
end;

//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

function PathCompactPath(const DC: HDC; const Path: string;
  const Width: Integer; CmpFmt: TCompactPath): string;
const
  Compacts: array [TCompactPath] of Cardinal = (DT_PATH_ELLIPSIS, DT_END_ELLIPSIS);
var
  TextRect: TRect;
  Fmt: Cardinal;
begin
  Result := '';
  if (DC <> 0) and (Path <> '') and (Width > 0) then
  begin
    { Here's a note from the Platform SDK to explain the + 5 in the call below:
    "If dwDTFormat includes DT_MODIFYSTRING, the function could add up to four additional characters
    to this string. The buffer containing the string should be large enough to accommodate these
    extra characters." }
    SetString(Result, PChar(Path), Length(Path) + 4);
    TextRect := Rect(0, 0, Width, 255);
    Fmt := DT_MODIFYSTRING or DT_CALCRECT or Compacts[CmpFmt];
    if DrawTextEx(DC, PChar(Result), -1, TextRect, Fmt, nil) <> 0 then
      StrResetLength(Result)
    else
      Result := '';  // in case of error
  end;
end;
                             
{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------

procedure PathExtractElements(const Source: string; var Drive, Path, FileName, Ext: string);
begin
  Drive := ExtractFileDrive(Source);
  Path := ExtractFilePath(Source);
  // Path includes drive so remove that
  if Drive <> '' then
    Delete(Path, 1, Length(Drive));
  // add/remove separators
  Drive := PathAddSeparator(Drive);
  Path := PathRemoveSeparator(Path);
  if (Path <> '') and (Path[1] = PathSeparator) then
    Delete(Path, 1, 1);
  // and extract the remaining elements
  FileName := PathExtractFileNameNoExt(Source);
  Ext := ExtractFileExt(Source);
end;

//--------------------------------------------------------------------------------------------------

function PathExtractFileDirFixed(const S: AnsiString): AnsiString;
begin
  Result := PathAddSeparator(ExtractFileDir(S));
end;

//--------------------------------------------------------------------------------------------------

function PathExtractFileNameNoExt(const Path: string): string;
begin
  Result := PathRemoveExtension(ExtractFileName(Path));
end;

//--------------------------------------------------------------------------------------------------

function PathExtractPathDepth(const Path: string; Depth: Integer): string;
var
  List: TStringList;
  LocalPath: string;
  I: Integer;
begin
  List := TStringList.Create;
  try
    if IsDirectory(Path) then
      LocalPath := Path
    else
      LocalPath := ExtractFilePath(Path);
    StrIToStrings(LocalPath, '\', List, True);
    I := Depth + 1;
    if PathIsUNC(LocalPath) then
      I := I + 2;
    while I < List.Count do
      List.Delete(I);
    Result := PathAddSeparator(StringsToStr(List, '\', True));
  finally
    List.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

//  Notes: maybe this function should first apply PathCanonicalize() ?

function PathGetDepth(const Path: string): Integer;
var
  List: TStringList;
  LocalPath: string;
  I, Start: Integer;
begin
  Result := 0;
  List := TStringList.Create;
  try
    if IsDirectory(Path) then
      LocalPath := Path
    else
      LocalPath := ExtractFilePath(Path);
    StrIToStrings(LocalPath, '\', List, False);
    if PathIsUNC(LocalPath) then
      Start := 1
    else
      Start := 0;
    for I := Start to List.Count - 1 do
    begin
      if Pos(':', List[I]) = 0 then
        Inc(Result);
    end;
  finally
    List.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

{$IFNDEF DROP_OBSOLETE_CODE}

function PathGetLongName2(const Path: string): string;
begin
  Result := PathGetLongName(Path);
end;

{$ENDIF ~DROP_OBSOLETE_CODE}

//--------------------------------------------------------------------------------------------------

function ShellGetLongPathName(const Path: string): string;
{$IFDEF FPC}
// As of 2004-10-17, FPC's ShlObj unit is just a dummy
begin
  Result := Path;
end;
{$ElSE ~FPC}
var
  PIDL: PItemIDList;
  Desktop: IShellFolder;
  AnsiName: AnsiString;
  WideName: array [0..MAX_PATH] of WideChar;
  Eaten, Attr: ULONG; // both unused but API requires them (incorrect translation)
begin
  Result := Path;
  if Path <> '' then
  begin
    if Succeeded(SHGetDesktopFolder(Desktop)) then
    begin
      MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, PChar(Path), -1, WideName, MAX_PATH);
      if Succeeded(Desktop.ParseDisplayName(0, nil, WideName, Eaten, PIDL, Attr)) then
      try
        SetLength(AnsiName, MAX_PATH);
        if SHGetPathFromIDList(PIDL, PChar(AnsiName)) then
          StrResetLength(AnsiName);
        Result := AnsiName;
      finally
        CoTaskMemFree(PIDL);
      end;
    end;
  end;
end;
{$ENDIF ~FPC}

{ TODO : Move RTDL code over to JclWin32 when JclWin32 gets overhauled. }
var
  _Kernel32Handle: TModuleHandle = INVALID_MODULEHANDLE_VALUE;
  _GetLongPathName: function (lpszShortPath: PChar; lpszLongPath: PChar;
    cchBuffer: DWORD): DWORD; stdcall;

function Kernel32Handle: HMODULE;
begin
  JclSysUtils.LoadModule(_Kernel32Handle, kernel32);
  Result := _Kernel32Handle;
end;

function RtdlGetLongPathName(const Path: string): string;
begin
  Result := Path;
  if not Assigned(_GetLongPathName) then
    _GetLongPathName := GetModuleSymbol(Kernel32Handle, 'GetLongPathNameA');
  if not Assigned(_GetLongPathName) then
    Result := ShellGetLongPathName(Path)
  else
  begin
    SetLength(Result, MAX_PATH);
    SetLength(Result, _GetLongPathName(PChar(Path), PChar(Result), MAX_PATH));
  end;
end;

function PathGetLongName(const Path: string): string;
begin
  if Pos('::', Path) > 0 then // Path contains '::{<GUID>}'
    Result := ShellGetLongPathName(Path)
  else
    Result := RtdlGetLongPathName(Path);
end;

//--------------------------------------------------------------------------------------------------

function PathGetShortName(const Path: string): string;
var
  Required: Integer;
begin
  Result := Path;
  Required := GetShortPathName(PChar(Path), nil, 0);
  if Required <> 0 then
  begin
    SetLength(Result, Required);
    Required := GetShortPathName(PChar(Path), PChar(Result), Required);
    if (Required <> 0) and (Required = Length(Result) - 1) then
      SetLength(Result, Required)
    else
      Result := Path;
  end;
end;

{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------

function PathGetRelativePath(Origin, Destination: string): string;
var
  OrigDrive: string;
  DestDrive: string;
  OrigList: TStringList;
  DestList: TStringList;
  DiffIndex: Integer;
  I: Integer;

  function StartsFromRoot(const Path: string): Boolean;
 {$IFDEF MSWINDOWS}
  var
    I: Integer;
  begin
    I := Length(ExtractFileDrive(Path));
    Result := (Length(Path) > I) and (Path[I + 1] = PathSeparator);
  end;
  {$ELSE}
  begin
    Result := Pos(PathSeparator, Path) = 1;
  end;
  {$ENDIF}

begin
  Origin := PathCanonicalize(Origin);
  Destination := PathCanonicalize(Destination);
  OrigDrive := ExtractFileDrive(Origin);
  DestDrive := ExtractFileDrive(Destination);
  if (Origin = Destination) or (Destination = '') then
    Result := ''
  else
  if Origin = '' then
    Result := Destination
  else
  {$IFDEF MSWINDOWS}
  if (DestDrive <> '') and ((OrigDrive = '') or ((OrigDrive <> '') and (OrigDrive <> DestDrive))) then
    Result := Destination
  else
  if (OrigDrive <> '') and (Pos('\', Destination) = 1) then
    Result := OrigDrive + Destination  // prepend drive part from Origin
  else
  {$ENDIF MSWINDOWS}
  if StartsFromRoot(Origin) and not StartsFromRoot(Destination) then
    Result := StrEnsureSuffix(PathSeparator, Origin) +
      StrEnsureNoPrefix(PathSeparator, Destination)
  else
  begin
    // create a list of paths as separate strings
    OrigList := TStringList.Create;
    DestList := TStringList.Create;
    try
      // NOTE: DO NOT USE DELIMITER AND DELIMITEDTEXT FROM
      // TSTRINGS, THEY WILL SPLIT PATHS WITH SPACES !!!!
      StrToStrings(Origin, PathSeparator, OrigList);
      StrToStrings(Destination, PathSeparator, DestList);
      begin
        // find the first directory that is not the same
        DiffIndex := OrigList.Count;
        if DestList.Count < DiffIndex then
          DiffIndex := DestList.Count;
        for I := 0 to DiffIndex - 1 do
        {$IFDEF MSWINDOWS} // case insensitive
          if not StrSame(OrigList[I], DestList[I]) then
        {$ELSE}            // case sensitive
          if OrigList[I] <> DestList[I] then
        {$ENDIF }
          begin
            DiffIndex := I;
            Break;
          end;

        Result := StrRepeat('..' + PathSeparator, OrigList.Count - DiffIndex);
        if DiffIndex < DestList.Count then
          Result := Result + DestList[DiffIndex];
        for I := DiffIndex + 1 to DestList.Count - 1 do
          Result := Result + PathSeparator + DestList[i];
      end;
    finally
      DestList.Free;
      OrigList.Free;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function PathGetTempPath: string;
{$IFDEF MSWINDOWS}
var
  BufSize: Cardinal;
begin
  BufSize := Windows.GetTempPath(0, nil);
  SetLength(Result, BufSize);
  { TODO : Check length (-1 or not) }
  Windows.GetTempPath(BufSize, PChar(Result));
  StrResetLength(Result);
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
begin
  Result := GetEnvironmentVariable('TMPDIR');
end;
{$ENDIF UNIX}

//--------------------------------------------------------------------------------------------------

function PathIsAbsolute(const Path: string): Boolean;
{$IFDEF MSWINDOWS}
var
  I: Integer;
{$ENDIF MSWINDOWS}
begin
  Result := False;
  if Path <> '' then
  begin
    {$IFDEF UNIX}
    Result := (Path[1] = PathSeparator);
    {$ENDIF UNIX}
    {$IFDEF MSWINDOWS}
    I := 0;
    if PathIsUnc(Path) then
      I := Length(PathUncPrefix)
    else
    if PathIsDiskDevice(Path) then
      I := Length(PathDevicePrefix);
    Result := (Length(Path) > I + 2) and (Path[I + 1] in DriveLetters) and
      (Path[I + 2] = ':') and (Path[I + 3] = PathSeparator);
    {$ENDIF MSWINDOWS}
  end;
end;

//--------------------------------------------------------------------------------------------------

function PathIsChild(const Path, Base: AnsiString): Boolean;
var
  L: Integer;
  B, P: string;
begin
  Result := False;
  B := PathRemoveSeparator(Base);
  P := PathRemoveSeparator(Path);
  // an empty path or one that's not longer than base cannot be a subdirectory
  L := Length(B);
  if (P = '') or (L >= Length(P)) then
    Exit;
  {$IFDEF MSWINDOWS}
  Result := AnsiSameText(StrLeft(P, L), B) and (P[L+1] = PathSeparator);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  Result := AnsiSameStr(StrLeft(P, L), B) and (P[L+1] = PathSeparator);
  {$ENDIF UNIX}
end;

//--------------------------------------------------------------------------------------------------

function PathIsDiskDevice(const Path: string): Boolean;
{$IFDEF UNIX}
var
  FullPath: string;
  F: PIOFile;
  Buffer: array [0..255] of Char;
  MountEntry: TMountEntry;
  FsTypes: TStringList;

  procedure GetAvailableFileSystems(const List: TStrings);
  var
    F: TextFile;
    S: string;
  begin
    AssignFile(F, '/proc/filesystems');
    Reset(F);
    repeat
      Readln(F, S);
      if Pos('nodev', S) = 0 then // how portable is this ?
        List.Add(Trim(S));
    until Eof(F);
    List.Add('supermount');
    CloseFile(F);
  end;

begin
  Result := False;

  SetLength(FullPath, _POSIX_PATH_MAX);
  if realpath(PChar(Path), PChar(FullPath)) = nil then
    RaiseLastOSError;
  StrResetLength(FullPath);
  
  FsTypes := TStringList.Create;
  try
    GetAvailableFileSystems(FsTypes);
    F := setmntent(_PATH_MOUNTED, 'r'); // PATH_MOUNTED is deprecated,
                                        // but PATH_MNTTAB is defective in Libc.pas
    try
      // get drives from mtab
      while not Result and (getmntent_r(F, MountEntry, Buffer, SizeOf(Buffer)) <> nil) do
        if FsTypes.IndexOf(MountEntry.mnt_type) <> -1 then
          Result := MountEntry.mnt_dir = FullPath;

    finally
      endmntent(F);
    end;
  finally
    FsTypes.Free;
  end;
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
begin
  Result := Copy(Path, 1, Length(PathDevicePrefix)) = PathDevicePrefix;
end;
{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------

function PathIsUNC(const Path: string): Boolean;

{$IFDEF MSWINDOWS}

const
  cUNCSuffix = '?\UNC';

var
  P: PChar;

  function AbsorbSeparator: Boolean;
  begin
    Result := (P <> nil) and (P^ = '\');
    if Result then
      Inc(P);
  end;

  function AbsorbMachineName: Boolean;
  var
    NonDigitFound: Boolean;
  begin
    // a valid machine name is a string composed of the set [a-z, A-Z, 0-9, -, _] but it may not
    // consist entirely out of numbers
    Result := True;
    NonDigitFound := False;
    while (P <> nil) and (P^ <> #0) and (P^ <> '\') do
    begin
      if P^ in ['a'..'z', 'A'..'Z', '-', '_', '.'] then
      begin
        NonDigitFound := True;
        Inc(P);
      end
      else
      if P^ in ['0'..'9'] then
        Inc(P)
      else
      begin
        Result := False;
        Break;
      end;
    end;
    Result := Result and NonDigitFound;
  end;

  function AbsorbShareName: Boolean;
  const
    InvalidCharacters =
      ['<', '>', '?', '/', ',', '*', '+', '=', '[', ']', '|', ':', ';', '"', ''''];
  begin
    // a valid share name is a string composed of a set the set !InvalidCharacters note that a
    // leading '$' is valid (indicates a hidden share)
    Result := True;
    while (P <> nil) and (P^ <> #0) and (P^ <> '\') do
    begin
      if P^ in InvalidCharacters then
      begin
        Result := False;
        Break;
      end;
      Inc(P);
    end;
  end;

begin
  Result := Copy(Path, 1, Length(PathUncPrefix)) = PathUncPrefix;
  if Result then
  begin
    if Copy(Path, 1, Length(PathUncPrefix + cUNCSuffix)) = PathUncPrefix + cUNCSuffix then
      P := @Path[Length(PathUncPrefix + cUNCSuffix)]
    else
    begin
      P := @Path[Length(PathUncPrefix)];
      Result := AbsorbSeparator and AbsorbMachineName;
    end;
    Result := Result and AbsorbSeparator;
    if Result then
    begin
      Result := AbsorbShareName;
      // remaining, if anything, is path and or filename (optional) check those?
    end;
  end;
end;

{$ENDIF MSWINDOWS}

{$IFDEF UNIX}

begin
  Result := False;
end;

{$ENDIF UNIX}

//--------------------------------------------------------------------------------------------------

function PathRemoveSeparator(const Path: string): string;
var
  L: Integer;
begin
  L := Length(Path);
  if (L <> 0) and (AnsiLastChar(Path) = PathSeparator) then
    Result := Copy(Path, 1, L - 1)
  else
    Result := Path;
end;

//--------------------------------------------------------------------------------------------------

function PathRemoveExtension(const Path: string): string;
var
  I: Integer;
begin
  I := LastDelimiter(':.' + PathSeparator, Path);
  if (I > 0) and (Path[I] = '.') then
    Result := Copy(Path, 1, I - 1)
  else
    Result := Path;
end;

//==================================================================================================
// Files and Directories
//==================================================================================================

function BuildFileList(const Path: string; const Attr: Integer; const List: TStrings): Boolean;
var
  SearchRec: TSearchRec;
  R: Integer;
begin
  Assert(List <> nil);
  R := FindFirst(Path, Attr, SearchRec);
  Result := R = 0;
  List.BeginUpdate;
  try
    if Result then
    begin
      while R = 0 do
      begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          List.Add(SearchRec.Name);
        R := FindNext(SearchRec);
      end;
      Result := R = ERROR_NO_MORE_FILES;
    end;
  finally
    SysUtils.FindClose(SearchRec);
    List.EndUpdate;
  end;  
end;

//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

function CloseVolume(var Volume: THandle): Boolean;
begin
  Result := False;
  if Volume <> INVALID_HANDLE_VALUE then
  begin
    Result := CloseHandle(Volume);
    if Result then
      Volume := INVALID_HANDLE_VALUE;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure CreateEmptyFile(const FileName: string);
var
  Handle: THandle;
begin
  Handle := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_ALWAYS, 0, 0);
  if Handle <> INVALID_HANDLE_VALUE then
    CloseHandle(Handle)
  else
    RaiseLastOSError;
end;

//--------------------------------------------------------------------------------------------------

{$IFNDEF FPC}  // needs JclShell
{ TODO -cHelp : Author: Jeff (but FileUtils.dtx says "Donator: Anthony Steele". Excuse me?) }

function DeleteDirectory(const DirectoryName: string; MoveToRecycleBin: Boolean): Boolean;
begin
  if MoveToRecycleBin then
    Result := SHDeleteFolder(0, DirectoryName, [doSilent, doAllowUndo])
  else
    Result := DelTree(DirectoryName);
end;
{$ENDIF ~FPC}

//--------------------------------------------------------------------------------------------------

function DelTree(const Path: string): Boolean;
begin
  Result := DelTreeEx(Path, False, nil);
end;

//--------------------------------------------------------------------------------------------------

function DelTreeEx(const Path: string; AbortOnFailure: Boolean; Progress: TDelTreeProgress): Boolean;
var
  Files: TStringList;
  LPath: string; // writable copy of Path
  FileName: string;
  I: Integer;
  PartialResult: Boolean;
  Attr: DWORD;
begin
  Assert(Path <> '', RsDelTreePathIsEmpty);
  {$IFNDEF ASSERTIONS_ON}
  if Path = '' then
  begin
    Result := False;
    Exit;
  end;
  {$ENDIF ~ASSERTIONS_ON}
  Result := True;
  Files := TStringList.Create;
  try
    LPath := PathRemoveSeparator(Path);
    BuildFileList(LPath + '\*.*', faAnyFile, Files);
    for I := 0 to Files.Count - 1 do
    begin
      FileName := LPath + '\' + Files[I];
      PartialResult := True;
      // If the current file is itself a directory then recursively delete it
      Attr := GetFileAttributes(PChar(FileName));
      if (Attr <> DWORD(-1)) and ((Attr and FILE_ATTRIBUTE_DIRECTORY) <> 0) then
        PartialResult := DelTreeEx(FileName, AbortOnFailure, Progress)
      else
      begin
        if Assigned(Progress) then
          PartialResult := Progress(FileName, Attr);
        if PartialResult then
        begin
          // Set attributes to normal in case it's a readonly file
          PartialResult := SetFileAttributes(PChar(FileName), FILE_ATTRIBUTE_NORMAL);
          if PartialResult then
            PartialResult := DeleteFile(FileName);
        end;
      end;
      if not PartialResult then
      begin
        Result := False;
        if AbortOnFailure then
          Break;
      end;
    end;
  finally
    FreeAndNil(Files);
  end;
  if Result then
  begin
    // Finally remove the directory itself
    Result := SetFileAttributes(PChar(LPath), FILE_ATTRIBUTE_NORMAL);
    if Result then
    begin
      {$IOCHECKS OFF}
      RmDir(LPath);
      {$IFDEF IOCHECKS_ON}
      {$IOCHECKS ON}
      {$ENDIF IOCHECKS_ON}
      Result := IOResult = 0;
    end;
  end;
end;

{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
function DirectoryExists(const Name: string): Boolean;
var
  R: DWORD;
begin
  R := GetFileAttributes(PChar(Name));
  Result := (R <> DWORD(-1)) and ((R and FILE_ATTRIBUTE_DIRECTORY) <> 0);
end;
{$ENDIF MSWINDOWS}

{$IFDEF UNIX}
function DirectoryExists(const Name: string; ResolveSymLinks: Boolean): Boolean;
begin
  Result := IsDirectory(Name, ResolveSymLinks);
end;
{$ENDIF UNIX}

//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

function DiskInDrive(Drive: Char): Boolean;
var
  ErrorMode: Cardinal;
begin
  Result := False;
  if Drive in ['a'..'z'] then
    Dec(Drive, $20);
  Assert(Drive in ['A'..'Z']);
  if Drive in ['A'..'Z'] then
  begin
  { try to access the drive, it doesn't really matter how we access the drive and as such calling
    DiskSize is more or less a random choice. The call to SetErrorMode supresses the system provided
    error dialog if there is no disk in the drive and causes the to DiskSize to fail. }
    ErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
    try
      Result := DiskSize(Ord(Drive) - $40) <> -1;
    finally
      SetErrorMode(ErrorMode);
    end;
  end;
end;

{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------

function FileCreateTemp(var Prefix: string): THandle;
{$IFDEF MSWINDOWS}
var
  TempName: string;
begin
  Result := INVALID_HANDLE_VALUE;
  TempName := FileGetTempName(Prefix);
  if TempName <> '' then
  begin
    Result := CreateFile(PChar(TempName), GENERIC_READ or GENERIC_WRITE, 0, nil,
      OPEN_EXISTING, FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE, 0);
    // In certain situations it's possible that CreateFile fails yet the file is actually created,
    // therefore explicitly delete it upon failure.
    if Result = INVALID_HANDLE_VALUE then
      DeleteFile(TempName);
    Prefix := TempName;
  end;
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
var
  Template: string;
begin
  // The mkstemp function generates a unique file name just as mktemp does, but
  // it also opens the file for you with open. If successful, it modifies
  // template in place and returns a file descriptor for that file open for
  // reading and writing. If mkstemp cannot create a uniquely-named file, it
  // returns -1. If template does not end with `XXXXXX', mkstemp returns -1 and
  // does not modify template.

  // The file is opened using mode 0600. If the file is meant to be used by
  // other users this mode must be changed explicitly.

  // Unlike mktemp, mkstemp is actually guaranteed to create a unique file that
  // cannot possibly clash with any other program trying to create a temporary
  // file. This is because it works by calling open with the O_EXCL flag, which
  // says you want to create a new file and get an error if the file already
  // exists.
  Template := Prefix + 'XXXXXX';
  Result := mkstemp(PChar(Template));
  Prefix := Template;
end;
{$ENDIF UNIX}

//--------------------------------------------------------------------------------------------------

function FileBackup(const FileName: string; Move: Boolean = False): Boolean;
begin
  if Move then
    Result := FileMove(FileName, GetBackupFileName(FileName), True)
  else
    Result := FileCopy(FileName, GetBackupFileName(FileName), True);
end;

//--------------------------------------------------------------------------------------------------

function FileCopy(const ExistingFileName, NewFileName: string; ReplaceExisting: Boolean = False): Boolean;
var
  {$IFDEF UNIX}
  SrcFile, DstFile: file;
  Buf: array[0..511] of Byte;
  BytesRead: Integer;
  {$ENDIF UNIX}
  DestFileName: string;
begin
  if IsDirectory(NewFileName) then
    DestFileName := PathAddSeparator(NewFileName) + ExtractFileName(ExistingFileName)
  else
    DestFileName := NewFileName;
  {$IFDEF MSWINDOWS}
  { TODO : Use CopyFileEx where available? }
  Result := CopyFile(PChar(ExistingFileName), PChar(DestFileName), not ReplaceExisting);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  Result := False;
  if not FileExists(DestFileName) or ReplaceExisting then
  begin
    AssignFile(SrcFile, ExistingFileName);
    Reset(SrcFile, 1);
    AssignFile(DstFile, DestFileName);
    Rewrite(DstFile, 1);
    while not Eof(SrcFile) do
    begin
      BlockRead(SrcFile, Buf, SizeOf(Buf), BytesRead);
      BlockWrite(DstFile, Buf, BytesRead);
    end;
    CloseFile(DstFile);
    CloseFile(SrcFile);
    Result := True;
  end;
  {$ENDIF UNIX}
end;

//--------------------------------------------------------------------------------------------------

function FileDelete(const FileName: string; MoveToRecycleBin: Boolean = False): Boolean;
{$IFDEF MSWINDOWS}
  { TODO -cHelp : Author: Jeff (but FileUtils.dtx says "Donator: Marcel van Brakel". Excuse me?) }
begin
  {$IFNDEF FPC}  // needs JclShell
  if MoveToRecycleBin then
    Result := SHDeleteFiles(0, FileName, [doSilent, doAllowUndo, doFilesOnly])
  else
  {$ENDIF ~FPC}
    Result := Windows.DeleteFile(PChar(FileName));
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
  { TODO : implement MoveToRecycleBin for appropriate Desktops (e.g. KDE) }
begin
  Result := remove(PChar(FileName)) <> -1;
end;
{$ENDIF UNIX}

//--------------------------------------------------------------------------------------------------

function FileExists(const FileName: string): Boolean;
begin
  // Attempt to access the file, doesn't matter how, using FileGetSize is as good as anything else.
  Result := FileGetSize(FileName) <> -1;
end;

//--------------------------------------------------------------------------------------------------

function FileMove(const ExistingFileName, NewFileName: string; ReplaceExisting: Boolean = False): Boolean;
{$IFDEF MSWINDOWS}
const
  Flag: array[Boolean] of Cardinal = (0, MOVEFILE_REPLACE_EXISTING);
{$ENDIF MSWINDOWS}
begin
  {$IFDEF MSWINDOWS}
  Result := MoveFileEx(PChar(ExistingFileName), PChar(NewFileName), Flag[ReplaceExisting]);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  Result := __rename(PChar(ExistingFileName), PChar(NewFileName)) = 0;
  {$ENDIF UNIX}
  if not Result then
  begin
    Result := FileCopy(ExistingFileName, NewFileName, ReplaceExisting);
    if Result then
      FileDelete(ExistingFileName);
  end;
end;

//--------------------------------------------------------------------------------------------------

function FileRestore(const FileName: string): Boolean;
var
  TempFileName: string;
begin
  Result := False;
  TempFileName := FileGetTempName('');

  if FileMove(GetBackupFileName(FileName), TempFileName, True) then
    if FileBackup(FileName, False) then
      Result := FileMove(TempFileName, FileName, True);
end;

//--------------------------------------------------------------------------------------------------

function GetBackupFileName(const FileName: string): string;
var
  NewExt: string;
begin
  NewExt := ExtractFileExt(FileName);
  if Length(NewExt) > 0 then
  begin
    NewExt[1] := '~';
    NewExt := '.' + NewExt
  end;
  Result := ChangeFileExt(FileName, NewExt);
end;

//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

function FileGetDisplayName(const FileName: string): string;
{$IFDEF FPC}
var
  FileInfo: TSHFileInfoA;
begin
  FillChar(FileInfo, SizeOf(FileInfo), #0);
  if SHGetFileInfo(PChar(FileName), 0, @FileInfo, SizeOf(FileInfo), SHGFI_DISPLAYNAME) <> nil then
    Result := FileInfo.szDisplayName
  else
    Result := FileName;
end;
{$ELSE ~FPC}
var
  FileInfo: TSHFileInfo;
begin
  FillChar(FileInfo, SizeOf(FileInfo), #0);
  if SHGetFileInfo(PChar(FileName), 0, FileInfo, SizeOf(FileInfo), SHGFI_DISPLAYNAME) <> 0 then
    Result := FileInfo.szDisplayName
  else
    Result := FileName;
end;
{$ENDIF ~FPC}

{$ELSE ~MSWINDOWS}

function FileGetDisplayName(const FileName: string): string;
begin
  { TODO -cHelp : mention this reduced solution }
  Result := FileName;
end;

{$ENDIF ~MSWINDOWS}

//--------------------------------------------------------------------------------------------------

{ TODO -cHelp : Donator: Scott Price; Contributor: Robert Rossmair }
function FileGetGroupName(const FileName: string): string;
{$IFDEF MSWINDOWS}
var
  DomainName: AnsiString;
  pSD: PSecurityDescriptor;
  BufSize: DWORD;
begin
  if IsWinNT then
  begin
    GetFileSecurity(PChar(FileName), GROUP_SECURITY_INFORMATION, nil, 0, BufSize);
    if BufSize > 0 then
    begin
      GetMem(pSD, BufSize);
      GetFileSecurity(PChar(FileName), GROUP_SECURITY_INFORMATION,
        pSD, BufSize, BufSize);
      LookupAccountBySid(Pointer(Integer(pSD) + Integer(pSD^.Group)), Result, DomainName);
      FreeMem(pSD);
      Result := Trim(Result);
    end;
  end;
end;
{$ENDIF ~MSWINDOWS}
{$IFDEF UNIX}
begin
  { TODO:  Implementation to retrieve similar data from Nix type systems }
end;
{$ENDIF ~UNIX}

//--------------------------------------------------------------------------------------------------

{ TODO -cHelp : Donator: Scott Price; Contributor: Robert Rossmair }
function FileGetOwnerName(const FileName: string): string;
{$IFDEF MSWINDOWS}
var
  DomainName: AnsiString;
  pSD: PSecurityDescriptor;
  BufSize: DWORD;
begin
  if IsWinNT then
  begin
    GetFileSecurity(PChar(FileName), OWNER_SECURITY_INFORMATION, nil, 0, BufSize);
    if BufSize > 0 then
    begin
      GetMem(pSD, BufSize);
      GetFileSecurity(PChar(FileName), OWNER_SECURITY_INFORMATION,
        pSD, BufSize, BufSize);
      LookupAccountBySid(Pointer(Integer(pSD) + Integer(pSD^.Owner)), Result, DomainName);
      FreeMem(pSD);
      Result := Trim(Result);
    end;
  end;
end;
{$ENDIF ~MSWINDOWS}
{$IFDEF UNIX}
begin
  { TODO:  Implementation to retrieve similar data from Nix type systems }
end;
{$ENDIF ~UNIX}

//--------------------------------------------------------------------------------------------------

function FileGetSize(const FileName: string): Integer;
var
  SearchRec: TSearchRec;
{$IFDEF MSWINDOWS}
  OldMode: Cardinal;
{$ENDIF MSWINDOWS}
begin
  Result := -1;
{$IFDEF MSWINDOWS}
  OldMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
{$ENDIF MSWINDOWS}
    if FindFirst(FileName, faAnyFile, SearchRec) = 0 then
    begin
      Result := SearchRec.Size;
      SysUtils.FindClose(SearchRec);
    end;
{$IFDEF MSWINDOWS}
  finally
    SetErrorMode(OldMode);
  end;
{$ENDIF MSWINDOWS}
end;

//--------------------------------------------------------------------------------------------------

{$IFDEF FPC}
{ TODO : Move this over to JclWin32 when JclWin32 gets overhauled. }
function GetTempFileName(lpPathName, lpPrefixString: PChar;
  uUnique: UINT; lpTempFileName: PChar): UINT; stdcall;
external kernel32 name 'GetTempFileNameA';
{$ENDIF FPC}

function FileGetTempName(const Prefix: string): string;
{$IFDEF MSWINDOWS}
var
  TempPath, TempFile: string;
  R: Cardinal;
begin
  Result := '';
  TempPath := PathGetTempPath;
  if TempPath <> '' then
  begin
    SetLength(TempFile, MAX_PATH);
    R := GetTempFileName(PChar(TempPath), PChar(Prefix), 0, PChar(TempFile));
    if R <> 0 then
    begin
      StrResetLength(TempFile);
      Result := TempFile;
    end;
  end;
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
// Warning: Between the time the pathname is constructed and the file is created
// another process might have created a file with the same name using tmpnam,
// leading to a possible security hole. The implementation generates names which
// can hardly be predicted, but when opening the file you should use the O_EXCL
// flag. Using tmpfile or mkstemp is a safe way to avoid this problem.
var
  P: PChar;
begin
  P := tempnam(PChar(PathGetTempPath), PChar(Prefix));
  Result := P;
  Libc.free(P);
end;
{$ENDIF UNIX}

//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

function FileGetTypeName(const FileName: string): string;
{$IFDEF FPC}
var
  FileInfo: TSHFileInfoA;
  RetVal: Pointer;
begin
  FillChar(FileInfo, SizeOf(FileInfo), #0);
  RetVal := SHGetFileInfo(PChar(FileName), 0, @FileInfo, SizeOf(FileInfo),
    SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES);
  if RetVal <> nil then
    Result := FileInfo.szTypeName;
  if (RetVal = nil) or (Trim(Result) = '') then
  begin
    // Lookup failed so mimic explorer behaviour by returning "XYZ File"
    Result := ExtractFileExt(FileName);
    Delete(Result, 1, 1);
    Result := TrimLeft(UpperCase(Result) + RsDefaultFileTypeName);
  end;
end;
{$ELSE ~FPC}
var
  FileInfo: TSHFileInfo;
  RetVal: DWORD;
begin
  FillChar(FileInfo, SizeOf(FileInfo), #0);
  RetVal := SHGetFileInfo(PChar(FileName), 0, FileInfo, SizeOf(FileInfo),
    SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES);
  if RetVal <> 0 then
    Result := FileInfo.szTypeName;
  if (RetVal = 0) or (Trim(Result) = '') then
  begin
    // Lookup failed so mimic explorer behaviour by returning "XYZ File"
    Result := ExtractFileExt(FileName);
    Delete(Result, 1, 1);
    Result := TrimLeft(UpperCase(Result) + RsDefaultFileTypeName);
  end;
end;
{$ENDIF ~FPC}

{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------

function FindUnusedFileName(const FileName, FileExt, Suffix: AnsiString): AnsiString;
var
  I: Integer;
begin
  Result := FileName + '.' + FileExt;
  I := 0;
  while FileExists(Result) do
  begin
    Inc(I);
    Result := FileName + Suffix + IntToStr(I) + '.' + FileExt;
  end;
end;

//--------------------------------------------------------------------------------------------------

// This routine is copied from FileCtrl.pas to avoid dependency on that unit.
// See the remark at the top of this section

function ForceDirectories(Name: string): Boolean;
begin
  Result := True;
  if Length(Name) = 0 then
    raise EJclFileUtilsError.CreateResRec(@RsCannotCreateDir);
  Name := PathRemoveSeparator(Name);
  {$IFDEF MSWINDOWS}
  if (Length(Name) < 3) or DirectoryExists(Name) or (ExtractFilePath(Name) = Name) then
    Exit;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  if (Length(Name) = 0) or DirectoryExists(Name) then
    Exit;
  {$ENDIF UNIX}
  Result := ForceDirectories(ExtractFilePath(Name)) and CreateDir(Name);
end;

//--------------------------------------------------------------------------------------------------

function GetDirectorySize(const Path: string): Int64;

  function RecurseFolder(const Path: string): Int64;
  var
    F: TSearchRec;
    R: Integer;
    {$IFDEF MSWINDOWS}
    TempSize: TULargeInteger;
    {$ENDIF MSWINDOWS}
  begin
    Result := 0;
    R := SysUtils.FindFirst(Path + '*.*', faAnyFile, F);
    if R = 0 then
    try
      while R = 0 do
      begin
        if (F.Name <> '.') and (F.Name <> '..') then
        begin
          if (F.Attr and faDirectory) = faDirectory then
            Inc(Result, RecurseFolder(Path + F.Name + '\'))
          else
          {$IFDEF MSWINDOWS}
          begin
            TempSize.LowPart := F.FindData.nFileSizeLow;
            TempSize.HighPart := F.FindData.nFileSizeHigh;
            Inc(Result, TempSize.QuadPart);
          end;
          {$ENDIF MSWINDOWS}
          {$IFDEF UNIX}
            // SysUtils.Find* don't perceive files >= 2 GB anyway 
            Inc(Result, Int64(F.Size));
          {$ENDIF UNIX}
        end;
        R := SysUtils.FindNext(F);
      end;
      if R <> ERROR_NO_MORE_FILES then
        Abort;
    finally
      SysUtils.FindClose(F);
    end;
  end;

begin
  if not DirectoryExists(PathRemoveSeparator(Path)) then
    Result := -1
  else
  try
    Result := RecurseFolder(PathAddSeparator(Path))
  except
    Result := -1;
  end;
end;

//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

function GetDriveTypeStr(const Drive: Char): string;
var
  DriveType: Integer;
  DriveStr: string;
begin
  if not (Drive in ['a'..'z', 'A'..'Z']) then
    raise EJclPathError.CreateResRecFmt(@RsPathInvalidDrive, [Drive]);
  DriveStr := Drive + ':\';
  DriveType := GetDriveType(PChar(DriveStr));
  case DriveType of
    DRIVE_REMOVABLE:
      Result := RsRemovableDrive;
    DRIVE_FIXED:
      Result := RsHardDisk;
    DRIVE_REMOTE:
      Result := RsRemoteDrive;
    DRIVE_CDROM:
      Result := RsCDRomDrive;
    DRIVE_RAMDISK:
      Result := RsRamDisk;
    else
      Result := RsUnknownDrive;
  end;
end;

//--------------------------------------------------------------------------------------------------

function GetFileAgeCoherence(const FileName: string): Boolean;
var
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Result := False;
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    {$IFDEF FPC}
    Result := CompareFileTime(@FindData.ftCreationTime, @FindData.ftLastWriteTime) <= 0;
    {$ELSE ~FPC}
    Result := CompareFileTime(FindData.ftCreationTime, FindData.ftLastWriteTime) <= 0;
    {$ENDIF ~FPC}
  end;
end;

{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------

procedure GetFileAttributeList(const Items: TStrings; const Attr: Integer);
begin
  { TODO : clear list? }
  Assert(Assigned(Items));
  if not Assigned(Items) then
    Exit;
  Items.BeginUpdate;
  try
    { TODO : differentiate Windows/UNIX idents }
    if Attr and faDirectory = faDirectory then
      Items.Add(RsAttrDirectory);
    if Attr and faReadOnly = faReadOnly then
      Items.Add(RsAttrReadOnly);
    if Attr and faSysFile = faSysFile then
      Items.Add(RsAttrSystemFile);
    if Attr and faVolumeID = faVolumeID then
      Items.Add(RsAttrVolumeID);
    if Attr and faArchive = faArchive then
      Items.Add(RsAttrArchive);
    if Attr and faAnyFile = faAnyFile then
      Items.Add(RsAttrAnyFile);
    if Attr and faHidden = faHidden then
      Items.Add(RsAttrHidden);
  finally
    Items.EndUpdate;
  end;
end;

//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

{ TODO : GetFileAttributeListEx - Unix version }
procedure GetFileAttributeListEx(const Items: TStrings; const Attr: Integer);
begin
  { TODO : clear list? }
  Assert(Assigned(Items));
  if not Assigned(Items) then
    Exit;
  Items.BeginUpdate;
  try
    if Attr and FILE_ATTRIBUTE_READONLY = FILE_ATTRIBUTE_READONLY then
      Items.Add(RsAttrReadOnly);
    if Attr and FILE_ATTRIBUTE_HIDDEN = FILE_ATTRIBUTE_HIDDEN then
      Items.Add(RsAttrHidden);
    if Attr and FILE_ATTRIBUTE_SYSTEM = FILE_ATTRIBUTE_SYSTEM then
      Items.Add(RsAttrSystemFile);
    if Attr and FILE_ATTRIBUTE_DIRECTORY = FILE_ATTRIBUTE_DIRECTORY then
      Items.Add(RsAttrDirectory);
    if Attr and FILE_ATTRIBUTE_ARCHIVE = FILE_ATTRIBUTE_ARCHIVE then
      Items.Add(RsAttrArchive);
    if Attr and FILE_ATTRIBUTE_NORMAL = FILE_ATTRIBUTE_NORMAL then
      Items.Add(RsAttrNormal);
    if Attr and FILE_ATTRIBUTE_TEMPORARY = FILE_ATTRIBUTE_TEMPORARY then
      Items.Add(RsAttrTemporary);
    if Attr and FILE_ATTRIBUTE_COMPRESSED = FILE_ATTRIBUTE_COMPRESSED then
      Items.Add(RsAttrCompressed);
    if Attr and FILE_ATTRIBUTE_OFFLINE = FILE_ATTRIBUTE_OFFLINE then
      Items.Add(RsAttrOffline);
    if Attr and FILE_ATTRIBUTE_ENCRYPTED = FILE_ATTRIBUTE_ENCRYPTED then
      Items.Add(RsAttrEncrypted);
    if Attr and FILE_ATTRIBUTE_REPARSE_POINT = FILE_ATTRIBUTE_REPARSE_POINT then
      Items.Add(RsAttrReparsePoint);
    if Attr and FILE_ATTRIBUTE_SPARSE_FILE = FILE_ATTRIBUTE_SPARSE_FILE then
      Items.Add(RsAttrSparseFile);
  finally
    Items.EndUpdate;
  end;
end;

{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------

function GetFileInformation(const FileName: string; out FileInfo: TSearchRec): Boolean;
begin
  Result := FindFirst(FileName, faAnyFile, FileInfo) = 0;
  if Result then
    SysUtils.FindClose(FileInfo);
end;

function GetFileInformation(const FileName: string): TSearchRec;
begin
  if not GetFileInformation(FileName, Result) then
    RaiseLastOSError;
end;

//--------------------------------------------------------------------------------------------------

{$IFDEF UNIX}

{ TODO -cHelp : Author: Robert Rossmair }

function GetFileStatus(const FileName: string; out StatBuf: TStatBuf64;
  const ResolveSymLinks: Boolean): Integer;
begin
  if ResolveSymLinks then
    Result := stat64(PChar(FileName), StatBuf)
  else
    Result := lstat64(PChar(FileName), StatBuf);
end;

{$ENDIF UNIX}

//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

function GetFileLastWrite(const FileName: string): TFileTime;
begin
  Result := GetFileInformation(FileName).FindData.ftLastWriteTime;
end;

function GetFileLastWrite(const FileName: string; out LocalTime: TDateTime): Boolean;
var
  FileInfo: TSearchRec;
begin
  Result := GetFileInformation(FileName, FileInfo);
  if Result then
    LocalTime := FileTimeToLocalDateTime(GetFileInformation(FileName).FindData.ftLastWriteTime);
end;

{$ENDIF MSWINDOWS}

{$IFDEF UNIX}

function GetFileLastWrite(const FileName: string; out TimeStamp: Integer; ResolveSymLinks: Boolean): Boolean;
var
  Buf: TStatBuf64;
begin
  Result := GetFileStatus(FileName, Buf, ResolveSymLinks) = 0;
  if Result then
    TimeStamp := Buf.st_mtime
end;

function GetFileLastWrite(const FileName: string; out LocalTime: TDateTime; ResolveSymLinks: Boolean): Boolean;
var
  Buf: TStatBuf64;
begin
  Result := GetFileStatus(FileName, Buf, ResolveSymLinks) = 0;
  if Result then
    LocalTime := FileDateToDateTime(Buf.st_mtime);
end;

function GetFileLastWrite(const FileName: string; ResolveSymLinks: Boolean): Integer;
var
  Buf: TStatBuf64;
begin
  if GetFileStatus(FileName, Buf, ResolveSymLinks) = 0 then
    Result := Buf.st_mtime
  else
    Result := -1;
end;

{$ENDIF UNIX}

//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

function GetFileLastAccess(const FileName: string): TFileTime;
begin
  Result := GetFileInformation(FileName).FindData.ftLastAccessTime;
end;

function GetFileLastAccess(const FileName: string; out LocalTime: TDateTime): Boolean;
var
  FileInfo: TSearchRec;
begin
  Result := GetFileInformation(FileName, FileInfo);
  if Result then
    LocalTime := FileTimeToLocalDateTime(GetFileInformation(FileName).FindData.ftLastAccessTime);
end;

{$ENDIF MSWINDOWS}

{$IFDEF UNIX}

function GetFileLastAccess(const FileName: string; out TimeStamp: Integer; ResolveSymLinks: Boolean): Boolean;
var
  Buf: TStatBuf64;
begin
  Result := GetFileStatus(FileName, Buf, ResolveSymLinks) = 0;
  if Result then
    TimeStamp := Buf.st_atime
end;

function GetFileLastAccess(const FileName: string; out LocalTime: TDateTime; ResolveSymLinks: Boolean): Boolean;
var
  Buf: TStatBuf64;
begin
  Result := GetFileStatus(FileName, Buf, ResolveSymLinks) = 0;
  if Result then
    LocalTime := FileDateToDateTime(Buf.st_atime);
end;

function GetFileLastAccess(const FileName: string; ResolveSymLinks: Boolean): Integer;
var
  Buf: TStatBuf64;
begin
  if GetFileStatus(FileName, Buf, ResolveSymLinks) = 0 then
    Result := Buf.st_atime
  else
    Result := -1;
end;

{$ENDIF UNIX}

//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

function GetFileCreation(const FileName: string): TFileTime;
begin
  Result := GetFileInformation(FileName).FindData.ftCreationTime;
end;

function GetFileCreation(const FileName: string; out LocalTime: TDateTime): Boolean;
var
  FileInfo: TSearchRec;
begin
  Result := GetFileInformation(FileName, FileInfo);
  if Result then
    LocalTime := FileTimeToLocalDateTime(GetFileInformation(FileName).FindData.ftCreationTime);
end;

{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------

{$IFDEF UNIX}

function GetFileLastAttrChange(const FileName: string; out TimeStamp: Integer; ResolveSymLinks: Boolean): Boolean;
var
  Buf: TStatBuf64;
begin
  Result := GetFileStatus(FileName, Buf, ResolveSymLinks) = 0;
  if Result then
    TimeStamp := Buf.st_ctime
end;

function GetFileLastAttrChange(const FileName: string; out LocalTime: TDateTime; ResolveSymLinks: Boolean): Boolean;
var
  Buf: TStatBuf64;
begin
  Result := GetFileStatus(FileName, Buf, ResolveSymLinks) = 0;
  if Result then
    LocalTime := FileDateToDateTime(Buf.st_ctime);
end;

function GetFileLastAttrChange(const FileName: string; ResolveSymLinks: Boolean): Integer;
var
  Buf: TStatBuf64;
begin
  if GetFileStatus(FileName, Buf, ResolveSymLinks) = 0 then
    Result := Buf.st_ctime
  else
    Result := -1;
end;

{$ENDIF UNIX}

//--------------------------------------------------------------------------------------------------

function GetModulePath(const Module: HMODULE): string;
var
  L: Integer;
begin
  L := MAX_PATH + 1;
  SetLength(Result, L);
{$IFDEF MSWINDOWS}
  L := Windows.GetModuleFileName(Module, Pointer(Result), L);
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
  L := GetModuleFileName(Module, Pointer(Result), L);
{$ENDIF UNIX}
  SetLength(Result, L);
end;

//--------------------------------------------------------------------------------------------------

function GetSizeOfFile(const FileName: string): Int64;
{$IFDEF MSWINDOWS}
var
  Handle: THandle;
  FindData: TWin32FindData;
  Size: TULargeInteger;
begin
  Result := 0;
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    Size.LowPart := FindData.nFileSizeLow;
    Size.HighPart := FindData.nFileSizeHigh;
    Result := Size.QuadPart;
  end
  else
    RaiseLastOSError;
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
var
  Buf: TStatBuf64;
begin
  if GetFileStatus(FileName, Buf, False) <> 0 then
    RaiseLastOSError;
  Result := Buf.st_size
end;
{$ENDIF UNIX}

//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

function GetSizeOfFile(Handle: THandle): Int64;
var
  Size: TULargeInteger;
begin
  Size.LowPart := GetFileSize(Handle, @Size.HighPart);
  Result := Size.QuadPart;
end;

{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------

function GetSizeOfFile(const FileInfo: TSearchRec): Int64;
{$IFDEF MSWINDOWS}
begin
  with Int64Rec(Result) do
  begin
    Lo := FileInfo.FindData.nFileSizeLow;
    Hi := FileInfo.FindData.nFileSizeHigh;
  end;
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
var
  Buf: TStatBuf64;
begin
  // rr: Note that SysUtils.FindFirst/Next ignore files >= 2 GB under Linux,
  //     thus the following code is rather pointless at the moment of this writing.
  //     We apparently need to write our own set of Findxxx functions to overcome this limitation.
  if GetFileStatus(FileInfo.PathOnly + FileInfo.Name, Buf, True) <> 0 then
    Result := -1
  else
    Result := Buf.st_size
end;
{$ENDIF UNIX}

//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

{$IFDEF FPC}
{ TODO : Move this over to JclWin32 when JclWin32 gets overhauled. }
function GetFileAttributesEx(lpFileName: PChar;
  fInfoLevelId: TGetFileExInfoLevels; lpFileInformation: Pointer): BOOL; stdcall;
external kernel32 name 'GetFileAttributesExA';
{$ENDIF FPC}

function GetStandardFileInfo(const FileName: string): TWin32FileAttributeData;
var
  Handle: THandle;
  FileInfo: TByHandleFileInformation;
begin
  Assert(FileName <> '');
  { TODO : Use RTDL-Version of GetFileAttributesEx }
  if IsWin95 or IsWin95OSR2 or IsWinNT3 then
  begin
    Handle := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
    if Handle <> INVALID_HANDLE_VALUE then
    try
      if not GetFileInformationByHandle(Handle, FileInfo) then
        raise EJclFileUtilsError.CreateResRecFmt(@RsFileUtilsAttrUnavailable, [FileName]);
      Result.dwFileAttributes := FileInfo.dwFileAttributes;
      Result.ftCreationTime := FileInfo.ftCreationTime;
      Result.ftLastAccessTime := FileInfo.ftLastAccessTime;
      Result.ftLastWriteTime := FileInfo.ftLastWriteTime;
      Result.nFileSizeHigh := FileInfo.nFileSizeHigh;
      Result.nFileSizeLow := FileInfo.nFileSizeLow;
    finally
      CloseHandle(Handle);
    end
    else
      raise EJclFileUtilsError.CreateResRecFmt(@RsFileUtilsAttrUnavailable, [FileName]);
  end
  else
  begin
    if not GetFileAttributesEx(PChar(FileName), GetFileExInfoStandard, @Result) then
      raise EJclFileUtilsError.CreateResRecFmt(@RsFileUtilsAttrUnavailable, [FileName]);
  end;
end;

{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
function IsDirectory(const FileName: string): Boolean;
var
  R: DWORD;
begin
  R := GetFileAttributes(PChar(FileName));
  Result := (R <> DWORD(-1)) and ((R and FILE_ATTRIBUTE_DIRECTORY) <> 0);
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
function IsDirectory(const FileName: string; ResolveSymLinks: Boolean): Boolean;
var
  Buf: TStatBuf64;
begin
  Result := False;
  if GetFileStatus(FileName, Buf, ResolveSymLinks) = 0 then
    Result := S_ISDIR(Buf.st_mode);
end;
{$ENDIF UNIX}

//--------------------------------------------------------------------------------------------------

function IsRootDirectory(const CanonicFileName: string): Boolean;
{$IFDEF MSWINDOWS}
var
  I: Integer;
begin
  I := Pos(':\', CanonicFileName);
  Result := (I > 0) and (I + 1 = Length(CanonicFileName));
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
begin
  Result := CanonicFileName = PathSeparator;
end;
{$ENDIF UNIX}

//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

function LockVolume(const Volume: string; var Handle: THandle): Boolean;
var
  BytesReturned: DWORD;
begin
  Result := False;
  Handle := CreateFile(PChar('\\.\' + Volume), GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING,
    FILE_FLAG_NO_BUFFERING, 0);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Result := DeviceIoControl(Handle, FSCTL_LOCK_VOLUME, nil, 0, nil, 0,
      BytesReturned, nil);
    if not Result then
    begin
      CloseHandle(Handle);
      Handle := INVALID_HANDLE_VALUE;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function OpenVolume(const Drive: Char): THandle;
var
  VolumeName: array [0..6] of Char;
begin
  VolumeName := '\\.\A:';
  VolumeName[4] := Drive;
  Result := CreateFile(VolumeName, GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil, OPEN_EXISTING, 0, 0);
end;

{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------

type
  // indicates the file time to set, used by SetFileTimesHelper and SetDirTimesHelper
  TFileTimes = (ftLastAccess, ftLastWrite {$IFDEF MSWINDOWS}, ftCreation {$ENDIF});

{$IFDEF MSWINDOWS}
function SetFileTimesHelper(const FileName: string; const DateTime: TDateTime; Times: TFileTimes): Boolean;
var
  Handle: THandle;
  FileTime: TFileTime;
  SystemTime: TSystemTime;
begin
  Result := False;
  Handle := CreateFile(PChar(FileName), GENERIC_WRITE, FILE_SHARE_READ, nil,
    OPEN_EXISTING, 0, 0);
  if Handle <> INVALID_HANDLE_VALUE then
  try
    //SysUtils.DateTimeToSystemTime(DateTimeToLocalDateTime(DateTime), SystemTime);
    SysUtils.DateTimeToSystemTime(DateTime, SystemTime);
    if Windows.SystemTimeToFileTime(SystemTime, FileTime) then
    begin
      case Times of
        ftLastAccess:
          Result := SetFileTime(Handle, nil, @FileTime, nil);
        ftLastWrite:
          Result := SetFileTime(Handle, nil, nil, @FileTime);
        ftCreation:
          Result := SetFileTime(Handle, @FileTime, nil, nil);
      end;
    end;
  finally
    CloseHandle(Handle);
  end;
end;
{$ENDIF MSWINDOWS}

{$IFDEF UNIX}
function SetFileTimesHelper(const FileName: string; const DateTime: TDateTime; Times: TFileTimes): Boolean;
var
  FileTime: Integer;
  StatBuf: TStatBuf64;
  TimeBuf: utimbuf;
begin
  Result := False;
  FileTime := DateTimeToFileDate(DateTime);
  if GetFileStatus(FileName, StatBuf, False) = 0 then
  begin
    TimeBuf.actime := StatBuf.st_atime;
    TimeBuf.modtime := StatBuf.st_mtime;
    case Times of
      ftLastAccess:
        TimeBuf.actime := FileTime;
      ftLastWrite:
        TimeBuf.modtime := FileTime;
    end;
    Result := utime(PChar(FileName), @TimeBuf) = 0;
  end;
end;
{$ENDIF UNIX}

//--------------------------------------------------------------------------------------------------

function SetFileLastAccess(const FileName: string; const DateTime: TDateTime): Boolean;
begin
  Result := SetFileTimesHelper(FileName, DateTime, ftLastAccess);
end;

//--------------------------------------------------------------------------------------------------

function SetFileLastWrite(const FileName: string; const DateTime: TDateTime): Boolean;
begin
  Result := SetFileTimesHelper(FileName, DateTime, ftLastWrite);
end;

//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

function SetFileCreation(const FileName: string; const DateTime: TDateTime): Boolean;
begin
  Result := SetFileTimesHelper(FileName, DateTime, ftCreation);
end;

//--------------------------------------------------------------------------------------------------

// utility function for SetDirTimesHelper

function BackupPrivilegesEnabled: Boolean;
begin
  Result := IsPrivilegeEnabled(SE_BACKUP_NAME) and IsPrivilegeEnabled(SE_RESTORE_NAME);
end;

//--------------------------------------------------------------------------------------------------

function SetDirTimesHelper(const DirName: string; const DateTime: TDateTime;
  Times: TFileTimes): Boolean;
var
  Handle: THandle;
  FileTime: TFileTime;
  SystemTime: TSystemTime;
begin
  Result := False;
  if IsDirectory(DirName) and BackupPrivilegesEnabled then
  begin
    Handle := CreateFile(PChar(DirName), GENERIC_WRITE, FILE_SHARE_READ, nil,
      OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
    if Handle <> INVALID_HANDLE_VALUE then
    try
      SysUtils.DateTimeToSystemTime(DateTime, SystemTime);
      Windows.SystemTimeToFileTime(SystemTime, FileTime);
      case Times of
        ftLastAccess:
          Result := SetFileTime(Handle, nil, @FileTime, nil);
        ftLastWrite:
          Result := SetFileTime(Handle, nil, nil, @FileTime);
        ftCreation:
          Result := SetFileTime(Handle, @FileTime, nil, nil);
      end;
    finally
      CloseHandle(Handle);
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function SetDirLastWrite(const DirName: string; const DateTime: TDateTime): Boolean;
begin
  Result := SetDirTimesHelper(DirName, DateTime, ftLastWrite);
end;

//--------------------------------------------------------------------------------------------------

function SetDirLastAccess(const DirName: string; const DateTime: TDateTime): Boolean;
begin
  Result := SetDirTimesHelper(DirName, DateTime, ftLastAccess);
end;

//--------------------------------------------------------------------------------------------------

function SetDirCreation(const DirName: string; const DateTime: TDateTime): Boolean;
begin
  Result := SetDirTimesHelper(DirName, DateTime, ftCreation);
end;

//--------------------------------------------------------------------------------------------------

procedure ShredFile(const FileName: string; Times: Integer);
const
  BUFSIZE   = 4096;
  ODD_FILL  = $C1;
  EVEN_FILL = $3E;
var
  Fs: TFileStream;
  Size: Integer;
  N: Integer;
  ContentPtr: Pointer;
begin
  Size := FileGetSize(FileName);
  if Size > 0 then
  begin
    if Times < 0 then
      Times := 2
    else
      Times := Times * 2;
    ContentPtr := nil;
    Fs := TFileStream.Create(FileName, fmOpenReadWrite);
    try
      GetMem(ContentPtr, BUFSIZE);
      while Times > 0 do
      begin
        if Times mod 2 = 0 then
          FillMemory(ContentPtr, BUFSIZE, EVEN_FILL)
        else
          FillMemory(ContentPtr, BUFSIZE, ODD_FILL);
        Fs.Seek(0, soFromBeginning);
        N := Size div BUFSIZE;
        while N > 0 do
        begin
          Fs.Write(ContentPtr^, BUFSIZE);
          Dec(N);
        end;
        N := Size mod BUFSIZE;
        if N > 0 then
          Fs.Write(ContentPtr^, N);
        FlushFileBuffers(Fs.Handle);
        Dec(Times);
      end;
    finally
      if ContentPtr <> nil then
        FreeMem(ContentPtr, Size);
      Fs.Free;
      DeleteFile(FileName);
    end;
  end
  else
    DeleteFile(FileName);
end;

//--------------------------------------------------------------------------------------------------

function UnlockVolume(var Handle: THandle): Boolean;
var
  BytesReturned: DWORD;
begin
  Result := False;
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Result := DeviceIoControl(Handle, FSCTL_UNLOCK_VOLUME, nil, 0, nil, 0,
      BytesReturned, nil);
    if Result then
    begin
      CloseHandle(Handle);
      Handle := INVALID_HANDLE_VALUE;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

{$IFNDEF DROP_OBSOLETE_CODE}

function Win32DeleteFile(const FileName: string; MoveToRecycleBin: Boolean): Boolean;
begin
  Result := FileDelete(FileName, MoveToRecycleBin);
end;

//--------------------------------------------------------------------------------------------------

function Win32MoveFileReplaceExisting(const SrcFileName, DstFileName: string): Boolean;
begin
  Result := FileMove(SrcFilename, DstFilename, True);
end;

//--------------------------------------------------------------------------------------------------

function Win32BackupFile(const FileName: string; Move: Boolean): Boolean;
begin
  Result := FileBackup(FileName, Move);
end;

//--------------------------------------------------------------------------------------------------

function Win32RestoreFile(const FileName: string): Boolean;
begin
  Result := FileRestore(FileName);
end;

{$ENDIF ~DROP_OBSOLETE_CODE}
{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------

{$IFDEF UNIX}

function CreateSymbolicLink(const Name, Target: string): Boolean;
begin
  Result := symlink(PChar(Target), PChar(Name)) = 0;
end;

//--------------------------------------------------------------------------------------------------

function SymbolicLinkTarget(const Name: string): string;
var
  N, BufLen: Integer;
begin
  BufLen := 128;
  repeat
    Inc(BufLen, BufLen);
    SetLength(Result, BufLen);
    N := readlink(PChar(Name), PChar(Result), BufLen);
    if N < 0 then // Error
    begin
      Result := '';
      Exit;
    end;
  until N < BufLen;
  SetLength(Result, N);
end;

{$ENDIF UNIX}

//==================================================================================================
// File Version info routines
//==================================================================================================

{$IFDEF MSWINDOWS}

const
  VerKeyNames: array [1..12] of string[17] =
   ('Comments',
    'CompanyName',
    'FileDescription',
    'FileVersion',
    'InternalName',
    'LegalCopyright',
    'LegalTradeMarks',
    'OriginalFilename',
    'ProductName',
    'ProductVersion',
    'SpecialBuild',
    'PrivateBuild');

//--------------------------------------------------------------------------------------------------

function OSIdentToString(const OSIdent: DWORD): string;
begin
  case OSIdent of
    VOS_UNKNOWN:
      Result := RsVosUnknown;
    VOS_DOS:
      Result := RsVosDos;
    VOS_OS216:
      Result := RsVosOS216;
    VOS_OS232:
      Result := RsVosOS232;
    VOS_NT:
      Result := RsVosNT;
    VOS__WINDOWS16:
      Result := RsVosWindows16;
    VOS__PM16:
      Result := RsVosPM16;
    VOS__PM32:
      Result := RsVosPM32;
    VOS__WINDOWS32:
      Result := RsVosWindows32;
    VOS_DOS_WINDOWS16:
      Result := RsVosDosWindows16;
    VOS_DOS_WINDOWS32:
      Result := RsVosDosWindows32;
    VOS_OS216_PM16:
      Result := RsVosOS216PM16;
    VOS_OS232_PM32:
      Result := RsVosOS232PM32;
    VOS_NT_WINDOWS32:
      Result := RsVosNTWindows32;
  else
    Result := RsVosUnknown;
  end;
  if Result <> RsVosUnknown then
    Result := RsVosDesignedFor + Result;
end;

//--------------------------------------------------------------------------------------------------

function OSFileTypeToString(const OSFileType: DWORD; const OSFileSubType: DWORD): string;
begin
  case OSFileType of
    VFT_UNKNOWN:
      Result := RsVftUnknown;
    VFT_APP:
      Result := RsVftApp;
    VFT_DLL:
      Result := RsVftDll;
    VFT_DRV:
      begin
        case OSFileSubType of
          VFT2_DRV_PRINTER:
            Result := RsVft2DrvPRINTER;
          VFT2_DRV_KEYBOARD:
            Result := RsVft2DrvKEYBOARD;
          VFT2_DRV_LANGUAGE:
            Result := RsVft2DrvLANGUAGE;
          VFT2_DRV_DISPLAY:
            Result := RsVft2DrvDISPLAY;
          VFT2_DRV_MOUSE:
            Result := RsVft2DrvMOUSE;
          VFT2_DRV_NETWORK:
            Result := RsVft2DrvNETWORK;
          VFT2_DRV_SYSTEM:
            Result := RsVft2DrvSYSTEM;
          VFT2_DRV_INSTALLABLE:
            Result := RsVft2DrvINSTALLABLE;
          VFT2_DRV_SOUND:
            Result := RsVft2DrvSOUND;
          VFT2_DRV_COMM:
            Result := RsVft2DrvCOMM;
        else
          Result := '';
        end;
        Result := Result + ' ' + RsVftDrv;
      end;
    VFT_FONT:
      begin
        case OSFileSubType of
          VFT2_FONT_RASTER:
            Result := RsVft2FontRASTER;
          VFT2_FONT_VECTOR:
            Result := RsVft2FontVECTOR;
          VFT2_FONT_TRUETYPE:
            Result := RsVft2FontTRUETYPE;
        else
          Result := '';
        end;
        Result := Result + ' ' + RsVftFont;
      end;
    VFT_VXD:
      Result := RsVftVxd;
    VFT_STATIC_LIB:
      Result := RsVftStaticLib;
  else
    Result := '';
  end;
  Result := TrimLeft(Result);
end;

//--------------------------------------------------------------------------------------------------

function VersionResourceAvailable(const FileName: string): Boolean;
var
  Size: DWORD;
  Handle: THandle;
  Buffer: string;
begin
  Result := False;
  Size := GetFileVersionInfoSize(PChar(FileName), Handle);
  if Size > 0 then
  begin
    SetLength(Buffer, Size);
    Result := GetFileVersionInfo(PChar(FileName), Handle, Size, PChar(Buffer));
  end;
end;

{$ENDIF MSWINDOWS}

//==================================================================================================
// Version Info formatting
//==================================================================================================

function FormatVersionString(const HiV, LoV: Word): string;
begin
  Result := Format('%u.%.2u', [HiV, LoV]);
end;

//--------------------------------------------------------------------------------------------------

function FormatVersionString(const Major, Minor, Build, Revision: Word): string;
begin
  Result := Format('%u.%u.%u.%u', [Major, Minor, Build, Revision]);
end;

//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

function FormatVersionString(const FixedInfo: TVSFixedFileInfo; VersionFormat: TFileVersionFormat): string;
begin
  with FixedInfo do
    case VersionFormat of
      vfMajorMinor:
        Result := Format('%u.%u', [HiWord(dwFileVersionMS), LoWord(dwFileVersionMS)]);
      vfFull:
        Result := Format('%u.%u.%u.%u', [HiWord(dwFileVersionMS), LoWord(dwFileVersionMS),
          HiWord(dwFileVersionLS), LoWord(dwFileVersionLS)]);
    end;
end;

//==================================================================================================
// Version Info extracting
//==================================================================================================

procedure VersionExtractFileInfo(const FixedInfo: TVSFixedFileInfo; var Major, Minor, Build, Revision: Word);
begin
  Major := HiWord(FixedInfo.dwFileVersionMS);
  Minor := LoWord(FixedInfo.dwFileVersionMS);
  Build := HiWord(FixedInfo.dwFileVersionLS);
  Revision := LoWord(FixedInfo.dwFileVersionLS);
end;

//--------------------------------------------------------------------------------------------------

procedure VersionExtractProductInfo(const FixedInfo: TVSFixedFileInfo; var Major, Minor, Build, Revision: Word);
begin
  Major := HiWord(FixedInfo.dwProductVersionMS);
  Minor := LoWord(FixedInfo.dwProductVersionMS);
  Build := HiWord(FixedInfo.dwProductVersionLS);
  Revision := LoWord(FixedInfo.dwProductVersionLS);
end;

//==================================================================================================
// Fixed Version Info routines
//==================================================================================================

function VersionFixedFileInfo(const FileName: string; var FixedInfo: TVSFixedFileInfo): Boolean;
var
  Size, FixInfoLen: DWORD;
  Handle: THandle;
  Buffer: string;
  FixInfoBuf: PVSFixedFileInfo;
begin
  Result := False;
  Size := GetFileVersionInfoSize(PChar(FileName), Handle);
  if Size > 0 then
  begin
    SetLength(Buffer, Size);
    if GetFileVersionInfo(PChar(FileName), Handle, Size, Pointer(Buffer)) and
      VerQueryValue(Pointer(Buffer), '\', Pointer(FixInfoBuf), FixInfoLen) and
      (FixInfoLen = SizeOf(TVSFixedFileInfo)) then
    begin
      Result := True;
      FixedInfo := FixInfoBuf^;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function VersionFixedFileInfoString(const FileName: string; VersionFormat: TFileVersionFormat;
  const NotAvailableText: string): string;
var
  FixedInfo: TVSFixedFileInfo;
begin
  if VersionFixedFileInfo(FileName, FixedInfo) then
    Result := FormatVersionString(FixedInfo, VersionFormat)
  else
    Result := NotAvailableText;
end;

//==================================================================================================
// TJclFileVersionInfo
//==================================================================================================

constructor TJclFileVersionInfo.Attach(VersionInfoData: Pointer; Size: Integer);
begin
  SetString(FBuffer, PChar(VersionInfoData), Size);
  ExtractData;
end;

//--------------------------------------------------------------------------------------------------

constructor TJclFileVersionInfo.Create(const FileName: string);
var
  Handle: THandle;
  Size: DWORD;
begin
  Size := GetFileVersionInfoSize(PChar(FileName), Handle);
  if Size = 0 then
    raise EJclFileVersionInfoError.CreateResRec(@RsFileUtilsNoVersionInfo);
  SetLength(FBuffer, Size);
  Win32Check(GetFileVersionInfo(PChar(FileName), Handle, Size, PChar(FBuffer)));
  ExtractData;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclFileVersionInfo.Destroy;
begin
  FreeAndNil(FItemList);
  FreeAndNil(FItems);
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileVersionInfo.CheckLanguageIndex(Value: Integer);
begin
  if (Value < 0) or (Value >= LanguageCount) then
    raise EJclFileVersionInfoError.CreateResRec(@RsFileUtilsLanguageIndex);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileVersionInfo.CreateItemsForLanguage;
var
  I: Integer;
begin
  Items.Clear;
  for I := 0 to FItemList.Count - 1 do
    if Integer(FItemList.Objects[I]) = FLanguageIndex then
      Items.AddObject(FItemList[I], Pointer(FLanguages[FLanguageIndex].Pair));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileVersionInfo.ExtractData;
var
  Data, EndOfData: PChar;
  Len, ValueLen, DataType: Word;
  HeaderSize: Integer;
  Key: string;
  Error, IsUnicode: Boolean;

  procedure Padding(var DataPtr: PChar);
  begin
    while DWORD(DataPtr) and 3 <> 0 do
      Inc(DataPtr);
  end;

  procedure GetHeader;
  var
    P: PChar;
    TempKey: PWideChar;
  begin
    P := Data;
    Len := PWord(P)^;
    if Len = 0 then
    begin
      Error := True;
      Exit;
    end;
    Inc(P, SizeOf(Word));
    ValueLen := PWord(P)^;
    Inc(P, SizeOf(Word));
    if IsUnicode then
    begin
      DataType := PWord(P)^;
      Inc(P, SizeOf(Word));
      TempKey := PWideChar(P);
      Inc(P, (lstrlenW(TempKey) + 1) * SizeOf(WideChar)); // length + #0#0
      Key := TempKey;
    end
    else
    begin
      DataType := 1;
      Key := PAnsiChar(P);
      Inc(P, lstrlenA(P) + 1);
    end;
    Padding(P);
    HeaderSize := P - Data;
    Data := P;
  end;

  procedure FixKeyValue;
  const
    HexNumberCPrefix = '0x';
  var
    I: Integer;
  begin // GAPI32.DLL version 5.5.2803.1 contanins '04050x04E2' value
    repeat
      I := Pos(HexNumberCPrefix, Key);
      if I > 0 then
        Delete(Key, I, Length(HexNumberCPrefix));
    until I = 0;
    I := 1;
    while I <= Length(Key) do
      if Key[I] in AnsiHexDigits then
        Inc(I)
      else
        Delete(Key, I, 1);
  end;

  procedure ProcessStringInfo(Size: Integer);
  var
    EndPtr, EndStringPtr: PChar;
    LangIndex: Integer;
    LangIdRec: TLangIdRec;
    Value: string;
  begin
    EndPtr := Data + Size;
    LangIndex := 0;
    while not Error and (Data < EndPtr) do
    begin
      GetHeader; // StringTable
      FixKeyValue;
      if (ValueLen <> 0) or (Length(Key) <> 8) then
      begin
        Error := True;
        Break;
      end;
      Padding(Data);
      LangIdRec.LangId := StrToIntDef('$' + Copy(Key, 1, 4), 0);
      LangIdRec.CodePage := StrToIntDef('$' + Copy(Key, 5, 4), 0);
      SetLength(FLanguages, LangIndex + 1);
      FLanguages[LangIndex] := LangIdRec;
      EndStringPtr := Data + Len - HeaderSize;
      while not Error and (Data < EndStringPtr) do
      begin
        GetHeader; // string
        case DataType of
          0:
            if ValueLen in [1..4] then
              Value := Format('$%.*x', [ValueLen * 2, PInteger(Data)^])
            else
              Value := '';
          1:
            if ValueLen = 0 then
              Value := ''
            else
            if IsUnicode then
            begin
              Value := WideCharLenToString(PWideChar(Data), ValueLen);
              StrResetLength(Value);
            end
            else
              Value := PAnsiChar(Data);
        else
          Error := True;
          Break;
        end;
        Inc(Data, Len - HeaderSize);
        Padding(Data); // String.Padding
        FItemList.AddObject(Format('%s=%s', [Key, Value]), Pointer(LangIndex));
      end;
      Inc(LangIndex);
    end;
  end;

  procedure ProcessVarInfo(Size: Integer);
  var
    TranslationIndex: Integer;
  begin
    GetHeader; // Var
    if SameText(Key, 'Translation') then
    begin
      SetLength(FTranslations, ValueLen div SizeOf(TLangIdRec));
      for TranslationIndex := 0 to Length(FTranslations) - 1 do
      begin
        FTranslations[TranslationIndex] := PLangIdRec(Data)^;
        Inc(Data, SizeOf(TLangIdRec));
      end;
    end;
  end;

begin
  FItemList := TStringList.Create;
  FItems := TStringList.Create;
  Data := Pointer(FBuffer);
  Assert(DWORD(Data) mod 4 = 0);
  IsUnicode := (PWord(Data + 4)^ in [0, 1]);
  Error := True;
  GetHeader;
  EndOfData := Data + Len - HeaderSize;
  if SameText(Key, 'VS_VERSION_INFO') and (ValueLen = SizeOf(TVSFixedFileInfo)) then
  begin
    FFixedInfo := PVSFixedFileInfo(Data);
    Error := FFixedInfo.dwSignature <> $FEEF04BD;
    Inc(Data, ValueLen); // VS_FIXEDFILEINFO
    Padding(Data);       // VS_VERSIONINFO.Padding2
    while not Error and (Data < EndOfData) do
    begin
      GetHeader;
      Inc(Data, ValueLen); // some files (VREDIR.VXD 4.00.1111) has non zero value of ValueLen
      Dec(Len, HeaderSize + ValueLen);
      if SameText(Key, 'StringFileInfo') then
        ProcessStringInfo(Len)
      else
      if SameText(Key, 'VarFileInfo') then
        ProcessVarInfo(Len)
      else
        Break;
    end;
    ExtractFlags;
    CreateItemsForLanguage;
  end;
  if Error then
    raise EJclFileVersionInfoError.CreateResRec(@RsFileUtilsNoVersionInfo);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileVersionInfo.ExtractFlags;
var
  Masked: DWORD;
begin
  FFileFlags := [];
  Masked := FFixedInfo^.dwFileFlags and FFixedInfo^.dwFileFlagsMask;
  if (Masked and VS_FF_DEBUG) <> 0 then
    Include(FFileFlags, ffDebug);
  if (Masked and VS_FF_INFOINFERRED) <> 0 then
    Include(FFileFlags, ffInfoInferred);
  if (Masked and VS_FF_PATCHED) <> 0 then
    Include(FFileFlags, ffPatched);
  if (Masked and VS_FF_PRERELEASE) <> 0 then
    Include(FFileFlags, ffPreRelease);
  if (Masked and VS_FF_PRIVATEBUILD) <> 0 then
    Include(FFileFlags, ffPrivateBuild);
  if (Masked and VS_FF_SPECIALBUILD) <> 0 then
    Include(FFileFlags, ffSpecialBuild);
end;

//--------------------------------------------------------------------------------------------------

function TJclFileVersionInfo.GetBinFileVersion: string;
begin
  with FFixedInfo^ do
    Result := Format('%u.%u.%u.%u', [HiWord(dwFileVersionMS),
      LoWord(dwFileVersionMS), HiWord(dwFileVersionLS), LoWord(dwFileVersionLS)]);
end;

//--------------------------------------------------------------------------------------------------

function TJclFileVersionInfo.GetBinProductVersion: string;
begin
  with FFixedInfo^ do
    Result := Format('%u.%u.%u.%u', [HiWord(dwProductVersionMS),
      LoWord(dwProductVersionMS), HiWord(dwProductVersionLS),
      LoWord(dwProductVersionLS)]);
end;

//--------------------------------------------------------------------------------------------------

function TJclFileVersionInfo.GetFileOS: DWORD;
begin
  Result := FFixedInfo^.dwFileOS;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileVersionInfo.GetFileSubType: DWORD;
begin
  Result := FFixedInfo^.dwFileSubtype;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileVersionInfo.GetFileType: DWORD;
begin
  Result := FFixedInfo^.dwFileType;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileVersionInfo.GetFixedInfo: TVSFixedFileInfo;
begin
  Result := FFixedInfo^;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileVersionInfo.GetItems: TStrings;
begin
  Result := FItems;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileVersionInfo.GetLanguageCount: Integer;
begin
  Result := Length(FLanguages);
end;

//--------------------------------------------------------------------------------------------------

function TJclFileVersionInfo.GetLanguageIds(Index: Integer): string;
begin
  CheckLanguageIndex(Index);
  Result := VersionLanguageId(FLanguages[Index]);
end;

//--------------------------------------------------------------------------------------------------

function TJclFileVersionInfo.GetLanguages(Index: Integer): TLangIdRec;
begin
  CheckLanguageIndex(Index);
  Result := FLanguages[Index];
end;

//--------------------------------------------------------------------------------------------------

function TJclFileVersionInfo.GetLanguageNames(Index: Integer): string;
begin
  CheckLanguageIndex(Index);
  Result := VersionLanguageName(FLanguages[Index].LangId);
end;

//--------------------------------------------------------------------------------------------------

function TJclFileVersionInfo.GetTranslationCount: Integer;
begin
  Result := Length(FTranslations);
end;

//--------------------------------------------------------------------------------------------------

function TJclFileVersionInfo.GetTranslations(Index: Integer): TLangIdRec;
begin
  Result := FTranslations[Index];
end;

//--------------------------------------------------------------------------------------------------

function TJclFileVersionInfo.GetVersionKeyValue(Index: Integer): string;
begin
  Result := Items.Values[VerKeyNames[Index]];
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileVersionInfo.SetLanguageIndex(const Value: Integer);
begin
  CheckLanguageIndex(Value);
  if FLanguageIndex <> Value then
  begin
    FLanguageIndex := Value;
    CreateItemsForLanguage;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileVersionInfo.TranslationMatchesLanguages(Exact: Boolean): Boolean;
var
  TransIndex, LangIndex: Integer;
  TranslationPair: DWORD;
begin
  Result := (LanguageCount = TranslationCount) or (not Exact and (TranslationCount > 0));
  if Result then
    for TransIndex := 0 to TranslationCount - 1 do
    begin
      TranslationPair := FTranslations[TransIndex].Pair;
      LangIndex := LanguageCount - 1;
      while (LangIndex >= 0) and (TranslationPair <> FLanguages[LangIndex].Pair) do
        Dec(LangIndex);
      if LangIndex < 0 then
      begin
        Result := False;
        Break;
      end;
    end;
end;

//--------------------------------------------------------------------------------------------------

class function TJclFileVersionInfo.VersionLanguageId(const LangIdRec: TLangIdRec): string;
begin
  with LangIdRec do
    Result := Format('%.4x%.4x', [LangId, CodePage]);
end;

//--------------------------------------------------------------------------------------------------

class function TJclFileVersionInfo.VersionLanguageName(const LangId: Word): string;
var
  R: DWORD;
begin
  SetLength(Result, MAX_PATH);
  R := VerLanguageName(LangId, PChar(Result), MAX_PATH);
  SetLength(Result, R);
end;

{$ENDIF MSWINDOWS}

//==================================================================================================
// TJclFileMaskComparator
//==================================================================================================

constructor TJclFileMaskComparator.Create;
begin
  inherited Create;
  FSeparator := ';';
end;

//--------------------------------------------------------------------------------------------------

function TJclFileMaskComparator.Compare(const NameExt: string): Boolean;
var
  I: Integer;
  NamePart, ExtPart: string;
  NameWild, ExtWild: Boolean;
begin
  Result := False;
  I := StrLastPos('.', NameExt);
  if I = 0 then
  begin
    NamePart := NameExt;
    ExtPart := '';
  end
  else
  begin
    NamePart := Copy(NameExt, 1, I - 1);
    ExtPart := Copy(NameExt, I + 1, Length(NameExt));
  end;
  for I := 0 to Length(FNames) - 1 do
  begin
    NameWild := FWildChars[I] and 1 = 1;
    ExtWild := FWildChars[I] and 2 = 2;
    if ((not NameWild and StrSame(FNames[I], NamePart)) or
      (NameWild and (StrMatches(FNames[I], NamePart, 1)))) and
      ((not ExtWild and StrSame(FExts[I], ExtPart)) or
      (ExtWild and (StrMatches(FExts[I], ExtPart, 1)))) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileMaskComparator.CreateMultiMasks;
const
  WildChars = ['*', '?'];
var
  List: TStringList;
  I, N: Integer;
  NS, ES: string;
begin
  FExts := nil;
  FNames := nil;
  FWildChars := nil;
  List := TStringList.Create;
  try
    StrToStrings(FFileMask, FSeparator, List);
    SetLength(FExts, List.Count);
    SetLength(FNames, List.Count);
    SetLength(FWildChars, List.Count);
    for I := 0 to List.Count - 1 do
    begin
      N := StrLastPos('.', List[I]);
      if N = 0 then
      begin
        NS := List[I];
        ES := '';
      end
      else
      begin
        NS := Copy(List[I], 1, N - 1);
        ES := Copy(List[I], N + 1, 255);
      end;
      FNames[I] := NS;
      FExts[I] := ES;
      N := 0;
      if StrContainsChars(NS, WildChars, False) then
        N := N or 1;
      if StrContainsChars(ES, WildChars, False) then
        N := N or 2;
      FWildChars[I] := N;
    end;
  finally
    List.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileMaskComparator.GetCount: Integer;
begin
  Result := Length(FWildChars);
end;

//--------------------------------------------------------------------------------------------------

function TJclFileMaskComparator.GetExts(Index: Integer): string;
begin
  Result := FExts[Index];
end;

//--------------------------------------------------------------------------------------------------

function TJclFileMaskComparator.GetMasks(Index: Integer): string;
begin
  Result := FNames[Index] + '.' + FExts[Index];
end;

//--------------------------------------------------------------------------------------------------

function TJclFileMaskComparator.GetNames(Index: Integer): string;
begin
  Result := FNames[Index];
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileMaskComparator.SetFileMask(const Value: string);
begin
  FFileMask := Value;
  CreateMultiMasks;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileMaskComparator.SetSeparator(const Value: Char);
begin
  if FSeparator <> Value then
  begin
    FSeparator := Value;
    CreateMultiMasks;
  end;
end;

//--------------------------------------------------------------------------------------------------

function AdvBuildFileList(const Path: string; const Attr: Integer; const Files: TStrings;
  const AttributeMatch: TJclAttributeMatch; const Options: TFileListOptions;
  const SubfoldersMask: string; const FileMatchFunc: TFileMatchFunc): Boolean;
var
  FileMask: string;
  RootDir: string;
  Folders: TStringList;
  CurrentItem: Integer;
  Counter: Integer;
  FindAttr: Integer;

  procedure BuildFolderList;
  var
    FindInfo: TSearchRec;
    Rslt: Integer;
  begin
    Counter := Folders.Count - 1;
    CurrentItem := 0;

    while CurrentItem <= Counter do
    begin
      // searching for subfolders
      Rslt := FindFirst(Folders[CurrentItem] + '*.*', faDirectory, FindInfo);
      try
        while Rslt = 0 do
        begin
          if (FindInfo.Name <> '.') and (FindInfo.Name <> '..') and
            (FindInfo.Attr and faDirectory = faDirectory) then
            Folders.Add(Folders[CurrentItem] + FindInfo.Name + PathSeparator);

          Rslt := FindNext(FindInfo);
        end;
      finally
        FindClose(FindInfo);
      end;
      Counter := Folders.Count - 1;
      Inc(CurrentItem);
    end;
  end;

  procedure FillFileList(CurrentCounter: Integer);
  var
    FindInfo: TSearchRec;
    Rslt: Integer;
    CurrentFolder: string;
    Matches: Boolean;
  begin
    CurrentFolder := Folders[CurrentCounter];

    Rslt := FindFirst(CurrentFolder + FileMask, FindAttr, FindInfo);

    try
      while Rslt = 0 do
      begin
         Matches := False;

         case AttributeMatch of
           amAny:
             Matches := True;
           amExact:
             Matches := Attr = FindInfo.Attr;
           amSubSetOf:
             Matches := (Attr and FindInfo.Attr) = Attr;
           amSuperSetOf:
             Matches := (Attr and FindInfo.Attr) = FindInfo.Attr;
           amCustom:
             if Assigned(FileMatchFunc) then
               Matches := FileMatchFunc(Attr,  FindInfo);
         end;

         if Matches then
          if flFullNames in Options then
            Files.Add(CurrentFolder + FindInfo.Name)
          else
            Files.Add(FindInfo.Name);

        Rslt := FindNext(FindInfo);
      end;
    finally
      FindClose(FindInfo);
    end;
  end;

begin
  Assert(Assigned(Files));
  FileMask := ExtractFileName(Path);
  RootDir := ExtractFilePath(Path);

  Folders := TStringList.Create;
  Files.BeginUpdate;
  try
    Folders.Add(RootDir);

    case AttributeMatch of
      amExact, amSuperSetOf:
        FindAttr := Attr;
    else
      FindAttr := faAnyFile;
    end;

    // here's the recursive search for nested folders

    if flRecursive in Options then
      BuildFolderList;

    for Counter := 0 to Folders.Count - 1 do
    begin
      if (((flMaskedSubfolders in Options) and (StrMatches(SubfoldersMask,
        Folders[Counter], 1))) or (not (flMaskedSubfolders in Options))) then
          FillFileList(Counter);
    end;
  finally
    Folders.Free;
    Files.EndUpdate;
  end;
  Result := True;
end;

//--------------------------------------------------------------------------------------------------

function VerifyFileAttributeMask(var RejectedAttributes, RequiredAttributes: Integer): Boolean;
begin
  if RequiredAttributes and faNormalFile <> 0 then
    RejectedAttributes := not faNormalFile or RejectedAttributes;
  Result := RequiredAttributes and RejectedAttributes = 0;
end;

//--------------------------------------------------------------------------------------------------

function AttributeMatch(FileAttributes, RejectedAttr, RequiredAttr: Integer): Boolean;
begin
  if FileAttributes = 0 then
    FileAttributes := faNormalFile;
  {$IFDEF MSWINDOWS}
  RequiredAttr := RequiredAttr and not faUnixSpecific;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  RequiredAttr := RequiredAttr and not faWindowsSpecific;
  {$ENDIF UNIX}
  Result := (FileAttributes and RejectedAttr = 0)
    and (FileAttributes and RequiredAttr = RequiredAttr);
end;

//--------------------------------------------------------------------------------------------------

function IsFileAttributeMatch(FileAttributes, RejectedAttributes,
  RequiredAttributes: Integer): Boolean;
begin
  VerifyFileAttributeMask(RejectedAttributes, RequiredAttributes);
  Result := AttributeMatch(FileAttributes, RejectedAttributes, RequiredAttributes);
end;

//--------------------------------------------------------------------------------------------------

function FileAttributesStr(const FileInfo: TSearchRec): string;
{$IFDEF MSWINDOWS}
const
  SAllAttrSet = 'rahs'; // readonly, archive, hidden, system
  Attributes: array [1..4] of Integer =
    (faReadOnly, faArchive, faHidden, faSysFile);
var
  I: Integer;
begin
  Result := SAllAttrSet;
  for I := Low(Attributes) to High(Attributes) do
    if (FileInfo.Attr and Attributes[I]) = 0 then
      Result[I] := '-';
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
const
  SAllAttrSet = 'drwxrwxrwx';
var
  I: Integer;
  Flag: Cardinal;
begin
  Result := SAllAttrSet;
  if FileInfo.Attr and faDirectory = 0 then
    Result[1] := '-'; // no directory
  Flag := 1 shl 8;
  for I := 2 to 10 do
  begin
    if FileInfo.Mode and Flag = 0 then
      Result[I] := '-';
    Flag := Flag shr 1;
  end;
end;
{$ENDIF UNIX}

//--------------------------------------------------------------------------------------------------

function IsFileNameMatch(FileName: string; const Mask: string;
  const CaseSensitive: Boolean): Boolean;
begin
  Result := True;
  {$IFDEF MSWINDOWS}
  if (Mask = '') or (Mask = '*') or (Mask = '*.*') then
    Exit;
  if Pos('.', FileName) = 0 then
    FileName := FileName + '.';  // file names w/o extension match '*.'
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  if (Mask = '') or (Mask = '*') then
    Exit;
  {$ENDIF UNIX}
  if CaseSensitive then
    Result := StrMatches(Mask, FileName)
  else
    Result := StrMatches(AnsiUpperCase(Mask), AnsiUpperCase(FileName));
end;

//--------------------------------------------------------------------------------------------------

// author: Robert Rossmair

function CanonicalizedSearchPath(const Directory: string): string;
begin
  Result := PathCanonicalize(Directory);
  {$IFDEF MSWINDOWS}
  // avoid changing "X:" (current directory on drive X:) into "X:\" (root dir.)
  if Result[Length(Result)] <> ':' then
  {$ENDIF MSWINDOWS}
    Result := PathAddSeparator(Result);
  // strip leading "./" resp. ".\"
  if Pos('.' + PathSeparator, Result) = 1 then
    Result := Copy(Result, 3, Length(Result) - 2);
end;

//--------------------------------------------------------------------------------------------------

procedure EnumFiles(const Path: string; HandleFile: TFileHandlerEx;
  RejectedAttributes: Integer = faRejectedByDefault; RequiredAttributes: Integer = 0;
  Abort: PBoolean = nil);
var
  Directory: string;
  FileInfo: TSearchRec;
  Attr: Integer;
  Found: Boolean;
begin
  Assert(Assigned(HandleFile));
  Assert(VerifyFileAttributeMask(RejectedAttributes, RequiredAttributes),
    RsFileSearchAttrInconsistency);

  Directory := ExtractFilePath(Path);

  Attr := faAnyFile and not RejectedAttributes;

  Found := SysUtils.FindFirst(Path, Attr, FileInfo) = 0;
  try
    while Found do
    begin
      if (Abort <> nil) and Abort^ then
        Exit;
      if AttributeMatch(FileInfo.Attr, RejectedAttributes, RequiredAttributes) then
        if ((FileInfo.Attr and faDirectory = 0)
        or ((FileInfo.Name <> '.') and (FileInfo.Name <> '..'))) then
          HandleFile(Directory, FileInfo);
      Found := FindNext(FileInfo) = 0;
    end;
  finally
    FindClose(FileInfo);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure EnumDirectories(const Root: string; const HandleDirectory: TFileHandler;
  const IncludeHiddenDirectories: Boolean; const SubDirectoriesMask: string;
  Abort: PBoolean {$IFDEF UNIX}; ResolveSymLinks: Boolean {$ENDIF});
var
  RootDir: string;
  Attr: Integer;

  procedure Process(const Directory: string);
  var
    DirInfo: TSearchRec;
    SubDir: string;
    Found: Boolean;
  begin
    HandleDirectory(Directory);

    Found := SysUtils.FindFirst(Directory + '*', Attr, DirInfo) = 0;
    try
      while Found do
      begin
        if (Abort <> nil) and Abort^ then
          Exit;
        if (DirInfo.Name <> '.') and (DirInfo.Name <> '..') and
          {$IFDEF UNIX}
          (IncludeHiddenDirectories or (Pos('.', DirInfo.Name) <> 1)) and
          ((DirInfo.Attr and faSymLink = 0) or ResolveSymLinks) and
          {$ENDIF UNIX}
          (DirInfo.Attr and faDirectory <> 0) then
        begin
          SubDir := Directory + DirInfo.Name + PathSeparator;
          if (SubDirectoriesMask = '') or StrMatches(SubDirectoriesMask, SubDir, Length(RootDir)) then
            Process(SubDir);
        end;
        Found := FindNext(DirInfo) = 0;
      end;
    finally
      FindClose(DirInfo);
    end;
  end;

begin
  Assert(Assigned(HandleDirectory));
  RootDir := CanonicalizedSearchPath(Root);

  if IncludeHiddenDirectories then
    Attr := faDirectory + faHidden  // no effect on Linux
  else
    Attr := faDirectory;

  Process(RootDir);
end;

//==================================================================================================
// TJclFileAttributeMask
//==================================================================================================

constructor TJclCustomFileAttrMask.Create;
begin
  inherited Create;
  FRejectedAttr := faRejectedByDefault;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclCustomFileAttrMask.Assign(Source: TPersistent);
begin
  if Source is TJclCustomFileAttrMask then
  begin
    Required := TJclCustomFileAttrMask(Source).Required;
    Rejected := TJclCustomFileAttrMask(Source).Rejected;
  end
  else
    inherited Assign(Source);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclCustomFileAttrMask.Clear;
begin
  Rejected := 0;
  Required := 0;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclCustomFileAttrMask.DefineProperties(Filer: TFiler);
var
  Ancestor: TJclCustomFileAttrMask;
  Attr: Integer;
begin
  Attr := 0;
  Ancestor := TJclCustomFileAttrMask(Filer.Ancestor);
  if Assigned(Ancestor) then
    Attr := Ancestor.FRequiredAttr;
  Filer.DefineProperty('Required', ReadRequiredAttributes, WriteRequiredAttributes,
    Attr <> FRequiredAttr);
  if Assigned(Ancestor) then
    Attr := Ancestor.FRejectedAttr;
  Filer.DefineProperty('Rejected', ReadRejectedAttributes, WriteRejectedAttributes,
    Attr <> FRejectedAttr);
end;

//--------------------------------------------------------------------------------------------------

function TJclCustomFileAttrMask.Match(FileAttributes: Integer): Boolean;
begin
  Result := AttributeMatch(FileAttributes, Rejected, Required);
end;

//--------------------------------------------------------------------------------------------------

function TJclCustomFileAttrMask.Match(const FileInfo: TSearchRec): Boolean;
begin
  Result := Match(FileInfo.Attr);
end;

//--------------------------------------------------------------------------------------------------

function TJclCustomFileAttrMask.GetAttr(Index: Integer): TAttributeInterest;
begin
  if ((FRequiredAttr and Index) <> 0) or (Index = faNormalFile) and
    (FRejectedAttr = not faNormalFile) then
    Result := aiRequired
  else
  if (FRejectedAttr and Index) <> 0 then
    Result := aiRejected
  else
    Result := aiIgnored;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclCustomFileAttrMask.ReadRejectedAttributes(Reader: TReader);
begin
  FRejectedAttr := Reader.ReadInteger;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclCustomFileAttrMask.ReadRequiredAttributes(Reader: TReader);
begin
  FRequiredAttr := Reader.ReadInteger;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclCustomFileAttrMask.SetAttr(Index: Integer; const Value: TAttributeInterest);
begin
  case Value of
    aiIgnored:
      begin
        FRequiredAttr := FRequiredAttr and not Index;
        FRejectedAttr := FRejectedAttr and not Index;
      end;
    aiRejected:
      begin
        FRequiredAttr := FRequiredAttr and not Index;
        FRejectedAttr := FRejectedAttr or Index;
      end;
    aiRequired:
      begin
        if Index = faNormalFile then
        begin
          FRequiredAttr := faNormalFile;
          FRejectedAttr := not faNormalFile;
        end
        else
        begin
          FRequiredAttr := FRequiredAttr or Index;
          FRejectedAttr := FRejectedAttr and not Index;
        end;
      end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclCustomFileAttrMask.WriteRejectedAttributes(Writer: TWriter);
begin
  Writer.WriteInteger(FRejectedAttr);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclCustomFileAttrMask.WriteRequiredAttributes(Writer: TWriter);
begin
  Writer.WriteInteger(FRequiredAttr);
end;

//==================================================================================================
// internal class TEnumFileThread
//==================================================================================================

type
  TEnumFileThread = class(TThread)
  private
    FID: TFileSearchTaskID;
    FFileMasks: TStringList;
    FDirectory: string;
    FSubDirectoryMask: string;
    FOnEnterDirectory: TFileHandler;
    FFileHandlerEx: TFileHandlerEx;
    FFileHandler: TFileHandler;
    FInternalDirHandler: TFileHandler;
    FInternalFileHandler: TFileHandlerEx;
    FFileInfo: TSearchRec;
    FRejectedAttr: Integer;
    FRequiredAttr: Integer;
    FFileSizeMin: Int64;
    FFileSizeMax: Int64;
    FFileTimeMin: Integer;
    FFileTimeMax: Integer;
    FSynchronizationMode: TFileEnumeratorSyncMode;
    FIncludeSubDirectories: Boolean;
    FIncludeHiddenSubDirectories: Boolean;
    FNotifyOnTermination: Boolean;
    FCaseSensitiveSearch: Boolean;
    FAllNamesMatch: Boolean;
    procedure EnterDirectory;
    procedure AsyncProcessDirectory(const Directory: string);
    procedure SyncProcessDirectory(const Directory: string);
    procedure AsyncProcessFile(const Directory: string; const FileInfo: TSearchRec);
    procedure SyncProcessFile(const Directory: string; const FileInfo: TSearchRec);
    function GetFileMasks: TStrings;
    procedure SetFileMasks(const Value: TStrings);
  protected
    procedure DoTerminate; override;
    procedure Execute; override;
    function FileMatch: Boolean;
    function FileNameMatchesMask: Boolean;
    procedure ProcessDirectory;
    procedure ProcessDirFiles;
    procedure ProcessFile;
    property AllNamesMatch: Boolean read FAllNamesMatch;
    property CaseSensitiveSearch: Boolean read FCaseSensitiveSearch write FCaseSensitiveSearch;
    property FileMasks: TStrings read GetFileMasks write SetFileMasks;
    property FileSizeMin: Int64 read FFileSizeMin write FFileSizeMin;
    property FileSizeMax: Int64 read FFileSizeMax write FFileSizeMax;
    property Directory: string read FDirectory write FDirectory;
    property IncludeSubDirectories: Boolean
      read FIncludeSubDirectories write FIncludeSubDirectories;
    property IncludeHiddenSubDirectories: Boolean
      read FIncludeHiddenSubDirectories write FIncludeHiddenSubDirectories;
    property RejectedAttr: Integer read FRejectedAttr write FRejectedAttr;
    property RequiredAttr: Integer read FRequiredAttr write FRequiredAttr;
    property SynchronizationMode: TFileEnumeratorSyncMode
      read FSynchronizationMode write FSynchronizationMode;
  public
    constructor Create;
    destructor Destroy; override;
    property ID: TFileSearchTaskID read FID;
    {$IFDEF FPC} // protected property
    property Terminated;
    {$ENDIF FPC}
  end;

//--------------------------------------------------------------------------------------------------

constructor TEnumFileThread.Create;
begin
  inherited Create(True);
  FFileMasks := TStringList.Create;
  FFileTimeMin := Low(FFileInfo.Time);
  FFileTimeMax := High(FFileInfo.Time);
  FFileSizeMax := High(FFileSizeMax);
  {$IFDEF MSWINDOWS}
  Priority := tpIdle;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  Priority := 0;
  {$ENDIF UNIX}
  FreeOnTerminate := True;
  FNotifyOnTermination := True;
end;

//--------------------------------------------------------------------------------------------------

destructor TEnumFileThread.Destroy;
begin
  FFileMasks.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TEnumFileThread.Execute;
begin
  if SynchronizationMode = smPerDirectory then
  begin
    FInternalDirHandler := SyncProcessDirectory;
    FInternalFileHandler := AsyncProcessFile;
  end
  else // SynchronizationMode = smPerFile
  begin
    FInternalDirHandler := AsyncProcessDirectory;
    FInternalFileHandler := SyncProcessFile;
  end;

  if FIncludeSubDirectories then
    EnumDirectories(Directory, FInternalDirHandler, FIncludeHiddenSubDirectories,
      FSubDirectoryMask, @Terminated)
  else
    FInternalDirHandler(CanonicalizedSearchPath(Directory));
end;

//--------------------------------------------------------------------------------------------------

procedure TEnumFileThread.DoTerminate;
begin
  if FNotifyOnTermination then
    inherited DoTerminate;
end;

//--------------------------------------------------------------------------------------------------

procedure TEnumFileThread.EnterDirectory;
begin
  FOnEnterDirectory(Directory);
end;

//--------------------------------------------------------------------------------------------------

procedure TEnumFileThread.ProcessDirectory;
begin
  if Assigned(FOnEnterDirectory) then
    EnterDirectory;
  ProcessDirFiles;
end;

//--------------------------------------------------------------------------------------------------

procedure TEnumFileThread.AsyncProcessDirectory(const Directory: string);
begin
  FDirectory := Directory;
  if Assigned(FOnEnterDirectory) then
    Synchronize(EnterDirectory);
  ProcessDirFiles;
end;

//--------------------------------------------------------------------------------------------------

procedure TEnumFileThread.SyncProcessDirectory(const Directory: string);
begin
  FDirectory := Directory;
  Synchronize(ProcessDirectory);
end;

//--------------------------------------------------------------------------------------------------

procedure TEnumFileThread.ProcessDirFiles;
begin
  EnumFiles(Directory + '*', FInternalFileHandler, FRejectedAttr, FRequiredAttr, @Terminated);
end;

//--------------------------------------------------------------------------------------------------

function TEnumFileThread.FileMatch: Boolean;
var
  FileSize: Int64;
begin
  Result := FileNameMatchesMask and (FFileInfo.Time >= FFileTimeMin) and (FFileInfo.Time <= FFileTimeMax);
  if Result then
  begin
    FileSize := GetSizeOfFile(FFileInfo);
    Result := (FileSize >= FFileSizeMin) and (FileSize <= FFileSizeMax);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEnumFileThread.FileNameMatchesMask: Boolean;
var
  I: Integer;
begin
  Result := AllNamesMatch;
  if not Result then
    for I := 0 to FileMasks.Count - 1 do
      if IsFileNameMatch(FFileInfo.Name, FileMasks[I], CaseSensitiveSearch) then
      begin
        Result := True;
        Break;
      end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEnumFileThread.ProcessFile;
begin
  if Assigned(FFileHandlerEx) then
    FFileHandlerEx(Directory, FFileInfo)
  else
    FFileHandler(Directory + FFileInfo.Name);
end;

//--------------------------------------------------------------------------------------------------

procedure TEnumFileThread.AsyncProcessFile(const Directory: string; const FileInfo: TSearchRec);
begin
  FFileInfo := FileInfo;
  if FileMatch then
    ProcessFile;
end;

//--------------------------------------------------------------------------------------------------

procedure TEnumFileThread.SyncProcessFile(const Directory: string; const FileInfo: TSearchRec);
begin
  FFileInfo := FileInfo;
  if FileMatch then
    Synchronize(ProcessFile);
end;

//--------------------------------------------------------------------------------------------------

function TEnumFileThread.GetFileMasks: TStrings;
begin
  Result := FFileMasks;
end;

//--------------------------------------------------------------------------------------------------

procedure TEnumFileThread.SetFileMasks(const Value: TStrings);
var
  I: Integer;
begin
  FAllNamesMatch := Value.Count = 0;
  for I := 0 to Value.Count - 1 do
    if (Value[I] = '*') {$IFDEF MSWINDOWS} or (Value[I] = '*.*') {$ENDIF} then
    begin
      FAllNamesMatch := True;
      Break;
    end;
  if FAllNamesMatch then
    FileMasks.Clear
  else
    FileMasks.Assign(Value);
end;

//==================================================================================================
// TJclFileEnumerator
//==================================================================================================

constructor TJclFileEnumerator.Create;
begin
  inherited Create;
  FTasks := TList.Create;
  FAttributeMask := TJclFileAttributeMask.Create;
  FRootDirectory := '.';
  FFileMasks := TStringList.Create;
  FFileMasks.Add('*');
  FSubDirectoryMask := '*';
  FOptions := [fsIncludeSubDirectories];
  FLastChangeAfter := MinDateTime;
  FLastChangeBefore := MaxDateTime;
  {$IFDEF UNIX}
  FCaseSensitiveSearch := True;
  {$ENDIF UNIX}
end;

//--------------------------------------------------------------------------------------------------

destructor TJclFileEnumerator.Destroy;
begin
  StopAllTasks(True);
  FTasks.Free;
  FAttributeMask.Free;
  FFileMasks.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileEnumerator.AfterConstruction;
begin
  inherited AfterConstruction;
  if GetOwner <> nil then
    GetOwner.GetInterface(IInterface, FOwnerInterface);
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator._AddRef: Integer;
begin
  if FOwnerInterface <> nil then
    Result := FOwnerInterface._AddRef
  else
    Result := InterlockedIncrement(FRefCount);
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator._Release: Integer;
begin
  if FOwnerInterface <> nil then
    Result := FOwnerInterface._Release
  else
  begin
    Result := InterlockedDecrement(FRefCount);
    if Result = 0 then
      Destroy;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileEnumerator.Assign(Source: TPersistent);
var
  Src: TJclFileEnumerator;
begin
  if Source is TJclFileEnumerator then
  begin
    Src := TJclFileEnumerator(Source);
    FCaseSensitiveSearch := Src.FCaseSensitiveSearch;
    FileMasks.Assign(Src.FileMasks);
    RootDirectory := Src.RootDirectory;
    SubDirectoryMask := Src.SubDirectoryMask;
    AttributeMask := Src.AttributeMask;
    Options := Src.Options;
    FileSizeMin := Src.FileSizeMin;
    FileSizeMax := Src.FileSizeMax;
    LastChangeAfter := Src.LastChangeAfter;
    LastChangeBefore := Src.LastChangeBefore;
    SynchronizationMode := Src.SynchronizationMode;
    OnEnterDirectory := Src.OnEnterDirectory;
    OnTerminateTask := Src.OnTerminateTask;
  end
  else
    inherited Assign(Source);
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.CreateTask: TThread;
var
  Task: TEnumFileThread;
begin
  Task := TEnumFileThread.Create;
  Task.FID := NextTaskID;
  Task.CaseSensitiveSearch := FCaseSensitiveSearch;
  Task.FileMasks := FileMasks;
  Task.Directory := RootDirectory;
  Task.RejectedAttr := AttributeMask.Rejected;
  Task.RequiredAttr := AttributeMask.Required;
  Task.IncludeSubDirectories := IncludeSubDirectories;
  Task.IncludeHiddenSubDirectories := IncludeHiddenSubDirectories;
  if fsMinSize in Options then
    Task.FileSizeMin := FileSizeMin;
  if fsMaxSize in Options then
    Task.FileSizeMax := FileSizeMax;
  if fsLastChangeAfter in Options then
    Task.FFileTimeMin := DateTimeToFileDate(LastChangeAfter);
  if fsLastChangeBefore in Options then
    Task.FFileTimeMax := DateTimeToFileDate(LastChangeBefore);
  Task.SynchronizationMode := SynchronizationMode;
  Task.FOnEnterDirectory := OnEnterDirectory;
  Task.OnTerminate := TaskTerminated;
  FTasks.Add(Task);
  if FRefCount > 0 then
    _AddRef;
  Result := Task;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.FillList(List: TStrings): TFileSearchTaskID;
begin
  List.BeginUpdate;
  try
    Result := ForEach(List.Append);
  finally
    List.EndUpdate;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.ForEach(Handler: TFileHandlerEx): TFileSearchTaskID;
var
  Task: TEnumFileThread;
begin
  Task := TEnumFileThread(CreateTask);
  Task.FFileHandlerEx := Handler;
  Result := Task.ID;
  Task.Resume;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.ForEach(Handler: TFileHandler): TFileSearchTaskID;
var
  Task: TEnumFileThread;
begin
  Task := TEnumFileThread(CreateTask);
  Task.FFileHandler := Handler;
  Result := Task.ID;
  Task.Resume;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.GetRunningTasks: Integer;
begin
  Result := FTasks.Count;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileEnumerator.StopTask(ID: TFileSearchTaskID);
var
  Task: TEnumFileThread;
  I: Integer;
begin
  for I := 0 to FTasks.Count - 1 do
  begin
    Task := TEnumFileThread(FTasks[I]);
    if Task.ID = ID then
    begin
      Task.Terminate;
      Break;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileEnumerator.StopAllTasks(Silently: Boolean = False);
var
  I: Integer;
begin
  for I := 0 to FTasks.Count - 1 do
    with TEnumFileThread(FTasks[I]) do
    begin
      FNotifyOnTermination := not Silently;
      Terminate;
    end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileEnumerator.TaskTerminated(Sender: TObject);
begin
  FTasks.Remove(Sender);
  try
    if Assigned(FOnTerminateTask) then
      with TEnumFileThread(Sender) do
        FOnTerminateTask(ID, Terminated);
  finally
    if FRefCount > 0 then
      _Release;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.GetNextTaskID: TFileSearchTaskID;
begin
  Result := FNextTaskID;
  Inc(FNextTaskID);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileEnumerator.SetAttributeMask(const Value: TJclFileAttributeMask);
begin
  FAttributeMask.Assign(Value);
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.GetLastChangeAfterStr: string;
begin
  Result := DateTimeToStr(LastChangeAfter);
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.GetLastChangeBeforeStr: string;
begin
  Result := DateTimeToStr(LastChangeBefore);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileEnumerator.SetLastChangeAfterStr(const Value: string);
begin
  if Value = '' then
    LastChangeAfter := MinDateTime
  else
    LastChangeAfter := StrToDateTime(Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileEnumerator.SetLastChangeBeforeStr(const Value: string);
begin
  if Value = '' then
    LastChangeBefore := MaxDateTime
  else
    LastChangeBefore := StrToDateTime(Value);
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.GetAttributeMask: TJclFileAttributeMask;
begin
  Result := FAttributeMask;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.GetCaseSensitiveSearch: Boolean;
begin
  Result := FCaseSensitiveSearch;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.GetRootDirectory: string;
begin
  Result := FRootDirectory;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.GetFileMask: string;
begin
  Result := StringsToStr(FileMasks, ';', False);
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.GetFileMasks: TStrings;
begin
  Result := FFileMasks;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.GetFileSizeMax: Int64;
begin
  Result := FFileSizeMax;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.GetFileSizeMin: Int64;
begin
  Result := FFileSizeMin;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.GetIncludeHiddenSubDirectories: Boolean;
begin
  Result := fsIncludeHiddenSubDirectories in Options;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.GetIncludeSubDirectories: Boolean;
begin
  Result := fsIncludeSubDirectories in Options;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.GetLastChangeAfter: TDateTime;
begin
  Result := FLastChangeAfter;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.GetLastChangeBefore: TDateTime;
begin
  Result := FLastChangeBefore;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.GetOnEnterDirectory: TFileHandler;
begin
  Result := FOnEnterDirectory;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.GetOnTerminateTask: TFileSearchTerminationEvent;
begin
  Result := FOnTerminateTask;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.GetOption(const Option: TFileSearchOption): Boolean;
begin
  Result := Option in FOptions;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.GetOptions: TFileSearchOptions;
begin
  Result := FOptions;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.GetSubDirectoryMask: string;
begin
  Result := FSubDirectoryMask;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.GetSynchronizationMode: TFileEnumeratorSyncMode;
begin
  Result := FSynchronizationMode;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.IsLastChangeAfterStored: Boolean;
begin
  Result := FLastChangeAfter <> MinDateTime;
end;

//--------------------------------------------------------------------------------------------------

function TJclFileEnumerator.IsLastChangeBeforeStored: Boolean;
begin
  Result := FLastChangeBefore <> MaxDateTime;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileEnumerator.SetCaseSensitiveSearch(const Value: Boolean);
begin
  FCaseSensitiveSearch := Value;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileEnumerator.SetRootDirectory(const Value: string);
begin
  FRootDirectory := Value;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileEnumerator.SetFileMask(const Value: string);
begin
  { TODO : UNIX : ? }
  StrToStrings(Value, ';', FFileMasks, False);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileEnumerator.SetFileMasks(const Value: TStrings);
begin
  FileMasks.Assign(Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileEnumerator.SetFileSizeMax(const Value: Int64);
begin
  FFileSizeMax := Value;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileEnumerator.SetFileSizeMin(const Value: Int64);
begin
  FFileSizeMin := Value;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileEnumerator.SetIncludeHiddenSubDirectories(
  const Value: Boolean);
begin
  SetOption(fsIncludeHiddenSubDirectories, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileEnumerator.SetIncludeSubDirectories(
  const Value: Boolean);
begin
  SetOption(fsIncludeSubDirectories, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileEnumerator.SetLastChangeAfter(const Value: TDateTime);
begin
  FLastChangeAfter := Value;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileEnumerator.SetLastChangeBefore(const Value: TDateTime);
begin
  FLastChangeBefore := Value;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileEnumerator.SetOnEnterDirectory(
  const Value: TFileHandler);
begin
  FOnEnterDirectory := Value;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileEnumerator.SetOnTerminateTask(
  const Value: TFileSearchTerminationEvent);
begin
  FOnTerminateTask := Value;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileEnumerator.SetOption(const Option: TFileSearchOption; const Value: Boolean);
begin
  if Value then
    Include(FOptions, Option)
  else
    Exclude(FOptions, Option);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileEnumerator.SetOptions(const Value: TFileSearchOptions);
begin
  FOptions := Value;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileEnumerator.SetSubDirectoryMask(const Value: string);
begin
  FSubDirectoryMask := Value;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFileEnumerator.SetSynchronizationMode(
  const Value: TFileEnumeratorSyncMode);
begin
  FSynchronizationMode := Value;
end;

//--------------------------------------------------------------------------------------------------

function FileSearch: IJclFileEnumerator;
begin
  Result := TJclFileEnumerator.Create;
end;

// History:

// $Log$
// Revision 1.35  2004/12/20 04:03:25  rrossmair
// - added FileGetOwnerName, FileGetGroupName functions (Windows part)
//
// Revision 1.34  2004/11/18 10:13:24  rrossmair
// - fixed for Unix
//
// Revision 1.33  2004/11/18 00:42:59  rrossmair
// - fixed mantis #1667
//
// Revision 1.32  2004/10/17 21:40:17  rrossmair
// fixed PathGetLongName
//
// Revision 1.31  2004/10/17 17:26:57  rrossmair
// restored Unix compatibility
//
// Revision 1.30  2004/10/17 06:02:51  rrossmair
// - rewrote PathGetLongName
// - rewrote PathGetLongName2 (now mere wrapper around PathGetLongName)
// - restored FPC compatibility
//
// Revision 1.29  2004/10/15 03:54:20  rrossmair
// - added FileCreateTemp/Unix
// - added FileBackup
// - added FileCopy
// - added FileDelete
// - added FileMove
// - added FileRestore
// - added FileGetTempName/Unix
// - added PathGetTempPath
// - reworked Win32* functions as mere wrappers for corresponding File* functions
// - removed some PH contributions
//
// Revision 1.28  2004/08/03 07:22:36  marquardt
// resourcestring cleanup
//
// Revision 1.27  2004/08/02 06:34:59  marquardt
// minor string literal improvements
//
// Revision 1.26  2004/08/01 05:52:11  marquardt
// move constructors/destructors
//
// Revision 1.25  2004/07/30 07:20:25  marquardt
// fixing TStringLists, adding BeginUpdate/EndUpdate
//
// Revision 1.24  2004/07/29 07:58:20  marquardt
// inc files updated
//
// Revision 1.23  2004/07/28 18:00:50  marquardt
// various style cleanings, some minor fixes
//
// Revision 1.22  2004/06/16 07:30:27  marquardt
// added tilde to all IFNDEF ENDIFs, inherited qualified
//
// Revision 1.21  2004/06/14 11:05:51  marquardt
// symbols added to all ENDIFs and some other minor style changes like removing IFOPT
//
// Revision 1.20  2004/06/14 06:24:52  marquardt
// style cleaning IFDEF
//
// Revision 1.19  2004/05/31 01:42:27  rrossmair
// Processed documentation TODOs
//
// Revision 1.18  2004/05/18 19:07:58  rrossmair
// fixed contributor information
//
// Revision 1.17  2004/05/13 07:46:05  rrossmair
// changes for FPC 1.9.3+ compatibility
//
// Revision 1.16  2004/05/09 11:19:13  rrossmair
// Contributor list update
//
// Revision 1.15  2004/05/08 19:56:55  rrossmair
// FPC-related improvements
//
// Revision 1.14  2004/05/08 08:44:17  rrossmair
// introduced & applied symbol HAS_UNIT_LIBC
//
// Revision 1.13  2004/05/06 05:09:55  rrossmair
// Changes for FPC v1.9.4 compatibility
//
// Revision 1.12  2004/05/05 00:04:11  mthoma
// Updated headers: Added donors as contributors, adjusted the initial authors, added cvs names when they were not obvious. Changed $data to $date where necessary,
//

end.
