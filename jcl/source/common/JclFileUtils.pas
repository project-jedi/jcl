 {******************************************************************************}
{                                                                              }
{ Project JEDI Code Library (JCL) http://delphi-jedi.org                       }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JclFileUtils.pas.                                       }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ 2000 of these individuals.                                                   }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This unit contains routines and classes for working with files, directories  }
{ and path strings. Additionally it contains wrapper classes for file mapping  }
{ objects and version resources. Generically speaking, everything that has to  }
{ do with files and directories. Note that filesystem specific functionality   }
{ has been extracted into external units, for example JclNTFS which contains   }
{ NTFS specific utility routines, and that the JclShell unit contains some     }
{ file related routines as well but they are specific to the Windows shell.    }
{                                                                              }
{ Unit owner: Marcel van Brakel                                                }
{ Last modified: April 19, 2001                                                }
{                                                                              }
{******************************************************************************}

unit JclFileUtils;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

uses
  {$IFDEF WIN32}
  Windows,
  {$ENDIF WIN32}
  Classes, Graphics, SysUtils,
  JclBase, JclSysInfo;

//------------------------------------------------------------------------------
// Path Manipulation
//
// Various support routines for working with path strings. For example, building
// a path from elements or extracting the elements from a path, interpretation
// of paths and transformations of paths.
//------------------------------------------------------------------------------

const
  {$IFDEF LINUX}
  PathSeparator    = '/';
  {$ENDIF LINUX}
  {$IFDEF WIN32}
  DriveLetters     = ['a'..'z', 'A'..'Z'];
  PathDevicePrefix = '\\.\';
  PathSeparator    = '\';
  PathUncPrefix    = '\\';
  {$ENDIF WIN32}

type
  TCompactPath = ({cpBegin, }cpCenter, cpEnd);

function PathAddSeparator(const Path: string): string;
function PathAddExtension(const Path, Extension: string): string;
function PathAppend(const Path, Append: string): string;
function PathBuildRoot(const Drive: Byte): string;
function PathCommonPrefix(const Path1, Path2: string): Integer;
function PathCompactPath(const DC: HDC; const Path: string; const Width: Integer;
  CmpFmt: TCompactPath): string; overload;
function PathCompactPath(const Canvas: TCanvas; const Path: string; const Width: Integer;
  CmpFmt: TCompactPath): string; overload;
procedure PathExtractElements(const Source: string; var Drive, Path, FileName, Ext: string);
function PathExtractFileDirFixed(const S: AnsiString): AnsiString;
function PathExtractFileNameNoExt(const Path: string): string;
function PathGetLongName(const Path: string): string;
function PathGetLongName2(Path: string): string;
function PathGetShortName(const Path: string): string;
function PathIsAbsolute(const Path: string): Boolean;
function PathIsChild(const Path, Base: AnsiString): Boolean;
function PathIsDiskDevice(const Path: string): Boolean;
function PathIsUNC(const Path: string): Boolean;
function PathRemoveSeparator(const Path: string): string;
function PathRemoveExtension(const Path: string): string;

//------------------------------------------------------------------------------
// Files and Directories
//
// Routines for working with files and directories. Includes routines to extract
// various file attributes or update them, volume locking and routines for
// creating temporary files.
//------------------------------------------------------------------------------

type
  TDelTreeProgress = function (const FileName: string; Attr: DWORD): Boolean;
  TFileListOption  = (flFullNames, flRecursive, flMaskedSubfolders);
  TFileListOptions = set of TFileListOption;

function BuildFileList(const Path: string; const Attr: Integer; const List: TStrings): Boolean;
function AdvBuildFileList(const Path: string; const Attr: Integer;
  const Files: TStrings; const Options: TFileListOptions {$IFDEF SUPPORTS_DEFAULTPARAMS} = [] {$ENDIF};
  const SubfoldersMask: string {$IFDEF SUPPORTS_DEFAULTPARAMS} = '' {$ENDIF}): Boolean;
function CloseVolume(var Volume: THandle): Boolean;
procedure CreateEmptyFile(const FileName: string);
function DelTree(const Path: string): Boolean;
function DelTreeEx(const Path: string; AbortOnFailure: Boolean; Progress: TDelTreeProgress): Boolean;
function DirectoryExists(const Name: string): Boolean;
{$IFDEF WIN32}
function DiskInDrive(Drive: Char): Boolean;
{$ENDIF WIN32}
function FileCreateTemp(var Prefix: string): THandle;
function FileExists(const FileName: string): Boolean;
function FileGetDisplayName(const FileName: string): string;
function FileGetSize(const FileName: string): Integer;
function FileGetTempName(const Prefix: string): string;
function FileGetTypeName(const FileName: string): string;
function FindUnusedFileName(const FileName, FileExt, Suffix: AnsiString): AnsiString;
function ForceDirectories(Name: string): Boolean;
function GetDirectorySize(const Path: string): Int64;
function GetDriveTypeStr(const Drive: Char): string;
function GetFileAgeCoherence(const FileName: string): Boolean;
procedure GetFileAttributeList(const Items: TStrings; const Attr: Integer);
procedure GetFileAttributeListEx(const Items: TStrings; const Attr: Integer);
function GetFileInformation(const FileName: string): TSearchRec;
function GetFileLastWrite(const FileName: string): TFileTime;
function GetFileLastAccess(const FileName: string): TFileTime;
function GetFileCreation(const FileName: string): TFileTime;
function GetModulePath(const Module: HMODULE): string;
function GetSizeOfFile(const FileName: string): Int64; overload;
function GetSizeOfFile(Handle: THandle): Int64; overload;
function GetStandardFileInfo(const FileName: string): TWin32FileAttributeData;
function IsDirectory(const FileName: string): Boolean;
function LockVolume(const Volume: string; var Handle: THandle): Boolean;
function OpenVolume(const Drive: Char): THandle;
function SetDirLastWrite(const DirName: string; const DateTime: TDateTime): Boolean;
function SetDirLastAccess(const DirName: string; const DateTime: TDateTime): Boolean;
function SetDirCreation(const DirName: string; const DateTime: TDateTime): Boolean;
function SetFileLastWrite(const FileName: string; const DateTime: TDateTime): Boolean;
function SetFileLastAccess(const FileName: string; const DateTime: TDateTime): Boolean;
function SetFileCreation(const FileName: string; const DateTime: TDateTime): Boolean;
procedure ShredFile(const FileName: string; Times: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF});
function UnlockVolume(var Handle: THandle): Boolean;

//------------------------------------------------------------------------------
// TFileVersionInfo
//
// Class that enables reading the version information stored in a PE file.
//------------------------------------------------------------------------------

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

  TJclFileVersionInfo = class (TObject)
  private
    FBuffer: string;
    FFixedInfo: PVSFixedFileInfo;
    FFileFlags: TFileFlags;
    FItemList: TStrings;
    FItems: TStrings;
    FLanguages: array of TLangIdRec;
    FLanguageIndex: Integer;
    FTranslations: array of TLangIdRec;
    function GetFixedInfo: TVSFixedFileInfo;
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
    function TranslationMatchesLanguages(Exact: Boolean {$IFDEF SUPPORTS_DEFAULTPARAMS} = True {$ENDIF}): Boolean;
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
    property Items: TStrings read FItems;
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

  EJclFileVersionInfoError = class (EJclError);

function OSIdentToString(const OSIdent: DWORD): string;
function OSFileTypeToString(const OSFileType: DWORD; const OSFileSubType: DWORD {$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF}): string;
function VersionResourceAvailable(const FileName: string): Boolean;

//------------------------------------------------------------------------------
// Streams
//
// TStream descendent classes for dealing with temporary files and for using
// file mapping objects.
//------------------------------------------------------------------------------

{ TTempFileStream }

type
  TJclTempFileStream = class (THandleStream)
  private
    FFileName: string;
  public
    constructor Create(const Prefix: string);
    destructor Destroy; override;
    property FileName: string read FFileName;
  end;

  TJclCustomFileMapping = class;

{ TJclFileMappingView }

  TJclFileMappingView = class (TCustomMemoryStream)
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

{ TJclCustomFileMapping }

  TJclFileMappingRoundOffset = (rvDown, rvUp);

  TJclCustomFileMapping = class (TObject)
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
      const Protect: Cardinal; MaximumSize: Int64; const SecAttr: PSecurityAttributes);
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

{ TJclFileMapping }

  TJclFileMapping = class (TJclCustomFileMapping)
  private
    FFileHandle: THandle;
  public
    constructor Create(const FileName: string; FileMode: Cardinal;
      const Name: string; Protect: Cardinal; const MaximumSize: Int64;
      const SecAttr: PSecurityAttributes); overload;
    constructor Create(const FileHandle: THandle; const Name: string;
      Protect: Cardinal; const MaximumSize: Int64;
      const SecAttr: PSecurityAttributes); overload;
    destructor Destroy; override;
    property FileHandle: THandle read FFileHandle;
  end;

{ TJclSwapFileMapping }

  TJclSwapFileMapping = class (TJclCustomFileMapping)
  public
    constructor Create(const Name: string; Protect: Cardinal;
      const MaximumSize: Int64; const SecAttr: PSecurityAttributes);
  end;

//------------------------------------------------------------------------------
// TJclFileMappingStream
//------------------------------------------------------------------------------

  TJclFileMappingStream = class (TCustomMemoryStream)
  private
    FFileHandle: THandle;
    FMapping: THandle;
  protected
    procedure Close;
  public
    constructor Create(const FileName: string; FileMode: Word
      {$IFDEF SUPPORTS_DEFAULTPARAMS} = fmOpenRead or fmShareDenyWrite {$ENDIF});
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

//------------------------------------------------------------------------------
// TJclFileMaskComparator
//------------------------------------------------------------------------------

// TODO UNTESTET/UNDOCUMENTED

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

//------------------------------------------------------------------------------
// Exceptions
//------------------------------------------------------------------------------

  EJclPathError = class (EJclError);
  EJclFileUtilsError = class (EJclError);
  EJclTempFileStreamError = class (EJclWin32Error);
  EJclFileMappingError = class (EJclWin32Error);
  EJclFileMappingViewError = class (EJclWin32Error);

implementation

uses
  {$IFDEF WIN32}
  ActiveX, ShellApi, ShlObj,
  {$ENDIF WIN32}
  JclResources, JclSecurity, JclStrings, JclSysUtils, JclWin32, JclDateTime;

{ Some general notes:

  This unit redeclares some functions from FileCtrl.pas to avoid a dependeny on
  that unit in the JCL. The problem is that FileCtrl.pas uses some units (eg
  Forms.pas) which have ridiculous initialization requirements. They add 4KB (!)
  to the executable and roughly 1 second of startup. That initialization is only
  necessary for GUI applications and is unacceptable for high performance
  services or console apps.

  The routines which query files or directories for their attributes
  deliberately use FindFirst eventhough there may be easier ways to get at the
  required information. This is because FindFirst is about the only routine
  which doesn't cause the file's last modification/accessed time to be changed
  which is usually an undesired side-effect. }

//==============================================================================
// TJclTempFileStream
//==============================================================================

constructor TJclTempFileStream.Create(const Prefix: string);
var
  FileHandle: THandle;
begin
  FFileName := Prefix;
  FileHandle := FileCreateTemp(FFileName);
  if FileHandle = INVALID_HANDLE_VALUE then
    raise EJclTempFileStreamError.CreateResRec(@RsFileStreamCreate);
  inherited Create(FileHandle);
end;

//------------------------------------------------------------------------------

destructor TJclTempFileStream.Destroy;
begin
  if THandle(Handle) <> INVALID_HANDLE_VALUE then
    CloseHandle(Handle);
  inherited Destroy;
end;

//==============================================================================
// TJclFileMappingView
//==============================================================================

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
  // If we are mapping a file and size = 0 then MapViewOfFile has mapped the
  // entire file. We must figure out the size ourselves before we can call
  // SetPointer. Since in case of failure to retrieve the size we raise an
  // exception, we also have to explicitly unmap the view which otherwise would
  // have been done by the destructor.
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

//------------------------------------------------------------------------------

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
  // If we are mapping a file and size = 0 then MapViewOfFile has mapped the
  // entire file. We must figure out the size ourselves before we can call
  // SetPointer. Since in case of failure to retrieve the size we raise an
  // exception, we also have to explicitly unmap the view which otherwise would
  // have been done by the destructor.
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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

function TJclFileMappingView.Flush(const Count: Cardinal): Boolean;
begin
  Result := FlushViewOfFile(Memory, Count);
end;

//------------------------------------------------------------------------------

function TJclFileMappingView.GetIndex: Integer;
begin
  Result := FFileMapping.IndexOf(Self);
end;

//------------------------------------------------------------------------------

function TJclFileMappingView.GetOffset: Int64;
begin
  CardinalsToI64(Result, FOffsetLow, FOffsetHigh);
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

procedure TJclFileMappingView.LoadFromStream(const Stream: TStream);
begin
  if Stream.Size > Size then
    raise EJclFileMappingViewError.CreateResRec(@RsLoadFromStreamSize);
  Stream.Position := 0;
  Stream.ReadBuffer(Memory^, Stream.Size);
end;

//------------------------------------------------------------------------------

function TJclFileMappingView.Write(const Buffer; Count: Integer): Longint;
begin
  Result := 0;
  if (Size - Position) >= Count then
  begin
    System.Move(Buffer, Memory^, Count);
    Position := Position + Count;
    Result := Count;
  end;
end;

//==============================================================================
// TJclCustomFileMapping
//==============================================================================

function TJclCustomFileMapping.Add(const Access, Count: Cardinal; const Offset: Int64): Integer;
var
  View: TJclFileMappingView;
begin
  // The view adds itself to the FViews list
  View := TJclFileMappingView.Create(Self, Access, Count, Offset);
  Result := View.Index;
end;

//------------------------------------------------------------------------------

function TJclCustomFileMapping.AddAt(const Access, Count: Cardinal;
  const Offset: Int64; const Address: Pointer): Integer;
var
  View: TJclFileMappingView;
begin
  // The view adds itself to the FViews list
  View := TJclFileMappingView.CreateAt(Self, Access, Count, Offset, Address);
  Result := View.Index;
end;

//------------------------------------------------------------------------------

procedure TJclCustomFileMapping.ClearViews;
var
  I: Integer;
begin
  // Note that the view destructor removes the view object from the FViews
  // list so we must loop downwards from count to 0
  for I := FViews.Count - 1 downto 0 do
    TJclFileMappingView(FViews[I]).Free;
end;

//------------------------------------------------------------------------------

constructor TJclCustomFileMapping.Create;
begin
  inherited Create;
  FViews := TList.Create;
  FRoundViewOffset := rvDown;
end;

//------------------------------------------------------------------------------

constructor TJclCustomFileMapping.Open(const Name: string;
  const InheritHandle: Boolean; const DesiredAccess: Cardinal);
begin
  Create;
  InternalOpen(Name, InheritHandle, DesiredAccess);
end;

//------------------------------------------------------------------------------

procedure TJclCustomFileMapping.Delete(const Index: Integer);
begin
  // Note that the view destructor removes itself from FViews
  TJclFileMappingView(FViews[Index]).Free;
end;

//------------------------------------------------------------------------------

destructor TJclCustomFileMapping.Destroy;
begin
  ClearViews;
  if FHandle <> 0 then
    CloseHandle(FHandle);
  FreeAndNil(FViews);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TJclCustomFileMapping.GetCount: Integer;
begin
  Result := FViews.Count;
end;

//------------------------------------------------------------------------------

function TJclCustomFileMapping.GetView(Index: Integer): TJclFileMappingView;
begin
  Result := TJclFileMappingView(FViews.Items[index]);
end;

//------------------------------------------------------------------------------

function TJclCustomFileMapping.IndexOf(const View: TJclFileMappingView): Integer;
begin
  Result := FViews.IndexOf(View);
end;

//------------------------------------------------------------------------------

procedure TJclCustomFileMapping.InternalCreate(const FileHandle: THandle;
  const Name: string; const Protect: Cardinal; MaximumSize: Int64;
  const SecAttr: PSecurityAttributes);
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

//------------------------------------------------------------------------------

procedure TJclCustomFileMapping.InternalOpen(const Name: string;
  const InheritHandle: Boolean; const DesiredAccess: Cardinal);
begin
  FExisted := True;
  FName := Name;
  FHandle := OpenFileMapping(DesiredAccess, InheritHandle, PChar(Name));
  if FHandle = 0 then
    raise EJclFileMappingError.CreateResRec(@RsCreateFileMapping);
end;

//==============================================================================
// TJclFileMapping
//==============================================================================

constructor TJclFileMapping.Create(const FileName: string; FileMode: Cardinal;
  const Name: string; Protect: Cardinal; const MaximumSize: Int64;
  const SecAttr: PSecurityAttributes);
begin
  FFileHandle := INVALID_HANDLE_VALUE;
  inherited Create;
  FFileHandle := FileOpen(FileName, FileMode);
  if FFileHandle = INVALID_HANDLE_VALUE then
    raise EJclFileMappingError.CreateResRec(@RsFileMappingOpenFile);
  InternalCreate(FFileHandle, Name, Protect, MaximumSize, SecAttr);
end;

//------------------------------------------------------------------------------

constructor TJclFileMapping.Create(const FileHandle: THandle; const Name: string;
  Protect: Cardinal; const MaximumSize: Int64; const SecAttr: PSecurityAttributes);
begin
  FFileHandle := INVALID_HANDLE_VALUE;
  inherited Create;
  if FileHandle = INVALID_HANDLE_VALUE then
    raise EJclFileMappingError.CreateResRec(@RsFileMappingInvalidHandle);
  InternalCreate(FileHandle, Name, Protect, MaximumSize, SecAttr);
  // Duplicate the handle into FFileHandle as opposed to assigning it directly.
  // This will cause FFileHandle to retrieve a unique copy which is independent
  // of FileHandle. This makes the remainder of the class, especially the
  // destructor, easier. The caller will have to close it's own copy of the
  // handle explicitly.
  DuplicateHandle(GetCurrentProcess, FileHandle, GetCurrentProcess,
    @FFileHandle, 0, False, DUPLICATE_SAME_ACCESS);
end;

//------------------------------------------------------------------------------

destructor TJclFileMapping.Destroy;
begin
  if FFileHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FFileHandle);
  inherited Destroy;
end;

//==============================================================================
// TJclSwapFileMapping
//==============================================================================

constructor TJclSwapFileMapping.Create(const Name: string; Protect: Cardinal;
  const MaximumSize: Int64; const SecAttr: PSecurityAttributes);
begin
  inherited Create;
  InternalCreate(INVALID_HANDLE_VALUE, Name, Protect, MaximumSize, SecAttr);
end;

//==============================================================================
// TJclFileMappingStream
//==============================================================================

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

//------------------------------------------------------------------------------

constructor TJclFileMappingStream.Create(const FileName: string; FileMode: Word);
var
  Protect, Access, Size: DWORD;
  BaseAddress: Pointer;
begin
  inherited Create;
  FFileHandle := FileOpen(FileName, FileMode);
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

//------------------------------------------------------------------------------

destructor TJclFileMappingStream.Destroy;
begin
  Close;
  inherited;
end;

//------------------------------------------------------------------------------

function TJclFileMappingStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := 0;
  if (Size - Position) >= Count then
  begin
    System.Move(Buffer, Memory^, Count);
    Position := Position + Count;
    Result := Count;
  end;
end;

//==============================================================================
// Path manipulation
//==============================================================================

function PathAddSeparator(const Path: string): string;
begin
  Result := Path;
  if (Length(Path) = 0) or (AnsiLastChar(Path) <> PathSeparator) then
    Result := Path + PathSeparator;
end;

//------------------------------------------------------------------------------

function PathAddExtension(const Path, Extension: string): string;
begin
  Result := Path;
  if (Path <> '') and (ExtractFileExt(Path) = '') and (Extension <> '') then
  begin
    // Note that if we get here Path is quarenteed not to end in a '.' otherwise
    // ExtractFileExt would have returned '.' therefore there's no need to check
    // it explicitly in the code below
    if Extension[1] = '.' then
      Result := Result + Extension
    else
      Result := Result + '.' + Extension;
  end;
end;

//------------------------------------------------------------------------------

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
      // The following code may look a bit complex but al it does is add Append
      // to Path ensuring that there is one and only one path separator character
      // between them
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

//------------------------------------------------------------------------------

function PathBuildRoot(const Drive: Byte): string;
begin
  {$IFDEF LINUX}
  Result := PathSeparator;
  {$ENDIF LINUX}
  {$IFDEF WIN32}
  // Remember, Win32 only allows 'a' to 'z' as drive letters (mapped to 0..25)
  if Drive < 26 then
    Result := Char(Drive + 65) + ':\'
  else
    raise EJclPathError.CreateResRecFmt(@RsPathInvalidDrive, [IntToStr(Drive)]);
  {$ENDIF WIN32}
end;

//------------------------------------------------------------------------------

function PathCommonPrefix(const Path1, Path2: string): Integer;
var
  P1, P2: PChar;
  LastSeparator: Integer;
begin
  Result := 0;
  if (Path1 <> '') and (Path2 <> '') then
  begin
    // Initialize P1 to the shortest of the two paths so that the actual comparison
    // loop below can use the terminating #0 of that string to terminate the loop.
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

//------------------------------------------------------------------------------

function PathCompactPath(const DC: HDC; const Path: string;
  const Width: Integer; CmpFmt: TCompactPath): string;
const
  Compacts: array [TCompactPath] of Cardinal = (DT_PATH_ELLIPSIS, DT_END_ELLIPSIS);
var
  TextRect: TRect;
  P: PChar;
  Fmt: Cardinal;
begin
  Result := '';
  if (DC <> 0) and (Path <> '') and (Width > 0) then
  begin
  { Here's a note from the Platform SDK to explain the + 5 in the call below:
    "If dwDTFormat includes DT_MODIFYSTRING, the function could add up to four
    additional characters to this string. The buffer containing the string
    should be large enough to accommodate these extra characters." }
    P := StrAlloc(Length(Path) + 5);
    try
      StrPCopy(P, Path);
      TextRect := Rect(0, 0, Width, 255);
      Fmt := DT_MODIFYSTRING + DT_CALCRECT + Compacts[CmpFmt];
      if DrawTextEx(DC, P, -1, TextRect, Fmt, nil) <> 0 then
        Result := P;
    finally
      StrDispose(P);
    end;
  end;
end;

//------------------------------------------------------------------------------

function PathCompactPath(const Canvas: TCanvas; const Path: string;
  const Width: Integer; CmpFmt: TCompactPath): string; overload;
begin
  Result := PathCompactPath(Canvas.Handle, Path, Width, CmpFmt);
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

function PathExtractFileDirFixed(const S: AnsiString): AnsiString;
begin
  Result := PathAddSeparator(ExtractFileDir(S));
end;

//------------------------------------------------------------------------------

function PathExtractFileNameNoExt(const Path: string): string;
begin
  Result := PathRemoveExtension(ExtractFileName(Path));
end;

//------------------------------------------------------------------------------

function PathGetLongName2(Path: string): string;
var
  I : Integer;
  SearchHandle : THandle;
  FindData : TWin32FindData;
  IsBackSlash : Boolean;
begin
  Path := ExpandFileName(Path);
  Result := ExtractFileDrive(Path);
  I := Length(Result);
  if Length(Path) <= I then Exit;   // only drive
  if Path[I + 1] = '\' then
  begin
    Result := Result + '\';
    Inc(I);
  end;
  Delete(Path, 1, I);
  repeat
    I := Pos('\', Path);
    IsBackSlash := I > 0;
    if Not IsBackSlash then
      I := Length(Path) + 1;
    SearchHandle := FindFirstFile(PChar(Result + Copy(Path, 1, 
      I - 1)), FindData);
    if SearchHandle <> INVALID_HANDLE_VALUE then
    begin
      try
        Result := Result + FindData.cFileName;
        if IsBackSlash then
          Result := Result + '\';
      finally
        Windows.FindClose(SearchHandle);
      end;
    end
    else
    begin
      Result := Result + Path;
      Break;
    end;
    Delete(Path, 1, I);
  until Length(Path) = 0;
end;

//------------------------------------------------------------------------------

function PathGetLongName(const Path: string): string;
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
          Result := PChar(AnsiName);
      finally
        CoTaskMemFree(PIDL);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

function PathIsAbsolute(const Path: string): Boolean;
{$IFDEF WIN32}
var
  I: Integer;
{$ENDIF WIN32}
begin
  Result := False;
  if Path <> '' then
  begin
    {$IFDEF LINUX}
    Result := (Path[1] = PathSeparator);
    {$ENDIF LINUX}
    {$IFDEF WIN32}
    I := 0;
    if PathIsUnc(Path) then
      I := Length(PathUncPrefix)
    else
    if PathIsDiskDevice(Path) then
      I := Length(PathDevicePrefix);
    Result := (Length(Path) > I + 2) and (Path[I + 1] in DriveLetters) and
      (Path[I + 2] = ':') and (Path[I + 3] = PathSeparator);
    {$ENDIF WIN32}
  end;
end;

//------------------------------------------------------------------------------

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
  {$IFDEF WIN32}
  Result := AnsiSameText(StrLeft(P, L), B) and (P[L+1] = PathSeparator);
  {$ENDIF WIN32}
  {$IFDEF LINUX}
  Result := AnsiSameStr(StrLeft(P, L), B) and (P[L+1] = PathSeparator);
  {$ENDIF LINUX}
end;

//------------------------------------------------------------------------------

function PathIsDiskDevice(const Path: string): Boolean;
begin
  {$IFDEF LINUX}
  NotImplemented('PathIsDiskDevice');
  {$ENDIF LINUX}
  {$IFDEF WIN32}
  Result := Copy(Path, 1, Length(PathDevicePrefix)) = PathDevicePrefix;
  {$ENDIF WIN32}
end;

//------------------------------------------------------------------------------

function PathIsUNC(const Path: string): Boolean;

{$IFDEF WIN32}

var
  P: PChar;

  function AbsorbSeperator: Boolean;
  begin
    Result := (P <> nil) and (P^ = '\');
    if Result then Inc(P);
  end;

  function AbsorbMachineName: Boolean;
  var
    NonDigitFound: Boolean;
  begin
    // a valid machine name is a string composed of the set [a-z, A-Z, 0-9, -]
    // but it may not consist entirely out of numbers
    Result := True;
    NonDigitFound := False;
    while (P <> nil) and (P^ <> #0) and (P^ <> '\') do
    begin
      if P^ in ['a'..'z', 'A'..'Z', '-'] then
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
    InvalidCharacters = ['<','>','?','/',',','*','+','=','[',']','|',':',';','"',''''];
  begin
    // a valid share name is a string composed of a set the set !InvalidCharacters
    // note that a leading '$' is valid (indicates a hidden share)
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
    if Copy(Path, 1, Length(PathUncPrefix + '?\UNC')) = PathUncPrefix + '?\UNC' then
      P := @Path[Length(PathUncPrefix + '?\UNC')]
    else
    begin
      P := @Path[Length(PathUncPrefix)];
      Result := AbsorbSeperator and AbsorbMachineName;
    end;
    Result := Result and AbsorbSeperator;
    if Result then
    begin
      Result := AbsorbShareName;
      // remaining, if anything, is path and or filename (optional) check those?
    end;
  end;
end;

{$ENDIF WIN32}
{$IFDEF LINUX}

begin
  Result := False;
end;

{$ENDIF LINUX}

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

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

//==============================================================================
// Files and Directories
//==============================================================================

function BuildFileList(const Path: string; const Attr: Integer; const List: TStrings): Boolean;
var
  SearchRec: TSearchRec;
  R: Integer;
begin
  Assert(List <> nil);
  R := FindFirst(Path, Attr, SearchRec);
  Result := R = 0;
  if Result then
  begin
    while R = 0 do
    begin
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        List.Add(SearchRec.Name);
      R := FindNext(SearchRec);
    end;
    Result := R = ERROR_NO_MORE_FILES;
    SysUtils.FindClose(SearchRec);
  end;
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

procedure CreateEmptyFile(const FileName: string);
var
  Handle: THandle;
begin
  Handle := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_NEW, 0, 0);
  if Handle <> INVALID_HANDLE_VALUE then
    CloseHandle(Handle)
  else
    RaiseLastOSError;
end;

//------------------------------------------------------------------------------

function DelTree(const Path: string): Boolean;
begin
  Result := DelTreeEx(Path, False, nil);
end;

//------------------------------------------------------------------------------

function DelTreeEx(const Path: string; AbortOnFailure: Boolean; Progress: TDelTreeProgress): Boolean;
var
  Files: TStringList;
  LPath: string; // writable copy of Path
  FileName: string;
  I: Integer;
  PartialResult: Boolean;
  Attr: DWORD;
begin
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
      {$I-}
      RmDir(LPath);
      {$I+}
      Result := IOResult = 0;
    end;
  end;
end;

//------------------------------------------------------------------------------

function DirectoryExists(const Name: string): Boolean;
var
  R: DWORD;
begin
  R := GetFileAttributes(PChar(Name));
  Result := (R <> DWORD(-1)) and ((R and FILE_ATTRIBUTE_DIRECTORY) <> 0);
end;

//------------------------------------------------------------------------------

{$IFDEF WIN32}

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
  { try to access the drive, it doesn't really matter how we access the drive
    and as such calling DiskSize is more or less a random choice. The call to
    SetErrorMode supresses the system provided error dialog if there is no
    disk in the drive and causes the to DiskSize to fail. }
    ErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
    try
      Result := DiskSize(Ord(Drive) - $40) <> -1;
    finally
      SetErrorMode(ErrorMode);
    end;
  end;
end;

{$ENDIF WIN32}

//------------------------------------------------------------------------------

function FileCreateTemp(var Prefix: string): THandle;
var
  TempName: string;
begin
  Result := INVALID_HANDLE_VALUE;
  TempName := FileGetTempName(Prefix);
  if TempName <> '' then
  begin
    Result := CreateFile(PChar(TempName), GENERIC_READ or GENERIC_WRITE, 0, nil,
      OPEN_EXISTING, FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE, 0);
    // In certain situations it's possible that CreateFile fails yet the file is
    // actually created, therefore explicitly delete it upon failure.
    if Result = INVALID_HANDLE_VALUE then
      DeleteFile(TempName);
    Prefix := TempName;
  end;
end;

//------------------------------------------------------------------------------

function FileExists(const FileName: string): Boolean;
begin
  // Attempt to access the file, doesn't matter how, using FileGetSize is as
  // good as anything else.
  Result := FileGetSize(FileName) <> -1;
end;

//------------------------------------------------------------------------------

function FileGetDisplayName(const FileName: string): string;
var
  FileInfo: TSHFileInfo;
begin
  FillChar(FileInfo, SizeOf(FileInfo), #0);
  if SHGetFileInfo(PChar(FileName), 0, FileInfo, SizeOf(FileInfo), SHGFI_DISPLAYNAME) <> 0 then
    Result := FileInfo.szDisplayName
  else
    Result := FileName;
end;

//------------------------------------------------------------------------------

function FileGetSize(const FileName: string): Integer;
var
  SearchRec: TSearchRec;
  OldMode: Cardinal;
begin
  Result := -1;
  OldMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    if FindFirst(FileName, faAnyFile, SearchRec) = 0 then
    begin
      Result := SearchRec.Size;
      SysUtils.FindClose(SearchRec);
    end;
  finally
    SetErrorMode(OldMode);
  end;
end;

//------------------------------------------------------------------------------

function FileGetTempName(const Prefix: string): string;
var
  TempPath, TempFile: string;
  R: Cardinal;
begin
  Result := '';
  R := GetTempPath(0, nil);
  SetLength(TempPath, R);
  R := GetTempPath(R, PChar(TempPath));
  if R <> 0 then
  begin
    SetLength(TempPath, StrLen(PChar(TempPath)));
    SetLength(TempFile, MAX_PATH);
    R := GetTempFileName(PChar(TempPath), PChar(Prefix), 0, PChar(TempFile));
    if R <> 0 then
    begin
      SetLength(TempFile, StrLen(PChar(TempFile)));
      Result := TempFile;
    end;
  end;
end;

//------------------------------------------------------------------------------

function FileGetTypeName(const FileName: string): string;
var
  FileInfo: TSHFileInfo;
  RetVal: DWORD;
begin
  FillChar(FileInfo, SizeOf(FileInfo), #0);
  RetVal := SHGetFileInfo(PChar(FileNAme), 0, FileInfo, SizeOf(FileInfo),
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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

// This routine is copied from FileCtrl.pas to avoid dependency on that unit.
// See the remark at the top of this section

function ForceDirectories(Name: string): Boolean;
begin
  Result := True;
  if Length(Name) = 0 then
    raise EJclFileUtilsError.CreateResRec(@RsCannotCreateDir);
  Name := PathRemoveSeparator(Name);
  if (Length(Name) < 3) or DirectoryExists(Name)
    or (ExtractFilePath(Name) = Name) then
    Exit;
  Result := ForceDirectories(ExtractFilePath(Name)) and CreateDir(Name);
end;

//------------------------------------------------------------------------------

function GetDirectorySize(const Path: string): Int64;

  function RecurseFolder(const Path: string): Int64;
  var
    F: TSearchRec;
    R: Integer;
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
            Result := Result + (F.FindData.nFileSizeHigh shl 32) + F.FindData.nFileSizeLow;
        end;
        R := SysUtils.FindNext(F);
      end;
      if R <> ERROR_NO_MORE_FILES then Abort;
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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

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
    Result := CompareFileTime(FindData.ftCreationTime, FindData.ftLastWriteTime) <= 0;
  end;
end;

//------------------------------------------------------------------------------

procedure GetFileAttributeList(const Items: TStrings; const Attr: Integer);
begin
  Assert(Items <> nil);
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
end;

//------------------------------------------------------------------------------

procedure GetFileAttributeListEx(const Items: TStrings; const Attr: Integer);
begin
  Assert(Items <> nil);
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
end;

//------------------------------------------------------------------------------

function GetFileInformation(const FileName: string): TSearchRec;
begin
  if FindFirst(FileName, faAnyFile, Result) = 0 then
    SysUtils.FindClose(Result)
  else
    RaiseLastOSError;
end;

//------------------------------------------------------------------------------

function GetFileLastWrite(const FileName: string): TFileTime;
begin
  Result := GetFileInformation(FileName).FindData.ftLastWriteTime;
end;

//------------------------------------------------------------------------------

function GetFileLastAccess(const FileName: string): TFileTime;
begin
  Result := GetFileInformation(FileName).FindData.ftLastAccessTime;
end;

//------------------------------------------------------------------------------

function GetFileCreation(const FileName: string): TFileTime;
begin
  Result := GetFileInformation(FileName).FindData.ftCreationTime;
end;

//------------------------------------------------------------------------------

function GetModulePath(const Module: HMODULE): string;
var
  L: Integer;
begin
  L := MAX_PATH + 1;
  SetLength(Result, L);
  L := Windows.GetModuleFileName(Module, Pointer(Result), L);
  SetLength(Result, L);
end;

//------------------------------------------------------------------------------

function GetSizeOfFile(const FileName: string): Int64;
var
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Result := 0;
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    Result := (FindData.nFileSizeHigh shl 32) + FindData.nFileSizeLow;
  end
  else
    RaiseLastOSError;
end;

//------------------------------------------------------------------------------

function GetSizeOfFile(Handle: THandle): Int64;
var
  Size: TULargeInteger absolute Result;
begin
  Size.LowPart := GetFileSize(Handle, @Size.HighPart);
end;

//------------------------------------------------------------------------------

function GetStandardFileInfo(const FileName: string): TWin32FileAttributeData;
var
  Handle: THandle;
  FileInfo: TByHandleFileInformation;
begin
  Assert(FileName <> '');
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

//------------------------------------------------------------------------------

function IsDirectory(const FileName: string): Boolean;
var
  R: DWORD;
begin
  R := GetFileAttributes(PChar(FileName));
  Result := (R <> DWORD(-1)) and ((R and FILE_ATTRIBUTE_DIRECTORY) <> 0);
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

function OpenVolume(const Drive: Char): THandle;
var
  VolumeName: array [0..6] of Char;
begin
  VolumeName := '\\.\A:';
  VolumeName[4] := Drive;
  Result := CreateFile(VolumeName, GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil, OPEN_EXISTING, 0, 0);
end;

//------------------------------------------------------------------------------

type
  // indicates the file time to set, used by SetFileTimesHelper and SetDirTimesHelper
  TFileTimes = (ftLastAccess, ftLastWrite, ftCreation);

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

//------------------------------------------------------------------------------

function SetFileLastAccess(const FileName: string; const DateTime: TDateTime): Boolean;
begin
  Result := SetFileTimesHelper(FileName, DateTime, ftLastAccess);
end;

//------------------------------------------------------------------------------

function SetFileLastWrite(const FileName: string; const DateTime: TDateTime): Boolean;
begin
  Result := SetFileTimesHelper(FileName, DateTime, ftLastWrite);
end;

//------------------------------------------------------------------------------

function SetFileCreation(const FileName: string; const DateTime: TDateTime): Boolean;
begin
  Result := SetFileTimesHelper(FileName, DateTime, ftCreation);
end;

//------------------------------------------------------------------------------

// utility function for SetDirTimesHelper

function BackupPrivilegesEnabled: Boolean;
begin
  Result := IsPrivilegeEnabled(SE_BACKUP_NAME) and IsPrivilegeEnabled(SE_RESTORE_NAME);
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

function SetDirLastWrite(const DirName: string; const DateTime: TDateTime): Boolean;
begin
  Result := SetDirTimesHelper(DirName, DateTime, ftLastWrite);
end;

//------------------------------------------------------------------------------

function SetDirLastAccess(const DirName: string; const DateTime: TDateTime): Boolean;
begin
  Result := SetDirTimesHelper(DirName, DateTime, ftLastAccess);
end;

//------------------------------------------------------------------------------

function SetDirCreation(const DirName: string; const DateTime: TDateTime): Boolean;
begin
  Result := SetDirTimesHelper(DirName, DateTime, ftCreation);
end;

//------------------------------------------------------------------------------

procedure ShredFile(const FileName: string; Times: Integer);
const
  BUFSIZE = 4096;
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
      Times := 1;
    ContentPtr := nil;
    Fs := TFileStream.Create(FileName, fmOpenReadWrite);
    try
      GetAndFillMem(ContentPtr, BUFSIZE, Ord('*'));
      while Times > 0 do
      begin
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

//------------------------------------------------------------------------------

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

//==============================================================================
// TJclFileVersionInfo
//==============================================================================

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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

constructor TJclFileVersionInfo.Attach(VersionInfoData: Pointer; Size: Integer);
begin
  SetString(FBuffer, PChar(VersionInfoData), Size);
  ExtractData;
end;

//------------------------------------------------------------------------------

procedure TJclFileVersionInfo.CheckLanguageIndex(Value: Integer);
begin
  if (Value < 0) or (Value >= LanguageCount) then
    raise EJclFileVersionInfoError.CreateResRec(@RsFileUtilsLanguageIndex);
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

procedure TJclFileVersionInfo.CreateItemsForLanguage;
var
  I: Integer;
begin
  FItems.Clear;
  for I := 0 to FItemList.Count - 1 do
    if Integer(FItemList.Objects[I]) = FLanguageIndex then
      FItems.AddObject(FItemList[I], Pointer(FLanguages[FLanguageIndex].Pair));
end;

//------------------------------------------------------------------------------

destructor TJclFileVersionInfo.Destroy;
begin
  FreeAndNil(FItemList);
  FreeAndNil(FItems);
  inherited;
end;

//------------------------------------------------------------------------------

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
        GetHeader; // String
        case DataType of
          0: if ValueLen in [1..4] then
               Value := Format('$%.*x', [ValueLen * 2, PInteger(Data)^])
             else
               Value := '';
          1: if ValueLen = 0 then
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
    if Key = 'Translation' then
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
  if (Key = 'VS_VERSION_INFO') and (ValueLen = SizeOf(TVSFixedFileInfo)) then
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
      if Key = 'StringFileInfo' then
        ProcessStringInfo(Len)
      else
      if Key = 'VarFileInfo' then
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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

function TJclFileVersionInfo.GetBinFileVersion: string;
begin
  with FFixedInfo^ do
    Result := Format('%u.%u.%u.%u', [HiWord(dwFileVersionMS),
      LoWord(dwFileVersionMS), HiWord(dwFileVersionLS), LoWord(dwFileVersionLS)]);
end;

//------------------------------------------------------------------------------

function TJclFileVersionInfo.GetBinProductVersion: string;
begin
  with FFixedInfo^ do
    Result := Format('%u.%u.%u.%u', [HiWord(dwProductVersionMS),
      LoWord(dwProductVersionMS), HiWord(dwProductVersionLS),
      LoWord(dwProductVersionLS)]);
end;

//------------------------------------------------------------------------------

function TJclFileVersionInfo.GetFileOS: DWORD;
begin
  Result := FFixedInfo^.dwFileOS;
end;

//------------------------------------------------------------------------------

function TJclFileVersionInfo.GetFileSubType: DWORD;
begin
  Result := FFixedInfo^.dwFileSubtype;
end;

//------------------------------------------------------------------------------

function TJclFileVersionInfo.GetFileType: DWORD;
begin
  Result := FFixedInfo^.dwFileType;
end;

//------------------------------------------------------------------------------

function TJclFileVersionInfo.GetFixedInfo: TVSFixedFileInfo;
begin
  Result := FFixedInfo^;
end;

//------------------------------------------------------------------------------

function TJclFileVersionInfo.GetLanguageCount: Integer;
begin
  Result := Length(FLanguages);
end;

//------------------------------------------------------------------------------

function TJclFileVersionInfo.GetLanguageIds(Index: Integer): string;
begin
  CheckLanguageIndex(Index);
  Result := VersionLanguageId(FLanguages[Index]);
end;

//------------------------------------------------------------------------------

function TJclFileVersionInfo.GetLanguages(Index: Integer): TLangIdRec;
begin
  CheckLanguageIndex(Index);
  Result := FLanguages[Index];
end;

//------------------------------------------------------------------------------

function TJclFileVersionInfo.GetLanguageNames(Index: Integer): string;
begin
  CheckLanguageIndex(Index);
  Result := VersionLanguageName(FLanguages[Index].LangId);
end;

//------------------------------------------------------------------------------

function TJclFileVersionInfo.GetTranslationCount: Integer;
begin
  Result := Length(FTranslations);
end;

//------------------------------------------------------------------------------

function TJclFileVersionInfo.GetTranslations(Index: Integer): TLangIdRec;
begin
  Result := FTranslations[Index];
end;

//------------------------------------------------------------------------------

function TJclFileVersionInfo.GetVersionKeyValue(Index: Integer): string;
begin
  Result := FItems.Values[VerKeyNames[Index]];
end;

//------------------------------------------------------------------------------

procedure TJclFileVersionInfo.SetLanguageIndex(const Value: Integer);
begin
  CheckLanguageIndex(Value);
  if FLanguageIndex <> Value then
  begin
    FLanguageIndex := Value;
    CreateItemsForLanguage;
  end;
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

class function TJclFileVersionInfo.VersionLanguageId(const LangIdRec: TLangIdRec): string;
begin
  with LangIdRec do
    Result := Format('%.4x%.4x', [LangId, CodePage]);
end;

//------------------------------------------------------------------------------

class function TJclFileVersionInfo.VersionLanguageName(const LangId: Word): string;
var
  R: DWORD;
begin
  SetLength(Result, MAX_PATH);
  R := VerLanguageName(LangId, PChar(Result), MAX_PATH);
  SetLength(Result, R);
end;

//==============================================================================
// TJclFileMaskComparator
//==============================================================================

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
    if ( (not NameWild and StrSame(FNames[I], NamePart)) or
      (NameWild and (StrMatches(FNames[I], NamePart, 1))) ) and
      ( (not ExtWild and StrSame(FExts[I], ExtPart)) or
      (ExtWild and (StrMatches(FExts[I], ExtPart, 1))) ) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

constructor TJclFileMaskComparator.Create;
begin
  inherited;
  FSeparator := ';';
end;

//------------------------------------------------------------------------------

procedure TJclFileMaskComparator.CreateMultiMasks;
const
  WildChars = ['*', '?'];
var
  List: TStrings;
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
      if StrContainsChars(NS, WildChars, False) then N := N or 1;
      if StrContainsChars(ES, WildChars, False) then N := N or 2;
      FWildChars[I] := N;
    end;
  finally
    List.Free;
  end;
end;

//------------------------------------------------------------------------------

function TJclFileMaskComparator.GetCount: Integer;
begin
  Result := Length(FWildChars);
end;

//------------------------------------------------------------------------------

function TJclFileMaskComparator.GetExts(Index: Integer): string;
begin
  Result := FExts[Index];
end;

//------------------------------------------------------------------------------

function TJclFileMaskComparator.GetMasks(Index: Integer): string;
begin
  Result := FNames[Index] + '.' + FExts[Index];
end;

//------------------------------------------------------------------------------

function TJclFileMaskComparator.GetNames(Index: Integer): string;
begin
  Result := FNames[Index];
end;

//------------------------------------------------------------------------------

procedure TJclFileMaskComparator.SetFileMask(const Value: string);
begin
  FFileMask := Value;
  CreateMultiMasks;
end;

//------------------------------------------------------------------------------

procedure TJclFileMaskComparator.SetSeparator(const Value: Char);
begin
  if FSeparator <> Value then
  begin
    FSeparator := Value;
    CreateMultiMasks;
  end;
end;

//------------------------------------------------------------------------------

function AdvBuildFileList(const Path: string; const Attr: Integer;
  const Files: TStrings; const Options: TFileListOptions; const SubfoldersMask: string): Boolean;
var
  FileMask: string;
  RootDir: string;
  Folders: TStringList;
  CurrentItem: Integer;
  Counter: Integer;
  LocAttr: Integer;

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
    CurrentFolder: String;
  begin
    CurrentFolder := Folders[CurrentCounter];
    Rslt := FindFirst(CurrentFolder + FileMask, LocAttr, FindInfo);
    try
      while Rslt = 0 do
      begin
        if (LocAttr and FindInfo.Attr = FindInfo.Attr) then
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
  FileMask := ExtractFileName(Path);
  RootDir := ExtractFilePath(Path);

  Folders := TStringList.Create;
  Folders.Add(RootDir);

  if Attr = faAnyFile then
    LocAttr := faReadOnly + faHidden + faSysFile + faArchive
  else
    LocAttr := Attr;

  // here's the recursive search for nested folders

  if flRecursive in Options then
    BuildFolderList;

  for Counter := 0 to Folders.Count - 1 do
  begin
    if (((flMaskedSubfolders in Options) and (StrMatches(SubfoldersMask, Folders[Counter]))) or
      (not (flMaskedSubfolders in Options))) then
      FillFileList(Counter);
  end;
  Folders.Free;
  Result := True;
end;

end.
