{******************************************************************************}
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.0 (the "License"); you may not use this file except in compliance with the }
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
{ of these individuals.                                                        }
{                                                                              }
{ Last modified: October 05, 2000                                              }
{                                                                              }
{******************************************************************************}

unit JclFileUtils;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

uses
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  Classes, Graphics, SysUtils,
  JclBase, JclSysInfo;

//------------------------------------------------------------------------------
// Path Manipulation
//------------------------------------------------------------------------------

const
  {$IFDEF LINUX}
  PathSeparator    = '/';
  {$ENDIF}
  {$IFDEF WIN32}
  DriveLetters     = ['a'..'z', 'A'..'Z'];
  PathDevicePrefix = '\\.\';
  PathSeparator    = '\';
  PathUncPrefix    = '\\';
  {$ENDIF}

function PathAddSeparator(const Path: string): string;
function PathAddExtension(const Path, Extension: string): string;
function PathAppend(const Path, Append: string): string;
function PathBuildRoot(const Drive: Byte): string;
function PathCommonPrefix(const Path1, Path2: string): Integer;
function PathCompactPath(const DC: HDC; const Path: string; const Width: Integer;
  CmpFmt: Boolean {$IFDEF SUPPORTS_DEFAULTPARAMS} = True {$ENDIF}): string; overload;
function PathCompactPath(const Canvas: TCanvas; const Path: string; const Width: Integer;
  CmpFmt: Boolean {$IFDEF SUPPORTS_DEFAULTPARAMS} = True {$ENDIF}): string; overload;
procedure PathExtractElements(const Source: string; var Drive, Path, FileName, Ext: string);
function PathExtractFileDirFixed(const S: AnsiString): AnsiString; // TODOC Anthony
function PathExtractFileNameNoExt(const Path: string): string;
function PathGetLongName(const Path: string): string;
function PathGetShortName(const Path: string): string;
function PathIsAbsolute(const Path: string): Boolean;
function PathIsChild(const Path, Base: AnsiString): Boolean; // TODOC Anthony
function PathIsDiskDevice(const Path: string): Boolean;
function PathIsUNC(const Path: string): Boolean;
function PathRemoveSeparator(const Path: string): string;
function PathRemoveExtension(const Path: string): string;

//------------------------------------------------------------------------------
// Files and Directories
//------------------------------------------------------------------------------

type
  TDelTreeProgress = function (const FileName: string; Attr: DWORD): Boolean;

function BuildFileList(const Path: string; const Attr: Integer; const List: TStrings): Boolean;
function CloseVolume(const Volume: THandle): Boolean; // TODO DOC MASSIMO
function DelTree(const Path: string): Boolean;
function DelTreeEx(Path: string; AbortOnFailure: Boolean; Progress: TDelTreeProgress): Boolean;
function DirectoryExists(const Name: string): Boolean;
function DiskInDrive(Drive: Char): Boolean;
function FileCreateTemp(var Prefix: string): THandle;
function FileExists(const FileName: string): Boolean;
function FileGetDisplayName(const FileName: string): string;
function FileGetSize(const FileName: string): Integer;
function FileGetTempName(const Prefix: string): string;
function FileGetTypeName(const FileName: string): string;
function ForceDirectories(Name: string): Boolean;
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
function GetStandardFileInfo(const FileName: string): TWin32FileAttributeData; // TODOC Anthony
function IsDirectory(const FileName: string): Boolean;
function LockVolume(const Volume: string; var Handle: THandle): Boolean;
function OpenVolume(const Drive: Char): DWORD; // TODO DOC MASSIMO
function SetDirLastWrite(const DirName: string; const DateTime: TDateTime): Boolean;
function SetDirLastAccess(const DirName: string; const DateTime: TDateTime): Boolean;
function SetDirCreation(const DirName: string; const DateTime: TDateTime): Boolean;
function SetFileLastWrite(const FileName: string; const DateTime: TDateTime): Boolean;
function SetFileLastAccess(const FileName: string; const DateTime: TDateTime): Boolean;
function SetFileCreation(const FileName: string; const DateTime: TDateTime): Boolean;
procedure ShredFile(const FileName: string; Times: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF});
function UnLockVolume(var Handle: THandle): Boolean;

//------------------------------------------------------------------------------
// TFileVersionInfo
//------------------------------------------------------------------------------

type
  TFileFlag = (ffDebug, ffInfoInferred, ffPatched, ffPreRelease, ffPrivateBuild,
               ffSpecialBuild);
  TFileFlags = set of TFileFlag;

  TJclFileVersionInfo = class (TObject)
  private
    FBuffer: string;
    FFileFlags: TFileFlags;
    FFileName: string;
    FFixedBuffer: PVSFixedFileInfo;
    FLanguageIndex: Integer;
    FLanguages: TStrings;
    function GetLanguageCount: Integer;
    function GetLanguageIds(Index: Integer): string;
    function GetLanguageNames(Index: Integer): string;
    procedure ExtractLanguageIds;
    procedure SetLanguageIndex(Value: Integer);
  protected
    function GetBinFileVersion: string;
    function GetBinProductVersion: string;
    function GetFileOS: DWORD;
    function GetFileSubType: DWORD;
    function GetFileType: DWORD;
    function GetUserKey(const Key: string): string;
    procedure GetVersionInfo;
    function GetVersionKeyValue(Index: Integer): string;
  public
    constructor Create(const AFileName: string);
    constructor Attach(const Buffer: string);
    destructor Destroy; override;
    property Comments: string index 1 read GetVersionKeyValue;
    property CompanyName: string index 2 read GetVersionKeyValue;
    property FileDescription: string index 3 read GetVersionKeyValue;
    property FileFlags: TFileFlags read FFileFlags;
    property FileOS: DWORD read GetFileOS;
    property FileSubType: DWORD read GetFileSubType;
    property FileType: DWORD read GetFileType;
    property FileVersion: string index 4 read GetVersionKeyValue;
    property InternalName: string index 5 read GetVersionKeyValue;
    property LanguageCount: Integer read GetLanguageCount;
    property LanguageIndex: Integer read FLanguageIndex write SetLanguageIndex;
    property LanguageIds[Index: Integer]: string read GetLanguageIds;
    property LanguageNames[Index: Integer]: string read GetLanguageNames;
    property LegalCopyright: string index 6 read GetVersionKeyValue;
    property LegalTradeMarks: string index 7 read GetVersionKeyValue;
    property OriginalFilename: string index 8 read GetVersionKeyValue;
    property ProductName: string index 9 read GetVersionKeyValue;
    property ProductVersion: string index 10 read GetVersionKeyValue;
    property SpecialBuild: string index 11 read GetVersionKeyValue;
    property PrivateBuild: string index 12 read GetVersionKeyValue;
    property UserKeys[const Key: string]: string read GetUserKey;
    property BinFileVersion: string read GetBinFileVersion;
    property BinProductVersion: string read GetBinProductVersion;
  end;

  EJclFileVersionInfoError = class (EJclError);

function OSIdentToString(const OSIdent: DWORD): string;
function VersionResourceAvailable(const FileName: string): Boolean;

//------------------------------------------------------------------------------
// Streams
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

{ Exceptions }

  EJclPathError = class (EJclError);
  EJclFileUtilsError = class (EJclError);
  EJclTempFileStreamError = class (EJclWin32Error);
  EJclFileMappingError = class (EJclWin32Error);
  EJclFileMappingViewError = class (EJclWin32Error);

implementation

uses
  {$IFDEF WIN32}
  ActiveX, ShellApi, ShlObj,
  {$ENDIF}
  JclResources, JclSecurity, JclSysUtils, JclWin32, JclStrings;

{ Some general notes. This unit redeclares some function from FileCtrl.pas to
  avoid a dependeny on that unit in the JCL. The problem is that FileCtrl.pas
  uses some units (eg Forms.pas) which have ridiculous initialization requirements.
  They add 4KB to the executable and roughly 1 second of startup. That
  initialization is only necessary for GUI applications and is unacceptable for
  services or console apps. }

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
  View := TJclFileMappingView.Create(Self, Access, Count, Offset);
  Result := View.Index;
end;

//------------------------------------------------------------------------------

function TJclCustomFileMapping.AddAt(const Access, Count: Cardinal;
  const Offset: Int64; const Address: Pointer): Integer;
var
  View: TJclFileMappingView;
begin
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
// Path manipulation
//==============================================================================

function PathAddSeparator(const Path: string): string;
var
  L: Integer;
begin
  Result := Path;
  L := Length(Path);
  if (L = 0) or (Path[L] <> PathSeparator) then
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
      // separating them
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
  {$ENDIF}
  {$IFDEF WIN32}
  // Remember, Win32 only allows 'a' to 'z' as drive letters (mapped to 0..25)
  if Drive < 26 then
    Result := Char(Drive + 65) + ':\'
  else
    raise EJclPathError.CreateResRecFmt(@RsPathInvalidDrive, [IntToStr(Drive)]);
  {$ENDIF}
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
  const Width: Integer; CmpFmt: Boolean): string;
const
  Compacts: array [Boolean] of Cardinal = (DT_END_ELLIPSIS, DT_PATH_ELLIPSIS);
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

function PathCompactPath(const Canvas: TCanvas; const Path: string; const Width: Integer;
  CmpFmt: Boolean {$IFDEF SUPPORTS_DEFAULTPARAMS} = True {$ENDIF}): string; overload;
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
  // TODO empty result string on failure is inconsistent with PathGetLongName
  Result := '';
  Required := GetShortPathName(PChar(Path), nil, 0);
  if Required <> 0 then
  begin
    SetLength(Result, Required);
    Required := GetShortPathName(PChar(Path), PChar(Result), Required);
    SetLength(Result, Required);
  end;
end;

//------------------------------------------------------------------------------

function PathIsAbsolute(const Path: string): Boolean;
{$IFDEF WIN32}
var
  I: Integer;
{$ENDIF}
begin
  Result := False;
  if Path <> '' then
  begin
    {$IFDEF LINUX}
    Result := (Path[1] = PathSeparator);
    {$ENDIF}
    {$IFDEF WIN32}
    I := 0;
    if PathIsUnc(Path) then
      I := Length(PathUncPrefix)
    else
    if PathIsDiskDevice(Path) then
      I := Length(PathDevicePrefix);
    Result := (Length(Path) > I + 2) and (Path[I + 1] in DriveLetters) and
      (Path[I + 2] = ':') and (Path[I + 3] = PathSeparator);
    {$ENDIF}
  end;
end;

//------------------------------------------------------------------------------

function PathIsChild(const Path, Base: AnsiString): Boolean;
var
  L: Integer;
begin
  Result := False;
  // an empty path or one that's shorter then base cannot be a subdirectory
  L := Length(Base);
  if (Path = '') or (L > Length(Path)) then
    Exit;
  {$IFDEF WIN32}
  Result := AnsiSameText(StrLeft(Path, L), Base);
  {$ELSE}
  Result := AnsiSameStr(StrLeft(Path, L), Base);
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function PathIsDiskDevice(const Path: string): Boolean;
begin
  {$IFDEF LINUX}
  NotImplemented('PathIsDiskDevice'); // TODO
  {$ENDIF}
  {$IFDEF WIN32}
  Result := Copy(Path, 1, Length(PathDevicePrefix)) = PathDevicePrefix;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function PathIsUNC(const Path: string): Boolean;
begin
  {$IFDEF LINUX}
  Result := False;
  {$ENDIF}
  {$IFDEF WIN32}
  // Format of a valid UNC path is: "\\machine\sharename[\filename]"
  Result := Copy(Path, 1, Length(PathUncPrefix)) = PathUncPrefix;
  { TODO further verification tests:
     \\.\whatever is not valid
     \\?\whatever is not valid unless \\?\UNC\sharename[\filename]
     \\[sharename\][filename] is not valid
     \\<x>:[whatever] is not valid
     \\machine\<x>:[<\pathname>|<\drivename>] is not valid
     \\machine\<x>$[<\pathname>|<\drivename>] is not valid _is_ valid }
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function PathRemoveSeparator(const Path: string): string;
var
  L: Integer;
begin
  L := Length(Path);
  if (L > 0) and (Path[L] = PathSeparator) then
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
  Result := Cardinal(R) <> INVALID_HANDLE_VALUE;
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

function CloseVolume(const Volume: THandle): Boolean;
begin
  Result := CloseHandle(Volume);
end;

//------------------------------------------------------------------------------

function DelTree(const Path: string): Boolean;
begin
  Result := DelTreeEx(Path, False, nil);
end;

//------------------------------------------------------------------------------

function DelTreeEx(Path: string; AbortOnFailure: Boolean; Progress: TDelTreeProgress): Boolean;
var
  Files: TStrings;
  FileName: string;
  I: Integer;
  PartialResult: Boolean;
  Attr: DWORD;
begin

  Result := True;
  Files := TStringList.Create;
  try
    // TODO Path parameter used as local variable: bad practise   
    Path := PathRemoveSeparator(Path);
    BuildFileList(Path + '\*.*', faAnyFile, Files);
    for I := 0 to Files.Count - 1 do
    begin
      FileName := Path + '\' + Files[I];
      PartialResult := True;
      //if IsDirectory(FileName) then
      //  PartialResult := DelTreeEx(FileName, AbortOnFailure, Progress)
      Attr := GetFileAttributes(PChar(FileName));
      if (Attr <> DWORD(-1)) and ((Attr and FILE_ATTRIBUTE_DIRECTORY) <> 0) then
        PartialResult := DelTreeEx(FileName, AbortOnFailure, Progress)
      else
      begin
        if Assigned(Progress) then
          PartialResult := Progress(FileName, Attr);
        if PartialResult then
        begin
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
    Result := SetFileAttributes(PChar(Path), FILE_ATTRIBUTE_NORMAL);
    if Result then
    begin
      {$I-}
      RmDir(Path);
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

function DiskInDrive(Drive: Char): Boolean;
var
  ErrorMode: Cardinal;
begin
  Result := False;
  if Drive in ['a'..'z'] then
    Dec(Drive, $20);
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
    Result := TrimLeft(UpperCase(Result) + ' File'); // TODO Localize
  end;
end;

//------------------------------------------------------------------------------

resourcestring
  RsCannotCreateDir = 'Unable to create directory'; // TODO move to Resources

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
    //0, 1: { handled by else }
    //  Result := RsUnknownDrive;
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
    RaiseLastWin32Error;
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
    RaiseLastWin32Error;
end;

//------------------------------------------------------------------------------

function GetSizeOfFile(Handle: THandle): Int64;
var
  Size: TULargeInteger absolute Result;
begin
  Size.LowPart := GetFileSize(Handle, @Size.HighPart);
end;

//------------------------------------------------------------------------------

resourcestring // TODO MOVE TO RESOURCES
  RsFileUtilsAttrUnavailable = 'Unable to retrieve attributes of %s';

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

function OpenVolume(const Drive: Char): DWORD;
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

function SetFileTimesHelper(const FileName: string; const DateTime: TDateTime;
  Times: TFileTimes): Boolean;
var
  Handle: THandle;
  FileTime: TFileTime;
  SystemTime: TSystemTime;
begin
  Result := False;
  Handle := CreateFile(PChar(FileName), GENERIC_WRITE, FILE_SHARE_READ, nil,
    OPEN_EXISTING, 0, 0);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    DateTimeToSystemTime(DateTime, SystemTime);
    SystemTimeToFileTime(SystemTime, FileTime);
    case Times of
      ftLastAccess:
        Result := SetFileTime(Handle, nil, @FileTime, nil);
      ftLastWrite:
        Result := SetFileTime(Handle, nil, nil, @FileTime);
      ftCreation:
        Result := SetFileTime(Handle, @FileTime, nil, nil);
    end;
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
  Result := PrivilegeEnabled(SE_BACKUP_NAME) and PrivilegeEnabled(SE_RESTORE_NAME);
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
    begin
      DateTimeToSystemTime(DateTime, SystemTime);
      SystemTimeToFileTime(SystemTime, FileTime);
      case Times of
        ftLastAccess:
          Result := SetFileTime(Handle, nil, @FileTime, nil);
        ftLastWrite:
          Result := SetFileTime(Handle, nil, nil, @FileTime);
        ftCreation:
          Result := SetFileTime(Handle, @FileTime, nil, nil);
      end;
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

function UnLockVolume(var Handle: THandle): Boolean;
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
// TFileVersionInfo
//==============================================================================

const
  VerFixedInfo:   PChar = '\';
  VerTranslation: PChar = '\VarFileInfo\Translation';
  VerStringInfo:  PChar = '\StringFileInfo\';

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

type
  PLangIdRec = ^TLangIdRec;
  TLangIdRec = packed record
    case Integer of
    0: (
      LangId: Word;
      CodePage: Word);
    1: (
      Pair: DWORD);
  end;

procedure TJclFileVersionInfo.ExtractLanguageIds;
const
  DefaultLangId = $0409;  // English (US)
  DefaultCodePage = $04E4;
var
  Translation: PLongint;
  I: Integer;
  Lang: TLangIdRec;
  Size: ULONG;
  NeutralLang: Boolean;
  LangString: string;

  procedure AddLang;
  begin
    with Lang do
      FLanguages.AddObject(Format('%.4x%.4x', [LangId, CodePage]), TObject(Pair));
  end;

begin
  NeutralLang := False;
  if VerQueryValue(PChar(FBuffer), PChar(VerTranslation), Pointer(Translation), Size) then
  begin
    for I := 0 to (Size div 4) - 1 do
    begin
      Lang := PLangIdRec(Longint(Translation) + (I * 4))^;
      if Lang.LangId = LANG_NEUTRAL then
        NeutralLang := True
      else
        AddLang;
    end;
  end;
  if NeutralLang then
  begin
    // 'Neutral language' usually doesn't match the value readed before. We have
    // to try search it using StringFileInfo key.
    I := Pos('StringFileInfo', FBuffer);
    if I > 0 then
    begin
      LangString := Copy(FBuffer, I + 20, 8);
      Lang.LangId := StrToIntDef('$' + Copy(LangString, 1, 4), DefaultLangId);
      Lang.CodePage := StrToIntDef('$' + Copy(LangString, 5, 4), DefaultCodePage);
      AddLang;
    end;
  end;
  if FLanguages.Count = 0 then
  begin
    Lang.LangId := DefaultLangId;
    Lang.CodePage := DefaultCodePage;
    AddLang;
  end;
end;

//------------------------------------------------------------------------------

constructor TJclFileVersionInfo.Attach(const Buffer: string);
var
  Size, Masked: ULONG;
begin
  FFileName := '';
  FFileFlags := [];
  FLanguages := TStringList.Create;
  FBuffer := Buffer;
  Win32Check(VerQueryValue(PChar(FBuffer), VerFixedInfo, Pointer(FFixedBuffer), Size));
  if Size < SizeOf(TVSFixedFileInfo) then
    RaiseLastWin32Error;
  ExtractLanguageIds;
  Masked := FFixedBuffer^.dwFileFlags and FFixedBuffer^.dwFileFlagsMask;
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

constructor TJclFileVersionInfo.Create(const AFileName: string);
var
  Size, Masked: ULONG;
begin
  FFileName := AFileName;
  FFileFlags := [];
  FLanguages := TStringList.Create;
  GetVersionInfo;
  Win32Check(VerQueryValue(PChar(FBuffer), VerFixedInfo, Pointer(FFixedBuffer), Size));
  if Size < SizeOf(TVSFixedFileInfo) then
    RaiseLastWin32Error;
  ExtractLanguageIds;
  Masked := FFixedBuffer^.dwFileFlags and FFixedBuffer^.dwFileFlagsMask;
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

destructor TJclFileVersionInfo.Destroy;
begin
  FreeAndNil(FLanguages);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TJclFileVersionInfo.GetBinFileVersion: string;
begin
  with FFixedBuffer^ do
    Result := Format('%u.%u.%u.%u', [HiWord(dwFileVersionMS),
      LoWord(dwFileVersionMS), HiWord(dwFileVersionLS), LoWord(dwFileVersionLS)]);
end;

//------------------------------------------------------------------------------

function TJclFileVersionInfo.GetBinProductVersion: string;
begin
  with FFixedBuffer^ do
    Result := Format('%u.%u.%u.%u', [HiWord(dwProductVersionMS),
      LoWord(dwProductVersionMS), HiWord(dwProductVersionLS),
      LoWord(dwProductVersionLS)]);
end;

//------------------------------------------------------------------------------

function TJclFileVersionInfo.GetFileOS: DWORD;
begin
  Result := FFixedBuffer^.dwFileOS;
end;

//------------------------------------------------------------------------------

function TJclFileVersionInfo.GetFileSubType: DWORD;
begin
  Result := FFixedBuffer^.dwFileSubType;
end;

//------------------------------------------------------------------------------

function TJclFileVersionInfo.GetFileType: DWORD;
begin
  Result := FFixedBuffer^.dwFileType;
end;

//------------------------------------------------------------------------------

function TJclFileVersionInfo.GetLanguageCount: Integer;
begin
  Result := FLanguages.Count;
end;

//------------------------------------------------------------------------------

function TJclFileVersionInfo.GetLanguageIds(Index: Integer): string;
begin
  Result := FLanguages[Index];
end;

//------------------------------------------------------------------------------

function TJclFileVersionInfo.GetLanguageNames(Index: Integer): string;
var
  R, D: DWORD;
begin
  SetLength(Result, MAX_PATH);
  D := Integer(Flanguages.Objects[Index]);
  R := VerLanguageName(LoWord(D), PChar(Result), MAX_PATH);
  SetLength(Result, R);
end;

//------------------------------------------------------------------------------

function TJclFileVersionInfo.GetUserKey(const Key: string): string;
var
  P: PChar;
  Size: UINT;
begin
  Result := '';
  if VerQueryValue(PChar(FBuffer), PChar(Format ('%s%s\%s',
       [VerStringInfo, FLanguages[0], Key])), Pointer(P), Size) then
    Result := P;
end;

//------------------------------------------------------------------------------

procedure TJclFileVersionInfo.GetVersionInfo;
var
  Size: DWORD;
  Handle: DWORD;
begin
  Size := GetFileVersionInfoSize(PChar(FFileName), Handle);
  if Size = 0 then
    raise EJclFileVersionInfoError.CreateResRec(@RsFileUtilsNoVersionInfo);
  SetLength(FBuffer, Size);
  Win32Check(GetFileVersionInfo(PChar(FFileName), Handle, Size, PChar(FBuffer)));
end;

//------------------------------------------------------------------------------

function TJclFileVersionInfo.GetVersionKeyValue(Index: Integer): string;
var
  P: PChar;
  Size: UINT;
begin
  Result := '';
  if VerQueryValue(PChar(FBuffer), PChar(Format('%s%s\%s',
    [VerStringInfo, FLanguages[0], VerKeyNames[Index]])),
    Pointer(P), Size) then
    Result := P;
end;

//------------------------------------------------------------------------------

procedure TJclFileVersionInfo.SetLanguageIndex(Value: Integer);
begin
  if (Value >= 0) and (Value < FLanguages.Count) then
    FLanguageIndex := Value
  else
    raise EJclFileVersionInfoError.CreateResRec(@RsFileUtilsLanguageIndex);
end;

end.
