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
{ The Original Code is JclDebug.pas.                                           }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{ Last modified: January 19, 2001                                              }
{                                                                              }
{******************************************************************************}

unit JclDebug;

interface

{$I JCL.INC}

uses
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  Classes, SysUtils,
  {$IFDEF DELPHI5_UP}
  Contnrs,
  {$ENDIF DELPHI5_UP}
  JclBase, JclFileUtils, JclPeImage;

//------------------------------------------------------------------------------
// Crash
//------------------------------------------------------------------------------

function EnableCrashOnCtrlScroll(const Enable: Boolean): Boolean;

//------------------------------------------------------------------------------
// Diagnostics
//------------------------------------------------------------------------------

procedure AssertKindOf(const ClassName: string; const Obj: TObject); overload;
procedure AssertKindOf(const ClassType: TClass; const Obj: TObject); overload;

procedure Trace(const Msg: string);
procedure TraceFmt(const Fmt: string; const Args: array of const);
procedure TraceLoc(const Msg: string);
procedure TraceLocFmt(const Fmt: string; const Args: array of const);

//------------------------------------------------------------------------------
// MAP file abstract parser
//------------------------------------------------------------------------------

type
  TJclMapAddress = record
    Segment: Word;
    Offset: Integer;
  end;

  PJclMapString = PAnsiChar;

  TJclAbstractMapParser = class (TObject)
  private
    FStream: TJclFileMappingStream;
  protected
    FLastUnitName: PJclMapString;
    FLastUnitFileName: PJclMapString;
    procedure ClassTableItem(const Address: TJclMapAddress; Len: Integer; SectionName, GroupName: PJclMapString); virtual; abstract;
    procedure SegmentItem(const Address: TJclMapAddress; Len: Integer; GroupName, UnitName: PJclMapString); virtual; abstract;
    procedure PublicsByNameItem(const Address: TJclMapAddress; Name: PJclMapString); virtual; abstract;
    procedure PublicsByValueItem(const Address: TJclMapAddress; Name: PJclMapString); virtual; abstract;
    procedure LineNumberUnitItem(UnitName, UnitFileName: PJclMapString); virtual; abstract;
    procedure LineNumbersItem(LineNumber: Integer; const Address: TJclMapAddress); virtual; abstract;
  public
    constructor Create(const MapFileName: TFileName); virtual;
    destructor Destroy; override;
    procedure Parse;
    class function MapStringToStr(MapString: PJclMapString): string;
    property Stream: TJclFileMappingStream read FStream;
  end;

//------------------------------------------------------------------------------
// MAP file parser
//------------------------------------------------------------------------------

  TJclMapClassTableEvent = procedure (Sender: TObject; const Address: TJclMapAddress; Len: Integer; const SectionName, GroupName: string) of object;
  TJclMapSegmentEvent = procedure (Sender: TObject; const Address: TJclMapAddress; Len: Integer; const GroupName, UnitName: string) of object;
  TJclMapPublicsEvent = procedure (Sender: TObject; const Address: TJclMapAddress; const Name: string) of object;
  TJclMapLineNumberUnitEvent = procedure (Sender: TObject; const UnitName, UnitFileName: string) of object;
  TJclMapLineNumbersEvent = procedure (Sender: TObject; LineNumber: Integer; const Address: TJclMapAddress) of object;

  TJclMapParser = class (TJclAbstractMapParser)
  private
    FOnClassTable: TJclMapClassTableEvent;
    FOnLineNumbers: TJclMapLineNumbersEvent;
    FOnLineNumberUnit: TJclMapLineNumberUnitEvent;
    FOnPublicsByValue: TJclMapPublicsEvent;
    FOnPublicsByName: TJclMapPublicsEvent;
    FOnSegmentItem: TJclMapSegmentEvent;
  protected
    procedure ClassTableItem(const Address: TJclMapAddress; Len: Integer; SectionName, GroupName: PJclMapString); override;
    procedure SegmentItem(const Address: TJclMapAddress; Len: Integer; GroupName, UnitName: PJclMapString); override;
    procedure PublicsByNameItem(const Address: TJclMapAddress; Name: PJclMapString); override;
    procedure PublicsByValueItem(const Address: TJclMapAddress; Name: PJclMapString); override;
    procedure LineNumberUnitItem(UnitName, UnitFileName: PJclMapString); override;
    procedure LineNumbersItem(LineNumber: Integer; const Address: TJclMapAddress); override;
  public
    property OnClassTable: TJclMapClassTableEvent read FOnClassTable write FOnClassTable;
    property OnSegment: TJclMapSegmentEvent read FOnSegmentItem write FOnSegmentItem;
    property OnPublicsByName: TJclMapPublicsEvent read FOnPublicsByName write FOnPublicsByName;
    property OnPublicsByValue: TJclMapPublicsEvent read FOnPublicsByValue write FOnPublicsByValue;
    property OnLineNumberUnit: TJclMapLineNumberUnitEvent read FOnLineNumberUnit write FOnLineNumberUnit;
    property OnLineNumbers: TJclMapLineNumbersEvent read FOnLineNumbers write FOnLineNumbers;
  end;

//------------------------------------------------------------------------------
// MAP file scanner
//------------------------------------------------------------------------------

  TJclMapSegment = record
    StartAddr: DWORD;
    EndAddr: DWORD;
    UnitName: PJclMapString;
  end;

  TJclMapProcName = record
    Addr: DWORD;
    ProcName: PJclMapString;
  end;

  TJclMapLineNumber = record
    Addr: DWORD;
    LineNumber: Integer;
  end;

  TJclMapScanner = class (TJclAbstractMapParser)
  private
    FLineNumbers: array of TJclMapLineNumber;
    FProcNames: array of TJclMapProcName;
    FSegments: array of TJclMapSegment;
    FSourceNames: array of TJclMapProcName;
    FLineNumbersCnt: Integer;
    FProcNamesCnt: Integer;
    FNewUnitFileName: PJclMapString;
  protected
    procedure ClassTableItem(const Address: TJclMapAddress; Len: Integer; SectionName, GroupName: PJclMapString); override;
    procedure SegmentItem(const Address: TJclMapAddress; Len: Integer; GroupName, UnitName: PJclMapString); override;
    procedure PublicsByNameItem(const Address: TJclMapAddress; Name: PJclMapString); override;
    procedure PublicsByValueItem(const Address: TJclMapAddress; Name: PJclMapString); override;
    procedure LineNumbersItem(LineNumber: Integer; const Address: TJclMapAddress); override;
    procedure LineNumberUnitItem(UnitName, UnitFileName: PJclMapString); override;
    procedure Scan;
  public
    constructor Create(const MapFileName: TFileName); override;
    function LineNumberFromAddr(Addr: DWORD): Integer;
    function ModuleNameFromAddr(Addr: DWORD): string;
    function ProcNameFromAddr(Addr: DWORD): string;
    function SourceNameFromAddr(Addr: DWORD): string;
  end;

//------------------------------------------------------------------------------
// JCL binary debug data generator and scanner
//------------------------------------------------------------------------------

const
  JclDbgDataSignature = $4742444A; // JDBG
  JclDbgDataResName = 'JCLDEBUG';

type
  PJclDbgHeader = ^TJclDbgHeader;
  TJclDbgHeader = packed record
    Signature: DWORD;
    Version: Byte;
    Units: Integer;
    SourceNames: Integer;
    Symbols: Integer;
    LineNumbers: Integer;
    Words: Integer;
  end;

  TJclBinDebugGenerator = class (TJclMapScanner)
  private
    FDataStream: TMemoryStream;
  protected
    procedure CreateData;
  public
    constructor Create(const MapFileName: TFileName); override;
    destructor Destroy; override;
    property DataStream: TMemoryStream read FDataStream;
  end;

  TJclBinDbgNameCache = record
    Addr: Integer;
    FirstWord: Integer;
    SecondWord: Integer;
  end;

  TJclBinDebugScanner = class (TObject)
  private
    FCacheData: Boolean;
    FStream: TCustomMemoryStream;
    FValidFormat: Boolean;
    FLineNumbers: array of TJclMapLineNumber;
    FProcNames: array of TJclBinDbgNameCache;
  protected
    procedure CacheLineNumbers;
    procedure CacheProcNames;
    procedure CheckFormat;
    function DataToStr(A: Integer): string;
    function MakePtr(A: Integer): Pointer;
    function ReadValue(var P: Pointer; var Value: Integer): Boolean;
  public
    constructor Create(AStream: TCustomMemoryStream; CacheData: Boolean);
    function LineNumberFromAddr(Addr: Integer): Integer;
    function ProcNameFromAddr(Addr: Integer): string;
    function ModuleNameFromAddr(Addr: Integer): string;
    function SourceNameFromAddr(Addr: Integer): string;
    property ValidFormat: Boolean read FValidFormat;
  end;

//------------------------------------------------------------------------------
// Source Locations
//------------------------------------------------------------------------------

  TJclDebugInfoSource = class;

  TJclLocationInfo = record
    Address: Pointer;               // Error adderss
    UnitName: string;               // Name of Delphi unit
    ProcedureName: string;          // Procedure name
    LineNumber: Integer;            // Line number
    SourceName: string;             // Module file name
    DebugInfo: TJclDebugInfoSource; // Location object
  end;

  TJclDebugInfoSource = class (TObject)
  private
    FModule: HMODULE;
    function GetFileName: TFileName;
  protected
    function InitializeSource: Boolean; virtual; abstract;
    function VAFromAddr(const Addr: Pointer): DWORD;
  public
    constructor Create(AModule: HMODULE); virtual;
    function GetLocationInfo(const Addr: Pointer; var Info: TJclLocationInfo): Boolean; virtual; abstract;
    property Module: HMODULE read FModule;
    property FileName: TFileName read GetFileName;
  end;

  TJclDebugInfoSourceClass = class of TJclDebugInfoSource;

  TJclDebugInfoList = class (TObjectList)
  private
    function GetItemFromModule(const Module: HMODULE): TJclDebugInfoSource;
    function GetItems(Index: Integer): TJclDebugInfoSource;
  protected
    function CreateDebugInfo(const Module: HMODULE): TJclDebugInfoSource;
  public
    function GetLocationInfo(const Addr: Pointer; var Info: TJclLocationInfo): Boolean;
    property ItemFromModule[const Module: HMODULE]: TJclDebugInfoSource read GetItemFromModule;
    property Items[Index: Integer]: TJclDebugInfoSource read GetItems;
  end;

//------------------------------------------------------------------------------
// Various source location implementations
//------------------------------------------------------------------------------

  TJclDebugInfoMap = class (TJclDebugInfoSource)
  private
    FScanner: TJclMapScanner;
  protected
    function InitializeSource: Boolean; override;
  public
    destructor Destroy; override;
    function GetLocationInfo(const Addr: Pointer; var Info: TJclLocationInfo): Boolean; override;
  end;

  TJclDebugInfoBinary = class (TJclDebugInfoSource)
  private
    FScanner: TJclBinDebugScanner;
    FStream: TCustomMemoryStream;
  protected
    function InitializeSource: Boolean; override;
  public
    destructor Destroy; override;
    function GetLocationInfo(const Addr: Pointer; var Info: TJclLocationInfo): Boolean; override;
  end;

  TJclDebugInfoExports = class (TJclDebugInfoSource)
  private
    FBorImage: TJclPeBorImage;
  protected
    function InitializeSource: Boolean; override;
  public
    destructor Destroy; override;
    function GetLocationInfo(const Addr: Pointer; var Info: TJclLocationInfo): Boolean; override;
  end;

//------------------------------------------------------------------------------
// Source location functions
//------------------------------------------------------------------------------

function ModuleFromAddr(const Addr: Pointer): HMODULE;
function IsSystemModule(const Module: HMODULE): Boolean;

function Caller(Level: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF}): Pointer;

function GetLocationInfo(const Addr: Pointer): TJclLocationInfo;
function GetLocationInfoStr(const Addr: Pointer): string;
procedure ClearLocationData;

function __FILE__(const Level: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF}): string;
function __MODULE__(const Level: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF}): string;
function __PROC__(const Level: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF}): string;
function __LINE__(const Level: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF}): Integer;
function __MAP__(const Level: Integer; var _File, _Module, _Proc: string; var _Line: Integer): Boolean;

function __FILE_OF_ADDR__(const Addr: Pointer): string;
function __MODULE_OF_ADDR__(const Addr: Pointer): string;
function __PROC_OF_ADDR__(const Addr: Pointer): string;
function __LINE_OF_ADDR__(const Addr: Pointer): Integer;
function __MAP_OF_ADDR__(const Addr: Pointer; var _File, _Module, _Proc: string;
  var _Line: Integer): Boolean;


//------------------------------------------------------------------------------
// Stack info routines
//------------------------------------------------------------------------------

{ TODO -cDOC : Hallvard Vassbotn }

type
  PDWORDArray = ^TDWORDArray;
  TDWORDArray = array[0..(MaxInt - $F) div SizeOf(DWORD)] of DWORD;

  PStackFrame = ^TStackFrame;
  TStackFrame = record
    CallersEBP: DWORD;
    CallerAdr: DWORD;
  end;

  TStackInfo = record
    CallerAdr: DWORD;
    Level: DWORD;
    CallersEBP: DWORD;
    DumpSize: DWORD;
    ParamSize: DWORD;
    ParamPtr: PDWORDArray;
    case Integer of
      0: (StackFrame: PStackFrame);
      1: (DumpPtr: PByteArray);
  end;

  TJclStackInfoItem = class (TObject)
  private
    FStackInfo: TStackInfo;
    function GetLogicalAddress: DWORD;
  public
    property LogicalAddress: DWORD read GetLogicalAddress;
    property StackInfo: TStackInfo read FStackInfo;
  end;

  TJclStackInfoList = class (TObjectList)
  private
    FIgnoreLevels: DWORD;
    FThreadID: DWORD;
    FTimeStamp: TDateTime;
    function GetItems(Index: Integer): TJclStackInfoItem;
  public
    constructor Create(Raw: Boolean; AIgnoreLevels: DWORD; FirstCaller: Pointer);
    procedure AddToStrings(Strings: TStrings);
    property Items[Index: Integer]: TJclStackInfoItem read GetItems; default;
    property IgnoreLevels: DWORD read FIgnoreLevels;
    property ThreadID: DWORD read FThreadID;
    property TimeStamp: TDateTime read FTimeStamp;
  end;

function JclCreateStackList(Raw: Boolean; AIgnoreLevels: Integer; FirstCaller: Pointer): TJclStackInfoList;
function JclLastExceptStackList: TJclStackInfoList;

//------------------------------------------------------------------------------
// Stack frame info routines
//------------------------------------------------------------------------------

{ TODO -cDOC : Marcel Bestebroer }

type
  JmpInstruction = packed record // from System.pas
    opCode: Byte;
    distance: Longint;
  end;

  TExcDescEntry = record // from System.pas
    vTable: Pointer;
    handler: Pointer;
  end;

  PExcDesc = ^TExcDesc;
  TExcDesc = packed record // from System.pas
    jmp: JmpInstruction;
    case Integer of
      0:      (instructions: array [0..0] of Byte);
      1{...}: (cnt: Integer; excTab: array [0..0{cnt-1}] of TExcDescEntry);
  end;

  PExcFrame = ^TExcFrame;
  TExcFrame =  record // from System.pas
    next: PExcFrame;
    desc: PExcDesc;
    hEBP: Pointer;
    case Integer of
    0:  ( );
    1:  ( ConstructedObject: Pointer );
    2:  ( SelfOfMethod: Pointer );
  end;

  TExcFrameType = (eftUnknown, eftFinally, eftAnyException, eftOnException,
    eftAutoException);

  TJclExceptFrame = class(TObject)
  private
    FExcFrame: PExcFrame;
    FFrameType: TExcFrameType;
  protected
    procedure DoDetermineFrameType;
  public
    constructor Create(AExcFrame: PExcFrame);
    function Handles(ExceptObj: TObject): Boolean;
    function HandlerInfo(ExceptObj: TObject; var HandlerAt: Pointer): Boolean;
    property ExcFrame: PExcFrame read FExcFrame;
    property FrameType: TExcFrameType read FFrameType;
  end;

  TJclExceptFrameList = class(TObjectList)
  private
    FIgnoreLevels: Integer;
    function GetItems(Index: Integer): TJclExceptFrame;
  protected
    function AddFrame(AFrame: PExcFrame): TJclExceptFrame;
  public
    constructor Create(AIgnoreLevels: Integer);
    procedure TraceExceptionFrames;
    property Items[Index: Integer]: TJclExceptFrame read GetItems;
    property IgnoreLevels: Integer read FIgnoreLevels write FIgnoreLevels;
  end;

function JclCreateExceptFrameList(AIgnoreLevels: Integer): TJclExceptFrameList;
function JclLastExceptFrameList: TJclExceptFrameList;

//------------------------------------------------------------------------------
// Exception hooking
//------------------------------------------------------------------------------

{ TODO -cDOC : Hallvard Vassbotn }

type
  TJclExceptNotifyProc = procedure (ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean);
  TJclExceptNotifyMethod = procedure (ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean) of object;

function JclHookExceptions: Boolean;
function JclUnhookExceptions: Boolean;
function JclExceptionsHooked: Boolean;

var
  ExceptNotifyProc: TJclExceptNotifyProc;
  ExceptNotifyMethod: TJclExceptNotifyMethod;

//------------------------------------------------------------------------------
// Global exceptional stack trackers enable variables
//------------------------------------------------------------------------------

var
  StackTrackingEnable: Boolean;
  RawStackTracking: Boolean;
  ExceptionFrameTrackingEnable: Boolean;

implementation

uses
  JclRegistry, JclStrings, JclSysUtils;

{$UNDEF StackFramesWasOn}
{$IFOPT W+}
  {$DEFINE StackFramesWasOn}
{$ENDIF}

//==============================================================================
// Crash
//==============================================================================

function EnableCrashOnCtrlScroll(const Enable: Boolean): Boolean;
const
  CrashCtrlScrollKey = 'System\CurrentControlSet\Services\i8042prt\Parameters';
  CrashCtrlScrollName = 'CrashOnCtrlScroll';
var
  Enabled: Integer;
begin
  Enabled := 0;
  if Enable then
    Enabled := 1;
  RegWriteInteger(HKEY_LOCAL_MACHINE, CrashCtrlScrollKey, CrashCtrlScrollName, Enabled);
  Result := RegReadInteger(HKEY_LOCAL_MACHINE, CrashCtrlScrollKey, CrashCtrlScrollName) = Enabled;
end;

//==============================================================================
// Diagnostics
//==============================================================================

procedure AssertKindOf(const ClassName: string; const Obj: TObject); overload;
var
  C: TClass;
begin
  if not Obj.ClassNameIs(ClassName) then
  begin
    C := Obj.ClassParent;
    while (C <> nil) and (not C.ClassNameIs(ClassName)) do
      C := C.ClassParent;
    Assert(C <> nil);
  end;
end;

//------------------------------------------------------------------------------

procedure AssertKindOf(const ClassType: TClass; const Obj: TObject); overload;
begin
  Assert(Obj.InheritsFrom(ClassType));
end;

//------------------------------------------------------------------------------

procedure Trace(const Msg: string);
begin
  OutputDebugString(PChar('"' + Msg + '"'));
end;

//------------------------------------------------------------------------------

procedure TraceFmt(const Fmt: string; const Args: array of const);
begin
  OutputDebugString(PChar(Format('"' + Fmt + '"', Args)));
end;

//------------------------------------------------------------------------------

procedure TraceLoc(const Msg: string);
begin
  OutputDebugString(PChar(Format('%s:%u (%s) "%s"', [__FILE__(1), __LINE__(1),
    __PROC__(1), Msg])));
end;

//------------------------------------------------------------------------------

procedure TraceLocFmt(const Fmt: string; const Args: array of const);
var
  S: string;
begin
  S := Format('%s:%u (%s) ', [__FILE__(1), __LINE__(1), __PROC__(1)]) +
    Format('"' + Fmt + '"', Args);
  OutputDebugString(PChar(S));
end;

//==============================================================================
// TJclAbstractMapParser
//==============================================================================

constructor TJclAbstractMapParser.Create(const MapFileName: TFileName);
begin
  if FileExists(MapFileName) then
    FStream := TJclFileMappingStream.Create(MapFileName);
end;

//------------------------------------------------------------------------------

destructor TJclAbstractMapParser.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;

//------------------------------------------------------------------------------

class function TJclAbstractMapParser.MapStringToStr(MapString: PJclMapString): string;
var
  P: PChar;
begin
  if MapString = nil then
  begin
    Result := '';
    Exit;
  end;
  if MapString^ = '(' then
  begin
    Inc(MapString);
    P := MapString;
    while not (P^ in [AnsiCarriageReturn, ')']) do
      Inc(P);
  end else
  begin
    P := MapString;
    while not (P^ in [AnsiSpace, AnsiCarriageReturn, '(']) do
      Inc(P);
  end;
  SetString(Result, MapString, P - MapString);
end;

//------------------------------------------------------------------------------

procedure TJclAbstractMapParser.Parse;
const
  TableHeader          = 'Start         Length     Name                   Class';
  SegmentsHeader       = 'Detailed map of segments';
  PublicsByNameHeader  = 'Address         Publics by Name';
  PublicsByValueHeader = 'Address         Publics by Value';
  LineNumbersPrefix    = 'Line numbers for';
  ResourceFilesHeader  = 'Bound resource files';
var
  CurrPos, EndPos: PChar;
  A: TJclMapAddress;
  L: Integer;
  P1, P2: PJclMapString;

  procedure SkipWhiteSpace;
  begin
    while CurrPos^ in AnsiWhiteSpace do
      Inc(CurrPos);
  end;

  procedure SkipEndLine;
  begin
    while CurrPos^ <> AnsiLineFeed do
      Inc(CurrPos);
    SkipWhiteSpace;
  end;

  function Eof: Boolean;
  begin
    Result := (CurrPos >= EndPos);
  end;

  function IsDecDigit: Boolean;
  begin
    Result := CurrPos^ in AnsiDecDigits;
  end;

  function ReadTextLine: string;
  var
    P: PChar;
  begin
    P := CurrPos;
    while not (CurrPos^ in [AnsiCarriageReturn, AnsiNull]) do
      Inc(CurrPos);
    SetString(Result, P, CurrPos - P);
  end;

  function ReadDecValue: Integer;
  begin
    Result := 0;
    while CurrPos^ in AnsiDecDigits do
    begin
      Result := Result * 10 + (Ord(CurrPos^) - Ord('0'));
      Inc(CurrPos);
    end;
  end;

  function ReadHexValue: Integer;
  var
    C: Char;
  begin
    Result := 0;
    repeat
      C := CurrPos^;
      case C of
        '0'..'9':
          begin
            Result := Result * 16;
            Inc(Result, Ord(C) - Ord('0'));
          end;
        'A'..'F':
          begin
            Result := Result * 16;
            Inc(Result, Ord(C) - Ord('A') + 10);
          end;
        'H':
          begin
            Inc(CurrPos);
            Break;
          end;
      else
        Break;
      end;
      Inc(CurrPos);
    until False;
  end;

  function ReadAddress: TJclMapAddress;
  begin
    Result.Segment := ReadHexValue;
    if CurrPos^ = ':' then
    begin
      Inc(CurrPos);
      Result.Offset := ReadHexValue;
    end else
      Result.Offset := 0;
  end;

  function ReadString: PJclMapString;
  begin
    SkipWhiteSpace;
    Result := CurrPos;
    while not (CurrPos^ in AnsiWhiteSpace) do
      Inc(CurrPos);
  end;

  procedure FindParam(Param: Char);
  begin
    while not ((CurrPos^ = Param) and ((CurrPos + 1)^ = '=')) do
      Inc(CurrPos);
    Inc(CurrPos, 2);
  end;

  function SyncToHeader(const Header: string): Boolean;
  var
    S: string;
  begin
    Result := False;
    while not Eof do
    begin
      S := Trim(ReadTextLine);
      Result := Pos(Header, S) = 1;
      if Result then
        Break;
      SkipEndLine;
    end;
    if not Eof then
      SkipWhiteSpace;
  end;

  function SyncToPrefix(const Prefix: string): Boolean;
  var
    I: Integer;
    P: PChar;
    S: string;
  begin
    if Eof then
    begin
      Result := False;
      Exit;
    end;
    SkipWhiteSpace;
    I := Length(Prefix);
    P := CurrPos;
    while not Eof and (not (P^ in [AnsiCarriageReturn, AnsiNull])) and (I > 0) do
    begin
      Inc(P);
      Dec(I);
    end;
    SetString(S, CurrPos, Length(Prefix));
    Result := (S = Prefix);
    if Result then
      CurrPos := P;
    SkipWhiteSpace;
  end;

begin
  if FStream = nil then
    Exit;
  CurrPos := FStream.Memory;
  EndPos := CurrPos + FStream.Size;
  if SyncToHeader(TableHeader) then
    while IsDecDigit do
    begin
      A := ReadAddress;
      SkipWhiteSpace;
      L := ReadHexValue;
      P1 := ReadString;
      P2 := ReadString;
      SkipEndLine;
      ClassTableItem(A, L, P1, P2);
    end;
  if SyncToHeader(SegmentsHeader) then
    while IsDecDigit do
    begin
      A := ReadAddress;
      SkipWhiteSpace;
      L := ReadHexValue;
      FindParam('C');
      P1 := ReadString;
      FindParam('M');
      P2 := ReadString;
      SkipEndLine;
      SegmentItem(A, L, P1, P2);
    end;
  if SyncToHeader(PublicsByNameHeader) then
    while IsDecDigit do
    begin
      A := ReadAddress;
      P1 := ReadString;
      SkipWhiteSpace;
      PublicsByNameItem(A, P1);
    end;
  if SyncToHeader(PublicsByValueHeader) then
    while IsDecDigit do
    begin
      A := ReadAddress;
      P1 := ReadString;
      SkipWhiteSpace;
      PublicsByValueItem(A, P1);
    end;
  while SyncToPrefix(LineNumbersPrefix) do
  begin
    FLastUnitName := CurrPos;
    FLastUnitFileName := CurrPos;
    while FLastUnitFileName^ <> '(' do
      Inc(FLastUnitFileName);
    SkipEndLine;
    LineNumberUnitItem(FLastUnitName, FLastUnitFileName);
    repeat
      SkipWhiteSpace;
      L := ReadDecValue;
      SkipWhiteSpace;
      A := ReadAddress;
      SkipWhiteSpace;
      LineNumbersItem(L, A);
    until not IsDecDigit;
  end;
end;

//==============================================================================
// TJclMapParser
//==============================================================================

procedure TJclMapParser.ClassTableItem(const Address: TJclMapAddress;
  Len: Integer; SectionName, GroupName: PJclMapString);
begin
  if Assigned(FOnClassTable) then
    FOnClassTable(Self, Address, Len, MapStringToStr(SectionName),
    MapStringToStr(GroupName));
end;

//------------------------------------------------------------------------------

procedure TJclMapParser.LineNumbersItem(LineNumber: Integer; const Address: TJclMapAddress);
begin
  if Assigned(FOnLineNumbers) then
    FOnLineNumbers(Self, LineNumber, Address);
end;

//------------------------------------------------------------------------------

procedure TJclMapParser.LineNumberUnitItem(UnitName, UnitFileName: PJclMapString);
begin
  if Assigned(FOnLineNumberUnit) then
    FOnLineNumberUnit(Self, MapStringToStr(UnitName), MapStringToStr(UnitFileName));
end;

//------------------------------------------------------------------------------

procedure TJclMapParser.PublicsByNameItem(const Address: TJclMapAddress;
  Name: PJclMapString);
begin
  if Assigned(FOnPublicsByName) then
    FOnPublicsByName(Self, Address, MapStringToStr(Name));
end;

//------------------------------------------------------------------------------

procedure TJclMapParser.PublicsByValueItem(const Address: TJclMapAddress;
  Name: PJclMapString);
begin
  if Assigned(FOnPublicsByValue) then
    FOnPublicsByValue(Self, Address, MapStringToStr(Name));
end;

//------------------------------------------------------------------------------

procedure TJclMapParser.SegmentItem(const Address: TJclMapAddress;
  Len: Integer; GroupName, UnitName: PJclMapString);
begin
  if Assigned(FOnSegmentItem) then
    FOnSegmentItem(Self, Address, Len, MapStringToStr(GroupName),
    MapStringToStr(UnitName));
end;

//==============================================================================
// TJclMapScanner
//==============================================================================

procedure TJclMapScanner.ClassTableItem(const Address: TJclMapAddress;
  Len: Integer; SectionName, GroupName: PJclMapString);
begin
end;

//------------------------------------------------------------------------------

constructor TJclMapScanner.Create(const MapFileName: TFileName);
begin
  inherited;
  Scan;
end;

//------------------------------------------------------------------------------

function TJclMapScanner.LineNumberFromAddr(Addr: DWORD): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Length(FLineNumbers) - 1 downto 0 do
    if FLineNumbers[I].Addr <= Addr then
    begin
      Result := FLineNumbers[I].LineNumber;
      Break;
    end;
end;

//------------------------------------------------------------------------------

procedure TJclMapScanner.LineNumbersItem(LineNumber: Integer;
  const Address: TJclMapAddress);
var
  C: Integer;
begin
  if FLineNumbersCnt mod 256 = 0 then
    SetLength(FLineNumbers, FLineNumbersCnt + 256);
  FLineNumbers[FLineNumbersCnt].Addr := Address.Offset;
  FLineNumbers[FLineNumbersCnt].LineNumber := LineNumber;
  Inc(FLineNumbersCnt);
  if FNewUnitFileName <> nil then
  begin
    C := Length(FSourceNames);
    SetLength(FSourceNames, C + 1);
    FSourceNames[C].Addr := Address.Offset;
    FSourceNames[C].ProcName := FNewUnitFileName;
    FNewUnitFileName := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclMapScanner.LineNumberUnitItem(UnitName, UnitFileName: PJclMapString);
begin
  FNewUnitFileName := UnitFileName;
end;

//------------------------------------------------------------------------------

function TJclMapScanner.ModuleNameFromAddr(Addr: DWORD): string;
var
  I: Integer;
begin
  Result := '';
  for I := Length(FSegments) - 1 downto 0 do
    if (FSegments[I].StartAddr <= Addr) and (FSegments[I].EndAddr >= Addr) then
    begin
      Result := MapStringToStr(FSegments[I].UnitName);
      Break;
    end;
end;

//------------------------------------------------------------------------------

function TJclMapScanner.ProcNameFromAddr(Addr: DWORD): string;
var
  I: Integer;
begin
  Result := '';
  for I := Length(FProcNames) - 1 downto 0 do
    if FProcNames[I].Addr <= Addr then
    begin
      Result := MapStringToStr(FProcNames[I].ProcName);
      Break;
    end;
end;

//------------------------------------------------------------------------------

procedure TJclMapScanner.PublicsByNameItem(const Address: TJclMapAddress;
  Name: PJclMapString);
begin
end;

//------------------------------------------------------------------------------

procedure TJclMapScanner.PublicsByValueItem(const Address: TJclMapAddress;
  Name: PJclMapString);
begin
  if Address.Segment = 1 then
  begin
    if FProcNamesCnt mod 256 = 0 then
      SetLength(FProcNames, FProcNamesCnt + 256);
    FProcNames[FProcNamesCnt].Addr := Address.Offset;
    FProcNames[FProcNamesCnt].ProcName := Name;
    Inc(FProcNamesCnt);
  end;
end;

//------------------------------------------------------------------------------

procedure TJclMapScanner.Scan;
begin
  Parse;
  SetLength(FLineNumbers, FLineNumbersCnt);
  SetLength(FProcNames, FProcNamesCnt);
end;

//------------------------------------------------------------------------------

procedure TJclMapScanner.SegmentItem(const Address: TJclMapAddress;
  Len: Integer; GroupName, UnitName: PJclMapString);
var
  C: Integer;
begin
  if Address.Segment = 1 then
  begin
    C := Length(FSegments);
    SetLength(FSegments, C + 1);
    FSegments[C].StartAddr := Address.Offset;
    FSegments[C].EndAddr := Address.Offset + Len;
    FSegments[C].UnitName := UnitName;
  end;
end;

//------------------------------------------------------------------------------

function TJclMapScanner.SourceNameFromAddr(Addr: DWORD): string;
var
  I: Integer;
begin
  Result := '';
  for I := Length(FSourceNames) - 1 downto 0 do
    if FSourceNames[I].Addr <= Addr then
    begin
      Result := MapStringToStr(FSourceNames[I].ProcName);
      Break;
    end;
end;

//==============================================================================
// JCL binary debug format string encoding/decoding routines
//==============================================================================

{ Strings are compressed to following 6bit format (A..D represents characters) }
{ and terminated with 6bit #0 char. First char = #1 indicates non compressed   }
{ text.                                                                        }
{                                                                              }
{ 7   6   5   4   3   2   1   0  |                                             }
{---------------------------------                                             }
{ B1  B0  A5  A4  A3  A2  A1  A0 | Data byte 0                                 }
{---------------------------------                                             }
{ C3  C2  C1  C0  B5  B4  B3  B2 | Data byte 1                                 }
{---------------------------------                                             }
{ D5  D4  D3  D2  D1  D0  C5  C4 | Data byte 2                                 }
{---------------------------------                                             }

//------------------------------------------------------------------------------

function DecodeNameString(const S: PChar): string;
var
  I: Integer;
  C: Byte;
  P: PByte;
begin
  Result := '';
  P := PByte(S);
  if P^ = 1 then
  begin
    Inc(P);
    Result := PChar(P);
    Exit;
  end;
  I := 0;
  C := 0;
  repeat
    case I and $03 of
      0: begin
           C := P^ and $3F;
         end;
      1: begin
           C := (P^ shr 6) and $03;
           Inc(P);
           Inc(C, (P^ and $0F) shl 2);
         end;
      2: begin
           C := (P^ shr 4) and $0F;
           Inc(P);
           Inc(C, (P^ and $03) shl 4);
         end;
      3: begin
           C := (P^ shr 2) and $3F;
           Inc(P);
         end;
    end;
    case C of
      $00:
        Break;
      $01..$0A:
        Inc(C, Ord('0') - $01);
      $0B..$24:
        Inc(C, Ord('A') - $0B);
      $25..$3E:
        Inc(C, Ord('a') - $25);
      $3F:
        C := Ord('_');
    end;
    Result := Result + Chr(C);
    Inc(I);
  until False;
end;

//------------------------------------------------------------------------------

function EncodeNameString(const S: string): string;
const
  ValidChars = ['0'..'9', 'A'..'Z', 'a'..'z', '_'];
var
  I: Integer;
  C: Byte;
  P: PByte;
begin
  for I := 1 to Length(S) do
    if not (S[I] in ValidChars) then
    begin
      Result := #1 + S + #0;
      Exit;
    end;
  SetLength(Result, Length(S));
  P := Pointer(Result);
  Dec(P);
  for I := 0 to Length(S) do // including null terminator
  begin
    C := Byte(S[I + 1]);
    case Char(C) of
      #0:
        C := 0;
      '0'..'9':
        Dec(C, Ord('0') - $01);
      'A'..'Z':
        Dec(C, Ord('A') - $0B);
      'a'..'z':
        Dec(C, Ord('a') - $25);
      '_':
        C := $3F;
    else
      C := $3F;
    end;
    case I and $03 of
      0: begin
           Inc(P);
           P^ := C;
         end;
      1: begin
           P^ := P^ or (C and $03) shl 6;
           Inc(P);
           P^ := (C shr 2) and $0F;
         end;
      2: begin
           P^ := P^ or (C shl 4);
           Inc(P);
           P^ := (C shr 4) and $03;
         end;
      3: begin
           P^ := P^ or (C shl 2);
         end;
    end;
  end;
  SetLength(Result, DWORD(P) - DWORD(Pointer(Result)) + 1);
end;

//==============================================================================
// TJclBinDebugGenerator
//==============================================================================

constructor TJclBinDebugGenerator.Create(const MapFileName: TFileName);
begin
  inherited;
  FDataStream := TMemoryStream.Create;
  if FStream <> nil then
    CreateData;
end;

//------------------------------------------------------------------------------

procedure TJclBinDebugGenerator.CreateData;
var
  FileHeader: TJclDbgHeader;
  WordList: TStringList;
  WordStream: TMemoryStream;

  I, D: Integer;
  S: string;
  L1, L2, L3: Integer;
  FirstWord, SecondWord: Integer;

  function AddWord(const S: string): Integer;
  var
    N: Integer;
    E: string;
  begin
    if S = '' then
    begin
      Result := 0;
      Exit;
    end;
    N := WordList.IndexOf(S);
    if N = -1 then
    begin
      Result := WordStream.Position;
      E := EncodeNameString(S);
      WordStream.WriteBuffer(Pointer(E)^, Length(E));
      WordList.AddObject(S, TObject(Result));
    end else
      Result := DWORD(WordList.Objects[N]);
    Inc(Result);
  end;

  procedure WriteValue(Value: Integer);
  var
    L: Integer;
    D: DWORD;
    P: array[1..5] of Byte;
  begin
    D := Value;
    L := 0;
    while D > $7F do
    begin
      Inc(L);
      P[L] := (D and $7F) or $80;
      D := D shr 7;
    end;
    Inc(L);
    P[L] := (D and $7F);
    FDataStream.WriteBuffer(P, L);
  end;

  procedure WriteValueOfs(Value: Integer; var LastValue: Integer);
  begin
    WriteValue(Value - LastValue);
    LastValue := Value;
  end;

begin
  WordStream := TMemoryStream.Create;
  WordList := TStringList.Create;
  try
    WordList.Sorted := True;
    WordList.Duplicates := dupError;

    FileHeader.Signature := JclDbgDataSignature;
    FileHeader.Version := 1;
    FDataStream.WriteBuffer(FileHeader, SizeOf(FileHeader));

    FileHeader.Units := FDataStream.Position;
    L1 := 0;
    L2 := 0;
    for I := 0 to Length(FSegments) - 1 do
    begin
      WriteValueOfs(FSegments[I].StartAddr, L1);
      WriteValueOfs(AddWord(MapStringToStr(FSegments[I].UnitName)), L2);
    end;
    WriteValue(MaxInt);

    FileHeader.SourceNames := FDataStream.Position;
    L1 := 0;
    L2 := 0;
    for I := 0 to Length(FSourceNames) - 1 do
    begin
      WriteValueOfs(FSourceNames[I].Addr, L1);
      WriteValueOfs(AddWord(MapStringToStr(FSourceNames[I].ProcName)), L2);
    end;
    WriteValue(MaxInt);

    FileHeader.Symbols := FDataStream.Position;
    L1 := 0;
    L2 := 0;
    L3 := 0;
    for I := 0 to Length(FProcNames) - 1 do
    begin
      WriteValueOfs(FProcNames[I].Addr, L1);
      S := MapStringToStr(FProcNames[I].ProcName);
      D := Pos('.', S);
      if D = 1 then
      begin
        FirstWord := 0;
        SecondWord := 0;
      end else
      if D = 0 then
      begin
        FirstWord := AddWord(S);
        SecondWord := 0;
      end else
      begin
        FirstWord := AddWord(Copy(S, 1, D - 1));
        SecondWord := AddWord(Copy(S, D + 1, Length(S)));
      end;
      WriteValueOfs(FirstWord, L2);
      WriteValueOfs(SecondWord, L3);
    end;
    WriteValue(MaxInt);

    FileHeader.LineNumbers := FDataStream.Position;
    L1 := 0;
    L2 := 0;
    for I := 0 to Length(FLineNumbers) - 1 do
    begin
      WriteValueOfs(FLineNumbers[I].Addr, L1);
      WriteValueOfs(FLineNumbers[I].LineNumber, L2);
    end;
    WriteValue(MaxInt);

    FileHeader.Words := FDataStream.Position;
    FDataStream.CopyFrom(WordStream, 0);
    FDataStream.Seek(0, soFromBeginning);
    FDataStream.WriteBuffer(FileHeader, SizeOf(FileHeader));
  finally
    WordStream.Free;
    WordList.Free;
  end;
end;

//------------------------------------------------------------------------------

destructor TJclBinDebugGenerator.Destroy;
begin
  FreeAndNil(FDataStream);
  inherited;
end;

//==============================================================================
// TJclBinDebugScanner
//==============================================================================

procedure TJclBinDebugScanner.CacheLineNumbers;
var
  P: Pointer;
  Value, LineNumber, CurrAddr, C: Integer;
begin
  if FLineNumbers = nil then
  begin
    LineNumber := 0;
    CurrAddr := 0;
    C := 0;
    P := MakePtr(PJclDbgHeader(FStream.Memory)^.LineNumbers);
    while ReadValue(P, Value) do
    begin
      Inc(CurrAddr, Value);
      ReadValue(P, Value);
      Inc(LineNumber, Value);
      SetLength(FLineNumbers, C + 1);
      FLineNumbers[C].Addr := CurrAddr;
      FLineNumbers[C].LineNumber := LineNumber;
      Inc(C);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclBinDebugScanner.CacheProcNames;
var
  P: Pointer;
  CurrAddr, Value, FirstWord, SecondWord, C: Integer;
begin
  if FProcNames = nil then
  begin
    FirstWord := 0;
    SecondWord := 0;
    CurrAddr := 0;
    C := 0;
    P := MakePtr(PJclDbgHeader(FStream.Memory)^.Symbols);
    while ReadValue(P, Value) do
    begin
      Inc(CurrAddr, Value);
      ReadValue(P, Value);
      Inc(FirstWord, Value);
      ReadValue(P, Value);
      Inc(SecondWord, Value);
      SetLength(FProcNames, C + 1);
      FProcNames[C].Addr := CurrAddr;
      FProcNames[C].FirstWord := FirstWord;
      FProcNames[C].SecondWord := SecondWord;
      Inc(C);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclBinDebugScanner.CheckFormat;
var
  P: Pointer;
begin
  P := FStream.Memory;
  FValidFormat := (P <> nil) and (FStream.Size > SizeOf(TJclDbgHeader)) and
    (PJclDbgHeader(P)^.Signature = JclDbgDataSignature) and
    (PJclDbgHeader(P)^.Version = 1);
end;

//------------------------------------------------------------------------------

constructor TJclBinDebugScanner.Create(AStream: TCustomMemoryStream; CacheData: Boolean);
begin
  FCacheData := CacheData;
  FStream := AStream;
  CheckFormat;
end;

//------------------------------------------------------------------------------

function TJclBinDebugScanner.DataToStr(A: Integer): string;
var
  P: PChar;
begin
  if A = 0 then
    Result := ''
  else
  begin
    P := PChar(DWORD(A) + DWORD(FStream.Memory) + DWORD(PJclDbgHeader(FStream.Memory)^.Words) - 1);
    Result := DecodeNameString(P);
  end;
end;

//------------------------------------------------------------------------------

function TJclBinDebugScanner.LineNumberFromAddr(Addr: Integer): Integer;
var
  P: Pointer;
  Value, LineNumber, CurrAddr: Integer;
begin
  LineNumber := 0;
  if FCacheData then
  begin
    CacheLineNumbers;
    for Value := Length(FLineNumbers) - 1 downto 0 do
      if Integer(FLineNumbers[Value].Addr) <= Addr then
      begin
        LineNumber := FLineNumbers[Value].LineNumber;
        Break;
      end;
  end
  else
  begin
    P := MakePtr(PJclDbgHeader(FStream.Memory)^.LineNumbers);
    CurrAddr := 0;
    while ReadValue(P, Value) do
    begin
      Inc(CurrAddr, Value);
      if Addr < CurrAddr then
        Break
      else
      begin
        ReadValue(P, Value);
        Inc(LineNumber, Value);
      end;
    end;
  end;
  Result := LineNumber;
end;

//------------------------------------------------------------------------------

function TJclBinDebugScanner.MakePtr(A: Integer): Pointer;
begin
  Result := Pointer(DWORD(FStream.Memory) + DWORD(A));
end;

//------------------------------------------------------------------------------

function TJclBinDebugScanner.ModuleNameFromAddr(Addr: Integer): string;
var
  Value, Name, StartAddr: Integer;
  P: Pointer;
begin
  P := MakePtr(PJclDbgHeader(FStream.Memory)^.Units);
  Name := 0;
  StartAddr := 0;
  while ReadValue(P, Value) do
  begin
    Inc(StartAddr, Value);
    if Addr < StartAddr then
      Break
    else
    begin
      ReadValue(P, Value);
      Inc(Name, Value);
    end;
  end;
  Result := DataToStr(Name);
end;

//------------------------------------------------------------------------------

function TJclBinDebugScanner.ProcNameFromAddr(Addr: Integer): string;
var
  P: Pointer;
  CurrAddr, Value, FirstWord, SecondWord: Integer;
begin
  FirstWord := 0;
  SecondWord := 0;
  if FCacheData then
  begin
    CacheProcNames;
    for Value := Length(FProcNames) - 1 downto 0 do
      if FProcNames[Value].Addr <= Addr then
      begin
        FirstWord := FProcNames[Value].FirstWord;
        SecondWord := FProcNames[Value].SecondWord;
        Break;
      end;
  end else
  begin
    P := MakePtr(PJclDbgHeader(FStream.Memory)^.Symbols);
    CurrAddr := 0;
    while ReadValue(P, Value) do
    begin
      Inc(CurrAddr, Value);
      if Addr < CurrAddr then
        Break
      else
      begin
        ReadValue(P, Value);
        Inc(FirstWord, Value);
        ReadValue(P, Value);
        Inc(SecondWord, Value);
      end;
    end;
  end;
  if FirstWord <> 0 then
  begin
    Result := DataToStr(FirstWord);
    if SecondWord <> 0 then
      Result := Result + '.' + DataToStr(SecondWord)
  end else
    Result := '';
end;

//------------------------------------------------------------------------------

function TJclBinDebugScanner.ReadValue(var P: Pointer; var Value: Integer): Boolean;
var
  N: DWORD;
  I: Integer;
  B: Byte;
begin
  N := 0;
  I := 0;
  repeat
    B := PByte(P)^;
    Inc(PByte(P));
    Inc(N, (B and $7F) shl I);
    Inc(I, 7);
  until B and $80 = 0;
  Value := N;
  Result := (Value <> MaxInt);
end;

//------------------------------------------------------------------------------

function TJclBinDebugScanner.SourceNameFromAddr(Addr: Integer): string;
var
  Value, Name, StartAddr: Integer;
  P: Pointer;
begin
  P := MakePtr(PJclDbgHeader(FStream.Memory)^.SourceNames);
  Name := 0;
  StartAddr := 0;
  while ReadValue(P, Value) do
  begin
    Inc(StartAddr, Value);
    if Addr < StartAddr then
      Break
    else
    begin
      ReadValue(P, Value);
      Inc(Name, Value);
    end;
  end;
  Result := DataToStr(Name);
end;

//==============================================================================
// TJclDebugInfoSource
//==============================================================================

constructor TJclDebugInfoSource.Create(AModule: HMODULE);
begin
  FModule := AModule;
end;

//------------------------------------------------------------------------------

function TJclDebugInfoSource.GetFileName: TFileName;
begin
  Result := GetModulePath(FModule);
end;

//------------------------------------------------------------------------------

function TJclDebugInfoSource.VAFromAddr(const Addr: Pointer): DWORD;
begin
  Result := DWORD(Addr) - FModule - $1000;
end;

//==============================================================================
// TJclDebugInfoList
//==============================================================================

var
  DebugInfoList: TJclDebugInfoList;

procedure NeedDebugInfoList;
begin
  if DebugInfoList = nil then
    DebugInfoList := TJclDebugInfoList.Create;
end;

//------------------------------------------------------------------------------

function TJclDebugInfoList.CreateDebugInfo(const Module: HMODULE): TJclDebugInfoSource;
const
  DebugInfoSources: array [1..3] of TJclDebugInfoSourceClass =
    (TJclDebugInfoMap, TJclDebugInfoBinary, TJclDebugInfoExports);
var
  I: Integer;
begin
  for I := Low(DebugInfoSources) to High(DebugInfoSources) do
  begin
    Result := DebugInfoSources[I].Create(Module);
    try
      if Result.InitializeSource then
        Break
      else
        FreeAndNil(Result);
    except
      FreeAndNil(Result);
      raise;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TJclDebugInfoList.GetItemFromModule(const Module: HMODULE): TJclDebugInfoSource;
var
  I: Integer;
begin
  Result := nil;
  if Module = 0 then
    Exit;
  for I := 0 to Count - 1 do
    if Items[I].Module = Module then
    begin
      Result := Items[I];
      Break;
    end;
  if Result = nil then
  begin
    Result := CreateDebugInfo(Module);
    if Result <> nil then
      Add(Result);
  end;
end;

//------------------------------------------------------------------------------

function TJclDebugInfoList.GetItems(Index: Integer): TJclDebugInfoSource;
begin
  Result := TJclDebugInfoSource(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

function TJclDebugInfoList.GetLocationInfo(const Addr: Pointer; var Info: TJclLocationInfo): Boolean;
var
  Item: TJclDebugInfoSource;
begin
  FillChar(Info, SizeOf(Info), #0);
  Item := ItemFromModule[ModuleFromAddr(Addr)];
  if Item <> nil then
    Result := Item.GetLocationInfo(Addr, Info)
  else
    Result := False;
end;

//==============================================================================
// TJclDebugInfoMap
//==============================================================================

destructor TJclDebugInfoMap.Destroy;
begin
  FreeAndNil(FScanner);
  inherited;
end;

//------------------------------------------------------------------------------

function TJclDebugInfoMap.GetLocationInfo(const Addr: Pointer; var Info: TJclLocationInfo): Boolean;
var
  VA: DWORD;
begin
  VA := VAFromAddr(Addr);
  with FScanner do
  begin
    Info.UnitName := ModuleNameFromAddr(VA);
    Result := (Info.UnitName <> '');
    if Result then
    begin
      Info.Address := Addr;
      Info.ProcedureName := ProcNameFromAddr(VA);
      Info.LineNumber := LineNumberFromAddr(VA);
      Info.SourceName := SourceNameFromAddr(VA);
      Info.DebugInfo := Self;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TJclDebugInfoMap.InitializeSource: Boolean;
var
  MapFileName: TFileName;
begin
  MapFileName := ChangeFileExt(FileName, '.map');
  Result := FileExists(MapFileName);
  if Result then
    FScanner := TJclMapScanner.Create(MapFileName);
end;

//==============================================================================
// TJclDebugInfoBinary
//==============================================================================

destructor TJclDebugInfoBinary.Destroy;
begin
  FreeAndNil(FScanner);
  FreeAndNil(FStream);
  inherited;
end;

//------------------------------------------------------------------------------

function TJclDebugInfoBinary.GetLocationInfo(const Addr: Pointer; var Info: TJclLocationInfo): Boolean;
var
  VA: DWORD;
begin
  VA := VAFromAddr(Addr);
  with FScanner do
  begin
    Info.UnitName := ModuleNameFromAddr(VA);
    Result := (Info.UnitName) <> '';
    if Result then
    begin
      Info.Address := Addr;
      Info.ProcedureName := ProcNameFromAddr(VA);
      Info.LineNumber := LineNumberFromAddr(VA);
      Info.SourceName := SourceNameFromAddr(VA);
      Info.DebugInfo := Self;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TJclDebugInfoBinary.InitializeSource: Boolean;
begin
  Result := FindResource(FModule, JclDbgDataResName, RT_RCDATA) <> 0;
  if Result then
    FStream := TResourceStream.Create(FModule, JclDbgDataResName, RT_RCDATA)
{  else
  begin
    Result := (PeMapImgFindSection(PeMapImgNtHeaders(Pointer(FModule)), JclDbgDataResName) <> nil);
    if Result then
      FStream := TJclPeSectionStream.Create(FModule, JclDbgDataResName);
  end};
  if Result then
    FScanner := TJclBinDebugScanner.Create(FStream, True);
end;

//==============================================================================
// TJclDebugInfoExports
//==============================================================================

destructor TJclDebugInfoExports.Destroy;
begin
  FreeAndNil(FBorImage);
  inherited;
end;

//------------------------------------------------------------------------------

function TJclDebugInfoExports.GetLocationInfo(const Addr: Pointer;
  var Info: TJclLocationInfo): Boolean;
var
  I, BasePos: Integer;
  VA: DWORD;
  Desc: TJclBorUmDescription;
  Unmangled: string;
  RawName: Boolean;
begin
  Result := False;
  VA := DWORD(Addr) - FModule;
  RawName := not FBorImage.IsPackage;
  with FBorImage.ExportList do
  begin
    SortList(esAddress);
    for I := Count - 1 downto 0 do
      if Items[I].Address <= VA then
      begin
        if RawName then
        begin
          Info.ProcedureName := Items[I].Name;
          Result := True;
        end else
        begin
          case PeBorUnmangleName(Items[I].Name, Unmangled, Desc, BasePos) of
            urOk:
              if not (Desc.Kind in [skRTTI, skVTable]) then
              begin
                Info.UnitName := Copy(Unmangled, 1, BasePos - 2);
                Info.ProcedureName := Copy(Unmangled, BasePos, Length(Unmangled));
                if smLinkProc in Desc.Modifiers then
                  Info.ProcedureName := '@' + Info.ProcedureName;
                Result := True;
              end;
            urNotMangled:
              begin
                Info.ProcedureName := Items[I].Name;
                Result := True;
              end;
          end;
        end;
        if Result then
        begin
          Info.Address := Addr;
          Info.DebugInfo := Self;
          Break;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

function TJclDebugInfoExports.InitializeSource: Boolean;
begin
  FBorImage := TJclPeBorImage.Create(True);
  FBorImage.AttachLoadedModule(FModule);
  Result := FBorImage.StatusOK;
end;

//==============================================================================
// Source location functions
//==============================================================================

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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

{$STACKFRAMES ON}
function Caller(Level: Integer): Pointer;
begin
  with TJclStackInfoList.Create(False, 1, nil) do
  try
    if Level < Count then
      Result := Pointer(Items[Level].StackInfo.CallerAdr)
    else
      Result := nil;
  finally
    Free;
  end;
end;
{$IFNDEF StackFramesWasOn}
  {$STACKFRAMES OFF}
{$ENDIF}

//------------------------------------------------------------------------------

function GetLocationInfo(const Addr: Pointer): TJclLocationInfo;
begin
  NeedDebugInfoList;
  DebugInfoList.GetLocationInfo(Addr, Result)
end;

//------------------------------------------------------------------------------

function GetLocationInfoStr(const Addr: Pointer): string;
var
  Info: TJclLocationInfo;
begin
  NeedDebugInfoList;
  if DebugInfoList.GetLocationInfo(Addr, Info) then
    with Info do
      Result := Format('[%p] %s.%s (Line %u, "%s")', [Addr, UnitName,
        ProcedureName, LineNumber, SourceName])
  else
    Result := Format('[%p]', [Addr]);
end;

//------------------------------------------------------------------------------

procedure ClearLocationData;
begin
  if DebugInfoList <> nil then
    DebugInfoList.Clear;
end;

//------------------------------------------------------------------------------

{$STACKFRAMES ON}

function __FILE__(const Level: Integer): string;
begin
  Result := GetLocationInfo(Caller(Level + 1)).SourceName;
end;

//------------------------------------------------------------------------------

function __MODULE__(const Level: Integer): string;
begin
  Result := GetLocationInfo(Caller(Level + 1)).UnitName;
end;

//------------------------------------------------------------------------------

function __PROC__(const Level: Integer): string;
begin
  Result := GetLocationInfo(Caller(Level + 1)).ProcedureName;
end;

//------------------------------------------------------------------------------

function __LINE__(const Level: Integer): Integer;
begin
  Result := GetLocationInfo(Caller(Level + 1)).LineNumber;
end;

//------------------------------------------------------------------------------

function __MAP__(const Level: Integer; var _File, _Module, _Proc: string;
  var _Line: Integer): Boolean;
begin
  Result := __MAP_OF_ADDR__(Caller(Level + 1), _File, _Module, _Proc, _Line);
end;

{$IFNDEF StackFramesWasOn}
  {$STACKFRAMES OFF}
{$ENDIF}

//------------------------------------------------------------------------------

function __FILE_OF_ADDR__(const Addr: Pointer): string;
begin
  Result := GetLocationInfo(Addr).SourceName;
end;

//------------------------------------------------------------------------------

function __MODULE_OF_ADDR__(const Addr: Pointer): string;
begin
  Result := GetLocationInfo(Addr).UnitName;
end;

//------------------------------------------------------------------------------

function __PROC_OF_ADDR__(const Addr: Pointer): string;
begin
  Result := GetLocationInfo(Addr).ProcedureName;
end;

//------------------------------------------------------------------------------

function __LINE_OF_ADDR__(const Addr: Pointer): Integer;
begin
  Result := GetLocationInfo(Addr).LineNumber;
end;

//------------------------------------------------------------------------------

function __MAP_OF_ADDR__(const Addr: Pointer; var _File, _Module, _Proc: string;
  var _Line: Integer): Boolean;
var
  LocInfo: TJclLocationInfo;
begin
  NeedDebugInfoList;
  Result := DebugInfoList.GetLocationInfo(Addr, LocInfo);
  if Result then
  begin
    _File := LocInfo.SourceName;
    _Module := LocInfo.UnitName;
    _Proc := LocInfo.ProcedureName;
    _Line := LocInfo.LineNumber;
  end;
end;

//==============================================================================
// Stack info routines
//==============================================================================

{$STACKFRAMES OFF}

var
  ExceptStackInfo: TJclStackInfoList;
  TopOfStack: DWORD = 0;
  BaseOfCode: DWORD = 0;
  TopOfCode: DWORD = 0;
  BaseOfStack: DWORD;

//------------------------------------------------------------------------------

type
  PExceptionArguments = ^TExceptionArguments;
  TExceptionArguments = record
    ExceptAddr: Pointer;
    ExceptObj: TObject;
  end;

//------------------------------------------------------------------------------

function GetEBP: Pointer;
asm
  MOV EAX, EBP
end;

//------------------------------------------------------------------------------

function GetESP: Pointer;
asm
  MOV EAX, ESP
end;

//------------------------------------------------------------------------------

function GetStackTop: DWORD;
// Reference: Matt Pietrek, MSJ, Under the hood, on TIBs:
// http://msdn.microsoft.com/library/periodic/period96/S2CE.htm
asm
  MOV EAX, FS:[4]
end;

//------------------------------------------------------------------------------

procedure InitGlobalVar;
var
  NTHeader: PImageNTHeaders;
begin
  if BaseOfCode = 0 then
  begin
    NTHeader := PeMapImgNtHeaders(Pointer(HInstance));
    BaseOfCode := DWORD(HInstance) + NTHeader.OptionalHeader.BaseOfCode;
    TopOfCode := BaseOfCode + NTHeader.OptionalHeader.SizeOfCode;
    TopOfStack := GetStackTop;
  end;
end;

//------------------------------------------------------------------------------

function ValidStackAddr(StackAddr: DWORD): Boolean;
begin
  Result := (BaseOfStack < StackAddr) and (StackAddr < TopOfStack);
end;

//------------------------------------------------------------------------------

function ValidCodeAddr(CodeAddr: DWORD): Boolean;
begin
//!  Result := (BaseOfCode < CodeAddr) and (CodeAddr < TopOfCode);
  Result := IsSystemModule(FindHInstance(Pointer(CodeAddr)));
end;

//------------------------------------------------------------------------------

function ValidCallSite(CodeAddr: DWORD): Boolean;
// Validate that the code address is a valid code site
//
// Information from Intel Manual 24319102(2).pdf, Download the 6.5 MBs from:
//  http://developer.intel.com/design/pentiumii/manuals/243191.htm
//  Instruction format, Chapter 2 and The CALL instruction: page 3-53, 3-54
var
  CodeDWORD4: DWORD;
  CodeDWORD8: DWORD;
  C4P, C8P: PDWORD;
begin
  // First check that the address is within range of our code segment!
//!  Result := (BaseOfCode < CodeAddr) and (CodeAddr < TopOfCode);
  C8P := PDWORD(CodeAddr - 8);
  C4P := PDWORD(CodeAddr - 4);
  Result := (CodeAddr > 8) and ValidCodeAddr(DWORD(C8P)) and
    not IsBadReadPtr(C8P, 8) and not IsBadCodePtr(C8P) and not IsBadCodePtr(C4P);

  // Now check to see if the instruction preceeding the return address
  // could be a valid CALL instruction
  if Result then
  begin
    // Check the instruction prior to the potential call site.
    // We consider it a valid call site if we find a CALL instruction there
    // Check the most common CALL variants first
//!    CodeDWORD8 := PDWORD(CodeAddr-8)^;
//!    CodeDWORD4 := PDWORD(CodeAddr-4)^;
    CodeDWORD8 := C8P^;
    CodeDWORD4 := C4P^;

    Result :=
          ((CodeDWORD8 and $FF000000) = $E8000000) // 5-byte, CALL [-$1234567]
       or ((CodeDWORD4 and $38FF0000) = $10FF0000) // 2 byte, CALL EAX
       or ((CodeDWORD4 and $0038FF00) = $0010FF00) // 3 byte, CALL [EBP+0x8]
       or ((CodeDWORD4 and $000038FF) = $000010FF) // 4 byte, CALL ??
       or ((CodeDWORD8 and $38FF0000) = $10FF0000) // 6-byte, CALL ??
       or ((CodeDWORD8 and $0038FF00) = $0010FF00) // 7-byte, CALL [ESP-0x1234567]
    // It is possible to simulate a CALL by doing a PUSH followed by RET,
    // so we check for a RET just prior to the return address
       or ((CodeDWORD4 and $FF000000) = $C3000000);// PUSH XX, RET

    // Because we're not doing a complete disassembly, we will potentially report
    // false positives. If there is odd code that uses the CALL 16:32 format, we
    // can also get false negatives.
  end;
end;

//------------------------------------------------------------------------------

function NextStackFrame(var StackFrame: PStackFrame; var StackInfo : TStackInfo): Boolean;
begin
  // Only report this stack frame into the StockInfo structure
  // if the StackFrame pointer, EBP on the stack and return
  // address on the stack are valid addresses
  while ValidStackAddr(DWORD(StackFrame)) do
  begin
    // CallerAdr within current process space, code segment etc.
    if ValidCodeAddr(StackFrame^.CallerAdr) then
    begin
      Inc(StackInfo.Level);
      StackInfo.StackFrame := StackFrame;
      StackInfo.ParamPtr := PDWORDArray(DWORD(StackFrame) + SizeOf(TStackFrame));
      StackInfo.CallersEBP := StackFrame^.CallersEBP;
      StackInfo.CallerAdr := StackFrame^.CallerAdr;
      StackInfo.DumpSize := StackFrame^.CallersEBP - DWORD(StackFrame);
      StackInfo.ParamSize := (StackInfo.DumpSize - SizeOf(TStackFrame)) div 4;
      // Step to the next stack frame by following the EBP pointer
      StackFrame := PStackFrame(StackFrame^.CallersEBP);
      Result := True;
      Exit;
    end;
    // Step to the next stack frame by following the EBP pointer
    StackFrame := PStackFrame(StackFrame^.CallersEBP);
  end;
  Result := False;
end;

//------------------------------------------------------------------------------

{$STACKFRAMES ON}

procedure DoExceptionStackTrace(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean);
var
  IgnoreLevels: Integer;
  FirstCaller: Pointer;
begin
  if RawStackTracking then
    IgnoreLevels := 8
  else
    IgnoreLevels := 4;
  if OSException then
    FirstCaller := ExceptAddr
  else
    FirstCaller := nil;
  JclCreateStackList(RawStackTracking, IgnoreLevels, FirstCaller);
end;

//------------------------------------------------------------------------------

function JclLastExceptStackList: TJclStackInfoList;
begin
  Result := ExceptStackInfo;
end;

//------------------------------------------------------------------------------

function JclCreateStackList(Raw: Boolean; AIgnoreLevels: Integer; FirstCaller: Pointer): TJclStackInfoList;
begin
  FreeAndNil(ExceptStackInfo);
  ExceptStackInfo := TJclStackInfoList.Create(Raw, AIgnoreLevels, FirstCaller);
  Result := ExceptStackInfo;
end;

//==============================================================================
// TJclStackInfoItem
//==============================================================================

function TJclStackInfoItem.GetLogicalAddress: DWORD;
begin
  Result := FStackInfo.CallerAdr - DWORD(ModuleFromAddr(Pointer(FStackInfo.CallerAdr)));
end;

//==============================================================================
// TJclStackInfoList
//==============================================================================

procedure StoreToList(List: TJclStackInfoList; const StackInfo: TStackInfo);
var
  Item: TJclStackInfoItem;
begin
  with List do
    if StackInfo.Level > IgnoreLevels + 1 then
    begin
      Item := TJclStackInfoItem.Create;
      Item.FStackInfo := StackInfo;
      Add(Item);
    end;
end;

//------------------------------------------------------------------------------

procedure TraceStackFrames(List: TJclStackInfoList);
var
  StackFrame: PStackFrame;
  StackInfo: TStackInfo;
begin
  // Start at level 0
  StackInfo.Level := 0;

  // Get the current stack fram from the EBP register
  StackFrame := GetEBP;

  // We define the bottom of the valid stack to be the current EBP Pointer
  // There is a TIB field called pvStackUserBase, but this includes more of the
  // stack than what would define valid stack frames.
  BaseOfStack := DWORD(StackFrame) - 1;

  // Loop over and report all valid stackframes
  while NextStackFrame(StackFrame, StackInfo) do
    StoreToList(List, StackInfo);
end;

//------------------------------------------------------------------------------

procedure TraceStackRaw(List: TJclStackInfoList);
var
  StackInfo: TStackInfo;
  StackPtr: PDWORD;
  PrevCaller: DWORD;
begin
  // We define the bottom of the valid stack to be the current ESP pointer
  BaseOfStack := DWORD(GetESP);

  // We will not be able to fill in all the fields in the StackInfo record,
  // so just blank it all out first
  FillChar(StackInfo, SizeOf(StackInfo), 0);

  // Clear the previous call address
  PrevCaller := 0;

  // Get a pointer to the current bottom of the stack
  StackPtr := PDWORD(BaseOfStack);

  // Loop through all of the valid stack space
  while DWORD(StackPtr) < TopOfStack do
  begin

    // If the current DWORD on the stack,
    // refers to a valid call site...
    if ValidCallSite(StackPtr^) and (StackPtr^ <> PrevCaller) then
    begin
      // then pick up the callers address
      StackInfo.CallerAdr := StackPtr^;

      // remeber to callers address so that we don't report it repeatedly
      PrevCaller := StackPtr^;

      // increase the stack level
      Inc(StackInfo.Level);

      // then report it back to our caller
      StoreToList(List, StackInfo);
    end;

    // Look at the next DWORD on the stack
    Inc(StackPtr);
  end;
end;

//------------------------------------------------------------------------------

procedure TJclStackInfoList.AddToStrings(Strings: TStrings);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Strings.Add(GetLocationInfoStr(Pointer(Items[I].StackInfo.CallerAdr)));
end;

//------------------------------------------------------------------------------

constructor TJclStackInfoList.Create(Raw: Boolean; AIgnoreLevels: DWORD;
  FirstCaller: Pointer);
var
  Item: TJclStackInfoItem;
begin
  inherited Create;
  FIgnoreLevels := AIgnoreLevels;
  FTimeStamp := Now;
  FThreadID := GetCurrentThreadId;
  InitGlobalVar;
  if FirstCaller <> nil then
  begin
    Item := TJclStackInfoItem.Create;
    Item.FStackInfo.CallerAdr := DWORD(FirstCaller);
    Add(Item);
  end;
  if Raw then
    TraceStackRaw(Self)
  else
    TraceStackFrames(Self);
end;

//------------------------------------------------------------------------------

function TJclStackInfoList.GetItems(Index: Integer): TJclStackInfoItem;
begin
  Result := TJclStackInfoItem(inherited Items[Index]);
end;

{$IFNDEF StackFramesWasOn}
  {$STACKFRAMES OFF}
{$ENDIF}

//==============================================================================
// Stack frame info routines
//==============================================================================

function GetFS: Pointer;
asm
  XOR EAX, EAX
  MOV EAX, FS:[EAX]
end;

//------------------------------------------------------------------------------

var
  LastExceptFrameList: TJclExceptFrameList;

function JclCreateExceptFrameList(AIgnoreLevels: Integer): TJclExceptFrameList;
begin
  FreeAndNil(LastExceptFrameList);
  LastExceptFrameList := TJclExceptFrameList.Create(AIgnoreLevels);
  Result := LastExceptFrameList;
end;

//------------------------------------------------------------------------------

function JclLastExceptFrameList: TJclExceptFrameList;
begin
  Result := LastExceptFrameList;
end;

//------------------------------------------------------------------------------

procedure DoExceptFrameTrace;
begin
  { Ignore first 2 levels; this is the try finally in DoExceptNotify and a
    FindClose call in the kernel. Note that it now ignores 3 levels; there's one
    level that is unexplainable; by the time it gets to the notifier method is
    already gone and invalid }
  JclCreateExceptFrameList(3);
end;

//==============================================================================
// TJclExceptFrame
//==============================================================================

procedure TJclExceptFrame.DoDetermineFrameType;
var
  Dest: Longint;
  LocInfo: TJclLocationInfo;
begin
  FFrameType := eftUnknown;
  if FExcFrame <> nil then
  begin
    if ExcFrame.desc.jmp.opCode = $E9 then
      Dest := ExcFrame.desc.jmp.distance + 5 + Longint(ExcFrame.desc)
    else if ExcFrame.desc.jmp.opCode = $EB then
      Dest := Byte(ExcFrame.desc.jmp.distance) + 2 + Longint(ExcFrame.desc)
    else
      Dest := 0;
    if Dest <> 0 then
    begin
      LocInfo := GetLocationInfo(Pointer(Dest));
      if (CompareText(LocInfo.UnitName, 'system') = 0) then
      begin
        if (CompareText(LocInfo.ProcedureName, '@HandleAnyException') = 0) then
          FFrameType := eftAnyException
        else if (CompareText(LocInfo.ProcedureName, '@HandleOnException') = 0) then
          FFrameType := eftOnException
        else if (CompareText(LocInfo.ProcedureName, '@HandleAutoException') = 0) then
          FFrameType := eftAutoException
        else if (CompareText(LocInfo.ProcedureName, '@HandleFinally') = 0) then
          FFrameType := eftFinally;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

constructor TJclExceptFrame.Create(AExcFrame: PExcFrame);
begin
  inherited Create;
  FExcFrame := AExcFrame;
  DoDetermineFrameType;
end;

//------------------------------------------------------------------------------

function TJclExceptFrame.Handles(ExceptObj: TObject): Boolean;
var
  Handler: Pointer;
begin
  Result := HandlerInfo(ExceptObj, Handler);
end;

//------------------------------------------------------------------------------

function TJclExceptFrame.HandlerInfo(ExceptObj: TObject;
  var HandlerAt: Pointer): Boolean;
var
  I: Integer;
  vTable: Pointer;
begin
  Result := FrameType = eftAnyException;
  if not Result and (FrameType = eftOnException) then
  begin
    I := 0;
    vTable := Pointer(Integer(ExceptObj.ClassType) + vmtSelfPtr);
    while (I < ExcFrame.desc.cnt) and not Result and (vTable <> nil) do
    begin
      Result := (ExcFrame.desc.excTab[I].vTable = nil) or
        (ExcFrame.desc.excTab[I].vTable = vTable);
      if not Result then
      begin
        Move(PChar(vTable)[vmtParent - vmtSelfPtr], vTable, 4);
        if vTable = nil then
        begin
          vTable := Pointer(Integer(ExceptObj.ClassType) + vmtSelfPtr);
          Inc(I);
        end;
      end;
    end;
    if Result then
      HandlerAt := ExcFrame.desc.excTab[I].handler;
  end
  else if Result then
    HandlerAt := @ExcFrame.desc.instructions
  else
    HandlerAt := nil;
end;

//==============================================================================
// TJclExceptFrameList
//==============================================================================

function TJclExceptFrameList.AddFrame(AFrame: PExcFrame): TJclExceptFrame;
begin
  Result := TJclExceptFrame.Create(AFrame);
  try
    Add(Result);
  except
    Remove(Result);
    Result.Free;
    raise;
  end;
end;

//------------------------------------------------------------------------------

function TJclExceptFrameList.GetItems(Index: Integer): TJclExceptFrame;
begin
  Result := TJclExceptFrame(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

constructor TJclExceptFrameList.Create(AIgnoreLevels: Integer);
begin
  inherited Create;
  FIgnoreLevels := AIgnoreLevels;
  TraceExceptionFrames;
end;

//------------------------------------------------------------------------------

procedure TJclExceptFrameList.TraceExceptionFrames;
var
  FS: PExcFrame;
  Level: Integer;
begin
  Clear;
  Level := 0;
  FS := GetFS;
  while Longint(FS) <> -1 do
  begin
    if Level >= IgnoreLevels then
      AddFrame(FS);
    Inc(Level);
    FS := FS.next;
  end;
end;

//==============================================================================
// Exception hooking
//==============================================================================

var
  Kernel32_RaiseException: procedure (dwExceptionCode, dwExceptionFlags,
    nNumberOfArguments: DWORD; lpArguments: PDWORD); stdcall;
  SysUtils_ExceptObjProc: function (P: PExceptionRecord): Exception;
  ExceptionsHooked: Boolean;
  PeImportHooks: TJclPeMapImgHooks;

//------------------------------------------------------------------------------

var
  Recursive: Boolean;

procedure DoExceptNotify(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean);
begin
  if not Recursive and (StackTrackingEnable or ExceptionFrameTrackingEnable or
    Assigned(ExceptNotifyProc) or Assigned(ExceptNotifyMethod)) then
  begin
    Recursive := True;
    try
      if StackTrackingEnable then
        DoExceptionStackTrace(ExceptObj, ExceptAddr, OSException);
      if ExceptionFrameTrackingEnable then
        DoExceptFrameTrace;
      if Assigned(ExceptNotifyProc) then
        ExceptNotifyProc(ExceptObj, ExceptAddr, OSException);
      if Assigned(ExceptNotifyMethod) then
        ExceptNotifyMethod(ExceptObj, ExceptAddr, OSException);
    finally
      Recursive := False;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure HookedRaiseException(ExceptionCode, ExceptionFlags, NumberOfArguments: DWORD;
  Arguments: PExceptionArguments); stdcall;
const
  cDelphiException = {$IFDEF DELPHI2}$0EEDFACE{$ELSE}$0EEDFADE{$ENDIF};
  cNonContinuable = 1;
begin
  if (ExceptionFlags = cNonContinuable) and (ExceptionCode = cDelphiException) and
    (NumberOfArguments = 7) and (DWORD(Arguments) = DWORD(@Arguments) + 4) then
      DoExceptNotify(Arguments.ExceptObj, Arguments.ExceptAddr, False);
  Kernel32_RaiseException(ExceptionCode, ExceptionFlags, NumberOfArguments, PDWORD(Arguments));
end;

//------------------------------------------------------------------------------

function HookedExceptObjProc(P: PExceptionRecord): Exception;
begin
  Result := SysUtils_ExceptObjProc(P);
  DoExceptNotify(Result, P^.ExceptionAddress, True);
end;

//------------------------------------------------------------------------------

function JclHookExceptions: Boolean;
begin
  if ExceptionsHooked then
    Result := False
  else
  begin
    Recursive := False;
    if PeImportHooks = nil then
      PeImportHooks := TJclPeMapImgHooks.Create;
    Result := PeImportHooks.HookImport(Pointer(FindClassHInstance(System.TObject)),
      kernel32, 'RaiseException', @HookedRaiseException, @Kernel32_RaiseException);
    if Result then
    begin
      SysUtils_ExceptObjProc := System.ExceptObjProc;
      System.ExceptObjProc := @HookedExceptObjProc;
    end;
    ExceptionsHooked := Result;
  end;
end;

//------------------------------------------------------------------------------

function JclUnhookExceptions: Boolean;
begin
  if ExceptionsHooked then
  begin
    PeImportHooks.UnhookByNewAddress(@HookedRaiseException);
    System.ExceptObjProc := @SysUtils_ExceptObjProc;
    @SysUtils_ExceptObjProc := nil;
    @Kernel32_RaiseException := nil;
    Result := True;
    ExceptionsHooked := False;
  end else
    Result := False;
end;

//------------------------------------------------------------------------------

function JclExceptionsHooked: Boolean;
begin
  Result := ExceptionsHooked;
end;

//------------------------------------------------------------------------------

initialization

finalization
  JclUnhookExceptions;
  FreeAndNil(DebugInfoList);
  FreeAndNil(ExceptStackInfo);
  FreeAndNil(LastExceptFrameList);
  FreeAndNil(PeImportHooks);

end.
