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
{ The Original Code is JclDebug.pas.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Various debugging support routines and classes. This includes: Diagnostics routines, Trace       }
{ routines, Stack tracing and Source Locations a la the C/C++ __FILE__ and __LINE__ macros.        }
{                                                                                                  }
{ Unit owner: Petr Vones                                                                           }
{ Last modified: April 6, 2002                                                                     }
{                                                                                                  }
{**************************************************************************************************}

unit JclDebug;

interface

{$I jcl.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Classes, SysUtils,
  {$IFDEF DELPHI5_UP}
  Contnrs,
  {$ENDIF DELPHI5_UP}
  JclBase, JclFileUtils, JclPeImage, JclSynch, JclTD32;

//--------------------------------------------------------------------------------------------------
// Diagnostics
//--------------------------------------------------------------------------------------------------

procedure AssertKindOf(const ClassName: string; const Obj: TObject); overload;
procedure AssertKindOf(const ClassType: TClass; const Obj: TObject); overload;

procedure Trace(const Msg: string);
procedure TraceFmt(const Fmt: string; const Args: array of const);
procedure TraceLoc(const Msg: string);
procedure TraceLocFmt(const Fmt: string; const Args: array of const);

//--------------------------------------------------------------------------------------------------
// Optimized functionality of JclSysInfo functions ModuleFromAddr and IsSystemModule
//--------------------------------------------------------------------------------------------------

type
  TJclModuleInfo = class (TObject)
  private
    FSize: Cardinal;
    FEndAddr: Pointer;
    FStartAddr: Pointer;
    FSystemModule: Boolean;
  public
    property EndAddr: Pointer read FEndAddr;
    property Size: Cardinal read FSize;
    property StartAddr: Pointer read FStartAddr;
    property SystemModule: Boolean read FSystemModule;
  end;

  TJclModuleInfoList = class (TObjectList)
  private
    FDynamicBuild: Boolean;
    FSystemModulesOnly: Boolean;
    function GetItems(Index: Integer): TJclModuleInfo;
    function GetModuleFromAddress(Addr: Pointer): TJclModuleInfo;
  protected
    procedure BuildModulesList;
    function CreateItemForAddress(Addr: Pointer; SystemModule: Boolean): TJclModuleInfo;
  public
    constructor Create(ADynamicBuild, ASystemModulesOnly: Boolean);
    function IsSystemModuleAddress(Addr: Pointer): Boolean;
    function IsValidModuleAddress(Addr: Pointer): Boolean;
    property DynamicBuild: Boolean read FDynamicBuild;
    property Items[Index: Integer]: TJclModuleInfo read GetItems;
    property ModuleFromAddress[Addr: Pointer]: TJclModuleInfo read GetModuleFromAddress;
  end;

//--------------------------------------------------------------------------------------------------
// MAP file abstract parser
//--------------------------------------------------------------------------------------------------

type
  PJclMapAddress = ^TJclMapAddress;
  TJclMapAddress = packed record
    Segment: Word;
    Offset: Integer;
  end;

  PJclMapString = PAnsiChar;

  TJclAbstractMapParser = class (TObject)
  private
    FLinkerBug: Boolean;
    FLinkerBugUnitName: PJclMapString;
    FStream: TJclFileMappingStream;
    function GetLinkerBugUnitName: string;
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
    property LinkerBug: Boolean read FLinkerBug;
    property LinkerBugUnitName: string read GetLinkerBugUnitName; 
    property Stream: TJclFileMappingStream read FStream;
  end;

//--------------------------------------------------------------------------------------------------
// MAP file parser
//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------
// MAP file scanner
//--------------------------------------------------------------------------------------------------

  PJclMapSegment = ^TJclMapSegment;
  TJclMapSegment = record
    StartAddr: DWORD;
    EndAddr: DWORD;
    UnitName: PJclMapString;
  end;

  PJclMapProcName = ^TJclMapProcName;
  TJclMapProcName = record
    Addr: DWORD;
    ProcName: PJclMapString;
  end;

  PJclMapLineNumber = ^TJclMapLineNumber;
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
    FLastValidAddr: TJclMapAddress;
    FLineNumbersCnt: Integer;
    FLineNumberErrors: Integer;
    FNewUnitFileName: PJclMapString;
    FProcNamesCnt: Integer;
    FTopValidAddr: Integer;
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
    function LineNumberFromAddr(Addr: DWORD): Integer; overload;
    function LineNumberFromAddr(Addr: DWORD; var Offset: Integer): Integer; overload;
    function ModuleNameFromAddr(Addr: DWORD): string;
    function ModuleStartFromAddr(Addr: DWORD): DWORD;
    function ProcNameFromAddr(Addr: DWORD): string; overload;
    function ProcNameFromAddr(Addr: DWORD; var Offset: Integer): string; overload;
    function SourceNameFromAddr(Addr: DWORD): string;
    property LineNumberErrors: Integer read FLineNumberErrors;
  end;

//--------------------------------------------------------------------------------------------------
// JCL binary debug data generator and scanner
//--------------------------------------------------------------------------------------------------

const
  JclDbgDataSignature = $4742444A; // JDBG
  JclDbgDataResName   = 'JCLDEBUG';
  JclDbgFileExtension = '.jdbg';

  JclDbgHeaderVersion = 1; // JCL 1.11 and 1.20

  MapFileExtension    = '.map';
  DrcFileExtension    = '.drc';

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
    ModuleName: Integer;
    CheckSum: Integer;
    CheckSumValid: Boolean;
  end;

  TJclBinDebugGenerator = class (TJclMapScanner)
  private
    FDataStream: TMemoryStream;
    FMapFileName: TFileName;
  protected
    procedure CreateData;
  public
    constructor Create(const MapFileName: TFileName); override;
    destructor Destroy; override;
    function CalculateCheckSum: Boolean;
    property DataStream: TMemoryStream read FDataStream;
  end;

  TJclBinDbgNameCache = record
    Addr: DWORD;
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
    function GetModuleName: string;
  protected
    procedure CacheLineNumbers;
    procedure CacheProcNames;
    procedure CheckFormat;
    function DataToStr(A: Integer): string;
    function MakePtr(A: Integer): Pointer;
    function ReadValue(var P: Pointer; var Value: Integer): Boolean;
  public
    constructor Create(AStream: TCustomMemoryStream; CacheData: Boolean);
    function IsModuleNameValid(const Name: TFileName): Boolean;
    function LineNumberFromAddr(Addr: DWORD): Integer; overload;
    function LineNumberFromAddr(Addr: DWORD; var Offset: Integer): Integer; overload;
    function ProcNameFromAddr(Addr: DWORD): string; overload;
    function ProcNameFromAddr(Addr: DWORD; var Offset: Integer): string; overload;
    function ModuleNameFromAddr(Addr: DWORD): string;
    function ModuleStartFromAddr(Addr: DWORD): DWORD;
    function SourceNameFromAddr(Addr: DWORD): string;
    property ModuleName: string read GetModuleName;
    property ValidFormat: Boolean read FValidFormat;
  end;

function ConvertMapFileToJdbgFile(const MapFileName: TFileName): Boolean; overload;
function ConvertMapFileToJdbgFile(const MapFileName: TFileName; var LinkerBugUnit: string;
  var LineNumberErrors: Integer): Boolean; overload;

function InsertDebugDataIntoExecutableFile(const ExecutableFileName,
  MapFileName: TFileName; var LinkerBugUnit: string;
  var MapFileSize, JclDebugDataSize: Integer): Boolean; overload;
function InsertDebugDataIntoExecutableFile(const ExecutableFileName,
  MapFileName: TFileName; var LinkerBugUnit: string;
  var MapFileSize, JclDebugDataSize, LineNumberErrors: Integer): Boolean; overload;

function InsertDebugDataIntoExecutableFile(const ExecutableFileName: TFileName;
  BinDebug: TJclBinDebugGenerator; var LinkerBugUnit: string;
  var MapFileSize, JclDebugDataSize: Integer): Boolean; overload;
function InsertDebugDataIntoExecutableFile(const ExecutableFileName: TFileName;
  BinDebug: TJclBinDebugGenerator; var LinkerBugUnit: string;
  var MapFileSize, JclDebugDataSize, LineNumberErrors: Integer): Boolean; overload;

//--------------------------------------------------------------------------------------------------
// Source Locations
//--------------------------------------------------------------------------------------------------

type
  TJclDebugInfoSource = class;

  PJclLocationInfo = ^TJclLocationInfo;
  TJclLocationInfo = record
    Address: Pointer;               // Error address
    UnitName: string;               // Name of Delphi unit
    ProcedureName: string;          // Procedure name
    OffsetFromProcName: Integer;    // Offset from Address to ProcedureName symbol location
    LineNumber: Integer;            // Line number
    OffsetFromLineNumber: Integer;  // Offset from Address to LineNumber symbol location
    SourceName: string;             // Module file name
    DebugInfo: TJclDebugInfoSource; // Location object
  end;

  TJclDebugInfoSource = class (TObject)
  private
    FModule: HMODULE;
    function GetFileName: TFileName;
  protected
    function InitializeSource: Boolean; virtual; abstract;
    function VAFromAddr(const Addr: Pointer): DWORD; virtual;
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

//--------------------------------------------------------------------------------------------------
// Various source location implementations
//--------------------------------------------------------------------------------------------------

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

  TJclDebugInfoTD32 = class(TJclDebugInfoSource)
  private
    FImage: TJclPeBorTD32Image;
  protected
    function InitializeSource: Boolean; override;
  public
    destructor Destroy; override;
    function GetLocationInfo(const Addr: Pointer; var Info: TJclLocationInfo): Boolean; override;
  end;

//--------------------------------------------------------------------------------------------------
// Source location functions
//--------------------------------------------------------------------------------------------------

function Caller(Level: Integer = 0): Pointer;

function GetLocationInfo(const Addr: Pointer): TJclLocationInfo; overload;
function GetLocationInfo(const Addr: Pointer; var Info: TJclLocationInfo): Boolean; overload;
function GetLocationInfoStr(const Addr: Pointer; IncludeModuleName: Boolean = False;
  IncludeAddressOffset: Boolean = False; IncludeStartProcLineOffset: Boolean = False): string;
function DebugInfoAvailable(const Module: HMODULE): Boolean;
procedure ClearLocationData;

function FileByLevel(const Level: Integer = 0): string;
function ModuleByLevel(const Level: Integer = 0): string;
function ProcByLevel(const Level: Integer = 0): string;
function LineByLevel(const Level: Integer = 0): Integer;
function MapByLevel(const Level: Integer; var File_, Module_, Proc_: string; var Line_: Integer): Boolean;

function FileOfAddr(const Addr: Pointer): string;
function ModuleOfAddr(const Addr: Pointer): string;
function ProcOfAddr(const Addr: Pointer): string;
function LineOfAddr(const Addr: Pointer): Integer;
function MapOfAddr(const Addr: Pointer; var File_, Module_, Proc_: string; var Line_: Integer): Boolean;

function ExtractClassName(const ProcedureName: string): string;
function ExtractMethodName(const ProcedureName: string): string;

// Original function names, deprecated will be removed in V2.0; do not use!

function __FILE__(const Level: Integer = 0): string; {$IFDEF DELPHI6_UP} deprecated; {$ENDIF}
function __MODULE__(const Level: Integer = 0): string; {$IFDEF DELPHI6_UP} deprecated; {$ENDIF}
function __PROC__(const Level: Integer  = 0): string; {$IFDEF DELPHI6_UP} deprecated; {$ENDIF}
function __LINE__(const Level: Integer = 0): Integer; {$IFDEF DELPHI6_UP} deprecated; {$ENDIF}
function __MAP__(const Level: Integer; var _File, _Module, _Proc: string; var _Line: Integer): Boolean; {$IFDEF DELPHI6_UP} deprecated; {$ENDIF}
function __FILE_OF_ADDR__(const Addr: Pointer): string; {$IFDEF DELPHI6_UP} deprecated; {$ENDIF}
function __MODULE_OF_ADDR__(const Addr: Pointer): string; {$IFDEF DELPHI6_UP} deprecated; {$ENDIF}
function __PROC_OF_ADDR__(const Addr: Pointer): string; {$IFDEF DELPHI6_UP} deprecated; {$ENDIF}
function __LINE_OF_ADDR__(const Addr: Pointer): Integer; {$IFDEF DELPHI6_UP} deprecated; {$ENDIF}
function __MAP_OF_ADDR__(const Addr: Pointer; var _File, _Module, _Proc: string;
  var _Line: Integer): Boolean; {$IFDEF DELPHI6_UP} deprecated; {$ENDIF}

//--------------------------------------------------------------------------------------------------
// Stack info routines base list
//--------------------------------------------------------------------------------------------------

type
  TJclStackBaseList = class (TObjectList)
  private
    FThreadID: DWORD;
    FTimeStamp: TDateTime;
  public
    constructor Create;
    property ThreadID: DWORD read FThreadID;
    property TimeStamp: TDateTime read FTimeStamp;
  end;

//--------------------------------------------------------------------------------------------------
// Stack info routines
//--------------------------------------------------------------------------------------------------

type
  PDWORDArray = ^TDWORDArray;
  TDWORDArray = array [0..(MaxInt - $F) div SizeOf(DWORD)] of DWORD;

  PStackFrame = ^TStackFrame;
  TStackFrame = record
    CallersEBP: DWORD;
    CallerAdr: DWORD;
  end;

  PStackInfo = ^TStackInfo;
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
    function GetCallerAdr: Pointer;
    function GetLogicalAddress: DWORD;
  public
    property CallerAdr: Pointer read GetCallerAdr; 
    property LogicalAddress: DWORD read GetLogicalAddress;
    property StackInfo: TStackInfo read FStackInfo;
  end;

  TJclStackInfoList = class (TJclStackBaseList)
  private
    FIgnoreLevels: DWORD;
    TopOfStack: Cardinal;
    BaseOfStack: Cardinal;
    FModuleInfoList: TJclModuleInfoList;
    function GetItems(Index: Integer): TJclStackInfoItem;
    function NextStackFrame(var StackFrame: PStackFrame; var StackInfo: TStackInfo): Boolean;
    procedure StoreToList(const StackInfo: TStackInfo);
    procedure TraceStackFrames;
    procedure TraceStackRaw;
    function ValidCallSite(CodeAddr: DWORD): Boolean;
    function ValidStackAddr(StackAddr: DWORD): Boolean;
  public
    constructor Create(Raw: Boolean; AIgnoreLevels: DWORD; FirstCaller: Pointer);
    destructor Destroy; override;
    procedure AddToStrings(Strings: TStrings; IncludeModuleName: Boolean = False;
      IncludeAddressOffset: Boolean = False; IncludeStartProcLineOffset: Boolean = False);
    property Items[Index: Integer]: TJclStackInfoItem read GetItems; default;
    property IgnoreLevels: DWORD read FIgnoreLevels;
  end;

function JclCreateStackList(Raw: Boolean; AIgnoreLevels: Integer; FirstCaller: Pointer): TJclStackInfoList;

function JclLastExceptStackList: TJclStackInfoList;
function JclLastExceptStackListToStrings(Strings: TStrings; IncludeModuleName: Boolean = False;
  IncludeAddressOffset: Boolean = False; IncludeStartProcLineOffset: Boolean = False): Boolean;

//--------------------------------------------------------------------------------------------------
// Exception frame info routines
//--------------------------------------------------------------------------------------------------

type
  PJmpInstruction = ^TJmpInstruction;
  TJmpInstruction = packed record // from System.pas
    OpCode: Byte;
    Distance: Longint;
  end;

  TExcDescEntry = record // from System.pas
    VTable: Pointer;
    Handler: Pointer;
  end;

  PExcDesc = ^TExcDesc;
  TExcDesc = packed record // from System.pas
    JMP: TJmpInstruction;
    case Integer of
      0: (
        Instructions: array [0..0] of Byte);
      1: (
        Cnt: Integer;
        ExcTab: array [0..0] of TExcDescEntry);
  end;

  PExcFrame = ^TExcFrame;
  TExcFrame =  record // from System.pas
    Next: PExcFrame;
    Desc: PExcDesc;
    HEBP: Pointer;
    case Integer of
      0: ();
      1: (ConstructedObject: Pointer);
      2: (SelfOfMethod: Pointer);
  end;

  PJmpTable = ^TJmpTable;
  TJmpTable = packed record
    OPCode: Word; // FF 25 = JMP DWORD PTR [$xxxxxxxx], encoded as $25FF
    Ptr: Pointer;
  end;

  TExceptFrameKind = (efkUnknown, efkFinally, efkAnyException, efkOnException,
    efkAutoException);

  TJclExceptFrame = class (TObject)
  private
    FExcFrame: PExcFrame;
    FFrameKind: TExceptFrameKind;
  protected
    procedure DoDetermineFrameKind;
  public
    constructor Create(AExcFrame: PExcFrame);
    function Handles(ExceptObj: TObject): Boolean;
    function HandlerInfo(ExceptObj: TObject; var HandlerAt: Pointer): Boolean;
    function CodeLocation: Pointer;
    property ExcFrame: PExcFrame read FExcFrame;
    property FrameKind: TExceptFrameKind read FFrameKind;
  end;

  TJclExceptFrameList = class (TJclStackBaseList)
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

//--------------------------------------------------------------------------------------------------
// Global exceptional stack tracker enable routines and variables
//--------------------------------------------------------------------------------------------------

type
  TJclStackTrackingOption = (stStack, stExceptFrame, stRawMode, stAllModules, stStaticModuleList);
  TJclStackTrackingOptions = set of TJclStackTrackingOption;

var
  JclStackTrackingOptions: TJclStackTrackingOptions = [stStack];

function JclStartExceptionTracking: Boolean;
function JclStopExceptionTracking: Boolean;
function JclExceptionTrackingActive: Boolean;

//--------------------------------------------------------------------------------------------------
// Thread exception tracking support
//--------------------------------------------------------------------------------------------------

type
  TJclDebugThread = class (TThread)
  private
    FSyncException: Exception;
    FThreadName: string;
    procedure DoHandleException;
    function GetThreadInfo: string;
  protected
    procedure DoNotify;
    procedure DoSyncHandleException; dynamic;
    procedure HandleException;
  public
    constructor Create(Suspended: Boolean; const AThreadName: string = '');
    destructor Destroy; override;
    property SyncException: Exception read FSyncException;
    property ThreadInfo: string read GetThreadInfo;
    property ThreadName: string read FThreadName;
  end;

  TJclDebugThreadNotifyEvent = procedure (Thread: TJclDebugThread) of object;
  TJclThreadIDNotifyEvent = procedure (ThreadID: DWORD) of object;

  TJclDebugThreadList = class (TObject)
  private
    FList: TStringList;
    FLock: TJclCriticalSection;
    FReadLock: TJclCriticalSection;
    FRegSyncThreadID: DWORD;
    FUnregSyncThreadID: DWORD;
    FOnSyncException: TJclDebugThreadNotifyEvent;
    FOnThreadRegistered: TJclThreadIDNotifyEvent;
    FOnThreadUnregistered: TJclThreadIDNotifyEvent;
    procedure DoSyncThreadRegistered;
    procedure DoSyncThreadUnregistered;
    function GetThreadIDs(Index: Integer): DWORD;
    function GetThreadIDCount: Integer;
    function GetThreadNames(ThreadID: DWORD; Index: Integer): string;
  protected
    procedure DoSyncException(Thread: TJclDebugThread);
    procedure DoThreadRegistered(Thread: TThread);
    procedure DoThreadUnregistered(Thread: TThread);
    procedure InternalRegisterThread(Thread: TThread; const ThreadName: string);
    procedure InternalUnregisterThread(Thread: TThread);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterThread(Thread: TThread; const ThreadName: string);
    procedure UnregisterThread(Thread: TThread);
    property Lock: TJclCriticalSection read FLock;
    property ThreadClassNames[ThreadID: DWORD]: string index 1 read GetThreadNames;
    property ThreadIDs[Index: Integer]: DWORD read GetThreadIDs;
    property ThreadIDCount: Integer read GetThreadIDCount;
    property ThreadInfos[ThreadID: DWORD]: string index 2 read GetThreadNames;
    property ThreadNames[ThreadID: DWORD]: string index 0 read GetThreadNames;
    property OnSyncException: TJclDebugThreadNotifyEvent read FOnSyncException write FOnSyncException;
    property OnThreadRegistered: TJclThreadIDNotifyEvent read FOnThreadRegistered write FOnThreadRegistered;
    property OnThreadUnregistered: TJclThreadIDNotifyEvent read FOnThreadUnregistered write FOnThreadUnregistered;
  end;

function JclDebugThreadList: TJclDebugThreadList;

//--------------------------------------------------------------------------------------------------
// Miscellanuous
//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

function EnableCrashOnCtrlScroll(const Enable: Boolean): Boolean;
function IsDebuggerAttached: Boolean;
function IsHandleValid(Handle: THandle): Boolean;

{$ENDIF MSWINDOWS}

implementation

uses
  {$IFDEF MSWINDOWS}
  JclRegistry,
  {$ENDIF MSWINDOWS}
  JclHookExcept, JclLogic, JclStrings, JclSysInfo, JclSysUtils;

//==================================================================================================
// Helper assembler routines
//==================================================================================================

{$STACKFRAMES OFF}

function GetEBP: Pointer;
asm
  MOV EAX, EBP
end;

//--------------------------------------------------------------------------------------------------

function GetESP: Pointer;
asm
  MOV EAX, ESP
end;

//--------------------------------------------------------------------------------------------------

function GetFS: Pointer;
asm
  XOR EAX, EAX
  MOV EAX, FS:[EAX]
end;

//--------------------------------------------------------------------------------------------------

// Reference: Matt Pietrek, MSJ, Under the hood, on TIBs:
// http://msdn.microsoft.com/library/periodic/period96/S2CE.htm

function GetStackTop: DWORD;
asm
  MOV EAX, FS:[4]
end;

{$IFDEF STACKFRAMES_ON}
{$STACKFRAMES ON}
{$ENDIF STACKFRAMES_ON}

//==================================================================================================
// Diagnostics
//==================================================================================================

procedure AssertKindOf(const ClassName: string; const Obj: TObject);
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

//--------------------------------------------------------------------------------------------------

procedure AssertKindOf(const ClassType: TClass; const Obj: TObject);
begin
  Assert(Obj.InheritsFrom(ClassType));
end;

//--------------------------------------------------------------------------------------------------

procedure Trace(const Msg: string);
begin
  OutputDebugString(PChar(StrDoubleQuote(Msg)));
end;

//--------------------------------------------------------------------------------------------------

procedure TraceFmt(const Fmt: string; const Args: array of const);
begin
  OutputDebugString(PChar(Format(StrDoubleQuote(Fmt), Args)));
end;

//--------------------------------------------------------------------------------------------------

procedure TraceLoc(const Msg: string);
begin
  OutputDebugString(PChar(Format('%s:%u (%s) "%s"', [FileByLevel(1), LineByLevel(1),
    ProcByLevel(1), Msg])));
end;

//--------------------------------------------------------------------------------------------------

procedure TraceLocFmt(const Fmt: string; const Args: array of const);
var
  S: string;
begin
  S := Format('%s:%u (%s) ', [FileByLevel(1), LineByLevel(1), ProcByLevel(1)]) +
    Format(StrDoubleQuote(Fmt), Args);
  OutputDebugString(PChar(S));
end;

//==================================================================================================
// TJclModuleInfoList
//==================================================================================================

procedure TJclModuleInfoList.BuildModulesList;
var
  List: TStringList;
  I: Integer;
  CurModule: PLibModule;
begin
  if FSystemModulesOnly then
  begin
    CurModule := LibModuleList;
    while CurModule <> nil do
    begin
      CreateItemForAddress(Pointer(CurModule.Instance), True);
      CurModule := CurModule.Next;
    end;
  end
  else
  begin
    List := TStringList.Create;
    try
      LoadedModulesList(List, GetCurrentProcessId, True);
      for I := 0 to List.Count - 1 do
        CreateItemForAddress(List.Objects[I], False);
    finally
      List.Free;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

constructor TJclModuleInfoList.Create(ADynamicBuild, ASystemModulesOnly: Boolean);
begin
  inherited Create(True);
  FDynamicBuild := ADynamicBuild;
  FSystemModulesOnly := ASystemModulesOnly;
  if not FDynamicBuild then
    BuildModulesList;
end;

//--------------------------------------------------------------------------------------------------

function TJclModuleInfoList.CreateItemForAddress(Addr: Pointer; SystemModule: Boolean): TJclModuleInfo;
var
  Module: HMODULE;
  NtHeaders: PImageNtHeaders;
begin
  Result := nil;
  Module := ModuleFromAddr(Addr);
  if Module > 0 then
  begin
    NtHeaders := PeMapImgNtHeaders(Pointer(Module));
    if NtHeaders <> nil then
    begin
      Result := TJclModuleInfo.Create;
      Result.FStartAddr := Pointer(Module);
      Result.FSize := NtHeaders^.OptionalHeader.SizeOfImage;
      Result.FEndAddr := Pointer(Module + Result.FSize - 1);
      if SystemModule then
        Result.FSystemModule := True
      else
        Result.FSystemModule := IsSystemModule(Module);
    end;
  end;
  if Result <> nil then
    Add(Result);
end;

//--------------------------------------------------------------------------------------------------

function TJclModuleInfoList.GetItems(Index: Integer): TJclModuleInfo;
begin
  Result := TJclModuleInfo(Get(Index));
end;

//--------------------------------------------------------------------------------------------------

function TJclModuleInfoList.GetModuleFromAddress(Addr: Pointer): TJclModuleInfo;
var
  I: Integer;
  Item: TJclModuleInfo;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    Item := Items[I];
    if (Cardinal(Item.StartAddr) <= Cardinal(Addr)) and (Cardinal(Item.EndAddr) > Cardinal(Addr)) then
    begin
      Result := Item;
      Break;
    end;
  end;
  if DynamicBuild and (Result = nil) then
    Result := CreateItemForAddress(Addr, False);
end;

//--------------------------------------------------------------------------------------------------

function TJclModuleInfoList.IsSystemModuleAddress(Addr: Pointer): Boolean;
var
  Item: TJclModuleInfo;
begin
  Item := ModuleFromAddress[Addr];
  Result := (Item <> nil) and Item.SystemModule;
end;

//--------------------------------------------------------------------------------------------------

function TJclModuleInfoList.IsValidModuleAddress(Addr: Pointer): Boolean;
begin
  Result := ModuleFromAddress[Addr] <> nil;
end;

//==================================================================================================
// TJclAbstractMapParser
//==================================================================================================

constructor TJclAbstractMapParser.Create(const MapFileName: TFileName);
begin
  if FileExists(MapFileName) then
    FStream := TJclFileMappingStream.Create(MapFileName, fmOpenRead or fmShareDenyWrite);
end;

//--------------------------------------------------------------------------------------------------

destructor TJclAbstractMapParser.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TJclAbstractMapParser.GetLinkerBugUnitName: string;
begin
  Result := MapStringToStr(FLinkerBugUnitName);
end;

//--------------------------------------------------------------------------------------------------

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
  end
  else
  begin
    P := MapString;
    while not (P^ in [AnsiSpace, AnsiCarriageReturn, '(']) do
      Inc(P);
  end;
  SetString(Result, MapString, P - MapString);
end;

//--------------------------------------------------------------------------------------------------

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
  A, PreviousA: TJclMapAddress;
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

{$OVERFLOWCHECKS OFF}

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
        'a'..'f':
          begin
            Result := Result * 16;
            Inc(Result, Ord(C) - Ord('a') + 10);
          end;
        'H', 'h':
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

{$IFDEF OVERFLOWCHECKS_ON}
{$OVERFLOWCHECKS ON}
{$ENDIF OVERFLOWCHECKS_ON}

  function ReadAddress: TJclMapAddress;
  begin
    Result.Segment := ReadHexValue;
    if CurrPos^ = ':' then
    begin
      Inc(CurrPos);
      Result.Offset := ReadHexValue;
    end
    else
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
  if FStream <> nil then
  begin
    FLinkerBug := False;
    PreviousA.Segment := 0;
    PreviousA.Offset := 0;
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
        if (not FLinkerBug) and (A.Offset < PreviousA.Offset) then
        begin
          FLinkerBugUnitName := FLastUnitName;
          FLinkerBug := True;
        end;
        PreviousA := A;
      until not IsDecDigit;
    end;
  end;
end;

//==================================================================================================
// TJclMapParser
//==================================================================================================

procedure TJclMapParser.ClassTableItem(const Address: TJclMapAddress;
  Len: Integer; SectionName, GroupName: PJclMapString);
begin
  if Assigned(FOnClassTable) then
    FOnClassTable(Self, Address, Len, MapStringToStr(SectionName), MapStringToStr(GroupName));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMapParser.LineNumbersItem(LineNumber: Integer; const Address: TJclMapAddress);
begin
  if Assigned(FOnLineNumbers) then
    FOnLineNumbers(Self, LineNumber, Address);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMapParser.LineNumberUnitItem(UnitName, UnitFileName: PJclMapString);
begin
  if Assigned(FOnLineNumberUnit) then
    FOnLineNumberUnit(Self, MapStringToStr(UnitName), MapStringToStr(UnitFileName));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMapParser.PublicsByNameItem(const Address: TJclMapAddress;
  Name: PJclMapString);
begin
  if Assigned(FOnPublicsByName) then
    FOnPublicsByName(Self, Address, MapStringToStr(Name));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMapParser.PublicsByValueItem(const Address: TJclMapAddress;
  Name: PJclMapString);
begin
  if Assigned(FOnPublicsByValue) then
    FOnPublicsByValue(Self, Address, MapStringToStr(Name));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMapParser.SegmentItem(const Address: TJclMapAddress;
  Len: Integer; GroupName, UnitName: PJclMapString);
begin
  if Assigned(FOnSegmentItem) then
    FOnSegmentItem(Self, Address, Len, MapStringToStr(GroupName), MapStringToStr(UnitName));
end;

//==================================================================================================
// TJclMapScanner
//==================================================================================================

procedure TJclMapScanner.ClassTableItem(const Address: TJclMapAddress; Len: Integer;
  SectionName, GroupName: PJclMapString);
begin
end;

//--------------------------------------------------------------------------------------------------

constructor TJclMapScanner.Create(const MapFileName: TFileName);
begin
  inherited;
  Scan;
end;

//--------------------------------------------------------------------------------------------------

function TJclMapScanner.LineNumberFromAddr(Addr: DWORD): Integer;
var
  Dummy: Integer;
begin
  Result := LineNumberFromAddr(Addr, Dummy);
end;

//--------------------------------------------------------------------------------------------------

function TJclMapScanner.LineNumberFromAddr(Addr: DWORD; var Offset: Integer): Integer;
var
  I: Integer;
  ModuleStartAddr: DWORD;
begin
  ModuleStartAddr := ModuleStartFromAddr(Addr);
  Result := 0;
  Offset := 0;
  for I := Length(FLineNumbers) - 1 downto 0 do
    if FLineNumbers[I].Addr <= Addr then
    begin
      if FLineNumbers[I].Addr >= ModuleStartAddr then
      begin
        Result := FLineNumbers[I].LineNumber;
        Offset := Addr - FLineNumbers[I].Addr;
      end;
      Break;
    end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMapScanner.LineNumbersItem(LineNumber: Integer; const Address: TJclMapAddress);
var
  C: Integer;
begin
  // Try to eliminate invalid line numbers caused by bug in the linker
  if (FLastValidAddr.Offset = 0) or ((Address.Offset > 0) and (Address.Offset <= FTopValidAddr) and
    (FLastValidAddr.Segment = Address.Segment) and (FLastValidAddr.Offset < Address.Offset)) then
  begin
    FLastValidAddr := Address;
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
  end
  else
    Inc(FLineNumberErrors);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMapScanner.LineNumberUnitItem(UnitName, UnitFileName: PJclMapString);
begin
  FNewUnitFileName := UnitFileName;
end;

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

function TJclMapScanner.ModuleStartFromAddr(Addr: DWORD): DWORD;
var
  I: Integer;
begin
  Result := DWORD(-1);
  for I := Length(FSegments) - 1 downto 0 do
    if (FSegments[I].StartAddr <= Addr) and (FSegments[I].EndAddr >= Addr) then
    begin
      Result := FSegments[I].StartAddr;
      Break;
    end;
end;

//--------------------------------------------------------------------------------------------------

function TJclMapScanner.ProcNameFromAddr(Addr: DWORD): string;
var
  Dummy: Integer;
begin
  Result := ProcNameFromAddr(Addr, Dummy);
end;

//--------------------------------------------------------------------------------------------------

function TJclMapScanner.ProcNameFromAddr(Addr: DWORD; var Offset: Integer): string;
var
  I: Integer;
  ModuleStartAddr: DWORD;
begin
  ModuleStartAddr := ModuleStartFromAddr(Addr);
  Result := '';
  Offset := 0;
  for I := Length(FProcNames) - 1 downto 0 do
    if FProcNames[I].Addr <= Addr then
    begin
      if FProcNames[I].Addr >= ModuleStartAddr then
      begin
        Result := MapStringToStr(FProcNames[I].ProcName);
        Offset := Addr - FProcNames[I].Addr;
      end;
      Break;
    end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMapScanner.PublicsByNameItem(const Address: TJclMapAddress;  Name: PJclMapString);
begin
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMapScanner.PublicsByValueItem(const Address: TJclMapAddress; Name: PJclMapString);
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

//--------------------------------------------------------------------------------------------------

procedure TJclMapScanner.Scan;
begin
  FLastValidAddr.Segment := 0;
  FLastValidAddr.Offset := 0;
  FTopValidAddr := 0;
  FLineNumberErrors := 0;
  Parse;
  SetLength(FLineNumbers, FLineNumbersCnt);
  SetLength(FProcNames, FProcNamesCnt);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMapScanner.SegmentItem(const Address: TJclMapAddress; Len: Integer;
  GroupName, UnitName: PJclMapString);
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
    FTopValidAddr := Max(FTopValidAddr, Address.Offset + Len);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclMapScanner.SourceNameFromAddr(Addr: DWORD): string;
var
  I: Integer;
  ModuleStartAddr: DWORD;
begin
  ModuleStartAddr := ModuleStartFromAddr(Addr);
  Result := '';
  for I := Length(FSourceNames) - 1 downto 0 do
    if FSourceNames[I].Addr <= Addr then
    begin
      if FSourceNames[I].Addr >= ModuleStartAddr then
        Result := MapStringToStr(FSourceNames[I].ProcName);
      Break;
    end;
end;

//==================================================================================================
// JCL binary debug format string encoding/decoding routines
//==================================================================================================

{ Strings are compressed to following 6bit format (A..D represents characters) and terminated with }
{ 6bit #0 char. First char = #1 indicates non compressed text, #2 indicates compressed text with   }
{ leading '@' character                                                                            }
{                                                                                                  }
{ 7   6   5   4   3   2   1   0  |                                                                 }
{---------------------------------                                                                 }
{ B1  B0  A5  A4  A3  A2  A1  A0 | Data byte 0                                                     }
{---------------------------------                                                                 }
{ C3  C2  C1  C0  B5  B4  B3  B2 | Data byte 1                                                     }
{---------------------------------                                                                 }
{ D5  D4  D3  D2  D1  D0  C5  C4 | Data byte 2                                                     }
{---------------------------------                                                                 }

//--------------------------------------------------------------------------------------------------

{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

function SimpleCryptString(const S: string): string;
var
  I: Integer;
  C: Byte;
  P: PByte;
begin
  SetLength(Result, Length(S));
  P := PByte(Result);
  for I := 1 to Length(S) do
  begin
    C := Ord(S[I]);
    if C <> $AA then
      C := C xor $AA;
    P^ := C;
    Inc(P);
  end;
end;

//--------------------------------------------------------------------------------------------------

function DecodeNameString(const S: PChar): string;
var
  I, B: Integer;
  C: Byte;
  P: PByte;
  Buffer: array[0..255] of Char;
begin
  Result := '';
  B := 0;
  P := PByte(S);
  case P^ of
    1:
      begin
        Inc(P);
        Result := SimpleCryptString(PChar(P));
        Exit;
      end;
    2:
      begin
        Inc(P);
        Buffer[B] := '@';
        Inc(B);
      end;
  end;
  I := 0;
  C := 0;
  repeat
    case I and $03 of
      0:
        C := P^ and $3F;
      1:
        begin
          C := (P^ shr 6) and $03;
          Inc(P);
          Inc(C, (P^ and $0F) shl 2);
        end;
      2:
        begin
          C := (P^ shr 4) and $0F;
          Inc(P);
          Inc(C, (P^ and $03) shl 4);
        end;
      3:
        begin
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
    Buffer[B] := Chr(C);
    Inc(B);
    Inc(I);
  until B >= SizeOf(Buffer) - 1;
  Buffer[B] := AnsiNull;
  Result := Buffer;
end;

//--------------------------------------------------------------------------------------------------

function EncodeNameString(const S: string): string;
const
  ValidChars = ['0'..'9', 'A'..'Z', 'a'..'z', '_'];
var
  I, StartIndex: Integer;
  C: Byte;
  P: PByte;
begin
  if (Length(S) > 1) and (S[1] = '@') then
    StartIndex := 1
  else
    StartIndex := 0;
  for I := StartIndex + 1 to Length(S) do
    if not (S[I] in ValidChars) then
    begin
      Result := #1 + SimpleCryptString(S) + #0;
      Exit;
    end;
  SetLength(Result, Length(S) + StartIndex);
  P := Pointer(Result);
  if StartIndex = 1 then
    P^ := 2 // store '@' leading char information
  else
    Dec(P);
  for I := 0 to Length(S) - StartIndex do // including null char
  begin
    C := Byte(S[I + 1 + StartIndex]);
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
      0: 
        begin
          Inc(P);
          P^ := C;
        end;
      1: 
        begin
          P^ := P^ or (C and $03) shl 6;
          Inc(P);
          P^ := (C shr 2) and $0F;
        end;
      2:
        begin
          P^ := P^ or (C shl 4);
          Inc(P);
          P^ := (C shr 4) and $03;
        end;
      3:
        P^ := P^ or (C shl 2);
    end;
  end;
  SetLength(Result, DWORD(P) - DWORD(Pointer(Result)) + 1);
end;

{$IFDEF RANGECHECKS_ON}
{$RANGECHECKS ON}
{$ENDIF RANGECHECKS_ON}

{$IFDEF OVERFLOWCHECKS_ON}
{$OVERFLOWCHECKS ON}
{$ENDIF OVERFLOWCHECKS_ON}

//--------------------------------------------------------------------------------------------------

function ConvertMapFileToJdbgFile(const MapFileName: TFileName): Boolean;
var
  Dummy1: string;
  Dummy2: Integer;
begin
  Result := ConvertMapFileToJdbgFile(MapFileName, Dummy1, Dummy2);
end;

//--------------------------------------------------------------------------------------------------

function ConvertMapFileToJdbgFile(const MapFileName: TFileName; var LinkerBugUnit: string;
  var LineNumberErrors: Integer): Boolean;
var
  JDbgFileName: TFileName;
  Generator: TJclBinDebugGenerator;
begin
  JDbgFileName := ChangeFileExt(MapFileName, JclDbgFileExtension);
  Generator := TJclBinDebugGenerator.Create(MapFileName);
  try
    Result := (Generator.DataStream.Size > 0) and Generator.CalculateCheckSum;
    if Result then
      Generator.DataStream.SaveToFile(JDbgFileName);
    LinkerBugUnit := Generator.LinkerBugUnitName;
    LineNumberErrors := Generator.LineNumberErrors;  
  finally
    Generator.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

function InsertDebugDataIntoExecutableFile(const ExecutableFileName, MapFileName: TFileName;
  var LinkerBugUnit: string; var MapFileSize, JclDebugDataSize: Integer): Boolean;
var
  Dummy: Integer;
begin
  Result := InsertDebugDataIntoExecutableFile(ExecutableFileName, MapFileName, LinkerBugUnit,
    MapFileSize, JclDebugDataSize, Dummy);
end;

//--------------------------------------------------------------------------------------------------

function InsertDebugDataIntoExecutableFile(const ExecutableFileName, MapFileName: TFileName;
  var LinkerBugUnit: string; var MapFileSize, JclDebugDataSize, LineNumberErrors: Integer): Boolean;
var
  BinDebug: TJclBinDebugGenerator;
begin
  BinDebug := TJclBinDebugGenerator.Create(MapFileName);
  try
    Result := InsertDebugDataIntoExecutableFile(ExecutableFileName, BinDebug,
      LinkerBugUnit, MapFileSize, JclDebugDataSize, LineNumberErrors);
  finally
    BinDebug.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

function InsertDebugDataIntoExecutableFile(const ExecutableFileName: TFileName;
  BinDebug: TJclBinDebugGenerator; var LinkerBugUnit: string;
  var MapFileSize, JclDebugDataSize: Integer): Boolean;
var
  Dummy: Integer;
begin
  Result := InsertDebugDataIntoExecutableFile(ExecutableFileName, BinDebug, LinkerBugUnit,
    MapFileSize, JclDebugDataSize, Dummy);
end;

//--------------------------------------------------------------------------------------------------

function InsertDebugDataIntoExecutableFile(const ExecutableFileName: TFileName;
  BinDebug: TJclBinDebugGenerator; var LinkerBugUnit: string;
  var MapFileSize, JclDebugDataSize, LineNumberErrors: Integer): Boolean;
var
  ImageStream: TMemoryStream;
  NtHeaders: PImageNtHeaders;
  Sections, LastSection, JclDebugSection: PImageSectionHeader;
  VirtualAlignedSize: DWORD;
  I, X, NeedFill: Integer;

  procedure RoundUpToAlignment(var Value: DWORD; Alignment: DWORD);
  begin
    if (Value mod Alignment) <> 0 then
      Value := ((Value div Alignment) + 1) * Alignment;
  end;

begin
  MapFileSize := 0;
  JclDebugDataSize := 0;
  LineNumberErrors := 0;
  LinkerBugUnit := '';
  if BinDebug.Stream <> nil then
  begin
    Result := True;
    if BinDebug.LinkerBug then
    begin
      LinkerBugUnit := BinDebug.LinkerBugUnitName;
      LineNumberErrors := BinDebug.LineNumberErrors;
    end;
  end
  else
    Result := False;
  if not Result then
    Exit;

  ImageStream := TMemoryStream.Create;
  try
    try
      ImageStream.LoadFromFile(ExecutableFileName);
      MapFileSize := BinDebug.Stream.Size;
      JclDebugDataSize := BinDebug.DataStream.Size;
      NtHeaders := PeMapImgNtHeaders(ImageStream.Memory);
      Assert(NtHeaders <> nil);
      Sections := PeMapImgSections(NtHeaders);
      Assert(Sections <> nil);
      // Check whether there is not a section with the name already. This
      // should never occur.
      Assert(PeMapImgFindSection(NtHeaders, JclDbgDataResName) = nil);
      LastSection := Sections;
      Inc(LastSection, NtHeaders^.FileHeader.NumberOfSections - 1);
      JclDebugSection := LastSection;
      Inc(JclDebugSection);

      // Increase the number of sections
      Inc(NtHeaders^.FileHeader.NumberOfSections);
      FillChar(JclDebugSection^, SizeOf(TImageSectionHeader), #0);
      // JCLDEBUG Virtual Address
      JclDebugSection^.VirtualAddress := LastSection^.VirtualAddress + LastSection^.Misc.VirtualSize;
      RoundUpToAlignment(JclDebugSection^.VirtualAddress, NtHeaders^.OptionalHeader.SectionAlignment);
      // JCLDEBUG Physical Ofset
      JclDebugSection^.PointerToRawData := LastSection^.PointerToRawData + LastSection^.SizeOfRawData;
      RoundUpToAlignment(JclDebugSection^.PointerToRawData, NtHeaders^.OptionalHeader.FileAlignment);
      // JCLDEBUG Section name
      StrPLCopy(PChar(@JclDebugSection^.Name), JclDbgDataResName, IMAGE_SIZEOF_SHORT_NAME);
      // JCLDEBUG Characteristics flags
      JclDebugSection^.Characteristics := IMAGE_SCN_MEM_READ or IMAGE_SCN_CNT_INITIALIZED_DATA;

      // Size of virtual data area
      JclDebugSection^.Misc.VirtualSize := JclDebugDataSize;
      VirtualAlignedSize := JclDebugDataSize;
      RoundUpToAlignment(VirtualAlignedSize, NtHeaders^.OptionalHeader.SectionAlignment);
      // Update Size of Image
      Inc(NtHeaders^.OptionalHeader.SizeOfImage, VirtualAlignedSize);
      // Raw data size
      JclDebugSection^.SizeOfRawData := JclDebugDataSize;
      RoundUpToAlignment(JclDebugSection^.SizeOfRawData, NtHeaders^.OptionalHeader.FileAlignment);
      // Update Initialized data size
      Inc(NtHeaders^.OptionalHeader.SizeOfInitializedData, JclDebugSection^.SizeOfRawData);

      // Fill data to alignment
      NeedFill := Integer(JclDebugSection^.SizeOfRawData) - JclDebugDataSize;

      // Note: Delphi linker seems to generate incorrect (unaligned) size of
      // the executable when adding TD32 debug data so the position could be
      // behind the size of the file then.
      ImageStream.Seek(JclDebugSection^.PointerToRawData, soFromBeginning);
      ImageStream.CopyFrom(BinDebug.DataStream, 0);
      X := 0;
      for I := 1 to NeedFill do
        ImageStream.WriteBuffer(X, 1);

      ImageStream.SaveToFile(ExecutableFileName);
    except
      Result := False;
    end;    
  finally
    ImageStream.Free;
  end;
end;

//==================================================================================================
// TJclBinDebugGenerator
//==================================================================================================

{$OVERFLOWCHECKS OFF}

function TJclBinDebugGenerator.CalculateCheckSum: Boolean;
var
  Header: PJclDbgHeader;
  P, EndData: PChar;
  CheckSum: Integer;
begin
  Result := DataStream.Size >= SizeOf(TJclDbgHeader);
  if Result then
  begin
    P := DataStream.Memory;
    EndData := P + DataStream.Size;
    Header := PJclDbgHeader(P);
    CheckSum := 0;
    Header^.CheckSum := 0;
    Header^.CheckSumValid := True;
    while P < EndData do
    begin
      Inc(CheckSum, PInteger(P)^);
      Inc(PInteger(P));
    end;
    Header^.CheckSum := CheckSum;
  end;
end;

{$IFDEF OVERFLOWCHECKS_ON}
{$OVERFLOWCHECKS ON}
{$ENDIF OVERFLOWCHECKS_ON}

//--------------------------------------------------------------------------------------------------

constructor TJclBinDebugGenerator.Create(const MapFileName: TFileName);
begin
  inherited;
  FDataStream := TMemoryStream.Create;
  FMapFileName := MapFileName;
  if FStream <> nil then
    CreateData;
end;

//--------------------------------------------------------------------------------------------------

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
    end
    else
      Result := DWORD(WordList.Objects[N]);
    Inc(Result);
  end;

  procedure WriteValue(Value: Integer);
  var
    L: Integer;
    D: DWORD;
    P: array [1..5] of Byte;
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
    FileHeader.Version := JclDbgHeaderVersion;
    FileHeader.CheckSum := 0;
    FileHeader.CheckSumValid := False;
    FileHeader.ModuleName := AddWord(PathExtractFileNameNoExt(FMapFileName));
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
      end
      else
      if D = 0 then
      begin
        FirstWord := AddWord(S);
        SecondWord := 0;
      end
      else
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
    I := 0;
    while FDataStream.Size mod 4 <> 0 do
      FDataStream.WriteBuffer(I, 1);
    FDataStream.Seek(0, soFromBeginning);
    FDataStream.WriteBuffer(FileHeader, SizeOf(FileHeader));
  finally
    WordStream.Free;
    WordList.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclBinDebugGenerator.Destroy;
begin
  FreeAndNil(FDataStream);
  inherited;
end;

//==================================================================================================
// TJclBinDebugScanner
//==================================================================================================

procedure TJclBinDebugScanner.CacheLineNumbers;
var
  P: Pointer;
  Value, LineNumber, C: Integer;
  CurrAddr: DWORD;
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

//--------------------------------------------------------------------------------------------------

procedure TJclBinDebugScanner.CacheProcNames;
var
  P: Pointer;
  Value, FirstWord, SecondWord, C: Integer;
  CurrAddr: DWORD;
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

//--------------------------------------------------------------------------------------------------

{$OVERFLOWCHECKS OFF}

procedure TJclBinDebugScanner.CheckFormat;
var
  CheckSum: Integer;
  Data, EndData: PChar;
  Header: PJclDbgHeader;
begin
  Data := FStream.Memory;
  Header := PJclDbgHeader(Data);
  FValidFormat := (Data <> nil) and (FStream.Size > SizeOf(TJclDbgHeader)) and
    (FStream.Size mod 4 = 0) and
    (Header^.Signature = JclDbgDataSignature) and (Header^.Version = JclDbgHeaderVersion);
  if FValidFormat and Header^.CheckSumValid then
  begin
    CheckSum := -Header^.CheckSum;
    EndData := Data + FStream.Size;
    while Data < EndData do
    begin
      Inc(CheckSum, PInteger(Data)^);
      Inc(PInteger(Data));
    end;
    CheckSum := (CheckSum shr 8) or (CheckSum shl 24);
    FValidFormat := (CheckSum = Header^.CheckSum);
  end;
end;

{$IFDEF OVERFLOWCHECKS_ON}
{$OVERFLOWCHECKS ON}
{$ENDIF OVERFLOWCHECKS_ON}

//--------------------------------------------------------------------------------------------------

constructor TJclBinDebugScanner.Create(AStream: TCustomMemoryStream; CacheData: Boolean);
begin
  FCacheData := CacheData;
  FStream := AStream;
  CheckFormat;
end;

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

function TJclBinDebugScanner.GetModuleName: string;
begin
  Result := DataToStr(PJclDbgHeader(FStream.Memory)^.ModuleName);
end;

//--------------------------------------------------------------------------------------------------

function TJclBinDebugScanner.IsModuleNameValid(const Name: TFileName): Boolean;
begin
  Result := AnsiSameText(ModuleName, PathExtractFileNameNoExt(Name));
end;

//--------------------------------------------------------------------------------------------------

function TJclBinDebugScanner.LineNumberFromAddr(Addr: DWORD): Integer;
var
  Dummy: Integer;
begin
  Result := LineNumberFromAddr(Addr, Dummy);
end;

//--------------------------------------------------------------------------------------------------

function TJclBinDebugScanner.LineNumberFromAddr(Addr: DWORD; var Offset: Integer): Integer;
var
  P: Pointer;
  Value, LineNumber: Integer;
  CurrAddr, ModuleStartAddr, ItemAddr: DWORD;
begin
  ModuleStartAddr := ModuleStartFromAddr(Addr);
  LineNumber := 0;
  Offset := 0;
  if FCacheData then
  begin
    CacheLineNumbers;
    for Value := Length(FLineNumbers) - 1 downto 0 do
      if FLineNumbers[Value].Addr <= Addr then
      begin
        if FLineNumbers[Value].Addr >= ModuleStartAddr then
        begin
          LineNumber := FLineNumbers[Value].LineNumber;
          Offset := Addr - FLineNumbers[Value].Addr;
        end;
        Break;
      end;
  end
  else
  begin
    P := MakePtr(PJclDbgHeader(FStream.Memory)^.LineNumbers);
    CurrAddr := 0;
    ItemAddr := 0;
    while ReadValue(P, Value) do
    begin
      Inc(CurrAddr, Value);
      if Addr < CurrAddr then
      begin
        if ItemAddr < ModuleStartAddr then
        begin
          LineNumber := 0;
          Offset := 0;
        end;
        Break;
      end
      else
      begin
        ItemAddr := CurrAddr;
        ReadValue(P, Value);
        Inc(LineNumber, Value);
        Offset := Addr - CurrAddr;
      end;
    end;
  end;
  Result := LineNumber;
end;

//--------------------------------------------------------------------------------------------------

function TJclBinDebugScanner.MakePtr(A: Integer): Pointer;
begin
  Result := Pointer(DWORD(FStream.Memory) + DWORD(A));
end;

//--------------------------------------------------------------------------------------------------

function TJclBinDebugScanner.ModuleNameFromAddr(Addr: DWORD): string;
var
  Value, Name: Integer;
  StartAddr: DWORD;
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

//--------------------------------------------------------------------------------------------------

function TJclBinDebugScanner.ModuleStartFromAddr(Addr: DWORD): DWORD;
var
  Value: Integer;
  StartAddr, ModuleStartAddr: DWORD;
  P: Pointer;
begin
  P := MakePtr(PJclDbgHeader(FStream.Memory)^.Units);
  StartAddr := 0;
  ModuleStartAddr := DWORD(-1);
  while ReadValue(P, Value) do
  begin
    Inc(StartAddr, Value);
    if Addr < StartAddr then
      Break
    else
    begin
      ReadValue(P, Value);
      ModuleStartAddr := StartAddr;
    end;
  end;
  Result := ModuleStartAddr;
end;

//--------------------------------------------------------------------------------------------------

function TJclBinDebugScanner.ProcNameFromAddr(Addr: DWORD): string;
var
  Dummy: Integer;
begin
  Result := ProcNameFromAddr(Addr, Dummy);
end;

//--------------------------------------------------------------------------------------------------

function TJclBinDebugScanner.ProcNameFromAddr(Addr: DWORD; var Offset: Integer): string;
var
  P: Pointer;
  Value, FirstWord, SecondWord: Integer;
  CurrAddr, ModuleStartAddr, ItemAddr: DWORD;
begin
  ModuleStartAddr := ModuleStartFromAddr(Addr);
  FirstWord := 0;
  SecondWord := 0;
  Offset := 0;
  if FCacheData then
  begin
    CacheProcNames;
    for Value := Length(FProcNames) - 1 downto 0 do
      if FProcNames[Value].Addr <= Addr then
      begin
        if FProcNames[Value].Addr >= ModuleStartAddr then
        begin
          FirstWord := FProcNames[Value].FirstWord;
          SecondWord := FProcNames[Value].SecondWord;
          Offset := Addr - FProcNames[Value].Addr;
        end;
        Break;
      end;
  end
  else
  begin
    P := MakePtr(PJclDbgHeader(FStream.Memory)^.Symbols);
    CurrAddr := 0;
    ItemAddr := 0;
    while ReadValue(P, Value) do
    begin
      Inc(CurrAddr, Value);
      if Addr < CurrAddr then
      begin
        if ItemAddr < ModuleStartAddr then
        begin
          FirstWord := 0;
          SecondWord := 0;
          Offset := 0;
        end;
        Break;
      end
      else
      begin
        ItemAddr := CurrAddr;
        ReadValue(P, Value);
        Inc(FirstWord, Value);
        ReadValue(P, Value);
        Inc(SecondWord, Value);
        Offset := Addr - CurrAddr;
      end;
    end;
  end;
  if FirstWord <> 0 then
  begin
    Result := DataToStr(FirstWord);
    if SecondWord <> 0 then
      Result := Result + '.' + DataToStr(SecondWord)
  end
  else
    Result := '';
end;

//--------------------------------------------------------------------------------------------------

function TJclBinDebugScanner.ReadValue(var P: Pointer; var Value: Integer): Boolean;
var
  N: Integer;
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

//--------------------------------------------------------------------------------------------------

function TJclBinDebugScanner.SourceNameFromAddr(Addr: DWORD): string;
var
  Value, Name: Integer;
  StartAddr, ModuleStartAddr, ItemAddr: DWORD;
  P: Pointer;
begin
  ModuleStartAddr := ModuleStartFromAddr(Addr);
  P := MakePtr(PJclDbgHeader(FStream.Memory)^.SourceNames);
  Name := 0;
  StartAddr := 0;
  ItemAddr := 0;
  while ReadValue(P, Value) do
  begin
    Inc(StartAddr, Value);
    if Addr < StartAddr then
    begin
      if ItemAddr < ModuleStartAddr then
        Name := 0;
      Break;
    end
    else
    begin
      ItemAddr := StartAddr;
      ReadValue(P, Value);
      Inc(Name, Value);
    end;
  end;
  Result := DataToStr(Name);
end;

//==================================================================================================
// TJclDebugInfoSource
//==================================================================================================

constructor TJclDebugInfoSource.Create(AModule: HMODULE);
begin
  FModule := AModule;
end;

//--------------------------------------------------------------------------------------------------

function TJclDebugInfoSource.GetFileName: TFileName;
begin
  Result := GetModulePath(FModule);
end;

//--------------------------------------------------------------------------------------------------

function TJclDebugInfoSource.VAFromAddr(const Addr: Pointer): DWORD;
begin
  Result := DWORD(Addr) - FModule - $1000;
end;

//==================================================================================================
// TJclDebugInfoList
//==================================================================================================

var
  DebugInfoList: TJclDebugInfoList;
  DebugInfoCritSect: TJclCriticalSection;

//--------------------------------------------------------------------------------------------------

procedure NeedDebugInfoList;
begin
  if DebugInfoList = nil then
    DebugInfoList := TJclDebugInfoList.Create;
end;

//--------------------------------------------------------------------------------------------------

function TJclDebugInfoList.CreateDebugInfo(const Module: HMODULE): TJclDebugInfoSource;
const
  DebugInfoSources: array [1..4] of TJclDebugInfoSourceClass =
    (TJclDebugInfoBinary, TJclDebugInfoTD32, TJclDebugInfoMap, TJclDebugInfoExports);
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

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

function TJclDebugInfoList.GetItems(Index: Integer): TJclDebugInfoSource;
begin
  Result := TJclDebugInfoSource(Get(Index));
end;

//--------------------------------------------------------------------------------------------------

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

//==================================================================================================
// TJclDebugInfoMap
//==================================================================================================

destructor TJclDebugInfoMap.Destroy;
begin
  FreeAndNil(FScanner);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

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
      Info.ProcedureName := ProcNameFromAddr(VA, Info.OffsetFromProcName);
      Info.LineNumber := LineNumberFromAddr(VA, Info.OffsetFromLineNumber);
      Info.SourceName := SourceNameFromAddr(VA);
      Info.DebugInfo := Self;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclDebugInfoMap.InitializeSource: Boolean;
var
  MapFileName: TFileName;
begin
  MapFileName := ChangeFileExt(FileName, MapFileExtension);
  Result := FileExists(MapFileName);
  if Result then
    FScanner := TJclMapScanner.Create(MapFileName);
end;

//==================================================================================================
// TJclDebugInfoBinary
//==================================================================================================

destructor TJclDebugInfoBinary.Destroy;
begin
  FreeAndNil(FScanner);
  FreeAndNil(FStream);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

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
      Info.ProcedureName := ProcNameFromAddr(VA, Info.OffsetFromProcName);
      Info.LineNumber := LineNumberFromAddr(VA, Info.OffsetFromLineNumber);
      Info.SourceName := SourceNameFromAddr(VA);
      Info.DebugInfo := Self;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclDebugInfoBinary.InitializeSource: Boolean;
var
  JdbgFileName: TFileName;
  VerifyFileName: Boolean;
begin
  VerifyFileName := False;
  Result := (PeMapImgFindSection(PeMapImgNtHeaders(Pointer(Module)), JclDbgDataResName) <> nil);
  if Result then
    FStream := TJclPeSectionStream.Create(Module, JclDbgDataResName)
  else
  begin
    JdbgFileName := ChangeFileExt(FileName, JclDbgFileExtension);
    Result := FileExists(JdbgFileName);
    if Result then
    begin
      FStream := TJclFileMappingStream.Create(JdbgFileName, fmOpenRead or fmShareDenyWrite);
      VerifyFileName := True;
    end;
  end;
  if Result then
  begin
    FScanner := TJclBinDebugScanner.Create(FStream, True);
    Result := FScanner.ValidFormat and
      (not VerifyFileName or FScanner.IsModuleNameValid(FileName));
  end;
end;

//==================================================================================================
// TJclDebugInfoExports
//==================================================================================================

destructor TJclDebugInfoExports.Destroy;
begin
  FreeAndNil(FBorImage);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TJclDebugInfoExports.GetLocationInfo(const Addr: Pointer; var Info: TJclLocationInfo): Boolean;
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
  Info.OffsetFromProcName := 0;
  Info.OffsetFromLineNumber := 0;
  with FBorImage.ExportList do
  begin
    SortList(esAddress, False);
    for I := Count - 1 downto 0 do
      if Items[I].Address <= VA then
      begin
        if RawName then
        begin
          Info.ProcedureName := Items[I].Name;
          Info.OffsetFromProcName := VA - Items[I].Address;
          Result := True;
        end
        else
        begin
          case PeBorUnmangleName(Items[I].Name, Unmangled, Desc, BasePos) of
            urOk:
              begin
                Info.UnitName := Copy(Unmangled, 1, BasePos - 2);
                if not (Desc.Kind in [skRTTI, skVTable]) then
                begin
                  Info.ProcedureName := Copy(Unmangled, BasePos, Length(Unmangled));
                  if smLinkProc in Desc.Modifiers then
                    Info.ProcedureName := '@' + Info.ProcedureName;
                  Info.OffsetFromProcName := VA - Items[I].Address;
                end;
                Result := True;
              end;
            urNotMangled:
              begin
                Info.ProcedureName := Items[I].Name;
                Info.OffsetFromProcName := VA - Items[I].Address;
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

//--------------------------------------------------------------------------------------------------

function TJclDebugInfoExports.InitializeSource: Boolean;
begin
  FBorImage := TJclPeBorImage.Create(True);
  FBorImage.AttachLoadedModule(FModule);
  Result := FBorImage.StatusOK and (FBorImage.ExportList.Count > 0);
end;

//==================================================================================================
// TJclDebugInfoTD32
//==================================================================================================

destructor TJclDebugInfoTD32.Destroy;
begin
  FreeAndNil(FImage);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TJclDebugInfoTD32.GetLocationInfo(const Addr: Pointer; var Info: TJclLocationInfo): Boolean;
var
  VA: DWORD;
begin
  VA := VAFromAddr(Addr);
  Info.UnitName := FImage.TD32Scanner.ModuleNameFromAddr(VA);
  Result := (Info.UnitName) <> '';
  if Result then
    with Info do
    begin
      Address := Addr;
      ProcedureName := FImage.TD32Scanner.ProcNameFromAddr(VA, OffsetFromProcName);
      LineNumber := FImage.TD32Scanner.LineNumberFromAddr(VA, OffsetFromLineNumber);
      SourceName := FImage.TD32Scanner.SourceNameFromAddr(VA);
      DebugInfo := Self;
    end;
end;

//--------------------------------------------------------------------------------------------------

function TJclDebugInfoTD32.InitializeSource: Boolean;
begin
  FImage := TJclPeBorTD32Image.Create(True);
  try
    FImage.AttachLoadedModule(Module);
    Result := FImage.IsTD32DebugPresent;
  except
    Result := False;
  end;
end;

//==================================================================================================
// Source location functions
//==================================================================================================

{$STACKFRAMES ON}

function Caller(Level: Integer): Pointer;
begin
  try
    with TJclStackInfoList.Create(False, 1, nil) do
    try
      if Level < Count then
        Result := Items[Level].CallerAdr
      else
        Result := nil;
    finally
      Free;
    end;
  except
    Result := nil;
  end;
end;

{$IFNDEF STACKFRAMES_ON}
{$STACKFRAMES OFF}
{$ENDIF STACKFRAMES_ON}

//--------------------------------------------------------------------------------------------------

function GetLocationInfo(const Addr: Pointer): TJclLocationInfo;
begin
  try
    DebugInfoCritSect.Enter;
    try
      NeedDebugInfoList;
      DebugInfoList.GetLocationInfo(Addr, Result)
    finally
      DebugInfoCritSect.Leave;
    end;
  except
    FillChar(Result, SizeOf(Result), #0);
  end;
end;

//--------------------------------------------------------------------------------------------------

function GetLocationInfo(const Addr: Pointer; var Info: TJclLocationInfo): Boolean;
begin
  try
    DebugInfoCritSect.Enter;
    try
      NeedDebugInfoList;
      Result := DebugInfoList.GetLocationInfo(Addr, Info);
    finally
      DebugInfoCritSect.Leave;
    end;
  except
    Result := False;
  end;
end;

//--------------------------------------------------------------------------------------------------

function GetLocationInfoStr(const Addr: Pointer; IncludeModuleName, IncludeAddressOffset,
  IncludeStartProcLineOffset: Boolean): string;
var
  Info, StartProcInfo: TJclLocationInfo;
  OffsetStr, StartProcOffsetStr: string;
begin
  if GetLocationInfo(Addr, Info) then
  with Info do
  begin
    OffsetStr := '';
    if LineNumber > 0 then
    begin
      if IncludeStartProcLineOffset and GetLocationInfo(Pointer(Cardinal(Info.Address) -
        Cardinal(Info.OffsetFromProcName)), StartProcInfo) and (StartProcInfo.LineNumber > 0) then
          StartProcOffsetStr := Format(' + %d', [LineNumber - StartProcInfo.LineNumber])
      else
        StartProcOffsetStr := '';
      if IncludeAddressOffset then
      begin
        if OffsetFromLineNumber >= 0 then
          OffsetStr := Format(' + $%x', [OffsetFromLineNumber])
        else
          OffsetStr := Format(' - $%x', [-OffsetFromLineNumber])
      end;
      Result := Format('[%p] %s.%s (Line %u, "%s"%s)%s', [Addr, UnitName, ProcedureName, LineNumber,
        SourceName, StartProcOffsetStr, OffsetStr]);
    end
    else
    begin
      if IncludeAddressOffset then
        OffsetStr := Format(' + $%x', [OffsetFromProcName]);
      if UnitName <> '' then
        Result := Format('[%p] %s.%s%s', [Addr, UnitName, ProcedureName, OffsetStr])
      else
        Result := Format('[%p] %s%s', [Addr, ProcedureName, OffsetStr]);
    end;
  end
  else
    Result := Format('[%p]', [Addr]);
  if IncludeModuleName then
    Insert(Format('{%-12s}', [ExtractFileName(GetModulePath(ModuleFromAddr(Addr)))]), Result, 11);
end;

//--------------------------------------------------------------------------------------------------

function DebugInfoAvailable(const Module: HMODULE): Boolean;
begin
  DebugInfoCritSect.Enter;
  try
    NeedDebugInfoList;
    Result := (DebugInfoList.ItemFromModule[Module] <> nil);
  finally
    DebugInfoCritSect.Leave;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure ClearLocationData;
begin
  DebugInfoCritSect.Enter;
  try
    if DebugInfoList <> nil then
      DebugInfoList.Clear;
  finally
    DebugInfoCritSect.Leave;
  end;
end;

//--------------------------------------------------------------------------------------------------

{$STACKFRAMES ON}

function FileByLevel(const Level: Integer): string;
begin
  Result := GetLocationInfo(Caller(Level + 1)).SourceName;
end;

//--------------------------------------------------------------------------------------------------

function ModuleByLevel(const Level: Integer): string;
begin
  Result := GetLocationInfo(Caller(Level + 1)).UnitName;
end;

//--------------------------------------------------------------------------------------------------

function ProcByLevel(const Level: Integer): string;
begin
  Result := GetLocationInfo(Caller(Level + 1)).ProcedureName;
end;

//--------------------------------------------------------------------------------------------------

function LineByLevel(const Level: Integer): Integer;
begin
  Result := GetLocationInfo(Caller(Level + 1)).LineNumber;
end;

//--------------------------------------------------------------------------------------------------

function MapByLevel(const Level: Integer; var File_, Module_, Proc_: string;
  var Line_: Integer): Boolean;
begin
  Result := MapOfAddr(Caller(Level + 1), File_, Module_, Proc_, Line_);
end;

//--------------------------------------------------------------------------------------------------

function ExtractClassName(const ProcedureName: string): string;
var
  D: Integer;
begin
  D := Pos('.', ProcedureName);
  if D < 2 then
    Result := ''
  else
    Result := Copy(ProcedureName, 1, D - 1);
end;

//--------------------------------------------------------------------------------------------------

function ExtractMethodName(const ProcedureName: string): string;
begin
  Result := Copy(ProcedureName, Pos('.', ProcedureName) + 1, Length(ProcedureName));
end;

//--------------------------------------------------------------------------------------------------

function __FILE__(const Level: Integer): string;
begin
  Result := FileByLevel(Level + 1);
end;

//--------------------------------------------------------------------------------------------------

function __MODULE__(const Level: Integer): string;
begin
  Result := ModuleByLevel(Level + 1);
end;

//--------------------------------------------------------------------------------------------------

function __PROC__(const Level: Integer): string;
begin
  Result := ProcByLevel(Level + 1);
end;

//--------------------------------------------------------------------------------------------------

function __LINE__(const Level: Integer): Integer;
begin
  Result := LineByLevel(Level + 1);
end;

//--------------------------------------------------------------------------------------------------

function __MAP__(const Level: Integer; var _File, _Module, _Proc: string; var _Line: Integer): Boolean;
begin
  Result := MapByLevel(Level + 1, _File, _Module, _Proc, _Line);
end;

{$IFNDEF STACKFRAMES_ON}
{$STACKFRAMES OFF}
{$ENDIF STACKFRAMES_ON}

//--------------------------------------------------------------------------------------------------

function FileOfAddr(const Addr: Pointer): string;
begin
  Result := GetLocationInfo(Addr).SourceName;
end;

//--------------------------------------------------------------------------------------------------

function ModuleOfAddr(const Addr: Pointer): string;
begin
  Result := GetLocationInfo(Addr).UnitName;
end;

//--------------------------------------------------------------------------------------------------

function ProcOfAddr(const Addr: Pointer): string;
begin
  Result := GetLocationInfo(Addr).ProcedureName;
end;

//--------------------------------------------------------------------------------------------------

function LineOfAddr(const Addr: Pointer): Integer;
begin
  Result := GetLocationInfo(Addr).LineNumber;
end;

//--------------------------------------------------------------------------------------------------

function MapOfAddr(const Addr: Pointer; var File_, Module_, Proc_: string;
  var Line_: Integer): Boolean;
var
  LocInfo: TJclLocationInfo;
begin
  NeedDebugInfoList;
  Result := DebugInfoList.GetLocationInfo(Addr, LocInfo);
  if Result then
  begin
    File_ := LocInfo.SourceName;
    Module_ := LocInfo.UnitName;
    Proc_ := LocInfo.ProcedureName;
    Line_ := LocInfo.LineNumber;
  end;
end;

//--------------------------------------------------------------------------------------------------

function __FILE_OF_ADDR__(const Addr: Pointer): string;
begin
  Result := FileOfAddr(Addr);
end;

//--------------------------------------------------------------------------------------------------

function __MODULE_OF_ADDR__(const Addr: Pointer): string;
begin
  Result := ModuleOfAddr(Addr);
end;

//--------------------------------------------------------------------------------------------------

function __PROC_OF_ADDR__(const Addr: Pointer): string;
begin
  Result := ProcOfAddr(Addr);
end;

//--------------------------------------------------------------------------------------------------

function __LINE_OF_ADDR__(const Addr: Pointer): Integer;
begin
  Result := LineOfAddr(Addr);
end;

//--------------------------------------------------------------------------------------------------

function __MAP_OF_ADDR__(const Addr: Pointer; var _File, _Module, _Proc: string;
  var _Line: Integer): Boolean;
begin
  Result := MapOfAddr(Addr, _File, _Module, _Proc, _Line);
end;

//==================================================================================================
// Info routines base list
//==================================================================================================

constructor TJclStackBaseList.Create;
begin
  inherited Create(True);
  FThreadID := GetCurrentThreadId;
  FTimeStamp := Now;
end;

//==================================================================================================
// TJclGlobalStackList
//==================================================================================================

type
  TJclStackBaseListClass = class of TJclStackBaseList;

  TJclGlobalStackList = class (TThreadList)
  private
    FLockedTID: DWORD;
    FTIDLocked: Boolean;
    function GetExceptStackInfo: TJclStackInfoList;
    function GetLastExceptFrameList: TJclExceptFrameList;
  public
    destructor Destroy; override;
    procedure AddObject(AObject: TJclStackBaseList);
    procedure LockThreadID(TID: DWORD);
    procedure UnlockThreadID;
    function FindObject(TID: DWORD; AClass: TJclStackBaseListClass): TJclStackBaseList;
    property ExceptStackInfo: TJclStackInfoList read GetExceptStackInfo;
    property LastExceptFrameList: TJclExceptFrameList read GetLastExceptFrameList;
  end;

var
  GlobalStackList: TJclGlobalStackList;

//--------------------------------------------------------------------------------------------------

procedure TJclGlobalStackList.AddObject(AObject: TJclStackBaseList);
var
  ReplacedObj: TObject;
begin
  with LockList do
  try
    ReplacedObj := FindObject(AObject.ThreadID, TJclStackBaseListClass(AObject.ClassType));
    if ReplacedObj <> nil then
    begin
      Remove(ReplacedObj);
      ReplacedObj.Free;
    end;
    Add(AObject);
  finally
    UnlockList;
  end;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclGlobalStackList.Destroy;
var
  I: Integer;
begin
  with LockList do
  try
    for I := 0 to Count - 1 do
      TObject(Items[I]).Free;
  finally
    UnlockList;
  end;
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TJclGlobalStackList.FindObject(TID: DWORD; AClass: TJclStackBaseListClass): TJclStackBaseList;
var
  I: Integer;
  Item: TJclStackBaseList;
begin
  Result := nil;
  with LockList do
  try
    if FTIDLocked and (GetCurrentThreadId = MainThreadID) then
      TID := FLockedTID;
    for I := 0 to Count - 1 do
    begin
      Item := Items[I];
      if (Item.ThreadID = TID) and (Item is AClass) then
      begin
        Result := Item;
        Break;
      end;
    end;
  finally
    UnlockList;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclGlobalStackList.GetExceptStackInfo: TJclStackInfoList;
begin
  Result := TJclStackInfoList(FindObject(GetCurrentThreadId, TJclStackInfoList));
end;

//--------------------------------------------------------------------------------------------------

function TJclGlobalStackList.GetLastExceptFrameList: TJclExceptFrameList;
begin
  Result := TJclExceptFrameList(FindObject(GetCurrentThreadId, TJclExceptFrameList));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclGlobalStackList.LockThreadID(TID: DWORD);
begin
  with LockList do
  try
    if GetCurrentThreadId = MainThreadID then
    begin
      FTIDLocked := True;
      FLockedTID := TID;
    end
    else
      FTIDLocked := False;
  finally
    UnlockList;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclGlobalStackList.UnlockThreadID;
begin
  with LockList do
  try
    FTIDLocked := False;
  finally
    UnlockList;
  end;
end;

//==================================================================================================
// TJclGlobalModulesList
//==================================================================================================

type
  TJclGlobalModulesList = class (TObject)
  private
    FLock: TJclCriticalSection;
    FModulesList: TJclModuleInfoList;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateModulesList: TJclModuleInfoList;
    procedure FreeModulesList(var ModulesList: TJclModuleInfoList);
  end;

var
  GlobalModulesList: TJclGlobalModulesList;

//--------------------------------------------------------------------------------------------------

constructor TJclGlobalModulesList.Create;
begin
  FLock := TJclCriticalSection.Create;
end;

//--------------------------------------------------------------------------------------------------

function TJclGlobalModulesList.CreateModulesList: TJclModuleInfoList;
begin
  FLock.Enter;
  try
    if FModulesList = nil then
    begin
      Result := TJclModuleInfoList.Create(False, not (stAllModules in JclStackTrackingOptions));
      if stStaticModuleList in JclStackTrackingOptions then
        FModulesList := Result;
    end
    else
      Result := FModulesList;
  finally
    FLock.Leave;
  end;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclGlobalModulesList.Destroy;
begin
  FreeAndNil(FLock);
  FreeAndNil(FModulesList);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclGlobalModulesList.FreeModulesList(var ModulesList: TJclModuleInfoList);
begin
  FLock.Enter;
  try
    if FModulesList <> ModulesList then
      FreeAndNil(ModulesList);
  finally
    FLock.Leave
  end;
end;

//==================================================================================================
// Stack info routines
//==================================================================================================

{$STACKFRAMES OFF}

function ValidCodeAddr(CodeAddr: DWORD; ModuleList: TJclModuleInfoList): Boolean;
begin
  if stAllModules in JclStackTrackingOptions then
    Result := ModuleList.IsValidModuleAddress(Pointer(CodeAddr))
  else
    Result := ModuleList.IsSystemModuleAddress(Pointer(CodeAddr));
{  if stAllModules in JclStackTrackingOptions then
    Result := (ModuleFromAddr(Pointer(CodeAddr)) <> 0)
  else
    Result := IsSystemModule(ModuleFromAddr(Pointer(CodeAddr)));}
end;

//--------------------------------------------------------------------------------------------------

procedure CorrectExceptStackListTop(List: TJclStackInfoList; SkipFirstItem: Boolean);
var
  TopItem,I, FoundPos: Integer;
begin
  FoundPos := -1;
  if SkipFirstItem then
    TopItem := 1
  else
    TopItem := 0;
  with List do
  begin
    for I := Count - 1 downto TopItem do
      if JclBelongsHookedCode(Items[I].CallerAdr) then
      begin
        FoundPos := I;
        Break;
      end;
    if FoundPos <> -1 then
      for I := FoundPos downto TopItem do
        Delete(I);
  end;
end;

//--------------------------------------------------------------------------------------------------

{$STACKFRAMES ON}

procedure DoExceptionStackTrace(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean);
var
  IgnoreLevels: Integer;
  FirstCaller: Pointer;
  RawMode: Boolean;
begin
  RawMode := stRawMode in JclStackTrackingOptions;
  if RawMode then
    IgnoreLevels := 10
  else
    IgnoreLevels := 5;
  if OSException then
    FirstCaller := ExceptAddr
  else
    FirstCaller := nil;
  CorrectExceptStackListTop(JclCreateStackList(RawMode, IgnoreLevels, FirstCaller), OSException);
end;

//--------------------------------------------------------------------------------------------------

function JclLastExceptStackList: TJclStackInfoList;
begin
  Result := GlobalStackList.ExceptStackInfo;
end;

//--------------------------------------------------------------------------------------------------

function JclLastExceptStackListToStrings(Strings: TStrings; IncludeModuleName, IncludeAddressOffset,
  IncludeStartProcLineOffset: Boolean): Boolean;
var
  List: TJclStackInfoList;
begin
  List := JclLastExceptStackList;
  Result := Assigned(List);
  if Result then
    List.AddToStrings(Strings, IncludeModuleName, IncludeAddressOffset, IncludeStartProcLineOffset);
end;

//--------------------------------------------------------------------------------------------------

function JclCreateStackList(Raw: Boolean; AIgnoreLevels: Integer; FirstCaller: Pointer): TJclStackInfoList;
begin
  Result := TJclStackInfoList.Create(Raw, AIgnoreLevels, FirstCaller);
  GlobalStackList.AddObject(Result);
end;

//==================================================================================================
// TJclStackInfoItem
//==================================================================================================

function TJclStackInfoItem.GetCallerAdr: Pointer;
begin
  Result := Pointer(FStackInfo.CallerAdr);
end;

//--------------------------------------------------------------------------------------------------

function TJclStackInfoItem.GetLogicalAddress: DWORD;
begin
  Result := FStackInfo.CallerAdr - DWORD(ModuleFromAddr(CallerAdr));
end;

//==================================================================================================
// TJclStackInfoList
//==================================================================================================

procedure TJclStackInfoList.AddToStrings(Strings: TStrings; IncludeModuleName, IncludeAddressOffset,
  IncludeStartProcLineOffset: Boolean);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Strings.Add(GetLocationInfoStr(Items[I].CallerAdr, IncludeModuleName, IncludeAddressOffset,
      IncludeStartProcLineOffset));
end;

//--------------------------------------------------------------------------------------------------

constructor TJclStackInfoList.Create(Raw: Boolean; AIgnoreLevels: DWORD; FirstCaller: Pointer);
var
  Item: TJclStackInfoItem;
begin
  inherited Create;
  FIgnoreLevels := AIgnoreLevels;
  TopOfStack := GetStackTop;
  FModuleInfoList := GlobalModulesList.CreateModulesList;
  if FirstCaller <> nil then
  begin
    Item := TJclStackInfoItem.Create;
    Item.FStackInfo.CallerAdr := DWORD(FirstCaller);
    Add(Item);
  end;
  if Raw then
    TraceStackRaw
  else
    TraceStackFrames;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclStackInfoList.Destroy;
begin
  GlobalModulesList.FreeModulesList(FModuleInfoList);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TJclStackInfoList.GetItems(Index: Integer): TJclStackInfoItem;
begin
  Result := TJclStackInfoItem(Get(Index));
end;

//--------------------------------------------------------------------------------------------------

{$OVERFLOWCHECKS OFF}

function TJclStackInfoList.NextStackFrame(var StackFrame: PStackFrame; var StackInfo: TStackInfo): Boolean;
begin
  // Only report this stack frame into the StockInfo structure
  // if the StackFrame pointer, EBP on the stack and return
  // address on the stack are valid addresses
  while ValidStackAddr(DWORD(StackFrame)) do
  begin
    // CallerAdr within current process space, code segment etc.
    // CallersEBP within current thread stack. Added Mar 12 2002 per Hallvard's suggestion
    if ValidCodeAddr(StackFrame^.CallerAdr, FModuleInfoList) and ValidStackAddr(StackFrame^.CallersEBP) then
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

{$IFDEF OVERFLOWCHECKS_ON}
{$OVERFLOWCHECKS ON}
{$ENDIF OVERFLOWCHECKS_ON}

//--------------------------------------------------------------------------------------------------

procedure TJclStackInfoList.StoreToList(const StackInfo: TStackInfo);
var
  Item: TJclStackInfoItem;
begin
  if StackInfo.Level > IgnoreLevels + 1 then
  begin
    Item := TJclStackInfoItem.Create;
    Item.FStackInfo := StackInfo;
    Add(Item);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStackInfoList.TraceStackFrames;
var
  StackFrame: PStackFrame;
  StackInfo: TStackInfo;
begin
  // Start at level 0
  StackInfo.Level := 0;
  // Get the current stack frame from the EBP register
  StackFrame := GetEBP;
  // We define the bottom of the valid stack to be the current EBP Pointer
  // There is a TIB field called pvStackUserBase, but this includes more of the
  // stack than what would define valid stack frames.
  BaseOfStack := DWORD(StackFrame) - 1;
  // Loop over and report all valid stackframes
  while NextStackFrame(StackFrame, StackInfo) do
    StoreToList(StackInfo);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStackInfoList.TraceStackRaw;
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
      // remember to callers address so that we don't report it repeatedly
      PrevCaller := StackPtr^;
      // increase the stack level
      Inc(StackInfo.Level);
      // then report it back to our caller
      StoreToList(StackInfo);
    end;
    // Look at the next DWORD on the stack
    Inc(StackPtr);
  end;
end;

//--------------------------------------------------------------------------------------------------

// Validate that the code address is a valid code site
//
// Information from Intel Manual 24319102(2).pdf, Download the 6.5 MBs from:
// http://developer.intel.com/design/pentiumii/manuals/243191.htm
// Instruction format, Chapter 2 and The CALL instruction: page 3-53, 3-54

{$OVERFLOWCHECKS OFF}

function TJclStackInfoList.ValidCallSite(CodeAddr: DWORD): Boolean;
var
  CodeDWORD4: DWORD;
  CodeDWORD8: DWORD;
  C4P, C8P: PDWORD;
begin
  // First check that the address is within range of our code segment!
  C8P := PDWORD(CodeAddr - 8);
  C4P := PDWORD(CodeAddr - 4);
  Result := (CodeAddr > 8) and ValidCodeAddr(DWORD(C8P), FModuleInfoList) and
    not IsBadReadPtr(C8P, 8) and not IsBadCodePtr(C8P) and not IsBadCodePtr(C4P);

  // Now check to see if the instruction preceding the return address
  // could be a valid CALL instruction
  if Result then
  begin
    // Check the instruction prior to the potential call site.
    // We consider it a valid call site if we find a CALL instruction there
    // Check the most common CALL variants first
    // ! CodeDWORD8 := PDWORD(CodeAddr-8)^;
    // ! CodeDWORD4 := PDWORD(CodeAddr-4)^;
    CodeDWORD8 := C8P^;
    CodeDWORD4 := C4P^;

    Result :=
      ((CodeDWORD8 and $FF000000) = $E8000000) or // 5-byte, CALL [-$1234567]
      ((CodeDWORD4 and $38FF0000) = $10FF0000) or // 2 byte, CALL EAX
      ((CodeDWORD4 and $0038FF00) = $0010FF00) or // 3 byte, CALL [EBP+0x8]
      ((CodeDWORD4 and $000038FF) = $000010FF) or // 4 byte, CALL ??
      ((CodeDWORD8 and $38FF0000) = $10FF0000) or // 6-byte, CALL ??
      ((CodeDWORD8 and $0038FF00) = $0010FF00) or // 7-byte, CALL [ESP-0x1234567]
    // It is possible to simulate a CALL by doing a PUSH followed by RET,
    // so we check for a RET just prior to the return address
      ((CodeDWORD4 and $FF000000) = $C3000000);   // PUSH XX, RET
    // Because we're not doing a complete disassembly, we will potentially report
    // false positives. If there is odd code that uses the CALL 16:32 format, we
    // can also get false negatives.
  end;
end;

{$IFDEF OVERFLOWCHECKS_ON}
{$OVERFLOWCHECKS ON}
{$ENDIF OVERFLOWCHECKS_ON}

{$IFNDEF STACKFRAMES_ON}
{$STACKFRAMES OFF}
{$ENDIF STACKFRAMES_ON}

//--------------------------------------------------------------------------------------------------

function TJclStackInfoList.ValidStackAddr(StackAddr: DWORD): Boolean;
begin
  Result := (BaseOfStack < StackAddr) and (StackAddr < TopOfStack);
end;

//==================================================================================================
// Exception frame info routines
//==================================================================================================

function JclCreateExceptFrameList(AIgnoreLevels: Integer): TJclExceptFrameList;
begin
  Result := TJclExceptFrameList.Create(AIgnoreLevels);
  GlobalStackList.AddObject(Result);
end;

//--------------------------------------------------------------------------------------------------

function JclLastExceptFrameList: TJclExceptFrameList;
begin
  Result := GlobalStackList.LastExceptFrameList;
end;

//--------------------------------------------------------------------------------------------------

procedure DoExceptFrameTrace;
begin
  // Ignore first 2 levels; the First level is an undefined frame (I haven't a
  // clue as to where it comes from. The second level is the try..finally block
  // in DoExceptNotify.
  JclCreateExceptFrameList(4);
end;

//--------------------------------------------------------------------------------------------------

function GetJmpDest(Jmp: PJmpInstruction): DWORD;
begin
  if Jmp.opCode = $E9 then
    Result := Longint(Jmp) + Jmp.distance + 5
  else
  if Jmp.opCode = $EB then
    Result := Longint(Jmp) + ShortInt(jmp.distance) + 2
  else
    Result := 0;
  if (Result <> 0) and (PJmpTable(Result).OPCode = $25FF) then
    if not IsBadReadPtr(PJmpTable(Result).Ptr, 4) then
      Result := PDWORD(PJmpTable(Result).Ptr)^;
end;

//==================================================================================================
// TJclExceptFrame
//==================================================================================================

procedure TJclExceptFrame.DoDetermineFrameKind;
var
  Dest: Longint;
  LocInfo: TJclLocationInfo;
begin
  FFrameKind := efkUnknown;
  if FExcFrame <> nil then
  begin
    Dest := GetJmpDest(@ExcFrame.desc.Jmp);
    if Dest <> 0 then
    begin
      LocInfo := GetLocationInfo(Pointer(Dest));
      if CompareText(LocInfo.UnitName, 'system') = 0 then
      begin
        if CompareText(LocInfo.ProcedureName, '@HandleAnyException') = 0 then
          FFrameKind := efkAnyException
        else
        if CompareText(LocInfo.ProcedureName, '@HandleOnException') = 0 then
          FFrameKind := efkOnException
        else
        if CompareText(LocInfo.ProcedureName, '@HandleAutoException') = 0 then
          FFrameKind := efkAutoException
        else
        if CompareText(LocInfo.ProcedureName, '@HandleFinally') = 0 then
          FFrameKind := efkFinally;
      end;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

constructor TJclExceptFrame.Create(AExcFrame: PExcFrame);
begin
  inherited Create;
  FExcFrame := AExcFrame;
  DoDetermineFrameKind;
end;

//--------------------------------------------------------------------------------------------------

function TJclExceptFrame.Handles(ExceptObj: TObject): Boolean;
var
  Handler: Pointer;
begin
  Result := HandlerInfo(ExceptObj, Handler);
end;

//--------------------------------------------------------------------------------------------------

function TJclExceptFrame.HandlerInfo(ExceptObj: TObject; var HandlerAt: Pointer): Boolean;
var
  I: Integer;
  VTable: Pointer;
begin
  Result := FrameKind in [efkAnyException, efkAutoException];
  if not Result and (FrameKind = efkOnException) then
  begin
    I := 0;
    VTable := Pointer(Integer(ExceptObj.ClassType) + vmtSelfPtr);
    while (I < ExcFrame.Desc.Cnt) and not Result and (VTable <> nil) do
    begin
      Result := (ExcFrame.Desc.ExcTab[I].VTable = nil) or
        (ExcFrame.Desc.ExcTab[I].VTable = VTable);
      if not Result then
      begin
        Move(PChar(VTable)[vmtParent - vmtSelfPtr], VTable, 4);
        if VTable = nil then
        begin
          VTable := Pointer(Integer(ExceptObj.ClassType) + vmtSelfPtr);
          Inc(I);
        end;
      end;
    end;
    if Result then
      HandlerAt := ExcFrame.Desc.ExcTab[I].Handler;
  end
  else
  if Result then
  begin
    HandlerAt := Pointer(GetJmpDest(@ExcFrame.Desc.Instructions));
    if HandlerAt = nil then
      HandlerAt := @ExcFrame.Desc.Instructions;
  end
  else
    HandlerAt := nil;
end;

//--------------------------------------------------------------------------------------------------

function TJclExceptFrame.CodeLocation: Pointer;
begin
  if FrameKind <> efkUnknown then
  begin
    Result := Pointer(GetJmpDest(PJmpInstruction(DWORD(@ExcFrame.Desc.Instructions))));
      if Result = nil then
        Result := @ExcFrame.Desc.Instructions;
  end
  else
  begin
    Result := Pointer(GetJmpDest(PJmpInstruction(DWORD(@ExcFrame.Desc))));
      if Result = nil then
        Result := @ExcFrame.Desc;
  end;
end;

//==================================================================================================
// TJclExceptFrameList
//==================================================================================================

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

//--------------------------------------------------------------------------------------------------

function TJclExceptFrameList.GetItems(Index: Integer): TJclExceptFrame;
begin
  Result := TJclExceptFrame(Get(Index));
end;

//--------------------------------------------------------------------------------------------------

constructor TJclExceptFrameList.Create(AIgnoreLevels: Integer);
begin
  inherited Create;
  FIgnoreLevels := AIgnoreLevels;
  TraceExceptionFrames;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclExceptFrameList.TraceExceptionFrames;
var
  FS: PExcFrame;
  Level: Integer;
  ModulesList: TJclModuleInfoList;
begin
  Clear;
  ModulesList := GlobalModulesList.CreateModulesList;
  try
    Level := 0;
    FS := GetFS;
    while Longint(FS) <> -1 do
    begin
      if (Level >= IgnoreLevels) and ValidCodeAddr(DWORD(FS.Desc), ModulesList) then
        AddFrame(FS);
      Inc(Level);
      FS := FS.next;
    end;
  finally
    GlobalModulesList.FreeModulesList(ModulesList);
  end;
end;

//==================================================================================================
// Exception hooking
//==================================================================================================

var
  TrackingActive: Boolean;

//--------------------------------------------------------------------------------------------------

procedure DoExceptNotify(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean);
begin
  if TrackingActive then
  begin
    if stStack in JclStackTrackingOptions then
      DoExceptionStackTrace(ExceptObj, ExceptAddr, OSException);
    if stExceptFrame in JclStackTrackingOptions then
      DoExceptFrameTrace;
  end;    
end;

//--------------------------------------------------------------------------------------------------

function JclStartExceptionTracking: Boolean;
begin
  if TrackingActive then
    Result := False
  else
  begin
    Result := JclHookExceptions and JclAddExceptNotifier(DoExceptNotify, npFirstChain);
    TrackingActive := Result;
  end;
end;

//--------------------------------------------------------------------------------------------------

function JclStopExceptionTracking: Boolean;
begin
  if TrackingActive then
  begin
    Result := JclRemoveExceptNotifier(DoExceptNotify);
    TrackingActive := False;
  end
  else
    Result := False;
end;

//--------------------------------------------------------------------------------------------------

function JclExceptionTrackingActive: Boolean;
begin
  Result := TrackingActive;
end;

//==================================================================================================
// Thread exception tracking support
//==================================================================================================

var
  RegisteredThreadList: TJclDebugThreadList;

//--------------------------------------------------------------------------------------------------

function JclDebugThreadList: TJclDebugThreadList;
begin
  if RegisteredThreadList = nil then
    RegisteredThreadList := TJclDebugThreadList.Create;
  Result := RegisteredThreadList;
end;

//==================================================================================================
// TJclDebugThread
//==================================================================================================

constructor TJclDebugThread.Create(Suspended: Boolean; const AThreadName: string);
begin
  FThreadName := AThreadName;
  inherited Create(True);
  JclDebugThreadList.RegisterThread(Self, AThreadName);
  if not Suspended then
    Resume;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclDebugThread.Destroy;
begin
  JclDebugThreadList.UnregisterThread(Self);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDebugThread.DoHandleException;
begin
  GlobalStackList.LockThreadID(ThreadID);
  try
    DoSyncHandleException;
  finally
    GlobalStackList.UnlockThreadID;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDebugThread.DoNotify;
begin
  JclDebugThreadList.DoSyncException(Self);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDebugThread.DoSyncHandleException;
begin
  // Note: JclLastExceptStackList and JclLastExceptFrameList returns information
  // for this Thread ID instead of MainThread ID here to allow use a common
  // exception handling routine easily.
  // Any other call of those JclLastXXX routines from another thread at the same
  // time will return expected information for current Thread ID.
  DoNotify;
end;

//--------------------------------------------------------------------------------------------------

function TJclDebugThread.GetThreadInfo: string;
begin
  Result := JclDebugThreadList.ThreadInfos[ThreadID];
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDebugThread.HandleException;
begin
  FSyncException := Exception(ExceptObject);
  try
    if not (FSyncException is EAbort) then
      Synchronize(DoHandleException);
  finally
    FSyncException := nil;
  end;
end;

//==================================================================================================
// TJclDebugThreadList
//==================================================================================================

type
  TThreadAccess = class (TThread);

//--------------------------------------------------------------------------------------------------

constructor TJclDebugThreadList.Create;
begin
  FLock := TJclCriticalSection.Create;
  FReadLock := TJclCriticalSection.Create;
  FList := TStringList.Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclDebugThreadList.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FLock);
  FreeAndNil(FReadLock);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDebugThreadList.DoSyncException(Thread: TJclDebugThread);
begin
  if Assigned(FOnSyncException) then
    FOnSyncException(Thread);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDebugThreadList.DoSyncThreadRegistered;
begin
  if Assigned(FOnThreadUnregistered) then
    FOnThreadRegistered(FRegSyncThreadID);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDebugThreadList.DoSyncThreadUnregistered;
begin
  if Assigned(FOnThreadUnregistered) then
    FOnThreadUnregistered(FUnregSyncThreadID);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDebugThreadList.DoThreadRegistered(Thread: TThread);
begin
  if Assigned(FOnThreadRegistered) then
  begin
    FRegSyncThreadID := Thread.ThreadID;
    TThreadAccess(Thread).Synchronize(DoSyncThreadRegistered);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDebugThreadList.DoThreadUnregistered(Thread: TThread);
begin
  if Assigned(FOnThreadUnregistered) then
  begin
    FUnregSyncThreadID := Thread.ThreadID;
    TThreadAccess(Thread).Synchronize(DoSyncThreadUnregistered);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclDebugThreadList.GetThreadIDCount: Integer;
begin
  FReadLock.Enter;
  try
    Result := FList.Count;
  finally
    FReadLock.Leave;
  end;    
end;

//--------------------------------------------------------------------------------------------------

function TJclDebugThreadList.GetThreadIDs(Index: Integer): DWORD;
begin
  Result := DWORD(FList.Objects[Index]);
end;

//--------------------------------------------------------------------------------------------------

function TJclDebugThreadList.GetThreadNames(ThreadID: DWORD; Index: Integer): string;
var
  I: Integer;

  function ThreadName: string;
  begin
    Result := FList[I];
    Delete(Result, 1, Pos('=', Result));
  end;

begin
  FReadLock.Enter;
  try
    I := FList.IndexOfObject(Pointer(ThreadID));
    if I <> -1 then
    begin
      case Index of
        0:
          Result := ThreadName;
        1:
          Result := FList.Names[I];
        2:
          Result := Format('%.8x [%s] "%s"', [ThreadID, FList.Names[I], ThreadName]);
      end;
    end
    else
      Result := '';
  finally
    FReadLock.Leave;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDebugThreadList.InternalRegisterThread(Thread: TThread; const ThreadName: string);
var
  I: Integer;

  function FormatInternalName: string;
  begin
    Result := Format('%s=%s', [Thread.ClassName, ThreadName]);
  end;

begin
  FLock.Enter;
  try
    I := FList.IndexOfObject(Pointer(Thread.ThreadID));
    if I = -1 then
    begin
      FReadLock.Enter;
      try
        FList.AddObject(FormatInternalName, Pointer(Thread.ThreadID));
      finally
        FReadLock.Leave;
      end;
      DoThreadRegistered(Thread);
    end;
  finally
    FLock.Leave;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDebugThreadList.InternalUnregisterThread(Thread: TThread);
var
  I: Integer;
begin
  FLock.Enter;
  try
    I := FList.IndexOfObject(Pointer(Thread.ThreadID));
    if I <> -1 then
    begin
      DoThreadUnregistered(Thread);
      FReadLock.Enter;
      try
        FList.Delete(I);
      finally
        FReadLock.Leave;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDebugThreadList.RegisterThread(Thread: TThread; const ThreadName: string);
begin
  InternalRegisterThread(Thread, ThreadName);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDebugThreadList.UnregisterThread(Thread: TThread);
begin
  InternalUnregisterThread(Thread);
end;

//==================================================================================================
// Miscellanuous
//==================================================================================================

{$IFDEF MSWINDOWS}

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

//--------------------------------------------------------------------------------------------------

function IsDebuggerAttached: Boolean;
var
  IsDebuggerPresent: function: Boolean; stdcall;
  KernelHandle: THandle;
  P: Pointer;
begin
  KernelHandle := GetModuleHandle(kernel32);
  @IsDebuggerPresent := GetProcAddress(KernelHandle, 'IsDebuggerPresent');
  if @IsDebuggerPresent <> nil then
  begin
    // Win98+ / NT4+
    Result := IsDebuggerPresent
  end
  else
  begin
    // Win9x uses thunk pointer outside the module when under a debugger
    P := GetProcAddress(KernelHandle, 'GetProcAddress');
    Result := DWORD(P) < KernelHandle;
  end;
end;

//--------------------------------------------------------------------------------------------------

function IsHandleValid(Handle: THandle): Boolean;
var
  Duplicate: THandle;
  Flags: DWORD;
begin
  if IsWinNT then
    Result := GetHandleInformation(Handle, Flags)
  else
    Result := False;  
  if not Result then
  begin
    // DuplicateHandle is used as an additional check for those object types not
    // supported by GetHandleInformation (e.g. according to the documentation,
    // GetHandleInformation doesn't support window stations and desktop although
    // tests show that it does). GetHandleInformation is tried first because its
    // much faster. Additionally GetHandleInformation is only supported on NT...
    Result := DuplicateHandle(GetCurrentProcess, Handle, GetCurrentProcess,
      @Duplicate, 0, False, DUPLICATE_SAME_ACCESS);
    if Result then
      Result := CloseHandle(Duplicate);
  end;
end;

{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------

initialization
  DebugInfoCritSect := TJclCriticalSection.Create;
  GlobalModulesList := TJclGlobalModulesList.Create;
  GlobalStackList := TJclGlobalStackList.Create;

finalization
  { TODO -oPV -cInvestigate : Calling JclStopExceptionTracking causes linking of various classes to
    the code without a real need. Although there doesn't seem to be a way to unhook exceptions
    safely because we need to be covered by JclHookExcept.Notifiers critical section }
  JclStopExceptionTracking;
  FreeAndNil(RegisteredThreadList);
  FreeAndNil(DebugInfoList);
  FreeAndNil(GlobalStackList);
  FreeAndNil(GlobalModulesList);
  FreeAndNil(DebugInfoCritSect);

end.
