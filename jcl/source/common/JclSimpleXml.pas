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
{ The Original Code is JvSimpleXML.PAS, released on 2002-06-03.                                    }
{                                                                                                  }
{ The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com].    }
{ Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Christophe Paris,                                                                              }
{   Florent Ouchet (move from the JVCL to the JCL)                                                 }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains Xml parser and writter classes                                                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

// Known Issues: This component does not parse the !DOCTYPE tags but preserves them

unit JclSimpleXml;

interface

{$I jcl.inc}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows, // Delphi 2005 inline
  {$ENDIF MSWINDOWS}
  SysUtils, Classes,
  Variants,
  IniFiles,
  JclBase, JclStreams;

type
  TJclSimpleXML = class;
  EJclSimpleXMLError = class(EJclError);
  {$TYPEINFO ON} // generate RTTI for published properties
  TJclSimpleXMLElem = class;
  {$IFNDEF TYPEINFO_ON}
  {$TYPEINFO OFF}
  {$ENDIF ~TYPEINFO_ON}
  TJclSimpleXMLElems = class;
  TJclSimpleXMLProps = class;
  TJclSimpleXMLElemComment = class;
  TJclSimpleXMLElemClassic = class;
  TJclSimpleXMLElemCData = class;
  TJclSimpleXMLElemDocType = class;
  TJclSimpleXMLElemText = class;
  TJclSimpleXMLElemHeader = class;
  TJclSimpleXMLElemSheet = class;
  TJclSimpleXMLElemMSOApplication = class;
  TJclOnSimpleXMLParsed = procedure(Sender: TObject; const Name: string) of object;
  TJclOnValueParsed = procedure(Sender: TObject; const Name, Value: string) of object;
  TJclOnSimpleProgress = procedure(Sender: TObject; const Position, Total: Integer) of object;

  //Those hash stuffs are for future use only
  //Plans are to replace current hash by this mechanism
  TJclHashKind = (hkList, hkDirect);
  PJclHashElem = ^TJclHashElem;
  TJclHashElem = packed record
    Next: PJclHashElem;
    Obj: TObject;
  end;
  PJclHashRecord = ^TJclHashRecord;
  TJclHashList = array [0..25] of PJclHashRecord;
  PJclHashList = ^TJclHashList;
  TJclHashRecord = packed record
    Count: Byte;
    case Kind: TJclHashKind of
      hkList: (List: PJclHashList);
      hkDirect: (FirstElem: PJclHashElem);
  end;

  TJclSimpleXMLProp = class(TObject)
  private
    FName: string;
    FValue: string;
    FParent: TJclSimpleXMLProps;
    FNameSpace: string;
    FData: Pointer;
    function GetBoolValue: Boolean;
    procedure SetBoolValue(const Value: Boolean);
    procedure SetName(const Value: string);
    function GetFloatValue: Extended;
    procedure SetFloatValue(const Value: Extended);
    function GetAnsiValue: AnsiString;
    procedure SetAnsiValue(const Value: AnsiString);
  protected
    function GetIntValue: Int64;
    procedure SetIntValue(const Value: Int64);
  public
    function GetSimpleXML: TJclSimpleXML;
    procedure SaveToStringStream(StringStream: TJclStringStream);
    function FullName:string;
    property Parent: TJclSimpleXMLProps read FParent write FParent;
    property Name: string read FName write SetName;
    property Value: string read FValue write FValue;
    property AnsiValue: AnsiString read GetAnsiValue write SetAnsiValue;
    property IntValue: Int64 read GetIntValue write SetIntValue;
    property BoolValue: Boolean read GetBoolValue write SetBoolValue;
    property FloatValue: Extended read GetFloatValue write SetFloatValue;
    property NameSpace: string read FNameSpace write FNameSpace;

    property Data: Pointer read FData write FData;
  end;

  TJclSimpleXMLProps = class(TObject)
  private
    FProperties: THashedStringList;
    FParent: TJclSimpleXMLElem;
    function GetCount: Integer;
    function GetItemNamedDefault(const Name, Default: string): TJclSimpleXMLProp;
    function GetItemNamed(const Name: string): TJclSimpleXMLProp;
  protected
    function GetSimpleXML: TJclSimpleXML;
    function GetItem(const Index: Integer): TJclSimpleXMLProp;
    procedure DoItemRename(Value: TJclSimpleXMLProp; const Name: string);
    procedure Error(const S: string);
    procedure FmtError(const S: string; const Args: array of const);
  public
    constructor Create(Parent: TJclSimpleXMLElem);
    destructor Destroy; override;
    function Add(const Name, Value: string): TJclSimpleXMLProp; overload;
    {$IFDEF SUPPORTS_UNICODE}
    function Add(const Name: string; const Value: AnsiString): TJclSimpleXMLProp; overload;
    {$ENDIF SUPPORTS_UNICODE}
    function Add(const Name: string; const Value: Int64): TJclSimpleXMLProp; overload;
    function Add(const Name: string; const Value: Boolean): TJclSimpleXMLProp; overload;
    function Insert(const Index: Integer; const Name, Value: string): TJclSimpleXMLProp; overload;
    function Insert(const Index: Integer; const Name: string; const Value: Int64): TJclSimpleXMLProp; overload;
    function Insert(const Index: Integer; const Name: string; const Value: Boolean): TJclSimpleXMLProp; overload;
    procedure Clear; virtual;
    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: string); overload;
    function Value(const Name: string; const Default: string = ''): string;
    function IntValue(const Name: string; const Default: Int64 = -1): Int64;
    function BoolValue(const Name: string; Default: Boolean = True): Boolean;
    function FloatValue(const Name: string; const Default: Extended = 0): Extended;
    procedure LoadFromStringStream(StringStream: TJclStringStream);
    procedure SaveToStringStream(StringStream: TJclStringStream);
    property Item[const Index: Integer]: TJclSimpleXMLProp read GetItem; default;
    property ItemNamed[const Name: string]: TJclSimpleXMLProp read GetItemNamed;
    property Count: Integer read GetCount;
  end;

  TJclSimpleXMLElemsProlog = class(TObject)
  private
    FElems: THashedStringList;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TJclSimpleXMLElem;
    function GetEncoding: string;
    function GetStandAlone: Boolean;
    function GetVersion: string;
    procedure SetEncoding(const Value: string);
    procedure SetStandAlone(const Value: Boolean);
    procedure SetVersion(const Value: string);
  protected
    function FindHeader: TJclSimpleXMLElem;
    procedure Error(const S: string);
    procedure FmtError(const S: string; const Args: array of const);
  public
    constructor Create;
    destructor Destroy; override;
    function AddComment(const AValue: string): TJclSimpleXMLElemComment;
    function AddDocType(const AValue: string): TJclSimpleXMLElemDocType;
    procedure Clear;
    function AddStyleSheet(const AType, AHRef: string): TJclSimpleXMLElemSheet;
    function AddMSOApplication(const AProgId : string): TJclSimpleXMLElemMSOApplication;
    procedure LoadFromStringStream(StringStream: TJclStringStream; AParent: TJclSimpleXML = nil);
    procedure SaveToStringStream(StringStream: TJclStringStream; AParent: TJclSimpleXML = nil);
    property Item[const Index: Integer]: TJclSimpleXMLElem read GetItem; default;
    property Count: Integer read GetCount;
    property Encoding: string read GetEncoding write SetEncoding;
    property StandAlone: Boolean read GetStandAlone write SetStandAlone;
    property Version: string read GetVersion write SetVersion;
  end;

  TJclSimpleXMLNamedElems = class(TObject)
  private
    FElems: TJclSimpleXMLElems;
    FName: string;
    function GetCount: Integer;
  protected
    FItems: TList;
    function GetItem(const Index: Integer): TJclSimpleXMLElem;
  public
    constructor Create(const AOwner: TJClSimpleXMLElems; const AName: string);
    destructor Destroy; override;

    function Add: TJclSimpleXmlElemClassic; overload;
    function Add(const Value: string): TJclSimpleXmlElemClassic; overload;
    function Add(const Value: Int64): TJclSimpleXmlElemClassic; overload;
    function Add(const Value: Boolean): TJclSimpleXmlElemClassic; overload;
    function Add(Value: TStream): TJclSimpleXmlElemClassic; overload;
    function AddFirst: TJclSimpleXmlElemClassic;
    function AddComment(const Value: string): TJclSimpleXMLElemComment;
    function AddCData(const Value: string): TJclSimpleXMLElemCData;
    function AddText(const Value: string): TJclSimpleXMLElemText;
    procedure Clear; virtual;
    procedure Delete(const Index: Integer);
    procedure Move(const CurIndex, NewIndex: Integer);
    function IndexOf(const Value: TJclSimpleXMLElem): Integer; overload;
    function IndexOf(const Value: string): Integer; overload;

    property Elems: TJclSimpleXMLElems read FElems;
    property Item[const Index: Integer]: TJclSimpleXMLElem read GetItem; default;
    property Count: Integer read GetCount;
    property Name: string read FName;
  end;

  TJclSimpleXMLElemCompare = function(Elems: TJclSimpleXMLElems; Index1, Index2: Integer): Integer of object;
  TJclSimpleXMLElems = class(TObject)
  private
    FParent: TJclSimpleXMLElem;
    function GetCount: Integer;
    function GetItemNamedDefault(const Name, Default: string): TJclSimpleXMLElem;
    function GetItemNamed(const Name: string): TJclSimpleXMLElem;
    function GetNamedElems(const Name: string): TJclSimpleXMLNamedElems;
  protected
    FElems: THashedStringList;
    FCompare: TJclSimpleXMLElemCompare;
    FNamedElems: THashedStringList;
    function GetItem(const Index: Integer): TJclSimpleXMLElem;
    procedure AddChild(const Value: TJclSimpleXMLElem);
    procedure AddChildFirst(const Value: TJclSimpleXMLElem);
    procedure InsertChild(const Value: TJclSimpleXMLElem; Index: Integer);
    procedure DoItemRename(Value: TJclSimpleXMLElem; const Name: string);
    procedure CreateElems;
  public
    constructor Create(const AOwner: TJclSimpleXMLElem);
    destructor Destroy; override;

    // Use notify to indicate to a list that the given element is removed
    // from the list so that it doesn't delete it as well as the one
    // that insert it in itself. This method is automatically called
    // by AddChild and AddChildFirst if the Container property of the
    // given element is set.
    procedure Notify(Value: TJclSimpleXMLElem; Operation: TOperation);

    function Add(const Name: string): TJclSimpleXMLElemClassic; overload;
    function Add(const Name, Value: string): TJclSimpleXMLElemClassic; overload;
    function Add(const Name: string; const Value: Int64): TJclSimpleXMLElemClassic; overload;
    function Add(const Name: string; const Value: Boolean): TJclSimpleXMLElemClassic; overload;
    function Add(const Name: string; Value: TStream): TJclSimpleXMLElemClassic; overload;
    function Add(Value: TJclSimpleXMLElem): TJclSimpleXMLElem; overload;
    function AddFirst(Value: TJclSimpleXMLElem): TJclSimpleXMLElem; overload;
    function AddFirst(const Name: string): TJclSimpleXMLElemClassic; overload;
    function AddComment(const Name: string; const Value: string): TJclSimpleXMLElemComment;
    function AddCData(const Name: string; const Value: string): TJclSimpleXMLElemCData;
    function AddText(const Name: string; const Value: string): TJclSimpleXMLElemText;
    function Insert(Value: TJclSimpleXMLElem; Index: Integer): TJclSimpleXMLElem; overload;
    function Insert(const Name: string; Index: Integer): TJclSimpleXMLElemClassic; overload;
    procedure Clear; virtual;
    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: string); overload;
    function Remove(Value: TJclSimpleXMLElem): Integer;
    procedure Move(const CurIndex, NewIndex: Integer);
    function IndexOf(const Value: TJclSimpleXMLElem): Integer; overload;
    function IndexOf(const Name: string): Integer; overload;
    function Value(const Name: string; const Default: string = ''): string;
    function IntValue(const Name: string; const Default: Int64 = -1): Int64;
    function FloatValue(const Name: string; const Default: Extended = 0): Extended;
    function BoolValue(const Name: string; Default: Boolean = True): Boolean;
    procedure BinaryValue(const Name: string; Stream: TStream);
    procedure LoadFromStringStream(StringStream: TJclStringStream; AParent: TJclSimpleXML = nil);
    procedure SaveToStringStream(StringStream: TJclStringStream; const Level: string = ''; AParent: TJclSimpleXML = nil);
    procedure Sort;
    procedure CustomSort(AFunction: TJclSimpleXMLElemCompare);
    property Parent: TJclSimpleXMLElem read FParent write FParent;
    property Item[const Index: Integer]: TJclSimpleXMLElem read GetItem; default;
    property ItemNamed[const Name: string]: TJclSimpleXMLElem read GetItemNamed;
    property Count: Integer read GetCount;
    property NamedElems[const Name: string]: TJclSimpleXMLNamedElems read GetNamedElems;
  end;

  {$TYPEINFO ON}
  TJclSimpleXMLElem = class(TObject)
  private
    FName: string;
    FParent: TJclSimpleXMLElem;
    FItems: TJclSimpleXMLElems;
    FProps: TJclSimpleXMLProps;
    FValue: string;
    FNameSpace: string;
    FData: Pointer;
    FSimpleXML: TJclSimpleXML;
    FContainer: TJclSimpleXMLElems;
    function GetFloatValue: Extended;
    procedure SetFloatValue(const Value: Extended);
    function GetAnsiValue: AnsiString;
    procedure SetAnsiValue(const Value: AnsiString);
  protected
    function GetSimpleXML: TJclSimpleXML;
    function GetIntValue: Int64;
    function GetBoolValue: Boolean;
    function GetChildsCount: Integer;
    function GetProps: TJclSimpleXMLProps;
    procedure SetBoolValue(const Value: Boolean);
    procedure SetName(const Value: string);
    procedure SetIntValue(const Value: Int64);
    function GetItems: TJclSimpleXMLElems;
    procedure Error(const S: string);
    procedure FmtError(const S: string; const Args: array of const);
  public
    constructor Create(const AOwner: TJclSimpleXMLElem); virtual;
    destructor Destroy; override;
    procedure Assign(Value: TJclSimpleXMLElem); virtual;
    procedure Clear; virtual;
    procedure LoadFromStringStream(StringStream: TJclStringStream; AParent: TJclSimpleXML = nil); virtual; abstract;
    procedure SaveToStringStream(StringStream: TJclStringStream; const Level: string = ''; AParent: TJclSimpleXML = nil); virtual;
      abstract;
    procedure LoadFromString(const Value: string);
    function SaveToString: string;
    procedure GetBinaryValue(Stream: TStream);
    property Data: Pointer read FData write FData;
    function GetChildIndex(const AChild: TJclSimpleXMLElem): Integer;
    function GetNamedIndex(const AChild: TJclSimpleXMLElem): Integer;

    property SimpleXML: TJclSimpleXML read GetSimpleXML;
    property Container: TJclSimpleXMLElems read FContainer write FContainer;
  published
    function FullName: string;virtual;
    property Name: string read FName write SetName;
    property Parent: TJclSimpleXMLElem read FParent write FParent;
    property NameSpace: string read FNameSpace write FNameSpace;
    property ChildsCount: Integer read GetChildsCount;
    property Items: TJclSimpleXMLElems read GetItems;
    property Properties: TJclSimpleXMLProps read GetProps;
    property IntValue: Int64 read GetIntValue write SetIntValue;
    property BoolValue: Boolean read GetBoolValue write SetBoolValue;
    property FloatValue: Extended read GetFloatValue write SetFloatValue;
    property Value: string read FValue write FValue;
    property AnsiValue: AnsiString read GetAnsiValue write SetAnsiValue;
  end;
  {$IFNDEF TYPEINFO_ON}
  {$TYPEINFO OFF}
  {$ENDIF ~TYPEINFO_ON}
  TJclSimpleXMLElemClass = class of TJclSimpleXMLElem;

  TJclSimpleXMLElemComment = class(TJclSimpleXMLElem)
  public
    procedure LoadFromStringStream(StringStream: TJclStringStream; AParent: TJclSimpleXML = nil); override;
    procedure SaveToStringStream(StringStream: TJclStringStream; const Level: string = ''; AParent: TJclSimpleXML = nil); override;
  end;

  TJclSimpleXMLElemClassic = class(TJclSimpleXMLElem)
  public
    procedure LoadFromStringStream(StringStream: TJclStringStream; AParent: TJclSimpleXML = nil); override;
    procedure SaveToStringStream(StringStream: TJclStringStream; const Level: string = ''; AParent: TJclSimpleXML = nil); override;
  end;

  TJclSimpleXMLElemCData = class(TJclSimpleXMLElem)
  public
    procedure LoadFromStringStream(StringStream: TJclStringStream; AParent: TJclSimpleXML = nil); override;
    procedure SaveToStringStream(StringStream: TJclStringStream; const Level: string = ''; AParent: TJclSimpleXML = nil); override;
  end;

  TJclSimpleXMLElemText = class(TJclSimpleXMLElem)
  public
    procedure LoadFromStringStream(StringStream: TJclStringStream; AParent: TJclSimpleXML = nil); override;
    procedure SaveToStringStream(StringStream: TJclStringStream; const Level: string = ''; AParent: TJclSimpleXML = nil); override;
  end;

  TJclSimpleXMLElemProcessingInstruction = class(TJclSimpleXMLElem)
  public
    procedure LoadFromStringStream(StringStream: TJclStringStream; AParent: TJclSimpleXML = nil); override;
    procedure SaveToStringStream(StringStream: TJclStringStream; const Level: string = ''; AParent: TJclSimpleXML = nil); override;
  end;

  TJclSimpleXMLElemHeader = class(TJclSimpleXMLElemProcessingInstruction)
  private
    function GetEncoding: string;
    function GetStandalone: Boolean;
    function GetVersion: string;
    procedure SetEncoding(const Value: string);
    procedure SetStandalone(const Value: Boolean);
    procedure SetVersion(const Value: string);
  public
    procedure LoadFromStringStream(StringStream: TJclStringStream; AParent: TJclSimpleXML = nil); override;
    procedure SaveToStringStream(StringStream: TJclStringStream; const Level: string = ''; AParent: TJclSimpleXML = nil); override;
    property Version: string read GetVersion write SetVersion;
    property StandAlone: Boolean read GetStandalone write SetStandalone;
    property Encoding: string read GetEncoding write SetEncoding;
  end;

  // for backward compatibility
  TJclSimpleXMLElemSheet = class(TJclSimpleXMLElemProcessingInstruction)
  end;

  // for backward compatibility
  TJclSimpleXMLElemMSOApplication = class(TJclSimpleXMLElemProcessingInstruction)
  end;

  TJclSimpleXMLElemDocType = class(TJclSimpleXMLElem)
  public
    procedure LoadFromStringStream(StringStream: TJclStringStream; AParent: TJclSimpleXML = nil); override;
    procedure SaveToStringStream(StringStream: TJclStringStream; const Level: string = ''; AParent: TJclSimpleXML = nil); override;
  end;

  TJclSimpleXMLOptions = set of (sxoAutoCreate, sxoAutoIndent, sxoAutoEncodeValue,
    sxoAutoEncodeEntity, sxoDoNotSaveProlog, sxoTrimPrecedingTextWhitespace,
    sxoTrimFollowingTextWhitespace);
  TJclSimpleXMLEncodeEvent = procedure(Sender: TObject; var Value: string) of object;
  TJclSimpleXMLEncodeStreamEvent = procedure(Sender: TObject; InStream, OutStream: TStream) of object;

  TJclSimpleXML = class(TObject)
  protected
    FEncoding: TJclStringEncoding;
    FCodePage: Word;
    FFileName: TFileName;
    FOptions: TJclSimpleXMLOptions;
    FRoot: TJclSimpleXMLElemClassic;
    FOnTagParsed: TJclOnSimpleXMLParsed;
    FOnValue: TJclOnValueParsed;
    FOnLoadProg: TJclOnSimpleProgress;
    FOnSaveProg: TJclOnSimpleProgress;
    FProlog: TJclSimpleXMLElemsProlog;
    FSaveCount: Integer;
    FSaveCurrent: Integer;
    FIndentString: string;
    FBaseIndentString: string;
    FOnEncodeValue: TJclSimpleXMLEncodeEvent;
    FOnDecodeValue: TJclSimpleXMLEncodeEvent;
    FOnDecodeStream: TJclSimpleXMLEncodeStreamEvent;
    FOnEncodeStream: TJclSimpleXMLEncodeStreamEvent;
    procedure SetIndentString(const Value: string);
    procedure SetBaseIndentString(const Value: string);
    procedure SetRoot(const Value: TJclSimpleXMLElemClassic);
    procedure SetFileName(const Value: TFileName);
    procedure DoLoadProgress(const APosition, ATotal: Integer);
    procedure DoSaveProgress;
    procedure DoTagParsed(const AName: string);
    procedure DoValueParsed(const AName, AValue: string);
    procedure DoEncodeValue(var Value: string); virtual;
    procedure DoDecodeValue(var Value: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromString(const Value: string);
    procedure LoadFromFile(const FileName: TFileName; Encoding: TJclStringEncoding = seAuto; CodePage: Word = CP_ACP);
    procedure LoadFromStream(Stream: TStream; Encoding: TJclStringEncoding = seAuto; CodePage: Word = CP_ACP);
    procedure LoadFromStringStream(StringStream: TJclStringStream);
    procedure LoadFromResourceName(Instance: THandle; const ResName: string; Encoding: TJclStringEncoding = seAuto; CodePage: Word = CP_ACP);
    procedure SaveToFile(const FileName: TFileName; Encoding: TJclStringEncoding = seAuto; CodePage: Word = CP_ACP);
    procedure SaveToStream(Stream: TStream; Encoding: TJclStringEncoding = seAuto; CodePage: Word = CP_ACP);
    procedure SaveToStringStream(StringStream: TJclStringStream);
    function SaveToString: string;
    property Prolog: TJclSimpleXMLElemsProlog read FProlog write FProlog;
    property Root: TJclSimpleXMLElemClassic read FRoot write SetRoot;
    property XMLData: string read SaveToString write LoadFromString;
    property FileName: TFileName read FFileName write SetFileName;
    property IndentString: string read FIndentString write SetIndentString;
    property BaseIndentString: string read FBaseIndentString write SetBaseIndentString;
    property Options: TJclSimpleXMLOptions read FOptions write FOptions;
    property OnSaveProgress: TJclOnSimpleProgress read FOnSaveProg write FOnSaveProg;
    property OnLoadProgress: TJclOnSimpleProgress read FOnLoadProg write FOnLoadProg;
    property OnTagParsed: TJclOnSimpleXMLParsed read FOnTagParsed write FOnTagParsed;
    property OnValueParsed: TJclOnValueParsed read FOnValue write FOnValue;
    property OnEncodeValue: TJclSimpleXMLEncodeEvent read FOnEncodeValue write FOnEncodeValue;
    property OnDecodeValue: TJclSimpleXMLEncodeEvent read FOnDecodeValue write FOnDecodeValue;
    property OnEncodeStream: TJclSimpleXMLEncodeStreamEvent read FOnEncodeStream write FOnEncodeStream;
    property OnDecodeStream: TJclSimpleXMLEncodeStreamEvent read FOnDecodeStream write FOnDecodeStream;
  end;

  TXMLVariant = class(TInvokeableVariantType)
  public
    procedure Clear(var V: TVarData); override;
    function IsClear(const V: TVarData): Boolean; override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); override;

    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): Boolean; override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; override;
    function SetProperty(const V: TVarData; const Name: string;
      const Value: TVarData): Boolean; override;
  end;

procedure XMLCreateInto(var ADest: Variant; const AXML: TJclSimpleXMLElem);
function XMLCreate(const AXML: TJclSimpleXMLElem): Variant; overload;
function XMLCreate: Variant; overload;
function VarXML: TVarType;

// Encodes a string into an internal format:
// any character <= #127 is preserved
// all other characters are converted to hex notation except
// for some special characters that are converted to XML entities
function SimpleXMLEncode(const S: string): string;
// Decodes a string encoded with SimpleXMLEncode:
// any character <= #127 is preserved
// all other characters and substrings are converted from
// the special XML entities to characters or from hex to characters
// NB! Setting TrimBlanks to true will slow down the process considerably
procedure SimpleXMLDecode(var S: string; TrimBlanks: Boolean);

function XMLEncode(const S: string): string;
function XMLDecode(const S: string): string;

// Encodes special characters (', ", <, > and &) into XML entities (@apos;, &quot;, &lt;, &gt; and &amp;)
function EntityEncode(const S: string): string;
// Decodes XML entities (@apos;, &quot;, &lt;, &gt; and &amp;) into special characters (', ", <, > and &)
function EntityDecode(const S: string): string;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common';
    Extra: '';
    Data: nil
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclCharsets,
  JclStrings,
  JclResources;

const
  cBufferSize = 8192;

var
  GlobalSorts: TList = nil;

  GlobalXMLVariant: TXMLVariant = nil;

  PreparedNibbleCharMapping: Boolean = False;
  NibbleCharMapping: array [Low(Char)..High(Char)] of Byte;

function GSorts: TList;
begin
  if not Assigned(GlobalSorts) then
    GlobalSorts := TList.Create;
  Result := GlobalSorts;
end;

function XMLVariant: TXMLVariant;
begin
  if not Assigned(GlobalXMLVariant) then
    GlobalXMLVariant := TXMLVariant.Create;
  Result := GlobalXMLVariant;
end;

procedure AddEntity(var Res: string; var ResIndex, ResLen: Integer; const Entity: string);
var
  EntityIndex, EntityLen: Integer;
begin
  EntityLen := Length(Entity);
  if (ResIndex + EntityLen) > ResLen then
  begin
    if ResLen <= EntityLen then
      ResLen := ResLen * EntityLen
    else
      ResLen := ResLen * 2;
    SetLength(Res, ResLen);
  end;
  for EntityIndex := 1 to EntityLen do
  begin
    Res[ResIndex] := Entity[EntityIndex];
    Inc(ResIndex);
  end;
end;

function EntityEncode(const S: string): string;
var
  C: Char;
  SIndex, SLen, RIndex, RLen: Integer;
  Tmp: string;
begin
  SLen := Length(S);
  RLen := SLen;
  RIndex := 1;
  SetLength(Tmp, RLen);
  for SIndex := 1 to SLen do
  begin
    C := S[SIndex];
    case C of
      '"':
        AddEntity(Tmp, RIndex, RLen, '&quot;');
      '&':
        AddEntity(Tmp, RIndex, RLen, '&amp;');
      #39:
        AddEntity(Tmp, RIndex, RLen, '&apos;');
      '<':
        AddEntity(Tmp, RIndex, RLen, '&lt;');
      '>':
        AddEntity(Tmp, RIndex, RLen, '&gt;');
    else
      if RIndex > RLen then
      begin
        RLen := RLen * 2;
        SetLength(Tmp, RLen);
      end;
      Tmp[RIndex] := C;
      Inc(RIndex);
    end;
  end;
  if RIndex > 1 then
    SetLength(Tmp, RIndex - 1);

  Result := Tmp;
end;

function EntityDecode(const S: string): string;
var
  I, J, L: Integer;
begin
  Result := S;
  I := 1;
  J := 1;
  L := Length(Result);

  while I <= L do
  begin
    if Result[I] = '&' then
    begin
      if StrSame(Copy(Result, I, 5), '&amp;') then
      begin
        Result[J] := '&';
        Inc(J);
        Inc(I, 4);
      end
      else
      if StrSame(Copy(Result, I, 4), '&lt;') then
      begin
        Result[J] := '<';
        Inc(J);
        Inc(I, 3);
      end
      else
      if StrSame(Copy(Result, I, 4), '&gt;') then
      begin
        Result[J] := '>';
        Inc(J);
        Inc(I, 3);
      end
      else
      if StrSame(Copy(Result, I, 6), '&apos;') then
      begin
        Result[J] := #39;
        Inc(J);
        Inc(I, 5);
      end
      else
      if StrSame(Copy(Result, I, 6), '&quot;') then
      begin
        Result[J] := '"';
        Inc(J);
        Inc(I, 5);
      end
      else
      begin
        Result[J] := Result[I];
        Inc(J);
      end;
    end
    else
    begin
      Result[J] := Result[I];
      Inc(J);
    end;
    Inc(I);
  end;
  if J > 1 then
    SetLength(Result, J - 1)
  else
    SetLength(Result, 0);
end;

function SimpleXMLEncode(const S: string): string;
var
  C: Char;
  SIndex, SLen, RIndex, RLen: Integer;
  Tmp: string;
begin
  SLen := Length(S);
  RLen := SLen;
  RIndex := 1;
  SetLength(Tmp, RLen);
  for SIndex := 1 to SLen do
  begin
    C := S[SIndex];
    case C of
      '"':
        AddEntity(Tmp, RIndex, RLen, '&quot;');
      '&':
        AddEntity(Tmp, RIndex, RLen, '&amp;');
      #39:
        AddEntity(Tmp, RIndex, RLen, '&apos;');
      '<':
        AddEntity(Tmp, RIndex, RLen, '&lt;');
      '>':
        AddEntity(Tmp, RIndex, RLen, '&gt;');
      Char(128)..Char(255):
        AddEntity(Tmp, RIndex, RLen, Format('&#x%.2x;', [Ord(C)]));
      {$IFDEF SUPPORTS_UNICODE}
      Char(256)..High(Char):
        AddEntity(Tmp, RIndex, RLen, Format('&#x%.4x;', [Ord(C)]));
      {$ENDIF SUPPORTS_UNICODE}
    else
      if RIndex > RLen then
      begin
        RLen := RLen * 2;
        SetLength(Tmp, RLen);
      end;
      Tmp[RIndex] := C;
      Inc(RIndex);
    end;
  end;
  if RIndex > 1 then
    SetLength(Tmp, RIndex - 1);

  Result := Tmp;
end;

procedure SimpleXMLDecode(var S: string; TrimBlanks: Boolean);
  procedure DecodeEntity(var S: string; StringLength: Cardinal;
    var ReadIndex, WriteIndex: Cardinal);
  const
    cHexPrefix: array [Boolean] of string = ('', '$');
  var
    I: Cardinal;
    Value: Integer;
    IsHex: Boolean;
  begin
    Inc(ReadIndex, 2);
    IsHex := (ReadIndex <= StringLength) and ((S[ReadIndex] = 'x') or (S[ReadIndex] = 'X'));
    Inc(ReadIndex, Ord(IsHex));
    I := ReadIndex;
    while ReadIndex <= StringLength do
    begin
      if S[ReadIndex] = ';' then
      begin
        Value := StrToIntDef(cHexPrefix[IsHex] + Copy(S, I, ReadIndex - I), -1); // no characters are less than 0
        if Value > 0 then
          S[WriteIndex] := Chr(Value)
        else
          ReadIndex := I - (2 + Cardinal(IsHex)); // reset to start
        Exit;
      end;
      Inc(ReadIndex);
    end;
    ReadIndex := I - (2 + Cardinal(IsHex)); // reset to start
  end;

  procedure SkipBlanks(var S: string; StringLength: Cardinal; var ReadIndex: Cardinal);
  begin
    while ReadIndex < StringLength do
    begin
      if S[ReadIndex] = NativeCarriageReturn then
        S[ReadIndex] := NativeLineFeed
      else
      if S[ReadIndex + 1] = NativeCarriageReturn then
        S[ReadIndex + 1] := NativeLineFeed;
      if (S[ReadIndex] < #33) and (S[ReadIndex] = S[ReadIndex + 1]) then
        Inc(ReadIndex)
      else
        Exit;
    end;
  end;

var
  StringLength, ReadIndex, WriteIndex: Cardinal;
begin
  // NB! This procedure replaces the text inplace to speed up the conversion. This
  // works because when decoding, the string can only become shorter. This is
  // accomplished by keeping track of the current read and write points.
  // In addition, the original string length is read only once and passed to the
  // inner procedures to speed up conversion as much as possible
  ReadIndex := 1;
  WriteIndex := 1;
  StringLength := Length(S);
  while ReadIndex <= StringLength do
  begin
    // this call lowers conversion speed by ~30%, ie 21MB/sec -> 15MB/sec (repeated tests, various inputs)
    if TrimBlanks then
      SkipBlanks(S, StringLength, ReadIndex);
    if S[ReadIndex] = '&' then
    begin
      if (ReadIndex < StringLength) and (S[ReadIndex + 1] = '#') then
      begin
        DecodeEntity(S, StringLength, ReadIndex, WriteIndex);
        Inc(WriteIndex);
      end
      else
      if StrSame(Copy(S, ReadIndex, 5), '&amp;') then
      begin
        S[WriteIndex] := '&';
        Inc(WriteIndex);
        Inc(ReadIndex, 4);
      end
      else
      if StrSame(Copy(S, ReadIndex, 4), '&lt;') then
      begin
        S[WriteIndex] := '<';
        Inc(WriteIndex);
        Inc(ReadIndex, 3);
      end
      else
      if StrSame(Copy(S, ReadIndex, 4), '&gt;') then
      begin
        S[WriteIndex] := '>';
        Inc(WriteIndex);
        Inc(ReadIndex, 3);
      end
      else
      if StrSame(Copy(S, ReadIndex, 6), '&apos;') then
      begin
        S[WriteIndex] := #39;
        Inc(WriteIndex);
        Inc(ReadIndex, 5);
      end
      else
      if StrSame(Copy(S, ReadIndex, 6), '&quot;') then
      begin
        S[WriteIndex] := '"';
        Inc(WriteIndex);
        Inc(ReadIndex, 5);
      end
      else
      begin
        S[WriteIndex] := S[ReadIndex];
        Inc(WriteIndex);
      end;
    end
    else
    begin
      S[WriteIndex] := S[ReadIndex];
      Inc(WriteIndex);
    end;
    Inc(ReadIndex);
  end;
  if WriteIndex > 0 then
    SetLength(S, WriteIndex - 1)
  else
    SetLength(S, 0);
    // this call lowers conversion speed by ~65%, ie 21MB/sec -> 7MB/sec (repeated tests, various inputs)
//  if TrimBlanks then
//    S := AdjustLineBreaks(S);
end;

function XMLEncode(const S: string): string;
begin
  Result := SimpleXMLEncode(S);
end;

function XMLDecode(const S: string): string;
begin
  Result := S;
  SimpleXMLDecode(Result, False);
end;

//=== { TJclSimpleXML } ======================================================

constructor TJclSimpleXML.Create;
begin
  inherited Create;
  FRoot := TJclSimpleXMLElemClassic.Create(nil);
  FRoot.FSimpleXML := Self;
  FProlog := TJclSimpleXMLElemsProlog.Create;
  FOptions := [sxoAutoIndent, sxoAutoEncodeValue, sxoAutoEncodeEntity];
  FIndentString := '  ';
end;

destructor TJclSimpleXML.Destroy;
begin
  FreeAndNil(FRoot);
  FreeAndNil(FProlog);
  inherited Destroy;
end;

procedure TJclSimpleXML.DoDecodeValue(var Value: string);
begin
  if sxoAutoEncodeValue in Options then
    SimpleXMLDecode(Value, False)
  else
  if sxoAutoEncodeEntity in Options then
    Value := EntityDecode(Value);
  if Assigned(FOnDecodeValue) then
    FOnDecodeValue(Self, Value);
end;

procedure TJclSimpleXML.DoEncodeValue(var Value: string);
begin
  if Assigned(FOnEncodeValue) then
    FOnEncodeValue(Self, Value);
  if sxoAutoEncodeValue in Options then
    Value := SimpleXMLEncode(Value)
  else
  if sxoAutoEncodeEntity in Options then
    Value := EntityEncode(Value);
end;

procedure TJclSimpleXML.DoLoadProgress(const APosition, ATotal: Integer);
begin
  if Assigned(FOnLoadProg) then
    FOnLoadProg(Self, APosition, ATotal);
end;

procedure TJclSimpleXML.DoSaveProgress;
begin
  if Assigned(FOnSaveProg) then
  begin
    Inc(FSaveCount);
    FOnSaveProg(Self, FSaveCurrent, FSaveCount);
  end;
end;

procedure TJclSimpleXML.DoTagParsed(const AName: string);
begin
  if Assigned(FOnTagParsed) then
    FOnTagParsed(Self, AName);
end;

procedure TJclSimpleXML.DoValueParsed(const AName, AValue: string);
begin
  if Assigned(FOnValue) then
    FOnValue(Self, AName, AValue);
end;

procedure TJclSimpleXML.LoadFromFile(const FileName: TFileName; Encoding: TJclStringEncoding; CodePage: Word);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(FileName);
    LoadFromStream(Stream, Encoding, CodePage);
  finally
    Stream.Free;
  end;
end;

procedure TJclSimpleXML.LoadFromResourceName(Instance: THandle; const ResName: string;
  Encoding: TJclStringEncoding; CodePage: Word);
{$IFNDEF MSWINDOWS}
const
  RT_RCDATA = PChar(10);
{$ENDIF !MSWINDOWS}
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(Instance, ResName, RT_RCDATA);
  try
    LoadFromStream(Stream, Encoding, CodePage);
  finally
    Stream.Free;
  end;
end;

procedure TJclSimpleXML.LoadFromStream(Stream: TStream; Encoding: TJclStringEncoding; CodePage: Word);
var
  AOutStream: TStream;
  AStringStream: TJclStringStream;
  DoFree: Boolean;
begin
  FRoot.Clear;
  FProlog.Clear;
  AOutStream := nil;
  DoFree := False;
  try
    if Assigned(FOnDecodeStream) then
    begin
      AOutStream := TMemoryStream.Create;
      DoFree := True;
      FOnDecodeStream(Self, Stream, AOutStream);
      AOutStream.Seek(0, soBeginning);
    end
    else
      AOutStream := Stream;

    case Encoding of
      seAnsi:
        begin
          AStringStream := TJclAnsiStream.Create(AOutStream, False);
          TJclAnsiStream(AStringStream).CodePage := CodePage;
        end;
      seUTF8:
        AStringStream := TJclUTF8Stream.Create(AOutStream, False);
      seUTF16:
        AStringStream := TJclUTF16Stream.Create(AOutStream, False);
    else
      AStringStream := TJclAutoStream.Create(AOutStream, False);
      if CodePage <> CP_ACP then
        TJclAutoStream(AStringStream).CodePage := CodePage;
    end;
    try
      AStringStream.SkipBOM;

      LoadFromStringStream(AStringStream);

      // save codepage and encoding for future saves
      if AStringStream is TJclAutoStream then
      begin
        FCodePage := TJclAutoStream(AStringStream).CodePage;
        FEncoding := TJclAutoStream(AStringStream).Encoding;
      end
      else
      if AStringStream is TJclAnsiStream then
      begin
        FCodePage := TJclAnsiStream(AStringStream).CodePage;
        FEncoding := Encoding;
      end
      else
      begin
        FCodePage := CodePage;
        FEncoding := Encoding;
      end;
    finally
      AStringStream.Free;
    end;
  finally
    if DoFree then
      AOutStream.Free;
  end;
end;

procedure TJclSimpleXML.LoadFromStringStream(StringStream: TJclStringStream);
begin
  if Assigned(FOnLoadProg) then
    FOnLoadProg(Self, StringStream.Stream.Position, StringStream.Stream.Size);

  // Read doctype and so on
  FProlog.LoadFromStringStream(StringStream, Self);
  // Read elements
  FRoot.LoadFromStringStream(StringStream, Self);

  if Assigned(FOnLoadProg) then
    FOnLoadProg(Self, StringStream.Stream.Position, StringStream.Stream.Size);
end;

procedure TJclSimpleXML.LoadFromString(const Value: string);
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(Value);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJclSimpleXML.SaveToFile(const FileName: TFileName; Encoding: TJclStringEncoding; CodePage: Word);
var
  Stream: TFileStream;
begin
  if SysUtils.FileExists(FileName) then
  begin
    Stream := TFileStream.Create(FileName, fmOpenWrite);
    Stream.Size := 0;
  end
  else
    Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream, Encoding, CodePage);
  finally
    Stream.Free;
  end;
end;

procedure TJclSimpleXML.SaveToStream(Stream: TStream; Encoding: TJclStringEncoding; CodePage: Word);
var
  AOutStream: TStream;
  AStringStream: TJclStringStream;
  DoFree: Boolean;
  XmlHeader: TJclSimpleXMLElemHeader;
  I: Integer;
begin
  if Assigned(FOnEncodeStream) then
  begin
    AOutStream := TMemoryStream.Create;
    DoFree := True;
  end
  else
  begin
    AOutStream := Stream;
    DoFree := False;
  end;
  try
    if Encoding = seAuto then
    begin
      XmlHeader := nil;
      for I := 0 to Prolog.Count - 1 do
        if Prolog.Item[I] is TJclSimpleXMLElemHeader then
      begin
        XmlHeader := TJclSimpleXMLElemHeader(Prolog.Item[I]);
        Break;
      end;
      if Assigned(XmlHeader) then
      begin
        CodePage := CodePageFromCharsetName(XmlHeader.Encoding);
        case CodePage of
          CP_UTF8:
            Encoding := seUTF8;
          CP_UTF16LE:
            Encoding := seUTF16;
        else
          Encoding := seAnsi;
        end;
      end
      else
      begin
        // restore from previous load
        Encoding := FEncoding;
        CodePage := FCodePage;
      end;
    end;

    case Encoding of
      seUTF8:
        AStringStream := TJclUTF8Stream.Create(AOutStream, False);
      seUTF16:
        AStringStream := TJclUTF16Stream.Create(AOutStream, False);
    else
      AStringStream := TJclAnsiStream.Create(AOutStream);
      TJclAnsiStream(AStringStream).CodePage := CodePage;
    end;
    try
      AStringStream.WriteBOM;
      SaveToStringStream(AStringStream);
      AStringStream.Flush;
    finally
      AStringStream.Free;
    end;
    if Assigned(FOnEncodeStream) then
    begin
      AOutStream.Seek(0, soBeginning);
      FOnEncodeStream(Self, AOutStream, Stream);
    end;
  finally
    if DoFree then
      AOutStream.Free;
  end;
end;

procedure TJclSimpleXML.SaveToStringStream(StringStream: TJclStringStream);
var
  lCount: Integer;
begin
  lCount := Root.ChildsCount + Prolog.Count;
  FSaveCount := lCount;
  FSaveCurrent := 0;

  if Assigned(FOnSaveProg) then
    FOnSaveProg(Self, 0, lCount);

  if not (sxoDoNotSaveProlog in FOptions) then
    Prolog.SaveToStringStream(StringStream, Self);

  Root.SaveToStringStream(StringStream, BaseIndentString, Self);

  if Assigned(FOnSaveProg) then
    FOnSaveProg(Self, lCount, lCount);
end;

function TJclSimpleXML.SaveToString: string;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('');
  try
    SaveToStream(Stream);
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

procedure TJclSimpleXML.SetBaseIndentString(const Value: string);
begin
  // test if the new value is only made of spaces or tabs
  if not StrContainsChars(Value, CharIsWhiteSpace, True) then
    Exit;

  FBaseIndentString := Value;
end;

procedure TJclSimpleXML.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
  LoadFromFile(Value);
end;

//=== { TJclSimpleXMLElem } ==================================================

procedure TJclSimpleXMLElem.Assign(Value: TJclSimpleXMLElem);
var
  Elems: TJclSimpleXMLElem;
  Elem: TJclSimpleXMLElem;
  I: Integer;
begin
  Clear;
  if Value = nil then
    Exit;
  Elems := TJclSimpleXMLElem(Value);
  Name := Elems.Name;
  Self.Value := Elems.Value;
  for I := 0 to Elems.Properties.Count - 1 do
    Properties.Add(Elems.Properties[I].Name, Elems.Properties[I].Value);

  for I := 0 to Elems.Items.Count - 1 do
  begin
    // Create from the class type, so that the virtual constructor is called
    // creating an element of the correct class type.
    Elem := TJclSimpleXMLElemClass(Elems.Items[I].ClassType).Create(Self);
    Elem.Assign(Elems.Items[I]);
    Items.Add(Elem);
  end;
end;

procedure TJclSimpleXMLElem.Clear;
begin
  if FItems <> nil then
    FItems.Clear;
  if FProps <> nil then
    FProps.Clear;
end;

constructor TJclSimpleXMLElem.Create(const AOwner: TJclSimpleXMLElem);
begin
  inherited Create;
  FName := '';
  FParent := TJclSimpleXMLElem(AOwner);
  if Assigned(FParent) then
    FSimpleXML := FParent.FSimpleXML;
  FContainer := nil;
end;

destructor TJclSimpleXMLElem.Destroy;
begin
  FParent := nil;
  Clear;
  FreeAndNil(FItems);
  FreeAndNil(FProps);
  inherited Destroy;
end;

procedure TJclSimpleXMLElem.Error(const S: string);
begin
  raise EJclSimpleXMLError.Create(S);
end;

procedure TJclSimpleXMLElem.FmtError(const S: string;
  const Args: array of const);
begin
  Error(Format(S, Args));
end;

function TJclSimpleXMLElem.FullName: string;
begin
  if FNameSpace <> '' then
    Result := FNameSpace + ':' + Name
  else
    Result := Name;
end;

function TJclSimpleXMLElem.GetAnsiValue: AnsiString;
begin
  Result := AnsiString(Value);
end;

procedure TJclSimpleXMLElem.GetBinaryValue(Stream: TStream);
var
  I, J, ValueLength, RequiredStreamSize: Integer;
  Buf: array [0..cBufferSize - 1] of Byte;
  N1, N2: Byte;

  function NibbleCharToNibble(const AChar: Char): Byte;
  begin
    case AChar of
      '0': Result := 0;
      '1': Result := 1;
      '2': Result := 2;
      '3': Result := 3;
      '4': Result := 4;
      '5': Result := 5;
      '6': Result := 6;
      '7': Result := 7;
      '8': Result := 8;
      '9': Result := 9;
      'a', 'A': Result := 10;
      'b', 'B': Result := 11;
      'c', 'C': Result := 12;
      'd', 'D': Result := 13;
      'e', 'E': Result := 14;
      'f', 'F': Result := 15;
      else
        Result := 16;
    end;
  end;

  procedure PrepareNibbleCharMapping;
  var
    C: Char;
  begin
    if not PreparedNibbleCharMapping then
    begin
      for C := Low(Char) to High(Char) do
        NibbleCharMapping[C] := NibbleCharToNibble(C);
      PreparedNibbleCharMapping := True;
    end;
  end;

var
  CurrentStreamPosition: Integer;
begin
  PrepareNibbleCharMapping;
  I := 1;
  J := 0;
  ValueLength := Length(Value);
  RequiredStreamSize := Stream.Position + ValueLength div 2;
  if Stream.Size < RequiredStreamSize then
  begin
    CurrentStreamPosition := Stream.Position;
    Stream.Size := RequiredStreamSize;
    Stream.Seek(CurrentStreamPosition, soBeginning);
  end;
  while I < ValueLength do
  begin
    //faster replacement for St := '$' + Value[I] + Value[I + 1]; Buf[J] := StrToIntDef(St, 0);
    N1 := NibbleCharMapping[Value[I]];
    N2 := NibbleCharMapping[Value[I + 1]];
    Inc(I, 2);
    if (N1 > 15) or (N2 > 15) then
      Buf[J] := 0
    else
      Buf[J] := (N1 shl 4) or N2;
    Inc(J);
    if J = cBufferSize - 1 then //Buffered write to speed up the process a little
    begin
      Stream.Write(Buf, J);
      J := 0;
    end;
  end;
  Stream.Write(Buf, J);
end;

function TJclSimpleXMLElem.GetBoolValue: Boolean;
begin
  Result := StrToBoolDef(Value, False);
end;

function TJclSimpleXMLElem.GetChildIndex(
  const AChild: TJclSimpleXMLElem): Integer;
begin
  if FItems = nil then
    Result := -1
  else
    Result := FItems.FElems.IndexOfObject(AChild);
end;

function TJclSimpleXMLElem.GetChildsCount: Integer;
var
  I: Integer;
begin
  Result := 1;
  if FItems <> nil then
    for I := 0 to FItems.Count - 1 do
      Result := Result + FItems[I].ChildsCount;
end;

function TJclSimpleXMLElem.GetFloatValue: Extended;
begin
  Result := 0.0;
  if not TryStrToFloat(Value, Result) then
    Result := 0.0;
end;

function TJclSimpleXMLElem.GetIntValue: Int64;
begin
  Result := StrToInt64Def(Value, -1);
end;

function TJclSimpleXMLElem.GetItems: TJclSimpleXMLElems;
begin
  if FItems = nil then
    FItems := TJclSimpleXMLElems.Create(Self);
  Result := FItems;
end;

function TJclSimpleXMLElem.GetNamedIndex(const AChild: TJclSimpleXMLElem): Integer;
begin
  Result := Items.NamedElems[AChild.Name].IndexOf(AChild);
end;

function TJclSimpleXMLElem.GetProps: TJclSimpleXMLProps;
begin
  if FProps = nil then
    FProps := TJclSimpleXMLProps.Create(Self);
  Result := FProps;
end;

function TJclSimpleXMLElem.GetSimpleXML: TJclSimpleXML;
begin
  if FParent <> nil then
    Result := FParent.GetSimpleXML
  else
    Result := FSimpleXML;
end;

procedure TJclSimpleXMLElem.LoadFromString(const Value: string);
var
  Stream: TJclStringStream;
  StrStream: TStringStream;
begin
  StrStream := TStringStream.Create(Value);
  try
    Stream := TJclAutoStream.Create(StrStream);
    try
      LoadFromStringStream(Stream);
    finally
      Stream.Free;
    end;
  finally
    StrStream.Free;
  end;
end;

function TJclSimpleXMLElem.SaveToString: string;
var
  Stream: TJclStringStream;
  StrStream: TStringStream;
begin
  StrStream := TStringStream.Create('');
  try
    Stream := TJclAutoStream.Create(StrStream);
    try
      SaveToStringStream(Stream);
      Stream.Flush;
    finally
      Stream.Free;
    end;
    Result := StrStream.DataString;
  finally
    StrStream.Free;
  end;
end;

procedure TJclSimpleXMLElem.SetAnsiValue(const Value: AnsiString);
begin
  Self.Value := string(Value);
end;

procedure TJclSimpleXMLElem.SetBoolValue(const Value: Boolean);
begin
  FValue := BoolToStr(Value);
end;

procedure TJclSimpleXMLElem.SetFloatValue(const Value: Extended);
begin
  FValue := FloatToStr(Value);
end;

procedure TJclSimpleXMLElem.SetIntValue(const Value: Int64);
begin
  FValue := IntToStr(Value);
end;

procedure TJclSimpleXMLElem.SetName(const Value: string);
begin
  if (Value <> FName) and (Value <> '') then
  begin
    if (Parent <> nil) and (FName <> '') then
      Parent.Items.DoItemRename(Self, Value);
    FName := Value;
  end;
end;

//=== { TJclSimpleXMLNamedElems } ============================================

constructor TJclSimpleXMLNamedElems.Create(const AOwner: TJClSimpleXMLElems; const AName: string);
begin
  inherited Create;
  FElems := AOwner;
  FName := AName;
  FItems := TList.Create;
end;

destructor TJclSimpleXMLNamedElems.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TJclSimpleXMLNamedElems.Add(const Value: Int64): TJclSimpleXmlElemClassic;
begin
  Result := Elems.Add(Name, Value);
end;

function TJclSimpleXMLNamedElems.Add(Value: TStream): TJclSimpleXmlElemClassic;
begin
  Result := Elems.Add(Name, Value);
end;

function TJclSimpleXMLNamedElems.Add(const Value: Boolean): TJclSimpleXmlElemClassic;
begin
  Result := Elems.Add(Name, Value);
end;

function TJclSimpleXMLNamedElems.Add: TJclSimpleXmlElemClassic;
begin
  Result := Elems.Add(Name);
end;

function TJclSimpleXMLNamedElems.Add(const Value: string): TJclSimpleXmlElemClassic;
begin
  Result := Elems.Add(Name, Value);
end;

function TJclSimpleXMLNamedElems.AddCData(const Value: string): TJclSimpleXMLElemCData;
begin
  Result := Elems.AddCData(Name, Value);
end;

function TJclSimpleXMLNamedElems.AddComment(const Value: string): TJclSimpleXMLElemComment;
begin
  Result := Elems.AddComment(Name, Value);
end;

function TJclSimpleXMLNamedElems.AddFirst: TJclSimpleXmlElemClassic;
begin
  Result := Elems.AddFirst(Name);
end;

function TJclSimpleXMLNamedElems.AddText(const Value: string): TJclSimpleXMLElemText;
begin
  Result := Elems.AddText(Name, Value);
end;

procedure TJclSimpleXMLNamedElems.Clear;
var
  Index: Integer;
begin
  for Index := FItems.Count - 1 downto 0 do
    Elems.Remove(TJclSimpleXMLElem(FItems.Items[Index]));
end;

procedure TJclSimpleXMLNamedElems.Delete(const Index: Integer);
begin
  if (Index >= 0) and (Index < FItems.Count) then
    Elems.Remove(TJclSimpleXMLElem(FItems.Items[Index]));
end;

function TJclSimpleXMLNamedElems.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJclSimpleXMLNamedElems.GetItem(const Index: Integer): TJclSimpleXMLElem;
begin
  if (Index >= 0) then
  begin
    While (Index >= Count) do
      if Assigned(Elems.Parent) and Assigned(Elems.Parent.SimpleXML) and
         (sxoAutoCreate in Elems.Parent.SimpleXML.Options) then
        Add
      else
        break;
    if Index < Count then
      Result := TJclSimpleXMLElem(FItems.Items[Index])
    else
      Result := nil;
  end
  else
    Result := nil;
end;

function TJclSimpleXMLNamedElems.IndexOf(const Value: TJclSimpleXMLElem): Integer;
begin
  Result := FItems.IndexOf(Value);
end;

function TJclSimpleXMLNamedElems.IndexOf(const Value: string): Integer;
var
  Index: Integer;
  NewItem: TJclSimpleXMLElem;
begin
  Result := -1;
  for Index := 0 to FItems.Count - 1 do
    if TJclSimpleXMLElem(FItems.Items[Index]).Value = Value then
  begin
    Result := Index;
    Break;
  end;
  if (Result = -1) and (sxoAutoCreate in Elems.Parent.SimpleXML.Options) then
  begin
    NewItem := Elems.Add(Name, Value);
    Result := FItems.IndexOf(NewItem);
  end;
end;

procedure TJclSimpleXMLNamedElems.Move(const CurIndex, NewIndex: Integer);
var
  ElemsCurIndex, ElemsNewIndex: Integer;
begin
  ElemsCurIndex := Elems.IndexOf(TJclSimpleXMLElem(FItems.Items[CurIndex]));
  ElemsNewIndex := Elems.IndexOf(TJclSimpleXMLElem(FItems.Items[NewIndex]));
  Elems.Move(ElemsCurIndex, ElemsNewIndex);
  FItems.Move(CurIndex, NewIndex);
end;

//=== { TJclSimpleXMLElems } =================================================

function TJclSimpleXMLElems.Add(const Name: string): TJclSimpleXMLElemClassic;
begin
  Result := TJclSimpleXMLElemClassic.Create(Parent);
  Result.FName := Name; //Directly set parent to avoid notification
  AddChild(Result);
end;

function TJclSimpleXMLElems.Add(const Name, Value: string): TJclSimpleXMLElemClassic;
begin
  Result := TJclSimpleXMLElemClassic.Create(Parent);
  Result.FName := Name;
  Result.Value := Value;
  AddChild(Result);
end;

function TJclSimpleXMLElems.Add(const Name: string; const Value: Int64): TJclSimpleXMLElemClassic;
begin
  Result := TJclSimpleXMLElemClassic.Create(Parent);
  Result.FName := Name;
  Result.Value := IntToStr(Value);
  AddChild(Result);
end;

function TJclSimpleXMLElems.Add(Value: TJclSimpleXMLElem): TJclSimpleXMLElem;
begin
  if Value <> nil then
    AddChild(Value);
  Result := Value;
end;

function TJclSimpleXMLElems.Add(const Name: string;
  const Value: Boolean): TJclSimpleXMLElemClassic;
begin
  Result := TJclSimpleXMLElemClassic.Create(Parent);
  Result.FName := Name;
  Result.Value := BoolToStr(Value);
  AddChild(Result);
end;

function TJclSimpleXMLElems.Add(const Name: string; Value: TStream): TJclSimpleXMLElemClassic;
var
  Stream: TStringStream;
  Buf: array [0..cBufferSize - 1] of Byte;
  St: string;
  I, Count: Integer;
begin
  Stream := TStringStream.Create('');
  try
    Buf[0] := 0;
    repeat
      Count := Value.Read(Buf, Length(Buf));
      St := '';
      for I := 0 to Count - 1 do
        St := St + IntToHex(Buf[I], 2);
      Stream.WriteString(St);
    until Count = 0;
    Result := TJclSimpleXMLElemClassic.Create(Parent);
    Result.FName := Name;
    Result.Value := Stream.DataString;
    AddChild(Result);
  finally
    Stream.Free;
  end;
end;

procedure TJclSimpleXMLElems.AddChild(const Value: TJclSimpleXMLElem);
var
  NamedIndex: Integer;
begin
  CreateElems;

  // If there already is a container, notify it to remove the element
  if Assigned(Value.Container) then
  begin
    Value.Container.Notify(Value, opRemove);
    Value.Parent := Parent;
  end;

  FElems.AddObject(Value.Name, Value);

  if FNamedElems <> nil then
  begin
    NamedIndex := FNamedElems.IndexOf(Value.Name);
    if NamedIndex >= 0 then
      TJclSimpleXMLNamedElems(FNamedElems.Objects[NamedIndex]).FItems.Add(Value);
  end;

  Notify(Value, opInsert);
end;

procedure TJclSimpleXMLElems.AddChildFirst(const Value: TJclSimpleXMLElem);
var
  NamedIndex: Integer;
begin
  CreateElems;

  // If there already is a container, notify it to remove the element
  if Assigned(Value.Container) then
  begin
    Value.Container.Notify(Value, opRemove);
    Value.Parent := Parent;
  end;

  FElems.InsertObject(0, Value.Name, Value);

  if FNamedElems <> nil then
  begin
    NamedIndex := FNamedElems.IndexOf(Value.Name);
    if NamedIndex >= 0 then
      TJclSimpleXMLNamedElems(FNamedElems.Objects[NamedIndex]).FItems.Insert(0, Value);
  end;

  Notify(Value, opInsert);
end;

function TJclSimpleXMLElems.AddFirst(const Name: string): TJclSimpleXMLElemClassic;
begin
  Result := TJclSimpleXMLElemClassic.Create(Parent);
  Result.FName := Name; //Directly set parent to avoid notification
  AddChildFirst(Result);
end;

function TJclSimpleXMLElems.AddFirst(Value: TJclSimpleXMLElem): TJclSimpleXMLElem;
begin
  if Value <> nil then
    AddChildFirst(Value);
  Result := Value;
end;

function TJclSimpleXMLElems.AddComment(const Name,
  Value: string): TJclSimpleXMLElemComment;
begin
  Result := TJclSimpleXMLElemComment.Create(Parent);
  Result.FName := Name;
  Result.Value := Value;
  AddChild(Result);
end;

function TJclSimpleXMLElems.AddCData(const Name, Value: string): TJclSimpleXMLElemCData;
begin
  Result := TJclSimpleXMLElemCData.Create(Parent);
  Result.FName := Name;
  Result.Value := Value;
  AddChild(Result);
end;

function TJclSimpleXMLElems.AddText(const Name, Value: string): TJclSimpleXMLElemText;
begin
  Result := TJclSimpleXMLElemText.Create(Parent);
  Result.FName := Name;
  Result.Value := Value;
  AddChild(Result);
end;

procedure TJclSimpleXMLElems.BinaryValue(const Name: string; Stream: TStream);
var
  Elem: TJclSimpleXMLElem;
begin
  Elem := GetItemNamed(Name);
  if Elem <> nil then
    Elem.GetBinaryValue(Stream);
end;

function TJclSimpleXMLElems.BoolValue(const Name: string; Default: Boolean): Boolean;
var
  Elem: TJclSimpleXMLElem;
begin
  try
    Elem := GetItemNamedDefault(Name, BoolToStr(Default));
    if (Elem = nil) or (Elem.Value = '') then
      Result := Default
    else
      Result := Elem.BoolValue;
  except
    Result := Default;
  end;
end;

procedure TJclSimpleXMLElems.Clear;
var
  I: Integer;
begin
  if FElems <> nil then
  begin
    for I := 0 to FElems.Count - 1 do
    begin
      // TJclSimpleXMLElem(FElems.Objects[I]).Clear; // (p3) not needed -called in Destroy
      FElems.Objects[I].Free;
      FElems.Objects[I] := nil;
    end;
    FElems.Clear;
  end;
  if FNamedElems <> nil then
  begin
    for I := 0 to FNamedElems.Count - 1 do
    begin
      FNamedElems.Objects[I].Free;
      FNamedElems.Objects[I] := nil;
    end;
    FNamedElems.Clear;
  end;
end;

constructor TJclSimpleXMLElems.Create(const AOwner: TJclSimpleXMLElem);
begin
  inherited Create;
  FParent := AOwner;
end;

procedure TJclSimpleXMLElems.CreateElems;
begin
  if FElems = nil then
    FElems := THashedStringList.Create;
end;

procedure TJclSimpleXMLElems.Delete(const Index: Integer);
var
  Elem: TJclSimpleXMLElem;
  NamedIndex: Integer;
begin
  if (FElems <> nil) and (Index >= 0) and (Index < FElems.Count) then
  begin
    Elem := TJclSimpleXMLElem(FElems.Objects[Index]);
    if FNamedElems <> nil then
    begin
      NamedIndex := FNamedElems.IndexOf(Elem.Name);
      if NamedIndex >= 0 then
        TJclSimpleXMLNamedElems(FNamedElems.Objects[NamedIndex]).FItems.Remove(Elem);
    end;
    FElems.Delete(Index);
    FreeAndNil(Elem);
    
  end;
end;

procedure TJclSimpleXMLElems.Delete(const Name: string);
begin
  if FElems <> nil then
    Delete(FElems.IndexOf(Name));
end;

destructor TJclSimpleXMLElems.Destroy;
begin
  FParent := nil;
  Clear;
  FreeAndNil(FElems);
  FreeAndNil(FNamedElems);
  inherited Destroy;
end;

procedure TJclSimpleXMLElems.DoItemRename(Value: TJclSimpleXMLElem; const Name: string);
var
  I: Integer;
  NamedIndex: Integer;
begin
  if FNamedElems <> nil then
  begin
    NamedIndex := FNamedElems.IndexOf(Value.Name);
    if NamedIndex >= 0 then
      TJclSimpleXMLNamedElems(FNamedElems.Objects[NamedIndex]).FItems.Remove(Value);
  end;

  I := FElems.IndexOfObject(Value);
  if I <> -1 then
    FElems.Strings[I] := Name;

  if FNamedElems <> nil then
  begin
    NamedIndex := FNamedElems.IndexOf(Name);
    if NamedIndex >= 0 then
      TJclSimpleXMLNamedElems(FNamedElems.Objects[NamedIndex]).FItems.Add(Value);
  end;
end;

function TJclSimpleXMLElems.FloatValue(const Name: string;
  const Default: Extended): Extended;
var
  Elem: TJclSimpleXMLElem;
begin
  Elem := GetItemNamedDefault(Name, FloatToStr(Default));
  if Elem = nil then
    Result := Default
  else
    Result := Elem.FloatValue;
end;

function TJclSimpleXMLElems.GetCount: Integer;
begin
  if FElems = nil then
    Result := 0
  else
    Result := FElems.Count;
end;

function TJclSimpleXMLElems.GetItem(const Index: Integer): TJclSimpleXMLElem;
begin
  if (FElems = nil) or (Index > FElems.Count) then
    Result := nil
  else
    Result := TJclSimpleXMLElem(FElems.Objects[Index]);
end;

function TJclSimpleXMLElems.GetItemNamedDefault(const Name, Default: string): TJclSimpleXMLElem;
var
  I: Integer;
begin
  Result := nil;
  if FElems <> nil then
  begin
    I := FElems.IndexOf(Name);
    if I <> -1 then
      Result := TJclSimpleXMLElem(FElems.Objects[I])
    else
    if Assigned(Parent) and Assigned(Parent.SimpleXML) and (sxoAutoCreate in Parent.SimpleXML.Options) then
      Result := Add(Name, Default);
  end
  else
  if Assigned(Parent) and Assigned(Parent.SimpleXML) and (sxoAutoCreate in Parent.SimpleXML.Options) then
    Result := Add(Name, Default);
end;

function TJclSimpleXMLElems.GetNamedElems(const Name: string): TJclSimpleXMLNamedElems;
var
  NamedIndex: Integer;
begin
  if FNamedElems = nil then
    FNamedElems := THashedStringList.Create;
  NamedIndex := FNamedElems.IndexOf(Name);
  if NamedIndex = -1 then
  begin
    Result := TJclSimpleXMLNamedElems.Create(Self, Name);
    FNamedElems.AddObject(Name, Result);
    if FElems <> nil then
      for NamedIndex := 0 to FElems.Count - 1 do                 
        if FElems.Strings[NamedIndex] = Name then
          Result.FItems.Add(FElems.Objects[NamedIndex]);
  end
  else
    Result := TJclSimpleXMLNamedElems(FNamedElems.Objects[NamedIndex]);
end;

function TJclSimpleXMLElems.GetItemNamed(const Name: string): TJclSimpleXMLElem;
begin
  Result := GetItemNamedDefault(Name, '');
end;

function TJclSimpleXMLElems.IntValue(const Name: string; const Default: Int64): Int64;
var
  Elem: TJclSimpleXMLElem;
begin
  Elem := GetItemNamedDefault(Name, IntToStr(Default));
  if Elem = nil then
    Result := Default
  else
    Result := Elem.IntValue;
end;

procedure TJclSimpleXMLElems.LoadFromStringStream(StringStream: TJclStringStream; AParent: TJclSimpleXML);
type
  TReadStatus = (rsWaitingTag, rsReadingTagKind);
var
  lPos: TReadStatus;
  St: string;
  lElem: TJclSimpleXMLElem;
  Ch: Char;
  lContainsText: Boolean;
begin
  St := '';
  lPos := rsWaitingTag;
  lContainsText := False;

  // We read from a stream, thus replacing the existing items
  Clear;

  if AParent <> nil then
    AParent.DoLoadProgress(StringStream.Stream.Position, StringStream.Stream.Size);

  while StringStream.PeekChar(Ch) do
  begin
    case lPos of
      rsWaitingTag: //We are waiting for a tag and thus avoiding spaces
        begin
          if Ch = '<' then
          begin
            lPos := rsReadingTagKind;
            St := Ch;
          end
          else
          if not CharIsWhiteSpace(Ch) then
            lContainsText := True;
        end;

      rsReadingTagKind: //We are trying to determine the kind of the tag
        begin
          lElem := nil;
          case Ch of
            '/':
              if St = '<' then
              begin // "</"
                // We have reached an end tag. If whitespace was found while
                // waiting for the end tag, and the user told us to keep it
                // then we have to create a text element.
                // But it must only be created if there are no other elements
                // in the list. If we did not check this, we would create a
                // text element for whitespace found between two adjacent end
                // tags.
                if lContainsText then
                begin
                  lElem := TJclSimpleXMLElemText.Create(Parent);
                  lElem.LoadFromStringStream(StringStream, AParent);
                  CreateElems;
                  FElems.AddObject(lElem.Name, lElem);
                  Notify(lElem,opInsert);
                end;
                Break;
              end
              else
              begin
                lElem := TJclSimpleXMLElemClassic.Create(Parent);
                St := St + Ch; // "<name/"
                lPos := rsWaitingTag;
              end;

            NativeSpace, '>', ':': //This should be a classic tag
              begin    // "<XXX " or "<XXX:" or "<XXX>
                lElem := TJclSimpleXMLElemClassic.Create(Parent);
                St := '';
                lPos := rsWaitingTag;
              end;
          else
            if lContainsText then
            begin
              // inner text
              lElem := TJclSimpleXMLElemText.Create(Parent);
              lPos := rsReadingTagKind;
              lContainsText := False;
            end
            else
            begin
              if (St <> '<![CDATA') or not CharIsWhiteSpace(Ch) then
                St := St + Ch;
              if St = '<![CDATA[' then
              begin
                lElem := TJclSimpleXMLElemCData.Create(Parent);
                lPos := rsWaitingTag;
                St := '';
              end
              else
              if St = '<!--' then
              begin
                lElem := TJclSimpleXMLElemComment.Create(Parent);
                lPos := rsWaitingTag;
                St := '';
              end
              else
              if St = '<?' then
              begin
                lElem := TJclSimpleXMLElemProcessingInstruction.Create(Parent);
                lPos := rsWaitingTag;
                St := '';
              end;
            end;
          end;

          if lElem <> nil then
          begin
            CreateElems;
            lElem.LoadFromStringStream(StringStream, AParent);
            FElems.AddObject(lElem.Name, lElem);
            Notify(lElem, opInsert);
          end;
        end;
    end;
  end;
end;

procedure TJclSimpleXMLElems.Notify(Value: TJclSimpleXMLElem;
  Operation: TOperation);
var
  NamedIndex: Integer;
begin
  case Operation of
    opRemove:
      if Value.Container = Self then  // Only remove if we have it
      begin
        if FNamedElems <> nil then
        begin
          NamedIndex := FNamedElems.IndexOf(Value.Name);
          if NamedIndex >= 0 then
            TJclSimpleXMLNamedElems(FNamedElems.Objects[NamedIndex]).FItems.Remove(Value);
        end;
        FElems.Delete(FElems.IndexOfObject(Value));
      end;
    opInsert:
      Value.Container := Self;
  end;
end;

function TJclSimpleXMLElems.Remove(Value: TJclSimpleXMLElem): Integer;
begin
  Result := FElems.IndexOfObject(Value);
  Notify(Value, opRemove);
end;

procedure TJclSimpleXMLElems.SaveToStringStream(StringStream: TJclStringStream;
  const Level: string; AParent: TJclSimpleXML);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Item[I].SaveToStringStream(StringStream, Level, AParent);
end;

function TJclSimpleXMLElems.Value(const Name, Default: string): string;
var
  Elem: TJclSimpleXMLElem;
begin
  Result := '';
  Elem := GetItemNamedDefault(Name, Default);
  if Elem = nil then
    Result := Default
  else
    Result := Elem.Value;
end;

procedure TJclSimpleXMLElems.Move(const CurIndex, NewIndex: Integer);
begin
  if FElems <> nil then
    FElems.Move(CurIndex, NewIndex);
end;

function TJclSimpleXMLElems.IndexOf(const Value: TJclSimpleXMLElem): Integer;
begin
  if FElems = nil then
    Result := -1
  else
    Result := FElems.IndexOfObject(Value);
end;

function TJclSimpleXMLElems.IndexOf(const Name: string): Integer;
begin
  if FElems = nil then
    Result := -1
  else
    Result := FElems.IndexOf(Name);
end;

procedure TJclSimpleXMLElems.InsertChild(const Value: TJclSimpleXMLElem; Index: Integer);
var
  NamedIndex: Integer;
begin
  CreateElems;

  // If there already is a container, notify it to remove the element
  if Assigned(Value.Container) then
  begin
    Value.Container.Notify(Value, opRemove);
    Value.Parent := Parent;
  end;

  FElems.InsertObject(Index, Value.Name, Value);

  if FNamedElems <> nil then
  begin
    NamedIndex := FNamedElems.IndexOf(Value.Name);
    if NamedIndex >= 0 then
      TJclSimpleXMLNamedElems(FNamedElems.Objects[NamedIndex]).FItems.Add(Value);
  end;

  Notify(Value, opInsert);
end;

function TJclSimpleXMLElems.Insert(Value: TJclSimpleXMLElem;
  Index: Integer): TJclSimpleXMLElem;
begin
  if Value <> nil then
    InsertChild(Value, Index);
  Result := Value;
end;

function TJclSimpleXMLElems.Insert(const Name: string;
  Index: Integer): TJclSimpleXMLElemClassic;
begin
  Result := TJclSimpleXMLElemClassic.Create(Parent);
  Result.FName := Name; //Directly set parent to avoid notification
  InsertChild(Result, Index);
end;

function SortItems(List: TStringList; Index1, Index2: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to GSorts.Count - 1 do
    if TJclSimpleXMLElems(GSorts[I]).FElems = List then
    begin
      Result := TJclSimpleXMLElems(GSorts[I]).FCompare(TJclSimpleXMLElems(GSorts[I]), Index1, Index2);
      Break;
    end;
end;

procedure TJclSimpleXMLElems.CustomSort(AFunction: TJclSimpleXMLElemCompare);
begin
  if FElems <> nil then
  begin
    GSorts.Add(Self);
    FCompare := AFunction;
    FElems.CustomSort(SortItems);
    GSorts.Remove(Self);
  end;
end;

procedure TJclSimpleXMLElems.Sort;
begin
  if FElems <> nil then
    FElems.Sort;
end;

//=== { TJclSimpleXMLProps } =================================================

function TJclSimpleXMLProps.Add(const Name, Value: string): TJclSimpleXMLProp;
var
  Elem: TJclSimpleXMLProp;
begin
  if FProperties = nil then
    FProperties := THashedStringList.Create;
  Elem := TJclSimpleXMLProp.Create();
  FProperties.AddObject(Name, Elem);
  Elem.FName := Name; //Avoid notification
  Elem.Value := Value;
  Elem.Parent := Self;
  Result := Elem;
end;

function TJclSimpleXMLProps.Add(const Name: string; const Value: Int64): TJclSimpleXMLProp;
begin
  Result := Add(Name, IntToStr(Value));
end;

function TJclSimpleXMLProps.Add(const Name: string; const Value: Boolean): TJclSimpleXMLProp;
begin
  Result := Add(Name, BoolToStr(Value));
end;

{$IFDEF SUPPORTS_UNICODE}
function TJclSimpleXMLProps.Add(const Name: string;
  const Value: AnsiString): TJclSimpleXMLProp;
begin
  Result := Add(Name, string(Value));
end;
{$ENDIF SUPPORTS_UNICODE}

function TJclSimpleXMLProps.Insert(const Index: Integer; const Name, Value: string): TJclSimpleXMLProp;
var
  Elem: TJclSimpleXMLProp;
begin
  if FProperties = nil then
    FProperties := THashedStringList.Create;
  Elem := TJclSimpleXMLProp.Create();
  FProperties.InsertObject(Index, Name, Elem);
  Elem.FName := Name; //Avoid notification
  Elem.Value := Value;
  Elem.Parent := Self;
  Result := Elem;
end;

function TJclSimpleXMLProps.Insert(const Index: Integer; const Name: string; const Value: Int64): TJclSimpleXMLProp;
begin
  Result := Insert(Index, Name, IntToStr(Value));
end;

function TJclSimpleXMLProps.Insert(const Index: Integer; const Name: string; const Value: Boolean): TJclSimpleXMLProp;
begin
  Result := Insert(Index, Name, BoolToStr(Value));
end;

function TJclSimpleXMLProps.BoolValue(const Name: string; Default: Boolean): Boolean;
var
  Prop: TJclSimpleXMLProp;
begin
  try
    Prop := GetItemNamedDefault(Name, BoolToStr(Default));
    if (Prop = nil) or (Prop.Value = '') then
      Result := Default
    else
      Result := Prop.BoolValue;
  except
    Result := Default;
  end;
end;

procedure TJclSimpleXMLProps.Clear;
var
  I: Integer;
begin
  if FProperties <> nil then
  begin
    for I := 0 to FProperties.Count - 1 do
    begin
      TJclSimpleXMLProp(FProperties.Objects[I]).Free;
      FProperties.Objects[I] := nil;
    end;
    FProperties.Clear;
  end;
end;

procedure TJclSimpleXMLProps.Delete(const Index: Integer);
begin
  if (FProperties <> nil) and (Index >= 0) and (Index < FProperties.Count) then
  begin
    TObject(FProperties.Objects[Index]).Free;
    FProperties.Delete(Index);
  end;
end;

constructor TJclSimpleXMLProps.Create(Parent: TJclSimpleXMLElem);
begin
  inherited Create;
  FParent := Parent;
end;

procedure TJclSimpleXMLProps.Delete(const Name: string);
begin
  if FProperties <> nil then
    Delete(FProperties.IndexOf(Name));
end;

destructor TJclSimpleXMLProps.Destroy;
begin
  FParent := nil;
  Clear;
  FreeAndNil(FProperties);
  inherited Destroy;
end;

procedure TJclSimpleXMLProps.DoItemRename(Value: TJclSimpleXMLProp; const Name: string);
var
  I: Integer;
begin
  if FProperties = nil then
    Exit;
  I := FProperties.IndexOfObject(Value);
  if I <> -1 then
    FProperties[I] := Name;
end;

procedure TJclSimpleXMLProps.Error(const S: string);
begin
  raise EJclSimpleXMLError.Create(S);
end;

function TJclSimpleXMLProps.FloatValue(const Name: string;
  const Default: Extended): Extended;
var
  Prop: TJclSimpleXMLProp;
begin
  Prop := GetItemNamedDefault(Name, FloatToStr(Default));
  if Prop = nil then
    Result := Default
  else
    Result := Prop.FloatValue;
end;

procedure TJclSimpleXMLProps.FmtError(const S: string;
  const Args: array of const);
begin
  Error(Format(S, Args));
end;

function TJclSimpleXMLProps.GetCount: Integer;
begin
  if FProperties = nil then
    Result := 0
  else
    Result := FProperties.Count;
end;

function TJclSimpleXMLProps.GetItem(const Index: Integer): TJclSimpleXMLProp;
begin
  if FProperties <> nil then
    Result := TJclSimpleXMLProp(FProperties.Objects[Index])
  else
    Result := nil;
end;

function TJclSimpleXMLProps.GetItemNamedDefault(const Name, Default: string): TJclSimpleXMLProp;
var
  I: Integer;
begin
  Result := nil;
  if FProperties <> nil then
  begin
    I := FProperties.IndexOf(Name);
    if I <> -1 then
      Result := TJclSimpleXMLProp(FProperties.Objects[I])
    else
    if Assigned(FParent) and Assigned(FParent.SimpleXML) and (sxoAutoCreate in FParent.SimpleXML.Options) then
      Result := Add(Name, Default);
  end
  else
  if Assigned(FParent) and Assigned(FParent.SimpleXML) and (sxoAutoCreate in FParent.SimpleXML.Options) then
  begin
    Result := Add(Name, Default);
  end;
end;

function TJclSimpleXMLProps.GetItemNamed(const Name: string): TJclSimpleXMLProp;
begin
  Result := GetItemNamedDefault(Name, '');
end;

function TJclSimpleXMLProps.GetSimpleXML: TJclSimpleXML;
begin
  if FParent <> nil then
    Result := FParent.GetSimpleXML
  else
    Result := nil;
end;

function TJclSimpleXMLProps.IntValue(const Name: string; const Default: Int64): Int64;
var
  Prop: TJclSimpleXMLProp;
begin
  Prop := GetItemNamedDefault(Name, IntToStr(Default));
  if Prop = nil then
    Result := Default
  else
    Result := Prop.IntValue;
end;

procedure TJclSimpleXMLProps.LoadFromStringStream(StringStream: TJclStringStream);
//<element Prop="foo" Prop='bar' foo:bar="beuh"/>
//Stop on / or ? or >
type
  TPosType = (
    ptWaiting,
    ptReadingName,
    ptStartingContent,
    ptReadingValue,
    ptSpaceBeforeEqual
    );
var
  lPos: TPosType;
  lName, lValue, lNameSpace: string;
  lPropStart: Char;
  Ch: Char;
begin
  lValue := '';
  lNameSpace := '';
  lName := '';
  lPropStart := NativeSpace;
  lPos := ptWaiting;

  // We read from a stream, thus replacing the existing properties
  Clear;

  while StringStream.PeekChar(Ch) do
  begin
    case lPos of
      ptWaiting: //We are waiting for a property
        begin
          if CharIsWhiteSpace(Ch) then
            StringStream.ReadChar(Ch)
          else
          if CharIsValidIdentifierLetter(Ch) or (Ch = '-') or (Ch = '.') then
          begin
            StringStream.ReadChar(Ch);
            lName := Ch;
            lNameSpace := '';
            lPos := ptReadingName;
          end
          else
          if (Ch = '/') or (Ch = '>') or (Ch = '?') then
            // end of properties
            Break
          else
            FmtError(LoadResString(@RsEInvalidXMLElementUnexpectedCharacte), [Ch, StringStream.PeekPosition]);
        end;

      ptReadingName: //We are reading a property name
        begin
          StringStream.ReadChar(Ch);
          if CharIsValidIdentifierLetter(Ch) or (Ch = '-') or (Ch = '.') then
          begin
            lName := lName + Ch;
          end
          else
          if Ch = ':' then
          begin
            lNameSpace := lName;
            lName := '';
          end
          else
          if Ch = '=' then
            lPos := ptStartingContent
          else
          if CharIsWhiteSpace(Ch) then
            lPos := ptSpaceBeforeEqual
          else
            FmtError(LoadResString(@RsEInvalidXMLElementUnexpectedCharacte), [Ch, StringStream.PeekPosition]);
        end;

      ptStartingContent: //We are going to start a property content
        begin
          StringStream.ReadChar(Ch);
          if CharIsWhiteSpace(Ch) then
            // ignore white space
          else
          if (Ch = '''') or (Ch = '"') then
          begin
            lPropStart := Ch;
            lValue := '';
            lPos := ptReadingValue;
          end
          else
            FmtError(LoadResString(@RsEInvalidXMLElementUnexpectedCharacte_), [Ch, StringStream.PeekPosition]);
        end;

      ptReadingValue: //We are reading a property
        begin
          StringStream.ReadChar(Ch);
          if Ch = lPropStart then
          begin
            if GetSimpleXML <> nil then
              GetSimpleXML.DoDecodeValue(lValue);
            with Add(lName, lValue) do
              NameSpace := lNameSpace;
            lPos := ptWaiting;
          end
          else
            lValue := lValue + Ch;
        end;

      ptSpaceBeforeEqual: // We are reading the white space between a property name and the = sign
        begin
          StringStream.ReadChar(Ch);
          if CharIsWhiteSpace(Ch) then
            // more white space, stay in this state and ignore
          else
          if Ch = '=' then
            lPos := ptStartingContent
          else
            FmtError(LoadResString(@RsEInvalidXMLElementUnexpectedCharacte), [Ch, StringStream.PeekPosition]);
        end;
    else
      Assert(False, RsEUnexpectedValueForLPos);
    end;
  end;
end;

procedure TJclSimpleXMLProps.SaveToStringStream(StringStream: TJclStringStream);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Item[I].SaveToStringStream(StringStream);
end;

function TJclSimpleXMLProps.Value(const Name, Default: string): string;
var
  Prop: TJclSimpleXMLProp;
begin
  Result := '';
  Prop := GetItemNamedDefault(Name, Default);
  if Prop = nil then
    Result := Default
  else
    Result := Prop.Value;
end;

//=== { TJclSimpleXMLProp } ==================================================

function TJclSimpleXMLProp.GetAnsiValue: AnsiString;
begin
  Result := AnsiString(Value);
end;

function TJclSimpleXMLProp.GetBoolValue: Boolean;
begin
  Result := StrToBoolDef(Value, False);
end;

function TJclSimpleXMLProp.GetFloatValue: Extended;
begin
  Result := 0.0;
  if not TryStrToFloat(Value, Result) then
    Result := 0.0;
end;

function TJclSimpleXMLProp.FullName: string;
begin
  if FNameSpace <> '' then
    Result := FNameSpace + ':' + Name
  else
    Result := Name;
end;

function TJclSimpleXMLProp.GetIntValue: Int64;
begin
  Result := StrToInt64Def(Value, -1);
end;

function TJclSimpleXMLProp.GetSimpleXML: TJclSimpleXML;
begin
  if (FParent <> nil) and (FParent.FParent <> nil) then
    Result := FParent.FParent.GetSimpleXML
  else
    Result := nil;
end;

procedure TJclSimpleXMLProp.SaveToStringStream(StringStream: TJclStringStream);
var
  AEncoder: TJclSimpleXML;
  Tmp:string;
begin
  AEncoder := GetSimpleXML;
  Tmp := FValue;
  if AEncoder <> nil then
    AEncoder.DoEncodeValue(Tmp);
  if NameSpace <> '' then
    Tmp := Format(' %s:%s="%s"', [NameSpace, Name, Tmp])
  else
    Tmp := Format(' %s="%s"', [Name, tmp]);
  StringStream.WriteString(Tmp, 1, Length(Tmp));
end;

procedure TJclSimpleXMLProp.SetAnsiValue(const Value: AnsiString);
begin
  Self.Value := string(Value);
end;

procedure TJclSimpleXMLProp.SetBoolValue(const Value: Boolean);
begin
  FValue := BoolToStr(Value);
end;

procedure TJclSimpleXMLProp.SetFloatValue(const Value: Extended);
begin
  FValue := FloatToStr(Value);
end;

procedure TJclSimpleXMLProp.SetIntValue(const Value: Int64);
begin
  FValue := IntToStr(Value);
end;

procedure TJclSimpleXMLProp.SetName(const Value: string);
begin
  if (Value <> FName) and (Value <> '') then
  begin
    if (Parent <> nil) and (FName <> '') then
      Parent.DoItemRename(Self, Value);
    FName := Value;
  end;
end;

//=== { TJclSimpleXMLElemClassic } ===========================================

procedure TJclSimpleXMLElemClassic.LoadFromStringStream(StringStream: TJclStringStream; AParent: TJclSimpleXML);
//<element Prop="foo" Prop='bar'/>
//<element Prop="foo" Prop='bar'>foor<b>beuh</b>bar</element>
//<xml:element Prop="foo" Prop='bar'>foor<b>beuh</b>bar</element>
type
  TReadStatus = (rsWaitingOpeningTag, rsOpeningName, rsTypeOpeningTag, rsEndSingleTag,
    rsWaitingClosingTag1, rsWaitingClosingTag2, rsClosingName);
var
  lPos: TReadStatus;
  St, lName, lValue, lNameSpace: string;
  Ch: Char;
begin
  St := '';
  lValue := '';
  lNameSpace := '';
  lPos := rsWaitingOpeningTag;

  if AParent <> nil then
    AParent.DoLoadProgress(StringStream.Stream.Position, StringStream.Stream.Size);

  while StringStream.ReadChar(Ch) do
  begin
    case lPos of
      rsWaitingOpeningTag: // wait beginning of tag
        if Ch = '<' then
          lPos := rsOpeningName // read name
        else
        if not CharIsWhiteSpace(Ch) then
          FmtError(LoadResString(@RsEInvalidXMLElementExpectedBeginningO), [Ch, StringStream.PeekPosition]);

      rsOpeningName:
        if CharIsValidIdentifierLetter(Ch) or (Ch = '-') or (Ch = '.') then
          St := St + Ch
        else
        if (Ch = ':') and (lNameSpace = '') then
        begin
          lNameSpace := St;
          st := '';
        end
        else
        if CharIsWhiteSpace(Ch) and (St = '') then
          // whitespace after "<" (no name)
          FmtError(LoadResString(@RsEInvalidXMLElementMalformedTagFoundn), [StringStream.PeekPosition])
        else
        if CharIsWhiteSpace(Ch) then
        begin
          lName := St;
          St := '';
          Properties.LoadFromStringStream(StringStream);
          lPos := rsTypeOpeningTag;
        end
        else
        if Ch = '/' then // single tag
        begin
          lName := St;
          lPos := rsEndSingleTag
        end
        else
        if Ch = '>' then // 2 tags
        begin
          lName := St;
          St := '';
          //Load elements
          Items.LoadFromStringStream(StringStream, AParent);
          lPos := rsWaitingClosingTag1;
        end
        else
          // other invalid characters
          FmtError(LoadResString(@RsEInvalidXMLElementMalformedTagFoundn), [StringStream.PeekPosition]);

      rsTypeOpeningTag:
        if CharIsWhiteSpace(Ch) then
          // nothing, spaces after name or properties
        else
        if Ch = '/' then
          lPos := rsEndSingleTag // single tag
        else
        if Ch = '>' then // 2 tags
        begin
          //Load elements
          Items.LoadFromStringStream(StringStream, AParent);
          lPos := rsWaitingClosingTag1;
        end
        else
          FmtError(LoadResString(@RsEInvalidXMLElementExpectedEndOfTagBu), [Ch, StringStream.PeekPosition]);

      rsEndSingleTag:
        if Ch = '>' then
          Break
        else
          FmtError(LoadResString(@RsEInvalidXMLElementExpectedEndOfTagBu), [Ch, StringStream.PeekPosition]);

      rsWaitingClosingTag1:
        if CharIsWhiteSpace(Ch) then
          // nothing, spaces before closing tag
        else
        if Ch = '<' then
          lPos := rsWaitingClosingTag2
        else
          FmtError(LoadResString(@RsEInvalidXMLElementExpectedEndOfTagBu), [Ch, StringStream.PeekPosition]);

      rsWaitingClosingTag2:
        if Ch = '/' then
          lPos := rsClosingName
        else
          FmtError(LoadResString(@RsEInvalidXMLElementExpectedEndOfTagBu), [Ch, StringStream.PeekPosition]);

      rsClosingName:
        if CharIsWhiteSpace(Ch) or (Ch = '>') then
        begin
          if lNameSpace <> '' then
          begin
            if not StrSame(lNameSpace + ':' + lName, St) then
              FmtError(LoadResString(@RsEInvalidXMLElementErroneousEndOfTagE), [lName, St, StringStream.PeekPosition]);
          end
          else
            if not StrSame(lName, St) then
              FmtError(LoadResString(@RsEInvalidXMLElementErroneousEndOfTagE), [lName, St, StringStream.PeekPosition]);
          //Set value if only one sub element
          //This might reduce speed, but this is for compatibility issues
          if (Items.Count = 1) and (Items[0] is TJclSimpleXMLElemText) then
          begin
            lValue := Items[0].Value;
            Items.Clear;
          end;
          Break;
        end
        else
        if CharIsValidIdentifierLetter(Ch) or (Ch = '-') or (Ch = '.') or (Ch = ':') then
          St := St + Ch
        else
          // other invalid characters
          FmtError(LoadResString(@RsEInvalidXMLElementMalformedTagFoundn), [StringStream.PeekPosition]);
    end;
  end;

  Name := lName;
  if GetSimpleXML <> nil then
    GetSimpleXML.DoDecodeValue(lValue);
  Value := lValue;
  NameSpace := lNameSpace;

  if AParent <> nil then
  begin
    AParent.DoTagParsed(lName);
    AParent.DoValueParsed(lName, lValue);
  end;
end;

procedure TJclSimpleXMLElemClassic.SaveToStringStream(StringStream: TJclStringStream; const Level: string; AParent: TJclSimpleXML);
var
  St, AName, tmp: string;
  LevelAdd: string;
begin
  if(NameSpace <> '') then
  begin
    AName := NameSpace + ':' + Name;
  end
  else
  begin
    AName := Name;
  end;

  if Name <> '' then
  begin
    if GetSimpleXML <> nil then
       GetSimpleXML.DoEncodeValue(AName);
    St := Level + '<' + AName;

    StringStream.WriteString(St, 1, Length(St));
    Properties.SaveToStringStream(StringStream);
  end;

  if (Items.Count = 0) then
  begin
    tmp := FValue;
    if (Name <> '') then
    begin
      if Value = '' then
        St := '/>' + sLineBreak
      else
      begin
        if GetSimpleXML <> nil then
          GetSimpleXML.DoEncodeValue(tmp);
        St := '>' + tmp + '</' + AName + '>' + sLineBreak;
      end;
      StringStream.WriteString(St, 1, Length(St));
    end;
  end
  else
  begin
    if (Name <> '') then
    begin
      St := '>' + sLineBreak;
      StringStream.WriteString(St, 1, Length(St));
    end;
    if Assigned(SimpleXML) and
      (sxoAutoIndent in SimpleXML.Options) then
    begin
      LevelAdd := SimpleXML.IndentString;
    end;
    Items.SaveToStringStream(StringStream, Level + LevelAdd, AParent);
    if Name <> '' then
    begin
      St := Level + '</' + AName + '>' + sLineBreak;
      StringStream.WriteString(St, 1, Length(St));
    end;
  end;
  if AParent <> nil then
    AParent.DoSaveProgress;
end;

//=== { TJclSimpleXMLElemComment } ===========================================

procedure TJclSimpleXMLElemComment.LoadFromStringStream(StringStream: TJclStringStream; AParent: TJclSimpleXML);
//<!-- declarations for <head> & <body> -->
const
  CS_START_COMMENT = '<!--';
  CS_STOP_COMMENT  = '    -->';
var
  lPos: Integer;
  St: string;
  Ch: Char;
  lOk: Boolean;
begin
  St := '';
  lPos := 1;
  lOk := False;

  if AParent <> nil then
    AParent.DoLoadProgress(StringStream.Stream.Position, StringStream.Stream.Size);

  while StringStream.ReadChar(Ch) do
  begin
    case lPos of
      1..4: //<!--
        if Ch = CS_START_COMMENT[lPos] then
          Inc(lPos)
        else
        if not CharIsWhiteSpace(Ch) then
          FmtError(LoadResString(@RsEInvalidCommentExpectedsButFounds), [CS_START_COMMENT[lPos], Ch, StringStream.PeekPosition]);
      5:
        if Ch = CS_STOP_COMMENT[lPos] then
          Inc(lPos)
        else
          St := St + Ch;
      6: //-
        if Ch = CS_STOP_COMMENT[lPos] then
          Inc(lPos)
        else
        begin
          St := St + '-' + Ch;
          Dec(lPos);
        end;
      7: //>
        if Ch = CS_STOP_COMMENT[lPos] then
        begin
          lOk := True;
          Break; //End if
        end
        else // -- is not authorized in comments
          FmtError(LoadResString(@RsEInvalidCommentNotAllowedInsideComme), [StringStream.PeekPosition]);
    end;
  end;

  if not lOk then
    FmtError(LoadResString(@RsEInvalidCommentUnexpectedEndOfData), [StringStream.PeekPosition]);

  Value := St;
  Name := '';

  if AParent <> nil then
    AParent.DoValueParsed('', St);
end;

procedure TJclSimpleXMLElemComment.SaveToStringStream(StringStream: TJclStringStream; const Level: string; AParent: TJclSimpleXML);
var
  St: string;
begin
  St := Level + '<!--';
  StringStream.WriteString(St, 1, Length(St));
  if Value <> '' then
    StringStream.WriteString(Value, 1, Length(Value));
  St := '-->' + sLineBreak;
  StringStream.WriteString(St, 1, Length(St));
  if AParent <> nil then
    AParent.DoSaveProgress;
end;

//=== { TJclSimpleXMLElemCData } =============================================

procedure TJclSimpleXMLElemCData.LoadFromStringStream(StringStream: TJclStringStream; AParent: TJclSimpleXML);
//<![CDATA[<greeting>Hello, world!</greeting>]]>
const
  CS_START_CDATA = '<![CDATA[';
  CS_STOP_CDATA  = '         ]]>';
var
  lPos: Integer;
  St: string;
  Ch: Char;
  lOk: Boolean;
begin
  St := '';
  lPos := 1;
  lOk := False;

  if AParent <> nil then
    AParent.DoLoadProgress(StringStream.Stream.Position, StringStream.Stream.Size);

  while StringStream.ReadChar(Ch) do
  begin
    case lPos of
      1..9: //<![CDATA[
        if Ch = CS_START_CDATA[lPos] then
          Inc(lPos)
        else
        if not CharIsWhiteSpace(Ch) then
          FmtError(LoadResString(@RsEInvalidCDATAExpectedsButFounds), [CS_START_CDATA[lPos], Ch, StringStream.PeekPosition]);
      10:
        if Ch = CS_STOP_CDATA[lPos] then
          Inc(lPos)
        else
          St := St + Ch;
      11: //-
        if Ch = CS_STOP_CDATA[lPos] then
          Inc(lPos)
        else
        begin
          St := St + ']' + Ch;
          Dec(lPos);
        end;
      12: //>
        if Ch = CS_STOP_CDATA[lPos] then
        begin
          lOk := True;
          Break; //End if
        end
        else
        begin
          St := St + ']]' + Ch;
          Dec(lPos, 2);
        end;
    end;
  end;

  if not lOk then
    FmtError(LoadResString(@RsEInvalidCDATAUnexpectedEndOfData), [StringStream.PeekPosition]);

  Value := St;
  Name := '';

  if AParent <> nil then
    AParent.DoValueParsed('', St);
end;

procedure TJclSimpleXMLElemCData.SaveToStringStream(StringStream: TJclStringStream; const Level: string; AParent: TJclSimpleXML);
var
  St: string;
begin
  St := Level + '<![CDATA[';
  StringStream.WriteString(St, 1, Length(St));
  if Value <> '' then
    StringStream.WriteString(Value, 1, Length(Value));
  St := ']]>' + sLineBreak;
  StringStream.WriteString(St, 1, Length(St));
  if AParent <> nil then
    AParent.DoSaveProgress;
end;

//=== { TJclSimpleXMLElemText } ==============================================

procedure TJclSimpleXMLElemText.LoadFromStringStream(StringStream: TJclStringStream; AParent: TJclSimpleXML);
var
  Ch: Char;
  St: string;
begin
  St := '';

  if AParent <> nil then
    AParent.DoLoadProgress(StringStream.Stream.Position, StringStream.Stream.Size);

  while StringStream.PeekChar(Ch) do
    case Ch of
      '<':
        //Quit text
        Break;
    else
      begin
        StringStream.ReadChar(Ch);
        St := St + Ch;
      end;
  end;

  if Assigned(SimpleXML) then
  begin
    GetSimpleXML.DoDecodeValue(St);

    if sxoTrimPrecedingTextWhitespace in SimpleXML.Options then
      St := TrimLeft(St);
    if sxoTrimFollowingTextWhitespace in SimpleXML.Options then
      St := TrimRight(St);
  end;

  Value := St;
  Name := '';

  if AParent <> nil then
    AParent.DoValueParsed('', St);
end;

procedure TJclSimpleXMLElemText.SaveToStringStream(StringStream: TJclStringStream; const Level: string; AParent: TJclSimpleXML);
var
  St, tmp: string;
begin
  // should never be used
  if Value <> '' then
  begin
    tmp := Value;
    if GetSimpleXML <> nil then
      GetSimpleXML.DoEncodeValue(tmp);
    St := Level + tmp + sLineBreak;
    StringStream.WriteString(St, 1, Length(St));
  end;
  if AParent <> nil then
    AParent.DoSaveProgress;
end;

//=== { TJclSimpleXMLElemProcessingInstruction } =============================

procedure TJclSimpleXMLElemProcessingInstruction.LoadFromStringStream(
  StringStream: TJclStringStream; AParent: TJclSimpleXML);
type
  TReadStatus = (rsWaitingOpeningTag, rsOpeningTag, rsOpeningName, rsEndTag1, rsEndTag2);
var
  lPos: TReadStatus;
  lOk: Boolean;
  St, lName, lNameSpace: string;
  Ch: Char;
begin
  St := '';
  lNameSpace := '';
  lPos := rsWaitingOpeningTag;
  lOk := False;

  if AParent <> nil then
    AParent.DoLoadProgress(StringStream.Stream.Position, StringStream.Stream.Size);

  while StringStream.ReadChar(Ch) do
  begin
    case lPos of
      rsWaitingOpeningTag: // wait beginning of tag
        if Ch = '<' then
          lPos := rsOpeningTag
        else
        if not CharIsWhiteSpace(Ch) then
          FmtError(LoadResString(@RsEInvalidXMLElementExpectedBeginningO), [Ch, StringStream.PeekPosition]);

      rsOpeningTag:
        if Ch = '?' then
          lPos := rsOpeningName // read name
        else
          FmtError(LoadResString(@RsEInvalidXMLElementMalformedTagFoundn), [StringStream.PeekPosition]);

      rsOpeningName:
        if CharIsValidIdentifierLetter(Ch) or (Ch = '-') or (Ch = '.') then
          St := St + Ch
        else
        if (Ch = ':') and (lNameSpace = '') then
        begin
          lNameSpace := St;
          St := '';
        end
        else
        if CharIsWhiteSpace(Ch) and (St = '') then
          // whitespace after "<" (no name)
          FmtError(LoadResString(@RsEInvalidXMLElementMalformedTagFoundn), [StringStream.PeekPosition])
        else
        if CharIsWhiteSpace(Ch) then
        begin
          lName := St;
          St := '';
          Properties.LoadFromStringStream(StringStream);
          lPos := rsEndTag1;
        end
        else
        if Ch = '?' then
        begin
          lName := St;
          lPos := rsEndTag2;
        end
        else
          // other invalid characters
          FmtError(LoadResString(@RsEInvalidXMLElementMalformedTagFoundn), [StringStream.PeekPosition]);

      rsEndTag1:
        if Ch = '?' then
          lPos := rsEndTag2
        else
        if not CharIsWhiteSpace(Ch) then
          FmtError(LoadResString(@RsEInvalidXMLElementExpectedEndOfTagBu), [Ch, StringStream.PeekPosition]);

      rsEndTag2:
        if Ch = '>' then
        begin
          lOk := True;
          Break;
        end
        else
          FmtError(LoadResString(@RsEInvalidXMLElementExpectedEndOfTagBu), [Ch, StringStream.PeekPosition]);
    end;
  end;

  if not lOk then
    FmtError(LoadResString(@RsEInvalidCommentUnexpectedEndOfData), [StringStream.PeekPosition]);

  Name := lName;
  NameSpace := lNameSpace;
end;

procedure TJclSimpleXMLElemProcessingInstruction.SaveToStringStream(
  StringStream: TJclStringStream; const Level: string; AParent: TJclSimpleXML);
var
  St: string;
begin
  St := Level + '<?';
  if NameSpace <> '' then
    St := St + NameSpace + ':' + Name
  else
    St := St + Name;
  StringStream.WriteString(St, 1, Length(St));
  Properties.SaveToStringStream(StringStream);
  St := '?>' + sLineBreak;
  StringStream.WriteString(St, 1, Length(St));
  if AParent <> nil then
    AParent.DoSaveProgress;
end;

//=== { TJclSimpleXMLElemHeader } ============================================

function TJclSimpleXMLElemHeader.GetEncoding: string;
begin
  Result := Properties.Value('encoding', 'iso-8859-1');
end;

function TJclSimpleXMLElemHeader.GetStandalone: Boolean;
begin
  Result := Properties.Value('standalone') = 'yes';
end;

function TJclSimpleXMLElemHeader.GetVersion: string;
begin
  Result := Properties.Value('version', '1.0');
end;

procedure TJclSimpleXMLElemHeader.LoadFromStringStream(StringStream: TJclStringStream; AParent: TJclSimpleXML);
//<?xml version="1.0" encoding="iso-xyzxx" standalone="yes"?>
var
  CodePage: Word;
  EncodingProp: TJclSimpleXMLProp;
begin
  inherited LoadFromStringStream(StringStream, AParent);
  
  EncodingProp := Properties.ItemNamed['encoding'];
  if Assigned(EncodingProp) and (EncodingProp.Value <> '') then
    CodePage := CodePageFromCharsetName(EncodingProp.Value)
  else
    CodePage := CP_ACP;

  // set current stringstream codepage
  if StringStream is TJclAutoStream then
    TJclAutoStream(StringStream).CodePage := CodePage
  else
  if StringStream is TJclAnsiStream then
    TJclAnsiStream(StringStream).CodePage := CodePage
  else
  if not (StringStream is TJclUTF8Stream) and not (StringStream is TJclUTF16Stream) then
    Error(LoadResString(@RsENoCharset));
end;

procedure TJclSimpleXMLElemHeader.SaveToStringStream(
  StringStream: TJclStringStream; const Level: string; AParent: TJclSimpleXML);
begin
  SetVersion(GetVersion);
  SetEncoding(GetEncoding);
  SetStandalone(GetStandalone);
  inherited SaveToStringStream(StringStream, Level, AParent);
end;

procedure TJclSimpleXMLElemHeader.SetEncoding(const Value: string);
var
  Prop: TJclSimpleXMLProp;
begin
  Prop := Properties.GetItemNamed('encoding');
  if Assigned(Prop) then
    Prop.Value := Value
  else
    Properties.Add('encoding', Value);
end;

procedure TJclSimpleXMLElemHeader.SetStandalone(const Value: Boolean);
var
  Prop: TJclSimpleXMLProp;
const
  BooleanValues: array [Boolean] of string = ('no', 'yes');
begin
  Prop := Properties.GetItemNamed('standalone');
  if Assigned(Prop) then
    Prop.Value := BooleanValues[Value]
  else
    Properties.Add('standalone', BooleanValues[Value]);
end;

procedure TJclSimpleXMLElemHeader.SetVersion(const Value: string);
var
  Prop: TJclSimpleXMLProp;
begin
  Prop := Properties.GetItemNamed('version');
  if Assigned(Prop) then
    Prop.Value := Value
  else
    Properties.Add('version', Value);
end;

//=== { TJclSimpleXMLElemDocType } ===========================================

procedure TJclSimpleXMLElemDocType.LoadFromStringStream(StringStream: TJclStringStream; AParent: TJclSimpleXML);
{
<!DOCTYPE test [
<!ELEMENT test (#PCDATA) >
<!ENTITY % xx '&#37;zz;'>
<!ENTITY % zz '&#60;!ENTITY tricky "error-prone" >' >
%xx;
]>

<!DOCTYPE greeting SYSTEM "hello.dtd">
}
const
  CS_START_DOCTYPE = '<!DOCTYPE';
var
  lPos: Integer;
  lOk: Boolean;
  Ch, lChar: Char;
  St: string;
begin
  lPos := 1;
  lOk := False;
  lChar := '>';
  St := '';

  if AParent <> nil then
    AParent.DoLoadProgress(StringStream.Stream.Position, StringStream.Stream.Size);

  while StringStream.ReadChar(Ch) do
  begin
    case lPos of
      1..9: //<!DOCTYPE
        if Ch = CS_START_DOCTYPE[lPos] then
          Inc(lPos)
        else
        if not CharIsWhiteSpace(Ch) then
          FmtError(LoadResString(@RsEInvalidHeaderExpectedsButFounds), [CS_START_DOCTYPE[lPos], Ch, StringStream.PeekPosition]);
      10: //]> or >
        if lChar = Ch then
        begin
          if lChar = '>' then
          begin
            lOk := True;
            Break; //This is the end
          end
          else
          begin
            St := St + Ch;
            lChar := '>';
          end;
        end
        else
        begin
          St := St + Ch;
          if Ch = '[' then
            lChar := ']';
        end;
    end;
  end;

  if not lOk then
    FmtError(LoadResString(@RsEInvalidCommentUnexpectedEndOfData), [StringStream.PeekPosition]);

  Name := '';
  Value := StrTrimCharsLeft(St, CharIsWhiteSpace);

  if AParent <> nil then
    AParent.DoValueParsed('', St);
end;

procedure TJclSimpleXMLElemDocType.SaveToStringStream(StringStream: TJclStringStream;
  const Level: string; AParent: TJclSimpleXML);
var
  St: string;
begin
  St := Level + '<!DOCTYPE ' + Value + '>' + sLineBreak;
  StringStream.WriteString(St, 1, Length(St));
  if AParent <> nil then
    AParent.DoSaveProgress;
end;

//=== { TJclSimpleXMLElemsProlog } ===========================================

constructor TJclSimpleXMLElemsProlog.Create;
begin
  inherited Create;
  FElems := THashedStringList.Create;
end;

destructor TJclSimpleXMLElemsProlog.Destroy;
begin
  Clear;
  FreeAndNil(FElems);
  inherited Destroy;
end;

procedure TJclSimpleXMLElemsProlog.Clear;
var
  I: Integer;
begin
  for I := 0 to FElems.Count - 1 do
  begin
    FElems.Objects[I].Free;
    FElems.Objects[I] := nil;
  end;
  FElems.Clear;
end;

function TJclSimpleXMLElemsProlog.GetCount: Integer;
begin
  Result := FElems.Count;
end;

function TJclSimpleXMLElemsProlog.GetItem(const Index: Integer): TJclSimpleXMLElem;
begin
  Result := TJclSimpleXMLElem(FElems.Objects[Index]);
end;

procedure TJclSimpleXMLElemsProlog.LoadFromStringStream(StringStream: TJclStringStream; AParent: TJclSimpleXML);
{<?xml version="1.0" encoding="UTF-8" ?>
<!-- Test -->
<!DOCTYPE greeting [
  <!ELEMENT greeting (#PCDATA)>
]>
<greeting>Hello, world!</greeting>

<?xml version="1.0"?> <!DOCTYPE greeting SYSTEM "hello.dtd"> <greeting>Hello, world!</greeting>
}
var
  lPos: Integer;
  St: string;
  lEnd: Boolean;
  lElem: TJclSimpleXMLElem;
  Ch: Char;
begin
  St := '';
  lPos := 0;

  if AParent <> nil then
    AParent.DoLoadProgress(StringStream.Stream.Position, StringStream.Stream.Size);

  while StringStream.PeekChar(Ch) do
  begin
    case lPos of
      0: //We are waiting for a tag and thus avoiding spaces and any BOM
        begin
          if CharIsWhiteSpace(Ch) then
            // still waiting
          else
          if Ch = '<' then
          begin
            lPos := 1;
            St := Ch;
          end
          else
            FmtError(LoadResString(@RsEInvalidDocumentUnexpectedTextInFile), [StringStream.PeekPosition]);
        end;
      1: //We are trying to determine the kind of the tag
        begin
          lElem := nil;
          lEnd := False;

          if (St <> '<![CDATA') or not CharIsWhiteSpace(Ch) then
            St := St + Ch;
          if St = '<![CDATA[' then
            lEnd := True
          else
          if St = '<!--' then
            lElem := TJclSimpleXMLElemComment.Create(nil)
          else
          if St = '<?xml-stylesheet' then
            lElem := TJclSimpleXMLElemSheet.Create(nil)
          else
          if St = '<?xml ' then
            lElem := TJclSimpleXMLElemHeader.Create(nil)
          else
          if St = '<!DOCTYPE' then
            lElem := TJclSimpleXMLElemDocType.Create(nil)
          else
          if St = '<?mso-application' then
            lElem := TJclSimpleXMLElemMSOApplication.Create(nil)
          else
          if (Length(St) > 3) and (St[2] = '?') and CharIsWhiteSpace(St[Length(St)]) then
            lElem := TJclSimpleXMLElemProcessingInstruction.Create(nil)
          else
          if (Length(St) > 1) and (St[2] <> '!') and (St[2] <> '?') then
            lEnd := True;

          if lEnd then
            Break
          else
          if lElem <> nil then
          begin
            lElem.LoadFromStringStream(StringStream, AParent);
            FElems.AddObject(lElem.Name, lElem);
            St := '';
            lPos := 0;
          end;
        end;
    end;
  end;
end;

procedure TJclSimpleXMLElemsProlog.SaveToStringStream(StringStream: TJclStringStream; AParent: TJclSimpleXML);
var
  I: Integer;
begin
  FindHeader;
  for I := 0 to Count - 1 do
    Item[I].SaveToStringStream(StringStream, '', AParent);
end;

function VarXML: TVarType;
begin
  Result := XMLVariant.VarType;
end;

procedure XMLCreateInto(var ADest: Variant; const AXML: TJclSimpleXMLElem);
begin
  TVarData(ADest).vType := VarXML;
  TVarData(ADest).vAny := AXML;
end;

function XMLCreate(const AXML: TJclSimpleXMLElem): Variant;
begin
  XMLCreateInto(Result, AXML);
end;

function XMLCreate: Variant;
begin
  XMLCreateInto(Result, TJclSimpleXMLElemClassic.Create(nil));
end;

//=== { TXMLVariant } ========================================================

procedure TXMLVariant.CastTo(var Dest: TVarData; const Source: TVarData;
  const AVarType: TVarType);
var
  StorageStream: TStringStream;
  ConversionString: TJclStringStream;
begin
  if Source.vType = VarType then
  begin
    case AVarType of
      varOleStr:
        begin
          StorageStream := TStringStream.Create('');
          try
            ConversionString := TJclUTF16Stream.Create(StorageStream, False);
            try
              ConversionString.WriteBOM;
              TJclSimpleXmlElem(Source.vAny).SaveToStringStream(ConversionString, '', nil);
              ConversionString.Flush;
            finally
              ConversionString.Free;
            end;
            VarDataFromOleStr(Dest, StorageStream.DataString);
          finally
            StorageStream.Free;
          end;
        end;
      varString:
        begin
          StorageStream := TStringStream.Create('');
          try
            {$IFDEF SUPPORTS_UNICODE}
            ConversionString := TJclUTF16Stream.Create(StorageStream, False);
            {$ELSE ~SUPPORTS_UNICODE}
            ConversionString := TJclAnsiStream.Create(StorageStream, False);
            {$ENDIF ~SUPPORTS_UNICODE}
            try
              ConversionString.WriteBOM;
              TJclSimpleXmlElem(Source.vAny).SaveToStringStream(ConversionString, '', nil);
              ConversionString.Flush;
            finally
              ConversionString.Free;
            end;
            VarDataFromStr(Dest, StorageStream.DataString);
          finally
            StorageStream.Free;
          end;
        end;
      {$IFDEF SUPPORTS_UNICODE_STRING}
      varUString:
        begin
          StorageStream := TStringStream.Create('');
          try
            ConversionString := TJclUTF16Stream.Create(StorageStream, False);
            try
              ConversionString.WriteBOM;
              TJclSimpleXmlElem(Source.vAny).SaveToStringStream(ConversionString, '', nil);
              ConversionString.Flush;
            finally
              ConversionString.Free;
            end;
            VarDataClear(Dest);
            Dest.VUString := nil;
            Dest.VType := varUString;
            UnicodeString(Dest.VUString) := UnicodeString(StorageStream.DataString);
          finally
            StorageStream.Free;
          end;
        end;
      {$ENDIF SUPPORTS_UNICODE_STRING}
    else
      RaiseCastError;
    end;
  end
  else
    inherited CastTo(Dest, Source, AVarType);
end;

procedure TXMLVariant.Clear(var V: TVarData);
begin
  V.vType := varEmpty;
  V.vAny := nil;
end;

procedure TXMLVariant.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
  begin
    Dest.vType := Source.vType;
    Dest.vAny := Source.vAny;
  end;
end;

function TXMLVariant.DoFunction(var Dest: TVarData; const V: TVarData;
  const Name: string; const Arguments: TVarDataArray): Boolean;
var
  VXML, LXML: TJclSimpleXMLElem;
  I, J, K: Integer;
begin
  Result := False;
  if (Length(Arguments) = 1) and (Arguments[0].vType in [vtInteger, vtExtended]) then
  begin
    VXML := TJclSimpleXmlElem(V.VAny);
    K := Arguments[0].vInteger;
    J := 0;

    if K > 0 then
      for I := 0 to VXML.Items.Count - 1 do
        if UpperCase(VXML.Items[I].Name) = Name then
        begin
          Inc(J);
          if J = K then
            Break;
        end;

    if (J = K) and (J < VXML.Items.Count) then
    begin
      LXML := VXML.Items[J];
      if LXML <> nil then
      begin
        Dest.vType := VarXML;
        Dest.vAny := Pointer(LXML);
        Result := True;
      end
    end;
  end
end;

function TXMLVariant.GetProperty(var Dest: TVarData; const V: TVarData;
  const Name: string): Boolean;
var
  VXML, LXML: TJclSimpleXMLElem;
  lProp: TJclSimpleXMLProp;
begin
  Result := False;
  VXML := TJclSimpleXMLElem(V.VAny);
  LXML := VXML.Items.ItemNamed[Name];
  if LXML <> nil then
  begin
    Dest.vType := VarXML;
    Dest.vAny := Pointer(LXML);
    Result := True;
  end
  else
  begin
    lProp := VXML.Properties.ItemNamed[Name];
    if lProp <> nil then
    begin
      VarDataFromOleStr(Dest, lProp.Value);
      Result := True;
    end;
  end;
end;

function TXMLVariant.IsClear(const V: TVarData): Boolean;
var
  VXML: TJclSimpleXMLElem;
begin
  VXML := TJclSimpleXMLElem(V.VAny);
  Result := (VXML = nil) or (VXML.Items.Count = 0);
end;

function TXMLVariant.SetProperty(const V: TVarData; const Name: string;
  const Value: TVarData): Boolean;

  function GetStrValue: string;
  begin
    try
      Result := Value.VOleStr;
    except
      Result := '';
    end;
  end;

var
  VXML, LXML: TJclSimpleXMLElem;
  lProp: TJclSimpleXMLProp;
begin
  Result := False;
  VXML := TJclSimpleXmlElem(V.VAny);
  LXML := VXML.Items.ItemNamed[Name];
  if LXML = nil then
  begin
    lProp := VXML.Properties.ItemNamed[Name];
    if lProp <> nil then
    begin
      lProp.Value := GetStrValue;
      Result := True;
    end;
  end
  else
  begin
    LXML.Value := GetStrValue;
    Result := True;
  end;
end;

procedure TJclSimpleXMLElemsProlog.Error(const S: string);
begin
  raise EJclSimpleXMLError.Create(S);
end;

procedure TJclSimpleXMLElemsProlog.FmtError(const S: string;
  const Args: array of const);
begin
  Error(Format(S, Args));
end;

procedure TJclSimpleXML.SetIndentString(const Value: string);
begin
  // test if the new value is only made of spaces or tabs
  if not StrContainsChars(Value, CharIsWhiteSpace, True) then
    Exit;
  FIndentString := Value;
end;

procedure TJclSimpleXML.SetRoot(const Value: TJclSimpleXMLElemClassic);
begin
  if Value <> FRoot then
  begin
//    FRoot.FSimpleXML := nil;
    FRoot := Value;
//    FRoot.FSimpleXML := Self;
  end;
end;

function TJclSimpleXMLElemsProlog.GetEncoding: string;
var
  Elem: TJclSimpleXMLElemHeader;
begin
  Elem := TJclSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Result := Elem.Encoding
  else
    Result := 'UTF-8';
end;

function TJclSimpleXMLElemsProlog.GetStandAlone: Boolean;
var
  Elem: TJclSimpleXMLElemHeader;
begin
  Elem := TJclSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Result := Elem.StandAlone
  else
    Result := False;
end;

function TJclSimpleXMLElemsProlog.GetVersion: string;
var
  Elem: TJclSimpleXMLElemHeader;
begin
  Elem := TJclSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Result := Elem.Version
  else
    Result := '1.0';
end;

procedure TJclSimpleXMLElemsProlog.SetEncoding(const Value: string);
var
  Elem: TJclSimpleXMLElemHeader;
begin
  Elem := TJclSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Elem.Encoding := Value;
end;

procedure TJclSimpleXMLElemsProlog.SetStandAlone(const Value: Boolean);
var
  Elem: TJclSimpleXMLElemHeader;
begin
  Elem := TJclSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Elem.StandAlone := Value;
end;

procedure TJclSimpleXMLElemsProlog.SetVersion(const Value: string);
var
  Elem: TJclSimpleXMLElemHeader;
begin
  Elem := TJclSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Elem.Version := Value;
end;

function TJclSimpleXMLElemsProlog.FindHeader: TJclSimpleXMLElem;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Item[I] is TJclSimpleXMLElemHeader then
    begin
      Result := Item[I];
      Exit;
    end;
  // (p3) if we get here, an xml header was not found
  Result := TJclSimpleXMLElemHeader.Create(nil);
  Result.Name := 'xml';
  FElems.AddObject('', Result);
end;

function TJclSimpleXMLElemsProlog.AddStyleSheet(const AType, AHRef: string): TJclSimpleXMLElemSheet;
begin
  // make sure there is an xml header
  FindHeader;
  Result := TJclSimpleXMLElemSheet.Create(nil);
  Result.Name := 'xml-stylesheet';
  Result.Properties.Add('type',AType);
  Result.Properties.Add('href',AHRef);
  FElems.AddObject('xml-stylesheet', Result);
end;

function TJclSimpleXMLElemsProlog.AddMSOApplication(const AProgId : string): TJclSimpleXMLElemMSOApplication;
begin
  // make sure there is an xml header
  FindHeader;
  Result := TJclSimpleXMLElemMSOApplication.Create(nil);
  Result.Name := 'mso-application';
  Result.Properties.Add('progid',AProgId);
  FElems.AddObject('mso-application', Result);
end;

function TJclSimpleXMLElemsProlog.AddComment(const AValue: string): TJclSimpleXMLElemComment;
begin
  // make sure there is an xml header
  FindHeader;
  Result := TJclSimpleXMLElemComment.Create(nil);
  Result.Value := AValue;
  FElems.AddObject('', Result);
end;

function TJclSimpleXMLElemsProlog.AddDocType(const AValue: string): TJclSimpleXMLElemDocType;
begin
  // make sure there is an xml header
  FindHeader;
  Result := TJclSimpleXMLElemDocType.Create(nil);
  Result.Value := AValue;
  FElems.AddObject('', Result);
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  FreeAndNil(GlobalXMLVariant);
  FreeAndNil(GlobalSorts);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
