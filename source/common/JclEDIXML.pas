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
{ The Original Code is JclEDIXML.pas.                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ A complementary unit to JclEDI.pas.                                                              }
{                                                                                                  }
{ Unit owner: Raymond Alexander                                                                    }
{ Date created: March 6, 2003                                                                      }
{ Last modified: March 25, 2003                                                                    }
{ Additional Info:                                                                                 }
{   E-Mail at RaysDelphiBox3@hotmail.com                                                           }
{   For latest EDI specific updates see http://sourceforge.net/projects/edisdk                     }
{   See home page for latest news & events and online help.                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ 04/21/2003 (R.A.)                                                                                }
{                                                                                                  }
{   The current status of this unit is experimental.                                               }
{                                                                                                  }
{   Release notes have been moved to ReleaseNotes.rtf                                              }
{                                                                                                  }
{**************************************************************************************************}

unit JclEDIXML;

{$I jcl.inc}

{$I jedi.inc}

{$WEAKPACKAGEUNIT ON}

interface

uses
  SysUtils, Classes, JclBase, JclStrings, JclEDI, JclEDI_ANSIX12;

resourcestring
  EDIXMLError001 = 'Could not open edi file.  File not specified.';
  EDIXMLError002 = 'Could not save edi file.  File name and path not specified.';
  EDIXMLError003 = 'Could not assign delimiters to edi file.  Disassemble cancelled.';
  EDIXMLError004 = 'Could not assign delimiters to edi file.  Assemble cancelled.';
  EDIXMLError005 = 'Could not assign delimiters to interchange control.  Disassemble cancelled.';
  EDIXMLError006 = 'Could not assign delimiters to interchange control.  Assemble cancelled.';
  EDIXMLError007 = 'Could not find interchange control end tag.';
  EDIXMLError008 = 'Could not find interchange control end tag delimiter.';
  EDIXMLError009 = 'Could not find interchange control header.';
  EDIXMLError010 = 'Could not find interchange control header end tag.';
  EDIXMLError011 = 'Could not find interchange control header end tag delimiter.';
  EDIXMLError012 = 'Could not find interchange control trailer.';
  EDIXMLError013 = 'Could not find interchange control trailer end tag.';
  EDIXMLError014 = 'Could not find interchange control trailer end tag delimiter.';
  EDIXMLError015 = 'Could not assign delimiters to functional group.  Disassemble cancelled.';
  EDIXMLError016 = 'Could not assign delimiters to functional group.  Assemble cancelled.';
  EDIXMLError017 = 'Could not find functional group end tag.';
  EDIXMLError018 = 'Could not find functional group end tag delimiter.';
  EDIXMLError019 = 'Could not find functional group header.';
  EDIXMLError020 = 'Could not find functional group header end tag.';
  EDIXMLError021 = 'Could not find functional group header end tag delimiter.';
  EDIXMLError022 = 'Could not find functional group trailer.';
  EDIXMLError023 = 'Could not find functional group trailer end tag.';
  EDIXMLError024 = 'Could not find functional group trailer end tag delimiter.';
  EDIXMLError025 = 'Could not assign delimiters to transactoin set.  Disassemble cancelled.';
  EDIXMLError026 = 'Could not assign delimiters to transactoin set.  Assemble cancelled.';
  EDIXMLError027 = 'Could not find transaction set end tag.';
  EDIXMLError028 = 'Could not find transaction set end tag delimiter.';
  EDIXMLError029 = 'Could not assign delimiters to transactoin set loop.  Disassemble cancelled.';
  EDIXMLError030 = 'Could not assign delimiters to transactoin set loop.  Assemble cancelled.';
  EDIXMLError031 = 'Could not find loop end tag';
  EDIXMLError032 = 'Could not find loop end tag delimiter';
  EDIXMLError033 = 'Could not set data object at index [%s].';
  EDIXMLError034 = 'Could not set data object at index [%s], Index too low.';
  EDIXMLError035 = 'Could not set data object at index [%s], Index too high.';
  EDIXMLError036 = 'Could not get data object at index [%s], There was no data object to get.';
  EDIXMLError037 = 'Could not get data object at index [%s], Index too low.';
  EDIXMLError038 = 'Could not get data object at index [%s], Index too high.';
  EDIXMLError039 = 'Could not get data object at index [%s], Data object does not exist.';
  EDIXMLError040 = 'Could not delete EDI data object';
  EDIXMLError041 = 'Could not assign delimiters to segment.  Disassemble cancelled.';
  EDIXMLError042 = 'Could not assign delimiters to segment.  Assemble cancelled.';
  EDIXMLError043 = 'Could not find segment begin tag';
  EDIXMLError044 = 'Could not find segment end tag';
  EDIXMLError045 = 'Could not find segment end tag delimiter';
  EDIXMLError046 = 'Could not assign delimiters to element.  Disassemble cancelled.';
  EDIXMLError047 = 'Could not assign delimiters to element.  Assemble cancelled.';
  EDIXMLError048 = 'Could not find element tag';
  EDIXMLError049 = 'Could not find element end tag';
  EDIXMLError050 = 'Could not find element end tag delimiter';
  EDIXMLError051 = 'Could not set element at index [%s].';
  EDIXMLError052 = 'Could not set element at index [%s], Index too low.';
  EDIXMLError053 = 'Could not set element at index [%s], Index too high.';
  EDIXMLError054 = 'Could not get element at index [%s], There was no element to get.';
  EDIXMLError055 = 'Could not get element at index [%s], Index too low.';
  EDIXMLError056 = 'Could not get element at index [%s], Index too high.';
  EDIXMLError057 = 'Could not get element at index [%s], Element does not exist.';
  EDIXMLError058 = 'Could not delete element at index [%s].';
  EDIXMLError059 = 'Could not find transaction set header.';
  EDIXMLError060 = 'Could not find transaction set trailer.';
  EDIXMLError061 = 'Could not find transaction set header and trailer.';
  EDIXMLError062 = 'TEDIXMLANSIX12FormatTranslator: Unexpected object [%s] found.';

const
  XMLTag_Element = 'Element';
  XMLTag_Segment = 'Segment';
  XMLTag_TransactionSetLoop = 'Loop';
  XMLTag_TransactionSet = 'TransactionSet';
  XMLTag_FunctionalGroup = 'FunctionalGroup';
  XMLTag_InterchangeControl = 'InterchangeControl';
  XMLTag_EDIFile = 'EDIFile';
  XMLTag_ICHSegmentId = ICHSegmentId; //Interchange Control Header Segment Id
  XMLTag_ICTSegmentId = ICTSegmentId; //Interchange Control Trailer Segment Id
  XMLTag_FGHSegmentId = FGHSegmentId; //Functional Group Header Segment Id
  XMLTag_FGTSegmentId = FGTSegmentId; //Functional Group Trailer Segment Id
  XMLTag_TSHSegmentId = TSHSegmentId; //Transaction Set Header Segment Id
  XMLTag_TSTSegmentId = TSTSegmentId; //Transaction Set Trailer Segment Id
  XMLAttribute_Id = 'Id';
  XMLAttribute_Position = 'Position';
  XMLAttribute_Description = 'Description';
  XMLAttribute_RequirementDesignator = 'RequirementDesignator';
  XMLAttribute_Type = 'Type';
  XMLAttribute_MinimumLength = 'MinimumLength';
  XMLAttribute_MaximumLength = 'MaximumLength';
  XMLAttribute_Section = 'Section';
  XMLAttribute_MaximumUsage = 'MaximumUsage';
  XMLAttribute_OwnerLoopId = 'OwnerLoopId';
  XMLAttribute_ParentLoopId = 'ParentLoopId';

type

//--------------------------------------------------------------------------------------------------
//  EDI Forward Class Declarations
//--------------------------------------------------------------------------------------------------

  TEDIXMLObject = class(TEDIObject);
  TEDIXMLDataObject = class;
  TEDIXMLElement = class;
  TEDIXMLSegment = class;
  TEDIXMLTransactionSet = class;
  TEDIXMLFunctionalGroup = class;
  TEDIXMLInterchangeControl = class;
  TEDIXMLFile = class;

//--------------------------------------------------------------------------------------------------
//  EDI Delimiters Object
//--------------------------------------------------------------------------------------------------

  TEDIXMLDelimiters = class(TEDIXMLObject)
  private
    FBeginTagDelimiter: string;
    FEndTagDelimiter: string;
    FBeginTagLength: Integer;
    FEndTagLength: Integer;
    FBeginCDataDelimiter: string;
    FEndCDataDelimiter: string;
    FBeginCDataLength: Integer;
    FEndCDataLength: Integer;
    FBeginOfEndTagDelimiter: string;
    FBeginOfEndTagLength: Integer;
    //Special Delimiters for Attributes
    FSpaceDelimiter: string;
    FAssignmentDelimiter: string;
    FSingleQuote: string;
    FDoubleQuote: string;
    procedure SetBeginTagDelimiter(const Value: string);
    procedure SetEndTagDelimiter(const Value: string);
    procedure SetBeginCDataDelimiter(const Value: string);
    procedure SetEndCDataDelimiter(const Value: string);
    procedure SetBeginOfEndTagDelimiter(const Value: string);
  public
    constructor Create;
  published
    property BTD: string read FBeginTagDelimiter write SetBeginTagDelimiter;
    property ETD: string read FEndTagDelimiter write SetEndTagDelimiter;
    property BTDLength: Integer read FBeginTagLength;
    property ETDLength: Integer read FEndTagLength;
    property BOfETD: string read FBeginOfEndTagDelimiter write SetBeginOfEndTagDelimiter;
    property BOfETDLength: Integer read FBeginOfEndTagLength;
    property BCDataD: string read FBeginCDataDelimiter write SetBeginCDataDelimiter;
    property ECDataD: string read FEndCDataDelimiter write SetEndCDataDelimiter;
    property BCDataLength: Integer read FBeginCDataLength;
    property ECDataLength: Integer read FEndCDataLength;
    //Special Delimiters for Attributes
    property SpaceDelimiter: string read FSpaceDelimiter write FSpaceDelimiter;
    property AssignmentDelimiter: string read FAssignmentDelimiter write FAssignmentDelimiter;
    property SingleQuote: string read FSingleQuote write FSingleQuote;
    property DoubleQuote: string read FDoubleQuote write FDoubleQuote;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI XML Attributes
//--------------------------------------------------------------------------------------------------

  TEDIXMLAttributes = class(TEDIXMLObject)
  private
    FAttributes: TStrings;
    FDelimiters: TEDIXMLDelimiters;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseAttributes(XMLStartTag: string);
    function CombineAttributes: string;
    procedure SetAttribute(Name, Value: string);
    function CheckAttribute(Name, Value: string): Integer;
    function GetAttributeValue(Name: string): string;
    function GetAttributeString(Name: string): string;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Data Object
//--------------------------------------------------------------------------------------------------

  TEDIXMLObjectArray = array of TEDIXMLObject;

  TEDIXMLDataObjectArray = array of TEDIXMLDataObject;

  TEDIXMLDataObject = class(TEDIXMLObject)
  private
    procedure SetDelimiters(const Delimiters: TEDIXMLDelimiters);
  protected
    FEDIDOT: TEDIDataObjectType;
    FState: TEDIDataObjectDataState;
    FData: string;
    FLength: Integer;
    FParent: TEDIXMLDataObject;
    FDelimiters: TEDIXMLDelimiters;
    FAttributes: TEDIXMLAttributes;
    FErrorLog: TStrings;
    FSpecPointer: Pointer;
    FCustomData1: Pointer;
    FCustomData2: Pointer;
    function GetData: string;
    procedure SetData(const Data: string);
    function Assemble: string; virtual; abstract;
    procedure Disassemble; virtual; abstract;
  public
    constructor Create(Parent: TEDIXMLDataObject); reintroduce;
    destructor Destroy; override;
    property SpecPointer: Pointer read FSpecPointer write FSpecPointer;
    property CustomData1: Pointer read FCustomData1 write FCustomData1;
    property CustomData2: Pointer read FCustomData2 write FCustomData2;
  published
    property State: TEDIDataObjectDataState read FState;
    property Data: string read GetData write SetData;
    property DataLength: Integer read FLength;
    property Parent: TEDIXMLDataObject read FParent write FParent;
    property Delimiters: TEDIXMLDelimiters read FDelimiters write SetDelimiters;
    property Attributes: TEDIXMLAttributes read FAttributes write FAttributes;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Element
//--------------------------------------------------------------------------------------------------

  TEDIXMLElementArray = array of TEDIXMLElement;

  TEDIXMLElement = class(TEDIXMLDataObject)
    FCData: Boolean;
  public
    constructor Create(Parent: TEDIXMLDataObject); reintroduce;
    function InternalAssignDelimiters: TEDIXMLDelimiters; virtual;
    function Assemble: string; override;
    procedure Disassemble; override;
    function GetIndexPositionFromParent: Integer;
  published
    property CData: Boolean read FCData write FCData;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Data Object Group
//--------------------------------------------------------------------------------------------------

  TEDIXMLDataObjectGroup = class(TEDIXMLDataObject)
  protected
    FEDIDataObjects: TEDIXMLDataObjectArray;
    function GetEDIDataObject(Index: Integer): TEDIXMLDataObject;
    procedure SetEDIDataObject(Index: Integer; EDIDataObject: TEDIXMLDataObject);
    function InternalAssignDelimiters: TEDIXMLDelimiters; virtual; abstract;
    function InternalCreateDataObjectGroup: TEDIXMLDataObjectGroup; virtual; abstract;
    function SearchForSegmentInDataString(Id: string; StartPos: Integer): Integer;
  public
    constructor Create(Parent: TEDIXMLDataObject); reintroduce;
    destructor Destroy; override;
    //
    //  ToDo:  More procedures and functions to manage internal structures
    //
    function AppendEDIDataObject(EDIDataObject: TEDIXMLDataObject): Integer;
    function InsertEDIDataObject(InsertIndex: Integer; EDIDataObject: TEDIXMLDataObject): Integer;
    procedure DeleteEDIDataObject(Index: Integer); overload;
    procedure DeleteEDIDataObject(EDIDataObject: TEDIXMLDataObject); overload;
    //
    function AddSegment: Integer;
    function InsertSegment(InsertIndex: Integer): Integer;
    //
    function AddGroup: Integer; virtual;
    function InsertGroup(InsertIndex: Integer): Integer; virtual;
    //
    procedure DeleteEDIDataObjects;
    property EDIDataObject[Index: Integer]: TEDIXMLDataObject read GetEDIDataObject
      write SetEDIDataObject; default;
    property EDIDataObjects: TEDIXMLDataObjectArray read FEDIDataObjects write FEDIDataObjects;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Segment Classes
//--------------------------------------------------------------------------------------------------

  TEDIXMLSegmentArray = array of TEDIXMLSegment;

  TEDIXMLSegment = class(TEDIXMLDataObject)
  private
    FSegmentID: string;
    FElements: TEDIXMLElementArray;
    function GetElement(Index: Integer): TEDIXMLElement;
    procedure SetElement(Index: Integer; Element: TEDIXMLElement);
  public
    constructor Create(Parent: TEDIXMLDataObject); reintroduce; overload;
    constructor Create(Parent: TEDIXMLDataObject; ElementCount: Integer); reintroduce; overload;
    destructor Destroy; override;
    //
    function InternalAssignDelimiters: TEDIXMLDelimiters; virtual;
    function InternalCreateElement: TEDIXMLElement; virtual;
    //
    function AddElement: Integer;
    function AppendElement(Element: TEDIXMLElement): Integer;
    function InsertElement(InsertIndex: Integer): Integer; overload;
    function InsertElement(InsertIndex: Integer; Element: TEDIXMLElement): Integer; overload;
    procedure DeleteElement(Index: Integer); overload;
    procedure DeleteElement(Element: TEDIXMLElement); overload;
    //
    function AddElements(Count: Integer): Integer;
    function AppendElements(ElementArray: TEDIXMLElementArray): Integer;
    function InsertElements(InsertIndex, Count: Integer): Integer; overload;
    function InsertElements(InsertIndex: Integer;
      ElementArray: TEDIXMLElementArray): Integer; overload;
    procedure DeleteElements; overload;
    procedure DeleteElements(Index, Count: Integer); overload;
    //
    function Assemble: string; override;
    procedure Disassemble; override;
    function GetIndexPositionFromParent: Integer;
    property Element[Index: Integer]: TEDIXMLElement read GetElement write SetElement; default;
    property Elements: TEDIXMLElementArray read FElements write FElements;
  published
    property SegmentID: string read FSegmentID write FSegmentID;
  end;

  TEDIXMLTransactionSetSegment = class(TEDIXMLSegment)
  public
    constructor Create(Parent: TEDIXMLDataObject); reintroduce; overload;
    constructor Create(Parent: TEDIXMLDataObject; ElementCount: Integer); reintroduce; overload;
    function InternalAssignDelimiters: TEDIXMLDelimiters; override;
  end;

  TEDIXMLFunctionalGroupSegment = class(TEDIXMLSegment)
  public
    constructor Create(Parent: TEDIXMLDataObject); reintroduce; overload;
    constructor Create(Parent: TEDIXMLDataObject; ElementCount: Integer); reintroduce; overload;
    function InternalAssignDelimiters: TEDIXMLDelimiters; override;
  end;

  TEDIXMLInterchangeControlSegment = class(TEDIXMLSegment)
  public
    constructor Create(Parent: TEDIXMLDataObject); reintroduce; overload;
    constructor Create(Parent: TEDIXMLDataObject; ElementCount: Integer); reintroduce; overload;
    function InternalAssignDelimiters: TEDIXMLDelimiters; override;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Transaction Set Loop
//--------------------------------------------------------------------------------------------------

  TEDIXMLTransactionSetLoop = class(TEDIXMLDataObjectGroup)
  private
    FParentTransactionSet: TEDIXMLTransactionSet;
  public
    constructor Create(Parent: TEDIXMLDataObject); reintroduce;
    destructor Destroy; override;
    function InternalAssignDelimiters: TEDIXMLDelimiters; override;
    function InternalCreateDataObjectGroup: TEDIXMLDataObjectGroup; override;
    function Assemble: string; override;
    procedure Disassemble; override;
  published
    property ParentTransactionSet: TEDIXMLTransactionSet read FParentTransactionSet
      write FParentTransactionSet;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Transaction Set
//--------------------------------------------------------------------------------------------------

  TEDIXMLTransactionSet = class(TEDIXMLTransactionSetLoop)
  private
    FSTSegment: TEDIXMLSegment;
    FSESegment: TEDIXMLSegment;
  public
    constructor Create(Parent: TEDIXMLDataObject); reintroduce;
    destructor Destroy; override;
    function InternalAssignDelimiters: TEDIXMLDelimiters; override;
    function InternalCreateDataObjectGroup: TEDIXMLDataObjectGroup; override;
    function Assemble: string; override;
    procedure Disassemble; override;
  published
    property SegmentST: TEDIXMLSegment read FSTSegment write FSTSegment;
    property SegmentSE: TEDIXMLSegment read FSESegment write FSESegment;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Functional Group
//--------------------------------------------------------------------------------------------------

  TEDIXMLFunctionalGroup = class(TEDIXMLDataObjectGroup)
  private
    FGSSegment: TEDIXMLSegment;
    FGESegment: TEDIXMLSegment;
  public
    constructor Create(Parent: TEDIXMLDataObject); reintroduce;
    destructor Destroy; override;
    function InternalAssignDelimiters: TEDIXMLDelimiters; override;
    function InternalCreateDataObjectGroup: TEDIXMLDataObjectGroup; override;
    function Assemble: string; override;
    procedure Disassemble; override;
  published
    property SegmentGS: TEDIXMLSegment read FGSSegment write FGSSegment;
    property SegmentGE: TEDIXMLSegment read FGESegment write FGESegment;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Interchange Control
//--------------------------------------------------------------------------------------------------

  TEDIXMLInterchangeControl = class(TEDIXMLDataObjectGroup)
  private
    FISASegment: TEDIXMLSegment;
    FIEASegment: TEDIXMLSegment;
  public
    constructor Create(Parent: TEDIXMLDataObject); reintroduce;
    destructor Destroy; override;
    function InternalAssignDelimiters: TEDIXMLDelimiters; override;
    function InternalCreateDataObjectGroup: TEDIXMLDataObjectGroup; override;
    function Assemble: string; override;
    procedure Disassemble; override;
  published
    property SegmentISA: TEDIXMLSegment read FISASegment write FISASegment;
    property SegmentIEA: TEDIXMLSegment read FIEASegment write FIEASegment;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI XML File Header
//--------------------------------------------------------------------------------------------------

  TEDIXMLNameSpaceOption = (nsNone, nsDefault, nsQualified);

  TEDIXMLFileHeader = class(TEDIXMLObject)
  private
    FDelimiters: TEDIXMLDelimiters;
    FAttributes: TEDIXMLAttributes;
    FXMLNameSpaceOption: TEDIXMLNameSpaceOption;
  protected
    function OutputAdditionalXMLHeaderAttributes: string; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseXMLHeader(XMLHeader: string);
    function OutputXMLHeader: string;
  published
    property Delimiters: TEDIXMLDelimiters read FDelimiters;
    property Attributes: TEDIXMLAttributes read FAttributes;
    property XMLNameSpaceOption: TEDIXMLNameSpaceOption read FXMLNameSpaceOption
      write FXMLNameSpaceOption;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI XML File
//--------------------------------------------------------------------------------------------------

  TEDIXMLFile = class(TEDIXMLDataObjectGroup)
  private
    FFileID: Integer;
    FFileName: string;
    FEDIXMLFileHeader: TEDIXMLFileHeader;
    procedure InternalLoadFromFile;
  public
    constructor Create(Parent: TEDIXMLDataObject); reintroduce;
    destructor Destroy; override;

    function InternalAssignDelimiters: TEDIXMLDelimiters; override;
    function InternalCreateDataObjectGroup: TEDIXMLDataObjectGroup; override;

    procedure LoadFromFile(const FileName: string);
    procedure ReLoadFromFile;
    procedure SaveToFile;
    procedure SaveAsToFile(const FileName: string);

    function Assemble: string; override;
    procedure Disassemble; override;
  published
    property FileID: Integer read FFileID write FFileID;
    property FileName: string read FFileName write FFileName;
    property XMLFileHeader: TEDIXMLFileHeader read FEDIXMLFileHeader;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI XML Format Translator
//--------------------------------------------------------------------------------------------------

  TEDIXMLANSIX12FormatTranslator = class(TEDIObject)
  private
    procedure ConvertTransactionSetLoopToXML(EDILoop: TEDITransactionSetLoop;
                                             XMLLoop: TEDIXMLTransactionSetLoop);
    procedure ConvertTransactionSetLoopToEDI(EDITransactionSet: TEDITransactionSet;
                                             XMLLoop: TEDIXMLTransactionSetLoop);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    //
    function ConvertToXMLSegment(EDISegment: TEDISegment): TEDIXMLSegment;
    function ConvertToXMLTransaction(
      EDITransactionSet: TEDITransactionSet): TEDIXMLTransactionSet; overload;
    function ConvertToXMLTransaction(EDITransactionSet: TEDITransactionSet;
      EDITransactionSetSpec: TEDITransactionSetSpec): TEDIXMLTransactionSet; overload;
    function ConvertToEDISegment(XMLSegment: TEDIXMLSegment): TEDISegment;
    function ConvertToEDITransaction(
      XMLTransactionSet: TEDIXMLTransactionSet): TEDITransactionSet;
  end;

implementation

uses
  JclResources;

//--------------------------------------------------------------------------------------------------
// TEDIXMLDelimiters
//--------------------------------------------------------------------------------------------------

{ TEDIXMLDelimiters }

constructor TEDIXMLDelimiters.Create;
begin
  inherited;
  SetBeginTagDelimiter('<');
  SetBeginOfEndTagDelimiter(FBeginTagDelimiter + '/');
  SetEndTagDelimiter('>');
  FSpaceDelimiter := ' ';
  FAssignmentDelimiter := '=';
  FSingleQuote := '''';
  FDoubleQuote := '"';
  SetBeginCDataDelimiter('<![CDATA[');
  SetEndCDataDelimiter(']]>');
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLDelimiters.SetBeginCDataDelimiter(const Value: string);
begin
  FBeginCDataDelimiter := Value;
  FBeginCDataLength := Length(FBeginCDataDelimiter);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLDelimiters.SetBeginOfEndTagDelimiter(const Value: string);
begin
  FBeginOfEndTagDelimiter := Value;
  FBeginOfEndTagLength := Length(FBeginOfEndTagDelimiter);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLDelimiters.SetBeginTagDelimiter(const Value: string);
begin
  FBeginTagDelimiter := Value;
  FBeginTagLength := Length(FBeginTagDelimiter);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLDelimiters.SetEndCDataDelimiter(const Value: string);
begin
  FEndCDataDelimiter := Value;
  FEndCDataLength := Length(FEndCDataDelimiter);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLDelimiters.SetEndTagDelimiter(const Value: string);
begin
  FEndTagDelimiter := Value;
  FEndTagLength := Length(FEndTagDelimiter);
end;

//--------------------------------------------------------------------------------------------------
// TEDIXMLAttributes
//--------------------------------------------------------------------------------------------------

{ TEDIXMLAttributes }

function TEDIXMLAttributes.CheckAttribute(Name, Value: string): Integer;
begin
  Result := -1;
  if FAttributes.Values[Name] = Value then
  begin
    Result := FAttributes.IndexOfName(Name);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLAttributes.CombineAttributes: string;
var
  I, J, K: Integer;
  QuoteDelimiter: string;
begin
  Result := '';
  for I := 0 to FAttributes.Count - 1 do
  begin
{$IFDEF DELPHI7_UP}
    J := StrSearch(FDelimiters.SingleQuote, FAttributes.ValueFromIndex[I]);
    K := StrSearch(FDelimiters.DoubleQuote, FAttributes.ValueFromIndex[I]);
{$ELSE}
    J := StrSearch(FDelimiters.SingleQuote, FAttributes.Values[FAttributes.Names[I]]);
    K := StrSearch(FDelimiters.DoubleQuote, FAttributes.Values[FAttributes.Names[I]]);
{$ENDIF}
    if J > K then
    begin
      QuoteDelimiter := FDelimiters.SingleQuote;
    end
    else
    begin
      QuoteDelimiter := FDelimiters.DoubleQuote;
    end;
    if Result <> '' then Result := Result + FDelimiters.SpaceDelimiter;
{$IFDEF DELPHI7_UP}
    Result := Result + FAttributes.Names[I] + FDelimiters.AssignmentDelimiter +
      QuoteDelimiter + FAttributes.ValueFromIndex[I] + QuoteDelimiter;
{$ELSE}
    Result := Result + FAttributes.Names[I] + FDelimiters.AssignmentDelimiter +
      QuoteDelimiter + FAttributes.Values[FAttributes.Names[I]] + QuoteDelimiter;
{$ENDIF}
  end;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIXMLAttributes.Create;
begin
  inherited;
  FAttributes := TStringList.Create;
  FDelimiters := TEDIXMLDelimiters.Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIXMLAttributes.Destroy;
begin
  FDelimiters.Free;
  FAttributes.Free;
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLAttributes.GetAttributeString(Name: string): string;
var
  J, K: Integer;
  QuoteDelimiter: string;
begin
  Result := '';
  J := StrSearch(FDelimiters.SingleQuote, FAttributes.Values[Name]);
  K := StrSearch(FDelimiters.DoubleQuote, FAttributes.Values[Name]);
  if J > K then
  begin
    QuoteDelimiter := FDelimiters.SingleQuote;
  end
  else
  begin
    QuoteDelimiter := FDelimiters.DoubleQuote;
  end;
  Result := Name + FDelimiters.AssignmentDelimiter +
    QuoteDelimiter + FAttributes.Values[Name] + QuoteDelimiter;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLAttributes.GetAttributeValue(Name: string): string;
begin
  Result := FAttributes.Values[Name];
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLAttributes.ParseAttributes(XMLStartTag: string);
var
  SearchResult: Integer;
  EndDataChar: string;
  Attribute, Value: string;
  AttributeStart, AttributeLen: Integer;
  ValueStart, ValueLen: Integer;
begin
  FAttributes.Clear;
  //Search for begin of attribute
  SearchResult := StrSearch(FDelimiters.SpaceDelimiter, XMLStartTag, 1);
  AttributeStart := SearchResult + Length(FDelimiters.SpaceDelimiter);
  while SearchResult > 0 do
  begin
    //Get the end data delimiter
    SearchResult := StrSearch(FDelimiters.AssignmentDelimiter, XMLStartTag, AttributeStart);
    if SearchResult > 0 then
    begin
      AttributeLen := SearchResult - AttributeStart;
      ValueStart := SearchResult + Length(FDelimiters.AssignmentDelimiter);
      EndDataChar := Copy(XMLStartTag, ValueStart, 1);
      //Search for end of data
      ValueStart := ValueStart + Length(FDelimiters.AssignmentDelimiter);
      SearchResult := StrSearch(EndDataChar, XMLStartTag, ValueStart);
      if SearchResult > 0 then
      begin
        ValueLen := SearchResult - ValueStart;
        Attribute := Copy(XMLStartTag, AttributeStart, AttributeLen);
        Value := Copy(XMLStartTag, ValueStart, ValueLen);
        FAttributes.Values[Attribute] := Value;
      end;
      //Search for begin of attribute
      SearchResult := StrSearch(FDelimiters.SpaceDelimiter, XMLStartTag, SearchResult);
      AttributeStart := SearchResult + Length(FDelimiters.SpaceDelimiter);
    end; //if SearchResult > 0 then
  end; //while SearchResult > 0 do
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLAttributes.SetAttribute(Name, Value: string);
begin
  FAttributes.Values[Name] := Value;
end;

//--------------------------------------------------------------------------------------------------
// TEDIXMLDataObject
//--------------------------------------------------------------------------------------------------

{ TEDIXMLDataObject }

constructor TEDIXMLDataObject.Create(Parent: TEDIXMLDataObject);
begin
  inherited Create;
  FState := ediCreated;
  FEDIDOT := ediUnknown;
  FData := '';
  FLength := 0;
  if Assigned(Parent) then
  begin
    FParent := Parent;
  end
  else
  begin
    FParent := nil;
  end;
  FDelimiters := nil;
  FAttributes := TEDIXMLAttributes.Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIXMLDataObject.Destroy;
begin
  FAttributes.Free;
  if not Assigned(FParent) then
  begin
    if Assigned(FDelimiters) then
    begin
      FDelimiters.Free;
    end;
  end;
  FDelimiters := nil;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLDataObject.GetData: string;
begin
  Result := FData;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLDataObject.SetData(const Data: string);
begin
  FData := Data;
  FLength := Length(FData);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLDataObject.SetDelimiters(const Delimiters: TEDIXMLDelimiters);
begin
  if not Assigned(FParent) then
  begin
    if Assigned(FDelimiters) then
    begin
      FDelimiters.Free;
      FDelimiters := nil;
    end;
  end;
  FDelimiters := Delimiters;
end;

//--------------------------------------------------------------------------------------------------
// TEDIXMLElement
//--------------------------------------------------------------------------------------------------

{ TEDIXMLElement }

function TEDIXMLElement.Assemble: string;
var
  AttributeString: string;
  OriginalData: string;
begin
  //Check delimiter assignment
  if not Assigned(FDelimiters) then
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
    begin
      raise EJclEDIError.CreateResRec(@EDIXMLError047);
    end;
  end;

  OriginalData := FData;
  //Handle Entity Reference Characters
  StrReplace(OriginalData, '&', '&amp;', [rfReplaceAll]);
  StrReplace(OriginalData, '<', '&lt;', [rfReplaceAll]);
  StrReplace(OriginalData, '>', '&gt;', [rfReplaceAll]);
  StrReplace(OriginalData, '"', '&quot;', [rfReplaceAll]);
  StrReplace(OriginalData, '''', '&apos;', [rfReplaceAll]);
  //
  AttributeString := FAttributes.CombineAttributes;
  if AttributeString <> '' then
  begin
    FData := FDelimiters.BTD + XMLTag_Element + FDelimiters.SpaceDelimiter +
      AttributeString + FDelimiters.ETD;
  end
  else
  begin
    FData := FDelimiters.BTD + XMLTag_Element + FDelimiters.ETD;
  end;

  if FCData then
  begin
    FData := FData + FDelimiters.BCDataD + OriginalData + FDelimiters.ECDataD;
  end;
  begin
    FData := FData + OriginalData;
  end;

  FData := FData + FDelimiters.BOfETD + XMLTag_Element + FDelimiters.ETD;

  Result := FData;
  FState := ediAssembled;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIXMLElement.Create(Parent: TEDIXMLDataObject);
begin
  if Assigned(Parent) and (Parent is TEDIXMLSegment) then
  begin
    inherited Create(Parent);
  end
  else
  begin
    inherited Create(nil);
  end;
  FEDIDOT := ediElement;
  FCData := False;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLElement.Disassemble;
var
  StartPos, EndPos, SearchResult: Integer;
  XMLStartTag: string;
begin
  //Check delimiter assignment
  if not Assigned(FDelimiters) then
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
    begin
      raise EJclEDIError.CreateResRec(@EDIXMLError046);
    end;
  end;
  //Set next start positon
  StartPos := 1;
  //Move past begin element tag
  SearchResult := StrSearch(FDelimiters.BTD + XMLTag_Element, FData, StartPos);
  if SearchResult > 0 then
  begin
    SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
    XMLStartTag := Copy(FData, StartPos, (SearchResult + FDelimiters.ETDLength) - StartPos);
    FAttributes.ParseAttributes(XMLStartTag);
  end
  else
  begin
    raise EJclEDIError.CreateResRec(@EDIXMLError048);
  end;
  //Set data start positon
  StartPos := SearchResult + FDelimiters.ETDLength;
  //Check for CData tag
  FCData := False;
  SearchResult := StrSearch(FDelimiters.BCDataD, FData, StartPos);
  if SearchResult > 0 then
  begin
    StartPos := SearchResult + FDelimiters.BCDataLength;
    FCData := True;
  end;
  //
  SearchResult := StrSearch(FDelimiters.BOfETD + XMLTag_Element, FData, StartPos);
  if SearchResult > 0 then
  begin
    EndPos := SearchResult;
    SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
    if SearchResult > 0 then
    begin
      if FCData then
      begin
        EndPos := EndPos - FDelimiters.ECDataLength;
      end;
      FData := Copy(FData, StartPos, (EndPos - StartPos));
    end
    else
    begin
      raise EJclEDIError.CreateResRec(@EDIXMLError050);
    end;
  end
  else
  begin
    raise EJclEDIError.CreateResRec(@EDIXMLError049);
  end;
  //Handle Entity Reference Characters
  StrReplace(FData, '&lt;', '<', [rfReplaceAll]);
  StrReplace(FData, '&gt;', '>', [rfReplaceAll]);
  StrReplace(FData, '&quot;', '"', [rfReplaceAll]);
  StrReplace(FData, '&apos;', '''', [rfReplaceAll]);
  StrReplace(FData, '&amp;', '&', [rfReplaceAll]);
  //
  FState := ediDissassembled;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLElement.GetIndexPositionFromParent: Integer;
var
  I: Integer;
begin
  Result := -1;
  if Assigned(Parent) and (Parent is TEDIXMLSegment) then
  begin
    for I := Low(TEDIXMLSegment(Parent).Elements) to High(TEDIXMLSegment(Parent).Elements) do
    begin
      if TEDIXMLSegment(Parent).Element[I] = Self then
      begin
        Result := I;
      end;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLElement.InternalAssignDelimiters: TEDIXMLDelimiters;
begin
  Result := nil;
  //Attempt to assign the delimiters
  if not Assigned(FDelimiters) then
  begin
    //Get the delimiters from the parent segment
    if Assigned(Parent) and (Parent is TEDIXMLSegment) then
    begin
      if Assigned(Parent.Delimiters) then
      begin
        Result := Parent.Delimiters;
        Exit;
      end;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------
// TEDIXMLSegment
//--------------------------------------------------------------------------------------------------

{ TEDIXMLSegment }

function TEDIXMLSegment.AddElement: Integer;
begin
  SetLength(FElements, Length(FElements) + 1);
  FElements[High(FElements)] := InternalCreateElement;
  Result := High(FElements); //Return position of element
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLSegment.AddElements(Count: Integer): Integer;
var
  I, J: Integer;
begin
  I := Length(FElements);
  Result := I; //Return position of 1st element
  //Resize
  SetLength(FElements, Length(FElements) + Count);
  //Add
  for J := I to High(FElements) do
  begin
    FElements[J]:= InternalCreateElement;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLSegment.AppendElement(Element: TEDIXMLElement): Integer;
begin
  SetLength(FElements, Length(FElements) + 1);
  FElements[High(FElements)] := Element;
  Element.Parent := Self;
  Result := High(FElements); //Return position of element
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLSegment.AppendElements(ElementArray: TEDIXMLElementArray): Integer;
var
  I, J, K: Integer;
begin
  I := 0;
  J := Length(FElements);
  Result := J; //Return position of 1st element
  //Resize
  SetLength(FElements, Length(FElements) + Length(ElementArray));
  //Append
  for K := J to High(ElementArray) do
  begin
    FElements[K] := ElementArray[I];
    FElements[K].Parent := Self;
    Inc(I);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLSegment.Assemble: string;
var
  I: Integer;
  AttributeString: string;
begin
  FData := '';
  FLength := 0;
  Result := '';

  if not Assigned(FDelimiters) then //Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
    begin
      raise EJclEDIError.CreateResRec(@EDIXMLError042);
    end;
  end;

  AttributeString := FAttributes.CombineAttributes;
  if AttributeString <> '' then
  begin
    FData := FDelimiters.BTD + XMLTag_Segment + FDelimiters.SpaceDelimiter +
             AttributeString + FDelimiters.ETD;
  end
  else
  begin
    FData := FDelimiters.BTD + XMLTag_Segment + FDelimiters.ETD;
  end;

  if Length(FElements) > 0 then
  begin
    for I := Low(FElements) to High(FElements) do
    begin
      if Assigned(FElements[I]) then
      begin
        FData := FData + FElements[I].Assemble;
      end
      else
      begin
        FData := FData + FDelimiters.BTD + XMLTag_Element + FDelimiters.ETD +
                         FDelimiters.BOfETD + XMLTag_Element + FDelimiters.ETD;
      end;
    end;
  end;
  FData := FData + FDelimiters.BOfETD + XMLTag_Segment + FDelimiters.ETD;
  FLength := Length(FData);
  Result := FData; //Return assembled string

  DeleteElements;

  FState := ediAssembled;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIXMLSegment.Create(Parent: TEDIXMLDataObject; ElementCount: Integer);
begin
  if Assigned(Parent) and (Parent is TEDIXMLTransactionSet) then
  begin
    inherited Create(Parent);
  end
  else
  begin
    inherited Create(nil);
  end;
  FEDIDOT := ediSegment;
  SetLength(FElements, 0);
  AddElements(ElementCount);
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIXMLSegment.Create(Parent: TEDIXMLDataObject);
begin
  if Assigned(Parent) and (Parent is TEDIXMLDataObjectGroup) then
  begin
    inherited Create(Parent);
  end
  else
  begin
    inherited Create(nil);
  end;
  FEDIDOT := ediSegment;
  SetLength(FElements, 0);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLSegment.DeleteElement(Element: TEDIXMLElement);
var
  I: Integer;
begin
  for I := Low(FElements) to High(FElements) do
  begin
    if FElements[I] = Element then
    begin
      DeleteElement(I);
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLSegment.DeleteElement(Index: Integer);
var
  I: Integer;
begin
  if (Length(FElements) > 0) and (Index >= Low(FElements)) and (Index <= High(FElements)) then
  begin
    //Delete
    FElements[Index].Free;
    FElements[Index] := nil;
    //Shift
    for I := Index + 1 to High(FElements) do
    begin
      FElements[I-1] := FElements[I];
    end;
    //Resize
    SetLength(FElements, High(FElements));
  end
  else
  begin
    raise EJclEDIError.CreateResRecFmt(@EDIXMLError058, [IntToStr(Index)]);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLSegment.DeleteElements;
var
  I: Integer;
begin
  for I := Low(FElements) to High(FElements) do
  begin
    if Assigned(FElements[I]) then
    begin
      //Delete
      FElements[I].Free;
      FElements[I] := nil;
    end;
  end;
  //Resize
  SetLength(FElements, 0);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLSegment.DeleteElements(Index, Count: Integer);
var
  I: Integer;
begin
  if (Length(FElements) > 0) and (Index >= Low(FElements)) and (Index <= High(FElements)) then
  begin
    //Delete
    for I := Index to (Index + Count) - 1 do
    begin
      if Assigned(FElements[I]) then
      begin
        FElements[I].Free;
        FElements[I] := nil;
      end;
    end;
    //Shift
    for I := (Index + Count) to High(FElements) do
    begin
      FElements[I-Count] := FElements[I];
      FElements[I] := nil;
    end;
    //Resize
    SetLength(FElements, Length(FElements) - Count);
  end
  else
  begin
    raise EJclEDIError.CreateResRecFmt(@EDIXMLError058, [IntToStr(Index)]);
  end;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIXMLSegment.Destroy;
begin
  DeleteElements;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLSegment.Disassemble;
var
  I, StartPos, SearchResult: Integer;
  XMLStartTag: string;
begin
  DeleteElements;
  //Check delimiter assignment
  if not Assigned(FDelimiters) then
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
    begin
      raise EJclEDIError.CreateResRec(@EDIXMLError041);
    end;
  end;
  //Set next start positon
  StartPos := 1;
  //Move past begin segment tag
  SearchResult := StrSearch(FDelimiters.BTD + XMLTag_Segment, FData, StartPos);
  if SearchResult > 0 then
  begin
    StartPos := SearchResult;
    SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
    XMLStartTag := Copy(FData, StartPos, (SearchResult + FDelimiters.ETDLength) - StartPos);
    FAttributes.ParseAttributes(XMLStartTag);
  end
  else
  begin
    raise EJclEDIError.CreateResRec(@EDIXMLError043);
  end;
  //Set next start positon
  StartPos := SearchResult + FDelimiters.ETDLength;
  //Search for element
  SearchResult := StrSearch(FDelimiters.BTD + XMLTag_Element, FData, StartPos);
  //Search for Segments
  while SearchResult > 0 do
  begin
    SearchResult := StrSearch(FDelimiters.BOfETD + XMLTag_Element, FData, SearchResult);
    if SearchResult > 0 then
    begin
      SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
      if SearchResult > 0 then
      begin
        I := AddElement; //Add Element
        FElements[I].Data :=
          Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.ETDLength));
        FElements[I].Disassemble;
      end
      else
      begin
        raise EJclEDIError.CreateResRec(@EDIXMLError050);
      end;
    end
    else
    begin
      raise EJclEDIError.CreateResRec(@EDIXMLError049);
    end;
    //Set next start positon
    StartPos := SearchResult + FDelimiters.ETDLength;
    //Search for element
    SearchResult := StrSearch(FDelimiters.BTD + XMLTag_Element, FData, StartPos);
  end; //while SearchResult > 0 do
  FData := '';
  //
  FState := ediDissassembled;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLSegment.GetElement(Index: Integer): TEDIXMLElement;
begin
  if (Length(FElements) > 0) then
    if (Index >= Low(FElements)) then
      if (Index <= High(FElements)) then
      begin
        if not Assigned(FElements[Index]) then
        begin
          raise EJclEDIError.CreateResRecFmt(@EDIXMLError057, [IntToStr(Index)]);
        end;
        Result := FElements[Index];
      end
      else
        raise EJclEDIError.CreateResRecFmt(@EDIXMLError056, [IntToStr(Index)])
    else
      raise EJclEDIError.CreateResRecFmt(@EDIXMLError055, [IntToStr(Index)])
  else
    raise EJclEDIError.CreateResRecFmt(@EDIXMLError054, [IntToStr(Index)]);
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLSegment.GetIndexPositionFromParent: Integer;
var
  I: Integer;
begin
  Result := -1;
  if Assigned(Parent) and (Parent is TEDIXMLTransactionSet) then
  begin
    for I := Low(TEDIXMLTransactionSet(Parent).EDIDataObjects) to
             High(TEDIXMLTransactionSet(Parent).EDIDataObjects) do
    begin
      if TEDIXMLTransactionSet(Parent).EDIDataObject[I] = Self then
      begin
        Result := I;
      end;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLSegment.InsertElement(InsertIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := InsertIndex;
  if (Length(FElements) > 0) and (InsertIndex >= Low(FElements)) and
    (InsertIndex <= High(FElements)) then
  begin
    //Resize
    SetLength(FElements, Length(FElements) + 1);
    //Shift
    for I := High(FElements) downto InsertIndex + 1 do
    begin
      FElements[I] := FElements[I-1];
    end;
    //Insert
    FElements[InsertIndex] := nil;
    FElements[InsertIndex] := InternalCreateElement;
  end
  else
  begin
    Result := AddElement;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLSegment.InsertElement(InsertIndex: Integer; Element: TEDIXMLElement): Integer;
var
  I: Integer;
begin
  Result := InsertIndex;
  if (Length(FElements) > 0) and (InsertIndex >= Low(FElements)) and
    (InsertIndex <= High(FElements)) then
  begin
    //Resize
    SetLength(FElements, Length(FElements) + 1);
    //Shift
    for I := High(FElements) downto InsertIndex + 1 do
    begin
      FElements[I] := FElements[I-1];
    end;
    //Insert
    FElements[InsertIndex] := nil;
    FElements[InsertIndex] := Element;
    FElements[InsertIndex].Parent := Self;
  end
  else
  begin
    Result := AppendElement(Element);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLSegment.InsertElements(InsertIndex: Integer;
  ElementArray: TEDIXMLElementArray): Integer;
var
  I, J, K: Integer;
begin
  Result := InsertIndex;
  I := Length(ElementArray);
  if (Length(FElements) > 0) and (InsertIndex >= Low(FElements)) and
    (InsertIndex <= High(FElements)) then
  begin
    //Resize
    SetLength(FElements, Length(FElements) + I);
    //Shift
    for J := High(FElements) downto InsertIndex + I do
    begin
      FElements[J] := FElements[J-I];
      FElements[J-I] := nil;
    end;
    //Insert
    K := 0;
    for J := InsertIndex to (InsertIndex + I) - 1 do
    begin
      FElements[J] := ElementArray[K];
      FElements[J].Parent := Self;
      Inc(K);
    end;
  end
  else
  begin
    Result := AppendElements(ElementArray);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLSegment.InsertElements(InsertIndex, Count: Integer): Integer;
var
  I: Integer;
begin
  Result := InsertIndex;
  if (Length(FElements) > 0) and (InsertIndex >= Low(FElements)) and
    (InsertIndex <= High(FElements)) then
  begin
    //Resize
    SetLength(FElements, Length(FElements) + Count);
    //Shift
    for I := High(FElements) downto InsertIndex + Count do
    begin
      FElements[I] := FElements[I-Count];
      FElements[I-Count] := nil;
    end;
    //Insert
    for I := InsertIndex to (InsertIndex + Count) - 1 do
    begin
      FElements[I] := InternalCreateElement;
    end;
  end
  else
  begin
    Result := AddElements(Count);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLSegment.InternalAssignDelimiters: TEDIXMLDelimiters;
begin
  Result := nil;
  if not Assigned(FDelimiters) then //Attempt to assign the delimiters
  begin
    //Get the delimiters from the transaction set loop
    if Assigned(Parent) and (Parent is TEDIXMLTransactionSetLoop) then
    begin
      if Assigned(Parent.Delimiters) then
      begin
        Result := TEDIXMLTransactionSetLoop(Parent).ParentTransactionSet.Delimiters;
        Exit;
      end;
    end;
    //Get the delimiters from the transaction set
    if Assigned(Parent) and (Parent is TEDIXMLTransactionSet) then
    begin
      if Assigned(Parent.Delimiters) then
      begin
        Result := Parent.Delimiters;
        Exit;
      end;
      //Get the delimiters from the functional group
      if Assigned(Parent.Parent) and (Parent.Parent is TEDIXMLFunctionalGroup) then
      begin
        if Assigned(Parent.Parent.Delimiters) then
        begin
          Result := Parent.Parent.Delimiters;
          Exit;
        end;
        //Get the delimiters from the interchange control header
        if Assigned(Parent.Parent.Parent) and
           (Parent.Parent.Parent is TEDIXMLInterchangeControl) then
        begin
          if Assigned(Parent.Parent.Parent.Delimiters) then
          begin
            Result := Parent.Parent.Parent.Delimiters;
          end;
        end;
      end;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLSegment.InternalCreateElement: TEDIXMLElement;
begin
  Result := TEDIXMLElement.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLSegment.SetElement(Index: Integer; Element: TEDIXMLElement);
begin
  if (Length(FElements) > 0) then
    if (Index >= Low(FElements)) then
      if (Index <= High(FElements)) then
      begin
        if Assigned(FElements[Index]) then
        begin
          FElements[Index].Free;
          FElements[Index] := nil;
        end;
        FElements[Index] := Element;
      end
      else
        raise EJclEDIError.CreateResRecFmt(@EDIXMLError053, [IntToStr(Index)])
    else
      raise EJclEDIError.CreateResRecFmt(@EDIXMLError052, [IntToStr(Index)])
  else
    raise EJclEDIError.CreateResRecFmt(@EDIXMLError051, [IntToStr(Index)]);
end;

//--------------------------------------------------------------------------------------------------
// TEDIXMLTransactionSetSegment
//--------------------------------------------------------------------------------------------------

{ TEDIXMLTransactionSetSegment }

constructor TEDIXMLTransactionSetSegment.Create(Parent: TEDIXMLDataObject);
begin
  inherited;
  if Assigned(Parent) and (Parent is TEDIXMLTransactionSet) then
  begin
    FParent := Parent;
  end;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIXMLTransactionSetSegment.Create(Parent: TEDIXMLDataObject; ElementCount: Integer);
begin
  inherited;
  if Assigned(Parent) and (Parent is TEDIXMLTransactionSet) then
  begin
    FParent := Parent;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLTransactionSetSegment.InternalAssignDelimiters: TEDIXMLDelimiters;
begin
  Result := inherited InternalAssignDelimiters;
end;

//--------------------------------------------------------------------------------------------------
// TEDIXMLFunctionalGroupSegment
//--------------------------------------------------------------------------------------------------

{ TEDIXMLFunctionalGroupSegment }

constructor TEDIXMLFunctionalGroupSegment.Create(Parent: TEDIXMLDataObject);
begin
  inherited;
  if Assigned(Parent) and (Parent is TEDIXMLFunctionalGroup) then
  begin
    FParent := Parent;
  end;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIXMLFunctionalGroupSegment.Create(Parent: TEDIXMLDataObject;
  ElementCount: Integer);
begin
  inherited;
  if Assigned(Parent) and (Parent is TEDIXMLFunctionalGroup) then
  begin
    FParent := Parent;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLFunctionalGroupSegment.InternalAssignDelimiters: TEDIXMLDelimiters;
begin
  Result := nil;
  //Attempt to assign the delimiters
  if not Assigned(FDelimiters) then
  begin
    //Get the delimiters from the functional group
    if Assigned(Parent) and (Parent is TEDIXMLFunctionalGroup) then
    begin
      if Assigned(Parent.Delimiters) then
      begin
        Result := Parent.Delimiters;
        Exit;
      end;
      //Get the delimiters from the interchange control
      if Assigned(Parent.Parent) and (Parent.Parent is TEDIXMLInterchangeControl) then
      begin
        if Assigned(Parent.Parent.Delimiters) then
        begin
          Result := Parent.Parent.Delimiters;
        end;
      end;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------
// TEDIXMLInterchangeControlSegment
//--------------------------------------------------------------------------------------------------

{ TEDIXMLInterchangeControlSegment }

constructor TEDIXMLInterchangeControlSegment.Create(Parent: TEDIXMLDataObject);
begin
  inherited;
  if Assigned(Parent) and (Parent is TEDIXMLInterchangeControl) then
  begin
    FParent := Parent;
  end;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIXMLInterchangeControlSegment.Create(Parent: TEDIXMLDataObject;
  ElementCount: Integer);
begin
  inherited;
  if Assigned(Parent) and (Parent is TEDIXMLInterchangeControl) then
  begin
    FParent := Parent;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLInterchangeControlSegment.InternalAssignDelimiters: TEDIXMLDelimiters;
begin
  Result := nil;
  //Attempt to assign the delimiters
  if not Assigned(FDelimiters) then
  begin
    //Get the delimiters from the interchange control
    if Assigned(Parent) and (Parent is TEDIXMLInterchangeControl) then
    begin
      if Assigned(Parent.Delimiters) then
      begin
        Result := Parent.Delimiters;
      end;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------
// TEDIXMLDataObjectGroup
//--------------------------------------------------------------------------------------------------

{ TEDIXMLDataObjectGroup }

function TEDIXMLDataObjectGroup.AddGroup: Integer;
var
  EDIGroup: TEDIXMLDataObjectGroup;
begin
  EDIGroup := InternalCreateDataObjectGroup;
  Result := AppendEDIDataObject(EDIGroup);
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLDataObjectGroup.AddSegment: Integer;
var
  EDISegment: TEDIXMLSegment;
begin
  EDISegment := TEDIXMLSegment.Create(Self);
  Result := AppendEDIDataObject(EDISegment);
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLDataObjectGroup.AppendEDIDataObject(EDIDataObject: TEDIXMLDataObject): Integer;
begin
  SetLength(FEDIDataObjects, Length(FEDIDataObjects) + 1);
  FEDIDataObjects[High(FEDIDataObjects)] := EDIDataObject;
  EDIDataObject.Parent := Self;
  Result := High(FEDIDataObjects);
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIXMLDataObjectGroup.Create(Parent: TEDIXMLDataObject);
begin
  inherited Create(Parent);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLDataObjectGroup.DeleteEDIDataObject(EDIDataObject: TEDIXMLDataObject);
var
  I: Integer;
begin
  for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
  begin
    if FEDIDataObjects[I] = EDIDataObject then
    begin
      DeleteEDIDataObject(I);
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLDataObjectGroup.DeleteEDIDataObject(Index: Integer);
var
  I: Integer;
begin
  if (Length(FEDIDataObjects) > 0) and (Index >= Low(FEDIDataObjects)) and
    (Index <= High(FEDIDataObjects)) then
  begin
    //Delete
    FEDIDataObjects[Index].Free;
    FEDIDataObjects[Index] := nil;
    //Shift
    for I := Index + 1 to High(FEDIDataObjects) do
    begin
      FEDIDataObjects[I-1] := FEDIDataObjects[I];
    end;
    //Resize
    SetLength(FEDIDataObjects, High(FEDIDataObjects));
  end
  else
  begin
    raise EJclEDIError.CreateResRec(@EDIXMLError040);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLDataObjectGroup.DeleteEDIDataObjects;
var
  I: Integer;
begin
  for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
  begin
    if Assigned(FEDIDataObjects[I]) then
    begin
      //Delete
      FEDIDataObjects[I].Free;
      FEDIDataObjects[I] := nil;
    end;
  end;
  //Resize
  SetLength(FEDIDataObjects, 0);
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIXMLDataObjectGroup.Destroy;
begin
  DeleteEDIDataObjects;
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLDataObjectGroup.GetEDIDataObject(Index: Integer): TEDIXMLDataObject;
begin
  if (Length(FEDIDataObjects) > 0) then
    if (Index >= Low(FEDIDataObjects)) then
      if (Index <= High(FEDIDataObjects)) then
      begin
        if not Assigned(FEDIDataObjects[Index]) then
        begin
          raise EJclEDIError.CreateResRecFmt(@EDIXMLError039, [IntToStr(Index)]);
        end;
        Result := FEDIDataObjects[Index];
      end
      else
        raise EJclEDIError.CreateResRecFmt(@EDIXMLError038, [IntToStr(Index)])
    else
      raise EJclEDIError.CreateResRecFmt(@EDIXMLError037, [IntToStr(Index)])
  else
    raise EJclEDIError.CreateResRecFmt(@EDIXMLError036, [IntToStr(Index)]);
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLDataObjectGroup.InsertEDIDataObject(InsertIndex: Integer;
  EDIDataObject: TEDIXMLDataObject): Integer;
var
  I: Integer;
begin
  Result := InsertIndex;
  if (Length(FEDIDataObjects) > 0) and (InsertIndex >= Low(FEDIDataObjects)) and
    (InsertIndex <= High(FEDIDataObjects)) then
  begin
    //Resize
    SetLength(FEDIDataObjects, Length(FEDIDataObjects) + 1);
    //Shift
    for I := High(FEDIDataObjects) downto InsertIndex + 1 do
    begin
      FEDIDataObjects[I] := FEDIDataObjects[I-1];
    end;
    //Insert
    FEDIDataObjects[InsertIndex] := nil;
    FEDIDataObjects[InsertIndex] := EDIDataObject;
    FEDIDataObjects[InsertIndex].Parent := Self;
  end
  else
  begin
    Result := AppendEDIDataObject(EDIDataObject);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLDataObjectGroup.InsertGroup(InsertIndex: Integer): Integer;
var
  EDIGroup: TEDIXMLDataObjectGroup;
begin
  EDIGroup := InternalCreateDataObjectGroup;
  Result := InsertEDIDataObject(InsertIndex, EDIGroup);
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLDataObjectGroup.InsertSegment(InsertIndex: Integer): Integer;
var
  EDISegment: TEDIXMLSegment;
begin
  EDISegment := TEDIXMLSegment.Create(Self);
  Result := InsertEDIDataObject(InsertIndex, EDISegment);
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLDataObjectGroup.SearchForSegmentInDataString(Id: string;
  StartPos: Integer): Integer;
var
  SegmentTag: string;
  SearchResult, SegmentTagStartPos: Integer;
  EDIXMLAttributes: TEDIXMLAttributes;
begin
  Result := 0;
  EDIXMLAttributes := TEDIXMLAttributes.Create;
  SearchResult := StrSearch(FDelimiters.BTD + XMLTag_Segment, FData, StartPos);
  SegmentTagStartPos := SearchResult;
  while SearchResult > 0 do
  begin
    SearchResult := StrSearch(FDelimiters.ETD, FData, SegmentTagStartPos);
    if SearchResult > 0 then
    begin
      SegmentTag := Copy(FData, SegmentTagStartPos, ((SearchResult - SegmentTagStartPos) + FDelimiters.ETDLength));
      EDIXMLAttributes.ParseAttributes(SegmentTag);
      Result := EDIXMLAttributes.CheckAttribute(XMLAttribute_Id, Id);
      if Result >= 0 then
      begin
        Result := SegmentTagStartPos;
        Break;
      end;
    end;
    SegmentTagStartPos := SearchResult + FDelimiters.ETDLength;
    SearchResult := StrSearch(FDelimiters.BTD + XMLTag_Segment, FData, SegmentTagStartPos);
  end;
  EDIXMLAttributes.Free;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLDataObjectGroup.SetEDIDataObject(Index: Integer; EDIDataObject: TEDIXMLDataObject);
begin
  if (Length(FEDIDataObjects) > 0) then
    if (Index >= Low(FEDIDataObjects)) then
      if (Index <= High(FEDIDataObjects)) then
      begin
        if Assigned(FEDIDataObjects[Index]) then
        begin
          FEDIDataObjects[Index].Free;
          FEDIDataObjects[Index] := nil;
        end;
        FEDIDataObjects[Index] := EDIDataObject;
      end
      else
        raise EJclEDIError.CreateResRecFmt(@EDIXMLError035, [IntToStr(Index)])
    else
      raise EJclEDIError.CreateResRecFmt(@EDIXMLError034, [IntToStr(Index)])
  else
    raise EJclEDIError.CreateResRecFmt(@EDIXMLError033, [IntToStr(Index)]);
end;

//--------------------------------------------------------------------------------------------------
// TEDIXMLTransactionSetLoop
//--------------------------------------------------------------------------------------------------

{ TEDIXMLTransactionSetLoop }

function TEDIXMLTransactionSetLoop.Assemble: string;
var
  I: Integer;
  AttributeString: string;
begin
  FData := '';
  FLength := 0;
  Result := '';

  if not Assigned(FDelimiters) then //Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
    begin
      raise EJclEDIError.CreateResRec(@EDIXMLError030);
    end;
  end;

  AttributeString := FAttributes.CombineAttributes;
  if AttributeString <> '' then
  begin
    FData := FDelimiters.BTD + XMLTag_TransactionSetLoop + FDelimiters.SpaceDelimiter +
             AttributeString + FDelimiters.ETD;
  end
  else
  begin
    FData := FDelimiters.BTD + XMLTag_TransactionSetLoop + FDelimiters.ETD;
  end;

  if Length(FEDIDataObjects) > 0 then
  begin
    for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
    begin
      if Assigned(FEDIDataObjects[I]) then
      begin
        FData := FData + FEDIDataObjects[I].Assemble;
      end;
    end;
  end;
  FData := FData + FDelimiters.BOfETD + XMLTag_TransactionSetLoop + FDelimiters.ETD;
  FLength := Length(FData);
  Result := FData; //Return assembled string

  DeleteEDIDataObjects;

  FState := ediAssembled;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIXMLTransactionSetLoop.Create(Parent: TEDIXMLDataObject);
begin
  inherited Create(Parent);
  if Assigned(Parent) and (Parent is TEDIXMLTransactionSet) then
  begin
    FParentTransactionSet := TEDIXMLTransactionSet(Parent);
  end
  else if Assigned(Parent) and (Parent is TEDIXMLTransactionSetLoop) then
  begin
    FParentTransactionSet := TEDIXMLTransactionSetLoop(Parent).ParentTransactionSet;
  end
  else
  begin
    FParentTransactionSet := nil;
  end;
  FEDIDOT := ediLoop;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIXMLTransactionSetLoop.Destroy;
begin
  inherited;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLTransactionSetLoop.Disassemble;
var
  I, J, StartPos, SearchResult: Integer;
  XMLStartTag, SearchTag: string;
  NestedLoopCount: Integer;
begin
  DeleteEDIDataObjects;
  //Check delimiter assignment
  if not Assigned(FDelimiters) then
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
    begin
      raise EJclEDIError.CreateResRec(@EDIXMLError029);
    end;
  end;
  //Set next start positon
  StartPos := 1;
  //Move past begin loop tag
  SearchResult := StrSearch(FDelimiters.BTD + XMLTag_TransactionSetLoop, FData, StartPos);
  if SearchResult > 0 then
  begin
    StartPos := SearchResult;
    SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
    XMLStartTag := Copy(FData, StartPos, (SearchResult + FDelimiters.ETDLength) - StartPos);
    FAttributes.ParseAttributes(XMLStartTag);
  end
  else
  begin
    raise EJclEDIError.CreateResRec(@EDIXMLError031);
  end;
  //Set next start positon
  StartPos := SearchResult + FDelimiters.ETDLength;
  //Determine the nearest tag to search for
  I := StrSearch(FDelimiters.BTD + XMLTag_Segment, FData, StartPos);
  J := StrSearch(FDelimiters.BTD + XMLTag_TransactionSetLoop, FData, StartPos);
  if (I < J) or (J <= 0) then
  begin
    SearchTag := XMLTag_Segment;
    SearchResult := I;
  end
  else
  begin
    SearchTag := XMLTag_TransactionSetLoop;
    SearchResult := J;
  end;
  //Search for Segments or Loops
  while SearchResult > 0 do
  begin
    if SearchTag = XMLTag_Segment then
    begin
      SearchResult := StrSearch(FDelimiters.BOfETD + SearchTag, FData, SearchResult);
      if SearchResult > 0 then
      begin
        SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
        if SearchResult > 0 then
        begin
          I := AddSegment; //Add Segment
          EDIDataObjects[I].Data :=
            Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.ETDLength));
          EDIDataObjects[I].Disassemble;
        end
        else
        begin
          raise EJclEDIError.CreateResRec(@EDIXMLError045);
        end;
      end
      else
      begin
        raise EJclEDIError.CreateResRec(@EDIXMLError044);
      end;
    end
    else
    begin
      NestedLoopCount := 0;
      SearchResult := StartPos;
      //Search for the proper end loop tag
      repeat
        I := StrSearch(FDelimiters.BOfETD + SearchTag, FData, SearchResult); //Find loop end
        J := StrSearch(FDelimiters.BTD + SearchTag, FData, SearchResult);    //Find loop begin
        if (I < J) or (J <= 0) then
        begin
          Dec(NestedLoopCount);
          SearchResult := I + FDelimiters.ETDLength;
        end
        else if (I > J) and (J > 0) then
        begin
          Inc(NestedLoopCount);
          SearchResult := J + FDelimiters.ETDLength;
        end;
      until (NestedLoopCount <= 0) or (I <= 0);
      SearchResult := I;
      //
      if SearchResult > 0 then
      begin
        SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
        if SearchResult > 0 then
        begin
          I := AddGroup; //Add Transaction Set Loop
          EDIDataObjects[I].Data :=
            Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.ETDLength));
          EDIDataObjects[I].Disassemble;
        end
        else
        begin
          raise EJclEDIError.CreateResRec(@EDIXMLError032);
        end;
      end
      else
      begin
        raise EJclEDIError.CreateResRec(@EDIXMLError031);
      end;
    end; //if SearchTag = XMLTag_Segment then
    //Set next start positon
    StartPos := SearchResult + FDelimiters.ETDLength;
    //Determine the nearest tag to search for
    I := StrSearch(FDelimiters.BTD + XMLTag_Segment, FData, StartPos);
    J := StrSearch(FDelimiters.BTD + XMLTag_TransactionSetLoop, FData, StartPos);
    if (I < J) or (J <= 0) then
    begin
      SearchTag := XMLTag_Segment;
    end
    else
    begin
      SearchTag := XMLTag_TransactionSetLoop;
    end;
    SearchResult := StrSearch(FDelimiters.BTD + SearchTag, FData, StartPos);
    StartPos := SearchResult;
  end; //while SearchResult > 0 do
  FData := '';
  //
  FState := ediDissassembled;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLTransactionSetLoop.InternalAssignDelimiters: TEDIXMLDelimiters;
begin
  Result := nil;
  if Assigned(FParentTransactionSet) then
  begin
    Result := Parent.Delimiters;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLTransactionSetLoop.InternalCreateDataObjectGroup: TEDIXMLDataObjectGroup;
begin
  Result := TEDIXMLTransactionSetLoop.Create(Self);
end;

//--------------------------------------------------------------------------------------------------
// TEDIXMLTransactionSet
//--------------------------------------------------------------------------------------------------

{ TEDIXMLTransactionSet }

function TEDIXMLTransactionSet.Assemble: string;
var
  I: Integer;
  AttributeString: string;
begin
  FData := '';
  FLength := 0;
  Result := '';

  if not Assigned(FDelimiters) then //Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
    begin
      raise EJclEDIError.CreateResRec(@EDIXMLError026);
    end;
  end;

  AttributeString := FAttributes.CombineAttributes;
  if AttributeString <> '' then
  begin
    FData := FDelimiters.BTD + XMLTag_TransactionSet + FDelimiters.SpaceDelimiter +
             AttributeString + FDelimiters.ETD;
  end
  else
  begin
    FData := FDelimiters.BTD + XMLTag_TransactionSet + FDelimiters.ETD;
  end;

  if Length(FEDIDataObjects) > 0 then
  begin
    for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
    begin
      if Assigned(FEDIDataObjects[I]) then
      begin
        FData := FData + FEDIDataObjects[I].Assemble;
      end;
    end;
  end;
  FData := FData + FDelimiters.BOfETD + XMLTag_TransactionSet + FDelimiters.ETD;
  FLength := Length(FData);
  Result := FData; //Return assembled string

  DeleteEDIDataObjects;

  FState := ediAssembled;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIXMLTransactionSet.Create(Parent: TEDIXMLDataObject);
begin
  inherited;
  FParentTransactionSet := Self;
  FEDIDOT := ediTransactionSet;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIXMLTransactionSet.Destroy;
begin
  inherited;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLTransactionSet.Disassemble;
var
  I, J, StartPos, SearchResult: Integer;
  SearchTag, TempData: string;
  NestedLoopCount: Integer;
begin
  DeleteEDIDataObjects;
  //Check delimiter assignment
  if not Assigned(FDelimiters) then
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
    begin
      raise EJclEDIError.CreateResRec(@EDIXMLError025);
    end;
  end;
  //Set next start positon
  StartPos := 1;
  //Determine the nearest tag to search for
  I := StrSearch(FDelimiters.BTD + XMLTag_Segment, FData, StartPos);
  J := StrSearch(FDelimiters.BTD + XMLTag_TransactionSetLoop, FData, StartPos);
  if (I < J) or (J <= 0) then
  begin
    SearchTag := XMLTag_Segment;
  end
  else
  begin
    SearchTag := XMLTag_TransactionSetLoop;
  end;
  //Search for Segments or Loops
  SearchResult := StrSearch(FDelimiters.BTD + SearchTag, FData, StartPos);
  StartPos := SearchResult;
  while SearchResult > 0 do
  begin
    if SearchTag = XMLTag_Segment then
    begin
      SearchResult := StrSearch(FDelimiters.BOfETD + SearchTag, FData, SearchResult);
      if SearchResult > 0 then
      begin
        SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
        if SearchResult > 0 then
        begin
          I := AddSegment; //Add Segment
          EDIDataObjects[I].Data :=
            Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.ETDLength));
          EDIDataObjects[I].Disassemble;
        end
        else
        begin
          raise EJclEDIError.CreateResRec(@EDIXMLError045);
        end;
      end
      else
      begin
        raise EJclEDIError.CreateResRec(@EDIXMLError044);
      end;
    end
    else
    begin
      NestedLoopCount := 0;
      SearchResult := StartPos;
      //Search for the proper end loop tag
      repeat
        I := StrSearch(FDelimiters.BOfETD + SearchTag, FData, SearchResult); //Find loop end
        J := StrSearch(FDelimiters.BTD + SearchTag, FData, SearchResult);    //Find loop begin
        if (I < J) or (J <= 0) then
        begin
          Dec(NestedLoopCount);
          SearchResult := I + FDelimiters.ETDLength;
        end
        else if (I > J) and (J > 0) then
        begin
          Inc(NestedLoopCount);
          SearchResult := J + FDelimiters.ETDLength;
        end;
      until (NestedLoopCount <= 0) or (I <= 0);
      SearchResult := I;
      //
      if SearchResult > 0 then
      begin
        SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
        if SearchResult > 0 then
        begin
          I := AddGroup; //Add Transaction Set Loop
          EDIDataObjects[I].Data :=
            Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.ETDLength));
          EDIDataObjects[I].Disassemble;
        end
        else
        begin
          raise EJclEDIError.CreateResRec(@EDIXMLError032);
        end;
      end
      else
      begin
        raise EJclEDIError.CreateResRec(@EDIXMLError031);
      end;
    end; //if SearchTag = XMLTag_Segment then
    //Set next start positon
    StartPos := SearchResult + FDelimiters.ETDLength;
    //Determine the nearest tag to search for
    I := StrSearch(FDelimiters.BTD + XMLTag_Segment, FData, StartPos);
    J := StrSearch(FDelimiters.BTD + XMLTag_TransactionSetLoop, FData, StartPos);
    if (I < J) or (J <= 0) then
    begin
      SearchTag := XMLTag_Segment;
    end
    else
    begin
      SearchTag := XMLTag_TransactionSetLoop;
    end;
    SearchResult := StrSearch(FDelimiters.BTD + SearchTag, FData, StartPos);
  end; //while SearchResult > 0 do

  if Length(FEDIDataObjects) > 0 then
  begin
    //Search for Transaction Set Header and Trailer
    FSTSegment := TEDIXMLSegment(FEDIDataObjects[0]);
    FSESegment := TEDIXMLSegment(FEDIDataObjects[High(FEDIDataObjects)]);

    if FSTSegment.Attributes.GetAttributeValue(XMLAttribute_Id) = XMLTag_TSHSegmentId then
    begin
      TempData := FEDIDataObjects[0].Assemble;
      FEDIDataObjects[0].Free;
      FEDIDataObjects[0] := nil;
      //
      FSTSegment := nil;
      FSTSegment := TEDIXMLTransactionSetSegment.Create(Self);
      FSTSegment.Data := TempData;
      FSTSegment.Disassemble;
      //
      FEDIDataObjects[0] := FSTSegment;
    end
    else
    begin
      FSTSegment := nil;
      raise EJclEDIError.CreateResRec(@EDIXMLError059);
    end;

    if FSESegment.Attributes.GetAttributeValue(XMLAttribute_Id) = XMLTag_TSTSegmentId then
    begin
      TempData := FEDIDataObjects[High(FEDIDataObjects)].Assemble;
      FEDIDataObjects[High(FEDIDataObjects)].Free;
      FEDIDataObjects[High(FEDIDataObjects)] := nil;
      //
      FSESegment := nil;
      FSESegment := TEDIXMLTransactionSetSegment.Create(Self);
      FSESegment.Data := TempData;
      FSESegment.Disassemble;
      //
      FEDIDataObjects[High(FEDIDataObjects)] := FSESegment;
    end
    else
    begin
      FSESegment := nil;
      raise EJclEDIError.CreateResRec(@EDIXMLError060);
    end;

  end
  else
  begin
    FSTSegment := nil;
    FSESegment := nil;
    raise EJclEDIError.CreateResRec(@EDIXMLError061);
  end;
  FData := '';
  //
  FState := ediDissassembled;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLTransactionSet.InternalAssignDelimiters: TEDIXMLDelimiters;
begin
  Result := nil;
  if not Assigned(FDelimiters) then //Attempt to assign the delimiters
  begin
    if Assigned(Parent) and (Parent is TEDIXMLFunctionalGroup) then
    begin
      if Assigned(Parent.Delimiters) then
      begin
        Result := Parent.Delimiters;
        Exit;
      end;
      if Assigned(Parent.Parent) and (Parent.Parent is TEDIXMLInterchangeControl) then
      begin
        if Assigned(Parent.Parent.Delimiters) then
        begin
          Result := Parent.Parent.Delimiters;
        end;
      end;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLTransactionSet.InternalCreateDataObjectGroup: TEDIXMLDataObjectGroup;
begin
  Result := TEDIXMLTransactionSetLoop.Create(Self);
end;

//--------------------------------------------------------------------------------------------------
// TEDIXMLFunctionalGroup
//--------------------------------------------------------------------------------------------------

{ TEDIXMLFunctionalGroup }

function TEDIXMLFunctionalGroup.Assemble: string;
var
  I: Integer;
  AttributeString: string;
begin
  FData := '';
  FLength := 0;
  Result := '';

  if not Assigned(FDelimiters) then //Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
    begin
      raise EJclEDIError.CreateResRec(@EDIXMLError016);
    end;
  end;

  AttributeString := FAttributes.CombineAttributes;
  if AttributeString <> '' then
  begin
    FData := FDelimiters.BTD + XMLTag_FunctionalGroup + FDelimiters.SpaceDelimiter +
             AttributeString + FDelimiters.ETD;
  end
  else
  begin
    FData := FDelimiters.BTD + XMLTag_FunctionalGroup + FDelimiters.ETD;
  end;

  if Length(FEDIDataObjects) > 0 then
  begin
    for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
    begin
      if Assigned(FEDIDataObjects[I]) then
      begin
        FData := FData + FEDIDataObjects[I].Assemble;
      end;
    end;
  end;
  FData := FData + FDelimiters.BOfETD + XMLTag_FunctionalGroup + FDelimiters.ETD;
  FLength := Length(FData);
  Result := FData; //Return assembled string

  DeleteEDIDataObjects;

  FState := ediAssembled;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIXMLFunctionalGroup.Create(Parent: TEDIXMLDataObject);
begin
  inherited;
  FEDIDOT := ediFunctionalGroup;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIXMLFunctionalGroup.Destroy;
begin
  inherited;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLFunctionalGroup.Disassemble;
var
  I, StartPos, SearchResult: Integer;
begin
  DeleteEDIDataObjects;
  //Check delimiter assignment
  if not Assigned(FDelimiters) then
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
    begin
      raise EJclEDIError.CreateResRec(@EDIXMLError015);
    end;
  end;
  //Search for Functional Group Header
  StartPos := 1;
  SearchResult := SearchForSegmentInDataString(XMLTag_FGHSegmentId, StartPos);
  if SearchResult > 0 then
  begin
    StartPos := SearchResult;
    SearchResult := StrSearch(FDelimiters.BOfETD + XMLTag_Segment, FData, SearchResult);
    if SearchResult > 0 then
    begin
      SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
      if SearchResult > 0 then
      begin
        FGSSegment := TEDIXMLFunctionalGroupSegment.Create(nil);
        AppendEDIDataObject(FGSSegment);
        FGSSegment.Data :=
          Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.ETDLength));
        FGSSegment.Disassemble;
      end
      else
      begin
        raise EJclEDIError.CreateResRec(@EDIXMLError021);
      end;
    end
    else
    begin
      raise EJclEDIError.CreateResRec(@EDIXMLError020);
    end;
  end
  else
  begin
    raise EJclEDIError.CreateResRec(@EDIXMLError019);
  end;
  //Set next start positon
  StartPos := SearchResult + FDelimiters.ETDLength;
  //Search for Transaction Set
  SearchResult := StrSearch(FDelimiters.BTD + XMLTag_TransactionSet, FData, StartPos);
  while SearchResult > 0 do
  begin
    StartPos := SearchResult;
    SearchResult := StrSearch(FDelimiters.BOfETD + XMLTag_TransactionSet, FData, SearchResult);
    if SearchResult > 0 then
    begin
      SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
      if SearchResult > 0 then
      begin
        I := AddGroup; //Add Transaction Set
        EDIDataObjects[I].Data :=
          Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.ETDLength));
        EDIDataObjects[I].Disassemble;
      end
      else
      begin
        raise EJclEDIError.CreateResRec(@EDIXMLError028);
      end;
    end
    else
    begin
      raise EJclEDIError.CreateResRec(@EDIXMLError027);
    end;
    //Set next start positon
    StartPos := SearchResult + FDelimiters.ETDLength;
    //Search for Transaction Set
    SearchResult := StrSearch(FDelimiters.BTD + XMLTag_TransactionSet, FData, StartPos);
  end;
  //Search for Functional Group Trailer
  SearchResult := SearchForSegmentInDataString(XMLTag_FGTSegmentId, StartPos);
  if SearchResult > 0 then
  begin
    StartPos := SearchResult;
    SearchResult := StrSearch(FDelimiters.BOfETD + XMLTag_Segment, FData, SearchResult);
    if SearchResult > 0 then
    begin
      SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
      if SearchResult > 0 then
      begin
        FGESegment := TEDIXMLFunctionalGroupSegment.Create(nil);
        AppendEDIDataObject(FGESegment);
        FGESegment.Data :=
          Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.ETDLength));
        FGESegment.Disassemble;
      end
      else
      begin
        raise EJclEDIError.CreateResRec(@EDIXMLError024);
      end;
    end
    else
    begin
      raise EJclEDIError.CreateResRec(@EDIXMLError023);
    end;
  end
  else
  begin
    raise EJclEDIError.CreateResRec(@EDIXMLError022);
  end;
  FData := '';
  //
  FState := ediDissassembled;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLFunctionalGroup.InternalAssignDelimiters: TEDIXMLDelimiters;
begin
  Result := nil;
  //Attempt to assign the delimiters
  if not Assigned(FDelimiters) then
  begin
    if Assigned(Parent) and (Parent is TEDIXMLInterchangeControl) then
    begin
      if Assigned(Parent.Delimiters) then
      begin
        Result := Parent.Delimiters;
      end;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLFunctionalGroup.InternalCreateDataObjectGroup: TEDIXMLDataObjectGroup;
begin
  Result := TEDIXMLTransactionSet.Create(Self);
end;

//--------------------------------------------------------------------------------------------------
// TEDIXMLInterchangeControl
//--------------------------------------------------------------------------------------------------

{ TEDIXMLInterchangeControl }

function TEDIXMLInterchangeControl.Assemble: string;
var
  I: Integer;
  AttributeString: string;
begin
  FData := '';
  FLength := 0;
  Result := '';

  if not Assigned(FDelimiters) then //Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
    begin
      raise EJclEDIError.CreateResRec(@EDIXMLError005);
    end;
  end;

  AttributeString := FAttributes.CombineAttributes;
  if AttributeString <> '' then
  begin
    FData := FDelimiters.BTD + XMLTag_InterchangeControl + FDelimiters.SpaceDelimiter +
             AttributeString + FDelimiters.ETD;
  end
  else
  begin
    FData := FDelimiters.BTD + XMLTag_InterchangeControl + FDelimiters.ETD;
  end;

  if Length(FEDIDataObjects) > 0 then
  begin
    for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
    begin
      if Assigned(FEDIDataObjects[I]) then
      begin
        FData := FData + FEDIDataObjects[I].Assemble;
      end;
    end;
  end;
  FData := FData + FDelimiters.BOfETD + XMLTag_InterchangeControl + FDelimiters.ETD;
  FLength := Length(FData);
  Result := FData; //Return assembled string

  DeleteEDIDataObjects;

  FState := ediAssembled;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIXMLInterchangeControl.Create(Parent: TEDIXMLDataObject);
begin
  inherited;
  FEDIDOT := ediInterchangeControl;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIXMLInterchangeControl.Destroy;
begin
  if Assigned(FDelimiters) then
  begin
    FDelimiters.Free;
    FDelimiters := nil;
  end;
  inherited;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLInterchangeControl.Disassemble;
var
  I, StartPos, SearchResult: Integer;
begin
  DeleteEDIDataObjects;
  //Check if delimiters are assigned
  if not Assigned(FDelimiters) then //Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
    begin
      raise EJclEDIError.CreateResRec(@EDIXMLError006);
    end;
  end;
  //Search for Interchange Control Header
  StartPos := 1;
  SearchResult := SearchForSegmentInDataString(XMLTag_ICHSegmentId, StartPos);
  if SearchResult > 0 then
  begin
    StartPos := SearchResult;
    SearchResult := StrSearch(FDelimiters.BOfETD + XMLTag_Segment, FData, SearchResult);
    if SearchResult > 0 then
    begin
      SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
      if SearchResult > 0 then
      begin
        FISASegment := TEDIXMLInterchangeControlSegment.Create(nil);
        AppendEDIDataObject(FISASegment);
        FISASegment.Data :=
          Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.ETDLength));
        FISASegment.Disassemble;
      end
      else
      begin
        raise EJclEDIError.CreateResRec(@EDIXMLError011);
      end;
    end
    else
    begin
      raise EJclEDIError.CreateResRec(@EDIXMLError010);
    end;
  end
  else
  begin
    raise EJclEDIError.CreateResRec(@EDIXMLError009);
  end;
  //Set next start position. Move past the delimiter
  StartPos := SearchResult + FDelimiters.ETDLength;
  //Search for Functional Group
  SearchResult := StrSearch(FDelimiters.BTD + XMLTag_FunctionalGroup, FData, StartPos);
  while SearchResult > 0 do
  begin
    StartPos := SearchResult;
    SearchResult := StrSearch(FDelimiters.BOfETD + XMLTag_FunctionalGroup, FData, SearchResult);
    if SearchResult > 0 then
    begin
      SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
      if SearchResult > 0 then
      begin
        I := AddGroup; //Add Functional Group
        EDIDataObjects[I].Data :=
          Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.ETDLength));
        EDIDataObjects[I].Disassemble;
      end
      else
      begin
        raise EJclEDIError.CreateResRec(@EDIXMLError018);
      end;
    end
    else
    begin
      raise EJclEDIError.CreateResRec(@EDIXMLError017);
    end;
    //Set next start positon
    StartPos := SearchResult + FDelimiters.ETDLength;
    //Search for Functional Group
    SearchResult := StrSearch(FDelimiters.BTD + XMLTag_FunctionalGroup, FData, StartPos);
  end;
  //Search for Interchange Control Trailer
  SearchResult := SearchForSegmentInDataString(XMLTag_ICTSegmentId, StartPos);
  if SearchResult > 0 then
  begin
    StartPos := SearchResult;
    SearchResult := StrSearch(FDelimiters.BOfETD + XMLTag_Segment, FData, SearchResult);
    if SearchResult > 0 then
    begin
      SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
      if SearchResult > 0 then
      begin
        FIEASegment := TEDIXMLInterchangeControlSegment.Create(nil);
        AppendEDIDataObject(FIEASegment);
        FIEASegment.Data :=
          Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.ETDLength));
        FIEASegment.Disassemble;
      end
      else
      begin
        raise EJclEDIError.CreateResRec(@EDIXMLError014);
      end;
    end
    else
    begin
      raise EJclEDIError.CreateResRec(@EDIXMLError013);
    end;
  end
  else
  begin
    raise EJclEDIError.CreateResRec(@EDIXMLError012);
  end;
  FData := '';
  //
  FState := ediDissassembled;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLInterchangeControl.InternalAssignDelimiters: TEDIXMLDelimiters;
begin
  Result := TEDIXMLDelimiters.Create;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLInterchangeControl.InternalCreateDataObjectGroup: TEDIXMLDataObjectGroup;
begin
  Result := TEDIXMLFunctionalGroup.Create(Self);
end;

//--------------------------------------------------------------------------------------------------
// TEDIXMLFile
//--------------------------------------------------------------------------------------------------

{ TEDIXMLFile }

function TEDIXMLFile.Assemble: string;
var
  I: Integer;
  AttributeString: string;
begin
  FData := '';
  FLength := 0;
  Result := '';

  if not Assigned(FDelimiters) then //Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
    begin
      raise EJclEDIError.CreateResRec(@EDIXMLError004);
    end;
  end;

  FData := FEDIXMLFileHeader.OutputXMLHeader;

  AttributeString := FAttributes.CombineAttributes;
  if AttributeString <> '' then
  begin
    FData := FData + FDelimiters.BTD + XMLTag_EDIFile + FDelimiters.SpaceDelimiter +
             AttributeString + FDelimiters.ETD;
  end
  else
  begin
    FData := FData + FDelimiters.BTD + XMLTag_EDIFile + FDelimiters.ETD;
  end;

  if Length(FEDIDataObjects) > 0 then
  begin
    for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
    begin
      if Assigned(FEDIDataObjects[I]) then
      begin
        FData := FData + FEDIDataObjects[I].Assemble;
      end;
    end;
  end;
  FData := FData + FDelimiters.BOfETD + XMLTag_EDIFile + FDelimiters.ETD;
  FLength := Length(FData);
  Result := FData; //Return assembled string

  DeleteEDIDataObjects;

  FState := ediAssembled;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIXMLFile.Create(Parent: TEDIXMLDataObject);
begin
  inherited;
  FEDIXMLFileHeader := TEDIXMLFileHeader.Create;
  FEDIDOT := ediFile;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIXMLFile.Destroy;
begin
  FEDIXMLFileHeader.Free;
  inherited;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLFile.Disassemble;
var
  I, StartPos, SearchResult: Integer;
  XMLHeader: string;
begin
  DeleteEDIDataObjects;
  //
  if not Assigned(FDelimiters) then //Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
    begin
      raise EJclEDIError.CreateResRec(@EDIXMLError003);
    end;
  end;
  //Search for XML file header
  StartPos := 1;
  SearchResult := StrSearch('<?', FData, StartPos);
  StartPos := SearchResult;
  if SearchResult > 0 then
  begin
    SearchResult := StrSearch('?>', FData, StartPos);
    if SearchResult > 0 then
    begin
      XMLHeader := Copy(FData, StartPos, ((SearchResult - StartPos) + Length('?>')));
      FEDIXMLFileHeader.ParseXMLHeader(XMLHeader);
    end
    else
    begin
      //Hey the header was not found
    end;
  end
  else
  begin
    //Hey the header was not found
  end;
  //Search for Interchange
  StartPos := 1;
  SearchResult := StrSearch(FDelimiters.BTD + XMLTag_InterchangeControl, FData, StartPos);
  StartPos := SearchResult;
  while SearchResult > 0 do
  begin
    //Search for Interchange end tag
    SearchResult := StrSearch(FDelimiters.BOfETD + XMLTag_InterchangeControl, FData, SearchResult);
    if SearchResult > 0 then
    begin
      //Search for Interchange end tag delimiter
      SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
      if SearchResult > 0 then
      begin
        I := AddGroup; //Add Interchange
        FEDIDataObjects[I].Delimiters := TEDIXMLDelimiters.Create;
        FEDIDataObjects[I].Data :=
          Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.ETDLength));
        FEDIDataObjects[I].Disassemble;
      end
      else
      begin
        raise EJclEDIError.CreateResRec(@EDIXMLError008);
      end;
    end
    else
    begin
      raise EJclEDIError.CreateResRec(@EDIXMLError007);
    end;
    //Set next start position. Move past the delimiter
    StartPos := SearchResult + FDelimiters.ETDLength;
    //Search for Interchange
    SearchResult := StrSearch(FDelimiters.BTD + XMLTag_InterchangeControl, FData, StartPos);
  end;
  FData := '';

  FState := ediDissassembled;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLFile.InternalAssignDelimiters: TEDIXMLDelimiters;
begin
  Result := TEDIXMLDelimiters.Create;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLFile.InternalCreateDataObjectGroup: TEDIXMLDataObjectGroup;
begin
  Result := TEDIXMLInterchangeControl.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLFile.InternalLoadFromFile;
var
  EDIFileStream: TFileStream;
begin
  FData := '';
  if FFileName <> '' then
  begin
    EDIFileStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
    try
      SetLength(FData, EDIFileStream.Size);
      EDIFileStream.Read(Pointer(FData)^, EDIFileStream.Size);
    finally
      EDIFileStream.Free;
    end;
    FData := StringReplace(FData, AnsiCrLf, '', [rfReplaceAll, rfIgnoreCase]);
  end
  else
  begin
    raise EJclEDIError.CreateResRec(@EDIXMLError001);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLFile.LoadFromFile(const FileName: string);
begin
  FFileName := FileName;
  InternalLoadFromFile;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLFile.ReLoadFromFile;
begin
  InternalLoadFromFile;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLFile.SaveAsToFile(const FileName: string);
var
  EDIFileStream: TFileStream;
begin
  FFileName := FileName;
  if FFileName <> '' then
  begin
    EDIFileStream := TFileStream.Create(FFileName, fmCreate or fmShareDenyNone);
    try
      EDIFileStream.Write(Pointer(FData)^, Length(FData));
    finally
      EDIFileStream.Free;
    end;
  end
  else
  begin
    raise EJclEDIError.CreateResRec(@EDIXMLError002);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLFile.SaveToFile;
var
  EDIFileStream: TFileStream;
begin
  if FFileName <> '' then
  begin
    EDIFileStream := TFileStream.Create(FFileName, fmCreate or fmShareDenyNone);
    try
      EDIFileStream.Write(Pointer(FData)^, Length(FData));
    finally
      EDIFileStream.Free;
    end;
  end
  else
  begin
    raise EJclEDIError.CreateResRec(@EDIXMLError002);
  end;
end;

//--------------------------------------------------------------------------------------------------
//  TEDIXMLFileHeader
//--------------------------------------------------------------------------------------------------

{ TEDIXMLFileHeader }

constructor TEDIXMLFileHeader.Create;
begin
  inherited;
  FAttributes := TEDIXMLAttributes.Create;
  FDelimiters := TEDIXMLDelimiters.Create;
  FAttributes.SetAttribute('version', '1.0');
  FAttributes.SetAttribute('encoding', 'windows-1252'); //ISO-8859-1
  FXMLNameSpaceOption := nsNone;
  FAttributes.SetAttribute('xmlns', 'EDITRANSDOC');
  FAttributes.SetAttribute('xmlns:EDI', 'EDITRANSDOC');
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIXMLFileHeader.Destroy;
begin
  FDelimiters.Free;
  FAttributes.Free;
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLFileHeader.OutputAdditionalXMLHeaderAttributes: string;
begin
  Result := '';
end;

//--------------------------------------------------------------------------------------------------

function TEDIXMLFileHeader.OutputXMLHeader: string;
var
  AdditionalAttributes: string;
begin
  Result := FDelimiters.BTD + '?xml' + Delimiters.SpaceDelimiter +
            FAttributes.GetAttributeString('version');
  case FXMLNameSpaceOption of
    nsNone:
    begin
      Result := Result + Delimiters.SpaceDelimiter + FAttributes.GetAttributeString('encoding');
    end;
    nsDefault:
    begin
      Result := Result +
        Delimiters.SpaceDelimiter + FAttributes.GetAttributeString('encoding') +
        Delimiters.SpaceDelimiter + FAttributes.GetAttributeString('xmlns');
    end;
    nsQualified:
    begin
      Result := Result +
        Delimiters.SpaceDelimiter + FAttributes.GetAttributeString('encoding') +
        Delimiters.SpaceDelimiter + FAttributes.GetAttributeString('xmlns:EDI');
    end;
  end; //case
  AdditionalAttributes := OutputAdditionalXMLHeaderAttributes;
  if AdditionalAttributes <> '' then
  begin
    Result := Result + Delimiters.SpaceDelimiter + AdditionalAttributes;
  end;
  Result := Result + '?' + FDelimiters.ETD;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIXMLFileHeader.ParseXMLHeader(XMLHeader: string);
begin
  FAttributes.ParseAttributes(XMLHeader);
end;

//--------------------------------------------------------------------------------------------------
// TEDIXMLANSIX12FormatTranslator
//--------------------------------------------------------------------------------------------------

{ TEDIXMLANSIX12FormatTranslator }

function TEDIXMLANSIX12FormatTranslator.ConvertToEDISegment(XMLSegment: TEDIXMLSegment): TEDISegment;
var
  ediE, xmlE: Integer;
begin
  if XMLSegment is TEDIXMLInterchangeControlSegment then
  begin
    Result := TEDIInterchangeControlSegment.Create(nil);
  end
  else if XMLSegment is TEDIXMLFunctionalGroupSegment then
  begin
    Result := TEDIFunctionalGroupSegment.Create(nil);
  end
  else if XMLSegment is TEDIXMLTransactionSetSegment then
  begin
    Result := TEDITransactionSetSegment.Create(nil);
  end
  else
  begin
    Result := TEDISegment.Create(nil);
  end;
  Result.SegmentID := XMLSegment.Attributes.GetAttributeValue(XMLAttribute_Id);
  for ediE := Low(XMLSegment.Elements) to High(XMLSegment.Elements) do
  begin
    xmlE := Result.AddElement;
    Result[xmlE].Data := XMLSegment[ediE].Data;
  end; //for ediE
end;

function TEDIXMLANSIX12FormatTranslator.ConvertToEDITransaction(
  XMLTransactionSet: TEDIXMLTransactionSet): TEDITransactionSet;
var
  I: Integer;
  EDISegment: TEDISegment;
  XMLSegment: TEDIXMLSegment;
  XMLLoop: TEDIXMLTransactionSetLoop;
begin
  Result := TEDITransactionSet.Create(nil);
  for I := Low(XMLTransactionSet.EDIDataObjects) to High(XMLTransactionSet.EDIDataObjects) do
  begin
    if XMLTransactionSet[I] is TEDIXMLSegment then
    begin
      XMLSegment := TEDIXMLSegment(XMLTransactionSet[I]);
      if XMLSegment.Attributes.GetAttributeValue(XMLAttribute_Id) = XMLTag_TSHSegmentId then
      begin
        EDISegment := ConvertToEDISegment(XMLSegment);
        Result.SegmentST := TEDITransactionSetSegment(EDISegment);
      end
      else if XMLSegment.Attributes.GetAttributeValue(XMLAttribute_Id) = XMLTag_TSTSegmentId then
      begin
        EDISegment := ConvertToEDISegment(XMLSegment);
        Result.SegmentSE := TEDITransactionSetSegment(EDISegment);
      end
      else
      begin
        EDISegment := ConvertToEDISegment(XMLSegment);
        Result.AppendSegment(EDISegment);
      end;
    end
    else if XMLTransactionSet[I] is TEDIXMLTransactionSetLoop then
    begin
      XMLLoop := TEDIXMLTransactionSetLoop(XMLTransactionSet[I]);
      ConvertTransactionSetLoopToEDI(Result, XMLLoop);
    end
    else
    begin
      raise EJclEDIError.CreateResRecFmt(@EDIXMLError062, [XMLTransactionSet[I].ClassName]);
    end;
  end;
end;

function TEDIXMLANSIX12FormatTranslator.ConvertToXMLSegment(EDISegment: TEDISegment): TEDIXMLSegment;
var
  ediE, xmlE: Integer;
begin
  if EDISegment is TEDIInterchangeControlSegment then
  begin
    Result := TEDIXMLInterchangeControlSegment.Create(nil);
  end
  else if EDISegment is TEDIFunctionalGroupSegment then
  begin
    Result := TEDIXMLFunctionalGroupSegment.Create(nil);
  end
  else if EDISegment is TEDITransactionSetSegment then
  begin
    Result := TEDIXMLTransactionSetSegment.Create(nil);
  end
  else
  begin
    Result := TEDIXMLSegment.Create(nil);
  end;
  Result.Attributes.SetAttribute(XMLAttribute_Id, EDISegment.SegmentID);
  for ediE := Low(EDISegment.Elements) to High(EDISegment.Elements) do
  begin
    xmlE := Result.AddElement;
    Result[xmlE].Data := EDISegment[ediE].Data;
  end; //for ediE
end;

function TEDIXMLANSIX12FormatTranslator.ConvertToXMLTransaction(
  EDITransactionSet: TEDITransactionSet;
  EDITransactionSetSpec: TEDITransactionSetSpec): TEDIXMLTransactionSet;
var
  EDIDoc: TEDITransactionSetDocument;
  XMLSegment: TEDIXMLSegment;
begin
  Result := TEDIXMLTransactionSet.Create(nil);
  EDIDoc := TEDITransactionSetDocument.Create(EDITransactionSet, EDITransactionSet,
                                              EDITransactionSetSpec);
  try
    EDIDoc.FormatDocument;

    XMLSegment := ConvertToXMLSegment(EDITransactionSet.SegmentST);
    Result.AppendEDIDataObject(XMLSegment);

    ConvertTransactionSetLoopToXML(EDIDoc, Result);

    XMLSegment := ConvertToXMLSegment(EDITransactionSet.SegmentSE);
    Result.AppendEDIDataObject(XMLSegment);

  finally
    EDIDoc.Free;
  end;
end;

function TEDIXMLANSIX12FormatTranslator.ConvertToXMLTransaction(
  EDITransactionSet: TEDITransactionSet): TEDIXMLTransactionSet;
var
  I: Integer;
  XMLSegment: TEDIXMLSegment;
begin
  Result := TEDIXMLTransactionSet.Create(nil);

  XMLSegment := ConvertToXMLSegment(EDITransactionSet.SegmentST);
  Result.AppendEDIDataObject(XMLSegment);

  for I := Low(EDITransactionSet.Segments) to High(EDITransactionSet.Segments) do
  begin
    XMLSegment := ConvertToXMLSegment(EDITransactionSet.Segment[I]);
    Result.AppendEDIDataObject(XMLSegment);
  end;

  XMLSegment := ConvertToXMLSegment(EDITransactionSet.SegmentSE);
  Result.AppendEDIDataObject(XMLSegment);
end;

procedure TEDIXMLANSIX12FormatTranslator.ConvertTransactionSetLoopToEDI(
  EDITransactionSet: TEDITransactionSet;
  XMLLoop: TEDIXMLTransactionSetLoop);
var
  I: Integer;
  EDISegment: TEDISegment;
  XMLSegment: TEDIXMLSegment;
  nXMLLoop: TEDIXMLTransactionSetLoop;
begin
  for I := Low(XMLLoop.EDIDataObjects) to High(XMLLoop.EDIDataObjects) do
  begin
    if XMLLoop[I] is TEDIXMLSegment then
    begin
      XMLSegment := TEDIXMLSegment(XMLLoop[I]);
      EDISegment := ConvertToEDISegment(XMLSegment);
      EDITransactionSet.AppendSegment(EDISegment);
    end
    else if XMLLoop[I] is TEDIXMLTransactionSetLoop then
    begin
      nXMLLoop := TEDIXMLTransactionSetLoop(XMLLoop[I]);
      ConvertTransactionSetLoopToEDI(EDITransactionSet, nXMLLoop);
    end
    else
    begin
      raise EJclEDIError.CreateResRecFmt(@EDIXMLError062, [XMLLoop[I].ClassName]);
    end;
  end;
end;

procedure TEDIXMLANSIX12FormatTranslator.ConvertTransactionSetLoopToXML(EDILoop: TEDITransactionSetLoop;
  XMLLoop: TEDIXMLTransactionSetLoop);
var
  I, xmlL: Integer;
  EDISegment: TEDISegment;
  XMLSegment: TEDIXMLSegment;
  nEDILoop: TEDITransactionSetLoop;
  nXMLLoop: TEDIXMLTransactionSetLoop;
begin
  for I := Low(EDILoop.EDIDataObjects) to High(EDILoop.EDIDataObjects) do
  begin
    if EDILoop[I] is TEDISegment then
    begin
      EDISegment := TEDISegment(EDILoop[I]);
      XMLSegment := ConvertToXMLSegment(EDISegment);
      XMLLoop.AppendEDIDataObject(XMLSegment);
    end
    else if EDILoop[I] is TEDITransactionSetLoop then
    begin
      nEDILoop := TEDITransactionSetLoop(EDILoop[I]);
      xmlL := XMLLoop.AddGroup;
      nXMLLoop := TEDIXMLTransactionSetLoop(XMLLoop[xmlL]);
      nXMLLoop.Attributes.SetAttribute(XMLAttribute_Id, nEDILoop.OwnerLoopId);
      ConvertTransactionSetLoopToXML(nEDILoop, nXMLLoop);
    end
    else
    begin
      raise EJclEDIError.CreateResRecFmt(@EDIXMLError062, [EDILoop[I].ClassName]);
    end;
  end;
end;

constructor TEDIXMLANSIX12FormatTranslator.Create;
begin
  inherited;
end;

destructor TEDIXMLANSIX12FormatTranslator.Destroy;
begin
  inherited;
end;

end.

