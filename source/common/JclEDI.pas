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
{ The Original Code is JclEDI.pas.                                                                 }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Contains classes to eaisly parse EDI documents and data. Variable delimiter detection allows     }
{ parsing of the file without knowledge of the standards at an Interchange level.  This allows     }
{ parsing and construction of EDI documents with different delimiters.  Various EDI  file errors   }
{ can also be detected.                                                                            }
{                                                                                                  }
{ Unit owner:    Raymond Alexander                                                                 }
{ Last modified: September 5, 2002                                                                 }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Notes:                                                                                           }
{                                                                                                  }
{ 9/30/2002 (R.A.)                                                                                 }
{                                                                                                  }
{   - Added new derived TEDIElementSpec & TEDISegmentSpec classes for                              }
{       EDI Specification Manager Demo (work in progress)                                          }
{       EDI Data Tree Demo (not yet released)                                                      }
{                                                                                                  }
{   - Added TEDIDataUsageType to TEDITransactionSet class.  This variable is used to control the   }
{     behavior of the TEDITransactionSet depending if is used to store data or specifications.     }
{                                                                                                  }
{   - TEDIFile now uses TFileStream File I/O instead of BlockRead/BlockWrite File I/O              }
{                                                                                                  }
{ 5/21/2002 (R.A.)                                                                                 }
{                                                                                                  }
{   - Re-Tested with ANSI X12 Documents                                                            }
{                                                                                                  }
{   - Just about all code has been revised to meet jedi guide lines                                }
{                                                                                                  }
{   - Error messages have been turned into constants                                               }
{      - Some error message constants repeat so they will have to be merged                        }
{      - Possibly change error constants to resourcestrings?                                       }
{                                                                                                  }
{   - Currently working on some help file information and a demo                                   }
{                                                                                                  }
{ 5/14/2002 (R.A.)                                                                                 }
{                                                                                                  }
{   - Tested with ANSI X12 Documents                                                               }
{                                                                                                  }
{ Additional Notes:                                                                                }
{                                                                                                  }
{   - Full variable delimiter length support is not available yet.  This option                    }
{     is intended to parse segments that are delimited by #13#10.                                  }
{                                                                                                  }
{                                                                                                  }
{**************************************************************************************************}

unit JclEDI;

{$I jcl.inc}

{$WEAKPACKAGEUNIT ON}

interface

uses
  SysUtils, Classes, JclBase, JclStrings;

const
  ICHSegmentId = 'ISA'; //Interchange Control Header Segment Id
  ICTSegmentId = 'IEA'; //Interchange Control Trailer Segment Id
  FGHSegmentId = 'GS';  //Functional Group Header Segment Id
  FGTSegmentId = 'GE';  //Functional Group Trailer Segment Id
  TSHSegmentId = 'ST';  //Transaction Set Header Segment Id
  TSTSegmentId = 'SE';  //Transaction Set Trailer Segment Id

type
  TEDIObject = class(TObject); //Base EDI Object
  EJclEDIError = EJclError;
  TEDIDataObjectType = (ediUnknown, ediElement, ediSegment, ediTransactionSet, ediFunctionalGroup,
    ediInterchangeControl, ediFile, ediCustom);
  TEDIDataUsageType = (ediUseAsData, ediUseAsSpec);
  //TODO:  Work in progress
  //TEDIStandardType = (stCustom, stANSIX12, stEDIFACT);

  TEDIDataObject = class;
  TEDIElement = class;
  TEDISegment = class;
  TEDITransactionSet = class;
  TEDIFunctionalGroup = class;
  TEDIInterchangeControl = class;
  TEDIFile = class;

//--------------------------------------------------------------------------------------------------
//  EDI Delimiters Object
//--------------------------------------------------------------------------------------------------

  TEDIDelimiters = class(TObject)
  private
    FSegmentDelimiter: string;
    FElementDelimiter: string;
    FSubElementSeperator: string;
    FSegmentDelimiterLength: Integer;
    FElementDelimiterLength: Integer;
    FSubelementSeperatorLength: Integer;
    //Subelement Seperator
    procedure SetSD(const Delimiter: string); //Segment Delimiter
    procedure SetED(const Delimiter: string); //Element Delimiter
    procedure SetSS(const Delimiter: string); //Sub Element Seperator
  public
    constructor Create; overload;
    constructor Create(const SD, ED, SS: string); overload;
    property SD: string read FSegmentDelimiter write SetSD;
    property ED: string read FElementDelimiter write SetED;
    property SS: string read FSubElementSeperator write SetSS;
    property SDLen: Integer read FSegmentDelimiterLength;
    property EDLen: Integer read FElementDelimiterLength;
    property SSLen: Integer read FSubElementSeperatorLength;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Data Object
//--------------------------------------------------------------------------------------------------

  TEDIObjectArray = array of TEDIObject;

  TEDIDataObjectArray = array of TEDIDataObject;

  TEDIDataObject = class(TEDIObject) //EDI Data Object Base Class
  protected
    FEDIDOT: TEDIDataObjectType;
    FData: string;    //Raw Data
    FLength: Integer; //Length (of data)
    FParent: TEDIDataObject;
    FDelimiters: TEDIDelimiters;
    FErrorLog: TStrings;
    function GetData: string;
    procedure SetData(const Data: string);
  public
    constructor Create(Parent: TEDIDataObject); reintroduce;
    destructor Destroy; override;
    property Data: string read GetData write SetData;
    property DataLength: Integer read FLength;
    property Parent: TEDIDataObject read FParent write FParent;
    property Delimiters: TEDIDelimiters read FDelimiters write FDelimiters;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Element
//--------------------------------------------------------------------------------------------------

  TEDIElementArray = array of TEDIElement;

  TEDIElement = class(TEDIDataObject)
  public
    constructor Create(Parent: TEDIDataObject); reintroduce;
    function Assemble: string; virtual;
    procedure Dissassemble; virtual;
  end;

  TEDIElementSpec = class(TEDIElement)
  private
    FId: string;
    FPosition: Integer;
    FDescription: string;
    FRequirementDesignator: string;
    FType: string;
    FMinimumLength: Integer;
    FMaximumLength: Integer;
  public
    function Assemble: string; override;
    procedure Dissassemble; override;
    property Id: string read FId write FId;
    property Position: Integer read FPosition write FPosition;
    property Description: string read FDescription write FDescription;
    property RequirementDesignator: string read FRequirementDesignator write FRequirementDesignator;
    property ElementType: string read FType write FType;
    property MinimumLength: Integer read FMinimumLength write FMinimumLength;
    property MaximumLength: Integer read FMaximumLength write FMaximumLength;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Segment
//--------------------------------------------------------------------------------------------------

  TEDISegmentArray = array of TEDISegment;

  TEDISegment = class(TEDIDataObject)
  private
    FSegmentID: string;          //Specification: Segment ID
    FElements: TEDIElementArray; //Dissassembled raw data
    function GetElement(Index: Integer): TEDIElement;
    procedure SetElement(Index: Integer; Element: TEDIElement);
  protected
    function InternalAssignDelimiters: TEDIDelimiters; virtual;
  public
    constructor Create(Parent: TEDIDataObject); reintroduce; overload;
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer); overload;
    destructor Destroy; override;
    //
    function AddElement: Integer; virtual;
    function AppendElement(Element: TEDIElement): Integer;
    function InsertElement(InsertIndex: Integer): Integer; overload; virtual;
    function InsertElement(InsertIndex: Integer; Element: TEDIElement): Integer; overload;
    procedure DeleteElement(Index: Integer);
    //
    function AddElements(Count: Integer): Integer; virtual;
    function AppendElements(ElementArray: TEDIElementArray): Integer;
    function InsertElements(InsertIndex, Count: Integer): Integer; overload; virtual;
    function InsertElements(InsertIndex: Integer;
      ElementArray: TEDIElementArray): Integer; overload;
    procedure DeleteElements; overload;
    procedure DeleteElements(Index, Count: Integer); overload;
    //
    function Assemble: string; virtual;
    procedure Dissassemble; virtual;
    property SegmentID: string read FSegmentID write FSegmentID;
    property Elements: TEDIElementArray read FElements write FElements;
    property Element[Index: Integer]: TEDIElement read GetElement write SetElement; default;
  end;

  TEDISegmentSpec = class(TEDISegment)
  private
    FPosition: Integer;
    FDescription: string;
    FSection: string;
    FRequirementDesignator: string;
    FMaximumUsage: Integer;
    FOwnerLoopId: string;
    FParentLoopId: string;
  public
    function AddElement: Integer; override;
    function InsertElement(InsertIndex: Integer): Integer; override;
    function AddElements(Count: Integer): Integer; override;
    function InsertElements(InsertIndex, Count: Integer): Integer; override;

    function Assemble: string; override;
    procedure Dissassemble; override;
    property Position: Integer read FPosition write FPosition;
    property Description: string read FDescription write FDescription;
    property Section: string read FSection write FSection;
    property RequirementDesignator: string read FRequirementDesignator write FRequirementDesignator;
    property MaximumUsage: Integer read FMaximumUsage write FMaximumUsage;
    property OwnerLoopId: string read FOwnerLoopId write FOwnerLoopId;
    property ParentLoopId: string read FParentLoopId write FParentLoopId;
  end;

  TEDIInterchangeControlSegment = class(TEDISegment)
  public
    constructor Create(Parent: TEDIDataObject); reintroduce; overload;
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer); overload;
    function InternalAssignDelimiters: TEDIDelimiters; override;
  end;

  TEDIInterchangeControlSegmentSpec = class(TEDISegmentSpec)
  public
    constructor Create(Parent: TEDIDataObject); reintroduce; overload;
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer); overload;
    function InternalAssignDelimiters: TEDIDelimiters; override;
  end;

  TEDIFunctionalGroupSegment = class(TEDISegment)
  public
    constructor Create(Parent: TEDIDataObject); reintroduce; overload;
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer); overload;
    function InternalAssignDelimiters: TEDIDelimiters; override;
  end;

  TEDIFunctionalGroupSegmentSpec = class(TEDISegmentSpec)
  public
    constructor Create(Parent: TEDIDataObject); reintroduce; overload;
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer); overload;
    function InternalAssignDelimiters: TEDIDelimiters; override;
  end;

  TEDITransactionSetSegment = class(TEDISegment)
  public
    constructor Create(Parent: TEDIDataObject); reintroduce; overload;
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer); overload;
    function InternalAssignDelimiters: TEDIDelimiters; override;
  end;

  TEDITransactionSetSegmentSpec = class(TEDISegmentSpec)
  public
    constructor Create(Parent: TEDIDataObject); reintroduce; overload;
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer); overload;
    function InternalAssignDelimiters: TEDIDelimiters; override;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Transaction Set
//--------------------------------------------------------------------------------------------------

  TEDITransactionSetArray = array of TEDITransactionSet;

  TEDITransactionSet = class(TEDIDataObject)
  private
    FSTSegment: TEDITransactionSetSegment;
    FSegments: TEDISegmentArray; //Dissassembled raw data
    FSESegment: TEDITransactionSetSegment;
    function GetSegment(Index: Integer): TEDISegment;
    procedure SetSegment(Index: Integer; Segment: TEDISegment);
    function InternalAssignDelimiters: TEDIDelimiters;
  public
    constructor Create(Parent: TEDIDataObject); reintroduce; overload;
    constructor Create(Parent: TEDIDataObject; SegmentCount: Integer); overload;
    destructor Destroy; override;

    function AddSegment: Integer;
    function AppendSegment(Segment: TEDISegment): Integer;
    function InsertSegment(InsertIndex: Integer): Integer; overload;
    function InsertSegment(InsertIndex: Integer; Segment: TEDISegment): Integer; overload;
    procedure DeleteSegment(Index: Integer);

    function AddSegments(Count: Integer): Integer;
    function AppendSegments(SegmentArray: TEDISegmentArray): Integer;
    function InsertSegments(InsertIndex, Count: Integer): Integer; overload;
    function InsertSegments(InsertIndex: Integer;
      SegmentArray: TEDISegmentArray): Integer; overload;
    procedure DeleteSegments; overload;
    procedure DeleteSegments(Index, Count: Integer); overload;

    function Assemble: string;
    procedure Dissassemble;
    property SegmentST: TEDITransactionSetSegment read FSTSegment write FSTSegment;
    property SegmentSE: TEDITransactionSetSegment read FSESegment write FSESegment;
    property Segments: TEDISegmentArray read FSegments write FSegments;
    property Segment[Index: Integer]: TEDISegment read GetSegment write SetSegment; default;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Functional Group
//--------------------------------------------------------------------------------------------------

  TEDIFunctionalGroupArray = array of TEDIFunctionalGroup;

  TEDIFunctionalGroup = class(TEDIDataObject)
  private
    FGSSegment: TEDIFunctionalGroupSegment;
    FTransactionSets: TEDITransactionSetArray; //Dissassembled raw data
    FGESegment: TEDIFunctionalGroupSegment;
    function GetTransactionSet(Index: Integer): TEDITransactionSet;
    procedure SetTransactionSet(Index: Integer; TransactionSet: TEDITransactionSet);
    function InternalAssignDelimiters: TEDIDelimiters;
  public
    constructor Create(Parent: TEDIDataObject); reintroduce; overload;
    constructor Create(Parent: TEDIDataObject; TransactionSetCount: Integer); overload;
    destructor Destroy; override;

    function AddTransactionSet: Integer;
    function AppendTransactionSet(TransactionSet: TEDITransactionSet): Integer;
    function InsertTransactionSet(InsertIndex: Integer): Integer; overload;
    function InsertTransactionSet(InsertIndex: Integer;
      TransactionSet: TEDITransactionSet): Integer; overload;
    procedure DeleteTransactionSet(Index: Integer);

    function AddTransactionSets(Count: Integer): Integer;
    function AppendTransactionSets(TransactionSetArray: TEDITransactionSetArray): Integer;
    function InsertTransactionSets(InsertIndex, Count: Integer): Integer; overload;
    function InsertTransactionSets(InsertIndex: Integer;
      TransactionSetArray: TEDITransactionSetArray): Integer; overload;
    procedure DeleteTransactionSets; overload;
    procedure DeleteTransactionSets(Index, Count: Integer); overload;

    function Assemble: string;
    procedure Dissassemble;
    property SegmentGS: TEDIFunctionalGroupSegment read FGSSegment write FGSSegment;
    property SegmentGE: TEDIFunctionalGroupSegment read FGESegment write FGESegment;
    property TransactionSets: TEDITransactionSetArray read FTransactionSets write FTransactionSets;
    property TransactionSet[Index: Integer]: TEDITransactionSet read GetTransactionSet
      write SetTransactionSet; default;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Interchange Control
//--------------------------------------------------------------------------------------------------

  TEDIInterchangeControlArray = array of TEDIInterchangeControl;

  TEDIInterchangeControl = class(TEDIDataObject)
  private
    FISASegment: TEDIInterchangeControlSegment;
    FFunctionalGroups: TEDIFunctionalGroupArray; //Dissassembled raw data
    FIEASegment: TEDIInterchangeControlSegment;
    FEDIDUT: TEDIDataUsageType;    
    function GetFunctionalGroup(Index: Integer): TEDIFunctionalGroup;
    procedure SetFunctionalGroup(Index: Integer; FunctionalGroup: TEDIFunctionalGroup);
  public
    constructor Create(Parent: TEDIDataObject; EDIDUT: TEDIDataUsageType = ediUseAsData);
      reintroduce; overload;
    constructor Create(Parent: TEDIDataObject; FunctionalGroupCount: Integer;
      EDIDUT: TEDIDataUsageType = ediUseAsData); overload;
    destructor Destroy; override;

    function AddFunctionalGroup: Integer;
    function AppendFunctionalGroup(FunctionalGroup: TEDIFunctionalGroup): Integer;
    function InsertFunctionalGroup(InsertIndex: Integer): Integer; overload;
    function InsertFunctionalGroup(InsertIndex: Integer;
      FunctionalGroup: TEDIFunctionalGroup): Integer; overload;
    procedure DeleteFunctionalGroup(Index: Integer);

    function AddFunctionalGroups(Count: Integer): Integer;
    function AppendFunctionalGroups(FunctionalGroupArray: TEDIFunctionalGroupArray): Integer;
    function InsertFunctionalGroups(InsertIndex, Count: Integer): Integer; overload;
    function InsertFunctionalGroups(InsertIndex: Integer;
      FunctionalGroupArray: TEDIFunctionalGroupArray): Integer; overload;
    procedure DeleteFunctionalGroups; overload;
    procedure DeleteFunctionalGroups(Index, Count: Integer); overload;

    function Assemble: string;
    procedure Dissassemble;
    property SegmentISA: TEDIInterchangeControlSegment read FISASegment write FISASegment;
    property SegmentIEA: TEDIInterchangeControlSegment read FIEASegment write FIEASegment;
    property FunctionalGroups: TEDIFunctionalGroupArray read FFunctionalGroups
      write FFunctionalGroups;
    property FunctionalGroup[Index: Integer]: TEDIFunctionalGroup read GetFunctionalGroup
      write SetFunctionalGroup; default;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI File
//--------------------------------------------------------------------------------------------------

  TEDIFileArray = array of TEDIFile;

  TEDIFileOptions = set of (foVariableDelimiterDetection //Variable Delimiter Detection
                           );

  TEDIFile = class(TEDIDataObject)
  private
    FFileID: Integer;
    FFileName: string;
    FInterchanges: TEDIInterchangeControlArray;
    FEDIFileOptions: TEDIFileOptions;
    //FErrorLog: TStrings;
    function GetInterchangeControl(Index: Integer): TEDIInterchangeControl;
    procedure SetInterchangeControl(Index: Integer; Interchange: TEDIInterchangeControl);
    procedure InternalLoadFromFile;
  public
    constructor Create(Parent: TEDIDataObject); reintroduce; overload;
    constructor Create(Parent: TEDIDataObject; InterchangeCount: Integer); overload;
    destructor Destroy; override;

    procedure LoadFromFile(const FileName: string);
    procedure ReLoadFromFile;
    procedure SaveToFile;
    procedure SaveAsToFile(const FileName: string);

    function AddInterchange: Integer;
    function AppendInterchange(Interchange: TEDIInterchangeControl): Integer;
    function InsertInterchange(InsertIndex: Integer): Integer; overload;
    function InsertInterchange(InsertIndex: Integer;
      Interchange: TEDIInterchangeControl): Integer; overload;
    procedure DeleteInterchange(Index: Integer);

    function AddInterchanges(Count: Integer): Integer; overload;
    function AppendInterchanges(
      InterchangeControlArray: TEDIInterchangeControlArray): Integer; overload;
    function InsertInterchanges(InsertIndex, Count: Integer): Integer; overload;
    function InsertInterchanges(InsertIndex: Integer;
      InterchangeControlArray: TEDIInterchangeControlArray): Integer; overload;
    procedure DeleteInterchanges; overload;
    procedure DeleteInterchanges(Index, Count: Integer); overload;

    function Assemble: string;
    procedure Dissassemble;
    property FileID: Integer read FFileID write FFileID;
    property FileName: string read FFileName write FFileName;
    property Interchanges: TEDIInterchangeControlArray read FInterchanges write FInterchanges;
    property Interchange[Index: Integer]: TEDIInterchangeControl read GetInterchangeControl
      write SetInterchangeControl; default;
    property Options: TEDIFileOptions read FEDIFileOptions write FEDIFileOptions;
  end;

//--------------------------------------------------------------------------------------------------
//  Other
//--------------------------------------------------------------------------------------------------

implementation

uses
  JclResources;

//==================================================================================================
// TEDIDelimiters
//==================================================================================================

constructor TEDIDelimiters.Create;
begin
  inherited Create;
  FSegmentDelimiter := '';
  FElementDelimiter := '';
  FSubElementSeperator := '';
  FSegmentDelimiterLength := 0;
  FElementDelimiterLength := 0;
  FSubelEmentSeperatorLength := 0;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIDelimiters.Create(const SD, ED, SS: string);
begin
  inherited Create;
  FSegmentDelimiter := SD;
  FElementDelimiter := ED;
  FSubElementSeperator := SS;
  FSegmentDelimiterLength := Length(SD);
  FElementDelimiterLength := Length(ED);
  FSubelementSeperatorLength := Length(SS);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIDelimiters.SetED(const Delimiter: string);
begin
  FElementDelimiter := Delimiter;
  FElementDelimiterLength := Length(FElementDelimiter);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIDelimiters.SetSD(const Delimiter: string);
begin
  FSegmentDelimiter := Delimiter;
  FSegmentDelimiterLength := Length(FSegmentDelimiter);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIDelimiters.SetSS(const Delimiter: string);
begin
  FSubelementSeperator := Delimiter;
  FSubelementSeperatorLength := Length(FSubElementSeperator);
end;

//==================================================================================================
// TEDIDataObject
//==================================================================================================

constructor TEDIDataObject.Create(Parent: TEDIDataObject);
begin
  inherited Create;
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
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIDataObject.Destroy;
begin
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

function TEDIDataObject.GetData: string;
begin
  Result := FData;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIDataObject.SetData(const Data: string);
begin
  FData := Data;
  FLength := Length(FData);
end;

//==================================================================================================
// TEDIElement
//==================================================================================================

constructor TEDIElement.Create(Parent: TEDIDataObject);
begin
  if Assigned(Parent) and (Parent is TEDISegment) then
  begin
    inherited Create(Parent);
  end
  else
  begin
    inherited Create(nil);
  end;
  FEDIDOT := ediElement;
end;

function TEDIElement.Assemble: string;
begin
  Result := FData;
end;

procedure TEDIElement.Dissassemble;
begin
  //Override in decendant class
end;

//==================================================================================================
// TEDISegment
//==================================================================================================

function TEDISegment.AddElements(Count: Integer): Integer;
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
    FElements[J]:= TEDIElement.Create(Self);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.AddElement: Integer;
begin
  SetLength(FElements, Length(FElements) + 1);
  FElements[High(FElements)] := TEDIElement.Create(Self);
  Result := High(FElements); //Return position of element
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.AppendElement(Element: TEDIElement): Integer;
begin
  SetLength(FElements, Length(FElements) + 1);
  FElements[High(FElements)] := Element;
  Element.Parent := Self;
  Result := High(FElements); //Return position of element
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.AppendElements(ElementArray: TEDIElementArray): Integer;
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

function TEDISegment.Assemble: string;
var
  I: Integer;
begin
  FData := '';
  FLength := 0;
  Result := '';

  if not Assigned(FDelimiters) then //Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
    begin
      raise EJclEDIError.CreateResRec(@EDIError074);
    end;
  end;

  FData := FSegmentID;
  if Length(FElements) > 0 then
  begin
    for I := Low(FElements) to High(FElements) do
    begin
      if Assigned(FElements[I]) then
      begin
        FData := FData + FDelimiters.ED + FElements[I].Assemble;
      end
      else
      begin
        FData := FData + FDelimiters.ED;
      end;
    end;
  end;
  FData := FData + FDelimiters.SD;
  FLength := Length(FData);
  Result := FData; //Return assembled string

  DeleteElements;
end;

//-------------------------------------------------------------------------------------------------

constructor TEDISegment.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  if Assigned(Parent) and (Parent is TEDITransactionSet) then
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

constructor TEDISegment.Create(Parent: TEDIDataObject);
begin
  if Assigned(Parent) and (Parent is TEDITransactionSet) then
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

procedure TEDISegment.DeleteElement(Index: Integer);
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
    raise EJclEDIError.CreateResRecFmt(@EDIError073, [IntToStr(Index)]);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISegment.DeleteElements(Index, Count: Integer);
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
    raise EJclEDIError.CreateResRecFmt(@EDIError072, [IntToStr(Index)]);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISegment.DeleteElements;
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

destructor TEDISegment.Destroy;
begin
  DeleteElements;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISegment.Dissassemble;
var
  I, StartPos, SearchResult: Integer;
begin
//Data Input Scenarios
//4.)  SegID*---*---~
//Composite Element Data Input Secnarios
//9.)  SegID*---*--->---~
  FSegmentID := '';
  DeleteElements;
  if not Assigned(FDelimiters) then //Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
    begin
      raise EJclEDIError.CreateResRec(@EDIError071);
    end;
  end;
  //Continue
  StartPos := 1;
  SearchResult := StrSearch(FDelimiters.ED, FData, StartPos);
  FSegmentID := Copy(FData, 1, SearchResult - 1);
  StartPos := SearchResult + 1;
  SearchResult := StrSearch(FDelimiters.ED, FData, StartPos);
  while SearchResult <> 0 do
  begin
    I := AddElement;
    if ((SearchResult - StartPos) > 0) then //data exists
    begin
      FElements[I].Data := Copy(FData, ((StartPos + FDelimiters.EDLen) - 1),
        (SearchResult - StartPos));
      FElements[I].Dissassemble;
    end;
    StartPos := SearchResult + 1;
    SearchResult := StrSearch(FDelimiters.ED, FData, StartPos);
  end;
  //Get last element before next segment
  SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
  if SearchResult <> 0 then
  begin
    if ((SearchResult - StartPos) > 0) then //data exists
    begin
      I := AddElement;
      FElements[I].Data := Copy(FData, ((StartPos + FDelimiters.EDLen) - 1),
        (SearchResult - StartPos));
      FElements[I].Dissassemble;
    end;
  end;
  FData := '';
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.GetElement(Index: Integer): TEDIElement;
begin
  Result := nil;
  if (Length(FElements) > 0) then
    if (Index >= Low(FElements)) then
      if (Index <= High(FElements)) then
      begin
        if not Assigned(FElements[Index]) then
        begin
          raise EJclEDIError.CreateResRecFmt(@EDIError067, [IntToStr(Index)]);
        end;
        Result := FElements[Index];
      end
      else
        raise EJclEDIError.CreateResRecFmt(@EDIError068, [IntToStr(Index)])
    else
      raise EJclEDIError.CreateResRecFmt(@EDIError069, [IntToStr(Index)])
  else
    raise EJclEDIError.CreateResRecFmt(@EDIError070, [IntToStr(Index)]);
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.InsertElement(InsertIndex: Integer): Integer;
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
    FElements[InsertIndex] := TEDIElement.Create(Self);
  end
  else
  begin
    Result := AddElement;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.InsertElement(InsertIndex: Integer; Element: TEDIElement): Integer;
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

function TEDISegment.InsertElements(InsertIndex: Integer; ElementArray: TEDIElementArray): Integer;
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

function TEDISegment.InsertElements(InsertIndex, Count: Integer): Integer;
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
      FElements[I] := TEDIElement.Create(Self);
    end;
  end
  else
  begin
    Result := AddElements(Count);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  if not Assigned(FDelimiters) then //Attempt to assign the delimiters
  begin
    //Get the delimiters from the transaction set
    if Assigned(Parent) and (Parent is TEDITransactionSet) then
    begin
      if Assigned(Parent.Delimiters) then
      begin
        Result := Parent.Delimiters;
        Exit;
      end;
      //Get the delimiters from the functional group
      if Assigned(Parent.Parent) and (Parent.Parent is TEDIFunctionalGroup) then
      begin
        if Assigned(Parent.Parent.Delimiters) then
        begin
          Result := Parent.Parent.Delimiters;
          Exit;
        end;
        //Get the delimiters from the interchange control header
        if Assigned(Parent.Parent.Parent) and (Parent.Parent.Parent is TEDIInterchangeControl) then
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

procedure TEDISegment.SetElement(Index: Integer; Element: TEDIElement);
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
        raise EJclEDIError.CreateResRecFmt(@EDIError064, [IntToStr(Index)])
    else
      raise EJclEDIError.CreateResRecFmt(@EDIError065, [IntToStr(Index)])
  else
    raise EJclEDIError.CreateResRecFmt(@EDIError066, [IntToStr(Index)]);
end;

//==================================================================================================
// TEDITransactionSet
//==================================================================================================

function TEDITransactionSet.AddSegment: Integer;
begin
  SetLength(FSegments, Length(FSegments) + 1);
  if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
  begin
    if Assigned(Parent.Parent) and (Parent.Parent is TEDIInterchangeControl) then
    begin
      if TEDIInterchangeControl(Parent.Parent).FEDIDUT = ediUseAsSpec then
      begin
        FSegments[High(FSegments)] := TEDISegmentSpec.Create(Self);
      end
      else
      begin
        FSegments[High(FSegments)] := TEDISegment.Create(Self);
      end;
    end
    else
    begin
      FSegments[High(FSegments)] := TEDISegment.Create(Self);
    end;
  end
  else
  begin
    FSegments[High(FSegments)] := TEDISegment.Create(Self);
  end;
  Result := High(FSegments);
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSet.AddSegments(Count: Integer): Integer;
var
  I, J: Integer;
begin
  I := Length(FSegments);
  Result := I;
  //Resize
  SetLength(FSegments, Length(FSegments) + Count);
  //Add
  for J := I to High(FSegments) do
  begin
    if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
    begin
      if Assigned(Parent.Parent) and (Parent.Parent is TEDIInterchangeControl) then
      begin
        if TEDIInterchangeControl(Parent.Parent).FEDIDUT = ediUseAsSpec then
        begin
          FSegments[J]:= TEDISegmentSpec.Create(Self);
        end
        else
        begin
          FSegments[J]:= TEDISegment.Create(Self);
        end;
      end
      else
      begin
        FSegments[J]:= TEDISegment.Create(Self);
      end;
    end
    else
    begin
      FSegments[J]:= TEDISegment.Create(Self);
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSet.AppendSegment(Segment: TEDISegment): Integer;
begin
  SetLength(FSegments, Length(FSegments) + 1);
  FSegments[High(FSegments)] := Segment;
  Segment.Parent := Self;
  Result := High(FSegments);
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSet.AppendSegments(SegmentArray: TEDISegmentArray): Integer;
var
  J, K, I: Integer;
begin
  I := 0;
  J := Length(FSegments);
  Result := J;
  //Resize
  SetLength(FSegments, Length(FSegments) + Length(SegmentArray));
  //Append
  for K := J to High(SegmentArray) do
  begin
    FSegments[K] := SegmentArray[I];
    FSegments[K].Parent := Self;
    Inc(I);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSet.Assemble: string;
var
  I: Integer;
begin
  FData := '';
  FLength := 0;
  Result := '';
  if not Assigned(FDelimiters) then //Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
    begin
      raise EJclEDIError.CreateResRec(@EDIError063);
    end;
  end;

  FData := FSTSegment.Assemble;
  FSTSegment.Data := '';

  if Length(FSegments) > 0 then
  begin
    for I := Low(FSegments) to High(FSegments) do
    begin
      if Assigned(FSegments[I]) then
      begin
        FData := FData + FSegments[I].Assemble;
      end;
    end;
  end;

  DeleteSegments;

  FData := FData + FSESegment.Assemble;
  FSESegment.Data := '';

  FLength := Length(FData);
  Result := FData;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDITransactionSet.Create(Parent: TEDIDataObject; SegmentCount: Integer);
begin
  if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
  begin
    inherited Create(Parent);
  end
  else
  begin
    inherited Create(nil);
  end;
  FEDIDOT := ediTransactionSet;
  if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
  begin
    if Assigned(Parent.Parent) and (Parent.Parent is TEDIInterchangeControl) then
    begin
      if TEDIInterchangeControl(Parent.Parent).FEDIDUT = ediUseAsSpec then
      begin
        FSTSegment := TEDITransactionSetSegment(TEDITransactionSetSegmentSpec.Create(Self));
        FSESegment := TEDITransactionSetSegment(TEDITransactionSetSegmentSpec.Create(Self));
      end
      else
      begin
        FSTSegment := TEDITransactionSetSegment.Create(Self);
        FSESegment := TEDITransactionSetSegment.Create(Self);
      end;
    end
    else
    begin
      FSTSegment := TEDITransactionSetSegment.Create(Self);
      FSESegment := TEDITransactionSetSegment.Create(Self);
    end;
  end
  else
  begin
    FSTSegment := TEDITransactionSetSegment.Create(Self);
    FSESegment := TEDITransactionSetSegment.Create(Self);
  end;
  SetLength(FSegments, 0);
  AddSegments(SegmentCount);
end;

//--------------------------------------------------------------------------------------------------

constructor TEDITransactionSet.Create(Parent: TEDIDataObject);
begin
  if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
  begin
    inherited Create(Parent);
  end
  else
  begin
    inherited Create(nil);
  end;
  FEDIDOT := ediTransactionSet;
  if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
  begin
    if Assigned(Parent.Parent) and (Parent.Parent is TEDIInterchangeControl) then
    begin
      if TEDIInterchangeControl(Parent.Parent).FEDIDUT = ediUseAsSpec then
      begin
        FSTSegment := TEDITransactionSetSegment(TEDITransactionSetSegmentSpec.Create(Self));
        FSESegment := TEDITransactionSetSegment(TEDITransactionSetSegmentSpec.Create(Self));
      end
      else
      begin
        FSTSegment := TEDITransactionSetSegment.Create(Self);
        FSESegment := TEDITransactionSetSegment.Create(Self);
      end;
    end
    else
    begin
      FSTSegment := TEDITransactionSetSegment.Create(Self);
      FSESegment := TEDITransactionSetSegment.Create(Self);
    end;
  end
  else
  begin
    FSTSegment := TEDITransactionSetSegment.Create(Self);
    FSESegment := TEDITransactionSetSegment.Create(Self);
  end;
  SetLength(FSegments, 0);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDITransactionSet.DeleteSegment(Index: Integer);
var
  I: Integer;
begin
  if (Length(FSegments) > 0) and (Index >= Low(FSegments)) and (Index <= High(FSegments)) then
  begin
    //Delete
    FSegments[Index].Free;
    FSegments[Index] := nil;
    //Shift
    for I := Index + 1 to High(FSegments) do
    begin
      FSegments[I-1] := FSegments[I];
    end;
    //Resize
    SetLength(FSegments, High(FSegments));
  end
  else
  begin
    raise EJclEDIError.CreateResRec(@EDIError062);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDITransactionSet.DeleteSegments;
var
  I: Integer;
begin
  for I := Low(FSegments) to High(FSegments) do
  begin
    if Assigned(FSegments[I]) then
    begin
      //Delete
      FSegments[I].Free;
      FSegments[I] := nil;
    end;
  end;
  //Resize
  SetLength(FSegments, 0);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDITransactionSet.DeleteSegments(Index, Count: Integer);
var
  I: Integer;
begin
  if (Length(FSegments) > 0) and (Index >= Low(FSegments)) and (Index <= High(FSegments)) then
  begin
    //Delete
    for I := Index to (Index + Count) - 1 do
    begin
      if Assigned(FSegments[I]) then
      begin
        FSegments[I].Free;
        FSegments[I] := nil;
      end;
    end;
    //Shift
    for I := (Index + Count) to High(FSegments) do
    begin
      FSegments[I-Count] := FSegments[I];
      FSegments[I] := nil;
    end;
    //Resize
    SetLength(FSegments, Length(FSegments) - Count);
  end
  else
  begin
    raise EJclEDIError.CreateResRecFmt(@EDIError061, [IntToStr(Index)]);
  end;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDITransactionSet.Destroy;
begin
  DeleteSegments;
  FSESegment.Free;
  FSTSegment.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDITransactionSet.Dissassemble;
var
  I, StartPos, SearchResult: Integer;
  S, S2: string;
begin
  FSTSegment.Data := '';
  FSTSegment.DeleteElements;
  FSESegment.Data := '';
  FSESegment.DeleteElements;
  DeleteSegments;
  //Check delimiter assignment
  if not Assigned(FDelimiters) then
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
    begin
      raise EJclEDIError.CreateResRec(@EDIError060);
    end;
  end;
  //Find the first segment
  StartPos := 1;
  SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
  while SearchResult <> 0 do
  begin
    S := Copy(FData, ((StartPos + FDelimiters.SDLen) - 1), Length(TSHSegmentId));
    S2 := Copy(FData, ((StartPos + FDelimiters.SDLen) - 1), Length(TSTSegmentId));
    if (S <> TSHSegmentId) and (S2 <> TSTSegmentId) then
    begin
      I := AddSegment;
      if ((SearchResult - StartPos) > 0) then //data exists
      begin
        FSegments[I].Data := Copy(FData, ((StartPos + FDelimiters.SDLen) - 1),
          ((SearchResult - StartPos) + FDelimiters.SDLen));
        FSegments[I].Dissassemble;
      end;
    end
    else if S = TSHSegmentId then
    begin
      if ((SearchResult - StartPos) > 0) then //data exists
      begin
        FSTSegment.Data := Copy(FData, ((StartPos + FDelimiters.SDLen) - 1),
          ((SearchResult - StartPos) + FDelimiters.SDLen));
        FSTSegment.Dissassemble;
      end;
    end
    else if S2 = TSTSegmentId then
    begin
      if ((SearchResult - StartPos) > 0) then //data exists
      begin
        FSESegment.Data := Copy(FData, ((StartPos + FDelimiters.SDLen) - 1),
          ((SearchResult - StartPos) + FDelimiters.SDLen));
        FSESegment.Dissassemble;
      end;
    end;
    StartPos := SearchResult + FDelimiters.SDLen;
    SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
  end;
  FData := '';
end;

function TEDITransactionSet.GetSegment(Index: Integer): TEDISegment;
begin
  Result := nil;
  if (Length(FSegments) > 0) then
    if (Index >= Low(FSegments)) then
      if (Index <= High(FSegments)) then
      begin
        if not Assigned(FSegments[Index]) then
        begin
          raise EJclEDIError.CreateResRecFmt(@EDIError056, [IntToStr(Index)]);
        end;
        Result := FSegments[Index];
      end
      else
        raise EJclEDIError.CreateResRecFmt(@EDIError057, [IntToStr(Index)])
    else
      raise EJclEDIError.CreateResRecFmt(@EDIError058, [IntToStr(Index)])
  else
    raise EJclEDIError.CreateResRecFmt(@EDIError059, [IntToStr(Index)]);
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSet.InsertSegment(InsertIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := InsertIndex;
  if (Length(FSegments) > 0) and (InsertIndex >= Low(FSegments)) and
    (InsertIndex <= High(FSegments)) then
  begin
    //Resize
    SetLength(FSegments, Length(FSegments) + 1);
    //Shift
    for I := High(FSegments) downto InsertIndex + 1 do
    begin
      FSegments[I] := FSegments[I-1];
    end;
    //Insert
    FSegments[InsertIndex] := nil;
    if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
    begin
      if Assigned(Parent.Parent) and (Parent.Parent is TEDIInterchangeControl) then
      begin
        if TEDIInterchangeControl(Parent.Parent).FEDIDUT = ediUseAsSpec then
        begin
          FSegments[InsertIndex] := TEDISegmentSpec.Create(Self);
        end
        else
        begin
          FSegments[InsertIndex] := TEDISegment.Create(Self);
        end;
      end
      else
      begin
        FSegments[InsertIndex] := TEDISegment.Create(Self);
      end;
    end
    else
    begin
      FSegments[InsertIndex] := TEDISegment.Create(Self);
    end;
  end
  else
  begin
    Result := AddSegment;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSet.InsertSegment(InsertIndex: Integer; Segment: TEDISegment): Integer;
var
  I: Integer;
begin
  Result := InsertIndex;
  if (Length(FSegments) > 0) and (InsertIndex >= Low(FSegments)) and
    (InsertIndex <= High(FSegments)) then
  begin
    //Resize
    SetLength(FSegments, Length(FSegments) + 1);
    //Shift
    for I := High(FSegments) downto InsertIndex + 1 do
    begin
      FSegments[I] := FSegments[I-1];
    end;
    //Insert
    FSegments[InsertIndex] := nil;
    FSegments[InsertIndex] := Segment;
    FSegments[InsertIndex].Parent := Self;
  end
  else
  begin
    Result := AppendSegment(Segment);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSet.InsertSegments(InsertIndex, Count: Integer): Integer;
var
  I: Integer;
begin
  Result := InsertIndex;
  if (Length(FSegments) > 0) and (InsertIndex >= Low(FSegments)) and
    (InsertIndex <= High(FSegments)) then
  begin
    //Resize
    SetLength(FSegments, Length(FSegments) + Count);
    //Shift
    for I := High(FSegments) downto InsertIndex + Count do
    begin
      FSegments[I] := FSegments[I-Count];
      FSegments[I-Count] := nil;
    end;
    //Insert
    for I := InsertIndex to (InsertIndex + Count) - 1 do
    begin
      if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
      begin
        if Assigned(Parent.Parent) and (Parent.Parent is TEDIInterchangeControl) then
        begin
          if TEDIInterchangeControl(Parent.Parent).FEDIDUT = ediUseAsSpec then
          begin
            FSegments[I] := TEDISegmentSpec.Create(Self);
          end
          else
          begin
            FSegments[I] := TEDISegment.Create(Self);
          end;
        end
        else
        begin
          FSegments[I] := TEDISegment.Create(Self);
        end;
      end
      else
      begin
        FSegments[I] := TEDISegment.Create(Self);
      end;
    end;
  end
  else
  begin
    Result := AddSegments(Count);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSet.InsertSegments(InsertIndex: Integer;
  SegmentArray: TEDISegmentArray): Integer;
var
  I, J, K: Integer;
begin
  Result := InsertIndex;
  I := Length(SegmentArray);
  if (Length(FSegments) > 0) and (InsertIndex >= Low(FSegments)) and
    (InsertIndex <= High(FSegments)) then
  begin
    //Resize
    SetLength(FSegments, Length(FSegments) + I);
    //Shift
    for J := High(FSegments) downto InsertIndex + I do
    begin
      FSegments[J] := FSegments[J-I];
      FSegments[J-I] := nil;
    end;
    //Insert
    K := 0;
    for J := InsertIndex to (InsertIndex + I) - 1 do
    begin
      FSegments[J] := SegmentArray[K];
      FSegments[J].Parent := Self;
      Inc(K);
    end;
  end
  else
  begin
    Result := AppendSegments(SegmentArray);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSet.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  if FDelimiters = nil then //Attempt to assign the delimiters
  begin
    if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
    begin
      if Assigned(Parent.Delimiters) then
      begin
        Result := Parent.Delimiters;
        Exit;
      end;
      if Assigned(Parent.Parent) and (Parent.Parent is TEDIInterchangeControl) then
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

procedure TEDITransactionSet.SetSegment(Index: Integer; Segment: TEDISegment);
begin
  if (Length(FSegments) > 0) then
    if (Index >= Low(FSegments)) then
      if (Index <= High(FSegments)) then
      begin
        if Assigned(FSegments[Index]) then
        begin
          FSegments[Index].Free;
          FSegments[Index] := nil;
        end;
        FSegments[Index] := Segment;
      end
      else
        raise EJclEDIError.CreateResRecFmt(@EDIError053, [IntToStr(Index)])
    else
      raise EJclEDIError.CreateResRecFmt(@EDIError054, [IntToStr(Index)])
  else
    raise EJclEDIError.CreateResRecFmt(@EDIError055, [IntToStr(Index)]);
end;

//==================================================================================================
// TEDIFunctionalGroup
//==================================================================================================

function TEDIFunctionalGroup.AddTransactionSet: Integer;
begin
  SetLength(FTransactionSets, Length(FTransactionSets) + 1);
  FTransactionSets[High(FTransactionSets)] := TEDITransactionSet.Create(Self);
  Result := High(FTransactionSets);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.AddTransactionSets(Count: Integer): Integer;
var
  I, J: Integer;
begin
  I := Length(FTransactionSets); //Previous Count
  Result := I;
  //Resize
  SetLength(FTransactionSets, Length(FTransactionSets) + Count);
  //Add
  for J := I to High(FTransactionSets) do
  begin
    FTransactionSets[J]:= TEDITransactionSet.Create(Self);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.AppendTransactionSet(TransactionSet: TEDITransactionSet): Integer;
begin
  SetLength(FTransactionSets, Length(FTransactionSets) + 1);
  FTransactionSets[High(FTransactionSets)] := TransactionSet;
  TransactionSet.Parent := Self;
  Result := High(FTransactionSets);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.AppendTransactionSets(
  TransactionSetArray: TEDITransactionSetArray): Integer;
var
  I, J, K: Integer;
begin
  I := 0;
  J := Length(FTransactionSets);
  Result := J;
  //Resize
  SetLength(FTransactionSets, Length(FTransactionSets) + Length(TransactionSetArray));
  //Append
  for K := J to High(TransactionSetArray) do
  begin
    FTransactionSets[K] := TransactionSetArray[I];
    FTransactionSets[K].Parent := Self;
    Inc(I);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.Assemble: string;
var
  I: Integer;
begin
  FData := '';
  FLength := 0;
  Result := '';
  if not Assigned(FDelimiters) then //Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
    begin
      raise EJclEDIError.CreateResRec(@EDIError052);
    end;
  end;
  FData := FGSSegment.Assemble;
  FGSSegment.Data := '';

  if (Length(FTransactionSets) > 0) then
  begin
    for I := Low(FTransactionSets) to High(FTransactionSets) do
    begin
      if Assigned(FTransactionSets[I]) then
      begin
        FData := FData + FTransactionSets[I].Assemble;
      end;
    end;
  end;

  DeleteTransactionSets;

  FData := FData + FGESegment.Assemble;
  FGESegment.Data := '';

  FLength := Length(FData);
  Result := FData;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIFunctionalGroup.Create(Parent: TEDIDataObject);
begin
  if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
  begin
    inherited Create(Parent);
  end
  else
  begin
    inherited Create(nil);
  end;
  FEDIDOT := ediFunctionalGroup;
  if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
  begin
    if TEDIInterchangeControl(Parent).FEDIDUT = ediUseAsSpec then
    begin
      FGSSegment := TEDIFunctionalGroupSegment(TEDIFunctionalGroupSegmentSpec.Create(Self));
      FGESegment := TEDIFunctionalGroupSegment(TEDIFunctionalGroupSegmentSpec.Create(Self));
    end
    else
    begin
      FGSSegment := TEDIFunctionalGroupSegment.Create(Self);
      FGESegment := TEDIFunctionalGroupSegment.Create(Self);
    end;
  end
  else
  begin
    FGSSegment := TEDIFunctionalGroupSegment.Create(Self);
    FGESegment := TEDIFunctionalGroupSegment.Create(Self);
  end;
  SetLength(FTransactionSets, 0);
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIFunctionalGroup.Create(Parent: TEDIDataObject; TransactionSetCount: Integer);
begin
  if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
  begin
    inherited Create(Parent);
  end
  else
  begin
    inherited Create(nil);
  end;
  FEDIDOT := ediFunctionalGroup;
  if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
  begin
    if TEDIInterchangeControl(Parent).FEDIDUT = ediUseAsSpec then
    begin
      FGSSegment := TEDIFunctionalGroupSegment(TEDIFunctionalGroupSegmentSpec.Create(Self));
      FGESegment := TEDIFunctionalGroupSegment(TEDIFunctionalGroupSegmentSpec.Create(Self));
    end
    else
    begin
      FGSSegment := TEDIFunctionalGroupSegment.Create(Self);
      FGESegment := TEDIFunctionalGroupSegment.Create(Self);
    end;
  end
  else
  begin
    FGSSegment := TEDIFunctionalGroupSegment.Create(Self);
    FGESegment := TEDIFunctionalGroupSegment.Create(Self);
  end;
  SetLength(FTransactionSets, 0);
  AddTransactionSets(TransactionSetCount);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroup.DeleteTransactionSet(Index: Integer);
var
  I: Integer;
begin
  if (Length(FTransactionSets) > 0) and (Index >= Low(FTransactionSets)) and
    (Index <= High(FTransactionSets)) then
  begin
    //Delete
    FTransactionSets[Index].Free;
    FTransactionSets[Index] := nil;
    //Shift
    for I := Index + 1 to High(FTransactionSets) do
    begin
      FTransactionSets[I-1] := FTransactionSets[I];
    end;
    //Resize
    SetLength(FTransactionSets, High(FTransactionSets));
  end
  else
  begin
    raise EJclEDIError.CreateResRecFmt(@EDIError051, [IntToStr(Index)]);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroup.DeleteTransactionSets;
var
  I: Integer;
begin
  for I := Low(FTransactionSets) to High(FTransactionSets) do
  begin
    if Assigned(FTransactionSets[I]) then
    begin
      //Delete
      FTransactionSets[I].Free;
      FTransactionSets[I] := nil;
    end;
  end;
  //Resize
  SetLength(FTransactionSets, 0);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroup.DeleteTransactionSets(Index, Count: Integer);
var
  I: Integer;
begin
  if (Length(FTransactionSets) > 0) and (Index >= Low(FTransactionSets)) and
    (Index <= High(FTransactionSets)) then
  begin
    //Delete
    for I := Index to (Index + Count) - 1 do
    begin
      if Assigned(FTransactionSets[I]) then
      begin
        FTransactionSets[I].Free;
        FTransactionSets[I] := nil;
      end;
    end;
    //Shift
    for I := (Index + Count) to High(FTransactionSets) do
    begin
      FTransactionSets[I-Count] := FTransactionSets[I];
      FTransactionSets[I] := nil;
    end;
    //Resize
    SetLength(FTransactionSets, Length(FTransactionSets) - Count);
  end
  else
  begin
    raise EJclEDIError.CreateResRecFmt(@EDIError050, [IntToStr(Index)]);
  end;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIFunctionalGroup.Destroy;
begin
  FGSSegment.Free;
  FGESegment.Free;
  DeleteTransactionSets;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroup.Dissassemble;
var
  I, StartPos, SearchResult: Integer;
begin
  FGSSegment.Data := '';
  FGSSegment.DeleteElements;
  FGESegment.Data := '';
  FGESegment.DeleteElements;
  DeleteTransactionSets;
  //Check delimiter assignment
  if not Assigned(FDelimiters) then
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
    begin
      raise EJclEDIError.CreateResRec(@EDIError042);
    end;
  end;
  //Find Functional Group Header Segment
  SearchResult := 0;
  StartPos := 1;
  //Search for Functional Group Header
  if FGHSegmentId + FDelimiters.ED = Copy(FData, 1, Length(FGHSegmentId + FDelimiters.ED)) then
  begin
    //Search for Functional Group Header Segment Terminator
    SearchResult := StrSearch(FDelimiters.SD, FData, 1);
    if (SearchResult - StartPos) > 0 then //data exists
    begin
      FGSSegment.Data := Copy(FData, 1, (SearchResult + FDelimiters.SDLen) - 1);
      FGSSegment.Dissassemble;
    end
    else
    begin
      raise EJclEDIError.CreateResRec(@EDIError043);
    end;
  end
  else
  begin
    raise EJclEDIError.CreateResRec(@EDIError044);
  end;
  //Search for Transaction Set Header
  SearchResult := StrSearch(FDelimiters.SD + TSHSegmentId + FDelimiters.ED, FData, StartPos);
  if SearchResult <= 0 then
  begin
    raise EJclEDIError.CreateResRec(@EDIError045);
  end;
  //Set next start position
  StartPos := SearchResult + FDelimiters.SDLen; //Move past the delimiter
  //Continue
  while SearchResult <> 0 do
  begin
    //Search for Transaction Set Trailer
    SearchResult := StrSearch(FDelimiters.SD + TSTSegmentId + FDelimiters.ED, FData, StartPos);
    if SearchResult <> 0 then
    begin
      //Set the next start position
      SearchResult := SearchResult + FDelimiters.SDLen; //Move past the delimiter
      //Search for the end of Transaction Set Trailer
      SearchResult := StrSearch(FDelimiters.SD, FData, SearchResult);
      if SearchResult <> 0 then
      begin
        I := AddTransactionSet;
        FTransactionSets[I].Data :=
          Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.SDLen));
        FTransactionSets[I].Dissassemble;
      end
      else
      begin
        raise EJclEDIError.CreateResRec(@EDIError046);
      end;
    end
    else
    begin
      raise EJclEDIError.CreateResRec(@EDIError047);
    end;
    //Set the next start position
    StartPos := SearchResult + FDelimiters.SDLen; //Move past the delimiter
    //
    //Verify the next record is a Transaction Set Header
    if (TSHSegmentId + FDelimiters.ED) <>
      Copy(FData, StartPos, (Length(TSHSegmentId) + FDelimiters.EDLen)) then
    begin
      Break;
    end;
  end;
  //Set the next start position
  StartPos := SearchResult + FDelimiters.SDLen; //Move past the delimiter
  //Find Functional Group Trailer Segment
  if (FGTSegmentId + FDelimiters.ED) =
    Copy(FData, StartPos, Length(FGTSegmentId + FDelimiters.ED)) then
  begin
    //Find Functional Group Trailer Segment Terminator
    SearchResult := StrSearch(FDelimiters.SD, FData, StartPos + FDelimiters.SDLen);
    if (SearchResult - StartPos) > 0 then //data exists
    begin
      FGESegment.Data := Copy(FData, StartPos, (SearchResult + FDelimiters.SDLen));
      FGESegment.Dissassemble;
    end
    else
    begin
      raise EJclEDIError.CreateResRec(@EDIError048);
    end;
  end
  else
  begin
    raise EJclEDIError.CreateResRec(@EDIError049);
  end;
  FData := '';
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.GetTransactionSet(Index: Integer): TEDITransactionSet;
begin
  Result := nil;
  if (Length(FTransactionSets) > 0) then
    if (Index >= Low(FTransactionSets)) then
      if (Index <= High(FTransactionSets)) then
      begin
        if not Assigned(FTransactionSets[Index]) then
        begin
          raise EJclEDIError.CreateResRecFmt(@EDIError038, [IntToStr(Index)]);
        end;
        Result := FTransactionSets[Index];
      end
      else
        raise EJclEDIError.CreateResRecFmt(@EDIError039, [IntToStr(Index)])
    else
      raise EJclEDIError.CreateResRecFmt(@EDIError040, [IntToStr(Index)])
  else
    raise EJclEDIError.CreateResRecFmt(@EDIError041, [IntToStr(Index)]);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.InsertTransactionSet(InsertIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := InsertIndex;
  if (Length(FTransactionSets) > 0) and (InsertIndex >= Low(FTransactionSets)) and
    (InsertIndex <= High(FTransactionSets)) then
  begin
    //Resize
    SetLength(FTransactionSets, Length(FTransactionSets) + 1);
    //Shift
    for I := High(FTransactionSets) downto InsertIndex + 1 do
    begin
      FTransactionSets[I] := FTransactionSets[I-1];
    end;
    //Insert
    FTransactionSets[InsertIndex] := nil;
    FTransactionSets[InsertIndex] := TEDITransactionSet.Create(Self);
  end
  else
  begin
    Result := AddTransactionSet;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.InsertTransactionSet(InsertIndex: Integer;
  TransactionSet: TEDITransactionSet): Integer;
var
  I: Integer;
begin
  Result := InsertIndex;
  if (Length(FTransactionSets) > 0) and (InsertIndex >= Low(FTransactionSets)) and
    (InsertIndex <= High(FTransactionSets)) then
  begin
    //Resize
    SetLength(FTransactionSets, Length(FTransactionSets) + 1);
    //Shift
    for I := High(FTransactionSets) downto InsertIndex + 1 do
    begin
      FTransactionSets[I] := FTransactionSets[I-1];
    end;
    //Insert
    FTransactionSets[InsertIndex] := nil;
    FTransactionSets[InsertIndex] := TransactionSet;
    FTransactionSets[InsertIndex].Parent := Self;
  end
  else
  begin
    Result := AppendTransactionSet(TransactionSet);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.InsertTransactionSets(InsertIndex: Integer;
  TransactionSetArray: TEDITransactionSetArray): Integer;
var
  I, J, K: Integer;
begin
  Result := InsertIndex;
  I := Length(TransactionSetArray);
  if (Length(FTransactionSets) > 0) and (InsertIndex >= Low(FTransactionSets)) and
    (InsertIndex <= High(FTransactionSets)) then
  begin
    //Resize
    SetLength(FTransactionSets, Length(FTransactionSets) + I);
    //Shift
    for J := High(FTransactionSets) downto InsertIndex + I do
    begin
      FTransactionSets[J] := FTransactionSets[J-I];
      FTransactionSets[J-I] := nil;
    end;
    //Insert
    K := 0;
    for J := InsertIndex to (InsertIndex + I) - 1 do
    begin
      FTransactionSets[J] := TransactionSetArray[K];
      FTransactionSets[J].Parent := Self;
      Inc(K);
    end;
  end
  else
  begin
    Result := AppendTransactionSets(TransactionSetArray);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.InsertTransactionSets(InsertIndex, Count: Integer): Integer;
var
  I: Integer;
begin
  Result := InsertIndex;
  if (Length(FTransactionSets) > 0) and (InsertIndex >= Low(FTransactionSets)) and
    (InsertIndex <= High(FTransactionSets)) then
  begin
    //Resize
    SetLength(FTransactionSets, Length(FTransactionSets) + Count);
    //Shift
    for I := High(FTransactionSets) downto InsertIndex + Count do
    begin
      FTransactionSets[I] := FTransactionSets[I-Count];
      FTransactionSets[I-Count] := nil;
    end;
    //Insert
    for I := InsertIndex to (InsertIndex + Count) - 1 do
    begin
      FTransactionSets[I] := TEDITransactionSet.Create(Self);
    end;
  end
  else
  begin
    Result := AddTransactionSets(Count);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  //Attempt to assign the delimiters
  if not Assigned(FDelimiters) then
  begin
    if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
    begin
      if Assigned(Parent.Delimiters) then
      begin
        Result := Parent.Delimiters;
      end;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroup.SetTransactionSet(Index: Integer; TransactionSet: TEDITransactionSet);
begin
  if (Length(FTransactionSets) > 0) then
    if (Index >= Low(FTransactionSets)) then
      if (Index <= High(FTransactionSets)) then
      begin
        if Assigned(FTransactionSets[Index]) then
        begin
          FTransactionSets[Index].Free;
          FTransactionSets[Index] := nil;
        end;
        FTransactionSets[Index] := TransactionSet;
      end
      else
        raise EJclEDIError.CreateResRecFmt(@EDIError035, [IntToStr(Index)])
    else
      raise EJclEDIError.CreateResRecFmt(@EDIError036, [IntToStr(Index)])
  else
    raise EJclEDIError.CreateResRecFmt(@EDIError037, [IntToStr(Index)]);
end;

//==================================================================================================
// TEDIInterchangeControl
//==================================================================================================

function TEDIInterchangeControl.AddFunctionalGroup: Integer;
begin
  SetLength(FFunctionalGroups, Length(FFunctionalGroups) + 1);
  FFunctionalGroups[High(FFunctionalGroups)] := TEDIFunctionalGroup.Create(Self);
  Result := High(FFunctionalGroups);
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.AddFunctionalGroups(Count: Integer): Integer;
var
  I, J: Integer;
begin
  I := Length(FFunctionalGroups);
  Result := I;
  //Resize
  SetLength(FFunctionalGroups, Length(FFunctionalGroups) + Count);
  //Add
  for J := I to High(FFunctionalGroups) do
  begin
    FFunctionalGroups[J]:= TEDIFunctionalGroup.Create(Self);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.AppendFunctionalGroup(
  FunctionalGroup: TEDIFunctionalGroup): Integer;
begin
  SetLength(FFunctionalGroups, Length(FFunctionalGroups) + 1);
  FFunctionalGroups[High(FFunctionalGroups)] := FunctionalGroup;
  FunctionalGroup.Parent := Self;
  Result := High(FFunctionalGroups);
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.AppendFunctionalGroups(
  FunctionalGroupArray: TEDIFunctionalGroupArray): Integer;
var
  I, J, K: Integer;
begin
  I := 0;
  J := Length(FFunctionalGroups);
  Result := J;
  //Resize
  SetLength(FFunctionalGroups, Length(FFunctionalGroups) + Length(FunctionalGroupArray));
  //Append
  for K := J to High(FunctionalGroupArray) do
  begin
    FFunctionalGroups[K] := FunctionalGroupArray[I];
    FFunctionalGroups[K].Parent := Self;
    Inc(I);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.Assemble: string;
var
  I: Integer;
begin
  FData := '';
  FLength := 0;
  Result := '';

  if not Assigned(FDelimiters) then
  begin
    raise EJclEDIError.CreateResRec(@EDIError034);
  end;

  FData := FISASegment.Assemble;
  FISASegment.Data := '';
  FISASegment.DeleteElements;

  if (Length(FFunctionalGroups) > 0) then
  begin
    for I := Low(FFunctionalGroups) to High(FFunctionalGroups) do
    begin
      if Assigned(FFunctionalGroups[I]) then
      begin
        FData := FData + FFunctionalGroups[I].Assemble;
      end;
    end;
  end;

  DeleteFunctionalGroups;

  FData := FData + FIEASegment.Assemble;
  FIEASegment.Data := '';
  FIEASegment.DeleteElements;

  FLength := Length(FData);
  Result := FData;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIInterchangeControl.Create(Parent: TEDIDataObject;
  EDIDUT: TEDIDataUsageType = ediUseAsData);
begin
  if Assigned(Parent) and (Parent is TEDIFile) then
  begin
    inherited Create(Parent);
  end
  else
  begin
    inherited Create(nil);
  end;
  FEDIDOT := ediInterchangeControl;
  FEDIDUT := EDIDUT;
  if FEDIDUT = ediUseAsSpec then
  begin
    FISASegment := TEDIInterchangeControlSegment(TEDIInterchangeControlSegmentSpec.Create(Self));
    FIEASegment := TEDIInterchangeControlSegment(TEDIInterchangeControlSegmentSpec.Create(Self));
  end
  else
  begin
    FISASegment := TEDIInterchangeControlSegment.Create(Self);
    FIEASegment := TEDIInterchangeControlSegment.Create(Self);
  end;
  SetLength(FFunctionalGroups, 0);
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIInterchangeControl.Create(Parent: TEDIDataObject; FunctionalGroupCount: Integer;
  EDIDUT: TEDIDataUsageType = ediUseAsData);
begin
  if Assigned(Parent) and (Parent is TEDIFile) then
  begin
    inherited Create(Parent);
  end
  else
  begin
    inherited Create(nil);
  end;
  FEDIDOT := ediInterchangeControl;
  FEDIDUT := EDIDUT;
  SetLength(FFunctionalGroups, 0);
  if FEDIDUT = ediUseAsSpec then
  begin
    FISASegment := TEDIInterchangeControlSegment(TEDIInterchangeControlSegmentSpec.Create(Self));
    FIEASegment := TEDIInterchangeControlSegment(TEDIInterchangeControlSegmentSpec.Create(Self));
  end
  else
  begin
    FISASegment := TEDIInterchangeControlSegment.Create(Self);
    FIEASegment := TEDIInterchangeControlSegment.Create(Self);
  end;
  AddFunctionalGroups(FunctionalGroupCount);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIInterchangeControl.DeleteFunctionalGroup(Index: Integer);
var
  I: Integer;
begin
  if (Length(FFunctionalGroups) > 0) and (Index >= Low(FFunctionalGroups)) and
    (Index <= High(FFunctionalGroups)) then
  begin
    //Delete
    FFunctionalGroups[Index].Free;
    FFunctionalGroups[Index] := nil;
    //Shift
    for I := Index + 1 to High(FFunctionalGroups) do
    begin
      FFunctionalGroups[I-1] := FFunctionalGroups[I];
    end;
    //Resize
    SetLength(FFunctionalGroups, High(FFunctionalGroups));
  end
  else
  begin
    raise EJclEDIError.CreateResRecFmt(@EDIError033, [IntToStr(Index)]);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIInterchangeControl.DeleteFunctionalGroups;
var
  I: Integer;
begin
  for I := Low(FFunctionalGroups) to High(FFunctionalGroups) do
  begin
    if Assigned(FFunctionalGroups[I]) then
    begin
      //Delete
      FFunctionalGroups[I].Free;
      FFunctionalGroups[I] := nil;
    end;
  end;
  //Resize
  SetLength(FFunctionalGroups, 0);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIInterchangeControl.DeleteFunctionalGroups(Index, Count: Integer);
var
  I: Integer;
begin
  if (Length(FFunctionalGroups) > 0) and (Index >= Low(FFunctionalGroups)) and
    (Index <= High(FFunctionalGroups)) then
  begin
    //Delete
    for I := Index to (Index + Count) - 1 do
    begin
      if Assigned(FFunctionalGroups[I]) then
      begin
        FFunctionalGroups[I].Free;
        FFunctionalGroups[I] := nil;
      end;
    end;
    //Shift
    for I := (Index + Count) to High(FFunctionalGroups) do
    begin
      FFunctionalGroups[I-Count] := FFunctionalGroups[I];
      FFunctionalGroups[I] := nil;
    end;
    //Resize
    SetLength(FFunctionalGroups, Length(FFunctionalGroups) - Count);
  end
  else
  begin
    raise EJclEDIError.CreateResRecFmt(@EDIError032, [IntToStr(Index)]);
  end;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIInterchangeControl.Destroy;
begin
  FISASegment.Free;
  FIEASegment.Free;
  DeleteFunctionalGroups;
  if Assigned(FDelimiters) then
  begin
    FDelimiters.Free;
    FDelimiters := nil;
  end;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIInterchangeControl.Dissassemble;
var
  I, StartPos, SearchResult: Integer;
begin
  FISASegment.Data := '';
  FISASegment.DeleteElements;
  FIEASegment.Data := '';
  FIEASegment.DeleteElements;
  DeleteFunctionalGroups;

  if not Assigned(FDelimiters) then
  begin
    raise EJclEDIError.CreateResRec(@EDIError024);
  end;

  SearchResult := 0;
  StartPos := 1;
  //Search for Interchange Control Header
  if ICHSegmentId + FDelimiters.ED = Copy(FData, 1, Length(ICHSegmentId + FDelimiters.ED)) then
  begin
    SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
    if (SearchResult - StartPos) > 0 then //data exists
    begin
      FISASegment.Data := Copy(FData, 1, (SearchResult + FDelimiters.SDLen) - 1);
      FISASegment.Dissassemble;
    end
    else
    begin
      raise EJclEDIError.CreateResRec(@EDIError025);
    end;
  end
  else
  begin
    raise EJclEDIError.CreateResRec(@EDIError026);
  end;
  //Search for Functional Group Header
  SearchResult := StrSearch(FDelimiters.SD + FGHSegmentId + FDelimiters.ED, FData, StartPos);
  if SearchResult <= 0 then
  begin
    raise EJclEDIError.CreateResRec(@EDIError027);
  end;
  //Set next start positon
  StartPos := SearchResult + FDelimiters.SDLen; //Move past the delimiter
  //Continue
  while ((StartPos + Length(FGHSegmentId)) < Length(FData)) and (SearchResult > 0) do
  begin
    //Search for Functional Group Trailer
    SearchResult := StrSearch(FDelimiters.SD + FGTSegmentId + FDelimiters.ED, FData, StartPos);
    if SearchResult > 0 then
    begin
      //Set next start positon
      SearchResult := SearchResult + FDelimiters.SDLen; //Move past the delimiter
      //Search for end of Functional Group Trailer Segment Terminator
      SearchResult := StrSearch(FDelimiters.SD, FData, SearchResult);
      if SearchResult > 0 then
      begin
        I := AddFunctionalGroup;
        FFunctionalGroups[I].Data :=
          Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.SDLen));
        FFunctionalGroups[I].Dissassemble;
      end
      else
      begin
        raise EJclEDIError.CreateResRec(@EDIError028);
      end;
    end
    else
    begin
      raise EJclEDIError.CreateResRec(@EDIError029);
    end;
    //Set next start positon
    StartPos := SearchResult + FDelimiters.SDLen; //Move past the delimiter
    //Verify the next record is a Functional Group Header
    if (FGHSegmentId + FDelimiters.ED) <>
      Copy(FData, StartPos, (Length(FGHSegmentId) + FDelimiters.EDLen)) then
    begin
      Break;
    end;
  end;
  //Verify the next record is a Interchange Control Trailer
  if (ICTSegmentId + FDelimiters.ED) =
    Copy(FData, StartPos, Length(ICTSegmentId + FDelimiters.ED)) then
  begin
    //Search for the end of Interchange Control Trailer Segment Terminator
    SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
    if (SearchResult - StartPos) > 0 then //data exists
    begin
      FIEASegment.Data := Copy(FData, StartPos, (SearchResult + FDelimiters.SDLen));
      FIEASegment.Dissassemble;
    end
    else
    begin
      raise EJclEDIError.CreateResRec(@EDIError030);
    end;
  end
  else
  begin
    raise EJclEDIError.CreateResRec(@EDIError031);
  end;
  FData := '';
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.GetFunctionalGroup(Index: Integer): TEDIFunctionalGroup;
begin
  Result := nil;
  if (Length(FFunctionalGroups) > 0) then
    if (Index >= Low(FFunctionalGroups)) then
      if (Index <= High(FFunctionalGroups)) then
      begin
        if not Assigned(FFunctionalGroups[Index]) then
        begin
          raise EJclEDIError.CreateResRecFmt(@EDIError020, [IntToStr(Index)]);
        end;
        Result := FFunctionalGroups[Index];
      end
      else
        raise EJclEDIError.CreateResRecFmt(@EDIError021, [IntToStr(Index)])
    else
      raise EJclEDIError.CreateResRecFmt(@EDIError022, [IntToStr(Index)])
  else
    raise EJclEDIError.CreateResRecFmt(@EDIError023, [IntToStr(Index)]);
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.InsertFunctionalGroup(InsertIndex: Integer;
  FunctionalGroup: TEDIFunctionalGroup): Integer;
var
  I: Integer;
begin
  Result := InsertIndex;
  if (Length(FFunctionalGroups) > 0) and (InsertIndex >= Low(FFunctionalGroups)) and
    (InsertIndex <= High(FFunctionalGroups)) then
  begin
    //Resize
    SetLength(FFunctionalGroups, Length(FFunctionalGroups) + 1);
    //Shift
    for I := High(FFunctionalGroups) downto InsertIndex + 1 do
    begin
      FFunctionalGroups[I] := FFunctionalGroups[I-1];
    end;
    //Insert
    FFunctionalGroups[InsertIndex] := nil;
    FFunctionalGroups[InsertIndex] := FunctionalGroup;
    FFunctionalGroups[InsertIndex].Parent := Self;
  end
  else
  begin
    Result := AppendFunctionalGroup(FunctionalGroup);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.InsertFunctionalGroup(InsertIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := InsertIndex;
  if (Length(FFunctionalGroups) > 0) and (InsertIndex >= Low(FFunctionalGroups)) and
    (InsertIndex <= High(FFunctionalGroups)) then
  begin
    //Resize
    SetLength(FFunctionalGroups, Length(FFunctionalGroups) + 1);
    //Shift
    for I := High(FFunctionalGroups) downto InsertIndex + 1 do
    begin
      FFunctionalGroups[I] := FFunctionalGroups[I-1];
    end;
    //Insert
    FFunctionalGroups[InsertIndex] := nil;
    FFunctionalGroups[InsertIndex] := TEDIFunctionalGroup.Create(Self);
  end
  else
  begin
    Result := AddFunctionalGroup;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.InsertFunctionalGroups(InsertIndex, Count: Integer): Integer;
var
  I: Integer;
begin
  Result := InsertIndex;
  if (Length(FFunctionalGroups) > 0) and (InsertIndex >= Low(FFunctionalGroups)) and
    (InsertIndex <= High(FFunctionalGroups)) then
  begin
    //Resize
    SetLength(FFunctionalGroups, Length(FFunctionalGroups) + Count);
    //Shift
    for I := High(FFunctionalGroups) downto InsertIndex + Count do
    begin
      FFunctionalGroups[I] := FFunctionalGroups[I-Count];
      FFunctionalGroups[I-Count] := nil;
    end;
    //Insert
    for I := InsertIndex to (InsertIndex + Count) - 1 do
    begin
      FFunctionalGroups[I] := TEDIFunctionalGroup.Create(Self);
    end;
  end
  else
  begin
    Result := AddFunctionalGroups(Count);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.InsertFunctionalGroups(InsertIndex: Integer;
  FunctionalGroupArray: TEDIFunctionalGroupArray): Integer;
var
  I, J, K: Integer;
begin
  Result := InsertIndex;
  I := Length(FunctionalGroupArray);
  if (Length(FFunctionalGroups) > 0) and (InsertIndex >= Low(FFunctionalGroups)) and
    (InsertIndex <= High(FFunctionalGroups)) then
  begin
    //Resize
    SetLength(FFunctionalGroups, Length(FFunctionalGroups) + I);
    //Shift
    for J := High(FFunctionalGroups) downto InsertIndex + I do
    begin
      FFunctionalGroups[J] := FFunctionalGroups[J-I];
      FFunctionalGroups[J-I] := nil;
    end;
    //Insert
    K := 0;
    for J := InsertIndex to (InsertIndex + I) - 1 do
    begin
      FFunctionalGroups[J] := FunctionalGroupArray[K];
      FFunctionalGroups[J].Parent := Self;
      Inc(K);
    end;
  end
  else
  begin
    Result := AppendFunctionalGroups(FunctionalGroupArray);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIInterchangeControl.SetFunctionalGroup(Index: Integer;
  FunctionalGroup: TEDIFunctionalGroup);
begin
  if (Length(FFunctionalGroups) > 0) then
    if (Index >= Low(FFunctionalGroups)) then
      if (Index <= High(FFunctionalGroups)) then
      begin
        if Assigned(FFunctionalGroups[Index]) then
        begin
          FFunctionalGroups[Index].Free;
          FFunctionalGroups[Index] := nil;
        end;
        FFunctionalGroups[Index] := FunctionalGroup;
      end
      else
        raise EJclEDIError.CreateResRecFmt(@EDIError017, [IntToStr(Index)])
    else
      raise EJclEDIError.CreateResRecFmt(@EDIError018, [IntToStr(Index)])
  else
    raise EJclEDIError.CreateResRecFmt(@EDIError019, [IntToStr(Index)]);
end;

//==================================================================================================
// TEDIFile
//==================================================================================================

function TEDIFile.AddInterchange: Integer;
begin
  SetLength(FInterchanges, Length(FInterchanges) + 1);
  FInterchanges[High(FInterchanges)] := TEDIInterchangeControl.Create(Self);
  Result := High(FInterchanges);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFile.AddInterchanges(Count: Integer): Integer;
var
  I, J: Integer;
begin
  I := Length(FInterchanges);
  Result := I;
  //Resize
  SetLength(FInterchanges, Length(FInterchanges) + Count);
  //Add
  for J := I to High(FInterchanges) do
  begin
    FInterchanges[J]:= TEDIInterchangeControl.Create(Self);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFile.AppendInterchange(Interchange: TEDIInterchangeControl): Integer;
begin
  SetLength(FInterchanges, Length(FInterchanges) + 1);
  FInterchanges[High(FInterchanges)] := Interchange;
  FInterchanges[High(FInterchanges)].Parent := Self;
  Result := High(FInterchanges);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFile.AppendInterchanges(InterchangeControlArray: TEDIInterchangeControlArray): Integer;
var
  I, J, K: Integer;
begin
  I := 0;
  J := Length(FInterchanges);
  Result := J;
  //Resize
  SetLength(FInterchanges, Length(FInterchanges) + Length(InterchangeControlArray));
  //Append
  for K := J to High(InterchangeControlArray) do
  begin
    FInterchanges[K] := InterchangeControlArray[I];
    FInterchanges[K].Parent := Self;
    Inc(I);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFile.Assemble: string;
var
  I: Integer;
begin
  FData := '';
  FLength := 0;
  Result := '';

  if (Length(FInterchanges) > 0) then
  begin
    for I := Low(FInterchanges) to High(FInterchanges) do
    begin
      if Assigned(FInterchanges[I]) then
        FData := FData + FInterchanges[I].Assemble;
      FInterchanges[I].Data := '';
    end;
  end;

  FLength := Length(FData);
  Result := FData;

  DeleteInterchanges;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIFile.Create(Parent: TEDIDataObject; InterchangeCount: Integer);
begin
  if Assigned(Parent) then //and (Parent is TEDIFile)
  begin
    inherited Create(Parent);
  end
  else
  begin
    inherited Create(nil);
  end;
  FEDIFileOptions := [foVariableDelimiterDetection];
  //FErrorLog := TStringList.Create;
  FEDIDOT := ediFile;
  SetLength(FInterchanges, 0);
  AddInterchanges(InterchangeCount);
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIFile.Create(Parent: TEDIDataObject);
begin
  if Assigned(Parent) then //and (Parent is TEDIFile)
  begin
    inherited Create(Parent);
  end
  else
  begin
    inherited Create(nil);
  end;
  FEDIFileOptions := [foVariableDelimiterDetection];
  //FErrorLog := TStringList.Create;
  FEDIDOT := ediFile;
  SetLength(FInterchanges, 0);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFile.DeleteInterchange(Index: Integer);
var
  I: Integer;
begin
  if (Length(FInterchanges) > 0) and (Index >= Low(FInterchanges)) and
    (Index <= High(FInterchanges)) then
  begin
    //Delete
    FInterchanges[Index].Free;
    FInterchanges[Index] := nil;
    //Shift
    for I := Index + 1 to High(FInterchanges) do
      FInterchanges[I-1] := FInterchanges[I];
    //Resize
    SetLength(FInterchanges, High(FInterchanges));
  end
  else
  begin
    raise EJclEDIError.CreateResRecFmt(@EDIError016, [IntToStr(Index)]);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFile.DeleteInterchanges(Index, Count: Integer);
var
  I: Integer;
begin
  if (Length(FInterchanges) > 0) and (Index >= Low(FInterchanges)) and
    (Index <= High(FInterchanges)) then
  begin
    //Delete
    for I := Index to (Index + Count) - 1 do
    begin
      if Assigned(FInterchanges[I]) then
      begin
        FInterchanges[I].Free;
        FInterchanges[I] := nil;
      end;
    end;
    //Shift
    for I := (Index + Count) to High(FInterchanges) do
    begin
      FInterchanges[I-Count] := FInterchanges[I];
      FInterchanges[I] := nil;
    end;
    //Resize
    SetLength(FInterchanges, Length(FInterchanges) - Count);
  end
  else
  begin
    raise EJclEDIError.CreateResRecFmt(@EDIError015, [IntToStr(Index)]);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFile.DeleteInterchanges;
var
  I: Integer;
begin
  for I := Low(FInterchanges) to High(FInterchanges) do
  begin
    if Assigned(FInterchanges[I]) then
    begin
      //Delete
      FInterchanges[I].Free;
      FInterchanges[I] := nil;
    end;
  end;
  //Resize
  SetLength(FInterchanges, 0);
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIFile.Destroy;
begin
  //FErrorLog.Free;
  DeleteInterchanges;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFile.Dissassemble;
var
  I, StartPos, SearchResult: Integer;
begin
  DeleteInterchanges;

  if FDelimiters = nil then
  begin
    FDelimiters := TEDIDelimiters.Create;
    FEDIFileOptions := FEDIFileOptions + [foVariableDelimiterDetection];
  end;

  FData := StringReplace(FData, AnsiCrLf, '', [rfReplaceAll, rfIgnoreCase]);
  FData := StringReplace(FData, AnsiCarriageReturn, '', [rfReplaceAll, rfIgnoreCase]);
  FData := StringReplace(FData, AnsiLineFeed, '', [rfReplaceAll, rfIgnoreCase]);

  SearchResult := 0;
  StartPos := 1;
  //Search for Interchange Control Header
  if ICHSegmentId = Copy(FData, StartPos, Length(ICHSegmentId)) then
  begin
    SearchResult := 1;
    if (foVariableDelimiterDetection in FEDIFileOptions) then
    begin
      FDelimiters.ED := Copy(FData, Length(ICHSegmentId) + 1, 1);
      for I := 0 to 15 do
      begin
        SearchResult := StrSearch(FDelimiters.ED, FData, SearchResult);
        SearchResult := SearchResult + 1;
      end;
      FDelimiters.SS := Copy(FData, SearchResult, 1);
      FDelimiters.SD := Copy(FData, SearchResult + 1, 1);
    end;
  end
  else
  begin
    raise EJclEDIError.CreateResRec(@EDIError011);
  end;
  //Continue
  while (StartPos + Length(ICHSegmentId)) < Length(FData) do
  begin
    //Search for Interchange Control Trailer
    SearchResult := StrSearch(FDelimiters.SD + ICTSegmentId + FDelimiters.ED, FData, StartPos);
    if SearchResult > 0 then
    begin
      SearchResult := SearchResult + FDelimiters.SDLen; //Move past the delimiter
      //Search for the end of Interchange Control Trailer
      SearchResult := StrSearch(FDelimiters.SD, FData, SearchResult);
      if SearchResult > 0 then
      begin
        I := AddInterchange;
        FInterchanges[I].Delimiters :=
          TEDIDelimiters.Create(FDelimiters.SD, FDelimiters.ED, FDelimiters.SS);
        FInterchanges[I].Data :=
          Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.SDLen));
        FInterchanges[I].Dissassemble;
      end
      else
      begin
        raise EJclEDIError.CreateResRec(@EDIError012);
      end;
    end
    else
    begin
      raise EJclEDIError.CreateResRec(@EDIError013);
    end;
    //Set next start position, Move past the delimiter
    StartPos := SearchResult + FDelimiters.SDLen;
    //Verify the next record is an Interchange Control Header
    if ICHSegmentId = Copy(FData, StartPos, Length(ICHSegmentId)) then
    begin
      SearchResult := 1;
      if (foVariableDelimiterDetection in FEDIFileOptions) then
      begin
        FDelimiters.ED := Copy(FData, Length(ICHSegmentId) + 1, 1);
        for I := 0 to 15 do
        begin
          SearchResult := StrSearch(FDelimiters.ED, FData, SearchResult);
          SearchResult := SearchResult + 1;
        end;
        FDelimiters.SS := Copy(FData, SearchResult, 1);
        FDelimiters.SD := Copy(FData, SearchResult + 1, 1);
      end;
    end
    else if (StartPos + Length(ICHSegmentId)) < Length(FData) then
    begin
      raise EJclEDIError.CreateResRec(@EDIError014);
    end;
  end;
  FData := '';
end;

//--------------------------------------------------------------------------------------------------

function TEDIFile.GetInterchangeControl(Index: Integer): TEDIInterchangeControl;
begin
  Result := nil;
  if (Length(FInterchanges) > 0) then
    if (Index >= Low(FInterchanges)) then
      if (Index <= High(FInterchanges)) then
      begin
        if not Assigned(FInterchanges[Index]) then
        begin
          raise EJclEDIError.CreateResRecFmt(@EDIError007, [IntToStr(Index)]);
        end;
        Result := FInterchanges[Index];
      end
      else
        raise EJclEDIError.CreateResRecFmt(@EDIError008, [IntToStr(Index)])
    else
      raise EJclEDIError.CreateResRecFmt(@EDIError009, [IntToStr(Index)])
  else
    raise EJclEDIError.CreateResRecFmt(@EDIError010, [IntToStr(Index)]);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFile.InsertInterchange(InsertIndex: Integer;
  Interchange: TEDIInterchangeControl): Integer;
var
  I: Integer;
begin
  Result := InsertIndex;
  if (Length(FInterchanges) > 0) and (InsertIndex >= Low(FInterchanges)) and
    (InsertIndex <= High(FInterchanges)) then
  begin
    //Resize
    SetLength(FInterchanges, Length(FInterchanges) + 1);
    //Shift
    for I := High(FInterchanges) downto InsertIndex + 1 do
      FInterchanges[I] := FInterchanges[I-1];
    //Insert
    FInterchanges[InsertIndex] := nil;
    FInterchanges[InsertIndex] := Interchange;
    FInterchanges[InsertIndex].Parent := Self;
  end
  else
  begin
    Result := AppendInterchange(Interchange);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFile.InsertInterchange(InsertIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := InsertIndex;
  if (Length(FInterchanges) > 0) and (InsertIndex >= Low(FInterchanges)) and
    (InsertIndex <= High(FInterchanges)) then
  begin
    //Resize
    SetLength(FInterchanges, Length(FInterchanges) + 1);
    //Shift
    for I := High(FInterchanges) downto InsertIndex + 1 do
      FInterchanges[I] := FInterchanges[I-1];
    //Insert
    FInterchanges[InsertIndex] := nil;
    FInterchanges[InsertIndex] := TEDIInterchangeControl.Create(Self);
  end
  else
  begin
    Result := AddInterchange;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFile.InsertInterchanges(InsertIndex, Count: Integer): Integer;
var
  I: Integer;
begin
  Result := InsertIndex;
  if (Length(FInterchanges) > 0) and (InsertIndex >= Low(FInterchanges)) and
    (InsertIndex <= High(FInterchanges)) then
  begin
    //Resize
    SetLength(FInterchanges, Length(FInterchanges) + Count);
    //Shift
    for I := High(FInterchanges) downto InsertIndex + Count do
    begin
      FInterchanges[I] := FInterchanges[I-Count];
      FInterchanges[I-Count] := nil;
    end;
    //Insert
    for I := InsertIndex to (InsertIndex + Count) - 1 do
    begin
      FInterchanges[I] := TEDIInterchangeControl.Create(Self);
    end;
  end
  else
  begin
    Result := AddInterchanges(Count);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFile.InsertInterchanges(InsertIndex: Integer;
  InterchangeControlArray: TEDIInterchangeControlArray): Integer;
var
  I, J, K: Integer;
begin
  Result := InsertIndex;
  I := Length(InterchangeControlArray);
  if (Length(FInterchanges) > 0) and (InsertIndex >= Low(FInterchanges)) and
    (InsertIndex <= High(FInterchanges)) then
  begin
    //Resize
    SetLength(FInterchanges, Length(FInterchanges) + I);
    //Shift
    for J := High(FInterchanges) downto InsertIndex + I do
    begin
      FInterchanges[J] := FInterchanges[J-I];
      FInterchanges[J-I] := nil;
    end;
    //Insert
    K := 0;
    for J := InsertIndex to (InsertIndex + I) - 1 do
    begin
      FInterchanges[J] := InterchangeControlArray[K];
      FInterchanges[J].Parent := Self;
      Inc(K);
    end;
  end
  else
  begin
    Result := AppendInterchanges(InterchangeControlArray);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFile.InternalLoadFromFile;
var
//Old code
//  EDIFile: file;
//  Buffer: PChar;
  EDIFileStream: TFileStream;
begin
  FData := '';
  if FFileName <> '' then
  begin
    EDIFileStream := TFileStream.Create(FFileName, fmOpenReadWrite or fmShareDenyNone);
    try
      SetLength(FData, EDIFileStream.Size);
      EDIFileStream.Read(Pointer(FData)^, EDIFileStream.Size);
    finally
      EDIFileStream.Free;
    end;
    FData := StringReplace(FData, AnsiCrLf, '', [rfReplaceAll, rfIgnoreCase]);
//Old code      
//    AssignFile(EDIFile, FFileName);
//    try
//      Reset(EDIFile, 1);
//      Buffer := StrAlloc(FileSize(EDIFile));
//      BlockRead(EDIFile, Buffer^, FileSize(EDIFile));
//      FData := Buffer;
//      FData := StringReplace(FData, AnsiCrLf, '', [rfReplaceAll, rfIgnoreCase]);
//    finally
//      CloseFile(EDIFile);
//    end;
  end
  else
  begin
    raise EJclEDIError.CreateResRec(@EDIError006);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFile.LoadFromFile(const FileName: string);
begin
  FFileName := FileName;
  InternalLoadFromFile;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFile.ReLoadFromFile;
begin
  InternalLoadFromFile;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFile.SaveAsToFile(const FileName: string);
var
//Old code
//  EDIFile: file;
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
//Old code
//    AssignFile(EDIFile, FileName);
//    try
//      ReWrite(EDIFile, 1);
//      BlockWrite(EDIFile, FData, Length(FData));
//    finally
//      CloseFile(EDIFile);
//    end;
  end
  else
  begin
    raise EJclEDIError.CreateResRec(@EDIError005);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFile.SaveToFile;
var
//Old code
//  EDIFile: file;
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
//Old code
//    AssignFile(EDIFile, FFileName);
//    try
//      ReWrite(EDIFile, 1);
//      BlockWrite(EDIFile, FData, Length(FData));
//    finally
//      CloseFile(EDIFile);
//    end;
  end
  else
  begin
    raise EJclEDIError.CreateResRec(@EDIError004);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFile.SetInterchangeControl(Index: Integer; Interchange: TEDIInterchangeControl);
begin
  if (Length(FInterchanges) > 0) then
    if (Index >= Low(FInterchanges)) then
      if (Index <= High(FInterchanges)) then
      begin
        if Assigned(FInterchanges[Index]) then
        begin
          FInterchanges[Index].Free;
          FInterchanges[Index] := nil;
        end;
        FInterchanges[Index] := Interchange
      end
      else
        raise EJclEDIError.CreateResRecFmt(@EDIError001, [IntToStr(Index)])
    else
      raise EJclEDIError.CreateResRecFmt(@EDIError002, [IntToStr(Index)])
  else
    raise EJclEDIError.CreateResRecFmt(@EDIError003, [IntToStr(Index)]);
end;

//==================================================================================================
// TEDIInterchangeControlSegment
//==================================================================================================

constructor TEDIInterchangeControlSegment.Create(Parent: TEDIDataObject);
begin
  inherited;
  if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
  begin
    FParent := Parent;
  end;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIInterchangeControlSegment.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited;
  if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
  begin
    FParent := Parent;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControlSegment.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  //Attempt to assign the delimiters
  if not Assigned(FDelimiters) then
  begin
    //Get the delimiters from the interchange control
    if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
    begin
      if Assigned(Parent.Delimiters) then
      begin
        Result := Parent.Delimiters;
      end;
    end;
  end;
end;

//==================================================================================================
// TEDIFunctionalGroupSegment
//==================================================================================================

constructor TEDIFunctionalGroupSegment.Create(Parent: TEDIDataObject);
begin
  inherited;
  if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
  begin
    FParent := Parent;
  end;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIFunctionalGroupSegment.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited;
  if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
  begin
    FParent := Parent;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroupSegment.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  //Attempt to assign the delimiters
  if not Assigned(FDelimiters) then
  begin
    //Get the delimiters from the functional group
    if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
    begin
      if Assigned(Parent.Delimiters) then
      begin
        Result := Parent.Delimiters;
        Exit;
      end;
      //Get the delimiters from the interchange control
      if Assigned(Parent.Parent) and (Parent.Parent is TEDIInterchangeControl) then
      begin
        if Assigned(Parent.Parent.Delimiters) then
        begin
          Result := Parent.Parent.Delimiters;
        end;
      end;
    end;
  end;
end;

//==================================================================================================
// TEDITransactionSetSegment
//==================================================================================================

constructor TEDITransactionSetSegment.Create(Parent: TEDIDataObject);
begin
  inherited;
  if Assigned(Parent) and (Parent is TEDITransactionSet) then
  begin
    FParent := Parent;
  end;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDITransactionSetSegment.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited;
  if Assigned(Parent) and (Parent is TEDITransactionSet) then
  begin
    FParent := Parent;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSetSegment.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := inherited InternalAssignDelimiters;
end;

//==================================================================================================
// TEDIElementSpec
//==================================================================================================

function TEDIElementSpec.Assemble: string;
var
  Data: TStrings;
begin
  Data := TStringList.Create;
  if FId <> 'Reserved' then
  begin
    Data.Values['Id'] := FId;
    Data.Values['Position'] := IntToStr(FPosition);
    Data.Values['Description'] := FDescription;
    Data.Values['RequirementDesignator'] := FRequirementDesignator;
    Data.Values['Type'] := FType;
    Data.Values['MinimumLength'] := IntToStr(FMinimumLength);
    Data.Values['MaximumLength'] := IntToStr(FMaximumLength);
    FData := Data.Text;    
  end;
  Result := FData;
  Data.Free;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIElementSpec.Dissassemble;
var
  Data: TStrings;
begin
  Data := TStringList.Create;
  Data.Text := FData;
  if Data.Values['Id'] <> 'Reserved' then
  begin
    FId := Data.Values['Id'];
    FPosition := StrToInt(Data.Values['Position']);
    FDescription := Data.Values['Description'];
    FRequirementDesignator := Data.Values['RequirementDesignator'];
    FType := Data.Values['Type'];
    FMinimumLength := StrToInt(Data.Values['MinimumLength']);
    FMaximumLength := StrToInt(Data.Values['MaximumLength']);
  end;
  Data.Free;
end;

//==================================================================================================
// TEDISegmentSpec
//==================================================================================================

function TEDISegmentSpec.AddElement: Integer;
begin
  SetLength(FElements, Length(FElements) + 1);
  FElements[High(FElements)] := TEDIElementSpec.Create(Self);
  Result := High(FElements); //Return position of element
end;

//--------------------------------------------------------------------------------------------------

function TEDISegmentSpec.AddElements(Count: Integer): Integer;
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
    FElements[J]:= TEDIElementSpec.Create(Self);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDISegmentSpec.Assemble: string;
var
  ReservedData: TStrings;
begin
  //Insert Segment Spec as Element[0]
  InsertElement(0);
  TEDIElementSpec(FElements[0]).Id := 'Reserved';
  ReservedData := TStringList.Create;
  ReservedData.Values['Id'] := 'Reserved';
  ReservedData.Values['Position'] := IntToStr(FPosition);
  ReservedData.Values['Description'] := FDescription;
  ReservedData.Values['Section'] := FSection;
  ReservedData.Values['RequirementDesignator'] := FRequirementDesignator;
  ReservedData.Values['MaximumUsage'] := IntToStr(FMaximumUsage);
  FElements[0].Data := ReservedData.Text;
  ReservedData.Free;
  //
  Result := inherited Assemble;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISegmentSpec.Dissassemble;
var
  ReservedData: TStrings;
begin
  inherited Dissassemble;
  //Element[0] is always the Segment Spec
  ReservedData := TStringList.Create;
  ReservedData.Text := FElements[0].Data;
  //FSegmentId already set by the inherited Dissassemble
  FPosition := StrToInt(ReservedData.Values['Position']);
  FDescription := ReservedData.Values['Description'];
  FSection := ReservedData.Values['Section'];
  FRequirementDesignator := ReservedData.Values['RequirementDesignator'];
  FMaximumUsage := StrToInt(ReservedData.Values['MaximumUsage']);
  ReservedData.Free;
  DeleteElement(0);
end;

//--------------------------------------------------------------------------------------------------

function TEDISegmentSpec.InsertElement(InsertIndex: Integer): Integer;
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
    FElements[InsertIndex] := TEDIElementSpec.Create(Self);
  end
  else
  begin
    Result := AddElement;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDISegmentSpec.InsertElements(InsertIndex, Count: Integer): Integer;
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
      FElements[I] := TEDIElementSpec.Create(Self);
    end;
  end
  else
  begin
    Result := AddElements(Count);
  end;
end;

//==================================================================================================
// TEDIInterchangeControlSegmentSpec
//==================================================================================================

constructor TEDIInterchangeControlSegmentSpec.Create(Parent: TEDIDataObject);
begin
  inherited;
  if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
  begin
    FParent := Parent;
  end;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIInterchangeControlSegmentSpec.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited;
  if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
  begin
    FParent := Parent;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControlSegmentSpec.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  //Attempt to assign the delimiters
  if not Assigned(FDelimiters) then
  begin
    //Get the delimiters from the interchange control
    if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
    begin
      if Assigned(Parent.Delimiters) then
      begin
        Result := Parent.Delimiters;
      end;
    end;
  end;
end;

//==================================================================================================
// TEDIFunctionalGroupSegmentSpec
//==================================================================================================

constructor TEDIFunctionalGroupSegmentSpec.Create(Parent: TEDIDataObject);
begin
  inherited;
  if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
  begin
    FParent := Parent;
  end;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIFunctionalGroupSegmentSpec.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited;
  if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
  begin
    FParent := Parent;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroupSegmentSpec.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  //Attempt to assign the delimiters
  if not Assigned(FDelimiters) then
  begin
    //Get the delimiters from the functional group
    if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
    begin
      if Assigned(Parent.Delimiters) then
      begin
        Result := Parent.Delimiters;
        Exit;
      end;
      //Get the delimiters from the interchange control
      if Assigned(Parent.Parent) and (Parent.Parent is TEDIInterchangeControl) then
      begin
        if Assigned(Parent.Parent.Delimiters) then
        begin
          Result := Parent.Parent.Delimiters;
        end;
      end;
    end;
  end;
end;

//==================================================================================================
// TEDITransactionSetSegmentSpec
//==================================================================================================

constructor TEDITransactionSetSegmentSpec.Create(Parent: TEDIDataObject);
begin
  inherited;
  if Assigned(Parent) and (Parent is TEDITransactionSet) then
  begin
    FParent := Parent;
  end;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDITransactionSetSegmentSpec.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited;
  if Assigned(Parent) and (Parent is TEDITransactionSet) then
  begin
    FParent := Parent;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSetSegmentSpec.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := inherited InternalAssignDelimiters;
end;

end.
