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
{ Contains classes to easily parse EDI documents and data. Variable delimiter detection allows     }
{ parsing of the file without knowledge of the standards at an Interchange level.  This enables    }
{ parsing and construction of EDI documents with different delimiters.  Various EDI  file errors   }
{ can also be detected.                                                                            }
{                                                                                                  }
{ Unit owner: Raymond Alexander                                                                    }
{ Date created: May 22, 2003                                                                       }
{ Last modified: July 31, 2003                                                                     }
{ Additional Info:                                                                                 }
{   E-Mail at RaysDelphiBox3@hotmail.com                                                           }
{   For latest EDI specific updates see http://sourceforge.net/projects/edisdk                     }
{   See home page for latest news & events and online help.                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ 04/21/2003 (R.A.)                                                                                }
{                                                                                                  }
{   Release notes have been moved to ReleaseNotes.rtf                                              }
{                                                                                                  }
{**************************************************************************************************}

unit JclEDI_UNEDIFACT;

{$I jcl.inc}

{$WEAKPACKAGEUNIT ON}

{$DEFINE OPTIMIZED_DISASSEMBLE}
{$DEFINE OPTIMIZED_STRINGREPLACE}

interface

uses
  SysUtils, Classes,
  JclBase, JclStrings, JclEDI;

const
  UNASegmentId = 'UNA';  // Service String Advice Segment Id
  UNBSegmentId = 'UNB';  // Interchange Control Header Segment Id
  UNZSegmentId = 'UNZ';  // Interchange Control Trailer Segment Id
  UNGSegmentId = 'UNG';  // Functional Group Header Segment Id
  UNESegmentId = 'UNE';  // Functional Group Trailer Segment Id
  UNHSegmentId = 'UNH';  // Message (Transaction Set) Header Segment Id
  UNTSegmentId = 'UNT';  // Message (Transaction Set) Trailer Segment Id

//--------------------------------------------------------------------------------------------------
//  EDI Forward Class Declarations
//--------------------------------------------------------------------------------------------------
type
  TEDIElement = class;
  TEDICompositeElement = class;
  TEDISegment = class;
  TEDIMessage = class; // (Transaction Set)
  TEDIFunctionalGroup = class;
  TEDIInterchangeControl = class;
  TEDIFile = class;

//--------------------------------------------------------------------------------------------------
//  EDI Element
//--------------------------------------------------------------------------------------------------

  TEDIElementArray = array of TEDIElement;

  TEDIElement = class(TEDIDataObject)
  public
    constructor Create(Parent: TEDIDataObject); reintroduce;
    function Assemble: string; override;
    procedure Disassemble; override;
    function GetIndexPositionFromParent: Integer;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Composite Element Classes
//--------------------------------------------------------------------------------------------------

  TEDICompositeElementArray = array of TEDICompositeElement;

  TEDICompositeElement = class(TEDIDataObjectGroup)
  private
    function GetElement(Index: Integer): TEDIElement;
    procedure SetElement(Index: Integer; Element: TEDIElement);
    function GetElements: TEDIElementArray;
    procedure SetElements(const Value: TEDIElementArray);
  protected
    function InternalCreateElement: TEDIElement; virtual;
    function InternalAssignDelimiters: TEDIDelimiters; override;
    function InternalCreateEDIDataObject: TEDIDataObject; override;
  public
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer = 0); reintroduce;
    destructor Destroy; override;
    //
    function AddElement: Integer;
    function AppendElement(Element: TEDIElement): Integer;
    function InsertElement(InsertIndex: Integer): Integer; overload;
    function InsertElement(InsertIndex: Integer; Element: TEDIElement): Integer; overload;
    procedure DeleteElement(Index: Integer); overload;
    procedure DeleteElement(Element: TEDIElement); overload;
    //
    function AddElements(Count: Integer): Integer;
    function AppendElements(ElementArray: TEDIElementArray): Integer;
    function InsertElements(InsertIndex, Count: Integer): Integer; overload;
    function InsertElements(InsertIndex: Integer;
      ElementArray: TEDIElementArray): Integer; overload;
    procedure DeleteElements; overload;
    procedure DeleteElements(Index, Count: Integer); overload;
    //
    function Assemble: string; override;
    procedure Disassemble; override;
    //
    property Element[Index: Integer]: TEDIElement read GetElement write SetElement; default;
    property Elements: TEDIElementArray read GetElements write SetElements;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Segment Classes
//--------------------------------------------------------------------------------------------------

  TEDISegmentArray = array of TEDISegment;

  TEDISegment = class(TEDIDataObjectGroup)
  private
    FSegmentID: string;
    //FSegmentIdData: TEDICompositeElement; // ToDo: ex: AAA:1:1:2+data1+data2'
  protected
    function InternalCreateElement: TEDIElement; virtual;
    function InternalCreateCompositeElement: TEDICompositeElement; virtual;
    function InternalAssignDelimiters: TEDIDelimiters; override;
    function InternalCreateEDIDataObject: TEDIDataObject; override;
  public
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer = 0); reintroduce;
    destructor Destroy; override;
    //
    function AddElement: Integer;
    function AppendElement(Element: TEDIElement): Integer;
    function InsertElement(InsertIndex: Integer): Integer; overload;
    function InsertElement(InsertIndex: Integer; Element: TEDIElement): Integer; overload;
    procedure DeleteElement(Index: Integer); overload;
    procedure DeleteElement(Element: TEDIElement); overload;
    //
    function AddElements(Count: Integer): Integer;
    function AppendElements(ElementArray: TEDIElementArray): Integer;
    function InsertElements(InsertIndex, Count: Integer): Integer; overload;
    function InsertElements(InsertIndex: Integer;
      ElementArray: TEDIElementArray): Integer; overload;
    procedure DeleteElements; overload;
    procedure DeleteElements(Index, Count: Integer); overload;
    //
    function AddCompositeElement: Integer;
    function AppendCompositeElement(CompositeElement: TEDICompositeElement): Integer;
    function InsertCompositeElement(InsertIndex: Integer): Integer; overload;
    function InsertCompositeElement(InsertIndex: Integer;
      CompositeElement: TEDICompositeElement): Integer; overload;
    //
    function AddCompositeElements(Count: Integer): Integer;
    function AppendCompositeElements(CompositeElementArray: TEDICompositeElementArray): Integer;
    function InsertCompositeElements(InsertIndex, Count: Integer): Integer; overload;
    function InsertCompositeElements(InsertIndex: Integer;
      CompositeElementArray: TEDICompositeElementArray): Integer; overload;
    //
    function Assemble: string; override;
    procedure Disassemble; override;
  published
    property SegmentID: string read FSegmentID write FSegmentID;
  end;

  TEDIMessageSegment = class(TEDISegment)
  public
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer = 0); reintroduce;
    function InternalAssignDelimiters: TEDIDelimiters; override;
  end;

  TEDIFunctionalGroupSegment = class(TEDISegment)
  public
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer = 0); reintroduce;
    function InternalAssignDelimiters: TEDIDelimiters; override;
  end;

  TEDIInterchangeControlSegment = class(TEDISegment)
  public
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer = 0); reintroduce;
    function InternalAssignDelimiters: TEDIDelimiters; override;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Message (Transaction Set)
//--------------------------------------------------------------------------------------------------

  TEDIMessageArray = array of TEDIMessage;

  TEDIMessage = class(TEDIDataObjectGroup)
  private
    FUNHSegment: TEDIMessageSegment;
    FUNTSegment: TEDIMessageSegment;
    function GetSegment(Index: Integer): TEDISegment;
    procedure SetSegment(Index: Integer; Segment: TEDISegment);
    function GetSegments: TEDISegmentArray;
    procedure SetSegments(const Value: TEDISegmentArray);
    procedure SetUNHSegment(const UNHSegment: TEDIMessageSegment);
    procedure SetUNTSegment(const UNTSegment: TEDIMessageSegment);
  protected
    procedure InternalCreateHeaderTrailerSegments; virtual;
    function InternalCreateSegment: TEDISegment; virtual;
    function InternalAssignDelimiters: TEDIDelimiters; override;
    function InternalCreateEDIDataObject: TEDIDataObject; override;
  public
    constructor Create(Parent: TEDIDataObject; SegmentCount: Integer = 0); reintroduce;
    destructor Destroy; override;

    function AddSegment: Integer;
    function AppendSegment(Segment: TEDISegment): Integer;
    function InsertSegment(InsertIndex: Integer): Integer; overload;
    function InsertSegment(InsertIndex: Integer; Segment: TEDISegment): Integer; overload;
    procedure DeleteSegment(Index: Integer); overload;
    procedure DeleteSegment(Segment: TEDISegment); overload;

    function AddSegments(Count: Integer): Integer;
    function AppendSegments(SegmentArray: TEDISegmentArray): Integer;
    function InsertSegments(InsertIndex, Count: Integer): Integer; overload;
    function InsertSegments(InsertIndex: Integer;
      SegmentArray: TEDISegmentArray): Integer; overload;
    procedure DeleteSegments; overload;
    procedure DeleteSegments(Index, Count: Integer); overload;

    function Assemble: string; override;
    procedure Disassemble; override;

    property Segment[Index: Integer]: TEDISegment read GetSegment write SetSegment; default;
    property Segments: TEDISegmentArray read GetSegments write SetSegments;
  published
    property SegmentUNH: TEDIMessageSegment read FUNHSegment write SetUNHSegment;
    property SegmentUNT: TEDIMessageSegment read FUNTSegment write SetUNTSegment;
    property SegmentCount: Integer read GetCount;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Functional Group
//--------------------------------------------------------------------------------------------------

  TEDIFunctionalGroupArray = array of TEDIFunctionalGroup;

  TEDIFunctionalGroup = class(TEDIDataObjectGroup)
  private
    FUNGSegment: TEDIFunctionalGroupSegment;
    FUNESegment: TEDIFunctionalGroupSegment;
    function GetMessage(Index: Integer): TEDIMessage;
    procedure SetMessage(Index: Integer; Message: TEDIMessage);
    function GetMessages: TEDIMessageArray;
    procedure SetMessages(const Value: TEDIMessageArray);
    procedure SetUNGSegment(const UNGSegment: TEDIFunctionalGroupSegment);
    procedure SetUNESegment(const UNESegment: TEDIFunctionalGroupSegment);
  protected
    procedure InternalCreateHeaderTrailerSegments; virtual;
    function InternalCreateMessage: TEDIMessage; virtual;
    function InternalAssignDelimiters: TEDIDelimiters; override;
    function InternalCreateEDIDataObject: TEDIDataObject; override;
  public
    constructor Create(Parent: TEDIDataObject; MessageCount: Integer = 0); reintroduce;
    destructor Destroy; override;

    function AddMessage: Integer;
    function AppendMessage(Message: TEDIMessage): Integer;
    function InsertMessage(InsertIndex: Integer): Integer; overload;
    function InsertMessage(InsertIndex: Integer;
      Message: TEDIMessage): Integer; overload;
    procedure DeleteMessage(Index: Integer); overload;
    procedure DeleteMessage(Message: TEDIMessage); overload;

    function AddMessages(Count: Integer): Integer;
    function AppendMessages(MessageArray: TEDIMessageArray): Integer;
    function InsertMessages(InsertIndex, Count: Integer): Integer; overload;
    function InsertMessages(InsertIndex: Integer;
      MessageArray: TEDIMessageArray): Integer; overload;
    procedure DeleteMessages; overload;
    procedure DeleteMessages(Index, Count: Integer); overload;

    function Assemble: string; override;
    procedure Disassemble; override;

    property Message[Index: Integer]: TEDIMessage read GetMessage
      write SetMessage; default;
    property Messages: TEDIMessageArray read GetMessages
      write SetMessages;
  published
    property SegmentUNG: TEDIFunctionalGroupSegment read FUNGSegment write SetUNGSegment;
    property SegmentUNE: TEDIFunctionalGroupSegment read FUNESegment write SetUNESegment;
    property MessageCount: Integer read GetCount;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Interchange Control
//--------------------------------------------------------------------------------------------------

  TEDIInterchangeControlArray = array of TEDIInterchangeControl;

  TEDIInterchangeControl = class(TEDIDataObjectGroup)
  private
    FUNASegment: TEDIInterchangeControlSegment;
    FUNBSegment: TEDIInterchangeControlSegment;
    FUNZSegment: TEDIInterchangeControlSegment;
    procedure SetUNBSegment(const UNBSegment: TEDIInterchangeControlSegment);
    procedure SetUNZSegment(const UNZSegment: TEDIInterchangeControlSegment);
  protected
    FCreateObjectType: TEDIDataObjectType;
    procedure InternalCreateHeaderTrailerSegments; virtual;
    function InternalCreateFunctionalGroup: TEDIFunctionalGroup; virtual;
    function InternalCreateMessage: TEDIMessage; virtual;
    function InternalAssignDelimiters: TEDIDelimiters; override;
    function InternalCreateEDIDataObject: TEDIDataObject; override;
  public
    constructor Create(Parent: TEDIDataObject; FunctionalGroupCount: Integer = 0); reintroduce;
    destructor Destroy; override;

    function AddFunctionalGroup: Integer;
    function AppendFunctionalGroup(FunctionalGroup: TEDIFunctionalGroup): Integer;
    function InsertFunctionalGroup(InsertIndex: Integer): Integer; overload;
    function InsertFunctionalGroup(InsertIndex: Integer;
      FunctionalGroup: TEDIFunctionalGroup): Integer; overload;

    function AddFunctionalGroups(Count: Integer): Integer;
    function AppendFunctionalGroups(FunctionalGroupArray: TEDIFunctionalGroupArray): Integer;
    function InsertFunctionalGroups(InsertIndex, Count: Integer): Integer; overload;
    function InsertFunctionalGroups(InsertIndex: Integer;
      FunctionalGroupArray: TEDIFunctionalGroupArray): Integer; overload;

    function AddMessage: Integer;
    function AppendMessage(Message: TEDIMessage): Integer;
    function InsertMessage(InsertIndex: Integer): Integer; overload;
    function InsertMessage(InsertIndex: Integer; Message: TEDIMessage): Integer; overload;

    function AddMessages(Count: Integer): Integer;
    function AppendMessages(MessageArray: TEDIMessageArray): Integer;
    function InsertMessages(InsertIndex, Count: Integer): Integer; overload;
    function InsertMessages(InsertIndex: Integer; MessageArray: TEDIMessageArray): Integer; overload;

    function Assemble: string; override;
    procedure Disassemble; override;
  published
    property SegmentUNA: TEDIInterchangeControlSegment read FUNASegment;
    property SegmentUNB: TEDIInterchangeControlSegment read FUNBSegment write SetUNBSegment;
    property SegmentUNZ: TEDIInterchangeControlSegment read FUNZSegment write SetUNZSegment;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI File
//--------------------------------------------------------------------------------------------------

  TEDIFileArray = array of TEDIFile;

  TEDIFileOptions = set of (foVariableDelimiterDetection, foRemoveCrLf, foRemoveCr, foRemoveLf);

  TEDIFile = class(TEDIDataObjectGroup)
  private
    FFileID: Integer;
    FFileName: string;
    FEDIFileOptions: TEDIFileOptions;
    function GetInterchangeControl(Index: Integer): TEDIInterchangeControl;
    procedure SetInterchangeControl(Index: Integer; Interchange: TEDIInterchangeControl);
    procedure InternalLoadFromFile;
    function GetInterchanges: TEDIInterchangeControlArray;
    procedure SetInterchanges(const Value: TEDIInterchangeControlArray);
  protected
    procedure InternalDelimitersDetection(StartPos: Integer); virtual;
    procedure InternalAlternateDelimitersDetection(StartPos: Integer);
    function InternalCreateInterchangeControl: TEDIInterchangeControl; virtual;
    function InternalAssignDelimiters: TEDIDelimiters; override;
    function InternalCreateEDIDataObject: TEDIDataObject; override;
  public
    constructor Create(Parent: TEDIDataObject; InterchangeCount: Integer = 0); reintroduce;
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
    procedure DeleteInterchange(Index: Integer); overload;
    procedure DeleteInterchange(Interchange: TEDIInterchangeControl); overload;

    function AddInterchanges(Count: Integer): Integer;
    function AppendInterchanges(
      InterchangeControlArray: TEDIInterchangeControlArray): Integer;
    function InsertInterchanges(InsertIndex, Count: Integer): Integer; overload;
    function InsertInterchanges(InsertIndex: Integer;
      InterchangeControlArray: TEDIInterchangeControlArray): Integer; overload;
    procedure DeleteInterchanges; overload;
    procedure DeleteInterchanges(Index, Count: Integer); overload;

    function Assemble: string; override;
    procedure Disassemble; override;

    property Interchange[Index: Integer]: TEDIInterchangeControl read GetInterchangeControl
      write SetInterchangeControl; default;
    property Interchanges: TEDIInterchangeControlArray read GetInterchanges
      write SetInterchanges;
  published
    property FileID: Integer read FFileID write FFileID;
    property FileName: string read FFileName write FFileName;
    property Options: TEDIFileOptions read FEDIFileOptions write FEDIFileOptions;
    property InterchangeControlCount: Integer read GetCount;
  end;

implementation

uses
  JclResources;

//==================================================================================================
// TEDIElement
//==================================================================================================

constructor TEDIElement.Create(Parent: TEDIDataObject);
begin
  if Assigned(Parent) and (Parent is TEDISegment) then
    inherited Create(Parent)
  else
    inherited Create(nil);
  FEDIDOT := ediElement;
end;

//--------------------------------------------------------------------------------------------------

function TEDIElement.Assemble: string;
begin
  Result := FData;
  FState := ediAssembled;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIElement.Disassemble;
begin
  FState := ediDissassembled;
end;

//--------------------------------------------------------------------------------------------------

function TEDIElement.GetIndexPositionFromParent: Integer;
var
  I: Integer;
begin
  Result := -1;
  if Assigned(Parent) and (Parent is TEDISegment) then
    for I := Low(TEDISegment(Parent).EDIDataObjects) to High(TEDISegment(Parent).EDIDataObjects) do
      if TEDISegment(Parent).EDIDataObjects[I] = Self then
      begin
        Result := I;
        Break;
      end;
end;

//==================================================================================================
// TEDISegment
//==================================================================================================

constructor TEDISegment.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  if Assigned(Parent) and (Parent is TEDIMessage) then
    inherited Create(Parent, ElementCount)
  else
    inherited Create(nil, ElementCount);
  FSegmentID := '';
  FEDIDOT := ediSegment;
  FCreateObjectType := ediElement;
//  FSegmentIdData := TEDICompositeElement.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISegment.Destroy;
begin
//  FSegmentIdData.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.AddElements(Count: Integer): Integer;
begin
  FCreateObjectType := ediElement;
  Result := AddEDIDataObjects(Count);
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.AddElement: Integer;
begin
  FCreateObjectType := ediElement;
  Result := AddEDIDataObject;
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.AppendElement(Element: TEDIElement): Integer;
begin
  Result := AppendEDIDataObject(Element);
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.AppendElements(ElementArray: TEDIElementArray): Integer;
begin
  Result := AppendEDIDataObjects(TEDIDataObjectArray(ElementArray));
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.Assemble: string;
var
  I: Integer;
begin
  FData := '';
  FLength := 0;
  Result := '';

  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateResRec(@RsEDIError036);
  end;

  FData := FSegmentID;
  if Length(FEDIDataObjects) > 0 then
    for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
      if Assigned(FEDIDataObjects[I]) then
        FData := FData + FDelimiters.ED + FEDIDataObjects[I].Assemble;
      else
        FData := FData + FDelimiters.ED;
  FData := FData + FDelimiters.SD;
  FLength := Length(FData);
  Result := FData; // Return assembled string

  DeleteElements;

  FState := ediAssembled;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISegment.DeleteElement(Index: Integer);
begin
  DeleteEDIDataObject(Index);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISegment.DeleteElement(Element: TEDIElement);
begin
  DeleteEDIDataObject(Element);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISegment.DeleteElements(Index, Count: Integer);
begin
  DeleteEDIDataObjects(Index, Count);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISegment.DeleteElements;
begin
  DeleteEDIDataObjects;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISegment.Disassemble;
var
  I, StartPos, SearchResult: Integer;
  ElementData: string;
begin
  // Data Input Scenarios
  // 4.)  SegID+data+data'
  // Composite Element Data Input Secnarios
  // 9.)  SegID+data+data:data'
  FSegmentID := '';
  DeleteElements;
  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateResRec(@RsEDIError035);
  end;
  // Continue
  StartPos := 1;
  SearchResult := StrSearch(FDelimiters.ED, FData, StartPos);
  FSegmentID := Copy(FData, 1, SearchResult - 1);
  StartPos := SearchResult + 1;
  SearchResult := StrSearch(FDelimiters.ED, FData, StartPos);
  while SearchResult <> 0 do
  begin
    ElementData := Copy(FData, ((StartPos + FDelimiters.EDLen) - 1), (SearchResult - StartPos));
    if StrSearch(FDelimiters.SS, ElementData, 1) <= 0 then
      I := AddElement
    else
      I := AddCompositeElement;
    if (SearchResult - StartPos) > 0 then // data exists
    begin
      FEDIDataObjects[I].Data := ElementData;
      FEDIDataObjects[I].Disassemble;
    end;
    StartPos := SearchResult + 1;
    SearchResult := StrSearch(FDelimiters.ED, FData, StartPos);
  end;
  // Get last element before next segment
  SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
  if SearchResult <> 0 then
    if (SearchResult - StartPos) > 0 then // data exists
    begin
      ElementData := Copy(FData, ((StartPos + FDelimiters.EDLen) - 1),
        (SearchResult - StartPos));
      if StrSearch(FDelimiters.SS, ElementData, 1) <= 0 then
        I := AddElement
      else
        I := AddCompositeElement;
      FEDIDataObjects[I].Data := ElementData;
      FEDIDataObjects[I].Disassemble;
    end;
  FData := '';

  FState := ediDissassembled;
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.InsertElement(InsertIndex: Integer): Integer;
begin
  FCreateObjectType := ediElement;
  Result := InsertEDIDataObject(InsertIndex);
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.InsertElement(InsertIndex: Integer; Element: TEDIElement): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex, Element);
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.InsertElements(InsertIndex: Integer; ElementArray: TEDIElementArray): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, TEDIDataObjectArray(ElementArray));
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.InsertElements(InsertIndex, Count: Integer): Integer;
begin
  FCreateObjectType := ediElement;
  Result := InsertEDIDataObjects(InsertIndex, Count);
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
  begin
    // Get the delimiters from the Message
    if Assigned(Parent) and (Parent is TEDIMessage) then
    begin
      if Assigned(Parent.Delimiters) then
      begin
        Result := Parent.Delimiters;
        Exit;
      end;
      // Get the delimiters from the functional group
      if Assigned(Parent.Parent) and (Parent.Parent is TEDIFunctionalGroup) then
      begin
        if Assigned(Parent.Parent.Delimiters) then
        begin
          Result := Parent.Parent.Delimiters;
          Exit;
        end;
        // Get the delimiters from the interchange control header
        if Assigned(Parent.Parent.Parent) and (Parent.Parent.Parent is TEDIInterchangeControl) then
          if Assigned(Parent.Parent.Parent.Delimiters) then
            Result := Parent.Parent.Parent.Delimiters;
      end;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.InternalCreateElement: TEDIElement;
begin
  Result := TEDIElement.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.InternalCreateEDIDataObject: TEDIDataObject;
begin
  case FCreateObjectType of
    ediElement:
      Result := InternalCreateElement;
    ediCompositeElement:
      Result := InternalCreateCompositeElement;
  else
    Result := nil;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.InternalCreateCompositeElement: TEDICompositeElement;
begin
  Result := TEDICompositeElement.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.AddCompositeElement: Integer;
begin
  FCreateObjectType := ediCompositeElement;
  Result := AddEDIDataObject;
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.AddCompositeElements(Count: Integer): Integer;
begin
  FCreateObjectType := ediCompositeElement;
  Result := AddEDIDataObjects(Count);
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.AppendCompositeElement(CompositeElement: TEDICompositeElement): Integer;
begin
  Result := AppendEDIDataObject(CompositeElement);
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.AppendCompositeElements(
  CompositeElementArray: TEDICompositeElementArray): Integer;
begin
  Result := AppendEDIDataObjects(TEDIDataObjectArray(CompositeElementArray));
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.InsertCompositeElement(InsertIndex: Integer): Integer;
begin
  FCreateObjectType := ediCompositeElement;
  Result := InsertEDIDataObject(InsertIndex);
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.InsertCompositeElement(InsertIndex: Integer;
  CompositeElement: TEDICompositeElement): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex, CompositeElement);
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.InsertCompositeElements(InsertIndex, Count: Integer): Integer;
begin
  FCreateObjectType := ediCompositeElement;
  Result := InsertEDIDataObjects(InsertIndex, Count);
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.InsertCompositeElements(InsertIndex: Integer;
  CompositeElementArray: TEDICompositeElementArray): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, TEDIDataObjectArray(CompositeElementArray));
end;

//==================================================================================================
// TEDIMessageSegment
//==================================================================================================

constructor TEDIMessageSegment.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  if Assigned(Parent) and (Parent is TEDIMessage) then
    FParent := Parent;
end;

//--------------------------------------------------------------------------------------------------

function TEDIMessageSegment.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := inherited InternalAssignDelimiters;
end;

//==================================================================================================
// TEDIFunctionalGroupSegment
//==================================================================================================

constructor TEDIFunctionalGroupSegment.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
    FParent := Parent;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroupSegment.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  // Attempt to assign the delimiters
  if not Assigned(FDelimiters) then
  begin
    // Get the delimiters from the functional group
    if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
    begin
      if Assigned(Parent.Delimiters) then
      begin
        Result := Parent.Delimiters;
        Exit;
      end;
      // Get the delimiters from the interchange control
      if Assigned(Parent.Parent) and (Parent.Parent is TEDIInterchangeControl) then
        if Assigned(Parent.Parent.Delimiters) then
          Result := Parent.Parent.Delimiters;
    end;
  end;
end;

//==================================================================================================
// TEDIInterchangeControlSegment
//==================================================================================================

constructor TEDIInterchangeControlSegment.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
    FParent := Parent;
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControlSegment.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  // Attempt to assign the delimiters
  if not Assigned(FDelimiters) then
  begin
    // Get the delimiters from the interchange control
    if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
      if Assigned(Parent.Delimiters) then
        Result := Parent.Delimiters;
  end;
end;

//==================================================================================================
// TEDIMessage
//==================================================================================================

function TEDIMessage.AddSegment: Integer;
begin
  Result := AddEDIDataObject;
end;

//--------------------------------------------------------------------------------------------------

function TEDIMessage.AddSegments(Count: Integer): Integer;
begin
  Result := AddEDIDataObjects(Count);
end;

//--------------------------------------------------------------------------------------------------

function TEDIMessage.AppendSegment(Segment: TEDISegment): Integer;
begin
  Result := AppendEDIDataObject(Segment);
end;

//--------------------------------------------------------------------------------------------------

function TEDIMessage.AppendSegments(SegmentArray: TEDISegmentArray): Integer;
begin
  Result := AppendEDIDataObjects(TEDIDataObjectArray(SegmentArray));
end;

//--------------------------------------------------------------------------------------------------

function TEDIMessage.Assemble: string;
var
  I: Integer;
begin
  FData := '';
  FLength := 0;
  Result := '';
  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateResRec(@RsEDIError031);
  end;

  FData := FUNHSegment.Assemble;
  FUNHSegment.Data := '';

  if Length(FEDIDataObjects) > 0 then
    for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
      if Assigned(FEDIDataObjects[I]) then
        FData := FData + FEDIDataObjects[I].Assemble;

  DeleteSegments;

  FData := FData + FUNTSegment.Assemble;
  FUNTSegment.Data := '';

  FLength := Length(FData);
  Result := FData;

  FState := ediAssembled;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIMessage.Create(Parent: TEDIDataObject; SegmentCount: Integer);
begin
  if Assigned(Parent) and
    ((Parent is TEDIFunctionalGroup) or (Parent is TEDIInterchangeControl)) then
    inherited Create(Parent, SegmentCount)
  else
    inherited Create(nil, SegmentCount);
  FEDIDOT := ediMessage;
  InternalCreateHeaderTrailerSegments;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIMessage.DeleteSegment(Index: Integer);
begin
  DeleteEDIDataObject(Index);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIMessage.DeleteSegment(Segment: TEDISegment);
begin
  DeleteEDIDataObject(Segment);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIMessage.DeleteSegments;
begin
  DeleteEDIDataObjects;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIMessage.DeleteSegments(Index, Count: Integer);
begin
  DeleteEDIDataObjects(Index, Count);
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIMessage.Destroy;
begin
  FUNTSegment.Free;
  FUNHSegment.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIMessage.Disassemble;
var
  I, StartPos, SearchResult: Integer;
  S, S2: string;
  {$IFDEF OPTIMIZED_DISASSEMBLE}
  LLHeader: TEDIDataObjectLinkedListHeader;
  LLItem: TEDIDataObjectLinkedListItem;
  EDISegment: TEDISegment;
  {$ENDIF}
begin
  {$IFDEF OPTIMIZED_DISASSEMBLE}
  LLHeader := TEDIDataObjectLinkedListHeader.Create;
  try
  {$ENDIF}
    FUNHSegment.Data := '';
    FUNHSegment.DeleteElements;
    FUNTSegment.Data := '';
    FUNTSegment.DeleteElements;
    DeleteSegments;
    // Check delimiter assignment
    if not Assigned(FDelimiters) then
    begin
      FDelimiters := InternalAssignDelimiters;
      if not Assigned(FDelimiters) then
        raise EJclEDIError.CreateResRec(@RsEDIError030);
    end;
    // Find the first segment
    StartPos := 1;
    SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
    while SearchResult <> 0 do
    begin
      S := Copy(FData, ((StartPos + FDelimiters.SDLen) - 1), Length(UNHSegmentId));
      S2 := Copy(FData, ((StartPos + FDelimiters.SDLen) - 1), Length(UNTSegmentId));
      if (S <> UNHSegmentId) and (S2 <> UNTSegmentId) then
      begin
        {$IFDEF OPTIMIZED_DISASSEMBLE}
        EDISegment := InternalCreateSegment;
        {$ELSE}
        I := AddSegment;
        {$ENDIF}
        if (SearchResult - StartPos) > 0 then // data exists
        begin
          {$IFDEF OPTIMIZED_DISASSEMBLE}
          EDISegment.Data := Copy(FData, ((StartPos + FDelimiters.SDLen) - 1),
            ((SearchResult - StartPos) + FDelimiters.SDLen));
          EDISegment.Disassemble;
          LLHeader.AppendEDIDataObject(EDISegment);
          {$ELSE}
          FEDIDataObjects[I].Data := Copy(FData, ((StartPos + FDelimiters.SDLen) - 1),
            ((SearchResult - StartPos) + FDelimiters.SDLen));
          FEDIDataObjects[I].Disassemble;
          {$ENDIF}
        end;
      end
      else
      if S = UNHSegmentId then
      begin
        if (SearchResult - StartPos) > 0 then // data exists
        begin
          FUNHSegment.Data := Copy(FData, ((StartPos + FDelimiters.SDLen) - 1),
            ((SearchResult - StartPos) + FDelimiters.SDLen));
          FUNHSegment.Disassemble;
        end;
      end
      else
      if S2 = UNTSegmentId then
      begin
        if (SearchResult - StartPos) > 0 then // data exists
        begin
          FUNTSegment.Data := Copy(FData, ((StartPos + FDelimiters.SDLen) - 1),
            ((SearchResult - StartPos) + FDelimiters.SDLen));
          FUNTSegment.Disassemble;
        end;
      end;
      StartPos := SearchResult + FDelimiters.SDLen;
      SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
    end;
    FData := '';
  {$IFDEF OPTIMIZED_DISASSEMBLE}
    // Add the objects in a chunk
    SetLength(FEDIDataObjects, Length(FEDIDataObjects) + LLHeader.ItemCount);
    //AddSegments(LLHeader.ItemCount);
    I := 0;
    LLItem := LLHeader.FirstItem;
    while LLItem <> nil do
    begin
      FEDIDataObjects[I] := LLItem.EDIDataObject;
      LLItem.EDIDataObject := nil;
      LLItem := LLItem.NextItem;
      Inc(I);
    end;
  finally
    LLHeader.Free;
  end;
  {$ENDIF}
  FState := ediDissassembled;
end;

//--------------------------------------------------------------------------------------------------

function TEDIMessage.GetSegment(Index: Integer): TEDISegment;
begin
  Result := TEDISegment(FEDIDataObjects[Index]);
end;

//--------------------------------------------------------------------------------------------------

function TEDIMessage.InsertSegment(InsertIndex: Integer): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex);
end;

//--------------------------------------------------------------------------------------------------

function TEDIMessage.GetSegments: TEDISegmentArray;
begin
  Result := TEDISegmentArray(FEDIDataObjects);
end;

//--------------------------------------------------------------------------------------------------

function TEDIMessage.InsertSegment(InsertIndex: Integer; Segment: TEDISegment): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex, Segment);
end;

//--------------------------------------------------------------------------------------------------

function TEDIMessage.InsertSegments(InsertIndex, Count: Integer): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, Count);
end;

//--------------------------------------------------------------------------------------------------

function TEDIMessage.InsertSegments(InsertIndex: Integer;
  SegmentArray: TEDISegmentArray): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, TEDIDataObjectArray(SegmentArray));
end;

//--------------------------------------------------------------------------------------------------

function TEDIMessage.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  if FDelimiters = nil then // Attempt to assign the delimiters
  begin
    if Assigned(Parent) and
      ((Parent is TEDIFunctionalGroup) or (Parent is TEDIInterchangeControl)) then
    begin
      if Assigned(Parent.Delimiters) then
      begin
        Result := Parent.Delimiters;
        Exit;
      end;
      if Assigned(Parent.Parent) and (Parent.Parent is TEDIInterchangeControl) then
        if Assigned(Parent.Parent.Delimiters) then
          Result := Parent.Parent.Delimiters;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIMessage.InternalCreateSegment: TEDISegment;
begin
  Result := TEDISegment.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIMessage.InternalCreateHeaderTrailerSegments;
begin
  FUNHSegment := TEDIMessageSegment.Create(Self);
  FUNTSegment := TEDIMessageSegment.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIMessage.SetSegment(Index: Integer; Segment: TEDISegment);
begin
  TEDISegment(FEDIDataObjects[Index]) := Segment;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIMessage.SetSegments(const Value: TEDISegmentArray);
begin
  TEDISegmentArray(FEDIDataObjects) := Value;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIMessage.SetUNTSegment(const UNTSegment: TEDIMessageSegment);
begin
  if Assigned(FUNTSegment) then
  begin
    FUNTSegment.Free;
    FUNTSegment := nil;
  end;
  FUNTSegment := UNTSegment;
  if Assigned(FUNTSegment) then
    FUNTSegment.Parent := Self;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIMessage.SetUNHSegment(const UNHSegment: TEDIMessageSegment);
begin
  if Assigned(FUNHSegment) then
  begin
    FUNHSegment.Free;
    FUNHSegment := nil;
  end;
  FUNHSegment := UNHSegment;
  if Assigned(FUNHSegment) then
    FUNHSegment.Parent := Self;
end;

//--------------------------------------------------------------------------------------------------

function TEDIMessage.InternalCreateEDIDataObject: TEDIDataObject;
begin
  Result := InternalCreateSegment;
end;

//==================================================================================================
// TEDIFunctionalGroup
//==================================================================================================

constructor TEDIFunctionalGroup.Create(Parent: TEDIDataObject; MessageCount: Integer);
begin
  if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
    inherited Create(Parent, MessageCount)
  else
    inherited Create(nil, MessageCount);
  FEDIDOT := ediFunctionalGroup;
  InternalCreateHeaderTrailerSegments;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIFunctionalGroup.Destroy;
begin
  FUNGSegment.Free;
  FUNESegment.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.AddMessage: Integer;
begin
  Result := AddEDIDataObject;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.AddMessages(Count: Integer): Integer;
begin
  Result := AddEDIDataObjects(Count);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.AppendMessage(Message: TEDIMessage): Integer;
begin
  Result := AppendEDIDataObject(Message);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.AppendMessages(
  MessageArray: TEDIMessageArray): Integer;
begin
  Result := AppendEDIDataObjects(TEDIDataObjectArray(MessageArray));
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.Assemble: string;
var
  I: Integer;
begin
  FData := '';
  FLength := 0;
  Result := '';
  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateResRec(@RsEDIError020);
  end;
  FData := FUNGSegment.Assemble;
  FUNGSegment.Data := '';

  if Length(FEDIDataObjects) > 0 then
    for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
      if Assigned(FEDIDataObjects[I]) then
        FData := FData + FEDIDataObjects[I].Assemble;

  DeleteMessages;

  FData := FData + FUNESegment.Assemble;
  FUNESegment.Data := '';

  FLength := Length(FData);
  Result := FData;

  FState := ediAssembled;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroup.DeleteMessage(Index: Integer);
begin
  DeleteEDIDataObject(Index);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroup.DeleteMessage(Message: TEDIMessage);
begin
  DeleteEDIDataObject(Message);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroup.DeleteMessages;
begin
  DeleteEDIDataObjects;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroup.DeleteMessages(Index, Count: Integer);
begin
  DeleteEDIDataObjects(Index, Count);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroup.Disassemble;
var
  I, StartPos, SearchResult: Integer;
  {$IFDEF OPTIMIZED_DISASSEMBLE}
  LLHeader: TEDIDataObjectLinkedListHeader;
  LLItem: TEDIDataObjectLinkedListItem;
  EDIMessage: TEDIMessage;
  {$ENDIF}
begin
  {$IFDEF OPTIMIZED_DISASSEMBLE}
  LLHeader := TEDIDataObjectLinkedListHeader.Create;
  try
  {$ENDIF}
    FUNGSegment.Data := '';
    FUNGSegment.DeleteElements;
    FUNESegment.Data := '';
    FUNESegment.DeleteElements;
    DeleteMessages;
    // Check delimiter assignment
    if not Assigned(FDelimiters) then
    begin
      FDelimiters := InternalAssignDelimiters;
      if not Assigned(FDelimiters) then
        raise EJclEDIError.CreateResRec(@RsEDIError019);
    end;
    // Find Functional Group Header Segment
    StartPos := 1;
    // Search for Functional Group Header
    if UNGSegmentId + FDelimiters.ED = Copy(FData, 1, Length(UNGSegmentId + FDelimiters.ED)) then
    begin
      // Search for Functional Group Header Segment Terminator
      SearchResult := StrSearch(FDelimiters.SD, FData, 1);
      if (SearchResult - StartPos) > 0 then // data exists
      begin
        FUNGSegment.Data := Copy(FData, 1, (SearchResult + FDelimiters.SDLen) - 1);
        FUNGSegment.Disassemble;
      end
      else
        raise EJclEDIError.CreateResRec(@RsEDIError021);
    end
    else
      raise EJclEDIError.CreateResRec(@RsEDIError022);
    // Search for Message Header
    SearchResult := StrSearch(FDelimiters.SD + UNHSegmentId + FDelimiters.ED, FData, StartPos);
    if SearchResult <= 0 then
      raise EJclEDIError.CreateResRec(@RsEDIError032);
    // Set next start position
    StartPos := SearchResult + FDelimiters.SDLen; // Move past the delimiter
    // Continue
    while SearchResult <> 0 do
    begin
      // Search for Message Trailer
      SearchResult := StrSearch(FDelimiters.SD + UNTSegmentId + FDelimiters.ED, FData, StartPos);
      if SearchResult <> 0 then
      begin
        // Set the next start position
        SearchResult := SearchResult + FDelimiters.SDLen; // Move past the delimiter
        // Search for the end of Message Trailer
        SearchResult := StrSearch(FDelimiters.SD, FData, SearchResult);
        if SearchResult <> 0 then
        begin
          {$IFDEF OPTIMIZED_DISASSEMBLE}
          EDIMessage := InternalCreateMessage;
          EDIMessage.Data :=
            Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.SDLen));
          EDIMessage.Disassemble;
          LLHeader.AppendEDIDataObject(EDIMessage);
          {$ELSE}
          I := AddMessage;
          FEDIDataObjects[I].Data :=
            Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.SDLen));
          FEDIDataObjects[I].Disassemble;
          {$ENDIF}
        end
        else
          raise EJclEDIError.CreateResRec(@RsEDIError033);
      end
      else
        raise EJclEDIError.CreateResRec(@RsEDIError034);
      // Set the next start position
      StartPos := SearchResult + FDelimiters.SDLen; // Move past the delimiter
      //
      // Verify the next record is a Message Header
      if (UNHSegmentId + FDelimiters.ED) <>
        Copy(FData, StartPos, (Length(UNHSegmentId) + FDelimiters.EDLen)) then
        Break;
    end;
    // Set the next start position
    StartPos := SearchResult + FDelimiters.SDLen; // Move past the delimiter
    // Find Functional Group Trailer Segment
    if (UNESegmentId + FDelimiters.ED) =
      Copy(FData, StartPos, Length(UNESegmentId + FDelimiters.ED)) then
    begin
      // Find Functional Group Trailer Segment Terminator
      SearchResult := StrSearch(FDelimiters.SD, FData, StartPos + FDelimiters.SDLen);
      if (SearchResult - StartPos) > 0 then // data exists
      begin
        FUNESegment.Data := Copy(FData, StartPos, (SearchResult + FDelimiters.SDLen));
        FUNESegment.Disassemble;
      end
      else
        raise EJclEDIError.CreateResRec(@RsEDIError023);
    end
    else
      raise EJclEDIError.CreateResRec(@RsEDIError024);
    FData := '';
  {$IFDEF OPTIMIZED_DISASSEMBLE}
    // Add the objects in a chunk
    SetLength(FEDIDataObjects, Length(FEDIDataObjects) + LLHeader.ItemCount);
    // AddMessages(LLHeader.ItemCount);
    I := 0;
    LLItem := LLHeader.FirstItem;
    while LLItem <> nil do
    begin
      FEDIDataObjects[I] := LLItem.EDIDataObject;
      LLItem.EDIDataObject := nil;
      LLItem := LLItem.NextItem;
      Inc(I);
    end;
  finally
    LLHeader.Free;
  end;
  {$ENDIF}
  FState := ediDissassembled;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.GetMessage(Index: Integer): TEDIMessage;
begin
  Result := TEDIMessage(FEDIDataObjects[Index]);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.InsertMessage(InsertIndex: Integer): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.InsertMessage(InsertIndex: Integer;
  Message: TEDIMessage): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex, Message);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.InsertMessages(InsertIndex: Integer;
  MessageArray: TEDIMessageArray): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, TEDIDataObjectArray(MessageArray));
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.InsertMessages(InsertIndex, Count: Integer): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, Count);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  // Attempt to assign the delimiters
  if not Assigned(FDelimiters) then
    if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
      if Assigned(Parent.Delimiters) then
        Result := Parent.Delimiters;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.InternalCreateMessage: TEDIMessage;
begin
  Result := TEDIMessage.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroup.InternalCreateHeaderTrailerSegments;
begin
  FUNGSegment := TEDIFunctionalGroupSegment.Create(Self);
  FUNESegment := TEDIFunctionalGroupSegment.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroup.SetMessage(Index: Integer; Message: TEDIMessage);
begin
  TEDIMessage(FEDIDataObjects[Index]) := Message;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroup.SetUNESegment(const UNESegment: TEDIFunctionalGroupSegment);
begin
  if Assigned(FUNESegment) then
  begin
    FUNESegment.Free;
    FUNESegment := nil;
  end;
  FUNESegment := UNESegment;
  if Assigned(FUNESegment) then
    FUNESegment.Parent := Self;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroup.SetUNGSegment(const UNGSegment: TEDIFunctionalGroupSegment);
begin
  if Assigned(FUNGSegment) then
  begin
    FUNGSegment.Free;
    FUNGSegment := nil;
  end;
  FUNGSegment := UNGSegment;
  if Assigned(FUNGSegment) then
    FUNGSegment.Parent := Self;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.GetMessages: TEDIMessageArray;
begin
  Result := TEDIMessageArray(FEDIDataObjects);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.InternalCreateEDIDataObject: TEDIDataObject;
begin
  Result := InternalCreateMessage;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroup.SetMessages(const Value: TEDIMessageArray);
begin
  TEDIMessageArray(FEDIDataObjects) := Value;
end;

//==================================================================================================
// TEDIInterchangeControl
//==================================================================================================

constructor TEDIInterchangeControl.Create(Parent: TEDIDataObject; FunctionalGroupCount: Integer);
begin
  if Assigned(Parent) and (Parent is TEDIFile) then
    inherited Create(Parent, FunctionalGroupCount)
  else
    inherited Create(nil, FunctionalGroupCount);
  FEDIDOT := ediInterchangeControl;
  InternalCreateHeaderTrailerSegments;
  FCreateObjectType := ediFunctionalGroup;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIInterchangeControl.Destroy;
begin
  FUNASegment.Free;
  FUNBSegment.Free;
  FUNZSegment.Free;
  FreeAndNil(FDelimiters);
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.AddFunctionalGroup: Integer;
begin
  FCreateObjectType := ediFunctionalGroup;
  Result := AddEDIDataObject;
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.AddFunctionalGroups(Count: Integer): Integer;
begin
  FCreateObjectType := ediFunctionalGroup;
  Result := AddEDIDataObjects(Count);
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.AppendFunctionalGroup(
  FunctionalGroup: TEDIFunctionalGroup): Integer;
begin
  FCreateObjectType := ediFunctionalGroup;
  Result := AppendEDIDataObject(FunctionalGroup);
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.AppendFunctionalGroups(
  FunctionalGroupArray: TEDIFunctionalGroupArray): Integer;
begin
  Result := AppendEDIDataObjects(TEDIDataObjectArray(FunctionalGroupArray));
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
    raise EJclEDIError.CreateResRec(@RsEDIError013);

  FData := FUNBSegment.Assemble;
  FUNBSegment.Data := '';

  if Length(FEDIDataObjects) > 0 then
    for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
      if Assigned(FEDIDataObjects[I]) then
        FData := FData + FEDIDataObjects[I].Assemble;

  DeleteEDIDataObjects;

  FData := FData + FUNZSegment.Assemble;
  FUNZSegment.Data := '';

  FLength := Length(FData);
  Result := FData;

  FState := ediAssembled;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIInterchangeControl.Disassemble;
var
  I, StartPos, SearchResult: Integer;
  {$IFDEF OPTIMIZED_DISASSEMBLE}
  LLHeader: TEDIDataObjectLinkedListHeader;
  LLItem: TEDIDataObjectLinkedListItem;
  EDIFunctionalGroup: TEDIFunctionalGroup;
  EDIMessage: TEDIMessage;
  {$ENDIF}
begin
  {$IFDEF OPTIMIZED_DISASSEMBLE}
  LLHeader := TEDIDataObjectLinkedListHeader.Create;
  try
  {$ENDIF}
    FUNBSegment.Data := '';
    FUNBSegment.DeleteElements;
    FUNZSegment.Data := '';
    FUNZSegment.DeleteElements;
    DeleteEDIDataObjects;

    if not Assigned(FDelimiters) then
    begin
      raise EJclEDIError.CreateResRec(@RsEDIError012);
    end;

    StartPos := 1;
    // Search for Interchange Control Header
    if UNBSegmentId + FDelimiters.ED = Copy(FData, 1, Length(UNBSegmentId + FDelimiters.ED)) then
    begin
      SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
      if (SearchResult - StartPos) > 0 then // data exists
      begin
        FUNBSegment.Data := Copy(FData, 1, (SearchResult + FDelimiters.SDLen) - 1);
        FUNBSegment.Disassemble;
      end
      else
        raise EJclEDIError.CreateResRec(@RsEDIError014);
    end
    else
    begin
      raise EJclEDIError.CreateResRec(@RsEDIError015);
    end;
    // Search for Functional Group Header
    SearchResult := StrSearch(FDelimiters.SD + UNGSegmentId + FDelimiters.ED, FData, StartPos);
    if SearchResult > 0 then
    begin
      // Set next start positon
      StartPos := SearchResult + FDelimiters.SDLen; // Move past the delimiter
      // Continue
      while ((StartPos + Length(UNGSegmentId)) < Length(FData)) and (SearchResult > 0) do
      begin
        // Search for Functional Group Trailer
        SearchResult := StrSearch(FDelimiters.SD + UNESegmentId + FDelimiters.ED, FData, StartPos);
        if SearchResult > 0 then
        begin
          // Set next start positon
          SearchResult := SearchResult + FDelimiters.SDLen; //Move past the delimiter
          // Search for end of Functional Group Trailer Segment Terminator
          SearchResult := StrSearch(FDelimiters.SD, FData, SearchResult);
          if SearchResult > 0 then
          begin
            {$IFDEF OPTIMIZED_DISASSEMBLE}
            EDIFunctionalGroup := InternalCreateFunctionalGroup;
            EDIFunctionalGroup.Data :=
              Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.SDLen));
            EDIFunctionalGroup.Disassemble;
            LLHeader.AppendEDIDataObject(EDIFunctionalGroup);
            {$ELSE}
            I := AddFunctionalGroup;
            FEDIDataObjects[I].Data :=
              Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.SDLen));
            FEDIDataObjects[I].Disassemble;
            {$ENDIF}
          end
          else
            raise EJclEDIError.CreateResRec(@RsEDIError023);
        end
        else
          raise EJclEDIError.CreateResRec(@RsEDIError024);
        // Set next start positon
        StartPos := SearchResult + FDelimiters.SDLen; // Move past the delimiter
        // Verify the next record is a Functional Group Header
        if (UNGSegmentId + FDelimiters.ED) <>
          Copy(FData, StartPos, (Length(UNGSegmentId) + FDelimiters.EDLen)) then
          Break;
      end;
      {$IFDEF OPTIMIZED_DISASSEMBLE}
      // Add the objects in a chunk
      SetLength(FEDIDataObjects, Length(FEDIDataObjects) + LLHeader.ItemCount);
      //AddFunctionalGroups(LLHeader.ItemCount);
      {$ENDIF}
    end
    else
    begin
      // Search for Message Header
      SearchResult := StrSearch(FDelimiters.SD + UNHSegmentId + FDelimiters.ED, FData, StartPos);
      if SearchResult <= 0 then
        raise EJclEDIError.CreateResRec(@RsEDIError032);
      // Set next start position
      StartPos := SearchResult + FDelimiters.SDLen; // Move past the delimiter
      // Continue
      while SearchResult <> 0 do
      begin
        // Search for Message Trailer
        SearchResult := StrSearch(FDelimiters.SD + UNTSegmentId + FDelimiters.ED, FData, StartPos);
        if SearchResult <> 0 then
        begin
          // Set the next start position
          SearchResult := SearchResult + FDelimiters.SDLen; //Move past the delimiter
          // Search for the end of Message Trailer
          SearchResult := StrSearch(FDelimiters.SD, FData, SearchResult);
          if SearchResult <> 0 then
          begin
            {$IFDEF OPTIMIZED_DISASSEMBLE}
            EDIMessage := InternalCreateMessage;
            EDIMessage.Data :=
              Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.SDLen));
            EDIMessage.Disassemble;
            LLHeader.AppendEDIDataObject(EDIMessage);
            {$ELSE}
            I := AddMessage;
            FEDIDataObjects[I].Data :=
              Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.SDLen));
            FEDIDataObjects[I].Disassemble;
            {$ENDIF}
          end
          else
            raise EJclEDIError.CreateResRec(@RsEDIError033);
        end
        else
          raise EJclEDIError.CreateResRec(@RsEDIError034);
        // Set the next start position
        StartPos := SearchResult + FDelimiters.SDLen; // Move past the delimiter
        // Verify the next record is a Message Header
        if (UNHSegmentId + FDelimiters.ED) <>
          Copy(FData, StartPos, (Length(UNHSegmentId) + FDelimiters.EDLen)) then
          Break;
      end;
      {$IFDEF OPTIMIZED_DISASSEMBLE}
      // Add the objects in a chunk
      SetLength(FEDIDataObjects, Length(FEDIDataObjects) + LLHeader.ItemCount);
      //AddMessages(LLHeader.ItemCount);
      {$ENDIF}
    end;
    // Verify the next record is a Interchange Control Trailer
    if (UNZSegmentId + FDelimiters.ED) =
      Copy(FData, StartPos, Length(UNZSegmentId + FDelimiters.ED)) then
    begin
      // Search for the end of Interchange Control Trailer Segment Terminator
      SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
      if (SearchResult - StartPos) > 0 then // data exists
      begin
        FUNZSegment.Data := Copy(FData, StartPos, (SearchResult + FDelimiters.SDLen));
        FUNZSegment.Disassemble;
      end
      else
        raise EJclEDIError.CreateResRec(@RsEDIError016);
    end
    else
      raise EJclEDIError.CreateResRec(@RsEDIError017);
    FData := '';
  {$IFDEF OPTIMIZED_DISASSEMBLE}
    I := 0;
    LLItem := LLHeader.FirstItem;
    while LLItem <> nil do
    begin
      FEDIDataObjects[I] := LLItem.EDIDataObject;
      LLItem.EDIDataObject := nil;
      LLItem := LLItem.NextItem;
      Inc(I);
    end;
  finally
    LLHeader.Free;
  end;
  {$ENDIF}
  FState := ediDissassembled;
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.InsertFunctionalGroup(InsertIndex: Integer;
  FunctionalGroup: TEDIFunctionalGroup): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex, FunctionalGroup);
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.InsertFunctionalGroup(InsertIndex: Integer): Integer;
begin
  FCreateObjectType := ediFunctionalGroup;
  Result := InsertEDIDataObject(InsertIndex);
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.InsertFunctionalGroups(InsertIndex, Count: Integer): Integer;
begin
  FCreateObjectType := ediFunctionalGroup;
  Result := InsertEDIDataObjects(InsertIndex, Count);
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.InsertFunctionalGroups(InsertIndex: Integer;
  FunctionalGroupArray: TEDIFunctionalGroupArray): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, TEDIDataObjectArray(FunctionalGroupArray));
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.InternalCreateFunctionalGroup: TEDIFunctionalGroup;
begin
  Result := TEDIFunctionalGroup.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIInterchangeControl.InternalCreateHeaderTrailerSegments;
begin
  FUNASegment := TEDIInterchangeControlSegment.Create(Self);
  FUNBSegment := TEDIInterchangeControlSegment.Create(Self);
  FUNZSegment := TEDIInterchangeControlSegment.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIInterchangeControl.SetUNZSegment(const UNZSegment: TEDIInterchangeControlSegment);
begin
  if Assigned(FUNZSegment) then
  begin
    FUNZSegment.Free;
    FUNZSegment := nil;
  end;
  FUNZSegment := UNZSegment;
  if Assigned(FUNZSegment) then
    FUNZSegment.Parent := Self;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIInterchangeControl.SetUNBSegment(const UNBSegment: TEDIInterchangeControlSegment);
begin
  if Assigned(FUNBSegment) then
  begin
    FUNBSegment.Free;
    FUNBSegment := nil;
  end;
  FUNBSegment := UNBSegment;
  if Assigned(FUNBSegment) then
    FUNBSegment.Parent := Self;
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.InternalCreateEDIDataObject: TEDIDataObject;
begin
  case FCreateObjectType of
    ediFunctionalGroup:
      Result := InternalCreateFunctionalGroup;
    ediMessage:
      Result := InternalCreateMessage;
  else
    Result := nil;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.InternalCreateMessage: TEDIMessage;
begin
  Result := TEDIMessage.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.AddMessage: Integer;
begin
  FCreateObjectType := ediMessage;
  Result := AddEDIDataObject;
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.AddMessages(Count: Integer): Integer;
begin
  FCreateObjectType := ediMessage;
  Result := AddEDIDataObjects(Count);
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.AppendMessage(Message: TEDIMessage): Integer;
begin
  FCreateObjectType := ediMessage;
  Result := AppendEDIDataObject(Message);
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.AppendMessages(MessageArray: TEDIMessageArray): Integer;
begin
  Result := AppendEDIDataObjects(TEDIDataObjectArray(MessageArray));
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.InsertMessage(InsertIndex: Integer; Message: TEDIMessage): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex, Message);
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.InsertMessage(InsertIndex: Integer): Integer;
begin
  FCreateObjectType := ediMessage;
  Result := InsertEDIDataObject(InsertIndex);
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.InsertMessages(InsertIndex, Count: Integer): Integer;
begin
  FCreateObjectType := ediMessage;
  Result := InsertEDIDataObjects(InsertIndex, Count);
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.InsertMessages(InsertIndex: Integer;
  MessageArray: TEDIMessageArray): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, TEDIDataObjectArray(MessageArray));
end;

//==================================================================================================
// TEDIFile
//==================================================================================================

constructor TEDIFile.Create(Parent: TEDIDataObject; InterchangeCount: Integer);
begin
  if Assigned(Parent) then
    inherited Create(Parent, InterchangeCount)
  else
    inherited Create(nil, InterchangeCount);
  FEDIFileOptions := [foVariableDelimiterDetection, foRemoveCrLf, foRemoveCr, foRemoveLf];
  FEDIDOT := ediFile;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIFile.Destroy;
begin
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFile.AddInterchange: Integer;
begin
  Result := AddEDIDataObject;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFile.AddInterchanges(Count: Integer): Integer;
begin
  Result := AddEDIDataObjects(Count);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFile.AppendInterchange(Interchange: TEDIInterchangeControl): Integer;
begin
  Result := AppendEDIDataObject(Interchange);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFile.AppendInterchanges(InterchangeControlArray: TEDIInterchangeControlArray): Integer;
begin
  Result := AppendEDIDataObjects(TEDIDataObjectArray(InterchangeControlArray));
end;

//--------------------------------------------------------------------------------------------------

function TEDIFile.Assemble: string;
var
  I: Integer;
  EDIInterchangeControl: TEDIInterchangeControl;
begin
  FData := '';
  FLength := 0;
  Result := '';

  if Length(FEDIDataObjects) > 0 then
    for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
    begin
      if Assigned(FEDIDataObjects[I]) then
      begin
        EDIInterchangeControl := TEDIInterchangeControl(FEDIDataObjects[I]);
        if Length(EDIInterchangeControl.SegmentUNA.EDIDataObjects) > 0 then
        begin
          FData := FData + EDIInterchangeControl.SegmentUNA.Assemble;
          EDIInterchangeControl.SegmentUNA.Data := '';
        end;
        FData := FData + FEDIDataObjects[I].Assemble;
      end;
      FEDIDataObjects[I].Data := '';
    end;

  FLength := Length(FData);
  Result := FData;

  DeleteInterchanges;

  FState := ediAssembled;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFile.DeleteInterchange(Index: Integer);
begin
  DeleteEDIDataObject(Index);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFile.DeleteInterchanges(Index, Count: Integer);
begin
  DeleteEDIDataObjects(Index, Count);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFile.DeleteInterchanges;
begin
  DeleteEDIDataObjects;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFile.Disassemble;
var
  I, StartPos, SearchResult: Integer;
  UNASegmentData: string;
  {$IFDEF OPTIMIZED_DISASSEMBLE}
  LLHeader: TEDIDataObjectLinkedListHeader;
  LLItem: TEDIDataObjectLinkedListItem;
  EDIInterchangeControl: TEDIInterchangeControl;
  {$ENDIF}
begin
  {$IFDEF OPTIMIZED_DISASSEMBLE}
  LLHeader := TEDIDataObjectLinkedListHeader.Create;
  try
  {$ENDIF}
    DeleteInterchanges;

    if not Assigned(FDelimiters) then
    begin
      FDelimiters := InternalAssignDelimiters;
      FEDIFileOptions := FEDIFileOptions + [foVariableDelimiterDetection];
    end;

    if foRemoveCrLf in FEDIFileOptions then
      {$IFDEF OPTIMIZED_STRINGREPLACE}
      FData := JclEDI.StringReplace(FData, AnsiCrLf, '', [rfReplaceAll]);
      {$ELSE}
      FData := SysUtils.StringReplace(FData, AnsiCrLf, '', [rfReplaceAll]);
      {$ENDIF}
    if foRemoveCr in FEDIFileOptions then
      {$IFDEF OPTIMIZED_STRINGREPLACE}
      FData := JclEDI.StringReplace(FData, AnsiCarriageReturn, '', [rfReplaceAll]);
      {$ELSE}
      FData := SysUtils.StringReplace(FData, AnsiCarriageReturn, '', [rfReplaceAll]);
      {$ENDIF}
    if foRemoveLf in FEDIFileOptions then
      {$IFDEF OPTIMIZED_STRINGREPLACE}
      FData := JclEDI.StringReplace(FData, AnsiLineFeed, '', [rfReplaceAll]);
      {$ELSE}
      FData := SysUtils.StringReplace(FData, AnsiLineFeed, '', [rfReplaceAll]);
      {$ENDIF}

    StartPos := 1;
    if UNASegmentId = Copy(FData, StartPos, Length(UNASegmentId)) then
    begin
      if foVariableDelimiterDetection in FEDIFileOptions then
        InternalDelimitersDetection(StartPos);
      SearchResult := StrSearch(FDelimiters.SD + UNBSegmentId + FDelimiters.ED, FData, StartPos);
      UNASegmentData := Copy(FData, StartPos, (SearchResult - StartPos) + 1);
      StartPos := SearchResult + 1;
    end
    else
    if UNBSegmentId = Copy(FData, StartPos, Length(UNBSegmentId)) then
    begin
      if foVariableDelimiterDetection in FEDIFileOptions then
        InternalAlternateDelimitersDetection(StartPos);
    end
    else
      raise EJclEDIError.CreateResRec(@RsEDIError015);

    // Continue
    while (StartPos + Length(UNBSegmentId)) < Length(FData) do
    begin
      // Search for Interchange Control Trailer
      SearchResult := StrSearch(FDelimiters.SD + UNZSegmentId + FDelimiters.ED, FData, StartPos);
      if SearchResult > 0 then
      begin
        SearchResult := SearchResult + FDelimiters.SDLen; //Move past the delimiter
        // Search for the end of Interchange Control Trailer
        SearchResult := StrSearch(FDelimiters.SD, FData, SearchResult);
        if SearchResult > 0 then
        begin
          {$IFDEF OPTIMIZED_DISASSEMBLE}
          EDIInterchangeControl := InternalCreateInterchangeControl;
          EDIInterchangeControl.Delimiters :=
            TEDIDelimiters.Create(FDelimiters.SD, FDelimiters.ED, FDelimiters.SS);
          TEDIInterchangeControl(EDIInterchangeControl).SegmentUNA.Data := UNASegmentData;
          TEDIInterchangeControl(EDIInterchangeControl).SegmentUNA.Disassemble;
          EDIInterchangeControl.Data :=
            Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.SDLen));
          EDIInterchangeControl.Disassemble;
          LLHeader.AppendEDIDataObject(EDIInterchangeControl);
          {$ELSE}
          I := AddInterchange;
          FEDIDataObjects[I].Delimiters :=
            TEDIDelimiters.Create(FDelimiters.SD, FDelimiters.ED, FDelimiters.SS);
          TEDIInterchangeControl(FEDIDataObjects[I]).SegmentUNA.Data := UNASegmentData;
          TEDIInterchangeControl(FEDIDataObjects[I]).SegmentUNA.Disassemble;
          FEDIDataObjects[I].Data :=
            Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.SDLen));
          FEDIDataObjects[I].Disassemble;
          {$ENDIF}
        end
        else
          raise EJclEDIError.CreateResRec(@RsEDIError016);
      end
      else
        raise EJclEDIError.CreateResRec(@RsEDIError017);
      // Set next start position, Move past the delimiter
      StartPos := SearchResult + FDelimiters.SDLen;
      //
      if UNASegmentId = Copy(FData, StartPos, Length(UNASegmentId)) then
      begin
        if foVariableDelimiterDetection in FEDIFileOptions then
          InternalDelimitersDetection(StartPos);
        SearchResult := StrSearch(FDelimiters.SD + UNBSegmentId + FDelimiters.ED, FData, StartPos);
        UNASegmentData := Copy(FData, StartPos, (SearchResult - StartPos) + 1);
        StartPos := SearchResult + 1;
      end
      else
      if UNBSegmentId = Copy(FData, StartPos, Length(UNBSegmentId)) then
      begin
        if foVariableDelimiterDetection in FEDIFileOptions then
          InternalAlternateDelimitersDetection(StartPos);
      end
      else
      if (StartPos + Length(UNBSegmentId)) < Length(FData) then
        raise EJclEDIError.CreateResRec(@RsEDIError018);
    end;
    FData := '';
  {$IFDEF OPTIMIZED_DISASSEMBLE}
    // Add the objects in a chunk
    SetLength(FEDIDataObjects, Length(FEDIDataObjects) + LLHeader.ItemCount);
    //AddInterchanges(LLHeader.ItemCount);
    I := 0;
    LLItem := LLHeader.FirstItem;
    while LLItem <> nil do
    begin
      FEDIDataObjects[I] := LLItem.EDIDataObject;
      LLItem.EDIDataObject := nil;
      LLItem := LLItem.NextItem;
      Inc(I);
    end;
  finally
    LLHeader.Free;
  end;
  {$ENDIF}
  FState := ediDissassembled;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFile.GetInterchangeControl(Index: Integer): TEDIInterchangeControl;
begin
  Result := TEDIInterchangeControl(FEDIDataObjects[Index]);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFile.InsertInterchange(InsertIndex: Integer;
  Interchange: TEDIInterchangeControl): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex, Interchange);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFile.InsertInterchange(InsertIndex: Integer): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFile.InsertInterchanges(InsertIndex, Count: Integer): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, Count);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFile.InsertInterchanges(InsertIndex: Integer;
  InterchangeControlArray: TEDIInterchangeControlArray): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, TEDIDataObjectArray(InterchangeControlArray));
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFile.InternalLoadFromFile;
var
  EDIFileStream: TFileStream;
begin
  FData := '';
  if FFileName <> '' then
  begin
    EDIFileStream := TFileStream.Create(FFileName, fmOpenRead	or fmShareDenyNone);
    try
      SetLength(FData, EDIFileStream.Size);
      EDIFileStream.Read(Pointer(FData)^, EDIFileStream.Size);
    finally
      EDIFileStream.Free;
    end;
  end
  else
    raise EJclEDIError.CreateResRec(@RsEDIError001);
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
    raise EJclEDIError.CreateResRec(@RsEDIError002);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFile.SaveToFile;
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
    raise EJclEDIError.CreateResRec(@RsEDIError002);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFile.SetInterchangeControl(Index: Integer; Interchange: TEDIInterchangeControl);
begin
  TEDIInterchangeControl(FEDIDataObjects[Index]) := Interchange;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFile.InternalDelimitersDetection(StartPos: Integer);
begin
  FDelimiters.SS := Copy(FData, StartPos + Length(UNASegmentId), 1);
  FDelimiters.ED := Copy(FData, StartPos + Length(UNASegmentId) + 1, 1);
  FDelimiters.SD := Copy(FData, StartPos + Length(UNASegmentId) + 5, 1);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFile.InternalAlternateDelimitersDetection(StartPos: Integer);
var
  SearchResult, I: Integer;
  Delimiter: string;
begin
  SearchResult := 1;
  FDelimiters.ED := Copy(FData, StartPos + Length(UNBSegmentId), 1);
  SearchResult := StrSearch(UNGSegmentId + FDelimiters.ED, FData, SearchResult);
  if SearchResult <= 0 then
    SearchResult := StrSearch(UNHSegmentId + FDelimiters.ED, FData, SearchResult);
  FDelimiters.SD := Copy(FData, SearchResult - 1, 1);
  SearchResult := SearchResult - 2;
  for I := SearchResult downto 1 do
  begin
    Delimiter := Copy(FData, I, 1);
    if not (Delimiter[1] in ['0'..'9','A'..'Z','a'..'z',
      FDelimiters.ED[1], FDelimiters.SD[1]]) then
    begin
      FDelimiters.SS := Copy(FData, I, 1);
      Break;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFile.InternalCreateInterchangeControl: TEDIInterchangeControl;
begin
  Result := TEDIInterchangeControl.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFile.DeleteInterchange(Interchange: TEDIInterchangeControl);
begin
  DeleteEDIDataObject(Interchange);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFile.GetInterchanges: TEDIInterchangeControlArray;
begin
  Result := TEDIInterchangeControlArray(FEDIDataObjects);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFile.SetInterchanges(const Value: TEDIInterchangeControlArray);
begin
  TEDIInterchangeControlArray(FEDIDataObjects) := Value;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFile.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := TEDIDelimiters.Create('''','+',':');
end;

//--------------------------------------------------------------------------------------------------

function TEDIFile.InternalCreateEDIDataObject: TEDIDataObject;
begin
  Result := InternalCreateInterchangeControl;
end;

//==================================================================================================
// TEDICompositeElement
//==================================================================================================

constructor TEDICompositeElement.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  if Assigned(Parent) and (Parent is TEDISegment) then
    inherited Create(Parent, ElementCount)
  else
    inherited Create(nil, ElementCount);
  FEDIDOT := ediElement;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDICompositeElement.Destroy;
begin
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDICompositeElement.AddElement: Integer;
begin
  Result := AddEDIDataObject;
end;

//--------------------------------------------------------------------------------------------------

function TEDICompositeElement.AddElements(Count: Integer): Integer;
begin
  Result := AddEDIDataObjects(Count);
end;

//--------------------------------------------------------------------------------------------------

function TEDICompositeElement.AppendElement(Element: TEDIElement): Integer;
begin
  Result := AppendEDIDataObject(Element);
end;

//--------------------------------------------------------------------------------------------------

function TEDICompositeElement.AppendElements(ElementArray: TEDIElementArray): Integer;
begin
  Result := AppendEDIDataObjects(TEDIDataObjectArray(ElementArray));
end;

//--------------------------------------------------------------------------------------------------

function TEDICompositeElement.Assemble: string;
var
  I: Integer;
begin
  FData := '';
  FLength := 0;
  Result := '';

  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateResRec(@RsEDIError038);
  end;

  if Length(FEDIDataObjects) > 0 then
    for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
      if Assigned(FEDIDataObjects[I]) then
      begin
        if FData <> '' then
          FData := FData + FDelimiters.SS + FEDIDataObjects[I].Assemble
        else
          FData := FData + FEDIDataObjects[I].Assemble;
      end
      else
      begin
        if I <> High(FEDIDataObjects) then
          FData := FData + FDelimiters.SS;
      end;
  FLength := Length(FData);
  Result := FData; // Return assembled string

  DeleteElements;

  FState := ediAssembled;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDICompositeElement.DeleteElement(Element: TEDIElement);
begin
  DeleteEDIDataObject(Element);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDICompositeElement.DeleteElement(Index: Integer);
begin
  DeleteEDIDataObject(Index);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDICompositeElement.DeleteElements;
begin
  DeleteEDIDataObjects;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDICompositeElement.DeleteElements(Index, Count: Integer);
begin
  DeleteEDIDataObjects(Index, Count);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDICompositeElement.Disassemble;
var
  StartPos, SearchResult, I: Integer;
  {$IFDEF OPTIMIZED_DISASSEMBLE}
  LLHeader: TEDIDataObjectLinkedListHeader;
  LLItem: TEDIDataObjectLinkedListItem;
  EDIElement: TEDIElement;
  {$ENDIF}
begin
  {$IFDEF OPTIMIZED_DISASSEMBLE}
  LLHeader := TEDIDataObjectLinkedListHeader.Create;
  try
  {$ENDIF}
    DeleteElements;
    if not Assigned(FDelimiters) then // Attempt to assign the delimiters
    begin
      FDelimiters := InternalAssignDelimiters;
      if not Assigned(FDelimiters) then
        raise EJclEDIError.CreateResRec(@RsEDIError037);
    end;
    StartPos := 1;
    SearchResult := StrSearch(FDelimiters.SS, FData, StartPos);
    while SearchResult > 0 do
    begin
      {$IFDEF OPTIMIZED_DISASSEMBLE}
      EDIElement := InternalCreateElement;
      LLHeader.AppendEDIDataObject(EDIElement);
      {$ELSE}
      I := AddElement;
      {$ENDIF}
      if (SearchResult - StartPos) > 0 then // data exists
      begin
        {$IFDEF OPTIMIZED_DISASSEMBLE}
        EDIElement.Data := Copy(FData, StartPos, (SearchResult - StartPos));
        EDIElement.Disassemble;
        {$ELSE}
        FEDIDataObjects[I].Data := Copy(FData, StartPos, (SearchResult - StartPos));
        FEDIDataObjects[I].Disassemble;
        {$ENDIF}
      end;
      StartPos := SearchResult + 1;
      SearchResult := StrSearch(FDelimiters.SS, FData, StartPos);
    end;
    if StartPos <= Length(FData) then
    begin
      {$IFDEF OPTIMIZED_DISASSEMBLE}
      EDIElement := InternalCreateElement;
      LLHeader.AppendEDIDataObject(EDIElement);
      EDIElement.Data := Copy(FData, StartPos, (Length(FData) - StartPos) + 1);
      EDIElement.Disassemble;
      {$ELSE}
      I := AddElement;
      FEDIDataObjects[I].Data := Copy(FData, StartPos, (Length(FData) - StartPos) + 1);
      FEDIDataObjects[I].Disassemble;
      {$ENDIF}
    end;
  {$IFDEF OPTIMIZED_DISASSEMBLE}
    // Add the objects in a chunk
    SetLength(FEDIDataObjects, Length(FEDIDataObjects) + LLHeader.ItemCount);
    //AddElements(LLHeader.ItemCount);
    I := 0;
    LLItem := LLHeader.FirstItem;
    while LLItem <> nil do
    begin
      FEDIDataObjects[I] := LLItem.EDIDataObject;
      LLItem.EDIDataObject := nil;
      LLItem := LLItem.NextItem;
      Inc(I);
    end;
  finally
    LLHeader.Free;
  end;
  {$ENDIF}
end;

//--------------------------------------------------------------------------------------------------

function TEDICompositeElement.GetElement(Index: Integer): TEDIElement;
begin
  Result := TEDIElement(FEDIDataObjects[Index]);
end;

//--------------------------------------------------------------------------------------------------

function TEDICompositeElement.GetElements: TEDIElementArray;
begin
  Result := TEDIElementArray(FEDIDataObjects);
end;

//--------------------------------------------------------------------------------------------------

function TEDICompositeElement.InsertElement(InsertIndex: Integer): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex);
end;

//--------------------------------------------------------------------------------------------------

function TEDICompositeElement.InsertElement(InsertIndex: Integer; Element: TEDIElement): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex, Element);
end;

//--------------------------------------------------------------------------------------------------

function TEDICompositeElement.InsertElements(InsertIndex: Integer;
  ElementArray: TEDIElementArray): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, TEDIDataObjectArray(ElementArray));
end;

//--------------------------------------------------------------------------------------------------

function TEDICompositeElement.InsertElements(InsertIndex, Count: Integer): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, Count);
end;

//--------------------------------------------------------------------------------------------------

function TEDICompositeElement.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
    // Get the delimiters from the segemnt
    if Assigned(Parent) and (Parent is TEDISegment) then
      if Assigned(Parent.Delimiters) then
        Result := Parent.Delimiters;
end;

//--------------------------------------------------------------------------------------------------

function TEDICompositeElement.InternalCreateEDIDataObject: TEDIDataObject;
begin
  Result := InternalCreateElement;
end;

//--------------------------------------------------------------------------------------------------

function TEDICompositeElement.InternalCreateElement: TEDIElement;
begin
  Result := TEDIElement.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDICompositeElement.SetElement(Index: Integer; Element: TEDIElement);
begin
  TEDIElement(FEDIDataObjects[Index]) := Element;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDICompositeElement.SetElements(const Value: TEDIElementArray);
begin
  TEDIElementArray(FEDIDataObjects) := Value;
end;

end.
