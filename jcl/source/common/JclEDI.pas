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
{ parsing of the file without knowledge of the standards at an Interchange level.  This enables    }
{ parsing and construction of EDI documents with different delimiters.  Various EDI  file errors   }
{ can also be detected.                                                                            }
{                                                                                                  }
{ Unit owner: Raymond Alexander                                                                    }
{ Date created: Before February, 1, 2001                                                           }
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

unit JclEDI;

{$I jcl.inc}

{$WEAKPACKAGEUNIT ON}

//Add the following directive in project options for debugging.
//{$DEFINE ENABLE_EDI_DEBUGGING}

interface

uses
  SysUtils, Classes, JclBase, JclStrings{, Dialogs};

const
  NA_LoopId = 'N/A'; //Constant used for loop id comparison
  ElementSpecId_Reserved = 'Reserved';

{$IFDEF ENABLE_EDI_DEBUGGING}
var
  Debug_EDIDataObjectsCreated: Int64;
  Debug_EDIDataObjectsDestroyed: Int64;
{$ENDIF}

type

  TEDIObject = class(TObject); //Base EDI Object
  TEDIObjectArray = array of TEDIObject;

  EJclEDIError = EJclError;

//--------------------------------------------------------------------------------------------------
//  EDI Forward Class Declarations
//--------------------------------------------------------------------------------------------------

  TEDIDataObject = class;
  TEDIDataObjectGroup = class;
  TEDIDataObjectLinkedListItem = class;
  TEDIDataObjectLinkedListHeader = class;

//--------------------------------------------------------------------------------------------------
//  EDI Delimiters Object
//--------------------------------------------------------------------------------------------------

  TEDIDelimiters = class(TEDIObject)
  private
    FSegmentDelimiter: string;
    FElementDelimiter: string;
    FSubElementSeperator: string; //Also known as: Component Data Seperator
    FSegmentDelimiterLength: Integer;
    FElementDelimiterLength: Integer;
    FSubelementSeperatorLength: Integer;
    procedure SetSD(const Delimiter: string);
    procedure SetED(const Delimiter: string);
    procedure SetSS(const Delimiter: string);
  public
    constructor Create; overload;
    constructor Create(const SD, ED, SS: string); overload;
  published
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

  TEDIDataObjectType = (ediUnknown, ediElement, ediCompositeElement, ediSegment, ediLoop,
    ediTransactionSet, ediMessage, ediFunctionalGroup, ediInterchangeControl, ediFile, ediCustom);

  TEDIDataObjectDataState = (ediCreated, ediAssembled, ediDissassembled);

  TEDIDataObjectArray = array of TEDIDataObject;

  TEDIDataObject = class(TEDIObject)
  private
    procedure SetDelimiters(const Delimiters: TEDIDelimiters);
  protected
    FEDIDOT: TEDIDataObjectType;
    FState: TEDIDataObjectDataState;
    FData: string;
    FLength: Integer;
    FParent: TEDIDataObject;
    FDelimiters: TEDIDelimiters;
    FErrorLog: TStrings;
    FSpecPointer: Pointer;
    FCustomData1: Pointer;
    FCustomData2: Pointer;
    function GetData: string;
    procedure SetData(const Data: string);
  public
    function Assemble: string; virtual; abstract;
    procedure Disassemble; virtual; abstract;
    constructor Create(Parent: TEDIDataObject); reintroduce;
    destructor Destroy; override;
    property SpecPointer: Pointer read FSpecPointer write FSpecPointer;
    property CustomData1: Pointer read FCustomData1 write FCustomData1;
    property CustomData2: Pointer read FCustomData2 write FCustomData2;
  published
    property State: TEDIDataObjectDataState read FState;
    property Data: string read GetData write SetData;
    property DataLength: Integer read FLength;
    property Parent: TEDIDataObject read FParent write FParent;
    property Delimiters: TEDIDelimiters read FDelimiters write SetDelimiters;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Data Object Group
//--------------------------------------------------------------------------------------------------

  TEDIDataObjectGroupArray = array of TEDIDataObjectGroup;

  TEDIDataObjectGroup = class(TEDIDataObject)
  private
  protected
    FEDIDataObjects: TEDIDataObjectArray;
    FCreateObjectType: TEDIDataObjectType;
    function GetCount: Integer;
    function GetEDIDataObject(Index: Integer): TEDIDataObject;
    procedure SetEDIDataObject(Index: Integer; EDIDataObject: TEDIDataObject);
    function InternalAssignDelimiters: TEDIDelimiters; virtual; abstract;
    function InternalCreateEDIDataObject: TEDIDataObject; virtual; abstract;
  public
    constructor Create(Parent: TEDIDataObject; EDIDataObjectCount: Integer{= 0}); reintroduce;
    destructor Destroy; override;
    //
    function AddEDIDataObject: Integer;
    function AppendEDIDataObject(EDIDataObject: TEDIDataObject): Integer;
    function InsertEDIDataObject(InsertIndex: Integer): Integer; overload;
    function InsertEDIDataObject(InsertIndex: Integer; EDIDataObject:
      TEDIDataObject): Integer; overload;
    procedure DeleteEDIDataObject(Index: Integer); overload;
    procedure DeleteEDIDataObject(EDIDataObject: TEDIDataObject); overload;
    //
    function AddEDIDataObjects(Count: Integer): Integer;
    function AppendEDIDataObjects(EDIDataObjectArray: TEDIDataObjectArray): Integer;
    function InsertEDIDataObjects(InsertIndex, Count: Integer): Integer; overload;
    function InsertEDIDataObjects(InsertIndex: Integer;
      EDIDataObjectArray: TEDIDataObjectArray): Integer; overload;
    procedure DeleteEDIDataObjects; overload;
    procedure DeleteEDIDataObjects(Index, Count: Integer); overload;
    //
    function GetIndexPositionFromParent: Integer; virtual;
    //
    property EDIDataObject[Index: Integer]: TEDIDataObject read GetEDIDataObject
      write SetEDIDataObject; default;
    property EDIDataObjects: TEDIDataObjectArray read FEDIDataObjects write FEDIDataObjects;
  published
    property CreateObjectType: TEDIDataObjectType read FCreateObjectType;
    property EDIDataObjectCount: Integer read GetCount; //[recommended instead of Length(EDIDataObject)]
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Data Object Linked List Header and Item classes
//--------------------------------------------------------------------------------------------------

  TEDIDataObjectLinkedListItemOptions = set of (lhFreeDataObject);

  TEDIDataObjectLinkedListItem = class(TEDIObject)
  protected
    FParent: TEDIDataObjectLinkedListHeader;
    FOptions: TEDIDataObjectLinkedListItemOptions;
    FPriorItem: TEDIDataObjectLinkedListItem;
    FNextItem: TEDIDataObjectLinkedListItem;
    FEDIDataObject: TEDIDataObject;
  public
    constructor Create(Parent: TEDIDataObjectLinkedListHeader;
      PriorItem: TEDIDataObjectLinkedListItem);
    destructor Destroy; override;
    function GetIndexPositionFromParent: Integer;
  published
    property PriorItem: TEDIDataObjectLinkedListItem read FPriorItem write FPriorItem;
    property NextItem: TEDIDataObjectLinkedListItem read FNextItem write FNextItem;
    property EDIDataObject: TEDIDataObject read FEDIDataObject write FEDIDataObject;
  end;

  TEDIDataObjectLinkedListHeader = class(TEDIObject)
  private
    function GetCount: Integer;
  protected
    FFirstItem: TEDIDataObjectLinkedListItem;
    FLastItem: TEDIDataObjectLinkedListItem;
    FCurrentItem: TEDIDataObjectLinkedListItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AppendEDIDataObject(EDIDataObject: TEDIDataObject);
    procedure DeleteEDIDataObjects(FreeReferences: Boolean = False);
    function First(Index: Integer = 0): TEDIDataObjectLinkedListItem;
    function Next: TEDIDataObjectLinkedListItem;
    function Prior: TEDIDataObjectLinkedListItem;
    function Last: TEDIDataObjectLinkedListItem;
  published
    property ItemCount: Integer read GetCount;
    property FirstItem: TEDIDataObjectLinkedListItem read FFirstItem;
    property LastItem: TEDIDataObjectLinkedListItem read FLastItem;
  end;

//--------------------------------------------------------------------------------------------------
//  Other
//--------------------------------------------------------------------------------------------------

function StringRemove(const S, Pattern: string; Flags: TReplaceFlags): string;
function StringReplace(const S, OldPattern, NewPattern: string; Flags: TReplaceFlags): string;

implementation

uses
  JclResources;

//==================================================================================================
// TEDIDelimiters
//==================================================================================================

{ TEDIDelimiters }

constructor TEDIDelimiters.Create;
begin
  inherited Create;
  SetSD('~');
  SetED('*');
  SetSS('>');
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIDelimiters.Create(const SD, ED, SS: string);
begin
  inherited Create;
  SetSD(SD);
  SetED(ED);
  SetSS(SS);
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

{ TEDIDataObject }

constructor TEDIDataObject.Create(Parent: TEDIDataObject);
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
  FSpecPointer := nil;
  FCustomData1 := nil;
  FCustomData2 := nil;
{$IFDEF ENABLE_EDI_DEBUGGING}
  Inc(Debug_EDIDataObjectsCreated);
{$ENDIF}
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIDataObject.Destroy;
begin
{$IFDEF ENABLE_EDI_DEBUGGING}
  Inc(Debug_EDIDataObjectsDestroyed);
{$ENDIF}
  if not Assigned(FParent) then
  begin
    if Assigned(FDelimiters) then
    begin
      FDelimiters.Free;
    end;
  end;
  FDelimiters := nil;
  FSpecPointer := nil;
  FCustomData1 := nil;
  FCustomData2 := nil;
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

//--------------------------------------------------------------------------------------------------

procedure TEDIDataObject.SetDelimiters(const Delimiters: TEDIDelimiters);
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

//==================================================================================================
// TEDIDataObjectGroup
//==================================================================================================

{ TEDIDataObjectGroup }

function TEDIDataObjectGroup.AddEDIDataObjects(Count: Integer): Integer;
var
  I, J: Integer;
begin
  I := Length(FEDIDataObjects);
  Result := I; //Return position of 1st
  //Resize
  SetLength(FEDIDataObjects, Length(FEDIDataObjects) + Count);
  //Add
  for J := I to High(FEDIDataObjects) do
  begin
    FEDIDataObjects[J]:= InternalCreateEDIDataObject;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectGroup.AddEDIDataObject: Integer;
begin
  SetLength(FEDIDataObjects, Length(FEDIDataObjects) + 1);
  FEDIDataObjects[High(FEDIDataObjects)] := InternalCreateEDIDataObject;
  Result := High(FEDIDataObjects); //Return position
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectGroup.AppendEDIDataObject(EDIDataObject: TEDIDataObject): Integer;
begin
  SetLength(FEDIDataObjects, Length(FEDIDataObjects) + 1);
  FEDIDataObjects[High(FEDIDataObjects)] := EDIDataObject;
  EDIDataObject.Parent := Self;
  Result := High(FEDIDataObjects); //Return position
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectGroup.AppendEDIDataObjects(EDIDataObjectArray: TEDIDataObjectArray): Integer;
var
  I, J, K: Integer;
begin
  I := 0;
  J := Length(FEDIDataObjects);
  Result := J; //Return position of 1st
  //Resize
  SetLength(FEDIDataObjects, Length(FEDIDataObjects) + Length(EDIDataObjectArray));
  //Append
  for K := J to High(EDIDataObjectArray) do
  begin
    FEDIDataObjects[K] := EDIDataObjectArray[I];
    FEDIDataObjects[K].Parent := Self;
    Inc(I);
  end;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIDataObjectGroup.Create(Parent: TEDIDataObject; EDIDataObjectCount: Integer);
begin
  if Assigned(Parent) then
  begin
    inherited Create(Parent);
  end
  else
  begin
    inherited Create(nil);
  end;
  FCreateObjectType := ediUnknown;
  SetLength(FEDIDataObjects, 0);
  if EDIDataObjectCount > 0 then
  begin
    AddEDIDataObjects(EDIDataObjectCount);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIDataObjectGroup.DeleteEDIDataObject(EDIDataObject: TEDIDataObject);
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

procedure TEDIDataObjectGroup.DeleteEDIDataObject(Index: Integer);
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
    raise EJclEDIError.CreateResRecFmt(@RsEDIError010, [Self.ClassName, IntToStr(Index)]);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIDataObjectGroup.DeleteEDIDataObjects;
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

procedure TEDIDataObjectGroup.DeleteEDIDataObjects(Index, Count: Integer);
var
  I: Integer;
begin
  if (Length(FEDIDataObjects) > 0) and (Index >= Low(FEDIDataObjects)) and
    (Index <= High(FEDIDataObjects)) then
  begin
    //Delete
    for I := Index to (Index + Count) - 1 do
    begin
      if Assigned(FEDIDataObjects[I]) then
      begin
        FEDIDataObjects[I].Free;
        FEDIDataObjects[I] := nil;
      end;
    end;
    //Shift
    for I := (Index + Count) to High(FEDIDataObjects) do
    begin
      FEDIDataObjects[I-Count] := FEDIDataObjects[I];
      FEDIDataObjects[I] := nil;
    end;
    //Resize
    SetLength(FEDIDataObjects, Length(FEDIDataObjects) - Count);
  end
  else
  begin
    raise EJclEDIError.CreateResRecFmt(@RsEDIError011, [IntToStr(Index)]);
  end;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIDataObjectGroup.Destroy;
begin
  DeleteEDIDataObjects;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectGroup.GetEDIDataObject(Index: Integer): TEDIDataObject;
begin
  if (Length(FEDIDataObjects) > 0) then
    if (Index >= Low(FEDIDataObjects)) then
      if (Index <= High(FEDIDataObjects)) then
      begin
        if not Assigned(FEDIDataObjects[Index]) then
        begin
          raise EJclEDIError.CreateResRecFmt(@RsEDIError006, [Self.ClassName, IntToStr(Index)]);
        end;
        Result := FEDIDataObjects[Index];
      end
      else
        raise EJclEDIError.CreateResRecFmt(@RsEDIError005, [Self.ClassName, IntToStr(Index)])
    else
      raise EJclEDIError.CreateResRecFmt(@RsEDIError004, [Self.ClassName, IntToStr(Index)])
  else
    raise EJclEDIError.CreateResRecFmt(@RsEDIError003, [Self.ClassName, IntToStr(Index)]);
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectGroup.InsertEDIDataObject(InsertIndex: Integer): Integer;
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
    FEDIDataObjects[InsertIndex] := InternalCreateEDIDataObject;
  end
  else
  begin
    Result := AddEDIDataObject;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectGroup.InsertEDIDataObject(InsertIndex: Integer;
  EDIDataObject: TEDIDataObject): Integer;
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

function TEDIDataObjectGroup.InsertEDIDataObjects(InsertIndex: Integer;
  EDIDataObjectArray: TEDIDataObjectArray): Integer;
var
  I, J, K: Integer;
begin
  Result := InsertIndex;
  I := Length(EDIDataObjectArray);
  if (Length(FEDIDataObjects) > 0) and (InsertIndex >= Low(FEDIDataObjects)) and
    (InsertIndex <= High(FEDIDataObjects)) then
  begin
    //Resize
    SetLength(FEDIDataObjects, Length(FEDIDataObjects) + I);
    //Shift
    for J := High(FEDIDataObjects) downto InsertIndex + I do
    begin
      FEDIDataObjects[J] := FEDIDataObjects[J-I];
      FEDIDataObjects[J-I] := nil;
    end;
    //Insert
    K := 0;
    for J := InsertIndex to (InsertIndex + I) - 1 do
    begin
      FEDIDataObjects[J] := EDIDataObjectArray[K];
      FEDIDataObjects[J].Parent := Self;
      Inc(K);
    end;
  end
  else
  begin
    Result := AppendEDIDataObjects(EDIDataObjectArray);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectGroup.InsertEDIDataObjects(InsertIndex, Count: Integer): Integer;
var
  I: Integer;
begin
  Result := InsertIndex;
  if (Length(FEDIDataObjects) > 0) and (InsertIndex >= Low(FEDIDataObjects)) and
    (InsertIndex <= High(FEDIDataObjects)) then
  begin
    //Resize
    SetLength(FEDIDataObjects, Length(FEDIDataObjects) + Count);
    //Shift
    for I := High(FEDIDataObjects) downto InsertIndex + Count do
    begin
      FEDIDataObjects[I] := FEDIDataObjects[I-Count];
      FEDIDataObjects[I-Count] := nil;
    end;
    //Insert
    for I := InsertIndex to (InsertIndex + Count) - 1 do
    begin
      FEDIDataObjects[I] := InternalCreateEDIDataObject;
    end;
  end
  else
  begin
    Result := AddEDIDataObjects(Count);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIDataObjectGroup.SetEDIDataObject(Index: Integer; EDIDataObject: TEDIDataObject);
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
        raise EJclEDIError.CreateResRecFmt(@RsEDIError009, [Self.ClassName, IntToStr(Index)])
    else
      raise EJclEDIError.CreateResRecFmt(@RsEDIError008, [Self.ClassName, IntToStr(Index)])
  else
    raise EJclEDIError.CreateResRecFmt(@RsEDIError007, [Self.ClassName, IntToStr(Index)]);
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectGroup.GetIndexPositionFromParent: Integer;
var
  I: Integer;
begin
  Result := -1;
  if Assigned(Parent) and (Parent is TEDIDataObjectGroup) then
  begin
    for I := Low(TEDIDataObjectGroup(Parent).EDIDataObjects) to
             High(TEDIDataObjectGroup(Parent).EDIDataObjects) do
    begin
      if TEDIDataObjectGroup(Parent).EDIDataObjects[I] = Self then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectGroup.GetCount: Integer;
begin
  Result := Length(FEDIDataObjects);
end;

//==================================================================================================
// TEDIDataObjectLinkedListItem
//==================================================================================================

{ TEDIDataObjectLinkedListItem }

constructor TEDIDataObjectLinkedListItem.Create(Parent: TEDIDataObjectLinkedListHeader;
  PriorItem: TEDIDataObjectLinkedListItem);
begin
  inherited Create;
  FOptions := [lhFreeDataObject];
  FEDIDataObject := nil;
  FPriorItem := PriorItem;
  FNextItem := nil;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIDataObjectLinkedListItem.Destroy;
begin
  FPriorItem := nil;
  FNextItem := nil;
  if (lhFreeDataObject in FOptions) and (FEDIDataObject <> nil) then
  begin
    FEDIDataObject.Free;
  end;
  FEDIDataObject := nil;
  FParent := nil;
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectLinkedListItem.GetIndexPositionFromParent: Integer;
var
  I: Integer;
  ListItem: TEDIDataObjectLinkedListItem;
begin
  Result := -1;
  for I := 0 to FParent.ItemCount - 1 do
  begin
    if I = 0 then
      ListItem := FParent.First
    else
      ListItem := FParent.Next;
    if Self = ListItem then
    begin
      Result := I;
      Break;
    end;
  end; //for I
end;

//==================================================================================================
// TEDIDataObjectLinkedListHeader
//==================================================================================================

{ TEDIDataObjectLinkedListHeader }

procedure TEDIDataObjectLinkedListHeader.AppendEDIDataObject(EDIDataObject: TEDIDataObject);
var
  ListItem: TEDIDataObjectLinkedListItem;
begin
  ListItem := TEDIDataObjectLinkedListItem.Create(Self, FLastItem);
  ListItem.EDIDataObject := EDIDataObject;
  if FLastItem <> nil then
  begin
    FLastItem.NextItem := ListItem;
  end;
  if FFirstItem = nil then
  begin
    FFirstItem := ListItem;
  end;
  FLastItem := ListItem;
  FCurrentItem := ListItem;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIDataObjectLinkedListHeader.Create;
begin
  inherited Create;
  FFirstItem := nil;
  FLastItem := nil;
  FCurrentItem := nil;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIDataObjectLinkedListHeader.DeleteEDIDataObjects(FreeReferences: Boolean = False);
var
  ListItem: TEDIDataObjectLinkedListItem;
  PriorItem: TEDIDataObjectLinkedListItem;
begin
  ListItem := FFirstItem;
  while ListItem <> nil do
  begin
    if FreeReferences and (ListItem.EDIDataObject <> nil) then
    begin
      ListItem.EDIDataObject.Free;
    end;
    ListItem.EDIDataObject := nil;
    PriorItem := ListItem;
    ListItem := ListItem.NextItem;
    PriorItem.Free;
  end;
  FFirstItem := nil;
  FLastItem := nil;
  FCurrentItem := nil;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIDataObjectLinkedListHeader.Destroy;
begin
  DeleteEDIDataObjects;
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectLinkedListHeader.First(Index: Integer): TEDIDataObjectLinkedListItem;
begin
  Result := nil;
  if Index = 0 then
    Result := FFirstItem;
  FCurrentItem := FFirstItem;
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectLinkedListHeader.GetCount: Integer;
var
  ListItem: TEDIDataObjectLinkedListItem;
begin
  Result := 0;
  ListItem := FFirstItem;
  while ListItem <> nil do
  begin
    ListItem := ListItem.NextItem;
    Inc(Result);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectLinkedListHeader.Last: TEDIDataObjectLinkedListItem;
begin
  FCurrentItem := FLastItem;
  Result := FCurrentItem;
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectLinkedListHeader.Next: TEDIDataObjectLinkedListItem;
begin
  FCurrentItem := FCurrentItem.NextItem;
  Result := FCurrentItem;
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectLinkedListHeader.Prior: TEDIDataObjectLinkedListItem;
begin
  FCurrentItem := FCurrentItem.PriorItem;
  Result := FCurrentItem;
end;

//--------------------------------------------------------------------------------------------------
//  Other
//--------------------------------------------------------------------------------------------------

function StringRemove(const S, Pattern: string; Flags: TReplaceFlags): string;
var
  SearchPattern: string;
  I, Offset, SearchPatternLength: Integer;
begin
  if rfIgnoreCase in Flags then
  begin
    Result := AnsiUpperCase(S);
    SearchPattern := AnsiUpperCase(Pattern);
  end else
  begin
    Result := S;
    SearchPattern := Pattern;
  end;
  SearchPatternLength := Length(SearchPattern);
  Result := S;

  I := 1;
  Offset := 1;
  while I <= Length(Result) do
  begin
    if SearchPatternLength = 1 then
    begin
      while Result[I] = SearchPattern[1] do
      begin
        Offset := Offset + SearchPatternLength;
        if not (rfReplaceAll in Flags) then Break;
        Inc(I);
      end;
    end
    else //PattLen > 1
    begin
      while Copy(Result, Offset, SearchPatternLength) = SearchPattern do
      begin
        Offset := Offset + SearchPatternLength;
        if not (rfReplaceAll in Flags) then Break;
      end;
    end;

    if Offset <= Length(Result) then
    begin
      Result[I] := S[Offset];
    end
    else
    begin
      Result[I] := #0;
      SetLength(Result, I-1);
      Break;
    end;

    if not (rfReplaceAll in Flags) then Break;

    Inc(I);
    Inc(Offset);
  end; //while
end;

//--------------------------------------------------------------------------------------------------

function StringReplace(const S, OldPattern, NewPattern: string; Flags: TReplaceFlags): string;
var
  SearchString, SearchPattern: string;
  I, SearchIndex, ReplaceIndex: Integer;
  SearchPatternLength, ReplacePatternLength: Integer;
  SearchResult, ReplaceCount: Integer;
begin
  Result := '';
  //Handle Case Sensitivity
  if rfIgnoreCase in Flags then
  begin
    SearchString := AnsiUpperCase(S);
    SearchPattern := AnsiUpperCase(OldPattern);
  end
  else
  begin
    SearchString := S;
    SearchPattern := OldPattern;
  end;
  SearchPatternLength := Length(OldPattern);
  ReplacePatternLength := Length(NewPattern);
  //Calculate length of result string
  ReplaceCount := 0;
  SearchResult := StrSearch(SearchPattern, SearchString, 1);
  while SearchResult <> 0 do
  begin
    Inc(SearchResult);
    Inc(ReplaceCount);
    SearchResult := StrSearch(SearchPattern, SearchString, SearchResult);
  end;
  SetLength(Result, Length(S) + ((ReplacePatternLength - SearchPatternLength) * ReplaceCount));
  //Shift the characters
  ReplaceCount := 0;
  ReplaceIndex := 1;
  SearchIndex := 1;
  while (ReplaceIndex <= Length(Result)) and (SearchIndex <= Length(SearchString)) do
  begin
    if (rfReplaceAll in Flags) or ((not (rfReplaceAll in Flags)) and (ReplaceCount = 0)) then
    begin
      while Copy(SearchString, SearchIndex, SearchPatternLength) = SearchPattern do
      begin
        //Move forward in the search string
        SearchIndex := SearchIndex + Length(SearchPattern);
        //Replace old pattern
        I := 1;
        while (ReplaceIndex <= Length(Result)) and (I <= ReplacePatternLength) do
        begin
          Result[ReplaceIndex] := NewPattern[I];
          Inc(I);
          Inc(ReplaceIndex);
        end; //while
        Inc(ReplaceCount);
        if not (rfReplaceAll in Flags) then Break;
      end; //while
    end; //if (rfReplaceAll in Flags) or ((not (rfReplaceAll in Flags)) and (ReplaceCount = 0)) then

    if (ReplaceIndex <= Length(Result)) and (SearchIndex <= Length(SearchString)) then
    begin
      Result[ReplaceIndex] := S[SearchIndex];
    end;

    if not (rfReplaceAll in Flags) then Break;

    Inc(SearchIndex);
    Inc(ReplaceIndex);
  end; //while
end;

end.
