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
{ The Initial Developer of the Original Code is Raymond Alexander.                                 }
{ Portions created by Raymond Alexander are Copyright Raymond Alexander. All rights reserved.      }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Raymond Alexander (rayspostbox3), Robert Marquardt, Robert Rossmair, Petr Vones                }
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

// $Id$

unit JclEDI;

{$I jcl.inc}

{$IFDEF SUPPORTS_WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF SUPPORTS_WEAKPACKAGEUNIT}

// Add the following directive in project options for debugging memory leaks.
// {$DEFINE ENABLE_EDI_DEBUGGING}

// (Default) Enable the following directive to use the TEDIObjectList implementation instead of
// the TEDIDataObjectArray implementation.  This new directive replaces the previous
// "OPTIMIZED_DISASSEMBLE" directive which was a temporary patch to prevent memory fragmentation.
{$DEFINE OPTIMIZED_INTERNAL_STRUCTURE}

interface

uses
  SysUtils, Classes,
  JclBase;

const
  NA_LoopId = 'N/A'; // Constant used for loop id comparison
  ElementSpecId_Reserved = 'Reserved';

  EDIDataType_Numeric = 'N';
  EDIDataType_Decimal = 'R';
  EDIDataType_Identifier = 'ID';
  EDIDataType_String = 'AN';
  EDIDataType_Date = 'DT';
  EDIDataType_Time = 'TM';
  EDIDataType_Binary = 'B';

{$IFDEF ENABLE_EDI_DEBUGGING}
var
  Debug_EDIDataObjectsCreated: Int64;
  Debug_EDIDataObjectsDestroyed: Int64;
  Debug_EDIDataObjectListCreated: Int64;
  Debug_EDIDataObjectListDestroyed: Int64;
  Debug_EDIDataObjectListItemsCreated: Int64;
  Debug_EDIDataObjectListItemsDestroyed: Int64;
{$ENDIF}

type
  TEDIObject = class(TObject); // Base EDI Object
  TEDIObjectArray = array of TEDIObject;

  EJclEDIError = EJclError;

//--------------------------------------------------------------------------------------------------
//  EDI Forward Class Declarations
//--------------------------------------------------------------------------------------------------

  TEDIDataObject = class;
  TEDIDataObjectGroup = class;
  TEDIObjectListItem = class;
  TEDIObjectList = class;
  TEDIDataObjectListItem = class;
  TEDIDataObjectList = class;  

//--------------------------------------------------------------------------------------------------
//  EDI Delimiters Object
//--------------------------------------------------------------------------------------------------

  TEDIDelimiters = class(TEDIObject)
  private
    FSegmentDelimiter: string;
    FElementDelimiter: string;
    FSubElementSeperator: string; // Also known as: Component Data Seperator
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

  TEDIDataObjectType =
   (ediUnknown, ediElement, ediCompositeElement, ediSegment, ediLoop,
    ediTransactionSet, ediMessage, ediFunctionalGroup,
    ediInterchangeControl, ediFile, ediCustom);

  TEDIDataObjectDataState = (ediCreated, ediAssembled, ediDisassembled);

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
    FSpecPointer: TEDIObject;
    FCustomData1: Pointer;
    FCustomData2: Pointer;
    function GetData: string;
    procedure SetData(const Data: string);
  public
    constructor Create(Parent: TEDIDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; virtual; abstract;
    procedure Disassemble; virtual; abstract;
    property SpecPointer: TEDIObject read FSpecPointer write FSpecPointer;
    property CustomData1: Pointer read FCustomData1 write FCustomData1;
    property CustomData2: Pointer read FCustomData2 write FCustomData2;
  published
    property State: TEDIDataObjectDataState read FState;
    property Data: string read GetData write SetData;
    property DataLength: Integer read FLength;
    property Parent: TEDIDataObject read FParent write FParent;
    property Delimiters: TEDIDelimiters read FDelimiters write SetDelimiters;
  end;

  TEDIDataObjectArray = array of TEDIDataObject;

//--------------------------------------------------------------------------------------------------
//  EDI Data Object Group
//--------------------------------------------------------------------------------------------------

  TEDIDataObjectGroup = class(TEDIDataObject)
  protected
    FGroupIsParent: Boolean;
    {$IFDEF OPTIMIZED_INTERNAL_STRUCTURE}
    FEDIDataObjects: TEDIDataObjectList;
    {$ELSE}
    FEDIDataObjects: TEDIDataObjectArray;
    {$ENDIF}
    FCreateObjectType: TEDIDataObjectType;
    function GetCount: Integer;
    function GetEDIDataObject(Index: Integer): TEDIDataObject;
    procedure SetEDIDataObject(Index: Integer; EDIDataObject: TEDIDataObject);
    function InternalAssignDelimiters: TEDIDelimiters; virtual; abstract;
    function InternalCreateEDIDataObject: TEDIDataObject; virtual; abstract;
  public
    constructor Create(Parent: TEDIDataObject; EDIDataObjectCount: Integer = 0); reintroduce;
    destructor Destroy; override;
    function IndexIsValid(Index: Integer): Boolean;
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
    {$IFDEF OPTIMIZED_INTERNAL_STRUCTURE}
    property EDIDataObjects: TEDIDataObjectList read FEDIDataObjects;
    {$ELSE}
    property EDIDataObjects: TEDIDataObjectArray read FEDIDataObjects;
    {$ENDIF}
  published
    property CreateObjectType: TEDIDataObjectType read FCreateObjectType;
    property EDIDataObjectCount: Integer read GetCount;
  end;

  TEDIDataObjectGroupArray = array of TEDIDataObjectGroup;

//--------------------------------------------------------------------------------------------------
//  EDI Data Object Linked List Header and Item classes
//--------------------------------------------------------------------------------------------------

  TEDIObjectListItem = class(TEDIObject)
  protected
    FParent: TEDIObjectList;
    FPriorItem: TEDIObjectListItem;
    FNextItem: TEDIObjectListItem;
    FEDIObject: TEDIObject;
    FItemIndex: Integer;
    FName: string;
  public
    constructor Create(Parent: TEDIObjectList; PriorItem: TEDIObjectListItem;
      EDIObject: TEDIObject = nil);
    destructor Destroy; override;
    function GetIndexPositionFromParent: Integer;
    procedure FreeAndNilEDIDataObject;
  published
    property ItemIndex: Integer read FItemIndex write FItemIndex;
    property PriorItem: TEDIObjectListItem read FPriorItem write FPriorItem;
    property NextItem: TEDIObjectListItem read FNextItem write FNextItem;
    property EDIObject: TEDIObject read FEDIObject write FEDIObject;
    property Name: string read FName write FName;
    property Parent: TEDIObjectList read FParent write FParent;
  end;

  TEDIDataObjectListOptions = set of (loAutoUpdateIndexes);

  TEDIObjectList = class(TEDIObject)
  private
    function GetItem(Index: Integer): TEDIObjectListItem;
  protected
    FOwnsObjects: Boolean;
    FCount: Integer;
    FOptions: TEDIDataObjectListOptions;
    FFirstItem: TEDIObjectListItem;
    FLastItem: TEDIObjectListItem;
    FCurrentItem: TEDIObjectListItem;
    function GetEDIObject(Index: Integer): TEDIObject;
    procedure SetEDIObject(Index: Integer; const Value: TEDIObject);
    function CreateListItem(PriorItem: TEDIObjectListItem;
      EDIObject: TEDIObject = nil): TEDIObjectListItem; virtual;
  public
    constructor Create(OwnsObjects: Boolean = True);
    destructor Destroy; override;
    procedure Add(Item: TEDIObjectListItem; Name: string = ''); overload;
    function Add(EDIObject: TEDIObject; Name: string = ''): TEDIObjectListItem; overload;
    function Find(Item: TEDIObjectListItem): TEDIObjectListItem; overload;
    function Find(EDIObject: TEDIObject): TEDIObjectListItem; overload;
    function FindEDIObject(EDIObject: TEDIObject): TEDIObject;
    function Extract(Item: TEDIObjectListItem): TEDIObjectListItem; overload; virtual;
    function Extract(EDIObject: TEDIObject): TEDIObject; overload; virtual;
    procedure Remove(Item: TEDIObjectListItem); overload;
    procedure Remove(EDIObject: TEDIObject); overload;
    function Insert(Item, BeforeItem: TEDIObjectListItem): TEDIObjectListItem; overload;
    function Insert(EDIObject, BeforeEDIObject: TEDIObject): TEDIObjectListItem; overload;
    function Insert(BeforeItem: TEDIObjectListItem): TEDIObjectListItem; overload;
    function Insert(BeforeEDIObject: TEDIObject): TEDIObjectListItem; overload;
    procedure Clear;
    function First(Index: Integer = 0): TEDIObjectListItem; virtual;
    function Next: TEDIObjectListItem; virtual;
    function Prior: TEDIObjectListItem; virtual;
    function Last: TEDIObjectListItem; virtual;
    procedure UpdateCount;
    // ...ByName procedures and functions
    function FindItemByName(Name: string;
      StartItem: TEDIObjectListItem = nil): TEDIObjectListItem; virtual;
    function ReturnListItemsByName(Name: string): TEDIObjectList; virtual;
    // Dynamic Array Emulation
    function IndexOf(Item: TEDIObjectListItem): Integer; overload;
    function IndexOf(EDIObject: TEDIObject): Integer; overload;
    function IndexIsValid(Index: Integer): Boolean;
    procedure Insert(InsertIndex: Integer; EDIObject: TEDIObject); overload;
    procedure Delete(Index: Integer); overload;
    procedure Delete(EDIObject: TEDIObject); overload;
    procedure UpdateIndexes(StartItem: TEDIObjectListItem = nil);
    //
    property Item[Index: Integer]: TEDIObjectListItem read GetItem;
    property EDIObject[Index: Integer]: TEDIObject read GetEDIObject
      write SetEDIObject; default;
  published
    property Count: Integer read FCount;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    property Options: TEDIDataObjectListOptions read FOptions write FOptions;
    property CurrentItem: TEDIObjectListItem read FCurrentItem;    
  end;

  TEDIDataObjectListItem = class(TEDIObjectListItem)
  private
    function GetEDIDataObject: TEDIDataObject;
    procedure SetEDIDataObject(const Value: TEDIDataObject);
  published
    property EDIDataObject: TEDIDataObject read GetEDIDataObject write SetEDIDataObject;
  end;

  TEDIDataObjectList = class(TEDIObjectList)
  private
    function GetEDIDataObject(Index: Integer): TEDIDataObject;
    procedure SetEDIDataObject(Index: Integer; const Value: TEDIDataObject);
  public
    function CreateListItem(PriorItem: TEDIObjectListItem;
      EDIObject: TEDIObject = nil): TEDIObjectListItem; override;  
    property EDIDataObject[Index: Integer]: TEDIDataObject read GetEDIDataObject
      write SetEDIDataObject; default;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Loop Stack
//--------------------------------------------------------------------------------------------------

  TEDILoopStackRecord = record
    SegmentId: string;
    SpecStartIndex: Integer;
    OwnerLoopId: string;
    ParentLoopId: string;
    EDIObject: TEDIObject;
    EDISpecObject: TEDIObject;    
  end;

  TEDILoopStackArray = array of TEDILoopStackRecord;

  TEDILoopStackFlags = (ediAltStackPointer, ediStackResized, ediLoopRepeated);

  TEDILoopStackFlagSet = set of TEDILoopStackFlags;

  TEDILoopStackOnAddLoopEvent = procedure(StackRecord: TEDILoopStackRecord;
    SegmentId, OwnerLoopId, ParentLoopId: string; var EDIObject: TEDIObject) of object;

  TEDILoopStack = class(TEDIObject)
  private
    function GetSize: Integer;
  protected
    FStack: TEDILoopStackArray;
    FFlags: TEDILoopStackFlagSet;
    FCheckAssignedEDIObject: Boolean;
    FOnAddLoop: TEDILoopStackOnAddLoopEvent;
    procedure DoAddLoop(StackRecord: TEDILoopStackRecord;
      SegmentId, OwnerLoopId, ParentLoopId: string; var EDIObject: TEDIObject);
  public
    constructor Create;
    destructor Destroy; override;
    // Basic Stack Routines
    function Peek: TEDILoopStackRecord; overload;
    function Peek(Index: Integer): TEDILoopStackRecord; overload;
    procedure Pop(Index: Integer);
    function Push(SegmentId, OwnerLoopId, ParentLoopId: string; StartIndex: Integer;
      EDIObject: TEDIObject): Integer;
    // Extended Stack Routines
    function GetSafeStackIndex(Index: Integer): Integer;
    function SetStackPointer(OwnerLoopId, ParentLoopId: string): Integer;
    procedure UpdateStackObject(EDIObject: TEDIObject);
    procedure UpdateStackData(SegmentId, OwnerLoopId, ParentLoopId: string; StartIndex: Integer;
      EDIObject: TEDIObject);
    // Extended Stack Routines
    function ValidateLoopStack(SegmentId, OwnerLoopId, ParentLoopId: string; StartIndex: Integer;
      EDIObject: TEDIObject): TEDILoopStackRecord;
    function Debug: string;
    //
    property Stack: TEDILoopStackArray read FStack;
  published
    property Size: Integer read GetSize;
    property Flags: TEDILoopStackFlagSet read FFlags write FFlags;
    property OnAddLoop: TEDILoopStackOnAddLoopEvent read FOnAddLoop write FOnAddLoop;
  end;

//--------------------------------------------------------------------------------------------------
//  Other
//--------------------------------------------------------------------------------------------------

// Compatibility functions
function StringRemove(const S, Pattern: string; Flags: TReplaceFlags): string;
function StringReplace(const S, OldPattern, NewPattern: string; Flags: TReplaceFlags): string;

implementation

uses
  JclResources, JclStrings;

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
  end
  else
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
        if not (rfReplaceAll in Flags) then
          Break;
        Inc(I);
      end;
    end
    else // SearchPatternLength > 1
    begin
      while Copy(Result, Offset, SearchPatternLength) = SearchPattern do
      begin
        Offset := Offset + SearchPatternLength;
        if not (rfReplaceAll in Flags) then
          Break;
      end;
    end;

    if Offset <= Length(Result) then
      Result[I] := S[Offset]
    else
    begin
      Result[I] := #0;
      SetLength(Result, I-1);
      Break;
    end;

    if not (rfReplaceAll in Flags) then
      Break;

    Inc(I);
    Inc(Offset);
  end;
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
  // Handle Case Sensitivity
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
  // Calculate length of result string
  ReplaceCount := 0;
  SearchResult := StrSearch(SearchPattern, SearchString, 1);
  while SearchResult <> 0 do
  begin
    Inc(SearchResult);
    Inc(ReplaceCount);
    SearchResult := StrSearch(SearchPattern, SearchString, SearchResult);
  end;
  SetLength(Result, Length(S) + ((ReplacePatternLength - SearchPatternLength) * ReplaceCount));
  // Copy the characters by looping through the result and source at the same time
  ReplaceCount := 0;
  ReplaceIndex := 1;
  SearchIndex := 1;
  // Loop while the indexes are still in range
  while (ReplaceIndex <= Length(Result)) and (SearchIndex <= Length(SearchString)) do
  begin
    // Enter algorithm if replacing a pattern or there have been no replacements yet
    if (rfReplaceAll in Flags) or ((not (rfReplaceAll in Flags)) and (ReplaceCount = 0)) then
      // Replace the pattern (including repeating patterns)
      while Copy(SearchString, SearchIndex, SearchPatternLength) = SearchPattern do
      begin
        // Move forward in the search string
        SearchIndex := SearchIndex + Length(SearchPattern);
        // Replace an old pattern by writing the new pattern to the result
        I := 1;
        while (ReplaceIndex <= Length(Result)) and (I <= ReplacePatternLength) do
        begin
          Result[ReplaceIndex] := NewPattern[I];
          Inc(I);
          Inc(ReplaceIndex);
        end;
        //
        Inc(ReplaceCount);
        // If only making one replacement then break
        if not (rfReplaceAll in Flags) then
          Break;
      end;

    // Copy character
    if (ReplaceIndex <= Length(Result)) and (SearchIndex <= Length(SearchString)) then
      Result[ReplaceIndex] := S[SearchIndex];

    if ReplacePatternLength > 0 then
      if not (rfReplaceAll in Flags) then
        Break;

    // Set indexes for next copy
    Inc(SearchIndex);
    Inc(ReplaceIndex);
  end;
end;

//==================================================================================================
// TEDIDelimiters
//==================================================================================================

{ TEDIDelimiters }

constructor TEDIDelimiters.Create;
begin
  Create('~', '*', '>');
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
  FParent := Parent;
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
    FDelimiters.Free;
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
    FreeAndNil(FDelimiters);
  FDelimiters := Delimiters;
end;

//==================================================================================================
// TEDIDataObjectGroup
//==================================================================================================

{ TEDIDataObjectGroup }

function TEDIDataObjectGroup.AddEDIDataObjects(Count: Integer): Integer;
var
  I: Integer;
  {$IFNDEF OPTIMIZED_INTERNAL_STRUCTURE}
  J: Integer;
  {$ENDIF}
begin
  {$IFDEF OPTIMIZED_INTERNAL_STRUCTURE}
  Result := FEDIDataObjects.Count; // Return position of 1st
  for I := 1 to Count do
    FEDIDataObjects.Add(InternalCreateEDIDataObject);
  {$ELSE}
  I := Length(FEDIDataObjects);
  Result := I; // Return position of 1st
  // Resize
  SetLength(FEDIDataObjects, Length(FEDIDataObjects) + Count);
  // Add
  for J := I to High(FEDIDataObjects) do
    FEDIDataObjects[J]:= InternalCreateEDIDataObject;
  {$ENDIF}
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectGroup.AddEDIDataObject: Integer;
begin
  {$IFDEF OPTIMIZED_INTERNAL_STRUCTURE}
  Result := FEDIDataObjects.Count; // Return position
  FEDIDataObjects.Add(InternalCreateEDIDataObject);
  {$ELSE}
  SetLength(FEDIDataObjects, Length(FEDIDataObjects) + 1);
  FEDIDataObjects[High(FEDIDataObjects)] := InternalCreateEDIDataObject;
  Result := High(FEDIDataObjects); // Return position
  {$ENDIF}
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectGroup.AppendEDIDataObject(EDIDataObject: TEDIDataObject): Integer;
begin
  {$IFDEF OPTIMIZED_INTERNAL_STRUCTURE}
  Result := FEDIDataObjects.Count; // Return position
  FEDIDataObjects.Add(EDIDataObject);
  if FGroupIsParent then
    EDIDataObject.Parent := Self;
  {$ELSE}
  SetLength(FEDIDataObjects, Length(FEDIDataObjects) + 1);
  FEDIDataObjects[High(FEDIDataObjects)] := EDIDataObject;
  if FGroupIsParent then
    EDIDataObject.Parent := Self;
  Result := High(FEDIDataObjects); // Return position
  {$ENDIF}
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectGroup.AppendEDIDataObjects(EDIDataObjectArray: TEDIDataObjectArray): Integer;
var
  I: Integer;
  {$IFNDEF OPTIMIZED_INTERNAL_STRUCTURE}
  J, K: Integer;
  {$ENDIF}
begin
  {$IFDEF OPTIMIZED_INTERNAL_STRUCTURE}
  Result := FEDIDataObjects.Count; // Return position of 1st
  for I := Low(EDIDataObjectArray) to High(EDIDataObjectArray) do
  begin
    FEDIDataObjects.Add(EDIDataObjectArray[I]);
    if FGroupIsParent then
      EDIDataObjectArray[I].Parent := Self;
  end;
  {$ELSE}
  I := 0;
  J := Length(FEDIDataObjects);
  Result := J; // Return position of 1st
  // Resize
  SetLength(FEDIDataObjects, Length(FEDIDataObjects) + Length(EDIDataObjectArray));
  // Append
  for K := J to High(EDIDataObjectArray) do
  begin
    FEDIDataObjects[K] := EDIDataObjectArray[I];
    if FGroupIsParent then
      FEDIDataObjects[K].Parent := Self;
    Inc(I);
  end;
  {$ENDIF}
end;

//--------------------------------------------------------------------------------------------------

constructor TEDIDataObjectGroup.Create(Parent: TEDIDataObject; EDIDataObjectCount: Integer);
begin
  if Assigned(Parent) then
    inherited Create(Parent)
  else
    inherited Create(nil);
  FCreateObjectType := ediUnknown;
  FGroupIsParent := True;
  {$IFDEF OPTIMIZED_INTERNAL_STRUCTURE}
  FEDIDataObjects := TEDIDataObjectList.Create;
  {$ELSE}
  SetLength(FEDIDataObjects, 0);
  {$ENDIF}
  if EDIDataObjectCount > 0 then
    AddEDIDataObjects(EDIDataObjectCount);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIDataObjectGroup.DeleteEDIDataObject(EDIDataObject: TEDIDataObject);
{$IFNDEF OPTIMIZED_INTERNAL_STRUCTURE}
var
  I: Integer;
{$ENDIF}
begin
  {$IFDEF OPTIMIZED_INTERNAL_STRUCTURE}
  if loAutoUpdateIndexes in FEDIDataObjects.Options then
    FEDIDataObjects.Delete(EDIDataObject)
  else
    FEDIDataObjects.Remove(EDIDataObject);
  {$ELSE}
  for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
    if FEDIDataObjects[I] = EDIDataObject then
      DeleteEDIDataObject(I);
  {$ENDIF}
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIDataObjectGroup.DeleteEDIDataObject(Index: Integer);
{$IFNDEF OPTIMIZED_INTERNAL_STRUCTURE}
//var
//  I: Integer;
{$ENDIF}
begin
  if IndexIsValid(Index) then
  begin
    {$IFDEF OPTIMIZED_INTERNAL_STRUCTURE}
    FEDIDataObjects.Delete(Index);
    {$ELSE}
    // Delete
    FreeAndNil(FEDIDataObjects[Index]);
    // Shift (Keep comment here for reference.)
    //for I := Index + 1 to High(FEDIDataObjects) do
    //  FEDIDataObjects[I-1] := FEDIDataObjects[I];
    Move(FEDIDataObjects[Index+1], FEDIDataObjects[Index],
      SizeOf(FEDIDataObjects[0]) * (Length(FEDIDataObjects)-(Index+1)));
    // Resize
    SetLength(FEDIDataObjects, High(FEDIDataObjects));
    {$ENDIF}
  end
  else
    raise EJclEDIError.CreateResRecFmt(@RsEDIError010, [Self.ClassName, IntToStr(Index)]);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIDataObjectGroup.DeleteEDIDataObjects;
{$IFNDEF OPTIMIZED_INTERNAL_STRUCTURE}
var
  I: Integer;
{$ENDIF}
begin
  {$IFDEF OPTIMIZED_INTERNAL_STRUCTURE}
  FEDIDataObjects.Clear;
  {$ELSE}
  for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
    // Delete
    FreeAndNil(FEDIDataObjects[I]);
  // Resize
  SetLength(FEDIDataObjects, 0);
  {$ENDIF}
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIDataObjectGroup.DeleteEDIDataObjects(Index, Count: Integer);
var
  I: Integer;
begin
  if IndexIsValid(Index) then
  begin
    {$IFDEF OPTIMIZED_INTERNAL_STRUCTURE}
    FEDIDataObjects.Options := FEDIDataObjects.Options - [loAutoUpdateIndexes];
    try
      for I := 1 to Count do
        DeleteEDIDataObject(Index);
    finally
      FEDIDataObjects.Options := FEDIDataObjects.Options + [loAutoUpdateIndexes];
    end;
    {$ELSE}
    // Delete
    for I := Index to (Index + Count) - 1 do
      FreeAndNil(FEDIDataObjects[I]);
    // Shift (Keep comment here for reference.)
    //for I := (Index + Count) to High(FEDIDataObjects) do
    //begin
    //  FEDIDataObjects[I-Count] := FEDIDataObjects[I];
    //  FEDIDataObjects[I] := nil;
    //end;
    Move(FEDIDataObjects[Index+Count], FEDIDataObjects[Index],
      SizeOf(FEDIDataObjects[0]) * (Length(FEDIDataObjects)-(Index+Count)));
    // Resize
    SetLength(FEDIDataObjects, Length(FEDIDataObjects) - Count);
    {$ENDIF}
  end
  else
    raise EJclEDIError.CreateResRecFmt(@RsEDIError011, [IntToStr(Index)]);
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIDataObjectGroup.Destroy;
begin
  DeleteEDIDataObjects;
  {$IFDEF OPTIMIZED_INTERNAL_STRUCTURE}
  FreeAndNil(FEDIDataObjects);
  {$ENDIF}  
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectGroup.GetEDIDataObject(Index: Integer): TEDIDataObject;
begin
  {$IFDEF OPTIMIZED_INTERNAL_STRUCTURE}
  if FEDIDataObjects.Count > 0 then
    if Index >= 0 then
      if Index <= FEDIDataObjects.Count - 1 then
      begin
        if not Assigned(FEDIDataObjects[Index]) then
          raise EJclEDIError.CreateResRecFmt(@RsEDIError006, [Self.ClassName, IntToStr(Index)]);
        Result := FEDIDataObjects[Index];
      end
      else
        raise EJclEDIError.CreateResRecFmt(@RsEDIError005, [Self.ClassName, IntToStr(Index)])
    else
      raise EJclEDIError.CreateResRecFmt(@RsEDIError004, [Self.ClassName, IntToStr(Index)])
  else
    raise EJclEDIError.CreateResRecFmt(@RsEDIError003, [Self.ClassName, IntToStr(Index)]);
  {$ELSE}
  if Length(FEDIDataObjects) > 0 then
    if Index >= Low(FEDIDataObjects) then
      if Index <= High(FEDIDataObjects) then
      begin
        if not Assigned(FEDIDataObjects[Index]) then
          raise EJclEDIError.CreateResRecFmt(@RsEDIError006, [Self.ClassName, IntToStr(Index)]);
        Result := FEDIDataObjects[Index];
      end
      else
        raise EJclEDIError.CreateResRecFmt(@RsEDIError005, [Self.ClassName, IntToStr(Index)])
    else
      raise EJclEDIError.CreateResRecFmt(@RsEDIError004, [Self.ClassName, IntToStr(Index)])
  else
    raise EJclEDIError.CreateResRecFmt(@RsEDIError003, [Self.ClassName, IntToStr(Index)]);
  {$ENDIF}
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectGroup.IndexIsValid(Index: Integer): Boolean;
begin
  {$IFDEF OPTIMIZED_INTERNAL_STRUCTURE}
  Result := FEDIDataObjects.IndexIsValid(Index);
  {$ELSE}
  Result := False;
  if (Length(FEDIDataObjects) > 0) and (Index >= Low(FEDIDataObjects)) and
    (Index <= High(FEDIDataObjects)) then Result := True;
  {$ENDIF}
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectGroup.InsertEDIDataObject(InsertIndex: Integer): Integer;
{$IFNDEF OPTIMIZED_INTERNAL_STRUCTURE}
//var
//  I: Integer;
{$ENDIF}
begin
  Result := InsertIndex; // Return position
  if IndexIsValid(InsertIndex) then
  begin
    {$IFDEF OPTIMIZED_INTERNAL_STRUCTURE}
    FEDIDataObjects.Insert(InsertIndex, InternalCreateEDIDataObject)
    {$ELSE}
    // Resize
    SetLength(FEDIDataObjects, Length(FEDIDataObjects) + 1);
    // Shift (Keep comment here for reference.)
    //for I := High(FEDIDataObjects) downto InsertIndex + 1 do
    //  FEDIDataObjects[I] := FEDIDataObjects[I-1];
    Move(FEDIDataObjects[InsertIndex], FEDIDataObjects[InsertIndex+1],
      SizeOf(FEDIDataObjects[0]) * (Length(FEDIDataObjects)-(InsertIndex+1)));
    // Insert
    FEDIDataObjects[InsertIndex] := InternalCreateEDIDataObject;
    {$ENDIF}
  end
  else
    Result := AddEDIDataObject;
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectGroup.InsertEDIDataObject(InsertIndex: Integer;
  EDIDataObject: TEDIDataObject): Integer;
{$IFNDEF OPTIMIZED_INTERNAL_STRUCTURE}
//var
//  I: Integer;
{$ENDIF}
begin
  Result := InsertIndex; // Return position
  if IndexIsValid(InsertIndex) then
  begin
    {$IFDEF OPTIMIZED_INTERNAL_STRUCTURE}
    FEDIDataObjects.Insert(InsertIndex, EDIDataObject);
    if FGroupIsParent then
      EDIDataObject.Parent := Self;
    {$ELSE}
    // Resize
    SetLength(FEDIDataObjects, Length(FEDIDataObjects) + 1);
    // Shift (Keep comment here for reference.)
    //for I := High(FEDIDataObjects) downto InsertIndex + 1 do
    //  FEDIDataObjects[I] := FEDIDataObjects[I-1];
    Move(FEDIDataObjects[InsertIndex], FEDIDataObjects[InsertIndex+1],
      SizeOf(FEDIDataObjects[0]) * (Length(FEDIDataObjects)-(InsertIndex+1)));
    // Insert
    FEDIDataObjects[InsertIndex] := EDIDataObject;
    if FGroupIsParent then
      FEDIDataObjects[InsertIndex].Parent := Self;
    {$ENDIF}
  end
  else
    Result := AppendEDIDataObject(EDIDataObject);
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectGroup.InsertEDIDataObjects(InsertIndex: Integer;
  EDIDataObjectArray: TEDIDataObjectArray): Integer;
var
  I{$IFNDEF OPTIMIZED_INTERNAL_STRUCTURE}, J, K{$ENDIF}: Integer;
begin
  Result := InsertIndex; // Return position of 1st
  if IndexIsValid(InsertIndex) then
  begin
    {$IFDEF OPTIMIZED_INTERNAL_STRUCTURE}
    for I := High(EDIDataObjectArray) downto Low(EDIDataObjectArray) do
    begin
      FEDIDataObjects.Insert(InsertIndex, EDIDataObjectArray[I]);
      if FGroupIsParent then
        EDIDataObjectArray[I].Parent := Self;
    end;
    {$ELSE}
    I := Length(EDIDataObjectArray);
    // Resize
    SetLength(FEDIDataObjects, Length(FEDIDataObjects) + I);
    // Shift (Keep comment here for reference.)
    //for J := High(FEDIDataObjects) downto InsertIndex + I do
    //begin
    //  FEDIDataObjects[J] := FEDIDataObjects[J-I];
    //  FEDIDataObjects[J-I] := nil;
    //end;
    Move(FEDIDataObjects[InsertIndex], FEDIDataObjects[InsertIndex+I],
      SizeOf(FEDIDataObjects[0]) * (Length(FEDIDataObjects)-(InsertIndex+I)));
    // Insert
    K := 0;
    for J := InsertIndex to (InsertIndex + I) - 1 do
    begin
      FEDIDataObjects[J] := EDIDataObjectArray[K];
      if FGroupIsParent then
        FEDIDataObjects[J].Parent := Self;
      Inc(K);
    end;
    {$ENDIF}
  end
  else
    Result := AppendEDIDataObjects(EDIDataObjectArray);
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectGroup.InsertEDIDataObjects(InsertIndex, Count: Integer): Integer;
var
  I: Integer;
begin
  Result := InsertIndex; // Return position of 1st
  if IndexIsValid(InsertIndex) then
  begin
    {$IFDEF OPTIMIZED_INTERNAL_STRUCTURE}
    for I := 1 to Count do
      FEDIDataObjects.Insert(InsertIndex, InternalCreateEDIDataObject);
    {$ELSE}
    // Resize
    SetLength(FEDIDataObjects, Length(FEDIDataObjects) + Count);
    // Shift (Keep comment here for reference.)
    //for I := High(FEDIDataObjects) downto InsertIndex + Count do
    //begin
    //  FEDIDataObjects[I] := FEDIDataObjects[I-Count];
    //  FEDIDataObjects[I-Count] := nil;
    //end;
    Move(FEDIDataObjects[InsertIndex], FEDIDataObjects[InsertIndex+Count],
      SizeOf(FEDIDataObjects[0]) * (Length(FEDIDataObjects)-(InsertIndex+Count)));
    // Insert
    for I := InsertIndex to (InsertIndex + Count) - 1 do
      FEDIDataObjects[I] := InternalCreateEDIDataObject;
    {$ENDIF}
  end
  else
    Result := AddEDIDataObjects(Count);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIDataObjectGroup.SetEDIDataObject(Index: Integer; EDIDataObject: TEDIDataObject);
begin
  {$IFDEF OPTIMIZED_INTERNAL_STRUCTURE}
  if FEDIDataObjects.Count > 0 then
    if Index >= 0 then
      if Index <= FEDIDataObjects.Count - 1 then
      begin
        FEDIDataObjects.Item[Index].FreeAndNilEDIDataObject;
        FEDIDataObjects[Index] := EDIDataObject;
        if FGroupIsParent then
          FEDIDataObjects[Index].Parent := Self;        
      end
      else
        raise EJclEDIError.CreateResRecFmt(@RsEDIError009, [Self.ClassName, IntToStr(Index)])
    else
      raise EJclEDIError.CreateResRecFmt(@RsEDIError008, [Self.ClassName, IntToStr(Index)])
  else
    raise EJclEDIError.CreateResRecFmt(@RsEDIError007, [Self.ClassName, IntToStr(Index)]);
  {$ELSE}
  if Length(FEDIDataObjects) > 0 then
    if Index >= Low(FEDIDataObjects) then
      if Index <= High(FEDIDataObjects) then
      begin
        FreeAndNil(FEDIDataObjects[Index]);
        FEDIDataObjects[Index] := EDIDataObject;
        if FGroupIsParent then
          FEDIDataObjects[Index].Parent := Self;
      end
      else
        raise EJclEDIError.CreateResRecFmt(@RsEDIError009, [Self.ClassName, IntToStr(Index)])
    else
      raise EJclEDIError.CreateResRecFmt(@RsEDIError008, [Self.ClassName, IntToStr(Index)])
  else
    raise EJclEDIError.CreateResRecFmt(@RsEDIError007, [Self.ClassName, IntToStr(Index)]);
  {$ENDIF}
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectGroup.GetIndexPositionFromParent: Integer;
var
  I: Integer;
  ParentGroup: TEDIDataObjectGroup;
begin
  Result := -1;
  if Assigned(Parent) and (Parent is TEDIDataObjectGroup) then
  begin
    ParentGroup := TEDIDataObjectGroup(Parent);
    {$IFDEF OPTIMIZED_INTERNAL_STRUCTURE}
    for I := 0 to ParentGroup.EDIDataObjectCount - 1 do
    {$ELSE}
    for I := Low(ParentGroup.EDIDataObjects) to High(ParentGroup.EDIDataObjects) do
    {$ENDIF}
      if ParentGroup.EDIDataObject[I] = Self then
      begin
        Result := I;
        Break;
      end;
  end; // if
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectGroup.GetCount: Integer;
begin
  {$IFDEF OPTIMIZED_INTERNAL_STRUCTURE}
  Result := FEDIDataObjects.Count;
  {$ELSE}
  Result := Length(FEDIDataObjects);
  {$ENDIF}
end;

//==================================================================================================
// TEDIObjectListItem
//==================================================================================================

{ TEDIObjectListItem }

constructor TEDIObjectListItem.Create(Parent: TEDIObjectList;
  PriorItem: TEDIObjectListItem; EDIObject: TEDIObject = nil);
begin
  inherited Create;
  FName := '';
  FParent := Parent;
  FItemIndex := 0;
  FEDIObject := EDIObject;
  FPriorItem := PriorItem;
  FNextItem := nil;
  if FPriorItem <> nil then
    FItemIndex := FPriorItem.ItemIndex + 1;
  {$IFDEF ENABLE_EDI_DEBUGGING}
  Inc(Debug_EDIDataObjectListItemsCreated);
  {$ENDIF}
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIObjectListItem.Destroy;
begin
  {$IFDEF ENABLE_EDI_DEBUGGING}
  Inc(Debug_EDIDataObjectListItemsDestroyed);
  {$ENDIF}
  FPriorItem := nil;
  FNextItem := nil;
  if FParent.OwnsObjects then
    FreeAndNilEDIDataObject;
  FEDIObject := nil;
  FParent := nil;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIObjectListItem.FreeAndNilEDIDataObject;
begin
  FreeAndNil(FEDIObject);
end;

//--------------------------------------------------------------------------------------------------

function TEDIObjectListItem.GetIndexPositionFromParent: Integer;
begin
  Result := FParent.IndexOf(Self);
end;

//==================================================================================================
// TEDIObjectList
//==================================================================================================

{ TEDIObjectList }

constructor TEDIObjectList.Create(OwnsObjects: Boolean = True);
begin
  inherited Create;
  FOwnsObjects := OwnsObjects;
  FFirstItem := nil;
  FLastItem := nil;
  FCurrentItem := nil;
  FCount := 0;
  FOptions := [loAutoUpdateIndexes];
  {$IFDEF ENABLE_EDI_DEBUGGING}
  Inc(Debug_EDIDataObjectListCreated);
  {$ENDIF}
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIObjectList.Destroy;
begin
  {$IFDEF ENABLE_EDI_DEBUGGING}
  Inc(Debug_EDIDataObjectListDestroyed);
  {$ENDIF}
  Clear;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIObjectList.Clear;
var
  ListItem: TEDIObjectListItem;
  TempItem: TEDIObjectListItem;
begin
  ListItem := FFirstItem;
  while ListItem <> nil do
  begin
    TempItem := ListItem;
    ListItem := ListItem.NextItem;
    TempItem.Free;
  end;
  FFirstItem := nil;
  FLastItem := nil;
  FCurrentItem := nil;
  FCount := 0;
end;

//--------------------------------------------------------------------------------------------------

function TEDIObjectList.First(Index: Integer): TEDIObjectListItem;
begin
  if Index = 0 then
    Result := FFirstItem
  else
    Result := GetItem(Index);
  FCurrentItem := Result;
end;

//--------------------------------------------------------------------------------------------------

function TEDIObjectList.Last: TEDIObjectListItem;
begin
  FCurrentItem := FLastItem;
  Result := FCurrentItem;
end;

//--------------------------------------------------------------------------------------------------

function TEDIObjectList.Next: TEDIObjectListItem;
begin
  FCurrentItem := FCurrentItem.NextItem;
  Result := FCurrentItem;
end;

//--------------------------------------------------------------------------------------------------

function TEDIObjectList.Prior: TEDIObjectListItem;
begin
  FCurrentItem := FCurrentItem.PriorItem;
  Result := FCurrentItem;
end;

//--------------------------------------------------------------------------------------------------

function TEDIObjectList.Add(EDIObject: TEDIObject; Name: string): TEDIObjectListItem;
begin
  Result := CreateListItem(FLastItem, EDIObject);
  Result.Name := Name;
  if FLastItem <> nil then
    FLastItem.NextItem := Result;
  if FFirstItem = nil then
    FFirstItem := Result;
  FLastItem := Result;
  FCurrentItem := Result;
  Inc(FCount);
end;

//--------------------------------------------------------------------------------------------------

function TEDIObjectList.FindItemByName(Name: string;
  StartItem: TEDIObjectListItem): TEDIObjectListItem;
var
  ListItem: TEDIObjectListItem;
begin
  Result := nil;
  if StartItem <> nil then
    ListItem := StartItem
  else
    ListItem := First;
  while ListItem <> nil do
  begin
    if ListItem.Name = Name then
    begin
      Result := ListItem;
      Break;
    end;
    ListItem := Next;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIObjectList.Insert(InsertIndex: Integer; EDIObject: TEDIObject);
var
  ListItem: TEDIObjectListItem;
begin
  FCurrentItem := GetItem(InsertIndex);
  if FCurrentItem <> nil then
  begin
    //Link new item
    ListItem := CreateListItem(FCurrentItem.PriorItem);
    ListItem.NextItem := FCurrentItem;
    ListItem.EDIObject := EDIObject;
    //Relink current item
    if FCurrentItem.PriorItem <> nil then
      FCurrentItem.PriorItem.NextItem := ListItem
    else
      FFirstItem := ListItem;
    FCurrentItem.PriorItem := ListItem;
    //
    FCurrentItem := ListItem;
    Inc(FCount);
    // Update the indexes starting at the current item.
    if loAutoUpdateIndexes in FOptions then
      UpdateIndexes(FCurrentItem); //Pass nil to force update of all items
  end
  else
    Add(EDIObject);
end;

//--------------------------------------------------------------------------------------------------

function TEDIObjectList.GetItem(Index: Integer): TEDIObjectListItem;
var
  I: Integer;
  ListItem: TEDIObjectListItem;
begin
  Result := nil;
  if FCurrentItem <> nil then // Attempt to search from the current item.
  begin
    if Index = FCurrentItem.ItemIndex then // The index already points to the current item.
      Result := FCurrentItem
    else
    if Index > FCurrentItem.ItemIndex then // Search forward in the list.
    begin
      I := FCurrentItem.ItemIndex - 1;
      ListItem := FCurrentItem;
      while ListItem <> nil do
      begin
        Inc(I);
        if I = Index then
        begin
          Result := ListItem;
          Break;
        end;
        ListItem := ListItem.NextItem;
      end;
      FCurrentItem := Result;
    end
    else // if Index < FCurrentItem.ItemIndex then // Search backward in the list.
    begin
      I := FCurrentItem.ItemIndex + 1;
      ListItem := FCurrentItem;
      while ListItem <> nil do
      begin
        Dec(I);
        if I = Index then
        begin
          Result := ListItem;
          Break;
        end;
        ListItem := ListItem.PriorItem;
      end;
      FCurrentItem := Result;
    end;
  end
  else // No current item was assigned so search from the beginning of the structure.
  begin
    I := -1;
    FCurrentItem := FFirstItem;
    ListItem := FFirstItem;
    while ListItem <> nil do
    begin
      Inc(I);
      if I = Index then
      begin
        Result := ListItem;
        Break;
      end;
      ListItem := ListItem.NextItem;
    end;
    FCurrentItem := Result;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIObjectList.Delete(Index: Integer);
var
  ListItem: TEDIObjectListItem;
begin
  ListItem := GetItem(Index);
  if ListItem <> nil then
  begin
    Remove(ListItem);
    // Update the indexes starting at the current item.
    if loAutoUpdateIndexes in FOptions then
      UpdateIndexes(FCurrentItem.PriorItem); //Pass nil to force update of all items
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIObjectList.Delete(EDIObject: TEDIObject);
begin
  Remove(EDIObject);
  // Update the indexes starting at the current item.
  if loAutoUpdateIndexes in FOptions then
    UpdateIndexes(nil); //Pass nil to force update of all items
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIObjectList.UpdateIndexes(StartItem: TEDIObjectListItem = nil);
var
  I: Integer;
  ListItem: TEDIObjectListItem;
begin
  if StartItem <> nil then
  begin
    ListItem := StartItem;
    I := StartItem.ItemIndex - 1;
  end
  else
  begin
    ListItem := FFirstItem;
    I := -1;
  end;
  while ListItem <> nil do
  begin
    Inc(I);
    ListItem.ItemIndex := I;
    ListItem := ListItem.NextItem;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIObjectList.UpdateCount;
var
  ListItem: TEDIObjectListItem;
begin
  FCount := 0;
  ListItem := FFirstItem;
  while ListItem <> nil do
  begin
    ListItem := ListItem.NextItem;
    Inc(FCount);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIObjectList.Remove(EDIObject: TEDIObject);
var
  ListItem: TEDIObjectListItem;
begin
  ListItem := Find(EDIObject);
  if ListItem <> nil then
  begin
    // Remove the item from the list
    ListItem := Extract(ListItem);
    // Free the list item
    FreeAndNil(ListItem);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIObjectList.Extract(EDIObject: TEDIObject): TEDIObject;
var
  ListItem: TEDIObjectListItem;
begin
  Result := nil;
  ListItem := Find(EDIObject);
  if ListItem <> nil then
  begin
    // Extract the EDI Data Object
    Result := ListItem.EDIObject;
    ListItem.EDIObject := nil;
    // Remove the item from the list
    ListItem := Extract(ListItem);
    // Free the list item
    FreeAndNil(ListItem);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIObjectList.IndexOf(EDIObject: TEDIObject): Integer;
var
  I: Integer;
  ListItem: TEDIObjectListItem;
begin
  Result := -1;
  I := 0;
  ListItem := FFirstItem;
  while ListItem <> nil do
  begin
    if ListItem.EDIObject = EDIObject then
    begin
      FCurrentItem := ListItem;
      FCurrentItem.ItemIndex := I;
      Result := I;
      Break;
    end;
    ListItem := ListItem.NextItem;
    Inc(I);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIObjectList.GetEDIObject(Index: Integer): TEDIObject;
var
  ListItem: TEDIObjectListItem;
begin
  Result := nil;
  ListItem := GetItem(Index);
  if ListItem <> nil then
    Result := ListItem.EDIObject;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIObjectList.SetEDIObject(Index: Integer; const Value: TEDIObject);
var
  ListItem: TEDIObjectListItem;
begin
  ListItem := GetItem(Index);
  if ListItem <> nil then
    ListItem.EDIObject := Value;
end;

//--------------------------------------------------------------------------------------------------

function TEDIObjectList.ReturnListItemsByName(Name: string): TEDIObjectList;
var
  ListItem: TEDIObjectListItem;
begin
  Result := TEDIObjectList.Create(False);
  ListItem := First;
  while ListItem <> nil do
  begin
    if ListItem.Name = Name then
      Result.Add(ListItem.EDIObject, ListItem.Name);
    ListItem := Next;
  end; //while
end;

//--------------------------------------------------------------------------------------------------

function TEDIObjectList.IndexOf(Item: TEDIObjectListItem): Integer;
var
  I: Integer;
  ListItem: TEDIObjectListItem;
begin
  Result := -1;
  I := 0;
  ListItem := FFirstItem;
  while ListItem <> nil do
  begin
    if ListItem = Item then
    begin
      FCurrentItem := ListItem;
      FCurrentItem.ItemIndex := I;
      Result := I;
      Break;
    end;
    ListItem := ListItem.NextItem;
    Inc(I);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIObjectList.Remove(Item: TEDIObjectListItem);
begin
  // Remove the item from the list
  Item := Extract(Item);
  // Free the list item
  FreeAndNil(Item);
end;

//--------------------------------------------------------------------------------------------------

function TEDIObjectList.Extract(Item: TEDIObjectListItem): TEDIObjectListItem;
begin
  Result := Item;
  // Set current item
  if Item.NextItem <> nil then
    FCurrentItem := Item.NextItem
  else
    FCurrentItem := Item.PriorItem;
  // Extract the item and relink existing items.
  if Item.NextItem <> nil then
    Item.NextItem.PriorItem := Item.PriorItem;
  if Item.PriorItem <> nil then
    Item.PriorItem.NextItem := Item.NextItem;
  if Item = FFirstItem then
    FFirstItem := Item.NextItem;
  if Item = FLastItem then
    FLastItem := Item.PriorItem;
  // Update the count
  Dec(FCount);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIObjectList.Add(Item: TEDIObjectListItem; Name: string);
begin
  Item.Parent := Self;
  Item.Name := Name;
  Item.NextItem := nil;
  Item.PriorItem := nil;
  if FLastItem <> nil then
  begin
    Item.PriorItem := FLastItem;
    FLastItem.NextItem := Item;
  end;
  if FFirstItem = nil then
    FFirstItem := Item;
  FLastItem := Item;
  FCurrentItem := Item;
  Inc(FCount);
end;

//--------------------------------------------------------------------------------------------------

function TEDIObjectList.FindEDIObject(EDIObject: TEDIObject): TEDIObject;
var
  ListItem: TEDIObjectListItem;
begin
  Result := nil;
  ListItem := FFirstItem;
  while ListItem <> nil do
  begin
    if ListItem.EDIObject = EDIObject then
    begin
      FCurrentItem := ListItem;
      Result := ListItem.EDIObject;
      Break;
    end;
    ListItem := ListItem.NextItem;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIObjectList.Find(Item: TEDIObjectListItem): TEDIObjectListItem;
var
  ListItem: TEDIObjectListItem;
begin
  Result := nil;
  ListItem := FFirstItem;
  while ListItem <> nil do
  begin
    if ListItem = Item then
    begin
      FCurrentItem := ListItem;
      Result := ListItem;
      Break;
    end;
    ListItem := ListItem.NextItem;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIObjectList.Find(EDIObject: TEDIObject): TEDIObjectListItem;
var
  ListItem: TEDIObjectListItem;
begin
  Result := nil;
  ListItem := FFirstItem;
  while ListItem <> nil do
  begin
    if ListItem.EDIObject = EDIObject then
    begin
      FCurrentItem := ListItem;
      Result := ListItem;
      Break;
    end;
    ListItem := ListItem.NextItem;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDIObjectList.IndexIsValid(Index: Integer): Boolean;
begin
  Result := False;
  if (FCount > 0) and (Index >= 0) and (Index <= FCount - 1) then
    Result := True;
end;

//--------------------------------------------------------------------------------------------------

function TEDIObjectList.Insert(Item, BeforeItem: TEDIObjectListItem): TEDIObjectListItem;
begin
  Result := Item;
  if Result = nil then
    Result := CreateListItem(BeforeItem, nil);
  Result.Parent := Self;
  Result.PriorItem := nil;
  Result.NextItem := nil;
  if BeforeItem <> nil then // Insert item
  begin
    Result.PriorItem := BeforeItem.PriorItem;
    BeforeItem.PriorItem := Result;
    if Result.PriorItem <> nil then
      Result.PriorItem.NextItem := Result;
    Result.NextItem := BeforeItem;
  end
  else if FFirstItem <> nil then  // Insert as first item
  begin
    FFirstItem.PriorItem := Result;
    Result.NextItem := FFirstItem;
    FFirstItem := Result;
  end
  else
    Add(Result); // Add as first item
  FCurrentItem := Result;
  Inc(FCount);
end;

//--------------------------------------------------------------------------------------------------

function TEDIObjectList.Insert(EDIObject, BeforeEDIObject: TEDIObject): TEDIObjectListItem;
var
  BeforeItem: TEDIObjectListItem;
begin
  BeforeItem := Find(BeforeEDIObject);
  Result := CreateListItem(BeforeItem, EDIObject);
  Insert(Result, BeforeItem);
end;

//--------------------------------------------------------------------------------------------------

function TEDIObjectList.Insert(BeforeItem: TEDIObjectListItem): TEDIObjectListItem;
begin
  Result := CreateListItem(BeforeItem, nil);
  Insert(Result, BeforeItem);
end;

//--------------------------------------------------------------------------------------------------

function TEDIObjectList.Insert(BeforeEDIObject: TEDIObject): TEDIObjectListItem;
begin
  Result := Insert(nil, BeforeEDIObject);
end;

//==================================================================================================
// TEDIDataObjectListItem
//==================================================================================================

{ TEDIDataObjectListItem }

function TEDIDataObjectListItem.GetEDIDataObject: TEDIDataObject;
begin
  Result := TEDIDataObject(FEDIObject);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIDataObjectListItem.SetEDIDataObject(const Value: TEDIDataObject);
begin
  FEDIObject := Value;
end;

//==================================================================================================
// TEDIDataObjectList
//==================================================================================================

{ TEDIDataObjectList }

function TEDIDataObjectList.CreateListItem(PriorItem: TEDIObjectListItem;
  EDIObject: TEDIObject): TEDIObjectListItem;
begin
  Result := TEDIDataObjectListItem.Create(Self, PriorItem, EDIObject);
end;

//--------------------------------------------------------------------------------------------------

function TEDIDataObjectList.GetEDIDataObject(Index: Integer): TEDIDataObject;
begin
  Result := TEDIDataObject(GetEDIObject(Index));
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIDataObjectList.SetEDIDataObject(Index: Integer; const Value: TEDIDataObject);
begin
  SetEDIObject(Index, Value);
end;

//--------------------------------------------------------------------------------------------------

function TEDIObjectList.CreateListItem(PriorItem: TEDIObjectListItem;
  EDIObject: TEDIObject = nil): TEDIObjectListItem;
begin
  Result := TEDIObjectListItem.Create(Self, PriorItem, EDIObject);
end;

//==================================================================================================
// TEDILoopStack
//==================================================================================================

{ TEDILoopStack }

constructor TEDILoopStack.Create;
begin
  inherited Create;
  SetLength(FStack, 0);
  FFlags := [];
end;

//--------------------------------------------------------------------------------------------------

destructor TEDILoopStack.Destroy;
var
  I: Integer;
begin
  for I := Low(FStack) to High(FStack) do
    FStack[I].EDIObject := nil;
  SetLength(FStack, 0);
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDILoopStack.Debug: string;
var
  I: Integer;
begin
  Result := 'Loop Stack' + #13#10;
  for I := 0 to High(FStack) do
    Result := Result + FStack[I].SegmentId + ', ' +
      FStack[I].OwnerLoopId + ', ' +
      FStack[I].ParentLoopId + ', ' +
      IntToStr(FStack[I].SpecStartIndex) + #13#10;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDILoopStack.DoAddLoop(StackRecord: TEDILoopStackRecord;
  SegmentId, OwnerLoopId, ParentLoopId: string; var EDIObject: TEDIObject);
begin
  if Assigned(FOnAddLoop) then
    FOnAddLoop(StackRecord, SegmentId, OwnerLoopId, ParentLoopId, EDIObject);
end;

//--------------------------------------------------------------------------------------------------

function TEDILoopStack.GetSafeStackIndex(Index: Integer): Integer;
begin
  if Length(FStack) > 0 then
  begin
    if Index >= Low(FStack) then
    begin
      if Index <= High(FStack) then
        Result := Index
      else
        Result := High(FStack);
    end
    else
      Result := Low(FStack);
  end
  else
    raise EJclEDIError.CreateResRecFmt(@RsEDIError057, [IntToStr(Index)]);
end;

//--------------------------------------------------------------------------------------------------

function TEDILoopStack.GetSize: Integer;
begin
  Result := Length(FStack);
end;

//--------------------------------------------------------------------------------------------------

function TEDILoopStack.Peek: TEDILoopStackRecord;
begin
  Result := FStack[High(FStack)];
end;

//--------------------------------------------------------------------------------------------------

function TEDILoopStack.Peek(Index: Integer): TEDILoopStackRecord;
begin
  if Length(FStack) > 0 then
    if Index >= Low(FStack) then
      if Index <= High(FStack) then
        Result := FStack[Index]
      else
        raise EJclEDIError.CreateResRecFmt(@RsEDIError054, [IntToStr(Index)])
    else
      raise EJclEDIError.CreateResRecFmt(@RsEDIError055, [IntToStr(Index)])
  else
    raise EJclEDIError.CreateResRecFmt(@RsEDIError056, [IntToStr(Index)]);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDILoopStack.Pop(Index: Integer);
begin
  // Resize loop stack if the index is less than the length
  if (Index >= 0) and (Index < Length(FStack)) then
  begin
    SetLength(FStack, Index);
    FFlags := FFlags + [ediStackResized];
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDILoopStack.Push(SegmentId, OwnerLoopId, ParentLoopId: string; StartIndex: Integer;
  EDIObject: TEDIObject): Integer;
begin
  // Add to loop stack
  SetLength(FStack, Length(FStack) + 1);
  UpdateStackData(SegmentId, OwnerLoopId, ParentLoopId, StartIndex, EDIObject);
  Result := High(FStack);
end;

//--------------------------------------------------------------------------------------------------

function TEDILoopStack.SetStackPointer(OwnerLoopId,
  ParentLoopId: string): Integer;
var
  I: Integer;
begin
  FFlags := FFlags - [ediStackResized];
  FFlags := FFlags - [ediAltStackPointer];
  Result := -1; // Entry not found
  // Find the loop in the stack
  for I := High(FStack) downto 0 do
  begin
    if (OwnerLoopId = FStack[I].OwnerLoopId) and
      (ParentLoopId = FStack[I].ParentLoopId) then
    begin
      Result := I;
      // Pop entries from the stack starting at the index after the found loop
      Pop(I + 1);
      Break;
    end;
  end;
  // Check if an exact entry was found
  if Result = -1 then
  begin
    // Find the parent loop in the stack
    for I := High(FStack) downto 0 do
    begin
      if (ParentLoopId = FStack[I].ParentLoopId) and
        (FStack[I].OwnerLoopId <> NA_LoopId) then
      begin
        FFlags := FFlags + [ediAltStackPointer];
        Result := GetSafeStackIndex(I);
        // Pop entries from the stack starting at the index after the found loop
        Pop(I + 1);
        Break;
      end;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDILoopStack.UpdateStackData(SegmentId, OwnerLoopId, ParentLoopId: string;
  StartIndex: Integer; EDIObject: TEDIObject);
begin
  FStack[High(FStack)].SegmentId := SegmentId;
  FStack[High(FStack)].OwnerLoopId := OwnerLoopId;
  FStack[High(FStack)].ParentLoopId := ParentLoopId;
  FStack[High(FStack)].SpecStartIndex := StartIndex;
  FStack[High(FStack)].EDIObject := EDIObject;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDILoopStack.UpdateStackObject(EDIObject: TEDIObject);
begin
  FStack[High(FStack)].EDIObject := EDIObject;
end;

//--------------------------------------------------------------------------------------------------

function TEDILoopStack.ValidateLoopStack(SegmentId, OwnerLoopId, ParentLoopId: string;
  StartIndex: Integer; EDIObject: TEDIObject): TEDILoopStackRecord;
var
  I: Integer;
  StackRecord: TEDILoopStackRecord;
begin
  if Length(FStack) <= 0 then
    // Add entry to stack
    Push(SegmentId, OwnerLoopId, ParentLoopId, StartIndex, EDIObject)
  else
  begin
    I := SetStackPointer(OwnerLoopId, ParentLoopId);
    if I >= 0 then // Entry found
    begin
      if ediLoopRepeated in FFlags then
      begin
        // Get the previous stack record so the repeated loop will not be nested
        StackRecord := Peek(I-1);
        // In event handler add loop to external data structure since it repeated
        // See JclEDI_ANSIX12.TEDITransactionSetDocument class for implementation example.
        DoAddLoop(StackRecord, SegmentId, OwnerLoopId, ParentLoopId, EDIObject);
        // Update stack object only
        UpdateStackObject(EDIObject);
        // Debug
        // ShowMessage('LoopRepeated');
      end
      else
      if ediAltStackPointer in FFlags then
      begin
        // Get the previous stack record because the loop
        // is not to be nested at the current stack pointer
        StackRecord := Peek(I-1);
        // In event handler add loop to external data structure since it is new
        // See JclEDI_ANSIX12.TEDITransactionSetDocument class for implementation example.
        DoAddLoop(StackRecord, SegmentId, OwnerLoopId, ParentLoopId, EDIObject);
        // Update stack entry
        UpdateStackData(SegmentId, OwnerLoopId, ParentLoopId, StartIndex, EDIObject);
        // Debug
        // ShowMessage('AltStackPointer');
      end
      else
      if ediStackResized in FFlags then
      begin
        // Debug
        // ShowMessage('Stack Size Decreased');
      end
      else
      begin
        // Segment is part of loop
      end;
    end
    else
    if I = -1 then // Entry not found.
    begin
      // In event handler add loop since it is new
      StackRecord := Peek;
      // In event handler add loop to external data structure since it is new
      DoAddLoop(StackRecord, SegmentId, OwnerLoopId, ParentLoopId, EDIObject);
      // Add entry to stack
      Push(SegmentId, OwnerLoopId, ParentLoopId, StartIndex, EDIObject);
      // Debug
      // ShowMessage('Stack Size Increased');
    end;
  end;
  Result := Peek;
end;

end.
