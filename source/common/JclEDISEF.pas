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
{ The Original Code is JclEDISEF.pas.                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ EDI Standard Exchange Format (*.sef) File Parser Unit (In Development Version)                   }
{                                                                                                  }
{ This unit is still in development and is currently experimental                                  }
{                                                                                                  }
{ Unit owner: Raymond Alexander                                                                    }
{ Date created: July, 20, 2003                                                                     }
{ Last modified: July 28, 2003                                                                     }
{ Additional Info:                                                                                 }
{   E-Mail at RaysDelphiBox3@hotmail.com                                                           }
{   For latest EDI specific updates see http://sourceforge.net/projects/edisdk                     }
{   See home page for latest news & events and online help.                                        }
{                                                                                                  }
{**************************************************************************************************}

unit JclEDISEF;

{$I jcl.inc}

{$WEAKPACKAGEUNIT ON}

interface

uses
  SysUtils, Classes, Contnrs,
  JclEDI;

const
  SectionTag_VER = '.VER';
  SectionTag_INI = '.INI';
  SectionTag_PRIVATE = '.PRIVATE';
  SectionTag_PUBLIC = '.PUBLIC';
  SectionTag_STD = '.STD';
  SectionTag_SETS = '.SETS';
  SectionTag_SEGS = '.SEGS';
  SectionTag_COMS = '.COMS';
  SectionTag_ELMS = '.ELMS';
  SectionTag_CODES = '.CODES';
  SectionTag_VALLISTS = '';
  SectionTag_OBJVARS = '';
  SectionTag_SEMREFS = '';
  SectionTag_TEXT = '';
  SectionTag_ = '';


  EDISEFComsUserAttributePeriod = '.';
  EDISEFComsUserAttributeExclamationPoint = '!';
  EDISEFComsUserAttributeDollarSign = '$';
  EDISEFComsUserAttributeHyphen = '-';
  EDISEFComsUserAttributeAmpersand = '&';

  EDISEFComsUserAttributes =
   [EDISEFComsUserAttributePeriod,
    EDISEFComsUserAttributeExclamationPoint,
    EDISEFComsUserAttributeDollarSign,
    EDISEFComsUserAttributeHyphen,
    EDISEFComsUserAttributeAmpersand];

type

  TEDISEFComsUserAttributes =
    (caPeriod, caExclamationPoint, caDollarSign, caHyphen, caAmpersand);

  TEDISEFDataObject = class;
  TEDISEFDataObjectGroup = class;
  TEDISEFElement = class;
  TEDISEFCompositeElement = class;
  TEDISEFSegment = class;
  TEDISEFLoop = class;
  TEDISEFSection = class;
  TEDISEFSet = class;
  TEDISEFFile = class;

  TEDISEFDataObjectListItem = class;
  TEDISEFDataObjectList = class;

  TEDISEFObjectParentType =
    (sefNil, sefList, sefElement, sefCompositeElement, sefSegment);

//--------------------------------------------------------------------------------------------------
//  EDI SEF Data Object
//--------------------------------------------------------------------------------------------------

  TEDISEFDataObject = class(TEDIObject)
  protected
    FState: TEDIDataObjectDataState;
    FId: string;
    FData: string;
    FLength: Integer;
    FParent: TEDISEFDataObject;
    FErrorLog: TStrings;
    function GetData: string;
    procedure SetData(const Data: string);
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; virtual; abstract;
    procedure Disassemble; virtual; abstract;
//    function Clone: TEDISEFDataObject; virtual; abstract;
  published
    property State: TEDIDataObjectDataState read FState;
    property Id: string read FId write FId;    
    property Data: string read GetData write SetData;
    property DataLength: Integer read FLength;
    property Parent: TEDISEFDataObject read FParent write FParent;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI SEF Data Object List Item
//--------------------------------------------------------------------------------------------------

  TEDISEFDataObjectListItemOptions = set of (lhFreeDataObject);

  TEDISEFDataObjectListItem = class(TEDIObject)
  protected
    FParent: TEDISEFDataObjectList;
    FOptions: TEDISEFDataObjectListItemOptions;
    FName: string;
    FPriorItem: TEDISEFDataObjectListItem;
    FNextItem: TEDISEFDataObjectListItem;
    FEDISEFDataObject: TEDISEFDataObject;
  public
    constructor Create(Parent: TEDISEFDataObjectList; PriorItem: TEDISEFDataObjectListItem);
    destructor Destroy; override;
    function GetIndexPositionFromParent: Integer;
  published
    property Name: string read FName write FName;
    property PriorItem: TEDISEFDataObjectListItem read FPriorItem write FPriorItem;
    property NextItem: TEDISEFDataObjectListItem read FNextItem write FNextItem;
    property EDISEFDataObject: TEDISEFDataObject read FEDISEFDataObject write FEDISEFDataObject;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI SEF Data Object List
//--------------------------------------------------------------------------------------------------

  TEDISEFDataObjectList = class(TEDIObject)
  private
    FFirstItem: TEDISEFDataObjectListItem;
    FLastItem: TEDISEFDataObjectListItem;
    FCurrentItem: TEDISEFDataObjectListItem;
    function GetCount: Integer;
    function GetEDISEFDataObject(Index: Integer): TEDISEFDataObjectListItem;
    procedure SetEDISEFDataObject(Index: Integer; const Value: TEDISEFDataObjectListItem);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AppendEDISEFDataObject(EDISEFDataObject: TEDISEFDataObject; Name: string = '');
    procedure DeleteEDISEFDataObjects(FreeReferences: Boolean = False);
    function First(Index: Integer = 0): TEDISEFDataObjectListItem;
    function Next: TEDISEFDataObjectListItem;
    function Prior: TEDISEFDataObjectListItem;
    function Last: TEDISEFDataObjectListItem;
    function FindItemByName(Name: string): TEDISEFDataObjectListItem;
    property EDISEFDataObject[Index: Integer]: TEDISEFDataObjectListItem read GetEDISEFDataObject
      write SetEDISEFDataObject; default;
  published
    property Count: Integer read GetCount;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI SEF Data Object Group
//--------------------------------------------------------------------------------------------------

  TEDISEFDataObjectGroup = class(TEDISEFDataObject)
  protected
    FEDISEFDataObjects: TEDISEFDataObjectList;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
//    function Clone: TEDISEFDataObject; override;
    property EDISEFDataObjects: TEDISEFDataObjectList read FEDISEFDataObjects;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Data Object Group
//--------------------------------------------------------------------------------------------------

  TEDISEFElement = class(TEDISEFDataObject)
  protected
    FUserAttribute: string;
    FOrdinal: Integer;
    FElementType: string;
    FMinimumLength: Integer;
    FMaximumLength: Integer;
    FRequirementDesignator: string;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    function Clone: TEDISEFElement; reintroduce;
    procedure AssignDefaultElementValues(EDISEFElement: TEDISEFElement;
      OverwriteExistingValues: Boolean = True);
  published
    property UserAttribute: string read FUserAttribute write FUserAttribute;
    property Ordinal: Integer read FOrdinal write FOrdinal;
    property ElementId: string read FId write FId;
    property ElementType: string read FElementType write FElementType;
    property MinimumLength: Integer read FMinimumLength write FMinimumLength;
    property MaximumLength: Integer read FMaximumLength write FMaximumLength;
    property RequirementDesignator: string read FRequirementDesignator write FRequirementDesignator;
  end;

  TEDISEFSubElement = class(TEDISEFElement)
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    function Clone: TEDISEFSubElement; reintroduce;
  end;

  TEDISEFCompositeElement = class(TEDISEFDataObjectGroup)
  private
    FUserAttribute: string;
    FOrdinal: Integer;
    FRequirementDesignator: string;
    procedure InternalDisassemble(ElementData: string; ELMSList: TEDISEFDataObjectList);
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble(ELMSList, COMSList: TEDISEFDataObjectList); reintroduce;
//    function Clone: TEDISEFCompositeElement; reintroduce;
    procedure Assign(CompositeElement: TEDISEFCompositeElement);
    property Elements: TEDISEFDataObjectList read FEDISEFDataObjects;
  published
    property UserAttribute: string read FUserAttribute write FUserAttribute;
    property CompositeElementId: string read FId write FId;
    property Ordinal: Integer read FOrdinal write FOrdinal;
  end;

  TEDISEFSegment = class(TEDISEFDataObjectGroup)
  private
    FRequirement: string;
    FMaximumUse: Integer;
    procedure InternalDisassemble(Data: string; ELMSList, COMSList: TEDISEFDataObjectList);
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble(ELMSList, COMSList: TEDISEFDataObjectList); reintroduce;
    property Elements: TEDISEFDataObjectList read FEDISEFDataObjects;
  published
    property SegmentId: string read FId write FId;
    property Requirement: string read FRequirement write FRequirement;
    property MaximumUse: Integer read FMaximumUse write FMaximumUse;
  end;

  TEDISEFLoop = class(TEDISEFDataObjectGroup)
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
  end;

  TEDISEFSection = class(TEDISEFDataObjectGroup)
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
  end;

  TEDISEFSet = class(TEDISEFLoop)
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble(SETSList: TEDISEFDataObjectList); reintroduce;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Data Object Group
//--------------------------------------------------------------------------------------------------

  TEDISEFFile = class(TEDISEFDataObject)
  private
    FFileName: string;
    FEDISEFCodesList: TStrings;
    FEDISEFElms: TEDISEFDataObjectList;
    FEDISEFComs: TEDISEFDataObjectList;
    FEDISEFSegs: TEDISEFDataObjectList;
    FEDISEFSets: TEDISEFDataObjectList;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    procedure LoadFromFile; overload;
    procedure LoadFromFile(const FileName: string); overload;
    procedure SaveToFile; overload;
    procedure SaveToFile(const FileName: string); overload;
    function Assemble: string; override;
    procedure Disassemble; override;
  published
    property FileName: string read FFileName write FFileName;
    property Codes: TStrings read FEDISEFCodesList;
    property ELMS: TEDISEFDataObjectList read FEDISEFElms;
    property COMS: TEDISEFDataObjectList read FEDISEFComs;
    property SEGS: TEDISEFDataObjectList read FEDISEFSegs;
    property SETS: TEDISEFDataObjectList read FEDISEFSets;
  end;

//--------------------------------------------------------------------------------------------------
//  Procedures
//--------------------------------------------------------------------------------------------------

//  function CreateSETSDataBlock(SourceData, var Start): TEDISEFDataObject
//    Section
//    Loop
//    Segment

//  function GetSEGSDataBlock(SourceData, var Start, out Data)

implementation

uses
  JclBase, JclResources, JclStrings;

//==================================================================================================
// TEDISEFDataObject
//==================================================================================================

constructor TEDISEFDataObject.Create(Parent: TEDISEFDataObject);
begin
  inherited Create;
  FId := '';
  FData := '';
  FLength := 0;
  // (rom) this is ridiculous! No if needed!
  if Assigned(Parent) then
    FParent := Parent
  else
    FParent := nil;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISEFDataObject.Destroy;
begin
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFDataObject.GetData: string;
begin
  Result := FData;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFDataObject.SetData(const Data: string);
begin
  FData := Data;
  FLength := Length(FData);
end;

//==================================================================================================
// TEDISEFDataObjectListItem
//==================================================================================================

constructor TEDISEFDataObjectListItem.Create(Parent: TEDISEFDataObjectList;
  PriorItem: TEDISEFDataObjectListItem);
begin
  inherited Create;
  FParent := Parent;
  FOptions := [lhFreeDataObject];
  FName := '';
  FEDISEFDataObject := nil;
  FPriorItem := PriorItem;
  FNextItem := nil;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISEFDataObjectListItem.Destroy;
begin
  FPriorItem := nil;
  FNextItem := nil;
  if (lhFreeDataObject in FOptions) and (FEDISEFDataObject <> nil) then
    FEDISEFDataObject.Free;
  FEDISEFDataObject := nil;
  FParent := nil;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFDataObjectListItem.GetIndexPositionFromParent: Integer;
var
  I: Integer;
  ListItem: TEDISEFDataObjectListItem;
begin
  Result := -1;
  for I := 0 to FParent.Count - 1 do
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
  end;
end;

//==================================================================================================
// TEDISEFDataObjectList
//==================================================================================================

constructor TEDISEFDataObjectList.Create;
begin
  inherited Create;
  FFirstItem := nil;
  FLastItem := nil;
  FCurrentItem := nil;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISEFDataObjectList.Destroy;
begin
  DeleteEDISEFDataObjects(True);
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFDataObjectList.AppendEDISEFDataObject(EDISEFDataObject: TEDISEFDataObject;
  Name: string = '');
var
  ListItem: TEDISEFDataObjectListItem;
begin
  ListItem := TEDISEFDataObjectListItem.Create(Self, FLastItem);
  ListItem.EDISEFDataObject := EDISEFDataObject;
  ListItem.Name := Name;
  if FLastItem <> nil then
    FLastItem.NextItem := ListItem;
  if FFirstItem = nil then
    FFirstItem := ListItem;
  FLastItem := ListItem;
  FCurrentItem := ListItem;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFDataObjectList.DeleteEDISEFDataObjects(FreeReferences: Boolean);
var
  ListItem: TEDISEFDataObjectListItem;
  PriorItem: TEDISEFDataObjectListItem;
begin
  ListItem := FFirstItem;
  while ListItem <> nil do
  begin
    if FreeReferences and (ListItem.EDISEFDataObject <> nil) then
      ListItem.EDISEFDataObject.Free;
    ListItem.EDISEFDataObject := nil;
    PriorItem := ListItem;
    ListItem := ListItem.NextItem;
    PriorItem.Free;
  end;
  FFirstItem := nil;
  FLastItem := nil;
  FCurrentItem := nil;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFDataObjectList.FindItemByName(Name: string): TEDISEFDataObjectListItem;
var
  ListItem: TEDISEFDataObjectListItem;
begin
  Result := nil;
  ListItem := FFirstItem;
  while ListItem <> nil do
  begin
    if ListItem.Name = Name then
      Result := ListItem;
    ListItem := ListItem.NextItem;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFDataObjectList.First(Index: Integer): TEDISEFDataObjectListItem;
begin
  Result := FFirstItem;
  FCurrentItem := FFirstItem;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFDataObjectList.GetCount: Integer;
var
  ListItem: TEDISEFDataObjectListItem;
begin
  // (rom) better keep a FCount variable updated on add and delete of elements
  Result := 0;
  ListItem := FFirstItem;
  while ListItem <> nil do
  begin
    ListItem := ListItem.NextItem;
    Inc(Result);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFDataObjectList.GetEDISEFDataObject(Index: Integer): TEDISEFDataObjectListItem;
var
  ListItem: TEDISEFDataObjectListItem;
  I: Integer;
begin
  Result := nil;
  ListItem := FFirstItem;
  I := 0;
  while ListItem <> nil do
  begin
    if I = Index then
      Result := ListItem;
    ListItem := ListItem.NextItem;
    Inc(I);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFDataObjectList.Last: TEDISEFDataObjectListItem;
begin
  Result := FLastItem;
  FCurrentItem := FLastItem;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFDataObjectList.Next: TEDISEFDataObjectListItem;
begin
  Result := FCurrentItem.NextItem;
  FCurrentItem := FCurrentItem.NextItem;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFDataObjectList.Prior: TEDISEFDataObjectListItem;
begin
  Result := FCurrentItem.PriorItem;
  FCurrentItem := FCurrentItem.PriorItem;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFDataObjectList.SetEDISEFDataObject(Index: Integer;
  const Value: TEDISEFDataObjectListItem);
var
  ListItem: TEDISEFDataObjectListItem;
  I: Integer;
begin
  ListItem := FFirstItem;
  I := 0;
  while ListItem <> nil do
  begin
    if I = Index then
      ListItem := Value;
    ListItem := ListItem.NextItem;
    Inc(I);
  end;
end;

//--------------------------------------------------------------------------------------------------

{
function TEDISEFDataObjectGroup.Clone: TEDISEFDataObject;
begin
  Result := nil;
end;
}

//==================================================================================================
// TEDISEFDataObjectGroup
//==================================================================================================

constructor TEDISEFDataObjectGroup.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
  FEDISEFDataObjects := TEDISEFDataObjectList.Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISEFDataObjectGroup.Destroy;
begin
  FEDISEFDataObjects.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFElement.Assemble: string;
begin

end;

//==================================================================================================
// TEDISEFElement
//==================================================================================================

constructor TEDISEFElement.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
  FUserAttribute := '';
  FOrdinal := 0;
  FElementType := '';
  FMinimumLength := 0;
  FMaximumLength := 0;
  FRequirementDesignator := 'O';
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISEFElement.Destroy;
begin
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFElement.AssignDefaultElementValues(EDISEFElement: TEDISEFElement;
  OverwriteExistingValues: Boolean);
begin
  if OverwriteExistingValues then
    FElementType := EDISEFElement.ElementType;
  if OverwriteExistingValues then
    FMinimumLength := EDISEFElement.MinimumLength;
  if OverwriteExistingValues then
    FMaximumLength := EDISEFElement.MaximumLength;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFElement.Clone: TEDISEFElement;
begin
  Result := TEDISEFElement.Create(nil);
  Result.Data := FData;
  Result.UserAttribute := FUserAttribute;
  Result.ElementId := FId;
  Result.Ordinal := FOrdinal;
  Result.ElementType := FElementType;
  Result.MinimumLength := FMinimumLength;
  Result.MaximumLength := FMaximumLength;
  Result.RequirementDesignator := FRequirementDesignator;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFElement.Disassemble;
var
  Temp: TStrings;
  I, J, K, L, M, N: Integer;
begin
  // Clear any old values
  FUserAttribute := '';
  FOrdinal := 0;
  FElementType := '';
  FMinimumLength := 0;
  FMaximumLength := 0;
  FRequirementDesignator := 'O';
  //
  Temp := TStringList.Create;
  try
    if FParent is TEDISEFFile then
    begin
      Temp.Text := FData;    
      FId := Temp.Names[0];
      {$IFDEF DELPHI7_UP}
      Temp.CommaText := Temp.ValueFromIndex[0];
      {$ELSE DELPHI7_UP}
      Temp.CommaText := Temp.Values[FId];
      {$ENDIF DELPHI7_UP}
      if Temp.Count >= 1 then
        FElementType := Temp[0];
      if Temp.Count >= 2 then
        FMinimumLength := StrToInt(Temp[1]);
      if Temp.Count >= 3 then
        FMaximumLength := StrToInt(Temp[2]);
    end
    else
    if FParent is TEDISEFCompositeElement then
    begin
      // Parse User Attribute
      if FData[1] in EDISEFComsUserAttributes then
      begin
        FUserAttribute := FData[1];
        I := 2;
      end
      else
        I := 1;
      // Get delimiter locations
      J := JclStrings.StrSearch('@', FData, 1);
      K := JclStrings.StrSearch(';', FData, 1);
      L := JclStrings.StrSearch(':', FData, 1);
      M := JclStrings.StrSearch(',', FData, 1);
      // Parse Id using the closest delimiter
      N := Length(FData) + 1;
      if J <> 0 then
        if N > J then
          N := J;
      if K <> 0 then
        if N > K then
          N := K;
      if L <> 0 then
        if N > L then N := L;
      if M <> 0 then
        if N > M then
          N := M;
      FId := Copy(FData, I, N - I);
      // Parse other attributes
      if J <> 0 then
      begin
        Inc(J);
        if K <> 0 then
          if N > K then
            N := K;
        if L <> 0 then
          if N > L then
            N := L;
        if M <> 0 then
          if N > M then
            N := M;
        FOrdinal := StrToInt(Copy(FData, J, N - J));
      end;
      if K <> 0 then
      begin
        Inc(K);
        if L <> 0 then
          if N > L then
            N := L;
        if M <> 0 then
          if N > M then
            N := M;
        FMinimumLength := StrToInt(Copy(FData, K, N - K));
      end;
      if L <> 0 then
      begin
        Inc(L);
        if M <> 0 then
          if N > M then
            N := M;
        FMaximumLength := StrToInt(Copy(FData, L, N - L));
      end;
      if M <> 0 then
        FRequirementDesignator := Copy(FData, M+1, 1);
    end
    else
    if FParent is TEDISEFSegment then
    begin
      Temp.CommaText := FData;
      if Temp.Count >= 1 then
        FId := Temp[0];
      if Temp.Count >= 2 then
        FRequirementDesignator := Temp[1];
      //if Temp.Count >= 3 then FRepeatCount := StrToInt(Temp[2]); // Not supported
    end;
  finally
    Temp.Free;
  end;
end;

//==================================================================================================
// TEDISEFSubElement
//==================================================================================================

constructor TEDISEFSubElement.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISEFSubElement.Destroy;
begin
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFSubElement.Assemble: string;
begin
  Result := '';
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFSubElement.Clone: TEDISEFSubElement;
begin
  Result := TEDISEFSubElement.Create(nil);
  Result.Data := FData;
  Result.UserAttribute := FUserAttribute;
  Result.ElementId := FId;
  Result.Ordinal := FOrdinal;
  Result.ElementType := FElementType;
  Result.MinimumLength := FMinimumLength;
  Result.MaximumLength := FMaximumLength;
  Result.RequirementDesignator := FRequirementDesignator;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFSubElement.Disassemble;
begin
  inherited Disassemble;
end;
{
var
  I, J, K, L, M, N: Integer;
begin
  //Clear any old values
  FUserAttribute := '';
  FOrdinal := 0;
  FElementType := '';
  FMinimumLength := 0;
  FMaximumLength := 0;
  FRequirementDesignator := '';
  //Parse User Attribute
  if FData[1] in EDISEFComsUserAttributes then
  begin
    FUserAttribute := FData[1];
    I := 2;
  end
  else
  begin
    I := 1;
  end;
  //Get delimiter locations
  J := JclStrings.StrSearch('@', FData, 1);
  K := JclStrings.StrSearch(';', FData, 1);
  L := JclStrings.StrSearch(':', FData, 1);
  M := JclStrings.StrSearch(',', FData, 1);
  //Parse Id using the closest delimiter
  N := Length(FData) + 1;
  if J <> 0 then if N > J then N := J;
  if K <> 0 then if N > K then N := K;
  if L <> 0 then if N > L then N := L;
  if M <> 0 then if N > M then N := M;
  FId := Copy(FData, I, N - I);
  //Parse other attributes
  if J <> 0 then
  begin
    Inc(J);
    if K <> 0 then if N > K then N := K;
    if L <> 0 then if N > L then N := L;
    if M <> 0 then if N > M then N := M;
    FOrdinal := StrToInt(Copy(FData, J, N - J));
  end;
  if K <> 0 then
  begin
    Inc(K);
    if L <> 0 then if N > L then N := L;
    if M <> 0 then if N > M then N := M;
    FMinimumLength := StrToInt(Copy(FData, K, N - K));
  end;
  if L <> 0 then
  begin
    Inc(L);
    if M <> 0 then if N > M then N := M;
    FMaximumLength := StrToInt(Copy(FData, L, N - L));
  end;
  if M <> 0 then
  begin
    FRequirementDesignator := Copy(FData, M+1, 1);
  end;
end;
}

//==================================================================================================
// TEDISEFCompositeElement
//==================================================================================================

constructor TEDISEFCompositeElement.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISEFCompositeElement.Destroy;
begin
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFCompositeElement.Assemble: string;
begin
  Result := '';
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFCompositeElement.Assign(CompositeElement: TEDISEFCompositeElement);
var
  ListItem: TEDISEFDataObjectListItem;
  SubElement: TEDISEFSubElement;
begin
  FEDISEFDataObjects.DeleteEDISEFDataObjects(True);
  FUserAttribute := CompositeElement.UserAttribute;
  FId := CompositeElement.CompositeElementId;
  FOrdinal := CompositeElement.Ordinal;
  ListItem := CompositeElement.Elements.First;
  while ListItem <> nil do
  begin
    if ListItem.EDISEFDataObject <> nil then
    begin
      SubElement := TEDISEFSubElement(ListItem.EDISEFDataObject);
      SubElement := SubElement.Clone;
      SubElement.Parent := Self;
    end
    else
      SubElement := TEDISEFSubElement.Create(Self);
    FEDISEFDataObjects.AppendEDISEFDataObject(SubElement);
    ListItem := CompositeElement.Elements.Next;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFCompositeElement.Disassemble(ELMSList, COMSList: TEDISEFDataObjectList);
var
  I: Integer;
  ElementData: string;
  Temp: TStrings;
  ListItem: TEDISEFDataObjectListItem;
  CompositeElement: TEDISEFCompositeElement;
begin
  FEDISEFDataObjects.DeleteEDISEFDataObjects(True);
  Temp := TStringList.Create;
  try
    if FParent is TEDISEFFile then
    begin
      I := JclStrings.StrSearch('=', FData, 1);
      FId := Copy(FData, 1, I - 1);
      ElementData := Copy(FData, I + 1, Length(FData) - I);
      InternalDisassemble(ElementData, ELMSList);
    end
    else
    if FParent is TEDISEFSegment then
    begin
      Temp.CommaText := FData;
      if Temp.Count >= 1 then
        FId := Temp[0];
      ListItem := COMSList.FindItemByName(FId);
      if (ListItem <> nil) and (ListItem.EDISEFDataObject <> nil) then
      begin
        CompositeElement := TEDISEFCompositeElement(ListItem.EDISEFDataObject);
        Assign(CompositeElement);
      end;
      if Temp.Count >= 2 then
        FRequirementDesignator := Temp[1];
    end;
  finally
    Temp.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFCompositeElement.InternalDisassemble(ElementData: string; ELMSList: TEDISEFDataObjectList);
var
  I, J, K, L, M, X: Integer;
  RepeatCount: Integer;
  RepeatData: string;
  SubElement: TEDISEFSubElement;
  Element: TEDISEFElement;
  ListItem: TEDISEFDataObjectListItem;
begin
  I := 1;
  M := I;
  while I > 0 do
  begin
    // Start search
    I := JclStrings.StrSearch('[', ElementData, M);
    J := JclStrings.StrSearch(']', ElementData, M);
    K := JclStrings.StrSearch('{', ElementData, M);
    L := JclStrings.StrSearch('}', ElementData, M);
    // (rom) better move this if after the I assignment for improved performance
    if I = 0 then
      Break;
    // Determine if data block to process is a repetition
    if (I < K) or (K = 0) then // Normal data block
    begin
      SubElement := TEDISEFSubElement.Create(Self);
      FEDISEFDataObjects.AppendEDISEFDataObject(SubElement);
      SubElement.Data := Copy(ElementData, I + 1, (J - I) - 1);
      SubElement.Disassemble;
      ListItem := ELMSList.FindItemByName(SubElement.ElementId);
      if ListItem <> nil then
      begin
        Element := TEDISEFElement(ListItem.EDISEFDataObject);
        SubElement.AssignDefaultElementValues(Element);
      end;
      M := J + 1;
    end
    else // Repeating data block (K = 1 on the first pass)
    begin
      // Get repeat count
      X := JclStrings.StrSearch('[', ElementData, K);
      RepeatCount := StrToInt(Copy(Data, K + 1, (X - K) - 1));
      // Correct start position
      K := JclStrings.StrSearch('[', ElementData, K);
      // Validate end position
      X := JclStrings.StrSearch('{', ElementData, K + 1);
      while (X <> 0) and (X < L) do // Detect nested repetition
      begin
        X := JclStrings.StrSearch('{', ElementData, X + 1); // Search for nested repetition
        L := JclStrings.StrSearch('}', ElementData, L + 1); // Correct end position
      end;
      // Copy data for repetition
      RepeatData := Copy(ElementData, K, L - K);
      //
      M := L + 1;
      // Disassemble data
      for X := 1 to RepeatCount do
        InternalDisassemble(RepeatData, ELMSList);
    end;
  end;
end;

//==================================================================================================
// TEDISEFSegment
//==================================================================================================

constructor TEDISEFSegment.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISEFSegment.Destroy;
begin
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFSegment.Assemble: string;
begin
  Result := '';
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFSegment.Disassemble(ELMSList, COMSList: TEDISEFDataObjectList);
var
  I{, J}: Integer;
  ElementData: string;
{
  Temp: TStrings;
  Element: TEDISEFElement;
  CompositeElement: TEDISEFCompositeElement;
  ListItem: TEDISEFDataObjectListItem;
}
begin
  FEDISEFDataObjects.DeleteEDISEFDataObjects(True);
  if FParent is TEDISEFFile then
  begin
    I := JclStrings.StrSearch('=', FData, 1);
    FId := Copy(FData, 1, I - 1);
    ElementData := Copy(FData, I + 1, Length(FData) - I);
    InternalDisassemble(ElementData, ELMSList, COMSList);
  end
  else // Data from ???
  begin

  end;
{
  Temp := TStringList.Create;
  try
    Temp.Text := FData;
    FSegmentId := Temp.Names[0];
    //Start first search
    I := JclStrings.StrSearch('[', FData, 1);
    while I > 0 do
    begin
      J := JclStrings.StrSearch(']', FData, I + 1);
      if I <> 0 then
      begin
        ElementData := Copy(FData, I + 1, (J - I) - 1);
        if ElementData[1] = 'C' then
        begin
          CompositeElement := TEDISEFCompositeElement.Create(Self);
          FEDISEFDataObjects.AppendEDISEFDataObject(CompositeElement);
          CompositeElement.Data := ElementData;
          CompositeElement.Disassemble(ELMSList);
        end
        else
        begin
          Element := TEDISEFElement.Create(Self);
          FEDISEFDataObjects.AppendEDISEFDataObject(Element);
          Element.Data := ElementData;
          Element.Disassemble;
          ListItem := ELMSList.FindItemByName(Element.ElementId);
          if ListItem <> nil then
          begin
            Element.AssignDefaultElementValues(TEDISEFElement(ListItem.EDISEFDataObject));
          end;
        end
      end;
      Inc(I);
      // Start next search
      I := JclStrings.StrSearch('[', FData, I);
    end;
  finally
    Temp.Free;
  end;
}
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFSegment.InternalDisassemble(Data: string; ELMSList, COMSList: TEDISEFDataObjectList);
var
  I, J, K, L, M, X: Integer;
  ElementData: string;
  RepeatCount: Integer;
  RepeatData: string;
  Element: TEDISEFElement;
  CompositeElement: TEDISEFCompositeElement;
  ListItem: TEDISEFDataObjectListItem;
begin
  I := 1;
  M := I;
  while I > 0 do
  begin
    // Start search
    I := JclStrings.StrSearch('[', Data, M);
    J := JclStrings.StrSearch(']', Data, M);
    K := JclStrings.StrSearch('{', Data, M);
    L := JclStrings.StrSearch('}', Data, M);
    // (rom) better move this if after the I assignment for improved performance
    if I = 0 then
      Break;
    // Determine if data block to process is a repetition
    if (I < K) or (K = 0) then // Normal data block
    begin
      ElementData := Copy(Data, I + 1, (J - I) - 1);
      if ElementData[1] = 'C' then
      begin
        CompositeElement := TEDISEFCompositeElement.Create(Self);
        FEDISEFDataObjects.AppendEDISEFDataObject(CompositeElement);
        CompositeElement.Data := ElementData;
        CompositeElement.Disassemble(ELMSList, COMSList);
      end
      else
      begin
        Element := TEDISEFElement.Create(Self);
        FEDISEFDataObjects.AppendEDISEFDataObject(Element);
        Element.Data := ElementData;
        Element.Disassemble;
        ListItem := ELMSList.FindItemByName(Element.ElementId);
        if ListItem <> nil then
          Element.AssignDefaultElementValues(TEDISEFElement(ListItem.EDISEFDataObject));
      end;
      M := J + 1;
    end
    else // Repeating data block (K = 1 on the first pass)
    begin
      // Get repeat count
      X := JclStrings.StrSearch('[', Data, K);
      RepeatCount := StrToInt(Copy(Data, K + 1, (X - K) - 1));
      // Correct start position
      K := JclStrings.StrSearch('[', Data, K);
      // Validate end position
      X := JclStrings.StrSearch('{', Data, K + 1);
      while (X <> 0) and (X < L) do //Detect nested repetition
      begin
        X := JclStrings.StrSearch('{', Data, X + 1); // Search for nested repetition
        L := JclStrings.StrSearch('}', Data, L + 1); // Correct end position
      end;
      // Copy data for repetition
      RepeatData := Copy(Data, K, L - K);
      //
      M := L + 1;
      // Disassemble data
      for X := 1 to RepeatCount do
        InternalDisassemble(RepeatData, ELMSList, COMSList);
    end;
  end;
end;

//==================================================================================================
// TEDISEFLoop
//==================================================================================================

constructor TEDISEFLoop.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISEFLoop.Destroy;
begin
  inherited Destroy;
end;

//==================================================================================================
// TEDISEFSection
//==================================================================================================

constructor TEDISEFSection.Create(Parent: TEDISEFDataObject);
begin
  // (rom) added this line
  inherited Create(Parent);
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISEFSection.Destroy;
begin
  inherited Destroy;
end;

//==================================================================================================
// TEDISEFSet
//==================================================================================================

constructor TEDISEFSet.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISEFSet.Destroy;
begin
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFSet.Assemble: string;
begin

end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFSet.Disassemble(SETSList: TEDISEFDataObjectList);
var
  I: Integer;
  SetData: string;
begin
  FEDISEFDataObjects.DeleteEDISEFDataObjects(True);
  if FParent is TEDISEFFile then
  begin
    I := JclStrings.StrSearch('=', FData, 1);
    FId := Copy(FData, 1, I - 1);
    SetData := Copy(FData, I + 1, Length(FData) - I);
//    InternalDisassemble(SetData, SETSList);
  end
  else // Data from ???
  begin

  end;

end;

//==================================================================================================
// TEDISEFFile
//==================================================================================================

constructor TEDISEFFile.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(nil);
  FEDISEFCodesList := TStringList.Create;
  FEDISEFElms := TEDISEFDataObjectList.Create;
  FEDISEFComs := TEDISEFDataObjectList.Create;
  FEDISEFSegs := TEDISEFDataObjectList.Create;
  FEDISEFSets := TEDISEFDataObjectList.Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISEFFile.Destroy;
begin
  FEDISEFSets.Free;
  FEDISEFSegs.Free;
  FEDISEFComs.Free;
  FEDISEFElms.Free;
  FEDISEFCodesList.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFFile.Assemble: string;
begin

end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFFile.Disassemble;
var
  TempList: TStrings;
  SearchResult, SearchResult2, I: Integer;
  Element: TEDISEFElement;
  CompositeElement: TEDISEFCompositeElement;
  Segment: TEDISEFSegment;
  TransactionSet: TEDISEFSet;
begin
  TempList := TStringList.Create;
  try
    // .CODES
    FEDISEFCodesList.Clear;
    SearchResult := JclStrings.StrSearch(SectionTag_CODES, FData, 1);
    if SearchResult > 0 then
    begin
      SearchResult := SearchResult + Length(SectionTag_CODES + #13#10);
      SearchResult2 := JclStrings.StrSearch(#13#10 + '.', FData, SearchResult + 1);
      if SearchResult2 <> 0 then
        FEDISEFCodesList.Text := Copy(FData, SearchResult, SearchResult2 - SearchResult)
      else
        FEDISEFCodesList.Text := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
    end;
    // .ELMS
    TempList.Clear;
    FEDISEFElms.DeleteEDISEFDataObjects(True);
    SearchResult := JclStrings.StrSearch(SectionTag_ELMS, FData, 1);
    if SearchResult > 0 then
    begin
      SearchResult := SearchResult + Length(SectionTag_ELMS + #13#10);
      SearchResult2 := JclStrings.StrSearch(#13#10 + '.', FData, SearchResult + 1);
      if SearchResult2 <> 0 then
        TempList.Text := Copy(FData, SearchResult, SearchResult2 - SearchResult)
      else
        TempList.Text := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
      for I := 0 to TempList.Count - 1 do
      begin
        Element := TEDISEFElement.Create(Self);
        Element.Data := TempList[I];
        Element.Disassemble;
        FEDISEFElms.AppendEDISEFDataObject(Element, Element.ElementId);
      end;
    end
    else
      // (rom) needs resourcestring
      raise Exception.Create('Undefined Error');
    // .COMS
    TempList.Clear;
    FEDISEFComs.DeleteEDISEFDataObjects(True);
    SearchResult := JclStrings.StrSearch(SectionTag_COMS, FData, 1);
    if SearchResult > 0 then
    begin
      SearchResult := SearchResult + Length(SectionTag_COMS + #13#10);
      SearchResult2 := JclStrings.StrSearch(#13#10 + '.', FData, SearchResult + 1);
      if SearchResult2 <> 0 then
        TempList.Text := Copy(FData, SearchResult, SearchResult2 - SearchResult)
      else
        TempList.Text := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
      for I := 0 to TempList.Count - 1 do
      begin
        CompositeElement := TEDISEFCompositeElement.Create(Self);
        CompositeElement.Data := TempList[I];
        CompositeElement.Disassemble(FEDISEFElms, nil);
        FEDISEFComs.AppendEDISEFDataObject(CompositeElement, CompositeElement.CompositeElementId);
      end;
    end
    else
      // (rom) needs resourcestring
      raise Exception.Create('Undefined Error');
    // .SEGS
    TempList.Clear;
    FEDISEFSegs.DeleteEDISEFDataObjects(True);
    SearchResult := JclStrings.StrSearch(SectionTag_SEGS, FData, 1);
    if SearchResult > 0 then
    begin
      SearchResult := SearchResult + Length(SectionTag_SEGS + #13#10);
      SearchResult2 := JclStrings.StrSearch(#13#10 + '.', FData, SearchResult + 1);
      if SearchResult2 <> 0 then
        TempList.Text := Copy(FData, SearchResult, SearchResult2 - SearchResult)
      else
        TempList.Text := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
      for I := 0 to TempList.Count - 1 do
      begin
        Segment := TEDISEFSegment.Create(Self);
        Segment.Data := TempList[I];
        FEDISEFSegs.AppendEDISEFDataObject(Segment);
        if Segment.Data <> '' then
          Segment.Disassemble(FEDISEFElms, FEDISEFComs);
      end;
    end
    else
      // (rom) needs resourcestring
      raise Exception.Create('Undefined Error');
    // .SETS
    TempList.Clear;
    FEDISEFSets.DeleteEDISEFDataObjects(True);
    SearchResult := JclStrings.StrSearch(SectionTag_SETS, FData, 1);
    if SearchResult > 0 then
    begin
      SearchResult := SearchResult + Length(SectionTag_SETS + #13#10);
      SearchResult2 := JclStrings.StrSearch(#13#10 + '.', FData, SearchResult + 1);
      if SearchResult2 <> 0 then
        TempList.Text := Copy(FData, SearchResult, SearchResult2 - SearchResult)
      else
        TempList.Text := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
      for I := 0 to TempList.Count - 1 do
      begin
        TransactionSet := TEDISEFSet.Create(Self);
        TransactionSet.Data := TempList[I];
        FEDISEFSets.AppendEDISEFDataObject(TransactionSet);
        if TransactionSet.Data <> '' then
          TransactionSet.Disassemble(FEDISEFSegs);
      end;
    end
    else
      // (rom) needs resourcestring
      raise Exception.Create('Undefined Error');
  finally
    TempList.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFFile.LoadFromFile(const FileName: string);
begin
  if FileName <> '' then
    FFileName := FileName;
  LoadFromFile;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFFile.LoadFromFile;
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
  end
  else
    // (rom) needs resourcestring
    raise Exception.Create('Undefined Error');
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFFile.SaveToFile(const FileName: string);
begin
  FFileName := FileName;
  SaveToFile;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFFile.SaveToFile;
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
    // (rom) needs resourcestring
    raise Exception.Create('Undefined Error');
end;

end.
