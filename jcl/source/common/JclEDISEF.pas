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
{ EDI Standard Exchange Format (*.sef) File Parser Unit                                            }
{                                                                                                  }
{ This unit is still in development                                                                }
{                                                                                                  }
{ Unit owner: Raymond Alexander                                                                    }
{ Date created: July, 20, 2003                                                                     }
{ Last modified: October 22, 2003                                                                  }
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
  SysUtils, Classes, Contnrs, JclEDI{, Dialogs};

const
  SectionTag_VER = '.VER';
  SectionTag_INI = '.INI';
  SectionTag_PRIVATE = '';
  SectionTag_PUBLIC = '';
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

  Value_UndefinedMaximum = 99999;

  EDISEFComsUserAttributePeriod = '.';
  EDISEFComsUserAttributeExclamationPoint = '!';
  EDISEFComsUserAttributeDollarSign = '$';
  EDISEFComsUserAttributeHyphen = '-';
  EDISEFComsUserAttributeAmpersand = '&';

  EDISEFComsUserAttributePeriodDesc = 'Not Used';
  EDISEFComsUserAttributeExclamationPointDesc = 'Mandatory';
  EDISEFComsUserAttributeDollarSignDesc = 'Recommended';
  EDISEFComsUserAttributeHyphenDesc = 'Not Recommended';
  EDISEFComsUserAttributeAmpersandDesc = 'Dependent';

  EDISEFComsUserAttributeSet = [EDISEFComsUserAttributePeriod,
                                EDISEFComsUserAttributeExclamationPoint,
                                EDISEFComsUserAttributeDollarSign,
                                EDISEFComsUserAttributeHyphen,
                                EDISEFComsUserAttributeAmpersand];

type

  TEDISEFComsUserAttributes = (caPeriod, caExclamationPoint, caDollarSign, caHyphen, caAmpersand);

  TEDISEFObject = class(TEDIObject);
  TEDISEFDataObject = class;
  TEDISEFDataObjectGroup = class;
  TEDISEFElement = class;
  TEDISEFCompositeElement = class;
  TEDISEFSegment = class;
  TEDISEFLoop = class;
  TEDISEFTable = class;
  TEDISEFSet = class;
  TEDISEFFile = class;

  TEDISEFDataObjectListItem = class;
  TEDISEFDataObjectList = class;

  TEDISEFObjectParentType = (sefNil, sefList, sefElement, sefCompositeElement, sefSegment);

//--------------------------------------------------------------------------------------------------
//  EDI SEF Data Object
//--------------------------------------------------------------------------------------------------

  TEDISEFDataObject = class(TEDISEFObject)
  protected
    FState: TEDIDataObjectDataState;
    FId: string;
    FData: string;
    FLength: Integer;
    FParent: TEDISEFDataObject;
    FSEFFile: TEDISEFFile;
    FErrorLog: TStrings;
    function GetData: string;
    procedure SetData(const Data: string);
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; virtual; abstract;
    procedure Disassemble; virtual; abstract;
    function Clone: TEDISEFDataObject; virtual; abstract;
  published
    property State: TEDIDataObjectDataState read FState;
    property Id: string read FId write FId;
    property Data: string read GetData write SetData;
    property DataLength: Integer read FLength;
    property Parent: TEDISEFDataObject read FParent write FParent;
    property SEFFile: TEDISEFFile read FSEFFile write FSEFFile;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI SEF Data Object List Item
//--------------------------------------------------------------------------------------------------

  TEDISEFDataObjectListItem = class(TEDIObjectListItem)
  private
    FName: string;
    function GetEDISEFDataObject: TEDISEFDataObject;
    procedure SetEDISEFDataObject(const Value: TEDISEFDataObject);
  published
    property Name: string read FName write FName;
    property EDISEFDataObject: TEDISEFDataObject read GetEDISEFDataObject write SetEDISEFDataObject;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI SEF Data Object List
//--------------------------------------------------------------------------------------------------

  TEDISEFDataObjectList = class(TEDIObjectList)
  private
    function GetEDISEFDataObject(Index: Integer): TEDISEFDataObject;
    procedure SetEDISEFDataObject(Index: Integer; const Value: TEDISEFDataObject);
  public
    function CreateListItem(PriorItem: TEDIObjectListItem;
      EDIObject: TEDIObject = nil): TEDIObjectListItem; override;
    function First(Index: Integer = 0): TEDISEFDataObjectListItem; reintroduce;
    function Next: TEDISEFDataObjectListItem; reintroduce;
    function Prior: TEDISEFDataObjectListItem; reintroduce;
    function Last: TEDISEFDataObjectListItem; reintroduce;
    //
    procedure AddByNameOrId(EDISEFDataObject: TEDISEFDataObject; Name: string = '');
    function FindItemByName(Name: string): TEDISEFDataObjectListItem;
    function GetObjectByItemByName(Name: string): TEDISEFDataObject;
    //
    property EDISEFDataObject[Index: Integer]: TEDISEFDataObject read GetEDISEFDataObject
      write SetEDISEFDataObject; default;
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
  published
    property EDISEFDataObjects: TEDISEFDataObjectList read FEDISEFDataObjects;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI SEF Repeating Pattern
//--------------------------------------------------------------------------------------------------

  TEDISEFRepeatingPattern = class(TEDISEFDataObjectGroup)
  private
    FBaseParent: TEDISEFDataObject;
    FRepeatCount: Integer;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    function Clone: TEDISEFRepeatingPattern; reintroduce;
  published
    property BaseParent: TEDISEFDataObject read FBaseParent;
    property RepeatCount: Integer read FRepeatCount write FRepeatCount;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI SEF Element
//--------------------------------------------------------------------------------------------------

  TEDISEFElement = class(TEDISEFDataObject)
  protected
    FUserAttribute: string;
    FOrdinal: Integer;
    FElementType: string;
    FMinimumLength: Integer;
    FMaximumLength: Integer;
    FRequirementDesignator: string;
    FRepeatCount: Integer;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    procedure Assign(EDISEFElement: TEDISEFElement);
    function Clone: TEDISEFElement; reintroduce;
  published
    property UserAttribute: string read FUserAttribute write FUserAttribute;
    property Ordinal: Integer read FOrdinal write FOrdinal;
    property ElementId: string read FId write FId;
    property ElementType: string read FElementType write FElementType;
    property MinimumLength: Integer read FMinimumLength write FMinimumLength;
    property MaximumLength: Integer read FMaximumLength write FMaximumLength;
    property RequirementDesignator: string read FRequirementDesignator write FRequirementDesignator;
    property RepeatCount: Integer read FRepeatCount write FRepeatCount;
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
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    procedure Assign(CompositeElement: TEDISEFCompositeElement);
    function Clone: TEDISEFCompositeElement; reintroduce;
  published
    property UserAttribute: string read FUserAttribute write FUserAttribute;
    property Ordinal: Integer read FOrdinal write FOrdinal;
    property CompositeElementId: string read FId write FId;
    property RequirementDesignator: string read FRequirementDesignator write FRequirementDesignator;
    property Elements: TEDISEFDataObjectList read FEDISEFDataObjects;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI SEF Segment
//--------------------------------------------------------------------------------------------------

  TEDISEFSegment = class(TEDISEFDataObjectGroup)
  private
    FUserAttribute: string;
    FOrdinal: Integer;
    FRequirementDesignator: string;
    FMaximumUse: Integer;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    procedure Assign(Segment: TEDISEFSegment);
    function Clone: TEDISEFSegment; reintroduce;
  published
    property UserAttribute: string read FUserAttribute write FUserAttribute;
    property Ordinal: Integer read FOrdinal write FOrdinal;
    property SegmentId: string read FId write FId;
    property RequirementDesignator: string read FRequirementDesignator write FRequirementDesignator;
    property MaximumUse: Integer read FMaximumUse write FMaximumUse;
    property Elements: TEDISEFDataObjectList read FEDISEFDataObjects;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI SEF Loop
//--------------------------------------------------------------------------------------------------

  TEDISEFLoop = class(TEDISEFDataObjectGroup)
  private
    FMaximumRepeat: Integer;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    function Clone: TEDISEFLoop; reintroduce;
  published
    property LoopId: string read FId write FId;
    property MaximumRepeat: Integer read FMaximumRepeat write FMaximumRepeat;
  published
  end;

//--------------------------------------------------------------------------------------------------
//  EDI SEF Table
//--------------------------------------------------------------------------------------------------

  TEDISEFTable = class(TEDISEFDataObjectGroup)
  private
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    function Clone: TEDISEFTable; reintroduce;
  published
  end;

//--------------------------------------------------------------------------------------------------
//  EDI SEF Set
//--------------------------------------------------------------------------------------------------

  TEDISEFSet = class(TEDISEFDataObjectGroup)
  private
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    function Clone: TEDISEFSet; reintroduce;
  published
    property Tables: TEDISEFDataObjectList read FEDISEFDataObjects;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI SEF File
//--------------------------------------------------------------------------------------------------

  TEDISEFFile = class(TEDISEFDataObject)
  private
    FFileName: string;
    FEDISEFCodesList: TStrings;
    FEDISEFElms: TEDISEFDataObjectList;
    FEDISEFComs: TEDISEFDataObjectList;
    FEDISEFSegs: TEDISEFDataObjectList;
    FEDISEFSets: TEDISEFDataObjectList;
    FEDISEFStd: TStrings;
    FEDISEFIni: TStrings;
    FEDISEFVer: string;
    procedure ParseCodes;
    procedure ParseELMS;
    procedure ParseCOMS;
    procedure ParseSEGS;
    procedure ParseSETS;
    procedure ParseSTD;
    procedure ParseINI;
    procedure ParseVER;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;

    procedure LoadFromFile; overload;
    procedure LoadFromFile(const FileName: string); overload;
    procedure SaveToFile; overload;
    procedure SaveToFile(const FileName: string); overload;
    procedure Unload;

    function Assemble: string; override;
    procedure Disassemble; override;

    function Clone: TEDISEFFile; reintroduce;
  published
    property FileName: string read FFileName write FFileName;
    property Codes: TStrings read FEDISEFCodesList;
    property ELMS: TEDISEFDataObjectList read FEDISEFElms;
    property COMS: TEDISEFDataObjectList read FEDISEFComs;
    property SEGS: TEDISEFDataObjectList read FEDISEFSegs;
    property SETS: TEDISEFDataObjectList read FEDISEFSets;
    property STD: TStrings read FEDISEFStd;
    property INI: TStrings read FEDISEFIni;
    property VER: string read FEDISEFVer write FEDISEFVer;
  end;

//--------------------------------------------------------------------------------------------------
//  Procedures
//--------------------------------------------------------------------------------------------------

function GetEDISEFComsUserAttributeDescription(
  Attribute: TEDISEFComsUserAttributes): string; overload;
function GetEDISEFComsUserAttributeDescription(Attribute: string): string; overload;

procedure ParseELMSDataOfELMSDefinition(Data: string; Element: TEDISEFElement);
function CombineELMSDataOfELMSDefinition(Element: TEDISEFElement): string;
procedure ParseELMSDataOfCOMSDefinition(Data: string; Element: TEDISEFElement;
  ELMSList: TEDISEFDataObjectList);
procedure ParseELMSDataOfSEGSDefinition(Data: string; Element: TEDISEFElement;
  ELMSList: TEDISEFDataObjectList);
function CombineELMSDataOfCOMSorSEGSDefinition(Element: TEDISEFElement;
  ELMSList: TEDISEFDataObjectList): string;

procedure ParseCOMSDataOfCOMSDefinition(Data: string; CompositeElement: TEDISEFCompositeElement;
  ELMSList: TEDISEFDataObjectList);
function CombineCOMSDataOfCOMSDefinition(CompositeElement: TEDISEFCompositeElement): string;
procedure ParseCOMSDataOfSEGSDefinition(Data: string; CompositeElement: TEDISEFCompositeElement;
  COMSList: TEDISEFDataObjectList);
function CombineCOMSDataOfSEGSDefinition(CompositeElement: TEDISEFCompositeElement): string;

procedure ParseSEGSDataOfSEGSDefinition(Data: string; Segment: TEDISEFSegment;
  SEFFile: TEDISEFFile);
function CombineSEGSDataOfSEGSDefinition(Segment: TEDISEFSegment): string;
procedure ParseSEGSDataOfSETSDefinition(Data: string; Segment: TEDISEFSegment;
  SEFFile: TEDISEFFile);
function CombineSEGSDataOfSETSDefinition(Segment: TEDISEFSegment): string;

procedure ParseLoopDataOfSETSDefinition(Data: string; Loop: TEDISEFLoop;
  SEFFile: TEDISEFFile);
procedure ParseTableDataOfSETSDefinition(Data: string; Table: TEDISEFTable;
  SEFFile: TEDISEFFile);
procedure ParseSetsDataOfSETSDefinition(Data: string; _Set: TEDISEFSet; SEFFile: TEDISEFFile);

implementation

uses
  JclResources, JclStrings;

//--------------------------------------------------------------------------------------------------
//  Procedures
//--------------------------------------------------------------------------------------------------

function GetEDISEFComsUserAttributeDescription(Attribute: TEDISEFComsUserAttributes): string;
begin
  case Attribute of
    caPeriod: Result := EDISEFComsUserAttributePeriodDesc;
    caExclamationPoint: Result := EDISEFComsUserAttributeExclamationPointDesc;
    caDollarSign: Result := EDISEFComsUserAttributeDollarSignDesc;
    caHyphen: Result := EDISEFComsUserAttributeHyphenDesc;
    caAmpersand: Result := EDISEFComsUserAttributeAmpersandDesc;
  else
    Result := 'Unknown Attribute';
  end;
end;

function GetEDISEFComsUserAttributeDescription(Attribute: string): string;
begin
  if Attribute = '' then Attribute := '?';
  case Attribute[1] of
    EDISEFComsUserAttributePeriod: Result := EDISEFComsUserAttributePeriodDesc;
    EDISEFComsUserAttributeExclamationPoint: Result := EDISEFComsUserAttributeExclamationPointDesc;
    EDISEFComsUserAttributeDollarSign: Result := EDISEFComsUserAttributeDollarSignDesc;
    EDISEFComsUserAttributeHyphen: Result := EDISEFComsUserAttributeHyphenDesc;
    EDISEFComsUserAttributeAmpersand: Result := EDISEFComsUserAttributeAmpersandDesc;
  else
    Result := 'Unknown Attribute';
  end;
end;

procedure ParseELMSDataOfELMSDefinition(Data: string; Element: TEDISEFElement);
var
  Temp: TStrings;
begin
  // Clear any old values
  Element.UserAttribute := '';
  Element.Ordinal := -1;
  Element.ElementType := '';
  Element.MinimumLength := 0;
  Element.MaximumLength := 0;
  Element.RequirementDesignator := 'O';
  //
  Temp := TStringList.Create;
  try
    Temp.Text := Data;
    Element.Id := Temp.Names[0];
    {$IFDEF DELPHI7_UP}
    Temp.CommaText := Temp.ValueFromIndex[0];
    {$ELSE}
    Temp.CommaText := Temp.Values[Element.Id];
    {$ENDIF}
    if Temp.Count >= 1 then
      Element.ElementType := Temp[0];
    if Temp.Count >= 2 then
      Element.MinimumLength := StrToInt(Temp[1]);
    if Temp.Count >= 3 then
      Element.MaximumLength := StrToInt(Temp[2]);
  finally
    Temp.Free;
  end;
end;

function CombineELMSDataOfELMSDefinition(Element: TEDISEFElement): string;
begin
  Result := Element.Id + '=' + Element.ElementType + ',' + IntToStr(Element.MaximumLength) + ',' +
    IntToStr(Element.MaximumLength);
end;

procedure ParseELMSDataOfCOMSDefinition(Data: string; Element: TEDISEFElement;
  ELMSList: TEDISEFDataObjectList);
var
  I, J, K, L, M, N, O: Integer;
  ListItem: TEDISEFDataObjectListItem;
begin
  // Clear any old values
  Element.UserAttribute := '';
  Element.Ordinal := -1;
  Element.ElementType := '';
  Element.MinimumLength := 0;
  Element.MaximumLength := 0;
  Element.RequirementDesignator := 'O';
  Element.RepeatCount := 0;
  // Parse User Attribute
  if Data[1] in EDISEFComsUserAttributeSet then
  begin
    Element.UserAttribute := Data[1];
    I := 2;
  end
  else
    I := 1;
  // Get delimiter locations
  J := StrSearch('@', Data, 1);
  K := StrSearch(';', Data, 1);
  L := StrSearch(':', Data, 1);
  M := StrSearch(',', Data, 1);
  O := Length(Data) + 1;
  // Parse Id using the closest delimiter
  N := O;
  if J <> 0 then if N > J then N := J;
  if K <> 0 then if N > K then N := K;
  if L <> 0 then if N > L then N := L;
  if M <> 0 then if N > M then N := M;
  Element.Id := Copy(Data, I, N - I);
  // Get Default Values
  if ELMSList <> nil then
  begin
    ListItem := ELMSList.FindItemByName(Element.Id);
    if ListItem <> nil then
      Element.Assign(TEDISEFElement(ListItem.EDISEFDataObject));
  end;
  // Parse other attributes
  if J <> 0 then
  begin
    Inc(J);
    N := O;
    if K <> 0 then if N > K then N := K;
    if L <> 0 then if N > L then N := L;
    if M <> 0 then if N > M then N := M;
    if (N - J) > 0 then
      Element.Ordinal := StrToInt(Copy(Data, J, N - J));
  end;
  if K <> 0 then
  begin
    Inc(K);
    N := O;
    if L <> 0 then if N > L then N := L;
    if M <> 0 then if N > M then N := M;
    if (N - K) > 0 then
      Element.MinimumLength := StrToInt(Copy(Data, K, N - K));
  end;
  if L <> 0 then
  begin
    Inc(L);
    N := O;
    if M <> 0 then if N > M then N := M;
    if (N - L) > 0 then
      Element.MaximumLength := StrToInt(Copy(Data, L, N - L));
  end;
  if M <> 0 then
  begin
    Inc(M);
    if (O - M) > 0 then
      Element.RequirementDesignator := Copy(Data, M, 1);
  end;
end;

function CombineELMSDataOfCOMSorSEGSDefinition(Element: TEDISEFElement;
  ELMSList: TEDISEFDataObjectList): string;
var
  CompareElement: TEDISEFElement;
  ListItem: TEDISEFDataObjectListItem;
begin
  if Element.UserAttribute <> '' then
    Result := Result + Element.UserAttribute;
  Result := Result + Element.Id;
  if Element.Ordinal <> -1 then
    Result := Result + '@' + IntToStr(Element.Ordinal);
  // Get Default Values
  CompareElement := nil;
  if ELMSList <> nil then
  begin
    ListItem := ELMSList.FindItemByName(Element.Id);
    if ListItem <> nil then
      CompareElement := TEDISEFElement(ListItem.EDISEFDataObject);
  end;
  // Test for changes in default values
  if CompareElement <> nil then
  begin
    if (CompareElement.MinimumLength <> Element.MinimumLength) or
      (CompareElement.MaximumLength <> Element.MaximumLength) then
    begin
      Result := Result + ';';
      if CompareElement.MinimumLength <> Element.MinimumLength then
        Result := Result + IntToStr(Element.MinimumLength);
      Result := Result + ':';
      if CompareElement.MaximumLength <> Element.MaximumLength then
        Result := Result + IntToStr(Element.MaximumLength);
    end;
  end
  else
  begin
    Result := Result + ';';
    Result := Result + IntToStr(Element.MinimumLength);
    Result := Result + ':';
    Result := Result + IntToStr(Element.MaximumLength);
  end;
  if (Element.RequirementDesignator <> '') and (Element.RequirementDesignator <> 'O') then
    Result := Result + ',' + Element.RequirementDesignator;
  if (Element.Parent is TEDISEFSegment) and (Element.RepeatCount > 0) then
    Result := Result + IntToStr(Element.RepeatCount);
end;

procedure ParseELMSDataOfSEGSDefinition(Data: string; Element: TEDISEFElement;
  ELMSList: TEDISEFDataObjectList);
var
  I, J, K, L, M, N, O, P: Integer;
  ListItem: TEDISEFDataObjectListItem;
begin
  // Clear any old values
  Element.UserAttribute := '';
  Element.Ordinal := -1;
  Element.ElementType := '';
  Element.MinimumLength := 0;
  Element.MaximumLength := 0;
  Element.RequirementDesignator := 'O';
  Element.RepeatCount := 0;
  // Parse User Attribute
  if Data[1] in EDISEFComsUserAttributeSet then
  begin
    Element.UserAttribute := Data[1];
    I := 2;
  end
  else
    I := 1;
  // Get delimiter locations
  J := StrSearch('@', Data, 1);
  K := StrSearch(';', Data, 1);
  L := StrSearch(':', Data, 1);
  M := StrSearch(',', Data, 1);
  N := StrSearch(',', Data, M + 1);
  P := Length(Data) + 1;
  // Parse Id
  O := P;
  if J <> 0 then if O > J then O := J;
  if K <> 0 then if O > K then O := K;
  if L <> 0 then if O > L then O := L;
  if M <> 0 then if O > M then O := M;
  Element.Id := Copy(Data, I, O - I);
  // Get Default Values
  if ELMSList <> nil then
  begin
    ListItem := ELMSList.FindItemByName(Element.Id);
    if ListItem <> nil then
      Element.Assign(TEDISEFElement(ListItem.EDISEFDataObject));
  end;
  // Parse other attributes
  if J <> 0 then
  begin
    Inc(J);
    O := P;
    if K <> 0 then if O > K then O := K;
    if L <> 0 then if O > L then O := L;
    if M <> 0 then if O > M then O := M;
    if (O - J) > 0 then
      Element.Ordinal := StrToInt(Copy(Data, J, O - J));
  end;
  if K <> 0 then
  begin
    Inc(K);
    O := P;
    if L <> 0 then if O > L then O := L;
    if M <> 0 then if O > M then O := M;
    if (O - K) > 0 then
      Element.MinimumLength := StrToInt(Copy(Data, K, O - K));
  end;
  if L <> 0 then
  begin
    Inc(L);
    O := P;
    if M <> 0 then if O > M then O := M;
    if (O - L) > 0 then
      Element.MaximumLength := StrToInt(Copy(Data, L, O - L));
  end;
  if M <> 0 then
  begin
    Inc(M);
    O := P;
    if N <> 0 then if O > N then O := N;
    if (O - M) > 0 then
      Element.RequirementDesignator := Copy(Data, M, O - M);
  end;
  if N <> 0 then
  begin
    Inc(N);
    if (P - N) > 0 then
      Element.RepeatCount := StrToInt(Copy(Data, N, 1));
  end;
end;

// Parse TEDISEFCompositeElement or Repeating Pattern in TEDISEFCompositeElement
procedure InternalParseCOMSDataOfCOMSDefinition(Data: string; Element: TEDISEFDataObjectGroup;
  ELMSList: TEDISEFDataObjectList);
var
  I, J, K, L, M, N: Integer;
  RepeatCount: Integer;
  RepeatData: string;
  SubElement: TEDISEFSubElement;
  RepeatingPattern: TEDISEFRepeatingPattern;
begin
  I := StrSearch('=', Data, 1);
  if (I > 0) and (Element is TEDISEFCompositeElement) then
  begin
    Element.EDISEFDataObjects.Clear;
    Element.Id := Copy(Data, 1, I - 1);
  end;
  Inc(I);
  M := I;
  while I > 0 do
  begin
    // Start search
    I := StrSearch('[', Data, M);
    if I = 0 then Break;
    J := StrSearch(']', Data, M);
    K := StrSearch('{', Data, M);
    L := StrSearch('}', Data, M);
    // Determine if data block to process is a repetition
    if (I < K) or (K = 0) then // Normal data block
    begin
      SubElement := TEDISEFSubElement.Create(Element);
      Element.EDISEFDataObjects.AddByNameOrId(SubElement);
      SubElement.Data := Copy(Data, I + 1, (J - I) - 1);
      SubElement.Disassemble;
      M := J + 1;
    end
    else // Repeating data block (K = 1 on the first pass)
    begin
      // Get repeat count
      N := StrSearch('[', Data, K);
      RepeatCount := StrToInt(Copy(Data, K + 1, (N - K) - 1));
      // Correct start position
      K := StrSearch('[', Data, K);
      // Validate end position
      N := StrSearch('{', Data, K + 1);
      while (N <> 0) and (N < L) do // Detect nested repetition
      begin
        N := StrSearch('{', Data, N + 1); // Search for nested repetition
        L := StrSearch('}', Data, L + 1); // Correct end position
      end;
      // Copy data for repetition
      RepeatData := Copy(Data, K, L - K);
      M := L + 1;
      // Handle Repeating Data Block
      RepeatingPattern := TEDISEFRepeatingPattern.Create(Element);
      Element.EDISEFDataObjects.AddByNameOrId(RepeatingPattern);
      RepeatingPattern.RepeatCount := RepeatCount;
      RepeatingPattern.Data := RepeatData;
      RepeatingPattern.Disassemble;
      //Disassemble data (Keep this commented code here for now)
      //for N := 1 to RepeatCount do
      //begin
      //  InternalParseCOMSDataOfCOMSDefinition(RepeatData, Element, ELMSList);
      //end;
    end; // if
  end; // while
end;

procedure ParseCOMSDataOfCOMSDefinition(Data: string; CompositeElement: TEDISEFCompositeElement;
  ELMSList: TEDISEFDataObjectList);
begin
  InternalParseCOMSDataOfCOMSDefinition(Data, CompositeElement, ELMSList);
end;

function CombineCOMSDataOfCOMSDefinition(CompositeElement: TEDISEFCompositeElement): string;
var
  I: Integer;
begin
  Result := CompositeElement.Id + '=';
  for I := 0 to CompositeElement.Elements.Count - 1 do
  begin
    if not (CompositeElement.Elements[I] is TEDISEFRepeatingPattern) then
      Result := Result + '[' + CompositeElement.Elements[I].Assemble + ']'
    else
      Result := Result + '{' + CompositeElement.Elements[I].Assemble + '}';
  end;
end;

procedure ParseCOMSDataOfSEGSDefinition(Data: string; CompositeElement: TEDISEFCompositeElement;
  COMSList: TEDISEFDataObjectList);
var
  Temp: TStrings;
  ListItem: TEDISEFDataObjectListItem;
  DefaultCompositeElement: TEDISEFCompositeElement;
begin
  CompositeElement.EDISEFDataObjects.Clear;
  Temp := TStringList.Create;
  try
    Temp.CommaText := Data;
    if Temp.Count >= 1 then
      CompositeElement.Id := Temp[0];
    ListItem := COMSList.FindItemByName(CompositeElement.Id);
    if (ListItem <> nil) and (ListItem.EDISEFDataObject <> nil) then
    begin
      DefaultCompositeElement := TEDISEFCompositeElement(ListItem.EDISEFDataObject);
      CompositeElement.Assign(DefaultCompositeElement);
    end;
    if Temp.Count >= 2 then
      CompositeElement.RequirementDesignator := Temp[1];
  finally
    Temp.Free;
  end;
end;

function CombineCOMSDataOfSEGSDefinition(CompositeElement: TEDISEFCompositeElement): string;
begin
  if CompositeElement.UserAttribute <> '' then
    Result := Result + CompositeElement.UserAttribute;
  Result := Result + CompositeElement.Id;
  if CompositeElement.Ordinal > 0 then
    Result := Result + '@' + IntToStr(CompositeElement.Ordinal);
  if (CompositeElement.RequirementDesignator <> '') and
    (CompositeElement.RequirementDesignator <> 'O') then
  begin
    Result := Result + ',' + CompositeElement.RequirementDesignator;
  end;
end;

// Parse TEDISEFSegment or Repeating Pattern in TEDISEFSegment
procedure InternalParseSEGSDataOfSEGSDefinition(Data: string; Segment: TEDISEFDataObjectGroup;
  SEFFile: TEDISEFFile);
var
  I, J, K, L, M, N: Integer;
  ElementData: string;
  RepeatCount: Integer;
  RepeatData: string;
  Element: TEDISEFElement;
  CompositeElement: TEDISEFCompositeElement;
  RepeatingPattern: TEDISEFRepeatingPattern;
begin
  I := StrSearch('=', Data, 1);
  if (I > 0) and (Segment is TEDISEFSegment) then
  begin
    Segment.EDISEFDataObjects.Clear;
    Segment.Id := Copy(Data, 1, I - 1);
    TEDISEFSegment(Segment).RequirementDesignator := 'O';
    TEDISEFSegment(Segment).MaximumUse := 1;
  end;
  Inc(I);
  M := I;
  while I > 0 do
  begin
    // Start search
    I := StrSearch('[', Data, M);
    if I = 0 then Break;
    J := StrSearch(']', Data, M);
    K := StrSearch('{', Data, M);
    L := StrSearch('}', Data, M);
    // Determine if data block to process is a repetition
    if (I < K) or (K = 0) then // Normal data block
    begin
      ElementData := Copy(Data, I + 1, (J - I) - 1);
      if ElementData[1] = 'C' then
      begin
        CompositeElement := TEDISEFCompositeElement.Create(Segment);
        Segment.EDISEFDataObjects.AddByNameOrId(CompositeElement);
        CompositeElement.SEFFile := SEFFile;
        CompositeElement.Data := ElementData;
        CompositeElement.Disassemble;
      end
      else
      begin
        Element := TEDISEFElement.Create(Segment);
        Segment.EDISEFDataObjects.AddByNameOrId(Element);
        Element.SEFFile := SEFFile;
        Element.Data := ElementData;
        Element.Disassemble;
      end; // if
      M := J + 1;
    end
    else // Repeating data block (K = 1 on the first pass)
    begin
      // Get repeat count
      N := StrSearch('[', Data, K);
      RepeatCount := StrToInt(Copy(Data, K + 1, (N - K) - 1));
      // Correct start position
      K := StrSearch('[', Data, K);
      // Validate end position
      N := StrSearch('{', Data, K + 1);
      while (N <> 0) and (N < L) do // Detect nested repetition
      begin
        N := StrSearch('{', Data, N + 1); // Search for nested repetition
        L := StrSearch('}', Data, L + 1); // Correct end position
      end;
      // Copy data for repetition
      RepeatData := Copy(Data, K, L - K);
      M := L + 1;
      // Handle Repeating Data Block
      RepeatingPattern := TEDISEFRepeatingPattern.Create(Segment);
      Segment.EDISEFDataObjects.AddByNameOrId(RepeatingPattern);
      RepeatingPattern.RepeatCount := RepeatCount;
      RepeatingPattern.Data := RepeatData;
      RepeatingPattern.Disassemble;
      //Disassemble data (Keep this commented code here for now)
      //for N := 1 to RepeatCount do
      //begin
      //  InternalParseSEGSDataOfSEGSDefinition(RepeatData, Segment, SEFFile);
      //end;
    end; //if
  end; //while
end;

procedure ParseSEGSDataOfSEGSDefinition(Data: string; Segment: TEDISEFSegment;
  SEFFile: TEDISEFFile);
begin
  InternalParseSEGSDataOfSEGSDefinition(Data, Segment, SEFFile);
end;

function CombineSEGSDataOfSEGSDefinition(Segment: TEDISEFSegment): string;
var
  I: Integer;
begin
  Result := Segment.Id + '=';
  for I := 0 to Segment.Elements.Count - 1 do
  begin
    if not (Segment.Elements[I] is TEDISEFRepeatingPattern) then
      Result := Result + '[' + Segment.Elements[I].Assemble + ']'
    else
      Result := Result + '{' + Segment.Elements[I].Assemble + '}';
  end;
end;

procedure ParseSEGSDataOfSETSDefinition(Data: string; Segment: TEDISEFSegment;
  SEFFile: TEDISEFFile);
var
  Temp: TStrings;
  ListItem: TEDISEFDataObjectListItem;
  SegmentDef: TEDISEFSegment;
begin
  Segment.RequirementDesignator := 'O';
  Segment.MaximumUse := 1;
  Segment.EDISEFDataObjects.Clear;
  Temp := TStringList.Create;
  try
    Temp.CommaText := Data;
    if Temp.Count >= 1 then
      Segment.Id := Temp[0];
    ListItem := SEFFile.SEGS.FindItemByName(Segment.Id);
    if (ListItem <> nil) and (ListItem.EDISEFDataObject <> nil) then
    begin
      SegmentDef := TEDISEFSegment(ListItem.EDISEFDataObject);
      Segment.Assign(SegmentDef);
    end;
    if Temp.Count >= 2 then
    begin
      if Temp[1] = '' then
        Temp[1] := 'O';
      Segment.RequirementDesignator := Temp[1];
    end;
    if Temp.Count >= 3 then
    begin
      if Temp[2] = '>1' then
        Temp[2] := IntToStr(Value_UndefinedMaximum);
      Segment.MaximumUse := StrToInt(Temp[2]);
    end;
  finally
    Temp.Free;
  end;
end;

function CombineSEGSDataOfSETSDefinition(Segment: TEDISEFSegment): string;
begin
  if Segment.UserAttribute <> '' then
    Result := Result + Segment.UserAttribute;
  Result := Result + Segment.Id;
  if Segment.Ordinal > 0 then
    Result := Result + '@' + IntToStr(Segment.Ordinal);
  if (Segment.RequirementDesignator <> '') and (Segment.RequirementDesignator <> 'O') then
    Result := Result + ',' + Segment.RequirementDesignator;
  if Segment.MaximumUse > 1 then
  begin
    if (Segment.RequirementDesignator = '') or (Segment.RequirementDesignator = 'O') then
      Result := Result + ',';
    if Segment.MaximumUse = Value_UndefinedMaximum then
      Result := Result + ',>1'
    else
      Result := Result + ',' + IntToStr(Segment.MaximumUse);
  end;
end;

procedure ParseLoopDataOfSETSDefinition(Data: string; Loop: TEDISEFLoop;
  SEFFile: TEDISEFFile);
var
  I, J, K, L, M, N: Integer;
  SegmentData: string;
  RepeatCount: Integer;
  LoopId, RepeatData: string;
  Segment: TEDISEFSegment;
  NestedLoop: TEDISEFLoop;
  ListItem: TEDISEFDataObjectListItem;
begin
  Loop.EDISEFDataObjects.Clear;
  I := 1;
  M := I;
  // Search for Loops and Segments
  while I > 0 do
  begin
    // Start search
    I := StrSearch('[', Data, M);
    if I = 0 then Break;
    J := StrSearch(']', Data, M);
    K := StrSearch('{', Data, M);
    L := StrSearch('}', Data, M);
    // Determine if data block to process is a repetition
    if (I < K) or (K = 0) then // Normal data block
    begin
      SegmentData := Copy(Data, I + 1, (J - I) - 1);
      //
      Segment := TEDISEFSegment.Create(Loop);
      Loop.EDISEFDataObjects.AddByNameOrId(Segment);
      Segment.SEFFile := SEFFile;
      Segment.Data := SegmentData;
      Segment.Disassemble;
      if Loop.Id = '' then
        Loop.Id := Segment.Id;
      M := J + 1;
    end
    else // Repeating data block (K = 1 on the first pass)
    begin
      N := StrSearch('[', Data, K);
      J := StrSearch('+', Data, K);
      if (J < N) and (J <> 0) then N := J;
      // Get Loop Id
      RepeatData := Copy(Data, K + 1, (N - K) - 1);
      J := StrSearch(':', RepeatData, 1);
      LoopId := Copy(RepeatData, 1, J - 1);
      // Get Repeat Count
      RepeatData := Copy(RepeatData, J + 1, Length(RepeatData));
      if RepeatData = '>1' then
        RepeatData := IntToStr(Value_UndefinedMaximum);
      if RepeatData = '' then
        RepeatData := '1';
      RepeatCount := StrToInt(RepeatData);
      // Correct start position
      K := StrSearch('[', Data, K);
      // Validate end position
      N := StrSearch('{', Data, K + 1);
      while (N <> 0) and (N < L) do // Detect nested repetition
      begin
        N := StrSearch('{', Data, N + 1); // Search for nested repetition
        L := StrSearch('}', Data, L + 1); // Correct end position
      end;
      // Copy data for repetition
      RepeatData := Copy(Data, K, L - K);
      //
      M := L + 1;
      // Create Loop Object
      NestedLoop := TEDISEFLoop.Create(Loop);
      Loop.EDISEFDataObjects.AddByNameOrId(NestedLoop);
      NestedLoop.SEFFile := SEFFile;
      NestedLoop.LoopId := LoopId;
      NestedLoop.MaximumRepeat := RepeatCount;
      NestedLoop.Data := RepeatData;
      NestedLoop.Disassemble;
      if NestedLoop.LoopId = '' then
      begin
        ListItem := NestedLoop.EDISEFDataObjects.First;
        NestedLoop.LoopId := ListItem.EDISEFDataObject.Id;
      end;
    end; // if
  end; // while
end;

procedure ParseTableDataOfSETSDefinition(Data: string; Table: TEDISEFTable;
  SEFFile: TEDISEFFile);
var
  I, J, K, L, M, N: Integer;
  SegmentData: string;
  RepeatCount: Integer;
  LoopId, RepeatData: string;
  Segment: TEDISEFSegment;
  Loop: TEDISEFLoop;
  ListItem: TEDISEFDataObjectListItem;
begin
  Table.EDISEFDataObjects.Clear;
  I := 1;
  M := I;
  // Search for Loops and Segments
  while I > 0 do
  begin
    // Start search
    I := StrSearch('[', Data, M);
    if I = 0 then Break;
    J := StrSearch(']', Data, M);
    K := StrSearch('{', Data, M);
    L := StrSearch('}', Data, M);
    // Determine if data block to process is a repetition
    if (I < K) or (K = 0) then // Normal data block
    begin
      SegmentData := Copy(Data, I + 1, (J - I) - 1);
      //
      Segment := TEDISEFSegment.Create(Table);
      Table.EDISEFDataObjects.AddByNameOrId(Segment);
      Segment.SEFFile := SEFFile;
      Segment.Data := SegmentData;
      Segment.Disassemble;
      M := J + 1;
    end
    else // Repeating data block (K = 1 on the first pass)
    begin
      N := StrSearch('[', Data, K);
      J := StrSearch('+', Data, K);
      if (J < N) and (J <> 0) then N := J;
      // Get Loop Id
      RepeatData := Copy(Data, K + 1, (N - K) - 1);
      J := StrSearch(':', RepeatData, 1);
      LoopId := Copy(RepeatData, 1, J - 1);
      // Get Repeat Count
      RepeatData := Copy(RepeatData, J + 1, Length(RepeatData));
      if RepeatData = '>1' then
        RepeatData := IntToStr(Value_UndefinedMaximum);
      if RepeatData = '' then
        RepeatData := '1';
      RepeatCount := StrToInt(RepeatData);
      // Correct start position
      K := StrSearch('[', Data, K);
      // Validate end position
      N := StrSearch('{', Data, K + 1);
      while (N <> 0) and (N < L) do // Detect nested repetition
      begin
        N := StrSearch('{', Data, N + 1); // Search for nested repetition
        L := StrSearch('}', Data, L + 1); // Correct end position
      end;
      // Copy data for repetition
      RepeatData := Copy(Data, K, L - K);
      //
      M := L + 1;
      // Create Loop Object
      Loop := TEDISEFLoop.Create(Table);
      Table.EDISEFDataObjects.AddByNameOrId(Loop);
      Loop.SEFFile := SEFFile;
      Loop.LoopId := LoopId;
      Loop.MaximumRepeat := RepeatCount;
      Loop.Data := RepeatData;
      Loop.Disassemble;
      if Loop.LoopId = '' then
      begin
        ListItem := Loop.EDISEFDataObjects.First;
        Loop.LoopId := ListItem.EDISEFDataObject.Id;
      end;
    end; // if
  end; // while
end;

procedure ParseSetsDataOfSETSDefinition(Data: string; _Set: TEDISEFSet; SEFFile: TEDISEFFile);
var
  I, J: Integer;
  Table: TEDISEFTable;
  TableData: string;
begin
  _Set.FEDISEFDataObjects.Clear;
  I := StrSearch('=', Data, 1);
  _Set.Id := Copy(Data, 1, I - 1);
  while I > 0 do
  begin
    // Start search
    I := StrSearch('^', Data, I);
    J := StrSearch('^', Data, I + 1);
    if I = 0 then
    begin
      Table := TEDISEFTable.Create(_Set);
      _Set.FEDISEFDataObjects.AddByNameOrId(Table);
      Table.Data := Data;
      Table.Disassemble;
    end
    else
    begin
      if J = 0 then
      begin
        TableData := Copy(Data, I + 1, Length(Data) - I);
        I := 0;
      end
      else
      begin
        TableData := Copy(Data, I + 1, J - (I + 1));
        I := J;
      end;
      Table := TEDISEFTable.Create(_Set);
      _Set.FEDISEFDataObjects.AddByNameOrId(Table);
      Table.Data := TableData;
      Table.Disassemble;
    end; // if
  end; // while
end;

{ TEDISEFDataObject }

constructor TEDISEFDataObject.Create(Parent: TEDISEFDataObject);
begin
  inherited Create;
  FId := '';
  FData := '';
  FLength := 0;
  FParent := nil;
  FSEFFile := nil;
  if Assigned(Parent) then
  begin
    FParent := Parent;
    if Parent is TEDISEFFile then
      FSEFFile := TEDISEFFile(Parent)
    else
      FSEFFile := Parent.SEFFile;
  end;
end;

destructor TEDISEFDataObject.Destroy;
begin
  inherited;
end;

function TEDISEFDataObject.GetData: string;
begin
  Result := FData;
end;

procedure TEDISEFDataObject.SetData(const Data: string);
begin
  FData := Data;
  FLength := Length(FData);
end;

{ TEDISEFDataObjectListItem }

function TEDISEFDataObjectListItem.GetEDISEFDataObject: TEDISEFDataObject;
begin
  Result := TEDISEFDataObject(FEDIObject);
end;

procedure TEDISEFDataObjectListItem.SetEDISEFDataObject(const Value: TEDISEFDataObject);
begin
  FEDIObject := Value;
end;

{ TEDISEFDataObjectList }

procedure TEDISEFDataObjectList.AddByNameOrId(EDISEFDataObject: TEDISEFDataObject;
  Name: string = '');
var
  ListItem: TEDISEFDataObjectListItem;
begin
  ListItem := TEDISEFDataObjectListItem(CreateListItem(FLastItem, EDISEFDataObject));
  if Name <> '' then
    ListItem.Name := Name
  else
    ListItem.Name := EDISEFDataObject.Id;
  if FLastItem <> nil then
    FLastItem.NextItem := ListItem;
  if FFirstItem = nil then
    FFirstItem := ListItem;
  FLastItem := ListItem;
  FCurrentItem := ListItem;
  Inc(FCount);
end;

function TEDISEFDataObjectList.CreateListItem(PriorItem: TEDIObjectListItem;
  EDIObject: TEDIObject): TEDIObjectListItem;
begin
  Result := TEDISEFDataObjectListItem.Create(Self, PriorItem, EDIObject);
end;

function TEDISEFDataObjectList.FindItemByName(Name: string): TEDISEFDataObjectListItem;
var
  ListItem: TEDISEFDataObjectListItem;
begin
  Result := nil;
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

function TEDISEFDataObjectList.First(Index: Integer): TEDISEFDataObjectListItem;
begin
  Result := TEDISEFDataObjectListItem(inherited First(Index));
end;

function TEDISEFDataObjectList.GetEDISEFDataObject(Index: Integer): TEDISEFDataObject;
begin
  Result := TEDISEFDataObject(GetEDIObject(Index));
end;

{ TEDISEFDataObjectGroup }

constructor TEDISEFDataObjectGroup.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
  FEDISEFDataObjects := TEDISEFDataObjectList.Create;
end;

destructor TEDISEFDataObjectGroup.Destroy;
begin
  FEDISEFDataObjects.Free;
  inherited;
end;

function TEDISEFDataObjectList.GetObjectByItemByName(Name: string): TEDISEFDataObject;
var
  ListItem: TEDISEFDataObjectListItem;
begin
  Result := nil;
  ListItem := FindItemByName(Name);
  if ListItem <> nil then
    Result := FindItemByName(Name).EDISEFDataObject;
end;

function TEDISEFDataObjectList.Last: TEDISEFDataObjectListItem;
begin
  Result := TEDISEFDataObjectListItem(inherited Last);
end;

function TEDISEFDataObjectList.Next: TEDISEFDataObjectListItem;
begin
  Result := TEDISEFDataObjectListItem(inherited Next);
end;

function TEDISEFDataObjectList.Prior: TEDISEFDataObjectListItem;
begin
  Result := TEDISEFDataObjectListItem(inherited Prior);
end;

procedure TEDISEFDataObjectList.SetEDISEFDataObject(Index: Integer; const Value: TEDISEFDataObject);
begin
  SetEDIObject(Index, Value);
end;

{ TEDISEFElement }

function TEDISEFElement.Assemble: string;
begin
  Result := '';
  if FParent is TEDISEFFile then
    Result := CombineELMSDataOfELMSDefinition(Self)
  else if (FParent is TEDISEFCompositeElement) or (FParent is TEDISEFSegment) then
    Result := CombineELMSDataOfCOMSorSEGSDefinition(Self, FSEFFile.ELMS)
  else if FParent is TEDISEFRepeatingPattern then
  begin
    if (TEDISEFRepeatingPattern(FParent).BaseParent is TEDISEFCompositeElement) or
      (TEDISEFRepeatingPattern(FParent).BaseParent is TEDISEFSegment) then
    begin
      Result := CombineELMSDataOfCOMSorSEGSDefinition(Self, FSEFFile.ELMS);
    end;
  end;
end;

procedure TEDISEFElement.Assign(EDISEFElement: TEDISEFElement);
begin
  // FUserAttribute := EDISEFElement.UserAttribute;
  // FOrdinal := EDISEFElement.Ordinal;
  FId := EDISEFElement.ElementId;
  FElementType := EDISEFElement.ElementType;
  FMinimumLength := EDISEFElement.MinimumLength;
  FMaximumLength := EDISEFElement.MaximumLength;
  FRequirementDesignator := EDISEFElement.RequirementDesignator;
  FRepeatCount := EDISEFElement.RepeatCount;
end;

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

constructor TEDISEFElement.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
  FUserAttribute := '';
  FOrdinal := -1;
  FElementType := '';
  FMinimumLength := 0;
  FMaximumLength := 0;
  FRequirementDesignator := '';
  FRepeatCount := 0;
end;

destructor TEDISEFElement.Destroy;
begin
  inherited;
end;

procedure TEDISEFElement.Disassemble;
begin
  if FParent is TEDISEFFile then
    ParseELMSDataOfELMSDefinition(FData, Self)
  else if FParent is TEDISEFCompositeElement then
    ParseELMSDataOfCOMSDefinition(FData, Self, FSEFFile.ELMS)
  else if FParent is TEDISEFSegment then
    ParseELMSDataOfSEGSDefinition(FData, Self, FSEFFile.ELMS)
  else if FParent is TEDISEFRepeatingPattern then
  begin
    if TEDISEFRepeatingPattern(FParent).BaseParent is TEDISEFCompositeElement then
      ParseELMSDataOfCOMSDefinition(FData, Self, FSEFFile.ELMS)
    else if TEDISEFRepeatingPattern(FParent).BaseParent is TEDISEFSegment then
      ParseELMSDataOfSEGSDefinition(FData, Self, FSEFFile.ELMS);
  end;
end;

{ TEDISEFSubElement }

function TEDISEFSubElement.Assemble: string;
begin
  Result := inherited Assemble;
end;

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

constructor TEDISEFSubElement.Create(Parent: TEDISEFDataObject);
begin
  inherited;
end;

destructor TEDISEFSubElement.Destroy;
begin
  inherited;
end;

procedure TEDISEFSubElement.Disassemble;
begin
  inherited;
end;

{ TEDISEFCompositeElement }

function TEDISEFCompositeElement.Assemble: string;
begin
  Result := '';
  if FParent is TEDISEFFile then
    Result := CombineCOMSDataOfCOMSDefinition(Self)
  else if FParent is TEDISEFSegment then
    Result := CombineCOMSDataOfSEGSDefinition(Self)
  else if FParent is TEDISEFRepeatingPattern then
  begin
    if TEDISEFRepeatingPattern(FParent).BaseParent is TEDISEFSegment then
    begin
      Result := CombineCOMSDataOfSEGSDefinition(Self);
    end;
  end; // if
end;

procedure TEDISEFCompositeElement.Assign(CompositeElement: TEDISEFCompositeElement);
var
  ListItem: TEDISEFDataObjectListItem;
  SubElement: TEDISEFSubElement;
  RepeatingPattern: TEDISEFRepeatingPattern;
begin
  FEDISEFDataObjects.Clear;
  FUserAttribute := CompositeElement.UserAttribute;
  FId := CompositeElement.CompositeElementId;
  FOrdinal := CompositeElement.Ordinal;
  ListItem := CompositeElement.Elements.First;
  while ListItem <> nil do
  begin
    if ListItem.EDISEFDataObject <> nil then
    begin
      if ListItem.EDISEFDataObject is TEDISEFSubElement then
      begin
        SubElement := TEDISEFSubElement(ListItem.EDISEFDataObject);
        SubElement := SubElement.Clone;
        SubElement.Parent := Self;
        FEDISEFDataObjects.AddByNameOrId(SubElement);
      end
      else if ListItem.EDISEFDataObject is TEDISEFRepeatingPattern then
      begin
        RepeatingPattern := TEDISEFRepeatingPattern(ListItem.EDISEFDataObject);
        RepeatingPattern := RepeatingPattern.Clone;
        RepeatingPattern.Parent := Self;
        FEDISEFDataObjects.AddByNameOrId(RepeatingPattern);
      end; // if
    end
    else
    begin
      SubElement := TEDISEFSubElement.Create(Self);
      FEDISEFDataObjects.AddByNameOrId(SubElement);
    end;
    ListItem := CompositeElement.Elements.Next;
  end; // while
end;

function TEDISEFCompositeElement.Clone: TEDISEFCompositeElement;
var
  ListItem: TEDISEFDataObjectListItem;
  SubElement: TEDISEFSubElement;
  RepeatingPattern: TEDISEFRepeatingPattern;
begin
  Result := TEDISEFCompositeElement.Create(nil);
  Result.UserAttribute := FUserAttribute;
  Result.CompositeElementId := FId;
  Result.RequirementDesignator := FRequirementDesignator;
  Result.Ordinal := FOrdinal;
  ListItem := FEDISEFDataObjects.First;
  while ListItem <> nil do
  begin
    if ListItem.EDISEFDataObject <> nil then
    begin
      if ListItem.EDISEFDataObject is TEDISEFSubElement then
      begin
        SubElement := TEDISEFSubElement(ListItem.EDISEFDataObject);
        SubElement := SubElement.Clone;
        SubElement.Parent := Result;
        Result.Elements.AddByNameOrId(SubElement);
      end
      else if ListItem.EDISEFDataObject is TEDISEFRepeatingPattern then
      begin
        RepeatingPattern := TEDISEFRepeatingPattern(ListItem.EDISEFDataObject);
        RepeatingPattern := RepeatingPattern.Clone;
        RepeatingPattern.Parent := Result;
        Result.Elements.AddByNameOrId(RepeatingPattern);
      end;
    end
    else
    begin
      SubElement := TEDISEFSubElement.Create(Self);
      Result.Elements.AddByNameOrId(SubElement);
    end;
    ListItem := FEDISEFDataObjects.Next;
  end; // while
end;

constructor TEDISEFCompositeElement.Create(Parent: TEDISEFDataObject);
begin
  inherited;
end;

destructor TEDISEFCompositeElement.Destroy;
begin
  inherited;
end;

procedure TEDISEFCompositeElement.Disassemble;
begin
  if FParent is TEDISEFFile then
    ParseCOMSDataOfCOMSDefinition(FData, Self, FSEFFile.ELMS)
  else if FParent is TEDISEFSegment then
    ParseCOMSDataOfSEGSDefinition(FData, Self, FSEFFile.COMS)
  else if FParent is TEDISEFRepeatingPattern then
  begin
    if TEDISEFRepeatingPattern(FParent).BaseParent is TEDISEFSegment then
      ParseCOMSDataOfSEGSDefinition(FData, Self, FSEFFile.COMS);
  end; // if
end;

{ TEDISEFSegment }

function TEDISEFSegment.Assemble: string;
begin
  Result := '';
  if FParent is TEDISEFFile then
    Result := CombineSEGSDataOfSEGSDefinition(Self)
  else if (FParent is TEDISEFTable) or (FParent is TEDISEFLoop) then
    Result := CombineSEGSDataOfSETSDefinition(Self);
end;

procedure TEDISEFSegment.Assign(Segment: TEDISEFSegment);
var
  ListItem: TEDISEFDataObjectListItem;
  EDISEFDataObject: TEDISEFDataObject;
begin
  FEDISEFDataObjects.Clear;
  FRequirementDesignator := Segment.RequirementDesignator;
  FMaximumUse := Segment.MaximumUse;
  ListItem := Segment.Elements.First;
  while ListItem <> nil do
  begin
    EDISEFDataObject := nil;
    if ListItem.EDISEFDataObject <> nil then
    begin
      if ListItem.EDISEFDataObject is TEDISEFElement then
        EDISEFDataObject := TEDISEFElement(ListItem.EDISEFDataObject).Clone
      else if ListItem.EDISEFDataObject is TEDISEFCompositeElement then
        EDISEFDataObject := TEDISEFCompositeElement(ListItem.EDISEFDataObject).Clone
      else if ListItem.EDISEFDataObject is TEDISEFRepeatingPattern then
        EDISEFDataObject := TEDISEFRepeatingPattern(ListItem.EDISEFDataObject).Clone;
      EDISEFDataObject.Parent := Self;
    end
    else
      EDISEFDataObject := TEDISEFElement.Create(Self);
    FEDISEFDataObjects.AddByNameOrId(EDISEFDataObject);
    ListItem := Segment.Elements.Next;
  end; // while
end;

function TEDISEFSegment.Clone: TEDISEFSegment;
begin
  Result := nil;
end;

constructor TEDISEFSegment.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
  FRequirementDesignator := '';
  FMaximumUse := 0;
end;

destructor TEDISEFSegment.Destroy;
begin
  inherited;
end;

procedure TEDISEFSegment.Disassemble;
begin
  if FParent is TEDISEFFile then
    ParseSEGSDataOfSEGSDefinition(FData, Self, FSEFFile)
  else if (FParent is TEDISEFTable) or (FParent is TEDISEFLoop) then
    ParseSEGSDataOfSETSDefinition(FData, Self, FSEFFile);
end;

{ TEDISEFLoop }

function TEDISEFLoop.Assemble: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FEDISEFDataObjects.Count - 1 do
  begin
    if not (FEDISEFDataObjects[I] is TEDISEFLoop) then
      Result := Result + '[' + FEDISEFDataObjects[I].Assemble + ']'
    else
      Result := Result + '{' + FEDISEFDataObjects[I].Assemble + '}';
  end;
  if FEDISEFDataObjects.Count > 0 then
  begin
    if FEDISEFDataObjects[0].Id <> FId then
      Result := FId + ':' + IntToStr(FMaximumRepeat) + Result
    else
      Result := ':' + IntToStr(FMaximumRepeat) + Result;
  end
  else
    Result := FId + ':' + IntToStr(FMaximumRepeat) + Result;
end;

function TEDISEFLoop.Clone: TEDISEFLoop;
begin
  Result := nil;
end;

constructor TEDISEFLoop.Create(Parent: TEDISEFDataObject);
begin
  inherited;
  FMaximumRepeat := Value_UndefinedMaximum;
end;

destructor TEDISEFLoop.Destroy;
begin
  inherited;
end;

{ TEDISEFSection }

function TEDISEFTable.Assemble: string;
var
  I: Integer;
begin
  Result := '';
  if FEDISEFDataObjects.Count > 0 then
    Result := '^';
  for I := 0 to FEDISEFDataObjects.Count - 1 do
  begin
    if not (FEDISEFDataObjects[I] is TEDISEFLoop) then
      Result := Result + '[' + FEDISEFDataObjects[I].Assemble + ']'
    else
      Result := Result + '{' + FEDISEFDataObjects[I].Assemble + '}';
  end;
end;

function TEDISEFTable.Clone: TEDISEFTable;
begin
  Result := nil;
end;

constructor TEDISEFTable.Create(Parent: TEDISEFDataObject);
begin
  inherited;
end;

destructor TEDISEFTable.Destroy;
begin
  inherited;
end;

procedure TEDISEFLoop.Disassemble;
begin
  // FParent is TEDISEFTable
  ParseLoopDataOfSETSDefinition(FData, Self, FSEFFile);
end;

{ TEDISEFSet }

function TEDISEFSet.Assemble: string;
var
  I: Integer;
begin
  Result := FId + '=';
  for I := 0 to FEDISEFDataObjects.Count - 1 do
  begin
    Result := Result + FEDISEFDataObjects[I].Assemble;
  end;
end;

function TEDISEFSet.Clone: TEDISEFSet;
begin
  Result := nil;
end;

constructor TEDISEFSet.Create(Parent: TEDISEFDataObject);
begin
  inherited;
end;

destructor TEDISEFSet.Destroy;
begin
  inherited;
end;

procedure TEDISEFSet.Disassemble;
begin
  // FParent is TEDISEFFile
  ParseSetsDataOfSETSDefinition(FData, Self, FSEFFile);
end;

{ TEDISEFFile }

function TEDISEFFile.Assemble: string;
var
  I: Integer;
begin
  Result := '';
  Result := Result + SectionTag_VER + ' ' + FEDISEFVer + #13#10;
  Result := Result + SectionTag_INI + #13#10;
  Result := Result + FEDISEFIni.Text + #13#10;
  if FEDISEFStd.Text <> '' then
    Result := Result + SectionTag_STD + #13#10;
  Result := Result + FEDISEFStd.Text + #13#10;
  if FEDISEFSets.Count > 0 then
  begin
    Result := Result + SectionTag_SETS + #13#10;
    for I := 0 to FEDISEFSets.Count - 1 do
      Result := Result + FEDISEFSets[I].Assemble + #13#10;
  end;
  if FEDISEFSegs.Count > 0 then
  begin
    Result := Result + SectionTag_SEGS + #13#10;
    for I := 0 to FEDISEFSegs.Count - 1 do
      Result := Result + FEDISEFSegs[I].Assemble + #13#10;
  end;
  if FEDISEFComs.Count > 0 then
  begin
    Result := Result + SectionTag_COMS + #13#10;
    for I := 0 to FEDISEFComs.Count - 1 do
      Result := Result + FEDISEFComs[I].Assemble + #13#10;
  end;
  if FEDISEFElms.Count > 0 then
  begin
    Result := Result + SectionTag_ELMS + #13#10;
    for I := 0 to FEDISEFElms.Count - 1 do
      Result := Result + FEDISEFElms[I].Assemble + #13#10;
  end;
  if FEDISEFCodesList.Text <> '' then
  begin
    Result := Result + SectionTag_CODES + #13#10;
    Result := Result + FEDISEFCodesList.Text + #13#10;
  end;
  FData := Result;
end;

function TEDISEFFile.Clone: TEDISEFFile;
begin
  Result := nil;
end;

constructor TEDISEFFile.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(nil);
  FEDISEFCodesList := TStringList.Create;
  FEDISEFElms := TEDISEFDataObjectList.Create;
  FEDISEFComs := TEDISEFDataObjectList.Create;
  FEDISEFSegs := TEDISEFDataObjectList.Create;
  FEDISEFSets := TEDISEFDataObjectList.Create;
  FEDISEFStd := TStringList.Create;
  FEDISEFIni := TStringList.Create;
end;

destructor TEDISEFFile.Destroy;
begin
  FEDISEFIni.Free;
  FEDISEFStd.Free;  
  FEDISEFSets.Free;
  FEDISEFSegs.Free;
  FEDISEFComs.Free;
  FEDISEFElms.Free;
  FEDISEFCodesList.Free;
  inherited;
end;

procedure TEDISEFFile.Disassemble;
begin
  // Must parse file in reverse in order to build specification from the dictionary values
  // .CODES
  ParseCodes;
  // .ELMS
  ParseELMS;
  // .COMS
  ParseCOMS;
  // .SEGS
  ParseSEGS;
  // .SETS
  ParseSETS;
  // .STD
  ParseSTD;
  // .INI
  ParseINI;
  // .VER
  ParseVER;
end;

procedure TEDISEFFile.LoadFromFile(const FileName: string);
begin
  if FileName <> '' then
    FFileName := FileName;
  LoadFromFile;
end;

procedure TEDISEFFile.LoadFromFile;
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
    raise Exception.Create('Undefined Error')
end;

procedure TEDISEFFile.ParseCodes;
var
  SearchResult, SearchResult2: Integer;
begin
  FEDISEFCodesList.Clear;
  SearchResult := StrSearch(SectionTag_CODES, FData, 1);
  if SearchResult > 0 then
  begin
    SearchResult := SearchResult + Length(SectionTag_CODES + #13#10);
    SearchResult2 := StrSearch(#13#10 + '.', FData, SearchResult + 1);
    if SearchResult2 <> 0 then
      FEDISEFCodesList.Text := Copy(FData, SearchResult, SearchResult2 - SearchResult)
    else
      FEDISEFCodesList.Text := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
  end;
end;

procedure TEDISEFFile.ParseCOMS;
var
  TempList: TStrings;
  SearchResult, SearchResult2, I: Integer;
  CompositeElement: TEDISEFCompositeElement;
begin
  TempList := TStringList.Create;
  try
    FEDISEFComs.Clear;
    SearchResult := StrSearch(SectionTag_COMS, FData, 1);
    if SearchResult > 0 then
    begin
      SearchResult := SearchResult + Length(SectionTag_COMS + #13#10);
      SearchResult2 := StrSearch(#13#10 + '.', FData, SearchResult + 1);
      if SearchResult2 <> 0 then
        TempList.Text := Copy(FData, SearchResult, SearchResult2 - SearchResult)
      else
        TempList.Text := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
      for I := 0 to TempList.Count - 1 do
      begin
        CompositeElement := TEDISEFCompositeElement.Create(Self);
        CompositeElement.Data := TempList[I];
        CompositeElement.SEFFile := Self;
        if CompositeElement.Data <> '' then
        begin
          CompositeElement.Disassemble;
        end;
        FEDISEFComs.AddByNameOrId(CompositeElement, CompositeElement.Id);
      end; // for
    end;
  finally
    TempList.Free;
  end;
end;

procedure TEDISEFFile.ParseELMS;
var
  TempList: TStrings;
  SearchResult, SearchResult2, I: Integer;
  Element: TEDISEFElement;
begin
  TempList := TStringList.Create;
  try
    FEDISEFElms.Clear;
    SearchResult := StrSearch(SectionTag_ELMS, FData, 1);
    if SearchResult > 0 then
    begin
      SearchResult := SearchResult + Length(SectionTag_ELMS + #13#10);
      SearchResult2 := StrSearch(#13#10 + '.', FData, SearchResult + 1);
      if SearchResult2 <> 0 then
        TempList.Text := Copy(FData, SearchResult, SearchResult2 - SearchResult)
      else
        TempList.Text := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
      for I := 0 to TempList.Count - 1 do
      begin
        Element := TEDISEFElement.Create(Self);
        Element.Data := TempList[I];
        Element.SEFFile := Self;
        if Element.Data <> '' then
          Element.Disassemble;
        FEDISEFElms.AddByNameOrId(Element, Element.Id);
      end; // for
    end;
  finally
    TempList.Free;
  end;
end;

procedure TEDISEFFile.ParseINI;
var
  SearchResult, SearchResult2: Integer;
begin
  FEDISEFIni.Clear;
  {$IFDEF DELPHI6_UP}
  FEDISEFIni.Delimiter := ',';
  {$ELSE}

  {$ENDIF}
  SearchResult := StrSearch(SectionTag_INI, FData, 1);
  if SearchResult > 0 then
  begin
    SearchResult := SearchResult + Length(SectionTag_INI + #13#10);
    SearchResult2 := StrSearch(#13#10 + '.', FData, SearchResult + 1);
    if SearchResult2 <> 0 then
      FEDISEFIni.Text := Copy(FData, SearchResult, SearchResult2 - SearchResult)
    else
      FEDISEFIni.Text := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
  end;
end;

procedure TEDISEFFile.ParseSEGS;
var
  TempList: TStrings;
  SearchResult, SearchResult2, I: Integer;
  Segment: TEDISEFSegment;
begin
  TempList := TStringList.Create;
  try
    FEDISEFSegs.Clear;
    SearchResult := StrSearch(SectionTag_SEGS, FData, 1);
    if SearchResult > 0 then
    begin
      SearchResult := SearchResult + Length(SectionTag_SEGS + #13#10);
      SearchResult2 := StrSearch(#13#10 + '.', FData, SearchResult + 1);
      if SearchResult2 <> 0 then
        TempList.Text := Copy(FData, SearchResult, SearchResult2 - SearchResult)
      else
        TempList.Text := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
      for I := 0 to TempList.Count - 1 do
      begin
        Segment := TEDISEFSegment.Create(Self);
        Segment.Data := TempList[I];
        Segment.SEFFile := Self;
        if Segment.Data <> '' then
        begin
          Segment.Disassemble;
        end;
        FEDISEFSegs.AddByNameOrId(Segment, Segment.Id);
      end; // for
    end;
  finally
    TempList.Free;
  end;
end;

procedure TEDISEFFile.ParseSETS;
var
  TempList: TStrings;
  SearchResult, SearchResult2, I: Integer;
  TransactionSet: TEDISEFSet;
begin
  TempList := TStringList.Create;
  try
    FEDISEFSets.Clear;
    SearchResult := StrSearch(SectionTag_SETS, FData, 1);
    if SearchResult > 0 then
    begin
      SearchResult := SearchResult + Length(SectionTag_SETS + #13#10);
      SearchResult2 := StrSearch(#13#10 + '.', FData, SearchResult + 1);
      if SearchResult2 <> 0 then
        TempList.Text := Copy(FData, SearchResult, SearchResult2 - SearchResult)
      else
        TempList.Text := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
      for I := 0 to TempList.Count - 1 do
      begin
        TransactionSet := TEDISEFSet.Create(Self);
        TransactionSet.Data := TempList[I];
        TransactionSet.SEFFile := Self;
        if TransactionSet.Data <> '' then
        begin
          TransactionSet.Disassemble;
        end;
        FEDISEFSets.AddByNameOrId(TransactionSet, TransactionSet.Id);
      end; // for
    end;
  finally
    TempList.Free;
  end;
end;

procedure TEDISEFFile.ParseSTD;
var
  SearchResult, SearchResult2: Integer;
begin
  FEDISEFStd.Clear;
  {$IFDEF DELPHI6_UP}
  FEDISEFStd.Delimiter := ',';
  {$ELSE}

  {$ENDIF}
  SearchResult := StrSearch(SectionTag_STD, FData, 1);
  if SearchResult > 0 then
  begin
    SearchResult := SearchResult + Length(SectionTag_STD + #13#10);
    SearchResult2 := StrSearch(#13#10 + '.', FData, SearchResult + 1);
    if SearchResult2 <> 0 then
    begin
      {$IFDEF DELPHI6_UP}
      FEDISEFStd.DelimitedText := Copy(FData, SearchResult, SearchResult2 - SearchResult);
      {$ELSE}
      FEDISEFStd.Text := Copy(FData, SearchResult, SearchResult2 - SearchResult);
      {$ENDIF}
    end
    else
    begin
      {$IFDEF DELPHI6_UP}
      FEDISEFStd.DelimitedText := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
      {$ELSE}
      FEDISEFStd.Text := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
      {$ENDIF}
    end;
  end;
end;

procedure TEDISEFFile.ParseVER;
var
  SearchResult, SearchResult2: Integer;
begin
  FEDISEFVer := '';
  SearchResult := StrSearch(SectionTag_VER, FData, 1);
  if SearchResult > 0 then
  begin
    SearchResult := SearchResult + Length(SectionTag_VER);
    SearchResult2 := StrSearch(#13#10 + '.', FData, SearchResult + 1);
    if SearchResult2 <> 0 then
      FEDISEFVer := Copy(FData, SearchResult, SearchResult2 - SearchResult)
    else
      FEDISEFVer := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
    if FEDISEFVer = '' then
      FEDISEFVer := '1.0';
  end;
end;

procedure TEDISEFFile.SaveToFile(const FileName: string);
begin
  FFileName := FileName;
  SaveToFile;
end;

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
    raise Exception.Create('Undefined Error');
end;

procedure TEDISEFTable.Disassemble;
begin
  // FParent is TEDISEFSet
  ParseTableDataOfSETSDefinition(FData, Self, FSEFFile);
end;

procedure TEDISEFFile.Unload;
begin
  FEDISEFCodesList.Clear;
  FEDISEFElms.Clear;
  FEDISEFComs.Clear;
  FEDISEFSegs.Clear;
  FEDISEFSets.Clear;
  FEDISEFStd.Clear;
  FEDISEFIni.Clear;
  FEDISEFVer := '';
end;

{ TEDISEFRepeatingPattern }

function TEDISEFRepeatingPattern.Assemble: string;
var
  I: Integer;
begin
  Result := IntToStr(FRepeatCount);
  for I := 0 to FEDISEFDataObjects.Count - 1 do
  begin
    if not (FEDISEFDataObjects[I] is TEDISEFRepeatingPattern) then
      Result := Result + '[' + FEDISEFDataObjects[I].Assemble + ']'
    else
      Result := Result + '{' + FEDISEFDataObjects[I].Assemble + '}';
  end;
end;

function TEDISEFRepeatingPattern.Clone: TEDISEFRepeatingPattern;
var
  ListItem: TEDISEFDataObjectListItem;
  SEFDataObject: TEDISEFDataObject;
begin
  Result := TEDISEFRepeatingPattern.Create(nil);
  Result.Parent := nil;
  Result.RepeatCount := FRepeatCount;
  ListItem := FEDISEFDataObjects.First;
  while ListItem <> nil do
  begin
    if ListItem.EDISEFDataObject <> nil then
    begin
      SEFDataObject := ListItem.EDISEFDataObject;
      if ListItem.EDISEFDataObject is TEDISEFElement then
        SEFDataObject := TEDISEFElement(SEFDataObject).Clone
      else if ListItem.EDISEFDataObject is TEDISEFCompositeElement then
        SEFDataObject := TEDISEFCompositeElement(SEFDataObject).Clone
      else if ListItem.EDISEFDataObject is TEDISEFSegment then
        SEFDataObject := TEDISEFSegment(SEFDataObject).Clone
      else if ListItem.EDISEFDataObject is TEDISEFRepeatingPattern then
        SEFDataObject := TEDISEFRepeatingPattern(SEFDataObject).Clone;
      SEFDataObject.Parent := Result;
      Result.EDISEFDataObjects.AddByNameOrId(SEFDataObject);
    end; // if
    ListItem := FEDISEFDataObjects.Next;
  end; // while
end;

constructor TEDISEFRepeatingPattern.Create(Parent: TEDISEFDataObject);
begin
  inherited;
  if FBaseParent is TEDISEFRepeatingPattern then
    FBaseParent := TEDISEFRepeatingPattern(Parent).BaseParent
  else
    FBaseParent := Parent;
end;

destructor TEDISEFRepeatingPattern.Destroy;
begin
  inherited;
end;

procedure TEDISEFRepeatingPattern.Disassemble;
begin
  FEDISEFDataObjects.Clear;
  if FBaseParent is TEDISEFCompositeElement then
    InternalParseCOMSDataOfCOMSDefinition(FData, Self, FSEFFile.ELMS)
  else if FBaseParent is TEDISEFSegment then
    InternalParseSEGSDataOfSEGSDefinition(FData, Self, FSEFFile);
end;

end.
