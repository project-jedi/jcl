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
{ The Initial Developer of the Original Code is Raymond Alexander.                                 }
{ Portions created by Raymond Alexander are Copyright (C) Raymond Alexander. All rights reserved.  }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Raymond Alexander (rayspostbox3), Robert Marquardt, Robert Rossmair, Petr Vones                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ EDI Standard Exchange Format (*.sef) File Parser Unit                                            }
{                                                                                                  }
{ This unit is still in development                                                                }
{                                                                                                  }
{ Unit owner: Raymond Alexander                                                                    }
{ Date created: July, 20, 2003                                                                     }
{ Additional Info:                                                                                 }
{   E-Mail at RaysDelphiBox3@hotmail.com                                                           }
{   For latest EDI specific updates see http://sourceforge.net/projects/edisdk                     }
{   See home page for latest news & events and online help.                                        }
{                                                                                                  }
{**************************************************************************************************}

// $Id$

unit JclEDISEF;

{$I jcl.inc}

{$IFDEF SUPPORTS_WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF SUPPORTS_WEAKPACKAGEUNIT}

interface

uses
  SysUtils, Classes, Contnrs, 
  JclEDI;

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
  SectionTag_TEXTSETS = '.TEXT,SETS';
  SectionTag_ = '';
  //EDI SDK Specific Extensions
  SectionTag_EDISDK_SETSEXT = '.SETSEXT';
  SectionTag_EDISDK_SEGSEXT = '.SEGSEXT';
  SectionTag_EDISDK_COMSEXT = '.COMSEXT';
  SectionTag_EDISDK_ELMSEXT = '.ELMSEXT';

  Value_UndefinedMaximum = 99999;

  EDISEFUserAttributePeriod = '.';
  EDISEFUserAttributeExclamationPoint = '!';
  EDISEFUserAttributeDollarSign = '$';
  EDISEFUserAttributeHyphen = '-';
  EDISEFUserAttributeAmpersand = '&';

  EDISEFUserAttributePeriodDesc = 'Not Used';
  EDISEFUserAttributeExclamationPointDesc = 'Mandatory';
  EDISEFUserAttributeDollarSignDesc = 'Recommended';
  EDISEFUserAttributeHyphenDesc = 'Not Recommended';
  EDISEFUserAttributeAmpersandDesc = 'Dependent';

  EDISEFUserAttributeSet =
    [EDISEFUserAttributePeriod, EDISEFUserAttributeExclamationPoint,
     EDISEFUserAttributeDollarSign, EDISEFUserAttributeHyphen,
     EDISEFUserAttributeAmpersand];

resourcestring
  // Transaction Set:850
  SEFTextSetsCode_Set0_Desc = 'Transaction Set or message title.';
  SEFTextSetsCode_Set1_Desc = 'Transaction Set functional group (X12).';
  SEFTextSetsCode_Set2_Desc = 'Transaction Set or message purpose.';
  SEFTextSetsCode_Set3_Desc = 'Level 1 note on transaction set or message.';
  SEFTextSetsCode_Set4_Desc = 'Level 2 note on transaction set or message.';
  SEFTextSetsCode_Set5_Desc = 'Level 3 note on transaction set or message.';
  // Transaction Set~segment ordinal number: 850~1
  SEFTextSetsCode_Seg0_Desc = 'Segment reference notes that are part of the transaction set in X12.';
  SEFTextSetsCode_Seg1_Desc = 'Segment reference notes documented with the segment (like in VICS/UCS).';
  SEFTextSetsCode_Seg2_Desc = 'Segment reference comment documented with the transaction set.';
  SEFTextSetsCode_Seg3_Desc = 'Segment name.';
  SEFTextSetsCode_Seg4_Desc = 'Level 1 note on segment.';
  SEFTextSetsCode_Seg5_Desc = 'Level 2 note on segment.';
  SEFTextSetsCode_Seg6_Desc = 'Segment purpose.';
  SEFTextSetsCode_Seg7_Desc = 'Level 3 note on segment. See * below for other levels of notes.';
  // Transaction Set~segment ordinal number~element or composite ordinal number: 850~1~4
  SEFTextSetsCode_Elm0_Desc = 'Level 1 note on element or composite.';
  SEFTextSetsCode_Elm1_Desc = 'Level 2 note on element or composite.';
  SEFTextSetsCode_Elm2_Desc = 'Name of element or composite.';
  SEFTextSetsCode_Elm4_Desc = 'Level 3 note on element or composite.';

const
  // EDI SEF Text,Sets Constants
  SEFTextCR = '\r'; //carriage return
  SEFTextLF = '\n'; //line feed
  SEFTextCRLF = SEFTextCR + SEFTextLF;
  // Transaction Set:850
  SEFTextSetsCode_Set0 = '0'; // Transaction Set or message title.
  SEFTextSetsCode_Set1 = '1'; // Transaction Set functional group (X12).
  SEFTextSetsCode_Set2 = '2'; // Transaction Set or message purpose.
  SEFTextSetsCode_Set3 = '3'; // Level 1 note on transaction set or message.
  SEFTextSetsCode_Set4 = '4'; // Level 2 note on transaction set or message.
  SEFTextSetsCode_Set5 = '5'; // Level 3 note on transaction set or message. See * below for other levels of notes.
  // Transaction Set~segment ordinal number: 850~1
  SEFTextSetsCode_Seg0 = '0'; // Segment reference notes that are part of the transaction set in X12.
  SEFTextSetsCode_Seg1 = '1'; // Segment reference notes documented with the segment (like in VICS/UCS).
  SEFTextSetsCode_Seg2 = '2'; // Segment reference comment documented with the transaction set.
  SEFTextSetsCode_Seg3 = '3'; // Segment name.
  SEFTextSetsCode_Seg4 = '4'; // Level 1 note on segment.
  SEFTextSetsCode_Seg5 = '5'; // Level 2 note on segment.
  SEFTextSetsCode_Seg6 = '6'; // Segment purpose.
  SEFTextSetsCode_Seg7 = '7'; // Level 3 note on segment. See * below for other levels of notes.
  // Transaction Set~segment ordinal number~element or composite ordinal number: 850~1~4
  SEFTextSetsCode_Elm0 = '0'; // Level 1 note on element or composite.
  SEFTextSetsCode_Elm1 = '1'; // Level 2 note on element or composite.
  SEFTextSetsCode_Elm2 = '2'; // Name of element or composite.
  SEFTextSetsCode_Elm4 = '4'; // Level 3 note on element or composite. See * below for other levels of notes.

type
  TEDISEFComsUserAttributes =
    (caPeriod, caExclamationPoint, caDollarSign, caHyphen, caAmpersand);

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

  TEDISEFObjectParentType =
    (sefNil, sefList, sefElement, sefCompositeElement, sefSegment);

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
    procedure SetParent(const Value: TEDISEFDataObject); virtual;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; virtual; abstract;
    procedure Disassemble; virtual; abstract;
    function Clone(NewParent: TEDISEFDataObject): TEDISEFDataObject; virtual; abstract;
  published
    property State: TEDIDataObjectDataState read FState;
    property Id: string read FId write FId;
    property Data: string read GetData write SetData;
    property DataLength: Integer read FLength;
    property Parent: TEDISEFDataObject read FParent write SetParent;
    property SEFFile: TEDISEFFile read FSEFFile write FSEFFile;
  end;

  TEDISEFDataObjectClass = class of TEDISEFDataObject;

//--------------------------------------------------------------------------------------------------
//  EDI SEF Data Object List Item
//--------------------------------------------------------------------------------------------------

  TEDISEFDataObjectListItem = class(TEDIObjectListItem)
  private
    function GetEDISEFDataObject: TEDISEFDataObject;
    procedure SetEDISEFDataObject(const Value: TEDISEFDataObject);
  published
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
    function FindItemByName(Name: string;
      StartItem: TEDIObjectListItem = nil): TEDISEFDataObjectListItem; reintroduce;
    function GetObjectByItemByName(Name: string): TEDISEFDataObject;
    //
    property EDISEFDataObject[Index: Integer]: TEDISEFDataObject read GetEDISEFDataObject
      write SetEDISEFDataObject; default;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI SEF Data Object Group
//--------------------------------------------------------------------------------------------------

  TEDISEFDataObjectGroup = class(TEDISEFDataObject)
  private
    function GetEDISEFDataObject(Index: Integer): TEDISEFDataObject;
    function GetCount: Integer;
  protected
    FEDISEFDataObjects: TEDISEFDataObjectList;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    property EDISEFDataObject[Index: Integer]: TEDISEFDataObject read GetEDISEFDataObject; default;
  published
    property EDISEFDataObjects: TEDISEFDataObjectList read FEDISEFDataObjects;
    property EDISEFDataObjectCount: Integer read GetCount;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI SEF Repeating Pattern
//--------------------------------------------------------------------------------------------------

  TEDISEFRepeatingPattern = class(TEDISEFDataObjectGroup)
  private
    FBaseParent: TEDISEFDataObject;
    FRepeatCount: Integer;
  protected
    procedure SetParent(const Value: TEDISEFDataObject); override;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    function Clone(NewParent: TEDISEFDataObject): TEDISEFRepeatingPattern; reintroduce;
  published
    property BaseParent: TEDISEFDataObject read FBaseParent;
    property RepeatCount: Integer read FRepeatCount write FRepeatCount;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI SEF Text Objects
//--------------------------------------------------------------------------------------------------

  TEDISEFWhereType = (twUnknown, twSet, twSegment, twElementOrCompositeElement, twSubElement);

  TEDISEFText = class(TEDIObject)
  private
    function GetText: string;
    procedure SetText(const Value: string);
  protected
    FData: string;
    FEDISEFWhereType: TEDISEFWhereType;
    FWhereLocation: TStrings;
    FWhere: string;
    FWhat: string;
    FText: string;
    function GetData: string;
    procedure SetData(const Value: string);
    function Assemble: string; virtual;
    procedure Disassemble; virtual;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Data: string read GetData write SetData;
    property WhereLocation: TStrings read FWhereLocation;
    property Where: string read FWhere;
    property What: string read FWhat;
    property Text: string read GetText write SetText;
  end;

  TEDISEFTextSet = class(TEDISEFText)
  private
    FWhereSet: string;
    FWhereSegment: Integer; // Ordinal
    FWhereElement: Integer; // Ordinal
    FWhereSubElement: Integer; // Ordinal
  public
    constructor Create;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
  end;

  TEDISEFTextSets = class(TEDIObjectList)
  public
    function GetText(Code: string): string;
    procedure SetText(EDISEFFile: TEDISEFFile; Location, Code, Text: string);
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
    FEDISEFTextSets: TEDISEFTextSets;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    procedure Assign(EDISEFElement: TEDISEFElement);
    function Clone(NewParent: TEDISEFDataObject): TEDISEFElement; reintroduce;
    function GetTextSetsLocation: string;
    procedure BindTextSets(TEXTSETS: TEDISEFTextSets);
  published
    property UserAttribute: string read FUserAttribute write FUserAttribute;
    property Ordinal: Integer read FOrdinal write FOrdinal;
    property ElementId: string read FId write FId;
    property ElementType: string read FElementType write FElementType;
    property MinimumLength: Integer read FMinimumLength write FMinimumLength;
    property MaximumLength: Integer read FMaximumLength write FMaximumLength;
    property RequirementDesignator: string read FRequirementDesignator write FRequirementDesignator;
    property RepeatCount: Integer read FRepeatCount write FRepeatCount;
    property TextSetsLocation: string read GetTextSetsLocation;
    //
    property TEXTSETS: TEDISEFTextSets read FEDISEFTextSets;
  end;

  TEDISEFSubElement = class(TEDISEFElement)
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    function Clone(NewParent: TEDISEFDataObject): TEDISEFSubElement; reintroduce;
  end;

  TEDISEFCompositeElement = class(TEDISEFDataObjectGroup)
  private
    FUserAttribute: string;
    FOrdinal: Integer;
    FRequirementDesignator: string;
    FRepeatCount: Integer;
    FEDISEFTextSets: TEDISEFTextSets;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    procedure Assign(CompositeElement: TEDISEFCompositeElement);
    function Clone(NewParent: TEDISEFDataObject): TEDISEFCompositeElement; reintroduce;
    function GetElementObjectList: TObjectList;
    procedure AssignElementOrdinals;
    function GetTextSetsLocation: string;
    procedure BindTextSets(TEXTSETS: TEDISEFTextSets);
  published
    property UserAttribute: string read FUserAttribute write FUserAttribute;
    property Ordinal: Integer read FOrdinal write FOrdinal;
    property CompositeElementId: string read FId write FId;
    property RequirementDesignator: string read FRequirementDesignator write FRequirementDesignator;
    property RepeatCount: Integer read FRepeatCount write FRepeatCount;
    property Elements: TEDISEFDataObjectList read FEDISEFDataObjects;
    property TextSetsLocation: string read GetTextSetsLocation;
    //
    property TEXTSETS: TEDISEFTextSets read FEDISEFTextSets;
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
    FOwnerLoopId: string;
    FParentLoopId: string;
    FParentSet: TEDISEFSet;
    FEDISEFTextSets: TEDISEFTextSets;
    function GetOwnerLoopId: string;
    function GetParentLoopId: string;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    procedure Assign(Segment: TEDISEFSegment);
    function Clone(NewParent: TEDISEFDataObject): TEDISEFSegment; reintroduce;
    function GetElementObjectList: TObjectList;
    procedure AssignElementOrdinals;
    procedure BindElementTextSets;
    function GetTextSetsLocation: string;
    procedure BindTextSets(TEXTSETS: TEDISEFTextSets);
  published
    property UserAttribute: string read FUserAttribute write FUserAttribute;
    property Ordinal: Integer read FOrdinal write FOrdinal;
    property SegmentId: string read FId write FId;
    property RequirementDesignator: string read FRequirementDesignator write FRequirementDesignator;
    property MaximumUse: Integer read FMaximumUse write FMaximumUse;
    property Elements: TEDISEFDataObjectList read FEDISEFDataObjects;
    property OwnerLoopId: string read GetOwnerLoopId;
    property ParentLoopId: string read GetParentLoopId;
    property TextSetsLocation: string read GetTextSetsLocation;
    property ParentSet: TEDISEFSet read FParentSet;
    //
    property TEXTSETS: TEDISEFTextSets read FEDISEFTextSets;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI SEF Loop
//--------------------------------------------------------------------------------------------------

  TEDISEFLoop = class(TEDISEFDataObjectGroup)
  private
    FMaximumRepeat: Integer;
    function GetParentLoopId: string;
    function GetBaseParentSet: TEDISEFSet;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    function Clone(NewParent: TEDISEFDataObject): TEDISEFLoop; reintroduce;
  published
    property LoopId: string read FId write FId;
    property MaximumRepeat: Integer read FMaximumRepeat write FMaximumRepeat;
    property ParentLoopId: string read GetParentLoopId;
    property BaseParentSet: TEDISEFSet read GetBaseParentSet;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI SEF Table
//--------------------------------------------------------------------------------------------------

  TEDISEFTable = class(TEDISEFDataObjectGroup)
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    function Clone(NewParent: TEDISEFDataObject): TEDISEFTable; reintroduce;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI SEF Set
//--------------------------------------------------------------------------------------------------

  TEDISEFSet = class(TEDISEFDataObjectGroup)
  private
    FEDISEFTextSets: TEDISEFTextSets;
    function GetEDISEFTable(Index: Integer): TEDISEFTable;
    procedure BuildSegmentObjectListFromLoop(ObjectList: TObjectList; Loop: TEDISEFLoop);
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    function Clone(NewParent: TEDISEFDataObject): TEDISEFSet; reintroduce;
    function GetSegmentObjectList: TObjectList;
    procedure AssignSegmentOrdinals;
    procedure BindSegmentTextSets;
    function GetTextSetsLocation: string;
    procedure BindTextSets(TEXTSETS: TEDISEFTextSets);
    property Table[Index: Integer]: TEDISEFTable read GetEDISEFTable;
  published
    property Tables: TEDISEFDataObjectList read FEDISEFDataObjects;
    property TextSetsLocation: string read GetTextSetsLocation;
    //
    property TEXTSETS: TEDISEFTextSets read FEDISEFTextSets;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI SEF File
//--------------------------------------------------------------------------------------------------

  TEDISEFFile = class(TEDISEFDataObject)
  private
    FFileName: string;
    FEDISEFTextSets: TEDISEFTextSets;
    FEDISEFCodesList: TStrings;
    FEDISEFElms: TEDISEFDataObjectList;
    FEDISEFComs: TEDISEFDataObjectList;
    FEDISEFSegs: TEDISEFDataObjectList;
    FEDISEFSets: TEDISEFDataObjectList;
    FEDISEFStd: TStrings;
    FEDISEFIni: TStrings;
    FEDISEFVer: string;
    procedure ParseTextSets;
    procedure ParseCodes;
    procedure ParseELMS;
    procedure ParseCOMS;
    procedure ParseSEGS;
    procedure ParseSETS;
    procedure ParseSTD;
    procedure ParseINI;
    procedure ParseVER;
    // EDI SDK SEF Extensions
    procedure ParseELMSExt;
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

    function Clone(NewParent: TEDISEFDataObject): TEDISEFFile; reintroduce;
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
    //
    property TEXTSETS: TEDISEFTextSets read FEDISEFTextSets;
  end;

//--------------------------------------------------------------------------------------------------
//  Procedures
//--------------------------------------------------------------------------------------------------

function GetEDISEFUserAttributeDescription(
  Attribute: TEDISEFComsUserAttributes): string; overload;
function GetEDISEFUserAttributeDescription(Attribute: string): string; overload;

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
procedure ParseSetsDataOfSETSDefinition(Data: string; Set_: TEDISEFSet; SEFFile: TEDISEFFile);

procedure ExtractFromDataObjectGroup(DataObjectClass: TEDISEFDataObjectClass;
  DataObjectGroup: TEDISEFDataObjectGroup; ObjectList: TObjectList); overload;

procedure ExtractFromDataObjectGroup(DataObjectClasses: array of TEDISEFDataObjectClass;
  DataObjectGroup: TEDISEFDataObjectGroup; ObjectList: TObjectList); overload;

implementation

uses
  JclResources, JclStrings;

const
  Value_Optional = 'O';
  Value_Conditional = 'C';
  Value_One = '1';
  Value_GreaterThanOne = '>1';
  Value_Version10 = '1.0';
  Value_QuestionMark = '?';

  SEFDelimiter_EqualSign = '=';
  SEFDelimiter_OpeningBrace = '{';
  SEFDelimiter_ClosingBrace = '}';
  SEFDelimiter_OpeningBracket = '[';
  SEFDelimiter_ClosingBracket = ']';
  SEFDelimiter_AtSign = '@';
  SEFDelimiter_SemiColon = ';';
  SEFDelimiter_Colon = ':';
  SEFDelimiter_Comma = ',';
  SEFDelimiter_Period = '.';
  SEFDelimiter_Caret = '^';
  SEFDelimiter_PlusSign = '+';
  SEFDelimiter_MinusSign = '-';
  SEFDelimiter_Asterisk = '*';  

//--------------------------------------------------------------------------------------------------
//  Procedures
//--------------------------------------------------------------------------------------------------

function GetEDISEFUserAttributeDescription(Attribute: TEDISEFComsUserAttributes): string;
begin
  case Attribute of
    caPeriod:
      Result := EDISEFUserAttributePeriodDesc;
    caExclamationPoint:
      Result := EDISEFUserAttributeExclamationPointDesc;
    caDollarSign:
      Result := EDISEFUserAttributeDollarSignDesc;
    caHyphen:
      Result := EDISEFUserAttributeHyphenDesc;
    caAmpersand:
      Result := EDISEFUserAttributeAmpersandDesc;
  else
    Result := RsUnknownAttribute;
  end;
end;

//--------------------------------------------------------------------------------------------------

function GetEDISEFUserAttributeDescription(Attribute: string): string;
begin
  if Attribute = '' then
    Attribute := Value_QuestionMark;
  case Attribute[1] of
    EDISEFUserAttributePeriod:
      Result := EDISEFUserAttributePeriodDesc;
    EDISEFUserAttributeExclamationPoint:
      Result := EDISEFUserAttributeExclamationPointDesc;
    EDISEFUserAttributeDollarSign:
      Result := EDISEFUserAttributeDollarSignDesc;
    EDISEFUserAttributeHyphen:
      Result := EDISEFUserAttributeHyphenDesc;
    EDISEFUserAttributeAmpersand:
      Result := EDISEFUserAttributeAmpersandDesc;
  else
    Result := RsUnknownAttribute;
  end;
end;

//--------------------------------------------------------------------------------------------------

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
  Element.RequirementDesignator := Value_Optional;
  Element.RepeatCount := 1;
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

//--------------------------------------------------------------------------------------------------

function CombineELMSDataOfELMSDefinition(Element: TEDISEFElement): string;
begin
  Result := Element.Id + SEFDelimiter_EqualSign + Element.ElementType + SEFDelimiter_Comma +
    IntToStr(Element.MinimumLength) + SEFDelimiter_Comma + IntToStr(Element.MaximumLength);
end;

//--------------------------------------------------------------------------------------------------

procedure ParseELMSDataOfCOMSDefinition(Data: string; Element: TEDISEFElement;
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
  Element.RequirementDesignator := Value_Optional;
  Element.RepeatCount := 1;
  // Parse User Attribute
  if Data[1] in EDISEFUserAttributeSet then
  begin
    Element.UserAttribute := Data[1];
    I := 2;
  end
  else
    I := 1;
  // Get delimiter locations
  J := StrSearch(SEFDelimiter_AtSign, Data, 1);
  K := StrSearch(SEFDelimiter_SemiColon, Data, 1);
  L := StrSearch(SEFDelimiter_Colon, Data, 1);
  M := StrSearch(SEFDelimiter_Comma, Data, 1);
  N := StrSearch(SEFDelimiter_Comma, Data, M + 1); 
  P := Length(Data) + 1;
  // Parse Id using the closest delimiter
  O := P;
  if J <> 0 then
    if O > J then
      O := J;
  if K <> 0 then
    if O > K then
      O := K;
  if L <> 0 then
    if O > L then
      O := L;
  if M <> 0 then
    if O > M then
      O := M;
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
    if K <> 0 then
      if O > K then
        O := K;
    if L <> 0 then
      if O > L then
        O := L;
    if M <> 0 then
      if O > M then
        O := M;
    if (O - J) > 0 then
      Element.Ordinal := StrToInt(Copy(Data, J, O - J));
  end;
  if K <> 0 then
  begin
    Inc(K);
    O := P;
    if L <> 0 then
      if O > L then
        O := L;
    if M <> 0 then
      if O > M then
        O := M;
    if (O - K) > 0 then
      Element.MinimumLength := StrToInt(Copy(Data, K, O - K));
  end;
  if L <> 0 then
  begin
    Inc(L);
    O := P;
    if M <> 0 then
      if O > M then
        O := M;
    if (O - L) > 0 then
      Element.MaximumLength := StrToInt(Copy(Data, L, O - L));
  end;
  if M <> 0 then
  begin
    Inc(M);
    O := P;
    if N <> 0 then
      if O > N then
        O := N;
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

//--------------------------------------------------------------------------------------------------

function CombineELMSDataOfCOMSorSEGSDefinition(Element: TEDISEFElement;
  ELMSList: TEDISEFDataObjectList): string;
var
  CompareElement: TEDISEFElement;
  ListItem: TEDISEFDataObjectListItem;
begin
  if Element.UserAttribute <> '' then
    Result := Result + Element.UserAttribute;
  Result := Result + Element.Id;
// ToDo: Only assign if flagged
//  if Element.Ordinal <> -1 then
//    Result := Result + SEFDelimiter_AtSign + IntToStr(Element.Ordinal);
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
      Result := Result + SEFDelimiter_SemiColon;
      if CompareElement.MinimumLength <> Element.MinimumLength then
        Result := Result + IntToStr(Element.MinimumLength);
      Result := Result + SEFDelimiter_Colon;
      if CompareElement.MaximumLength <> Element.MaximumLength then
        Result := Result + IntToStr(Element.MaximumLength);
    end;
  end
  else
  begin
    Result := Result + SEFDelimiter_SemiColon;
    Result := Result + IntToStr(Element.MinimumLength);
    Result := Result + SEFDelimiter_Colon;
    Result := Result + IntToStr(Element.MaximumLength);
  end;
  if (Element.RequirementDesignator <> '') and
    (Element.RequirementDesignator <> Value_Optional) then
    Result := Result + SEFDelimiter_Comma + Element.RequirementDesignator;
  if Element.RepeatCount > 1 then
  begin
    if (Element.RequirementDesignator = '') or
      (Element.RequirementDesignator = Value_Optional) then
      Result := Result + SEFDelimiter_Comma;
    Result := Result + SEFDelimiter_Comma + IntToStr(Element.RepeatCount);
  end;
end;

//--------------------------------------------------------------------------------------------------

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
  Element.RequirementDesignator := Value_Optional;
  Element.RepeatCount := 1;
  // Parse User Attribute
  if Data[1] in EDISEFUserAttributeSet then
  begin
    Element.UserAttribute := Data[1];
    I := 2;
  end
  else
    I := 1;
  // Get delimiter locations
  J := StrSearch(SEFDelimiter_AtSign, Data, 1);
  K := StrSearch(SEFDelimiter_SemiColon, Data, 1);
  L := StrSearch(SEFDelimiter_Colon, Data, 1);
  M := StrSearch(SEFDelimiter_Comma, Data, 1);
  N := StrSearch(SEFDelimiter_Comma, Data, M + 1);
  P := Length(Data) + 1;
  // Parse Id
  O := P;
  if J <> 0 then
    if O > J then
      O := J;
  if K <> 0 then
    if O > K then
      O := K;
  if L <> 0 then
    if O > L then
      O := L;
  if M <> 0 then
    if O > M then
      O := M;
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
    if K <> 0 then
      if O > K then
        O := K;
    if L <> 0 then
      if O > L then
        O := L;
    if M <> 0 then
      if O > M then
        O := M;
    if (O - J) > 0 then
      Element.Ordinal := StrToInt(Copy(Data, J, O - J));
  end;
  if K <> 0 then
  begin
    Inc(K);
    O := P;
    if L <> 0 then
      if O > L then
        O := L;
    if M <> 0 then
      if O > M then
        O := M;
    if (O - K) > 0 then
      Element.MinimumLength := StrToInt(Copy(Data, K, O - K));
  end;
  if L <> 0 then
  begin
    Inc(L);
    O := P;
    if M <> 0 then
      if O > M then
        O := M;
    if (O - L) > 0 then
      Element.MaximumLength := StrToInt(Copy(Data, L, O - L));
  end;
  if M <> 0 then
  begin
    Inc(M);
    O := P;
    if N <> 0 then
      if O > N then
        O := N;
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

//--------------------------------------------------------------------------------------------------

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
  I := StrSearch(SEFDelimiter_EqualSign, Data, 1);
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
    I := StrSearch(SEFDelimiter_OpeningBracket, Data, M);
    if I = 0 then
      Break;
    J := StrSearch(SEFDelimiter_ClosingBracket, Data, M);
    K := StrSearch(SEFDelimiter_OpeningBrace, Data, M);
    L := StrSearch(SEFDelimiter_ClosingBrace, Data, M);
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
      N := StrSearch(SEFDelimiter_OpeningBracket, Data, K);
      RepeatCount := StrToInt(Copy(Data, K + 1, (N - K) - 1));
      // Correct start position
      K := StrSearch(SEFDelimiter_OpeningBracket, Data, K);
      // Validate end position
      N := StrSearch(SEFDelimiter_OpeningBrace, Data, K + 1);
      while (N <> 0) and (N < L) do // Detect nested repetition
      begin
        N := StrSearch(SEFDelimiter_OpeningBrace, Data, N + 1); // Search for nested repetition
        L := StrSearch(SEFDelimiter_ClosingBrace, Data, L + 1); // Correct end position
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
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure ParseCOMSDataOfCOMSDefinition(Data: string; CompositeElement: TEDISEFCompositeElement;
  ELMSList: TEDISEFDataObjectList);
begin
  InternalParseCOMSDataOfCOMSDefinition(Data, CompositeElement, ELMSList);
end;

//--------------------------------------------------------------------------------------------------

function CombineCOMSDataOfCOMSDefinition(CompositeElement: TEDISEFCompositeElement): string;
var
  I: Integer;
begin
  Result := CompositeElement.Id + SEFDelimiter_EqualSign;
  for I := 0 to CompositeElement.Elements.Count - 1 do
  begin
    if not (CompositeElement.Elements[I] is TEDISEFRepeatingPattern) then
      Result := Result + SEFDelimiter_OpeningBracket + CompositeElement.Elements[I].Assemble +
        SEFDelimiter_ClosingBracket
    else
      Result := Result + SEFDelimiter_OpeningBrace + CompositeElement.Elements[I].Assemble +
        SEFDelimiter_ClosingBrace;
  end;
end;

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

function CombineCOMSDataOfSEGSDefinition(CompositeElement: TEDISEFCompositeElement): string;
begin
  if CompositeElement.UserAttribute <> '' then
    Result := Result + CompositeElement.UserAttribute;
  Result := Result + CompositeElement.Id;
// ToDo: Only assign if flagged
//  if CompositeElement.Ordinal > 0 then
//    Result := Result + SEFDelimiter_AtSign + IntToStr(CompositeElement.Ordinal);
  if (CompositeElement.RequirementDesignator <> '') and
    (CompositeElement.RequirementDesignator <> Value_Optional) then
  begin
    Result := Result + SEFDelimiter_Comma + CompositeElement.RequirementDesignator;
  end;
end;

//--------------------------------------------------------------------------------------------------

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
  I := StrSearch(SEFDelimiter_EqualSign, Data, 1);
  if (I > 0) and (Segment is TEDISEFSegment) then
  begin
    Segment.EDISEFDataObjects.Clear;
    Segment.Id := Copy(Data, 1, I - 1);
    TEDISEFSegment(Segment).RequirementDesignator := Value_Optional;
    TEDISEFSegment(Segment).MaximumUse := 1;
  end;
  Inc(I);
  M := I;
  while I > 0 do
  begin
    // Start search
    I := StrSearch(SEFDelimiter_OpeningBracket, Data, M);
    if I = 0 then
      Break;
    J := StrSearch(SEFDelimiter_ClosingBracket, Data, M);
    K := StrSearch(SEFDelimiter_OpeningBrace, Data, M);
    L := StrSearch(SEFDelimiter_ClosingBrace, Data, M);
    // Determine if data block to process is a repetition
    if (I < K) or (K = 0) then // Normal data block
    begin
      ElementData := Copy(Data, I + 1, (J - I) - 1);
      if ElementData[1] = Value_Conditional then
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
      end;
      M := J + 1;
    end
    else // Repeating data block (K = 1 on the first pass)
    begin
      // Get repeat count
      N := StrSearch(SEFDelimiter_OpeningBracket, Data, K);
      RepeatCount := StrToInt(Copy(Data, K + 1, (N - K) - 1));
      // Correct start position
      K := StrSearch(SEFDelimiter_OpeningBracket, Data, K);
      // Validate end position
      N := StrSearch(SEFDelimiter_OpeningBrace, Data, K + 1);
      while (N <> 0) and (N < L) do // Detect nested repetition
      begin
        N := StrSearch(SEFDelimiter_OpeningBrace, Data, N + 1); // Search for nested repetition
        L := StrSearch(SEFDelimiter_ClosingBrace, Data, L + 1); // Correct end position
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
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure ParseSEGSDataOfSEGSDefinition(Data: string; Segment: TEDISEFSegment;
  SEFFile: TEDISEFFile);
begin
  InternalParseSEGSDataOfSEGSDefinition(Data, Segment, SEFFile);
end;

//--------------------------------------------------------------------------------------------------

function CombineSEGSDataOfSEGSDefinition(Segment: TEDISEFSegment): string;
var
  I: Integer;
begin
  Result := Segment.Id + SEFDelimiter_EqualSign;
  for I := 0 to Segment.Elements.Count - 1 do
  begin
    if not (Segment.Elements[I] is TEDISEFRepeatingPattern) then
      Result := Result + SEFDelimiter_OpeningBracket + Segment.Elements[I].Assemble +
        SEFDelimiter_ClosingBracket
    else
      Result := Result + SEFDelimiter_OpeningBrace + Segment.Elements[I].Assemble +
        SEFDelimiter_ClosingBrace;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure ParseSEGSDataOfSETSDefinition(Data: string; Segment: TEDISEFSegment;
  SEFFile: TEDISEFFile);
var
  Temp: TStrings;
  ListItem: TEDISEFDataObjectListItem;
  SegmentDef: TEDISEFSegment;
  I, J, K: Integer;
begin
  Segment.UserAttribute := '';
  Segment.Ordinal := -1;
  Segment.RequirementDesignator := Value_Optional;
  Segment.MaximumUse := 1;
  Segment.EDISEFDataObjects.Clear;
  Temp := TStringList.Create;
  try
    Temp.CommaText := Data;
    if Temp.Count >= 1 then
    begin
      I := 1;
      // Parse User Attribute
      if Temp[0][1] in EDISEFUserAttributeSet then
      begin
        Segment.UserAttribute := Temp[0][1];
        I := 2;
      end;
      J := StrSearch(SEFDelimiter_Asterisk, Temp[0], 1);
      K := StrSearch(SEFDelimiter_AtSign, Temp[0], 1);
      if (K < J) and (K <> 0) then
        J := K;
      if J = 0 then
        Segment.Id := Temp[0]
      else
        Segment.Id := Copy(Temp[0], I, J - I);
    end;
    ListItem := SEFFile.SEGS.FindItemByName(Segment.Id);
    if (ListItem <> nil) and (ListItem.EDISEFDataObject <> nil) then
    begin
      SegmentDef := TEDISEFSegment(ListItem.EDISEFDataObject);
      Segment.Assign(SegmentDef);
    end;
    if Temp.Count >= 2 then
    begin
      if Temp[1] = '' then
        Temp[1] := Value_Optional;
      Segment.RequirementDesignator := Temp[1];
    end;
    if Temp.Count >= 3 then
    begin
      if Temp[2] = Value_GreaterThanOne then
        Temp[2] := IntToStr(Value_UndefinedMaximum);
      Segment.MaximumUse := StrToInt(Temp[2]);
    end;
  finally
    Temp.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

function CombineSEGSDataOfSETSDefinition(Segment: TEDISEFSegment): string;
begin
  if Segment.UserAttribute <> '' then
    Result := Result + Segment.UserAttribute;
  Result := Result + Segment.Id;
// ToDo: Only assign if flagged
//  if Segment.Ordinal > 0 then
//    Result := Result + SEFDelimiter_AtSign + IntToStr(Segment.Ordinal);
  if (Segment.RequirementDesignator <> '') and
    (Segment.RequirementDesignator <> Value_Optional) then
    Result := Result + SEFDelimiter_Comma + Segment.RequirementDesignator;
  if Segment.MaximumUse > 1 then
  begin
    if (Segment.RequirementDesignator = '') or
      (Segment.RequirementDesignator = Value_Optional) then
      Result := Result + SEFDelimiter_Comma;
    if Segment.MaximumUse = Value_UndefinedMaximum then
      Result := Result + SEFDelimiter_Comma + Value_GreaterThanOne 
    else
      Result := Result + SEFDelimiter_Comma + IntToStr(Segment.MaximumUse);
  end;
end;

//--------------------------------------------------------------------------------------------------

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
    I := StrSearch(SEFDelimiter_OpeningBracket, Data, M);
    if I = 0 then
      Break;
    J := StrSearch(SEFDelimiter_ClosingBracket, Data, M);
    K := StrSearch(SEFDelimiter_OpeningBrace, Data, M);
    L := StrSearch(SEFDelimiter_ClosingBrace, Data, M);
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
      N := StrSearch(SEFDelimiter_OpeningBracket, Data, K);
      J := StrSearch(SEFDelimiter_PlusSign, Data, K);
      M := StrSearch(SEFDelimiter_MinusSign, Data, K);
      // Adjustments
      if (J < N) and (J <> 0) then
        N := J;
      if (M < N) and (M <> 0) then
        N := M;
      // Get Loop Id: <Id>:<Repeat>+<Number>[<Id>]..."
      RepeatData := Copy(Data, K + 1, (N - K) - 1);
      J := StrSearch(SEFDelimiter_Colon, RepeatData, 1);
      if J = 0 then
      begin
        LoopId := RepeatData;
        RepeatData := '';
      end
      else
      begin
        LoopId := Copy(RepeatData, 1, J - 1);
        RepeatData := Copy(RepeatData, J + 1, Length(RepeatData) - J);
      end;
      // Get Repeat Count
      if RepeatData = Value_GreaterThanOne then
        RepeatData := IntToStr(Value_UndefinedMaximum);
      if RepeatData = '' then
        RepeatData := Value_One;
      RepeatCount := StrToInt(RepeatData);
      // Correct start position
      K := StrSearch(SEFDelimiter_OpeningBracket, Data, K);
      // Validate end position
      N := StrSearch(SEFDelimiter_OpeningBrace, Data, K + 1);
      while (N <> 0) and (N < L) do // Detect nested repetition
      begin
        N := StrSearch(SEFDelimiter_OpeningBrace, Data, N + 1); // Search for nested repetition
        L := StrSearch(SEFDelimiter_ClosingBrace, Data, L + 1); // Correct end position
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
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

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
    I := StrSearch(SEFDelimiter_OpeningBracket, Data, M);
    if I = 0 then
      Break;
    J := StrSearch(SEFDelimiter_ClosingBracket, Data, M);
    K := StrSearch(SEFDelimiter_OpeningBrace, Data, M);
    L := StrSearch(SEFDelimiter_ClosingBrace, Data, M);
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
      N := StrSearch(SEFDelimiter_OpeningBracket, Data, K);
      J := StrSearch(SEFDelimiter_PlusSign, Data, K);
      M := StrSearch(SEFDelimiter_MinusSign, Data, K);
      // Adjustments
      if (J < N) and (J <> 0) then
        N := J;
      if (M < N) and (M <> 0) then
        N := M;
      // Get Loop Id: <Id>:<Repeat><+or-><Number>[<Id>]..."
      RepeatData := Copy(Data, K + 1, (N - K) - 1);
      J := StrSearch(SEFDelimiter_Colon, RepeatData, 1);
      if J = 0 then
      begin
        LoopId := RepeatData;
        RepeatData := '';
      end
      else
      begin
        LoopId := Copy(RepeatData, 1, J - 1);
        RepeatData := Copy(RepeatData, J + 1, Length(RepeatData) - J);
      end;
      // Get Repeat Count
      if RepeatData = Value_GreaterThanOne then
        RepeatData := IntToStr(Value_UndefinedMaximum);
      if RepeatData = '' then
        RepeatData := Value_One;
      RepeatCount := StrToInt(RepeatData);
      // Correct start position (Move to first "[<Id>]")
      K := StrSearch(SEFDelimiter_OpeningBracket, Data, K);
      // Validate end position
      N := StrSearch(SEFDelimiter_OpeningBrace, Data, K + 1);
      while (N <> 0) and (N < L) do // Detect nested repetition
      begin
        N := StrSearch(SEFDelimiter_OpeningBrace, Data, N + 1); // Search for nested repetition
        L := StrSearch(SEFDelimiter_ClosingBrace, Data, L + 1); // Correct end position
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
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure ParseSetsDataOfSETSDefinition(Data: string; Set_: TEDISEFSet; SEFFile: TEDISEFFile);
var
  I, J: Integer;
  Table: TEDISEFTable;
  TableData: string;
begin
  Set_.EDISEFDataObjects.Clear;
  I := StrSearch(SEFDelimiter_EqualSign, Data, 1);
  Set_.Id := Copy(Data, 1, I - 1);
  while I > 0 do
  begin
    // Start search
    I := StrSearch(SEFDelimiter_Caret, Data, I);
    J := StrSearch(SEFDelimiter_Caret, Data, I + 1);
    if I = 0 then
    begin
      Table := TEDISEFTable.Create(Set_);
      Set_.EDISEFDataObjects.AddByNameOrId(Table);
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
      Table := TEDISEFTable.Create(Set_);
      Set_.EDISEFDataObjects.AddByNameOrId(Table);
      Table.Data := TableData;
      Table.Disassemble;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure ExtractFromDataObjectGroup(DataObjectClass: TEDISEFDataObjectClass;
  DataObjectGroup: TEDISEFDataObjectGroup; ObjectList: TObjectList);
var
  I: Integer;
  RepeatingPattern: TEDISEFRepeatingPattern;
begin
  for I := 0 to DataObjectGroup.EDISEFDataObjectCount - 1 do
  begin
    if DataObjectGroup[I] is DataObjectClass then
      ObjectList.Add(DataObjectGroup[I])
    else
    if DataObjectGroup[I] is TEDISEFRepeatingPattern then
    begin
      RepeatingPattern := TEDISEFRepeatingPattern(DataObjectGroup[I]);
      ExtractFromDataObjectGroup(DataObjectClass, RepeatingPattern, ObjectList);
    end; //if
  end; //for I
end;

procedure ExtractFromDataObjectGroup(DataObjectClasses: array of TEDISEFDataObjectClass;
  DataObjectGroup: TEDISEFDataObjectGroup; ObjectList: TObjectList); overload;
var
  ClassCount: Integer;
  I, J: Integer;
  ClassMatch: Boolean;
  RepeatingPattern: TEDISEFRepeatingPattern;
begin
  ClassCount := Length(DataObjectClasses);
  for I := 0 to DataObjectGroup.EDISEFDataObjectCount - 1 do
  begin
    ClassMatch := False;
    for J := 0 to ClassCount - 1 do
    begin
      if DataObjectGroup[I] is DataObjectClasses[J] then
      begin
        ClassMatch := True;
        Break;
      end; //if
    end; //for J
    if ClassMatch then
      ObjectList.Add(DataObjectGroup[I])
    else
    if DataObjectGroup[I] is TEDISEFRepeatingPattern then
    begin
      RepeatingPattern := TEDISEFRepeatingPattern(DataObjectGroup[I]);
      ExtractFromDataObjectGroup(DataObjectClasses, RepeatingPattern, ObjectList);
    end; //if
  end; //for I
end;

//==================================================================================================
// TEDISEFDataObject
//==================================================================================================

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

//--------------------------------------------------------------------------------------------------

procedure TEDISEFDataObject.SetParent(const Value: TEDISEFDataObject);
begin
  FParent := Value;
  if FParent is TEDISEFFile then
    FSEFFile := TEDISEFFile(FParent)
  else
    FSEFFile := FParent.SEFFile;
end;

//==================================================================================================
// TEDISEFDataObjectListItem
//==================================================================================================

{ TEDISEFDataObjectListItem }

function TEDISEFDataObjectListItem.GetEDISEFDataObject: TEDISEFDataObject;
begin
  Result := TEDISEFDataObject(FEDIObject);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFDataObjectListItem.SetEDISEFDataObject(const Value: TEDISEFDataObject);
begin
  FEDIObject := Value;
end;

//==================================================================================================
// TEDISEFDataObjectList
//==================================================================================================

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

//--------------------------------------------------------------------------------------------------

function TEDISEFDataObjectList.CreateListItem(PriorItem: TEDIObjectListItem;
  EDIObject: TEDIObject): TEDIObjectListItem;
begin
  Result := TEDISEFDataObjectListItem.Create(Self, PriorItem, EDIObject);
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFDataObjectList.FindItemByName(Name: string;
  StartItem: TEDIObjectListItem = nil): TEDISEFDataObjectListItem;
var
  ListItem: TEDIObjectListItem;
begin
  ListItem := inherited FindItemByName(Name, StartItem);
  Result := TEDISEFDataObjectListItem(ListItem);
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFDataObjectList.First(Index: Integer): TEDISEFDataObjectListItem;
begin
  Result := TEDISEFDataObjectListItem(inherited First(Index));
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFDataObjectList.GetEDISEFDataObject(Index: Integer): TEDISEFDataObject;
begin
  Result := TEDISEFDataObject(GetEDIObject(Index));
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFDataObjectList.GetObjectByItemByName(Name: string): TEDISEFDataObject;
var
  ListItem: TEDISEFDataObjectListItem;
begin
  Result := nil;
  ListItem := FindItemByName(Name);
  if ListItem <> nil then
    Result := FindItemByName(Name).EDISEFDataObject;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFDataObjectList.Last: TEDISEFDataObjectListItem;
begin
  Result := TEDISEFDataObjectListItem(inherited Last);
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFDataObjectList.Next: TEDISEFDataObjectListItem;
begin
  Result := TEDISEFDataObjectListItem(inherited Next);
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFDataObjectList.Prior: TEDISEFDataObjectListItem;
begin
  Result := TEDISEFDataObjectListItem(inherited Prior);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFDataObjectList.SetEDISEFDataObject(Index: Integer; const Value: TEDISEFDataObject);
begin
  SetEDIObject(Index, Value);
end;

//==================================================================================================
// TEDISEFDataObjectGroup
//==================================================================================================

{ TEDISEFDataObjectGroup }

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

function TEDISEFDataObjectGroup.GetCount: Integer;
begin
  Result := FEDISEFDataObjects.Count;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFDataObjectGroup.GetEDISEFDataObject(Index: Integer): TEDISEFDataObject;
begin
  Result := FEDISEFDataObjects[Index];
end;

//==================================================================================================
// TEDISEFElement
//==================================================================================================

{ TEDISEFElement }

constructor TEDISEFElement.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
  FUserAttribute := '';
  FOrdinal := -1;
  FElementType := '';
  FMinimumLength := 0;
  FMaximumLength := 0;
  FRequirementDesignator := '';
  FRepeatCount := -1;
  FEDISEFTextSets := TEDISEFTextSets.Create(False);
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISEFElement.Destroy;
begin
  FEDISEFTextSets.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFElement.Assemble: string;
begin
  Result := '';
  if FParent is TEDISEFFile then
    Result := CombineELMSDataOfELMSDefinition(Self)
  else
  if (FParent is TEDISEFCompositeElement) or (FParent is TEDISEFSegment) then
    Result := CombineELMSDataOfCOMSorSEGSDefinition(Self, FSEFFile.ELMS)
  else
  if FParent is TEDISEFRepeatingPattern then
  begin
    if (TEDISEFRepeatingPattern(FParent).BaseParent is TEDISEFCompositeElement) or
      (TEDISEFRepeatingPattern(FParent).BaseParent is TEDISEFSegment) then
    begin
      Result := CombineELMSDataOfCOMSorSEGSDefinition(Self, FSEFFile.ELMS);
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

function TEDISEFElement.Clone(NewParent: TEDISEFDataObject): TEDISEFElement;
begin
  Result := TEDISEFElement.Create(NewParent);
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
begin
  if FParent is TEDISEFFile then
    ParseELMSDataOfELMSDefinition(FData, Self)
  else
  if FParent is TEDISEFCompositeElement then
    ParseELMSDataOfCOMSDefinition(FData, Self, FSEFFile.ELMS)
  else
  if FParent is TEDISEFSegment then
    ParseELMSDataOfSEGSDefinition(FData, Self, FSEFFile.ELMS)
  else
  if FParent is TEDISEFRepeatingPattern then
  begin
    if TEDISEFRepeatingPattern(FParent).BaseParent is TEDISEFCompositeElement then
      ParseELMSDataOfCOMSDefinition(FData, Self, FSEFFile.ELMS)
    else
    if TEDISEFRepeatingPattern(FParent).BaseParent is TEDISEFSegment then
      ParseELMSDataOfSEGSDefinition(FData, Self, FSEFFile.ELMS);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFElement.GetTextSetsLocation: string;
var
  DataObject: TEDISEFDataObject;
begin
  Result := '';
  if FParent is TEDISEFCompositeElement then
    Result := TEDISEFCompositeElement(FParent).GetTextSetsLocation
  else
  if FParent is TEDISEFSegment then
    Result := TEDISEFSegment(FParent).GetTextSetsLocation
  else
  if FParent is TEDISEFRepeatingPattern then
  begin
    DataObject := TEDISEFRepeatingPattern(FParent).BaseParent;
    if DataObject is TEDISEFCompositeElement then
      Result := TEDISEFCompositeElement(FParent).GetTextSetsLocation
    else
    if DataObject is TEDISEFSegment then
      Result := TEDISEFSegment(DataObject).GetTextSetsLocation;
  end; //if
  if Result <> '' then
    Result := Result + '~' + IntToStr(FOrdinal);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFElement.BindTextSets(TEXTSETS: TEDISEFTextSets);
begin
  FEDISEFTextSets.Free;
  FEDISEFTextSets := TEDISEFTextSets(TextSets.ReturnListItemsByName(GetTextSetsLocation));
end;

//==================================================================================================
// TEDISEFSubElement
//==================================================================================================

{ TEDISEFSubElement }

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
  Result := inherited Assemble;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFSubElement.Clone(NewParent: TEDISEFDataObject): TEDISEFSubElement;
begin
  Result := TEDISEFSubElement.Create(NewParent);
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

//==================================================================================================
// TEDISEFCompositeElement
//==================================================================================================

{ TEDISEFCompositeElement }

constructor TEDISEFCompositeElement.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
  FUserAttribute := '';
  FOrdinal := -1;
  FRequirementDesignator := '';
  FEDISEFTextSets := TEDISEFTextSets.Create(False);
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISEFCompositeElement.Destroy;
begin
  FEDISEFTextSets.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFCompositeElement.Assemble: string;
begin
  Result := '';
  if FParent is TEDISEFFile then
    Result := CombineCOMSDataOfCOMSDefinition(Self)
  else
  if FParent is TEDISEFSegment then
    Result := CombineCOMSDataOfSEGSDefinition(Self)
  else
  if FParent is TEDISEFRepeatingPattern then
    if TEDISEFRepeatingPattern(FParent).BaseParent is TEDISEFSegment then
      Result := CombineCOMSDataOfSEGSDefinition(Self);
end;

//--------------------------------------------------------------------------------------------------

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
        SubElement := SubElement.Clone(Self);
        FEDISEFDataObjects.AddByNameOrId(SubElement);
      end
      else
      if ListItem.EDISEFDataObject is TEDISEFRepeatingPattern then
      begin
        RepeatingPattern := TEDISEFRepeatingPattern(ListItem.EDISEFDataObject);
        RepeatingPattern := RepeatingPattern.Clone(Self);
        FEDISEFDataObjects.AddByNameOrId(RepeatingPattern);
      end;
    end
    else
    begin
      SubElement := TEDISEFSubElement.Create(Self);
      FEDISEFDataObjects.AddByNameOrId(SubElement);
    end;
    ListItem := CompositeElement.Elements.Next;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFCompositeElement.Clone(NewParent: TEDISEFDataObject): TEDISEFCompositeElement;
var
  ListItem: TEDISEFDataObjectListItem;
  SubElement: TEDISEFSubElement;
  RepeatingPattern: TEDISEFRepeatingPattern;
begin
  Result := TEDISEFCompositeElement.Create(NewParent);
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
        SubElement := SubElement.Clone(Result);
        Result.Elements.AddByNameOrId(SubElement);
      end
      else
      if ListItem.EDISEFDataObject is TEDISEFRepeatingPattern then
      begin
        RepeatingPattern := TEDISEFRepeatingPattern(ListItem.EDISEFDataObject);
        RepeatingPattern := RepeatingPattern.Clone(Result);
        Result.Elements.AddByNameOrId(RepeatingPattern);
      end;
    end
    else
    begin
      SubElement := TEDISEFSubElement.Create(Self);
      Result.Elements.AddByNameOrId(SubElement);
    end;
    ListItem := FEDISEFDataObjects.Next;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFCompositeElement.Disassemble;
begin
  if FParent is TEDISEFFile then
    ParseCOMSDataOfCOMSDefinition(FData, Self, FSEFFile.ELMS)
  else
  if FParent is TEDISEFSegment then
    ParseCOMSDataOfSEGSDefinition(FData, Self, FSEFFile.COMS)
  else
  if FParent is TEDISEFRepeatingPattern then
  begin
    if TEDISEFRepeatingPattern(FParent).BaseParent is TEDISEFSegment then
      ParseCOMSDataOfSEGSDefinition(FData, Self, FSEFFile.COMS);
  end;
  AssignElementOrdinals;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFCompositeElement.GetTextSetsLocation: string;
begin
  Result := '';
  if FParent is TEDISEFSegment then
    Result := TEDISEFSegment(FParent).GetTextSetsLocation
  else
  if FParent is TEDISEFRepeatingPattern then
    if TEDISEFRepeatingPattern(FParent).BaseParent is TEDISEFSegment then
      Result := TEDISEFSegment(TEDISEFRepeatingPattern(FParent).BaseParent).GetTextSetsLocation;
  if Result <> '' then
    Result := Result + '~' + IntToStr(FOrdinal);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFCompositeElement.AssignElementOrdinals;
var
  I: Integer;
  Element: TEDISEFElement;
  ElementList: TObjectList;
begin
  ElementList := GetElementObjectList;
  try
    for I := 0 to ElementList.Count - 1 do
    begin
      if ElementList[I] is TEDISEFElement then
      begin
        Element := TEDISEFElement(ElementList[I]);
        if Element.Ordinal = -1 then
          Element.Ordinal := I+1;
      end; //if
    end; //for I
  finally
    ElementList.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFCompositeElement.GetElementObjectList: TObjectList;
begin
  Result := TObjectList.Create(False);
  ExtractFromDataObjectGroup(TEDISEFElement, Self, Result);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFCompositeElement.BindTextSets(TEXTSETS: TEDISEFTextSets);
begin
  FEDISEFTextSets.Free;
  FEDISEFTextSets := TEDISEFTextSets(TextSets.ReturnListItemsByName(GetTextSetsLocation));
end;

//==================================================================================================
// TEDISEFSegment
//==================================================================================================

{ TEDISEFSegment }

constructor TEDISEFSegment.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
  if FParent is TEDISEFTable then
    FParentSet := TEDISEFSet(FParent.Parent)
  else
  if FParent is TEDISEFLoop then
    FParentSet := TEDISEFLoop(FParent).BaseParentSet
  else
    FParentSet := nil;
  FOrdinal := -1;
  FRequirementDesignator := '';
  FMaximumUse := 0;
  FOwnerLoopId := NA_LoopId;
  FParentLoopId := NA_LoopId;
  FEDISEFTextSets := TEDISEFTextSets.Create(False);
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
  if FParent is TEDISEFFile then
    Result := CombineSEGSDataOfSEGSDefinition(Self)
  else
  if (FParent is TEDISEFTable) or (FParent is TEDISEFLoop) then
    Result := CombineSEGSDataOfSETSDefinition(Self);
end;

//--------------------------------------------------------------------------------------------------

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
        EDISEFDataObject := TEDISEFElement(ListItem.EDISEFDataObject).Clone(Self)
      else
      if ListItem.EDISEFDataObject is TEDISEFCompositeElement then
        EDISEFDataObject := TEDISEFCompositeElement(ListItem.EDISEFDataObject).Clone(Self)
      else
      if ListItem.EDISEFDataObject is TEDISEFRepeatingPattern then
        EDISEFDataObject := TEDISEFRepeatingPattern(ListItem.EDISEFDataObject).Clone(Self);
    end
    else
      EDISEFDataObject := TEDISEFElement.Create(Self);
    FEDISEFDataObjects.AddByNameOrId(EDISEFDataObject);
    ListItem := Segment.Elements.Next;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFSegment.Clone(NewParent: TEDISEFDataObject): TEDISEFSegment;
begin
  Result := nil;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFSegment.Disassemble;
begin
  if FParent is TEDISEFFile then
    ParseSEGSDataOfSEGSDefinition(FData, Self, FSEFFile)
  else
  if (FParent is TEDISEFTable) or (FParent is TEDISEFLoop) then
    ParseSEGSDataOfSETSDefinition(FData, Self, FSEFFile);
  AssignElementOrdinals;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFSegment.GetOwnerLoopId: string;
begin
  Result := NA_LoopId;
  if FParent is TEDISEFLoop then
    Result := FParent.Id;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFSegment.GetParentLoopId: string;
begin
  Result := NA_LoopId;
  if FParent is TEDISEFLoop then
    Result := TEDISEFLoop(FParent).ParentLoopId;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFSegment.GetTextSetsLocation: string;
begin
  Result := '';
  if FParentSet <> nil then
    Result := FParentSet.GetTextSetsLocation + '~' + IntToStr(FOrdinal);
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFSegment.GetElementObjectList: TObjectList;
begin
  Result := TObjectList.Create(False);
  ExtractFromDataObjectGroup([TEDISEFElement, TEDISEFCompositeElement], Self, Result);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFSegment.AssignElementOrdinals;
var
  I: Integer;
  Element: TEDISEFElement;
  CompositeElement: TEDISEFCompositeElement;
  ElementList: TObjectList;
begin
  ElementList := GetElementObjectList;
  try
    for I := 0 to ElementList.Count - 1 do
    begin
      if ElementList[I] is TEDISEFElement then
      begin
        Element := TEDISEFElement(ElementList[I]);
        if Element.Ordinal = -1 then
          Element.Ordinal := I+1;
      end
      else
      if ElementList[I] is TEDISEFCompositeElement then
      begin
        CompositeElement := TEDISEFCompositeElement(ElementList[I]);
        if CompositeElement.Ordinal = -1 then
          CompositeElement.Ordinal := I+1;
      end; //if
    end; //for I
  finally
    ElementList.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFSegment.BindTextSets(TEXTSETS: TEDISEFTextSets);
begin
  FEDISEFTextSets.Free;
  FEDISEFTextSets := TEDISEFTextSets(TextSets.ReturnListItemsByName(GetTextSetsLocation));
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFSegment.BindElementTextSets;
var
  I: Integer;
  Element: TEDISEFElement;
  CompositeElement: TEDISEFCompositeElement;
  ElementList: TObjectList;
begin
  ElementList := GetElementObjectList;
  try
    for I := 0 to ElementList.Count - 1 do
    begin
      if ElementList[I] is TEDISEFElement then
      begin
        Element := TEDISEFElement(ElementList[I]);
        Element.BindTextSets(FSEFFile.TEXTSETS);
      end
      else
      if ElementList[I] is TEDISEFCompositeElement then
      begin
        CompositeElement := TEDISEFCompositeElement(ElementList[I]);
        CompositeElement.BindTextSets(FSEFFile.TEXTSETS);
      end; //if
    end; //for I
  finally
    ElementList.Free;
  end;
end;

//==================================================================================================
// TEDISEFLoop
//==================================================================================================

{ TEDISEFLoop }

constructor TEDISEFLoop.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
  FMaximumRepeat := Value_UndefinedMaximum;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISEFLoop.Destroy;
begin
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFLoop.Assemble: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FEDISEFDataObjects.Count - 1 do
  begin
    if not (FEDISEFDataObjects[I] is TEDISEFLoop) then
      Result := Result + SEFDelimiter_OpeningBracket + FEDISEFDataObjects[I].Assemble +
        SEFDelimiter_ClosingBracket
    else
      Result := Result + SEFDelimiter_OpeningBrace + FEDISEFDataObjects[I].Assemble +
        SEFDelimiter_ClosingBrace;
  end;
  if FEDISEFDataObjects.Count > 0 then
  begin
    if FEDISEFDataObjects[0].Id <> FId then
      Result := FId + SEFDelimiter_Colon + IntToStr(FMaximumRepeat) + Result
    else
      Result := SEFDelimiter_Colon + IntToStr(FMaximumRepeat) + Result;
  end
  else
    Result := FId + SEFDelimiter_Colon + IntToStr(FMaximumRepeat) + Result;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFLoop.Clone(NewParent: TEDISEFDataObject): TEDISEFLoop;
begin
  Result := nil;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFLoop.Disassemble;
begin
  // FParent is TEDISEFTable
  ParseLoopDataOfSETSDefinition(FData, Self, FSEFFile);
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFLoop.GetParentLoopId: string;
begin
  Result := NA_LoopId;
  if FParent is TEDISEFLoop then
    Result := FParent.Id;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFLoop.GetBaseParentSet: TEDISEFSet;
var
  DataObject: TEDISEFDataObject;
begin
  Result := nil;
  DataObject := FParent;
  while DataObject <> nil do
  begin
    if DataObject is TEDISEFTable then
    begin
      Result := TEDISEFSet(DataObject.Parent);
      Break;
    end
    else
    if DataObject is TEDISEFLoop then
      DataObject := DataObject.Parent;
  end; //while
end;

//==================================================================================================
// TEDISEFTable
//==================================================================================================

{ TEDISEFTable }

constructor TEDISEFTable.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISEFTable.Destroy;
begin
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFTable.Assemble: string;
var
  I: Integer;
begin
  Result := '';
  if FEDISEFDataObjects.Count > 0 then
    Result := SEFDelimiter_Caret;
  for I := 0 to FEDISEFDataObjects.Count - 1 do
  begin
    if not (FEDISEFDataObjects[I] is TEDISEFLoop) then
      Result := Result + SEFDelimiter_OpeningBracket + FEDISEFDataObjects[I].Assemble +
        SEFDelimiter_ClosingBracket
    else
      Result := Result + SEFDelimiter_OpeningBrace + FEDISEFDataObjects[I].Assemble +
        SEFDelimiter_ClosingBrace;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFTable.Clone(NewParent: TEDISEFDataObject): TEDISEFTable;
begin
  Result := nil;
end;

//==================================================================================================
// TEDISEFSet
//==================================================================================================

{ TEDISEFSet }

constructor TEDISEFSet.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
  FEDISEFTextSets := TEDISEFTextSets.Create(False);
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISEFSet.Destroy;
begin
  FEDISEFTextSets.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFSet.Assemble: string;
var
  I: Integer;
begin
  Result := FId + SEFDelimiter_EqualSign;
  for I := 0 to FEDISEFDataObjects.Count - 1 do
  begin
    Result := Result + FEDISEFDataObjects[I].Assemble;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFSet.Clone(NewParent: TEDISEFDataObject): TEDISEFSet;
begin
  Result := nil;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFSet.Disassemble;
begin
  // FParent is TEDISEFFile
  ParseSetsDataOfSETSDefinition(FData, Self, FSEFFile);
  // Assign segment ordinals that were not explicitly defined
  AssignSegmentOrdinals;
  // Bind TEXT,SETS to segments
  BindSegmentTextSets;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFSet.GetEDISEFTable(Index: Integer): TEDISEFTable;
begin
  Result := TEDISEFTable(FEDISEFDataObjects[Index])
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFSet.BuildSegmentObjectListFromLoop(ObjectList: TObjectList; Loop: TEDISEFLoop);
var
  I: Integer;
  NestedLoop: TEDISEFLoop;
  Segment: TEDISEFSegment;
begin
  for I := 0 to Loop.EDISEFDataObjectCount - 1 do
  begin
    if Loop.EDISEFDataObject[I] is TEDISEFSegment then
    begin
      Segment := TEDISEFSegment(Loop.EDISEFDataObject[I]);
      ObjectList.Add(Segment)
    end
    else
    if Loop.EDISEFDataObject[I] is TEDISEFLoop then
    begin
      NestedLoop := TEDISEFLoop(Loop.EDISEFDataObject[I]);
      BuildSegmentObjectListFromLoop(ObjectList, NestedLoop);
    end;
  end; //for I
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFSet.GetSegmentObjectList: TObjectList;
var
  I, J: Integer;
  Table: TEDISEFTable;
  Loop: TEDISEFLoop;
begin
  Result := TObjectList.Create(False);
  for I := 0 to FEDISEFDataObjects.Count - 1 do
  begin
    Table := TEDISEFTable(FEDISEFDataObjects[I]);
    for J := 0 to Table.EDISEFDataObjectCount - 1 do
    begin
      if Table.EDISEFDataObject[J] is TEDISEFSegment then
        Result.Add(Table.EDISEFDataObject[J])
      else
      if Table.EDISEFDataObject[J] is TEDISEFLoop then
      begin
        Loop := TEDISEFLoop(Table.EDISEFDataObject[J]);
        BuildSegmentObjectListFromLoop(Result, Loop);
      end
    end; //for J
  end; //for I
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFSet.AssignSegmentOrdinals;
var
  I: Integer;
  Segment: TEDISEFSegment;
  SegmentList: TObjectList;
begin
  SegmentList := GetSegmentObjectList;
  try
    for I := 0 to SegmentList.Count - 1 do
    begin
      Segment := TEDISEFSegment(SegmentList[I]);
      if Segment.Ordinal = -1 then
        Segment.Ordinal := I+1;
    end; //for I
  finally
    SegmentList.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFSet.GetTextSetsLocation: string;
begin
  Result := FId;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFSet.BindTextSets(TEXTSETS: TEDISEFTextSets);
begin
  FEDISEFTextSets.Free;
  FEDISEFTextSets := TEDISEFTextSets(TextSets.ReturnListItemsByName(GetTextSetsLocation));
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFSet.BindSegmentTextSets;
var
  I: Integer;
  Segment: TEDISEFSegment;
  SegmentList: TObjectList;
begin
  SegmentList := GetSegmentObjectList;
  try
    for I := 0 to SegmentList.Count - 1 do
    begin
      Segment := TEDISEFSegment(SegmentList[I]);
      Segment.BindTextSets(FSEFFile.TEXTSETS);
      Segment.BindElementTextSets;
    end; //for I
  finally
    SegmentList.Free;
  end;
end;

//==================================================================================================
// TEDISEFFile
//==================================================================================================

{ TEDISEFFile }

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
  //
  FEDISEFTextSets := TEDISEFTextSets.Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISEFFile.Destroy;
begin
  FEDISEFIni.Free;
  FEDISEFStd.Free;
  FEDISEFSets.Free;
  FEDISEFSegs.Free;
  FEDISEFComs.Free;
  FEDISEFElms.Free;
  FEDISEFCodesList.Free;
  FEDISEFTextSets.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFFile.Assemble: string;
var
  I: Integer;
begin
  Result := '';
  Result := Result + SectionTag_VER + AnsiSpace + FEDISEFVer + AnsiCrLf;
  Result := Result + SectionTag_INI + AnsiCrLf;
  Result := Result + FEDISEFIni.Text + AnsiCrLf;
  if FEDISEFStd.Text <> '' then
    Result := Result + SectionTag_STD + AnsiCrLf;
  Result := Result + FEDISEFStd.Text + AnsiCrLf;
  if FEDISEFSets.Count > 0 then
  begin
    Result := Result + SectionTag_SETS + AnsiCrLf;
    for I := 0 to FEDISEFSets.Count - 1 do
      Result := Result + FEDISEFSets[I].Assemble + AnsiCrLf;
  end;
  if FEDISEFSegs.Count > 0 then
  begin
    Result := Result + SectionTag_SEGS + AnsiCrLf;
    for I := 0 to FEDISEFSegs.Count - 1 do
      Result := Result + FEDISEFSegs[I].Assemble + AnsiCrLf;
  end;
  if FEDISEFComs.Count > 0 then
  begin
    Result := Result + SectionTag_COMS + AnsiCrLf;
    for I := 0 to FEDISEFComs.Count - 1 do
      Result := Result + FEDISEFComs[I].Assemble + AnsiCrLf;
  end;
  if FEDISEFElms.Count > 0 then
  begin
    Result := Result + SectionTag_ELMS + AnsiCrLf;
    for I := 0 to FEDISEFElms.Count - 1 do
      Result := Result + FEDISEFElms[I].Assemble + AnsiCrLf;
  end;
  if FEDISEFCodesList.Text <> '' then
  begin
    Result := Result + SectionTag_CODES + AnsiCrLf;
    Result := Result + FEDISEFCodesList.Text + AnsiCrLf;
  end;
  if FEDISEFTextSets.Count > 0 then
  begin
    Result := Result + SectionTag_TEXTSETS + AnsiCrLf;
    for I := 0 to FEDISEFTextSets.Count - 1 do
      if TEDISEFText(FEDISEFTextSets[I]).Text <> '' then
        Result := Result + TEDISEFText(FEDISEFTextSets[I]).Assemble + AnsiCrLf;
  end;
  FData := Result;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFFile.Clone(NewParent: TEDISEFDataObject): TEDISEFFile;
begin
  Result := nil;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFFile.Disassemble;
begin
  // Must parse file in reverse order to build specification from the dictionary values
  // .TEXT,SETS
  ParseTextSets;
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

procedure TEDISEFFile.ParseTextSets;
var
  TempList: TStrings;
  SearchResult, SearchResult2, I: Integer;
  TextSet: TEDISEFTextSet;
begin
  TempList := TStringList.Create;
  try
    FEDISEFTextSets.Clear;
    SearchResult := StrSearch(SectionTag_TEXTSETS, FData, 1);
    if SearchResult > 0 then
    begin
      SearchResult := SearchResult + Length(SectionTag_TEXTSETS + AnsiCrLf);
      SearchResult2 := StrSearch(AnsiCrLf + SEFDelimiter_Period, FData, SearchResult + 1);
      if SearchResult2 <> 0 then
        TempList.Text := Copy(FData, SearchResult, SearchResult2 - SearchResult)
      else
        TempList.Text := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
      for I := 0 to TempList.Count - 1 do
      begin
        TextSet := TEDISEFTextSet.Create;
        TextSet.Data := TempList[I];
        if TextSet.Data <> '' then
          TextSet.Disassemble;
        FEDISEFTextSets.Add(TextSet, TextSet.Where);
      end;
    end;
  finally
    TempList.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFFile.ParseCodes;
var
  SearchResult, SearchResult2: Integer;
begin
  FEDISEFCodesList.Clear;
  SearchResult := StrSearch(SectionTag_CODES, FData, 1);
  if SearchResult > 0 then
  begin
    SearchResult := SearchResult + Length(SectionTag_CODES + AnsiCrLf);
    SearchResult2 := StrSearch(AnsiCrLf + SEFDelimiter_Period, FData, SearchResult + 1);
    if SearchResult2 <> 0 then
      FEDISEFCodesList.Text := Copy(FData, SearchResult, SearchResult2 - SearchResult)
    else
      FEDISEFCodesList.Text := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
  end;
end;

//--------------------------------------------------------------------------------------------------

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
      SearchResult := SearchResult + Length(SectionTag_COMS + AnsiCrLf);
      SearchResult2 := StrSearch(AnsiCrLf + SEFDelimiter_Period, FData, SearchResult + 1);
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
      end;
    end;
  finally
    TempList.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

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
      SearchResult := SearchResult + Length(SectionTag_ELMS + AnsiCrLf);
      SearchResult2 := StrSearch(AnsiCrLf + SEFDelimiter_Period, FData, SearchResult + 1);
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
      end;
    end;
  finally
    TempList.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFFile.ParseELMSExt;
begin
// ToDo:
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFFile.ParseINI;
var
  SearchResult, SearchResult2: Integer;
begin
  FEDISEFIni.Clear;
  {$IFDEF DELPHI6_UP}
  FEDISEFIni.Delimiter := SEFDelimiter_Comma;
  {$ELSE}

  {$ENDIF}
  SearchResult := StrSearch(SectionTag_INI, FData, 1);
  if SearchResult > 0 then
  begin
    SearchResult := SearchResult + Length(SectionTag_INI + AnsiCrLf);
    SearchResult2 := StrSearch(AnsiCrLf + SEFDelimiter_Period, FData, SearchResult + 1);
    if SearchResult2 <> 0 then
      FEDISEFIni.Text := Copy(FData, SearchResult, SearchResult2 - SearchResult)
    else
      FEDISEFIni.Text := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
  end;
  FId := FEDISEFIni.Text;
end;

//--------------------------------------------------------------------------------------------------

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
      SearchResult := SearchResult + Length(SectionTag_SEGS + AnsiCrLf);
      SearchResult2 := StrSearch(AnsiCrLf + SEFDelimiter_Period, FData, SearchResult + 1);
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
          Segment.Disassemble;
        FEDISEFSegs.AddByNameOrId(Segment, Segment.Id);
      end;
    end;
  finally
    TempList.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

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
      SearchResult := SearchResult + Length(SectionTag_SETS + AnsiCrLf);
      SearchResult2 := StrSearch(AnsiCrLf + SEFDelimiter_Period, FData, SearchResult + 1);
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
          TransactionSet.Disassemble;
        FEDISEFSets.AddByNameOrId(TransactionSet, TransactionSet.Id);
        TransactionSet.BindTextSets(FEDISEFTextSets);
      end;
    end;
  finally
    TempList.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFFile.ParseSTD;
var
  SearchResult, SearchResult2: Integer;
begin
  FEDISEFStd.Clear;
  {$IFDEF DELPHI6_UP}
  FEDISEFStd.Delimiter := SEFDelimiter_Comma;
  {$ELSE}

  {$ENDIF}
  SearchResult := StrSearch(SectionTag_STD, FData, 1);
  if SearchResult > 0 then
  begin
    SearchResult := SearchResult + Length(SectionTag_STD + AnsiCrLf);
    SearchResult2 := StrSearch(AnsiCrLf + SEFDelimiter_Period, FData, SearchResult + 1);
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

//--------------------------------------------------------------------------------------------------

procedure TEDISEFFile.ParseVER;
var
  SearchResult, SearchResult2: Integer;
begin
  FEDISEFVer := '';
  SearchResult := StrSearch(SectionTag_VER, FData, 1);
  if SearchResult > 0 then
  begin
    SearchResult := SearchResult + Length(SectionTag_VER);
    SearchResult2 := StrSearch(AnsiCrLf + SEFDelimiter_Period, FData, SearchResult + 1);
    if SearchResult2 <> 0 then
      FEDISEFVer := Copy(FData, SearchResult, SearchResult2 - SearchResult)
    else
      FEDISEFVer := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
    if FEDISEFVer = '' then
      FEDISEFVer := Value_Version10;
  end;
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
    raise EJclEDIError.CreateResRec(@RsEDIError002);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFTable.Disassemble;
begin
  // FParent is TEDISEFSet
  ParseTableDataOfSETSDefinition(FData, Self, FSEFFile);
end;

//--------------------------------------------------------------------------------------------------

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

//==================================================================================================
// TEDISEFRepeatingPattern
//==================================================================================================

{ TEDISEFRepeatingPattern }

constructor TEDISEFRepeatingPattern.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
  if Parent is TEDISEFRepeatingPattern then
    FBaseParent := TEDISEFRepeatingPattern(Parent).BaseParent
  else
    FBaseParent := Parent;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISEFRepeatingPattern.Destroy;
begin
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFRepeatingPattern.Assemble: string;
var
  I: Integer;
begin
  Result := IntToStr(FRepeatCount);
  for I := 0 to FEDISEFDataObjects.Count - 1 do
  begin
    if not (FEDISEFDataObjects[I] is TEDISEFRepeatingPattern) then
      Result := Result + SEFDelimiter_OpeningBracket + FEDISEFDataObjects[I].Assemble +
        SEFDelimiter_ClosingBracket
    else
      Result := Result + SEFDelimiter_OpeningBrace + FEDISEFDataObjects[I].Assemble +
        SEFDelimiter_ClosingBrace;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFRepeatingPattern.Clone(NewParent: TEDISEFDataObject): TEDISEFRepeatingPattern;
var
  ListItem: TEDISEFDataObjectListItem;
  SEFDataObject: TEDISEFDataObject;
begin
  Result := TEDISEFRepeatingPattern.Create(NewParent);
  Result.Id := FId;
  Result.RepeatCount := FRepeatCount;
  ListItem := FEDISEFDataObjects.First;
  while ListItem <> nil do
  begin
    if ListItem.EDISEFDataObject <> nil then
    begin
      SEFDataObject := ListItem.EDISEFDataObject;
      if ListItem.EDISEFDataObject is TEDISEFElement then
        SEFDataObject := TEDISEFElement(SEFDataObject).Clone(Result)
      else
      if ListItem.EDISEFDataObject is TEDISEFCompositeElement then
        SEFDataObject := TEDISEFCompositeElement(SEFDataObject).Clone(Result)
      else
      if ListItem.EDISEFDataObject is TEDISEFSegment then
        SEFDataObject := TEDISEFSegment(SEFDataObject).Clone(Result)
      else
      if ListItem.EDISEFDataObject is TEDISEFRepeatingPattern then
        SEFDataObject := TEDISEFRepeatingPattern(SEFDataObject).Clone(Result);
      Result.EDISEFDataObjects.AddByNameOrId(SEFDataObject);
    end;
    ListItem := FEDISEFDataObjects.Next;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFRepeatingPattern.Disassemble;
begin
  FEDISEFDataObjects.Clear;
  FId := FData;
  if FParent is TEDISEFCompositeElement then
    InternalParseCOMSDataOfCOMSDefinition(FData, Self, FSEFFile.ELMS)
  else
  if FParent is TEDISEFSegment then
    InternalParseSEGSDataOfSEGSDefinition(FData, Self, FSEFFile)
  else
  if FParent is TEDISEFRepeatingPattern then
  begin
    if FBaseParent is TEDISEFCompositeElement then
      InternalParseCOMSDataOfCOMSDefinition(FData, Self, FSEFFile.ELMS)
    else
    if FBaseParent is TEDISEFSegment then
      InternalParseSEGSDataOfSEGSDefinition(FData, Self, FSEFFile);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFRepeatingPattern.SetParent(const Value: TEDISEFDataObject);
begin
  inherited;
  if Value is TEDISEFRepeatingPattern then
    FBaseParent := TEDISEFRepeatingPattern(Value).BaseParent
  else
    FBaseParent := Value;
end;

//==================================================================================================
// TEDISEFText
//==================================================================================================

{ TEDISEFText }

function TEDISEFText.Assemble: string;
var
  I: Integer;
begin
  FWhere := '';
  for I := 0 to FWhereLocation.Count - 1 do
  begin
    if (FWhere <> '') and (FWhereLocation[I] <> '') then
      FWhere := FWhere + '~';
    FWhere := FWhere + FWhereLocation[I];
  end;
  Result := FWhere + ',' + FWhat + ',' + FText;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDISEFText.Create;
begin
  inherited Create;
  FEDISEFWhereType := twUnknown;
  FData := '';
  FWhere := '';
  FWhat := '';
  FText := '';
  FWhereLocation := TStringList.Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISEFText.Destroy;
begin
  FWhereLocation.Free;
  inherited;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFText.Disassemble;
var
  SearchResult, SearchResult2: Integer;
begin
  FEDISEFWhereType := twUnknown;
  SearchResult := StrSearch(',', FData, 1);
  FWhere := Copy(FData, 1, SearchResult - 1);
  FWhereLocation.Text := Copy(FData, 1, SearchResult - 1);
  FWhereLocation.CommaText := JclEDI.StringReplace(FWhereLocation.Text,'~',',',[rfReplaceAll]);
  SearchResult2 := StrSearch(',', FData, SearchResult + 1);
  FWhat := Copy(FData, SearchResult + 1, (SearchResult2 - SearchResult) - 1);
  if SearchResult2 > 0 then
    FText := Copy(FData, SearchResult2 + 1, Length(FData) - SearchResult2);
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFText.GetData: string;
begin
  Result := FData;
end;

//--------------------------------------------------------------------------------------------------

function TEDISEFText.GetText: string;
begin
  Result := FText;
  Result := JclEDI.StringReplace(Result, SEFTextCRLF, AnsiCrLf, [rfReplaceAll]);
  Result := JclEDI.StringReplace(Result, SEFTextCR, AnsiCarriageReturn, [rfReplaceAll]);
  Result := JclEDI.StringReplace(Result, SEFTextLF, AnsiLineFeed, [rfReplaceAll]);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFText.SetData(const Value: string);
begin
  FData := Value;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFText.SetText(const Value: string);
var
  Temp: string;
begin
  Temp := Value;
  Temp := JclEDI.StringReplace(Temp, AnsiCrLf, SEFTextCRLF, [rfReplaceAll]);
  Temp := JclEDI.StringReplace(Temp, AnsiCarriageReturn, SEFTextCR, [rfReplaceAll]);
  Temp := JclEDI.StringReplace(Temp, AnsiLineFeed, SEFTextLF, [rfReplaceAll]);
  FText := Temp;
end;

//==================================================================================================
// TEDISEFTextSet
//==================================================================================================

{ TEDISEFTextSet }

function TEDISEFTextSet.Assemble: string;
begin
  Result := inherited Assemble;
end;

//--------------------------------------------------------------------------------------------------

constructor TEDISEFTextSet.Create;
begin
  inherited;
  FWhereSet := '';
  FWhereSegment := -1;
  FWhereElement := -1;
  FWhereSubElement := -1;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISEFTextSet.Destroy;
begin
  inherited;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFTextSet.Disassemble;
begin
  FWhereSet := '';
  FWhereSegment := -1;
  FWhereElement := -1;
  FWhereSubElement := -1;
  inherited;
  if FWhereLocation.Count >= 1 then
  begin
    FEDISEFWhereType := twSet;
    FWhereSet := FWhereLocation[0];
  end;
  if FWhereLocation.Count >= 2 then
  begin
    FEDISEFWhereType := twSegment;
    FWhereSegment := StrToInt(FWhereLocation[1]);
  end;
  if FWhereLocation.Count >= 3 then
  begin
    FEDISEFWhereType := twElementOrCompositeElement;
    if FWhereLocation[2][1] in ['0'..'9'] then
      FWhereElement := StrToInt(FWhereLocation[2]);
  end;
  if FWhereLocation.Count >= 4 then
  begin
    FEDISEFWhereType := twSubElement;
    if FWhereLocation[3][1] in ['0'..'9'] then
      FWhereSubElement := StrToInt(FWhereLocation[3]);
  end;
end;

//==================================================================================================
// TEDISEFTextSets
//==================================================================================================

{ TEDISEFTextSets }

function TEDISEFTextSets.GetText(Code: string): string;
var
  ListItem: TEDIObjectListItem;
  TextSet: TEDISEFTextSet;
begin
  Result := '';
  ListItem := FFirstItem;
  while ListItem <> nil do
  begin
    TextSet := TEDISEFTextSet(ListItem.EDIObject);
    if TextSet.What = Code then
    begin
      Result := TextSet.Text;
      Break;
    end;
    ListItem := ListItem.NextItem;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISEFTextSets.SetText(EDISEFFile: TEDISEFFile; Location, Code, Text: string);
var
  ListItem: TEDIObjectListItem;
  TextSet: TEDISEFTextSet;
  Found: Boolean;
begin
  Found := False;
  ListItem := FFirstItem;
  while ListItem <> nil do
  begin
    TextSet := TEDISEFTextSet(ListItem.EDIObject);
    if TextSet.What = Code then
    begin
      TextSet.Text := Text;
      Break;
    end;
    ListItem := ListItem.NextItem;
  end;
  // If the item is not found then it will be created
  if (not Found) and (Text <> '') then
  begin
    TextSet := TEDISEFTextSet.Create;
    TextSet.Data := Location + ',' + Code + ',' + Text;
    TextSet.Disassemble;
    Add(TextSet, TextSet.Where);
    EDISEFFile.TEXTSETS.Add(TextSet, TextSet.Where);
  end;
end;

end.
