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
{ The Original Code is JclEDI_ANSIX12.pas.                                                                 }
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

unit JclEDI_ANSIX12;

{$I jcl.inc}

{$WEAKPACKAGEUNIT ON}

//Comment these compiler directives if you encounter problems with the following optimized methods.
{$DEFINE OPTIMIZED_DISASSEMBLE}
{$DEFINE OPTIMIZED_STRINGREPLACE}

interface

uses
  SysUtils, Classes,
  JclBase, JclStrings, JclEDI;

const
  ICHSegmentId = 'ISA'; //Interchange Control Header Segment Id
  ICTSegmentId = 'IEA'; //Interchange Control Trailer Segment Id
  FGHSegmentId = 'GS';  //Functional Group Header Segment Id
  FGTSegmentId = 'GE';  //Functional Group Trailer Segment Id
  TSHSegmentId = 'ST';  //Transaction Set Header Segment Id
  TSTSegmentId = 'SE';  //Transaction Set Trailer Segment Id

{ Documentation: Reserved Data Field Names

  'Id'
  'Position'
  'Description'
  'Notes'
  'Section'                //For Segment Only
  'RequirementDesignator'
  'MaximumUsage'           //For Segment Only
  'OwnerLoopId'            //...
  'ParentLoopId'           //...
  'MaximumLoopRepeat'      //For Loop however saved in segment that begins loop  
  'Type'                   //For Element Only
  'MinimumLength'          //...
  'MaximumLength'          //...

  'TransSetId'             //For Segment ST Only
  'TransSetDesc'           //...

  'FunctionalGroupId'      //For Segment GS Only
  'FGDescription'          //...
  'AgencyCodeId'           //...
  'VersionReleaseId'       //...

  'StandardId'             //For Segment ISA Only
  'VersionId'              //...
  'ICDescription'          //...
}

//--------------------------------------------------------------------------------------------------
//  EDI Forward Class Declarations
//--------------------------------------------------------------------------------------------------
type
  TEDIElement = class;
  TEDISegment = class;
  TEDITransactionSet = class;
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
//  EDI Element Specification
//--------------------------------------------------------------------------------------------------

  TEDIElementSpec = class(TEDIElement)
  private
    FReservedData: TStrings;  
    FId: string;
    FPosition: Integer;
    FDescription: string;
    FNotes: string;
    FRequirementDesignator: string;
    FType: string;
    FMinimumLength: Integer;
    FMaximumLength: Integer;
  public
    constructor Create(Parent: TEDIDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
  published
    property ReservedData: TStrings read FReservedData;  
    property Id: string read FId write FId;
    property Position: Integer read FPosition write FPosition;
    property Description: string read FDescription write FDescription;
    property Notes: string read FNotes write FNotes;
    property RequirementDesignator: string read FRequirementDesignator write FRequirementDesignator;
    property ElementType: string read FType write FType;
    property MinimumLength: Integer read FMinimumLength write FMinimumLength;
    property MaximumLength: Integer read FMaximumLength write FMaximumLength;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Segment Classes
//--------------------------------------------------------------------------------------------------

  TEDISegmentArray = array of TEDISegment;

  TEDISegment = class(TEDIDataObjectGroup)
  private
    FSegmentID: string;
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
  published
    property SegmentID: string read FSegmentID write FSegmentID;
    property ElementCount: Integer read GetCount;
  end;

  TEDITransactionSetSegment = class(TEDISegment)
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
//  EDI Segment Specification Classes
//--------------------------------------------------------------------------------------------------

  TEDISegmentSpec = class(TEDISegment)
  private
    FReservedData: TStrings;
    FPosition: Integer;
    FDescription: string;
    FNotes: string;
    FSection: string;
    FRequirementDesignator: string;
    FMaximumUsage: Integer;
    FOwnerLoopId: string;
    FParentLoopId: string;
  public
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer = 0); reintroduce;
    destructor Destroy; override;
    procedure AssembleReservedData(ReservedData: TStrings); virtual;
    procedure DisassembleReservedData(ReservedData: TStrings); virtual;
    function InternalCreateElement: TEDIElement; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    procedure ValidateElementIndexPositions;
  published
    property ReservedData: TStrings read FReservedData;
    property Position: Integer read FPosition write FPosition;
    property Description: string read FDescription write FDescription;
    property Notes: string read FNotes write FNotes;
    property Section: string read FSection write FSection;
    property RequirementDesignator: string read FRequirementDesignator write FRequirementDesignator;
    property MaximumUsage: Integer read FMaximumUsage write FMaximumUsage;
    property OwnerLoopId: string read FOwnerLoopId write FOwnerLoopId;
    property ParentLoopId: string read FParentLoopId write FParentLoopId;
  end;

  TEDITransactionSetSegmentSpec = class(TEDISegmentSpec)
  public
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer = 0); reintroduce;
    function InternalAssignDelimiters: TEDIDelimiters; override;
  end;

  TEDITransactionSetSegmentSTSpec = class(TEDITransactionSetSegmentSpec)
  public
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer = 0); reintroduce;
    procedure AssembleReservedData(ReservedData: TStrings); override;
    procedure DisassembleReservedData(ReservedData: TStrings); override;
  end;

  TEDIFunctionalGroupSegmentSpec = class(TEDISegmentSpec)
  public
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer = 0); reintroduce;
    function InternalAssignDelimiters: TEDIDelimiters; override;
  end;

  TEDIFunctionalGroupSegmentGSSpec = class(TEDIFunctionalGroupSegmentSpec)
  public
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer = 0); reintroduce;
    procedure AssembleReservedData(ReservedData: TStrings); override;
    procedure DisassembleReservedData(ReservedData: TStrings); override;
  end;

  TEDIInterchangeControlSegmentSpec = class(TEDISegmentSpec)
  public
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer = 0); reintroduce;
    function InternalAssignDelimiters: TEDIDelimiters; override;
  end;

  TEDIInterchangeControlSegmentISASpec = class(TEDIInterchangeControlSegmentSpec)
  public
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer = 0); reintroduce;
    function Assemble: string; override;
    procedure Disassemble; override;
    procedure AssembleReservedData(ReservedData: TStrings); override;
    procedure DisassembleReservedData(ReservedData: TStrings); override;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Transaction Set
//--------------------------------------------------------------------------------------------------

  TEDITransactionSetArray = array of TEDITransactionSet;

  TEDITransactionSet = class(TEDIDataObjectGroup)
  private
    FSTSegment: TEDITransactionSetSegment;
    FSESegment: TEDITransactionSetSegment;
    function GetSegment(Index: Integer): TEDISegment;
    procedure SetSegment(Index: Integer; Segment: TEDISegment);
    function GetSegments: TEDISegmentArray;
    procedure SetSegments(const Value: TEDISegmentArray);
    procedure SetSTSegment(const STSegment: TEDITransactionSetSegment);
    procedure SetSESegment(const SESegment: TEDITransactionSetSegment);
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
    property SegmentST: TEDITransactionSetSegment read FSTSegment write SetSTSegment;
    property SegmentSE: TEDITransactionSetSegment read FSESegment write SetSESegment;
    property SegmentCount: Integer read GetCount;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Transaction Set Specification
//--------------------------------------------------------------------------------------------------

  TEDITransactionSetSpec = class(TEDITransactionSet)
  private
    FTransactionSetId: string;
    FTSDescription: string;
  public
    procedure InternalCreateHeaderTrailerSegments; override;
    function InternalCreateSegment: TEDISegment; override;
    procedure ValidateSegmentIndexPositions;
  published
    property TransactionSetId: string read FTransactionSetId write FTransactionSetId;
    property TSDescription: string read FTSDescription write FTSDescription;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Transaction Set Loop
//--------------------------------------------------------------------------------------------------

  TEDITransactionSetLoop = class(TEDIDataObject)
  private
    FOwnerLoopId: string;
    FParentLoopId: string;
    FParentTransactionSet: TEDITransactionSet;
    //
    FEDIDataObjects: TEDIDataObjectArray;
    function GetEDIDataObject(Index: Integer): TEDIDataObject;
    procedure SetEDIDataObject(Index: Integer; EDIDataObject: TEDIDataObject);
    function GetCount: Integer;
  public
    constructor Create(Parent: TEDIDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    //
    //  ToDo:  More procedures and functions to manage internal structures
    //
    function FindLoop(LoopId: string; var StartIndex: Integer): TEDITransactionSetLoop;
    function FindSegment(SegmentId: string; var StartIndex: Integer): TEDISegment; overload;
    function FindSegment(SegmentId: string; var StartIndex: Integer;
      ElementConditions: TStrings): TEDISegment; overload;
    //
    function AddLoop(OwnerLoopId, ParentLoopId: string): Integer;
    procedure AppendSegment(Segment: TEDISegment);
    procedure DeleteEDIDataObjects;
    property EDIDataObject[Index: Integer]: TEDIDataObject read GetEDIDataObject
      write SetEDIDataObject; default;

    property EDIDataObjects: TEDIDataObjectArray read FEDIDataObjects write FEDIDataObjects;
  published
    property OwnerLoopId: string read FOwnerLoopId write FOwnerLoopId;
    property ParentLoopId: string read FParentLoopId write FParentLoopId;
    property ParentTransactionSet: TEDITransactionSet read FParentTransactionSet
      write FParentTransactionSet;
    property Count: Integer read GetCount;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Transaction Set Document and related types and classes
//--------------------------------------------------------------------------------------------------

  TEDILoopStackRecord = record
    SegmentId: string;
    SpecStartIndex: Integer;
    OwnerLoopId: string;
    ParentLoopId: string;
    Loop: TEDITransactionSetLoop;
  end;

  TEDILoopStackArray = array of TEDILoopStackRecord;

  TEDILoopStack = class(TEDIObject)
  private
    FEDILoopStack: TEDILoopStackArray;
    FAltStackPointer: Boolean;
    FStackResized: Boolean;
    function GetSafeStackIndex(Index: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function GetStackSize: Integer;
    function SetStackPointer(OwnerLoopId, ParentLoopId: string): Integer;
    function GetStackRecord: TEDILoopStackRecord; overload;
    function GetStackRecord(Index: Integer): TEDILoopStackRecord; overload;
    function Add(SegmentId, OwnerLoopId, ParentLoopId: string; StartIndex: Integer;
      Loop: TEDITransactionSetLoop): Integer;
    procedure UpdateLoopReference(Loop: TEDITransactionSetLoop);
    procedure Update(SegmentId, OwnerLoopId, ParentLoopId: string; StartIndex: Integer;
      Loop: TEDITransactionSetLoop);
    function Debug: string;
    property Stack: TEDILoopStackArray read FEDILoopStack write FEDILoopStack;
  published
    property AltStackPointer: Boolean read FAltStackPointer write FAltStackPointer;
    property StackResized: Boolean read FStackResized write FStackResized;
  end;

  TEDITransactionSetDocumentOptions = set of (doLinkSpecToDataObject);

  TEDITransactionSetDocument = class(TEDITransactionSetLoop)
  private
  protected
    FErrorOccured: Boolean;
    FEDITSDOptions: TEDITransactionSetDocumentOptions;
    //References
    FEDITransactionSet: TEDITransactionSet;
    FEDITransactionSetSpec: TEDITransactionSetSpec;
    //Helper Object
    FEDILoopStack: TEDILoopStack;
    //Helper functions
    function ValidateSegSpecIndex(DataSegmentId: string; SpecStartIndex: Integer;
      var LoopRepeated: Boolean): Integer;
    function ValidateLoopStack(SpecSegmentId, SpecOwnerLoopId, SpecParentLoopId: string;
      SpecStartIndex: Integer; Loop: TEDITransactionSetLoop;
      LoopRepeated: Boolean): TEDILoopStackRecord;
    function AdvanceSegSpecIndex(DataIndex, SpecStartIndex, SpecEndIndex: Integer): Integer;
    //
    procedure SetSpecificationPointers(DataSegment, SpecSegment: TEDISegment);
  protected
    procedure ValidateData(TSDocument: TEDITransactionSetDocument;
      LoopStack: TEDILoopStack;
      DataSegment, SpecSegment: TEDISegment;
      var DataIndex, SpecIndex: Integer;
      var ErrorOccured: Boolean); virtual;
  public
    constructor Create(Parent: TEDIDataObject; EDITransactionSet: TEDITransactionSet;
      EDITransactionSetSpec: TEDITransactionSetSpec); reintroduce;
    destructor Destroy; override;
    //
    //  ToDo:  More procedures and functions to manage internal structures
    //
    procedure FormatDocument; virtual;
  published
    property EDITSDOptions: TEDITransactionSetDocumentOptions read FEDITSDOptions
      write FEDITSDOptions;
    property ErrorOccured: Boolean read FErrorOccured;
  end;

  TEDITransactionSetDocumentArray = array of TEDITransactionSetDocument;

//--------------------------------------------------------------------------------------------------
//  EDI Functional Group
//--------------------------------------------------------------------------------------------------

  TEDIFunctionalGroupArray = array of TEDIFunctionalGroup;

  TEDIFunctionalGroup = class(TEDIDataObjectGroup)
  private
    FGSSegment: TEDIFunctionalGroupSegment;
    FGESegment: TEDIFunctionalGroupSegment;
    function GetTransactionSet(Index: Integer): TEDITransactionSet;
    procedure SetTransactionSet(Index: Integer; TransactionSet: TEDITransactionSet);
    function GetTransactionSets: TEDITransactionSetArray;
    procedure SetTransactionSets(const Value: TEDITransactionSetArray);
    procedure SetGSSegment(const GSSegment: TEDIFunctionalGroupSegment);
    procedure SetGESegment(const GESegment: TEDIFunctionalGroupSegment);
  protected
    procedure InternalCreateHeaderTrailerSegments; virtual;
    function InternalCreateTransactionSet: TEDITransactionSet; virtual;
    function InternalAssignDelimiters: TEDIDelimiters; override;
    function InternalCreateEDIDataObject: TEDIDataObject; override;
  public
    constructor Create(Parent: TEDIDataObject; TransactionSetCount: Integer = 0); reintroduce;
    destructor Destroy; override;

    function AddTransactionSet: Integer;
    function AppendTransactionSet(TransactionSet: TEDITransactionSet): Integer;
    function InsertTransactionSet(InsertIndex: Integer): Integer; overload;
    function InsertTransactionSet(InsertIndex: Integer;
      TransactionSet: TEDITransactionSet): Integer; overload;
    procedure DeleteTransactionSet(Index: Integer); overload;
    procedure DeleteTransactionSet(TransactionSet: TEDITransactionSet); overload;

    function AddTransactionSets(Count: Integer): Integer;
    function AppendTransactionSets(TransactionSetArray: TEDITransactionSetArray): Integer;
    function InsertTransactionSets(InsertIndex, Count: Integer): Integer; overload;
    function InsertTransactionSets(InsertIndex: Integer;
      TransactionSetArray: TEDITransactionSetArray): Integer; overload;
    procedure DeleteTransactionSets; overload;
    procedure DeleteTransactionSets(Index, Count: Integer); overload;

    function Assemble: string; override;
    procedure Disassemble; override;

    property TransactionSet[Index: Integer]: TEDITransactionSet read GetTransactionSet
      write SetTransactionSet; default;
    property TransactionSets: TEDITransactionSetArray read GetTransactionSets
      write SetTransactionSets;
  published
    property SegmentGS: TEDIFunctionalGroupSegment read FGSSegment write SetGSSegment;
    property SegmentGE: TEDIFunctionalGroupSegment read FGESegment write SetGESegment;
    property TransactionSetCount: Integer read GetCount;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Functional Specification
//--------------------------------------------------------------------------------------------------

  TEDIFunctionalGroupSpec = class(TEDIFunctionalGroup)
  private
    FFunctionalGroupId: string;
    FFGDescription: string;
    FAgencyCodeId: string;
    FVersionReleaseId: string;
  public
    procedure InternalCreateHeaderTrailerSegments; override;
    function InternalCreateTransactionSet: TEDITransactionSet; override;
    function FindTransactionSetSpec(TransactionSetId: string): TEDITransactionSetSpec;
  published
    property FunctionalGroupId: string read FFunctionalGroupId write FFunctionalGroupId;
    property FGDescription: string read FFGDescription write FFGDescription;
    property AgencyCodeId: string read FAgencyCodeId write FAgencyCodeId;
    property VersionReleaseId: string read FVersionReleaseId write FVersionReleaseId;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Interchange Control
//--------------------------------------------------------------------------------------------------

  TEDIInterchangeControlArray = array of TEDIInterchangeControl;

  TEDIInterchangeControl = class(TEDIDataObjectGroup)
  private
    FISASegment: TEDIInterchangeControlSegment;
    FIEASegment: TEDIInterchangeControlSegment;
    function GetFunctionalGroup(Index: Integer): TEDIFunctionalGroup;
    procedure SetFunctionalGroup(Index: Integer; FunctionalGroup: TEDIFunctionalGroup);
    function GetFunctionalGroups: TEDIFunctionalGroupArray;
    procedure SetFunctionalGroups(const Value: TEDIFunctionalGroupArray);
    procedure SetISASegment(const ISASegment: TEDIInterchangeControlSegment);
    procedure SetIEASegment(const IEASegment: TEDIInterchangeControlSegment);
  protected
    procedure InternalCreateHeaderTrailerSegments; virtual;
    function InternalCreateFunctionalGroup: TEDIFunctionalGroup; virtual;
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
    procedure DeleteFunctionalGroup(Index: Integer); overload;
    procedure DeleteFunctionalGroup(FunctionalGroup: TEDIFunctionalGroup); overload;

    function AddFunctionalGroups(Count: Integer): Integer;
    function AppendFunctionalGroups(FunctionalGroupArray: TEDIFunctionalGroupArray): Integer;
    function InsertFunctionalGroups(InsertIndex, Count: Integer): Integer; overload;
    function InsertFunctionalGroups(InsertIndex: Integer;
      FunctionalGroupArray: TEDIFunctionalGroupArray): Integer; overload;
    procedure DeleteFunctionalGroups; overload;
    procedure DeleteFunctionalGroups(Index, Count: Integer); overload;

    function Assemble: string; override;
    procedure Disassemble; override;

    property FunctionalGroup[Index: Integer]: TEDIFunctionalGroup read GetFunctionalGroup
      write SetFunctionalGroup; default;
    property FunctionalGroups: TEDIFunctionalGroupArray read GetFunctionalGroups
      write SetFunctionalGroups;
  published
    property SegmentISA: TEDIInterchangeControlSegment read FISASegment write SetISASegment;
    property SegmentIEA: TEDIInterchangeControlSegment read FIEASegment write SetIEASegment;
    property FunctionalGroupCount: Integer read GetCount;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI Interchange Specification
//--------------------------------------------------------------------------------------------------

  TEDIInterchangeControlSpec = class(TEDIInterchangeControl)
  private
    FStandardId: string;
    FVersionId: string;
    FICDescription: string;
  public
    procedure InternalCreateHeaderTrailerSegments; override;
    function InternalCreateFunctionalGroup: TEDIFunctionalGroup; override;
    function FindFunctionalGroupSpec(FunctionalGroupId, AgencyCodeId,
      VersionReleaseId: string): TEDIFunctionalGroupSpec;
    function FindTransactionSetSpec(FunctionalGroupId, AgencyCodeId, VersionReleaseId,
      TransactionSetId: string): TEDITransactionSetSpec;
  published
    property StandardId: string read FStandardId write FStandardId;
    property VersionId: string read FVersionId write FVersionId;
    property ICDescription: string read FICDescription write FICDescription;
  end;

//--------------------------------------------------------------------------------------------------
//  EDI File
//--------------------------------------------------------------------------------------------------

  TEDIFileArray = array of TEDIFile;

  TEDIFileOptions = set of (foVariableDelimiterDetection, foUseAltDelimiterDetection, foRemoveCrLf,
    foRemoveCr, foRemoveLf);

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

//--------------------------------------------------------------------------------------------------
//  EDI File Specification
//--------------------------------------------------------------------------------------------------

  TEDIFileSpec = class(TEDIFile)
  public
    constructor Create(Parent: TEDIDataObject; InterchangeCount: Integer = 0); reintroduce;
    function FindTransactionSetSpec(StandardId, VersionId, FunctionalGroupId, AgencyCodeId,
      VersionReleaseId, TransactionSetId: string): TEDITransactionSetSpec;
    function FindFunctionalGroupSpec(StandardId, VersionId, FunctionalGroupId, AgencyCodeId,
      VersionReleaseId: string): TEDIFunctionalGroupSpec;
    function FindInterchangeControlSpec(StandardId, VersionId: string): TEDIInterchangeControlSpec;
    procedure InternalDelimitersDetection(StartPos: Integer); override;
    function InternalCreateInterchangeControl: TEDIInterchangeControl; override;
  end;

implementation

uses
  JclResources;

const
  cUnknown = 'Unknown';
  cStandardId = 'StandardId';
  cVersionId = 'VersionId';
  cICDescription = 'ICDescription';

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
    for I := Low(TEDISegment(Parent).Elements) to High(TEDISegment(Parent).Elements) do
      if TEDISegment(Parent).Element[I] = Self then
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
  if Assigned(Parent) and (Parent is TEDITransactionSet) then
    inherited Create(Parent, ElementCount)
  else
    inherited Create(nil, ElementCount);
  FSegmentID := '';
  FEDIDOT := ediSegment;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISegment.Destroy;
begin
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.AddElements(Count: Integer): Integer;
begin
  Result := AddEDIDataObjects(Count);
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.AddElement: Integer;
begin
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
        FData := FData + FDelimiters.ED + FEDIDataObjects[I].Assemble
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
    // Data Input Scenarios
    // 4.)  SegID*---*---~
    // Composite Element Data Input Secnarios
    // 9.)  SegID*---*--->---~
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
      {$IFDEF OPTIMIZED_DISASSEMBLE}
      EDIElement := InternalCreateElement;
      LLHeader.AppendEDIDataObject(EDIElement);
      {$ELSE}
      I := AddElement;
      {$ENDIF}
      if (SearchResult - StartPos) > 0 then // data exists
      begin
        {$IFDEF OPTIMIZED_DISASSEMBLE}
        EDIElement.Data := Copy(FData, ((StartPos + FDelimiters.EDLen) - 1),
          (SearchResult - StartPos));
        EDIElement.Disassemble;
        {$ELSE}
        FEDIDataObjects[I].Data := Copy(FData, ((StartPos + FDelimiters.EDLen) - 1),
          (SearchResult - StartPos));
        FEDIDataObjects[I].Disassemble;
        {$ENDIF}
      end;
      StartPos := SearchResult + 1;
      SearchResult := StrSearch(FDelimiters.ED, FData, StartPos);
    end;
    // Get last element before next segment
    SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
    if SearchResult <> 0 then
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
        EDIElement.Data := Copy(FData, ((StartPos + FDelimiters.EDLen) - 1),
          (SearchResult - StartPos));
        EDIElement.Disassemble;
        {$ELSE}
        FEDIDataObjects[I].Data := Copy(FData, ((StartPos + FDelimiters.EDLen) - 1),
          (SearchResult - StartPos));
        FEDIDataObjects[I].Disassemble;
        {$ENDIF}
      end;
    end;
    FData := '';
  {$IFDEF OPTIMIZED_DISASSEMBLE}
    // Add the objects in a chunk
    SetLength(FEDIDataObjects, Length(FEDIDataObjects) + LLHeader.ItemCount);
    // AddElements(LLHeader.ItemCount);
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

function TEDISegment.GetElement(Index: Integer): TEDIElement;
begin
  Result := TEDIElement(FEDIDataObjects[Index]);
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.InsertElement(InsertIndex: Integer): Integer;
begin
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
  Result := InsertEDIDataObjects(InsertIndex, Count);
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
    // Get the delimiters from the transaction set
    if Assigned(Parent) and (Parent is TEDITransactionSet) then
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
          Result := Parent.Parent.Parent.Delimiters;
      end;
    end;
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.InternalCreateElement: TEDIElement;
begin
  Result := TEDIElement.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISegment.SetElement(Index: Integer; Element: TEDIElement);
begin
  TEDIElement(FEDIDataObjects[Index]) := Element;
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.GetElements: TEDIElementArray;
begin
  Result := TEDIElementArray(FEDIDataObjects);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISegment.SetElements(const Value: TEDIElementArray);
begin
  TEDIElementArray(FEDIDataObjects) := Value;
end;

//--------------------------------------------------------------------------------------------------

function TEDISegment.InternalCreateEDIDataObject: TEDIDataObject;
begin
  Result := InternalCreateElement;
end;

//==================================================================================================
// TEDITransactionSetSegment
//==================================================================================================

constructor TEDITransactionSetSegment.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  if Assigned(Parent) and (Parent is TEDITransactionSet) then
    FParent := Parent;
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSetSegment.InternalAssignDelimiters: TEDIDelimiters;
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
        Result := Parent.Parent.Delimiters;
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

destructor TEDITransactionSet.Destroy;
begin
  FSESegment.Free;
  FSTSegment.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControlSegment.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  // Attempt to assign the delimiters
  if not Assigned(FDelimiters) then
    // Get the delimiters from the interchange control
    if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
      Result := Parent.Delimiters;
end;

//==================================================================================================
// TEDITransactionSet
//==================================================================================================

constructor TEDITransactionSet.Create(Parent: TEDIDataObject; SegmentCount: Integer);
begin
  if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
    inherited Create(Parent, SegmentCount)
  else
    inherited Create(nil, SegmentCount);
  FEDIDOT := ediTransactionSet;
  InternalCreateHeaderTrailerSegments;
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSet.AddSegment: Integer;
begin
  Result := AddEDIDataObject;
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSet.AddSegments(Count: Integer): Integer;
begin
  Result := AddEDIDataObjects(Count);
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSet.AppendSegment(Segment: TEDISegment): Integer;
begin
  Result := AppendEDIDataObject(Segment);
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSet.AppendSegments(SegmentArray: TEDISegmentArray): Integer;
begin
  Result := AppendEDIDataObjects(TEDIDataObjectArray(SegmentArray));
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSet.Assemble: string;
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
      raise EJclEDIError.CreateResRec(@RsEDIError026);
  end;

  FData := FSTSegment.Assemble;
  FSTSegment.Data := '';

  if Length(FEDIDataObjects) > 0 then
    for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
      if Assigned(FEDIDataObjects[I]) then
        FData := FData + FEDIDataObjects[I].Assemble;

  DeleteSegments;

  FData := FData + FSESegment.Assemble;
  FSESegment.Data := '';

  FLength := Length(FData);
  Result := FData;

  FState := ediAssembled;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDITransactionSet.DeleteSegment(Index: Integer);
begin
  DeleteEDIDataObject(Index);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDITransactionSet.DeleteSegment(Segment: TEDISegment);
begin
  DeleteEDIDataObject(Segment);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDITransactionSet.DeleteSegments;
begin
  DeleteEDIDataObjects;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDITransactionSet.DeleteSegments(Index, Count: Integer);
begin
  DeleteEDIDataObjects(Index, Count);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDITransactionSet.Disassemble;
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
    FSTSegment.Data := '';
    FSTSegment.DeleteElements;
    FSESegment.Data := '';
    FSESegment.DeleteElements;
    DeleteSegments;
    // Check delimiter assignment
    if not Assigned(FDelimiters) then
    begin
      FDelimiters := InternalAssignDelimiters;
      if not Assigned(FDelimiters) then
        raise EJclEDIError.CreateResRec(@RsEDIError025);
    end;
    // Find the first segment
    StartPos := 1;
    SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
    while SearchResult <> 0 do
    begin
      S := Copy(FData, ((StartPos + FDelimiters.SDLen) - 1), Length(TSHSegmentId));
      S2 := Copy(FData, ((StartPos + FDelimiters.SDLen) - 1), Length(TSTSegmentId));
      if (S <> TSHSegmentId) and (S2 <> TSTSegmentId) then
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
      if S = TSHSegmentId then
      begin
        if (SearchResult - StartPos) > 0 then // data exists
        begin
          FSTSegment.Data := Copy(FData, ((StartPos + FDelimiters.SDLen) - 1),
            ((SearchResult - StartPos) + FDelimiters.SDLen));
          FSTSegment.Disassemble;
        end;
      end
      else
      if S2 = TSTSegmentId then
      begin
        if (SearchResult - StartPos) > 0 then // data exists
        begin
          FSESegment.Data := Copy(FData, ((StartPos + FDelimiters.SDLen) - 1),
            ((SearchResult - StartPos) + FDelimiters.SDLen));
          FSESegment.Disassemble;
        end;
      end;
      StartPos := SearchResult + FDelimiters.SDLen;
      SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
    end;
    FData := '';
  {$IFDEF OPTIMIZED_DISASSEMBLE}
    // Add the objects in a chunk
    SetLength(FEDIDataObjects, Length(FEDIDataObjects) + LLHeader.ItemCount);
    // AddSegments(LLHeader.ItemCount);
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

function TEDITransactionSet.GetSegment(Index: Integer): TEDISegment;
begin
  Result := TEDISegment(FEDIDataObjects[Index]);
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSet.InsertSegment(InsertIndex: Integer): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex);
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSet.GetSegments: TEDISegmentArray;
begin
  Result := TEDISegmentArray(FEDIDataObjects);
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSet.InsertSegment(InsertIndex: Integer; Segment: TEDISegment): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex, Segment);
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSet.InsertSegments(InsertIndex, Count: Integer): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, Count);
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSet.InsertSegments(InsertIndex: Integer;
  SegmentArray: TEDISegmentArray): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, TEDIDataObjectArray(SegmentArray));
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSet.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  if FDelimiters = nil then // Attempt to assign the delimiters
    if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
    begin
      if Assigned(Parent.Delimiters) then
      begin
        Result := Parent.Delimiters;
        Exit;
      end;
      if Assigned(Parent.Parent) and (Parent.Parent is TEDIInterchangeControl) then
        Result := Parent.Parent.Delimiters;
    end;
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSet.InternalCreateSegment: TEDISegment;
begin
  Result := TEDISegment.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDITransactionSet.InternalCreateHeaderTrailerSegments;
begin
  FSTSegment := TEDITransactionSetSegment.Create(Self);
  FSESegment := TEDITransactionSetSegment.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDITransactionSet.SetSegment(Index: Integer; Segment: TEDISegment);
begin
  TEDISegment(FEDIDataObjects[Index]) := Segment;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDITransactionSet.SetSegments(const Value: TEDISegmentArray);
begin
  TEDISegmentArray(FEDIDataObjects) := Value;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDITransactionSet.SetSESegment(const SESegment: TEDITransactionSetSegment);
begin
  FreeAndNil(FSESegment);
  FSESegment := SESegment;
  if Assigned(FSESegment) then
    FSESegment.Parent := Self;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDITransactionSet.SetSTSegment(const STSegment: TEDITransactionSetSegment);
begin
  FreeAndNil(FSTSegment);
  FSTSegment := STSegment;
  if Assigned(FSTSegment) then
    FSTSegment.Parent := Self;
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSet.InternalCreateEDIDataObject: TEDIDataObject;
begin
  Result := InternalCreateSegment;
end;

//==================================================================================================
// TEDIFunctionalGroup
//==================================================================================================

constructor TEDIFunctionalGroup.Create(Parent: TEDIDataObject; TransactionSetCount: Integer);
begin
  if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
    inherited Create(Parent, TransactionSetCount)
  else
    inherited Create(nil, TransactionSetCount);
  FEDIDOT := ediFunctionalGroup;
  InternalCreateHeaderTrailerSegments;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIFunctionalGroup.Destroy;
begin
  FGSSegment.Free;
  FGESegment.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.AddTransactionSet: Integer;
begin
  Result := AddEDIDataObject;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.AddTransactionSets(Count: Integer): Integer;
begin
  Result := AddEDIDataObjects(Count);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.AppendTransactionSet(TransactionSet: TEDITransactionSet): Integer;
begin
  Result := AppendEDIDataObject(TransactionSet);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.AppendTransactionSets(
  TransactionSetArray: TEDITransactionSetArray): Integer;
begin
  Result := AppendEDIDataObjects(TEDIDataObjectArray(TransactionSetArray));
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
  FData := FGSSegment.Assemble;
  FGSSegment.Data := '';

  if Length(FEDIDataObjects) > 0 then
    for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
      if Assigned(FEDIDataObjects[I]) then
        FData := FData + FEDIDataObjects[I].Assemble;

  DeleteTransactionSets;

  FData := FData + FGESegment.Assemble;
  FGESegment.Data := '';

  FLength := Length(FData);
  Result := FData;

  FState := ediAssembled;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroup.DeleteTransactionSet(Index: Integer);
begin
  DeleteEDIDataObject(Index);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroup.DeleteTransactionSet(TransactionSet: TEDITransactionSet);
begin
  DeleteEDIDataObject(TransactionSet);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroup.DeleteTransactionSets;
begin
  DeleteEDIDataObjects;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroup.DeleteTransactionSets(Index, Count: Integer);
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
  EDITransactionSet: TEDITransactionSet;
  {$ENDIF}
begin
  {$IFDEF OPTIMIZED_DISASSEMBLE}
  LLHeader := TEDIDataObjectLinkedListHeader.Create;
  try
  {$ENDIF}
    FGSSegment.Data := '';
    FGSSegment.DeleteElements;
    FGESegment.Data := '';
    FGESegment.DeleteElements;
    DeleteTransactionSets;
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
    if FGHSegmentId + FDelimiters.ED = Copy(FData, 1, Length(FGHSegmentId + FDelimiters.ED)) then
    begin
      // Search for Functional Group Header Segment Terminator
      SearchResult := StrSearch(FDelimiters.SD, FData, 1);
      if (SearchResult - StartPos) > 0 then // data exists
      begin
        FGSSegment.Data := Copy(FData, 1, (SearchResult + FDelimiters.SDLen) - 1);
        FGSSegment.Disassemble;
      end
      else
        raise EJclEDIError.CreateResRec(@RsEDIError021);
    end
    else
      raise EJclEDIError.CreateResRec(@RsEDIError022);
    // Search for Transaction Set Header
    SearchResult := StrSearch(FDelimiters.SD + TSHSegmentId + FDelimiters.ED, FData, StartPos);
    if SearchResult <= 0 then
      raise EJclEDIError.CreateResRec(@RsEDIError027);
    // Set next start position
    StartPos := SearchResult + FDelimiters.SDLen; // Move past the delimiter
    // Continue
    while SearchResult <> 0 do
    begin
      // Search for Transaction Set Trailer
      SearchResult := StrSearch(FDelimiters.SD + TSTSegmentId + FDelimiters.ED, FData, StartPos);
      if SearchResult <> 0 then
      begin
        // Set the next start position
        SearchResult := SearchResult + FDelimiters.SDLen; // Move past the delimiter
        // Search for the end of Transaction Set Trailer
        SearchResult := StrSearch(FDelimiters.SD, FData, SearchResult);
        if SearchResult <> 0 then
        begin
          {$IFDEF OPTIMIZED_DISASSEMBLE}
          EDITransactionSet := InternalCreateTransactionSet;
          EDITransactionSet.Data :=
            Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.SDLen));
          EDITransactionSet.Disassemble;
          LLHeader.AppendEDIDataObject(EDITransactionSet);
          {$ELSE}
          I := AddTransactionSet;
          FEDIDataObjects[I].Data :=
            Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.SDLen));
          FEDIDataObjects[I].Disassemble;
          {$ENDIF}
        end
        else
          raise EJclEDIError.CreateResRec(@RsEDIError028);
      end
      else
        raise EJclEDIError.CreateResRec(@RsEDIError029);
      // Set the next start position
      StartPos := SearchResult + FDelimiters.SDLen; // Move past the delimiter
      //
      // Verify the next record is a Transaction Set Header
      if (TSHSegmentId + FDelimiters.ED) <>
         Copy(FData, StartPos, (Length(TSHSegmentId) + FDelimiters.EDLen)) then
        Break;
    end;
    // Set the next start position
    StartPos := SearchResult + FDelimiters.SDLen; // Move past the delimiter
    // Find Functional Group Trailer Segment
    if (FGTSegmentId + FDelimiters.ED) =
      Copy(FData, StartPos, Length(FGTSegmentId + FDelimiters.ED)) then
    begin
      // Find Functional Group Trailer Segment Terminator
      SearchResult := StrSearch(FDelimiters.SD, FData, StartPos + FDelimiters.SDLen);
      if (SearchResult - StartPos) > 0 then // data exists
      begin
        FGESegment.Data := Copy(FData, StartPos, (SearchResult + FDelimiters.SDLen));
        FGESegment.Disassemble;
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
    // AddTransactionSets(LLHeader.ItemCount);
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

function TEDIFunctionalGroup.GetTransactionSet(Index: Integer): TEDITransactionSet;
begin
  Result := TEDITransactionSet(FEDIDataObjects[Index]);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.InsertTransactionSet(InsertIndex: Integer): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.InsertTransactionSet(InsertIndex: Integer;
  TransactionSet: TEDITransactionSet): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex, TransactionSet);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.InsertTransactionSets(InsertIndex: Integer;
  TransactionSetArray: TEDITransactionSetArray): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, TEDIDataObjectArray(TransactionSetArray));
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.InsertTransactionSets(InsertIndex, Count: Integer): Integer;
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
      Result := Parent.Delimiters;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.InternalCreateTransactionSet: TEDITransactionSet;
begin
  Result := TEDITransactionSet.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroup.InternalCreateHeaderTrailerSegments;
begin
  FGSSegment := TEDIFunctionalGroupSegment.Create(Self);
  FGESegment := TEDIFunctionalGroupSegment.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroup.SetTransactionSet(Index: Integer; TransactionSet: TEDITransactionSet);
begin
  TEDITransactionSet(FEDIDataObjects[Index]) := TransactionSet;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroup.SetGESegment(const GESegment: TEDIFunctionalGroupSegment);
begin
  FreeAndNil(FGESegment);
  FGESegment := GESegment;
  if Assigned(FGESegment) then
    FGESegment.Parent := Self;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroup.SetGSSegment(const GSSegment: TEDIFunctionalGroupSegment);
begin
  FreeAndNil(FGSSegment);
  FGSSegment := GSSegment;
  if Assigned(FGSSegment) then
    FGSSegment.Parent := Self;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.GetTransactionSets: TEDITransactionSetArray;
begin
  Result := TEDITransactionSetArray(FEDIDataObjects);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroup.InternalCreateEDIDataObject: TEDIDataObject;
begin
  Result := InternalCreateTransactionSet;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroup.SetTransactionSets(const Value: TEDITransactionSetArray);
begin
  TEDITransactionSetArray(FEDIDataObjects) := Value;
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
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIInterchangeControl.Destroy;
begin
  FISASegment.Free;
  FIEASegment.Free;
  FreeAndNil(FDelimiters);
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.AddFunctionalGroup: Integer;
begin
  Result := AddEDIDataObject;
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.AddFunctionalGroups(Count: Integer): Integer;
begin
  Result := AddEDIDataObjects(Count);
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.AppendFunctionalGroup(
  FunctionalGroup: TEDIFunctionalGroup): Integer;
begin
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

  FData := FISASegment.Assemble;
  FISASegment.Data := '';

  if Length(FEDIDataObjects) > 0 then
    for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
      if Assigned(FEDIDataObjects[I]) then
        FData := FData + FEDIDataObjects[I].Assemble;

  DeleteFunctionalGroups;

  FData := FData + FIEASegment.Assemble;
  FIEASegment.Data := '';

  FLength := Length(FData);
  Result := FData;

  FState := ediAssembled;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIInterchangeControl.DeleteFunctionalGroup(Index: Integer);
begin
  DeleteEDIDataObject(Index);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIInterchangeControl.DeleteFunctionalGroups;
begin
  DeleteEDIDataObjects;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIInterchangeControl.DeleteFunctionalGroups(Index, Count: Integer);
begin
  DeleteEDIDataObjects(Index, Count);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIInterchangeControl.Disassemble;
var
  I, StartPos, SearchResult: Integer;
  {$IFDEF OPTIMIZED_DISASSEMBLE}
  LLHeader: TEDIDataObjectLinkedListHeader;
  LLItem: TEDIDataObjectLinkedListItem;
  EDIFunctionalGroup: TEDIFunctionalGroup;
  {$ENDIF}
begin
  {$IFDEF OPTIMIZED_DISASSEMBLE}
  LLHeader := TEDIDataObjectLinkedListHeader.Create;
  try
  {$ENDIF}
    FISASegment.Data := '';
    FISASegment.DeleteElements;
    FIEASegment.Data := '';
    FIEASegment.DeleteElements;
    DeleteFunctionalGroups;

    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateResRec(@RsEDIError012);

    StartPos := 1;
    // Search for Interchange Control Header
    if ICHSegmentId + FDelimiters.ED = Copy(FData, 1, Length(ICHSegmentId + FDelimiters.ED)) then
    begin
      SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
      if (SearchResult - StartPos) > 0 then // data exists
      begin
        FISASegment.Data := Copy(FData, 1, (SearchResult + FDelimiters.SDLen) - 1);
        FISASegment.Disassemble;
      end
      else
        raise EJclEDIError.CreateResRec(@RsEDIError014);
    end
    else
      raise EJclEDIError.CreateResRec(@RsEDIError015);
    // Search for Functional Group Header
    SearchResult := StrSearch(FDelimiters.SD + FGHSegmentId + FDelimiters.ED, FData, StartPos);
    if SearchResult <= 0 then
      raise EJclEDIError.CreateResRec(@RsEDIError022);
    // Set next start positon
    StartPos := SearchResult + FDelimiters.SDLen; // Move past the delimiter
    // Continue
    while ((StartPos + Length(FGHSegmentId)) < Length(FData)) and (SearchResult > 0) do
    begin
      // Search for Functional Group Trailer
      SearchResult := StrSearch(FDelimiters.SD + FGTSegmentId + FDelimiters.ED, FData, StartPos);
      if SearchResult > 0 then
      begin
        // Set next start positon
        SearchResult := SearchResult + FDelimiters.SDLen; // Move past the delimiter
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
      if (FGHSegmentId + FDelimiters.ED) <>
        Copy(FData, StartPos, (Length(FGHSegmentId) + FDelimiters.EDLen)) then
        Break;
    end;
    // Verify the next record is a Interchange Control Trailer
    if (ICTSegmentId + FDelimiters.ED) =
      Copy(FData, StartPos, Length(ICTSegmentId + FDelimiters.ED)) then
    begin
      // Search for the end of Interchange Control Trailer Segment Terminator
      SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
      if (SearchResult - StartPos) > 0 then // data exists
      begin
        FIEASegment.Data := Copy(FData, StartPos, (SearchResult + FDelimiters.SDLen));
        FIEASegment.Disassemble;
      end
      else
        raise EJclEDIError.CreateResRec(@RsEDIError016);
    end
    else
      raise EJclEDIError.CreateResRec(@RsEDIError017);
    FData := '';
  {$IFDEF OPTIMIZED_DISASSEMBLE}
    // Add the objects in a chunk
    SetLength(FEDIDataObjects, Length(FEDIDataObjects) + LLHeader.ItemCount);
    // AddFunctionalGroups(LLHeader.ItemCount);
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

function TEDIInterchangeControl.GetFunctionalGroup(Index: Integer): TEDIFunctionalGroup;
begin
  Result := TEDIFunctionalGroup(FEDIDataObjects[Index]);
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
  Result := InsertEDIDataObject(InsertIndex);
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.InsertFunctionalGroups(InsertIndex, Count: Integer): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, Count);
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.InsertFunctionalGroups(InsertIndex: Integer;
  FunctionalGroupArray: TEDIFunctionalGroupArray): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, TEDIDataObjectArray(FunctionalGroupArray));
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIInterchangeControl.SetFunctionalGroup(Index: Integer;
  FunctionalGroup: TEDIFunctionalGroup);
begin
  TEDIFunctionalGroup(FEDIDataObjects[Index]) := FunctionalGroup;
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.InternalCreateFunctionalGroup: TEDIFunctionalGroup;
begin
  Result := TEDIFunctionalGroup.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIInterchangeControl.InternalCreateHeaderTrailerSegments;
begin
  FISASegment := TEDIInterchangeControlSegment.Create(Self);
  FIEASegment := TEDIInterchangeControlSegment.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIInterchangeControl.SetIEASegment(const IEASegment: TEDIInterchangeControlSegment);
begin
  FreeAndNil(FIEASegment);
  FIEASegment := IEASegment;
  if Assigned(FIEASegment) then
    FIEASegment.Parent := Self;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIInterchangeControl.SetISASegment(const ISASegment: TEDIInterchangeControlSegment);
begin
  FreeAndNil(FISASegment);
  FISASegment := ISASegment;
  if Assigned(FISASegment) then
    FISASegment.Parent := Self;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIInterchangeControl.DeleteFunctionalGroup(FunctionalGroup: TEDIFunctionalGroup);
begin
  DeleteEDIDataObject(FunctionalGroup);
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.GetFunctionalGroups: TEDIFunctionalGroupArray;
begin
  Result := TEDIFunctionalGroupArray(FEDIDataObjects);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIInterchangeControl.SetFunctionalGroups(const Value: TEDIFunctionalGroupArray);
begin
  TEDIFunctionalGroupArray(FEDIDataObjects) := Value;
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.InternalCreateEDIDataObject: TEDIDataObject;
begin
  Result := InternalCreateFunctionalGroup;
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControl.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
end;

//==================================================================================================
// TEDIFile
//==================================================================================================

constructor TEDIFile.Create(Parent: TEDIDataObject; InterchangeCount: Integer);
begin
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
begin
  FData := '';
  FLength := 0;
  Result := '';

  if Length(FEDIDataObjects) > 0 then
    for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
    begin
      if Assigned(FEDIDataObjects[I]) then
        FData := FData + FEDIDataObjects[I].Assemble;
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
    begin
      {$IFDEF OPTIMIZED_STRINGREPLACE}
      FData := JclEDI.StringReplace(FData, AnsiLineFeed, '', [rfReplaceAll]);
      {$ELSE}
      FData := SysUtils.StringReplace(FData, AnsiLineFeed, '', [rfReplaceAll]);
      {$ENDIF}
    end;
    StartPos := 1;
    // Search for Interchange Control Header
    if ICHSegmentId = Copy(FData, StartPos, Length(ICHSegmentId)) then
    begin
      if foVariableDelimiterDetection in FEDIFileOptions then
        if foUseAltDelimiterDetection in FEDIFileOptions then
          InternalAlternateDelimitersDetection(StartPos)
        else
          InternalDelimitersDetection(StartPos);
    end
    else
      raise EJclEDIError.CreateResRec(@RsEDIError015);
    // Continue
    while (StartPos + Length(ICHSegmentId)) < Length(FData) do
    begin
      // Search for Interchange Control Trailer
      SearchResult := StrSearch(FDelimiters.SD + ICTSegmentId + FDelimiters.ED, FData, StartPos);
      if SearchResult > 0 then
      begin
        SearchResult := SearchResult + FDelimiters.SDLen; // Move past the delimiter
        // Search for the end of Interchange Control Trailer
        SearchResult := StrSearch(FDelimiters.SD, FData, SearchResult);
        if SearchResult > 0 then
        begin
          {$IFDEF OPTIMIZED_DISASSEMBLE}
          EDIInterchangeControl := InternalCreateInterchangeControl;
          EDIInterchangeControl.Delimiters :=
            TEDIDelimiters.Create(FDelimiters.SD, FDelimiters.ED, FDelimiters.SS);
          EDIInterchangeControl.Data :=
            Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.SDLen));
          EDIInterchangeControl.Disassemble;
          LLHeader.AppendEDIDataObject(EDIInterchangeControl);
          {$ELSE}
          I := AddInterchange;
          FEDIDataObjects[I].Delimiters :=
            TEDIDelimiters.Create(FDelimiters.SD, FDelimiters.ED, FDelimiters.SS);
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
      // Verify the next record is an Interchange Control Header
      if ICHSegmentId = Copy(FData, StartPos, Length(ICHSegmentId)) then
      begin
        if (foVariableDelimiterDetection in FEDIFileOptions) then
          if foUseAltDelimiterDetection in FEDIFileOptions then
            InternalAlternateDelimitersDetection(StartPos)
          else
            InternalDelimitersDetection(StartPos);
      end
      else
      if (StartPos + Length(ICHSegmentId)) < Length(FData) then
        raise EJclEDIError.CreateResRec(@RsEDIError018);
    end;
    FData := '';
  {$IFDEF OPTIMIZED_DISASSEMBLE}
    // Add the objects in a chunk
    SetLength(FEDIDataObjects, Length(FEDIDataObjects) + LLHeader.ItemCount);
    // AddInterchanges(LLHeader.ItemCount);
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
  if FileName <> '' then
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
var
  I, SearchResult: Integer;
begin
  SearchResult := 1;
  FDelimiters.ED := Copy(FData, StartPos + Length(ICHSegmentId), 1);
  for I := 0 to 15 do
  begin
    SearchResult := StrSearch(FDelimiters.ED, FData, SearchResult);
    SearchResult := SearchResult + 1;
  end;
  FDelimiters.SS := Copy(FData, SearchResult, 1);
  FDelimiters.SD := Copy(FData, SearchResult + 1, 1);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFile.InternalAlternateDelimitersDetection(StartPos: Integer);
var
  SearchResult: Integer;
begin
  SearchResult := 1;
  FDelimiters.ED := Copy(FData, StartPos + Length(ICHSegmentId), 1);
  SearchResult := StrSearch(FGHSegmentId + FDelimiters.ED, FData, SearchResult);
  FDelimiters.SS := Copy(FData, SearchResult - 2, 1);
  FDelimiters.SD := Copy(FData, SearchResult - 1, 1);
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
  Result := TEDIDelimiters.Create;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFile.InternalCreateEDIDataObject: TEDIDataObject;
begin
  Result := InternalCreateInterchangeControl;
end;

//==================================================================================================
// TEDIElementSpec
//==================================================================================================

constructor TEDIElementSpec.Create(Parent: TEDIDataObject);
begin
  inherited Create(Parent);
  FReservedData := TStringList.Create;
  FId := '';
  FPosition := 0;
  FDescription := '';
  FRequirementDesignator := '';
  FType := '';
  FMinimumLength := 1;
  FMaximumLength := 1;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDIElementSpec.Destroy;
begin
  FReservedData.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDIElementSpec.Assemble: string;
begin
  if FId <> ElementSpecId_Reserved then
  begin
    if FId = '' then
      FId := 'Not Assigned';
    FReservedData.Values['Id'] := FId;
    FReservedData.Values['Position'] := IntToStr(FPosition);
    if FDescription = '' then
      FDescription := 'None';
    FReservedData.Values['Description'] := FDescription;
    if FNotes = '' then
      FNotes := 'None';
    FReservedData.Values['Notes'] := FNotes;
    if FRequirementDesignator = '' then
      FRequirementDesignator := 'O';
    FReservedData.Values['RequirementDesignator'] := FRequirementDesignator;
    if FType = '' then
      FType := 'AN';
    FReservedData.Values['Type'] := FType;
    FReservedData.Values['MinimumLength'] := IntToStr(FMinimumLength);
    FReservedData.Values['MaximumLength'] := IntToStr(FMaximumLength);
    FData := FReservedData.CommaText;
  end;
  FReservedData.Clear;
  Result := FData;
  FState := ediAssembled;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIElementSpec.Disassemble;
begin
  FReservedData.Clear;
  FReservedData.CommaText := FData;
  if FReservedData.Values['Id'] <> ElementSpecId_Reserved then
  begin
    FId := FReservedData.Values['Id'];
    if FId = '' then
      FId := 'Not Assigned';
    FPosition := StrToInt(FReservedData.Values['Position']);
    FDescription := FReservedData.Values['Description'];
    if FDescription = '' then
      FDescription := 'None';
    FNotes := FReservedData.Values['Notes'];
    if FNotes = '' then
      FNotes := 'None';
    FRequirementDesignator := FReservedData.Values['RequirementDesignator'];
    if FRequirementDesignator = '' then
      FRequirementDesignator := 'O';
    FType := FReservedData.Values['Type'];
    if FType = '' then
      FType := 'AN';
    FMinimumLength := StrToInt(FReservedData.Values['MinimumLength']);
    FMaximumLength := StrToInt(FReservedData.Values['MaximumLength']);
  end;
  FState := ediDissassembled;
end;

//==================================================================================================
// TEDISegmentSpec
//==================================================================================================

constructor TEDISegmentSpec.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  FReservedData := TStringList.Create;
  FSegmentID := 'Not Assigned';
  FPosition := 0;
  FDescription := 'None';
  FRequirementDesignator := 'O';
  FSection := '?';
  FMaximumUsage := 999;
  FOwnerLoopId := 'N/A';
  FParentLoopId := 'N/A';
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISegmentSpec.Destroy;
begin
  FReservedData.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDISegmentSpec.Assemble: string;
begin
  // Insert Segment Spec as Element[0]
  InsertElement(0);
  TEDIElementSpec(FEDIDataObjects[0]).Id := ElementSpecId_Reserved;
  AssembleReservedData(FReservedData);
  FEDIDataObjects[0].Data := FReservedData.CommaText;
  FReservedData.Clear;
  //
  Result := inherited Assemble;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISegmentSpec.AssembleReservedData(ReservedData: TStrings);
begin
  with FReservedData do
  begin
    Values['Id'] := ElementSpecId_Reserved;
    Values['Position'] := IntToStr(FPosition);
    Values['Description'] := FDescription;
    Values['Notes'] := FNotes;
    Values['Section'] := FSection;
    Values['RequirementDesignator'] := FRequirementDesignator;
    Values['MaximumUsage'] := IntToStr(FMaximumUsage);
    if FOwnerLoopId = '' then
      FOwnerLoopId := NA_LoopId;
    Values['OwnerLoopId'] := FOwnerLoopId;
    if FParentLoopId = '' then
      FParentLoopId := NA_LoopId;
    Values['ParentLoopId'] := FParentLoopId;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISegmentSpec.Disassemble;
begin
  inherited Disassemble;
  // Element[0] is always the Segment Spec
  FReservedData.Clear;
  FReservedData.CommaText := FEDIDataObjects[0].Data;
  DisassembleReservedData(FReservedData);
  DeleteElement(0);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISegmentSpec.ValidateElementIndexPositions;
var
  I: Integer;
begin
  for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
    TEDIElementSpec(FEDIDataObjects[I]).Position := I + 1;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISegmentSpec.DisassembleReservedData(ReservedData: TStrings);
begin
  with FReservedData do
  begin
    // FSegmentId already set by the inherited Disassemble
    FPosition := StrToInt(Values['Position']);
    FDescription := Values['Description'];
    FNotes := Values['Notes'];
    FSection := Values['Section'];
    FRequirementDesignator := Values['RequirementDesignator'];
    FMaximumUsage := StrToInt(Values['MaximumUsage']);
    FOwnerLoopId := Values['OwnerLoopId'];
    if FOwnerLoopId = '' then
      FOwnerLoopId := NA_LoopId;
    FParentLoopId := Values['ParentLoopId'];
    if FParentLoopId = '' then
      FParentLoopId := NA_LoopId;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDISegmentSpec.InternalCreateElement: TEDIElement;
begin
  Result := TEDIElementSpec.Create(Self);
end;

//==================================================================================================
// TEDITransactionSetSegmentSpec
//==================================================================================================

constructor TEDITransactionSetSegmentSpec.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  if Assigned(Parent) and (Parent is TEDITransactionSet) then
    FParent := Parent;
  FRequirementDesignator := 'M';
  FMaximumUsage := 1;
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSetSegmentSpec.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := inherited InternalAssignDelimiters;
end;

//==================================================================================================
// TEDITransactionSetSegmentSTSpec
//==================================================================================================

constructor TEDITransactionSetSegmentSTSpec.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  FSegmentID := TSHSegmentId;
  FPosition := 0;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDITransactionSetSegmentSTSpec.AssembleReservedData(ReservedData: TStrings);
var
  Spec: TEDITransactionSetSpec;
begin
  if Parent is TEDITransactionSetSpec then
  begin
    Spec := TEDITransactionSetSpec(Parent);
    if Spec.TransactionSetId = '' then
      Spec.TransactionSetId := cUnknown;
    ReservedData.Values['TransSetId'] := Spec.TransactionSetId;
    if Spec.TSDescription = '' then
      Spec.TSDescription := 'None';
    ReservedData.Values['TransSetDesc'] := Spec.TSDescription;
  end;
  inherited AssembleReservedData(ReservedData);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDITransactionSetSegmentSTSpec.DisassembleReservedData(ReservedData: TStrings);
var
  Spec: TEDITransactionSetSpec;
begin
  inherited DisassembleReservedData(ReservedData);
  if Parent is TEDITransactionSetSpec then
  begin
    Spec := TEDITransactionSetSpec(Parent);
    Spec.TransactionSetId := ReservedData.Values['TransSetId'];
    if Spec.TransactionSetId = '' then
      Spec.TransactionSetId := cUnknown;
    Spec.TSDescription := ReservedData.Values['TransSetDesc'];
    if Spec.TSDescription = '' then
      Spec.TSDescription := 'None';
  end;
end;

//==================================================================================================
// TEDIFunctionalGroupSegmentSpec
//==================================================================================================

constructor TEDIFunctionalGroupSegmentSpec.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
    FParent := Parent;
  FRequirementDesignator := 'M';
  FMaximumUsage := 1;
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroupSegmentSpec.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  // Attempt to assign the delimiters
  if not Assigned(FDelimiters) then
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
        Result := Parent.Parent.Delimiters;
    end;
end;

//==================================================================================================
// TEDIFunctionalGroupSegmentGSSpec
//==================================================================================================

constructor TEDIFunctionalGroupSegmentGSSpec.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  FSegmentID := FGHSegmentId;
  FPosition := -1;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroupSegmentGSSpec.AssembleReservedData(ReservedData: TStrings);
var
  Spec: TEDIFunctionalGroupSpec;
begin
  if Parent is TEDIFunctionalGroupSpec then
  begin
    Spec := TEDIFunctionalGroupSpec(Parent);
    if Spec.FunctionalGroupId = '' then
      Spec.FunctionalGroupId := cUnknown;
    ReservedData.Values['FunctionalGroupId'] := Spec.FunctionalGroupId;
    if Spec.FGDescription = '' then
      Spec.FGDescription := 'None';
    ReservedData.Values['FGDescription'] := Spec.FGDescription;
    if Spec.AgencyCodeId = '' then
      Spec.AgencyCodeId := cUnknown;
    ReservedData.Values['AgencyCodeId'] := Spec.AgencyCodeId;
    if Spec.VersionReleaseId = '' then
      Spec.VersionReleaseId := cUnknown;
    ReservedData.Values['VersionReleaseId'] := Spec.VersionReleaseId;
  end;
  inherited AssembleReservedData(ReservedData);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFunctionalGroupSegmentGSSpec.DisassembleReservedData(ReservedData: TStrings);
var
  Spec: TEDIFunctionalGroupSpec;
begin
  inherited;

  if Parent is TEDIFunctionalGroupSpec then
  begin
    Spec := TEDIFunctionalGroupSpec(Parent);
    Spec.FunctionalGroupId := ReservedData.Values['FunctionalGroupId'];
    if Spec.FunctionalGroupId = '' then
      Spec.FunctionalGroupId := cUnknown;
    Spec.FGDescription := ReservedData.Values['FGDescription'];
    if Spec.FGDescription = '' then
      Spec.FGDescription := 'None';
    Spec.AgencyCodeId := ReservedData.Values['AgencyCodeId'];
    if Spec.AgencyCodeId = '' then
      Spec.AgencyCodeId := cUnknown;
    Spec.VersionReleaseId := ReservedData.Values['VersionReleaseId'];
    if Spec.VersionReleaseId = '' then
      Spec.VersionReleaseId := cUnknown;
  end;
end;

//==================================================================================================
// TEDIInterchangeControlSegmentSpec
//==================================================================================================

constructor TEDIInterchangeControlSegmentSpec.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
    FParent := Parent;
  FRequirementDesignator := 'M';
  FMaximumUsage := 1;
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControlSegmentSpec.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  // Attempt to assign the delimiters
  if not Assigned(FDelimiters) then
    // Get the delimiters from the interchange control
    if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
      Result := Parent.Delimiters;
end;

//==================================================================================================
// TEDIInterchangeControlSegmentISASpec
//==================================================================================================

constructor TEDIInterchangeControlSegmentISASpec.Create(Parent: TEDIDataObject;
  ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  FSegmentID := ICHSegmentId;
  FPosition := -2;
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControlSegmentISASpec.Assemble: string;
begin
  // Because the last element carries specification data and not the subelement separator
  // the subelement separator must be added as an additional element.
  Result := inherited Assemble;
  Result := Copy(Result, 1, Length(Result)-1) + FDelimiters.ED + FDelimiters.SS + FDelimiters.SD;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIInterchangeControlSegmentISASpec.AssembleReservedData(ReservedData: TStrings);
var
  Spec: TEDIInterchangeControlSpec;
begin
  if Parent is TEDIInterchangeControlSpec then
  begin
    Spec := TEDIInterchangeControlSpec(Parent);
    if Spec.StandardId = '' then
      Spec.StandardId := cUnknown;
    ReservedData.Values[cStandardId] := Spec.StandardId;
    if Spec.VersionId = '' then
      Spec.VersionId := cUnknown;
    ReservedData.Values[cVersionId] := Spec.VersionId;
    if Spec.ICDescription = '' then
      Spec.ICDescription := 'None';
    ReservedData.Values[cICDescription] := Spec.ICDescription;
  end;
  inherited AssembleReservedData(ReservedData);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIInterchangeControlSegmentISASpec.Disassemble;
var
  SearchResult: Integer;
begin
  // Because the subelement separator was added as an additional element it must now be removed.
  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateResRec(@RsEDIError035);
  end;
  SearchResult := StrSearch(FDelimiters.ED + FDelimiters.SS, FData, 1);
  if SearchResult <> 0 then
    FData := StringReplace(FData, FDelimiters.ED + FDelimiters.SS, '', [rfReplaceAll]);
  inherited Disassemble;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIInterchangeControlSegmentISASpec.DisassembleReservedData(ReservedData: TStrings);
var
  Spec: TEDIInterchangeControlSpec;
begin
  inherited DisassembleReservedData(ReservedData);
  if Parent is TEDIInterchangeControlSpec then
  begin
    Spec := TEDIInterchangeControlSpec(Parent);
    Spec.StandardId := ReservedData.Values[cStandardId];
    if Spec.StandardId = '' then
      Spec.StandardId := cUnknown;
    Spec.VersionId := ReservedData.Values[cVersionId];
    if Spec.VersionId = '' then
      Spec.VersionId := cUnknown;
    Spec.ICDescription := ReservedData.Values[cICDescription];
    if Spec.ICDescription = '' then
      Spec.ICDescription := 'None';
  end;
end;

//==================================================================================================
// TEDITransactionSetSpec
//==================================================================================================

procedure TEDITransactionSetSpec.InternalCreateHeaderTrailerSegments;
begin
  FSTSegment := TEDITransactionSetSegment(TEDITransactionSetSegmentSTSpec.Create(Self));
  FSESegment := TEDITransactionSetSegment(TEDITransactionSetSegmentSpec.Create(Self));
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSetSpec.InternalCreateSegment: TEDISegment;
begin
  Result := TEDISegmentSpec.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDITransactionSetSpec.ValidateSegmentIndexPositions;
var
  I: Integer;
begin
  for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
  begin
    TEDISegmentSpec(FEDIDataObjects[I]).Position := I + 1;
    TEDISegmentSpec(FEDIDataObjects[I]).ValidateElementIndexPositions;
  end;
end;

//==================================================================================================
// TEDIFunctionalGroupSpec
//==================================================================================================

procedure TEDIFunctionalGroupSpec.InternalCreateHeaderTrailerSegments;
begin
  FGSSegment := TEDIFunctionalGroupSegment(TEDIFunctionalGroupSegmentGSSpec.Create(Self));
  FGESegment := TEDIFunctionalGroupSegment(TEDIFunctionalGroupSegmentSpec.Create(Self));
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroupSpec.InternalCreateTransactionSet: TEDITransactionSet;
begin
  Result := TEDITransactionSetSpec.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFunctionalGroupSpec.FindTransactionSetSpec(
  TransactionSetId: string): TEDITransactionSetSpec;
var
  I: Integer;
  EDITransactionSetSpec: TEDITransactionSetSpec;
begin
  Result := nil;
  for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
  begin
    EDITransactionSetSpec := TEDITransactionSetSpec(FEDIDataObjects[I]);
    if TransactionSetId = EDITransactionSetSpec.TransactionSetId then
    begin
      Result := EDITransactionSetSpec;
      Break;
    end;
  end;
end;

//==================================================================================================
// TEDIInterchangeControlSpec
//==================================================================================================

procedure TEDIInterchangeControlSpec.InternalCreateHeaderTrailerSegments;
begin
  FISASegment := TEDIInterchangeControlSegment(TEDIInterchangeControlSegmentISASpec.Create(Self));
  FIEASegment := TEDIInterchangeControlSegment(TEDIInterchangeControlSegmentSpec.Create(Self));
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControlSpec.InternalCreateFunctionalGroup: TEDIFunctionalGroup;
begin
  Result := TEDIFunctionalGroupSpec.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControlSpec.FindTransactionSetSpec(FunctionalGroupId, AgencyCodeId,
  VersionReleaseId, TransactionSetId: string): TEDITransactionSetSpec;
var
  EDIFunctionalGroupSpec: TEDIFunctionalGroupSpec;
begin
  Result := nil;
  EDIFunctionalGroupSpec := FindFunctionalGroupSpec(FunctionalGroupId, AgencyCodeId,
    VersionReleaseId);
  if EDIFunctionalGroupSpec <> nil then
    Result := EDIFunctionalGroupSpec.FindTransactionSetSpec(TransactionSetId);
end;

//--------------------------------------------------------------------------------------------------

function TEDIInterchangeControlSpec.FindFunctionalGroupSpec(FunctionalGroupId, AgencyCodeId,
  VersionReleaseId: string): TEDIFunctionalGroupSpec;
var
  F: Integer;
  EDIFunctionalGroupSpec: TEDIFunctionalGroupSpec;
begin
  Result := nil;
  for F := Low(FEDIDataObjects) to High(FEDIDataObjects) do
  begin
    EDIFunctionalGroupSpec := TEDIFunctionalGroupSpec(FEDIDataObjects[F]);
    if (FunctionalGroupId = EDIFunctionalGroupSpec.FunctionalGroupId) and
      (AgencyCodeId = EDIFunctionalGroupSpec.AgencyCodeId) and
      (VersionReleaseId = EDIFunctionalGroupSpec.VersionReleaseId) then
    begin
      Result := EDIFunctionalGroupSpec;
      Exit;
    end;
  end;
end;

//==================================================================================================
// TEDIFileSpec
//==================================================================================================

constructor TEDIFileSpec.Create(Parent: TEDIDataObject; InterchangeCount: Integer);
begin
  inherited Create(Parent, InterchangeCount);
  FEDIFileOptions := [foVariableDelimiterDetection, foUseAltDelimiterDetection];
end;

//--------------------------------------------------------------------------------------------------

procedure TEDIFileSpec.InternalDelimitersDetection(StartPos: Integer);
begin
  InternalAlternateDelimitersDetection(StartPos);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFileSpec.InternalCreateInterchangeControl: TEDIInterchangeControl;
begin
  Result := TEDIInterchangeControlSpec.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFileSpec.FindTransactionSetSpec(StandardId, VersionId, FunctionalGroupId, AgencyCodeId,
  VersionReleaseId, TransactionSetId: string): TEDITransactionSetSpec;
var
  EDIFunctionalGroupSpec: TEDIFunctionalGroupSpec;
begin
  Result := nil;
  EDIFunctionalGroupSpec := FindFunctionalGroupSpec(StandardId, VersionId, FunctionalGroupId,
    AgencyCodeId, VersionReleaseId);
  if EDIFunctionalGroupSpec <> nil then
    Result := EDIFunctionalGroupSpec.FindTransactionSetSpec(TransactionSetId);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFileSpec.FindFunctionalGroupSpec(StandardId, VersionId, FunctionalGroupId,
  AgencyCodeId, VersionReleaseId: string): TEDIFunctionalGroupSpec;
var
  EDIInterchangeControlSpec: TEDIInterchangeControlSpec;
begin
  Result := nil;
  EDIInterchangeControlSpec := FindInterchangeControlSpec(StandardId, VersionId);
  if EDIInterchangeControlSpec <> nil then
    Result := EDIInterchangeControlSpec.FindFunctionalGroupSpec(FunctionalGroupId,
      AgencyCodeId, VersionReleaseId);
end;

//--------------------------------------------------------------------------------------------------

function TEDIFileSpec.FindInterchangeControlSpec(StandardId,
  VersionId: string): TEDIInterchangeControlSpec;
var
  I: Integer;
  EDIInterchangeControlSpec: TEDIInterchangeControlSpec;
begin
  Result := nil;
  for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
  begin
    EDIInterchangeControlSpec := TEDIInterchangeControlSpec(FEDIDataObjects[I]);
    if (EDIInterchangeControlSpec.StandardId = StandardId) and
       (EDIInterchangeControlSpec.VersionId = VersionId) then
      Result := EDIInterchangeControlSpec;
  end;
end;

//==================================================================================================
// TEDITransactionSetLoop
//==================================================================================================

constructor TEDITransactionSetLoop.Create(Parent: TEDIDataObject);
begin
  inherited Create(Parent);
  if Assigned(Parent) and (Parent is TEDITransactionSet) then
    FParentTransactionSet := TEDITransactionSet(Parent)
  else
  if Assigned(Parent) and (Parent is TEDITransactionSetLoop) then
    FParentTransactionSet := TEDITransactionSetLoop(Parent).ParentTransactionSet
  else
    FParentTransactionSet := nil;
  FEDIDOT := ediLoop;
  SetLength(FEDIDataObjects, 0);
end;

//--------------------------------------------------------------------------------------------------

destructor TEDITransactionSetLoop.Destroy;
begin
  DeleteEDIDataObjects;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSetLoop.AddLoop(OwnerLoopId, ParentLoopId: string): Integer;
begin
  SetLength(FEDIDataObjects, Length(FEDIDataObjects) + 1);
  FEDIDataObjects[High(FEDIDataObjects)] := TEDITransactionSetLoop.Create(Self);
  TEDITransactionSetLoop(FEDIDataObjects[High(FEDIDataObjects)]).OwnerLoopId := OwnerLoopId;
  TEDITransactionSetLoop(FEDIDataObjects[High(FEDIDataObjects)]).ParentLoopId := ParentLoopId;
  Result := High(FEDIDataObjects);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDITransactionSetLoop.AppendSegment(Segment: TEDISegment);
begin
  SetLength(FEDIDataObjects, Length(FEDIDataObjects) + 1);
  FEDIDataObjects[High(FEDIDataObjects)] := Segment;
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSetLoop.Assemble: string;
begin
  Result := '';
end;

//--------------------------------------------------------------------------------------------------

procedure TEDITransactionSetLoop.DeleteEDIDataObjects;
var
  I: Integer;
begin
  for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
    if Assigned(FEDIDataObjects[I]) then
      try
        // Delete
        if FEDIDataObjects[I] is TEDITransactionSetLoop then
          FreeAndNil(FEDIDataObjects[I])
        else
          // Do not free segments
          FEDIDataObjects[I] := nil;
      except
        // This exception block was put here to capture the case where FEDIDataObjects[I] was
        // actually destroyed prior to destroying this object.
        FEDIDataObjects[I] := nil;
      end;
  // Resize
  SetLength(FEDIDataObjects, 0);
end;

//--------------------------------------------------------------------------------------------------

procedure TEDITransactionSetLoop.Disassemble;
begin
  // Do Nothing
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSetLoop.GetEDIDataObject(Index: Integer): TEDIDataObject;
begin
  if Length(FEDIDataObjects) > 0 then
    if Index >= Low(FEDIDataObjects) then
      if Index <= High(FEDIDataObjects) then
      begin
        if not Assigned(FEDIDataObjects[Index]) then
          raise EJclEDIError.CreateResRecFmt(@RsEDIError039, [IntToStr(Index)]);
        Result := FEDIDataObjects[Index];
      end
      else
        raise EJclEDIError.CreateResRecFmt(@RsEDIError040, [IntToStr(Index)])
    else
      raise EJclEDIError.CreateResRecFmt(@RsEDIError041, [IntToStr(Index)])
  else
    raise EJclEDIError.CreateResRecFmt(@RsEDIError042, [IntToStr(Index)]);
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSetLoop.FindLoop(LoopId: string;
  var StartIndex: Integer): TEDITransactionSetLoop;
var
  I, J: Integer;
begin
  Result := nil;
  J := StartIndex;
  for I := StartIndex to High(FEDIDataObjects) do
  begin
    StartIndex := I;
    if FEDIDataObjects[I] is TEDITransactionSetLoop then
    begin
      Result := TEDITransactionSetLoop(GetEDIDataObject(I));
      if Result.OwnerLoopId = LoopId then
      begin
        Inc(StartIndex);
        Break;
      end;
      Result := nil;
    end;
  end;
  if Result = nil then
    StartIndex := J;
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSetLoop.FindSegment(SegmentId: string; var StartIndex: Integer): TEDISegment;
var
  I, J: Integer;
begin
  Result := nil;
  J := StartIndex;
  for I := StartIndex to High(FEDIDataObjects) do
  begin
    StartIndex := I;
    if FEDIDataObjects[I] is TEDISegment then
    begin
      Result := TEDISegment(GetEDIDataObject(I));
      if Result.SegmentId = SegmentId then
      begin
        Inc(StartIndex);
        Break;
      end;
      Result := nil;
    end;
  end;
  if Result = nil then
    StartIndex := J;
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSetLoop.FindSegment(SegmentId: string; var StartIndex: Integer;
  ElementConditions: TStrings): TEDISegment;
var
  I, TrueCount, ElementIndex: Integer;
  Name: string;
begin
  Result := FindSegment(SegmentId, StartIndex);
  while Result <> nil do
  begin
    TrueCount := 0;
    for I := 0 to ElementConditions.Count - 1 do
    begin
      Name := ElementConditions.Names[I];
      ElementIndex := StrToInt(Name);
      if Result[ElementIndex].Data = ElementConditions.Values[Name] then
        Inc(TrueCount);
    end;
    if TrueCount = ElementConditions.Count then
      Break;
    Result := FindSegment(SegmentId, StartIndex);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDITransactionSetLoop.SetEDIDataObject(Index: Integer; EDIDataObject: TEDIDataObject);
begin
  if Length(FEDIDataObjects) > 0 then
    if Index >= Low(FEDIDataObjects) then
      if Index <= High(FEDIDataObjects) then
      begin
        FreeAndNil(FEDIDataObjects[Index]);
        FEDIDataObjects[Index] := EDIDataObject;
      end
      else
        raise EJclEDIError.CreateResRecFmt(@RsEDIError043, [IntToStr(Index)])
    else
      raise EJclEDIError.CreateResRecFmt(@RsEDIError044, [IntToStr(Index)])
  else
    raise EJclEDIError.CreateResRecFmt(@RsEDIError045, [IntToStr(Index)]);
end;

function TEDITransactionSetLoop.GetCount: Integer;
begin
  Result := Length(FEDIDataObjects);
end;

//==================================================================================================
// TEDILoopStack
//==================================================================================================

constructor TEDILoopStack.Create;
begin
  inherited Create;
  SetLength(FEDILoopStack, 0);
end;

//--------------------------------------------------------------------------------------------------

destructor TEDILoopStack.Destroy;
var
  I: Integer;
begin
  for I := Low(FEDILoopStack) to High(FEDILoopStack) do
    FEDILoopStack[I].Loop := nil;
  SetLength(FEDILoopStack, 0);
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDILoopStack.Add(SegmentId, OwnerLoopId, ParentLoopId: string;
  StartIndex: Integer; Loop: TEDITransactionSetLoop): Integer;
begin
  // Add to loop stack
  SetLength(FEDILoopStack, Length(FEDILoopStack) + 1);
  FEDILoopStack[High(FEDILoopStack)].SegmentId := SegmentId;
  FEDILoopStack[High(FEDILoopStack)].OwnerLoopId := OwnerLoopId;
  FEDILoopStack[High(FEDILoopStack)].ParentLoopId := ParentLoopId;
  FEDILoopStack[High(FEDILoopStack)].SpecStartIndex := StartIndex;
  FEDILoopStack[High(FEDILoopStack)].Loop := Loop;
  Result := High(FEDILoopStack);
end;

//--------------------------------------------------------------------------------------------------

function TEDILoopStack.Debug: string;
var
  I: Integer;
begin
  Result := 'Loop Stack' + #13#10;
  for I := 0 to High(FEDILoopStack) do
    Result := Result + FEDILoopStack[I].SegmentId + ', ' +
      FEDILoopStack[I].OwnerLoopId + ', ' +
      FEDILoopStack[I].ParentLoopId + ', ' +
      IntToStr(FEDILoopStack[I].SpecStartIndex) + #13#10;
end;

//--------------------------------------------------------------------------------------------------

function TEDILoopStack.GetStackRecord: TEDILoopStackRecord;
begin
  Result := FEDILoopStack[High(FEDILoopStack)];
end;

//--------------------------------------------------------------------------------------------------

function TEDILoopStack.GetSafeStackIndex(Index: Integer): Integer;
begin
  if Length(FEDILoopStack) > 0 then
  begin
    if Index >= Low(FEDILoopStack) then
    begin
      if Index <= High(FEDILoopStack) then
        Result := Index
      else
        Result := High(FEDILoopStack);
    end
    else
      Result := Low(FEDILoopStack);
  end
  else
    raise EJclEDIError.CreateResRecFmt(@RsEDIError057, [IntToStr(Index)]);
end;

//--------------------------------------------------------------------------------------------------

function TEDILoopStack.GetStackRecord(Index: Integer): TEDILoopStackRecord;
begin
  if Length(FEDILoopStack) > 0 then
    if Index >= Low(FEDILoopStack) then
      if Index <= High(FEDILoopStack) then
      begin
        if not Assigned(FEDILoopStack[Index].Loop) then
          raise EJclEDIError.CreateResRecFmt(@RsEDIError053, [IntToStr(Index)]);
        Result := FEDILoopStack[Index];
      end
      else
        raise EJclEDIError.CreateResRecFmt(@RsEDIError054, [IntToStr(Index)])
    else
      raise EJclEDIError.CreateResRecFmt(@RsEDIError055, [IntToStr(Index)])
  else
    raise EJclEDIError.CreateResRecFmt(@RsEDIError056, [IntToStr(Index)]);
end;

//--------------------------------------------------------------------------------------------------

function TEDILoopStack.GetStackSize: Integer;
begin
  Result := Length(FEDILoopStack);
end;

//--------------------------------------------------------------------------------------------------

function TEDILoopStack.SetStackPointer(OwnerLoopId, ParentLoopId: string): Integer;
var
  I: Integer;
begin
  FStackResized := False;
  FAltStackPointer := False;
  Result := -1; // Entry not found
  // Find the loop in the stack
  for I := High(FEDILoopStack) downto 0 do
  begin
    if (OwnerLoopId = FEDILoopStack[I].OwnerLoopId) and
      (ParentLoopId = FEDILoopStack[I].ParentLoopId) then
    begin
      Result := I;
      // Resize loop stack if entry found is less than high entry
      if I < High(FEDILoopStack) then
      begin
        SetLength(FEDILoopStack, I + 1);
        FStackResized := True;
      end;
      Break;
    end;
  end;
  // Check if an exact entry was found
  if Result = -1 then
  begin
    // Find the parent loop in the stack
    for I := High(FEDILoopStack) downto 0 do
    begin
      if (ParentLoopId = FEDILoopStack[I].ParentLoopId) and
        (FEDILoopStack[I].OwnerLoopId <> NA_LoopId) then
      begin
        FAltStackPointer := True;
        Result := GetSafeStackIndex(I);
        // Resize loop stack if entry found is less than high entry
        if I < High(FEDILoopStack) then
        begin
          SetLength(FEDILoopStack, I + 1);
          FStackResized := True;
        end;
        Break;
      end;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDILoopStack.Update(SegmentId, OwnerLoopId, ParentLoopId: string;
  StartIndex: Integer; Loop: TEDITransactionSetLoop);
begin
  FEDILoopStack[High(FEDILoopStack)].SegmentId := SegmentId;
  FEDILoopStack[High(FEDILoopStack)].OwnerLoopId := OwnerLoopId;
  FEDILoopStack[High(FEDILoopStack)].ParentLoopId := ParentLoopId;
  FEDILoopStack[High(FEDILoopStack)].SpecStartIndex := StartIndex;
  FEDILoopStack[High(FEDILoopStack)].Loop := Loop;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDILoopStack.UpdateLoopReference(Loop: TEDITransactionSetLoop);
begin
  FEDILoopStack[High(FEDILoopStack)].Loop := Loop;
end;

//==================================================================================================
// TEDITransactionSetDocument
//==================================================================================================

constructor TEDITransactionSetDocument.Create(Parent: TEDIDataObject;
  EDITransactionSet: TEDITransactionSet;
  EDITransactionSetSpec: TEDITransactionSetSpec);
begin
  inherited Create(Parent);
  FEDILoopStack := TEDILoopStack.Create;
  FEDITransactionSet := EDITransactionSet;
  FEDITransactionSetSpec := EDITransactionSetSpec;
  FEDITSDOptions := [];
end;

//--------------------------------------------------------------------------------------------------

destructor TEDITransactionSetDocument.Destroy;
begin
  FreeAndNil(FEDILoopStack);
  FEDITransactionSet := nil;
  FEDITransactionSetSpec := nil;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDITransactionSetDocument.FormatDocument;
var
  I, J: Integer;
  LSR: TEDILoopStackRecord;
  LoopRepeated: Boolean;
  DataSegment: TEDISegment;
  SpecSegment: TEDISegmentSpec;
  EDIFunctionalGroup: TEDIFunctionalGroup;
  EDIFunctionalGroupSpec: TEDIFunctionalGroupSpec;
  EDIInterchangeControl: TEDIInterchangeControl;
  EDIInterchangeControlSpec: TEDIInterchangeControlSpec;
begin
  I := 0;
  J := 0;
  if doLinkSpecToDataObject in FEDITSDOptions then
  begin
    FEDITransactionSet.SpecPointer := FEDITransactionSetSpec;
    FEDITransactionSet.SegmentST.SpecPointer := FEDITransactionSetSpec.SegmentST;
    SetSpecificationPointers(FEDITransactionSet.SegmentST, FEDITransactionSetSpec.SegmentSE);
    FEDITransactionSet.SegmentSE.SpecPointer := FEDITransactionSetSpec.SegmentSE;
    SetSpecificationPointers(FEDITransactionSet.SegmentST, FEDITransactionSetSpec.SegmentSE);
    if FEDITransactionSet.Parent <> nil then
    begin
      EDIFunctionalGroup := TEDIFunctionalGroup(FEDITransactionSet.Parent);
      EDIFunctionalGroupSpec := TEDIFunctionalGroupSpec(FEDITransactionSetSpec.Parent);
      EDIFunctionalGroup.SpecPointer := EDIFunctionalGroupSpec;
      EDIFunctionalGroup.SegmentGS.SpecPointer := EDIFunctionalGroupSpec.SegmentGS;
      SetSpecificationPointers(EDIFunctionalGroup.SegmentGS, EDIFunctionalGroupSpec.SegmentGS);
      EDIFunctionalGroup.SegmentGE.SpecPointer := EDIFunctionalGroupSpec.SegmentGE;
      SetSpecificationPointers(EDIFunctionalGroup.SegmentGE, EDIFunctionalGroupSpec.SegmentGE);
      if EDIFunctionalGroup.Parent <> nil then
      begin
        EDIInterchangeControl := TEDIInterchangeControl(EDIFunctionalGroup.Parent);
        EDIInterchangeControlSpec := TEDIInterchangeControlSpec(EDIFunctionalGroupSpec.Parent);
        EDIInterchangeControl.SpecPointer := EDIInterchangeControlSpec;
        EDIInterchangeControl.SegmentISA.SpecPointer := EDIInterchangeControlSpec.SegmentISA;
        SetSpecificationPointers(EDIInterchangeControl.SegmentISA, EDIInterchangeControlSpec.SegmentISA);
        EDIInterchangeControl.SegmentIEA.SpecPointer := EDIInterchangeControlSpec.SegmentIEA;
        SetSpecificationPointers(EDIInterchangeControl.SegmentIEA, EDIInterchangeControlSpec.SegmentIEA);
      end;
    end;
  end;
  // Initialize the stack
  LSR := ValidateLoopStack(FEDITransactionSet.Segment[I].SegmentID,
    NA_LoopId, NA_LoopId, 0, Self, False);
  //
  while (I <= High(FEDITransactionSet.Segments)) and (J <= High(FEDITransactionSetSpec.Segments)) do
  begin
    LoopRepeated := False;
    DataSegment := FEDITransactionSet.Segment[I];
    // If loop has repeated then move the spec index back
    J := ValidateSegSpecIndex(DataSegment.SegmentID, J, LoopRepeated);
    // Check current segment against segment spec
    SpecSegment := TEDISegmentSpec(FEDITransactionSetSpec.Segment[J]);
    if DataSegment.SegmentID = SpecSegment.SegmentID then
    begin
      // Retrieve the correct record to use from the stack
      LSR := ValidateLoopStack(SpecSegment.SegmentID, SpecSegment.OwnerLoopId,
        SpecSegment.ParentLoopId, J, LSR.Loop, LoopRepeated);
      //
      // Debug - Keep the following line here in case someone wants to debug what happens to the stack.
      // ShowMessage('Current Data Segment: [' + IntToStr(I) + '] ' + DataSegment.SegmentID + #13#10 +
      //             'Current Spec Segment: [' + IntToStr(J) + '] ' + SpecSegment.SegmentID + #13#10 +
      //             FEDILoopStack.Debug);
      //
      // Do error checking and data validation in decendent class
      ValidateData(Self, FEDILoopStack, DataSegment, SpecSegment, I, J, FErrorOccured);
      if FErrorOccured then
        Exit;
      // Process Segment Id
      LSR.Loop.AppendSegment(DataSegment);
      //
      if doLinkSpecToDataObject in FEDITSDOptions then
        SetSpecificationPointers(DataSegment, SpecSegment);
      // Move to the next data segment
      Inc(I);
    end
    else
    begin
      // Do error checking and data validation in decendent class
      ValidateData(Self, FEDILoopStack, DataSegment, SpecSegment, I, J, FErrorOccured);
      if FErrorOccured then
        Exit;
      //
      // Debug - Keep the following line here in case someone wants to debug what happens to the stack.
      // ShowMessage('Current Data Segment: [' + IntToStr(I) + '] ' + DataSegment.SegmentID + #13#10 +
      //             'Current Spec Segment: [' + IntToStr(J) + '] ' + SpecSegment.SegmentID + #13#10 +
      //             FEDILoopStack.Debug);
      //
      // Move to the next specification segment
      J := AdvanceSegSpecIndex(I, J, High(FEDITransactionSetSpec.Segments)); //Inc(J);
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDITransactionSetDocument.ValidateData(
  TSDocument: TEDITransactionSetDocument; LoopStack: TEDILoopStack;
  DataSegment, SpecSegment: TEDISegment; var DataIndex, SpecIndex: Integer;
  var ErrorOccured: Boolean);
begin
  ErrorOccured := False;
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSetDocument.AdvanceSegSpecIndex(DataIndex, SpecStartIndex,
  SpecEndIndex: Integer): Integer;
var
  DataSegment: TEDISegment;
  TestSegment: TEDISegmentSpec;
  I: Integer;
begin
  Result := SpecEndIndex + 1;
  DataSegment := FEDITransactionSet.Segment[DataIndex];
  for I := SpecStartIndex + 1 to SpecEndIndex do
  begin
    TestSegment := TEDISegmentSpec(FEDITransactionSetSpec.Segment[I]);
    // Find matching segment
    if ((DataSegment.SegmentID) = (TestSegment.SegmentID)) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDITransactionSetDocument.SetSpecificationPointers(DataSegment, SpecSegment: TEDISegment);
var
  I, J: Integer;
begin
  DataSegment.SpecPointer := SpecSegment;
  J := High(SpecSegment.Elements);
  for I := Low(DataSegment.Elements) to High(DataSegment.Elements) do
  begin
    if I > J then
      raise EJclEDIError.CreateResRecFmt(@RsEDIError058,
        [IntToStr(I), DataSegment.SegmentID,
         IntToStr(DataSegment.GetIndexPositionFromParent)]);
    DataSegment.Element[I].SpecPointer := SpecSegment.Element[I];
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSetDocument.ValidateLoopStack(SpecSegmentId, SpecOwnerLoopId,
  SpecParentLoopId: string; SpecStartIndex: Integer; Loop: TEDITransactionSetLoop;
  LoopRepeated: Boolean): TEDILoopStackRecord;
var
  I: Integer;
  SR: TEDILoopStackRecord;
begin
  if FEDILoopStack.GetStackSize <= 0 then
    // Add entry to stack
    FEDILoopStack.Add(SpecSegmentId, SpecOwnerLoopId, SpecParentLoopId, SpecStartIndex, Loop)
  else
  begin
    I := FEDILoopStack.SetStackPointer(SpecOwnerLoopId, SpecParentLoopId);
    if I >= 0 then // Entry found
    begin
      if LoopRepeated then
      begin
        // Get the previous stack record so the repeated loop will not be nested
        SR := FEDILoopStack.GetStackRecord(I-1);
        // Add loop since it repeated
        I := SR.Loop.AddLoop(SpecOwnerLoopId, SpecParentLoopId);
        // Update stack loop with new loop reference
        FEDILoopStack.UpdateLoopReference(TEDITransactionSetLoop(SR.Loop[I]));
        // Debug
        // ShowMessage('LoopRepeated');
      end
      else
      if FEDILoopStack.AltStackPointer then
      begin
        // Get the previous stack record because the loop is
        // not to be nested at the current stack pointer
        SR := FEDILoopStack.GetStackRecord(I-1);
        // Add loop since it is new
        I := SR.Loop.AddLoop(SpecOwnerLoopId, SpecParentLoopId);
        // Update stack entry
        FEDILoopStack.Update(SpecSegmentId, SpecOwnerLoopId, SpecParentLoopId, SpecStartIndex,
          TEDITransactionSetLoop(SR.Loop[I]));
        // Debug
        // ShowMessage('AltStackPointer');
      end
      else
      if FEDILoopStack.StackResized then
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
      I := Loop.AddLoop(SpecOwnerLoopId, SpecParentLoopId);
      // Add entry to stack
      FEDILoopStack.Add(SpecSegmentId, SpecOwnerLoopId, SpecParentLoopId, SpecStartIndex,
        TEDITransactionSetLoop(Loop[I]));
      // Debug
      // ShowMessage('Stack Size Increased');
    end;
  end;
  Result := FEDILoopStack.GetStackRecord;
end;

//--------------------------------------------------------------------------------------------------

function TEDITransactionSetDocument.ValidateSegSpecIndex(DataSegmentId: string;
  SpecStartIndex: Integer; var LoopRepeated: Boolean): Integer;
var
  I: Integer;
begin
  Result := SpecStartIndex;
  // Find the segment in the stack to determine if a loop has repeated
  for I := High(FEDILoopStack.Stack) downto Low(FEDILoopStack.Stack) do
  begin
    if (DataSegmentId = FEDILoopStack.Stack[I].SegmentId) and
      (FEDILoopStack.Stack[I].OwnerLoopId <> NA_LoopId) then
    begin
      LoopRepeated := True;
      Result := FEDILoopStack.Stack[I].SpecStartIndex;
      Break;
    end;
  end;
end;

end.
