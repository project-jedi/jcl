{**************************************************************************************************}
{                                                                                                  }
{ Ray's Jedi Projects                                                                              }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is EDICOM_ANSIX12.pas.                                                         }
{                                                                                                  }
{ The Initial Developer of the Original Code is Raymond Alexander.                                 }
{ Portions created by Raymond Alexander are Copyright Raymond Alexander. All rights reserved.      }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{  This is an experimental unit for COM interop with other languages.                              }
{                                                                                                  }
{ Unit owner: Raymond Alexander                                                                    }
{ Date created: May 29, 2004                                                                       }
{ Last modified: May 30, 2004                                                                      }
{ Additional Info:                                                                                 }
{   E-Mail at RaysDelphiBox3@hotmail.com                                                           }
{   For latest EDI specific updates see http://sourceforge.net/projects/edisdk                     }
{   See home page for latest news & events and online help.                                        }
{                                                                                                  }
{**************************************************************************************************}
unit JclEDICOM_ANSIX12;

interface

uses
  Windows, ActiveX, Classes, ComObj, StdVcl, EDISDK_TLB,
  JclEDI, JclEDI_ANSIX12;

type

  IEDICOMInternalInterface = interface
    ['{72227476-D4D4-448C-9C28-08552373C737}']
    procedure SetInternalEDIObjectRef(EDIObject: TEDIObject);
  end;

  TEDICOMDelimiters = class(TAutoObject, IEDICOMInternalInterface, IEDICOMDelimiters)
  private
    FDelimiters: TEDIDelimiters;
  protected
    function Get_SD: WideString; safecall;
    procedure Set_SD(const Value: WideString); safecall;
    function Get_ED: WideString; safecall;
    procedure Set_ED(const Value: WideString); safecall;
    function Get_SS: WideString; safecall;
    procedure Set_SS(const Value: WideString); safecall;
    function Get_SDLen: Integer; safecall;
    function Get_EDLen: Integer; safecall;
    function Get_SSLen: Integer; safecall;
  public
    procedure Initialize; override;
    destructor Destroy; override;
    procedure SetInternalEDIObjectRef(EDIObject: TEDIObject);
  end;

  TEDICOMElement = class(TAutoObject, IEDICOMInternalInterface, IEDICOMElement)
  private
    FDelimitersIntf: TEDICOMDelimiters;
    FElement: TEDIElement;
  protected
    function Assemble: WideString; safecall;
    procedure Disassemble; safecall;
    function Get_State: Integer; safecall;
    function Get_Data: WideString; safecall;
    procedure Set_Data(const Value: WideString); safecall;
    function Get_DataLength: Integer; safecall;
    function Get_Delimiters: IEDICOMDelimiters; safecall;
    property Delimiters: IEDICOMDelimiters read Get_Delimiters;
    property State: Integer read Get_State;
    property Data: WideString read Get_Data write Set_Data;
    property DataLength: Integer read Get_DataLength;
  public
    procedure Initialize; override;
    destructor Destroy; override;
    procedure SetInternalEDIObjectRef(EDIObject: TEDIObject);
  end;

  TEDICOMSegment = class(TAutoObject, IEDICOMInternalInterface, IEDICOMSegment)
  private
    FDelimitersIntf: TEDICOMDelimiters;
    FElementIntf: TEDICOMElement;
    FSegment: TEDISegment;
  protected
    function Assemble: WideString; safecall;
    procedure Disassemble; safecall;
    function Get_State: Integer; safecall;
    function Get_Data: WideString; safecall;
    procedure Set_Data(const Value: WideString); safecall;
    function Get_DataLength: Integer; safecall;
    property State: Integer read Get_State;
    property Data: WideString read Get_Data write Set_Data;
    property DataLength: Integer read Get_DataLength;
    //
    function AddElement: Integer; safecall;
    function InsertElement(InsertIndex: Integer): Integer; safecall;
    procedure DeleteElement(Index: Integer); safecall;
    function AddElements(Count: Integer): Integer; safecall;
    function InsertElements(InsertIndex: Integer; Count: Integer): Integer; safecall;
    procedure DeleteElements; safecall;

    function Get_Element(Index: Integer): IEDICOMElement; safecall;
    property Element[Index: Integer]: IEDICOMElement read Get_Element;
    function Get_SegmentId: WideString; safecall;
    procedure Set_SegmentId(const Value: WideString); safecall;

    function Get_ElementCount: Integer; safecall;
    property ElementCount: Integer read Get_ElementCount;

    function Get_Delimiters: IEDICOMDelimiters; safecall;
    property Delimiters: IEDICOMDelimiters read Get_Delimiters;
    property SegmentId: WideString read Get_SegmentId write Set_SegmentId;
  public
    procedure Initialize; override;
    destructor Destroy; override;
    procedure SetInternalEDIObjectRef(EDIObject: TEDIObject);
  end;

  TEDICOMTransactionSet = class(TAutoObject, IEDICOMInternalInterface, IEDICOMTransactionSet)
  private
    FDelimitersIntf: TEDICOMDelimiters;
    FSegmentIntf: TEDICOMSegment;
    FTransactionSet: TEDITransactionSet;
  protected
    function Assemble: WideString; safecall;
    procedure Disassemble; safecall;
    function Get_State: Integer; safecall;
    function Get_Data: WideString; safecall;
    procedure Set_Data(const Value: WideString); safecall;
    function Get_DataLength: Integer; safecall;
    property State: Integer read Get_State;
    property Data: WideString read Get_Data write Set_Data;
    property DataLength: Integer read Get_DataLength;
    //
    function AddSegment: Integer; safecall;
    function InsertSegment(InsertIndex: Integer): Integer; safecall;
    procedure DeleteSegment(Index: Integer); safecall;
    function AddSegments(Count: Integer): Integer; safecall;
    function InsertSegments(InsertIndex: Integer; Count: Integer): Integer; safecall;
    procedure DeleteSegments; safecall;

    function Get_SegmentST: IEDICOMSegment; safecall;
    function Get_SegmentSE: IEDICOMSegment; safecall;
    function Get_Segment(Index: Integer): IEDICOMSegment; safecall;

    function Get_SegmentCount: Integer; safecall;
    property SegmentCount: Integer read Get_SegmentCount;

    function Get_Delimiters: IEDICOMDelimiters; safecall;
    property Delimiters: IEDICOMDelimiters read Get_Delimiters;
    property SegmentST: IEDICOMSegment read Get_SegmentST;
    property SegmentSE: IEDICOMSegment read Get_SegmentSE;
    property Segment[Index: Integer]: IEDICOMSegment read Get_Segment;
  public
    procedure Initialize; override;
    destructor Destroy; override;
    procedure SetInternalEDIObjectRef(EDIObject: TEDIObject);
  end;

  TEDICOMFunctionalGroup = class(TAutoObject, IEDICOMInternalInterface, IEDICOMFunctionalGroup)
  private
    FDelimitersIntf: TEDICOMDelimiters;
    FSegmentIntf: TEDICOMSegment;
    FTransactionSetIntf: TEDICOMTransactionSet;
    FFunctionalGroup: TEDIFunctionalGroup;
  protected
    function Assemble: WideString; safecall;
    procedure Disassemble; safecall;
    function Get_State: Integer; safecall;
    function Get_Data: WideString; safecall;
    procedure Set_Data(const Value: WideString); safecall;
    function Get_DataLength: Integer; safecall;
    property State: Integer read Get_State;
    property Data: WideString read Get_Data write Set_Data;
    property DataLength: Integer read Get_DataLength;
    //
    function AddTransactionSet: Integer; safecall;
    function InsertTransactionSet(InsertIndex: Integer): Integer; safecall;
    procedure DeleteTransactionSet(Index: Integer); safecall;
    function AddTransactionSets(Count: Integer): Integer; safecall;
    function InsertTransactionSets(InsertIndex: Integer; Count: Integer): Integer; safecall;
    procedure DeleteTransactionSets; safecall;

    function Get_SegmentGS: IEDICOMSegment; safecall;
    function Get_SegmentGE: IEDICOMSegment; safecall;
    function Get_TransactionSet(Index: Integer): IEDICOMTransactionSet; safecall;

    function Get_TransactionSetCount: Integer; safecall;
    property TransactionSetCount: Integer read Get_TransactionSetCount;

    function Get_Delimiters: IEDICOMDelimiters; safecall;
    property Delimiters: IEDICOMDelimiters read Get_Delimiters;
    property SegmentGS: IEDICOMSegment read Get_SegmentGS;
    property SegmentGE: IEDICOMSegment read Get_SegmentGE;
    property TransactionSet[Index: Integer]: IEDICOMTransactionSet read Get_TransactionSet;
  public
    procedure Initialize; override;
    destructor Destroy; override;
    procedure SetInternalEDIObjectRef(EDIObject: TEDIObject);
  end;

  TEDICOMInterchangeControl = class(TAutoObject, IEDICOMInternalInterface, IEDICOMInterchangeControl)
  private
    FDelimitersIntf: TEDICOMDelimiters;
    FSegmentIntf: TEDICOMSegment;
    FFunctionalGroupIntf: TEDICOMFunctionalGroup;
    FInterchangeControl: TEDIInterchangeControl;
  protected
    function Assemble: WideString; safecall;
    procedure Disassemble; safecall;
    function Get_State: Integer; safecall;
    function Get_Data: WideString; safecall;
    procedure Set_Data(const Value: WideString); safecall;
    function Get_DataLength: Integer; safecall;
    property State: Integer read Get_State;
    property Data: WideString read Get_Data write Set_Data;
    property DataLength: Integer read Get_DataLength;
    //
    procedure SetDelimiters(const SD: WideString; const ED: WideString; const SS: WideString); safecall;

    function AddFunctionalGroup: Integer; safecall;
    function InsertFunctionalGroup(InsertIndex: Integer): Integer; safecall;
    procedure DeleteFunctionalGroup(Index: Integer); safecall;
    function AddFunctionalGroups(InsertIndex: Integer): Integer; safecall;
    function InsertFunctionalGroups(InsertIndex: Integer; Count: Integer): Integer; safecall;
    procedure DeleteFunctionalGroups; safecall;

    function Get_SegmentISA: IEDICOMSegment; safecall;
    function Get_SegmentIEA: IEDICOMSegment; safecall;
    function Get_FunctionalGroup(Index: Integer): IEDICOMFunctionalGroup; safecall;

    function Get_FunctionalGroupCount: Integer; safecall;
    property FunctionalGroupCount: Integer read Get_FunctionalGroupCount;

    function Get_Delimiters: IEDICOMDelimiters; safecall;
    property Delimiters: IEDICOMDelimiters read Get_Delimiters;
    property SegmentISA: IEDICOMSegment read Get_SegmentISA;
    property SegmentIEA: IEDICOMSegment read Get_SegmentIEA;
    property FunctionalGroup[Index: Integer]: IEDICOMFunctionalGroup read Get_FunctionalGroup;
  public
    procedure Initialize; override;
    destructor Destroy; override;
    procedure SetInternalEDIObjectRef(EDIObject: TEDIObject);
  end;

  TEDICOMFile = class(TAutoObject, IEDICOMFile)
  private
    FDelimitersIntf: TEDICOMDelimiters;
    FInterchangeControlIntf: TEDICOMInterchangeControl;
    FEDIFile: TEDIFile;
  protected
    function Assemble: WideString; safecall;
    procedure Disassemble; safecall;
    function Get_State: Integer; safecall;
    function Get_Data: WideString; safecall;
    procedure Set_Data(const Value: WideString); safecall;
    function Get_DataLength: Integer; safecall;
    property State: Integer read Get_State;
    property Data: WideString read Get_Data write Set_Data;
    property DataLength: Integer read Get_DataLength;
    //
    procedure LoadFromFile(const FileName: WideString); safecall;
    procedure ReLoadFromFile; safecall;
    procedure SaveToFile; safecall;
    procedure SaveAsToFile(const FileName: WideString); safecall;
    function Get_FileName: WideString; safecall;
    procedure Set_FileName(const Value: WideString); safecall;
    function Get_Interchange(Index: Integer): IEDICOMInterchangeControl; safecall;
    function Get_Options: Byte; safecall;
    procedure Set_Options(Value: Byte); safecall;

    function AddInterchange: Integer; safecall;
    function InsertInterchange(InsertIndex: Integer): Integer; safecall;
    procedure DeleteInterchange(Index: Integer); safecall;
    function AddInterchanges(Count: Integer): Integer; safecall;
    function InsertInterchanges(InsertIndex: Integer; Count: Integer): Integer; safecall;
    procedure DeleteInterchanges; safecall;

    function Get_InterchangeCount: Integer; safecall;
    property InterchangeCount: Integer read Get_InterchangeCount;

    function Get_Delimiters: IEDICOMDelimiters; safecall;
    property Delimiters: IEDICOMDelimiters read Get_Delimiters;
    property FileName: WideString read Get_FileName write Set_FileName;
    property Interchange[Index: Integer]: IEDICOMInterchangeControl read Get_Interchange;
    property Options: Byte read Get_Options write Set_Options;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;

implementation

uses ComServ, SysUtils;

{ TEDICOMElement }

function TEDICOMElement.Assemble: WideString;
begin
  Result := FElement.Assemble;
end;

destructor TEDICOMElement.Destroy;
begin
  FDelimitersIntf.ObjRelease;
  FDelimitersIntf := nil;
  FElement := nil;
  inherited;
end;

procedure TEDICOMElement.Disassemble;
begin
  FElement.Disassemble;
end;

function TEDICOMElement.Get_Data: WideString;
begin
  Result := FElement.Data;
end;

function TEDICOMElement.Get_DataLength: Integer;
begin
  Result := FElement.DataLength;
end;

function TEDICOMElement.Get_Delimiters: IEDICOMDelimiters;
begin
  FDelimitersIntf.SetInternalEDIObjectRef(FElement.Delimiters);
  Result := FDelimitersIntf;
end;

function TEDICOMElement.Get_State: Integer;
begin
  Result := Integer(FElement.State);
end;

procedure TEDICOMElement.Initialize;
begin
  inherited;
  FDelimitersIntf := TEDICOMDelimiters.Create;
  FDelimitersIntf.ObjAddRef;
end;

procedure TEDICOMElement.SetInternalEDIObjectRef(EDIObject: TEDIObject);
begin
  FElement := TEDIElement(EDIObject);
end;

procedure TEDICOMElement.Set_Data(const Value: WideString);
begin
  FElement.Data := Value;
end;

{ TEDICOMSegment }

function TEDICOMSegment.AddElement: Integer;
begin
  Result := FSegment.AddElement;
end;

function TEDICOMSegment.AddElements(Count: Integer): Integer;
begin
  Result := FSegment.AddElements(Count);
end;

function TEDICOMSegment.Assemble: WideString;
begin
  Result := FSegment.Assemble;
end;

procedure TEDICOMSegment.DeleteElement(Index: Integer);
begin
  FSegment.DeleteElement(Index);
end;

procedure TEDICOMSegment.DeleteElements;
begin
  FSegment.DeleteElements;
end;

destructor TEDICOMSegment.Destroy;
begin
  FElementIntf.ObjRelease;
  FElementIntf := nil;
  FDelimitersIntf.ObjRelease;
  FDelimitersIntf := nil;
  FSegment := nil;
  inherited;
end;

procedure TEDICOMSegment.Disassemble;
begin
  FSegment.Disassemble;
end;

function TEDICOMSegment.Get_Data: WideString;
begin
  Result := FSegment.Data;
end;

function TEDICOMSegment.Get_DataLength: Integer;
begin
  Result := FSegment.DataLength;
end;

function TEDICOMSegment.Get_Delimiters: IEDICOMDelimiters;
begin
  FDelimitersIntf.SetInternalEDIObjectRef(FSegment.Delimiters);
  Result := FDelimitersIntf;
end;

function TEDICOMSegment.Get_Element(Index: Integer): IEDICOMElement;
begin
  FElementIntf.SetInternalEDIObjectRef(FSegment[Index]);
  Result := FElementIntf;
end;

function TEDICOMSegment.Get_ElementCount: Integer;
begin
  Result := FSegment.ElementCount;
end;

function TEDICOMSegment.Get_SegmentId: WideString;
begin
  Result := FSegment.SegmentId;
end;

function TEDICOMSegment.Get_State: Integer;
begin
  Result := Integer(FSegment.State);
end;

procedure TEDICOMSegment.Initialize;
begin
  inherited;
  FDelimitersIntf := TEDICOMDelimiters.Create;
  FDelimitersIntf.ObjAddRef;  
  FElementIntf := TEDICOMElement.Create;
  FElementIntf.ObjAddRef;
  FSegment := nil;
end;

function TEDICOMSegment.InsertElement(InsertIndex: Integer): Integer;
begin
  Result := FSegment.InsertElement(InsertIndex);
end;

function TEDICOMSegment.InsertElements(InsertIndex, Count: Integer): Integer;
begin
  Result := FSegment.InsertElements(InsertIndex, Count);
end;

procedure TEDICOMSegment.SetInternalEDIObjectRef(EDIObject: TEDIObject);
begin
  FSegment := TEDISegment(EDIObject);
end;

procedure TEDICOMSegment.Set_Data(const Value: WideString);
begin
  FSegment.Data := Value;
end;

procedure TEDICOMSegment.Set_SegmentId(const Value: WideString);
begin
  FSegment.SegmentId := Value;
end;

{ TEDICOMTransactionSet }

function TEDICOMTransactionSet.AddSegment: Integer;
begin
  Result := FTransactionSet.AddSegment;
end;

function TEDICOMTransactionSet.AddSegments(Count: Integer): Integer;
begin
  Result := FTransactionSet.AddSegments(Count);
end;

function TEDICOMTransactionSet.Assemble: WideString;
begin
  Result := FTransactionSet.Assemble;
end;

procedure TEDICOMTransactionSet.DeleteSegment(Index: Integer);
begin
  FTransactionSet.DeleteSegment(Index);
end;

procedure TEDICOMTransactionSet.DeleteSegments;
begin
  FTransactionSet.DeleteSegments;
end;

destructor TEDICOMTransactionSet.Destroy;
begin
  FSegmentIntf.ObjRelease;
  FSegmentIntf := nil;
  FDelimitersIntf.ObjRelease;
  FDelimitersIntf := nil;
  FTransactionSet := nil;
  inherited;
end;

procedure TEDICOMTransactionSet.Disassemble;
begin
  FTransactionSet.Disassemble;
end;

function TEDICOMTransactionSet.Get_Data: WideString;
begin
  Result := FTransactionSet.Data;
end;

function TEDICOMTransactionSet.Get_DataLength: Integer;
begin
  Result := FTransactionSet.DataLength;
end;

function TEDICOMTransactionSet.Get_Delimiters: IEDICOMDelimiters;
begin
  FDelimitersIntf.SetInternalEDIObjectRef(FTransactionSet.Delimiters);
  Result := FDelimitersIntf;
end;

function TEDICOMTransactionSet.Get_Segment(Index: Integer): IEDICOMSegment;
begin
  FSegmentIntf.SetInternalEDIObjectRef(FTransactionSet[Index]);
  Result := FSegmentIntf;
end;

function TEDICOMTransactionSet.Get_SegmentCount: Integer;
begin
  Result := FTransactionSet.SegmentCount;
end;

function TEDICOMTransactionSet.Get_SegmentSE: IEDICOMSegment;
begin
  FSegmentIntf.SetInternalEDIObjectRef(FTransactionSet.SegmentSE);
  Result := FSegmentIntf;
end;

function TEDICOMTransactionSet.Get_SegmentST: IEDICOMSegment;
begin
  FSegmentIntf.SetInternalEDIObjectRef(FTransactionSet.SegmentST);
  Result := FSegmentIntf;
end;

function TEDICOMTransactionSet.Get_State: Integer;
begin
  Result := Integer(FTransactionSet.State);
end;

procedure TEDICOMTransactionSet.Initialize;
begin
  inherited;
  FDelimitersIntf := TEDICOMDelimiters.Create;
  FDelimitersIntf.ObjAddRef;  
  FSegmentIntf := TEDICOMSegment.Create;
  FSegmentIntf.ObjAddRef;  
  FTransactionSet := nil;
end;

function TEDICOMTransactionSet.InsertSegment(InsertIndex: Integer): Integer;
begin
  Result := FTransactionSet.InsertSegment(InsertIndex);
end;

function TEDICOMTransactionSet.InsertSegments(InsertIndex, Count: Integer): Integer;
begin
  Result := FTransactionSet.InsertSegments(InsertIndex, Count);
end;

procedure TEDICOMTransactionSet.SetInternalEDIObjectRef(EDIObject: TEDIObject);
begin
  FTransactionSet := TEDITransactionSet(EDIObject);
end;

procedure TEDICOMTransactionSet.Set_Data(const Value: WideString);
begin
  FTransactionSet.Data := Value;
end;

{ TEDICOMFunctionalGroup }

function TEDICOMFunctionalGroup.AddTransactionSet: Integer;
begin
  Result := FFunctionalGroup.AddTransactionSet;
end;

function TEDICOMFunctionalGroup.AddTransactionSets(Count: Integer): Integer;
begin
  Result := FFunctionalGroup.AddTransactionSets(Count);
end;

function TEDICOMFunctionalGroup.Assemble: WideString;
begin
  Result := FFunctionalGroup.Assemble;
end;

procedure TEDICOMFunctionalGroup.DeleteTransactionSet(Index: Integer);
begin
  FFunctionalGroup.DeleteTransactionSet(Index);
end;

procedure TEDICOMFunctionalGroup.DeleteTransactionSets;
begin
  FFunctionalGroup.DeleteTransactionSets;
end;

destructor TEDICOMFunctionalGroup.Destroy;
begin
  FTransactionSetIntf.ObjRelease;
  FTransactionSetIntf := nil;
  FSegmentIntf.ObjRelease;
  FSegmentIntf := nil;
  FDelimitersIntf.ObjRelease;
  FDelimitersIntf := nil;
  FFunctionalGroup := nil;
  inherited;
end;

procedure TEDICOMFunctionalGroup.Disassemble;
begin
  FFunctionalGroup.Disassemble;
end;

function TEDICOMFunctionalGroup.Get_Data: WideString;
begin
  Result := FFunctionalGroup.Data;
end;

function TEDICOMFunctionalGroup.Get_DataLength: Integer;
begin
  Result := FFunctionalGroup.DataLength;
end;

function TEDICOMFunctionalGroup.Get_Delimiters: IEDICOMDelimiters;
begin
  FDelimitersIntf.SetInternalEDIObjectRef(FFunctionalGroup.Delimiters);
  Result := FDelimitersIntf;
end;

function TEDICOMFunctionalGroup.Get_SegmentGE: IEDICOMSegment;
begin
  FSegmentIntf.SetInternalEDIObjectRef(FFunctionalGroup.SegmentGE);
  Result := FSegmentIntf;
end;

function TEDICOMFunctionalGroup.Get_SegmentGS: IEDICOMSegment;
begin
  FSegmentIntf.SetInternalEDIObjectRef(FFunctionalGroup.SegmentGS);
  Result := FSegmentIntf;
end;

function TEDICOMFunctionalGroup.Get_State: Integer;
begin
  Result := Integer(FFunctionalGroup.State);
end;

function TEDICOMFunctionalGroup.Get_TransactionSet(Index: Integer): IEDICOMTransactionSet;
begin
  FTransactionSetIntf.SetInternalEDIObjectRef(FFunctionalGroup[Index]);
  Result := FTransactionSetIntf;
end;

function TEDICOMFunctionalGroup.Get_TransactionSetCount: Integer;
begin
  Result := FFunctionalGroup.TransactionSetCount;
end;

procedure TEDICOMFunctionalGroup.Initialize;
begin
  inherited;
  FDelimitersIntf := TEDICOMDelimiters.Create;
  FDelimitersIntf.ObjAddRef;  
  FSegmentIntf := TEDICOMSegment.Create;
  FSegmentIntf.ObjAddRef;
  FTransactionSetIntf := TEDICOMTransactionSet.Create;
  FTransactionSetIntf.ObjAddRef;
  FFunctionalGroup := nil;
end;

function TEDICOMFunctionalGroup.InsertTransactionSet(InsertIndex: Integer): Integer;
begin
  Result := FFunctionalGroup.InsertTransactionSet(InsertIndex);
end;

function TEDICOMFunctionalGroup.InsertTransactionSets(InsertIndex, Count: Integer): Integer;
begin
  Result := FFunctionalGroup.InsertTransactionSets(InsertIndex, Count);
end;

procedure TEDICOMFunctionalGroup.SetInternalEDIObjectRef(EDIObject: TEDIObject);
begin
  FFunctionalGroup := TEDIFunctionalGroup(EDIObject);
end;

procedure TEDICOMFunctionalGroup.Set_Data(const Value: WideString);
begin
  FFunctionalGroup.Data := Value;
end;

{ TEDICOMInterchangeControl }

function TEDICOMInterchangeControl.Assemble: WideString;
begin
  Result := FInterchangeControl.Assemble;
end;

destructor TEDICOMInterchangeControl.Destroy;
begin
  FFunctionalGroupIntf.ObjRelease;
  FFunctionalGroupIntf := nil;
  FSegmentIntf.ObjRelease;
  FSegmentIntf := nil;
  FDelimitersIntf.ObjRelease;
  FDelimitersIntf := nil;
  FInterchangeControl := nil;
  inherited;
end;

procedure TEDICOMInterchangeControl.Disassemble;
begin
  FInterchangeControl.Disassemble;
end;

function TEDICOMInterchangeControl.Get_Data: WideString;
begin
  Result := FInterchangeControl.Data;
end;

function TEDICOMInterchangeControl.Get_DataLength: Integer;
begin
  Result := FInterchangeControl.DataLength;
end;

function TEDICOMInterchangeControl.Get_FunctionalGroup(Index: Integer): IEDICOMFunctionalGroup;
begin
  FFunctionalGroupIntf.SetInternalEDIObjectRef(FInterchangeControl[Index]);
  Result := FFunctionalGroupIntf;
end;

function TEDICOMInterchangeControl.Get_SegmentIEA: IEDICOMSegment;
begin
  FSegmentIntf.SetInternalEDIObjectRef(FInterchangeControl.SegmentIEA);
  Result := FSegmentIntf;
end;

function TEDICOMInterchangeControl.Get_SegmentISA: IEDICOMSegment;
begin
  FSegmentIntf.SetInternalEDIObjectRef(FInterchangeControl.SegmentISA);
  Result := FSegmentIntf;
end;

function TEDICOMInterchangeControl.Get_State: Integer;
begin
  Result := Integer(FInterchangeControl.State);
end;

procedure TEDICOMInterchangeControl.Initialize;
begin
  inherited;
  FDelimitersIntf := TEDICOMDelimiters.Create;
  FDelimitersIntf.ObjAddRef;  
  FSegmentIntf := TEDICOMSegment.Create;
  FSegmentIntf.ObjAddRef;
  FFunctionalGroupIntf := TEDICOMFunctionalGroup.Create;
  FFunctionalGroupIntf.ObjAddRef;
  FInterchangeControl := nil;
end;

procedure TEDICOMInterchangeControl.Set_Data(const Value: WideString);
begin
  FInterchangeControl.Data := Value;
end;

procedure TEDICOMInterchangeControl.SetInternalEDIObjectRef(EDIObject: TEDIObject);
begin
  FInterchangeControl := TEDIInterchangeControl(EDIObject);
end;

function TEDICOMInterchangeControl.AddFunctionalGroup: Integer;
begin
  Result := FInterchangeControl.AddFunctionalGroup;
end;

function TEDICOMInterchangeControl.AddFunctionalGroups(InsertIndex: Integer): Integer;
begin
  Result := FInterchangeControl.InsertFunctionalGroup(InsertIndex);
end;

procedure TEDICOMInterchangeControl.DeleteFunctionalGroup(Index: Integer);
begin
  FInterchangeControl.DeleteFunctionalGroup(Index);
end;

procedure TEDICOMInterchangeControl.DeleteFunctionalGroups;
begin
  FInterchangeControl.DeleteFunctionalGroups;
end;

function TEDICOMInterchangeControl.InsertFunctionalGroup(InsertIndex: Integer): Integer;
begin
  Result := FInterchangeControl.InsertFunctionalGroup(InsertIndex);
end;

function TEDICOMInterchangeControl.InsertFunctionalGroups(InsertIndex, Count: Integer): Integer;
begin
  Result := FInterchangeControl.InsertFunctionalGroups(InsertIndex, Count);
end;

function TEDICOMInterchangeControl.Get_Delimiters: IEDICOMDelimiters;
begin
  FDelimitersIntf.SetInternalEDIObjectRef(FInterchangeControl.Delimiters);
  Result := FDelimitersIntf;
end;

procedure TEDICOMInterchangeControl.SetDelimiters(const SD, ED, SS: WideString);
begin
  FInterchangeControl.Delimiters := TEDIDelimiters.Create(SD, ED, SS);
end;

function TEDICOMInterchangeControl.Get_FunctionalGroupCount: Integer;
begin
  Result := FInterchangeControl.FunctionalGroupCount;
end;

{ TEDICOMFile }

function TEDICOMFile.AddInterchange: Integer;
begin
  Result := FEDIFile.AddInterchange;
end;

function TEDICOMFile.AddInterchanges(Count: Integer): Integer;
begin
  Result := FEDIFile.AddInterchanges(Count);
end;

function TEDICOMFile.Assemble: WideString;
begin
  Result := FEDIFile.Assemble;
end;

procedure TEDICOMFile.DeleteInterchange(Index: Integer);
begin
  FEDIFile.DeleteInterchange(Index);
end;

procedure TEDICOMFile.DeleteInterchanges;
begin
  FEDIFile.DeleteInterchanges;
end;

destructor TEDICOMFile.Destroy;
begin
  FInterchangeControlIntf.ObjRelease;
  FInterchangeControlIntf := nil;
  FDelimitersIntf.ObjRelease;
  FDelimitersIntf := nil;
  FEDIFile.Free;
  FEDIFile := nil;
  inherited;
end;

procedure TEDICOMFile.Disassemble;
begin
  FEDIFile.Disassemble;
end;

function TEDICOMFile.Get_Data: WideString;
begin
  Result := FEDIFile.Data;
end;

function TEDICOMFile.Get_DataLength: Integer;
begin
  Result := FEDIFile.DataLength;
end;

function TEDICOMFile.Get_Delimiters: IEDICOMDelimiters;
begin
  FDelimitersIntf.SetInternalEDIObjectRef(FEDIFile.Delimiters);
  Result := FDelimitersIntf;
end;

function TEDICOMFile.Get_FileName: WideString;
begin
  Result := FEDIFile.FileName;
end;

function TEDICOMFile.Get_Interchange(Index: Integer): IEDICOMInterchangeControl;
begin
  FInterchangeControlIntf.SetInternalEDIObjectRef(FEDIFile[Index]);
  Result := FInterchangeControlIntf;
end;

function TEDICOMFile.Get_InterchangeCount: Integer;
begin
  Result := FEDIFile.InterchangeControlCount;
end;

function TEDICOMFile.Get_Options: Byte;
begin
  Result := Byte(FEDIFIle.Options);
end;

function TEDICOMFile.Get_State: Integer;
begin
  Result := Integer(FEDIFile.State);
end;

procedure TEDICOMFile.Initialize;
begin
  inherited;
  FDelimitersIntf := TEDICOMDelimiters.Create;
  FDelimitersIntf.ObjAddRef;  
  FInterchangeControlIntf := TEDICOMInterchangeControl.Create;
  FInterchangeControlIntf.ObjAddRef;
  FEDIFile := TEDIFile.Create(nil);
end;

function TEDICOMFile.InsertInterchange(InsertIndex: Integer): Integer;
begin
  Result := FEDIFile.InsertInterchange(InsertIndex);
end;

function TEDICOMFile.InsertInterchanges(InsertIndex, Count: Integer): Integer;
begin
  Result := FEDIFile.InsertInterchanges(InsertIndex, Count);
end;

procedure TEDICOMFile.LoadFromFile(const FileName: WideString);
begin
  FEDIFile.LoadFromFile(FileName);
end;

procedure TEDICOMFile.ReLoadFromFile;
begin
  FEDIFile.ReLoadFromFile;
end;

procedure TEDICOMFile.SaveAsToFile(const FileName: WideString);
begin
  FEDIFile.SaveAsToFile(FileName);
end;

procedure TEDICOMFile.SaveToFile;
begin
  FEDIFile.SaveToFile;
end;

procedure TEDICOMFile.Set_Data(const Value: WideString);
begin
  FEDIFile.Data := Value;
end;

procedure TEDICOMFile.Set_FileName(const Value: WideString);
begin
  FEDIFile.FileName := Value;
end;

procedure TEDICOMFile.Set_Options(Value: Byte);
begin
  FEDIFile.Options := TEDIFileOptions(Value);
end;

{ TEDICOMDelimiters }

destructor TEDICOMDelimiters.Destroy;
begin
  FDelimiters := nil;
  inherited;
end;

function TEDICOMDelimiters.Get_ED: WideString;
begin
  Result := FDelimiters.ED;
end;

function TEDICOMDelimiters.Get_EDLen: Integer;
begin
  Result := FDelimiters.EDLen;
end;

function TEDICOMDelimiters.Get_SD: WideString;
begin
  Result := FDelimiters.SD;
end;

function TEDICOMDelimiters.Get_SDLen: Integer;
begin
  Result := FDelimiters.SDLen;
end;

function TEDICOMDelimiters.Get_SS: WideString;
begin
  Result := FDelimiters.SS;
end;

function TEDICOMDelimiters.Get_SSLen: Integer;
begin
  Result := FDelimiters.SSLen;
end;

procedure TEDICOMDelimiters.Initialize;
begin
  inherited;
  FDelimiters := nil;
end;

procedure TEDICOMDelimiters.Set_ED(const Value: WideString);
begin
  FDelimiters.ED := Value;
end;

procedure TEDICOMDelimiters.Set_SD(const Value: WideString);
begin
  FDelimiters.SD := Value;
end;

procedure TEDICOMDelimiters.Set_SS(const Value: WideString);
begin
  FDelimiters.SS := Value;
end;

procedure TEDICOMDelimiters.SetInternalEDIObjectRef(EDIObject: TEDIObject);
begin
  FDelimiters := TEDIDelimiters(EDIObject);
end;

initialization
  TAutoObjectFactory.Create(ComServer, TEDICOMDelimiters, CLASS_EDICOMDelimiters,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TEDICOMElement, Class_EDICOMElement,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TEDICOMSegment, Class_EDICOMSegment,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TEDICOMTransactionSet, Class_EDICOMTransactionSet,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TEDICOMFunctionalGroup, Class_EDICOMFunctionalGroup,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TEDICOMInterchangeControl, Class_EDICOMInterchangeControl,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TEDICOMFile, Class_EDICOMFile,
    ciMultiInstance, tmApartment);

// History

// rrossmair 2004-07-17:
// - removed unit Dialogs usage

end.
