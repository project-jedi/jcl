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
{ The Original Code is JclEDITranslators.pas.                                                      }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ EDI Translators Unit for classes that translate EDI objects from one format to another.          }
{                                                                                                  }
{ This unit is still in development                                                                }
{                                                                                                  }
{ Unit owner: Raymond Alexander                                                                    }
{ Last created: October 2, 2003                                                                    }
{ Last modified: October 22, 2003                                                                  }
{ Additional Info:                                                                                 }
{   E-Mail at RaysDelphiBox3@hotmail.com                                                           }
{   For latest EDI specific updates see http://sourceforge.net/projects/edisdk                     }
{   See home page for latest news & events and online help.                                        }
{                                                                                                  }
{**************************************************************************************************}

unit JclEDITranslators;

interface

uses
  SysUtils, Classes,
  JclEDI, JclEDI_ANSIX12, JclEDISEF;

type
  TEDISpecToSEFTranslator = class(TEDIObject)
  public
    constructor Create;
    destructor Destroy; override;
    //
    function TranslateToSEFElement(ElementSpec: TEDIElementSpec;
      Parent: TEDISEFFile): TEDISEFElement; overload;
    function TranslateToSEFElement(ElementSpec: TEDIElementSpec;
      Parent: TEDISEFSegment): TEDISEFElement; overload;
    function TranslateToSEFSegment(SegmentSpec: TEDISegmentSpec;
      Parent: TEDISEFFile): TEDISEFSegment; overload;
    function TranslateToSEFSegment(SegmentSpec: TEDISegmentSpec;
      Parent: TEDISEFTable): TEDISEFSegment; overload;
    function TranslateToSEFSegment(SegmentSpec: TEDISegmentSpec;
      Parent: TEDISEFLoop): TEDISEFSegment; overload;
    function TranslateToSEFSet(TransactionSetSpec: TEDITransactionSetSpec;
      Parent: TEDISEFFile): TEDISEFSet;
    procedure TranslateLoopToSEFSet(StackRecord: TEDILoopStackRecord;
      SegmentId, OwnerLoopId, ParentLoopId: string; var EDIObject: TEDIObject);
    function TranslateToSEFFile(ICSpec: TEDIInterchangeControlSpec): TEDISEFFile;
  end;

  TEDISEFToSpecTranslator = class(TEDIObject)
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

//==================================================================================================
// TEDISpecToSEFTranslator
//==================================================================================================

constructor TEDISpecToSEFTranslator.Create;
begin
  inherited Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISpecToSEFTranslator.Destroy;
begin
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEDISpecToSEFTranslator.TranslateToSEFElement(ElementSpec: TEDIElementSpec;
  Parent: TEDISEFFile): TEDISEFElement;
begin
  Result := TEDISEFElement.Create(Parent);
  Result.Id := ElementSpec.Id;
  Result.ElementType := ElementSpec.ElementType;
  Result.MinimumLength := ElementSpec.MinimumLength;
  Result.MaximumLength := ElementSpec.MaximumLength;
end;

//--------------------------------------------------------------------------------------------------

function TEDISpecToSEFTranslator.TranslateToSEFElement(ElementSpec: TEDIElementSpec;
  Parent: TEDISEFSegment): TEDISEFElement;
var
  ListItem: TEDISEFDataObjectListItem;
begin
  Result := TEDISEFElement.Create(Parent);
  Result.Id := ElementSpec.Id;
  ListItem := Parent.SEFFile.ELMS.FindItemByName(ElementSpec.Id);
  if ListItem <> nil then
    Result.Assign(TEDISEFElement(ListItem.EDISEFDataObject))
  else
  begin
    Result.ElementType := ElementSpec.ElementType;
    Result.MinimumLength := ElementSpec.MinimumLength;
    Result.MaximumLength := ElementSpec.MaximumLength;
    Result.RequirementDesignator := ElementSpec.RequirementDesignator;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDISpecToSEFTranslator.TranslateToSEFSegment(SegmentSpec: TEDISegmentSpec;
  Parent: TEDISEFFile): TEDISEFSegment;
var
  E: Integer;
  ElementSpec: TEDIElementSpec;
begin
  Result := TEDISEFSegment.Create(Parent);
  Result.Id := SegmentSpec.Id;
  Result.RequirementDesignator := SegmentSpec.RequirementDesignator;
  Result.MaximumUse := SegmentSpec.MaximumUsage;
  for E := 0 to SegmentSpec.ElementCount - 1 do
  begin
    ElementSpec := TEDIElementSpec(SegmentSpec[E]);
    Result.Elements.AddByNameOrId(TranslateToSEFElement(ElementSpec, Result));
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDISpecToSEFTranslator.TranslateToSEFSegment(SegmentSpec: TEDISegmentSpec;
  Parent: TEDISEFTable): TEDISEFSegment;
var
  ListItem: TEDISEFDataObjectListItem;
begin
  Result := TEDISEFSegment.Create(Parent);
  Result.Id := SegmentSpec.Id;
  ListItem := Parent.SEFFile.SEGS.FindItemByName(SegmentSpec.Id);
  if ListItem <> nil then
    Result.Assign(TEDISEFSegment(ListItem.EDISEFDataObject))
  else
  begin
    Result.RequirementDesignator := SegmentSpec.RequirementDesignator;
    Result.MaximumUse := SegmentSpec.MaximumUsage;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TEDISpecToSEFTranslator.TranslateToSEFSegment(SegmentSpec: TEDISegmentSpec;
  Parent: TEDISEFLoop): TEDISEFSegment;
var
  ListItem: TEDISEFDataObjectListItem;
begin
  Result := TEDISEFSegment.Create(Parent);
  Result.Id := SegmentSpec.Id;
  ListItem := Parent.SEFFile.SEGS.FindItemByName(SegmentSpec.Id);
  if ListItem <> nil then
    Result.Assign(TEDISEFSegment(ListItem.EDISEFDataObject))
  else
  begin
    Result.RequirementDesignator := SegmentSpec.RequirementDesignator;
    Result.MaximumUse := SegmentSpec.MaximumUsage;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TEDISpecToSEFTranslator.TranslateLoopToSEFSet(StackRecord: TEDILoopStackRecord;
  SegmentId, OwnerLoopId, ParentLoopId: string; var EDIObject: TEDIObject);
var
  SEFLoop: TEDISEFLoop;
begin
  SEFLoop := nil;
  if StackRecord.EDIObject is TEDISEFDataObjectGroup then
  begin
    SEFLoop := TEDISEFLoop.Create(TEDISEFDataObject(StackRecord.EDIObject));
    SEFLoop.Id := SegmentId;
    TEDISEFDataObjectGroup(StackRecord.EDIObject).EDISEFDataObjects.AddByNameOrId(SEFLoop);
  end;
  EDIObject := SEFLoop;
end;

//--------------------------------------------------------------------------------------------------

function TEDISpecToSEFTranslator.TranslateToSEFSet(TransactionSetSpec: TEDITransactionSetSpec;
  Parent: TEDISEFFile): TEDISEFSet;
var
  S: Integer;
  SegmentSpec: TEDISegmentSpec;
  SEFTable: TEDISEFTable;
  SEFLoop: TEDISEFLoop;
  LS: TEDILoopStack;
  LSR: TEDILoopStackRecord;
begin
  Result := TEDISEFSet.Create(Parent);
  Result.Id := TransactionSetSpec.Id;
  SEFTable := TEDISEFTable.Create(Result);
  Result.EDISEFDataObjects.AddByNameOrId(SEFTable);

  LS := TEDILoopStack.Create;
  LS.OnAddLoop := TranslateLoopToSEFSet;
  //
  for S := 0 to TransactionSetSpec.SegmentCount - 1 do
  begin
    SegmentSpec := TEDISegmentSpec(TransactionSetSpec[S]);
    if S = 0 then
      // Initialize the stack
      LSR := LS.ValidateLoopStack(SegmentSpec.SegmentID, NA_LoopId, NA_LoopId, 0, SEFTable)
    else
      LSR := LS.ValidateLoopStack(SegmentSpec.SegmentID, SegmentSpec.OwnerLoopId,
        SegmentSpec.ParentLoopId, 0, LSR.EDIObject);

// Debug - Keep the following line here in case someone wants to debug what happens to the stack.
//    ShowMessage('Current Spec Segment: [' + IntToStr(S) + '] ' + SegmentSpec.SegmentID + #13#10 +
//                LS.Debug);

    if LSR.EDIObject is TEDISEFTable then
    begin
      SEFTable := TEDISEFTable(LSR.EDIObject);
      SEFTable.EDISEFDataObjects.AddByNameOrId(TranslateToSEFSegment(SegmentSpec, SEFTable));
    end
    else
    if LSR.EDIObject is TEDISEFLoop then
    begin
      SEFLoop := TEDISEFLoop(LSR.EDIObject);
      SEFLoop.EDISEFDataObjects.AddByNameOrId(TranslateToSEFSegment(SegmentSpec, SEFLoop))
    end;
  end;

  LS.Free;
end;

//--------------------------------------------------------------------------------------------------

function TEDISpecToSEFTranslator.TranslateToSEFFile(ICSpec: TEDIInterchangeControlSpec): TEDISEFFile;
var
  F, T, S, E: Integer;
  ElementList: TEDIObjectList;
  SegmentSpec: TEDISegmentSpec;
  ElementSpec: TEDIElementSpec;
  TransactionSetSpec: TEDITransactionSetSpec;
begin
  Result := TEDISEFFile.Create(nil);

  ElementList := TEDIObjectList.Create(False);
  try
    //Fill Element Dictionary
    for F := 0 to ICSpec.FunctionalGroupCount - 1 do
      for T := 0 to ICSpec[F].TransactionSetCount - 1 do
        for S := 0 to ICSpec[F][T].SegmentCount - 1 do
        begin
          SegmentSpec := TEDISegmentSpec(ICSpec[F][T][S]);
          for E := 0 to SegmentSpec.ElementCount - 1 do
          begin
            ElementSpec := TEDIElementSpec(SegmentSpec[E]);
            if Result.ELMS.FindItemByName(ElementSpec.Id) = nil then
              Result.ELMS.AddByNameOrId(TranslateToSEFElement(ElementSpec, Result))
            else
            begin
              //raise Exception.Create('Element Repeated - Incompatible File');
            end;
          end;
        end;
    //Fill Segment Dictionary
    for F := 0 to ICSpec.FunctionalGroupCount - 1 do
      for T := 0 to ICSpec[F].TransactionSetCount - 1 do
        for S := 0 to ICSpec[F][T].SegmentCount - 1 do
        begin
          SegmentSpec := TEDISegmentSpec(ICSpec[F][T][S]);
          if Result.SEGS.FindItemByName(SegmentSpec.Id) = nil then
            Result.SEGS.AddByNameOrId(TranslateToSEFSegment(SegmentSpec, Result))
          else
          begin
            //raise Exception.Create('Segment Repeated - Incompatible File');
          end;
        end;
    //Fill Transaction Set Dictionary
    for F := 0 to ICSpec.FunctionalGroupCount - 1 do
      for T := 0 to ICSpec[F].TransactionSetCount - 1 do
        for S := 0 to ICSpec[F][T].SegmentCount - 1 do
        begin
          TransactionSetSpec := TEDITransactionSetSpec(ICSpec[F][T]);
          if Result.SETS.FindItemByName(TransactionSetSpec.Id) = nil then
            Result.SETS.AddByNameOrId(TranslateToSEFSet(TransactionSetSpec, Result))
          else
          begin
            //raise Exception.Create('Segment Repeated - Incompatible File');
          end;
        end;
  finally
    ElementList.Free;
  end;

end;

//==================================================================================================
// TEDISEFToSpecTranslator
//==================================================================================================

constructor TEDISEFToSpecTranslator.Create;
begin
  inherited Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TEDISEFToSpecTranslator.Destroy;
begin
  inherited Destroy;
end;

end.
