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
{ Last modified: May 25, 2003                                                                      }
{ Additional Info:                                                                                 }
{   E-Mail at RaysDelphiBox3@hotmail.com                                                           }
{   Help and Demos at http://24.54.82.216/DelphiJedi/Default.htm                                   }
{    My website is usually available between 8:00am-10:00pm EST                                    }
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

{$I jedi.inc}

{$WEAKPACKAGEUNIT ON}

interface

uses
  SysUtils, Classes, JclBase, JclStrings{, Dialogs};

//These resource strings replace all those in JclResources.pas
resourcestring
  RsEDIError001 = 'Could not open edi file.  File not specified.';
  RsEDIError002 = 'Could not save edi file.  File name and path not specified.';
  RsEDIError003 = 'Could not get data object from %s at index [%s],';
  RsEDIError004 = 'Could not get data object from %s at index [%s], Index too low.';
  RsEDIError005 = 'Could not get data object from %s at index [%s], Index too high.';
  RsEDIError006 = 'Could not get data object from %s at index [%s], There was no data object assigned.';
  RsEDIError007 = 'Could not set data object from %s at index [%s].';
  RsEDIError008 = 'Could not set data object from %s at index [%s], Index too low.';
  RsEDIError009 = 'Could not set data object from %s at index [%s], Index too high.';
  RsEDIError010 = 'Could not delete data object from %s at index [%s]';
  RsEDIError011 = 'Could not delete data objects from %s at index [%s]';
  RsEDIError012 = 'Delimiters have not been assigned to interchange.  Dissassemble cancelled.';
  RsEDIError013 = 'Delimiters have not been assigned to interchange.  Assemble cancelled.';
  RsEDIError014 = 'Could not find interchange control header segment terminator.';
  RsEDIError015 = 'Could not find interchange control header.';
  RsEDIError016 = 'Could not find interchange control trailer segment terminator.';
  RsEDIError017 = 'Could not find interchange control trailer.';
  RsEDIError018 = 'Could not find interchange control trailer or garbage at end of file.';
  RsEDIError019 = 'Could not assign delimiters to functional group.  Dissassemble cancelled.';
  RsEDIError020 = 'Could not assign delimiters to functional group.  Assemble cancelled.';
  RsEDIError021 = 'Could not find functional group header segment terminator.';
  RsEDIError022 = 'Could not find functional group header.'; //conditional for UN/EDIFACT
  RsEDIError023 = 'Could not find functional group trailer segment terminator.';
  RsEDIError024 = 'Could not find functional group trailer.';
  RsEDIError025 = 'Could not assign delimiters to transaction set.  Dissassemble cancelled.';
  RsEDIError026 = 'Could not assign delimiters to transaction set.  Assemble cancelled.';
  RsEDIError027 = 'Could not find transaction set header.';
  RsEDIError028 = 'Could not find transaction set trailer segment terminator.';
  RsEDIError029 = 'Could not find transaction set trailer.';
  RsEDIError030 = 'Could not assign delimiters to message.  Dissassemble cancelled.';
  RsEDIError031 = 'Could not assign delimiters to message.  Assemble cancelled.';
  RsEDIError032 = 'Could not find message header.';
  RsEDIError033 = 'Could not find message trailer segment terminator.';
  RsEDIError034 = 'Could not find message trailer.';
  RsEDIError035 = 'Could not assign delimiters to segment.  Dissassemble cancelled.';
  RsEDIError036 = 'Could not assign delimiters to segment.  Assemble cancelled.';
  RsEDIError037 = 'Could not assign delimiters to composite element.  Dissassemble cancelled.';
  RsEDIError038 = 'Could not assign delimiters to composite element.  Assemble cancelled.';
  RsEDIError039 = 'Could not get data object in transaction set loop at index [%s], Data object does not exist.';
  RsEDIError040 = 'Could not get data object in transaction set loop at index [%s], Index too high.';
  RsEDIError041 = 'Could not get data object in transaction set loop at index [%s], Index too low.';
  RsEDIError042 = 'Could not get data object in transaction set loop at index [%s].';
  RsEDIError043 = 'Could not set data object in transaction set loop at index [%s], Index too high.';
  RsEDIError044 = 'Could not set data object in transaction set loop at index [%s], Index too low.';
  RsEDIError045 = 'Could not set data object in transaction set loop at index [%s].';
  RsEDIError046 = 'Could not get data object in message loop at index [%s], Data object does not exist.';
  RsEDIError047 = 'Could not get data object in message loop at index [%s], Index too high.';
  RsEDIError048 = 'Could not get data object in message loop at index [%s], Index too low.';
  RsEDIError049 = 'Could not get data object in message loop at index [%s].';
  RsEDIError050 = 'Could not set data object in message loop at index [%s], Index too high.';
  RsEDIError051 = 'Could not set data object in message loop at index [%s], Index too low.';
  RsEDIError052 = 'Could not set data object in message loop at index [%s].';
  RsEDIError053 = 'Loop in loop stack record at index [%s] does not exist.';
  RsEDIError054 = 'Could not get loop stack record at index [%s], Index too high.';
  RsEDIError055 = 'Could not get loop stack record at index [%s], Index too low.';
  RsEDIError056 = 'Could not get loop stack record at index [%s].';
  RsEDIError057 = 'Could not get safe loop stack index [%s].';
  RsEDIError058 = 'Could not assign element specification to element at index [%s]' +
                   ' in segment [%s] at index [%s] in transaction set.';

const
  NA_LoopId = 'N/A';    //Constant used for loop id comparison
  ElementSpecId_Reserved = 'Reserved';

type

  TEDIObject = class(TObject); //Base EDI Object
  TEDIObjectArray = array of TEDIObject;

  EJclEDIError = EJclError;

//--------------------------------------------------------------------------------------------------
//  EDI Forward Class Declarations
//--------------------------------------------------------------------------------------------------

  TEDIDataObject = class;
  TEDIDataObjectGroup = class;

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
  protected
    FEDIDataObjects: TEDIDataObjectArray;
    FCreateObjectType: TEDIDataObjectType;
    function GetEDIDataObject(Index: Integer): TEDIDataObject;
    procedure SetEDIDataObject(Index: Integer; EDIDataObject: TEDIDataObject);
    function InternalAssignDelimiters: TEDIDelimiters; virtual; abstract;
    function InternalCreateEDIDataObject: TEDIDataObject; virtual; abstract;
  public
    constructor Create(Parent: TEDIDataObject); reintroduce; overload;
    constructor Create(Parent: TEDIDataObject; EDIDataObjectCount: Integer); reintroduce; overload;
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

constructor TEDIDataObjectGroup.Create(Parent: TEDIDataObject);
begin
  if Assigned(Parent) then
  begin
    inherited Create(Parent);
  end
  else
  begin
    inherited Create(nil);
  end;
  SetLength(FEDIDataObjects, 0);
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
  SetLength(FEDIDataObjects, 0);
  AddEDIDataObjects(EDIDataObjectCount);
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
    FEDIDataObjects[InsertIndex] := InternalCreateEDIDataObject;
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

end.
