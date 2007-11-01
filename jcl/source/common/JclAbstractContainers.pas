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
{ The Original Code is AbstractContainer.pas and DCL_Util.pas.                                     }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Daniele Teti (dade2004)                                                                        }
{   Robert Marquardt (marquardt)                                                                   }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ The Delphi Container Library                                                                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclAbstractContainers;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF CLR}
  System.Threading,
  {$ELSE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$ENDIF CLR}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  SyncObjs,
  SysUtils, Classes, JclBase, JclContainerIntf, JclSysUtils;

type
  {$IFDEF KEEP_DEPRECATED}
  TJclIntfCriticalSection = JclSysUtils.TJclIntfCriticalSection;
  {$ENDIF KEEP_DEPRECATED}

  TJclAbstractLockable = class(TInterfacedObject {$IFDEF THREADSAFE}, IJclLockable {$ENDIF THREADSAFE})
  {$IFDEF THREADSAFE}
  private
    FLockDelegate: IJclLockable;
    {$IFDEF CLR}
    FReaderWriterLock: ReaderWriterLock;
    FUpgradedWrite: Boolean;
    FLockCookie: LockCookie;
    {$ELSE ~CLR}
    FCriticalSection: TCriticalSection;
    {$ENDIF ~CLR}
  protected
    procedure ReadLock;
    procedure ReadUnlock;
    procedure WriteLock;
    procedure WriteUnlock;
  public
    destructor Destroy; override;
  {$ENDIF THREADSAFE}
  public
    constructor Create(const ALockDelegate: IInterface);
  end;

  TJclAbstractContainer = class(TJclAbstractLockable, IJclContainer,
    {$IFDEF THREADSAFE}IJclLockable,{$ENDIF THREADSAFE} IJclCloneable, IJclIntfCloneable)
  protected
    FAllowDefaultElements: Boolean;
    FDuplicates: TDuplicates;
    FRemoveSingleElement: Boolean;
    FReturnDefaultElements: Boolean;
    FCapacity: Integer;
    FSize: Integer;
    FAutoGrowParameter: Integer;
    FAutoGrowStrategy: TJclAutoGrowStrategy;
    FAutoPackParameter: Integer;
    FAutoPackStrategy: TJclAutoPackStrategy;
    procedure AutoGrow; virtual;
    procedure AutoPack; virtual;
    function CheckDuplicate: Boolean;
    function CreateEmptyContainer: TJclAbstractContainer; virtual; abstract;
    procedure AssignDataTo(Dest: TJclAbstractContainer); virtual;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); virtual;
    { IJclContainer }
    procedure Assign(const Source: IJclContainer);
    procedure AssignTo(const Dest: IJclContainer);
    function GetAllowDefaultElements: Boolean; virtual;
    function GetContainerReference: TObject;
    function GetDuplicates: TDuplicates; virtual;
    function GetRemoveSingleElement: Boolean; virtual;
    function GetReturnDefaultElements: Boolean; virtual;
    procedure SetAllowDefaultElements(Value: Boolean); virtual;
    procedure SetDuplicates(Value: TDuplicates); virtual;
    procedure SetRemoveSingleElement(Value: Boolean); virtual;
    procedure SetReturnDefaultElements(Value: Boolean); virtual;
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
    // IJclGrowable is not in interface list because some descendants won't use this code
    { IJclGrowable }
    function GetAutoGrowParameter: Integer; virtual;
    function GetAutoGrowStrategy: TJclAutoGrowStrategy; virtual;
    procedure Grow; virtual;
    procedure SetAutoGrowParameter(Value: Integer); virtual;
    procedure SetAutoGrowStrategy(Value: TJclAutoGrowStrategy); virtual;
    // IJclPackable is not in interface list because some descendants won't use this code
    { IJclPackable }
    function GetAutoPackParameter: Integer; virtual;
    function GetAutoPackStrategy: TJclAutoPackStrategy; virtual;
    function GetCapacity: Integer; virtual;
    procedure Pack; virtual;
    procedure SetAutoPackParameter(Value: Integer); virtual;
    procedure SetAutoPackStrategy(Value: TJclAutoPackStrategy); virtual;
    procedure SetCapacity(Value: Integer); virtual;
  public
    constructor Create(const ALockDelegate: IInterface);
  end;

  TJclAbstractIterator = class(TJclAbstractLockable, IJclAbstractIterator,
    {$IFDEF THREADSAFE}IJclLockable,{$ENDIF THREADSAFE} IJclCloneable, IJclIntfCloneable)
  private
    FValid: Boolean;
  protected
    procedure CheckValid;
    function CreateEmptyIterator: TJclAbstractIterator; virtual; abstract;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); virtual;
    { IJclAbstractIterator }
    procedure Assign(const Source: IJclAbstractIterator);
    procedure AssignTo(const Dest: IJclAbstractIterator);
    function GetIteratorReference: TObject;
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const ALockDelegate: IInterface; AValid: Boolean);
    property Valid: Boolean read FValid write FValid;
  end;

  TJclIntfContainer = class(TJclAbstractContainer, IJclContainer,
    {$IFDEF THREADSAFE}IJclLockable,{$ENDIF THREADSAFE} IJclIntfEqualityComparer, IJclIntfComparer)
  protected
    function FreeObject(var AInterface: IInterface): IInterface;
    { IJclIntfEqualityComparer }
    function ItemsEqual(const A, B: IInterface): Boolean;
    { IJclIntfComparer }
    function ItemsCompare(const A, B: IInterface): Integer;
  end;

  TJclStrContainer = class(TJclAbstractContainer, IJclContainer, IJclStrContainer,
    {$IFDEF THREADSAFE}IJclLockable,{$ENDIF THREADSAFE} IJclStrEqualityComparer, IJclStrComparer, IJclStrHashConverter)
  protected
    FCaseSensitive: Boolean;
    FEncoding: TJclAnsiStrEncoding;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    function FreeString(var AString: string): string;
    { IJclAnsiStrContainer }
    function GetCaseSensitive: Boolean; virtual;
    function GetEncoding: TJclAnsiStrEncoding; virtual;
    procedure SetCaseSensitive(Value: Boolean); virtual;
    procedure SetEncoding(Value: TJclAnsiStrEncoding); virtual;
    { IJclStrEqualityComparer }
    function ItemsEqual(const A, B: string): Boolean;
    { IJclStrComparer }
    function ItemsCompare(const A, B: string): Integer;
    { IJclStrHashConverter }
    function Hash(const AString: string): Integer;
  end;

  TJclContainer = class(TJclAbstractContainer, IJclContainer, IJclObjectOwner,
    {$IFDEF THREADSAFE}IJclLockable,{$ENDIF THREADSAFE} IJclEqualityComparer, IJclComparer)
  private
    FOwnsObjects: Boolean;
  protected
    { IJclEqualityComparer }
    function ItemsEqual(A, B: TObject): Boolean;
    { IJclComparer }
    function ItemsCompare(A, B: TObject): Integer;
    { IJclObjectOwner }
    function FreeObject(var AObject: TObject): TObject; virtual;
    function GetOwnsObjects: Boolean; virtual;
  public
    constructor Create(const ALockDelegate: IInterface; AOwnsObjects: Boolean);
    property OwnsObjects: Boolean read FOwnsObjects;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  TJclContainer<T> = class(TJclAbstractContainer, IJclContainer, IJclItemOwner<T>,
    {$IFDEF THREADSAFE}IJclLockable,{$ENDIF THREADSAFE} IJclEqualityComparer<T>, IJclComparer<T>)
  private
    FOwnsItems: Boolean;
  protected
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; virtual;
    { IJclComparer<T> }
    function ItemsCompare(const A, B: T): Integer; virtual;
    { IJclItemOwner<T> }
    function FreeItem(var AItem: T): T; virtual;
    function GetOwnsItems: Boolean; virtual;
  public
    constructor Create(const ALockDelegate: IInterface; AOwnsItems: Boolean);
    property OwnsItems: Boolean read FOwnsItems;
  end;
  {$ENDIF SUPPORTS_GENERICS}

  TJclStrAbstractCollection = class(TJclStrContainer, IJclContainer, IJclStrContainer, IJclStrFlatContainer,
    IJclStrCollection, {$IFDEF THREADSAFE}IJclLockable,{$ENDIF THREADSAFE} IJclStrEqualityComparer, IJclStrComparer)
  protected
    { IJclStrCollection }
    function Add(const AString: string): Boolean; virtual; abstract;
    function AddAll(const ACollection: IJclStrCollection): Boolean; virtual; abstract;
    procedure Clear; virtual; abstract;
    function Contains(const AString: string): Boolean; virtual; abstract;
    function ContainsAll(const ACollection: IJclStrCollection): Boolean; virtual; abstract;
    function Equals(const ACollection: IJclStrCollection): Boolean; virtual; abstract;
    function First: IJclStrIterator; virtual; abstract;
    function IsEmpty: Boolean; virtual; abstract;
    function Last: IJclStrIterator; virtual; abstract;
    function Remove(const AString: string): Boolean; overload; virtual; abstract;
    function RemoveAll(const ACollection: IJclStrCollection): Boolean; virtual; abstract;
    function RetainAll(const ACollection: IJclStrCollection): Boolean; virtual; abstract;
    function Size: Integer; virtual; abstract;
    procedure LoadFromStrings(Strings: TStrings);
    procedure SaveToStrings(Strings: TStrings);
    procedure AppendToStrings(Strings: TStrings);
    procedure AppendFromStrings(Strings: TStrings);
    function GetAsStrings: TStrings;
    function GetAsDelimited(const Separator: string = AnsiLineBreak): string;
    procedure AppendDelimited(const AString: string; const Separator: string = AnsiLineBreak);
    procedure LoadDelimited(const AString: string; const Separator: string = AnsiLineBreak);
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

//=== { TJclAbstractLockable } ===============================================

constructor TJclAbstractLockable.Create(const ALockDelegate: IInterface);
begin
  inherited Create;
  {$IFDEF THREADSAFE}
  FLockDelegate := ALockDelegate as IJclLockable;
  if FLockDelegate = nil then
    {$IFDEF CLR}
    FReaderWriterLock := ReaderWriterLock.Create;
    {$ELSE ~CLR}
    FCriticalSection := TCriticalSection.Create;
    {$ENDIF ~CLR}
  {$ENDIF THREADSAFE}
end;

{$IFDEF THREADSAFE}
destructor TJclAbstractLockable.Destroy;
begin
  {$IFDEF CLR}
  FReaderWriterLock.Free;
  {$ELSE ~CLR}
  FCriticalSection.Free;
  {$ENDIF ~CLR}
  FLockDelegate := nil;
  inherited Destroy;
end;

procedure TJclAbstractLockable.ReadLock;
begin
  if FLockDelegate <> nil then
    FLockDelegate.ReadLock
  else
    {$IFDEF CLR}
    // if current thread has write access, no need to request a read access
    if not FReaderWriterLock.IsWriterLockHeld then
      FReaderWriterLock.AcquireReaderLock(-1);
    {$ELSE ~CLR}
    FCriticalSection.Acquire;
    {$ENDIF ~CLR}
end;

procedure TJclAbstractLockable.ReadUnlock;
begin
  if FLockDelegate <> nil then
    FLockDelegate.ReadUnlock
  else
    {$IFDEF CLR}
    // if current thread has write access, no need to release read access
    if not FReaderWriterLock.IsWriterLockHeld then
      FReaderWriterLock.ReleaseReaderLock;
    {$ELSE ~CLR}
    FCriticalSection.Release;
    {$ENDIF ~CLR}
end;

procedure TJclAbstractLockable.WriteLock;
begin
  if FLockDelegate <> nil then
    FLockDelegate.WriteLock
  else
    {$IFDEF CLR}
    if FReaderWriterLock.IsReaderLockHeld then
    begin
      FLockCookie := FReaderWriterLock.UpgradeToWriterLock(-1);
      FUpgradedWrite := True;
    end
    else
      FReaderWriterLock.AcquireWriterLock(-1);
    {$ELSE ~CLR}
    FCriticalSection.Acquire;
    {$ENDIF ~CLR}
end;

procedure TJclAbstractLockable.WriteUnlock;
begin
  if FLockDelegate <> nil then
    FLockDelegate.WriteUnlock
  else
    {$IFDEF CLR}
    if FUpgradedWrite then
    begin
      FUpgradedWrite := False;
      FReaderWriterLock.DowngradeFromWriterLock(FLockCookie);
    end
    else
      FReaderWriterLock.ReleaseWriterLock;
    {$ELSE ~CLR}
    FCriticalSection.Release;
    {$ENDIF ~CLR}
end;
{$ENDIF THREADSAFE}

//=== { TJclAbstractContainer } ==============================================

constructor TJclAbstractContainer.Create(const ALockDelegate: IInterface);
begin
  inherited Create(ALockDelegate);

  FAllowDefaultElements := True;
  FDuplicates := dupAccept;
  FRemoveSingleElement := True;
  FReturnDefaultElements := True;
  FAutoGrowStrategy := agsProportional;
  FAutoGrowParameter := 4;
  FAutoPackStrategy := apsDisabled;
  FAutoPackParameter := 4;
end;

procedure TJclAbstractContainer.Assign(const Source: IJclContainer);
begin
  Source.AssignTo(Self);
end;

procedure TJclAbstractContainer.AssignDataTo(Dest: TJclAbstractContainer);
begin
  // override to customize
end;

procedure TJclAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  // override to customize
  Dest.SetAllowDefaultElements(GetAllowDefaultElements);
  Dest.SetDuplicates(GetDuplicates);
  Dest.SetRemoveSingleElement(GetRemoveSingleElement);
  Dest.SetReturnDefaultElements(GetReturnDefaultElements);
  Dest.SetAutoGrowParameter(GetAutoGrowParameter);
  Dest.SetAutoGrowStrategy(GetAutoGrowStrategy);
  Dest.SetAutoPackParameter(GetAutoPackParameter);
  Dest.SetAutoPackStrategy(GetAutoPackStrategy);
end;

procedure TJclAbstractContainer.AssignTo(const Dest: IJclContainer);
var
  DestObject: TObject;
begin
  DestObject := Dest.GetContainerReference;
  if DestObject is TJclAbstractContainer then
  begin
    AssignPropertiesTo(TJclAbstractContainer(DestObject));
    AssignDataTo(TJclAbstractContainer(DestObject));
  end
  else
    raise EJclAssignError.Create;
end;

procedure TJclAbstractContainer.AutoGrow;
begin
  case FAutoGrowStrategy of
    agsDisabled: ;
    agsAgressive:
      SetCapacity(FCapacity + 1);
    agsProportional:
      SetCapacity(FCapacity + FCapacity div FAutoGrowParameter);
    agsIncremental:
      SetCapacity(FCapacity + FAutoGrowParameter);
  end;
end;

procedure TJclAbstractContainer.AutoPack;
var
  Decrement: Integer;
begin
  case FAutoPackStrategy of
    apsDisabled:
      Decrement := 0;
    apsAgressive:
      Decrement := 1;
    apsProportional:
      Decrement := FCapacity div FAutoPackParameter;
    apsIncremental:
      Decrement := FAutoPackParameter;
  else
    Decrement := 0;
  end;
  if (Decrement > 0) and ((FSize + Decrement) <= FCapacity) then
    SetCapacity(FSize);
end;

function TJclAbstractContainer.CheckDuplicate: Boolean;
begin
  case FDuplicates of
    dupIgnore:
      Result := False;
    dupAccept:
      Result := True;
    //dupError: ;
  else
    raise EJclDuplicateElementError.Create;
  end;
end;

function TJclAbstractContainer.Clone: TObject;
var
  NewContainer: TJclAbstractContainer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewContainer := CreateEmptyContainer;
    AssignDataTo(NewContainer);
    Result := NewContainer;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAbstractContainer.GetAllowDefaultElements: Boolean;
begin
  Result := FAllowDefaultElements;
end;

function TJclAbstractContainer.GetAutoGrowParameter: Integer;
begin
  Result := FAutoGrowParameter;
end;

function TJclAbstractContainer.GetAutoGrowStrategy: TJclAutoGrowStrategy;
begin
  Result := FAutoGrowStrategy;
end;

function TJclAbstractContainer.GetAutoPackParameter: Integer;
begin
  Result := FAutoPackParameter;
end;

function TJclAbstractContainer.GetAutoPackStrategy: TJclAutoPackStrategy;
begin
  Result := FAutoPackStrategy;
end;

function TJclAbstractContainer.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TJclAbstractContainer.GetContainerReference: TObject;
begin
  Result := Self;
end;

function TJclAbstractContainer.GetDuplicates: TDuplicates;
begin
  Result := FDuplicates;
end;

function TJclAbstractContainer.GetRemoveSingleElement: Boolean;
begin
  Result := FRemoveSingleElement;
end;

function TJclAbstractContainer.GetReturnDefaultElements: Boolean;
begin
  Result := FReturnDefaultElements;
end;

procedure TJclAbstractContainer.Grow;
begin
  // override to customize
  AutoGrow;
end;

function TJclAbstractContainer.IntfClone: IInterface;
var
  NewContainer: TJclAbstractContainer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewContainer := CreateEmptyContainer;
    AssignDataTo(NewContainer);
    Result := NewContainer;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAbstractContainer.Pack;
begin
  // override to customize
  SetCapacity(FSize);
end;

procedure TJclAbstractContainer.SetAllowDefaultElements(Value: Boolean);
begin
  FAllowDefaultElements := Value;
end;

procedure TJclAbstractContainer.SetAutoGrowParameter(Value: Integer);
begin
  FAutoGrowParameter := Value;
end;

procedure TJclAbstractContainer.SetAutoGrowStrategy(Value: TJclAutoGrowStrategy);
begin
  FAutoGrowStrategy := Value;
end;

procedure TJclAbstractContainer.SetAutoPackParameter(Value: Integer);
begin
  FAutoPackParameter := Value;
end;

procedure TJclAbstractContainer.SetAutoPackStrategy(Value: TJclAutoPackStrategy);
begin
  FAutoPackStrategy := Value;
end;

procedure TJclAbstractContainer.SetCapacity(Value: Integer);
begin
  FCapacity := Value;
end;

procedure TJclAbstractContainer.SetDuplicates(Value: TDuplicates);
begin
  FDuplicates := Value;
end;

procedure TJclAbstractContainer.SetRemoveSingleElement(Value: Boolean);
begin
  FRemoveSingleElement := Value;
end;

procedure TJclAbstractContainer.SetReturnDefaultElements(Value: Boolean);
begin
  FReturnDefaultElements := Value;
end;

//=== { TJclAbstractIterator } ===============================================

constructor TJclAbstractIterator.Create(const ALockDelegate: IInterface; AValid: Boolean);
begin
  inherited Create(ALockDelegate);
  FValid := AValid;
end;

procedure TJclAbstractIterator.Assign(const Source: IJclAbstractIterator);
begin
  Source.AssignTo(Self);
end;

procedure TJclAbstractIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
begin
  Dest.FValid := FValid;
end;

procedure TJclAbstractIterator.AssignTo(const Dest: IJclAbstractIterator);
var
  DestObject: TObject;
begin
  DestObject := Dest.GetIteratorReference;
  if DestObject is TJclAbstractIterator then
    AssignPropertiesTo(TJclAbstractIterator(DestObject))
  else
    raise EJclAssignError.Create;
end;

procedure TJclAbstractIterator.CheckValid;
begin
  if not Valid then
    raise EJclIllegalStateOperationError.Create;
end;

function TJclAbstractIterator.Clone: TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyIterator;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAbstractIterator.GetIteratorReference: TObject;
begin
  Result := Self;
end;

function TJclAbstractIterator.IntfClone: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyIterator;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclIntfContainer } ==================================================

function TJclIntfContainer.FreeObject(var AInterface: IInterface): IInterface;
begin
  Result := AInterface;
  AInterface := nil;
end;

function TJclIntfContainer.ItemsCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclIntfContainer.ItemsEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

//=== { TJclStrContainer } ===================================================

function TJclStrContainer.Hash(const AString: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  case FEncoding of
    seISO:
      if FCaseSensitive then
        for I := 1 to Length(AString) do
          Inc(Result, Ord(AString[I]) * (I - 1) * 256)
      else
        for I := 1 to Length(AString) do
          Inc(Result, Ord(UpCase(AString[I])) * (I - 1) * 256);
    //seUTF8:
    //  Result := 0;
  end;
end;

function TJclStrContainer.ItemsCompare(const A, B: string): Integer;
begin
  case FEncoding of
    seISO:
      if FCaseSensitive then
        Result := CompareStr(A, B)
      else
        Result := CompareText(A, B);
    //seUTF8:
    //  Result := 0;
  else
    Result := 0;
  end;
end;

function TJclStrContainer.ItemsEqual(const A, B: string): Boolean;
begin
  case FEncoding of
    seISO:
      if FCaseSensitive then
        Result := CompareStr(A, B) = 0
      else
        Result := CompareText(A, B) = 0;
    //seUTF8:
    //  Result := 0;
  else
    Result := False;
  end;
end;

procedure TJclStrContainer.AssignPropertiesTo(Dest: TJclAbstractContainer);
var
  ADest: TJclStrContainer;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclStrContainer then
  begin
    ADest := TJclStrContainer(Dest);
    ADest.SetCaseSensitive(GetCaseSensitive);
    ADest.SetEncoding(GetEncoding);
  end;
end;

function TJclStrContainer.FreeString(var AString: string): string;
begin
  Result := AString;
  AString := '';
end;

function TJclStrContainer.GetCaseSensitive: Boolean;
begin
  Result := FCaseSensitive;
end;

function TJclStrContainer.GetEncoding: TJclAnsiStrEncoding;
begin
  Result := FEncoding;
end;

procedure TJclStrContainer.SetCaseSensitive(Value: Boolean);
begin
  FCaseSensitive := Value;
end;

procedure TJclStrContainer.SetEncoding(Value: TJclAnsiStrEncoding);
begin
  FEncoding := Value;
end;

//=== { TJclContainer } ======================================================

constructor TJclContainer.Create(const ALockDelegate: IInterface; AOwnsObjects: Boolean);
begin
  inherited Create(ALockDelegate);
  FOwnsObjects := AOwnsObjects;
end;

function TJclContainer.FreeObject(var AObject: TObject): TObject;
begin
  if FOwnsObjects then
  begin
    Result := nil;
    FreeAndNil(AObject);
  end
  else
  begin
    Result := AObject;
    AObject := nil;
  end;
end;

function TJclContainer.GetOwnsObjects: Boolean;
begin
  Result := FOwnsObjects;
end;

function TJclContainer.ItemsCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclContainer.ItemsEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$IFDEF SUPPORTS_GENERICS}
//=== { TJclContainer<T> } ===================================================

constructor TJclContainer<T>.Create(const ALockDelegate: IInterface; AOwnsItems: Boolean);
begin
  inherited Create(ALockDelegate);
  FOwnsItems := AOwnsItems;
end;

function TJclContainer<T>.FreeItem(var AItem: T): T;
begin
  if FOwnsItems then
  begin
    Result := Default(T);
    FreeAndNil(AItem);
  end
  else
  begin
    Result := AItem;
    AItem := Default(T);
  end;
end;

function TJclContainer<T>.GetOwnsItems: Boolean;
begin
  Result := FOwnsItems;
end;

function TJclContainer<T>.ItemsCompare(const A, B: T): Integer;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclContainer<T>.ItemsEqual(const A, B: T): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

{$ENDIF SUPPORTS_GENERICS}

//=== { TJclStrCollection } ==================================================

procedure TJclStrAbstractCollection.AppendDelimited(const AString, Separator: string);
{$IFDEF CLR}
var
  I, StartIndex: Integer;
begin
  I := Pos(Separator, AString);
  if I <> 0 then
  begin
    Dec(I); // to .NET string index base
    StartIndex := 0;
    repeat
      Add(AString.Substring(StartIndex, I - StartIndex + 1));
      StartIndex := I + 1;
      I := AString.IndexOf(Separator, StartIndex);
    until I < 0;
  end
  else
    Add(AString);
end;
{$ELSE}
var
  Item: string;
  SepLen: Integer;
  PString, PSep, PPos: PChar;
begin
  PString := PChar(AString);
  PSep := PChar(Separator);
  PPos := StrPos(PString, PSep);
  if PPos <> nil then
  begin
    SepLen := StrLen(PSep);
    repeat
      //SetLength(Item, PPos - PString + 1);
      SetLength(Item, PPos - PString);
      Move(PString^, Item[1], PPos - PString);
      //Item[PPos - PString + 1] := #0;
      Add(Item);
      PString := PPos + SepLen;
      PPos := StrPos(PString, PSep);
    until PPos = nil;
    if StrLen(PString) > 0 then //ex. hello#world
      Add(PString);
  end
  else //There isnt a Separator in AString
    Add(AString);
end;
{$ENDIF CLR}

procedure TJclStrAbstractCollection.AppendFromStrings(Strings: TStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    Add(Strings[I]);
end;

procedure TJclStrAbstractCollection.AppendToStrings(Strings: TStrings);
var
  It: IJclStrIterator;
begin
  It := First;
  Strings.BeginUpdate;
  try
    while It.HasNext do
      Strings.Add(It.Next);
  finally
    Strings.EndUpdate;
  end;
end;

function TJclStrAbstractCollection.GetAsDelimited(const Separator: string): string;
var
  It: IJclStrIterator;
begin
  It := First;
  Result := '';
  if It.HasNext then
    Result := It.Next;
  while It.HasNext do
    Result := Result + Separator + It.Next;
end;

function TJclStrAbstractCollection.GetAsStrings: TStrings;
begin
  Result := TStringList.Create;
  try
    AppendToStrings(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TJclStrAbstractCollection.LoadDelimited(const AString, Separator: string);
begin
  Clear;
  AppendDelimited(AString, Separator);
end;

procedure TJclStrAbstractCollection.LoadFromStrings(Strings: TStrings);
begin
  Clear;
  AppendFromStrings(Strings);
end;

procedure TJclStrAbstractCollection.SaveToStrings(Strings: TStrings);
begin
  Strings.Clear;
  AppendToStrings(Strings);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

