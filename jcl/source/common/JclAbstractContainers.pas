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

  TJclAbstractContainerBase = class(TJclAbstractLockable, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclContainer)
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
    function CreateEmptyContainer: TJclAbstractContainerBase; virtual; abstract;
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); virtual;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); virtual;
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

  TJclAbstractIterator = class(TJclAbstractLockable, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclAbstractIterator)
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

  TJclIntfAbstractContainer = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclContainer, IJclIntfEqualityComparer, IJclIntfComparer)
  protected
    function FreeObject(var AInterface: IInterface): IInterface;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclIntfEqualityComparer }
    function ItemsEqual(const A, B: IInterface): Boolean;
    { IJclIntfComparer }
    function ItemsCompare(const A, B: IInterface): Integer;
  end;

  TJclStrAbstractContainer = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclContainer, IJclStrContainer)
  protected
    FCaseSensitive: Boolean;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclStrContainer }
    function GetCaseSensitive: Boolean; virtual;
    procedure SetCaseSensitive(Value: Boolean); virtual;
  end;

  TJclAnsiStrAbstractContainer = class(TJclStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer,
    IJclAnsiStrEqualityComparer, IJclAnsiStrComparer, IJclAnsiStrHashConverter)
  protected
    FEncoding: TJclAnsiStrEncoding;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function FreeString(var AString: AnsiString): AnsiString;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclAnsiStrContainer }
    function GetEncoding: TJclAnsiStrEncoding; virtual;
    procedure SetEncoding(Value: TJclAnsiStrEncoding); virtual;
    { IJclAnsiStrEqualityComparer }
    function ItemsEqual(const A, B: AnsiString): Boolean;
    { IJclAnsiStrComparer }
    function ItemsCompare(const A, B: AnsiString): Integer;
    { IJclAnsiStrHashConverter }
    function Hash(const AString: AnsiString): Integer;
  end;

  TJclWideStrAbstractContainer = class(TJclStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclContainer, IJclStrContainer, IJclWideStrContainer,
    IJclWideStrEqualityComparer, IJclWideStrComparer, IJclWideStrHashConverter)
  protected
    FEncoding: TJclWideStrEncoding;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function FreeString(var AString: WideString): WideString;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclWideStrContainer }
    function GetEncoding: TJclWideStrEncoding; virtual;
    procedure SetEncoding(Value: TJclWideStrEncoding); virtual;
    { IJclWideStrEqualityComparer }
    function ItemsEqual(const A, B: WideString): Boolean;
    { IJclWideStrComparer }
    function ItemsCompare(const A, B: WideString): Integer;
    { IJclWideStrHashConverter }
    function Hash(const AString: WideString): Integer;
  end;

  TJclAbstractContainer = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclContainer, IJclObjectOwner, IJclEqualityComparer, IJclComparer)
  private
    FOwnsObjects: Boolean;
  protected
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
  TJclAbstractContainer<T> = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>)
  private
    FOwnsItems: Boolean;
  protected
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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

  TJclAnsiStrAbstractCollection = class(TJclAnsiStrAbstractContainer,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclCloneable, IJclIntfCloneable, IJclContainer,
    IJclStrContainer, IJclAnsiStrContainer, IJclAnsiStrFlatContainer, IJclAnsiStrCollection,
    IJclAnsiStrEqualityComparer, IJclAnsiStrComparer)
  protected
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclAnsiStrCollection }
    function Add(const AString: AnsiString): Boolean; virtual; abstract;
    function AddAll(const ACollection: IJclAnsiStrCollection): Boolean; virtual; abstract;
    procedure Clear; virtual; abstract;
    function Contains(const AString: AnsiString): Boolean; virtual; abstract;
    function ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean; virtual; abstract;
    function Equals(const ACollection: IJclAnsiStrCollection): Boolean; virtual; abstract;
    function First: IJclAnsiStrIterator; virtual; abstract;
    function IsEmpty: Boolean; virtual; abstract;
    function Last: IJclAnsiStrIterator; virtual; abstract;
    function Remove(const AString: AnsiString): Boolean; overload; virtual; abstract;
    function RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean; virtual; abstract;
    function RetainAll(const ACollection: IJclAnsiStrCollection): Boolean; virtual; abstract;
    function Size: Integer; virtual; abstract;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclAnsistrIterator; virtual; abstract;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclAnsiStrFlatContainer }
    procedure LoadFromStrings(Strings: TStrings);
    procedure SaveToStrings(Strings: TStrings);
    procedure AppendToStrings(Strings: TStrings);
    procedure AppendFromStrings(Strings: TStrings);
    function GetAsStrings: TStrings;
    function GetAsDelimited(const Separator: AnsiString = AnsiLineBreak): AnsiString;
    procedure AppendDelimited(const AString: AnsiString; const Separator: AnsiString = AnsiLineBreak);
    procedure LoadDelimited(const AString: AnsiString; const Separator: AnsiString = AnsiLineBreak);
  end;

  TJclWideStrAbstractCollection = class(TJclWideStrAbstractContainer,
    {$IFDEF THREADSAFE}IJclLockable,{$ENDIF THREADSAFE} IJclCloneable, IJclIntfCloneable, IJclContainer,
    IJclStrContainer, IJclWideStrContainer, IJclWideStrFlatContainer, IJclWideStrCollection,
    IJclWideStrEqualityComparer, IJclWideStrComparer)
  protected
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclWideStrCollection }
    function Add(const AString: WideString): Boolean; virtual; abstract;
    function AddAll(const ACollection: IJclWideStrCollection): Boolean; virtual; abstract;
    procedure Clear; virtual; abstract;
    function Contains(const AString: WideString): Boolean; virtual; abstract;
    function ContainsAll(const ACollection: IJclWideStrCollection): Boolean; virtual; abstract;
    function Equals(const ACollection: IJclWideStrCollection): Boolean; virtual; abstract;
    function First: IJclWideStrIterator; virtual; abstract;
    function IsEmpty: Boolean; virtual; abstract;
    function Last: IJclWideStrIterator; virtual; abstract;
    function Remove(const AString: WideString): Boolean; overload; virtual; abstract;
    function RemoveAll(const ACollection: IJclWideStrCollection): Boolean; virtual; abstract;
    function RetainAll(const ACollection: IJclWideStrCollection): Boolean; virtual; abstract;
    function Size: Integer; virtual; abstract;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclWideStrIterator; virtual; abstract;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclWideStrFlatContainer }
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

{$IFNDEF RTL140_UP}
uses
  JclWideStrings;
{$ENDIF ~RTL140_UP}

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

//=== { TJclAbstractContainerBase } ==========================================

constructor TJclAbstractContainerBase.Create(const ALockDelegate: IInterface);
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

procedure TJclAbstractContainerBase.Assign(const Source: IJclContainer);
begin
  Source.AssignTo(Self);
end;

procedure TJclAbstractContainerBase.AssignDataTo(Dest: TJclAbstractContainerBase);
begin
  // override to customize
end;

procedure TJclAbstractContainerBase.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
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

procedure TJclAbstractContainerBase.AssignTo(const Dest: IJclContainer);
var
  DestObject: TObject;
begin
  DestObject := Dest.GetContainerReference;
  if DestObject is TJclAbstractContainerBase then
  begin
    AssignPropertiesTo(TJclAbstractContainerBase(DestObject));
    AssignDataTo(TJclAbstractContainerBase(DestObject));
  end
  else
    raise EJclAssignError.Create;
end;

procedure TJclAbstractContainerBase.AutoGrow;
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

procedure TJclAbstractContainerBase.AutoPack;
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

function TJclAbstractContainerBase.CheckDuplicate: Boolean;
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

function TJclAbstractContainerBase.Clone: TObject;
var
  NewContainer: TJclAbstractContainerBase;
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

function TJclAbstractContainerBase.GetAllowDefaultElements: Boolean;
begin
  Result := FAllowDefaultElements;
end;

function TJclAbstractContainerBase.GetAutoGrowParameter: Integer;
begin
  Result := FAutoGrowParameter;
end;

function TJclAbstractContainerBase.GetAutoGrowStrategy: TJclAutoGrowStrategy;
begin
  Result := FAutoGrowStrategy;
end;

function TJclAbstractContainerBase.GetAutoPackParameter: Integer;
begin
  Result := FAutoPackParameter;
end;

function TJclAbstractContainerBase.GetAutoPackStrategy: TJclAutoPackStrategy;
begin
  Result := FAutoPackStrategy;
end;

function TJclAbstractContainerBase.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TJclAbstractContainerBase.GetContainerReference: TObject;
begin
  Result := Self;
end;

function TJclAbstractContainerBase.GetDuplicates: TDuplicates;
begin
  Result := FDuplicates;
end;

function TJclAbstractContainerBase.GetRemoveSingleElement: Boolean;
begin
  Result := FRemoveSingleElement;
end;

function TJclAbstractContainerBase.GetReturnDefaultElements: Boolean;
begin
  Result := FReturnDefaultElements;
end;

procedure TJclAbstractContainerBase.Grow;
begin
  // override to customize
  AutoGrow;
end;

function TJclAbstractContainerBase.IntfClone: IInterface;
var
  NewContainer: TJclAbstractContainerBase;
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

procedure TJclAbstractContainerBase.Pack;
begin
  // override to customize
  SetCapacity(FSize);
end;

procedure TJclAbstractContainerBase.SetAllowDefaultElements(Value: Boolean);
begin
  FAllowDefaultElements := Value;
end;

procedure TJclAbstractContainerBase.SetAutoGrowParameter(Value: Integer);
begin
  FAutoGrowParameter := Value;
end;

procedure TJclAbstractContainerBase.SetAutoGrowStrategy(Value: TJclAutoGrowStrategy);
begin
  FAutoGrowStrategy := Value;
end;

procedure TJclAbstractContainerBase.SetAutoPackParameter(Value: Integer);
begin
  FAutoPackParameter := Value;
end;

procedure TJclAbstractContainerBase.SetAutoPackStrategy(Value: TJclAutoPackStrategy);
begin
  FAutoPackStrategy := Value;
end;

procedure TJclAbstractContainerBase.SetCapacity(Value: Integer);
begin
  FCapacity := Value;
end;

procedure TJclAbstractContainerBase.SetDuplicates(Value: TDuplicates);
begin
  FDuplicates := Value;
end;

procedure TJclAbstractContainerBase.SetRemoveSingleElement(Value: Boolean);
begin
  FRemoveSingleElement := Value;
end;

procedure TJclAbstractContainerBase.SetReturnDefaultElements(Value: Boolean);
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

//=== { TJclIntfAbstractContainer } ==========================================

function TJclIntfAbstractContainer.FreeObject(var AInterface: IInterface): IInterface;
begin
  Result := AInterface;
  AInterface := nil;
end;

function TJclIntfAbstractContainer.ItemsCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclIntfAbstractContainer.ItemsEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

//=== { TJclStrAbstractContainer } ===========================================

procedure TJclStrAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclStrAbstractContainer then
    TJclStrAbstractContainer(Dest).SetCaseSensitive(GetCaseSensitive);
end;

function TJclStrAbstractContainer.GetCaseSensitive: Boolean;
begin
  Result := FCaseSensitive;
end;

procedure TJclStrAbstractContainer.SetCaseSensitive(Value: Boolean);
begin
  FCaseSensitive := Value;
end;

//=== { TJclAnsiStrAbstractContainer } =======================================

procedure TJclAnsiStrAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclAnsiStrAbstractContainer then
    TJclAnsiStrAbstractContainer(Dest).SetEncoding(GetEncoding);
end;

function TJclAnsiStrAbstractContainer.FreeString(var AString: AnsiString): AnsiString;
begin
  Result := AString;
  AString := '';
end;

function TJclAnsiStrAbstractContainer.GetEncoding: TJclAnsiStrEncoding;
begin
  Result := FEncoding;
end;

function TJclAnsiStrAbstractContainer.Hash(const AString: AnsiString): Integer;
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
  else
    raise EJclOperationNotSupportedError.Create;
  end;
end;

function TJclAnsiStrAbstractContainer.ItemsCompare(const A, B: AnsiString): Integer;
begin
  case FEncoding of
    seISO:
      if FCaseSensitive then
        Result := CompareStr(A, B)
      else
        Result := CompareText(A, B);
    //seUTF8:
  else
    raise EJclOperationNotSupportedError.Create;
  end;
end;

function TJclAnsiStrAbstractContainer.ItemsEqual(const A, B: AnsiString): Boolean;
begin
  case FEncoding of
    seISO:
      if FCaseSensitive then
        Result := CompareStr(A, B) = 0
      else
        Result := CompareText(A, B) = 0;
    //seUTF8:
  else
    raise EJclOperationNotSupportedError.Create;
  end;
end;

procedure TJclAnsiStrAbstractContainer.SetEncoding(Value: TJclAnsiStrEncoding);
begin
  FEncoding := Value;
end;

//=== { TJclWideStrContainer } ===============================================

procedure TJclWideStrAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclWideStrAbstractContainer then
    TJclWideStrAbstractContainer(Dest).SetEncoding(GetEncoding);
end;

function TJclWideStrAbstractContainer.FreeString(var AString: WideString): WideString;
begin
  Result := AString;
  AString := '';
end;

function TJclWideStrAbstractContainer.GetEncoding: TJclWideStrEncoding;
begin
  Result := FEncoding;
end;

function TJclWideStrAbstractContainer.Hash(const AString: WideString): Integer;
var
  I: Integer;
begin
  Result := 0;
  case FEncoding of
    weUCS2:
      //if FCaseSensitive then
        for I := 1 to Length(AString) do
          Inc(Result, Ord(AString[I]) * (I - 1) * 65536)
      //else
      //  for I := 1 to Length(AString) do
      //    Inc(Result, Ord(AString[I]) * (I - 1) * 65536); // TODO: case folding
    //weUTF16:
  else
    raise EJclOperationNotSupportedError.Create;
  end;
end;

function TJclWideStrAbstractContainer.ItemsCompare(const A, B: WideString): Integer;
begin
  case FEncoding of
    weUCS2:
      if FCaseSensitive then
        Result := WideCompareStr(A, B)
      else
        Result := WideCompareText(A, B);
    //weUTF16:
  else
    raise EJclOperationNotSupportedError.Create;
  end;
end;

function TJclWideStrAbstractContainer.ItemsEqual(const A, B: WideString): Boolean;
begin
  case FEncoding of
    weUCS2:
      if FCaseSensitive then
        Result := WideCompareStr(A, B) = 0
      else
        Result := WideCompareText(A, B) = 0;
    //weUTF16:
  else
    raise EJclOperationNotSupportedError.Create;
  end;
end;

procedure TJclWideStrAbstractContainer.SetEncoding(Value: TJclWideStrEncoding);
begin
  FEncoding := Value;
end;

//=== { TJclAbstractContainer } ==============================================

constructor TJclAbstractContainer.Create(const ALockDelegate: IInterface; AOwnsObjects: Boolean);
begin
  inherited Create(ALockDelegate);
  FOwnsObjects := AOwnsObjects;
end;

function TJclAbstractContainer.FreeObject(var AObject: TObject): TObject;
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

function TJclAbstractContainer.GetOwnsObjects: Boolean;
begin
  Result := FOwnsObjects;
end;

function TJclAbstractContainer.ItemsCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclAbstractContainer.ItemsEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$IFDEF SUPPORTS_GENERICS}
//=== { TJclAbstractContainer<T> } ===========================================

constructor TJclAbstractContainer<T>.Create(const ALockDelegate: IInterface; AOwnsItems: Boolean);
begin
  inherited Create(ALockDelegate);
  FOwnsItems := AOwnsItems;
end;

function TJclAbstractContainer<T>.FreeItem(var AItem: T): T;
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

function TJclAbstractContainer<T>.GetOwnsItems: Boolean;
begin
  Result := FOwnsItems;
end;

function TJclAbstractContainer<T>.ItemsCompare(const A, B: T): Integer;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TJclAbstractContainer<T>.ItemsEqual(const A, B: T): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

{$ENDIF SUPPORTS_GENERICS}

//=== { TJclAnsiStrCollection } ==============================================

// TODO: common implementation, need a function to search for a string starting from
// a predefined index
procedure TJclAnsiStrAbstractCollection.AppendDelimited(const AString, Separator: AnsiString);
{$IFDEF CLR}
var
  I, StartIndex: Integer;
  BString: string;
begin
  I := Pos(Separator, AString);
  if I <> 0 then
  begin
    BString := AString;
    Dec(I); // to .NET string index base
    StartIndex := 0;
    repeat
      Add(BString.Substring(StartIndex, I - StartIndex + 1));
      StartIndex := I + 1;
      I := BString.IndexOf(Separator, StartIndex);
    until I < 0;
  end
  else
    Add(AString);
end;
{$ELSE}
var
  Item: AnsiString;
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

procedure TJclAnsiStrAbstractCollection.AppendFromStrings(Strings: TStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    Add(Strings[I]);
end;

procedure TJclAnsiStrAbstractCollection.AppendToStrings(Strings: TStrings);
var
  It: IJclAnsiStrIterator;
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

function TJclAnsiStrAbstractCollection.GetAsDelimited(const Separator: AnsiString): AnsiString;
var
  It: IJclAnsiStrIterator;
begin
  It := First;
  Result := '';
  if It.HasNext then
    Result := It.Next;
  while It.HasNext do
    Result := Result + Separator + It.Next;
end;

function TJclAnsiStrAbstractCollection.GetAsStrings: TStrings;
begin
  Result := TStringList.Create;
  try
    AppendToStrings(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TJclAnsiStrAbstractCollection.LoadDelimited(const AString, Separator: AnsiString);
begin
  Clear;
  AppendDelimited(AString, Separator);
end;

procedure TJclAnsiStrAbstractCollection.LoadFromStrings(Strings: TStrings);
begin
  Clear;
  AppendFromStrings(Strings);
end;

procedure TJclAnsiStrAbstractCollection.SaveToStrings(Strings: TStrings);
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

