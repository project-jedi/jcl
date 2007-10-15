{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

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
{ The Original Code is Stack.pas.                                                                  }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
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

unit JclStacks;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF SUPPORTS_GENERICS}
  {$IFDEF CLR}
  System.Collections.Generic,
  {$ENDIF CLR}
  JclAlgorithms,
  {$ENDIF SUPPORTS_GENERICS}
  JclBase, JclAbstractContainers, JclContainerIntf;

type

  TJclIntfStack = class(TJclIntfContainer, IJclIntfStack,IJclContainer, IJclIntfEqualityComparer,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FElements: JclBase.TDynIInterfaceArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainer); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclIntfStack }
    procedure Clear;
    function Contains(const AInterface: IInterface): Boolean;
    function Empty: Boolean;
    function Peek: IInterface;
    function Pop: IInterface;
    function Push(const AInterface: IInterface): Boolean;
    function Size: Integer;
    function CreateEmptyContainer: TJclAbstractContainer; override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;


  TJclStrStack = class(TJclStrContainer, IJclStrStack,IJclContainer, IJclStrContainer, IJclStrEqualityComparer,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FElements: JclBase.TDynStringArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainer); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclStrStack }
    procedure Clear;
    function Contains(const AString: string): Boolean;
    function Empty: Boolean;
    function Peek: string;
    function Pop: string;
    function Push(const AString: string): Boolean;
    function Size: Integer;
    function CreateEmptyContainer: TJclAbstractContainer; override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;


  TJclStack = class(TJclContainer, IJclStack,IJclContainer, IJclEqualityComparer, IJclObjectOwner,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FElements: JclBase.TDynObjectArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainer); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclStack }
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function Empty: Boolean;
    function Peek: TObject;
    function Pop: TObject;
    function Push(AObject: TObject): Boolean;
    function Size: Integer;
    function CreateEmptyContainer: TJclAbstractContainer; override;
  public
    constructor Create(ACapacity: Integer; AOwnsObjects: Boolean);
    destructor Destroy; override;
  end;

  {$IFDEF SUPPORTS_GENERICS}


  TJclStack<T> = class(TJclContainer<T>, IJclStack<T>,IJclContainer, IJclEqualityComparer<T>, IJclItemOwner<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FElements: TJclBase<T>.TDynArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainer); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclStack<T> }
    procedure Clear;
    function Contains(const AItem: T): Boolean;
    function Empty: Boolean;
    function Peek: T;
    function Pop: T;
    function Push(const AItem: T): Boolean;
    function Size: Integer;
  public
    constructor Create(ACapacity: Integer; AOwnsItems: Boolean);
    destructor Destroy; override;
  end;

  // E = external helper to compare items for equality
  TJclStackE<T> = class(TJclStack<T>, IJclStack<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FEqualityComparer: IEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    function CreateEmptyContainer: TJclAbstractContainer; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityComparer: IEqualityComparer<T>;
      ACapacity: Integer = DefaultContainerCapacity; AOwnsItems: Boolean = True);

    property EqualityComparer: IEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclStackF<T> = class(TJclStack<T>, IJclStack<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FEqualityCompare: TEqualityCompare<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    function CreateEmptyContainer: TJclAbstractContainer; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(AEqualityCompare: TEqualityCompare<T>;
      ACapacity: Integer = DefaultContainerCapacity; AOwnsItems: Boolean = True);

    property EqualityCompare: TEqualityCompare<T> read FEqualityCompare write FEqualityCompare;
  end;

  // I = items can compare themselves to an other for equality
  TJclStackI<T: IEquatable<T>> = class(TJclStack<T>, IJclStack<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  protected
    function CreateEmptyContainer: TJclAbstractContainer; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;
  {$ENDIF SUPPORTS_GENERICS}

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

uses
  SysUtils;


//=== { TJclIntfStack } =======================================================

constructor TJclIntfStack.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
end;

destructor TJclIntfStack.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclIntfStack.AssignDataTo(Dest: TJclAbstractContainer);
var
  ADest: TJclIntfStack;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfStack then
  begin
    ADest := TJclIntfStack(Dest);
    ADest.Clear;
    ADest.SetCapacity(FSize + 1);
    for I := 0 to FSize - 1 do
      ADest.FElements[I] := FElements[I];
    ADest.FSize := FSize;
  end;
end;

procedure TJclIntfStack.Clear;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeObject(FElements[I]);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfStack.Contains(const AInterface: IInterface): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElements[I], AInterface) then
      begin
        Result := True;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfStack.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclIntfStack.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfStack.Empty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfStack.Peek: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FElements[FSize - 1]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfStack.Pop: IInterface;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
    begin
      Dec(FSize);
      Result := FElements[FSize];
      FElements[FSize] := nil;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfStack.Push(const AInterface: IInterface): Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize = FCapacity then
      AutoGrow;
    Result := FSize < FCapacity;
    if Result then
    begin
      FElements[FSize] := AInterface;
      Inc(FSize);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfStack.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Value < FSize then
      raise EJclOutOfBoundsError.Create;
    SetLength(FElements, Value);
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfStack.Size: Integer;
begin
  Result := FSize;
end;


//=== { TJclStrStack } =======================================================

constructor TJclStrStack.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
end;

destructor TJclStrStack.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclStrStack.AssignDataTo(Dest: TJclAbstractContainer);
var
  ADest: TJclStrStack;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclStrStack then
  begin
    ADest := TJclStrStack(Dest);
    ADest.Clear;
    ADest.SetCapacity(FSize + 1);
    for I := 0 to FSize - 1 do
      ADest.FElements[I] := FElements[I];
    ADest.FSize := FSize;
  end;
end;

procedure TJclStrStack.Clear;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeString(FElements[I]);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStack.Contains(const AString: string): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElements[I], AString) then
      begin
        Result := True;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStack.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclStrStack.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclStrStack.Empty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclStrStack.Peek: string;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FElements[FSize - 1]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStack.Pop: string;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
    begin
      Dec(FSize);
      Result := FElements[FSize];
      FElements[FSize] := '';
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStack.Push(const AString: string): Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize = FCapacity then
      AutoGrow;
    Result := FSize < FCapacity;
    if Result then
    begin
      FElements[FSize] := AString;
      Inc(FSize);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrStack.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Value < FSize then
      raise EJclOutOfBoundsError.Create;
    SetLength(FElements, Value);
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStack.Size: Integer;
begin
  Result := FSize;
end;


//=== { TJclStack } =======================================================

constructor TJclStack.Create(ACapacity: Integer; AOwnsObjects: Boolean);
begin
  inherited Create(nil, AOwnsObjects);
  SetCapacity(ACapacity);
end;

destructor TJclStack.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclStack.AssignDataTo(Dest: TJclAbstractContainer);
var
  ADest: TJclStack;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclStack then
  begin
    ADest := TJclStack(Dest);
    ADest.Clear;
    ADest.SetCapacity(FSize + 1);
    for I := 0 to FSize - 1 do
      ADest.FElements[I] := FElements[I];
    ADest.FSize := FSize;
  end;
end;

procedure TJclStack.Clear;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeObject(FElements[I]);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack.Contains(AObject: TObject): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElements[I], AObject) then
      begin
        Result := True;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclStack.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclStack.Empty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclStack.Peek: TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FElements[FSize - 1]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack.Pop: TObject;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
    begin
      Dec(FSize);
      Result := FElements[FSize];
      FElements[FSize] := nil;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack.Push(AObject: TObject): Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize = FCapacity then
      AutoGrow;
    Result := FSize < FCapacity;
    if Result then
    begin
      FElements[FSize] := AObject;
      Inc(FSize);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStack.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Value < FSize then
      raise EJclOutOfBoundsError.Create;
    SetLength(FElements, Value);
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack.Size: Integer;
begin
  Result := FSize;
end;

{$IFDEF SUPPORTS_GENERICS}


//=== { TJclStack<T> } =======================================================

constructor TJclStack<T>.Create(ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(nil, AOwnsItems);
  SetCapacity(ACapacity);
end;

destructor TJclStack<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclStack<T>.AssignDataTo(Dest: TJclAbstractContainer);
var
  ADest: TJclStack<T>;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclStack<T> then
  begin
    ADest := TJclStack<T>(Dest);
    ADest.Clear;
    ADest.SetCapacity(FSize + 1);
    for I := 0 to FSize - 1 do
      ADest.FElements[I] := FElements[I];
    ADest.FSize := FSize;
  end;
end;

procedure TJclStack<T>.Clear;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeItem(FElements[I]);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack<T>.Contains(const AItem: T): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElements[I], AItem) then
      begin
        Result := True;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclStack<T>.Empty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclStack<T>.Peek: T;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if FSize > 0 then
      Result := FElements[FSize - 1]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack<T>.Pop: T;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if FSize > 0 then
    begin
      Dec(FSize);
      Result := FElements[FSize];
      FElements[FSize] := Default(T);
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack<T>.Push(const AItem: T): Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize = FCapacity then
      AutoGrow;
    Result := FSize < FCapacity;
    if Result then
    begin
      FElements[FSize] := AItem;
      Inc(FSize);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStack<T>.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Value < FSize then
      raise EJclOutOfBoundsError.Create;
    SetLength(FElements, Value);
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack<T>.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclStackE<T> } ======================================================

constructor TJclStackE<T>.Create(const AEqualityComparer: IEqualityComparer<T>; ACapacity: Integer;
  AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

procedure TJclStackE<T>.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclStackE<T> then
    TJclStackE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclStackE<T>.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclStackE<T>.Create(FEqualityComparer, FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclStackE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityComparer.Equals(A, B);
end;

//=== { TJclStackF<T> } ======================================================

constructor TJclStackF<T>.Create(AEqualityCompare: TEqualityCompare<T>; ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FEqualityCompare := AEqualityCompare;
end;

procedure TJclStackF<T>.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclStackF<T> then
    TJclStackF<T>(Dest).FEqualityCompare := FEqualityCompare;
end;

function TJclStackF<T>.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclStackF<T>.Create(FEqualityCompare, FSize + 1, False);
  AssignPropertiesTo(Result);
end;

function TJclStackF<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if not Assigned(EqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityCompare(A, B);
end;

//=== { TJclStackI<T> } ======================================================

function TJclStackI<T>.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclStackI<T>.Create(FSize + 1, False);
  AssignPropertiesTo(Result);
end;

function TJclStackI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  Result := A.Equals(B);
end;

{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
