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
  TJclIntfStack = class(TJclAbstractContainer, IJclIntfStack,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FElements: TDynIInterfaceArray;
    FCount: Integer;
    FCapacity: Integer;
  protected
    { IJclPackable }
    procedure Pack;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    { IJclGrowable }
    procedure Grow; overload; virtual;
    procedure Grow(Increment: Integer); overload;
    procedure Grow(Num, Denom: Integer); overload;
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclIntfStack }
    procedure Clear;
    function Contains(const AInterface: IInterface): Boolean;
    function Empty: Boolean;
    function Peek: IInterface;
    function Pop: IInterface;
    procedure Push(const AInterface: IInterface);
    function Size: Integer;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity);
    destructor Destroy; override;
  end;

  TJclStrStack = class(TJclAbstractContainer, IJclStrStack,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FElements: TDynStringArray;
    FCount: Integer;
    FCapacity: Integer;
  protected
    { IJclPackable }
    procedure Pack;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    { IJclGrowable }
    procedure Grow; overload; virtual;
    procedure Grow(Increment: Integer); overload;
    procedure Grow(Num, Denom: Integer); overload;
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclStrStack }
    procedure Clear;
    function Contains(const AString: string): Boolean;
    function Empty: Boolean;
    function Peek: string;
    function Pop: string;
    procedure Push(const AString: string);
    function Size: Integer;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity);
    destructor Destroy; override;
  end;

  TJclStack = class(TJclAbstractContainer, IJclStack,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FElements: TDynObjectArray;
    FCount: Integer;
    FCapacity: Integer;
    FOwnsObjects: Boolean;
  protected
    procedure FreeObject(var AObject: TObject);
    { IJclPackable }
    procedure Pack;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    { IJclGrowable }
    procedure Grow; overload; virtual;
    procedure Grow(Increment: Integer); overload;
    procedure Grow(Num, Denom: Integer); overload;
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclStack }
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function Empty: Boolean;
    function Peek: TObject;
    function Pop: TObject;
    procedure Push(AObject: TObject);
    function Size: Integer;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity; AOwnsObjects: Boolean = True);
    destructor Destroy; override;
    property OwnsObjects: Boolean read FOwnsObjects;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  TJclStack<T> = class(TJclAbstractContainer, IJclStack<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FElements: TJclBase<T>.TDynArray;
    FCount: Integer;
    FCapacity: Integer;
    FOwnsItems: Boolean;
  protected
    function CreateEmptyStack(ACapacity: Integer): TJclStack<T>; virtual; abstract;
    function ItemsEqual(const A, B: T): Boolean; virtual; abstract;
    procedure FreeItem(var AItem: T);
    { IJclPackable }
    procedure Pack;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    { IJclGrowable }
    procedure Grow; overload; virtual;
    procedure Grow(Increment: Integer); overload;
    procedure Grow(Num, Denom: Integer); overload;
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclStack<T> }
    procedure Clear;
    function Contains(const AItem: T): Boolean;
    function Empty: Boolean;
    function Peek: T;
    function Pop: T;
    procedure Push(const AItem: T);
    function Size: Integer;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity; AOwnsItems: Boolean = True);
    destructor Destroy; override;
    property OwnsItems: Boolean read FOwnsItems;
  end;

  // E = external helper to compare items for equality
  TJclStackE<T> = class(TJclStack<T>, IJclStack<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FEqualityComparer: IEqualityComparer<T>;
  protected
    function CreateEmptyStack(ACapacity: Integer): TJclStack<T>; override;
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
    function CreateEmptyStack(ACapacity: Integer): TJclStack<T>; override;
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
    function CreateEmptyStack(ACapacity: Integer): TJclStack<T>; override;
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

//=== { TJclIntfStack } ======================================================

constructor TJclIntfStack.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  FCount := 0;
  SetCapacity(ACapacity);
end;

destructor TJclIntfStack.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclIntfStack.Clear;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCount - 1 do
      FElements[I] := nil;
    FCount := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfStack.Clone: TObject;
var
  NewStack: TJclIntfStack;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewStack := TJclIntfStack.Create(FCount);
    for I := 0 to FCount - 1 do
      NewStack.FElements[I] := FElements[I];
    NewStack.FCount := FCount;
    Result := NewStack;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
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
    for I := 0 to FCount - 1 do
      if FElements[I] = AInterface then
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

function TJclIntfStack.Empty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclIntfStack.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

procedure TJclIntfStack.Grow(Num, Denom: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(FCapacity * Num div Denom);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfStack.Grow(Increment: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(FCapacity + Increment);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfStack.Grow;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCapacity > 64 then
      SetCapacity(FCapacity + FCapacity div 4)
    else
      SetCapacity(fCapacity + 16);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfStack.IntfClone: IInterface;
var
  NewStack: TJclIntfStack;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewStack := TJclIntfStack.Create(FCount);
    for I := 0 to FCount - 1 do
      NewStack.FElements[I] := FElements[I];
    NewStack.FCount := FCount;
    Result := NewStack;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfStack.Pack;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(FCount);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfStack.Peek: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCount > 0 then
      Result := FElements[FCount - 1]
    else
      Result := nil;
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
    if FCount > 0 then
    begin
      Dec(FCount);
      Result := FElements[FCount];
      FElements[FCount] := nil;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfStack.Push(const AInterface: IInterface);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCount = FCapacity then
      Grow;
    FElements[FCount] := AInterface;
    Inc(FCount);
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
    if Value < FCount then
      raise EJclOutOfBoundsError.Create;
    FCapacity := Value;
    SetLength(FElements, Value);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfStack.Size: Integer;
begin
  Result := FCount;
end;

//=== { TJclStrStack } =======================================================

constructor TJclStrStack.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  FCount := 0;
  SetCapacity(ACapacity);
end;

destructor TJclStrStack.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclStrStack.Clear;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCount - 1 do
      FElements[I] := '';
    FCount := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStack.Clone: TObject;
var
  NewStack: TJclStrStack;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewStack := TJclStrStack.Create(FCount);
    for I := 0 to FCount - 1 do
      NewStack.FElements[I] := FElements[I];
    NewStack.FCount := FCount;
    Result := NewStack;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
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
    for I := 0 to FCount - 1 do
      if FElements[I] = AString then
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

function TJclStrStack.Empty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclStrStack.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

procedure TJclStrStack.Grow(Num, Denom: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(FCapacity * Num div Denom);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrStack.Grow(Increment: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(FCapacity + Increment);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrStack.Grow;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCapacity > 64 then
      SetCapacity(FCapacity + FCapacity div 4)
    else
      SetCapacity(fCapacity + 16);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStack.IntfClone: IInterface;
var
  NewStack: TJclStrStack;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewStack := TJclStrStack.Create(FCount);
    for I := 0 to FCount - 1 do
      NewStack.FElements[I] := FElements[I];
    NewStack.FCount := FCount;
    Result := NewStack;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrStack.Pack;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(FCount);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStack.Peek: string;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCount > 0 then
      Result := FElements[FCount - 1]
    else
      Result := '';
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
    if FCount > 0 then
    begin
      Dec(FCount);
      Result := FElements[FCount];
      FElements[FCount] := '';
    end
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrStack.Push(const AString: string);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCount = FCapacity then
      Grow;
    FElements[FCount] := AString;
    Inc(FCount);
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
    if Value < FCount then
      raise EJclOutOfBoundsError.Create;
    FCapacity := Value;
    SetLength(FElements, Value);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStack.Size: Integer;
begin
  Result := FCount;
end;
//=== { TJclStack } ==========================================================

constructor TJclStack.Create(ACapacity: Integer; AOwnsObjects: Boolean);
begin
  inherited Create(nil);
  FCount := 0;
  FOwnsObjects := AOwnsObjects;
  SetCapacity(ACapacity);
end;

destructor TJclStack.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclStack.Clear;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCount - 1 do
      FreeObject(FElements[I]);
    FCount := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack.Clone: TObject;
var
  NewStack: TJclStack;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewStack := TJclStack.Create(FCount, False);
    for I := 0 to FCount - 1 do
      NewStack.FElements[I] := FElements[I];
    NewStack.FCount := FCount;
    Result := NewStack;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
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
    for I := 0 to FCount - 1 do
      if FElements[I] = AObject then
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

function TJclStack.Empty: Boolean;
begin
  Result := FCount = 0;
end;

procedure TJclStack.FreeObject(var AObject: TObject);
begin
  if FOwnsObjects then
    FreeAndNil(AObject)
  else
    AObject := nil;
end;

function TJclStack.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

procedure TJclStack.Grow(Num, Denom: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(FCapacity * Num div Denom);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStack.Grow(Increment: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(FCapacity + Increment);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStack.Grow;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCapacity > 64 then
      SetCapacity(FCapacity + FCapacity div 4)
    else
      SetCapacity(fCapacity + 16);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack.IntfClone: IInterface;
var
  NewStack: TJclStack;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewStack := TJclStack.Create(FCount, False);
    for I := 0 to FCount - 1 do
      NewStack.FElements[I] := FElements[I];
    NewStack.FCount := FCount;
    Result := NewStack;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStack.Pack;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(FCount);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack.Peek: TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCount > 0 then
      Result := FElements[FCount - 1]
    else
      Result := nil;
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
    if FCount > 0 then
    begin
      Dec(FCount);
      Result := FElements[FCount];
      FElements[FCount] := nil;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStack.Push(AObject: TObject);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCount = FCapacity then
      Grow;
    FElements[FCount] := AObject;
    Inc(FCount);
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
    if Value < FCount then
      raise EJclOutOfBoundsError.Create;
    FCapacity := Value;
    SetLength(FElements, Value);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack.Size: Integer;
begin
  Result := FCount;
end;

{$IFDEF SUPPORTS_GENERICS}

//=== { TJclStack<T> } =======================================================

constructor TJclStack<T>.Create(ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(nil);
  FCount := 0;
  FOwnsItems := AOwnsItems;
  SetCapacity(ACapacity);
end;

destructor TJclStack<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclStack<T>.Clear;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCount - 1 do
      FreeItem(FElements[I]);
    FCount := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack<T>.Clone: TObject;
var
  NewStack: TJclStack<T>;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewStack := CreateEmptyStack(FCount);
    for I := 0 to FCount - 1 do
      NewStack.FElements[I] := FElements[I];
    NewStack.FCount := FCount;
    Result := NewStack;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
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
    for I := 0 to FCount - 1 do
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
  Result := FCount = 0;
end;

procedure TJclStack<T>.FreeItem(var AItem: T);
begin
  if FOwnsItems then
    FreeAndNil(AItem)
  else
    AItem := Default(T);
end;

function TJclStack<T>.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

procedure TJclStack<T>.Grow(Num, Denom: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(FCapacity * Num div Denom);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStack<T>.Grow(Increment: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(FCapacity + Increment);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStack<T>.Grow;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCapacity > 64 then
      SetCapacity(FCapacity + FCapacity div 4)
    else
      SetCapacity(fCapacity + 16);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack<T>.IntfClone: IInterface;
var
  NewStack: TJclStack<T>;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewStack := CreateEmptyStack(FCount);
    for I := 0 to FCount - 1 do
      NewStack.FElements[I] := FElements[I];
    NewStack.FCount := FCount;
    Result := NewStack;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStack<T>.Pack;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(FCount);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack<T>.Peek: T;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCount > 0 then
      Result := FElements[FCount - 1]
    else
      Result := Default(T);
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
    if FCount > 0 then
    begin
      Dec(FCount);
      Result := FElements[FCount];
      FElements[FCount] := Default(T);
    end
    else
      Result := Default(T);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStack<T>.Push(const AItem: T);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCount = FCapacity then
      Grow;
    FElements[FCount] := AItem;
    Inc(FCount);
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
    if Value < FCount then
      raise EJclOutOfBoundsError.Create;
    FCapacity := Value;
    SetLength(FElements, Value);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack<T>.Size: Integer;
begin
  Result := FCount;
end;

//=== { TJclStackE<T> } ======================================================

constructor TJclStackE<T>.Create(const AEqualityComparer: IEqualityComparer<T>; ACapacity: Integer;
  AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

function TJclStackE<T>.CreateEmptyStack(ACapacity: Integer): TJclStack<T>;
begin
  Result := TJclStackE<T>.Create(FEqualityComparer, ACapacity, False);
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

function TJclStackF<T>.CreateEmptyStack(ACapacity: Integer): TJclStack<T>;
begin
  Result := TJclStackF<T>.Create(FEqualityCompare, ACapacity, False);
end;

function TJclStackF<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if not Assigned(EqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityCompare(A, B);
end;

//=== { TJclStackI<T> } ======================================================

function TJclStackI<T>.CreateEmptyStack(ACapacity: Integer): TJclStack<T>;
begin
  Result := TJclStackI<T>.Create(ACapacity, False);
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
