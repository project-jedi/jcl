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
{ The Original Code is Queue.pas.                                                                  }
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

unit JclQueues;

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
  TJclIntfQueue = class(TJclAbstractContainer, IJclIntfQueue,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FCapacity: Integer;
    FElements: TDynIInterfaceArray;
    FHead: Integer;
    FTail: Integer;
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
    { IJclIntfQueue }
    procedure Clear;
    function Contains(const AInterface: IInterface): Boolean;
    function Dequeue: IInterface;
    function Empty: Boolean;
    procedure Enqueue(const AInterface: IInterface);
    function Peek: IInterface;
    function Size: Integer;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity);
    destructor Destroy; override;
  end;

  TJclStrQueue = class(TJclAbstractContainer, IJclStrQueue,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FCapacity: Integer;
    FElements: TDynStringArray;
    FHead: Integer;
    FTail: Integer;
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
    { IJclStrQueue }
    procedure Clear;
    function Contains(const AString: string): Boolean;
    function Dequeue: string;
    function Empty: Boolean;
    procedure Enqueue(const AString: string);
    function Peek: string;
    function Size: Integer;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity);
    destructor Destroy; override;
  end;

  TJclQueue = class(TJclAbstractContainer, IJclQueue,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FCapacity: Integer;
    FElements: TDynObjectArray;
    FHead: Integer;
    FTail: Integer;
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
    { IJclQueue }
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function Dequeue: TObject;
    function Empty: Boolean;
    procedure Enqueue(AObject: TObject);
    function Peek: TObject;
    function Size: Integer;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity; AOwnsObjects: Boolean = True);
    destructor Destroy; override;
    property OwnsObjects: Boolean read FOwnsObjects;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  TJclQueue<T> = class(TJclAbstractContainer, IJclQueue<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FCapacity: Integer;
    FElements: TJclBase<T>.TDynArray;
    FHead: Integer;
    FTail: Integer;
    FOwnsItems: Boolean;
  protected
    function CreateEmptyQueue(ACapacity: Integer): TJclQueue<T>; virtual; abstract;
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
    { IJclQueue<T> }
    procedure Clear;
    function Contains(const AItem: T): Boolean;
    function Dequeue: T;
    function Empty: Boolean;
    procedure Enqueue(const AItem: T);
    function Peek: T;
    function Size: Integer;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity; AOwnsItems: Boolean = True);
    destructor Destroy; override;
    property OwnsItems: Boolean read FOwnsItems;
  end;

  // E = external helper to compare items for equality (GetHashCode is not used)
  TJclQueueE<T> = class(TJclQueue<T>, IJclQueue<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FEqualityComparer: IEqualityComparer<T>;
  protected
    function CreateEmptyQueue(ACapacity: Integer): TJclQueue<T>; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityComparer: IEqualityComparer<T>;
      ACapacity: Integer = DefaultContainerCapacity; AOwnsItems: Boolean = True);

    property EqualityComparer: IEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = function to compare items for equality
  TJclQueueF<T> = class(TJclQueue<T>, IJclQueue<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FEqualityCompare: TEqualityCompare<T>;
  protected
    function CreateEmptyQueue(ACapacity: Integer): TJclQueue<T>; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(AEqualityCompare: TEqualityCompare<T>;
      ACapacity: Integer = DefaultContainerCapacity; AOwnsItems: Boolean = True);

    property EqualityCompare: TEqualityCompare<T> read FEqualityCompare write FEqualityCompare;
  end;

  // I = items can compare themselves to an other
  TJclQueueI<T: IEquatable<T>> = class(TJclQueue<T>, IJclQueue<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  protected
    function CreateEmptyQueue(ACapacity: Integer): TJclQueue<T>; override;
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

//=== { TJclIntfQueue } ======================================================

constructor TJclIntfQueue.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  FHead := 0;
  FTail := 0;
  SetCapacity(ACapacity);
end;

destructor TJclIntfQueue.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclIntfQueue.Clear;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
     I := FHead;
     while I <> FTail do
     begin
       FElements[I] := nil;
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     FHead := 0;
     FTail := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfQueue.Clone: TObject;
var
  NewQueue: TJclIntfQueue;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
     NewQueue := TJclIntfQueue.Create(Size + 1);
     I := FHead;
     while I <> FTail do
     begin
       NewQueue.Enqueue(FElements[I]);
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     Result := NewQueue;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfQueue.Contains(const AInterface: IInterface): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    I := FHead;
    while I <> FTail do
    begin
      if FElements[I] = AInterface then
      begin
        Result := True;
        Break;
      end;
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfQueue.Dequeue: IInterface;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FTail = FHead then
      Exit;
    Result := FElements[FHead];
    FElements[FHead] := nil;
    Inc(FHead);
    if FHead = FCapacity then
      FHead := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfQueue.Empty: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := FTail = FHead;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfQueue.Enqueue(const AInterface: IInterface);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FTail = (FHead - 1)) or (FTail = (FHead + FCapacity - 1)) then
      Grow;
    FElements[FTail] := AInterface;
    Inc(FTail);
    if FTail = FCapacity then
      FTail := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfQueue.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

procedure TJclIntfQueue.Grow(Num, Denom: Integer);
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

procedure TJclIntfQueue.Grow;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCapacity >= 64 then
      SetCapacity(FCapacity + FCapacity div 4)
    else
      SetCapacity(FCapacity + 16);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfQueue.Grow(Increment: Integer);
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

function TJclIntfQueue.IntfClone: IInterface;
var
  NewQueue: TJclIntfQueue;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
     NewQueue := TJclIntfQueue.Create(Size + 1);
     I := FHead;
     while I <> FTail do
     begin
       NewQueue.Enqueue(FElements[I]);
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     Result := NewQueue;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfQueue.Pack;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(Size + 1);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfQueue.Peek: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FTail = FHead then
      Exit;
    Result := FElements[FHead];
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfQueue.SetCapacity(Value: Integer);
var
  NewHead: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Value < 1 then
      raise EJclIllegalQueueCapacityError.Create;
    if Value <= Size then
      raise EJclOutOfBoundsError.Create;

    if FHead > FTail then // looped
    begin
      NewHead := FHead + Value - FCapacity;
      if Value > FCapacity then
        // growing
        SetLength(FElements, Value);
      MoveArray(FElements, FHead, NewHead, FCapacity - FHead);
      if FCapacity > Value then
        // packing
        SetLength(FElements, Value);
      FHead := NewHead;
    end
    else
    begin
      // unlooped
      if Value < FCapacity then
      begin
        MoveArray(FElements, FHead, 0, FTail - FHead);
        Dec(FTail, FHead);
        FHead := 0;
      end;
      SetLength(FElements, Value);
    end;
    FCapacity := Value;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfQueue.Size: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FHead > FTail then
      Result := FCapacity - FHead + FTail  // looped
    else
      Result := FTail - FHead;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclStrQueue } =======================================================

constructor TJclStrQueue.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  FHead := 0;
  FTail := 0;
  SetCapacity(ACapacity);
end;

destructor TJclStrQueue.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclStrQueue.Clear;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
     I := FHead;
     while I <> FTail do
     begin
       FElements[I] := '';
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     FHead := 0;
     FTail := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrQueue.Clone: TObject;
var
  NewQueue: TJclStrQueue;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
     NewQueue := TJclStrQueue.Create(Size + 1);
     I := FHead;
     while I <> FTail do
     begin
       NewQueue.Enqueue(FElements[I]);
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     Result := NewQueue;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrQueue.Contains(const AString: string): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    I := FHead;
    while I <> FTail do
    begin
      if FElements[I] = AString then
      begin
        Result := True;
        Break;
      end;
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrQueue.Dequeue: string;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FTail = FHead then
      Exit;
    Result := FElements[FHead];
    FElements[FHead] := '';
    Inc(FHead);
    if FHead = FCapacity then
      FHead := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrQueue.Empty: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := FTail = FHead;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrQueue.Enqueue(const AString: string);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FTail = (FHead - 1)) or (FTail = (FHead + FCapacity - 1)) then
      Grow;
    FElements[FTail] := AString;
    Inc(FTail);
    if FTail = FCapacity then
      FTail := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrQueue.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

procedure TJclStrQueue.Grow(Num, Denom: Integer);
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

procedure TJclStrQueue.Grow;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCapacity >= 64 then
      SetCapacity(FCapacity + FCapacity div 4)
    else
      SetCapacity(FCapacity + 16);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrQueue.Grow(Increment: Integer);
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

function TJclStrQueue.IntfClone: IInterface;
var
  NewQueue: TJclStrQueue;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
     NewQueue := TJclStrQueue.Create(Size + 1);
     I := FHead;
     while I <> FTail do
     begin
       NewQueue.Enqueue(FElements[I]);
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     Result := NewQueue;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrQueue.Pack;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(Size + 1);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrQueue.Peek: string;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FTail = FHead then
      Exit;
    Result := FElements[FHead];
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrQueue.SetCapacity(Value: Integer);
var
  NewHead: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Value < 1 then
      raise EJclIllegalQueueCapacityError.Create;
    if Value <= Size then
      raise EJclOutOfBoundsError.Create;

    if FHead > FTail then // looped
    begin
      NewHead := FHead + Value - FCapacity;
      if Value > FCapacity then
        // growing
        SetLength(FElements, Value);
      MoveArray(FElements, FHead, NewHead, FCapacity - FHead);
      if FCapacity > Value then
        // packing
        SetLength(FElements, Value);
      FHead := NewHead;
    end
    else
    begin
      // unlooped
      if Value < FCapacity then
      begin
        MoveArray(FElements, FHead, 0, FTail - FHead);
        Dec(FTail, FHead);
        FHead := 0;
      end;
      SetLength(FElements, Value);
    end;
    FCapacity := Value;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrQueue.Size: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FHead > FTail then
      Result := FCapacity - FHead + FTail  // looped
    else
      Result := FTail - FHead;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclQueue } ==========================================================

constructor TJclQueue.Create(ACapacity: Integer; AOwnsObjects: Boolean);
begin
  inherited Create(nil);
  FOwnsObjects := AOwnsObjects;
  FHead := 0;
  FTail := 0;
  SetCapacity(ACapacity);
end;

destructor TJclQueue.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclQueue.Clear;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
     I := FHead;
     while I <> FTail do
     begin
       FreeObject(FElements[I]);
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     FHead := 0;
     FTail := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue.Clone: TObject;
var
  NewQueue: TJclQueue;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
     NewQueue := TJclQueue.Create(Size + 1);
     I := FHead;
     while I <> FTail do
     begin
       NewQueue.Enqueue(FElements[I]);
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     Result := NewQueue;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue.Contains(AObject: TObject): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    I := FHead;
    while I <> FTail do
    begin
      if FElements[I] = AObject then
      begin
        Result := True;
        Break;
      end;
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue.Dequeue: TObject;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FTail = FHead then
      Exit;
    Result := FElements[FHead];
    FElements[FHead] := nil;
    Inc(FHead);
    if FHead = FCapacity then
      FHead := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue.Empty: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := FTail = FHead;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclQueue.Enqueue(AObject: TObject);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FTail = (FHead - 1)) or (FTail = (FHead + FCapacity - 1)) then
      Grow;
    FElements[FTail] := AObject;
    Inc(FTail);
    if FTail = FCapacity then
      FTail := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclQueue.FreeObject(var AObject: TObject);
begin
  if FOwnsObjects then
    FreeAndNil(AObject)
  else
    AObject := nil;
end;

function TJclQueue.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

procedure TJclQueue.Grow(Num, Denom: Integer);
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

procedure TJclQueue.Grow;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCapacity >= 64 then
      SetCapacity(FCapacity + FCapacity div 4)
    else
      SetCapacity(FCapacity + 16);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclQueue.Grow(Increment: Integer);
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

function TJclQueue.IntfClone: IInterface;
var
  NewQueue: TJclQueue;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
     NewQueue := TJclQueue.Create(Size + 1);
     I := FHead;
     while I <> FTail do
     begin
       NewQueue.Enqueue(FElements[I]);
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     Result := NewQueue;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclQueue.Pack;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(Size + 1);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue.Peek: TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FTail = FHead then
      Exit;
    Result := FElements[FHead];
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclQueue.SetCapacity(Value: Integer);
var
  NewHead: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Value < 1 then
      raise EJclIllegalQueueCapacityError.Create;
    if Value <= Size then
      raise EJclOutOfBoundsError.Create;

    if FHead > FTail then // looped
    begin
      NewHead := FHead + Value - FCapacity;
      if Value > FCapacity then
        // growing
        SetLength(FElements, Value);
      MoveArray(FElements, FHead, NewHead, FCapacity - FHead);
      if FCapacity > Value then
        // packing
        SetLength(FElements, Value);
      FHead := NewHead;
    end
    else
    begin
      // unlooped
      if Value < FCapacity then
      begin
        MoveArray(FElements, FHead, 0, FTail - FHead);
        Dec(FTail, FHead);
        FHead := 0;
      end;
      SetLength(FElements, Value);
    end;
    FCapacity := Value;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue.Size: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FHead > FTail then
      Result := FCapacity - FHead + FTail  // looped
    else
      Result := FTail - FHead;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_GENERICS}

//=== { TJclQueue<T> } =======================================================

constructor TJclQueue<T>.Create(ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(nil);
  FOwnsItems := AOwnsItems;
  FHead := 0;
  FTail := 0;
  SetCapacity(ACapacity);
end;

destructor TJclQueue<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclQueue<T>.Clear;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
     I := FHead;
     while I <> FTail do
     begin
       FreeItem(FElements[I]);
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     FHead := 0;
     FTail := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue<T>.Clone: TObject;
var
  NewQueue: TJclQueue<T>;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
     NewQueue := CreateEmptyQueue(Size + 1);
     I := FHead;
     while I <> FTail do
     begin
       NewQueue.Enqueue(FElements[I]);
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     Result := NewQueue;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue<T>.Contains(const AItem: T): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    I := FHead;
    while I <> FTail do
    begin
      if ItemsEqual(FElements[I], AItem) then
      begin
        Result := True;
        Break;
      end;
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue<T>.Dequeue: T;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if FTail = FHead then
      Exit;
    Result := FElements[FHead];
    FElements[FHead] := Default(T);
    Inc(FHead);
    if FHead = FCapacity then
      FHead := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue<T>.Empty: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := FTail = FHead;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclQueue<T>.Enqueue(const AItem: T);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FTail = (FHead - 1)) or (FTail = (FHead + FCapacity - 1)) then
      Grow;
    FElements[FTail] := AItem;
    Inc(FTail);
    if FTail = FCapacity then
      FTail := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclQueue<T>.FreeItem(var AItem: T);
begin
  if FOwnsItems then
    FreeAndNil(AItem)
  else
    AItem := Default(T);
end;

function TJclQueue<T>.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

procedure TJclQueue<T>.Grow(Num, Denom: Integer);
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

procedure TJclQueue<T>.Grow;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCapacity >= 64 then
      SetCapacity(FCapacity + FCapacity div 4)
    else
      SetCapacity(FCapacity + 16);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclQueue<T>.Grow(Increment: Integer);
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

function TJclQueue<T>.IntfClone: IInterface;
var
  NewQueue: TJclQueue<T>;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
     NewQueue := CreateEmptyQueue(Size + 1);
     I := FHead;
     while I <> FTail do
     begin
       NewQueue.Enqueue(FElements[I]);
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     Result := NewQueue;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclQueue<T>.Pack;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(Size + 1);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue<T>.Peek: T;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if FTail = FHead then
      Exit;
    Result := FElements[FHead];
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclQueue<T>.SetCapacity(Value: Integer);
var
  NewHead: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Value < 1 then
      raise EJclIllegalQueueCapacityError.Create;
    if Value <= Size then
      raise EJclOutOfBoundsError.Create;

    if FHead > FTail then // looped
    begin
      NewHead := FHead + Value - FCapacity;
      if Value > FCapacity then
        // growing
        SetLength(FElements, Value);
      TJclBase<T>.MoveArray(FElements, FHead, NewHead, FCapacity - FHead);
      if FCapacity > Value then
        // packing
        SetLength(FElements, Value);
      FHead := NewHead;
    end
    else
    begin
      // unlooped
      if Value < FCapacity then
      begin
        TJclBase<T>.MoveArray(FElements, FHead, 0, FTail - FHead);
        Dec(FTail, FHead);
        FHead := 0;
      end;
      SetLength(FElements, Value);
    end;
    FCapacity := Value;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue<T>.Size: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FHead > FTail then
      Result := FCapacity - FHead + FTail  // looped
    else
      Result := FTail - FHead;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclQueueE<T> } ======================================================

constructor TJclQueueE<T>.Create(const AEqualityComparer: IEqualityComparer<T>;
  ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

function TJclQueueE<T>.CreateEmptyQueue(ACapacity: Integer): TJclQueue<T>;
begin
  Result := TJclQueueE<T>.Create(EqualityComparer, ACapacity, False);
end;

function TJclQueueE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityComparer.Equals(A, B);
end;

//=== { TJclQueueF<T> } ======================================================

constructor TJclQueueF<T>.Create(AEqualityCompare: TEqualityCompare<T>;
  ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FEqualityCompare := AEqualityCompare;
end;

function TJclQueueF<T>.CreateEmptyQueue(ACapacity: Integer): TJclQueue<T>;
begin
  Result := TJclQueueF<T>.Create(EqualityCompare, ACapacity, False);
end;

function TJclQueueF<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if not Assigned(EqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityCompare(A, B);
end;

//=== { TJclQueueI<T> } ======================================================

function TJclQueueI<T>.CreateEmptyQueue(ACapacity: Integer): TJclQueue<T>;
begin
  Result := TJclQueueI<T>.Create(ACapacity, False);
end;

function TJclQueueI<T>.ItemsEqual(const A, B: T): Boolean;
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
