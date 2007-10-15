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

  TJclIntfQueue = class(TJclIntfContainer, IJclIntfQueue, IJclContainer, IJclIntfEqualityComparer,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FElements: JclBase.TDynIInterfaceArray;
    FHead: Integer;
    FTail: Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainer); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclIntfQueue }
    procedure Clear;
    function Contains(const AInterface: IInterface): Boolean;
    function Dequeue: IInterface;
    function Empty: Boolean;
    function Enqueue(const AInterface: IInterface): Boolean;
    function Peek: IInterface;
    function Size: Integer;
    function CreateEmptyContainer: TJclAbstractContainer; override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;


  TJclStrQueue = class(TJclStrContainer, IJclStrQueue, IJclContainer, IJclStrContainer, IJclStrEqualityComparer,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FElements: JclBase.TDynStringArray;
    FHead: Integer;
    FTail: Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainer); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclStrQueue }
    procedure Clear;
    function Contains(const AString: string): Boolean;
    function Dequeue: string;
    function Empty: Boolean;
    function Enqueue(const AString: string): Boolean;
    function Peek: string;
    function Size: Integer;
    function CreateEmptyContainer: TJclAbstractContainer; override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;


  TJclQueue = class(TJclContainer, IJclQueue, IJclContainer, IJclEqualityComparer, IJclObjectOwner,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FElements: JclBase.TDynObjectArray;
    FHead: Integer;
    FTail: Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainer); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclQueue }
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function Dequeue: TObject;
    function Empty: Boolean;
    function Enqueue(AObject: TObject): Boolean;
    function Peek: TObject;
    function Size: Integer;
    function CreateEmptyContainer: TJclAbstractContainer; override;
  public
    constructor Create(ACapacity: Integer; AOwnsObjects: Boolean);
    destructor Destroy; override;
  end;

  {$IFDEF SUPPORTS_GENERICS}


  TJclQueue<T> = class(TJclContainer<T>, IJclQueue<T>, IJclContainer, IJclEqualityComparer<T>, IJclItemOwner<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FElements: TJclBase<T>.TDynArray;
    FHead: Integer;
    FTail: Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainer); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclQueue<T> }
    procedure Clear;
    function Contains(const AItem: T): Boolean;
    function Dequeue: T;
    function Empty: Boolean;
    function Enqueue(const AItem: T): Boolean;
    function Peek: T;
    function Size: Integer;
  public
    constructor Create(ACapacity: Integer; AOwnsItems: Boolean);
    destructor Destroy; override;
  end;

  // E = external helper to compare items for equality (GetHashCode is not used)
  TJclQueueE<T> = class(TJclQueue<T>, IJclQueue<T>,
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

  // F = function to compare items for equality
  TJclQueueF<T> = class(TJclQueue<T>, IJclQueue<T>,
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

  // I = items can compare themselves to an other
  TJclQueueI<T: IEquatable<T>> = class(TJclQueue<T>, IJclQueue<T>,
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


//=== { TJclIntfQueue } =======================================================

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

procedure TJclIntfQueue.AssignDataTo(Dest: TJclAbstractContainer);
var
  ADest: TJclIntfQueue;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfQueue then
  begin
    ADest := TJclIntfQueue(Dest);
    ADest.Clear;
    ADest.SetCapacity(Size + 1);
    I := FHead;
    while I <> FTail do
    begin
      ADest.Enqueue(FElements[I]);
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  end;
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
      if ItemsEqual(FElements[I], AInterface) then
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

function TJclIntfQueue.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclIntfQueue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;

function TJclIntfQueue.Dequeue: IInterface;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FTail <> FHead then
    begin
      Result := FElements[FHead];
      FElements[FHead] := nil;
      Inc(FHead);
      if FHead = FCapacity then
        FHead := 0;
      AutoPack;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
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

function TJclIntfQueue.Enqueue(const AInterface: IInterface): Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FTail = (FHead - 1)) or (FTail = (FHead + FCapacity - 1)) then
      AutoGrow;
    Result := (FTail <> (FHead - 1)) and (FTail <> (FHead + FCapacity - 1));
    if Result then
    begin
      FElements[FTail] := AInterface;
      Inc(FTail);
      if FTail = FCapacity then
        FTail := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
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
    if FTail <> FHead then
      Result := FElements[FHead]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
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
      JclBase.MoveArray(FElements, FHead, NewHead, FCapacity - FHead);
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
        JclBase.MoveArray(FElements, FHead, 0, FTail - FHead);
        Dec(FTail, FHead);
        FHead := 0;
      end;
      SetLength(FElements, Value);
    end;
    inherited SetCapacity(Value);
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

procedure TJclStrQueue.AssignDataTo(Dest: TJclAbstractContainer);
var
  ADest: TJclStrQueue;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclStrQueue then
  begin
    ADest := TJclStrQueue(Dest);
    ADest.Clear;
    ADest.SetCapacity(Size + 1);
    I := FHead;
    while I <> FTail do
    begin
      ADest.Enqueue(FElements[I]);
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  end;
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
       FreeString(FElements[I]);
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
      if ItemsEqual(FElements[I], AString) then
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

function TJclStrQueue.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclStrQueue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;

function TJclStrQueue.Dequeue: string;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FTail <> FHead then
    begin
      Result := FElements[FHead];
      FElements[FHead] := '';
      Inc(FHead);
      if FHead = FCapacity then
        FHead := 0;
      AutoPack;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
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

function TJclStrQueue.Enqueue(const AString: string): Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FTail = (FHead - 1)) or (FTail = (FHead + FCapacity - 1)) then
      AutoGrow;
    Result := (FTail <> (FHead - 1)) and (FTail <> (FHead + FCapacity - 1));
    if Result then
    begin
      FElements[FTail] := AString;
      Inc(FTail);
      if FTail = FCapacity then
        FTail := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
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
    if FTail <> FHead then
      Result := FElements[FHead]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
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
      JclBase.MoveArray(FElements, FHead, NewHead, FCapacity - FHead);
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
        JclBase.MoveArray(FElements, FHead, 0, FTail - FHead);
        Dec(FTail, FHead);
        FHead := 0;
      end;
      SetLength(FElements, Value);
    end;
    inherited SetCapacity(Value);
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


//=== { TJclQueue } =======================================================

constructor TJclQueue.Create(ACapacity: Integer; AOwnsObjects: Boolean);
begin
  inherited Create(nil, AOwnsObjects);
  FHead := 0;
  FTail := 0;
  SetCapacity(ACapacity);
end;

destructor TJclQueue.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclQueue.AssignDataTo(Dest: TJclAbstractContainer);
var
  ADest: TJclQueue;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclQueue then
  begin
    ADest := TJclQueue(Dest);
    ADest.Clear;
    ADest.SetCapacity(Size + 1);
    I := FHead;
    while I <> FTail do
    begin
      ADest.Enqueue(FElements[I]);
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  end;
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
      if ItemsEqual(FElements[I], AObject) then
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

function TJclQueue.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclQueue.Create(Size + 1, False);
  AssignPropertiesTo(Result);
end;

function TJclQueue.Dequeue: TObject;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FTail <> FHead then
    begin
      Result := FElements[FHead];
      FElements[FHead] := nil;
      Inc(FHead);
      if FHead = FCapacity then
        FHead := 0;
      AutoPack;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
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

function TJclQueue.Enqueue(AObject: TObject): Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FTail = (FHead - 1)) or (FTail = (FHead + FCapacity - 1)) then
      AutoGrow;
    Result := (FTail <> (FHead - 1)) and (FTail <> (FHead + FCapacity - 1));
    if Result then
    begin
      FElements[FTail] := AObject;
      Inc(FTail);
      if FTail = FCapacity then
        FTail := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
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
    if FTail <> FHead then
      Result := FElements[FHead]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
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
      JclBase.MoveArray(FElements, FHead, NewHead, FCapacity - FHead);
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
        JclBase.MoveArray(FElements, FHead, 0, FTail - FHead);
        Dec(FTail, FHead);
        FHead := 0;
      end;
      SetLength(FElements, Value);
    end;
    inherited SetCapacity(Value);
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
  inherited Create(nil, AOwnsItems);
  FHead := 0;
  FTail := 0;
  SetCapacity(ACapacity);
end;

destructor TJclQueue<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclQueue<T>.AssignDataTo(Dest: TJclAbstractContainer);
var
  ADest: TJclQueue<T>;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclQueue<T> then
  begin
    ADest := TJclQueue<T>(Dest);
    ADest.Clear;
    ADest.SetCapacity(Size + 1);
    I := FHead;
    while I <> FTail do
    begin
      ADest.Enqueue(FElements[I]);
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  end;
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
    if FTail <> FHead then
    begin
      Result := FElements[FHead];
      FElements[FHead] := Default(T);
      Inc(FHead);
      if FHead = FCapacity then
        FHead := 0;
      AutoPack;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
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

function TJclQueue<T>.Enqueue(const AItem: T): Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FTail = (FHead - 1)) or (FTail = (FHead + FCapacity - 1)) then
      AutoGrow;
    Result := (FTail <> (FHead - 1)) and (FTail <> (FHead + FCapacity - 1));
    if Result then
    begin
      FElements[FTail] := AItem;
      Inc(FTail);
      if FTail = FCapacity then
        FTail := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
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
    if FTail <> FHead then
      Result := FElements[FHead]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
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
    inherited SetCapacity(Value);
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

procedure TJclQueueE<T>.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclQueueE<T> then
    TJclQueueE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclQueueE<T>.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclQueueE<T>.Create(EqualityComparer, Size + 1, False);
  AssignPropertiesTo(Result);
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

procedure TJclQueueF<T>.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclQueueF<T> then
    TJclQueueF<T>(Dest).FEqualityCompare := FEqualityCompare;
end;

function TJclQueueF<T>.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclQueueF<T>.Create(EqualityCompare, Size + 1, False);
  AssignPropertiesTo(Result);
end;

function TJclQueueF<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if not Assigned(EqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityCompare(A, B);
end;

//=== { TJclQueueI<T> } ======================================================

function TJclQueueI<T>.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclQueueI<T>.Create(Size + 1, False);
  AssignPropertiesTo(Result);
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
