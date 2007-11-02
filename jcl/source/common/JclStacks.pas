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

  TJclIntfStack = class(TJclIntfAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclIntfEqualityComparer,
    IJclIntfStack)
  private
    FElements: JclBase.TDynIInterfaceArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
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
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;


  TJclAnsiStrStack = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer, IJclAnsiStrEqualityComparer,
    IJclAnsiStrStack)
  private
    FElements: JclBase.TDynAnsiStringArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclAnsiStrStack }
    procedure Clear;
    function Contains(const AString: AnsiString): Boolean;
    function Empty: Boolean;
    function Peek: AnsiString;
    function Pop: AnsiString;
    function Push(const AString: AnsiString): Boolean;
    function Size: Integer;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;


  TJclWideStrStack = class(TJclWideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclStrContainer, IJclWideStrContainer, IJclWideStrEqualityComparer,
    IJclWideStrStack)
  private
    FElements: JclBase.TDynWideStringArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclWideStrStack }
    procedure Clear;
    function Contains(const AString: WideString): Boolean;
    function Empty: Boolean;
    function Peek: WideString;
    function Pop: WideString;
    function Push(const AString: WideString): Boolean;
    function Size: Integer;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrStack = TJclAnsiStrStack;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrStack = TJclWideStrStack;
  {$ENDIF CONTAINER_WIDESTR}


  TJclStack = class(TJclAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclEqualityComparer, IJclObjectOwner,
    IJclStack)
  private
    FElements: JclBase.TDynObjectArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
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
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer; AOwnsObjects: Boolean);
    destructor Destroy; override;
  end;

  {$IFDEF SUPPORTS_GENERICS}


  TJclStack<T> = class(TJclAbstractContainer<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclEqualityComparer<T>, IJclItemOwner<T>,
    IJclStack<T>)
  private
    FElements: TJclBase<T>.TDynArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
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
  TJclStackE<T> = class(TJclStack<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer,
    IJclStack<T>, IJclItemOwner<T>)
  private
    FEqualityComparer: IEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; ACapacity: Integer; AOwnsItems: Boolean);

    property EqualityComparer: IEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclStackF<T> = class(TJclStack<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer,
    IJclStack<T>, IJclItemOwner<T>)
  private
    FEqualityCompare: TEqualityCompare<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(AEqualityCompare: TEqualityCompare<T>; ACapacity: Integer; AOwnsItems: Boolean);

    property EqualityCompare: TEqualityCompare<T> read FEqualityCompare write FEqualityCompare;
  end;

  // I = items can compare themselves to an other for equality
  TJclStackI<T: IEquatable<T>> = class(TJclStack<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer,
    IJclStack<T>, IJclItemOwner<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
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

procedure TJclIntfStack.AssignDataTo(Dest: TJclAbstractContainerBase);
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

function TJclIntfStack.CreateEmptyContainer: TJclAbstractContainerBase;
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


//=== { TJclAnsiStrStack } =======================================================

constructor TJclAnsiStrStack.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
end;

destructor TJclAnsiStrStack.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclAnsiStrStack.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclAnsiStrStack;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclAnsiStrStack then
  begin
    ADest := TJclAnsiStrStack(Dest);
    ADest.Clear;
    ADest.SetCapacity(FSize + 1);
    for I := 0 to FSize - 1 do
      ADest.FElements[I] := FElements[I];
    ADest.FSize := FSize;
  end;
end;

procedure TJclAnsiStrStack.Clear;
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

function TJclAnsiStrStack.Contains(const AString: AnsiString): Boolean;
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

function TJclAnsiStrStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrStack.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrStack.Empty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclAnsiStrStack.Peek: AnsiString;
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

function TJclAnsiStrStack.Pop: AnsiString;
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

function TJclAnsiStrStack.Push(const AString: AnsiString): Boolean;
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

procedure TJclAnsiStrStack.SetCapacity(Value: Integer);
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

function TJclAnsiStrStack.Size: Integer;
begin
  Result := FSize;
end;


//=== { TJclWideStrStack } =======================================================

constructor TJclWideStrStack.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
end;

destructor TJclWideStrStack.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclWideStrStack.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclWideStrStack;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclWideStrStack then
  begin
    ADest := TJclWideStrStack(Dest);
    ADest.Clear;
    ADest.SetCapacity(FSize + 1);
    for I := 0 to FSize - 1 do
      ADest.FElements[I] := FElements[I];
    ADest.FSize := FSize;
  end;
end;

procedure TJclWideStrStack.Clear;
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

function TJclWideStrStack.Contains(const AString: WideString): Boolean;
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

function TJclWideStrStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrStack.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclWideStrStack.Empty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclWideStrStack.Peek: WideString;
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

function TJclWideStrStack.Pop: WideString;
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

function TJclWideStrStack.Push(const AString: WideString): Boolean;
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

procedure TJclWideStrStack.SetCapacity(Value: Integer);
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

function TJclWideStrStack.Size: Integer;
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

procedure TJclStack.AssignDataTo(Dest: TJclAbstractContainerBase);
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

function TJclStack.CreateEmptyContainer: TJclAbstractContainerBase;
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

procedure TJclStack<T>.AssignDataTo(Dest: TJclAbstractContainerBase);
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

procedure TJclStackE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclStackE<T> then
    TJclStackE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclStackE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
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

procedure TJclStackF<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclStackF<T> then
    TJclStackF<T>(Dest).FEqualityCompare := FEqualityCompare;
end;

function TJclStackF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
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

function TJclStackI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
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
