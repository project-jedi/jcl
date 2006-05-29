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
{ The Original Code is ArraySet.pas.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ The Delphi Container Library                                                                     }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$

unit JclArraySets;

{$I jcl.inc}

interface

uses
  JclBase, JclAbstractContainers, JclContainerIntf, JclArrayLists;

type
  TJclIntfArraySet = class(TJclIntfArrayList, IJclIntfCollection, IJclIntfSet,
      IJclIntfCloneable)
  private
    function BinarySearch(AInterface: IInterface): Integer;
  protected
    { IJclIntfCollection }
    function Add(AInterface: IInterface): Boolean;
    function AddAll(ACollection: IJclIntfCollection): Boolean;
    function Contains(AInterface: IInterface): Boolean;
    { IJclIntfList }
    procedure Insert(Index: Integer; AInterface: IInterface); overload;
    { IJclIntfSet }
    procedure Intersect(ACollection: IJclIntfCollection);
    procedure Subtract(ACollection: IJclIntfCollection);
    procedure Union(ACollection: IJclIntfCollection);
  end;

  TJclStrArraySet = class(TJclStrArrayList, IJclStrSet, IJclCloneable)
  private
    function BinarySearch(const AString: string): Integer;
  protected
    { IJclStrCollection }
    function Add(const AString: string): Boolean; override;
    function AddAll(ACollection: IJclStrCollection): Boolean; override;
    function Contains(const AString: string): Boolean; override;
    { IJclStrList }
    procedure Insert(Index: Integer; const AString: string); overload;
    { IJclStrSet }
    procedure Intersect(ACollection: IJclStrCollection);
    procedure Subtract(ACollection: IJclStrCollection);
    procedure Union(ACollection: IJclStrCollection);
  end;

  TJclArraySet = class(TJclArrayList, IJclCollection, IJclSet, IJclCloneable)
  private
    function BinarySearch(AObject: TObject): Integer;
  protected
    { IJclCollection }
    function Add(AObject: TObject): Boolean;
    function AddAll(ACollection: IJclCollection): Boolean;
    function Contains(AObject: TObject): Boolean;
    { IJclList }
    procedure Insert(Index: Integer; AObject: TObject); overload;
    { IJclSet }
    procedure Intersect(ACollection: IJclCollection);
    procedure Subtract(ACollection: IJclCollection);
    procedure Union(ACollection: IJclCollection);
  end;

implementation

uses
  SysUtils,
  JclResources;

function ObjectCompare(Obj1, Obj2: TObject): Integer;
begin
  if Cardinal(Obj1) < Cardinal(Obj2) then
    Result := -1
  else
  if Cardinal(Obj1) > Cardinal(Obj2) then
    Result := 1
  else
    Result := 0;
end;

function InterfaceCompare(Obj1, Obj2: IInterface): Integer;
begin
  if Cardinal(Obj1) < Cardinal(Obj2) then
    Result := -1
  else
  if Cardinal(Obj1) > Cardinal(Obj2) then
    Result := 1
  else
    Result := 0;
end;

//=== { TJclIntfArraySet } ===================================================

function TJclIntfArraySet.Add(AInterface: IInterface): Boolean;
var
  Idx: Integer;
begin
  Idx := BinarySearch(AInterface);
  if Idx >= 0 then
    Result := InterfaceCompare(GetObject(Idx), AInterface) <> 0
  else
    Result := True;
  if Result then
    inherited Insert(Idx + 1, AInterface);
end;

function TJclIntfArraySet.AddAll(ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) or Result;
end;

function TJclIntfArraySet.BinarySearch(AInterface: IInterface): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  LoPos := 0;
  HiPos := Size - 1;
  CompPos := (HiPos - LoPos) div 2;
  while HiPos >= LoPos do
  begin
    Comp := InterfaceCompare(GetObject(CompPos), AInterface);
    if Comp < 0 then
      LoPos := CompPos + 1
    else
    if Comp > 0 then
      HiPos := CompPos - 1
    else
    begin
      HiPos := CompPos;
      LoPos := CompPos + 1;
    end;
    CompPos := (HiPos - LoPos) div 2 + LoPos;
  end;
  Result := HiPos;
end;

function TJclIntfArraySet.Contains(AInterface: IInterface): Boolean;
var
  Idx: Integer;
begin
  Idx := BinarySearch(AInterface);
  if Idx >= 0 then
    Result := InterfaceCompare(GetObject(Idx), AInterface) = 0
  else
    Result := False;
end;

procedure TJclIntfArraySet.Insert(Index: Integer; AInterface: IInterface);
begin
  {$IFDEF CLR}
  raise EJclOperationNotSupportedError.Create(RsEOperationNotSupported);
  {$ELSE}
  raise EJclOperationNotSupportedError.CreateRes(@RsEOperationNotSupported);
  {$ENDIF CLR}
end;

procedure TJclIntfArraySet.Intersect(ACollection: IJclIntfCollection);
begin
  RetainAll(ACollection);
end;

procedure TJclIntfArraySet.Subtract(ACollection: IJclIntfCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclIntfArraySet.Union(ACollection: IJclIntfCollection);
begin
  AddAll(ACollection);
end;

//=== { TJclStrArraySet } ====================================================

function TJclStrArraySet.Add(const AString: string): Boolean;
var
  Idx: Integer;
begin
  Idx := BinarySearch(AString);
  if Idx >= 0 then
    Result := CompareStr(GetString(Idx), AString) <> 0
  else
    Result := True;
  if Result then
    inherited Insert(Idx + 1, AString);
end;

function TJclStrArraySet.AddAll(ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) or Result;
end;

function TJclStrArraySet.BinarySearch(const AString: string): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  LoPos := 0;
  HiPos := Size - 1;
  CompPos := (HiPos - LoPos) div 2;
  while HiPos >= LoPos do
  begin
    Comp := CompareStr(GetString(CompPos), AString);
    if Comp < 0 then
      LoPos := CompPos + 1
    else
    if Comp > 0 then
      HiPos := CompPos - 1
    else
    begin
      HiPos := CompPos;
      LoPos := CompPos + 1;
    end;
    CompPos := (HiPos - LoPos) div 2 + LoPos;
  end;
  Result := HiPos;
end;

function TJclStrArraySet.Contains(const AString: string): Boolean;
var
  Idx: Integer;
begin
  Idx := BinarySearch(AString);
  if Idx >= 0 then
    Result := CompareStr(GetString(Idx), AString) = 0
  else
    Result := False;
end;

procedure TJclStrArraySet.Insert(Index: Integer; const AString: string);
begin
  {$IFDEF CLR}
  raise EJclOperationNotSupportedError.Create(RsEOperationNotSupported);
  {$ELSE}
  raise EJclOperationNotSupportedError.CreateRes(@RsEOperationNotSupported);
  {$ENDIF CLR}
end;

procedure TJclStrArraySet.Intersect(ACollection: IJclStrCollection);
begin
  RetainAll(ACollection);
end;

procedure TJclStrArraySet.Subtract(ACollection: IJclStrCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclStrArraySet.Union(ACollection: IJclStrCollection);
begin
  AddAll(ACollection);
end;

//=== { TJclArraySet } =======================================================

function TJclArraySet.Add(AObject: TObject): Boolean;
var
  Idx: Integer;
begin
  Idx := BinarySearch(AObject);
  if Idx >= 0 then
    Result := ObjectCompare(GetObject(Idx), AObject) <> 0
  else
    Result := True;
  if Result then
    inherited Insert(Idx + 1, AObject);
end;

function TJclArraySet.AddAll(ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) or Result;
end;

function TJclArraySet.BinarySearch(AObject: TObject): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  LoPos := 0;
  HiPos := Size - 1;
  CompPos := (HiPos - LoPos) div 2;
  while HiPos >= LoPos do
  begin
    Comp := ObjectCompare(GetObject(CompPos), AObject);
    if Comp < 0 then
      LoPos := CompPos + 1
    else
    if Comp > 0 then
      HiPos := CompPos - 1
    else
    begin
      HiPos := CompPos;
      LoPos := CompPos + 1;
    end;
    CompPos := (HiPos - LoPos) div 2 + LoPos;
  end;
  Result := HiPos;
end;

function TJclArraySet.Contains(AObject: TObject): Boolean;
var
  Idx: Integer;
begin
  Idx := BinarySearch(AObject);
  if Idx >= 0 then
    Result := ObjectCompare(GetObject(Idx), AObject) = 0
  else
    Result := False;
end;

procedure TJclArraySet.Insert(Index: Integer; AObject: TObject);
begin
  {$IFDEF CLR}
  raise EJclOperationNotSupportedError.Create(RsEOperationNotSupported);
  {$ELSE}
  raise EJclOperationNotSupportedError.CreateRes(@RsEOperationNotSupported);
  {$ENDIF CLR}
end;

procedure TJclArraySet.Intersect(ACollection: IJclCollection);
begin
  RetainAll(ACollection);
end;

procedure TJclArraySet.Subtract(ACollection: IJclCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclArraySet.Union(ACollection: IJclCollection);
begin
  AddAll(ACollection);
end;

end.

