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
// For history see end of file

unit JclArraySet;

{$I jcl.inc}

interface

uses
  JclBase, JclAbstractContainer, JclDCL_intf, JclDCLUtil, JclArrayList;

type
  TJclIntfArraySet = class(TJclIntfArrayList, IIntfCollection, IIntfSet, IIntfCloneable)
  protected
    function Add(AObject: IInterface): Boolean;
    function AddAll(ACollection: IIntfCollection): Boolean;
    { IIntfSet }
    procedure Intersect(ACollection: IIntfCollection);
    procedure Subtract(ACollection: IIntfCollection);
    procedure Union(ACollection: IIntfCollection);
  end;

  TJclStrArraySet = class(TJclStrArrayList, IStrCollection, IStrSet, ICloneable)
  protected
    function Add(const AString: string): Boolean;
    function AddAll(ACollection: IStrCollection): Boolean;
    { IStrSet }
    procedure Intersect(ACollection: IStrCollection);
    procedure Subtract(ACollection: IStrCollection);
    procedure Union(ACollection: IStrCollection);
  end;

  TJclArraySet = class(TJclArrayList, ICollection, ISet, ICloneable)
  protected
    function Add(AObject: TObject): Boolean;
    function AddAll(ACollection: ICollection): Boolean;
    { ISet }
    procedure Intersect(ACollection: ICollection);
    procedure Subtract(ACollection: ICollection);
    procedure Union(ACollection: ICollection);
  end;

implementation

//=== { TJclIntfArraySet } ===================================================

function TJclIntfArraySet.Add(AObject: IInterface): Boolean;
begin
  Result := not Contains(AObject);
  if Result then
    inherited Add(AObject);
end;

function TJclIntfArraySet.AddAll(ACollection: IIntfCollection): Boolean;
var
  It: IIntfIterator;
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

procedure TJclIntfArraySet.Intersect(ACollection: IIntfCollection);
begin
  RetainAll(ACollection);
end;

procedure TJclIntfArraySet.Subtract(ACollection: IIntfCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclIntfArraySet.Union(ACollection: IIntfCollection);
begin
  AddAll(ACollection);
end;

//=== { TJclStrArraySet } ====================================================

function TJclStrArraySet.Add(const AString: string): Boolean;
begin
  Result := not Contains(AString);
  if Result then
    inherited Add(AString);
end;

function TJclStrArraySet.AddAll(ACollection: IStrCollection): Boolean;
var
  It: IStrIterator;
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

procedure TJclStrArraySet.Intersect(ACollection: IStrCollection);
begin
  RetainAll(ACollection);
end;

procedure TJclStrArraySet.Subtract(ACollection: IStrCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclStrArraySet.Union(ACollection: IStrCollection);
begin
  AddAll(ACollection);
end;

//=== { TJclArraySet } =======================================================

function TJclArraySet.Add(AObject: TObject): Boolean;
begin
  Result := not Contains(AObject);
  if Result then
    inherited Add(AObject);
end;

function TJclArraySet.AddAll(ACollection: ICollection): Boolean;
var
  It: IIterator;
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

procedure TJclArraySet.Intersect(ACollection: ICollection);
begin
  RetainAll(ACollection);
end;

procedure TJclArraySet.Subtract(ACollection: ICollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclArraySet.Union(ACollection: ICollection);
begin
  AddAll(ACollection);
end;

end.

