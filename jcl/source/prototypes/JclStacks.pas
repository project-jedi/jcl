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
{$I containers\JclStacks.imp}
type
(*$JPPEXPANDMACRO JCLSTACKINT(TJclIntfStack,IJclIntfStack,TJclIntfContainer,JclBase.TDynIInterfaceArray,IJclContainer\, IJclIntfEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainer; override;,,,const AInterface: IInterface,IInterface)*)

(*$JPPEXPANDMACRO JCLSTACKINT(TJclStrStack,IJclStrStack,TJclStrContainer,JclBase.TDynStringArray,IJclContainer\, IJclStrContainer\, IJclStrEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainer; override;,,,const AString: string,string)*)

(*$JPPEXPANDMACRO JCLSTACKINT(TJclStack,IJclStack,TJclContainer,JclBase.TDynObjectArray,IJclContainer\, IJclEqualityComparer\, IJclObjectOwner\,,,
    function CreateEmptyContainer: TJclAbstractContainer; override;,,; AOwnsObjects: Boolean,AObject: TObject,TObject)*)

  {$IFDEF SUPPORTS_GENERICS}

(*$JPPEXPANDMACRO JCLSTACKINT(TJclStack<T>,IJclStack<T>,TJclContainer<T>,TJclBase<T>.TDynArray,IJclContainer\, IJclEqualityComparer<T>\, IJclItemOwner<T>\,,,,,; AOwnsItems: Boolean,const AItem: T,T)*)

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

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfStack.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclIntfStack.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLSTACKIMP(TJclIntfStack,,,const AInterface: IInterface,AInterface,IInterface,nil,FreeObject)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclStrStack.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclStrStack.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLSTACKIMP(TJclStrStack,,,const AString: string,AString,string,'',FreeString)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclStack.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclStack.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLSTACKIMP(TJclStack,; AOwnsObjects: Boolean,\, AOwnsObjects,AObject: TObject,AObject,TObject,nil,FreeObject)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$IFDEF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER}
(*$JPPEXPANDMACRO JCLSTACKIMP(TJclStack<T>,; AOwnsItems: Boolean,\, AOwnsItems,const AItem: T,AItem,T,Default(T),FreeItem)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

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
