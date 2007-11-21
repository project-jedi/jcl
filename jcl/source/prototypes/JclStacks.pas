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
(*$JPPEXPANDMACRO JCLSTACKINT(TJclIntfStack,IJclIntfStack,TJclIntfAbstractContainer,JclBase.TDynIInterfaceArray, IJclIntfEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const AInterface: IInterface,IInterface)*)

(*$JPPEXPANDMACRO JCLSTACKINT(TJclAnsiStrStack,IJclAnsiStrStack,TJclAnsiStrAbstractContainer,JclBase.TDynAnsiStringArray, IJclStrContainer\, IJclAnsiStrContainer\, IJclAnsiStrEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const AString: AnsiString,AnsiString)*)

(*$JPPEXPANDMACRO JCLSTACKINT(TJclWideStrStack,IJclWideStrStack,TJclWideStrAbstractContainer,JclBase.TDynWideStringArray, IJclStrContainer\, IJclWideStrContainer\, IJclWideStrEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const AString: WideString,WideString)*)

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrStack = TJclAnsiStrStack;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrStack = TJclWideStrStack;
  {$ENDIF CONTAINER_WIDESTR}

(*$JPPEXPANDMACRO JCLSTACKINT(TJclSingleStack,IJclSingleStack,TJclSingleAbstractContainer,JclBase.TDynSingleArray, IJclSingleContainer\, IJclSingleEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const AValue: Single,Single)*)

(*$JPPEXPANDMACRO JCLSTACKINT(TJclDoubleStack,IJclDoubleStack,TJclDoubleAbstractContainer,JclBase.TDynDoubleArray, IJclDoubleContainer\, IJclDoubleEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const AValue: Double,Double)*)

(*$JPPEXPANDMACRO JCLSTACKINT(TJclExtendedStack,IJclExtendedStack,TJclExtendedAbstractContainer,JclBase.TDynExtendedArray, IJclExtendedContainer\, IJclExtendedEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const AValue: Extended,Extended)*)

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatStack = TJclExtendedStack;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatStack = TJclDoubleStack;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatStack = TJclSingleStack;
  {$ENDIF MATH_SINGLE_PRECISION}

(*$JPPEXPANDMACRO JCLSTACKINT(TJclIntegerStack,IJclIntegerStack,TJclIntegerAbstractContainer,JclBase.TDynIntegerArray, IJclIntegerEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,AValue: Integer,Integer)*)

(*$JPPEXPANDMACRO JCLSTACKINT(TJclCardinalStack,IJclCardinalStack,TJclCardinalAbstractContainer,JclBase.TDynCardinalArray, IJclCardinalEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,AValue: Cardinal,Cardinal)*)

(*$JPPEXPANDMACRO JCLSTACKINT(TJclInt64Stack,IJclInt64Stack,TJclInt64AbstractContainer,JclBase.TDynInt64Array, IJclInt64EqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const AValue: Int64,Int64)*)

  {$IFNDEF CLR}
(*$JPPEXPANDMACRO JCLSTACKINT(TJclPtrStack,IJclPtrStack,TJclPtrAbstractContainer,JclBase.TDynPointerArray, IJclPtrEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,APtr: Pointer,Pointer)*)
  {$ENDIF ~CLR}

(*$JPPEXPANDMACRO JCLSTACKINT(TJclStack,IJclStack,TJclAbstractContainer,JclBase.TDynObjectArray, IJclEqualityComparer\, IJclObjectOwner\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,; AOwnsObjects: Boolean,AObject: TObject,TObject)*)

  {$IFDEF SUPPORTS_GENERICS}

(*$JPPEXPANDMACRO JCLSTACKINT(TJclStack<T>,IJclStack<T>,TJclAbstractContainer<T>,TJclBase<T>.TDynArray, IJclEqualityComparer<T>\, IJclItemOwner<T>\,,,,,; AOwnsItems: Boolean,const AItem: T,T)*)

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
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(AEqualityCompare: TEqualityCompare<T>; ACapacity: Integer; AOwnsItems: Boolean);
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

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfStack.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLSTACKIMP(TJclIntfStack,,,const AInterface: IInterface,AInterface,IInterface,nil,FreeObject)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclAnsiStrStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrStack.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLSTACKIMP(TJclAnsiStrStack,,,const AString: AnsiString,AString,AnsiString,'',FreeString)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclWideStrStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrStack.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLSTACKIMP(TJclWideStrStack,,,const AString: WideString,AString,WideString,'',FreeString)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclSingleStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleStack.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLSTACKIMP(TJclSingleStack,,,const AValue: Single,AValue,Single,0.0,FreeSingle)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclDoubleStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleStack.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLSTACKIMP(TJclDoubleStack,,,const AValue: Double,AValue,Double,0.0,FreeDouble)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclExtendedStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedStack.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLSTACKIMP(TJclExtendedStack,,,const AValue: Extended,AValue,Extended,0.0,FreeExtended)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntegerStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerStack.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLSTACKIMP(TJclIntegerStack,,,AValue: Integer,AValue,Integer,0,FreeInteger)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclCardinalStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalStack.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLSTACKIMP(TJclCardinalStack,,,AValue: Cardinal,AValue,Cardinal,0,FreeCardinal)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclInt64Stack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64Stack.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLSTACKIMP(TJclInt64Stack,,,const AValue: Int64,AValue,Int64,0,FreeInt64)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$IFNDEF CLR}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclPtrStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrStack.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLSTACKIMP(TJclPtrStack,,,APtr: Pointer,APtr,Pointer,nil,FreePointer)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$ENDIF ~CLR}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclStack.CreateEmptyContainer: TJclAbstractContainerBase;
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
  if EqualityComparer <> nil then
    Result := EqualityComparer.Equals(A, B)
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclStackF<T> } ======================================================

constructor TJclStackF<T>.Create(AEqualityCompare: TEqualityCompare<T>; ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  SetEqualityCompare(AEqualityCompare);
end;

function TJclStackF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclStackF<T>.Create(FEqualityCompare, FSize + 1, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclStackI<T> } ======================================================

function TJclStackI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclStackI<T>.Create(FSize + 1, False);
  AssignPropertiesTo(Result);
end;

function TJclStackI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
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
