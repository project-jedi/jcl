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
  Generics.Collections,
  JclAlgorithms,
  {$ENDIF SUPPORTS_GENERICS}
  JclBase, JclAbstractContainers, JclContainerIntf, JclSynch;
{$I containers\JclContainerCommon.imp}
{$I containers\JclStacks.imp}
{$I containers\JclStacks.int}
type
(*$JPPEXPANDMACRO JCLSTACKINT(TJclIntfStack,IJclIntfStack,TJclIntfAbstractContainer,TDynIInterfaceArray, IJclIntfEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AInterface,IInterface)*)

(*$JPPEXPANDMACRO JCLSTACKINT(TJclAnsiStrStack,IJclAnsiStrStack,TJclAnsiStrAbstractContainer,TDynAnsiStringArray, IJclStrContainer\, IJclAnsiStrContainer\, IJclAnsiStrEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AString,AnsiString)*)

(*$JPPEXPANDMACRO JCLSTACKINT(TJclWideStrStack,IJclWideStrStack,TJclWideStrAbstractContainer,TDynWideStringArray, IJclStrContainer\, IJclWideStrContainer\, IJclWideStrEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AString,WideString)*)

{$IFDEF SUPPORTS_UNICODE_STRING}
(*$JPPEXPANDMACRO JCLSTACKINT(TJclUnicodeStrStack,IJclUnicodeStrStack,TJclUnicodeStrAbstractContainer,TDynUnicodeStringArray, IJclStrContainer\, IJclUnicodeStrContainer\, IJclUnicodeStrEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AString,UnicodeString)*)
{$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrStack = TJclAnsiStrStack;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrStack = TJclWideStrStack;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrStack = TJclUnicodeStrStack;
  {$ENDIF CONTAINER_UNICODESTR}

(*$JPPEXPANDMACRO JCLSTACKINT(TJclSingleStack,IJclSingleStack,TJclSingleAbstractContainer,TDynSingleArray, IJclSingleContainer\, IJclSingleEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AValue,Single)*)

(*$JPPEXPANDMACRO JCLSTACKINT(TJclDoubleStack,IJclDoubleStack,TJclDoubleAbstractContainer,TDynDoubleArray, IJclDoubleContainer\, IJclDoubleEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AValue,Double)*)

(*$JPPEXPANDMACRO JCLSTACKINT(TJclExtendedStack,IJclExtendedStack,TJclExtendedAbstractContainer,TDynExtendedArray, IJclExtendedContainer\, IJclExtendedEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AValue,Extended)*)

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatStack = TJclExtendedStack;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatStack = TJclDoubleStack;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatStack = TJclSingleStack;
  {$ENDIF MATH_SINGLE_PRECISION}

(*$JPPEXPANDMACRO JCLSTACKINT(TJclIntegerStack,IJclIntegerStack,TJclIntegerAbstractContainer,TDynIntegerArray, IJclIntegerEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,AValue,Integer)*)

(*$JPPEXPANDMACRO JCLSTACKINT(TJclCardinalStack,IJclCardinalStack,TJclCardinalAbstractContainer,TDynCardinalArray, IJclCardinalEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,AValue,Cardinal)*)

(*$JPPEXPANDMACRO JCLSTACKINT(TJclInt64Stack,IJclInt64Stack,TJclInt64AbstractContainer,TDynInt64Array, IJclInt64EqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AValue,Int64)*)

(*$JPPEXPANDMACRO JCLSTACKINT(TJclPtrStack,IJclPtrStack,TJclPtrAbstractContainer,TDynPointerArray, IJclPtrEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,APtr,Pointer)*)

(*$JPPEXPANDMACRO JCLSTACKINT(TJclStack,IJclStack,TJclAbstractContainer,TDynObjectArray, IJclEqualityComparer\, IJclObjectOwner\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,; AOwnsObjects: Boolean,,AObject,TObject)*)

  {$IFDEF SUPPORTS_GENERICS}

(*$JPPEXPANDMACRO JCLSTACKINT(TJclStack<T>,IJclStack<T>,TJclAbstractContainer<T>,TDynArray, IJclEqualityComparer<T>\, IJclItemOwner<T>\,,
  protected
    type
      TDynArray = array of T;,; AOwnsItems: Boolean,const ,AItem,T)*)

  // E = external helper to compare items for equality
  TJclStackE<T> = class(TJclStack<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer,
    IJclStack<T>, IJclItemOwner<T>)
  private
    FEqualityComparer: IEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; ACapacity: Integer; AOwnsItems: Boolean);
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
    property EqualityComparer: IEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclStackF<T> = class(TJclStack<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer,
    IJclStack<T>, IJclItemOwner<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(AEqualityCompare: TEqualityCompare<T>; ACapacity: Integer; AOwnsItems: Boolean);
  end;

  // I = items can compare themselves to an other for equality
  TJclStackI<T: IEquatable<T>> = class(TJclStack<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer,
    IJclStack<T>, IJclItemOwner<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
  end;
  {$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils;

(*$JPPEXPANDMACRO JCLSTACKIMP(TJclIntfStack,,,const ,AInterface,IInterface,nil,FreeObject)*)

function TJclIntfStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfStack.Create(FSize);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLSTACKIMP(TJclAnsiStrStack,,,const ,AString,AnsiString,'',FreeString)*)

function TJclAnsiStrStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrStack.Create(FSize);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLSTACKIMP(TJclWideStrStack,,,const ,AString,WideString,'',FreeString)*)

function TJclWideStrStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrStack.Create(FSize);
  AssignPropertiesTo(Result);
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
(*$JPPEXPANDMACRO JCLSTACKIMP(TJclUnicodeStrStack,,,const ,AString,UnicodeString,'',FreeString)*)

function TJclUnicodeStrStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrStack.Create(FSize);
  AssignPropertiesTo(Result);
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

(*$JPPEXPANDMACRO JCLSTACKIMP(TJclSingleStack,,,const ,AValue,Single,0.0,FreeSingle)*)

function TJclSingleStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleStack.Create(FSize);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLSTACKIMP(TJclDoubleStack,,,const ,AValue,Double,0.0,FreeDouble)*)

function TJclDoubleStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleStack.Create(FSize);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLSTACKIMP(TJclExtendedStack,,,const ,AValue,Extended,0.0,FreeExtended)*)

function TJclExtendedStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedStack.Create(FSize);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLSTACKIMP(TJclIntegerStack,,,,AValue,Integer,0,FreeInteger)*)

function TJclIntegerStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerStack.Create(FSize);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLSTACKIMP(TJclCardinalStack,,,,AValue,Cardinal,0,FreeCardinal)*)

function TJclCardinalStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalStack.Create(FSize);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLSTACKIMP(TJclInt64Stack,,,const ,AValue,Int64,0,FreeInt64)*)

function TJclInt64Stack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64Stack.Create(FSize);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLSTACKIMP(TJclPtrStack,,,,APtr,Pointer,nil,FreePointer)*)

function TJclPtrStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrStack.Create(FSize);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLSTACKIMP(TJclStack,; AOwnsObjects: Boolean,AOwnsObjects,,AObject,TObject,nil,FreeObject)*)

function TJclStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclStack.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

{$IFDEF SUPPORTS_GENERICS}

(*$JPPEXPANDMACRO JCLSTACKIMP(TJclStack<T>,; AOwnsItems: Boolean,AOwnsItems,const ,AItem,T,Default(T),FreeItem)*)

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
