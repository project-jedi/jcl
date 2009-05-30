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
  JclBase, JclAbstractContainers, JclContainerIntf, JclSynch;
{$I containers\JclContainerCommon.imp}
{$I containers\JclQueues.imp}
{$I containers\JclQueues.int}
type
(*$JPPEXPANDMACRO JCLQUEUEINT(TJclIntfQueue,IJclIntfQueue,TJclIntfAbstractContainer,TDynIInterfaceArray, IJclIntfEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AInterface,IInterface)*)

(*$JPPEXPANDMACRO JCLQUEUEINT(TJclAnsiStrQueue,IJclAnsiStrQueue,TJclAnsiStrAbstractContainer,TDynAnsiStringArray, IJclStrContainer\, IJclAnsiStrContainer\, IJclAnsiStrEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AString,AnsiString)*)

(*$JPPEXPANDMACRO JCLQUEUEINT(TJclWideStrQueue,IJclWideStrQueue,TJclWideStrAbstractContainer,TDynWideStringArray, IJclStrContainer\, IJclWideStrContainer\, IJclWideStrEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AString,WideString)*)

{$IFDEF SUPPORTS_UNICODE_STRING}
(*$JPPEXPANDMACRO JCLQUEUEINT(TJclUnicodeStrQueue,IJclUnicodeStrQueue,TJclUnicodeStrAbstractContainer,TDynUnicodeStringArray, IJclStrContainer\, IJclUnicodeStrContainer\, IJclUnicodeStrEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AString,UnicodeString)*)
{$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrQueue = TJclAnsiStrQueue;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrQueue = TJclWideStrQueue;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrQueue = TJclUnicodeStrQueue;
  {$ENDIF CONTAINER_UNICODESTR}

(*$JPPEXPANDMACRO JCLQUEUEINT(TJclSingleQueue,IJclSingleQueue,TJclSingleAbstractContainer,TDynSingleArray, IJclSingleContainer\, IJclSingleEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AValue,Single)*)

(*$JPPEXPANDMACRO JCLQUEUEINT(TJclDoubleQueue,IJclDoubleQueue,TJclDoubleAbstractContainer,TDynDoubleArray, IJclDoubleContainer\, IJclDoubleEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AValue,Double)*)

(*$JPPEXPANDMACRO JCLQUEUEINT(TJclExtendedQueue,IJclExtendedQueue,TJclExtendedAbstractContainer,TDynExtendedArray, IJclExtendedContainer\, IJclExtendedEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AValue,Extended)*)

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatQueue = TJclExtendedQueue;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatQueue = TJclDoubleQueue;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatQueue = TJclSingleQueue;
  {$ENDIF MATH_SINGLE_PRECISION}

(*$JPPEXPANDMACRO JCLQUEUEINT(TJclIntegerQueue,IJclIntegerQueue,TJclIntegerAbstractContainer,TDynIntegerArray, IJclIntegerEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,AValue,Integer)*)

(*$JPPEXPANDMACRO JCLQUEUEINT(TJclCardinalQueue,IJclCardinalQueue,TJclCardinalAbstractContainer,TDynCardinalArray, IJclCardinalEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,AValue,Cardinal)*)

(*$JPPEXPANDMACRO JCLQUEUEINT(TJclInt64Queue,IJclInt64Queue,TJclInt64AbstractContainer,TDynInt64Array, IJclInt64EqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AValue,Int64)*)

  {$IFNDEF CLR}
(*$JPPEXPANDMACRO JCLQUEUEINT(TJclPtrQueue,IJclPtrQueue,TJclPtrAbstractContainer,TDynPointerArray, IJclPtrEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,APtr,Pointer)*)
  {$ENDIF ~CLR}

(*$JPPEXPANDMACRO JCLQUEUEINT(TJclQueue,IJclQueue,TJclAbstractContainer,TDynObjectArray, IJclEqualityComparer\, IJclObjectOwner\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,; AOwnsObjects: Boolean,,AObject,TObject)*)

  {$IFDEF SUPPORTS_GENERICS}

(*$JPPEXPANDMACRO JCLQUEUEINT(TJclQueue<T>,IJclQueue<T>,TJclAbstractContainer<T>,TDynArray, IJclEqualityComparer<T>\, IJclItemOwner<T>\,,
  protected
    type
      TDynArray = array of T;
    procedure MoveArray(var List: TDynArray; FromIndex, ToIndex, Count: Integer);,; AOwnsItems: Boolean,const ,AItem,T)*)

  // E = external helper to compare items for equality (GetHashCode is not used)
  TJclQueueE<T> = class(TJclQueue<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclQueue<T>, IJclItemOwner<T>)
  private
    FEqualityComparer: IEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function ItemsEqual(const A, B: T): Boolean; override;
  public
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; ACapacity: Integer; AOwnsItems: Boolean);

    property EqualityComparer: IEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = function to compare items for equality
  TJclQueueF<T> = class(TJclQueue<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclQueue<T>, IJclItemOwner<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(AEqualityCompare: TEqualityCompare<T>; ACapacity: Integer; AOwnsItems: Boolean);
  end;

  // I = items can compare themselves to an other
  TJclQueueI<T: IEquatable<T>> = class(TJclQueue<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclQueue<T>, IJclItemOwner<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function ItemsEqual(const A, B: T): Boolean; override;
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

(*$JPPEXPANDMACRO JCLQUEUEIMP(TJclIntfQueue,,,const ,AInterface,IInterface,nil,FreeObject)*)

function TJclIntfQueue.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfQueue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLQUEUEIMP(TJclAnsiStrQueue,,,const ,AString,AnsiString,'',FreeString)*)

function TJclAnsiStrQueue.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrQueue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLQUEUEIMP(TJclWideStrQueue,,,const ,AString,WideString,'',FreeString)*)

function TJclWideStrQueue.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrQueue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
(*$JPPEXPANDMACRO JCLQUEUEIMP(TJclUnicodeStrQueue,,,const ,AString,UnicodeString,'',FreeString)*)

function TJclUnicodeStrQueue.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrQueue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

(*$JPPEXPANDMACRO JCLQUEUEIMP(TJclSingleQueue,,,const ,AValue,Single,0.0,FreeSingle)*)

function TJclSingleQueue.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleQueue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLQUEUEIMP(TJclDoubleQueue,,,const ,AValue,Double,0.0,FreeDouble)*)

function TJclDoubleQueue.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleQueue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLQUEUEIMP(TJclExtendedQueue,,,const ,AValue,Extended,0.0,FreeExtended)*)

function TJclExtendedQueue.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedQueue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLQUEUEIMP(TJclIntegerQueue,,,,AValue,Integer,0,FreeInteger)*)

function TJclIntegerQueue.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerQueue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLQUEUEIMP(TJclCardinalQueue,,,,AValue,Cardinal,0,FreeCardinal)*)

function TJclCardinalQueue.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalQueue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLQUEUEIMP(TJclInt64Queue,,,const ,AValue,Int64,0,FreeInt64)*)

function TJclInt64Queue.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64Queue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;

{$IFNDEF CLR}
(*$JPPEXPANDMACRO JCLQUEUEIMP(TJclPtrQueue,,,,APtr,Pointer,nil,FreePointer)*)

function TJclPtrQueue.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrQueue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;
{$ENDIF ~CLR}

(*$JPPEXPANDMACRO JCLQUEUEIMP(TJclQueue,; AOwnsObjects: Boolean,AOwnsObjects,,AObject,TObject,nil,FreeObject)*)

function TJclQueue.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclQueue.Create(Size + 1, False);
  AssignPropertiesTo(Result);
end;

{$IFDEF SUPPORTS_GENERICS}

(*$JPPEXPANDMACRO JCLQUEUEIMP(TJclQueue<T>,; AOwnsItems: Boolean,AOwnsItems,const ,AItem,T,Default(T),FreeItem)*)

procedure TJclQueue<T>.MoveArray(var List: TDynArray; FromIndex, ToIndex, Count: Integer);
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];

    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
        List[FromIndex + I] := Default(T)
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := Default(T);
  end
  else
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];

    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
        List[FromIndex + I] := Default(T)
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := Default(T);
  end; 
end;

//=== { TJclQueueE<T> } ======================================================

constructor TJclQueueE<T>.Create(const AEqualityComparer: IEqualityComparer<T>;
  ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

procedure TJclQueueE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclQueueE<T> then
    TJclQueueE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclQueueE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclQueueE<T>.Create(EqualityComparer, Size + 1, False);
  AssignPropertiesTo(Result);
end;

function TJclQueueE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer <> nil then
    Result := EqualityComparer.Equals(A, B)
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclQueueF<T> } ======================================================

constructor TJclQueueF<T>.Create(AEqualityCompare: TEqualityCompare<T>;
  ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  SetEqualityCompare(AEqualityCompare);
end;

function TJclQueueF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclQueueF<T>.Create(EqualityCompare, Size + 1, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclQueueI<T> } ======================================================

function TJclQueueI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclQueueI<T>.Create(Size + 1, False);
  AssignPropertiesTo(Result);
end;

function TJclQueueI<T>.ItemsEqual(const A, B: T): Boolean;
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
