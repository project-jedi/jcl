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
{$I containers\JclQueues.imp}
type
(*$JPPEXPANDMACRO JCLQUEUEINT(TJclIntfQueue,IJclIntfQueue,TJclIntfContainer,IInterface,JclBase.TDynIInterfaceArray, IJclContainer\, IJclIntfEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainer; override;,,,const AInterface: IInterface)*)

(*$JPPEXPANDMACRO JCLQUEUEINT(TJclStrQueue,IJclStrQueue,TJclStrContainer,string,JclBase.TDynStringArray, IJclContainer\, IJclStrContainer\, IJclStrEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainer; override;,,,const AString: string)*)

(*$JPPEXPANDMACRO JCLQUEUEINT(TJclQueue,IJclQueue,TJclContainer,TObject,JclBase.TDynObjectArray, IJclContainer\, IJclEqualityComparer\, IJclObjectOwner\,,,
    function CreateEmptyContainer: TJclAbstractContainer; override;,,; AOwnsObjects: Boolean,AObject: TObject)*)

  {$IFDEF SUPPORTS_GENERICS}

(*$JPPEXPANDMACRO JCLQUEUEINT(TJclQueue<T>,IJclQueue<T>,TJclContainer<T>,T,TJclBase<T>.TDynArray, IJclContainer\, IJclEqualityComparer<T>\, IJclItemOwner<T>\,,,,,; AOwnsItems: Boolean,const AItem: T)*)

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

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfQueue.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclIntfQueue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLQUEUEIMP(TJclIntfQueue,,,const AInterface: IInterface,AInterface,IInterface,nil,JclBase.MoveArray,FreeObject)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclStrQueue.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclStrQueue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLQUEUEIMP(TJclStrQueue,,,const AString: string,AString,string,'',JclBase.MoveArray,FreeString)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclQueue.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclQueue.Create(Size + 1, False);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLQUEUEIMP(TJclQueue,; AOwnsObjects: Boolean,\, AOwnsObjects,AObject: TObject,AObject,TObject,nil,JclBase.MoveArray,FreeObject)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$IFDEF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER}
(*$JPPEXPANDMACRO JCLQUEUEIMP(TJclQueue<T>,; AOwnsItems: Boolean,\, AOwnsItems,const AItem: T,AItem,T,Default(T),TJclBase<T>.MoveArray,FreeItem)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
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
