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
{ The Original Code is JclSortedMaps.pas.                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet. Portions created by                }
{ Florent Ouchet are Copyright (C) Florent Ouchet <outchy att users dott sourceforge dott net      }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
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

unit JclSortedMaps;

interface

{$I jcl.inc}

uses
  Classes,
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

{$I containers\JclSortedMaps.imp}
type
(*$JPPEXPANDMACRO JCLSORTEDMAPINT(IInterface,IInterface,TJclIntfIntfEntry,TJclIntfIntfEntryArray,TJclIntfIntfSortedMap,TJclAbstractContainerBase,IJclIntfIntfMap,IJclIntfIntfSortedMap,IJclIntfSet,IJclIntfCollection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(const A\, B: IInterface): Integer;,,,,const Key: IInterface,const Value: IInterface,const ToKey: IInterface,const FromKey\, ToKey: IInterface,const FromKey: IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(AnsiString,IInterface,TJclAnsiStrIntfEntry,TJclAnsiStrIntfEntryArray,TJclAnsiStrIntfSortedMap,TJclAnsiStrAbstractContainer,IJclAnsiStrIntfMap,IJclAnsiStrIntfSortedMap,IJclAnsiStrSet,IJclIntfCollection, IJclStrContainer\, IJclAnsiStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A\, B: AnsiString): Integer;
    function ValuesCompare(const A\, B: IInterface): Integer;,,,,const Key: AnsiString,const Value: IInterface,const ToKey: AnsiString,const FromKey\, ToKey: AnsiString,const FromKey: AnsiString)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(IInterface,AnsiString,TJclIntfAnsiStrEntry,TJclIntfAnsiStrEntryArray,TJclIntfAnsiStrSortedMap,TJclAnsiStrAbstractContainer,IJclIntfAnsiStrMap,IJclIntfAnsiStrSortedMap,IJclIntfSet,IJclAnsiStrCollection, IJclStrContainer\, IJclAnsiStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: AnsiString): AnsiString;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(const A\, B: AnsiString): Integer;,,,,const Key: IInterface,const Value: AnsiString,const ToKey: IInterface,const FromKey\, ToKey: IInterface,const FromKey: IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(AnsiString,AnsiString,TJclAnsiStrAnsiStrEntry,TJclAnsiStrAnsiStrEntryArray,TJclAnsiStrAnsiStrSortedMap,TJclAnsiStrAbstractContainer,IJclAnsiStrAnsiStrMap,IJclAnsiStrAnsiStrSortedMap,IJclAnsiStrSet,IJclAnsiStrCollection, IJclStrContainer\, IJclAnsiStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function FreeValue(var Value: AnsiString): AnsiString;
    function KeysCompare(const A\, B: AnsiString): Integer;
    function ValuesCompare(const A\, B: AnsiString): Integer;,,,,const Key: AnsiString,const Value: AnsiString,const ToKey: AnsiString,const FromKey\, ToKey: AnsiString,const FromKey: AnsiString)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(WideString,IInterface,TJclWideStrIntfEntry,TJclWideStrIntfEntryArray,TJclWideStrIntfSortedMap,TJclWideStrAbstractContainer,IJclWideStrIntfMap,IJclWideStrIntfSortedMap,IJclWideStrSet,IJclIntfCollection, IJclStrContainer\, IJclWideStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A\, B: WideString): Integer;
    function ValuesCompare(const A\, B: IInterface): Integer;,,,,const Key: WideString,const Value: IInterface,const ToKey: WideString,const FromKey\, ToKey: WideString,const FromKey: WideString)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(IInterface,WideString,TJclIntfWideStrEntry,TJclIntfWideStrEntryArray,TJclIntfWideStrSortedMap,TJclWideStrAbstractContainer,IJclIntfWideStrMap,IJclIntfWideStrSortedMap,IJclIntfSet,IJclWideStrCollection, IJclStrContainer\, IJclWideStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: WideString): WideString;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(const A\, B: WideString): Integer;,,,,const Key: IInterface,const Value: WideString,const ToKey: IInterface,const FromKey\, ToKey: IInterface,const FromKey: IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(WideString,WideString,TJclWideStrWideStrEntry,TJclWideStrWideStrEntryArray,TJclWideStrWideStrSortedMap,TJclWideStrAbstractContainer,IJclWideStrWideStrMap,IJclWideStrWideStrSortedMap,IJclWideStrSet,IJclWideStrCollection, IJclStrContainer\, IJclWideStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function FreeValue(var Value: WideString): WideString;
    function KeysCompare(const A\, B: WideString): Integer;
    function ValuesCompare(const A\, B: WideString): Integer;,,,,const Key: WideString,const Value: WideString,const ToKey: WideString,const FromKey\, ToKey: WideString,const FromKey: WideString)*)

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrIntfSortedMap = TJclAnsiStrIntfSortedMap;
  TJclIntfStrSortedMap = TJclIntfAnsiStrSortedMap;
  TJclStrStrSortedMap = TJclAnsiStrAnsiStrSortedMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrIntfSortedMap = TJclWideStrIntfSortedMap;
  TJclIntfStrSortedMap = TJclIntfWideStrSortedMap;
  TJclStrStrSortedMap = TJclWideStrWideStrSortedMap;
  {$ENDIF CONTAINER_WIDESTR}

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(IInterface,TObject,TJclIntfEntry,TJclIntfEntryArray,TJclIntfSortedMap,TJclIntfAbstractContainer,IJclIntfMap,IJclIntfSortedMap,IJclIntfSet,IJclCollection, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(A\, B: TObject): Integer;,
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const Key: IInterface,Value: TObject,const ToKey: IInterface,const FromKey\, ToKey: IInterface,const FromKey: IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(AnsiString,TObject,TJclAnsiStrEntry,TJclAnsiStrEntryArray,TJclAnsiStrSortedMap,TJclAnsiStrAbstractContainer,IJclAnsiStrMap,IJclAnsiStrSortedMap,IJclAnsiStrSet,IJclCollection, IJclStrContainer\, IJclAnsiStrContainer\, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function KeysCompare(const A\, B: AnsiString): Integer;
    function ValuesCompare(A\, B: TObject): Integer;,
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const Key: AnsiString,Value: TObject,const ToKey: AnsiString,const FromKey\, ToKey: AnsiString,const FromKey: AnsiString)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(WideString,TObject,TJclWideStrEntry,TJclWideStrEntryArray,TJclWideStrSortedMap,TJclWideStrAbstractContainer,IJclWideStrMap,IJclWideStrSortedMap,IJclWideStrSet,IJclCollection, IJclStrContainer\, IJclWideStrContainer\, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function KeysCompare(const A\, B: WideString): Integer;
    function ValuesCompare(A\, B: TObject): Integer;,
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const Key: WideString,Value: TObject,const ToKey: WideString,const FromKey\, ToKey: WideString,const FromKey: WideString)*)

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrSortedMap = TJclAnsiStrSortedMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrSortedMap = TJclWideStrSortedMap;
  {$ENDIF CONTAINER_WIDESTR}

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TObject,TObject,TJclEntry,TJclEntryArray,TJclSortedMap,TJclAbstractContainerBase,IJclMap,IJclSortedMap,IJclSet,IJclCollection, IJclKeyOwner\, IJclValueOwner\,,
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;,
    { IJclKeyOwner }
    function FreeKey(var Key: TObject): TObject;
    function GetOwnsKeys: Boolean;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function KeysCompare(A\, B: TObject): Integer;
    function ValuesCompare(A\, B: TObject): Integer;,
    property OwnsKeys: Boolean read FOwnsKeys;
    property OwnsValues: Boolean read FOwnsValues;,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,Key: TObject,Value: TObject,ToKey: TObject,FromKey\, ToKey: TObject,FromKey: TObject)*)

  {$IFDEF SUPPORTS_GENERICS_DISABLED}
(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TKey,TValue,TJclEntry<TKey\,TValue>,TJclEntryArray<TKey\,TValue>,TJclSortedMap<TKey\,TValue>,TJclAbstractContainerBase,IJclMap<TKey\,TValue>,IJclSortedMap<TKey\,TValue>,IJclSet<TKey>,IJclCollection<TValue>, IJclPairOwner<TKey\,TValue>\,,
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;,
    { IJclPairOwner }
    function FreeKey(var Key: TKey): TKey;
    function FreeValue(var Value: TValue): TValue;
    function GetOwnsKeys: Boolean;
    function GetOwnsValues: Boolean;
    function KeysCompare(const A\, B: TKey): Integer; virtual; abstract;
    function ValuesCompare(const A\, B: TValue): Integer; virtual; abstract;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; virtual; abstract;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; virtual; abstract;,
    property OwnsKeys: Boolean read FOwnsKeys;
    property OwnsValues: Boolean read FOwnsValues;,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,const Key: TKey,const Value: TValue,const ToKey: TKey,const FromKey\, ToKey: TKey,const FromKey: TKey)*)

  // E = external helper to compare items
  TJclSortedMapE<TKey, TValue> = class(TJclSortedMap<TKey,TValue>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclMap<TKey,TValue>, IJclSortedMap<TKey,TValue>, IJclPairOwner<TKey,TValue>)
  private
    FKeyComparer: IComparer<TKey>;
    FValueComparer: IComparer<TValue>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function KeysCompare(const A, B: TKey): Integer; override;
    function ValuesCompare(const A, B: TValue): Integer; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AKeyComparer: IComparer<TKey>; const AValueComparer: IComparer<TValue>;
      ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);

    property KeyComparer: IComparer<TKey> read FKeyComparer write FKeyComparer;
    property ValueComparer: IComparer<TValue> read FValueComparer write FValueComparer;
  end;

  // F = Functions to compare items
  TJclSortedMapF<TKey, TValue> = class(TJclSortedMap<TKey, TValue>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclMap<TKey,TValue>, IJclSortedMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  private
    FKeyCompare: TCompare<TKey>;
    FValueCompare: TCompare<TValue>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function KeysCompare(const A, B: TKey): Integer; override;
    function ValuesCompare(const A, B: TValue): Integer; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(AKeyCompare: TCompare<TKey>; AValueCompare: TCompare<TValue>;
      ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);

    property KeyCompare: TCompare<TKey> read FKeyCompare write FKeyCompare;
    property ValueCompare: TCompare<TValue> read FValueCompare write FValueCompare;
  end;

  // I = items can compare themselves to an other
  TJclSortedMapI<TKey: IComparable<TKey>; TValue: IComparable<TValue>> = class(TJclSortedMap<TKey, TValue>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer,
    IJclMap<TKey,TValue>, IJclSortedMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  protected
    function KeysCompare(const A, B: TKey): Integer; override;
    function ValuesCompare(const A, B: TValue): Integer; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
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
  SysUtils,
  JclArrayLists,
  JclArraySets;

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfIntfSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclIntfIntfSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  Result := Integer(A) - Integer(B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclIntfIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  Result := Integer(A) - Integer(B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfIntfSortedMap,TJclIntfIntfEntry,IJclIntfIntfMap,IJclIntfIntfSortedMap,IJclIntfSet,IJclIntfIterator,IJclIntfCollection,,,,const Key: IInterface,IInterface,nil,const Value: IInterface,IInterface,nil,const ToKey: IInterface,const FromKey\, ToKey: IInterface,const FromKey: IInterface)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclAnsiStrIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclAnsiStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclAnsiStrIntfSortedMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclAnsiStrIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclAnsiStrIntfSortedMap.KeysCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclAnsiStrIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  Result := Integer(A) - Integer(B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclAnsiStrIntfSortedMap,TJclAnsiStrIntfEntry,IJclAnsiStrIntfMap,IJclAnsiStrIntfSortedMap,IJclAnsiStrSet,IJclAnsiStrIterator,IJclIntfCollection,,,,const Key: AnsiString,AnsiString,'',const Value: IInterface,IInterface,nil,const ToKey: AnsiString,const FromKey\, ToKey: AnsiString,const FromKey: AnsiString)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfAnsiStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfAnsiStrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclAnsiStrArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfAnsiStrSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfAnsiStrSortedMap.FreeValue(var Value: AnsiString): AnsiString;
begin
  Result := Value;
  Value := '';
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclIntfAnsiStrSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  Result := Integer(A) - Integer(B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclIntfAnsiStrSortedMap.ValuesCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfAnsiStrSortedMap,TJclIntfAnsiStrEntry,IJclIntfAnsiStrMap,IJclIntfAnsiStrSortedMap,IJclIntfSet,IJclIntfIterator,IJclAnsiStrCollection,,,,const Key: IInterface,IInterface,nil,const Value: AnsiString,AnsiString,'',const ToKey: IInterface,const FromKey\, ToKey: IInterface,const FromKey: IInterface)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclAnsiStrAnsiStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrAnsiStrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclAnsiStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclAnsiStrArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclAnsiStrAnsiStrSortedMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclAnsiStrAnsiStrSortedMap.FreeValue(var Value: AnsiString): AnsiString;
begin
  Result := Value;
  Value := '';
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclAnsiStrAnsiStrSortedMap.KeysCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclAnsiStrAnsiStrSortedMap.ValuesCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclAnsiStrAnsiStrSortedMap,TJclAnsiStrAnsiStrEntry,IJclAnsiStrAnsiStrMap,IJclAnsiStrAnsiStrSortedMap,IJclAnsiStrSet,IJclAnsiStrIterator,IJclAnsiStrCollection,,,,const Key: AnsiString,AnsiString,'',const Value: AnsiString,AnsiString,'',const ToKey: AnsiString,const FromKey\, ToKey: AnsiString,const FromKey: AnsiString)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclWideStrIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclWideStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclWideStrIntfSortedMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclWideStrIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclWideStrIntfSortedMap.KeysCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclWideStrIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  Result := Integer(A) - Integer(B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclWideStrIntfSortedMap,TJclWideStrIntfEntry,IJclWideStrIntfMap,IJclWideStrIntfSortedMap,IJclWideStrSet,IJclWideStrIterator,IJclIntfCollection,,,,const Key: WideString,WideString,'',const Value: IInterface,IInterface,nil,const ToKey: WideString,const FromKey\, ToKey: WideString,const FromKey: WideString)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfWideStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfWideStrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclWideStrArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfWideStrSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfWideStrSortedMap.FreeValue(var Value: WideString): WideString;
begin
  Result := Value;
  Value := '';
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclIntfWideStrSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  Result := Integer(A) - Integer(B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclIntfWideStrSortedMap.ValuesCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfWideStrSortedMap,TJclIntfWideStrEntry,IJclIntfWideStrMap,IJclIntfWideStrSortedMap,IJclIntfSet,IJclIntfIterator,IJclWideStrCollection,,,,const Key: IInterface,IInterface,nil,const Value: WideString,WideString,'',const ToKey: IInterface,const FromKey\, ToKey: IInterface,const FromKey: IInterface)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclWideStrWideStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrWideStrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclWideStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclWideStrArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclWideStrWideStrSortedMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclWideStrWideStrSortedMap.FreeValue(var Value: WideString): WideString;
begin
  Result := Value;
  Value := '';
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclWideStrWideStrSortedMap.KeysCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclWideStrWideStrSortedMap.ValuesCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclWideStrWideStrSortedMap,TJclWideStrWideStrEntry,IJclWideStrWideStrMap,IJclWideStrWideStrSortedMap,IJclWideStrSet,IJclWideStrIterator,IJclWideStrCollection,,,,const Key: WideString,WideString,'',const Value: WideString,WideString,'',const ToKey: WideString,const FromKey\, ToKey: WideString,const FromKey: WideString)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfSortedMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES
function TJclIntfSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclIntfSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  Result := Integer(A) - Integer(B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclIntfSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  Result := Integer(A) - Integer(B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfSortedMap,TJclIntfEntry,IJclIntfMap,IJclIntfSortedMap,IJclIntfSet,IJclIntfIterator,IJclCollection,,; AOwnsValues: Boolean,
  FOwnsValues := AOwnsValues;,const Key: IInterface,IInterface,nil,Value: TObject,TObject,nil,const ToKey: IInterface,const FromKey\, ToKey: IInterface,const FromKey: IInterface)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclAnsiStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclAnsiStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclAnsiStrSortedMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclAnsiStrSortedMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES
function TJclAnsiStrSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclAnsiStrSortedMap.KeysCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclAnsiStrSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  Result := Integer(A) - Integer(B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclAnsiStrSortedMap,TJclAnsiStrEntry,IJclAnsiStrMap,IJclAnsiStrSortedMap,IJclAnsiStrSet,IJclAnsiStrIterator,IJclCollection,,; AOwnsValues: Boolean,
  FOwnsValues := AOwnsValues;,const Key: AnsiString,AnsiString,'',Value: TObject,TObject,nil,const ToKey: AnsiString,const FromKey\, ToKey: AnsiString,const FromKey: AnsiString)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclWideStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclWideStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclWideStrSortedMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclWideStrSortedMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES
function TJclWideStrSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclWideStrSortedMap.KeysCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclWideStrSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  Result := Integer(A) - Integer(B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclWideStrSortedMap,TJclWideStrEntry,IJclWideStrMap,IJclWideStrSortedMap,IJclWideStrSet,IJclWideStrIterator,IJclCollection,,; AOwnsValues: Boolean,
  FOwnsValues := AOwnsValues;,const Key: WideString,WideString,'',Value: TObject,TObject,nil,const ToKey: WideString,const FromKey\, ToKey: WideString,const FromKey: WideString)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSortedMap.Create(FSize, False, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclArraySet.Create(Param, False)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclSortedMap.FreeKey(var Key: TObject): TObject;
begin
  if FOwnsKeys then
  begin
    Result := nil;
    FreeAndNil(Key);
  end
  else
  begin
    Result := Key;
    Key := nil;
  end;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclSortedMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS
function TJclSortedMap.GetOWnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;
}
{$JPPDEFINEMACRO GETOWNSVALUES
function TJclSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclSortedMap.KeysCompare(A, B: TObject): Integer;
begin
  Result := Integer(A) - Integer(B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  Result := Integer(A) - Integer(B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclSortedMap,TJclEntry,IJclMap,IJclSortedMap,IJclSet,IJclIterator,IJclCollection,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;,Key: TObject,TObject,nil,Value: TObject,TObject,nil,ToKey: TObject,FromKey\, ToKey: TObject,FromKey: TObject)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$IFDEF SUPPORTS_GENERICS_DISABLED}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclArraySet<TKey>.Create(Param, False)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList<TValue>.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclSortedMap<TKey,TValue>.FreeKey(var Key: TKey): TKey;
begin
  if FOwnsKeys then
  begin
    Result := Default(TKey);
    FreeAndNil(Key);
  end
  else
  begin
    Result := Key;
    Key := Default(TKey);
  end;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclSortedMap<TKey,TValue>.FreeValue(var Value: TValue): TValue;
begin
  if FOwnsValues then
  begin
    Result := Default(TValue);
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := Default(TValue);
  end;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS
function TJclSortedMap<TKey,TValue>.GetOWnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;
}
{$JPPDEFINEMACRO GETOWNSVALUES
function TJclSortedMap<TKey,TValue>.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO KEYSCOMPARE}
{$JPPDEFINEMACRO VALUESCOMPARE}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclSortedMap<TKey\,TValue>,TJclEntry<TKey\,TValue>,IJclMap<TKey\,TValue>,IJclSortedMap<TKey\,TValue>,IJclSet<TKey>,IJclIterator<TKey>,IJclCollection<TValue>,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;,const Key: TKey,TKey,Default(TKey),const Value: TValue,TValue,Default(TValue),const ToKey: TKey,const FromKey\, ToKey: TKey,const FromKey: TKey)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{function TJclSortedMap<TKey,TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSortedMap<TKey,TValue>.Create(FSize, False, False);
  AssignPropertiesTo(Result);
end;}

{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
