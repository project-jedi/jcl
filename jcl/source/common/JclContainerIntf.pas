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
{ The Original Code is DCL_intf.pas.                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
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

unit JclContainerIntf;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes,
  JclBase;

const
  DefaultContainerCapacity = 16;

type
  IJclAbstractIterator = interface
    ['{1064D0B4-D9FC-475D-88BE-520490013B46}']
    procedure Assign(const Source: IJclAbstractIterator);
    procedure AssignTo(const Dest: IJclAbstractIterator);
    function GetIteratorReference: TObject;
  end;

  IJclContainer = interface
    ['{C517175A-028E-486A-BF27-5EF7FC3101D9}']
    procedure Assign(const Source: IJclContainer);
    procedure AssignTo(const Dest: IJclContainer);
    function GetAllowDefaultElements: Boolean;
    function GetContainerReference: TObject;
    function GetDuplicates: TDuplicates;
    function GetRemoveSingleElement: Boolean;
    function GetReturnDefaultElements: Boolean;
    procedure SetAllowDefaultElements(Value: Boolean);
    procedure SetDuplicates(Value: TDuplicates);
    procedure SetRemoveSingleElement(Value: Boolean);
    procedure SetReturnDefaultElements(Value: Boolean);
    property AllowDefaultElements: Boolean read GetAllowDefaultElements write SetAllowDefaultElements;
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
    property RemoveSingleElement: Boolean read GetRemoveSingleElement write SetRemoveSingleElement;
    property ReturnDefaultElements: Boolean read GetReturnDefaultElements write SetReturnDefaultElements;
  end;

  IJclStrContainer = interface(IJclContainer)
    ['{9753E1D7-F093-4D5C-8B32-40403F6F700E}']
    function GetCaseSensitive: Boolean;
    procedure SetCaseSensitive(Value: Boolean);
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
  end;

  TJclAnsiStrEncoding = (seISO {, seUTF8}); // TODO: make JclUnicode compatible with Linux and .NET

  IJclAnsiStrContainer = interface(IJclStrContainer)
    ['{F8239357-B96F-46F1-A48E-B5DF25B5F1FA}']
    function GetEncoding: TJclAnsiStrEncoding;
    procedure SetEncoding(Value: TJclAnsiStrEncoding);
    property Encoding: TJclAnsiStrEncoding read GetEncoding write SetEncoding;
  end;

  IJclAnsiStrFlatContainer = interface(IJclAnsiStrContainer)
    ['{8A45A4D4-6317-4CDF-8314-C3E5CC6899F4}']
    procedure LoadFromStrings(Strings: TStrings);
    procedure SaveToStrings(Strings: TStrings);
    procedure AppendToStrings(Strings: TStrings);
    procedure AppendFromStrings(Strings: TStrings);
    function GetAsStrings: TStrings;
    function GetAsDelimited(const Separator: AnsiString = AnsiLineBreak): AnsiString;
    procedure AppendDelimited(const AString: AnsiString; const Separator: AnsiString = AnsiLineBreak);
    procedure LoadDelimited(const AString: AnsiString; const Separator: AnsiString = AnsiLineBreak);
  end;

  TJclWideStrEncoding = (weUCS2 {, wsUTF16}); // TODO: make JclUnicode compatible with Linux and .NET

  IJclWideStrContainer = interface(IJclStrContainer)
    ['{875E1AC4-CA22-46BC-8999-048E5B9BF11D}']
    function GetEncoding: TJclWideStrEncoding;
    procedure SetEncoding(Value: TJclWideStrEncoding);
    property Encoding: TJclWideStrEncoding read GetEncoding write SetEncoding;
  end;

  IJclWideStrFlatContainer = interface(IJclWideStrContainer)
    ['{5B001B93-CA1C-47A8-98B8-451CCB444930}']
    {procedure LoadFromStrings(Strings: TWideStrings);
    procedure SaveToStrings(Strings: TWideStrings);
    procedure AppendToStrings(Strings: TWideStrings);
    procedure AppendFromStrings(Strings: TWideStrings);
    function GetAsStrings: TWideStrings;
    function GetAsDelimited(const Separator: WideString = WideLineBreak): WideString;
    procedure AppendDelimited(const AString: WideString; const Separator: WideString = WideLineBreak);
    procedure LoadDelimited(const AString: WideString; const Separator: WideString = WideLineBreak);}
  end;

  IJclIntfEqualityComparer = interface
    ['{5CC2DF51-BE56-4D02-A171-31BAAC097632}']
    function ItemsEqual(const A, B: IInterface): Boolean;
  end;

  IJclAnsiStrEqualityComparer = interface
    ['{E3DB9016-F0D0-4CE0-B156-4C5DCA47FD3B}']
    function ItemsEqual(const A, B: AnsiString): Boolean;
  end;

  IJclWideStrEqualityComparer = interface
    ['{2E5696C9-8374-4347-9DC9-B3722F47F5FB}']
    function ItemsEqual(const A, B: WideString): Boolean;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrEqualityComparer = IJclAnsiStrEqualityComparer;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrEqualityComparer = IJclWideStrEqualityComparer;
  {$ENDIF CONTAINER_WIDESTR}

  IJclEqualityComparer = interface
    ['{82C67986-8365-44AB-8D56-7B0CF4F6B918}']
    function ItemsEqual(A, B: TObject): Boolean;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  IJclEqualityComparer<T> = interface
    ['{4AF79AD6-D9F4-424B-BEAA-68857F9222B4}']
    function ItemsEqual(const A, B: T): Boolean;
  end;
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfComparer = interface
    ['{EB41B843-184B-420D-B5DA-27D055B4CD55}']
    function ItemsCompare(const A, B: IInterface): Integer;
  end;

  IJclAnsiStrComparer = interface
    ['{09063CBB-9226-4734-B2A0-A178C2343176}']
    function ItemsCompare(const A, B: AnsiString): Integer;
  end;

  IJclWideStrComparer = interface
    ['{7A24AEDA-25B1-4E73-B2E9-5D74011E4C9C}']
    function ItemsCompare(const A, B: WideString): Integer;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrComparer = IJclAnsiStrComparer;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrComparer = IJclWideStrComparer;
  {$ENDIF CONTAINER_WIDESTR}

  IJclComparer = interface
    ['{7B376028-56DC-4C4A-86A9-1AC19E3EDF75}']
    function ItemsCompare(A, B: TObject): Integer;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  IJclComparer<T> = interface
    ['{830AFC8C-AA06-46F5-AABD-8EB46B2A9986}']
    function ItemsCompare(const A, B: T): Integer;
  end;
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfHashConverter = interface
    ['{7BAA0791-3B45-4D0F-9CD8-D13B81694786}']
    function Hash(const AInterface: IInterface): Integer;
  end;

  IJclAnsiStrHashConverter = interface
    ['{9841014E-8A31-4C79-8AD5-EB03C4E85533}']
    function Hash(const AString: AnsiString): Integer;
  end;

  IJclWideStrHashConverter = interface
    ['{2584118F-19AE-443E-939B-0DB18BCD0117}']
    function Hash(const AString: WideString): Integer;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrHashConverter = IJclAnsiStrHashConverter;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrHashConverter = IJclWideStrHashConverter;
  {$ENDIF CONTAINER_WIDESTR}

  IJclHashConverter = interface
    ['{2D0DD6F4-162E-41D6-8A34-489E7EACABCD}']
    function Hash(AObject: TObject): Integer;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  IJclHashConverter<T> = interface
    ['{300AEA0E-7433-4C3E-99A6-E533212ACF42}']
    function Hash(const AItem: T): Integer;
  end;
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfCloneable = interface
    ['{BCF77740-FB60-4306-9BD1-448AADE5FF4E}']
    function Clone: IInterface;
  end;

  IJclCloneable = interface
    ['{D224AE70-2C93-4998-9479-1D513D75F2B2}']
    function Clone: TObject;
  end;

  IJclLockable = interface
    ['{524AD65E-AE1B-4BC6-91C8-8181F0198BA9}']
    procedure ReadLock;
    procedure ReadUnlock;
    procedure WriteLock;
    procedure WriteUnlock;
  end;

  TJclAutoPackStrategy = (apsDisabled, apsAgressive, apsProportional, apsIncremental);

  // parameter signification depends on strategy
  //  - Disabled = unused (arrays are never packed)
  //  - Agressive = unused (arrays are always packed)
  //  - Proportional = ratio of empty slots before the array is packed
  //    number of empty slots is computed by this formula: Capacity div Parameter
  //  - Incremental = amount of empty slots before the array is packed

  IJclPackable = interface
    ['{03802D2B-E0AB-4300-A777-0B8A2BD993DF}']
    function GetAutoPackParameter: Integer;
    function GetAutoPackStrategy: TJclAutoPackStrategy;
    function GetCapacity: Integer;
    procedure Pack; // reduce used memory by eliminating empty storage area (force)
    procedure SetAutoPackParameter(Value: Integer);
    procedure SetAutoPackStrategy(Value: TJclAutoPackStrategy);
    procedure SetCapacity(Value: Integer);
    property AutoPackParameter: Integer read GetAutoPackParameter write SetAutoPackParameter;
    property AutoPackStrategy: TJclAutoPackStrategy read GetAutoPackStrategy write SetAutoPackStrategy;
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

  TJclAutoGrowStrategy = (agsDisabled, agsAgressive, agsProportional, agsIncremental);

  // parameter signification depends on strategy
  //  - Disabled = unused (arrays never grow)
  //  - Agressive = unused (arrays always grow by 1 element)
  //  - Proportional = ratio of empty slots to add to the array
  //    number of empty slots is computed by this formula: Capacity div Parameter
  //  - Incremental = amount of empty slots to add to the array

  IJclGrowable = interface(IJclPackable)
    ['{C71E8586-5688-444C-9BDD-9969D988123B}']
    function GetAutoGrowParameter: Integer;
    function GetAutoGrowStrategy: TJclAutoGrowStrategy;
    procedure Grow;
    procedure SetAutoGrowParameter(Value: Integer);
    procedure SetAutoGrowStrategy(Value: TJclAutoGrowStrategy);
    property AutoGrowParameter: Integer read GetAutoGrowParameter write SetAutoGrowParameter;
    property AutoGrowStrategy: TJclAutoGrowStrategy read GetAutoGrowStrategy write SetAutoGrowStrategy;
  end;

  IJclObjectOwner = interface
    ['{5157EA13-924E-4A56-995D-36956441025C}']
    function FreeObject(var AObject: TObject): TObject;
    function GetOwnsObjects: Boolean;
    property OwnsObjects: Boolean read GetOwnsObjects;
  end;

  IJclKeyOwner = interface
    ['{8BE209E6-2F85-44FD-B0CD-A8363C95349A}']
    function FreeKey(var Key: TObject): TObject;
    function GetOwnsKeys: Boolean;
    property OwnsKeys: Boolean read GetOwnsKeys;
  end;

  IJclValueOwner = interface
    ['{3BCD98CE-7056-416A-A9E7-AE3AB2A62E54}']
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read GetOwnsValues;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  IJclItemOwner<T> = interface
    ['{0CC220C1-E705-4B21-9F53-4AD340952165}']
    function FreeItem(var AItem: T): T;
    function GetOwnsItems: Boolean;
    property OwnsItems: Boolean read GetOwnsItems;
  end;

  IJclPairOwner<TKey, TValue> = interface
    ['{321C1FF7-AA2E-4229-966A-7EC6417EA16D}']
    function FreeKey(var Key: TKey): TKey;
    function FreeValue(var Value: TValue): TValue;
    function GetOwnsKeys: Boolean;
    function GetOwnsValues: Boolean;
    property OwnsKeys: Boolean read GetOwnsKeys;
    property OwnsValues: Boolean read GetOwnsValues;
  end;
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfIterator = interface(IJclAbstractIterator)
    ['{E121A98A-7C43-4587-806B-9189E8B2F106}']
    function Add(const AInterface: IInterface): Boolean;
    function GetObject: IInterface;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AInterface: IInterface): Boolean;
    function Next: IInterface;
    function NextIndex: Integer;
    function Previous: IInterface;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetObject(const AInterface: IInterface);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: IInterface read GetObject;
    {$ENDIF SUPPORTS_FOR_IN}
  end;

  IJclAnsiStrIterator = interface(IJclAbstractIterator)
    ['{D5D4B681-F902-49C7-B9E1-73007C9D64F0}']
    function Add(const AString: AnsiString): Boolean;
    function GetString: AnsiString;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AString: AnsiString): Boolean;
    function Next: AnsiString;
    function NextIndex: Integer;
    function Previous: AnsiString;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetString(const AString: AnsiString);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: AnsiString read GetString;
    {$ENDIF SUPPORTS_FOR_IN}
  end;

  IJclWideStrIterator = interface(IJclAbstractIterator)
    ['{F03BC7D4-CCDA-4C4A-AF3A-E51FDCDE8ADE}']
    function Add(const AString: WideString): Boolean;
    function GetString: WideString;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AString: WideString): Boolean;
    function Next: WideString;
    function NextIndex: Integer;
    function Previous: WideString;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetString(const AString: WideString);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: WideString read GetString;
    {$ENDIF SUPPORTS_FOR_IN}
  end;

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrIterator = IJclAnsiStrIterator;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrIterator = IJclWideStrIterator;
  {$ENDIF CONTAINER_WIDESTR}

  IJclIterator = interface(IJclAbstractIterator)
    ['{997DF9B7-9AA2-4239-8B94-14DFFD26D790}']
    function Add(AObject: TObject): Boolean;
    function GetObject: TObject;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(AObject: TObject): Boolean;
    function Next: TObject;
    function NextIndex: Integer;
    function Previous: TObject;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetObject(AObject: TObject);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: TObject read GetObject;
    {$ENDIF SUPPORTS_FOR_IN}
  end;

  {$IFDEF SUPPORTS_GENERICS}
  IJclIterator<T> = interface(IJclAbstractIterator)
    ['{6E8547A4-5B5D-4831-8AE3-9C6D04071B11}']
    function Add(const AItem: T): Boolean;
    function GetItem: T;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AItem: T): Boolean;
    function Next: T;
    function NextIndex: Integer;
    function Previous: T;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetItem(const AItem: T);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: T read GetItem;
    {$ENDIF SUPPORTS_FOR_IN}
  end;
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfCollection = interface(IJclContainer)
    ['{8E178463-4575-487A-B4D5-DC2AED3C7ACA}']
    function Add(const AInterface: IInterface): Boolean;
    function AddAll(const ACollection: IJclIntfCollection): Boolean;
    procedure Clear;
    function Contains(const AInterface: IInterface): Boolean;
    function ContainsAll(const ACollection: IJclIntfCollection): Boolean;
    function Equals(const ACollection: IJclIntfCollection): Boolean;
    function First: IJclIntfIterator;
    function IsEmpty: Boolean;
    function Last: IJclIntfIterator;
    function Remove(const AInterface: IInterface): Boolean;
    function RemoveAll(const ACollection: IJclIntfCollection): Boolean;
    function RetainAll(const ACollection: IJclIntfCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIntfIterator;
    {$ENDIF SUPPORTS_FOR_IN}
  end;

  IJclAnsiStrCollection = interface(IJclAnsiStrFlatContainer)
    ['{3E3CFC19-E8AF-4DD7-91FA-2DF2895FC7B9}']
    function Add(const AString: AnsiString): Boolean;
    function AddAll(const ACollection: IJclAnsiStrCollection): Boolean;
    procedure Clear;
    function Contains(const AString: AnsiString): Boolean;
    function ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean;
    function Equals(const ACollection: IJclAnsiStrCollection): Boolean;
    function First: IJclAnsiStrIterator;
    function IsEmpty: Boolean;
    function Last: IJclAnsiStrIterator;
    function Remove(const AString: AnsiString): Boolean;
    function RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean;
    function RetainAll(const ACollection: IJclAnsiStrCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclAnsiStrIterator;
    {$ENDIF SUPPORTS_FOR_IN}
  end;

  IJclWideStrCollection = interface(IJclWideStrFlatContainer)
    ['{CDCC0F94-4DD0-4F25-B441-6AE55D5C7466}']
    function Add(const AString: WideString): Boolean;
    function AddAll(const ACollection: IJclWideStrCollection): Boolean;
    procedure Clear;
    function Contains(const AString: WideString): Boolean;
    function ContainsAll(const ACollection: IJclWideStrCollection): Boolean;
    function Equals(const ACollection: IJclWideStrCollection): Boolean;
    function First: IJclWideStrIterator;
    function IsEmpty: Boolean;
    function Last: IJclWideStrIterator;
    function Remove(const AString: WideString): Boolean;
    function RemoveAll(const ACollection: IJclWideStrCollection): Boolean;
    function RetainAll(const ACollection: IJclWideStrCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclWideStrIterator;
    {$ENDIF SUPPORTS_FOR_IN}
  end;

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrCollection = IJclAnsiStrCollection;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrCollection = IJclWideStrCollection;
  {$ENDIF CONTAINER_WIDESTR}

  IJclCollection = interface(IJclContainer)
    ['{58947EF1-CD21-4DD1-AE3D-225C3AAD7EE5}']
    function Add(AObject: TObject): Boolean;
    function AddAll(const ACollection: IJclCollection): Boolean;
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function ContainsAll(const ACollection: IJclCollection): Boolean;
    function Equals(const ACollection: IJclCollection): Boolean;
    function First: IJclIterator;
    function IsEmpty: Boolean;
    function Last: IJclIterator;
    function Remove(AObject: TObject): Boolean;
    function RemoveAll(const ACollection: IJclCollection): Boolean;
    function RetainAll(const ACollection: IJclCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIterator;
    {$ENDIF SUPPORTS_FOR_IN}
  end;

  {$IFDEF SUPPORTS_GENERICS}
  IJclCollection<T> = interface(IJclContainer)
    ['{67EE8AF3-19B0-4DCA-A730-3C9B261B8EC5}']
    function Add(const AItem: T): Boolean;
    function AddAll(const ACollection: IJclCollection<T>): Boolean;
    procedure Clear;
    function Contains(const AItem: T): Boolean;
    function ContainsAll(const ACollection: IJclCollection<T>): Boolean;
    function Equals(const ACollection: IJclCollection<T>): Boolean;
    function First: IJclIterator<T>;
    function IsEmpty: Boolean;
    function Last: IJclIterator<T>;
    function Remove(const AItem: T): Boolean;
    function RemoveAll(const ACollection: IJclCollection<T>): Boolean;
    function RetainAll(const ACollection: IJclCollection<T>): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIterator<T>;
    {$ENDIF SUPPORTS_FOR_IN}
  end;
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfList = interface(IJclIntfCollection)
    ['{E14EDA4B-1DAA-4013-9E6C-CDCB365C7CF9}']
    function Insert(Index: Integer; const AInterface: IInterface): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclIntfCollection): Boolean;
    function GetObject(Index: Integer): IInterface;
    function IndexOf(const AInterface: IInterface): Integer;
    function LastIndexOf(const AInterface: IInterface): Integer;
    function Delete(Index: Integer): IInterface;
    procedure SetObject(Index: Integer; const AInterface: IInterface);
    function SubList(First, Count: Integer): IJclIntfList;
    
    property Items[Key: Integer]: IInterface read GetObject write SetObject; default;
  end;

  IJclAnsiStrList = interface(IJclAnsiStrCollection)
    ['{07DD7644-EAC6-4059-99FC-BEB7FBB73186}']
    function Insert(Index: Integer; const AString: AnsiString): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclAnsiStrCollection): Boolean;
    function GetString(Index: Integer): AnsiString;
    function IndexOf(const AString: AnsiString): Integer;
    function LastIndexOf(const AString: AnsiString): Integer;
    function Delete(Index: Integer): AnsiString;
    procedure SetString(Index: Integer; const AString: AnsiString);
    function SubList(First, Count: Integer): IJclAnsiStrList;
    //Daniele Teti
    property Items[Key: Integer]: AnsiString read GetString write SetString; default;
  end;

  IJclWideStrList = interface(IJclWideStrCollection)
    ['{C9955874-6AC0-4CE0-8CC0-606A3F1702C6}']
    function Insert(Index: Integer; const AString: WideString): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclWideStrCollection): Boolean;
    function GetString(Index: Integer): WideString;
    function IndexOf(const AString: WideString): Integer;
    function LastIndexOf(const AString: WideString): Integer;
    function Delete(Index: Integer): WideString;
    procedure SetString(Index: Integer; const AString: WideString);
    function SubList(First, Count: Integer): IJclWideStrList;
    //Daniele Teti
    property Items[Key: Integer]: WideString read GetString write SetString; default;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrList = IJclAnsiStrList;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrList = IJclWideStrList;
  {$ENDIF CONTAINER_WIDESTR}

  IJclList = interface(IJclCollection)
    ['{8ABC70AC-5C06-43EA-AFE0-D066379BCC28}']
    function Insert(Index: Integer; AObject: TObject): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclCollection): Boolean;
    function GetObject(Index: Integer): TObject;
    function IndexOf(AObject: TObject): Integer;
    function LastIndexOf(AObject: TObject): Integer;
    function Delete(Index: Integer): TObject;
    procedure SetObject(Index: Integer; AObject: TObject);
    function SubList(First, Count: Integer): IJclList;
    //Daniele Teti
    property Items[Key: Integer]: TObject read GetObject write SetObject; default;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  IJclList<T> = interface(IJclCollection<T>)
    ['{3B4BE3D7-8FF7-4163-91DF-3F73AE6935E7}']
    function Insert(Index: Integer; const AItem: T): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclCollection<T>): Boolean;
    function GetItem(Index: Integer): T;
    function IndexOf(const AItem: T): Integer;
    function LastIndexOf(const AItem: T): Integer;
    function Delete(Index: Integer): T;
    procedure SetItem(Index: Integer; const AItem: T);
    function SubList(First, Count: Integer): IJclList<T>;
    //Daniele Teti
    property Items[Key: Integer]: T read GetItem write SetItem; default;
  end;
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfArray = interface(IJclIntfList)
    ['{B055B427-7817-43FC-97D4-AD1845643D63}']
    {$IFDEF CLR}
    function GetObject(Index: Integer): IInterface;
    procedure SetObject(Index: Integer; const AInterface: IInterface);
    {$ENDIF CLR}
    property Items[Index: Integer]: IInterface read GetObject write SetObject; default;
  end;

  IJclAnsiStrArray = interface(IJclAnsiStrList)
    ['{B055B427-7817-43FC-97D4-AD1845643D63}']
    {$IFDEF CLR}
    function GetString(Index: Integer): AnsiString;
    procedure SetString(Index: Integer; const AString: AnsiString);
    {$ENDIF CLR}
    property Items[Index: Integer]: AnsiString read GetString write SetString; default;
  end;

  IJclWideStrArray = interface(IJclWideStrList)
    ['{3CE09F9A-5CB4-4867-80D5-C2313D278D69}']
    {$IFDEF CLR}
    function GetString(Index: Integer): WideString;
    procedure SetString(Index: Integer; const AString: WideString);
    {$ENDIF CLR}
    property Items[Index: Integer]: WideString read GetString write SetString; default;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrArray = IJclAnsiStrArray;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrArray = IJclWideStrArray;
  {$ENDIF CONTAINER_WIDESTR}

  IJclArray = interface(IJclList)
    ['{A69F6D35-54B2-4361-852E-097ED75E648A}']
    {$IFDEF CLR}
    function GetObject(Index: Integer): TObject;
    procedure SetObject(Index: Integer; AObject: TObject);
    {$ENDIF CLR}
    property Items[Index: Integer]: TObject read GetObject write SetObject; default;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  IJclArray<T> = interface(IJclList<T>)
    ['{38810C13-E35E-428A-B84F-D25FB994BE8E}']
    {$IFDEF CLR}
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; const AItem: T);
    {$ENDIF CLR}
    property Items[Index: Integer]: T read GetItem write SetItem; default;
  end;
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfSet = interface(IJclIntfCollection)
    ['{E2D28852-9774-49B7-A739-5DBA2B705924}']
    procedure Intersect(const ACollection: IJclIntfCollection);
    procedure Subtract(const ACollection: IJclIntfCollection);
    procedure Union(const ACollection: IJclIntfCollection);
  end;

  IJclAnsiStrSet = interface(IJclAnsiStrCollection)
    ['{72204D85-2B68-4914-B9F2-09E5180C12E9}']
    procedure Intersect(const ACollection: IJclAnsiStrCollection);
    procedure Subtract(const ACollection: IJclAnsiStrCollection);
    procedure Union(const ACollection: IJclAnsiStrCollection);
  end;

  IJclWideStrSet = interface(IJclWideStrCollection)
    ['{08009E0A-ABDD-46AB-8CEE-407D4723E17C}']
    procedure Intersect(const ACollection: IJclWideStrCollection);
    procedure Subtract(const ACollection: IJclWideStrCollection);
    procedure Union(const ACollection: IJclWideStrCollection);
  end;

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrSet = IJclAnsiStrSet;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrSet = IJclWideStrSet;
  {$ENDIF CONTAINER_WIDESTR}

  IJclSet = interface(IJclCollection)
    ['{0B7CDB90-8588-4260-A54C-D87101C669EA}']
    procedure Intersect(const ACollection: IJclCollection);
    procedure Subtract(const ACollection: IJclCollection);
    procedure Union(const ACollection: IJclCollection);
  end;

  {$IFDEF SUPPORTS_GENERICS}
  IJclSet<T> = interface(IJclCollection<T>)
    ['{0B7CDB90-8588-4260-A54C-D87101C669EA}']
    procedure Intersect(const ACollection: IJclCollection<T>);
    procedure Subtract(const ACollection: IJclCollection<T>);
    procedure Union(const ACollection: IJclCollection<T>);
  end;
  {$ENDIF SUPPORTS_GENERICS}

  TJclTraverseOrder = (toPreOrder, toOrder, toPostOrder);

  IJclIntfTree = interface(IJclIntfCollection)
    ['{5A21688F-113D-41B4-A17C-54BDB0BD6559}']
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  IJclAnsiStrTree = interface(IJclAnsiStrCollection)
    ['{1E1896C0-0497-47DF-83AF-A9422084636C}']
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  IJclWideStrTree = interface(IJclWideStrCollection)
    ['{E325615A-7A20-4788-87FA-9051002CCD91}']
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrTree = IJclAnsiStrTree;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrTree = IJclWideStrTree;
  {$ENDIF CONTAINER_WIDESTR}

  IJclTree = interface(IJclCollection)
    ['{B0C658CC-FEF5-4178-A4C5-442C0DEDE207}']
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  IJclTree<T> = interface(IJclCollection<T>)
    ['{3F963AB5-5A75-41F9-A21B-7E7FB541A459}']
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfIntfMap = interface(IJclContainer)
    ['{01D05399-4A05-4F3E-92F4-0C236BE77019}']
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Equals(const AMap: IJclIntfIntfMap): Boolean;
    function GetValue(const Key: IInterface): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfIntfMap);
    procedure PutValue(const Key, Value: IInterface);
    function Remove(const Key: IInterface): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    property Items[const Key: IInterface]: IInterface read GetValue write PutValue;
  end;

  (*IJclMultiIntfIntfMap = interface(IJclIntfIntfMap)
    ['{497775A5-D3F1-49FC-A641-15CC9E77F3D0}']
    function GetValues(const Key: IInterface): IJclIntfIterator;
    function Count(const Key: IInterface): Integer;
  end;*)

  IJclAnsiStrIntfMap = interface(IJclAnsiStrContainer)
    ['{A4788A96-281A-4924-AA24-03776DDAAD8A}']
    procedure Clear;
    function ContainsKey(const Key: AnsiString): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Equals(const AMap: IJclAnsiStrIntfMap): Boolean;
    function GetValue(const Key: AnsiString): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): AnsiString;
    function KeySet: IJclAnsiStrSet;
    procedure PutAll(const AMap: IJclAnsiStrIntfMap);
    procedure PutValue(const Key: AnsiString; const Value: IInterface);
    function Remove(const Key: AnsiString): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    property Items[const Key: AnsiString]: IInterface read GetValue write PutValue;
  end;

  IJclWideStrIntfMap = interface(IJclWideStrContainer)
    ['{C959AB76-9CF0-4C2C-A2C6-8A1846563FAF}']
    procedure Clear;
    function ContainsKey(const Key: WideString): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Equals(const AMap: IJclWideStrIntfMap): Boolean;
    function GetValue(const Key: WideString): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): WideString;
    function KeySet: IJclWideStrSet;
    procedure PutAll(const AMap: IJclWideStrIntfMap);
    procedure PutValue(const Key: WideString; const Value: IInterface);
    function Remove(const Key: WideString): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    property Items[const Key: WideString]: IInterface read GetValue write PutValue;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrIntfMap = IJclAnsiStrIntfMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrIntfMap = IJclWideStrIntfMap;
  {$ENDIF CONTAINER_WIDESTR}

  IJclIntfAnsiStrMap = interface(IJclAnsiStrContainer)
    ['{B10E324A-1D98-42FF-B9B4-7F99044591B2}']
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: AnsiString): Boolean;
    function Equals(const AMap: IJclIntfAnsiStrMap): Boolean;
    function GetValue(const Key: IInterface): AnsiString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: AnsiString): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfAnsiStrMap);
    procedure PutValue(const Key: IInterface; const Value: AnsiString);
    function Remove(const Key: IInterface): AnsiString;
    function Size: Integer;
    function Values: IJclAnsiStrCollection;
    property Items[const Key: IInterface]: AnsiString read GetValue write PutValue;
  end;

  IJclIntfWideStrMap = interface(IJclWideStrContainer)
    ['{D9FD7887-B840-4636-8A8F-E586663E332C}']
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: WideString): Boolean;
    function Equals(const AMap: IJclIntfWideStrMap): Boolean;
    function GetValue(const Key: IInterface): WideString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: WideString): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfWideStrMap);
    procedure PutValue(const Key: IInterface; const Value: WideString);
    function Remove(const Key: IInterface): WideString;
    function Size: Integer;
    function Values: IJclWideStrCollection;
    property Items[const Key: IInterface]: WideString read GetValue write PutValue;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  IJclIntfStrMap = IJclIntfAnsiStrMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclIntfStrMap = IJclIntfWideStrMap;
  {$ENDIF CONTAINER_WIDESTR}

  IJclAnsiStrAnsiStrMap = interface(IJclAnsiStrContainer)
    ['{A4788A96-281A-4924-AA24-03776DDAAD8A}']
    procedure Clear;
    function ContainsKey(const Key: AnsiString): Boolean;
    function ContainsValue(const Value: AnsiString): Boolean;
    function Equals(const AMap: IJclAnsiStrAnsiStrMap): Boolean;
    function GetValue(const Key: AnsiString): AnsiString;
    function IsEmpty: Boolean;
    //Daniele Teti
    function KeyOfValue(const Value: AnsiString): AnsiString;
    function KeySet: IJclAnsiStrSet;
    procedure PutAll(const AMap: IJclAnsiStrAnsiStrMap);
    procedure PutValue(const Key, Value: AnsiString);
    function Remove(const Key: AnsiString): AnsiString;
    function Size: Integer;
    function Values: IJclAnsiStrCollection;
    //Daniele Teti
    property Items[const Key: AnsiString]: AnsiString read GetValue write PutValue;
  end;

  IJclWideStrWideStrMap = interface(IJclWideStrContainer)
    ['{8E8D2735-C4FB-4F00-8802-B2102BCE3644}']
    procedure Clear;
    function ContainsKey(const Key: WideString): Boolean;
    function ContainsValue(const Value: WideString): Boolean;
    function Equals(const AMap: IJclWideStrWideStrMap): Boolean;
    function GetValue(const Key: WideString): WideString;
    function IsEmpty: Boolean;
    //Daniele Teti
    function KeyOfValue(const Value: WideString): WideString;
    function KeySet: IJclWideStrSet;
    procedure PutAll(const AMap: IJclWideStrWideStrMap);
    procedure PutValue(const Key, Value: WideString);
    function Remove(const Key: WideString): WideString;
    function Size: Integer;
    function Values: IJclWideStrCollection;
    //Daniele Teti
    property Items[const Key: WideString]: WideString read GetValue write PutValue;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrStrMap = IJclAnsiStrAnsiStrMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrStrMap = IJclWideStrWideStrMap;
  {$ENDIF CONTAINER_WIDESTR}

  IJclIntfMap = interface(IJclContainer)
    ['{C70570C6-EDDB-47B4-9003-C637B486731D}']
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(const AMap: IJclIntfMap): Boolean;
    function GetValue(const Key: IInterface): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfMap);
    procedure PutValue(const Key: IInterface; Value: TObject);
    function Remove(const Key: IInterface): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    //Daniele Teti
    property Items[const Key: IInterface]: TObject read GetValue write PutValue;
  end;

  IJclAnsiStrMap = interface(IJclAnsiStrContainer)
    ['{A7D0A882-6952-496D-A258-23D47DDCCBC4}']
    procedure Clear;
    function ContainsKey(const Key: AnsiString): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(const AMap: IJclAnsiStrMap): Boolean;
    function GetValue(const Key: AnsiString): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): AnsiString;
    function KeySet: IJclAnsiStrSet;
    procedure PutAll(const AMap: IJclAnsiStrMap);
    procedure PutValue(const Key: AnsiString; Value: TObject);
    function Remove(const Key: AnsiString): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    //Daniele Teti
    property Items[const Key: AnsiString]: TObject read GetValue write PutValue;
  end;

  IJclWideStrMap = interface(IJclWideStrContainer)
    ['{ACE8E6B4-5A56-4753-A2C6-BAE195A56B63}']
    procedure Clear;
    function ContainsKey(const Key: WideString): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(const AMap: IJclWideStrMap): Boolean;
    function GetValue(const Key: WideString): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): WideString;
    function KeySet: IJclWideStrSet;
    procedure PutAll(const AMap: IJclWideStrMap);
    procedure PutValue(const Key: WideString; Value: TObject);
    function Remove(const Key: WideString): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    //Daniele Teti
    property Items[const Key: WideString]: TObject read GetValue write PutValue;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrMap = IJclAnsiStrMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrMap = IJclWideStrMap;
  {$ENDIF CONTAINER_WIDESTR}

  IJclMap = interface(IJclContainer)
    ['{A7D0A882-6952-496D-A258-23D47DDCCBC4}']
    procedure Clear;
    function ContainsKey(Key: TObject): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(const AMap: IJclMap): Boolean;
    function GetValue(Key: TObject): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): TObject;
    function KeySet: IJclSet;
    procedure PutAll(const AMap: IJclMap);
    procedure PutValue(Key, Value: TObject);
    function Remove(Key: TObject): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    //Daniele Teti
    property Items[Key: TObject]: TObject read GetValue write PutValue;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  IHashable = interface
    function GetHashCode: Integer;
  end;

  IJclMap<TKey,TValue> = interface(IJclContainer)
    ['{22624C43-4828-4A1E-BDD4-4A7FE59AE135}']
    procedure Clear;
    function ContainsKey(const Key: TKey): Boolean;
    function ContainsValue(const Value: TValue): Boolean;
    function Equals(const AMap: IJclMap<TKey,TValue>): Boolean;
    function GetValue(const Key: TKey): TValue;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: TValue): TKey;
    function KeySet: IJclSet<TKey>;
    procedure PutAll(const AMap: IJclMap<TKey,TValue>);
    procedure PutValue(const Key: TKey; const Value: TValue);
    function Remove(const Key: TKey): TValue;
    function Size: Integer;
    function Values: IJclCollection<TValue>;

    property Items[const Key: TKey]: TValue read GetValue write PutValue;
  end;
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfQueue = interface(IJclContainer)
    ['{B88756FE-5553-4106-957E-3E33120BFA99}']
    procedure Clear;
    function Contains(const AInterface: IInterface): Boolean;
    function Dequeue: IInterface;
    function Empty: Boolean;
    function Enqueue(const AInterface: IInterface): Boolean;
    function Peek: IInterface;
    function Size: Integer;
  end;

  IJclAnsiStrQueue = interface(IJclAnsiStrContainer)
    ['{5BA0ED9A-5AF3-4F79-9D80-34FA7FF15D1F}']
    procedure Clear;
    function Contains(const AString: AnsiString): Boolean;
    function Dequeue: AnsiString;
    function Empty: Boolean;
    function Enqueue(const AString: AnsiString): Boolean;
    function Peek: AnsiString;
    function Size: Integer;
  end;

  IJclWideStrQueue = interface(IJclWideStrContainer)
    ['{058BBFB7-E9B9-44B5-B676-D5B5B9A79BEF}']
    procedure Clear;
    function Contains(const AString: WideString): Boolean;
    function Dequeue: WideString;
    function Empty: Boolean;
    function Enqueue(const AString: WideString): Boolean;
    function Peek: WideString;
    function Size: Integer;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrQueue = IJclAnsiStrQueue;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrQueue = IJclWideStrQueue;
  {$ENDIF CONTAINER_WIDESTR}

  IJclQueue = interface(IJclContainer)
    ['{7D0F9DE4-71EA-46EF-B879-88BCFD5D9610}']
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function Dequeue: TObject;
    function Empty: Boolean;
    function Enqueue(AObject: TObject): Boolean;
    function Peek: TObject;
    function Size: Integer;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  IJclQueue<T> = interface(IJclContainer)
    ['{16AB909F-2194-46CF-BD89-B4207AC0CAB8}']
    procedure Clear;
    function Contains(const AItem: T): Boolean;
    function Dequeue: T;
    function Empty: Boolean;
    function Enqueue(const AItem: T): Boolean;
    function Peek: T;
    function Size: Integer;
  end;
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfIntfSortedMap = interface(IJclIntfIntfMap)
    ['{265A6EB2-4BB3-459F-8813-360FD32A4971}']
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfIntfSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfIntfSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfIntfSortedMap;
  end;

  IJclAnsiStrIntfSortedMap = interface(IJclAnsiStrIntfMap)
    ['{706D1C91-5416-4FDC-B6B1-F4C1E8CFCD38}']
    function FirstKey: AnsiString;
    function HeadMap(const ToKey: AnsiString): IJclAnsiStrIntfSortedMap;
    function LastKey: AnsiString;
    function SubMap(const FromKey, ToKey: AnsiString): IJclAnsiStrIntfSortedMap;
    function TailMap(const FromKey: AnsiString): IJclAnsiStrIntfSortedMap;
  end;

  IJclWideStrIntfSortedMap = interface(IJclWideStrIntfMap)
    ['{299FDCFD-2DB7-4D64-BF18-EE3668316430}']
    function FirstKey: WideString;
    function HeadMap(const ToKey: WideString): IJclWideStrIntfSortedMap;
    function LastKey: WideString;
    function SubMap(const FromKey, ToKey: WideString): IJclWideStrIntfSortedMap;
    function TailMap(const FromKey: WideString): IJclWideStrIntfSortedMap;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrIntfSortedMap = IJclAnsiStrIntfSortedMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrIntfSortedMap = IJclWideStrIntfSortedMap;
  {$ENDIF CONTAINER_WIDESTR}

  IJclIntfAnsiStrSortedMap = interface(IJclIntfAnsiStrMap)
    ['{96E6AC5E-8C40-4795-9C8A-CFD098B58680}']
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfAnsiStrSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfAnsiStrSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfAnsiStrSortedMap;
  end;

  IJclIntfWideStrSortedMap = interface(IJclIntfWideStrMap)
    ['{FBE3AD2E-2781-4DC0-9E80-027027380E21}']
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfWideStrSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfWideStrSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfWideStrSortedMap;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  IJclIntfStrSortedMap = IJclIntfAnsiStrSortedMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclIntfStrSortedMap = IJclIntfWideStrSortedMap;
  {$ENDIF CONTAINER_WIDESTR}

  IJclAnsiStrAnsiStrSortedMap = interface(IJclAnsiStrAnsiStrMap)
    ['{4F457799-5D03-413D-A46C-067DC4200CC3}']
    function FirstKey: AnsiString;
    function HeadMap(const ToKey: AnsiString): IJclAnsiStrAnsiStrSortedMap;
    function LastKey: AnsiString;
    function SubMap(const FromKey, ToKey: AnsiString): IJclAnsiStrAnsiStrSortedMap;
    function TailMap(const FromKey: AnsiString): IJclAnsiStrAnsiStrSortedMap;
  end;

  IJclWideStrWideStrSortedMap = interface(IJclWideStrWideStrMap)
    ['{3B0757B2-2290-4AFA-880D-F9BA600E501E}']
    function FirstKey: WideString;
    function HeadMap(const ToKey: WideString): IJclWideStrWideStrSortedMap;
    function LastKey: WideString;
    function SubMap(const FromKey, ToKey: WideString): IJclWideStrWideStrSortedMap;
    function TailMap(const FromKey: WideString): IJclWideStrWideStrSortedMap;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrStrSortedMap = IJclAnsiStrAnsiStrSortedMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrStrSortedMap = IJclWideStrWideStrSortedMap;
  {$ENDIF CONTAINER_WIDESTR}

  IJclIntfSortedMap = interface(IJclIntfMap)
    ['{3CED1477-B958-4109-9BDA-7C84B9E063B2}']
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfSortedMap;
  end;

  IJclAnsiStrSortedMap = interface(IJclAnsiStrMap)
    ['{573F98E3-EBCD-4F28-8F35-96A7366CBF47}']
    function FirstKey: AnsiString;
    function HeadMap(const ToKey: AnsiString): IJclAnsiStrSortedMap;
    function LastKey: AnsiString;
    function SubMap(const FromKey, ToKey: AnsiString): IJclAnsiStrSortedMap;
    function TailMap(const FromKey: AnsiString): IJclAnsiStrSortedMap;
  end;

  IJclWideStrSortedMap = interface(IJclWideStrMap)
    ['{B3021EFC-DE25-4B4B-A896-ACE823CD5C01}']
    function FirstKey: WideString;
    function HeadMap(const ToKey: WideString): IJclWideStrSortedMap;
    function LastKey: WideString;
    function SubMap(const FromKey, ToKey: WideString): IJclWideStrSortedMap;
    function TailMap(const FromKey: WideString): IJclWideStrSortedMap;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrSortedMap = IJclAnsiStrSortedMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrSortedMap = IJclWideStrSortedMap;
  {$ENDIF CONTAINER_WIDESTR}

  IJclSortedMap = interface(IJclMap)
    ['{F317A70F-7851-49C2-9DCF-092D8F4D4F98}']
    function FirstKey: TObject;
    function HeadMap(ToKey: TObject): IJclSortedMap;
    function LastKey: TObject;
    function SubMap(FromKey, ToKey: TObject): IJclSortedMap;
    function TailMap(FromKey: TObject): IJclSortedMap;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  IJclSortedMap<K,V> = interface(IJclMap<K,V>)
    ['{C62B75C4-891B-442E-A5D6-9954E75A5C0C}']
    function FirstKey: K;
    function HeadMap(const ToKey: K): IJclSortedMap<K,V>;
    function LastKey: K;
    function SubMap(const FromKey, ToKey: K): IJclSortedMap<K,V>;
    function TailMap(const FromKey: K): IJclSortedMap<K,V>;
  end;
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfSortedSet = interface(IJclIntfSet)
    ['{159BE5A7-7349-42FF-BE55-9CA1B9DBA991}']
    function HeadSet(const AEndObject: IInterface): IJclIntfSortedSet;
    function SubSet(const Start, Finish: IInterface): IJclIntfSortedSet;
    function TailSet(const AStartObject: IInterface): IJclIntfSortedSet;
  end;

  IJclAnsiStrSortedSet = interface(IJclAnsiStrSet)
    ['{03198146-F967-4310-868B-7AD3D52D5CBE}']
    function HeadSet(const AEndObject: AnsiString): IJclAnsiStrSortedSet;
    function SubSet(const Start, Finish: AnsiString): IJclAnsiStrSortedSet;
    function TailSet(const AStartObject: AnsiString): IJclAnsiStrSortedSet;
  end;

  IJclWideStrSortedSet = interface(IJclWideStrSet)
    ['{ED9567E2-C1D3-4C00-A1D4-90D5C7E27C2D}']
    function HeadSet(const AEndObject: WideString): IJclWideStrSortedSet;
    function SubSet(const Start, Finish: WideString): IJclWideStrSortedSet;
    function TailSet(const AStartObject: WideString): IJclWideStrSortedSet;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrSortedSet = IJclAnsiStrSortedSet;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrSortedSet = IJclWideStrSortedSet;
  {$ENDIF CONTAINER_WIDESTR}

  IJclSortedSet = interface(IJclSet)
    ['{A3D23E76-ADE9-446C-9B97-F49FCE895D9F}']
    function HeadSet(AEndObject: TObject): IJclSortedSet;
    function SubSet(Start, Finish: TObject): IJclSortedSet;
    function TailSet(AStartObject: TObject): IJclSortedSet;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  IJclSortedSet<T> = interface(IJclSet<T>)
    ['{30F836E3-2FB1-427E-A499-DFAE201633C8}']
    function HeadSet(const AEndItem: T): IJclSortedSet<T>;
    function SubSet(const Start, Finish: T): IJclSortedSet<T>;
    function TailSet(const AStartItem: T): IJclSortedSet<T>;
  end;
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfStack = interface(IJclContainer)
    ['{CA1DC7A1-8D8F-4A5D-81D1-0FE32E9A4E84}']
    procedure Clear;
    function Contains(const AInterface: IInterface): Boolean;
    function Empty: Boolean;
    function Peek: IInterface;
    function Pop: IInterface;
    function Push(const AInterface: IInterface): Boolean;
    function Size: Integer;
  end;

  IJclAnsiStrStack = interface(IJclAnsiStrContainer)
    ['{649BB74C-D7BE-40D9-9F4E-32DDC3F13F3B}']
    procedure Clear;
    function Contains(const AString: AnsiString): Boolean;
    function Empty: Boolean;
    function Peek: AnsiString;
    function Pop: AnsiString;
    function Push(const AString: AnsiString): Boolean;
    function Size: Integer;
  end;

  IJclWideStrStack = interface(IJclWideStrContainer)
    ['{B2C3B165-33F1-4B7D-A2EC-0B19D12CE33C}']
    procedure Clear;
    function Contains(const AString: WideString): Boolean;
    function Empty: Boolean;
    function Peek: WideString;
    function Pop: WideString;
    function Push(const AString: WideString): Boolean;
    function Size: Integer;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrStack = IJclAnsiStrStack;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrStack = IJclWideStrStack;
  {$ENDIF CONTAINER_WIDESTR}

  IJclStack = interface(IJclContainer)
    ['{E07E0BD8-A831-41B9-B9A0-7199BD4873B9}']
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function Empty: Boolean;
    function Peek: TObject;
    function Pop: TObject;
    function Push(AObject: TObject): Boolean;
    function Size: Integer;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  IJclStack<T> = interface(IJclContainer)
    ['{2F08EAC9-270D-496E-BE10-5E975918A5F2}']
    procedure Clear;
    function Contains(const AItem: T): Boolean;
    function Empty: Boolean;
    function Peek: T;
    function Pop: T;
    function Push(const AItem: T): Boolean;
    function Size: Integer;
  end;
  {$ENDIF SUPPORTS_GENERICS}

  // Exceptions
  EJclContainerError = class(EJclError);

  EJclOutOfBoundsError = class(EJclContainerError)
  public
    // RsEOutOfBounds
    constructor Create;
  end;

  EJclNoSuchElementError = class(EJclContainerError)
  public
    // RsEValueNotFound
    constructor Create(const Value: string);
  end;

  EJclDuplicateElementError = class(EJclContainerError)
  public
    // RsEDuplicateElement
    constructor Create;
  end;

  EJclIllegalArgumentError = class(EJclContainerError)
  end;

  EJclNoCollectionError = class(EJclIllegalArgumentError)
  public
    // RsENoCollection
    constructor Create;
  end;

  EJclIllegalQueueCapacityError = class(EJclIllegalArgumentError)
  public
    // RsEIllegalQueueCapacity
    constructor Create;
  end;

  EJclOperationNotSupportedError = class(EJclContainerError)
  public
    // RsEOperationNotSupported
    constructor Create;
  end;

  EJclNoEqualityComparerError = class(EJclContainerError)
  public
    // RsENoEqualityComparer
    constructor Create;
  end;

  EJclNoComparerError = class(EJclContainerError)
  public
    // RsENoComparer
    constructor Create;
  end;

  EJclNoHashConverterError = class(EJclContainerError)
  public
    // RsENoHashConverter
    constructor Create;
  end;

  EJclIllegalStateOperationError = class(EJclContainerError)
  public
    // RsEIllegalStateOperation
    constructor Create;
  end;

  EJclAssignError = class(EJclContainerError)
  public
    // RsEAssignError
    constructor Create;
  end;

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
  JclResources;

//=== { EJclOutOfBoundsError } ===============================================

constructor EJclOutOfBoundsError.Create;
begin
  {$IFDEF CLR}
  inherited Create(RsEOutOfBounds);
  {$ELSE ~CLR}
  inherited CreateRes(@RsEOutOfBounds);
  {$ENDIF ~CLR}
end;

//=== { EJclNoSuchElementError } =============================================

constructor EJclNoSuchElementError.Create(const Value: string);
begin
  {$IFDEF CLR}
  inherited Create(Format(RsEValueNotFound, [Value]));
  {$ELSE ~CLR}
  inherited CreateResFmt(@RsEValueNotFound, [Value]);
  {$ENDIF ~CLR}
end;

//=== { EJclDuplicateElementError } ==========================================

constructor EJclDuplicateElementError.Create;
begin
  {$IFDEF CLR}
  inherited Create(RsEDuplicateElement);
  {$ELSE ~CLR}
  inherited CreateRes(@RsEDuplicateElement);
  {$ENDIF ~CLR}
end;

//=== { EJclIllegalQueueCapacityError } ======================================

constructor EJclIllegalQueueCapacityError.Create;
begin
  {$IFDEF CLR}
  inherited Create(RsEIllegalQueueCapacity);
  {$ELSE ~CLR}
  inherited CreateRes(@RsEIllegalQueueCapacity);
  {$ENDIF ~CLR}
end;

//=== { EJclNoCollectionError } ==============================================

constructor EJclNoCollectionError.Create;
begin
  {$IFDEF CLR}
  inherited Create(RsENoCollection);
  {$ELSE ~CLR}
  inherited CreateRes(@RsENoCollection);
  {$ENDIF ~CLR}
end;

//=== { EJclOperationNotSupportedError } =====================================

constructor EJclOperationNotSupportedError.Create;
begin
  {$IFDEF CLR}
  inherited Create(RsEOperationNotSupported);
  {$ELSE ~CLR}
  inherited CreateRes(@RsEOperationNotSupported);
  {$ENDIF ~CLR}
end;

//=== { EJclIllegalStateOperationError } =====================================

constructor EJclIllegalStateOperationError.Create;
begin
  {$IFDEF CLR}
  inherited Create(RsEIllegalStateOperation);
  {$ELSE ~CLR}
  inherited CreateRes(@RsEIllegalStateOperation);
  {$ENDIF ~CLR}
end;

//=== { EJclNoComparerError } ================================================

constructor EJclNoComparerError.Create;
begin
  {$IFDEF CLR}
  inherited Create(RsENoComparer);
  {$ELSE ~CLR}
  inherited CreateRes(@RsENoComparer);
  {$ENDIF ~CLR}
end;

//=== { EJclNoEqualityComparerError } ========================================

constructor EJclNoEqualityComparerError.Create;
begin
  {$IFDEF CLR}
  inherited Create(RsENoEqualityComparer);
  {$ELSE ~CLR}
  inherited CreateRes(@RsENoEqualityComparer);
  {$ENDIF ~CLR}
end;

//=== { EJclNoHashConverterError } ===========================================

constructor EJclNoHashConverterError.Create;
begin
  {$IFDEF CLR}
  inherited Create(RsENoHashConverter);
  {$ELSE ~CLR}
  inherited CreateRes(@RsENoHashConverter);
  {$ENDIF ~CLR}
end;

//=== { EJclAssignError } ====================================================

constructor EJclAssignError.Create;
begin
  {$IFDEF CLR}
  inherited Create(RsEAssignError);
  {$ELSE ~CLR}
  inherited CreateRes(@RsEAssignError);
  {$ENDIF ~CLR}
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

