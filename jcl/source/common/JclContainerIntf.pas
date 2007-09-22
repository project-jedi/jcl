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

  IJclPackable = interface
    ['{03802D2B-E0AB-4300-A777-0B8A2BD993DF}']
    procedure Pack; // reduce memory usable by eliminating empty storage area
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

  IJclGrowable = interface(IJclPackable)
    ['{C71E8586-5688-444C-9BDD-9969D988123B}']
    procedure Grow; overload;
    procedure Grow(Increment: Integer); overload;
    procedure Grow(Num, Denom: Integer); overload;
  end;

  IJclObjectOwner = interface
    ['{5157EA13-924E-4A56-995D-36956441025C}']
    procedure FreeObject(var AObject: TObject);
  end;

  {$IFDEF SUPPORTS_GENERICS}
  IJclItemOwner<T> = interface
    ['{0CC220C1-E705-4B21-9F53-4AD340952165}']
    procedure FreeItem(var AItem: T);
  end;
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfIterator = interface
    ['{E121A98A-7C43-4587-806B-9189E8B2F106}']
    procedure Add(const AInterface: IInterface);
    function GetObject: IInterface;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    procedure Insert(const AInterface: IInterface);
    function Next: IInterface;
    function NextIndex: Integer;
    function Previous: IInterface;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetObject(const AInterface: IInterface);
  end;

  IJclStrIterator = interface
    ['{D5D4B681-F902-49C7-B9E1-73007C9D64F0}']
    procedure Add(const AString: string);
    function GetString: string;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    procedure Insert(const AString: string);
    function Next: string;
    function NextIndex: Integer;
    function Previous: string;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetString(const AString: string);
  end;

  IJclIterator = interface
    ['{997DF9B7-9AA2-4239-8B94-14DFFD26D790}']
    procedure Add(AObject: TObject);
    function GetObject: TObject;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    procedure Insert(AObject: TObject);
    function Next: TObject;
    function NextIndex: Integer;
    function Previous: TObject;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetObject(AObject: TObject);
  end;

  {$IFDEF SUPPORTS_GENERICS}
  IJclIterator<T> = interface
    ['{6E8547A4-5B5D-4831-8AE3-9C6D04071B11}']
    procedure Add(const AItem: T);
    function GetItem: T;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    procedure Insert(const AItem: T);
    function Next: T;
    function NextIndex: Integer;
    function Previous: T;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetItem(const AItem: T);

    property Item: T read GetItem write SetItem;
  end;
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfCollection = interface
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
  end;

  IJclStrCollection = interface
    ['{3E3CFC19-E8AF-4DD7-91FA-2DF2895FC7B9}']
    function Add(const AString: string): Boolean;
    function AddAll(const ACollection: IJclStrCollection): Boolean;
    procedure Clear;
    function Contains(const AString: string): Boolean;
    function ContainsAll(const ACollection: IJclStrCollection): Boolean;
    function Equals(const ACollection: IJclStrCollection): Boolean;
    function First: IJclStrIterator;
    function IsEmpty: Boolean;
    function Last: IJclStrIterator;
    function Remove(const AString: string): Boolean;
    function RemoveAll(const ACollection: IJclStrCollection): Boolean;
    function RetainAll(const ACollection: IJclStrCollection): Boolean;
    function Size: Integer;
    //Daniele Teti 27/12/2004
    procedure LoadFromStrings(Strings: TStrings);
    procedure SaveToStrings(Strings: TStrings);
    procedure AppendToStrings(Strings: TStrings);
    procedure AppendFromStrings(Strings: TStrings);
    function GetAsStrings: TStrings;
    function GetAsDelimited(const Separator: string = AnsiLineBreak): string;
    procedure AppendDelimited(const AString: string; const Separator: string = AnsiLineBreak);
    procedure LoadDelimited(const AString: string; const Separator: string = AnsiLineBreak);
  end;

  IJclCollection = interface
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
  end;

  {$IFDEF SUPPORTS_GENERICS}
  IJclCollection<T> = interface
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
  end;
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfList = interface(IJclIntfCollection)
    ['{E14EDA4B-1DAA-4013-9E6C-CDCB365C7CF9}']
    procedure Insert(Index: Integer; const AInterface: IInterface); overload;
    function InsertAll(Index: Integer; const ACollection: IJclIntfCollection): Boolean; overload;
    function GetObject(Index: Integer): IInterface;
    function IndexOf(const AInterface: IInterface): Integer;
    function LastIndexOf(const AInterface: IInterface): Integer;
    function Remove(Index: Integer): IInterface; overload;
    procedure SetObject(Index: Integer; const AInterface: IInterface);
    function SubList(First, Count: Integer): IJclIntfList;
    
    property Items[Key: Integer]: IInterface read GetObject write SetObject; default;
  end;

  IJclStrList = interface(IJclStrCollection)
    ['{07DD7644-EAC6-4059-99FC-BEB7FBB73186}']
    procedure Insert(Index: Integer; const AString: string); overload;
    function InsertAll(Index: Integer; const ACollection: IJclStrCollection): Boolean; overload;
    function GetString(Index: Integer): string;
    function IndexOf(const AString: string): Integer;
    function LastIndexOf(const AString: string): Integer;
    function Remove(Index: Integer): string; overload;
    procedure SetString(Index: Integer; const AString: string);
    function SubList(First, Count: Integer): IJclStrList;
    //Daniele Teti
    property Items[Key: Integer]: string read GetString write SetString; default;
  end;

  IJclList = interface(IJclCollection)
    ['{8ABC70AC-5C06-43EA-AFE0-D066379BCC28}']
    procedure Insert(Index: Integer; AObject: TObject); overload;
    function InsertAll(Index: Integer; const ACollection: IJclCollection): Boolean; overload;
    function GetObject(Index: Integer): TObject;
    function IndexOf(AObject: TObject): Integer;
    function LastIndexOf(AObject: TObject): Integer;
    function Remove(Index: Integer): TObject; overload;
    procedure SetObject(Index: Integer; AObject: TObject);
    function SubList(First, Count: Integer): IJclList;
    //Daniele Teti
    property Items[Key: Integer]: TObject read GetObject write SetObject; default;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  IJclList<T> = interface(IJclCollection<T>)
    ['{3B4BE3D7-8FF7-4163-91DF-3F73AE6935E7}']
    procedure Insert(Index: Integer; const AItem: T); overload;
    function InsertAll(Index: Integer; const ACollection: IJclCollection<T>): Boolean; overload;
    function GetItem(Index: Integer): T;
    function IndexOf(const AItem: T): Integer;
    function LastIndexOf(const AItem: T): Integer;
    function Remove(Index: Integer): T; overload;
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

  IJclStrArray = interface(IJclStrList)
    ['{B055B427-7817-43FC-97D4-AD1845643D63}']
    {$IFDEF CLR}
    function GetString(Index: Integer): string;
    procedure SetString(Index: Integer; const AString: string);
    {$ENDIF CLR}
    property Items[Index: Integer]: string read GetString write SetString; default;
  end;

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

  IJclStrSet = interface(IJclStrCollection)
    ['{72204D85-2B68-4914-B9F2-09E5180C12E9}']
    procedure Intersect(const ACollection: IJclStrCollection);
    procedure Subtract(const ACollection: IJclStrCollection);
    procedure Union(const ACollection: IJclStrCollection);
  end;

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

  IJclStrTree = interface(IJclStrCollection)
    ['{1E1896C0-0497-47DF-83AF-A9422084636C}']
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

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

  IJclIntfIntfMap = interface
    ['{01D05399-4A05-4F3E-92F4-0C236BE77019}']
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Equals(const AMap: IJclIntfIntfMap): Boolean;
    function GetValue(const Key: IInterface): IInterface;
    function IsEmpty: Boolean;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfIntfMap);
    procedure PutValue(const Key, Value: IInterface);
    function Remove(const Key: IInterface): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  end;

  IJclMultiIntfIntfMap = interface(IJclIntfIntfMap)
    ['{497775A5-D3F1-49FC-A641-15CC9E77F3D0}']
    function GetValues(const Key: IInterface): IJclIntfIterator;
    function Count(const Key: IInterface): Integer;
  end;

  IJclStrIntfMap = interface
    ['{A4788A96-281A-4924-AA24-03776DDAAD8A}']
    procedure Clear;
    function ContainsKey(const Key: string): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Equals(const AMap: IJclStrIntfMap): Boolean;
    function GetValue(const Key: string): IInterface;
    function IsEmpty: Boolean;
    function KeySet: IJclStrSet;
    procedure PutAll(const AMap: IJclStrIntfMap);
    procedure PutValue(const Key: string; const Value: IInterface);
    function Remove(const Key: string): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  end;

  IJclStrStrMap = interface
    ['{A4788A96-281A-4924-AA24-03776DDAAD8A}']
    procedure Clear;
    function ContainsKey(const Key: string): Boolean;
    function ContainsValue(const Value: string): Boolean;
    function Equals(const AMap: IJclStrStrMap): Boolean;
    function GetValue(const Key: string): string;
    function IsEmpty: Boolean;
    function KeySet: IJclStrSet;
    procedure PutAll(const AMap: IJclStrStrMap);
    procedure PutValue(const Key, Value: string);
    function Remove(const Key: string): string;
    function Size: Integer;
    function Values: IJclStrCollection;
    //Daniele Teti
    function KeyOfValue(const Value: string): string;
    //Daniele Teti
    property Items[const Key: string]: string read GetValue write PutValue; default;
  end;

  IJclStrMap = interface
    ['{A7D0A882-6952-496D-A258-23D47DDCCBC4}']
    procedure Clear;
    function ContainsKey(const Key: string): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(const AMap: IJclStrMap): Boolean;
    function GetValue(const Key: string): TObject;
    function IsEmpty: Boolean;
    function KeySet: IJclStrSet;
    procedure PutAll(const AMap: IJclStrMap);
    procedure PutValue(const Key: string; Value: TObject);
    function Remove(const Key: string): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    //Daniele Teti
    property Items[const Key: string]: TObject read GetValue write PutValue; default;
  end;

  IJclMap = interface
    ['{A7D0A882-6952-496D-A258-23D47DDCCBC4}']
    procedure Clear;
    function ContainsKey(Key: TObject): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(const AMap: IJclMap): Boolean;
    function GetValue(Key: TObject): TObject;
    function IsEmpty: Boolean;
    function KeySet: IJclSet;
    procedure PutAll(const AMap: IJclMap);
    procedure PutValue(Key, Value: TObject);
    function Remove(Key: TObject): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    //Daniele Teti
    property Items[Key: TObject]: TObject read GetValue write PutValue; default;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  IHashable = interface
    function GetHashCode: Integer;
  end;

  IJclMap<TKey,TValue> = interface
    ['{22624C43-4828-4A1E-BDD4-4A7FE59AE135}']
    procedure Clear;
    function ContainsKey(const Key: TKey): Boolean;
    function ContainsValue(const Value: TValue): Boolean;
    function Equals(const AMap: IJclMap<TKey,TValue>): Boolean;
    function GetValue(Key: TKey): TValue;
    function IsEmpty: Boolean;
    function KeySet: IJclSet<TKey>;
    procedure PutAll(const AMap: IJclMap<TKey,TValue>);
    procedure PutValue(Key: TKey; const Value: TValue);
    function Remove(const Key: TKey): TValue;
    function Size: Integer;
    function Values: IJclCollection<TValue>;

    property Items[Key: TKey]: TValue read GetValue write PutValue; default;
  end;
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfQueue = interface
    ['{B88756FE-5553-4106-957E-3E33120BFA99}']
    procedure Clear;
    function Contains(const AInterface: IInterface): Boolean;
    function Dequeue: IInterface;
    function Empty: Boolean;
    procedure Enqueue(const AInterface: IInterface);
    function Peek: IInterface;
    function Size: Integer;
  end;

  IJclStrQueue = interface
    ['{5BA0ED9A-5AF3-4F79-9D80-34FA7FF15D1F}']
    procedure Clear;
    function Contains(const AString: string): Boolean;
    function Dequeue: string;
    function Empty: Boolean;
    procedure Enqueue(const AString: string);
    function Peek: string;
    function Size: Integer;
  end;

  IJclQueue = interface
    ['{7D0F9DE4-71EA-46EF-B879-88BCFD5D9610}']
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function Dequeue: TObject;
    function Empty: Boolean;
    procedure Enqueue(AObject: TObject);
    function Peek: TObject;
    function Size: Integer;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  IJclQueue<T> = interface
    ['{16AB909F-2194-46CF-BD89-B4207AC0CAB8}']
    procedure Clear;
    function Contains(const AItem: T): Boolean;
    function Dequeue: T;
    function Empty: Boolean;
    procedure Enqueue(const AItem: T);
    function Peek: T;
    function Size: Integer;
  end;
  {$ENDIF SUPPORTS_GENERICS}

  IJclStrStrSortedMap = interface(IJclStrStrMap)
    ['{4F457799-5D03-413D-A46C-067DC4200CC3}']
    function FirstKey: string;
    function HeadMap(const ToKey: string): IJclStrStrSortedMap;
    function LastKey: string;
    function SubMap(const FromKey, ToKey: string): IJclStrStrSortedMap;
    function TailMap(const FromKey: string): IJclStrStrSortedMap;
  end;

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

  IJclIntfStack = interface
    ['{CA1DC7A1-8D8F-4A5D-81D1-0FE32E9A4E84}']
    procedure Clear;
    function Contains(const AInterface: IInterface): Boolean;
    function Empty: Boolean;
    function Peek: IInterface;
    function Pop: IInterface;
    procedure Push(const AInterface: IInterface);
    function Size: Integer;
  end;

  IJclStrStack = interface
    ['{649BB74C-D7BE-40D9-9F4E-32DDC3F13F3B}']
    procedure Clear;
    function Contains(const AString: string): Boolean;
    function Empty: Boolean;
    function Peek: string;
    function Pop: string;
    procedure Push(const AString: string);
    function Size: Integer;
  end;

  IJclStack = interface
    ['{E07E0BD8-A831-41B9-B9A0-7199BD4873B9}']
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function Empty: Boolean;
    function Peek: TObject;
    function Pop: TObject;
    procedure Push(AObject: TObject);
    function Size: Integer;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  IJclStack<T> = interface
    ['{2F08EAC9-270D-496E-BE10-5E975918A5F2}']
    procedure Clear;
    function Contains(const AItem: T): Boolean;
    function Empty: Boolean;
    function Peek: T;
    function Pop: T;
    procedure Push(const AItem: T);
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

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

