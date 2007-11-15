{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

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
{ The Original Code is BinaryTree.pas.                                                             }
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

unit JclBinaryTrees;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes,
  {$IFDEF SUPPORTS_GENERICS}
  {$IFDEF CLR}
  System.Collections.Generic,
  {$ENDIF CLR}
  {$ENDIF SUPPORTS_GENERICS}
  JclBase, JclAbstractContainers, JclAlgorithms, JclContainerIntf;
type

  TJclIntfBinaryNode = class
  public
    Value: IInterface;
    Left: TJclIntfBinaryNode;
    Right: TJclIntfBinaryNode;
    Parent: TJclIntfBinaryNode;
  end;

  TJclIntfBinaryTree = class(TJclIntfAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclIntfEqualityComparer, IJclIntfComparer,
    IJclIntfCollection, IJclIntfTree)
  private
    FMaxDepth: Integer;
    FRoot: TJclIntfBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    FCompare: TIntfCompare;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclIntfCollection }
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
    { IJclIntfTree }
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    { IJclIntfComparer }
    function ItemsCompare(const A, B: IInterface): Integer;
    { IJclIntfEqualityComparer }
    function ItemsEqual(const A, B: IInterface): Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TIntfCompare);
    destructor Destroy; override;
    property Compare: TIntfCompare read FCompare write FCompare;
  end;


  TJclAnsiStrBinaryNode = class
  public
    Value: AnsiString;
    Left: TJclAnsiStrBinaryNode;
    Right: TJclAnsiStrBinaryNode;
    Parent: TJclAnsiStrBinaryNode;
  end;

  TJclAnsiStrBinaryTree = class(TJclAnsiStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclStrContainer,
    IJclAnsiStrContainer, IJclAnsiStrFlatContainer, IJclAnsiStrEqualityComparer, IJclAnsiStrComparer,
    IJclAnsiStrCollection, IJclAnsiStrTree)
  private
    FMaxDepth: Integer;
    FRoot: TJclAnsiStrBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    FCompare: TAnsiStrCompare;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclAnsiStrCollection }
    function Add(const AString: AnsiString): Boolean; override;
    function AddAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    procedure Clear; override;
    function Contains(const AString: AnsiString): Boolean; override;
    function ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function Equals(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function First: IJclAnsiStrIterator; override;
    function IsEmpty: Boolean; override;
    function Last: IJclAnsiStrIterator; override;
    function Remove(const AString: AnsiString): Boolean; override;
    function RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function RetainAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function Size: Integer; override;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclAnsiStrIterator; override;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclAnsiStrTree }
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    { IJclAnsiStrComparer }
    function ItemsCompare(const A, B: AnsiString): Integer;
    { IJclAnsiStrEqualityComparer }
    function ItemsEqual(const A, B: AnsiString): Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TAnsiStrCompare);
    destructor Destroy; override;
    property Compare: TAnsiStrCompare read FCompare write FCompare;
  end;


  TJclWideStrBinaryNode = class
  public
    Value: WideString;
    Left: TJclWideStrBinaryNode;
    Right: TJclWideStrBinaryNode;
    Parent: TJclWideStrBinaryNode;
  end;

  TJclWideStrBinaryTree = class(TJclWideStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclStrContainer,
    IJclWideStrContainer, IJclWideStrFlatContainer, IJclWideStrEqualityComparer, IJclWideStrComparer,
    IJclWideStrCollection, IJclWideStrTree)
  private
    FMaxDepth: Integer;
    FRoot: TJclWideStrBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    FCompare: TWideStrCompare;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclWideStrCollection }
    function Add(const AString: WideString): Boolean; override;
    function AddAll(const ACollection: IJclWideStrCollection): Boolean; override;
    procedure Clear; override;
    function Contains(const AString: WideString): Boolean; override;
    function ContainsAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function Equals(const ACollection: IJclWideStrCollection): Boolean; override;
    function First: IJclWideStrIterator; override;
    function IsEmpty: Boolean; override;
    function Last: IJclWideStrIterator; override;
    function Remove(const AString: WideString): Boolean; override;
    function RemoveAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function RetainAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function Size: Integer; override;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclWideStrIterator; override;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclWideStrTree }
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    { IJclWideStrComparer }
    function ItemsCompare(const A, B: WideString): Integer;
    { IJclWideStrEqualityComparer }
    function ItemsEqual(const A, B: WideString): Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TWideStrCompare);
    destructor Destroy; override;
    property Compare: TWideStrCompare read FCompare write FCompare;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrBinaryTree = TJclAnsiStrBinaryTree;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrBinaryTree = TJclWideStrBinaryTree;
  {$ENDIF CONTAINER_WIDESTR}


  TJclSingleBinaryNode = class
  public
    Value: Single;
    Left: TJclSingleBinaryNode;
    Right: TJclSingleBinaryNode;
    Parent: TJclSingleBinaryNode;
  end;

  TJclSingleBinaryTree = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclSingleContainer,
    IJclSingleEqualityComparer, IJclSingleComparer,
    IJclSingleCollection, IJclSingleTree)
  private
    FMaxDepth: Integer;
    FRoot: TJclSingleBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    FCompare: TSingleCompare;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclSingleCollection }
    function Add(const AValue: Single): Boolean;
    function AddAll(const ACollection: IJclSingleCollection): Boolean;
    procedure Clear;
    function Contains(const AValue: Single): Boolean;
    function ContainsAll(const ACollection: IJclSingleCollection): Boolean;
    function Equals(const ACollection: IJclSingleCollection): Boolean;
    function First: IJclSingleIterator;
    function IsEmpty: Boolean;
    function Last: IJclSingleIterator;
    function Remove(const AValue: Single): Boolean;
    function RemoveAll(const ACollection: IJclSingleCollection): Boolean;
    function RetainAll(const ACollection: IJclSingleCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclSingleIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclSingleTree }
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    { IJclSingleComparer }
    function ItemsCompare(const A, B: Single): Integer;
    { IJclSingleEqualityComparer }
    function ItemsEqual(const A, B: Single): Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TSingleCompare);
    destructor Destroy; override;
    property Compare: TSingleCompare read FCompare write FCompare;
  end;


  TJclDoubleBinaryNode = class
  public
    Value: Double;
    Left: TJclDoubleBinaryNode;
    Right: TJclDoubleBinaryNode;
    Parent: TJclDoubleBinaryNode;
  end;

  TJclDoubleBinaryTree = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclDoubleContainer,
    IJclDoubleEqualityComparer, IJclDoubleComparer,
    IJclDoubleCollection, IJclDoubleTree)
  private
    FMaxDepth: Integer;
    FRoot: TJclDoubleBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    FCompare: TDoubleCompare;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclDoubleCollection }
    function Add(const AValue: Double): Boolean;
    function AddAll(const ACollection: IJclDoubleCollection): Boolean;
    procedure Clear;
    function Contains(const AValue: Double): Boolean;
    function ContainsAll(const ACollection: IJclDoubleCollection): Boolean;
    function Equals(const ACollection: IJclDoubleCollection): Boolean;
    function First: IJclDoubleIterator;
    function IsEmpty: Boolean;
    function Last: IJclDoubleIterator;
    function Remove(const AValue: Double): Boolean;
    function RemoveAll(const ACollection: IJclDoubleCollection): Boolean;
    function RetainAll(const ACollection: IJclDoubleCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclDoubleIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclDoubleTree }
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    { IJclDoubleComparer }
    function ItemsCompare(const A, B: Double): Integer;
    { IJclDoubleEqualityComparer }
    function ItemsEqual(const A, B: Double): Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TDoubleCompare);
    destructor Destroy; override;
    property Compare: TDoubleCompare read FCompare write FCompare;
  end;


  TJclExtendedBinaryNode = class
  public
    Value: Extended;
    Left: TJclExtendedBinaryNode;
    Right: TJclExtendedBinaryNode;
    Parent: TJclExtendedBinaryNode;
  end;

  TJclExtendedBinaryTree = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclExtendedContainer,
    IJclExtendedEqualityComparer, IJclExtendedComparer,
    IJclExtendedCollection, IJclExtendedTree)
  private
    FMaxDepth: Integer;
    FRoot: TJclExtendedBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    FCompare: TExtendedCompare;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclExtendedCollection }
    function Add(const AValue: Extended): Boolean;
    function AddAll(const ACollection: IJclExtendedCollection): Boolean;
    procedure Clear;
    function Contains(const AValue: Extended): Boolean;
    function ContainsAll(const ACollection: IJclExtendedCollection): Boolean;
    function Equals(const ACollection: IJclExtendedCollection): Boolean;
    function First: IJclExtendedIterator;
    function IsEmpty: Boolean;
    function Last: IJclExtendedIterator;
    function Remove(const AValue: Extended): Boolean;
    function RemoveAll(const ACollection: IJclExtendedCollection): Boolean;
    function RetainAll(const ACollection: IJclExtendedCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclExtendedIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclExtendedTree }
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    { IJclExtendedComparer }
    function ItemsCompare(const A, B: Extended): Integer;
    { IJclExtendedEqualityComparer }
    function ItemsEqual(const A, B: Extended): Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TExtendedCompare);
    destructor Destroy; override;
    property Compare: TExtendedCompare read FCompare write FCompare;
  end;

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatBinaryTree = TJclExtendedBinaryTree;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatBinaryTree = TJclDoubleBinaryTree;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatBinaryTree = TJclSingleBinaryTree;
  {$ENDIF MATH_SINGLE_PRECISION}


  TJclIntegerBinaryNode = class
  public
    Value: Integer;
    Left: TJclIntegerBinaryNode;
    Right: TJclIntegerBinaryNode;
    Parent: TJclIntegerBinaryNode;
  end;

  TJclIntegerBinaryTree = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclIntegerEqualityComparer, IJclIntegerComparer,
    IJclIntegerCollection, IJclIntegerTree)
  private
    FMaxDepth: Integer;
    FRoot: TJclIntegerBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    FCompare: TIntegerCompare;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclIntegerCollection }
    function Add(AValue: Integer): Boolean;
    function AddAll(const ACollection: IJclIntegerCollection): Boolean;
    procedure Clear;
    function Contains(AValue: Integer): Boolean;
    function ContainsAll(const ACollection: IJclIntegerCollection): Boolean;
    function Equals(const ACollection: IJclIntegerCollection): Boolean;
    function First: IJclIntegerIterator;
    function IsEmpty: Boolean;
    function Last: IJclIntegerIterator;
    function Remove(AValue: Integer): Boolean;
    function RemoveAll(const ACollection: IJclIntegerCollection): Boolean;
    function RetainAll(const ACollection: IJclIntegerCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIntegerIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclIntegerTree }
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    { IJclIntegerComparer }
    function ItemsCompare(A, B: Integer): Integer;
    { IJclIntegerEqualityComparer }
    function ItemsEqual(A, B: Integer): Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TIntegerCompare);
    destructor Destroy; override;
    property Compare: TIntegerCompare read FCompare write FCompare;
  end;


  TJclCardinalBinaryNode = class
  public
    Value: Cardinal;
    Left: TJclCardinalBinaryNode;
    Right: TJclCardinalBinaryNode;
    Parent: TJclCardinalBinaryNode;
  end;

  TJclCardinalBinaryTree = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclCardinalEqualityComparer, IJclCardinalComparer,
    IJclCardinalCollection, IJclCardinalTree)
  private
    FMaxDepth: Integer;
    FRoot: TJclCardinalBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    FCompare: TCardinalCompare;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclCardinalCollection }
    function Add(AValue: Cardinal): Boolean;
    function AddAll(const ACollection: IJclCardinalCollection): Boolean;
    procedure Clear;
    function Contains(AValue: Cardinal): Boolean;
    function ContainsAll(const ACollection: IJclCardinalCollection): Boolean;
    function Equals(const ACollection: IJclCardinalCollection): Boolean;
    function First: IJclCardinalIterator;
    function IsEmpty: Boolean;
    function Last: IJclCardinalIterator;
    function Remove(AValue: Cardinal): Boolean;
    function RemoveAll(const ACollection: IJclCardinalCollection): Boolean;
    function RetainAll(const ACollection: IJclCardinalCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclCardinalIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclCardinalTree }
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    { IJclCardinalComparer }
    function ItemsCompare(A, B: Cardinal): Integer;
    { IJclCardinalEqualityComparer }
    function ItemsEqual(A, B: Cardinal): Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TCardinalCompare);
    destructor Destroy; override;
    property Compare: TCardinalCompare read FCompare write FCompare;
  end;


  TJclInt64BinaryNode = class
  public
    Value: Int64;
    Left: TJclInt64BinaryNode;
    Right: TJclInt64BinaryNode;
    Parent: TJclInt64BinaryNode;
  end;

  TJclInt64BinaryTree = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclInt64EqualityComparer, IJclInt64Comparer,
    IJclInt64Collection, IJclInt64Tree)
  private
    FMaxDepth: Integer;
    FRoot: TJclInt64BinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    FCompare: TInt64Compare;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclInt64Collection }
    function Add(const AValue: Int64): Boolean;
    function AddAll(const ACollection: IJclInt64Collection): Boolean;
    procedure Clear;
    function Contains(const AValue: Int64): Boolean;
    function ContainsAll(const ACollection: IJclInt64Collection): Boolean;
    function Equals(const ACollection: IJclInt64Collection): Boolean;
    function First: IJclInt64Iterator;
    function IsEmpty: Boolean;
    function Last: IJclInt64Iterator;
    function Remove(const AValue: Int64): Boolean;
    function RemoveAll(const ACollection: IJclInt64Collection): Boolean;
    function RetainAll(const ACollection: IJclInt64Collection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclInt64Iterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclInt64Tree }
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    { IJclInt64Comparer }
    function ItemsCompare(const A, B: Int64): Integer;
    { IJclInt64EqualityComparer }
    function ItemsEqual(const A, B: Int64): Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TInt64Compare);
    destructor Destroy; override;
    property Compare: TInt64Compare read FCompare write FCompare;
  end;

  {$IFNDEF CLR}

  TJclPtrBinaryNode = class
  public
    Value: Pointer;
    Left: TJclPtrBinaryNode;
    Right: TJclPtrBinaryNode;
    Parent: TJclPtrBinaryNode;
  end;

  TJclPtrBinaryTree = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclPtrEqualityComparer, IJclPtrComparer,
    IJclPtrCollection, IJclPtrTree)
  private
    FMaxDepth: Integer;
    FRoot: TJclPtrBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    FCompare: TPtrCompare;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclPtrCollection }
    function Add(APtr: Pointer): Boolean;
    function AddAll(const ACollection: IJclPtrCollection): Boolean;
    procedure Clear;
    function Contains(APtr: Pointer): Boolean;
    function ContainsAll(const ACollection: IJclPtrCollection): Boolean;
    function Equals(const ACollection: IJclPtrCollection): Boolean;
    function First: IJclPtrIterator;
    function IsEmpty: Boolean;
    function Last: IJclPtrIterator;
    function Remove(APtr: Pointer): Boolean;
    function RemoveAll(const ACollection: IJclPtrCollection): Boolean;
    function RetainAll(const ACollection: IJclPtrCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclPtrIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclPtrTree }
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    { IJclPtrComparer }
    function ItemsCompare(A, B: Pointer): Integer;
    { IJclPtrEqualityComparer }
    function ItemsEqual(A, B: Pointer): Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TPtrCompare);
    destructor Destroy; override;
    property Compare: TPtrCompare read FCompare write FCompare;
  end;
  {$ENDIF ~CLR}


  TJclBinaryNode = class
  public
    Value: TObject;
    Left: TJclBinaryNode;
    Right: TJclBinaryNode;
    Parent: TJclBinaryNode;
  end;

  TJclBinaryTree = class(TJclAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclObjectOwner, IJclEqualityComparer, IJclComparer,
    IJclCollection, IJclTree)
  private
    FMaxDepth: Integer;
    FRoot: TJclBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    FCompare: TCompare;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclCollection }
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
    { IJclTree }
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    { IJclComparer }
    function ItemsCompare(A, B: TObject): Integer;
    { IJclEqualityComparer }
    function ItemsEqual(A, B: TObject): Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TCompare; AOwnsObjects: Boolean);
    destructor Destroy; override;
    property Compare: TCompare read FCompare write FCompare;
  end;

  {$IFDEF SUPPORTS_GENERICS}

  TJclBinaryNode<T> = class
  public
    Value: T;
    Left: TJclBinaryNode<T>;
    Right: TJclBinaryNode<T>;
    Parent: TJclBinaryNode<T>;
  end;

  TJclBinaryTree<T> = class(TJclAbstractContainer<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  private
    FMaxDepth: Integer;
    FRoot: TJclBinaryNode<T>;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclCollection<T> }
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
    { IJclTree<T> }
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
  public
    constructor Create(AOwnsItems: Boolean);
    destructor Destroy; override;
  end;

  // E = External helper to compare items
  TJclBinaryTreeE<T> = class(TJclBinaryTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  private
    FComparer: IComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclComparer<T> }
    function ItemsCompare(const A, B: T): Integer; override;
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AComparer: IComparer<T>; AOwnsItems: Boolean);
    property Comparer: IComparer<T> read FComparer write FComparer;
  end;

  // F = Function to compare items
  TJclBinaryTreeF<T> = class(TJclBinaryTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  private
    FCompare: TCompare<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclComparer<T> }
    function ItemsCompare(const A, B: T): Integer; override;
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(ACompare: TCompare<T>; AOwnsItems: Boolean);
    property Compare: TCompare<T> read FCompare write FCompare;
  end;

  // I = Items can compare themselves to an other
  TJclBinaryTreeI<T: IComparable<T>> = class(TJclBinaryTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclComparer<T> }
    function ItemsCompare(const A, B: T): Integer; override;
    { IJclEqualityComparer<T> }
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


//=== { TIntfItr } ===========================================================

type
  TIntfItr = class(TJclAbstractIterator, IJclIntfIterator)
  protected
    FCursor: TJclIntfBinaryNode;
    FOwnList: IJclIntfCollection;
    FEqualityComparer: IJclIntfEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclIntfBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclIntfBinaryNode; virtual; abstract;
    { IJclIntfIterator }
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
  public
    constructor Create(const OwnList: IJclIntfCollection; Start: TJclIntfBinaryNode; AValid: Boolean);
  end;

constructor TIntfItr.Create(const OwnList: IJclIntfCollection; Start: TJclIntfBinaryNode; AValid: Boolean);
begin
  inherited Create(OwnList, AValid);
  FCursor := Start;
  FOwnList := OwnList;
  FEqualityComparer := FOwnList as IJclIntfEqualityComparer;
end;

function TIntfItr.Add(const AInterface: IInterface): Boolean;
begin
  Result := FOwnList.Add(AInterface);
end;

procedure TIntfItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TIntfItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TIntfItr then
  begin
    ADest := TIntfItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
  end;
end;

function TIntfItr.GetObject: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  CheckValid;
  Result := nil;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    Result := GetNextCursor <> nil
  else
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    Result := GetPreviousCursor <> nil
  else
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.Insert(const AInterface: IInterface): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TIntfItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TIntfItr.Next: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    FCursor := GetNextCursor
  else
    Valid := True;
  Result := nil;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TIntfItr.Previous: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    FCursor := GetPreviousCursor
  else
    Valid := True;
  Result := nil;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TIntfItr.Remove;
var
  OldCursor: TJclIntfBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  CheckValid;
  Valid := False;
  OldCursor := FCursor;
  if OldCursor <> nil then
  begin
    repeat
      FCursor := GetNextCursor;
    until (FCursor = nil) or FOwnList.RemoveSingleElement
      or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
    FOwnList.Remove(OldCursor.Value);
  end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TIntfItr.SetObject(const AInterface: IInterface);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderIntfItr } ===================================================

type
  TPreOrderIntfItr = class(TIntfItr, IJclIntfIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntfBinaryNode; override;
    function GetPreviousCursor: TJclIntfBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPreOrderIntfItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderIntfItr.Create(FOwnList, FCursor, Valid);
end;

function TPreOrderIntfItr.GetNextCursor: TJclIntfBinaryNode;
var
  LastRet: TJclIntfBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderIntfItr.GetPreviousCursor: TJclIntfBinaryNode;
var
  LastRet: TJclIntfBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil) then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderIntfItr } ====================================================

type
  TInOrderIntfItr = class(TIntfItr, IJclIntfIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntfBinaryNode; override;
    function GetPreviousCursor: TJclIntfBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TInOrderIntfItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderIntfItr.Create(FOwnList, FCursor, Valid);
end;

function TInOrderIntfItr.GetNextCursor: TJclIntfBinaryNode;
var
  LastRet: TJclIntfBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderIntfItr.GetPreviousCursor: TJclIntfBinaryNode;
var
  LastRet: TJclIntfBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderIntfItr } ==================================================

type
  TPostOrderIntfItr = class(TIntfItr, IJclIntfIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntfBinaryNode; override;
    function GetPreviousCursor: TJclIntfBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPostOrderIntfItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderIntfItr.Create(FOwnList, FCursor, Valid);
end;

function TPostOrderIntfItr.GetNextCursor: TJclIntfBinaryNode;
var
  LastRet: TJclIntfBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderIntfItr.GetPreviousCursor: TJclIntfBinaryNode;
var
  LastRet: TJclIntfBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

//=== { TAnsiStrItr } ===========================================================

type
  TAnsiStrItr = class(TJclAbstractIterator, IJclAnsiStrIterator)
  protected
    FCursor: TJclAnsiStrBinaryNode;
    FOwnList: IJclAnsiStrCollection;
    FEqualityComparer: IJclAnsiStrEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclAnsiStrBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclAnsiStrBinaryNode; virtual; abstract;
    { IJclAnsiStrIterator }
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
  public
    constructor Create(const OwnList: IJclAnsiStrCollection; Start: TJclAnsiStrBinaryNode; AValid: Boolean);
  end;

constructor TAnsiStrItr.Create(const OwnList: IJclAnsiStrCollection; Start: TJclAnsiStrBinaryNode; AValid: Boolean);
begin
  inherited Create(OwnList, AValid);
  FCursor := Start;
  FOwnList := OwnList;
  FEqualityComparer := FOwnList as IJclAnsiStrEqualityComparer;
end;

function TAnsiStrItr.Add(const AString: AnsiString): Boolean;
begin
  Result := FOwnList.Add(AString);
end;

procedure TAnsiStrItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TAnsiStrItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TAnsiStrItr then
  begin
    ADest := TAnsiStrItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
  end;
end;

function TAnsiStrItr.GetString: AnsiString;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  CheckValid;
  Result := '';
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    Result := GetNextCursor <> nil
  else
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    Result := GetPreviousCursor <> nil
  else
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.Insert(const AString: AnsiString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TAnsiStrItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TAnsiStrItr.Next: AnsiString;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    FCursor := GetNextCursor
  else
    Valid := True;
  Result := '';
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TAnsiStrItr.Previous: AnsiString;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    FCursor := GetPreviousCursor
  else
    Valid := True;
  Result := '';
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TAnsiStrItr.Remove;
var
  OldCursor: TJclAnsiStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  CheckValid;
  Valid := False;
  OldCursor := FCursor;
  if OldCursor <> nil then
  begin
    repeat
      FCursor := GetNextCursor;
    until (FCursor = nil) or FOwnList.RemoveSingleElement
      or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
    FOwnList.Remove(OldCursor.Value);
  end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TAnsiStrItr.SetString(const AString: AnsiString);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderAnsiStrItr } ===================================================

type
  TPreOrderAnsiStrItr = class(TAnsiStrItr, IJclAnsiStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclAnsiStrBinaryNode; override;
    function GetPreviousCursor: TJclAnsiStrBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPreOrderAnsiStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderAnsiStrItr.Create(FOwnList, FCursor, Valid);
end;

function TPreOrderAnsiStrItr.GetNextCursor: TJclAnsiStrBinaryNode;
var
  LastRet: TJclAnsiStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderAnsiStrItr.GetPreviousCursor: TJclAnsiStrBinaryNode;
var
  LastRet: TJclAnsiStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil) then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderAnsiStrItr } ====================================================

type
  TInOrderAnsiStrItr = class(TAnsiStrItr, IJclAnsiStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclAnsiStrBinaryNode; override;
    function GetPreviousCursor: TJclAnsiStrBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TInOrderAnsiStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderAnsiStrItr.Create(FOwnList, FCursor, Valid);
end;

function TInOrderAnsiStrItr.GetNextCursor: TJclAnsiStrBinaryNode;
var
  LastRet: TJclAnsiStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderAnsiStrItr.GetPreviousCursor: TJclAnsiStrBinaryNode;
var
  LastRet: TJclAnsiStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderAnsiStrItr } ==================================================

type
  TPostOrderAnsiStrItr = class(TAnsiStrItr, IJclAnsiStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclAnsiStrBinaryNode; override;
    function GetPreviousCursor: TJclAnsiStrBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPostOrderAnsiStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderAnsiStrItr.Create(FOwnList, FCursor, Valid);
end;

function TPostOrderAnsiStrItr.GetNextCursor: TJclAnsiStrBinaryNode;
var
  LastRet: TJclAnsiStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderAnsiStrItr.GetPreviousCursor: TJclAnsiStrBinaryNode;
var
  LastRet: TJclAnsiStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

//=== { TWideStrItr } ===========================================================

type
  TWideStrItr = class(TJclAbstractIterator, IJclWideStrIterator)
  protected
    FCursor: TJclWideStrBinaryNode;
    FOwnList: IJclWideStrCollection;
    FEqualityComparer: IJclWideStrEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclWideStrBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclWideStrBinaryNode; virtual; abstract;
    { IJclWideStrIterator }
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
  public
    constructor Create(const OwnList: IJclWideStrCollection; Start: TJclWideStrBinaryNode; AValid: Boolean);
  end;

constructor TWideStrItr.Create(const OwnList: IJclWideStrCollection; Start: TJclWideStrBinaryNode; AValid: Boolean);
begin
  inherited Create(OwnList, AValid);
  FCursor := Start;
  FOwnList := OwnList;
  FEqualityComparer := FOwnList as IJclWideStrEqualityComparer;
end;

function TWideStrItr.Add(const AString: WideString): Boolean;
begin
  Result := FOwnList.Add(AString);
end;

procedure TWideStrItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TWideStrItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TWideStrItr then
  begin
    ADest := TWideStrItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
  end;
end;

function TWideStrItr.GetString: WideString;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  CheckValid;
  Result := '';
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    Result := GetNextCursor <> nil
  else
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    Result := GetPreviousCursor <> nil
  else
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.Insert(const AString: WideString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TWideStrItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TWideStrItr.Next: WideString;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    FCursor := GetNextCursor
  else
    Valid := True;
  Result := '';
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TWideStrItr.Previous: WideString;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    FCursor := GetPreviousCursor
  else
    Valid := True;
  Result := '';
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TWideStrItr.Remove;
var
  OldCursor: TJclWideStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  CheckValid;
  Valid := False;
  OldCursor := FCursor;
  if OldCursor <> nil then
  begin
    repeat
      FCursor := GetNextCursor;
    until (FCursor = nil) or FOwnList.RemoveSingleElement
      or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
    FOwnList.Remove(OldCursor.Value);
  end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TWideStrItr.SetString(const AString: WideString);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderWideStrItr } ===================================================

type
  TPreOrderWideStrItr = class(TWideStrItr, IJclWideStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclWideStrBinaryNode; override;
    function GetPreviousCursor: TJclWideStrBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPreOrderWideStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderWideStrItr.Create(FOwnList, FCursor, Valid);
end;

function TPreOrderWideStrItr.GetNextCursor: TJclWideStrBinaryNode;
var
  LastRet: TJclWideStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderWideStrItr.GetPreviousCursor: TJclWideStrBinaryNode;
var
  LastRet: TJclWideStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil) then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderWideStrItr } ====================================================

type
  TInOrderWideStrItr = class(TWideStrItr, IJclWideStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclWideStrBinaryNode; override;
    function GetPreviousCursor: TJclWideStrBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TInOrderWideStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderWideStrItr.Create(FOwnList, FCursor, Valid);
end;

function TInOrderWideStrItr.GetNextCursor: TJclWideStrBinaryNode;
var
  LastRet: TJclWideStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderWideStrItr.GetPreviousCursor: TJclWideStrBinaryNode;
var
  LastRet: TJclWideStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderWideStrItr } ==================================================

type
  TPostOrderWideStrItr = class(TWideStrItr, IJclWideStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclWideStrBinaryNode; override;
    function GetPreviousCursor: TJclWideStrBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPostOrderWideStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderWideStrItr.Create(FOwnList, FCursor, Valid);
end;

function TPostOrderWideStrItr.GetNextCursor: TJclWideStrBinaryNode;
var
  LastRet: TJclWideStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderWideStrItr.GetPreviousCursor: TJclWideStrBinaryNode;
var
  LastRet: TJclWideStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

//=== { TSingleItr } ===========================================================

type
  TSingleItr = class(TJclAbstractIterator, IJclSingleIterator)
  protected
    FCursor: TJclSingleBinaryNode;
    FOwnList: IJclSingleCollection;
    FEqualityComparer: IJclSingleEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclSingleBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclSingleBinaryNode; virtual; abstract;
    { IJclSingleIterator }
    function Add(const AValue: Single): Boolean;
    function GetValue: Single;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AValue: Single): Boolean;
    function Next: Single;
    function NextIndex: Integer;
    function Previous: Single;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetValue(const AValue: Single);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Single read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
  public
    constructor Create(const OwnList: IJclSingleCollection; Start: TJclSingleBinaryNode; AValid: Boolean);
  end;

constructor TSingleItr.Create(const OwnList: IJclSingleCollection; Start: TJclSingleBinaryNode; AValid: Boolean);
begin
  inherited Create(OwnList, AValid);
  FCursor := Start;
  FOwnList := OwnList;
  FEqualityComparer := FOwnList as IJclSingleEqualityComparer;
end;

function TSingleItr.Add(const AValue: Single): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TSingleItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TSingleItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TSingleItr then
  begin
    ADest := TSingleItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
  end;
end;

function TSingleItr.GetValue: Single;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  CheckValid;
  Result := 0.0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    Result := GetNextCursor <> nil
  else
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    Result := GetPreviousCursor <> nil
  else
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.Insert(const AValue: Single): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TSingleItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TSingleItr.Next: Single;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    FCursor := GetNextCursor
  else
    Valid := True;
  Result := 0.0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TSingleItr.Previous: Single;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    FCursor := GetPreviousCursor
  else
    Valid := True;
  Result := 0.0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TSingleItr.Remove;
var
  OldCursor: TJclSingleBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  CheckValid;
  Valid := False;
  OldCursor := FCursor;
  if OldCursor <> nil then
  begin
    repeat
      FCursor := GetNextCursor;
    until (FCursor = nil) or FOwnList.RemoveSingleElement
      or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
    FOwnList.Remove(OldCursor.Value);
  end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TSingleItr.SetValue(const AValue: Single);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderSingleItr } ===================================================

type
  TPreOrderSingleItr = class(TSingleItr, IJclSingleIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclSingleBinaryNode; override;
    function GetPreviousCursor: TJclSingleBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPreOrderSingleItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderSingleItr.Create(FOwnList, FCursor, Valid);
end;

function TPreOrderSingleItr.GetNextCursor: TJclSingleBinaryNode;
var
  LastRet: TJclSingleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderSingleItr.GetPreviousCursor: TJclSingleBinaryNode;
var
  LastRet: TJclSingleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil) then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderSingleItr } ====================================================

type
  TInOrderSingleItr = class(TSingleItr, IJclSingleIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclSingleBinaryNode; override;
    function GetPreviousCursor: TJclSingleBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TInOrderSingleItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderSingleItr.Create(FOwnList, FCursor, Valid);
end;

function TInOrderSingleItr.GetNextCursor: TJclSingleBinaryNode;
var
  LastRet: TJclSingleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderSingleItr.GetPreviousCursor: TJclSingleBinaryNode;
var
  LastRet: TJclSingleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderSingleItr } ==================================================

type
  TPostOrderSingleItr = class(TSingleItr, IJclSingleIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclSingleBinaryNode; override;
    function GetPreviousCursor: TJclSingleBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPostOrderSingleItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderSingleItr.Create(FOwnList, FCursor, Valid);
end;

function TPostOrderSingleItr.GetNextCursor: TJclSingleBinaryNode;
var
  LastRet: TJclSingleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderSingleItr.GetPreviousCursor: TJclSingleBinaryNode;
var
  LastRet: TJclSingleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

//=== { TDoubleItr } ===========================================================

type
  TDoubleItr = class(TJclAbstractIterator, IJclDoubleIterator)
  protected
    FCursor: TJclDoubleBinaryNode;
    FOwnList: IJclDoubleCollection;
    FEqualityComparer: IJclDoubleEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclDoubleBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclDoubleBinaryNode; virtual; abstract;
    { IJclDoubleIterator }
    function Add(const AValue: Double): Boolean;
    function GetValue: Double;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AValue: Double): Boolean;
    function Next: Double;
    function NextIndex: Integer;
    function Previous: Double;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetValue(const AValue: Double);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Double read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
  public
    constructor Create(const OwnList: IJclDoubleCollection; Start: TJclDoubleBinaryNode; AValid: Boolean);
  end;

constructor TDoubleItr.Create(const OwnList: IJclDoubleCollection; Start: TJclDoubleBinaryNode; AValid: Boolean);
begin
  inherited Create(OwnList, AValid);
  FCursor := Start;
  FOwnList := OwnList;
  FEqualityComparer := FOwnList as IJclDoubleEqualityComparer;
end;

function TDoubleItr.Add(const AValue: Double): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TDoubleItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TDoubleItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TDoubleItr then
  begin
    ADest := TDoubleItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
  end;
end;

function TDoubleItr.GetValue: Double;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  CheckValid;
  Result := 0.0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    Result := GetNextCursor <> nil
  else
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    Result := GetPreviousCursor <> nil
  else
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.Insert(const AValue: Double): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TDoubleItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TDoubleItr.Next: Double;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    FCursor := GetNextCursor
  else
    Valid := True;
  Result := 0.0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TDoubleItr.Previous: Double;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    FCursor := GetPreviousCursor
  else
    Valid := True;
  Result := 0.0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TDoubleItr.Remove;
var
  OldCursor: TJclDoubleBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  CheckValid;
  Valid := False;
  OldCursor := FCursor;
  if OldCursor <> nil then
  begin
    repeat
      FCursor := GetNextCursor;
    until (FCursor = nil) or FOwnList.RemoveSingleElement
      or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
    FOwnList.Remove(OldCursor.Value);
  end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TDoubleItr.SetValue(const AValue: Double);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderDoubleItr } ===================================================

type
  TPreOrderDoubleItr = class(TDoubleItr, IJclDoubleIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclDoubleBinaryNode; override;
    function GetPreviousCursor: TJclDoubleBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPreOrderDoubleItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderDoubleItr.Create(FOwnList, FCursor, Valid);
end;

function TPreOrderDoubleItr.GetNextCursor: TJclDoubleBinaryNode;
var
  LastRet: TJclDoubleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderDoubleItr.GetPreviousCursor: TJclDoubleBinaryNode;
var
  LastRet: TJclDoubleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil) then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderDoubleItr } ====================================================

type
  TInOrderDoubleItr = class(TDoubleItr, IJclDoubleIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclDoubleBinaryNode; override;
    function GetPreviousCursor: TJclDoubleBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TInOrderDoubleItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderDoubleItr.Create(FOwnList, FCursor, Valid);
end;

function TInOrderDoubleItr.GetNextCursor: TJclDoubleBinaryNode;
var
  LastRet: TJclDoubleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderDoubleItr.GetPreviousCursor: TJclDoubleBinaryNode;
var
  LastRet: TJclDoubleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderDoubleItr } ==================================================

type
  TPostOrderDoubleItr = class(TDoubleItr, IJclDoubleIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclDoubleBinaryNode; override;
    function GetPreviousCursor: TJclDoubleBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPostOrderDoubleItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderDoubleItr.Create(FOwnList, FCursor, Valid);
end;

function TPostOrderDoubleItr.GetNextCursor: TJclDoubleBinaryNode;
var
  LastRet: TJclDoubleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderDoubleItr.GetPreviousCursor: TJclDoubleBinaryNode;
var
  LastRet: TJclDoubleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

//=== { TExtendedItr } ===========================================================

type
  TExtendedItr = class(TJclAbstractIterator, IJclExtendedIterator)
  protected
    FCursor: TJclExtendedBinaryNode;
    FOwnList: IJclExtendedCollection;
    FEqualityComparer: IJclExtendedEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclExtendedBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclExtendedBinaryNode; virtual; abstract;
    { IJclExtendedIterator }
    function Add(const AValue: Extended): Boolean;
    function GetValue: Extended;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AValue: Extended): Boolean;
    function Next: Extended;
    function NextIndex: Integer;
    function Previous: Extended;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetValue(const AValue: Extended);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Extended read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
  public
    constructor Create(const OwnList: IJclExtendedCollection; Start: TJclExtendedBinaryNode; AValid: Boolean);
  end;

constructor TExtendedItr.Create(const OwnList: IJclExtendedCollection; Start: TJclExtendedBinaryNode; AValid: Boolean);
begin
  inherited Create(OwnList, AValid);
  FCursor := Start;
  FOwnList := OwnList;
  FEqualityComparer := FOwnList as IJclExtendedEqualityComparer;
end;

function TExtendedItr.Add(const AValue: Extended): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TExtendedItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TExtendedItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TExtendedItr then
  begin
    ADest := TExtendedItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
  end;
end;

function TExtendedItr.GetValue: Extended;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  CheckValid;
  Result := 0.0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    Result := GetNextCursor <> nil
  else
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    Result := GetPreviousCursor <> nil
  else
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.Insert(const AValue: Extended): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TExtendedItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TExtendedItr.Next: Extended;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    FCursor := GetNextCursor
  else
    Valid := True;
  Result := 0.0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TExtendedItr.Previous: Extended;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    FCursor := GetPreviousCursor
  else
    Valid := True;
  Result := 0.0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TExtendedItr.Remove;
var
  OldCursor: TJclExtendedBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  CheckValid;
  Valid := False;
  OldCursor := FCursor;
  if OldCursor <> nil then
  begin
    repeat
      FCursor := GetNextCursor;
    until (FCursor = nil) or FOwnList.RemoveSingleElement
      or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
    FOwnList.Remove(OldCursor.Value);
  end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TExtendedItr.SetValue(const AValue: Extended);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderExtendedItr } ===================================================

type
  TPreOrderExtendedItr = class(TExtendedItr, IJclExtendedIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclExtendedBinaryNode; override;
    function GetPreviousCursor: TJclExtendedBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPreOrderExtendedItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderExtendedItr.Create(FOwnList, FCursor, Valid);
end;

function TPreOrderExtendedItr.GetNextCursor: TJclExtendedBinaryNode;
var
  LastRet: TJclExtendedBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderExtendedItr.GetPreviousCursor: TJclExtendedBinaryNode;
var
  LastRet: TJclExtendedBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil) then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderExtendedItr } ====================================================

type
  TInOrderExtendedItr = class(TExtendedItr, IJclExtendedIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclExtendedBinaryNode; override;
    function GetPreviousCursor: TJclExtendedBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TInOrderExtendedItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderExtendedItr.Create(FOwnList, FCursor, Valid);
end;

function TInOrderExtendedItr.GetNextCursor: TJclExtendedBinaryNode;
var
  LastRet: TJclExtendedBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderExtendedItr.GetPreviousCursor: TJclExtendedBinaryNode;
var
  LastRet: TJclExtendedBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderExtendedItr } ==================================================

type
  TPostOrderExtendedItr = class(TExtendedItr, IJclExtendedIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclExtendedBinaryNode; override;
    function GetPreviousCursor: TJclExtendedBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPostOrderExtendedItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderExtendedItr.Create(FOwnList, FCursor, Valid);
end;

function TPostOrderExtendedItr.GetNextCursor: TJclExtendedBinaryNode;
var
  LastRet: TJclExtendedBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderExtendedItr.GetPreviousCursor: TJclExtendedBinaryNode;
var
  LastRet: TJclExtendedBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

//=== { TIntegerItr } ===========================================================

type
  TIntegerItr = class(TJclAbstractIterator, IJclIntegerIterator)
  protected
    FCursor: TJclIntegerBinaryNode;
    FOwnList: IJclIntegerCollection;
    FEqualityComparer: IJclIntegerEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclIntegerBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclIntegerBinaryNode; virtual; abstract;
    { IJclIntegerIterator }
    function Add(AValue: Integer): Boolean;
    function GetValue: Integer;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(AValue: Integer): Boolean;
    function Next: Integer;
    function NextIndex: Integer;
    function Previous: Integer;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetValue(AValue: Integer);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Integer read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
  public
    constructor Create(const OwnList: IJclIntegerCollection; Start: TJclIntegerBinaryNode; AValid: Boolean);
  end;

constructor TIntegerItr.Create(const OwnList: IJclIntegerCollection; Start: TJclIntegerBinaryNode; AValid: Boolean);
begin
  inherited Create(OwnList, AValid);
  FCursor := Start;
  FOwnList := OwnList;
  FEqualityComparer := FOwnList as IJclIntegerEqualityComparer;
end;

function TIntegerItr.Add(AValue: Integer): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TIntegerItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TIntegerItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TIntegerItr then
  begin
    ADest := TIntegerItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
  end;
end;

function TIntegerItr.GetValue: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  CheckValid;
  Result := 0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    Result := GetNextCursor <> nil
  else
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    Result := GetPreviousCursor <> nil
  else
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.Insert(AValue: Integer): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TIntegerItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TIntegerItr.Next: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    FCursor := GetNextCursor
  else
    Valid := True;
  Result := 0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TIntegerItr.Previous: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    FCursor := GetPreviousCursor
  else
    Valid := True;
  Result := 0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TIntegerItr.Remove;
var
  OldCursor: TJclIntegerBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  CheckValid;
  Valid := False;
  OldCursor := FCursor;
  if OldCursor <> nil then
  begin
    repeat
      FCursor := GetNextCursor;
    until (FCursor = nil) or FOwnList.RemoveSingleElement
      or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
    FOwnList.Remove(OldCursor.Value);
  end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TIntegerItr.SetValue(AValue: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderIntegerItr } ===================================================

type
  TPreOrderIntegerItr = class(TIntegerItr, IJclIntegerIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntegerBinaryNode; override;
    function GetPreviousCursor: TJclIntegerBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPreOrderIntegerItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderIntegerItr.Create(FOwnList, FCursor, Valid);
end;

function TPreOrderIntegerItr.GetNextCursor: TJclIntegerBinaryNode;
var
  LastRet: TJclIntegerBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderIntegerItr.GetPreviousCursor: TJclIntegerBinaryNode;
var
  LastRet: TJclIntegerBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil) then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderIntegerItr } ====================================================

type
  TInOrderIntegerItr = class(TIntegerItr, IJclIntegerIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntegerBinaryNode; override;
    function GetPreviousCursor: TJclIntegerBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TInOrderIntegerItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderIntegerItr.Create(FOwnList, FCursor, Valid);
end;

function TInOrderIntegerItr.GetNextCursor: TJclIntegerBinaryNode;
var
  LastRet: TJclIntegerBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderIntegerItr.GetPreviousCursor: TJclIntegerBinaryNode;
var
  LastRet: TJclIntegerBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderIntegerItr } ==================================================

type
  TPostOrderIntegerItr = class(TIntegerItr, IJclIntegerIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntegerBinaryNode; override;
    function GetPreviousCursor: TJclIntegerBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPostOrderIntegerItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderIntegerItr.Create(FOwnList, FCursor, Valid);
end;

function TPostOrderIntegerItr.GetNextCursor: TJclIntegerBinaryNode;
var
  LastRet: TJclIntegerBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderIntegerItr.GetPreviousCursor: TJclIntegerBinaryNode;
var
  LastRet: TJclIntegerBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

//=== { TCardinalItr } ===========================================================

type
  TCardinalItr = class(TJclAbstractIterator, IJclCardinalIterator)
  protected
    FCursor: TJclCardinalBinaryNode;
    FOwnList: IJclCardinalCollection;
    FEqualityComparer: IJclCardinalEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclCardinalBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclCardinalBinaryNode; virtual; abstract;
    { IJclCardinalIterator }
    function Add(AValue: Cardinal): Boolean;
    function GetValue: Cardinal;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(AValue: Cardinal): Boolean;
    function Next: Cardinal;
    function NextIndex: Integer;
    function Previous: Cardinal;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetValue(AValue: Cardinal);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Cardinal read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
  public
    constructor Create(const OwnList: IJclCardinalCollection; Start: TJclCardinalBinaryNode; AValid: Boolean);
  end;

constructor TCardinalItr.Create(const OwnList: IJclCardinalCollection; Start: TJclCardinalBinaryNode; AValid: Boolean);
begin
  inherited Create(OwnList, AValid);
  FCursor := Start;
  FOwnList := OwnList;
  FEqualityComparer := FOwnList as IJclCardinalEqualityComparer;
end;

function TCardinalItr.Add(AValue: Cardinal): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TCardinalItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TCardinalItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TCardinalItr then
  begin
    ADest := TCardinalItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
  end;
end;

function TCardinalItr.GetValue: Cardinal;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  CheckValid;
  Result := 0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    Result := GetNextCursor <> nil
  else
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    Result := GetPreviousCursor <> nil
  else
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.Insert(AValue: Cardinal): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TCardinalItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TCardinalItr.Next: Cardinal;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    FCursor := GetNextCursor
  else
    Valid := True;
  Result := 0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TCardinalItr.Previous: Cardinal;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    FCursor := GetPreviousCursor
  else
    Valid := True;
  Result := 0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TCardinalItr.Remove;
var
  OldCursor: TJclCardinalBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  CheckValid;
  Valid := False;
  OldCursor := FCursor;
  if OldCursor <> nil then
  begin
    repeat
      FCursor := GetNextCursor;
    until (FCursor = nil) or FOwnList.RemoveSingleElement
      or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
    FOwnList.Remove(OldCursor.Value);
  end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TCardinalItr.SetValue(AValue: Cardinal);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderCardinalItr } ===================================================

type
  TPreOrderCardinalItr = class(TCardinalItr, IJclCardinalIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclCardinalBinaryNode; override;
    function GetPreviousCursor: TJclCardinalBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPreOrderCardinalItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderCardinalItr.Create(FOwnList, FCursor, Valid);
end;

function TPreOrderCardinalItr.GetNextCursor: TJclCardinalBinaryNode;
var
  LastRet: TJclCardinalBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderCardinalItr.GetPreviousCursor: TJclCardinalBinaryNode;
var
  LastRet: TJclCardinalBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil) then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderCardinalItr } ====================================================

type
  TInOrderCardinalItr = class(TCardinalItr, IJclCardinalIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclCardinalBinaryNode; override;
    function GetPreviousCursor: TJclCardinalBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TInOrderCardinalItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderCardinalItr.Create(FOwnList, FCursor, Valid);
end;

function TInOrderCardinalItr.GetNextCursor: TJclCardinalBinaryNode;
var
  LastRet: TJclCardinalBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderCardinalItr.GetPreviousCursor: TJclCardinalBinaryNode;
var
  LastRet: TJclCardinalBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderCardinalItr } ==================================================

type
  TPostOrderCardinalItr = class(TCardinalItr, IJclCardinalIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclCardinalBinaryNode; override;
    function GetPreviousCursor: TJclCardinalBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPostOrderCardinalItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderCardinalItr.Create(FOwnList, FCursor, Valid);
end;

function TPostOrderCardinalItr.GetNextCursor: TJclCardinalBinaryNode;
var
  LastRet: TJclCardinalBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderCardinalItr.GetPreviousCursor: TJclCardinalBinaryNode;
var
  LastRet: TJclCardinalBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

//=== { TInt64Itr } ===========================================================

type
  TInt64Itr = class(TJclAbstractIterator, IJclInt64Iterator)
  protected
    FCursor: TJclInt64BinaryNode;
    FOwnList: IJclInt64Collection;
    FEqualityComparer: IJclInt64EqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclInt64BinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclInt64BinaryNode; virtual; abstract;
    { IJclInt64Iterator }
    function Add(const AValue: Int64): Boolean;
    function GetValue: Int64;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AValue: Int64): Boolean;
    function Next: Int64;
    function NextIndex: Integer;
    function Previous: Int64;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetValue(const AValue: Int64);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Int64 read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
  public
    constructor Create(const OwnList: IJclInt64Collection; Start: TJclInt64BinaryNode; AValid: Boolean);
  end;

constructor TInt64Itr.Create(const OwnList: IJclInt64Collection; Start: TJclInt64BinaryNode; AValid: Boolean);
begin
  inherited Create(OwnList, AValid);
  FCursor := Start;
  FOwnList := OwnList;
  FEqualityComparer := FOwnList as IJclInt64EqualityComparer;
end;

function TInt64Itr.Add(const AValue: Int64): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TInt64Itr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TInt64Itr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TInt64Itr then
  begin
    ADest := TInt64Itr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
  end;
end;

function TInt64Itr.GetValue: Int64;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  CheckValid;
  Result := 0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    Result := GetNextCursor <> nil
  else
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    Result := GetPreviousCursor <> nil
  else
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.Insert(const AValue: Int64): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TInt64Itr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TInt64Itr.Next: Int64;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    FCursor := GetNextCursor
  else
    Valid := True;
  Result := 0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TInt64Itr.Previous: Int64;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    FCursor := GetPreviousCursor
  else
    Valid := True;
  Result := 0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TInt64Itr.Remove;
var
  OldCursor: TJclInt64BinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  CheckValid;
  Valid := False;
  OldCursor := FCursor;
  if OldCursor <> nil then
  begin
    repeat
      FCursor := GetNextCursor;
    until (FCursor = nil) or FOwnList.RemoveSingleElement
      or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
    FOwnList.Remove(OldCursor.Value);
  end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TInt64Itr.SetValue(const AValue: Int64);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderInt64Itr } ===================================================

type
  TPreOrderInt64Itr = class(TInt64Itr, IJclInt64Iterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclInt64BinaryNode; override;
    function GetPreviousCursor: TJclInt64BinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPreOrderInt64Itr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderInt64Itr.Create(FOwnList, FCursor, Valid);
end;

function TPreOrderInt64Itr.GetNextCursor: TJclInt64BinaryNode;
var
  LastRet: TJclInt64BinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderInt64Itr.GetPreviousCursor: TJclInt64BinaryNode;
var
  LastRet: TJclInt64BinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil) then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderInt64Itr } ====================================================

type
  TInOrderInt64Itr = class(TInt64Itr, IJclInt64Iterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclInt64BinaryNode; override;
    function GetPreviousCursor: TJclInt64BinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TInOrderInt64Itr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderInt64Itr.Create(FOwnList, FCursor, Valid);
end;

function TInOrderInt64Itr.GetNextCursor: TJclInt64BinaryNode;
var
  LastRet: TJclInt64BinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderInt64Itr.GetPreviousCursor: TJclInt64BinaryNode;
var
  LastRet: TJclInt64BinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderInt64Itr } ==================================================

type
  TPostOrderInt64Itr = class(TInt64Itr, IJclInt64Iterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclInt64BinaryNode; override;
    function GetPreviousCursor: TJclInt64BinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPostOrderInt64Itr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderInt64Itr.Create(FOwnList, FCursor, Valid);
end;

function TPostOrderInt64Itr.GetNextCursor: TJclInt64BinaryNode;
var
  LastRet: TJclInt64BinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderInt64Itr.GetPreviousCursor: TJclInt64BinaryNode;
var
  LastRet: TJclInt64BinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;
{$IFNDEF CLR}

//=== { TPtrItr } ===========================================================

type
  TPtrItr = class(TJclAbstractIterator, IJclPtrIterator)
  protected
    FCursor: TJclPtrBinaryNode;
    FOwnList: IJclPtrCollection;
    FEqualityComparer: IJclPtrEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclPtrBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclPtrBinaryNode; virtual; abstract;
    { IJclPtrIterator }
    function Add(APtr: Pointer): Boolean;
    function GetPtr: Pointer;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(APtr: Pointer): Boolean;
    function Next: Pointer;
    function NextIndex: Integer;
    function Previous: Pointer;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetPtr(APtr: Pointer);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Pointer read GetPtr;
    {$ENDIF SUPPORTS_FOR_IN}
  public
    constructor Create(const OwnList: IJclPtrCollection; Start: TJclPtrBinaryNode; AValid: Boolean);
  end;

constructor TPtrItr.Create(const OwnList: IJclPtrCollection; Start: TJclPtrBinaryNode; AValid: Boolean);
begin
  inherited Create(OwnList, AValid);
  FCursor := Start;
  FOwnList := OwnList;
  FEqualityComparer := FOwnList as IJclPtrEqualityComparer;
end;

function TPtrItr.Add(APtr: Pointer): Boolean;
begin
  Result := FOwnList.Add(APtr);
end;

procedure TPtrItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TPtrItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TPtrItr then
  begin
    ADest := TPtrItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
  end;
end;

function TPtrItr.GetPtr: Pointer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  CheckValid;
  Result := nil;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    Result := GetNextCursor <> nil
  else
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    Result := GetPreviousCursor <> nil
  else
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.Insert(APtr: Pointer): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TPtrItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TPtrItr.Next: Pointer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    FCursor := GetNextCursor
  else
    Valid := True;
  Result := nil;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TPtrItr.Previous: Pointer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    FCursor := GetPreviousCursor
  else
    Valid := True;
  Result := nil;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TPtrItr.Remove;
var
  OldCursor: TJclPtrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  CheckValid;
  Valid := False;
  OldCursor := FCursor;
  if OldCursor <> nil then
  begin
    repeat
      FCursor := GetNextCursor;
    until (FCursor = nil) or FOwnList.RemoveSingleElement
      or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
    FOwnList.Remove(OldCursor.Value);
  end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TPtrItr.SetPtr(APtr: Pointer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderPtrItr } ===================================================

type
  TPreOrderPtrItr = class(TPtrItr, IJclPtrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclPtrBinaryNode; override;
    function GetPreviousCursor: TJclPtrBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPreOrderPtrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderPtrItr.Create(FOwnList, FCursor, Valid);
end;

function TPreOrderPtrItr.GetNextCursor: TJclPtrBinaryNode;
var
  LastRet: TJclPtrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderPtrItr.GetPreviousCursor: TJclPtrBinaryNode;
var
  LastRet: TJclPtrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil) then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderPtrItr } ====================================================

type
  TInOrderPtrItr = class(TPtrItr, IJclPtrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclPtrBinaryNode; override;
    function GetPreviousCursor: TJclPtrBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TInOrderPtrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderPtrItr.Create(FOwnList, FCursor, Valid);
end;

function TInOrderPtrItr.GetNextCursor: TJclPtrBinaryNode;
var
  LastRet: TJclPtrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderPtrItr.GetPreviousCursor: TJclPtrBinaryNode;
var
  LastRet: TJclPtrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderPtrItr } ==================================================

type
  TPostOrderPtrItr = class(TPtrItr, IJclPtrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclPtrBinaryNode; override;
    function GetPreviousCursor: TJclPtrBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPostOrderPtrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderPtrItr.Create(FOwnList, FCursor, Valid);
end;

function TPostOrderPtrItr.GetNextCursor: TJclPtrBinaryNode;
var
  LastRet: TJclPtrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderPtrItr.GetPreviousCursor: TJclPtrBinaryNode;
var
  LastRet: TJclPtrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;
{$ENDIF ~CLR}

//=== { TItr } ===========================================================

type
  TItr = class(TJclAbstractIterator, IJclIterator)
  protected
    FCursor: TJclBinaryNode;
    FOwnList: IJclCollection;
    FEqualityComparer: IJclEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclBinaryNode; virtual; abstract;
    { IJclIterator }
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
  public
    constructor Create(const OwnList: IJclCollection; Start: TJclBinaryNode; AValid: Boolean);
  end;

constructor TItr.Create(const OwnList: IJclCollection; Start: TJclBinaryNode; AValid: Boolean);
begin
  inherited Create(OwnList, AValid);
  FCursor := Start;
  FOwnList := OwnList;
  FEqualityComparer := FOwnList as IJclEqualityComparer;
end;

function TItr.Add(AObject: TObject): Boolean;
begin
  Result := FOwnList.Add(AObject);
end;

procedure TItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TItr then
  begin
    ADest := TItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
  end;
end;

function TItr.GetObject: TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  CheckValid;
  Result := nil;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    Result := GetNextCursor <> nil
  else
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    Result := GetPreviousCursor <> nil
  else
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.Insert(AObject: TObject): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TItr.Next: TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    FCursor := GetNextCursor
  else
    Valid := True;
  Result := nil;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TItr.Previous: TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  if Valid then
    FCursor := GetPreviousCursor
  else
    Valid := True;
  Result := nil;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TItr.Remove;
var
  OldCursor: TJclBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  CheckValid;
  Valid := False;
  OldCursor := FCursor;
  if OldCursor <> nil then
  begin
    repeat
      FCursor := GetNextCursor;
    until (FCursor = nil) or FOwnList.RemoveSingleElement
      or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
    FOwnList.Remove(OldCursor.Value);
  end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr.SetObject(AObject: TObject);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderItr } ===================================================

type
  TPreOrderItr = class(TItr, IJclIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclBinaryNode; override;
    function GetPreviousCursor: TJclBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPreOrderItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderItr.Create(FOwnList, FCursor, Valid);
end;

function TPreOrderItr.GetNextCursor: TJclBinaryNode;
var
  LastRet: TJclBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderItr.GetPreviousCursor: TJclBinaryNode;
var
  LastRet: TJclBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil) then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderItr } ====================================================

type
  TInOrderItr = class(TItr, IJclIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclBinaryNode; override;
    function GetPreviousCursor: TJclBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TInOrderItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderItr.Create(FOwnList, FCursor, Valid);
end;

function TInOrderItr.GetNextCursor: TJclBinaryNode;
var
  LastRet: TJclBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderItr.GetPreviousCursor: TJclBinaryNode;
var
  LastRet: TJclBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderItr } ==================================================

type
  TPostOrderItr = class(TItr, IJclIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclBinaryNode; override;
    function GetPreviousCursor: TJclBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPostOrderItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderItr.Create(FOwnList, FCursor, Valid);
end;

function TPostOrderItr.GetNextCursor: TJclBinaryNode;
var
  LastRet: TJclBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderItr.GetPreviousCursor: TJclBinaryNode;
var
  LastRet: TJclBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;
{$IFDEF SUPPORTS_GENERICS}

//=== { TItr<T> } ===========================================================

type
  TItr<T> = class(TJclAbstractIterator, IJclIterator<T>)
  protected
    FCursor: TJclBinaryNode<T>;
    FOwnList: IJclCollection<T>;
    FEqualityComparer: IJclEqualityComparer<T>;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclBinaryNode<T>; virtual; abstract;
    function GetPreviousCursor: TJclBinaryNode<T>; virtual; abstract;
    { IJclIterator<T> }
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
  public
    constructor Create(const OwnList: IJclCollection<T>; Start: TJclBinaryNode<T>; AValid: Boolean);
  end;

constructor TItr<T>.Create(const OwnList: IJclCollection<T>; Start: TJclBinaryNode<T>; AValid: Boolean);
begin
  inherited Create(OwnList, AValid);
  FCursor := Start;
  FOwnList := OwnList;
  FEqualityComparer := FOwnList as IJclEqualityComparer<T>;
end;

function TItr<T>.Add(const AItem: T): Boolean;
begin
  Result := FOwnList.Add(AItem);
end;

procedure TItr<T>.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TItr<T>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TItr<T> then
  begin
    ADest := TItr<T>(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
  end;
end;

function TItr<T>.GetItem: T;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := Default(T);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnList.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.Insert(const AItem: T): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TItr<T>.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TItr<T>.Next: T;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := Default(T);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnList.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TItr<T>.Previous: T;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := Default(T);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnList.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TItr<T>.Remove;
var
  OldCursor: TJclBinaryNode<T>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnList.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnList.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr<T>.SetItem(const AItem: T);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderItr<T> } ===================================================

type
  TPreOrderItr<T> = class(TItr<T>, IJclIterator<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclBinaryNode<T>; override;
    function GetPreviousCursor: TJclBinaryNode<T>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPreOrderItr<T>.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderItr<T>.Create(FOwnList, FCursor, Valid);
end;

function TPreOrderItr<T>.GetNextCursor: TJclBinaryNode<T>;
var
  LastRet: TJclBinaryNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderItr<T>.GetPreviousCursor: TJclBinaryNode<T>;
var
  LastRet: TJclBinaryNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderItr<T> } ====================================================

type
  TInOrderItr<T> = class(TItr<T>, IJclIterator<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclBinaryNode<T>; override;
    function GetPreviousCursor: TJclBinaryNode<T>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TInOrderItr<T>.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderItr<T>.Create(FOwnList, FCursor, Valid);
end;

function TInOrderItr<T>.GetNextCursor: TJclBinaryNode<T>;
var
  LastRet: TJclBinaryNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderItr<T>.GetPreviousCursor: TJclBinaryNode<T>;
var
  LastRet: TJclBinaryNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderItr<T> } ==================================================

type
  TPostOrderItr<T> = class(TItr<T>, IJclIterator<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclBinaryNode<T>; override;
    function GetPreviousCursor: TJclBinaryNode<T>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPostOrderItr<T>.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderItr<T>.Create(FOwnList, FCursor, Valid);
end;

function TPostOrderItr<T>.GetNextCursor: TJclBinaryNode<T>;
var
  LastRet: TJclBinaryNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderItr<T>.GetPreviousCursor: TJclBinaryNode<T>;
var
  LastRet: TJclBinaryNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;
{$ENDIF SUPPORTS_GENERICS}


//=== { TJclIntfBinaryTree } =================================================

constructor TJclIntfBinaryTree.Create(ACompare: TIntfCompare);
begin
  inherited Create(nil);
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  FCompare := ACompare;
end;

destructor TJclIntfBinaryTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclIntfBinaryTree.Add(const AInterface: IInterface): Boolean;
var
  NewNode, Current, Save: TJclIntfBinaryNode;
  Comp, Depth: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
  if FAllowDefaultElements or not ItemsEqual(AInterface, nil) then
  begin
    Save := nil;
    Current := FRoot;
    Comp := 1;
    Depth := 0;
    while Current <> nil do
    begin
      Inc(Depth);
      Save := Current;
      Comp := ItemsCompare(AInterface, Current.Value);
      if Comp < 0 then
        Current := Current.Left
      else
      if Comp > 0 then
        Current := Current.Right
      else
        Break;
    end;
    if (Comp <> 0) or CheckDuplicate then
    begin
      NewNode := TJclIntfBinaryNode.Create;
      NewNode.Value := AInterface;
      NewNode.Parent := Save;
      if Save = nil then
        FRoot := NewNode
      else
      if ItemsCompare(NewNode.Value, Save.Value) < 0 then
        Save.Left := NewNode
      else
        Save.Right := NewNode;
      Inc(FSize);
      Inc(Depth);
      if Depth > FMaxDepth then
        FMaxDepth := Depth;
      Result := True;
      AutoPack;
    end
    else
      Result := False;
  end
  else
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.AddAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclIntfBinaryNode): TJclIntfBinaryNode;
  begin
    Result := TJclIntfBinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclIntfBinaryTree;
  ACollection: IJclIntfCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfBinaryTree then
  begin
    ADest := TJclIntfBinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclIntfCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclIntfBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfBinaryTree then
    TJclIntfBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclIntfBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclIntfBinaryTree.Clear;
var
  Current, Parent: TJclIntfBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // postorder
  Current := FRoot;
  if Current = nil then
    Exit;
    // find first in post-order
  while (Current.Left <> nil) or (Current.Right <> nil) do
  begin
    if Current.Left <> nil then
      Current := Current.Left
    else
      Current := Current.Right;
  end;
    // for all items in the tree in post-order
  repeat
    Parent := Current.Parent;
      // remove reference
    if Parent <> nil then
    begin
      if Parent.Left = Current then
        Parent.Left := nil
      else
      if Parent.Right = Current then
        Parent.Right := nil;
    end;

      // free item
    FreeObject(Current.Value);
    Current.Free;

      // find next item
    Current := Parent;
    if (Current <> nil) and (Current.Right <> nil) then
    begin
      Current := Current.Right;
      while (Current.Left <> nil) or (Current.Right <> nil) do
      begin
        if Current.Left <> nil then
          Current := Current.Left
        else
          Current := Current.Right;
      end;
    end;
  until Current = nil;
  FRoot := nil;
  FSize := 0;
  FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.Contains(const AInterface: IInterface): Boolean;
var
  Comp: Integer;
  Current: TJclIntfBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  Current := FRoot;
  while Current <> nil do
  begin
    Comp := ItemsCompare(Current.Value, AInterface);
    if Comp = 0 then
    begin
      Result := True;
      Break;
    end
    else
    if Comp > 0 then
      Current := Current.Left
    else
      Current := Current.Right;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.ContainsAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while Result and It.HasNext do
    Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfBinaryTree.Create(FCompare);
  AssignPropertiesTo(Result);
end;

function TJclIntfBinaryTree.Equals(const ACollection: IJclIntfCollection): Boolean;
var
  It, ItSelf: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  if FSize <> ACollection.Size then
    Exit;
  Result := True;
  It := ACollection.First;
  ItSelf := First;
  while ItSelf.HasNext do
    if not ItemsEqual(ItSelf.Next, It.Next) then
    begin
      Result := False;
      Break;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.First: IJclIntfIterator;
var
  Start: TJclIntfBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Start := FRoot;
  case GetTraverseOrder of
    toPreOrder:
      Result := TPreOrderIntfItr.Create(Self, Start, False);
    toOrder:
    begin
      if Start <> nil then
        while Start.Left <> nil do
          Start := Start.Left;
      Result := TInOrderIntfItr.Create(Self, Start, False);
    end;
    toPostOrder:
    begin
      if Start <> nil then
        while (Start.Left <> nil) or (Start.Right <> nil) do
        begin
          if Start.Left <> nil then
            Start := Start.Left
          else
            Start := Start.Right;
        end;
      Result := TPostOrderIntfItr.Create(Self, Start, False);
    end;
  else
    Result := nil;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntfBinaryTree.GetEnumerator: IJclIntfIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntfBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclIntfBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfBinaryTree.ItemsCompare(const A, B: IInterface): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;

function TJclIntfBinaryTree.ItemsEqual(const A, B: IInterface): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;

function TJclIntfBinaryTree.Last: IJclIntfIterator;
var
  Start: TJclIntfBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Start := FRoot;
  case FTraverseOrder of
    toPreOrder:
    begin
      if Start <> nil then
        while (Start.Left <> nil) or (Start.Right <> nil) do
        begin
          if Start.Right <> nil then
            Start := Start.Right
          else
            Start := Start.Left;
        end;
      Result := TPreOrderIntfItr.Create(Self, Start, False);
    end;
    toOrder:
    begin
      if Start <> nil then
        while Start.Right <> nil do
          Start := Start.Right;
      Result := TInOrderIntfItr.Create(Self, Start, False);
    end;
    toPostOrder:
      Result := TPostOrderIntfItr.Create(Self, Start, False);
  else
    Result := nil;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfBinaryTree.Pack;
type
  TLeafArray = array of TJclIntfBinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclIntfBinaryNode;
    Offset: Integer): TJclIntfBinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclIntfBinaryNode;
  Index:     Integer;
begin
  SetLength(Leafarray, FSize);
  try
    // in order enumeration of nodes
    ANode := FRoot;
    if ANode <> nil then
    begin
      // find first node
      while ANode.Left <> nil do
        ANode := ANode.Left;

      Index := 0;
      while ANode <> nil do
      begin
        LeafArray[Index] := ANode;
        Inc(Index);
        if ANode.Right <> nil then
        begin
          ANode := ANode.Right;
          while (ANode.Left <> nil) do
            ANode := ANode.Left;
        end
        else
        begin
          BNode := ANode;
          ANode := ANode.Parent;
          while (ANode <> nil) and (ANode.Right = BNode) do
          begin
            BNode := ANode;
            ANode := ANode.Parent;
          end;
        end;
      end;

      Index := FSize shr 1;
      FRoot := LeafArray[Index];
      FRoot.Parent := nil;
      if Index > 0 then
        FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
      else
        FRoot.Left := nil;
      if Index < (FSize - 1) then
        FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
      else
        FRoot.Right := nil;
    end;
  finally
    SetLength(LeafArray, 0);
  end;
end;

function TJclIntfBinaryTree.Remove(const AInterface: IInterface): Boolean;
var
  Current, Successor: TJclIntfBinaryNode;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
    // locate AInterface in the tree
  Current := FRoot;
  repeat
    while Current <> nil do
    begin
      Comp := ItemsCompare(AInterface, Current.Value);
      if Comp = 0 then
        Break
      else
      if Comp < 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    if Current = nil then
      Break;
    Result := True;
      // Remove Current from tree
    if (Current.Left = nil) and (Current.Right <> nil) then
    begin
        // remove references to Current
      Current.Right.Parent := Current.Parent;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Current.Right
        else
          Current.Parent.Right := Current.Right;
      end
      else
          // fix root
        FRoot := Current.Right;
      Successor := Current.Parent;
      if Successor = nil then
        Successor := FRoot;
    end
    else
    if (Current.Left <> nil) and (Current.Right = nil) then
    begin
        // remove references to Current
      Current.Left.Parent := Current.Parent;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Current.Left
        else
          Current.Parent.Right := Current.Left;
      end
      else
          // fix root
        FRoot := Current.Left;
      Successor := Current.Parent;
      if Successor = nil then
        Successor := FRoot;
    end
    else
    if (Current.Left <> nil) and (Current.Right <> nil) then
    begin
        // find the successor in tree
      Successor := Current.Right;
      while Successor.Left <> nil do
        Successor := Successor.Left;

      if Successor <> Current.Right then
      begin
          // remove references to successor
        if Successor.Parent.Left = Successor then
          Successor.Parent.Left := Successor.Right
        else
          Successor.Parent.Right := Successor.Right;
        if Successor.Right <> nil then
          Successor.Right.Parent := Successor.Parent;
        Successor.Right := Current.Right;
      end;

        // insert successor in new position
      Successor.Parent := Current.Parent;
      Successor.Left := Current.Left;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Successor
        else
          Current.Parent.Right := Successor;
      end
      else
          // fix root
        FRoot := Successor;
      Successor := Current.Parent;
      if Successor <> nil then
        Successor := FRoot;
    end
    else
    begin
        // (Current.Left = nil) and (Current.Right = nil)
      Successor := Current.Parent;
      if Successor <> nil then
      begin
          // remove references from parent
        if Successor.Left = Current then
          Successor.Left := nil
        else
          Successor.Right := nil;
      end
      else
        FRoot := nil;
    end;
    FreeObject(Current.Value);
    Current.Free;
    Dec(FSize);
    Current := Successor;
  until FRemoveSingleElement or (Current = nil);
  AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.RemoveAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.RetainAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntfBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclIntfBinaryTree.Size: Integer;
begin
  Result := FSize;
end;


//=== { TJclAnsiStrBinaryTree } =================================================

constructor TJclAnsiStrBinaryTree.Create(ACompare: TAnsiStrCompare);
begin
  inherited Create(nil);
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  FCompare := ACompare;
end;

destructor TJclAnsiStrBinaryTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclAnsiStrBinaryTree.Add(const AString: AnsiString): Boolean;
var
  NewNode, Current, Save: TJclAnsiStrBinaryNode;
  Comp, Depth: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
  if FAllowDefaultElements or not ItemsEqual(AString, '') then
  begin
    Save := nil;
    Current := FRoot;
    Comp := 1;
    Depth := 0;
    while Current <> nil do
    begin
      Inc(Depth);
      Save := Current;
      Comp := ItemsCompare(AString, Current.Value);
      if Comp < 0 then
        Current := Current.Left
      else
      if Comp > 0 then
        Current := Current.Right
      else
        Break;
    end;
    if (Comp <> 0) or CheckDuplicate then
    begin
      NewNode := TJclAnsiStrBinaryNode.Create;
      NewNode.Value := AString;
      NewNode.Parent := Save;
      if Save = nil then
        FRoot := NewNode
      else
      if ItemsCompare(NewNode.Value, Save.Value) < 0 then
        Save.Left := NewNode
      else
        Save.Right := NewNode;
      Inc(FSize);
      Inc(Depth);
      if Depth > FMaxDepth then
        FMaxDepth := Depth;
      Result := True;
      AutoPack;
    end
    else
      Result := False;
  end
  else
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.AddAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclAnsiStrBinaryNode): TJclAnsiStrBinaryNode;
  begin
    Result := TJclAnsiStrBinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclAnsiStrBinaryTree;
  ACollection: IJclAnsiStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclAnsiStrBinaryTree then
  begin
    ADest := TJclAnsiStrBinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclAnsiStrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclAnsiStrBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclAnsiStrBinaryTree then
    TJclAnsiStrBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclAnsiStrBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclAnsiStrBinaryTree.Clear;
var
  Current, Parent: TJclAnsiStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // postorder
  Current := FRoot;
  if Current = nil then
    Exit;
    // find first in post-order
  while (Current.Left <> nil) or (Current.Right <> nil) do
  begin
    if Current.Left <> nil then
      Current := Current.Left
    else
      Current := Current.Right;
  end;
    // for all items in the tree in post-order
  repeat
    Parent := Current.Parent;
      // remove reference
    if Parent <> nil then
    begin
      if Parent.Left = Current then
        Parent.Left := nil
      else
      if Parent.Right = Current then
        Parent.Right := nil;
    end;

      // free item
    FreeString(Current.Value);
    Current.Free;

      // find next item
    Current := Parent;
    if (Current <> nil) and (Current.Right <> nil) then
    begin
      Current := Current.Right;
      while (Current.Left <> nil) or (Current.Right <> nil) do
      begin
        if Current.Left <> nil then
          Current := Current.Left
        else
          Current := Current.Right;
      end;
    end;
  until Current = nil;
  FRoot := nil;
  FSize := 0;
  FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.Contains(const AString: AnsiString): Boolean;
var
  Comp: Integer;
  Current: TJclAnsiStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  Current := FRoot;
  while Current <> nil do
  begin
    Comp := ItemsCompare(Current.Value, AString);
    if Comp = 0 then
    begin
      Result := True;
      Break;
    end
    else
    if Comp > 0 then
      Current := Current.Left
    else
      Current := Current.Right;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while Result and It.HasNext do
    Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrBinaryTree.Create(FCompare);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrBinaryTree.Equals(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It, ItSelf: IJclAnsiStrIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  if FSize <> ACollection.Size then
    Exit;
  Result := True;
  It := ACollection.First;
  ItSelf := First;
  while ItSelf.HasNext do
    if not ItemsEqual(ItSelf.Next, It.Next) then
    begin
      Result := False;
      Break;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.First: IJclAnsiStrIterator;
var
  Start: TJclAnsiStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Start := FRoot;
  case GetTraverseOrder of
    toPreOrder:
      Result := TPreOrderAnsiStrItr.Create(Self, Start, False);
    toOrder:
    begin
      if Start <> nil then
        while Start.Left <> nil do
          Start := Start.Left;
      Result := TInOrderAnsiStrItr.Create(Self, Start, False);
    end;
    toPostOrder:
    begin
      if Start <> nil then
        while (Start.Left <> nil) or (Start.Right <> nil) do
        begin
          if Start.Left <> nil then
            Start := Start.Left
          else
            Start := Start.Right;
        end;
      Result := TPostOrderAnsiStrItr.Create(Self, Start, False);
    end;
  else
    Result := nil;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclAnsiStrBinaryTree.GetEnumerator: IJclAnsiStrIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclAnsiStrBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclAnsiStrBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclAnsiStrBinaryTree.ItemsCompare(const A, B: AnsiString): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;

function TJclAnsiStrBinaryTree.ItemsEqual(const A, B: AnsiString): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;

function TJclAnsiStrBinaryTree.Last: IJclAnsiStrIterator;
var
  Start: TJclAnsiStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Start := FRoot;
  case FTraverseOrder of
    toPreOrder:
    begin
      if Start <> nil then
        while (Start.Left <> nil) or (Start.Right <> nil) do
        begin
          if Start.Right <> nil then
            Start := Start.Right
          else
            Start := Start.Left;
        end;
      Result := TPreOrderAnsiStrItr.Create(Self, Start, False);
    end;
    toOrder:
    begin
      if Start <> nil then
        while Start.Right <> nil do
          Start := Start.Right;
      Result := TInOrderAnsiStrItr.Create(Self, Start, False);
    end;
    toPostOrder:
      Result := TPostOrderAnsiStrItr.Create(Self, Start, False);
  else
    Result := nil;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrBinaryTree.Pack;
type
  TLeafArray = array of TJclAnsiStrBinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclAnsiStrBinaryNode;
    Offset: Integer): TJclAnsiStrBinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclAnsiStrBinaryNode;
  Index:     Integer;
begin
  SetLength(Leafarray, FSize);
  try
    // in order enumeration of nodes
    ANode := FRoot;
    if ANode <> nil then
    begin
      // find first node
      while ANode.Left <> nil do
        ANode := ANode.Left;

      Index := 0;
      while ANode <> nil do
      begin
        LeafArray[Index] := ANode;
        Inc(Index);
        if ANode.Right <> nil then
        begin
          ANode := ANode.Right;
          while (ANode.Left <> nil) do
            ANode := ANode.Left;
        end
        else
        begin
          BNode := ANode;
          ANode := ANode.Parent;
          while (ANode <> nil) and (ANode.Right = BNode) do
          begin
            BNode := ANode;
            ANode := ANode.Parent;
          end;
        end;
      end;

      Index := FSize shr 1;
      FRoot := LeafArray[Index];
      FRoot.Parent := nil;
      if Index > 0 then
        FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
      else
        FRoot.Left := nil;
      if Index < (FSize - 1) then
        FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
      else
        FRoot.Right := nil;
    end;
  finally
    SetLength(LeafArray, 0);
  end;
end;

function TJclAnsiStrBinaryTree.Remove(const AString: AnsiString): Boolean;
var
  Current, Successor: TJclAnsiStrBinaryNode;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
    // locate AString in the tree
  Current := FRoot;
  repeat
    while Current <> nil do
    begin
      Comp := ItemsCompare(AString, Current.Value);
      if Comp = 0 then
        Break
      else
      if Comp < 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    if Current = nil then
      Break;
    Result := True;
      // Remove Current from tree
    if (Current.Left = nil) and (Current.Right <> nil) then
    begin
        // remove references to Current
      Current.Right.Parent := Current.Parent;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Current.Right
        else
          Current.Parent.Right := Current.Right;
      end
      else
          // fix root
        FRoot := Current.Right;
      Successor := Current.Parent;
      if Successor = nil then
        Successor := FRoot;
    end
    else
    if (Current.Left <> nil) and (Current.Right = nil) then
    begin
        // remove references to Current
      Current.Left.Parent := Current.Parent;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Current.Left
        else
          Current.Parent.Right := Current.Left;
      end
      else
          // fix root
        FRoot := Current.Left;
      Successor := Current.Parent;
      if Successor = nil then
        Successor := FRoot;
    end
    else
    if (Current.Left <> nil) and (Current.Right <> nil) then
    begin
        // find the successor in tree
      Successor := Current.Right;
      while Successor.Left <> nil do
        Successor := Successor.Left;

      if Successor <> Current.Right then
      begin
          // remove references to successor
        if Successor.Parent.Left = Successor then
          Successor.Parent.Left := Successor.Right
        else
          Successor.Parent.Right := Successor.Right;
        if Successor.Right <> nil then
          Successor.Right.Parent := Successor.Parent;
        Successor.Right := Current.Right;
      end;

        // insert successor in new position
      Successor.Parent := Current.Parent;
      Successor.Left := Current.Left;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Successor
        else
          Current.Parent.Right := Successor;
      end
      else
          // fix root
        FRoot := Successor;
      Successor := Current.Parent;
      if Successor <> nil then
        Successor := FRoot;
    end
    else
    begin
        // (Current.Left = nil) and (Current.Right = nil)
      Successor := Current.Parent;
      if Successor <> nil then
      begin
          // remove references from parent
        if Successor.Left = Current then
          Successor.Left := nil
        else
          Successor.Right := nil;
      end
      else
        FRoot := nil;
    end;
    FreeString(Current.Value);
    Current.Free;
    Dec(FSize);
    Current := Successor;
  until FRemoveSingleElement or (Current = nil);
  AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.RetainAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclAnsiStrBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclAnsiStrBinaryTree.Size: Integer;
begin
  Result := FSize;
end;


//=== { TJclWideStrBinaryTree } =================================================

constructor TJclWideStrBinaryTree.Create(ACompare: TWideStrCompare);
begin
  inherited Create(nil);
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  FCompare := ACompare;
end;

destructor TJclWideStrBinaryTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclWideStrBinaryTree.Add(const AString: WideString): Boolean;
var
  NewNode, Current, Save: TJclWideStrBinaryNode;
  Comp, Depth: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
  if FAllowDefaultElements or not ItemsEqual(AString, '') then
  begin
    Save := nil;
    Current := FRoot;
    Comp := 1;
    Depth := 0;
    while Current <> nil do
    begin
      Inc(Depth);
      Save := Current;
      Comp := ItemsCompare(AString, Current.Value);
      if Comp < 0 then
        Current := Current.Left
      else
      if Comp > 0 then
        Current := Current.Right
      else
        Break;
    end;
    if (Comp <> 0) or CheckDuplicate then
    begin
      NewNode := TJclWideStrBinaryNode.Create;
      NewNode.Value := AString;
      NewNode.Parent := Save;
      if Save = nil then
        FRoot := NewNode
      else
      if ItemsCompare(NewNode.Value, Save.Value) < 0 then
        Save.Left := NewNode
      else
        Save.Right := NewNode;
      Inc(FSize);
      Inc(Depth);
      if Depth > FMaxDepth then
        FMaxDepth := Depth;
      Result := True;
      AutoPack;
    end
    else
      Result := False;
  end
  else
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.AddAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclWideStrBinaryNode): TJclWideStrBinaryNode;
  begin
    Result := TJclWideStrBinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclWideStrBinaryTree;
  ACollection: IJclWideStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclWideStrBinaryTree then
  begin
    ADest := TJclWideStrBinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclWideStrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclWideStrBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclWideStrBinaryTree then
    TJclWideStrBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclWideStrBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclWideStrBinaryTree.Clear;
var
  Current, Parent: TJclWideStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // postorder
  Current := FRoot;
  if Current = nil then
    Exit;
    // find first in post-order
  while (Current.Left <> nil) or (Current.Right <> nil) do
  begin
    if Current.Left <> nil then
      Current := Current.Left
    else
      Current := Current.Right;
  end;
    // for all items in the tree in post-order
  repeat
    Parent := Current.Parent;
      // remove reference
    if Parent <> nil then
    begin
      if Parent.Left = Current then
        Parent.Left := nil
      else
      if Parent.Right = Current then
        Parent.Right := nil;
    end;

      // free item
    FreeString(Current.Value);
    Current.Free;

      // find next item
    Current := Parent;
    if (Current <> nil) and (Current.Right <> nil) then
    begin
      Current := Current.Right;
      while (Current.Left <> nil) or (Current.Right <> nil) do
      begin
        if Current.Left <> nil then
          Current := Current.Left
        else
          Current := Current.Right;
      end;
    end;
  until Current = nil;
  FRoot := nil;
  FSize := 0;
  FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.Contains(const AString: WideString): Boolean;
var
  Comp: Integer;
  Current: TJclWideStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  Current := FRoot;
  while Current <> nil do
  begin
    Comp := ItemsCompare(Current.Value, AString);
    if Comp = 0 then
    begin
      Result := True;
      Break;
    end
    else
    if Comp > 0 then
      Current := Current.Left
    else
      Current := Current.Right;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.ContainsAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while Result and It.HasNext do
    Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrBinaryTree.Create(FCompare);
  AssignPropertiesTo(Result);
end;

function TJclWideStrBinaryTree.Equals(const ACollection: IJclWideStrCollection): Boolean;
var
  It, ItSelf: IJclWideStrIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  if FSize <> ACollection.Size then
    Exit;
  Result := True;
  It := ACollection.First;
  ItSelf := First;
  while ItSelf.HasNext do
    if not ItemsEqual(ItSelf.Next, It.Next) then
    begin
      Result := False;
      Break;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.First: IJclWideStrIterator;
var
  Start: TJclWideStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Start := FRoot;
  case GetTraverseOrder of
    toPreOrder:
      Result := TPreOrderWideStrItr.Create(Self, Start, False);
    toOrder:
    begin
      if Start <> nil then
        while Start.Left <> nil do
          Start := Start.Left;
      Result := TInOrderWideStrItr.Create(Self, Start, False);
    end;
    toPostOrder:
    begin
      if Start <> nil then
        while (Start.Left <> nil) or (Start.Right <> nil) do
        begin
          if Start.Left <> nil then
            Start := Start.Left
          else
            Start := Start.Right;
        end;
      Result := TPostOrderWideStrItr.Create(Self, Start, False);
    end;
  else
    Result := nil;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclWideStrBinaryTree.GetEnumerator: IJclWideStrIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclWideStrBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclWideStrBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclWideStrBinaryTree.ItemsCompare(const A, B: WideString): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;

function TJclWideStrBinaryTree.ItemsEqual(const A, B: WideString): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;

function TJclWideStrBinaryTree.Last: IJclWideStrIterator;
var
  Start: TJclWideStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Start := FRoot;
  case FTraverseOrder of
    toPreOrder:
    begin
      if Start <> nil then
        while (Start.Left <> nil) or (Start.Right <> nil) do
        begin
          if Start.Right <> nil then
            Start := Start.Right
          else
            Start := Start.Left;
        end;
      Result := TPreOrderWideStrItr.Create(Self, Start, False);
    end;
    toOrder:
    begin
      if Start <> nil then
        while Start.Right <> nil do
          Start := Start.Right;
      Result := TInOrderWideStrItr.Create(Self, Start, False);
    end;
    toPostOrder:
      Result := TPostOrderWideStrItr.Create(Self, Start, False);
  else
    Result := nil;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrBinaryTree.Pack;
type
  TLeafArray = array of TJclWideStrBinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclWideStrBinaryNode;
    Offset: Integer): TJclWideStrBinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclWideStrBinaryNode;
  Index:     Integer;
begin
  SetLength(Leafarray, FSize);
  try
    // in order enumeration of nodes
    ANode := FRoot;
    if ANode <> nil then
    begin
      // find first node
      while ANode.Left <> nil do
        ANode := ANode.Left;

      Index := 0;
      while ANode <> nil do
      begin
        LeafArray[Index] := ANode;
        Inc(Index);
        if ANode.Right <> nil then
        begin
          ANode := ANode.Right;
          while (ANode.Left <> nil) do
            ANode := ANode.Left;
        end
        else
        begin
          BNode := ANode;
          ANode := ANode.Parent;
          while (ANode <> nil) and (ANode.Right = BNode) do
          begin
            BNode := ANode;
            ANode := ANode.Parent;
          end;
        end;
      end;

      Index := FSize shr 1;
      FRoot := LeafArray[Index];
      FRoot.Parent := nil;
      if Index > 0 then
        FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
      else
        FRoot.Left := nil;
      if Index < (FSize - 1) then
        FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
      else
        FRoot.Right := nil;
    end;
  finally
    SetLength(LeafArray, 0);
  end;
end;

function TJclWideStrBinaryTree.Remove(const AString: WideString): Boolean;
var
  Current, Successor: TJclWideStrBinaryNode;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
    // locate AString in the tree
  Current := FRoot;
  repeat
    while Current <> nil do
    begin
      Comp := ItemsCompare(AString, Current.Value);
      if Comp = 0 then
        Break
      else
      if Comp < 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    if Current = nil then
      Break;
    Result := True;
      // Remove Current from tree
    if (Current.Left = nil) and (Current.Right <> nil) then
    begin
        // remove references to Current
      Current.Right.Parent := Current.Parent;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Current.Right
        else
          Current.Parent.Right := Current.Right;
      end
      else
          // fix root
        FRoot := Current.Right;
      Successor := Current.Parent;
      if Successor = nil then
        Successor := FRoot;
    end
    else
    if (Current.Left <> nil) and (Current.Right = nil) then
    begin
        // remove references to Current
      Current.Left.Parent := Current.Parent;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Current.Left
        else
          Current.Parent.Right := Current.Left;
      end
      else
          // fix root
        FRoot := Current.Left;
      Successor := Current.Parent;
      if Successor = nil then
        Successor := FRoot;
    end
    else
    if (Current.Left <> nil) and (Current.Right <> nil) then
    begin
        // find the successor in tree
      Successor := Current.Right;
      while Successor.Left <> nil do
        Successor := Successor.Left;

      if Successor <> Current.Right then
      begin
          // remove references to successor
        if Successor.Parent.Left = Successor then
          Successor.Parent.Left := Successor.Right
        else
          Successor.Parent.Right := Successor.Right;
        if Successor.Right <> nil then
          Successor.Right.Parent := Successor.Parent;
        Successor.Right := Current.Right;
      end;

        // insert successor in new position
      Successor.Parent := Current.Parent;
      Successor.Left := Current.Left;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Successor
        else
          Current.Parent.Right := Successor;
      end
      else
          // fix root
        FRoot := Successor;
      Successor := Current.Parent;
      if Successor <> nil then
        Successor := FRoot;
    end
    else
    begin
        // (Current.Left = nil) and (Current.Right = nil)
      Successor := Current.Parent;
      if Successor <> nil then
      begin
          // remove references from parent
        if Successor.Left = Current then
          Successor.Left := nil
        else
          Successor.Right := nil;
      end
      else
        FRoot := nil;
    end;
    FreeString(Current.Value);
    Current.Free;
    Dec(FSize);
    Current := Successor;
  until FRemoveSingleElement or (Current = nil);
  AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.RemoveAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.RetainAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclWideStrBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclWideStrBinaryTree.Size: Integer;
begin
  Result := FSize;
end;


//=== { TJclSingleBinaryTree } =================================================

constructor TJclSingleBinaryTree.Create(ACompare: TSingleCompare);
begin
  inherited Create(nil);
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  FCompare := ACompare;
end;

destructor TJclSingleBinaryTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclSingleBinaryTree.Add(const AValue: Single): Boolean;
var
  NewNode, Current, Save: TJclSingleBinaryNode;
  Comp, Depth: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
  if FAllowDefaultElements or not ItemsEqual(AValue, 0.0) then
  begin
    Save := nil;
    Current := FRoot;
    Comp := 1;
    Depth := 0;
    while Current <> nil do
    begin
      Inc(Depth);
      Save := Current;
      Comp := ItemsCompare(AValue, Current.Value);
      if Comp < 0 then
        Current := Current.Left
      else
      if Comp > 0 then
        Current := Current.Right
      else
        Break;
    end;
    if (Comp <> 0) or CheckDuplicate then
    begin
      NewNode := TJclSingleBinaryNode.Create;
      NewNode.Value := AValue;
      NewNode.Parent := Save;
      if Save = nil then
        FRoot := NewNode
      else
      if ItemsCompare(NewNode.Value, Save.Value) < 0 then
        Save.Left := NewNode
      else
        Save.Right := NewNode;
      Inc(FSize);
      Inc(Depth);
      if Depth > FMaxDepth then
        FMaxDepth := Depth;
      Result := True;
      AutoPack;
    end
    else
      Result := False;
  end
  else
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTree.AddAll(const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclSingleBinaryNode): TJclSingleBinaryNode;
  begin
    Result := TJclSingleBinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclSingleBinaryTree;
  ACollection: IJclSingleCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclSingleBinaryTree then
  begin
    ADest := TJclSingleBinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclSingleCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclSingleBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclSingleBinaryTree then
    TJclSingleBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclSingleBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclSingleBinaryTree.Clear;
var
  Current, Parent: TJclSingleBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // postorder
  Current := FRoot;
  if Current = nil then
    Exit;
    // find first in post-order
  while (Current.Left <> nil) or (Current.Right <> nil) do
  begin
    if Current.Left <> nil then
      Current := Current.Left
    else
      Current := Current.Right;
  end;
    // for all items in the tree in post-order
  repeat
    Parent := Current.Parent;
      // remove reference
    if Parent <> nil then
    begin
      if Parent.Left = Current then
        Parent.Left := nil
      else
      if Parent.Right = Current then
        Parent.Right := nil;
    end;

      // free item
    FreeSingle(Current.Value);
    Current.Free;

      // find next item
    Current := Parent;
    if (Current <> nil) and (Current.Right <> nil) then
    begin
      Current := Current.Right;
      while (Current.Left <> nil) or (Current.Right <> nil) do
      begin
        if Current.Left <> nil then
          Current := Current.Left
        else
          Current := Current.Right;
      end;
    end;
  until Current = nil;
  FRoot := nil;
  FSize := 0;
  FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTree.Contains(const AValue: Single): Boolean;
var
  Comp: Integer;
  Current: TJclSingleBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  Current := FRoot;
  while Current <> nil do
  begin
    Comp := ItemsCompare(Current.Value, AValue);
    if Comp = 0 then
    begin
      Result := True;
      Break;
    end
    else
    if Comp > 0 then
      Current := Current.Left
    else
      Current := Current.Right;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTree.ContainsAll(const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while Result and It.HasNext do
    Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleBinaryTree.Create(FCompare);
  AssignPropertiesTo(Result);
end;

function TJclSingleBinaryTree.Equals(const ACollection: IJclSingleCollection): Boolean;
var
  It, ItSelf: IJclSingleIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  if FSize <> ACollection.Size then
    Exit;
  Result := True;
  It := ACollection.First;
  ItSelf := First;
  while ItSelf.HasNext do
    if not ItemsEqual(ItSelf.Next, It.Next) then
    begin
      Result := False;
      Break;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTree.First: IJclSingleIterator;
var
  Start: TJclSingleBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Start := FRoot;
  case GetTraverseOrder of
    toPreOrder:
      Result := TPreOrderSingleItr.Create(Self, Start, False);
    toOrder:
    begin
      if Start <> nil then
        while Start.Left <> nil do
          Start := Start.Left;
      Result := TInOrderSingleItr.Create(Self, Start, False);
    end;
    toPostOrder:
    begin
      if Start <> nil then
        while (Start.Left <> nil) or (Start.Right <> nil) do
        begin
          if Start.Left <> nil then
            Start := Start.Left
          else
            Start := Start.Right;
        end;
      Result := TPostOrderSingleItr.Create(Self, Start, False);
    end;
  else
    Result := nil;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclSingleBinaryTree.GetEnumerator: IJclSingleIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclSingleBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclSingleBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclSingleBinaryTree.ItemsCompare(const A, B: Single): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;

function TJclSingleBinaryTree.ItemsEqual(const A, B: Single): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;

function TJclSingleBinaryTree.Last: IJclSingleIterator;
var
  Start: TJclSingleBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Start := FRoot;
  case FTraverseOrder of
    toPreOrder:
    begin
      if Start <> nil then
        while (Start.Left <> nil) or (Start.Right <> nil) do
        begin
          if Start.Right <> nil then
            Start := Start.Right
          else
            Start := Start.Left;
        end;
      Result := TPreOrderSingleItr.Create(Self, Start, False);
    end;
    toOrder:
    begin
      if Start <> nil then
        while Start.Right <> nil do
          Start := Start.Right;
      Result := TInOrderSingleItr.Create(Self, Start, False);
    end;
    toPostOrder:
      Result := TPostOrderSingleItr.Create(Self, Start, False);
  else
    Result := nil;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleBinaryTree.Pack;
type
  TLeafArray = array of TJclSingleBinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclSingleBinaryNode;
    Offset: Integer): TJclSingleBinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclSingleBinaryNode;
  Index:     Integer;
begin
  SetLength(Leafarray, FSize);
  try
    // in order enumeration of nodes
    ANode := FRoot;
    if ANode <> nil then
    begin
      // find first node
      while ANode.Left <> nil do
        ANode := ANode.Left;

      Index := 0;
      while ANode <> nil do
      begin
        LeafArray[Index] := ANode;
        Inc(Index);
        if ANode.Right <> nil then
        begin
          ANode := ANode.Right;
          while (ANode.Left <> nil) do
            ANode := ANode.Left;
        end
        else
        begin
          BNode := ANode;
          ANode := ANode.Parent;
          while (ANode <> nil) and (ANode.Right = BNode) do
          begin
            BNode := ANode;
            ANode := ANode.Parent;
          end;
        end;
      end;

      Index := FSize shr 1;
      FRoot := LeafArray[Index];
      FRoot.Parent := nil;
      if Index > 0 then
        FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
      else
        FRoot.Left := nil;
      if Index < (FSize - 1) then
        FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
      else
        FRoot.Right := nil;
    end;
  finally
    SetLength(LeafArray, 0);
  end;
end;

function TJclSingleBinaryTree.Remove(const AValue: Single): Boolean;
var
  Current, Successor: TJclSingleBinaryNode;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
    // locate AValue in the tree
  Current := FRoot;
  repeat
    while Current <> nil do
    begin
      Comp := ItemsCompare(AValue, Current.Value);
      if Comp = 0 then
        Break
      else
      if Comp < 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    if Current = nil then
      Break;
    Result := True;
      // Remove Current from tree
    if (Current.Left = nil) and (Current.Right <> nil) then
    begin
        // remove references to Current
      Current.Right.Parent := Current.Parent;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Current.Right
        else
          Current.Parent.Right := Current.Right;
      end
      else
          // fix root
        FRoot := Current.Right;
      Successor := Current.Parent;
      if Successor = nil then
        Successor := FRoot;
    end
    else
    if (Current.Left <> nil) and (Current.Right = nil) then
    begin
        // remove references to Current
      Current.Left.Parent := Current.Parent;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Current.Left
        else
          Current.Parent.Right := Current.Left;
      end
      else
          // fix root
        FRoot := Current.Left;
      Successor := Current.Parent;
      if Successor = nil then
        Successor := FRoot;
    end
    else
    if (Current.Left <> nil) and (Current.Right <> nil) then
    begin
        // find the successor in tree
      Successor := Current.Right;
      while Successor.Left <> nil do
        Successor := Successor.Left;

      if Successor <> Current.Right then
      begin
          // remove references to successor
        if Successor.Parent.Left = Successor then
          Successor.Parent.Left := Successor.Right
        else
          Successor.Parent.Right := Successor.Right;
        if Successor.Right <> nil then
          Successor.Right.Parent := Successor.Parent;
        Successor.Right := Current.Right;
      end;

        // insert successor in new position
      Successor.Parent := Current.Parent;
      Successor.Left := Current.Left;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Successor
        else
          Current.Parent.Right := Successor;
      end
      else
          // fix root
        FRoot := Successor;
      Successor := Current.Parent;
      if Successor <> nil then
        Successor := FRoot;
    end
    else
    begin
        // (Current.Left = nil) and (Current.Right = nil)
      Successor := Current.Parent;
      if Successor <> nil then
      begin
          // remove references from parent
        if Successor.Left = Current then
          Successor.Left := nil
        else
          Successor.Right := nil;
      end
      else
        FRoot := nil;
    end;
    FreeSingle(Current.Value);
    Current.Free;
    Dec(FSize);
    Current := Successor;
  until FRemoveSingleElement or (Current = nil);
  AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTree.RemoveAll(const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTree.RetainAll(const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclSingleBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclSingleBinaryTree.Size: Integer;
begin
  Result := FSize;
end;


//=== { TJclDoubleBinaryTree } =================================================

constructor TJclDoubleBinaryTree.Create(ACompare: TDoubleCompare);
begin
  inherited Create(nil);
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  FCompare := ACompare;
end;

destructor TJclDoubleBinaryTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclDoubleBinaryTree.Add(const AValue: Double): Boolean;
var
  NewNode, Current, Save: TJclDoubleBinaryNode;
  Comp, Depth: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
  if FAllowDefaultElements or not ItemsEqual(AValue, 0.0) then
  begin
    Save := nil;
    Current := FRoot;
    Comp := 1;
    Depth := 0;
    while Current <> nil do
    begin
      Inc(Depth);
      Save := Current;
      Comp := ItemsCompare(AValue, Current.Value);
      if Comp < 0 then
        Current := Current.Left
      else
      if Comp > 0 then
        Current := Current.Right
      else
        Break;
    end;
    if (Comp <> 0) or CheckDuplicate then
    begin
      NewNode := TJclDoubleBinaryNode.Create;
      NewNode.Value := AValue;
      NewNode.Parent := Save;
      if Save = nil then
        FRoot := NewNode
      else
      if ItemsCompare(NewNode.Value, Save.Value) < 0 then
        Save.Left := NewNode
      else
        Save.Right := NewNode;
      Inc(FSize);
      Inc(Depth);
      if Depth > FMaxDepth then
        FMaxDepth := Depth;
      Result := True;
      AutoPack;
    end
    else
      Result := False;
  end
  else
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTree.AddAll(const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclDoubleBinaryNode): TJclDoubleBinaryNode;
  begin
    Result := TJclDoubleBinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclDoubleBinaryTree;
  ACollection: IJclDoubleCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclDoubleBinaryTree then
  begin
    ADest := TJclDoubleBinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclDoubleCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclDoubleBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclDoubleBinaryTree then
    TJclDoubleBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclDoubleBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclDoubleBinaryTree.Clear;
var
  Current, Parent: TJclDoubleBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // postorder
  Current := FRoot;
  if Current = nil then
    Exit;
    // find first in post-order
  while (Current.Left <> nil) or (Current.Right <> nil) do
  begin
    if Current.Left <> nil then
      Current := Current.Left
    else
      Current := Current.Right;
  end;
    // for all items in the tree in post-order
  repeat
    Parent := Current.Parent;
      // remove reference
    if Parent <> nil then
    begin
      if Parent.Left = Current then
        Parent.Left := nil
      else
      if Parent.Right = Current then
        Parent.Right := nil;
    end;

      // free item
    FreeDouble(Current.Value);
    Current.Free;

      // find next item
    Current := Parent;
    if (Current <> nil) and (Current.Right <> nil) then
    begin
      Current := Current.Right;
      while (Current.Left <> nil) or (Current.Right <> nil) do
      begin
        if Current.Left <> nil then
          Current := Current.Left
        else
          Current := Current.Right;
      end;
    end;
  until Current = nil;
  FRoot := nil;
  FSize := 0;
  FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTree.Contains(const AValue: Double): Boolean;
var
  Comp: Integer;
  Current: TJclDoubleBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  Current := FRoot;
  while Current <> nil do
  begin
    Comp := ItemsCompare(Current.Value, AValue);
    if Comp = 0 then
    begin
      Result := True;
      Break;
    end
    else
    if Comp > 0 then
      Current := Current.Left
    else
      Current := Current.Right;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTree.ContainsAll(const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while Result and It.HasNext do
    Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleBinaryTree.Create(FCompare);
  AssignPropertiesTo(Result);
end;

function TJclDoubleBinaryTree.Equals(const ACollection: IJclDoubleCollection): Boolean;
var
  It, ItSelf: IJclDoubleIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  if FSize <> ACollection.Size then
    Exit;
  Result := True;
  It := ACollection.First;
  ItSelf := First;
  while ItSelf.HasNext do
    if not ItemsEqual(ItSelf.Next, It.Next) then
    begin
      Result := False;
      Break;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTree.First: IJclDoubleIterator;
var
  Start: TJclDoubleBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Start := FRoot;
  case GetTraverseOrder of
    toPreOrder:
      Result := TPreOrderDoubleItr.Create(Self, Start, False);
    toOrder:
    begin
      if Start <> nil then
        while Start.Left <> nil do
          Start := Start.Left;
      Result := TInOrderDoubleItr.Create(Self, Start, False);
    end;
    toPostOrder:
    begin
      if Start <> nil then
        while (Start.Left <> nil) or (Start.Right <> nil) do
        begin
          if Start.Left <> nil then
            Start := Start.Left
          else
            Start := Start.Right;
        end;
      Result := TPostOrderDoubleItr.Create(Self, Start, False);
    end;
  else
    Result := nil;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclDoubleBinaryTree.GetEnumerator: IJclDoubleIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclDoubleBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclDoubleBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclDoubleBinaryTree.ItemsCompare(const A, B: Double): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;

function TJclDoubleBinaryTree.ItemsEqual(const A, B: Double): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;

function TJclDoubleBinaryTree.Last: IJclDoubleIterator;
var
  Start: TJclDoubleBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Start := FRoot;
  case FTraverseOrder of
    toPreOrder:
    begin
      if Start <> nil then
        while (Start.Left <> nil) or (Start.Right <> nil) do
        begin
          if Start.Right <> nil then
            Start := Start.Right
          else
            Start := Start.Left;
        end;
      Result := TPreOrderDoubleItr.Create(Self, Start, False);
    end;
    toOrder:
    begin
      if Start <> nil then
        while Start.Right <> nil do
          Start := Start.Right;
      Result := TInOrderDoubleItr.Create(Self, Start, False);
    end;
    toPostOrder:
      Result := TPostOrderDoubleItr.Create(Self, Start, False);
  else
    Result := nil;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleBinaryTree.Pack;
type
  TLeafArray = array of TJclDoubleBinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclDoubleBinaryNode;
    Offset: Integer): TJclDoubleBinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclDoubleBinaryNode;
  Index:     Integer;
begin
  SetLength(Leafarray, FSize);
  try
    // in order enumeration of nodes
    ANode := FRoot;
    if ANode <> nil then
    begin
      // find first node
      while ANode.Left <> nil do
        ANode := ANode.Left;

      Index := 0;
      while ANode <> nil do
      begin
        LeafArray[Index] := ANode;
        Inc(Index);
        if ANode.Right <> nil then
        begin
          ANode := ANode.Right;
          while (ANode.Left <> nil) do
            ANode := ANode.Left;
        end
        else
        begin
          BNode := ANode;
          ANode := ANode.Parent;
          while (ANode <> nil) and (ANode.Right = BNode) do
          begin
            BNode := ANode;
            ANode := ANode.Parent;
          end;
        end;
      end;

      Index := FSize shr 1;
      FRoot := LeafArray[Index];
      FRoot.Parent := nil;
      if Index > 0 then
        FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
      else
        FRoot.Left := nil;
      if Index < (FSize - 1) then
        FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
      else
        FRoot.Right := nil;
    end;
  finally
    SetLength(LeafArray, 0);
  end;
end;

function TJclDoubleBinaryTree.Remove(const AValue: Double): Boolean;
var
  Current, Successor: TJclDoubleBinaryNode;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
    // locate AValue in the tree
  Current := FRoot;
  repeat
    while Current <> nil do
    begin
      Comp := ItemsCompare(AValue, Current.Value);
      if Comp = 0 then
        Break
      else
      if Comp < 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    if Current = nil then
      Break;
    Result := True;
      // Remove Current from tree
    if (Current.Left = nil) and (Current.Right <> nil) then
    begin
        // remove references to Current
      Current.Right.Parent := Current.Parent;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Current.Right
        else
          Current.Parent.Right := Current.Right;
      end
      else
          // fix root
        FRoot := Current.Right;
      Successor := Current.Parent;
      if Successor = nil then
        Successor := FRoot;
    end
    else
    if (Current.Left <> nil) and (Current.Right = nil) then
    begin
        // remove references to Current
      Current.Left.Parent := Current.Parent;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Current.Left
        else
          Current.Parent.Right := Current.Left;
      end
      else
          // fix root
        FRoot := Current.Left;
      Successor := Current.Parent;
      if Successor = nil then
        Successor := FRoot;
    end
    else
    if (Current.Left <> nil) and (Current.Right <> nil) then
    begin
        // find the successor in tree
      Successor := Current.Right;
      while Successor.Left <> nil do
        Successor := Successor.Left;

      if Successor <> Current.Right then
      begin
          // remove references to successor
        if Successor.Parent.Left = Successor then
          Successor.Parent.Left := Successor.Right
        else
          Successor.Parent.Right := Successor.Right;
        if Successor.Right <> nil then
          Successor.Right.Parent := Successor.Parent;
        Successor.Right := Current.Right;
      end;

        // insert successor in new position
      Successor.Parent := Current.Parent;
      Successor.Left := Current.Left;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Successor
        else
          Current.Parent.Right := Successor;
      end
      else
          // fix root
        FRoot := Successor;
      Successor := Current.Parent;
      if Successor <> nil then
        Successor := FRoot;
    end
    else
    begin
        // (Current.Left = nil) and (Current.Right = nil)
      Successor := Current.Parent;
      if Successor <> nil then
      begin
          // remove references from parent
        if Successor.Left = Current then
          Successor.Left := nil
        else
          Successor.Right := nil;
      end
      else
        FRoot := nil;
    end;
    FreeDouble(Current.Value);
    Current.Free;
    Dec(FSize);
    Current := Successor;
  until FRemoveSingleElement or (Current = nil);
  AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTree.RemoveAll(const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTree.RetainAll(const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclDoubleBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclDoubleBinaryTree.Size: Integer;
begin
  Result := FSize;
end;


//=== { TJclExtendedBinaryTree } =================================================

constructor TJclExtendedBinaryTree.Create(ACompare: TExtendedCompare);
begin
  inherited Create(nil);
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  FCompare := ACompare;
end;

destructor TJclExtendedBinaryTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclExtendedBinaryTree.Add(const AValue: Extended): Boolean;
var
  NewNode, Current, Save: TJclExtendedBinaryNode;
  Comp, Depth: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
  if FAllowDefaultElements or not ItemsEqual(AValue, 0.0) then
  begin
    Save := nil;
    Current := FRoot;
    Comp := 1;
    Depth := 0;
    while Current <> nil do
    begin
      Inc(Depth);
      Save := Current;
      Comp := ItemsCompare(AValue, Current.Value);
      if Comp < 0 then
        Current := Current.Left
      else
      if Comp > 0 then
        Current := Current.Right
      else
        Break;
    end;
    if (Comp <> 0) or CheckDuplicate then
    begin
      NewNode := TJclExtendedBinaryNode.Create;
      NewNode.Value := AValue;
      NewNode.Parent := Save;
      if Save = nil then
        FRoot := NewNode
      else
      if ItemsCompare(NewNode.Value, Save.Value) < 0 then
        Save.Left := NewNode
      else
        Save.Right := NewNode;
      Inc(FSize);
      Inc(Depth);
      if Depth > FMaxDepth then
        FMaxDepth := Depth;
      Result := True;
      AutoPack;
    end
    else
      Result := False;
  end
  else
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTree.AddAll(const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclExtendedBinaryNode): TJclExtendedBinaryNode;
  begin
    Result := TJclExtendedBinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclExtendedBinaryTree;
  ACollection: IJclExtendedCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclExtendedBinaryTree then
  begin
    ADest := TJclExtendedBinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclExtendedCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclExtendedBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclExtendedBinaryTree then
    TJclExtendedBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclExtendedBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclExtendedBinaryTree.Clear;
var
  Current, Parent: TJclExtendedBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // postorder
  Current := FRoot;
  if Current = nil then
    Exit;
    // find first in post-order
  while (Current.Left <> nil) or (Current.Right <> nil) do
  begin
    if Current.Left <> nil then
      Current := Current.Left
    else
      Current := Current.Right;
  end;
    // for all items in the tree in post-order
  repeat
    Parent := Current.Parent;
      // remove reference
    if Parent <> nil then
    begin
      if Parent.Left = Current then
        Parent.Left := nil
      else
      if Parent.Right = Current then
        Parent.Right := nil;
    end;

      // free item
    FreeExtended(Current.Value);
    Current.Free;

      // find next item
    Current := Parent;
    if (Current <> nil) and (Current.Right <> nil) then
    begin
      Current := Current.Right;
      while (Current.Left <> nil) or (Current.Right <> nil) do
      begin
        if Current.Left <> nil then
          Current := Current.Left
        else
          Current := Current.Right;
      end;
    end;
  until Current = nil;
  FRoot := nil;
  FSize := 0;
  FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTree.Contains(const AValue: Extended): Boolean;
var
  Comp: Integer;
  Current: TJclExtendedBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  Current := FRoot;
  while Current <> nil do
  begin
    Comp := ItemsCompare(Current.Value, AValue);
    if Comp = 0 then
    begin
      Result := True;
      Break;
    end
    else
    if Comp > 0 then
      Current := Current.Left
    else
      Current := Current.Right;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTree.ContainsAll(const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while Result and It.HasNext do
    Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedBinaryTree.Create(FCompare);
  AssignPropertiesTo(Result);
end;

function TJclExtendedBinaryTree.Equals(const ACollection: IJclExtendedCollection): Boolean;
var
  It, ItSelf: IJclExtendedIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  if FSize <> ACollection.Size then
    Exit;
  Result := True;
  It := ACollection.First;
  ItSelf := First;
  while ItSelf.HasNext do
    if not ItemsEqual(ItSelf.Next, It.Next) then
    begin
      Result := False;
      Break;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTree.First: IJclExtendedIterator;
var
  Start: TJclExtendedBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Start := FRoot;
  case GetTraverseOrder of
    toPreOrder:
      Result := TPreOrderExtendedItr.Create(Self, Start, False);
    toOrder:
    begin
      if Start <> nil then
        while Start.Left <> nil do
          Start := Start.Left;
      Result := TInOrderExtendedItr.Create(Self, Start, False);
    end;
    toPostOrder:
    begin
      if Start <> nil then
        while (Start.Left <> nil) or (Start.Right <> nil) do
        begin
          if Start.Left <> nil then
            Start := Start.Left
          else
            Start := Start.Right;
        end;
      Result := TPostOrderExtendedItr.Create(Self, Start, False);
    end;
  else
    Result := nil;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclExtendedBinaryTree.GetEnumerator: IJclExtendedIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclExtendedBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclExtendedBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclExtendedBinaryTree.ItemsCompare(const A, B: Extended): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;

function TJclExtendedBinaryTree.ItemsEqual(const A, B: Extended): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;

function TJclExtendedBinaryTree.Last: IJclExtendedIterator;
var
  Start: TJclExtendedBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Start := FRoot;
  case FTraverseOrder of
    toPreOrder:
    begin
      if Start <> nil then
        while (Start.Left <> nil) or (Start.Right <> nil) do
        begin
          if Start.Right <> nil then
            Start := Start.Right
          else
            Start := Start.Left;
        end;
      Result := TPreOrderExtendedItr.Create(Self, Start, False);
    end;
    toOrder:
    begin
      if Start <> nil then
        while Start.Right <> nil do
          Start := Start.Right;
      Result := TInOrderExtendedItr.Create(Self, Start, False);
    end;
    toPostOrder:
      Result := TPostOrderExtendedItr.Create(Self, Start, False);
  else
    Result := nil;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedBinaryTree.Pack;
type
  TLeafArray = array of TJclExtendedBinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclExtendedBinaryNode;
    Offset: Integer): TJclExtendedBinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclExtendedBinaryNode;
  Index:     Integer;
begin
  SetLength(Leafarray, FSize);
  try
    // in order enumeration of nodes
    ANode := FRoot;
    if ANode <> nil then
    begin
      // find first node
      while ANode.Left <> nil do
        ANode := ANode.Left;

      Index := 0;
      while ANode <> nil do
      begin
        LeafArray[Index] := ANode;
        Inc(Index);
        if ANode.Right <> nil then
        begin
          ANode := ANode.Right;
          while (ANode.Left <> nil) do
            ANode := ANode.Left;
        end
        else
        begin
          BNode := ANode;
          ANode := ANode.Parent;
          while (ANode <> nil) and (ANode.Right = BNode) do
          begin
            BNode := ANode;
            ANode := ANode.Parent;
          end;
        end;
      end;

      Index := FSize shr 1;
      FRoot := LeafArray[Index];
      FRoot.Parent := nil;
      if Index > 0 then
        FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
      else
        FRoot.Left := nil;
      if Index < (FSize - 1) then
        FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
      else
        FRoot.Right := nil;
    end;
  finally
    SetLength(LeafArray, 0);
  end;
end;

function TJclExtendedBinaryTree.Remove(const AValue: Extended): Boolean;
var
  Current, Successor: TJclExtendedBinaryNode;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
    // locate AValue in the tree
  Current := FRoot;
  repeat
    while Current <> nil do
    begin
      Comp := ItemsCompare(AValue, Current.Value);
      if Comp = 0 then
        Break
      else
      if Comp < 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    if Current = nil then
      Break;
    Result := True;
      // Remove Current from tree
    if (Current.Left = nil) and (Current.Right <> nil) then
    begin
        // remove references to Current
      Current.Right.Parent := Current.Parent;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Current.Right
        else
          Current.Parent.Right := Current.Right;
      end
      else
          // fix root
        FRoot := Current.Right;
      Successor := Current.Parent;
      if Successor = nil then
        Successor := FRoot;
    end
    else
    if (Current.Left <> nil) and (Current.Right = nil) then
    begin
        // remove references to Current
      Current.Left.Parent := Current.Parent;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Current.Left
        else
          Current.Parent.Right := Current.Left;
      end
      else
          // fix root
        FRoot := Current.Left;
      Successor := Current.Parent;
      if Successor = nil then
        Successor := FRoot;
    end
    else
    if (Current.Left <> nil) and (Current.Right <> nil) then
    begin
        // find the successor in tree
      Successor := Current.Right;
      while Successor.Left <> nil do
        Successor := Successor.Left;

      if Successor <> Current.Right then
      begin
          // remove references to successor
        if Successor.Parent.Left = Successor then
          Successor.Parent.Left := Successor.Right
        else
          Successor.Parent.Right := Successor.Right;
        if Successor.Right <> nil then
          Successor.Right.Parent := Successor.Parent;
        Successor.Right := Current.Right;
      end;

        // insert successor in new position
      Successor.Parent := Current.Parent;
      Successor.Left := Current.Left;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Successor
        else
          Current.Parent.Right := Successor;
      end
      else
          // fix root
        FRoot := Successor;
      Successor := Current.Parent;
      if Successor <> nil then
        Successor := FRoot;
    end
    else
    begin
        // (Current.Left = nil) and (Current.Right = nil)
      Successor := Current.Parent;
      if Successor <> nil then
      begin
          // remove references from parent
        if Successor.Left = Current then
          Successor.Left := nil
        else
          Successor.Right := nil;
      end
      else
        FRoot := nil;
    end;
    FreeExtended(Current.Value);
    Current.Free;
    Dec(FSize);
    Current := Successor;
  until FRemoveSingleElement or (Current = nil);
  AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTree.RemoveAll(const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTree.RetainAll(const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclExtendedBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclExtendedBinaryTree.Size: Integer;
begin
  Result := FSize;
end;


//=== { TJclIntegerBinaryTree } =================================================

constructor TJclIntegerBinaryTree.Create(ACompare: TIntegerCompare);
begin
  inherited Create(nil);
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  FCompare := ACompare;
end;

destructor TJclIntegerBinaryTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclIntegerBinaryTree.Add(AValue: Integer): Boolean;
var
  NewNode, Current, Save: TJclIntegerBinaryNode;
  Comp, Depth: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
  if FAllowDefaultElements or not ItemsEqual(AValue, 0) then
  begin
    Save := nil;
    Current := FRoot;
    Comp := 1;
    Depth := 0;
    while Current <> nil do
    begin
      Inc(Depth);
      Save := Current;
      Comp := ItemsCompare(AValue, Current.Value);
      if Comp < 0 then
        Current := Current.Left
      else
      if Comp > 0 then
        Current := Current.Right
      else
        Break;
    end;
    if (Comp <> 0) or CheckDuplicate then
    begin
      NewNode := TJclIntegerBinaryNode.Create;
      NewNode.Value := AValue;
      NewNode.Parent := Save;
      if Save = nil then
        FRoot := NewNode
      else
      if ItemsCompare(NewNode.Value, Save.Value) < 0 then
        Save.Left := NewNode
      else
        Save.Right := NewNode;
      Inc(FSize);
      Inc(Depth);
      if Depth > FMaxDepth then
        FMaxDepth := Depth;
      Result := True;
      AutoPack;
    end
    else
      Result := False;
  end
  else
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTree.AddAll(const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclIntegerBinaryNode): TJclIntegerBinaryNode;
  begin
    Result := TJclIntegerBinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclIntegerBinaryTree;
  ACollection: IJclIntegerCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntegerBinaryTree then
  begin
    ADest := TJclIntegerBinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclIntegerCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclIntegerBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntegerBinaryTree then
    TJclIntegerBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclIntegerBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclIntegerBinaryTree.Clear;
var
  Current, Parent: TJclIntegerBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // postorder
  Current := FRoot;
  if Current = nil then
    Exit;
    // find first in post-order
  while (Current.Left <> nil) or (Current.Right <> nil) do
  begin
    if Current.Left <> nil then
      Current := Current.Left
    else
      Current := Current.Right;
  end;
    // for all items in the tree in post-order
  repeat
    Parent := Current.Parent;
      // remove reference
    if Parent <> nil then
    begin
      if Parent.Left = Current then
        Parent.Left := nil
      else
      if Parent.Right = Current then
        Parent.Right := nil;
    end;

      // free item
    FreeInteger(Current.Value);
    Current.Free;

      // find next item
    Current := Parent;
    if (Current <> nil) and (Current.Right <> nil) then
    begin
      Current := Current.Right;
      while (Current.Left <> nil) or (Current.Right <> nil) do
      begin
        if Current.Left <> nil then
          Current := Current.Left
        else
          Current := Current.Right;
      end;
    end;
  until Current = nil;
  FRoot := nil;
  FSize := 0;
  FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTree.Contains(AValue: Integer): Boolean;
var
  Comp: Integer;
  Current: TJclIntegerBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  Current := FRoot;
  while Current <> nil do
  begin
    Comp := ItemsCompare(Current.Value, AValue);
    if Comp = 0 then
    begin
      Result := True;
      Break;
    end
    else
    if Comp > 0 then
      Current := Current.Left
    else
      Current := Current.Right;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTree.ContainsAll(const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while Result and It.HasNext do
    Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerBinaryTree.Create(FCompare);
  AssignPropertiesTo(Result);
end;

function TJclIntegerBinaryTree.Equals(const ACollection: IJclIntegerCollection): Boolean;
var
  It, ItSelf: IJclIntegerIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  if FSize <> ACollection.Size then
    Exit;
  Result := True;
  It := ACollection.First;
  ItSelf := First;
  while ItSelf.HasNext do
    if not ItemsEqual(ItSelf.Next, It.Next) then
    begin
      Result := False;
      Break;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTree.First: IJclIntegerIterator;
var
  Start: TJclIntegerBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Start := FRoot;
  case GetTraverseOrder of
    toPreOrder:
      Result := TPreOrderIntegerItr.Create(Self, Start, False);
    toOrder:
    begin
      if Start <> nil then
        while Start.Left <> nil do
          Start := Start.Left;
      Result := TInOrderIntegerItr.Create(Self, Start, False);
    end;
    toPostOrder:
    begin
      if Start <> nil then
        while (Start.Left <> nil) or (Start.Right <> nil) do
        begin
          if Start.Left <> nil then
            Start := Start.Left
          else
            Start := Start.Right;
        end;
      Result := TPostOrderIntegerItr.Create(Self, Start, False);
    end;
  else
    Result := nil;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntegerBinaryTree.GetEnumerator: IJclIntegerIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntegerBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclIntegerBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntegerBinaryTree.ItemsCompare(A, B: Integer): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;

function TJclIntegerBinaryTree.ItemsEqual(A, B: Integer): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;

function TJclIntegerBinaryTree.Last: IJclIntegerIterator;
var
  Start: TJclIntegerBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Start := FRoot;
  case FTraverseOrder of
    toPreOrder:
    begin
      if Start <> nil then
        while (Start.Left <> nil) or (Start.Right <> nil) do
        begin
          if Start.Right <> nil then
            Start := Start.Right
          else
            Start := Start.Left;
        end;
      Result := TPreOrderIntegerItr.Create(Self, Start, False);
    end;
    toOrder:
    begin
      if Start <> nil then
        while Start.Right <> nil do
          Start := Start.Right;
      Result := TInOrderIntegerItr.Create(Self, Start, False);
    end;
    toPostOrder:
      Result := TPostOrderIntegerItr.Create(Self, Start, False);
  else
    Result := nil;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerBinaryTree.Pack;
type
  TLeafArray = array of TJclIntegerBinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclIntegerBinaryNode;
    Offset: Integer): TJclIntegerBinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclIntegerBinaryNode;
  Index:     Integer;
begin
  SetLength(Leafarray, FSize);
  try
    // in order enumeration of nodes
    ANode := FRoot;
    if ANode <> nil then
    begin
      // find first node
      while ANode.Left <> nil do
        ANode := ANode.Left;

      Index := 0;
      while ANode <> nil do
      begin
        LeafArray[Index] := ANode;
        Inc(Index);
        if ANode.Right <> nil then
        begin
          ANode := ANode.Right;
          while (ANode.Left <> nil) do
            ANode := ANode.Left;
        end
        else
        begin
          BNode := ANode;
          ANode := ANode.Parent;
          while (ANode <> nil) and (ANode.Right = BNode) do
          begin
            BNode := ANode;
            ANode := ANode.Parent;
          end;
        end;
      end;

      Index := FSize shr 1;
      FRoot := LeafArray[Index];
      FRoot.Parent := nil;
      if Index > 0 then
        FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
      else
        FRoot.Left := nil;
      if Index < (FSize - 1) then
        FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
      else
        FRoot.Right := nil;
    end;
  finally
    SetLength(LeafArray, 0);
  end;
end;

function TJclIntegerBinaryTree.Remove(AValue: Integer): Boolean;
var
  Current, Successor: TJclIntegerBinaryNode;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
    // locate AValue in the tree
  Current := FRoot;
  repeat
    while Current <> nil do
    begin
      Comp := ItemsCompare(AValue, Current.Value);
      if Comp = 0 then
        Break
      else
      if Comp < 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    if Current = nil then
      Break;
    Result := True;
      // Remove Current from tree
    if (Current.Left = nil) and (Current.Right <> nil) then
    begin
        // remove references to Current
      Current.Right.Parent := Current.Parent;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Current.Right
        else
          Current.Parent.Right := Current.Right;
      end
      else
          // fix root
        FRoot := Current.Right;
      Successor := Current.Parent;
      if Successor = nil then
        Successor := FRoot;
    end
    else
    if (Current.Left <> nil) and (Current.Right = nil) then
    begin
        // remove references to Current
      Current.Left.Parent := Current.Parent;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Current.Left
        else
          Current.Parent.Right := Current.Left;
      end
      else
          // fix root
        FRoot := Current.Left;
      Successor := Current.Parent;
      if Successor = nil then
        Successor := FRoot;
    end
    else
    if (Current.Left <> nil) and (Current.Right <> nil) then
    begin
        // find the successor in tree
      Successor := Current.Right;
      while Successor.Left <> nil do
        Successor := Successor.Left;

      if Successor <> Current.Right then
      begin
          // remove references to successor
        if Successor.Parent.Left = Successor then
          Successor.Parent.Left := Successor.Right
        else
          Successor.Parent.Right := Successor.Right;
        if Successor.Right <> nil then
          Successor.Right.Parent := Successor.Parent;
        Successor.Right := Current.Right;
      end;

        // insert successor in new position
      Successor.Parent := Current.Parent;
      Successor.Left := Current.Left;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Successor
        else
          Current.Parent.Right := Successor;
      end
      else
          // fix root
        FRoot := Successor;
      Successor := Current.Parent;
      if Successor <> nil then
        Successor := FRoot;
    end
    else
    begin
        // (Current.Left = nil) and (Current.Right = nil)
      Successor := Current.Parent;
      if Successor <> nil then
      begin
          // remove references from parent
        if Successor.Left = Current then
          Successor.Left := nil
        else
          Successor.Right := nil;
      end
      else
        FRoot := nil;
    end;
    FreeInteger(Current.Value);
    Current.Free;
    Dec(FSize);
    Current := Successor;
  until FRemoveSingleElement or (Current = nil);
  AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTree.RemoveAll(const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTree.RetainAll(const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntegerBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclIntegerBinaryTree.Size: Integer;
begin
  Result := FSize;
end;


//=== { TJclCardinalBinaryTree } =================================================

constructor TJclCardinalBinaryTree.Create(ACompare: TCardinalCompare);
begin
  inherited Create(nil);
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  FCompare := ACompare;
end;

destructor TJclCardinalBinaryTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclCardinalBinaryTree.Add(AValue: Cardinal): Boolean;
var
  NewNode, Current, Save: TJclCardinalBinaryNode;
  Comp, Depth: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
  if FAllowDefaultElements or not ItemsEqual(AValue, 0) then
  begin
    Save := nil;
    Current := FRoot;
    Comp := 1;
    Depth := 0;
    while Current <> nil do
    begin
      Inc(Depth);
      Save := Current;
      Comp := ItemsCompare(AValue, Current.Value);
      if Comp < 0 then
        Current := Current.Left
      else
      if Comp > 0 then
        Current := Current.Right
      else
        Break;
    end;
    if (Comp <> 0) or CheckDuplicate then
    begin
      NewNode := TJclCardinalBinaryNode.Create;
      NewNode.Value := AValue;
      NewNode.Parent := Save;
      if Save = nil then
        FRoot := NewNode
      else
      if ItemsCompare(NewNode.Value, Save.Value) < 0 then
        Save.Left := NewNode
      else
        Save.Right := NewNode;
      Inc(FSize);
      Inc(Depth);
      if Depth > FMaxDepth then
        FMaxDepth := Depth;
      Result := True;
      AutoPack;
    end
    else
      Result := False;
  end
  else
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTree.AddAll(const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclCardinalBinaryNode): TJclCardinalBinaryNode;
  begin
    Result := TJclCardinalBinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclCardinalBinaryTree;
  ACollection: IJclCardinalCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclCardinalBinaryTree then
  begin
    ADest := TJclCardinalBinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclCardinalCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclCardinalBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclCardinalBinaryTree then
    TJclCardinalBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclCardinalBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclCardinalBinaryTree.Clear;
var
  Current, Parent: TJclCardinalBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // postorder
  Current := FRoot;
  if Current = nil then
    Exit;
    // find first in post-order
  while (Current.Left <> nil) or (Current.Right <> nil) do
  begin
    if Current.Left <> nil then
      Current := Current.Left
    else
      Current := Current.Right;
  end;
    // for all items in the tree in post-order
  repeat
    Parent := Current.Parent;
      // remove reference
    if Parent <> nil then
    begin
      if Parent.Left = Current then
        Parent.Left := nil
      else
      if Parent.Right = Current then
        Parent.Right := nil;
    end;

      // free item
    FreeCardinal(Current.Value);
    Current.Free;

      // find next item
    Current := Parent;
    if (Current <> nil) and (Current.Right <> nil) then
    begin
      Current := Current.Right;
      while (Current.Left <> nil) or (Current.Right <> nil) do
      begin
        if Current.Left <> nil then
          Current := Current.Left
        else
          Current := Current.Right;
      end;
    end;
  until Current = nil;
  FRoot := nil;
  FSize := 0;
  FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTree.Contains(AValue: Cardinal): Boolean;
var
  Comp: Integer;
  Current: TJclCardinalBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  Current := FRoot;
  while Current <> nil do
  begin
    Comp := ItemsCompare(Current.Value, AValue);
    if Comp = 0 then
    begin
      Result := True;
      Break;
    end
    else
    if Comp > 0 then
      Current := Current.Left
    else
      Current := Current.Right;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTree.ContainsAll(const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while Result and It.HasNext do
    Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalBinaryTree.Create(FCompare);
  AssignPropertiesTo(Result);
end;

function TJclCardinalBinaryTree.Equals(const ACollection: IJclCardinalCollection): Boolean;
var
  It, ItSelf: IJclCardinalIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  if FSize <> ACollection.Size then
    Exit;
  Result := True;
  It := ACollection.First;
  ItSelf := First;
  while ItSelf.HasNext do
    if not ItemsEqual(ItSelf.Next, It.Next) then
    begin
      Result := False;
      Break;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTree.First: IJclCardinalIterator;
var
  Start: TJclCardinalBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Start := FRoot;
  case GetTraverseOrder of
    toPreOrder:
      Result := TPreOrderCardinalItr.Create(Self, Start, False);
    toOrder:
    begin
      if Start <> nil then
        while Start.Left <> nil do
          Start := Start.Left;
      Result := TInOrderCardinalItr.Create(Self, Start, False);
    end;
    toPostOrder:
    begin
      if Start <> nil then
        while (Start.Left <> nil) or (Start.Right <> nil) do
        begin
          if Start.Left <> nil then
            Start := Start.Left
          else
            Start := Start.Right;
        end;
      Result := TPostOrderCardinalItr.Create(Self, Start, False);
    end;
  else
    Result := nil;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclCardinalBinaryTree.GetEnumerator: IJclCardinalIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclCardinalBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclCardinalBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclCardinalBinaryTree.ItemsCompare(A, B: Cardinal): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;

function TJclCardinalBinaryTree.ItemsEqual(A, B: Cardinal): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;

function TJclCardinalBinaryTree.Last: IJclCardinalIterator;
var
  Start: TJclCardinalBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Start := FRoot;
  case FTraverseOrder of
    toPreOrder:
    begin
      if Start <> nil then
        while (Start.Left <> nil) or (Start.Right <> nil) do
        begin
          if Start.Right <> nil then
            Start := Start.Right
          else
            Start := Start.Left;
        end;
      Result := TPreOrderCardinalItr.Create(Self, Start, False);
    end;
    toOrder:
    begin
      if Start <> nil then
        while Start.Right <> nil do
          Start := Start.Right;
      Result := TInOrderCardinalItr.Create(Self, Start, False);
    end;
    toPostOrder:
      Result := TPostOrderCardinalItr.Create(Self, Start, False);
  else
    Result := nil;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalBinaryTree.Pack;
type
  TLeafArray = array of TJclCardinalBinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclCardinalBinaryNode;
    Offset: Integer): TJclCardinalBinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclCardinalBinaryNode;
  Index:     Integer;
begin
  SetLength(Leafarray, FSize);
  try
    // in order enumeration of nodes
    ANode := FRoot;
    if ANode <> nil then
    begin
      // find first node
      while ANode.Left <> nil do
        ANode := ANode.Left;

      Index := 0;
      while ANode <> nil do
      begin
        LeafArray[Index] := ANode;
        Inc(Index);
        if ANode.Right <> nil then
        begin
          ANode := ANode.Right;
          while (ANode.Left <> nil) do
            ANode := ANode.Left;
        end
        else
        begin
          BNode := ANode;
          ANode := ANode.Parent;
          while (ANode <> nil) and (ANode.Right = BNode) do
          begin
            BNode := ANode;
            ANode := ANode.Parent;
          end;
        end;
      end;

      Index := FSize shr 1;
      FRoot := LeafArray[Index];
      FRoot.Parent := nil;
      if Index > 0 then
        FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
      else
        FRoot.Left := nil;
      if Index < (FSize - 1) then
        FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
      else
        FRoot.Right := nil;
    end;
  finally
    SetLength(LeafArray, 0);
  end;
end;

function TJclCardinalBinaryTree.Remove(AValue: Cardinal): Boolean;
var
  Current, Successor: TJclCardinalBinaryNode;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
    // locate AValue in the tree
  Current := FRoot;
  repeat
    while Current <> nil do
    begin
      Comp := ItemsCompare(AValue, Current.Value);
      if Comp = 0 then
        Break
      else
      if Comp < 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    if Current = nil then
      Break;
    Result := True;
      // Remove Current from tree
    if (Current.Left = nil) and (Current.Right <> nil) then
    begin
        // remove references to Current
      Current.Right.Parent := Current.Parent;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Current.Right
        else
          Current.Parent.Right := Current.Right;
      end
      else
          // fix root
        FRoot := Current.Right;
      Successor := Current.Parent;
      if Successor = nil then
        Successor := FRoot;
    end
    else
    if (Current.Left <> nil) and (Current.Right = nil) then
    begin
        // remove references to Current
      Current.Left.Parent := Current.Parent;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Current.Left
        else
          Current.Parent.Right := Current.Left;
      end
      else
          // fix root
        FRoot := Current.Left;
      Successor := Current.Parent;
      if Successor = nil then
        Successor := FRoot;
    end
    else
    if (Current.Left <> nil) and (Current.Right <> nil) then
    begin
        // find the successor in tree
      Successor := Current.Right;
      while Successor.Left <> nil do
        Successor := Successor.Left;

      if Successor <> Current.Right then
      begin
          // remove references to successor
        if Successor.Parent.Left = Successor then
          Successor.Parent.Left := Successor.Right
        else
          Successor.Parent.Right := Successor.Right;
        if Successor.Right <> nil then
          Successor.Right.Parent := Successor.Parent;
        Successor.Right := Current.Right;
      end;

        // insert successor in new position
      Successor.Parent := Current.Parent;
      Successor.Left := Current.Left;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Successor
        else
          Current.Parent.Right := Successor;
      end
      else
          // fix root
        FRoot := Successor;
      Successor := Current.Parent;
      if Successor <> nil then
        Successor := FRoot;
    end
    else
    begin
        // (Current.Left = nil) and (Current.Right = nil)
      Successor := Current.Parent;
      if Successor <> nil then
      begin
          // remove references from parent
        if Successor.Left = Current then
          Successor.Left := nil
        else
          Successor.Right := nil;
      end
      else
        FRoot := nil;
    end;
    FreeCardinal(Current.Value);
    Current.Free;
    Dec(FSize);
    Current := Successor;
  until FRemoveSingleElement or (Current = nil);
  AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTree.RemoveAll(const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTree.RetainAll(const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclCardinalBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclCardinalBinaryTree.Size: Integer;
begin
  Result := FSize;
end;


//=== { TJclInt64BinaryTree } =================================================

constructor TJclInt64BinaryTree.Create(ACompare: TInt64Compare);
begin
  inherited Create(nil);
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  FCompare := ACompare;
end;

destructor TJclInt64BinaryTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclInt64BinaryTree.Add(const AValue: Int64): Boolean;
var
  NewNode, Current, Save: TJclInt64BinaryNode;
  Comp, Depth: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
  if FAllowDefaultElements or not ItemsEqual(AValue, 0) then
  begin
    Save := nil;
    Current := FRoot;
    Comp := 1;
    Depth := 0;
    while Current <> nil do
    begin
      Inc(Depth);
      Save := Current;
      Comp := ItemsCompare(AValue, Current.Value);
      if Comp < 0 then
        Current := Current.Left
      else
      if Comp > 0 then
        Current := Current.Right
      else
        Break;
    end;
    if (Comp <> 0) or CheckDuplicate then
    begin
      NewNode := TJclInt64BinaryNode.Create;
      NewNode.Value := AValue;
      NewNode.Parent := Save;
      if Save = nil then
        FRoot := NewNode
      else
      if ItemsCompare(NewNode.Value, Save.Value) < 0 then
        Save.Left := NewNode
      else
        Save.Right := NewNode;
      Inc(FSize);
      Inc(Depth);
      if Depth > FMaxDepth then
        FMaxDepth := Depth;
      Result := True;
      AutoPack;
    end
    else
      Result := False;
  end
  else
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTree.AddAll(const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64BinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclInt64BinaryNode): TJclInt64BinaryNode;
  begin
    Result := TJclInt64BinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclInt64BinaryTree;
  ACollection: IJclInt64Collection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclInt64BinaryTree then
  begin
    ADest := TJclInt64BinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclInt64Collection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclInt64BinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclInt64BinaryTree then
    TJclInt64BinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclInt64BinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclInt64BinaryTree.Clear;
var
  Current, Parent: TJclInt64BinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // postorder
  Current := FRoot;
  if Current = nil then
    Exit;
    // find first in post-order
  while (Current.Left <> nil) or (Current.Right <> nil) do
  begin
    if Current.Left <> nil then
      Current := Current.Left
    else
      Current := Current.Right;
  end;
    // for all items in the tree in post-order
  repeat
    Parent := Current.Parent;
      // remove reference
    if Parent <> nil then
    begin
      if Parent.Left = Current then
        Parent.Left := nil
      else
      if Parent.Right = Current then
        Parent.Right := nil;
    end;

      // free item
    FreeInt64(Current.Value);
    Current.Free;

      // find next item
    Current := Parent;
    if (Current <> nil) and (Current.Right <> nil) then
    begin
      Current := Current.Right;
      while (Current.Left <> nil) or (Current.Right <> nil) do
      begin
        if Current.Left <> nil then
          Current := Current.Left
        else
          Current := Current.Right;
      end;
    end;
  until Current = nil;
  FRoot := nil;
  FSize := 0;
  FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTree.Contains(const AValue: Int64): Boolean;
var
  Comp: Integer;
  Current: TJclInt64BinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  Current := FRoot;
  while Current <> nil do
  begin
    Comp := ItemsCompare(Current.Value, AValue);
    if Comp = 0 then
    begin
      Result := True;
      Break;
    end
    else
    if Comp > 0 then
      Current := Current.Left
    else
      Current := Current.Right;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTree.ContainsAll(const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while Result and It.HasNext do
    Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64BinaryTree.Create(FCompare);
  AssignPropertiesTo(Result);
end;

function TJclInt64BinaryTree.Equals(const ACollection: IJclInt64Collection): Boolean;
var
  It, ItSelf: IJclInt64Iterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  if FSize <> ACollection.Size then
    Exit;
  Result := True;
  It := ACollection.First;
  ItSelf := First;
  while ItSelf.HasNext do
    if not ItemsEqual(ItSelf.Next, It.Next) then
    begin
      Result := False;
      Break;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTree.First: IJclInt64Iterator;
var
  Start: TJclInt64BinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Start := FRoot;
  case GetTraverseOrder of
    toPreOrder:
      Result := TPreOrderInt64Itr.Create(Self, Start, False);
    toOrder:
    begin
      if Start <> nil then
        while Start.Left <> nil do
          Start := Start.Left;
      Result := TInOrderInt64Itr.Create(Self, Start, False);
    end;
    toPostOrder:
    begin
      if Start <> nil then
        while (Start.Left <> nil) or (Start.Right <> nil) do
        begin
          if Start.Left <> nil then
            Start := Start.Left
          else
            Start := Start.Right;
        end;
      Result := TPostOrderInt64Itr.Create(Self, Start, False);
    end;
  else
    Result := nil;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclInt64BinaryTree.GetEnumerator: IJclInt64Iterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclInt64BinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclInt64BinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclInt64BinaryTree.ItemsCompare(const A, B: Int64): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;

function TJclInt64BinaryTree.ItemsEqual(const A, B: Int64): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;

function TJclInt64BinaryTree.Last: IJclInt64Iterator;
var
  Start: TJclInt64BinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Start := FRoot;
  case FTraverseOrder of
    toPreOrder:
    begin
      if Start <> nil then
        while (Start.Left <> nil) or (Start.Right <> nil) do
        begin
          if Start.Right <> nil then
            Start := Start.Right
          else
            Start := Start.Left;
        end;
      Result := TPreOrderInt64Itr.Create(Self, Start, False);
    end;
    toOrder:
    begin
      if Start <> nil then
        while Start.Right <> nil do
          Start := Start.Right;
      Result := TInOrderInt64Itr.Create(Self, Start, False);
    end;
    toPostOrder:
      Result := TPostOrderInt64Itr.Create(Self, Start, False);
  else
    Result := nil;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64BinaryTree.Pack;
type
  TLeafArray = array of TJclInt64BinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclInt64BinaryNode;
    Offset: Integer): TJclInt64BinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclInt64BinaryNode;
  Index:     Integer;
begin
  SetLength(Leafarray, FSize);
  try
    // in order enumeration of nodes
    ANode := FRoot;
    if ANode <> nil then
    begin
      // find first node
      while ANode.Left <> nil do
        ANode := ANode.Left;

      Index := 0;
      while ANode <> nil do
      begin
        LeafArray[Index] := ANode;
        Inc(Index);
        if ANode.Right <> nil then
        begin
          ANode := ANode.Right;
          while (ANode.Left <> nil) do
            ANode := ANode.Left;
        end
        else
        begin
          BNode := ANode;
          ANode := ANode.Parent;
          while (ANode <> nil) and (ANode.Right = BNode) do
          begin
            BNode := ANode;
            ANode := ANode.Parent;
          end;
        end;
      end;

      Index := FSize shr 1;
      FRoot := LeafArray[Index];
      FRoot.Parent := nil;
      if Index > 0 then
        FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
      else
        FRoot.Left := nil;
      if Index < (FSize - 1) then
        FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
      else
        FRoot.Right := nil;
    end;
  finally
    SetLength(LeafArray, 0);
  end;
end;

function TJclInt64BinaryTree.Remove(const AValue: Int64): Boolean;
var
  Current, Successor: TJclInt64BinaryNode;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
    // locate AValue in the tree
  Current := FRoot;
  repeat
    while Current <> nil do
    begin
      Comp := ItemsCompare(AValue, Current.Value);
      if Comp = 0 then
        Break
      else
      if Comp < 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    if Current = nil then
      Break;
    Result := True;
      // Remove Current from tree
    if (Current.Left = nil) and (Current.Right <> nil) then
    begin
        // remove references to Current
      Current.Right.Parent := Current.Parent;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Current.Right
        else
          Current.Parent.Right := Current.Right;
      end
      else
          // fix root
        FRoot := Current.Right;
      Successor := Current.Parent;
      if Successor = nil then
        Successor := FRoot;
    end
    else
    if (Current.Left <> nil) and (Current.Right = nil) then
    begin
        // remove references to Current
      Current.Left.Parent := Current.Parent;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Current.Left
        else
          Current.Parent.Right := Current.Left;
      end
      else
          // fix root
        FRoot := Current.Left;
      Successor := Current.Parent;
      if Successor = nil then
        Successor := FRoot;
    end
    else
    if (Current.Left <> nil) and (Current.Right <> nil) then
    begin
        // find the successor in tree
      Successor := Current.Right;
      while Successor.Left <> nil do
        Successor := Successor.Left;

      if Successor <> Current.Right then
      begin
          // remove references to successor
        if Successor.Parent.Left = Successor then
          Successor.Parent.Left := Successor.Right
        else
          Successor.Parent.Right := Successor.Right;
        if Successor.Right <> nil then
          Successor.Right.Parent := Successor.Parent;
        Successor.Right := Current.Right;
      end;

        // insert successor in new position
      Successor.Parent := Current.Parent;
      Successor.Left := Current.Left;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Successor
        else
          Current.Parent.Right := Successor;
      end
      else
          // fix root
        FRoot := Successor;
      Successor := Current.Parent;
      if Successor <> nil then
        Successor := FRoot;
    end
    else
    begin
        // (Current.Left = nil) and (Current.Right = nil)
      Successor := Current.Parent;
      if Successor <> nil then
      begin
          // remove references from parent
        if Successor.Left = Current then
          Successor.Left := nil
        else
          Successor.Right := nil;
      end
      else
        FRoot := nil;
    end;
    FreeInt64(Current.Value);
    Current.Free;
    Dec(FSize);
    Current := Successor;
  until FRemoveSingleElement or (Current = nil);
  AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTree.RemoveAll(const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTree.RetainAll(const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64BinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclInt64BinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclInt64BinaryTree.Size: Integer;
begin
  Result := FSize;
end;

{$IFNDEF CLR}

//=== { TJclPtrBinaryTree } =================================================

constructor TJclPtrBinaryTree.Create(ACompare: TPtrCompare);
begin
  inherited Create(nil);
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  FCompare := ACompare;
end;

destructor TJclPtrBinaryTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclPtrBinaryTree.Add(APtr: Pointer): Boolean;
var
  NewNode, Current, Save: TJclPtrBinaryNode;
  Comp, Depth: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
  if FAllowDefaultElements or not ItemsEqual(APtr, nil) then
  begin
    Save := nil;
    Current := FRoot;
    Comp := 1;
    Depth := 0;
    while Current <> nil do
    begin
      Inc(Depth);
      Save := Current;
      Comp := ItemsCompare(APtr, Current.Value);
      if Comp < 0 then
        Current := Current.Left
      else
      if Comp > 0 then
        Current := Current.Right
      else
        Break;
    end;
    if (Comp <> 0) or CheckDuplicate then
    begin
      NewNode := TJclPtrBinaryNode.Create;
      NewNode.Value := APtr;
      NewNode.Parent := Save;
      if Save = nil then
        FRoot := NewNode
      else
      if ItemsCompare(NewNode.Value, Save.Value) < 0 then
        Save.Left := NewNode
      else
        Save.Right := NewNode;
      Inc(FSize);
      Inc(Depth);
      if Depth > FMaxDepth then
        FMaxDepth := Depth;
      Result := True;
      AutoPack;
    end
    else
      Result := False;
  end
  else
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTree.AddAll(const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclPtrBinaryNode): TJclPtrBinaryNode;
  begin
    Result := TJclPtrBinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclPtrBinaryTree;
  ACollection: IJclPtrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclPtrBinaryTree then
  begin
    ADest := TJclPtrBinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclPtrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclPtrBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclPtrBinaryTree then
    TJclPtrBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclPtrBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclPtrBinaryTree.Clear;
var
  Current, Parent: TJclPtrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // postorder
  Current := FRoot;
  if Current = nil then
    Exit;
    // find first in post-order
  while (Current.Left <> nil) or (Current.Right <> nil) do
  begin
    if Current.Left <> nil then
      Current := Current.Left
    else
      Current := Current.Right;
  end;
    // for all items in the tree in post-order
  repeat
    Parent := Current.Parent;
      // remove reference
    if Parent <> nil then
    begin
      if Parent.Left = Current then
        Parent.Left := nil
      else
      if Parent.Right = Current then
        Parent.Right := nil;
    end;

      // free item
    FreePointer(Current.Value);
    Current.Free;

      // find next item
    Current := Parent;
    if (Current <> nil) and (Current.Right <> nil) then
    begin
      Current := Current.Right;
      while (Current.Left <> nil) or (Current.Right <> nil) do
      begin
        if Current.Left <> nil then
          Current := Current.Left
        else
          Current := Current.Right;
      end;
    end;
  until Current = nil;
  FRoot := nil;
  FSize := 0;
  FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTree.Contains(APtr: Pointer): Boolean;
var
  Comp: Integer;
  Current: TJclPtrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  Current := FRoot;
  while Current <> nil do
  begin
    Comp := ItemsCompare(Current.Value, APtr);
    if Comp = 0 then
    begin
      Result := True;
      Break;
    end
    else
    if Comp > 0 then
      Current := Current.Left
    else
      Current := Current.Right;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTree.ContainsAll(const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while Result and It.HasNext do
    Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrBinaryTree.Create(FCompare);
  AssignPropertiesTo(Result);
end;

function TJclPtrBinaryTree.Equals(const ACollection: IJclPtrCollection): Boolean;
var
  It, ItSelf: IJclPtrIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  if FSize <> ACollection.Size then
    Exit;
  Result := True;
  It := ACollection.First;
  ItSelf := First;
  while ItSelf.HasNext do
    if not ItemsEqual(ItSelf.Next, It.Next) then
    begin
      Result := False;
      Break;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTree.First: IJclPtrIterator;
var
  Start: TJclPtrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Start := FRoot;
  case GetTraverseOrder of
    toPreOrder:
      Result := TPreOrderPtrItr.Create(Self, Start, False);
    toOrder:
    begin
      if Start <> nil then
        while Start.Left <> nil do
          Start := Start.Left;
      Result := TInOrderPtrItr.Create(Self, Start, False);
    end;
    toPostOrder:
    begin
      if Start <> nil then
        while (Start.Left <> nil) or (Start.Right <> nil) do
        begin
          if Start.Left <> nil then
            Start := Start.Left
          else
            Start := Start.Right;
        end;
      Result := TPostOrderPtrItr.Create(Self, Start, False);
    end;
  else
    Result := nil;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclPtrBinaryTree.GetEnumerator: IJclPtrIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclPtrBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclPtrBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclPtrBinaryTree.ItemsCompare(A, B: Pointer): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;

function TJclPtrBinaryTree.ItemsEqual(A, B: Pointer): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;

function TJclPtrBinaryTree.Last: IJclPtrIterator;
var
  Start: TJclPtrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Start := FRoot;
  case FTraverseOrder of
    toPreOrder:
    begin
      if Start <> nil then
        while (Start.Left <> nil) or (Start.Right <> nil) do
        begin
          if Start.Right <> nil then
            Start := Start.Right
          else
            Start := Start.Left;
        end;
      Result := TPreOrderPtrItr.Create(Self, Start, False);
    end;
    toOrder:
    begin
      if Start <> nil then
        while Start.Right <> nil do
          Start := Start.Right;
      Result := TInOrderPtrItr.Create(Self, Start, False);
    end;
    toPostOrder:
      Result := TPostOrderPtrItr.Create(Self, Start, False);
  else
    Result := nil;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrBinaryTree.Pack;
type
  TLeafArray = array of TJclPtrBinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclPtrBinaryNode;
    Offset: Integer): TJclPtrBinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclPtrBinaryNode;
  Index:     Integer;
begin
  SetLength(Leafarray, FSize);
  try
    // in order enumeration of nodes
    ANode := FRoot;
    if ANode <> nil then
    begin
      // find first node
      while ANode.Left <> nil do
        ANode := ANode.Left;

      Index := 0;
      while ANode <> nil do
      begin
        LeafArray[Index] := ANode;
        Inc(Index);
        if ANode.Right <> nil then
        begin
          ANode := ANode.Right;
          while (ANode.Left <> nil) do
            ANode := ANode.Left;
        end
        else
        begin
          BNode := ANode;
          ANode := ANode.Parent;
          while (ANode <> nil) and (ANode.Right = BNode) do
          begin
            BNode := ANode;
            ANode := ANode.Parent;
          end;
        end;
      end;

      Index := FSize shr 1;
      FRoot := LeafArray[Index];
      FRoot.Parent := nil;
      if Index > 0 then
        FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
      else
        FRoot.Left := nil;
      if Index < (FSize - 1) then
        FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
      else
        FRoot.Right := nil;
    end;
  finally
    SetLength(LeafArray, 0);
  end;
end;

function TJclPtrBinaryTree.Remove(APtr: Pointer): Boolean;
var
  Current, Successor: TJclPtrBinaryNode;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
    // locate APtr in the tree
  Current := FRoot;
  repeat
    while Current <> nil do
    begin
      Comp := ItemsCompare(APtr, Current.Value);
      if Comp = 0 then
        Break
      else
      if Comp < 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    if Current = nil then
      Break;
    Result := True;
      // Remove Current from tree
    if (Current.Left = nil) and (Current.Right <> nil) then
    begin
        // remove references to Current
      Current.Right.Parent := Current.Parent;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Current.Right
        else
          Current.Parent.Right := Current.Right;
      end
      else
          // fix root
        FRoot := Current.Right;
      Successor := Current.Parent;
      if Successor = nil then
        Successor := FRoot;
    end
    else
    if (Current.Left <> nil) and (Current.Right = nil) then
    begin
        // remove references to Current
      Current.Left.Parent := Current.Parent;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Current.Left
        else
          Current.Parent.Right := Current.Left;
      end
      else
          // fix root
        FRoot := Current.Left;
      Successor := Current.Parent;
      if Successor = nil then
        Successor := FRoot;
    end
    else
    if (Current.Left <> nil) and (Current.Right <> nil) then
    begin
        // find the successor in tree
      Successor := Current.Right;
      while Successor.Left <> nil do
        Successor := Successor.Left;

      if Successor <> Current.Right then
      begin
          // remove references to successor
        if Successor.Parent.Left = Successor then
          Successor.Parent.Left := Successor.Right
        else
          Successor.Parent.Right := Successor.Right;
        if Successor.Right <> nil then
          Successor.Right.Parent := Successor.Parent;
        Successor.Right := Current.Right;
      end;

        // insert successor in new position
      Successor.Parent := Current.Parent;
      Successor.Left := Current.Left;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Successor
        else
          Current.Parent.Right := Successor;
      end
      else
          // fix root
        FRoot := Successor;
      Successor := Current.Parent;
      if Successor <> nil then
        Successor := FRoot;
    end
    else
    begin
        // (Current.Left = nil) and (Current.Right = nil)
      Successor := Current.Parent;
      if Successor <> nil then
      begin
          // remove references from parent
        if Successor.Left = Current then
          Successor.Left := nil
        else
          Successor.Right := nil;
      end
      else
        FRoot := nil;
    end;
    FreePointer(Current.Value);
    Current.Free;
    Dec(FSize);
    Current := Successor;
  until FRemoveSingleElement or (Current = nil);
  AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTree.RemoveAll(const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTree.RetainAll(const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclPtrBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclPtrBinaryTree.Size: Integer;
begin
  Result := FSize;
end;
{$ENDIF ~CLR}


//=== { TJclBinaryTree } =================================================

constructor TJclBinaryTree.Create(ACompare: TCompare; AOwnsObjects: Boolean);
begin
  inherited Create(nil, AOwnsObjects);
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  FCompare := ACompare;
end;

destructor TJclBinaryTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclBinaryTree.Add(AObject: TObject): Boolean;
var
  NewNode, Current, Save: TJclBinaryNode;
  Comp, Depth: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
  if FAllowDefaultElements or not ItemsEqual(AObject, nil) then
  begin
    Save := nil;
    Current := FRoot;
    Comp := 1;
    Depth := 0;
    while Current <> nil do
    begin
      Inc(Depth);
      Save := Current;
      Comp := ItemsCompare(AObject, Current.Value);
      if Comp < 0 then
        Current := Current.Left
      else
      if Comp > 0 then
        Current := Current.Right
      else
        Break;
    end;
    if (Comp <> 0) or CheckDuplicate then
    begin
      NewNode := TJclBinaryNode.Create;
      NewNode.Value := AObject;
      NewNode.Parent := Save;
      if Save = nil then
        FRoot := NewNode
      else
      if ItemsCompare(NewNode.Value, Save.Value) < 0 then
        Save.Left := NewNode
      else
        Save.Right := NewNode;
      Inc(FSize);
      Inc(Depth);
      if Depth > FMaxDepth then
        FMaxDepth := Depth;
      Result := True;
      AutoPack;
    end
    else
      Result := False;
  end
  else
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.AddAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclBinaryNode): TJclBinaryNode;
  begin
    Result := TJclBinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclBinaryTree;
  ACollection: IJclCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclBinaryTree then
  begin
    ADest := TJclBinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclBinaryTree then
    TJclBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclBinaryTree.Clear;
var
  Current, Parent: TJclBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // postorder
  Current := FRoot;
  if Current = nil then
    Exit;
    // find first in post-order
  while (Current.Left <> nil) or (Current.Right <> nil) do
  begin
    if Current.Left <> nil then
      Current := Current.Left
    else
      Current := Current.Right;
  end;
    // for all items in the tree in post-order
  repeat
    Parent := Current.Parent;
      // remove reference
    if Parent <> nil then
    begin
      if Parent.Left = Current then
        Parent.Left := nil
      else
      if Parent.Right = Current then
        Parent.Right := nil;
    end;

      // free item
    FreeObject(Current.Value);
    Current.Free;

      // find next item
    Current := Parent;
    if (Current <> nil) and (Current.Right <> nil) then
    begin
      Current := Current.Right;
      while (Current.Left <> nil) or (Current.Right <> nil) do
      begin
        if Current.Left <> nil then
          Current := Current.Left
        else
          Current := Current.Right;
      end;
    end;
  until Current = nil;
  FRoot := nil;
  FSize := 0;
  FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.Contains(AObject: TObject): Boolean;
var
  Comp: Integer;
  Current: TJclBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  Current := FRoot;
  while Current <> nil do
  begin
    Comp := ItemsCompare(Current.Value, AObject);
    if Comp = 0 then
    begin
      Result := True;
      Break;
    end
    else
    if Comp > 0 then
      Current := Current.Left
    else
      Current := Current.Right;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.ContainsAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while Result and It.HasNext do
    Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBinaryTree.Create(FCompare, False);
  AssignPropertiesTo(Result);
end;

function TJclBinaryTree.Equals(const ACollection: IJclCollection): Boolean;
var
  It, ItSelf: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  if FSize <> ACollection.Size then
    Exit;
  Result := True;
  It := ACollection.First;
  ItSelf := First;
  while ItSelf.HasNext do
    if not ItemsEqual(ItSelf.Next, It.Next) then
    begin
      Result := False;
      Break;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.First: IJclIterator;
var
  Start: TJclBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Start := FRoot;
  case GetTraverseOrder of
    toPreOrder:
      Result := TPreOrderItr.Create(Self, Start, False);
    toOrder:
    begin
      if Start <> nil then
        while Start.Left <> nil do
          Start := Start.Left;
      Result := TInOrderItr.Create(Self, Start, False);
    end;
    toPostOrder:
    begin
      if Start <> nil then
        while (Start.Left <> nil) or (Start.Right <> nil) do
        begin
          if Start.Left <> nil then
            Start := Start.Left
          else
            Start := Start.Right;
        end;
      Result := TPostOrderItr.Create(Self, Start, False);
    end;
  else
    Result := nil;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclBinaryTree.GetEnumerator: IJclIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclBinaryTree.ItemsCompare(A, B: TObject): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;

function TJclBinaryTree.ItemsEqual(A, B: TObject): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;

function TJclBinaryTree.Last: IJclIterator;
var
  Start: TJclBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
  Start := FRoot;
  case FTraverseOrder of
    toPreOrder:
    begin
      if Start <> nil then
        while (Start.Left <> nil) or (Start.Right <> nil) do
        begin
          if Start.Right <> nil then
            Start := Start.Right
          else
            Start := Start.Left;
        end;
      Result := TPreOrderItr.Create(Self, Start, False);
    end;
    toOrder:
    begin
      if Start <> nil then
        while Start.Right <> nil do
          Start := Start.Right;
      Result := TInOrderItr.Create(Self, Start, False);
    end;
    toPostOrder:
      Result := TPostOrderItr.Create(Self, Start, False);
  else
    Result := nil;
  end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBinaryTree.Pack;
type
  TLeafArray = array of TJclBinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclBinaryNode;
    Offset: Integer): TJclBinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclBinaryNode;
  Index:     Integer;
begin
  SetLength(Leafarray, FSize);
  try
    // in order enumeration of nodes
    ANode := FRoot;
    if ANode <> nil then
    begin
      // find first node
      while ANode.Left <> nil do
        ANode := ANode.Left;

      Index := 0;
      while ANode <> nil do
      begin
        LeafArray[Index] := ANode;
        Inc(Index);
        if ANode.Right <> nil then
        begin
          ANode := ANode.Right;
          while (ANode.Left <> nil) do
            ANode := ANode.Left;
        end
        else
        begin
          BNode := ANode;
          ANode := ANode.Parent;
          while (ANode <> nil) and (ANode.Right = BNode) do
          begin
            BNode := ANode;
            ANode := ANode.Parent;
          end;
        end;
      end;

      Index := FSize shr 1;
      FRoot := LeafArray[Index];
      FRoot.Parent := nil;
      if Index > 0 then
        FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
      else
        FRoot.Left := nil;
      if Index < (FSize - 1) then
        FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
      else
        FRoot.Right := nil;
    end;
  finally
    SetLength(LeafArray, 0);
  end;
end;

function TJclBinaryTree.Remove(AObject: TObject): Boolean;
var
  Current, Successor: TJclBinaryNode;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
    // locate AObject in the tree
  Current := FRoot;
  repeat
    while Current <> nil do
    begin
      Comp := ItemsCompare(AObject, Current.Value);
      if Comp = 0 then
        Break
      else
      if Comp < 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    if Current = nil then
      Break;
    Result := True;
      // Remove Current from tree
    if (Current.Left = nil) and (Current.Right <> nil) then
    begin
        // remove references to Current
      Current.Right.Parent := Current.Parent;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Current.Right
        else
          Current.Parent.Right := Current.Right;
      end
      else
          // fix root
        FRoot := Current.Right;
      Successor := Current.Parent;
      if Successor = nil then
        Successor := FRoot;
    end
    else
    if (Current.Left <> nil) and (Current.Right = nil) then
    begin
        // remove references to Current
      Current.Left.Parent := Current.Parent;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Current.Left
        else
          Current.Parent.Right := Current.Left;
      end
      else
          // fix root
        FRoot := Current.Left;
      Successor := Current.Parent;
      if Successor = nil then
        Successor := FRoot;
    end
    else
    if (Current.Left <> nil) and (Current.Right <> nil) then
    begin
        // find the successor in tree
      Successor := Current.Right;
      while Successor.Left <> nil do
        Successor := Successor.Left;

      if Successor <> Current.Right then
      begin
          // remove references to successor
        if Successor.Parent.Left = Successor then
          Successor.Parent.Left := Successor.Right
        else
          Successor.Parent.Right := Successor.Right;
        if Successor.Right <> nil then
          Successor.Right.Parent := Successor.Parent;
        Successor.Right := Current.Right;
      end;

        // insert successor in new position
      Successor.Parent := Current.Parent;
      Successor.Left := Current.Left;
      if Current.Parent <> nil then
      begin
        if Current.Parent.Left = Current then
          Current.Parent.Left := Successor
        else
          Current.Parent.Right := Successor;
      end
      else
          // fix root
        FRoot := Successor;
      Successor := Current.Parent;
      if Successor <> nil then
        Successor := FRoot;
    end
    else
    begin
        // (Current.Left = nil) and (Current.Right = nil)
      Successor := Current.Parent;
      if Successor <> nil then
      begin
          // remove references from parent
        if Successor.Left = Current then
          Successor.Left := nil
        else
          Successor.Right := nil;
      end
      else
        FRoot := nil;
    end;
    FreeObject(Current.Value);
    Current.Free;
    Dec(FSize);
    Current := Successor;
  until FRemoveSingleElement or (Current = nil);
  AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.RemoveAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.RetainAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  Result := True;
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclBinaryTree.Size: Integer;
begin
  Result := FSize;
end;

{$IFDEF SUPPORTS_GENERICS}


//=== { TJclBinaryTree<T> } =================================================

constructor TJclBinaryTree<T>.Create(AOwnsItems: Boolean);
begin
  inherited Create(nil, AOwnsItems);
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
end;

destructor TJclBinaryTree<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclBinaryTree<T>.Add(const AItem: T): Boolean;
var
  NewNode, Current, Save: TJclBinaryNode<T>;
  Comp, Depth: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AItem, Default(T)) then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AItem, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclBinaryNode<T>.Create;
        NewNode.Value := AItem;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) < 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.AddAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBinaryTree<T>.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclBinaryNode<T>): TJclBinaryNode<T>;
  begin
    Result := TJclBinaryNode<T>.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclBinaryTree<T>;
  ACollection: IJclCollection<T>;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclBinaryTree<T> then
  begin
    ADest := TJclBinaryTree<T>(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclCollection<T>, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclBinaryTree<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclBinaryTree<T> then
    TJclBinaryTree<T>(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclBinaryTree<T>.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclBinaryTree<T>.Clear;
var
  Current, Parent: TJclBinaryNode<T>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeItem(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.Contains(const AItem: T): Boolean;
var
  Comp: Integer;
  Current: TJclBinaryNode<T>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AItem);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.ContainsAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.Equals(const ACollection: IJclCollection<T>): Boolean;
var
  It, ItSelf: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.First: IJclIterator<T>;
var
  Start: TJclBinaryNode<T>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderItr<T>.Create(Self, Start, False);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TInOrderItr<T>.Create(Self, Start, False);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TPostOrderItr<T>.Create(Self, Start, False);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclBinaryTree<T>.GetEnumerator: IJclIterator<T>;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclBinaryTree<T>.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclBinaryTree<T>.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclBinaryTree<T>.Last: IJclIterator<T>;
var
  Start: TJclBinaryNode<T>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TPreOrderItr<T>.Create(Self, Start, False);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TInOrderItr<T>.Create(Self, Start, False);
        end;
      toPostOrder:
        Result := TPostOrderItr<T>.Create(Self, Start, False);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBinaryTree<T>.Pack;
type
  TLeafArray = array of TJclBinaryNode<T>;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclBinaryNode<T>;
    Offset: Integer): TJclBinaryNode<T>;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclBinaryNode<T>;
  Index: Integer;
begin
  SetLength(Leafarray, FSize);
  try
    // in order enumeration of nodes
    ANode := FRoot;
    if ANode <> nil then
    begin
      // find first node
      while ANode.Left <> nil do
        ANode := ANode.Left;

      Index := 0;
      while ANode <> nil do
      begin
        LeafArray[Index] := ANode;
        Inc(Index);
        if ANode.Right <> nil then
        begin
          ANode := ANode.Right;
          while (ANode.Left <> nil) do
            ANode := ANode.Left;
        end
        else
        begin
          BNode := ANode;
          ANode := ANode.Parent;
          while (ANode <> nil) and (ANode.Right = BNode) do
          begin
            BNode := ANode;
            ANode := ANode.Parent;
          end;
        end;
      end;

      Index := FSize shr 1;
      FRoot := LeafArray[Index];
      FRoot.Parent := nil;
      if Index > 0 then
        FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
      else
        FRoot.Left := nil;
      if Index < (FSize - 1) then
        FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
      else
        FRoot.Right := nil;
    end;
  finally
    SetLength(LeafArray, 0);
  end;
end;

function TJclBinaryTree<T>.Remove(const AItem: T): Boolean;
var
  Current, Successor: TJclBinaryNode<T>;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AItem in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AItem, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          if Successor.Parent.Left = Successor then
            Successor.Parent.Left := Successor.Right
          else
            Successor.Parent.Right := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
        end;

        // insert successor in new position
        Successor.Parent := Current.Parent;
        Successor.Left := Current.Left;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      FreeItem(Current.Value);
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.RemoveAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.RetainAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBinaryTree<T>.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclBinaryTree<T>.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclBinaryTree<T>.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclBinaryTreeE<T> } =================================================

constructor TJclBinaryTreeE<T>.Create(const AComparer: IComparer<T>; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  FComparer := AComparer;
end;

procedure TJclBinaryTreeE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclBinaryTreeE<T> then
    TJclBinaryTreeE<T>(Dest).FComparer := FComparer;
end;

function TJclBinaryTreeE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBinaryTreeE<T>.Create(Comparer, False);
  AssignPropertiesTo(Result);
end;

function TJclBinaryTreeE<T>.ItemsCompare(const A, B: T): Integer;
begin
  if Comparer = nil then
    raise EJclNoComparerError.Create;
  Result := Comparer.Compare(A, B);
end;

function TJclBinaryTreeE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Comparer = nil then
    raise EJclNoComparerError.Create;
  Result := Comparer.Compare(A, B) = 0;
end;

//=== { TJclBinaryTreeF<T> } =================================================

constructor TJclBinaryTreeF<T>.Create(ACompare: TCompare<T>; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  FCompare := ACompare;
end;

procedure TJclBinaryTreeF<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclBinaryTreeF<T> then
    TJclBinaryTreeF<T>(Dest).FCompare := FCompare;
end;

function TJclBinaryTreeF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBinaryTreeF<T>.Create(Compare, False);
  AssignPropertiesTo(Result);
end;

function TJclBinaryTreeF<T>.ItemsCompare(const A, B: T): Integer;
begin
  if not Assigned(Compare) then
    raise EJclNoComparerError.Create;
  Result := Compare(A, B);
end;

function TJclBinaryTreeF<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if not Assigned(Compare) then
    raise EJclNoComparerError.Create;
  Result := Compare(A, B) = 0;
end;

//=== { TJclBinaryTreeI<T> } =================================================

function TJclBinaryTreeI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBinaryTreeI<T>.Create(False);
  AssignPropertiesTo(Result);
end;

function TJclBinaryTreeI<T>.ItemsCompare(const A, B: T): Integer;
begin
  Result := A.CompareTo(B);
end;

function TJclBinaryTreeI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  Result := A.CompareTo(B) = 0;
end;

{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
