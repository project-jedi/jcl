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
{ The Original Code is HashMap.pas.                                                                }
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

unit JclHashMaps;

{$I jcl.inc}

interface

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

{$I containers\JclHashMaps.imp}
type
  // Hash Function
  // Result must be in 0..Range-1
  TJclHashFunction = function(Key, Range: Integer): Integer;

(*$JPPEXPANDMACRO JCLHASHMAPINT(IInterface,IInterface,TJclIntfIntfEntry,TJclIntfIntfEntryArray,TJclIntfIntfBucket,TJclIntfIntfBucketArray,TJclIntfIntfHashMap,TJclAbstractContainerBase,IJclIntfIntfMap,IJclIntfSet,IJclIntfCollection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: IInterface): IInterface;
    function Hash(const AInterface: IInterface): Integer;
    function KeysEqual(const A\, B: IInterface): Boolean;
    function ValuesEqual(const A\, B: IInterface): Boolean;,,,,const Key: IInterface,const Value: IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(AnsiString,IInterface,TJclAnsiStrIntfEntry,TJclAnsiStrIntfEntryArray,TJclAnsiStrIntfBucket,TJclAnsiStrIntfBucketArray,TJclAnsiStrIntfHashMap,TJclAnsiStrAbstractContainer,IJclAnsiStrIntfMap,IJclAnsiStrSet,IJclIntfCollection, IJclStrContainer\, IJclAnsiStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A\, B: AnsiString): Boolean;
    function ValuesEqual(const A\, B: IInterface): Boolean;,,,,const Key: AnsiString,const Value: IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(IInterface,AnsiString,TJclIntfAnsiStrEntry,TJclIntfAnsiStrEntryArray,TJclIntfAnsiStrBucket,TJclIntfAnsiStrBucketArray,TJclIntfAnsiStrHashMap,TJclAnsiStrAbstractContainer,IJclIntfAnsiStrMap,IJclIntfSet,IJclAnsiStrCollection, IJclStrContainer\, IJclAnsiStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: AnsiString): AnsiString;
    function Hash(const AInterface: IInterface): Integer;
    function KeysEqual(const A\, B: IInterface): Boolean;
    function ValuesEqual(const A\, B: AnsiString): Boolean;,,,,const Key: IInterface,const Value: AnsiString)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(AnsiString,AnsiString,TJclAnsiStrAnsiStrEntry,TJclAnsiStrAnsiStrEntryArray,TJclAnsiStrAnsiStrBucket,TJclAnsiStrAnsiStrBucketArray,TJclAnsiStrAnsiStrHashMap,TJclAnsiStrAbstractContainer,IJclAnsiStrAnsiStrMap,IJclAnsiStrSet,IJclAnsiStrCollection, IJclStrContainer\, IJclAnsiStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function FreeValue(var Value: AnsiString): AnsiString;
    function KeysEqual(const A\, B: AnsiString): Boolean;
    function ValuesEqual(const A\, B: AnsiString): Boolean;,,,,const Key: AnsiString,const Value: AnsiString)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(WideString,IInterface,TJclWideStrIntfEntry,TJclWideStrIntfEntryArray,TJclWideStrIntfBucket,TJclWideStrIntfBucketArray,TJclWideStrIntfHashMap,TJclWideStrAbstractContainer,IJclWideStrIntfMap,IJclWideStrSet,IJclIntfCollection, IJclStrContainer\, IJclWideStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A\, B: WideString): Boolean;
    function ValuesEqual(const A\, B: IInterface): Boolean;,,,,const Key: WideString,const Value: IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(IInterface,WideString,TJclIntfWideStrEntry,TJclIntfWideStrEntryArray,TJclIntfWideStrBucket,TJclIntfWideStrBucketArray,TJclIntfWideStrHashMap,TJclWideStrAbstractContainer,IJclIntfWideStrMap,IJclIntfSet,IJclWideStrCollection, IJclStrContainer\, IJclWideStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: WideString): WideString;
    function Hash(const AInterface: IInterface): Integer;
    function KeysEqual(const A\, B: IInterface): Boolean;
    function ValuesEqual(const A\, B: WideString): Boolean;,,,,const Key: IInterface,const Value: WideString)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(WideString,WideString,TJclWideStrWideStrEntry,TJclWideStrWideStrEntryArray,TJclWideStrWideStrBucket,TJclWideStrWideStrBucketArray,TJclWideStrWideStrHashMap,TJclWideStrAbstractContainer,IJclWideStrWideStrMap,IJclWideStrSet,IJclWideStrCollection, IJclStrContainer\, IJclWideStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function FreeValue(var Value: WideString): WideString;
    function KeysEqual(const A\, B: WideString): Boolean;
    function ValuesEqual(const A\, B: WideString): Boolean;,,,,const Key: WideString,const Value: WideString)*)

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrIntfHashMap = TJclAnsiStrIntfHashMap;
  TJclIntfStrHashMap = TJclIntfAnsiStrHashMap;
  TJclStrStrHashMap = TJclAnsiStrAnsiStrHashMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrIntfHashMap = TJclWideStrIntfHashMap;
  TJclIntfStrHashMap = TJclIntfWideStrHashMap;
  TJclStrStrHashMap = TJclWideStrWideStrHashMap;
  {$ENDIF CONTAINER_WIDESTR}

(*$JPPEXPANDMACRO JCLHASHMAPINT(Single,IInterface,TJclSingleIntfEntry,TJclSingleIntfEntryArray,TJclSingleIntfBucket,TJclSingleIntfBucketArray,TJclSingleIntfHashMap,TJclSingleAbstractContainer,IJclSingleIntfMap,IJclSingleSet,IJclIntfCollection, IJclSingleContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Single): Single;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A\, B: Single): Boolean;
    function ValuesEqual(const A\, B: IInterface): Boolean;,,,,const Key: Single,const Value: IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(IInterface,Single,TJclIntfSingleEntry,TJclIntfSingleEntryArray,TJclIntfSingleBucket,TJclIntfSingleBucketArray,TJclIntfSingleHashMap,TJclSingleAbstractContainer,IJclIntfSingleMap,IJclIntfSet,IJclSingleCollection, IJclSingleContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Single): Single;
    function Hash(const AInterface: IInterface): Integer;
    function KeysEqual(const A\, B: IInterface): Boolean;
    function ValuesEqual(const A\, B: Single): Boolean;,,,,const Key: IInterface,const Value: Single)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(Single,Single,TJclSingleSingleEntry,TJclSingleSingleEntryArray,TJclSingleSingleBucket,TJclSingleSingleBucketArray,TJclSingleSingleHashMap,TJclSingleAbstractContainer,IJclSingleSingleMap,IJclSingleSet,IJclSingleCollection, IJclSingleContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Single): Single;
    function FreeValue(var Value: Single): Single;
    function KeysEqual(const A\, B: Single): Boolean;
    function ValuesEqual(const A\, B: Single): Boolean;,,,,const Key: Single,const Value: Single)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(Double,IInterface,TJclDoubleIntfEntry,TJclDoubleIntfEntryArray,TJclDoubleIntfBucket,TJclDoubleIntfBucketArray,TJclDoubleIntfHashMap,TJclDoubleAbstractContainer,IJclDoubleIntfMap,IJclDoubleSet,IJclIntfCollection, IJclDoubleContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Double): Double;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A\, B: Double): Boolean;
    function ValuesEqual(const A\, B: IInterface): Boolean;,,,,const Key: Double,const Value: IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(IInterface,Double,TJclIntfDoubleEntry,TJclIntfDoubleEntryArray,TJclIntfDoubleBucket,TJclIntfDoubleBucketArray,TJclIntfDoubleHashMap,TJclDoubleAbstractContainer,IJclIntfDoubleMap,IJclIntfSet,IJclDoubleCollection, IJclDoubleContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Double): Double;
    function Hash(const AInterface: IInterface): Integer;
    function KeysEqual(const A\, B: IInterface): Boolean;
    function ValuesEqual(const A\, B: Double): Boolean;,,,,const Key: IInterface,const Value: Double)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(Double,Double,TJclDoubleDoubleEntry,TJclDoubleDoubleEntryArray,TJclDoubleDoubleBucket,TJclDoubleDoubleBucketArray,TJclDoubleDoubleHashMap,TJclDoubleAbstractContainer,IJclDoubleDoubleMap,IJclDoubleSet,IJclDoubleCollection, IJclDoubleContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Double): Double;
    function FreeValue(var Value: Double): Double;
    function KeysEqual(const A\, B: Double): Boolean;
    function ValuesEqual(const A\, B: Double): Boolean;,,,,const Key: Double,const Value: Double)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(Extended,IInterface,TJclExtendedIntfEntry,TJclExtendedIntfEntryArray,TJclExtendedIntfBucket,TJclExtendedIntfBucketArray,TJclExtendedIntfHashMap,TJclExtendedAbstractContainer,IJclExtendedIntfMap,IJclExtendedSet,IJclIntfCollection, IJclExtendedContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Extended): Extended;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A\, B: Extended): Boolean;
    function ValuesEqual(const A\, B: IInterface): Boolean;,,,,const Key: Extended,const Value: IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(IInterface,Extended,TJclIntfExtendedEntry,TJclIntfExtendedEntryArray,TJclIntfExtendedBucket,TJclIntfExtendedBucketArray,TJclIntfExtendedHashMap,TJclExtendedAbstractContainer,IJclIntfExtendedMap,IJclIntfSet,IJclExtendedCollection, IJclExtendedContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Extended): Extended;
    function Hash(const AInterface: IInterface): Integer;
    function KeysEqual(const A\, B: IInterface): Boolean;
    function ValuesEqual(const A\, B: Extended): Boolean;,,,,const Key: IInterface,const Value: Extended)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(Extended,Extended,TJclExtendedExtendedEntry,TJclExtendedExtendedEntryArray,TJclExtendedExtendedBucket,TJclExtendedExtendedBucketArray,TJclExtendedExtendedHashMap,TJclExtendedAbstractContainer,IJclExtendedExtendedMap,IJclExtendedSet,IJclExtendedCollection, IJclExtendedContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Extended): Extended;
    function FreeValue(var Value: Extended): Extended;
    function KeysEqual(const A\, B: Extended): Boolean;
    function ValuesEqual(const A\, B: Extended): Boolean;,,,,const Key: Extended,const Value: Extended)*)

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatIntfHashMap = TJclExtendedIntfHashMap;
  TJclIntfFloatHashMap = TJclIntfExtendedHashMap;
  TJclFloatFloatHashMap = TJclExtendedExtendedHashMap;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatIntfHashMap = TJclDoubleIntfHashMap;
  TJclIntfFloatHashMap = TJclIntfDoubleHashMap;
  TJclFloatFloatHashMap = TJclDoubleDoubleHashMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatIntfHashMap = TJclSingleIntfHashMap;
  TJclIntfFloatHashMap = TJclIntfSingleHashMap;
  TJclFloatFloatHashMap = TJclSingleSingleHashMap;
  {$ENDIF MATH_SINGLE_PRECISION}

(*$JPPEXPANDMACRO JCLHASHMAPINT(Integer,IInterface,TJclIntegerIntfEntry,TJclIntegerIntfEntryArray,TJclIntegerIntfBucket,TJclIntegerIntfBucketArray,TJclIntegerIntfHashMap,TJclIntegerAbstractContainer,IJclIntegerIntfMap,IJclIntegerSet,IJclIntfCollection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Integer): Integer;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(A\, B: Integer): Boolean;
    function ValuesEqual(const A\, B: IInterface): Boolean;,,,,Key: Integer,const Value: IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(IInterface,Integer,TJclIntfIntegerEntry,TJclIntfIntegerEntryArray,TJclIntfIntegerBucket,TJclIntfIntegerBucketArray,TJclIntfIntegerHashMap,TJclIntegerAbstractContainer,IJclIntfIntegerMap,IJclIntfSet,IJclIntegerCollection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Integer): Integer;
    function Hash(const AInterface: IInterface): Integer;
    function KeysEqual(const A\, B: IInterface): Boolean;
    function ValuesEqual(A\, B: Integer): Boolean;,,,,const Key: IInterface,Value: Integer)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(Integer,Integer,TJclIntegerIntegerEntry,TJclIntegerIntegerEntryArray,TJclIntegerIntegerBucket,TJclIntegerIntegerBucketArray,TJclIntegerIntegerHashMap,TJclIntegerAbstractContainer,IJclIntegerIntegerMap,IJclIntegerSet,IJclIntegerCollection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Integer): Integer;
    function FreeValue(var Value: Integer): Integer;
    function KeysEqual(A\, B: Integer): Boolean;
    function ValuesEqual(A\, B: Integer): Boolean;,,,,Key: Integer,Value: Integer)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(Cardinal,IInterface,TJclCardinalIntfEntry,TJclCardinalIntfEntryArray,TJclCardinalIntfBucket,TJclCardinalIntfBucketArray,TJclCardinalIntfHashMap,TJclCardinalAbstractContainer,IJclCardinalIntfMap,IJclCardinalSet,IJclIntfCollection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Cardinal): Cardinal;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(A\, B: Cardinal): Boolean;
    function ValuesEqual(const A\, B: IInterface): Boolean;,,,,Key: Cardinal,const Value: IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(IInterface,Cardinal,TJclIntfCardinalEntry,TJclIntfCardinalEntryArray,TJclIntfCardinalBucket,TJclIntfCardinalBucketArray,TJclIntfCardinalHashMap,TJclCardinalAbstractContainer,IJclIntfCardinalMap,IJclIntfSet,IJclCardinalCollection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Cardinal): Cardinal;
    function Hash(const AInterface: IInterface): Integer;
    function KeysEqual(const A\, B: IInterface): Boolean;
    function ValuesEqual(A\, B: Cardinal): Boolean;,,,,const Key: IInterface,Value: Cardinal)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(Cardinal,Cardinal,TJclCardinalCardinalEntry,TJclCardinalCardinalEntryArray,TJclCardinalCardinalBucket,TJclCardinalCardinalBucketArray,TJclCardinalCardinalHashMap,TJclCardinalAbstractContainer,IJclCardinalCardinalMap,IJclCardinalSet,IJclCardinalCollection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Cardinal): Cardinal;
    function FreeValue(var Value: Cardinal): Cardinal;
    function KeysEqual(A\, B: Cardinal): Boolean;
    function ValuesEqual(A\, B: Cardinal): Boolean;,,,,Key: Cardinal,Value: Cardinal)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(Int64,IInterface,TJclInt64IntfEntry,TJclInt64IntfEntryArray,TJclInt64IntfBucket,TJclInt64IntfBucketArray,TJclInt64IntfHashMap,TJclInt64AbstractContainer,IJclInt64IntfMap,IJclInt64Set,IJclIntfCollection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Int64): Int64;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A\, B: Int64): Boolean;
    function ValuesEqual(const A\, B: IInterface): Boolean;,,,,const Key: Int64,const Value: IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(IInterface,Int64,TJclIntfInt64Entry,TJclIntfInt64EntryArray,TJclIntfInt64Bucket,TJclIntfInt64BucketArray,TJclIntfInt64HashMap,TJclInt64AbstractContainer,IJclIntfInt64Map,IJclIntfSet,IJclInt64Collection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Int64): Int64;
    function Hash(const AInterface: IInterface): Integer;
    function KeysEqual(const A\, B: IInterface): Boolean;
    function ValuesEqual(const A\, B: Int64): Boolean;,,,,const Key: IInterface,const Value: Int64)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(Int64,Int64,TJclInt64Int64Entry,TJclInt64Int64EntryArray,TJclInt64Int64Bucket,TJclInt64Int64BucketArray,TJclInt64Int64HashMap,TJclInt64AbstractContainer,IJclInt64Int64Map,IJclInt64Set,IJclInt64Collection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Int64): Int64;
    function FreeValue(var Value: Int64): Int64;
    function KeysEqual(const A\, B: Int64): Boolean;
    function ValuesEqual(const A\, B: Int64): Boolean;,,,,const Key: Int64,const Value: Int64)*)

  {$IFNDEF CLR}
(*$JPPEXPANDMACRO JCLHASHMAPINT(Pointer,IInterface,TJclPtrIntfEntry,TJclPtrIntfEntryArray,TJclPtrIntfBucket,TJclPtrIntfBucketArray,TJclPtrIntfHashMap,TJclPtrAbstractContainer,IJclPtrIntfMap,IJclPtrSet,IJclIntfCollection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Pointer): Pointer;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(A\, B: Pointer): Boolean;
    function ValuesEqual(const A\, B: IInterface): Boolean;,,,,Key: Pointer,const Value: IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(IInterface,Pointer,TJclIntfPtrEntry,TJclIntfPtrEntryArray,TJclIntfPtrBucket,TJclIntfPtrBucketArray,TJclIntfPtrHashMap,TJclPtrAbstractContainer,IJclIntfPtrMap,IJclIntfSet,IJclPtrCollection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Pointer): Pointer;
    function Hash(const AInterface: IInterface): Integer;
    function KeysEqual(const A\, B: IInterface): Boolean;
    function ValuesEqual(A\, B: Pointer): Boolean;,,,,const Key: IInterface,Value: Pointer)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(Pointer,Pointer,TJclPtrPtrEntry,TJclPtrPtrEntryArray,TJclPtrPtrBucket,TJclPtrPtrBucketArray,TJclPtrPtrHashMap,TJclPtrAbstractContainer,IJclPtrPtrMap,IJclPtrSet,IJclPtrCollection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Pointer): Pointer;
    function FreeValue(var Value: Pointer): Pointer;
    function KeysEqual(A\, B: Pointer): Boolean;
    function ValuesEqual(A\, B: Pointer): Boolean;,,,,Key: Pointer,Value: Pointer)*)
  {$ENDIF ~CLR}

(*$JPPEXPANDMACRO JCLHASHMAPINT(IInterface,TObject,TJclIntfEntry,TJclIntfEntryArray,TJclIntfBucket,TJclIntfBucketArray,TJclIntfHashMap,TJclAbstractContainerBase,IJclIntfMap,IJclIntfSet,IJclCollection, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function Hash(const AInterface: IInterface): Integer;
    function KeysEqual(const A\, B: IInterface): Boolean;
    function ValuesEqual(A\, B: TObject): Boolean;,
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const Key: IInterface,Value: TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(AnsiString,TObject,TJclAnsiStrEntry,TJclAnsiStrEntryArray,TJclAnsiStrBucket,TJclAnsiStrBucketArray,TJclAnsiStrHashMap,TJclAnsiStrAbstractContainer,IJclAnsiStrMap,IJclAnsiStrSet,IJclCollection, IJclStrContainer\, IJclAnsiStrContainer\, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function KeysEqual(const A\, B: AnsiString): Boolean;
    function ValuesEqual(A\, B: TObject): Boolean;,
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const Key: AnsiString,Value: TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(WideString,TObject,TJclWideStrEntry,TJclWideStrEntryArray,TJclWideStrBucket,TJclWideStrBucketArray,TJclWideStrHashMap,TJclwideStrAbstractContainer,IJclWideStrMap,IJclWideStrSet,IJclCollection, IJclStrContainer\, IJclWideStrContainer\, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function KeysEqual(const A\, B: WideString): Boolean;
    function ValuesEqual(A\, B: TObject): Boolean;,
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const Key: WideString,Value: TObject)*)

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrHashMap = TJclAnsiStrHashMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrHashMap = TJclWideStrHashMap;
  {$ENDIF CONTAINER_WIDESTR}

(*$JPPEXPANDMACRO JCLHASHMAPINT(Single,TObject,TJclSingleEntry,TJclSingleEntryArray,TJclSingleBucket,TJclSingleBucketArray,TJclSingleHashMap,TJclSingleAbstractContainer,IJclSingleMap,IJclSingleSet,IJclCollection, IJclSingleContainer\, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Single): Single;
    function KeysEqual(const A\, B: Single): Boolean;
    function ValuesEqual(A\, B: TObject): Boolean;,
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const Key: Single,Value: TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(Double,TObject,TJclDoubleEntry,TJclDoubleEntryArray,TJclDoubleBucket,TJclDoubleBucketArray,TJclDoubleHashMap,TJclDoubleAbstractContainer,IJclDoubleMap,IJclDoubleSet,IJclCollection, IJclDoubleContainer\, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Double): Double;
    function KeysEqual(const A\, B: Double): Boolean;
    function ValuesEqual(A\, B: TObject): Boolean;,
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const Key: Double,Value: TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(Extended,TObject,TJclExtendedEntry,TJclExtendedEntryArray,TJclExtendedBucket,TJclExtendedBucketArray,TJclExtendedHashMap,TJclExtendedAbstractContainer,IJclExtendedMap,IJclExtendedSet,IJclCollection, IJclExtendedContainer\, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Extended): Extended;
    function KeysEqual(const A\, B: Extended): Boolean;
    function ValuesEqual(A\, B: TObject): Boolean;,
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const Key: Extended,Value: TObject)*)

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatHashMap = TJclExtendedHashMap;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatHashMap = TJclDoubleHashMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatHashMap = TJclSingleHashMap;
  {$ENDIF MATH_SINGLE_PRECISION}

(*$JPPEXPANDMACRO JCLHASHMAPINT(Integer,TObject,TJclIntegerEntry,TJclIntegerEntryArray,TJclIntegerBucket,TJclIntegerBucketArray,TJclIntegerHashMap,TJclIntegerAbstractContainer,IJclIntegerMap,IJclIntegerSet,IJclCollection, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Integer): Integer;
    function KeysEqual(A\, B: Integer): Boolean;
    function ValuesEqual(A\, B: TObject): Boolean;,
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,Key: Integer,Value: TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(Cardinal,TObject,TJclCardinalEntry,TJclCardinalEntryArray,TJclCardinalBucket,TJclCardinalBucketArray,TJclCardinalHashMap,TJclCardinalAbstractContainer,IJclCardinalMap,IJclCardinalSet,IJclCollection, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Cardinal): Cardinal;
    function KeysEqual(A\, B: Cardinal): Boolean;
    function ValuesEqual(A\, B: TObject): Boolean;,
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,Key: Cardinal,Value: TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(Int64,TObject,TJclInt64Entry,TJclInt64EntryArray,TJclInt64Bucket,TJclInt64BucketArray,TJclInt64HashMap,TJclInt64AbstractContainer,IJclInt64Map,IJclInt64Set,IJclCollection, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Int64): Int64;
    function KeysEqual(const A\, B: Int64): Boolean;
    function ValuesEqual(A\, B: TObject): Boolean;,
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const Key: Int64,Value: TObject)*)

  {$IFNDEF CLR}
(*$JPPEXPANDMACRO JCLHASHMAPINT(Pointer,TObject,TJclPtrEntry,TJclPtrEntryArray,TJclPtrBucket,TJclPtrBucketArray,TJclPtrHashMap,TJclPtrAbstractContainer,IJclPtrMap,IJclPtrSet,IJclCollection, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Pointer): Pointer;
    function KeysEqual(A\, B: Pointer): Boolean;
    function ValuesEqual(A\, B: TObject): Boolean;,
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,Key: Pointer,Value: TObject)*)
  {$ENDIF ~CLR}

(*$JPPEXPANDMACRO JCLHASHMAPINT(TObject,TObject,TJclEntry,TJclEntryArray,TJclBucket,TJclBucketArray,TJclHashMap,TJclAbstractContainerBase,IJclMap,IJclSet,IJclCollection, IJclKeyOwner\, IJclValueOwner\,,
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;,
    { IJclKeyOwner }
    function FreeKey(var Key: TObject): TObject;
    function GetOwnsKeys: Boolean;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function Hash(AObject: TObject): Integer;
    function KeysEqual(A\, B: TObject): Boolean;
    function ValuesEqual(A\, B: TObject): Boolean;,
    property OwnsKeys: Boolean read FOwnsKeys;
    property OwnsValues: Boolean read FOwnsValues;,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,Key: TObject,Value: TObject)*)

  {$IFDEF SUPPORTS_GENERICS}
(*$JPPEXPANDMACRO JCLHASHMAPINT(TKey,TValue,TJclEntry<TKey\,TValue>,TJclEntryArray<TKey\,TValue>,TJclBucket<TKey\,TValue>,TJclBucketArray<TKey\,TValue>,TJclHashMap<TKey\,TValue>,TJclAbstractContainerBase,IJclMap<TKey\,TValue>,IJclSet<TKey>,IJclCollection<TValue>, IJclPairOwner<TKey\, TValue>\,,
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;,
    { IJclPairOwner }
    function FreeKey(var Key: TKey): TKey;
    function FreeValue(var Value: TValue): TValue;
    function GetOwnsKeys: Boolean;
    function GetOwnsValues: Boolean;
    function Hash(const AKey: TKey): Integer; virtual; abstract;
    function KeysEqual(const A\, B: TKey): Boolean; virtual; abstract;
    function ValuesEqual(const A\, B: TValue): Boolean; virtual; abstract;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; virtual; abstract;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; virtual; abstract;,
    property OwnsKeys: Boolean read FOwnsKeys;
    property OwnsValues: Boolean read FOwnsValues;,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,const Key: TKey,const Value: TValue)*)

  // E = external helper to compare and hash items
  // KeyComparer is used only when getting KeySet
  // GetHashCode and Equals methods of KeyEqualityComparer are used
  // GetHashCode of ValueEqualityComparer is not used
  TJclHashMapE<TKey, TValue> = class(TJclHashMap<TKey, TValue>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  private
    FKeyEqualityComparer: IEqualityComparer<TKey>;
    FKeyComparer: IComparer<TKey>;
    FValueEqualityComparer: IEqualityComparer<TValue>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function Hash(const AKey: TKey): Integer; override;
    function KeysEqual(const A, B: TKey): Boolean; override;
    function ValuesEqual(const A, B: TValue): Boolean; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AKeyEqualityComparer: IEqualityComparer<TKey>;
      const AValueEqualityComparer: IEqualityComparer<TValue>;
      const AKeyComparer: IComparer<TKey>; ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);

    property KeyEqualityComparer: IEqualityComparer<TKey> read FKeyEqualityComparer write FKeyEqualityComparer;
    property KeyComparer: IComparer<TKey> read FKeyComparer write FKeyComparer;
    property ValueEqualityComparer: IEqualityComparer<TValue> read FValueEqualityComparer write FValueEqualityComparer;
  end;

  // F = Functions to compare and hash items
  // KeyComparer is used only when getting KeySet
  TJclHashMapF<TKey, TValue> = class(TJclHashMap<TKey, TValue>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  private
    FKeyEqualityCompare: TEqualityCompare<TKey>;
    FKeyHash: THash<TKey>;
    FKeyCompare: TCompare<TKey>;
    FValueEqualityCompare: TEqualityCompare<TValue>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function Hash(const AKey: TKey): Integer; override;
    function KeysEqual(const A, B: TKey): Boolean; override;
    function ValuesEqual(const A, B: TValue): Boolean; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(AKeyEqualityCompare: TEqualityCompare<TKey>; AKeyHash: THash<TKey>;
      AValueEqualityCompare: TEqualityCompare<TValue>; AKeyCompare: TCompare<TKey>;
      ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);

    property KeyEqualityCompare: TEqualityCompare<TKey> read FKeyEqualityCompare write FKeyEqualityCompare;
    property KeyCompare: TCompare<TKey> read FKeyCompare write FKeyCompare;
    property KeyHash: THash<TKey> read FKeyHash write FKeyHash;
    property ValueEqualityCompare: TEqualityCompare<TValue> read FValueEqualityCompare write FValueEqualityCompare;
  end;

  // I = items can compare themselves to an other, items can create hash value from themselves
  TJclHashMapI<TKey: IComparable<TKey>, IEquatable<TKey>, IHashable; TValue: IEquatable<TValue>> = class(TJclHashMap<TKey, TValue>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer,
    IJclMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  protected
    function Hash(const AKey: TKey): Integer; override;
    function KeysEqual(const A, B: TKey): Boolean; override;
    function ValuesEqual(const A, B: TValue): Boolean; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;
  {$ENDIF SUPPORTS_GENERICS}

function HashMul(Key, Range: Integer): Integer;

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
  JclArrayLists, JclArraySets, JclResources;

function HashMul(Key, Range: Integer): Integer;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Trunc(Range * (Frac(Key * A)));
end;

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfIntfHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfIntfHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH
function TJclIntfIntfHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;
}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclIntfIntfHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclIntfIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfIntfHashMap,TJclIntfIntfEntryArray,TJclIntfIntfBucket,IJclIntfIntfMap,IJclIntfSet,IJclIntfIterator,IJclIntfCollection,,,,const Key: IInterface,IInterface,nil,const Value: IInterface,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclAnsiStrIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrIntfHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclAnsiStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclAnsiStrIntfHashMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclAnsiStrIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclAnsiStrIntfHashMap.KeysEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclAnsiStrIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclAnsiStrIntfHashMap,TJclAnsiStrIntfEntryArray,TJclAnsiStrIntfBucket,IJclAnsiStrIntfMap,IJclAnsiStrSet,IJclAnsiStrIterator,IJclIntfCollection,,,,const Key: AnsiString,AnsiString,'',const Value: IInterface,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfAnsiStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfAnsiStrHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclAnsiStrArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfAnsiStrHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfAnsiStrHashMap.FreeValue(var Value: AnsiString): AnsiString;
begin
  Result := Value;
  Value := '';
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH
function TJclIntfAnsiStrHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;
}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclIntfAnsiStrHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclIntfAnsiStrHashMap.ValuesEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfAnsiStrHashMap,TJclIntfAnsiStrEntryArray,TJclIntfAnsiStrBucket,IJclIntfAnsiStrMap,IJclIntfSet,IJclIntfIterator,IJclAnsiStrCollection,,,,const Key: IInterface,IInterface,nil,const Value: AnsiString,AnsiString,'')}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclAnsiStrAnsiStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrAnsiStrHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclAnsiStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclAnsiStrArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclAnsiStrAnsiStrHashMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclAnsiStrAnsiStrHashMap.FreeValue(var Value: AnsiString): AnsiString;
begin
  Result := Value;
  Value := '';
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclAnsiStrAnsiStrHashMap.KeysEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclAnsiStrAnsiStrHashMap.ValuesEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclAnsiStrAnsiStrHashMap,TJclAnsiStrAnsiStrEntryArray,TJclAnsiStrAnsiStrBucket,IJclAnsiStrAnsiStrMap,IJclAnsiStrSet,IJclAnsiStrIterator,IJclAnsiStrCollection,,,,const Key: AnsiString,AnsiString,'',const Value: AnsiString,AnsiString,'')}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclWideStrIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrIntfHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclWideStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclWideStrIntfHashMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclWideStrIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclWideStrIntfHashMap.KeysEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclWideStrIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclWideStrIntfHashMap,TJclWideStrIntfEntryArray,TJclWideStrIntfBucket,IJclWideStrIntfMap,IJclWideStrSet,IJclWideStrIterator,IJclIntfCollection,,,,const Key: WideString,WideString,'',const Value: IInterface,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfWideStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfWideStrHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclWideStrArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfWideStrHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfWideStrHashMap.FreeValue(var Value: WideString): WideString;
begin
  Result := Value;
  Value := '';
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH
function TJclIntfWideStrHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;
}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclIntfWideStrHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclIntfWideStrHashMap.ValuesEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfWideStrHashMap,TJclIntfWideStrEntryArray,TJclIntfWideStrBucket,IJclIntfWideStrMap,IJclIntfSet,IJclIntfIterator,IJclWideStrCollection,,,,const Key: IInterface,IInterface,nil,const Value: WideString,WideString,'')}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclWideStrWideStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrWideStrHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclWideStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclWideStrArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclWideStrWideStrHashMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclWideStrWideStrHashMap.FreeValue(var Value: WideString): WideString;
begin
  Result := Value;
  Value := '';
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclWideStrWideStrHashMap.KeysEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclWideStrWideStrHashMap.ValuesEqual(const A, B: Widestring): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclWideStrWideStrHashMap,TJclWideStrWideStrEntryArray,TJclWideStrWideStrBucket,IJclWideStrWideStrMap,IJclWideStrSet,IJclWideStrIterator,IJclWideStrCollection,,,,const Key: WideString,WideString,'',const Value: WideString,WideString,'')}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclSingleIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleIntfHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclSingleArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclSingleIntfHashMap.FreeKey(var Key: Single): Single;
begin
  Result := Key;
  Key := 0.0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclSingleIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclSingleIntfHashMap.KeysEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclSingleIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclSingleIntfHashMap,TJclSingleIntfEntryArray,TJclSingleIntfBucket,IJclSingleIntfMap,IJclSingleSet,IJclSingleIterator,IJclIntfCollection,,,,const Key: Single,Single,0.0,const Value: IInterface,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfSingleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfSingleHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclSingleArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfSingleHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfSingleHashMap.FreeValue(var Value: Single): Single;
begin
  Result := Value;
  Value := 0.0;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH
function TJclIntfSingleHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;
}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclIntfSingleHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclIntfSingleHashMap.ValuesEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfSingleHashMap,TJclIntfSingleEntryArray,TJclIntfSingleBucket,IJclIntfSingleMap,IJclIntfSet,IJclIntfIterator,IJclSingleCollection,,,,const Key: IInterface,IInterface,nil,const Value: Single,Single,0.0)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclSingleSingleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleSingleHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclSingleArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclSingleArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclSingleSingleHashMap.FreeKey(var Key: Single): Single;
begin
  Result := Key;
  Key := 0.0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclSingleSingleHashMap.FreeValue(var Value: Single): Single;
begin
  Result := Value;
  Value := 0.0;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclSingleSingleHashMap.KeysEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclSingleSingleHashMap.ValuesEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclSingleSingleHashMap,TJclSingleSingleEntryArray,TJclSingleSingleBucket,IJclSingleSingleMap,IJclSingleSet,IJclSingleIterator,IJclSingleCollection,,,,const Key: Single,Single,0.0,const Value: Single,Single,0.0)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclDoubleIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleIntfHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclDoubleArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclDoubleIntfHashMap.FreeKey(var Key: Double): Double;
begin
  Result := Key;
  Key := 0.0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclDoubleIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclDoubleIntfHashMap.KeysEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclDoubleIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclDoubleIntfHashMap,TJclDoubleIntfEntryArray,TJclDoubleIntfBucket,IJclDoubleIntfMap,IJclDoubleSet,IJclDoubleIterator,IJclIntfCollection,,,,const Key: Double,Double,0.0,const Value: IInterface,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfDoubleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfDoubleHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclDoubleArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfDoubleHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfDoubleHashMap.FreeValue(var Value: Double): Double;
begin
  Result := Value;
  Value := 0.0;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH
function TJclIntfDoubleHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;
}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclIntfDoubleHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclIntfDoubleHashMap.ValuesEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfDoubleHashMap,TJclIntfDoubleEntryArray,TJclIntfDoubleBucket,IJclIntfDoubleMap,IJclIntfSet,IJclIntfIterator,IJclDoubleCollection,,,,const Key: IInterface,IInterface,nil,const Value: Double,Double,0.0)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclDoubleDoubleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleDoubleHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclDoubleArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclDoubleArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclDoubleDoubleHashMap.FreeKey(var Key: Double): Double;
begin
  Result := Key;
  Key := 0.0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclDoubleDoubleHashMap.FreeValue(var Value: Double): Double;
begin
  Result := Value;
  Value := 0.0;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclDoubleDoubleHashMap.KeysEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclDoubleDoubleHashMap.ValuesEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclDoubleDoubleHashMap,TJclDoubleDoubleEntryArray,TJclDoubleDoubleBucket,IJclDoubleDoubleMap,IJclDoubleSet,IJclDoubleIterator,IJclDoubleCollection,,,,const Key: Double,Double,0.0,const Value: Double,Double,0.0)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclExtendedIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedIntfHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclExtendedArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclExtendedIntfHashMap.FreeKey(var Key: Extended): Extended;
begin
  Result := Key;
  Key := 0.0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclExtendedIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclExtendedIntfHashMap.KeysEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclExtendedIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclExtendedIntfHashMap,TJclExtendedIntfEntryArray,TJclExtendedIntfBucket,IJclExtendedIntfMap,IJclExtendedSet,IJclExtendedIterator,IJclIntfCollection,,,,const Key: Extended,Extended,0.0,const Value: IInterface,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfExtendedHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfExtendedHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclExtendedArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfExtendedHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfExtendedHashMap.FreeValue(var Value: Extended): Extended;
begin
  Result := Value;
  Value := 0.0;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH
function TJclIntfExtendedHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;
}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclIntfExtendedHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclIntfExtendedHashMap.ValuesEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfExtendedHashMap,TJclIntfExtendedEntryArray,TJclIntfExtendedBucket,IJclIntfExtendedMap,IJclIntfSet,IJclIntfIterator,IJclExtendedCollection,,,,const Key: IInterface,IInterface,nil,const Value: Extended,Extended,0.0)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclExtendedExtendedHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedExtendedHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclExtendedArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclExtendedArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclExtendedExtendedHashMap.FreeKey(var Key: Extended): Extended;
begin
  Result := Key;
  Key := 0.0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclExtendedExtendedHashMap.FreeValue(var Value: Extended): Extended;
begin
  Result := Value;
  Value := 0.0;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclExtendedExtendedHashMap.KeysEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclExtendedExtendedHashMap.ValuesEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclExtendedExtendedHashMap,TJclExtendedExtendedEntryArray,TJclExtendedExtendedBucket,IJclExtendedExtendedMap,IJclExtendedSet,IJclExtendedIterator,IJclExtendedCollection,,,,const Key: Extended,Extended,0.0,const Value: Extended,Extended,0.0)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntegerIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerIntfHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntegerArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntegerIntfHashMap.FreeKey(var Key: Integer): Integer;
begin
  Result := Key;
  Key := 0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntegerIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclIntegerIntfHashMap.KeysEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclIntegerIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntegerIntfHashMap,TJclIntegerIntfEntryArray,TJclIntegerIntfBucket,IJclIntegerIntfMap,IJclIntegerSet,IJclIntegerIterator,IJclIntfCollection,,,,Key: Integer,Integer,0,const Value: IInterface,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfIntegerHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfIntegerHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntegerArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfIntegerHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfIntegerHashMap.FreeValue(var Value: Integer): Integer;
begin
  Result := Value;
  Value := 0;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH
function TJclIntfIntegerHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;
}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclIntfIntegerHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclIntfIntegerHashMap.ValuesEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfIntegerHashMap,TJclIntfIntegerEntryArray,TJclIntfIntegerBucket,IJclIntfIntegerMap,IJclIntfSet,IJclIntfIterator,IJclIntegerCollection,,,,const Key: IInterface,IInterface,nil,Value: Integer,Integer,0)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntegerIntegerHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerIntegerHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntegerArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntegerArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntegerIntegerHashMap.FreeKey(var Key: Integer): Integer;
begin
  Result := Key;
  Key := 0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntegerIntegerHashMap.FreeValue(var Value: Integer): Integer;
begin
  Result := Value;
  Value := 0;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclIntegerIntegerHashMap.KeysEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclIntegerIntegerHashMap.ValuesEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntegerIntegerHashMap,TJclIntegerIntegerEntryArray,TJclIntegerIntegerBucket,IJclIntegerIntegerMap,IJclIntegerSet,IJclIntegerIterator,IJclIntegerCollection,,,,Key: Integer,Integer,0,Value: Integer,Integer,0)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclCardinalIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalIntfHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclCardinalArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclCardinalIntfHashMap.FreeKey(var Key: Cardinal): Cardinal;
begin
  Result := Key;
  Key := 0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclCardinalIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclCardinalIntfHashMap.KeysEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclCardinalIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclCardinalIntfHashMap,TJclCardinalIntfEntryArray,TJclCardinalIntfBucket,IJclCardinalIntfMap,IJclCardinalSet,IJclCardinalIterator,IJclIntfCollection,,,,Key: Cardinal,Cardinal,0,const Value: IInterface,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfCardinalHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfCardinalHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclCardinalArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfCardinalHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfCardinalHashMap.FreeValue(var Value: Cardinal): Cardinal;
begin
  Result := Value;
  Value := 0;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH
function TJclIntfCardinalHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;
}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclIntfCardinalHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclIntfCardinalHashMap.ValuesEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfCardinalHashMap,TJclIntfCardinalEntryArray,TJclIntfCardinalBucket,IJclIntfCardinalMap,IJclIntfSet,IJclIntfIterator,IJclCardinalCollection,,,,const Key: IInterface,IInterface,nil,Value: Cardinal,Cardinal,0)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclCardinalCardinalHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalCardinalHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclCardinalArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclCardinalArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclCardinalCardinalHashMap.FreeKey(var Key: Cardinal): Cardinal;
begin
  Result := Key;
  Key := 0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclCardinalCardinalHashMap.FreeValue(var Value: Cardinal): Cardinal;
begin
  Result := Value;
  Value := 0;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclCardinalCardinalHashMap.KeysEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclCardinalCardinalHashMap.ValuesEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclCardinalCardinalHashMap,TJclCardinalCardinalEntryArray,TJclCardinalCardinalBucket,IJclCardinalCardinalMap,IJclCardinalSet,IJclCardinalIterator,IJclCardinalCollection,,,,Key: Cardinal,Cardinal,0,Value: Cardinal,Cardinal,0)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclInt64IntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64IntfHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclInt64ArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclInt64IntfHashMap.FreeKey(var Key: Int64): Int64;
begin
  Result := Key;
  Key := 0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclInt64IntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclInt64IntfHashMap.KeysEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclInt64IntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclInt64IntfHashMap,TJclInt64IntfEntryArray,TJclInt64IntfBucket,IJclInt64IntfMap,IJclInt64Set,IJclInt64Iterator,IJclIntfCollection,,,,const Key: Int64,Int64,0,const Value: IInterface,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfInt64HashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfInt64HashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclInt64ArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfInt64HashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfInt64HashMap.FreeValue(var Value: Int64): Int64;
begin
  Result := Value;
  Value := 0;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH
function TJclIntfInt64HashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;
}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclIntfInt64HashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclIntfInt64HashMap.ValuesEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfInt64HashMap,TJclIntfInt64EntryArray,TJclIntfInt64Bucket,IJclIntfInt64Map,IJclIntfSet,IJclIntfIterator,IJclInt64Collection,,,,const Key: IInterface,IInterface,nil,const Value: Int64,Int64,0)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclInt64Int64HashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64Int64HashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclInt64ArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclInt64ArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclInt64Int64HashMap.FreeKey(var Key: Int64): Int64;
begin
  Result := Key;
  Key := 0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclInt64Int64HashMap.FreeValue(var Value: Int64): Int64;
begin
  Result := Value;
  Value := 0;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclInt64Int64HashMap.KeysEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclInt64Int64HashMap.ValuesEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclInt64Int64HashMap,TJclInt64Int64EntryArray,TJclInt64Int64Bucket,IJclInt64Int64Map,IJclInt64Set,IJclInt64Iterator,IJclInt64Collection,,,,const Key: Int64,Int64,0,const Value: Int64,Int64,0)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$IFNDEF CLR}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclPtrIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrIntfHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclPtrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclPtrIntfHashMap.FreeKey(var Key: Pointer): Pointer;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclPtrIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclPtrIntfHashMap.KeysEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclPtrIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclPtrIntfHashMap,TJclPtrIntfEntryArray,TJclPtrIntfBucket,IJclPtrIntfMap,IJclPtrSet,IJclPtrIterator,IJclIntfCollection,,,,Key: Pointer,Pointer,nil,const Value: IInterface,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfPtrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfPtrHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclPtrArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfPtrHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfPtrHashMap.FreeValue(var Value: Pointer): Pointer;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH
function TJclIntfPtrHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;
}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclIntfPtrHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclIntfPtrHashMap.ValuesEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfPtrHashMap,TJclIntfPtrEntryArray,TJclIntfPtrBucket,IJclIntfPtrMap,IJclIntfSet,IJclIntfIterator,IJclPtrCollection,,,,const Key: IInterface,IInterface,nil,Value: Pointer,Pointer,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclPtrPtrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrPtrHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclPtrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclPtrArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclPtrPtrHashMap.FreeKey(var Key: Pointer): Pointer;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclPtrPtrHashMap.FreeValue(var Value: Pointer): Pointer;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclPtrPtrHashMap.KeysEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclPtrPtrHashMap.ValuesEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclPtrPtrHashMap,TJclPtrPtrEntryArray,TJclPtrPtrBucket,IJclPtrPtrMap,IJclPtrSet,IJclPtrIterator,IJclPtrCollection,,,,Key: Pointer,Pointer,nil,Value: Pointer,Pointer,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}
{$ENDIF ~CLR}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfHashMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfHashMap.FreeValue(var Value: TObject): TObject;
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
function TJclIntfHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO HASH
function TJclIntfHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;
}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclIntfHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclIntfHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfHashMap,TJclIntfEntryArray,TJclIntfBucket,IJclIntfMap,IJclIntfSet,IJclIntfIterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,const Key: IInterface,IInterface,nil,Value: TObject,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclAnsiStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrHashMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclAnsiStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclAnsiStrHashMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclAnsiStrHashMap.FreeValue(var Value: TObject): TObject;
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
function TJclAnsiStrHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclAnsiStrHashMap.KeysEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclAnsiStrHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclAnsiStrHashMap,TJclAnsiStrEntryArray,TJclAnsiStrBucket,IJclAnsiStrMap,IJclAnsiStrSet,IJclAnsiStrIterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,const Key: AnsiString,AnsiString,'',Value: TObject,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclWideStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrHashMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclWideStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclWideStrHashMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclWideStrHashMap.FreeValue(var Value: TObject): TObject;
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
function TJclWideStrHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclWideStrHashMap.KeysEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclWideStrHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclWideStrHashMap,TJclWideStrEntryArray,TJclWideStrBucket,IJclWideStrMap,IJclWideStrSet,IJclWideStrIterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,const Key: WideString,WideString,'',Value: TObject,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclSingleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleHashMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclSingleArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclSingleHashMap.FreeKey(var Key: Single): Single;
begin
  Result := Key;
  Key := 0.0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclSingleHashMap.FreeValue(var Value: TObject): TObject;
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
function TJclSingleHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclSingleHashMap.KeysEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclSingleHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclSingleHashMap,TJclSingleEntryArray,TJclSingleBucket,IJclSingleMap,IJclSingleSet,IJclSingleIterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,const Key: Single,Single,0.0,Value: TObject,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclDoubleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleHashMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclDoubleArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclDoubleHashMap.FreeKey(var Key: Double): Double;
begin
  Result := Key;
  Key := 0.0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclDoubleHashMap.FreeValue(var Value: TObject): TObject;
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
function TJclDoubleHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclDoubleHashMap.KeysEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclDoubleHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclDoubleHashMap,TJclDoubleEntryArray,TJclDoubleBucket,IJclDoubleMap,IJclDoubleSet,IJclDoubleIterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,const Key: Double,Double,0.0,Value: TObject,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclExtendedHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedHashMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclExtendedArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclExtendedHashMap.FreeKey(var Key: Extended): Extended;
begin
  Result := Key;
  Key := 0.0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclExtendedHashMap.FreeValue(var Value: TObject): TObject;
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
function TJclExtendedHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclExtendedHashMap.KeysEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclExtendedHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclExtendedHashMap,TJclExtendedEntryArray,TJclExtendedBucket,IJclExtendedMap,IJclExtendedSet,IJclExtendedIterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,const Key: Extended,Extended,0.0,Value: TObject,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntegerHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerHashMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntegerArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntegerHashMap.FreeKey(var Key: Integer): Integer;
begin
  Result := Key;
  Key := 0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntegerHashMap.FreeValue(var Value: TObject): TObject;
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
function TJclIntegerHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclIntegerHashMap.KeysEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclIntegerHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntegerHashMap,TJclIntegerEntryArray,TJclIntegerBucket,IJclIntegerMap,IJclIntegerSet,IJclIntegerIterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,Key: Integer,Integer,0,Value: TObject,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclCardinalHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalHashMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclCardinalArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclCardinalHashMap.FreeKey(var Key: Cardinal): Cardinal;
begin
  Result := Key;
  Key := 0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclCardinalHashMap.FreeValue(var Value: TObject): TObject;
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
function TJclCardinalHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclCardinalHashMap.KeysEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclCardinalHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclCardinalHashMap,TJclCardinalEntryArray,TJclCardinalBucket,IJclCardinalMap,IJclCardinalSet,IJclCardinalIterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,Key: Cardinal,Cardinal,0,Value: TObject,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclInt64HashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64HashMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclInt64ArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclInt64HashMap.FreeKey(var Key: Int64): Int64;
begin
  Result := Key;
  Key := 0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclInt64HashMap.FreeValue(var Value: TObject): TObject;
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
function TJclInt64HashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclInt64HashMap.KeysEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclInt64HashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclInt64HashMap,TJclInt64EntryArray,TJclInt64Bucket,IJclInt64Map,IJclInt64Set,IJclInt64Iterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,const Key: Int64,Int64,0,Value: TObject,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$IFNDEF CLR}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclPtrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrHashMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclPtrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclPtrHashMap.FreeKey(var Key: Pointer): Pointer;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclPtrHashMap.FreeValue(var Value: TObject): TObject;
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
function TJclPtrHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclPtrHashMap.KeysEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclPtrHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclPtrHashMap,TJclPtrEntryArray,TJclPtrBucket,IJclPtrMap,IJclPtrSet,IJclPtrIterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,Key: Pointer,Pointer,nil,Value: TObject,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}
{$ENDIF ~CLR}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashMap.Create(FSize, False, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclArraySet.Create(Param, False)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclHashMap.FreeKey(var Key: TObject): TObject;
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
function TJclHashMap.FreeValue(var Value: TObject): TObject;
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
function TJclHashMap.GetOwnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;
}
{$JPPDEFINEMACRO GETOWNSVALUES
function TJclHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO HASH
function TJclHashMap.Hash(AObject: TObject): Integer;
begin
  Result := Integer(AObject);
end;
}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclHashMap.KeysEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclHashMap,TJclEntryArray,TJclBucket,IJclMap,IJclSet,IJclIterator,IJclCollection,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;,Key: TObject,TObject,nil,Value: TObject,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$IFDEF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)CreateEmptyArraySet(Param, False)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)CreateEmptyArrayList(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclHashMap<TKey, TValue>.FreeKey(var Key: TKey): TKey;
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
function TJclHashMap<TKey, TValue>.FreeValue(var Value: TValue): TValue;
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
function TJclHashMap<TKey, TValue>.GetOwnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;
}
{$JPPDEFINEMACRO GETOWNSVALUES
function TJclHashMap<TKey, TValue>.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL}
{$JPPDEFINEMACRO VALUESEQUAL}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclHashMap<TKey\, TValue>,TJclEntryArray<TKey\, TValue>,TJclBucket<TKey\, TValue>,IJclMap<TKey\, TValue>,IJclSet<TKey>,IJclIterator<TKey>,IJclCollection<TValue>,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;,const Key: TKey,TKey,Default(TKey),const Value: TValue,TValue,Default(TValue))}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

//=== { TJclHashMapE<TKey, TValue> } =========================================

constructor TJclHashMapE<TKey, TValue>.Create(const AKeyEqualityComparer: IEqualityComparer<TKey>;
  const AValueEqualityComparer: IEqualityComparer<TValue>; const AKeyComparer: IComparer<TKey>; ACapacity: Integer;
  AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create(ACapacity, AOwnsKeys, AOwnsValues);
  FKeyEqualityComparer := AKeyEqualityComparer;
  FValueEqualityComparer := AValueEqualityComparer;
  FKeyComparer := AKeyComparer;
end;

procedure TJclHashMapE<TKey, TValue>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclHashMapE<TKey, TValue>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclHashMapE<TKey, TValue> then
  begin
    ADest := TJclHashMapE<TKey, TValue>(Dest);
    ADest.FKeyEqualityComparer := FKeyEqualityComparer;
    ADest.FValueEqualityComparer := FValueEqualityComparer;
    ADest.FKeyComparer := FKeyComparer;
  end;
end;

function TJclHashMapE<TKey, TValue>.CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>;
begin
  Result := TJclArrayListE<TValue>.Create(ValueEqualityComparer, ACapacity, AOwnsObjects);
end;

function TJclHashMapE<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>;
begin
  Result := TJclArraySetE<TKey>.Create(KeyComparer, ACapacity, AOwnsObjects);
end;

function TJclHashMapE<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashMapE<TKey, TValue>.Create(KeyEqualityComparer, ValueEqualityComparer,
    KeyComparer, FSize, False, False);
  AssignPropertiesTo(Result);
end;

function TJclHashMapE<TKey, TValue>.Hash(const AKey: TKey): Integer;
begin
  if KeyEqualityComparer = nil then
    raise EJclNoHashConverterError.Create;
  Result := KeyEqualityComparer.GetHashCode(AKey);
end;

function TJclHashMapE<TKey, TValue>.KeysEqual(const A, B: TKey): Boolean;
begin
  if KeyEqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := KeyEqualityComparer.Equals(A, B);
end;

function TJclHashMapE<TKey, TValue>.ValuesEqual(const A, B: TValue): Boolean;
begin
  if ValueEqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := ValueEqualityComparer.Equals(A, B);
end;

//=== { TJclHashMapF<TKey, TValue> } =========================================

constructor TJclHashMapF<TKey, TValue>.Create(AKeyEqualityCompare: TEqualityCompare<TKey>;
  AKeyHash: THash<TKey>; AValueEqualityCompare: TEqualityCompare<TValue>; AKeyCompare: TCompare<TKey>;
  ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create(ACapacity, AOwnsKeys, AOwnsValues);
  FKeyEqualityCompare := AKeyEqualityCompare;
  FKeyHash := AKeyHash;
  FValueEqualityCompare := AValueEqualityCompare;
  FKeyCompare := AKeyCompare;
end;

procedure TJclHashMapF<TKey, TValue>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclHashMapF<TKey, TValue>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclHashMapF<TKey, TValue> then
  begin
    ADest := TJclHashMapF<TKey, TValue>(Dest);
    ADest.FKeyEqualityCompare := FKeyEqualityCompare;
    ADest.FKeyHash := FKeyHash;
    ADest.FValueEqualityCompare := FValueEqualityCompare;
    ADest.FKeyCompare := FKeyCompare;
  end;
end;

function TJclHashMapF<TKey, TValue>.CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>;
begin
  Result := TJclArrayListF<TValue>.Create(ValueEqualityCompare, ACapacity, AOwnsObjects);
end;

function TJclHashMapF<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>;
begin
  Result := TJclArraySetF<TKey>.Create(KeyCompare, ACapacity, AOwnsObjects);
end;

function TJclHashMapF<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashMapF<TKey, TValue>.Create(KeyEqualityCompare, KeyHash, ValueEqualityCompare, KeyCompare, FSize,
    False, False);
  AssignPropertiesTo(Result);
end;

function TJclHashMapF<TKey, TValue>.Hash(const AKey: TKey): Integer;
begin
  if not Assigned(KeyHash) then
    raise EJclNoHashConverterError.Create;
  Result := KeyHash(AKey);
end;

function TJclHashMapF<TKey, TValue>.KeysEqual(const A, B: TKey): Boolean;
begin
  if not Assigned(KeyEqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := KeyEqualityCompare(A, B);
end;

function TJclHashMapF<TKey, TValue>.ValuesEqual(const A, B: TValue): Boolean;
begin
  if not Assigned(ValueEqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := ValueEqualityCompare(A, B);
end;

//=== { TJclHashMapI<TKey, TValue> } =========================================

function TJclHashMapI<TKey, TValue>.CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>;
begin
  Result := TJclArrayListI<TValue>.Create(ACapacity, AOwnsObjects);
end;

function TJclHashMapI<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>;
begin
  Result := TJclArraySetI<TKey>.Create(ACapacity, AOwnsObjects);
end;

function TJclHashMapI<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashMapI<TKey, TValue>.Create(FSize, False, False);
  AssignPropertiesTo(Result);
end;

function TJclHashMapI<TKey, TValue>.Hash(const AKey: TKey): Integer;
begin
  Result := AKey.GetHashCode;
end;

function TJclHashMapI<TKey, TValue>.KeysEqual(const A, B: TKey): Boolean;
begin
  Result := A.Equals(B);
end;

function TJclHashMapI<TKey, TValue>.ValuesEqual(const A, B: TValue): Boolean;
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
