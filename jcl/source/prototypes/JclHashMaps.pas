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
  JclAlgorithms,
  {$ENDIF SUPPORTS_GENERICS}
  JclBase, JclSynch,
  JclContainerIntf, JclAbstractContainers, JclArrayLists, JclArraySets;
{$I containers\JclContainerCommon.imp}
{$I containers\JclHashMaps.imp}
{$I containers\JclHashMaps.int}
type
  // Hash Function
  // Result must be in 0..Range-1
  TJclHashFunction = function(Key, Range: Integer): Integer;

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclIntfIntfHashEntry,TJclIntfIntfBucket,IInterface,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclIntfIntfBucket,TJclIntfIntfHashMap,TJclIntfAbstractContainer,IJclIntfIntfMap,IJclIntfSet,IJclIntfCollection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A\, B: IInterface): Boolean;
    function ValuesEqual(const A\, B: IInterface): Boolean;,,,const ,IInterface,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclAnsiStrIntfHashEntry,TJclAnsiStrIntfBucket,AnsiString,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclAnsiStrIntfBucket,TJclAnsiStrIntfHashMap,TJclAnsiStrAbstractContainer,IJclAnsiStrIntfMap,IJclAnsiStrSet,IJclIntfCollection, IJclStrContainer\, IJclAnsiStrContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A\, B: AnsiString): Boolean;
    function ValuesEqual(const A\, B: IInterface): Boolean;,,,const ,AnsiString,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclIntfAnsiStrHashEntry,TJclIntfAnsiStrBucket,IInterface,AnsiString)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclIntfAnsiStrBucket,TJclIntfAnsiStrHashMap,TJclAnsiStrAbstractContainer,IJclIntfAnsiStrMap,IJclIntfSet,IJclAnsiStrCollection, IJclStrContainer\, IJclAnsiStrContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: AnsiString): AnsiString;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A\, B: IInterface): Boolean;
    function ValuesEqual(const A\, B: AnsiString): Boolean;,,,const ,IInterface,const ,AnsiString)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclAnsiStrAnsiStrHashEntry,TJclAnsiStrAnsiStrBucket,AnsiString,AnsiString)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclAnsiStrAnsiStrBucket,TJclAnsiStrAnsiStrHashMap,TJclAnsiStrAbstractContainer,IJclAnsiStrAnsiStrMap,IJclAnsiStrSet,IJclAnsiStrCollection, IJclStrContainer\, IJclAnsiStrContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function FreeValue(var Value: AnsiString): AnsiString;
    function KeysEqual(const A\, B: AnsiString): Boolean;
    function ValuesEqual(const A\, B: AnsiString): Boolean;,,,const ,AnsiString,const ,AnsiString)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclWideStrIntfHashEntry,TJclWideStrIntfBucket,WideString,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclWideStrIntfBucket,TJclWideStrIntfHashMap,TJclWideStrAbstractContainer,IJclWideStrIntfMap,IJclWideStrSet,IJclIntfCollection, IJclStrContainer\, IJclWideStrContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A\, B: WideString): Boolean;
    function ValuesEqual(const A\, B: IInterface): Boolean;,,,const ,WideString,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclIntfWideStrHashEntry,TJclIntfWideStrBucket,IInterface,WideString)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclIntfWideStrBucket,TJclIntfWideStrHashMap,TJclWideStrAbstractContainer,IJclIntfWideStrMap,IJclIntfSet,IJclWideStrCollection, IJclStrContainer\, IJclWideStrContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: WideString): WideString;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A\, B: IInterface): Boolean;
    function ValuesEqual(const A\, B: WideString): Boolean;,,,const ,IInterface,const ,WideString)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclWideStrWideStrHashEntry,TJclWideStrWideStrBucket,WideString,WideString)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclWideStrWideStrBucket,TJclWideStrWideStrHashMap,TJclWideStrAbstractContainer,IJclWideStrWideStrMap,IJclWideStrSet,IJclWideStrCollection, IJclStrContainer\, IJclWideStrContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function FreeValue(var Value: WideString): WideString;
    function KeysEqual(const A\, B: WideString): Boolean;
    function ValuesEqual(const A\, B: WideString): Boolean;,,,const ,WideString,const ,WideString)*)

{$IFDEF SUPPORTS_UNICODE_STRING}
(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclUnicodeStrIntfHashEntry,TJclUnicodeStrIntfBucket,UnicodeString,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclUnicodeStrIntfBucket,TJclUnicodeStrIntfHashMap,TJclUnicodeStrAbstractContainer,IJclUnicodeStrIntfMap,IJclUnicodeStrSet,IJclIntfCollection, IJclStrContainer\, IJclUnicodeStrContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: UnicodeString): UnicodeString;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A\, B: UnicodeString): Boolean;
    function ValuesEqual(const A\, B: IInterface): Boolean;,,,const ,UnicodeString,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclIntfUnicodeStrHashEntry,TJclIntfUnicodeStrBucket,IInterface,UnicodeString)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclIntfUnicodeStrBucket,TJclIntfUnicodeStrHashMap,TJclUnicodeStrAbstractContainer,IJclIntfUnicodeStrMap,IJclIntfSet,IJclUnicodeStrCollection, IJclStrContainer\, IJclUnicodeStrContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: UnicodeString): UnicodeString;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A\, B: IInterface): Boolean;
    function ValuesEqual(const A\, B: UnicodeString): Boolean;,,,const ,IInterface,const ,UnicodeString)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclUnicodeStrUnicodeStrHashEntry,TJclUnicodeStrUnicodeStrBucket,UnicodeString,UnicodeString)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclUnicodeStrUnicodeStrBucket,TJclUnicodeStrUnicodeStrHashMap,TJclUnicodeStrAbstractContainer,IJclUnicodeStrUnicodeStrMap,IJclUnicodeStrSet,IJclUnicodeStrCollection, IJclStrContainer\, IJclUnicodeStrContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: UnicodeString): UnicodeString;
    function FreeValue(var Value: UnicodeString): UnicodeString;
    function KeysEqual(const A\, B: UnicodeString): Boolean;
    function ValuesEqual(const A\, B: UnicodeString): Boolean;,,,const ,UnicodeString,const ,UnicodeString)*)
{$ENDIF SUPPORTS_UNICODE_STRING}

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
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrIntfHashMap = TJclUnicodeStrIntfHashMap;
  TJclIntfStrHashMap = TJclIntfUnicodeStrHashMap;
  TJclStrStrHashMap = TJclUnicodeStrUnicodeStrHashMap;
  {$ENDIF CONTAINER_UNICODESTR}

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclSingleIntfHashEntry,TJclSingleIntfBucket,Single,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclSingleIntfBucket,TJclSingleIntfHashMap,TJclSingleAbstractContainer,IJclSingleIntfMap,IJclSingleSet,IJclIntfCollection, IJclSingleContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Single): Single;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A\, B: Single): Boolean;
    function ValuesEqual(const A\, B: IInterface): Boolean;,,,const ,Single,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclIntfSingleHashEntry,TJclIntfSingleBucket,IInterface,Single)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclIntfSingleBucket,TJclIntfSingleHashMap,TJclSingleAbstractContainer,IJclIntfSingleMap,IJclIntfSet,IJclSingleCollection, IJclSingleContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Single): Single;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A\, B: IInterface): Boolean;
    function ValuesEqual(const A\, B: Single): Boolean;,,,const ,IInterface,const ,Single)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclSingleSingleHashEntry,TJclSingleSingleBucket,Single,Single)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclSingleSingleBucket,TJclSingleSingleHashMap,TJclSingleAbstractContainer,IJclSingleSingleMap,IJclSingleSet,IJclSingleCollection, IJclSingleContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Single): Single;
    function FreeValue(var Value: Single): Single;
    function KeysEqual(const A\, B: Single): Boolean;
    function ValuesEqual(const A\, B: Single): Boolean;,,,const ,Single,const ,Single)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclDoubleIntfHashEntry,TJclDoubleIntfBucket,Double,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclDoubleIntfBucket,TJclDoubleIntfHashMap,TJclDoubleAbstractContainer,IJclDoubleIntfMap,IJclDoubleSet,IJclIntfCollection, IJclDoubleContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Double): Double;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A\, B: Double): Boolean;
    function ValuesEqual(const A\, B: IInterface): Boolean;,,,const ,Double,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclIntfDoubleHashEntry,TJclIntfDoubleBucket,IInterface,Double)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclIntfDoubleBucket,TJclIntfDoubleHashMap,TJclDoubleAbstractContainer,IJclIntfDoubleMap,IJclIntfSet,IJclDoubleCollection, IJclDoubleContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Double): Double;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A\, B: IInterface): Boolean;
    function ValuesEqual(const A\, B: Double): Boolean;,,,const ,IInterface,const ,Double)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclDoubleDoubleHashEntry,TJclDoubleDoubleBucket,Double,Double)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclDoubleDoubleBucket,TJclDoubleDoubleHashMap,TJclDoubleAbstractContainer,IJclDoubleDoubleMap,IJclDoubleSet,IJclDoubleCollection, IJclDoubleContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Double): Double;
    function FreeValue(var Value: Double): Double;
    function KeysEqual(const A\, B: Double): Boolean;
    function ValuesEqual(const A\, B: Double): Boolean;,,,const ,Double,const ,Double)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclExtendedIntfHashEntry,TJclExtendedIntfBucket,Extended,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclExtendedIntfBucket,TJclExtendedIntfHashMap,TJclExtendedAbstractContainer,IJclExtendedIntfMap,IJclExtendedSet,IJclIntfCollection, IJclExtendedContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Extended): Extended;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A\, B: Extended): Boolean;
    function ValuesEqual(const A\, B: IInterface): Boolean;,,,const ,Extended,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclIntfExtendedHashEntry,TJclIntfExtendedBucket,IInterface,Extended)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclIntfExtendedBucket,TJclIntfExtendedHashMap,TJclExtendedAbstractContainer,IJclIntfExtendedMap,IJclIntfSet,IJclExtendedCollection, IJclExtendedContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Extended): Extended;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A\, B: IInterface): Boolean;
    function ValuesEqual(const A\, B: Extended): Boolean;,,,const ,IInterface,const ,Extended)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclExtendedExtendedHashEntry,TJclExtendedExtendedBucket,Extended,Extended)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclExtendedExtendedBucket,TJclExtendedExtendedHashMap,TJclExtendedAbstractContainer,IJclExtendedExtendedMap,IJclExtendedSet,IJclExtendedCollection, IJclExtendedContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Extended): Extended;
    function FreeValue(var Value: Extended): Extended;
    function KeysEqual(const A\, B: Extended): Boolean;
    function ValuesEqual(const A\, B: Extended): Boolean;,,,const ,Extended,const ,Extended)*)

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

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclIntegerIntfHashEntry,TJclIntegerIntfBucket,Integer,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclIntegerIntfBucket,TJclIntegerIntfHashMap,TJclIntegerAbstractContainer,IJclIntegerIntfMap,IJclIntegerSet,IJclIntfCollection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Integer): Integer;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(A\, B: Integer): Boolean;
    function ValuesEqual(const A\, B: IInterface): Boolean;,,,,Integer,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclIntfIntegerHashEntry,TJclIntfIntegerBucket,IInterface,Integer)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclIntfIntegerBucket,TJclIntfIntegerHashMap,TJclIntegerAbstractContainer,IJclIntfIntegerMap,IJclIntfSet,IJclIntegerCollection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Integer): Integer;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A\, B: IInterface): Boolean;
    function ValuesEqual(A\, B: Integer): Boolean;,,,const ,IInterface,,Integer)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclIntegerIntegerHashEntry,TJclIntegerIntegerBucket,Integer,Integer)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclIntegerIntegerBucket,TJclIntegerIntegerHashMap,TJclIntegerAbstractContainer,IJclIntegerIntegerMap,IJclIntegerSet,IJclIntegerCollection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Integer): Integer;
    function FreeValue(var Value: Integer): Integer;
    function KeysEqual(A\, B: Integer): Boolean;
    function ValuesEqual(A\, B: Integer): Boolean;,,,,Integer,,Integer)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclCardinalIntfHashEntry,TJclCardinalIntfBucket,Cardinal,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclCardinalIntfBucket,TJclCardinalIntfHashMap,TJclCardinalAbstractContainer,IJclCardinalIntfMap,IJclCardinalSet,IJclIntfCollection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Cardinal): Cardinal;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(A\, B: Cardinal): Boolean;
    function ValuesEqual(const A\, B: IInterface): Boolean;,,,,Cardinal,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclIntfCardinalHashEntry,TJclIntfCardinalBucket,IInterface,Cardinal)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclIntfCardinalBucket,TJclIntfCardinalHashMap,TJclCardinalAbstractContainer,IJclIntfCardinalMap,IJclIntfSet,IJclCardinalCollection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Cardinal): Cardinal;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A\, B: IInterface): Boolean;
    function ValuesEqual(A\, B: Cardinal): Boolean;,,,const ,IInterface,,Cardinal)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclCardinalCardinalHashEntry,TJclCardinalCardinalBucket,Cardinal,Cardinal)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclCardinalCardinalBucket,TJclCardinalCardinalHashMap,TJclCardinalAbstractContainer,IJclCardinalCardinalMap,IJclCardinalSet,IJclCardinalCollection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Cardinal): Cardinal;
    function FreeValue(var Value: Cardinal): Cardinal;
    function KeysEqual(A\, B: Cardinal): Boolean;
    function ValuesEqual(A\, B: Cardinal): Boolean;,,,,Cardinal,,Cardinal)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclInt64IntfHashEntry,TJclInt64IntfBucket,Int64,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclInt64IntfBucket,TJclInt64IntfHashMap,TJclInt64AbstractContainer,IJclInt64IntfMap,IJclInt64Set,IJclIntfCollection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Int64): Int64;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A\, B: Int64): Boolean;
    function ValuesEqual(const A\, B: IInterface): Boolean;,,,const ,Int64,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclIntfInt64HashEntry,TJclIntfInt64Bucket,IInterface,Int64)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclIntfInt64Bucket,TJclIntfInt64HashMap,TJclInt64AbstractContainer,IJclIntfInt64Map,IJclIntfSet,IJclInt64Collection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Int64): Int64;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A\, B: IInterface): Boolean;
    function ValuesEqual(const A\, B: Int64): Boolean;,,,const ,IInterface,const ,Int64)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclInt64Int64HashEntry,TJclInt64Int64Bucket,Int64,Int64)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclInt64Int64Bucket,TJclInt64Int64HashMap,TJclInt64AbstractContainer,IJclInt64Int64Map,IJclInt64Set,IJclInt64Collection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Int64): Int64;
    function FreeValue(var Value: Int64): Int64;
    function KeysEqual(const A\, B: Int64): Boolean;
    function ValuesEqual(const A\, B: Int64): Boolean;,,,const ,Int64,const ,Int64)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclPtrIntfHashEntry,TJclPtrIntfBucket,Pointer,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclPtrIntfBucket,TJclPtrIntfHashMap,TJclPtrAbstractContainer,IJclPtrIntfMap,IJclPtrSet,IJclIntfCollection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Pointer): Pointer;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(A\, B: Pointer): Boolean;
    function ValuesEqual(const A\, B: IInterface): Boolean;,,,,Pointer,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclIntfPtrHashEntry,TJclIntfPtrBucket,IInterface,Pointer)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclIntfPtrBucket,TJclIntfPtrHashMap,TJclPtrAbstractContainer,IJclIntfPtrMap,IJclIntfSet,IJclPtrCollection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Pointer): Pointer;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A\, B: IInterface): Boolean;
    function ValuesEqual(A\, B: Pointer): Boolean;,,,const ,IInterface,,Pointer)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclPtrPtrHashEntry,TJclPtrPtrBucket,Pointer,Pointer)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclPtrPtrBucket,TJclPtrPtrHashMap,TJclPtrAbstractContainer,IJclPtrPtrMap,IJclPtrSet,IJclPtrCollection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Pointer): Pointer;
    function FreeValue(var Value: Pointer): Pointer;
    function KeysEqual(A\, B: Pointer): Boolean;
    function ValuesEqual(A\, B: Pointer): Boolean;,,,,Pointer,,Pointer)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclIntfHashEntry,TJclIntfBucket,IInterface,TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclIntfBucket,TJclIntfHashMap,TJclAbstractContainerBase,IJclIntfMap,IJclIntfSet,IJclCollection, IJclValueOwner\,,
  private
    FOwnsValues: Boolean;
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A\, B: IInterface): Boolean;
    function ValuesEqual(A\, B: TObject): Boolean;
  public
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const ,IInterface,,TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclAnsiStrHashEntry,TJclAnsiStrBucket,AnsiString,TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclAnsiStrBucket,TJclAnsiStrHashMap,TJclAnsiStrAbstractContainer,IJclAnsiStrMap,IJclAnsiStrSet,IJclCollection, IJclStrContainer\, IJclAnsiStrContainer\, IJclValueOwner\,,
  private
    FOwnsValues: Boolean;
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function KeysEqual(const A\, B: AnsiString): Boolean;
    function ValuesEqual(A\, B: TObject): Boolean;
  public
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const ,AnsiString,,TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclWideStrHashEntry,TJclWideStrBucket,WideString,TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclWideStrBucket,TJclWideStrHashMap,TJclwideStrAbstractContainer,IJclWideStrMap,IJclWideStrSet,IJclCollection, IJclStrContainer\, IJclWideStrContainer\, IJclValueOwner\,,
  private
    FOwnsValues: Boolean;
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function KeysEqual(const A\, B: WideString): Boolean;
    function ValuesEqual(A\, B: TObject): Boolean;
  public
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const ,WideString,,TObject)*)

{$IFDEF SUPPORTS_UNICODE_STRING}
(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclUnicodeStrHashEntry,TJclUnicodeStrBucket,UnicodeString,TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclUnicodeStrBucket,TJclUnicodeStrHashMap,TJclUnicodeStrAbstractContainer,IJclUnicodeStrMap,IJclUnicodeStrSet,IJclCollection, IJclStrContainer\, IJclUnicodeStrContainer\, IJclValueOwner\,,
  private
    FOwnsValues: Boolean;
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: UnicodeString): UnicodeString;
    function KeysEqual(const A\, B: UnicodeString): Boolean;
    function ValuesEqual(A\, B: TObject): Boolean;
  public
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const ,UnicodeString,,TObject)*)
{$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrHashMap = TJclAnsiStrHashMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrHashMap = TJclWideStrHashMap;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrHashMap = TJclUnicodeStrHashMap;
  {$ENDIF CONTAINER_UNICODESTR}

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclSingleHashEntry,TJclSingleBucket,Single,TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclSingleBucket,TJclSingleHashMap,TJclSingleAbstractContainer,IJclSingleMap,IJclSingleSet,IJclCollection, IJclSingleContainer\, IJclValueOwner\,,
  private
    FOwnsValues: Boolean;
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Single): Single;
    function KeysEqual(const A\, B: Single): Boolean;
    function ValuesEqual(A\, B: TObject): Boolean;
  public
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const ,Single,,TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclDoubleHashEntry,TJclDoubleBucket,Double,TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclDoubleBucket,TJclDoubleHashMap,TJclDoubleAbstractContainer,IJclDoubleMap,IJclDoubleSet,IJclCollection, IJclDoubleContainer\, IJclValueOwner\,,
  private
    FOwnsValues: Boolean;
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Double): Double;
    function KeysEqual(const A\, B: Double): Boolean;
    function ValuesEqual(A\, B: TObject): Boolean;
  public
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const ,Double,,TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclExtendedHashEntry,TJclExtendedBucket,Extended,TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclExtendedBucket,TJclExtendedHashMap,TJclExtendedAbstractContainer,IJclExtendedMap,IJclExtendedSet,IJclCollection, IJclExtendedContainer\, IJclValueOwner\,,
  private
    FOwnsValues: Boolean;
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Extended): Extended;
    function KeysEqual(const A\, B: Extended): Boolean;
    function ValuesEqual(A\, B: TObject): Boolean;
  public
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const ,Extended,,TObject)*)

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatHashMap = TJclExtendedHashMap;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatHashMap = TJclDoubleHashMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatHashMap = TJclSingleHashMap;
  {$ENDIF MATH_SINGLE_PRECISION}

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclIntegerHashEntry,TJclIntegerBucket,Integer,TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclIntegerBucket,TJclIntegerHashMap,TJclIntegerAbstractContainer,IJclIntegerMap,IJclIntegerSet,IJclCollection, IJclValueOwner\,,
  private
    FOwnsValues: Boolean;
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Integer): Integer;
    function KeysEqual(A\, B: Integer): Boolean;
    function ValuesEqual(A\, B: TObject): Boolean;
  public
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,,Integer,,TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclCardinalHashEntry,TJclCardinalBucket,Cardinal,TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclCardinalBucket,TJclCardinalHashMap,TJclCardinalAbstractContainer,IJclCardinalMap,IJclCardinalSet,IJclCollection, IJclValueOwner\,,
  private
    FOwnsValues: Boolean;
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Cardinal): Cardinal;
    function KeysEqual(A\, B: Cardinal): Boolean;
    function ValuesEqual(A\, B: TObject): Boolean;
  public
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,,Cardinal,,TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclInt64HashEntry,TJclInt64Bucket,Int64,TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclInt64Bucket,TJclInt64HashMap,TJclInt64AbstractContainer,IJclInt64Map,IJclInt64Set,IJclCollection, IJclValueOwner\,,
  private
    FOwnsValues: Boolean;
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Int64): Int64;
    function KeysEqual(const A\, B: Int64): Boolean;
    function ValuesEqual(A\, B: TObject): Boolean;
  public
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const ,Int64,,TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclPtrHashEntry,TJclPtrBucket,Pointer,TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclPtrBucket,TJclPtrHashMap,TJclPtrAbstractContainer,IJclPtrMap,IJclPtrSet,IJclCollection, IJclValueOwner\,,
  private
    FOwnsValues: Boolean;
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Pointer): Pointer;
    function KeysEqual(A\, B: Pointer): Boolean;
    function ValuesEqual(A\, B: TObject): Boolean;
  public
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,,Pointer,,TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclHashEntry,TJclBucket,TObject,TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TJclBucket,TJclHashMap,TJclAbstractContainerBase,IJclMap,IJclSet,IJclCollection, IJclKeyOwner\, IJclValueOwner\,,
  private
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function Hash(AObject: TObject): Integer;
    function KeysEqual(A\, B: TObject): Boolean;
    function ValuesEqual(A\, B: TObject): Boolean;
  public
    { IJclKeyOwner }
    function FreeKey(var Key: TObject): TObject;
    function GetOwnsKeys: Boolean;
    property OwnsKeys: Boolean read FOwnsKeys;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read FOwnsValues;,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,,TObject,,TObject)*)

  {$IFDEF SUPPORTS_GENERICS}
(*$JPPEXPANDMACRO JCLHASHMAPTYPESINT(TJclHashEntry<TKey\,TValue>,TJclBucket<TKey\,TValue>,TKey,TValue)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TBucket,TJclHashMap<TKey\,TValue>,TJclAbstractContainerBase,IJclMap<TKey\,TValue>,IJclSet<TKey>,IJclCollection<TValue>, IJclPairOwner<TKey\, TValue>\,,
  protected
    type
      TBucket = TJclBucket<TKey\,TValue>;
  private
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
  protected
    function Hash(const AKey: TKey): Integer; virtual; abstract;
    function KeysEqual(const A\, B: TKey): Boolean; virtual; abstract;
    function ValuesEqual(const A\, B: TValue): Boolean; virtual; abstract;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; virtual; abstract;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; virtual; abstract;
  public
    { IJclPairOwner }
    function FreeKey(var Key: TKey): TKey;
    function FreeValue(var Value: TValue): TValue;
    function GetOwnsKeys: Boolean;
    function GetOwnsValues: Boolean;
    property OwnsKeys: Boolean read FOwnsKeys;
    property OwnsValues: Boolean read FOwnsValues;,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,const ,TKey,const ,TValue)*)

  // E = external helper to compare and hash items
  // KeyComparer is used only when getting KeySet
  // GetHashCode and Equals methods of KeyEqualityComparer are used
  // GetHashCode of ValueEqualityComparer is not used
  TJclHashMapE<TKey, TValue> = class(TJclHashMap<TKey, TValue>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  protected
    type
      TArrayList = TJclArrayListE<TValue>;
      TArraySet = TJclArraySetE<TKey>;
  private
    FKeyEqualityComparer: IJclEqualityComparer<TKey>;
    FKeyHashConverter: IJclHashConverter<TKey>;
    FKeyComparer: IJclComparer<TKey>;
    FValueEqualityComparer: IJclEqualityComparer<TValue>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function Hash(const AKey: TKey): Integer; override;
    function KeysEqual(const A, B: TKey): Boolean; override;
    function ValuesEqual(const A, B: TValue): Boolean; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
  public
    constructor Create(const AKeyEqualityComparer: IJclEqualityComparer<TKey>;
      const AKeyHashConverter: IJclHashConverter<TKey>; const AValueEqualityComparer: IJclEqualityComparer<TValue>;
      const AKeyComparer: IJclComparer<TKey>; ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);

    property KeyEqualityComparer: IJclEqualityComparer<TKey> read FKeyEqualityComparer write FKeyEqualityComparer;
    property KeyHashConverter: IJclHashConverter<TKey> read FKeyHashConverter write FKeyHashConverter;
    property KeyComparer: IJclComparer<TKey> read FKeyComparer write FKeyComparer;
    property ValueEqualityComparer: IJclEqualityComparer<TValue> read FValueEqualityComparer write FValueEqualityComparer;
  end;

  // F = Functions to compare and hash items
  // KeyComparer is used only when getting KeySet
  TJclHashMapF<TKey, TValue> = class(TJclHashMap<TKey, TValue>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  protected
    type
      TArrayList = TJclArrayListF<TValue>;
      TArraySet = TJclArraySetF<TKey>;
  private
    FKeyEqualityCompare: TEqualityCompare<TKey>;
    FKeyHash: THashConvert<TKey>;
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
  public
    constructor Create(AKeyEqualityCompare: TEqualityCompare<TKey>; AKeyHash: THashConvert<TKey>;
      AValueEqualityCompare: TEqualityCompare<TValue>; AKeyCompare: TCompare<TKey>;
      ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);

    property KeyEqualityCompare: TEqualityCompare<TKey> read FKeyEqualityCompare write FKeyEqualityCompare;
    property KeyCompare: TCompare<TKey> read FKeyCompare write FKeyCompare;
    property KeyHash: THashConvert<TKey> read FKeyHash write FKeyHash;
    property ValueEqualityCompare: TEqualityCompare<TValue> read FValueEqualityCompare write FValueEqualityCompare;
  end;

  // I = items can compare themselves to an other, items can create hash value from themselves
  TJclHashMapI<TKey: IComparable<TKey>, IEquatable<TKey>, IHashable; TValue: IEquatable<TValue>> = class(TJclHashMap<TKey, TValue>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer,
    IJclMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  protected
    type
      TArrayList = TJclArrayListI<TValue>;
      TArraySet = TJclArraySetI<TKey>;
  protected
    function Hash(const AKey: TKey): Integer; override;
    function KeysEqual(const A, B: TKey): Boolean; override;
    function ValuesEqual(const A, B: TValue): Boolean; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
  end;
  {$ENDIF SUPPORTS_GENERICS}

function HashMul(Key, Range: Integer): Integer;

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
  SysUtils,
  JclResources;

function HashMul(Key, Range: Integer): Integer;
// return a value between 0 and (Range-1) based on integer-hash Key
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Trunc(Range * (Frac(Abs(Key * A))));
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclIntfIntfBucket,nil,nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfIntfHashMap,TJclIntfIntfBucket,IJclIntfIntfMap,IJclIntfSet,IJclIntfIterator,IJclIntfCollection,,,,const ,IInterface,nil,const ,IInterface,nil,TJclIntfArraySet.Create(FSize),TJclIntfArrayList.Create(FSize))}

function TJclIntfIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfIntfHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclIntfIntfHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclIntfIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclAnsiStrIntfBucket,'',nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclAnsiStrIntfHashMap,TJclAnsiStrIntfBucket,IJclAnsiStrIntfMap,IJclAnsiStrSet,IJclAnsiStrIterator,IJclIntfCollection,,,,const ,AnsiString,'',const ,IInterface,nil,TJclAnsiStrArraySet.Create(FSize),TJclIntfArrayList.Create(FSize))}

function TJclAnsiStrIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrIntfHashMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;

function TJclAnsiStrIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclAnsiStrIntfHashMap.KeysEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclAnsiStrIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclIntfAnsiStrBucket,nil,'')}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfAnsiStrHashMap,TJclIntfAnsiStrBucket,IJclIntfAnsiStrMap,IJclIntfSet,IJclIntfIterator,IJclAnsiStrCollection,,,,const ,IInterface,nil,const ,AnsiString,'',TJclIntfArraySet.Create(FSize),TJclAnsiStrArrayList.Create(FSize))}

function TJclIntfAnsiStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfAnsiStrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfAnsiStrHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfAnsiStrHashMap.FreeValue(var Value: AnsiString): AnsiString;
begin
  Result := Value;
  Value := '';
end;

function TJclIntfAnsiStrHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfAnsiStrHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfAnsiStrHashMap.ValuesEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclAnsiStrAnsiStrBucket,'','')}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclAnsiStrAnsiStrHashMap,TJclAnsiStrAnsiStrBucket,IJclAnsiStrAnsiStrMap,IJclAnsiStrSet,IJclAnsiStrIterator,IJclAnsiStrCollection,,,,const ,AnsiString,'',const ,AnsiString,'',TJclAnsiStrArraySet.Create(FSize),TJclAnsiStrArrayList.Create(FSize))}

function TJclAnsiStrAnsiStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrAnsiStrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrAnsiStrHashMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;

function TJclAnsiStrAnsiStrHashMap.FreeValue(var Value: AnsiString): AnsiString;
begin
  Result := Value;
  Value := '';
end;

function TJclAnsiStrAnsiStrHashMap.KeysEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclAnsiStrAnsiStrHashMap.ValuesEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclWideStrIntfBucket,'',nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclWideStrIntfHashMap,TJclWideStrIntfBucket,IJclWideStrIntfMap,IJclWideStrSet,IJclWideStrIterator,IJclIntfCollection,,,,const ,WideString,'',const ,IInterface,nil,TJclWideStrArraySet.Create(FSize),TJclIntfArrayList.Create(FSize))}

function TJclWideStrIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclWideStrIntfHashMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;

function TJclWideStrIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclWideStrIntfHashMap.KeysEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclWideStrIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclIntfWideStrBucket,nil,'')}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfWideStrHashMap,TJclIntfWideStrBucket,IJclIntfWideStrMap,IJclIntfSet,IJclIntfIterator,IJclWideStrCollection,,,,const ,IInterface,nil,const ,WideString,'',TJclIntfArraySet.Create(FSize),TJclWideStrArrayList.Create(FSize))}

function TJclIntfWideStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfWideStrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfWideStrHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfWideStrHashMap.FreeValue(var Value: WideString): WideString;
begin
  Result := Value;
  Value := '';
end;

function TJclIntfWideStrHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfWideStrHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfWideStrHashMap.ValuesEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclWideStrWideStrBucket,'','')}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclWideStrWideStrHashMap,TJclWideStrWideStrBucket,IJclWideStrWideStrMap,IJclWideStrSet,IJclWideStrIterator,IJclWideStrCollection,,,,const ,WideString,'',const ,WideString,'',TJclWideStrArraySet.Create(FSize),TJclWideStrArrayList.Create(FSize))}

function TJclWideStrWideStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrWideStrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclWideStrWideStrHashMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;

function TJclWideStrWideStrHashMap.FreeValue(var Value: WideString): WideString;
begin
  Result := Value;
  Value := '';
end;

function TJclWideStrWideStrHashMap.KeysEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclWideStrWideStrHashMap.ValuesEqual(const A, B: Widestring): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclUnicodeStrIntfBucket,'',nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclUnicodeStrIntfHashMap,TJclUnicodeStrIntfBucket,IJclUnicodeStrIntfMap,IJclUnicodeStrSet,IJclUnicodeStrIterator,IJclIntfCollection,,,,const ,UnicodeString,'',const ,IInterface,nil,TJclUnicodeStrArraySet.Create(FSize),TJclIntfArrayList.Create(FSize))}

function TJclUnicodeStrIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclUnicodeStrIntfHashMap.FreeKey(var Key: UnicodeString): UnicodeString;
begin
  Result := Key;
  Key := '';
end;

function TJclUnicodeStrIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclUnicodeStrIntfHashMap.KeysEqual(const A, B: UnicodeString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclUnicodeStrIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclIntfUnicodeStrBucket,nil,'')}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfUnicodeStrHashMap,TJclIntfUnicodeStrBucket,IJclIntfUnicodeStrMap,IJclIntfSet,IJclIntfIterator,IJclUnicodeStrCollection,,,,const ,IInterface,nil,const ,UnicodeString,'',TJclIntfArraySet.Create(FSize),TJclUnicodeStrArrayList.Create(FSize))}

function TJclIntfUnicodeStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfUnicodeStrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfUnicodeStrHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfUnicodeStrHashMap.FreeValue(var Value: UnicodeString): UnicodeString;
begin
  Result := Value;
  Value := '';
end;

function TJclIntfUnicodeStrHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfUnicodeStrHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfUnicodeStrHashMap.ValuesEqual(const A, B: UnicodeString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclUnicodeStrUnicodeStrBucket,'','')}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclUnicodeStrUnicodeStrHashMap,TJclUnicodeStrUnicodeStrBucket,IJclUnicodeStrUnicodeStrMap,IJclUnicodeStrSet,IJclUnicodeStrIterator,IJclUnicodeStrCollection,,,,const ,UnicodeString,'',const ,UnicodeString,'',TJclUnicodeStrArraySet.Create(FSize),TJclUnicodeStrArrayList.Create(FSize))}

function TJclUnicodeStrUnicodeStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrUnicodeStrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclUnicodeStrUnicodeStrHashMap.FreeKey(var Key: UnicodeString): UnicodeString;
begin
  Result := Key;
  Key := '';
end;

function TJclUnicodeStrUnicodeStrHashMap.FreeValue(var Value: UnicodeString): UnicodeString;
begin
  Result := Value;
  Value := '';
end;

function TJclUnicodeStrUnicodeStrHashMap.KeysEqual(const A, B: UnicodeString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclUnicodeStrUnicodeStrHashMap.ValuesEqual(const A, B: Unicodestring): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$ENDIF SUPPORTS_UNICODE_STRING}

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclSingleIntfBucket,0.0,nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclSingleIntfHashMap,TJclSingleIntfBucket,IJclSingleIntfMap,IJclSingleSet,IJclSingleIterator,IJclIntfCollection,,,,const ,Single,0.0,const ,IInterface,nil,TJclSingleArraySet.Create(FSize),TJclIntfArrayList.Create(FSize))}

function TJclSingleIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclSingleIntfHashMap.FreeKey(var Key: Single): Single;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclSingleIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclSingleIntfHashMap.KeysEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclSingleIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclIntfSingleBucket,nil,0.0)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfSingleHashMap,TJclIntfSingleBucket,IJclIntfSingleMap,IJclIntfSet,IJclIntfIterator,IJclSingleCollection,,,,const ,IInterface,nil,const ,Single,0.0,TJclIntfArraySet.Create(FSize),TJclSingleArrayList.Create(FSize))}

function TJclIntfSingleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfSingleHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfSingleHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfSingleHashMap.FreeValue(var Value: Single): Single;
begin
  Result := Value;
  Value := 0.0;
end;

function TJclIntfSingleHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfSingleHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfSingleHashMap.ValuesEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclSingleSingleBucket,0.0,0.0)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclSingleSingleHashMap,TJclSingleSingleBucket,IJclSingleSingleMap,IJclSingleSet,IJclSingleIterator,IJclSingleCollection,,,,const ,Single,0.0,const ,Single,0.0,TJclSingleArraySet.Create(FSize),TJclSingleArrayList.Create(FSize))}

function TJclSingleSingleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleSingleHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclSingleSingleHashMap.FreeKey(var Key: Single): Single;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclSingleSingleHashMap.FreeValue(var Value: Single): Single;
begin
  Result := Value;
  Value := 0.0;
end;

function TJclSingleSingleHashMap.KeysEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclSingleSingleHashMap.ValuesEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclDoubleIntfBucket,0.0,nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclDoubleIntfHashMap,TJclDoubleIntfBucket,IJclDoubleIntfMap,IJclDoubleSet,IJclDoubleIterator,IJclIntfCollection,,,,const ,Double,0.0,const ,IInterface,nil,TJclDoubleArraySet.Create(FSize),TJclIntfArrayList.Create(FSize))}

function TJclDoubleIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclDoubleIntfHashMap.FreeKey(var Key: Double): Double;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclDoubleIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclDoubleIntfHashMap.KeysEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclDoubleIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclIntfDoubleBucket,nil,0.0)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfDoubleHashMap,TJclIntfDoubleBucket,IJclIntfDoubleMap,IJclIntfSet,IJclIntfIterator,IJclDoubleCollection,,,,const ,IInterface,nil,const ,Double,0.0,TJclIntfArraySet.Create(FSize),TJclDoubleArrayList.Create(FSize))}

function TJclIntfDoubleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfDoubleHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfDoubleHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfDoubleHashMap.FreeValue(var Value: Double): Double;
begin
  Result := Value;
  Value := 0.0;
end;

function TJclIntfDoubleHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfDoubleHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfDoubleHashMap.ValuesEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclDoubleDoubleBucket,0.0,0.0)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclDoubleDoubleHashMap,TJclDoubleDoubleBucket,IJclDoubleDoubleMap,IJclDoubleSet,IJclDoubleIterator,IJclDoubleCollection,,,,const ,Double,0.0,const ,Double,0.0,TJclDoubleArraySet.Create(FSize),TJclDoubleArrayList.Create(FSize))}

function TJclDoubleDoubleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleDoubleHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclDoubleDoubleHashMap.FreeKey(var Key: Double): Double;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclDoubleDoubleHashMap.FreeValue(var Value: Double): Double;
begin
  Result := Value;
  Value := 0.0;
end;

function TJclDoubleDoubleHashMap.KeysEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclDoubleDoubleHashMap.ValuesEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclExtendedIntfBucket,0.0,nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclExtendedIntfHashMap,TJclExtendedIntfBucket,IJclExtendedIntfMap,IJclExtendedSet,IJclExtendedIterator,IJclIntfCollection,,,,const ,Extended,0.0,const ,IInterface,nil,TJclExtendedArraySet.Create(FSize),TJclIntfArrayList.Create(FSize))}

function TJclExtendedIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclExtendedIntfHashMap.FreeKey(var Key: Extended): Extended;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclExtendedIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclExtendedIntfHashMap.KeysEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclExtendedIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclIntfExtendedBucket,nil,0.0)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfExtendedHashMap,TJclIntfExtendedBucket,IJclIntfExtendedMap,IJclIntfSet,IJclIntfIterator,IJclExtendedCollection,,,,const ,IInterface,nil,const ,Extended,0.0,TJclIntfArraySet.Create(FSize),TJclExtendedArrayList.Create(FSize))}

function TJclIntfExtendedHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfExtendedHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfExtendedHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfExtendedHashMap.FreeValue(var Value: Extended): Extended;
begin
  Result := Value;
  Value := 0.0;
end;

function TJclIntfExtendedHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfExtendedHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfExtendedHashMap.ValuesEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclExtendedExtendedBucket,0.0,0.0)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclExtendedExtendedHashMap,TJclExtendedExtendedBucket,IJclExtendedExtendedMap,IJclExtendedSet,IJclExtendedIterator,IJclExtendedCollection,,,,const ,Extended,0.0,const ,Extended,0.0,TJclExtendedArraySet.Create(FSize),TJclExtendedArrayList.Create(FSize))}

function TJclExtendedExtendedHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedExtendedHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclExtendedExtendedHashMap.FreeKey(var Key: Extended): Extended;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclExtendedExtendedHashMap.FreeValue(var Value: Extended): Extended;
begin
  Result := Value;
  Value := 0.0;
end;

function TJclExtendedExtendedHashMap.KeysEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclExtendedExtendedHashMap.ValuesEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclIntegerIntfBucket,0,nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntegerIntfHashMap,TJclIntegerIntfBucket,IJclIntegerIntfMap,IJclIntegerSet,IJclIntegerIterator,IJclIntfCollection,,,,,Integer,0,const ,IInterface,nil,TJclIntegerArraySet.Create(FSize),TJclIntfArrayList.Create(FSize))}

function TJclIntegerIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntegerIntfHashMap.FreeKey(var Key: Integer): Integer;
begin
  Result := Key;
  Key := 0;
end;

function TJclIntegerIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclIntegerIntfHashMap.KeysEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclIntegerIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclIntfIntegerBucket,nil,0)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfIntegerHashMap,TJclIntfIntegerBucket,IJclIntfIntegerMap,IJclIntfSet,IJclIntfIterator,IJclIntegerCollection,,,,const ,IInterface,nil,,Integer,0,TJclIntfArraySet.Create(FSize),TJclIntegerArrayList.Create(FSize))}

function TJclIntfIntegerHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfIntegerHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfIntegerHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfIntegerHashMap.FreeValue(var Value: Integer): Integer;
begin
  Result := Value;
  Value := 0;
end;

function TJclIntfIntegerHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfIntegerHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfIntegerHashMap.ValuesEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclIntegerIntegerBucket,0,0)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntegerIntegerHashMap,TJclIntegerIntegerBucket,IJclIntegerIntegerMap,IJclIntegerSet,IJclIntegerIterator,IJclIntegerCollection,,,,,Integer,0,,Integer,0,TJclIntegerArraySet.Create(FSize),TJclIntegerArrayList.Create(FSize))}

function TJclIntegerIntegerHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerIntegerHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntegerIntegerHashMap.FreeKey(var Key: Integer): Integer;
begin
  Result := Key;
  Key := 0;
end;

function TJclIntegerIntegerHashMap.FreeValue(var Value: Integer): Integer;
begin
  Result := Value;
  Value := 0;
end;

function TJclIntegerIntegerHashMap.KeysEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclIntegerIntegerHashMap.ValuesEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclCardinalIntfBucket,0,nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclCardinalIntfHashMap,TJclCardinalIntfBucket,IJclCardinalIntfMap,IJclCardinalSet,IJclCardinalIterator,IJclIntfCollection,,,,,Cardinal,0,const ,IInterface,nil,TJclCardinalArraySet.Create(FSize),TJclIntfArrayList.Create(FSize))}

function TJclCardinalIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclCardinalIntfHashMap.FreeKey(var Key: Cardinal): Cardinal;
begin
  Result := Key;
  Key := 0;
end;

function TJclCardinalIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclCardinalIntfHashMap.KeysEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclCardinalIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclIntfCardinalBucket,nil,0)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfCardinalHashMap,TJclIntfCardinalBucket,IJclIntfCardinalMap,IJclIntfSet,IJclIntfIterator,IJclCardinalCollection,,,,const ,IInterface,nil,,Cardinal,0,TJclIntfArraySet.Create(FSize),TJclCardinalArrayList.Create(FSize))}

function TJclIntfCardinalHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfCardinalHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfCardinalHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfCardinalHashMap.FreeValue(var Value: Cardinal): Cardinal;
begin
  Result := Value;
  Value := 0;
end;

function TJclIntfCardinalHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfCardinalHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfCardinalHashMap.ValuesEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclCardinalCardinalBucket,0,0)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclCardinalCardinalHashMap,TJclCardinalCardinalBucket,IJclCardinalCardinalMap,IJclCardinalSet,IJclCardinalIterator,IJclCardinalCollection,,,,,Cardinal,0,,Cardinal,0,TJclCardinalArraySet.Create(FSize),TJclCardinalArrayList.Create(FSize))}

function TJclCardinalCardinalHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalCardinalHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclCardinalCardinalHashMap.FreeKey(var Key: Cardinal): Cardinal;
begin
  Result := Key;
  Key := 0;
end;

function TJclCardinalCardinalHashMap.FreeValue(var Value: Cardinal): Cardinal;
begin
  Result := Value;
  Value := 0;
end;

function TJclCardinalCardinalHashMap.KeysEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclCardinalCardinalHashMap.ValuesEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclInt64IntfBucket,0,nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclInt64IntfHashMap,TJclInt64IntfBucket,IJclInt64IntfMap,IJclInt64Set,IJclInt64Iterator,IJclIntfCollection,,,,const ,Int64,0,const ,IInterface,nil,TJclInt64ArraySet.Create(FSize),TJclIntfArrayList.Create(FSize))}

function TJclInt64IntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64IntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclInt64IntfHashMap.FreeKey(var Key: Int64): Int64;
begin
  Result := Key;
  Key := 0;
end;

function TJclInt64IntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclInt64IntfHashMap.KeysEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclInt64IntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclIntfInt64Bucket,nil,0)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfInt64HashMap,TJclIntfInt64Bucket,IJclIntfInt64Map,IJclIntfSet,IJclIntfIterator,IJclInt64Collection,,,,const ,IInterface,nil,const ,Int64,0,TJclIntfArraySet.Create(FSize),TJclInt64ArrayList.Create(FSize))}

function TJclIntfInt64HashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfInt64HashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfInt64HashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfInt64HashMap.FreeValue(var Value: Int64): Int64;
begin
  Result := Value;
  Value := 0;
end;

function TJclIntfInt64HashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfInt64HashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfInt64HashMap.ValuesEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclInt64Int64Bucket,0,0)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclInt64Int64HashMap,TJclInt64Int64Bucket,IJclInt64Int64Map,IJclInt64Set,IJclInt64Iterator,IJclInt64Collection,,,,const ,Int64,0,const ,Int64,0,TJclInt64ArraySet.Create(FSize),TJclInt64ArrayList.Create(FSize))}

function TJclInt64Int64HashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64Int64HashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclInt64Int64HashMap.FreeKey(var Key: Int64): Int64;
begin
  Result := Key;
  Key := 0;
end;

function TJclInt64Int64HashMap.FreeValue(var Value: Int64): Int64;
begin
  Result := Value;
  Value := 0;
end;

function TJclInt64Int64HashMap.KeysEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclInt64Int64HashMap.ValuesEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclPtrIntfBucket,nil,nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclPtrIntfHashMap,TJclPtrIntfBucket,IJclPtrIntfMap,IJclPtrSet,IJclPtrIterator,IJclIntfCollection,,,,,Pointer,nil,const ,IInterface,nil,TJclPtrArraySet.Create(FSize),TJclIntfArrayList.Create(FSize))}

function TJclPtrIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclPtrIntfHashMap.FreeKey(var Key: Pointer): Pointer;
begin
  Result := Key;
  Key := nil;
end;

function TJclPtrIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclPtrIntfHashMap.KeysEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclPtrIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclIntfPtrBucket,nil,nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfPtrHashMap,TJclIntfPtrBucket,IJclIntfPtrMap,IJclIntfSet,IJclIntfIterator,IJclPtrCollection,,,,const ,IInterface,nil,,Pointer,nil,TJclIntfArraySet.Create(FSize),TJclPtrArrayList.Create(FSize))}

function TJclIntfPtrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfPtrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfPtrHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfPtrHashMap.FreeValue(var Value: Pointer): Pointer;
begin
  Result := Value;
  Value := nil;
end;

function TJclIntfPtrHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfPtrHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfPtrHashMap.ValuesEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclPtrPtrBucket,nil,nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclPtrPtrHashMap,TJclPtrPtrBucket,IJclPtrPtrMap,IJclPtrSet,IJclPtrIterator,IJclPtrCollection,,,,,Pointer,nil,,Pointer,nil,TJclPtrArraySet.Create(FSize),TJclPtrArrayList.Create(FSize))}

function TJclPtrPtrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrPtrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclPtrPtrHashMap.FreeKey(var Key: Pointer): Pointer;
begin
  Result := Key;
  Key := nil;
end;

function TJclPtrPtrHashMap.FreeValue(var Value: Pointer): Pointer;
begin
  Result := Value;
  Value := nil;
end;

function TJclPtrPtrHashMap.KeysEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclPtrPtrHashMap.ValuesEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclIntfBucket,nil,nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfHashMap,TJclIntfBucket,IJclIntfMap,IJclIntfSet,IJclIntfIterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,const ,IInterface,nil,,TObject,nil,TJclIntfArraySet.Create(FSize),TJclArrayList.Create(FSize, False))}

function TJclIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclIntfHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

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

function TJclIntfHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclIntfHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclAnsiStrBucket,'',nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclAnsiStrHashMap,TJclAnsiStrBucket,IJclAnsiStrMap,IJclAnsiStrSet,IJclAnsiStrIterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,const ,AnsiString,'',,TObject,nil,TJclAnsiStrArraySet.Create(FSize),TJclArrayList.Create(FSize, False))}

function TJclAnsiStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrHashMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;

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

function TJclAnsiStrHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclAnsiStrHashMap.KeysEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclAnsiStrHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclWideStrBucket,'',nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclWideStrHashMap,TJclWideStrBucket,IJclWideStrMap,IJclWideStrSet,IJclWideStrIterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,const ,WideString,'',,TObject,nil,TJclWideStrArraySet.Create(FSize),TJclArrayList.Create(FSize, False))}

function TJclWideStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclWideStrHashMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;

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

function TJclWideStrHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclWideStrHashMap.KeysEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclWideStrHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclUnicodeStrBucket,'',nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclUnicodeStrHashMap,TJclUnicodeStrBucket,IJclUnicodeStrMap,IJclUnicodeStrSet,IJclUnicodeStrIterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,const ,UnicodeString,'',,TObject,nil,TJclUnicodeStrArraySet.Create(FSize),TJclArrayList.Create(FSize, False))}

function TJclUnicodeStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclUnicodeStrHashMap.FreeKey(var Key: UnicodeString): UnicodeString;
begin
  Result := Key;
  Key := '';
end;

function TJclUnicodeStrHashMap.FreeValue(var Value: TObject): TObject;
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

function TJclUnicodeStrHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclUnicodeStrHashMap.KeysEqual(const A, B: UnicodeString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclUnicodeStrHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclSingleBucket,0.0,nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclSingleHashMap,TJclSingleBucket,IJclSingleMap,IJclSingleSet,IJclSingleIterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,const ,Single,0.0,,TObject,nil,TJclSingleArraySet.Create(FSize),TJclArrayList.Create(FSize, False))}

function TJclSingleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclSingleHashMap.FreeKey(var Key: Single): Single;
begin
  Result := Key;
  Key := 0.0;
end;

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

function TJclSingleHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclSingleHashMap.KeysEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclSingleHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclDoubleBucket,0.0,nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclDoubleHashMap,TJclDoubleBucket,IJclDoubleMap,IJclDoubleSet,IJclDoubleIterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,const ,Double,0.0,,TObject,nil,TJclDoubleArraySet.Create(FSize),TJclArrayList.Create(FSize, False))}

function TJclDoubleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclDoubleHashMap.FreeKey(var Key: Double): Double;
begin
  Result := Key;
  Key := 0.0;
end;

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

function TJclDoubleHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclDoubleHashMap.KeysEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclDoubleHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclExtendedBucket,0.0,nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclExtendedHashMap,TJclExtendedBucket,IJclExtendedMap,IJclExtendedSet,IJclExtendedIterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,const ,Extended,0.0,,TObject,nil,TJclExtendedArraySet.Create(FSize),TJclArrayList.Create(FSize, False))}

function TJclExtendedHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclExtendedHashMap.FreeKey(var Key: Extended): Extended;
begin
  Result := Key;
  Key := 0.0;
end;

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

function TJclExtendedHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclExtendedHashMap.KeysEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclExtendedHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclIntegerBucket,0,nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntegerHashMap,TJclIntegerBucket,IJclIntegerMap,IJclIntegerSet,IJclIntegerIterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,,Integer,0,,TObject,nil,TJclIntegerArraySet.Create(FSize),TJclArrayList.Create(FSize, False))}

function TJclIntegerHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclIntegerHashMap.FreeKey(var Key: Integer): Integer;
begin
  Result := Key;
  Key := 0;
end;

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

function TJclIntegerHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclIntegerHashMap.KeysEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclIntegerHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclCardinalBucket,0,nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclCardinalHashMap,TJclCardinalBucket,IJclCardinalMap,IJclCardinalSet,IJclCardinalIterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,,Cardinal,0,,TObject,nil,TJclCardinalArraySet.Create(FSize),TJclArrayList.Create(FSize, False))}

function TJclCardinalHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclCardinalHashMap.FreeKey(var Key: Cardinal): Cardinal;
begin
  Result := Key;
  Key := 0;
end;

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

function TJclCardinalHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclCardinalHashMap.KeysEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclCardinalHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclInt64Bucket,0,nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclInt64HashMap,TJclInt64Bucket,IJclInt64Map,IJclInt64Set,IJclInt64Iterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,const ,Int64,0,,TObject,nil,TJclInt64ArraySet.Create(FSize),TJclArrayList.Create(FSize, False))}

function TJclInt64HashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64HashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclInt64HashMap.FreeKey(var Key: Int64): Int64;
begin
  Result := Key;
  Key := 0;
end;

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

function TJclInt64HashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclInt64HashMap.KeysEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclInt64HashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclPtrBucket,nil,nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclPtrHashMap,TJclPtrBucket,IJclPtrMap,IJclPtrSet,IJclPtrIterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,,Pointer,nil,,TObject,nil,TJclPtrArraySet.Create(FSize),TJclArrayList.Create(FSize, False))}

function TJclPtrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclPtrHashMap.FreeKey(var Key: Pointer): Pointer;
begin
  Result := Key;
  Key := nil;
end;

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

function TJclPtrHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclPtrHashMap.KeysEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclPtrHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclBucket,nil,nil)}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclHashMap,TJclBucket,IJclMap,IJclSet,IJclIterator,IJclCollection,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;,,TObject,nil,,TObject,nil,TJclArraySet.Create(FSize, False),TJclArrayList.Create(FSize, False))}

function TJclHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashMap.Create(FCapacity, False, False);
  AssignPropertiesTo(Result);
end;

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

function TJclHashMap.GetOwnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;

function TJclHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclHashMap.Hash(AObject: TObject): Integer;
begin
  Result := Integer(AObject);
end;

function TJclHashMap.KeysEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$IFDEF SUPPORTS_GENERICS}
{$JPPEXPANDMACRO JCLHASHMAPTYPESIMP(TJclBucket<TKey\, TValue>,Default(TKey),Default(TValue))}

{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclHashMap<TKey\, TValue>,TBucket,IJclMap<TKey\, TValue>,IJclSet<TKey>,IJclIterator<TKey>,IJclCollection<TValue>,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;,const ,TKey,Default(TKey),const ,TValue,Default(TValue),CreateEmptyArraySet(FSize, False),CreateEmptyArrayList(FSize, False))}

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

function TJclHashMap<TKey, TValue>.GetOwnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;

function TJclHashMap<TKey, TValue>.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

//=== { TJclHashMapE<TKey, TValue> } =========================================

constructor TJclHashMapE<TKey, TValue>.Create(const AKeyEqualityComparer: IJclEqualityComparer<TKey>;
  const AKeyHashConverter: IJclHashConverter<TKey>; const AValueEqualityComparer: IJclEqualityComparer<TValue>;
  const AKeyComparer: IJclComparer<TKey>; ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create(ACapacity, AOwnsKeys, AOwnsValues);
  FKeyEqualityComparer := AKeyEqualityComparer;
  FKeyHashConverter := AKeyHashConverter;
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
    ADest.FKeyHashConverter := FKeyHashConverter;
    ADest.FValueEqualityComparer := FValueEqualityComparer;
    ADest.FKeyComparer := FKeyComparer;
  end;
end;

function TJclHashMapE<TKey, TValue>.CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>;
begin
  Result := TArrayList.Create(ValueEqualityComparer, ACapacity, AOwnsObjects);
end;

function TJclHashMapE<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>;
begin
  Result := TArraySet.Create(KeyComparer, ACapacity, AOwnsObjects);
end;

function TJclHashMapE<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashMapE<TKey, TValue>.Create(KeyEqualityComparer, KeyHashConverter, ValueEqualityComparer,
    KeyComparer, FCapacity, False, False);
  AssignPropertiesTo(Result);
end;

function TJclHashMapE<TKey, TValue>.Hash(const AKey: TKey): Integer;
begin
  if KeyEqualityComparer = nil then
    raise EJclNoHashConverterError.Create;
  Result := KeyHashConverter.Hash(AKey);
end;

function TJclHashMapE<TKey, TValue>.KeysEqual(const A, B: TKey): Boolean;
begin
  if KeyEqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := KeyEqualityComparer.ItemsEqual(A, B);
end;

function TJclHashMapE<TKey, TValue>.ValuesEqual(const A, B: TValue): Boolean;
begin
  if ValueEqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := ValueEqualityComparer.ItemsEqual(A, B);
end;

//=== { TJclHashMapF<TKey, TValue> } =========================================

constructor TJclHashMapF<TKey, TValue>.Create(AKeyEqualityCompare: TEqualityCompare<TKey>;
  AKeyHash: THashConvert<TKey>; AValueEqualityCompare: TEqualityCompare<TValue>; AKeyCompare: TCompare<TKey>;
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
  Result := TArrayList.Create(ValueEqualityCompare, ACapacity, AOwnsObjects);
end;

function TJclHashMapF<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>;
begin
  Result := TArraySet.Create(KeyCompare, ACapacity, AOwnsObjects);
end;

function TJclHashMapF<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashMapF<TKey, TValue>.Create(KeyEqualityCompare, KeyHash, ValueEqualityCompare, KeyCompare, FCapacity,
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
  Result := TArrayList.Create(ACapacity, AOwnsObjects);
end;

function TJclHashMapI<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>;
begin
  Result := TArraySet.Create(ACapacity, AOwnsObjects);
end;

function TJclHashMapI<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashMapI<TKey, TValue>.Create(FCapacity, False, False);
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

