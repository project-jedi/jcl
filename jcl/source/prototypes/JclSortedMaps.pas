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
{ Last modified: $Date::                                                                        $ }
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
  JclBase, JclSynch,
  JclAbstractContainers, JclContainerIntf, JclArrayLists, JclArraySets;
{$I containers\JclContainerCommon.imp}
{$I containers\JclSortedMaps.imp}
{$I containers\JclSortedMaps.int}
type
(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclIntfIntfSortedEntry,IInterface,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntfIntfSortedEntry,TJclIntfIntfSortedMap,TJclIntfAbstractContainer,IJclIntfIntfMap,IJclIntfIntfSortedMap,IJclIntfSet,IJclIntfCollection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(const A\, B: IInterface): Integer;,,,const ,IInterface,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclAnsiStrIntfSortedEntry,AnsiString,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclAnsiStrIntfSortedEntry,TJclAnsiStrIntfSortedMap,TJclAnsiStrAbstractContainer,IJclAnsiStrIntfMap,IJclAnsiStrIntfSortedMap,IJclAnsiStrSet,IJclIntfCollection, IJclStrContainer\, IJclAnsiStrContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A\, B: AnsiString): Integer;
    function ValuesCompare(const A\, B: IInterface): Integer;,,,const ,AnsiString,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclIntfAnsiStrSortedEntry,IInterface,AnsiString)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntfAnsiStrSortedEntry,TJclIntfAnsiStrSortedMap,TJclAnsiStrAbstractContainer,IJclIntfAnsiStrMap,IJclIntfAnsiStrSortedMap,IJclIntfSet,IJclAnsiStrCollection, IJclStrContainer\, IJclAnsiStrContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: AnsiString): AnsiString;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(const A\, B: AnsiString): Integer;,,,const ,IInterface,const ,AnsiString)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclAnsiStrAnsiStrSortedEntry,AnsiString,AnsiString)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclAnsiStrAnsiStrSortedEntry,TJclAnsiStrAnsiStrSortedMap,TJclAnsiStrAbstractContainer,IJclAnsiStrAnsiStrMap,IJclAnsiStrAnsiStrSortedMap,IJclAnsiStrSet,IJclAnsiStrCollection, IJclStrContainer\, IJclAnsiStrContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function FreeValue(var Value: AnsiString): AnsiString;
    function KeysCompare(const A\, B: AnsiString): Integer;
    function ValuesCompare(const A\, B: AnsiString): Integer;,,,const ,AnsiString,const ,AnsiString)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclWideStrIntfSortedEntry,WideString,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclWideStrIntfSortedEntry,TJclWideStrIntfSortedMap,TJclWideStrAbstractContainer,IJclWideStrIntfMap,IJclWideStrIntfSortedMap,IJclWideStrSet,IJclIntfCollection, IJclStrContainer\, IJclWideStrContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A\, B: WideString): Integer;
    function ValuesCompare(const A\, B: IInterface): Integer;,,,const ,WideString,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclIntfWideStrSortedEntry,IInterface,WideString)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntfWideStrSortedEntry,TJclIntfWideStrSortedMap,TJclWideStrAbstractContainer,IJclIntfWideStrMap,IJclIntfWideStrSortedMap,IJclIntfSet,IJclWideStrCollection, IJclStrContainer\, IJclWideStrContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: WideString): WideString;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(const A\, B: WideString): Integer;,,,const ,IInterface,const ,WideString)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclWideStrWideStrSortedEntry,WideString,WideString)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclWideStrWideStrSortedEntry,TJclWideStrWideStrSortedMap,TJclWideStrAbstractContainer,IJclWideStrWideStrMap,IJclWideStrWideStrSortedMap,IJclWideStrSet,IJclWideStrCollection, IJclStrContainer\, IJclWideStrContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function FreeValue(var Value: WideString): WideString;
    function KeysCompare(const A\, B: WideString): Integer;
    function ValuesCompare(const A\, B: WideString): Integer;,,,const ,WideString,const ,WideString)*)

{$IFDEF SUPPORTS_UNICODE_STRING}
(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclUnicodeStrIntfSortedEntry,UnicodeString,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclUnicodeStrIntfSortedEntry,TJclUnicodeStrIntfSortedMap,TJclUnicodeStrAbstractContainer,IJclUnicodeStrIntfMap,IJclUnicodeStrIntfSortedMap,IJclUnicodeStrSet,IJclIntfCollection, IJclStrContainer\, IJclUnicodeStrContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: UnicodeString): UnicodeString;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A\, B: UnicodeString): Integer;
    function ValuesCompare(const A\, B: IInterface): Integer;,,,const ,UnicodeString,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclIntfUnicodeStrSortedEntry,IInterface,UnicodeString)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntfUnicodeStrSortedEntry,TJclIntfUnicodeStrSortedMap,TJclUnicodeStrAbstractContainer,IJclIntfUnicodeStrMap,IJclIntfUnicodeStrSortedMap,IJclIntfSet,IJclUnicodeStrCollection, IJclStrContainer\, IJclUnicodeStrContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: UnicodeString): UnicodeString;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(const A\, B: UnicodeString): Integer;,,,const ,IInterface,const ,UnicodeString)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclUnicodeStrUnicodeStrSortedEntry,UnicodeString,UnicodeString)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclUnicodeStrUnicodeStrSortedEntry,TJclUnicodeStrUnicodeStrSortedMap,TJclUnicodeStrAbstractContainer,IJclUnicodeStrUnicodeStrMap,IJclUnicodeStrUnicodeStrSortedMap,IJclUnicodeStrSet,IJclUnicodeStrCollection, IJclStrContainer\, IJclUnicodeStrContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: UnicodeString): UnicodeString;
    function FreeValue(var Value: UnicodeString): UnicodeString;
    function KeysCompare(const A\, B: UnicodeString): Integer;
    function ValuesCompare(const A\, B: UnicodeString): Integer;,,,const ,UnicodeString,const ,UnicodeString)*)
{$ENDIF SUPPORTS_UNICODE_STRING}

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
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrIntfSortedMap = TJclUnicodeStrIntfSortedMap;
  TJclIntfStrSortedMap = TJclIntfUnicodeStrSortedMap;
  TJclStrStrSortedMap = TJclUnicodeStrUnicodeStrSortedMap;
  {$ENDIF CONTAINER_UNICODESTR}

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclSingleIntfSortedEntry,Single,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclSingleIntfSortedEntry,TJclSingleIntfSortedMap,TJclSingleAbstractContainer,IJclSingleIntfMap,IJclSingleIntfSortedMap,IJclSingleSet,IJclIntfCollection, IJclSingleContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Single): Single;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A\, B: Single): Integer;
    function ValuesCompare(const A\, B: IInterface): Integer;,,,const ,Single,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclIntfSingleSortedEntry,IInterface,Single)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntfSingleSortedEntry,TJclIntfSingleSortedMap,TJclSingleAbstractContainer,IJclIntfSingleMap,IJclIntfSingleSortedMap,IJclIntfSet,IJclSingleCollection, IJclSingleContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Single): Single;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(const A\, B: Single): Integer;,,,const ,IInterface,const ,Single)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclSingleSingleSortedEntry,Single,Single)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclSingleSingleSortedEntry,TJclSingleSingleSortedMap,TJclSingleAbstractContainer,IJclSingleSingleMap,IJclSingleSingleSortedMap,IJclSingleSet,IJclSingleCollection, IJclSingleContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Single): Single;
    function FreeValue(var Value: Single): Single;
    function KeysCompare(const A\, B: Single): Integer;
    function ValuesCompare(const A\, B: Single): Integer;,,,const ,Single,const ,Single)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclDoubleIntfSortedEntry,Double,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclDoubleIntfSortedEntry,TJclDoubleIntfSortedMap,TJclDoubleAbstractContainer,IJclDoubleIntfMap,IJclDoubleIntfSortedMap,IJclDoubleSet,IJclIntfCollection, IJclDoubleContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Double): Double;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A\, B: Double): Integer;
    function ValuesCompare(const A\, B: IInterface): Integer;,,,const ,Double,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclIntfDoubleSortedEntry,IInterface,Double)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntfDoubleSortedEntry,TJclIntfDoubleSortedMap,TJclDoubleAbstractContainer,IJclIntfDoubleMap,IJclIntfDoubleSortedMap,IJclIntfSet,IJclDoubleCollection, IJclDoubleContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Double): Double;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(const A\, B: Double): Integer;,,,const ,IInterface,const ,Double)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclDoubleDoubleSortedEntry,Double,Double)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclDoubleDoubleSortedEntry,TJclDoubleDoubleSortedMap,TJclDoubleAbstractContainer,IJclDoubleDoubleMap,IJclDoubleDoubleSortedMap,IJclDoubleSet,IJclDoubleCollection, IJclDoubleContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Double): Double;
    function FreeValue(var Value: Double): Double;
    function KeysCompare(const A\, B: Double): Integer;
    function ValuesCompare(const A\, B: Double): Integer;,,,const ,Double,const ,Double)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclExtendedIntfSortedEntry,Extended,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclExtendedIntfSortedEntry,TJclExtendedIntfSortedMap,TJclExtendedAbstractContainer,IJclExtendedIntfMap,IJclExtendedIntfSortedMap,IJclExtendedSet,IJclIntfCollection, IJclExtendedContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Extended): Extended;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A\, B: Extended): Integer;
    function ValuesCompare(const A\, B: IInterface): Integer;,,,const ,Extended,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclIntfExtendedSortedEntry,IInterface,Extended)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntfExtendedSortedEntry,TJclIntfExtendedSortedMap,TJclExtendedAbstractContainer,IJclIntfExtendedMap,IJclIntfExtendedSortedMap,IJclIntfSet,IJclExtendedCollection, IJclExtendedContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Extended): Extended;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(const A\, B: Extended): Integer;,,,const ,IInterface,const ,Extended)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclExtendedExtendedSortedEntry,Extended,Extended)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclExtendedExtendedSortedEntry,TJclExtendedExtendedSortedMap,TJclExtendedAbstractContainer,IJclExtendedExtendedMap,IJclExtendedExtendedSortedMap,IJclExtendedSet,IJclExtendedCollection, IJclExtendedContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Extended): Extended;
    function FreeValue(var Value: Extended): Extended;
    function KeysCompare(const A\, B: Extended): Integer;
    function ValuesCompare(const A\, B: Extended): Integer;,,,const ,Extended,const ,Extended)*)

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatIntfSortedMap = TJclExtendedIntfSortedMap;
  TJclIntfFloatSortedMap = TJclIntfExtendedSortedMap;
  TJclFloatFloatSortedMap = TJclExtendedExtendedSortedMap;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatIntfSortedMap = TJclDoubleIntfSortedMap;
  TJclIntfFloatSortedMap = TJclIntfDoubleSortedMap;
  TJclFloatFloatSortedMap = TJclDoubleDoubleSortedMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatIntfSortedMap = TJclSingleIntfSortedMap;
  TJclIntfFloatSortedMap = TJclIntfSingleSortedMap;
  TJclFloatFloatSortedMap = TJclSingleSingleSortedMap;
  {$ENDIF MATH_SINGLE_PRECISION}

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclIntegerIntfSortedEntry,Integer,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntegerIntfSortedEntry,TJclIntegerIntfSortedMap,TJclIntegerAbstractContainer,IJclIntegerIntfMap,IJclIntegerIntfSortedMap,IJclIntegerSet,IJclIntfCollection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Integer): Integer;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(A\, B: Integer): Integer;
    function ValuesCompare(const A\, B: IInterface): Integer;,,,,Integer,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclIntfIntegerSortedEntry,IInterface,Integer)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntfIntegerSortedEntry,TJclIntfIntegerSortedMap,TJclIntegerAbstractContainer,IJclIntfIntegerMap,IJclIntfIntegerSortedMap,IJclIntfSet,IJclIntegerCollection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Integer): Integer;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(A\, B: Integer): Integer;,,,const ,IInterface,,Integer)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclIntegerIntegerSortedEntry,Integer,Integer)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntegerIntegerSortedEntry,TJclIntegerIntegerSortedMap,TJclIntegerAbstractContainer,IJclIntegerIntegerMap,IJclIntegerIntegerSortedMap,IJclIntegerSet,IJclIntegerCollection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Integer): Integer;
    function FreeValue(var Value: Integer): Integer;
    function KeysCompare(A\, B: Integer): Integer;
    function ValuesCompare(A\, B: Integer): Integer;,,,,Integer,,Integer)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclCardinalIntfSortedEntry,Cardinal,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclCardinalIntfSortedEntry,TJclCardinalIntfSortedMap,TJclCardinalAbstractContainer,IJclCardinalIntfMap,IJclCardinalIntfSortedMap,IJclCardinalSet,IJclIntfCollection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Cardinal): Cardinal;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(A\, B: Cardinal): Integer;
    function ValuesCompare(const A\, B: IInterface): Integer;,,,,Cardinal,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclIntfCardinalSortedEntry,IInterface,Cardinal)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntfCardinalSortedEntry,TJclIntfCardinalSortedMap,TJclCardinalAbstractContainer,IJclIntfCardinalMap,IJclIntfCardinalSortedMap,IJclIntfSet,IJclCardinalCollection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Cardinal): Cardinal;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(A\, B: Cardinal): Integer;,,,const ,IInterface,,Cardinal)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclCardinalCardinalSortedEntry,Cardinal,Cardinal)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclCardinalCardinalSortedEntry,TJclCardinalCardinalSortedMap,TJclCardinalAbstractContainer,IJclCardinalCardinalMap,IJclCardinalCardinalSortedMap,IJclCardinalSet,IJclCardinalCollection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Cardinal): Cardinal;
    function FreeValue(var Value: Cardinal): Cardinal;
    function KeysCompare(A\, B: Cardinal): Integer;
    function ValuesCompare(A\, B: Cardinal): Integer;,,,,Cardinal,,Cardinal)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclInt64IntfSortedEntry,Int64,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclInt64IntfSortedEntry,TJclInt64IntfSortedMap,TJclInt64AbstractContainer,IJclInt64IntfMap,IJclInt64IntfSortedMap,IJclInt64Set,IJclIntfCollection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Int64): Int64;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A\, B: Int64): Integer;
    function ValuesCompare(const A\, B: IInterface): Integer;,,,const ,Int64,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclIntfInt64SortedEntry,IInterface,Int64)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntfInt64SortedEntry,TJclIntfInt64SortedMap,TJclInt64AbstractContainer,IJclIntfInt64Map,IJclIntfInt64SortedMap,IJclIntfSet,IJclInt64Collection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Int64): Int64;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(const A\, B: Int64): Integer;,,,const ,IInterface,const ,Int64)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclInt64Int64SortedEntry,Int64,Int64)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclInt64Int64SortedEntry,TJclInt64Int64SortedMap,TJclInt64AbstractContainer,IJclInt64Int64Map,IJclInt64Int64SortedMap,IJclInt64Set,IJclInt64Collection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Int64): Int64;
    function FreeValue(var Value: Int64): Int64;
    function KeysCompare(const A\, B: Int64): Integer;
    function ValuesCompare(const A\, B: Int64): Integer;,,,const ,Int64,const ,Int64)*)

  {$IFNDEF CLR}
(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclPtrIntfSortedEntry,Pointer,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclPtrIntfSortedEntry,TJclPtrIntfSortedMap,TJclPtrAbstractContainer,IJclPtrIntfMap,IJclPtrIntfSortedMap,IJclPtrSet,IJclIntfCollection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Pointer): Pointer;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(A\, B: Pointer): Integer;
    function ValuesCompare(const A\, B: IInterface): Integer;,,,,Pointer,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclIntfPtrSortedEntry,IInterface,Pointer)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntfPtrSortedEntry,TJclIntfPtrSortedMap,TJclPtrAbstractContainer,IJclIntfPtrMap,IJclIntfPtrSortedMap,IJclIntfSet,IJclPtrCollection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Pointer): Pointer;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(A\, B: Pointer): Integer;,,,const ,IInterface,,Pointer)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclPtrPtrSortedEntry,Pointer,Pointer)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclPtrPtrSortedEntry,TJclPtrPtrSortedMap,TJclPtrAbstractContainer,IJclPtrPtrMap,IJclPtrPtrSortedMap,IJclPtrSet,IJclPtrCollection,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Pointer): Pointer;
    function FreeValue(var Value: Pointer): Pointer;
    function KeysCompare(A\, B: Pointer): Integer;
    function ValuesCompare(A\, B: Pointer): Integer;,,,,Pointer,,Pointer)*)
  {$ENDIF ~CLR}

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclIntfSortedEntry,IInterface,TObject)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntfSortedEntry,TJclIntfSortedMap,TJclIntfAbstractContainer,IJclIntfMap,IJclIntfSortedMap,IJclIntfSet,IJclCollection, IJclValueOwner\,,
  private
    FOwnsValues: Boolean;
  protected
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(A\, B: TObject): Integer;
  public
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const ,IInterface,,TObject)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclAnsiStrSortedEntry,AnsiString,TObject)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclAnsiStrSortedEntry,TJclAnsiStrSortedMap,TJclAnsiStrAbstractContainer,IJclAnsiStrMap,IJclAnsiStrSortedMap,IJclAnsiStrSet,IJclCollection, IJclStrContainer\, IJclAnsiStrContainer\, IJclValueOwner\,,
  private
    FOwnsValues: Boolean;
  protected
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function KeysCompare(const A\, B: AnsiString): Integer;
    function ValuesCompare(A\, B: TObject): Integer;
  public
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const ,AnsiString,,TObject)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclWideStrSortedEntry,WideString,TObject)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclWideStrSortedEntry,TJclWideStrSortedMap,TJclWideStrAbstractContainer,IJclWideStrMap,IJclWideStrSortedMap,IJclWideStrSet,IJclCollection, IJclStrContainer\, IJclWideStrContainer\, IJclValueOwner\,,
  private
    FOwnsValues: Boolean;
  protected
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function KeysCompare(const A\, B: WideString): Integer;
    function ValuesCompare(A\, B: TObject): Integer;
  public
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const ,WideString,,TObject)*)

{$IFDEF SUPPORTS_UNICODE_STRING}
(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclUnicodeStrSortedEntry,UnicodeString,TObject)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclUnicodeStrSortedEntry,TJclUnicodeStrSortedMap,TJclUnicodeStrAbstractContainer,IJclUnicodeStrMap,IJclUnicodeStrSortedMap,IJclUnicodeStrSet,IJclCollection, IJclStrContainer\, IJclUnicodeStrContainer\, IJclValueOwner\,,
  private
    FOwnsValues: Boolean;
  protected
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: UnicodeString): UnicodeString;
    function KeysCompare(const A\, B: UnicodeString): Integer;
    function ValuesCompare(A\, B: TObject): Integer;
  public
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const ,UnicodeString,,TObject)*)
{$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrSortedMap = TJclAnsiStrSortedMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrSortedMap = TJclWideStrSortedMap;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrSortedMap = TJclUnicodeStrSortedMap;
  {$ENDIF CONTAINER_UNICODESTR}

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclSingleSortedEntry,Single,TObject)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclSingleSortedEntry,TJclSingleSortedMap,TJclSingleAbstractContainer,IJclSingleMap,IJclSingleSortedMap,IJclSingleSet,IJclCollection, IJclSingleContainer\, IJclValueOwner\,,
  private
    FOwnsValues: Boolean;
  protected
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Single): Single;
    function KeysCompare(const A\, B: Single): Integer;
    function ValuesCompare(A\, B: TObject): Integer;
  public
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const ,Single,,TObject)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclDoubleSortedEntry,Double,TObject)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclDoubleSortedEntry,TJclDoubleSortedMap,TJclDoubleAbstractContainer,IJclDoubleMap,IJclDoubleSortedMap,IJclDoubleSet,IJclCollection, IJclDoubleContainer\, IJclValueOwner\,,
  private
    FOwnsValues: Boolean;
  protected
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Double): Double;
    function KeysCompare(const A\, B: Double): Integer;
    function ValuesCompare(A\, B: TObject): Integer;
  public
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const ,Double,,TObject)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclExtendedSortedEntry,Extended,TObject)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclExtendedSortedEntry,TJclExtendedSortedMap,TJclExtendedAbstractContainer,IJclExtendedMap,IJclExtendedSortedMap,IJclExtendedSet,IJclCollection, IJclExtendedContainer\, IJclValueOwner\,,
  private
    FOwnsValues: Boolean;
  protected
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Extended): Extended;
    function KeysCompare(const A\, B: Extended): Integer;
    function ValuesCompare(A\, B: TObject): Integer;
  public
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const ,Extended,,TObject)*)

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatSortedMap = TJclExtendedSortedMap;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatSortedMap = TJclDoubleSortedMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatSortedMap = TJclSingleSortedMap;
  {$ENDIF MATH_SINGLE_PRECISION}

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclIntegerSortedEntry,Integer,TObject)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntegerSortedEntry,TJclIntegerSortedMap,TJclIntegerAbstractContainer,IJclIntegerMap,IJclIntegerSortedMap,IJclIntegerSet,IJclCollection, IJclValueOwner\,,
  private
    FOwnsValues: Boolean;
  protected
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Integer): Integer;
    function KeysCompare(A\, B: Integer): Integer;
    function ValuesCompare(A\, B: TObject): Integer;
  public
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,,Integer,,TObject)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclCardinalSortedEntry,Cardinal,TObject)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclCardinalSortedEntry,TJclCardinalSortedMap,TJclCardinalAbstractContainer,IJclCardinalMap,IJclCardinalSortedMap,IJclCardinalSet,IJclCollection, IJclValueOwner\,,
  private
    FOwnsValues: Boolean;
  protected
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Cardinal): Cardinal;
    function KeysCompare(A\, B: Cardinal): Integer;
    function ValuesCompare(A\, B: TObject): Integer;
  public
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,,Cardinal,,TObject)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclInt64SortedEntry,Int64,TObject)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclInt64SortedEntry,TJclInt64SortedMap,TJclInt64AbstractContainer,IJclInt64Map,IJclInt64SortedMap,IJclInt64Set,IJclCollection, IJclValueOwner\,,
  private
    FOwnsValues: Boolean;
  protected
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Int64): Int64;
    function KeysCompare(const A\, B: Int64): Integer;
    function ValuesCompare(A\, B: TObject): Integer;
  public
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const ,Int64,,TObject)*)

  {$IFNDEF CLR}
(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclPtrSortedEntry,Pointer,TObject)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclPtrSortedEntry,TJclPtrSortedMap,TJclPtrAbstractContainer,IJclPtrMap,IJclPtrSortedMap,IJclPtrSet,IJclCollection, IJclValueOwner\,,
  private
    FOwnsValues: Boolean;
  protected
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Pointer): Pointer;
    function KeysCompare(A\, B: Pointer): Integer;
    function ValuesCompare(A\, B: TObject): Integer;
  public
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,,Pointer,,TObject)*)
  {$ENDIF ~CLR}

(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclSortedEntry,TObject,TObject)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclSortedEntry,TJclSortedMap,TJclAbstractContainerBase,IJclMap,IJclSortedMap,IJclSet,IJclCollection, IJclKeyOwner\, IJclValueOwner\,,
  private
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
  protected
    { IJclKeyOwner }
    function FreeKey(var Key: TObject): TObject;
    function GetOwnsKeys: Boolean;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function KeysCompare(A\, B: TObject): Integer;
    function ValuesCompare(A\, B: TObject): Integer;
  public
    property OwnsKeys: Boolean read FOwnsKeys;
    property OwnsValues: Boolean read FOwnsValues;,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,,TObject,,TObject)*)

  {$IFDEF SUPPORTS_GENERICS}
(*$JPPEXPANDMACRO JCLSORTEDMAPTYPESINT(TJclSortedEntry<TKey\,TValue>,TKey,TValue)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TSortedEntry,TJclSortedMap<TKey\,TValue>,TJclAbstractContainerBase,IJclMap<TKey\,TValue>,IJclSortedMap<TKey\,TValue>,IJclSet<TKey>,IJclCollection<TValue>, IJclPairOwner<TKey\,TValue>\,,
  protected
    type
      TSortedEntry = TJclSortedEntry<TKey\,TValue>;
  private
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
  protected
    { IJclPairOwner }
    function FreeKey(var Key: TKey): TKey;
    function FreeValue(var Value: TValue): TValue;
    function GetOwnsKeys: Boolean;
    function GetOwnsValues: Boolean;
    function KeysCompare(const A\, B: TKey): Integer; virtual; abstract;
    function ValuesCompare(const A\, B: TValue): Integer; virtual; abstract;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; virtual; abstract;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; virtual; abstract;
  public
    property OwnsKeys: Boolean read FOwnsKeys;
    property OwnsValues: Boolean read FOwnsValues;,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,const ,TKey,const ,TValue)*)

  // E = external helper to compare items
  TJclSortedMapE<TKey, TValue> = class(TJclSortedMap<TKey,TValue>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclMap<TKey,TValue>, IJclSortedMap<TKey,TValue>, IJclPairOwner<TKey,TValue>)
  protected
    type
      TArrayList = TJclArrayListE<TValue>;
      TArraySet = TJclArraySetE<TKey>;
  private
    FKeyComparer: IJclComparer<TKey>;
    FValueComparer: IJclComparer<TValue>;
    FValueEqualityComparer: IJclEqualityComparer<TValue>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function KeysCompare(const A, B: TKey): Integer; override;
    function ValuesCompare(const A, B: TValue): Integer; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
  public
    constructor Create(const AKeyComparer: IJclComparer<TKey>; const AValueComparer: IJclComparer<TValue>;
      const AValueEqualityComparer: IJclEqualityComparer<TValue>; ACapacity: Integer; AOwnsValues: Boolean;
      AOwnsKeys: Boolean);

    property KeyComparer: IJclComparer<TKey> read FKeyComparer write FKeyComparer;
    property ValueComparer: IJclComparer<TValue> read FValueComparer write FValueComparer;
    property ValueEqualityComparer: IJclEqualityComparer<TValue> read FValueEqualityComparer write FValueEqualityComparer;
  end;

  // F = Functions to compare items
  TJclSortedMapF<TKey, TValue> = class(TJclSortedMap<TKey, TValue>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclMap<TKey,TValue>, IJclSortedMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  protected
    type
      TArrayList = TJclArrayListF<TValue>;
      TArraySet = TJclArraySetF<TKey>;
  private
    FKeyCompare: TCompare<TKey>;
    FValueCompare: TCompare<TValue>;
    FValueEqualityCompare: TEqualityCompare<TValue>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function KeysCompare(const A, B: TKey): Integer; override;
    function ValuesCompare(const A, B: TValue): Integer; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
  public
    constructor Create(AKeyCompare: TCompare<TKey>; AValueCompare: TCompare<TValue>;
      AValueEqualityCompare: TEqualityCompare<TValue>; ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);

    property KeyCompare: TCompare<TKey> read FKeyCompare write FKeyCompare;
    property ValueCompare: TCompare<TValue> read FValueCompare write FValueCompare;
    property ValueEqualityCompare: TEqualityCompare<TValue> read FValueEqualityCompare write FValueEqualityCompare;
  end;

  // I = items can compare themselves to an other
  TJclSortedMapI<TKey: IComparable<TKey>; TValue: IComparable<TValue>, IEquatable<TValue>> = class(TJclSortedMap<TKey, TValue>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer,
    IJclMap<TKey,TValue>, IJclSortedMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  protected
    type
      TArrayList = TJclArrayListI<TValue>;
      TArraySet = TJclArraySetI<TKey>;
  protected
    function KeysCompare(const A, B: TKey): Integer; override;
    function ValuesCompare(const A, B: TValue): Integer; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
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

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfIntfSortedMap,TJclIntfIntfSortedEntry,IJclIntfIntfMap,IJclIntfIntfSortedMap,IJclIntfSet,IJclIntfIterator,IJclIntfCollection,,,,const ,IInterface,nil,const ,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclIntfIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfIntfSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclIntfIntfSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclIntfIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  Result := ItemsCompare(A, B);
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclAnsiStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclAnsiStrIntfSortedMap,TJclAnsiStrIntfSortedEntry,IJclAnsiStrIntfMap,IJclAnsiStrIntfSortedMap,IJclAnsiStrSet,IJclAnsiStrIterator,IJclIntfCollection,,,,const ,AnsiString,'',const ,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclAnsiStrIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrIntfSortedMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;

function TJclAnsiStrIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclAnsiStrIntfSortedMap.KeysCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclAnsiStrIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclAnsiStrArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfAnsiStrSortedMap,TJclIntfAnsiStrSortedEntry,IJclIntfAnsiStrMap,IJclIntfAnsiStrSortedMap,IJclIntfSet,IJclIntfIterator,IJclAnsiStrCollection,,,,const ,IInterface,nil,const ,AnsiString,'')}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclIntfAnsiStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfAnsiStrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfAnsiStrSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfAnsiStrSortedMap.FreeValue(var Value: AnsiString): AnsiString;
begin
  Result := Value;
  Value := '';
end;

function TJclIntfAnsiStrSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclIntfAnsiStrSortedMap.ValuesCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclAnsiStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclAnsiStrArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclAnsiStrAnsiStrSortedMap,TJclAnsiStrAnsiStrSortedEntry,IJclAnsiStrAnsiStrMap,IJclAnsiStrAnsiStrSortedMap,IJclAnsiStrSet,IJclAnsiStrIterator,IJclAnsiStrCollection,,,,const ,AnsiString,'',const ,AnsiString,'')}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclAnsiStrAnsiStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrAnsiStrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrAnsiStrSortedMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;

function TJclAnsiStrAnsiStrSortedMap.FreeValue(var Value: AnsiString): AnsiString;
begin
  Result := Value;
  Value := '';
end;

function TJclAnsiStrAnsiStrSortedMap.KeysCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclAnsiStrAnsiStrSortedMap.ValuesCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclWideStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclWideStrIntfSortedMap,TJclWideStrIntfSortedEntry,IJclWideStrIntfMap,IJclWideStrIntfSortedMap,IJclWideStrSet,IJclWideStrIterator,IJclIntfCollection,,,,const ,WideString,'',const ,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclWideStrIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclWideStrIntfSortedMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;

function TJclWideStrIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclWideStrIntfSortedMap.KeysCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclWideStrIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclWideStrArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfWideStrSortedMap,TJclIntfWideStrSortedEntry,IJclIntfWideStrMap,IJclIntfWideStrSortedMap,IJclIntfSet,IJclIntfIterator,IJclWideStrCollection,,,,const ,IInterface,nil,const ,WideString,'')}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclIntfWideStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfWideStrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfWideStrSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfWideStrSortedMap.FreeValue(var Value: WideString): WideString;
begin
  Result := Value;
  Value := '';
end;

function TJclIntfWideStrSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclIntfWideStrSortedMap.ValuesCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclWideStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclWideStrArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclWideStrWideStrSortedMap,TJclWideStrWideStrSortedEntry,IJclWideStrWideStrMap,IJclWideStrWideStrSortedMap,IJclWideStrSet,IJclWideStrIterator,IJclWideStrCollection,,,,const ,WideString,'',const ,WideString,'')}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclWideStrWideStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrWideStrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclWideStrWideStrSortedMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;

function TJclWideStrWideStrSortedMap.FreeValue(var Value: WideString): WideString;
begin
  Result := Value;
  Value := '';
end;

function TJclWideStrWideStrSortedMap.KeysCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclWideStrWideStrSortedMap.ValuesCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclUnicodeStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclUnicodeStrIntfSortedMap,TJclUnicodeStrIntfSortedEntry,IJclUnicodeStrIntfMap,IJclUnicodeStrIntfSortedMap,IJclUnicodeStrSet,IJclUnicodeStrIterator,IJclIntfCollection,,,,const ,UnicodeString,'',const ,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclUnicodeStrIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclUnicodeStrIntfSortedMap.FreeKey(var Key: UnicodeString): UnicodeString;
begin
  Result := Key;
  Key := '';
end;

function TJclUnicodeStrIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclUnicodeStrIntfSortedMap.KeysCompare(const A, B: UnicodeString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclUnicodeStrIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclUnicodeStrArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfUnicodeStrSortedMap,TJclIntfUnicodeStrSortedEntry,IJclIntfUnicodeStrMap,IJclIntfUnicodeStrSortedMap,IJclIntfSet,IJclIntfIterator,IJclUnicodeStrCollection,,,,const ,IInterface,nil,const ,UnicodeString,'')}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclIntfUnicodeStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfUnicodeStrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfUnicodeStrSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfUnicodeStrSortedMap.FreeValue(var Value: UnicodeString): UnicodeString;
begin
  Result := Value;
  Value := '';
end;

function TJclIntfUnicodeStrSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclIntfUnicodeStrSortedMap.ValuesCompare(const A, B: UnicodeString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclUnicodeStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclUnicodeStrArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclUnicodeStrUnicodeStrSortedMap,TJclUnicodeStrUnicodeStrSortedEntry,IJclUnicodeStrUnicodeStrMap,IJclUnicodeStrUnicodeStrSortedMap,IJclUnicodeStrSet,IJclUnicodeStrIterator,IJclUnicodeStrCollection,,,,const ,UnicodeString,'',const ,UnicodeString,'')}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclUnicodeStrUnicodeStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrUnicodeStrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclUnicodeStrUnicodeStrSortedMap.FreeKey(var Key: UnicodeString): UnicodeString;
begin
  Result := Key;
  Key := '';
end;

function TJclUnicodeStrUnicodeStrSortedMap.FreeValue(var Value: UnicodeString): UnicodeString;
begin
  Result := Value;
  Value := '';
end;

function TJclUnicodeStrUnicodeStrSortedMap.KeysCompare(const A, B: UnicodeString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclUnicodeStrUnicodeStrSortedMap.ValuesCompare(const A, B: UnicodeString): Integer;
begin
  Result := ItemsCompare(A, B);
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclSingleArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclSingleIntfSortedMap,TJclSingleIntfSortedEntry,IJclSingleIntfMap,IJclSingleIntfSortedMap,IJclSingleSet,IJclSingleIterator,IJclIntfCollection,,,,const ,Single,0.0,const ,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclSingleIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclSingleIntfSortedMap.FreeKey(var Key: Single): Single;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclSingleIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclSingleIntfSortedMap.KeysCompare(const A, B: Single): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclSingleIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclSingleArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfSingleSortedMap,TJclIntfSingleSortedEntry,IJclIntfSingleMap,IJclIntfSingleSortedMap,IJclIntfSet,IJclIntfIterator,IJclSingleCollection,,,,const ,IInterface,nil,const ,Single,0.0)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclIntfSingleSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfSingleSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfSingleSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfSingleSortedMap.FreeValue(var Value: Single): Single;
begin
  Result := Value;
  Value := 0.0;
end;

function TJclIntfSingleSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclIntfSingleSortedMap.ValuesCompare(const A, B: Single): Integer;
begin
  Result := ItemsCompare(A, B);
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclSingleArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclSingleArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclSingleSingleSortedMap,TJclSingleSingleSortedEntry,IJclSingleSingleMap,IJclSingleSingleSortedMap,IJclSingleSet,IJclSingleIterator,IJclSingleCollection,,,,const ,Single,0.0,const ,Single,0.0)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclSingleSingleSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleSingleSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclSingleSingleSortedMap.FreeKey(var Key: Single): Single;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclSingleSingleSortedMap.FreeValue(var Value: Single): Single;
begin
  Result := Value;
  Value := 0.0;
end;

function TJclSingleSingleSortedMap.KeysCompare(const A, B: Single): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclSingleSingleSortedMap.ValuesCompare(const A, B: Single): Integer;
begin
  Result := ItemsCompare(A, B);
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclDoubleArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclDoubleIntfSortedMap,TJclDoubleIntfSortedEntry,IJclDoubleIntfMap,IJclDoubleIntfSortedMap,IJclDoubleSet,IJclDoubleIterator,IJclIntfCollection,,,,const ,Double,0.0,const ,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclDoubleIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclDoubleIntfSortedMap.FreeKey(var Key: Double): Double;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclDoubleIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclDoubleIntfSortedMap.KeysCompare(const A, B: Double): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclDoubleIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclDoubleArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfDoubleSortedMap,TJclIntfDoubleSortedEntry,IJclIntfDoubleMap,IJclIntfDoubleSortedMap,IJclIntfSet,IJclIntfIterator,IJclDoubleCollection,,,,const ,IInterface,nil,const ,Double,0.0)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclIntfDoubleSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfDoubleSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfDoubleSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfDoubleSortedMap.FreeValue(var Value: Double): Double;
begin
  Result := Value;
  Value := 0.0;
end;

function TJclIntfDoubleSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclIntfDoubleSortedMap.ValuesCompare(const A, B: Double): Integer;
begin
  Result := ItemsCompare(A, B);
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclDoubleArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclDoubleArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclDoubleDoubleSortedMap,TJclDoubleDoubleSortedEntry,IJclDoubleDoubleMap,IJclDoubleDoubleSortedMap,IJclDoubleSet,IJclDoubleIterator,IJclDoubleCollection,,,,const ,Double,0.0,const ,Double,0.0)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclDoubleDoubleSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleDoubleSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclDoubleDoubleSortedMap.FreeKey(var Key: Double): Double;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclDoubleDoubleSortedMap.FreeValue(var Value: Double): Double;
begin
  Result := Value;
  Value := 0.0;
end;

function TJclDoubleDoubleSortedMap.KeysCompare(const A, B: Double): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclDoubleDoubleSortedMap.ValuesCompare(const A, B: Double): Integer;
begin
  Result := ItemsCompare(A, B);
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclExtendedArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclExtendedIntfSortedMap,TJclExtendedIntfSortedEntry,IJclExtendedIntfMap,IJclExtendedIntfSortedMap,IJclExtendedSet,IJclExtendedIterator,IJclIntfCollection,,,,const ,Extended,0.0,const ,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclExtendedIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclExtendedIntfSortedMap.FreeKey(var Key: Extended): Extended;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclExtendedIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclExtendedIntfSortedMap.KeysCompare(const A, B: Extended): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclExtendedIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclExtendedArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfExtendedSortedMap,TJclIntfExtendedSortedEntry,IJclIntfExtendedMap,IJclIntfExtendedSortedMap,IJclIntfSet,IJclIntfIterator,IJclExtendedCollection,,,,const ,IInterface,nil,const ,Extended,0.0)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclIntfExtendedSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfExtendedSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfExtendedSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfExtendedSortedMap.FreeValue(var Value: Extended): Extended;
begin
  Result := Value;
  Value := 0.0;
end;

function TJclIntfExtendedSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclIntfExtendedSortedMap.ValuesCompare(const A, B: Extended): Integer;
begin
  Result := ItemsCompare(A, B);
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclExtendedArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclExtendedArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclExtendedExtendedSortedMap,TJclExtendedExtendedSortedEntry,IJclExtendedExtendedMap,IJclExtendedExtendedSortedMap,IJclExtendedSet,IJclExtendedIterator,IJclExtendedCollection,,,,const ,Extended,0.0,const ,Extended,0.0)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclExtendedExtendedSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedExtendedSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclExtendedExtendedSortedMap.FreeKey(var Key: Extended): Extended;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclExtendedExtendedSortedMap.FreeValue(var Value: Extended): Extended;
begin
  Result := Value;
  Value := 0.0;
end;

function TJclExtendedExtendedSortedMap.KeysCompare(const A, B: Extended): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclExtendedExtendedSortedMap.ValuesCompare(const A, B: Extended): Integer;
begin
  Result := ItemsCompare(A, B);
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntegerArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntegerIntfSortedMap,TJclIntegerIntfSortedEntry,IJclIntegerIntfMap,IJclIntegerIntfSortedMap,IJclIntegerSet,IJclIntegerIterator,IJclIntfCollection,,,,,Integer,0,const ,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclIntegerIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntegerIntfSortedMap.FreeKey(var Key: Integer): Integer;
begin
  Result := Key;
  Key := 0;
end;

function TJclIntegerIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclIntegerIntfSortedMap.KeysCompare(A, B: Integer): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclIntegerIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntegerArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfIntegerSortedMap,TJclIntfIntegerSortedEntry,IJclIntfIntegerMap,IJclIntfIntegerSortedMap,IJclIntfSet,IJclIntfIterator,IJclIntegerCollection,,,,const ,IInterface,nil,,Integer,0)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclIntfIntegerSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfIntegerSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfIntegerSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfIntegerSortedMap.FreeValue(var Value: Integer): Integer;
begin
  Result := Value;
  Value := 0;
end;

function TJclIntfIntegerSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclIntfIntegerSortedMap.ValuesCompare(A, B: Integer): Integer;
begin
  Result := ItemsCompare(A, B);
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntegerArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntegerArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntegerIntegerSortedMap,TJclIntegerIntegerSortedEntry,IJclIntegerIntegerMap,IJclIntegerIntegerSortedMap,IJclIntegerSet,IJclIntegerIterator,IJclIntegerCollection,,,,,Integer,0,,Integer,0)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclIntegerIntegerSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerIntegerSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntegerIntegerSortedMap.FreeKey(var Key: Integer): Integer;
begin
  Result := Key;
  Key := 0;
end;

function TJclIntegerIntegerSortedMap.FreeValue(var Value: Integer): Integer;
begin
  Result := Value;
  Value := 0;
end;

function TJclIntegerIntegerSortedMap.KeysCompare(A, B: Integer): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclIntegerIntegerSortedMap.ValuesCompare(A, B: Integer): Integer;
begin
  Result := ItemsCompare(A, B);
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclCardinalArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclCardinalIntfSortedMap,TJclCardinalIntfSortedEntry,IJclCardinalIntfMap,IJclCardinalIntfSortedMap,IJclCardinalSet,IJclCardinalIterator,IJclIntfCollection,,,,,Cardinal,0,const ,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclCardinalIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclCardinalIntfSortedMap.FreeKey(var Key: Cardinal): Cardinal;
begin
  Result := Key;
  Key := 0;
end;

function TJclCardinalIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclCardinalIntfSortedMap.KeysCompare(A, B: Cardinal): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclCardinalIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclCardinalArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfCardinalSortedMap,TJclIntfCardinalSortedEntry,IJclIntfCardinalMap,IJclIntfCardinalSortedMap,IJclIntfSet,IJclIntfIterator,IJclCardinalCollection,,,,const ,IInterface,nil,,Cardinal,0)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclIntfCardinalSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfCardinalSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfCardinalSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfCardinalSortedMap.FreeValue(var Value: Cardinal): Cardinal;
begin
  Result := Value;
  Value := 0;
end;

function TJclIntfCardinalSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclIntfCardinalSortedMap.ValuesCompare(A, B: Cardinal): Integer;
begin
  Result := ItemsCompare(A, B);
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclCardinalArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclCardinalArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclCardinalCardinalSortedMap,TJclCardinalCardinalSortedEntry,IJclCardinalCardinalMap,IJclCardinalCardinalSortedMap,IJclCardinalSet,IJclCardinalIterator,IJclCardinalCollection,,,,,Cardinal,0,,Cardinal,0)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclCardinalCardinalSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalCardinalSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclCardinalCardinalSortedMap.FreeKey(var Key: Cardinal): Cardinal;
begin
  Result := Key;
  Key := 0;
end;

function TJclCardinalCardinalSortedMap.FreeValue(var Value: Cardinal): Cardinal;
begin
  Result := Value;
  Value := 0;
end;

function TJclCardinalCardinalSortedMap.KeysCompare(A, B: Cardinal): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclCardinalCardinalSortedMap.ValuesCompare(A, B: Cardinal): Integer;
begin
  Result := ItemsCompare(A, B);
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclInt64ArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclInt64IntfSortedMap,TJclInt64IntfSortedEntry,IJclInt64IntfMap,IJclInt64IntfSortedMap,IJclInt64Set,IJclInt64Iterator,IJclIntfCollection,,,,const ,Int64,0,const ,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclInt64IntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64IntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclInt64IntfSortedMap.FreeKey(var Key: Int64): Int64;
begin
  Result := Key;
  Key := 0;
end;

function TJclInt64IntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclInt64IntfSortedMap.KeysCompare(const A, B: Int64): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclInt64IntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclInt64ArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfInt64SortedMap,TJclIntfInt64SortedEntry,IJclIntfInt64Map,IJclIntfInt64SortedMap,IJclIntfSet,IJclIntfIterator,IJclInt64Collection,,,,const ,IInterface,nil,const ,Int64,0)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclIntfInt64SortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfInt64SortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfInt64SortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfInt64SortedMap.FreeValue(var Value: Int64): Int64;
begin
  Result := Value;
  Value := 0;
end;

function TJclIntfInt64SortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclIntfInt64SortedMap.ValuesCompare(const A, B: Int64): Integer;
begin
  Result := ItemsCompare(A, B);
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclInt64ArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclInt64ArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclInt64Int64SortedMap,TJclInt64Int64SortedEntry,IJclInt64Int64Map,IJclInt64Int64SortedMap,IJclInt64Set,IJclInt64Iterator,IJclInt64Collection,,,,const ,Int64,0,const ,Int64,0)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclInt64Int64SortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64Int64SortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclInt64Int64SortedMap.FreeKey(var Key: Int64): Int64;
begin
  Result := Key;
  Key := 0;
end;

function TJclInt64Int64SortedMap.FreeValue(var Value: Int64): Int64;
begin
  Result := Value;
  Value := 0;
end;

function TJclInt64Int64SortedMap.KeysCompare(const A, B: Int64): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclInt64Int64SortedMap.ValuesCompare(const A, B: Int64): Integer;
begin
  Result := ItemsCompare(A, B);
end;

{$IFNDEF CLR}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclPtrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclPtrIntfSortedMap,TJclPtrIntfSortedEntry,IJclPtrIntfMap,IJclPtrIntfSortedMap,IJclPtrSet,IJclPtrIterator,IJclIntfCollection,,,,,Pointer,nil,const ,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclPtrIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclPtrIntfSortedMap.FreeKey(var Key: Pointer): Pointer;
begin
  Result := Key;
  Key := nil;
end;

function TJclPtrIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclPtrIntfSortedMap.KeysCompare(A, B: Pointer): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclPtrIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclPtrArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfPtrSortedMap,TJclIntfPtrSortedEntry,IJclIntfPtrMap,IJclIntfPtrSortedMap,IJclIntfSet,IJclIntfIterator,IJclPtrCollection,,,,const ,IInterface,nil,,Pointer,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclIntfPtrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfPtrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfPtrSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfPtrSortedMap.FreeValue(var Value: Pointer): Pointer;
begin
  Result := Value;
  Value := nil;
end;

function TJclIntfPtrSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclIntfPtrSortedMap.ValuesCompare(A, B: Pointer): Integer;
begin
  Result := ItemsCompare(A, B);
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclPtrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclPtrArrayList.Create(Param)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclPtrPtrSortedMap,TJclPtrPtrSortedEntry,IJclPtrPtrMap,IJclPtrPtrSortedMap,IJclPtrSet,IJclPtrIterator,IJclPtrCollection,,,,,Pointer,nil,,Pointer,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclPtrPtrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrPtrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclPtrPtrSortedMap.FreeKey(var Key: Pointer): Pointer;
begin
  Result := Key;
  Key := nil;
end;

function TJclPtrPtrSortedMap.FreeValue(var Value: Pointer): Pointer;
begin
  Result := Value;
  Value := nil;
end;

function TJclPtrPtrSortedMap.KeysCompare(A, B: Pointer): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclPtrPtrSortedMap.ValuesCompare(A, B: Pointer): Integer;
begin
  Result := ItemsCompare(A, B);
end;
{$ENDIF ~CLR}

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfSortedMap,TJclIntfSortedEntry,IJclIntfMap,IJclIntfSortedMap,IJclIntfSet,IJclIntfIterator,IJclCollection,,; AOwnsValues: Boolean,
  FOwnsValues := AOwnsValues;,const ,IInterface,nil,,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclIntfSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

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

function TJclIntfSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclIntfSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclIntfSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclAnsiStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclAnsiStrSortedMap,TJclAnsiStrSortedEntry,IJclAnsiStrMap,IJclAnsiStrSortedMap,IJclAnsiStrSet,IJclAnsiStrIterator,IJclCollection,,; AOwnsValues: Boolean,
  FOwnsValues := AOwnsValues;,const ,AnsiString,'',,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclAnsiStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrSortedMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;

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

function TJclAnsiStrSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclAnsiStrSortedMap.KeysCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclAnsiStrSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclWideStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclWideStrSortedMap,TJclWideStrSortedEntry,IJclWideStrMap,IJclWideStrSortedMap,IJclWideStrSet,IJclWideStrIterator,IJclCollection,,; AOwnsValues: Boolean,
  FOwnsValues := AOwnsValues;,const ,WideString,'',,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclWideStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclWideStrSortedMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;

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

function TJclWideStrSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclWideStrSortedMap.KeysCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclWideStrSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclUnicodeStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclUnicodeStrSortedMap,TJclUnicodeStrSortedEntry,IJclUnicodeStrMap,IJclUnicodeStrSortedMap,IJclUnicodeStrSet,IJclUnicodeStrIterator,IJclCollection,,; AOwnsValues: Boolean,
  FOwnsValues := AOwnsValues;,const ,UnicodeString,'',,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclUnicodeStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclUnicodeStrSortedMap.FreeKey(var Key: UnicodeString): UnicodeString;
begin
  Result := Key;
  Key := '';
end;

function TJclUnicodeStrSortedMap.FreeValue(var Value: TObject): TObject;
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

function TJclUnicodeStrSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclUnicodeStrSortedMap.KeysCompare(const A, B: UnicodeString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclUnicodeStrSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclSingleArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclSingleSortedMap,TJclSingleSortedEntry,IJclSingleMap,IJclSingleSortedMap,IJclSingleSet,IJclSingleIterator,IJclCollection,,; AOwnsValues: Boolean,
  FOwnsValues := AOwnsValues;,const ,Single,0.0,,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclSingleSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclSingleSortedMap.FreeKey(var Key: Single): Single;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclSingleSortedMap.FreeValue(var Value: TObject): TObject;
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

function TJclSingleSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclSingleSortedMap.KeysCompare(const A, B: Single): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclSingleSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclDoubleArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclDoubleSortedMap,TJclDoubleSortedEntry,IJclDoubleMap,IJclDoubleSortedMap,IJclDoubleSet,IJclDoubleIterator,IJclCollection,,; AOwnsValues: Boolean,
  FOwnsValues := AOwnsValues;,const ,Double,0.0,,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclDoubleSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclDoubleSortedMap.FreeKey(var Key: Double): Double;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclDoubleSortedMap.FreeValue(var Value: TObject): TObject;
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

function TJclDoubleSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclDoubleSortedMap.KeysCompare(const A, B: Double): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclDoubleSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclExtendedArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclExtendedSortedMap,TJclExtendedSortedEntry,IJclExtendedMap,IJclExtendedSortedMap,IJclExtendedSet,IJclExtendedIterator,IJclCollection,,; AOwnsValues: Boolean,
  FOwnsValues := AOwnsValues;,const ,Extended,0.0,,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclExtendedSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclExtendedSortedMap.FreeKey(var Key: Extended): Extended;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclExtendedSortedMap.FreeValue(var Value: TObject): TObject;
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

function TJclExtendedSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclExtendedSortedMap.KeysCompare(const A, B: Extended): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclExtendedSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntegerArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntegerSortedMap,TJclIntegerSortedEntry,IJclIntegerMap,IJclIntegerSortedMap,IJclIntegerSet,IJclIntegerIterator,IJclCollection,,; AOwnsValues: Boolean,
  FOwnsValues := AOwnsValues;,,Integer,0,,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclIntegerSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclIntegerSortedMap.FreeKey(var Key: Integer): Integer;
begin
  Result := Key;
  Key := 0;
end;

function TJclIntegerSortedMap.FreeValue(var Value: TObject): TObject;
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

function TJclIntegerSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclIntegerSortedMap.KeysCompare(A, B: Integer): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclIntegerSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclCardinalArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclCardinalSortedMap,TJclCardinalSortedEntry,IJclCardinalMap,IJclCardinalSortedMap,IJclCardinalSet,IJclCardinalIterator,IJclCollection,,; AOwnsValues: Boolean,
  FOwnsValues := AOwnsValues;,,Cardinal,0,,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclCardinalSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclCardinalSortedMap.FreeKey(var Key: Cardinal): Cardinal;
begin
  Result := Key;
  Key := 0;
end;

function TJclCardinalSortedMap.FreeValue(var Value: TObject): TObject;
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

function TJclCardinalSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclCardinalSortedMap.KeysCompare(A, B: Cardinal): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclCardinalSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclInt64ArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclInt64SortedMap,TJclInt64SortedEntry,IJclInt64Map,IJclInt64SortedMap,IJclInt64Set,IJclInt64Iterator,IJclCollection,,; AOwnsValues: Boolean,
  FOwnsValues := AOwnsValues;,const ,Int64,0,,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclInt64SortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64SortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclInt64SortedMap.FreeKey(var Key: Int64): Int64;
begin
  Result := Key;
  Key := 0;
end;

function TJclInt64SortedMap.FreeValue(var Value: TObject): TObject;
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

function TJclInt64SortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclInt64SortedMap.KeysCompare(const A, B: Int64): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclInt64SortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

{$IFNDEF CLR}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclPtrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclPtrSortedMap,TJclPtrSortedEntry,IJclPtrMap,IJclPtrSortedMap,IJclPtrSet,IJclPtrIterator,IJclCollection,,; AOwnsValues: Boolean,
  FOwnsValues := AOwnsValues;,,Pointer,nil,,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclPtrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclPtrSortedMap.FreeKey(var Key: Pointer): Pointer;
begin
  Result := Key;
  Key := nil;
end;

function TJclPtrSortedMap.FreeValue(var Value: TObject): TObject;
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

function TJclPtrSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclPtrSortedMap.KeysCompare(A, B: Pointer): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclPtrSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;
{$ENDIF ~CLR}

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclArraySet.Create(Param, False)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclSortedMap,TJclSortedEntry,IJclMap,IJclSortedMap,IJclSet,IJclIterator,IJclCollection,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;,,TObject,nil,,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

function TJclSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSortedMap.Create(FSize, False, False);
  AssignPropertiesTo(Result);
end;

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

function TJclSortedMap.GetOWnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;

function TJclSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclSortedMap.KeysCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

{$IFDEF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)CreateEmptyArraySet(Param, False)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)CreateEmptyArrayList(Param, False)}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclSortedMap<TKey\,TValue>,TSortedEntry,IJclMap<TKey\,TValue>,IJclSortedMap<TKey\,TValue>,IJclSet<TKey>,IJclIterator<TKey>,IJclCollection<TValue>,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;,const ,TKey,Default(TKey),const ,TValue,Default(TValue))}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}

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

function TJclSortedMap<TKey,TValue>.GetOWnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;

function TJclSortedMap<TKey,TValue>.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

//=== { TJclSortedMapE<TKey, TValue> } =======================================

constructor TJclSortedMapE<TKey, TValue>.Create(const AKeyComparer: IJclComparer<TKey>;
  const AValueComparer: IJclComparer<TValue>; const AValueEqualityComparer: IJclEqualityComparer<TValue>; ACapacity: Integer;
  AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create(ACapacity, AOwnsValues, AOwnsKeys);
  FKeyComparer := AKeyComparer;
  FValueComparer := AValueComparer;
  FValueEqualityComparer := AValueEqualityComparer;
end;

procedure TJclSortedMapE<TKey, TValue>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclSortedMapE<TKey, TValue>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclSortedMapE<TKey, TValue> then
  begin
    ADest := TJclSortedMapE<TKey, TValue>(Dest);
    ADest.FKeyComparer := FKeyComparer;
    ADest.FValueComparer := FValueComparer;
  end;
end;

function TJclSortedMapE<TKey, TValue>.CreateEmptyArrayList(ACapacity: Integer;
  AOwnsObjects: Boolean): IJclCollection<TValue>;
begin
  if FValueEqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := TArrayList.Create(FValueEqualityComparer, ACapacity, AOwnsObjects);
end;

function TJclSortedMapE<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSortedMapE<TKey, TValue>.Create(FKeyComparer, FValueComparer, FValueEqualityComparer, FCapacity,
    FOwnsValues, FOwnsKeys);
  AssignPropertiesTo(Result);
end;

function TJclSortedMapE<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>;
begin
  Result := TArraySet.Create(FKeyComparer, FCapacity, AOwnsObjects);
end;

function TJclSortedMapE<TKey, TValue>.KeysCompare(const A, B: TKey): Integer;
begin
  if KeyComparer = nil then
    raise EJclNoComparerError.Create;
  Result := KeyComparer.Compare(A, B);
end;

function TJclSortedMapE<TKey, TValue>.ValuesCompare(const A, B: TValue): Integer;
begin
  if ValueComparer = nil then
    raise EJclNoComparerError.Create;
  Result := ValueComparer.Compare(A, B);
end;

//=== { TJclSortedMapF<TKey, TValue> } =======================================

constructor TJclSortedMapF<TKey, TValue>.Create(AKeyCompare: TCompare<TKey>; AValueCompare: TCompare<TValue>;
  AValueEqualityCompare: TEqualityCompare<TValue>; ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create(ACapacity, AOwnsValues, AOwnsKeys);
  FKeyCompare := AKeyCompare;
  FValueCompare := AValueCompare;
  FValueEqualityCompare := AValueEqualityCompare;
end;

procedure TJclSortedMapF<TKey, TValue>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclSortedMapF<TKey, TValue>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclSortedMapF<TKey, TValue> then
  begin
    ADest := TJclSortedMapF<TKey, TValue>(Dest);
    ADest.FKeyCompare := FKeyCompare;
    ADest.FValueCompare := FValueCompare;
  end;
end;

function TJclSortedMapF<TKey, TValue>.CreateEmptyArrayList(ACapacity: Integer;
  AOwnsObjects: Boolean): IJclCollection<TValue>;
begin
  if not Assigned(FValueEqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := TArrayList.Create(FValueEqualityCompare, ACapacity, AOwnsObjects);
end;

function TJclSortedMapF<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSortedMapF<TKey, TValue>.Create(FKeyCompare, FValueCompare, FValueEqualityCompare, FCapacity,
    FOwnsValues, FOwnsKeys);
  AssignPropertiesTo(Result);
end;

function TJclSortedMapF<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>;
begin
  Result := TArraySet.Create(FKeyCompare, FCapacity, AOwnsObjects);
end;

function TJclSortedMapF<TKey, TValue>.KeysCompare(const A, B: TKey): Integer;
begin
  if not Assigned(KeyCompare) then
    raise EJclNoComparerError.Create;
  Result := KeyCompare(A, B);
end;

function TJclSortedMapF<TKey, TValue>.ValuesCompare(const A, B: TValue): Integer;
begin
  if not Assigned(ValueCompare) then
    raise EJclNoComparerError.Create;
  Result := ValueCompare(A, B);
end;

//=== { TJclSortedMapI<TKey, TValue> } =======================================

function TJclSortedMapI<TKey, TValue>.CreateEmptyArrayList(ACapacity: Integer;
  AOwnsObjects: Boolean): IJclCollection<TValue>;
begin
  Result := TArrayList.Create(ACapacity, AOwnsObjects);
end;

function TJclSortedMapI<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSortedMapI<TKey, TValue>.Create(FCapacity, FOwnsValues, FOwnsKeys);
  AssignPropertiesTo(Result);
end;

function TJclSortedMapI<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>;
begin
  Result := TArraySet.Create(FCapacity, AOwnsObjects);
end;

function TJclSortedMapI<TKey, TValue>.KeysCompare(const A, B: TKey): Integer;
begin
  Result := A.CompareTo(B);
end;

function TJclSortedMapI<TKey, TValue>.ValuesCompare(const A, B: TValue): Integer;
begin
  Result := A.CompareTo(B);
end;

{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
