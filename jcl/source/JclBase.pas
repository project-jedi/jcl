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
{ The Original Code is JclBase.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains generic JCL base classes and routines to support earlier                      }
{ versions of Delphi as well as FPC.                                                               }
{                                                                                                  }
{ Unit owner: Marcel van Brakel                                                                    }
{ Last modified: March 15, 2002                                                                    }
{                                                                                                  }
{**************************************************************************************************}

unit JclBase;

{$I jcl.inc}

{$WEAKPACKAGEUNIT ON}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Classes, SysUtils;

//--------------------------------------------------------------------------------------------------
// Version
//--------------------------------------------------------------------------------------------------

const
  JclVersionMajor   = 1;    // 0=pre-release|beta/1, 2, ...=final
  JclVersionMinor   = 20;   // Forth minor release JCL 1.20
  JclVersionRelease = 1;    // 0=pre-release|beta/1=release
  JclVersionBuild   = 779;  // build number, days since march 1, 2000
  JclVersion = (JclVersionMajor shl 24) or (JclVersionMinor shl 16) or
               (JclVersionRelease shl 15) or (JclVersionBuild shl 0);

//--------------------------------------------------------------------------------------------------
// FreePascal Support
//--------------------------------------------------------------------------------------------------

{$IFDEF FPC}

type
  PResStringRec = ^string;

function SysErrorMessage(ErrNo: Integer): string;

{$IFDEF MSWINDOWS}
procedure RaiseLastWin32Error;

procedure QueryPerformanceCounter(var C: Int64);
function QueryPerformanceFrequency(var Frequency: Int64): Boolean;
{$ENDIF MSWINDOWS}

var
  Default8087CW: Word;
{$ENDIF FPC}

//--------------------------------------------------------------------------------------------------
// EJclError
//--------------------------------------------------------------------------------------------------

type
  EJclError = class (Exception)
  public
    constructor CreateResRec(ResStringRec: PResStringRec);
    constructor CreateResRecFmt(ResStringRec: PResStringRec; const Args: array of const);
  end;

//--------------------------------------------------------------------------------------------------
// EJclWin32Error
//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

type
  EJclWin32Error = class (EJclError)
  private
    FLastError: DWORD;
    FLastErrorMsg: string;
  public
    constructor Create(const Msg: string);
    constructor CreateFmt(const Msg: string; const Args: array of const);
    constructor CreateRes(Ident: Integer);
    constructor CreateResRec(ResStringRec: PResStringRec);
    property LastError: DWORD read FLastError;
    property LastErrorMsg: string read FLastErrorMsg;
  end;

{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------
// Types
//--------------------------------------------------------------------------------------------------

type
  {$IFDEF MATH_EXTENDED_PRECISION}
  Float = Extended;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  Float = Double;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  Float = Single;
  {$ENDIF MATH_SINGLE_PRECISION}

  PFloat = ^Float;

{$IFDEF FPC}
type
  LongWord = Cardinal;
  TSysCharSet = set of Char;
{$ENDIF FPC}

type
  PPointer = ^Pointer;

//--------------------------------------------------------------------------------------------------
// Int64 support
//--------------------------------------------------------------------------------------------------

procedure I64ToCardinals(I: Int64; var LowPart, HighPart: Cardinal);
procedure CardinalsToI64(var I: Int64; const LowPart, HighPart: Cardinal);

// Redefinition of TLargeInteger to relieve dependency on Windows.pas

type
  PLargeInteger = ^TLargeInteger;
  TLargeInteger = record
    case Integer of
    0: (
      LowPart: LongWord;
      HighPart: Longint);
    1: (
      QuadPart: Int64);
  end;

// Redefinition of TULargeInteger to relieve dependency on Windows.pas

type
  PULargeInteger = ^TULargeInteger;
  TULargeInteger = record
    case Integer of
    0: (
      LowPart: LongWord;
      HighPart: LongWord);
    1: (
      QuadPart: Int64);
  end;

//--------------------------------------------------------------------------------------------------
// Dynamic Array support
//--------------------------------------------------------------------------------------------------

type
  TDynByteArray     = array of Byte;
  TDynShortintArray = array of Shortint;
  TDynSmallintArray = array of Smallint;
  TDynWordArray     = array of Word;
  TDynIntegerArray  = array of Integer;
  TDynLongintArray  = array of Longint;
  TDynCardinalArray = array of Cardinal;
  TDynInt64Array    = array of Int64;
  TDynExtendedArray = array of Extended;
  TDynDoubleArray   = array of Double;
  TDynSingleArray   = array of Single;
  TDynFloatArray    = array of Float;
  TDynPointerArray  = array of Pointer;

//--------------------------------------------------------------------------------------------------
// TObjectList
//--------------------------------------------------------------------------------------------------

{$IFNDEF DELPHI5_UP}
type
  TObjectList = class (TList)
  private
    FOwnsObjects: Boolean;
    function GetItems(Index: Integer): TObject;
    procedure SetItems(Index: Integer; const Value: TObject);
  public
    procedure Clear; override;
    constructor Create(AOwnsObjects: Boolean = False);
    property Items[Index: Integer]: TObject read GetItems write SetItems; default;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;
{$ENDIF DELPHI5_UP}

//--------------------------------------------------------------------------------------------------
// Cross-Platform Compatibility
//--------------------------------------------------------------------------------------------------

{$IFNDEF DELPHI6_UP}
procedure RaiseLastOSError;
{$ENDIF DELPHI6_UP}

//--------------------------------------------------------------------------------------------------
// Interface compatibility
//--------------------------------------------------------------------------------------------------

{$IFDEF SUPPORTS_INTERFACE}
{$IFNDEF COMPILER6_UP}

type
  IInterface = IUnknown;

{$ENDIF COMPILER6_UP}
{$ENDIF SUPPORTS_INTERFACE}

//--------------------------------------------------------------------------------------------------
// TStringList.CustomSort compatibility
//--------------------------------------------------------------------------------------------------

{$IFDEF DELPHI4}

type
  TStringListCustomSortCompare = function(List: TStringList; Index1, Index2: Integer): Integer;

procedure StringListCustomSort(StringList: TStringList; SortFunc: TStringListCustomSortCompare);

{$ENDIF DELPHI4}

implementation

uses
  JclResources;

//==================================================================================================
// EJclError
//==================================================================================================

constructor EJclError.CreateResRec(ResStringRec: PResStringRec);
begin
  {$IFDEF FPC}
  inherited Create(ResStringRec^);
  {$ELSE FPC}
  inherited Create(LoadResString(ResStringRec));
  {$ENDIF FPC}
end;

constructor EJclError.CreateResRecFmt(ResStringRec: PResStringRec; const Args: array of const);
begin
  {$IFDEF FPC}
  inherited CreateFmt(ResStringRec^, Args);
  {$ELSE FPC}
  inherited CreateFmt(LoadResString(ResStringRec), Args);
  {$ENDIF FPC}
end;

//==================================================================================================
// FreePascal support
//==================================================================================================

{$IFDEF FPC}
{$IFDEF MSWINDOWS}

function SysErrorMessage(ErrNo: Integer): string;
var
  Size: Integer;
  Buffer: PChar;
begin
  GetMem(Buffer, 4000);
  Size := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ARGUMENT_ARRAY, nil, ErrNo,
    0, Buffer, 4000, nil);
  SetString(Result, Buffer, Size);
end;

//--------------------------------------------------------------------------------------------------

procedure RaiseLastWin32Error;
begin
end;

//--------------------------------------------------------------------------------------------------

function QueryPerformanceFrequency(var Frequency: Int64): Boolean;
var
  T: TLargeInteger;
begin
  Windows.QueryPerformanceFrequency(@T);
  CardinalsToI64(Frequency, T.LowPart, T.HighPart);
end;

//--------------------------------------------------------------------------------------------------

procedure QueryPerformanceCounter(var C: Int64);
var
  T: TLargeInteger;
begin
  Windows.QueryPerformanceCounter(@T);
  CardinalsToI64(C, T.LowPart, T.HighPart);
end;

{$ELSE MSWINDOWS}

function SysErrorMessage(ErrNo: Integer): string;
begin
  Result := Format(RsSysErrorMessageFmt, [ErrNo, ErrNo]);
end;

{$ENDIF MSWINDOWS}
{$ENDIF FPC}

//==================================================================================================
// EJclWin32Error
//==================================================================================================

{$IFDEF MSWINDOWS}

constructor EJclWin32Error.Create(const Msg: string);
begin
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  inherited CreateFmt(Msg + #13 + RsWin32Prefix, [FLastErrorMsg, FLastError]);
end;

//--------------------------------------------------------------------------------------------------

constructor EJclWin32Error.CreateFmt(const Msg: string; const Args: array of const);
begin
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  inherited CreateFmt(Msg + #13 + Format(RsWin32Prefix, [FLastErrorMsg, FLastError]), Args);
end;

//--------------------------------------------------------------------------------------------------

constructor EJclWin32Error.CreateRes(Ident: Integer);
begin
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  inherited CreateFmt(LoadStr(Ident) + #13 + RsWin32Prefix, [FLastErrorMsg, FLastError]);
end;

//--------------------------------------------------------------------------------------------------

constructor EJclWin32Error.CreateResRec(ResStringRec: PResStringRec);
begin
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  {$IFDEF FPC}
  inherited CreateFmt(ResStringRec^ + #13 + RsWin32Prefix, [FLastErrorMsg, FLastError]);
  {$ELSE FPC}
  inherited CreateFmt(LoadResString(ResStringRec) + #13 + RsWin32Prefix, [FLastErrorMsg, FLastError]);
  {$ENDIF FPC}
end;

{$ENDIF MSWINDOWS}

//==================================================================================================
// Int64 support
//==================================================================================================

procedure I64ToCardinals(I: Int64; var LowPart, HighPart: Cardinal);
begin
  LowPart := TULargeInteger(I).LowPart;
  HighPart := TULargeInteger(I).HighPart;
end;

//--------------------------------------------------------------------------------------------------

procedure CardinalsToI64(var I: Int64; const LowPart, HighPart: Cardinal);
begin
  TULargeInteger(I).LowPart := LowPart;
  TULargeInteger(I).HighPart := HighPart;
end;

//==================================================================================================
// TObjectList
//==================================================================================================

{$IFNDEF DELPHI5_UP}

procedure TObjectList.Clear;
var
  I: Integer;
begin
  if OwnsObjects then
    for I := 0 to Count - 1 do
      Items[I].Free;
  inherited;
end;

//--------------------------------------------------------------------------------------------------

constructor TObjectList.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

//--------------------------------------------------------------------------------------------------

function TObjectList.GetItems(Index: Integer): TObject;
begin
  Result := TObject(Get(Index));
end;

//--------------------------------------------------------------------------------------------------

procedure TObjectList.SetItems(Index: Integer; const Value: TObject);
begin
  Put(Index, Value);
end;

{$ENDIF DELPHI5_UP}

//==================================================================================================
// Cross=Platform Compatibility
//==================================================================================================

{$IFNDEF DELPHI6_UP}

procedure RaiseLastOSError;
begin
  RaiseLastWin32Error;
end;

{$ENDIF DELPHI6_UP}

//==================================================================================================
// TStringList.CustomSort compatibility
//==================================================================================================

{$IFDEF DELPHI4}

procedure StringListCustomSort(StringList: TStringList; SortFunc: TStringListCustomSortCompare);

  procedure QuickSort(L, R: Integer);
  var
    I, J, P: Integer;
  begin
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        while SortFunc(StringList, I, P) < 0 do
          Inc(I);
        while SortFunc(StringList, J, P) > 0 do
          Dec(J);
        if I <= J then
        begin
          StringList.Exchange(I, J);
          if P = I then
            P := J
          else
          if P = J then
            P := I;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

begin
  QuickSort(0, StringList.Count - 1);
end;

{$ENDIF DELPHI4}

end.
