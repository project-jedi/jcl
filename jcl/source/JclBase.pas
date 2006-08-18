{******************************************************************************}
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JclBase.pas.                                            }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ 2000 of these individuals.                                                   }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This unit contains generic JCL base classes and routines to support earlier  }
{ versions of Delphi as well as FPC.                                           }
{                                                                              }
{ Unit owner: Marcel van Brakel                                                }
{ Last modified: July 19, 2001                                                 }
{                                                                              }
{******************************************************************************}

unit JclBase;

{$I JCL.INC}

interface

uses
  {$IFDEF WIN32}
  Windows,
  {$ENDIF WIN32}
  Classes, SysUtils;

//------------------------------------------------------------------------------
// Version
//------------------------------------------------------------------------------

const
  JclVersionMajor   = 1;    // 0=pre-release|beta/1, 2, ...=final
  JclVersionMinor   = 11;   // Forth minor release JCL 1.11
  JclVersionRelease = 1;    // 0=pre-release|beta/1=release
  JclVersionBuild   = 505;  // build number, days since march 1, 2000
  JclVersion = (JclVersionMajor shl 24) or (JclVersionMinor shl 16) or
               (JclVersionRelease shl 15) or (JclVersionBuild shl 0);

//------------------------------------------------------------------------------
// FreePascal Support
//------------------------------------------------------------------------------

{$IFDEF FPC}

type
  PResStringRec = ^string;

function SysErrorMessage(ErrNo: Integer): string;

{$ENDIF FPC}

//------------------------------------------------------------------------------
// EJclError
//------------------------------------------------------------------------------

type
  EJclError = class (Exception)
  public
    constructor CreateResRec(ResStringRec: PResStringRec);
    constructor CreateResRecFmt(ResStringRec: PResStringRec; const Args: array of const);
  end;

//------------------------------------------------------------------------------
// EJclWin32Error
//------------------------------------------------------------------------------

{$IFDEF WIN32}

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

{$ENDIF WIN32}

//------------------------------------------------------------------------------
// Types
//------------------------------------------------------------------------------

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

{$IFNDEF COMPILER4_UP}

{$IFDEF FPC}

type
  LongWord = Cardinal;
  TSysCharSet = set of Char;

{$ENDIF FPC}

{$ENDIF COMPILER4_UP}

type
  PPointer = ^Pointer;

{$IFNDEF SUPPORTS_INT64}

//------------------------------------------------------------------------------
// Int64 support
//------------------------------------------------------------------------------

type
  PInt64 = ^Int64;
  Int64 = packed record
    case Integer of
    0: (
      LowPart: DWORD;
      HighPart: Longint);
    {$IFNDEF BCB3}
    1: (
      QuadPart: LONGLONG);
    {$ENDIF BCB3}
  end;

procedure I64Assign(var I: Int64; const Low, High: Longint);
procedure I64Copy(var Dest: Int64; const Source: Int64);
function I64Compare(const I1, I2: Int64): Integer;

{$ENDIF SUPPORTS_INT64}

{$IFDEF SUPPORTS_INT64}

procedure I64ToCardinals(I: Int64; var LowPart, HighPart: Cardinal);
procedure CardinalsToI64(var I: Int64; const LowPart, HighPart: Cardinal);

{$ENDIF SUPPORTS_INT64}

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

//------------------------------------------------------------------------------
// Dynamic Array support
//------------------------------------------------------------------------------

{$IFDEF SUPPORTS_DYNAMICARRAYS}

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

{$ELSE}

const
  DynByteArrayHigh     = 2147483646; // 2^31 / SizeOf(Byte)
  DynShortArrayHigh    = 2147483646; // 2^31 / SizeOf(Short)
  DynSmallintArrayHigh = 1073741822; // 2^31 / SizeOf(Smallint)
  DynWordArrayHigh     = 1073741822; // 2^31 / SizeOf(Word)
  DynIntegerArrayHigh  = 536870908;  // 2^31 / SizeOf(Integer)
  DynLongintArrayHigh  = 536870908;  // 2^31 / SizeOf(Longint)
  DynCardinalArrayHigh = 536870908;  // 2^31 / SizeOf(Cardinal)
  DynInt64ArrayHigh    = 268435448;  // 2^31 / SizeOf(Int64)
  DynExtendedArrayHigh = 214748363;  // 2^31 / SizeOf(Extended)
  DynDoubleArrayHigh   = 268435448;  // 2^31 / SizeOf(Double)
  DynSingleArrayHigh   = 536870908;  // 2^31 / SizeOf(Single)
  {$IFDEF MATH_EXTENDED_PRECISION}
  DynFloatArrayHigh    = 214748363;  // 2^31 / SizeOf(Extended)
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  DynFloatArrayHigh    = 536870908;  // 2^31 / SizeOf(Double)
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  DynFloatArrayHigh    = 268435448;  // 2^31 / SizeOf(Single)
  {$ENDIF MATH_SINGLE_PRECISION}
  DynPointerArrayHigh  = 536870908;  // 2^31 / SizeOf(Pointer)

type
  PDynByteArray     = array [0..DynByteArrayHigh] of Byte;
  PDynShortintArray = array [0..DynShortArrayHigh] of Shortint;
  PDynSmallintArray = array [0..DynSmallintArrayHigh] of Smallint;
  PDynWordArray     = array [0..DynWordArrayHigh] of Word;
  PDynIntegerArray  = array [0..DynIntegerArrayHigh] of Integer;
  PDynLongintArray  = array [0..DynLongintArrayHigh] of Longint;
  PDynCardinalArray = array [0..DynCardinalArrayHigh] of Cardinal;
  PDynInt64Array    = array [0..DynInt64ArrayHigh] of Int64;
  PDynExtendedArray = array [0..DynExtendedArrayHigh] of Extended;
  PDynDoubleArray   = array [0..DynDoubleArrayHigh] of Double;
  PDynSingleArray   = array [0..DynSingleArrayHigh] of Single;
  PDynFloatArray    = array [0..DynFloatArrayHigh] of Float;
  PDynPointerArray  = array [0..DynPointerArrayHigh] of Pointer;

  TDynByteArray     = ^PDynByteArray;
  TDynShortintArray = ^PDynShortintArray;
  TDynSmallintArray = ^PDynSmallintArray;
  TDynWordArray     = ^PDynWordArray;
  TDynIntegerArray  = ^PDynIntegerArray;
  TDynLongintArray  = ^PDynLongintArray;
  TDynCardinalArray = ^PDynCardinalArray;
  TDynInt64Array    = ^PDynInt64Array;
  TDynExtendedArray = ^PDynExtendedArray;
  TDynDoubleArray   = ^PDynDoubleArray;
  TDynSingleArray   = ^PDynSingleArray;
  TDynFloatArray    = ^PDynFloatArray;
  TDynPointerArray  = ^PDynPointerArray;

function DynArrayAllocSize(const A): Longint;
function DynArrayLength(const A): Longint;
function DynArrayElemSize(const A): Longint;
procedure DynArrayInitialize(var A; ElementSize, InitialLength: Longint);
procedure DynArrayFinalize(var A);
procedure DynArraySetLength(var A; NewLength: Integer);

{$ENDIF SUPPORTS_DYNAMICARRAYS}

//------------------------------------------------------------------------------
// TObjectList
//------------------------------------------------------------------------------

{$IFNDEF DELPHI5_UP}

type
  TObjectList = class (TList)
  private
    FOwnsObjects: Boolean;
    function GetItems(Index: Integer): TObject;
    procedure SetItems(Index: Integer; const Value: TObject);
  public
    procedure Clear; {$IFNDEF FPC} override; {$ENDIF}
    constructor Create(AOwnsObjects: Boolean {$IFDEF SUPPORTS_DEFAULTPARAMS} = False {$ENDIF});
    property Items[Index: Integer]: TObject read GetItems write SetItems; default;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

{$ENDIF DELPHI5_UP}

//------------------------------------------------------------------------------
// Cross-Platform Compatibility
//------------------------------------------------------------------------------

{$IFNDEF DELPHI6_UP}
procedure RaiseLastOSError;
{$ENDIF}


implementation

uses
  JclResources;

//==============================================================================
// EJclError
//==============================================================================

constructor EJclError.CreateResRec(ResStringRec: PResStringRec);
begin
  {$IFDEF FPC}
  inherited Create(ResStringRec^);
  {$ELSE}
  inherited Create(LoadResString(ResStringRec));
  {$ENDIF FPC}
end;

constructor EJclError.CreateResRecFmt(ResStringRec: PResStringRec; const Args: array of const);
begin
  {$IFDEF FPC}
  inherited CreateFmt(ResStringRec^, Args);
  {$ELSE}
  inherited CreateFmt(LoadResString(ResStringRec), Args);
  {$ENDIF FPC}
end;

//==============================================================================
// FreePascal support
//==============================================================================

{$IFDEF FPC}
{$IFDEF WIN32}

function SysErrorMessage(ErrNo: Integer): string;
var
  Size: Integer;
  Buffer: array [0..1024] of Char;
begin
  Size := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ARGUMENT_ARRAY,
    nil, ErrNo, 0, Buffer, SizeOf(Buffer), nil);
  while (Size > 0) and (Buffer[Size - 1] in [#0..#32, '.']) do
    Dec(Size);
  SetString(Result, Buffer, Size);
end;

{$ELSE}

function SysErrorMessage(ErrNo: Integer): string;
begin
  Result := Format('Win32 Error %d (%x)', [ErrNo, ErrNo]);
end;

{$ENDIF WIN32}
{$ENDIF FPC}

//==============================================================================
// EJclWin32Error
//==============================================================================

{$IFDEF WIN32}

constructor EJclWin32Error.Create(const Msg: string);
begin
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  inherited CreateFmt(Msg + #13 + RsWin32Prefix, [FLastErrorMsg, FLastError]);
end;

//------------------------------------------------------------------------------

constructor EJclWin32Error.CreateFmt(const Msg: string; const Args: array of const);
begin
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  inherited CreateFmt(Msg + #13 + Format(RsWin32Prefix, [FLastErrorMsg, FLastError]), Args);
end;

//------------------------------------------------------------------------------

constructor EJclWin32Error.CreateRes(Ident: Integer);
begin
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  inherited CreateFmt(LoadStr(Ident) + #13 + RsWin32Prefix, [FLastErrorMsg, FLastError]);
end;

//------------------------------------------------------------------------------

constructor EJclWin32Error.CreateResRec(ResStringRec: PResStringRec);
begin
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  {$IFDEF FPC}
  inherited CreateFmt(ResStringRec^ + #13 + RsWin32Prefix, [FLastErrorMsg, FLastError]);
  {$ELSE}
  inherited CreateFmt(LoadResString(ResStringRec) + #13 + RsWin32Prefix, [FLastErrorMsg, FLastError]);
  {$ENDIF FPC}
end;

{$ENDIF WIN32}

//==============================================================================
// Dynamic array support
//==============================================================================

{$IFNDEF SUPPORTS_DYNAMICARRAYS}

type
  PDynArrayRec = ^TDynArrayRec;
  TDynArrayRec = packed record
    AllocSize: Longint;
    Length: Longint;
    ElemSize: Longint;
  end;

const
  DynArrayRecSize = SizeOf(TDynArrayRec);

//------------------------------------------------------------------------------

function DynArrayAllocSize(const A): Longint;
var
  P: Pointer;
begin
  P := Pointer(Longint(Pointer(A)) - 12);
  Result := Longint(P^);
end;

//------------------------------------------------------------------------------

function DynArrayLength(const A): Longint;
var
  P: Pointer;
begin
  P := Pointer(Longint(Pointer(A)) - 8);
  Result := Longint(P^);
end;

//------------------------------------------------------------------------------

function DynArrayElemSize(const A): Longint;
var
  P: Pointer;
begin
  P := Pointer(Longint(Pointer(A)) - 4);
  Result := Longint(P^);
end;

//------------------------------------------------------------------------------

procedure DynArrayInitialize(var A; ElementSize, InitialLength: Longint);
var
  P: Pointer;
  Size: Longint;
begin
  if (ElementSize < 1) or (ElementSize > 8) then
    raise EJclError.CreateResRec(@RsDynArrayError);
  if InitialLength < 0 then
    InitialLength := 0;
  Size := DynArrayRecSize + (InitialLength * ElementSize);
  P := AllocMem(Size);
  with TDynArrayRec(P^) do
  begin
    AllocSize := Size;
    Length := InitialLength;
    ElemSize := ElementSize;
  end;
  Pointer(A) := Pointer(Longint(P) + DynArrayRecSize);
end;

//------------------------------------------------------------------------------

procedure DynArrayFinalize(var A);
var
  P: Pointer;
begin
  P := Pointer(Longint(Pointer(A)) - DynArrayRecSize);
  FreeMem(P);
  Pointer(A) := nil;
end;

//------------------------------------------------------------------------------

procedure DynArraySetLength(var A; NewLength: Integer);
var
  P: Pointer;
  Size: Longint;
  ElemSize: Longint;
begin
  P := Pointer(Longint(Pointer(A)) - DynArrayRecSize);
  ElemSize := DynArrayElemSize(A);
  Size := DynArrayRecSize + (NewLength * ElemSize);
  ReallocMem(P, Size);
  with TDynArrayRec(P^) do
  begin
    AllocSize := Size;
    Length := NewLength;
  end;
  Pointer(A) := Pointer(Longint(P) + DynArrayRecSize);
end;

{$ENDIF SUPPORTS_DYNAMICARRAYS}

//==============================================================================
// Int64 support
//==============================================================================

{$IFNDEF SUPPORTS_INT64}

procedure I64Assign(var I: Int64; const Low, High: Longint);
begin
  I.LowPart := Low;
  I.HighPart := High;
end;

//------------------------------------------------------------------------------

procedure I64Copy(var Dest: Int64; const Source: Int64);
begin
  Dest.LowPart := Source.LowPart;
  Dest.HighPart := Source.HighPart;
end;

//------------------------------------------------------------------------------

function I64Compare(const I1, I2: Int64): Integer;
begin
  if I1.HighPart < I2.HighPart then
    Result := -1
  else
  if I1.HighPart > I1.HighPart then
    Result := 1
  else
  if I1.LowPart < I2.LowPart then
    Result := -1
  else
  if I1.LowPart > I2.LowPart then
    Result := 1
  else
    Result := 0;
end;

{$ENDIF SUPPORTS_INT64}

//------------------------------------------------------------------------------

{$IFDEF SUPPORTS_INT64}

procedure I64ToCardinals(I: Int64; var LowPart, HighPart: Cardinal);
begin
  LowPart := TULargeInteger(I).LowPart;
  HighPart := TULargeInteger(I).HighPart;
end;

//------------------------------------------------------------------------------

procedure CardinalsToI64(var I: Int64; const LowPart, HighPart: Cardinal);
begin
  TULargeInteger(I).LowPart := LowPart;
  TULargeInteger(I).HighPart := HighPart;
end;

{$ENDIF SUPPORTS_INT64}

//==============================================================================
// TObjectList
//==============================================================================

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

//------------------------------------------------------------------------------

constructor TObjectList.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

//------------------------------------------------------------------------------

function TObjectList.GetItems(Index: Integer): TObject;
begin
  Result := TObject(Get(Index));
end;

//------------------------------------------------------------------------------

procedure TObjectList.SetItems(Index: Integer; const Value: TObject);
begin
  Put(Index, Value);
end;
{$ENDIF DELPHI5_UP}


//==============================================================================
// Cross=Platform Compatibility
//==============================================================================

{$IFNDEF DELPHI6_UP}
procedure RaiseLastOSError;
begin
  RaiseLastWin32Error;
end;
{$ENDIF}


end.
