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
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright Marcel van Brakel. All rights reserved.      }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Marcel van Brakel,                                                                             }
{   Peter Friese,                                                                                  }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Petr Vones (pvones)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains generic JCL base classes and routines to support earlier                      }
{ versions of Delphi as well as FPC.                                                               }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$

unit JclBase;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF CLR}
  System.Reflection,
  {$ELSE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$ENDIF CLR}
  SysUtils;

// Version
const
  JclVersionMajor   = 1;    // 0=pre-release|beta/1, 2, ...=final
  JclVersionMinor   = 98;   // Fifth minor release since JCL 1.90
  JclVersionRelease = 0;    // 0: pre-release|beta/>=1: release
  JclVersionBuild   = 2485; // build number, days since march 1, 2000
  JclVersion = (JclVersionMajor shl 24) or (JclVersionMinor shl 16) or
    (JclVersionRelease shl 15) or (JclVersionBuild shl 0);

// EJclError
type
  EJclError = class(Exception);
  {$IFDEF CLR}
  DWORD = LongWord;
  TIntegerSet = set of 0..SizeOf(Integer) * 8 - 1;
  {$ENDIF CLR}

// EJclWin32Error
{$IFDEF MSWINDOWS}
type
  EJclWin32Error = class(EJclError)
  private
    FLastError: DWORD;
    FLastErrorMsg: string;
  public
    constructor Create(const Msg: string);
    constructor CreateFmt(const Msg: string; const Args: array of const);
    {$IFNDEF CLR}
    constructor CreateRes(Ident: Integer); overload;
    constructor CreateRes(ResStringRec: PResStringRec); overload;
    {$ENDIF ~CLR}
    property LastError: DWORD read FLastError;
    property LastErrorMsg: string read FLastErrorMsg;
  end;
{$ENDIF MSWINDOWS}

// EJclInternalError
type
  EJclInternalError = class(EJclError);

// Types
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

type
  {$IFDEF FPC}
  Largeint = Int64;
  {$ELSE}
  PPointer = ^Pointer;
  {$IFDEF RTL140_UP}
  {$IFDEF CLR}
  PJclByteArray = TBytes;
  {$ELSE}
  PByte = System.PByte;
  Int8 = ShortInt;
  Int16 = Smallint;
  Int32 = Integer;
  UInt8 = Byte;
  UInt16 = Word;
  UInt32 = LongWord;
  {$ENDIF CLR}
  {$ELSE ~RTL140_UP}
  PBoolean = ^Boolean;
  PByte = Windows.PByte;
  {$ENDIF ~RTL140_UP}
  {$ENDIF FPC}
  PCardinal = ^Cardinal;
  {$IFNDEF COMPILER7_UP}
  UInt64 = Int64;
  {$ENDIF ~COMPILER7_UP}

// Interface compatibility
{$IFDEF SUPPORTS_INTERFACE}
{$IFNDEF FPC}
{$IFNDEF RTL140_UP}

type
  IInterface = IUnknown;

{$ENDIF ~RTL140_UP}
{$ENDIF ~FPC}
{$ENDIF SUPPORTS_INTERFACE}

// Int64 support
procedure I64ToCardinals(I: Int64; var LowPart, HighPart: Cardinal);
procedure CardinalsToI64(var I: Int64; const LowPart, HighPart: Cardinal);

// Redefinition of TLargeInteger to relieve dependency on Windows.pas

type
  PLargeInteger = ^TLargeInteger;
  TLargeInteger = Int64;

{$IFDEF CLR}
type
  TJclBytes = TBytes;
{$ELSE}
// Redefinition of PByteArray to avoid range check exceptions.
type
  TJclByteArray = array [0..MaxInt div SizeOf(Byte) - 1] of Byte;
  PJclByteArray = ^TJclByteArray;
  TJclBytes = Pointer; // under .NET System.pas: TBytes = array of Byte;

// Redefinition of TULargeInteger to relieve dependency on Windows.pas
type
  PULargeInteger = ^TULargeInteger;
  TULargeInteger = record
    case Integer of
    0:
     (LowPart: LongWord;
      HighPart: LongWord);
    1:
     (QuadPart: Int64);
  end;
{$ENDIF ~CLR}

// Dynamic Array support
type
  TDynByteArray       = array of Byte;
  TDynShortIntArray   = array of Shortint;
  TDynWordArray       = array of Word;
  TDynSmallIntArray   = array of Smallint;
  TDynLongIntArray    = array of Longint;
  TDynInt64Array      = array of Int64;
  TDynCardinalArray   = array of Cardinal;
  TDynIntegerArray    = array of Integer;
  TDynExtendedArray   = array of Extended;
  TDynDoubleArray     = array of Double;
  TDynSingleArray     = array of Single;
  TDynFloatArray      = array of Float;
  {$IFNDEF CLR}
  TDynPointerArray    = array of Pointer;
  {$ENDIF ~CLR}
  TDynStringArray     = array of string;
  TDynIInterfaceArray = array of IInterface;
  TDynObjectArray     = array of TObject;

// Cross-Platform Compatibility
const
  // (rom) too basic for JclStrings
  AnsiLineFeed       = AnsiChar(#10);
  AnsiCarriageReturn = AnsiChar(#13);
  AnsiCrLf           = AnsiString(#13#10);
  {$IFDEF MSWINDOWS}
  AnsiLineBreak = AnsiCrLf;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  AnsiLineBreak = AnsiLineFeed;
  {$ENDIF UNIX}

  AnsiSigns                  = ['-', '+'];
  AnsiUppercaseLetters       = ['A'..'Z'];
  AnsiLowercaseLetters       = ['a'..'z'];
  AnsiLetters                = ['A'..'Z', 'a'..'z'];
  AnsiDecDigits              = ['0'..'9'];
  AnsiOctDigits              = ['0'..'7'];
  AnsiHexDigits              = ['0'..'9', 'A'..'F', 'a'..'f'];
  AnsiValidIdentifierLetters = ['0'..'9', 'A'..'Z', 'a'..'z', '_'];

{$IFNDEF XPLATFORM_RTL}
procedure RaiseLastOSError;
{$ENDIF ~XPLATFORM_RTL}

procedure MoveArray(var List: TDynIInterfaceArray; FromIndex, ToIndex, Count: Integer); overload;
procedure MoveArray(var List: TDynStringArray; FromIndex, ToIndex, Count: Integer); overload;
procedure MoveArray(var List: TDynObjectArray; FromIndex, ToIndex, Count: Integer); overload;
procedure MoveArray(var List: TDynIntegerArray; FromIndex, ToIndex, Count: Integer); overload;
procedure MoveChar(const Source: string; FromIndex: Integer;
  var Dest: string; ToIndex, Count: Integer); overload; // Index: 0..n-1
{$IFDEF CLR}
function GetBytesEx(const Value): TBytes;
procedure SetBytesEx(var Value; Bytes: TBytes);
procedure SetIntegerSet(var DestSet: TIntegerSet; Value: UInt32); inline;

function ByteArrayStringLen(Data: TBytes): Integer;
function StringToByteArray(const S: string): TBytes;
function ByteArrayToString(const Data: TBytes; Count: Integer): string;
{$ENDIF CLR}

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
  JclResources;

procedure MoveArray(var List: TDynIInterfaceArray; FromIndex, ToIndex, Count: Integer); overload;
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I]
  else
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
{$ELSE}
begin
  Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
  { Keep reference counting working }
  if FromIndex < ToIndex then
    FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
  else
  if FromIndex > ToIndex then
    FillChar(List[FromIndex + Count - 1], (FromIndex - ToIndex) * SizeOf(List[0]), 0);
{$ENDIF CLR}
end;

procedure MoveArray(var List: TDynStringArray; FromIndex, ToIndex, Count: Integer); overload;
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I]
  else
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
{$ELSE}
begin
  Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
  { Keep reference counting working }
  if FromIndex < ToIndex then
    FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
  else
  if FromIndex > ToIndex then
    FillChar(List[FromIndex + Count - 1], (FromIndex - ToIndex) * SizeOf(List[0]), 0);
{$ENDIF CLR}
end;

procedure MoveArray(var List: TDynObjectArray; FromIndex, ToIndex, Count: Integer); overload;
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I]
  else
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
{$ELSE}
begin
  Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
{$ENDIF CLR}
end;

procedure MoveArray(var List: TDynIntegerArray; FromIndex, ToIndex, Count: Integer); overload;
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I]
  else
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
{$ELSE}
begin
  Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
{$ENDIF CLR}
end;

procedure MoveChar(const Source: string; FromIndex: Integer;
  var Dest: string; ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
  Buf: array of Char;
begin
  Buf := Dest.ToCharArray;
  if FromIndex <= ToIndex then
    for I := 0 to Count - 1 do
      Buf[ToIndex + I] := Source[FromIndex + I]
  else
    for I := Count - 1 downto 0 do
      Buf[ToIndex + I] := Source[FromIndex + I];
  Dest := System.String.Create(Buf);
{$ELSE}
begin
  Move(Source[FromIndex + 1], Dest[ToIndex + 1], Count * SizeOf(Char));
{$ENDIF CLR}
end;

{$IFDEF CLR}

function GetBytesEx(const Value): TBytes;
begin
  if TObject(Value) is TBytes then
    Result := Copy(TBytes(Value))
  else
  if TObject(Value) is TDynByteArray then
    Result := Copy(TDynByteArray(Value))
  else
  if TObject(Value) is System.Enum then // e.g. TIntegerSet
    BitConverter.GetBytes(UInt32(Value))
  { TODO : Add further types }
  else
    raise EJclError.CreateFmt(RsEGetBytesExFmt, [TObject(Value).GetType.FullName]);
end;

procedure SetBytesEx(var Value; Bytes: TBytes);
begin
  if TObject(Value) is TBytes then
    Value := Copy(Bytes)
  else
  if TObject(Value) is TDynByteArray then
    Value := Copy(Bytes)
  else
  if TObject(Value) is System.Enum then // e.g. TIntegerSet
    Value := BitConverter.ToUInt32(Bytes, 0)
  { TODO : Add further types }
  else
    raise EJclError.CreateFmt(RsESetBytesExFmt, [TObject(Value).GetType.FullName]);
end;

procedure SetIntegerSet(var DestSet: TIntegerSet; Value: UInt32);
begin
  DestSet := TIntegerSet(Value);
end;

function ByteArrayStringLen(Data: TBytes): Integer;
var
  I: Integer;
begin
  for I := 0 to High(Data) do
    if Data[I] = 0 then
    begin
      Result := I + 1;
      Exit;
    end;
  Result := Length(Data);
end;

function StringToByteArray(const S: string): TBytes;
var
  I: Integer;
  AnsiS: AnsiString;
begin
  AnsiS := S; // convert to AnsiString
  SetLength(Result, Length(AnsiS));
  for I := 0 to High(Result) do
    Result[I] := Byte(AnsiS[I + 1]);
end;

function ByteArrayToString(const Data: TBytes; Count: Integer): string;
var
  I: Integer;
  AnsiS: AnsiString;
begin
  if Length(Data) < Count then
    Count := Length(Data);
  SetLength(AnsiS, Count);
  for I := 0 to Length(AnsiS) - 1 do
    AnsiS[I + 1] := AnsiChar(Data[I]);
  Result := AnsiS; // convert to System.String
end;

{$ENDIF CLR}

//== { EJclWin32Error } ======================================================

{$IFDEF MSWINDOWS}

constructor EJclWin32Error.Create(const Msg: string);
begin
  {$IFDEF CLR}
  inherited Create(''); // this works because the GC cleans the memory
  {$ENDIF CLR}
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  inherited CreateFmt(Msg + AnsiLineBreak + RsWin32Prefix, [FLastErrorMsg, FLastError]);
end;

constructor EJclWin32Error.CreateFmt(const Msg: string; const Args: array of const);
begin
  {$IFDEF CLR}
  inherited Create(''); // this works because the GC cleans the memory
  {$ENDIF CLR}
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  inherited CreateFmt(Msg + AnsiLineBreak + Format(RsWin32Prefix, [FLastErrorMsg, FLastError]), Args);
end;

{$IFNDEF CLR}
constructor EJclWin32Error.CreateRes(Ident: Integer);
begin
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  inherited CreateFmt(LoadStr(Ident) + AnsiLineBreak + RsWin32Prefix, [FLastErrorMsg, FLastError]);
end;

constructor EJclWin32Error.CreateRes(ResStringRec: PResStringRec);
begin
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  {$IFDEF FPC}
  inherited CreateFmt(ResStringRec^ + AnsiLineBreak + RsWin32Prefix, [FLastErrorMsg, FLastError]);
  {$ELSE}
  inherited CreateFmt(LoadResString(ResStringRec) + AnsiLineBreak + RsWin32Prefix, [FLastErrorMsg, FLastError]);
  {$ENDIF FPC}
end;
{$ENDIF ~CLR}

{$ENDIF MSWINDOWS}

// Int64 support

procedure I64ToCardinals(I: Int64; var LowPart, HighPart: Cardinal);
begin
  {$IFDEF CLR}
  LowPart := Cardinal(I and $00000000FFFFFFFF);
  HighPart := Cardinal(I shr 32);
  {$ELSE}
  LowPart := TULargeInteger(I).LowPart;
  HighPart := TULargeInteger(I).HighPart;
  {$ENDIF CLR}
end;

procedure CardinalsToI64(var I: Int64; const LowPart, HighPart: Cardinal);
begin
  {$IFDEF CLR}
  I := Int64(HighPart) shl 16 or LowPart;
  {$ELSE}
  TULargeInteger(I).LowPart := LowPart;
  TULargeInteger(I).HighPart := HighPart;
  {$ENDIF CLR}
end;

// Cross Platform Compatibility

{$IFNDEF XPLATFORM_RTL}
procedure RaiseLastOSError;
begin
  RaiseLastWin32Error;
end;
{$ENDIF ~XPLATFORM_RTL}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
