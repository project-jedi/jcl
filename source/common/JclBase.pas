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
{ Contributor(s):                                                                                  }
{   Marcel van Brakel, Peter Friese, Peter J. Haas (PeterJHaas) jediplus@pjh2.de, Robert Marquardt,}
{   Robert Rossmair, Petr Vones                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains generic JCL base classes and routines to support earlier                      }
{ versions of Delphi as well as FPC.                                                               }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclBase;

{$I jcl.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils;

//--------------------------------------------------------------------------------------------------
// Version
//--------------------------------------------------------------------------------------------------

const
  JclVersionMajor   = 1;    // 0=pre-release|beta/1, 2, ...=final
  JclVersionMinor   = 91;   // Forth minor release JCL 1.20
  JclVersionRelease = 0;    // 0=pre-release|beta/1=release
  JclVersionBuild   = 1531;  // build number, days since march 1, 2000
  JclVersion = (JclVersionMajor shl 24) or (JclVersionMinor shl 16) or
    (JclVersionRelease shl 15) or (JclVersionBuild shl 0);

//--------------------------------------------------------------------------------------------------
// FreePascal Support
//--------------------------------------------------------------------------------------------------

{$IFDEF FPC}

function SysErrorMessage(ErrNo: Integer): string;

{$IFDEF MSWINDOWS}
function Win32Check(RetVal: BOOL): BOOL;
{$ENDIF MSWINDOWS}

var
  Default8087CW: Word;
{$ENDIF FPC}

//--------------------------------------------------------------------------------------------------
// EJclError
//--------------------------------------------------------------------------------------------------

type
  EJclError = class(Exception)
  public
    constructor CreateResRec(ResStringRec: PResStringRec);
    constructor CreateResRecFmt(ResStringRec: PResStringRec; const Args: array of const);
  end;

//--------------------------------------------------------------------------------------------------
// EJclWin32Error
//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

type
  EJclWin32Error = class(EJclError)
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
// EJclInternalError
//--------------------------------------------------------------------------------------------------

type
  { TODO -cHelp : Exception class for JCL internal errors }
  EJclInternalError = class(EJclError);

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
  Largeint    = Int64;
{$ENDIF FPC}
type
  {$IFNDEF FPC}
  PPointer = ^Pointer;
  {$ENDIF ~FPC}

  {$IFNDEF FPC}
  {$IFNDEF RTL140_UP}
  PBoolean = ^Boolean;
  {$ENDIF RTL140_UP}
  {$ENDIF ~FPC}

  {$IFNDEF COMPILER7}
  UInt64 = Int64;
  {$ENDIF COMPILER7}

//--------------------------------------------------------------------------------------------------
// Int64 support
//--------------------------------------------------------------------------------------------------

procedure I64ToCardinals(I: Int64; var LowPart, HighPart: Cardinal);
procedure CardinalsToI64(var I: Int64; const LowPart, HighPart: Cardinal);

// Redefinition of TLargeInteger to relieve dependency on Windows.pas

type
  PLargeInteger = ^TLargeInteger;
  TLargeInteger = Int64;

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

//--------------------------------------------------------------------------------------------------
// Dynamic Array support
//--------------------------------------------------------------------------------------------------

type
  TDynByteArray     = array of Byte;
  TDynShortIntArray = array of ShortInt;
  TDynWordArray     = array of Word;
  TDynSmallIntArray = array of SmallInt;
  TDynLongWordArray = array of LongWord;
  TDynLongIntArray  = array of LongInt;
  TDynInt64Array    = array of Int64;
  TDynCardinalArray = array of Cardinal;
  TDynIntegerArray  = array of Integer;
  TDynExtendedArray = array of Extended;
  TDynDoubleArray   = array of Double;
  TDynSingleArray   = array of Single;
  TDynFloatArray    = array of Float;
  TDynPointerArray  = array of Pointer;
  TDynStringArray   = array of string;
  TDynBooleanArray  = array of Boolean;

//--------------------------------------------------------------------------------------------------
// Cross-Platform Compatibility
//--------------------------------------------------------------------------------------------------

{$IFNDEF XPLATFORM_RTL}
procedure RaiseLastOSError;
{$ENDIF XPLATFORM_RTL}

//--------------------------------------------------------------------------------------------------
// Interface compatibility
//--------------------------------------------------------------------------------------------------

{$IFDEF SUPPORTS_INTERFACE}
{$IFNDEF FPC}
{$IFNDEF RTL140_UP}

type
  IInterface = IUnknown;

{$ENDIF ~RTL140_UP}
{$ENDIF ~FPC}
{$ENDIF SUPPORTS_INTERFACE}

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

function Win32Check(RetVal: BOOL): BOOL;
begin
  if not RetVal then
    RaiseLastOSError;
  Result := RetVal;
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
// Cross=Platform Compatibility
//==================================================================================================

{$IFNDEF XPLATFORM_RTL}

procedure RaiseLastOSError;
begin
  RaiseLastWin32Error;
end;

{$ENDIF XPLATFORM_RTL}

// History:

// $Log$
// Revision 1.13  2004/05/08 19:56:55  rrossmair
// FPC-related improvements
//
// Revision 1.12  2004/05/06 05:09:55  rrossmair
// Changes for FPC v1.9.4 compatibility
//
// Revision 1.11  2004/05/05 00:04:10  mthoma
// Updated headers: Added donors as contributors, adjusted the initial authors, added cvs names when they were not obvious. Changed $data to $date where necessary,
//
// Revision 1.10  2004/04/19 06:02:18  rrossmair
// fixed QueryPerformanceCounter (FPC compatibility routine)
//
// Revision 1.9  2004/04/14 23:04:09  peterjhaas
// add TDynLongWordArray, TDynBooleanArray
//
// Revision 1.8  2004/04/06 04:53:18  peterjhaas
// adapt compiler conditions, add log entry
//

end.
