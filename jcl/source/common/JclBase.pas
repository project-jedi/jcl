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
  JclVersionMinor   = 94;   // Fourth minor release since JCL 1.90
  JclVersionRelease = 1;    // 0=pre-release|beta/1=release
  JclVersionBuild   = 1802; // build number, days since march 1, 2000
  JclVersion = (JclVersionMajor shl 24) or (JclVersionMinor shl 16) or
    (JclVersionRelease shl 15) or (JclVersionBuild shl 0);

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

type
  {$IFDEF FPC}
  Largeint = Int64;
  {$ELSE}
  PPointer = ^Pointer;
  {$IFNDEF RTL140_UP}
  PBoolean = ^Boolean;
  {$ENDIF ~RTL140_UP}
  {$ENDIF FPC}

  {$IFNDEF COMPILER7}
  UInt64 = Int64;
  {$ENDIF ~COMPILER7}

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
  TDynPointerArray    = array of Pointer;
  TDynStringArray     = array of string;
  TDynIInterfaceArray = array of IInterface;
  TDynObjectArray     = array of TObject;

//--------------------------------------------------------------------------------------------------
// Cross-Platform Compatibility
//--------------------------------------------------------------------------------------------------

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

{$IFNDEF XPLATFORM_RTL}
procedure RaiseLastOSError;
{$ENDIF ~XPLATFORM_RTL}

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
  {$ELSE}
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
// Cross Platform Compatibility
//==================================================================================================

{$IFNDEF XPLATFORM_RTL}
procedure RaiseLastOSError;
begin
  RaiseLastWin32Error;
end;
{$ENDIF ~XPLATFORM_RTL}

// History:

// $Log$
// Revision 1.28  2005/02/05 14:21:59  rrossmair
// - version information updated
//
// Revision 1.27  2005/01/06 18:48:31  marquardt
// AnsiLineBreak, AnsiLineFeed, AnsiCarriageReturn, AnsiCrLf moved to JclBase JclStrings now reexports the names
//
// Revision 1.26  2004/12/23 04:31:42  rrossmair
// - check-in for JCL 1.94 RC 1
//
// Revision 1.25  2004/12/18 03:58:05  rrossmair
// - fixed to compile in Delphi 5 again
//
// Revision 1.24  2004/12/17 05:33:02  marquardt
// updates for DCL
//
// Revision 1.23  2004/11/18 00:57:14  rrossmair
// - check-in for release 1.93
//
// Revision 1.22  2004/11/06 02:13:24  mthoma
// history cleaning.
//
// Revision 1.21  2004/09/30 07:50:29  marquardt
// remove PH contributions
//
// Revision 1.20  2004/09/16 19:47:32  rrossmair
// check-in in preparation for release 1.92
//
// Revision 1.19  2004/06/16 07:30:14  marquardt
// added tilde to all IFNDEF ENDIFs, inherited qualified
//
// Revision 1.18  2004/06/14 11:05:50  marquardt
// symbols added to all ENDIFs and some other minor style changes like removing IFOPT
//
// Revision 1.17  2004/06/14 06:24:52  marquardt
// style cleaning IFDEF
//
// Revision 1.16  2004/06/06 01:31:09  rrossmair
// version information updated for build #1558
//
// Revision 1.15  2004/05/31 01:43:18  rrossmair
// Processed documentation TODOs
//
// Revision 1.14  2004/05/13 07:47:30  rrossmair
// Removed FPC compatibility code rendered superfluous by latest FPC updates; updated build #
//
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
// Revision 1.9  2004/04/14 23:04:09  
// add TDynLongWordArray, TDynBooleanArray
//
// Revision 1.8  2004/04/06 04:53:18
// adapt compiler conditions, add log entry
//

end.
