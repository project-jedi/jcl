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
{ The Original Code is JclPCRE.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Peter Thornqvist.                                  }
{ Portions created by Peter Thornqvist are Copyright (C) of Peter Thornqvist. All rights reserved. }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Robert Rossmair (rrossmair)                                                                    }
{   Mario R. Carro                                                                                 }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Class wrapper for PCRE (PERL Compatible Regular Expression)                                      }
{                                                                                                  }
{ Unit owner: Peter Thörnqvist                                                                     }
{ Last modified: $Date$                          }
{                                                                                                  }
{**************************************************************************************************}

unit JclPCRE;

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  Classes, SysUtils;

type
  EPCREError = class(Exception)
  private
    FErrorCode: Integer;
  public
    constructor CreateRes(ResStringRec: PResStringRec; ErrorCode: Integer);
    property ErrorCode: Integer read FErrorCode;
  end;

  TPCREIntArray = array [0..2999] of Integer; // 1000 subpatterns should be enough...
  PPCREIntArray = ^TPCREIntArray;

  TJclAnsiRegExOption = (roIgnoreCase, roMultiLine, roDotAll, roExtended,
    roAnchored, roDollarEndOnly, roExtra, roNotBOL, roNotEOL, roUnGreedy,
    roNotEmpty, roUTF8, roNoAutoCapture, roNoUTF8Check, roAutoCallout,
    roPartial, roDfaShortest, roDfaRestart, roDfaFirstLine, roDupNames,
    roNewLineCR, roNewLineLF);
  TJclAnsiRegExOptions = set of TJclAnsiRegExOption;
  TJclAnsiCaptureOffset = record
    FirstPos: Integer;
    LastPos: Integer;
  end;

  TJclAnsiRegEx = class(TObject)
  private
    FCode: Pointer;
    FExtra: Pointer;
    FOptions: TJclAnsiRegExOptions;
    FSubject: AnsiString;
    FErrorMessage: AnsiString;
    FErrorOffset: Integer;
    FVector: TPCREIntArray;
    FStringCount: Integer;
    FVectorSize: Integer;
    FTables: PChar;
    FMaxCaptureLength: Integer;
    function GetCaptureCount: Integer;
    function GetCaptures(Index: Integer): AnsiString;
    function GetAPIOptions(RunTime: Boolean): Integer;
    function GetCapturesOffset(Index: Integer): TJclAnsiCaptureOffset;
  public
    constructor Create;
    destructor Destroy; override;
    function Compile(const Pattern: AnsiString; Study, UserLocale: Boolean): Boolean;
    function Match(const Subject: AnsiString; StartOffset: Cardinal = 1): Boolean;
    property Options: TJclAnsiRegExOptions read FOptions write FOptions;
    property CaptureCount: Integer read GetCaptureCount;
    property Captures[Index: Integer]: AnsiString read GetCaptures;
    property CaptureOffset[Index: Integer]: TJclAnsiCaptureOffset read GetCapturesOffset;
    property ErrorMessage: AnsiString read FErrorMessage;
    property ErrorOffset: Integer read FErrorOffset;
    property MaxCaptureLength: Integer read FMaxCaptureLength write FMaxCaptureLength;
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
  pcre,
  JclResources;

function JclPCREGetMem(Size: Integer): Pointer; cdecl;
begin
  GetMem(Result, Size);
end;

procedure JclPCREFreeMem(P: Pointer); cdecl;
begin
  FreeMem(P);
end;

function PCRECheck(Value: Integer): Boolean;
var
  PErr: PResStringRec;
begin
  Result := False;
  PErr := nil;
  case Value of
    PCRE_ERROR_NOMATCH:
      PErr := @RsErrNoMatch;
    PCRE_ERROR_NULL:
      PErr := @RsErrNull;
    PCRE_ERROR_BADOPTION:
      PErr := @RsErrBadOption;
    PCRE_ERROR_BADMAGIC:
      PErr := @RsErrBadMagic;
    PCRE_ERROR_UNKNOWN_NODE:
      PErr := @RsErrUnknownNode;
    PCRE_ERROR_NOMEMORY:
      PErr := @RsErrNoMemory;
    PCRE_ERROR_NOSUBSTRING:
      PErr := @RsErrNoSubString;
    PCRE_ERROR_MATCHLIMIT:
      PErr := @RsErrMatchLimit;
    PCRE_ERROR_CALLOUT:
      PErr := @RsErrCallout;
    PCRE_ERROR_BADUTF8:
      PErr := @RsErrBadUTF8;
    PCRE_ERROR_BADUTF8_OFFSET:
      PErr := @RsErrBadUTF8Offset;
    PCRE_ERROR_PARTIAL:
      PErr := @RsErrPartial;
    PCRE_ERROR_BADPARTIAL:
      PErr := @RsErrBadPartial;
    PCRE_ERROR_INTERNAL:
      PErr := @RsErrInternal;
    PCRE_ERROR_BADCOUNT:
      PErr := @RsErrBadCount;
    PCRE_ERROR_DFA_UITEM:
      PErr := @RsErrDfaUItem;
    PCRE_ERROR_DFA_UCOND:
      PErr := @RsErrDfaUCond;
    PCRE_ERROR_DFA_UMLIMIT:
      PErr := @RsErrDfaUMLimit;
    PCRE_ERROR_DFA_WSSIZE:
      PErr := @RsErrDfaWSSize;
    PCRE_ERROR_DFA_RECURSE:
      PErr := @RsErrDfaRecurse;
    PCRE_ERROR_RECURSIONLIMIT:
      PErr := @RsErrRecursionLimit;
  else
    Result := True;
  end;
  if not Result then
    raise EPCREError.CreateRes(PErr, Value);
end;

//=== { TJclAnsiRegEx } ======================================================

constructor TJclAnsiRegEx.Create;
begin
  inherited Create;
  FMaxCaptureLength := 1024;
  FVectorSize := SizeOf(FVector) div SizeOf(Integer);
end;

destructor TJclAnsiRegEx.Destroy;
begin
  if FCode <> nil then
    pcre_free^(FCode);
  if FExtra <> nil then
    pcre_free^(FExtra);

  inherited Destroy;
end;

function TJclAnsiRegEx.Compile(const Pattern: AnsiString; Study, UserLocale: Boolean): Boolean;
var
  ErrPtr: PChar;
  ErrOffset: Integer;
begin
  if UserLocale then
    FTables := pcre_maketables
  else
    FTables := nil;
  if Pattern = '' then
    raise EPCREError.CreateRes(@RsErrNull, PCRE_ERROR_NULL);
  if FCode <> nil then pcre_free^(FCode);
  FCode := pcre_compile(PChar(Pattern), GetAPIOptions(False), @ErrPtr, @ErrOffset, FTables);
  FErrorMessage := ErrPtr;
  FErrorOffset := ErrOffset;
  Result := (FCode <> nil);
  if Result and Study then
  begin
    if FExtra <> nil then pcre_free^(FExtra);
    FExtra := pcre_study(FCode, 0, @ErrPtr);
  end;
end;

function TJclAnsiRegEx.GetAPIOptions(RunTime: Boolean): Integer;
const
  { roIgnoreCase, roMultiLine, roDotAll, roExtended,
    roAnchored, roDollarEndOnly, roExtra, roNotBOL, roNotEOL, roUnGreedy,
    roNotEmpty, roUTF8, roNoAutoCapture, roNoUTF8Check, roAutoCallout,
    roPartial, roDfaShortest, roDfaRestart, roDfaFirstLine, roDupNames,
    roNewLineCR, roNewLineLF }
  cDesignOptions: array [TJclAnsiRegExOption] of Integer =
   (PCRE_CASELESS, PCRE_MULTILINE, PCRE_DOTALL, PCRE_EXTENDED, PCRE_ANCHORED,
    PCRE_DOLLAR_ENDONLY, PCRE_EXTRA, 0, 0, PCRE_UNGREEDY, 0, PCRE_UTF8,
    PCRE_NO_AUTO_CAPTURE, PCRE_NO_UTF8_CHECK, PCRE_AUTO_CALLOUT, 0, 0, 0, 0,
    PCRE_DUPNAMES, PCRE_NEWLINE_CR, PCRE_NEWLINE_LF);
  cRunOptions: array [TJclAnsiRegExOption] of Integer =
   (0, 0, 0, 0, 0, 0, 0, PCRE_NOTBOL, PCRE_NOTEOL, 0, PCRE_NOTEMPTY, 0, 0,
   PCRE_NO_UTF8_CHECK, 0, PCRE_PARTIAL, 0, 0, 0, 0, PCRE_NEWLINE_CR,
   PCRE_NEWLINE_LF);
var
  I: TJclAnsiRegExOption;
begin
  Result := 0;
  if RunTime then
  begin
    for I := Low(TJclAnsiRegExOption) to High(TJclAnsiRegExOption) do
      if I in Options then
        Result := Result or cRunOptions[I];
  end
  else
  begin
    for I := Low(TJclAnsiRegExOption) to High(TJclAnsiRegExOption) do
      if I in Options then
        Result := Result or cDesignOptions[I];
  end;
end;

function TJclAnsiRegEx.GetCaptureCount: Integer;
begin
  Result := FStringCount;
  //  PCRECheck(pcre_fullinfo(FCode, FExtra, PCRE_INFO_CAPTURECOUNT, @Result));
end;

function TJclAnsiRegEx.GetCaptures(Index: Integer): AnsiString;
var
  Len: Integer;
begin
  SetLength(Result, MaxCaptureLength);
  Len := pcre_copy_substring(PChar(FSubject), @FVector, FStringCount, Index, PChar(Result), MaxCaptureLength);
  PCRECheck(Len);
  SetLength(Result, Len);
end;

function TJclAnsiRegEx.GetCapturesOffset(Index: Integer): TJclAnsiCaptureOffset;
begin
  if (Index < 0) or (Index >= FStringCount) then
  begin
    Result.FirstPos := -1;
    Result.LastPos := -1;
  end;
  Result.FirstPos := FVector[Index * 2];
  Result.LastPos := FVector[Index * 2 + 1];
end;

function TJclAnsiRegEx.Match(const Subject: AnsiString; StartOffset: Cardinal = 1): Boolean;
begin
  if (FCode = nil) or (Subject = '') then
  begin
    Result := False;
    Exit;
  end;
  if StartOffset < 1 then
    StartOffset := 1;
  FSubject := Subject;
  FStringCount := pcre_exec(FCode, FExtra, PChar(FSubject), Length(FSubject),
    StartOffset - 1, GetAPIOptions(True), @FVector, FVectorSize);
  Result := FStringCount > 0;
end;

//=== { EPCREError } =========================================================

constructor EPCREError.CreateRes(ResStringRec: PResStringRec; ErrorCode: Integer);
begin
  FErrorCode := ErrorCode;
  inherited CreateRes(ResStringRec);
end;

procedure LibNotLoadedHandler; cdecl;
begin
  raise EPCREError.CreateRes(@RsErrLibNotLoaded, 0);
end;

initialization
  pcre.LibNotLoadedHandler := LibNotLoadedHandler;
  LoadPCRE;
  SetPCREMallocCallback(JclPCREGetMem);
  SetPCREFreeCallback(JclPCREFreeMem);
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  UnloadPCRE;

end.

