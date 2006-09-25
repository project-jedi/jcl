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

{$RANGECHECKS OFF}

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

const
  JCL_PCRE_ERROR_STUDYFAILED = -999;

type
  EPCREError = class(Exception)
  private
    FErrorCode: Integer;
  public
    constructor CreateRes(ResStringRec: PResStringRec; ErrorCode: Integer);
    property ErrorCode: Integer read FErrorCode;
  end;

  TPCREIntArray = array [0 .. 0] of Integer;
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
    FSubject: String;

    FErrorCode: Integer;
    FErrorMessage: String;
    FErrorOffset: Integer;

    FVector: PPCREIntArray;
    FVectorSize: Integer;
    FStringCount: Integer;

    function GetCaptureCount: Integer;
    function GetCaptures(Index: Integer): String;
    function GetAPIOptions(RunTime: Boolean): Integer;
    function GetCapturesOffset(Index: Integer): TJclAnsiCaptureOffset;

  public
    destructor Destroy; override;

    property Options: TJclAnsiRegExOptions read FOptions write FOptions;
    function Compile(const Pattern: String; Study: Boolean;
      UserLocale: Boolean = False): Boolean;
    function Match(const Subject: String; StartOffset: Cardinal = 1): Boolean;
    property CaptureCount: Integer read GetCaptureCount;
    property Captures[Index: Integer]: String read GetCaptures;
    property CaptureOffset[Index: Integer]: TJclAnsiCaptureOffset read GetCapturesOffset;

    property ErrorCode: Integer read FErrorCode;
    property ErrorMessage: String read FErrorMessage;
    property ErrorOffset: Integer read FErrorOffset;
  end;

procedure InitializeLocaleSupport;
procedure TerminateLocaleSupport;

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

var
  GTables: PChar;

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
    JCL_PCRE_ERROR_STUDYFAILED:
      PErr := @RsErrStudyFailed;
  else
    Result := True;
  end;
  if not Result then
    raise EPCREError.CreateRes(PErr, Value);
end;

//=== { TJclAnsiRegEx } ======================================================

destructor TJclAnsiRegEx.Destroy;
begin
  if Assigned(FCode) then
    pcre_free^(FCode);
  if Assigned(FExtra) then
    pcre_free^(FExtra);
  if Assigned(FVector) then
    FreeMem(FVector);

  inherited Destroy;
end;

function TJclAnsiRegEx.Compile(const Pattern: String; Study: Boolean;
  UserLocale: Boolean = False): Boolean;
var
  ErrMsgPtr: PChar;
  Tables: PChar;
begin
  if UserLocale then
  begin
    InitializeLocaleSupport;
    Tables := GTables;
  end
  else
    Tables := nil;

  if Pattern = '' then
    raise EPCREError.CreateRes(@RsErrNull, PCRE_ERROR_NULL);

  if Assigned(FCode) then pcre_free^(FCode);
  FCode := pcre_compile2(PChar(Pattern), GetAPIOptions(False),
    @FErrorCode, @ErrMsgPtr, @FErrorOffset, Tables);
  FErrorMessage := ErrMsgPtr;
  Result := Assigned(FCode);
  if Result then
  begin
    if Study then
    begin
      if Assigned(FExtra) then pcre_free^(FExtra);
      FExtra := pcre_study(FCode, 0, @ErrMsgPtr);
      Result := Assigned(FExtra) or (not Assigned(ErrMsgPtr));
      if not Result then
      begin
        FErrorCode := JCL_PCRE_ERROR_STUDYFAILED;
        FErrorMessage := ErrMsgPtr;
      end;
    end;

    PCRECheck(pcre_fullinfo(FCode, FExtra, PCRE_INFO_CAPTURECOUNT, @FStringCount));
    if FStringCount > 0 then
      FVectorSize := (FStringCount + 1) * 3
    else
      FVectorSize := 0;
    ReAllocMem(FVector, FVectorSize * SizeOf(Integer));
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
end;

function TJclAnsiRegEx.GetCaptures(Index: Integer): String;
var
  From, Len: Integer;
begin
  if (Index < 0) or (Index >= FStringCount) then
    PCRECheck(PCRE_ERROR_NOSUBSTRING)
  else
  begin
    Index := Index * 2;
    From := FVector^[Index];
    Len := FVector^[Index + 1] - From;
    SetLength(Result, Len);
    Move(FSubject[From + 1], PChar(Result)^, Len);
  end;
end;

function TJclAnsiRegEx.GetCapturesOffset(Index: Integer): TJclAnsiCaptureOffset;
begin
  if (Index < 0) or (Index >= FStringCount) then
  begin
    Result.FirstPos := -1;
    Result.LastPos := -1;
  end;
  Index := Index * 2;
  Result.FirstPos := FVector^[Index];
  Result.LastPos := FVector^[Index + 1] - 1;
end;

function TJclAnsiRegEx.Match(const Subject: String; StartOffset: Cardinal = 1): Boolean;
begin
  if (not Assigned(FCode)) or (Subject = '') then
  begin
    Result := False;
    Exit;
  end;
  if StartOffset < 1 then
    StartOffset := 1;

  FSubject := Subject;
  FStringCount := pcre_exec(FCode, FExtra, PChar(FSubject), Length(FSubject),
    StartOffset - 1, GetAPIOptions(True), PInteger(FVector), FVectorSize);
  Result := FStringCount >= 0;
end;

procedure InitializeLocaleSupport;
begin
  if not Assigned(GTables) then
    GTables := pcre_maketables;
end;

procedure TerminateLocaleSupport;
begin
  if Assigned(GTables) then
  begin
    pcre_free^(GTables);
    GTables := nil;
  end;
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
  TerminateLocaleSupport;
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  UnloadPCRE;

end.

