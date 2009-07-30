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
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclPCRE;

{$I jcl.inc}

{$RANGECHECKS OFF}

interface

uses
  pcre,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  Classes, SysUtils,
  JclBase, JclStringConversions;

const
  JCL_PCRE_CALLOUT_NOERROR      = 0;
  JCL_PCRE_CALLOUT_FAILCONTINUE = 1;

  JCL_PCRE_ERROR_CALLOUTERROR   = -998;
  JCL_PCRE_ERROR_STUDYFAILED    = -999;

type
  TJclRegEx = class;

  EPCREError = class(EJclError)
  private
    FErrorCode: Integer;
  public
    constructor CreateRes(ResStringRec: PResStringRec; ErrorCode: Integer);
    property ErrorCode: Integer read FErrorCode;
  end;

  TPCREIntArray = array [0 .. 0] of Integer;
  PPCREIntArray = ^TPCREIntArray;

  TJclRegExOption = (roIgnoreCase, roMultiLine, roDotAll, roExtended,
    roAnchored, roDollarEndOnly, roExtra, roNotBOL, roNotEOL, roUnGreedy,
    roNotEmpty, roUTF8, roNoAutoCapture, roNoUTF8Check, roAutoCallout,
    roPartial, roDfaShortest, roDfaRestart, roDfaFirstLine, roDupNames,
    roNewLineCR, roNewLineLF, roNewLineCRLF, roNewLineAny, roBSRAnyCRLF,
    roBSRUnicode, roJavascriptCompat, roNoStartOptimize);
  TJclRegExOptions = set of TJclRegExOption;
  TJclCaptureRange = record
    FirstPos: Integer;
    LastPos: Integer;
  end;

  TJclRegExCallout = procedure (Sender: TJclRegEx;
    Index, MatchStart, SubjectPos, LastCapture, PatternPos, NextItemLength: Integer;
    var ErrorCode: Integer) of object;
  TPCRECalloutIndex = 0 .. 255;

  TJclRegEx = class(TObject)
  private
    FCode: PPCRE;
    FExtra: PPCREExtra;
    FOptions: TJclRegExOptions;
    FPattern: string;
    FDfaMode: Boolean;
    FSubject: string;

    FViewChanges: Boolean;
    FChangedCaptures: TList;
    FResultValues: array of string;

    FErrorCode: Integer;
    FErrorMessage: string;
    FErrorOffset: Integer;

    FVector: PPCREIntArray;
    FVectorSize: Integer;
    FCaptureCount: Integer;

    FOnCallout: TJclRegExCallout;

    function GetResult: string;
    function GetCapture(Index: Integer): string;
    procedure SetCapture(Index: Integer; const Value: string);
    function GetCaptureRange(Index: Integer): TJclCaptureRange;
    function GetNamedCapture(const Name: string): string;
    procedure SetNamedCapture(const Name, Value: string);
    function GetCaptureNameCount: Integer;
    function GetCaptureName(Index: Integer): string;
    function GetAPIOptions(RunTime: Boolean): Integer;
    function CalloutHandler(var CalloutBlock: pcre_callout_block): Integer;

  public
    destructor Destroy; override;

    property Options: TJclRegExOptions read FOptions write FOptions;
    function Compile(const Pattern: string; Study: Boolean;
      UserLocale: Boolean = False): Boolean;
    property Pattern: string read FPattern;
    property DfaMode: Boolean read FDfaMode write FDfaMode;
    function Match(const Subject: string; StartOffset: Cardinal = 1): Boolean;
    property Subject: string read FSubject;
    property Result: string read GetResult;

    property ViewChanges: Boolean read FViewChanges write FViewChanges;
    property CaptureCount: Integer read FCaptureCount write FCaptureCount;
    property Captures[Index: Integer]: string read GetCapture write SetCapture;
    property CaptureRanges[Index: Integer]: TJclCaptureRange read GetCaptureRange;

    property NamedCaptures[const Name: string]: string
      read GetNamedCapture write SetNamedCapture;
    property CaptureNameCount: Integer read GetCaptureNameCount;
    property CaptureNames[Index: Integer]: string read GetCaptureName;
    function IndexOfName(const Name: string): Integer;
    function IsNameValid(const Name: string): Boolean;

    property ErrorCode: Integer read FErrorCode;
    property ErrorMessage: string read FErrorMessage;
    property ErrorOffset: Integer read FErrorOffset;

    property OnCallout: TJclRegExCallout read FOnCallout write FOnCallout;
  end;

  TJclAnsiRegEx = TJclRegEx;
  TJclAnsiRegExOption = TJclRegExOption;
  TJclAnsiRegExOptions = TJclRegExOptions;
  TJclAnsiCaptureRange = TJclCaptureRange;
  TJclAnsiRegExCallout = TJclRegExCallout;


procedure InitializeLocaleSupport;
procedure TerminateLocaleSupport;

// Args is an array of pairs (CaptureIndex, Value) or (CaptureName, Value).
// For example: NewIp := StrReplaceRegEx(DirIP, '(\d+)\.(\d+)\.(\d+)\.(\d+)', [3, '128', 4, '254']); 
function StrReplaceRegEx(const Subject, Pattern: string; Args: array of const): string;

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
  SysConst,
  JclResources;

function EncodeString(const S: string; ToUTF8: Boolean): AnsiString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
begin
  if ToUTF8 then
    Result := StringToUTF8(S)
  else
    Result := AnsiString(S);
end;

function DecodeString(const S: AnsiString; IsUTF8: Boolean): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
begin
  if IsUTF8 then
    Result := UTF8ToString(S)
  else
    Result := string(S);
end;

function TranslateIndex(const S: string; ToUTF8: Boolean; Index: Integer): Integer;
var
  UTF8Buffer: TUTF8String;
  UTF8Pos, StrPos, StrLen: Integer;
  Ch: UCS4;
begin
  if ToUTF8 then
  begin
    SetLength(UTF8Buffer, 6);
    StrPos := 1;
    StrLen := Length(S);
    while (StrPos > 0) and (StrPos <= StrLen) and (Index > 1) do
    begin
      UTF8Pos := 1;
      Ch := StringGetNextChar(S, StrPos);
      if (StrPos > 0) and UTF8SetNextChar(UTF8Buffer, UTF8Pos, Ch) and (UTF8Pos > 0) then
        Dec(Index, UTF8Pos - 1);
    end;
    if StrPos <= 0 then
      raise EJclUnexpectedEOSequenceError.Create
    else
    if StrPos > StrLen then
      Result := StrLen + 1
    else
      Result := StrPos;
  end
  else
    Result := Index;
end;

var
  GTables: PAnsiChar;

function JclPCREGetMem(Size: Integer): Pointer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
  GetMem(Result, Size);
end;

procedure JclPCREFreeMem(P: Pointer); {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
  FreeMem(P);
end;

function JclPCRECallout(var callout_block: pcre_callout_block): Integer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
   Result := TJclRegEx(callout_block.callout_data).CalloutHandler(callout_block);
end;

function PCRECheck(Value: Integer): Boolean;
var
  PErr: PResStringRec;
begin
  Result := Value >= 0;
  if Result then Exit;

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
    PCRE_ERROR_NULLWSLIMIT:
      PErr := @RsErrNullWsLimit;
    PCRE_ERROR_BADNEWLINE:
      PErr := @RsErrBadNewLine;
    JCL_PCRE_ERROR_STUDYFAILED:
      PErr := @RsErrStudyFailed;
    JCL_PCRE_ERROR_CALLOUTERROR:
      PErr := @RsErrCalloutError;
  else
    PErr := @RsErrUnknownError;
  end;

  raise EPCREError.CreateRes(PErr, Value);
end;

//=== { TJclRegEx } ===========================================================

destructor TJclRegEx.Destroy;
begin
  if Assigned(FCode) then
    CallPCREFree(FCode);
  if Assigned(FExtra) then
    CallPCREFree(FExtra);
  if Assigned(FVector) then
    FreeMem(FVector);
  if Assigned(FChangedCaptures) then
    FChangedCaptures.Free;

  inherited Destroy;
end;

function TJclRegEx.Compile(const Pattern: string; Study: Boolean;
  UserLocale: Boolean = False): Boolean;
var
  ErrMsgPtr: PAnsiChar;
  Tables: PAnsiChar;
begin
  if UserLocale then
  begin
    InitializeLocaleSupport;
    Tables := GTables;
  end
  else
    Tables := nil;

  FPattern := Pattern;
  if FPattern = '' then
    raise EPCREError.CreateRes(@RsErrNull, PCRE_ERROR_NULL);

  if Assigned(FCode) then
  begin
    CallPCREFree(FCode);
    FCode := nil;
  end;
  FCode := pcre_compile2(PAnsiChar(EncodeString(FPattern, roUTF8 in Options)), GetAPIOptions(False),
    @FErrorCode, @ErrMsgPtr, @FErrorOffset, Tables);
  Inc(FErrorOffset);
  FErrorMessage := string(AnsiString(ErrMsgPtr));
  Result := Assigned(FCode);
  if Result then
  begin
    if Study then
    begin
      if Assigned(FExtra) then CallPCREFree(FExtra);
      FExtra := pcre_study(FCode, 0, @ErrMsgPtr);
      Result := Assigned(FExtra) or (not Assigned(ErrMsgPtr));
      if not Result then
      begin
        FErrorCode := JCL_PCRE_ERROR_STUDYFAILED;
        FErrorMessage := string(AnsiString(ErrMsgPtr));
      end;
    end;

    if FDfaMode then
      FVectorSize := FCaptureCount
    else
    begin
      PCRECheck(pcre_fullinfo(FCode, FExtra, PCRE_INFO_CAPTURECOUNT, @FCaptureCount));
      FVectorSize := (FCaptureCount + 1) * 3;
    end;
    ReAllocMem(FVector, FVectorSize * SizeOf(Integer));
  end;
end;

function TJclRegEx.GetAPIOptions(RunTime: Boolean): Integer;
const
  { roIgnoreCase, roMultiLine, roDotAll, roExtended,
    roAnchored, roDollarEndOnly, roExtra, roNotBOL, roNotEOL, roUnGreedy,
    roNotEmpty, roUTF8, roNoAutoCapture, roNoUTF8Check, roAutoCallout,
    roPartial, roDfaShortest, roDfaRestart, roDfaFirstLine, roDupNames,
    roNewLineCR, roNewLineLF, roNewLineCRLF, roNewLineAny }
  cDesignOptions: array [TJclRegExOption] of Integer =
   (PCRE_CASELESS, PCRE_MULTILINE, PCRE_DOTALL, PCRE_EXTENDED, PCRE_ANCHORED,
    PCRE_DOLLAR_ENDONLY, PCRE_EXTRA, 0, 0, PCRE_UNGREEDY, 0, PCRE_UTF8,
    PCRE_NO_AUTO_CAPTURE, PCRE_NO_UTF8_CHECK, PCRE_AUTO_CALLOUT, 0, 0, 0, 0,
    PCRE_DUPNAMES, PCRE_NEWLINE_CR, PCRE_NEWLINE_LF, PCRE_NEWLINE_CRLF,
    PCRE_NEWLINE_ANY, PCRE_BSR_ANYCRLF, PCRE_BSR_UNICODE,
    PCRE_JAVASCRIPT_COMPAT, PCRE_NO_START_OPTIMIZE);
  cRunOptions: array [TJclRegExOption] of Integer =
   (0, 0, 0, 0, 0, 0, 0, PCRE_NOTBOL, PCRE_NOTEOL, 0, PCRE_NOTEMPTY, 0, 0,
   PCRE_NO_UTF8_CHECK, 0, PCRE_PARTIAL, 0, 0, 0, 0, PCRE_NEWLINE_CR,
   PCRE_NEWLINE_LF, PCRE_NEWLINE_CRLF, PCRE_NEWLINE_ANY, PCRE_BSR_ANYCRLF,
   PCRE_BSR_UNICODE, PCRE_JAVASCRIPT_COMPAT, PCRE_NO_START_OPTIMIZE);
var
  I: TJclRegExOption;
  SUPPORT_UTF8: Integer;
begin
  PCRECheck(pcre_config(PCRE_CONFIG_UTF8, @SUPPORT_UTF8));
  if (roUTF8 in Options) and (SUPPORT_UTF8 = 0) then
    raise EPCREError.CreateRes(@RsErrNoUTF8Support, 0);

  Result := 0;
  if RunTime then
  begin
    for I := Low(TJclRegExOption) to High(TJclRegExOption) do
      if I in Options then
        Result := Result or cRunOptions[I];
  end
  else
  begin
    for I := Low(TJclRegExOption) to High(TJclRegExOption) do
      if I in Options then
        Result := Result or cDesignOptions[I];
  end;
end;

function TJclRegEx.GetResult: string;
var
  Index, CaptureIndex, Pos: Integer;
  Range: TJclCaptureRange;
begin
  if Assigned(FChangedCaptures) and (FChangedCaptures.Count > 0) then
  begin
    Pos := 1;
    Result := '';
    for Index := 0 to FChangedCaptures.Count - 1 do
    begin
      CaptureIndex := Integer(FChangedCaptures[Index]);
      Range := GetCaptureRange(CaptureIndex);
      
      Result := Result +
        Copy(FSubject, Pos, Range.FirstPos - Pos) + 
        FResultValues[CaptureIndex];
        
      Pos := Range.LastPos + 1;
    end;
    if Pos <= Length(FSubject) then
      Result := Result + Copy(FSubject, Pos, Length(FSubject) - Pos + 1);
  end
  else
    Result := FSubject;
end;

function TJclRegEx.GetCapture(Index: Integer): string;
var
  FromPos, ToPos: Integer;
begin
  if (Index < 0) or (Index >= FCaptureCount) then
    PCRECheck(PCRE_ERROR_NOSUBSTRING)
  else
  begin
    if FViewChanges and (FChangedCaptures.IndexOf(Pointer(Index)) >= 0) then
    begin
      Result := FResultValues[Index];
      Exit;
    end;

    Index := Index * 2;
    FromPos := TranslateIndex(FSubject, roUTF8 in Options, FVector^[Index] + 1);
    ToPos := TranslateIndex(FSubject, roUTF8 in Options, FVector^[Index + 1] + 1) - 1;
    Result := Copy(FSubject, FromPos, ToPos - FromPos + 1);
  end;
end;

procedure TJclRegEx.SetCapture(Index: Integer; const Value: string);
begin
  if (Index < 0) or (Index >= FCaptureCount) then
    PCRECheck(PCRE_ERROR_NOSUBSTRING)
  else
  begin
    if (not Assigned(FChangedCaptures)) or (FChangedCaptures.Count = 0) then
    begin
      if not Assigned(FChangedCaptures) then
        FChangedCaptures := TList.Create;

      // Always resize to the max length to avoid repeated allocations.
      FChangedCaptures.Capacity := FCaptureCount;
      SetLength(FResultValues, FCaptureCount);
    end;

    if FChangedCaptures.IndexOf(Pointer(Index)) < 0 then
      FChangedCaptures.Add(Pointer(Index));
    FResultValues[Index] := Value;
  end;
end;

function TJclRegEx.GetCaptureRange(Index: Integer): TJclCaptureRange;
begin
  if (Index < 0) or (Index >= FCaptureCount) then
    PCRECheck(PCRE_ERROR_NOSUBSTRING)
  else
  begin
    Index := Index * 2;
    Result.FirstPos := TranslateIndex(FSubject, roUTF8 in Options, FVector^[Index] + 1);
    Result.LastPos := TranslateIndex(FSubject, roUTF8 in Options, FVector^[Index + 1] + 1) - 1;
  end;
end;

function TJclRegEx.GetNamedCapture(const Name: string): string;
var
  Index: Integer;
begin
  Index := pcre_get_stringnumber(FCode, PAnsiChar(EncodeString(Name, roUTF8 in Options)));
  PCRECheck(Index);

  Result := GetCapture(Index);
end;

procedure TJclRegEx.SetNamedCapture(const Name, Value: string);
var
  Index: Integer;
begin
  Index := pcre_get_stringnumber(FCode, PAnsiChar(EncodeString(Name, roUTF8 in Options)));
  PCRECheck(Index);

  SetCapture(Index, Value);
end;

function TJclRegEx.GetCaptureNameCount: Integer;
begin
  PCRECheck(pcre_fullinfo(FCode, FExtra, PCRE_INFO_NAMECOUNT, @Result));
end;

function TJclRegEx.GetCaptureName(Index: Integer): string;
var
  NameTable: PAnsiChar;
  EntrySize: Integer;
begin
  PCRECheck(pcre_fullinfo(FCode, FExtra, PCRE_INFO_NAMETABLE, @NameTable));
  PCRECheck(pcre_fullinfo(FCode, FExtra, PCRE_INFO_NAMEENTRYSIZE, @EntrySize));

  NameTable := NameTable + EntrySize * Index + 2;
  Result := DecodeString(AnsiString(NameTable), roUTF8 in Options);
end;

function TJclRegEx.CalloutHandler(var CalloutBlock: pcre_callout_block): Integer;
begin
  try
    Result := JCL_PCRE_CALLOUT_NOERROR;
    if Assigned(FOnCallout) then
    begin
      with CalloutBlock do
      begin
        FCaptureCount := capture_top;
        FOnCallout(Self, callout_number, start_match + 1, current_position + 1,
          capture_last, pattern_position + 1, next_item_length, Result);
      end;
    end;
  except
    on E: Exception do
    begin
      FErrorMessage := E.Message;
      Result := JCL_PCRE_ERROR_CALLOUTERROR;
    end;
  end;
end;

function TJclRegEx.Match(const Subject: string; StartOffset: Cardinal = 1): Boolean;
var
  LocalExtra: real_pcre_extra;
  Extra: Pointer;
  WorkSpace: array [0 .. 19] of Integer;
  ExecRslt: Integer;
  EncodedSubject: AnsiString;
begin
  if Assigned(FOnCallout) then
  begin
    if Assigned(FExtra) then
    begin
      LocalExtra.flags := PCRE_EXTRA_STUDY_DATA or PCRE_EXTRA_CALLOUT_DATA;
      LocalExtra.study_data := FExtra;
    end
    else
      LocalExtra.flags := PCRE_EXTRA_CALLOUT_DATA;
    LocalExtra.callout_data := Self;
    Extra := @LocalExtra;
    SetPCRECalloutCallback(JclPCRECallout);
  end
  else
  begin
    Extra := FExtra;
    SetPCRECalloutCallback(nil);
  end;

  FSubject := Subject;
  if Assigned(FChangedCaptures) then
    FChangedCaptures.Clear;
  EncodedSubject := EncodeString(FSubject, roUTF8 in Options);

  // convert index
  if roUTF8 in Options then
    StartOffset := Length(EncodeString(Copy(FSubject, 1, StartOffset - 1), True)) + 1;

  if FDfaMode then
  begin
    ExecRslt := pcre_dfa_exec(FCode, Extra, PAnsiChar(EncodedSubject), Length(EncodedSubject),
      StartOffset - 1, GetAPIOptions(True), PInteger(FVector), FVectorSize, @Workspace, 20);
  end
  else
  begin
    ExecRslt := pcre_exec(FCode, Extra, PAnsiChar(EncodedSubject), Length(EncodedSubject),
      StartOffset - 1, GetAPIOptions(True), PInteger(FVector), FVectorSize);
  end;
  Result := ExecRslt >= 0;
  if Result then
  begin
    FCaptureCount := ExecRslt;
    FErrorCode := 0;
  end
  else
  begin
    FErrorCode := ExecRslt;
    if FErrorCode <> PCRE_ERROR_NOMATCH then
      PCRECheck(FErrorCode);
  end;
end;

function TJclRegEx.IndexOfName(const Name: string): Integer;
begin
  Result := pcre_get_stringnumber(FCode, PAnsiChar(EncodeString(Name, roUTF8 in Options)));
end;

function TJclRegEx.IsNameValid(const Name: string): Boolean;
begin
  Result := pcre_get_stringnumber(FCode, PAnsiChar(EncodeString(Name, roUTF8 in Options))) >= 0;
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
    CallPCREFree(GTables);
    GTables := nil;
  end;
end;

// TODO: Better/specific error messages, show index when available.
function StrReplaceRegEx(const Subject, Pattern: string; Args: array of const): string;

  function ArgToString(Index: Integer): string;
  begin
    // TODO: Any other type?
    case TVarRec(Args[Index]).VType of
      vtPChar:
        Result := string(AnsiString(TVarRec(Args[Index]).VPChar));
      vtPWideChar:
        Result := string(WideString(TVarRec(Args[Index]).VPWideChar));
      vtString:
        Result := string(TVarRec(Args[Index]).VString^);
      vtAnsiString:
        Result := string(AnsiString(TVarRec(Args[Index]).VAnsiString));
      vtWideString:
        Result := string(WideString(TVarRec(Args[Index]).VWideString));
      {$IFDEF SUPPORTS_UNICODE_STRING}
      vtUnicodeString:
        Result := string(UnicodeString(TVarRec(Args[Index]).VUnicodeString));
      {$ENDIF SUPPORTS_UNICODE_STRING}
      vtChar:
        Result := string(AnsiString(TVarRec(Args[Index]).VChar));
      vtWideChar:
        Result := string(WideString(TVarRec(Args[Index]).VWideChar));
    else
      raise EConvertError.Create(SInvalidFormat);
    end;
  end;

var
  Re: TJclRegEx;
  Index, ArgIndex: Integer;
  Value: string;
begin
  if Odd(Length(Args)) then
    raise EConvertError.Create(SArgumentMissing)
  else
  begin
    Re := TJclRegEx.Create;
    try
      if Re.Compile(Pattern, False) and Re.Match(Subject) then
      begin
        for Index := 0 to Length(Args) div 2 - 1 do
        begin
          ArgIndex := Index * 2;
          Value := ArgToString(ArgIndex + 1);

          if TVarRec(Args[ArgIndex]).VType = vtInteger then
            Re.Captures[TVarRec(Args[ArgIndex]).VInteger] := Value
          else
            Re.NamedCaptures[ArgToString(ArgIndex)] := Value;
        end;

        Result := Re.Result;
      end
      else
        raise EConvertError.Create(SInvalidFormat);
    finally
      Re.Free;
    end;
  end;
end;

//=== { EPCREError } =========================================================

constructor EPCREError.CreateRes(ResStringRec: PResStringRec; ErrorCode: Integer);
begin
  FErrorCode := ErrorCode;
  inherited CreateRes(ResStringRec);
end;

procedure LibNotLoadedHandler; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
  raise EPCREError.CreateRes(@RsErrLibNotLoaded, 0);
end;

initialization
  pcre.LibNotLoadedHandler := LibNotLoadedHandler;
  if LoadPCRE then
  begin
    SetPCREMallocCallback(JclPCREGetMem);
    SetPCREFreeCallback(JclPCREFreeMem);
  end;
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

