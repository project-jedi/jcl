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
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Class wrapper for PCRE (PERL Compatible Regular Expression)                                      }
{                                                                                                  }
{ Unit owner: Peter Thörnqvist                                                                     }
{ Last modified: $Date$                                                      }
{                                                                                                  }
{**************************************************************************************************}

unit JclPCRE;

interface

uses
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
    constructor Create(const Msg: AnsiString; ErrorCode: Integer);
    property ErrorCode: Integer read FErrorCode;
  end;

  TPCREIntArray = array [0..2999] of Integer; // 1000 subpatterns should be enough...
  PPCREIntArray = ^TPCREIntArray;

  TJclAnsiRegExOption = (roIgnoreCase, roMultiLine, roDotAll, roExtended,
    roAnchored, roDollarEndOnly, roExtra, roNotBOL, roNotEOL, roUnGreedy, roNotEmpty, roUTF8);
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
  end;

implementation

uses
  pcre,
  JclResources;

function PCRECheck(Value: Integer): Boolean;
var
  S: AnsiString;
begin
  Result := False;
  case Value of
    PCRE_ERROR_NOMATCH:
      S := SErrNoMatch;
    PCRE_ERROR_NULL:
      S := SErrNull;
    PCRE_ERROR_BADOPTION:
      S := SErrBadOption;
    PCRE_ERROR_BADMAGIC:
      S := SErrBadMagic;
    PCRE_ERROR_UNKNOWN_NODE:
      S := SErrUnknownNode;
    PCRE_ERROR_NOMEMORY:
      S := SErrNoMemory;
    PCRE_ERROR_NOSUBSTRING:
      S := SErrNoSubString;
  else
    Result := True;
  end;
  if not Result then
    raise EPCREError.Create(S, Value);
end;

//=== { TJclAnsiRegEx } ======================================================

constructor TJclAnsiRegEx.Create;
begin
  inherited Create;
  FVectorSize := SizeOf(FVector) div SizeOf(Integer);
end;

destructor TJclAnsiRegEx.Destroy;
begin
  (*
    if FCode <> nil then
      pcre_free(FCode);
    if FExtra <> nil then
      pcre_free(FExtra);
  *)
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
    raise EPCREError.Create(SErrNull, PCRE_ERROR_NULL);
  FCode := pcre_compile(PChar(Pattern), GetAPIOptions(False), @ErrPtr, @ErrOffset, FTables);
  FErrorMessage := ErrPtr;
  FErrorOffset := ErrOffset;
  Result := (FCode <> nil);
  if Result and Study then
    FExtra := pcre_study(FCode, 0, @ErrPtr);
end;

function TJclAnsiRegEx.GetAPIOptions(RunTime: Boolean): Integer;
const
  cDesignOptions: array [TJclAnsiRegExOption] of Integer =
   (PCRE_CASELESS, PCRE_MULTILINE, PCRE_DOTALL, PCRE_EXTENDED, PCRE_ANCHORED, PCRE_DOLLAR_ENDONLY,
    PCRE_EXTRA, 0, 0, PCRE_UNGREEDY, 0, PCRE_UTF8);
  cRunOptions: array [TJclAnsiRegExOption] of Integer =
   (0, 0, 0, 0, 0, 0,
    0, PCRE_NOTBOL, PCRE_NOTEOL, 0, PCRE_NOTEMPTY, 0);
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
  Buffer: array [0..1024] of Char;
begin
  PCRECheck(pcre_copy_substring(PChar(FSubject), @FVector, FStringCount, Index, Buffer, SizeOf(Buffer)));
  Result := AnsiString(Buffer);
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

constructor EPCREError.Create(const Msg: AnsiString; ErrorCode: Integer);
begin
  FErrorCode := ErrorCode;
  inherited Create(Msg);
end;

procedure LibNotLoadedHandler; cdecl;
begin
  raise EPCREError.Create(SErrLibNotLoaded, 0);
end;

initialization
  pcre.LibNotLoadedHandler := LibNotLoadedHandler;
  LoadPCRE;

finalization
  UnloadPCRE;

// History:

// $Log$
// Revision 1.8  2005/02/24 16:34:40  marquardt
// remove divider lines, add section lines (unfinished)
//
// Revision 1.7  2004/11/09 07:53:07  rrossmair
// - JclPCRE string extracted to JclResources
//
// Revision 1.6  2004/11/06 02:20:20  rrossmair
// - better handling of calls into DLL when it got not loaded.
//
// Revision 1.5  2004/07/28 18:00:51  marquardt
// various style cleanings, some minor fixes
//
// Revision 1.4  2004/07/28 00:14:12  rrossmair
// fixed TJclAnsiRegEx.GetAPIOptions bug introduced in 1.3
//
// Revision 1.3  2004/07/27 06:42:23  marquardt
// style cleaning of pcre files
//
// Revision 1.2  2004/07/26 05:13:52  rrossmair
// made it compile under Kylix (no functional tests performed yet)
//  

end.

