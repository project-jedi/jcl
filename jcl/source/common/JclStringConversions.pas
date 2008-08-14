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
{ The Original Code is JclUnicode.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Mike Lischke (public att lischke-online dott de).  }
{ Portions created by Mike Lischke are Copyright (C) 1999-2000 Mike Lischke. All Rights Reserved.  }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel van Brakel                                                                              }
{   Andreas Hausladen (ahuser)                                                                     }
{   Mike Lischke                                                                                   }
{   Flier Lu (flier)                                                                               }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{   Peter Schraut (http://www.console-dev.de)                                                      }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ String conversion routines                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                       $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclStringConversions;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase;

type
  EJclStringConversionError = class(EJclError);
  EJclUnexpectedEOSequenceError = class (EJclStringConversionError)
  public
    constructor Create;
  end;

// conversion routines between Ansi, UTF-16, UCS-4 and UTF8 strings

{$IFNDEF CLR}
// one shot conversion between PAnsiChar and PWideChar
procedure ExpandANSIString(const Source: PChar; Target: PWideChar; Count: Cardinal);
{$ENDIF ~CLR}

// iterative conversions

// UTF8GetNextChar = read next UTF8 sequence at StrPos
// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF8 sequence)
// StrPos will be incremented by the number of chars that were read
function UTF8GetNextChar(const S: TUTF8String; var StrPos: Integer): UCS4;

// UTF8SkipChars = skip NbSeq UTF8 sequences starting from StrPos
// returns False if String is too small
// if UNICODE_SILENT_FAILURE is not defined StrPos is set to -1 on error (invalid UTF8 sequence)
// StrPos will be incremented by the number of chars that were skipped
// On return, NbSeq contains the number of UTF8 sequences that were skipped
function UTF8SkipChars(const S: TUTF8String; var StrPos: Integer; var NbSeq: Integer): Boolean;

// UTF8SetNextChar = append an UTF8 sequence at StrPos
// returns False on error:
//    - if an UCS4 character cannot be stored to an UTF-8 string:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags string being too small, callee did nothing, caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written
function UTF8SetNextChar(var S: TUTF8String; var StrPos: Integer; Ch: UCS4): Boolean;

// UTF16GetNextChar = read next UTF16 sequence at StrPos
// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF16 sequence)
// StrPos will be incremented by the number of chars that were read
function UTF16GetNextChar(const S: TUTF16String; var StrPos: Integer): UCS4;

// UTF16GetPreviousChar = read previous UTF16 sequence starting at StrPos-1
// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF16 sequence)
// StrPos will be decremented by the number of chars that were read
function UTF16GetPreviousChar(const S: TUTF16String; var StrPos: Integer): UCS4;

// UTF16SkipChars = skip NbSeq UTF16 sequences starting from StrPos
// returns False if String is too small
// if UNICODE_SILENT_FAILURE is not defined StrPos is set to -1 on error (invalid UTF16 sequence)
// StrPos will be incremented by the number of chars that were skipped
// On return, NbChar contains the number of UTF16 sequences that were skipped
function UTF16SkipChars(const S: TUTF16String; var StrPos: Integer; var NbSeq: Integer): Boolean;

// UTF16SetNextChar = append an UTF16 sequence at StrPos
// returns False on error:
//    - if an UCS4 character cannot be stored to an UTF-8 string:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags string being too small, callee did nothing and caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written
function UTF16SetNextChar(var S: TUTF16String; var StrPos: Integer; Ch: UCS4): Boolean;

// one shot conversions between WideString and others
function WideStringToUTF8(const S: WideString): TUTF8String; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function UTF8ToWideString(const S: TUTF8String): WideString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function WideStringToUCS4(const S: WideString): TUCS4Array; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function UCS4ToWideString(const S: TUCS4Array): WideString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}

// one shot conversions between AnsiString and others
function AnsiStringToUTF8(const S: AnsiString): TUTF8String; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function UTF8ToAnsiString(const S: TUTF8String): AnsiString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function AnsiStringToUTF16(const S: AnsiString): TUTF16String; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function UTF16ToAnsiString(const S: TUTF16String): AnsiString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function AnsiStringToUCS4(const S: AnsiString): TUCS4Array; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function UCS4ToAnsiString(const S: TUCS4Array): AnsiString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}

// one shot conversions between string and others
function StringToUTF8(const S: string): TUTF8String; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function UTF8ToString(const S: TUTF8String): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function StringToUTF16(const S: string): TUTF16String; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function UTF16ToString(const S: TUTF16String): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function StringToUCS4(const S: string): TUCS4Array; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function UCS4ToString(const S: TUCS4Array): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}

function UTF8ToUTF16(const S: TUTF8String): TUTF16String;
function UTF16ToUTF8(const S: TUTF16String): TUTF8String;
function UTF8ToUCS4(const S: TUTF8String): TUCS4Array;
function UCS4ToUTF8(const S: TUCS4Array): TUTF8String;
function UTF16ToUCS4(const S: TUTF16String): TUCS4Array;
function UCS4ToUTF16(const S: TUCS4Array): TUTF16String;

// indexed conversions
function UTF8CharCount(const S: TUTF8String): Integer;
function UTF16CharCount(const S: TUTF16String): Integer;
function UCS2CharCount(const S: TUCS2String): Integer;
function UCS4CharCount(const S: TUCS4Array): Integer;
// returns False if string is too small
// if UNICODE_SILENT_FAILURE is not defined and an invalid UTFX sequence is detected, an exception is raised
// returns True on success and Value contains UCS4 character that was read
function GetUCS4CharAt(const UTF8Str: TUTF8String; Index: Integer; out Value: UCS4): Boolean; overload;
function GetUCS4CharAt(const WideStr: TUTF16String; Index: Integer; out Value: UCS4; IsUTF16: Boolean = True): Boolean; overload;
function GetUCS4CharAt(const UCS4Str: TUCS4Array; Index: Integer; out Value: UCS4): Boolean; overload;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\windows'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclResources;

constructor EJclUnexpectedEOSequenceError.Create;
begin
  {$IFDEF CLR}
  inherited Create(RsEUnexpectedEOSeq);
  {$ELSE ~CLR}
  inherited CreateRes(@RsEUnexpectedEOSeq);
  {$ENDIF ~CLR}
end;
  
//----------------- conversion routines ------------------------------------------------------------

// Converts the given source ANSI string into a Unicode string by expanding each character
// from one byte to two bytes.
// EAX contains Source, EDX contains Target, ECX contains Count

{$IFNDEF CLR}
procedure ExpandANSIString(const Source: PChar; Target: PWideChar; Count: Cardinal);
// Source in EAX
// Target in EDX
// Count in ECX
asm
       JECXZ   @@Finish           // go out if there is nothing to do (ECX = 0)
       PUSH    ESI
       MOV     ESI, EAX
       XOR     EAX, EAX
@@1:
       MOV     AL, [ESI]
       INC     ESI
       MOV     [EDX], AX
       ADD     EDX, 2
       DEC     ECX
       JNZ     @@1
       POP     ESI
@@Finish:
end;
{$ENDIF ~CLR}

const
  HalfShift: Integer = 10;

  HalfBase: UCS4 = $0010000;
  HalfMask: UCS4 = $3FF;

  OffsetsFromUTF8: array [0..5] of UCS4 =
    ($00000000, $00003080, $000E2080,
     $03C82080, $FA082080, $82082080);

  BytesFromUTF8: array [0..255] of Byte =
   (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5);

  FirstByteMark: array [0..6] of Byte =
    ($00, $00, $C0, $E0, $F0, $F8, $FC);

procedure FlagInvalidSequence(var StrPos: Integer; Increment: Integer; var Ch: UCS4); overload;
begin
  {$IFDEF UNICODE_SILENT_FAILURE}
  Ch := UCS4ReplacementCharacter;
  Inc(StrPos, Increment);
  {$ELSE ~UNICODE_SILENT_FAILURE}
  StrPos := -1;
  {$ENDIF ~UNICODE_SILENT_FAILURE}
end;

procedure FlagInvalidSequence(var StrPos: Integer; Increment: Integer); overload;
begin
  {$IFDEF UNICODE_SILENT_FAILURE}
  Inc(StrPos, Increment);
  {$ELSE ~UNICODE_SILENT_FAILURE}
  StrPos := -1;
  {$ENDIF ~UNICODE_SILENT_FAILURE}
end;

// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF8 sequence)
// StrPos will be incremented by the number of chars that were read
function UTF8GetNextChar(const S: TUTF8String; var StrPos: Integer): UCS4;
var
  StrLength: Integer;
  ChNext: UCS4;
begin
  StrLength := Length(S);

  if (StrPos <= StrLength) and (StrPos > 0) then
  begin
    Result := UCS4(S[StrPos]);

    case Result of
      $00..$7F:
        // 1 byte to read
        Inc(StrPos);
      $C0..$DF:
        begin
          // 2 bytes to read
          if StrPos >= StrLength then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          ChNext := UCS4(S[StrPos + 1]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          Result := ((Result and $1F) shl 6) or (ChNext and $3F);
          Inc(StrPos, 2);
        end;
      $E0..$EF:
        begin
          // 3 bytes to read
          if (StrPos + 1) >= StrLength then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          ChNext := UCS4(S[StrPos + 1]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          Result := ((Result and $0F) shl 12) or ((ChNext and $3F) shl 6);
          ChNext := UCS4(S[StrPos + 2]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 2, Result);
            Exit;
          end;
          Result := Result or (ChNext and $3F);
          Inc(StrPos, 3);
        end;
      $F0..$F7:
        begin
          // 4 bytes to read
          if (StrPos + 2) >= StrLength then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          ChNext := UCS4(S[StrPos + 1]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          Result := ((Result and $07) shl 18) or ((ChNext and $3F) shl 12);
          ChNext := UCS4(S[StrPos + 2]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 2, Result);
            Exit;
          end;
          Result := Result or ((ChNext and $3F) shl 6);
          ChNext := UCS4(S[StrPos + 3]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 3, Result);
            Exit;
          end;
          Result := Result or (ChNext and $3F);
          Inc(StrPos, 4);
        end;
      $F8..$FB:
        begin
          // 5 bytes to read
          if (StrPos + 3) >= StrLength then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          ChNext := UCS4(S[StrPos + 1]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          Result := ((Result and $03) shl 24) or ((ChNext and $3F) shl 18);
          ChNext := UCS4(S[StrPos + 2]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 2, Result);
            Exit;
          end;
          Result := Result or ((ChNext and $3F) shl 12);
          ChNext := UCS4(S[StrPos + 3]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 3, Result);
            Exit;
          end;
          Result := Result or ((ChNext and $3F) shl 6);
          ChNext := UCS4(S[StrPos + 4]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 4, Result);
            Exit;
          end;
          Result := Result or (ChNext and $3F);
          Inc(StrPos, 5);
        end;
      $FC..$FD:
        begin
          // 6 bytes to read
          if (StrPos + 4) >= StrLength then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          ChNext := UCS4(S[StrPos + 1]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          Result := ((Result and $01) shl 30) or ((ChNext and $3F) shl 24);
          ChNext := UCS4(S[StrPos + 2]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 2, Result);
            Exit;
          end;
          Result := Result or ((ChNext and $3F) shl 18);
          ChNext := UCS4(S[StrPos + 3]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 3, Result);
            Exit;
          end;
          Result := Result or ((ChNext and $3F) shl 12);
          ChNext := UCS4(S[StrPos + 4]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 4, Result);
            Exit;
          end;
          Result := Result or ((ChNext and $3F) shl 6);
          ChNext := UCS4(S[StrPos + 5]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 5, Result);
            Exit;
          end;
          Result := Result or (ChNext and $3F);
          Inc(StrPos, 6);
        end;
    else
      FlagInvalidSequence(StrPos, 1, Result);
      Exit;
    end;
  end
  else
  begin
    // StrPos > StrLength
    Result := 0;
    FlagInvalidSequence(StrPos, 0, Result);
  end;
end;

// returns False if String is too small
// if UNICODE_SILENT_FAILURE is not defined StrPos is set to -1 on error (invalid UTF8 sequence)
// StrPos will be incremented by the number of ansi chars that were skipped
// On return, NbSeq contains the number of UTF8 sequences that were skipped
function UTF8SkipChars(const S: TUTF8String; var StrPos: Integer; var NbSeq: Integer): Boolean;
var
  StrLength: Integer;
  Ch: UCS4;
  Index: Integer;
begin
  Result := True;
  StrLength := Length(S);

  Index := 0;
  while (Index < NbSeq) and (StrPos > 0) do
  begin
    Ch := UCS4(S[StrPos]);

    case Ch of
      $00..$7F:
        // 1 byte to skip
        Inc(StrPos);
      $C0..$DF:
        // 2 bytes to skip
        if (StrPos >= StrLength) or ((UCS4(S[StrPos + 1]) and $C0) <> $80) then
          FlagInvalidSequence(StrPos, 1)
        else
          Inc(StrPos, 2);
      $E0..$EF:
        // 3 bytes to skip
        if ((StrPos + 1) >= StrLength) or ((UCS4(S[StrPos + 1]) and $C0) <> $80) then
          FlagInvalidSequence(StrPos, 1)
        else
        if (UCS4(S[StrPos + 2]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 2)
        else
          Inc(StrPos, 3);
      $F0..$F7:
        // 4 bytes to skip
        if ((StrPos + 2) >= StrLength) or ((UCS4(S[StrPos + 1]) and $C0) <> $80) then
          FlagInvalidSequence(StrPos, 1)
        else
        if (UCS4(S[StrPos + 2]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 2)
        else
        if (UCS4(S[StrPos + 3]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 3)
        else
          Inc(StrPos, 4);
      $F8..$FB:
        // 5 bytes to skip
        if ((StrPos + 3) >= StrLength) or ((UCS4(S[StrPos + 1]) and $C0) <> $80) then
          FlagInvalidSequence(StrPos, 1)
        else
        if (UCS4(S[StrPos + 2]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 2)
        else
        if (UCS4(S[StrPos + 3]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 3)
        else
        if (UCS4(S[StrPos + 4]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 4)
        else
          Inc(StrPos, 5);
      $FC..$FD:
        // 6 bytes to skip
        if ((StrPos + 4) >= StrLength) or ((UCS4(S[StrPos + 1]) and $C0) <> $80) then
          FlagInvalidSequence(StrPos, 1)
        else
        if (UCS4(S[StrPos + 2]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 2)
        else
        if (UCS4(S[StrPos + 3]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 3)
        else
        if (UCS4(S[StrPos + 4]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 4)
        else
        if (UCS4(S[StrPos + 5]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 5)
        else
          Inc(StrPos, 6);
    else
      FlagInvalidSequence(StrPos, 1);
    end;

    if StrPos <> -1 then
      Inc(Index);
    if (StrPos > StrLength) and (Index < NbSeq) then
    begin
      Result := False;
      Break;
    end;
  end;
  NbSeq := Index;
end;

// returns False on error:
//    - if an UCS4 character cannot be stored to an UTF-8 string:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags string being too small, caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written
function UTF8SetNextChar(var S: TUTF8String; var StrPos: Integer; Ch: UCS4): Boolean;
var
  StrLength: Integer;
begin
  StrLength := Length(S);

  if Ch <= $7F then
  begin
    // 7 bits to store
    Result := StrPos <= StrLength;
    if Result then
    begin
      S[StrPos] := AnsiChar(Ch);
      Inc(StrPos);
    end;
  end
  else
  if Ch <= $7FF then
  begin
    // 11 bits to store
    Result := StrPos < StrLength;
    if Result then
    begin
      S[StrPos] := AnsiChar($C0 or (Ch shr 6));  // 5 bits
      S[StrPos + 1] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 2);
    end;
  end
  else
  if Ch <= $FFFF then
  begin
    // 16 bits to store
    Result := StrPos < (StrLength - 1);
    if Result then
    begin
      S[StrPos] := AnsiChar($E0 or (Ch shr 12)); // 4 bits
      S[StrPos + 1] := AnsiChar(((Ch shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 3);
    end;
  end
  else
  if Ch <= $1FFFFF then
  begin
    // 21 bits to store
    Result := StrPos < (StrLength - 2);
    if Result then
    begin
      S[StrPos] := AnsiChar($F0 or (Ch shr 18)); // 3 bits
      S[StrPos + 1] := AnsiChar(((Ch shr 12) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar(((Ch shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 3] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 4);
    end;
  end
  else
  if Ch <= $3FFFFFF then
  begin
    // 26 bits to store
    Result := StrPos < (StrLength - 2);
    if Result then
    begin
      S[StrPos] := AnsiChar($F8 or (Ch shr 24)); // 2 bits
      S[StrPos + 1] := AnsiChar(((Ch shr 18) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar(((Ch shr 12) and $3F) or $80); // 6 bits
      S[StrPos + 3] := AnsiChar(((Ch shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 4] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 5);
    end;
  end
  else
  if Ch <= MaximumUCS4 then
  begin
    // 31 bits to store
    Result := StrPos < (StrLength - 3);
    if Result then
    begin
      S[StrPos] := AnsiChar($FC or (Ch shr 30)); // 1 bits
      S[StrPos + 1] := AnsiChar(((Ch shr 24) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar(((Ch shr 18) and $3F) or $80); // 6 bits
      S[StrPos + 3] := AnsiChar(((Ch shr 12) and $3F) or $80); // 6 bits
      S[StrPos + 4] := AnsiChar(((Ch shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 5] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 6);
    end;
  end
  else
  begin
    {$IFDEF UNICOLE_SILENT_FAILURE}
    // add ReplacementCharacter
    Result := StrPos < (StrLength - 1);
    if Result then
    begin
      S[StrPos] := AnsiChar($E0 or (ReplacementCharacter shr 12)); // 4 bits
      S[StrPos + 1] := AnsiChar(((ReplacementCharacter shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar((ReplacementCharacter and $3F) or $80); // 6 bits
      Inc(StrPos, 3);
    end;
    {$ELSE ~UNICODE_SILENT_FAILURE}
    StrPos := -1;
    Result := False;
    {$ENDIF ~UNICODE_SILENT_FAILURE}
  end;
end;

// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF16 sequence)
// StrPos will be incremented by the number of chars that were read
function UTF16GetNextChar(const S: TUTF16String; var StrPos: Integer): UCS4;
var
  StrLength: Integer;
  ChNext: UCS4;
begin
  StrLength := Length(S);

  if (StrPos <= StrLength) and (StrPos > 0) then
  begin
    Result := UCS4(S[StrPos]);

    case Result of
      SurrogateHighStart..SurrogateHighEnd:
        begin
          // 2 bytes to read
          if StrPos >= StrLength then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          ChNext := UCS4(S[StrPos + 1]);
          if (ChNext < SurrogateLowStart) or (ChNext > SurrogateLowEnd) then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          Result := ((Result - SurrogateHighStart) shl HalfShift) +  (ChNext - SurrogateLowStart) + HalfBase;
          Inc(StrPos, 2);
        end;
      SurrogateLowStart..SurrogateLowEnd:
        FlagInvalidSequence(StrPos, 1, Result);
    else
      // 1 byte to read
      Inc(StrPos);
    end;
  end
  else
  begin
    // StrPos > StrLength
    Result := 0;
    FlagInvalidSequence(StrPos, 0, Result);
  end;
end;

// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF16 sequence)
// StrPos will be decremented by the number of chars that were read
function UTF16GetPreviousChar(const S: TUTF16String; var StrPos: Integer): UCS4;
var
  StrLength: Integer;
  ChPrev: UCS4;
begin
  StrLength := Length(S);

  if (StrPos <= (StrLength + 1)) and (StrPos > 1) then
  begin
    Result := UCS4(S[StrPos - 1]);

    case Result of
      SurrogateHighStart..SurrogateHighEnd:
        FlagInvalidSequence(StrPos, -1, Result);
      SurrogateLowStart..SurrogateLowEnd:
        begin
          // 2 bytes to read
          if StrPos <= 2 then
          begin
            FlagInvalidSequence(StrPos, -1, Result);
            Exit;
          end;
          ChPrev := UCS4(S[StrPos - 2]);
          if (ChPrev < SurrogateHighStart) or (ChPrev > SurrogateHighEnd) then
          begin
            FlagInvalidSequence(StrPos, -1, Result);
            Exit;
          end;
          Result := ((ChPrev - SurrogateHighStart) shl HalfShift) +  (Result - SurrogateLowStart) + HalfBase;
          Dec(StrPos, 2);
        end;
    else
      // 1 byte to read
      Dec(StrPos);
    end;
  end
  else
  begin
    // StrPos > StrLength
    Result := 0;
    FlagInvalidSequence(StrPos, 0, Result);
  end;
end;

// returns False if String is too small
// if UNICODE_SILENT_FAILURE is not defined StrPos is set to -1 on error (invalid UTF16 sequence)
// StrPos will be incremented by the number of chars that were skipped
// On return, NbSeq contains the number of UTF16 sequences that were skipped
function UTF16SkipChars(const S: TUTF16String; var StrPos: Integer; var NbSeq: Integer): Boolean;
var
  StrLength, Index: Integer;
  Ch, ChNext: UCS4;
begin
  Result := True;
  StrLength := Length(S);

  Index := 0;
  if NbSeq >= 0 then
    while (Index < NbSeq) and (StrPos > 0) do
    begin
      Ch := UCS4(S[StrPos]);
  
      case Ch of
        SurrogateHighStart..SurrogateHighEnd:
          // 2 bytes to skip
          if StrPos >= StrLength then
            FlagInvalidSequence(StrPos, 1)
          else
          begin
            ChNext := UCS4(S[StrPos + 1]);
            if (ChNext < SurrogateLowStart) or (ChNext > SurrogateLowEnd) then
              FlagInvalidSequence(StrPos, 1)
            else
              Inc(StrPos, 2);
          end;
        SurrogateLowStart..SurrogateLowEnd:
          // error
          FlagInvalidSequence(StrPos, 1);
      else
        // 1 byte to skip
        Inc(StrPos);
      end;

      if StrPos <> -1 then
        Inc(Index);

      if (StrPos > StrLength) and (Index < NbSeq) then
      begin
        Result := False;
        Break;
      end;
    end
  else
    while (Index > NbSeq) and (StrPos > 1) do
    begin
      Ch := UCS4(S[StrPos - 1]);

      case Ch of
        SurrogateHighStart..SurrogateHighEnd:
          // error
          FlagInvalidSequence(StrPos, -1);
        SurrogateLowStart..SurrogateLowEnd:
          // 2 bytes to skip
          if StrPos <= 2 then
            FlagInvalidSequence(StrPos, -1)
          else
          begin
            ChNext := UCS4(S[StrPos - 2]);
            if (ChNext < SurrogateHighStart) or (ChNext > SurrogateHighEnd) then
              FlagInvalidSequence(StrPos, -1)
            else
              Dec(StrPos, 2);
          end;
      else
        // 1 byte to skip
        Dec(StrPos);
      end;

      if StrPos <> -1 then
        Dec(Index);

      if (StrPos = 1) and (Index > NbSeq) then
      begin
        Result := False;
        Break;
      end;
    end;
  NbSeq := Index;
end;

// returns False on error:
//    - if an UCS4 character cannot be stored to an UTF-8 string:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags string being too small, caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written
function UTF16SetNextChar(var S: TUTF16String; var StrPos: Integer; Ch: UCS4): Boolean;
var
  StrLength: Integer;
begin
  StrLength := Length(S);

  if Ch <= MaximumUCS2 then
  begin
    // 16 bits to store in place
    Result := StrPos <= StrLength;
    if Result then
    begin
      S[StrPos] := WideChar(Ch);
      Inc(StrPos);
    end;
  end
  else
  if Ch <= MaximumUTF16 then
  begin
    // stores a surrogate pair
    Result := StrPos < StrLength;
    if Result then
    begin
      Ch := Ch - HalfBase;
      S[StrPos] := WideChar((Ch shr HalfShift) + SurrogateHighStart);
      S[StrPos + 1] := WideChar((Ch and HalfMask) + SurrogateLowStart);
      Inc(StrPos, 2);
    end;
  end
  else
  begin
    {$IFDEF UNICOLE_SILENT_FAILURE}
    // add ReplacementCharacter
    Result := StrPos <= StrLength;
    if Result then
    begin
      S[StrPos] := WideChar(ReplacementCharacter);
      Inc(StrPos, 1);
    end;
    {$ELSE ~UNICODE_SILENT_FAILURE}
    StrPos := -1;
    Result := False;
    {$ENDIF ~UNICODE_SILENT_FAILURE}
  end;
end;

function WideStringToUTF8(const S: WideString): TUTF8String;
begin
  Result := UTF16ToUTF8(S);
end;

function UTF8ToWideString(const S: TUTF8String): WideString;
begin
  Result := UTF8ToWideString(S);
end;

function WideStringToUCS4(const S: WideString): TUCS4Array;
begin
  Result := UTF16ToUCS4(S);
end;

function UCS4ToWideString(const S: TUCS4Array): WideString;
begin
  Result := UCS4ToUTF16(S);
end;

function AnsiStringToUTF8(const S: AnsiString): TUTF8String;
var
  WS: TUTF16String;
begin
  WS := TUTF16String(S);
  Result := UTF16ToUTF8(WS);
end;

function UTF8ToAnsiString(const S: TUTF8String): AnsiString;
var
  WS: TUTF16String;
begin
  WS := UTF8ToUTF16(S);
  Result := AnsiString(WS);
end;

function AnsiStringToUTF16(const S: AnsiString): TUTF16String;
begin
  Result := TUTF16String(S);
end;

function UTF16ToAnsiString(const S: TUTF16String): AnsiString;
begin
  Result := AnsiString(S);
end;

function AnsiStringToUCS4(const S: AnsiString): TUCS4Array;
var
  WS: TUTF16String;
begin
  WS := TUTF16String(S);
  Result := UTF16ToUCS4(WS);
end;

function UCS4ToAnsiString(const S: TUCS4Array): AnsiString;
var
  WS: TUTF16String;
begin
  WS := UCS4ToUTF16(S);
  Result := AnsiString(WS);
end;

function StringToUTF8(const S: string): TUTF8String;
var
  WS: TUTF16String;
begin
  WS := TUTF16String(S);
  Result := UTF16ToUTF8(WS);
end;

function UTF8ToString(const S: TUTF8String): string;
var
  WS: TUTF16String;
begin
  WS := UTF8ToUTF16(S);
  Result := string(WS);
end;

function StringToUTF16(const S: string): TUTF16String;
begin
  Result := TUTF16String(S);
end;

function UTF16ToString(const S: TUTF16String): string;
begin
  Result := string(S);
end;

function StringToUCS4(const S: string): TUCS4Array;
var
  WS: TUTF16String;
begin
  WS := TUTF16String(S);
  Result := UTF16ToUCS4(WS);
end;

function UCS4ToString(const S: TUCS4Array): string;
var
  WS: WideString;
begin
  WS := UCS4ToUTF16(S);
  Result := string(WS);
end;

function UTF8ToUTF16(const S: TUTF8String): TUTF16String;
var
  SrcIndex, SrcLength, DestIndex: Integer;
  Ch: UCS4;
begin
  if S = '' then
    Result := ''
  else
  begin
    SrcLength := Length(S);
    SetLength(Result, SrcLength); // create enough room

    SrcIndex := 1;
    DestIndex := 1;
    while SrcIndex <= SrcLength do
    begin
      Ch := UTF8GetNextChar(S, SrcIndex);
      if SrcIndex = -1 then
        raise EJclUnexpectedEOSequenceError.Create;

      UTF16SetNextChar(Result, DestIndex, Ch);
    end;
    SetLength(Result, DestIndex - 1); // now fix up length
  end;
end;

function UTF16ToUTF8(const S: TUTF16String): TUTF8String;
var
  SrcIndex, SrcLength, DestIndex: Integer;
  Ch: UCS4;
begin
  if S = '' then
    Result := ''
  else
  begin
    SrcLength := Length(S);
    SetLength(Result, SrcLength * 3); // worste case

    SrcIndex := 1;
    DestIndex := 1;
    while SrcIndex <= SrcLength do
    begin
      Ch := UTF16GetNextChar(S, SrcIndex);
      if SrcIndex = -1 then
        raise EJclUnexpectedEOSequenceError.Create;

      UTF8SetNextChar(Result, DestIndex, Ch);
    end;
    SetLength(Result, DestIndex - 1); // now fix up length
  end;
end;

function UTF8ToUCS4(const S: TUTF8String): TUCS4Array;
var
  SrcIndex, SrcLength, DestIndex: Integer;
  Ch: UCS4;
begin
  if S <> '' then
  begin
    SrcLength := Length(S);
    SetLength(Result, SrcLength); // create enough room

    SrcIndex := 1;
    DestIndex := 0;
    while SrcIndex <= SrcLength do
    begin
      Ch := UTF8GetNextChar(S, SrcIndex);
      if SrcIndex = -1 then
        raise EJclUnexpectedEOSequenceError.Create;

      Result[DestIndex] := Ch;
      Inc(DestIndex);
    end;
    SetLength(Result, DestIndex); // now fix up length
  end;
end;

function UCS4ToUTF8(const S: TUCS4Array): TUTF8String;
var
  SrcIndex, SrcLength, DestIndex: Integer;
begin
  SrcLength := Length(S);
  if Length(S) = 0 then
    Result := ''
  else
  begin
    SetLength(Result, SrcLength * 3); // assume worst case
    DestIndex := 1;

    for SrcIndex := 0 to SrcLength - 1 do
    begin
      UTF8SetNextChar(Result, DestIndex, S[SrcIndex]);
      if DestIndex = -1 then
        raise EJclUnexpectedEOSequenceError.Create;
    end;

    SetLength(Result, DestIndex - 1); // set to actual length
  end;
end;

function UTF16ToUCS4(const S: TUTF16String): TUCS4Array;
var
  SrcIndex, SrcLength, DestIndex: Integer;
  Ch: UCS4;
begin
  if S <> '' then
  begin
    SrcLength := Length(S);
    SetLength(Result, SrcLength); // create enough room

    SrcIndex := 1;
    DestIndex := 0;
    while SrcIndex <= SrcLength do
    begin
      Ch := UTF16GetNextChar(S, SrcIndex);
      if SrcIndex = -1 then
        raise EJclUnexpectedEOSequenceError.Create;

      Result[DestIndex] := Ch;
      Inc(DestIndex);
    end;
    SetLength(Result, DestIndex); // now fix up length
  end;
end;

function UCS4ToUTF16(const S: TUCS4Array): TUTF16String;
var
  SrcIndex, SrcLength, DestIndex: Integer;
begin
  SrcLength := Length(S);
  if SrcLength = 0 then
    Result := ''
  else
  begin
    SetLength(Result, SrcLength * 3); // assume worst case
    DestIndex := 1;

    for SrcIndex := 0 to SrcLength - 1 do
    begin
      UTF16SetNextChar(Result, DestIndex, S[SrcIndex]);
      if DestIndex = -1 then
        raise EJclUnexpectedEOSequenceError.Create;
    end;

    SetLength(Result, DestIndex - 1); // set to actual length
  end;
end;

function UTF8CharCount(const S: TUTF8String): Integer;
var
  StrPos: Integer;
begin
  StrPos := 1;
  Result := Length(S);
  UTF8SkipChars(S, StrPos, Result);
  if StrPos = -1 then
    raise EJclUnexpectedEOSequenceError.Create;
end;

function UTF16CharCount(const S: TUTF16String): Integer;
var
  StrPos: Integer;
begin
  StrPos := 1;
  Result := Length(S);
  UTF16SkipChars(S, StrPos, Result);
  if StrPos = -1 then
    raise EJclUnexpectedEOSequenceError.Create;
end;

function UCS2CharCount(const S: TUCS2String): Integer;
begin
  Result := Length(S);
end;

function UCS4CharCount(const S: TUCS4Array): Integer;
begin
  Result := Length(S);
end;

function GetUCS4CharAt(const UTF8Str: TUTF8String; Index: Integer; out Value: UCS4): Boolean; overload;
var
  StrPos: Integer;
begin
  StrPos := 1;
  Result := Index >= 0;
  if Result then
    Result := UTF8SkipChars(UTF8Str, StrPos, Index);
  if StrPos = -1 then
    raise EJclUnexpectedEOSequenceError.Create;
  Result := Result and (StrPos <= Length(UTF8Str));
  if Result then
  begin
    Value := UTF8GetNextChar(UTF8Str, StrPos);
    if StrPos = -1 then
      raise EJclUnexpectedEOSequenceError.Create;
  end;
end;

function GetUCS4CharAt(const WideStr: WideString; Index: Integer; out Value: UCS4; IsUTF16: Boolean): Boolean; overload;
var
  StrPos: Integer;
begin
  if IsUTF16 then
  begin
    StrPos := 1;
    Result := Index >= 0;
    if Result then
      Result := UTF16SkipChars(WideStr, StrPos, Index);
    if StrPos = -1 then
      raise EJclUnexpectedEOSequenceError.Create;
    Result := Result and (StrPos <= Length(WideStr));
    if Result then
    begin
      Value := UTF16GetNextChar(WideStr, StrPos);
      if StrPos = -1 then
        raise EJclUnexpectedEOSequenceError.Create;
    end;
  end
  else
  begin
    Result := (Index >= 1) and (Index <= Length(WideStr));
    Value := UCS4(WideStr[Index]);
  end;
end;

function GetUCS4CharAt(const UCS4Str: TUCS4Array; Index: Integer; out Value: UCS4): Boolean; overload;
begin
  Result := (Index >= 0) and (Index < Length(UCS4Str));
  if Result then
    Value := UCS4Str[Index];
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
