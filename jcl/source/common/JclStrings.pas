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
{ The Original Code is JclStrings.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright (C) Marcel van Brakel. All rights reserved.  }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Alexander Radchenko                                                                            }
{   Anthony Steele                                                                                 }
{   Azret Botash                                                                                   }
{   Barry Kelly                                                                                    }
{   Huanlin Tsai                                                                                   }
{   Jack N.A. Bakker                                                                               }
{   Jean-Fabien Connault                                                                           }
{   John C Molyneux                                                                                }
{   Leonard Wennekers                                                                              }
{   Martin Kimmings                                                                                }
{   Martin Kubecka                                                                                 }
{   Massimo Maria Ghisalberti                                                                      }
{   Matthias Thoma (mthoma)                                                                        }
{   Michael Winter                                                                                 }
{   Nick Hodges                                                                                    }
{   Olivier Sannier                                                                                }
{   Pelle F. S. Liljendal                                                                          }
{   Petr Vones                                                                                     }
{   Robert Lee                                                                                     }
{   Robert Marquardt                                                                               }
{   Robert Rossmair (rrossmair)                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Various character and string routines (searching, testing and transforming)                      }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclStrings;

{$I jcl.inc}

interface

uses
  Classes, SysUtils,
  JclBase, JclWideStrings;

// Character constants and sets

const
  // Misc. often used character definitions
  AnsiNull           = AnsiChar(#0);
  AnsiSoh            = AnsiChar(#1);
  AnsiStx            = AnsiChar(#2);
  AnsiEtx            = AnsiChar(#3);
  AnsiEot            = AnsiChar(#4);
  AnsiEnq            = AnsiChar(#5);
  AnsiAck            = AnsiChar(#6);
  AnsiBell           = AnsiChar(#7);
  AnsiBackspace      = AnsiChar(#8);
  AnsiTab            = AnsiChar(#9);
  AnsiLineFeed       = JclBase.AnsiLineFeed;
  AnsiVerticalTab    = AnsiChar(#11);
  AnsiFormFeed       = AnsiChar(#12);
  AnsiCarriageReturn = JclBase.AnsiCarriageReturn;
  AnsiCrLf           = JclBase.AnsiCrLf;
  AnsiSo             = AnsiChar(#14);
  AnsiSi             = AnsiChar(#15);
  AnsiDle            = AnsiChar(#16);
  AnsiDc1            = AnsiChar(#17);
  AnsiDc2            = AnsiChar(#18);
  AnsiDc3            = AnsiChar(#19);
  AnsiDc4            = AnsiChar(#20);
  AnsiNak            = AnsiChar(#21);
  AnsiSyn            = AnsiChar(#22);
  AnsiEtb            = AnsiChar(#23);
  AnsiCan            = AnsiChar(#24);
  AnsiEm             = AnsiChar(#25);
  AnsiEndOfFile      = AnsiChar(#26);
  AnsiEscape         = AnsiChar(#27);
  AnsiFs             = AnsiChar(#28);
  AnsiGs             = AnsiChar(#29);
  AnsiRs             = AnsiChar(#30);
  AnsiUs             = AnsiChar(#31);
  AnsiSpace          = AnsiChar(' ');
  AnsiComma          = AnsiChar(',');
  AnsiBackslash      = AnsiChar('\');
  AnsiForwardSlash   = AnsiChar('/');

  AnsiDoubleQuote = AnsiChar('"');
  AnsiSingleQuote = AnsiChar('''');

  AnsiLineBreak = JclBase.AnsiLineBreak;

// Misc. character sets

  AnsiSigns          = ['-', '+'];
  AnsiWhiteSpace     = [AnsiTab, AnsiLineFeed, AnsiVerticalTab, AnsiFormFeed,
    AnsiCarriageReturn, AnsiSpace];
  AnsiDecDigits      = ['0'..'9'];
  AnsiOctDigits      = ['0'..'7'];
  AnsiHexDigits      = ['0'..'9', 'A'..'F', 'a'..'f'];

const
  // CharType return values
  C1_UPPER  = $0001; // Uppercase
  C1_LOWER  = $0002; // Lowercase
  C1_DIGIT  = $0004; // Decimal digits
  C1_SPACE  = $0008; // Space characters
  C1_PUNCT  = $0010; // Punctuation
  C1_CNTRL  = $0020; // Control characters
  C1_BLANK  = $0040; // Blank characters
  C1_XDIGIT = $0080; // Hexadecimal digits
  C1_ALPHA  = $0100; // Any linguistic character: alphabetic, syllabary, or ideographic

  {$IFDEF MSWINDOWS}
  {$IFDEF SUPPORTS_EXTSYM}
  {$EXTERNALSYM C1_UPPER}
  {$EXTERNALSYM C1_LOWER}
  {$EXTERNALSYM C1_DIGIT}
  {$EXTERNALSYM C1_SPACE}
  {$EXTERNALSYM C1_PUNCT}
  {$EXTERNALSYM C1_CNTRL}
  {$EXTERNALSYM C1_BLANK}
  {$EXTERNALSYM C1_XDIGIT}
  {$EXTERNALSYM C1_ALPHA}
  {$ENDIF SUPPORTS_EXTSYM}
  {$ENDIF MSWINDOWS}

// String Test Routines
function StrIsAlpha(const S: AnsiString): Boolean;
function StrIsAlphaNum(const S: AnsiString): Boolean;
function StrIsAlphaNumUnderscore(const S: AnsiString): Boolean;
function StrContainsChars(const S: AnsiString; Chars: TSysCharSet; CheckAll: Boolean): Boolean;
function StrConsistsOfNumberChars(const S: AnsiString): Boolean;
function StrIsDigit(const S: AnsiString): Boolean;
function StrIsSubset(const S: AnsiString; const ValidChars: TSysCharSet): Boolean;
function StrSame(const S1, S2: AnsiString): Boolean;

// String Transformation Routines
function StrCenter(const S: AnsiString; L: Integer; C: AnsiChar  = ' '): AnsiString;
function StrCharPosLower(const S: AnsiString; CharPos: Integer): AnsiString;
function StrCharPosUpper(const S: AnsiString; CharPos: Integer): AnsiString;
function StrDoubleQuote(const S: AnsiString): AnsiString;
function StrEnsureNoPrefix(const Prefix, Text: AnsiString): AnsiString;
function StrEnsureNoSuffix(const Suffix, Text: AnsiString): AnsiString;
function StrEnsurePrefix(const Prefix, Text: AnsiString): AnsiString;
function StrEnsureSuffix(const Suffix, Text: AnsiString): AnsiString;
function StrEscapedToString(const S: AnsiString): AnsiString;
function StrLower(const S: AnsiString): AnsiString;
procedure StrLowerInPlace(var S: AnsiString);
procedure StrLowerBuff(S: PAnsiChar);
procedure StrMove(var Dest: AnsiString; const Source: AnsiString; const ToIndex,
  FromIndex, Count: Integer);
function StrPadLeft(const S: AnsiString; Len: Integer; C: AnsiChar = AnsiSpace ): AnsiString;
function StrPadRight(const S: AnsiString; Len: Integer; C: AnsiChar = AnsiSpace ): AnsiString;
function StrProper(const S: AnsiString): AnsiString;
procedure StrProperBuff(S: PAnsiChar);
function StrQuote(const S: AnsiString; C: AnsiChar): AnsiString;
function StrRemoveChars(const S: AnsiString; const Chars: TSysCharSet): AnsiString;
function StrKeepChars(const S: AnsiString; const Chars: TSysCharSet): AnsiString;
procedure StrReplace(var S: AnsiString; const Search, Replace: AnsiString; Flags: TReplaceFlags = []);
function StrReplaceChar(const S: AnsiString; const Source, Replace: Char): AnsiString;
function StrReplaceChars(const S: AnsiString; const Chars: TSysCharSet; Replace: Char): AnsiString;
function StrReplaceButChars(const S: AnsiString; const Chars: TSysCharSet; Replace: Char): AnsiString;
function StrRepeat(const S: AnsiString; Count: Integer): AnsiString;
function StrRepeatLength(const S: AnsiString; Const L: Integer): AnsiString;
function StrReverse(const S: AnsiString): AnsiString;
procedure StrReverseInPlace(var S: AnsiString);
function StrSingleQuote(const S: AnsiString): AnsiString;
function StrSmartCase(const S: AnsiString; Delimiters: TSysCharSet): AnsiString;
function StrStringToEscaped(const S: AnsiString): AnsiString;
function StrStripNonNumberChars(const S: AnsiString): AnsiString;
function StrToHex(const Source: AnsiString): AnsiString;
function StrTrimCharLeft(const S: AnsiString; C: AnsiChar): AnsiString;
function StrTrimCharsLeft(const S: AnsiString; const Chars: TSysCharSet): AnsiString;
function StrTrimCharRight(const S: AnsiString; C: AnsiChar): AnsiString;
function StrTrimCharsRight(const S: AnsiString; const Chars: TSysCharSet): AnsiString;
function StrTrimQuotes(const S: AnsiString): AnsiString;
function StrUpper(const S: AnsiString): AnsiString;
procedure StrUpperInPlace(var S: AnsiString);
procedure StrUpperBuff(S: PAnsiChar);
{$IFDEF WIN32}
function StrOemToAnsi(const S: AnsiString): AnsiString;
function StrAnsiToOem(const S: AnsiString): AnsiString;
{$ENDIF WIN32}

// String Management
procedure StrAddRef(var S: AnsiString);
function StrAllocSize(const S: AnsiString): Longint;
procedure StrDecRef(var S: AnsiString);
function StrLen(S: PChar): Integer;
function StrLength(const S: AnsiString): Longint;
function StrRefCount(const S: AnsiString): Longint;
procedure StrResetLength(var S: AnsiString);

// String Search and Replace Routines
function StrCharCount(const S: AnsiString; C: AnsiChar): Integer;
function StrCharsCount(const S: AnsiString; Chars: TSysCharSet): Integer;
function StrStrCount(const S, SubS: AnsiString): Integer;
function StrCompare(const S1, S2: AnsiString): Integer;
function StrCompareRange(const S1, S2: AnsiString; const Index, Count: Integer): Integer;
function StrFillChar(const C: AnsiChar; const Count: Integer): AnsiString;
function StrFind(const Substr, S: AnsiString; const Index: Integer = 1): Integer;
function StrHasPrefix(const S: AnsiString; const Prefixes: array of string): Boolean;
function StrIndex(const S: AnsiString; const List: array of AnsiString): Integer;
function StrILastPos(const SubStr, S: AnsiString): Integer;
function StrIPos(const SubStr, S: AnsiString): Integer;
function StrIsOneOf(const S: AnsiString; const List: array of AnsiString): Boolean;
function StrLastPos(const SubStr, S: AnsiString): Integer;
function StrMatch(const Substr, S: AnsiString; const Index: Integer = 1): Integer;
function StrMatches(const Substr, S: AnsiString; const Index: Integer = 1): Boolean;
function StrNIPos(const S, SubStr: AnsiString; N: Integer): Integer;
function StrNPos(const S, SubStr: AnsiString; N: Integer): Integer;
function StrPrefixIndex(const S: AnsiString; const Prefixes: array of string): Integer;
function StrSearch(const Substr, S: AnsiString; const Index: Integer = 1): Integer;

// String Extraction
function StrAfter(const SubStr, S: AnsiString): AnsiString;
function StrBefore(const SubStr, S: AnsiString): AnsiString;
function StrBetween(const S: AnsiString; const Start, Stop: AnsiChar): AnsiString;
function StrChopRight(const S: AnsiString; N: Integer): AnsiString;
function StrLeft(const S: AnsiString; Count: Integer): AnsiString;
function StrMid(const S: AnsiString; Start, Count: Integer): AnsiString;
function StrRestOf(const S: AnsiString; N: Integer): AnsiString;
function StrRight(const S: AnsiString; Count: Integer): AnsiString;

// Character Test Routines
function CharEqualNoCase(const C1, C2: AnsiChar): Boolean;
function CharIsAlpha(const C: AnsiChar): Boolean;
function CharIsAlphaNum(const C: AnsiChar): Boolean;
function CharIsBlank(const C: AnsiChar): Boolean;
function CharIsControl(const C: AnsiChar): Boolean;
function CharIsDelete(const C: AnsiChar): Boolean;
function CharIsDigit(const C: AnsiChar): Boolean;
function CharIsLower(const C: AnsiChar): Boolean;
function CharIsNumberChar(const C: AnsiChar): Boolean;
function CharIsPrintable(const C: AnsiChar): Boolean;
function CharIsPunctuation(const C: AnsiChar): Boolean;
function CharIsReturn(const C: AnsiChar): Boolean;
function CharIsSpace(const C: AnsiChar): Boolean;
function CharIsUpper(const C: AnsiChar): Boolean;
function CharIsWhiteSpace(const C: AnsiChar): Boolean;
function CharType(const C: AnsiChar): Word;

// Character Transformation Routines
function CharHex(const C: AnsiChar): Byte;
function CharLower(const C: AnsiChar): AnsiChar;
function CharUpper(const C: AnsiChar): AnsiChar;
function CharToggleCase(const C: AnsiChar): AnsiChar;

// Character Search and Replace
function CharPos(const S: AnsiString; const C: AnsiChar; const Index: Integer = 1): Integer;
function CharLastPos(const S: AnsiString; const C: AnsiChar; const Index: Integer = 1): Integer;
function CharIPos(const S: AnsiString; const C: AnsiChar; const Index: Integer = 1 ): Integer;
function CharReplace(var S: AnsiString; const Search, Replace: AnsiChar): Integer;

// PCharVector
type
  PCharVector = ^PChar;

function StringsToPCharVector(var Dest: PCharVector; const Source: TStrings): PCharVector;
function PCharVectorCount(Source: PCharVector): Integer;
procedure PCharVectorToStrings(const Dest: TStrings; Source: PCharVector);
procedure FreePCharVector(var Dest: PCharVector);

// MultiSz Routines
type
  PMultiSz = PChar;
  PWideMultiSz = PWideChar;

function StringsToMultiSz(var Dest: PMultiSz; const Source: TStrings): PMultiSz;
procedure MultiSzToStrings(const Dest: TStrings; const Source: PMultiSz);
function MultiSzLength(const Source: PMultiSz): Integer;
procedure AllocateMultiSz(var Dest: PMultiSz; Len: Integer);
procedure FreeMultiSz(var Dest: PMultiSz);
function MultiSzDup(const Source: PMultiSz): PMultiSz;

function WideStringsToWideMultiSz(var Dest: PWideMultiSz; const Source: TWideStrings): PWideMultiSz;
procedure WideMultiSzToWideStrings(const Dest: TWideStrings; const Source: PWideMultiSz);
function WideMultiSzLength(const Source: PWideMultiSz): Integer;
procedure AllocateWideMultiSz(var Dest: PWideMultiSz; Len: Integer);
procedure FreeWideMultiSz(var Dest: PWideMultiSz);
function WideMultiSzDup(const Source: PWideMultiSz): PWideMultiSz;

// TStrings Manipulation
procedure StrIToStrings(S, Sep: AnsiString; const List: TStrings; const AllowEmptyString: Boolean = True);
procedure StrToStrings(S, Sep: AnsiString; const List: TStrings; const AllowEmptyString: Boolean = True);
function StringsToStr(const List: TStrings; const Sep: AnsiString; const AllowEmptyString: Boolean = True): AnsiString;
procedure TrimStrings(const List: TStrings; DeleteIfEmpty: Boolean = True );
procedure TrimStringsRight(const List: TStrings; DeleteIfEmpty: Boolean = True);
procedure TrimStringsLeft(const List: TStrings; DeleteIfEmpty: Boolean = True );
function AddStringToStrings(const S: string; Strings: TStrings; const Unique: Boolean): Boolean;

// Miscellaneous
function BooleanToStr(B: Boolean): AnsiString;
function FileToString(const FileName: AnsiString): AnsiString;
procedure StringToFile(const FileName, Contents: AnsiString);
function StrToken(var S: AnsiString; Separator: AnsiChar): AnsiString;
procedure StrTokens(const S: AnsiString; const List: TStrings);
procedure StrTokenToStrings(S: AnsiString; Separator: AnsiChar; const List: TStrings);
function StrWord(var S: PAnsiChar; out Word: AnsiString): Boolean;
function StrToFloatSafe(const S: AnsiString): Float;
function StrToIntSafe(const S: AnsiString): Integer;
procedure StrNormIndex(const StrLen: integer; var Index: integer; var Count: integer); overload;

// Exceptions
type
  EJclStringError = EJclError;

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  JclSysUtils, JclLogic, JclResources;

//=== Internal ===============================================================

type
  TAnsiStrRec = packed record
    AllocSize: Longint;
    RefCount: Longint;
    Length: Longint;
  end;

const
  AnsiStrRecSize  = SizeOf(TAnsiStrRec);     // size of the AnsiString header rec
  AnsiCharCount   = Ord(High(AnsiChar)) + 1; // # of chars in one set
  AnsiLoOffset    = AnsiCharCount * 0;       // offset to lower case chars
  AnsiUpOffset    = AnsiCharCount * 1;       // offset to upper case chars
  AnsiReOffset    = AnsiCharCount * 2;       // offset to reverse case chars
  AnsiAlOffset    = 12;                      // offset to AllocSize in StrRec
  AnsiRfOffset    = 8;                       // offset to RefCount in StrRec
  AnsiLnOffset    = 4;                       // offset to Length in StrRec
  AnsiCaseMapSize = AnsiCharCount * 3;       // # of chars is a table

var
  AnsiCaseMap: array [0..AnsiCaseMapSize - 1] of AnsiChar; // case mappings
  AnsiCaseMapReady: Boolean = False;         // true if case map exists
  AnsiCharTypes: array [AnsiChar] of Word;

procedure LoadCharTypes;
var
  CurrChar: AnsiChar;
  CurrType: Word;
begin
  for CurrChar := Low(AnsiChar) to High(AnsiChar) do
  begin
    {$IFDEF MSWINDOWS}
    GetStringTypeExA(LOCALE_USER_DEFAULT, CT_CTYPE1, @CurrChar, SizeOf(AnsiChar), CurrType);
    {$DEFINE CHAR_TYPES_INITIALIZED}
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    CurrType := 0;
    if isupper(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_UPPER;
    if islower(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_LOWER;
    if isdigit(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_DIGIT;
    if isspace(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_SPACE;
    if ispunct(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_PUNCT;
    if iscntrl(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_CNTRL;
    if isblank(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_BLANK;
    if isxdigit(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_XDIGIT;
    if isalpha(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_ALPHA;
    {$DEFINE CHAR_TYPES_INITIALIZED}
    {$ENDIF LINUX}
    AnsiCharTypes[CurrChar] := CurrType;
    {$IFNDEF CHAR_TYPES_INITIALIZED}
    Implement case map initialization here
    {$ENDIF ~CHAR_TYPES_INITIALIZED}
  end;
end;

procedure LoadCaseMap;
var
  CurrChar, UpCaseChar, LoCaseChar, ReCaseChar: AnsiChar;
begin
  if not AnsiCaseMapReady then
  begin
    for CurrChar := Low(AnsiChar) to High(AnsiChar) do
    begin
      {$IFDEF MSWINDOWS}
      LoCaseChar := CurrChar;
      UpCaseChar := CurrChar;
      Windows.CharLowerBuff(@LoCaseChar, 1);
      Windows.CharUpperBuff(@UpCaseChar, 1);
      {$DEFINE CASE_MAP_INITIALIZED}
      {$ENDIF MSWINDOWS}
      {$IFDEF LINUX}
      LoCaseChar := AnsiChar(tolower(Byte(CurrChar)));
      UpCaseChar := AnsiChar(toupper(Byte(CurrChar)));
      {$DEFINE CASE_MAP_INITIALIZED}
      {$ENDIF LINUX}
      {$IFNDEF CASE_MAP_INITIALIZED}
      Implement case map initialization here
      {$ENDIF ~CASE_MAP_INITIALIZED}
      if CharIsUpper(CurrChar) then
        ReCaseChar := LoCaseChar
      else
        if CharIsLower(CurrChar) then
          ReCaseChar := UpCaseChar
        else
          ReCaseChar := CurrChar;
      AnsiCaseMap[Ord(CurrChar) + AnsiLoOffset] := LoCaseChar;
      AnsiCaseMap[Ord(CurrChar) + AnsiUpOffset] := UpCaseChar;
      AnsiCaseMap[Ord(CurrChar) + AnsiReOffset] := ReCaseChar;
    end;
    AnsiCaseMapReady := True;
  end;
end;

// Uppercases or Lowercases a give AnsiString depending on the
// passed offset. (UpOffset or LoOffset)

procedure StrCase(var Str: AnsiString; const Offset: Integer); register; assembler;
asm
        // make sure that the string is not null

        TEST    EAX, EAX
        JZ      @@StrIsNull

        // create unique string if this one is ref-counted

        PUSH    EDX
        CALL    UniqueString
        POP     EDX

        // make sure that the new string is not null

        TEST    EAX, EAX
        JZ      @@StrIsNull

        // get the length, and prepare the counter

        MOV     ECX, [EAX - AnsiStrRecSize].TAnsiStrRec.Length
        DEC     ECX
        JS      @@StrIsNull

        // ebx will hold the case map, esi pointer to Str

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        // load case map and prepare variables }

        {$IFDEF PIC}
        LEA     EBX, [EBX][AnsiCaseMap + EDX]
        {$ELSE}
        LEA     EBX, [AnsiCaseMap + EDX]
        {$ENDIF PIC}
        MOV     ESI, EAX
        XOR     EDX, EDX
        XOR     EAX, EAX

@@NextChar:
        // get current char from the AnsiString

        MOV     DL, [ESI]

        // get corresponding char from the case map

        MOV     AL, [EBX + EDX]

        // store it back in the string

        MOV     [ESI], AL

        // update the loop counter and check the end of stirng

        DEC     ECX
        JL      @@Done

        // do the same thing with next 3 chars

        MOV     DL, [ESI + 1]
        MOV     AL, [EBX + EDX]
        MOV     [ESI + 1], AL

        DEC     ECX
        JL      @@Done
        MOV     DL, [ESI + 2]
        MOV     AL, [EBX+EDX]
        MOV     [ESI + 2], AL

        DEC     ECX
        JL      @@Done
        MOV     DL, [ESI + 3]
        MOV     AL, [EBX + EDX]
        MOV     [ESI + 3], AL

        // point AnsiString to next 4 chars

        ADD     ESI, 4

        // update the loop counter and check the end of stirng

        DEC     ECX
        JGE     @@NextChar

@@Done:
        POP     EDI
        POP     ESI
        POP     EBX

@@StrIsNull:
end;

// Internal utility function
// Uppercases or Lowercases a give null terminated string depending on the
// passed offset. (UpOffset or LoOffset)

procedure StrCaseBuff(S: PAnsiChar; const Offset: Integer); register; assembler;
asm
        // make sure the string is not null

        TEST    EAX, EAX
        JZ      @@StrIsNull

        // ebx will hold the case map, esi pointer to Str

        PUSH    EBX
        PUSH    ESI

        // load case map and prepare variables

        {$IFDEF PIC}
        LEA     EBX, [EBX][AnsiCaseMap + EDX]
        {$ELSE}
        LEA     EBX, [AnsiCaseMap + EDX]
        {$ENDIF PIC}
        MOV     ESI, EAX
        XOR     EDX, EDX
        XOR     EAX, EAX

@@NextChar:
        // get current char from the string

        MOV     DL, [ESI]

        // check for null char

        TEST    DL, DL
        JZ      @@Done

        // get corresponding char from the case map

        MOV     AL, [EBX + EDX]

        // store it back in the string

        MOV     [ESI], AL

        // do the same thing with next 3 chars

        MOV     DL, [ESI + 1]
        TEST    DL, DL
        JZ      @@Done
        MOV     AL, [EBX+EDX]
        MOV     [ESI + 1], AL

        MOV     DL, [ESI + 2]
        TEST    DL, DL
        JZ      @@Done
        MOV     AL, [EBX+EDX]
        MOV     [ESI + 2], AL

        MOV     DL, [ESI + 3]
        TEST    DL, DL
        JZ      @@Done
        MOV     AL, [EBX+EDX]
        MOV     [ESI + 3], AL

        // point string to next 4 chars

        ADD     ESI, 4
        JMP     @@NextChar

@@Done:
        POP     ESI
        POP     EBX

@@StrIsNull:
end;

function StrEndW(Str: PWideChar): PWideChar;
// returns a pointer to the end of a null terminated string
// stolen from JclUnicode
asm
       MOV     EDX, EDI
       MOV     EDI, EAX
       MOV     ECX, 0FFFFFFFFH
       XOR     AX, AX
       REPNE   SCASW
       LEA     EAX, [EDI - 2]
       MOV     EDI, EDX
end;

// String Test Routines
function StrIsAlpha(const S: AnsiString): Boolean;
var
  I: Integer;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
  begin
    if not CharIsAlpha(S[I]) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function StrIsAlphaNum(const S: AnsiString): Boolean;
var
  I: Integer;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
  begin
    if not CharIsAlphaNum(S[I]) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function StrConsistsofNumberChars(const S: AnsiString): Boolean;
var
  I: Integer;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
  begin
    if not CharIsNumberChar(S[I]) then
    begin
      Result := False;
      Exit;
    end;
 end;
end;

function StrContainsChars(const S: AnsiString; Chars: TSysCharSet; CheckAll: Boolean): Boolean;
var
  I: Integer;
  C: Char;
begin
  Result := Chars = [];
  if not Result then
  begin
    if CheckAll then
    begin
      for I := 1 to Length(S) do
      begin
        C := S[I];
        if C in Chars then
        begin
          Chars := Chars - [C];
          if Chars = [] then
            Break;
        end;
      end;
      Result := (Chars = []);
    end
    else
    begin
      for I := 1 to Length(S) do
        if S[I] in Chars then
        begin
          Result := True;
          Break;
        end;
    end;
  end;
end;

function StrIsAlphaNumUnderscore(const S: AnsiString): Boolean;
var
  I: Integer;
  C: AnsiChar;
begin
  for i := 1 to Length(s) do
  begin
    C := S[I];

    if not (CharIsAlphaNum(C) or (C = '_')) then
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := True and (Length(S) > 0);
end;

function StrIsDigit(const S: AnsiString): Boolean;
var
  I: Integer;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
  begin
    if not CharIsDigit(S[I]) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function StrIsSubset(const S: AnsiString; const ValidChars: TSysCharSet): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(S) do
  begin
    if not (S[I] in ValidChars) then
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := True and (Length(S) > 0);
end;

function StrSame(const S1, S2: AnsiString): Boolean;
begin
  Result := StrCompare(S1, S2) = 0;
end;

//=== String Transformation Routines =========================================

function StrCenter(const S: AnsiString; L: Integer; C: AnsiChar  = ' '): AnsiString;
begin
  if Length(S) < L then
  begin
    Result := StringOfChar(C, (L - Length(S)) div 2) + S;
    Result := Result + StringOfChar(C, L - Length(Result));
  end
  else
    Result := S;
end;

function StrCharPosLower(const S: AnsiString; CharPos: Integer): AnsiString;
begin
  Result := S;
  if (CharPos > 0) and (CharPos <= Length(S)) then
    Result[CharPos] := JclStrings.CharLower(Result[CharPos]);
end;

function StrCharPosUpper(const S: AnsiString; CharPos: Integer): AnsiString;
begin
  Result := S;
  if (CharPos > 0) and (CharPos <= Length(S)) then
    Result[CharPos] := CharUpper(Result[CharPos]);
end;

function StrDoubleQuote(const S: AnsiString): AnsiString;
begin
  Result := AnsiDoubleQuote + S + AnsiDoubleQuote;
end;

function StrEnsureNoPrefix(const Prefix, Text: AnsiString): AnsiString;
var
  PrefixLen : Integer;
begin
  PrefixLen := Length(Prefix);
  if Copy(Text, 1, PrefixLen) = Prefix then
    Result := Copy(Text, PrefixLen + 1, Length(Text))
  else
    Result := Text;
end;

function StrEnsureNoSuffix(const Suffix, Text: AnsiString): AnsiString;
var
  SuffixLen : Integer;
  StrLength : Integer;
begin
  SuffixLen := Length(Suffix);
  StrLength := Length(Text);
  if Copy(Text, StrLength - SuffixLen + 1, SuffixLen) = Suffix then
    Result := Copy(Text, 1, StrLength - SuffixLen)
  else
    Result := Text;
end;

function StrEnsurePrefix(const Prefix, Text: AnsiString): AnsiString;
var
  PrefixLen: Integer;
begin
  PrefixLen := Length(Prefix);
  if Copy(Text, 1, PrefixLen) = Prefix then
    Result := Text
  else
    Result := Prefix + Text;
end;

function StrEnsureSuffix(const Suffix, Text: AnsiString): AnsiString;
var
  SuffixLen: Integer;
begin
  SuffixLen := Length(Suffix);
  if Copy(Text, Length(Text) - SuffixLen + 1, SuffixLen) = Suffix then
    Result := Text
  else
    Result := Text + Suffix;
end;

function StrEscapedToString(const S: AnsiString): AnsiString;
var
  I, Len, N, Val: Integer;

  procedure HandleHexEscapeSeq;
  const
    HexDigits = AnsiString('0123456789abcdefABCDEF');
  begin
    N := Pos(S[I + 1], HexDigits) - 1;
    if N < 0 then
      // '\x' without hex digit following is not escape sequence
      Result := Result + '\x'
    else
    begin
      Inc(I); // Jump over x
      if N >= 16 then
        N := N - 6;
      Val := N;
      // Same for second digit
      if I < Len then
      begin
        N := Pos(S[I + 1], HexDigits) - 1;
        if N >= 0 then
        begin
          Inc(I); // Jump over first digit
          if N >= 16 then
            N := N - 6;
          Val := Val * 16 + N;
        end;
      end;

      if val > 255 then
        raise EJclStringError.CreateResRec(@RsNumericConstantTooLarge);

      Result := Result + Chr(Val);
    end;
  end;

  procedure HandleOctEscapeSeq;
  const
    OctDigits = AnsiString('01234567');
  begin
    // first digit
    Val := Pos(S[I], OctDigits) - 1;
    if I < Len then
    begin
      N := Pos(S[I + 1], OctDigits) - 1;
      if N >= 0 then
      begin
        Inc(I);
        Val := Val * 8 + N;
      end;
      if I < Len then
      begin
        N := Pos(S[I + 1], OctDigits) - 1;
        if N >= 0 then
        begin
          Inc(I);
          Val := Val * 8 + N;
        end;
      end;
    end;

    if val > 255 then
      raise EJclStringError.CreateResRec(@RsNumericConstantTooLarge);

    Result := Result + Chr(Val);
  end;

begin
  Result := '';
  I := 1;
  Len := Length(S);
  while I <= Len do
  begin
    if not ((S[I] = '\') and (I < Len)) then
      Result := Result + S[I]
    else
    begin
      Inc(I); // Jump over escape character
      case S[I] of
        'a':
          Result := Result + AnsiBell;
        'b':
          Result := Result + AnsiBackspace;
        'f':
          Result := Result + AnsiFormFeed;
        'n':
          Result := Result + AnsiLineFeed;
        'r':
          Result := Result + AnsiCarriageReturn;
        't':
          Result := Result + AnsiTab;
        'v':
          Result := Result + AnsiVerticalTab;
        '\':
          Result := Result + '\';
        '"':
          Result := Result + '"';
        '''':
          Result := Result + ''''; // Optionally escaped
        '?':
          Result := Result + '?';  // Optionally escaped
        'x':
          if I < Len then
            // Start of hex escape sequence
            HandleHexEscapeSeq
          else
            // '\x' at end of AnsiString is not escape sequence
            Result := Result + '\x';
        '0'..'7':
          // start of octal escape sequence
          HandleOctEscapeSeq;
      else
        // no escape sequence
        Result := Result + '\' + S[I];
      end;
    end;
    Inc(I);
  end;
end;

function StrLower(const S: AnsiString): AnsiString;
begin
  Result := S;
  StrLowerInPlace(Result);
end;

procedure StrLowerInPlace(var S: AnsiString); assembler;
{$IFDEF PIC}
begin
  StrCase(S, AnsiLoOffset);
end;
{$ELSE}
asm
        // StrCase(S, AnsiLoOffset)

        XOR     EDX, EDX         // MOV     EDX, LoOffset
        JMP     StrCase
end;
{$ENDIF PIC}

procedure StrLowerBuff(S: PAnsiChar); assembler;
{$IFDEF PIC}
begin
  StrCaseBuff(S, AnsiLoOffset);
end;
{$ELSE}
asm
        // StrCaseBuff(S, LoOffset)
        XOR     EDX, EDX                // MOV     EDX, LoOffset
        JMP     StrCaseBuff
end;
{$ENDIF PIC}

procedure StrMove(var Dest: AnsiString; const Source: AnsiString;
  const ToIndex, FromIndex, Count: Integer);
begin
  // Check strings
  if (Source = '') or (Length(Dest) = 0) then
    Exit;

  // Check FromIndex
  if (FromIndex <= 0) or (FromIndex > Length(Source)) or
     (ToIndex <= 0) or (ToIndex > Length(Dest)) or
     ((FromIndex + Count - 1) > Length(Source)) or ((ToIndex + Count - 1) > Length(Dest)) then
     { TODO : Is failure without notice the proper thing to do here? }
     Exit;

  // Move
  Move(Source[FromIndex], Dest[ToIndex], Count);
end;

function StrPadLeft(const S: AnsiString; Len: Integer; C: AnsiChar): AnsiString;
var
  L: Integer;
begin
  L := Length(S);
  if L < Len then
    Result := StringOfChar(C, Len - L) + S
  else
    Result := S;
end;

function StrPadRight(const S: AnsiString; Len: Integer; C: AnsiChar): AnsiString;
var
  L: Integer;
begin
  L := Length(S);
  if L < Len then
    Result := S + StringOfChar(C, Len - L)
  else
    Result := S;
end;

function StrProper(const S: AnsiString): AnsiString;
begin
  Result := S;
  StrProperBuff(PChar(Result));
end;

procedure StrProperBuff(S: PAnsiChar);
begin
  if (S <> nil) and (S^ <> #0) then
  begin
    StrLowerBuff(S);
    S^ := CharUpper(S^);
  end;
end;

function StrQuote(const S: AnsiString; C: AnsiChar): AnsiString;
var
  L: Integer;
begin
  L := Length(S);
  Result := S;
  if L > 0 then
  begin
    if Result[1] <> C then
    begin
      Result := C + Result;
      Inc(L);
    end;
    if Result[L] <> C then
      Result := Result + C;
  end;
end;

function StrRemoveChars(const S: AnsiString; const Chars: TSysCharSet): AnsiString;
var
  Source, Dest: PChar;
begin
  SetLength(Result, Length(S));
  UniqueString(Result);
  Source := PChar(S);
  Dest := PChar(Result);
  while (Source <> nil) and (Source^ <> #0) do
  begin
    if not (Source^ in Chars) then
    begin
      Dest^ := Source^;
      Inc(Dest);
    end;
    Inc(Source);
  end;
  SetLength(Result, (Longint(Dest) - Longint(PChar(Result))) div SizeOf(AnsiChar));
end;

function StrKeepChars(const S: AnsiString; const Chars: TSysCharSet): AnsiString;
var
  Source, Dest: PChar;
begin
  SetLength(Result, Length(S));
  UniqueString(Result);
  Source := PChar(S);
  Dest := PChar(Result);
  while (Source <> nil) and (Source^ <> #0) do
  begin
    if Source^ in Chars then
    begin
      Dest^ := Source^;
      Inc(Dest);
    end;
    Inc(Source);
  end;
  SetLength(Result, (Longint(Dest) - Longint(PChar(Result))) div SizeOf(AnsiChar));
end;

function StrRepeat(const S: AnsiString; Count: Integer): AnsiString;
var
  L: Integer;
  P: PChar;
begin
  L := Length(S);
  SetLength(Result, Count * L);
  P := Pointer(Result);
  while Count > 0 do
  begin
    Move(Pointer(S)^, P^, L);
    P := P + L;
    Dec(Count);
  end;
end;

function StrRepeatLength(const S: AnsiString; Const L: Integer): AnsiString;
var
  Count: Integer;
  LenS: Integer;
  P: PChar;
begin
  Result := '';

  LenS := Length(S);

  if LenS > 0 then
  begin
    Count := L div LenS;
    if Count * LenS < L then
      Inc(Count);
    SetLength(Result, Count * LenS);
    P := Pointer(Result);
    while Count> 0 do
    begin
      Move(Pointer(S)^, P^, LenS);
      P := P + LenS;
      Dec(Count);
    end;
    if Length(S) > L then
      SetLength(Result, L);
  end;
end;

procedure StrReplace(var S: AnsiString; const Search, Replace: AnsiString; Flags: TReplaceFlags);
var
  SearchStr: AnsiString;
  ResultStr: AnsiString; { result string }
  SourcePtr: PChar;      { pointer into S of character under examination }
  SourceMatchPtr: PChar; { pointers into S and Search when first character has }
  SearchMatchPtr: PChar; { been matched and we're probing for a complete match }
  ResultPtr: PChar;      { pointer into Result of character being written }
  ResultIndex,
  SearchLength,          { length of search string }
  ReplaceLength,         { length of replace string }
  BufferLength,          { length of temporary result buffer }
  ResultLength: Integer; { length of result string }
  C: Char;               { first character of search string }
  IgnoreCase: Boolean;
begin
  if Search = '' then
    if S = '' then
    begin
      S := Replace;
      Exit;
    end
    else
      raise EJclStringError.CreateResRec(@RsBlankSearchString);

  if S <> '' then
  begin
    IgnoreCase := rfIgnoreCase in Flags;
    if IgnoreCase then
      SearchStr := AnsiUpperCase(Search)
    else
      SearchStr := Search;
    { avoid having to call Length() within the loop }
    SearchLength := Length(Search);
    ReplaceLength := Length(Replace);
    ResultLength := Length(S);
    BufferLength := ResultLength;
    SetLength(ResultStr, BufferLength);
    { get pointers to begin of source and result }
    ResultPtr := PChar(ResultStr);
    SourcePtr := PChar(S);
    C := SearchStr[1];
    { while we haven't reached the end of the string }
    while True do
    begin
      { copy characters until we find the first character of the search string }
      if IgnoreCase then
        while (CharUpper(SourcePtr^) <> C) and (SourcePtr^ <> #0) do
        begin
          ResultPtr^ := SourcePtr^;
          Inc(ResultPtr);
          Inc(SourcePtr);
        end
      else
        while (SourcePtr^ <> C) and (SourcePtr^ <> #0) do
        begin
          ResultPtr^ := SourcePtr^;
          Inc(ResultPtr);
          Inc(SourcePtr);
        end;
      { did we find that first character or did we hit the end of the string? }
      if SourcePtr^ = #0 then
        Break
      else
      begin
        { continue comparing, +1 because first character was matched already }
        SourceMatchPtr := SourcePtr + 1;
        SearchMatchPtr := PChar(SearchStr) + 1;
        if IgnoreCase then
          while (CharUpper(SourceMatchPtr^) = SearchMatchPtr^) and (SearchMatchPtr^ <> #0) do
          begin
            Inc(SourceMatchPtr);
            Inc(SearchMatchPtr);
          end
        else
          while (SourceMatchPtr^ = SearchMatchPtr^) and (SearchMatchPtr^ <> #0) do
          begin
            Inc(SourceMatchPtr);
            Inc(SearchMatchPtr);
          end;
        { did we find a complete match? }
        if SearchMatchPtr^ = #0 then
        begin
          // keep track of result length
          Inc(ResultLength, ReplaceLength - SearchLength);
          if ReplaceLength > 0 then
          begin
            // increase buffer size if required
            if ResultLength > BufferLength then
            begin
              BufferLength := ResultLength * 2;
              ResultIndex := ResultPtr - PChar(ResultStr) + 1;
              SetLength(ResultStr, BufferLength);
              ResultPtr := @ResultStr[ResultIndex];
            end;
            { append replace to result and move past the search string in source }
            Move((@Replace[1])^, ResultPtr^, ReplaceLength);
          end;
          Inc(SourcePtr, SearchLength);
          Inc(ResultPtr, ReplaceLength);
          { replace all instances or just one? }
          if not (rfReplaceAll in Flags) then
          begin
            { just one, copy until end of source and break out of loop }
            while SourcePtr^ <> #0 do
            begin
              ResultPtr^ := SourcePtr^;
              Inc(ResultPtr);
              Inc(SourcePtr);
            end;
            Break;
          end;
        end
        else
        begin
          { copy current character and start over with the next }
          ResultPtr^ := SourcePtr^;
          Inc(ResultPtr);
          Inc(SourcePtr);
        end;
      end;
    end;
    { set result length and copy result into S }
    SetLength(ResultStr, ResultLength);
    S := ResultStr;
  end;
end;

function StrReplaceChar(const S: AnsiString; const Source, Replace: Char): AnsiString;
var
  I: Integer;
begin
  Result := S;
  for I := 1 to Length(S) do
    if Result[I] = Source then
      Result[I] := Replace;
end;

function StrReplaceChars(const S: AnsiString; const Chars: TSysCharSet; Replace: Char): AnsiString;
var
  I: Integer;
begin
  Result := S;
  for I := 1 to Length(S) do
    if Result[I] in Chars then
      Result[I] := Replace;
end;

function StrReplaceButChars(const S: AnsiString; const Chars: TSysCharSet;
  Replace: Char): AnsiString;
var
  I: Integer;
begin
  Result := S;
  for I := 1 to Length(S) do
    if not (Result[I] in Chars) then
      Result[I] := Replace;
end;

function StrReverse(const S: AnsiString): AnsiString;
begin
  Result := S;
  StrReverseInplace(Result);
end;

procedure StrReverseInPlace(var S: AnsiString);
var
  P1, P2: PChar;
  C: AnsiChar;
begin
  UniqueString(S);
  P1 := PChar(S);
  P2 := P1 + SizeOf(AnsiChar) * (Length(S) - 1);
  while P1 < P2 do
  begin
    C := P1^;
    P1^ := P2^;
    P2^ := C;
    Inc(P1);
    Dec(P2);
  end;
end;

function StrSingleQuote(const S: AnsiString): AnsiString;
begin
  Result := AnsiSingleQuote + S + AnsiSingleQuote;
end;

function StrSmartCase(const S: AnsiString; Delimiters: TSysCharSet): AnsiString;
var
  Source, Dest: PChar;

begin
  Result := '';

  if Delimiters = [] then
    Include(Delimiters, AnsiSpace);

  if S <> '' then
  begin
    Result := S;
    UniqueString(Result);
    
    Source  := PChar(S);
    Dest := PChar(Result);

    Inc(Dest);

    while Source^ <> #0 do
    begin
      if (Source^ in Delimiters) and (Dest^ <> #0) then
        Dest^ := CharUpper(Dest^);

      Inc(Dest);
      Inc(Source);
    end;

    Result[1] := CharUpper(Result[1]);
  end;
end;

function StrStringToEscaped(const S: AnsiString): AnsiString;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    case S[I] of
      AnsiBackspace:
        Result := Result + '\b';
      AnsiBell:
        Result := Result + '\a';
      AnsiCarriageReturn:
        Result := Result + '\r';
      AnsiFormFeed:
        Result := Result + '\f';
      AnsiLineFeed:
        Result := Result + '\n';
      AnsiTab:
        Result := Result + '\t';
      AnsiVerticalTab:
        Result := Result + '\v';
      '\':
        Result := Result + '\\';
      '"':
        Result := Result + '\"';
    else
      // Characters < ' ' are escaped with hex sequence
      if S[I] < #32 then
        Result := Result + Format('\x%.2x',[Integer(S[I])])
      else
        Result := Result + S[I];
    end;
  end;
end;

function StrStripNonNumberChars(const S: AnsiString): AnsiString;
var
  I: Integer;
  C: AnsiChar;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    C := S[I];
    if CharIsNumberChar(C) then
      Result := Result + C;
  end;
end;

function StrToHex(const Source: AnsiString): AnsiString;
var
  P: PChar;
  C, L, N: Integer;
  BL, BH: Byte;
  S: AnsiString;
begin
  Result := '';
  if Source <> '' then
  begin
    S := Source;
    L := Length(S);
    if Odd(L) then
    begin
      S := '0' + S;
      Inc(L);
    end;
    P := PChar(S);
    SetLength(Result, L div 2);
    C := 1;
    N := 1;
    while C <= L do
    begin
      BH := CharHex(P^);
      Inc(P);
      BL := CharHex(P^);
      Inc(P);
      Inc(C, 2);
      if (BH = $FF) or (BL = $FF) then
      begin
        Result := '';
        Exit;
      end;
      Byte(Result[N]) := Byte((BH shl 4) + BL);
      Inc(N);
    end;
  end;
end;

function StrTrimCharLeft(const S: AnsiString; C: AnsiChar): AnsiString;
var
  I, L: Integer;
begin
  I := 1;
  L := Length(S);
  while (I <= L) and (S[I] = C) do Inc(I);
  Result := Copy(S, I, L - I + 1);
end;

function StrTrimCharsLeft(const S: AnsiString; const Chars: TSysCharSet): AnsiString;
var
  I, L: Integer;
begin
  I := 1;
  L := Length(S);
  while (I <= L) and (S[I] in Chars) do Inc(I);
  Result := Copy(S, I, L - I + 1);
end;

function StrTrimCharsRight(const S: AnsiString; const Chars: TSysCharSet): AnsiString;
var
  I: Integer;
begin
  I := Length(S);
  while (I >= 1) and (S[I] in Chars) do Dec(I);
  Result := Copy(S, 1, I);
end;

function StrTrimCharRight(const S: AnsiString; C: AnsiChar): AnsiString;
var
  I: Integer;
begin
  I := Length(S);
  while (I >= 1) and (S[I] = C) do Dec(I);
  Result := Copy(S, 1, I);
end;

function StrTrimQuotes(const S: AnsiString): AnsiString;
var
  First, Last: AnsiChar;
  L: Integer;
begin
  L := Length(S);
  if L > 1 then
  begin
    First := S[1];
    Last := S[L];
    if (First = Last) and ((First = AnsiSingleQuote) or (First = AnsiDoubleQuote)) then
      Result := Copy(S, 2, L - 2)
    else
      Result := S;
  end
  else
    Result := S;
end;

function StrUpper(const S: AnsiString): AnsiString;
begin
  Result := S;
  StrUpperInPlace(Result);
end;

procedure StrUpperInPlace(var S: AnsiString); assembler;
{$IFDEF PIC}
begin
  StrCase(S, AnsiUpOffset);
end;
{$ELSE}
asm
        // StrCase(Str, AnsiUpOffset)
        MOV     EDX, AnsiUpOffset
        JMP     StrCase
end;
{$ENDIF PIC}

procedure StrUpperBuff(S: PAnsiChar); assembler;
{$IFDEF PIC}
begin
  StrCaseBuff(S, AnsiUpOffset);
end;
{$ELSE}
asm
        // StrCaseBuff(S, UpOffset)
        MOV     EDX, AnsiUpOffset
        JMP     StrCaseBuff
end;
{$ENDIF PIC}

{$IFDEF WIN32}
function StrOemToAnsi(const S: AnsiString): AnsiString;
begin
  SetLength(Result, Length(S));
  OemToAnsiBuff(@S[1], @Result[1], Length(S));
end;
{$ENDIF WIN32}

{$IFDEF WIN32}
function StrAnsiToOem(const S: AnsiString): AnsiString;
begin
  SetLength(Result, Length(S));
  AnsiToOemBuff(@S[1], @Result[1], Length(S));
end;
{$ENDIF WIN32}


//=== String Management ======================================================

procedure StrAddRef(var S: AnsiString);
var
  Foo: AnsiString;
begin
  if StrRefCount(S) = -1 then
    UniqueString(S)
  else
  begin
    Foo := S;
    Pointer(Foo) := nil;
  end;
end;

function StrAllocSize(const S: AnsiString): Longint;
var
  P: Pointer;
begin
  Result := 0;
  if Pointer(S) <> nil then
  begin
    P := Pointer(Integer(Pointer(S)) - AnsiRfOffset);
    if Integer(P^) <> -1 then
    begin
      P := Pointer(Integer(Pointer(S)) - AnsiAlOffset);
      Result := Integer(P^);
    end;
  end;
end;

procedure StrDecRef(var S: AnsiString);
var
  Foo: AnsiString;
begin
  case StrRefCount(S) of
    -1, 0: { nothing } ;
     1:
       begin
         Finalize(S);
         Pointer(S) := nil;
       end;
  else
    Pointer(Foo) := Pointer(S);
  end;
end;

function StrLen(S: PChar): Integer; assembler;
asm
        TEST    EAX, EAX
        JZ      @@EXIT

        PUSH    EBX
        MOV     EDX, EAX                 // save pointer
@L1:    MOV     EBX, [EAX]               // read 4 bytes
        ADD     EAX, 4                   // increment pointer
        LEA     ECX, [EBX-$01010101]     // subtract 1 from each byte
        NOT     EBX                      // invert all bytes
        AND     ECX, EBX                 // and these two
        AND     ECX, $80808080           // test all sign bits
        JZ      @L1                      // no zero bytes, continue loop
        TEST    ECX, $00008080           // test first two bytes
        JZ      @L2
        SHL     ECX, 16                  // not in the first 2 bytes
        SUB     EAX, 2
@L2:    SHL     ECX, 9                   // use carry flag to avoid a branch
        SBB     EAX, EDX                 // compute length
        POP     EBX

        JZ      @@EXIT                   // Az: SBB sets zero flag
        DEC     EAX                      // do not include null terminator
@@EXIT:
end;

function StrLength(const S: AnsiString): Longint;
var
  P: Pointer;
begin
  Result := 0;
  if Pointer(S) <> nil then
  begin
    P := Pointer(Integer(Pointer(S)) - AnsiLnOffset);
    Result := Integer(P^) and (not $80000000 shr 1);
  end;
end;

function StrRefCount(const S: AnsiString): Longint;
var
  P: Pointer;
begin
  Result := 0;
  if Pointer(S) <> nil then
  begin
    P := Pointer(Integer(Pointer(S)) - AnsiRfOffset);
    Result := Integer(P^);
  end;
end;

procedure StrResetLength(var S: AnsiString);
begin
  SetLength(S, StrLen(PChar(S)));
end;

//=== String Search and Replace Routines =====================================

function StrCharCount(const S: AnsiString; C: AnsiChar): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    if S[I] = C then
      Inc(Result);
end;

function StrCharsCount(const S: AnsiString; Chars: TSysCharSet): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    if S[I] in Chars then
      Inc(Result);
end;

function StrStrCount(const S, SubS: AnsiString): Integer;
var
  I: Integer;
begin
  Result := 0;
  if (Length(SubS) > Length(S)) or (Length(SubS) = 0) or (Length(S) = 0) then
    Exit;
  if Length(SubS) = 1 then
  begin
    Result := StrCharCount(S, SubS[1]);
    Exit;
  end;
  I := StrSearch(SubS, S, 1);

  if I > 0 then
    Inc(Result);

  while (I > 0) and (Length(S) > I+Length(SubS)) do
  begin
    I := StrSearch(SubS, S, I+1);

    if I > 0 then
      Inc(Result);
  end
end;

{$IFDEF PIC}
function _StrCompare(const S1, S2: AnsiString): Integer; forward;

function StrCompare(const S1, S2: AnsiString): Integer;
begin
  Result := _StrCompare(S1, S2);
end;

function _StrCompare(const S1, S2: AnsiString): Integer; assembler;
{$ELSE}
function StrCompare(const S1, S2: AnsiString): Integer; assembler;
{$ENDIF PIC}
asm
        // check if pointers are equal

        CMP     EAX, EDX
        JE      @@Equal

        // if S1 is nil return - Length(S2)

        TEST    EAX, EAX
        JZ      @@Str1Null

        // if S2 is nil return  Length(S1)

        TEST    EDX, EDX
        JZ      @@Str2Null

        // EBX will hold case map, ESI S1, EDI S2

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        // move AnsiString pointers

        MOV     ESI, EAX
        MOV     EDI, EDX

        // get the length of strings

        MOV     EAX, [ESI-AnsiStrRecSize].TAnsiStrRec.Length
        MOV     EDX, [EDI-AnsiStrRecSize].TAnsiStrRec.Length

        // exit if Length(S1) <> Length(S2)

        CMP     EAX, EDX
        JNE     @@MissMatch

        // check the length just in case

        DEC     EDX
        JS      @@InvalidStr

        DEC     EAX
        JS      @@InvalidStr

        // load case map

        LEA     EBX, AnsiCaseMap

        // make ECX our loop counter

        MOV     ECX, EAX

        // clear working regs

        XOR     EAX, EAX
        XOR     EDX, EDX

        // get last chars

        MOV     AL, [ESI+ECX]
        MOV     DL, [EDI+ECX]

        // lower case them

        MOV     AL, [EBX+EAX]
        MOV     DL, [EBX+EDX]

        // compare them

        CMP     AL, DL
        JNE     @@MissMatch

        // if there was only 1 char then exit

        JECXZ   @@Match

@@NextChar:
        // case sensitive compare of strings

        REPE    CMPSB
        JE      @@Match

        // if there was a missmatch try case insensitive compare, get the chars

        MOV     AL, [ESI-1]
        MOV     DL, [EDI-1]

        // lowercase and compare them, if equal then continue

        MOV     AL, [EBX+EAX]
        MOV     DL, [EBX+EDX]
        CMP     AL, DL
        JE      @@NextChar

        // if we make it here then strings don't match,  return the difference

@@MissMatch:
        SUB     EAX, EDX
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@Match:
        // match, return 0

        XOR     EAX, EAX
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@InvalidStr:
        XOR     EAX, EAX
        DEC     EAX
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@Str1Null:
        // return = - Length(Str2);

        MOV     EDX, [EDX-AnsiStrRecSize].TAnsiStrRec.Length
        SUB     EAX, EDX
        RET

@@Str2Null:
        // return = Length(Str2);

        MOV     EAX, [EAX-AnsiStrRecSize].TAnsiStrRec.Length
        RET

@@Equal:
        XOR     EAX, EAX
end;

function StrCompareRange(const S1, S2: AnsiString; const Index, Count: Integer): Integer; assembler;
asm
        TEST    EAX, EAX
        JZ      @@Str1Null

        TEST    EDX, EDX
        JZ      @@StrNull

        DEC     ECX
        JS      @@StrNull

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     EBX, Count
        DEC     EBX
        JS      @@NoWork

        MOV     ESI, EAX
        MOV     EDI, EDX

        MOV     EDX, [ESI - AnsiStrRecSize].TAnsiStrRec.Length

        // # of chars in S1 - (Index - 1)
        SUB     EDX, ECX
        JLE     @@NoWork

        // # of chars in S1 - (Count - 1)
        SUB     EDX, EBX
        JLE     @@NoWork

        // move to index'th char
        ADD     ESI, ECX

        MOV     ECX, [EDI - AnsiStrRecSize].TAnsiStrRec.Length
        DEC     ECX
        JS      @@NoWork

        // if Length(S2) > Count then ECX := Count else ECX := Length(S2)

        CMP     ECX, EBX
        JLE     @@Skip1
        MOV     ECX, EBX

@@Skip1:
        XOR     EAX, EAX
        XOR     EDX, EDX

@@Loop:
        MOV     AL, [ESI]
        INC     ESI
        MOV     DL, [EDI]
        INC     EDI

        CMP     AL, DL
        JNE     @@MisMatch

        DEC     ECX
        JGE     @@Loop

@@Match:
        XOR     EAX, EAX
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     @@Exit

@@MisMatch:
        SUB     EAX, EDX
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     @@Exit

@@NoWork:
        MOV     EAX, -2
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     @@Exit

@@Str1Null:
        MOV     EAX, 0
        TEST    EDX, EDX
        JZ      @@Exit

@@StrNull:
        MOV     EAX, -1

@@Exit:
end;

function StrFillChar(const C: AnsiChar; const Count: Integer): AnsiString;
begin
  Assert(Count >= 0);
  SetLength(Result, Count);
  if (Count > 0) then
    FillChar(Result[1], Count, Ord(C));
end;

function StrFind(const Substr, S: AnsiString; const Index: Integer): Integer; assembler;
const
   SearchChar: Byte = 0;
   NumberOfChars: Integer = 0;
asm
        // if SubStr = '' then  Return := 0;

        TEST    EAX, EAX
        JZ      @@SubstrIsNull

        // if Str = '' then  Return := 0;

        TEST    EDX, EDX
        JZ      @@StrIsNull

        // Index := Index - 1; if Index < 0 then Return := 0;

        DEC     ECX
        JL      @@IndexIsSmall

        // EBX will hold the case table, ESI pointer to Str, EDI pointer
        // to Substr and - # of chars in Substr to compare

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        // set the string pointers

        MOV     ESI, EDX
        MOV     EDI, EAX

        // save the Index in EDX

        MOV     EDX, ECX

        // temporary get the length of Substr and Str

        MOV     EBX, [EDI - AnsiStrRecSize].TAnsiStrRec.Length
        MOV     ECX, [ESI - AnsiStrRecSize].TAnsiStrRec.Length

        // save the address of Str to compute the result

        PUSH    ESI

        // dec the length of Substr because the first char is brought out of it

        DEC     EBX
        JS      @@NotFound

        // #positions in Str to look at = Length(Str) - Length(Substr) - Index - 2

        SUB     ECX, EBX
        JLE     @@NotFound

        SUB     ECX, EDX
        JLE     @@NotFound

        // # of chars in Substr to compare

        MOV     NumberOfChars, EBX

        // point Str to Index'th char

        ADD     ESI, EDX

        // load case map into EBX, and clear EAX

        LEA     EBX, AnsiCaseMap
        XOR     EAX, EAX
        XOR     EDX, EDX

        // bring the first char out of the Substr and point Substr to the next char

        MOV     DL, [EDI]
        INC     EDI

        // lower case it

        MOV     DL, [EBX + EDX]
        MOV     SearchChar, DL

        JMP     @@Find

@@FindNext:

        // update the loop counter and check the end of AnsiString.
        // if we reached the end, Substr was not found.

        DEC     ECX
        JL      @@NotFound

@@Find:

        // get current char from the AnsiString, and point Str to the next one

        MOV     AL, [ESI]
        INC     ESI


        // lower case current char

        MOV     AL, [EBX + EAX]

        // does current char match primary search char? if not, go back to the main loop

        CMP     AL, SearchChar
        JNE     @@FindNext

@@Compare:

        // # of chars in Substr to compare

        MOV     EDX, NumberOfChars

@@CompareNext:

        // dec loop counter and check if we reached the end. If yes then we found it

        DEC     EDX
        JL      @@Found

        // get the chars from Str and Substr, if they are equal then continue comparing

        MOV     AL, [ESI + EDX]
        CMP     AL, [EDI + EDX]
        JE      @@CompareNext

        // otherwise try the reverse case. If they still don't match go back to the Find loop

        MOV     AL, [EBX + EAX + AnsiReOffset]
        CMP     AL, [EDI + EDX]
        JNE     @@FindNext

        // if they matched, continue comparing

        JMP     @@CompareNext

@@Found:
        // we found it, calculate the result

        MOV     EAX, ESI
        POP     ESI
        SUB     EAX, ESI

        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@NotFound:

        // not found it, clear the result

        XOR     EAX, EAX
        POP     ESI
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@IndexIsSmall:
@@StrIsNull:

        // clear the result

        XOR     EAX, EAX

@@SubstrIsNull:
@@Exit:
end;

function StrHasPrefix(const S: AnsiString; const Prefixes: array of string): Boolean;
begin
  Result := StrPrefixIndex(S, Prefixes) > -1;
end;

function StrIndex(const S: AnsiString; const List: array of AnsiString): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(List) to High(List) do
  begin
    if AnsiSameText(S, List[I]) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function StrILastPos(const SubStr, S: AnsiString): Integer;
begin
  Result := JclStrings.StrLastPos(StrUpper(SubStr), StrUpper(S));
end;

function StrIPos(const SubStr, S: AnsiString): integer;
begin
  Result := Pos(AnsiUpperCase(SubStr), AnsiUpperCase(S));
end;

function StrIsOneOf(const S: AnsiString; const List: array of AnsiString): Boolean;
begin
  Result := StrIndex(S, List) > -1;
end;

function StrLastPos(const SubStr, S: AnsiString): Integer;
var
  Last, Current: PAnsiChar;

begin
  Result := 0;
  Last := nil;
  Current := PAnsiChar(S);

  while (Current <> nil) and (Current^ <> #0) do
  begin
    Current := AnsiStrPos(PAnsiChar(Current), PAnsiChar(SubStr));
    if Current <> nil then
    begin
      Last := Current;
      Inc(Current);
    end;
  end;
  if Last <> nil then
    Result := Abs((Longint(PAnsiChar(S)) - Longint(Last)) div SizeOf(AnsiChar)) + 1;
end;

// IMPORTANT NOTE: The StrMatch function does currently not work with the Asterix (*)

function StrMatch(const Substr, S: AnsiString; const Index: Integer): Integer; assembler;
asm
        // make sure that strings are not null

        TEST    EAX, EAX
        JZ      @@SubstrIsNull

        TEST    EDX, EDX
        JZ      @@StrIsNull

        // limit index to satisfy 1 <= index, and dec it

        DEC     ECX
        JL      @@IndexIsSmall

        // EBX will hold the case table, ESI pointer to Str, EDI pointer
        // to Substr and EBP # of chars in Substr to compare

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        // set the AnsiString pointers

        MOV     ESI, EDX
        MOV     EDI, EAX

        // save the Index in EDX

        MOV     EDX, ECX

        // save the address of Str to compute the result

        PUSH    ESI

        // temporary get the length of Substr and Str

        MOV     EBX, [EDI - AnsiStrRecSize].TAnsiStrRec.Length
        MOV     ECX, [ESI - AnsiStrRecSize].TAnsiStrRec.Length

        // dec the length of Substr because the first char is brought out of it

        DEC     EBX
        JS      @@NotFound

        // #positions in Str to look at = Length(Str) - Length(Substr) - Index - 2

        SUB     ECX, EBX
        JLE     @@NotFound

        SUB     ECX, EDX
        JLE     @@NotFound

        // # of chars in Substr to compare

        MOV     EBP, EBX

        // point Str to Index'th char

        ADD     ESI, EDX

        // load case map into EBX, and clear EAX & ECX

        LEA     EBX, AnsiCaseMap
        XOR     EAX, EAX
        XOR     ECX, ECX

        // bring the first char out of the Substr and point Substr to the next char

        MOV     CL, [EDI]
        INC     EDI

        // lower case it

        MOV     CL, [EBX + ECX]

@@FindNext:

        // get the current char from Str into al

        MOV     AL, [ESI]
        INC     ESI

        // check the end of AnsiString

        TEST    AL, AL
        JZ      @@NotFound


        CMP     CL, '*'    // Wild Card?
        JE      @@Compare

        CMP     CL, '?'    // Wild Card?
        JE      @@Compare

        // lower case current char

        MOV     AL, [EBX + EAX]

        // check if the current char matches the primary search char,
        // if not continue searching

        CMP     AL, CL
        JNE     @@FindNext

@@Compare:

        // # of chars in Substr to compare }

        MOV     EDX, EBP

@@CompareNext:

        // dec loop counter and check if we reached the end. If yes then we found it

        DEC     EDX
        JL      @@Found

        // get the chars from Str and Substr, if they are equal then continue comparing

        MOV     AL, [EDI + EDX]               // char from  Substr

        CMP     AL, '*'                     // wild card?
        JE      @@CompareNext

        CMP     AL, '?'                     // wild card?
        JE      @@CompareNext

        CMP     AL, [ESI + EDX]               // equal to PChar(Str)^ ?
        JE      @@CompareNext

        MOV     AL, [EBX + EAX + AnsiReOffset]  // reverse case?
        CMP     AL, [ESI + EDX]
        JNE     @@FindNext                  // if still no, go back to the main loop

        // if they matched, continue comparing

        JMP     @@CompareNext

@@Found:
        // we found it, calculate the result

        MOV     EAX, ESI
        POP     ESI
        SUB     EAX, ESI

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@NotFound:

        // not found it, clear the result

        XOR     EAX, EAX
        POP     ESI
        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@IndexIsSmall:
@@StrIsNull:

        // clear the result

        XOR     EAX, EAX

@@SubstrIsNull:
@@Exit:
end;

// Derived from "Like" by Michael Winter

function StrMatches(const Substr, S: AnsiString; const Index: Integer): Boolean;
var
  StringPtr: PChar;
  PatternPtr: PChar;
  StringRes: PChar;
  PatternRes: PChar;

begin
  if SubStr = '' then
    raise EJclStringError.CreateResRec(@RsBlankSearchString);

  Result := SubStr = '*';

  if Result or (S = '') then
    Exit;

  StringPtr := PChar(@S[Index]);
  PatternPtr := PChar(SubStr);
  StringRes := nil;
  PatternRes := nil;

  repeat
    repeat
      case PatternPtr^ of
        #0:
          begin
            Result := StringPtr^ = #0;
            if Result or (StringRes = nil) or (PatternRes = nil) then
              Exit;

            StringPtr := StringRes;
            PatternPtr := PatternRes;
            Break;
          end;
        '*':
          begin
            Inc(PatternPtr);
            PatternRes := PatternPtr;
            Break;
          end;
        '?':
          begin
            if StringPtr^ = #0 then
              Exit;
            Inc(StringPtr);
            Inc(PatternPtr);
          end;
        else
          begin
            if StringPtr^ = #0 then
              Exit;
            if StringPtr^ <> PatternPtr^ then
            begin
              if (StringRes = nil) or (PatternRes = nil) then
                Exit;
              StringPtr := StringRes;
              PatternPtr := PatternRes;
              Break;
            end
            else
            begin
              Inc(StringPtr);
              Inc(PatternPtr);
            end;
          end;
      end;
    until False;

    repeat
      case PatternPtr^ of
        #0:
          begin
            Result := True;
            Exit;
          end;
        '*':
          begin
            Inc(PatternPtr);
            PatternRes := PatternPtr;
          end;
        '?':
          begin
            if StringPtr^ = #0 then
              Exit;
            Inc(StringPtr);
            Inc(PatternPtr);
          end;
        else
          begin
            repeat
              if StringPtr^ = #0 then
                Exit;
              if StringPtr^ = PatternPtr^ then
                Break;
              Inc(StringPtr);
            until False;
            Inc(StringPtr);
            StringRes := StringPtr;
            Inc(PatternPtr);
            Break;
          end;
      end;
    until False;
  until False;
end;

function StrNPos(const S, SubStr: AnsiString; N: Integer): Integer;
var
  I, P: Integer;
begin
  if N < 1 then
  begin
    Result := 0;
    Exit;
  end;

  Result := StrSearch(SubStr, S, 1);
  I := 1;
  while I < N do
  begin
    P := StrSearch(SubStr, S, Result + 1);
    if P = 0 then
    begin
      Result := 0;
      Break;
    end
    else
    begin
      Result := P;
      Inc(I);
    end;
  end;
end;

function StrNIPos(const S, SubStr: AnsiString; N: Integer): Integer;
var
  I, P: Integer;
begin
  if N < 1 then
  begin
    Result := 0;
    Exit;
  end;

  Result := StrFind(SubStr, S, 1);
  I := 1;
  while I < N do
  begin
    P := StrFind(SubStr, S, Result + 1);
    if P = 0 then
    begin
      Result := 0;
      Break;
    end
    else
    begin
      Result := P;
      Inc(I);
    end;
  end;
end;

function StrPrefixIndex(const S: AnsiString; const Prefixes: array of string): Integer;
var
  I: Integer;
  Test: string;
begin
  Result := -1;
  for I := Low(Prefixes) to High(Prefixes) do
  begin
    Test := StrLeft(S, Length(Prefixes[I]));
    if AnsiSameText(Test, Prefixes[I]) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function StrSearch(const Substr, S: AnsiString; const Index: Integer): Integer; assembler;
asm
        // make sure that strings are not null

        TEST    EAX, EAX
        JZ      @@SubstrIsNull

        TEST    EDX, EDX
        JZ      @@StrIsNull

        // limit index to satisfy 1 <= index, and dec it

        DEC     ECX
        JL      @@IndexIsSmall

        // ebp will hold # of chars in Substr to compare, esi pointer to Str,
        // edi pointer to Substr, ebx primary search char

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        // set the AnsiString pointers

        MOV     ESI, EDX
        MOV     EDI, EAX

        // save the (Index - 1) in edx

        MOV     EDX, ECX

        // save the address of Str to compute the result

        PUSH    ESI

        // temporary get the length of Substr and Str

        MOV     EBX, [EDI-AnsiStrRecSize].TAnsiStrRec.Length
        MOV     ECX, [ESI-AnsiStrRecSize].TAnsiStrRec.Length

        // dec the length of Substr because the first char is brought out of it

        DEC     EBX
        JS      @@NotFound

        // # of positions in Str to look at = Length(Str) - Length(Substr) - Index - 2

        SUB     ECX, EBX
        JLE     @@NotFound

        SUB     ECX, EDX
        JLE     @@NotFound

        // point Str to Index'th char

        ADD     ESI, EDX

        // # of chars in Substr to compare

        MOV     EBP, EBX

        // clear EAX & ECX (working regs)

        XOR     EAX, EAX
        XOR     EBX, EBX

        // bring the first char out of the Substr, and
        // point Substr to the next char

        MOV     BL, [EDI]
        INC     EDI

        // jump into the loop

        JMP     @@Find

@@FindNext:

        // update the loop counter and check the end of AnsiString.
        // if we reached the end, Substr was not found.

        DEC     ECX
        JL      @@NotFound

@@Find:

        // get current char from the AnsiString, and /point Str to the next one.
        MOV     AL, [ESI]
        INC     ESI

        // does current char match primary search char? if not, go back to the main loop

        CMP     AL, BL
        JNE     @@FindNext

        // otherwise compare SubStr

@@Compare:

        // move # of char to compare into edx, edx will be our compare loop counter.

        MOV     EDX, EBP

@@CompareNext:

        // check if we reached the end of Substr. If yes we found it.

        DEC     EDX
        JL      @@Found

        // get last chars from Str and SubStr and compare them,
        // if they don't match go back to out main loop.

        MOV     AL, [EDI+EDX]
        CMP     AL, [ESI+EDX]
        JNE     @@FindNext

        // if they matched, continue comparing

        JMP     @@CompareNext

@@Found:
        // we found it, calculate the result and exit.

        MOV     EAX, ESI
        POP     ESI
        SUB     EAX, ESI

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@NotFound:
        // not found it, clear result and exit.

        XOR     EAX, EAX
        POP     ESI
        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@IndexIsSmall:
@@StrIsNull:
        // clear result and exit.

        XOR     EAX, EAX

@@SubstrIsNull:
@@Exit:
end;

//=== String Extraction ======================================================

function StrAfter(const SubStr, S: AnsiString): AnsiString;
var
  P: Integer;
begin
  P := StrFind(SubStr, S, 1); // StrFind is case-insensitive pos
  if P <= 0 then
    Result := ''           // substr not found -> nothing after it
  else
    Result := StrRestOf(S, P + Length(SubStr));
end;

function StrBefore(const SubStr, S: AnsiString): AnsiString;
var
  P: Integer;
begin
  P := StrFind(SubStr, S, 1);
  if P <= 0 then
    Result := S
  else
    Result := StrLeft(S, P - 1);
end;


function StrBetween(const S: AnsiString; const Start, Stop: AnsiChar): AnsiString;
var
  PosStart, PosEnd: Integer;
  L: Integer;
begin
  PosStart := Pos(Start, S);
  PosEnd := StrSearch(Stop, S, PosStart+1);  // PosEnd has to be after PosStart.

  if (PosStart > 0) and (PosEnd > PosStart) then
  begin
    L := PosEnd - PosStart;
    Result := Copy(S, PosStart + 1, L - 1);
  end
  else
    Result := '';
end;

function StrChopRight(const S: AnsiString; N: Integer): AnsiString;
begin
  Result := Copy(S, 1, Length(S) - N);
end;

function StrLeft(const S: AnsiString; Count: Integer): AnsiString;
begin
  Result := Copy(S, 1, Count);
end;

function StrMid(const S: AnsiString; Start, Count: Integer): AnsiString;
begin
  Result := Copy(S, Start, Count);
end;

function StrRestOf(const S: AnsiString; N: Integer ): AnsiString;
begin
  Result := Copy(S, N, (Length(S) - N + 1));
end;

function StrRight(const S: AnsiString; Count: Integer): AnsiString;
begin
  Result := Copy(S, Length(S) - Count + 1, Count);
end;

//=== Character (do we have it ;) ============================================

function CharEqualNoCase(const C1, C2: AnsiChar): Boolean;
begin
  //if they are not equal chars, may be same letter different case
  Result := (C1 = C2) or
    (CharIsAlpha(C1) and CharIsAlpha(C2) and (CharLower(C1) = CharLower(C2)));
end;


function CharIsAlpha(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_ALPHA) <> 0;
end;

function CharIsAlphaNum(const C: AnsiChar): Boolean;
begin
  Result := ((AnsiCharTypes[C] and C1_ALPHA) <> 0) or
    ((AnsiCharTypes[C] and C1_DIGIT) <> 0);
end;

function CharIsBlank(const C: AnsiChar): Boolean;
begin
  Result := ((AnsiCharTypes[C] and C1_BLANK) <> 0);
end;

function CharIsControl(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_CNTRL) <> 0;
end;

function CharIsDelete(const C: AnsiChar): Boolean;
begin
  Result := (C = #8);
end;

function CharIsDigit(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_DIGIT) <> 0;
end;

function CharIsLower(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_LOWER) <> 0;
end;

function CharIsNumberChar(const C: AnsiChar): Boolean;
begin
  Result := ((AnsiCharTypes[C] and C1_DIGIT) <> 0) or
    (C in AnsiSigns) or (C = DecimalSeparator);
end;

function CharIsPrintable(const C: AnsiChar): Boolean;
begin
  Result := not CharIsControl(C);
end;

function CharIsPunctuation(const C: AnsiChar): Boolean;
begin
  Result := ((AnsiCharTypes[C] and C1_PUNCT) <> 0);
end;

function CharIsReturn(const C: AnsiChar): Boolean;
begin
  Result := (C = AnsiLineFeed) or (C = AnsiCarriageReturn);
end;

function CharIsSpace(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_SPACE) <> 0;
end;

function CharIsUpper(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_UPPER) <> 0;
end;

function CharIsWhiteSpace(const C: AnsiChar): Boolean;
begin
  Result := C in AnsiWhiteSpace;
end;

function CharType(const C: AnsiChar): Word;
begin
  Result := AnsiCharTypes[C];
end;

//=== PCharVector ============================================================

function StringsToPCharVector(var Dest: PCharVector; const Source: TStrings): PCharVector;
var
  I: Integer;
  S: AnsiString;
  List: array of PChar;
begin
  Assert(Source <> nil);
  Dest := AllocMem((Source.Count + SizeOf(AnsiChar)) * SizeOf(PChar));
  SetLength(List, Source.Count + SizeOf(AnsiChar));
  for I := 0 to Source.Count - 1 do
  begin
    S := Source[I];
    List[I] := StrAlloc(Length(S) + SizeOf(AnsiChar));
    StrPCopy(List[I], S);
  end;
  List[Source.Count] := nil;
  Move(List[0], Dest^, (Source.Count + 1) * SizeOf(PChar));
  Result := Dest;
end;

function PCharVectorCount(Source: PCharVector): Integer;
var
  P: PChar;
begin
  Result := 0;
  if Source <> nil then
  begin
    P := Source^;
    while P <> nil do
    begin
      Inc(Result);
      P := PCharVector(Longint(Source) + (SizeOf(PChar) * Result))^;
    end;
  end;
end;

procedure PCharVectorToStrings(const Dest: TStrings; Source: PCharVector);
var
  I, Count: Integer;
  List: array of PChar;
begin
  Assert(Dest <> nil);
  if Source <> nil then
  begin
    Count := PCharVectorCount(Source);
    SetLength(List, Count);
    Move(Source^, List[0], Count * SizeOf(PChar));
    Dest.BeginUpdate;
    try
      Dest.Clear;
      for I := 0 to Count - 1 do
        Dest.Add(List[I]);
    finally
      Dest.EndUpdate;
    end;
  end;
end;

procedure FreePCharVector(var Dest: PCharVector);
var
  I, Count: Integer;
  List: array of PChar;
begin
  if Dest <> nil then
  begin
    Count := PCharVectorCount(Dest);
    SetLength(List, Count);
    Move(Dest^, List[0], Count * SizeOf(PChar));
    for I := 0 to Count - 1 do
      StrDispose(List[I]);
    FreeMem(Dest, (Count + 1) * SizeOf(PChar));
    Dest := nil;
  end;
end;

//=== Character Transformation Routines ======================================

function CharHex(const C: AnsiChar): Byte;
begin
  Result := $FF;
  if C in AnsiDecDigits then
    Result := Ord(CharUpper(C)) - Ord('0')
  else
  begin
    if C in AnsiHexDigits then
      Result := Ord(CharUpper(C)) - (Ord('A')) + 10;
  end;
end;

function CharLower(const C: AnsiChar): AnsiChar;
begin
  Result := AnsiCaseMap[Ord(C) + AnsiLoOffset];
end;

function CharToggleCase(const C: AnsiChar): AnsiChar;
begin
  Result := AnsiCaseMap[Ord(C) + AnsiReOffset];
end;

function CharUpper(const C: AnsiChar): AnsiChar;
begin
  Result := AnsiCaseMap[Ord(C) + AnsiUpOffset];
end;

//=== Character Search and Replace ===========================================

function CharLastPos(const S: AnsiString; const C: AnsiChar; const Index: Integer): Integer;
var
  P: PAnsiChar;
begin
  Result := 0;
  if (Index > 0) and (Index <= Length(S)) then
  begin
    P := PAnsiChar(S);
    Result := Length(S);
    Inc(P, Result-1);
    while Result > Index do
    begin
      if P^ = C then
        Break;

      Dec(Result);
      Dec(P);
    end;
    if P^ <> C then
      Result := 0;
  end;
end;

function CharPos(const S: AnsiString; const C: AnsiChar; const Index: Integer): Integer;
var
  P: PAnsiChar;
begin
  Result := 0;
  if (Index > 0) and (Index <= Length(S)) then
  begin
    P := PAnsiChar(S);
    Result := Index - 1;
    Inc(P, Result);
    while P^ <> #0 do
    begin
      Inc(Result);
      if P^ = C then
        Break;
      Inc(P);
    end;
    if P^ = #0 then
      Result := 0;
  end;
end;

function CharIPos(const S: AnsiString; const C: AnsiChar; const Index: Integer): Integer;
var
  P: PAnsiChar;
  CU: AnsiChar;
begin
  Result := 0;
  if (Index > 0) and (Index <= Length(S)) then
  begin
    CU := CharUpper(C);
    P := PAnsiChar(S);
    Result := Index - 1;
    Inc(P, Result);
    while P^ <> #0 do
    begin
      Inc(Result);
      if AnsiCaseMap[Ord(P^) + AnsiUpOffset] = CU then
        Break;
      Inc(P);
    end;
    if P^ = #0 then
      Result := 0;
  end;
end;

function CharReplace(var S: AnsiString; const Search, Replace: AnsiChar): Integer;
var
  P: PAnsiChar;
begin
  Result := 0;
  if Search <> Replace then
  begin
    UniqueString(S);
    P := PAnsiChar(S);
    while P^ <> #0 do
    begin
      if P^ = Search then
      begin
        P^ := Replace;
        Inc(Result);
      end;
      Inc(P);
    end;
  end;
end;

//=== MultiSz ================================================================

function StringsToMultiSz(var Dest: PMultiSz; const Source: TStrings): PMultiSz;
var
  I, TotalLength: Integer;
  P: PMultiSz;
begin
  Assert(Source <> nil);
  TotalLength := 1;
  for I := 0 to Source.Count - 1 do
    if Source[I] = '' then
      raise EJclStringError.CreateResRec(@RsInvalidEmptyStringItem)
    else
      Inc(TotalLength, StrLen(PChar(Source[I])) + 1);
  AllocateMultiSz(Dest, TotalLength);
  P := Dest;
  for I := 0 to Source.Count - 1 do
  begin
    P := StrECopy(P, PChar(Source[I]));
    Inc(P);
  end;
  P^ := #0;
  Result := Dest;
end;

procedure MultiSzToStrings(const Dest: TStrings; const Source: PMultiSz);
var
  P: PMultiSz;
begin
  Assert(Dest <> nil);
  Dest.BeginUpdate;
  try
    Dest.Clear;
    if Source <> nil then
    begin
      P := Source;
      while P^ <> #0 do
      begin
        Dest.Add(P);
        P := StrEnd(P);
        Inc(P);
      end;
    end;
  finally
    Dest.EndUpdate;
  end;
end;

function MultiSzLength(const Source: PMultiSz): Integer;
var
  P: PMultiSz;
begin
  Result := 0;
  if Source <> nil then
  begin
    P := Source;
    repeat
      Inc(Result, StrLen(P) + 1);
      P := StrEnd(P);
      Inc(P);
    until P^ = #0;
    Inc(Result);
  end;
end;

procedure AllocateMultiSz(var Dest: PMultiSz; Len: Integer);
begin
  if Len > 0 then
    GetMem(Dest, Len * SizeOf(Char))
  else
    Dest := nil;
end;

procedure FreeMultiSz(var Dest: PMultiSz);
begin
  if Dest <> nil then
    FreeMem(Dest);
  Dest := nil;
end;

function MultiSzDup(const Source: PMultiSz): PMultiSz;
var
  Len: Integer;
begin
  if Source <> nil then
  begin
    Len := MultiSzLength(Source);
    AllocateMultiSz(Result, Len);
    Move(Source^, Result^, Len * SizeOf(Char));
  end
  else
    Result := nil;
end;

function WideStringsToWideMultiSz(var Dest: PWideMultiSz; const Source: TWideStrings): PWideMultiSz;
var
  I, TotalLength: Integer;
  P: PWideMultiSz;
begin
  Assert(Source <> nil);
  TotalLength := 1;
  for I := 0 to Source.Count - 1 do
    if Source[I] = '' then
      raise EJclStringError.CreateResRec(@RsInvalidEmptyStringItem)
    else
      Inc(TotalLength, StrLenW(PWideChar(Source[I])) + 1);
  AllocateWideMultiSz(Dest, TotalLength);
  P := Dest;
  for I := 0 to Source.Count - 1 do
  begin
    P := StrECopyW(P, PWideChar(Source[I]));
    Inc(P);
  end;
  P^:= #0;
  Result := Dest;
end;

procedure WideMultiSzToWideStrings(const Dest: TWideStrings; const Source: PWideMultiSz);
var
  P: PWideMultiSz;
begin
  Assert(Dest <> nil);
  Dest.BeginUpdate;
  try
    Dest.Clear;
    if Source <> nil then
    begin
      P := Source;
      while P^ <> #0 do
      begin
        Dest.Add(P);
        P := StrEndW(P);
        Inc(P);
      end;
    end;
  finally
    Dest.EndUpdate;
  end;
end;

function WideMultiSzLength(const Source: PWideMultiSz): Integer;
var
  P: PWideMultiSz;
begin
  Result := 0;
  if Source <> nil then
  begin
    P := Source;
    repeat
      Inc(Result, StrLenW(P) + 1);
      P := StrEndW(P);
      Inc(P);
    until P^ = #0;
    Inc(Result);
  end;
end;

procedure AllocateWideMultiSz(var Dest: PWideMultiSz; Len: Integer);
begin
  if Len > 0 then
    GetMem(Dest, Len * SizeOf(WideChar))
  else
    Dest := nil;
end;

procedure FreeWideMultiSz(var Dest: PWideMultiSz);
begin
  if Dest <> nil then
    FreeMem(Dest);
  Dest := nil;
end;

function WideMultiSzDup(const Source: PWideMultiSz): PWideMultiSz;
var
  Len: Integer;
begin
  if Source <> nil then
  begin
    Len := WideMultiSzLength(Source);
    AllocateWideMultiSz(Result, Len);
    Move(Source^, Result^, Len * SizeOf(WideChar));
  end
  else
    Result := nil;
end;

//=== TStrings Manipulation ==================================================

procedure StrToStrings(S, Sep: AnsiString; const List: TStrings; const AllowEmptyString: Boolean = True);
var
  I, L: Integer;
  Left: AnsiString;
begin
  Assert(List <> nil);
  List.BeginUpdate;
  try
    List.Clear;
    L := Length(Sep);
    I := Pos(Sep, S);
    while I > 0 do
    begin
      Left := StrLeft(S, I - 1);
      if (Left <> '') or AllowEmptyString then
        List.Add(Left);
      System.Delete(S, 1, I + L - 1);
      I := Pos(Sep, S);
    end;
    if S <> '' then
      List.Add(S);  // Ignore empty strings at the end.
  finally
    List.EndUpdate;
  end;
end;

procedure StrIToStrings(S, Sep: AnsiString; const List: TStrings; const AllowEmptyString: Boolean = True);
var
  I, L: Integer;
  LowerCaseStr: string;
  Left: AnsiString;
begin
  Assert(List <> nil);
  LowerCaseStr := StrLower(S);
  Sep := StrLower(Sep);
  L := Length(Sep);
  I := Pos(Sep, LowerCaseStr);
  List.BeginUpdate;
  try
    List.Clear;
    while I > 0 do
    begin
      Left := StrLeft(S, I - 1);
      if (Left <> '') or AllowEmptyString then
        List.Add(Left);
      System.Delete(S, 1, I + L - 1);
      System.Delete(LowerCaseStr, 1, I + L - 1);
      I := Pos(Sep, LowerCaseStr);
    end;
    if S <> '' then
      List.Add(S);  // Ignore empty strings at the end.
  finally
    List.EndUpdate;
  end;
end;

function StringsToStr(const List: TStrings; const Sep: AnsiString; const AllowEmptyString: Boolean): AnsiString;
var
  I, L: Integer;
begin
  Result := '';
  for I := 0 to List.Count - 1 do
  begin
    if (List[I] <> '') or AllowEmptyString then
    begin
      // don't combine these into one addition, somehow it hurts performance
      Result := Result + List[I];
      Result := Result + Sep;
    end;
  end;
  // remove terminating separator
  if List.Count <> 0 then
  begin
    L := Length(Sep);
    System.Delete(Result, Length(Result) - L + 1, L);
  end;
end;

procedure TrimStrings(const List: TStrings; DeleteIfEmpty: Boolean);
var
  I: Integer;
begin
  Assert(List <> nil);
  List.BeginUpdate;
  try
    for I := List.Count - 1 downto 0 do
    begin
      List[I] := Trim(List[I]);
      if (List[I] = '') and DeleteIfEmpty then
        List.Delete(I);
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TrimStringsRight(const List: TStrings; DeleteIfEmpty: Boolean);
var
  I: Integer;
begin
  Assert(List <> nil);
  List.BeginUpdate;
  try
    for I := List.Count - 1 downto 0 do
    begin
      List[I] := TrimRight(List[I]);
      if (List[I] = '') and DeleteIfEmpty then
        List.Delete(I);
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TrimStringsLeft(const List: TStrings; DeleteIfEmpty: Boolean);
var
  I: Integer;
begin
  Assert(List <> nil);
  List.BeginUpdate;
  try
    for I := List.Count - 1 downto 0 do
    begin
      List[I] := TrimLeft(List[I]);
      if (List[I] = '') and DeleteIfEmpty then
        List.Delete(I);
    end;
  finally
    List.EndUpdate;
  end;
end;

function AddStringToStrings(const S: string; Strings: TStrings; const Unique: Boolean): Boolean;
begin
  Assert(Strings <> nil);
  Result := Unique and (Strings.IndexOf(S) <> -1);
  if not Result then
    Result := Strings.Add(S) > -1;
end;

//=== Miscellaneous ==========================================================

function BooleanToStr(B: Boolean): AnsiString;
const
  Bools: array [Boolean] of PAnsiChar = ('False', 'True');
begin
  Result := Bools[B];
end;

function FileToString(const FileName: AnsiString): AnsiString;
var
  fs: TFileStream;
  len: Integer;
begin
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    len := fs.Size;
    SetLength(Result, len);
    if len > 0 then
      fs.ReadBuffer(Result[1], len);
  finally
    fs.Free;
  end;
end;

procedure StringToFile(const FileName, Contents: AnsiString);
var
  fs: TFileStream;
  len: Integer;
begin
  fs := TFileStream.Create(FileName, fmCreate);
  try
    len := Length(Contents);
    if len > 0 then
      fs.WriteBuffer(Contents[1], Length(Contents));
  finally
    fs.Free;
  end;
end;

function StrToken(var S: AnsiString; Separator: AnsiChar): AnsiString;
var
  I: Integer;
begin
  I := Pos(Separator, S);
  if I <> 0 then
  begin
    Result := Copy(S, 1, I - 1);
    Delete(S, 1, I);
  end
  else
  begin
    Result := S;
    S := '';
  end;
end;

procedure StrTokens(const S: AnsiString; const List: TStrings);
var
  Start: PChar;
  Token: AnsiString;
  Done: Boolean;
begin
  Assert(List <> nil);
  if List = nil then
    Exit;

  List.BeginUpdate;
  try
    List.Clear;
    Start := Pointer(S);
    repeat
      Done := StrWord(Start, Token);
      if Token <> '' then
        List.Add(Token);
    until Done;
  finally
    List.EndUpdate;
  end;
end;

procedure StrTokenToStrings(S: AnsiString; Separator: AnsiChar; const List: TStrings);
var
  Token: AnsiString;
begin
  Assert(List <> nil);

  if List = nil then
    Exit;

  List.BeginUpdate;
  try
    List.Clear;
    while S <> '' do
    begin
      Token := StrToken(S, Separator);
      List.Add(Token);
    end;
  finally
    List.EndUpdate;
  end;
end;

function StrWord(var S: PAnsiChar; out Word: AnsiString): Boolean;
var
  Start: PAnsiChar;
begin
  Word := '';
  if S = nil then
  begin
    Result := True;
    Exit;
  end;
  Start := nil;
  Result := False;
  while True do
  begin
    case S^ of
      #0:
        begin
          if Start <> nil then
            SetString(Word, Start, S - Start);
          Result := True;
          Exit;
        end;
      AnsiSpace, AnsiLineFeed, AnsiCarriageReturn:
        begin
          if Start <> nil then
          begin
            SetString(Word, Start, S - Start);
            Exit;
          end
          else
            while (S^ in [AnsiSpace, AnsiLineFeed, AnsiCarriageReturn]) do
              Inc(S);
        end;
    else
      if Start = nil then
        Start := S;
      Inc(S);
    end;
  end;
end;

function StrToFloatSafe(const S: AnsiString): Float;
var
  Temp: AnsiString;
  I, J, K: Integer;
  SwapSeparators, IsNegative: Boolean;
begin
  Temp := S;
  SwapSeparators := False;

  IsNegative := False;
  J := 0;
  for I := 1 to Length(Temp) do
  begin
    if Temp[I] = '-' then
      IsNegative := not IsNegative
    else
      if not (Temp[I] in [' ', '(', '+']) then
      begin
        // if it appears prior to any digit, it has to be a decimal separator
        SwapSeparators := Temp[I] = ThousandSeparator;
        J := I;
        Break;
      end;
  end;

  if not SwapSeparators then
  begin
    K := CharPos(Temp, DecimalSeparator);
    SwapSeparators :=
      // if it appears prior to any digit, it has to be a decimal separator
      (K > J) and
      // if it appears multiple times, it has to be a thousand separator
      ((StrCharCount(Temp, DecimalSeparator) > 1) or
      // we assume (consistent with Windows Platform SDK documentation),
      // that thousand separators appear only to the left of the decimal
      (K < CharPos(Temp, ThousandSeparator)));
  end;

  if SwapSeparators then
  begin
    // assume a numerical string from a different locale,
    // where DecimalSeparator and ThousandSeparator are exchanged
    for I := 1 to Length(Temp) do
      if Temp[I] = DecimalSeparator then
        Temp[I] := ThousandSeparator
      else
        if Temp[I] = ThousandSeparator then
          Temp[I] := DecimalSeparator;
  end;

  Temp := StrKeepChars(Temp, ['0'..'9', DecimalSeparator]);

  if Length(Temp) > 0 then
  begin
    if Temp[1] = DecimalSeparator then
      Temp := '0' + Temp;
    if Temp[length(Temp)] = DecimalSeparator then
      Temp := Temp + '0';
    Result := StrToFloat(Temp);
    if IsNegative then
      Result := -Result;
  end
  else
    Result := 0.0;
end;

function StrToIntSafe(const S: AnsiString): Integer;
begin
  Result := Trunc(StrToFloatSafe(S));
end;

procedure StrNormIndex(const StrLen: integer; var Index: integer; var Count: integer); overload;
begin
   Index := Max(1, Min(Index, StrLen+1));
   Count := Max(0, Min(Count, StrLen+1 - Index));
end;

initialization
  LoadCharTypes;  // this table first
  LoadCaseMap;    // or this function does not work

// History:

// MT:

// - StrIToStrings default parameter now true
// - StrToStrings default parameter now true
// - Rewrote StrSmartCase to fix a bug.
// - Fixed a bug in StrIsAlphaNumUnderscore
// - Fixed a bug in StrIsSubset
// - Simplified StrLower
// - Fixed a bug in StrRepeatLength
// - Fixed a bug in StrLastPos
// - Added function StrTrimCharsRight (Leonard Wennekers)
// - Added function StrTrimCharsLeft (Leonard Wennekers)
// - Added StrNormIndex function (Alexander Radchenko)
// - Changed Assert in StrTokens/ to If List <> nil
// - Deleted an commented out version of StrReplace. If anyone ever want to finish the old
//   version please go the archive version 0.39
// - Modified StrFillChar a little bit (added an if for count > 0)
// - StrCharPosLower (Jean-Fabien Connault)
// - StrCharPosUpper (Jean-Fabien Connault)
// - Changed to 100 chars per line style
// - Note to Marcel:  Have a look at StrToStrings and StrItoStrings. They are untested but
//                    should work more or less equal to the BreakApart functions by JFC.
// - Changed StrNPos for special case
// - Changed StrIPos for special case
// - Fixed a bug in CharPos : didn'T work if index = length(s)
// - Fixed a bug in CharIPos : didn'T work if index = length(s)

// 2003-02-25, Robert Rossmair
//  - Linux port (implemented LoadCharTypes & LoadCaseMap)

// 2002-01-20, Marcel van Brakel
//  - added StrIToStrings to interface section
//  - added AllowEmptyString parameter to StringsToStr function
//  - added AddStringToStrings() by Jeff

// $Log$
// Revision 1.35  2005/02/24 16:34:40  marquardt
// remove divider lines, add section lines (unfinished)
//
// Revision 1.34  2005/02/05 03:45:35  rrossmair
// - fixed issue #0002455 (Calculation of ResultLength inappropriate in StrReplace)
//
// Revision 1.33  2005/01/06 18:48:31  marquardt
// AnsiLineBreak, AnsiLineFeed, AnsiCarriageReturn, AnsiCrLf moved to JclBase JclStrings now reexports the names
//
// Revision 1.32  2004/12/23 04:31:43  rrossmair
// - check-in for JCL 1.94 RC 1
//
// Revision 1.31  2004/10/18 04:54:42  marquardt
// remove PH contributor
//
// Revision 1.30  2004/10/17 20:25:21  mthoma
// style cleaning, adjusting contributors
//
// Revision 1.29  2004/10/11 14:54:38  marquardt
// MultiSz finetuning
//
// Revision 1.28  2004/10/11 08:13:03  marquardt
// cleaning of JclStrings
//
// Revision 1.27  2004/10/10 12:52:12  marquardt
// DestroyEnvironmentBlock introduced
//
// Revision 1.26  2004/09/30 13:11:27  marquardt
// remove contributions
//
// Revision 1.25  2004/09/30 07:50:29  marquardt
// remove contributions
//
// Revision 1.24  2004/08/03 07:22:37  marquardt
// resourcestring cleanup
//
// Revision 1.23  2004/07/30 07:20:25  marquardt
// fixing TStringLists, adding BeginUpdate/EndUpdate
//
// Revision 1.22  2004/06/14 06:24:52  marquardt
// style cleaning IFDEF
//
// Revision 1.21  2004/05/30 23:54:42  rrossmair
// Processed documentation TODOs
//
// Revision 1.20  2004/05/08 08:44:17  rrossmair
// introduced & applied symbol HAS_UNIT_LIBC
//
// Revision 1.19  2004/05/06 16:22:27  rrossmair
// fixed LoadCaseMap for Kylix
//
// Revision 1.18  2004/05/06 05:09:55  rrossmair
// Changes for FPC v1.9.4 compatibility
//
// Revision 1.17  2004/05/05 00:11:24  mthoma
// Updated headers: Added donors as contributors, adjusted the initial authors, added cvs names when they were not obvious. Changed $data to $date where necessary,
//
// Revision 1.16  2004/04/19 06:12:34  rrossmair
// AddStringToStrings help TODO done
//
// Revision 1.15  2004/04/14 20:39:59  mthoma
// Reintroduced StrIsNumber as StrConsistsofNumberChars, copied local function StrEndW from JclUnicode to get rid of that dependency.
//
// Revision 1.14  2004/04/12 22:07:45
// Bugfix: StringsToMultiString, MultiStringToStrings,
//         empty list entries are not allowed
// Add: StringsToMultiWideString, MultiWideStringToStrings
//
// Revision 1.13  2004/04/11 15:58:25  mthoma
// Fixed #1119. Removed StrIsNumber (see bugnote), renamed CharIsNumber to CharisNumberChar. Changed some Strings to AnsiString (unit now compiles also in H- mode).
//
// Revision 1.12  2004/04/09 20:35:14  mthoma
// Added StrLastPos. changed $Data$ to $Date$
//
// Revision 1.11  2004/04/08 19:40:26  mthoma
// Fixed 0000947, 0001060 (StrBetween with same start/end symbol problem). Added a note to the docs.
//
// Revision 1.10  2004/04/06 04:31:32
// Add functions for String <--> MultiString conversion
//

end.
