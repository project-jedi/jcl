{******************************************************************************}
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.0 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JclStrings.pas.                                         }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{ Last modified: August 22, 2000                                               }
{                                                                              }
{******************************************************************************}

unit JclStrings;

{$I JCL.INC}

{
   (Azret)
   o CharIsAlphaNum was using AND, should be OR. (Fixed)
   o Modified StrLength to use AnsiLnOffset constant instead of
     hardcoded 4
   o AnsiOctalDigits was renamed to AnsiOctDigits
   o AnsiOctDigits and AnsiHexDigits changed to sets (Robert Marquardt)

   o StrTokens (Azret)
   o StrWord: tokenizes the string. Returns True if end of string is reached.
            see StrTokens for an example.

   o StrReplace  (Robert Lee)
   o StrLen (Robert Lee)

   o TrimStrings (Anthony Steele)
   o RepeatStr   (Anthony Steele)
                 Az: renamed to StrRepeat,
                 Az: Re-implemented to use move proc. instead of Result + Result

   o fixes:   StrCompareRange, (Az)
   o fixed:   StrLen  (Az)
   o added:   AnsiLineBreak (Robert Marquardt)
   o changed: #13, #10 to AnsiCarriageReturn, AnsiLineFeed (Robert Marquardt)
   o improved: StrFind
             StrFind now does not need the string to be null terminated you can use
             it on any ansistring buffer.
}

interface

uses
  Classes, SysUtils;

type
  TAnsiStrRec = packed record
    AllocSize: Longint;
    RefCount: Longint;
    Length: Longint;
  end;

const
  AnsiNull           = AnsiChar(#0);
  AnsiBell           = AnsiChar(#7);
  AnsiBackspace      = AnsiChar(#8);
  AnsiTab            = AnsiChar(#9);
  AnsiLineFeed       = AnsiChar(#10);
  AnsiVerticalTab    = AnsiChar(#11);
  AnsiFormFeed       = AnsiChar(#12);
  AnsiCarriageReturn = AnsiChar(#13);
  AnsiCrLf           = AnsiString(#13#10);
  AnsiEndOfFile      = AnsiChar(#26);
  AnsiEscape         = AnsiChar(#27);
  AnsiSpace          = AnsiChar(' ');
  AnsiComma          = AnsiChar(',');
  AnsiBackslash      = AnsiChar('\');
  AnsiForwardSlash   = AnsiChar('/');

  {$IFDEF WIN32}
  AnsiLineBreak      = AnsiCrLf;
  {$ENDIF}

  AnsiSigns          = ['-','+'];
  AnsiWhiteSpace     = [AnsiTab, AnsiLineFeed, AnsiVerticalTab, AnsiFormFeed,
                        AnsiCarriageReturn, AnsiSpace];

  AnsiDecDigits      = ['0'..'9'];
  AnsiOctDigits      = ['0'..'7'];
  AnsiHexDigits      = ['0'..'9', 'A'..'F', 'a'..'f'];

{ String Routines }

procedure StrAddRef(var S: AnsiString);
function StrAllocSize(const S: AnsiString): Longint;
function StrCharCount(const S: AnsiString; C: AnsiChar): Integer;
function StrCompare(const S1, S2: AnsiString): Integer;
function StrCompareRange(const S1, S2: AnsiString; const Index, Count: Integer): Integer;
function StrDoubleQuote(const S: AnsiString): AnsiString;
function StrEnsureSuffix(const Suffix, Text: AnsiString): AnsiString;
function StrEnsurePrefix(const Prefix, Text: AnsiString): AnsiString;
function StrFind(const Substr, Str: AnsiString; const Index: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Integer;
function StrIsAlpha(const S: AnsiString): Boolean;
function StrIsAlphaNum(const S: AnsiString): Boolean;
function StrIsNumber(const S: AnsiString): Boolean;
function StrIsSubset(const S: AnsiString; ValidChars: TSysCharSet): Boolean;
function StrLastPos(const SubStr, S: AnsiString): Integer;
function StrLeft(const S: AnsiString; Count: Integer): AnsiString;
function StrLen(S: PChar): Integer;  {Robert Lee}
function StrLength(const S: AnsiString): Longint;
procedure StrLower(var S: AnsiString);
procedure StrLowerBuff(S: PAnsiChar);
function StrMatch(const Substr, Str: AnsiString; const Index: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Integer;
function StrMid(const S: AnsiString; Start, Count: Integer): AnsiString;
procedure StrMove(var Dest: AnsiString; const Source: AnsiString; const ToIndex,
  FromIndex, Count: Integer);
function StrPadLeft(const S: AnsiString; Len: Integer; C: AnsiChar {$IFDEF SUPPORTS_DEFAULTPARAMS} = AnsiSpace {$ENDIF}): AnsiString;
function StrPadRight(const S: AnsiString; Len: Integer; C: AnsiChar {$IFDEF SUPPORTS_DEFAULTPARAMS} = AnsiSpace {$ENDIF}): AnsiString;
function StrSearch(const Substr, Str: AnsiString; const Index: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Integer;
function StrRefCount(const S: AnsiString): Longint;
function StrRemoveChars(const S: AnsiString; Chars: TSysCharSet): AnsiString;
procedure StrReplace(var S: AnsiString; const Search, Replace: AnsiString); {Robert Lee}
function StrRepeat(const S: AnsiString; Count: Integer): AnsiString;
procedure StrResetLength(var S: AnsiString);
function StrReverse(const S: AnsiString): AnsiString;
procedure StrReverseInPlace(var S: AnsiString);
function StrRight(const S: AnsiString; Count: Integer): AnsiString;
function StrSame(const S1, S2: AnsiString): Boolean;
function StrSingleQuote(const S: AnsiString): AnsiString;
function StrSmartCase(const S: AnsiString; Delimiters: TSysCharSet): AnsiString;
function StrToken(var S: AnsiString; Separator: AnsiChar): AnsiString;
procedure StrTokens(const S: AnsiString; List: TStrings);
procedure StrTokenToStrings(S: AnsiString; Separator: AnsiChar; List: TStrings);
procedure StrToStrings(S: AnsiString; Sep: AnsiString; List: TStrings);
function StringsToStr(List: TStrings; Sep: AnsiString): AnsiString;
function StrTrimQuotes(const S: AnsiString): AnsiString;
function StrQuote(const S: AnsiString; C: Char): AnsiString;
procedure StrUpper(var S: AnsiString);
procedure StrUpperBuff(S: PAnsiChar);
function StrWord(var S: PAnsiChar; out Word: AnsiString): Boolean;
function BooleanToStr(B: Boolean): string;

{ Character Routines }

{$IFDEF WIN32}
const
  C1_UPPER  = $0001; // Uppercase
  C1_LOWER  = $0002; // Lowercase
  C1_DIGIT  = $0004; // Decimal digits
  C1_SPACE  = $0008; // Space characters
  C1_PUNCT  = $0010; // Punctuation
  C1_CNTRL  = $0020; // Control characters
  C1_BLANK  = $0040; // Blank characters
  C1_XDIGIT = $0080; // Hexadecimal digits
  C1_ALPHA  = $0100; // Any linguistic character: alphabetic, syllabary, or ideographic
{$ENDIF}

function CharType(const C: AnsiChar): Word;
function CharIsAlpha(const C: AnsiChar): Boolean;
function CharIsUpper(const C: AnsiChar): Boolean;
function CharIsLower(const C: AnsiChar): Boolean;
function CharIsAlphaNum(const C: AnsiChar): Boolean;
function CharIsBlank(const C: AnsiChar): Boolean;
function CharIsPunct(const C: AnsiChar): Boolean;
function CharIsPrint(const C: AnsiChar): Boolean;
function CharIsControl(const C: AnsiChar): Boolean;
function CharIsDigit(const C: AnsiChar): Boolean;
function CharIsNumber(const C: AnsiChar): Boolean;
function CharIsReturn(const C: AnsiChar): Boolean;
function CharIsSpace(const C: AnsiChar): Boolean;
function CharIsWhiteSpace(const C: AnsiChar): Boolean;
function CharLower(const C: AnsiChar): AnsiChar;
function CharUpper(const C: AnsiChar): AnsiChar;
function CharPos(const S: AnsiString; const C: AnsiChar;
  const Index: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Integer;
function CharReplace(var S: AnsiString; const Search, Replace: AnsiChar): Integer;
function CharToggleCase(const C: AnsiChar): AnsiChar;

{ MultiSz Routines }

function StringsToMultiSz(var Dest: PChar; const Source: TStrings): PChar;
procedure MultiSzToStrings(const Dest: TStrings; const Source: PChar);
procedure FreeMultiSz(var Dest: PChar);

{$IFDEF SUPPORTS_DYNAMICARRAYS}

{ PCharVector }

type
  PCharVector = ^PChar;

function StringsToPCharVector(var Dest: PCharVector;
  const Source: TStrings): PCharVector;
function PCharVectorCount(const Source: PCharVector): Integer;
procedure PCharVectorToStrings(const Dest: TStrings; const Source: PCharVector);
procedure FreePCharVector(var Dest: PCharVector);

{$ENDIF}

function StrStringToEscaped(const S: AnsiString): AnsiString;
function StrEscapedToString(const S: AnsiString): AnsiString;

{ String List }

procedure TrimStrings(List: TStrings);

implementation

uses
  {$IFDEF WIN32}
  Windows;
  {$ENDIF}

//==============================================================================
// Internal
//==============================================================================

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

//------------------------------------------------------------------------------

{$IFDEF WIN32}

procedure LoadCharTypes;
var
  CurrChar: AnsiChar;
  CurrType: Word;
begin
  for CurrChar := Low(AnsiChar) to High(AnsiChar) do
  begin
     GetStringTypeExA(LOCALE_USER_DEFAULT, CT_CTYPE1, @CurrChar, SizeOf(AnsiChar), CurrType);
     AnsiCharTypes[CurrChar] := CurrType;
  end;
end;

{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF WIN32}

procedure LoadCaseMap;
var
  CurrChar, UpCaseChar, LoCaseChar, ReCaseChar: AnsiChar;
begin
  if not AnsiCaseMapReady then
  begin
    for CurrChar := Low(AnsiChar) to High(AnsiChar) do
    begin
      LoCaseChar := CurrChar;
      UpCaseChar := CurrChar;
      Windows.CharLowerBuff(@LoCaseChar, 1);
      Windows.CharUpperBuff(@UpCaseChar, 1);

      if CharIsUpper(CurrChar) then      // Az
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

{$ENDIF}

//------------------------------------------------------------------------------

// Internal utility function
// Uppercases or Lowercases a give AnsiString depending on the
// passed offset. (UpOffset or LoOffset)

procedure StrCase{(var Str: AnsiString; const Offset: Integer)}; assembler;
asm
        {       make sure that the string is not null}

        TEST    EAX, EAX
        JZ      @@StrIsNull

        {       create unique string if this one is ref-counted }

        PUSH    EDX
        CALL    UniqueString
        POP     EDX

        {       make sure that the new string is not null  }

        TEST    EAX, EAX
        JZ      @@StrIsNull

        {       get the length, and prepare the counter }

        MOV     ECX, [EAX - AnsiStrRecSize].TAnsiStrRec.Length
        DEC     ECX
        JS      @@StrIsNull

        {       ebx will hold the case map, esi pointer to Str }

        PUSH    EBX
        PUSH    ESI

        {       load case map and prepare variables }

        LEA     EBX,[AnsiCaseMap + EDX]
        MOV     ESI, EAX
        XOR     EDX, EDX
        XOR     EAX, EAX

@@NextChar:
        {       get current char from the AnsiString }

        MOV     DL, [ESI]

        {       get corresponding char from the case map }

        MOV     AL, [EBX+EDX]

        {       store it back in the string }

        MOV     [ESI], AL

        {       update the loop counter and check the end of stirng }

        DEC     ECX
        JL      @@Done

        {       do the same thing with next 3 chars  }

        MOV     DL, [ESI + 1]
        MOV     AL, [EBX+EDX]
        MOV     [ESI + 1], AL

        DEC     ECX
        JL      @@Done
        MOV     DL, [ESI + 2]
        MOV     AL, [EBX+EDX]
        MOV     [ESI + 2], AL

        DEC     ECX
        JL      @@Done
        MOV     DL, [ESI + 3]
        MOV     AL, [EBX+EDX]
        MOV     [ESI + 3], AL

        {       point AnsiString to next 4 chars }

        ADD     ESI, 4

        {       update the loop counter and check the end of stirng }

        DEC     ECX
        JGE     @@NextChar

@@Done:
        POP     ESI
        POP     EBX

@@StrIsNull:
end;

//------------------------------------------------------------------------------

// Internal utility function
// Uppercases or Lowercases a give null terminated string depending on the
// passed offset. (UpOffset or LoOffset)

procedure StrCaseBuff{(S: PAnsiChar; const Offset: Integer)}; assembler;
asm
        {       make sure the string is not null}

        TEST    EAX, EAX
        JZ      @@StrIsNull

        {       ebx will hold the case map, esi pointer to Str }

        PUSH    EBX
        PUSH    ESI

        {       load case map and prepare variables }

        LEA     EBX, [AnsiCaseMap + EDX]
        MOV     ESI, EAX
        XOR     EDX, EDX
        XOR     EAX, EAX

@@NextChar:
        {       get current char from the string }

        MOV     DL, [ESI]

        {       check for null char            }

        TEST    DL, DL
        JZ      @@Done

        {       get corresponding char from the case map }

        MOV     AL, [EBX+EDX]

        {       store it back in the string }

        MOV     [ESI], AL

        {       do the same thing with next 3 chars  }

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

        {       point string to next 4 chars }

        ADD     ESI, 4

        JMP     @@NextChar

@@Done:
        POP     ESI
        POP     EBX

@@StrIsNull:
end;

//==============================================================================
// String
//==============================================================================

procedure StrAddRef(var S: AnsiString);
var
  Foo: AnsiString;
begin
  Foo := S;
  Pointer(Foo) := nil;
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

procedure StrLowerBuff(S: PAnsiChar); assembler;
asm
        {       StrCaseBuff(S, LoOffset)  }

        XOR     EDX, EDX                {MOV     EDX,LoOffset}
        JMP     StrCaseBuff
end;

//------------------------------------------------------------------------------

procedure StrUpperBuff(S: PAnsiChar); assembler;
asm
        {       StrCaseBuff(S, UpOffset)  }

        MOV     EDX, AnsiUpOffset
        JMP     StrCaseBuff
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

function BooleanToStr(B: Boolean): string;
const
  Bools: array [Boolean] of PChar = ('False', 'True');
begin
  Result := Bools[B];
end;

//------------------------------------------------------------------------------

function StrCharCount(const S: AnsiString; C: AnsiChar): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    if S[I] = C then
      Inc(Result);
end;

//------------------------------------------------------------------------------

function StrCompareRange(const S1, S2: AnsiString; const Index, Count: Integer): Integer; assembler;
asm
        TEST    EAX, EAX
        JZ      @@StrNull

        TEST    EDX, EDX
        JZ      @@StrNull

        DEC     ECX
        JS      @@StrNull

        PUSH    EBX
        PUSH    ESI      {Az May 4, push esi was after push edi}
        PUSH    EDI

        MOV     EBX, Count
        DEC     EBX
        JS      @@NoWork

        MOV     ESI, EAX
        MOV     EDI, EDX

        MOV     EDX, [ESI - AnsiStrRecSize].TAnsiStrRec.Length

        { # of chars in Str1 - (Index - 1) }
        SUB     EDX, ECX
        JLE     @@NoWork

        { # of chars in Str1 - (Count - 1) }
        SUB     EDX, EBX
        JLE     @@NoWork

        { move to index'th char }
        ADD     ESI, ECX

        MOV     ECX, [EDI - AnsiStrRecSize].TAnsiStrRec.Length
        DEC     ECX
        JS      @@NoWork

        { if Length(Str2) > Count then ECX := Count else ECX := Length(Str2 ) }
        CMP     ECX, EBX
        JLE     @@Skip1
        MOV     ECX, EBX

@@Skip1:
        XOR     EAX, EAX
        XOR     EDX, EDX

@@Loop:
        MOV     AL, [ESI]
        INC     ESI      {Az: May 4}
        MOV     DL, [EDI]
        INC     EDI      {Az: May 4}

        CMP     AL, DL
        JNE     @@MisMatch

        DEC     ECX
        JG      @@Loop

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

@@StrNull:
        MOV     EAX, -1

@@Exit:
end;

//------------------------------------------------------------------------------

function StrCompare(const S1, S2: AnsiString): Integer; assembler;
asm
        {       check if pointers are equal }

        CMP     EAX, EDX
        JE      @@Equal

        {       if str1 is null return - Length(str2) }

        TEST    EAX, EAX
        JZ      @@Str1Null

        {       if str1 is null return  Length(str1) }

        TEST    EDX, EDX
        JZ      @@Str2Null

        {       EBX will hold case map, ESI Str1, EDI Str2 }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        {       move AnsiString pointers }

        MOV     ESI, EAX
        MOV     EDI, EDX

        {       get the length of strings }

        MOV     EAX, [ESI-AnsiStrRecSize].TAnsiStrRec.Length
        MOV     EDX, [EDI-AnsiStrRecSize].TAnsiStrRec.Length

        {       exit if Length(Str1) <> Length(Str2) }

        CMP     EAX, EDX
        JNE     @@MissMatch

        {       check the length just in case  }

        DEC     EDX
        JS      @@InvalidStr

        DEC     EAX
        JS      @@InvalidStr

        {       load case map }

        LEA     EBX, AnsiCaseMap

        {       make ECX our loop counter }

        MOV     ECX, EAX

        {       clear working regs        }

        XOR     EAX, EAX
        XOR     EDX, EDX

        {       get last chars }

        MOV     AL, [ESI+ECX]
        MOV     DL, [EDI+ECX]

        {       lower case them   }

        MOV     AL, [EBX+EAX]
        MOV     DL, [EBX+EDX]

        {       compare them   }

        CMP     AL, DL
        JNE     @@MissMatch

        {       if there was only 1 char then exit  }

        JECXZ   @@Match

@@NextChar:
        {       case sensitive compare of strings }

        REPE    CMPSB
        JE      @@Match

        {       if there was a missmatch try case insensitive compare,
                get the chars.   }

        MOV     AL, [ESI-1]
        MOV     DL, [EDI-1]

        {       lowercase them     }

        MOV     AL, [EBX+EAX]
        MOV     DL, [EBX+EDX]

        {       if equal continue comparing }

        CMP     AL, DL
        JE      @@NextChar

        {       if we make it here then strings don't match,
                return the difference. }

@@MissMatch:
        SUB     EAX, EDX
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@Match:
        {       match, return 0 }

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
        {       return = - Length(str2); }

        MOV     EDX, [EDX-AnsiStrRecSize].TAnsiStrRec.Length
        SUB     EAX, EDX
        RET

@@Str2Null:
        {       return = Length(str2); }

        MOV     EAX, [EAX-AnsiStrRecSize].TAnsiStrRec.Length
        RET

@@Equal:
        XOR     EAX, EAX
end;

//------------------------------------------------------------------------------

function StrDoubleQuote(const S: AnsiString): AnsiString;
begin
  Result := '"' + S + '"';
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

function StrFind(const Substr, Str: AnsiString; const Index: Integer): Integer; assembler;
const
   SearchChar: Byte = 0;
   NumberOfChars: Integer = 0;
asm
        {       if SubStr = '' then
                begin
                  Result := 0;
                  Exit;
                end;                            }

        TEST    EAX, EAX
        JZ      @@SubstrIsNull

        {       if Str = '' then
                begin
                  Result := 0;
                  Exit;
                end;                            }

        TEST    EDX, EDX
        JZ      @@StrIsNull

        {       Index := Index - 1;
                if Index < 0 then
                beign
                  Result := 0;
                  Exit;
                end;    }

        DEC     ECX
        JL      @@IndexIsSmall

        {       EBX will hold the case table, ESI pointer to Str, EDI pointer
                to Substr and - # of chars in Substr to compare }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        {       set the string pointers }

        MOV     ESI, EDX
        MOV     EDI, EAX

        {       save the Index in EDX }

        MOV     EDX, ECX

        {      temporary get the length of Substr and Str }

        MOV     EBX, [EDI-AnsiStrRecSize].TAnsiStrRec.Length
        MOV     ECX, [ESI-AnsiStrRecSize].TAnsiStrRec.Length

        {       save the address of Str to compute the result }

        PUSH    ESI

        {       dec the length of Substr because the first char is brought
                out of it }

        DEC     EBX
        JS      @@NotFound

        {        #positions in Str to look at
                 = Length(Str) - Length(Substr) - Index - 2 }

        SUB     ECX, EBX
        JLE     @@NotFound

        SUB     ECX, EDX
        JLE     @@NotFound

        {       # of chars in Substr to compare }

        MOV     NumberOfChars, EBX

        {       point Str to Index'th char }

        ADD     ESI, EDX

        {        load case map into EBX, and clear EAX  }

        LEA     EBX, AnsiCaseMap
        XOR     EAX, EAX
        XOR     EDX, EDX

        {       bring the first char out of the Substr and point
                Substr to the next char }

        MOV     DL, [EDI]
        INC     EDI

        {       lower case it   }

        MOV     DL, [EBX+EDX]
        MOV     SearchChar, DL

        JMP     @@Find

@@FindNext:

        {       update the loop counter and check the end of AnsiString.
                if we reached the end, Substr was not found. }

        DEC     ECX
        JL      @@NotFound

@@Find:

        {       get current char from the AnsiString, and
                point Str to the next one. }

        MOV     AL, [ESI]
        INC     ESI


        {       lower case current char     }

        MOV     AL, [EBX+EAX]

        {       does current char match primary search char?
                if not, go back to the main loop }
        CMP     AL, SearchChar
        JNE     @@FindNext
        

@@Compare:

        {       # of chars in Substr to compare }

        MOV     EDX, NumberOfChars

@@CompareNext:

        {       dec loop counter and check if we reached the end. If yes then
                we found it.}

        DEC     EDX
        JL      @@Found

        {       get the chars from Str and Substr, if they are equal then
                continue comparing. }

        MOV     AL, [ESI+EDX]
        CMP     AL, [EDI+EDX]
        JE      @@CompareNext

        {       otheriwise try the reverse case. If they still don't
                match go back to the Find loop }

        MOV     AL, [EBX+EAX+AnsiReOffset]
        CMP     AL, [EDI+EDX]
        JNE     @@FindNext

        {       if they matched, continue comparing }

        JMP     @@CompareNext

@@Found:
        {       we found it, calculate the result. }

        MOV     EAX, ESI
        POP     ESI
        SUB     EAX, ESI

        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@NotFound:

        {       not found it, clear the result. }

        XOR     EAX, EAX
        POP     ESI
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@IndexIsSmall:
@@StrIsNull:

        {       clear the result. }

        XOR     EAX, EAX

@@SubstrIsNull:
@@Exit:
end;

//------------------------------------------------------------------------------

function StrMatch(const Substr, Str: AnsiString; const Index: Integer): Integer; assembler;
asm
        {       make sure that strings are not null}

        TEST    EAX, EAX
        JZ      @@SubstrIsNull

        TEST    EDX, EDX
        JZ      @@StrIsNull

        {       limit index to satisfy 1 <= index, and dec it }

        DEC     ECX
        JL      @@IndexIsSmall

        {       EBX will hold the case table, ESI pointer to Str, EDI pointer
                to Substr and EBP # of chars in Substr to compare }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        {       set the AnsiString pointers }

        MOV     ESI, EDX
        MOV     EDI, EAX

        {       save the Index in EDX }

        MOV     EDX, ECX

        {       save the address of Str to compute the result }

        PUSH    ESI

        {      temporary get the length of Substr and Str }

        MOV     EBX, [EDI-AnsiStrRecSize].TAnsiStrRec.Length
        MOV     ECX, [ESI-AnsiStrRecSize].TAnsiStrRec.Length

        {       dec the length of Substr because the first char is brought
                out of it }

        DEC     EBX
        JS      @@NotFound

        {        #positions in Str to look at
                 = Length(Str) - Length(Substr) - Index - 2 }

        SUB     ECX, EBX
        JLE     @@NotFound

        SUB     ECX, EDX
        JLE     @@NotFound

        {       # of chars in Substr to compare }

        MOV     EBP, EBX

        {       point Str to Index'th char }

        ADD     ESI, EDX

        {        load case map into EBX, and clear EAX & ECX }

        LEA     EBX, AnsiCaseMap
        XOR     EAX, EAX
        XOR     ECX, ECX

        {       bring the first char out of the Substr and point
                Substr to the next char }

        MOV     CL, [EDI]
        INC     EDI

        {       lower case it   }

        MOV     CL, [EBX+ECX]

@@FindNext:

        {       get the current char from Str into al }

        MOV     AL, [ESI]
        INC     ESI

        {       check the end of AnsiString }

        TEST    AL, AL
        JZ      @@NotFound


        CMP     CL, '*'    // Wild Card?
        JE      @@Compare

        CMP     CL, '?'    // Wild Card?
        JE      @@Compare

        {       lower case current char }

        MOV     AL, [EBX+EAX]

        {       check if the current char matches the primary search char,
                if not continue searching }

        CMP     AL, CL
        JNE     @@FindNext

      @@Compare:

        {       # of chars in Substr to compare }

        MOV     EDX, EBP

@@CompareNext:

        {       dec loop counter and check if we reached the end. If yes then
                we found it.}

        DEC     EDX
        JL      @@Found

        {       get the chars from Str and Substr, if they are equal then
                continue comparing. }

        MOV     AL, [EDI+EDX]     // char from  Substr

        CMP     AL, '*' // wild card?
        JE      @@CompareNext

        CMP     AL, '?' // wild card?
        JE      @@CompareNext

        CMP     AL, [ESI+EDX] // equal to PChar(Str)^ ?
        JE      @@CompareNext

        MOV     AL, [EBX+EAX+AnsiReOffset]  // reverse case?
        CMP     AL, [ESI+EDX]
        JNE     @@FindNext       // if still no, go back to the main loop

        {       if they matched, continue comparing }

        JMP     @@CompareNext

@@Found:
        {       we found it, calculate the result. }

        MOV     EAX, ESI
        POP     ESI
        SUB     EAX, ESI

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@NotFound:

        {       not found it, clear the result. }

        XOR     EAX, EAX
        POP     ESI
        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@IndexIsSmall:
@@StrIsNull:

        {       clear the result. }

        XOR     EAX, EAX

@@SubstrIsNull:
@@Exit:
end;

//------------------------------------------------------------------------------

function StrIsAlpha(const S: AnsiString): Boolean;
var
  I: Integer;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
    if not CharIsAlpha(S[I]) then   // Az
    begin
      Result := False;
      Exit;
    end;
end;

//------------------------------------------------------------------------------

function StrIsAlphaNum(const S: AnsiString): Boolean;
var
  I: Integer;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
    if not CharIsAlphaNum(S[I]) then
    begin
      Result := False;
      Exit;
    end;
end;

//------------------------------------------------------------------------------

function StrIsNumber(const S: AnsiString): Boolean;
var
  I: Integer;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
    if not CharIsNumber(S[I]) then
    begin
      Result := False;
      Exit;
    end;
end;

//------------------------------------------------------------------------------

function StrIsSubset(const S: AnsiString; ValidChars: TSysCharSet): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    if not (S[I] in ValidChars) then
    begin
      Result := False;
      Exit;
    end;
  Result := True;
end;

//------------------------------------------------------------------------------

function StrLastPos(const SubStr, S: AnsiString): Integer;
var
  Last, Current: PChar;
begin
  Result := 0;
  Last := nil;
  Current := PChar(S);
  while (Current <> nil) and (Current^ <> #0) do
  begin
    Current := SysUtils.StrPos(PChar(Current), PChar(SubStr));
    if Current <> nil then
    begin
      Last := Current;
      Inc(Current, Length(SubStr));
    end;
  end;
  if Last <> nil then
    Result := Abs((Longint(PChar(S)) - Longint(Last)) div SizeOf(AnsiChar)) + 1;
end;

//------------------------------------------------------------------------------

function StrLeft(const S: AnsiString; Count: Integer): AnsiString;
begin
  Result := Copy(S, 1, Count);
end;

//------------------------------------------------------------------------------

function StrLen(S: PChar): Integer; assembler;
asm
        TEST    EAX, EAX
        JZ      @@exit


        PUSH    EBX
        MOV     EDX, EAX                  { save pointer }
@L1:    MOV     EBX, [EAX]                { read 4 bytes}
        ADD     EAX, 4                    { increment pointer}
        LEA     ECX, [EBX-$01010101]      { subtract 1 from each byte}
        NOT     EBX                       { invert all bytes}
        AND     ECX, EBX                  { and these two}
        AND     ECX, $80808080            { test all sign bits}
        JZ      @L1                       { no zero bytes, continue loop}
        TEST    ECX, $00008080            { test first two bytes}
        JZ      @L2
        SHL     ECX, 16                   { not in the first 2 bytes}
        SUB     EAX, 2
@L2:    SHL     ECX, 9                    { use carry flag to avoid a branch}
        SBB     EAX, EDX                  { compute length}
        POP     EBX

        { Az: SBB sets zero flag }
        JZ      @@exit
        { do not inlcude null terminator }
        DEC     EAX
@@exit:
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

procedure StrLower(var S: AnsiString); assembler;
asm
        {       StrCase(Str, LoOffset)  }

        XOR     EDX, EDX         { MOV     EDX,LoOffset }
        JMP     StrCase
end;

//------------------------------------------------------------------------------

function StrMid(const S: AnsiString; Start, Count: Integer): AnsiString;
begin
  Result := Copy(S, Start, Count);
end;

//------------------------------------------------------------------------------

procedure StrMove(var Dest: AnsiString; const Source: AnsiString;
  const ToIndex, FromIndex, Count: Integer); assembler;
asm
        {       make sure that Source is not null}

        TEST    EDX, EDX
        JZ      @@NoMove

        {       save params before UniqueString call }

        PUSH    EDX
        PUSH    ECX

        {       create a new AnsiString if Dest if ref-counted }

        CALL    UniqueString

        {       restore params }

        POP     ECX
        POP     EDX

        {       make sure that the new AnsiString is not null }

        TEST    EAX, EAX
        JZ      @@NoMove

        {       ToIndex >= 1 }

        DEC     ECX
        JS      @@NoMove

        {       FromIndex >= 1 }

        DEC     FromIndex
        JS      @@NoMove

        PUSH    EBX
        PUSH    EDI
        PUSH    ESI

        {       AnsiString pointers }

        MOV     ESI, EDX
        MOV     EDI, EAX

        {       get lengths of strings }

        MOV     EBX, [ESI-AnsiStrRecSize].TAnsiStrRec.Length
        MOV     EDX, [EDI-AnsiStrRecSize].TAnsiStrRec.Length

        {       make sure that FromIndex index is <= Length(Source) }

        SUB     EBX, FromIndex
        JLE     @@Skip

        {       make sure that count is less then # of chars availible.
                if Count > Length(Source) then Count := Length(Source) }

        CMP     EBX, Count
        JLE     @@Skip
        MOV     EBX, Count

@@Skip:
        {       make sure that we don't move data beyond Dest AnsiString
                if ToIndex > Length(Dest) then Exit; }

        SUB     EDX, ECX
        JLE     @@Done

        {       make sure that Dest AnsiString is big enough for Count
                after subtructing indexes }

        CMP     EDX, EBX
        JL      @@Done

        {       point Dest to requested index }

        ADD     EDI, ECX

        {       point Source to requested index }

        ADD     ESI, FromIndex

        {       move # of chars to move into ECX and leave EBX we'll need it }

        MOV     ECX, EBX

        {       # of DWORDs we have in Count, the long loop will be
                DWORD by DWORD, Count div 4 }

        SHR     ECX, 2
        JZ      @@moveRest

@@longLoop:

        {       get 4 bytes from source  }

        MOV     EAX, [ESI]

        {       move 'em into dest }

        MOV     [EDI], EAX

        {       check the counter  }

        DEC     ECX
        JZ      @@moveRest4

        {       do the same thing with next 4 bytes }

        MOV     EAX, [ESI+4]
        MOV     [EDI+4], EAX

        {       inc AnsiString pointers by 8 }

        ADD     ESI, 8
        ADD     EDI, 8

        {       check the counter  }

        DEC     ECX
        JNE     @@longLoop


        JMP     @@moveRest

@@moveRest4:
        ADD     ESI, 4
        ADD     EDI, 4

@@moveRest:
        {       get saved count back into ECX }

        MOV     ECX, EBX

        {       clear direction flag so that MOVSB goes forword }

        CLD

        {       get # of chars remaining, Count mod 4 }

        AND     ECX, $03

        {       and move the rest of the Source }

        REP     MOVSB

@@Done:
        POP     EDI
        POP     ESI
        POP     EBX

@@NoMove:
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

function StrSearch(const Substr, Str: AnsiString; const Index: Integer): Integer; assembler;
asm
        {       make sure that strings are not null }

        TEST    EAX, EAX
        JZ      @@SubstrIsNull

        TEST    EDX, EDX
        JZ      @@StrIsNull

        {       limit index to satisfy 1 <= index, and dec it }

        DEC     ECX
        JL      @@IndexIsSmall

        {       ebp will hold # of chars in Substr to compare, esi pointer to Str,
                edi pointer to Substr, ebx primary search char }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        {       set the AnsiString pointers }

        MOV     ESI, EDX
        MOV     EDI, EAX

        {       save the (Index - 1) in edx }

        MOV     EDX, ECX

        {       save the address of Str to compute the result }

        PUSH    ESI

        {       temporary get the length of Substr and Str }

        MOV     EBX, [EDI-AnsiStrRecSize].TAnsiStrRec.Length
        MOV     ECX, [ESI-AnsiStrRecSize].TAnsiStrRec.Length

        {       dec the length of Substr because the first char is brought
                out of it }

        DEC     EBX
        JS      @@NotFound

        {       # of positions in Str to look at
                = Length(Str) - Length(Substr) - Index - 2 }

        SUB     ECX, EBX
        JLE     @@NotFound

        SUB     ECX, EDX
        JLE     @@NotFound

        {       point Str to Index'th char }

        ADD     ESI, EDX

        {       # of chars in Substr to compare }

        MOV     EBP, EBX

        {       clear EAX & ECX (working regs) }

        XOR     EAX, EAX
        XOR     EBX, EBX

        {       bring the first char out of the Substr, and
                point Substr to the next char }

        MOV     BL, [EDI]
        INC     EDI

        {       jump into the loop }

        JMP     @@Find

@@FindNext:

        {       update the loop counter and check the end of AnsiString.
                if we reached the end, Substr was not found. }

        DEC     ECX
        JL      @@NotFound

@@Find:

        {       get current char from the AnsiString, and
                point Str to the next one. }

        MOV     AL, [ESI]
        INC     ESI

        {       does current char match primary search char?
                if not, go back to the main loop }

        CMP     AL, BL
        JNE     @@FindNext

        {       otherwise compare SubStr  }

@@Compare:

        {       move # of char to compare into edx, edx will be our
                compare loop counter. }

        MOV     EDX, EBP

@@CompareNext:

        {      check if we reached the end of Substr. If yes we found it. }

        DEC     EDX
        JL      @@Found

        {       get last chars from Str and SubStr and compare them,
                if they don't match go back to out main loop. }

        MOV     AL, [EDI+EDX]
        CMP     AL, [ESI+EDX]
        JNE     @@FindNext

        {       if they matched, continue comparing }

        JMP     @@CompareNext

@@Found:
        {       we found it, calculate the result and exit. }

        MOV     EAX, ESI
        POP     ESI
        SUB     EAX, ESI

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@NotFound:
        {       not found it, clear result and exit. }

        XOR     EAX, EAX
        POP     ESI
        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@IndexIsSmall:
@@StrIsNull:
        {       clear result and exit. }

        XOR     EAX, EAX

@@SubstrIsNull:
@@Exit:
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

function StrRemoveChars(const S: AnsiString; Chars: TSysCharSet): AnsiString;
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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

procedure StrReplace(var S: AnsiString; const Search, Replace: AnsiString);
var
  SearchPtr: PChar;
  istr, jstr, StrEnd, ReplaceEnd, pTemp : PChar;
  k, l, SearchLen, ReplaceLen: Integer;
  WorkStr: AnsiString;
  C: Char;
begin
  SearchPtr := PChar(Search);

  SearchLen := Length(Search);
  ReplaceLen := Length(Replace);

  StrEnd := @S[Length(S)+1];
  ReplaceEnd := @Replace[ReplaceLen+1];

  Assert(SearchLen > 0, 'Search - must not be empty.');

  // make sure newStr is long enough
  if ReplaceLen > SearchLen then
    SetLength(WorkStr, ((Length(S) div SearchLen)+1) * ReplaceLen)
  else
    SetLength(WorkStr, Length(S));

  iStr := @S[1];
  jStr := @WorkStr[1];

  // primary search char
  C := Search[1];

  // set to stop loop (could be a problem if ref count > 1 )
  StrEnd[0] := c;

  while True do
  begin
    // copy until possible match
    k := 0;
    while iStr[k] <> c do
    begin
      jStr[k] := iStr[k];
      Inc(k);
    end;

    Inc(jStr, k);
    Inc(iStr, k);

    if iStr = StrEnd then
       Break;

    l := StrEnd - iStr;

    k := 1; // First char is already matched.
    if l > SearchLen then
    begin
      // don't need to check l since the zero at end of Search will stop loop
      l := SearchLen;
      pTemp := SearchPtr;  // increases "priority" of SearchPtr

      while iStr[k] = ptemp[k] do
        Inc(k);
    end
    else
      // here we have to check
      while (k < l) and (iStr[k] = SearchPtr[k]) do
        Inc(k);

    if k = l then // match
    begin  // copy
      Inc(iStr, SearchLen);
      k := -ReplaceLen;
      Inc(jstr, ReplaceLen);

      while k < 0 do
      begin
        jstr[k] := ReplaceEnd[k];
        Inc(k);
      end;
    end
    else // no match
    begin
      jStr^ := iStr^;
      Inc(jStr);
      Inc(iStr);
    end;
  end;

  StrEnd[0] := #0;  // return to original state
  SetLength(WorkStr, jstr-PChar(WorkStr));
  S := WorkStr;
end;

//------------------------------------------------------------------------------

procedure StrResetLength(var S: AnsiString);
begin
  SetLength(S, StrLen(PChar(S)));
end;

//------------------------------------------------------------------------------

function StrReverse(const S: AnsiString): AnsiString;
var
  P1, P2: PChar;
  C: AnsiChar;
begin
  Result := S;
  UniqueString(Result);
  P1 := PChar(Result);
  P2 := P1 + SizeOf(AnsiChar) * (Length(Result) - 1);
  while P1 < P2 do
  begin
    C := P1^;
    P1^ := P2^;
    P2^ := C;
    Inc(P1);
    Dec(P2);
  end;
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

function StrRight(const S: AnsiString; Count: Integer): AnsiString;
begin
  Result := Copy(S, Length(S) - Count + 1, Count);
end;

//------------------------------------------------------------------------------

function StrSame(const S1, S2: AnsiString): Boolean;
begin
  Result := StrCompare(S1, S2) = 0;
end;

//------------------------------------------------------------------------------

function StrSingleQuote(const S: AnsiString): AnsiString;
begin
  Result := '''' + S + '''';
end;

//------------------------------------------------------------------------------

function StrSmartCase(const S: AnsiString; Delimiters: TSysCharSet): AnsiString;
var
  Source, Dest: PChar;
begin
  if Delimiters = [] then
    Include(Delimiters, AnsiSpace);
  if S <> '' then
  begin
    SetLength(Result, Length(S));
    Source := PChar(S);
    Dest := PChar(Result);
    Dest^ := UpCase(Source^);
    Inc(Source);
    Inc(Dest);
    while Source^ <> #0 do
    begin
      Dest^ := Source^;
      if Source^ in Delimiters then
      begin
        Inc(Source);
        Inc(Dest);
        Dest^ := UpCase(Source^);
      end;
      Inc(Dest);
      Inc(Source);
    end;
  end;
end;

//------------------------------------------------------------------------------

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

procedure StrTokens(const S: AnsiString; List: TStrings);
var
  Start: PChar;
  Token: string;
  Done: Boolean;
begin
  List.Clear;
  Start := Pointer(S);
  repeat
    Done := StrWord(Start, Token);
    if Token <> '' then
      List.Add(Token);
  until Done;
end;

//------------------------------------------------------------------------------

procedure StrTokenToStrings(S: AnsiString; Separator: AnsiChar; List: TStrings);
var
  Token: AnsiString;
begin
  List.Clear;
  Token := StrToken(S, Separator);
  while Token <> '' do
  begin
    List.Add(Token);
    Token := StrToken(S, Separator);
  end;
end;

//------------------------------------------------------------------------------

procedure StrToStrings(S: AnsiString; Sep: AnsiString; List: TStrings);
var
  I, L: Integer;
  Left: AnsiString;
begin
  List.Clear;
  L := Length(Sep);
  I := Pos(Sep, S);
  while (I > 0) do
  begin
    Left := StrLeft(S, I - 1);
    List.Add(Left);
    Delete(S, 1, I + L - 1);
    I := Pos(Sep, S);
  end;
  if S <> '' then
    List.Add(S);
end;

//------------------------------------------------------------------------------

function StringsToStr(List: TStrings; Sep: AnsiString): AnsiString;
var
  I, L: Integer;
begin
  Result := '';
  for I := 0 to List.Count - 1 do
  begin
    Result := Result + List[I];
    Result := Result + Sep;
  end;
  if List.Count <> 0 then
  begin
    L := Length(Sep);
    Delete(Result, Length(Result) - L + 1, L);
  end;
end;

//------------------------------------------------------------------------------

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
    if (First = Last) and ((First = '''') or (First = '"')) then
      Result := Copy(S, 2, L - 2)
    else
      Result := S;
  end
  else
    Result := '';
end;

//------------------------------------------------------------------------------

function StrQuote(const S: AnsiString; C: Char): AnsiString;
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

//------------------------------------------------------------------------------

procedure StrUpper(var S: AnsiString); assembler;
asm
        {       StrCase(Str, UpOffset)  }

        MOV     EDX, AnsiUpOffset
        JMP     StrCase
end;

//==============================================================================
// Character
//==============================================================================

function CharIsAlpha(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_ALPHA) <> 0;
end;

//------------------------------------------------------------------------------

function CharType(const C: AnsiChar): Word;
begin
  Result := AnsiCharTypes[C];
end;

//------------------------------------------------------------------------------

function CharIsUpper(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_UPPER) <> 0;
end;

//------------------------------------------------------------------------------

function CharIsLower(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_LOWER) <> 0;
end;

//------------------------------------------------------------------------------

{$IFDEF WIN32}

function CharIsAlphaNum(const C: AnsiChar): Boolean;
begin
  Result := ((AnsiCharTypes[C] and C1_ALPHA) <> 0) or
            ((AnsiCharTypes[C] and C1_DIGIT) <> 0);
end;

{$ENDIF}

//------------------------------------------------------------------------------

function CharIsBlank(const C: AnsiChar): Boolean;
begin
  Result := ((AnsiCharTypes[C] and C1_BLANK) <> 0);
end;

//------------------------------------------------------------------------------

function CharIsPunct(const C: AnsiChar): Boolean;
begin
  Result := ((AnsiCharTypes[C] and C1_PUNCT) <> 0);
end;

//------------------------------------------------------------------------------

{$IFDEF WIN32}

function CharIsPrint(const C: AnsiChar): Boolean;
begin
  Result := not CharIsControl(C);
end;

{$ENDIF}

//------------------------------------------------------------------------------

function CharIsControl(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_CNTRL) <> 0;
end;

//------------------------------------------------------------------------------

function CharIsDigit(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_DIGIT) <> 0;
end;

//------------------------------------------------------------------------------

function CharIsNumber(const C: AnsiChar): Boolean;
begin
  Result := ((AnsiCharTypes[C] and C1_DIGIT) <> 0) or
              (C in AnsiSigns) or (C = DecimalSeparator);
end;

//------------------------------------------------------------------------------

function CharIsReturn(const C: AnsiChar): Boolean;
begin
  Result := (C = AnsiLineFeed) or (C = AnsiCarriageReturn);
end;

//------------------------------------------------------------------------------

function CharIsSpace(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_SPACE) <> 0;
end;

//------------------------------------------------------------------------------

function CharIsWhiteSpace(const C: AnsiChar): Boolean;
begin
  Result := C in AnsiWhiteSpace;
end;

//------------------------------------------------------------------------------

function CharLower(const C: AnsiChar): AnsiChar;
begin
  Result := AnsiCaseMap[Ord(C) + AnsiLoOffset];
end;

//------------------------------------------------------------------------------

function CharPos(const S: AnsiString; const C: AnsiChar; const Index: Integer): Integer; assembler;
asm
        {       make sure that str is not nil }

        TEST    EAX, EAX
        JZ      @StrIsNil

        {       limit index to satisfy 1 <= index, and dec it }

        DEC     ECX
        JL      @IdxIsSmall

        {       ebx will hold the length of str to calculate the result
                at the end, edi will hold the pointer to str }

        PUSH    EBX
        PUSH    EDI

        {       move str pointer into edi }

        MOV     EDI, EAX

        {       clear eax (work reg) and move the Ch into al }

        XOR     EAX, EAX
        MOV     AL, DL

        {       get the length of str into edx and save in ebx,
                we'll need it to calculate the result at the end. }

        MOV     EDX, [EDI-AnsiStrRecSize].TAnsiStrRec.Length
        MOV     EBX, EDX

        {       start from the given Index and limit it to satisfy Index <= Length(str) }

        SUB     EDX, ECX
        JLE     @IdxIsBig
        ADD     EDI, ECX

        {       make ecx our loop counter  }

        MOV     ECX, EDX

        {       clear the direction flag for scasb to make sure we go forword }

        CLD

        {       scan AnsiString until Ch is found  }

        REPNE   SCASB
        JNE     @NoMatch

        {       if we make it here then we found the it. Calculate the
                result and exit. }

        MOV     EAX, EBX
        SUB     EAX, ECX
        POP     EDI
        POP     EBX
        RET

@IdxIsBig:
@NoMatch:

        {       AnsiChar was not found clear the result and exit.}

        XOR     EAX, EAX
        POP     EDI
        POP     EBX
        RET

@IdxIsSmall:

        XOR     EAX, EAX

@StrIsNil:
end;

//------------------------------------------------------------------------------

function CharReplace(var S: AnsiString; const Search, Replace: AnsiChar): Integer; assembler;
asm
        TEST    EAX, EAX
        JZ      @StrIsNil

        {       save the params before UnicodeString call     }

        PUSH    EDX
        PUSH    ECX

        {       create a unique AnsiString if this one is ref-counted }

        CALL    UniqueString

        {       restore the params  }

        POP     ECX
        POP     EDX

        {       make sure that new AnsiString is not nil  }

        TEST    EAX, EAX
        JZ      @StrIsNil

        {       esi will hold pointer to Str, edi # of chars replaced,
                eax current char, ebx search char  }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        {       move string pointer to esi  }

        MOV     ESI, EAX

        {       # of chars replaced = 0   }

        XOR     EDI, EDI

        {       clear eax (working reg)   }

        XOR     EAX, EAX

        {       move Search char into ebx  }

        XOR     EBX, EBX
        MOV     BL, DL

        {       move Replace char into edx  }

        XOR     EDX, EDX
        MOV     DL, CL

        {       get the length of str into ecx        }

        MOV     ECX, [ESI-AnsiStrRecSize].TAnsiStrRec.Length

        {       exit if Length(Str) <= 0              }

        DEC     ECX
        JS      @Done

        {       jump into the loop         }

        JMP     @@Next

@@Replace:

        {       inc # of chars replaced   }

        INC     EDI

        {       replace current char with replace char}

        MOV     [ESI], DL

        {       update loop counter and check the end of AnsiString  }

        DEC     ECX
        JL      @Done

@@NextChar:

        {       point AnsiString to the next char }

        INC     ESI

@@Next:
        {       get current char from Str into al }

        MOV     AL, [ESI]

        {       replace it if it matches out search char  }

        CMP     AL, BL
        JE      @@Replace

        {       update the counter, continue replacing if there are more chars }

        DEC     ECX
        JG      @@NextChar

@Done:
        {       return # of chars replaced }

        MOV     EAX, EDI
        POP     EDI
        POP     ESI
        POP     EBX

@StrIsNil:
end;

//------------------------------------------------------------------------------

function CharToggleCase(const C: AnsiChar): AnsiChar;
begin
  Result := AnsiCaseMap[Ord(C) + AnsiReOffset];
end;

//------------------------------------------------------------------------------

function CharUpper(const C: AnsiChar): AnsiChar;
begin
  Result := AnsiCaseMap[Ord(C) + AnsiUpOffset];
end;

//==============================================================================
// MultiSz
//==============================================================================

function StringsToMultiSz(var Dest: PChar; const Source: TStrings): PChar;
var
  I, TotalLength: Integer;
  P: PChar;
begin
  if Source = nil then   {Az}
  begin
    Result := nil;
    Exit;
  end;

  TotalLength := 0;
  for I := 0 to Source.Count - 1 do
    Inc(TotalLength, Length(Source[I]) + SizeOf(AnsiChar));
  Dest := AllocMem(TotalLength + SizeOf(AnsiChar));
  P := Dest;
  for I := 0 to Source.Count - 1 do
  begin
    P := StrECopy(P, PChar(Source[I]));
    Inc(P);
  end;
  Result := Dest;
end;

//------------------------------------------------------------------------------

procedure MultiSzToStrings(const Dest: TStrings; const Source: PChar);
var
  P: PChar;
begin
  if (Source = nil) or (Dest = nil) then   {Az}
    Exit;

  Dest.Clear;
  P := Source;
  while P^ <> #0 do
  begin
    Dest.Add(P);
    P := StrEnd(P);
    Inc(P);
  end;
end;

//------------------------------------------------------------------------------

procedure FreeMultiSz(var Dest: PChar);
begin
  // FreeMem(Dest, TotalLength + SizeOf(AnsiChar));
  FreeMem(Dest);
  Dest := nil;
end;

{$IFDEF SUPPORTS_DYNAMICARRAYS}

//==============================================================================
// PCharVector
//==============================================================================

function StringsToPCharVector(var Dest: PCharVector;
  const Source: TStrings): PCharVector;
var
  I: Integer;
  S: AnsiString;
  List: array of PChar;
begin
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

//------------------------------------------------------------------------------

function PCharVectorCount(const Source: PCharVector): Integer;
var
  P: PChar;
begin
  Result := 0;

  if Source = nil then   {Az}
     Exit;

  P := Source^;
  while P <> nil do
  begin
    Inc(Result);
    P := PCharVector(Longint(Source) + (SizeOf(PChar) * Result))^;
  end;
end;

//------------------------------------------------------------------------------

procedure PCharVectorToStrings(const Dest: TStrings; const Source: PCharVector);
var
  I, Count: Integer;
  List: array of PChar;
begin
  if (Source = nil) or (Dest = nil) then  {Az}
     Exit;

  Count := PCharVectorCount(Source);
  SetLength(List, Count);
  Move(Source^, List[0], Count * SizeOf(PChar));
  Dest.Clear;
  for I := 0 to Count - 1 do
    Dest.Add(List[I]);
end;

//------------------------------------------------------------------------------

procedure FreePCharVector(var Dest: PCharVector);
var
  I, Count: Integer;
  List: array of PChar;
begin
  if Dest = nil then     {Az}
     Exit;

  Count := PCharVectorCount(Dest);
  SetLength(List, Count);
  Move(Dest^, List[0], Count * SizeOf(PChar));
  for I := 0 to Count - 1 do
    StrDispose(List[I]);
  FreeMem(Dest, (Count + 1) * SizeOf(PChar));
  Dest := nil;
end;

{$ENDIF}

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

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
        '0'..'9':
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

procedure TrimStrings(List: TStrings);
var
  I: Integer;
begin
  if List = nil then
     Exit;

  for I := List.Count - 1 downto 0 do
  begin
     List[I] := Trim(List[I]);
     if List[I] = '' then
        List.Delete(I);
  end;
end;

initialization
  LoadCharTypes;  // this table first
  LoadCaseMap;    // or this function does not work

end.
