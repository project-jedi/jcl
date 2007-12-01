{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{ DUnit Test Unit                                                                                  }
{                                                                                                  }
{ Covers:      JclStrings                                                                          }
{ Last Update: $Date$                                                        }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{**************************************************************************************************}

unit TestJclStrings;

interface
uses
  TestFramework,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  Types,
  {$ENDIF}
  Classes,
  SysUtils,
  JclStrings;

{ TJclStringCharacterTestRoutines }

type
  TJclStringCharacterTestRoutines = class(TTestCase)
  private
  published
    procedure _CharEqualNoCase;
    procedure _CharIsAlpha;
    procedure _CharIsAlphaNum;
    procedure _CharIsBlank;
    procedure _CharIsControl;
    procedure _CharIsDelete;
    procedure _CharIsDigit;
    procedure _CharIsNumberChar;
    procedure _CharIsPrintable;
    procedure _CharIsPunctuation;
    procedure _CharIsReturn;
    procedure _CharIsSpace;
    procedure _CharIsWhiteSpace;
    procedure _CharIsUpper;
    procedure _CharIsLower;
end;


{ TJclStringTransformation }

type
  TJclStringTransormation = class (TTestCase)
  private
    StringArray : array[0..5000] of string;
    StringArray2 : array[0..5000] of string;

  published
    { String Transformation }
    procedure _StrIsAlpha_StrIsAlpaNum_StrIsAlphaNumUnderscore;
    procedure _StrContainsChars;
    procedure _StrSame;
    procedure _StrIsDigit_StrConsistsOfNumberChars_StrIsSubset;
    procedure _StrCenter;
    procedure _StrCharPosLower;
    procedure _StrCharPosUpper;
    procedure _StrDoubleQuote;
    procedure _StrEnsurePrefix;
    procedure _StrEnsureSuffix;
    procedure _StrEscapedToString_StrStringToEscaped;
    procedure _StrLower_StrLowerInPlace_StrLowerBuff;
    procedure _StrMove;
    procedure _StrPadLeft;
    procedure _StrPadRight;
    procedure _StrProper_StrProperBuff;
    procedure _StrQuote;
    procedure _StrReplace;
    procedure _StrReplaceChar;
    procedure _StrReplaceChars;
    procedure _StrReplacebutChars;
    procedure _StrRemoveChars;
    procedure _StrKeepChars;
    procedure _StrRepeat;
    procedure _StrRepeatLength;
    procedure _StrReverse_StrReverseInPlace;
    procedure _StrSingleQuote;
    procedure _StrSmartCase;
    procedure _StrStripNonNumberChars;
    procedure _StrToHex;
    procedure _StrTrimCharLeft;
    procedure _StrTrimCharsLeft;
    procedure _StrTrimCharRight;
    procedure _StrTrimCharsRight;
    procedure _StrTrimQuotes;
    procedure _StrUpper_StrUpperInPlace_StrUpperBuff;
    procedure _StrOemToAnsi_StrAnsiToOem;
  end;

  { TJclStringManagment }

  TJclStringManagment = class (TTestCase)
  published
    procedure StringManagement;
  end;

  { TJclStringSearchandReplace }

  TJclStringSearchandReplace = class (TTestCase)
  private
    StringArray : array[0..5000] of string;
    StringArray2 : array[0..5000] of string;
  published
    procedure _StrCharCount;
    procedure _StrCharsCount;
    procedure _StrStrCount;
    procedure _StrCompare;
    procedure _StrCompareRange;
    procedure _StrFillChar;
    procedure _StrFind;
    procedure _StrHasPrefix;
    procedure _StrIndex;
    procedure _StrILastPos;
    procedure _StrIPos;
    procedure _StrIsOneOf;
    procedure _StrLastPos;
    procedure _StrMatch;
    procedure _StrNPos;
    procedure _StrMatches;
    procedure _StrNIPos;
    procedure _StrPrefixIndex;
    procedure _StrSearch;
  end;

  { TJclStringExtraction }

  TJclStringExtraction = class (TTestCase)
  published
    procedure _StrAfter;
    procedure _StrBefore;
    procedure _StrBetween;
    procedure _StrChopRight;
    procedure _StrLeft;
    procedure _StrMid;
    procedure _StrRight;
    procedure _StrRestOf;
  end;

  { TJclStringTabSet }
  TJclStringTabSet = class(TTestCase)
  published
    procedure _CalculatedTabWidth;
    procedure _Expand;
    procedure _FromString;
    procedure _NilSet;
    procedure _OptimalFill;
    procedure _Optimize;
    procedure _TabFrom;
    procedure _TabStopAdding;
    procedure _TabStopDeleting;
    procedure _TabStopModifying;
    procedure _ToString;
    procedure _UpdatePosition;
    procedure _ZeroBased;
end;

implementation

{$IFDEF LINUX}
uses
  LibC;
{$ENDIF LINUX}
{$IFDEF WIN32}
const
  LibC = 'msvcrt40.dll';

function isalnum(C: Integer): LongBool; cdecl; external LibC;
function isalpha(C: Integer): LongBool; cdecl; external LibC;
{$ENDIF WIN32}

//-----------------------------------------------------------------------------------------------
// Generators
//-----------------------------------------------------------------------------------------------

procedure GenerateAlpha(Len: Cardinal; const Count: Cardinal;
  var Strings: array of string; RandLen: boolean = False);
var
  i: integer;
  t: Integer;
  d: Integer;
  v: Integer;
  s: string;

begin
  RandSeed := 785378134; // Everything has to be reproducible

  if RandLen then
    Len := random(Len) + 1;

  for t := 1 to Count do
  begin
    s := '';

    for i := 1 to Len do
    begin
      d := random(Ord('z')-Ord('a'))+1;
      v := random(2);
      case v of
        0: s := s + chr(ord('a') + d);
        1: s := s + chr(ord('A') + d);
      end;
    end;

    Strings[t-1] := s;
  end;
end;

//------------------------------------------------------------------------------

procedure GenerateAlphaLowerCase(Len: Cardinal; const Count: Cardinal;
  var Strings: array of string; RandLen: Boolean = False);
var
  i: integer;
  t: Integer;
  d: Integer;
  s: string;

begin
  RandSeed := 728134; // Everything has to be reproducible
  if RandLen then
    Len := random(Len) + 1;

  for t := 1 to Count do
  begin
    s := '';

    for i := 1 to Len do
    begin
      d := random(Ord('z')-Ord('a'))+1;
      s := s + chr(ord('a') + d);
    end;

    Strings[t-1] := s;
  end;
end;

//------------------------------------------------------------------------------

procedure GenerateAlphaUpperCase(Len: Cardinal; const Count: Cardinal;
  var Strings: array of string; RandLen: Boolean = False);
var
  i: integer;
  t: Integer;
  d: Integer;
  s: string;

begin
  RandSeed := 728134; // Everything has to be reproducible

  if RandLen then
    Len := random(Len) + 1;

  for t := 1 to Count do
  begin
    s := '';

    for i := 1 to Len do
    begin
      d := random(Ord('z')-Ord('a'))+1;
      s := s + chr(ord('A') + d);
    end;

    Strings[t-1] := s;
  end;
end;

//------------------------------------------------------------------------------

procedure GenerateAlphaNum(Len: Cardinal; const Count: Cardinal;
  var Strings: array of string; RandLen: Boolean = False);
var
  i: integer;
  t: Integer;
  d: Integer;
  v: Integer;
  s: string;

begin
  RandSeed := 785378134; // Everything has to be reproducible

  if RandLen then
    Len := random(Len) + 1;

  for t := 1 to Count do
  begin
    s := '';

    for i := 1 to Len do
    begin
      d := random(Ord('z')-Ord('a'))+1;
      case random(2) of
        0: begin
             v := random(2);
             case v of
               0: s := s + chr(ord('a') + d);
               1: s := s + chr(ord('A') + d);
             end;
           end;
        1: begin
             d := random(Ord('9')-Ord('0'));
             s := s + chr(ord('0') + d);
           end;
      end;
    end;

    Strings[t-1] := s;
  end;
end;

//------------------------------------------------------------------------------

procedure GenerateAll(Len: Cardinal; const Count: Cardinal;
  var Strings: array of string; RandLen: Boolean = False);
var
  i: integer;
  t: Integer;
  d: Integer;
  v: Integer;
  s: string;

begin
  RandSeed := 781134; // Everything has to be reproducible
  v := Len;

  for t := 1 to Count do
  begin
    s := '';

    if RandLen then
      Len := random(v) + 1;

    for i := 1 to Len do
    begin
      d := random(255);
      s := s + chr(1+d);
    end;

    Strings[t-1] := s;
  end;
end;

function StrLower2(const S: AnsiString): AnsiString;
begin
  Result := S;
  StrLowerInPlace(Result);
end;

//==================================================================================================
// TJclStringTransormation
//==================================================================================================

procedure TJclStringTransormation._StrIsAlpha_StrIsAlpaNum_StrIsAlphaNumUnderscore;
var
  i: Integer;
  s: String;

begin
  CheckEquals(False, StrIsAlpha(''), 'StrIsAlpha');  // per doc
  CheckEquals(False, StrIsAlphaNumUnderscore(''), 'StrIsAlphaNumUnderscore9');  // per doc
  CheckEquals(False, StrIsAlphaNum(''), 'StrIsAlphaNum');  // per doc

  GenerateAlpha(2000, 1000, stringarray);

  for i := 1 to 500 do
  begin
    s := stringarray[i-1];
    CheckEquals(True, StrIsAlpha(s), 'StrIsAlpha');
    CheckEquals(True, StrIsAlphaNum(s), 'StrIsAlphaNum');
    CheckEquals(True, StrIsAlphaNumUnderscore(s), 'StrIsAlphaNumUnderscore');
  end;

  GenerateAlphaNum(2000, 1000, stringarray, True);

  for i := 1 to 500 do
  begin
    s := stringarray[i-1];
    CheckEquals(True, StrIsAlphaNum(s), 'StrIsAlphaNum');
    CheckEquals(True, StrIsAlphaNumUnderscore(s), 'StrIsAlphaNumUnderscore');

    s := s + '_';
    CheckEquals(False,StrIsAlphaNum(s),'StrIsAlphaNum');
    CheckEquals(True, StrIsAlphaNumUnderscore(s),'StrIsAlphaNumUnderscore');
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrContainsChars;
begin
  Fail('TODO: StrContainsChars');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrSame;
var
  i: Integer;

begin
  // StrSame
  CheckEquals(StrSame('',''), True, 'StrSame');  // per doc
  CheckEquals(True,StrSame('aaa','AAA'), 'StrSame'); // Case insensitive

  GenerateAll(1000, 500, stringarray, True);
  GenerateAll(50, 500, stringarray2, True);

  for i := 1 to 500 do
  begin
    CheckEquals(True, StrSame(stringarray[i-1], stringarray[i-1]), 'StrSame');
    CheckEquals(False, StrSame(stringarray[i-1], stringarray2[i-1]), 'StrSame');
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrIsDigit_StrConsistsOfNumberChars_StrIsSubset;
begin
  // StrIsDigit
  CheckEquals(StrIsDigit('') , False,'StrIsDigit');  // per doc

  // StrConsistsOfNumberChars
  CheckEquals(StrConsistsOfNumberChars('') , False,'StrConsistsOfNumberChars');  // per doc

  // StrIsSubset
  CheckEquals(StrIsSubset('',[' ']), False,'StrIsSubset');  // per doc
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrCenter;
var
  i: Integer;
  s, SN: String;

begin
  // StrCenter should return s unchanged. Since the length parameter is
  // smaller than (even negative) the acutal length of S.

  S := '1234567890';

  for i := -100 to 9 do
  begin
    SN := StrCenter(S, i, '#');
    CheckEquals(SN, S, 'StrCenter');
  end;

  // StrCenter should add the fill pattern. The length is checked.

  for i := 10 to 400 do
  begin
    SN := StrCenter(S, i, '#');
    CheckEquals(i, Length(SN), 'StrCenter');
  end;

  // StrCenter work tests.

  SN := StrCenter('', 10, '#');
  CheckEquals(Length(SN), 10, 'StrCenter');
  CheckEquals(SN, '##########', 'StrCenter');

  SN := StrCenter('t', 6, '#');
  CheckEquals(SN, '##t###', 'StrCenter');

  SN := StrCenter('t', 7, '!');
  CheckEquals(SN, '!!!t!!!', 'StrCenter');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrCharPosLower;
begin
  CheckEquals('This is a test.', StrCharPosLower('This is a test.', -1));
  CheckEquals('This is a test.', StrCharPosLower('This is a test.', 0));
  CheckEquals('this is a test.', StrCharPosLower('This is a test.', 1));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrCharPosUpper;
begin
  CheckEquals('This is a test.', StrCharPosUpper('This is a test.', -1));
  CheckEquals('This is a test.', StrCharPosUpper('This is a test.', 0));
  CheckEquals('This is a test.', StrCharPosUpper('This is a test.', 1));
  CheckEquals('THis is a test.', StrCharPosUpper('This is a test.', 2));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrDoubleQuote;
var
  SN, S: string;
  i: Integer;
  
begin
  SN := StrDoubleQuote('');
  CheckEquals('""', SN, 'StrDoubleQuote');

  SN := StrDoubleQuote('Project JEDI');
  CheckEquals('"Project JEDI"',SN, 'StrDoubleQuote');

  // Test if String is has been quoted. Since StrDoubleQuote adds quotes also
  // when they are already there no special tests are needed.

  GenerateAll(2000,200, StringArray, True);

  for i := 1 to 200 do
  begin
    S := StringArray[i-1];
    CheckEquals('"'+S+'"',StrDoubleQuote(s) ,'StrDoubleQuote');
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrEnsurePrefix;
var
  Prefix, s, SN: String;
  I: Integer;

begin
  s := 'TestIt!';
  CheckEquals('TestIt!', StrEnsurePrefix('',S), 'StrEnsurePrefix');
  CheckEquals(StrEnsurePrefix(S,''), 'TestIt!', 'StrEnsurePrefix');
  CheckEquals(StrEnsurePrefix('TestIt!',S), 'TestIt!', 'StrEnsurePrefix');

  s := 'TestIT!';
  CheckEquals(StrEnsurePrefix('TestIt!',S), 'TestIt!TestIT!','StrEnsurePrefix');

  // Test StrEnsurePrefix using the Generators. S is the string, Prefix the
  // wanted prefix and SN is prefix + s or s if the prefix is already there.

  GenerateAll(2000, 100, StringArray, True);
  GenerateAll(20, 100, StringArray2, True);

  for i := 1 to 100 do
  begin
    S := StringArray[i-1];
    Prefix := StringArray2[i-1];

    SN := StrEnsurePrefix(Prefix,S);

    if copy(s, 0, length(Prefix)) <> prefix then
      Check(SN = prefix+s, Format('StrEnsurePrefix source: %s  prefix: %s  result: %s ', [s, prefix, sn]))
    else
     Check(SN = s, Format('StrEnsurePrefix source: %s  prefix: %s  result: %s ', [s, prefix, sn]));
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrEnsureSuffix;
var
  Suffix, s, SN: String;
  I: Integer;
begin
  s := 'TestIt!';
  CheckEquals(StrEnsureSuffix('',S), 'TestIt!', 'StrEnsureSuffix');
  CheckEquals(StrEnsureSuffix(S,''), 'TestIt!', 'StrEnsureSuffix');
  CheckEquals(StrEnsureSuffix('TestIt!',S), 'TestIt!', 'StrEnsureSuffix');

  s := 'TestIT!';
  CheckEquals(StrEnsureSuffix('TestIt!',S) , 'TestIT!TestIt!', 'StrEnsureSuffix');

  // Test StrEnsureSuffix using the Generators. S is the string, Suffix the
  // wanted suffix and SN is s + suffix or s if the suffix is already there.

  GenerateAll(2000, 200, StringArray, True);
  GenerateAll(20, 200, StringArray2, True);

  for i := 1 to 200 do
  begin
    S := StringArray[i-1];
    Suffix := StringArray2[i-1];

    SN := StrEnsureSuffix(suffix,s);

    if copy(s, length(s) - length(suffix), 300) <> suffix then
      Check(SN = s + suffix, Format('StrEnsureSuffix source: %s  prefix: %s  result: %s ', [s, suffix, sn]))
    else
     Check(SN = s, Format('StrEnsureSuffix source: %s  prefix: %s  result: %s ', [s, suffix, sn]));
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrEscapedToString_StrStringToEscaped;
var
  s, sn: string;
  i: Integer;
  

begin
  S := StrEscapedToString('');
  sn := '';
  CheckEquals(StrEscapedToString(SN), S, 'StrEscapedToString');

  GenerateAll(1000, 200, StringArray, true);

  for i := 1 to 200 do
  begin
    S := StringArray[i-1];
    sn := StrStringToEscaped(s);

    CheckEquals(StrEscapedToString(SN), s, 'StrEscapedToString');
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure  TJclStringTransormation._StrLower_StrLowerInPlace_StrLowerBuff;
var
 sp: pointer;
 i: Integer;
 s, sn: string;

begin
  CheckEquals(StrLower(''), '', 'StrLower');

  { LowerBuff nil tests }
  sp := nil;
  StrLowerBuff(nil);
  StrLowerBuff(sp);
  CheckEquals(Integer(sp), Integer(nil), 'StrLowerBuff');

  { Tests StrLower, StrLowerBuff and StrLowerInPlace against AnsiLowerCase and
    against each other. The Testdata consits of only uppercase chars in this test. }

  GenerateAlphaUpperCase(500,500,StringArray, True);

  for i := 1 to 500 do
  begin
    s := StringArray[i-1];
    SN := s;
    StrLowerInPlace(SN);
    CheckEquals(StrLower(s), AnsiLowerCase(s), 'StrLower');
    CheckEquals(StrLower(s), SN, 'StrLower');

    StrLowerBuff(PChar(s));
    CheckEquals(s, SN,'StrLowerBuff');
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure  TJclStringTransormation._StrMove;
var
  Dest: string;

begin
  Dest := 'ATest';

  StrMove(Dest, 'xxxx', 1, 1, 5);
  CheckEquals('ATest',Dest, 'StrMove');

  StrMove(Dest, 'xxxx', 1, 4, 3);
  CheckEquals('ATest',Dest, 'StrMove');

  StrMove(Dest, 'xxxx', -1, 1, 3);
  CheckEquals('ATest',Dest, 'StrMove');

  StrMove(Dest, 'xxxx', 1, -1, 3);
  CheckEquals('ATest',Dest, 'StrMove');

  StrMove(Dest, 'xxxx', 1, 1, -3);
  CheckEquals('ATest',Dest, 'StrMove');

  StrMove(Dest, 'xxxx', 1, 1, 3);
  CheckEquals('xxxst',Dest, 'StrMove');

  Dest := 'ATest';
  StrMove(Dest, 'abcd', 3, 2, 2);
  CheckEquals('ATbct',Dest, 'StrMove');

  Dest := 'ATest';
  StrMove(Dest, 'abcd', 5, 4, 1);
  CheckEquals('ATesd',Dest, 'StrMove');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrPadLeft;
var
  S, S3: String;
  I, v,t: Integer;

begin
  // -- StrPadLeft --

  S := '';
  S := StrPadLeft(S, 10, '#');
  CheckEquals(S, '##########','StrPadLeft');

  s := StrPadLeft(S, -10, '$');
  CheckEquals(S , '##########','StrPadLeft');

  { StrPadLeft is tested using the Generator. A random number of dollar signs are
    added to the string s. The first comparisation test against the length, the
    second performs an actual test.}

  GenerateAll(2000,100, StringArray, True);
  RandSeed := 123456;

  for i := 1 to 100 do
  begin
    s := StringArray[i-1];
    v := random(20)+2;

    s3 := StrPadLeft(s, length(s) + v, '$');
    CheckEquals(Length(s3), length(s) + v,'StrPadLeft');

    for t := 1 to v do
      s := '$' + s;

    CheckEquals(s3, s,'StrPadLeft');
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrPadRight;
var
  S, S3: String;
  I, v,t: Integer;

begin
  // -- StrPadRight --
  S := '';
  s := StrPadRight(S, 10, '#');
  CheckEquals(S , '##########','StrPadRight');

  s := StrPadRight(S, -10, '$');
  CheckEquals(S , '##########','StrPadRight');

  { StrPadRight is tested using the Generator. A random number of percent char are
    added to the string s. The first comparisation test against the length, the
    second performs an actual test.}

  GenerateAll(2000,100,StringArray, True);

  for i := 1 to 100 do
  begin
    s := StringArray[i-1];
    v := random(20)+2;
    s3 := StrPadRight(s, length(s) + v, '%');
    CheckEquals(Length(s3), length(s) + v,'StrPadRight');

    for t := 1 to v do
      s := s + '%';

    CheckEquals(s3, s,'StrPadRight');
   end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrProper_StrProperBuff;
var
 s, s3, sn: string;
 i: Integer;

begin
  CheckEquals(StrProper('') , '','StrProper');
  CheckEquals(StrProper('Test') , 'Test','StrProper');
  CheckEquals(StrProper('TeSt') , 'Test','StrProper');
  CheckEquals(StrProper('TEST') , 'Test','StrProper');
  CheckEquals(StrProper('TeST1234') , 'Test1234','StrProper');

  s := 'TeST';
  s3 := s;
  s3 := StrProper(s);
  CheckNotEquals(s3,s,'StrProper');

  StrProperBuff(nil);

  GenerateAll(400,200,StringArray, True);

  for i := 1 to 200 do
  begin
    s := StringArray[i-1];
    sn := AnsiLowerCase(S);
    sn[1] := AnsiUpperCase(s[1])[1];
    s3 := StrProper(s);
    CheckEquals(s3, sn,'StrProper');
    StrProperBuff(PChar(s));
    CheckEquals(s, sn,'StrProperBuff');
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrQuote;
var
  i: Integer;
  s: string;
  
begin
  CheckEquals(StrQuote('','#'), '','StrQuote');
  CheckEquals(StrQuote('a','#'), '#a#','StrQuote');
  CheckEquals(StrQuote('Test','#'), '#Test#','StrQuote');
  CheckEquals(StrQuote('#Test#','#'), '#Test#','StrQuote');
  CheckEquals(StrQuote('"Test"','#'), '#"Test"#','StrQuote');
  CheckEquals(StrQuote('"Test#','"'), '"Test#"','StrQuote');

  { StrQuote is tested using the Generator. Since it is possible that the char
    is already on the left or right side we have to check all four cases.}

  GenerateAll(2000,200,StringArray, True);

  for i := 1 to 200 do
  begin
    s := StringArray[i-1];
    if (s[1] <> '"') and (s[Length(s)] <> '"') then
      CheckEquals(StrQuote(s,'"'), '"'+s+'"','StrQuote')
    else
    if (s[1] = '"') and (s[Length(s)] = '"') then
      CheckEquals(StrQuote(s,'"'), s,'StrQuote')
    else
    if (s[1] <> '"') and (s[Length(s)] = '"') then
      CheckEquals(StrQuote(s,'"'), '"'+s,'StrQuote')
    else
    if (s[1] = '"') and (s[Length(s)] <> '"') then
      CheckEquals(StrQuote(s,'"'), s+'"','StrQuote');
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrRemoveChars;
var
  i, t, v: Integer;
  s, s3, sn: string;
  sset: TSysCharSet;
  
begin
  // -- StrRemoveChars --
  CheckEquals(StrRemoveChars('',['e']), '', 'StrRemoveChars');
  CheckEquals(StrRemoveChars('Test',['e']), 'Tst', 'StrRemoveChars');

  GenerateAll(20,200,StringArray2, True);
  GenerateAll(400,200,StringArray, True);

  { Check StrRemoveChars against a self made one using the Pos function }

  for i := 1 to 200 do
  begin
    s := StringArray[i-1];
    s3 := StringArray[i-1];
    sn := StringArray2[i-1];
    sset := [];

    for t := 1 to length(sn) do
    begin
      if not(sn[t] in sset) then
        sset := sset + [char(sn[t])];

      v := pos(sn[t],S3);

      while v > 0 do
      begin
        Delete(S3, v, 1);
        v := pos(sn[t],S3);
      end;
    end;

    CheckEquals(StrRemoveChars(s, sset), s3,'StrRemoveChars');
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrKeepChars;
var
  i, t: Integer;
  s, s3, sn: String;
  sset: TSysCharSet;

begin
  CheckEquals(StrKeepChars('',[]),'','StrKeepChars');
  CheckEquals(StrKeepChars('Joint Endeavour of Delphi Innovators',['e', 'a', 'o', 'u', 'i']),'oieaouoeioao','StrKeepChars');
  CheckEquals(StrKeepChars('Joint Endeavour of Delphi Innovators',[' ', 'e', 'a', 'o', 'u', 'i']),'oi eaou o ei oao','StrKeepChars');

  GenerateAll(20,200,StringArray2, True);
  GenerateAll(400,200,StringArray, True);

  { Check StrKeepChars against a self made one }

  for i := 1 to 200 do
  begin
    s := StringArray[i-1];
    s3 := '';
    sn := StringArray2[i-1];
    sset := [];

    for t := 1 to length(sn) do
    begin
      if not(sn[t] in sset) then
        sset := sset + [char(sn[t])];
    end;

    for t := 1 to length(s) do
    begin
      if s[t] in sset then
        s3 := s3 + s[t];
    end;

    CheckEquals(StrKeepChars(s, sset), s3,'StrKeepChars');
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrReplace;
var
  s: string;
  
begin
  s := '';
  StrReplace(s, '', 'Test', []);
  CheckEquals(s, '', 'StrReplace');

  s := 'This is a test.';
  StrReplace(s, 'is a', 'is a successful', []);
  CheckEquals(s, 'This is a successful test.', 'StrReplace');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrReplaceChar;
begin
  CheckEquals(StrReplaceChar('', 'a', 'b'),'','StrReplaceChar');
  CheckEquals(StrReplaceChar('', #0, #0),'','StrReplaceChar');
  CheckEquals(StrReplaceChar('ababab', 'a', 'b'),'bbbbbb','StrReplaceChar');
  CheckEquals(StrReplaceChar('ababab', 'b', 'a'),'aaaaaa','StrReplaceChar');
  CheckEquals(StrReplaceChar('xabababx', 'b', 'a'),'xaaaaaax','StrReplaceChar');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrReplaceChars;
begin
  CheckEquals(StrReplaceChars('', ['a'], 'b'),'','StrReplaceChars');
  CheckEquals(StrReplaceChars('', ['a'], 'b'),'','StrReplaceChars');
  CheckEquals(StrReplaceChars('ababab', ['a','b'], 'b'),'bbbbbb','StrReplaceChars');
  CheckEquals(StrReplaceChars('xabababx', ['a','b'], 'b'),'xbbbbbbx','StrReplaceChars');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrReplacebutChars;
begin
  CheckEquals(StrReplaceButChars('', ['a'], 'b'),'','StrReplaceButChars');
  CheckEquals(StrReplaceButChars('xabababx', ['a','b'], 'v'),'vabababv','StrReplaceChars');
  CheckEquals(StrReplaceButChars('TxabababxT', ['a','b'], 'v'),'vvabababvv','StrReplaceChars');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrRepeat;
var
  i,t, v: Integer;
  s, s3: string;

begin
  CheckEquals(StrRepeat('Test',0) , '', 'StrRepeat');
  CheckEquals(StrRepeat('Test',-1) , '', 'StrRepeat');
  CheckEquals(StrRepeat('Test',-1000) , '', 'StrRepeat');
  CheckEquals(StrRepeat('He',3) , 'HeHeHe', 'StrRepeat');
  CheckEquals(StrRepeat('H e',3) , 'H eH eH e', 'StrRepeat');

  GenerateAll(50,200,StringArray, True);

  { Check StrRepeat against a self made one }

  RandSeed := 432321;

  for i := 1 to 200 do
  begin
    s := StringArray[i-1];
    s3 := '';
    v := random(20)+1;

    for t := 1 to v do
      s3 := s3 + s;

    CheckEquals(StrRepeat(s,v) ,s3, 'StrRepeat');
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrRepeatLength;
begin
  CheckEquals(StrRepeatLength('Test',0),'','StrRepeatLength');
  CheckEquals(StrRepeatLength('Test',1),'T','StrRepeatLength');
  CheckEquals(StrRepeatLength('Test',2),'Te','StrRepeatLength');
  CheckEquals(StrRepeatLength('Test',3),'Tes','StrRepeatLength');
  CheckEquals(StrRepeatLength('Test',4),'Test','StrRepeatLength');
  CheckEquals(StrRepeatLength('TestTest',8),'TestTest','StrRepeatLength');
  CheckEquals(StrRepeatLength('Test',-1),'','StrRepeatLength');
  CheckEquals(StrRepeatLength('Test',-100),'','StrRepeatLength');
  CheckEquals(StrRepeatLength('',-100),'','StrRepeatLength');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrReverse_StrReverseInPlace;
var
  i,t: Integer;
   s, s3: string;

begin
  // -- StrReverse / StrReverseInPlace --
  CheckEquals(StrReverse(''), '', 'StrReverse');
  CheckEquals(StrReverse('a'), 'a', 'StrReverse');
  CheckEquals(StrReverse('ab'), 'ba', 'StrReverse');
  CheckEquals(StrReverse('abc'), 'cba', 'StrReverse');

  { Check StrReverse against a (slow) self made one }

  GenerateAll(100,200,StringArray, True);

  for i := 1 to 200 do
  begin
    s := StringArray[i-1];
    SetLength(s3, length(s));

    for t := 1 to length(s) do
      s3[t] := s[(length(s) - t) + 1];

    s := StrReverse(s);
    CheckEquals(s, s3, 'StrReverse');

    s := StringArray[i-1];

    StrReverseInPlace(s);
    CheckEquals(s, s3, 'StrReverse');
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrSingleQuote;
var
  i: Integer;
  s: string;

begin
  CheckEquals(StrSingleQuote(''), '''''', 'StrSingleQuote');
  CheckEquals(StrSingleQuote('Project JEDI'), '''Project JEDI''', 'StrSingleQuote');

  GenerateAll(2000,200,StringArray, True);

  for i := 1 to 200 do
  begin
    S := StringArray[i-1];
    CheckEquals(StrSingleQuote(s),''''+S+'''', 'StrSingleQuote');
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrSmartCase;
begin
  CheckEquals(StrSmartCase('',[' ']), '', 'StrSmartCase');
  CheckEquals(StrSmartCase('project jedi',[' ']),'Project Jedi', 'StrSmartCase');
  CheckEquals(StrSmartCase('project jedi ',[' ']),'Project Jedi ', 'StrSmartCase');
  CheckEquals(StrSmartCase(' project jedi ',[' ']),' Project Jedi ', 'StrSmartCase3');
  CheckEquals(StrSmartCase('  project jedi ',[' ']),'  Project Jedi ', 'StrSmartCase3');
  CheckEquals(StrSmartCase('xxxxxAx',[' ','x']),'XXXXXAx', 'StrSmartCase3');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrStripNonNumberChars;
var
  i: Integer;
  s: string;

begin
  CheckEquals(StrStripNonNumberChars(''),'','StrStripNonNumberChars');
  CheckEquals(StrStripNonNumberChars('abc1234+1234abc'),'1234+1234','StrStripNonNumberChars');
  CheckEquals(StrStripNonNumberChars('123+abcabc+123'),'123++123','StrStripNonNumberChars');
  CheckEquals(StrStripNonNumberChars('abc1234+1234abc'),'1234+1234','StrStripNonNumberChars');

  GenerateAlpha(200,50,StringArray, True);

  for i := 1 to 50 do
  begin
    S := StringArray[i-1];
    CheckEquals(StrStripNonNumberChars(s),'', 'StrStripNonNumberChars');
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrToHex;
var
  s, sn: string;

begin
  CheckEquals(StrToHex(''),'','StrToHex');

  SN := '262A32543B';
  SetLength(S,20);
  HexToBin(PChar(SN),PChar(S),20);
  CheckEquals(StrToHex(SN),Copy(S,1,Length(SN) div 2),'StrToHex');

  SN := 'FF2A2B2C2D1A2F';
  HexToBin(PChar(SN),PChar(S),20);
  CheckEquals(StrToHex(SN),Copy(S,1,Length(SN) div 2),'StrToHex');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrTrimCharLeft;
var
  i,t: Integer;
  s, s3, sn: string;
begin
  CheckEquals(StrTrimCharLeft('',#0),'','StrTrimCharLeft');
  CheckEquals(StrTrimCharLeft('AAAAAAAAAA','A'),'','StrTrimCharLeft');

  GenerateAll(200, 2000, StringArray);
  GenerateAll(1,   2000, StringArray2);

  for i := 1 to 2000 do
  begin
    S  := StringArray[i-1];
    SN := StringArray2[i-1];

    while S[1] = SN do
      s := '#' + s;

    S3 := S;
    t := random(100);

    while t <> 0 do
    begin
      S3 := SN + S3;
      dec(t);
    end;

    CheckEquals(StrTrimCharLeft(S3,SN[1]), S,'StrTrimCharLeft');
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrTrimCharsLeft;
begin
  Fail('TODO: StrTrimCharsLeft');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrTrimCharRight;
var
  i,t: Integer;
  s, sn, s3: string;

begin
  // -- StrTrimCharRight --
  CheckEquals(StrTrimCharRight('',#0),'','StrTrimCharRight');
  CheckEquals(StrTrimCharRight('AAAAAAAAAA','A'),'','StrTrimCharRight');

  GenerateAll(200, 2000, StringArray);
  GenerateAll(1,   2000, StringArray2);

  for i := 1 to 2000 do
  begin
    S  := StringArray[i-1];
    SN := StringArray2[i-1];

    while S[Length(S)] = SN do
      s := s + '#';

    S3 := S;
    t := random(100);

    while t <> 0 do
    begin
      S3 := S3 + SN;
      dec(t);
    end;

    CheckEquals(StrTrimCharRight(S3,SN[1]), S,'StrTrimCharRight');
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrTrimCharsRight;
begin
  Fail('TODO: _StrTrimCharsLeft');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrTrimQuotes;
var
  i: Integer;
  s, s3, s4: string;
begin
  CheckEquals(StrTrimQuotes(''),'','StrTrimQuotes');
  CheckEquals(StrTrimQuotes('""'),'','StrTrimQuotes');
  CheckEquals(StrTrimQuotes(''''''),'','StrTrimQuotes');

  CheckEquals(StrTrimQuotes('""TEST""'),'"TEST"','StrTrimQuotes');
  CheckEquals(StrTrimQuotes('''''TEST'''''),'''TEST''','StrTrimQuotes');

  GenerateAll(200,100,StringArray);

  for i := 1 to 100 do
  begin
     s := StringArray[i-1];
     s3 := StrDoubleQuote(s);
     s4 := StrSingleQuote(s);

    CheckEquals(StrTrimQuotes(s3),s,'StrTrimQuotes');
    CheckEquals(StrTrimQuotes(s4),s,'StrTrimQuotes');
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrUpper_StrUpperInPlace_StrUpperBuff;
var
  i: Integer;
  s4, s, s3: string;

begin
  GenerateAll(200,200,StringArray);

  for i := 1 to 200 do
  begin
    S  := StringArray[i-1];
    S3 := AnsiUpperCase(s);
    S4 := S;
    StrUpperInPlace(S4);
    CheckEquals(StrUpper(S), S3, 'StrUpper');
    CheckEquals(S4, S3, 'StrUpperInPlace');

    S4 := S;
    StrUpperBuff(PChar(S4));
    CheckEquals(S4, S3, 'StrUpperBuff');
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTransormation._StrOemToAnsi_StrAnsiToOem;
begin
  Fail('TODO: _StrOemToAnsi_StrAnsiToOem');
end;

//==================================================================================================
// String Managment
//==================================================================================================

procedure TJclStringManagment.StringManagement;
var
 s1: string;

begin
  StrAddRef(s1);
  StrAddRef(s1);
  StrAddRef(s1);
  CheckEquals(StrRefCount(s1),  0,'StrRefCount');

  s1 := 'test';
  StrAddRef(s1);
  StrAddRef(s1);
  CheckEquals(StrRefCount(s1), 2,'StrRefCount');

  StrAddRef(s1);
  StrAddRef(s1);
  CheckEquals(StrRefCount(s1), 4,'StrRefCount');
  StrDecRef(s1);
  CheckEquals(StrRefCount(s1), 3,'StrRefCount');
  StrDecRef(s1);
  CheckEquals(StrRefCount(s1), 2,'StrRefCount');
  StrDecRef(s1);
  CheckEquals(StrRefCount(s1), 1,'StrRefCount');
  StrDecRef(s1);
  CheckEquals(StrRefCount(s1), 0,'StrRefCount');
end;

//==================================================================================================
// String Search and Replace
//==================================================================================================

procedure TJclStringSearchandReplace._StrCharCount;
var
  s: string;
  ca, t, i: Integer;
  c: char;
  
begin
  CheckEquals(StrCharCount('','x'),0,'StrCharCount');
  CheckEquals(StrCharCount('Test',#0),0,'StrCharCount');
  CheckEquals(StrCharCount('Test','T'),1,'StrCharCount');
  CheckEquals(StrCharCount('Test','t'),1,'StrCharCount');
  CheckEquals(StrCharCount('TestTT','T'),3,'StrCharCount');
  CheckEquals(StrCharCount('Ttetstt','t'),4,'StrCharCount');

  GenerateAll(500,100,StringArray, True);

  for i := 1 to 100 do
  begin
    s := StringArray[i-1];

    for c := #1 to #255 do
    begin
      ca := 0;

      for t := 1 to length(s) do
      begin
        if s[t] = c then
         inc(ca);
      end;

      CheckEquals(StrCharCount(s,c),ca,'StrCharCount');
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringSearchandReplace._StrCharsCount;
begin
  CheckEquals(StrCharsCount('',['x']),0,'StrCharsCount');
  CheckEquals(StrCharsCount('Test',[#0]),0,'StrCharsCount');
  CheckEquals(StrCharsCount('Test',['T']),1,'StrCharsCount');
  CheckEquals(StrCharsCount('Test',['t']),1,'StrCharsCount');
  CheckEquals(StrCharsCount('TestTT',['T']),3,'StrCharsCount');
  CheckEquals(StrCharsCount('Ttetstt',['t']),4,'StrCharsCount');
  CheckEquals(StrCharsCount('Ttetstt',['t','T']),5,'StrCharsCount');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringSearchandReplace._StrStrCount;
begin
  CheckEquals(1, StrStrCount('Test', 'Test'), 'StrStrCount_1');
  CheckEquals(2, StrStrCount('TestTest', 'Test'), 'StrStrCount_2');
  CheckEquals(0, StrStrCount('Test', 'Quark'), 'StrStrCount_3');
  CheckEquals(0, StrStrCount('', 'Quark'), 'StrStrCount_4');
  CheckEquals(0, StrStrCount('', ''), 'StrStrCount_5');
  CheckEquals(0, StrStrCount('Test', ''), 'StrStrCount_6');
  CheckEquals(0, StrStrCount('Test', 'TEST'), 'StrStrCount_7');  // Case sensive ?
  CheckEquals(0, StrStrCount('', 'Test'), 'StrStrCount_8');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringSearchandReplace._StrCompare;
var
  i, t: Integer;
  S, S1, S2: String;
  
begin
  CheckEquals(StrCompare('',''),0,'StrCompare');
  CheckEquals(StrCompare('jedi','jedi'),0,'StrCompare');
  CheckEquals(StrCompare('jedi','je'),2,'StrCompare');
  CheckEquals(StrCompare('di','jedi'),-2,'StrCompare');
  CheckEquals(StrCompare('project jedi','jedi'),8,'StrCompare');
  CheckEquals(StrCompare('jedi','judi'),Ord('e') - Ord('u'),'StrCompare');
  CheckEquals(StrCompare('JEDI','Judi'),Ord('e') - Ord('u'),'StrCompare');

  GenerateAll(600,200,StringArray);

  for i := 1 to 200 do
  begin
    S := StringArray[i-1];
    S1 := S;
    CheckEquals(StrCompare(S,S1),0,'StrCompare');
    CheckEquals(StrCompare(S,S),0,'StrCompare');
  end;

  GenerateAll(600,1000,StringArray, True);

  for i := 1 to 200 do
  begin
    S  := StringArray[i-1];
    S1 := StringArray[199+i];

    if Length(S) = Length(S1) then
      S1 := S1 + 'x';

    CheckEquals(StrCompare(S,S1),Length(S) - Length(S1),'StrCompare');
    CheckEquals(StrCompare(S1,S),Length(S1) - Length(S),'StrCompare');
  end;

  GenerateAll(600,2000,StringArray);
  GenerateAll(1,1000,StringArray2);

  for i := 1 to 200 do
  begin
    S  := StringArray[i-1];
    S2 := S;

    S1 := StringArray[i];
    t := random(Length(S));

    while s1 = S[1 + t] do
      t := random(Length(S));

    S[1+t] := Char(s1[1]);
    CheckEquals(StrCompare(S2,S), ord(CharLower(S2[1+t])) - ord(CharLower(S[1+t])) ,'StrCompare');
    CheckEquals(StrCompare(S,S2), ord(CharLower(S[1+t])) - ord(CharLower(S2[1+t])) ,'StrCompare');
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringSearchandReplace._StrCompareRange;
begin
  CheckEquals(StrCompareRange('','',1,0),0,'StrCompareRange1');
  CheckEquals(StrCompareRange('Test1234','Test',1,4),0,'StrCompareRange5');
  CheckEquals(StrCompareRange('Test1234','Test1234',1,25),0,'StrCompareRange6');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringSearchandReplace._StrFillChar;
begin
  {$ASSERTIONS OFF}
  CheckEquals(StrFillChar('a', 0),'','StrFillChar');
  CheckEquals(StrFillChar('a', 1),'a','StrFillChar');
  CheckEquals(StrFillChar('a', 2),'aa','StrFillChar');
  CheckEquals(StrFillChar('b', 4),'bbbb','StrFillChar');
  {$ASSERTIONS ON}
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringSearchandReplace._StrFind;
begin
  CheckEquals(0, StrFind('abc', 'Test'));
  CheckEquals(1, StrFind('Test', 'Test'));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringSearchandReplace._StrHasPrefix;
begin
  CheckEquals(StrHasPrefix('',[]), False ,'StrHasPrefix');
  CheckEquals(StrHasPrefix('',['TEST']), False ,'StrHasPrefix');
  CheckEquals(StrHasPrefix('',['TEST','TEST2']), False ,'StrHasPrefix');
  CheckEquals(StrHasPrefix('Test',['TEST','TEST2']), True ,'StrHasPrefix');
  CheckEquals(StrHasPrefix('Test2',['TEST','TEST2']), True ,'StrHasPrefix');
  CheckEquals(StrHasPrefix('Test12345',['TEST','TEST2']), True ,'StrHasPrefix');
  CheckEquals(StrHasPrefix('Test21234',['TEST','TEST2']), True ,'StrHasPrefix');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringSearchandReplace._StrIndex;
begin
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringSearchandReplace._StrILastPos;
begin
  Fail('TODO: StrILastPos');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringSearchandReplace._StrIPos;
begin
  Fail('TODO: StrIPos');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringSearchandReplace._StrIsOneOf;
begin
  CheckEquals(StrIsOneOf('Test', ['a','atest','Test', 'Fest']), True, 'StrIsOneOf_1');
  CheckEquals(StrIsOneOf('Test', ['a','atest', 'Fest']), False, 'StrIsOneOf_2');
  CheckEquals(StrIsOneOf('', ['a','atest', 'Fest']), False, 'StrIsOneOf_3');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringSearchandReplace._StrLastPos;
begin
  CheckEquals(StrLastPos('a', 'aaaaaaaaaa'), 10, 'StrLastPos_1');
  CheckEquals(StrLastPos('aba', 'aabaaababababababa'), 16, 'StrLastPos_2');
  CheckEquals(StrLastPos('abba', 'abbaabbabba'), 8, 'StrLastPos_3');
  CheckEquals(StrLastPos('_abba', 'abbaabbabba'), 0, 'StrLastPos_4');
  CheckEquals(StrLastPos('_abba', 'abba_abbabba'), 5, 'StrLastPos_5');
  CheckEquals(StrLastPos('ABA', 'aabaaaABAbabababa'), 7, 'StrLastPos_6');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringSearchandReplace._StrMatch;
begin
  CheckEquals(StrMatch('','Test',1),0,'StrMatch_1');
  CheckEquals(StrMatch('Test','Test',1),1,'StrMatch_2');
  CheckEquals(StrMatch('Test','aTest',1),2,'StrMatch_3');
  CheckEquals(StrMatch('Test','abTest',1),3,'StrMatch_4');
  CheckEquals(StrMatch('Test','abcTest',1),4,'StrMatch_5');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringSearchandReplace._StrNPos;
begin
  CheckEquals(0, StrNPos('testtesttest','Test',3)); // case sensitive test
  CheckEquals(9, StrNPos('TestTestTest','Test',3));

  CheckEquals(1, StrNPos('Test','Test',1), 'StrNPos_1');
  CheckEquals(0, StrNPos('Test','Test',0), 'StrNPos_2');
  CheckEquals(0, StrNPos('Test','Test',-1), 'StrNPos_3');
  CheckEquals(5, StrNPos('TestTest','Test',2), 'StrNPos_4');
  CheckEquals(0, StrNPos('Testtest','Test',2), 'StrNPos_5');
  CheckEquals(3, StrNPos('__Test__','Test',1), 'StrNPos_6');
  CheckEquals(9, StrNPos('__Test__Test','Test',2), 'StrNPos_6');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringSearchandReplace._StrMatches;
begin
  CheckEquals(StrMatches('','Test',1),False,'StrMatches_1');
  CheckEquals(StrMatches('Test','Test',1),True,'StrMatches_2');
  CheckEquals(StrMatches('Test','aTest',2),True,'StrMatches_3');
  CheckEquals(StrMatches('Test','abTest',1),False,'StrMatches_4');
  CheckEquals(StrMatches('Test','abcTest',1),False,'StrMatches_5');
  CheckEquals(True, StrMatches('T?st', 'Test'), 'StrMatches_6');
  CheckEquals(True, StrMatches('T??t', 'Test'), 'StrMatches_6');
  CheckEquals(True, StrMatches('T*', 'Test'), 'StrMatches_6');
  CheckEquals(True, StrMatches('T*st', 'Test'), 'StrMatches_6');
  CheckEquals(False, StrMatches('T*st', 'Tett'), 'StrMatches_6');
  CheckEquals(True, StrMatches('T???', 'Test'), 'StrMatches_6');
  CheckEquals(False, StrMatches('T???', 'Tes'), 'StrMatches_6');
  CheckEquals(True, StrMatches('T?*', 'Test'), 'StrMatches_6');
  CheckEquals(False, StrMatches('T?*', 'T'), 'StrMatches_6');
  CheckEquals(True, StrMatches('T?s?', 'Test'), 'StrMatches_6');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringSearchandReplace._StrNIPos;
begin
  Fail('TODO: StrNIPos');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringSearchandReplace._StrPrefixIndex;
begin
  CheckEquals(StrPrefixIndex('Project',['Pro']),0,'StrPrefixIndex');
  CheckEquals(StrPrefixIndex('Project',['Pro','Con']),0,'StrPrefixIndex');
  CheckEquals(StrPrefixIndex('Project',['']),0,'StrPrefixIndex');
  CheckEquals(StrPrefixIndex('Project',['Con','Pro']),1,'StrPrefixIndex');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringSearchandReplace._StrSearch;
begin
  CheckEquals(StrSearch('', '', 1), 0, 'StrSearch_1');
  CheckEquals(StrSearch('Test', 'Test', 1), 1, 'StrSearch_2');
  CheckEquals(StrSearch('Test', 'Test12', 1), 1, 'StrSearch_3');
  CheckEquals(StrSearch('Test', 'Test123', 1), 1, 'StrSearch_4');
  CheckEquals(StrSearch('Test', 'abTest123', 1), 3, 'StrSearch_5');
  CheckEquals(StrSearch('Test', 'abTest123', 3), 3, 'StrSearch_6');
  CheckEquals(StrSearch('Test', 'abTaest123', 3), 0, 'StrSearch_7');
  CheckEquals(StrSearch('Test', 'abT', 4), 0, 'StrSearch_8');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringCharacterTestRoutines._CharEqualNoCase;
var
  c1, c2: char;

begin
  for c1 := #0 to #255 do
  for c2 := #0 to #255 do
    Check(CharEqualNoCase(c1,c2) = (AnsiUpperCase(C1) = AnsiUpperCase(C2)),Format('CharEqualNoCase: C1: %s C2: %s',[c1,c2]));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringCharacterTestRoutines._CharIsAlpha;
var
  C: char;
begin
  for C := #0 to #255 do
    CheckEquals(LONGBOOL(isalpha(Ord(C))), CharIsAlpha(C), 'CharIsAlpha');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringCharacterTestRoutines._CharIsAlphaNum;
var
  C: char;
begin
  for C := #0 to #255 do
    CheckEquals(LONGBOOL(isalnum(Ord(C))), CharIsAlphaNum(C) , 'CharIsAlphaNum');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringCharacterTestRoutines._CharIsBlank;
begin
  Fail('TODO: CharIsBlank');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringCharacterTestRoutines._CharIsControl;
var
  c1: char;

begin
  for c1 := #0 to #255 do
    CheckEquals(CharIsControl(c1) , (ord(c1) < 32 ),'CharIsControl');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringCharacterTestRoutines._CharIsDelete;
var
  c1: char;

begin
  for c1 := #0 to #255 do
    CheckEquals(CharIsDelete(c1) , (ord(c1) = 8),'CharIsDelete');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringCharacterTestRoutines._CharIsDigit;
var
  c1: char;
  
begin
  for c1 := #0 to #255 do
    CheckEquals(CharIsDigit(c1) , (c1 in ['0'..'9']),'CharIsDigit');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringCharacterTestRoutines._CharIsNumberChar;
var
  c1: char;

begin
  for c1 := #0 to #255 do
    CheckEquals(CharIsNumberChar(c1) , (c1 in ['0'..'9', '+', '-', DecimalSeparator]),'CharIsNumberChar');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringCharacterTestRoutines._CharIsPrintable;
var
  c1: char;

begin
  for c1 := #0 to #255 do
    CheckEquals(CharIsPrintable(c1) , (ord(c1) > 31),'CharIsPrintable');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringCharacterTestRoutines._CharIsPunctuation;
var
  c1: char;
begin
  for c1 := #0 to #255 do
    CheckEquals(CharIsPunctuation(c1) , (c1 in [#123..#126, #91..#96, #38..#47, '@', #60..#63, '#','$','%','"','.',',','!',':','=',';']),'CharIsPunctuation');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringCharacterTestRoutines._CharIsReturn;
var
  c1: char;
begin
  for c1 := #0 to #255 do
    CheckEquals(CharIsReturn(c1) , ((c1 = #13) or (c1 = #10)),'CharIsReturn');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringCharacterTestRoutines._CharIsSpace;
var
  c1: char;
begin
  for c1 := #0 to #255 do
    CheckEquals(CharIsSpace(c1) , c1 in [' ', #9, #10, #11, #12, #13],'CharIsSpace');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringCharacterTestRoutines._CharIsWhiteSpace;
var
  c1: char;
begin
  for c1 := #0 to #255 do
    CheckEquals(CharIsWhiteSpace(c1) , (c1 in AnsiWhiteSpace),'CharIsWhiteSpace');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringCharacterTestRoutines._CharIsUpper;
var
  c1: char;
begin
  for c1 := #0 to #255 do
    CheckEquals(CharIsUpper(c1) , (c1 in ['A'..'Z']),'CharIsUpper');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringCharacterTestRoutines._CharIsLower;
var
  c1: char;
begin
  for c1 := #0 to #255 do
    CheckEquals(CharIsLower(c1) , (c1 in ['a'..'z']),'CharIsLower');
end;


//==================================================================================================
// String Extraction
//==================================================================================================

procedure TJclStringExtraction._StrAfter;
begin
  CheckEquals(StrAfter('',''),'','StrAfter');
  CheckEquals(StrAfter('Hello', 'Hello World'),' World','StrAfter');
  CheckEquals(StrAfter('Hello ', 'Hello World'),'World','StrAfter');
  CheckEquals(StrAfter('is a ', 'This is a test.'),'test.','StrAfter');
  CheckEquals(StrAfter('is a ', 'This is a test. is a test'),'test. is a test','StrAfter');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringExtraction._StrBefore;
begin
  CheckEquals(StrBefore('',''),'','StrBefore');
  CheckEquals(StrBefore('World', 'Hello World'),'Hello ','StrBefore');
  CheckEquals(StrBefore('Hello ', 'Hello World'),'','StrBefore');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringExtraction._StrBetween;
begin
  CheckEquals(StrBetween('',char(#0),char(#0)),'','StrBetween');
  CheckEquals(StrBetween('',char(#0),char(#1)),'','StrBetween');
  CheckEquals(StrBetween('aTestb',char('a'),char('b')),'Test','StrBetween');
  CheckEquals(StrBetween(' Test ',char(' '),char(' ')),'','StrBetween');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringExtraction._StrChopRight;
var
  i: Integer;
  
begin
  for i := -10 to 10 do
    CheckEquals(StrChopRight('',i),'','StrChopRight');

  CheckEquals(StrChopRight('Project JEDI',1),'Project JED','StrChopRight');
  CheckEquals(StrChopRight('Project JEDI',2),'Project JE','StrChopRight');
  CheckEquals(StrChopRight('Project JEDI',3),'Project J','StrChopRight');
  CheckEquals(StrChopRight('Project JEDI',4),'Project ','StrChopRight');
  CheckEquals(StrChopRight('Project JEDI',5),'Project','StrChopRight');
  CheckEquals(StrChopRight('Project JEDI',15),'','StrChopRight');
  CheckEquals(StrChopRight('Project JEDI',50),'','StrChopRight');
  CheckEquals(StrChopRight('Project JEDI',-5),'Project JEDI','StrChopRight');
  CheckEquals(StrChopRight('Project JEDI',-50),'Project JEDI','StrChopRight');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringExtraction._StrLeft;
var
  i: Integer;

begin
  for i := -10 to 10 do
    CheckEquals(StrLeft('',i),'','StrLeft');

  CheckEquals(StrLeft('Project JEDI',0),'','StrLeft');
  CheckEquals(StrLeft('Project JEDI',1),'P','StrLeft');
  CheckEquals(StrLeft('Project JEDI',3),'Pro','StrLeft');
  CheckEquals(StrLeft('Project JEDI',5),'Proje','StrLeft');
  CheckEquals(StrLeft('Project JEDI',-5),'','StrLeft');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringExtraction._StrMid;
begin
  CheckEquals(StrMid('Test',1,4),'Test','StrLeft');
  CheckEquals(StrMid('Test',1,3),'Tes','StrLeft');
  CheckEquals(StrMid('Test',1,2),'Te','StrLeft');
  CheckEquals(StrMid('Test',1,1),'T','StrLeft');
  CheckEquals(StrMid('Test',1,-1),'','StrLeft');
  CheckEquals(StrMid('Test',1,0),'','StrLeft');
  CheckEquals(StrMid('Test',2,0),'','StrLeft');
  CheckEquals(StrMid('Test',2,4),'est','StrLeft');
  CheckEquals(StrMid('Test',2,3),'est','StrLeft');
  CheckEquals(StrMid('Test',2,2),'es','StrLeft');
  CheckEquals(StrMid('Test',2,1),'e','StrLeft');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringExtraction._StrRight;
var
  i: Integer;

begin
  for i := -10 to 10 do
    CheckEquals(StrRight('',i),'','StrRight');

  CheckEquals(StrRight('Test',1),'t','StrRight');
  CheckEquals(StrRight('Test',2),'st','StrRight');
  CheckEquals(StrRight('Test',3),'est','StrRight');
  CheckEquals(StrRight('Test',4),'Test','StrRight');
  CheckEquals(StrRight('Test',8),'Test','StrRight');
  CheckEquals(StrRight('Test',-8),'','StrRight');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringExtraction._StrRestOf;
var
  i: Integer;

begin
  for i := -10 to 10 do
    CheckEquals(StrRestOf('',i),'','StrRestOf');

  for i := -100 to -1 do
    CheckEquals(StrRestOf('Test',i),'Test','StrRestOf');

  CheckEquals(StrRestOf('Test',1),'Test','StrRestOf');
  CheckEquals(StrRestOf('Test',2),'est','StrRestOf');
  CheckEquals(StrRestOf('Test',3),'st','StrRestOf');
end;

//--------------------------------------------------------------------------------------------------

(*
//------------------------------------------------------------------------------

procedure TJclStringsTest.CharacterTransformationRoutines;
var
  i,t : integer;
  c1, c2: char;
  charhextable: array[0..255] of byte;

begin
  // -- CharHex --
  for i:=0 to 255 do
    charhextable[i] := $FF;

  for i := ord('0') to ord('9') do
    charhextable[i] := i - ord('0');

  for i := ord('a') to ord('f') do
    charhextable[i] := 10 + i - ord('a');

  for i := ord('A') to ord('F') do
    charhextable[i] := 10 + i - ord('A');

  for c1 := #0 to #255 do
    CheckEquals(CharHex(c1) , charhextable[ord(c1)], 'CharHex');

  // -- CharLower --
  for c1 := 'A' to 'Z' do
    CheckEquals(CharLower(c1) , chr(ord('a') + ord(c1) - ord('A')),  Format('CharLower %s (%d)',[string(c1),ord(c1)]));

  // -- CharUpper --
  for c1 := 'a' to 'z' do
    CheckEquals(CharUpper(c1) , chr(ord('A') + ord(c1) - ord('a')),  Format('CharUpper %s (%d)',[string(c1),ord(c1)]));

  // -- CharToggleCase --
  for c1 := 'a' to 'z' do
    CheckEquals(CharToggleCase(c1) , chr(ord('A') + ord(c1) - ord('a')),  Format('CharToggleCase %s (%d)',[string(c1),ord(c1)]));

  for c1 := 'A' to 'Z' do
    CheckEquals(CharToggleCase(c1) , chr(ord('a') + ord(c1) - ord('A')),  Format('CharToggleCase %s (%d)',[string(c1),ord(c1)]));
end;

//------------------------------------------------------------------------------

procedure TJclStringsTest.CharacterSearchandReplace;
var
 s: string;
 Strings: TStringList;
 c, c1, c2: char;
 index, i, r: Integer;

begin
  Strings := TStringList.Create;
  try
    Strings.LoadFromFile('Data/charpos.dat');

    i := 0;

    while i < Strings.Count do
    begin
      s := Strings.Strings[i];
      c := (Strings.Strings[i+1])[1];
      index := strtoint(Strings.Strings[i+2]);
      r := CharPos(s, c, index);
      Check(r = strtoint(Strings.Strings[i+3]),Format('CharPos %s %s %d %d ',[s,c,index, r]));
      r := CharIPos(s, c, index);
      Check(r = strtoint(Strings.Strings[i+4]),Format('CharIPos %s',[s]));
      inc(i,5);
    end;

    c := #0;
    r := CharIPos('',c);
    CheckEquals(r , 0,'CharIPos');
    r := CharPos('',c);
    CheckEquals(r , 0,'CharPos');

   // -- CharReplace --

    Strings.LoadFromFile('Data/charreplace.dat');

    i := 0;

    while i < Strings.Count - 1 do
    begin
      s := Strings.Strings[i];
      c1 := (Strings.Strings[i+1])[1];
      c2 := (Strings.Strings[i+2])[1];
      r := strtoint(Strings.Strings[i+3]);
      CheckEquals(CharReplace(s,c1,c2), r , 'CharReplace');
      CheckEquals(s, Strings.Strings[i+4] , 'CharReplace');
      inc(i,5);
    end;

    SetLength(s,0);
    CheckEquals(CharReplace(s,#0,#0) , 0,'CharReplace');

   finally
    Strings.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclStringsTest.PCharVectorRoutines;
var
  Strings: TStringList;
  Strings2: TStringList;
  Vector: PCharVector;
  i: Integer;

begin
  // -- StringsToPCharVector --
  Strings := TStringList.Create;
  try
    Strings2 := TStringList.Create;

    try
      for i := 1 to 1000 do
      begin
        Strings.Add(inttostr(i))
      end;

    StringsToPCharVector(Vector, Strings);

    // -- PCharVectorCount --
    CheckEquals(PCharVectorCount(Vector),1000,'PCharVectorCount');
    CheckEquals(PCharVectorCount(Vector),1000,'PCharVectorCount');

    for i := 1001 to 1500 do
    begin
       Strings.Add(inttostr(i))
    end;

    StringsToPCharVector(Vector, Strings);

    // -- PCharVectorCount --
    CheckEquals(PCharVectorCount(Vector),1500,'PCharVectorCount');
    CheckEquals(PCharVectorCount(Vector),1500,'PCharVectorCount');

    // -- PCharVectorToStrings --
    PCharVectorToStrings(Strings2, Vector);

    for i := 0 to 1499 do
    begin
      CheckEquals(Strings.Strings[i],Strings2.Strings[i],'PCharVectorToStrings');
    end;

    // -- FreePCharVector --
    FreePCharVector(Vector);
    CheckEquals(Integer(Vector),0,'FreePCharVector');
    finally
      Strings2.Free;
    end;

  finally
    Strings.Free;
  end;

end;

//------------------------------------------------------------------------------

procedure TJclStringsTest.MultiSzRoutines;
var
  msz: PChar;
  g: TStringList;
  nb: Integer;
  mszo: PChar;
  s: string;

begin
  g := TStringList.Create;
  try
    g.Add('Project');
    g.Add('JEDI');
    g.Add('RULES!');

    StringsToMultiSz(Msz, g);

    // Check it in memory
    s := 'Project' + #0 + 'JEDI' + #0 + 'RULES!' + #0 + #0;
    MsZo := PChar(s);

    CheckEquals(CompareMem(Msz, MszO, 21), True, 'StringsToMultiSz');

    FreeMultiSz(Msz);
  finally
    g.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclStringsTest.TStringsManipulation;
var
  source, dest: TStringList;

begin
  // -- StrToStrings --

  // -- StringsToStr --

  // -- TrimStrings --

  // -- TrimStringsRight --

  // -- TrimStringsLeft --
end;

//------------------------------------------------------------------------------

procedure TJclStringsTest.Miscellaneous;
var
 S: String;
 B: Boolean;
 SL: TStringList;

begin
 // -- BooleanToStr --
 B := True;
 CheckEquals(BooleanToStr(B) , 'True', 'BooleanToStr(TRUE)');
 CheckEquals(BooleanToStr(not B) , 'False', 'BooleanToStr(FALSE)');

 // -- FileToString --
 // -- StringToFile --

 // -- StrToken --
 S := 'Test1;Test2';
 CheckEquals(StrToken(s,';'),'Test1','StrToken');
 CheckEquals(S,'Test2','StrToken');

 S := ';Test';
 CheckEquals(StrToken(s,';'),'','StrToken');
 CheckEquals(S,'Test','StrToken');

 S := ';;Test';
 CheckEquals(StrToken(s,';'),'','StrToken');
 CheckEquals(S,';Test','StrToken');

 // -- StrTokens --
 // -- StrTokenToStrings --
 SL := TStringList.Create;

 S := 'Test1;Test2;Test3;Test4';
 StrTokenToStrings(S,';',SL);
 CheckEquals(SL.Strings[0],'Test1','StrToken');
 CheckEquals(SL.Strings[1],'Test2','StrToken');
 CheckEquals(SL.Strings[2],'Test3','StrToken');
 CheckEquals(SL.Strings[3],'Test4','StrToken');
 CheckEquals(SL.Count, 4,'StrTokenToStrings');

 SL.Clear;
 S := 'Test1;;Test3;Test4';
 StrTokenToStrings(S,';',SL);
 CheckEquals(SL.Strings[0],'Test1','StrTokenToStrings');
 CheckEquals(SL.Strings[1],'','StrTokenToStrings');
 CheckEquals(SL.Strings[2],'Test3','StrTokenToStrings');
 CheckEquals(SL.Strings[3],'Test4','StrTokenToStrings');
 CheckEquals(SL.Count, 4,'StrTokenToStrings');

 SL.Clear;
 S := '';
 StrTokenToStrings(S,';',SL);
 CheckEquals(SL.Count, 0,'StrTokenToStrings');
 SL.Free;

 // -- StrWord --
 // -- StrToFloatSafe --
 // -- StrToIntSafe --
end;

*)

//==================================================================================================
// TabSet
//==================================================================================================

procedure TJclStringTabSet._CalculatedTabWidth;
var
  tabs1: TJclTabSet;
  tabs2: TJclTabSet;
begin
  tabs1 := TJclTabSet.Create([4,8], True);
  try
    CheckEquals(0, tabs1.TabWidth, 'tabs1.TabWidth');
    CheckEquals(4, tabs1.ActualTabWidth, 'tabs1.ActualTabWidth');
  finally
    FreeAndNil(tabs1);
  end;

  tabs2 := TJclTabSet.Create([4,7], False, -1);
  try
    CheckEquals(-1, tabs2.TabWidth, 'tabs2.TabWidth');
    CheckEquals(3, tabs2.ActualTabWidth, 'tabs2.ActualTabWidth');
  finally
    FreeAndNil(tabs2);
  end;
end;

//------------------------------------------------------------------------------

procedure TJclStringTabSet._Expand;
var
  tabs: TJclTabSet;
  inp: string;
  exp: string;
begin
  tabs := TJclTabSet.Create([17, 22, 32], False, 4);
  try
    inp :=  'Test:'#9'LD'#9'A,(HL)'#9'; Read from memory'#13#10+
            #9'LD'#9'B, 100'#13#10 +
            #9'CALL'#9'Test2'#13#10+
            #9#9#9'; another comment';
    exp :=  'Test:           LD   A,(HL)    ; Read from memory'#13#10 +
            '                LD   B, 100'#13#10 +
            '                CALL Test2'#13#10+
            '                               ; another comment';
    CheckEqualsString(exp, tabs.Expand(inp));
  finally
    FreeAndNil(tabs);
  end;
end;

//------------------------------------------------------------------------------

procedure TJclStringTabSet._FromString;
var
  tabs: TJclTabSet;
begin
  // just a tab width
  tabs := TJclTabSet.FromString('+4');
  try
    CheckEquals(0, tabs.Count, 'FromString(''+4'').Count');
    CheckEquals(False, tabs.ZeroBased, 'FromString(''+4'').ZeroBased');
    CheckEquals(4, tabs.ActualTabWidth, 'FromString(''+4'').ActualTabWidth');
    CheckEquals(4, tabs.TabWidth, 'FromString(''+4'').TabWidth');
  finally
    FreeAndNil(tabs);
  end;

  // stops and tab width; with excessive whitespace, including tab
  tabs := TJclTabSet.FromString('4,   7   ' + #9 + '+4');
  try
    CheckEquals(2, tabs.Count, 'FromString(''4,   7   '' + #9 + ''+4'').Count');
    CheckEquals(4, tabs[0], 'FromString(''4,   7   '' + #9 + ''+4'').tabs[0]');
    CheckEquals(7, tabs[1], 'FromString(''4,   7   '' + #9 + ''+4'').tabs[1]');
    CheckEquals(False, tabs.ZeroBased, 'FromString(''4,   7   '' + #9 + ''+4'').ZeroBased');
    CheckEquals(4, tabs.ActualTabWidth, 'FromString(''4,   7   '' + #9 + ''+4'').ActualTabWidth');
    CheckEquals(4, tabs.TabWidth, 'FromString(''4,   7   '' + #9 + ''+4'').TabWidth');
  finally
    FreeAndNil(tabs);
  end;

  // zero-based, bracketed stops, auto width
  tabs := TJclTabSet.FromString('0[4,7]');
  try
    CheckEquals(2, tabs.Count, 'FromString(''0[4,7]'').Count');
    CheckEquals(4, tabs[0], 'FromString(''0[4,7]'').tabs[0]');
    CheckEquals(7, tabs[1], 'FromString(''0[4,7]'').tabs[1]');
    CheckEquals(True, tabs.ZeroBased, 'FromString(''0[4,7]'').ZeroBased');
    CheckEquals(3, tabs.ActualTabWidth, 'FromString(''0[4,7]'').ActualTabWidth');
    CheckTrue(tabs.TabWidth < 1, 'FromString(''0[4,7]'').TabWidth');
  finally
    FreeAndNil(tabs);
  end;
end;

//------------------------------------------------------------------------------

procedure TJclStringTabSet._NilSet;
var
  tabs: TJclTabSet;
begin
  // simplify nil tabset access
  tabs := nil;

  // nil tabset should be zero based
  CheckTrue(tabs.ZeroBased, 'Nil tabset.ZeroBased');

  // nil tabset should have no tab stops
  CheckEquals(0, tabs.Count, 'Nil tabset.Count');

  // nil tabset should have an actual tabwidth of 2
  CheckEquals(2, tabs.ActualTabWidth, 'Nil tabset.ActualTabWidth');

  // nil tabset should have a set tabwidth of <1 or 2
  CheckTrue((tabs.TabWidth = 2) or (tabs.TabWidth < 1), 'Nil tabset.TabWidth');

  // nil tabset expand test
  CheckEquals('A bc  de', tabs.Expand('A'#9'bc'#9'de'), 'Nil tabset.Expand')
end;

//------------------------------------------------------------------------------

procedure TJclStringTabSet._OptimalFill;
var
  tabs: TJclTabSet;
  tabCount: Integer;
  spaceCount: Integer;
begin
  tabs := TJclTabSet.Create([17, 22, 32], False, 4);
  try
    // test 1: tabs and spaces to get from column 1 to column 17
    tabs.OptimalFillInfo(1, 17, tabCount, spaceCount);
    CheckEquals(1, tabCount, 'tabCount for column 1->17');
    CheckEquals(0, spaceCount, 'spaceCount for column 1->17');

    // test 2: tabs and spaces to get from column 1 to column 4
    tabs.OptimalFillInfo(1, 4, tabCount, spaceCount);
    CheckEquals(0, tabCount, 'tabCount for column 1->4');
    CheckEquals(3, spaceCount, 'spaceCount for column 1->4');

    // test 3: tabs and spaces to get from column 1 to column 34
    tabs.OptimalFillInfo(1, 34, tabCount, spaceCount);
    CheckEquals(3, tabCount, 'tabCount for column 1->34');
    CheckEquals(2, spaceCount, 'spaceCount for column 1->34');
  finally
    FreeAndNil(tabs);
  end;
end;

//------------------------------------------------------------------------------

procedure TJclStringTabSet._Optimize;
var
  tabs: TJclTabSet;
  inp: string;
  exp: string;
begin
  tabs := TJclTabSet.Create([17, 22, 32], False, 4);
  try
    inp := '   '#9'   test                          second';
    exp := #9'   test'#9#9#9#9#9'  second';
    CheckEquals(exp, tabs.Optimize(inp));
  finally
    FreeAndNil(tabs);
  end;
end;

//------------------------------------------------------------------------------

procedure TJclStringTabSet._TabFrom;
var
  tabs: TJclTabSet;
  idx: Integer;
begin
  tabs := TJclTabSet.Create([15, 20, 30], True, 2);
  try
    // test first fixed stop
    // columns 0 through 14 will tab to column 15
    for idx := 0 to 14 do
      CheckEquals(15, tabs.TabFrom(idx), 'set=[15,20,30]+2; TabFrom(' + IntToStr(idx) + ')');

    // test second fixed stop
    // columns 15 through 19 will tab to column 20
    for idx := 15 to 19 do
      CheckEquals(20, tabs.TabFrom(idx), 'set=[15,20,30]+2; TabFrom(' + IntToStr(idx) + ')');

    // test third and final fixed stop
    // columns 20 through 29 will tab to column 30
    for idx := 20 to 29 do
      CheckEquals(30, tabs.TabFrom(idx), 'set=[15,20,30]+2; TabFrom(' + IntToStr(idx) + ')');

    // test tab width beyond fixed positions
    // columns 30 through 39 will tab to column 32 (30-31), 34 (32-33), 36 (34-35), 38 (36-37) or 40 (38-39)
    for idx := 30 to 39 do
      CheckEquals(2 * Succ(idx div 2), tabs.TabFrom(idx), 'set=[15,20,30]+2; TabFrom(' + IntToStr(idx) + ')');
  finally
    FreeAndNil(tabs);
  end;
end;

//------------------------------------------------------------------------------

procedure TJclStringTabSet._TabStopAdding;
var
  tabs: TJclTabSet;
  x: Integer;
  failed: Boolean;
begin
  tabs := TJclTabSet.Create([15, 30], True);
  try
    // Add column 20 and check if the index=1
    CheckEquals(1, tabs.Add(20), 'Index of Add(20)');
    // We should have three stops
    CheckEquals(3, tabs.Count, 'Count after Add(20)');
    // The first should be 15
    CheckEquals(15, tabs[0], 'tabs[0]');
    // The second should be 20
    CheckEquals(20, tabs[1], 'tabs[1]');
    // The third should be 30
    CheckEquals(30, tabs[2], 'tabs[2]');
    // Adding a duplicate should fail...
    begin
      try
        x := tabs.Add(30);
        failed := True;
      except
        failed := False;
        x := 0; // make compiler happy
      end;
      if failed then
        Fail('tabs.Add(30) returned ' + IntToStr(x) + '; should''ve resulted in an exception.');
    end;
    // Adding anything less than StartColumn should fail...
    begin
      try
        x := tabs.Add(tabs.StartColumn - 1);
        failed := True;
      except
        failed := False;
        x := 0;
      end;
      if failed then
        Fail('tabs.Add(' + IntToStr(tabs.StartColumn - 1) + ') returned ' + IntToStr(x) + '; should''ve resulted in an exception.');
    end;
  finally
    FreeAndNil(tabs);
  end;
end;

//------------------------------------------------------------------------------

procedure TJclStringTabSet._TabStopDeleting;
var
  tabs: TJclTabSet;
  x: Integer;
begin
  tabs := TJclTabSet.Create([15, 17, 20, 30], True, 2);
  try
    CheckEquals(1, tabs.Delete(17), 'Index of Delete(17)');
    // We should have three stops
    CheckEquals(3, tabs.Count, 'Count after Add(20)');
    // The first should be 15
    CheckEquals(15, tabs[0], 'tabs[0]');
    // The second should be 20
    CheckEquals(20, tabs[1], 'tabs[1]');
    // The third should be 30
    CheckEquals(30, tabs[2], 'tabs[2]');
    // Deleting a non-existing tab stop should result in a negative value
    x := tabs.Delete(24);
    CheckTrue(x < 0, 'tabs.Delete(24) returned ' + IntToStr(x) + '; should''ve returned a negative value.');
  finally
    FreeAndNil(tabs);
  end;
end;

//------------------------------------------------------------------------------

procedure TJclStringTabSet._TabStopModifying;
var
  tabs: TJclTabSet;
begin
  tabs := TJclTabSet.Create([15, 17, 2, 30], True, 2);
  try
    // check tabs array before overwriting the first tab stop...
    CheckEquals(2, tabs[0], 'tabs[0] before modify.');
    CheckEquals(15, tabs[1], 'tabs[1] before modify.');
    CheckEquals(17, tabs[2], 'tabs[2] before modify.');
    CheckEquals(30, tabs[3], 'tabs[3] before modify.');
    // overwrite the first tab stop
    tabs[0] := 20;
    // check tabs array after overwriting the first tab stop...
    CheckEquals(15, tabs[0], 'tabs[0] after modify.');
    CheckEquals(17, tabs[1], 'tabs[1] after modify.');
    CheckEquals(20, tabs[2], 'tabs[2] after modify.');
    CheckEquals(30, tabs[3], 'tabs[3] after modify.');
  finally
    FreeAndNil(tabs);
  end;
end;

//------------------------------------------------------------------------------

procedure TJclStringTabSet._ToString;
var
  tabs: TJclTabSet;
begin
  tabs := nil;
  CheckEquals('0 [] +2', tabs.ToString, 'nil-set, full');
  CheckEquals('0', tabs.ToString(TabSetFormatting_Default), 'nil-set, default');

  tabs := TJclTabSet.Create([15, 17, 20, 30], True, 4);
  try
    CheckEquals('0 [15,17,20,30] +4', tabs.ToString, 'zero-based, full');
    CheckEquals('0 15,17,20,30 +4', tabs.ToString(TabSetFormatting_Default), 'zero-based, default');
    tabs.ZeroBased := False;
    CheckEquals('[16,18,21,31] +4', tabs.ToString, 'one-based, full');
    CheckEquals('16,18,21,31 +4', tabs.ToString(TabSetFormatting_Default), 'one-based, default');
  finally
    tabs.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclStringTabSet._UpdatePosition;
var
  tabs: TJclTabSet;
  column: Integer;
  line: Integer;
begin
  tabs := TJclTabSet.Create([17, 22, 32], False, 4);
  try
    column := tabs.StartColumn;
    line := 1;
    tabs.UpdatePosition(
      'Label1:'#9'LD'#9'A,0'#9'; init A'#13#10+
      #9'LD'#9'B, 100'#9'; loop counter'#13#10+
      #13#10+
      'lp1:'#9'ADD'#9'(HL)'#9'; add data'#13+
      #9'JR'#9'NC,nxt'#9'; no carry=>skip to nxt'#13+
      #13+
      #9'RRCA'#10+
      #10+
      'nxt:'#9'INC'#9'H'#9'; next scanline'#13#10+
      #9'DJNZ'#9'lp1', column, line);
    CheckEquals(10, line, 'line');
    CheckEquals(25, column, 'column');
  finally
    tabs.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclStringTabSet._ZeroBased;
var
  tabs: TJclTabSet;
  x: Integer;
  failed: Boolean;
begin
  tabs := TJclTabSet.Create([15, 20, 30], True, 2);
  try
    // make sure it's actually zero-based
    CheckTrue(tabs.ZeroBased, 'tabset should be zero based.');
    // can we tab from column 0?
    CheckEquals(15, tabs.TabFrom(0), 'tabs.TabFrom(0) in zero-based mode.');
    // we should have three stops
    CheckEquals(3, tabs.Count, 'tabs.Count (zero-based)');
    // are they 15, 20 and 30 respectively?
    CheckEquals(15, tabs[0], 'tabs[0] (zero-based)');
    CheckEquals(20, tabs[1], 'tabs[1] (zero-based)');
    CheckEquals(30, tabs[2], 'tabs[2] (zero-based)');

    // switch to not zero-based
    tabs.ZeroBased := False;
    // make sure it's no longer zero-based
    CheckFalse(tabs.ZeroBased, 'tabset shouldn''t be zero based.');
    // we still should have three stops
    CheckEquals(3, tabs.Count, 'tabs.Count (not zero-based)');
    // are they 16, 21 and 31 respectively?
    CheckEquals(16, tabs[0], 'tabs[0] (not zero-based)');
    CheckEquals(21, tabs[1], 'tabs[1] (not zero-based)');
    CheckEquals(31, tabs[2], 'tabs[2] (not zero-based)');
    // we shouldn't be able to tab from column 0?
    try
      x := tabs.TabFrom(0);
      failed := False;
    except
      // swallow exception
      failed := True;
      x := 0; // make compiler happy
    end;
    if not failed then
      Fail('tab.TabFrom(0) resulted in ' + IntToStr(x) + '; should''ve resulted in an exception when not in zero-based mode.');
  finally
    FreeAndNil(tabs);
  end;
end;

initialization
  RegisterTest('JCLStrings', TJclStringTransormation.Suite);
  RegisterTest('JCLStrings', TJclStringManagment.Suite);
  RegisterTest('JCLStrings', TJclStringSearchandReplace.Suite);
  RegisterTest('JCLStrings', TJclStringCharacterTestRoutines.Suite);
  RegisterTest('JCLStrings', TJclStringExtraction.Suite);
  RegisterTest('JCLStrings', TJclStringTabSet.Suite);

// History:
//
// $Log$
// Revision 1.3  2004/12/05 15:55:32  rrossmair
// - restored D5 compatibility
//

end.
