unit FpWinAPICompatibility;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF FPC}

interface

uses
  Types,
  SysUtils;

const
  CP_ACP = 0;
  CP_UTF8 = 65001;
  MB_PRECOMPOSED = 1;
  MB_COMPOSITE = 2;
  MB_USEGLYPHCHARS = 4;

type
  QWord = UInt64;
  PtrUInt = QWord;
  LCID = DWORD;
  DWORD_PTR = PtrUInt;

function MultiByteToWideChar(CodePage: DWord; dwFlags: DWord; lpMultiByteStr: PAnsiChar; cchMultiByte: longint; lpWideCharStr:PWideChar; cchWideChar:longint): longint;
function WideCharToMultiByte(CodePage: DWord; dwFlags:DWORD; lpWideCharStr: PWideChar; cchWideChar:longint; lpMultiByteStr: PAnsiChar;cchMultiByte:longint; lpDefaultChar: PAnsiChar; lpUsedDefaultChar: PBoolean):longint;

implementation

uses
  Math;

function MultiByteToWideChar(CodePage: DWord; dwFlags: DWord; lpMultiByteStr: PAnsiChar; cchMultiByte: longint; lpWideCharStr:PWideChar; cchWideChar:longint): longint;
var
  UTF16Str: UnicodeString;
begin
  if CodePage = CP_UTF8 then
  begin
    Result := Utf8ToUnicode(lpWideCharStr, cchWideChar, lpMultiByteStr, cchMultiByte);
  end
  else
  begin
    //fpc 2.6.x does not support arbitrary code pages. Uses ansi for now
    if cchMultiByte = -1 then
      cchMultiByte := strlen(lpMultiByteStr);
    widestringmanager.Ansi2UnicodeMoveProc(lpMultiByteStr, CodePage, UTF16Str, cchWideChar);
    Result := Min(cchWideChar, Length(UTF16Str));
    if Result > 0 then
      Move(UTF16Str[1], lpWideCharStr^, Result * SizeOf(PWideChar));
  end;
end;

//copied from fpc trunk
function fpc_pwidechar_length(p:pwidechar):sizeint;
var i : sizeint;
begin
  i:=0;
  if assigned(p) then
    while p[i]<>#0 do
      inc(i);
  exit(i);
end;

function WideCharToMultiByte(CodePage: DWord; dwFlags:DWORD; lpWideCharStr: PWideChar; cchWideChar:longint; lpMultiByteStr: PAnsiChar;cchMultiByte:longint; lpDefaultChar: PAnsiChar; lpUsedDefaultChar: PBoolean):longint;
var
  AnsiStr: AnsiString;
begin
  if CodePage = CP_UTF8 then
  begin
    Result := UnicodeToUtf8(lpMultiByteStr, cchMultiByte, lpWideCharStr, cchWideChar);
  end
  else
  begin
    //fpc 2.6.x does not support arbitrary code pages. Uses ansi for now
    if cchWideChar = -1 then
      cchWideChar := fpc_pwidechar_length(lpWideCharStr);
    widestringmanager.Unicode2AnsiMoveProc(lpWideCharStr, AnsiStr, CodePage, cchWideChar);
    Result := Min(cchMultiByte, Length(AnsiStr));
    if Result > 0 then
      Move(AnsiStr[1], lpMultiByteStr^, Result);
  end;
end;

end.

