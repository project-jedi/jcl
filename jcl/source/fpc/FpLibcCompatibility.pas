unit FpLibcCompatibility;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

Function isgraph(c : Byte) : Integer;
Function isupper(c : Byte) : Integer;
Function islower(c : Byte) : Integer;
Function isalpha(c : Byte) : Integer;
Function isdigit(c : Byte) : Integer;
Function isalnum(c : Byte) : Integer;
Function ispunct(c : Byte) : Integer;
Function isspace(c : Byte) : Integer;
Function iscntrl(c : Byte) : Integer;
Function isblank(c : Byte) : Integer;
Function isxdigit(c : Byte) : Integer;
Function tolower(c : Byte) : Integer;
Function toupper(c : Byte) : Integer;

implementation

Function isgraph(c : Byte) : Integer;
begin
     if (c - $21) < $5e
         then isgraph := 1
     else isgraph := 0;
end;

Function isupper(c : Byte) : Integer;
begin
     if ((c >= 192) and (c <= 223)) or ((AnsiChar(c) >= 'A') and (AnsiChar(c) <= 'Z'))
         then isupper := 1
     else isupper := 0;
end;

Function islower(c : Byte) : Integer;
begin
     if ((c >= 224) and (c <= 254)) or ((AnsiChar(c) >= 'a') and (AnsiChar(c) <= 'z'))
         then islower := 1
     else islower := 0;
end;

Function isalpha(c : Byte) : Integer;
begin
     if ((c >= 192) and (c <= 223)) or ((AnsiChar(c) >= 'A') and (AnsiChar(c) <= 'Z'))
     or ((c >= 224) and (c <= 254)) or ((AnsiChar(c) >= 'a') and (AnsiChar(c) <= 'z'))
         then isalpha := 1
     else isalpha := 0;
end;

Function isdigit(c : Byte) : Integer;
begin
     if AnsiChar(c) in ['0'..'9'] then isdigit := 1
     else isdigit := 0;
end;

Function isalnum(c : Byte) : Integer;
begin
     if (isalpha(c) <> 0) or (isdigit(c) <> 0) then isalnum := 1
     else isalnum := 0;
end;

Function ispunct(c : Byte) : Integer;
begin
     if (isgraph(c) = 1) and (isalnum(c) = 0)
         then ispunct := 1
     else ispunct := 0;
end;


Function isspace(c : Byte) : Integer;
begin
     if AnsiChar(c) in [#9..#13,#32] then isspace := 1
     else isspace := 0;
end;

Function iscntrl(c : Byte) : Integer;
begin
     if (c < $20) or (c = $7f) then iscntrl := 1
     else iscntrl := 0;
end;

Function isblank(c : Byte) : Integer;
begin
     if c in [9, 32] then isblank := 1
     else isblank := 0;
end;

Function isxdigit(c : Byte) : Integer;
begin
     if (isdigit(c) = 1) or ((c in [ord('a')..ord('f')]) or (c in [ord('A')..ord('F')])) then isxdigit := 1
     else isxdigit := 0;
end;

Function tolower(c : Byte) : Integer;
begin
  Result := Integer(lowerCase(Char(c)));
end;

Function toupper(c : Byte) : Integer;
begin
  Result := Integer(upCase(Char(c)));
end;

end.

