unit TestJclEDI;

interface

uses
  SysUtils,
  TestFrameWork, TestExtensions;

type
	TJclEDI_StringReplace_Tests = class(TTestCase)
	private
	  procedure DoTest(S1, OldPattern, NewPattern: string;
      Flags: TReplaceFlags = []);
	published
		procedure StringReplace_001;
		procedure StringReplace_002;
		procedure StringReplace_003;
		procedure StringReplace_004;
		procedure StringReplace_005;
		procedure StringReplace_006;
		procedure StringReplace_007;
		procedure StringReplace_008;
		procedure StringReplace_009;
		procedure StringReplace_010;
		procedure StringReplace_011;
		procedure StringReplace_012;
		procedure StringReplace_013;
		procedure StringReplace_014;
		procedure StringReplace_015;
		procedure StringReplace_016;
		procedure StringReplace_017;
		procedure StringReplace_018;
		procedure StringReplace_019;
		procedure StringReplace_020;
		procedure StringReplace_021;
		procedure StringReplace_022;
		procedure StringReplace_023;
		procedure StringReplace_024;
		procedure StringReplace_025;
		procedure StringReplace_026;
		procedure StringReplace_027;
		procedure StringReplace_028;
		procedure StringReplace_029;
		procedure StringReplace_030;
		procedure StringReplace_031;
		procedure StringReplace_032;
		procedure StringReplace_033;
		procedure StringReplace_034;
		procedure StringReplace_035;
		procedure StringReplace_036;
	end;

implementation

uses
	JclEDI;

const
	ErrorMsg_001 = 'JclEDI.StringReplace failure!';

	TestData01 = 'aaabbbcccdddeee';
	TestData02 = 'abcdefghijklmno';
	TestData03 = 'aaabbbcccdddeeeaaabbbcccdddeee';

procedure TJclEDI_StringReplace_Tests.DoTest(S1, OldPattern, NewPattern: string;
	Flags: TReplaceFlags);
var
  S2, S3: string;
begin
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, Flags);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, Flags);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace a single character with a single character.
}
procedure TJclEDI_StringReplace_Tests.StringReplace_001;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := '5';
	NewPattern := '7';
	S1 := 'Delphi 5 is great!';
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace a single character with a string.
}
procedure TJclEDI_StringReplace_Tests.StringReplace_002;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := '5';
	NewPattern := '2005';
	S1 := 'Delphi 5 is great!';
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace a string with a single character.
}
procedure TJclEDI_StringReplace_Tests.StringReplace_003;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := '2005';
	NewPattern := '5';
	S1 := 'Delphi 2005 is great!';
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace the first single character with a single character.
}
procedure TJclEDI_StringReplace_Tests.StringReplace_004;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := '7';
	NewPattern := '5';
	S1 := 'Delphi 7 was great! Delphi 7 is better!';
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace the first single character with a string
}
procedure TJclEDI_StringReplace_Tests.StringReplace_005;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := '5';
	NewPattern := 'three';
	S1 := 'Delphi 5 was great! Delphi 5 is better!';
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace the first string with a single character
}
procedure TJclEDI_StringReplace_Tests.StringReplace_006;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'five';
	NewPattern := '3';
	S1 := 'Delphi five was great! Delphi 5 is better!';
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace char pattern (beginning)
}
procedure TJclEDI_StringReplace_Tests.StringReplace_007;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'a';
	NewPattern := 'z';
	S1 := TestData01;
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace char pattern (middle)
}
procedure TJclEDI_StringReplace_Tests.StringReplace_008;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'c';
	NewPattern := 'z';
	S1 := TestData01;
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace char pattern (end)
}
procedure TJclEDI_StringReplace_Tests.StringReplace_009;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'e';
	NewPattern := 'z';
	S1 := TestData01;
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace char pattern (beginning)
	with string
}
procedure TJclEDI_StringReplace_Tests.StringReplace_010;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'a';
	NewPattern := 'zzz';
	S1 := TestData01;
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace char pattern (middle)
	with string
}
procedure TJclEDI_StringReplace_Tests.StringReplace_011;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'c';
	NewPattern := 'zzz';
	S1 := TestData01;
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace char pattern (end)
	with string
}
procedure TJclEDI_StringReplace_Tests.StringReplace_012;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'e';
	NewPattern := 'zzz';
	S1 := TestData01;
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace string (beginning)
	with char
}
procedure TJclEDI_StringReplace_Tests.StringReplace_013;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'zzz';
	NewPattern := 'a';
	S1 := 'zzzbbbcccdddeee';
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace string (middle)
	with char
}
procedure TJclEDI_StringReplace_Tests.StringReplace_014;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'zzz';
	NewPattern := 'c';
	S1 := 'aaabbbzzzdddeee';
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace string (end)
	with char
}
procedure TJclEDI_StringReplace_Tests.StringReplace_015;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'zzz';
	NewPattern := 'e';
	S1 := 'aaabbbcccdddzzz';
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace large string with small string (beginning)
}
procedure TJclEDI_StringReplace_Tests.StringReplace_016;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'aaa';
	NewPattern := 'zz';
	S1 := TestData01;
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace large string with small string (middle)
}
procedure TJclEDI_StringReplace_Tests.StringReplace_017;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'ccc';
	NewPattern := 'zz';
	S1 := TestData01;
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace large string with small string (end)
}
procedure TJclEDI_StringReplace_Tests.StringReplace_018;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'eee';
	NewPattern := 'zz';
	S1 := TestData01;
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace small string with large string (beginning)
}
procedure TJclEDI_StringReplace_Tests.StringReplace_019;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'aaa';
	NewPattern := 'zzzzzz';
	S1 := TestData01;
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace small string with large string (middle)
}
procedure TJclEDI_StringReplace_Tests.StringReplace_020;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'ccc';
	NewPattern := 'zzzzzz';
	S1 := TestData01;
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace small string with large string (end)
}
procedure TJclEDI_StringReplace_Tests.StringReplace_021;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'eee';
	NewPattern := 'zzzzzz';
	S1 := TestData01;
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Remove first char
}
procedure TJclEDI_StringReplace_Tests.StringReplace_022;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'a';
	NewPattern := '';
	S1 := TestData02;
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Remove middle char
}
procedure TJclEDI_StringReplace_Tests.StringReplace_023;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'h';
	NewPattern := '';
	S1 := TestData02;
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Remove last char
}
procedure TJclEDI_StringReplace_Tests.StringReplace_024;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'o';
	NewPattern := '';
	S1 := TestData02;
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Remove first string
}
procedure TJclEDI_StringReplace_Tests.StringReplace_025;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'aaa';
	NewPattern := '';
	S1 := TestData01;
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Remove middle string
}
procedure TJclEDI_StringReplace_Tests.StringReplace_026;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'ccc';
	NewPattern := '';
	S1 := TestData01;
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Remove last string
}
procedure TJclEDI_StringReplace_Tests.StringReplace_027;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'eee';
	NewPattern := '';
	S1 := TestData01;
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
  Multi-string replacement tests
}
procedure TJclEDI_StringReplace_Tests.StringReplace_028;
begin
  DoTest(TestData01, 'aaa', 'zzz', [rfReplaceAll]);
end;

procedure TJclEDI_StringReplace_Tests.StringReplace_029;
begin
  DoTest(TestData01, 'ccc', 'zzz', [rfReplaceAll]);
end;

procedure TJclEDI_StringReplace_Tests.StringReplace_030;
begin
  DoTest(TestData01, 'eee', 'zzz', [rfReplaceAll]);
end;

procedure TJclEDI_StringReplace_Tests.StringReplace_031;
begin
  DoTest(TestData01, 'aaa', 'zz', [rfReplaceAll]);
end;

procedure TJclEDI_StringReplace_Tests.StringReplace_032;
begin
  DoTest(TestData01, 'ccc', 'zz', [rfReplaceAll]);
end;

procedure TJclEDI_StringReplace_Tests.StringReplace_033;
begin
  DoTest(TestData01, 'eee', 'zz', [rfReplaceAll]);
end;

procedure TJclEDI_StringReplace_Tests.StringReplace_034;
begin
  DoTest(TestData01, 'aaa', 'zzzzzz', [rfReplaceAll]);
end;

procedure TJclEDI_StringReplace_Tests.StringReplace_035;
begin
  DoTest(TestData01, 'ccc', 'zzzzzz', [rfReplaceAll]);
end;

procedure TJclEDI_StringReplace_Tests.StringReplace_036;
begin
  DoTest(TestData01, 'eee', 'zzzzzz', [rfReplaceAll]);
end;

initialization
 TestFramework.RegisterTest(TJclEDI_StringReplace_Tests.Suite);

end.
