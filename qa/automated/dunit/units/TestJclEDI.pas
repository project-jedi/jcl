unit TestJclEDI;

interface

{.$DEFINE USING_EDI_NEW_PROTOTYPE}

uses
  SysUtils,
  TestFrameWork, TestExtensions,
{$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
  	_EDI_;
{$ELSE}
  	JclEDI;
{$ENDIF}

type

{$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
  TEDIAnsiStringData_Tests = class(TTestCase)
  private
    FData: TEDIAnsiStringData;
  protected
    procedure Setup; override;
    procedure TearDown; override;
	published
    procedure Comprehensive_001;
  end;

  TEDIStringStreamData_Tests = class(TTestCase)
  private
    FData: TEDIStringStreamData;
  protected
    procedure Setup; override;
    procedure TearDown; override;
	published
    procedure Comprehensive_001;
  end;
{$ENDIF}

  TEDIObjectList_Tests = class(TTestCase)
	published
    procedure Comprehensive_001;
    procedure Comprehensive_002;
  end;

	TEDIStringReplace_Tests = class(TTestCase)
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

const
	ErrorMsg_001 = 'JclEDI.StringReplace failure!';

	TestData01 = 'aaabbbcccdddeee';
	TestData02 = 'abcdefghijklmno';
	TestData03 = 'aaabbbcccdddeeeaaabbbcccdddeee';

{ TEDIObjectList_Tests }
{
	Tests with items
}
procedure TEDIObjectList_Tests.Comprehensive_001;
var
  OL: TEDIObjectList;
  LI, LI0, LI1, LI2, LI3, LI4, LI5: TEDIObjectListItem;
begin
  OL := TEDIObjectList.Create(True);
  try
    LI0 := TEDIObjectListItem.Create(nil, nil, TEDIObject.Create);
    OL.Add(LI0, 'Element 1');
    LI1 := TEDIObjectListItem.Create(nil, nil, TEDIObject.Create);
    OL.Add(LI1, 'Element 2');
    LI2 := TEDIObjectListItem.Create(nil, nil, TEDIObject.Create);
    OL.Add(LI2, 'Element 3');
    LI3 := TEDIObjectListItem.Create(nil, nil, TEDIObject.Create);
    OL.Add(LI3, 'Element 4');
    LI4 := TEDIObjectListItem.Create(nil, nil, TEDIObject.Create);
    OL.Add(LI4, 'Element 5');
    Check(OL.Count = 5, 'TEDIObjectList.Add failed.');
    LI5 := OL.Find(LI2);
    Check(LI5 = LI2, 'TEDIObjectList.Find failed.');
    LI2 := OL.Extract(LI2);
    Check(OL.Count = 4, 'TEDIObjectList.Extract failed.');
    OL.Remove(LI1);
    Check(OL.Count = 3, 'TEDIObjectList.Remove failed.');
    LI2 := OL.Insert(LI2, LI3);
    Check(OL.Count = 4, 'TEDIObjectList.Insert failed.');
    LI1 := OL.Insert(LI2);
    Check(OL.Count = 5, 'TEDIObjectList.Insert failed.');
    LI := OL.First;
    Check(LI = LI0, 'TEDIObjectList.First failed.');
    LI := OL.Next;
    Check(LI = LI1, 'TEDIObjectList.Next failed.');
    LI := OL.Last;
    Check(LI = LI4, 'TEDIObjectList.Last failed.');
    LI := OL.Prior;
    Check(LI = LI3, 'TEDIObjectList.Prior failed.');
  finally
    OL.Free;
  end; //try
end;
{
	Tests with objects
}
procedure TEDIObjectList_Tests.Comprehensive_002;
var
  OL: TEDIObjectList;
  {LI0, }LI1, LI2, LI3, {LI4, }LI5: TEDIObjectListItem;
  O: TEDIObject;
begin
  OL := TEDIObjectList.Create(True);
  try
    {LI0 := }OL.Add(TEDIObject.Create, 'Element 1');
    LI1 := OL.Add(TEDIObject.Create, 'Element 2');
    LI2 := OL.Add(TEDIObject.Create, 'Element 3');
    LI3 := OL.Add(TEDIObject.Create, 'Element 4');
    {LI4 := }OL.Add(TEDIObject.Create, 'Element 5');
    Check(OL.Count = 5, 'TEDIObjectList.Add failed.');
    {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
    LI5 := OL.FindItem(LI2.EDIObject);
    {$ELSE}
    LI5 := OL.Find(LI2.EDIObject);
    {$ENDIF}
    Check(LI5 = LI2, 'TEDIObjectList.Find failed.');
    O := OL.Extract(LI2.EDIObject);
    Check(OL.Count = 4, 'TEDIObjectList.Extract failed.');
    OL.Remove(LI1.EDIObject);
    Check(OL.Count = 3, 'TEDIObjectList.Remove failed.');
    LI2 := OL.Insert(O, LI3.EDIObject);
    Check(OL.Count = 4, 'TEDIObjectList.Insert failed.');
    {LI1 := }OL.Insert(LI2.EDIObject);
    Check(OL.Count = 5, 'TEDIObjectList.Insert failed.');
  finally
    OL.Free;
  end; //try
end;

{ TEDIStringReplace_Tests }

procedure TEDIStringReplace_Tests.DoTest(S1, OldPattern, NewPattern: string;
	Flags: TReplaceFlags);
var
  S2, S3: string;
begin
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, Flags);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, Flags);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, Flags);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace a single character with a single character.
}
procedure TEDIStringReplace_Tests.StringReplace_001;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := '5';
	NewPattern := '7';
	S1 := 'Delphi 5 is great!';
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, []);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace a single character with a string.
}
procedure TEDIStringReplace_Tests.StringReplace_002;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := '5';
	NewPattern := '2005';
	S1 := 'Delphi 5 is great!';
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, []);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace a string with a single character.
}
procedure TEDIStringReplace_Tests.StringReplace_003;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := '2005';
	NewPattern := '5';
	S1 := 'Delphi 2005 is great!';
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, []);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace the first single character with a single character.
}
procedure TEDIStringReplace_Tests.StringReplace_004;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := '7';
	NewPattern := '5';
	S1 := 'Delphi 7 was great! Delphi 7 is better!';
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, []);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace the first single character with a string
}
procedure TEDIStringReplace_Tests.StringReplace_005;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := '5';
	NewPattern := 'three';
	S1 := 'Delphi 5 was great! Delphi 5 is better!';
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, []);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace the first string with a single character
}
procedure TEDIStringReplace_Tests.StringReplace_006;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'five';
	NewPattern := '3';
	S1 := 'Delphi five was great! Delphi 5 is better!';
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, []);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace char pattern (beginning)
}
procedure TEDIStringReplace_Tests.StringReplace_007;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'a';
	NewPattern := 'z';
	S1 := TestData01;
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace char pattern (middle)
}
procedure TEDIStringReplace_Tests.StringReplace_008;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'c';
	NewPattern := 'z';
	S1 := TestData01;
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace char pattern (end)
}
procedure TEDIStringReplace_Tests.StringReplace_009;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'e';
	NewPattern := 'z';
	S1 := TestData01;
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace char pattern (beginning)
	with string
}
procedure TEDIStringReplace_Tests.StringReplace_010;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'a';
	NewPattern := 'zzz';
	S1 := TestData01;
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace char pattern (middle)
	with string
}
procedure TEDIStringReplace_Tests.StringReplace_011;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'c';
	NewPattern := 'zzz';
	S1 := TestData01;
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace char pattern (end)
	with string
}
procedure TEDIStringReplace_Tests.StringReplace_012;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'e';
	NewPattern := 'zzz';
	S1 := TestData01;
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, [rfReplaceAll]);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace string (beginning)
	with char
}
procedure TEDIStringReplace_Tests.StringReplace_013;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'zzz';
	NewPattern := 'a';
	S1 := 'zzzbbbcccdddeee';
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, []);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace string (middle)
	with char
}
procedure TEDIStringReplace_Tests.StringReplace_014;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'zzz';
	NewPattern := 'c';
	S1 := 'aaabbbzzzdddeee';
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, []);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace string (end)
	with char
}
procedure TEDIStringReplace_Tests.StringReplace_015;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'zzz';
	NewPattern := 'e';
	S1 := 'aaabbbcccdddzzz';
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, []);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace large string with small string (beginning)
}
procedure TEDIStringReplace_Tests.StringReplace_016;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'aaa';
	NewPattern := 'zz';
	S1 := TestData01;
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, []);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace large string with small string (middle)
}
procedure TEDIStringReplace_Tests.StringReplace_017;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'ccc';
	NewPattern := 'zz';
	S1 := TestData01;
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, []);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace large string with small string (end)
}
procedure TEDIStringReplace_Tests.StringReplace_018;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'eee';
	NewPattern := 'zz';
	S1 := TestData01;
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, []);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace small string with large string (beginning)
}
procedure TEDIStringReplace_Tests.StringReplace_019;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'aaa';
	NewPattern := 'zzzzzz';
	S1 := TestData01;
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, []);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace small string with large string (middle)
}
procedure TEDIStringReplace_Tests.StringReplace_020;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'ccc';
	NewPattern := 'zzzzzz';
	S1 := TestData01;
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, []);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Replace small string with large string (end)
}
procedure TEDIStringReplace_Tests.StringReplace_021;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'eee';
	NewPattern := 'zzzzzz';
	S1 := TestData01;
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, []);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Remove first char
}
procedure TEDIStringReplace_Tests.StringReplace_022;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'a';
	NewPattern := '';
	S1 := TestData02;
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, []);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Remove middle char
}
procedure TEDIStringReplace_Tests.StringReplace_023;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'h';
	NewPattern := '';
	S1 := TestData02;
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, []);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Remove last char
}
procedure TEDIStringReplace_Tests.StringReplace_024;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'o';
	NewPattern := '';
	S1 := TestData02;
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, []);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Remove first string
}
procedure TEDIStringReplace_Tests.StringReplace_025;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'aaa';
	NewPattern := '';
	S1 := TestData01;
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, []);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Remove middle string
}
procedure TEDIStringReplace_Tests.StringReplace_026;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'ccc';
	NewPattern := '';
	S1 := TestData01;
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, []);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
	Remove last string
}
procedure TEDIStringReplace_Tests.StringReplace_027;
var
	OldPattern, NewPattern: string;
	S1, S2, S3: string;
begin
	OldPattern := 'eee';
	NewPattern := '';
	S1 := TestData01;
  {$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
	S2 := _EDI_.StringReplace(S1, OldPattern, NewPattern, []);
  {$ELSE}
	S2 := JclEDI.StringReplace(S1, OldPattern, NewPattern, []);
  {$ENDIF}
	S3 := SysUtils.StringReplace(S1, OldPattern, NewPattern, []);
	Check(S2 = S3, ErrorMsg_001);
end;
{
  Multi-string replacement tests
}
procedure TEDIStringReplace_Tests.StringReplace_028;
begin
  DoTest(TestData01, 'aaa', 'zzz', [rfReplaceAll]);
end;

procedure TEDIStringReplace_Tests.StringReplace_029;
begin
  DoTest(TestData01, 'ccc', 'zzz', [rfReplaceAll]);
end;

procedure TEDIStringReplace_Tests.StringReplace_030;
begin
  DoTest(TestData01, 'eee', 'zzz', [rfReplaceAll]);
end;

procedure TEDIStringReplace_Tests.StringReplace_031;
begin
  DoTest(TestData01, 'aaa', 'zz', [rfReplaceAll]);
end;

procedure TEDIStringReplace_Tests.StringReplace_032;
begin
  DoTest(TestData01, 'ccc', 'zz', [rfReplaceAll]);
end;

procedure TEDIStringReplace_Tests.StringReplace_033;
begin
  DoTest(TestData01, 'eee', 'zz', [rfReplaceAll]);
end;

procedure TEDIStringReplace_Tests.StringReplace_034;
begin
  DoTest(TestData01, 'aaa', 'zzzzzz', [rfReplaceAll]);
end;

procedure TEDIStringReplace_Tests.StringReplace_035;
begin
  DoTest(TestData01, 'ccc', 'zzzzzz', [rfReplaceAll]);
end;

procedure TEDIStringReplace_Tests.StringReplace_036;
begin
  DoTest(TestData01, 'eee', 'zzzzzz', [rfReplaceAll]);
end;

{$IFDEF USING_EDI_NEW_PROTOTYPE THEN}

{ TEDIAnsiStringData_Tests }

procedure TEDIAnsiStringData_Tests.Comprehensive_001;
const
  FileName = 'c:\temp\TEDIAnsiStringData_Test_001.edi';
  FileData1 = 'This is a test string of data.';
  FileData2 = ' Some more data.';
var
  I, J: Integer;
  Data: string;
begin
  FData.Append(FileData1);
  //Test SaveToFile-LoadFromFile
  FData.SaveToFile(FileName);
  FData.LoadFromFile(FileName);
	Check(FData.Data = FileData1, 'TEDIAnsiStringData.SaveToFile-LoadFromFile failure.');
  //Test IndexOf
  I := FData.IndexOf('test', 1);
  J := AnsiPos('test', FData.Data);
	Check(I = J, 'TEDIAnsiStringData.IndexOf failure.');
  //Test IsEqual
	Check(FData.IsEqual(I, 'test'), 'TEDIAnsiStringData.IsEqual failure.');
  //Test Copy
  Data := FData.Copy(I, 4);
	Check(Data = 'test', 'TEDIAnsiStringData.Copy failure.');
  //Test Append
  FData.Length := FData.Length + Length(FileData2);
  FData.Append(FileData2);
  Data := FData.Copy(Length(FileData1) + 1, Length(FileData2));
	Check(Data = FileData2, 'TEDIAnsiStringData.Append failure.');
  //Test Replace
  Data := SysUtils.StringReplace(FData.Data, 'a', 'z', [rfReplaceAll, rfIgnoreCase]);
  FData.Replace('a', 'z');
	Check(Data = FData.Data, 'TEDIAnsiStringData.Append failure.');
  //Test Clear
  FData.Clear;
  Check(FData.Data = '', 'TEDIAnsiStringData.Clear failure.');
end;

procedure TEDIAnsiStringData_Tests.Setup;
begin
  inherited;
  ForceDirectories('c:\Temp');
  FData := TEDIAnsiStringData.Create('');
end;

procedure TEDIAnsiStringData_Tests.TearDown;
begin
  FData.Free;
  inherited;
end;

{ TEDIStringStreamData_Tests }

procedure TEDIStringStreamData_Tests.Comprehensive_001;
const
  FileName = 'c:\temp\TEDIStringStreamData_Test_001.edi';
  FileData1 = 'This is a test string of data.';
  FileData2 = ' Some more data.';
var
  I, J: Integer;
  Data: string;
begin
  FData.Append(FileData1);
  //Test SaveToFile-LoadFromFile
  FData.SaveToFile(FileName);
  FData.LoadFromFile(FileName);
	Check(FData.Data = FileData1, 'TEDIStringStreamData.SaveToFile-LoadFromFile failure.');
  //Test IndexOf
  I := FData.IndexOf('test', 1);
  J := AnsiPos('test', FData.Data);
	Check(I = J, 'TEDIStringStreamData.IndexOf failure.');
  //Test IsEqual
	Check(FData.IsEqual(I, 'test'), 'TEDIStringStreamData.IsEqual failure.');
  //Test Copy
  Data := FData.Copy(I, 4);
	Check(Data = 'test', 'TEDIStringStreamData.Copy failure.');
  //Test Append
  FData.Append(FileData2);
  Data := FData.Copy(Length(FileData1) + 1, Length(FileData2));
	Check(Data = FileData2, 'TEDIStringStreamData.Append failure.');
  //Test Replace
  Data := SysUtils.StringReplace(FData.Data, 'a', 'z', [rfReplaceAll, rfIgnoreCase]);
  FData.Replace('a', 'z');
	Check(Data = FData.Data, 'TEDIStringStreamData.Append failure.');
  //Test Clear
  FData.Clear;
  Check(FData.Data = '', 'TEDIStringStreamData.Clear failure.');
end;

procedure TEDIStringStreamData_Tests.Setup;
begin
  inherited;
  ForceDirectories('c:\Temp');
  FData := TEDIStringStreamData.Create('');
end;

procedure TEDIStringStreamData_Tests.TearDown;
begin
  FData.Free;
  inherited;
end;

{$ENDIF}

initialization

{$IFDEF USING_EDI_NEW_PROTOTYPE THEN}
 TestFramework.RegisterTests('JclEDI',
   [TEDIAnsiStringData_Tests.Suite,
    TEDIStringStreamData_Tests.Suite]);
{$ENDIF}

 TestFramework.RegisterTests('JclEDI',
   [TEDIObjectList_Tests.Suite,
    TEDIStringReplace_Tests.Suite]);

end.
