unit TestJclEDI_ANSIX12;

interface

uses
  SysUtils, Classes,
  TestFrameWork, TestExtensions;

type
	TJclEDI_ANSIX12_Tests = class(TTestCase)
	private
    FTestData: TStringList;
    procedure Setup; override;
    procedure TearDown; override;
	published
    procedure TEDISegment_Disassemble;
    procedure TEDISegment_Assemble;

//    procedure TEDISegment_Disassemble;
//    procedure TEDISegment_Assemble;
  end;

implementation

uses
	JclEDI, JclEDI_ANSIX12;

const
  TestData001File = 'TestData001.txt';

{ TJclEDI_ANSIX12_Tests }

procedure TJclEDI_ANSIX12_Tests.Setup;
begin
  inherited;
  FTestData := TStringList.Create;
  if FileExists(TestData001File) then
    FTestData.LoadFromFile(TestData001File);
end;

procedure TJclEDI_ANSIX12_Tests.TearDown;
begin
  FTestData.Free;
  inherited;
end;

procedure TJclEDI_ANSIX12_Tests.TEDISegment_Assemble;
var
  S: TEDISegment;
  Result: string;
begin
  S := TEDISegment.Create(nil, 5);
  S.Delimiters := TEDIDelimiters.Create;
  S.SegmentId := 'S1';
  S[0].Data := 'Data1';
  S[1].Data := 'Data2';
  S[2].Data := 'Data3';
  S[3].Data := 'Data4';
  S[4].Data := 'Data5';
  Result := S.Assemble;
  Check(Result = 'S1*Data1*Data2*Data3*Data4*Data5~', 'Assemble Failure');
end;

procedure TJclEDI_ANSIX12_Tests.TEDISegment_Disassemble;
var
  S: TEDISegment;
begin
  S := TEDISegment.Create(nil);
  S.Delimiters := TEDIDelimiters.Create;
  S.Data := 'S1*Data1*Data2*Data3*Data4*Data5~';
  S.Disassemble;
  Check(S.SegmentId = 'S1', 'Parse Segment Id Failure');
  Check(S.Element[0].Data = 'Data1', 'Parse Element 1 Failure');
  Check(S.Element[1].Data = 'Data2', 'Parse Element 2 Failure');
  Check(S.Element[2].Data = 'Data3', 'Parse Element 3 Failure');
  Check(S.Element[3].Data = 'Data4', 'Parse Element 4 Failure');
  Check(S.Element[4].Data = 'Data5', 'Parse Element 5 Failure');
end;

initialization
  RegisterTests('JclEDI_ANSIX12', [TJclEDI_ANSIX12_Tests.Suite]);

end.
