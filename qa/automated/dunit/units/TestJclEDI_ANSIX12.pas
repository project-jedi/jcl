unit TestJclEDI_ANSIX12;

interface

uses
  SysUtils, Classes,
	JclEDI, JclEDI_ANSIX12,
  TestFrameWork, TestExtensions;

type
	TJclEDI_ANSIX12_Tests = class(TTestCase)
	private
    FTestData: TStringList;
    procedure CheckSegment(SegmentId, DataSeries: string; S: TEDISegment);
    procedure CreateSegmentData(SegmentId, DataSeries: string; S: TEDISegment);
    procedure CheckTransactionSet(T: TEDITransactionSet);
    procedure CreateTransactionData(T: TEDITransactionSet);
    procedure CheckFunctionalGroup(F: TEDIFunctionalGroup);
    procedure CreateFunctionalGroupData(F: TEDIFunctionalGroup);
    procedure CheckInterchange(I: TEDIInterchangeControl);
  protected
    procedure Setup; override;
    procedure TearDown; override;
	published
    procedure TEDISegment_Disassemble;
    procedure TEDISegment_Assemble;
    procedure TEDITransactionSet_Disassemble;
    procedure TEDITransactionSet_Assemble;
    procedure TEDIFunctionalGroup_Disassemble;
    procedure TEDIFunctionalGroup_Assemble;
    procedure TEDIInterchangeControl_Disassemble;
    procedure TEDIInterchangeControl_Assemble;
    procedure TEDIFile_Disassemble_001;
    procedure TEDIFile_Disassemble_002;
    procedure TEDIFile_Disassemble_003;
    procedure TEDIFile_Disassemble_004;
    procedure TEDIFile_Disassemble_005;
    procedure TEDIFile_Integrity_Check_001;
    procedure TEDIFile_Integrity_Check_002;
  end;

implementation

const
  TestData001File = 'TestData001.txt';

  TestData_Segment_001 = 'S1*Data1a*Data2a*Data3a*Data4a*Data5a~';
  TestData_Segment_002 = 'S2*Data1b*Data2b*Data3b*Data4b*Data5b~';
  TestData_Segment_003 = 'S3*Data1c*Data2c*Data3c*Data4c*Data5c~';

  TestData_Transaction_001 =
      'ST*DataA*DataB~' +
      TestData_Segment_001 + TestData_Segment_002 + TestData_Segment_003 +
      'SE*DataC*DataD~';

  TestData_FunctionialGroup_001 =
      'GS*DataA*DataB*DataC*DataD*DataE*DataF*DataG*DataH~' +
      TestData_Transaction_001 +
      TestData_Transaction_001 +
      TestData_Transaction_001 +
      'GE*DataI*DataJ~';

  TestData_InterchangeControl_001 =
      'ISA*DataA*DataB*DataC*DataD*DataE*DataF*DataG*DataH*' +
      'DataI*DataJ*DataK*DataL*DataM*DataN*DataO*>~' +
      TestData_FunctionialGroup_001 +
      TestData_FunctionialGroup_001 +
      TestData_FunctionialGroup_001 +
      'IEA*DataQ*DataR~';

  TestData_File_001 =
    TestData_InterchangeControl_001 +
    TestData_InterchangeControl_001 +
    TestData_InterchangeControl_001;

{ TJclEDI_ANSIX12_Tests }

procedure TJclEDI_ANSIX12_Tests.CheckFunctionalGroup(F: TEDIFunctionalGroup);
begin
  Check(F.SegmentGS.SegmentId = 'GS', 'Parse segment ST failure');
  Check(F.SegmentGS.Element[0].Data = 'DataA',
    'Parse element 1 failure in GS');
  Check(F.SegmentGS.Element[1].Data = 'DataB',
    'Parse element 2 failure in GS');
  Check(F.SegmentGS.Element[2].Data = 'DataC',
    'Parse element 3 failure in GS');
  Check(F.SegmentGS.Element[3].Data = 'DataD',
    'Parse element 4 failure in GS');
  Check(F.SegmentGS.Element[4].Data = 'DataE',
    'Parse element 5 failure in GS');
  Check(F.SegmentGS.Element[5].Data = 'DataF',
    'Parse element 6 failure in GS');
  Check(F.SegmentGS.Element[6].Data = 'DataG',
    'Parse element 7 failure in GS');
  Check(F.SegmentGS.Element[7].Data = 'DataH',
    'Parse element 8 failure in GS');
  CheckTransactionSet(F[0]);
  CheckTransactionSet(F[1]);
  CheckTransactionSet(F[2]);
  Check(F.SegmentGE.SegmentId = 'GE', 'Parse segment GE failure');
  Check(F.SegmentGE.Element[0].Data = 'DataI',
    'Parse element 1 failure in GE');
  Check(F.SegmentGE.Element[1].Data = 'DataJ',
    'Parse element 2 failure in GE');
end;

procedure TJclEDI_ANSIX12_Tests.CheckInterchange(
  I: TEDIInterchangeControl);
begin
  Check(I.SegmentISA.SegmentId = 'ISA', 'Parse segment ISA failure');
  Check(I.SegmentISA.Element[0].Data = 'DataA',
    'Parse element 1 failure in ISA');
  Check(I.SegmentISA.Element[1].Data = 'DataB',
    'Parse element 2 failure in ISA');
  Check(I.SegmentISA.Element[2].Data = 'DataC',
    'Parse element 3 failure in ISA');
  Check(I.SegmentISA.Element[3].Data = 'DataD',
    'Parse element 4 failure in ISA');
  Check(I.SegmentISA.Element[4].Data = 'DataE',
    'Parse element 5 failure in ISA');
  Check(I.SegmentISA.Element[5].Data = 'DataF',
    'Parse element 6 failure in ISA');
  Check(I.SegmentISA.Element[6].Data = 'DataG',
    'Parse element 7 failure in ISA');
  Check(I.SegmentISA.Element[7].Data = 'DataH',
    'Parse element 8 failure in ISA');
  Check(I.SegmentISA.Element[8].Data = 'DataI',
    'Parse element 9 failure in ISA');
  Check(I.SegmentISA.Element[9].Data = 'DataJ',
    'Parse element 10 failure in ISA');
  Check(I.SegmentISA.Element[10].Data = 'DataK',
    'Parse element 11 failure in ISA');
  Check(I.SegmentISA.Element[11].Data = 'DataL',
    'Parse element 12 failure in ISA');
  Check(I.SegmentISA.Element[12].Data = 'DataM',
    'Parse element 13 failure in ISA');
  Check(I.SegmentISA.Element[13].Data = 'DataN',
    'Parse element 14 failure in ISA');
  Check(I.SegmentISA.Element[14].Data = 'DataO',
    'Parse element 15 failure in ISA');
  Check(I.SegmentISA.Element[15].Data = '>',
    'Parse element 16 failure in ISA');

  CheckFunctionalGroup(I[0]);
  CheckFunctionalGroup(I[1]);
  CheckFunctionalGroup(I[2]);

  Check(I.SegmentIEA.SegmentId = 'IEA', 'Parse segment IEA failure');
  Check(I.SegmentIEA.Element[0].Data = 'DataQ',
    'Parse element 1 failure in IEA');
  Check(I.SegmentIEA.Element[1].Data = 'DataR',
    'Parse element 2 failure in IEA');
end;

procedure TJclEDI_ANSIX12_Tests.CheckSegment(SegmentId, DataSeries: string;
  S: TEDISegment);
begin
  Check(S.SegmentId = SegmentId, 'Parse segment id failure');
  Check(S.Element[0].Data = 'Data1' + DataSeries,
    'Parse element 1 failure in ' + SegmentId);
  Check(S.Element[1].Data = 'Data2' + DataSeries,
    'Parse element 2 failure in ' + SegmentId);
  Check(S.Element[2].Data = 'Data3' + DataSeries,
    'Parse element 3 failure in ' + SegmentId);
  Check(S.Element[3].Data = 'Data4' + DataSeries,
    'Parse element 4 failure in ' + SegmentId);
  Check(S.Element[4].Data = 'Data5' + DataSeries,
    'Parse element 5 failure in ' + SegmentId);
end;

procedure TJclEDI_ANSIX12_Tests.CheckTransactionSet(T: TEDITransactionSet);
begin
  Check(T.SegmentST.SegmentId = 'ST', 'Parse segment ST failure');
  Check(T.SegmentST.Element[0].Data = 'DataA',
    'Parse element 1 failure in ST');
  Check(T.SegmentST.Element[1].Data = 'DataB',
    'Parse element 2 failure in ST');
  CheckSegment('S1', 'a', T[0]);
  CheckSegment('S2', 'b', T[1]);
  CheckSegment('S3', 'c', T[2]);
  Check(T.SegmentSE.SegmentId = 'SE', 'Parse segment SE failure');
  Check(T.SegmentSE.Element[0].Data = 'DataC',
    'Parse element 1 failure in SE');
  Check(T.SegmentSE.Element[1].Data = 'DataD',
    'Parse element 2 failure in SE');
end;

procedure TJclEDI_ANSIX12_Tests.CreateFunctionalGroupData(
  F: TEDIFunctionalGroup);
begin
  with F.SegmentGS do
  begin
    SegmentId := 'GS';
    AddElements(8);
    Elements[0].Data := 'DataA';
    Elements[1].Data := 'DataB';
    Elements[2].Data := 'DataC';
    Elements[3].Data := 'DataD';
    Elements[4].Data := 'DataE';
    Elements[5].Data := 'DataF';
    Elements[6].Data := 'DataG';
    Elements[7].Data := 'DataH';
  end;
  F.AddTransactionSets(3);
  CreateTransactionData(F[0]);
  CreateTransactionData(F[1]);
  CreateTransactionData(F[2]);
  with F.SegmentGE do
  begin
    SegmentId := 'GE';
    AddElements(2);
    Elements[0].Data := 'DataI';
    Elements[1].Data := 'DataJ';
  end;
end;

procedure TJclEDI_ANSIX12_Tests.CreateSegmentData(SegmentId, DataSeries: string;
  S: TEDISegment);
begin
  S.SegmentId := SegmentId;
  S.AddElements(5);
  S.Elements[0].Data := 'Data1' + DataSeries;
  S.Elements[1].Data := 'Data2' + DataSeries;
  S.Elements[2].Data := 'Data3' + DataSeries;
  S.Elements[3].Data := 'Data4' + DataSeries;
  S.Elements[4].Data := 'Data5' + DataSeries;
end;

procedure TJclEDI_ANSIX12_Tests.CreateTransactionData(
  T: TEDITransactionSet);
begin
  with T.SegmentST do
  begin
    SegmentId := 'ST';
    AddElements(2);
    Elements[0].Data := 'DataA';
    Elements[1].Data := 'DataB';
  end;
  T.AddSegments(3);
  CreateSegmentData('S1', 'a', T.Segment[0]);
  CreateSegmentData('S2', 'b', T.Segment[1]);
  CreateSegmentData('S3', 'c', T.Segment[2]);
  with T.SegmentSE do
  begin
    SegmentId := 'SE';
    AddElements(2);
    Elements[0].Data := 'DataC';
    Elements[1].Data := 'DataD';
  end;
end;

procedure TJclEDI_ANSIX12_Tests.Setup;
begin
  inherited;
  FTestData := TStringList.Create;
  ForceDirectories('c:\Temp');
  if FileExists(TestData001File) then
    FTestData.LoadFromFile(TestData001File);
end;

procedure TJclEDI_ANSIX12_Tests.TearDown;
begin
  FTestData.Free;
  inherited;
end;

procedure TJclEDI_ANSIX12_Tests.TEDIFile_Integrity_Check_001;
const
  FileName1 = 'c:\temp\TEDIFile_Integrity_Check_001a.edi';
  FileName2 = 'c:\temp\TEDIFile_Integrity_Check_001b.edi';
var
  F1, F2: TEDIFile;
begin
  F1 := TEDIFile.Create(nil);
  F2 := TEDIFile.Create(nil);
  try
    //Prepare data
    F1.Data := TestData_File_001;
    F1.SaveAsToFile(FileName1);
    //Continue
    F1.LoadFromFile(FileName1);
    F1.Disassemble;
    F1.Assemble;
    F1.SaveAsToFile(FileName2);
    F1.LoadFromFile(FileName1);
    F1.Disassemble;
    F2.LoadFromFile(FileName2);
    F2.Disassemble;
    Check(F1.Assemble = F2.Assemble, 'EDI file integrity corrupted!');
  finally
    if FileExists(FileName1) then
      DeleteFile(FileName1);
    if FileExists(FileName2) then
      DeleteFile(FileName2);
    F1.Free;
    F2.Free;
  end;
end;

procedure TJclEDI_ANSIX12_Tests.TEDIFile_Disassemble_001;
var
  F: TEDIFile;
begin
  F := TEDIFile.Create(nil);
  try
    F.Data := TestData_File_001;
    F.Disassemble;
    CheckInterchange(F[0]);
    CheckInterchange(F[1]);
    CheckInterchange(F[2]);
  finally
    F.Free;
  end;
end;

procedure TJclEDI_ANSIX12_Tests.TEDIFile_Disassemble_002;
var
  F: TEDIFile;
begin
  F := TEDIFile.Create(nil);
  try
    F.Data := TestData_File_001;
    F.Options := F.Options + [foUseAltDelimiterDetection];
    F.Disassemble;
    CheckInterchange(F[0]);
    CheckInterchange(F[1]);
    CheckInterchange(F[2]);
  finally
    F.Free;
  end;
end;

procedure TJclEDI_ANSIX12_Tests.TEDIFile_Disassemble_003;
var
  F: TEDIFile;
begin
  F := TEDIFile.Create(nil);
  try
    F.Data := JclEDI.StringReplace(TestData_File_001, '~', #13#10, [rfReplaceAll]);
    F.Options := F.Options - [foRemoveCrLf, foRemoveCr, foRemoveLf];
    F.Disassemble;
    CheckInterchange(F[0]);
    CheckInterchange(F[1]);
    CheckInterchange(F[2]);
  finally
    F.Free;
  end;
end;

procedure TJclEDI_ANSIX12_Tests.TEDIFile_Disassemble_004;
var
  F: TEDIFile;
begin
  F := TEDIFile.Create(nil);
  try
    F.Data := TestData_File_001 + 'garbage at end of file';
    F.Options := F.Options + [foIgnoreGarbageAtEndOfFile];
    F.Disassemble;
    CheckInterchange(F[0]);
    CheckInterchange(F[1]);
    CheckInterchange(F[2]);
  finally
    F.Free;
  end;
end;

procedure TJclEDI_ANSIX12_Tests.TEDIFile_Disassemble_005;
var
  F: TEDIFile;
  Data2, Data3: string;
begin
  F := TEDIFile.Create(nil);
  try
    Data2 := JclEDI.StringReplace(TestData_File_001, '*', '+', [rfReplaceAll]);
    Data2 := JclEDI.StringReplace(TestData_File_001, '~', '^', [rfReplaceAll]);
    Data2 := JclEDI.StringReplace(TestData_File_001, '>', ':', [rfReplaceAll]);
    Data3 := JclEDI.StringReplace(TestData_File_001, '*', '.', [rfReplaceAll]);
    Data3 := JclEDI.StringReplace(TestData_File_001, '~', '|', [rfReplaceAll]);
    Data3 := JclEDI.StringReplace(TestData_File_001, '>', '-', [rfReplaceAll]);
    F.Data := TestData_File_001 + Data2 + Data3;
    F.Disassemble;
    CheckInterchange(F[0]);
    CheckInterchange(F[1]);
    CheckInterchange(F[2]);
  finally
    F.Free;
  end;
end;

procedure TJclEDI_ANSIX12_Tests.TEDIFunctionalGroup_Assemble;
var
  G: TEDIFunctionalGroup;
  Result: string;
begin
  G := TEDIFunctionalGroup.Create(nil, 3);
  try
    G.Delimiters := TEDIDelimiters.Create;
    with G.SegmentGS do
    begin
      SegmentId := 'GS';
      AddElements(8);
      Elements[0].Data := 'DataA';
      Elements[1].Data := 'DataB';
      Elements[2].Data := 'DataC';
      Elements[3].Data := 'DataD';
      Elements[4].Data := 'DataE';
      Elements[5].Data := 'DataF';
      Elements[6].Data := 'DataG';
      Elements[7].Data := 'DataH';
    end;
    CreateTransactionData(G[0]);
    CreateTransactionData(G[1]);
    CreateTransactionData(G[2]);
    with G.SegmentGE do
    begin
      SegmentId := 'GE';
      AddElements(2);
      Elements[0].Data := 'DataI';
      Elements[1].Data := 'DataJ';
    end;
    Result := G.Assemble;
    Check(Result = TestData_FunctionialGroup_001, 'Functional Group assemble failure');
  finally
    G.Free;
  end;
end;

procedure TJclEDI_ANSIX12_Tests.TEDIFunctionalGroup_Disassemble;
var
  G: TEDIFunctionalGroup;
begin
  G := TEDIFunctionalGroup.Create(nil);
  try
    G.Delimiters := TEDIDelimiters.Create;
    G.Data := TestData_FunctionialGroup_001;
    G.Disassemble;

    Check(G.SegmentGS.SegmentId = 'GS', 'Parse segment GS failure');
    Check(G.SegmentGS.Element[0].Data = 'DataA',
      'Parse element 1 failure in GS');
    Check(G.SegmentGS.Element[1].Data = 'DataB',
      'Parse element 2 failure in GS');
    Check(G.SegmentGS.Element[2].Data = 'DataC',
      'Parse element 3 failure in GS');
    Check(G.SegmentGS.Element[3].Data = 'DataD',
      'Parse element 4 failure in GS');
    Check(G.SegmentGS.Element[4].Data = 'DataE',
      'Parse element 5 failure in GS');
    Check(G.SegmentGS.Element[5].Data = 'DataF',
      'Parse element 6 failure in GS');
    Check(G.SegmentGS.Element[6].Data = 'DataG',
      'Parse element 7 failure in GS');
    Check(G.SegmentGS.Element[7].Data = 'DataH',
      'Parse element 8 failure in GS');

    CheckTransactionSet(G[0]);
    CheckTransactionSet(G[1]);
    CheckTransactionSet(G[2]);

    Check(G.SegmentGE.SegmentId = 'GE', 'Parse segment GE failure');
    Check(G.SegmentGE.Element[0].Data = 'DataI',
      'Parse element 1 failure in GE');
    Check(G.SegmentGE.Element[1].Data = 'DataJ',
      'Parse element 2 failure in GE');
  finally
    G.Free;
  end;
end;

procedure TJclEDI_ANSIX12_Tests.TEDIInterchangeControl_Assemble;
var
  I: TEDIInterchangeControl;
  Result: string;
begin
  I := TEDIInterchangeControl.Create(nil);
  try
    I.Delimiters := TEDIDelimiters.Create;
    with I.SegmentISA do
    begin
      SegmentId := 'ISA';
      AddElements(16);
      Elements[0].Data := 'DataA';
      Elements[1].Data := 'DataB';
      Elements[2].Data := 'DataC';
      Elements[3].Data := 'DataD';
      Elements[4].Data := 'DataE';
      Elements[5].Data := 'DataF';
      Elements[6].Data := 'DataG';
      Elements[7].Data := 'DataH';
      Elements[8].Data := 'DataI';
      Elements[9].Data := 'DataJ';
      Elements[10].Data := 'DataK';
      Elements[11].Data := 'DataL';
      Elements[12].Data := 'DataM';
      Elements[13].Data := 'DataN';
      Elements[14].Data := 'DataO';
      Elements[15].Data := '>';
    end;
    I.AddFunctionalGroups(3);
    CreateFunctionalGroupData(I[0]);
    CreateFunctionalGroupData(I[1]);
    CreateFunctionalGroupData(I[2]);
    with I.SegmentIEA do
    begin
      SegmentId := 'IEA';
      AddElements(2);
      Elements[0].Data := 'DataQ';
      Elements[1].Data := 'DataR';
    end;
    Result := I.Assemble;
    Check(Result = TestData_InterchangeControl_001, 'Interchange assemble failure');
  finally
    I.Free;
  end;
end;

procedure TJclEDI_ANSIX12_Tests.TEDIInterchangeControl_Disassemble;
var
  I: TEDIInterchangeControl;
begin
  I := TEDIInterchangeControl.Create(nil);
  try
    I.Delimiters := TEDIDelimiters.Create;
    I.Data := TestData_InterchangeControl_001;
    I.Disassemble;

    Check(I.SegmentISA.SegmentId = 'ISA', 'Parse segment ISA failure');
    Check(I.SegmentISA.Element[0].Data = 'DataA',
      'Parse element 1 failure in ISA');
    Check(I.SegmentISA.Element[1].Data = 'DataB',
      'Parse element 2 failure in ISA');
    Check(I.SegmentISA.Element[2].Data = 'DataC',
      'Parse element 3 failure in ISA');
    Check(I.SegmentISA.Element[3].Data = 'DataD',
      'Parse element 4 failure in ISA');
    Check(I.SegmentISA.Element[4].Data = 'DataE',
      'Parse element 5 failure in ISA');
    Check(I.SegmentISA.Element[5].Data = 'DataF',
      'Parse element 6 failure in ISA');
    Check(I.SegmentISA.Element[6].Data = 'DataG',
      'Parse element 7 failure in ISA');
    Check(I.SegmentISA.Element[7].Data = 'DataH',
      'Parse element 8 failure in ISA');
    Check(I.SegmentISA.Element[8].Data = 'DataI',
      'Parse element 9 failure in ISA');
    Check(I.SegmentISA.Element[9].Data = 'DataJ',
      'Parse element 10 failure in ISA');
    Check(I.SegmentISA.Element[10].Data = 'DataK',
      'Parse element 11 failure in ISA');
    Check(I.SegmentISA.Element[11].Data = 'DataL',
      'Parse element 12 failure in ISA');
    Check(I.SegmentISA.Element[12].Data = 'DataM',
      'Parse element 13 failure in ISA');
    Check(I.SegmentISA.Element[13].Data = 'DataN',
      'Parse element 14 failure in ISA');
    Check(I.SegmentISA.Element[14].Data = 'DataO',
      'Parse element 15 failure in ISA');
    Check(I.SegmentISA.Element[15].Data = '>',
      'Parse element 16 failure in ISA');

    CheckFunctionalGroup(I[0]);
    CheckFunctionalGroup(I[1]);
    CheckFunctionalGroup(I[2]);

    Check(I.SegmentIEA.SegmentId = 'IEA', 'Parse segment IEA failure');
    Check(I.SegmentIEA.Element[0].Data = 'DataQ',
      'Parse element 1 failure in IEA');
    Check(I.SegmentIEA.Element[1].Data = 'DataR',
      'Parse element 2 failure in IEA');
  finally
    I.Free;
  end;
end;

procedure TJclEDI_ANSIX12_Tests.TEDISegment_Assemble;
var
  S: TEDISegment;
  Result: string;
begin
  S := TEDISegment.Create(nil, 5);
  try
    S.Delimiters := TEDIDelimiters.Create;
    S.SegmentId := 'S1';
    S[0].Data := 'Data1';
    S[1].Data := 'Data2';
    S[2].Data := 'Data3';
    S[3].Data := 'Data4';
    S[4].Data := 'Data5';
    Result := S.Assemble;
    Check(Result = 'S1*Data1*Data2*Data3*Data4*Data5~', 'Assemble failure');
  finally
    S.Free;
  end;
end;

procedure TJclEDI_ANSIX12_Tests.TEDISegment_Disassemble;
var
  S: TEDISegment;
begin
  S := TEDISegment.Create(nil);
  try
    S.Delimiters := TEDIDelimiters.Create;
    S.Data := 'S1*Data1*Data2*Data3*Data4*Data5~';
    S.Disassemble;
    Check(S.ElementCount = 5, 'Disassemble to proper number of elements failure');
    Check(S.SegmentId = 'S1', 'Parse Segment Id failure');
    Check(S.Element[0].Data = 'Data1', 'Parse element 1 failure');
    Check(S.Element[1].Data = 'Data2', 'Parse element 2 failure');
    Check(S.Element[2].Data = 'Data3', 'Parse element 3 failure');
    Check(S.Element[3].Data = 'Data4', 'Parse element 4 failure');
    Check(S.Element[4].Data = 'Data5', 'Parse element 5 failure');
  finally
    S.Free;
  end;
end;

procedure TJclEDI_ANSIX12_Tests.TEDITransactionSet_Assemble;
var
  T: TEDITransactionSet;
  Result: string;
begin
  T := TEDITransactionSet.Create(nil);
  try
    T.Delimiters := TEDIDelimiters.Create;
    with T.SegmentST do
    begin
      SegmentId := 'ST';
      AddElements(2);
      Elements[0].Data := 'DataA';
      Elements[1].Data := 'DataB';
    end;
    T.AddSegments(3);
    CreateSegmentData('S1', 'a', T.Segment[0]);
    CreateSegmentData('S2', 'b', T.Segment[1]);
    CreateSegmentData('S3', 'c', T.Segment[2]);
    with T.SegmentSE do
    begin
      SegmentId := 'SE';
      AddElements(2);
      Elements[0].Data := 'DataC';
      Elements[1].Data := 'DataD';
    end;
    Result := T.Assemble;
    Check(Result = TestData_Transaction_001, 'Transaction Set assemble failure');
  finally
    T.Free;
  end;
end;

procedure TJclEDI_ANSIX12_Tests.TEDITransactionSet_Disassemble;
var
  T: TEDITransactionSet;
begin
  T := TEDITransactionSet.Create(nil);
  try
    T.Delimiters := TEDIDelimiters.Create;
    T.Data := TestData_Transaction_001;
    T.Disassemble;
    Check(T.SegmentCount = 3, 'Disassemble to proper number of segments failure');

    Check(T.SegmentST.SegmentId = 'ST', 'Parse segment ST failure');
    Check(T.SegmentST.Element[0].Data = 'DataA',
      'Parse element 1 failure in ST');
    Check(T.SegmentST.Element[1].Data = 'DataB',
      'Parse element 2 failure in ST');

    CheckSegment('S1', 'a', T[0]);
    CheckSegment('S2', 'b', T[1]);
    CheckSegment('S3', 'c', T[2]);

    Check(T.SegmentSE.SegmentId = 'SE', 'Parse segment SE failure');
    Check(T.SegmentSE.Element[0].Data = 'DataC',
      'Parse element 1 failure in SE');
    Check(T.SegmentSE.Element[1].Data = 'DataD',
      'Parse element 2 failure in SE');
  finally
    T.Free;
  end;
end;

procedure TJclEDI_ANSIX12_Tests.TEDIFile_Integrity_Check_002;
const
  FileName1 = 'c:\temp\TEDIFile_Integrity_Check_001a.edi';
  FileName2 = 'c:\temp\TEDIFile_Integrity_Check_001b.edi';
var
  F1, F2: TEDIFile;
  Data2, Data3: string;
begin
  F1 := TEDIFile.Create(nil);
  F2 := TEDIFile.Create(nil);
  try
    //Prepare data
    Data2 := JclEDI.StringReplace(TestData_File_001, '*', '+', [rfReplaceAll]);
    Data2 := JclEDI.StringReplace(TestData_File_001, '~', '^', [rfReplaceAll]);
    Data2 := JclEDI.StringReplace(TestData_File_001, '>', ':', [rfReplaceAll]);
    Data3 := JclEDI.StringReplace(TestData_File_001, '*', '.', [rfReplaceAll]);
    Data3 := JclEDI.StringReplace(TestData_File_001, '~', '|', [rfReplaceAll]);
    Data3 := JclEDI.StringReplace(TestData_File_001, '>', '-', [rfReplaceAll]);
    F1.Data := TestData_File_001 + Data2 + Data3;
    F1.SaveAsToFile(FileName1);
    //Continue
    F1.LoadFromFile(FileName1);
    F1.Disassemble;
    F1.Assemble;
    F1.SaveAsToFile(FileName2);
    F1.LoadFromFile(FileName1);
    F1.Disassemble;
    F2.LoadFromFile(FileName2);
    F2.Disassemble;
    Check(F1.Assemble = F2.Assemble, 'EDI file integrity corrupted!');
  finally
    if FileExists(FileName1) then
      DeleteFile(FileName1);
    if FileExists(FileName2) then
      DeleteFile(FileName2);
    F1.Free;
    F2.Free;
  end;
end;

initialization
  RegisterTest('JclEDI_ANSIX12', TJclEDI_ANSIX12_Tests.Suite);

end.
