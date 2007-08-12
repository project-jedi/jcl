unit EDICOMExampleMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, OleServer, EDISDK_TLB;

type
  TForm1 = class(TForm)
    F: TEDICOMFile;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
  I: Integer;
begin
  F.Options := 0;
  F.Options := F.Options or foVariableDelimiterDetection;
  F.Options := F.Options or foUseAltDelimiterDetection;
  F.Options := F.Options or foRemoveCrLf;
  F.Options := F.Options or foRemoveCr;
  F.Options := F.Options or foRemoveLf;
  F.Options := F.Options or foIgnoreGarbageAtEndOfFile;
  F.LoadFromFile(ExtractFileDir(Application.ExeName) + '\sample.edi');
  Memo1.Lines.Add( F.Data );
  F.Disassemble;
  Memo1.Lines.Add(F.Interchange[0].SegmentISA.SegmentId);
  Memo1.Lines.Add(F.Interchange[0].FunctionalGroup[0].SegmentGS.SegmentId);
  Memo1.Lines.Add(F.Interchange[0].FunctionalGroup[0].TransactionSet[0].SegmentST.SegmentId);
  for I := 0 to F.Interchange[0].FunctionalGroup[0].TransactionSet[0].SegmentCount - 1 do
  begin
    F.Interchange[0].FunctionalGroup[0].TransactionSet[0].Segment[I].Assemble;
    Memo1.Lines.Add( F.Interchange[0].FunctionalGroup[0].TransactionSet[0].Segment[I].Data );
  end;
  Memo1.Lines.Add(F.Interchange[0].FunctionalGroup[0].TransactionSet[0].Segment[0].Data);
  Memo1.Lines.Add(F.Interchange[0].FunctionalGroup[0].TransactionSet[0].SegmentSE.SegmentId);
  Memo1.Lines.Add(F.Interchange[0].FunctionalGroup[0].SegmentGE.SegmentId);
  Memo1.Lines.Add(F.Interchange[0].SegmentIEA.SegmentId);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  C: IEDICOMFile;
  I, F, T, S: Integer;
begin
  Memo1.Lines.Clear;

  C := CoEDICOMFile.Create;
  I := C.AddInterchange;
  C.Interchange[I].SetDelimiters('~' + #13#10, '*', '>');
  with C.Interchange[I].SegmentISA do
  begin
    SegmentId := 'ISA';
    DeleteElements;
    AddElements(17);
    Element[0].Data := 'data';
    Element[1].Data := 'data';
    Element[2].Data := 'data';
    Element[3].Data := 'data';
    Element[4].Data := 'data';
    Element[5].Data := 'data';
    Element[6].Data := 'data';
    Element[7].Data := 'data';
    Element[8].Data := 'data';
    Element[9].Data := 'data';
    Element[10].Data := 'data';
    Element[11].Data := 'data';
    Element[12].Data := 'data';
    Element[13].Data := 'data';
    Element[14].Data := 'data';
    Element[15].Data := 'data';
    Element[16].Data := C.Interchange[I].Delimiters.SS;
  end;

  F := C.Interchange[I].AddFunctionalGroup;
  with C.Interchange[I].FunctionalGroup[F].SegmentGS do
  begin
    SegmentId := 'GS';
    DeleteElements;
    AddElements(8);
    Element[0].Data := 'data';
    Element[1].Data := 'data';
    Element[2].Data := 'data';
    Element[3].Data := 'data';
    Element[4].Data := 'data';
    Element[5].Data := 'data';
    Element[6].Data := 'data';
    Element[7].Data := 'data';
  end;
  
  T := C.Interchange[I].FunctionalGroup[F].AddTransactionSet;
  with C.Interchange[I].FunctionalGroup[F].TransactionSet[T].SegmentST do
  begin
    SegmentId := 'ST';
    DeleteElements;
    AddElements(2);
    Element[0].Data := 'data';
    Element[1].Data := 'data';
  end;
  
  S := C.Interchange[I].FunctionalGroup[F].TransactionSet[T].AddSegment;
  with C.Interchange[I].FunctionalGroup[F].TransactionSet[T].Segment[S] do
  begin
    SegmentId := 'TST';
    AddElements(2);
    Element[0].Data := 'data 1';
    Element[1].Data := 'data 2';
  end;

  with C.Interchange[I].FunctionalGroup[F].TransactionSet[T].SegmentSE do
  begin
    SegmentId := 'SE';
    DeleteElements;
    AddElements(2);
    Element[0].Data := 'data';
    Element[1].Data := 'data';
  end;

  with C.Interchange[I].FunctionalGroup[F].SegmentGE do
  begin
    SegmentId := 'GE';
    DeleteElements;
    AddElements(2);
    Element[0].Data := 'data';
    Element[1].Data := 'data';
  end;

  with C.Interchange[I].SegmentIEA do
  begin
    SegmentId := 'IEA';
    DeleteElements;
    AddElements(2);
    Element[0].Data := 'data';
    Element[1].Data := 'data';
  end;

  Memo1.Lines.Add( C.Assemble );
end;

end.
