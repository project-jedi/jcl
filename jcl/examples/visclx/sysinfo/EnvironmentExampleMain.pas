unit EnvironmentExampleMain;

interface

uses
  SysUtils, Classes, QControls, QForms, QGrids, QStdCtrls, QExtCtrls,
  JclSysInfo; 

type
  TForm1 = class(TForm)
    EnvironmentGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure EnvironmentGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: WideString);
    procedure RefreshBtnClick(Sender: TObject);
  private
    { Private declarations }
    procedure GetEnvironment;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  EnvironmentGrid.Cells[0, 0] := 'Variable';
  EnvironmentGrid.Cells[1, 0] := 'Value';
  EnvironmentGrid.ColWidths[0] := 200;
  EnvironmentGrid.ColWidths[1] := 1000;
  GetEnvironment;
end;

procedure TForm1.EnvironmentGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: WideString);
var
  Key: string;
begin
  with EnvironmentGrid do
  begin
    Key := Cells[0, Row];
    SetEnvironmentVar(Key, Value);
  end;
end;

procedure TForm1.RefreshBtnClick(Sender: TObject);
begin
  GetEnvironment;
end;

procedure TForm1.GetEnvironment;
var
  I: Integer;
  Key: string;
  S: TStringList;
begin
  S := TStringList.Create;
  try
    GetEnvironmentVars(S);
    EnvironmentGrid.RowCount := S.Count + 1;
    for I := 0 to S.Count - 1 do
    begin
      Key := S.Names[I];
      EnvironmentGrid.Cells[0, I + 1] := Key;
      EnvironmentGrid.Cells[1, I + 1] := S.Values[Key];
    end;
    S.Sorted := True;
  finally
    S.Free;
  end;
end;

end.

