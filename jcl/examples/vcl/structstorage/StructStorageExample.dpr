program StructStorageExample;

uses
  Forms,
  StructStorageExampleMain in 'StructStorageExampleMain.pas' {frmMain},
  PropsFrm in 'PropsFrm.pas' {frmProps};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Compound Document Editor';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
