program StructStorageDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  PropsFrm in 'PropsFrm.pas' {frmProps};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Compound Document Editor';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
