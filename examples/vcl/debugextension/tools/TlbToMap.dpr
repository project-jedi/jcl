program TlbToMap;

uses
  Forms,
  TlbToMapMain in 'TlbToMapMain.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TLB to MAP';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
