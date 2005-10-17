program NtSvcExample;

uses
  Forms,
  NtSvcDemoMain in 'NtSvcDemoMain.pas' {frmMain},
  NtSvcDemoDependent in 'NtSvcDemoDependent.pas' {frmDependent},
  NtSvcDemoGroups in 'NtSvcDemoGroups.pas' {frmServiceGroups};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
