program JclInstallHelper;

uses
  Forms,
  JclInstallHelperMain in 'JclInstallHelperMain.pas' {MainForm},
  DelphiInstall in 'DelphiInstall.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JCL Installation';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
