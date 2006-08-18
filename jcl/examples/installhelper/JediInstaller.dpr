program JediInstaller;

uses
  Forms,
  JediInstallerMain in 'JediInstallerMain.pas' {MainForm},
  DelphiInstall in 'DelphiInstall.pas',
  JediInstallIntf in 'JediInstallIntf.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JEDI Installer';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
