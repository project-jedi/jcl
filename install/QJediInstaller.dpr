{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit. Manual modifications will be lost on next release.  }
{**************************************************************************************************}

program QJediInstaller;

uses
  
  QForms,
  QJclInstall in 'QJclInstall.pas',
  QJediInstallIntf in 'QJediInstallIntf.pas',
  QJediInstallerMain in 'QJediInstallerMain.pas' {MainForm},
  QProductFrames in 'QProductFrames.pas' {ProductFrame: TFrame},
  
  BorRADToolInstall in 'BorRADToolInstall.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JEDI Installer';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
