{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit. Manual modifications will be lost on next release.  }
{**************************************************************************************************}

program JediInstaller;

uses
  
  Forms,
  JclInstall in 'JclInstall.pas',
  JediInstallIntf in 'JediInstallIntf.pas',
  JediInstallerMain in 'JediInstallerMain.pas' {MainForm},
  ProductFrames in 'ProductFrames.pas' {ProductFrame: TFrame},
  
  BorRADToolInstall in 'BorRADToolInstall.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JEDI Installer';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
