program {$IFDEF VisualCLX}QJediInstaller{$ELSE}JediInstaller{$ENDIF};

uses
  {$IFDEF VisualCLX}
  QForms,
  QJclInstall in 'QJclInstall.pas',
  QJediInstallIntf in 'QJediInstallIntf.pas',
  QJediInstallerMain in 'QJediInstallerMain.pas' {MainForm},
  QProductFrames in 'QProductFrames.pas' {ProductFrame: TFrame},
  {$ELSE VCL}
  Forms,
  JclInstall in 'JclInstall.pas',
  JediInstallIntf in 'JediInstallIntf.pas',
  JediInstallerMain in 'JediInstallerMain.pas' {MainForm},
  ProductFrames in 'ProductFrames.pas' {ProductFrame: TFrame},
  {$ENDIF VCL}
  JclBorlandTools in '..\source\common\JclBorlandTools.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JEDI Installer';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
