program QJediInstaller;

uses
  QForms,
  JediInstall in 'JediInstall.pas',
  JclInstall in 'JclInstall.pas',
  QJediInstallerMain in 'QJediInstallerMain.pas' {MainForm},
  QProductFrames in 'QProductFrames.pas' {ProductFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JEDI Installer';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
