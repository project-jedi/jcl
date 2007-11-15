{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

program QJediInstaller;

uses
  QForms,
  JediInstall in 'JediInstall.pas',
  JclInstall in 'JclInstall.pas',
  JediInstallConfigIni in 'JediInstallConfigIni.pas',
  JclResources in '../source/common/JclResources.pas',
  JclBorlandTools in '../source/common/JclBorlandTools.pas',
  QJediGUIReadme in 'ClxGui/QJediGUIReadme.pas' {ReadmeFrame: TFrame},
  QJediGUIInstall in 'ClxGui/QJediGUIInstall.pas' {InstallFrame: TFrame},
  QJediGUIMain in 'ClxGui/QJediGUIMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JEDI Installer';
  InstallCore.Execute;
end.
