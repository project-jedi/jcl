program JclBuildNumber;

uses
  Forms,
  JclBuildNumberMain in 'JclBuildNumberMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JCL Build Number generator';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
