program StackTrackDLLsExample;

{%File 'makefile.mak'}

uses
  Forms,
  StackTrackDLLsDemoMain in 'StackTrackDLLsDemoMain.pas' {MainForm},
  ExceptDlg in '..\..\debugextension\dialog\ExceptDlg.pas' {ExceptionDialog};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
