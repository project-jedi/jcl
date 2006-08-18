program DependView;

uses
  Forms,
  SysUtils,
  DependViewMain in 'DependViewMain.pas' {MainForm},
  FileViewer in 'FileViewer.pas' {FileViewerChild},
  ToolsUtils in '..\Common\ToolsUtils.pas',
  About in '..\Common\About.pas' {AboutBox},
  FindDlg in '..\Common\FindDlg.pas' {FindTextForm},
  ExceptionDlg in '..\Common\ExceptionDlg.pas' {ExceptionDialog};

{$R *.RES}

begin
  try
    Application.Initialize;
    Application.Title := 'Dependency Viewer';
    Application.CreateForm(TMainForm, MainForm);
  Application.Run;
  except // fix for Delphi 5's RTL bug
    SysUtils.ShowException(ExceptObject, ExceptAddr);
  end;
end.
