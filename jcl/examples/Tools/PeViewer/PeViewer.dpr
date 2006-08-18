program PeViewer;

uses
  Forms,
  SysUtils,
  PeViewerMain in 'PeViewerMain.pas' {MainForm},
  PeDump in 'PeDump.pas' {PeDumpChild},
  PeSearch in 'PeSearch.pas' {PeSearchChild},
  PeViewer_TLB in 'PeViewer_TLB.pas',
  PeViewerControl in 'PeViewerControl.pas' {PeViewerControl: CoClass},
  PeResource in 'PeResource.pas',
  PeResView in 'PeResView.pas' {PeResViewChild},
  ToolsUtils in '..\Common\ToolsUtils.pas',
  About in '..\Common\About.pas' {AboutBox},
  PeGenDef in 'PeGenDef.pas' {PeGenDefChild},
  FindDlg in '..\Common\FindDlg.pas' {FindTextForm},
  ExceptionDlg in '..\Common\ExceptionDlg.pas' {ExceptionDialog};

{$R *.TLB}

{$R *.RES}

begin
  try
    Application.Initialize;
    Application.Title := 'PE Viewer';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
  except // fix for Delphi 5's RTL bug
    SysUtils.ShowException(ExceptObject, ExceptAddr);
  end;
end.
