program PeViewer;

uses
  Forms,
  SysUtils,
  D6MdiMsgFix in '..\Common\D6MdiMsgFix.pas',
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
  ExceptDlg in '..\..\..\..\experts\debug\dialog\ExceptDlg.pas' {ExceptionDialog},
  SHDocVw_TLB in '..\Common\SHDocVw_TLB.pas';

{$R *.TLB}

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'PE Viewer';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
