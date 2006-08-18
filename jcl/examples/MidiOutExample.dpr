program MidiOutExample;

uses
{$IFDEF COMPLIB_CLX}
  QForms,
{$ELSE}
  Forms,
{$ENDIF}
  MidiOutExampleMain in 'MidiOutExampleMain.pas' {Keyboard},
  MidiOutExampleTuningDlg in 'MidiOutExampleTuningDlg.pas' {TuningDialog};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TKeyboard, Keyboard);
  Application.CreateForm(TTuningDialog, TuningDialog);
  Application.Run;
end.
