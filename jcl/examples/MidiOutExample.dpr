program MidiOutExample;

uses
  Forms,
  JclMIDI in '..\source\JclMIDI.pas',
  MidiOutExampleMain in 'MidiOutExampleMain.pas' {Keyboard},
  MidiOutExampleTuningDlg in 'MidiOutExampleTuningDlg.pas' {TuningDialog};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TKeyboard, Keyboard);
  Application.CreateForm(TTuningDialog, TuningDialog);
  Application.Run;
end.
