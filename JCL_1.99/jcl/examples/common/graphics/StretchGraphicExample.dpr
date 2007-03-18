program StretchGraphicExample;

uses
  Forms,
  StretchGraphicDemoMain in 'StretchGraphicDemoMain.pas' {StretchDemoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TStretchDemoForm, StretchDemoForm);
  Application.Run;
end.
