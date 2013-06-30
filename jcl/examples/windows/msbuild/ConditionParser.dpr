program ConditionParser;

uses
  Forms,
  ConditionParserMain in 'ConditionParserMain.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
