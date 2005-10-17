program SingleInstExample;

uses
  JclAppInst, // Added JclAppInst unit
  Forms,
  SingleInstDemoMain in 'SingleInstDemoMain.pas' {Form1};

{$R *.RES}

begin
  JclAppInstances.CheckSingleInstance; // Added instance checking
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
