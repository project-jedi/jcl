program QRTTIDemo;

uses
  QForms,
  QRTTIDemoMain in 'QRTTIDemoMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
