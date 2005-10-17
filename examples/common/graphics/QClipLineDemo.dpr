program QClipLineDemo;

{%ToDo 'ClipLineDemo.todo'}
uses
  QStyle,
  QForms,
  QClipLineDemoMain in 'QClipLineDemoMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
