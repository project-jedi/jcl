program ClipLineDemo;

{%ToDo 'ClipLineDemo.todo'}
uses
  QStyle,
  QForms,
  ClipLineDemoMain in 'ClipLineDemoMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
