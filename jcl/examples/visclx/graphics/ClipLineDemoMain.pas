unit ClipLineDemoMain;

{$I jedi.inc}

interface

uses
  SysUtils, Types, Classes,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF VisualCLX}
  Qt, QGraphics, QControls, QForms, QStdCtrls, QExtCtrls, QTypes, JclQGraphUtils,
{$ENDIF VisualCLX}
{$IFDEF VCL}
  Graphics, ExtCtrls, Forms, JclGraphUtils,
{$ENDIF VCL}
  JclBase;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }
    R: TRect;
    P: TPointArray;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$IFDEF VisualCLX}
{$R *.xfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  //Randomize;
  R.Left := 100;
  R.Top := 100;
  R.Right := 300;
  R.Bottom := 300;
  SetLength(P, 50);
  for i := 0 to Length(P)-1 do
  begin
    P[i].X := Random(Width);
    P[i].Y := Random(Height);
  end;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  Canvas.Brush.Color := clWhite;
  Canvas.FillRect(R);
  Canvas.Pen.Color := $FFC0C0;
  Canvas.PolyLine(P);
  Canvas.Pen.Color := clBlue;
  DrawPolyLine(Canvas, P, R);
end;

end.

