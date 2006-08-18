unit StackTrackDemoMain;

interface

{.$DEFINE ShowAllExceptions}

// If ShowAllExceptions is defined then all exceptions (including those which
// are not handled by TApplication object will be logged.
// Otherwise, only exceptions handled by TApplication object will be logged.

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Db, DBTables;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ListBox1: TListBox;
    RawCheckBox: TCheckBox;
    Table1: TTable;
    Button4: TButton;
    Button5: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure RawCheckBoxClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    procedure ApplicationEvents1Exception(Sender: TObject; E: Exception);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  {$IFDEF ShowAllExceptions}
  JclHookExcept,
  {$ENDIF}
  JclDebug;

procedure AnyExceptionNotify(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean);
begin
  with Form1 do
  begin
    Memo1.Lines.BeginUpdate;
    if JclLastExceptStackList <> nil then
      JclLastExceptStackList.AddToStrings(Memo1.Lines);
    Memo1.Lines.EndUpdate;
    Memo1.Lines.Add('');
  end;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
{$IFNDEF ShowAllExceptions}
  Application.OnException := ApplicationEvents1Exception;
{$ENDIF}
end;

procedure TForm1.ApplicationEvents1Exception(Sender: TObject; E: Exception);
begin
  Memo1.Lines.BeginUpdate;

  // Show exception stack info
  if JclLastExceptStackList <> nil then
    JclLastExceptStackList.AddToStrings(Memo1.Lines);

  Memo1.Lines.EndUpdate;
  Memo1.Lines.Add('');
  Application.ShowException(E);
end;

procedure TForm1.RawCheckBoxClick(Sender: TObject);
begin
  if RawCheckBox.Checked then
    Include(JclStackTrackingOptions, stRawMode)
  else
    Exclude(JclStackTrackingOptions, stRawMode)
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  PInteger(nil)^ := 0;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ListBox1.Items[1] := 'a';
end;

procedure AAA;
begin
  PInteger(nil)^ := 0;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  AAA;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Table1.Open;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  try // See ShowAllExceptions above for details
    StrToInt('abc');
  except
  end;
end;

initialization

  // Start Exception tracking
  JclStartExceptionTracking;

{$IFDEF ShowAllExceptions}
  // Assign notification procedure for hooked RaiseException API call. This
  // allows being notified of any exception
  JclAddExceptNotifier(AnyExceptionNotify);
{$ENDIF}

end.
