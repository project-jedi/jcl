unit CreateProcAsUserDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    edtDomain: TEdit;
    Label1: TLabel;
    edtUserName: TEdit;
    Label2: TLabel;
    edtPassword: TEdit;
    Label3: TLabel;
    edtCommandLine: TEdit;
    Label4: TLabel;
    btnCreateProcAsUser: TButton;
    btnCreateProcAsUserEx: TButton;
    lbEnvironment: TListBox;
    Label5: TLabel;
    edtEnvString: TEdit;
    btnAddEnvString: TButton;
    btnRemoveEnvString: TButton;
    btnClearEnvStrings: TButton;
    chkEnvAdditional: TCheckBox;
    chkEnvCurrentUser: TCheckBox;
    chkEnvLocalMachine: TCheckBox;
    procedure btnAddEnvStringClick(Sender: TObject);
    procedure btnClearEnvStringsClick(Sender: TObject);
    procedure btnRemoveEnvStringClick(Sender: TObject);
    procedure btnCreateProcAsUserClick(Sender: TObject);
    procedure btnCreateProcAsUserExClick(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

uses
  JclMiscel, JclSysInfo, JclStrings;

{$R *.DFM}

procedure TForm1.btnAddEnvStringClick(Sender: TObject);
begin
  lbEnvironment.Items.Add(edtEnvString.Text);
end;

procedure TForm1.btnClearEnvStringsClick(Sender: TObject);
begin
  lbEnvironment.Items.Clear;
end;

procedure TForm1.btnRemoveEnvStringClick(Sender: TObject);
var
  i: integer;
begin
  for i := Pred(lbEnvironment.Items.Count) downto 0 do
  begin
    if lbEnvironment.Selected[i] then
      lbEnvironment.Items.Delete(i);
  end;
end;

procedure TForm1.btnCreateProcAsUserClick(Sender: TObject);
begin
  CreateProcAsUser(edtDomain.Text, edtUserName.Text,
                   edtPassWord.Text, edtCommandline.Text);
end;

procedure TForm1.btnCreateProcAsUserExClick(Sender: TObject);
var
  pcharEnv: PCHAR;
  envOptions: TEnvironmentOptions;
begin
  envOptions := [];
  if chkEnvAdditional.Checked then
    envOptions := envOptions + [eoAdditional];
  if chkEnvCurrentUser.Checked then
    envOptions := envOptions + [eoCurrentUser];
  if chkEnvLocalMachine.Checked then
    envOptions := envOptions + [eoLocalMachine];

  pcharEnv := CreateEnvironmentBlock(envOptions, lbEnvironment.Items);
  CreateProcAsUserEx(edtDomain.Text, edtUserName.Text,
                     edtPassWord.Text, edtCommandline.Text, pcharEnv);
  FreeMultiSz(pcharEnv);
end;

end.
