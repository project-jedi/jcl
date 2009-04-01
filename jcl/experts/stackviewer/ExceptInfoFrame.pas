unit ExceptInfoFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, JclDebugStackUtils;

type
  TfrmException = class(TFrame)
    Label1: TLabel;
    Label2: TLabel;
    lbExceptionClassName: TLabel;
    lbExceptionMessage: TLabel;
  private
    FException: TException;
    procedure SetException(const Value: TException);
    { Private declarations }
  public
    { Public declarations }
    property Exception: TException read FException write SetException;
  end;

implementation

{$R *.dfm}

{ TfrmException }

procedure TfrmException.SetException(const Value: TException);
begin
  FException := Value;
  if Assigned(FException) then
  begin
    lbExceptionClassName.Caption := FException.ExceptionClassName;
    lbExceptionMessage.Caption := FException.ExceptionMessage;
  end
  else
  begin
    lbExceptionClassName.Caption := '';
    lbExceptionMessage.Caption := '';
  end;
end;

end.
