{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is ExceptDlg.pas.                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Sample Application exception dialog replacement with sending report by the default mail client   }
{ functionality                                                                                    }
{                                                                                                  }
{ Last modified: July 29, 2002                                                                     }
{                                                                                                  }
{**************************************************************************************************}

unit ExceptDlgMail;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExceptDlg, StdCtrls, ExtCtrls, JclMapi;

type
  TExceptionDialogMail = class(TExceptionDialog)
    SendBtn: TButton;
    procedure SendBtnClick(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure AfterCreateDetails; override;
    procedure BeforeCreateDetails; override;
    function ReportMaxColumns: Integer; override;
  public
    { Public declarations }
  end;

var
  ExceptionDialogMail: TExceptionDialogMail;

implementation

{$R *.dfm}

const
  SendBugReportAddress = 'bugreport@yourdomain.com';
  SendBugReportSubject = 'Bug Report';

{ TExceptionDialogMail }

procedure TExceptionDialogMail.AfterCreateDetails;
begin
  inherited;
  SendBtn.Enabled := True;
end;

procedure TExceptionDialogMail.BeforeCreateDetails;
begin
  inherited;
  SendBtn.Enabled := False;
end;

function TExceptionDialogMail.ReportMaxColumns: Integer;
begin
  Result := 78;
end;

procedure TExceptionDialogMail.SendBtnClick(Sender: TObject);
begin
  with TJclEmail.Create do
  try
    ParentWnd := Application.Handle;
    Recipients.Add(SendBugReportAddress);
    Subject := SendBugReportSubject;
    Body := ReportAsText;
    SaveTaskWindows;
    try
      Send(True);
    finally
      RestoreTaskWindows;
    end;    
  finally
    Free;
  end;
end;

initialization
  ExceptionDialogClass := TExceptionDialogMail;

end.
