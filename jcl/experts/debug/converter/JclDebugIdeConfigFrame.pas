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
{ The Original Code is JclDebugIdeConfigFrame.pas.                                                 }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet.                                    }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet.                          }
{                                                                                                  }
{ Contributors:                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclDebugIdeConfigFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls;

type
  TJclDebugIdeConfigFrame = class(TFrame)
    CheckBoxGenerateJdbg: TCheckBox;
    CheckBoxInsertJdbg: TCheckBox;
    CheckBoxEnableExpert: TCheckBox;
    procedure CheckBoxEnableExpertClick(Sender: TObject);
  private
    function GetEnableExpert: Boolean;
    function GetGenerateJdbg: Boolean;
    function GetInsertJdbg: Boolean;
    procedure SetEnableExpert(const Value: Boolean);
    procedure SetGenerateJdbg(const Value: Boolean);
    procedure SetInsertJdbg(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;

    property EnableExpert: Boolean read GetEnableExpert write SetEnableExpert;
    property GenerateJdbg: Boolean read GetGenerateJdbg write SetGenerateJdbg;
    property InsertJdbg: Boolean read GetInsertJdbg write SetInsertJdbg;
  end;

implementation

{$R *.dfm}

uses
  JclOtaResources;

procedure TJclDebugIdeConfigFrame.CheckBoxEnableExpertClick(Sender: TObject);
begin
  CheckBoxGenerateJdbg.Enabled := CheckBoxEnableExpert.Checked;
  CheckBoxInsertJdbg.Enabled := CheckBoxEnableExpert.Checked;
end;

constructor TJclDebugIdeConfigFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CheckBoxEnableExpert.Caption := RsDebugEnableExpert;
  CheckBoxGenerateJdbg.Caption := RsDebugGenerateJdbg;
  CheckBoxInsertJdbg.Caption := RsDebugInsertJdbg;
end;

function TJclDebugIdeConfigFrame.GetEnableExpert: Boolean;
begin
  Result := CheckBoxEnableExpert.Checked;
end;

function TJclDebugIdeConfigFrame.GetGenerateJdbg: Boolean;
begin
  Result := CheckBoxGenerateJdbg.Checked;
end;

function TJclDebugIdeConfigFrame.GetInsertJdbg: Boolean;
begin
  Result := CheckBoxInsertJdbg.Checked;
end;

procedure TJclDebugIdeConfigFrame.SetEnableExpert(const Value: Boolean);
begin
  CheckBoxEnableExpert.Checked := Value;
  CheckBoxGenerateJdbg.Enabled := Value;
  CheckBoxInsertJdbg.Enabled := Value;
end;

procedure TJclDebugIdeConfigFrame.SetGenerateJdbg(const Value: Boolean);
begin
  CheckBoxGenerateJdbg.Checked := Value;
end;

procedure TJclDebugIdeConfigFrame.SetInsertJdbg(const Value: Boolean);
begin
  CheckBoxInsertJdbg.Checked := Value;
end;

end.
