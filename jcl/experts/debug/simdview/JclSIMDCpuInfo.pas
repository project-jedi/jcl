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
{ The Original Code is: JvSIMDCPUInfo.pas, released on 2005-05-09.                                 }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet                                     }
{ [ouchet dott florent att laposte dott net]                                                       }
{ Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.                        }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ You may retrieve the latest version of this file at the Project JEDI's JCL home page,            }
{ located at http://jcl.sourceforge.net                                                            }
{                                                                                                  }
{**************************************************************************************************}

// $Id$

unit JclSIMDCpuInfo;

interface

{$I jcl.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  JclSysInfo;

type
  TJclFormCpuInfo = class(TForm)
    LabelName: TLabel;
    EditName: TEdit;
    LabelVendor: TLabel;
    EditVendor: TEdit;
    LabelFrequency: TLabel;
    EditFrequency: TEdit;
    CheckBoxMMX: TCheckBox;
    CheckBoxExMMX: TCheckBox;
    CheckBox3DNow: TCheckBox;
    CheckBoxEx3DNow: TCheckBox;
    CheckBox64Bits: TCheckBox;
    CheckBoxSSE1: TCheckBox;
    CheckBoxSSE2: TCheckBox;
    CheckBoxSSE3: TCheckBox;
    ButtonClose: TButton;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    procedure Execute(const CpuInfo: TCPUInfo);
  end;

implementation

{$R *.dfm}

//=== { TJclFormCpuInfo } ====================================================

procedure TJclFormCpuInfo.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  // Fixing the Window Ghosting "bug"
  Params.Style := params.Style or WS_POPUP;
  if Assigned(Screen.ActiveForm) then
    Params.WndParent := Screen.ActiveForm.Handle
  else
  if Assigned(Application.MainForm) then
    Params.WndParent := Application.MainForm.Handle
  else
    Params.WndParent := Application.Handle;
end;

procedure TJclFormCpuInfo.Execute(const CpuInfo: TCPUInfo);
begin
  EditName.Text := CpuInfo.CpuName;
  EditVendor.Text := CpuInfo.VendorIDString;
  EditFrequency.Text := IntToStr(CpuInfo.FrequencyInfo.NormFreq);
  CheckBoxMMX.Checked := CpuInfo.MMX;
  CheckBoxExMMX.Checked := CpuInfo.ExMMX;
  CheckBox3DNow.Checked := CpuInfo._3DNow;
  CheckBoxEx3DNow.Checked := CpuInfo.Ex3DNow;
  CheckBox64Bits.Checked := CpuInfo.Is64Bits;
  CheckBoxSSE1.Checked := CpuInfo.SSE >= 1;
  CheckBoxSSE2.Checked := CpuInfo.SSE >= 2;
  CheckBoxSSE3.Checked := CpuInfo.SSE >= 3;
  ShowModal;
end;

end.
