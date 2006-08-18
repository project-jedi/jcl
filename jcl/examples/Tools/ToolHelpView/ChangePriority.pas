{==============================================================================|
| Project : ToolHelp Viewer                                      | 000.005.000 |
|==============================================================================|
| Content:  Process change priority dialog                                     |
|==============================================================================|
| The contents of this file are subject to the Mozilla Public License Ver. 1.0 |
| (the "License"); you may not use this file except in compliance with the     |
| License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ |
|                                                                              |
| Software distributed under the License is distributed on an "AS IS" basis,   |
| WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for |
| the specific language governing rights and limitations under the License.    |
|==============================================================================|
| The Original Code is Petr Vones.                                             |
|==============================================================================|
| The Initial Developer of the Original Code is Petr Vones, petr.v@mujmail.cz  |
| Portions created by Petr Vones are Copyright (C) 1999.                       |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================}

unit ChangePriority;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TChangePriorityDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    PriorityRadioGroup: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    FProcessID: DWORD;
    procedure SetProcessID(const Value: DWORD);
  public
    property ProcessID: DWORD write SetProcessID;
  end;

var
  ChangePriorityDlg: TChangePriorityDlg;

implementation

{$R *.DFM}

uses
  ToolsUtils;

resourcestring
  sCantChange = 'Couldn''t change process priority';

{ TChangePriorityDlg }

procedure TChangePriorityDlg.SetProcessID(const Value: DWORD);
var
  Handle: THandle;
  Priority: DWORD;
  I: Integer;
begin
  FProcessID := Value;
  Handle := OpenProcess(PROCESS_ALL_ACCESS{PROCESS_QUERY_INFORMATION}, False, FProcessID);
  if Handle <> 0 then
  begin
    Priority := GetPriorityClass(Handle);
    CloseHandle(Handle);
  end else Priority := 0;
  I := PriorityRadioGroup.Items.IndexOfObject(Pointer(Priority));
  if I = -1 then I := 1;
  PriorityRadioGroup.ItemIndex := I;
end;

procedure TChangePriorityDlg.FormCreate(Sender: TObject);
begin
  with PriorityRadioGroup.Items do
  begin
    BeginUpdate;
    AddObject('&Idle', Pointer(IDLE_PRIORITY_CLASS));
    AddObject('&Normal', Pointer(NORMAL_PRIORITY_CLASS));
    AddObject('&High', Pointer(HIGH_PRIORITY_CLASS));
    AddObject('&Realtime', Pointer(REALTIME_PRIORITY_CLASS));
    EndUpdate;
  end;
end;

procedure TChangePriorityDlg.OKBtnClick(Sender: TObject);
var
  Handle: THandle;
  Priority: DWORD;
  Res: Boolean;
begin
  with PriorityRadioGroup do Priority := DWORD(Items.Objects[ItemIndex]);
  Handle := OpenProcess(PROCESS_ALL_ACCESS{PROCESS_SET_INFORMATION}, False, FProcessID);
  if Handle <> 0 then
  begin
    Res := SetPriorityClass(Handle, Priority);
    CloseHandle(Handle);
  end else Res := False;
  if Res then
    ModalResult := mrOk
  else
    MessBox(sCantChange, MB_ICONERROR);
end;

end.
