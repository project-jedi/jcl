{****************************************************************************}
{                                                                            }
{ Project JEDI Code Library (JCL)                                            }
{                                                                            }
{ The contents of this file are subject to the Mozilla Public License        }
{ Version 1.1 (the "License");                                               }
{ you may not use this file except in compliance with the License. You may   }
{ obtain a copy of the License at http://www.mozilla.org/MPL/                }
{                                                                            }
{ Software distributed under the License is distributed on an "AS IS" basis, }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   }
{ for the specific language governing rights and limitations under the       }
{ License.                                                                   }
{                                                                            }
{ The Original Code is JclOtaExcDlgTraceFrame.pas.                           }
{                                                                            }
{ The Initial Developer of the Original Code is Florent Ouchet               }
{         <outchy att users dott sourceforge dott net>                       }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet.    }
{                                                                            }
{ Contributors:                                                              }
{                                                                            }
{****************************************************************************}
{                                                                            }
{ Last modified: $Date: $                                                    }
{                                                                            }
{****************************************************************************}

unit JclOtaExcDlgTraceFrame;

interface

{$I jcl.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JclDebug,
  JclOtaExcDlgRepository, JclOtaWizardFrame;

type
  TJclOtaExcDlgTracePage = class(TJclWizardFrame)
    CheckBoxRawData: TCheckBox;
    CheckBoxModuleName: TCheckBox;
    CheckBoxCodeDetails: TCheckBox;
    CheckBoxVirtualAddress: TCheckBox;
    CheckBoxModuleOffset: TCheckBox;
    MemoStack: TMemo;
    LabelPreview: TLabel;
    CheckBoxStackList: TCheckBox;
    CheckBoxAllThreads: TCheckBox;
    procedure CheckBoxClick(Sender: TObject);
    procedure CheckBoxStackListClick(Sender: TObject);
  private
    FParams: TJclOtaExcDlgParams;
    FTestThread: TJclDebugThread;
    procedure UpdatePreview;
    procedure UpdateCheckBoxes;
  public
    constructor Create(AOwner: TComponent; AParams: TJclOtaExcDlgParams); reintroduce;
    destructor Destroy; override;

    procedure PageActivated(Direction: TJclWizardDirection); override;
    procedure PageDesactivated(Direction: TJclWizardDirection); override;

    property Params: TJclOtaExcDlgParams read FParams write FParams;
  end;

  // in interface to be exported and have basic debug informations based on exports
  TTestThread = class(TJclDebugThread)
  private
    procedure ExecuteTask;
    procedure ExecuteSubTask;
  protected
    procedure Execute; override;
  end;

implementation

{$R *.dfm}

uses
  JclOtaResources;

//=== { TTestThread } ========================================================

{$W+}

procedure TTestThread.Execute;
begin
  ExecuteTask;
end;

{$IFNDEF STACKFRAMES_ON}
{$W-}
{$ENDIF ~STACKFRAMES_ON}

procedure TTestThread.ExecuteTask;
begin
  ExecuteSubTask;
end;

procedure TTestThread.ExecuteSubTask;
begin
  while not Terminated do
    Sleep(100);
end;

//=== { TJclOtaExcDlgTracePage } =============================================

procedure TJclOtaExcDlgTracePage.CheckBoxClick(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TJclOtaExcDlgTracePage.CheckBoxStackListClick(Sender: TObject);
begin
  UpdateCheckBoxes;
end;

constructor TJclOtaExcDlgTracePage.Create(AOwner: TComponent;
  AParams: TJclOtaExcDlgParams);
begin
  FParams := AParams;
  inherited Create(AOwner);
  FTestThread := TTestThread.Create(False, 'MyTaskThread');

  Caption := RsExcDlgTraceOptions;
  CheckBoxStackList.Caption := RsStackList;
  CheckBoxRawData.Caption := RsRawData;
  CheckBoxModuleName.Caption := RsModuleName;
//  CheckBoxAddressOffset.Caption := RsAddressOffset;
  CheckBoxCodeDetails.Caption := RsCodeDetails;
  CheckBoxVirtualAddress.Caption := RsVirtualAddress;
  CheckBoxModuleOffset.Caption := RsModuleOffset;
  LabelPreview.Caption := RsPreview;
  CheckBoxAllThreads.Caption := RsAllThreads;
end;

destructor TJclOtaExcDlgTracePage.Destroy;
begin
  FTestThread.Free;
  inherited Destroy;
end;

procedure TJclOtaExcDlgTracePage.PageActivated(Direction: TJclWizardDirection);
begin
  inherited PageActivated(Direction);

  CheckBoxStackList.Checked := Params.StackList;
  CheckBoxRawData.Checked := Params.RawData;
  CheckBoxModuleName.Checked := Params.ModuleName;
//  CheckBoxAddressOffset.Checked := Params.AddressOffset;
  CheckBoxCodeDetails.Checked := Params.CodeDetails;
  CheckBoxVirtualAddress.Checked := Params.VirtualAddress;
  CheckBoxModuleOffset.Checked := Params.ModuleOffset;
  CheckBoxAllThreads.Checked := Params.AllThreads;

  UpdateCheckBoxes;
end;

procedure TJclOtaExcDlgTracePage.PageDesactivated(
  Direction: TJclWizardDirection);
begin
  inherited PageDesactivated(Direction);

  Params.StackList := CheckBoxStackList.Checked;
  Params.RawData := CheckBoxRawData.Checked;
  Params.ModuleName := CheckBoxModuleName.Checked;
//  Params.AddressOffset := CheckBoxAddressOffset.Checked;
  Params.CodeDetails := CheckBoxCodeDetails.Checked;
  Params.VirtualAddress := CheckBoxVirtualAddress.Checked;
  Params.ModuleOffset := CheckBoxModuleOffset.Checked;
  Params.AllThreads := CheckBoxAllThreads.Checked;
end;

procedure TJclOtaExcDlgTracePage.UpdateCheckBoxes;
var
  AEnabled: Boolean;
begin
  AEnabled := CheckBoxStackList.Enabled;

  CheckBoxRawData.Enabled := AEnabled;
  CheckBoxModuleName.Enabled := AEnabled;
  CheckBoxCodeDetails.Enabled := AEnabled;
  CheckBoxVirtualAddress.Enabled := AEnabled;
  CheckBoxModuleOffset.Enabled := AEnabled;
end;

procedure TJclOtaExcDlgTracePage.UpdatePreview;
var
  AStack: TJclStackInfoList;
begin
  MemoStack.Lines.Clear;
  
  if CheckBoxAllThreads.Checked then
    MemoStack.Lines.Add('Main thread stack trace');

  AStack := TJclStackInfoList.Create(CheckBoxRawData.Checked, 0, nil, False);
  try
    AStack.AddToStrings(MemoStack.Lines, CheckBoxModuleName.Checked,
      CheckBoxModuleOffset.Checked, CheckBoxCodeDetails.Checked, CheckBoxVirtualAddress.Checked);
  finally
    AStack.Free;
  end;

  if CheckBoxAllThreads.Checked then
  begin
    MemoStack.Lines.Add('');
    MemoStack.Lines.Add(Format('Stack trace for thread: "%s" (%s)', [FTestThread.ThreadName, FTestThread.ClassName]));
    AStack := JclCreateThreadStackTrace(CheckBoxRawData.Checked, FTestThread.Handle);
    try
      AStack.AddToStrings(MemoStack.Lines, CheckBoxModuleName.Checked,
        CheckBoxModuleOffset.Checked, CheckBoxCodeDetails.Checked, CheckBoxVirtualAddress.Checked);
    finally
      AStack.Free;
    end;
  end;
end;

end.
