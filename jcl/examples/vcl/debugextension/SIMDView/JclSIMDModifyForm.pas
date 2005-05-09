{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSIMDUtils.pas, released on 2004-10-11.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JclSIMDModifyForm;

interface

{$I jedi.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, JclSysInfo, JclSIMDUtils, ToolsApi;

const
  WM_MODIFYCONTINUE = WM_USER + 100;

type
  TJclSIMDModifyFrm = class(TForm)
    ComboBoxDisplay: TComboBox;
    ComboBoxFormat: TComboBox;
    LabelDisplay: TLabel;
    LabelFormat: TLabel;
    LabelBlank: TLabel;
    PanelModify: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    MemoTip: TMemo;
    procedure ComboBoxDisplayChange(Sender: TObject);
    procedure ComboBoxFormatChange(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
  private
    FDisplay: TJclXMMContentType;
    FFormat: TJclSIMDFormat;
    FXMMRegister: TJclXMMRegister;
    FDebugServices: IOTADebuggerServices;
    FServices: IOTAServices;
    FComboBoxList: TList;
    FLabelList: TList;
    FHistory: TStringList;
    FThread: IOTAThread;
    FTextIndex: Integer;
    FExprStr: string;
    FResultStr: string;
    FReturnCode: Cardinal;
    FCPUInfo: TCpuInfo;
    procedure ContinueModify;
    procedure StartModify;
    procedure WMModifyContinue(var Message: TMessage); message WM_MODIFYCONTINUE;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(AThread: IOTAThread; ADisplay: TJclXMMContentType;
      AFormat: TJclSIMDFormat; var ARegister: TJclXMMRegister;
      const ACpuInfo: TCpuInfo):Boolean;
    procedure ThreadEvaluate(const ExprStr, ResultStr: string;
      ReturnCode: Integer);
    procedure UpdateDisplay;
    procedure UpdateFormat;
    procedure LoadHistory;
    procedure SaveHistory;
    procedure MergeHistory;
    property XMMRegister: TJclXMMRegister read FXMMRegister;
    property Display: TJclXMMContentType read FDisplay;
    property Format: TJclSIMDFormat read FFormat;
    property History: TStringList read FHistory;
    property DebugServices: IOTADebuggerServices read FDebugServices;
    property Services: IOTAServices read FServices;
    property Thread: IOTAThread read FThread;
  end;

implementation

{$R *.dfm}

uses
  Registry;

const
  NbEdits: array [TJclXMMContentType] of Byte =
    ( 16, 8, 4, 2, 4, 2 );
  Texts: array [TJclXMMContentType] of string =
    ( 'Byte', 'Word', 'DWord', 'QWord', 'Single', 'Double' );
  HistoryRegKey = '\History Lists\hlSIMDModify';

{ TJclSIMDModifyFrm }

constructor TJclSIMDModifyFrm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComboBoxList := TList.Create;
  FLabelList := TList.Create;
end;

destructor TJclSIMDModifyFrm.Destroy;
begin
  FLabelList.Free;
  FComboBoxList.Free;
  inherited Destroy;
end;

function TJclSIMDModifyFrm.Execute(AThread: IOTAThread; ADisplay: TJclXMMContentType;
  AFormat: TJclSIMDFormat; var ARegister: TJclXMMRegister;
  const ACPUInfo: TCPUInfo): Boolean;
begin
  FTextIndex := 0;
  FXMMRegister := ARegister;
  FFormat := AFormat;
  FDisplay := ADisplay;
  FThread := AThread;
  FCpuInfo := ACpuInfo;
  FHistory := TStringList.Create;
  FHistory.Duplicates := dupIgnore;
  try
    Assert(Supports(BorlandIDEServices,IOTAServices,FServices),
      'Unable to get Borland IDE Services');
    Assert(Supports(BorlandIDEServices,IOTADebuggerServices,FDebugServices),
      'Unable to get Borland Debug Services');

    LoadHistory;

    ComboBoxDisplay.ItemIndex := Integer(Display);
    ComboBoxFormat.Enabled := Display in [xt16Bytes..xt2QWords];
    ComboBoxFormat.ItemIndex := Integer(Format);
    UpdateDisplay;

    Result := ShowModal = mrOk;

    if Result then
      ARegister := XMMRegister;

    MergeHistory;
    SaveHistory;
  finally
    FHistory.Free;
  end;
end;

procedure TJclSIMDModifyFrm.UpdateDisplay;
var
  Index: Integer;
  AComboBox: TComboBox;
  ALabel: TLabel;
  X, Y: Integer;
begin
  MergeHistory;
  while PanelModify.ControlCount>0 do
    PanelModify.Controls[0].Free;
  FComboBoxList.Clear;
  FLabelList.Clear;

  ComboBoxDisplay.ItemIndex := Integer(Display);
  ComboBoxFormat.Enabled := Display in [xt16Bytes..xt2QWords];
  ComboBoxFormat.ItemIndex := Integer(Format);

  X := 0;
  Y := 12;
  for Index := 0 to NbEdits[Display]-1 do
  begin
    AComboBox := TComboBox.Create(Self);
    AComboBox.Parent := PanelModify;
    AComboBox.SetBounds(X+130,Y,90,AComboBox.Height);
    AComboBox.Tag := Index;
    AComboBox.Text := '';
    AComboBox.Items.Assign(History);
    FComboBoxList.Add(AComboBox);
    ALabel := TLabel.Create(Self);
    ALabel.Parent := PanelModify;
    ALabel.SetBounds(X+5,Y+2,60,ALabel.Height);
    ALabel.Tag := Index;
    FLabelList.Add(Pointer(ALabel));
    if (Index=7) then
    begin
      Y := 12;
      X := 230;
    end else Inc(Y,32);
  end;
  UpdateFormat;
end;

procedure TJclSIMDModifyFrm.UpdateFormat;
var
  Index: Integer;
  Value: TJclSIMDValue;
begin
  Value.Display := Display;
  for Index := 0 to FLabelList.Count-1 do
  begin
    with TLabel(FLabelList.Items[Index]) do
      case Display of
        xt16Bytes  : Value.ValueByte := XMMRegister.Bytes[Tag];
        xt8Words   : Value.ValueWord := XMMRegister.Words[Tag];
        xt4DWords  : Value.ValueDWord := XMMRegister.DWords[Tag];
        xt2QWords  : Value.ValueQWord := XMMRegister.QWords[Tag];
        xt4Singles : Value.ValueSingle := XMMRegister.Singles[Tag];
        xt2Doubles : Value.ValueDouble := XMMRegister.Doubles[Tag];
      end;
    TLabel(FLabelList.Items[Index]).Caption := SysUtils.Format('%s%d = %s',[Texts[Display],Index,FormatValue(Value,Format)]);
  end;
end;

procedure TJclSIMDModifyFrm.ComboBoxDisplayChange(Sender: TObject);
begin
  FDisplay := TJclXMMContentType((Sender as TComboBox).ItemIndex);
  UpdateDisplay;
end;

procedure TJclSIMDModifyFrm.ComboBoxFormatChange(Sender: TObject);
begin
  FFormat := TJclSIMDFormat((Sender as TComboBox).ItemIndex);
  UpdateFormat;
end;

procedure TJclSIMDModifyFrm.LoadHistory;
var
  Registry: TRegistry;
  Index, Count: Integer;
begin
  Registry := TRegistry.Create(KEY_READ);
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKey(Services.GetBaseRegistryKey + HistoryRegKey, False) then
    begin
      Count := Registry.ReadInteger('Count');
      History.Clear;
      for Index := 0 to Count-1 do
        History.Add(Registry.ReadString(SysUtils.Format('Item%d',[Index])));
    end;
    Registry.CloseKey;
  finally
    Registry.Free;
  end;
end;

procedure TJclSIMDModifyFrm.SaveHistory;
var
  Registry: TRegistry;
  Index: Integer;
begin
  Registry := TRegistry.Create(KEY_ALL_ACCESS);
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKey(Services.GetBaseRegistryKey + HistoryRegKey, True) then
    begin
      Registry.WriteInteger('Count',History.Count);
      for Index := 0 to History.Count-1 do
        Registry.WriteString(SysUtils.Format('Item%d',[Index]),History.Strings[Index]);
    end;
    Registry.CloseKey;
  finally
    Registry.Free;
  end;
end;

procedure TJclSIMDModifyFrm.MergeHistory;
var
  i, j: Integer;
begin
  History.Duplicates := dupIgnore;
  for i := 0 to PanelModify.ControlCount-1 do
    if PanelModify.Controls[i] is TComboBox then
      with TComboBox(PanelModify.Controls[i]) do
  begin
    for j := 0 to Items.Count-1 do
      if (Items.Strings[j]<>'') and (History.IndexOf(Items.Strings[j])=-1) then
        History.Add(Items.Strings[j]);
    if (Text<>'') and (History.IndexOf(Text)=-1) then
      History.Add(Text);
  end;
  while (History.Count>30) do
    History.Delete(0);
end;

procedure TJclSIMDModifyFrm.WMModifyContinue(var Message: TMessage);
begin
  ContinueModify;
end;

procedure TJclSIMDModifyFrm.StartModify;
begin
  FTextIndex := -1;
  FResultStr := '';
  FReturnCode := 0;
  ContinueModify;
end;

procedure TJclSIMDModifyFrm.ContinueModify;
const
  ResultBufferSize = 200;
var
  EvaluateResult: TOTAEvaluateResult;
  AValue: TJclSIMDValue;
  AComboBox: TComboBox;
  ResultBuffer: array [0..ResultBufferSize-1] of Char;
  ResultAddr, ResultSize: Cardinal;
  CanModify: Boolean;
  VectorFrame: TJclVectorFrame;
begin
  if (FReturnCode <> 0) then
    EvaluateResult := erError
  else EvaluateResult := erOK;
  AValue.Display := Display;
  GetVectorContext(Thread.Handle,VectorFrame);
  while (FTextIndex < FComboBoxList.Count) and (EvaluateResult = erOK) do
  begin
    if (FTextIndex >= 0) and (FResultStr <> '') then
    begin
      if (ParseValue(FResultStr,AValue,Format)) then
        case AValue.Display of
          xt16Bytes  : FXMMRegister.Bytes[FTextIndex] := AValue.ValueByte;
          xt8Words   : FXMMRegister.Words[FTextIndex] := AValue.ValueWord;
          xt4DWords  : FXMMRegister.DWords[FTextIndex] := AValue.ValueDWord;
          xt2QWords  : FXMMRegister.QWords[FTextIndex] := AValue.ValueQWord;
          xt4Singles : FXMMRegister.Singles[FTextIndex] := AValue.ValueSingle;
          xt2Doubles : FXMMRegister.Doubles[FTextIndex] := AValue.ValueDouble;
        end
      else EvaluateResult := erError;
    end;
    if (EvaluateResult = erOK) then
    begin
      Inc(FTextIndex);
      if (FTextIndex < FComboBoxList.Count) then
      begin
        AComboBox := TComboBox(FComboBoxList.Items[FTextIndex]);
        FExprStr := AComboBox.Text;
        if (FExprStr <> '') then
        begin
          if (not ParseValue(FExprStr,AValue,Format)) then
          begin
            if ReplaceXMMRegisters(FExprStr,FCPUInfo.Is64Bits,VectorFrame.XMMRegisters) then
              EvaluateResult := Thread.Evaluate(FExprStr,ResultBuffer,
                ResultBufferSize,CanModify,True,'',ResultAddr,ResultSize,FReturnCode)
            else EvaluateResult := erError;
            if (EvaluateResult <> erDeferred) and (FReturnCode <> 0) then
              EvaluateResult := erError;
            if (EvaluateResult = erOK) then
              FResultStr := ResultBuffer;
            if (FResultStr = '') then
              EvaluateResult := erError;
          end else
          begin
            FResultStr := FExprStr;
            EvaluateResult := erOK;
          end;
        end else FResultStr := '';
      end;
    end;
  end;
  if (EvaluateResult = erError) and (FTextIndex < FComboBoxList.Count) then
  begin
    AComboBox := TComboBox(FComboBoxList.Items[FTextIndex]);
    FocusControl(AComboBox);
    AComboBox.SelectAll;
  end else if (EvaluateResult = erOK) and (FTextIndex >= FComboBoxList.Count) then
    ModalResult := mrOk;
end;

procedure TJclSIMDModifyFrm.ButtonOKClick(Sender: TObject);
begin
  StartModify;
end;

procedure TJclSIMDModifyFrm.ThreadEvaluate(const ExprStr, ResultStr: string;
  ReturnCode: Integer);
begin
  if (CompareText(FExprStr,ExprStr)=0) then
  begin
    FResultStr := ResultStr;
    FReturnCode := ReturnCode;
    PostMessage(Handle,WM_MODIFYCONTINUE,0,0);
  end;
end;

end.
