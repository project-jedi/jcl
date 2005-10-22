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
{ The Original Code is: JvSIMDModifyForm.pas, released on 2004-10-11.                              }
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

unit JclSIMDModifyForm;

interface

{$I jcl.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ToolsApi, IniFiles, Contnrs,
  JclSysInfo, JclSIMDUtils;

const
  WM_MODIFYCONTINUE = WM_USER + 100;

type
  TJclRegisterType = (rtXMM, rtMM);
  
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
    FRegisterType: TJclRegisterType;
    FXMMRegister: TJclXMMRegister;
    FMMRegister: TJclMMRegister;
    FDisplay: TJclXMMContentType;
    FFormat: TJclSIMDFormat;
    FDebuggerServices: IOTADebuggerServices;
    FSettings: TCustomIniFile;
    FComboBoxList: TComponentList;
    FLabelList: TComponentList;
    FHistory: TStringList;
    FThread: IOTAThread;
    FTextIndex: Integer;
    FExprStr: string;
    FResultStr: string;
    FReturnCode: Cardinal;
    FCPUInfo: TCpuInfo;
    procedure ContinueModify;
    procedure StartModify;
    procedure WMModifyContinue(var Msg: TMessage); message WM_MODIFYCONTINUE;
  protected
    property RegisterType: TJclRegisterType read FRegisterType;
    property XMMRegister: TJclXMMRegister read FXMMRegister;
    property MMRegister: TJclMMRegister read FMMRegister;
    property DebuggerServices: IOTADebuggerServices read FDebuggerServices;
    property Settings: TCustomIniFile read FSettings;
  public
    constructor Create (AOwner: TComponent;
      ADebuggerServices: IOTADebuggerServices; ASettings: TCustomIniFile); reintroduce;
    destructor Destroy; override;
    function Execute(AThread: IOTAThread; ADisplay: TJclXMMContentType;
      AFormat: TJclSIMDFormat; var ARegister: TJclXMMRegister;
      const ACpuInfo: TCpuInfo): Boolean; overload;
    function Execute(AThread: IOTAThread; ADisplay: TJclXMMContentType;
      AFormat: TJclSIMDFormat; var ARegister: TJclMMRegister;
      const ACpuInfo: TCpuInfo): Boolean; overload;
    procedure ThreadEvaluate(const ExprStr, ResultStr: string; ReturnCode: Integer);
    procedure UpdateDisplay;
    procedure UpdateFormat;
    procedure LoadHistory;
    procedure SaveHistory;
    procedure MergeHistory;

    property Display: TJclXMMContentType read FDisplay;
    property Format: TJclSIMDFormat read FFormat;
    property History: TStringList read FHistory;
    property Thread: IOTAThread read FThread;
  end;

implementation

{$R *.dfm}

const
  NbEdits: array [TJclRegisterType, TJclXMMContentType] of Byte =
   (
    (16, 8, 4, 2, 4, 2),
    ( 8, 4, 2, 1, 2, 1)
   );

  Texts: array [TJclXMMContentType] of string =
    ('Byte', 'Word', 'DWord', 'QWord', 'Single', 'Double');

  ItemFormat = 'Item%d';
  CountPropertyName = 'Count';

  HistoryListSize = 30;

//=== { TJclSIMDModifyFrm } ==================================================

constructor TJclSIMDModifyFrm.Create(AOwner: TComponent;
  ADebuggerServices: IOTADebuggerServices; ASettings: TCustomIniFile);
begin
  inherited Create(AOwner);

  FDebuggerServices := ADebuggerServices;
  FSettings := ASettings;

  FComboBoxList := TComponentList.Create(False);
  FLabelList := TComponentList.Create(False);
  FHistory := TStringList.Create;
  FHistory.Duplicates := dupIgnore;
end;

destructor TJclSIMDModifyFrm.Destroy;
begin
  FLabelList.Free;
  FComboBoxList.Free;
  FHistory.Free;
  FDebuggerServices := nil;

  inherited Destroy;
end;

function TJclSIMDModifyFrm.Execute(AThread: IOTAThread; ADisplay: TJclXMMContentType;
  AFormat: TJclSIMDFormat; var ARegister: TJclXMMRegister;
  const ACPUInfo: TCPUInfo): Boolean;
begin
  FTextIndex := 0;
  FRegisterType := rtXMM;
  FXMMRegister := ARegister;
  FFormat := AFormat;
  FDisplay := ADisplay;
  FThread := AThread;
  FCpuInfo := ACpuInfo;

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
end;

function TJclSIMDModifyFrm.Execute(AThread: IOTAThread;
  ADisplay: TJclXMMContentType; AFormat: TJclSIMDFormat;
  var ARegister: TJclMMRegister; const ACpuInfo: TCpuInfo): Boolean;
begin
  FTextIndex := 0;
  FRegisterType := rtMM;
  FMMRegister := ARegister;
  FFormat := AFormat;
  FDisplay := ADisplay;
  FThread := AThread;
  FCpuInfo := ACpuInfo;

  LoadHistory;

  ComboBoxDisplay.ItemIndex := Integer(Display);
  ComboBoxFormat.Enabled := Display in [xt16Bytes..xt2QWords];
  ComboBoxFormat.ItemIndex := Integer(Format);
  UpdateDisplay;

  Result := ShowModal = mrOk;

  if Result then
    ARegister := MMRegister;

  MergeHistory;
  SaveHistory;
end;

procedure TJclSIMDModifyFrm.UpdateDisplay;
var
  Index: Integer;
  AComboBox: TComboBox;
  ALabel: TLabel;
  X, Y: Integer;
begin
  MergeHistory;
  while PanelModify.ControlCount > 0 do
    PanelModify.Controls[0].Free;
  FComboBoxList.Clear;
  FLabelList.Clear;

  ComboBoxDisplay.ItemIndex := Integer(Display);
  ComboBoxFormat.Enabled := Display in [xt16Bytes..xt2QWords];
  ComboBoxFormat.ItemIndex := Integer(Format);

  X := 0;
  Y := 12;
  for Index := 0 to NbEdits[RegisterType, Display] - 1 do
  begin
    AComboBox := TComboBox.Create(Self);
    AComboBox.Parent := PanelModify;
    AComboBox.SetBounds(X + 130, Y, 90, AComboBox.Height);
    AComboBox.Tag := Index;
    AComboBox.Text := '';
    AComboBox.Items.Assign(History);
    FComboBoxList.Add(AComboBox);
    ALabel := TLabel.Create(Self);
    ALabel.Parent := PanelModify;
    ALabel.SetBounds(X + 5, Y + 2, 60, ALabel.Height);
    ALabel.Tag := Index;
    FLabelList.Add(ALabel);
    if Index = 7 then
    begin
      Y := 12;
      X := 230;
    end
    else
      Inc(Y, 32);
  end;
  UpdateFormat;
end;

procedure TJclSIMDModifyFrm.UpdateFormat;
var
  Index: Integer;
  Value: TJclSIMDValue;
  ALabel: TLabel;
begin
  Value.Display := Display;
  for Index := 0 to FLabelList.Count - 1 do
  begin
    ALabel := FLabelList.Items[Index] as TLabel;
    case RegisterType of
      rtXMM:
        case Display of
          xt16Bytes:
            Value.ValueByte := XMMRegister.Bytes[ALabel.Tag];
          xt8Words:
            Value.ValueWord := XMMRegister.Words[ALabel.Tag];
          xt4DWords:
            Value.ValueDWord := XMMRegister.DWords[ALabel.Tag];
          xt2QWords:
            Value.ValueQWord := XMMRegister.QWords[ALabel.Tag];
          xt4Singles:
            Value.ValueSingle := XMMRegister.Singles[ALabel.Tag];
          xt2Doubles:
            Value.ValueDouble := XMMRegister.Doubles[ALabel.Tag];
        end;
      rtMM:
        case Display of
          xt16Bytes:
            Value.ValueByte := MMRegister.Bytes[ALabel.Tag];
          xt8Words:
            Value.ValueWord := MMRegister.Words[ALabel.Tag];
          xt4DWords:
            Value.ValueDWord := MMRegister.DWords[ALabel.Tag];
          xt2QWords:
            Value.ValueQWord := MMRegister.QWords;
          xt4Singles:
            Value.ValueSingle := MMRegister.Singles[ALabel.Tag];
          xt2Doubles:
            begin
              ALabel.Caption := '';
              Break;
            end;
        end;
    end;
    ALabel.Caption := SysUtils.Format('%s%d = %s', [Texts[Display], Index, FormatValue(Value, Format)]);
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
  Index, Count: Integer;
begin
  Count := Settings.ReadInteger(ClassName, CountPropertyName, 0);
  History.Clear;

  for Index := 0 to Count - 1 do
    History.Add(Settings.ReadString(ClassName, SysUtils.Format(ItemFormat, [Index]), ''));
end;

procedure TJclSIMDModifyFrm.SaveHistory;
var
  Index: Integer;
begin
  Settings.WriteInteger(ClassName, CountPropertyName, History.Count);
  for Index := 0 to History.Count-1 do
    Settings.WriteString(ClassName, SysUtils.Format(ItemFormat, [Index]), History.Strings[Index]);
end;

procedure TJclSIMDModifyFrm.MergeHistory;
var
  I, J: Integer;
begin
  History.Duplicates := dupIgnore;
  for I := 0 to PanelModify.ControlCount - 1 do
    if PanelModify.Controls[I] is TComboBox then
      with TComboBox(PanelModify.Controls[I]) do
  begin
    for J := 0 to Items.Count - 1 do
      if (Items.Strings[J] <> '') and (History.IndexOf(Items.Strings[J]) = -1) then
        History.Add(Items.Strings[J]);
    if (Text <> '') and (History.IndexOf(Text) = -1) then
      History.Add(Text);
  end;
  while History.Count > HistoryListSize do
    History.Delete(0);
end;

procedure TJclSIMDModifyFrm.WMModifyContinue(var Msg: TMessage);
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
  else
    EvaluateResult := erOK;
  AValue.Display := Display;
  GetVectorContext(Thread.Handle, VectorFrame);
  while (FTextIndex < FComboBoxList.Count) and (EvaluateResult = erOK) do
  begin
    if (FTextIndex >= 0) and (FResultStr <> '') then
    begin
      if (ParseValue(FResultStr,AValue,Format)) then
        case RegisterType of
          rtXMM:
            case AValue.Display of
              xt16Bytes:
                FXMMRegister.Bytes[FTextIndex] := AValue.ValueByte;
              xt8Words:
                FXMMRegister.Words[FTextIndex] := AValue.ValueWord;
              xt4DWords:
                FXMMRegister.DWords[FTextIndex] := AValue.ValueDWord;
              xt2QWords:
                FXMMRegister.QWords[FTextIndex] := AValue.ValueQWord;
              xt4Singles:
                FXMMRegister.Singles[FTextIndex] := AValue.ValueSingle;
              xt2Doubles:
                FXMMRegister.Doubles[FTextIndex] := AValue.ValueDouble;
            end;
          rtMM:
            case AValue.Display of
              xt16Bytes:
                FMMRegister.Bytes[FTextIndex] := AValue.ValueByte;
              xt8Words:
                FMMRegister.Words[FTextIndex] := AValue.ValueWord;
              xt4DWords:
                FMMRegister.DWords[FTextIndex] := AValue.ValueDWord;
              xt2QWords:
                FMMRegister.QWords := AValue.ValueQWord;
              xt4Singles:
                FMMRegister.Singles[FTextIndex] := AValue.ValueSingle;
              xt2Doubles:
                EvaluateResult := erError;
            end;
          else
            EvaluateResult := erError;
        end
      else
        EvaluateResult := erError;
    end;
    if EvaluateResult = erOK then
    begin
      Inc(FTextIndex);
      if FTextIndex < FComboBoxList.Count then
      begin
        AComboBox := TComboBox(FComboBoxList.Items[FTextIndex]);
        FExprStr := AComboBox.Text;
        if FExprStr <> '' then
        begin
          if not ParseValue(FExprStr, AValue, Format) then
          begin
            if ReplaceSIMDRegisters(FExprStr, FCPUInfo.Is64Bits, VectorFrame) then
              EvaluateResult := Thread.Evaluate(FExprStr, ResultBuffer,
                ResultBufferSize, CanModify, True, '', ResultAddr, ResultSize, FReturnCode)
            else
              EvaluateResult := erError;
            if (EvaluateResult <> erDeferred) and (FReturnCode <> 0) then
              EvaluateResult := erError;
            if EvaluateResult = erOK then
              FResultStr := ResultBuffer;
            if FResultStr = '' then
              EvaluateResult := erError;
          end
          else
          begin
            FResultStr := FExprStr;
            EvaluateResult := erOK;
          end;
        end
        else
          FResultStr := '';
      end;
    end;
  end;
  if (EvaluateResult = erError) and (FTextIndex < FComboBoxList.Count) then
  begin
    AComboBox := TComboBox(FComboBoxList.Items[FTextIndex]);
    FocusControl(AComboBox);
    AComboBox.SelectAll;
  end
  else
  if (EvaluateResult = erOK) and (FTextIndex >= FComboBoxList.Count) then
    ModalResult := mrOk;
end;

procedure TJclSIMDModifyFrm.ButtonOKClick(Sender: TObject);
begin
  StartModify;
end;

procedure TJclSIMDModifyFrm.ThreadEvaluate(const ExprStr, ResultStr: string; ReturnCode: Integer);
begin
  if CompareText(FExprStr, ExprStr) = 0 then
  begin
    FResultStr := ResultStr;
    FReturnCode := ReturnCode;
    PostMessage(Handle, WM_MODIFYCONTINUE, 0, 0);
  end;
end;

end.
