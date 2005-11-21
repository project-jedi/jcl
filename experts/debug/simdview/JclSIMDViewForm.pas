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
{ The Original Code is: JvSIMDViewForm.pas, released on 2004-10-11.                                }
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

unit JclSIMDViewForm;

{$I jcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ToolsApi, Grids, ExtCtrls, Menus, ActnList,
  JclOtaUtils, JclSysInfo, JclSIMDUtils, JclSIMDModifyForm;

type
  TJclSIMDViewFrm = class(TForm)
    Splitter: TSplitter;
    ListBoxRegs: TListBox;           
    ListBoxMXCSR: TListBox;
    PopupMenuRegs: TPopupMenu;
    PopupMenuMXCSR: TPopupMenu;
    MenuItemComplement: TMenuItem;
    MenuItemBinary: TMenuItem;
    MenuItemSigned: TMenuItem;
    MenuItemUnsigned: TMenuItem;
    MenuItemHexa: TMenuItem;
    MenuItemDisplay: TMenuItem;
    MenuItemFormat: TMenuItem;
    MenuItemBytes: TMenuItem;
    MenuItemWords: TMenuItem;
    MenuItemDWords: TMenuItem;
    MenuItemQWords: TMenuItem;
    MenuItemSeparator1: TMenuItem;
    MenuItemSingles: TMenuItem;
    MenuItemDoubles: TMenuItem;
    MenuItemSeparator2: TMenuItem;
    MenuItemStayOnTop: TMenuItem;
    MenuItemModify: TMenuItem;
    MenuItemCpuInfo: TMenuItem;
    ActionListOptions: TActionList;
    ActionStayOnTop: TAction;
    ActionModify: TAction;
    ActionComplement: TAction;
    ActionEmpty: TAction;
    ActionEmptyAll: TAction;
    MenuItemEmptyMM: TMenuItem;
    MenuItemEmptyAll: TMenuItem;
    procedure ListBoxMXCSRDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ListBoxMXCSRMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ListBoxRegsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure MenuItemFormatClick(Sender: TObject);
    procedure MenuItemDisplayClick(Sender: TObject);
    procedure MenuItemCpuInfoClick(Sender: TObject);
    procedure ActionStayOnTopUpdate(Sender: TObject);
    procedure ActionStayOnTopExecute(Sender: TObject);
    procedure ActionModifyUpdate(Sender: TObject);
    procedure ActionModifyExecute(Sender: TObject);
    procedure ActionComplementExecute(Sender: TObject);
    procedure ActionComplementUpdate(Sender: TObject);
    procedure ActionEmptyUpdate(Sender: TObject);
    procedure ActionEmptyAllUpdate(Sender: TObject);
    procedure ActionEmptyExecute(Sender: TObject);
    procedure ActionEmptyAllExecute(Sender: TObject);
    procedure ListBoxesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FDebuggerServices: IOTADebuggerServices;
    FVectorFrame: TJclVectorFrame;
    FDisplay: TJclXMMContentType;
    FFormat: TJclSIMDFormat;
    FCpuInfo: TCpuInfo;
    FSIMDCaption: string;
    FNbMMRegister: Integer;
    FNbXMMRegister: Integer;
    FOldThreadID: LongWord;
    FOldThreadState: TOTAThreadState;
    FModifyForm: TJclSIMDModifyFrm;
    FExpert: TJclOTAExpert;
    FMXCSRChanged: array [TMXCSRRange] of Boolean;
    FRegisterChanged: array of Boolean;
    procedure SetDisplay(const Value: TJclXMMContentType);
    procedure SetFormat(const Value: TJclSIMDFormat);
  protected
    procedure DoClose(var Action: TCloseAction); override;
    procedure UpdateActions; override;
  public
    constructor Create(AOwner: TComponent; ADebuggerServices: IOTADebuggerServices;
      AExpert: TJclOTAExpert); reintroduce;
    destructor Destroy; override;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure ThreadEvaluate(const ExprStr, ResultStr: string; ReturnCode: Integer);
    procedure SetThreadValues;
    procedure GetThreadValues;
    property CpuInfo: TCpuInfo read FCpuInfo;
    property Format: TJclSIMDFormat read FFormat write SetFormat;
    property Display: TJclXMMContentType read FDisplay write SetDisplay;
    property SIMDCaption: string read FSIMDCaption write FSIMDCaption;
    property DebuggerServices: IOTADebuggerServices read FDebuggerServices;
    property NbMMRegister: Integer read FNbMMRegister;
    property NbXMMRegister: Integer read FNbXMMRegister;
  end;

implementation

uses
  TypInfo,
  JclSIMDCpuInfo;

{$R *.dfm}

constructor TJclSIMDViewFrm.Create(AOwner: TComponent;
  ADebuggerServices: IOTADebuggerServices; AExpert: TJclOTAExpert);
var
  I: TMXCSRRange;
  J: Integer;
begin
  inherited Create(AOwner);

  FDebuggerServices := ADebuggerServices;
  FOldThreadID := 0;
  FOldThreadState := tsNone;
  FExpert := AExpert;

  JclSysInfo.GetCpuInfo(FCpuInfo);

  // the behaviour of Delphi and C++Builder overrides all changes made on
  // the floating point context of the debugged thread when it is run
  // (even using step into and step over).
  // to be uncommented as soon as Borland changes this behaviour
  {if CpuInfo.MMX or CPUInfo._3DNow then
    FNbMMRegister := 8
  else
    FNbMMRegister := 0;}

  FNbMMRegister := 0;

  if CpuInfo.SSE = 0 then
    FNbXMMRegister := 0
  else
  if CpuInfo.Is64Bits then
    FNbXMMRegister := 17
  else
    FNbXMMRegister := 9;

  ListBoxMXCSR.Items.Clear;
  with CpuInfo do
    for I := Low(TMXCSRRange) to High(TMXCSRRange) do
      ListBoxMXCSR.Items.Add('0');
  ListBoxRegs.Items.Clear;

  SetLength(FRegisterChanged,NbMMRegister + NbXMMRegister);
  for J := 0 to NbMMRegister + NbXMMRegister - 1 do
  // MM registers (MMX) + XMM registers (SSE) + 1 cardinal (MXCSR)
    ListBoxRegs.Items.Add('');

  MenuItemBinary.Tag := Integer(sfBinary);
  MenuItemSigned.Tag := Integer(sfSigned);
  MenuItemUnsigned.Tag := Integer(sfUnsigned);
  MenuItemHexa.Tag := Integer(sfHexa);
  MenuItemBytes.Tag := Integer(xt16Bytes);
  MenuItemWords.Tag := Integer(xt8Words);
  MenuItemDWords.Tag := Integer(xt4DWords);
  MenuItemQWords.Tag := Integer(xt2QWords);
  MenuItemSingles.Tag := Integer(xt4Singles);
  MenuItemDoubles.Tag := Integer(xt2Doubles);

  Format := sfHexa;
  Display := xt8Words;

  GetThreadValues;
end;

destructor TJclSIMDViewFrm.Destroy;
begin
  SetLength(FRegisterChanged,0);
  FDebuggerServices := nil;

  inherited Destroy;
end;

procedure TJclSIMDViewFrm.LoadSettings;
begin
  SetBounds(
    FExpert.LoadInteger('Left', Left),
    FExpert.LoadInteger('Top', Top),
    FExpert.LoadInteger('Width', Width),
    FExpert.LoadInteger('Height', Height));

  if Left < 0 then
    Left := 0;
  if Top < 0 then
    Top := 0;
  if Width > Screen.Width then
    Width := Screen.Width;
  if (Left + Width) > Screen.DesktopWidth then
    Left := Screen.DesktopWidth - Width;
  if Height > Screen.Height then
    Height := Screen.Height;
  if (Top + Height) > Screen.DesktopHeight then
    Top := Screen.DesktopHeight - Height;

  Format := TJclSIMDFormat(GetEnumValue(TypeInfo(TJclSIMDFormat),
    FExpert.LoadString('Format', GetEnumName(TypeInfo(TJclSIMDFormat), Integer(sfHexa)))));
  Display := TJclXMMContentType(GetEnumValue(TypeInfo(TJclXMMContentType),
    FExpert.LoadString('Display', GetEnumName(TypeInfo(TJclXMMContentType), Integer(xt8Words)))));

  if FExpert.LoadInteger('StayOnTop', 0) = 1 then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
end;

procedure TJclSIMDViewFrm.SaveSettings;
begin
  FExpert.SaveInteger('Left', Left);
  FExpert.SaveInteger('Top', Top);
  FExpert.SaveInteger('Width', Width);
  FExpert.SaveInteger('Height', Height);
  FExpert.SaveString('Display', GetEnumName(TypeInfo(TJclXMMContentType), Integer(Display)));
  FExpert.SaveString('Format', GetEnumName(TypeInfo(TJclSIMDFormat), Integer(Format)));
  FExpert.SaveInteger('StayOnTop', Ord(FormStyle = fsStayOnTop));
end;

procedure TJclSIMDViewFrm.ListBoxMXCSRDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with (Control as TListBox), Canvas do
  begin
    if not (odFocused in State) then
    begin
      Pen.Color := Brush.Color;
      if odSelected in State then
        Font.Color := clWindow;
    end;
    Rectangle(Rect);
    TextOut(Rect.Left + 2, Rect.Top, MXCSRBitsDescriptions[Index].ShortName);
    if FMXCSRChanged[Index] then
      Font.Color := clRed;
    TextOut(Rect.Left + 2 + TextExtent(MXCSRBitsDescriptions[Index].ShortName).cx, Rect.Top, Items[Index]);
  end;
end;

procedure TJclSIMDViewFrm.ListBoxMXCSRMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  AIndex: Integer;
  AText: string;
begin
  if Shift <> [] then
    Application.HideHint
  else
    with Sender as TListBox do
    begin
      AIndex := ItemAtPos(Point(X,Y),True);
      if (AIndex >= 0) and (AIndex < Items.Count) then
      begin
        with MXCSRBitsDescriptions[AIndex] do
        begin
          AText := LongName;
          if AndMask = MXCSR_RC then
            case (FVectorFrame.MXCSR and AndMask) shr Shifting of
              0:
                AText := SysUtils.Format('%s (%s)', [AText, RsRoundToNearest]);
              1:
                AText := SysUtils.Format('%s (%s)', [AText, RsRoundDown]);
              2:
                AText := SysUtils.Format('%s (%s)', [AText, RsRoundUp]);
              3:
                AText := SysUtils.Format('%s (%s)', [AText, RsRoundTowardZero]);
          end;
          if AText <> Hint then
          begin
            Hint := AText;
            Application.HideHint;
            Application.ActivateHint(Point(X, Y));
          end;
        end;
      end
      else
      begin
        Hint := '';
        Application.HideHint;
      end;
    end;
end;

procedure TJclSIMDViewFrm.ListBoxRegsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  AText: string;
begin
  with (Control as TListBox), Canvas do
  begin
    if not (odFocused in State) then
    begin
      Pen.Color := Brush.Color;
      if odSelected in State then
        Font.Color := clWindow;
    end;
    Rectangle(Rect);
    if Index < NbMMRegister then
      AText := SysUtils.Format('MM%d ', [Index])
    else
    if Index < NbMMRegister + NbXMMRegister - 1 then
    begin
      if CpuInfo.Is64Bits then
        AText := SysUtils.Format('XMM%.2d ', [Index - NbMMRegister])
      else
        AText := SysUtils.Format('XMM%d ', [Index - NbMMRegister]);
    end
    else
      AText := 'MXCSR ';
    TextOut(Rect.Left + 2, Rect.Top, AText);
    if FRegisterChanged[Index] then
      Font.Color := clRed;
    TextOut(Rect.Left + 2 + TextExtent(AText).cx, Rect.Top, Items[Index]);
  end;
end;

procedure TJclSIMDViewFrm.GetThreadValues;
var
  NewVectorFrame: TJclVectorFrame;
  NewBitValue, OldBitValue: Cardinal;
  Index: Integer;
  AProcess: IOTAProcess;
  AThread: IOTAThread;

  function ChangedFlag(const Value1, Value2: TJclXMMRegister): Boolean; overload;
  begin
    Result := (Value1.QWords[0] <> Value2.QWords[0]) or (Value1.QWords[1] <> Value2.QWords[1]);
  end;

  function ChangedFlag(const Value1, Value2: TJclMMRegister): Boolean; overload;
  begin
    Result := Value1.QWords <> Value2.QWords;
  end;

  function FormatReg(const AReg: TJclXMMRegister): string; overload;
  var
    I: Integer;
    Value: TJclSIMDValue;
  begin
    Result := '';
    Value.Display := Display;
    case Display of
      xt16Bytes:
        for I := High(AReg.Bytes) downto Low(AReg.Bytes) do
        begin
          Value.ValueByte := AReg.Bytes[I];
          Result := Result + ' ' + FormatValue(Value, Format);
        end;
      xt8Words:
        for I := High(AReg.Words) downto Low(AReg.Words) do
        begin
          Value.ValueWord := AReg.Words[I];
          Result := Result + ' ' + FormatValue(Value, Format);
        end;
      xt4DWords:
        for I := High(AReg.DWords) downto Low(AReg.DWords) do
        begin
          Value.ValueDWord := AReg.DWords[I];
          Result := Result + ' ' + FormatValue(Value, Format);
        end;
      xt2QWords:
        for I := High(AReg.QWords) downto Low(AReg.QWords) do
        begin
          Value.ValueQWord := AReg.QWords[I];
          Result := Result + ' ' + FormatValue(Value, Format);
        end;
      xt4Singles:
        for I := High(AReg.Singles) downto Low(AReg.Singles) do
        begin
          Value.ValueSingle := AReg.Singles[I];
          Result := Result + ' ' + FormatValue(Value, sfBinary);
        end;
      xt2Doubles:
        for I := High(AReg.Doubles) downto Low(AReg.Doubles) do
        begin
          Value.ValueDouble := AReg.Doubles[I];
          Result := Result + ' ' + FormatValue(Value, sfBinary);
        end;
    end;
  end;

  function FormatReg(const AReg: TJclFPUData; Index: Cardinal): string; overload;
  var
    I: Integer;
    Value: TJclSIMDValue;
  begin
    Result := '';
    Value.Display := Display;

    if (AReg.Reserved = $FFFF) and ((NewVectorFrame.FTW and (1 shl Index)) <> 0) then
      case Display of
        xt16Bytes:
          for I := High(AReg.MMRegister.Bytes) downto Low(AReg.MMRegister.Bytes) do
          begin
            Value.ValueByte := AReg.MMRegister.Bytes[I];
            Result := Result + ' ' + FormatValue(Value, Format);
          end;
        xt8Words:
          for I := High(AReg.MMRegister.Words) downto Low(AReg.MMRegister.Words) do
          begin
            Value.ValueWord := AReg.MMRegister.Words[I];
            Result := Result + ' ' + FormatValue(Value, Format);
          end;
        xt4DWords:
          for I := High(AReg.MMRegister.DWords) downto Low(AReg.MMRegister.DWords) do
          begin
            Value.ValueDWord := AReg.MMRegister.DWords[I];
            Result := Result + ' ' + FormatValue(Value, Format);
          end;
        xt2QWords:
          begin
            Value.ValueQWord := AReg.MMRegister.QWords;
            Result := FormatValue(Value, Format);
          end;
        xt4Singles:
          for I := High(AReg.MMRegister.Singles) downto Low(AReg.MMRegister.Singles) do
          begin
            Value.ValueSingle := AReg.MMRegister.Singles[I];
            Result := Result + ' ' + FormatValue(Value, sfBinary);
          end;
        xt2Doubles:
          Result := RsNotSupportedFormat;
      end
    else
      Result := RsNoPackedData;
  end;

begin
  AProcess := nil;
  AThread := nil;
  if DebuggerServices.ProcessCount > 0 then
    AProcess := DebuggerServices.CurrentProcess;
  if (AProcess <> nil) and (AProcess.ThreadCount > 0) then
    AThread := AProcess.CurrentThread;

  if (AThread = nil) or (AThread.State = tsNone) or
    (AThread.GetOSThreadID = 0) or (AThread.Handle = 0) then
  begin
    Close;
    Exit;
  end;

  case AThread.State of
    tsStopped:
      begin
        if DebuggerServices.CurrentProcess.ThreadCount > 1 then
          Caption := SysUtils.Format('%s Thread : %d', [SIMDCaption,AThread.GetOSThreadID])
        else
          Caption := SIMDCaption;

        GetVectorContext(AThread,NewVectorFrame);

        for Index := 0 to ListBoxMXCSR.Items.Count - 1 do
          with ListBoxMXCSR, Items, MXCSRBitsDescriptions[Index] do
          begin
            NewBitValue := NewVectorFrame.MXCSR and AndMask;
            OldBitValue := FVectorFrame.MXCSR and AndMask;
            FMXCSRChanged[Index] := NewBitValue <> OldBitValue;
            Strings[Index] := IntToStr(NewBitValue shr Shifting);
          end;
        ListBoxMXCSR.Invalidate;

        for Index := 0 to NbMMRegister - 1 do
        begin
          FRegisterChanged[Index] := ChangedFlag(NewVectorFrame.FPURegisters[Index].Data.MMRegister,
            FVectorFrame.FPURegisters[Index].Data.MMRegister);
          ListBoxRegs.Items.Strings[Index] := FormatReg(NewVectorFrame.FPURegisters[Index].Data, Index);
        end;

        if FNbXMMRegister > 0 then
        begin
          for Index := 0 to FNbXMMRegister - 2 do
          begin
            FRegisterChanged[Index + NbMMRegister] := ChangedFlag(NewVectorFrame.XMMRegisters.LongXMM[Index],
              FVectorFrame.XMMRegisters.LongXMM[Index]);
            ListBoxRegs.Items.Strings[Index + NbMMRegister] := FormatReg(NewVectorFrame.XMMRegisters.LongXMM[Index]);
          end;

          FRegisterChanged[NbMMRegister + NbXMMRegister - 1] := NewVectorFrame.MXCSR <> FVectorFrame.MXCSR;
          ListBoxRegs.Items.Strings[NbMMRegister + NbXMMRegister - 1] := IntToHex(NewVectorFrame.MXCSR, 8);
        end;
        ListBoxRegs.Invalidate;

        FVectorFrame := NewVectorFrame;
      end;
    tsRunnable:
      Caption := SysUtils.Format('%s <running>', [SIMDCaption]);
    tsBlocked:
      Caption := SysUtils.Format('%s <blocked>', [SIMDCaption]);
  end;
end;

procedure TJclSIMDViewFrm.SetThreadValues;
begin
  if not SetVectorContext(DebuggerServices.CurrentProcess.CurrentThread,FVectorFrame) then
    raise Exception.Create('Unable to update the thread context');
end;

procedure TJclSIMDViewFrm.MenuItemFormatClick(Sender: TObject);
begin
  Format := TJclSIMDFormat((Sender as TMenuItem).Tag);
end;

procedure TJclSIMDViewFrm.SetDisplay(const Value: TJclXMMContentType);
var
  AEnabled: Boolean;
begin
  FDisplay := Value;
  MenuItemBytes.Checked := Value = xt16Bytes;
  MenuItemWords.Checked := Value = xt8Words;
  MenuItemDWords.Checked := Value = xt4DWords;
  MenuItemQWords.Checked := Value = xt2QWords;
  MenuItemSingles.Checked := Value = xt4Singles;
  MenuItemDoubles.Checked := Value = xt2Doubles;

  AEnabled := not (Value in [xt4Singles, xt2Doubles]);
  MenuItemBinary.Enabled := AEnabled;
  MenuItemSigned.Enabled := AEnabled;
  MenuItemUnsigned.Enabled := AEnabled;
  MenuItemHexa.Enabled := AEnabled;

  GetThreadValues;
end;

procedure TJclSIMDViewFrm.SetFormat(const Value: TJclSIMDFormat);
begin
  FFormat := Value;
  MenuItemBinary.Checked := Value = sfBinary;
  MenuItemSigned.Checked := Value = sfSigned;
  MenuItemUnsigned.Checked := Value = sfUnsigned;
  MenuItemHexa.Checked := Value = sfHexa;

  GetThreadValues;
end;

procedure TJclSIMDViewFrm.MenuItemDisplayClick(Sender: TObject);
begin
  Display := TJclXMMContentType((Sender as TMenuItem).Tag);
end;

procedure TJclSIMDViewFrm.DoClose(var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TJclSIMDViewFrm.MenuItemCpuInfoClick(Sender: TObject);
var
  FormCPUInfo: TJclFormCpuInfo;
begin
  FormCPUInfo := TJclFormCpuInfo.Create(Self);
  try
    FormCPUInfo.Execute(CpuInfo);
  finally
    FormCPUInfo.Free;
  end;
end;

procedure TJclSIMDViewFrm.UpdateActions;
var
  CurrentThreadID: Cardinal;
  AProcess: IOTAProcess;
  AThread: IOTAThread;
  ANewThreadState: TOTAThreadState;
begin
  inherited UpdateActions;

  CurrentThreadID := 0;
  AProcess := nil;
  AThread := nil;

  if DebuggerServices.ProcessCount > 0 then
    AProcess := DebuggerServices.CurrentProcess;
  if (AProcess <> nil) and (AProcess.ThreadCount > 0) then
    AThread := AProcess.CurrentThread;
  if AThread <> nil then
  begin
    ANewThreadState := AThread.State;
    if ANewThreadState in [tsStopped, tsBlocked] then
      CurrentThreadID := AThread.GetOSThreadID;
    if (CurrentThreadID <> 0) and ((CurrentThreadID <> FOldThreadID) or (ANewThreadState <> FOldThreadState)) then
    begin
      FOldThreadID := CurrentThreadID;
      FOldThreadState := ANewThreadState;
      GetThreadValues;
    end;
  end;
end;

procedure TJclSIMDViewFrm.ThreadEvaluate(const ExprStr, ResultStr: string;
  ReturnCode: Integer);
begin
  if Assigned(FModifyForm) then
    FModifyForm.ThreadEvaluate(ExprStr, ResultStr, ReturnCode);
end;

procedure TJclSIMDViewFrm.ActionStayOnTopUpdate(Sender: TObject);
var
  AAction: TAction;
begin
  AAction := Sender as TAction;
  AAction.Checked := FormStyle = fsStayOnTop;
  AAction.Enabled := True;
end;

procedure TJclSIMDViewFrm.ActionStayOnTopExecute(Sender: TObject);
begin
  if FormStyle = fsStayOnTop then
    FormStyle := fsNormal
  else
    FormStyle := fsStayOnTop;
end;

procedure TJclSIMDViewFrm.ActionModifyUpdate(Sender: TObject);
var
  AProcess: IOTAProcess;
  AThread: IOTAThread;
  AItemIndex: Integer;
begin
  AProcess := DebuggerServices.CurrentProcess;
  AThread := nil;
  AItemIndex := ListBoxRegs.ItemIndex;
  if NbXMMRegister > 0 then
    Inc(AItemIndex);

  if Assigned(AProcess) then
    AThread := AProcess.CurrentThread;

  (Sender as TAction).Enabled := Assigned(AThread) and (AThread.State = tsStopped) and
    (AItemIndex >= 0) and (AItemIndex < (NbMMRegister + NbXMMRegister));
end;

procedure TJclSIMDViewFrm.ActionModifyExecute(Sender: TObject);
var
  AItemIndex: Integer;
begin
  AItemIndex := ListBoxRegs.ItemIndex;
  if AItemIndex >= 0 then
  try
    FModifyForm := TJclSIMDModifyFrm.Create(Self, DebuggerServices, FExpert);
    FModifyForm.Icon.Assign(Self.Icon);

    if AItemIndex < NbMMRegister then
    begin
      FModifyForm.Caption := SysUtils.Format(RsModifyMM, [AItemIndex]);
      if FModifyForm.Execute(DebuggerServices.CurrentProcess.CurrentThread, Display,
        Format, FVectorFrame.FPURegisters[AItemIndex].Data.MMRegister ,FCpuInfo) then
      begin
        FVectorFrame.FPURegisters[AItemIndex].Data.Reserved := $FFFF;
        FVectorFrame.FTW := FVectorFrame.FTW or (1 shl AItemIndex);
        SetThreadValues;
        GetThreadValues;
        FRegisterChanged[AItemIndex] := True;
        ListBoxRegs.Invalidate;
      end;
    end else
    begin
      if CpuInfo.Is64Bits then
        FModifyForm.Caption := SysUtils.Format(RsModifyXMM2, [AItemIndex - NbMMRegister])
      else
        FModifyForm.Caption := SysUtils.Format(RsModifyXMM1, [AItemIndex - NbMMRegister]);
      if FModifyForm.Execute(DebuggerServices.CurrentProcess.CurrentThread, Display,
        Format, FVectorFrame.XMMRegisters.LongXMM[AItemIndex - NbMMRegister], FCpuInfo) then
      begin
        SetThreadValues;
        GetThreadValues;
        FRegisterChanged[AItemIndex] := True;
        ListBoxRegs.Invalidate;
      end;
    end;
  finally
    FreeAndNil(FModifyForm);
  end;
end;

procedure TJclSIMDViewFrm.ActionEmptyUpdate(Sender: TObject);
var
  AProcess: IOTAProcess;
  AThread: IOTAThread;
  AItemIndex: Integer;
begin
  AProcess := DebuggerServices.CurrentProcess;
  AThread := nil;
  AItemIndex := ListBoxRegs.ItemIndex;
  if Assigned(AProcess) then
    AThread := AProcess.CurrentThread;
  (Sender as TAction).Enabled := Assigned(AThread) and (AThread.State = tsStopped) and
    (AItemIndex >= 0) and (AItemIndex < NbMMRegister) and
    ((FVectorFrame.FTW and (1 shl AItemIndex)) <> 0) and
    (FVectorFrame.FPURegisters[AItemIndex].Data.Reserved = $FFFF);
end;

procedure TJclSIMDViewFrm.ActionEmptyExecute(Sender: TObject);
var
  AItemIndex: Integer;
begin
  AItemIndex := ListBoxRegs.ItemIndex;
  FVectorFrame.FTW := FVectorFrame.FTW and not (1 shl AItemIndex);
  FVectorFrame.FPURegisters[AItemIndex].Data.FloatValue := 0.0;
  SetThreadValues;
  GetThreadValues;
  FRegisterChanged[AItemIndex] := True;
end;

procedure TJclSIMDViewFrm.ActionEmptyAllUpdate(Sender: TObject);
var
  AProcess: IOTAProcess;
  AThread: IOTAThread;
  AItemIndex: Integer;
begin
  AProcess := DebuggerServices.CurrentProcess;
  AThread := nil;
  AItemIndex := ListBoxRegs.ItemIndex;
  if Assigned(AProcess) then
    AThread := AProcess.CurrentThread;
  (Sender as TAction).Enabled := (AItemIndex >= 0) and (AItemIndex < NbMMRegister) and
    Assigned(AThread) and (AThread.State = tsStopped);
end;

procedure TJclSIMDViewFrm.ActionEmptyAllExecute(Sender: TObject);
var
  Index: Integer;
begin
  FVectorFrame.FTW := 0;
  for Index := Low(FVectorFrame.FPURegisters) to High(FVectorFrame.FPURegisters) do
    FVectorFrame.FPURegisters[Index].Data.FloatValue := 0.0;
  SetThreadValues;
  GetThreadValues;
end;

procedure TJclSIMDViewFrm.ActionComplementUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ListBoxMXCSR.ItemIndex >= 0;
end;

procedure TJclSIMDViewFrm.ActionComplementExecute(Sender: TObject);
var
  BitValue: Cardinal;
  OldMXCSRValue: Cardinal;
begin
  if ListBoxMXCSR.ItemIndex >= 0 then
    with MXCSRBitsDescriptions[ListBoxMXCSR.ItemIndex] do
  begin
    OldMXCSRValue := FVectorFrame.MXCSR;
    BitValue := (Cardinal(FVectorFrame.MXCSR) and AndMask) shr Shifting;
    Inc(BitValue);
    FVectorFrame.MXCSR := (FVectorFrame.MXCSR and (not AndMask)) or ((BitValue shl Shifting) and AndMask);
    SetThreadValues;
    FVectorFrame.MXCSR := OldMXCSRValue;
    GetThreadValues;
  end;
end;

procedure TJclSIMDViewFrm.ListBoxesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AListBox: TListBox;
begin
  if Button = mbRight then
  begin
    AListBox := Sender as TListBox;
    AListBox.ItemIndex := AListBox.ItemAtPos(Point(X, Y), True);
  end;
end;

// History:

// $Log$
// Revision 1.6  2005/11/21 21:25:40  outchy
// Modified the get/set methods of thread context for Delphi 2005
//
// Revision 1.5  2005/10/26 03:29:44  rrossmair
// - improved header information, added Date and Log CVS tags.
//

end.
