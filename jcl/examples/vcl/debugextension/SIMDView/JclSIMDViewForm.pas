{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSIMDViewForm.pas, released on 2004-10-11.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JclSIMDViewForm;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ToolsApi, Grids, ExtCtrls, Menus, JclSysInfo, JclSIMDUtils,
  JclSIMDModifyForm;

type
  TJclSIMDViewFrm = class(TForm)
    Splitter: TSplitter;
    ListBoxRegs: TListBox;            // ListBoxRegs.Items.Objetcs         Bit 0 : Changed
    ListBoxMXCSR: TListBox;           // ListBoxMXCSR.Items.Objects        Bits 0 to 7 : BitId (TJclMXCSRBits)
                                      //                                   Bit 8 : Changed
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
    procedure ListBoxMXCSRDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ListBoxMXCSRMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ListBoxRegsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ListBoxMXCSRMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItemComplementClick(Sender: TObject);
    procedure MenuItemFormatClick(Sender: TObject);
    procedure MenuItemDisplayClick(Sender: TObject);
    procedure MenuItemStayOnTopClick(Sender: TObject);
    procedure ListBoxRegsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItemModifyClick(Sender: TObject);
    procedure MenuItemCpuInfoClick(Sender: TObject);
    procedure PopupMenuRegsPopup(Sender: TObject);
  private
    FDebugServices:IOTADebuggerServices;
    FVectorFrame:TJclVectorFrame;
    FDisplay:TJclXMMContentType;
    FFormat:TJclSIMDFormat;
    FCpuInfo: TCpuInfo;
    FCpuInfoValid: Boolean;
    FSSECaption: string;
    FOldThreadID: Cardinal;
    FModifyForm: TJclSIMDModifyFrm;
    procedure SetDisplay(const Value: TJclXMMContentType);
    procedure SetFormat(const Value: TJclSIMDFormat);
  protected
    procedure DoClose(var Action: TCloseAction); override;
    procedure UpdateActions; override;
  public
    constructor Create (AOwner:TComponent); override;
    procedure ThreadEvaluate(const ExprStr, ResultStr: string;
      ReturnCode: Integer);
    destructor Destroy; override;
    procedure SetThreadValues;
    procedure GetThreadValues;
    procedure FormatRegs;
    function CpuInfo: TCpuInfo;
    property Format:TJclSIMDFormat read FFormat write SetFormat;
    property Display:TJclXMMContentType read FDisplay write SetDisplay;
    property DebugServices: IOTADebuggerServices read FDebugServices;
    property SSECaption: string read FSSECaption write FSSECaption;
  end;

implementation

uses
  JclSIMDCpuInfo;

{$R *.dfm}

constructor TJclSIMDViewFrm.Create(AOwner: TComponent);
var
  I:TJclMXCSRBits;
  J:Integer;
begin
  inherited Create(AOwner);
  FCpuInfoValid := False;
  FOldThreadID := 0;
  Assert(Supports(BorlandIDEServices,IOTADebuggerServices,FDebugServices),
    'Unable to get Borland Debug Services');
  ListBoxMXCSR.Items.Clear;
  with CpuInfo do
    for I:=mbInvalidOperationException to mbFlushToZero do
      if (MXCSRBitsDescription[I].BitCount<>0) then
      ListBoxMXCSR.Items.AddObject('0',TObject(I));
  ListBoxRegs.Items.Clear;
  if CpuInfo.Is64Bits
    then J := 16
    else J := 8;
  while (J>-1) do   // 16 128-bit registers + 1 cardinal (MXCSR)
  begin
    ListBoxRegs.Items.InsertObject(0,'0',nil);
    Dec(J);
  end;
  MenuItemBinary.Tag:=Integer(sfBinary);
  MenuItemSigned.Tag:=Integer(sfSigned);
  MenuItemUnsigned.Tag:=Integer(sfUnsigned);
  MenuItemHexa.Tag:=Integer(sfHexa);
  MenuItemBytes.Tag:=Integer(xt16Bytes);
  MenuItemWords.Tag:=Integer(xt8Words);
  MenuItemDWords.Tag:=Integer(xt4DWords);
  MenuItemQWords.Tag:=Integer(xt2QWords);
  MenuItemSingles.Tag:=Integer(xt4Singles);
  MenuItemDoubles.Tag:=Integer(xt2Doubles);
  Format:=sfHexa;
  Display:=xt8Words;
  GetThreadValues;
end;

destructor TJclSIMDViewFrm.Destroy;
begin
  inherited Destroy;
end;

function TJclSIMDViewFrm.CpuInfo: TCpuInfo;
begin
  if not FCpuInfoValid then
  begin
    JclSysInfo.GetCpuInfo(FCpuInfo);
    FCpuInfoValid := True;
  end;
  Result := FCpuInfo;
end;

procedure TJclSIMDViewFrm.ListBoxMXCSRDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  BitId:TJclMXCSRBits;
begin
  with (Control as TListBox),Canvas do
  begin
    if not (odFocused in State) then
    begin
      Pen.Color:=Brush.Color;
      if (odSelected in State)
        then Font.Color:=clWindow;
    end;
    Rectangle(Rect);
    BitId:=TJclMXCSRBits(Cardinal(Items.Objects[Index]) and $FF);
    TextOut(Rect.Left+2,Rect.Top,MXCSRBitsDescription[BitId].ShortName);
    if ((Cardinal(Items.Objects[Index]) and $100)<>0)
      then Font.Color:=clRed;
    TextOut(Rect.Left+2+TextExtent(MXCSRBitsDescription[BitId].ShortName).cx,Rect.Top,
            Items[Index]);
  end;
end;

procedure TJclSIMDViewFrm.ListBoxMXCSRMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  AIndex:Integer;
  AText:string;
  BitId:TJclMXCSRBits;
begin
  if (Shift<>[])
    then Application.HideHint
    else with (Sender as TListBox) do
  begin
    AIndex:=ItemAtPos(Point(X,Y),True);
    if (AIndex>=0) and (AIndex<Items.Count) then
    begin
      BitId:=TJclMXCSRBits(Cardinal(Items.Objects[AIndex]) and $FF);
      with MXCSRBitsDescription[BitId] do
      begin
        AText := LongName;
        if BitCount=2 then
          AText := SysUtils.Format('%s (%s)',[AText,Names[(Cardinal(FVectorFrame.MXCSR) shr 13) and 3]]);
        if (AText<>Hint) then
        begin
          Hint:=AText;
          Application.HideHint;
          Application.ActivateHint(Point(X,Y));
        end;
      end;
    end
    else
    begin
      Hint:='';
      Application.HideHint;
    end;
  end;
end;

procedure TJclSIMDViewFrm.ListBoxRegsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  AText:string;
begin
  with (Control as TListBox),Canvas do
  begin
    if not (odFocused in State) then
    begin
      Pen.Color:=Brush.Color;
      if (odSelected in State)
        then Font.Color:=clWindow;
    end;
    Rectangle(Rect);
    if Index>0 then
      AText:='XMM'+SysUtils.Format('%.2d',[Index-1])
    else
      AText:='MXCSR ';
    TextOut(Rect.Left+2,Rect.Top,AText);
    if Boolean(Items.Objects[Index])
      then Font.Color:=clRed;
    TextOut(Rect.Left+2+TextExtent(AText).cx,Rect.Top,
            Items[Index]);
  end;
end;

procedure TJclSIMDViewFrm.ListBoxMXCSRMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AIndex: Integer;
begin
  if (Button=mbRight) then
    with (Sender as TListBox) do
  begin
    AIndex:=ItemAtPos(Point(X,Y),True);
    if (AIndex>-1) then
    begin
      ItemIndex:=AIndex;
      with ClientToScreen(Point(X,Y)) do
        PopupMenu.Popup(X,Y);
    end;
  end;
end;

procedure TJclSIMDViewFrm.ListBoxRegsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button=mbRight) then
    with (Sender as TListBox) do
  begin
    MenuItemModify.Enabled := ItemAtPos(Point(X,Y),True)>0;
    with ClientToScreen(Point(X,Y)) do
      PopupMenu.Popup(X,Y);
  end;
end;

procedure TJclSIMDViewFrm.MenuItemComplementClick(Sender: TObject);
var
  AndMask:Cardinal;
  BitValue:Cardinal;
  BitId:TJclMXCSRBits;
begin
  with ListBoxMXCSR do
  begin
    BitId:=TJclMXCSRBits(Cardinal(Items.Objects[ItemIndex]) and $FF);
    with MXCSRBitsDescription[BitId], FVectorFrame do
    begin
      AndMask:=((1 shl BitCount)-1) shl Cardinal(BitId);
      BitValue:=(Cardinal(MXCSR) and AndMask) shr Cardinal(BitId);
      Inc(BitValue);
      MXCSR:=TJclMXCSR(   (Cardinal(MXCSR) and (not AndMask))
                       or ((BitValue shl Cardinal(BitId)) and AndMask) );
    end;
  end;
  SetThreadValues;
end;

procedure TJclSIMDViewFrm.GetThreadValues;
  function MakeFlag(Value:Boolean):TObject; overload;
  begin
       if (Value)
         then Result:=TObject(1)
         else Result:=nil;
  end;
  function MakeFlag(Value1:TJclXMMRegister;
    Value2:TJclXMMRegister):TObject; overload;
  begin
       Result:=MakeFlag(   (Value1.QWords[0]<>Value2.QWords[0])
                        or (Value1.QWords[1]<>Value2.QWords[1]));
  end;
var
  NewVectorFrame:TJclVectorFrame;
  AndMask:Cardinal;
  NewBitValue:Cardinal;
  OldBitValue:Cardinal;
  Index:Integer;
  BitId:TJclMXCSRBits;
  AProcess: IOTAProcess;
  AThread: IOTAThread;
begin
  AProcess := nil;
  AThread := nil;
  if (DebugServices.ProcessCount >0) then
    AProcess := DebugServices.CurrentProcess;
  if (AProcess <> nil) and (AProcess.ThreadCount > 0) then
    AThread := AProcess.CurrentThread;
  if (AThread = nil) or (AThread.State = tsNone) or (AThread.GetOSThreadID = 0)
    or (AThread.Handle = 0) then
  begin
    Close;
    Exit;
  end;

  if (AThread.State = tsRunnable) then
  begin
    Caption := SysUtils.Format('%s <running>',[SSECaption]);
    Exit;
  end;

  if (DebugServices.CurrentProcess.ThreadCount > 1) then
    Caption := SysUtils.Format('%s Thread : %d',[SSECaption,AThread.GetOSThreadID])
  else Caption := SSECaption;
  GetVectorContext(AThread.Handle,NewVectorFrame);
  for Index:=0 to ListBoxMXCSR.Items.Count-1 do
    with ListBoxMXCSR, Items do
  begin
    BitId:=TJclMXCSRBits(Cardinal(Objects[Index]) and $FF);
    with MXCSRBitsDescription[BitId] do
    begin
      AndMask:=((1 shl BitCount)-1) shl Cardinal(BitId);
      NewBitValue:=(Cardinal(NewVectorFrame.MXCSR) and AndMask) shr Cardinal(BitId);
      OldBitValue:=(Cardinal(FVectorFrame.MXCSR) and AndMask) shr Cardinal(BitId);
      if (NewBitValue<>OldBitValue) then
        Objects[Index]:=TObject(Cardinal(Objects[Index]) or $100)
      else
        Objects[Index]:=TObject(Cardinal(Objects[Index]) and (not $100));
      Strings[Index]:=IntToStr(NewBitValue);
    end;
  end;
  ListBoxMXCSR.Invalidate;
  ListBoxRegs.Items.Objects[0]:=MakeFlag(NewVectorFrame.MXCSR<>FVectorFrame.MXCSR);
  ListBoxRegs.Items.Strings[0]:=IntToHex(Cardinal(NewVectorFrame.MXCSR),8);
  if CpuInfo.Is64Bits
    then Index:=16
    else Index:=8;
  while (Index>0) do
  begin
    ListBoxRegs.Items.Objects[Index]:=MakeFlag(NewVectorFrame.XMMRegisters.LongXMM[Index-1],
                                               FVectorFrame.XMMRegisters.LongXMM[Index-1]);
    Dec(Index);
  end;
  FVectorFrame:=NewVectorFrame;
  FormatRegs;
end;

procedure TJclSIMDViewFrm.SetThreadValues;
begin
  SetVectorContext(DebugServices.CurrentProcess.CurrentThread.Handle,FVectorFrame);
  GetThreadValues;
end;

procedure TJclSIMDViewFrm.FormatRegs;
  function FormatReg(AReg:TJclXMMRegister):string;
  var
    I:Integer;
    Value: TJclSIMDValue;
  begin
    Result:='';
    Value.Display := Display;
    case Display of
      xt16Bytes  : for I:=High(AReg.Bytes) downto Low(AReg.Bytes) do
                   begin
                     Value.ValueByte := AReg.Bytes[I];
                     Result:=Result+' '+FormatValue(Value,Format);
                   end;
      xt8Words   : for I:=High(AReg.Words) downto Low(AReg.Words) do
                   begin
                     Value.ValueWord := AReg.Words[I];
                     Result:=Result+' '+FormatValue(Value,Format);
                   end;
      xt4DWords  : for I:=High(AReg.DWords) downto Low(AReg.DWords) do
                   begin
                     Value.ValueDWord := AReg.DWords[I];
                     Result:=Result+' '+FormatValue(Value,Format);
                   end;
      xt2QWords  : for I:=High(AReg.QWords) downto Low(AReg.QWords) do
                   begin
                     Value.ValueQWord := AReg.QWords[I];
                     Result:=Result+' '+FormatValue(Value,Format);
                   end;
      xt4Singles : for I:=High(AReg.Singles) downto Low(AReg.Singles) do
                   begin
                     Value.ValueSingle := AReg.Singles[I];
                     Result:=Result+' '+FormatValue(Value,sfBinary);
                   end;
      xt2Doubles : for I:=High(AReg.Doubles) downto Low(AReg.Doubles) do
                   begin
                     Value.ValueDouble := AReg.Doubles[I];
                     Result:=Result+' '+FormatValue(Value,sfBinary);
                   end;
    end;
  end;
var
  Index:Integer;
begin
  if CpuInfo.Is64Bits
    then Index:=16
    else Index:=8;
  while (Index>0) do
  begin
    ListBoxRegs.Items.Strings[Index]:=FormatReg(FVectorFrame.XMMRegisters.LongXMM[Index-1]);
    Dec(Index);
  end;
  ListBoxMXCSR.Invalidate;
end;

procedure TJclSIMDViewFrm.MenuItemFormatClick(Sender: TObject);
begin
  Format:=TJclSIMDFormat((Sender as TMenuItem).Tag);
end;

procedure TJclSIMDViewFrm.SetDisplay(const Value: TJclXMMContentType);
var
  AEnabled:Boolean;
begin
  FDisplay := Value;
  MenuItemBytes.Checked := Value=xt16Bytes;
  MenuItemWords.Checked := Value=xt8Words;
  MenuItemDWords.Checked := Value=xt4DWords;
  MenuItemQWords.Checked := Value=xt2QWords;
  MenuItemSingles.Checked := Value=xt4Singles;
  MenuItemDoubles.Checked := Value=xt2Doubles;

  AEnabled:=not (Value in [xt4Singles, xt2Doubles]);
  MenuItemBinary.Enabled := AEnabled;
  MenuItemSigned.Enabled := AEnabled;
  MenuItemUnsigned.Enabled := AEnabled;
  MenuItemHexa.Enabled := AEnabled;

  FormatRegs;
end;

procedure TJclSIMDViewFrm.SetFormat(const Value: TJclSIMDFormat);
begin
  FFormat := Value;
  MenuItemBinary.Checked:=Value=sfBinary;
  MenuItemSigned.Checked:=Value=sfSigned;
  MenuItemUnsigned.Checked:=Value=sfUnsigned;
  MenuItemHexa.Checked:=Value=sfHexa;
  FormatRegs;
end;

procedure TJclSIMDViewFrm.MenuItemDisplayClick(Sender: TObject);
begin
  Display:=TJclXMMContentType((Sender as TMenuItem).Tag);
end;

procedure TJclSIMDViewFrm.DoClose(var Action: TCloseAction);
begin
  Action:=caFree;
end;

procedure TJclSIMDViewFrm.MenuItemStayOnTopClick(Sender: TObject);
begin
  with (Sender as TMenuItem) do
  begin
    Checked:=not Checked;
    if (Checked)
      then FormStyle:=fsStayOnTop
      else FormStyle:=fsNormal;
  end;
end;

procedure TJclSIMDViewFrm.MenuItemModifyClick(Sender: TObject);
begin
  if ListBoxRegs.ItemIndex>0 then
  try
    FModifyForm := TJclSIMDModifyFrm.Create(Self);
    FModifyForm.Icon.Assign(Self.Icon);
    if (FModifyForm.Execute(DebugServices.CurrentProcess.CurrentThread,Display,
      Format,FVectorFrame.XMMRegisters.LongXMM[ListBoxRegs.ItemIndex-1],FCpuInfo))
      then SetThreadValues;
  finally
    FreeAndNil(FModifyForm);
  end;
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

procedure TJclSIMDViewFrm.PopupMenuRegsPopup(Sender: TObject);
begin
  MenuItemModify.Enabled := ListBoxRegs.ItemIndex>0;
end;

procedure TJclSIMDViewFrm.UpdateActions;
var
  CurrentThreadID: Cardinal;
  AProcess: IOTAProcess;
  AThread: IOTAThread;
begin
  inherited UpdateActions;
  CurrentThreadID := 0;
  AProcess := nil;
  AThread := nil;
  if (DebugServices.ProcessCount > 0) then
    AProcess := DebugServices.CurrentProcess;
  if (AProcess <> nil) and (AProcess.ThreadCount > 0) then
    AThread := AProcess.CurrentThread;
  if (AThread <> nil) and (AThread.State in [tsStopped, tsBlocked]) then
    CurrentThreadID := AThread.GetOSThreadID;
  if (CurrentThreadID <> 0) and (CurrentThreadID <> FOldThreadID) then
    GetThreadValues;
  FOldThreadID := CurrentThreadID;
end;

procedure TJclSIMDViewFrm.ThreadEvaluate(const ExprStr, ResultStr: string;
  ReturnCode: Integer);
begin
  if Assigned(FModifyForm) then
    FModifyForm.ThreadEvaluate(ExprStr,ResultStr,ReturnCode);
end;

end.
