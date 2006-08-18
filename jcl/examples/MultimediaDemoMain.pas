unit MultimediaDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, MMSystem, JclMultimedia;

type
  TMainForm = class(TForm)
    PageControl: TPageControl;
    TabSheet1: TTabSheet;
    Label2: TLabel;
    OpenDriveBtn: TButton;
    CloseDriveBtn: TButton;
    MediaPresentBtn: TButton;
    AudioInfoBtn: TButton;
    AudioInfoMemo: TMemo;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    DriveComboBox: TComboBox;
    DefaultDriveCheckBox: TCheckBox;
    TabSheet2: TTabSheet;
    MixerTreeView: TTreeView;
    Label3: TLabel;
    MixerDetailListView: TListView;
    Label4: TLabel;
    GroupBox2: TGroupBox;
    SpeakersMuteCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure OpenDriveBtnClick(Sender: TObject);
    procedure CloseDriveBtnClick(Sender: TObject);
    procedure MediaPresentBtnClick(Sender: TObject);
    procedure AudioInfoBtnClick(Sender: TObject);
    procedure DefaultDriveCheckBoxClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MixerTreeViewCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure MixerTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure SpeakersMuteCheckBoxClick(Sender: TObject);
  private
    FMixer: TJclMixer;
    procedure BuildDrives;
    procedure BuildMixerTree;
    function GetSelectedDrive: Char;
    procedure UpdateMixerDetails(MixerObject: TObject);
    procedure UpdateMixerControl(MixerHandle: HMIXER; ControlID: DWORD);
    procedure UpdateMixerLine(MixerHandle: HMIXER; LineID: DWORD);
    procedure UpdateSelectedMixerInfo;
    procedure UpdateMixerSpeakerControls;
    procedure WMMmMixmControlChange(var Message: TMessage); message MM_MIXM_CONTROL_CHANGE;
    procedure WMMmMixmLineChange(var Message: TMessage); message MM_MIXM_LINE_CHANGE;
    function GetSelectedMixerTreeObject: TObject;
  public
    property SelectedDrive: Char read GetSelectedDrive;
    property SelectedMixerTreeObject: TObject read GetSelectedMixerTreeObject; 
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

uses
  JclFileUtils, JclStrings, JclSysUtils;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FMixer := TJclMixer.Create(Handle);
  BuildDrives;
  BuildMixerTree;
  UpdateMixerSpeakerControls;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMixer);
end;

//==================================================================================================
// CD audio
//==================================================================================================

procedure TMainForm.BuildDrives;
var
  D: Char;
  DriveStr: string;
begin
  for D := 'A' to 'Z' do
  begin
    DriveStr := D + ':\';
    if GetDriveType(PChar(DriveStr)) = DRIVE_CDROM then
      DriveComboBox.Items.Add(D);
  end;
  if DriveComboBox.Items.Count > 0 then
    DriveComboBox.ItemIndex := 0;
end;

procedure TMainForm.DefaultDriveCheckBoxClick(Sender: TObject);
begin
  DriveComboBox.Enabled := not DefaultDriveCheckBox.Checked;
end;

function TMainForm.GetSelectedDrive: Char;
begin
  if DefaultDriveCheckBox.Checked then
    Result := #0
  else
    Result := DriveComboBox.Text[1];
end;

procedure TMainForm.OpenDriveBtnClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    OpenCloseCdDrive(True, SelectedDrive);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.CloseDriveBtnClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    OpenCloseCdDrive(False, SelectedDrive);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.MediaPresentBtnClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    ShowMessage(BooleanToStr(IsMediaPresentInDrive(SelectedDrive)));
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.AudioInfoBtnClick(Sender: TObject);
var
  TotalTimeStr: string;
begin
  Screen.Cursor := crHourGlass;
  AudioInfoMemo.Lines.BeginUpdate;
  try
    AudioInfoMemo.Lines.Add('Product                : ' + GetCdInfo(miProduct, SelectedDrive));
    AudioInfoMemo.Lines.Add('Identity               : ' + GetCdInfo(miIdentity, SelectedDrive));
    AudioInfoMemo.Lines.Add('Universal Product Code : ' + GetCdInfo(miUPC, SelectedDrive));
    TotalTimeStr := GetCDAudioTrackList(AudioInfoMemo.Lines, True, SelectedDrive);
    AudioInfoMemo.Lines.Add('Total time:       ' + TotalTimeStr);
  finally
    AudioInfoMemo.Lines.EndUpdate;
    Screen.Cursor := crDefault;
  end;
  AudioInfoMemo.Lines.Add('');
end;

//==================================================================================================
// Audio mixer
//==================================================================================================

procedure TMainForm.BuildMixerTree;
var
  DeviceIndex, DestionationIndex, SourceIndex, LineControlIndex: Integer;
  DeviceNode, DestionationNode, SourceNode: TTreeNode;
  Device: TJclMixerDevice;
  Destination: TJclMixerDestination;
  SourceLine: TJclMixerSource;
  LineControl: TJclMixerLineControl;
begin
  with MixerTreeView do
  begin
    Items.BeginUpdate;
    Screen.Cursor := crHourGlass;
    try
      Items.Clear;
      for DeviceIndex := 0 to FMixer.DeviceCount - 1 do
      begin
        Device := FMixer.Devices[DeviceIndex];
        DeviceNode := Items.AddChildObjectFirst(nil, Device.ProductName, Device);

        for DestionationIndex := 0 to Device.DestinationCount - 1 do
        begin
          Destination := Device.Destinations[DestionationIndex];
          DestionationNode := Items.AddChildObjectFirst(DeviceNode, Destination.Name, Destination);

          for LineControlIndex := 0 to Destination.LineControlCount - 1 do
          begin
            LineControl := Destination.LineControls[LineControlIndex];
            Items.AddChildObjectFirst(DestionationNode, LineControl.Name, LineControl);
          end;

          for SourceIndex := 0 to Destination.SourceCount - 1 do
          begin
            SourceLine := Destination.Sources[SourceIndex];
            SourceNode := Items.AddChildObjectFirst(DestionationNode, SourceLine.Name, SourceLine);

            for LineControlIndex := 0 to SourceLine.LineControlCount - 1 do
            begin
              LineControl := SourceLine.LineControls[LineControlIndex];
              Items.AddChildObjectFirst(SourceNode, LineControl.Name, LineControl);
            end;

          end;
        end;
      end;
      FullExpand;
      if Items.Count > 0 then
      begin
        Selected := Items.GetFirstNode;
        Selected.MakeVisible;
      end;
    finally
      Items.EndUpdate;
      Screen.Cursor := crDefault;
    end;
  end;
end;

function TMainForm.GetSelectedMixerTreeObject: TObject;
begin
  if MixerTreeView.Selected <> nil then
    Result := TObject(MixerTreeView.Selected.Data)
  else
    Result := nil;  
end;

procedure TMainForm.UpdateMixerDetails(MixerObject: TObject);

  procedure AddLine(const ItemName, Value: string);
  begin
    with MixerDetailListView.Items.Add do
    begin
      Caption := ItemName;
      SubItems.Add(Value);
    end;
  end;

  procedure BuildMixerDeviceDetails(Device: TJclMixerDevice);
  begin
    with Device do
    begin
      AddLine('Handle', IntToHex(Handle, 8));
      AddLine('Mid', IntToHex(Capabilities.wMid, 4));
      AddLine('Pid', IntToHex(Capabilities.wPid, 4));
      with WordRec(LongRec(Capabilities.vDriverVersion).Lo) do
        AddLine('Driver version', FormatVersionString(Hi, Lo));
      AddLine('Support', IntToHex(Capabilities.fdwSupport, 8));
    end;
  end;

  procedure BuildMixerLineDetails(Line: TJclMixerLine);
  begin
    with Line do
    begin
      AddLine('Component type', ComponentString);
      AddLine('ID', IntToHex(ID, 8));
      AddLine('Channels', IntToStr(LineInfo.cChannels));
      AddLine('Connections', IntToStr(LineInfo.cConnections));
      AddLine('Target name', LineInfo.Target.szPname);
    end;
  end;

  procedure BuildMixerDestinationDetails(Destination: TJclMixerDestination);
  begin
    BuildMixerLineDetails(Destination);
  end;

  procedure BuildMixerSourceDetails(Source: TJclMixerSource);
  begin
    BuildMixerLineDetails(Source);
  end;

  procedure BuildMixerLineControlDetails(LineControl: TJclMixerLineControl);
  begin
    with LineControl do
    begin
      AddLine('ID', IntToHex(ControlInfo.dwControlID, 8));
      AddLine('Control type', IntToHex(ControlInfo.dwControlType, 8));
      AddLine('Disabled', BooleanToStr(IsDisabled));
      AddLine('List', BooleanToStr(IsList));
      AddLine('Multiple', BooleanToStr(IsMultiple));
      AddLine('Uniform', BooleanToStr(IsUniform));
      AddLine('Multiple items', IntToHex(ControlInfo.cMultipleItems, 8));
      if not IsMultiple then
        AddLine('Uniform value', IntToHex(UniformValue, 8));
      AddLine('Minimum', IntToHex(ControlInfo.Bounds.lMinimum, 8));
      AddLine('Maximum', IntToHex(ControlInfo.Bounds.lMaximum, 8));
      AddLine('Steps', IntToHex(ControlInfo.Metrics.cSteps, 8));
      AddLine('Value', ValueString);
      AddLine('List text', ListText.CommaText);
    end;
  end;

begin
  with MixerDetailListView do
  begin
    Items.BeginUpdate;
    try
      Items.Clear;
      if MixerObject is TJclMixerDevice then
        BuildMixerDeviceDetails(TJclMixerDevice(MixerObject))
      else
      if MixerObject is TJclMixerDestination then
        BuildMixerDestinationDetails(TJclMixerDestination(MixerObject))
      else
      if MixerObject is TJclMixerSource then
        BuildMixerSourceDetails(TJclMixerSource(MixerObject))
      else
      if MixerObject is TJclMixerLineControl then
        BuildMixerLineControlDetails(TJclMixerLineControl(MixerObject));
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TMainForm.UpdateMixerControl(MixerHandle: HMIXER; ControlID: DWORD);
var
  Control: TJclMixerLineControl;
begin
  Control := FMixer.LineControlByID[MixerHandle, ControlID];
  if Control <> nil then
  begin
    if Control = SelectedMixerTreeObject then
      UpdateSelectedMixerInfo;
  end;
end;

procedure TMainForm.UpdateMixerLine(MixerHandle: HMIXER; LineID: DWORD);
var
  Line: TJclMixerLine;
begin
  Line := FMixer.LineByID[MixerHandle, LineID];
  if Line <> nil then
  begin
    if Line = SelectedMixerTreeObject then
      UpdateSelectedMixerInfo;
    if Line.LineInfo.dwComponentType = MIXERLINE_COMPONENTTYPE_DST_SPEAKERS then
      UpdateMixerSpeakerControls;
  end;
end;

procedure TMainForm.UpdateSelectedMixerInfo;
begin
  UpdateMixerDetails(SelectedMixerTreeObject);
end;

procedure TMainForm.UpdateMixerSpeakerControls;
begin
  SpeakersMuteCheckBox.Checked := FMixer.SpeakersMute;
end;

procedure TMainForm.MixerTreeViewCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  NodeObject: TObject;
begin
  NodeObject := TObject(Node.Data);
  if NodeObject is TJclMixerDevice then
    Sender.Canvas.Font.Style := [fsBold]
  else
  if NodeObject is TJclMixerDestination then
  begin
    Sender.Canvas.Font.Style := [fsBold];
    if not (cdsFocused in State) then
      Sender.Canvas.Font.Color := clRed;
  end
  else
  if NodeObject is TJclMixerSource then
  begin
    Sender.Canvas.Font.Style := [fsBold];
    if not (cdsFocused in State) then
      Sender.Canvas.Font.Color := clBlue;
  end;
end;

procedure TMainForm.MixerTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  UpdateMixerDetails(TObject(Node.Data));
end;

procedure TMainForm.SpeakersMuteCheckBoxClick(Sender: TObject);
begin
  FMixer.SpeakersMute := SpeakersMuteCheckBox.Checked;
end;

procedure TMainForm.WMMmMixmControlChange(var Message: TMessage);
begin
  UpdateMixerControl(Message.WParam, Message.LParam);
end;

procedure TMainForm.WMMmMixmLineChange(var Message: TMessage);
begin
  UpdateMixerLine(Message.WParam, Message.LParam);
end;

end.
