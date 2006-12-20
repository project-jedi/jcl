{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) extension                                                        }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JediInstallerMain.pas.                                                      }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) of Petr Vones. All Rights Reserved.                                                }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Andreas Hausladen (ahuser)                                                                     }
{   Robert Rossmair (rrossmair) - crossplatform & BCB support, refactoring                         }
{   Florent Ouchet (outchy) - New installer core                                                   }
{                                                                                                  }
{**************************************************************************************************}

// $Id$

{$IFNDEF PROTOTYPE}
{$IFDEF VCL}
unit JediGUIInstall;
{$ELSE VisualCLX}
unit QJediGUIInstall;
{$ENDIF VisualCLX}
{$ENDIF ~PROTOTYPE}

{$I jcl.inc}
{$I crossplatform.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF VisualCLX}
  Types,
  QGraphics, QForms, QControls, QStdCtrls, QComCtrls, QExtCtrls,
  {$ELSE}
  Graphics, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, FrmCompile,
  {$ENDIF}
  JclBorlandTools, JediInstall;

type
  TInstallFrame = class(TFrame, IJediInstallPage, IJediPage)
    ComponentsTreePanel: TPanel;
    Label1: TLabel;
    TreeView: TTreeView;
    Splitter: TSplitter;
    InfoPanel: TPanel;
    Label2: TLabel;
    {$IFDEF VisualCLX}
    InfoDisplay: TMemo;
    {$ELSE VCL}
    InfoDisplay: TRichEdit;
    {$ENDIF VCL}
    OptionsGroupBox: TGroupBox;
    ProgressBar: TProgressBar;
    procedure SplitterCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeViewKeyPress(Sender: TObject; var Key: Char);
    {$IFDEF VisualCLX}
    procedure TreeViewCustomDrawItem(Sender: TCustomViewControl; Item: TCustomViewItem;
      Canvas: TCanvas; const Rect: TRect; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    {$ELSE}
    procedure TreeViewCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    {$ENDIF}
  private
    FNodeData: TList;
    FDirectories: TList;
    FCheckedCount: Integer;
    FInstallCount: Integer;
    FInstalling: Boolean;
    {$IFDEF VCL}
    FFormCompile: TFormCompile;
    function GetFormCompile: TFormCompile;
    {$ENDIF VCL}
    function GetNodeChecked(Node: TTreeNode): Boolean;
    function IsAutoChecked(Node: TTreeNode): Boolean;
    function IsRadioButton(Node: TTreeNode): Boolean;
    function IsStandAloneParent(Node: TTreeNode): Boolean;
    function IsExpandable(Node: TTreeNode): Boolean;
    procedure UpdateNode(N: TTreeNode; C: Boolean);
    procedure SetNodeChecked(Node: TTreeNode; const Value: Boolean);
    procedure ToggleNodeChecked(Node: TTreeNode);
    procedure DirectoryEditChange(Sender: TObject);
    procedure DirectorySelectBtnClick(Sender: TObject);
    function GetNode(Id: Integer): TTreeNode;
    procedure UpdateImageIndex(N: TTreeNode);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // IJediPage
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function GetHintAtPos(ScreenX, ScreenY: Integer): string;
    procedure Show;
    // IJediInstallPage
    procedure AddInstallOption(Id: Integer; Options: TJediInstallGUIOptions;
      const Caption: string = ''; const Hint: string = ''; Parent: Integer = -1);
    procedure InitDisplay;
    function GetOptionChecked(Id: Integer): Boolean;
    procedure SetOptionChecked(Id: Integer; Value: Boolean);
    function GetDirectoryCount: Integer;
    function GetDirectory(Index: Integer): string;
    procedure SetDirectory(Index: Integer; const Value: string);
    function AddDirectory(Caption: string): Integer;
    function GetProgress: Integer;
    procedure SetProgress(Value: Integer);
    procedure BeginInstall;
    procedure MarkOptionBegin(Id: Integer);
    procedure MarkOptionEnd(Id: Integer; Failed: Boolean);
    procedure EndInstall;
    procedure CompilationStart(const ProjectName: string);
    procedure AddLogLine(const Line: string);
    procedure AddHint(const Line: string);
    procedure AddWarning(const Line: string);
    procedure AddError(const Line: string);
    procedure AddFatal(const Line: string);
    procedure AddText(const Line: string);
    procedure CompilationProgress(const FileName: string; LineNumber: Integer);
  end;

implementation

{$IFDEF VCL}
{$R *.dfm}
{$ELSE VisualCLX}
{$R *.xfm}
{$ENDIF VisualCLX}

uses
  {$IFDEF MSWINDOWS}
  Windows, Messages,
  {$ENDIF MSWINDOWS}
  {$IFDEF VisualCLX}
  Qt, QDialogs,
  {$ELSE}
  FileCtrl,
  {$ENDIF}
  JclStrings;

const
  // Icon indexes
  IcoUnchecked      = 0;
  IcoChecked        = 1;
  IcoRadioUnchecked = 2;
  IcoRadioChecked   = 3;
  IcoNotInstalled   = 4;
  IcoFailed         = 5;
  IcoInstalled      = 6;

  IconIndexes: array [Boolean {RadioButton}, Boolean {Checked}] of Integer =
   ( (IcoUnchecked, IcoChecked), (IcoRadioUnchecked, IcoRadioChecked) ); 

type
  TNodeRec = record
    Id: Integer;
    Options: TJediInstallGUIOptions;
    Hint: string;
  end;

  PNodeRec = ^TNodeRec;

  TDirectoryRec = record
    Edit: TEdit;
    Button: TButton;
  end;

  PDirectoryRec = ^TDirectoryRec;

resourcestring
  RsSelectPath      = 'Select path';
  RsEnterValidPath  = '(Enter valid path)';
  RsInvalidOption   = 'Invalid option: %d';
  //RsDuplicateOption = 'Duplicate option: %s';
  //RsCannotFindNode  = 'Cannot find node for Id %d';

constructor TInstallFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FNodeData := TList.Create;
  FDirectories := TList.Create;
end;

destructor TInstallFrame.Destroy;
var
  Index: Integer;
begin
  for Index := FNodeData.Count - 1 downto 0 do
    Dispose(FNodeData.Items[Index]);
  FNodeData.Free;
  for Index := FDirectories.Count - 1 downto 0 do
    Dispose(FDirectories.Items[Index]);
  FDirectories.Free;

  inherited Destroy;
end;

procedure TInstallFrame.DirectoryEditChange(Sender: TObject);
var
  AEdit: TEdit;
begin
  AEdit := Sender as TEdit;
  if DirectoryExists(AEdit.Text) then
    AEdit.Font.Color := clWindowText
  else
    AEdit.Font.Color := clRed;
end;

function TInstallFrame.GetNodeChecked(Node: TTreeNode): Boolean;
begin
  Result := goChecked in PNodeRec(Node.Data)^.Options;
end;

function TInstallFrame.IsAutoChecked(Node: TTreeNode): Boolean;
begin
  Result := not (goNoAutoCheck in PNodeRec(Node.Data)^.Options);
end;

function TInstallFrame.IsRadioButton(Node: TTreeNode): Boolean;
begin
  Result := goRadioButton in PNodeRec(Node.Data)^.Options;
end;

function TInstallFrame.IsStandAloneParent(Node: TTreeNode): Boolean;
begin
  Result := goStandaloneParent in PNodeRec(Node.Data)^.Options;
end;

function TInstallFrame.IsExpandable(Node: TTreeNode): Boolean;
begin
  Result := goExpandable in PNodeRec(Node.Data)^.Options;
end;

procedure TInstallFrame.UpdateNode(N: TTreeNode; C: Boolean);
var
  ANodeRec: PNodeRec;
begin
  ANodeRec := N.Data;
  if C then
    Include(ANodeRec^.Options, goChecked)
  else
    Exclude(ANodeRec^.Options, goChecked);
  UpdateImageIndex(N);
end;

procedure TInstallFrame.SetNodeChecked(Node: TTreeNode; const Value: Boolean);

  procedure UpdateTreeDown(N: TTreeNode; C: Boolean);
  begin
    N := N.getFirstChild;
    while Assigned(N) do
    begin
      if not C or IsAutoChecked(N) then
      begin
        if not IsRadioButton(N) then
          UpdateNode(N, C);
        UpdateTreeDown(N, C);
      end;
      N := N.getNextSibling;
    end;
  end;

  procedure UpdateTreeUp(N: TTreeNode; C: Boolean);
  var
    ParentNode: TTreeNode;
    ParentChecked: Boolean;
  begin
    if C then
      while Assigned(N) do
      begin
        UpdateNode(N, True);
        N := N.Parent;
      end
    else
    begin
      ParentNode := N.Parent;
      while Assigned(ParentNode) do
      begin
        N := ParentNode.getFirstChild;
        ParentChecked := IsStandAloneParent(ParentNode);
        while Assigned(N) do
          if GetNodeChecked(N) and not IsRadioButton(N) then
          begin
            ParentChecked := True;
            Break;
          end
          else
            N := N.getNextSibling;
        UpdateNode(ParentNode, ParentChecked);
        ParentNode := ParentNode.Parent;
      end;
    end;
  end;

  procedure UpdateRadioButton(N: TTreeNode; C: Boolean);
  var
    Node: TTreeNode;
  begin
    if Value and not GetNodeChecked(N) then
    begin
      Node := N.Parent;
      if Node <> nil then
      begin
        Node := Node.getFirstChild;
        while Node <> nil do
        begin
          if IsRadioButton(Node) then
            UpdateNode(Node, Node = N);
          Node := Node.getNextSibling;
        end;
      end;
    end;
  end;

begin
  if IsRadioButton(Node) then
    UpdateRadioButton(Node, Value)
  else
  begin
    UpdateTreeDown(Node, Value);
    UpdateNode(Node, Value);
    UpdateTreeUp(Node, Value);
  end;
  TreeView.Invalidate;
end;

procedure TInstallFrame.ToggleNodeChecked(Node: TTreeNode);
begin
  if Assigned(Node) then
    SetNodeChecked(Node, not GetNodeChecked(Node));
end;

function TInstallFrame.GetNode(Id: Integer): TTreeNode;
begin
  Result := TreeView.Items.GetFirstNode;
  while Assigned(Result) do
  begin
    if PNodeRec(Result.Data)^.Id = Id then
      Break;
    Result := Result.GetNext;
  end;
end;

procedure TInstallFrame.UpdateImageIndex(N: TTreeNode);
var
  ImgIndex: Integer;
begin
  ImgIndex := IconIndexes[IsRadioButton(N), GetNodeChecked(N)];
  N.ImageIndex := ImgIndex;
  N.SelectedIndex := ImgIndex;
end;

procedure TInstallFrame.DirectorySelectBtnClick(Sender: TObject);
var
  Index: Integer;
  Button: TButton;
  Edit: TEdit;
  {$IFDEF VisualCLX}
    {$IFDEF COMPILER7_UP}
      {$DEFINE USE_WIDESTRING}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF KYLIX}
    {$DEFINE USE_WIDESTRING}
  {$ENDIF KYLIX}
  {$IFDEF USE_WIDESTRING}
  Directory: WideString;
  {$UNDEF USE_WIDESTRING}
  {$ELSE}
  Directory: string;
  {$ENDIF}
  DirectoryRec: PDirectoryRec;
begin
  Button := Sender as TButton;
  Edit := nil;
  for Index := 0 to FDirectories.Count - 1 do
  begin
    DirectoryRec := FDirectories.Items[Index];
    if DirectoryRec^.Button = Button then
    begin
      Edit := DirectoryRec^.Edit;
      Break;
    end;
  end;
  if Assigned(Edit) and SelectDirectory(RsSelectPath, '', Directory) then
    Edit.Text := Directory;
end;

procedure TInstallFrame.SplitterCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  Accept := NewSize > 150;
end;

{$IFDEF VisualCLX}
procedure TInstallFrame.TreeViewCustomDrawItem(Sender: TCustomViewControl; Item: TCustomViewItem;
  Canvas: TCanvas; const Rect: TRect; State: TCustomDrawState; Stage: TCustomDrawStage; 
  var DefaultDraw: Boolean);
{$ELSE}
procedure TInstallFrame.TreeViewCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; 
  State: TCustomDrawState; var DefaultDraw: Boolean);
{$ENDIF}
begin
  case TTreeNode({$IFDEF VisualCLX}Item{$ELSE}Node{$ENDIF}).Level of
    0: begin
         {$IFDEF VCL}Sender.{$ENDIF}Canvas.Font.Style := [fsBold, fsUnderline];
       end;
    1: begin
         {$IFDEF VCL}Sender.{$ENDIF}Canvas.Font.Style := [fsBold];
       end;
  end;
end;

procedure TInstallFrame.TreeViewKeyPress(Sender: TObject; var Key: Char);
begin
  with TTreeView(Sender) do
    case Key of
      #32:
        if not FInstalling then
        begin
          ToggleNodeChecked(Selected);
          Key := #0;
        end;
      '+':
        Selected.Expanded := True;
      '-':
        Selected.Expanded := False;
    end;
end;

{$IFDEF VisualCLX}
function TreeNodeIconHit(TreeView: TTreeView; X, Y: Integer; Node: TTreeNode = nil): Boolean;
var
  Level, X1: Integer;
begin
  Result := False;
  if Node = nil then
    Node := TreeView.GetNodeAt(X, Y);
  if Assigned(Node) then
  begin
    Level := Node.Level;
    if QListView_rootIsDecorated(TreeView.Handle) then
      Inc(Level);
    X1 := QListView_treeStepSize(TreeView.Handle) * Level;
    Result := (X > X1) and (X <= X1 + TreeView.Images.Width);
  end;
end;
{$ELSE VCL}
function TreeNodeIconHit(TreeView: TTreeView; X, Y: Integer): Boolean;
begin
  Result := htOnIcon in TreeView.GetHitTestInfoAt(X, Y);
end;
{$ENDIF VCL}

procedure TInstallFrame.TreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  if not FInstalling then
    with TTreeView(Sender) do
  begin
    Node := GetNodeAt(X, Y);
    if (Button = mbLeft) and TreeNodeIconHit(TreeView, X, Y{$IFDEF VisualCLX}, Node{$ENDIF}) then
      ToggleNodeChecked(Node);
  end;
end;

{$IFDEF VCL}
function TInstallFrame.GetFormCompile: TFormCompile;
begin
  if not Assigned(FFormCompile) then
  begin
    FFormCompile := TFormCompile.Create(Self);
    SetWindowLong(FFormCompile.Handle, GWL_HWNDPARENT, Handle);
    FFormCompile.Init(Caption, True);
    FFormCompile.Show;
    Application.ProcessMessages;
  end;
  Result := FFormCompile;
end;
{$ENDIF VCL}

// IJediPage
function TInstallFrame.GetCaption: string;
begin
  Result := (Parent as TTabSheet).Caption;
end;

procedure TInstallFrame.SetCaption(const Value: string);
begin
  (Parent as TTabSheet).Caption := Value;
  AddInstallOption(JediTargetOption, [goExpandable, goChecked], Value, RsHintTarget, -1);
end;

function TInstallFrame.GetHintAtPos(ScreenX, ScreenY: Integer): string;
var
  TreeViewCoord: TPoint;
  ANode: TTreeNode;
begin
  TreeViewCoord := TreeView.ScreenToClient(Point(ScreenX, ScreenY));
  if (TreeViewCoord.X >= 0) and (TreeViewCoord.Y >= 0) and
    (TreeViewCoord.X < TreeView.Width) and (TreeViewCoord.Y < TreeView.Height) then
  begin
    ANode := TreeView.GetNodeAt(TreeViewCoord.X, TreeViewCoord.Y);
    if Assigned(ANode) then
      Result := PNodeRec(ANode.Data)^.Hint;
  end;
end;

procedure TInstallFrame.Show;
var
  ATabSheet: TTabSheet;
begin
  ATabSheet := Parent as TTabSheet;
  (ATabSheet.Parent as TPageControl).ActivePage := ATabSheet;
end;

// IJediInstallPage
procedure TInstallFrame.AddInstallOption(Id: Integer; Options: TJediInstallGUIOptions;
  const Caption: string = ''; const Hint: string = ''; Parent: Integer = -1);
var
  NodeRec: PNodeRec;
  ParentNode, ThisNode: TTreeNode;
begin
  if Id = -1 then
    raise Exception.CreateResFmt(@RsInvalidOption, [Id]);

  if Parent <> -1 then
    ParentNode := GetNode(Parent)
  else
    ParentNode := nil;
  ThisNode := GetNode(Id);
  if Assigned(ThisNode) then
    ThisNode.Text := Caption
  else
  begin
    New(NodeRec);
    NodeRec^.Id := Id;
    NodeRec^.Hint := Hint;
    NodeRec^.Options := Options;
    ThisNode := TreeView.Items.AddChildObject(ParentNode, Caption, NodeRec);
    FNodeData.Add(NodeRec);
  end;

  UpdateImageIndex(ThisNode);
end;

procedure TInstallFrame.InitDisplay;
var
  ANode: TTreeNode;
begin
  ANode := TreeView.Items.GetFirstNode;
  while Assigned(ANode) do
  begin
    if (ANode.Count > 0) and IsExpandable(ANode) then
      ANode.Expand(False);
    ANode := ANode.GetNext;
  end;
end;

function TInstallFrame.GetOptionChecked(Id: Integer): Boolean;
var
  ANode: TTreeNode;
begin
  ANode := GetNode(Id);
  Result := Assigned(ANode) and GetNodeChecked(ANode);
end;

procedure TInstallFrame.SetOptionChecked(Id: Integer; Value: Boolean);
var
  ANode: TTreeNode;
begin
  ANode := GetNode(Id);
  if Assigned(ANode) then
    UpdateNode(ANode, Value);
end;

function TInstallFrame.GetDirectoryCount: Integer;
begin
  Result := FDirectories.Count;
end;

function TInstallFrame.GetDirectory(Index: Integer): string;
begin
  Result := PDirectoryRec(FDirectories.Items[Index])^.Edit.Text;
end;

procedure TInstallFrame.SetDirectory(Index: Integer; const Value: string);
begin
  PDirectoryRec(FDirectories.Items[Index])^.Edit.Text := Value;
end;

function TInstallFrame.AddDirectory(Caption: string): Integer;
var
  ADirectoryRec: PDirectoryRec;
  ALabel: TLabel;
  ControlTop, ButtonWidth, LabelRight: Integer;
begin
  if FDirectories.Count > 0 then
  begin
    ADirectoryRec := FDirectories.Items[FDirectories.Count - 1];
    ControlTop := ADirectoryRec^.Edit.Top + ADirectoryRec^.Edit.Height + 10;
  end
  else
    ControlTop := 16;

  New(ADirectoryRec);
  ALabel := TLabel.Create(Self);
  ALabel.Parent := OptionsGroupBox;
  ALabel.Caption := Caption;
  ALabel.AutoSize := True;
  ADirectoryRec^.Edit := TEdit.Create(Self);
  ADirectoryRec^.Edit.Parent := OptionsGroupBox;
  ADirectoryRec^.Edit.Anchors := [akLeft, akTop, akRight];
  ADirectoryRec^.Button := TButton.Create(Self);
  ADirectoryRec^.Button.Parent := OptionsGroupBox;
  ADirectoryRec^.Button.Caption := '...';
  ADirectoryRec^.Button.Anchors := [akTop, akRight];

  ButtonWidth := 2 * ALabel.Height;
  LabelRight := (ALabel.Width div 16) * 16 + 32 + ALabel.Left; // make edits aligned when label widths are nearly equals

  ADirectoryRec^.Edit.SetBounds(LabelRight, ControlTop,
    OptionsGroupBox.ClientWidth - LabelRight - ButtonWidth - 16,
    ADirectoryRec^.Edit.Height);
  ADirectoryRec^.Button.SetBounds(OptionsGroupBox.ClientWidth - ButtonWidth - 8,
    ControlTop, ButtonWidth, ADirectoryRec^.Edit.Height);
  ALabel.SetBounds(8, ControlTop + (ADirectoryRec^.Edit.Height - ALabel.Height) div 2,
    ALabel.Width, ALabel.Height);

  ADirectoryRec^.Edit.OnChange := DirectoryEditChange;
  ADirectoryRec^.Button.OnClick := DirectorySelectBtnClick;

  OptionsGroupBox.ClientHeight := ADirectoryRec^.Edit.Top + ADirectoryRec^.Edit.Height + 10;
  {$IFDEF VisualCLX}
  InfoDisplay.Height := InfoPanel.Height + OptionsGroupBox.Top - 8;
  {$ELSE ~VisualCLX}
  OptionsGroupBox.Top := TreeView.Height + TreeView.Top - OptionsGroupBox.Height;
  InfoDisplay.Height := OptionsGroupBox.Top - InfoDisplay.Top - 8;
  {$ENDIF ~VisualCLX}

  Result := FDirectories.Add(ADirectoryRec);
end;

function TInstallFrame.GetProgress: Integer;
begin
  Result := ProgressBar.Position;
end;

procedure TInstallFrame.SetProgress(Value: Integer);
begin
  ProgressBar.Position := Value;
end;

procedure TInstallFrame.BeginInstall;
var
  ANode: TTreeNode;
begin
  ProgressBar.Visible := True;

  InfoDisplay.Lines.Clear;

  FCheckedCount := 0;
  FInstallCount := 0;
  ANode := TreeView.Items.GetFirstNode;
  while Assigned(ANode) do
  begin
    if GetNodeChecked(ANode) then
      Inc(FCheckedCount);
    ANode := ANode.GetNext;
  end;

  FInstalling := True;
end;

procedure TInstallFrame.MarkOptionBegin(Id: Integer);
var
  ANode: TTreeNode;
begin
  ANode := GetNode(Id);
  if Assigned(ANode) then
  begin
    ANode.ImageIndex := IcoNotInstalled;
    ANode.SelectedIndex := IcoNotInstalled;
  end;
end;

procedure TInstallFrame.MarkOptionEnd(Id: Integer; Failed: Boolean);
var
  ANode: TTreeNode;
begin
  {$IFDEF VCL}
  if Assigned(FFormCompile) then
  begin
    if FFormCompile.Errors > 0 then // do not make the dialog modal when no error occured
      FFormCompile.Done(' ')
    else
      FFormCompile.Done;
    FreeAndNil(FFormCompile);
  end;
  {$ENDIF VCL}
  ANode := GetNode(Id);
  if Assigned(ANode) and GetNodeChecked(ANode) then
  begin
    if Failed then
    begin
      ANode.ImageIndex := IcoFailed;
      ANode.SelectedIndex := IcoFailed;
    end
    else
    begin
      ANode.ImageIndex := IcoInstalled;
      ANode.SelectedIndex := IcoInstalled;
    end;
    Inc(FInstallCount);
    if FCheckedCount > 0 then
      SetProgress(100 * FInstallCount div FCheckedCount);
  end;
end;

procedure TInstallFrame.EndInstall;
var
  ANode: TTreeNode;
begin
  FInstalling := False;

  MarkOptionEnd(-1, True);
  ANode := TreeView.Items.GetFirstNode;
  while Assigned(ANode) do
  begin
    UpdateImageIndex(ANode);
    ANode := ANode.GetNext;
  end;
  ProgressBar.Visible := False;
end;

procedure TInstallFrame.CompilationStart(const ProjectName: string);
begin
  {$IFDEF VCL}
  GetFormCompile.Init(ProjectName, True);
  {$ENDIF VCL}
end;

procedure TInstallFrame.AddLogLine(const Line: string);
{$IFDEF VCL}
begin
  InfoDisplay.Lines.Append(Line);
  InfoDisplay.Perform(EM_SCROLLCARET, 0, 0);
end;
{$ELSE ~VCL}
var
  NewCaretPos: TCaretPos;
begin
  with InfoDisplay do
  begin
    Lines.BeginUpdate;
    Lines.Append(Line);
    Lines.EndUpdate;
    NewCaretPos.Line := Lines.Count;
    NewCaretPos.Col := 0;
    CaretPos := NewCaretPos;
  end;
end;
{$ENDIF ~VCL}

procedure TInstallFrame.AddHint(const Line: string);
begin
  {$IFDEF VCL}
  GetFormCompile.AddHint(Line);
  {$ENDIF VCL}
  AddLogLine(Line);
end;

procedure TInstallFrame.AddWarning(const Line: string);
begin
  {$IFDEF VCL}
  GetFormCompile.AddWarning(Line);
  {$ENDIF VCL}
  AddLogLine(Line);
end;

procedure TInstallFrame.AddError(const Line: string);
begin
  {$IFDEF VCL}
  GetFormCompile.AddError(Line);
  {$ENDIF VCL}
  AddLogLine(Line);
end;

procedure TInstallFrame.AddFatal(const Line: string);
begin
  {$IFDEF VCL}
  GetFormCompile.AddFatal(Line);
  {$ENDIF VCL}
  AddLogLine(Line);
end;

procedure TInstallFrame.AddText(const Line: string);
begin
  //{$IFDEF VCL}
  //GetFormCompile.AddText(Line);
  //{$ENDIF VCL}
  AddLogLine(Line);
end;

procedure TInstallFrame.CompilationProgress(const FileName: string; LineNumber: Integer);
begin
  {$IFDEF VCL}
  GetFormCompile.CompilationProgress(FileName, LineNumber);
  {$ENDIF VCL}
end;

end.


