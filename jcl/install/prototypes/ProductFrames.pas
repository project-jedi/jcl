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
{                                                                                                  }
{**************************************************************************************************}

// $Id$

{$IFNDEF Develop}unit {$IFDEF VisualCLX}QProductFrames{$ELSE}ProductFrames{$ENDIF};{$ENDIF}

interface

{$I jcl.inc}
{$I crossplatform.inc}

uses
  SysUtils, Classes,
  {$IFDEF VisualCLX}
  Types,
  QGraphics, QForms, QControls, QStdCtrls, QComCtrls, QExtCtrls,
  {$ELSE}
  Graphics, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls,
  {$ENDIF}
  JclBorlandTools, JediInstall;

const
  // Feature masks
  FID_Expandable           = $20000000;
  FID_StandaloneParent     = $40000000; // do not auto-uncheck when all child nodes are unchecked
  FID_Checked              = $80000000;
  FID_NumberMask           = $0FFFFFFF;

  // Icon indexes
  IcoProduct               = 0;
  IcoLevel1                = 1;
  IcoChecked               = 2;
  IcoUnchecked             = 3;

type
  TProductFrame = class(TFrame)
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
    BplPathLabel: TLabel;
    DcpPathLabel: TLabel;
    BplPathEdit: TEdit;
    Button1: TButton;
    Button2: TButton;
    DcpPathEdit: TEdit;
    procedure PathEditChange(Sender: TObject);
    procedure PathSelectBtnClick(Sender: TObject);
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
    { Private declarations }
    FInstallation: TJclBorRADToolInstallation;
    function GetNodeChecked(Node: TTreeNode): Boolean;
    function IsStandAloneParent(Node: TTreeNode): Boolean;
    procedure SetInstallation(Value: TJclBorRADToolInstallation);
    procedure SetNodeChecked(Node: TTreeNode; const Value: Boolean);
    procedure ToggleNodeChecked(Node: TTreeNode);
  public
    { Public declarations }
    class function GetName(Installation: TJclBorRADToolInstallation): string;
    function FeatureChecked(FeatureID: Cardinal): Boolean;
    procedure LogOutputLine(const Line: string);
    procedure UpdateTree;
    procedure StartCompilation(Installation: TJclBorRADToolInstallation);
    procedure StopCompilation(Installation: TJclBorRADToolInstallation);
    property NodeChecked[Node: TTreeNode]: Boolean read GetNodeChecked write SetNodeChecked;
    property Installation: TJclBorRADToolInstallation read FInstallation write SetInstallation;
  end;

implementation

{$IFDEF VisualCLX}
{$R *.xfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  {$IFDEF MSWINDOWS}
  Messages,
  {$ENDIF MSWINDOWS}
  {$IFDEF VisualCLX}
  Qt, QDialogs,
  {$ELSE}
  FileCtrl, FrmCompile,
  {$ENDIF}
  JclStrings,
  JclInstall;

resourcestring
  RsSelectPath      = 'Select path';
  RsEnterValidPath  = '(Enter valid path)';

procedure TProductFrame.PathEditChange(Sender: TObject);
begin
  with (Sender as TEdit) do
    if DirectoryExists(Text) then
      Font.Color := clWindowText
    else
      Font.Color := clRed;
end;

function TProductFrame.FeatureChecked(FeatureID: Cardinal): Boolean;
var
  F: Cardinal;
  Node: TTreeNode;
begin
  Result := False;
  Node := TreeView.Items.GetFirstNode;
  while Node <> nil do
  begin
    F := Cardinal(Node.Data);
    if F and FID_NumberMask = FeatureID then
    begin
      Result := F and FID_Checked <> 0;
      Break;
    end;
    Node := Node.GetNext;
  end;
end;

class function TProductFrame.GetName(Installation: TJclBorRADToolInstallation): string;
begin
  Result := Format('%s%dProduct', [Prefixes[Installation.RADToolKind], Installation.VersionNumber]);
end;

function TProductFrame.GetNodeChecked(Node: TTreeNode): Boolean;
begin
  Result := Cardinal(Node.Data) and FID_Checked <> 0;
end;

function TProductFrame.IsStandAloneParent(Node: TTreeNode): Boolean;
begin
  Result := Cardinal(Node.Data) and FID_StandAloneParent <> 0;
end;

procedure TProductFrame.LogOutputLine(const Line: string);
{$IFDEF VCL}

  function Cmp(const S: string): Boolean;
  begin
    Result := Copy(Line, 1, Length(S)) = S;
  end;

begin
  if FormCompile.HandleLine(Line) <> clFileProgress then
  begin
    if Cmp(AnsiLineBreak + 'Installing package ') then
      FormCompile.Linking(Copy(Line, 20, MaxInt))
    else if Cmp('Copying .hpp files...') then
      FormCompile.Done;

    InfoDisplay.Lines.Append(Line);
    InfoDisplay.Perform(EM_SCROLLCARET, 0, 0);
  end;
end;
{$ELSE VisualCLX}
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
{$ENDIF VisualCLX}

procedure TProductFrame.SetInstallation(Value: TJclBorRADToolInstallation);

  function GetPathForEdit(Path: string): string;
  begin
    if DirectoryExists(Path) then
      Result := Path
    else
      Result := RsEnterValidPath;
  end;

begin
  FInstallation := Value;
  Name := GetName(Value);
  if Value.RadToolKind = brCppBuilder then
    DcpPathLabel.Caption := '.bpi Path';
  BplPathEdit.Text := GetPathForEdit(Installation.BPLOutputPath);
  DcpPathEdit.Text := GetPathForEdit(Installation.DCPOutputPath);
end;

procedure TProductFrame.SetNodeChecked(Node: TTreeNode; const Value: Boolean);

  procedure UpdateNode(N: TTreeNode; C: Boolean);
  const
    CheckedState: array[Boolean] of Cardinal = (0, FID_Checked);
  begin
    N.Data := Pointer(Cardinal(N.Data) and (not FID_Checked) or CheckedState[C]);
    if C then
    begin
      N.ImageIndex := IcoChecked;
      N.SelectedIndex := IcoChecked;
    end
    else
    begin
      N.ImageIndex := IcoUnchecked;
      N.SelectedIndex := IcoUnchecked;
    end;
  end;

  procedure UpdateTreeDown(N: TTreeNode; C: Boolean);
  var
    TempNode: TTreeNode;
  begin
    TempNode := N.getFirstChild;
    while Assigned(TempNode) do
    begin
      UpdateNode(TempNode, C);
      UpdateTreeDown(TempNode, C);
      TempNode := TempNode.getNextSibling;
    end;
  end;

  procedure UpdateTreeUp(N: TTreeNode; C: Boolean);
  var
    TempNode, SiblingNode: TTreeNode;
    ParentChecked: Boolean;
  begin
    TempNode := N;
    if C then
    while Assigned(TempNode) do
    begin
      UpdateNode(TempNode, True);
      TempNode := TempNode.Parent;
    end
    else
    while True do
    begin
      SiblingNode := TempNode;
      ParentChecked := False;
      if SiblingNode.Parent <> nil then
      begin
        SiblingNode := TempNode.Parent.getFirstChild;
        ParentChecked := IsStandAloneParent(TempNode.Parent);
      end;
      while Assigned(SiblingNode) do
        if NodeChecked[SiblingNode] then
        begin
          ParentChecked := True;
          Break;
        end
        else
          SiblingNode := SiblingNode.getNextSibling;
      TempNode := TempNode.Parent;
      if Assigned(TempNode) then
        UpdateNode(TempNode, ParentChecked)
      else
        Break;
    end;
  end;

begin
  UpdateNode(Node, Value);
  UpdateTreeDown(Node, Value);
  UpdateTreeUp(Node, Value);
end;

procedure TProductFrame.ToggleNodeChecked(Node: TTreeNode);
begin
  if Assigned(Node) then
    NodeChecked[Node] := not NodeChecked[Node];
end;

procedure TProductFrame.PathSelectBtnClick(Sender: TObject);
var
  I: Integer;
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
begin
  Button := Sender as TButton;
  Edit := nil;
  with Button.Parent do
    for I := 0 to ControlCount - 1 do
      if (Controls[I].Top = Button.Top) and (Controls[I] is TEdit) then
        Edit := TEdit(Controls[I]);
  if Assigned(Edit) and SelectDirectory(RsSelectPath, '', Directory) then
    Edit.Text := Directory;
end;

procedure TProductFrame.SplitterCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  Accept := NewSize > 150;
end;

{$IFDEF VisualCLX}
procedure TProductFrame.TreeViewCustomDrawItem(Sender: TCustomViewControl; Item: TCustomViewItem; 
  Canvas: TCanvas; const Rect: TRect; State: TCustomDrawState; Stage: TCustomDrawStage; 
  var DefaultDraw: Boolean);
{$ELSE}
procedure TProductFrame.TreeViewCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; 
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

procedure TProductFrame.TreeViewKeyPress(Sender: TObject; var Key: Char);
begin
  with TTreeView(Sender) do
    if (Key = #32) then
    begin
      ToggleNodeChecked(Selected);
      Key := #0;
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

procedure TProductFrame.TreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  with TTreeView(Sender) do
  begin
    Node := GetNodeAt(X, Y);
    if (Button = mbLeft) and TreeNodeIconHit(TreeView, X, Y{$IFDEF VisualCLX}, Node{$ENDIF}) then
      ToggleNodeChecked(Node);
  end;
end;

procedure TProductFrame.UpdateTree;
begin
  TreeView.FullExpand;
end;

procedure TProductFrame.StartCompilation(Installation: TJclBorRADToolInstallation);
begin
  {$IFDEF VCL}
  if not Assigned(FormCompile) then
    FormCompile := TFormCompile.Create(Self);
  FormCompile.Init(Installation.Name, True);
  FormCompile.Show;
  Application.ProcessMessages;
  {$ENDIF VCL}
end;

procedure TProductFrame.StopCompilation(Installation: TJclBorRADToolInstallation);
begin
  {$IFDEF VCL}
  if FormCompile.Errors > 0 then // do not make the dialog modal when no error occured
    FormCompile.Done(' ')
  else
    FormCompile.Done;
  FormCompile.Free;
  FormCompile := nil;
  {$ENDIF VCL}
end;

end.
