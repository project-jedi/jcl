{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit. Manual modifications will be lost on next release.  }
{**************************************************************************************************}

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
{ Contributor(s): Robert Rossmair (crossplatform support)                                          }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: February 13, 2004                                                                 }
{                                                                                                  }
{**************************************************************************************************}

unit QProductFrames;

interface

{$I jcl.inc}

uses
  SysUtils, Classes,
  
  Types, 
  QGraphics, QForms, QControls, QStdCtrls, QComCtrls, QExtCtrls,
  QJediInstallIntf;
  

type
  TProductFrame = class(TFrame)
    ComponentsTreePanel: TPanel;
    Label1: TLabel;
    TreeView: TTreeView;
    Splitter: TSplitter;
    InfoPanel: TPanel;
    Label2: TLabel;
    
    InfoDisplay: TTextBrowser;
    
    OptionsGroupBox: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    BplPathEdit: TEdit;
    Button1: TButton;
    Button2: TButton;
    DcpPathEdit: TEdit;
    procedure DcpPathEditChange(Sender: TObject);
    procedure PathSelectBtnClick(Sender: TObject);
    procedure SplitterCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeViewKeyPress(Sender: TObject; var Key: Char);
    
    procedure TreeViewCustomDrawItem(Sender: TCustomViewControl; Item: TCustomViewItem; 
      Canvas: TCanvas; const Rect: TRect; State: TCustomDrawState; Stage: TCustomDrawStage; 
      var DefaultDraw: Boolean);
    
  private
    { Private declarations }
    function GetNodeChecked(Node: TTreeNode): Boolean;
    procedure SetNodeChecked(Node: TTreeNode; const Value: Boolean);
    procedure ToggleNodeChecked(Node: TTreeNode);
  public
    { Public declarations }
    function FeatureChecked(FeatureID: Cardinal; VersionNumber: Integer): Boolean;
    property NodeChecked[Node: TTreeNode]: Boolean read GetNodeChecked write SetNodeChecked;
  end;

implementation

{$R *.xfm}

uses QDialogs;

resourcestring
  RsSelectPath      = 'Select path';

procedure TProductFrame.DcpPathEditChange(Sender: TObject);
begin
  with (Sender as TEdit) do
    if DirectoryExists(Text) then
      Font.Color := clWindowText
    else
      Font.Color := clRed;
end;

function TProductFrame.FeatureChecked(FeatureID: Cardinal; VersionNumber: Integer): Boolean;
var
  I: Integer;
  F: Cardinal;
begin
  Result := False;
  for I := 0 to TreeView.Items.Count - 1 do
  begin
    F := Cardinal(TreeView.Items[I].Data);
    if F and FID_NumberMask = FeatureID then
    begin
      Result := F and FID_Checked <> 0;
      Break;
    end;
  end;
end;

function TProductFrame.GetNodeChecked(Node: TTreeNode): Boolean;
begin
  Result := Cardinal(Node.Data) and FID_Checked <> 0;
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
    AnyChecked: Boolean;
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
      if SiblingNode.Parent <> nil then
        SiblingNode := TempNode.Parent.getFirstChild;
      AnyChecked := False;
      while Assigned(SiblingNode) do
        if NodeChecked[SiblingNode] then
        begin
          AnyChecked := True;
          Break;
        end
        else
          SiblingNode := SiblingNode.getNextSibling;
      TempNode := TempNode.Parent;
      if Assigned(TempNode) then
        UpdateNode(TempNode, AnyChecked)
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
  Directory: {$IFDEF UNIX} WideString; {$ELSE} string; {$ENDIF}
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


procedure TProductFrame.TreeViewCustomDrawItem(Sender: TCustomViewControl; Item: TCustomViewItem; 
  Canvas: TCanvas; const Rect: TRect; State: TCustomDrawState; Stage: TCustomDrawStage; 
  var DefaultDraw: Boolean);

begin
  case TTreeNode(Item).Level of
    0: begin
         Canvas.Font.Style := [fsBold, fsUnderline];
       end;
    1: begin
         Canvas.Font.Style := [fsBold];
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

procedure TProductFrame.TreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  with TTreeView(Sender) do
  begin
    Node := GetNodeAt(X, Y);
    if (Button = mbLeft) then
      ToggleNodeChecked(Node);
  end;
end;

end.
