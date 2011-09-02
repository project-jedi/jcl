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
{ The Original Code is OpenDlgFavAdapter.pas.                                                      }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones.                                        }
{ Portions created by Petr Vones are Copyright (C) Petr Vones. All rights reserved.                }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Salvatore Besso                                                                                }
{   Florent Ouchet (move to JCL runtime)                                                           }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclOpenDialogFavorites;

interface

{$I jcl.inc}

uses
  {$IFDEF HAS_UNITSCOPE}
  Winapi.Windows, Winapi.Messages, System.Classes, System.SysUtils, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls,
  {$ELSE ~HAS_UNITSCOPE}
  Windows, Messages, Classes, SysUtils, Controls, StdCtrls, ExtCtrls,
  {$ENDIF ~HAS_UNITSCOPE}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclPeImage, JclWin32,
  JclOpenDialogHooks;

type
  TJclOpenDialogFavoritesHook = class (TJclOpenDialogHook)
  private
    FAddButton: TButton;
    FDeleteMode: Boolean;
    FFavoriteComboBox: TComboBox;
    FFavoriteFolders: TStrings;
    FFavoritePanel: TPanel;
    procedure AddButtonClick(Sender: TObject);
    procedure FavoriteComboBoxClick(Sender: TObject);
    procedure SetDeleteMode(const Value: Boolean);
  protected
    procedure AdjustControlPos; override;
    procedure DialogFolderChange; override;
    procedure DialogShow; override;
    procedure WndProc(var Message: TMessage); override;
    property DeleteMode: Boolean read FDeleteMode write SetDeleteMode;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFavorites(const FileName: string);
    property FavoriteFolders: TStrings read FFavoriteFolders;
  end;

function InitializeOpenDialogFavorites: TJclOpenDialogFavoritesHook;
procedure FinalizeOpenDialogFavorites;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\vcl';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNITSCOPE}
  Winapi.CommDlg, Winapi.Dlgs,
  {$ELSE ~HAS_UNITSCOPE}
  CommDlg, Dlgs,
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase, JclFileUtils, JclStrings, JclSysInfo, JclSysUtils, JclVclResources;

function InitializeOpenDialogFavorites: TJclOpenDialogFavoritesHook;
begin
  Result := InitializeOpenDialogHook(TJclOpenDialogFavoritesHook) as TJclOpenDialogFavoritesHook;
end;

procedure FinalizeOpenDialogFavorites;
begin
  FinalizeOpenDialogHook;
end;

//=== { TJclOpenDialogFavoritesHook } ========================================

constructor TJclOpenDialogFavoritesHook.Create;
begin
  inherited Create;
  FFavoriteFolders := TStringList.Create;
  FFavoritePanel := TPanel.Create(nil);
  with FFavoritePanel do
  begin
    Name := 'FavoritePanel';
    BevelOuter := bvNone;
    Caption := '';
    FullRepaint := False;
    FFavoriteComboBox := TComboBox.Create(FFavoritePanel);
    with FFavoriteComboBox do
    begin
      SetBounds(6, 14, 300, Height);
      Style := csDropDownList;
      Sorted := True;
      OnClick := FavoriteComboBoxClick;
      Parent := FFavoritePanel;
    end;
    with TStaticText.Create(FFavoritePanel) do
    begin
      AutoSize := False;
      SetBounds(6, 0, 100, 14);
      Caption := LoadResString(@RsOpenDialogFavorites);
      FocusControl := FFavoriteComboBox;
      Parent := FFavoritePanel;
    end;
    FAddButton := TButton.Create(FFavoritePanel);
    with FAddButton do
    begin
      SetBounds(333, 14, 75, 23);
      Caption := LoadResString(@RsOpenDialogAdd);
      OnClick := AddButtonClick;
      Parent := FFavoritePanel;
    end;
  end;
end;

destructor TJclOpenDialogFavoritesHook.Destroy;
begin
  FreeAndNil(FFavoritePanel);
  FreeAndNil(FFavoriteFolders);
  inherited Destroy;
end;

procedure TJclOpenDialogFavoritesHook.AddButtonClick(Sender: TObject);
var
  I: Integer;
  Path: string;
begin
  if DeleteMode then
  begin
    I := FFavoriteComboBox.ItemIndex;
    Path := FFavoriteComboBox.Items[I];
    if MessageBox(FHandle,
                  PChar(Format(LoadResString(@RsOpenDialogDelConfirm), [Path])),
                  PChar(LoadResString(@RsOpenDialogConfirmation)),
                  MB_YESNO or MB_ICONQUESTION or MB_DEFBUTTON2) = ID_YES then
    begin
      FFavoriteComboBox.Items.Delete(I);
      DeleteMode := False;
    end;
  end
  else
  begin
    Path := CurrentFolder;
    I := FFavoriteComboBox.Items.IndexOf(Path);
    if I = -1 then
    begin
      FFavoriteComboBox.Items.Add(Path);
      I := FFavoriteComboBox.Items.IndexOf(Path);
      FFavoriteComboBox.ItemIndex := I;
      DeleteMode := True;
    end;
  end;
end;

procedure TJclOpenDialogFavoritesHook.AdjustControlPos;
var
  ParentRect, FileNameEditRect, OkButtonRect: TRect;

  procedure GetDlgItemRect(ItemID: Integer; var R: TRect);
  begin
    GetWindowRect(GetDlgItem(FParentWnd, ItemID), R);
    MapWindowPoints(0, FParentWnd, R, 2);
  end;

begin
  inherited AdjustControlPos;
  GetWindowRect(FParentWnd, ParentRect);
  if GetDlgItem(FParentWnd, edt1) <> 0 then
    GetDlgItemRect(edt1, FileNameEditRect)
  else
    GetDlgItemRect(cmb1, FileNameEditRect);
  GetDlgItemRect(1, OkButtonRect);

// Salvatore Besso: Changes to avoid truncation of Add button. I don't know why, but debugging I
//   have discovered that ParentRect.Right was equal to 1024, ie Screen.Width. I also can't figure
//   out why I can't preserve original help button that disappears using this expert.
//   As visible in the changes, favorite panel width is just left of the original button column.

  if IsWin2k or IsWinXP then
    FAddButton.Width := 65;
  FFavoritePanel.Width := OkButtonRect.Left - 1;
  FFavoriteComboBox.Width := FFavoritePanel.Width - FFavoriteComboBox.Left - FAddButton.Width - 16;
  FAddButton.Left := FFavoriteComboBox.Width + 14;
end;

procedure TJclOpenDialogFavoritesHook.DialogFolderChange;
var
  Path: string;
begin
  inherited DialogFolderChange;
  Path := CurrentFolder;
  with FFavoriteComboBox do
  begin
    ItemIndex := Items.IndexOf(Path);
    DeleteMode := (ItemIndex <> -1);
  end;
end;

procedure TJclOpenDialogFavoritesHook.DialogShow;
var
  PreviewRect: TRect;
begin
  inherited DialogShow;
  if not IsOpenPictureDialog then
  begin
    GetClientRect(FHandle, PreviewRect);
    PreviewRect.Top := PreviewRect.Bottom - 43;
    FFavoritePanel.BoundsRect := PreviewRect;
    FFavoritePanel.ParentWindow := FHandle;
    AdjustControlPos;
    FFavoriteComboBox.Items.Assign(FavoriteFolders);
  end;
end;

procedure TJclOpenDialogFavoritesHook.FavoriteComboBoxClick(Sender: TObject);
begin
  with FFavoriteComboBox do
    if ItemIndex <> - 1 then
      CurrentFolder := FFavoriteComboBox.Items[ItemIndex];
end;

procedure TJclOpenDialogFavoritesHook.LoadFavorites(const FileName: string);
begin
  if FileExists(FileName) then
    FavoriteFolders.LoadFromFile(FileName)
  else
    FavoriteFolders.Clear;
end;

procedure TJclOpenDialogFavoritesHook.SetDeleteMode(const Value: Boolean);
begin
  if FDeleteMode <> Value then
  begin
    FDeleteMode := Value;
    if FDeleteMode then
      FAddButton.Caption := LoadResString(@RsOpenDialogDelete)
    else
      FAddButton.Caption := LoadResString(@RsOpenDialogAdd);
    FFavoriteComboBox.Invalidate;
  end;
end;

procedure TJclOpenDialogFavoritesHook.WndProc(var Message: TMessage);
begin
  if FHandle <> 0 then
  begin
    case Message.Msg of
      WM_DESTROY:
        begin
          if not IsOpenPictureDialog then
            FavoriteFolders.Assign(FFavoriteComboBox.Items);
          try
            DoClose;
            inherited WndProc(Message);
          finally
            if not IsOpenPictureDialog then
              FFavoritePanel.ParentWindow := 0;
            FParentWnd := 0;
          end;
        end;
    else
      inherited WndProc(Message);
    end;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
