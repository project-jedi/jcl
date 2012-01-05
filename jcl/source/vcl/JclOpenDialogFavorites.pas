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
    FFavoriteComboBox: TComboBox;
    FFavoriteFolders: TStrings;
    FFavoriteStaticText: TStaticText;
    FFavoritePanel: TPanel;
    FTextAdd: string;
    FTextDelete: string;
    FTextVirtual: string;
    procedure FavoriteComboBoxClick(Sender: TObject);
  protected
    procedure DialogAdjustControlPos; override;
    procedure DialogFolderChange; override;
    procedure DialogShow; override;
    procedure DialogClose; override;
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
  FFavoritePanel.Name := 'FavoritePanel';
  FFavoritePanel.BevelOuter := bvNone;
  FFavoritePanel.Caption := '';
  FFavoritePanel.FullRepaint := False;

  FFavoriteComboBox := TComboBox.Create(nil);
  FFavoriteComboBox.Parent := FFavoritePanel;
  FFavoriteComboBox.Align := alClient;
  FFavoriteComboBox.Style := csDropDownList;
  FFavoriteComboBox.Sorted := True;
  FFavoriteComboBox.OnClick := FavoriteComboBoxClick;

  FFavoriteStaticText := TStaticText.Create(nil);
  FFavoriteStaticText.SetBounds(6, 18, FFavoriteStaticText.Width, FFavoriteStaticText.Height);
  FFavoriteStaticText.Caption := LoadResString(@RsOpenDialogFavorites);
  FFavoriteStaticText.AutoSize := True;
  FFavoriteStaticText.FocusControl := FFavoriteComboBox;

  FTextAdd := LoadResString(@RsOpenDialogAdd);
  FTextDelete := LoadResString(@RsOpenDialogDelete);
  FTextVirtual := LoadResString(@RsOpenDialogVirtual);
end;

destructor TJclOpenDialogFavoritesHook.Destroy;
begin
  FreeAndNil(FFavoriteComboBox);
  FreeAndNil(FFavoritePanel);
  FreeAndNil(FFavoriteStaticText);
  FreeAndNil(FFavoriteFolders);
  inherited Destroy;
end;

procedure TJclOpenDialogFavoritesHook.DialogAdjustControlPos;
var
  FileTypeStaticTextRect, FileTypeEditRect,          // ID = 1136 1089
  FileNameStaticTextRect, FileNameEditRect: TRect;   // ID = 1148 1090

  procedure GetDlgItemRect(ItemID: Integer; var R: TRect);
  begin
    GetWindowRect(GetDlgItem(FParentWnd, ItemID), R);
    MapWindowPoints(0, FParentWnd, R, 2);
  end;

begin
  inherited DialogAdjustControlPos;

  GetDlgItemRect(stc2, FileTypeStaticTextRect);
  GetDlgItemRect(cmb1, FileTypeEditRect);
  GetDlgItemRect(stc3, FileNameStaticTextRect);
  GetDlgItemRect(cmb13, FileNameEditRect);

  FFavoriteStaticText.Left := FileTypeStaticTextRect.Left;
  FFavoriteStaticText.Top := 2 * FileTypeStaticTextRect.Top - FileNameStaticTextRect.Top;

  FFavoritePanel.Left := FileNameEditRect.Left;
  FFavoritePanel.Top := 2 * FileTypeEditRect.Top - FileNameEditRect.Top;
  FFavoritePanel.Width := FileTypeEditRect.Right - FileTypeEditRect.Left;
end;

procedure TJclOpenDialogFavoritesHook.DialogClose;
begin
  inherited DialogClose;
  if not IsOpenPictureDialog then
  begin
    FFavoriteComboBox.Items.Delete(0);
    FavoriteFolders.Assign(FFavoriteComboBox.Items);
  end;
  FFavoritePanel.ParentWindow := 0;
  FFavoriteStaticText.ParentWindow := 0;
  FParentWnd := 0;
end;

procedure TJclOpenDialogFavoritesHook.DialogFolderChange;
var
  Path: string;
begin
  inherited DialogFolderChange;
  Path := CurrentFolder;
  FFavoriteComboBox.ItemIndex := FFavoriteComboBox.Items.IndexOf(Path);
  if FFavoriteComboBox.ItemIndex = -1 then
  begin
    if Path <> '' then
      FFavoriteComboBox.Items[0] := FTextAdd
    else
      FFavoriteComboBox.Items[0] := FTextVirtual;
    FFavoriteComboBox.ItemIndex := 0;
  end
  else
    FFavoriteComboBox.Items[0] := FTextDelete;
  FFavoriteComboBox.Invalidate;
end;

procedure TJclOpenDialogFavoritesHook.DialogShow;
begin
  inherited DialogShow;
  if not IsOpenPictureDialog then
  begin
    FFavoritePanel.ParentWindow := FHandle;
    FFavoriteStaticText.ParentWindow := FHandle;
    FFavoriteComboBox.Items.Assign(FavoriteFolders);
    FFavoriteComboBox.Items.Insert(0, FTextAdd);
  end;
end;

procedure TJclOpenDialogFavoritesHook.FavoriteComboBoxClick(Sender: TObject);
var
  I: Integer;
  Path: string;
begin
  if FFavoriteComboBox.ItemIndex = 0 then
  begin
    Path := CurrentFolder;
    I := FFavoriteComboBox.Items.IndexOf(Path);
    if I > 0 then
    begin
      // delete current folder
      if MessageBox(FHandle,
                    PChar(Format(LoadResString(@RsOpenDialogDelConfirm), [Path])),
                    PChar(LoadResString(@RsOpenDialogConfirmation)),
                    MB_YESNO or MB_ICONQUESTION or MB_DEFBUTTON2) = ID_YES then
      begin
        FFavoriteComboBox.Items.Delete(I);
        FFavoriteComboBox.Items[0] := FTextAdd;
        FFavoriteComboBox.ItemIndex := 0;
      end;
    end
    else
    if Path <> '' then
    begin
      // add current folder
      FFavoriteComboBox.ItemIndex := FFavoriteComboBox.Items.Add(Path);
      FFavoriteComboBox.Items[0] := FTextDelete;
    end;
    FFavoriteComboBox.Invalidate;
  end
  else
  if FFavoriteComboBox.ItemIndex > 0 then
    // switch to selected folder
    CurrentFolder := FFavoriteComboBox.Items[FFavoriteComboBox.ItemIndex];
end;

procedure TJclOpenDialogFavoritesHook.LoadFavorites(const FileName: string);
begin
  if FileExists(FileName) then
    FavoriteFolders.LoadFromFile(FileName)
  else
    FavoriteFolders.Clear;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
