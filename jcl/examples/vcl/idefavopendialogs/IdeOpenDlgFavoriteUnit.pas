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
{ The Original Code is IdeOpenDlgFavoriteUnit.pas.                                                 }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Unit owner: Petr Vones                                                                           }
{ Last modified: June 12, 2002                                                                     }
{                                                                                                  }
{**************************************************************************************************}

unit IdeOpenDlgFavoriteUnit;

interface

{$I jcl.inc}

uses
  ToolsAPI, OpenDlgFavAdapter, JclOtaUtils;

type
  TOpenDialogsFavoriteExpert = class(TJclOTAExpert)
  private
    FFavOpenDialog: TFavOpenDialog;
    procedure DialogClose(Sender: TObject);
    procedure DialogShow(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
  end;

procedure Register;

implementation

uses
  JclIniFiles, JclFileUtils, JclSysInfo;

const
  FavoritesSectionName     = 'IdeFavoriteFolders';
  PictDialogFolderItemName = 'PictureDialogPath';

  BorlandImagesPath        = 'Borland Shared\Images';

procedure Register;
begin
  RegisterPackageWizard(TOpenDialogsFavoriteExpert.Create);
end;

//==================================================================================================
// TOpenDialogsFavoriteExpert
//==================================================================================================

constructor TOpenDialogsFavoriteExpert.Create;
begin
  inherited Create;
  FFavOpenDialog := InitializeFavOpenDialog;
  FFavOpenDialog.DisableHelpButton := True;
  FFavOpenDialog.HookDialogs;
  FFavOpenDialog.OnClose := DialogClose;
  FFavOpenDialog.OnShow := DialogShow;
  FFavOpenDialog.PictureDialogLastFolder := JediIniFile.ReadString(FavoritesSectionName,
    PictDialogFolderItemName, PathAddSeparator(GetCommonFilesFolder) + BorlandImagesPath);
end;

//--------------------------------------------------------------------------------------------------

destructor TOpenDialogsFavoriteExpert.Destroy;
begin
  FFavOpenDialog.UnhookDialogs;
  inherited;
end;

//--------------------------------------------------------------------------------------------------

procedure TOpenDialogsFavoriteExpert.DialogClose(Sender: TObject);
begin
  IniWriteStrings(JediIniFile, FavoritesSectionName, FFavOpenDialog.FavoriteFolders);
  JediIniFile.WriteString(FavoritesSectionName, PictDialogFolderItemName, FFavOpenDialog.PictureDialogLastFolder);
end;

//--------------------------------------------------------------------------------------------------

procedure TOpenDialogsFavoriteExpert.DialogShow(Sender: TObject);
begin
  IniReadStrings(JediIniFile, FavoritesSectionName, FFavOpenDialog.FavoriteFolders);
end;

//--------------------------------------------------------------------------------------------------

end.
