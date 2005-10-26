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
{ The Initial Developer of the Original Code is Petr Vones.                                        }
{ Portions created by Petr Vones are Copyright (C) of Petr Vones.                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Unit owner: Petr Vones                                                                           }
{ Last modified: $Date$                                                      }
{                                                                                                  }
{**************************************************************************************************}

unit IdeOpenDlgFavoriteUnit;

interface

{$I jcl.inc}

uses
  ToolsAPI, OpenDlgFavAdapter,
  JclOtaUtils;

type
  TJclOpenDialogsFavoriteExpert = class(TJclOTAExpert)
  private
    FFavOpenDialog: TFavOpenDialog;
    procedure DialogClose(Sender: TObject);
    procedure DialogShow(Sender: TObject);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

procedure Register;

implementation

uses
  JclFileUtils, JclOtaConsts, JclSysInfo;

procedure Register;
begin
  RegisterPackageWizard(TJclOpenDialogsFavoriteExpert.Create);
end;

constructor TJclOpenDialogsFavoriteExpert.Create;
begin
  inherited Create(JclFavoritesExpertName);
  FFavOpenDialog := InitializeFavOpenDialog;
  FFavOpenDialog.DisableHelpButton := True;
  FFavOpenDialog.HookDialogs;
  FFavOpenDialog.OnClose := DialogClose;
  FFavOpenDialog.OnShow := DialogShow;
  FFavOpenDialog.PictureDialogLastFolder := LoadString(PictDialogFolderItemName,
    PathAddSeparator(GetCommonFilesFolder) + BorlandImagesPath);
end;

destructor TJclOpenDialogsFavoriteExpert.Destroy;
begin
  FFavOpenDialog.UnhookDialogs;
  inherited Destroy;
end;

procedure TJclOpenDialogsFavoriteExpert.DialogClose(Sender: TObject);
begin
  SaveStrings(JclFavoritesListSubKey, FFavOpenDialog.FavoriteFolders);
  SaveString(PictDialogFolderItemName, FFavOpenDialog.PictureDialogLastFolder);
end;

procedure TJclOpenDialogsFavoriteExpert.DialogShow(Sender: TObject);
begin
  LoadStrings(JclFavoritesListSubKey, FFavOpenDialog.FavoriteFolders);
end;

// History:

// $Log$
// Revision 1.5  2005/10/26 03:29:44  rrossmair
// - improved header information, added Date and Log CVS tags.
//

end.
