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
  SysUtils,
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
    procedure RegisterCommands; override;
    procedure UnregisterCommands; override;
  end;

// design package entry point
procedure Register;

// expert DLL entry point
function JCLWizardInit(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var TerminateProc: TWizardTerminateProc): Boolean; stdcall;

implementation

uses
  JclFileUtils, JclSysInfo,
  JclOtaConsts, JclOtaResources;

procedure Register;
begin
  try
    RegisterPackageWizard(TJclOpenDialogsFavoriteExpert.Create);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

var
  JCLWizardIndex: Integer = -1;

procedure JclWizardTerminate;
var
  OTAWizardServices: IOTAWizardServices;
begin
  try
    if JCLWizardIndex <> -1 then
    begin
      Supports(BorlandIDEServices, IOTAWizardServices, OTAWizardServices);
      if not Assigned(OTAWizardServices) then
        raise EJclExpertException.CreateTrace(RsENoWizardServices);

      OTAWizardServices.RemoveWizard(JCLWizardIndex);
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

function JCLWizardInit(const BorlandIDEServices: IBorlandIDEServices;
    RegisterProc: TWizardRegisterProc;
    var TerminateProc: TWizardTerminateProc): Boolean stdcall;
var
  OTAWizardServices: IOTAWizardServices;
begin
  try
    TerminateProc := JclWizardTerminate;

    Supports(BorlandIDEServices, IOTAWizardServices, OTAWizardServices);
    if not Assigned(OTAWizardServices) then
      raise EJclExpertException.CreateTrace(RsENoWizardServices);

    JCLWizardIndex := OTAWizardServices.AddWizard(TJclOpenDialogsFavoriteExpert.Create);

    Result := True;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      Result := False;
    end;
  end;
end;

constructor TJclOpenDialogsFavoriteExpert.Create;
begin
  inherited Create(JclFavoritesExpertName);
end;

procedure TJclOpenDialogsFavoriteExpert.DialogClose(Sender: TObject);
begin
  Settings.SaveStrings(JclFavoritesListSubKey, FFavOpenDialog.FavoriteFolders);
  Settings.SaveString(PictDialogFolderItemName, FFavOpenDialog.PictureDialogLastFolder);
end;

procedure TJclOpenDialogsFavoriteExpert.DialogShow(Sender: TObject);
begin
  Settings.LoadStrings(JclFavoritesListSubKey, FFavOpenDialog.FavoriteFolders);
end;

procedure TJclOpenDialogsFavoriteExpert.RegisterCommands;
begin
  inherited RegisterCommands;
  FFavOpenDialog := InitializeFavOpenDialog;
  FFavOpenDialog.DisableHelpButton := True;
  FFavOpenDialog.HookDialogs;
  FFavOpenDialog.OnClose := DialogClose;
  FFavOpenDialog.OnShow := DialogShow;
  FFavOpenDialog.PictureDialogLastFolder := Settings.LoadString(PictDialogFolderItemName,
    PathAddSeparator(GetCommonFilesFolder) + BorlandImagesPath);
end;

procedure TJclOpenDialogsFavoriteExpert.UnregisterCommands;
begin
  FFavOpenDialog.UnhookDialogs;
  inherited UnregisterCommands;
end;

// History:

// $Log$
// Revision 1.9  2006/03/26 20:22:19  outchy
// Command registration moved out of expert constructors and destructors
//
// Revision 1.8  2006/01/08 17:16:57  outchy
// Settings reworked.
// Common window for expert configurations
//
// Revision 1.7  2005/12/26 18:03:41  outchy
// Enhanced bds support (including C#1 and D8)
// Introduction of dll experts
// Project types in templates
//
// Revision 1.6  2005/12/16 23:46:25  outchy
// Added expert stack form.
// Added code to display call stack on expert exception.
// Fixed package extension for D2006.
//
// Revision 1.5  2005/10/26 03:29:44  rrossmair
// - improved header information, added $Date$ and $Log$
// - improved header information, added $Date: 2006/01/08 17:16:57 $ and Revision 1.9  2006/03/26 20:22:19  outchy
// - improved header information, added $Date: 2006/01/08 17:16:57 $ and Command registration moved out of expert constructors and destructors
// - improved header information, added $Date: 2006/01/08 17:16:57 $ and
// - improved header information, added $Date$ and Revision 1.8  2006/01/08 17:16:57  outchy
// - improved header information, added $Date$ and Settings reworked.
// - improved header information, added $Date$ and Common window for expert configurations
// - improved header information, added $Date$ and
// - improved header information, added $Date$ and Revision 1.7  2005/12/26 18:03:41  outchy
// - improved header information, added $Date$ and Enhanced bds support (including C#1 and D8)
// - improved header information, added $Date$ and Introduction of dll experts
// - improved header information, added $Date$ and Project types in templates
// - improved header information, added $Date$ and
// - improved header information, added $Date$ and Revision 1.6  2005/12/16 23:46:25  outchy
// - improved header information, added $Date$ and Added expert stack form.
// - improved header information, added $Date$ and Added code to display call stack on expert exception.
// - improved header information, added $Date$ and Fixed package extension for D2006.
// - improved header information, added $Date$ and CVS tags.
//

end.
