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
{ The Original Code is VersionControlImpl.pas.                                                     }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Unit owner: Elahn Ientile                                                                        }
{ Last modified: $Date$                                                      }
{ Revision: $Revision$                                                                       }
{                                                                                                  }
{**************************************************************************************************}

unit VersionControlImpl;

{$I jcl.inc}

interface

uses
  Classes, Menus, ActnList, ToolsAPI, SysUtils, Graphics, Dialogs, JclOtaUtils;

type
  TJclVersionControlExpert = class(TJclOTAExpert)
  private
    FMenu: TMenuItem;
    FmnuDiffFile: TMenuItem;
    FactDiffFile: TAction;
    FmnuBlameFile: TMenuItem;
    FactBlameFile: TAction;
    FmnuGraphFile: TMenuItem;
    FactGraphFile: TAction;
    FmnuProperties: TMenuItem;
    FactProperties: TAction;
    FmnuExplore: TMenuItem;
    FactExplore: TAction;
    FmnuContextMenu: TMenuItem;
    FactContextMenu: TAction;
    FmnuStatusFile: TMenuItem;
    FactStatusFile: TAction;
    FmnuStatusSandbox: TMenuItem;
    FactStatusSandbox: TAction;
    FmnuLogFile: TMenuItem;
    FactLogFile: TAction;
    FmnuLockFile: TMenuItem;
    FactLockFile: TAction;
    FmnuUnlockFile: TMenuItem;
    FactUnlockFile: TAction;
    FmnuLogSandbox: TMenuItem;
    FactLogSandbox: TAction;
    FmnuRevertFile: TMenuItem;
    FactRevertFile: TAction;
    FmnuRevertSandbox: TMenuItem;
    FactRevertSandbox: TAction;
    FmnuUpdateFile: TMenuItem;
    FactUpdateFile: TAction;
    FmnuUpdateSandbox: TMenuItem;
    FactUpdateSandbox: TAction;
    FmnuCommitFile: TMenuItem;
    FactCommitFile: TAction;
    FmnuCommitSandbox: TMenuItem;
    FactCommitSandbox: TAction;
    FmnuRepoBrowser: TMenuItem;
    FactRepoBrowser: TAction;
    FmnuSeperator1: TMenuItem;
    FmnuSeperator2: TMenuItem;
    FmnuSeperator3: TMenuItem;
    FmnuSeperator4: TMenuItem;
    FmnuSeperator5: TMenuItem;
    FmnuSeperator6: TMenuItem;
    FmnuSeperator7: TMenuItem;

    FTortoiseProc: string;

    procedure MenuClick(Sender: TObject);
    procedure actDiffFileExecute(Sender: TObject);
    procedure actBlameFileExecute(Sender: TObject);
    procedure actGraphFileExecute(Sender: TObject);
    procedure actPropertiesExecute(Sender: TObject);
    procedure actExploreExecute(Sender: TObject);
    procedure actContextMenuExecute(Sender: TObject);
    procedure actStatusFileExecute(Sender: TObject);
    procedure actStatusSandboxExecute(Sender: TObject);
    procedure actLogFileExecute(Sender: TObject);
    procedure actLogSandboxExecute(Sender: TObject);
    procedure actLockFileExecute(Sender: TObject);
    procedure actUnlockFileExecute(Sender: TObject);
    procedure actRevertFileExecute(Sender: TObject);
    procedure actRevertSandboxExecute(Sender: TObject);
    procedure actUpdateFileExecute(Sender: TObject);
    procedure actUpdateSandboxExecute(Sender: TObject);
    procedure actCommitFileExecute(Sender: TObject);
    procedure actCommitSandboxExecute(Sender: TObject);
    procedure actRepoBrowserExecute(Sender: TObject);

    function GetTortoiseProc: Boolean;
    function ActiveModuleFileName(ASave, IncludeFamily: Boolean): string;
    function SandboxPath: string;
    function ExtractFirstPath(PathList: string): string;
    function LaunchApp(exeName, params, currentDir: string; windowState: WORD): Boolean;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure RegisterCommands; override;
    procedure UnregisterCommands; override;
  end;

procedure Register;

implementation

{$R JclVersionControlImagesSVN.res}

uses
  JclDebug, JclFileUtils, JclRegistry, JclOtaConsts, JclOtaResources,
  Windows, JclShell, Controls;

procedure Register;
begin
  try
    RegisterPackageWizard(TJclVersionControlExpert.Create);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

//=== VersionControlImpl.pas ===================================================
resourcestring
  RsSvnMenuCaption = 'Subversio&n';
  RsSvnMenuName = 'SubversionMenu';
  RsSvnMenuActionName = 'SubversionMenuCommand';
  RsSvnActDiffFileCaption = '&Diff';
  RsSvnActDiffFileName = 'SvnDiffFileCommand';
  RsSvnMnuDiffFileName = 'SvnDiffFileMenu';
  RsSvnActBlameFileCaption = '&Blame';
  RsSvnActBlameFileName = 'SvnBlameFileCommand';
  RsSvnMnuBlameFileName = 'SvnBlameFileMenu';
  RsSvnActGraphFileCaption = 'Revision Gr&aph';
  RsSvnActGraphFileName = 'SvnGraphFileCommand';
  RsSvnMnuGraphFileName = 'SvnGraphFileMenu';
  RsSvnActPropertiesCaption = 'Pr&operties';
  RsSvnActPropertiesName = 'SvnPropertiesCommand';
  RsSvnMnuPropertiesName = 'SvnPropertiesMenu';
  RsSvnActExploreCaption = 'E&xplore Folder';
  RsSvnActExploreName = 'SvnExploreCommand';
  RsSvnMnuExploreName = 'SvnExploreMenu';
  RsSvnActContextMenuCaption = 'Co&ntext Menu (right-click)';
  RsSvnActContextMenuName = 'SvnContextMenuCommand';
  RsSvnMnuContextMenuName = 'SvnContextMenuMenu';
  RsSvnActStatusFileCaption = 'S&tatus - File';
  RsSvnActStatusFileName = 'SvnStatusFileCommand';
  RsSvnMnuStatusFileName = 'SvnStatusFileMenu';
  RsSvnActStatusSandboxCaption = '&Status - Sandbox';
  RsSvnActStatusSandboxName = 'SvnStatusSandboxCommand';
  RsSvnMnuStatusSandboxName = 'SvnStatusSandboxMenu';
  RsSvnActLogFileCaption = '&Log - File';
  RsSvnActLogFileName = 'SvnLogFileCommand';
  RsSvnMnuLogFileName = 'SvnLogFileMenu';
  RsSvnActLogSandboxCaption = 'Lo&g - Sandbox';
  RsSvnActLogSandboxName = 'SvnLogSandboxCommand';
  RsSvnMnuLogSandboxName = 'SvnLogSandboxMenu';
  RsSvnActLockFileCaption = 'Loc&k - File';
  RsSvnActLockFileName = 'SvnLockFileCommand';
  RsSvnMnuLockFileName = 'SvnLockFileMenu';
  RsSvnActUnlockFileCaption = 'Unlock - &File';
  RsSvnActUnlockFileName = 'SvnUnlockFileCommand';
  RsSvnMnuUnlockFileName = 'SvnUnlockFileMenu';
  RsSvnActRevertFileCaption = '&Revert - File';
  RsSvnActRevertFileName = 'SvnRevertFileCommand';
  RsSvnMnuRevertFileName = 'SvnRevertFileMenu';
  RsSvnActRevertSandboxCaption = 'R&evert - Sandbox';
  RsSvnActRevertSandboxName = 'SvnRevertSandboxCommand';
  RsSvnMnuRevertSandboxName = 'SvnRevertSandboxMenu';
  RsSvnActUpdateFileCaption = 'U&pdate - File';
  RsSvnActUpdateFileName = 'SvnUpdateFileCommand';
  RsSvnMnuUpdateFileName = 'SvnUpdateFileMenu';
  RsSvnActUpdateSandboxCaption = '&Update - Sandbox';
  RsSvnActUpdateSandboxName = 'SvnUpdateSandboxCommand';
  RsSvnMnuUpdateSandboxName = 'SvnUpdateSandboxMenu';
  RsSvnActCommitFileCaption = 'Co&mmit - File';
  RsSvnActCommitFileName = 'SvnCommitFileCommand';
  RsSvnMnuCommitFileName = 'SvnCommitFileMenu';
  RsSvnActCommitSandboxCaption = '&Commit - Sandbox';
  RsSvnActCommitSandboxName = 'SvnCommitSandboxCommand';
  RsSvnMnuCommitSandboxName = 'SvnCommitSandboxMenu';
  RsSvnActRepoBrowserCaption = 'Repositor&y Browser';
  RsSvnActRepoBrowserName = 'SvnRepoBrowserCommand';
  RsSvnMnuRepoBrowserName = 'SvnRepoBrowserMenu';

  RsSvnMenuItemNotInserted = 'Can''t insert the ''%s'' menu item';
  RsENoToolsMenuItem = 'Tools menu item not found';
  RsTortoiseProcNotFound = 'TortoiseProc could not be found.'
    + #13#10'This probably means that TortoiseSVN is not installed.';
  RsENoActionServices = 'Unable to get Borland Action Services';
  RsErrorClosingFile = 'Error closing file: %s.'
    + #13#10#13#10'%s'#13#10#13#10'Reloading of modified files has been aborted!';
  RsErrorClosingDeletedFile = 'the file has been Deleted and does not need to be reloaded';
  RsErrorClosingModifiedFile = 'the file has been Modified and should be reloaded';
  RsErrorReOpeningFile = 'Error re-opening file:'#13#10#13#10'%s';
  RsNotUnderVersionControl = 'This folder is not under Subversion Version Control.';

//=== { TJclVersionControlExpert } ===================================================


{ TJclVersionControlExpert }

procedure TJclVersionControlExpert.actBlameFileExecute(Sender: TObject);
var
  lFileName: string;
begin
  if GetTortoiseProc then
  begin
    lFileName := ActiveModuleFileName(True, False);
    LaunchApp(FTortoiseProc, '/command:blame /path:"' + lFileName + '" /notempfile',
      ExtractFilePath(lFileName), SW_SHOW);
  end;
end;

procedure TJclVersionControlExpert.actCommitFileExecute(Sender: TObject);
var
  lFileName: string;
begin
  if GetTortoiseProc then
  begin
    lFileName := ActiveModuleFileName(True, True);
    LaunchApp(FTortoiseProc, '/command:commit /path:"' + lFileName + '" /notempfile',
      ExtractFirstPath(lFileName), SW_SHOW);
  end;
end;

procedure TJclVersionControlExpert.actCommitSandboxExecute(Sender: TObject);
var
  lPath: string;
begin
  if GetTortoiseProc then
  begin
    lPath := SandboxPath;
    LaunchApp(FTortoiseProc, '/command:commit /path:"' + lPath + '" /notempfile',
      lPath, SW_SHOW);
  end;
end;

procedure TJclVersionControlExpert.actContextMenuExecute(Sender: TObject);
var
  lFileName: string;
begin
  lFileName := ActiveModuleFileName(True, False);
  DisplayContextMenu(0, lFileName, Mouse.CursorPos);
end;

procedure TJclVersionControlExpert.actDiffFileExecute(Sender: TObject);
var
  lFileName: string;
begin
  if GetTortoiseProc then
  begin
    lFileName := ActiveModuleFileName(True, False);
    LaunchApp(FTortoiseProc, '/command:diff /path:"' + lFileName + '" /notempfile',
      ExtractFilePath(lFileName), SW_SHOW);
  end;
end;

procedure TJclVersionControlExpert.actExploreExecute(Sender: TObject);
var
  lPath: string;
begin
  lPath := ExtractFilePath(ActiveModuleFileName(True, False));
  OpenFolder(lPath, 0, True);
end;

procedure TJclVersionControlExpert.actGraphFileExecute(Sender: TObject);
var
  lFileName: string;
begin
  if GetTortoiseProc then
  begin
    lFileName := ActiveModuleFileName(True, False);
    LaunchApp(FTortoiseProc, '/command:revisiongraph /path:"' + lFileName + '" /notempfile',
      ExtractFilePath(lFileName), SW_SHOW);
  end;
end;

function TJclVersionControlExpert.ActiveModuleFileName(ASave, IncludeFamily: Boolean): string;
var
  ModuleServices: IOTAModuleServices;
  lModule: IOTAModule;
  lExt, lFileName: string;
begin
  try
    Supports(BorlandIDEServices, IOTAModuleServices, ModuleServices);
    if not Assigned(ModuleServices) then
      raise EJclExpertException.CreateTrace(RsENoModuleServices);
    lModule := ModuleServices.CurrentModule;
    if (lModule = nil) or (lModule.FileSystem <> '') then
      Result := ''
    else
    begin
      Result := lModule.CurrentEditor.FileName;
      if ASave then
        lModule.Save(False, False);
      if IncludeFamily and (lModule.ModuleFileCount > 1) then
      begin
        lExt := ExtractFileExt(Result);
        if (lExt = '.pas') then
        begin
          lFileName := ChangeFileExt(Result, '.dfm');
          if FileExists(lFileName) then
            Result := Result + '*' + lFileName;
        end
        else if (lExt = '.dfm') then
        begin
          lFileName := ChangeFileExt(Result, '.pas');
          if FileExists(lFileName) then
            Result := Result + '*' + lFileName;
        end
      end;
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TJclVersionControlExpert.actLockFileExecute(Sender: TObject);
var
  lFileName: string;
begin
  if GetTortoiseProc then
  begin
    lFileName := ActiveModuleFileName(True, True);
    LaunchApp(FTortoiseProc, '/command:lock /path:"' + lFileName + '" /notempfile',
      ExtractFirstPath(lFileName), SW_SHOW);
  end;
end;

procedure TJclVersionControlExpert.actLogFileExecute(Sender: TObject);
var
  lFileName: string;
begin
  if GetTortoiseProc then
  begin
    lFileName := ActiveModuleFileName(True, False);
    LaunchApp(FTortoiseProc, '/command:log /path:"' + lFileName + '" /notempfile',
      ExtractFilePath(lFileName), SW_SHOW);
  end;
end;

procedure TJclVersionControlExpert.actLogSandboxExecute(Sender: TObject);
var
  lPath: string;
begin
  if GetTortoiseProc then
  begin
    lPath := SandboxPath;
    LaunchApp(FTortoiseProc, '/command:log /path:"' + lPath + '" /notempfile',
      lPath, SW_SHOW);
  end;
end;

procedure TJclVersionControlExpert.actPropertiesExecute(Sender: TObject);
var
  lFileName: string;
begin
  lFileName := ActiveModuleFileName(True, False);
  DisplayPropDialog(0, lFileName);
end;

procedure TJclVersionControlExpert.actRepoBrowserExecute(Sender: TObject);
var
  lPath: string;
begin
  if GetTortoiseProc then
  begin
    lPath := ExtractFilePath(ActiveModuleFileName(False, False));
    LaunchApp(FTortoiseProc, '/command:repobrowser /path:"' + lPath + '" /notempfile',
      lPath, SW_SHOW);
  end;
end;

procedure TJclVersionControlExpert.actRevertFileExecute(Sender: TObject);
var
  lFileName: string;
begin
  if GetTortoiseProc then
  begin
    lFileName := ActiveModuleFileName(True, True);
    LaunchApp(FTortoiseProc, '/command:revert /path:"' + lFileName + '" /notempfile',
      ExtractFirstPath(lFileName), SW_SHOW);
  end;
end;

procedure TJclVersionControlExpert.actRevertSandboxExecute(Sender: TObject);
var
  lPath: string;
begin
  if GetTortoiseProc then
  begin
    lPath := SandboxPath;
    LaunchApp(FTortoiseProc, '/command:revert /path:"' + lPath + '" /notempfile',
      lPath, SW_SHOW);
  end;
end;

procedure TJclVersionControlExpert.actStatusFileExecute(Sender: TObject);
var
  lFileName: string;
begin
  if GetTortoiseProc then
  begin
    lFileName := ActiveModuleFileName(True, True);
    LaunchApp(FTortoiseProc, '/command:repostatus /path:"' + lFileName + '" /notempfile',
      ExtractFirstPath(lFileName), SW_SHOW);
  end;
end;

procedure TJclVersionControlExpert.actStatusSandboxExecute(Sender: TObject);
var
  lPath: string;
begin
  if GetTortoiseProc then
  begin
    lPath := SandboxPath;
    LaunchApp(FTortoiseProc, '/command:repostatus /path:"' + lPath + '" /notempfile',
      lPath, SW_SHOW);
  end;
end;

procedure TJclVersionControlExpert.actUnlockFileExecute(Sender: TObject);
var
  lFileName: string;
begin
  if GetTortoiseProc then
  begin
    lFileName := ActiveModuleFileName(True, True);
    LaunchApp(FTortoiseProc, '/command:unlock /path:"' + lFileName + '" /notempfile',
      ExtractFirstPath(lFileName), SW_SHOW);
  end;
end;

procedure TJclVersionControlExpert.actUpdateFileExecute(Sender: TObject);
var
  lFileName: string;
begin
  if GetTortoiseProc then
  begin
    lFileName := ActiveModuleFileName(True, True);
    LaunchApp(FTortoiseProc, '/command:update /path:"' + lFileName + '" /notempfile',
      ExtractFirstPath(lFileName), SW_SHOW);
  end;
end;

procedure TJclVersionControlExpert.actUpdateSandboxExecute(Sender: TObject);
var
  lPath: string;
begin
  if GetTortoiseProc then
  begin
    lPath := SandboxPath;
    LaunchApp(FTortoiseProc, '/command:update /path:"' + lPath + '" /notempfile',
      lPath, SW_SHOW);
  end;
end;

constructor TJclVersionControlExpert.Create;
begin
  inherited Create('JclVersionControlExpert');
end;

destructor TJclVersionControlExpert.Destroy;
begin
  inherited;
end;

function TJclVersionControlExpert.ExtractFirstPath(PathList: string): string;
var
  i: Integer;
begin
  i := Pos('*', PathList);
  if i > 0 then
    Result := ExtractFilePath(Copy(PathList, 1, i))
  else
    Result := ExtractFilePath(PathList);
end;

function TJclVersionControlExpert.GetTortoiseProc: Boolean;
begin
  try
    if FTortoiseProc <> '' then
      Result := True
    else
    begin
      FTortoiseProc := RegReadString(HKLM, 'SOFTWARE\TortoiseSVN', 'ProcPath');
      if FTortoiseProc = '' then
      begin
        Result := False;
        ShowMessage(RsTortoiseProcNotFound);
      end
      else
        Result := True;
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

function TJclVersionControlExpert.LaunchApp(exeName, params, currentDir: string;
  windowState: WORD): Boolean;
var
  startupInfo: TStartupInfo;
  processInfo: TProcessInformation;
begin
  FillChar(StartupInfo,SizeOf(TStartupInfo),#0);
  FillChar(ProcessInfo,SizeOf(TProcessInformation),#0);
  startupInfo.cb := SizeOf(TStartupInfo);
  startupInfo.dwFlags := STARTF_USESHOWWINDOW;
  startupInfo.wShowWindow := windowState;

  Result := CreateProcess(nil, PChar(exeName + ' ' + params), nil,
    nil, False, 0, nil, PChar(currentDir), startupInfo, processInfo);
end;

procedure TJclVersionControlExpert.MenuClick(Sender: TObject);
begin
end;

procedure TJclVersionControlExpert.RegisterCommands;
var
  IDEMainMenu: TMainMenu;
  IDEToolsItem: TMenuItem;
  IDEActionList: TActionList;
  I: Integer;
  ImageBmp: Graphics.TBitmap;
  procedure LoadImageFor(Action: TAction; Resource: string);
  begin
    ImageBmp := Graphics.TBitmap.Create;
    try
      ImageBmp.LoadFromResourceName(FindResourceHInstance(HInstance), Resource);
      Action.ImageIndex := NTAServices.AddMasked(ImageBmp, clOlive);
    finally
      ImageBmp.Free;
    end;
  end;
  procedure CreateMenuItem(var ActionPtr: TAction; var MenuPtr: TMenuItem;
      ActionCaption, ActionName, MenuName: string; AMethod: TNotifyEvent);
  begin
    ActionPtr := TAction.Create(nil);
    ActionPtr.Caption := ActionCaption;
    ActionPtr.Visible := True;
    ActionPtr.OnExecute := AMethod;
    ActionPtr.Name := ActionName;
    ActionPtr.ActionList := IDEActionList;
    RegisterAction(ActionPtr);
    MenuPtr := TMenuItem.Create(FMenu);
    MenuPtr.Action := ActionPtr;
    MenuPtr.Name := MenuName;
    FMenu.Add(MenuPtr);
    if not Assigned(MenuPtr.Parent) then
      raise EJclExpertException.CreateTrace(Format(RsSvnMenuItemNotInserted, [ActionCaption]));
  end;
  procedure AddMenuSeparator(var MenuPtr: TMenuItem);
  begin
    MenuPtr := TMenuItem.Create(FMenu);
    MenuPtr.Caption := cLineCaption;
    FMenu.Add(MenuPtr);
  end;
begin
  inherited RegisterCommands;
  try
    IDEMainMenu := NTAServices.MainMenu;
    IDEToolsItem := nil;
    with IDEMainMenu do
      for I := 0 to Items.Count - 1 do
        if Items[I].Name = 'ToolsMenu' then
        begin
          IDEToolsItem := Items[I];
          Break;
        end;
    if not Assigned(IDEToolsItem) then
      raise EJclExpertException.CreateTrace(RsENoToolsMenuItem);

    IDEActionList := TActionList(NTAServices.ActionList);

    FMenu := TMenuItem.Create(nil);
    FMenu.Caption := RsSvnMenuCaption;
    FMenu.Name := RsSvnMenuName;
    FMenu.OnClick := MenuClick;
    IDEMainMenu.Items.Insert(I + 1, FMenu);
    if not Assigned(FMenu.Parent) then
      raise EJclExpertException.CreateTrace(Format(RsSvnMenuItemNotInserted, [RsSvnMenuCaption]));

    // Diff File
    CreateMenuItem(FactDiffFile, FmnuDiffFile, RsSvnActDiffFileCaption,
      RsSvnActDiffFileName, RsSvnMnuDiffFileName, actDiffFileExecute);
    LoadImageFor(FactDiffFile, 'SVNDIFF');
    // Blame File
    CreateMenuItem(FactBlameFile, FmnuBlameFile, RsSvnActBlameFileCaption,
      RsSvnActBlameFileName, RsSvnMnuBlameFileName, actBlameFileExecute);
    LoadImageFor(FactBlameFile, 'SVNBLAME');
    // Revision Graph File
    CreateMenuItem(FactGraphFile, FmnuGraphFile, RsSvnActGraphFileCaption,
      RsSvnActGraphFileName, RsSvnMnuGraphFileName, actGraphFileExecute);
    LoadImageFor(FactGraphFile, 'SVNREVISIONGRAPH');
    // Properties
    CreateMenuItem(FactProperties, FmnuProperties, RsSvnActPropertiesCaption,
      RsSvnActPropertiesName, RsSvnMnuPropertiesName, actPropertiesExecute);
    // Explore Folder
    CreateMenuItem(FactExplore, FmnuExplore, RsSvnActExploreCaption,
      RsSvnActExploreName, RsSvnMnuExploreName, actExploreExecute);
    // Context Menu (right-click)
    CreateMenuItem(FactContextMenu, FmnuContextMenu, RsSvnActContextMenuCaption,
      RsSvnActContextMenuName, RsSvnMnuContextMenuName, actContextMenuExecute);
    // Seperator 1
    AddMenuSeparator(FmnuSeperator1);
    // Status File
    CreateMenuItem(FactStatusFile, FmnuStatusFile, RsSvnActStatusFileCaption,
      RsSvnActStatusFileName, RsSvnMnuStatusFileName, actStatusFileExecute);
    LoadImageFor(FactStatusFile, 'SVNSTATUS');
    // Status Sandbox
    CreateMenuItem(FactStatusSandbox, FmnuStatusSandbox, RsSvnActStatusSandboxCaption,
      RsSvnActStatusSandboxName, RsSvnMnuStatusSandboxName, actStatusSandboxExecute);
    FactStatusSandbox.ImageIndex := FactStatusFile.ImageIndex;
    // Seperator 2
    AddMenuSeparator(FmnuSeperator2);
    // Log File
    CreateMenuItem(FactLogFile, FmnuLogFile, RsSvnActLogFileCaption,
      RsSvnActLogFileName, RsSvnMnuLogFileName, actLogFileExecute);
    LoadImageFor(FactLogFile, 'SVNLOG');
    // Log Sandbox
    CreateMenuItem(FactLogSandbox, FmnuLogSandbox, RsSvnActLogSandboxCaption,
      RsSvnActLogSandboxName, RsSvnMnuLogSandboxName, actLogSandboxExecute);
    FactLogSandbox.ImageIndex := FactLogFile.ImageIndex;
    // Seperator 3
    AddMenuSeparator(FmnuSeperator3);
    // Lock File
    CreateMenuItem(FactLockFile, FmnuLockFile, RsSvnActLockFileCaption,
      RsSvnActLockFileName, RsSvnMnuLockFileName, actLockFileExecute);
    LoadImageFor(FactLockFile, 'SVNLOCK');
    // Unlock File
    CreateMenuItem(FactUnlockFile, FmnuUnlockFile, RsSvnActUnlockFileCaption,
      RsSvnActUnlockFileName, RsSvnMnuUnlockFileName, actUnlockFileExecute);
    LoadImageFor(FactUnlockFile, 'SVNUNLOCK');
    // Seperator 4
    AddMenuSeparator(FmnuSeperator4);
    // Revert File
    CreateMenuItem(FactRevertFile, FmnuRevertFile, RsSvnActRevertFileCaption,
      RsSvnActRevertFileName, RsSvnMnuRevertFileName, actRevertFileExecute);
    LoadImageFor(FactRevertFile, 'SVNREVERT');
    // Revert Sandbox
    CreateMenuItem(FactRevertSandbox, FmnuRevertSandbox, RsSvnActRevertSandboxCaption,
      RsSvnActRevertSandboxName, RsSvnMnuRevertSandboxName, actRevertSandboxExecute);
    FactRevertSandbox.ImageIndex := FactRevertFile.ImageIndex;
    // Seperator 5
    AddMenuSeparator(FmnuSeperator5);
    // Update File
    CreateMenuItem(FactUpdateFile, FmnuUpdateFile, RsSvnActUpdateFileCaption,
      RsSvnActUpdateFileName, RsSvnMnuUpdateFileName, actUpdateFileExecute);
    LoadImageFor(FactUpdateFile, 'SVNUPDATE');
    // Update Sandbox
    CreateMenuItem(FactUpdateSandbox, FmnuUpdateSandbox, RsSvnActUpdateSandboxCaption,
      RsSvnActUpdateSandboxName, RsSvnMnuUpdateSandboxName, actUpdateSandboxExecute);
    FactUpdateSandbox.ImageIndex := FactUpdateFile.ImageIndex;
    // Seperator 6
    AddMenuSeparator(FmnuSeperator6);
    // Commit File
    CreateMenuItem(FactCommitFile, FmnuCommitFile, RsSvnActCommitFileCaption,
      RsSvnActCommitFileName, RsSvnMnuCommitFileName, actCommitFileExecute);
    LoadImageFor(FactCommitFile, 'SVNCOMMIT');
    // Commit Sandbox
    CreateMenuItem(FactCommitSandbox, FmnuCommitSandbox, RsSvnActCommitSandboxCaption,
      RsSvnActCommitSandboxName, RsSvnMnuCommitSandboxName, actCommitSandboxExecute);
    FactCommitSandbox.ImageIndex := FactCommitFile.ImageIndex;
    // Seperator 7
    AddMenuSeparator(FmnuSeperator7);
    // Repository Browser
    CreateMenuItem(FactRepoBrowser, FmnuRepoBrowser, RsSvnActRepoBrowserCaption,
      RsSvnActRepoBrowserName, RsSvnMnuRepoBrowserName, actRepoBrowserExecute);
    LoadImageFor(FactRepoBrowser, 'SVNREPOBROWSER');
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

function TJclVersionControlExpert.SandboxPath: string;
var
  i: Integer;
  lPath, lParent, lTemp: string;
begin
  try
    lPath := ExtractFilePath(ActiveModuleFileName(False, False));
    if (lPath = '') or not DirectoryExists(PathAddSeparator(lPath) + '.svn') then
    begin
      Result := '';
      ShowMessage(RsNotUnderVersionControl);
      Exit;
    end;
    i := PathGetDepth(lPath);
    Dec(i);
    lParent := '';
    while i >= 0 do
    begin
      lTemp := PathExtractPathDepth(lPath, i);
      if DirectoryExists(lTemp + '.svn') then
        lParent := lTemp
      else
        Break;
      Dec(i);
    end;
    if lParent <> '' then
      Result := lParent
    else
      Result := lPath;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TJclVersionControlExpert.UnregisterCommands;
begin
  inherited UnregisterCommands;
  try
    UnregisterAction(FactRepoBrowser);
    FreeAndNil(FmnuRepoBrowser);
    FreeAndNil(FactRepoBrowser);
    UnregisterAction(FactCommitSandbox);
    FreeAndNil(FmnuCommitSandbox);
    FreeAndNil(FactCommitSandbox);
    UnregisterAction(FactCommitFile);
    FreeAndNil(FmnuCommitFile);
    FreeAndNil(FactCommitFile);
    FreeAndNil(FmnuSeperator5);
    UnregisterAction(FactUpdateSandbox);
    FreeAndNil(FmnuUpdateSandbox);
    FreeAndNil(FactUpdateSandbox);
    UnregisterAction(FactUpdateFile);
    FreeAndNil(FmnuUpdateFile);
    FreeAndNil(FactUpdateFile);
    FreeAndNil(FmnuSeperator4);
    UnregisterAction(FactRevertSandbox);
    FreeAndNil(FmnuRevertSandbox);
    FreeAndNil(FactRevertSandbox);
    UnregisterAction(FactRevertFile);
    FreeAndNil(FmnuRevertFile);
    FreeAndNil(FactRevertFile);
    FreeAndNil(FmnuSeperator3);
    UnregisterAction(FactUnlockFile);
    FreeAndNil(FmnuUnlockFile);
    FreeAndNil(FactUnlockFile);
    UnregisterAction(FactLockFile);
    FreeAndNil(FmnuLockFile);
    FreeAndNil(FactLockFile);
    UnregisterAction(FactLogSandbox);
    FreeAndNil(FmnuLogSandbox);
    FreeAndNil(FactLogSandbox);
    UnregisterAction(FactLogFile);
    FreeAndNil(FmnuLogFile);
    FreeAndNil(FactLogFile);
    FreeAndNil(FmnuSeperator2);
    UnregisterAction(FactStatusSandbox);
    FreeAndNil(FmnuStatusSandbox);
    FreeAndNil(FactStatusSandbox);
    UnregisterAction(FactStatusFile);
    FreeAndNil(FmnuStatusFile);
    FreeAndNil(FactStatusFile);
    FreeAndNil(FmnuSeperator1);
    UnregisterAction(FactContextMenu);
    FreeAndNil(FmnuContextMenu);
    FreeAndNil(FactContextMenu);
    UnregisterAction(FactExplore);
    FreeAndNil(FmnuExplore);
    FreeAndNil(FactExplore);
    UnregisterAction(FactProperties);
    FreeAndNil(FmnuProperties);
    FreeAndNil(FactProperties);
    UnregisterAction(FactGraphFile);
    FreeAndNil(FmnuGraphFile);
    FreeAndNil(FactGraphFile);
    UnregisterAction(FactBlameFile);
    FreeAndNil(FmnuBlameFile);
    FreeAndNil(FactBlameFile);
    UnregisterAction(FactDiffFile);
    FreeAndNil(FmnuDiffFile);
    FreeAndNil(FactDiffFile);
    FreeAndNil(FMenu);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

end.
