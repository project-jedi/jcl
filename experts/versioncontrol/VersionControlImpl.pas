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
{ The Original Code is VersionControlImpl.pas                                                      }
{                                                                                                  }
{ The Initial Developer of the Original Code is Elahn Ientile.                                     }
{ Portions created by Elahn Ientile are Copyright (C) of Elahn Ientile.                            }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Florent Ouchet (outchy)                                                                        }
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
  SysUtils, Classes, Graphics, Controls, Menus, ActnList, Dialogs,
  ToolsAPI,
{$IFNDEF COMPILER8_UP}
  Idemenuaction, // dependency walker reports a class TPopupAction in
  // unit Idemenuaction in designide.bpl used by the IDE to display tool buttons
  // with a drop down menu, this class seems to have the same interface
  // as TControlAction defined in Controls.pas for newer versions of Delphi
{$ENDIF COMPILER8_UP}
  JclOtaUtils, JclVersionCtrlCommonOptions;

type
  TJclVersionControlAction = (
    vcaAdd,               // add current file
    vcaAddSandbox,        // add file in the sandbox
    vcaBlame,             // detailed authors of the current file
    vcaBranch,            // branch current file
    vcaBranchSandbox,     // branch files of the sandbox
    vcaCheckOutSandbox,   // checkout a new sandbox
    vcaCommit,            // commit the current file
    vcaCommitSandbox,     // commit files of the sandbox
    vcaContextMenu,       // explorer context menu of the file
    vcaDiff,              // diff current file
    vcaExploreSandbox,    // explore sandbox
    vcaGraph,             // modification graph of the current file
    vcaLog,               // log of the current file
    vcaLogSandbox,        // log of files in the sandbox
    vcaLock,              // lock current file
    vcaLockSandbox,       // lock files of the sandbox
    vcaMerge,             // merge current file
    vcaMergeSandbox,      // merge files of the sandbox
    vcaProperties,        // properties of the file
    vcaPropertiesSandbox, // properties of the sandbox
    vcaRename,            // rename current file
    //vcaRenameSandbox (renaming current sandbox) will not work because the IDE
    // owns handles to project directories
    vcaRepoBrowser,       // repository browser
    vcaRevert,            // revert changes in the current file
    vcaRevertSandbox,     // revert changes in all files of the sandbox
    vcaStatus,            // status of current file
    vcaStatusSandbox,     // status of the sandbox
    vcaTag,               // tag the current file
    vcaTagSandBox,        // tag the current sandbox
    vcaUpdate,            // update current file
    vcaUpdateSandbox,     // update sandbox
    vcaUpdateTo,          // update current file to...
    vcaUpdateSandboxTo,   // update sandbox to...
    vcaUnlock,            // unlock current file
    vcaUnlockSandbox      // unlock sandbox
  );

  TJclVersionControlActions = set of TJclVersionControlAction;

  TJclVersionControlExpert = class;

  TJclVersionControlPlugin = class
  private
    FExpert: TJclVersionControlExpert;
  protected
    // get supported actions by the plugin
    function GetSupportedActions: TJclVersionControlActions; virtual;
    // get actions for the current file
    function GetFileActions(const FileName: string): TJclVersionControlActions; virtual;
    // get actions for the current sandbox (sandbox can be not yet initialized)
    function GetSandboxActions(const SdBxName: string): TJclVersionControlActions; virtual;
    // get icon for the action
    function GetIcon(const Action: TJclVersionControlAction): Integer; virtual;
    // true if the plugin is supported (third-party tools present)
    function GetEnabled: Boolean; virtual;
    // friendly name of the plugin
    function GetName: string; virtual;
  public
    constructor Create(const AExpert: TJclVersionControlExpert); reintroduce; virtual;
    // returns sandbox names
    // returns true and initialized sandbox names if presents
    // returns false and all parent directories names if no sandbox is present
    function GetSandboxNames(const FileName: string; SdBxNames: TStrings): Boolean; virtual;
    // execute the action of a file or on a sandbox
    function ExecuteAction(const FileName: string;
      const Action: TJclVersionControlAction): Boolean; virtual;
    property SupportActions: TJclVersionControlActions read GetSupportedActions;
    property FileActions[const FileName: string]: TJclVersionControlActions read GetFileActions;
    property SandboxActions[const SdBxName: string]: TJclVersionControlActions read GetSandboxActions;
    property Icons[const Action: TJclVersionControlAction]: Integer read GetIcon;
    property Enabled: Boolean read GetEnabled;
    property Expert: TJclVersionControlExpert read FExpert;
    property Name: string read GetName;
  end;

  TJclVersionControlPluginClass = class of TJclVersionControlPlugin;

  TJclVersionControlCache = class (TObject)
  private
    FSandboxList: TList;
    FFileName: string;
    FPlugin: TJclVersionControlPlugin;
    FActions: TJclVersionControlActions;
    FValidityTime: TDateTime;
    function GetSandBox(Index: Integer): string;
    function GetSandboxAction(Index: Integer): TJclVersionControlActions;
    function GetSandboxCount: Integer;
  public
    constructor Create(APlugin: TJclVersionControlPlugin; AFileName: string);
    destructor Destroy; override;
    function GetValid(const ATime: TDateTime): Boolean;
    property Plugin: TJclVersionControlPlugin read FPlugin;
    property FileName: string read FFileName;
    property Actions: TJclVersionControlActions read FActions;
    property SandBoxes[Index: Integer]: string read GetSandBox;
    property SandBoxActions[Index: Integer]: TJclVersionControlActions read GetSandboxAction;
    property SandBoxCount: Integer read GetSandboxCount;
  end;

  TJclVersionControlExpert = class (TJclOTAExpert)
  private
    FVersionCtrlMenu: TMenuItem;
    FActions: array [TJclVersionControlAction] of TCustomAction;
    FPluginList: TList;
    FIconCache: TList;
    FFileCache: TList;
    FLastPlugin: TJclVersionControlPlugin;
    FModuleServices: IOTAModuleServices;
    FHideActions: Boolean;
    FIconType: Integer;
    FActOnTopSandbox: Boolean;
    FSaveConfirmation: Boolean;
    FDisableActions: Boolean;
    FOptionsFrame: TJclVersionCtrlOptionsFrame;
    FMenuOrganization: TStringList;
    procedure RefreshIcon(const AAction: TCustomAction);
    function GetPlugin(Index: Integer): TJclVersionControlPlugin;
    function GetPluginCount: Integer;
    procedure SetIconType(const Value: Integer);

    procedure ActionUpdate(Sender: TObject);
    procedure ActionExecute(Sender: TObject);
    procedure IDEActionMenuClick(Sender: TObject);
    procedure SubItemClick(Sender: TObject);
    procedure DropDownMenuPopup(Sender: TObject);
    procedure IDEVersionCtrlMenuClick(Sender: TObject);
    procedure RefreshIcons;
    procedure RefreshMenu;
    procedure CleanSubMenus(const AMenuItem: TMenuItem);
    function GetCurrentCache: TJclVersionControlCache;
    function GetCurrentPlugin: TJclVersionControlPlugin;
    function GetCurrentFileName: string;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure RegisterCommands; override;
    procedure UnregisterCommands; override;
    procedure AddConfigurationPages(AddPageFunc: TJclOTAAddPageFunc); override;
    procedure ConfigurationClosed(AControl: TControl; SaveChanges: Boolean); override;
    function CacheResourceIcon(const ResourceFile: string; const ResourceID: Integer): Integer; overload;
    function CacheResourceIcon(const ResourceFile: string; const ResourceName: string): Integer; overload;
    function SaveModules(const FileName: string;
      const IncludeSubDirectories: Boolean): Boolean;
    function GetFileCache(const FileName: string; const Plugin: TJclVersionControlPlugin): TJclVersionControlCache;

    property ModuleServices: IOTAModuleServices read FModuleServices;
    property ActOnTopSandbox: Boolean read FActOnTopSandbox write FActOnTopSandbox;
    property DisableActions: Boolean read FDisableActions write FDisableActions;
    property HideActions: Boolean read FHideActions write FHideActions;
    property SaveConfirmation: Boolean read FSaveConfirmation write FSaveConfirmation;
    property IconType: Integer read FIconType write SetIconType;
    property CurrentCache: TJclVersionControlCache read GetCurrentCache;
    property CurrentPlugin: TJclVersionControlPlugin read GetCurrentPlugin;
    property CurrentFileName: string read GetCurrentFileName;
    property PluginCount: Integer read GetPluginCount;
    property Plugins[Index: Integer]: TJclVersionControlPlugin read GetPlugin;
  // plugin functions
  private
    procedure ClassRegistered(const APluginClass: TJclVersionControlPluginClass);
    procedure ClassUnregistered(const APluginClass: TJclVersionControlPluginClass);
  public
    class procedure RegisterPluginClass(const APluginClass: TJclVersionControlPluginClass);
    class procedure UnregisterPluginClass(const APluginClass: TJclVersionControlPluginClass);
  end;

{$IFDEF COMPILER8_UP}
  TDropDownAction = TControlAction;
{$ELSE COMPILER8_UP}
  TDropDownAction = TPopupAction;
{$ENDIF COMPILER8_UP}

// design package entry point
procedure Register;

// expert DLL entry point
function JCLWizardInit(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var TerminateProc: TWizardTerminateProc): Boolean; stdcall;

function GetItemIndexA(const Item: string): Integer;
function GetItemIndexB(const Item: string): Integer;
function GetItemName(const Item: string): string;

implementation

uses
  Windows, Forms, TypInfo,
  JclDebug, JclFileUtils, JclRegistry, JclShell, JclStrings,
  JclOtaConsts, JclOtaResources;

//=== VersionControlImpl.pas ===================================================
const
  JclVersionCtrlMenuName = 'JclVersionCtrlMenu';
  // vcaAdd
  JclVersionCtrlAddActionName = 'JclVersionCtrlAddCommand';
  // vcaAddSandbox
  JclVersionCtrlAddSandboxActionName = 'JclVersionCtrlAddSandboxCommand';
  // vcaBlame
  JclVersionCtrlBlameActionName = 'JclVersionCtrlBlameCommand';
  // vcaBranch
  JclVersionCtrlBranchActionName = 'JclVersionCtrlBranchCommand';
  // vcaBranchSandbox
  JclVersionCtrlBranchSandboxActionName = 'JclVersionCtrlBranchSandboxCommand';
  // vcaCheckoutSandbox
  JclVersionCtrlCheckoutSandboxActionName = 'JclVersionCtrlCheckOutSandboxCommand';
  // vcaCommit
  JclVersionCtrlCommitActionName = 'JclVersionCtrlCommitCommand';
  // vcaCommitSandbox
  JclVersionCtrlCommitSandboxActionName = 'JclVersionCtrlCommitSandboxCommand';
  // vcaContextMenu
  JclVersionCtrlContextMenuActionName = 'JclVersionCtrlContextMenuCommand';
  // vcaDiff
  JclVersionCtrlDiffActionName = 'JclVersionCtrlDiffCommand';
  // vcaExploreSandbox
  JclVersionCtrlExploreSandboxActionName = 'JclVersionCtrlExploreSandboxCommand';
  // vcaGraph
  JclVersionCtrlGraphActionName = 'JclVersionCtrlGraphCommand';
  // vcaLog
  JclVersionCtrlLogActionName = 'JclVersionCtrlLogCommand';
  // vcaLogSandbox
  JclVersionCtrlLogSandboxActionName = 'JclVersionCtrlLogSandboxCommand';
  // vcaLock
  JclVersionCtrlLockActionName = 'JclVersionCtrlLockCommand';
  // vcaLockSandbox
  JclVersionCtrlLockSandboxActionName = 'JclVersionCtrlLockSandboxCommand';
  // vcaMerge
  JclVersionCtrlMergeActionName = 'JclVersionCtrlMergeCommand';
  // vcaMergeSandbox
  JclVersionCtrlMergeSandboxActionName = 'JclVersionCtrlMergeSandboxCommand';
  // vcaProperties
  JclVersionCtrlPropertiesActionName = 'JclVersionCtrlPropertiesCommand';
  // vcaPropertiesSandbox
  JclVersionCtrlPropertiesSandboxActionName = 'JclVersionCtrlPropertiesSandboxCommand';
  // vcaRename
  JclVersionCtrlRenameActionName = 'JclVersionCtrlRenameCommand';
  // vcaRepoBrowser
  JclVersionCtrlRepoBrowserActionName = 'JclVersionCtrlRepoBrowserCommand';
  // vcaRevert
  JclVersionCtrlRevertActionName = 'JclVersionCtrlRevertCommand';
  // vcaRevertSandbox
  JclVersionCtrlRevertSandboxActionName = 'JclVersionCtrlRevertSandboxCommand';
  // vcaStatus
  JclVersionCtrlStatusActionName = 'JclVersionCtrlStatusCommand';
  // vcaStatusSandbox
  JclVersionCtrlStatusSandboxActionName = 'JclVersionCtrlStatusSandboxCommand';
  // vcaTag
  JclVersionCtrlTagActionName = 'JclVersionCtrlTagCommand';
  // vcaTagSandBox
  JclVersionCtrlTagSandboxActionName = 'JclVersionCtrlTagSandboxCommand';
  // vcaUpdate
  JclVersionCtrlUpdateActionName = 'JclVersionCtrlUpdateCommand';
  // vcaUpdateSandbox
  JclVersionCtrlUpdateSandboxActionName = 'JclVersionCtrlUpdateSandboxCommand';
  // vcaUpdateTo
  JclVersionCtrlUpdateToActionName = 'JclVersionCtrlUpdateToCommand';
  // vcaUpdateSandboxTo
  JclVersionCtrlUpdateSandboxToActionName = 'JclVersionCtrlUpdateSandboxToCommand';
  // vcaUnlock
  JclVersionCtrlUnlockActionName = 'JclVersionCtrlUnlockCommand';
  // vcaUnlockSandbox
  JclVersionCtrlUnlockSandboxActionName = 'JclVersionCtrlUnlockSandboxCommand';

  JclVersionCtrlActOnTopSandboxName = 'ActOnTopSandbox';
  JclVersionCtrlMenuOrganizationName = 'MenuOrganization';
  JclVersionCtrlSaveConfirmationName = 'SaveConfirmation';
  JclVersionCtrlDisableActionsName = 'DisableActions';
  JclVersionCtrlHideActionsName = 'HideActions';
  JclVersionCtrlIconTypeName = 'IconType';
  JclVersionCtrlIconTypeAutoValue = 'auto';
  JclVersionCtrlIconTypeNoIconValue = 'noicon';
  JclVersionCtrlIconTypeJclIconValue = 'jclicons';

resourcestring
  RsVersionCtrlMenuCaption = 'Jcl &Version';
  RsVersionCtrlAddCaption = '&Add';                         // vcaAdd
  RsVersionCtrlAddSandboxCaption = 'Add ...';               // vcaAddSandbox
  RsVersionCtrlBlameCaption = '&Blame';                     // vcaBlame
  RsVersionCtrlBranchCaption = 'Branc&h';                   // vcaBranch
  RsVersionCtrlBranchSandboxCaption = 'Branch ...';         // vcaBranchSandbox
  RsVersionCtrlCheckOutSandboxCaption = 'C&heck out ...';   // vcaCreateSandbox
  RsVersionCtrlCommitCaption = 'Co&mmit';                   // vcaCommit
  RsVersionCtrlCommitSandboxCaption = 'Commit ...';         // vcaCommitSandbox
  RsVersionCtrlContextMenuCaption = 'Ex&plorer Menu';       // vcaContextMenu
  RsVersionCtrlDiffCaption = '&Diff';                       // vcaDiff
  RsVersionCtrlExploreSandboxCaption = 'E&xplore ...';      // vcaExploreSandbox
  RsVersionCtrlGraphCaption = 'Revision Gr&aph';            // vcaGraph
  RsVersionCtrlLogCaption = '&Log';                         // vcaLog
  RsVersionCtrlLogSandboxCaption = 'Log ...';               // vcaLogSandbox
  RsVersionCtrlLockCaption = 'Loc&k';                       // vcaLock
  RsVersionCtrlLockSandboxCaption = 'Lock ...';             // vcaLockSandbox
  RsVersionCtrlMergeCaption = '&Merge';                     // vcaMerge
  RsVersionCtrlMergeSandboxCaption = 'Merge ...';           // vcaMergeSandbox
  RsVersionCtrlPropertiesCaption = 'Pr&operties';           // vcaProperties
  RsVersionCtrlPropertiesSandboxCaption = 'Properties ...'; // vcaPropertiesSandbox
  RsVersionCtrlRenameCaption = '&Rename';                   // vcaRename
  RsVersionCtrlRepoBrowserCaption = 'Repositor&y Browser';  // vcaRepoBrowser
  RsVersionCtrlRevertCaption = '&Revert';                   // vcaRevert
  RsVersionCtrlRevertSandboxCaption = 'Revert ...';         // vcaRevertSandbox
  RsVersionCtrlStatusCaption = 'S&tatus';                   // vcaStatus
  RsVersionCtrlStatusSandboxCaption = 'Status ...';         // vcaStatusSandbox
  RsVersionCtrlTagCaption = 'Ta&g';                         // vcaTag
  RsVersionCtrlTagSandboxCaption = 'Tag ...';               // vcaTagSandBox
  RsVersionCtrlUpdateCaption = 'U&pdate';                   // vcaUpdate
  RsVersionCtrlUpdateSandboxCaption = 'Update ...';         // vcaUpdateSandbox
  RsVersionCtrlUpdateToCaption = 'Update &to ';             // vcaUpdateTo
  RsVersionCtrlUpdateSandboxToCaption = 'Update to ...';    // vcaUpdateSandboxTo
  RsVersionCtrlUnlockCaption = '&Unlock';                   // vcaUnlock
  RsVersionCtrlUnlockSandboxCaption = 'Unlock ...';         // vcaUnlockSandbox

  RsSvnMenuItemNotInserted = 'Can''t insert the ''%s'' menu item';
  RsENoToolsMenuItem = 'Tools menu item not found';
  RsVersionControlSheet = 'Version control';
  RsActionCategory = 'Jedi Code Library';

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

    JCLWizardIndex := OTAWizardServices.AddWizard(TJclVersionControlExpert.Create);

    Result := True;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      Result := False;
    end;
  end;
end;

function GetItemIndexA(const Item: string): Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 1 to Length(Item) do
    if not (Item[Index] in AnsiDecDigits) then
  begin
    Result := StrToInt(Copy(Item, 1, Index - 1));
    Exit;
  end;
  Abort;
end;

function GetItemIndexB(const Item: string): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := Length(Item) downto 1 do
    if not (Item[Index] in AnsiDecDigits) then
  begin
    if Index < Length(Item) then
      Result := StrToInt(Copy(Item, Index + 1, Length(Item) - Index));
    Exit;
  end;
end;

function GetItemName(const Item: string): string;
var
  Index1, Index2: Integer;
begin
  for Index1 := 1 to Length(Item) do
    if not (Item[Index1] in AnsiDecDigits) then
  begin
    if Index1 = 1 then
      Abort;
    Break;
  end;

  for Index2 := Length(Item) downto 1 do
    if not (Item[Index2] in AnsiDecDigits) then
      Break;

  Result := Copy(Item, Index1, Index2 - Index1 + 1);
end;

function MenuOrganizationSort(List: TStringList; Index1, Index2: Integer): Integer;
var
  Item1, Item2: string;
  Index1A, Index1B, Index2A, Index2B: Integer;
begin
  Item1 := List.Strings[Index1];
  Item2 := List.Strings[Index2];
  Index1A := GetItemIndexA(Item1);
  Index1B := GetItemIndexB(Item1);
  Index2A := GetItemIndexA(Item2);
  Index2B := GetItemIndexB(Item2);

  if Index1A < Index2A then
    Result := -1
  else if Index1A > Index2A then
    Result := 1
  else if Index1B < Index2B then
    Result := -1
  else if Index1B > Index2B then
    Result := 1
  else
    Result := 0;
end;

type
  TJclVersionControlActionRec = record
    Sandbox: Boolean;
    SaveFile: Boolean;
    Caption: string;
    ActionName: string;
  end;

const
  VersionControlActionInfos: array [TJclVersionControlAction] of TJclVersionControlActionRec =
   ( (SandBox: False;
      SaveFile: True;
      Caption: RsVersionCtrlAddCaption;               // vcaAdd
      ActionName: JclVersionCtrlAddActionName),
     (SandBox: True;
      SaveFile: True;
      Caption: RsVersionCtrlAddSandboxCaption;         // vcaAddSandbox
      ActionName: JclVersionCtrlAddSandboxActionName),
     (SandBox: False;
      SaveFile: True;
      Caption: RsVersionCtrlBlameCaption;             // vcaBlame
      ActionName: JclVersionCtrlBlameActionName),
     (SandBox: False;
      SaveFile: True;
      Caption: RsVersionCtrlBranchCaption;            // vcaBranch
      ActionName: JclVersionCtrlBranchActionName),
     (SandBox: True;
      SaveFile: True;
      Caption: RsVersionCtrlBranchSandboxCaption;      // vcaBranchSandbox
      ActionName: JclVersionCtrlBranchSandboxActionName),
     (SandBox: True;
      SaveFile: True;
      Caption: RsVersionCtrlCheckOutSandboxCaption;    // vcaCheckOutSandbox
      ActionName: JclVersionCtrlCheckOutSandboxActionName),
     (SandBox: False;
      SaveFile: True;
      Caption: RsVersionCtrlCommitCaption;            // vcaCommit
      ActionName: JclVersionCtrlCommitActionName),
     (SandBox: True;
      SaveFile: True;
      Caption: RsVersionCtrlCommitSandboxCaption;      // vcaCommitSandbox
      ActionName: JclVersionCtrlCommitSandboxActionName),
     (SandBox: False;
      SaveFile: False;
      Caption: RsVersionCtrlContextMenuCaption;       // vcaContextMenu
      ActionName: JclVersionCtrlContextMenuActionName),
     (SandBox: False;
      SaveFile: True;
      Caption: RsVersionCtrlDiffCaption;              // vcaDiff
      ActionName: JclVersionCtrlDiffActionName),
     (SandBox: True;
      SaveFile: False;
      Caption: RsVersionCtrlExploreSandboxCaption;     // vcaExploreSandbox
      ActionName: JclVersionCtrlExploreSandboxActionName),
     (SandBox: False;
      SaveFile: False;
      Caption: RsVersionCtrlGraphCaption;             // vcaGraph
      ActionName: JclVersionCtrlGraphActionName),
     (SandBox: False;
      SaveFile: False;
      Caption: RsVersionCtrlLogCaption;               // vcaLog
      ActionName: JclVersionCtrlLogActionName),
     (SandBox: True;
      SaveFile: False;
      Caption: RsVersionCtrlLogSandboxCaption;         // vcaLogSandbox
      ActionName: JclVersionCtrlLogSandboxActionName),
     (SandBox: False;
      SaveFile: True;
      Caption: RsVersionCtrlLockCaption;              // vcaLock
      ActionName: JclVersionCtrlLockActionName),
     (SandBox: True;
      SaveFile: True;
      Caption: RsVersionCtrlLockSandboxCaption;        // vcaLockSandbox
      ActionName: JclVersionCtrlLockSandboxActionName),
     (SandBox: False;
      SaveFile: True;
      Caption: RsVersionCtrlMergeCaption;             // vcaMerge
      ActionName: JclVersionCtrlMergeActionName),
     (SandBox: True;
      SaveFile: True;
      Caption: RsVersionCtrlMergeSandboxCaption;       // vcaMergeSandbox
      ActionName: JclVersionCtrlMergeSandboxActionName),
     (SandBox: False;
      SaveFile: True;
      Caption: RsVersionCtrlPropertiesCaption;        // vcaProperties
      ActionName: JclVersionCtrlPropertiesActionName),
     (SandBox: True;
      SaveFile: True;
      Caption: RsVersionCtrlPropertiesSandboxCaption;  // vcaPropertiesSandbox
      ActionName: JclVersionCtrlPropertiesSandboxActionName),
     (SandBox: False;
      SaveFile: True;
      Caption: RsVersionCtrlRenameCaption;            // vcaRename
      ActionName: JclVersionCtrlRenameActionName),
     (SandBox: False;
      SaveFile: False;
      Caption: RsVersionCtrlRepoBrowserCaption;       // vcaRepoBrowser
      ActionName: JclVersionCtrlRepoBrowserActionName),
     (SandBox: False;
      SaveFile: False;
      Caption: RsVersionCtrlRevertCaption;            // vcaRevert
      ActionName: JclVersionCtrlRevertActionName),
     (SandBox: True;
      SaveFile: False;
      Caption: RsVersionCtrlRevertSandboxCaption;      // vcaRevertSandbox
      ActionName: JclVersionCtrlRevertSandboxActionName),
     (SandBox: False;
      SaveFile: True;
      Caption: RsVersionCtrlStatusCaption;            // vcaStatus
      ActionName: JclVersionCtrlStatusActionName),
     (SandBox: True;
      SaveFile: True;
      Caption: RsVersionCtrlStatusSandboxCaption;      // vcaStatusSandbox
      ActionName: JclVersionCtrlStatusSandboxActionName),
     (SandBox: False;
      SaveFile: True;
      Caption: RsVersionCtrlTagCaption;               // vcaTag
      ActionName: JclVersionCtrlTagActionName),
     (SandBox: True;
      SaveFile: True;
      Caption: RsVersionCtrlTagSandboxCaption;         // vcaTagSandBox
      ActionName: JclVersionCtrlTagSandboxActionName),
     (SandBox: False;
      SaveFile: True;
      Caption: RsVersionCtrlUpdateCaption;            // vcaUpdate
      ActionName: JclVersionCtrlUpdateActionName),
     (SandBox: True;
      SaveFile: True;
      Caption: RsVersionCtrlUpdateSandboxCaption;      // vcaUpdateSandbox
      ActionName: JclVersionCtrlUpdateSandboxActionName),
     (SandBox: False;
      SaveFile: True;
      Caption: RsVersionCtrlUpdateToCaption;          // vcaUpdateTo
      ActionName: JclVersionCtrlUpdateToActionName),
     (SandBox: True;
      SaveFile: True;
      Caption: RsVersionCtrlUpdateSandboxToCaption;    // vcaUpdateSandboxTo
      ActionName: JclVersionCtrlUpdateSandboxToActionName),
     (SandBox: False;
      SaveFile: True;
      Caption: RsVersionCtrlUnlockCaption;            // vcaUnlock
      ActionName: JclVersionCtrlUnlockActionName),
     (SandBox: True;
      SaveFile: True;
      Caption: RsVersionCtrlUnlockSandboxCaption;      // vcaUnlockSandbox
      ActionName: JclVersionCtrlUnlockSandboxActionName)
   );

//=== { TJclIconCacheInfo } ==========================================================

type
  TJclIconCacheInfo = class (TObject)
  private
    FFileName: string;
    FIconIndex: Integer;
    FResourceName: string;
  public
    property FileName: string read FFileName write FFileName;
    property ResourceName: string read FResourceName write FResourceName;
    property IconIndex: Integer read FIconIndex write FIconIndex;
  end;

//=== { TJclVersionControlExpert } ===================================================

var
  PluginClassList: TList = nil;
  ExpertInstanceList: TList = nil;

class procedure TJclVersionControlExpert.RegisterPluginClass(
  const APluginClass: TJclVersionControlPluginClass);
var
  Index: Integer;
begin
  if not Assigned(PluginClassList) then
    PluginClassList := TList.Create;
  PluginClassList.Add(APluginClass);
  if Assigned(ExpertInstanceList) then
    for Index := 0 to ExpertInstanceList.Count - 1 do
      TJclVersionControlExpert(ExpertInstanceList.Items[Index]).ClassRegistered(APluginClass);
end;

class procedure TJclVersionControlExpert.UnregisterPluginClass(
  const APluginClass: TJclVersionControlPluginClass);
var
  Index: Integer;
begin
  if Assigned(PluginClassList) then
    PluginClassList.Remove(APluginClass);
  if Assigned(ExpertInstanceList) then
    for Index := 0 to ExpertInstanceList.Count - 1 do
      TJclVersionControlExpert(ExpertInstanceList.Items[Index]).ClassUnregistered(APluginClass);
end;

{function TJclVersionControlExpert.ActiveModuleFileName(ASave, IncludeFamily: Boolean): string;
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
end;       }

procedure TJclVersionControlExpert.ActionExecute(Sender: TObject);
var
  Index: Integer;
  AAction: TCustomAction;
  AControlAction: TJclVersionControlAction;
  APlugin: TJclVersionControlPlugin;
  AFileName: string;
  AFileCache: TJclVersionControlCache;
begin
  try
    AAction := Sender as TCustomAction;
    for AControlAction := Low(TJclVersionControlAction) to High(TJclVersionControlAction) do
      if FActions[AControlAction] = AAction then
    begin
      if VersionControlActionInfos[AControlAction].Sandbox then
      begin
        AFileCache := CurrentCache;
        if not Assigned(AFileCache) then
          Exit;
        if ActOnTopSandbox then
        begin
          for Index := AFileCache.SandboxCount - 1 downto 0 do
            if AControlAction in AFileCache.SandboxActions[Index] then
          begin
            if VersionControlActionInfos[AControlAction].SaveFile then
              SaveModules(AFileCache.SandBoxes[Index], True);
            AFileCache.Plugin.ExecuteAction(AFileCache.SandBoxes[Index], AControlAction);
            Exit;
          end;
        end
        else
        begin
          for Index := 0 to AFileCache.SandboxCount - 1 do
            if AControlAction in AFileCache.SandboxActions[Index] then
          begin
            if VersionControlActionInfos[AControlAction].SaveFile then
              SaveModules(AFileCache.SandBoxes[Index], True);
            AFileCache.Plugin.ExecuteAction(AFileCache.SandBoxes[Index], AControlAction);
            Exit;
          end;
        end;
      end
      else
      begin
        AFileName := CurrentFileName;
        APlugin := CurrentPlugin;
        if VersionControlActionInfos[AControlAction].SaveFile then
          SaveModules(AFileName, False);
        if Assigned(APlugin) then
          APlugin.ExecuteAction(AFileName, AControlAction);
        end;
      Exit;
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TJclVersionControlExpert.ActionUpdate(Sender: TObject);
var
  Index: Integer;
  AAction: TCustomAction;
  AControlAction: TJclVersionControlAction;
  AFileCache: TJclVersionControlCache;
begin
  try
    AAction := Sender as TCustomAction;
    AFileCache := CurrentCache;
    
    if IconType = -1 then
    begin
      if Assigned(AFileCache) then
        FLastPlugin := AFileCache.Plugin
      else
        FLastPlugin := nil;
      RefreshIcon(AAction);
    end;
    
    for AControlAction := Low(TJclVersionControlAction) to High(TJclVersionControlAction) do
      if FActions[AControlAction] = AAction then
    begin
      if HideActions then
        AAction.Visible := Assigned(AFileCache.Plugin)
          and (AControlAction in AFileCache.Plugin.SupportActions)
      else
        AAction.Visible := True;

      if DisableActions then
      begin
        if VersionControlActionInfos[AControlAction].Sandbox then
        begin
          for Index := 0 to AFileCache.SandBoxCount - 1 do
            if AControlAction in AFileCache.SandBoxActions[Index] then
          begin
            AAction.Enabled := True;
            Exit;
          end;
          AAction.Enabled := False;
          Exit;
        end;
        AFileCache := CurrentCache;
        AAction.Enabled := AControlAction in AFileCache.Actions;
        Exit;
      end
      else
        AAction.Enabled := True;
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TJclVersionControlExpert.AddConfigurationPages(
  AddPageFunc: TJclOTAAddPageFunc);
var
  Index: Integer;
  IconTypeNames: TStrings;
begin
  inherited AddConfigurationPages(AddPageFunc);
  FOptionsFrame := TJclVersionCtrlOptionsFrame.Create(nil);
  FOptionsFrame.DisableActions := DisableActions;
  FOptionsFrame.HideActions := HideActions;
  FOptionsFrame.SaveConfirmation := SaveConfirmation;
  FOptionsFrame.ActOnTopSandbox := ActOnTopSandbox;
  IconTypeNames := TStringList.Create;
  try
    for Index := 0 to FPluginList.Count - 1 do
      IconTypeNames.Add(TJclVersionControlPlugin(FPluginList.Items[Index]).Name);
    FOptionsFrame.SetIconTypeNames(IconTypeNames);
  finally
    IconTypeNames.Free;
  end;
  FOptionsFrame.SetActions(FActions);
  // after SetActions
  FOptionsFrame.MenuTree := FMenuOrganization;
  FOptionsFrame.IconType := IconType;
  AddPageFunc(FOptionsFrame, RsVersionControlSheet, Self);
end;

function TJclVersionControlExpert.CacheResourceIcon(const ResourceFile: string;
  const ResourceID: Integer): Integer;
var
  ResourceName: string;
begin
  SetLength(ResourceName, SizeOf(ResourceID));
  Move(ResourceID, ResourceName[1], SizeOf(ResourceID));
  Result := CacheResourceIcon(ResourceFile, ResourceName);
end;

function TJclVersionControlExpert.CacheResourceIcon(const ResourceFile: string;
  const ResourceName: string): Integer;
var
  Index: Integer;
  AIconCacheInfo: TJclIconCacheInfo;
  AIcon: TIcon;
  FileModule: HMODULE;
  ResourceID: Integer;
begin
  for Index := 0 to FIconCache.Count - 1 do
  begin
    AIconCacheInfo := TJclIconCacheInfo(FIconCache.Items[Index]);
    if (CompareText(AIconCacheInfo.ResourceName, ResourceName) = 0) and
       (CompareText(AIconCacheInfo.FileName, ResourceFile) = 0) then
    begin
      Result := AIconCacheInfo.IconIndex;
      Exit;
    end;
  end;
  Result := -1;
  AIconCacheInfo := TJclIconCacheInfo.Create;
  AIconCacheInfo.FileName := ResourceFile;
  AIconCacheInfo.ResourceName := ResourceName;
  FileModule := LoadLibraryEx(PChar(ResourceFile), 0, LOAD_LIBRARY_AS_DATAFILE);
  if FileModule <> 0 then
  try
    AIcon := TIcon.Create;
    try
      if (Length(ResourceName) = 4) and (ResourceName[3] = #0) and (ResourceName[4] = #0) then
      begin
        Move(ResourceName[1], ResourceID, SizeOf(ResourceID));
        AIcon.Handle := LoadIcon(FileModule, PChar(ResourceID));
      end
      else
        AIcon.Handle := LoadIcon(FileModule, PChar(ResourceName));
      Result := NTAServices.ImageList.AddIcon(AIcon);
    finally
      AIcon.Free;
    end;
  finally
    FreeLibrary(FileModule);
  end;

  AIconCacheInfo := TJclIconCacheInfo.Create;
  AIconCacheInfo.FileName := ResourceFile;
  AIconCacheInfo.ResourceName := ResourceName;
  AIconCacheInfo.IconIndex := Result;
  FIconCache.Add(AIconCacheInfo);
end;

procedure TJclVersionControlExpert.ClassRegistered(
  const APluginClass: TJclVersionControlPluginClass);
begin
  FPluginList.Add(APluginClass.Create(Self));
end;

procedure TJclVersionControlExpert.ClassUnregistered(
  const APluginClass: TJclVersionControlPluginClass);
var
  Index: Integer;
  APlugin: TJclVersionControlPlugin;
  AFileCache: TJclVersionControlCache;
begin
  for Index := FPluginList.Count - 1 downto 0 do
  begin
    APlugin := TJclVersionControlPlugin(FPluginList.Items[Index]);
    if APlugin = FLastPlugin then
      FLastPlugin := nil;
    if APlugin.ClassType = APluginClass then
    begin
      APlugin.Free;
      FPluginList.Delete(Index);
    end;
  end;
  for Index := FFileCache.Count downto 0 do
  begin
    AFileCache := TJclVersionControlCache(FFileCache.Items[Index]);
    if Assigned(AFileCache.Plugin) and (AFileCache.Plugin.ClassType = APluginClass) then
    begin
      AFileCache.Free;
      FFileCache.Delete(Index);
    end;
  end;
end;

procedure TJclVersionControlExpert.CleanSubMenus(const AMenuItem: TMenuItem);
var
  Index: Integer;
  BMenuItem: TMenuItem;
begin
  if Assigned(AMenuItem) then
    for Index := AMenuItem.Count - 1 downto 0 do
  begin
    BMenuItem := AMenuItem.Items[Index];
    CleanSubMenus(BMenuItem);
    BMenuItem.Free;
  end;
end;

procedure TJclVersionControlExpert.ConfigurationClosed(AControl: TControl;
  SaveChanges: Boolean);
begin
  if (AControl = FOptionsFrame) and Assigned(FOptionsFrame) then
  begin
    if SaveChanges then
    begin
      DisableActions := FOptionsFrame.DisableActions;
      HideActions := FOptionsFrame.HideActions;
      SaveConfirmation := FOptionsFrame.SaveConfirmation;
      ActOnTopSandbox := FOptionsFrame.ActOnTopSandbox;
      FMenuOrganization.Assign(FOptionsFrame.MenuTree);
      IconType := FOptionsFrame.IconType;
      RefreshMenu;
    end;
    FreeAndNil(FOptionsFrame);
  end
  else
    inherited ConfigurationClosed(AControl, SaveChanges);
end;

constructor TJclVersionControlExpert.Create;
var
  Index: Integer;
begin
  Supports(BorlandIDEServices, IOTAModuleServices, FModuleServices);
  if not Assigned(FModuleServices) then
    raise EJclExpertException.CreateTrace(RsENoModuleServices);
    
  FMenuOrganization := TStringList.Create;
  FPluginList := TList.Create;
  FIconCache := TList.Create;
  FFileCache := TList.Create;

  for Index := 0 to PluginClassList.Count - 1 do
    FPluginList.Add(TJclVersionControlPluginClass(PluginClassList.Items[Index]).Create(Self));

  inherited Create('JclVersionControlExpert');

  if not Assigned(ExpertInstanceList) then
    ExpertInstanceList := TList.Create;
  ExpertInstanceList.Add(Self);
end;

destructor TJclVersionControlExpert.Destroy;
var
  Index: Integer;
begin
  ExpertInstanceList.Remove(Self);

  inherited Destroy;

  for Index := FPluginList.Count - 1 downto 0 do
    TJclVersionControlPlugin(FPluginList.Items[Index]).Free;
  FPluginList.Free;

  for Index := FIconCache.Count - 1 downto 0 do
    TJclIconCacheInfo(FIconCache.Items[Index]).Free;
  FIconCache.Free;

  for Index := FFileCache.Count - 1 downto 0 do
    TJclVersionControlCache(FFileCache.Items[Index]).Free;

  FMenuOrganization.Free;
end;

procedure TJclVersionControlExpert.DropDownMenuPopup(Sender: TObject);
var
  APopupMenu: TPopupMenu;
  AMenuItem: TMenuItem;
  AControlAction: TJclVersionControlAction;
  AFileCache: TJclVersionControlCache;
  Index: Integer;
begin
  try
    APopupMenu := Sender as TPopupMenu;
    AControlAction := TJclVersionControlAction(APopupMenu.Tag);

    CleanSubMenus(APopupMenu.Items);

    AFileCache := CurrentCache;
    if Assigned(AFileCache) then
      for Index := 0 to AFileCache.SandBoxCount - 1 do
        if AControlAction in AFileCache.SandBoxActions[Index] then
    begin
      AMenuItem := TMenuItem.Create(nil);
      AMenuItem.Caption := AFileCache.SandBoxes[Index];
      AMenuItem.Tag := APopupMenu.Tag;
      AMenuItem.OnClick := SubItemClick;
      APopupMenu.Items.Add(AMenuItem);
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

function TJclVersionControlExpert.GetCurrentCache: TJclVersionControlCache;
var
  Index: Integer;
  AFileName: string;
  APlugin: TJclVersionControlPlugin;
begin
  AFileName := CurrentFileName;
  for Index := 0 to FPluginList.Count - 1 do
  begin
    APlugin := TJclVersionControlPlugin(FPluginList.Items[Index]);
    Result := GetFileCache(AFileName, APlugin);
    if not (vcaCheckOutSandbox in Result.Actions) then
      Exit;
  end;
  Result := nil;
end;

function TJclVersionControlExpert.GetCurrentFileName: string;
var
  AOTAModule: IOTAModule;
begin
  AOTAModule := ModuleServices.CurrentModule;
  if Assigned(AOTAModule) and (AOTAModule.FileSystem = '') then
    Result := AOTAModule.FileName
  else
    Result := '';
end;

function TJclVersionControlExpert.GetCurrentPlugin: TJclVersionControlPlugin;
var
  Index: Integer;
  AFileCacheInfo: TJclVersionControlCache;
  AFileName: string;
begin
  AFileName := CurrentFileName;
  for Index := 0 to FPluginList.Count - 1 do
  begin
    Result := TJclVersionControlPlugin(FPluginList.Items[Index]);
    AFileCacheInfo := GetFileCache(AFileName, Result);
    if not (vcaCheckOutSandbox in AFileCacheInfo.Actions) then
      Exit;
  end;
  Result := nil;
end;

function TJclVersionControlExpert.GetFileCache(const FileName: string;
  const Plugin: TJclVersionControlPlugin): TJclVersionControlCache;
var
  Index: Integer;
  AFileCache: TJclVersionControlCache;
  ATime: TDateTime;
begin
  ATime := Date;
  Result := nil;

  for Index := FFileCache.Count - 1 downto 0 do
  begin
    AFileCache := TJclVersionControlCache(FFileCache.Items[Index]);
    if AFileCache.GetValid(ATime) then
    begin
      AFileCache.Free;
      FFileCache.Delete(Index);
    end
    else if (AFileCache.FileName = FileName) and (AFileCache.Plugin = Plugin) then
    begin
      Result := AFileCache;
      Break;
    end;
  end;
  if not Assigned(Result) then
  begin
    Result := TJclVersionControlCache.Create(Plugin, FileName);
    FFileCache.Add(Result);
  end;
end;

function TJclVersionControlExpert.GetPlugin(
  Index: Integer): TJclVersionControlPlugin;
begin
  Result := TJclVersionControlPlugin(FPluginList.Items[Index]);
end;

function TJclVersionControlExpert.GetPluginCount: Integer;
begin
  Result := FPluginList.Count;
end;

procedure TJclVersionControlExpert.IDEActionMenuClick(Sender: TObject);
var
  AMenuItem, SubMenuItem: TMenuItem;
  AControlAction: TJclVersionControlAction;
  Index: Integer;
  AFileCache: TJclVersionControlCache;
begin
  try
    AMenuItem := Sender as TMenuItem;
    // do not delete the dummy subitem
    for Index := AMenuItem.Count - 1 downto 1 do
      AMenuItem.Items[Index].Free;
    AFileCache := CurrentCache;
    AControlAction := TJclVersionControlAction(AMenuItem.Tag);
    if Assigned(AFileCache) then
      for Index := 0 to AFileCache.SandBoxCount - 1 do
        if AControlAction in AFileCache.SandBoxActions[Index] then
    begin
      SubMenuItem := TMenuItem.Create(nil);
      SubMenuItem.Caption := AFileCache.SandBoxes[Index];
      SubMenuItem.Tag := Integer(AControlAction);
      SubMenuItem.OnClick := SubItemClick;
      AMenuItem.Add(SubMenuItem);
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TJclVersionControlExpert.IDEVersionCtrlMenuClick(Sender: TObject);
  procedure UpdateMenuItem(const AMenuItem: TMenuItem);
  var
    BMenuItem: TMenuItem;
    IndexMenu, IndexSandbox: Integer;
    AControlAction: TJclVersionControlAction;
    AFileCache: TJclVersionControlCache;
    AEnabled: Boolean;
  begin
    for IndexMenu := 0 to AMenuItem.Count - 1 do
    begin
      BMenuItem := AMenuItem.Items[IndexMenu];
      if BMenuItem.Tag = -1 then
        UpdateMenuItem(BMenuItem)
      else if BMenuItem.Tag >= 0 then
      begin
        AControlAction := TJclVersionControlAction(BMenuItem.Tag);
        if VersionControlActionInfos[AControlAction].Sandbox then
        begin
          AFileCache := CurrentCache;
          if IconType = -1 then
          begin
            if Assigned(AFileCache.Plugin) then
              BMenuItem.ImageIndex := AFileCache.Plugin.GetIcon(AControlAction)
            else
              BMenuItem.ImageIndex := -1;
          end;
          if HideActions then
            BMenuItem.Visible := Assigned(AFileCache.Plugin)
              and (AControlAction in AFileCache.Plugin.SupportActions)
          else
            BMenuItem.Visible := True;
          if DisableActions then
          begin
            AEnabled := False;
            for IndexSandbox := 0 to AFileCache.SandboxCount - 1 do
              if AControlAction in AFileCache.SandboxActions[IndexSandbox] then
            begin
              AEnabled := True;
              Break;
            end;
            BMenuItem.Enabled := AEnabled;
          end
          else
            BMenuItem.Enabled := True;
        end;
      end;
    end;
  end;
begin
  try
    UpdateMenuItem(FVersionCtrlMenu);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TJclVersionControlExpert.RefreshIcon(const AAction: TCustomAction);
var
  ControlAction: TJclVersionControlAction;
begin
  if not Assigned(AAction) then
    Exit;

  for ControlAction := Low(TJclVersionControlAction) to High(TJclVersionControlAction) do
    if FActions[ControlAction] = AAction then
  begin
    case IconType of
      // No icon
      -3 :
        AAction.ImageIndex := -1;
      // JCL icons
      // TODO: create resources
      -2 :
        AAction.ImageIndex := -1;
      // auto icons
      -1 :
        if Assigned(FLastPlugin) then
          AAction.ImageIndex := FLastPlugin.GetIcon(ControlAction)
        else
          AAction.ImageIndex := -1;
      // Specific icons
      0..High(Integer) :
        if IconType < FPluginList.Count then
          AAction.ImageIndex := TJclVersionControlPlugin(FPluginList.Items[IconType]).Icons[ControlAction]
        else
          AAction.ImageIndex := -1;
    end;
    Exit;
  end;
end;

procedure TJclVersionControlExpert.RefreshIcons;
var
  Action: TJclVersionControlAction;
begin
  for Action := Low(TJclVersionControlAction) to High(TJclVersionControlAction) do
    if Assigned(FActions[Action]) then
  begin
    case IconType of
      // No icon
      -3 :
        FActions[Action].ImageIndex := -1;
      // JCL icons
      // TODO: create resources
      -2 :
        FActions[Action].ImageIndex := -1;
      // Auto icons
      -1 :
        if Assigned(FLastPlugin) then
          FActions[Action].ImageIndex := FLastPlugin.Icons[Action]
        else
          FActions[Action].ImageIndex := -1;
      // Specific icons
      0..High(Integer) :
        if IconType < FPluginList.Count then
          FActions[Action].ImageIndex := TJclVersionControlPlugin(FPluginList.Items[IconType]).Icons[Action]
        else
          FActions[Action].ImageIndex := -1;
    end;
  end;
end;

procedure TJclVersionControlExpert.RefreshMenu;
  procedure LoadDefaultMenu;
  var
    Action: TJclVersionControlAction;
  begin
    FMenuOrganization.Clear;
    for Action := Low(TJclVersionControlAction) to High(TJclVersionControlAction) do
      FMenuOrganization.Add(Format('%d%s', [Integer(Action), GetEnumName(TypeInfo(TJclVersionControlAction), Integer(Action))]));
  end;
var
  Index, IndexA, IndexB, ActionIndex: Integer;
  SubMenuItem, ActionMenuItem, DummyMenuItem: TMenuItem;
  Item, ItemName: string;
  AAction: TCustomAction;
begin
  CleanSubMenus(FVersionCtrlMenu);

  if FMenuOrganization.Count > 0 then
  try
    FMenuOrganization.CustomSort(MenuOrganizationSort);
  except
    LoadDefaultMenu;
  end
  else
    LoadDefaultMenu;

  SubMenuItem := nil;
  for Index := 0 to FMenuOrganization.Count - 1 do
  begin
    Item := FMenuOrganization.Strings[Index];
    IndexA := GetItemIndexA(Item);
    IndexB := GetItemIndexB(Item);
    ItemName := GetItemName(Item);
    ActionIndex := GetEnumValue(TypeInfo(TJclVersionControlAction), ItemName);

    if IndexB = -1 then
    begin
      if FVersionCtrlMenu.Count <> IndexA then
        Abort;

      if (ActionIndex = -1) or (ItemName = '-') then
      begin
        SubMenuItem := TMenuItem.Create(nil);
        SubMenuItem.Caption := ItemName;
        SubMenuItem.Tag := -1;
        FVersionCtrlMenu.Add(SubMenuItem);
      end
      else
      begin
        ActionMenuItem := TMenuItem.Create(nil);
        AAction := FActions[TJclVersionControlAction(ActionIndex)];
        if VersionControlActionInfos[TJclVersionControlAction(ActionIndex)].Sandbox then
        begin
          ActionMenuItem.Caption := AAction.Caption;
          ActionMenuItem.ShortCut := AAction.ShortCut;
          ActionMenuItem.ImageIndex := AAction.ImageIndex;
          ActionMenuItem.Tag := ActionIndex;
          ActionMenuItem.OnClick := IDEActionMenuClick;

          // to always have the arrow in the parent menu item
          DummyMenuItem := TMenuItem.Create(nil);
          DummyMenuItem.Visible := False;
          DummyMenuItem.Tag := -2;
          ActionMenuItem.Add(DummyMenuItem);
        end
        else
          ActionMenuItem.Action := AAction;
        FVersionCtrlMenu.Add(ActionMenuItem);
        SubMenuItem := nil;
      end;
    end
    else
    begin
      if (not Assigned(SubMenuItem)) or (SubMenuItem.Count <> IndexB) then
        Abort;
      if (ActionIndex = -1) or (ItemName = '-') then
      begin
        ActionMenuItem := TMenuItem.Create(nil);
        ActionMenuItem.Caption := ItemName;
      end
      else
      begin
        ActionMenuItem := TMenuItem.Create(nil);
        AAction := FActions[TJclVersionControlAction(ActionIndex)];
        if VersionControlActionInfos[TJclVersionControlAction(ActionIndex)].Sandbox then
        begin
          ActionMenuItem.Caption := AAction.Caption;
          ActionMenuItem.ShortCut := AAction.ShortCut;
          ActionMenuItem.ImageIndex := AAction.ImageIndex;
          ActionMenuItem.Tag := ActionIndex;
          ActionMenuItem.OnClick := IDEActionMenuClick;

          // to always have the arrow in the parent menu item
          DummyMenuItem := TMenuItem.Create(nil);
          DummyMenuItem.Visible := False;
          DummyMenuItem.Tag := -2;
          ActionMenuItem.Add(DummyMenuItem);
        end
        else
          ActionMenuItem.Action := AAction;
      end;
      SubMenuItem.Add(ActionMenuItem);
    end;
  end;
end;

procedure TJclVersionControlExpert.RegisterCommands;
var
  IDEMainMenu: TMainMenu;
  IDEToolsItem: TMenuItem;
  IDEActionList: TCustomActionList;
  I: Integer;
  AAction: TCustomAction;
  ADropDownAction: TDropDownAction;
  IconTypeStr: string;
  ControlAction: TJclVersionControlAction;
begin
  inherited RegisterCommands;

  Settings.LoadStrings(JclVersionCtrlMenuOrganizationName, FMenuOrganization);
  SaveConfirmation := Settings.LoadBool(JclVersionCtrlSaveConfirmationName, True);
  DisableActions := Settings.LoadBool(JclVersionCtrlDisableActionsName, True);
  HideActions := Settings.LoadBool(JclVersionCtrlHideActionsName, False);
  IconTypeStr := Settings.LoadString(JclVersionCtrlIconTypeName, JclVersionCtrlIconTypeAutoValue);
  ActOnTopSandbox := Settings.LoadBool(JclVersionCtrlActOnTopSandboxName, False);

  FIconType := -1;
  if IconTypeStr = JclVersionCtrlIconTypeNoIconValue then
    FIconType := -3
  else if IconTypeStr = JclVersionCtrlIconTypeJclIconValue then
    FIconType := -2
  else if IconTypeStr = JclVersionCtrlIconTypeAutoValue then
    FIconType := -1
  else for I := 0 to FPluginList.Count - 1 do
    if IconTypeStr = TJclVersionControlPlugin(FPluginList.Items[I]).Name then
      FIconType := I;

  IDEMainMenu := NTAServices.MainMenu;
  IDEToolsItem := nil;
  for I := 0 to IDEMainMenu.Items.Count - 1 do
    if IDEMainMenu.Items[I].Name = 'ToolsMenu' then
  begin
    IDEToolsItem := IDEMainMenu.Items[I];
    Break;
  end;
  if not Assigned(IDEToolsItem) then
    raise EJclExpertException.CreateTrace(RsENoToolsMenuItem);

  IDEActionList := NTAServices.ActionList;

  FVersionCtrlMenu := TMenuItem.Create(nil);
  FVersionCtrlMenu.Caption := RsVersionCtrlMenuCaption;
  FVersionCtrlMenu.Name := JclVersionCtrlMenuName;
  FVersionCtrlMenu.OnClick := IDEVersionCtrlMenuClick;
  IDEMainMenu.Items.Insert(IDEToolsItem.MenuIndex + 1, FVersionCtrlMenu);
  if not Assigned(FVersionCtrlMenu.Parent) then
    raise EJclExpertException.CreateTrace(Format(RsSvnMenuItemNotInserted, [FVersionCtrlMenu.Caption]));

  for ControlAction := Low(TJclVersionControlAction) to High(TJclVersionControlAction) do
  begin
    if VersionControlActionInfos[ControlAction].Sandbox then
    begin
      ADropDownAction := TDropDownAction.Create(nil);
      ADropDownAction.DropdownMenu := TPopupMenu.Create(nil);
      ADropDownAction.DropdownMenu.AutoPopup := True;
      ADropDownAction.DropdownMenu.AutoHotkeys := maManual;
      ADropDownAction.DropdownMenu.Tag := Integer(ControlAction);
      ADropDownAction.DropdownMenu.OnPopup := DropDownMenuPopup;
      AAction := ADropDownAction;
    end
    else
      AAction := TAction.Create(nil);

    AAction.Caption := VersionControlActionInfos[ControlAction].Caption;
    AAction.Name := VersionControlActionInfos[ControlAction].ActionName;
    AAction.Visible := True;
    AAction.ActionList := IDEActionList;
    AAction.OnExecute := ActionExecute;
    AAction.OnUpdate := ActionUpdate;
    AAction.Category := RsActionCategory;
    RegisterAction(AAction);
    FActions[ControlAction] := AAction;
  end;

  RefreshIcons;

  RefreshMenu;
end;

function TJclVersionControlExpert.SaveModules(const FileName: string;
  const IncludeSubDirectories: Boolean): Boolean;
var
  Module: IOTAModule;
  Index: Integer;
  Save: Boolean;
begin
  Result := True;

  for Index := 0 to ModuleServices.ModuleCount - 1 do
  begin
    Module := ModuleServices.Modules[Index];

    if Module.FileSystem <> '' then
    begin
      if IncludeSubDirectories then
        Save := PathIsChild(Module.FileName, FileName)
      else
        Save := Module.FileName = FileName;

      if Save then
        Module.Save(False, True);
    end;
  end;
end;

procedure TJclVersionControlExpert.SetIconType(const Value: Integer);
begin
  if Value <> FIconType then
  begin
    FIconType := Value;
    RefreshIcons;
  end;
end;

procedure TJclVersionControlExpert.SubItemClick(Sender: TObject);
var
  APlugin: TJclVersionControlPlugin;
  AMenuItem: TMenuItem;
  AAction: TCustomAction;
  Directory: string;
  ControlAction: TJclVersionControlAction;
begin
  try
    APlugin := CurrentPlugin;
    if Sender is TCustomAction then
    begin
      AAction := TCustomAction(Sender);
      ControlAction := TJclVersionControlAction(AAction.Tag);
      Directory := AAction.Caption;
    end
    else if Sender is TMenuItem then
    begin
      AMenuItem := TMenuItem(Sender);
      ControlAction := TJclVersionControlAction(AMenuItem.Tag);
      Directory := AMenuItem.Caption;
    end
    else
      Exit;

    Directory := StrRemoveChars(Directory, ['&']);
    if VersionControlActionInfos[ControlAction].SaveFile then
      SaveModules(Directory, True);
    if Assigned(APlugin) then
      APlugin.ExecuteAction(Directory , ControlAction);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TJclVersionControlExpert.UnregisterCommands;
var
  ControlAction: TJclVersionControlAction;
  ADropDownAction: TDropDownAction;
begin
  inherited UnregisterCommands;

  Settings.SaveStrings(JclVersionCtrlMenuOrganizationName, FMenuOrganization);
  Settings.SaveBool(JclVersionCtrlSaveConfirmationName, SaveConfirmation);
  Settings.SaveBool(JclVersionCtrlDisableActionsName, DisableActions);
  Settings.SaveBool(JclVersionCtrlHideActionsName, HideActions);
  Settings.SaveBool(JclVersionCtrlActOnTopSandboxName, ActOnTopSandbox);
  case FIconType of
    -3:
      Settings.SaveString(JclVersionCtrlIconTypeName, JclVersionCtrlIconTypeNoIconValue);
    -2:
      Settings.SaveString(JclVersionCtrlIconTypeName, JclVersionCtrlIconTypeJclIconValue);
    -1:
      Settings.SaveString(JclVersionCtrlIconTypeName, JclVersionCtrlIconTypeAutoValue);
    0..High(Integer):
      Settings.SaveString(JclVersionCtrlIconTypeName, TJclVersionControlPlugin(FPluginList.Items[IconType]).Name);
  end;

  for ControlAction := Low(TJclVersionControlAction) to High(TJclVersionControlAction) do
  begin
    UnregisterAction(FActions[ControlAction]);
    if FActions[ControlAction] is TDropDownAction then
    begin
      ADropDownAction := TDropDownAction(FActions[ControlAction]);
      if Assigned(ADropDownAction.DropDownMenu) then
      begin
        CleanSubMenus(ADropDownAction.DropDownMenu.Items);
        ADropDownAction.DropDownMenu.Free;
        ADropDownAction.DropDownMenu := nil;
      end;
    end;
    FreeAndNil(FActions[ControlAction]);
  end;
  CleanSubMenus(FVersionCtrlMenu);
  FreeAndNil(FVersionCtrlMenu);
  FVersionCtrlMenu := nil;
end;

//=== TJclVersionControlPlugin ===============================================

constructor TJclVersionControlPlugin.Create(const AExpert: TJclVersionControlExpert);
begin
  inherited Create;
  FExpert := AExpert;
end;

function TJclVersionControlPlugin.ExecuteAction(const FileName: string;
  const Action: TJclVersionControlAction): Boolean;
begin
  case Action of
    vcaContextMenu:
      Result := DisplayContextMenu(0, FileName, Mouse.CursorPos);
    vcaExploreSandbox:
      Result := OpenFolder(FileName, Application.Handle, True);
    vcaProperties,
    vcaPropertiesSandbox:
      Result := DisplayPropDialog(Application.Handle, FileName);
    else
      Result := False;
  end;
end;

function TJclVersionControlPlugin.GetEnabled: Boolean;
begin
  Result := False;
end;

function TJclVersionControlPlugin.GetFileActions(
  const FileName: string): TJclVersionControlActions;
begin
  Result := [vcaContextMenu, vcaExploreSandbox, vcaProperties, vcaPropertiesSandbox];
end;

function TJclVersionControlPlugin.GetSupportedActions: TJclVersionControlActions;
begin
  Result := [vcaContextMenu, vcaExploreSandbox, vcaProperties, vcaPropertiesSandbox];
end;

function TJclVersionControlPlugin.GetIcon(
  const Action: TJclVersionControlAction): Integer;
begin
  Result := -1;
end;

function TJclVersionControlPlugin.GetName: string;
begin
  Result := '';
end;

function TJclVersionControlPlugin.GetSandboxActions(
  const SdBxName: string): TJclVersionControlActions;
begin
  Result := [vcaExploreSandbox, vcaPropertiesSandbox];
end;

function TJclVersionControlPlugin.GetSandboxNames(const FileName: string;
  SdBxNames: TStrings): Boolean;
var
  Index: Integer;
begin
  Result := False;

  SdBxNames.BeginUpdate;
  try
    SdBxNames.Clear;
    for Index := Length(FileName) downto 1 do
      if FileName[Index] = PathSeparator then
    begin
      SdBxNames.Add(Copy(FileName, 1, Index));
    end;
  finally
    SdBxNames.EndUpdate;
  end;
end;

//=== TJclVersionControlActionsCache =========================================

type
  TJclVersionControlActionsCache = class (TObject)
  private
    FSandbox: string;
    FActions: TJclVersionControlActions;
  public
    constructor Create(ASandbox: string; AActions: TJclVersionControlActions);
    property Sandbox: string read FSandbox;
    property Actions: TJclVersionControlActions read FActions;
  end;

constructor TJclVersionControlActionsCache.Create(ASandbox: string;
  AActions: TJclVersionControlActions);
begin
  inherited Create;
  FSandbox := ASandbox;
  FActions := AActions;
end;

//=== TJclVersionControlCache ================================================

constructor TJclVersionControlCache.Create(APlugin: TJclVersionControlPlugin;
  AFileName: string);
var
  Index: Integer;
  SandboxNames: TStrings;
begin
  inherited Create;

  FSandboxList := TList.Create;
  FFileName := AFileName;
  FPlugin := APlugin;
  // TODO: cache time validity customization
  FValidityTime := Date + 5.0 / SecsPerDay;
  FActions := APlugin.FileActions[FileName];

  SandboxNames := TStringList.Create;
  try
    APlugin.GetSandboxNames(FileName, SandboxNames);

  for Index := 0 to SandboxNames.Count - 1 do
    FSandboxList.Add(TJclVersionControlActionsCache.Create(SandboxNames.Strings[Index], APlugin.SandboxActions[SandboxNames.Strings[Index]]));
  finally
    SandboxNames.Free;
  end;
end;

destructor TJclVersionControlCache.Destroy;
var
  Index: Integer;
begin
  for Index := 0 to FSandboxList.Count - 1 do
    TJclVersionControlActionsCache(FSandboxList.Items[Index]).Free;
  FSandboxList.Free;
  
  inherited Destroy;
end;

function TJclVersionControlCache.GetSandBox(Index: Integer): string;
begin
  Result := TJclVersionControlActionsCache(FSandboxList.Items[Index]).Sandbox;
end;

function TJclVersionControlCache.GetSandboxAction(
  Index: Integer): TJclVersionControlActions;
begin
  Result := TJclVersionControlActionsCache(FSandboxList.Items[Index]).Actions;
end;

function TJclVersionControlCache.GetSandboxCount: Integer;
begin
  Result := FSandboxList.Count;
end;

function TJclVersionControlCache.GetValid(const ATime: TDateTime): Boolean;
begin
  Result := (ATime - FValidityTime) < 0;
end;

initialization

finalization

FreeAndNil(ExpertInstanceList);
FreeAndNil(PluginClassList);

end.

// History:

// $Log$
// Revision 1.6  2006/01/19 07:58:11  elahn
// Bugfix - ContextMenu not appearing at Mouse.CursorPos
//
// Revision 1.5  2006/01/15 20:58:03  outchy
// Delphi 5 support: no TCustomAction.AutoCheck property
// Removed unused resources
//
// Revision 1.4  2006/01/15 11:33:21  outchy
// cvs support in version control expert
// version control expert integration in the installer
//