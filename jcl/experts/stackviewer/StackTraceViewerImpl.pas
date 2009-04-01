unit StackTraceViewerImpl;

{$I jcl.inc}

interface

uses
  Windows, Classes, Menus, ActnList, ToolsAPI, SysUtils, Graphics, Dialogs, Controls, Forms,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclOtaUtils, StackViewForm, StackTraceViewerConfigFrame, ExceptionViewerOptionsUnit;//td1

{$R 'JclSIMDIcon.dcr'} //todo - own icon

type
  TJclStackTraceViewerExpert = class(TJclOTAExpert)
  private
    FIcon: TIcon;
    FOptions: TExceptionViewerOption;
    FOptionsFrame: TJclStackTraceViewerConfigFrame;
    FStackTraceViewMenuItem: TMenuItem;
    FStackTraceViewAction: TAction;
    procedure ActionExecute(Sender: TObject);
    procedure LoadExpertValues;
    procedure SaveExpertValues;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure RegisterCommands; override;
    procedure UnregisterCommands; override;
    procedure AddConfigurationPages(AddPageFunc: TJclOTAAddPageFunc); override;
    procedure ConfigurationClosed(AControl: TControl; SaveChanges: Boolean); override;
  end;

// design package entry point
procedure Register;

// expert DLL entry point
function JCLWizardInit(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var TerminateProc: TWizardTerminateProc): Boolean; stdcall;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL:  $';
    Revision: '$Revision:  $';
    Date: '$Date:  $';
    LogPath: ''
    );
{$ENDIF UNITVERSIONING}

implementation

{$R JclDebugIdeIcon.res}//todo - own icon

uses
  JclDebug, JclFileUtils, JclOtaConsts,
  JclOtaResources;

resourcestring
  rsStackTraceViewerCaption = 'Stack Traces';//todo - move to JclOtaResources.pas

const
  JclStackTraceViewerExpertName = 'JclStackTraceViewerExpert';//todo - move to JclOtaConsts.pas
  JclStackTraceViewerActionName = 'JCLStackTraceViewerCommand';
  JclStackTraceViewerMenuName   = 'JCLStackTraceViewerMenu';

procedure Register;
begin
  try
    RegisterPackageWizard(TJclStackTraceViewerExpert.Create);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

var
  JCLWizardIndex: Integer;

procedure JclWizardTerminate;
begin
  try
    if JCLWizardIndex <> -1 then
      TJclOTAExpertBase.GetOTAWizardServices.RemoveWizard(JCLWizardIndex);
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
begin
  try
    TerminateProc := JclWizardTerminate;

    JCLWizardIndex := TJclOTAExpertBase.GetOTAWizardServices.AddWizard(TJclStackTraceViewerExpert.Create);

    Result := True;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      Result := False;
    end;
  end;
end;

//=== { TJclStackTraceViewerExpert } =========================================

constructor TJclStackTraceViewerExpert.Create;
begin
  inherited Create(JclStackTraceViewerExpertName);
  FOptions := TExceptionViewerOption.Create;
end;

destructor TJclStackTraceViewerExpert.Destroy;
begin
  FOptions.Free;
  FreeAndNil(frmStackView);
  inherited Destroy;
end;

procedure TJclStackTraceViewerExpert.ActionExecute(Sender: TObject);
begin
  try
    if not Assigned(frmStackView) then
    begin
      frmStackView := TfrmStackView.Create(Application);
      frmStackView.Icon := FIcon;
      frmStackView.Options := FOptions;
      frmStackView.RootDir := RootDir;
      frmStackView.Show;
    end
    else
      frmStackView.Show;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TJclStackTraceViewerExpert.AddConfigurationPages(AddPageFunc: TJclOTAAddPageFunc);
begin
  inherited AddConfigurationPages(AddPageFunc);
  FOptionsFrame := TJclStackTraceViewerConfigFrame.Create(nil);
  FOptionsFrame.Options := FOptions;
  AddPageFunc(FOptionsFrame, 'Stack Trace Viewer', Self);//todo - resourcestring
end;

procedure TJclStackTraceViewerExpert.ConfigurationClosed(AControl: TControl; SaveChanges: Boolean);
begin
  if (AControl = FOptionsFrame) and Assigned(FOptionsFrame) then
  begin
    if SaveChanges then
      FOptions.Assign(FOptionsFrame.Options);
    FreeAndNil(FOptionsFrame);
    if SaveChanges and Assigned(frmStackView) then
      frmStackView.Options := FOptions;
  end
  else
    inherited ConfigurationClosed(AControl, SaveChanges);
end;

procedure TJclStackTraceViewerExpert.LoadExpertValues;
begin
  FOptions.ExpandTreeView := Settings.LoadBool('ExpandTreeView', FOptions.ExpandTreeView);
end;

procedure TJclStackTraceViewerExpert.RegisterCommands;
var
  I, ViewDebugMenuIdx: Integer;
  IDEMenu: TMainMenu;
  ViewMenu: TMenuItem;
  Category: string;
  ImageBmp: TBitmap;
  NTAServices: INTAServices;
begin
  inherited RegisterCommands;

  NTAServices := GetNTAServices;

  Category := '';
  { TODO : verify if command exists in <= D2007 }
  for I := 0 to NTAServices.ActionList.ActionCount - 1 do
    if CompareText(NTAServices.ActionList.Actions[I].Name, 'ViewPrjMgrCommand') = 0 then
    begin
      Category := NTAServices.ActionList.Actions[I].Category;
      Break;
    end;

  FIcon := TIcon.Create;
  FIcon.Handle := LoadIcon(FindResourceHInstance(ModuleHInstance), 'SIMDICON');//todo - resource name

  // create actions
  FStackTraceViewAction := TAction.Create(nil);
  FStackTraceViewAction.Caption := rsStackTraceViewerCaption;
  FStackTraceViewAction.Visible := True;
  FStackTraceViewAction.OnExecute := ActionExecute;
  FStackTraceViewAction.Category := Category;
  FStackTraceViewAction.Name := JclStackTraceViewerActionName;
  FStackTraceViewAction.ActionList := NTAServices.ActionList;
  ImageBmp := TBitmap.Create;
  try
    ImageBmp.LoadFromResourceName(FindResourceHInstance(ModuleHInstance), 'JCLDEBUG');//todo - resource name
    FStackTraceViewAction.ImageIndex := NTAServices.AddMasked(ImageBmp, clPurple);
  finally
    ImageBmp.Free;
  end;

  FStackTraceViewMenuItem := TMenuItem.Create(nil);
  FStackTraceViewMenuItem.Name := JclStackTraceViewerMenuName;
  FStackTraceViewMenuItem.Action := FStackTraceViewAction;

  IDEMenu := NTAServices.MainMenu;

  LoadExpertValues;

  ViewMenu := nil;
  for I := 0 to IDEMenu.Items.Count - 1 do
    if CompareText(IDEMenu.Items[I].Name, 'ViewsMenu') = 0 then
      ViewMenu := IDEMenu.Items[I];
  if not Assigned(ViewMenu) then
    raise EJclExpertException.CreateTrace(RsENoViewMenuItem);

  ViewDebugMenuIdx := -1;
  for I := 0 to ViewMenu.Count - 1 do
    if CompareText(ViewMenu.Items[I].Name, 'ViewDebugItem') = 0 then
    begin
      ViewDebugMenuIdx := I;
      Break;
    end;
  if ViewDebugMenuIdx = -1 then
    raise EJclExpertException.CreateTrace(RsENoDebugWindowsMenuItem);

  ViewMenu.Insert(ViewDebugMenuIdx + 1, FStackTraceViewMenuItem);

  RegisterAction(FStackTraceViewAction);
end;

procedure TJclStackTraceViewerExpert.SaveExpertValues;
begin
  Settings.SaveBool('ExpandTreeView', FOptions.ExpandTreeView);
end;

procedure TJclStackTraceViewerExpert.UnregisterCommands;
begin
  inherited UnregisterCommands;
  SaveExpertValues;
  UnregisterAction(FStackTraceViewAction);
  FreeAndNil(FIcon);
  FreeAndNil(FStackTraceViewMenuItem);
  FreeAndNil(FStackTraceViewAction);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
