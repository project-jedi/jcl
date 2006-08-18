unit JclInstall;

interface

{$I jcl.inc}

uses
  Windows, SysUtils, Classes, ComCtrls, JediInstallIntf;

type
  TJclInstall = class (TInterfacedObject, IJediInstall)
  private
    FJclPath: string;
    FJclSourcePath: string;
    FClxDialogFileName: string;
    FVclDialogFileName: string;
    FClxDialogIconFileName: string;
    FVclDialogIconFileName: string;
    FJclChmHelpFileName: string;
    FJclHlpHelpFileName: string;
    FJclReadmeFileName: string;
    FTool: IJediInstallTool;
  public
    function InitInformation(const ApplicationFileName: string): Boolean;
    function Install: Boolean;
    function PopulateTreeView(Nodes: TTreeNodes; VersionNumber: Integer; Page: TTabSheet): Boolean;
    function SelectedNodeCollapsing(Node: TTreeNode): Boolean;
    procedure SelectedNodeChanged(Node: TTreeNode);
    procedure SetTool(const Value: IJediInstallTool);
    property Tool: IJediInstallTool read FTool;
  end;

function CreateJediInstall: IJediInstall;

implementation

uses
  JclBase, JclFileUtils, JclStrings, DelphiInstall;

const
  DialogsPath       = 'examples\debugextension\dialog\';
  ClxDialogFileName = 'ClxExceptDlg.pas';
  VclDialogFileName = 'ExceptDlg.pas';
  ClxDialogName     = 'CLX Exception Dialog';
  VclDialogName     = 'Exception Dialog';
  DialogDescription = 'JCL Application exception dialog';
  DialogAuthor      = 'Project JEDI';
  DialogPage        = 'Dialogs';

  JclChmHelpFile    = 'Help\JCLHelp.chm';
  JclHlpHelpFile    = 'Help\JCLHelp.hlp';
  JclHelpTitle      = 'JCL 1.20 Help';
  JclHelpIndexName  = 'Jedi Code Library Reference';
  HHFileName        = 'HH.EXE';

  JclRuntimeDpk     = 'packages\DJCL%d0.dpk';
  JclIdeDebugDpk    = 'examples\debugextension\JclDebugIde%d0.dpk';
  JclIdeAnalyzerDpk = 'examples\projectanalyzer\ProjectAnalyzer%d0.dpk';
  JclIdeFavoriteDpk = 'examples\idefavopendialogs\IdeOpenDlgFavorite%d0.dpk';
  JclIdeThrNamesDpk = 'examples\debugextension\threadnames\ThreadNameExpert%d0.dpk';

  // Feature IDs
  FID_JCL                  = $02000000;

  FID_JCL_Env              = FID_JCL + $00010000;
  FID_JCL_EnvLibPath       = FID_JCL + $00010001;
  FID_JCL_Help             = FID_JCL + $00020000;
  FID_JCL_HelpHlp          = FID_JCL + $00020100;
  FID_JCL_HelpChm          = FID_JCL + $00020200;
  FID_JCL_Experts          = FID_JCL + $00030000;
  FID_JCL_ExpertDebug      = FID_JCL + $00030100;
  FID_JCL_ExpertAnalyzer   = FID_JCL + $00030200;
  FID_JCL_ExpertFavorite   = FID_JCL + $00030300;
  FID_JCL_ExpertsThrNames  = FID_JCL + $00030400;
  FID_JCL_ExcDialog        = FID_JCL + $00040000;
  FID_JCL_ExcDialogVCL     = FID_JCL + $00040100;
  FID_JCL_ExcDialogCLX     = FID_JCL + $00040200;

  // Products
  RsJCL             = 'JEDI Code Library';

  // Common features
  RsEnvironment     = 'Environment';
  RsEnvLibPath      = 'Include source code location to IDE Library Path';
  RsHelpFiles       = 'Help files';
  RsIdeExperts      = 'IDE experts';
  RsIdeHelpHlp      = 'Add help file to Delphi IDE help system';
  RsIdeHelpChm      = 'Add HTML help to the Tools menu';

  // Product specific features
  RsJCLExceptDlg    = 'Sample Exception Dialogs in the Object Reporitory';
  RsJCLDialogVCL    = 'VCL Exception Dialog';
  RsJCLDialogCLX    = 'CLX Exception Dialog';
  RsJCLIdeDebug     = 'Debug Extension';
  RsJCLIdeAnalyzer  = 'Project Analyzer';
  RsJCLIdeFavorite  = 'Favorite combobox in Open/Save dialogs';
  RsJCLIdeThrNames  = 'Displaying thread names in Thread Status window';

resourcestring
  RsSourceLibHint   = 'Adds "%s" to the Library Path';
  RsStatusMessage   = 'Installing %s ...';
  RsInstallFailed   = 'Installation of %s failed';

function CreateJediInstall: IJediInstall;
begin
  Result := TJclInstall.Create as IJediInstall;
end;

{ TJclInstall }

function TJclInstall.InitInformation(const ApplicationFileName: string): Boolean;
var
  ReadmeText: string;
begin
  FJclPath := PathAddSeparator(PathCanonicalize(PathExtractFileDirFixed(ApplicationFileName) + '..'));
  FJclSourcePath := FJclPath + 'Source';
  FClxDialogFileName := AnsiUpperCase(FJclPath + DialogsPath + ClxDialogFileName);
  FVclDialogFileName := AnsiUpperCase(FJclPath + DialogsPath + VclDialogFileName);
  FClxDialogIconFileName := ChangeFileExt(FClxDialogFileName, '.ICO');
  FVclDialogIconFileName := ChangeFileExt(FVclDialogFileName, '.ICO');
  FJclChmHelpFileName := FJclPath + JclChmHelpFile;
  FJclHlpHelpFileName := FJclPath + JclHlpHelpFile;
  if not FileExists(FJclChmHelpFileName) then
    FJclChmHelpFileName := '';
  if not FileExists(FJclHlpHelpFileName) then
    FJclHlpHelpFileName := '';
  // Reset ReadOnly flag for dialog forms
  FileSetAttr(FClxDialogFileName, faArchive);
  FileSetAttr(ChangeFileExt(FClxDialogFileName, '.xfm'), faArchive);
  FileSetAttr(FVclDialogFileName, faArchive);
  FileSetAttr(ChangeFileExt(FVclDialogFileName, '.dfm'), faArchive);
  Result := (FileExists(FClxDialogFileName) and FileExists(FVclDialogFileName) and
    FileExists(FClxDialogIconFileName) and FileExists(FVclDialogIconFileName));
  FJclReadmeFileName := PathAddSeparator(FJclPath) + 'Readme.txt';
  if FileExists(FJclReadmeFileName) then
  begin
    ReadmeText := FileToString(FJclReadmeFileName);
    Tool.UpdateInfo(4, ReadmeText);
    Tool.UpdateInfo(5, ReadmeText);
    Tool.UpdateInfo(6, ReadmeText);
  end;  
end;

function TJclInstall.Install: Boolean;
var
  Installation: TJclDelphiInstallation;

  procedure AddHelpToDelphiHelp;
  begin
    Installation.OpenHelp.AddHelpFile(FJclHlpHelpFileName, JclHelpIndexName);
  end;

  procedure AddHelpToIdeTools;
  var
    ToolsIndex: Integer;
  begin
    if Installation.IdeTools.IndexOfTitle(JclHelpTitle) = -1 then
    begin
      ToolsIndex := Installation.IdeTools.Count;
      Installation.IdeTools.Count := ToolsIndex + 1;
      Installation.IdeTools.Title[ToolsIndex] := JclHelpTitle;
      Installation.IdeTools.Path[ToolsIndex] := HHFileName;
      Installation.IdeTools.Parameters[ToolsIndex] := StrDoubleQuote(FJclChmHelpFileName);
      Installation.IdeTools.WorkingDir[ToolsIndex] := FJclPath;
    end;
  end;

  function InstallPackage(const Name: string): Boolean;
  var
    PackageFileName: string;
    I: Integer;
  begin
    PackageFileName := FJclPath + Format(Name, [Installation.VersionNumber]);
    if not FileExists(PackageFileName) and (Installation.VersionNumber >= 6) then
    begin
      I := Pos('0.dpk', PackageFileName);
      if I > 1 then
        Delete(PackageFileName, I - 1, 2);
    end;
    Tool.UpdateStatus(Format(RsStatusMessage, [ExtractFileName(PackageFileName)]));
    Result := Installation.Compiler.InstallPackage(PackageFileName);
    Tool.UpdateStatus('');
    if not Result then
      Tool.MessageBox(Format(RsInstallFailed, [PackageFileName]), MB_OK or MB_ICONERROR);
  end;

  procedure D4Install;
  begin
    Tool.UpdateStatus(Format(RsStatusMessage, [Installation.Name]));
    if Tool.FeatureChecked(FID_JCL_EnvLibPath, Installation.VersionNumber) then
      Installation.AddToLibrarySearchPath(FJclSourcePath);
    if Tool.FeatureChecked(FID_JCL_HelpHlp, Installation.VersionNumber) then
      AddHelpToDelphiHelp;
    if Tool.FeatureChecked(FID_JCL_HelpChm, Installation.VersionNumber) then
      AddHelpToIdeTools;
    if Tool.FeatureChecked(FID_JCL_ExcDialogVCL, Installation.VersionNumber) then
      Installation.Repository.AddObject(FVclDialogFileName, DelphiRepositoryFormTemplate,
        Installation.Repository.FindPage(DialogPage, 1), VclDialogName, FVclDialogIconFileName,
        DialogDescription, DialogAuthor, DelphiRepositoryDesignerDfm);
    if Tool.FeatureChecked(FID_JCL_Experts, Installation.VersionNumber) then
      InstallPackage(JclRuntimeDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertDebug, Installation.VersionNumber) then
      InstallPackage(JclIdeDebugDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertAnalyzer, Installation.VersionNumber) then
      InstallPackage(JclIdeAnalyzerDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertFavorite, Installation.VersionNumber) then
      InstallPackage(JclIdeFavoriteDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertsThrNames, Installation.VersionNumber) then
      InstallPackage(JclIdeThrNamesDpk);
  end;

  procedure D5Install;
  begin
    Tool.UpdateStatus(Format(RsStatusMessage, [Installation.Name]));
    if Tool.FeatureChecked(FID_JCL_EnvLibPath, Installation.VersionNumber) then
      Installation.AddToLibrarySearchPath(FJclSourcePath);
    if Tool.FeatureChecked(FID_JCL_HelpHlp, Installation.VersionNumber) then
      AddHelpToDelphiHelp;
    if Tool.FeatureChecked(FID_JCL_HelpChm, Installation.VersionNumber) then
      AddHelpToIdeTools;
    if Tool.FeatureChecked(FID_JCL_ExcDialogVCL, Installation.VersionNumber) then
      Installation.Repository.AddObject(FVclDialogFileName, DelphiRepositoryFormTemplate,
        Installation.Repository.FindPage(DialogPage, 1), VclDialogName, FVclDialogIconFileName,
        DialogDescription, DialogAuthor, DelphiRepositoryDesignerDfm);
    if Tool.FeatureChecked(FID_JCL_Experts, Installation.VersionNumber) then
      InstallPackage(JclRuntimeDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertDebug, Installation.VersionNumber) then
      InstallPackage(JclIdeDebugDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertAnalyzer, Installation.VersionNumber) then
      InstallPackage(JclIdeAnalyzerDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertFavorite, Installation.VersionNumber) then
      InstallPackage(JclIdeFavoriteDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertsThrNames, Installation.VersionNumber) then
      InstallPackage(JclIdeThrNamesDpk);
  end;

  procedure D6Install;
  begin
    Tool.UpdateStatus(Format(RsStatusMessage, [Installation.Name]));
    if Tool.FeatureChecked(FID_JCL_EnvLibPath, Installation.VersionNumber) then
      Installation.AddToLibrarySearchPath(FJclSourcePath);
    if Tool.FeatureChecked(FID_JCL_HelpHlp, Installation.VersionNumber) then
      AddHelpToDelphiHelp;
    if Tool.FeatureChecked(FID_JCL_HelpChm, Installation.VersionNumber) then
      AddHelpToIdeTools;
    if Tool.FeatureChecked(FID_JCL_ExcDialogVCL, Installation.VersionNumber) then
      Installation.Repository.AddObject(FVclDialogFileName, DelphiRepositoryFormTemplate,
        Installation.Repository.FindPage(DialogPage, 1), VclDialogName, FVclDialogIconFileName,
        DialogDescription, DialogAuthor, DelphiRepositoryDesignerDfm);
    if Tool.FeatureChecked(FID_JCL_ExcDialogCLX, Installation.VersionNumber) then
      Installation.Repository.AddObject(FClxDialogFileName, DelphiRepositoryFormTemplate,
        Installation.Repository.FindPage(DialogPage, 1), ClxDialogName, FClxDialogIconFileName,
        DialogDescription, DialogAuthor, DelphiRepositoryDesignerXfm);
    if Tool.FeatureChecked(FID_JCL_Experts, Installation.VersionNumber) then
      InstallPackage(JclRuntimeDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertDebug, Installation.VersionNumber) then
      InstallPackage(JclIdeDebugDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertAnalyzer, Installation.VersionNumber) then
      InstallPackage(JclIdeAnalyzerDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertFavorite, Installation.VersionNumber) then
      InstallPackage(JclIdeFavoriteDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertsThrNames, Installation.VersionNumber) then
      InstallPackage(JclIdeThrNamesDpk);
  end;

begin
  Result := True;
  try
    Installation := Tool.DelphiInstallations.InstallationFromVersion[4];
    if Assigned(Installation) and Installation.Valid then
      D4Install;
    Installation := Tool.DelphiInstallations.InstallationFromVersion[5];
    if Assigned(Installation) and Installation.Valid then
      D5Install;
    Installation := Tool.DelphiInstallations.InstallationFromVersion[6];
    if Assigned(Installation) and Installation.Valid then
      D6Install;
  finally
    Tool.UpdateStatus('');
  end;
end;

function TJclInstall.PopulateTreeView(Nodes: TTreeNodes; VersionNumber: Integer; Page: TTabSheet): Boolean;
var
  InstallationNode, ProductNode, TempNode: TTreeNode;
  Installation: TJclDelphiInstallation;

  function AddNode(Parent: TTreeNode; const Caption: string; FeatureID: Cardinal): TTreeNode;
  begin
    FeatureID := FeatureID or FID_Checked;
    Result := Nodes.AddChildObject(Parent, Caption, Pointer(FeatureID));
    Result.ImageIndex := IcoChecked;
    Result.SelectedIndex := IcoChecked;
  end;

begin
  Installation := Tool.DelphiInstallations.InstallationFromVersion[VersionNumber];
  Result := Assigned(Installation) and Installation.Valid;
  Nodes.BeginUpdate;
  try
    if Result then
    begin
      InstallationNode := AddNode(nil, Installation.Name, 0);
      InstallationNode.StateIndex := 0;
      // JCL
      ProductNode := AddNode(InstallationNode, RsJCL, FID_JCL);
      TempNode := AddNode(ProductNode, RsEnvironment, FID_JCL_Env);
      AddNode(TempNode, RsEnvLibPath, FID_JCL_EnvLibPath);
      if (FJclHlpHelpFileName <> '') or (FJclChmHelpFileName <> '') then
      begin
        TempNode := AddNode(ProductNode, RsHelpFiles, FID_JCL_Help);
        if FJclHlpHelpFileName <> '' then
          AddNode(TempNode, RsIdeHelpHlp, FID_JCL_HelpHlp);
        if FJclChmHelpFileName <> '' then
          AddNode(TempNode, RsIdeHelpChm, FID_JCL_HelpChm);
      end;    
      TempNode := AddNode(ProductNode, RsJCLExceptDlg, FID_JCL_ExcDialog);
      AddNode(TempNode, RsJCLDialogVCL, FID_JCL_ExcDialogVCL);
      if (Installation.VersionNumber >= 6) and (Installation.Edition <> deSTD) then
        AddNode(TempNode, RsJCLDialogCLX, FID_JCL_ExcDialogCLX);
      TempNode := AddNode(ProductNode, RsIdeExperts, FID_JCL_Experts);
      AddNode(TempNode, RsJCLIdeDebug, FID_JCL_ExpertDebug);
      AddNode(TempNode, RsJCLIdeAnalyzer, FID_JCL_ExpertAnalyzer);
      AddNode(TempNode, RsJCLIdeFavorite, FID_JCL_ExpertFavorite);
      AddNode(TempNode, RsJCLIdeThrNames, FID_JCL_ExpertsThrNames);
      InstallationNode.Expand(True);
    end;
  finally
    Nodes.EndUpdate;
  end;
end;

procedure TJclInstall.SelectedNodeChanged(Node: TTreeNode);
begin
end;

function TJclInstall.SelectedNodeCollapsing(Node: TTreeNode): Boolean;
begin
  Result := False;
end;

procedure TJclInstall.SetTool(const Value: IJediInstallTool);
begin
  FTool := Value;
end;

end.
