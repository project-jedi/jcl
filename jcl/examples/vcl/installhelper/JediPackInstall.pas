unit JediPackInstall;

interface

{$I jcl.inc}

uses
  Windows, SysUtils, Classes, ComCtrls, JediInstallIntf;

type
  TJediPackInstall = class (TInterfacedObject, IJediInstall)
  private
    FApiPath: string;
    FApiSourcePath: string;
    FJclPath: string;
    FJclSourcePath: string;
    FClxDialogFileName: string;
    FVclDialogFileName: string;
    FVclDialogSendFileName: string;
    FClxDialogIconFileName: string;
    FVclDialogIconFileName: string;
    FVclDialogSendIconFileName: string;
    FJclChmHelpFileName: string;
    FJclHlpHelpFileName: string;
    FJclReadmeFileName: string;
    FVclPath: string;
    FVclSourcePath: string;
    FVclCommonPath: string;
    FVclReadmeFileName: string;
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
  // JCL
  DialogsPath       = 'examples\debugextension\dialog\';
  ClxDialogFileName = 'ClxExceptDlg.pas';
  VclDialogFileName = 'ExceptDlg.pas';
  VclDlgSndFileName = 'ExceptDlgMail.pas';
  ClxDialogName     = 'CLX Exception Dialog';
  VclDialogName     = 'Exception Dialog';
  VclDialogNameSend = 'Exception Dialog with Send';
  DialogDescription = 'JCL Application exception dialog';
  DialogAuthor      = 'Project JEDI';
  DialogPage        = 'Dialogs';

  JclChmHelpFile    = 'Help\JCLHelp.chm';
  JclHlpHelpFile    = 'Help\JCLHelp.hlp';
  JclHelpTitle      = 'JCL 1.22 Help';
  JclHelpIndexName  = 'Jedi Code Library Reference';
  HHFileName        = 'HH.EXE';

  JclRuntimeDpk     = 'packages\DJCL%d0.dpk';
  JclIdeDebugDpk    = 'examples\debugextension\JclDebugIde%d0.dpk';
  JclIdeAnalyzerDpk = 'examples\projectanalyzer\ProjectAnalyzer%d0.dpk';
  JclIdeFavoriteDpk = 'examples\idefavopendialogs\IdeOpenDlgFavorite%d0.dpk';
  JclIdeThrNamesDpk = 'examples\debugextension\threadnames\ThreadNameExpert%d0.dpk';

  // JVCL
  VclRuntimeDpk     = 'packages\JVCL200_R%d0.dpk';
  VclRuntimeDpkPers = 'packages\JVCL200_R%d0Personal.dpk';
  VclDesignDpk      = 'packages\JVCL200_D%d0.dpk';
  VclDesignDpkPers  = 'packages\JVCL200_D%d0Personal.dpk';


  // Feature IDs
  FID_API                  = $01000000;
  FID_JCL                  = $02000000;
  FID_VCL                  = $03000000;

  FID_API_Env              = FID_API + $00010000;
  FID_API_EnvLibPath       = FID_API + $00010001;

  FID_JCL_Env              = FID_JCL + $00010000;
  FID_JCL_EnvLibPath       = FID_JCL + $00010100;
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
  FID_JCL_ExcDialogVCLSnd  = FID_JCL + $00040200;
  FID_JCL_ExcDialogCLX     = FID_JCL + $00040300;

  FID_VCL_Env              = FID_VCL + $00010000;
  FID_VCL_EnvLibPath       = FID_VCL + $00010001;
  FID_VCL_EnvDesignPkg     = FID_VCL + $00010002;
  FID_VCL_Help             = FID_VCL + $00020000;
  FID_VCL_HelpHlp          = FID_VCL + $00020100;

  // Products
  RsAPI             = 'JEDI API Library';
  RsJCL             = 'JEDI Code Library';
  RsJVCL            = 'JEDI Visual Component Library';

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
  RsJCLDialogVCLSnd = 'VCL Exception Dialog with Send button';
  RsJCLDialogCLX    = 'CLX Exception Dialog';
  RsJCLIdeDebug     = 'Debug Extension';
  RsJCLIdeAnalyzer  = 'Project Analyzer';
  RsJCLIdeFavorite  = 'Favorite combobox in Open/Save dialogs';
  RsJCLIdeThrNames  = 'Displaying thread names in Thread Status window';
  RsVCLDesignPkg    = 'Install design-time component packages';

resourcestring
  RsSourceLibHint   = 'Adds "%s" to the Library Path';
  RsStatusMessage   = 'Compiling/Installing %s ...';
  RsInstallFailed   = 'Installation of %s failed, see JediInstaller.log for details.';

function CreateJediInstall: IJediInstall;
begin
  Result := TJediPackInstall.Create as IJediInstall;
end;

{ TJediPackInstall }

function TJediPackInstall.InitInformation(const ApplicationFileName: string): Boolean;
begin
  // API
  FApiPath := PathAddSeparator(PathCanonicalize(PathExtractFileDirFixed(ApplicationFileName) + '..\..\API'));
  FApiSourcePath := FApiPath + 'Pas';
  // JCL
  FJclPath := PathAddSeparator(PathCanonicalize(PathExtractFileDirFixed(ApplicationFileName) + '..'));
  FJclSourcePath := FJclPath + 'Source';
  FClxDialogFileName := AnsiUpperCase(FJclPath + DialogsPath + ClxDialogFileName);
  FVclDialogFileName := AnsiUpperCase(FJclPath + DialogsPath + VclDialogFileName);
  FVclDialogSendFileName := AnsiUpperCase(FJclPath + DialogsPath + VclDlgSndFileName);
  FClxDialogIconFileName := ChangeFileExt(FClxDialogFileName, '.ICO');
  FVclDialogIconFileName := ChangeFileExt(FVclDialogFileName, '.ICO');
  FVclDialogSendIconFileName := ChangeFileExt(FVclDialogSendFileName, '.ICO');
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
  FileSetAttr(FVclDialogSendFileName, faArchive);
  FileSetAttr(ChangeFileExt(FVclDialogSendFileName, '.dfm'), faArchive);
  Result := (FileExists(FClxDialogFileName) and FileExists(FVclDialogFileName) and
    FileExists(FClxDialogIconFileName) and FileExists(FVclDialogIconFileName));
  FJclReadmeFileName := PathAddSeparator(FJclPath) + 'Readme.txt';
  // VCL
  FVclPath := PathAddSeparator(PathCanonicalize(PathExtractFileDirFixed(ApplicationFileName) + '..\..\JVCL'));
  FVclSourcePath := FVclPath + 'Source';
  FVclCommonPath := FVclPath + 'Common';
  FVclReadmeFileName := FVclPath + 'Readme.txt';
end;

function TJediPackInstall.Install: Boolean;
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

  function InstallPackage(const Path, Name: string): Boolean;
  var
    PackageFileName: string;
  begin
    PackageFileName := Path + Format(Name, [Installation.VersionNumber]);
    Tool.WriteInstallLog(Format('Installing package %s', [PackageFileName]));
    Tool.UpdateStatus(Format(RsStatusMessage, [ExtractFileName(PackageFileName)]));
    Result := Installation.Compiler.InstallPackage(PackageFileName, Tool.BPLPath(Installation.VersionNumber),
      Tool.DCPPath(Installation.VersionNumber));
    Tool.WriteInstallLog(Installation.Compiler.DCC32Output);
    Tool.WriteInstallLog('');
    Tool.UpdateStatus('');
    if not Result then
      Tool.MessageBox(Format(RsInstallFailed, [PackageFileName]), MB_OK or MB_ICONERROR);
  end;

  procedure CleanupRepository;
  begin
    if Tool.FeatureChecked(FID_JCL, Installation.VersionNumber) then
    begin
      Installation.Repository.RemoveObjects(DialogsPath, VclDialogFileName, DelphiRepositoryFormTemplate);
      Installation.Repository.RemoveObjects(DialogsPath, ClxDialogFileName, DelphiRepositoryFormTemplate);
      Installation.Repository.RemoveObjects(DialogsPath, VclDlgSndFileName, DelphiRepositoryFormTemplate);
    end;
  end;

  procedure WriteDelphiVersionToLog;
  begin
    Tool.WriteInstallLog(StrPadRight(Format('%s Build %s ', [Installation.Name, Installation.IdeExeBuildNumber]), 120, '='));
  end;

  procedure D5Install;
  begin
    Tool.UpdateStatus(Format(RsStatusMessage, [Installation.Name]));
    WriteDelphiVersionToLog;
    // JCL
    CleanupRepository;
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
    if Tool.FeatureChecked(FID_JCL_ExcDialogVCLSnd, Installation.VersionNumber) then
      Installation.Repository.AddObject(FVclDialogSendFileName, DelphiRepositoryFormTemplate,
        Installation.Repository.FindPage(DialogPage, 1), VclDialogNameSend, FVclDialogSendIconFileName,
        DialogDescription, DialogAuthor, DelphiRepositoryDesignerDfm, FVclDialogFileName);
    if Tool.FeatureChecked(FID_JCL_Experts, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclRuntimeDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertDebug, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeDebugDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertAnalyzer, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeAnalyzerDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertFavorite, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeFavoriteDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertsThrNames, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeThrNamesDpk);
    // JVCL
    if Tool.FeatureChecked(FID_VCL_EnvLibPath, Installation.VersionNumber) then
    begin
      Installation.AddToLibrarySearchPath(FVclSourcePath);
      Installation.AddToLibrarySearchPath(FVclCommonPath);
    end;
    if Tool.FeatureChecked(FID_VCL_EnvDesignPkg, Installation.VersionNumber) then
    begin
      InstallPackage(FVclPath, VclRuntimeDpk);
      InstallPackage(FVclPath, VclDesignDpk);
    end;
  end;

  procedure D6Install;
  begin
    Tool.UpdateStatus(Format(RsStatusMessage, [Installation.Name]));
    WriteDelphiVersionToLog;
    // JCL
    CleanupRepository;
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
    if Tool.FeatureChecked(FID_JCL_ExcDialogVCLSnd, Installation.VersionNumber) then
      Installation.Repository.AddObject(FVclDialogSendFileName, DelphiRepositoryFormTemplate,
        Installation.Repository.FindPage(DialogPage, 1), VclDialogNameSend, FVclDialogSendIconFileName,
        DialogDescription, DialogAuthor, DelphiRepositoryDesignerDfm, FVclDialogFileName);
    if Tool.FeatureChecked(FID_JCL_ExcDialogCLX, Installation.VersionNumber) then
      Installation.Repository.AddObject(FClxDialogFileName, DelphiRepositoryFormTemplate,
        Installation.Repository.FindPage(DialogPage, 1), ClxDialogName, FClxDialogIconFileName,
        DialogDescription, DialogAuthor, DelphiRepositoryDesignerXfm);
    if Tool.FeatureChecked(FID_JCL_Experts, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclRuntimeDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertDebug, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeDebugDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertAnalyzer, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeAnalyzerDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertFavorite, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeFavoriteDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertsThrNames, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeThrNamesDpk);
    // JVCL
    if Tool.FeatureChecked(FID_VCL_EnvLibPath, Installation.VersionNumber) then
    begin
      Installation.AddToLibrarySearchPath(FVclSourcePath);
      Installation.AddToLibrarySearchPath(FVclCommonPath);
    end;
    if Tool.FeatureChecked(FID_VCL_EnvDesignPkg, Installation.VersionNumber) then
      case Installation.Edition of
        deSTD:
          begin
            InstallPackage(FVclPath, VclRuntimeDpkPers);
            InstallPackage(FVclPath, VclDesignDpkPers);
          end;
        dePRO, deCSS:
          begin
            InstallPackage(FVclPath, VclRuntimeDpk);
            InstallPackage(FVclPath, VclDesignDpk);
          end;
      end;
  end;

  procedure D7Install;
  begin
    Tool.UpdateStatus(Format(RsStatusMessage, [Installation.Name]));
    // JCL
    WriteDelphiVersionToLog;
    CleanupRepository;
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
    if Tool.FeatureChecked(FID_JCL_ExcDialogVCLSnd, Installation.VersionNumber) then
      Installation.Repository.AddObject(FVclDialogSendFileName, DelphiRepositoryFormTemplate,
        Installation.Repository.FindPage(DialogPage, 1), VclDialogNameSend, FVclDialogSendIconFileName,
        DialogDescription, DialogAuthor, DelphiRepositoryDesignerDfm, FVclDialogFileName);
    if Tool.FeatureChecked(FID_JCL_ExcDialogCLX, Installation.VersionNumber) then
      Installation.Repository.AddObject(FClxDialogFileName, DelphiRepositoryFormTemplate,
        Installation.Repository.FindPage(DialogPage, 1), ClxDialogName, FClxDialogIconFileName,
        DialogDescription, DialogAuthor, DelphiRepositoryDesignerXfm);
    if Tool.FeatureChecked(FID_JCL_Experts, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclRuntimeDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertDebug, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeDebugDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertAnalyzer, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeAnalyzerDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertFavorite, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeFavoriteDpk);
    // JVCL
    if Tool.FeatureChecked(FID_VCL_EnvLibPath, Installation.VersionNumber) then
    begin
      Installation.AddToLibrarySearchPath(FVclSourcePath);
      Installation.AddToLibrarySearchPath(FVclCommonPath);
    end;
    if Tool.FeatureChecked(FID_VCL_EnvDesignPkg, Installation.VersionNumber) then
      case Installation.Edition of
        deSTD:
          begin
            InstallPackage(FVclPath, VclRuntimeDpkPers);
            InstallPackage(FVclPath, VclDesignDpkPers);
          end;
        dePRO, deCSS:
          begin
            InstallPackage(FVclPath, VclRuntimeDpk);
            InstallPackage(FVclPath, VclDesignDpk);
          end;
      end;
  end;

begin
  Result := True;
  try
    Installation := Tool.DelphiInstallations.InstallationFromVersion[5];
    if Assigned(Installation) and Installation.Valid then
      D5Install;
    Installation := Tool.DelphiInstallations.InstallationFromVersion[6];
    if Assigned(Installation) and Installation.Valid then
      D6Install;
    Installation := Tool.DelphiInstallations.InstallationFromVersion[7];
    if Assigned(Installation) and Installation.Valid then
      D7Install;
  finally
    Tool.UpdateStatus('');
  end;
end;

function TJediPackInstall.PopulateTreeView(Nodes: TTreeNodes; VersionNumber: Integer; Page: TTabSheet): Boolean;
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
      if Assigned(Installation) and Installation.Valid then
      begin
        // API Library
        if DirectoryExists(FApiSourcePath) then
        begin
          ProductNode := AddNode(InstallationNode, RsAPI, FID_API);
          TempNode := AddNode(ProductNode, RsEnvironment, FID_API_Env);
          AddNode(TempNode, RsEnvLibPath, FID_API_EnvLibPath);
        end;
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
        AddNode(TempNode, RsJCLDialogVCLSnd, FID_JCL_ExcDialogVCLSnd);
        if (Installation.VersionNumber >= 6) and (Installation.Edition <> deSTD) then
          AddNode(TempNode, RsJCLDialogCLX, FID_JCL_ExcDialogCLX);
        TempNode := AddNode(ProductNode, RsIdeExperts, FID_JCL_Experts);
        AddNode(TempNode, RsJCLIdeDebug, FID_JCL_ExpertDebug);
        AddNode(TempNode, RsJCLIdeAnalyzer, FID_JCL_ExpertAnalyzer);
        AddNode(TempNode, RsJCLIdeFavorite, FID_JCL_ExpertFavorite);
        if Installation.VersionNumber <= 6 then
          AddNode(TempNode, RsJCLIdeThrNames, FID_JCL_ExpertsThrNames);
        InstallationNode.Expand(True);
        // JVCL
        if DirectoryExists(FVclSourcePath) and (Installation.VersionNumber >= 5) then
        begin
          ProductNode := AddNode(InstallationNode, RsJVCL, FID_VCL);
          TempNode := AddNode(ProductNode, RsEnvironment, FID_VCL_Env);
          AddNode(TempNode, RsEnvLibPath, FID_VCL_EnvLibPath);
          AddNode(TempNode, RsVCLDesignPkg, FID_VCL_EnvDesignPkg);
          TempNode := AddNode(ProductNode, RsHelpFiles, FID_VCL_Help);
          AddNode(TempNode, RsIdeHelpHlp, FID_VCL_HelpHlp);
        end;
      end;
      InstallationNode.Expand(True);
    end;
  finally
    Nodes.EndUpdate;
  end;
end;

function TJediPackInstall.SelectedNodeCollapsing(Node: TTreeNode): Boolean;
begin
  Result := False;
end;

procedure TJediPackInstall.SelectedNodeChanged(Node: TTreeNode);
var
  ReadmeText: string;
begin
  case Cardinal(Node.Data) and FID_Product of
    FID_JCL: ReadmeText := FileToString(FJclReadmeFileName);
    FID_VCL: ReadmeText := FileToString(FVclReadmeFileName);
  else
    ReadmeText := '';
  end;
  Tool.UpdateInfo(Tool.ActiveVersionNumberPage, ReadmeText);
end;

procedure TJediPackInstall.SetTool(const Value: IJediInstallTool);
begin
  FTool := Value;
end;

end.
