{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) extension                                                        }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclInstall.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) of Petr Vones. All Rights Reserved.                                                }
{                                                                                                  }
{ Contributor(s): Robert Rossmair (crossplatform support)                                          }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: February 13, 2004                                                                 }
{                                                                                                  }
{**************************************************************************************************}

{$IFNDEF Develop}unit {$IFDEF VisualCLX}QJclInstall{$ELSE}JclInstall{$ENDIF};{$ENDIF}

interface

{$I jcl.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, Classes,
  {$IFDEF VisualCLX}
  Types,
  QComCtrls, QDialogs, QJediInstallIntf;
  {$ELSE}
  ComCtrls, Dialogs, JediInstallIntf;
  {$ENDIF}

type
  TJclInstall = class (TInterfacedObject, IJediInstall)
  private
    FJclPath: string;
    FJclUnitOutputDir: string;
    FJclLibraryPath: string;
    FJclDebugDCUPath: string;
    FJclSourcePath: string;
    FClxDialogFileName: string;
    FClxDialogIconFileName: string;
    {$IFDEF MSWINDOWS}
    FVclDialogFileName: string;
    FVclDialogSendFileName: string;
    FVclDialogIconFileName: string;
    FVclDialogSendIconFileName: string;
    {$ENDIF MSWINDOWS}
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
  {$IFDEF KYLIX}
  VersionDir = '/k%d';
  VersionDirExp = '/k%%d';
  {$ELSE}
  VersionDir = '\d%d';
  VersionDirExp = '\d%%d';
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  {$IFNDEF COMPILER6_UP}
  PathSep = ';';
  {$ENDIF COMPILER6_UP}
  VclDialogFileName = 'ExceptDlg.pas';
  VclDlgSndFileName = 'ExceptDlgMail.pas';
  VclDialogName     = 'Exception Dialog';
  VclDialogNameSend = 'Exception Dialog with Send';
  
  JclVclDpk         = 'packages\d%d\DJclVcl.dpk';
  JclD5RuntimeDpk   = 'packages\d5\DJcl50.dpk';
  JclIdeDebugDpk    = 'examples\vcl\debugextension\JclDebugIde%d0.dpk';
  JclIdeAnalyzerDpk = 'examples\vcl\projectanalyzer\ProjectAnalyzer%d0.dpk';
  JclIdeFavoriteDpk = 'examples\vcl\idefavopendialogs\IdeOpenDlgFavorite%d0.dpk';
  JclIdeThrNamesDpk = 'examples\vcl\debugextension\threadnames\ThreadNameExpert%d0.dpk';
  {$ENDIF MSWINDOWS}

  DialogsPath       = 'examples' + PathSeparator + 'vcl' + PathSeparator + 'debugextension'
                      + PathSeparator + 'dialog' + PathSeparator;
  ClxDialogFileName = 'ClxExceptDlg.pas';
  ClxDialogName     = 'CLX Exception Dialog';
  DialogDescription = 'JCL Application exception dialog';
  DialogAuthor      = 'Project JEDI';
  DialogPage        = 'Dialogs';

  JclChmHelpFile    = 'help' + PathSeparator + 'JCLHelp.chm';
  JclHlpHelpFile    = 'help' + PathSeparator + 'JCLHelp.hlp';
  JclHelpTitle      = 'JCL %d.%d Help';
  JclHelpIndexName  = 'Jedi Code Library Reference';
  HHFileName        = 'HH.EXE';

  JclRuntimeDpk     = 'packages' + VersionDir + PathSeparator + 'DJcl.dpk';
  JclVClxDpk        = 'packages' + VersionDir + PathSeparator + 'DJclVClx.dpk';

  // Feature IDs
  FID_JCL                  = $02000000;

  FID_JCL_Env              = FID_JCL + $00010000;
  FID_JCL_EnvLibPath       = FID_JCL + $00010100;
  FID_JCL_EnvBrowsingPath  = FID_JCL + $00010200;
  FID_JCL_EnvDebugDCUPath  = FID_JCL + $00010300;
  FID_JCL_Make             = FID_JCL + $00020000;
  FID_JCL_MakeRelease      = FID_JCL + $00020100;
  FID_JCL_MakeDebug        = FID_JCL + $00020200;
  FID_JCL_Windows          = $00000001;
  FID_JCL_Vcl              = $00000002;
  FID_JCL_VClx             = $00000003;
  FID_JCL_Help             = FID_JCL + $00030000;
  FID_JCL_HelpHlp          = FID_JCL + $00030100;
  FID_JCL_HelpChm          = FID_JCL + $00030200;
  FID_JCL_Packages         = FID_JCL + $00040000;
  FID_JCL_Experts          = FID_JCL + $00050000;
  FID_JCL_ExpertDebug      = FID_JCL + $00050100;
  FID_JCL_ExpertAnalyzer   = FID_JCL + $00050200;
  FID_JCL_ExpertFavorite   = FID_JCL + $00050300;
  FID_JCL_ExpertsThrNames  = FID_JCL + $00050400;
  FID_JCL_ExcDialog        = FID_JCL + $00060000;
  FID_JCL_ExcDialogVCL     = FID_JCL + $00060100;
  FID_JCL_ExcDialogVCLSnd  = FID_JCL + $00060200;
  FID_JCL_ExcDialogCLX     = FID_JCL + $00060300;

  // Products
  RsJCL             = 'JEDI Code Library';

  // Common features
  RsEnvironment     = 'Environment';
  RsEnvLibPath      = 'Add JCL to IDE Library Path';
  RsEnvBrowsingPath = 'Add JCL to IDE Browsing Path';
  RsEnvDebugDCUPath = 'Add JCL to Debug DCU Path';
  RsMake            = 'Make library units';
  RsMakeRelease     = 'Release';
  RsMakeDebug       = 'Debug';
  RsMakeWindows     = 'Windows';
  RsMakeVcl         = 'VCL';
  RsMakeVClx        = 'Visual CLX';

  RsHelpFiles       = 'Help files';
  RsIdeExperts      = 'IDE experts';
  RsJCLPackages     = 'Runtime packages';
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

resourcestring
  RsSourceLibHint   = 'Adds "%s" to the Library Path';
  RsStatusMessage   = 'Installing %s ...';
  RsInstallFailed   = 'Installation of %s failed, see JediInstaller.log for details.';
  {$IFDEF KYLIX}
  RsLibDescriptor   = '%s library %sunits for Kylix %d for Delphi';
  {$ELSE}
  RsLibDescriptor   = '%s library %sunits for Delphi %d';
  {$ENDIF}
  {$IFDEF VisualCLX}
  RsReadmeFileName  = 'Readme.html';
  {$ELSE}
  RsReadmeFileName  = 'Readme.txt';
  {$ENDIF}

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
  FJclUnitOutputDir := Format('%slib' + VersionDirExp, [FJclPath]);
  FJclLibraryPath := Format('%slib' + VersionDirExp + PathSep + '%0:ssource', [FJclPath]);
  FJclDebugDCUPath := Format('%slib' + VersionDirExp + PathSeparator + 'debug', [FJclPath]);
  FJclSourcePath := Format('%0:scommon' + PathSep + '%0:swindows' + PathSep + '%0:svcl' + PathSep
    + '%0:svisclx', [FJclPath + 'source' + PathSeparator]);
  {$IFDEF MSWINDOWS}
  FClxDialogFileName := AnsiUpperCase(FJclPath + DialogsPath + ClxDialogFileName);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  FClxDialogFileName := FJclPath + DialogsPath + ClxDialogFileName;
  {$ENDIF UNIX}
  FClxDialogIconFileName := ChangeFileExt(FClxDialogFileName, '.ico');
  {$IFDEF MSWINDOWS}
  FVclDialogFileName := AnsiUpperCase(FJclPath + DialogsPath + VclDialogFileName);
  FVclDialogSendFileName := AnsiUpperCase(FJclPath + DialogsPath + VclDlgSndFileName);
  FVclDialogIconFileName := ChangeFileExt(FVclDialogFileName, '.ico');
  FVclDialogSendIconFileName := ChangeFileExt(FVclDialogSendFileName, '.ico');
  {$ENDIF MSWINDOWS}
  FJclChmHelpFileName := FJclPath + JclChmHelpFile;
  FJclHlpHelpFileName := FJclPath + JclHlpHelpFile;
  if not FileExists(FJclChmHelpFileName) then
    FJclChmHelpFileName := '';
  if not FileExists(FJclHlpHelpFileName) then
    FJclHlpHelpFileName := '';
  {$IFDEF MSWINDOWS}
  // Reset ReadOnly flag for dialog forms
  FileSetAttr(FClxDialogFileName, faArchive);
  FileSetAttr(ChangeFileExt(FClxDialogFileName, '.xfm'), faArchive);
  FileSetAttr(FVclDialogFileName, faArchive);
  FileSetAttr(ChangeFileExt(FVclDialogFileName, '.dfm'), faArchive);
  FileSetAttr(FVclDialogSendFileName, faArchive);
  FileSetAttr(ChangeFileExt(FVclDialogSendFileName, '.dfm'), faArchive);
  {$ENDIF MSWINDOWS}
  Result := FileExists(FClxDialogFileName) and FileExists(FClxDialogIconFileName)
  {$IFDEF MSWINDOWS}
    and FileExists(FVclDialogFileName)  and FileExists(FVclDialogIconFileName)
  {$ENDIF MSWINDOWS};
  FJclReadmeFileName := PathAddSeparator(FJclPath) + RsReadmeFileName;
  if FileExists(FJclReadmeFileName) then
  begin
    ReadmeText := FileToString(FJclReadmeFileName);
    Tool.UpdateInfo(3, ReadmeText);
    Tool.UpdateInfo(5, ReadmeText);
    Tool.UpdateInfo(6, ReadmeText);
    Tool.UpdateInfo(7, ReadmeText);
  end;
end;

function TJclInstall.Install: Boolean;
var
  Installation: TJclDelphiInstallation;

  procedure AddHelpToDelphiHelp;
  begin
    {$IFDEF MSWINDOWS}
    Installation.OpenHelp.AddHelpFile(FJclHlpHelpFileName, JclHelpIndexName);
    {$ENDIF MSWINDOWS}
  end;

  procedure AddHelpToIdeTools;
  var
    ToolsIndex: Integer;
    HelpTitle: string;
  begin
    HelpTitle := Format(JclHelpTitle, [JclVersionMajor, JclVersionMinor]);
    if Installation.IdeTools.IndexOfTitle(HelpTitle) = -1 then
    begin
      ToolsIndex := Installation.IdeTools.Count;
      Installation.IdeTools.Count := ToolsIndex + 1;
      Installation.IdeTools.Title[ToolsIndex] := HelpTitle;
      Installation.IdeTools.Path[ToolsIndex] := HHFileName;
      Installation.IdeTools.Parameters[ToolsIndex] := StrDoubleQuote(FJclChmHelpFileName);
      Installation.IdeTools.WorkingDir[ToolsIndex] := FJclPath;
    end;
  end;

  function CompileLibraryUnits(const SubDir: string; Debug: Boolean): Boolean;
  var
    I: Integer;
    {$IFDEF KYLIX}
    J: Integer;
    {$ENDIF}
    Units: TStringList;
    UnitType: string;
    LibDescriptor: string;
    SaveDir, LibSubDir: string;
    Success: Boolean;
  begin
    Result := True;
    if Debug then
      UnitType := 'debug ';
    LibDescriptor := Format(RsLibDescriptor, [SubDir, UnitType, Installation.VersionNumber]);
    Tool.WriteInstallLog(Format('Making %s', [LibDescriptor]));
    Units := TStringList.Create;
    try
      Tool.UpdateStatus(Format('Compiling %s ...', [LibDescriptor]));
      BuildFileList(Format('%ssource' + PathSeparator + '%s' + PathSeparator + '*.pas',
        [FJclPath, SubDir]), faAnyFile, Units);
      with Installation.Compiler do
      begin
        Options.Clear;
        Options.Add('-M');
        if Debug then
        begin
          Options.Add('-$O-');
          Options.Add('-$R+');
          Options.Add('-$Q+');
          Options.Add('-$D+');
          Options.Add('-$L+');
          Options.Add('-$Y+');
          LibSubDir := PathSeparator + 'debug';
        end
        else
        begin
          Options.Add('-$O+');
          Options.Add('-$R-');
          Options.Add('-$Q-');
          Options.Add('-$C-');
          Options.Add('-$D-');
          LibSubDir := '';
        end;
        AddPathOption('N', Format(FJclUnitOutputDir + '%s', [Installation.VersionNumber, LibSubDir]));
        AddPathOption('I', FJclPath + 'source');
        AddPathOption('R', FJclSourcePath);
        AddPathOption('U', FJclSourcePath);
        SaveDir := GetCurrentDir;
        Success := SetCurrentDir(Format('%ssource' + PathSeparator + '%s', [FJclPath, SubDir]));
        {$IFDEF WIN32}
        Win32Check(Success);
        {$ENDIF WIN32}
        {$IFDEF UNIX}
        if Success then
        {$ENDIF}
        try
          for I := 0 to Units.Count - 1 do
          begin
            Compile(Units[I]);
            Tool.WriteInstallLog(Installation.Compiler.DCCOutput);
            {$IFDEF KYLIX}
            J := Options.Add('-P');   // generate position independent code (PIC)
            Compile(Units[I]);
            Options.Delete(J);        // remove PIC option
            Tool.WriteInstallLog(Installation.Compiler.DCCOutput);
            {$ENDIF KYLIX}
          end;
        finally
          SetCurrentDir(SaveDir);
        end;
      end;
    finally
      Units.Free;
    end;
    Tool.WriteInstallLog('');
    Tool.UpdateStatus('');
    {if not Result then
      Tool.MessageBox(Format(RsInstallFailed, [LibDescriptor]), MB_OK or MB_ICONERROR);}
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
    Tool.WriteInstallLog(Installation.Compiler.DCCOutput);
    Tool.WriteInstallLog('');
    Tool.UpdateStatus('');
    if not Result then
      Tool.MessageBox(Format(RsInstallFailed, [PackageFileName]), mtError);
  end;

  procedure CleanupRepository;
  begin
    if Tool.FeatureChecked(FID_JCL, Installation.VersionNumber) then
    begin
      Installation.Repository.RemoveObjects(DialogsPath, ClxDialogFileName, DelphiRepositoryFormTemplate);
      {$IFDEF MSWINDOWS}
      Installation.Repository.RemoveObjects(DialogsPath, VclDialogFileName, DelphiRepositoryFormTemplate);
      Installation.Repository.RemoveObjects(DialogsPath, VclDlgSndFileName, DelphiRepositoryFormTemplate);
      {$ENDIF MSWINDOWS}
    end;
  end;

  procedure WriteDelphiVersionToLog;
  begin
    Tool.WriteInstallLog(StrPadRight(Format('%s Build %s ', [Installation.Name, Installation.IdeExeBuildNumber]), 120, '='));
  end;

  procedure MakeUnits(Debug: Boolean);
  begin
    CompileLibraryUnits('common', Debug);
    {$IFDEF MSWINDOWS}
    if (Installation.VersionNumber < 6)
    or Tool.FeatureChecked(FID_JCL_MakeRelease + FID_JCL_Windows, Installation.VersionNumber) then
      CompileLibraryUnits('windows', Debug);
    if (Installation.VersionNumber < 6)
    or Tool.FeatureChecked(FID_JCL_MakeRelease + FID_JCL_Vcl, Installation.VersionNumber) then
      CompileLibraryUnits('vcl', Debug);
    {$ENDIF MSWINDOWS}
    if Tool.FeatureChecked(FID_JCL_MakeRelease + FID_JCL_VClx, Installation.VersionNumber) then
      CompileLibraryUnits('visclx', Debug);
  end;

  procedure DxInstall;
  begin
    Tool.UpdateStatus(Format(RsStatusMessage, [Installation.Name]));
    WriteDelphiVersionToLog;
    CleanupRepository;
    if Tool.FeatureChecked(FID_JCL_EnvLibPath, Installation.VersionNumber) then
      Installation.AddToLibrarySearchPath(Format(FJclLibraryPath, [Installation.VersionNumber]));
    if Tool.FeatureChecked(FID_JCL_EnvDebugDCUPath, Installation.VersionNumber) then
      Installation.AddToDebugDCUPath(Format(FJclDebugDCUPath, [Installation.VersionNumber]));
    if Tool.FeatureChecked(FID_JCL_EnvBrowsingPath, Installation.VersionNumber) then
      Installation.AddToLibraryBrowsingPath(FJclSourcePath);
    if Tool.FeatureChecked(FID_JCL_Make, Installation.VersionNumber) then
    begin
      MakeUnits(False);
      if Tool.FeatureChecked(FID_JCL_MakeDebug, Installation.VersionNumber) then
        MakeUnits(True);
    end;
    if Tool.FeatureChecked(FID_JCL_HelpHlp, Installation.VersionNumber) then
      AddHelpToDelphiHelp;
    if Tool.FeatureChecked(FID_JCL_HelpChm, Installation.VersionNumber) then
      AddHelpToIdeTools;
    {$IFDEF VCL}
    if Tool.FeatureChecked(FID_JCL_ExcDialogVCL, Installation.VersionNumber) then
      Installation.Repository.AddObject(FVclDialogFileName, DelphiRepositoryFormTemplate,
        Installation.Repository.FindPage(DialogPage, 1), VclDialogName, FVclDialogIconFileName,
        DialogDescription, DialogAuthor, DelphiRepositoryDesignerDfm);
    if Tool.FeatureChecked(FID_JCL_ExcDialogVCLSnd, Installation.VersionNumber) then
      Installation.Repository.AddObject(FVclDialogSendFileName, DelphiRepositoryFormTemplate,
        Installation.Repository.FindPage(DialogPage, 1), VclDialogNameSend, FVclDialogSendIconFileName,
        DialogDescription, DialogAuthor, DelphiRepositoryDesignerDfm, FVclDialogFileName);
    {$ENDIF VCL}
  end;

  {$IFDEF MSWINDOWS}
  procedure D5Install;
  begin
    DxInstall;
    if Tool.FeatureChecked(FID_JCL_Experts, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclD5RuntimeDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertDebug, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeDebugDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertAnalyzer, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeAnalyzerDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertFavorite, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeFavoriteDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertsThrNames, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeThrNamesDpk);
  end;

  procedure D6Install;
  begin
    DxInstall;
    if Tool.FeatureChecked(FID_JCL_ExcDialogCLX, Installation.VersionNumber) then
      Installation.Repository.AddObject(FClxDialogFileName, DelphiRepositoryFormTemplate,
        Installation.Repository.FindPage(DialogPage, 1), ClxDialogName, FClxDialogIconFileName,
        DialogDescription, DialogAuthor, DelphiRepositoryDesignerXfm);
    if Tool.FeatureChecked(FID_JCL_Experts, Installation.VersionNumber) then
    begin
      InstallPackage(FJclPath, JclRuntimeDpk);
      InstallPackage(FJclPath, JclVclDpk);
      InstallPackage(FJclPath, JclVClxDpk);
    end;
    if Tool.FeatureChecked(FID_JCL_ExpertDebug, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeDebugDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertAnalyzer, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeAnalyzerDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertFavorite, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeFavoriteDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertsThrNames, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeThrNamesDpk);
  end;

  procedure D7Install;
  begin
    DxInstall;
    if Tool.FeatureChecked(FID_JCL_ExcDialogCLX, Installation.VersionNumber) then
      Installation.Repository.AddObject(FClxDialogFileName, DelphiRepositoryFormTemplate,
        Installation.Repository.FindPage(DialogPage, 1), ClxDialogName, FClxDialogIconFileName,
        DialogDescription, DialogAuthor, DelphiRepositoryDesignerXfm);
    if Tool.FeatureChecked(FID_JCL_Experts, Installation.VersionNumber) then
    begin
      InstallPackage(FJclPath, JclRuntimeDpk);
      InstallPackage(FJclPath, JclVclDpk);
      InstallPackage(FJclPath, JclVClxDpk);
    end;
    if Tool.FeatureChecked(FID_JCL_ExpertDebug, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeDebugDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertAnalyzer, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeAnalyzerDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertFavorite, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeFavoriteDpk);
  end;
  {$ENDIF MSWINDOWS}

  procedure K3Install;
  begin
    DxInstall;
    if Tool.FeatureChecked(FID_JCL_ExcDialogCLX, Installation.VersionNumber) then
      Installation.Repository.AddObject(FClxDialogFileName, DelphiRepositoryFormTemplate,
        Installation.Repository.FindPage(DialogPage, 1), ClxDialogName, FClxDialogIconFileName,
        DialogDescription, DialogAuthor, DelphiRepositoryDesignerXfm);
    if Tool.FeatureChecked(FID_JCL_Packages, Installation.VersionNumber) then
    begin
      InstallPackage(FJclPath, JclRuntimeDpk);
      InstallPackage(FJclPath, JclVClxDpk);
    end;
  end;

begin
  Result := True;
  Tool.WriteInstallLog(Format('Installation started %s', [DateTimeToStr(Now)]));
  try
    {$IFDEF MSWINDOWS}
    Installation := Tool.DelphiInstallations.InstallationFromVersion[5];
    if Assigned(Installation) and Installation.Valid then
      D5Install;
    Installation := Tool.DelphiInstallations.InstallationFromVersion[6];
    if Assigned(Installation) and Installation.Valid then
      D6Install;
    Installation := Tool.DelphiInstallations.InstallationFromVersion[7];
    if Assigned(Installation) and Installation.Valid then
      D7Install;
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    Installation := Tool.DelphiInstallations.InstallationFromVersion[3];
    if Assigned(Installation) and Installation.Valid then
      K3Install;
    {$ENDIF UNIX}
  finally
    Tool.UpdateStatus('');
  end;
end;

function TJclInstall.PopulateTreeView(Nodes: TTreeNodes; VersionNumber: Integer; Page: TTabSheet): Boolean;
var
  InstallationNode, ProductNode, TempNode, MakeNode: TTreeNode;
  Installation: TJclDelphiInstallation;

  function AddNode(Parent: TTreeNode; const Caption: string; FeatureID: Cardinal): TTreeNode;
  begin
    FeatureID := FeatureID or FID_Checked;
    Result := Nodes.AddChildObject(Parent, Caption, Pointer(FeatureID));
    Result.ImageIndex := IcoChecked;
    Result.SelectedIndex := IcoChecked;
  end;

  procedure AddMakeNodes(Parent: TTreeNode; DebugSettings: Boolean);
  const
    Caption: array[Boolean] of string = (RsMakeRelease, RsMakeDebug);
    Feature: array[Boolean] of Cardinal = (FID_JCL_MakeRelease, FID_JCL_MakeDebug);
  var
    Node: TTreeNode;
  begin
    Node := AddNode(Parent, Caption[DebugSettings], Feature[DebugSettings]);
    {$IFDEF KYLIX}
    AddNode(Node, RsMakeVClx, Feature[DebugSettings] or FID_JCL_VClx);
    {$ELSE}
    if Installation.VersionNumber >= 6 then
    begin
      AddNode(Node, RsMakeWindows, Feature[DebugSettings] or FID_JCL_Windows);
      AddNode(Node, RsMakeVcl, Feature[DebugSettings] or FID_JCL_Vcl);
      if Installation.SupportsVisualCLX then
        AddNode(Node, RsMakeVClx, Feature[DebugSettings] or FID_JCL_VClx);
    end;
    {$ENDIF}
  end;

begin
  Installation := Tool.DelphiInstallations.InstallationFromVersion[VersionNumber];
  Result := Assigned(Installation) and Installation.Valid;
  Nodes.BeginUpdate;
  try
    if Result then
    begin
      InstallationNode := AddNode(nil, Installation.Name, 0);
      //InstallationNode.StateIndex := 0;
      // JCL
      ProductNode := AddNode(InstallationNode, RsJCL, FID_JCL);
      TempNode := AddNode(ProductNode, RsEnvironment, FID_JCL_Env);
      AddNode(TempNode, RsEnvLibPath, FID_JCL_EnvLibPath);
      AddNode(TempNode, RsEnvBrowsingPath, FID_JCL_EnvBrowsingPath);
      AddNode(TempNode, RsEnvDebugDCUPath, FID_JCL_EnvDebugDCUPath);

      MakeNode := AddNode(ProductNode, RsMake, FID_JCL_Make);
      AddMakeNodes(MakeNode, False);
      AddMakeNodes(MakeNode, True);

      {$IFNDEF KYLIX}
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
      if Installation.SupportsVisualCLX then
        AddNode(TempNode, RsJCLDialogCLX, FID_JCL_ExcDialogCLX);
      {$ENDIF}
      AddNode(ProductNode, RsJCLPackages, FID_JCL_Packages);
      {$IFDEF MSWINDOWS}
      TempNode := AddNode(ProductNode, RsIdeExperts, FID_JCL_Experts);
      AddNode(TempNode, RsJCLIdeDebug, FID_JCL_ExpertDebug);
      AddNode(TempNode, RsJCLIdeAnalyzer, FID_JCL_ExpertAnalyzer);
      AddNode(TempNode, RsJCLIdeFavorite, FID_JCL_ExpertFavorite);
      if Installation.VersionNumber <= 6 then
        AddNode(TempNode, RsJCLIdeThrNames, FID_JCL_ExpertsThrNames);
      {$ENDIF MSWINDOWS}
      InstallationNode.Expand(True);
      MakeNode.Collapse(True);
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
