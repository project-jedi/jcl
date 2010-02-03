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
{ The Original Code is DelphiInstall.pas.                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) of Petr Vones. All Rights Reserved.                                                }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Andreas Hausladen (ahuser)                                                                     }
{   Florent Ouchet (outchy)                                                                        }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair) - crossplatform & BCB support                                      }
{   Uwe Schuster (uschuster)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclCompilerUtils;

{$I jcl.inc}
{$I crossplatform.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Classes, SysUtils, IniFiles,
  JclBase, JclSysUtils;

type
  EJclCompilerUtilsException = class(EJclError);

  TJclCompilerSettingsFormat = (csfDOF, csfBDSProj, csfMsBuild);

  TJclBorlandCommandLineTool = class;
  TJclBorlandCommandLineToolEvent = procedure(Sender:TJclBorlandCommandLineTool) of object;

  TJclBorlandCommandLineTool = class(TInterfacedObject, IJclCommandLineTool)
  private
    FBinDirectory: string;
    FCompilerSettingsFormat: TJclCompilerSettingsFormat;
    FLongPathBug: Boolean;
    FOptions: TStringList;
    FOutputCallback: TTextHandler;
    FOutput: string;
    FOnAfterExecute: TJclBorlandCommandLineToolEvent;
    FOnBeforeExecute: TJclBorlandCommandLineToolEvent;
    procedure OemTextHandler(const Text: string);
  protected
    procedure CheckOutputValid;
    function GetFileName: string;
    function InternalExecute(const CommandLine: string): Boolean;
  public
    constructor Create(const ABinDirectory: string; ALongPathBug: Boolean;
      ACompilerSettingsFormat: TJclCompilerSettingsFormat);
    destructor Destroy; override;
    { IJclCommandLineTool }
    function GetExeName: string; virtual;
    function GetOptions: TStrings;
    function GetOutput: string;
    function GetOutputCallback: TTextHandler;
    procedure AddPathOption(const Option, Path: string);
    function Execute(const CommandLine: string): Boolean; virtual;
    procedure SetOutputCallback(const CallbackMethod: TTextHandler);
    property BinDirectory: string read FBinDirectory;
    property CompilerSettingsFormat: TJclCompilerSettingsFormat read FCompilerSettingsFormat;
    property ExeName: string read GetExeName;
    property LongPathBug: Boolean read FLongPathBug;
    property Options: TStrings read GetOptions;
    property OutputCallback: TTextHandler write SetOutputCallback;
    property Output: string read GetOutput;

    property FileName: string read GetFileName;
    property OnAfterExecute: TJclBorlandCommandLineToolEvent read FOnAfterExecute write FOnAfterExecute;
    property OnBeforeExecute: TJclBorlandCommandLineToolEvent read FOnBeforeExecute write FOnBeforeExecute;
  end;

  TJclBCC32 = class(TJclBorlandCommandLineTool)
  public
    function GetExeName: string; override;
  end;

  TProjectOptions = record
    UsePackages: Boolean;
    UnitOutputDir: string;
    SearchPath: string;
    DynamicPackages: string;
    SearchDcpPath: string;
    Conditionals: string;
  end;

  TJclDCC32 = class(TJclBorlandCommandLineTool)
  private
    FDCPSearchPath: string;
    FLibrarySearchPath: string;
    FCppSearchPath: string;
    FSupportsNoConfig: Boolean;
  protected
    procedure AddProjectOptions(const ProjectFileName, DCPPath: string);
    function Compile(const ProjectFileName: string): Boolean;
  public
    constructor Create(const ABinDirectory: string; ALongPathBug: Boolean;
      ACompilerSettingsFormat: TJclCompilerSettingsFormat; ASupportsNoConfig: Boolean;
      const ADCPSearchPath, ALibrarySearchPath, ACppSearchPath: string);
    function GetExeName: string; override;
    function Execute(const CommandLine: string): Boolean; override;
    function MakePackage(const PackageName, BPLPath, DCPPath: string; ExtraOptions: string = ''): Boolean;
    function MakeProject(const ProjectName, OutputDir, DcpSearchPath: string; ExtraOptions: string = ''): Boolean;
    procedure SetDefaultOptions; virtual;
    function AddBDSProjOptions(const ProjectFileName: string; var ProjectOptions: TProjectOptions): Boolean;
    function AddDOFOptions(const ProjectFileName: string; var ProjectOptions: TProjectOptions): Boolean;
    function AddDProjOptions(const ProjectFileName: string; var ProjectOptions: TProjectOptions): Boolean;
    property CppSearchPath: string read FCppSearchPath;
    property DCPSearchPath: string read FDCPSearchPath;
    property LibrarySearchPath: string read FLibrarySearchPath;
    property SupportsNoConfig: Boolean read FSupportsNoConfig;
  end;

  {$IFDEF MSWINDOWS}
  TJclDCCIL = class(TJclDCC32)
  private
    FMaxCLRVersion: string;
  protected
    function GetMaxCLRVersion: string;
  public
    function GetExeName: string; override;
    function MakeProject(const ProjectName, OutputDir, ExtraOptions: string): Boolean; reintroduce;
    procedure SetDefaultOptions; override;
    property MaxCLRVersion: string read GetMaxCLRVersion;
  end;
  {$ENDIF MSWINDOWS}

  TJclBpr2Mak = class(TJclBorlandCommandLineTool)
  public
    function GetExeName: string; override;
  end;

  TJclBorlandMake = class(TJclBorlandCommandLineTool)
  public
    function GetExeName: string; override;
  end;

const
  AsmExeName                = 'tasm32.exe';
  BCC32ExeName              = 'bcc32.exe';
  DCC32ExeName              = 'dcc32.exe';
  DCCILExeName              = 'dccil.exe';
  Bpr2MakExeName            = 'bpr2mak.exe';
  MakeExeName               = 'make.exe';

  BinaryExtensionPackage       = '.bpl';
  BinaryExtensionLibrary       = '.dll';
  BinaryExtensionExecutable    = '.exe';
  SourceExtensionDelphiPackage = '.dpk';
  SourceExtensionBCBPackage    = '.bpk';
  SourceExtensionDelphiProject = '.dpr';
  SourceExtensionBCBProject    = '.bpr';
  SourceExtensionDProject      = '.dproj';
  SourceExtensionBDSProject    = '.bdsproj';
  SourceExtensionDOFProject    = '.dof';
  SourceExtensionConfiguration = '.cfg';

function BinaryFileName(const OutputPath, ProjectFileName: string): string;

function IsDelphiPackage(const FileName: string): Boolean;
function IsDelphiProject(const FileName: string): Boolean;
function IsBCBPackage(const FileName: string): Boolean;
function IsBCBProject(const FileName: string): Boolean;

procedure GetDPRFileInfo(const DPRFileName: string; out BinaryExtension: string;
  const LibSuffix: PString = nil);
procedure GetBPRFileInfo(const BPRFileName: string; out BinaryFileName: string;
  const Description: PString = nil);
procedure GetDPKFileInfo(const DPKFileName: string; out RunOnly: Boolean;
  const LibSuffix: PString = nil; const Description: PString = nil);
procedure GetBPKFileInfo(const BPKFileName: string; out RunOnly: Boolean;
  const BinaryFileName: PString = nil; const Description: PString = nil);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysConst,
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  JclFileUtils, JclDevToolsResources,
  JclAnsiStrings, JclWideStrings, JclStrings,
  JclSysInfo, JclSimpleXml;

const
  // DOF options
  DOFDirectoriesSection = 'Directories';
  DOFUnitOutputDirKey   = 'UnitOutputDir';
  DOFSearchPathName     = 'SearchPath';
  DOFConditionals       = 'Conditionals';
  DOFLinkerSection      = 'Linker';
  DOFPackagesKey        = 'Packages';
  DOFCompilerSection    = 'Compiler';
  DOFPackageNoLinkKey   = 'PackageNoLink';
  // injection of new compiler options to workaround L1496 internal error of Delphi 5 and C++Builder 5
  // adding -B switch to the compiler command line forces units to be built
  DOFAdditionalSection  = 'Additional';
  DOFOptionsKey         = 'Options';

  // BDSProj options
  BDSProjPersonalityInfoNodeName = 'PersonalityInfo';
  BDSProjOptionNodeName = 'Option';
  BDSProjNameProperty = 'Name';
  BDSProjPersonalityValue = 'Personality';
  BDSProjUnitOutputDirValue = 'UnitOutputDir';
  BDSProjSearchPathValue = 'SearchPath';
  BDSProjPackagesValue = 'Packages';
  BDSProjConditionalsValue = 'Conditionals';
  BDSProjUsePackagesValue = 'UsePackages';
  BDSProjDirectoriesNodeName = 'Directories';

  // DProj options
  DProjProjectExtensionsNodeName = 'ProjectExtensions';
  DProjPersonalityNodeName = 'Borland.Personality';
  DProjDelphiPersonalityValue = 'Delphi.Personality';
  DProjDelphiDotNetPersonalityValue = 'DelphiDotNet.Personality';
  DProjPropertyGroupNodeName = 'PropertyGroup';
  DProjConditionValueName = 'Condition';
  DProjUsePackageNodeName = 'DCC_UsePackage';
  DProjDcuOutputDirNodeName = 'DCC_DcuOutput';
  DProjUnitSearchPathNodeName = 'DCC_UnitSearchPath';
  DProjDefineNodeName = 'DCC_Define';
  DProjConfigurationNodeName = 'Configuration';
  DProjPlatformNodeName = 'Platform';
  DProjProjectVersionNodeName = 'ProjectVersion';
  DProjConfigNodeName = 'Config';

  DelphiLibSuffixOption   = '{$LIBSUFFIX ''';
  DelphiDescriptionOption = '{$DESCRIPTION ''';
  DelphiRunOnlyOption     = '{$RUNONLY}';
  DelphiBinaryExtOption   = '{$E ';
  BCBLFlagsOption     = '<LFLAGS ';
  BCBDSwitchOption    = '-D';
  BCBGprSwitchOption  = '-Gpr';
  BCBProjectOption    = '<PROJECT ';

function AnsiStartsText(const SubStr, S: string): Boolean;
begin
  if Length(SubStr) <= Length(S) then
    Result := AnsiStrLIComp(PChar(S), PChar(SubStr), Length(SubStr)) = 0
  else
    Result := False;
end;

procedure GetDPRFileInfo(const DPRFileName: string; out BinaryExtension: string;
  const LibSuffix: PString = nil);
var
  Index: Integer;
  S: string;
  DPRFile: TStrings;
const
  ProgramText = 'program';
  LibraryText = 'library';
begin
  DPRFile := TStringList.Create;
  try
    DPRFile.LoadFromFile(DPRFileName);

    if Assigned(LibSuffix) then
      LibSuffix^ := '';

    BinaryExtension := '';

    for Index := 0 to DPRFile.Count - 1 do
    begin
      S := TrimRight(DPRFile.Strings[Index]);
      if AnsiStartsText(ProgramText, S) and (BinaryExtension = '') then
        BinaryExtension := BinaryExtensionExecutable;
      if AnsiStartsText(LibraryText, S) and (BinaryExtension = '') then
        BinaryExtension := BinaryExtensionLibrary;
      if AnsiStartsText(DelphiBinaryExtOption, S) then
        BinaryExtension :=
          StrTrimQuotes(Copy(S, Length(DelphiBinaryExtOption), Length(S) - Length(DelphiBinaryExtOption)));
      if Assigned(LibSuffix) and AnsiStartsText(DelphiLibSuffixOption, S) then
        LibSuffix^ :=
          StrTrimQuotes(Copy(S, Length(DelphiLibSuffixOption), Length(S) - Length(DelphiLibSuffixOption)));
    end;
  finally
    DPRFile.Free;
  end;
end;

procedure GetBPRFileInfo(const BPRFileName: string; out BinaryFileName: string;
  const Description: PString = nil);
var
  I, J: Integer;
  S, SubS1, SubS2, SubS3: string;
  BPKFile: TStringList;
  LProjectPos, BinaryFileNamePos, EndFileNamePos, LFlagsPos, DSwitchPos: Integer;
  SemiColonPos, AmpPos: Integer;
begin
  BPKFile := TStringList.Create;
  try
    BPKFile.LoadFromFile(BPRFileName);
    BinaryFileName := '';
    if Assigned(Description) then
      Description^ := '';
    for I := 0 to BPKFile.Count - 1 do
    begin
      S := BPKFile[I];

      LProjectPos := Pos(BCBProjectOption, S);
      if LProjectPos > 0 then
      begin
        SubS1 := Copy(S, LProjectPos, Length(S));
        J := 1;
        while (Pos('>', SubS1) = 0) and ((I + J) < BPKFile.Count) do
        begin
          SubS1 := SubS1 + BPKFile[I + J];
          Inc(J);
        end;

        BinaryFileNamePos := Pos('"', SubS1);
        if BinaryFileNamePos > 0 then
        begin
          SubS2 := Copy(SubS1, BinaryFileNamePos + 1, Length(SubS1) - BinaryFileNamePos);
          EndFileNamePos := Pos('"', SubS2);

          if EndFileNamePos > 0 then
            BinaryFileName := Copy(SubS2, 1, EndFileNamePos - 1);
        end;
      end;

      LFlagsPos := Pos(BCBLFlagsOption, S);
      if LFlagsPos > 0 then
      begin
        SubS1 := Copy(S, LFlagsPos, Length(S));
        J := 1;
        while (Pos('>', SubS1) = 0) and ((I + J) < BPKFile.Count) do
        begin
          SubS1 := SubS1 + BPKFile[I + J];
          Inc(J);
        end;
        DSwitchPos := Pos(BCBDSwitchOption, SubS1);
        if DSwitchPos > 0 then
        begin
          SubS2 := Copy(SubS1, DSwitchPos, Length(SubS1));
          SemiColonPos := Pos(';', SubS2);
          if SemiColonPos > 0 then
          begin
            SubS3 := Copy(SubS2, SemiColonPos + 1, Length(SubS2));
            AmpPos := Pos('&', SubS3);
            if (Description <> nil) and (AmpPos > 0) then
              Description^ := Copy(SubS3, 1, AmpPos - 1);
          end;
        end;
      end;
    end;
  finally
    BPKFile.Free;
  end;
end;

procedure GetDPKFileInfo(const DPKFileName: string; out RunOnly: Boolean;
  const LibSuffix: PString = nil; const Description: PString = nil);
var
  I: Integer;
  S: string;
  DPKFile: TStringList;
begin
  DPKFile := TStringList.Create;
  try
    DPKFile.LoadFromFile(DPKFileName);
    if Assigned(Description) then
      Description^ := '';
    if Assigned(LibSuffix) then
      LibSuffix^ := '';
    RunOnly := False;
    for I := 0 to DPKFile.Count - 1 do
    begin
      S := TrimRight(DPKFile.Strings[I]);
      if Assigned(Description) and (Pos(DelphiDescriptionOption, S) = 1) then
        Description^ := Copy(S, Length(DelphiDescriptionOption), Length(S) - Length(DelphiDescriptionOption))
      else
      if Assigned(LibSuffix) and (Pos(DelphiLibSuffixOption, S) = 1) then
        LibSuffix^ := StrTrimQuotes(Copy(S, Length(DelphiLibSuffixOption), Length(S) - Length(DelphiLibSuffixOption)))
      else
      if Pos(DelphiRunOnlyOption, S) = 1 then
        RunOnly := True;
    end;
  finally
    DPKFile.Free;
  end;
end;

procedure GetBPKFileInfo(const BPKFileName: string; out RunOnly: Boolean;
  const BinaryFileName: PString = nil; const Description: PString = nil);
var
  I, J: Integer;
  S, SubS1, SubS2, SubS3: string;
  BPKFile: TStringList;
  LFlagsPos, DSwitchPos, SemiColonPos, AmpPos, GprPos: Integer;
  LProjectPos, BinaryFileNamePos, EndFileNamePos: Integer;
begin
  BPKFile := TStringList.Create;
  try
    BPKFile.LoadFromFile(BPKFileName);
    if Assigned(Description) then
      Description^ := '';
    if Assigned(BinaryFileName) then
      BinaryFileName^ := '';
    RunOnly := False;
    for I := 0 to BPKFile.Count - 1 do
    begin
      S := BPKFile[I];

      LProjectPos := Pos(BCBProjectOption, S);
      if Assigned(BinaryFileName) and (LProjectPos > 0) then
      begin
        SubS1 := Copy(S, LProjectPos, Length(S));
        J := 1;
        while (Pos('>', SubS1) = 0) and ((I + J) < BPKFile.Count) do
        begin
          SubS1 := SubS1 + BPKFile[I + J];
          Inc(J);
        end;

        BinaryFileNamePos := Pos('"', SubS1);
        if BinaryFileNamePos > 0 then
        begin
          SubS2 := Copy(SubS1, BinaryFileNamePos + 1, Length(SubS1) - BinaryFileNamePos);
          EndFileNamePos := Pos('"', SubS2);

          if EndFileNamePos > 0 then
            BinaryFileName^ := Copy(SubS2, 1, EndFileNamePos - 1);
        end;
      end;

      LFlagsPos := Pos(BCBLFlagsOption, S);
      if LFlagsPos > 0 then
      begin
        SubS1 := Copy(S, LFlagsPos, Length(S));
        J := 1;
        while (Pos('>', SubS1) = 0) and ((I + J) < BPKFile.Count) do
        begin
          SubS1 := SubS1 + BPKFile[I + J];
          Inc(J);
        end;
        DSwitchPos := Pos(BCBDSwitchOption, SubS1);
        GprPos := Pos(BCBGprSwitchOption, SubS1);
        if DSwitchPos > 0 then
        begin
          SubS2 := Copy(SubS1, DSwitchPos, Length(SubS1));
          SemiColonPos := Pos(';', SubS2);
          if SemiColonPos > 0 then
          begin
            SubS3 := Copy(SubS2, SemiColonPos + 1, Length(SubS2));
            AmpPos := Pos('&', SubS3);
            if (Description <> nil) and (AmpPos > 0) then
              Description^ := Copy(SubS3, 1, AmpPos - 1);
          end;
        end;
        if GprPos > 0 then
          RunOnly := True;
      end;
    end;
  finally
    BPKFile.Free;
  end;
end;

function BinaryFileName(const OutputPath, ProjectFileName: string): string;
var
  ProjectExtension, LibSuffix, BinaryExtension: string;
  RunOnly: Boolean;
begin
  ProjectExtension := ExtractFileExt(ProjectFileName);
  if SameText(ProjectExtension, SourceExtensionDelphiPackage) then
  begin
    GetDPKFileInfo(ProjectFileName, RunOnly, @LibSuffix);
    Result := PathExtractFileNameNoExt(ProjectFileName) + LibSuffix + BinaryExtensionPackage;
  end
  else
  if SameText(ProjectExtension, SourceExtensionDelphiProject) then
  begin
    GetDPRFileInfo(ProjectFileName, BinaryExtension, @LibSuffix);
    Result := PathExtractFileNameNoExt(ProjectFileName) + LibSuffix + BinaryExtension;
  end
  else
  if SameText(ProjectExtension, SourceExtensionBCBPackage) then
    GetBPKFileInfo(ProjectFileName, RunOnly, @Result)
  else
  if SameText(ProjectExtension, SourceExtensionBCBProject) then
    GetBPRFileInfo(ProjectFileName, Result)
  else
    raise EJclCompilerUtilsException.CreateResFmt(@RsEUnknownProjectExtension, [ProjectExtension]);

  Result := PathAddSeparator(OutputPath) + Result;
end;

function IsDelphiPackage(const FileName: string): Boolean;
begin
  Result := SameText(ExtractFileExt(FileName), SourceExtensionDelphiPackage);
end;

function IsDelphiProject(const FileName: string): Boolean;
begin
  Result := SameText(ExtractFileExt(FileName), SourceExtensionDelphiProject);
end;

function IsBCBPackage(const FileName: string): Boolean;
begin
  Result := SameText(ExtractFileExt(FileName), SourceExtensionBCBPackage);
end;

function IsBCBProject(const FileName: string): Boolean;
begin
  Result := SameText(ExtractFileExt(FileName), SourceExtensionBCBProject);
end;

{$IFDEF MSWINDOWS}
type
  TFindResStartRec = record
    StartStr: WideString;
    MatchStr: WideString;
  end;
  PFindResStartRec = ^TFindResStartRec;

// helper function to check strings starting "StartStr" in current string table
function FindResStartCallBack(hModule: HMODULE; lpszType, lpszName: PChar;
  lParam: PFindResStartRec): BOOL; stdcall;
var
  ResInfo, ResHData, ResSize, ResIndex: Cardinal;
  ResData: PWord;
  StrLength: Word;
  MatchLen: Integer;
begin
  Result := True;
  MatchLen := Length(lParam^.StartStr);

  ResInfo := FindResource(hModule, lpszName, lpszType);
  if ResInfo <> 0 then
  begin
    ResHData := LoadResource(hModule, ResInfo);
    if ResHData <> 0 then
    begin
      ResData := LockResource(ResHData);
      if Assigned(ResData) then
      begin
        // string tables are a concatenation of maximum 16 prefixed-length widestrings
        ResSize := SizeofResource(hModule, ResInfo) div 2;
        ResIndex := 0;
        // iterate all concatenated strings
        while ResIndex < ResSize do
        begin
          StrLength := ResData^;
          Inc(ResData);
          Inc(ResIndex);
          if (StrLength >= MatchLen) and
            (StrLICompW(PWideChar(lParam^.StartStr), PWideChar(ResData), MatchLen) = 0) then
          begin
            // we have a match
            SetLength(lParam^.MatchStr, StrLength);
            Move(ResData^, lParam^.MatchStr[1], StrLength * SizeOf(lParam^.MatchStr[1]));
            Result := False;
            Break;
          end;
          Inc(ResData, StrLength);
          Inc(ResIndex, StrLength);
        end;
      end;
    end;
  end;
end;

// find in specified module "FileName" a resourcestring starting with StartStr
function FindResStart(const FileName: string; const StartStr: WideString): WideString;
var
  H: HMODULE;
  FindResRec: TFindResStartRec;
begin
  FindResRec.StartStr := StartStr;
  FindResRec.MatchStr := '';

  H := LoadLibraryEx(PChar(FileName), 0, LOAD_LIBRARY_AS_DATAFILE or DONT_RESOLVE_DLL_REFERENCES);
  if H <> 0 then
    try
      EnumResourceNames(H, RT_STRING, @FindResStartCallBack, LPARAM(@FindResRec));
    finally
      FreeLibrary(H);
    end;

  Result := FindResRec.MatchStr;
end;
{$ENDIF MSWINDOWS}

//=== { TJclBorlandCommandLineTool } =========================================

constructor TJclBorlandCommandLineTool.Create(const ABinDirectory: string; ALongPathBug: Boolean;
  ACompilerSettingsFormat: TJclCompilerSettingsFormat);
begin
  inherited Create;
  FBinDirectory := ABinDirectory;
  FLongPathBug := ALongPathBug;
  FCompilerSettingsFormat := ACompilerSettingsFormat;
  FOptions := TStringList.Create;
end;

destructor TJclBorlandCommandLineTool.Destroy;
begin
  FreeAndNil(FOptions);
  inherited Destroy;
end;

procedure TJclBorlandCommandLineTool.AddPathOption(const Option, Path: string);
var
  S: string;

  // path before Delphi 2005 must be shortened
  // to avoid the 126 character limit of DCC32 (and eventually other command line tools)
  // which shows up with misleading error messages ("Fatal: System.pas not found") or
  // might even cause AVs
  procedure ConvertToShortPathNames(var Paths: string);
  var
    List: TStringList;
    I: Integer;
  begin
    {$IFDEF MSWINDOWS}
    if LongPathBug then
    begin
      List := TStringList.Create;
      try
        StrToStrings(Paths, PathSep, List);
        for I := 0 to List.Count - 1 do
          List[I] := PathGetShortName(List[I]);
        Paths := StringsToStr(List, PathSep);
      finally
        List.Free;
      end;
    end;
    {$ENDIF MSWINDOWS}
  end;

begin
  S := PathRemoveSeparator(Path);
  ConvertToShortPathNames(S);
  { TODO : If we were sure that options are always case-insensitive
           for Borland tools, we could use UpperCase(Option) below. }
  S := Format('-%s"%s"', [Option, S]);
  // avoid duplicate entries
  if Options.IndexOf(S) = -1 then
    Options.Add(S);
end;

procedure TJclBorlandCommandLineTool.CheckOutputValid;
begin
  if Assigned(FOutputCallback) then
    raise EJclCommandLineToolError.CreateResFmt(@RsECmdLineToolOutputInvalid, [GetExeName]);
end;

function TJclBorlandCommandLineTool.Execute(const CommandLine: string): Boolean;
begin
  if Assigned(FOnBeforeExecute) then
    FOnBeforeExecute(Self);

  Result := InternalExecute(CommandLine);

  if Assigned(FOnAfterExecute) then
    FOnAfterExecute(Self);
end;

function TJclBorlandCommandLineTool.GetExeName: string;
begin
  Result := '';
  {$IFDEF MSWINDOWS}
  raise EAbstractError.CreateResFmt(@SAbstractError, ['']); // BCB doesn't support abstract keyword
  {$ENDIF MSWINDOWS}
end;

function TJclBorlandCommandLineTool.GetFileName: string;
begin
  Result := BinDirectory + GetExeName;
  if Pos(' ', Result) > 0 then
    Result := AnsiQuotedStr(Result, '"');
end;

function TJclBorlandCommandLineTool.GetOptions: TStrings;
begin
  Result := FOptions;
end;

function TJclBorlandCommandLineTool.GetOutput: string;
begin
  CheckOutputValid;
  Result := FOutput;
end;

function TJclBorlandCommandLineTool.GetOutputCallback: TTextHandler;
begin
  Result := FOutputCallback;
end;

function TJclBorlandCommandLineTool.InternalExecute(
  const CommandLine: string): Boolean;
var
  LaunchCommand: string;
begin
  LaunchCommand := Format('%s %s', [FileName, CommandLine]);
  if Assigned(FOutputCallback) then
  begin
    FOutputCallback(LaunchCommand);
    Result := JclSysUtils.Execute(LaunchCommand, OemTextHandler) = 0;
  end
  else
  begin
    Result := JclSysUtils.Execute(LaunchCommand, FOutput) = 0;
    {$IFDEF MSWINDOWS}
    FOutput := string(StrOemToAnsi(AnsiString(FOutput)));
    {$ENDIF MSWINDOWS}
  end;
end;

procedure TJclBorlandCommandLineTool.OemTextHandler(const Text: string);
var
  AnsiText: string;
begin
  if Assigned(FOutputCallback) then
  begin
    {$IFDEF MSWINDOWS}
    // Text is OEM under Windows
    // Code below seems to crash older compilers at times, so we only do
    // the casts when it's absolutely necessary, that is when compiling
    // with a unicode compiler.
    {$IFDEF UNICODE}
    AnsiText := string(StrOemToAnsi(AnsiString(Text)));
    {$ELSE}
    AnsiText := StrOemToAnsi(Text);
    {$ENDIF UNICODE}
    {$ELSE ~MSWINDOWS}
    AnsiText := Text;
    {$ENDIF ~MSWINDOWS}
    FOutputCallback(AnsiText);
  end;
end;

procedure TJclBorlandCommandLineTool.SetOutputCallback(const CallbackMethod: TTextHandler);
begin
  FOutputCallback := CallbackMethod;
end;

//=== { TJclBCC32 } ============================================================

function TJclBCC32.GetExeName: string;
begin
  Result := BCC32ExeName;
end;

//=== { TJclDCC32 } ============================================================

function TJclDCC32.AddDProjOptions(const ProjectFileName: string; var ProjectOptions: TProjectOptions): Boolean;
var
  DProjFileName, ProjectConfiguration, ProjectPlatform, PersonalityName: string;
  OptionsXmlFile: TJclSimpleXML;
  ProjectExtensionsNode, PropertyGroupNode, PersonalityNode, ChildNode: TJclSimpleXMLElem;
  NodeIndex: Integer;
  ConditionProperty: TJclSimpleXMLProp;
  Version: string;
begin
  Version := '';
  DProjFileName := ChangeFileExt(ProjectFileName, SourceExtensionDProject);
  Result := FileExists(DProjFileName) and (CompilerSettingsFormat = csfMsBuild);
  if Result then
  begin
    OptionsXmlFile := TJclSimpleXML.Create;
    try
      OptionsXmlFile.LoadFromFile(DProjFileName);
      OptionsXmlFile.Options := OptionsXmlFile.Options - [sxoAutoCreate];
      PersonalityName := '';
      ProjectExtensionsNode := OptionsXmlFile.Root.Items.ItemNamed[DProjProjectExtensionsNodeName];
      if Assigned(ProjectExtensionsNode) then
      begin
        PersonalityNode := ProjectExtensionsNode.Items.ItemNamed[DProjPersonalityNodeName];
        if Assigned(PersonalityNode) then
          PersonalityName := PersonalityNode.Value;
      end;
      if StrHasPrefix(PersonalityName, [DProjDelphiPersonalityValue]) or
        AnsiSameText(PersonalityName, DProjDelphiDotNetPersonalityValue) then
      begin
        ProjectConfiguration := '';
        ProjectPlatform := '';
        for NodeIndex := 0 to OptionsXmlFile.Root.Items.Count - 1 do
        begin
          PropertyGroupNode := OptionsXmlFile.Root.Items.Item[NodeIndex];
          if AnsiSameText(PropertyGroupNode.Name, DProjPropertyGroupNodeName) then
          begin
            ConditionProperty := PropertyGroupNode.Properties.ItemNamed[DProjConditionValueName];
            if Assigned(ConditionProperty) then
            begin
              if ((Version = '') and (ProjectConfiguration <> '') and (ProjectPlatform <> '') and
                  (AnsiPos(Format('%s|%s', [ProjectConfiguration, ProjectPlatform]), ConditionProperty.Value) > 0))
                 or
                 ((Version <> '') and (ProjectConfiguration <> '') and
                  (AnsiPos(ProjectConfiguration, ConditionProperty.Value) > 0))
                 or
                 ((Version <> '') and (ProjectConfiguration <> '') and
                  (AnsiPos('$(Base)', ConditionProperty.Value) > 0)) then
              begin
                // this is the active configuration, check for overrides
                ChildNode := PropertyGroupNode.Items.ItemNamed[DProjUsePackageNodeName];
                if Assigned(ChildNode) then
                  ProjectOptions.DynamicPackages := ChildNode.Value;
                ProjectOptions.UsePackages := ProjectOptions.DynamicPackages <> '';
                ChildNode := PropertyGroupNode.Items.ItemNamed[DProjDcuOutputDirNodeName];
                if Assigned(ChildNode) then
                  ProjectOptions.UnitOutputDir := ChildNode.Value;
                ChildNode := PropertyGroupNode.Items.ItemNamed[DProjUnitSearchPathNodeName];
                if Assigned(ChildNode) then
                  ProjectOptions.SearchPath := ChildNode.Value;
                ChildNode := PropertyGroupNode.Items.ItemNamed[DProjDefineNodeName];
                if Assigned(ChildNode) then
                  ProjectOptions.Conditionals := ChildNode.Value;
              end;
            end
            else
            begin
              // check for version and default configurations
              ChildNode := PropertyGroupNode.Items.ItemNamed[DProjProjectVersionNodeName];
              if Assigned(ChildNode) then
                Version := ChildNode.Value;

              if Version = '' then
              begin
                ChildNode := PropertyGroupNode.Items.ItemNamed[DProjConfigurationNodeName];
                if Assigned(ChildNode) then
                  ProjectConfiguration := ChildNode.Value;
                ChildNode := PropertyGroupNode.Items.ItemNamed[DProjPlatformNodeName];
                if Assigned(ChildNode) then
                  ProjectPlatform := ChildNode.Value;
              end
              else
              begin
                ChildNode := PropertyGroupNode.Items.ItemNamed[DProjConfigNodeName];
                if Assigned(ChildNode) then
                  ProjectConfiguration := ChildNode.Value;
              end;
            end;
          end;
        end;
      end;
    finally
      OptionsXmlFile.Free;
    end;
  end;
end;

function TJclDCC32.AddBDSProjOptions(const ProjectFileName: string; var ProjectOptions: TProjectOptions): Boolean;
var
  BDSProjFileName, PersonalityName: string;
  OptionsXmlFile: TJclSimpleXML;
  PersonalityInfoNode, OptionNode, ChildNode, PersonalityNode, DirectoriesNode: TJclSimpleXMLElem;
  NodeIndex: Integer;
  NameProperty: TJclSimpleXMLProp;
begin
  BDSProjFileName := ChangeFileExt(ProjectFileName, SourceExtensionBDSProject);
  Result := FileExists(BDSProjFileName);
  if Result then
  begin
    OptionsXmlFile := TJclSimpleXML.Create;
    try
      OptionsXmlFile.LoadFromFile(BDSProjFileName);
      OptionsXmlFile.Options := OptionsXmlFile.Options - [sxoAutoCreate];
      PersonalityInfoNode := OptionsXmlFile.Root.Items.ItemNamed[BDSProjPersonalityInfoNodeName];
      PersonalityName := '';
      if Assigned(PersonalityInfoNode) then
      begin
        OptionNode := PersonalityInfoNode.Items.ItemNamed[BDSProjOptionNodeName];
        if Assigned(OptionNode) then
          for NodeIndex := 0 to OptionNode.Items.Count - 1 do
          begin
            ChildNode := OptionNode.Items.Item[NodeIndex];
            if SameText(ChildNode.Name, BDSProjOptionNodeName) then
            begin
              NameProperty := ChildNode.Properties.ItemNamed[BDSProjNameProperty];
              if Assigned(NameProperty) and SameText(NameProperty.Value, BDSProjPersonalityValue) then
              begin
                PersonalityName := ChildNode.Value;
                Break;
              end;
            end;
          end;
      end;
      if PersonalityName <> '' then
      begin
        PersonalityNode := OptionsXmlFile.Root.Items.ItemNamed[PersonalityName];
        if Assigned(PersonalityNode) then
        begin
          DirectoriesNode := PersonalityNode.Items.ItemNamed[BDSProjDirectoriesNodeName];
          if Assigned(DirectoriesNode) then
            for NodeIndex := 0 to DirectoriesNode.Items.Count - 1 do
            begin
              ChildNode := DirectoriesNode.Items.Item[NodeIndex];
              if SameText(ChildNode.Name, BDSProjDirectoriesNodeName) then
              begin
                NameProperty := ChildNode.Properties.ItemNamed[BDSProjNameProperty];
                if Assigned(NameProperty) then
                begin
                  if SameText(NameProperty.Value, BDSProjUnitOutputDirValue) then
                    ProjectOptions.UnitOutputDir := ChildNode.Value
                  else
                  if SameText(NameProperty.Value, BDSProjSearchPathValue) then
                    ProjectOptions.SearchPath := ChildNode.Value
                  else
                  if SameText(NameProperty.Value, BDSProjPackagesValue) then
                    ProjectOptions.DynamicPackages := ChildNode.Value
                  else
                  if SameText(NameProperty.Value, BDSProjConditionalsValue) then
                    ProjectOptions.Conditionals := ChildNode.Value
                  else
                  if SameText(NameProperty.Value, BDSProjUsePackagesValue) then
                    ProjectOptions.UsePackages := StrToBoolean(ChildNode.Value);
                end;
              end;
            end;
        end;
      end;
    finally
      OptionsXmlFile.Free;
    end;
  end;
end;

function TJclDCC32.AddDOFOptions(const ProjectFileName: string; var ProjectOptions: TProjectOptions): Boolean;
var
  DOFFileName: string;
  OptionsFile: TIniFile;
begin
  DOFFileName := ChangeFileExt(ProjectFileName, SourceExtensionDOFProject);
  Result := FileExists(DOFFileName);
  if Result then
  begin
    OptionsFile := TIniFile.Create(DOFFileName);
    try
      ProjectOptions.SearchPath := OptionsFile.ReadString(DOFDirectoriesSection, DOFSearchPathName, '');
      ProjectOptions.UnitOutputDir := OptionsFile.ReadString(DOFDirectoriesSection, DOFUnitOutputDirKey, '');
      ProjectOptions.Conditionals := OptionsFile.ReadString(DOFDirectoriesSection, DOFConditionals, '');
      ProjectOptions.UsePackages := OptionsFile.ReadString(DOFCompilerSection, DOFPackageNoLinkKey, '') = '1';
      ProjectOptions.DynamicPackages := OptionsFile.ReadString(DOFLinkerSection, DOFPackagesKey, '');
    finally
      OptionsFile.Free;
    end;
  end;
end;

procedure TJclDCC32.AddProjectOptions(const ProjectFileName, DCPPath: string);
var
  ProjectOptions: TProjectOptions;
begin
  ProjectOptions.UsePackages := False;
  ProjectOptions.UnitOutputDir := '';
  ProjectOptions.SearchPath := '';
  ProjectOptions.DynamicPackages := '';
  ProjectOptions.SearchDcpPath := '';
  ProjectOptions.Conditionals := '';

  if AddDProjOptions(ProjectFileName, ProjectOptions) or
     AddBDSProjOptions(ProjectFileName, ProjectOptions) or
     AddDOFOptions(ProjectFileName, ProjectOptions) then
  begin
    if ProjectOptions.UnitOutputDir <> '' then
      AddPathOption('N', ProjectOptions.UnitOutputDir);
    if ProjectOptions.SearchPath <> '' then
    begin
      AddPathOption('I', ProjectOptions.SearchPath);
      AddPathOption('R', ProjectOptions.SearchPath);
    end;
    if ProjectOptions.Conditionals <> '' then
      Options.Add(Format('-D%s', [ProjectOptions.Conditionals]));
    if SamePath(DCPPath, DCPSearchPath) then
      ProjectOptions.SearchDcpPath := DCPPath
    else
      ProjectOptions.SearchDcpPath := StrEnsureSuffix(PathSep, DCPPath) + DCPSearchPath;
    AddPathOption('U', StrEnsureSuffix(PathSep, ProjectOptions.SearchDcpPath) + ProjectOptions.SearchPath);
    if ProjectOptions.UsePackages and (ProjectOptions.DynamicPackages <> '') then
      Options.Add(Format('-LU"%s"', [ProjectOptions.DynamicPackages]));
  end;
end;

function TJclDCC32.Compile(const ProjectFileName: string): Boolean;
begin
  // Note: PathGetShortName may not return the short path if it's a network
  // drive. Hence we always double quote the path, regardless of the compiling
  // environment.
  Result := Execute(StrDoubleQuote(StrTrimQuotes(ProjectFileName)));
end;

constructor TJclDCC32.Create(const ABinDirectory: string; ALongPathBug: Boolean;
  ACompilerSettingsFormat: TJclCompilerSettingsFormat; ASupportsNoConfig: Boolean;
  const ADCPSearchPath, ALibrarySearchPath, ACppSearchPath: string);
begin
  inherited Create(ABinDirectory, ALongPathBug, ACompilerSettingsFormat);
  FSupportsNoConfig := ASupportsNoConfig;
  FDCPSearchPath := ADCPSearchPath;
  FLibrarySearchPath := ALibrarySearchPath;
  FCppSearchPath := ACppSearchPath;
  SetDefaultOptions; // in case $(DELPHI)\bin\dcc32.cfg (replace as appropriate) is invalid
end;

function TJclDCC32.Execute(const CommandLine: string): Boolean;
  function IsPathOption(const S: string; out Len: Integer): Boolean;
  begin
    Result := False;
    if (Length(S) >= 2) and (S[1] = '-') then
      case UpCase(S[2]) of
        'E', 'I', 'O', 'R', 'U':
          begin
            Result := True;
            Len := 2;
          end;
        'L':
          if Length(S) >= 3 then
          begin
            case UpCase(S[3]) of
              'E', 'e',
              'N', 'n':
                Result := True;
            else
              Result := False;
            end;
            Len := 3;
          end;
        'N':
          begin
            Result := True;
            if (Length(S) >= 3) then
            begin
              case Upcase(S[3]) of
                '0'..'9',
                'H', 'O', 'B':
                  Len := 3;
              else
                Len := 2;
              end;
            end;
          end;
      end;
  end;
var
  OptionIndex, PathIndex, SwitchLen: Integer;
  PathList: TStrings;
  Option, Arguments, CurrentFolder: string;
begin
  if Assigned(FOnBeforeExecute) then
    FOnBeforeExecute(Self);

  FOutput := '';
  Arguments := '';
  CurrentFolder := GetCurrentFolder;

  PathList := TStringList.Create;
  try
    for OptionIndex := 0 to Options.Count - 1 do
    begin
      Option := Options.Strings[OptionIndex];
      if IsPathOption(Option, SwitchLen) then
      begin

        StrToStrings(StrTrimQuotes(Copy(Option, SwitchLen + 1, Length(Option) - SwitchLen)), PathSep, PathList);
        // change to relative paths to avoid DCC32 126 character path limit
        for PathIndex := 0 to PathList.Count - 1 do
          PathList.Strings[PathIndex] := PathGetRelativePath(CurrentFolder, ExpandFileName(PathList[PathIndex]));
        if PathList.Count > 0 then
          Arguments := Format('%s %s"%s"', [Arguments, Copy(Option, 1, SwitchLen),
            StringsToStr(PathList, PathSep)]);
      end
      else
        Arguments := Format('%s %s', [Arguments, Option]);
    end;
  finally
    PathList.Free;
  end;

  Result := InternalExecute(CommandLine + Arguments);

  if Assigned(FOnAfterExecute) then
    FOnAfterExecute(Self);
end;

function TJclDCC32.GetExeName: string;
begin
  Result := DCC32ExeName;
end;

function TJclDCC32.MakePackage(const PackageName, BPLPath, DCPPath: string; ExtraOptions: string): Boolean;
var
  SaveDir: string;
  ConfigurationFileName, BackupFileName: string;
begin
  SaveDir := GetCurrentDir;
  SetCurrentDir(ExtractFilePath(PackageName) + '.');
  try
    // backup existing configuration file, if any
    ConfigurationFileName := ChangeFileExt(PackageName, SourceExtensionConfiguration);
    if FileExists(ConfigurationFileName) then
      FileBackup(ConfigurationFileName, True);

    Options.Clear;
    SetDefaultOptions;
    AddProjectOptions(PackageName, DCPPath);
    try
      AddPathOption('LN', DCPPath);
      AddPathOption('LE', BPLPath);
      Options.Add(ExtraOptions);
      Result := Compile(PackageName);
    finally
      // restore existing configuration file, if any
      BackupFileName := GetBackupFileName(ConfigurationFileName);
      if FileExists(BackupFileName) then
        FileMove(BackupFileName, ConfigurationFileName, True);
    end;
  finally
    SetCurrentDir(SaveDir);
  end;
end;

function TJclDCC32.MakeProject(const ProjectName, OutputDir, DcpSearchPath: string;
  ExtraOptions: string): Boolean;
var
  SaveDir: string;
  ConfigurationFileName, BackupFileName: string;
begin
  SaveDir := GetCurrentDir;
  SetCurrentDir(ExtractFilePath(ProjectName) + '.');
  try
    // backup existing configuration file, if any
    ConfigurationFileName := ChangeFileExt(ProjectName, SourceExtensionConfiguration);
    if FileExists(ConfigurationFileName) then
      FileBackup(ConfigurationFileName, True);

    Options.Clear;
    SetDefaultOptions;
    AddProjectOptions(ProjectName, DcpSearchPath);
    try
      AddPathOption('E', OutputDir);
      Options.Add(ExtraOptions);
      Result := Compile(ProjectName);
    finally
      // restore existing configuration file, if any
      BackupFileName := GetBackupFileName(ConfigurationFileName);
      if FileExists(BackupFileName) then
        FileMove(BackupFileName, ConfigurationFileName, True);
    end;
  finally
    SetCurrentDir(SaveDir);
  end;
end;

procedure TJclDCC32.SetDefaultOptions;
begin
  Options.Clear;
  if SupportsNoConfig then
    Options.Add('--no-config');
  AddPathOption('U', LibrarySearchPath);
  if CppSearchPath <> '' then
  begin
    AddPathOption('U', CppSearchPath);
    Options.Add('-LUrtl');
  end;
end;

{$IFDEF MSWINDOWS}
//=== { TJclDCCIL } ==========================================================

function TJclDCCIL.GetExeName: string;
begin
  Result := DCCILExeName;
end;

function TJclDCCIL.GetMaxCLRVersion: string;
var
  StartPos, EndPos: Integer;
begin
  if FMaxCLRVersion <> '' then
  begin
    Result := FMaxCLRVersion;
    Exit;
  end;

  Result := FindResStart(BinDirectory + GetExeName, '  --clrversion');

  StartPos := Pos(':', Result);
  if StartPos = 0 then
    StartPos := Pos('=', Result);

  if StartPos > 0 then
    Result := Copy(Result, StartPos + 1, Length(Result) - StartPos);

  EndPos := Pos(' ', Result);
  if EndPos > 0 then
    SetLength(Result, EndPos - 1);

  if Result = '' then
    Result := 'v1.1.4322'; // do not localize

  FMaxCLRVersion := Result;
end;

function TJclDCCIL.MakeProject(const ProjectName, OutputDir,
  ExtraOptions: string): Boolean;
var
  SaveDir: string;
begin
  SaveDir := GetCurrentDir;
  SetCurrentDir(ExtractFilePath(ProjectName) + '.');
  try
    Options.Clear;
    SetDefaultOptions;
    AddProjectOptions(ProjectName, '');
    AddPathOption('E', OutputDir);
    Options.Add(ExtraOptions);
    Result := Compile(ProjectName);
  finally
    SetCurrentDir(SaveDir);
  end;
end;

procedure TJclDCCIL.SetDefaultOptions;
begin
  Options.Clear;
  AddPathOption('U', LibrarySearchPath);
end;

{$ENDIF MSWINDOWS}

//=== { TJclBorlandMake } ====================================================

function TJclBorlandMake.GetExeName: string;
begin
  Result := MakeExeName;
end;

//=== { TJclBpr2Mak } ========================================================

function TJclBpr2Mak.GetExeName: string;
begin
  Result := Bpr2MakExeName;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

