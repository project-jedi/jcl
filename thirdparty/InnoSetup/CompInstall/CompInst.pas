unit CompInst;

interface

uses
  Windows, SysUtils, Classes, JclBorlandTools;

function Installations: TJclBorRADToolInstallations;

function compinst_init: Integer; stdcall;

function compinst_isDelphiInstalled(Version: Integer): Integer; stdcall;
function compinst_isBCBInstalled(Version: Integer): Integer; stdcall;
function compinst_isBDSInstalled(IDEVersion: Integer): Integer; stdcall;

function compinst_installDelphiDesignPackage(Version: Integer; const BplFilename, Description: PChar): Integer; stdcall;
function compinst_installBCBDesignPackage(Version: Integer; const BplFilename, Description: PChar): Integer; stdcall;
function compinst_uninstallDelphiDesignPackage(Version: Integer; const BplFilename: PChar): Integer; stdcall;
function compinst_uninstallBCBDesignPackage(Version: Integer; const BplFilename: PChar): Integer; stdcall;
function compinst_uninstallDelphiDesignPackagesPrefixed(Version: Integer; BplFilenamePrefix: PChar): Integer; stdcall;
function compinst_uninstallBCBDesignPackagesPrefixed(Version: Integer; BplFilenamePrefix: PChar): Integer; stdcall;

function compinst_installDelphiExpert(Version: Integer; const Filename, Description: PChar): Integer; stdcall;
function compinst_installBCBExpert(Version: Integer; const Filename, Description: PChar): Integer; stdcall;
function compinst_uninstallDelphiExpert(Version: Integer; const Filename: PChar): Integer; stdcall;
function compinst_uninstallBCBExpert(Version: Integer; const Filename: PChar): Integer; stdcall;
function compinst_uninstallDelphiExpertsPrefixed(Version: Integer; FilenamePrefix: PChar): Integer; stdcall;
function compinst_uninstallBCBExpertsPrefixed(Version: Integer; FilenamePrefix: PChar): Integer; stdcall;

function compinst_addDelphiSearchPaths(Version: Integer; SearchPaths, DebugPaths, BrowsePaths: PChar): Integer; stdcall;
function compinst_addBCBSearchPaths(Version: Integer; SearchPaths, DebugPaths, BrowsePaths: PChar): Integer; stdcall;
function compinst_removeDelphiSearchPaths(Version: Integer; SearchPaths, DebugPaths, BrowsePaths: PChar): Integer; stdcall;
function compinst_removeBCBSearchPaths(Version: Integer; SearchPaths, DebugPaths, BrowsePaths: PChar): Integer; stdcall;

implementation

uses
  JclPeImage, StrUtils;

var
  GlobalInstallations: TJclBorRADToolInstallations;

function GetPackageDescription(const BplFilename: string): string;
var
  hLib: THandle;
  Info: TJclPePackageInfo;
begin
  Result := '';
  hLib := LoadLibraryEx(PChar(BplFilename), 0, LOAD_LIBRARY_AS_DATAFILE);
  if hLib <> 0 then
  begin
    Info := TJclPePackageInfo.Create(hLib);
    try
      Result := Trim(Info.Description);
    finally
      Info.Free;
    end;
    FreeLibrary(hLib);
  end;
  if Result = '' then
    Result := ChangeFileExt(ExtractFileName(BplFilename), '');
end;

procedure SplitPaths(List: TStrings; const Paths: string);
var
  I, StartI: Integer;
  S: string;
begin
  StartI := 1;
  for I := 1 to Length(Paths) do
  begin
    if Paths[I] = PathSep then
    begin
      S := Trim(Copy(Paths, StartI, I - StartI));
      if S <> '' then
        List.Add(S);
      StartI := I + 1;
    end;
  end;
  S := Trim(Copy(Paths, StartI, MaxInt));
  if S <> '' then
    List.Add(S);
end;

function Installations: TJclBorRADToolInstallations;
begin
  if GlobalInstallations = nil then
    GlobalInstallations := TJclBorRADToolInstallations.Create;
  Result := GlobalInstallations;
end;

function compinst_init: Integer; stdcall;
var
  I: Integer;
  Inst: TJclBorRADToolInstallation;
  VStr: string;
  ConfigDataLocation: string;
begin
  Result := Installations.Count;
  for I := 0 to Installations.Count - 1 do
  begin
    Inst := Installations[I];

    ConfigDataLocation := Inst.ConfigDataLocation;
    if (ConfigDataLocation <> '') and (ConfigDataLocation[1] = PathDelim) then
      ConfigDataLocation := Copy(ConfigDataLocation, 2, MaxInt); // there is no such thing as am absolute "\Software" registry key

    case Inst.RadToolKind of
      brDelphi:
        begin
          VStr := IntToStr(Inst.VersionNumber);
          SetEnvironmentVariable(PChar('DELPHI' + VStr), PChar(Inst.RootDir));
          SetEnvironmentVariable(PChar('DELPHI' + VStr + 'BPL'), PChar(Inst.BPLOutputPath));
          SetEnvironmentVariable(PChar('DELPHI' + VStr + 'DCP'), PChar(Inst.DCPOutputPath));
          SetEnvironmentVariable(PChar('DELPHI' + VStr + 'RegKey'), PChar(ConfigDataLocation));
        end;
      brCppBuilder:
        begin
          VStr := IntToStr(Inst.VersionNumber);
          SetEnvironmentVariable(PChar('BCB' + VStr), PChar(Inst.RootDir));
          SetEnvironmentVariable(PChar('BCB' + VStr + 'BPL'), PChar(Inst.BPLOutputPath));
          SetEnvironmentVariable(PChar('BCB' + VStr + 'DCP'), PChar(Inst.DCPOutputPath));
          SetEnvironmentVariable(PChar('BCB' + VStr + 'RegKey'), PChar(ConfigDataLocation));
        end;
      brBorlandDevStudio:
        begin
          VStr := IntToStr(9 - 3 + Inst.VersionNumber); // Delphi 9 is BDS 3
          if bpDelphi32 in Inst.Personalities then
          begin
            SetEnvironmentVariable(PChar('DELPHI' + VStr), PChar(Inst.RootDir));
            SetEnvironmentVariable(PChar('DELPHI' + VStr + 'BPL'), PChar(Inst.BPLOutputPath));
            SetEnvironmentVariable(PChar('DELPHI' + VStr + 'DCP'), PChar(Inst.DCPOutputPath));
            SetEnvironmentVariable(PChar('DELPHI' + VStr + 'RegKey'), PChar(ConfigDataLocation));
          end;
          if bpBCBuilder32 in Inst.Personalities then
          begin
            SetEnvironmentVariable(PChar('BCB' + VStr), PChar(Inst.RootDir));
            SetEnvironmentVariable(PChar('BCB' + VStr + 'BPL'), PChar(Inst.BPLOutputPath));
            SetEnvironmentVariable(PChar('BCB' + VStr + 'DCP'), PChar(Inst.DCPOutputPath));
            SetEnvironmentVariable(PChar('BCB' + VStr + 'RegKey'), PChar(ConfigDataLocation));
          end;
        end;
    end;
  end;
end;

function compinst_IsDelphiInstalled(Version: Integer): Integer; stdcall;
begin
  Result := Ord(Installations.DelphiVersionInstalled[Version]);
end;

function compinst_IsBCBInstalled(Version: Integer): Integer; stdcall;
begin
  Result := Ord(Installations.BCBVersionInstalled[Version]);
end;

function compinst_IsBDSInstalled(IDEVersion: Integer): Integer; stdcall;
begin
  Result := Ord(Installations.BDSVersionInstalled[IDEVersion]);
end;

{ Design Packages }

function InstallDesignPackage(Inst: TJclBorRADToolInstallation; const BplFilename, Description: string): Integer;
var
  Filename, Descr: string;
  MatchFound: TFilenameCaseMatch;
begin
  Result := 0;
  if Inst <> nil then
  begin
    Descr := Description;
    if Descr = '' then
      Descr := GetPackageDescription(BplFilename);
    Filename := ExpandFileNameCase(BplFilename, MatchFound); // correct file name
    if Inst.RegisterPackage(Filename, Descr) then
      Result := 1;
  end;
end;

function UninstallDesignPackage(Inst: TJclBorRADToolInstallation; const BplFilename: string): Integer;
begin
  Result := 0;
  if Inst <> nil then
    if Inst.UnregisterPackage(BplFilename) then
      Result := 1;
end;

function UninstallDesignPackagesPrefixed(Inst: TJclBorRADToolInstallation; const BplFilenamePrefix: string): Integer;
var
  I: Integer;
  Filename: string;
begin
  Result := 0;
  if Inst <> nil then
  begin
    for I := Inst.IdePackages.Count - 1 downto 0 do
    begin
      FileName := Inst.IdePackages.PackageFileNames[I];
      if AnsiStartsText(BplFilenamePrefix, ExtractFileName(FileName)) then
      begin
        UninstallDesignPackage(Inst, Filename);
        Inc(Result);
      end;
    end;
  end;
end;

{ Experts }

function InstallExpert(Inst: TJclBorRADToolInstallation; const Filename, Description: string): Integer;
var
  MatchFound: TFilenameCaseMatch;
begin
  Result := 0;
  if Inst <> nil then
  begin
    if Inst.RegisterExpert(ExpandFileNameCase(Filename, MatchFound), Description) then
      Result := 1;
  end;
end;

function UninstallExpert(Inst: TJclBorRADToolInstallation; const Filename: string): Integer;
begin
  Result := 0;
  if Inst <> nil then
    if Inst.UnregisterExpert(Filename) then
      Result := 1;
end;

function UninstallExpertsPrefixed(Inst: TJclBorRADToolInstallation; const FilenamePrefix: string): Integer;
var
  I: Integer;
  Filename: string;
begin
  Result := 0;
  if Inst <> nil then
  begin
    for I := Inst.IdePackages.ExpertCount - 1 downto 0 do
    begin
      FileName := Inst.IdePackages.ExpertFileNames[I];
      if AnsiStartsText(FilenamePrefix, ExtractFileName(FileName)) then
      begin
        UninstallExpert(Inst, Filename);
        Inc(Result);
      end;
    end;
  end;
end;

{ Search Paths }

function ChangeSearchPaths(Inst: TJclBorRADToolInstallation; Installing: Boolean; const SearchPaths, DebugPaths, BrowsePaths: string): Integer;
begin
  Result := 0;
  if Inst <> nil then
  begin
    if Installing then
    begin
      Inst.AddToLibrarySearchPath(SearchPaths);
      Inst.AddToDebugDCUPath(DebugPaths);
      Inst.AddToLibraryBrowsingPath(BrowsePaths);
    end
    else
    begin
      Inst.RemoveFromLibrarySearchPath(SearchPaths);
      Inst.RemoveFromDebugDCUPath(DebugPaths);
      Inst.RemoveFromLibraryBrowsingPath(BrowsePaths);
    end;
    Result := 1;
  end;
end;

{ Design Packages }

function compinst_installDelphiDesignPackage(Version: Integer; const BplFilename, Description: PChar): Integer; stdcall;
begin
  Result := InstallDesignPackage(Installations.DelphiInstallationFromVersion[Version], BplFilename, Description);
end;

function compinst_installBCBDesignPackage(Version: Integer; const BplFilename, Description: PChar): Integer; stdcall;
begin
  Result := InstallDesignPackage(Installations.BCBInstallationFromVersion[Version], BplFilename, Description);
end;

function compinst_uninstallDelphiDesignPackage(Version: Integer; const BplFilename: PChar): Integer; stdcall;
begin
  Result := UninstallDesignPackage(Installations.DelphiInstallationFromVersion[Version], BplFilename);
end;

function compinst_uninstallBCBDesignPackage(Version: Integer; const BplFilename: PChar): Integer; stdcall;
begin
  Result := UninstallDesignPackage(Installations.BCBInstallationFromVersion[Version], BplFilename);
end;

function compinst_uninstallDelphiDesignPackagesPrefixed(Version: Integer; BplFilenamePrefix: PChar): Integer; stdcall;
begin
  Result := UninstallDesignPackagesPrefixed(Installations.DelphiInstallationFromVersion[Version], BplFilenamePrefix);
end;

function compinst_uninstallBCBDesignPackagesPrefixed(Version: Integer; BplFilenamePrefix: PChar): Integer; stdcall;
begin
  Result := UninstallDesignPackagesPrefixed(Installations.BCBInstallationFromVersion[Version], BplFilenamePrefix);
end;

{ Experts }

function compinst_installDelphiExpert(Version: Integer; const Filename, Description: PChar): Integer; stdcall;
begin
  Result := InstallExpert(Installations.DelphiInstallationFromVersion[Version], Filename, Description);
end;

function compinst_installBCBExpert(Version: Integer; const Filename, Description: PChar): Integer; stdcall;
begin
  Result := InstallExpert(Installations.BCBInstallationFromVersion[Version], Filename, Description);
end;

function compinst_uninstallDelphiExpert(Version: Integer; const Filename: PChar): Integer; stdcall;
begin
  Result := UninstallExpert(Installations.DelphiInstallationFromVersion[Version], Filename);
end;

function compinst_uninstallBCBExpert(Version: Integer; const Filename: PChar): Integer; stdcall;
begin
  Result := UninstallExpert(Installations.BCBInstallationFromVersion[Version], Filename);
end;

function compinst_uninstallDelphiExpertsPrefixed(Version: Integer; FilenamePrefix: PChar): Integer; stdcall;
begin
  Result := UninstallExpertsPrefixed(Installations.DelphiInstallationFromVersion[Version], FilenamePrefix);
end;

function compinst_uninstallBCBExpertsPrefixed(Version: Integer; FilenamePrefix: PChar): Integer; stdcall;
begin
  Result := UninstallExpertsPrefixed(Installations.BCBInstallationFromVersion[Version], FilenamePrefix);
end;

{ Search Paths }

function compinst_addDelphiSearchPaths(Version: Integer; SearchPaths, DebugPaths, BrowsePaths: PChar): Integer; stdcall;
begin
  Result := ChangeSearchPaths(Installations.DelphiInstallationFromVersion[Version], True, SearchPaths, DebugPaths, BrowsePaths);
end;

function compinst_addBCBSearchPaths(Version: Integer; SearchPaths, DebugPaths, BrowsePaths: PChar): Integer; stdcall;
begin
  Result := ChangeSearchPaths(Installations.BCBInstallationFromVersion[Version], True, SearchPaths, DebugPaths, BrowsePaths);
end;

function compinst_removeDelphiSearchPaths(Version: Integer; SearchPaths, DebugPaths, BrowsePaths: PChar): Integer; stdcall;
begin
  Result := ChangeSearchPaths(Installations.DelphiInstallationFromVersion[Version], False, SearchPaths, DebugPaths, BrowsePaths);
end;

function compinst_removeBCBSearchPaths(Version: Integer; SearchPaths, DebugPaths, BrowsePaths: PChar): Integer; stdcall;
begin
  Result := ChangeSearchPaths(Installations.BCBInstallationFromVersion[Version], False, SearchPaths, DebugPaths, BrowsePaths);
end;

initialization

finalization
  Installations.Free;

end.
