{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: BuildTarget.pas, released on 2004-03-25.

The Initial Developer of the Original Code is Andreas Hausladen
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

program build;

{$APPTYPE CONSOLE}

{ build.exe setups the environment for a Delphi compiler }

uses
  Windows, ShlObj;

type
  TOption = record
   Name: string;
   Env: string;
   Default: string;
 end;

{$IFDEF JCL}
const
  LibraryName = 'JCL';
  LibraryRootDirRelativeToBuild = 2; // means: '..\..'
  pgEditFile = 'install\build\pgEdit.xml'; // relative to the Library-Directory
  ExtraOptions: array[0..0] of TOption = (
    (Name: ''; Env: ''; Default: '')
  );
  PackageGroupName = 'JclPackages*0';
{$ENDIF JCL}
{$IFDEF JVCL}
const
  LibraryName = 'JVCL';
  LibraryRootDirRelativeToBuild = 2; // means: '..\..'
  pgEditFile = 'devtools\bin\pgEdit.xml'; // relative to the Library-Directory
  ExtraOptions: array[0..0] of TOption = (
    (Name: 'jcl-path'; Env: 'JCLROOT'; Default: '..\..\..\jcl')
  );
  PackageGroupName = '* Packages';
{$ENDIF JVCL}

{$IFNDEF JCL}
 {$IFNDEF JVCL}
  {$IFDEF MSWINDOWS}
   {$Message Fatal 'Neither JCL nor JVCL is defined'}
  {$ENDIF MSWINDOWS}
 {$ENDIF ~JVCL}
{$ENDIF ~JCL}

type
  TTarget = record
    Name: string;
    PerName: string;
    PerDir: string;
  end;

const // keep in sync with JVCL Installer's DelphiData.pas
  BDSVersions: array[1..3] of record
                                Name: string;
                                VersionStr: string;
                                Version: Integer;
                                CIV: string; // coreide version
                                ProjectDirResId: Integer;
                                Supported: Boolean;
                              end = (
    (Name: 'C#Builder'; VersionStr: '1.0'; Version: 1; CIV: '71'; ProjectDirResId: 64507; Supported: False),
    (Name: 'Delphi'; VersionStr: '8'; Version: 8; CIV: '71'; ProjectDirResId: 64460; Supported: False),
    (Name: 'Delphi'; VersionStr: '2005'; Version: 9; CIV: '90'; ProjectDirResId: 64431; Supported: True)
  );

type
  TEdition = class(TObject)
  private
    FMainName: string;      // d7
    FName: string;          // d7p        ( with/-out personal "p" )

    FRootDir: string;
    FBplDir: string;
    FDcpDir: string;
    FLibDir: string;
    FIsPersonal: Boolean;
    FIsCLX: Boolean;

    function GetBDSProjectsDir: string;
    procedure ReadRegistryData;
  public
    Typ: (Delphi, BCB, BDS);
    VersionStr: string;     // '9' for BDS 3.0
    Version: Integer;       // 9 for BDS 3.0
    IDEVersionStr: string;  // '3' for BDS 3.0
    IDEVersion: Integer;    // 3 for BDS 3.0
    PkgDir: string;         // d7 / d7per
  public
    constructor Create(const AEditionName, PerDirName: string);

    property RootDir: string read FRootDir;
    property BDSProjectsDir: string read GetBDSProjectsDir;
    property BplDir: string read FBplDir;
    property DcpDir: string read FDcpDir;
    property LibDir: string read FLibDir;

    property MainName: string read FMainName;
    property Name: string read FName;
    property IsPersonal: Boolean read FIsPersonal;
    property IsCLX: Boolean read FIsCLX;
  end;

var
  LibraryRootDir: string;
  DxgettextDir: string = '';
  ExtraUnitDirs: string = '';
  MakeOptions: string = '';
  Verbose: Boolean = False;
  Force: Boolean = False; // force even if the target is not installed
  DccOpt: string = '-Q -M';
  UserLibDir, UserDcpDir, UserBplDir: string;

  Targets: array of TTarget = nil;
  Editions: array of TEdition = nil;

{ Helper functions because no SysUtils unit is used. }
{******************************************************************************}
function ExtractFileDir(const S: string): string;
var
  ps: Integer;
begin
  ps := Length(S);
  while (ps > 1) and (S[ps] <> '\') do
    Dec(ps);
  Result := Copy(S, 1, ps - 1);
end;
{******************************************************************************}
function ExcludeTrailingPathDelimiter(const S: string): string;
begin
  if (S <> '') and (S[Length(S)] = '\') then
    Result := Copy(S, 1, Length(S) - 1)
  else
    Result := S;
end;
{******************************************************************************}
function StrLen(P: PChar): Integer;
begin
  Result := 0;
  while P[Result] <> #0 do
    Inc(Result);
end;
{******************************************************************************}
function StrToInt(const S: string): Integer;
var
  Error: Integer;
begin
  Val(S, Result, Error);
end;
{******************************************************************************}
function IntToStr(Value: Integer): string;
begin
  Str(Value, Result);
end;
{******************************************************************************}
function SameText(const S1, S2: string): Boolean;
var
  i, len: Integer;
begin
  Result := False;
  len := Length(S1);
  if len = Length(S2) then
  begin
    for i := 1 to len do
      if UpCase(S1[i]) <> UpCase(S2[i]) then
        Exit;
    Result := True;
  end;
end;
{******************************************************************************}
function StartsText(const SubStr, S: string): Boolean;
var
  i, len: Integer;
begin
  Result := False;
  len := Length(SubStr);
  if len <= Length(S) then
  begin
    for i := 1 to len do
      if UpCase(SubStr[i]) <> UpCase(S[i]) then
        Exit;
    Result := True;
  end;
end;
{******************************************************************************}
function GetEnvironmentVariable(const Name: string): string;
begin
  SetLength(Result, 8 * 1024);
  SetLength(Result, Windows.GetEnvironmentVariable(PChar(Name), PChar(Result), Length(Result)));
end;
{******************************************************************************}
function FileExists(const Filename: string): Boolean;
var
  attr: Cardinal;
begin
  attr := GetFileAttributes(PChar(Filename));
  Result := (attr <> $FFFFFFFF) and (attr and FILE_ATTRIBUTE_DIRECTORY = 0);
end;
{******************************************************************************}
function Execute(const Cmd: string): Integer;
var
  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfo;
begin
  StartupInfo.cb := SizeOf(StartupInfo);
  GetStartupInfo(StartupInfo);
  if CreateProcess(nil, PChar(Cmd), nil, nil, True, 0, nil,
    PChar(ExtractFileDir(ParamStr(0))), StartupInfo, ProcessInfo) then
  begin
    CloseHandle(ProcessInfo.hThread);
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess, Cardinal(Result));
    CloseHandle(ProcessInfo.hProcess);
  end
  else
    Result := -1;
end;
{******************************************************************************}
function GetWindowsDir: string;
begin
  SetLength(Result, MAX_PATH);
  SetLength(Result, GetWindowsDirectory(PChar(Result), Length(Result)));
end;
{******************************************************************************}
function GetSystemDir: string;
begin
  SetLength(Result, MAX_PATH);
  SetLength(Result, GetSystemDirectory(PChar(Result), Length(Result)));
end;
{******************************************************************************}

{ a very small XML parser }
type
  IAttr = interface
    function Name: string;
    function Value: string;
  end;

  ITag = interface
    function Name: string;
    function Attrs(const Name: string): IAttr;
  end;

  TXmlFile = class(TObject)
  private
    FText: string;
    FPosition: Integer;
  public
    constructor Create(const Filename: string);
    function NextTag: ITag;
  end;

  TTag = class(TInterfacedObject, ITag)
  private
    FText: string;
  public
    constructor Create(const AText: string);
    function Name: string;
    function Attrs(const Name: string): IAttr;
  end;

  TAttr = class(TInterfacedObject, IAttr)
  private
    FText: string;
  public
    constructor Create(const AText: string);
    function Name: string;
    function Value: string;
  end;

{******************************************************************************}
{ TXmlFile }

constructor TXmlFile.Create(const Filename: string);
var
  f: file of Byte;
begin
  inherited Create;
  FileMode := 0;
  AssignFile(f, Filename);
  Reset(f);
  SetLength(FText, FileSize(f));
  BlockRead(f, FText[1], FileSize(f));
  CloseFile(f);
  FPosition := 0;
end;
{******************************************************************************}
function TXmlFile.NextTag: ITag;
var
  F, P: PChar;
  InStr1, InStr2: Boolean;
  S: string;
begin
  InStr1 := False;
  InStr2 := False;
  if FPosition >= Length(FText) then
  begin
    Result := nil;
    Exit;
  end;

  P := PChar(FText) + FPosition;
  while (P[0] <> #0) and (P[0] <> '<') do
    Inc(P);
  if P[0] <> #0 then
  begin
    if P[1] = '!' then // comment
    begin
      while (P[0] <> #0) do
      begin
        if (P[0] = '-') and (P[1] = '-') and (P[2] = '>') then
          Break;
        Inc(P);
      end;
      FPosition := P - PChar(FText);
      Result := NextTag;
      Exit;
    end;
    F := P;
    while True do
    begin
      case P[0] of
        #0:
          Break;
        '>':
          if not (InStr1 or InStr2) then
          begin
            SetString(S, F + 1, P - F - 1);
            Result := TTag.Create(S);
            Inc(P);
            Break;
          end;
        '''':
          InStr1 := not InStr1;
        '"':
          InStr2 := not InStr2;
      end;
      Inc(P);
    end;
  end;
  FPosition := P - PChar(FText);
end;
{******************************************************************************}
{ TTag }

constructor TTag.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
end;
{******************************************************************************}
function TTag.Name: string;
var
  ps: Integer;
begin
  ps := Pos(' ', FText);
  if ps = 0 then
    Result := FText
  else
    Result := Copy(FText, 1, ps - 1);
end;
{******************************************************************************}
function TTag.Attrs(const Name: string): IAttr;
var
  ps: Integer;
  InStr1, InStr2: Boolean;
  F, P: PChar;
  S: string;
begin
  Result := TAttr.Create('');
  ps := Pos(' ', FText);
  if ps = 0 then
    Exit;
  P := PChar(FText) + ps;
  while P[0] <> #0 do
  begin
    while P[0] in [#1..#32] do
      Inc(P);
    if P[0] = #0 then
      Break;
    F := P;
    InStr1 := False;
    InStr2 := False;
    while True do
    begin
      case P[0] of
        #0, #9, #32, '/':
          if not (InStr1 or InStr2) or (P[0] = #0) then
          begin
            SetString(S, F, P - F);
            Result := TAttr.Create(S);
            if SameText(Result.Name, Name) then
              Exit;
            Inc(P);
            Break;
          end;
        '''':
          InStr1 := not InStr1;
        '"':
          InStr2 := not InStr2;
      end;
      Inc(P);
    end;
  end;
  Result := TAttr.Create('');
end;
{******************************************************************************}
{ TAttr }

constructor TAttr.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
end;
{******************************************************************************}
function TAttr.Name: string;
var
  ps: Integer;
begin
  ps := Pos('=', FText);
  if ps = 0 then
    Result := FText
  else
    Result := Copy(FText, 1, ps - 1);
end;
{******************************************************************************}
function TAttr.Value: string;
var
  ps: Integer;
begin
  ps := Pos('=', FText);
  if ps = 0 then
    Result := ''
  else
  begin
    Result := Copy(FText, ps + 1, MaxInt);
    if (Result <> '') and (Result[1] in ['''', '"']) then
    begin
      Delete(Result, 1, 1);
      Delete(Result, Length(Result), 1);
    end;
  end;
end;
{******************************************************************************}
function AsterixMacro(const S, AsterixRepl: string): string;
var
  I: Integer;
begin
  Result := S;
  I := Pos('*', Result);
  if I > 0 then
  begin
    Delete(Result, I, 1);
    Insert(AsterixRepl, Result, I);
  end;
end;
{******************************************************************************}
procedure LoadTargetNames;
var
  xml: TXmlFile;
  tg: ITag;
begin
  xml := TXmlFile.Create(LibraryRootDir + '\' + pgEditFile);
  try
    tg := xml.NextTag;
    while tg <> nil do
    begin
      if SameText(tg.Name, 'model') and SameText(tg.Attrs('name').Value, LibraryName) then
      begin
        tg := xml.NextTag;
        while not SameText(tg.Name, 'targets') do
          tg := xml.NextTag;
        while not SameText(tg.Name, '/targets') do
        begin
          if SameText(tg.Name, 'target') then
          begin
            if FileExists(LibraryRootDir + '\packages\' + AsterixMacro(PackageGroupName, tg.Attrs('name').Value) + '.bpg') or
               FileExists(LibraryRootDir + '\packages\' + AsterixMacro(PackageGroupName, tg.Attrs('name').Value) + '.bdsgroup') then
            begin
              SetLength(Targets, Length(Targets) + 1); // we do not have 10tnds iterations so this is acceptable
              with Targets[High(Targets)] do
              begin
                Name := tg.Attrs('name').Value;
                PerName := tg.Attrs('pname').Value;
                PerDir := tg.Attrs('pdir').Value;
              end;
            end;
          end;
          tg := xml.NextTag;
        end;
        Break; // we only want the "LibraryName" part
      end;
      tg := xml.NextTag;
    end;
  finally
    xml.Free;
  end;
end;
{******************************************************************************}
{ TEdition }

constructor TEdition.Create(const AEditionName, PerDirName: string);
var
  Index: Integer;
begin
  if UpCase(AEditionName[1]) = 'D' then
    Typ := Delphi
  else
    Typ := BCB;

  VersionStr := AEditionName[2];
  if (Length(AEditionName) > 2) and (AEditionName[3] in ['0'..'9']) then
  begin
    VersionStr := VersionStr + AEditionName[3];
    Index := 4;
  end
  else
    Index := 3;

  Version := StrToInt(VersionStr);
  IDEVersionStr := VersionStr;
  IDEVersion := Version;

  if Version > 7 then
  begin
    Typ := BDS;
    IDEVersion := Version - 6; // D 8 = BDS 2
    IDEVersionStr := IntToStr(IDEVersion);
  end;

  FMainName := Copy(AEditionName, 1, Index - 1);
  FName := AEditionName;
  PkgDir := AEditionName;

  FIsCLX := SameText('clx', Copy(AEditionName, Index, 3));
  FIsPersonal := False;
  if Length(AEditionName) > Index then
  begin
    if (UpCase(AEditionName[Index]) = 'P') or (UpCase(AEditionName[Index]) = 'S') then
    begin
      FIsPersonal := True;
      PkgDir := PerDirName
    end;
  end;

  ReadRegistryData;
end;
{******************************************************************************}
procedure TEdition.ReadRegistryData;
var
  KeyName: string;
  Reg: HKEY;
  RegTyp: LongWord;
  ProjectsDir: string;

  function ReadStr(const Name: string): string;
  var
    Len: Longint;
  begin
    Len := MAX_PATH;
    SetLength(Result, MAX_PATH);
    RegQueryValueEx(Reg, PChar(Name), nil, @RegTyp, PByte(Result), @Len);
    SetLength(Result, StrLen(PChar(Result)));
  end;

  function ResolveMacros(const Dir: string): string;
  var
    ps, psEnd: Integer;
    S: string;
  begin
    if StartsText('$(DELPHI)', Dir) then
      Result := FRootDir + Copy(Dir, 10, MaxInt)
    else if StartsText('$(BCB)', Dir) then
      Result := FRootDir + Copy(Dir, 7, MaxInt)
    else if StartsText('$(BDS)', Dir) then
      Result := FRootDir + Copy(Dir, 7, MaxInt)
    else if StartsText('$(BDSPROJECTSDIR)', Dir) then
      Result := GetBDSProjectsDir + Copy(Dir, 18, MaxInt)
    else
    begin
      Result := Dir;
      ps := Pos('$(', Result);
      if ps > 0 then
      begin
        psEnd := Pos(')', Result);
        if psEnd > 0 then
        begin
          S := Copy(Result, ps + 2, psEnd - ps - 2);
          if S <> '' then
          begin
            Delete(Result, ps, 2 + Length(S) + 1);
            Insert(GetEnvironmentVariable(S), Result, ps);
          end
        end;
      end;
    end
  end;

begin
  case Typ of
    Delphi:
      KeyName := 'Software\Borland\Delphi\' + IDEVersionStr + '.0';
    BCB:
      KeyName := 'Software\Borland\C++Builder\' + IDEVersionStr + '.0';
    BDS:
      KeyName := 'Software\Borland\BDS\' + IDEVersionStr + '.0';
  end;

  if RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar(KeyName), 0, KEY_QUERY_VALUE or KEY_READ, Reg) = ERROR_SUCCESS then
  begin
    FRootDir := ExcludeTrailingPathDelimiter(ReadStr('RootDir'));
    RegCloseKey(Reg);
  end;

  if Typ = BDS then
    ProjectsDir := GetBDSProjectsDir
  else
    ProjectsDir := FRootDir + '\Projects';

  FDcpDir := FRootDir + '\Projects\Bpl';
  FBplDir := FRootDir + '\Projects\Bpl';
  if Typ = BCB then
    FLibDir := FRootDir + '\Projects\Lib'
  else
    FLibDir := FRootDir + '\Projects\Bpl';

  if RegOpenKeyEx(HKEY_CURRENT_USER, PChar(KeyName + '\Library'), 0, KEY_QUERY_VALUE or KEY_READ, Reg) = ERROR_SUCCESS then
  begin
    FDcpDir := ResolveMacros(ExcludeTrailingPathDelimiter(ReadStr('Package DCP Output')));
    FBplDir := ResolveMacros(ExcludeTrailingPathDelimiter(ReadStr('Package DPL Output')));
    RegCloseKey(Reg);
  end;
end;
{******************************************************************************}
function TEdition.GetBDSProjectsDir: string;
var
  h: HMODULE;
  LocaleName: array[0..4] of Char;
  Filename: string;
  PersDir: string;
begin
  if (Typ = BDS) and (IDEVersion >= Low(BDSVersions)) and (IDEVersion <= High(BDSVersions)) then
  begin
    Result := 'Borland Studio Projects'; // do not localize

    FillChar(LocaleName, SizeOf(LocaleName[0]), 0);
    GetLocaleInfo(GetThreadLocale, LOCALE_SABBREVLANGNAME, LocaleName, SizeOf(LocaleName));
    if LocaleName[0] <> #0 then
    begin
      Filename := RootDir + '\Bin\coreide' + BDSVersions[IDEVersion].CIV + '.';
      if FileExists(Filename + LocaleName) then
        Filename := Filename + LocaleName
      else
      begin
        LocaleName[2] := #0;
        if FileExists(Filename + LocaleName) then
          Filename := Filename + LocaleName
        else
          Filename := '';
      end;

      if Filename <> '' then
      begin
        h := LoadLibraryEx(PChar(Filename), 0,
          LOAD_LIBRARY_AS_DATAFILE or DONT_RESOLVE_DLL_REFERENCES);
        if h <> 0 then
        begin
          SetLength(Result, 1024);
          SetLength(Result, LoadString(h, BDSVersions[IDEVersion].ProjectDirResId, PChar(Result), Length(Result) - 1));
          FreeLibrary(h);
        end;
      end;
    end;

    SetLength(PersDir, MAX_PATH);
    if SHGetSpecialFolderPath(0, PChar(PersDir), CSIDL_PERSONAL, False) then
    begin
      SetLength(PersDir, StrLen(PChar(PersDir)));
      Result := ExcludeTrailingPathDelimiter(PersDir) + '\' + Result;
    end
    else
      Result := '';
  end
  else
    Result := '';
end;
{******************************************************************************}
procedure FindDxgettext(Version: Integer);
var
  reg: HKEY;
  len: Longint;
  RegTyp: LongWord;
  i: Integer;
  S: string;
begin
 // dxgettext detection
  if RegOpenKeyEx(HKEY_CLASSES_ROOT, 'bplfile\Shell\Extract strings\Command', 0, KEY_QUERY_VALUE or KEY_READ, reg) <> ERROR_SUCCESS then
    Exit;
  SetLength(S, MAX_PATH);
  len := MAX_PATH;
  RegQueryValueEx(reg, '', nil, @RegTyp, PByte(S), @len);
  SetLength(S, StrLen(PChar(S)));
  RegCloseKey(reg);

  if S <> '' then
  begin
    if S[1] = '"' then
    begin
      Delete(S, 1, 1);
      i := 1;
      while (i <= Length(S)) and (S[i] <> '"') do
        Inc(i);
      SetLength(S, i - 1);
    end;
    S := ExtractFileDir(S);
    DxgettextDir := S;
    if not FileExists(DxgettextDir + '\msgfmt.exe') then
      DxgettextDir := ''
    else
    begin
      if Version = 5 then
        S := S + '\delphi5';
      ExtraUnitDirs := ExtraUnitDirs + ';' + S;
    end;
  end;
end;
{******************************************************************************}
function TargetIndexOfEdition(const ed: string): Integer;
begin
  for Result := 0 to High(Targets) do
    if SameText(Targets[Result].Name, ed) or SameText(Targets[Result].PerName, ed) then
      Exit;
  Result := -1;
end;
{******************************************************************************}
procedure AddEdition(const ed: string);
var
  I: Integer;
begin
  if ed = '' then
    Exit;
  if SameText(ed, 'k3') then // build.exe is for Windows only (maybe CrossKylix)
    Exit;
  for I := 0 to High(Editions) do
    if SameText(Editions[i].Name, ed) then
      Exit;

  I := TargetIndexOfEdition(ed);
  if I >= 0 then
  begin
    SetLength(Editions, Length(Editions) + 1);
    Editions[High(Editions)] := TEdition.Create(ed, Targets[I].PerDir);
  end;
end;
{******************************************************************************}
procedure AddAllEditions(AddPersonal: Boolean);
var
  i: Integer;
begin
  Editions := nil;
  for i := 0 to High(Targets) do
  begin
    AddEdition(Targets[i].Name);
    if AddPersonal then
      AddEdition(Targets[i].PerName);
  end;
end;
{******************************************************************************}
function GetNewestEdition: TEdition;
var
  I: Integer;
  ed: TEdition;
begin
  Result := TEdition.Create('d5', '');
  for I := High(Targets) downto 0 do
  begin
    ed := TEdition.Create(Targets[I].Name, Targets[I].PerDir);
    try
      if ed.Version >= Result.Version then
      begin
        if (Result.Version < ed.Version) or
           { prefere Delphi version instead of C++Builder version: }
           ((Result.Typ = BCB) and (ed.Typ <> BCB)) then
        begin
          if ed.IsCLX then
            Continue; // this is not a valid version

          if (ed.RootDir <> '') and FileExists(ed.RootDir + '\bin\dcc32.exe') then
          begin
            Result.Free;
            Result := ed;
            ed := nil;
          end
        end;
      end;
    finally
      ed.Free;
    end;
  end;
end;
{******************************************************************************}
function GetNewestEditionName: string;
var
  ed: TEdition;
begin
  ed := GetNewestEdition;
  try
    if ed <> nil then
      Result := ed.Name
    else
      Result := '';
  finally
    ed.Free;
  end;
end;
{******************************************************************************}
procedure AddNewestEdition;
begin
  Editions := nil;
  AddEdition(GetNewestEditionName);
end;
{******************************************************************************}
procedure Help;
var
  I: Integer;
begin
  AddAllEditions(True);
  WriteLn('build.exe setups the environment for the given targets and executes the');
  WriteLn('make file that does the required actions.');
  WriteLn;
  WriteLn('build.exe [TARGET] [OPTIONS]');
  WriteLn('  TARGETS:');

  Write('    ');
  for I := 0 to High(Editions) - 1 do
    Write(Editions[I].Name, ', ');
  if Length(Editions) > 0 then
    WriteLn(Editions[High(Editions)].Name);
  //WriteLn('    c5, c6, c6p, d5, d5s, d6, d6p, d7, d7p, d7clx, d9');

  WriteLn;
  WriteLn('  OPTIONS:');
  WriteLn('    --make=X        X will be added to the make command line.');
  WriteLn('    --dcc-opt=X     sets the DCCOPT environment variable to X.');
  WriteLn('    --bpl-path=X    sets the BPLDIR and DCPDIR environment variable to X.');
  WriteLn('    --lib-path=X    sets the LIBDIR environment variable to X (BCB only).');
  WriteLn('    --hpp-path=X    sets the HPPDIR environment variable to X (BCB only).');
  WriteLn('                      Defaults to $(ROOT)\Include\Vcl');
  WriteLn('                      Set this to an empty string if you want the hpp files to');
  WriteLn('                      be left in the same directory as their source pas file.');

  for I := 0 to High(ExtraOptions) do
    if ExtraOptions[I].Name <> '' then
      WriteLn('    --', ExtraOptions[I].Name, '=X    sets the ', ExtraOptions[I].Env, ' environment variable to X.');

  WriteLn('    --targets=X     sets the TARGETS environment variable to X. Only these .bpl');
  WriteLn('                    files will be compiled.');
  WriteLn('                    (Example:');
  WriteLn('                      buildtarget "--targets=JvCoreD7R.bpl JvCoreD7R.bpl" )');
  WriteLn;
  WriteLn('    --build         forces the Delphi compiler to build the targets.');
  WriteLn('    --force         Compile/Generate even if the target is not installed.');
  WriteLn('    --verbose       Show all commands that are executed.');
  WriteLn;
end;
{******************************************************************************}
procedure ProcessArgs;
var
  i, j, Count: Integer;
  S: string;
  HppPathSet: Boolean;
begin
  i := 1;
  Count := ParamCount;
  HppPathSet := False;
  while i <= Count do
  begin
    S := ParamStr(i);
    if S[1] = '-' then
    begin
      if StartsText('--make=', S) then
      begin
        Delete(S, 1, 7);
        if S <> '' then
          if Pos(' ', S) > 0 then
            MakeOptions := MakeOptions + ' "' + S + '"'
          else
            MakeOptions := MakeOptions + ' ' + S;
      end
      else if StartsText('--dcc-opt=', S) then
      begin
        Delete(S, 1, 10);
        DccOpt := S;
      end
      else if StartsText('--bpl-path=', S) then
      begin
        Delete(S, 1, 11);
        UserBplDir := S;
        UserDcpDir := S;
      end
      else if StartsText('--lib-path=', S) then
      begin
        Delete(S, 1, 11);
        UserLibDir := S;
      end
      else if StartsText('--hpp-path=', S) then
      begin
        Delete(S, 1, 11);
        SetEnvironmentVariable('HPPDIR', Pointer(S));
        HppPathSet := True;
      end
      else if StartsText('--targets=', S) then
      begin
        Delete(S, 1, 10);
        SetEnvironmentVariable('TARGETS', Pointer(S));
      end
      else if SameText(S, '--build') then
      begin
        DccOpt := DccOpt + ' -B';
      end
      else if SameText('--force', S) then
      begin
        Force := True;
      end
      else if SameText('--verbose', S) then
      begin
        Verbose := True;
      end
      else
      begin
        for j := 0 to High(ExtraOptions) do
        begin
          if (ExtraOptions[I].Name <> '') and StartsText('--' + ExtraOptions[j].Name + '=', S) then
          begin
            Delete(S, 1, 2 + Length(ExtraOptions[j].Name) + 1);
            SetEnvironmentVariable(PChar(ExtraOptions[j].Env), Pointer(S));
          end;
        end
      end;
    end
    else
    begin
      if SameText(S, 'all') then
      begin
        AddAllEditions(False);
      end
      else if SameText(S, 'newest') then
      begin
        AddNewestEdition;
        WriteLn('Using ', GetNewestEditionName, ' for build process.');
        WriteLn;
      end
      else if TargetIndexOfEdition(S) = -1 then
      begin
        WriteLn('Unknown edition: ', S);
        Halt(1);
      end
      else
        AddEdition(S);
    end;
    Inc(i);
  end;
  if not HppPathSet then
    SetEnvironmentVariable('HPPDIR', '$(ROOT)\Include\Vcl');
end;
{******************************************************************************}
procedure ClearEnvironment;
{ ClearEnvironment deletes almost all environment variables }
var
  EnvP, P, StartP: PChar;
  S: string;
begin
  EnvP := GetEnvironmentStrings;
  if EnvP <> nil then
  begin
    try
      P := EnvP;
      StartP := P;
      repeat
        while P^ <> #0 do
          Inc(P);
        if P^ = #0 then
        begin
          SetString(S, StartP, P - StartP);
          S := Copy(S, 1, Pos('=', S) - 1);
          if S <> '' then
          begin
            { Delete the environment variable }
            if not (
              SameText(S, 'TEMP') or  SameText(S, 'ComSpec') or SameText(S ,'OS') or
              SameText(S, 'PATHEXT') or SameText(S, 'windir') or SameText(S, 'SystemRoot') or
              SameText(S, 'SystemDrive') or
              SameText(S, 'INSTALLOPTIONS') or SameText(S, 'LANG')
              ) then
             SetEnvironmentVariable(PChar(S), nil);
          end;

          Inc(P);
          if P^ = #0 then
            Break; // finished

          StartP := P;
        end;
      until False;
    finally
      FreeEnvironmentStrings(EnvP);
    end;
  end;
end;
{******************************************************************************}
function GetLibraryRootDir: string;
var
  I: Integer;
begin
  Result := ExtractFileDir(ParamStr(0));
  for I := 1 to LibraryRootDirRelativeToBuild do
    Result := ExtractFileDir(Result);
end;
{******************************************************************************}
function ExtractShortPathName(const Path: string): string;
begin
  SetLength(Result, MAX_PATH);
  SetLength(Result, GetShortPathName(PChar(Path), PChar(Result), Length(Result)));
end;
{******************************************************************************}

var
  I: Integer;
  UnitOutDir, Path: string;
  Edition: TEdition;
begin
  LibraryRootDir := GetLibraryRootDir;
  // ClearEnvironment; // remove almost all environment variables for "make.exe long command line"
  // ahuser (2005-01-22): make.exe fails only if a path with spaces is in the PATH envvar

  // set ExtraOptions default values
  for I := 0 to High(ExtraOptions) do
    if ExtraOptions[I].Name <> '' then
      SetEnvironmentVariable(PChar(ExtraOptions[I].Env), Pointer(ExtraOptions[I].Default));
  SetEnvironmentVariable(PChar(LibraryName + 'ROOT'), PChar(LibraryRootDir));

  UserBplDir := '';
  UserDcpDir := '';
  UserLibDir := '';

  LoadTargetNames;
  ProcessArgs;

  if Length(Editions) = 0 then
  begin
    Help;
    Halt(1);
  end;
  if not Verbose then
  begin
    MakeOptions := ' -s' + MakeOptions;
    SetEnvironmentVariable('QUIET', '-s');
  end
  else
    SetEnvironmentVariable('QUIET', nil);

  for I := 0 to High(Editions) do
  begin
    ExtraUnitDirs := '';

    Edition := Editions[I];
    if Length(Editions) > 1 then
      WriteLn('################################ ' + Edition.Name + ' #########################################');

    // test for valid root directory/valid IDE installation
    if not Force then
    begin
      if Edition.RootDir = '' then
      begin
        WriteLn('Delphi/BCB version not installed.');
        Continue;
      end;
    end
    else
    begin
      if Edition.RootDir = '' then
        Edition := GetNewestEdition;
      if Edition.RootDir = '' then
      begin
        WriteLn('No Delphi/BCB version installed.');
        Continue;
      end;
    end;

    UnitOutDir := LibraryRootDir + '\lib\' + Edition.MainName;
    if UserDcpDir = '' then
      UserDcpDir := Edition.DcpDir;
    if UserBplDir = '' then
      UserBplDir := Edition.BplDir;
    if UserLibDir = '' then
      UserLibDir := Edition.LibDir;

    FindDxgettext(Edition.Version);

    // setup environment and execute make.exe
    Path := GetWindowsDir + ';' + GetSystemDir + ';' + GetWindowsDir + '\Command';
    if UserLibDir <> UserBplDir then
      Path := ExtractShortPathName(Edition.RootDir) + '\bin;' + ExtractShortPathName(UserBplDir) + ';' + ExtractShortPathName(UserLibDir) + ';' + Path
    else
      Path := ExtractShortPathName(Edition.RootDir) + '\bin;' + ExtractShortPathName(UserBplDir) + ';' + Path;
    { Add original BPL directory for "common" BPLs, but add it as the very last
      path to prevent collisions between packages in TargetConfig.BplDir and
      Target.BplDir. }
    Path := Path + ';' + ExtractShortPathName(Edition.BplDir);

    SetEnvironmentVariable('PATH', Pointer(Path));

    SetEnvironmentVariable('MAINBPLDIR', Pointer(Edition.BplDir));
    SetEnvironmentVariable('MAINDCPDIR', Pointer(Edition.DcpDir));
    SetEnvironmentVariable('BPLDIR', Pointer(UserBplDir));
    SetEnvironmentVariable('DCPDIR', Pointer(UserDcpDir));
    SetEnvironmentVariable('LIBDIR', Pointer(UserLibDir));
    SetEnvironmentVariable('BPILIBDIR', Pointer(UserLibDir));
    SetEnvironmentVariable('PERSONALEDITION_OPTION', nil);
    SetEnvironmentVariable('ROOT', PChar(Edition.RootDir));
    SetEnvironmentVariable('VERSION', PChar(Edition.VersionStr));
    SetEnvironmentVariable('UNITOUTDIR', PChar(UnitOutDir));
    SetEnvironmentVariable('DCCOPT', Pointer(DccOpt));
    SetEnvironmentVariable('DCC', PChar('"' + Edition.RootDir + '\bin\dcc32.exe" ' + DccOpt));

    if Edition.IsPersonal then
    begin
      SetEnvironmentVariable('PERSONALEDITION_OPTION', '-DDelphiPersonalEdition');
      SetEnvironmentVariable('PKGDIR', PChar(Edition.PkgDir));
      SetEnvironmentVariable('EDITION', PChar(Edition.MainName));
      if Verbose then
        Execute('"' + Edition.RootDir + '\bin\make.exe" -f makefile.mak pg.exe')
      else
        Execute('"' + Edition.RootDir + '\bin\make.exe" -s -f makefile.mak pg.exe');
    end;

    SetEnvironmentVariable('EDITION', PChar(Edition.Name));
    SetEnvironmentVariable('PKGDIR', PChar(Edition.PkgDir));

    if (ExtraUnitDirs <> '') and (ExtraUnitDirs[1] = ';') then
      Delete(ExtraUnitDirs, 1, 1);
    SetEnvironmentVariable('EXTRAUNITDIRS', Pointer(ExtraUnitDirs));
    SetEnvironmentVariable('DXGETTEXTDIR', Pointer(DxgettextDir));


    ExitCode := Execute('"' + Edition.RootDir + '\bin\make.exe" ' + MakeOptions);
    if ExitCode <> 0 then
    begin
      if ExitCode < 0 then
        WriteLn('Failed: ', '"' + Edition.RootDir + '\bin\make.exe" ' + MakeOptions);
      WriteLn('Press ENTER to continue');
      ReadLn;
    end;
  end;
end.

