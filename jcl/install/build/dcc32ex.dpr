{$A8,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W+,X+,Y+,Z1}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
program dcc32ex;

{$APPTYPE CONSOLE}

uses
  Windows;

var
  DxgettextDir: string;
  ExtraUnitDirs: string;
  UseSearchPaths: Boolean;
  RequireJcl: Boolean;
  RequireJvcl: Boolean;

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
function ExtractFileName(const S: string): string;
var
  ps: Integer;
begin
  ps := Length(S);
  while (ps > 1) and (S[ps] <> '\') do
    Dec(ps);
  Result := Copy(S, ps + 1, MaxInt);
end;
{******************************************************************************}
function ChangeFileExt(const Filename, NewExt: string): string;
var
  ps: Integer;
begin
  ps := Length(Filename);
  while (ps > 1) and (Filename[ps] <> '.') do
    Dec(ps);
  if ps > 0 then
    Result := Copy(Filename, 1, ps - 1) + NewExt
  else
    Result := Filename + NewExt;
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
function ExpandDirMacros(const Path, RootDir: string): string;
var
  i: Integer;
  Start, Len: Integer;
  NewS, S: string;
begin
  Result := Path;
  Len := Length(Result);
  i := 1;
  while i <= Len do
  begin
    if (Result[i] = '$') and (i < Len - 1) and (Result[i + 1] = '(') then
    begin
      Start := i;
      while (i <= Len) and (Result[i] <> ')') do
        Inc(i);
      if i <= Len then
      begin
        S := Copy(Result, Start + 2, i - Start - 2);

        if SameText(S, 'BDS') or SameText(S, 'BCB') or SameText(S, 'DELPHI') then
          NewS := ExtractFileDir(RootDir)
        else
          NewS := GetEnvironmentVariable(S);

        Delete(Result, Start, i - Start + 1);
        Insert(NewS, Result, Start);
        Dec(i, Length(S) + 3);
        Inc(i, Length(NewS));
        Len := Length(Result);
      end;
    end;
    Inc(i);
  end;
end;
{******************************************************************************}
function FileExists(const Filename: string): Boolean;
var
  Attr: Cardinal;
begin
  Attr := GetFileAttributes(PChar(Filename));
  Result := (Attr <> $FFFFFFFF) and (Attr and FILE_ATTRIBUTE_DIRECTORY = 0);
end;
{******************************************************************************}
function DirectoryExists(const Filename: string): Boolean;
var
  Attr: Cardinal;
begin
  Attr := GetFileAttributes(PChar(Filename));
  Result := (Attr <> $FFFFFFFF) and (Attr and FILE_ATTRIBUTE_DIRECTORY <> 0);
end;
{******************************************************************************}
function Execute(const Cmd, StartDir: string; HideOutput: Boolean): Integer;
var
  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfo;
begin
  StartupInfo.cb := SizeOf(StartupInfo);
  GetStartupInfo(StartupInfo);
  if HideOutput then
  begin
    StartupInfo.hStdOutput := 0;
    StartupInfo.hStdError := 0;
    StartupInfo.dwFlags := STARTF_USESTDHANDLES;
  end;
  if CreateProcess(nil, PChar(Cmd), nil, nil, True, 0, nil,
    Pointer(StartDir), StartupInfo, ProcessInfo) then
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
function GetTempDir: string;
begin
  SetLength(Result, MAX_PATH);
  SetLength(Result, GetTempPath(Length(Result), PChar(Result)));
  Result := ExcludeTrailingPathDelimiter(Result);
  if Result = '' then
    Result := ExcludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP'));
  if Result = '' then
    Result := '.';
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
      if ExtraUnitDirs <> '' then
        ExtraUnitDirs := ExtraUnitDirs + ';' + S
      else
        ExtraUnitDirs := S;
    end;
  end;
end;
{******************************************************************************}

type
  TTargetType = (ttNone, ttDelphi, ttBCB, ttBDS);
const
  ttFirst = ttDelphi;
  TargetNames: array[TTargetType] of PChar = (
    'none', 'Delphi', 'C++Builder', 'Delphi'
  );

type
  TTarget = record
    Typ: TTargetType;
    Version: Integer;
    IDEVersion: Integer;
    RootDir: string;
    LibDirs: string;
    SearchPaths: string;
    KeyName: string;
    Id: string; // ["d"|"c"]<version>
    InstalledJcl: Boolean;
    InstalledJvcl: Boolean;
  end;

function ReadTargetInfo(Typ: TTargetType; IDEVersion: Integer): TTarget;
var
  Reg: HKEY;
  RegTyp: LongWord;

  function ReadStr(const Name: string): string;
  var
    Len: Longint;
  begin
    Len := MAX_PATH;
    SetLength(Result, MAX_PATH);
    RegQueryValueEx(Reg, PChar(Name), nil, @RegTyp, PByte(Result), @Len);
    SetLength(Result, StrLen(PChar(Result)));
  end;

var
  IDEVersionStr: string;
  JediLibDirs, Dir, DcpDir, RootDir: string;
begin
  Result.Typ := ttNone;
  Result.Version := 0;
  Result.IDEVersion := 0;
  Result.RootDir := '';
  Result.KeyName := '';
  Result.Id := '';
  Result.InstalledJcl := False;
  Result.InstalledJvcl := False;

  Str(IDEVersion, IDEVersionStr);
  case Typ of
    ttDelphi:
      begin
        Result.KeyName := 'Software\Borland\Delphi\' + IDEVersionStr + '.0';
        Result.Id := 'd';
      end;
    ttBCB:
      begin
        Result.KeyName := 'Software\Borland\C++Builder\' + IDEVersionStr + '.0';
         Result.Id := 'c';
      end;
    ttBDS:
      begin
        Result.KeyName := 'Software\Borland\BDS\' + IDEVersionStr + '.0';
        Result.Id := 'd';
      end;
  end;

  if RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar(Result.KeyName), 0,
                  KEY_QUERY_VALUE or KEY_READ, Reg) = ERROR_SUCCESS then
  begin
    Result.RootDir := ExcludeTrailingPathDelimiter(ReadStr('RootDir'));
    RegCloseKey(Reg);
    if Result.RootDir = '' then
      Exit;
    Result.Version := IDEVersion;
    if Typ = ttBDS then
    begin
      if IDEVersion <= 2 then // C#Builder 1 and Delphi 8 can't build the installer
      begin
        Result.Typ := ttNone;
        Result.Version := 0;
        Result.IDEVersion := 0;
        Result.RootDir := '';
        Result.KeyName := '';
        Result.Id := '';
        Exit;
      end;
      Inc(Result.Version, 6); // 3.0 => 9
    end;
    Result.Typ := Typ;
    Result.IDEVersion := IDEVersion;
    Result.LibDirs := Result.RootDir + '\Lib';
    if Typ = ttBCB then
      Result.LibDirs := Result.LibDirs + ';' + Result.RootDir + '\Lib\Obj';
    Result.Id := Result.Id + IntToStr(Result.Version);


    if RegOpenKeyEx(HKEY_CURRENT_USER, PChar(Result.KeyName + '\Library'), 0,
                    KEY_QUERY_VALUE or KEY_READ, Reg) = ERROR_SUCCESS then
    begin
      Result.SearchPaths := ExpandDirMacros(ExcludeTrailingPathDelimiter(ReadStr('Search Path')), Result.RootDir);
      RegCloseKey(Reg);
      if UseSearchPaths then
        Result.LibDirs := Result.LibDirs + ';' + Result.SearchPaths;
    end;

    JediLibDirs := '';
    if RequireJcl and
       (RegOpenKeyEx(HKEY_CURRENT_USER, PChar(Result.KeyName + '\Jedi\JCL'), 0,
                     KEY_QUERY_VALUE or KEY_READ, Reg) = ERROR_SUCCESS) then
    begin
      DcpDir := ExcludeTrailingPathDelimiter(ExpandDirMacros(ExcludeTrailingPathDelimiter(ReadStr('DcpDir')), Result.RootDir));
      RootDir := ExcludeTrailingPathDelimiter(ExpandDirMacros(ExcludeTrailingPathDelimiter(ReadStr('RootDir')), Result.RootDir));
      RegCloseKey(Reg);
      Dir := RootDir + '\lib\' + Result.Id;
      if FileExists(Dir + '\JclBase.dcu') then
      begin
        if not SameText(Dir, DcpDir) then
          JediLibDirs := JediLibDirs + ';' + Dir + ';' + DcpDir
        else
          JediLibDirs := JediLibDirs + ';' + Dir;
        JediLibDirs := JediLibDirs + ';' +  RootDir + '\source';
        Result.InstalledJcl := True;
      end
      else if FileExists(RootDir + '\source\common\JclBase.pas') then 
      begin
        JediLibDirs := JediLibDirs + ';' + RootDir + '\source;' + RootDir + '\source\common;' + RootDir + '\source\vcl;' + RootDir + '\source\visclx';
        {$IFDEF WIN32}
        JediLibDirs := JediLibDirs + ';' + RootDir + '\source\windows';
        {$ELSE}
        JediLibDirs := JediLibDirs + ':' + RootDir + '\source\unix';
        {$ENDIF WINDOWS}
        Result.InstalledJcl := True;
      end;
    end;
    if RequireJvcl and
       (RegOpenKeyEx(HKEY_CURRENT_USER, PChar(Result.KeyName + '\Jedi\JVCL'), 0,
                     KEY_QUERY_VALUE or KEY_READ, Reg) = ERROR_SUCCESS) then
    begin
      DcpDir := ExcludeTrailingPathDelimiter(ExpandDirMacros(ExcludeTrailingPathDelimiter(ReadStr('DcpDir')), Result.RootDir));
      RootDir := ExcludeTrailingPathDelimiter(ExpandDirMacros(ExcludeTrailingPathDelimiter(ReadStr('RootDir')), Result.RootDir));
      RegCloseKey(Reg);
      Dir := RootDir + '\lib\' + Result.Id;
      if FileExists(Dir + '\JVCLVer.dcu') then
      begin
        if not SameText(Dir, DcpDir) then
          JediLibDirs := JediLibDirs + ';' + Dir + ';' + DcpDir
        else
          JediLibDirs := JediLibDirs + ';' + Dir;
        JediLibDirs := JediLibDirs + ';' +  RootDir + '\common;' + RootDir + '\Resources';
        Result.InstalledJvcl := True;
      end;
    end;
    if JediLibDirs <> '' then
      Result.LibDirs := Result.LibDirs + JediLibDirs; // leading ';' is already in JediLibDirs
  end
  else
  begin
    Result.KeyName := '';
    Exit;
  end;
end;
{******************************************************************************}
procedure TestDelphi6Update2(const Target: TTarget);
var
  f: TextFile;
  TestFilename: string;
  Status: Integer;
begin
  // Test for Delphi 6 Update 2
  TestFilename := GetTempDir + '\delphi6compiletest.dpr';
  AssignFile(f, Testfilename);
  {$I-}
  Rewrite(f);
  WriteLn(f, 'program delphi6compiletest;');
  WriteLn(f, 'uses Windows, Graphics;');
  WriteLn(f, 'begin');
  WriteLn(f, '  ExitCode := ');
  WriteLn(f, '  {$IF declared(clHotLight)}');
  WriteLn(f, '  0;');
  WriteLn(f, '  {$ELSE}');
  WriteLn(f, '  1;');
  WriteLn(f, '  {$IFEND}');
  WriteLn(f, 'end.');
  CloseFile(f);
  {$I+}
  if IOResult <> 0 then
  begin
    WriteLn(ErrOutput, 'Failed to write file ', TestFilename);
    DeleteFile(PChar(TestFilename));
  end
  else
  begin
    // compile <TestFilename>.dpr
    Status := Execute('"' + Target.RootDir + '\bin\dcc32.exe" ' +
                      '-Q -E. -N. -U"' + Target.LibDirs + '" ' + ExtractFileName(TestFilename),
                      ExtractFileDir(TestFilename), True);
    DeleteFile(PChar(TestFilename));
    if Status <> 0 then
    begin
      if Status = -1 then
        WriteLn(ErrOutput, 'Failed to start "', Target.RootDir, '\bin\dcc32.exe"')
      else
        ;//WriteLn(ErrOutput, 'Compilation of "', TestFilename, '" failed.');
      Halt(1);
    end;

    // start <TextFilename>.exe
    Status := Execute('"' + ChangeFileExt(TestFilename, '.exe') + '"',
                      ExtractFileDir(TestFilename), False);
    DeleteFile(PChar(ChangeFileExt(TestFilename, '.exe')));
    if Status <> 0 then
    begin
      if Status = -1 then
        WriteLn(ErrOutput, '"' + ChangeFileExt(TestFilename, '.exe') + '"')
      else
      begin
        WriteLn(ErrOutput, 'Delphi 6 Update 2 is not installed.');
        MessageBox(0, 'Delphi 6 Update 2 is not installed.', 'dcc32ex.exe', MB_ICONERROR or MB_OK);
      end;
      Halt(1);
    end;
  end;
end;

function SkipOption(CmdLine: PChar): PChar;
begin
  Result := CmdLine;
  if Result <> nil then
  begin
    if Result[0] = '"' then
    begin
      Inc(Result);
      while (Result[0] <> #0) and (Result[0] <> '"') do
        Inc(Result);
      if Result[0] = '"' then
        Inc(Result);
    end
    else
    begin
      while (Result[0] <> #0) and (Result[0] <> ' ') and (Result[0] <> #9) do
      begin
        if Result[0] = '"' then // embedded quotes:  -U"C:\Program Files\Borland\Delphi7\Lib"
        begin
          Inc(Result);
          while (Result[0] <> #0) and (Result[0] <> '"') do
            Inc(Result);
          if Result[0] = '"' then
            Inc(Result);
        end;
        Inc(Result);
      end;
      if Result[0] in [' ', #9] then
        Inc(Result);
    end;

    // skip whitespaces
    while Result[0] = ' ' do
      Inc(Result);

    if Result[0] = #0 then
      Result := nil;
  end;
end;

function ParseParams(CmdLine: PChar): PChar;
var
  S: string;
  i: Integer;
begin
  Result := CmdLine;
  while CmdLine <> nil do
  begin
    CmdLine := SkipOption(Result);
    if CmdLine = nil then
      S := Result
    else
      SetString(S, Result, CmdLine - Result);

    // delete right spaces
    i := Length(S);
    while (i > 0) and (S[i] = ' ') do
      Dec(i);
    if i <> Length(S) then
      S := Copy(S, 1, i);

    if (S <> '') and (S[1] = '"') then
      S := Copy(S, 2, Length(S) - 2);
    if SameText(S, '--use-search-paths') then
      UseSearchPaths := True
    else
    if SameText(S, '--requires-jcl') then
      RequireJcl := True
    else
    if SameText(S, '--requires-jvcl') then
      RequireJvcl := True
    else
      Break;
    Result := CmdLine;
  end;
end;

var
  Typ: TTargetType;
  IDEVersion: Integer;
  NewestTarget: TTarget;
  Target: TTarget;
  InvalidFound: Boolean;
  DependencyCheckFailed: Boolean;
  f: TextFile;
  Status: Integer;
  Dcc32Cfg, CurDir, ExtraOpts: string;
  CmdLine: PChar;
  DelphiVersion: string;
  PreferedTyp: TTargetType;
  PreferedVersion: Integer;
  PreferedTarget: TTarget;
  Err: Integer;
  ErrMsg: string;
begin
  CmdLine := GetCommandLine;
  CmdLine := SkipOption(CmdLine); // skip executable name
  CmdLine := ParseParams(CmdLine);
{  UseSearchPaths := GetEnvironmentVariable('UseSeachPaths') = '1';
  RequireJcl := GetEnvironmentVariable('RequireJCL') = '1';
  RequireJvcl := GetEnvironmentVariable('RequireJVCL') = '1';}


  PreferedTyp := ttNone;
  PreferedVersion := 0;
  DelphiVersion := GetEnvironmentVariable('DelphiVersion');
  if DelphiVersion <> '' then
  begin
    Val(Copy(DelphiVersion, 2, MaxInt), PreferedVersion, Err);
    if (Err = 0) and (PreferedVersion >= 5) then
    begin
      if DelphiVersion[1] in ['D', 'd'] then
        PreferedTyp := ttDelphi;
      if DelphiVersion[1] in ['C', 'c'] then
      begin
        if PreferedVersion <> 7 then
          PreferedTyp := ttBCB;
      end;
      if PreferedVersion > 7 then
        PreferedTyp := ttBDS;
    end;
  end;
  PreferedTarget.Typ := ttNone;

  NewestTarget.Typ := ttNone;
  DependencyCheckFailed := False;
  InvalidFound := False;
  for Typ := ttFirst to High(TTargetType) do
  begin
    for IDEVersion := 1 to 20 do
    begin
      Target := ReadTargetInfo(Typ, IDEVersion);
      if (Target.Typ <> ttNone) and (Target.Version >= 5) then
      begin
        // is the target valid
        if FileExists(Target.RootDir + '\bin\dcc32.exe') and
           (FileExists(Target.RootDir + '\lib\System.dcu') or FileExists(Target.RootDir + '\lib\obj\System.dcu')) then
        begin
          if (not RequireJcl or Target.InstalledJcl) and (not RequireJvcl or Target.InstalledJvcl) then
          begin
            if (NewestTarget.Typ = ttNone) or (NewestTarget.Version < Target.Version) then
              NewestTarget := Target;

            if (Target.Typ = PreferedTyp) and (Target.Version = PreferedVersion) then
              PreferedTarget := Target;
          end
          else
          begin
            WriteLn('Missing dependencies for ', TargetNames[Target.Typ], ' ', Target.Version);
            if RequireJcl and not Target.InstalledJcl then
              WriteLn(' - JCL  is required but not installed. (http://jcl.sourceforge.net)');
            if RequireJvcl and not Target.InstalledJvcl then
              WriteLn(' - JVCL is required but not installed. (http://jvcl.sourceforge.net)');
            WriteLn;
            DependencyCheckFailed := True;
            InvalidFound := True;
          end;
        end
        else
        begin
          WriteLn(TargetNames[Target.Typ], ' ', Target.Version, ' is no valid installation');
          if not DirectoryExists(Target.RootDir) then
            WriteLn(' - RootDir registry entry is not valid')
          else
          begin
            if not FileExists(Target.RootDir + '\bin\dcc32.exe') then
              WriteLn(' - dcc32.exe missing');
            if not (FileExists(Target.RootDir + '\lib\System.dcu') or FileExists(Target.RootDir + '\lib\obj\System.dcu')) then
              WriteLn(' - System.dcu missing');
          end;
          WriteLn;
          InvalidFound := True;
        end;
      end;
    end;
  end;

  if PreferedTarget.Typ <> ttNone then
    NewestTarget := PreferedTarget;

  if NewestTarget.Typ = ttNone then
  begin
    if InvalidFound then
    begin
      if DependencyCheckFailed then
        ErrMsg := 'Dependencies are missing. Please install them first.'
      else
        ErrMsg := 'No valid Delphi/BCB/BDS version found. Are your registry settings correct?';
    end
    else
      ErrMsg := 'No Delphi/BCB/BDS version installed.';
    WriteLn(ErrOutput, ErrMsg);
    MessageBox(0, PChar(ErrMsg), 'dcc32ex.exe', MB_ICONERROR or MB_OK);
    Halt(1);
  end;

  Target := NewestTarget;
  WriteLn('Using ', TargetNames[Target.Typ], ' ', Target.Version);
  if Target.Version = 6 then
    TestDelphi6Update2(Target);

  ExtraOpts := '';
  // dxgettext
  FindDxgettext(Target.Version);
  if ExtraUnitDirs <> '' then
  begin
    Target.LibDirs := Target.LibDirs + ';' + ExtraUnitDirs;
    ExtraOpts := ExtraOpts + '-DUSE_DXGETTEXT ';
  end;

  // start dcc32.exe
  GetDir(0, CurDir);
  CurDir := ExcludeTrailingPathDelimiter(CurDir);
  Dcc32Cfg := CurDir + '\dcc32.cfg';
  SetFileAttributes(PChar(Dcc32Cfg), FILE_ATTRIBUTE_NORMAL);
  AssignFile(f, Dcc32Cfg);
  {$I-}
  Rewrite(f);
  WriteLn(f, '-U"' + Target.LibDirs + '"');
  WriteLn(f, '-I"' + Target.LibDirs + '"');
  WriteLn(f, '-R"' + Target.LibDirs + '"');
  WriteLn(f, '-O"' + Target.LibDirs + '"');
  CloseFile(f);
  {$I+}
  if IOResult <> 0 then
  begin
    //WriteLn(ErrOutput, 'Failed to write file ', Dcc32Cfg);
    ExtraOpts := ExtraOpts + '-U"' + Target.LibDirs + '" -I"' + Target.LibDirs + '" -R"' + Target.LibDirs + '" -O"' + Target.LibDirs + '" ';
    DeleteFile(PChar(Dcc32Cfg));
    Dcc32Cfg := '';
  end;

  Status := Execute('"' + Target.RootDir + '\bin\dcc32.exe" ' + ExtraOpts + CmdLine, CurDir, False);
  if Dcc32Cfg <> '' then
    DeleteFile(PChar(Dcc32Cfg));

  if ParamCount = 0 then
  begin
    WriteLn;
    WriteLn('Additional options (must be specified before any dcc32 parameter):');
    WriteLn('  --use-search-paths     Use the IDE''s search paths');
    WriteLn('  --requires-jcl         Requires an installed JCL');
    WriteLn('  --requires-jvcl        Requires an installed JVCL');
    WriteLn;
    WriteLn('Environment variables:');
    WriteLn('  DELPHIVERSION = d10    Prefer this Delphi/BCB/BDS version');
    WriteLn('                         (d5, d6, d7, c5, c6, d9, d10, ...)');
  end;

  ExitCode := Status;
  {if DebugHook <> 0 then
    ReadLn;}
end.
