program VersionBuilder;

{$APPTYPE CONSOLE}

uses
  Windows, Classes, IniFiles, SysUtils,
  JclMiscel;

const
  VersionScriptFileName = 'VersionScript.txt';
  TempRCFileName        = 'Temp.rc';

  GlobalSectionName = 'GLOBAL';
  RootSectionName   = 'ROOT';

procedure Error(const Text: string);
begin
  WriteLn(Text);
  Beep;
  Halt(1);
end;

procedure BuildVersionInfo(const ScriptName: string);
var
  Script: TIniFile;
  Sections, GlobalVerInfo, RootInfo, TempSection, VersionInfo: TStringList;
  RootFolder, ResFileName: string;
  I, N: Integer;

  procedure BuildResFile;
  var
    RCFile: TStringList;
    I: Integer;
    CmdLine, FileVersion: string;
  begin
    RCFile := TStringList.Create;
    try
      FileVersion := VersionInfo.Values['FileVersion'];
      if FileVersion = '' then
        Error('* Error, unspecified FileVersion');
      FileVersion := Trim(StringReplace(FileVersion, '.', ', ', [rfReplaceAll]));
      RCFile.Add('1 VERSIONINFO');
      RCFile.Add('FILEVERSION ' + FileVersion);
      RCFile.Add('PRODUCTVERSION ' + FileVersion);
      RCFile.Add('FILEOS VOS__WINDOWS32');
      RCFile.Add('FILETYPE VFT_DLL');
      RCFile.Add('{');
      RCFile.Add(' BLOCK "StringFileInfo"');
      RCFile.Add(' {');
      RCFile.Add('  BLOCK "040904E4"');
      RCFile.Add('  {');
      for I := 0 to VersionInfo.Count - 1 do
        RCFile.Add(Format('    VALUE "%s", "%s\000"', [VersionInfo.Names[I], VersionInfo.ValueFromIndex[I]]));
      RCFile.Add('  }');
      RCFile.Add(' }');
      RCFile.Add(' BLOCK "VarFileInfo"');
      RCFile.Add(' {');
      RCFile.Add('  VALUE "Translation", 0x409, 0x4E4');
      RCFile.Add(' }');
      RCFile.Add('}');
      RCFile.SaveToFile(TempRCFileName);
      CmdLine := Format('BRCC32 -fo"%s" "%s"', [ResFileName, TempRCFileName]);
      I := WinExec32AndWait(CmdLine, SW_HIDE);
      DeleteFile(TempRCFileName);
      if I <> 0 then
        Error(Format('* Error code: %s', [I]));
    finally
      RCFile.Free;
    end;
  end;

begin
  Sections := nil;
  GlobalVerInfo := nil;
  RootInfo := nil;
  TempSection := nil;
  VersionInfo := nil;
  Script := TIniFile.Create(ScriptName);
  try
    Sections := TStringList.Create;
    GlobalVerInfo := TStringList.Create;
    RootInfo := TStringList.Create;
    TempSection := TStringList.Create;
    VersionInfo := TStringList.Create;
    Script.ReadSections(Sections);
    Script.ReadSectionValues(GlobalSectionName, GlobalVerInfo);
    Script.ReadSectionValues(RootSectionName, RootInfo);
    RootFolder := Trim(RootInfo.Values['Folder']);
    if RootFolder = '' then
      Error('* Error, Root folder is not specified');
    for I := 0 to Sections.Count - 1 do
    begin
      ResFileName := Sections[I];
      if Pos('\', ResFileName) = 0 then
        Continue;
      ResFileName := RootFolder + ResFileName;
      Script.ReadSectionValues(Sections[I], TempSection);
      if not FileExists(ResFileName) then
        Error(Format('* Error, file "%s" does not exists.', [ResFileName]));
      WriteLn(ResFileName);
      VersionInfo.Clear;
      VersionInfo.AddStrings(GlobalVerInfo);
      for N := 0 to TempSection.Count - 1 do
        VersionInfo.Values[TempSection.Names[N]] := TempSection.ValueFromIndex[N];
      for N := 0 to VersionInfo.Count - 1 do
        WriteLn(VersionInfo.Names[N], '=', VersionInfo.ValueFromIndex[N]);
      BuildResFile;
      WriteLn;
    end;
  finally
    Script.Free;
    Sections.Free;
    GlobalVerInfo.Free;
    RootInfo.Free;
    TempSection.Free;
    VersionInfo.Free;
  end;
end;

procedure Run;
begin
  WriteLn('Jedi version info RES file builder, version 0.1');
  BuildVersionInfo(ExpandFileName(VersionScriptFileName));
end;

begin
  Run;
end.
