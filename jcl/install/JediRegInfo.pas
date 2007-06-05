{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JediInfo.pas, released on 2006-02-26.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2006 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JCL / JVCL
home page, located at http://jcl.sourceforge.net / http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

unit JediRegInfo;

{$I jedi.inc}

interface

uses
  SysUtils, Classes;

type
  TJediInformation = record
    Version: string; // example: '1.98'
    DcpDir: string;  // example: 'C:\Program Files\Borland\Delphi7\Projects\BPL', the JVCL Installer resolves macros
    BplDir: string;  // example: 'C:\Program Files\Borland\Delphi7\Projects\BPL', the JVCL Installer resolves macros
    RootDir: string; // example: 'C:\Program Files\Borland\Delphi7', the JVCL Installer resolves macros
  end;

{ InstallJediInformation() writes the "Version", "DcpDir", "BplDir" and "RootDir"
  values into the registry key IdeRegKey\Jedi\ProjectName. Returns True if the
  values could be written. }
function InstallJediRegInformation(const IdeRegKey, ProjectName, Version, DcpDir,
  BplDir, RootDir: string): Boolean;

{ RemoveJediInformation() deletes the registry key IdeRegKey\Jedi\ProjectName.
  If there is no further subkeys to IdeRegKey\Jedi and no values in this key,
  the whole Jedi-key is deleted. }
procedure RemoveJediRegInformation(const IdeRegKey, ProjectName: string);

{ ReadJediInformation() reads the JEDI Information from the registry. Returns
  False if Version='' or DcpDir='' or BplDir='' or RootDir=''. }
function ReadJediRegInformation(const IdeRegKey, ProjectName: string; out Version,
  DcpDir, BplDir, RootDir: string): Boolean; overload;

{ ReadJediInformation() reads the Jedi Information from the registry. }
function ReadJediRegInformation(const IdeRegKey, ProjectName: string): TJediInformation; overload;

{ ParseVersionNumber() converts a version number 'major.minor.release.build' to
  cardinal like the JclBase JclVersion constant. If the VersionStr is invalid
  the function returns 0. }
function ParseVersionNumber(const VersionStr: string): Cardinal;

implementation

uses
  Windows, Registry;

{$IFNDEF RTL140_UP}
function ExcludeTrailingPathDelimiter(const Path: string): string;
begin
  if (Path <> '') and (Path[Length(Path)] = '\') then
    Result := Copy(Path, 1, Length(Path) - 1)
  else
    Result := Path;
end;
{$ENDIF ~RTL140_UP}

function InstallJediRegInformation(const IdeRegKey, ProjectName, Version, DcpDir,
  BplDir, RootDir: string): Boolean;
var
  Reg: TRegistry;
begin
  Result := False;
  if (Version <> '') and (DcpDir <> '') and (BplDir <> '') and (RootDir <> '') then
  begin
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey(IdeRegKey + '\Jedi', True) then // do not localize
	    Reg.CloseKey;
      if Reg.OpenKey(IdeRegKey + '\Jedi\' + ProjectName, True) then // do not localize
      begin
        Reg.WriteString('Version', Version); // do not localize
        Reg.WriteString('DcpDir', ExcludeTrailingPathDelimiter(DcpDir)); // do not localize
        Reg.WriteString('BplDir', ExcludeTrailingPathDelimiter(BplDir)); // do not localize
        Reg.WriteString('RootDir', ExcludeTrailingPathDelimiter(RootDir)); // do not localize
        Result := True;
      end;
    finally
      Reg.Free;
    end;
  end;
end;

procedure RemoveJediRegInformation(const IdeRegKey, ProjectName: string);
var
  Reg: TRegistry;
  Names: TStringList;
  JediKeyName, ProjectKeyName: string;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
// (outchy) do not delete target settings
//    Reg.DeleteKey(IdeRegKey + '\Jedi\' + ProjectName); // do not localize

    JediKeyName := IdeRegKey + '\Jedi';                  // do not localize
    ProjectKeyName := JediKeyName + '\' + ProjectName;   // do not localize

    if Reg.OpenKey(ProjectKeyName, False) then
    begin
      Reg.DeleteValue('Version'); // do not localize
      Reg.DeleteValue('DcpDir'); // do not localize
      Reg.DeleteValue('BplDir'); // do not localize
      Reg.DeleteValue('RootDir'); // do not localize

      Names := TStringList.Create;
      try
        Reg.GetKeyNames(Names);
        if Names.Count = 0 then
        begin
          Reg.GetValueNames(Names);
          if Names.Count = 0 then
          begin
            Reg.CloseKey;
            Reg.DeleteKey(ProjectKeyName); // do not localize
          end;
        end;
      finally
        Names.Free;
      end;
    end;


    if Reg.OpenKey(JediKeyName, False) then // do not localize
    begin
      Names := TStringList.Create;
      try
        Reg.GetKeyNames(Names);
        if Names.Count = 0 then
        begin
          Reg.GetValueNames(Names);
          if Names.Count = 0 then
          begin
            Reg.CloseKey;
            Reg.DeleteKey(JediKeyName); // do not localize
          end;
        end;
      finally
        Names.Free;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

function ReadJediRegInformation(const IdeRegKey, ProjectName: string; out Version,
  DcpDir, BplDir, RootDir: string): Boolean; overload;
var
  Reg: TRegistry;
begin
  Version := '';
  DcpDir := '';
  BplDir := '';
  RootDir := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly(IdeRegKey + '\Jedi\' + ProjectName) then // do not localize
    begin
      if Reg.ValueExists('Version') then // do not localize
        Version := Reg.ReadString('Version'); // do not localize
      if Reg.ValueExists('DcpDir') then // do not localize
        DcpDir := ExcludeTrailingPathDelimiter(Reg.ReadString('DcpDir')); // do not localize
      if Reg.ValueExists('BplDir') then // do not localize
        BplDir := ExcludeTrailingPathDelimiter(Reg.ReadString('BplDir')); // do not localize
      if Reg.ValueExists('RootDir') then // do not localize
        RootDir := ExcludeTrailingPathDelimiter(Reg.ReadString('RootDir')); // do not localize
    end;
  finally
    Reg.Free;
  end;
  Result := (Version <> '') and (DcpDir <> '') and (BplDir <> '') and (RootDir <> '');
end;

function ReadJediRegInformation(const IdeRegKey, ProjectName: string): TJediInformation;
begin
  ReadJediRegInformation(IdeRegKey, ProjectName, Result.Version, Result.DcpDir,
    Result.BplDir, Result.RootDir);
end;

function ParseVersionNumber(const VersionStr: string): Cardinal;
const
  Shifts: array[0..3] of Integer = (24, 16, 15, 0);
var
  S: string;
  ps: Integer;
  Count: Integer;
begin
  S := VersionStr;
  Result := 0;
  if S <> '' then
  begin
    Result := 0;
    try
      Count := 0;
      ps := Pos('.', S);
      while (ps > 0) and (Count < High(Shifts)) do
      begin
        Result := Result or (Cardinal(StrToInt(Copy(S, 1, ps - 1))) shl Shifts[Count]);
        S := Copy(S, ps + 1, MaxInt);
        ps := Pos('.', S);
        Inc(Count);
      end;
      Result := Result or (Cardinal(StrToInt(Copy(S, 1, MaxInt))) shl Shifts[Count]);
    except
      Result := 0;
    end;
  end;
end;

end.
