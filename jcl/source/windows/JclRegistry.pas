{******************************************************************************}
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JclRegistry.pas.                                        }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{ Last modified: December 12, 2000                                             }
{                                                                              }
{******************************************************************************}

//  JclRegistry created from JclRegIni by ESF  2000/06/05

unit JclRegistry;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

uses
  Windows, Classes,
  JclBase;

//------------------------------------------------------------------------------
// Registry
//------------------------------------------------------------------------------

function RegCreateKey(const Key, Value: string): Longint;
function RegDeleteEntry(RootKey: HKEY; const Key, Name: string): Boolean;
function RegDeleteKey(RootKey: HKEY; const Key: string):Boolean;

function RegReadBool(RootKey: HKEY; const Key, Name: string): Boolean;
function RegReadBoolDef(RootKey: HKEY; const Key, Name: string; Def: Boolean): Boolean;
function RegReadInteger(RootKey: HKEY; const Key, Name: string): Integer;
function RegReadIntegerDef(RootKey: HKEY; const Key, Name: string; Def: Integer): Integer;
function RegReadString(RootKey: HKEY; const Key, Name: string): string;
function RegReadStringDef(RootKey: HKEY; const Key, Name, Def: string): string;
procedure RegWriteBool(RootKey: HKEY; const Key, Name: string; Value: Boolean);
procedure RegWriteInteger(RootKey: HKEY; const Key, Name: string; Value: Integer);
procedure RegWriteString(RootKey: HKEY; const Key, Name, Value: string);

function RegGetValueNames(const RootKey: HKEY; const Key: string; const List: TStrings): Boolean;
function RegGetKeyNames(const RootKey: HKEY; const Key: string; const List: TStrings): Boolean;
function RegHasSubKeys(const RootKey: HKEY; const Key: string): Boolean;

type
  TExecKind = (ekMachineRun, ekMachineRunOnce, ekUserRun, ekUserRunOnce,
               ekServiceRun, ekServiceRunOnce);

  EJclRegistryError = class (EJclError);

function UnregisterAutoExec(ExecKind: TExecKind; const Path: string): Boolean;
function RegisterAutoExec(ExecKind: TExecKind; const Path: string): Boolean;

implementation

uses
  Registry, SysUtils,
  JclResources, JclSysInfo;

//==============================================================================
// Registry
//==============================================================================

// (rom) local helper

procedure OpenKey(WinReg: TRegistry; const RootKey: HKEY;
  const Key: string; const ForRead: Boolean);
begin
  WinReg.RootKey := RootKey;
  if not WinReg.OpenKey(Key, False) then
    if ForRead then
      raise EJclRegistryError.CreateResRecFmt(@RsUnableToOpenKeyRead, [Key])
    else
      raise EJclRegistryError.CreateResRecFmt(@RsUnableToOpenKeyWrite, [Key]);
end;

//------------------------------------------------------------------------------

function RegCreateKey(const Key, Value: string): Longint;
begin
  Result := RegSetValue(HKEY_CLASSES_ROOT, PChar(Key), REG_SZ, PChar(Value),
    Length(Value));
end;

//------------------------------------------------------------------------------

function RegDeleteEntry(RootKey: HKEY; const Key, Name: string): Boolean;
var
  WinReg: TRegistry;
begin
  Result := False;
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := RootKey;
    if WinReg.OpenKey(Key, False) then
      Result := WinReg.DeleteValue(Name);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

function RegDeleteKey(RootKey: HKEY;const Key: string):Boolean;
var
  WinReg: TRegistry;
begin
  Result := False;
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := RootKey;
    if WinReg.KeyExists(Key) then
    begin
      WinReg.CloseKey;
      Result := WinReg.Deletekey(Key);
    end;
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

function RegReadBool(RootKey: HKEY; const Key, Name: string): Boolean;
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    OpenKey(WinReg, RootKey, Key, True);
    Result := WinReg.ReadBool(Name);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

function RegReadBoolDef(RootKey: HKEY; const Key, Name: string; Def: Boolean): Boolean;
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := RootKey;
    if WinReg.OpenKey(Key, False) and WinReg.ValueExists(Name) then
      Result := WinReg.ReadBool(Name)
    else
      Result := Def;
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

function RegReadInteger(RootKey: HKEY; const Key, Name: string): Integer;
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    OpenKey(WinReg, RootKey, Key, True);
    Result := WinReg.ReadInteger(Name);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

function RegReadIntegerDef(RootKey: HKEY; const Key, Name: string; Def: Integer): Integer;
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := RootKey;
    if WinReg.OpenKey(Key, False) and WinReg.ValueExists(Name) then
      Result := WinReg.ReadInteger(Name)
    else
      Result := Def;
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

function RegReadString(RootKey: HKEY; const Key, Name: string): string;
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    OpenKey(WinReg, RootKey, Key, True);
    Result := WinReg.ReadString(Name);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

function RegReadStringDef(RootKey: HKEY; const Key, Name, Def: string): string;
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := RootKey;
    if WinReg.OpenKey(Key, False) and WinReg.ValueExists(Name) then
      Result := WinReg.ReadString(Name)
    else
      Result := Def;
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure RegWriteBool(RootKey: HKEY; const Key, Name: string; Value: Boolean);
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    OpenKey(WinReg, RootKey, Key, False);
    WinReg.WriteBool(Name, Value);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure RegWriteInteger(RootKey: HKEY; const Key, Name: string; Value: Integer);
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    OpenKey(WinReg, RootKey, Key, False);
    WinReg.WriteInteger(Name, Value);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure RegWriteString(RootKey: HKEY; const Key, Name, Value: string);
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    OpenKey(WinReg, RootKey, Key, False);
    WinReg.WriteString(Name, Value);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure GetKeyAndPath(ExecKind: TExecKind; var Key: HKEY; var RegPath: string);
begin
  Key := HKEY_CURRENT_USER;
  if ExecKind in [ekMachineRun, ekMachineRunOnce, ekServiceRun, ekServiceRunOnce] then
    Key := HKEY_LOCAL_MACHINE;
  RegPath := 'Software\Microsoft\Windows\CurrentVersion\';
  case ExecKind of
    ekMachineRun, ekUserRun:
      RegPath := RegPath + 'Run';
    ekMachineRunOnce, ekUserRunOnce:
      RegPath := RegPath + 'RunOnce';
    ekServiceRun:
      RegPath := RegPath + 'RunServices';
    ekServiceRunOnce:
      RegPath := RegPath + 'RunServicesOnce';
  end;
end;

//------------------------------------------------------------------------------

function UnregisterAutoExec(ExecKind: TExecKind; const Path: string): Boolean;
var
  Key: HKEY;
  RegPath: string;
  WinReg: TRegistry;
begin
  Result := False;
  if (ExecKind in [ekServiceRun, ekServiceRunOnce]) and IsWinNT then
    Exit;
  GetKeyAndPath(ExecKind, Key, RegPath);
  WinReg := TRegistry.Create;
  WinReg.RootKey := Key;
  OpenKey(WinReg, Key, RegPath, False);
  if WinReg.ValueExists(ExtractFileName(Path)) then
    Result := Winreg.DeleteValue(ExtractFileName(Path))
  else
    Result := True;
end;

//------------------------------------------------------------------------------

function RegisterAutoExec(ExecKind: TExecKind; const Path: string): Boolean;
var
  Key: HKEY;
  RegPath: string;
begin
  Result := False;
  if (ExecKind in [ekServiceRun, ekServiceRunOnce]) and IsWinNT then
    Exit;
  GetKeyAndPath(ExecKind, Key, RegPath);
  RegWriteString(Key, RegPath, ExtractFileName(Path), Path);
  Result := True;
end;

//------------------------------------------------------------------------------

function RegGetValueNames(const RootKey: HKEY; const Key: string; const List: TStrings): Boolean;
var
  WinReg: TRegistry;
begin
  List.Clear;
  WinReg := TRegistry.Create;
  try
    OpenKey(WinReg, RootKey, Key, True);
    WinReg.GetValueNames(List);
    Result := List.Count > 0;
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

function RegGetKeyNames(const RootKey: HKEY; const Key: string; const List: TStrings): Boolean;
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    OpenKey(WinReg, RootKey, Key, True);
    WinReg.GetKeyNames(List);
    Result := List.Count > 0;
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

function RegHasSubKeys(const RootKey: HKEY; const Key: string): Boolean;
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    OpenKey(WinReg, RootKey, Key, True);
    Result := WinReg.HasSubKeys;
  finally
    WinReg.Free;
  end;
end;

end.
