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
{******************************************************************************}
{                                                                              }
{ Contains various utility routines to read and write registry values. Using   }
{ these routines prevents you from having to instantiate temporary TRegistry   }
{ objects and since the routines directly call the registry API they do not    }
{ suffer from the resource overhead as TRegistry does.                         }
{                                                                              }
{ Unit owner: Eric S.Fisher                                                    }
{ Last modified: January 30, 2001                                              }
{                                                                              }
{******************************************************************************}


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

function RegCreateKey(const RootKey: HKEY; const Key, Value: string): Longint;
function RegDeleteEntry(const RootKey: HKEY; const Key, Name: string): Boolean;
function RegDeleteKeyTree(const RootKey: HKEY; const Key: string): Boolean;

function RegReadBool(const RootKey: HKEY; const Key, Name: string): Boolean;
function RegReadBoolDef(const RootKey: HKEY; const Key, Name: string; Def: Boolean): Boolean;
function RegReadInteger(const RootKey: HKEY; const Key, Name: string): Integer;
function RegReadIntegerDef(const RootKey: HKEY; const Key, Name: string; Def: Integer): Integer;
function RegReadString(const RootKey: HKEY; const Key, Name: string): string;
function RegReadStringDef(const RootKey: HKEY; const Key, Name, Def: string): string;
function RegReadBinary(const RootKey: HKEY; const Key, Name: string; var Value; const ValueSize: Cardinal): Cardinal;
function RegReadBinaryDef(const RootKey: HKEY; const Key, Name: string;
  var Value; const ValueSize: Cardinal; const Def: Byte): Cardinal;
procedure RegWriteBool(const RootKey: HKEY; const Key, Name: string; Value: Boolean);
procedure RegWriteInteger(const RootKey: HKEY; const Key, Name: string; Value: Integer);
procedure RegWriteString(const RootKey: HKEY; const Key, Name, Value: string);
procedure RegWriteBinary(const RootKey: HKEY; const Key, Name: string; var Value; const ValueSize: Cardinal);

function RegGetValueNames(const RootKey: HKEY; const Key: string; const List: TStrings): Boolean;
function RegGetKeyNames(const RootKey: HKEY; const Key: string; const List: TStrings): Boolean;
function RegHasSubKeys(const RootKey: HKEY; const Key: string): Boolean;

type
  TExecKind = (ekMachineRun, ekMachineRunOnce, ekUserRun, ekUserRunOnce,
    ekServiceRun, ekServiceRunOnce);

  EJclRegistryError = class (EJclError);

function UnregisterAutoExec(ExecKind: TExecKind; const Name: string): Boolean;
function RegisterAutoExec(ExecKind: TExecKind; const Name, Cmdline: string): Boolean;

function RegSaveList(const RootKey: HKEY; const Key: string; const ListName: string;
  const Items: TStrings): Boolean;
function RegLoadList(const RootKey: HKEY; const Key: string; const ListName: string;
  const SaveTo: TStrings): Boolean;
function RegDelList(const RootKey: HKEY; const Key: string; const ListName: string): Boolean;

implementation

uses
  SysUtils,
  JclResources;

//==============================================================================
// Internal helper routines
//==============================================================================

procedure ReadError(const Key: string);
begin
  raise EJclRegistryError.CreateResRecFmt(@RsUnableToOpenKeyRead, [Key]);
end;

//------------------------------------------------------------------------------

procedure WriteError(const Key: string);
begin
  raise EJclRegistryError.CreateResRecFmt(@RsUnableToOpenKeyWrite, [Key]);
end;

//------------------------------------------------------------------------------

procedure ValueError(const Key, Name: string);
begin
  raise EJclRegistryError.CreateResRecFmt(@RsUnableToAccessValue, [Key, Name]);
end;

//------------------------------------------------------------------------------

function GetKeyAndPath(ExecKind: TExecKind; var Key: HKEY; var RegPath: string): Boolean;
begin
  Result := False;
  if (ExecKind in [ekServiceRun, ekServiceRunOnce]) and (Win32Platform = VER_PLATFORM_WIN32_NT) then
    Exit;
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
  Result := True;
end;

//------------------------------------------------------------------------------

function RelativeKey(const Key: string): PChar;
begin
  Result := PChar(Key);
  if (Key <> '') and (Key[1] = '\') then
    Inc(Result);
end;

//==============================================================================
// Registry
//==============================================================================

function RegCreateKey(const RootKey: HKEY; const Key, Value: string): Longint;
begin
  Result := RegSetValue(RootKey, RelativeKey(Key), REG_SZ, PChar(Value), Length(Value));
end;

//------------------------------------------------------------------------------

function RegDeleteEntry(const RootKey: HKEY; const Key, Name: string): Boolean;
var
  RegKey: HKEY;
begin
  Result := False;
  if RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_SET_VALUE, RegKey) = ERROR_SUCCESS then
  begin
    Result := RegDeleteValue(RegKey, PChar(Name)) = ERROR_SUCCESS;
    RegCloseKey(RegKey);
    if not Result then
      ValueError(Key, Name);
  end
  else
    WriteError(Key);
end;

//------------------------------------------------------------------------------

function RegDeleteKeyTree(const RootKey: HKEY; const Key: string): Boolean;
var
  RegKey: HKEY;
  I: DWORD;
  Size: DWORD;
  NumSubKeys: DWORD;
  MaxSubKeyLen: DWORD;
  KeyName: string;
begin
  Result := RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_ALL_ACCESS, RegKey) = ERROR_SUCCESS;
  if Result then
  begin
    RegQueryInfoKey(RegKey, nil, nil, nil, @NumSubKeys, @MaxSubKeyLen, nil, nil, nil, nil, nil, nil);
    if NumSubKeys <> 0 then
      for I := NumSubKeys-1 downto 0 do
      begin
        Size := MaxSubKeyLen+1;
        SetLength(KeyName, Size);
        RegEnumKeyEx(RegKey, I, PChar(KeyName), Size, nil, nil, nil, nil);
        SetLength(KeyName, StrLen(PChar(KeyName)));
        Result := RegDeleteKeyTree(RootKey, Key + '\' + KeyName);
        if not Result then
          Break;
      end;
    RegCloseKey(RegKey);
    if Result then
      Result := Windows.RegDeleteKey(RootKey, RelativeKey(Key)) = ERROR_SUCCESS;
    end
    else
      WriteError(Key);
end;

//------------------------------------------------------------------------------

function RegReadBool(const RootKey: HKEY; const Key, Name: string): Boolean;
begin
  Result := RegReadInteger(RootKey, Key, Name) <> 0;
end;

//------------------------------------------------------------------------------

function RegReadBoolDef(const RootKey: HKEY; const Key, Name: string; Def: Boolean): Boolean;
begin
  Result := Boolean(RegReadIntegerDef(RootKey, Key, Name, Ord(Def)));
end;

//------------------------------------------------------------------------------

function RegReadInteger(const RootKey: HKEY; const Key, Name: string): Integer;
var
  RegKey: HKEY;
  Size: DWORD;
  IntVal: Integer;
  RegKind: DWORD;
  Ret: Longint;
begin
  Result := 0;
  if RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    RegKind := 0;
    Size := SizeOf(Integer);
    Ret := RegQueryValueEx(RegKey, PChar(Name), nil, @RegKind, @IntVal, @Size);
    RegCloseKey(RegKey);
    if Ret = ERROR_SUCCESS then
    begin
      if RegKind = REG_DWORD then
        Result := IntVal
      else
        ValueError(Key, Name);
    end
    else
      ValueError(Key, Name);
  end
  else
    ReadError(Key);
end;

//------------------------------------------------------------------------------

function RegReadIntegerDef(const RootKey: HKEY; const Key, Name: string; Def: Integer): Integer;
var
  RegKey: HKEY;
  Size: DWORD;
  IntVal: Integer;
  RegKind: DWORD;
begin
  Result := Def;
  if RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    RegKind := 0;
    Size := SizeOf(Integer);
    if RegQueryValueEx(RegKey, PChar(Name), nil, @RegKind, @IntVal, @Size) = ERROR_SUCCESS then
      if RegKind = REG_DWORD then
        Result := IntVal;
    RegCloseKey(RegKey);
  end;
end;

//------------------------------------------------------------------------------

function RegReadString(const RootKey: HKEY; const Key, Name: string): string;
var
  RegKey: HKEY;
  Size: DWORD;
  StrVal: string;
  RegKind: DWORD;
  Ret: Longint;
begin
  Result := '';
  if RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    RegKind := 0;
    Size := 0;
    Ret := RegQueryValueEx(RegKey, PChar(Name), nil, @RegKind, nil, @Size);
    if Ret = ERROR_SUCCESS then
      if RegKind in [REG_SZ, REG_EXPAND_SZ] then
      begin
        SetLength(StrVal, Size);
        RegQueryValueEx(RegKey, PChar(Name), nil, @RegKind, PByte(StrVal), @Size);
        SetLength(StrVal, StrLen(PChar(StrVal)));
        Result := StrVal;
      end;
    RegCloseKey(RegKey);
    if not (RegKind in [REG_SZ, REG_EXPAND_SZ]) then
      ValueError(Key, Name);
  end
  else
    ReadError(Key);
end;

//------------------------------------------------------------------------------

function RegReadStringDef(const RootKey: HKEY; const Key, Name, Def: string): string;
var
  RegKey: HKEY;
  Size: DWORD;
  StrVal: string;
  RegKind: DWORD;
begin
  Result := Def;
  if RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    RegKind := 0;
    Size := 0;
    if RegQueryValueEx(RegKey, PChar(Name), nil, @RegKind, nil, @Size) = ERROR_SUCCESS then
      if RegKind in [REG_SZ, REG_EXPAND_SZ] then
      begin
        SetLength(StrVal, Size);
        if RegQueryValueEx(RegKey, PChar(Name), nil, @RegKind, PByte(StrVal), @Size) = ERROR_SUCCESS then
        begin
          SetLength(StrVal, StrLen(PChar(StrVal)));
          Result := StrVal;
        end;
      end;
    RegCloseKey(RegKey);
  end;
end;

//------------------------------------------------------------------------------

function RegReadBinary(const RootKey: HKEY; const Key, Name: string; var Value; const ValueSize: Cardinal): Cardinal;
var
  RegKey: HKEY;
  Size: DWORD;
  RegKind: DWORD;
  Ret: Longint;
begin
  Result := 0;
  if RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    RegKind := 0;
    Size := 0;
    Ret := RegQueryValueEx(RegKey, PChar(Name), nil, @RegKind, nil, @Size);
    if Ret = ERROR_SUCCESS then
      if RegKind = REG_BINARY then
      begin
        if Size > ValueSize then
          Size := ValueSize;
        RegQueryValueEx(RegKey, PChar(Name), nil, @RegKind, @Value, @Size);
        Result := Size;
      end;
    RegCloseKey(RegKey);
    if RegKind <> REG_BINARY then
      ValueError(Key, Name);
  end
  else
    ReadError(Key);
end;

//------------------------------------------------------------------------------

function RegReadBinaryDef(const RootKey: HKEY; const Key, Name: string;
  var Value; const ValueSize: Cardinal; const Def: Byte): Cardinal;
var
  RegKey: HKEY;
  Size: DWORD;
  StrVal: string;
  RegKind: DWORD;
begin
  Result := 0;
  FillChar(Value, ValueSize, Def);
  if RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    RegKind := 0;
    Size := 0;
    if RegQueryValueEx(RegKey, PChar(Name), nil, @RegKind, nil, @Size) = ERROR_SUCCESS then
      if RegKind = REG_BINARY then
      begin
        if RegQueryValueEx(RegKey, PChar(Name), nil, @RegKind, PByte(StrVal), @Size) = ERROR_SUCCESS then
        begin
          if Size > ValueSize then
            Size := ValueSize;
          RegQueryValueEx(RegKey, PChar(Name), nil, @RegKind, @Value, @Size);
          Result := Size;
        end;
      end;
    RegCloseKey(RegKey);
  end;
end;

//------------------------------------------------------------------------------

procedure RegWriteBool(const RootKey: HKEY; const Key, Name: string; Value: Boolean);
begin
  RegWriteInteger(RootKey, Key, Name, Ord(Value));
end;

//------------------------------------------------------------------------------

procedure RegWriteInteger(const RootKey: HKEY; const Key, Name: string; Value: Integer);
var
  RegKey: HKEY;
  Ret: Longint;
begin
  if RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_SET_VALUE, RegKey) = ERROR_SUCCESS then
  begin
    Ret := RegSetValueEx(RegKey, PChar(Name), 0, REG_DWORD, @Value, SizeOf(Integer));
    RegCloseKey(RegKey);
    if Ret <> ERROR_SUCCESS then
      WriteError(Key);
  end
  else
    WriteError(Key);
end;

//------------------------------------------------------------------------------

procedure RegWriteString(const RootKey: HKEY; const Key, Name, Value: string);
var
  RegKey: HKEY;
  Ret: Longint;
begin
  if RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_SET_VALUE, RegKey) = ERROR_SUCCESS then
  begin
    Ret := RegSetValueEx(RegKey, PChar(Name), 0, REG_SZ, PChar(Value), Length(Value));
    RegCloseKey(RegKey);
    if Ret <> ERROR_SUCCESS then
      WriteError(Key);
  end
  else
    WriteError(Key);
end;

//------------------------------------------------------------------------------

procedure RegWriteBinary(const RootKey: HKEY; const Key, Name: string; var Value; const ValueSize: Cardinal);
var
  RegKey: HKEY;
  Ret: Longint;
begin
  if RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_SET_VALUE, RegKey) = ERROR_SUCCESS then
  begin
    Ret := RegSetValueEx(RegKey, PChar(Name), 0, REG_BINARY, @Value, ValueSize);
    RegCloseKey(RegKey);
    if Ret <> ERROR_SUCCESS then
      WriteError(Key);
  end
  else
    WriteError(Key);
end;

//------------------------------------------------------------------------------

function UnregisterAutoExec(ExecKind: TExecKind; const Name: string): Boolean;
var
  Key: HKEY;
  RegPath: string;
begin
  Result := GetKeyAndPath(ExecKind, Key, RegPath);
  if Result then
    Result := RegDeleteEntry(Key, RegPath, Name);
end;

//------------------------------------------------------------------------------

function RegisterAutoExec(ExecKind: TExecKind; const Name, Cmdline: string): Boolean;
var
  Key: HKEY;
  RegPath: string;
begin
  Result := GetKeyAndPath(ExecKind, Key, RegPath);
  if Result then
     RegWriteString(Key, RegPath, Name, Cmdline);
end;

//------------------------------------------------------------------------------

function RegGetValueNames(const RootKey: HKEY; const Key: string; const List: TStrings): Boolean;
var
  RegKey: HKEY;
  I: DWORD;
  Size: DWORD;
  NumSubKeys: DWORD;
  NumSubValues: DWORD;
  MaxSubValueLen: DWORD;
  ValueName: string;
begin
  Result := False;
  List.Clear;
  if RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    if RegQueryInfoKey(RegKey, nil, nil, nil, @NumSubKeys, nil, nil, @NumSubValues, @MaxSubValueLen, nil, nil, nil) = ERROR_SUCCESS then
    begin
      SetLength(ValueName, MaxSubValueLen + 1);
      if NumSubValues <> 0 then
        for I := 0 to NumSubValues - 1 do
        begin
          Size := MaxSubValueLen + 1;
          RegEnumValue(RegKey, I, PChar(ValueName), Size, nil, nil, nil, nil);
          List.Add(PChar(ValueName));
        end;
      Result := True;
    end;
    RegCloseKey(RegKey);
  end
  else
    ReadError(Key);
end;

//------------------------------------------------------------------------------

function RegGetKeyNames(const RootKey: HKEY; const Key: string; const List: TStrings): Boolean;
var
  RegKey: HKEY;
  I: DWORD;
  Size: DWORD;
  NumSubKeys: DWORD;
  MaxSubKeyLen: DWORD;
  KeyName: string;
begin
  Result := False;
  List.Clear;
  if RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    if RegQueryInfoKey(RegKey, nil, nil, nil, @NumSubKeys, @MaxSubKeyLen, nil, nil, nil, nil, nil, nil) = ERROR_SUCCESS then
    begin
      SetLength(KeyName, MaxSubKeyLen+1);
      if NumSubKeys <> 0 then
        for I := 0 to NumSubKeys-1 do
        begin
          Size := MaxSubKeyLen+1;
          RegEnumKeyEx(RegKey, I, PChar(KeyName), Size, nil, nil, nil, nil);
          List.Add(PChar(KeyName));
        end;
      Result := True;
    end;
    RegCloseKey(RegKey);
  end
  else
    ReadError(Key);
end;

//------------------------------------------------------------------------------

function RegHasSubKeys(const RootKey: HKEY; const Key: string): Boolean;
var
  RegKey: HKEY;
  NumSubKeys: Integer;
begin
  Result := False;
  if RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    RegQueryInfoKey(RegKey, nil, nil, nil, @NumSubKeys, nil, nil, nil, nil, nil, nil, nil);
    Result := NumSubKeys <> 0;
    RegCloseKey(RegKey);
  end
  else
    ReadError(Key);
end;

//------------------------------------------------------------------------------

function RegSaveList(const RootKey: HKEY; const Key: string;
  const ListName: string; const Items: TStrings): Boolean;
var
  I: Integer;
  Subkey: string;
begin
  Result := False;
  Subkey := Key + '\' + ListName;
  if RegCreateKey(RootKey, Subkey, '') = ERROR_SUCCESS then
  begin
    // Save Number of strings
    RegWriteInteger(RootKey, Subkey, 'Items', Items.Count);
    for I := 1 to Items.Count do
      RegWriteString(RootKey, Subkey, IntToStr(I), Items[I-1]);
    Result := True;
  end;
end;

//------------------------------------------------------------------------------

function RegLoadList(const RootKey: HKEY; const Key: string;
  const ListName: string; const SaveTo: TStrings): Boolean;
var
  I, N: Integer;
  Subkey: string;
begin
  SaveTo.Clear;
  Subkey := Key + '\' + ListName;
  N := RegReadInteger(RootKey, Subkey, 'Items');
  for I := 1 to N do
    SaveTo.Add(RegReadString(RootKey, Subkey, IntToStr(I)));
  Result := N > 0;
end;

//------------------------------------------------------------------------------

function RegDelList(const RootKey: HKEY; const Key: string; const ListName: string): Boolean;
var
  I, N: Integer;
  Subkey: string;
begin
  Result := False;
  Subkey := Key + '\' + ListName;
  N := RegReadIntegerDef(RootKey, Subkey, 'Items', -1);
  if (N > 0) and RegDeleteEntry(RootKey, Subkey, 'Items') then
    for I := 1 to N do
    begin
      Result := RegDeleteEntry(RootKey, Subkey, IntToStr(I));
      if not Result then
        Break;
    end;
end;

end.
