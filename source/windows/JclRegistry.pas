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
{ The Original Code is JclRegistry.pas.                                                            }
{                                                                                                  }
{ The Initial Developers of the Original Code are John C Molyneux, Marcel van Brakel and           }
{ Charlie Calvert. Portions created by these individuals are Copyright (C) of these individuals.   }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Marcel van Brakel                                                                              }
{   Stephane Fillon                                                                                }
{   Eric S.Fisher                                                                                  }
{   Peter Friese                                                                                   }
{   Andreas Hausladen (ahuser)                                                                     }
{   Manlio Laschena (manlio)                                                                       }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Petr Vones (pvones)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Contains various utility routines to read and write registry values. Using these routines        }
{ prevents you from having to instantiate temporary TRegistry objects and since the routines       }
{ directly call the registry API they do not suffer from the resource overhead as TRegistry does.  }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclRegistry;

{$I jcl.inc}

interface

uses
  Windows, Classes,
  JclBase, JclStrings, JclWideStrings;

type
  DelphiHKEY = Longword;
  {$HPPEMIT '// BCB users must typecast the HKEY values to DelphiHKEY or use the HK-values below.'}

  TExecKind = (ekMachineRun, ekMachineRunOnce, ekUserRun, ekUserRunOnce,
    ekServiceRun, ekServiceRunOnce);

  EJclRegistryError = class(EJclError);

{$IFNDEF FPC}
// (rom) from JclMiscel.pas, now put to good use for BCB
const
  HKCR = DelphiHKEY(HKEY_CLASSES_ROOT);
  HKCU = DelphiHKEY(HKEY_CURRENT_USER);
  HKLM = DelphiHKEY(HKEY_LOCAL_MACHINE);
  HKUS = DelphiHKEY(HKEY_USERS);
  HKPD = DelphiHKEY(HKEY_PERFORMANCE_DATA);
  HKCC = DelphiHKEY(HKEY_CURRENT_CONFIG);
  HKDD = DelphiHKEY(HKEY_DYN_DATA);
{$ENDIF ~FPC}

function RegCreateKey(const RootKey: DelphiHKEY; const Key: string): Longint; overload;
function RegCreateKey(const RootKey: DelphiHKEY; const Key, Value: string): Longint; overload;
function RegDeleteEntry(const RootKey: DelphiHKEY; const Key, Name: string): Boolean;
function RegDeleteKeyTree(const RootKey: DelphiHKEY; const Key: string): Boolean;

function RegGetDataSize(const RootKey: DelphiHKEY; const Key, Name: string;
  out DataSize: Cardinal): Boolean;
function RegGetDataType(const RootKey: DelphiHKEY; const Key, Name: string;
  out DataType: Cardinal): Boolean;
function RegReadBool(const RootKey: DelphiHKEY; const Key, Name: string): Boolean;
function RegReadBoolDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Boolean): Boolean;
function RegReadInteger(const RootKey: DelphiHKEY; const Key, Name: string): Integer;
function RegReadIntegerDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Integer): Integer;
function RegReadCardinal(const RootKey: DelphiHKEY; const Key, Name: string): Cardinal;
function RegReadCardinalDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Cardinal): Cardinal;
function RegReadDWORD(const RootKey: DelphiHKEY; const Key, Name: string): DWORD;
function RegReadDWORDDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: DWORD): DWORD;
function RegReadInt64(const RootKey: DelphiHKEY; const Key, Name: string): Int64;
function RegReadInt64Def(const RootKey: DelphiHKEY; const Key, Name: string; Def: Int64): Int64;
function RegReadUInt64(const RootKey: DelphiHKEY; const Key, Name: string): UInt64;
function RegReadUInt64Def(const RootKey: DelphiHKEY; const Key, Name: string; Def: UInt64): UInt64;

function RegReadString(const RootKey: DelphiHKEY; const Key, Name: string): string;
function RegReadStringDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: string): string;
function RegReadAnsiString(const RootKey: DelphiHKEY; const Key, Name: AnsiString): AnsiString;
function RegReadAnsiStringDef(const RootKey: DelphiHKEY; const Key, Name: AnsiString; Def: AnsiString): AnsiString;
function RegReadWideString(const RootKey: DelphiHKEY; const Key, Name: string): WideString;
function RegReadWideStringDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: WideString): WideString;

procedure RegReadMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: TStrings); overload;
function RegReadMultiSz(const RootKey: DelphiHKEY; const Key, Name: string): PMultiSz; overload;
procedure RegReadMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string; Value, Def: TStrings); overload;
function RegReadMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: PMultiSz): PMultiSz; overload;

procedure RegReadWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: TWideStrings); overload;
function RegReadWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string): PWideMultiSz; overload;
procedure RegReadWideMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string; Value, Def: TWideStrings); overload;
function RegReadWideMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: PWideMultiSz): PWideMultiSz; overload;

function RegReadBinary(const RootKey: DelphiHKEY; const Key, Name: string; var Value; const ValueSize: Cardinal): Cardinal;
function RegReadBinaryDef(const RootKey: DelphiHKEY; const Key, Name: string;
  var Value; const ValueSize: Cardinal; const Def: Byte): Cardinal;

procedure RegWriteBool(const RootKey: DelphiHKEY; const Key, Name: string; Value: Boolean); overload;
procedure RegWriteBool(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: Boolean); overload;
procedure RegWriteInteger(const RootKey: DelphiHKEY; const Key, Name: string; Value: Integer); overload;
procedure RegWriteInteger(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: Integer); overload;
procedure RegWriteCardinal(const RootKey: DelphiHKEY; const Key, Name: string; Value: Cardinal); overload;
procedure RegWriteCardinal(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: Cardinal); overload;
procedure RegWriteDWORD(const RootKey: DelphiHKEY; const Key, Name: string; Value: DWORD); overload;
procedure RegWriteDWORD(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: DWORD); overload;
procedure RegWriteInt64(const RootKey: DelphiHKEY; const Key, Name: string; Value: Int64); overload;
procedure RegWriteInt64(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: Int64); overload;
procedure RegWriteUInt64(const RootKey: DelphiHKEY; const Key, Name: string; Value: UInt64); overload;
procedure RegWriteUInt64(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: UInt64); overload;

procedure RegWriteString(const RootKey: DelphiHKEY; const Key, Name, Value: string); overload;
procedure RegWriteString(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: string); overload;
procedure RegWriteAnsiString(const RootKey: DelphiHKEY; const Key, Name, Value: AnsiString); overload;
procedure RegWriteAnsiString(const RootKey: DelphiHKEY; const Key, Name: AnsiString; DataType: Cardinal; Value: AnsiString); overload;
procedure RegWriteWideString(const RootKey: DelphiHKEY; const Key, Name: string; Value: WideString); overload;
procedure RegWriteWideString(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: WideString); overload;

procedure RegWriteMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: PMultiSz); overload;
procedure RegWriteMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; const Value: TStrings); overload;
procedure RegWriteMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: PMultiSz); overload;
procedure RegWriteMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; const Value: TStrings); overload;
procedure RegWriteWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: PWideMultiSz); overload;
procedure RegWriteWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; const Value: TWideStrings); overload;
procedure RegWriteWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: PWideMultiSz); overload;
procedure RegWriteWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; const Value: TWideStrings); overload;

procedure RegWriteBinary(const RootKey: DelphiHKEY; const Key, Name: string; const Value; const ValueSize: Cardinal);

function RegGetValueNames(const RootKey: DelphiHKEY; const Key: string; const List: TStrings): Boolean;
function RegGetKeyNames(const RootKey: DelphiHKEY; const Key: string; const List: TStrings): Boolean;
function RegHasSubKeys(const RootKey: DelphiHKEY; const Key: string): Boolean;

{
From: Jean-Fabien Connault [cycocrew att worldnet dott fr]
Descr: Test whether a registry key exists as a subkey of RootKey
Used test cases:
procedure TForm1.Button1Click(Sender: TObject);
var
  RegKey: HKEY;
begin
  if RegOpenKeyEx(HKEY_CURRENT_USER, 'Software', 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    Assert(not RegKeyExists(RegKey, 'Microsoft\_Windows'));
    RegCloseKey(RegKey);
  end;
  if RegOpenKeyEx(HKEY_CURRENT_USER, 'Software', 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    Assert(RegKeyExists(RegKey, 'Microsoft\Windows'));;
    RegCloseKey(RegKey);
  end;
  Assert(RegKeyExists(HKEY_CURRENT_USER, ''));
  Assert(RegKeyExists(HKEY_CURRENT_USER, 'Software'));
  Assert(RegKeyExists(HKEY_CURRENT_USER, 'Software\Microsoft'));
  Assert(RegKeyExists(HKEY_CURRENT_USER, 'Software\Microsoft\Windows'));
  Assert(RegKeyExists(HKEY_CURRENT_USER, '\Software\Microsoft\Windows'));
  Assert(not RegKeyExists(HKEY_CURRENT_USER, '\Software\Microsoft2\Windows'));
end;
}
function RegKeyExists(const RootKey: DelphiHKEY; const Key: string): Boolean;

function UnregisterAutoExec(ExecKind: TExecKind; const Name: string): Boolean;
function RegisterAutoExec(ExecKind: TExecKind; const Name, Cmdline: string): Boolean;

function RegSaveList(const RootKey: DelphiHKEY; const Key: string; const ListName: string;
  const Items: TStrings): Boolean;
function RegLoadList(const RootKey: DelphiHKEY; const Key: string; const ListName: string;
  const SaveTo: TStrings): Boolean;
function RegDelList(const RootKey: DelphiHKEY; const Key: string; const ListName: string): Boolean;

implementation

uses
  SysUtils,
  JclResources, JclSysUtils, JclWin32;

type
  TRegKind = REG_NONE..REG_QWORD;
  TRegKinds = set of TRegKind;

const
  cItems = 'Items';
  cRegBinKinds = [REG_SZ..REG_QWORD];  // all types

//==================================================================================================
// Internal helper routines
//==================================================================================================

procedure ReadError(const Key: string);
begin
  raise EJclRegistryError.CreateResRecFmt(@RsUnableToOpenKeyRead, [Key]);
end;

//--------------------------------------------------------------------------------------------------

procedure WriteError(const Key: string);
begin
  raise EJclRegistryError.CreateResRecFmt(@RsUnableToOpenKeyWrite, [Key]);
end;

//--------------------------------------------------------------------------------------------------

procedure ValueError(const Key, Name: string);
begin
  raise EJclRegistryError.CreateResRecFmt(@RsUnableToAccessValue, [Key, Name]);
end;

//--------------------------------------------------------------------------------------------------

procedure DataError(const Key, Name: string);
begin
  raise EJclRegistryError.CreateResRecFmt(@RsWrongDataType, [Key, Name]);
end;

//--------------------------------------------------------------------------------------------------

function GetKeyAndPath(ExecKind: TExecKind; var Key: HKEY; out RegPath: string): Boolean;
begin
  Result := False;
  if (ExecKind in [ekServiceRun, ekServiceRunOnce]) and (Win32Platform = VER_PLATFORM_WIN32_NT) then
    Exit;
  if ExecKind in [ekMachineRun, ekMachineRunOnce, ekServiceRun, ekServiceRunOnce] then
    Key := HKEY_LOCAL_MACHINE
  else
    Key := HKEY_CURRENT_USER;
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

//--------------------------------------------------------------------------------------------------

function RelativeKey(Key: PChar): PChar;
begin
  Result := Key;
  if Result^ = '\' then
    Inc(Result);
end;

//--------------------------------------------------------------------------------------------------

function RelativeKeyW(Key: PWideChar): PWideChar;
begin
  Result := Key;
  if Result^ = WideChar('\') then
    Inc(Result);
end;

//--------------------------------------------------------------------------------------------------

procedure InternalGetData(const RootKey: DelphiHKEY; const Key, Name: string;
  RegKinds: TRegKinds; ExpectedSize: DWORD;
  out DataType: DWORD; Data: Pointer; out DataSize: DWORD);
var
  RegKey: HKEY;
  WideKey, WideName: WideString;
begin
  DataType := REG_NONE;
  DataSize := 0;
  WideKey := Key;
  WideName := Name;
  // using Unicode functions to avoid REG_SZ transformations
  if RegOpenKeyExW(RootKey, RelativeKeyW(PWideChar(WideKey)), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    try
      if RegQueryValueExW(RegKey, PWideChar(WideName), nil, @DataType, nil, @DataSize) = ERROR_SUCCESS then
      begin
        if not (DataType in RegKinds) or (DataSize > ExpectedSize) then
          DataError(Key, Name);
        if RegQueryValueExW(RegKey, PWideChar(WideName), nil, nil, Data, @DataSize) <> ERROR_SUCCESS then
          ValueError(Key, Name);
      end
      else
        ValueError(Key, Name);
    finally
      RegCloseKey(RegKey);
    end;
  end
  else
    ReadError(Key);
end;

//--------------------------------------------------------------------------------------------------

function InternalGetString(const RootKey: DelphiHKEY; const Key, Name: string; MultiFlag: Boolean): string;
var
  RegKey: HKEY;
  DataType, DataSize: DWORD;
  RegKinds: TRegKinds;
begin
  DataType := REG_NONE;
  DataSize := 0;
  Result := '';
  if RegOpenKeyEx(RootKey, RelativeKey(PChar(Key)), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    try
      if RegQueryValueEx(RegKey, PChar(Name), nil, @DataType, nil, @DataSize) = ERROR_SUCCESS then
      begin
        RegKinds := [REG_BINARY, REG_SZ, REG_EXPAND_SZ];
        if MultiFlag then
          RegKinds := RegKinds + [REG_MULTI_SZ];
        if not (DataType in RegKinds) then
          DataError(Key, Name);
        SetLength(Result, DataSize div SizeOf(Char) + 1);
        if RegQueryValueEx(RegKey, PChar(Name), nil, nil, Pointer(Result), @DataSize) <> ERROR_SUCCESS then
        begin
          Result := '';
          ValueError(Key, Name);
        end;
        SetLength(Result, (DataSize - 1) div SizeOf(Char));
      end
      else
        ValueError(Key, Name);
    finally
      RegCloseKey(RegKey);
    end;
  end
  else
    ReadError(Key);
end;

//--------------------------------------------------------------------------------------------------

function InternalGetWideString(const RootKey: DelphiHKEY; const Key, Name: string; MultiFlag: Boolean): WideString;
var
  RegKey: HKEY;
  DataType, DataSize: DWORD;
  WideKey, WideName: WideString;
  RegKinds: TRegKinds;
begin
  DataType := REG_NONE;
  DataSize := 0;
  WideKey := Key;
  WideName := Name;
  Result := '';
  if RegOpenKeyExW(RootKey, RelativeKeyW(PWideChar(WideKey)), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    try
      if RegQueryValueExW(RegKey, PWideChar(WideName), nil, @DataType, nil, @DataSize) = ERROR_SUCCESS then
      begin
        if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
          RegKinds := [REG_BINARY]
        else
        if MultiFlag then
          RegKinds := [REG_BINARY, REG_SZ, REG_EXPAND_SZ, REG_MULTI_SZ]
        else
          RegKinds := [REG_BINARY, REG_SZ, REG_EXPAND_SZ];
        if not (DataType in RegKinds) then
          DataError(Key, Name);
        SetLength(Result, DataSize div SizeOf(WideChar) + 1);
        if RegQueryValueExW(RegKey, PWideChar(WideName), nil, nil, Pointer(Result), @DataSize) <> ERROR_SUCCESS then
        begin
          Result := '';
          ValueError(Key, Name);
        end;
        SetLength(Result, (DataSize - 1) div SizeOf(WideChar));
      end
      else
        ValueError(Key, Name);
    finally
      RegCloseKey(RegKey);
    end;
  end
  else
    ReadError(Key);
end;

//--------------------------------------------------------------------------------------------------

procedure InternalRegSetData(const RootKey: DelphiHKEY; const Key, Name: string;
  RegKind: TRegKind; Value: Pointer; ValueSize: Cardinal);
var
  RegKey: HKEY;
begin
  if not RegKeyExists(RootKey, Key) then
    RegCreateKey(RootKey, Key);
  if RegOpenKeyEx(RootKey, RelativeKey(PChar(Key)), 0, KEY_WRITE, RegKey) = ERROR_SUCCESS then
    try
      if RegSetValueEx(RegKey, PChar(Name), 0, RegKind, Value, ValueSize) <> ERROR_SUCCESS then
      WriteError(Key);
    finally
      RegCloseKey(RegKey);
    end
  else
    WriteError(Key);
end;

//--------------------------------------------------------------------------------------------------

procedure InternalRegSetWideData(const RootKey: DelphiHKEY; const Key, Name: string;
  RegKind: TRegKind; Value: Pointer; ValueSize: Cardinal);
var
  RegKey: HKEY;
  WideKey, WideName: WideString;
begin
  if not RegKeyExists(RootKey, Key) then
    RegCreateKey(RootKey, Key);
  WideKey := Key;
  WideName := Name;
  if RegOpenKeyExW(RootKey, RelativeKeyW(PWideChar(WideKey)), 0, KEY_WRITE, RegKey) = ERROR_SUCCESS then
    try
      if RegSetValueExW(RegKey, PWideChar(WideName), 0, RegKind, Value, ValueSize) <> ERROR_SUCCESS then
      WriteError(Key);
    finally
      RegCloseKey(RegKey);
    end
  else
    WriteError(Key);
end;

//==================================================================================================
// Registry
//==================================================================================================

function RegCreateKey(const RootKey: DelphiHKEY; const Key: string): Longint;
var
  RegKey: HKEY;
begin
  Result := Windows.RegCreateKey(RootKey, RelativeKey(PChar(Key)), RegKey);
  if Result = ERROR_SUCCESS then
    RegCloseKey(RegKey);
end;

//--------------------------------------------------------------------------------------------------

function RegCreateKey(const RootKey: DelphiHKEY; const Key, Value: string): Longint;
begin
  Result := RegSetValue(RootKey, RelativeKey(PChar(Key)), REG_SZ, PChar(Value), Length(Value));
end;

//--------------------------------------------------------------------------------------------------

function RegDeleteEntry(const RootKey: DelphiHKEY; const Key, Name: string): Boolean;
var
  RegKey: HKEY;
begin
  Result := False;
  if RegOpenKeyEx(RootKey, RelativeKey(PChar(Key)), 0, KEY_SET_VALUE, RegKey) = ERROR_SUCCESS then
  begin
    Result := RegDeleteValue(RegKey, PChar(Name)) = ERROR_SUCCESS;
    RegCloseKey(RegKey);
    if not Result then
      ValueError(Key, Name);
  end
  else
    WriteError(Key);
end;

//--------------------------------------------------------------------------------------------------

function RegDeleteKeyTree(const RootKey: DelphiHKEY; const Key: string): Boolean;
var
  RegKey: HKEY;
  I: DWORD;
  Size: DWORD;
  NumSubKeys: DWORD;
  MaxSubKeyLen: DWORD;
  KeyName: string;
begin
  Result := RegOpenKeyEx(RootKey, RelativeKey(PChar(Key)), 0, KEY_ALL_ACCESS, RegKey) = ERROR_SUCCESS;
  if Result then
  begin
    RegQueryInfoKey(RegKey, nil, nil, nil, @NumSubKeys, @MaxSubKeyLen, nil, nil, nil, nil, nil, nil);
    if NumSubKeys <> 0 then
      for I := NumSubKeys - 1 downto 0 do
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
      Result := Windows.RegDeleteKey(RootKey, RelativeKey(PChar(Key))) = ERROR_SUCCESS;
    end
    else
      WriteError(Key);
end;

//--------------------------------------------------------------------------------------------------

function RegGetDataSize(const RootKey: DelphiHKEY; const Key, Name: string;
  out DataSize: Cardinal): Boolean;
var
  RegKey: HKEY;
begin
  DataSize := 0;
  Result := RegOpenKeyEx(RootKey, RelativeKey(PChar(Key)), 0, KEY_READ, RegKey) = ERROR_SUCCESS;
  if Result then
  begin
    Result := RegQueryValueEx(RegKey, PChar(Name), nil, nil, nil, @DataSize) = ERROR_SUCCESS;
    RegCloseKey(RegKey);
  end;
end;

//--------------------------------------------------------------------------------------------------

function RegGetDataType(const RootKey: DelphiHKEY; const Key, Name: string;
  out DataType: DWORD): Boolean;
var
  RegKey: HKEY;
begin
  DataType := REG_NONE;
  Result := RegOpenKeyEx(RootKey, RelativeKey(PChar(Key)), 0, KEY_READ, RegKey) = ERROR_SUCCESS;
  if Result then
  begin
    Result := RegQueryValueEx(RegKey, PChar(Name), nil, @DataType, nil, nil) = ERROR_SUCCESS;
    RegCloseKey(RegKey);
  end;
end;

//--------------------------------------------------------------------------------------------------

function RegReadBool(const RootKey: DelphiHKEY; const Key, Name: string): Boolean;
begin
  Result := RegReadInteger(RootKey, Key, Name) <> 0;
end;

//--------------------------------------------------------------------------------------------------

function RegReadBoolDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Boolean): Boolean;
begin
  Result := RegReadIntegerDef(RootKey, Key, Name, Ord(Def)) <> 0;
end;

//--------------------------------------------------------------------------------------------------

function RegReadInteger(const RootKey: DelphiHKEY; const Key, Name: string): Integer;
var
  DataType, DataSize: DWORD;
  Ret: Int64;
begin
  Ret := 0;
  RegGetDataType(RootKey, Key, Name, DataType);
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    Ret := StrToInt64(RegReadString(RootKey, Key, Name))
  else
    InternalGetData(RootKey, Key, Name, [REG_BINARY, REG_DWORD, REG_QWORD],
      SizeOf(Ret), DataType, @Ret, DataSize);
  Result := Ret and $FFFFFFFF;
end;

//--------------------------------------------------------------------------------------------------

function RegReadIntegerDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Integer): Integer;
begin
  try
    Result := RegReadInteger(RootKey, Key, Name);
  except
    Result := Def;
  end;
end;

//--------------------------------------------------------------------------------------------------

function RegReadCardinal(const RootKey: DelphiHKEY; const Key, Name: string): Cardinal;
var
  DataType, DataSize: DWORD;
  Ret: Int64;
begin
  Ret := 0;
  RegGetDataType(RootKey, Key, Name, DataType);
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    Ret := StrToInt64(RegReadString(RootKey, Key, Name))
  else
    InternalGetData(RootKey, Key, Name, [REG_BINARY, REG_DWORD, REG_QWORD],
      SizeOf(Ret), DataType, @Ret, DataSize);
  Result := Ret and $FFFFFFFF;
end;

//--------------------------------------------------------------------------------------------------

function RegReadCardinalDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Cardinal): Cardinal;
begin
  try
    Result := RegReadCardinal(RootKey, Key, Name);
  except
    Result := Def;
  end;
end;

//--------------------------------------------------------------------------------------------------

function RegReadDWORD(const RootKey: DelphiHKEY; const Key, Name: string): DWORD;
begin
  Result := RegReadCardinal(RootKey, Key, Name);
end;

//--------------------------------------------------------------------------------------------------

function RegReadDWORDDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: DWORD): DWORD;
begin
  Result := RegReadCardinalDef(RootKey, Key, Name, Def);
end;

//--------------------------------------------------------------------------------------------------

function RegReadInt64(const RootKey: DelphiHKEY; const Key, Name: string): Int64;
var
  DataType, DataSize: DWORD;
  Data: array [0..1] of Integer;
  Ret: Int64;
begin
  RegGetDataType(RootKey, Key, Name, DataType);
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
  begin
    // (rom) circumvents internal compiler error for D6
    Ret := StrToInt64(RegReadString(RootKey, Key, Name));
    Result := Ret;
  end
  else
  begin
    FillChar(Data[0], SizeOf(Data), 0);
    InternalGetData(RootKey, Key, Name, [REG_BINARY, REG_DWORD, REG_QWORD],
       SizeOf(Data), DataType, @Data, DataSize);
    // REG_BINARY is implicitly unsigned if DataSize < 8
    if DataType = REG_DWORD then
      // DWORDs get sign extended
      Result := Data[0]
    else
      Move(Data[0], Result, SizeOf(Data));
  end;
end;

//--------------------------------------------------------------------------------------------------

function RegReadInt64Def(const RootKey: DelphiHKEY; const Key, Name: string; Def: Int64): Int64;
begin
  try
    Result := RegReadInt64(RootKey, Key, Name);
  except
    Result := Def;
  end;
end;

//--------------------------------------------------------------------------------------------------

function RegReadUInt64(const RootKey: DelphiHKEY; const Key, Name: string): UInt64;
var
  DataType, DataSize: DWORD;
  Ret: Int64;
begin
  RegGetDataType(RootKey, Key, Name, DataType);
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
  begin
    // (rom) circumvents internal compiler error for D6
    Ret := StrToInt64(RegReadString(RootKey, Key, Name));
    Result := UInt64(Ret);
  end
  else
  begin
    // type cast required to circumvent internal error in D7
    Result := UInt64(0);
    InternalGetData(RootKey, Key, Name, [REG_BINARY, REG_DWORD, REG_QWORD],
      SizeOf(Result), DataType, @Result, DataSize);
  end;
end;

//--------------------------------------------------------------------------------------------------

function RegReadUInt64Def(const RootKey: DelphiHKEY; const Key, Name: string; Def: UInt64): UInt64;
begin
  try
    Result := RegReadUInt64(RootKey, Key, Name);
  except
    Result := Def;
  end;
end;

//--------------------------------------------------------------------------------------------------

function RegReadString(const RootKey: DelphiHKEY; const Key, Name: string): string;
begin
  Result := RegReadAnsiString(RootKey, Key, Name);
end;

//--------------------------------------------------------------------------------------------------

function RegReadStringDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: string): string;
begin
  Result := RegReadAnsiStringDef(RootKey, Key, Name, Def);
end;

//--------------------------------------------------------------------------------------------------

function RegReadAnsiString(const RootKey: DelphiHKEY; const Key, Name: AnsiString): AnsiString;
begin
  Result := InternalGetString(RootKey, Key, Name, False);
end;

//--------------------------------------------------------------------------------------------------

function RegReadAnsiStringDef(const RootKey: DelphiHKEY; const Key, Name: AnsiString; Def: AnsiString): AnsiString;
begin
  try
    Result := RegReadAnsiString(RootKey, Key, Name);
  except
    Result := Def;
  end;
end;

//--------------------------------------------------------------------------------------------------

function RegReadWideString(const RootKey: DelphiHKEY; const Key, Name: string): WideString;
begin
  Result := InternalGetWideString(RootKey, Key, Name, False);
end;

//--------------------------------------------------------------------------------------------------

function RegReadWideStringDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: WideString): WideString;
begin
  try
    Result := RegReadWideString(RootKey, Key, Name);
  except
    Result := Def;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure RegReadMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: TStrings);
var
  S: string;
begin
  S := InternalGetString(RootKey, Key, Name, True);
  MultiSzToStrings(Value, PMultiSz(PChar(S)));
end;

//--------------------------------------------------------------------------------------------------

procedure RegReadMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string; Value, Def: TStrings);
begin
  try
    RegReadMultiSz(RootKey, Key, Name, Value);
  except
    Value.Assign(Def);
  end;
end;

//--------------------------------------------------------------------------------------------------

function RegReadMultiSz(const RootKey: DelphiHKEY; const Key, Name: string): PMultiSz;
var
  S: string;
begin
  S := InternalGetString(RootKey, Key, Name, True);
  // always returns a newly allocated PMultiSz
  Result := MultiSzDup(PMultiSz(PChar(S)));
end;

//--------------------------------------------------------------------------------------------------

function RegReadMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: PMultiSz): PMultiSz;
begin
  try
    Result := RegReadMultiSz(RootKey, Key, Name);
  except
    // always returns a newly allocated PMultiSz
    Result := MultiSzDup(Def);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure RegReadWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: TWideStrings);
var
  S: WideString;
begin
  S := InternalGetWideString(RootKey, Key, Name, True);
  WideMultiSzToWideStrings(Value, PWideMultiSz(PWideChar(S)));
end;

//--------------------------------------------------------------------------------------------------

procedure RegReadWideMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string; Value, Def: TWideStrings);
begin
  try
    RegReadWideMultiSz(RootKey, Key, Name, Value);
  except
    Value.Assign(Def);
  end;
end;

//--------------------------------------------------------------------------------------------------

function RegReadWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string): PWideMultiSz;
var
  S: WideString;
begin
  S := InternalGetWideString(RootKey, Key, Name, True);
  // always returns a newly allocated PMultiWideSz
  Result := WideMultiSzDup(PWideMultiSz(PWideChar(S)));
end;

//--------------------------------------------------------------------------------------------------

function RegReadWideMultiSzDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: PWideMultiSz): PWideMultiSz;
begin
  try
    Result := RegReadWideMultiSz(RootKey, Key, Name);
  except
    // always returns a newly allocated PWideMultiSz
    Result := WideMultiSzDup(Def);
  end;
end;

//--------------------------------------------------------------------------------------------------

function RegReadBinary(const RootKey: DelphiHKEY; const Key, Name: string; var Value;
  const ValueSize: Cardinal): Cardinal;
var
  DataType: DWORD;
begin
  InternalGetData(RootKey, Key, Name, cRegBinKinds, ValueSize, DataType, @Value, Result);
end;

//--------------------------------------------------------------------------------------------------

function RegReadBinaryDef(const RootKey: DelphiHKEY; const Key, Name: string;
  var Value; const ValueSize: Cardinal; const Def: Byte): Cardinal;
begin
  try
    Result := RegReadBinary(RootKey, Key, Name, Value, ValueSize);
  except
    FillChar(Value, ValueSize, Def);
    Result := ValueSize;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteBool(const RootKey: DelphiHKEY; const Key, Name: string; Value: Boolean);
begin
  RegWriteCardinal(RootKey, Key, Name, REG_DWORD, Cardinal(Ord(Value)));
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteBool(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: Boolean);
begin
  RegWriteCardinal(RootKey, Key, Name, DataType, Cardinal(Ord(Value)));
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteInteger(const RootKey: DelphiHKEY; const Key, Name: string; Value: Integer);
begin
  RegWriteInteger(RootKey, Key, Name, REG_DWORD, Cardinal(Value));
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteInteger(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: Integer);
var
  Val: Int64;
  Size: Integer;
begin
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    RegWriteString(RootKey, Key, Name, DataType, Format('%d', [Value]))
  else
  if DataType in [REG_DWORD, REG_QWORD, REG_BINARY] then
  begin
    // sign extension
    Val := Value;
    if DataType = REG_QWORD then
      Size := SizeOf(Int64)
    else
      Size := SizeOf(Value);
    InternalRegSetData(RootKey, Key, Name, DataType, @Val, Size);
  end
  else
    DataError(Key, Name);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteCardinal(const RootKey: DelphiHKEY; const Key, Name: string; Value: Cardinal);
begin
  RegWriteCardinal(RootKey, Key, Name, REG_DWORD, Cardinal(Value));
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteCardinal(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: Cardinal);
var
  Val: Int64;
  Size: Integer;
begin
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    RegWriteString(RootKey, Key, Name, DataType, Format('%u', [Value]))
  else
  if DataType in [REG_DWORD, REG_QWORD, REG_BINARY] then
  begin
    // no sign extension
    Val := Value and $FFFFFFFF;
    if DataType = REG_QWORD then
      Size := SizeOf(Int64)
    else
      Size := SizeOf(Value);
    InternalRegSetData(RootKey, Key, Name, DataType, @Val, Size);
  end
  else
    DataError(Key, Name);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteDWORD(const RootKey: DelphiHKEY; const Key, Name: string; Value: DWORD);
begin
  RegWriteCardinal(RootKey, Key, Name, REG_DWORD, Cardinal(Value));
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteDWORD(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: DWORD);
begin
  RegWriteCardinal(RootKey, Key, Name, DataType, Cardinal(Value));
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteInt64(const RootKey: DelphiHKEY; const Key, Name: string; Value: Int64);
begin
  RegWriteInt64(RootKey, Key, Name, REG_QWORD, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteInt64(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: Int64);
begin
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    RegWriteString(RootKey, Key, Name, DataType, Format('%d', [Value]))
  else
    RegWriteUInt64(RootKey, Key, Name, DataType, UInt64(Value));
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteUInt64(const RootKey: DelphiHKEY; const Key, Name: string; Value: UInt64);
begin
  RegWriteUInt64(RootKey, Key, Name, REG_QWORD, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteUInt64(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: UInt64);
begin
  if DataType in [REG_SZ, REG_EXPAND_SZ] then
    RegWriteString(RootKey, Key, Name, DataType, Format('%u', [Value]))
  else
  if DataType in [REG_QWORD, REG_BINARY] then
    InternalRegSetData(RootKey, Key, Name, DataType, @Value, SizeOf(Value))
  else
    DataError(Key, Name);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteString(const RootKey: DelphiHKEY; const Key, Name, Value: string);
begin
  RegWriteAnsiString(RootKey, Key, Name, REG_SZ, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteString(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: string);
begin
  RegWriteAnsiString(RootKey, Key, Name, DataType, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteAnsiString(const RootKey: DelphiHKEY; const Key, Name, Value: AnsiString);
begin
  RegWriteAnsiString(RootKey, Key, Name, REG_SZ, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteAnsiString(const RootKey: DelphiHKEY; const Key, Name: AnsiString; DataType: Cardinal; Value: AnsiString);
begin
  if DataType in [REG_BINARY, REG_SZ, REG_EXPAND_SZ] then
    InternalRegSetData(RootKey, Key, Name, DataType, PChar(Value),
      (Length(Value) + 1) * SizeOf(AnsiChar))
  else
    DataError(Key, Name);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteWideString(const RootKey: DelphiHKEY; const Key, Name: string; Value: WideString);
begin
  RegWriteWideString(RootKey, Key, Name, REG_SZ, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteWideString(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: WideString);
begin
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
    InternalRegSetWideData(RootKey, Key, Name, REG_BINARY, PWideChar(Value),
      (Length(Value) + 1) * SizeOf(WideChar))
  else
  if DataType in [REG_BINARY, REG_SZ, REG_EXPAND_SZ] then
    InternalRegSetWideData(RootKey, Key, Name, DataType, PWideChar(Value),
      (Length(Value) + 1) * SizeOf(WideChar))
  else
    DataError(Key, Name);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: PMultiSz);
begin
  RegWriteMultiSz(RootKey, Key, Name, REG_MULTI_SZ, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: PMultiSz);
begin
  if DataType in [REG_BINARY, REG_MULTI_SZ] then
    InternalRegSetData(RootKey, Key, Name, DataType, Value,
      MultiSzLength(Value) * SizeOf(Char))
  else
    DataError(Key, Name);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; const Value: TStrings);
begin
  RegWriteMultiSz(RootKey, Key, Name, REG_MULTI_SZ, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; const Value: TStrings);
var
  Dest: PMultiSz;
begin
  if DataType in [REG_BINARY, REG_MULTI_SZ] then
  begin
    StringsToMultiSz(Dest, Value);
    try
      RegWriteMultiSz(RootKey, Key, Name, DataType, Dest);
    finally
      FreeMultiSz(Dest);
    end;
  end
  else
    DataError(Key, Name);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; Value: PWideMultiSz);
begin
  RegWriteWideMultiSz(RootKey, Key, Name, REG_MULTI_SZ, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; Value: PWideMultiSz);
begin
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
    DataType := REG_BINARY;
  if DataType in [REG_BINARY, REG_MULTI_SZ] then
    InternalRegSetWideData(RootKey, Key, Name, DataType, Value,
      WideMultiSzLength(Value) * SizeOf(WideChar))
  else
    DataError(Key, Name);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; const Value: TWideStrings);
begin
  RegWriteWideMultiSz(RootKey, Key, Name, REG_MULTI_SZ, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteWideMultiSz(const RootKey: DelphiHKEY; const Key, Name: string; DataType: Cardinal; const Value: TWideStrings);
var
  Dest: PWideMultiSz;
begin
  if DataType in [REG_BINARY, REG_MULTI_SZ] then
  begin
    WideStringsToWideMultiSz(Dest, Value);
    try
      RegWriteWideMultiSz(RootKey, Key, Name, DataType, Dest);
    finally
      FreeWideMultiSz(Dest);
    end;
  end
  else
    DataError(Key, Name);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteBinary(const RootKey: DelphiHKEY; const Key, Name: string; const Value; const ValueSize: Cardinal);
begin
  InternalRegSetData(RootKey, Key, Name, REG_BINARY, @Value, ValueSize);
end;

//--------------------------------------------------------------------------------------------------

function UnregisterAutoExec(ExecKind: TExecKind; const Name: string): Boolean;
var
  Key: HKEY;
  RegPath: string;
begin
  Result := GetKeyAndPath(ExecKind, Key, RegPath);
  if Result then
    Result := RegDeleteEntry(Key, RegPath, Name);
end;

//--------------------------------------------------------------------------------------------------

function RegisterAutoExec(ExecKind: TExecKind; const Name, Cmdline: string): Boolean;
var
  Key: HKEY;
  RegPath: string;
begin
  Result := GetKeyAndPath(ExecKind, Key, RegPath);
  if Result then
    RegWriteString(Key, RegPath, Name, Cmdline);
end;

//--------------------------------------------------------------------------------------------------

function RegGetValueNames(const RootKey: DelphiHKEY; const Key: string; const List: TStrings): Boolean;
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
  List.BeginUpdate;
  try
    List.Clear;
    if RegOpenKeyEx(RootKey, RelativeKey(PChar(Key)), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
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
  finally
    List.EndUpdate;
  end;
end;

//--------------------------------------------------------------------------------------------------

function RegGetKeyNames(const RootKey: DelphiHKEY; const Key: string; const List: TStrings): Boolean;
var
  RegKey: HKEY;
  I: DWORD;
  Size: DWORD;
  NumSubKeys: DWORD;
  MaxSubKeyLen: DWORD;
  KeyName: string;
begin
  Result := False;
  List.BeginUpdate;
  try
    List.Clear;
    if RegOpenKeyEx(RootKey, RelativeKey(PChar(Key)), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
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
  finally
    List.EndUpdate;
  end;
end;

//--------------------------------------------------------------------------------------------------

function RegHasSubKeys(const RootKey: DelphiHKEY; const Key: string): Boolean;
var
  RegKey: HKEY;
  NumSubKeys: Integer;
begin
  Result := False;
  if RegOpenKeyEx(RootKey, RelativeKey(PChar(Key)), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    RegQueryInfoKey(RegKey, nil, nil, nil, @NumSubKeys, nil, nil, nil, nil, nil, nil, nil);
    Result := NumSubKeys <> 0;
    RegCloseKey(RegKey);
  end
  else
    ReadError(Key);
end;

//--------------------------------------------------------------------------------------------------

function RegKeyExists(const RootKey: DelphiHKEY; const Key: string): Boolean;
var
  RegKey: HKEY;
begin
  Result := (RegOpenKeyEx(RootKey, RelativeKey(PChar(Key)), 0, KEY_READ, RegKey) = ERROR_SUCCESS);
  if Result then
    RegCloseKey(RegKey);
end;

//--------------------------------------------------------------------------------------------------

function RegSaveList(const RootKey: DelphiHKEY; const Key: string;
  const ListName: string; const Items: TStrings): Boolean;
var
  I: Integer;
  SubKey: string;
begin
  Result := False;
  SubKey := Key + '\' + ListName;
  if RegCreateKey(RootKey, SubKey) = ERROR_SUCCESS then
  begin
    // Save Number of strings
    RegWriteInteger(RootKey, SubKey, cItems, Items.Count);
    for I := 1 to Items.Count do
      RegWriteString(RootKey, SubKey, IntToStr(I), Items[I-1]);
    Result := True;
  end;
end;

//--------------------------------------------------------------------------------------------------

function RegLoadList(const RootKey: DelphiHKEY; const Key: string;
  const ListName: string; const SaveTo: TStrings): Boolean;
var
  I, N: Integer;
  SubKey: string;
begin
  SaveTo.BeginUpdate;
  try
    SaveTo.Clear;
    SubKey := Key + '\' + ListName;
    N := RegReadInteger(RootKey, SubKey, cItems);
    for I := 1 to N do
      SaveTo.Add(RegReadString(RootKey, SubKey, IntToStr(I)));
    Result := N > 0;
  finally
    SaveTo.EndUpdate;
  end;
end;

//--------------------------------------------------------------------------------------------------

function RegDelList(const RootKey: DelphiHKEY; const Key: string; const ListName: string): Boolean;
var
  I, N: Integer;
  SubKey: string;
begin
  Result := False;
  SubKey := Key + '\' + ListName;
  N := RegReadIntegerDef(RootKey, SubKey, cItems, -1);
  if (N > 0) and RegDeleteEntry(RootKey, SubKey, cItems) then
    for I := 1 to N do
    begin
      Result := RegDeleteEntry(RootKey, SubKey, IntToStr(I));
      if not Result then
        Break;
    end;
end;

// History:

// $Log$
// Revision 1.27  2004/10/21 06:38:53  marquardt
// style clenaing, bugfixes, improvements
//
// Revision 1.26  2004/10/20 17:13:53  rrossmair
// - fixed RegReadUInt64 (DataType undefined)
//
// Revision 1.25  2004/10/20 16:57:32  rrossmair
// - RegReadUInt64: D7 internal error C1118 workaround
//
// Revision 1.24  2004/10/19 06:27:03  marquardt
// JclRegistry extended, JclNTFS made compiling, JclDateTime style cleaned
//
// Revision 1.23  2004/10/18 16:22:14  marquardt
// JclRegistry redesign to remove PH contributor
//
// Revision 1.22  2004/10/17 21:00:15  mthoma
// cleaning
//
// Revision 1.21  2004/10/11 08:13:04  marquardt
// PH cleaning of JclStrings
//
// Revision 1.20  2004/09/30 07:50:29  marquardt
// remove PH contributions
//
// Revision 1.19  2004/07/31 06:21:03  marquardt
// fixing TStringLists, adding BeginUpdate/EndUpdate, finalization improved
//
// Revision 1.18  2004/07/28 18:00:53  marquardt
// various style cleanings, some minor fixes
//
// Revision 1.17  2004/06/14 13:05:21  marquardt
// style cleaning ENDIF, Tabs
//
// Revision 1.16  2004/06/14 11:05:53  marquardt
// symbols added to all ENDIFs and some other minor style changes like removing IFOPT
//
// Revision 1.15  2004/05/31 22:45:07  rrossmair
// rollback to rev. 1.13 state
//
// Revision 1.13  2004/05/19 21:43:36  rrossmair
// processed help TODOs
//
// Revision 1.12  2004/05/05 07:33:49  rrossmair
// header updated according to new policy: initial developers & contributors listed
//
// Revision 1.11  2004/04/12 22:02:53  peterjhaas
// Bugfix RegReadBinary for @Value = Nil or ValueSize = 0,
// add some WideString support, add RegGetDataSize, RegGetDataType, add alternative RegReadBinary function
//
// Revision 1.10  2004/04/08 13:46:38  ahuser
// BCB 6 compatible (no impact on Delphi)
//
// Revision 1.9  2004/04/08 10:34:58  rrossmair
// revert to 1.7 (temporarily?)
//
// Revision 1.7  2004/04/06 05:56:10  rrossmair
// fixed RegReadUInt64 & RegReadUInt64Def
//
// Revision 1.6  2004/04/06 04:45:57  
// Unite the single read functions and the single write functions, add Cardinal,
// Int64, UInt64 and Multistring support

end.

