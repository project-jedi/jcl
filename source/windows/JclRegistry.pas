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
{ The Initial Developers of the Original Code are documented in the accompanying help file         }
{ JCLHELP.hlp. Portions created by these individuals are Copyright (C) of these individuals.       }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Eric S.Fisher                                                                                  }
{   Peter J. Haas (PeterJHaas), jediplus@pjh2.de                                                   }
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
  JclBase;

//--------------------------------------------------------------------------------------------------
// Registry
//--------------------------------------------------------------------------------------------------

function RegCreateKey(const RootKey: HKEY; const Key, Value: string): Longint;
function RegDeleteEntry(const RootKey: HKEY; const Key, Name: string): Boolean;
function RegDeleteKeyTree(const RootKey: HKEY; const Key: string): Boolean;

function RegReadBool(const RootKey: HKEY; const Key, Name: string): Boolean;
function RegReadBoolDef(const RootKey: HKEY; const Key, Name: string; Def: Boolean): Boolean;
{ TODO -cHelp : Contributer: Peter J. Haas }
function RegReadInteger(const RootKey: HKEY; const Key, Name: string): Integer;
{ TODO -cHelp : Contributer: Peter J. Haas }
function RegReadIntegerDef(const RootKey: HKEY; const Key, Name: string; Def: Integer): Integer;
{ TODO -cHelp : Author: Peter J. Haas }
function RegReadCardinal(const RootKey: HKEY; const Key, Name: string): Cardinal;
{ TODO -cHelp : Author: Peter J. Haas }
function RegReadCardinalDef(const RootKey: HKEY; const Key, Name: string; Def: Cardinal): Cardinal;
{ TODO -cHelp : Author: Peter J. Haas }
function RegReadInt64(const RootKey: HKEY; const Key, Name: string): Int64;
{ TODO -cHelp : Author: Peter J. Haas }
function RegReadInt64Def(const RootKey: HKEY; const Key, Name: string; Def: Int64): Int64;
{ TODO -cHelp : Author: Peter J. Haas }
function RegReadUInt64(const RootKey: HKEY; const Key, Name: string): UInt64;
{ TODO -cHelp : Author: Peter J. Haas }
function RegReadUInt64Def(const RootKey: HKEY; const Key, Name: string; Def: UInt64): UInt64;
{ TODO -cHelp : Contributer: Peter J. Haas }
function RegReadString(const RootKey: HKEY; const Key, Name: string): string;
{ TODO -cHelp : Contributer: Peter J. Haas }
function RegReadStringDef(const RootKey: HKEY; const Key, Name, Def: string): string;
{ TODO -cHelp : Author: Peter J. Haas }
function RegReadMultiString(const RootKey: HKEY; const Key, Name: string): string; overload;
{ TODO -cHelp : Author: Peter J. Haas }
procedure RegReadMultiString(const RootKey: HKEY; const Key, Name: string; out Value: TDynStringArray); overload;
{ TODO -cHelp : Author: Peter J. Haas }
procedure RegReadMultiString(const RootKey: HKEY; const Key, Name: string; Value: TStrings); overload;
{ TODO -cHelp : Author: Peter J. Haas }
function RegReadMultiStringDef(const RootKey: HKEY; const Key, Name: string; const Def: string): string; overload;
{ TODO -cHelp : Author: Peter J. Haas }
procedure RegReadMultiStringDef(const RootKey: HKEY; const Key, Name: string; out Value: TDynStringArray; const Def: TDynStringArray); overload;
{ TODO -cHelp : Author: Peter J. Haas }
procedure RegReadMultiStringDef(const RootKey: HKEY; const Key, Name: string; Value, Def: TStrings); overload;
{ TODO -cHelp : Contributer: Peter J. Haas }
function RegReadBinary(const RootKey: HKEY; const Key, Name: string; var Value; const ValueSize: Cardinal): Cardinal;
{ TODO -cHelp : Contributer: Peter J. Haas }
function RegReadBinaryDef(const RootKey: HKEY; const Key, Name: string;
  var Value; const ValueSize: Cardinal; const Def: Byte): Cardinal;
procedure RegWriteBool(const RootKey: HKEY; const Key, Name: string; Value: Boolean);
{ TODO -cHelp : Contributer: Peter J. Haas }
procedure RegWriteInteger(const RootKey: HKEY; const Key, Name: string; Value: Integer);
{ TODO -cHelp : Author: Peter J. Haas }
procedure RegWriteCardinal(const RootKey: HKEY; const Key, Name: string; Value: Cardinal);
{ TODO -cHelp : Author: Peter J. Haas }
procedure RegWriteInt64(const RootKey: HKEY; const Key, Name: string; Value: Int64);
{ TODO -cHelp : Author: Peter J. Haas }
procedure RegWriteUInt64(const RootKey: HKEY; const Key, Name: string; Value: UInt64);
{ TODO -cHelp : Contributer: Peter J. Haas }
procedure RegWriteString(const RootKey: HKEY; const Key, Name, Value: string);
{ TODO -cHelp : Author: Peter J. Haas }
procedure RegWriteMultiString(const RootKey: HKEY; const Key, Name: string; const Value: String); overload;
{ TODO -cHelp : Author: Peter J. Haas }
procedure RegWriteMultiString(const RootKey: HKEY; const Key, Name: string; const Value: array of String); overload;
{ TODO -cHelp : Author: Peter J. Haas }
procedure RegWriteMultiString(const RootKey: HKEY; const Key, Name: string; const Value: TDynStringArray); overload;
{ TODO -cHelp : Author: Peter J. Haas }
procedure RegWriteMultiString(const RootKey: HKEY; const Key, Name: string; const Value: TStrings); overload;
{ TODO -cHelp : Contributer: Peter J. Haas }
procedure RegWriteBinary(const RootKey: HKEY; const Key, Name: string; const Value; const ValueSize: Cardinal);

function RegGetValueNames(const RootKey: HKEY; const Key: string; const List: TStrings): Boolean;
function RegGetKeyNames(const RootKey: HKEY; const Key: string; const List: TStrings): Boolean;
function RegHasSubKeys(const RootKey: HKEY; const Key: string): Boolean;

{ TODO -cHelp : RegKeyExists }
{
From: Jean-Fabien Connault [mailto:cycocrew@worldnet.fr]
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
function RegKeyExists(const RootKey: HKEY; const Key: string): Boolean;

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

//--------------------------------------------------------------------------------------------------
// obsolete functions
//--------------------------------------------------------------------------------------------------

// This function create / read invalid 8 byte REG_DWORD values.
{ TODO -cHelp : Contributer: Peter J. Haas }
{ TODO -cHelp : Obsolete, this function read invalid 8 byte REG_DWORD values.
                Use RegReadInt64 instead. }
function RegReadDWORD(const RootKey: HKEY; const Key, Name: string): Int64;
{ TODO -cHelp : Contributer: Peter J. Haas }
{ TODO -cHelp : Obsolete, this function read invalid 8 byte REG_DWORD values.
                Use RegReadInt64 instead. }
function RegReadDWORDDef(const RootKey: HKEY; const Key, Name: string; Def: Int64): Int64;
{ TODO -cHelp : Obsolete, this function create invalid 8 byte REG_DWORD keys 
                Use RegWriteInt64 instead (this create REG_QWORD values). }
procedure RegWriteDWORD(const RootKey: HKEY; const Key, Name: string; Value: Int64);

implementation
                 
uses
  SysUtils, RegStr,
  JclWin32, JclSysUtils, JclStrings, JclResources;

type
  TRegKind = REG_NONE..REG_QWORD;
  TRegKinds = set of TRegKind;

const
  CItems = 'Items';
  { TODO : Why the restriction to this registry types? }
  //CRegBinKinds = [REG_BINARY, REG_MULTI_SZ];
  CRegBinKinds = [REG_SZ..REG_QWORD];  // all types

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

function GetKeyAndPath(ExecKind: TExecKind; var Key: HKEY; out RegPath: string): Boolean;
begin
  Result := False;
  if (ExecKind in [ekServiceRun, ekServiceRunOnce]) and (Win32Platform = VER_PLATFORM_WIN32_NT) then
    Exit;
  Key := HKEY_CURRENT_USER;
  if ExecKind in [ekMachineRun, ekMachineRunOnce, ekServiceRun, ekServiceRunOnce] then
    Key := HKEY_LOCAL_MACHINE;
  case ExecKind of
    ekMachineRun, ekUserRun:
      RegPath := REGSTR_PATH_RUN;
    ekMachineRunOnce, ekUserRunOnce:
      RegPath := REGSTR_PATH_RUNONCE;
    ekServiceRun:
      RegPath := REGSTR_PATH_RUNSERVICES;
    ekServiceRunOnce:
      RegPath := REGSTR_PATH_RUNSERVICESONCE;
  end;
  Result := True;
end;

//--------------------------------------------------------------------------------------------------

// Note: this function need a string variable as parameter, in case of
// calculated strings it can crash, because the pointer point to released resp.
// modified memory
function RelativeKey(const Key: string): PChar;
begin
  Result := PChar(Key);
  if (Key <> '') and (Key[1] = '\') then
    Inc(Result);
end;

//--------------------------------------------------------------------------------------------------

{ TODO -oPJH : delete or realize }
function InternalRegOpenKey(const RootKey: HKEY; const SubKey: string;
  Desired: REGSAM; out ResultKey: HKEY): Boolean;
begin
  Result := RegOpenKeyEx(RootKey, RelativeKey(SubKey), 0, Desired, ResultKey) = ERROR_SUCCESS; 
end;

//==================================================================================================
// Registry
//==================================================================================================

function RegCreateKey(const RootKey: HKEY; const Key, Value: string): Longint;
begin
  Result := RegSetValue(RootKey, RelativeKey(Key), REG_SZ, PChar(Value), Length(Value));
end;

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

function RegDeleteKeyTree(const RootKey: HKEY; const Key: string): Boolean;
var
  RegKey: HKEY;
  I: DWORD;
  Size: DWORD;
  NumSubKeys: DWORD;
  MaxSubKeyLen: DWORD;
  KeyName: string;
begin
//  Result := InternalRegOpenKey(RootKey, Key, KEY_ALL_ACCESS, RegKey);
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

//--------------------------------------------------------------------------------------------------

function RegReadBool(const RootKey: HKEY; const Key, Name: string): Boolean;
begin
  Result := RegReadInteger(RootKey, Key, Name) <> 0;
end;

//--------------------------------------------------------------------------------------------------

function RegReadBoolDef(const RootKey: HKEY; const Key, Name: string; Def: Boolean): Boolean;
begin
  Result := Boolean(RegReadIntegerDef(RootKey, Key, Name, Ord(Def)));
end;

//--------------------------------------------------------------------------------------------------

{ TODO : find a solution for HKEY_PERFORMANCE_DATA: 
    If hKey specifies HKEY_PERFORMANCE_DATA and the lpData buffer is too small,
    RegQueryValueEx returns ERROR_MORE_DATA but lpcbData does not return the
    required buffer size. This is because the size of the performance data can
    change from one call to the next. In this case, you must increase the buffer
    size and call RegQueryValueEx again passing the updated buffer size in the
    lpcbData parameter. Repeat this until the function succeeds. You need to
    maintain a separate variable to keep track of the buffer size, because the
    value returned by lpcbData is unpredictable. }

// RegKinds:
//   Allowed registry types.
// RaiseException:
//   If RaiseException is True, the function raise a exception in case of error.
// DataPtr:
//   The data buffer. If DataPtr = Nil, InternalRegRead allocate memory (GetMem)
//   and return the pointer to this buffer. In such a case the calling function
//   need to release the memory, if InternalRegRead return True.
// DataSize:
//   The size of the data buffer. If DataSize = 0 and DataPtr = Nil the function
//   detect the needed size and allocate the buffer.
// DataPtr <> Nil and DataSize = 0 is a invalid combination and raise a internal
// exception.
function InternalRegRead(const RootKey: HKEY; const Key, Name: string;
  RegKinds: TRegKinds; RaiseException: Boolean; var DataPtr: Pointer;
  var DataSize: DWord): Boolean;
var
  RegKey: HKEY;
  RegKind: DWord;
  Ptr: Pointer;
  Size: DWord;
begin
//  Result := InternalRegOpenKey(RootKey, Key, KEY_READ, RegKey);
  Result := RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS;
  if Result then
  begin
    RegKind := REG_NONE;
    Assert(not Assigned(DataPtr) or (DataSize > 0));
    // get the needed size
    if DataSize = 0 then
    begin
      if Assigned(DataPtr) then
        raise EJclInternalError.Create('JclRegistry.InternalRegRead: Assigned DataPtr and DataSize = 0');
      Result := RegQueryValueEx(RegKey, PChar(Name), nil, nil, nil, @DataSize) = ERROR_SUCCESS;
      if not Result then
      begin
        if RaiseException then
          ValueError(Key, Name);
        Exit;
      end;
    end;
    // allocate memory, if needed
    if Assigned(DataPtr) then
      Ptr := DataPtr
    else
      GetMem(Ptr, DataSize);
    Size := DataSize;
    // get value data
    Result := RegQueryValueEx(RegKey, PChar(Name), nil, @RegKind, Ptr, @Size) = ERROR_SUCCESS;
    RegCloseKey(RegKey);
    // additional checks     
    Result := Result and (Size <= DataSize) and (RegKind in RegKinds);
    if Result then
      DataPtr := Ptr                 // return the buffer
    else
    begin                            // error
      if not Assigned(DataPtr) then  //   release allocated memory
        FreeMem(Ptr);
      if RaiseException then
        ValueError(Key, Name);
    end;
  end
  else
  begin  // RegOpenKeyEx failed
    if RaiseException then
      ReadError(Key);
  end;
end;

//--------------------------------------------------------------------------------------------------

function InternalRegReadFixedSize(const RootKey: HKEY; const Key, Name: string;
  RegKinds: TRegKinds; DataPtr: Pointer; DataSize: DWord; RaiseException: Boolean): Boolean;
var
  Size: DWord;
begin
  Size := DataSize;
  Result := InternalRegRead(RootKey, Key, Name, RegKinds, RaiseException, DataPtr, Size);
  if Result then
    Result := Size = DataSize;
  if not Result and RaiseException then
    ValueError(Key, Name);
end;

//--------------------------------------------------------------------------------------------------

function RegReadInteger(const RootKey: HKEY; const Key, Name: string): Integer;
begin
  InternalRegReadFixedSize(RootKey, Key, Name, [REG_DWORD], @Result, SizeOf(Result), True);
end;

{function RegReadInteger(const RootKey: HKEY; const Key, Name: string): Integer;
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
end;}

//--------------------------------------------------------------------------------------------------

function RegReadIntegerDef(const RootKey: HKEY; const Key, Name: string; Def: Integer): Integer;
begin
  if not InternalRegReadFixedSize(RootKey, Key, Name, [REG_DWORD], @Result, SizeOf(Result), False) then
    Result := Def;
end;

{function RegReadIntegerDef(const RootKey: HKEY; const Key, Name: string; Def: Integer): Integer;
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
end;}

//--------------------------------------------------------------------------------------------------

function RegReadCardinal(const RootKey: HKEY; const Key, Name: string): Cardinal;
begin
  InternalRegReadFixedSize(RootKey, Key, Name, [REG_DWORD], @Result, SizeOf(Result), True);
end;

//--------------------------------------------------------------------------------------------------

function RegReadCardinalDef(const RootKey: HKEY; const Key, Name: string; Def: Cardinal): Cardinal;
begin
  if not InternalRegReadFixedSize(RootKey, Key, Name, [REG_DWORD], @Result, SizeOf(Result), False) then
    Result := Def;
end;

//--------------------------------------------------------------------------------------------------

function InternalRegRead64(const RootKey: HKEY; const Key, Name: string;
  RaiseException: Boolean; Signed: Boolean; out Value: Int64): Boolean;
var
  DataPtr: Pointer;
  DataSize: DWord;
begin
  Value := 0;
  DataPtr := @Value;
  DataSize := SizeOf(Value);
  Result := InternalRegRead(RootKey, Key, Name, [REG_BINARY, REG_DWORD, REG_QWORD],
    RaiseException, DataPtr, DataSize);
  if Result then
  begin
    // extend the value to 64 bit
    if Signed then
      Value := ExtendToInt64(Value, DataSize);
  end;
  if not Result and RaiseException then
    ValueError(Key, Name);
end;

//--------------------------------------------------------------------------------------------------

function RegReadInt64(const RootKey: HKEY; const Key, Name: string): Int64;
begin
  InternalRegRead64(RootKey, Key, Name, True, True, Result);
end;

//--------------------------------------------------------------------------------------------------

function RegReadInt64Def(const RootKey: HKEY; const Key, Name: string; Def: Int64): Int64;
begin
  if not InternalRegRead64(RootKey, Key, Name, False, True, Result) then
    Result := Def;
end;

//--------------------------------------------------------------------------------------------------

function RegReadUInt64(const RootKey: HKEY; const Key, Name: string): UInt64;
begin
  InternalRegRead64(RootKey, Key, Name, True, False, Int64(Result));
end;

//--------------------------------------------------------------------------------------------------

function RegReadUInt64Def(const RootKey: HKEY; const Key, Name: string; Def: UInt64): UInt64;
begin
  if not InternalRegRead64(RootKey, Key, Name, False, False, Int64(Result)) then
    Result := Def;
end;

//--------------------------------------------------------------------------------------------------

function InternalRegReadString(const RootKey: HKEY; const Key, Name: string;
  RegKinds: TRegKinds; RaiseException: Boolean; out Value: string): Boolean;
var
  DataPtr: Pointer;
  DataSize: DWord;
begin
  Value := '';
  DataPtr := Nil;
  DataSize := 0;
  Result := InternalRegRead(RootKey, Key, Name, RegKinds, RaiseException, DataPtr, DataSize);
  if Result then
  begin
    SetString(Value, PChar(DataPtr), DataSize - 1);  // DataSize = 1 for empty strings
    FreeMem(DataPtr);
  end;
  if not Result and RaiseException then
    ValueError(Key, Name);
end;

//--------------------------------------------------------------------------------------------------

function RegReadString(const RootKey: HKEY; const Key, Name: string): string;
begin
  InternalRegReadString(RootKey, Key, Name, [REG_SZ, REG_EXPAND_SZ], True, Result);
end;

{function RegReadString(const RootKey: HKEY; const Key, Name: string): string;
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
        RegQueryValueEx(RegKey, PChar(Name), nil, @RegKind, PByte(PChar(StrVal)), @Size);
        SetLength(StrVal, StrLen(PChar(StrVal)));
        Result := StrVal;
      end;
    RegCloseKey(RegKey);
    if not (RegKind in [REG_SZ, REG_EXPAND_SZ]) then
      ValueError(Key, Name);
  end
  else
    ReadError(Key);
end;}

//--------------------------------------------------------------------------------------------------

function RegReadStringDef(const RootKey: HKEY; const Key, Name, Def: string): string;
begin
  if not InternalRegReadString(RootKey, Key, Name, [REG_SZ, REG_EXPAND_SZ], False, Result) then
    Result := Def;
end;

{function RegReadStringDef(const RootKey: HKEY; const Key, Name, Def: string): string;
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
        if RegQueryValueEx(RegKey, PChar(Name), nil, @RegKind, PByte(PChar(StrVal)), @Size) = ERROR_SUCCESS then
        begin
          SetLength(StrVal, StrLen(PChar(StrVal)));
          Result := StrVal;
        end;
      end;
    RegCloseKey(RegKey);
  end;
end;}

//--------------------------------------------------------------------------------------------------

function RegReadMultiString(const RootKey: HKEY; const Key, Name: string): string;
begin
  InternalRegReadString(RootKey, Key, Name, [REG_SZ, REG_EXPAND_SZ, REG_MULTI_SZ], True, Result);
end;

//--------------------------------------------------------------------------------------------------

procedure RegReadMultiString(const RootKey: HKEY; const Key, Name: string; out Value: TDynStringArray);
var
  S: String;
begin
  S := RegReadMultiString(RootKey, Key, Name);
  MultiStringToStrings(Value, S);
end;

//--------------------------------------------------------------------------------------------------

procedure RegReadMultiString(const RootKey: HKEY; const Key, Name: string; Value: TStrings);
var
  S: String;
begin
  S := RegReadMultiString(RootKey, Key, Name);
  MultiStringToStrings(Value, S);
end;

//--------------------------------------------------------------------------------------------------

function InternalRegReadMultiStringDef(const RootKey: HKEY; const Key, Name: string;
  out Value: string): Boolean;
begin
  Result := InternalRegReadString(RootKey, Key, Name, [REG_SZ, REG_EXPAND_SZ, REG_MULTI_SZ], False, Value);
end;

//--------------------------------------------------------------------------------------------------

function RegReadMultiStringDef(const RootKey: HKEY; const Key, Name: string; const Def: string): string;
begin
  if not InternalRegReadMultiStringDef(RootKey, Key, Name, Result) then
    Result := Def;
end;

//--------------------------------------------------------------------------------------------------

procedure RegReadMultiStringDef(const RootKey: HKEY; const Key, Name: string; out Value: TDynStringArray; const Def: TDynStringArray);
var
  S: string;
begin
  if InternalRegReadMultiStringDef(RootKey, Key, Name, S) then
    MultiStringToStrings(Value, S)
  else
    Value := Copy(Def);
end;

//--------------------------------------------------------------------------------------------------

procedure RegReadMultiStringDef(const RootKey: HKEY; const Key, Name: string; Value, Def: TStrings);
var
  S: string;
begin
  if InternalRegReadMultiStringDef(RootKey, Key, Name, S) then
    MultiStringToStrings(Value, S)
  else
    Value.Assign(Def);
end;

//--------------------------------------------------------------------------------------------------

function InternalRegReadBinary(const RootKey: HKEY; const Key, Name: string;
  RaiseException: Boolean; out Value; var ValueSize: Cardinal): Boolean;
var
  DataPtr: Pointer;
begin
  DataPtr := @Value;
  Result := InternalRegRead(RootKey, Key, Name, CRegBinKinds, // [REG_BINARY, REG_MULTI_SZ]
    RaiseException, DataPtr, ValueSize);
  if not Result and RaiseException then
    ValueError(Key, Name);
end;

//--------------------------------------------------------------------------------------------------

function RegReadBinary(const RootKey: HKEY; const Key, Name: string; var Value;
  const ValueSize: Cardinal): Cardinal;
begin
  Result := ValueSize;
  if ValueSize > 0 then
    InternalRegReadBinary(RootKey, Key, Name, True, Value, Result);
end;

{function RegReadBinary(const RootKey: HKEY; const Key, Name: string; var Value; const ValueSize: Cardinal): Cardinal;
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
      if RegKind in CRegBinKinds then
      begin
        if Size > ValueSize then
          Size := ValueSize;
        RegQueryValueEx(RegKey, PChar(Name), nil, @RegKind, @Value, @Size);
        Result := Size;
      end;
    RegCloseKey(RegKey);
    if not (RegKind in CRegBinKinds) then
      ValueError(Key, Name);
  end
  else
    ReadError(Key);
end;}

//--------------------------------------------------------------------------------------------------

function RegReadBinaryDef(const RootKey: HKEY; const Key, Name: string;
  var Value; const ValueSize: Cardinal; const Def: Byte): Cardinal;
begin
  Result := ValueSize;
  if ValueSize > 0 then
  begin
    if not InternalRegReadBinary(RootKey, Key, Name, False, Value, Result) then
      Result := 0;
    FillRemainBytes(Value, ValueSize, Result, Def);
  end;
end;

{function RegReadBinaryDef(const RootKey: HKEY; const Key, Name: string;
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
      if RegKind in CRegBinKinds then
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
end;}

//--------------------------------------------------------------------------------------------------

function RegReadDWORD(const RootKey: HKEY; const Key, Name: string): Int64;
begin
  InternalRegReadFixedSize(RootKey, Key, Name, [REG_DWORD], @Result, SizeOf(Result), True);
end;

{function RegReadDWORD(const RootKey: HKEY; const Key, Name: string): Int64;
var
  RegKey: HKEY;
  Size: DWORD;
  IntVal: Int64;
  RegKind: DWORD;
  Ret: Longint;
begin
  Result := 0;
  if RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    RegKind := 0;
    Size := SizeOf(Int64);
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
end;}

//--------------------------------------------------------------------------------------------------

function RegReadDWORDDef(const RootKey: HKEY; const Key, Name: string; Def: Int64): Int64;
begin
  if not InternalRegReadFixedSize(RootKey, Key, Name, [REG_DWORD], @Result, SizeOf(Result), False) then
    Result := Def;
end;

{function RegReadDWORDDef(const RootKey: HKEY; const Key, Name: string; Def: Int64): Int64;
var
  RegKey: HKEY;
  Size: DWORD;
  IntVal: Int64;
  RegKind: DWORD;
begin
  Result := Def;
  if RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    RegKind := 0;
    Size := SizeOf(Int64);
    if RegQueryValueEx(RegKey, PChar(Name), nil, @RegKind, @IntVal, @Size) = ERROR_SUCCESS then
      if RegKind = REG_DWORD then
        Result := IntVal;
    RegCloseKey(RegKey);
  end;
end;}

//--------------------------------------------------------------------------------------------------

procedure RegWriteBool(const RootKey: HKEY; const Key, Name: string; Value: Boolean);
begin
  RegWriteInteger(RootKey, Key, Name, Ord(Value));
end;

//--------------------------------------------------------------------------------------------------

procedure InternalRegWrite(const RootKey: HKEY; const Key, Name: string;
  RegKind: TRegKind; const Value; ValueSize: Cardinal);
var
  RegKey: HKEY;
  Ret: Longint;
begin
  if RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_SET_VALUE, RegKey) = ERROR_SUCCESS then
  begin
    Ret := RegSetValueEx(RegKey, PChar(Name), 0, RegKind, @Value, ValueSize);
    RegCloseKey(RegKey);
    if Ret <> ERROR_SUCCESS then
      WriteError(Key);
  end
  else
    WriteError(Key);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteInteger(const RootKey: HKEY; const Key, Name: string; Value: Integer);
begin
  InternalRegWrite(RootKey, Key, Name, REG_DWORD, Value, SizeOf(Value));
end;

{procedure RegWriteInteger(const RootKey: HKEY; const Key, Name: string; Value: Integer);
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
end;}

//--------------------------------------------------------------------------------------------------

procedure RegWriteCardinal(const RootKey: HKEY; const Key, Name: string; Value: Cardinal);
begin
  InternalRegWrite(RootKey, Key, Name, REG_DWORD, Value, SizeOf(Value));
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteInt64(const RootKey: HKEY; const Key, Name: string; Value: Int64);
begin
  InternalRegWrite(RootKey, Key, Name, REG_QWORD, Value, SizeOf(Value));
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteUInt64(const RootKey: HKEY; const Key, Name: string; Value: UInt64);
begin
  InternalRegWrite(RootKey, Key, Name, REG_QWORD, Value, SizeOf(Value));
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteString(const RootKey: HKEY; const Key, Name, Value: string);
begin
  InternalRegWrite(RootKey, Key, Name, REG_SZ, PChar(Value)^, Length(Value) + 1);
end;

{procedure RegWriteString(const RootKey: HKEY; const Key, Name, Value: string);
var
  RegKey: HKEY;
  Ret: Longint;
begin
  if RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_SET_VALUE, RegKey) = ERROR_SUCCESS then
  begin
    Ret := RegSetValueEx(RegKey, PChar(Name), 0, REG_SZ, PByte(PChar(Value)), Length(Value)+1);
    RegCloseKey(RegKey);
    if Ret <> ERROR_SUCCESS then
      WriteError(Key);
  end
  else
    WriteError(Key);
end;}

//--------------------------------------------------------------------------------------------------

procedure RegWriteMultiString(const RootKey: HKEY; const Key, Name: string; const Value: String);
begin
  InternalRegWrite(RootKey, Key, Name, REG_MULTI_SZ, PChar(Value)^, Length(Value) + 1);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteMultiString(const RootKey: HKEY; const Key, Name: string; const Value: array of String);
begin
  RegWriteMultiString(RootKey, Key, Name, StringsToMultiString(Value));
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteMultiString(const RootKey: HKEY; const Key, Name: string; const Value: TDynStringArray); overload;
begin
  RegWriteMultiString(RootKey, Key, Name, StringsToMultiString(Value));
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteMultiString(const RootKey: HKEY; const Key, Name: string; const Value: TStrings);
begin
  RegWriteMultiString(RootKey, Key, Name, StringsToMultiString(Value));
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteBinary(const RootKey: HKEY; const Key, Name: string; const Value; const ValueSize: Cardinal);
begin
  InternalRegWrite(RootKey, Key, Name, REG_BINARY, Value, ValueSize);
end;

{procedure RegWriteBinary(const RootKey: HKEY; const Key, Name: string; var Value; const ValueSize: Cardinal);
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
end;}

//--------------------------------------------------------------------------------------------------

procedure RegWriteDWORD(const RootKey: HKEY; const Key, Name: string; Value: Int64);
begin
  InternalRegWrite(RootKey, Key, Name, REG_DWORD, Value, SizeOf(Value));
end;

{procedure RegWriteDWORD(const RootKey: HKEY; const Key, Name: string; Value: Int64);
var
  RegKey: HKEY;
  Ret: Longint;
begin
  if RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_SET_VALUE, RegKey) = ERROR_SUCCESS then
  begin
    Ret := RegSetValueEx(RegKey, PChar(Name), 0, REG_DWORD, @Value, SizeOf(Int64));
    RegCloseKey(RegKey);
    if Ret <> ERROR_SUCCESS then
      WriteError(Key);
  end
  else
    WriteError(Key);
end;}

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

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

function RegKeyExists(const RootKey: HKEY; const Key: string): Boolean;
var
  RegKey: HKEY;
begin
  Result := (RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS);
  if Result then RegCloseKey(RegKey);
end;

//--------------------------------------------------------------------------------------------------

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
    RegWriteInteger(RootKey, Subkey, CItems, Items.Count);
    for I := 1 to Items.Count do
      RegWriteString(RootKey, Subkey, IntToStr(I), Items[I-1]);
    Result := True;
  end;
end;

//--------------------------------------------------------------------------------------------------

function RegLoadList(const RootKey: HKEY; const Key: string;
  const ListName: string; const SaveTo: TStrings): Boolean;
var
  I, N: Integer;
  Subkey: string;
begin
  SaveTo.Clear;
  Subkey := Key + '\' + ListName;
  N := RegReadInteger(RootKey, Subkey, CItems);
  for I := 1 to N do
    SaveTo.Add(RegReadString(RootKey, Subkey, IntToStr(I)));
  Result := N > 0;
end;

//--------------------------------------------------------------------------------------------------

function RegDelList(const RootKey: HKEY; const Key: string; const ListName: string): Boolean;
var
  I, N: Integer;
  Subkey: string;
begin
  Result := False;
  Subkey := Key + '\' + ListName;
  N := RegReadIntegerDef(RootKey, Subkey, CItems, -1);
  if (N > 0) and RegDeleteEntry(RootKey, Subkey, CItems) then
    for I := 1 to N do
    begin
      Result := RegDeleteEntry(RootKey, Subkey, IntToStr(I));
      if not Result then
        Break;
    end;
end;

// History:

// $Log$
// Revision 1.7  2004/04/06 05:56:10  rrossmair
// fixed RegReadUInt64 & RegReadUInt64Def
//
// Revision 1.6  2004/04/06 04:45:57  peterjhaas
// Unite the single read functions and the single write functions, add Cardinal, Int64, UInt64 and Multistring support
//

end.
