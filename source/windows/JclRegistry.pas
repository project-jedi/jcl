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
{   Peter J. Haas (peterjhaas)                                                                     }
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
  JclBase;

type
  DelphiHKEY = LongWord;
{$HPPEMIT '// BCB users must typecast the HKEY values to DelphiHKEY.'}  

//--------------------------------------------------------------------------------------------------
// Registry
//--------------------------------------------------------------------------------------------------

function RegCreateKey(const RootKey: DelphiHKEY; const Key, Value: string): Longint;
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
function RegReadInt64(const RootKey: DelphiHKEY; const Key, Name: string): Int64;
function RegReadInt64Def(const RootKey: DelphiHKEY; const Key, Name: string; Def: Int64): Int64;
function RegReadUInt64(const RootKey: DelphiHKEY; const Key, Name: string): UInt64;
function RegReadUInt64Def(const RootKey: DelphiHKEY; const Key, Name: string; Def: UInt64): UInt64;

function RegReadString(const RootKey: DelphiHKEY; const Key, Name: string): string;
function RegReadStringDef(const RootKey: DelphiHKEY; const Key, Name, Def: string): string;
function RegReadAnsiString(const RootKey: DelphiHKEY; const Key, Name: AnsiString): AnsiString;
function RegReadAnsiStringDef(const RootKey: DelphiHKEY; const Key, Name, Def: AnsiString): AnsiString;
function RegReadWideString(const RootKey: DelphiHKEY; const Key, Name: WideString): WideString;
function RegReadWideStringDef(const RootKey: DelphiHKEY; const Key, Name, Def: WideString): WideString;

function RegReadMultiString(const RootKey: DelphiHKEY; const Key, Name: string): string; overload;
procedure RegReadMultiString(const RootKey: DelphiHKEY; const Key, Name: string; out Value: TDynStringArray); overload;
procedure RegReadMultiString(const RootKey: DelphiHKEY; const Key, Name: string; Value: TStrings); overload;
function RegReadMultiStringDef(const RootKey: DelphiHKEY; const Key, Name: string; const Def: string): string; overload;
procedure RegReadMultiStringDef(const RootKey: DelphiHKEY; const Key, Name: string; out Value: TDynStringArray; const Def: TDynStringArray); overload;
procedure RegReadMultiStringDef(const RootKey: DelphiHKEY; const Key, Name: string; Value, Def: TStrings); overload;
function RegReadMultiAnsiString(const RootKey: DelphiHKEY; const Key, Name: AnsiString): AnsiString;
function RegReadMultiAnsiStringDef(const RootKey: DelphiHKEY; const Key, Name: AnsiString; const Def: AnsiString): AnsiString;
function RegReadMultiWideString(const RootKey: DelphiHKEY; const Key, Name: WideString): WideString;
function RegReadMultiWideStringDef(const RootKey: DelphiHKEY; const Key, Name: WideString; const Def: WideString): WideString;

function RegReadBinary(const RootKey: DelphiHKEY; const Key, Name: string; var Value; const ValueSize: Cardinal): Cardinal; overload;
procedure RegReadBinary(const RootKey: DelphiHKEY; const Key, Name: string; out Value: TDynByteArray); overload;
function RegReadBinaryDef(const RootKey: DelphiHKEY; const Key, Name: string;
  var Value; const ValueSize: Cardinal; const Def: Byte): Cardinal; overload;
procedure RegReadBinaryDef(const RootKey: DelphiHKEY; const Key, Name: string;
  out Value: TDynByteArray; const Def: TDynByteArray); overload;

function RegReadBinaryAsAnsiString(const RootKey: DelphiHKEY; const Key, Name: AnsiString): AnsiString;
function RegReadBinaryAsAnsiStringDef(const RootKey: DelphiHKEY; const Key, Name: string;
  const Def: AnsiString): AnsiString;
function RegReadBinaryAsWideString(const RootKey: DelphiHKEY; const Key, Name: WideString): WideString;
function RegReadBinaryAsWideStringDef(const RootKey: DelphiHKEY; const Key, Name: string;
  const Def: WideString): WideString;

procedure RegWriteBool(const RootKey: DelphiHKEY; const Key, Name: string; Value: Boolean);
procedure RegWriteInteger(const RootKey: DelphiHKEY; const Key, Name: string; Value: Integer);
procedure RegWriteCardinal(const RootKey: DelphiHKEY; const Key, Name: string; Value: Cardinal);
procedure RegWriteInt64(const RootKey: DelphiHKEY; const Key, Name: string; Value: Int64);
procedure RegWriteUInt64(const RootKey: DelphiHKEY; const Key, Name: string; Value: UInt64);

procedure RegWriteString(const RootKey: DelphiHKEY; const Key, Name, Value: string);
procedure RegWriteAnsiString(const RootKey: DelphiHKEY; const Key, Name, Value: AnsiString);
procedure RegWriteWideString(const RootKey: DelphiHKEY; const Key, Name, Value: WideString);

procedure RegWriteMultiString(const RootKey: DelphiHKEY; const Key, Name: string; const Value: string); overload;
procedure RegWriteMultiString(const RootKey: DelphiHKEY; const Key, Name: string; const Value: array of string); overload;
procedure RegWriteMultiString(const RootKey: DelphiHKEY; const Key, Name: string; const Value: TDynStringArray); overload;
procedure RegWriteMultiString(const RootKey: DelphiHKEY; const Key, Name: string; const Value: TStrings); overload;

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

type
  TExecKind = (ekMachineRun, ekMachineRunOnce, ekUserRun, ekUserRunOnce,
    ekServiceRun, ekServiceRunOnce);

  EJclRegistryError = class(EJclError);

function UnregisterAutoExec(ExecKind: TExecKind; const Name: string): Boolean;
function RegisterAutoExec(ExecKind: TExecKind; const Name, Cmdline: string): Boolean;

function RegSaveList(const RootKey: DelphiHKEY; const Key: string; const ListName: string;
  const Items: TStrings): Boolean;
function RegLoadList(const RootKey: DelphiHKEY; const Key: string; const ListName: string;
  const SaveTo: TStrings): Boolean;
function RegDelList(const RootKey: DelphiHKEY; const Key: string; const ListName: string): Boolean;

//--------------------------------------------------------------------------------------------------
// obsolete functions
//--------------------------------------------------------------------------------------------------

{$IFNDEF DROP_OBSOLETE_CODE}

// This obsolete functions create resp. read invalid 8 byte REG_DWORD values.
// Do not use.

function RegReadDWORD(const RootKey: DelphiHKEY; const Key, Name: string): Int64;
  {$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}
function RegReadDWORDDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Int64): Int64;
  {$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}

procedure RegWriteDWORD(const RootKey: DelphiHKEY; const Key, Name: string; Value: Int64);
  {$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}

{$ENDIF ~DROP_OBSOLETE_CODE}

implementation

uses
  SysUtils,
  {$IFDEF FPC}
  JwaRegStr,
  {$ELSE}
  RegStr,
  {$ENDIF FPC}
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
function RelativeKeyA(const Key: AnsiString): PAnsiChar;
begin
  Result := PAnsiChar(Key);
  if (Key <> '') and (Key[1] = '\') then
    Inc(Result);
end;

//--------------------------------------------------------------------------------------------------

// Note: this function need a string variable as parameter, in case of
// calculated strings it can crash, because the pointer point to released resp.
// modified memory
function RelativeKeyW(const Key: WideString): PWideChar;
begin
  Result := PWideChar(Key);
  if (Key <> '') and (Key[1] = '\') then
    Inc(Result);
end;

//==================================================================================================
// Registry
//==================================================================================================

function RegCreateKey(const RootKey: DelphiHKEY; const Key, Value: string): Longint;
begin
  Result := RegSetValue(RootKey, RelativeKeyA(Key), REG_SZ, PChar(Value), Length(Value));
end;

//--------------------------------------------------------------------------------------------------

function RegDeleteEntry(const RootKey: DelphiHKEY; const Key, Name: string): Boolean;
var
  RegKey: HKEY;
begin
  Result := False;
  if RegOpenKeyEx(RootKey, RelativeKeyA(Key), 0, KEY_SET_VALUE, RegKey) = ERROR_SUCCESS then
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
  Result := RegOpenKeyEx(RootKey, RelativeKeyA(Key), 0, KEY_ALL_ACCESS, RegKey) = ERROR_SUCCESS;
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
      Result := Windows.RegDeleteKey(RootKey, RelativeKeyA(Key)) = ERROR_SUCCESS;
    end
    else
      WriteError(Key);
end;

//--------------------------------------------------------------------------------------------------

function RegReadBool(const RootKey: DelphiHKEY; const Key, Name: string): Boolean;
begin
  Result := RegReadInteger(RootKey, Key, Name) <> 0;
end;

//--------------------------------------------------------------------------------------------------

function RegReadBoolDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Boolean): Boolean;
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
//   The data buffer. If DataPtr = nil, InternalRegRead allocate memory (GetMem)
//   and return the pointer to this buffer. In such a case the calling function
//   need to release the memory, if InternalRegRead return True.
// DataSize:
//   The size of the data buffer. If DataSize = 0 and DataPtr = nil the function
//   detect the needed size and allocate the buffer.
// DataPtr <> nil and DataSize = 0 is a invalid combination and raise a internal
// exception.
function InternalRegReadA(const RootKey: DelphiHKEY; const Key, Name: AnsiString;
  RegKinds: TRegKinds; RaiseException: Boolean; var DataPtr: Pointer;
  var DataSize: DWord): Boolean;
var
  RegKey: HKEY;
  RegKind: DWord;
  Ptr: Pointer;
  Size: DWord;
begin
  Result := RegOpenKeyExA(RootKey, RelativeKeyA(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS;
  if Result then
  begin
    RegKind := REG_NONE;
    Assert(not Assigned(DataPtr) or (DataSize > 0));
    // get the needed size
    if DataSize = 0 then
    begin
      if Assigned(DataPtr) then
        raise EJclInternalError.Create('JclRegistry.InternalRegRead: Assigned DataPtr and DataSize = 0');
      Result := RegQueryValueExA(RegKey, PAnsiChar(Name), nil, nil, nil, @DataSize) = ERROR_SUCCESS;
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
    Result := RegQueryValueExA(RegKey, PAnsiChar(Name), nil, @RegKind, Ptr, @Size) = ERROR_SUCCESS;
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

function InternalRegReadW(const RootKey: DelphiHKEY; const Key, Name: WideString;
  RegKinds: TRegKinds; RaiseException: Boolean; var DataPtr: Pointer;
  var DataSize: DWord): Boolean;
var
  RegKey: HKEY;
  RegKind: DWord;
  Ptr: Pointer;
  Size: DWord;
begin
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
  begin
    Result := InternalRegReadA(RootKey, Key, Name, RegKinds, RaiseException,
      DataPtr, DataSize);
  end
  else
  begin
    Result := RegOpenKeyExW(RootKey, RelativeKeyW(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS;
    if Result then
    begin
      RegKind := REG_NONE;
      Assert(not Assigned(DataPtr) or (DataSize > 0));
      // get the needed size
      if DataSize = 0 then
      begin
        if Assigned(DataPtr) then
          raise EJclInternalError.Create('JclRegistry.InternalRegRead: Assigned DataPtr and DataSize = 0');
        Result := RegQueryValueExW(RegKey, PWideChar(Name), nil, nil, nil, @DataSize) = ERROR_SUCCESS;
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
      Result := RegQueryValueExW(RegKey, PWideChar(Name), nil, @RegKind, Ptr, @Size) = ERROR_SUCCESS;
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
    else  // RegOpenKeyExW failed
    begin
      if RaiseException then
        ReadError(Key);
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function RegGetDataSize(const RootKey: DelphiHKEY; const Key, Name: string;
  out DataSize: Cardinal): Boolean;
var
  RegKey: HKEY;
begin
  DataSize := 0;
  Result := RegOpenKeyExA(RootKey, RelativeKeyA(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS;
  if Result then
  begin
    Result := RegQueryValueExA(RegKey, PAnsiChar(Name), nil, nil, nil, @DataSize) = ERROR_SUCCESS;
    RegCloseKey(RegKey);
  end;
end;

//--------------------------------------------------------------------------------------------------

function RegGetDataType(const RootKey: DelphiHKEY; const Key, Name: string;
  out DataType: Cardinal): Boolean;
var
  RegKey: HKEY;
begin
  DataType := REG_NONE;
  Result := RegOpenKeyExA(RootKey, RelativeKeyA(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS;
  if Result then
  begin
    Result := RegQueryValueExA(RegKey, PAnsiChar(Name), nil, @DataType, nil, nil) = ERROR_SUCCESS;
    RegCloseKey(RegKey);
  end;
end;

//--------------------------------------------------------------------------------------------------

function InternalRegReadFixedSize(const RootKey: DelphiHKEY; const Key, Name: string;
  RegKinds: TRegKinds; DataPtr: Pointer; DataSize: DWord; RaiseException: Boolean): Boolean;
var
  Size: DWord;
begin
  Size := DataSize;
  Result := InternalRegReadA(RootKey, Key, Name, RegKinds, RaiseException, DataPtr, Size);
  if Result then
    Result := Size = DataSize;
  if not Result and RaiseException then
    ValueError(Key, Name);
end;

//--------------------------------------------------------------------------------------------------

function RegReadInteger(const RootKey: DelphiHKEY; const Key, Name: string): Integer;
begin
  InternalRegReadFixedSize(RootKey, Key, Name, [REG_DWORD], @Result, SizeOf(Result), True);
end;

//--------------------------------------------------------------------------------------------------

function RegReadIntegerDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Integer): Integer;
begin
  if not InternalRegReadFixedSize(RootKey, Key, Name, [REG_DWORD], @Result, SizeOf(Result), False) then
    Result := Def;
end;

//--------------------------------------------------------------------------------------------------

function RegReadCardinal(const RootKey: DelphiHKEY; const Key, Name: string): Cardinal;
begin
  InternalRegReadFixedSize(RootKey, Key, Name, [REG_DWORD], @Result, SizeOf(Result), True);
end;

//--------------------------------------------------------------------------------------------------

function RegReadCardinalDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Cardinal): Cardinal;
begin
  if not InternalRegReadFixedSize(RootKey, Key, Name, [REG_DWORD], @Result, SizeOf(Result), False) then
    Result := Def;
end;

//--------------------------------------------------------------------------------------------------

function InternalRegRead64(const RootKey: DelphiHKEY; const Key, Name: string;
  RaiseException: Boolean; Signed: Boolean; out Value: Int64): Boolean;
var
  DataPtr: Pointer;
  DataSize: DWord;
begin
  Value := 0;
  DataPtr := @Value;
  DataSize := SizeOf(Value);
  Result := InternalRegReadA(RootKey, Key, Name, [REG_BINARY, REG_DWORD, REG_QWORD],
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

function RegReadInt64(const RootKey: DelphiHKEY; const Key, Name: string): Int64;
begin
  InternalRegRead64(RootKey, Key, Name, True, True, Result);
end;

//--------------------------------------------------------------------------------------------------

function RegReadInt64Def(const RootKey: DelphiHKEY; const Key, Name: string; Def: Int64): Int64;
begin
  if not InternalRegRead64(RootKey, Key, Name, False, True, Result) then
    Result := Def;
end;

//--------------------------------------------------------------------------------------------------

function RegReadUInt64(const RootKey: DelphiHKEY; const Key, Name: string): UInt64;
begin
  InternalRegRead64(RootKey, Key, Name, True, False, Int64(Result));
end;

//--------------------------------------------------------------------------------------------------

function RegReadUInt64Def(const RootKey: DelphiHKEY; const Key, Name: string; Def: UInt64): UInt64;
begin
  if not InternalRegRead64(RootKey, Key, Name, False, False, Int64(Result)) then
    Result := Def;
end;

//--------------------------------------------------------------------------------------------------

function InternalRegReadAnsiString(const RootKey: DelphiHKEY; const Key, Name: AnsiString;
  RegKinds: TRegKinds; RaiseException: Boolean; out Value: AnsiString): Boolean;
var
  DataPtr: Pointer;
  DataSize: DWord;
begin
  Value := '';
  DataPtr := nil;
  DataSize := 0;
  Result := InternalRegReadA(RootKey, Key, Name, RegKinds, RaiseException, DataPtr, DataSize);
  if Result then
  begin
    SetString(Value, PAnsiChar(DataPtr), DataSize div SizeOf(AnsiChar) - 1);  // DataSize = 1 for empty strings
    FreeMem(DataPtr);
  end;
  if not Result and RaiseException then
    ValueError(Key, Name);
end;

//--------------------------------------------------------------------------------------------------

function InternalRegReadWideString(const RootKey: DelphiHKEY; const Key, Name: WideString;
  RegKinds: TRegKinds; RaiseException: Boolean; out Value: WideString): Boolean;
var
  DataPtr: Pointer;
  DataSize: DWord;
begin
  Value := '';
  DataPtr := nil;
  DataSize := 0;
  Result := InternalRegReadW(RootKey, Key, Name, RegKinds, RaiseException, DataPtr, DataSize);
  if Result then
  begin
    SetString(Value, PWideChar(DataPtr), DataSize div SizeOf(WideChar) - 1);  // DataSize = 1 for empty strings
    FreeMem(DataPtr);
  end;
  if not Result and RaiseException then
    ValueError(Key, Name);
end;

//--------------------------------------------------------------------------------------------------

function RegReadAnsiString(const RootKey: DelphiHKEY; const Key, Name: AnsiString): AnsiString;
begin
  InternalRegReadAnsiString(RootKey, Key, Name, [REG_SZ, REG_EXPAND_SZ], True, Result);
end;

//--------------------------------------------------------------------------------------------------

function RegReadWideString(const RootKey: DelphiHKEY; const Key, Name: WideString): WideString;
begin
  InternalRegReadWideString(RootKey, Key, Name, [REG_SZ, REG_EXPAND_SZ], True, Result);
end;

//--------------------------------------------------------------------------------------------------

function RegReadString(const RootKey: DelphiHKEY; const Key, Name: string): string;
begin
  Result := RegReadAnsiString(RootKey, Key, Name);
end;

//--------------------------------------------------------------------------------------------------

function RegReadAnsiStringDef(const RootKey: DelphiHKEY; const Key, Name, Def: AnsiString): AnsiString;
begin
  if not InternalRegReadAnsiString(RootKey, Key, Name, [REG_SZ, REG_EXPAND_SZ], False, Result) then
    Result := Def;
end;

//--------------------------------------------------------------------------------------------------

function RegReadWideStringDef(const RootKey: DelphiHKEY; const Key, Name, Def: WideString): WideString;
begin
  if not InternalRegReadWideString(RootKey, Key, Name, [REG_SZ, REG_EXPAND_SZ], False, Result) then
    Result := Def;
end;

//--------------------------------------------------------------------------------------------------

function RegReadStringDef(const RootKey: DelphiHKEY; const Key, Name, Def: string): string;
begin
  Result := RegReadAnsiStringDef(RootKey, Key, Name, Def);
end;

//--------------------------------------------------------------------------------------------------

function RegReadMultiAnsiString(const RootKey: DelphiHKEY; const Key, Name: AnsiString): AnsiString;
begin
  InternalRegReadAnsiString(RootKey, Key, Name, [REG_SZ, REG_EXPAND_SZ, REG_MULTI_SZ], True, Result);
end;

//--------------------------------------------------------------------------------------------------

function RegReadMultiWideString(const RootKey: DelphiHKEY; const Key, Name: WideString): WideString;
begin
  InternalRegReadWideString(RootKey, Key, Name, [REG_SZ, REG_EXPAND_SZ, REG_MULTI_SZ], True, Result);
end;

//--------------------------------------------------------------------------------------------------

function RegReadMultiString(const RootKey: DelphiHKEY; const Key, Name: string): string;
begin
  Result := RegReadMultiAnsiString(RootKey, Key, Name);
end;

//--------------------------------------------------------------------------------------------------

procedure RegReadMultiString(const RootKey: DelphiHKEY; const Key, Name: string; out Value: TDynStringArray);
var
  S: string;
begin
  S := RegReadMultiString(RootKey, Key, Name);
  MultiStringToStrings(Value, S);
end;

//--------------------------------------------------------------------------------------------------

procedure RegReadMultiString(const RootKey: DelphiHKEY; const Key, Name: string; Value: TStrings);
var
  S: string;
begin
  S := RegReadMultiString(RootKey, Key, Name);
  MultiStringToStrings(Value, S);
end;

//--------------------------------------------------------------------------------------------------

function InternalRegReadMultiAnsiStringDef(const RootKey: DelphiHKEY; const Key, Name: AnsiString;
  out Value: AnsiString): Boolean;
begin
  Result := InternalRegReadAnsiString(RootKey, Key, Name, [REG_SZ, REG_EXPAND_SZ, REG_MULTI_SZ], False, Value);
end;

//--------------------------------------------------------------------------------------------------

function InternalRegReadMultiWideStringDef(const RootKey: DelphiHKEY; const Key, Name: WideString;
  out Value: WideString): Boolean;
begin
  Result := InternalRegReadWideString(RootKey, Key, Name, [REG_SZ, REG_EXPAND_SZ, REG_MULTI_SZ], False, Value);
end;

//--------------------------------------------------------------------------------------------------

function RegReadMultiAnsiStringDef(const RootKey: DelphiHKEY; const Key, Name: AnsiString; const Def: AnsiString): AnsiString;
begin
  if not InternalRegReadMultiAnsiStringDef(RootKey, Key, Name, Result) then
    Result := Def;
end;

//--------------------------------------------------------------------------------------------------

function RegReadMultiWideStringDef(const RootKey: DelphiHKEY; const Key, Name: WideString; const Def: WideString): WideString;
begin
  if not InternalRegReadMultiWideStringDef(RootKey, Key, Name, Result) then
    Result := Def;
end;

//--------------------------------------------------------------------------------------------------

function RegReadMultiStringDef(const RootKey: DelphiHKEY; const Key, Name: string; const Def: string): string;
begin
  Result := RegReadMultiAnsiStringDef(RootKey, Key, Name, Def);
end;

//--------------------------------------------------------------------------------------------------

procedure RegReadMultiStringDef(const RootKey: DelphiHKEY; const Key, Name: string; out Value: TDynStringArray; const Def: TDynStringArray);
var
  S: string;
begin
  if InternalRegReadMultiAnsiStringDef(RootKey, Key, Name, S) then
    MultiStringToStrings(Value, S)
  else
    Value := Copy(Def);
end;

//--------------------------------------------------------------------------------------------------

procedure RegReadMultiStringDef(const RootKey: DelphiHKEY; const Key, Name: string; Value, Def: TStrings);
var
  S: string;
begin
  if InternalRegReadMultiAnsiStringDef(RootKey, Key, Name, S) then
    MultiStringToStrings(Value, S)
  else
    Value.Assign(Def);
end;

//--------------------------------------------------------------------------------------------------

function InternalRegReadBinaryFixedSize(const RootKey: DelphiHKEY; const Key,
  Name: string; RaiseException: Boolean; out Value; var ValueSize: Cardinal): Boolean;
var
  DataPtr: Pointer;
begin
  DataPtr := @Value;
  if (ValueSize = 0) or (not Assigned(DataPtr)) then
    Result := RegGetDataSize(RootKey, Key, Name, ValueSize)
  else
  begin  // ValueSize > 0
    Result := InternalRegReadA(RootKey, Key, Name, CRegBinKinds, RaiseException,
      DataPtr, ValueSize);
  end;
  if not Result and RaiseException then
    ValueError(Key, Name);
end;

//--------------------------------------------------------------------------------------------------

function InternalRegReadBinaryA(const RootKey: DelphiHKEY; const Key, Name: AnsiString;
  RaiseException: Boolean; out DataPtr: Pointer; out DataSize: DWord): Boolean;
begin
  DataPtr := nil;
  DataSize := 0;
  Result := InternalRegReadA(RootKey, Key, Name, CRegBinKinds, RaiseException,
    DataPtr, DataSize);
  if not Result and RaiseException then
    ValueError(Key, Name);
end;

//--------------------------------------------------------------------------------------------------

function InternalRegReadBinaryW(const RootKey: DelphiHKEY; const Key, Name: WideString;
  RaiseException: Boolean; out DataPtr: Pointer; out DataSize: DWord): Boolean;
begin
  DataPtr := nil;
  DataSize := 0;
  Result := InternalRegReadW(RootKey, Key, Name, CRegBinKinds, RaiseException,
    DataPtr, DataSize);
  if not Result and RaiseException then
    ValueError(Key, Name);
end;

//--------------------------------------------------------------------------------------------------

function RegReadBinary(const RootKey: DelphiHKEY; const Key, Name: string; var Value;
  const ValueSize: Cardinal): Cardinal;
begin
  Result := ValueSize;
  InternalRegReadBinaryFixedSize(RootKey, Key, Name, True, Value, Result);
end;

//--------------------------------------------------------------------------------------------------

function RegReadBinaryDef(const RootKey: DelphiHKEY; const Key, Name: string;
  var Value; const ValueSize: Cardinal; const Def: Byte): Cardinal;
begin
  Result := ValueSize;
  if not InternalRegReadBinaryFixedSize(RootKey, Key, Name, False, Value, Result) then
    Result := 0;
  FillRemainBytes(Value, ValueSize, Result, Def);
end;

//--------------------------------------------------------------------------------------------------

function InternalRegReadBinaryAsArray(const RootKey: DelphiHKEY; const Key, Name: AnsiString;
  RaiseException: Boolean; out Value: TDynByteArray): Boolean;
var
  DataPtr: Pointer;
  DataSize: DWord;
begin
  Result := InternalRegReadBinaryA(RootKey, Key, Name, RaiseException, DataPtr, DataSize);
  if Result then
  begin
    try
      SetLength(Value, DataSize);
      if DataSize > 0 then
        Move(DataPtr^, Value[Low(Value)], DataSize);
    finally
      if DataSize > 0 then
        FreeMem(DataPtr);
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure RegReadBinary(const RootKey: DelphiHKEY; const Key, Name: string;
  out Value: TDynByteArray);
begin
  InternalRegReadBinaryAsArray(RootKey, Key, Name, True, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure RegReadBinaryDef(const RootKey: DelphiHKEY; const Key, Name: string;
  out Value: TDynByteArray; const Def: TDynByteArray);
begin
  if not InternalRegReadBinaryAsArray(RootKey, Key, Name, False, Value) then
    Value := Copy(Def);
end;

//--------------------------------------------------------------------------------------------------

function InternalRegReadBinaryAsAnsiString(const RootKey: DelphiHKEY; const Key,
  Name: AnsiString; RaiseException: Boolean; out Value: AnsiString): Boolean;
var
  DataPtr: Pointer;
  DataSize: DWord;
  Ptr: PAnsiChar;
  Len: Integer;
begin
  Value := '';
  Result := InternalRegReadBinaryA(RootKey, Key, Name, RaiseException, DataPtr, DataSize);
  if Result then
  begin
    try
      if DataSize > 0 then
      begin
        Ptr := DataPtr;
        Len := DataSize;
        Inc(Ptr, Len - 1);
        if Ptr^ = #0 then
          Dec(Len);
        SetString(Value, PAnsiChar(DataPtr), Len);
      end;
    finally
      if DataSize > 0 then
        FreeMem(DataPtr);
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function RegReadBinaryAsAnsiString(const RootKey: DelphiHKEY; const Key, Name:
  AnsiString): AnsiString;
begin
  InternalRegReadBinaryAsAnsiString(RootKey, Key, Name, True, Result);
end;

//--------------------------------------------------------------------------------------------------

function RegReadBinaryAsAnsiStringDef(const RootKey: DelphiHKEY; const Key,
  Name: AnsiString; const Def: AnsiString): AnsiString;
begin
  if not InternalRegReadBinaryAsAnsiString(RootKey, Key, Name, False, Result) then
    Result := Def;
end;

//--------------------------------------------------------------------------------------------------

function InternalRegReadBinaryAsWideString(const RootKey: DelphiHKEY; const Key,
  Name: WideString; RaiseException: Boolean; out Value: WideString): Boolean;
var
  DataPtr: Pointer;
  DataSize: DWord;
  Ptr: PWideChar;
  Len: Integer;
begin
  Value := '';
  Result := InternalRegReadBinaryW(RootKey, Key, Name, RaiseException, DataPtr, DataSize);
  if Result then
  begin
    try
      if DataSize > 0 then
      begin
        Ptr := DataPtr;
        Len := DataSize div SizeOf(WideChar);
        Inc(Ptr, Len - 1);
        if Ptr^ = #0 then
          Dec(Len);
        SetString(Value, PWideChar(DataPtr), Len);
      end;
    finally
      if DataSize > 0 then
        FreeMem(DataPtr);
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function RegReadBinaryAsWideString(const RootKey: DelphiHKEY; const Key, Name:
  WideString): WideString;
begin
  InternalRegReadBinaryAsWideString(RootKey, Key, Name, True, Result);
end;

//--------------------------------------------------------------------------------------------------

function RegReadBinaryAsWideStringDef(const RootKey: DelphiHKEY; const Key,
  Name: string; const Def: WideString): WideString;
begin
  if not InternalRegReadBinaryAsWideString(RootKey, Key, Name, False, Result) then
    Result := Def;
end;

//--------------------------------------------------------------------------------------------------

function RegReadDWORD(const RootKey: DelphiHKEY; const Key, Name: string): Int64;
begin
  InternalRegReadFixedSize(RootKey, Key, Name, [REG_DWORD], @Result, SizeOf(Result), True);
end;

//--------------------------------------------------------------------------------------------------

function RegReadDWORDDef(const RootKey: DelphiHKEY; const Key, Name: string; Def: Int64): Int64;
begin
  if not InternalRegReadFixedSize(RootKey, Key, Name, [REG_DWORD], @Result, SizeOf(Result), False) then
    Result := Def;
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteBool(const RootKey: DelphiHKEY; const Key, Name: string; Value: Boolean);
begin
  RegWriteInteger(RootKey, Key, Name, Ord(Value));
end;

//--------------------------------------------------------------------------------------------------

procedure InternalRegWriteA(const RootKey: DelphiHKEY; const Key, Name: AnsiString;
  RegKind: TRegKind; const Value; ValueSize: Cardinal);
var
  RegKey: HKEY;
  Ret: Longint;
begin
  if RegOpenKeyExA(RootKey, RelativeKeyA(Key), 0, KEY_SET_VALUE, RegKey) = ERROR_SUCCESS then
  begin
    Ret := RegSetValueExA(RegKey, PAnsiChar(Name), 0, RegKind, @Value, ValueSize);
    RegCloseKey(RegKey);
    if Ret <> ERROR_SUCCESS then
      WriteError(Key);
  end
  else
    WriteError(Key);
end;

//--------------------------------------------------------------------------------------------------

procedure InternalRegWriteW(const RootKey: DelphiHKEY; const Key, Name: WideString;
  RegKind: TRegKind; const Value; ValueSize: Cardinal);
var
  RegKey: HKEY;
  Ret: Longint;
begin
  if RegOpenKeyExW(RootKey, RelativeKeyW(Key), 0, KEY_SET_VALUE, RegKey) = ERROR_SUCCESS then
  begin
    Ret := RegSetValueExW(RegKey, PWideChar(Name), 0, RegKind, @Value, ValueSize);
    RegCloseKey(RegKey);
    if Ret <> ERROR_SUCCESS then
      WriteError(Key);
  end
  else
    WriteError(Key);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteInteger(const RootKey: DelphiHKEY; const Key, Name: string; Value: Integer);
begin
  InternalRegWriteA(RootKey, Key, Name, REG_DWORD, Value, SizeOf(Value));
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteCardinal(const RootKey: DelphiHKEY; const Key, Name: string; Value: Cardinal);
begin
  InternalRegWriteA(RootKey, Key, Name, REG_DWORD, Value, SizeOf(Value));
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteInt64(const RootKey: DelphiHKEY; const Key, Name: string; Value: Int64);
begin
  InternalRegWriteA(RootKey, Key, Name, REG_QWORD, Value, SizeOf(Value));
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteUInt64(const RootKey: DelphiHKEY; const Key, Name: string; Value: UInt64);
begin
  InternalRegWriteA(RootKey, Key, Name, REG_QWORD, Value, SizeOf(Value));
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteAnsiString(const RootKey: DelphiHKEY; const Key, Name, Value: AnsiString);
begin
  InternalRegWriteA(RootKey, Key, Name, REG_SZ, PAnsiChar(Value)^,
    (Length(Value) + 1) * SizeOf(AnsiChar));
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteWideString(const RootKey: DelphiHKEY; const Key, Name, Value: WideString);
begin
  InternalRegWriteW(RootKey, Key, Name, REG_SZ, PWideChar(Value)^,
    (Length(Value) + 1) * SizeOf(WideChar));
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteString(const RootKey: DelphiHKEY; const Key, Name, Value: string);
begin
  RegWriteAnsiString(RootKey, Key, Name, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteMultiAnsiString(const RootKey: DelphiHKEY; const Key, Name: AnsiString;
  const Value: AnsiString);
begin
  InternalRegWriteA(RootKey, Key, Name, REG_MULTI_SZ, PAnsiChar(Value)^,
    (Length(Value) + 1) * SizeOf(AnsiChar));
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteMultiWideString(const RootKey: DelphiHKEY; const Key, Name: WideString;
  const Value: WideString);
begin
  InternalRegWriteW(RootKey, Key, Name, REG_MULTI_SZ, PWideChar(Value)^,
    (Length(Value) + 1) * SizeOf(WideChar));
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteMultiString(const RootKey: DelphiHKEY; const Key, Name: string;
  const Value: string);
begin
  RegWriteMultiAnsiString(RootKey, Key, Name, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteMultiString(const RootKey: DelphiHKEY; const Key, Name: string;
  const Value: array of string);
begin
  RegWriteMultiAnsiString(RootKey, Key, Name, StringsToMultiString(Value));
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteMultiString(const RootKey: DelphiHKEY; const Key, Name: string;
  const Value: TDynStringArray); 
begin
  RegWriteMultiAnsiString(RootKey, Key, Name, StringsToMultiString(Value));
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteMultiString(const RootKey: DelphiHKEY; const Key, Name: string;
  const Value: TStrings);
begin
  RegWriteMultiAnsiString(RootKey, Key, Name, StringsToMultiString(Value));
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteBinary(const RootKey: DelphiHKEY; const Key, Name: string; const Value; const ValueSize: Cardinal);
begin
  InternalRegWriteA(RootKey, Key, Name, REG_BINARY, Value, ValueSize);
end;

//--------------------------------------------------------------------------------------------------

procedure RegWriteDWORD(const RootKey: DelphiHKEY; const Key, Name: string; Value: Int64);
begin
  InternalRegWriteA(RootKey, Key, Name, REG_DWORD, Value, SizeOf(Value));
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
    if RegOpenKeyEx(RootKey, RelativeKeyA(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
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
    if RegOpenKeyEx(RootKey, RelativeKeyA(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
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
  if RegOpenKeyEx(RootKey, RelativeKeyA(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
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
  Result := (RegOpenKeyEx(RootKey, RelativeKeyA(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS);
  if Result then RegCloseKey(RegKey);
end;

//--------------------------------------------------------------------------------------------------

function RegSaveList(const RootKey: DelphiHKEY; const Key: string;
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

function RegLoadList(const RootKey: DelphiHKEY; const Key: string;
  const ListName: string; const SaveTo: TStrings): Boolean;
var
  I, N: Integer;
  Subkey: string;
begin
  SaveTo.BeginUpdate;
  try
    SaveTo.Clear;
    Subkey := Key + '\' + ListName;
    N := RegReadInteger(RootKey, Subkey, CItems);
    for I := 1 to N do
      SaveTo.Add(RegReadString(RootKey, Subkey, IntToStr(I)));
    Result := N > 0;
  finally
    SaveTo.EndUpdate;
  end;
end;

//--------------------------------------------------------------------------------------------------

function RegDelList(const RootKey: DelphiHKEY; const Key: string; const ListName: string): Boolean;
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
// Revision 1.6  2004/04/06 04:45:57  peterjhaas
// Unite the single read functions and the single write functions, add Cardinal, Int64, UInt64 and Multistring support
//

end.
