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
{ The Original Code is JclSecurity.pas.                                        }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ Various NT security related routines to perform commen asks such as enabling }
{ and disabling privileges.                                                    }
{                                                                              }
{ Unit owner: Peter Friese                                                     }
{ Last modified: January 29, 2000                                              }
{                                                                              }
{******************************************************************************}

unit JclSecurity;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

uses
  Windows,
  SysUtils,
  JclBase;

//------------------------------------------------------------------------------
// Access Control
//------------------------------------------------------------------------------

function AllowRegKeyForEveryone(Key: HKEY; Path: string): Boolean;
function CreateNullDacl(var Sa: TSecurityAttributes;
  const Inheritable: Boolean): PSecurityAttributes;
function CreateInheritable(var Sa: TSecurityAttributes): PSecurityAttributes;

//------------------------------------------------------------------------------
// Privileges
//------------------------------------------------------------------------------

function IsAdministrator: Boolean;
function EnableProcessPrivilege(const Enable: Boolean;
  const Privilege: string): Boolean;
function EnableThreadPrivilege(const Enable: Boolean;
  const Privilege: string): Boolean;
function IsPrivilegeEnabled(const Privilege: string): Boolean;

function GetPrivilegeDisplayName(const PrivilegeName: string): string;
function SetUserObjectFullAccess(hUserObject: THandle): Boolean;
function GetUserObjectName(hUserObject: THandle): string;

implementation

uses
  {$IFDEF COMPILER5_UP}
  AccCtrl, AclApi,
  {$ENDIF COMPILER5_UP}
  JclStrings, JclWin32;

//==============================================================================
// Access Control
//==============================================================================

function AllowRegKeyForEveryone(Key: HKEY; Path: string): Boolean;
var
  WidePath: PWideChar;
  Len: Integer;
begin
  case Key of
    HKEY_LOCAL_MACHINE:
      Path := 'MACHINE\' + Path;
    HKEY_CURRENT_USER:
      Path := 'CURRENT_USER\' + Path;
    HKEY_CLASSES_ROOT:
      Path := 'CLASSES_ROOT\' + Path;
    HKEY_USERS:
      Path := 'USERS\' + Path;
  end;
  Len := (Length(Path)+1)*SizeOf(WideChar);
  GetMem(WidePath,Len);
  MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, PChar(Path), -1, WidePath, Len);
  Result := SetNamedSecurityInfoW(WidePath, SE_REGISTRY_KEY,
    DACL_SECURITY_INFORMATION, nil, nil, nil, nil) = ERROR_SUCCESS;
  FreeMem(WidePath);
end;

//------------------------------------------------------------------------------

function CreateNullDacl(var Sa: TSecurityAttributes;
  const Inheritable: Boolean): PSecurityAttributes;
var
  Sd: PSecurityDescriptor;
begin
  Sd := AllocMem(SizeOf(TSecurityDescriptor));
  Win32Check(InitializeSecurityDescriptor(Sd, SECURITY_DESCRIPTOR_REVISION));
  Win32Check(SetSecurityDescriptorDacl(Sd, True, nil, False));
  Sa.nLength := SizeOf(Sa);
  Sa.lpSecurityDescriptor := Sd;
  Sa.bInheritHandle := Inheritable;
  Result := @Sa;
end;

//------------------------------------------------------------------------------

function CreateInheritable(var Sa: TSecurityAttributes): PSecurityAttributes;
begin
  Sa.nLength := SizeOf(Sa);
  Sa.lpSecurityDescriptor := nil;
  Sa.bInheritHandle := True;
  Result := @Sa;
end;

//==============================================================================
// Privileges
//==============================================================================

function IsAdministrator: Boolean;
var
  psidAdmin: Pointer;
  Token: THandle;
  Count: DWORD;
  TokenInfo: PTokenGroups;
  HaveToken: Boolean;
  I: Integer;
begin
  Result := False;
  psidAdmin := nil;
  TokenInfo := nil;
  HaveToken := False;
  try
    HaveToken := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, Token);
    if (not HaveToken) and (GetLastError = ERROR_NO_TOKEN) then
      HaveToken := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, Token);
    if HaveToken then
    begin
      Win32Check(AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
        SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0,
        psidAdmin));
      if GetTokenInformation(Token, TokenGroups, nil, 0, Count) or
       (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
         RaiseLastOSError;
      TokenInfo := PTokenGroups(AllocMem(Count));
      Win32Check(GetTokenInformation(Token, TokenGroups, TokenInfo, Count, Count));
      for I := 0 to TokenInfo^.GroupCount - 1 do
      begin
        {$R-} // Groups is an array [0..0] of TSIDAndAttributes, ignore ERangeError
        Result := EqualSid(psidAdmin, TokenInfo^.Groups[I].Sid);
        {$R+}
        if Result then Break;
      end;
    end;
  finally
    if TokenInfo <> nil then FreeMem(TokenInfo);
    if HaveToken then CloseHandle(Token);
    if psidAdmin <> nil then FreeSid(psidAdmin);
  end;
end;

//------------------------------------------------------------------------------

function EnableProcessPrivilege(const Enable: Boolean;
  const Privilege: string): Boolean;
const
  PrivAttrs: array [Boolean] of DWORD = (0, SE_PRIVILEGE_ENABLED);
var
  Token: THandle;
  TokenPriv: TTokenPrivileges;
begin
  Result := False;
  if OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES, Token) then
  begin
    TokenPriv.PrivilegeCount := 1;
    LookupPrivilegeValue(nil, PChar(Privilege), TokenPriv.Privileges[0].Luid);
    TokenPriv.Privileges[0].Attributes := PrivAttrs[Enable];
    JclWin32.AdjustTokenPrivileges(Token, False, TokenPriv, SizeOf(TokenPriv),
      nil, nil);
    Result := GetLastError = ERROR_SUCCESS;
    CloseHandle(Token);
  end;
end;

//------------------------------------------------------------------------------

function EnableThreadPrivilege(const Enable: Boolean;
  const Privilege: string): Boolean;
const
  PrivAttrs: array [Boolean] of DWORD = (0, SE_PRIVILEGE_ENABLED);
var
  Token: THandle;
  TokenPriv: TTokenPrivileges;
  HaveToken: Boolean;
begin
  Result := False;
  Token := 0;
  HaveToken := OpenThreadToken(GetCurrentThread, TOKEN_ADJUST_PRIVILEGES,
    False, Token);
  if (not HaveToken) and (GetLastError = ERROR_NO_TOKEN) then
    HaveToken := OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES,
      Token);
  if HaveToken then
  begin
    TokenPriv.PrivilegeCount := 1;
    LookupPrivilegeValue(nil, PChar(Privilege), TokenPriv.Privileges[0].Luid);
    TokenPriv.Privileges[0].Attributes := PrivAttrs[Enable];
    JclWin32.AdjustTokenPrivileges(Token, False, TokenPriv, SizeOf(TokenPriv),
      nil, nil);
    Result := GetLastError = ERROR_SUCCESS;
    CloseHandle(Token);
  end;
end;

//------------------------------------------------------------------------------

function IsPrivilegeEnabled(const Privilege: string): Boolean;
var
  Token: THandle;
  TokenPriv: TPrivilegeSet;
  Res: LongBool;
  HaveToken: Boolean;
begin
  Result := False;
  Token := 0;
  HaveToken := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, False, Token);
  if (not HaveToken) and (GetLastError = ERROR_NO_TOKEN) then
    HaveToken := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, Token);
  if HaveToken then
  begin
    TokenPriv.PrivilegeCount := 1;
    TokenPriv.Control := 0;
    LookupPrivilegeValue(nil, PChar(Privilege), TokenPriv.Privilege[0].Luid);
    Result := PrivilegeCheck(Token, TokenPriv, Res) and Res;
    CloseHandle(Token);
  end;
end;

//------------------------------------------------------------------------------

function GetPrivilegeDisplayName(const PrivilegeName: string): string;
var
  Count: DWORD;
  LangID: DWORD;
begin
  Count  := 0;
  LangID := 0; // li := DWORD(MAKELANGID(LANG_DEFAULT, LANG_USER));

  // have the the API function determine the required string length
  if not LookupPrivilegeDisplayName(nil, PChar(PrivilegeName), PChar(Result), Count, LangID) then
    Count := 256;
  SetLength(Result, Count + 1);

  if LookupPrivilegeDisplayName(nil, PChar(PrivilegeName), PChar(Result), Count, LangID) then
    StrResetLength(Result)
  else
    Result:= '';
end;

//------------------------------------------------------------------------------

function SetUserObjectFullAccess(hUserObject: THandle): Boolean;
var
  Sd: PSecurity_Descriptor;
  Si: Security_Information;
begin
  Sd := PSecurity_Descriptor(LocalAlloc(LPTR, SECURITY_DESCRIPTOR_MIN_LENGTH));
  InitializeSecurityDescriptor(Sd, SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(Sd, True, nil, False);

  Si := DACL_SECURITY_INFORMATION;
  Result := SetUserObjectSecurity(hUserObject, Si, Sd);

  LocalFree(HLOCAL(Sd));
end;

//------------------------------------------------------------------------------

function GetUserObjectName(hUserObject: THandle): string;
var
  Count: DWORD;
begin
  // have the the API function determine the required string length
  GetUserObjectInformation(hUserObject, UOI_NAME, PChar(Result), 0, Count);
  SetLength(Result, Count + 1);

  if GetUserObjectInformation(hUserObject, UOI_NAME, PChar(Result), Count, Count) then
    StrResetLength(Result)
  else
    Result := '';
end;

end.
