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
{ The Original Code is JclSecurity.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright (C) Marcel van Brakel. All Rights Reserved.  }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel van Brakel                                                                              }
{   Peter Friese                                                                                   }
{   Robert Marquardt (marquardt)                                                                   }
{   John C Molyneux                                                                                }
{   Robert Rossmair (rrossmair)                                                                    }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Various NT security related routines to perform commen asks such as enabling and disabling       }
{ privileges.                                                                                      }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclSecurity;

{$I jcl.inc}
{$I windowsonly.inc}

{$HPPEMIT '#define TTokenInformationClass TOKEN_INFORMATION_CLASS'}

interface

uses
  Windows, SysUtils,
  JclBase;

//--------------------------------------------------------------------------------------------------
// Access Control
//--------------------------------------------------------------------------------------------------

function CreateNullDacl(var Sa: TSecurityAttributes;
  const Inheritable: Boolean): PSecurityAttributes;
function CreateInheritable(var Sa: TSecurityAttributes): PSecurityAttributes;

//--------------------------------------------------------------------------------------------------
// Privileges
//--------------------------------------------------------------------------------------------------

function IsAdministrator: Boolean;
function EnableProcessPrivilege(const Enable: Boolean;
  const Privilege: string): Boolean;
function EnableThreadPrivilege(const Enable: Boolean;
  const Privilege: string): Boolean;
function IsPrivilegeEnabled(const Privilege: string): Boolean;

function GetPrivilegeDisplayName(const PrivilegeName: string): string;
function SetUserObjectFullAccess(hUserObject: THandle): Boolean;
function GetUserObjectName(hUserObject: THandle): string;

//--------------------------------------------------------------------------------------------------
// Account Information
//--------------------------------------------------------------------------------------------------

procedure LookupAccountBySid(Sid: PSID; out Name, Domain: string);
procedure QueryTokenInformation(Token: THandle; InformationClass: TTokenInformationClass; var Buffer: Pointer);
procedure FreeTokenInformation(var Buffer: Pointer);
{$IFNDEF FPC}
function GetInteractiveUserName: string;
{$ENDIF ~FPC}

implementation

uses
  {$IFDEF FPC}
  WinSysUt,
  JwaAccCtrl,
  {$ELSE}
  AccCtrl,
  {$ENDIF FPC}
  JclResources, JclStrings, JclSysInfo, JclWin32;

//==================================================================================================
// Access Control
//==================================================================================================

function CreateNullDacl(var Sa: TSecurityAttributes;
  const Inheritable: Boolean): PSecurityAttributes;
begin
  if IsWinNT then
  begin
    Sa.lpSecurityDescriptor := AllocMem(SizeOf(TSecurityDescriptor));
    try
      Sa.nLength := SizeOf(Sa);
      Sa.bInheritHandle := Inheritable;
      Win32Check(InitializeSecurityDescriptor(Sa.lpSecurityDescriptor, SECURITY_DESCRIPTOR_REVISION));
      Win32Check(SetSecurityDescriptorDacl(Sa.lpSecurityDescriptor, True, nil, False));
      Result := @Sa;
    except
      FreeMem(Sa.lpSecurityDescriptor);
      Sa.lpSecurityDescriptor := nil;
      raise;
    end;
  end
  else
  begin
    Sa.lpSecurityDescriptor := nil;
    Result := nil;
  end;
end;

//--------------------------------------------------------------------------------------------------

function CreateInheritable(var Sa: TSecurityAttributes): PSecurityAttributes;
begin
  Sa.nLength := SizeOf(Sa);
  Sa.lpSecurityDescriptor := nil;
  Sa.bInheritHandle := True;
  if IsWinNT then
    Result := @Sa
  else
    Result := nil;
end;

//==================================================================================================
// Privileges
//==================================================================================================

function IsAdministrator: Boolean;
var
  psidAdmin: Pointer;
  Token: THandle;
  Count: DWORD;
  TokenInfo: PTokenGroups;
  HaveToken: Boolean;
  I: Integer;
begin
  Result := not IsWinNT;
  if Result then  // Win9x/ME
    Exit;
  psidAdmin := nil;
  TokenInfo := nil;
  HaveToken := False;
  try
    HaveToken := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, Token);
    if (not HaveToken) and (GetLastError = ERROR_NO_TOKEN) then
      HaveToken := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, Token);
    if HaveToken then
    begin
      {$IFDEF FPC}
      Win32Check(AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
        SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0,
        psidAdmin));
      if GetTokenInformation(Token, TokenGroups, nil, 0, @Count) or
       (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
         RaiseLastOSError;
      TokenInfo := PTokenGroups(AllocMem(Count));
      Win32Check(GetTokenInformation(Token, TokenGroups, TokenInfo, Count, @Count));
      {$ELSE FPC}
      Win32Check(AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
        SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0,
        psidAdmin));
      if GetTokenInformation(Token, TokenGroups, nil, 0, Count) or
       (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
         RaiseLastOSError;
      TokenInfo := PTokenGroups(AllocMem(Count));  
      Win32Check(GetTokenInformation(Token, TokenGroups, TokenInfo, Count, Count));
      {$ENDIF FPC}
      for I := 0 to TokenInfo^.GroupCount - 1 do
      begin
        {$RANGECHECKS OFF} // Groups is an array [0..0] of TSIDAndAttributes, ignore ERangeError
        Result := EqualSid(psidAdmin, TokenInfo^.Groups[I].Sid);
        {$IFDEF RANGECHECKS_ON}
        {$RANGECHECKS ON}
        {$ENDIF RANGECHECKS_ON}
        if Result then
          Break;
      end;
    end;
  finally
    if TokenInfo <> nil then
      FreeMem(TokenInfo);
    if HaveToken then
      CloseHandle(Token);
    if psidAdmin <> nil then
      FreeSid(psidAdmin);
  end;
end;

//--------------------------------------------------------------------------------------------------

function EnableProcessPrivilege(const Enable: Boolean;
  const Privilege: string): Boolean;
const
  PrivAttrs: array [Boolean] of DWORD = (0, SE_PRIVILEGE_ENABLED);
var
  Token: THandle;
  TokenPriv: TTokenPrivileges;
begin
  Result := not IsWinNT;
  if Result then  // if Win9x, then function return True
    Exit;
  if OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES, Token) then
  begin
    TokenPriv.PrivilegeCount := 1;
    LookupPrivilegeValue(nil, PChar(Privilege), TokenPriv.Privileges[0].Luid);
    TokenPriv.Privileges[0].Attributes := PrivAttrs[Enable];
    JclWin32.AdjustTokenPrivileges(Token, False, TokenPriv, SizeOf(TokenPriv), nil, nil);
    Result := GetLastError = ERROR_SUCCESS;
    CloseHandle(Token);
  end;
end;

//--------------------------------------------------------------------------------------------------

function EnableThreadPrivilege(const Enable: Boolean;
  const Privilege: string): Boolean;
const
  PrivAttrs: array [Boolean] of DWORD = (0, SE_PRIVILEGE_ENABLED);
var
  Token: THandle;
  TokenPriv: TTokenPrivileges;
  HaveToken: Boolean;
begin
  Result := not IsWinNT;
  if Result then  // Win9x/ME
    Exit;
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
    JclWin32.AdjustTokenPrivileges(Token, False, TokenPriv, SizeOf(TokenPriv), nil, nil);
    Result := GetLastError = ERROR_SUCCESS;
    CloseHandle(Token);
  end;
end;

//--------------------------------------------------------------------------------------------------

function IsPrivilegeEnabled(const Privilege: string): Boolean;
var
  Token: THandle;
  TokenPriv: TPrivilegeSet;
  Res: LongBool;
  HaveToken: Boolean;
begin
  Result := not IsWinNT;
  if Result then  // Win9x/ME
    Exit;
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

//--------------------------------------------------------------------------------------------------

function GetPrivilegeDisplayName(const PrivilegeName: string): string;
var
  Count: DWORD;
  LangID: DWORD;
begin
  if IsWinNT then
  begin
    Count  := 0;
    LangID := LANG_USER_DEFAULT;

    // have the the API function determine the required string length
    if not LookupPrivilegeDisplayName(nil, PChar(PrivilegeName), PChar(Result), Count, LangID) then
      Count := 256;
    SetLength(Result, Count + 1);

    if LookupPrivilegeDisplayName(nil, PChar(PrivilegeName), PChar(Result), Count, LangID) then
      StrResetLength(Result)
    else
      Result := '';
  end
  else
    Result := '';  // Win9x/ME
end;

//--------------------------------------------------------------------------------------------------

function SetUserObjectFullAccess(hUserObject: THandle): Boolean;
var
  Sd: PSecurity_Descriptor;
  Si: Security_Information;
begin
  Result := not IsWinNT;
  if Result then  // Win9x/ME
    Exit;
  { TODO : Check the success of called functions }
  Sd := PSecurity_Descriptor(LocalAlloc(LPTR, SECURITY_DESCRIPTOR_MIN_LENGTH));
  InitializeSecurityDescriptor(Sd, SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(Sd, True, nil, False);

  Si := DACL_SECURITY_INFORMATION;
  Result := SetUserObjectSecurity(hUserObject, Si, Sd);

  LocalFree(HLOCAL(Sd));
end;

//--------------------------------------------------------------------------------------------------

function GetUserObjectName(hUserObject: THandle): string;
var
  Count: DWORD;
begin
  if IsWinNT then
  begin
    // have the API function determine the required string length
    GetUserObjectInformation(hUserObject, UOI_NAME, PChar(Result), 0, Count);
    SetLength(Result, Count + 1);

    if GetUserObjectInformation(hUserObject, UOI_NAME, PChar(Result), Count, Count) then
      StrResetLength(Result)
    else
      Result := '';
  end
  else
    Result := '';
end;

//==================================================================================================
// Account Information
//==================================================================================================

procedure LookupAccountBySid(Sid: PSID; out Name, Domain: string);
var
  NameSize, DomainSize: DWORD;
  Use: SID_NAME_USE;
begin
  if IsWinNT then
  begin
    NameSize := 0;
    DomainSize := 0;
    { TODO : Check the success }
    LookupAccountSid(nil, Sid, nil, NameSize, nil, DomainSize, Use);
    SetLength(Name, NameSize);
    SetLength(Domain, DomainSize);
    Win32Check(LookupAccountSid(nil, Sid, PChar(Name), NameSize, PChar(Domain), DomainSize, Use));
    SetLength(Domain, StrLen(PChar(Domain)));
    SetLength(Name, StrLen(PChar(Name)));
  end
  else
  begin             // if Win9x, then function return ''
    Name := '';
    Domain := '';
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure QueryTokenInformation(Token: THandle; InformationClass: TTokenInformationClass;
  var Buffer: Pointer);
var
  Ret: BOOL;
  Length, LastError: DWORD;
begin
  Buffer := nil;
  if not IsWinNT then  // Win9x/ME
    Exit;
  Length := 0;
  {$IFDEF FPC}
  Ret := GetTokenInformation(Token, InformationClass, Buffer, Length, @Length);
  {$ELSE}
  Ret := GetTokenInformation(Token, InformationClass, Buffer, Length, Length);
  {$ENDIF FPC}
  if (not Ret) and (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
  begin
    GetMem(Buffer, Length);
    {$IFDEF FPC}
    Ret := GetTokenInformation(Token, InformationClass, Buffer, Length, @Length);
    {$ELSE}
    Ret := GetTokenInformation(Token, InformationClass, Buffer, Length, Length);
    {$ENDIF FPC}
    if not Ret then
    begin
      LastError := GetLastError;
      FreeTokenInformation(Buffer);
      SetLastError(LastError);
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure FreeTokenInformation(var Buffer: Pointer);
begin
  if Buffer <> nil then
    FreeMem(Buffer);
  Buffer := nil;
end;

//--------------------------------------------------------------------------------------------------

{$IFNDEF FPC} // JclSysInfo.GetShellProcessHandle not available
function GetInteractiveUserName: string;
var
  Handle: THandle;
  Token: THandle;
  User: PTokenUser;
  Name, Domain: string;
begin
  Result := '';
  if not IsWinNT then  // if Win9x, then function return ''
    Exit;
  Handle := GetShellProcessHandle;
  try
    Win32Check(OpenProcessToken(Handle, TOKEN_QUERY, Token));
    try
      QueryTokenInformation(Token, TokenUser, Pointer(User));
      try
        LookupAccountBySid(User.User.Sid, Name, Domain);
        Result := Domain + '\' + Name;
      finally
        FreeMem(User);
      end;
    finally
      CloseHandle(Token);
    end;
  finally
    CloseHandle(Handle);
  end;
end;
{$ENDIF ~FPC}

// History:

// $Log$
// Revision 1.15  2004/10/25 08:51:23  marquardt
// PH cleaning
//
// Revision 1.14  2004/10/17 21:00:16  mthoma
// cleaning
//
// Revision 1.13  2004/07/29 07:58:22  marquardt
// inc files updated
//
// Revision 1.12  2004/07/28 18:00:54  marquardt
// various style cleanings, some minor fixes
//
// Revision 1.11  2004/06/14 11:05:53  marquardt
// symbols added to all ENDIFs and some other minor style changes like removing IFOPT
//
// Revision 1.10  2004/06/02 03:23:47  rrossmair
// cosmetic changes in several units (code formatting, help TODOs processed etc.)
//
// Revision 1.9  2004/05/13 07:46:06  rrossmair
// changes for FPC 1.9.3+ compatibility
//
// Revision 1.8  2004/05/05 07:30:54  rrossmair
// Changes for FPC compatibility; header updated according to new policy: initial developers & contributors listed
//
// Revision 1.7  2004/04/06 04:55:18  
// adapt compiler conditions, add log entry
//

end.
