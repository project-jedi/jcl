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
{ The Original Code is JclLANMan.pas.                                          }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This unit contains routines and classes to handle user and group management  }
{ tasks. As the name implies, it uses the LAN Manager API.                     }
{                                                                              }
{ Unit owner: Peter Friese                                                     }
{ Last modified: July 16, 2001                                                 }
{                                                                              }
{******************************************************************************}

unit JclLANMan;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

uses
  Windows, Classes;

//------------------------------------------------------------------------------
// User Management
//------------------------------------------------------------------------------

type
  TNetUserFlag = (ufAccountDisable, ufHomedirRequired, ufLockout,
    ufPasswordNotRequired, ufPasswordCantChange, ufDontExpirePassword,
    ufMNSLogonAccount);
  TNetUserFlags = set of TNetUserFlag;
  TNetUserInfoFlag = (uifScript, uifTempDuplicateAccount, uifNormalAccount,
    uifInterdomainTrustAccount, uifWorkstationTrustAccount, uifServerTrustAccount);
  TNetUserInfoFlags = set of TNetUserInfoFlag;
  TNetUserPriv = (upUnknown, upGuest, upUser, upAdmin);
  TNetUserAuthFlag = (afOpPrint, afOpComm, afOpServer, afOpAccounts);
  TNetUserAuthFlags = set of TNetUserAuthFlag;
  TNetWellKnownRID = (wkrAdmins, wkrUsers, wkrGuests, wkrPowerUsers, wkrBackupOPs,
    wkrReplicator, wkrEveryone);

function CreateAccount(const Server, Username, Fullname, Password, Description,
  Homedir, Script: string): boolean;
function CreateLocalAccount(const Username, Fullname, Password, Description,
  Homedir, Script: string): Boolean;
function DeleteAccount(const Servername, Username: string): Boolean;
function DeleteLocalAccount(Username: string): Boolean;
function CreateLocalGroup(const Server, Groupname, Description: string): Boolean;
function CreateGlobalGroup(const Server, Groupname, Description: string): Boolean;
function DeleteLocalGroup(const Server, Groupname: string): Boolean;

function GetLocalGroups(const Server: string; const Groups: TStrings): boolean;
function GetGlobalGroups(const Server: string; const Groups: TStrings): boolean;
function LocalGroupExists(const Group: string): boolean;
function GlobalGroupExists(const Server, Group: string): boolean;

function AddAccountToLocalGroup(const Accountname, Groupname: string): Boolean;
function LookupGroupName(const Server: string; const RID: TNetWellKnownRID): string;
procedure ParseAccountName(const QualifiedName: string; var Domain, UserName: string);
function IsLocalAccount(const AccountName: string): boolean;

implementation

uses
  SysUtils,
  LM, JclBase, JclStrings, JclWin32;

//------------------------------------------------------------------------------
// User Management
//------------------------------------------------------------------------------

function CreateAccount(const Server, Username, Fullname, Password, Description,
  Homedir, Script: string): boolean;
var
  wServer, wUsername, wFullname,
  wPassword, wDescription, wHomedir, wScript: WideString;
  details: USER_INFO_2;
  err: NET_API_STATUS;
  parmErr: DWORD;
begin
  wServer := Server;
  wUsername := Username;
  wFullname := Fullname;
  wPassword := Password;
  wDescription := Description;
  wScript := Script;
  wHomedir := Homedir;

  FillChar (details, sizeof(details), 0);
  with details do
  begin
    usri2_name := PWideChar(wUsername);
    usri2_full_name := PWideChar(wFullname);
    usri2_password := PWideChar(wPassword);
    usri2_comment := PWideChar(wDescription);
    usri2_priv := USER_PRIV_USER;
    usri2_flags := UF_SCRIPT;
    usri2_script_path := PWideChar(wScript);
    usri2_home_dir := PWideChar(wHomedir);
  end;

  err := NetUserAdd(PWideChar(wServer), 2, @details, @parmErr);
  Result := (err = NERR_SUCCESS);
end;

//------------------------------------------------------------------------------

function CreateLocalAccount(const Username, Fullname, Password, Description,
  Homedir, Script: string): boolean;
begin
  Result := CreateAccount('', Username, Fullname, Password, Description, Homedir, Script);
end;

//------------------------------------------------------------------------------

function DeleteAccount(const Servername, Username: string): Boolean;
var
  wServername, wUsername: WideString;
  err: NET_API_STATUS;
begin
  wServername := Servername;
  wUsername := Username;
  err := NetUserDel(PWideChar(wServername), PWideChar(wUsername));
  Result := (err = NERR_SUCCESS);
end;

//------------------------------------------------------------------------------

function DeleteLocalAccount(Username: string): Boolean;
begin
  Result := DeleteAccount('', Username);
end;

//------------------------------------------------------------------------------

function CreateGlobalGroup(const Server, Groupname, Description: string): boolean;
var
  wServer, wGroupname, wDescription: WideString;
  details: GROUP_INFO_1;
  err: NET_API_STATUS;
  parmErr: DWORD;
begin
  wServer := Server;
  wGroupname := Groupname;
  wDescription := Description;

  FillChar (details, sizeof(details), 0);
  details.grpi1_name := PWideChar(wGroupName);
  details.grpi1_comment := PWideChar(wDescription);

  err := NetGroupAdd(PWideChar(wServer), 1, @details, @parmErr);
  Result := (err = NERR_SUCCESS);
end;

//------------------------------------------------------------------------------

function CreateLocalGroup(const Server, Groupname, Description: string): boolean;
var
  wServer, wGroupname, wDescription: WideString;
  details: LOCALGROUP_INFO_1;
  err: NET_API_STATUS;
  parmErr: DWORD;
begin
  wServer := Server;
  wGroupname := Groupname;
  wDescription := Description;

  FillChar (details, sizeof(details), 0);
  details.lgrpi1_name := PWideChar(wGroupName);
  details.lgrpi1_comment := PWideChar(wDescription);

  err := NetLocalGroupAdd(PWideChar(wServer), 1, @details, @parmErr);
  Result := (err = NERR_SUCCESS);
end;

//------------------------------------------------------------------------------

function DeleteLocalGroup(const Server, Groupname: string): Boolean;
var
  wServername, wUsername: WideString;
  err: NET_API_STATUS;
begin
  wServername := Server;
  wUsername := Groupname;
  err := NetLocalGroupDel(PWideChar(wServername), PWideChar(wUsername));
  Result := (err = NERR_SUCCESS);
end;

//------------------------------------------------------------------------------

function GetLocalGroups(const Server: string; const Groups: TStrings): boolean;
var
  err: NET_API_STATUS;
  wServername: WideString;
  buffer: Pointer;
  details: PLocalGroupInfo0;
  entriesread, totalentries: Cardinal;
  i: integer;
begin
  wServername := Server;
  err := NetLocalGroupEnum(PWideChar(wServername), 0, buffer, MAX_PREFERRED_LENGTH,
    entriesread, totalentries, nil);

  if err = NERR_SUCCESS then begin
    details := PLocalGroupInfo0(buffer);
    for i := 0 to entriesread - 1 do begin
      Groups.Add(details^.lgrpi0_name);
      Inc(details);
    end;
  end;

  NetApiBufferFree(@details);
  Result := (err = NERR_SUCCESS);
end;

//------------------------------------------------------------------------------

function GetGlobalGroups(const Server: string; const Groups: TStrings): boolean;
var
  err: NET_API_STATUS;
  wServername: WideString;
  buffer: Pointer;
  details: PGroupInfo0;
  entriesread, totalentries: Cardinal;
  i: integer;
begin
  wServername := Server;
  err := NetGroupEnum(PWideChar(wServername), 0, buffer, MAX_PREFERRED_LENGTH,
    entriesread, totalentries, nil);

  if err = NERR_SUCCESS then begin
    details := PGroupInfo0(buffer);
    if (entriesread <> 1) or (details^.grpi0_name <> 'None') then
      for i := 0 to entriesread - 1 do begin
        Groups.Add(details^.grpi0_name);
        Inc(details);
      end;
  end
  else
    RaiseLastOSError;

  NetApiBufferFree(@details);
  Result := (err = NERR_SUCCESS);
end;

//------------------------------------------------------------------------------

function LocalGroupExists(const Group: string): boolean;
var
  groups: TStrings;
begin
  groups := TStringList.Create;
  try
    GetLocalGroups('', groups);
    Result := (groups.IndexOf(Group) >= 0);
  finally
    groups.Free;
  end;
end;

//------------------------------------------------------------------------------

function GlobalGroupExists(const Server, Group: string): boolean;
var
  groups: TStrings;
begin
  groups := TStringList.Create;
  try
    GetGlobalGroups(Server, groups);
    Result := (groups.IndexOf(Group) >= 0);
  finally
    groups.Free;
  end;
end;

//------------------------------------------------------------------------------
function DeleteGlobalGroup(const Server, Groupname: string): Boolean;
var
  wServername, wUsername: WideString;
  err: NET_API_STATUS;
begin
  wServername := Server;
  wUsername := Groupname;
  err := NetGroupDel(PWideChar(wServername), PWideChar(wUsername));
  Result := (err = NERR_SUCCESS);
end;

//------------------------------------------------------------------------------

function AddAccountToLocalGroup(const Accountname, Groupname: string): boolean;
var
  err: NET_API_STATUS;
  wAccountname, wGroupname: WideString;
  details: LOCALGROUP_MEMBERS_INFO_3;
begin
  wGroupname := Groupname;
  wAccountname := AccountName;

  details.lgrmi3_domainandname := PWideChar(wAccountname);
  err := NetLocalGroupAddMembers(nil, PWideChar(wGroupname), 3, @details, 1);
  Result := (err = NERR_SUCCESS);
end;

//------------------------------------------------------------------------------

function RIDToDWORD(const RID: TNetWellKnownRID): DWORD;
begin
  case RID of
    wkrAdmins: Result := DOMAIN_ALIAS_RID_ADMINS;
    wkrUsers: Result := DOMAIN_ALIAS_RID_USERS;
    wkrGuests: Result := DOMAIN_ALIAS_RID_GUESTS;
    wkrPowerUsers: Result := DOMAIN_ALIAS_RID_POWER_USERS;
    wkrBackupOPs: Result := DOMAIN_ALIAS_RID_BACKUP_OPS;
    wkrReplicator: Result := DOMAIN_ALIAS_RID_REPLICATOR;
  else // (wkrEveryone)
    Result := SECURITY_WORLD_RID;
  end;
end;


//------------------------------------------------------------------------------

function DWORDToRID(const RID: DWORD): TNetWellKnownRID;
begin
  case RID of
    DOMAIN_ALIAS_RID_ADMINS: Result := wkrAdmins;
    DOMAIN_ALIAS_RID_USERS: Result := wkrUsers;
    DOMAIN_ALIAS_RID_GUESTS: Result := wkrGuests;
    DOMAIN_ALIAS_RID_POWER_USERS: Result := wkrPowerUsers;
    DOMAIN_ALIAS_RID_BACKUP_OPS: Result := wkrBackupOPs;
    DOMAIN_ALIAS_RID_REPLICATOR: Result := wkrReplicator;
  else // (SECURITY_WORLD_RID)
    Result := wkrEveryone;
  end;
end;

//------------------------------------------------------------------------------

function LookupGroupName(const Server: string; const RID: TNetWellKnownRID): string;
var
  sia: SID_IDENTIFIER_AUTHORITY;
  rd1, rd2: DWORD;
  ridCOunt: integer;
  sd: PSID;
  AccountNameLen, DomainNameLen: DWORD;
  SidNameUse: SID_NAME_USE;
begin
  Result := '';
  rd2 := 0;

  if RID = wkrEveryOne then
  begin
    sia := SECURITY_WORLD_SID_AUTHORITY;
    rd1 := RIDToDWORD(RID);
    ridCount := 1;
  end
  else
  begin
    sia := SECURITY_NT_AUTHORITY;
    rd1 := SECURITY_BUILTIN_DOMAIN_RID;
    rd2 := RIDToDWORD(RID);
    ridCount := 2;
  end;
  if AllocateAndInitializeSid(sia, ridCount, rd1, rd2, 0, 0, 0, 0, 0, 0, sd) then
  try
    AccountNameLen := 0;
    DomainNameLen := 0;
    if not LookupAccountSID(PChar(Server), sd, PChar(Result), AccountNameLen,
                            nil, DomainNameLen, SidNameUse)
    then
      SetLength(Result, AccountNamelen);

    if LookupAccountSID(PChar(Server), sd, PChar(Result), AccountNameLen,
                        nil, DomainNameLen, sidNameUse)
    then
      StrResetLength(Result)
    else
      RaiseLastOSError;
  finally
    FreeSID(sd);
  end;
end;

//------------------------------------------------------------------------------

procedure ParseAccountName(const QualifiedName: string; var Domain, UserName: string);
var
  Parts: TStrings;
begin
  Parts := TStringList.Create;
  try
    StrTokenToStrings(QualifiedName, '\', Parts);
    if Parts.Count = 1 then
      UserName := Parts[0]
    else begin
      Domain := Parts[0];
      UserName := Parts[1];
    end;
  finally
    Parts.Free;
  end;
end;

//------------------------------------------------------------------------------

function IsLocalAccount(const AccountName: string): boolean;
var
  Domain: string;
  Username: string;
begin
  ParseAccountName(AccountName, Domain, Username);
  Result := (Domain = '');
end;

end.
