{******************************************************************************}
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.0 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JclMapi.pas.                                            }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{ Last modified: June 26, 2000                                                 }
{                                                                              }
{******************************************************************************}

unit JclMapi;

{$I JCL.INC}

interface

uses
  Windows, Classes, Mapi, SysUtils,
  {$IFDEF DELPHI5_UP}
  Contnrs,
  {$ENDIF}
  JclBase;

//------------------------------------------------------------------------------
// Simple MAPI interface
//------------------------------------------------------------------------------

type
  EJclMapiError = EJclError;

  TJclMapiClient = record
    ClientName: string;
    ClientPath: string;
    RegKeyName: string;
    Valid: Boolean;
  end;

  TJclMapiClientType = (ctAutomatic, ctMapi, ctDirect);

  TJclSimpleMapi = class (TObject)
  private
    FAnyClientInstalled: Boolean;
    FClients: array of TJclMapiClient;
    FClientLibHandle: THandle;
    FFunctions: array of ^Pointer;
    FMapiInstalled: Boolean;
    FMapiVersion: string;
    FSimpleMapiInstalled: Boolean;
    FDefaultClientIndex: Integer;
    FSelectedClientIndex: Integer;
    FSelectedClientType: TJclMapiClientType;
    FBeforeUnloadClient: TNotifyEvent;
    FTaskActiveWindow: HWND;
    FTaskWindowsList: TList;
    FMapiAddress: TFNMapiAddress;
    FMapiDeleteMail: TFNMapiDeleteMail;
    FMapiDetails: TFNMapiDetails;
    FMapiFindNext: TFNMapiFindNext;
    FMapiFreeBuffer: TFNMapiFreeBuffer;
    FMapiLogOff: TFNMapiLogOff;
    FMapiLogOn: TFNMapiLogOn;
    FMapiReadMail: TFNMapiReadMail;
    FMapiResolveName: TFNMapiResolveName;
    FMapiSaveMail: TFNMapiSaveMail;
    FMapiSendDocuments: TFNMapiSendDocuments;
    FMapiSendMail: TFNMapiSendMail;
    function GetClientCount: Integer;
    function GetClients(Index: Integer): TJclMapiClient;
    procedure SetSelectedClientIndex(const Value: Integer);
    procedure SetSelectedClientType(const Value: TJclMapiClientType);
    function UseMapi: Boolean;
    function GetCurrentClientName: string;
  protected
    procedure CheckListIndex(I: Integer);
    function GetClientLibName: string;
    procedure ReadMapiSettings;
  public
    constructor Create;
    destructor Destroy; override;
    function ClientLibLoaded: Boolean;
    procedure LoadClientLib;
    procedure RestoreTaskWindowsState;
    procedure SaveTaskWindowsState;
    procedure UnloadClientLib;
    property AnyClientInstalled: Boolean read FAnyClientInstalled;
    property ClientCount: Integer read GetClientCount;
    property Clients[Index: Integer]: TJclMapiClient read GetClients; default;
    property CurrentClientName: string read GetCurrentClientName;
    property DefaultClientIndex: Integer read FDefaultClientIndex;
    property MapiInstalled: Boolean read FMapiInstalled;
    property MapiVersion: string read FMapiVersion;
    property SelectedClientIndex: Integer read FSelectedClientIndex write SetSelectedClientIndex;
    property SelectedClientType: TJclMapiClientType read FSelectedClientType write SetSelectedClientType;
    property SimpleMapiInstalled: Boolean read FSimpleMapiInstalled;
    property BeforeUnloadClient: TNotifyEvent read FBeforeUnloadClient write FBeforeUnloadClient;

    // Simple MAPI functions
    property MapiAddress: TFNMapiAddress read FMapiAddress;
    property MapiDeleteMail: TFNMapiDeleteMail read FMapiDeleteMail;
    property MapiDetails: TFNMapiDetails read FMapiDetails;
    property MapiFindNext: TFNMapiFindNext read FMapiFindNext;
    property MapiFreeBuffer: TFNMapiFreeBuffer read FMapiFreeBuffer;
    property MapiLogOff: TFNMapiLogOff read FMapiLogOff;
    property MapiLogOn: TFNMapiLogOn read FMapiLogOn;
    property MapiReadMail: TFNMapiReadMail read FMapiReadMail;
    property MapiResolveName: TFNMapiResolveName read FMapiResolveName;
    property MapiSaveMail: TFNMapiSaveMail read FMapiSaveMail;
    property MapiSendDocuments: TFNMapiSendDocuments read FMapiSendDocuments;
    property MapiSendMail: TFNMapiSendMail read FMapiSendMail;
  end;

//------------------------------------------------------------------------------
// Simple email
//------------------------------------------------------------------------------

  TJclEmail = class (TJclSimpleMapi)
  public
    procedure SendMail(const Recipient, Name, Subject, Body: string; ParentWND: HWND);
    class procedure SimpleSendMail(const Recipient, Name, Subject, Body: string; ParentWND: HWND);
  end;

//------------------------------------------------------------------------------
// MAPI Errors
//------------------------------------------------------------------------------

function MapiCheck(const Res: DWORD; IgnoreUserAbort: Boolean {$IFDEF SUPPORTS_DEFAULTPARAMS} = True {$ENDIF}): DWORD;

function MapiErrorMessage(const ErrorCode: DWORD): string;

implementation

uses
  Consts, Registry,
  JclPeImage, JclResources, JclStrings, JclSysInfo, JclSysUtils;

const
  mapidll = 'mapi32.dll';
  MapiExportNames: array [0..11] of PChar =
    ('MAPIAddress',
     'MAPIDeleteMail',
     'MAPIDetails',
     'MAPIFindNext',
     'MAPIFreeBuffer',
     'MAPILogoff',
     'MAPILogon',
     'MAPIReadMail',
     'MAPIResolveName',
     'MAPISaveMail',
     'MAPISendDocuments',
     'MAPISendMail'
     );

//------------------------------------------------------------------------------
// MAPI Errors check
//------------------------------------------------------------------------------

function MapiCheck(const Res: DWORD; IgnoreUserAbort: Boolean): DWORD;
begin
  if (Res = SUCCESS_SUCCESS) or (IgnoreUserAbort and (Res = MAPI_E_USER_ABORT)) then
    Result := Res
  else
    raise EJclMapiError.CreateResRecFmt(@RsMapiError, [Res, MapiErrorMessage(Res)]);
end;

//------------------------------------------------------------------------------

function MapiErrorMessage(const ErrorCode: DWORD): string;
const
  ErrorMessages: array [1..26] of PResStringRec = (
    @RsMapiErrUSER_ABORT,
    @RsMapiErrFAILURE,
    @RsMapiErrLOGIN_FAILURE,
    @RsMapiErrDISK_FULL,
    @RsMapiErrINSUFFICIENT_MEMORY,
    @RsMapiErrACCESS_DENIED,
    nil,
    @RsMapiErrTOO_MANY_SESSIONS,
    @RsMapiErrTOO_MANY_FILES,
    @RsMapiErrTOO_MANY_RECIPIENTS,
    @RsMapiErrATTACHMENT_NOT_FOUND,
    @RsMapiErrATTACHMENT_OPEN_FAILURE,
    @RsMapiErrATTACHMENT_WRITE_FAILURE,
    @RsMapiErrUNKNOWN_RECIPIENT,
    @RsMapiErrBAD_RECIPTYPE,
    @RsMapiErrNO_MESSAGES,
    @RsMapiErrINVALID_MESSAGE,
    @RsMapiErrTEXT_TOO_LARGE,
    @RsMapiErrINVALID_SESSION,
    @RsMapiErrTYPE_NOT_SUPPORTED,
    @RsMapiErrAMBIGUOUS_RECIPIENT,
    @RsMapiErrMESSAGE_IN_USE,
    @RsMapiErrNETWORK_FAILURE,
    @RsMapiErrINVALID_EDITFIELDS,
    @RsMapiErrINVALID_RECIPS,
    @RsMapiErrNOT_SUPPORTED);
begin
  Result := '';
  if ErrorCode <= High(ErrorMessages) then
    Result := LoadResString(ErrorMessages[ErrorCode]);
end;

//==============================================================================
// TJclSimpleMapi
//==============================================================================

procedure TJclSimpleMapi.CheckListIndex(I: Integer);
begin
  if (I < 0) or (I >= ClientCount) then
    raise EJclMapiError.CreateResRecFmt(@SListIndexError, [I]);
end;

//------------------------------------------------------------------------------

function TJclSimpleMapi.ClientLibLoaded: Boolean;
begin
  Result := FClientLibHandle <> 0;
end;

//------------------------------------------------------------------------------

constructor TJclSimpleMapi.Create;
begin
  FTaskWindowsList := TList.Create;
  SetLength(FFunctions, 12);
  FFunctions[0] := @@FMapiAddress;
  FFunctions[1] := @@FMapiDeleteMail;
  FFunctions[2] := @@FMapiDetails;
  FFunctions[3] := @@FMapiFindNext;
  FFunctions[4] := @@FMapiFreeBuffer;
  FFunctions[5] := @@FMapiLogOff;
  FFunctions[6] := @@FMapiLogOn;
  FFunctions[7] := @@FMapiReadMail;
  FFunctions[8] := @@FMapiResolveName;
  FFunctions[9] := @@FMapiSaveMail;
  FFunctions[10] := @@FMapiSendDocuments;
  FFunctions[11] := @@FMapiSendMail;
  FDefaultClientIndex := -1;
  FSelectedClientType := ctAutomatic;
  FSelectedClientIndex := -1;
  ReadMapiSettings;
end;

//------------------------------------------------------------------------------

destructor TJclSimpleMapi.Destroy;
begin
  UnloadClientLib;
  FreeAndNil(FTaskWindowsList);
  inherited;
end;

//------------------------------------------------------------------------------

function TJclSimpleMapi.GetClientCount: Integer;
begin
  Result := Length(FClients);
end;

//------------------------------------------------------------------------------

function TJclSimpleMapi.GetClientLibName: string;
begin
  if UseMapi then
    Result := mapidll
  else
    Result := FClients[FSelectedClientIndex].ClientPath;
end;

//------------------------------------------------------------------------------

function TJclSimpleMapi.GetClients(Index: Integer): TJclMapiClient;
begin
  CheckListIndex(Index);
  Result := FClients[Index];
end;

//------------------------------------------------------------------------------

function TJclSimpleMapi.GetCurrentClientName: string;
begin
  if UseMapi then
    Result := 'MAPI'
  else
  if ClientCount > 0 then
    Result := Clients[SelectedClientIndex].ClientName
  else
    Result := '';
end;

//------------------------------------------------------------------------------

procedure TJclSimpleMapi.LoadClientLib;
var
  I: Integer;
  P: Pointer;
begin
  if ClientLibLoaded then
    Exit;
  FClientLibHandle := LoadLibrary(PChar(GetClientLibName));
  if FClientLibHandle = 0 then
    RaiseLastWin32Error;
  for I := 0 to Length(FFunctions) - 1 do
  begin
    P := GetProcAddress(FClientLibHandle, MapiExportNames[I]);
    if P = nil then
    begin
      UnloadClientLib;
      raise EJclMapiError.CreateResRecFmt(@RsMapiMissingExport, [MapiExportNames[I]]);
    end
    else
      FFunctions[I]^ := P;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclSimpleMapi.ReadMapiSettings;
const
  MessageSubsytemKey = 'SOFTWARE\Microsoft\Windows Messaging Subsystem';
  MailClientsKey = 'SOFTWARE\Clients\Mail';
var
  DefaultClient: string;
  SL: TStringList;
  I: Integer;

  function CheckValid(var Client: TJclMapiClient): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := Low(MapiExportNames) to High(MapiExportNames) do
      if not PeDoesExportFunction(Client.ClientPath, MapiExportNames[I], [scSimpleCompare]) then
      begin
        Result := False;
        Break;
      end;
    Client.Valid := Result;
  end;

begin
  FClients := nil;
  FDefaultClientIndex := -1;
  SL := TStringList.Create;
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(MessageSubsytemKey) then
    begin
      FMapiInstalled := ReadString('MAPIX') = '1';
      FSimpleMapiInstalled := ReadString('MAPI') = '1';
      FMapiVersion := ReadString('MAPIXVER');
      CloseKey;
    end;
    FAnyClientInstalled := FMapiInstalled;
    if OpenKeyReadOnly(MailClientsKey) then
    begin
      DefaultClient := ReadString('');
      GetKeyNames(SL);
      CloseKey;
      SetLength(FClients, SL.Count);
      for I := 0 to SL.Count - 1 do
      begin
        FClients[I].RegKeyName := SL[I];
        FClients[I].Valid := False;
        if OpenKeyReadOnly(MailClientsKey + '\' + SL[I]) then
        begin
          FClients[I].ClientName := ReadString('');
          FClients[I].ClientPath := ReadString('DLLPath');
          ExpandEnvironmentVar(FClients[I].ClientPath);
          if CheckValid(FClients[I]) then
            FAnyClientInstalled := True;
          CloseKey;
        end;
      end;
      FDefaultClientIndex := SL.IndexOf(DefaultClient);
      FSelectedClientIndex := FDefaultClientIndex;
    end;
    PeClearGlobalImage;
  finally
    Free;
    SL.Free;
  end;
end;

//------------------------------------------------------------------------------

function RestoreTaskWnds(Wnd: HWND; Data: TList): BOOL; stdcall;
begin
  if IsWindowVisible(Wnd) then
    EnableWindow(Wnd, False);
  Result := True;
end;

procedure TJclSimpleMapi.RestoreTaskWindowsState;
var
  I: Integer;
begin
  EnumThreadWindows(MainThreadID, @RestoreTaskWnds, Integer(FTaskWindowsList));
  for I := 0 to FTaskWindowsList.Count - 1 do
    EnableWindow(HWND(FTaskWindowsList[I]), True);
  FTaskWindowsList.Clear;
  SetFocus(FTaskActiveWindow);
  FTaskActiveWindow := 0;
end;

//------------------------------------------------------------------------------

function SaveTaskWnds(Wnd: HWND; Data: TList): BOOL; stdcall;
begin
  if IsWindowVisible(Wnd) and IsWindowEnabled(Wnd) then
  begin
    Data.Add(Pointer(Wnd));
    EnableWindow(Wnd, False);
  end;
  Result := True;
end;

procedure TJclSimpleMapi.SaveTaskWindowsState;
begin
  FTaskWindowsList.Clear;
  FTaskActiveWindow := GetFocus;
  EnumThreadWindows(MainThreadID, @SaveTaskWnds, Integer(FTaskWindowsList));
end;

//------------------------------------------------------------------------------

procedure TJclSimpleMapi.SetSelectedClientIndex(const Value: Integer);
begin
  CheckListIndex(Value);
  if FSelectedClientIndex <> Value then
  begin
    FSelectedClientIndex := Value;
    UnloadClientLib;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclSimpleMapi.SetSelectedClientType(const Value: TJclMapiClientType);
begin
  if FSelectedClientType <> Value then
  begin
    FSelectedClientType := Value;
    UnloadClientLib;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclSimpleMapi.UnloadClientLib;
var
  I: Integer;
begin
  if ClientLibLoaded then
  begin
    if Assigned(FBeforeUnloadClient) then
      FBeforeUnloadClient(Self);
    FreeLibrary(FClientLibHandle);
    FClientLibHandle := 0;
    for I := 0 to Length(FFunctions) - 1 do
      FFunctions[I]^ := nil;
  end;
end;

//------------------------------------------------------------------------------

function TJclSimpleMapi.UseMapi: Boolean;
begin
  case FSelectedClientType of
    ctAutomatic:
      UseMapi := FSimpleMapiInstalled;
    ctMapi:
      UseMapi := True;
    ctDirect:
      UseMapi := False;
  else
    UseMapi := True;
  end;
end;

//==============================================================================
// TJclEmail
//==============================================================================

procedure TJclEmail.SendMail(const Recipient, Name, Subject, Body: string;
  ParentWND: HWND);
var
  MapiMessage: TMapiMessage;
  Recip: TMapiRecipDesc;
begin
  FillChar(Recip, SizeOf(Recip), #0);
  Recip.ulRecipClass := MAPI_TO;
  Recip.lpszAddress := PChar(Recipient);
  Recip.lpszName := PChar(Name);
  FillChar(MapiMessage, SizeOf(MapiMessage), #0);
  MapiMessage.lpszSubject := PChar(Subject);
  MapiMessage.lpszNoteText := PChar(Body);
  MapiMessage.lpRecips := @Recip;
  MapiMessage.nRecipCount := 1;
  SaveTaskWindowsState;
  try
    MapiCheck(MapiSendMail(0, ParentWND, MapiMessage,
      MAPI_DIALOG or MAPI_LOGON_UI or MAPI_NEW_SESSION, 0), True);
  finally
    RestoreTaskWindowsState;
  end;
end;

//------------------------------------------------------------------------------

class procedure TJclEmail.SimpleSendMail(const Recipient, Name, Subject,
  Body: string; ParentWND: HWND);
begin
  with Create do
  try
    LoadClientLib;
    SendMail(Recipient, Name, Subject, Body, ParentWND);
  finally
    Free;
  end;
end;

end.
