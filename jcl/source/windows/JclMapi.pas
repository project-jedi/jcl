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
{ Last modified: November 28, 2000                                             }
{                                                                              }
{******************************************************************************}

unit JclMapi;

{$I JCL.INC}

interface

uses
  Windows, Classes, Mapi, SysUtils,
  {$IFDEF DELPHI5_UP}
  Contnrs,
  {$ENDIF DELPHI5_UP}
  JclBase;

type
  EJclMapiError = EJclError;

//------------------------------------------------------------------------------
// Task windows store/restore
//------------------------------------------------------------------------------

  TJclMapiTaskWindows = class (TObject)
  private
    FTaskActiveWindow: HWND;
    FTaskWindowsList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RestoreTaskWindowsState;
    procedure SaveTaskWindowsState;
  end;

//------------------------------------------------------------------------------
// Simple MAPI interface
//------------------------------------------------------------------------------

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

  TJclEmailRecipKind = (rkTO, rkCC, rkBCC);

  TJclEmailRecip = class (TObject)
  private
    FAddress: string;
    FKind: TJclEmailRecipKind;
    FName: string;
  protected
    function SortingName: string;
  public
    function AddressAndName: string;
    property Address: string read FAddress write FAddress;
    property Kind: TJclEmailRecipKind read FKind write FKind;
    property Name: string read FName write FName;
  end;

  TJclEmailRecips = class (TObjectList)
  private
    function GetItems(Index: Integer): TJclEmailRecip;
  public
    function Add(const Address: string;
      const Name: string{$IFDEF SUPPORTS_DEFAULTPARAMS} = '' {$ENDIF};
      const Kind: TJclEmailRecipKind{$IFDEF SUPPORTS_DEFAULTPARAMS} = rkTO {$ENDIF}): Integer;
    procedure SortRecips;
    property Items[Index: Integer]: TJclEmailRecip read GetItems; default;
  end;

  TJclEmail = class (TJclSimpleMapi)
  private
    FAttachments: TStrings;
    FBody: string;
    FSubject: string;
    FRecipients: TJclEmailRecips;
  protected
    function FindParentWindow: HWND;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Send(ShowDialog: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = True {$ENDIF};
      ParentWND: HWND{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF}): Boolean;
    class function SimpleSendMail(const ARecipient, AName, ASubject, ABody: string;
      ShowDialog: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = True {$ENDIF};
      ParentWND: HWND{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF}): Boolean;
    procedure SortAttachments;
    property Attachments: TStrings read FAttachments write FAttachments;
    property Body: string read FBody write FBody;
    property Recipients: TJclEmailRecips read FRecipients;
    property Subject: string read FSubject write FSubject;
  end;

//------------------------------------------------------------------------------
// MAPI Errors
//------------------------------------------------------------------------------

function MapiCheck(const Res: DWORD; IgnoreUserAbort: Boolean {$IFDEF SUPPORTS_DEFAULTPARAMS} = True {$ENDIF}): DWORD;

function MapiErrorMessage(const ErrorCode: DWORD): string;

implementation

uses
  Consts, Registry,
  JclResources, JclStrings, JclSysInfo, JclSysUtils;

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
// TJclMapiTaskWindows
//==============================================================================

constructor TJclMapiTaskWindows.Create;
begin
  FTaskWindowsList := TList.Create;
end;

//------------------------------------------------------------------------------

destructor TJclMapiTaskWindows.Destroy;
begin
  FreeAndNil(FTaskWindowsList);
  inherited;
end;

//------------------------------------------------------------------------------

function RestoreTaskWnds(Wnd: HWND; Data: TList): BOOL; stdcall;
begin
  if IsWindowVisible(Wnd) then
    EnableWindow(Wnd, False);
  Result := True;
end;

procedure TJclMapiTaskWindows.RestoreTaskWindowsState;
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

procedure TJclMapiTaskWindows.SaveTaskWindowsState;
begin
  FTaskWindowsList.Clear;
  FTaskActiveWindow := GetFocus;
  EnumThreadWindows(MainThreadID, @SaveTaskWnds, Integer(FTaskWindowsList));
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
  SetLength(FFunctions, Length(MapiExportNames));
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
    LibHandle: THandle;
  begin
    LibHandle := LoadLibraryEx(PChar(Client.ClientPath), 0, DONT_RESOLVE_DLL_REFERENCES);
    Result := (LibHandle <> 0);
    if Result then
    begin
      for I := Low(MapiExportNames) to High(MapiExportNames) do
        if GetProcAddress(LibHandle, MapiExportNames[I]) = nil then
        begin
          Result := False;
          Break;
        end;
      FreeLibrary(LibHandle);
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
  finally
    Free;
    SL.Free;
  end;
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
// TJclEmailRecip
//==============================================================================

function TJclEmailRecip.AddressAndName: string;
var
  N: string;
begin
  if Name = '' then
    N := Address
  else
    N := Name;
  Result := Format('"%s" <%s>', [N, Address]);
end;

//------------------------------------------------------------------------------

function TJclEmailRecip.SortingName: string;
begin
  if FName = '' then
    Result := FAddress
  else
    Result := FName;
end;

//==============================================================================
// TJclEmailRecips
//==============================================================================

function TJclEmailRecips.Add(const Address, Name: string;
  const Kind: TJclEmailRecipKind): Integer;
var
  Item: TJclEmailRecip;
begin
  Item := TJclEmailRecip.Create;
  try
    Item.FAddress := Trim(Address);
    Item.FName := Name;
    Item.FKind := Kind;
    Result := inherited Add(Item);
  except
    Item.Free;
    raise;
  end;
end;

//------------------------------------------------------------------------------

function TJclEmailRecips.GetItems(Index: Integer): TJclEmailRecip;
begin
  Result := TJclEmailRecip(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

function EmailRecipsCompare(Item1, Item2: Pointer): Integer;
var
  R1, R2: TJclEmailRecip;
begin
  R1 := TJclEmailRecip(Item1);
  R2 := TJclEmailRecip(Item2);
  if R1.Kind < R2.Kind then
    Result := -1
  else
  if R1.Kind > R2.Kind then
    Result := 1
  else
    Result := AnsiCompareStr(R1.SortingName, R2.SortingName);
end;

procedure TJclEmailRecips.SortRecips;
begin
  Sort(EmailRecipsCompare);
end;

//==============================================================================
// TJclEmail
//==============================================================================

procedure TJclEmail.Clear;
begin
  FAttachments.Clear;
  FBody := '';
  FSubject := '';
  FRecipients.Clear;
end;

//------------------------------------------------------------------------------

constructor TJclEmail.Create;
begin
  inherited;
  FAttachments := TStringList.Create;
  FRecipients := TJclEmailRecips.Create;
end;

//------------------------------------------------------------------------------

destructor TJclEmail.Destroy;
begin
  FreeAndNil(FAttachments);
  FreeAndNil(FRecipients);
  inherited;
end;

//------------------------------------------------------------------------------

function TJclEmail.FindParentWindow: HWND;
var
  FoundWnd: HWND;

  function EnumThreadWndProc(Wnd: HWND; Param: LPARAM): Boolean; stdcall;
  begin
    if IsWindowVisible(Wnd) and (GetWindowLong(Wnd, GWL_STYLE) and WS_POPUP <> 0) then
    begin
      PDWORD(Param)^ := Wnd;
      Result := False;
    end else
      Result := True;
  end;

begin
  FoundWnd := 0;
  EnumThreadWindows(MainThreadID, @EnumThreadWndProc, Integer(@FoundWnd));
  Result := FoundWnd;
end;

//------------------------------------------------------------------------------

function TJclEmail.Send(ShowDialog: Boolean; ParentWND: HWND): Boolean;
const
  RecipClasses: array [TJclEmailRecipKind] of DWORD =
    (MAPI_TO, MAPI_CC, MAPI_BCC);
var
  AttachArray: array of TMapiFileDesc;
  RecipArray: array of TMapiRecipDesc;
  MapiMessage: TMapiMessage;
  Flags, Res: DWORD;
  I: Integer;
  TaskWindows: TJclMapiTaskWindows;
begin
  if FAttachments.Count > 0 then
  begin
    SetLength(AttachArray, FAttachments.Count);
    for I := 0 to FAttachments.Count - 1 do
    begin
      if not FileExists(FAttachments[I]) then
        MapiCheck(MAPI_E_ATTACHMENT_NOT_FOUND, False);
      FAttachments[I] := ExpandFileName(FAttachments[I]);
      FillChar(AttachArray[I], SizeOf(TMapiFileDesc), #0);
      AttachArray[I].nPosition := DWORD(-1);
      AttachArray[I].lpszFileName := nil;
      AttachArray[I].lpszPathName := PChar(FAttachments[I]);
    end;
  end else
    AttachArray := nil;

  if Recipients.Count > 0 then
  begin
    SetLength(RecipArray, Recipients.Count);
    for I := 0 to Recipients.Count - 1 do
    begin
      FillChar(RecipArray[I], SizeOf(TMapiRecipDesc), #0);
      with RecipArray[I], Recipients[I] do
      begin
        ulRecipClass := RecipClasses[Kind];
        if Name = '' then // some clients requires Name item always filled
          lpszName := PChar(FAddress)
        else
          lpszName := PChar(FName);
        lpszAddress := PChar(FAddress);
      end;
    end;
  end else
  begin
    if ShowDialog then
      RecipArray := nil
    else
      MapiCheck(MAPI_E_INVALID_RECIPS, False);
  end;    

  LoadClientLib;

  FillChar(MapiMessage, SizeOf(MapiMessage), #0);
  MapiMessage.lpszSubject := PChar(FSubject);
  MapiMessage.lpszNoteText := PChar(FBody);
  MapiMessage.lpRecips := PMapiRecipDesc(RecipArray);
  MapiMessage.nRecipCount := Length(RecipArray);
  MapiMessage.lpFiles := PMapiFileDesc(AttachArray);
  MapiMessage.nFileCount := Length(AttachArray);
  TaskWindows := TJclMapiTaskWindows.Create;
  try
    Flags := MAPI_LOGON_UI or MAPI_NEW_SESSION;
    if ShowDialog then
    begin
      Flags := Flags or MAPI_DIALOG;
      if ParentWND = 0 then
        ParentWND := FindParentWindow;
      TaskWindows.SaveTaskWindowsState;
    end;
    Res := MapiCheck(MapiSendMail(0, ParentWND, MapiMessage, Flags, 0), True);
    Result := (Res = SUCCESS_SUCCESS);
  finally
    if ShowDialog then
      TaskWindows.RestoreTaskWindowsState;
    TaskWindows.Free;
  end;
end;

//------------------------------------------------------------------------------

class function TJclEmail.SimpleSendMail(const ARecipient, AName, ASubject,
  ABody: string; ShowDialog: Boolean; ParentWND: HWND): Boolean;
begin
  with Create do
  try
    Recipients.Add(ARecipient, AName, rkTO);
    Subject := ASubject;
    Body := ABody;
    Result := Send(ShowDialog, ParentWND);
  finally
    Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclEmail.SortAttachments;
begin
  TStringList(FAttachments).Sort;
end;

end.
