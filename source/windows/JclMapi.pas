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
{ The Original Code is JclMapi.pas.                                            }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{ Last modified: January 18, 2001                                              }
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
  EJclMapiError = class (EJclError)
  private
    FErrorCode: DWORD;
  public
    property ErrorCode: DWORD read FErrorCode;
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
    procedure BeforeUnloadClientLib; dynamic;
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
// Simple email classes
//------------------------------------------------------------------------------

  TJclEmailRecipKind = (rkOriginator, rkTO, rkCC, rkBCC);

  TJclEmailRecip = class (TObject)
  private
    FAddress: string;
    FAddressType: string;
    FKind: TJclEmailRecipKind;
    FName: string;
  protected
    function SortingName: string;
  public
    function AddressAndName: string;
    property AddressType: string read FAddressType write FAddressType;
    property Address: string read FAddress write FAddress;
    property Kind: TJclEmailRecipKind read FKind write FKind;
    property Name: string read FName write FName;
  end;

  TJclEmailRecips = class (TObjectList)
  private
    FAddressesType: string;
    function GetItems(Index: Integer): TJclEmailRecip;
  public
    function Add(const Address: string;
      const Name: string{$IFDEF SUPPORTS_DEFAULTPARAMS} = '' {$ENDIF};
      const Kind: TJclEmailRecipKind{$IFDEF SUPPORTS_DEFAULTPARAMS} = rkTO {$ENDIF};
      const AddressType: string{$IFDEF SUPPORTS_DEFAULTPARAMS} = ''{$ENDIF}): Integer;
    procedure SortRecips;
    property AddressesType: string read FAddressesType write FAddressesType;
    property Items[Index: Integer]: TJclEmailRecip read GetItems; default;
  end;

  TJclEmailLogonOptions = set of (loLogonUI, loNewSession);
  TJclEmailReadOptions = set of (roUnreadOnly, roFifo, roPeek, roHeaderOnly,
    roAttachments);

  TJclEmailReadedMsg = record
    MessageType: string;
    DateReceived: TDateTime;
    ConversationID: string;
    Flags: FLAGS;
  end;

  TJclEmail = class (TJclSimpleMapi)
  private
    FAttachments: TStrings;
    FBody: string;
    FLogonOptions: TJclEmailLogonOptions;
    FReadedMsg: TJclEmailReadedMsg;
    FReadMailOptions: TJclEmailReadOptions;
    FRecipients: TJclEmailRecips;
    FSeedMessageID: string;
    FSessionHandle: THandle;
    FSubject: string;
    function GetUserLogged: Boolean;
  protected
    procedure BeforeUnloadClientLib; override;
    function GetParentWindow(Wnd: HWND): HWND;
    function LogonOptionsToFlags: DWORD;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LogOff(ParentWND: HWND{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF});
    procedure LogOn(const ProfileName, Password: string;
      ParentWND: HWND{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF});
    procedure MessageReport(Strings: TStrings; MaxWidth: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = 80{$ENDIF});
    function Send(ShowDialog: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = True {$ENDIF};
      ParentWND: HWND{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF}): Boolean;
    procedure SortAttachments;
    property Attachments: TStrings read FAttachments;
    property Body: string read FBody write FBody;
    function FindFirstMessage: Boolean;
    function FindNextMessage: Boolean;
    function Read: Boolean;
    property LogonOptions: TJclEmailLogonOptions read FLogonOptions write FLogonOptions;
    property ReadedMsg: TJclEmailReadedMsg read FReadedMsg;
    property ReadMailOptions: TJclEmailReadOptions read FReadMailOptions write FReadMailOptions;
    property Recipients: TJclEmailRecips read FRecipients;
    property SeedMessageID: string read FSeedMessageID write FSeedMessageID;
    property SessionHandle: THandle read FSessionHandle;
    property Subject: string read FSubject write FSubject;
    property UserLogged: Boolean read GetUserLogged;
  end;

//------------------------------------------------------------------------------
// Simple email send function
//------------------------------------------------------------------------------

function JclSimpleSendMail(const ARecipient, AName, ASubject, ABody: string;
  const AAttachment: TFileName{$IFDEF SUPPORTS_DEFAULTPARAMS} = '' {$ENDIF};
  ShowDialog: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = True {$ENDIF};
  ParentWND: HWND{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF}): Boolean;

//------------------------------------------------------------------------------
// MAPI Errors
//------------------------------------------------------------------------------

function MapiCheck(const Res: DWORD; IgnoreUserAbort: Boolean {$IFDEF SUPPORTS_DEFAULTPARAMS} = True {$ENDIF}): DWORD;

function MapiErrorMessage(const ErrorCode: DWORD): string;

implementation

uses
  Consts, Registry,
  JclLogic, JclResources, JclSscanf, JclStrings, JclSysInfo, JclSysUtils;

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
var
  Error: EJclMapiError;
begin
  if (Res = SUCCESS_SUCCESS) or (IgnoreUserAbort and (Res = MAPI_E_USER_ABORT)) then
    Result := Res
  else
  begin
    Error := EJclMapiError.CreateResRecFmt(@RsMapiError, [Res, MapiErrorMessage(Res)]);
    Error.FErrorCode := Res;
    raise Error;
  end;
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

procedure TJclSimpleMapi.BeforeUnloadClientLib;
begin
  if Assigned(FBeforeUnloadClient) then
    FBeforeUnloadClient(Self);
end;

//------------------------------------------------------------------------------

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
    BeforeUnloadClientLib;
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
  const Kind: TJclEmailRecipKind; const AddressType: string): Integer;
var
  Item: TJclEmailRecip;
begin
  Item := TJclEmailRecip.Create;
  try
    Item.Address := Trim(Address);
    Item.AddressType := AddressType;
    Item.Name := Name;
    Item.Kind := Kind;
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
  Result := Integer(R1.Kind) - Integer(R2.Kind);
  if Result = 0 then
    Result := AnsiCompareStr(R1.SortingName, R2.SortingName);
end;

procedure TJclEmailRecips.SortRecips;
begin
  Sort(EmailRecipsCompare);
end;

//==============================================================================
// TJclEmail
//==============================================================================

procedure TJclEmail.BeforeUnloadClientLib;
begin
  if UserLogged then
    LogOff(0);
  inherited;
end;

//------------------------------------------------------------------------------

procedure TJclEmail.Clear;
begin
  FAttachments.Clear;
  FBody := '';
  FSubject := '';
  FRecipients.Clear;
  FReadedMsg.MessageType := '';
  FReadedMsg.DateReceived := 0;
  FReadedMsg.ConversationID := '';
  FReadedMsg.Flags := 0;
end;

//------------------------------------------------------------------------------

constructor TJclEmail.Create;
begin
  inherited;
  FAttachments := TStringList.Create;
  FLogonOptions := [loLogonUI, loNewSession];
  FReadMailOptions := [roFifo, roPeek];
  FRecipients := TJclEmailRecips.Create(False);
  FRecipients.AddressesType := 'SMTP';
end;

//------------------------------------------------------------------------------

destructor TJclEmail.Destroy;
begin
  FreeAndNil(FAttachments);
  FreeAndNil(FRecipients);
  inherited;
end;

//------------------------------------------------------------------------------

function TJclEmail.FindFirstMessage: Boolean;
begin
  FSeedMessageID := '';
  Result := FindNextMessage;
end;

//------------------------------------------------------------------------------

function TJclEmail.FindNextMessage: Boolean;
var
  MsgID: array[0..512] of AnsiChar;
  Flags, Res: ULONG;
begin
  Result := False;
  if not UserLogged then
    Exit;
  Flags := MAPI_LONG_MSGID;
  if roFifo in FReadMailOptions then
    Inc(Flags, MAPI_GUARANTEE_FIFO);
  if roUnreadOnly in FReadMailOptions then
    Inc(Flags, MAPI_UNREAD_ONLY);
  Res := MapiFindNext(FSessionHandle, 0, nil, PChar(FSeedMessageID), Flags, 0, MsgId);
  Result := (Res = SUCCESS_SUCCESS);
  if Result then
    FSeedMessageID := MsgID
  else
  begin
    FSeedMessageID := '';
    if Res <> MAPI_E_NO_MESSAGES then
      MapiCheck(Res, True);
  end;
end;

//------------------------------------------------------------------------------

function TJclEmail.GetParentWindow(Wnd: HWND): HWND;
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
  if IsWindow(Wnd) then
    Result := Wnd
  else
  begin
    FoundWnd := 0;
    EnumThreadWindows(MainThreadID, @EnumThreadWndProc, Integer(@FoundWnd));
    Result := FoundWnd;
  end;
end;

//------------------------------------------------------------------------------

function TJclEmail.GetUserLogged: Boolean;
begin
  Result := (FSessionHandle <> 0);
end;

//------------------------------------------------------------------------------

procedure TJclEmail.LogOff(ParentWND: HWND);
begin
  if UserLogged then
  begin
    MapiCheck(MapiLogOff(FSessionHandle, GetParentWindow(ParentWND), 0, 0), True);
    FSessionHandle := 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclEmail.LogOn(const ProfileName, Password: string; ParentWND: HWND);
begin
  if not UserLogged then
  begin
    LoadClientLib;
    MapiCheck(MapiLogOn(GetParentWindow(ParentWND), PChar(ProfileName),
      PChar(Password), LogonOptionsToFlags, 0, @FSessionHandle), True);
  end; 
end;

//------------------------------------------------------------------------------

function TJclEmail.LogonOptionsToFlags: DWORD;
begin
  Result := 0;
  if loLogonUI in FLogonOptions then
    Inc(Result, MAPI_LOGON_UI);
  if loNewSession in FLogonOptions then
    Inc(Result, MAPI_NEW_SESSION);
end;

//------------------------------------------------------------------------------

procedure TJclEmail.MessageReport(Strings: TStrings; MaxWidth: Integer);
const
  NameDelimiter = ', ';
  KindNames: array [TJclEmailRecipKind] of string =
    (RsMapiMailORIG, RsMapiMailTO, RsMapiMailCC, RsMapiMailBCC);
var
  LabelsWidth: Integer;
  NamesList: array [TJclEmailRecipKind] of string;
  ReportKind: TJclEmailRecipKind;
  I: Integer;
  BreakStr, S: string;
begin
  LabelsWidth := Length(RsMapiMailSubject);
  for ReportKind := Low(ReportKind) to High(ReportKind) do
  begin
    NamesList[ReportKind] := '';
    LabelsWidth := Max(LabelsWidth, Length(KindNames[ReportKind]));
  end;
  BreakStr := AnsiCrLf + StringOfChar(' ', LabelsWidth + 2);
  for I := 0 to FRecipients.Count - 1 do
    with FRecipients[I] do
      NamesList[Kind] := NamesList[Kind] + AddressAndName + NameDelimiter;
  for ReportKind := Low(ReportKind) to High(ReportKind) do
    if NamesList[ReportKind] <> '' then
    begin
      S := StrPadRight(KindNames[ReportKind], LabelsWidth, AnsiSpace) + ': ' +
        Copy(NamesList[ReportKind], 1, Length(NamesList[ReportKind]) - Length(NameDelimiter));
      Strings.Add(WrapText(S, BreakStr, [AnsiTab, AnsiSpace], MaxWidth));
    end;
  S := RsMapiMailSubject + ': ' + Subject;
  Strings.Add(WrapText(S, BreakStr, [AnsiTab, AnsiSpace], MaxWidth));
  Strings.Add('');
  Strings.Add(WrapText(Body, BreakStr, [AnsiTab, AnsiSpace], MaxWidth));
  if FAttachments.Count > 0 then
  begin
    Strings.Add('');
    Strings.Add(RsMapiMailAttachments + ':');
    Strings.AddStrings(FAttachments);
  end;
end;

//------------------------------------------------------------------------------

function TJclEmail.Read: Boolean;
var
  Flags: ULONG;
  Msg: PMapiMessage;
  I: Integer;
  Recips: PMapiRecipDesc;
  Files: PMapiFileDesc;

  procedure AddRecip(RecipDesc: PMapiRecipDesc);
  var
    S: string;
    N: Integer;
    Kind: TJclEmailRecipKind;
  begin
    if RecipDesc = nil then
      Exit;
    Kind := rkOriginator;
    with RecipDesc^ do
    begin
      S := lpszAddress;
      N := Pos(':', S);
      if N = 0 then
        N := Length(S) + 1;
      if ulRecipClass in [MAPI_ORIG..MAPI_BCC] then
        Kind := TJclEmailRecipKind(ulRecipClass)
      else
        MapiCheck(MAPI_E_INVALID_MESSAGE, True);
      Recipients.Add(Copy(S, 1, N - 1), lpszName, Kind, Copy(S, N + 1, Length(S)));
    end;
  end;

  function MessageDateToDate(const S: string): TDateTime;
  var
    T: TSystemTime;
  begin
    FillChar(T, SizeOf(T), #0);
    with T do
    begin
      Sscanf(S, '%4h/%2h/%2h %2h:%2h', [@wYear, @wMonth, @wDay, @wHour, @wMinute]);
      Result := EncodeDate(wYear, wMonth, wDay) +
        EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
    end;
  end;

begin
  Result := False;
  if not UserLogged then
    Exit;
  Clear;
  Flags := 0;
  if roHeaderOnly in FReadMailOptions then
    Inc(Flags, MAPI_ENVELOPE_ONLY);
  if roPeek in FReadMailOptions then
    Inc(Flags, MAPI_PEEK);
  if not (roAttachments in FReadMailOptions) then
    Inc(Flags, MAPI_SUPPRESS_ATTACH);
  MapiCheck(MapiReadMail(SessionHandle, 0, PChar(FSeedMessageID), Flags, 0, Msg), True);
  try
    AddRecip(Msg^.lpOriginator);
    Recips := Msg^.lpRecips;
    for I := 0 to Msg^.nRecipCount - 1 do
    begin
      AddRecip(Recips);
      Inc(Recips);
    end;
    FSubject := Msg^.lpszSubject;
    FBody := AdjustLineBreaks(Msg^.lpszNoteText);
    Files := Msg^.lpFiles;
    for I := 0 to Msg^.nFileCount - 1 do
    begin
      Attachments.Add(Files^.lpszFileName);
      Inc(Files);
    end;
    FReadedMsg.MessageType := Msg^.lpszMessageType;
    FReadedMsg.DateReceived := MessageDateToDate(Msg^.lpszDateReceived);
    FReadedMsg.ConversationID := Msg^.lpszConversationID;
    FReadedMsg.Flags := Msg^.flFlags;
    Result := True;
  finally
    MapiFreeBuffer(Msg);
  end;
end;

//------------------------------------------------------------------------------

function TJclEmail.Send(ShowDialog: Boolean; ParentWND: HWND): Boolean;
const
  RecipClasses: array [TJclEmailRecipKind] of DWORD =
    (MAPI_ORIG, MAPI_TO, MAPI_CC, MAPI_BCC);
var
  AttachArray: array of TMapiFileDesc;
  RecipArray: array of TMapiRecipDesc;
  RealAdresses: array of string;
  MapiMessage: TMapiMessage;
  Flags: DWORD;
  I: Integer;
begin
  if not AnyClientInstalled then
    raise EJclMapiError.CreateResRec(@RsMapiMailNoClient);

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
    SetLength(RealAdresses, Recipients.Count);
    for I := 0 to Recipients.Count - 1 do
    begin
      FillChar(RecipArray[I], SizeOf(TMapiRecipDesc), #0);
      with RecipArray[I], Recipients[I] do
      begin
        ulRecipClass := RecipClasses[Kind];
        if Name = '' then // some clients requires Name item always filled
        begin
          if FAddress = '' then
            MapiCheck(MAPI_E_INVALID_RECIPS, False);
          lpszName := PChar(FAddress);
        end
        else
          lpszName := PChar(FName);
        if FAddressType <> '' then
          RealAdresses[I] := FAddressType + ':' + FAddress
        else
        if Recipients.AddressesType <> '' then
          RealAdresses[I] := Recipients.AddressesType + ':' + FAddress
        else
          RealAdresses[I] := FAddress;
        lpszAddress := PCharOrNil(RealAdresses[I]);
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
  Flags := LogonOptionsToFlags;
  if ShowDialog then
  begin
    Flags := Flags or MAPI_DIALOG;
    ParentWND := GetParentWindow(ParentWnd);
  end;
  Result := (MapiCheck(MapiSendMail(FSessionHandle, ParentWND, MapiMessage, Flags, 0), True) = SUCCESS_SUCCESS);
end;

//------------------------------------------------------------------------------

procedure TJclEmail.SortAttachments;
begin
  TStringList(FAttachments).Sort;
end;

//==============================================================================
// Simple email send function
//==============================================================================

function JclSimpleSendMail(const ARecipient, AName, ASubject, ABody: string;
  const AAttachment: TFileName; ShowDialog: Boolean; ParentWND: HWND): Boolean;
begin
  with TJclEmail.Create do
  try
    Recipients.Add(ARecipient, AName, rkTO, '');
    Subject := ASubject;
    Body := ABody;
    if AAttachment <> '' then
      Attachments.Add(AAttachment);
    Result := Send(ShowDialog, ParentWND);
  finally
    Free;
  end;
end;

//------------------------------------------------------------------------------

end.

