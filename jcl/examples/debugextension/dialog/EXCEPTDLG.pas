{******************************************************************************}
{                                                                              }
{ Project JEDI Code Library (JCL) extension                                    }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.0 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is ExceptDlg.pas.                                          }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ Sample Application exception dialog replacement                              }
{                                                                              }
{ Last modified: February 20, 2001                                             }
{                                                                              }
{******************************************************************************}

unit ExceptDlg;

interface

{$I JEDI.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

const
  UM_CREATEDETAILS = WM_USER + $100;

type
  TExceptionDialog = class(TForm)
    OkBtn: TButton;
    DetailsMemo: TMemo;
    DetailsBtn: TButton;
    Bevel1: TBevel;
    TextLabel: TMemo;
    SupportBtn: TButton;
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DetailsBtnClick(Sender: TObject);
    procedure SupportBtnClick(Sender: TObject);
    procedure FormConstrainedResize(Sender: TObject; var MinWidth,
      MinHeight, MaxWidth, MaxHeight: Integer);
  private
    FDetailsVisible: Boolean;
    FIsMainThead: Boolean;
    FLastActiveControl: TWinControl;
    FNonDetailsHeight: Integer;
    FFullHeight: Integer;
    FSupportAddress: string;
    procedure CreateDetails;
    procedure SetDetailsVisible(const Value: Boolean);
    procedure UMCreateDetails(var Message: TMessage); message UM_CREATEDETAILS;
  public
    procedure CreateSupportMessage;
    class procedure ExceptionHandler(Sender: TObject; E: Exception);
    class procedure ShowException(E: Exception);
    property DetailsVisible: Boolean read FDetailsVisible write SetDetailsVisible;
  end;

var
  ExceptionDialog: TExceptionDialog;

implementation

{$R *.DFM}

uses
  JclBase, JclDebug, JclFileUtils, JclMapi, JclStrings, JclSysInfo, JclSysUtils;

resourcestring
  RsAppError = '%s - application error';
  RsExceptionClass = 'Exception class: %s';
  RsExceptionAddr = 'Exception address: %p';
  RsStackList = 'Stack list, generated %s';
  RsModulesList = 'List of loaded modules:';
  RsOSVersion = 'OS: Win%s, Version: %d.%d, Build: %x, "%s"';
  RsProcessor = 'Processor: %s %s %d %s%s';
  RsScreenRes = 'Screen: %dx%d pixels, %d bpp';
  RsActiveControl = 'ActiveControl:';
  RsMainThread = 'IsMainThread: %s';

  RsSupportAddress = ''; // Put support email address here.

//==============================================================================
// Helper routines
//==============================================================================

function GetBPP: Integer;
var
  DC: HDC;
begin
  DC := GetDC(0);
  Result := GetDeviceCaps(DC, BITSPIXEL) * GetDeviceCaps(DC, PLANES);
  ReleaseDC(0, DC);
end;

procedure SaveStringToFile(const S: string; const FileName: TFileName);
var
  FileHandle: THandle;
begin
  FileHandle := FileCreate(FileName);
  if FileHandle = INVALID_HANDLE_VALUE then
    RaiseLastWin32Error;
  try
    FileWrite(FileHandle, Pointer(S)^, Length(S));
  finally
    FileClose(FileHandle);
  end;
end;

type
  PTaskWindowsList = ^TTaskWindowsList;
  TTaskWindowsList = array of HWND;

procedure RestoreTaskWindowsList(const List: TTaskWindowsList);
var
  I: Integer;

  function RestoreTaskWnds(Wnd: HWND; List: TTaskWindowsList): BOOL; stdcall;
  var
    I: Integer;
    EnableIt: Boolean;
  begin
    if IsWindowVisible(Wnd) then
    begin
      EnableIt := False;
      for I := 1 to Length(List) - 1 do
        if List[I] = Wnd then
        begin
          EnableIt := True;
          Break;
        end;
      EnableWindow(Wnd, EnableIt);
    end;
    Result := True;
  end;

begin
  EnumThreadWindows(MainThreadID, @RestoreTaskWnds, Integer(List));
  for I := 0 to Length(List) - 1 do
    EnableWindow(List[I], True);
  SetFocus(List[0]);
end;

function SaveTaskWindowsList: TTaskWindowsList;

  function SaveTaskWnds(Wnd: HWND; Data: PTaskWindowsList): BOOL; stdcall;
  var
    C: Integer;
  begin
    if IsWindowVisible(Wnd) and IsWindowEnabled(Wnd) then
    begin
      C := Length(Data^);
      SetLength(Data^, C + 1);
      Data^[C] := Wnd;
    end;
    Result := True;
  end;

begin
  SetLength(Result, 1);
  Result[0] := GetFocus;
  EnumThreadWindows(MainThreadID, @SaveTaskWnds, Integer(@Result));
end;

//==============================================================================
// Exception dialog
//==============================================================================

var
  ExceptionShowing: Boolean;

{ TExceptionDialog }

procedure TExceptionDialog.CreateDetails;
const
  OSNames: array [TWindowsVersion] of string =
    ('?', '95', '95OSR2', '98', '98SE', 'ME', 'NT3', 'NT4', '2000', 'XP');
  MMXText: array[Boolean] of PChar = ('', 'MMX');
  FDIVText: array[Boolean] of PChar = (' [FDIV Bug]', '');
var
  SL: TStringList;
  I: Integer;
  ModuleName: TFileName;
  CpuInfo: TCpuInfo;
  C: TWinControl;
begin
  Screen.Cursor := crHourGlass;
  DetailsMemo.Lines.BeginUpdate;
  SL := TStringList.Create;
  try
    // Stack list
    with JclLastExceptStackList do
    begin
      DetailsMemo.Lines.Add(Format(RsStackList, [DateTimeToStr(TimeStamp)]));
      AddToStrings(DetailsMemo.Lines);
    end;
    DetailsMemo.Lines.Add('');
    // Active controls
    DetailsMemo.Lines.Add(RsActiveControl);
    C := FLastActiveControl;
    while C <> nil do
    begin
      DetailsMemo.Lines.Add(Format('%s "%s"', [C.ClassName, C.Name]));
      C := C.Parent;
    end;
    DetailsMemo.Lines.Add('');
    // OS information
    DetailsMemo.Lines.Add(Format(RsOSVersion, [OSNames[GetWindowsVersion],
      Win32MajorVersion, Win32MinorVersion, Win32BuildNumber, Win32CSDVersion]));
    GetCpuInfo(CpuInfo);
    with CpuInfo do
      DetailsMemo.Lines.Add(Format(RsProcessor, [Manufacturer, CpuName,
        RoundFrequency(FrequencyInfo.NormFreq),
        MMXText[MMX], FDIVText[IsFDIVOK]]));
    DetailsMemo.Lines.Add(Format(RsScreenRes, [Screen.Width, Screen.Height, GetBPP]));
    DetailsMemo.Lines.Add('');
    // Modules list
    if LoadedModulesList(SL, GetCurrentProcessId) then
    begin
      DetailsMemo.Lines.Add(RsModulesList);
      SL.Sort;
      for I := 0 to SL.Count - 1 do
      begin
        ModuleName := SL[I];
        DetailsMemo.Lines.Add(Format('[%.8x] %s', [DWORD(SL.Objects[I]), ModuleName]));
        if VersionResourceAvailable(ModuleName) then
          with TJclFileVersionInfo.Create(ModuleName) do
          try
            DetailsMemo.Lines.Add(StrRepeat(' ', 11) + BinFileVersion + ' - ' + FileVersion);
            if FileDescription <> '' then
              DetailsMemo.Lines.Add(StrRepeat(' ', 11) + FileDescription);
          finally
            Free;
          end;
      end;
    end;

    DetailsMemo.SelStart := 0;
    SendMessage(DetailsMemo.Handle, EM_SCROLLCARET, 0, 0);
    SupportBtn.Enabled := True;
  finally
    SL.Free;
    DetailsMemo.Lines.EndUpdate;
    OkBtn.Enabled := True;
    DetailsBtn.Enabled := True;
    OkBtn.SetFocus;
    Screen.Cursor := crDefault;
  end;
end;

procedure TExceptionDialog.CreateSupportMessage;
var
  SupportMessage: string;
  BugReportFileName: TFileName;
  TaskWindowsList: TTaskWindowsList;
begin
  BugReportFileName := PathAddSeparator(ExtractFilePath(Application.ExeName)) + 'BugReport.txt';
  SupportMessage := StrEnsureSuffix(AnsiCrLf, TextLabel.Text) + DetailsMemo.Text;
  SaveStringToFile(SupportMessage, BugReportFileName);
  // You can specify another message body in SupportMessage variable here
  
  TaskWindowsList := SaveTaskWindowsList;
  try
    JclSimpleSendMail(FSupportAddress, '', Application.Title,
      SupportMessage, BugReportFileName, True, Handle);
  finally
    RestoreTaskWindowsList(TaskWindowsList);
    DeleteFile(BugReportFileName);
  end;
end;

procedure TExceptionDialog.DetailsBtnClick(Sender: TObject);
begin
  DetailsVisible := not DetailsVisible;
end;

class procedure TExceptionDialog.ExceptionHandler(Sender: TObject; E: Exception);
begin
  if ExceptionShowing then
    Application.ShowException(E)
  else
  begin
    ExceptionShowing := True;
    try
      ShowException(E);
    finally
      ExceptionShowing := False;
    end;
  end;
end;

procedure TExceptionDialog.FormConstrainedResize(Sender: TObject;
  var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer);
begin
  if FDetailsVisible then
  begin
    MinHeight := FNonDetailsHeight + 100;
    MaxHeight := Screen.Height;
  end
  else
  begin
    MinHeight := FNonDetailsHeight;
    MaxHeight := FNonDetailsHeight;
  end;
end;

procedure TExceptionDialog.FormCreate(Sender: TObject);
begin
  FFullHeight := ClientHeight;
  DetailsVisible := False;
  OnConstrainedResize := FormConstrainedResize;
  Caption := Format(RsAppError, [Application.Title]);
  FSupportAddress := RsSupportAddress;
  SupportBtn.Visible := FSupportAddress <> '';
end;

procedure TExceptionDialog.FormPaint(Sender: TObject);
begin
  DrawIcon(Canvas.Handle, TextLabel.Left - GetSystemMetrics(SM_CXICON) - 15,
    TextLabel.Top, LoadIcon(0, IDI_ERROR));
end;

procedure TExceptionDialog.FormShow(Sender: TObject);
begin
  MessageBeep(MB_ICONERROR);
  if FIsMainThead and (GetWindowThreadProcessId(Handle, nil) = MainThreadID) then
    PostMessage(Handle, UM_CREATEDETAILS, 0, 0)
  else
    CreateDetails;
end;

procedure TExceptionDialog.SetDetailsVisible(const Value: Boolean);
var
  DetailsCaption: string;
begin
  FDetailsVisible := Value;
  DetailsCaption := Trim(StrRemoveChars(DetailsBtn.Caption, ['<', '>']));
  if Value then
  begin
    DetailsCaption := '<< ' + DetailsCaption;
    ClientHeight := FFullHeight;
    DetailsMemo.Height := FFullHeight - DetailsMemo.Top - 3;
  end
  else
  begin
    FFullHeight := ClientHeight;
    DetailsCaption := DetailsCaption + ' >>';
    if FNonDetailsHeight = 0 then
    begin
      ClientHeight := Bevel1.Top;
      FNonDetailsHeight := Height;
    end
    else
      Height := FNonDetailsHeight;
  end;
  DetailsBtn.Caption := DetailsCaption;
  DetailsMemo.Enabled := Value;
end;

class procedure TExceptionDialog.ShowException(E: Exception);
begin
  if ExceptionDialog = nil then
    ExceptionDialog := Create(Application);
  try
    with ExceptionDialog do
    begin
      FIsMainThead := (GetCurrentThreadId = MainThreadID);
      FLastActiveControl := Screen.ActiveControl;
      TextLabel.Text := AdjustLineBreaks(StrEnsureSuffix('.', E.Message));
      DetailsMemo.Lines.Add(Format(RsExceptionClass, [E.ClassName]));
      DetailsMemo.Lines.Add(Format(RsExceptionAddr, [ExceptAddr]));
      DetailsMemo.Lines.Add(Format(RsMainThread, [BooleanToStr(FIsMainThead)]));
      DetailsMemo.Lines.Add('');
      ShowModal;
    end;
  finally
    FreeAndNil(ExceptionDialog);
  end;
end;

procedure TExceptionDialog.SupportBtnClick(Sender: TObject);
begin
  CreateSupportMessage;
end;

procedure TExceptionDialog.UMCreateDetails(var Message: TMessage);
begin
  Update;
  CreateDetails;
end;

//==============================================================================
// Exception handler initialization code
//==============================================================================

procedure InitializeHandler;
begin
  JclStackTrackingOptions := JclStackTrackingOptions + [stRawMode]; 
  JclStartExceptionTracking;
  Application.OnException := TExceptionDialog.ExceptionHandler;
end;

initialization
  InitializeHandler;

end.
