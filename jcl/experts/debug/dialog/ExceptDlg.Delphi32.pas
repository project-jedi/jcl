{****************************************************************************}
{                                                                            }
{ Project JEDI Code Library (JCL)                                            }
{                                                                            }
{ The contents of this file are subject to the Mozilla Public License        }
{ Version 1.1 (the "License"); you may not use this file except in           }
{ compliance with the License. You may obtain a copy of the License at       }
{ http://www.mozilla.org/MPL/                                                }
{                                                                            }
{ Software distributed under the License is distributed on an "AS IS" basis, }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   }
{ for the specific language governing rights and limitations under the       }
{ License.                                                                   }
{                                                                            }
{ The Original Code is ExceptDlg.pas.                                        }
{                                                                            }
{ The Initial Developer of the Original Code is Petr Vones.                  }
{ Portions created by Petr Vones are Copyright (C) of Petr Vones.            }
{                                                                            }
{****************************************************************************}
{                                                                            }
{ Last modified: $Date: 2006-05-18 18:04:47 +0200 (jeu., 18 mai 2006) $      }
{                                                                            }
{****************************************************************************}

unit %MODULENAME%;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  JclSysUtils,%if SendEMail JclMapi,%endif JclDebug;

const
  UM_CREATEDETAILS = WM_USER + $100;

type
  T%FORMNAME% = class(%ANCESTORNAME%)
%if SendEMail    SendBtn: TButton;%endif
    TextLabel: TMemo;
    OkBtn: TButton;
    DetailsBtn: TButton;
    BevelDetails: TBevel;
    DetailsMemo: TMemo;
%if SendEMail    procedure SendBtnClick(Sender: TObject);%endif
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DetailsBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    private
    FDetailsVisible: Boolean;
    FIsMainThead: Boolean;
%if ActiveControls    FLastActiveControl: TWinControl;%endif
    FNonDetailsHeight: Integer;
    FFullHeight: Integer;
%if LogFile    FSimpleLog: TJclSimpleLog;
    procedure ReportToLog;%endif
    function GetReportAsText: string;
    procedure SetDetailsVisible(const Value: Boolean);
    procedure UMCreateDetails(var Message: TMessage); message UM_CREATEDETAILS;
  protected
    procedure AfterCreateDetails; dynamic;
    procedure BeforeCreateDetails; dynamic;
    procedure CreateDetails; dynamic;
    procedure CreateReport;
    function ReportMaxColumns: Integer; virtual;
    function ReportNewBlockDelimiterChar: Char; virtual;
    procedure NextDetailBlock;
    procedure UpdateTextLabelScrollbars;
  public
    procedure CopyReportToClipboard;
    class procedure ExceptionHandler(Sender: TObject; E: Exception);
    class procedure ExceptionThreadHandler(Thread: TJclDebugThread);
    class procedure ShowException(E: Exception; Thread: TJclDebugThread);
    property DetailsVisible: Boolean read FDetailsVisible
      write SetDetailsVisible;
    property ReportAsText: string read GetReportAsText;
%if LogFile    property SimpleLog: TJclSimpleLog read FSimpleLog;%endif
  end;

  T%FORMNAME%Class = class of T%FORMNAME%;

var
  %FORMNAME%Class: T%FORMNAME%Class = T%FORMNAME%;

implementation

{$R *.dfm}

uses
  ClipBrd, Math,
  JclBase, JclFileUtils, JclHookExcept, JclPeImage, JclStrings, JclSysInfo, JclWin32;

resourcestring
  RsAppError = '%s - application error';
  RsExceptionClass = 'Exception class: %s';
  RsExceptionAddr = 'Exception address: %p';
  RsStackList = 'Stack list, generated %s';
  RsModulesList = 'List of loaded modules:';
  RsOSVersion = 'System   : %s %s, Version: %d.%d, Build: %x, "%s"';
  RsProcessor = 'Processor: %s, %s, %d MHz';
  RsMemory = 'Memory: %d; free %d';
  RsScreenRes = 'Display  : %dx%d pixels, %d bpp';
  RsActiveControl = 'Active Controls hierarchy:';
  RsThread = 'Thread: %s';
  RsMissingVersionInfo = '(no version info)';

var
  %FORMNAME%: T%FORMNAME%;

//============================================================================
// Helper routines
//============================================================================

// SortModulesListByAddressCompare
// sorts module by address
function SortModulesListByAddressCompare(List: TStringList;
  Index1, Index2: Integer): Integer;
var
  Addr1, Addr2: Cardinal;
begin
  Addr1 := Cardinal(List.Objects[Index1]);
  Addr2 := Cardinal(List.Objects[Index2]);
  if Addr1 > Addr2 then
    Result := 1
  else if Addr1 < Addr2 then
    Result := -1
  else
    Result := 0;
end;

//============================================================================
// TApplication.HandleException method code hooking for exceptions from DLLs
//============================================================================

// We need to catch the last line of TApplication.HandleException method:
// [...]
//   end else
//    SysUtils.ShowException(ExceptObject, ExceptAddr);
// end;

procedure HookShowException(ExceptObject: TObject; ExceptAddr: Pointer);
begin
  if JclValidateModuleAddress(ExceptAddr)
    and (ExceptObject.InstanceSize >= Exception.InstanceSize) then
    T%FORMNAME%.ExceptionHandler(nil, Exception(ExceptObject))
  else
    SysUtils.ShowException(ExceptObject, ExceptAddr);
end;

//----------------------------------------------------------------------------

function HookTApplicationHandleException: Boolean;
const
  CallOffset      = $86;
  CallOffsetDebug = $94;
type
  PCALLInstruction = ^TCALLInstruction;
  TCALLInstruction = packed record
    Call: Byte;
    Address: Integer;
  end;
var
  TApplicationHandleExceptionAddr, SysUtilsShowExceptionAddr: Pointer;
  CALLInstruction: TCALLInstruction;
  CallAddress: Pointer;
  WrittenBytes: Cardinal;

  function CheckAddressForOffset(Offset: Cardinal): Boolean;
  begin
    try
      CallAddress := Pointer(Cardinal(TApplicationHandleExceptionAddr) + Offset);
      CALLInstruction.Call := $E8;
      Result := PCALLInstruction(CallAddress)^.Call = CALLInstruction.Call;
      if Result then
      begin
        if IsCompiledWithPackages then
          Result := PeMapImgResolvePackageThunk(Pointer(Integer(CallAddress) + Integer(PCALLInstruction(CallAddress)^.Address) + SizeOf(CALLInstruction))) = SysUtilsShowExceptionAddr
        else
          Result := PCALLInstruction(CallAddress)^.Address = Integer(SysUtilsShowExceptionAddr) - Integer(CallAddress) - SizeOf(CALLInstruction);
      end;
    except
      Result := False;
    end;
  end;

begin
  TApplicationHandleExceptionAddr := PeMapImgResolvePackageThunk(@TApplication.HandleException);
  SysUtilsShowExceptionAddr := PeMapImgResolvePackageThunk(@SysUtils.ShowException);
  Result := CheckAddressForOffset(CallOffset) or CheckAddressForOffset(CallOffsetDebug);
  if Result then
  begin
    CALLInstruction.Address := Integer(@HookShowException) - Integer(CallAddress) - SizeOf(CALLInstruction);
    Result := WriteProtectedMemory(CallAddress, @CallInstruction, SizeOf(CallInstruction), WrittenBytes);
  end;
end;

//============================================================================
// Exception dialog with Send
//============================================================================

var
  ExceptionShowing: Boolean;

//=== { T%FORMNAME% } ===============================================

procedure T%FORMNAME%.AfterCreateDetails;
begin
%if SendEMail  SendBtn.Enabled := True;%endif
end;

//----------------------------------------------------------------------------

procedure T%FORMNAME%.BeforeCreateDetails;
begin
%if SendEMail  SendBtn.Enabled := False;%endif
end;

//----------------------------------------------------------------------------

function T%FORMNAME%.ReportMaxColumns: Integer;
begin
  Result := 78;
end;

%if SendEMail//----------------------------------------------------------------------------

procedure T%FORMNAME%.SendBtnClick(Sender: TObject);
begin
  with TJclEmail.Create do
  try
    ParentWnd := Application.Handle;
    Recipients.Add(%StrValue EMailAddress);
    Subject := %StrValue EMailSubject;
    Body := ReportAsText;
    SaveTaskWindows;
    try
      Send(True);
    finally
      RestoreTaskWindows;
    end;
  finally
    Free;
  end;
end;
%endif
//----------------------------------------------------------------------------

procedure T%FORMNAME%.CopyReportToClipboard;
begin
  ClipBoard.AsText := ReportAsText;
end;

//----------------------------------------------------------------------------

procedure T%FORMNAME%.CreateDetails;
begin
  Screen.Cursor := crHourGlass;
  DetailsMemo.Lines.BeginUpdate;
  try
    CreateReport;
%if LogFile    ReportToLog;%endif
    DetailsMemo.SelStart := 0;
    SendMessage(DetailsMemo.Handle, EM_SCROLLCARET, 0, 0);
    AfterCreateDetails;
  finally
    DetailsMemo.Lines.EndUpdate;
    OkBtn.Enabled := True;
    DetailsBtn.Enabled := True;
    OkBtn.SetFocus;
    Screen.Cursor := crDefault;
  end;
end;

//----------------------------------------------------------------------------

procedure T%FORMNAME%.CreateReport;
var
  SL: TStringList;
%if ModuleList  I: Integer;
  ModuleName: TFileName;
  NtHeaders32: PImageNtHeaders32;
  NtHeaders64: PImageNtHeaders64;
  ModuleBase: Cardinal;
  ImageBaseStr: string;
  PETarget: TJclPeTarget;%endif
%if ActiveControls  C: TWinControl;%endif
%if OSInfo  CpuInfo: TCpuInfo;
  ProcessorDetails: string;%endif
%if StackList  StackList: TJclStackInfoList;%endif
begin
  SL := TStringList.Create;
  try
%if StackList    // Stack list
    StackList := JclLastExceptStackList;
    if Assigned(StackList) then
    begin
      DetailsMemo.Lines.Add(Format(RsStackList, [DateTimeToStr(StackList.TimeStamp)]));
      StackList.AddToStrings(DetailsMemo.Lines, %BoolValue ModuleName, %BoolValue ModuleOffset, %BoolValue CodeDetails, %BoolValue VirtualAddress);
      NextDetailBlock;
    end;
%endif

%if OSInfo    // System and OS information
    DetailsMemo.Lines.Add(Format(RsOSVersion, [GetWindowsVersionString, NtProductTypeString,
      Win32MajorVersion, Win32MinorVersion, Win32BuildNumber, Win32CSDVersion]));
    GetCpuInfo(CpuInfo);
    with CpuInfo do
    begin
      ProcessorDetails := Format(RsProcessor, [Manufacturer, CpuName,
        RoundFrequency(FrequencyInfo.NormFreq)]);
      if not IsFDIVOK then
        ProcessorDetails := ProcessorDetails + ' [FDIV Bug]';
      if ExMMX then
        ProcessorDetails := ProcessorDetails + ' MMXex'
      else if MMX then
        ProcessorDetails := ProcessorDetails + ' MMX';
      if SSE > 0 then
        ProcessorDetails := Format('%s SSE%d', [ProcessorDetails, SSE]);
      if Ex3DNow then
        ProcessorDetails := ProcessorDetails + ' 3DNow!ex'
      else if _3DNow then
        ProcessorDetails := ProcessorDetails + ' 3DNow!';
      if Is64Bits then
        ProcessorDetails := ProcessorDetails + ' 64 bits';
      if DEPCapable then
        ProcessorDetails := ProcessorDetails + ' DEP';
    end;
    DetailsMemo.Lines.Add(ProcessorDetails);
    DetailsMemo.Lines.Add(Format(RsMemory, [GetTotalPhysicalMemory div 1024 div 1024,
      GetFreePhysicalMemory div 1024 div 1024]));
    DetailsMemo.Lines.Add(Format(RsScreenRes, [Screen.Width, Screen.Height, GetBPP]));
    NextDetailBlock;
%endif

%if ModuleList    // Modules list
    if LoadedModulesList(SL, GetCurrentProcessId) then
    begin
      DetailsMemo.Lines.Add(RsModulesList);
      SL.CustomSort(SortModulesListByAddressCompare);
      for I := 0 to SL.Count - 1 do
      begin
        ModuleName := SL[I];
        ModuleBase := Cardinal(SL.Objects[I]);
        DetailsMemo.Lines.Add(Format('[%.8x] %s', [ModuleBase, ModuleName]));
        PETarget := PeMapImgTarget(Pointer(ModuleBase));
        NtHeaders32 := nil;
        NtHeaders64 := nil;
        if PETarget = taWin32 then
          NtHeaders32 := PeMapImgNtHeaders32(Pointer(ModuleBase))
        else
        if PETarget = taWin64 then
          NtHeaders64 := PeMapImgNtHeaders64(Pointer(ModuleBase));
        if (NtHeaders32 <> nil) and (NtHeaders32^.OptionalHeader.ImageBase <> ModuleBase) then
          ImageBaseStr := Format('<%.8x> ', [NtHeaders32^.OptionalHeader.ImageBase])
        else
        if (NtHeaders64 <> nil) and (NtHeaders64^.OptionalHeader.ImageBase <> ModuleBase) then
          ImageBaseStr := Format('<%.8x> ', [NtHeaders64^.OptionalHeader.ImageBase])
        else
          ImageBaseStr := StrRepeat(' ', 11);
        if VersionResourceAvailable(ModuleName) then
          with TJclFileVersionInfo.Create(ModuleName) do
          try
            DetailsMemo.Lines.Add(ImageBaseStr + BinFileVersion + ' - ' + FileVersion);
            if FileDescription <> '' then
              DetailsMemo.Lines.Add(StrRepeat(' ', 11) + FileDescription);
          finally
            Free;
          end
        else
          DetailsMemo.Lines.Add(ImageBaseStr + RsMissingVersionInfo);
      end;
      NextDetailBlock;
    end;
%endif

%if ActiveControls    // Active controls
    if (FLastActiveControl <> nil) then
    begin
      DetailsMemo.Lines.Add(RsActiveControl);
      C := FLastActiveControl;
      while C <> nil do
      begin
        DetailsMemo.Lines.Add(Format('%s "%s"', [C.ClassName, C.Name]));
        C := C.Parent;
      end;
      NextDetailBlock;
    end;
%endif
  finally
    SL.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure T%FORMNAME%.DetailsBtnClick(Sender: TObject);
begin
  DetailsVisible := not DetailsVisible;
end;

//--------------------------------------------------------------------------------------------------

class procedure T%FORMNAME%.ExceptionHandler(Sender: TObject; E: Exception);
begin
  if ExceptionShowing then
    Application.ShowException(E)
  else
  begin
    ExceptionShowing := True;
    try
      ShowException(E, nil);
    finally
      ExceptionShowing := False;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

class procedure T%FORMNAME%.ExceptionThreadHandler(Thread: TJclDebugThread);
begin
  if ExceptionShowing then
    Application.ShowException(Thread.SyncException)
  else
  begin
    ExceptionShowing := True;
    try
      ShowException(Thread.SyncException, Thread);
    finally
      ExceptionShowing := False;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure T%FORMNAME%.FormCreate(Sender: TObject);
begin
%if LogFile  FSimpleLog := TJclSimpleLog.Create(%StrValue LogFileName);%endif
  FFullHeight := ClientHeight;
  DetailsVisible := False;
  Caption := Format(RsAppError, [Application.Title]);
end;

//--------------------------------------------------------------------------------------------------

procedure T%FORMNAME%.FormDestroy(Sender: TObject);
begin
%if LogFile  FreeAndNil(FSimpleLog);%endif
end;

//--------------------------------------------------------------------------------------------------

procedure T%FORMNAME%.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = Ord('C')) and (ssCtrl in Shift) then
  begin
    CopyReportToClipboard;
    MessageBeep(MB_OK);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure T%FORMNAME%.FormPaint(Sender: TObject);
begin
  DrawIcon(Canvas.Handle, TextLabel.Left - GetSystemMetrics(SM_CXICON) - 15,
    TextLabel.Top, LoadIcon(0, IDI_ERROR));
end;

//--------------------------------------------------------------------------------------------------

procedure T%FORMNAME%.FormResize(Sender: TObject);
begin
  UpdateTextLabelScrollbars;
end;

//--------------------------------------------------------------------------------------------------

procedure T%FORMNAME%.FormShow(Sender: TObject);
begin
  BeforeCreateDetails;
  MessageBeep(MB_ICONERROR);
  if FIsMainThead and (GetWindowThreadProcessId(Handle, nil) = MainThreadID) then
    PostMessage(Handle, UM_CREATEDETAILS, 0, 0)
  else
    CreateReport;
end;

//--------------------------------------------------------------------------------------------------

function T%FORMNAME%.GetReportAsText: string;
begin
  Result := StrEnsureSuffix(AnsiCrLf, TextLabel.Text) + AnsiCrLf + DetailsMemo.Text;
end;

//--------------------------------------------------------------------------------------------------

procedure T%FORMNAME%.NextDetailBlock;
begin
  DetailsMemo.Lines.Add(StrRepeat(ReportNewBlockDelimiterChar, ReportMaxColumns));
end;

//--------------------------------------------------------------------------------------------------

function T%FORMNAME%.ReportNewBlockDelimiterChar: Char;
begin
  Result := '-';
end;

%if LogFile//--------------------------------------------------------------------------------------------------

procedure T%FORMNAME%.ReportToLog;
begin
  FSimpleLog.WriteStamp(ReportMaxColumns);
  try
    FSimpleLog.Write(ReportAsText);
  finally
    FSimpleLog.CloseLog;
  end;
end;
%endif
//--------------------------------------------------------------------------------------------------

procedure T%FORMNAME%.SetDetailsVisible(const Value: Boolean);
var
  DetailsCaption: string;
begin
  FDetailsVisible := Value;
  DetailsCaption := Trim(StrRemoveChars(DetailsBtn.Caption, ['<', '>']));
  if Value then
  begin
    Constraints.MinHeight := FNonDetailsHeight + 100;
    Constraints.MaxHeight := Screen.Height;
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
      ClientHeight := BevelDetails.Top;
      FNonDetailsHeight := Height;
    end
    else
      Height := FNonDetailsHeight;
    Constraints.MinHeight := FNonDetailsHeight;
    Constraints.MaxHeight := FNonDetailsHeight
  end;
  DetailsBtn.Caption := DetailsCaption;
  DetailsMemo.Enabled := Value;
end;

//--------------------------------------------------------------------------------------------------

class procedure T%FORMNAME%.ShowException(E: Exception; Thread: TJclDebugThread);
begin
  if %FORMNAME% = nil then
    %FORMNAME% := T%FORMNAME%Class.Create(Application);
  try
    with %FORMNAME% do
    begin
      FIsMainThead := (GetCurrentThreadId = MainThreadID);
%if ActiveControls      FLastActiveControl := Screen.ActiveControl;%endif
      TextLabel.Text := AdjustLineBreaks(StrEnsureSuffix('.', E.Message));
      UpdateTextLabelScrollbars;
      DetailsMemo.Lines.Add(Format(RsExceptionClass, [E.ClassName]));
      if Thread = nil then
        DetailsMemo.Lines.Add(Format(RsExceptionAddr, [ExceptAddr]))
      else
        DetailsMemo.Lines.Add(Format(RsThread, [Thread.ThreadInfo]));
      NextDetailBlock;
      ShowModal;
    end;
  finally
    FreeAndNil(%FORMNAME%);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure T%FORMNAME%.UMCreateDetails(var Message: TMessage);
begin
  Update;
  CreateDetails;
end;

//--------------------------------------------------------------------------------------------------

procedure T%FORMNAME%.UpdateTextLabelScrollbars;
begin
%if AutoScrollBars  Canvas.Font := TextLabel.Font;
  if TextLabel.Lines.Count * Canvas.TextHeight('Wg') > TextLabel.ClientHeight then
    TextLabel.ScrollBars := ssVertical
  else
    TextLabel.ScrollBars := ssNone;%endif   
end;

//==================================================================================================
// Exception handler initialization code
//==================================================================================================

procedure InitializeHandler;
begin
%if RawData  JclStackTrackingOptions := JclStackTrackingOptions + [stRawMode];%endif
%if HookDll  JclStackTrackingOptions := JclStackTrackingOptions + [stStaticModuleList];%endif
%if DelayedTrace  JclStackTrackingOptions := JclStackTrackingOptions + [stDelayedTrace];%endif
%if MainThreadOnly  JclStackTrackingOptions := JclStackTrackingOptions + [stMainThreadOnly];%endif
  JclDebugThreadList.OnSyncException := T%FORMNAME%.ExceptionThreadHandler;
  JclStartExceptionTracking;
%if HookDll  if HookTApplicationHandleException then
    JclTrackExceptionsFromLibraries;%endif
  Application.OnException := T%FORMNAME%.ExceptionHandler;
end;

//--------------------------------------------------------------------------------------------------

procedure UnInitializeHandler;
begin
  Application.OnException := nil;
  JclDebugThreadList.OnSyncException := nil;
  JclUnhookExceptions;
  JclStopExceptionTracking;
end;

//--------------------------------------------------------------------------------------------------

initialization
  InitializeHandler;

finalization
  UnInitializeHandler;

end.
