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
{ The Original Code is OpenDlgFavAdapter.pas.                                                      }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones.                                        }
{ Portions created by Petr Vones are Copyright (C) Petr Vones. All rights reserved.                }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Salvatore Besso                                                                                }
{   Florent Ouchet (move to JCL runtime)                                                           }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclOpenDialogHooks;

interface

{$I jcl.inc}

uses
  Windows, Messages, Classes, SysUtils, Controls, StdCtrls, ExtCtrls,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase, JclPeImage, JclWin32;

type
  TJclOpenDialogHook = class (TObject)
  private
    FDisableHelpButton: Boolean;
    FDisablePlacesBar: Boolean;
    FHooks: TJclPeMapImgHooks;
    FIsOpenPictureDialog: Boolean;
    FParentWndInstance: Pointer;
    FOldParentWndInstance: Pointer;
    FPictureDialogLastFolder: string;
    FWndInstance: Pointer;
    FOldWndInstance: Pointer;
    FOnClose: TNotifyEvent;
    FOnShow: TNotifyEvent;
    function GetCurrentFolder: string;
    function GetFileNameEditWnd: HWND;
    procedure SetCurrentFolder(const Value: string);
  protected
    FHandle: HWND;
    FParentWnd: HWND;
    procedure AdjustControlPos; virtual;
    procedure DialogFolderChange; virtual;
    procedure DialogShow; virtual;
    procedure DoClose;
    procedure DoShow;
    procedure ParentWndProc(var Message: TMessage); virtual;
    procedure WndProc(var Message: TMessage); virtual;
    property FileNameEditWnd: HWND read GetFileNameEditWnd;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure HookDialogs;
    procedure UnhookDialogs;
    property CurrentFolder: string read GetCurrentFolder write SetCurrentFolder;
    property DisableHelpButton: Boolean read FDisableHelpButton write FDisableHelpButton;
    property DisablePlacesBar: Boolean read FDisablePlacesBar write FDisablePlacesBar;
    property IsOpenPictureDialog: Boolean read FIsOpenPictureDialog;
    property PictureDialogLastFolder: string read FPictureDialogLastFolder write FPictureDialogLastFolder;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;

  TJclOpenDialogHookClass = class of TJclOpenDialogHook;

  EJclOpenDialogHookError = class(EJclError);

function InitializeOpenDialogHook(OpenDialogHookClass: TJclOpenDialogHookClass): TJclOpenDialogHook;
procedure FinalizeOpenDialogHook;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\vcl';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  CommDlg, Dlgs,
  JclFileUtils, JclStrings, JclSysInfo, JclSysUtils,
  JclVclResources;

{$R JclOpenDialog.res}

const
  OpenDialogTemplateName        = 'JCLOPENDLGHOOK';
  OpenPictureDialogTemplateName = 'DLGTEMPLATE';


type
  TGetOpenFileName = function (var OpenFile: TOpenFilename): Bool; stdcall;

var
  OldGetOpenFileName: TGetOpenFileName;
  OldGetSaveFileName: TGetOpenFileName;
  OldExplorerHook: function(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): UINT stdcall;
  GlobalOpenDialogHook: TJclOpenDialogHook;

function NewExplorerHook(Wnd: HWnd; Msg: UINT; WParam: WPARAM; LParam: LPARAM): UINT; stdcall;
begin
  Result := OldExplorerHook(Wnd, Msg, WParam, LParam);
  if (Msg = WM_INITDIALOG) and Assigned(GlobalOpenDialogHook) then
  begin
    GlobalOpenDialogHook.FHandle := Wnd;
    GlobalOpenDialogHook.FOldWndInstance := Pointer(SetWindowLongPtr(Wnd, GWLP_WNDPROC, LONG_PTR(GlobalOpenDialogHook.FWndInstance)));
    CallWindowProc(GlobalOpenDialogHook.FWndInstance, Wnd, Msg, WParam, LParam);
  end;
end;

procedure InitOpenFileStruct(var OpenFile: TOpenFilename);
var
  InitDir: string;
begin
  with OpenFile do
    if Flags and OFN_EXPLORER <> 0 then
    begin
      if Assigned(GlobalOpenDialogHook) then
        GlobalOpenDialogHook.FIsOpenPictureDialog := False;
      if Flags and OFN_ENABLETEMPLATE = 0 then
      begin
        OldExplorerHook := lpfnHook;
        lpfnHook := NewExplorerHook;
        lpTemplateName := OpenDialogTemplateName;
        hInstance := FindResourceHInstance(FindClassHInstance(GlobalOpenDialogHook.ClassType));
        Flags := Flags or OFN_ENABLETEMPLATE;
        if Assigned(GlobalOpenDialogHook) then
        begin
          if GlobalOpenDialogHook.DisableHelpButton then
            Flags := Flags and (not OFN_SHOWHELP);
          if GlobalOpenDialogHook.DisablePlacesBar and (lStructSize = SizeOf(TOpenFilename)) then
            FlagsEx := FlagsEx or OFN_EX_NOPLACESBAR;
        end;
      end
      else
      if (StrIComp(lpTemplateName, OpenPictureDialogTemplateName) = 0) and Assigned(GlobalOpenDialogHook) then
      begin
        GlobalOpenDialogHook.FIsOpenPictureDialog := True;
        OldExplorerHook := lpfnHook;
        lpfnHook := NewExplorerHook;
        InitDir := GlobalOpenDialogHook.PictureDialogLastFolder;
        if DirectoryExists(InitDir) then
          lpstrInitialDir := PChar(GlobalOpenDialogHook.PictureDialogLastFolder)
        else
          GlobalOpenDialogHook.PictureDialogLastFolder := '';
      end;
   end;
end;

function NewGetOpenFileName(var OpenFile: TOpenFilename): Bool; stdcall;
begin
  InitOpenFileStruct(OpenFile);
  Result := OldGetOpenFileName(OpenFile);
end;

function NewGetSaveFileName(var OpenFile: TOpenFilename): Bool; stdcall;
begin
  InitOpenFileStruct(OpenFile);
  Result := OldGetSaveFileName(OpenFile);
end;

function InitializeOpenDialogHook(OpenDialogHookClass: TJclOpenDialogHookClass): TJclOpenDialogHook;
begin
  if Assigned(GlobalOpenDialogHook) then
  begin
    if GlobalOpenDialogHook.ClassType <> OpenDialogHookClass then
      raise EJclOpenDialogHookError.CreateResFmt(@RsEOpenDialogHookExists, [GlobalOpenDialogHook.ClassName]);
  end
  else
    GlobalOpenDialogHook := OpenDialogHookClass.Create;
  Result := GlobalOpenDialogHook;
end;

procedure FinalizeOpenDialogHook;
begin
  FreeAndNil(GlobalOpenDialogHook);
end;

//=== { TJclOpenDialogHook } =================================================

constructor TJclOpenDialogHook.Create;
begin
  inherited Create;
  FHooks := TJclPeMapImgHooks.Create;
  FParentWndInstance := MakeObjectInstance(ParentWndProc);
  FWndInstance := MakeObjectInstance(WndProc);
end;

destructor TJclOpenDialogHook.Destroy;
begin
  UnhookDialogs;
  FreeObjectInstance(FParentWndInstance);
  FreeObjectInstance(FWndInstance);
  FreeAndNil(FHooks);
  inherited Destroy;
end;

procedure TJclOpenDialogHook.AdjustControlPos;
begin
  // override to customize
end;

procedure TJclOpenDialogHook.DialogFolderChange;
begin
  // override to customize
end;

procedure TJclOpenDialogHook.DialogShow;
begin
  // override to customize
  FParentWnd := GetParent(FHandle);
  if IsWin2k or IsWinXP then
    FOldParentWndInstance := Pointer(SetWindowLongPtr(FParentWnd, GWLP_WNDPROC, LONG_PTR(FParentWndInstance)));
end;

procedure TJclOpenDialogHook.DoClose;
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TJclOpenDialogHook.DoShow;
begin
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

function TJclOpenDialogHook.GetCurrentFolder: string;
var
  Path: array [0..MAX_PATH] of Char;
begin
  SetString(Result, Path, SendMessage(FParentWnd, CDM_GETFOLDERPATH, SizeOf(Path), Integer(@Path)));
  StrResetLength(Result);
end;

function TJclOpenDialogHook.GetFileNameEditWnd: HWND;
begin
  Result := GetDlgItem(FParentWnd, edt1);
  if Result = 0 then
    Result := GetDlgItem(FParentWnd, cmb13);
end;

procedure TJclOpenDialogHook.HookDialogs;
  procedure HookImportsForModule(ModuleBase: Pointer);
  const
    comdlg32 = 'comdlg32.dll';
  begin
    if ModuleBase <> nil then
    begin
      {$IFDEF UNICODE}
      FHooks.HookImport(ModuleBase, comdlg32, 'GetOpenFileNameW', @NewGetOpenFileName, @OldGetOpenFileName);
      FHooks.HookImport(ModuleBase, comdlg32, 'GetSaveFileNameW', @NewGetSaveFileName, @OldGetSaveFileName);
      {$ELSE}
      FHooks.HookImport(ModuleBase, comdlg32, 'GetOpenFileNameA', @NewGetOpenFileName, @OldGetOpenFileName);
      FHooks.HookImport(ModuleBase, comdlg32, 'GetSaveFileNameA', @NewGetSaveFileName, @OldGetSaveFileName);
      {$ENDIF UNICODE}
    end;
  end;
var
  Pe: TJclPeImage;
  I: Integer;
  HookedModule: LongWord;
begin
  { TODO : Hook all loaded modules }
  Pe := TJclPeImage.Create(True);
  try
    HookedModule := FindClassHInstance(ClassType);
    Pe.AttachLoadedModule(HookedModule);
    if Pe.StatusOK then
    begin
      HookImportsForModule(Pointer(HookedModule));
      for I := 0 to Pe.ImportList.UniqueLibItemCount - 1 do
        HookImportsForModule(Pointer(GetModuleHandle(PChar(Pe.ImportList.UniqueLibItems[I].FileName))));
    end;
  finally
    Pe.Free;
  end;
end;

procedure TJclOpenDialogHook.ParentWndProc(var Message: TMessage);
begin
  with Message do
  begin
    Result := CallWindowProc(FOldParentWndInstance, FParentWnd, Msg, WParam, LParam);
    if Msg = WM_SIZE then
      AdjustControlPos;
  end;
end;

procedure TJclOpenDialogHook.SetCurrentFolder(const Value: string);
var
  LastFocus: HWND;
  FileNameBuffer: string;
begin
  if (FParentWnd <> 0) and DirectoryExists(Value) then
  begin
    LastFocus := GetFocus;
    FileNameBuffer := GetWindowCaption(FileNameEditWnd);
    SendMessage(FParentWnd, CDM_SETCONTROLTEXT, edt1, LPARAM(PChar(Value)));
    SendMessage(GetDlgItem(FParentWnd, 1), BM_CLICK, 0, 0);
    SendMessage(FParentWnd, CDM_SETCONTROLTEXT, edt1, LPARAM(PChar(FileNameBuffer)));
    SetFocus(LastFocus);
  end;
end;

procedure TJclOpenDialogHook.UnhookDialogs;
var
  I: Integer;
begin
  I := 0;
  while I < FHooks.Count do
    if not FHooks[I].Unhook then
      Inc(I);
end;

procedure TJclOpenDialogHook.WndProc(var Message: TMessage);

  procedure Default;
  begin
    with Message do
      Result := CallWindowProc(FOldWndInstance, FHandle, Msg, WParam, LParam);
  end;

begin
  if FHandle <> 0 then
  begin
    case Message.Msg of
      WM_NOTIFY:
        begin
          case (POFNotify(Message.LParam)^.hdr.code) of
            CDN_INITDONE:
              DialogShow;
            CDN_FOLDERCHANGE:
              if not IsOpenPictureDialog then
                DialogFolderChange;
            CDN_FILEOK:
              if IsOpenPictureDialog then
                FPictureDialogLastFolder := CurrentFolder;
          end;
          Default;
        end;
      WM_NCDESTROY:
        begin
          Default;
          FHandle := 0;
        end;
    else
      Default;
    end;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
