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
{ The Original Code is JclHookExcept.pas.                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Exception hooking routines                                                                       }
{                                                                                                  }
{ Unit owner: Petr Vones                                                                           }
{ Last modified: April 28, 2002                                                                    }
{                                                                                                  }
{**************************************************************************************************}

unit JclHookExcept;

interface

{$I jcl.inc}

uses
  Windows, SysUtils;

//--------------------------------------------------------------------------------------------------
// Exception hooking notifiers routines
//--------------------------------------------------------------------------------------------------

type
  TJclExceptNotifyProc = procedure (ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean);
  TJclExceptNotifyMethod = procedure (ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean) of object;

  TJclExceptNotifyPriority = (npNormal, npFirstChain);

function JclAddExceptNotifier(const NotifyProc: TJclExceptNotifyProc; Priority: TJclExceptNotifyPriority = npNormal): Boolean; overload;
function JclAddExceptNotifier(const NotifyMethod: TJclExceptNotifyMethod; Priority: TJclExceptNotifyPriority = npNormal): Boolean; overload;

function JclRemoveExceptNotifier(const NotifyProc: TJclExceptNotifyProc): Boolean; overload;
function JclRemoveExceptNotifier(const NotifyMethod: TJclExceptNotifyMethod): Boolean;  overload;

procedure JclReplaceExceptObj(NewExceptObj: Exception);

//--------------------------------------------------------------------------------------------------
// Exception hooking routines
//--------------------------------------------------------------------------------------------------

function JclHookExceptions: Boolean;
function JclUnhookExceptions: Boolean;
function JclExceptionsHooked: Boolean;

function JclHookExceptionsInModule(Module: HMODULE): Boolean;
function JclUnkookExceptionsInModule(Module: HMODULE): Boolean;

//--------------------------------------------------------------------------------------------------
// Exceptions hooking in libraries (hooking from main EXE module option)
//--------------------------------------------------------------------------------------------------

type
  TJclModuleArray = array of HMODULE;

function JclHookExceptionsInLibraries(var ModulesList: TJclModuleArray; ReturnBorlandModules: Boolean = False): Boolean;

//--------------------------------------------------------------------------------------------------
// Exceptions hooking in libraries (hooking from libraries option)
//--------------------------------------------------------------------------------------------------

function JclInitializeLibrariesHookExcept(DynamicLinkingOnly: Boolean = False): Boolean;
function JclHookedExceptModulesList(var ModulesList: TJclModuleArray): Boolean;

//--------------------------------------------------------------------------------------------------
// Hooking routines location info helper
//--------------------------------------------------------------------------------------------------

function JclBelongsHookedCode(Addr: Pointer): Boolean;

implementation

uses
  Classes,
  JclBase, JclPeImage, JclSysInfo, JclSysUtils;

{$UNDEF JclHookExceptUseMappingVersion}

type
  PExceptionArguments = ^TExceptionArguments;
  TExceptionArguments = record
    ExceptAddr: Pointer;
    ExceptObj: Exception;
  end;

  TNotifierItem = class (TObject)
  private
    FNotifyMethod: TJclExceptNotifyMethod;
    FNotifyProc: TJclExceptNotifyProc;
    FPriority: TJclExceptNotifyPriority;
  public
    constructor Create(const NotifyProc: TJclExceptNotifyProc; Priority: TJclExceptNotifyPriority); overload;
    constructor Create(const NotifyMethod: TJclExceptNotifyMethod; Priority: TJclExceptNotifyPriority); overload;
    procedure DoNotify(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean);
    property Priority: TJclExceptNotifyPriority read FPriority;
  end;

var
  ExceptionsHooked: Boolean;
  Kernel32_RaiseException: procedure (dwExceptionCode, dwExceptionFlags,
    nNumberOfArguments: DWORD; lpArguments: PDWORD); stdcall;
  SysUtils_ExceptObjProc: function (P: PExceptionRecord): Exception;
  Notifiers: TThreadList;

{$IFDEF JclHookExceptUseMappingVersion}

const
  MappingNameFormat = 'JclHookExceptDataMap$%.8x_1';
  MutexNameFormat   = 'JclHookExceptDataMtx$%.8x_1';

type
  PJclHookExceptData = ^TJclHookExceptData;
  TJclHookExceptData = packed record
    HookedRaiseExceptionAddr: Pointer;
    HookedModules: array [0..127] of HMODULE;
  end;

  TJclHookExceptSharedData = class (TObject)
  private
    FData: PJclHookExceptData;
    FMutexHandle: THandle;
    FMappingHandle: THandle;
  protected
    procedure Close;
    function FindModuleIndex(Module: HMODULE; var FreeSlotIndex: Integer): Integer;
    procedure InitializeHook;
    procedure Open;
    procedure UninitializeHook;
  public
    constructor Create;
    destructor Destroy; override;
    class function Exists: Boolean;
    procedure List(var ModulesList: TJclModuleArray);
    procedure HookModule;
    procedure UnhookModule;
  end;

var
  HookExceptSharedData: TJclHookExceptSharedData;

{$ELSE JclHookExceptUseMappingVersion}

const
  JclHookExceptDebugHookName = '__JclHookExcept';

type
  TJclHookExceptDebugHook = procedure (Module: HMODULE; Hook: Boolean); stdcall;

  TJclHookExceptModuleList = class (TObject)
  private
    FModules: TThreadList;
  protected
    procedure HookStaticModules;
  public
    constructor Create;
    destructor Destroy; override;
    class function JclHookExceptDebugHookAddr: Pointer;
    procedure HookModule(Module: HMODULE);
    procedure List(var ModulesList: TJclModuleArray);
    procedure UnhookModule(Module: HMODULE);
  end;

var
  HookExceptModuleList: TJclHookExceptModuleList;
  JclHookExceptDebugHook: Pointer;

exports
  JclHookExceptDebugHook name JclHookExceptDebugHookName;

{$ENDIF JclHookExceptUseMappingVersion}

{$STACKFRAMES OFF}

threadvar
  Recursive: Boolean;
  NewResultExc: Exception;

//==================================================================================================
// Helper routines
//==================================================================================================

function RaiseExceptionAddress: Pointer;
begin
  Result := GetProcAddress(GetModuleHandle(kernel32), 'RaiseException');
  Assert(Result <> nil);
end;

//--------------------------------------------------------------------------------------------------

procedure FreeNotifiers;
var
  I: Integer;
begin
  with Notifiers.LockList do
  try
    for I := 0 to Count - 1 do
      TObject(Items[I]).Free;
  finally
    Notifiers.UnlockList;
  end;
  FreeAndNil(Notifiers);
end;

//==================================================================================================
// TNotifierItem
//==================================================================================================

constructor TNotifierItem.Create(const NotifyProc: TJclExceptNotifyProc; Priority: TJclExceptNotifyPriority);
begin
  FNotifyProc := NotifyProc;
  FPriority := Priority;
end;

//--------------------------------------------------------------------------------------------------

constructor TNotifierItem.Create(const NotifyMethod: TJclExceptNotifyMethod; Priority: TJclExceptNotifyPriority);
begin
  FNotifyMethod := NotifyMethod;
  FPriority := Priority;
end;

//--------------------------------------------------------------------------------------------------

procedure TNotifierItem.DoNotify(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean);
begin
  if Assigned(FNotifyProc) then
    FNotifyProc(ExceptObj, ExceptAddr, OSException)
  else
  if Assigned(FNotifyMethod) then
    FNotifyMethod(ExceptObj, ExceptAddr, OSException);
end;

//--------------------------------------------------------------------------------------------------

{$STACKFRAMES ON}

procedure DoExceptNotify(ExceptObj: Exception; ExceptAddr: Pointer; OSException: Boolean);
var
  Priorities: TJclExceptNotifyPriority;
  I: Integer;
begin
  if Recursive then
    Exit;
  Recursive := True;
  NewResultExc := nil;
  try
    with Notifiers.LockList do
    try
      for Priorities := High(Priorities) downto Low(Priorities) do
        for I := 0 to Count - 1 do
          with TNotifierItem(Items[I]) do
            if Priority = Priorities then
              DoNotify(ExceptObj, ExceptAddr, OSException);
    finally
      Notifiers.UnlockList;
    end;
  finally
    Recursive := False;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure HookedRaiseException(ExceptionCode, ExceptionFlags, NumberOfArguments: DWORD;
  Arguments: PExceptionArguments); stdcall;
const
  {$IFDEF DELPHI2}
  cDelphiException = $0EEDFACE;
  {$ELSE}
  cDelphiException = $0EEDFADE;
  {$ENDIF DELPHI2}
  cNonContinuable = 1;
begin
  if (ExceptionFlags = cNonContinuable) and (ExceptionCode = cDelphiException) and
    (NumberOfArguments = 7) and (DWORD(Arguments) = DWORD(@Arguments) + 4) then
      DoExceptNotify(Arguments.ExceptObj, Arguments.ExceptAddr, False);
  Kernel32_RaiseException(ExceptionCode, ExceptionFlags, NumberOfArguments, PDWORD(Arguments));
end;

//--------------------------------------------------------------------------------------------------

function HookedExceptObjProc(P: PExceptionRecord): Exception;
var
  NewResultExcCache: Exception; // TLS optimization
begin
  Result := SysUtils_ExceptObjProc(P);
  DoExceptNotify(Result, P^.ExceptionAddress, True);
  NewResultExcCache := NewResultExc;
  if NewResultExcCache <> nil then
    Result := NewResultExcCache;
end;

{$IFNDEF STACKFRAMES_ON}
{$STACKFRAMES OFF}
{$ENDIF STACKFRAMES_ON}

//--------------------------------------------------------------------------------------------------

// Do not change ordering of HookedRaiseException, HookedExceptObjProc and JclBelongsHookedCode routines
function JclBelongsHookedCode(Addr: Pointer): Boolean;
begin
  Result := (Cardinal(@HookedRaiseException) < Cardinal(@JclBelongsHookedCode)) and
    (Cardinal(@HookedRaiseException) <= Cardinal(Addr)) and
    (Cardinal(@JclBelongsHookedCode) > Cardinal(Addr));
end;

//--------------------------------------------------------------------------------------------------

function JclAddExceptNotifier(const NotifyProc: TJclExceptNotifyProc; Priority: TJclExceptNotifyPriority): Boolean;
begin
  Result := Assigned(NotifyProc);
  if Result then
    with Notifiers.LockList do
    try
      Add(TNotifierItem.Create(NotifyProc, Priority));
    finally
      Notifiers.UnlockList;
    end;
end;

//--------------------------------------------------------------------------------------------------

function JclAddExceptNotifier(const NotifyMethod: TJclExceptNotifyMethod; Priority: TJclExceptNotifyPriority): Boolean;
begin
  Result := Assigned(NotifyMethod);
  if Result then
    with Notifiers.LockList do
    try
      Add(TNotifierItem.Create(NotifyMethod, Priority));
    finally
      Notifiers.UnlockList;
    end;
end;

//--------------------------------------------------------------------------------------------------

function JclRemoveExceptNotifier(const NotifyProc: TJclExceptNotifyProc): Boolean;
var
  O: TNotifierItem;
  I: Integer;
begin
  Result := Assigned(NotifyProc);
  if Result then
    with Notifiers.LockList do
    try
      for I := 0 to Count - 1 do
      begin
        O := TNotifierItem(Items[I]);
        if @O.FNotifyProc = @NotifyProc then
        begin
          O.Free;
          Items[I] := nil;
        end;
      end;
      Pack;
    finally
      Notifiers.UnlockList;
    end;
end;

//--------------------------------------------------------------------------------------------------

function JclRemoveExceptNotifier(const NotifyMethod: TJclExceptNotifyMethod): Boolean;
var
  O: TNotifierItem;
  I: Integer;
begin
  Result := Assigned(NotifyMethod);
  if Result then
    with Notifiers.LockList do
    try
      for I := 0 to Count - 1 do
      begin
        O := TNotifierItem(Items[I]);
        if (TMethod(O.FNotifyMethod).Code = TMethod(NotifyMethod).Code) and
          (TMethod(O.FNotifyMethod).Data = TMethod(NotifyMethod).Data) then
        begin
          O.Free;
          Items[I] := nil;
        end;
      end;
      Pack;
    finally
      Notifiers.UnlockList;
    end;
end;

//--------------------------------------------------------------------------------------------------

procedure JclReplaceExceptObj(NewExceptObj: Exception);
begin
  Assert(Recursive);
  NewResultExc := NewExceptObj;
end;

//--------------------------------------------------------------------------------------------------

function JclHookExceptions: Boolean;
var
  RaiseExceptionAddressCache: Pointer;
begin
  if not ExceptionsHooked then
  begin
    Recursive := False;
    RaiseExceptionAddressCache := RaiseExceptionAddress;
    with TJclPeMapImgHooks do
      Result := ReplaceImport(SystemBase, kernel32, RaiseExceptionAddressCache, @HookedRaiseException);
    if Result then
    begin
      @Kernel32_RaiseException := RaiseExceptionAddressCache;
      SysUtils_ExceptObjProc := System.ExceptObjProc;
      System.ExceptObjProc := @HookedExceptObjProc;
    end;
    ExceptionsHooked := Result;
  end
  else
    Result := True;
end;

//--------------------------------------------------------------------------------------------------

function JclUnhookExceptions: Boolean;
begin
  if ExceptionsHooked then
  begin
    with TJclPeMapImgHooks do
      ReplaceImport(SystemBase, kernel32, @HookedRaiseException, @Kernel32_RaiseException);
    System.ExceptObjProc := @SysUtils_ExceptObjProc;
    @SysUtils_ExceptObjProc := nil;
    @Kernel32_RaiseException := nil;
    Result := True;
    ExceptionsHooked := False;
  end
  else
    Result := True;
end;

//--------------------------------------------------------------------------------------------------

function JclExceptionsHooked: Boolean;
begin
  Result := ExceptionsHooked;
end;

//--------------------------------------------------------------------------------------------------

function JclHookExceptionsInModule(Module: HMODULE): Boolean;
begin
  Result := ExceptionsHooked and
    TJclPeMapImgHooks.ReplaceImport(Pointer(Module), kernel32, RaiseExceptionAddress, @HookedRaiseException);
end;

//--------------------------------------------------------------------------------------------------

function JclUnkookExceptionsInModule(Module: HMODULE): Boolean;
begin
  Result := ExceptionsHooked and
    TJclPeMapImgHooks.ReplaceImport(Pointer(Module), kernel32, @HookedRaiseException, @Kernel32_RaiseException);
end;

//==================================================================================================
// Exceptions hooking in libraries (hooking from main EXE module option)
//==================================================================================================

function JclHookExceptionsInLibraries(var ModulesList: TJclModuleArray; ReturnBorlandModules: Boolean): Boolean;
var
  Modules: TStringList;
  PeImage: TJclPeBorImage;
  I, C: Integer;
  H: HMODULE;
begin
  Result := ExceptionsHooked and not IsLibrary;
  if Result then
  begin
    ModulesList := nil;
    C := Length(ModulesList);
    PeImage := nil;
    Modules := TStringList.Create;
    try
      PeImage := TJclPeBorImage.Create(True);
      if LoadedModulesList(Modules, GetCurrentProcessId, True) then
        for I := 0 to Modules.Count - 1 do
        begin
          H := HMODULE(Modules.Objects[I]);
          if H <> HInstance then
          begin
            PeImage.AttachLoadedModule(H);
            if PeImage.StatusOK and PeImage.IsBorlandImage and (JclHookExceptionsInModule(H) or ReturnBorlandModules) then
            begin
              SetLength(ModulesList, C + 1);
              ModulesList[C] := H;
              Inc(C);
            end;
          end;
        end;
    finally
      Modules.Free;
      PeImage.Free;
    end;
  end;
end;

//==================================================================================================
// Exceptions hooking in libraries (hooking from libraries option)
//==================================================================================================

{$IFDEF JclHookExceptUseMappingVersion}

function JclInitializeLibrariesHookExcept(DynamicLinkingOnly: Boolean): Boolean;
begin
  if not Assigned(HookExceptSharedData) and
    ((not IsLibrary) or (not DynamicLinkingOnly) or TJclHookExceptSharedData.Exists) then
    HookExceptSharedData := TJclHookExceptSharedData.Create;
  Result := Assigned(HookExceptSharedData);
end;

//--------------------------------------------------------------------------------------------------

function JclHookedExceptModulesList(var ModulesList: TJclModuleArray): Boolean;
begin
  Result := Assigned(HookExceptSharedData);
  if Result then
    HookExceptSharedData.List(ModulesList);
end;

//==================================================================================================
// TJclHookExceptSharedData
//==================================================================================================

procedure TJclHookExceptSharedData.Close;
begin
  if FMutexHandle <> 0 then
    CloseHandle(FMutexHandle);
  if FData <> nil then
    UnmapViewOfFile(FData);
  if FMappingHandle <> 0 then
    CloseHandle(FMappingHandle);
end;

//--------------------------------------------------------------------------------------------------

constructor TJclHookExceptSharedData.Create;
begin
  Open;
  if IsLibrary then
    HookModule
  else
    InitializeHook;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclHookExceptSharedData.Destroy;
begin
  if IsLibrary then
    UnhookModule
  else
    UninitializeHook;
  Close;
  inherited;
end;

//--------------------------------------------------------------------------------------------------

class function TJclHookExceptSharedData.Exists: Boolean;
var
  H: THandle;
  MutexName: string;
begin
  MutexName := Format(MutexNameFormat, [GetCurrentProcessId]);
  H := OpenMutex(MUTEX_ALL_ACCESS, False, PChar(MutexName));
  Result := H <> 0;
  if Result then
    CloseHandle(H);
end;

//--------------------------------------------------------------------------------------------------

function TJclHookExceptSharedData.FindModuleIndex(Module: HMODULE; var FreeSlotIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  FreeSlotIndex := -1;
  with FData^ do
    for I := Low(HookedModules) to High(HookedModules) do
      if HookedModules[I] = Module then
      begin
        Result := I;
        Break;
      end
      else
      if (HookedModules[I] = 0) and (FreeSlotIndex = -1) then
        FreeSlotIndex := I;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclHookExceptSharedData.HookModule;
var
  I: Integer;
  Module: HMODULE;
begin
  WaitForSingleObject(FMutexHandle, INFINITE);
  try
    Module := HInstance;
    if FindModuleIndex(Module, I) = -1 then
    begin
      FData^.HookedModules[I] := Module;
      if FData^.HookedRaiseExceptionAddr <> nil then
        with TJclPeMapImgHooks do
          ReplaceImport(SystemBase, kernel32, RaiseExceptionAddress, FData^.HookedRaiseExceptionAddr);
    end;
  finally
    ReleaseMutex(FMutexHandle);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclHookExceptSharedData.InitializeHook;
var
  I: Integer;
begin
  WaitForSingleObject(FMutexHandle, INFINITE);
  try
    with FData^ do
    begin
      HookedRaiseExceptionAddr := @HookedRaiseException;
      for I := Low(HookedModules) to High(HookedModules) do
        if HookedModules[I] <> 0 then
          JclHookExceptionsInModule(HookedModules[I]);
    end;
  finally
    ReleaseMutex(FMutexHandle);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclHookExceptSharedData.List(var ModulesList: TJclModuleArray);
var
  I, C: Integer;
begin
  WaitForSingleObject(FMutexHandle, INFINITE);
  try
    C := 0;
    ModulesList := nil;
    with FData^ do
    begin
      for I := Low(HookedModules) to High(HookedModules) do
        if HookedModules[I] <> 0 then
        begin
          SetLength(ModulesList, C + 1);
          ModulesList[C] := HookedModules[I];
          Inc(C);
        end;
    end;
  finally
    ReleaseMutex(FMutexHandle);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclHookExceptSharedData.Open;
var
  MappingName, MutexName: string;
begin
  MappingName := Format(MappingNameFormat, [GetCurrentProcessId]);
  MutexName := Format(MutexNameFormat, [GetCurrentProcessId]);
  FMutexHandle := CreateMutex(nil, False, PChar(MutexName));
  if FMutexHandle = 0 then
    RaiseLastOSError;
  FMappingHandle := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0,
    SizeOf(TJclHookExceptData), PChar(MappingName));
  if FMappingHandle = 0 then
    RaiseLastOSError;
  FData := MapViewOfFile(FMappingHandle, FILE_MAP_WRITE, 0, 0, 0);
  if FData = nil then
    RaiseLastOSError;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclHookExceptSharedData.UnhookModule;
var
  I, Slot: Integer;
  Module: HMODULE;
begin
  WaitForSingleObject(FMutexHandle, INFINITE);
  try
    Module := HInstance;
    I := FindModuleIndex(Module, Slot);
    if I <> -1 then
    begin
      FData^.HookedModules[I] := 0;
    end;
  finally
    ReleaseMutex(FMutexHandle);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclHookExceptSharedData.UninitializeHook;
begin
  WaitForSingleObject(FMutexHandle, INFINITE);
  try
    FData^.HookedRaiseExceptionAddr := nil;
  finally
    ReleaseMutex(FMutexHandle);
  end;
end;

//--------------------------------------------------------------------------------------------------

{$ELSE JclHookExceptUseMappingVersion}

//--------------------------------------------------------------------------------------------------

procedure JclHookExceptDebugHookProc(Module: HMODULE; Hook: Boolean); stdcall;
begin
  if Hook then
    HookExceptModuleList.HookModule(Module)
  else
    HookExceptModuleList.UnhookModule(Module);
end;

//--------------------------------------------------------------------------------------------------

function CallExportedHookExceptProc(Module: HMODULE; Hook: Boolean): Boolean;
var
  HookExceptProcPtr: PPointer;
  HookExceptProc: TJclHookExceptDebugHook;
begin
  HookExceptProcPtr := TJclHookExceptModuleList.JclHookExceptDebugHookAddr;
  Result := Assigned(HookExceptProcPtr);
  if Result then
  begin
    @HookExceptProc := HookExceptProcPtr^;
    if Assigned(HookExceptProc) then
      HookExceptProc(Module, True);
  end;
end;

//--------------------------------------------------------------------------------------------------

function JclInitializeLibrariesHookExcept(DynamicLinkingOnly: Boolean): Boolean;
begin
  if IsLibrary then
    Result := CallExportedHookExceptProc(SystemTObjectInstance, True)
  else
  begin
    if not Assigned(HookExceptModuleList) then
      HookExceptModuleList := TJclHookExceptModuleList.Create;
    Result := True;
  end;
end;

//--------------------------------------------------------------------------------------------------

function JclHookedExceptModulesList(var ModulesList: TJclModuleArray): Boolean;
begin
  Result := Assigned(HookExceptModuleList);
  if Result then
    HookExceptModuleList.List(ModulesList);
end;

//--------------------------------------------------------------------------------------------------

procedure FinalizeLibrariesHookExcept;
begin
  if IsLibrary then
    CallExportedHookExceptProc(SystemTObjectInstance, False);
end;

//==================================================================================================
// TJclHookExceptModuleList
//==================================================================================================

constructor TJclHookExceptModuleList.Create;
begin
  FModules := TThreadList.Create;
  HookStaticModules;
  JclHookExceptDebugHook := @JclHookExceptDebugHookProc;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclHookExceptModuleList.Destroy;
begin
  JclHookExceptDebugHook := nil;
  FreeAndNil(FModules);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclHookExceptModuleList.HookModule(Module: HMODULE);
begin
  with FModules.LockList do
  try
    if IndexOf(Pointer(Module)) = -1 then
    begin
      Add(Pointer(Module));
      JclHookExceptionsInModule(Module);
    end;
  finally
    FModules.UnlockList;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclHookExceptModuleList.HookStaticModules;
var
  ModulesList: TStringList;
  I: Integer;
begin
  ModulesList := nil;
  with FModules.LockList do
  try
    ModulesList := TStringList.Create;
    if LoadedModulesList(ModulesList, GetCurrentProcessId, True) then
      for I := 0 to ModulesList.Count - 1 do
        if GetProcAddress(HMODULE(ModulesList.Objects[I]), JclHookExceptDebugHookName) <> nil then
          HookModule(HMODULE(ModulesList.Objects[I]));
  finally
    FModules.UnlockList;
    ModulesList.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

class function TJclHookExceptModuleList.JclHookExceptDebugHookAddr: Pointer;
var
  HostModule: HMODULE;
begin
  HostModule := GetModuleHandle(nil);
  Result := GetProcAddress(HostModule, JclHookExceptDebugHookName);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclHookExceptModuleList.List(var ModulesList: TJclModuleArray);
var
  I: Integer;
begin
  with FModules.LockList do
  try
    SetLength(ModulesList, Count);
    for I := 0 to Count - 1 do
      ModulesList[I] := HMODULE(Items[I]);
  finally
    FModules.UnlockList;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclHookExceptModuleList.UnhookModule(Module: HMODULE);
begin
  with FModules.LockList do
  try
    Remove(Pointer(Module));
  finally
    FModules.UnlockList;
  end;
end;

{$ENDIF JclHookExceptUseMappingVersion}

//--------------------------------------------------------------------------------------------------

initialization
  Notifiers := TThreadList.Create;
  
finalization
  {$IFDEF JclHookExceptUseMappingVersion}
  FreeAndNil(HookExceptSharedData);
  {$ELSE JclHookExceptUseMappingVersion}
  FreeAndNil(HookExceptModuleList);
  FinalizeLibrariesHookExcept;
  {$ENDIF JclHookExceptUseMappingVersion}
  FreeNotifiers;

end.
