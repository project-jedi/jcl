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
{ The Original Code is JclHookExcept.pas.                                      }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ Exception hooking routines                                                   }
{                                                                              }
{ Unit owner: Petr Vones                                                       }
{ Last modified: July 15, 2001                                                 }
{                                                                              }
{******************************************************************************}

unit JclHookExcept;

interface

{$I JCL.INC}

uses
  SysUtils;

//------------------------------------------------------------------------------
// Exception hooking notifiers routines
//------------------------------------------------------------------------------

type
  TJclExceptNotifyProc = procedure (ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean);
  TJclExceptNotifyMethod = procedure (ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean) of object;

  TJclExceptNotifyPriority = (npNormal, npFirstChain);

function JclAddExceptNotifier(const NotifyProc: TJclExceptNotifyProc; Priority: TJclExceptNotifyPriority {$IFDEF SUPPORTS_DEFAULTPARAMS} = npNormal {$ENDIF}): Boolean; overload;
function JclAddExceptNotifier(const NotifyMethod: TJclExceptNotifyMethod; Priority: TJclExceptNotifyPriority {$IFDEF SUPPORTS_DEFAULTPARAMS} = npNormal {$ENDIF}): Boolean; overload;

function JclRemoveExceptNotifier(const NotifyProc: TJclExceptNotifyProc): Boolean; overload;
function JclRemoveExceptNotifier(const NotifyMethod: TJclExceptNotifyMethod): Boolean;  overload;

procedure JclReplaceExceptObj(NewExceptObj: Exception);

//------------------------------------------------------------------------------
// Exception initialization routines
//------------------------------------------------------------------------------

function JclHookExceptions: Boolean;
function JclUnhookExceptions: Boolean;
function JclExceptionsHooked: Boolean;

implementation

uses
  Classes, Windows,
  JclBase, JclPeImage, JclSysUtils;

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

threadvar
  Recursive: Boolean;
  NewResultExc: Exception;

//==============================================================================
// TNotifierItem
//==============================================================================

constructor TNotifierItem.Create(const NotifyProc: TJclExceptNotifyProc; Priority: TJclExceptNotifyPriority);
begin
  FNotifyProc := NotifyProc;
  FPriority := Priority;
end;

//------------------------------------------------------------------------------

constructor TNotifierItem.Create(const NotifyMethod: TJclExceptNotifyMethod; Priority: TJclExceptNotifyPriority);
begin
  FNotifyMethod := NotifyMethod;
  FPriority := Priority;
end;

//------------------------------------------------------------------------------

procedure TNotifierItem.DoNotify(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean);
begin
  if Assigned(FNotifyProc) then
    FNotifyProc(ExceptObj, ExceptAddr, OSException)
  else
  if Assigned(FNotifyMethod) then
    FNotifyMethod(ExceptObj, ExceptAddr, OSException);
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

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

{$IFNDEF STACKFRAMES_ON} {$STACKFRAMES OFF} {$ENDIF}

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

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
        if (TMethod(O.FNotifyMethod).Code = TMethod(NotifyMethod).Code) and (TMethod(O.FNotifyMethod).Data = TMethod(NotifyMethod).Data) then
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

//------------------------------------------------------------------------------

procedure JclReplaceExceptObj(NewExceptObj: Exception);
begin
  Assert(Recursive);
  NewResultExc := NewExceptObj;
end;

//------------------------------------------------------------------------------

function JclHookExceptions: Boolean;
var
  RaiseExceptionAddress: Pointer;
begin
  if not ExceptionsHooked then
  begin
    Recursive := False;
    RaiseExceptionAddress := GetProcAddress(GetModuleHandle(kernel32), 'RaiseException');
    Assert(RaiseExceptionAddress <> nil);
    with TJclPeMapImgHooks do
      Result := ReplaceImport(SystemBase, kernel32, RaiseExceptionAddress, @HookedRaiseException);
    if Result then
    begin
      @Kernel32_RaiseException := RaiseExceptionAddress;
      SysUtils_ExceptObjProc := System.ExceptObjProc;
      System.ExceptObjProc := @HookedExceptObjProc;
    end;
    ExceptionsHooked := Result;
  end
  else
    Result := True;
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

function JclExceptionsHooked: Boolean;
begin
  Result := ExceptionsHooked;
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

initialization
  Notifiers := TThreadList.Create;

finalization
  FreeNotifiers;

end.
