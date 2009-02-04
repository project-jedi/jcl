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
{ The Original Code is JclNotify.pas.                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel Bestebroer.                                 }
{ Portions created by Marcel Bestebroer are Copyright Marcel Bestebroer. All rights reserved.      }
{                                                                                                  }
{ Contributors:                                                                                    }
{   -                                                                                              }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains generic JCL notification/listener pattern interfaces and base implementations }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclNotify;

{$I jcl.inc}

interface

uses
  JclBase,
  {$IFDEF THREADSAFE}
  JclSynch,
  {$ENDIF}
  Classes;

  { The following interfaces provide a basic notifier/listener setup. Whenever code issues a notification through the
    IJclNotifier.Notify method, all listeners registered with the notifier will receive the message (through the
    listener's Notification method). Since this setup doesn't care which or how many listeners are actually responding,
    it can greatly simplify code that need some form of notification. }
type
  // forward declarations
  IJclListener = interface;
  IJclNotificationMessage = interface;
  IJclNotifier = interface;

  IJclListener = interface
    ['{26A52ECC-4C22-4B71-BC88-D0EB98AF4ED5}']
    procedure Notification(msg: IJclNotificationMessage); stdcall;
  end;

  IJclNotificationMessage = interface
    ['{2618CCC6-0C7D-47EE-9A91-7A7F5264385D}']
  end;

  IJclNotifier = interface
    ['{CAAD7814-DD04-497C-91AC-558C2D5BFF81}']
    procedure Add(listener: IJclListener); stdcall;
    procedure Remove(listener: IJclListener); stdcall;
    procedure Notify(msg: IJclNotificationMessage); stdcall;
  end;

  { The following classes provide a basic notifier/listener implementation. Note that using one of these classes does
    not imply the usage of the related classes; the notifier can be used in conjection with any class implementing
    IJclListener and vice versa. }
type
  TJclBaseListener = class (TInterfacedObject, IJclListener)
  protected
    procedure Notification(msg: IJclNotificationMessage); virtual; stdcall; 
  end;

  TJclBaseNotificationMessage = class (TInterfacedObject, IJclNotificationMessage)
  end;

  TJclBaseNotifier = class (TInterfacedObject, IJclNotifier)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FListeners: TInterfaceList;
    {$IFDEF THREADSAFE}
    FSynchronizer: TJclMultiReadExclusiveWrite;
    {$ENDIF}
  protected
    procedure Add(listener: IJclListener); stdcall;
    procedure Notify(msg: IJclNotificationMessage); stdcall;
    procedure Remove(listener: IJclListener); stdcall;
  end;

implementation

uses
  SysUtils;

{ TJclBaseNotifier }

constructor TJclBaseNotifier.Create;
begin
  inherited Create;
  FListeners := TInterfaceList.Create;
  {$IFDEF THREADSAFE}
  FSynchronizer := TJclMultiReadExclusiveWrite.Create{$IFNDEF CLR}(mpReaders){$ENDIF !CLR};
  {$ENDIF}
end;

destructor TJclBaseNotifier.Destroy;
begin
  {$IFDEF THREADSAFE}
  FSynchronizer.BeginWrite;
  try
  {$ENDIF}
  FreeAndNil(FListeners);
  {$IFDEF THREADSAFE}
  finally
    FSynchronizer.EndWrite;
    FreeAndNil(FSynchronizer);
  end;
  {$ENDIF}
  inherited Destroy;
end;

procedure TJclBaseNotifier.Add(listener: IJclListener);
begin
  {$IFDEF THREADSAFE}
  FSynchronizer.BeginWrite;
  try
  {$ENDIF}
    if FListeners.IndexOf(listener) < 0 then
      FListeners.Add(listener);
  {$IFDEF THREADSAFE}
  finally
    FSynchronizer.EndWrite;
  end;
  {$ENDIF}
end;

procedure TJclBaseNotifier.Notify(msg: IJclNotificationMessage);
var
  idx: Integer;
begin
  {$IFDEF THREADSAFE}
  FSynchronizer.BeginRead;
  try
  {$ENDIF}
    for idx := 0 to FListeners.Count - 1 do
      IJclListener(FListeners[idx]).Notification(msg);
  {$IFDEF THREADSAFE}
  finally
    FSynchronizer.EndRead;
  end;
  {$ENDIF}
end;

procedure TJclBaseNotifier.Remove(listener: IJclListener);
var
  idx: Integer;
begin
  {$IFDEF THREADSAFE}
  FSynchronizer.BeginWrite;
  try
  {$ENDIF}
    idx := FListeners.IndexOf(listener);
    if idx < 0 then
      FListeners.Delete(idx);
  {$IFDEF THREADSAFE}
  finally
    FSynchronizer.EndWrite;
  end;
  {$ENDIF}
end;

{ TJclBaseListener }

procedure TJclBaseListener.Notification(msg: IJclNotificationMessage);
begin
  // do nothing; descendants should override this method to process incoming notifications
end;

end.
