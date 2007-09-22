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
{ The Original Code is AbstractContainer.pas and DCL_Util.pas.                                     }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Daniele Teti (dade2004)                                                                        }
{   Robert Marquardt (marquardt)                                                                   }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ The Delphi Container Library                                                                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclAbstractContainers;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF CLR}
  System.Threading,
  {$ELSE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$ENDIF CLR}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  SyncObjs,
  SysUtils, Classes, JclBase, JclContainerIntf, JclSysUtils;

type
  {$IFDEF KEEP_DEPRECATED}
  TJclIntfCriticalSection = JclSysUtils.TJclIntfCriticalSection;
  {$ENDIF KEEP_DEPRECATED}

  TJclAbstractContainer = class(TInterfacedObject {$IFDEF THREADSAFE}, IJclLockable {$ENDIF THREADSAFE})
  {$IFDEF THREADSAFE}
  private
    FLockDelegate: IJclLockable;
    {$IFDEF CLR}
    FReaderWriterLock: ReaderWriterLock;
    FUpgradedWrite: Boolean;
    FLockCookie: LockCookie;
    {$ELSE ~CLR}
    FCriticalSection: TCriticalSection;
    {$ENDIF ~CLR}
  protected
    procedure ReadLock;
    procedure ReadUnlock;
    procedure WriteLock;
    procedure WriteUnlock;
  public
    destructor Destroy; override;
  {$ENDIF THREADSAFE}
  public
    constructor Create(ALockDelegate: IInterface);
  end;

  TJclAbstractIterator = class(TJclAbstractContainer {$IFDEF THREADSAFE}, IJclLockable {$ENDIF THREADSAFE})
  private
    FValid: Boolean;
  public
    property Valid: Boolean read FValid write FValid;
  end;

  TJclStrCollection = class(TJclAbstractContainer, IJclStrCollection {$IFDEF THREADSAFE},IJclLockable{$ENDIF THREADSAFE})
  protected
    { IJclStrCollection }
    function Add(const AString: string): Boolean; virtual; abstract;
    function AddAll(const ACollection: IJclStrCollection): Boolean; virtual; abstract;
    procedure Clear; virtual; abstract;
    function Contains(const AString: string): Boolean; virtual; abstract;
    function ContainsAll(const ACollection: IJclStrCollection): Boolean; virtual; abstract;
    function Equals(const ACollection: IJclStrCollection): Boolean; virtual; abstract;
    function First: IJclStrIterator; virtual; abstract;
    function IsEmpty: Boolean; virtual; abstract;
    function Last: IJclStrIterator; virtual; abstract;
    function Remove(const AString: string): Boolean; overload; virtual; abstract;
    function RemoveAll(const ACollection: IJclStrCollection): Boolean; virtual; abstract;
    function RetainAll(const ACollection: IJclStrCollection): Boolean; virtual; abstract;
    function Size: Integer; virtual; abstract;
    procedure LoadFromStrings(Strings: TStrings);
    procedure SaveToStrings(Strings: TStrings);
    procedure AppendToStrings(Strings: TStrings);
    procedure AppendFromStrings(Strings: TStrings);
    function GetAsStrings: TStrings;
    function GetAsDelimited(const Separator: string = AnsiLineBreak): string;
    procedure AppendDelimited(const AString: string; const Separator: string = AnsiLineBreak);
    procedure LoadDelimited(const AString: string; const Separator: string = AnsiLineBreak);
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

//=== { TJclAbstractContainer } ==============================================

constructor TJclAbstractContainer.Create(ALockDelegate: IInterface);
begin
  inherited Create;
  {$IFDEF THREADSAFE}
  FLockDelegate := ALockDelegate as IJclLockable;
  if FLockDelegate = nil then
    {$IFDEF CLR}
    FReaderWriterLock := ReaderWriterLock.Create;
    {$ELSE ~CLR}
    FCriticalSection := TCriticalSection.Create;
    {$ENDIF ~CLR}
  {$ENDIF THREADSAFE}
end;

{$IFDEF THREADSAFE}

destructor TJclAbstractContainer.Destroy;
begin
  {$IFDEF CLR}
  FReaderWriterLock.Free;
  {$ELSE ~CLR}
  FCriticalSection.Free;
  {$ENDIF ~CLR}
  FLockDelegate := nil;
  inherited Destroy;
end;

procedure TJclAbstractContainer.ReadLock;
begin
  if FLockDelegate <> nil then
    FLockDelegate.ReadLock
  else
    {$IFDEF CLR}
    // if current thread has write access, no need to request a read access
    if not FReaderWriterLock.IsWriterLockHeld then
      FReaderWriterLock.AcquireReaderLock(-1);
    {$ELSE ~CLR}
    FCriticalSection.Acquire;
    {$ENDIF ~CLR}
end;

procedure TJclAbstractContainer.ReadUnlock;
begin
  if FLockDelegate <> nil then
    FLockDelegate.ReadUnlock
  else
    {$IFDEF CLR}
    // if current thread has write access, no need to release read access
    if not FReaderWriterLock.IsWriterLockHeld then
      FReaderWriterLock.ReleaseReaderLock;
    {$ELSE ~CLR}
    FCriticalSection.Release;
    {$ENDIF ~CLR}
end;

procedure TJclAbstractContainer.WriteLock;
begin
  if FLockDelegate <> nil then
    FLockDelegate.WriteLock
  else
    {$IFDEF CLR}
    if FReaderWriterLock.IsReaderLockHeld then
    begin
      FLockCookie := FReaderWriterLock.UpgradeToWriterLock(-1);
      FUpgradedWrite := True;
    end
    else
      FReaderWriterLock.AcquireWriterLock(-1);
    {$ELSE ~CLR}
    FCriticalSection.Acquire;
    {$ENDIF ~CLR}
end;

procedure TJclAbstractContainer.WriteUnlock;
begin
  if FLockDelegate <> nil then
    FLockDelegate.WriteUnlock
  else
    {$IFDEF CLR}
    if FUpgradedWrite then
    begin
      FUpgradedWrite := False;
      FReaderWriterLock.DowngradeFromWriterLock(FLockCookie);
    end
    else
      FReaderWriterLock.ReleaseWriterLock;
    {$ELSE ~CLR}
    FCriticalSection.Release;
    {$ENDIF ~CLR}
end;

{$ENDIF THREADSAFE}

//=== { TJclStrCollection } ==================================================

procedure TJclStrCollection.AppendDelimited(const AString, Separator: string);
{$IFDEF CLR}
var
  I, StartIndex: Integer;
begin
  I := Pos(Separator, AString);
  if I <> 0 then
  begin
    Dec(I); // to .NET string index base
    StartIndex := 0;
    repeat
      Add(AString.Substring(StartIndex, I - StartIndex + 1));
      StartIndex := I + 1;
      I := AString.IndexOf(Separator, StartIndex);
    until I < 0;
  end
  else
    Add(AString);
end;
{$ELSE}
var
  Item: string;
  SepLen: Integer;
  PString, PSep, PPos: PChar;
begin
  PString := PChar(AString);
  PSep := PChar(Separator);
  PPos := StrPos(PString, PSep);
  if PPos <> nil then
  begin
    SepLen := StrLen(PSep);
    repeat
      //SetLength(Item, PPos - PString + 1);
      SetLength(Item, PPos - PString);
      Move(PString^, Item[1], PPos - PString);
      //Item[PPos - PString + 1] := #0;
      Add(Item);
      PString := PPos + SepLen;
      PPos := StrPos(PString, PSep);
    until PPos = nil;
    if StrLen(PString) > 0 then //ex. hello#world
      Add(PString);
  end
  else //There isnt a Separator in AString
    Add(AString);
end;
{$ENDIF CLR}

procedure TJclStrCollection.AppendFromStrings(Strings: TStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    Add(Strings[I]);
end;

procedure TJclStrCollection.AppendToStrings(Strings: TStrings);
var
  It: IJclStrIterator;
begin
  It := First;
  Strings.BeginUpdate;
  try
    while It.HasNext do
      Strings.Add(It.Next);
  finally
    Strings.EndUpdate;
  end;
end;

function TJclStrCollection.GetAsDelimited(const Separator: string): string;
var
  It: IJclStrIterator;
begin
  It := First;
  Result := '';
  if It.HasNext then
    Result := It.Next;
  while It.HasNext do
    Result := Result + Separator + It.Next;
end;

function TJclStrCollection.GetAsStrings: TStrings;
begin
  Result := TStringList.Create;
  try
    AppendToStrings(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TJclStrCollection.LoadDelimited(const AString, Separator: string);
begin
  Clear;
  AppendDelimited(AString, Separator);
end;

procedure TJclStrCollection.LoadFromStrings(Strings: TStrings);
begin
  Clear;
  AppendFromStrings(Strings);
end;

procedure TJclStrCollection.SaveToStrings(Strings: TStrings);
begin
  Strings.Clear;
  AppendToStrings(Strings);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

