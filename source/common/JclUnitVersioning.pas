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
{ The Original Code is JclUnitVersioning.pas.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Andreas Hausladen.                                 }
{ Portions created by Andreas Hausladen are Copyright (C) Andreas Hausladen. All rights reserved.  }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Andreas Hausladen (ahuser)                                                                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ A unit version information system. That collects information form prepared units by each module. }
{ It also works with units in DLLs.                                                                }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclUnitVersioning;

{$I jedi.inc}

interface

{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF MSWINDOWS}
{$IFDEF HAS_UNIT_LIBC}
uses
  Libc;
{$ENDIF HAS_UNIT_LIBC}

type
  PUnitVersionInfo = ^TUnitVersionInfo;
  {$IFDEF BCB}
  TUnitVersionInfo = record
    Next: PUnitVersionInfo;
  {$ELSE}
  TUnitVersionInfo = object
  private
    Next: PUnitVersionInfo;
  public
  {$ENDIF BCB}
    RCSfile: string;  // $'RCSfile$
    Revision: string; // $'Revision$
    Date: string;     // $'Date$     in UTC (GMT)
    LogPath: string;  // logical file path
    Extra: string;    // user defined string
    Data: Pointer;    // user data
    {$IFNDEF BCB}
    function GetRCSfile: string;
    function GetRevision: string;
    function GetDate: string;
    {$ENDIF ~BCB}
  end;

  TUnitVersioningModule = class(TObject)
  private
    FInstance: THandle;
    FHead, FTail: PUnitVersionInfo;
    FCount: Integer;
    function GetItems(Index: Integer): PUnitVersionInfo;

    procedure Add(Info: PUnitVersionInfo);
  public
    property Instance: THandle read FInstance;

    property Count: Integer read FCount;
    property Items[Index: Integer]: PUnitVersionInfo read GetItems; default;
  end;

  TUnitVersioning = class(TObject)
  private
    FModules: array of TUnitVersioningModule;
    FModuleCount: Integer;
    function GetItems(Index: Integer): PUnitVersionInfo;
    function GetModuleCount: Integer;
    function GetModules(Index: Integer): TUnitVersioningModule;
    function GetCount: Integer;

    procedure ValidateModules;
    // These two methods must be virtual because they can be invoked by a DLL.
    // And static linking would mean that the DLL's TUnitVersioning methods
    // handle the call.
    procedure Add(Instance: THandle; Info: PUnitVersionInfo); virtual;
    procedure UnregisterModule(Instance: THandle); virtual;
  public
    destructor Destroy; override;

    // units by modules
    property ModuleCount: Integer read GetModuleCount;
    property Modules[Index: Integer]: TUnitVersioningModule read GetModules;

    // all units
    property Count: Integer read GetCount;
    property Items[Index: Integer]: PUnitVersionInfo read GetItems; default;
  end;

procedure RegisterUnitVersion(hInstance: THandle; const CvsVersionString: TUnitVersionInfo);
procedure UnregisterUnitVersion(hInstance: THandle);

function GetUnitVersioning: TUnitVersioning;

implementation

{$IFNDEF BCB}

{ TUnitVersionInfo }

function TUnitVersionInfo.GetRCSfile: string;
var
  I: Integer;
begin
  Result := RCSfile;
  Delete(Result, 1, 10);
  Delete(Result, Length(Result) - 1, 2);
  for I := Length(Result) downto 1 do
    if Result[I] = ',' then
    begin
      Delete(Result, I, MaxInt);
      Break;
    end;
end;

function TUnitVersionInfo.GetRevision: string;
begin
  Result := Copy(Revision, 12, Length(Revision) - 11 - 2);
end;

function TUnitVersionInfo.GetDate: string;
begin
  Result := Date;
  Delete(Result, 1, 7);
  Delete(Result, Length(Result) - 1, 2);
end;

{$ENDIF ~BCB}

{ TUnitVersioningModule }

function TUnitVersioningModule.GetItems(Index: Integer): PUnitVersionInfo;
begin
  Result := FHead;
  while Result <> nil do
  begin
    if Index = 0 then
      Exit;
    Dec(Index);
    Result := Result.Next;
  end;
end;

procedure TUnitVersioningModule.Add(Info: PUnitVersionInfo);
begin
  Inc(FCount);
  if FHead = nil then
  begin
    FHead := Info;
    FTail := FHead;
  end
  else
  begin
    FTail.Next := Info;
    FTail := FTail.Next;
  end;
end;

{ TUnitVersioning }

destructor TUnitVersioning.Destroy;
var
  I: Integer;
begin
  for I := 0 to FModuleCount - 1 do
    FModules[I].Free;
  inherited Destroy;
end;

procedure TUnitVersioning.Add(Instance: THandle; Info: PUnitVersionInfo);
var
  I: Integer;
begin
  for I := 0 to FModuleCount - 1 do
  begin
    if FModules[I].Instance = Instance then
    begin
      FModules[I].Add(Info);
      Exit;
    end;
  end;
  if FModuleCount + 1 > Length(FModules) then
    SetLength(FModules, FModuleCount + 5);
  FModules[FModuleCount] := TUnitVersioningModule.Create;
  FModules[FModuleCount].FInstance := Instance;
  FModules[FModuleCount].Add(Info);
  Inc(FModuleCount);
end;

procedure TUnitVersioning.UnregisterModule(Instance: THandle);
var
  I: Integer;
begin
  for I := 0 to FModuleCount - 1 do
  begin
    if FModules[I].Instance = Instance then
    begin
      FModules[I].Free;
      if I < FModuleCount - 1 then
        Move(FModules[I + 1], FModules[I], (FModuleCount - I) * SizeOf(TUnitVersioningModule));
      Dec(FModuleCount);
      Break;
    end;
  end;
  if Length(FModules) > FModuleCount + 5 then
    SetLength(FModules, FModuleCount);
end;

function TUnitVersioning.GetCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  ValidateModules;
  for I := 0 to FModuleCount - 1 do
    Inc(Result, FModules[I].Count);
end;

function TUnitVersioning.GetItems(Index: Integer): PUnitVersionInfo;
var
  Cnt, I: Integer;
begin
  Result := nil;
  ValidateModules;
  Cnt := 0;
  for I := 0 to FModuleCount - 1 do
  begin
    if Index < Cnt + FModules[I].Count then
    begin
      Result := FModules[I].Items[Index - Cnt];
      Break;
    end;
    Inc(Cnt, FModules[I].Count);
  end;
end;

function TUnitVersioning.GetModuleCount: Integer;
begin
  ValidateModules;
  Result := FModuleCount;
end;

function TUnitVersioning.GetModules(Index: Integer): TUnitVersioningModule;
begin
  Result := FModules[Index];
end;

procedure TUnitVersioning.ValidateModules;
var
  I: Integer;
  Buffer: string;
begin
  for I := FModuleCount - 1 downto 0 do
  begin
    SetLength(Buffer, 1024);
    if GetModuleFileName(FModules[I].Instance, PChar(Buffer), 1024) = 0 then
      // This module is no more in memory but has not unregistered itself so
      // unregister it here.
      UnregisterModule(FModules[I].Instance);
  end;
end;

{******************************************************************************}
function GetNamedProcessAddress(const Id: ShortString; out RefCount: Integer): Pointer; forward;
  // Returns a 3820 Bytes large block [= 4096 - 276 = 4096 - (8+256+4+8)]
  // max 20 blocks can be allocated
function ReleaseNamedProcessAddress(P: Pointer): Integer; forward;


{$IFDEF UNIX}
const
  PAGE_OFFSET = $C0000000; // from linux/include/asm-i386/page.h
{$ENDIF UNIX}

const
  Signature1 = $ABCDEF0123456789;
  Signature2 = $9876543210FEDCBA;

type
  PNPARecord = ^TNPARecord;
  TNPARecord = record
    Signature1: Int64;
    Id: ShortString;
    RefCount: Integer;
    Signature2: Int64;
    Data: record end;
  end;

function GetNamedProcessAddress(const Id: ShortString; out RefCount: Integer): Pointer;
const
  MaxPages = 20;
var
  {$IFDEF MSWINDOWS}
  SysInfo: TSystemInfo;
  {$ENDIF MSWINDOWS}
  Requested, Allocated: PNPARecord;
  Pages: Integer;
  pid: Integer;
  PageSize: Cardinal;
  MaximumApplicationAddress: Pointer;
begin
  RefCount := 0;
  {$IFDEF MSWINDOWS}
  pid := GetCurrentProcessId;
  GetSystemInfo(SysInfo);
  PageSize := SysInfo.dwPageSize;
  MaximumApplicationAddress := SysInfo.lpMaximumApplicationAddress;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  PageSize := getpagesize;
  pid := getpid;
  MaximumApplicationAddress := Pointer(PAGE_OFFSET - 1);
  {$ENDIF UNIX}
  Pages := 0;
  repeat
    Requested := MaximumApplicationAddress;
    Requested := Pointer((Cardinal(Requested) div $10000) * $10000);
    Dec(Cardinal(Requested), Pages * $10000);
    Requested := Pointer((Cardinal(Requested) div PageSize) * PageSize);
    {$IFDEF MSWINDOWS}
    Allocated := VirtualAlloc(Requested, PageSize, MEM_RESERVE or MEM_COMMIT, PAGE_READWRITE);
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    // Do not use MAP_FIXED because it replaces the already allocated map by a
    // new map.
    Allocated := mmap(Requested, PageSize, PROT_READ or PROT_WRITE,
      MAP_PRIVATE or MAP_ANONYMOUS, 0, 0);
    if Allocated = MAP_FAILED then
    begin
      // Prevent SEGV by signature-test code and try the next memory page.
      Inc(Pages);
      Continue;
    end
    else
    if Allocated <> Requested then
    begin
      // It was relocated, means the requested address is already allocated
      munmap(Allocated, PageSize);
      Allocated := nil;
    end;
    {$ENDIF UNIX}

    if Assigned(Allocated) then
    begin
      //if Requested = Allocated then
        Break; // new block allocated
    end
    else
    begin
      if (Requested.Signature1 = Signature1 xor pid) and
         (Requested.Signature2 = Signature2 xor pid) and
         (Requested.Id = Id) then
        Break; // Found correct, already existing block.
    end;

    Inc(Pages);
  until Pages > MaxPages;

  Result := nil;
  if Allocated <> nil then
  begin
    if Requested = Allocated then
    begin
      // initialize the block
      Requested.Signature1 := Signature1 xor pid;
      Requested.Id := Id;
      Requested.Signature2 := Signature2 xor pid;
      Requested.RefCount := 1;
      Result := @Requested.Data;
      RefCount := 1;
    end;
  end
  else
  begin
    if (Requested.Signature1 = Signature1 xor pid) and
       (Requested.Signature2 = Signature2 xor pid) and
       (Requested.Id = Id) then
    begin
      Inc(Requested.RefCount);
      Result := @Requested.Data;
      RefCount := Requested.RefCount;
    end;
  end;
end;

function ReleaseNamedProcessAddress(P: Pointer): Integer;
var
  Requested: PNPARecord;
begin
  Result := 0;
  if P <> nil then
  begin
    Requested := PNPARecord(Cardinal(P) - SizeOf(TNPARecord));
    Dec(Requested.RefCount);
    Result := Requested.RefCount;
    if Requested.RefCount = 0 then
      {$IFDEF MSWINDOWS}
      VirtualFree(Requested, 0, MEM_DECOMMIT or MEM_RELEASE);
      {$ENDIF MSWINDOWS}
      {$IFDEF UNIX}
      munmap(Requested, getpagesize);
      {$ENDIF UNIX}
  end;
end;

{******************************************************************************}

var
  UnitVersioningOwner: Boolean = False;
  GlobalUnitVersioning: TUnitVersioning = nil;

type
  PUnitVersioning = ^TUnitVersioning;

var
  UnitVersioningNPA: PUnitVersioning = nil;

function GetUnitVersioning: TUnitVersioning;
var
  RefCount: Integer;
begin
  if GlobalUnitVersioning = nil then
  begin
    UnitVersioningNPA := GetNamedProcessAddress('UnitVersioning', RefCount);
    if UnitVersioningNPA <> nil then
    begin
      GlobalUnitVersioning := UnitVersioningNPA^;
      if (GlobalUnitVersioning = nil) or (RefCount = 1) then
      begin
        GlobalUnitVersioning := TUnitVersioning.Create;
        UnitVersioningNPA^ := GlobalUnitVersioning;
        UnitVersioningOwner := True;
      end;
    end
    else
    begin
      GlobalUnitVersioning := TUnitVersioning.Create;
      UnitVersioningOwner := True;
    end;
  end
  else
  if UnitVersioningNPA <> nil then
    GlobalUnitVersioning := UnitVersioningNPA^; // update (maybe the owner has destroyed the instance)
  Result := GlobalUnitVersioning;
end;

procedure FinalizeUnitVersioning;
var
  RefCount: Integer;
begin
  try
    if GlobalUnitVersioning <> nil then
    begin
      RefCount := ReleaseNamedProcessAddress(UnitVersioningNPA);
      if UnitVersioningOwner then
      begin
        if RefCount > 0 then
          UnitVersioningNPA^ := nil;
        GlobalUnitVersioning.Free;
      end;
      GlobalUnitVersioning := nil;
    end;
  except
    // ignore
  end;
end;

procedure RegisterUnitVersion(hInstance: THandle; const CvsVersionString: TUnitVersionInfo);
var
  UnitVersioning: TUnitVersioning;
begin
  UnitVersioning := GetUnitVersioning;
  if Assigned(UnitVersioning) then
    UnitVersioning.Add(hInstance, @CvsVersionString);
end;

procedure UnregisterUnitVersion(hInstance: THandle);
var
  UnitVersioning: TUnitVersioning;
begin
  UnitVersioning := GetUnitVersioning;
  if Assigned(UnitVersioning) then
    UnitVersioning.UnregisterModule(hInstance);
end;


initialization

finalization
  FinalizeUnitVersioning;

// History:

// $Log$
// Revision 1.1  2004/09/01 14:56:16  ahuser
// Added common/JclUnitVersioning.pas
//

end.

