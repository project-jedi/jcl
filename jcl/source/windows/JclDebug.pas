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
{ The Original Code is JclDebug.pas.                                           }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ 2000 of these individuals.                                                   }
{                                                                              }
{ Last modified: June 21, 2000                                                 }
{                                                                              }
{******************************************************************************}

// (rom)   added local const AnsiSpace to replace #32
// (rom)   changed the class prefix from T to TJcl and E to EJcl

unit JclDebug;

interface

{$I JCL.INC}

uses
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  Classes,
  JclBase;

//------------------------------------------------------------------------------
// Crash
//------------------------------------------------------------------------------

function EnableCrashOnCtrlScroll(const Enable: Boolean): Boolean;

//------------------------------------------------------------------------------
// Diagnostics
//------------------------------------------------------------------------------

procedure AssertKindOf(const ClassName: string; const Obj: TObject); overload;
procedure AssertKindOf(const ClassType: TClass; const Obj: TObject); overload;

procedure Trace(const Msg: string);
procedure TraceFmt(const Fmt: string; const Args: array of const);
procedure TraceLoc(const Msg: string);
procedure TraceLocFmt(const Fmt: string; const Args: array of const);

//------------------------------------------------------------------------------
// Source Locations
//------------------------------------------------------------------------------

{ TMapFiles }

type
  PSection = ^TSection;
  TSection = record
    ID: Integer;
    Start: Integer;
    Size: Integer;
    Name: string;
    SectionClass: string;
  end;

  PLine = ^TLine;
  TLine = record
    LineNumber: Integer;
    Start: Integer;
  end;

  PUnit = ^TUnit;
  TUnit = record
    UnitName: string;
    Filename: string;
    Start: Integer;
    Size: Integer;
    Lines: TList;
  end;

  PPublic = ^TPublic;
  TPublic = record
    Start: Integer;
    Name: string;
  end;

  TJclMap = class (TObject)
  private
    FFilename: string;
    FSections: TList;
    FUnits: TList;
    FPublics: TList;
    FModuleHandle: THandle;
  protected
    procedure LoadUnitInformation(const Lines: TStrings; const LineNumber: Integer);
    procedure LoadSections(const Lines: TStrings);
    procedure LoadUnits(const Lines: TStrings);
    procedure LoadPublics(const Lines: TStrings);
    procedure GetUnitRange(const Lines: TStrings; var UnitInfo: TUnit);
    function GetPublicName(const Location: Pointer): string;
    procedure Load; virtual;
    procedure Clear; virtual;
  public
    constructor Create(const Filename: string; ModuleHandle: THandle);
    destructor Destroy; override;
    function Lookup(const Location: Pointer; var ModuleName, ProcName,
      Filename: string; var LineNumber: Integer): Boolean; virtual;
  end;

const
  USE_CURRENT_PROCESS = INVALID_HANDLE_VALUE;

type
  PModule = ^TModule;
  TModule = record
    Handle: THandle;            // handle to this module
    BaseAddress: Pointer;       // base address that this module is loaded on
    Size: LongWord;             // size of image in memory, measured from base
    Filename: string;           // name of actual module loaded
    Map: TJclMap;               // is nil if module has no map file
  end;

  PLocationInfo = ^TLocationInfo;
  TLocationInfo = record
    Location: Pointer;          // always set to the location passed
    Module: PModule;            // is nil if not known
    ModuleName: string;         // Name of Delphi module; is empty if not known
    ProcName: string;           // Name of procedure; is empty if not known
    LineNumber: Integer;        // Line number of location; is 0 if not known
    SourceName: string;         // Name of source-code file; is empty if not known
  end;

  TJclMapFiles = class (TObject)
  private
    FModules: TList;
    FProcessHandle: THandle;
  protected
    procedure Load; virtual;
    function ModuleOf(const Location: Pointer): PModule; virtual;
  public
    constructor Create(AProcessHandle: THandle {$IFDEF SUPPORTS_DEFAULTPARAMS} = USE_CURRENT_PROCESS {$ENDIF});
    destructor Destroy; override;
    procedure Refresh;
    function GetLocationInfo(const Location: Pointer): TLocationInfo; virtual;
  end;

  EJclMapFilesError = class (EJclError);

function Caller(Level: Integer): PChar;
function ExceptionAddr: Pointer;
function StackFrame(Level: Integer): PChar;

function __FILE__(const Level: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF}): string;
function __MODULE__(const Level: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF}): string;
function __PROC__(const Level: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF}): string;
function __LINE__(const Level: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF}): Integer;

function __FILE_OF_ADDR__(const Addr: Pointer): string;
function __MODULE_OF_ADDR__(const Addr: Pointer): string;
function __PROC_OF_ADDR__(const Addr: Pointer): string;
function __LINE_OF_ADDR__(const Addr: Pointer): Integer;

implementation

uses
  SysUtils,
  {$IFDEF JCL_DEBUG_EXTENSION}
  ZLib,
  {$ENDIF}
  JclRegistry, JclResources, JclSysInfo, JclSysUtils, JclWin32;

//==============================================================================
// Crash
//==============================================================================

function EnableCrashOnCtrlScroll(const Enable: Boolean): Boolean;
const
  CrashCtrlScrollKey = 'System\CurrentControlSet\Services\i8042prt\Parameters';
  CrashCtrlScrollName = 'CrashOnCtrlScroll';
var
  Enabled: Integer;
begin
  Enabled := 0;
  if Enable then
    Enabled := 1;
  RegWriteInteger(HKEY_LOCAL_MACHINE, CrashCtrlScrollKey, CrashCtrlScrollName, Enabled);
  Result := RegReadInteger(HKEY_LOCAL_MACHINE, CrashCtrlScrollKey, CrashCtrlScrollName) = Enabled;
end;

//==============================================================================
// Diagnostics
//==============================================================================

procedure AssertKindOf(const ClassName: string; const Obj: TObject); overload;
var
  C: TClass;
begin
  if not Obj.ClassNameIs(ClassName) then
  begin
    C := Obj.ClassParent;
    while (C <> nil) and (not C.ClassNameIs(ClassName)) do
      C := C.ClassParent;
    Assert(C <> nil);
  end;
end;

//------------------------------------------------------------------------------

procedure AssertKindOf(const ClassType: TClass; const Obj: TObject); overload;
begin
  Assert(Obj.InheritsFrom(ClassType));
end;

//------------------------------------------------------------------------------

procedure Trace(const Msg: string);
begin
  OutputDebugString(PChar('"' + Msg + '"'));
end;

//------------------------------------------------------------------------------

procedure TraceFmt(const Fmt: string; const Args: array of const);
begin
  OutputDebugString(PChar(Format('"' + Fmt + '"', Args)));
end;

//------------------------------------------------------------------------------

procedure TraceLoc(const Msg: string);
begin
  OutputDebugString(PChar(Format('%s:%u (%s) "%s"', [__FILE__(1), __LINE__(1),
    __PROC__(1), Msg])));
end;

//------------------------------------------------------------------------------

procedure TraceLocFmt(const Fmt: string; const Args: array of const);
var
  S: string;
begin
  S := Format('%s:%u (%s) ', [__FILE__(1), __LINE__(1), __PROC__(1)]) +
    Format('"' + Fmt + '"', Args);
  OutputDebugString(PChar(S));
end;

//==============================================================================
// Global Constants
//==============================================================================

// Defines the identifying strings inside the map file that augment the start
// of a section

const
  SModuleStart  = 'Line numbers for ';
  SSegmentStart = 'Detailed map of segments';
  SCodeSegment  = 'S=.text';
  SPublicsStart = '  Address         Publics by Value';

  // (rom) from JclStrings
  AnsiSpace = AnsiChar(' ');

//==============================================================================
// Global Utility Functions
//==============================================================================

// Returns the substring from S defined by 1..FirstOccurenceOf(#32). If #32
// does not exist in S then First returns S.

function First(const S: string): string;
begin
  if Pos(AnsiSpace, S) = 0 then
    Result := S
  else
    Result := Copy(S, 1, Pos(Ansispace, S) - 1);
end;

//------------------------------------------------------------------------------

// Returns the substring from S defined by FirstOccurenceOf(#32)..Length(S). If
// #32 does not exist in S then ButFirst returns S.

function ButFirst(const S: string): string;
begin
  if Pos(AnsiSpace, S) = 0 then
    Result := ''
  else
    Result := Copy(S, Pos(AnsiSpace, S) + 1, Length(S));
end;

{$IFDEF JCL_DEBUG_EXTENSION}

const
  JclDebugResName = 'JCLDEBUG';

function CheckJclDebugInfo(const FileName: string; Module: THandle): Boolean;
begin
  Result := FileExists(ChangeFileExt(FileName, RsDebugMapFileExtension)) or
    (FindResource(Module, JclDebugResName, RT_RCDATA) <> 0);
end;

{$ENDIF}

//==============================================================================
// TJclMapFiles
//==============================================================================

constructor TJclMapFiles.Create(AProcessHandle: THandle);
begin
  inherited Create;
  FModules := TList.Create;
  FProcessHandle := AProcessHandle;
  Load;
end;

//------------------------------------------------------------------------------

destructor TJclMapFiles.Destroy;
var
  I: Integer;
begin
  for I := 0 to FModules.Count - 1 do
  begin
    PModule(FModules[I])^.Map.Free;
    Dispose(PModule(FModules[I]));
    FModules[I] := nil;
  end;
  FModules.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TJclMapFiles.GetLocationInfo(const Location: Pointer): TLocationInfo;
var
  ModuleLocation: Pointer;
begin
  // Get basic values
  Result.Location := Location;
  Result.Module := ModuleOf(Result.Location);
  Assert(Result.Module <> nil, RsDebugNoProcessInfo);
  // Default values
  Result.ModuleName := '';
  Result.ProcName := '';
  Result.SourceName := '';
  Result.LineNumber := 0;
  // Now try to find specific values
  if Result.Module.Map <> nil then
  begin
    ModuleLocation := Pointer(Integer(Result.Location) -
      Integer(Result.Module.BaseAddress) - $1000);
    if not Result.Module.Map.Lookup(ModuleLocation, Result.ModuleName,
      Result.ProcName, Result.SourceName, Result.LineNumber) then
    begin
      Result.ModuleName := '';
      Result.ProcName := '';
      Result.SourceName := '';
      Result.LineNumber := 0;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclMapFiles.Load;
var
  PID: THandle;

  procedure EnumModulesPsApi(PID: LongWord);
  type
    PModuleArray = ^TModuleArray;
    TModuleArray = array [0..32767] of THandle;
  var
    Needed: LongWord;
    Modules: PModuleArray;
    Index: LongWord;
    Filename: array [0..1023] of Char;
    ModuleInfo: TModuleInfo;
    M: PModule;
  begin
    if EnumProcessModules(PID, nil, 0, Needed) then
    begin
      GetMem(Modules, Needed);
      try
        if EnumProcessModules(PID, @(Modules^[0]), Needed, Needed) then
        begin
          //SetLength(FModules, Needed div 4);
          Index := 0;
          while Index < (Needed div 4) do
          begin
            // Retrieve information about the module
            if (GetModuleFileNameEx(PID, Modules^[Index], Filename, SizeOf(Filename)) > 0) and // PSAPI
               (GetModuleInformation(PID, Modules^[Index], @ModuleInfo, SizeOf(ModuleInfo))) then // PSAPI
            begin
              M := AllocMem(SizeOf(TModule));
              FModules.Add(M);
              with M^ do
              begin
                Handle := Modules^[Index];
                BaseAddress := ModuleInfo.lpBaseOfDll;
                Size := ModuleInfo.SizeOfImage;
                Filename := Filename;
                {$IFDEF JCL_DEBUG_EXTENSION}
                if CheckJclDebugInfo(Filename, Handle) then
                  Map := TJclMap.Create(ChangeFileExt(Filename, RsDebugMapFileExtension), Handle)
                else
                  Map := nil;
                {$ELSE}
                if FileExists(ChangeFileExt(Filename, RsDebugMapFileExtension)) then
                  Map := TJclMap.Create(ChangeFileExt(Filename, RsDebugMapFileExtension), 0)
                else
                  Map := nil;
                {$ENDIF}
              end;
            end;
            Inc(Index);
          end;
        end;
      finally
        FreeMem(Modules);
      end;
    end;
  end;

  procedure EnumModulesTLHelp32(PID: LongWord);
  var
    Snapshot: THandle;
    Module32: TModuleEntry32;
    GotEntry: Boolean;
    M: PModule;
  begin
    Snapshot := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, PID);
    if Snapshot = $FFFFFFFF then
      raise EJclMapFilesError.CreateResRec(@RsDebugSnapshot);
    try
      FillChar(Module32, SizeOf(Module32), #0);
      Module32.dwSize := SizeOf(Module32);
      GotEntry := Module32First(Snapshot, Module32);
      while GotEntry do
      begin
        //SetLength(FModules, Length(FModules) + 1);
        //with FModules[Length(FModules) - 1] do
        M := AllocMem(SizeOf(TModule));
        FModules.Add(M);
        with M^ do
        begin
          Handle := Module32.hModule;
          BaseAddress := Module32.modBaseAddr;
          Size := Module32.modBaseSize;
          FileName := Module32.szExePath;
          {$IFDEF JCL_DEBUG_EXTENSION}
          if CheckJclDebugInfo(FileName, Handle) then
            Map := TJclMap.Create(ChangeFileExt(FileName, RsDebugMapFileExtension), Handle)
          else
            Map := nil;
          {$ELSE}
          if FileExists(ChangeFileExt(FileName, RsDebugMapFileExtension)) then
            Map := TJclMap.Create(ChangeFileExt(FileName, RsDebugMapFileExtension), 0)
          else
            Map := nil;
          {$ENDIF}
        end;
        GotEntry := Module32Next(Snapshot, Module32);
      end;
    finally
      CloseHandle(Snapshot);
    end;
  end;

begin
  // (rom) if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MinorVersion = 4) then
  if GetWindowsVersion in [wvWinNT3, wvWinNT4] then
  begin
    if FProcessHandle = USE_CURRENT_PROCESS then
      EnumModulesPsApi(GetCurrentProcess)
    else
    begin
      PID := OpenProcess(PROCESS_ALL_ACCESS, False, FProcessHandle);
      if PID <> 0 then
        try
          EnumModulesPsApi(PID);
        finally
          CloseHandle(PID);
        end;
    end;
  end
  else
  begin
    if FProcessHandle = USE_CURRENT_PROCESS then
      EnumModulesTLHelp32(GetCurrentProcessId)
    else
    begin
      PID := OpenProcess(PROCESS_ALL_ACCESS, False, FProcessHandle);
      if PID <> 0 then
        try
          EnumModulesTLHelp32(PID);
        finally
          CloseHandle(PID);
        end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TJclMapFiles.ModuleOf(const Location: Pointer): PModule;
var
  I: Integer;
  M: TModule;
begin
  Result := nil;
  for I := 0 to FModules.Count - 1 do
  begin
    M := PModule(FModules[I])^;
    if (LongWord(Location) >= LongWord(M.BaseAddress)) and
      (LongWord(Location) < (LongWord(M.BaseAddress) + M.Size)) then
    begin
      Result := FModules[I];
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclMapFiles.Refresh;
begin
  Load;
end;

//==============================================================================
// TJclMap
//==============================================================================

procedure TJclMap.Clear;
var
  I, J: Integer;
begin
  for i := 0 to FSections.Count - 1 do
    Dispose(PSection(FSections[I]));
  FSections.Clear;
  for I := 0 to FUnits.Count - 1 do
  begin
    for J := 0 to PUnit(FUnits[I]).Lines.Count - 1 do
      Dispose(PLine(PUnit(FUnits[I]).Lines[J]));
    PUnit(FUnits[I]).Lines.Free;
    Dispose(PUnit(FUnits[I]));
  end;
  FUnits.Clear;
  for I := 0 to FPublics.Count - 1 do
    Dispose(PPublic(FPublics[I]));
  FPublics.Clear;
end;

//------------------------------------------------------------------------------

constructor TJclMap.Create(const Filename: string; ModuleHandle: THandle);
begin
  inherited Create;
  FFilename := Filename;
  FModuleHandle := ModuleHandle;
  Load;
end;

//------------------------------------------------------------------------------

destructor TJclMap.Destroy;
begin
  Clear;
  FSections.Free;
  FUnits.Free;
  FPublics.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TJclMap.GetPublicName(const Location: Pointer): string;
var
  Index: Integer;
  PublicInfo: PPublic;
begin
  Result := '';
  Index := 0;
  while Index < FPublics.Count do
  begin
    PublicInfo := PPublic(FPublics[Index]);
    if Integer(Location) >= PublicInfo.Start then
    begin
      if Index < FPublics.Count - 1 then
      begin
        if Integer(Location) < PPublic(FPublics[Index + 1]).Start then
        begin
          Result := PublicInfo.Name;
          Break;
        end;
      end
      else
        Result := PublicInfo.Name;
    end;
    Inc(Index);
  end;
end;

//------------------------------------------------------------------------------

procedure TJclMap.GetUnitRange(const Lines: TStrings; var UnitInfo: TUnit);
var
  Index: Integer;
  Line: string;
begin
  Index := 0;
  while Index < Lines.Count do
  begin
    if Trim(Lines[Index]) = SSegmentStart then
    begin
      Inc(Index, 2);
      while Index < Lines.Count do
      begin
        Line := Lines[Index];
        // Check for data
        if Line <> '' then
        begin
          if (Pos(SCodeSegment, Line) > 0) and
             (Pos('M=' + UnitInfo.UnitName + ' ', Line) > 0) then
          begin
            UnitInfo.Start := StrToInt('$' + Copy(Line, 7, 8));
            UnitInfo.Size := StrToInt('$' + Copy(Line, 16, 8));
            Break;
          end;
        end
        else
          Break;
        Inc(Index);
      end;
      Break;
    end
    else
      Inc(Index);
  end;
end;

//------------------------------------------------------------------------------

procedure TJclMap.Load;
var
  Lines: TStringList;

  {$IFDEF JCL_DEBUG_EXTENSION}

  procedure DecompressData;
  var
    CompStream: TDecompressionStream;
    CompOutStream: TMemoryStream;
    OriginalSize: Integer;
    ResourceStream: TResourceStream;
  begin
    ResourceStream := TResourceStream.Create(FModuleHandle, JclDebugResName, RT_RCDATA);
    try
      ResourceStream.ReadBuffer(OriginalSize, SizeOf(OriginalSize));
      CompOutStream := TMemoryStream.Create;
      try
        CompStream := TDecompressionStream.Create(ResourceStream);
        try
          CompOutStream.CopyFrom(CompStream, OriginalSize);
        finally
          CompStream.Free;
        end;
        CompOutStream.Seek(0, soFromBeginning);
        Lines.LoadFromStream(CompOutStream);
      finally
        CompOutStream.Free;
      end;
    finally
      ResourceStream.Free;
    end;
  end;

  {$ENDIF}

begin
  Lines := TStringList.Create;
  try
    // First load map file into memory
    {$IFDEF JCL_DEBUG_EXTENSION}
    if FindResource(FModuleHandle, JclDebugResName, RT_RCDATA) <> 0 then
      DecompressData
    else
      Lines.LoadFromFile(FFilename);
    {$ELSE}
    Lines.LoadFromFile(FFilename);
    {$ENDIF}
    // Then extract information from it
    LoadSections(Lines);
    LoadUnits(Lines);
    LoadPublics(Lines);
  finally
    Lines.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclMap.LoadPublics(const Lines: TStrings);
var
  Index: Integer;
  Line: string;
  PublicInfo: PPublic;
begin
  FPublics := TList.Create;
  Index := 0;
  while Index < Lines.Count do
  begin
    Line := Lines[Index];
    if Line = SPublicsStart then
    begin
      // Skip to data
      Inc(Index, 2);
      while Index < Lines.Count do
      begin
        Line := Lines[Index];
        // If end of publics, exit loop
        if Line = '' then
          Break
        else
        begin
          Line := Trim(Line);
          if Copy(Line, 1, 5) = '0001:' then
          begin
            New(PublicInfo);
            try
              PublicInfo.Start := StrToInt('$' + Copy(Line, 6, 8));
              PublicInfo.Name := Copy(Line, 21, Length(Line));
              FPublics.Add(PublicInfo);
            except
              Dispose(PublicInfo);
              raise;
            end;
          end;
        end;
        Inc(Index);
      end;
      Break;
    end;
    Inc(Index);
  end;
end;

//------------------------------------------------------------------------------

procedure TJclMap.LoadSections(const Lines: TStrings);
var
  Index: Integer;
  Line: string;
  Section: PSection;
begin
  FSections := TList.Create;
  Index := 2;
  while Trim(Lines[Index]) <> '' do
  begin
    Line := Lines[Index];
    // If got data, extract section information from it
    if Line <> '' then
    begin
      New(Section);
      try
        with Section^ do
        begin
          ID := StrToInt(Copy(Line, 2, 4));
          Start := StrToInt('$' + Copy(Line, 7, 8));
          Size := StrToInt('$' + Copy(Line, 16, 8));
          Name := Trim(Copy(Line, 26, 24));
          SectionClass := Trim(Copy(Line, 50, Length(Line)));
        end;
        FSections.Add(Section);
      except
        Dispose(Section);
        raise;
      end;
    end;
    Inc(Index);
  end;
end;

//------------------------------------------------------------------------------

procedure TJclMap.LoadUnitInformation(const Lines: TStrings; const LineNumber: Integer);
var
  Line: string;
  MapIndex: Integer;
  UnitLineNumber: Integer;
  UnitAddress: string;
  UnitInfo: PUnit;
  LineInfo: PLine;
begin
  New(UnitInfo);
  try
    UnitInfo.Lines := TList.Create;
    try
      // Extract unit name
      Line := Lines[LineNumber];
      Delete(Line, 1, Length(SModuleStart));
      UnitInfo.UnitName := Trim(Copy(Line, 1, Pos('(', Line) - 1));
      // Extract file name
      Line := Copy(Line, Pos('(', Line) + 1, Length(Line));
      Line := Trim(Copy(Line, 1, Pos(')', Line) - 1));
      UnitInfo.Filename := Line;
      // Extract line numbers
      GetUnitRange(Lines, UnitInfo^);
      MapIndex := LineNumber + 2;
      while MapIndex < Lines.Count do
      begin
        Line := Trim(Lines[MapIndex]);
        // Check for information
        if (Line <> '') then
        begin
          while (Line <> '') do
          begin
            // Extract data from text
            UnitLineNumber := StrToInt(Trim(First(Line)));
            Line := Trim(ButFirst(Line));
            UnitAddress := Trim(First(Line));
            Line := Trim(ButFirst(Line));
            // Add to list
            New(LineInfo);
            LineInfo.LineNumber := UnitLineNumber;
            LineInfo.Start := StrToInt('$' + Copy(UnitAddress, 6, 8));
            UnitInfo.Lines.Add(LineInfo);
          end;
        end
        else
          Break;
        Inc(MapIndex);
      end;
    except
      UnitInfo.Lines.Free;
      raise;
    end;
    FUnits.Add(UnitInfo);
  except
    Dispose(UnitInfo);
    raise;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclMap.LoadUnits(const Lines: TStrings);
var
  Index: Integer;
  Line: string;
begin
  FUnits := TList.Create;
  // Read all units into table
  Index := 0;
  while Index < Lines.Count do
  begin
    // Read one line
    Line := Lines[Index];
    // Check for module information
    if Copy(Line, 1, Length(SModuleStart)) = SModuleStart then
      LoadUnitInformation(Lines, Index);
    Inc(Index);
  end;
end;

//------------------------------------------------------------------------------

function TJclMap.Lookup(const Location: Pointer; var ModuleName, ProcName,
  Filename: string; var LineNumber: Integer): Boolean;
var
  Index: Integer;
  UnitInfo: PUnit;
  LineInfo: PLine;
begin
  Result := False;
  Index := 0;
  while Index < FUnits.Count do
  begin
    if (Integer(Location) >= PUnit(FUnits[Index]).Start) and
      (Integer(Location) < PUnit(FUnits[Index]).Start +
        PUnit(FUnits[Index]).Size) then
    begin
      UnitInfo := PUnit(FUnits[Index]);
      // Get name of module
      ModuleName := UnitInfo.UnitName;
      Filename := UnitInfo.Filename;
      LineNumber := 0;
      ProcName := GetPublicName(Location);
      // Find correct line
      Index := 0;
      while (Index < UnitInfo.Lines.Count) and (LineNumber = 0) do
      begin
        LineInfo := PLine(UnitInfo.Lines[Index]);
        if Integer(Location) >= LineInfo.Start then
        begin
          if (Index < UnitInfo.Lines.Count - 1) then
          begin
            if (Integer(Location) < PLine(UnitInfo.Lines[Index + 1]).Start) then
              LineNumber := LineInfo.LineNumber;
          end
          else
            LineNumber := LineInfo.LineNumber;
        end; // if at least above this line
        Inc(Index);
      end; // while more lines to check
      Result := True;
      // Break now that we found the correct unit
      Break;
    end; // if found the correct unit
    Inc(Index);
  end;
end;

//==============================================================================
// Auxilliary functions
//==============================================================================

function Caller(Level: Integer): PChar;
begin
  // Obtain the address of the caller at that level
  try
    Move((StackFrame(Level + 1) + 4)^, Result, 4);
    Dec(Result); // Caller always points to instruction following the call
  except
    Result := nil;
  end;
end;

//------------------------------------------------------------------------------

function ExceptionAddr: Pointer;
begin
  // Exception address always points to next instruction to execute
  // This procedures adjusts for it by returning an adjusted pointer
  Result := ExceptAddr;
  if Result <> nil then
    Dec(Integer(Result));
end;

//------------------------------------------------------------------------------

function StackFrame(Level: Integer): PChar;
var
  CurEBP: PChar;
begin
  Result := nil;
  // Obtain the current base pointer of the stack
  asm
          PUSH    EBP
          POP     CurEBP
  end;
  // Then wind back the stack
  while Level > 0 do
  begin
    try
      Move(CurEBP^, CurEBP, 4);
    except
      on Exception do
        Exit;
    end;
    Dec(Level);
  end;
  Result := CurEBP;
end;

//------------------------------------------------------------------------------

// The one and only TJclMapFiles object used by the __XXX__ functions.
// Created at first use and remains until application exits.

var
  MapFilesObj: TJclMapFiles = nil;

//------------------------------------------------------------------------------

// Helper function for __XXX__ functions. Creates the global TJclMapFiles object
// if it hasn't been created yet.

procedure NeedMapFiles;
begin
  if MapFilesObj = nil then
    MapFilesObj := TJclMapFiles.Create(USE_CURRENT_PROCESS);
end;

//------------------------------------------------------------------------------

function __FILE__(const Level: Integer): string;
var
  LocInfo: TLocationInfo;
  Addr: Pointer;
begin
  NeedMapFiles;
  Addr := Caller(Level + 1);
  LocInfo := MapFilesObj.GetLocationInfo(Addr);
  Result := LocInfo.SourceName;
end;

//------------------------------------------------------------------------------

function __MODULE__(const Level: Integer): string;
var
  LocInfo: TLocationInfo;
  Addr: Pointer;
begin
  NeedMapFiles;
  Addr := Caller(Level + 1);
  LocInfo := MapFilesObj.GetLocationInfo(Addr);
  Result := LocInfo.ModuleName;
end;

//------------------------------------------------------------------------------

function __PROC__(const Level: Integer): string;
var
  LocInfo: TLocationInfo;
  Addr: Pointer;
begin
  NeedMapFiles;
  Addr := Caller(Level + 1);
  LocInfo := MapFilesObj.GetLocationInfo(Addr);
  Result := LocInfo.ProcName;
end;

//------------------------------------------------------------------------------

function __LINE__(const Level: Integer): Integer;
var
  LocInfo: TLocationInfo;
  Addr : Pointer;
begin
  NeedMapFiles;
  Addr := Caller(Level + 1);
  LocInfo := MapFilesObj.GetLocationInfo(Addr);
  Result := LocInfo.LineNumber;
end;

//------------------------------------------------------------------------------

function __FILE_OF_ADDR__(const Addr: Pointer): string;
var
  LocInfo: TLocationInfo;
begin
  NeedMapFiles;
  LocInfo := MapFilesObj.GetLocationInfo(Addr);
  Result := LocInfo.SourceName;
end;

//------------------------------------------------------------------------------

function __MODULE_OF_ADDR__(const Addr: Pointer): string;
var
  LocInfo: TLocationInfo;
begin
  NeedMapFiles;
  LocInfo := MapFilesObj.GetLocationInfo(Addr);
  Result := LocInfo.ModuleName;
end;

//------------------------------------------------------------------------------

function __PROC_OF_ADDR__(const Addr: Pointer): string;
var
  LocInfo: TLocationInfo;
begin
  NeedMapFiles;
  LocInfo := MapFilesObj.GetLocationInfo(Addr);
  Result := LocInfo.ProcName;
end;

//------------------------------------------------------------------------------

function __LINE_OF_ADDR__(const Addr: Pointer): Integer;
var
  LocInfo: TLocationInfo;
begin
  NeedMapFiles;
  LocInfo := MapFilesObj.GetLocationInfo(Addr);
  Result := LocInfo.LineNumber;
end;

//------------------------------------------------------------------------------

initialization
  // nothing to initialize

finalization
  FreeAndNil(MapFilesObj);
end.
