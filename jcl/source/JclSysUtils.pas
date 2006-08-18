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
{ The Original Code is JclSysUtils.pas.                                        }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ 2000 of these individuals.                                                   }
{                                                                              }
{ Description: Various pointer and class related routines.                     }
{ Unit Owner: Jeroen Speldekamp                                                }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This unit contains various routine for manipulating the math coprocessor.    }
{ This includes such things as querying and setting the rounding precision of  }
{ floating point operations and retrieving the coprocessor's status word.      }
{                                                                              }
{ Unit owner: Eric S. Fisher                                                   }
{ Last modified: May 03, 2001                                                  }
{                                                                              }
{******************************************************************************}

unit JclSysUtils;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

uses
  {$IFDEF WIN32}
  Windows,
  {$ENDIF WIN32}
  Classes, TypInfo,
  JclBase;

//------------------------------------------------------------------------------
// Pointer manipulation
//------------------------------------------------------------------------------

{$IFNDEF DELPHI5_UP}
procedure FreeAndNil(var Obj);
{$ENDIF DELPHI5_UP}

procedure GetAndFillMem(var P: Pointer; const Size: Integer; const Value: Byte);
procedure FreeMemAndNil(var P: Pointer);
function PCharOrNil(const S: AnsiString): PAnsiChar;
{$IFDEF SUPPORTS_WIDESTRING}
function PWideCharOrNil(const W: WideString): PWideChar;
{$ENDIF SUPPORTS_WIDESTRING}

function SizeOfMem(const APointer: Pointer): Integer;

//------------------------------------------------------------------------------
// Guards
//------------------------------------------------------------------------------

{$IFDEF SUPPORTS_INTERFACE}

type
  ISafeGuard = interface
    function ReleaseItem: Pointer;
    function GetItem: Pointer;
    procedure FreeItem;
    property Item: Pointer read GetItem;
  end;

function Guard(Mem: Pointer; out SafeGuard: ISafeGuard): Pointer; overload;
function Guard(Obj: TObject; out SafeGuard: ISafeGuard): TObject; overload;

function GuardGetMem(Size: Cardinal; out SafeGuard: ISafeGuard): Pointer;
function GuardAllocMem(Size: Cardinal; out SafeGuard: ISafeGuard): Pointer;

{$ENDIF SUPPORTS_INTERFACE}

//------------------------------------------------------------------------------
// Object lists
//------------------------------------------------------------------------------

procedure ClearObjectList(List: TList);
procedure FreeObjectList(var List: TList);

//------------------------------------------------------------------------------
// replacement for the C ternary conditional operator ? :
//------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: string): string; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Char): Char; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Byte): Byte; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Integer): Integer; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Cardinal): Cardinal; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Float): Float; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Boolean): Boolean; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Pointer): Pointer; overload;
{$IFDEF SUPPORTS_INT64}
function Iff(const Condition: Boolean; const TruePart, FalsePart: Int64): Int64; overload;
{$ENDIF SUPPORTS_INT64}

//------------------------------------------------------------------------------
// Classes information and manipulation
//------------------------------------------------------------------------------

type
  EJclVMTError = class (EJclError);

//------------------------------------------------------------------------------
// Virtual Methods
//------------------------------------------------------------------------------

function GetVirtualMethodCount(AClass: TClass): Integer;
function GetVirtualMethod(AClass: TClass; const Index: Integer): Pointer;
{$IFDEF WIN32}
procedure SetVirtualMethod(AClass: TClass; const Index: Integer; const Method: Pointer);
{$ENDIF WIN32}

//------------------------------------------------------------------------------
// Dynamic Methods
//------------------------------------------------------------------------------

type
  TDynamicIndexList = array [0..MaxInt div 16] of Word;
  PDynamicIndexList = ^TDynamicIndexList;
  TDynamicAddressList = array [0..MaxInt div 16] of Pointer;
  PDynamicAddressList = ^TDynamicAddressList;

function GetDynamicMethodCount(AClass: TClass): Integer;
function GetDynamicIndexList(AClass: TClass): PDynamicIndexList;
function GetDynamicAddressList(AClass: TClass): PDynamicAddressList;
function HasDynamicMethod(AClass: TClass; Index: Integer): Boolean;
function GetDynamicMethod(AClass: TClass; Index: Integer): Pointer;

{ init table methods }

function GetInitTable(AClass: TClass): PTypeInfo;

{ field table methods }

type
  PFieldEntry = ^TFieldEntry;
  TFieldEntry = packed record
    OffSet: Integer;
    IDX: Word;
    Name: ShortString;
  end;

  PFieldClassTable = ^TFieldClassTable;
  TFieldClassTable = packed record
    Count: Smallint;
    Classes: array [0..8191] of ^TPersistentClass;
  end;

  PFieldTable = ^TFieldTable;
  TFieldTable = packed record
    EntryCount: Word;
    FieldClassTable: PFieldClassTable;
    FirstEntry: TFieldEntry;
   {Entries: array [1..65534] of TFieldEntry;}
  end;

function GetFieldTable(AClass: TClass): PFieldTable;

{ method table }

type
  PMethodEntry = ^TMethodEntry;
  TMethodEntry = packed record
    EntrySize: Word;
    Address: Pointer;
    Name: ShortString;
  end;

  PMethodTable = ^TMethodTable;
  TMethodTable = packed record
    Count: Word;
    FirstEntry: TMethodEntry;
   {Entries: array [1..65534] of TMethodEntry;}
  end;

function GetMethodTable(AClass: TClass): PMethodTable;
function GetMethodEntry(MethodTable: PMethodTable; Index: Integer): PMethodEntry;

//------------------------------------------------------------------------------
// Class Parent
//------------------------------------------------------------------------------

{$IFDEF WIN32}
procedure SetClassParent(AClass: TClass; NewClassParent: TClass);
{$ENDIF WIN32}
function GetClassParent(AClass: TClass): TClass;

function IsClass(Address: Pointer): Boolean;
function IsObject(Address: Pointer): Boolean;

//------------------------------------------------------------------------------
// Numeric formatting routines
//------------------------------------------------------------------------------

function IntToStrZeroPad(Value, Count: Integer): AnsiString;

//------------------------------------------------------------------------------
// Loading of modules (DLLs)
//------------------------------------------------------------------------------

{$IFDEF WIN32}

type
  TModuleHandle = HINST;

const
  INVALID_MODULEHANDLE_VALUE = TModuleHandle(0);

function LoadModule(var Module: TModuleHandle; FileName: string): Boolean;
function LoadModuleEx(var Module: TModuleHandle; FileName: string; Flags: Cardinal): Boolean;
procedure UnloadModule(var Module: TModuleHandle);
function GetModuleSymbol(Module: TModuleHandle; SymbolName: string): Pointer;
function GetModuleSymbolEx(Module: TModuleHandle; SymbolName: string; var Accu: Boolean): Pointer;
function ReadModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
function WriteModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;

{$ENDIF WIN32}

implementation

uses
  SysUtils,
  JclResources, JclStrings;

//==============================================================================
// Pointer manipulation
//==============================================================================

{$IFNDEF DELPHI5_UP}

procedure FreeAndNil(var Obj);
var
  O: TObject;
begin
  O := TObject(Obj);
  Pointer(Obj) := nil;
  O.Free;
end;

{$ENDIF DELPHI5_UP}

//------------------------------------------------------------------------------

procedure GetAndFillMem(var P: Pointer; const Size: Integer; const Value: Byte);
begin
  GetMem(P, Size);
  FillChar(P^, Size, Value);
end;

//------------------------------------------------------------------------------

procedure FreeMemAndNil(var P: Pointer);
var
  Q: Pointer;
begin
  Q := P;
  P := nil;
  FreeMem(Q);
end;

//------------------------------------------------------------------------------

function PCharOrNil(const S: AnsiString): PAnsiChar;
begin
  if Length(S) = 0 then
    Result := nil
  else
    Result := PAnsiChar(S);
end;

//------------------------------------------------------------------------------

{$IFDEF SUPPORTS_WIDESTRING}

function PWideCharOrNil(const W: WideString): PWideChar;
begin
  if Length(W) = 0 then
    Result := nil
  else
    Result := PWideChar(W);
end;

{$ENDIF SUPPORTS_WIDESTRING}

//------------------------------------------------------------------------------

type
  PUsed = ^TUsed;
  TUsed = record
    SizeFlags: Integer;
  end;

const
  cThisUsedFlag = 2;
  cPrevFreeFlag = 1;
  cFillerFlag   = Integer($80000000);
  cFlags        = cThisUsedFlag or cPrevFreeFlag or cFillerFlag;

function SizeOfMem(const APointer: Pointer): Integer;
var
  U: PUsed;
begin
  if IsMemoryManagerSet then
    Result:= -1
  else
  begin
    Result := 0;
    if APointer <> nil then
    begin
      U := APointer;
      U := PUsed(PChar(U) - SizeOf(TUsed));
      if (U.SizeFlags and cThisUsedFlag) <> 0 then
        Result := (U.SizeFlags) and (not cFlags - SizeOf(TUsed));
    end;
  end;
end;

//==============================================================================
// Guards
//==============================================================================

{$IFDEF SUPPORTS_INTERFACE}

type
  TSafeGuard = class (TInterfacedObject, ISafeGuard)
  private
    FItem: Pointer;
  public
    constructor Create(Mem: Pointer);
    destructor Destroy; override;
    function ReleaseItem: Pointer;
    function GetItem: Pointer;
    procedure FreeItem; virtual;
  end;

  TObjSafeGuard = class (TSafeGuard, ISafeGuard)
  public
    constructor Create(Obj: TObject);
    procedure FreeItem; override;
  end;

//------------------------------------------------------------------------------

constructor TSafeGuard.Create(Mem: Pointer);
begin
  FItem := Mem;
end;

//------------------------------------------------------------------------------

destructor TSafeGuard.Destroy;
begin
  FreeItem;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TSafeGuard.ReleaseItem: Pointer;
begin
  Result := FItem;
  FItem := nil;
end;

//------------------------------------------------------------------------------

function TSafeGuard.GetItem: Pointer;
begin
  Result := FItem;
end;

//------------------------------------------------------------------------------

procedure TSafeGuard.FreeItem;
begin
  if FItem <> nil then
    FreeMem(FItem);
  FItem := nil;
end;

//------------------------------------------------------------------------------

constructor TObjSafeGuard.Create(Obj: TObject);
begin
  inherited Create(Obj);
end;

//------------------------------------------------------------------------------

procedure TObjSafeGuard.FreeItem;
begin
  if FItem <> nil then
  begin
    TObject(FItem).Free;
    FItem := nil;
  end;
end;

//------------------------------------------------------------------------------

function Guard(Mem: Pointer; out SafeGuard: ISafeGuard): Pointer; overload;
begin
  Result := Mem;
  SafeGuard := TSafeGuard.Create(Mem);
end;

//------------------------------------------------------------------------------

function Guard(Obj: TObject; out SafeGuard: ISafeGuard): TObject; overload;
begin
  Result := Obj;
  SafeGuard := TObjSafeGuard.Create(Obj);
end;

//------------------------------------------------------------------------------

function GuardGetMem(Size: Cardinal; out SafeGuard: ISafeGuard): Pointer;
begin
  GetMem(Result, Size);
  Guard(Result, SafeGuard);
end;

//------------------------------------------------------------------------------

function GuardAllocMem(Size: Cardinal; out SafeGuard: ISafeGuard): Pointer;
begin
  Result := AllocMem(Size);
  Guard(Result, SafeGuard);
end;

{$ENDIF SUPPORTS_INTERFACE}

//==============================================================================
// Object lists
//==============================================================================

procedure ClearObjectList(List: TList);
var
  I: Integer;
begin
  if List <> nil then
  begin
    for I := 0 to List.Count - 1 do
    begin
      if List[I] <> nil then
      begin
        if TObject(List[I]) is TList then
        begin
          // recursively delete TList sublists
          ClearObjectList(TList(List[I]));
        end;
        TObject(List[I]).Free;
        List[I] := nil;
      end;
    end;
    List.Clear;
  end;
end;

//------------------------------------------------------------------------------

procedure FreeObjectList(var List: TList);
begin
  if List <> nil then
  begin
    ClearObjectList(List);
    FreeAndNil(List);
  end;
end;

//==============================================================================
// replacement for the C distfix operator ? :
//==============================================================================

function Iff(const Condition: Boolean; const TruePart, FalsePart: string): string; overload;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Char): Char; overload;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Byte): Byte; overload;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Integer): Integer; overload;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Cardinal): Cardinal; overload;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Float): Float; overload;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Boolean): Boolean; overload;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Pointer): Pointer; overload;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//------------------------------------------------------------------------------

{$IFDEF SUPPORTS_INT64}

function Iff(const Condition: Boolean; const TruePart, FalsePart: Int64): Int64; overload;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

{$ENDIF SUPPORTS_INT64}

//==============================================================================
// Classes information and manipulation
//==============================================================================

//==============================================================================
// Virtual Methods
//==============================================================================

function GetVirtualMethodCount(AClass: TClass): Integer;
var
  BeginVMT: Longint;
  EndVMT: Longint;
  TablePointer: Longint;
  I: Integer;
begin
  BeginVMT := Longint(AClass);

  // Scan the offset entries in the class table for the various fields,
  // namely vmtIntfTable, vmtAutoTable, ..., vmtDynamicTable
  // The last entry is always the vmtClassName, so stop once we got there
  // After the last virtual method there is one of these entries.

  EndVMT := PLongint(Longint(AClass) + vmtClassName)^;
  // Set iterator to first item behind VMT table pointer
  I := vmtSelfPtr + SizeOf(Pointer);
  repeat
    TablePointer := PLongint(Longint(AClass) + I)^;
    if (TablePointer <> 0) and (TablePointer >= BeginVMT) and
       (TablePointer < EndVMT) then
      EndVMT := Longint(TablePointer);
    Inc(I, SizeOf(Pointer));
  until I >= vmtClassName;

  Result := (EndVMT - BeginVMT) div SizeOf(Pointer);
end;

//------------------------------------------------------------------------------

function GetVirtualMethod(AClass: TClass; const Index: Integer): Pointer;
begin
  Result := PPointer(Integer(AClass) + Index * SizeOf(Pointer))^;
end;

//------------------------------------------------------------------------------

{$IFDEF WIN32}

procedure SetVirtualMethod(AClass: TClass; const Index: Integer; const Method: Pointer);
var
  WrittenBytes: DWORD;
  PatchAddress: Pointer;
begin
  PatchAddress := PPointer(Integer(AClass) + Index * SizeOf(Pointer))^;
  //! StH: WriteProcessMemory IMO is not exactly the politically correct approach;
  // better VirtualProtect, direct patch, VirtualProtect
  if not WriteProcessMemory(GetCurrentProcess, PatchAddress, {@}Method,
    SizeOf(Pointer), WrittenBytes) then
    raise EJclVMTError.CreateResRecFmt(@RsVMTMemoryWriteError, [SysErrorMessage(GetLastError)]);

  if WrittenBytes <> SizeOf(Pointer) then
    raise EJclVMTError.CreateResRecFmt(@RsVMTMemoryWriteError, [IntToStr(WrittenBytes)]);

  // make sure that everything keeps working in a dual processor setting
  FlushInstructionCache(GetCurrentProcess, PatchAddress, SizeOf(Pointer));
end;

{$ENDIF WIN32}

//==============================================================================
// Dynamic Methods
//==============================================================================

type
  TvmtDynamicTable = packed record
    Count: Word;
   {IndexList: array [1..Count] of Word;
    AddressList: array [1..Count] of Pointer;}
  end;

//------------------------------------------------------------------------------

function GetDynamicMethodCount(AClass: TClass): Integer; assembler;
asm
        MOV     EAX, [EAX].vmtDynamicTable
        TEST    EAX, EAX
        JE      @@Exit
        MOVZX   EAX, WORD PTR [EAX]
@@Exit:
end;

//------------------------------------------------------------------------------

function GetDynamicIndexList(AClass: TClass): PDynamicIndexList; assembler;
asm
        MOV     EAX, [EAX].vmtDynamicTable
        ADD     EAX, 2
end;

//------------------------------------------------------------------------------

function GetDynamicAddressList(AClass: TClass): PDynamicAddressList; assembler;
asm
        MOV     EAX, [EAX].vmtDynamicTable
        MOVZX   EDX, Word ptr [EAX]
        ADD     EAX, EDX
        ADD     EAX, EDX
        ADD     EAX, 2
end;

//------------------------------------------------------------------------------

function HasDynamicMethod(AClass: TClass; Index: Integer): Boolean; assembler;
// Mainly copied from System.GetDynaMethod
asm
        { ->    EAX     vmt of class            }
        {       DX      dynamic method index    }

        PUSH    EDI
        XCHG    EAX, EDX
        JMP     @@HaveVMT
@@OuterLoop:
        MOV     EDX, [EDX]
@@HaveVMT:
        MOV     EDI, [EDX].vmtDynamicTable
        TEST    EDI, EDI
        JE      @@Parent
        MOVZX   ECX, WORD PTR [EDI]
        PUSH    ECX
        ADD     EDI,2
        REPNE   SCASW
        JE      @@Found
        POP     ECX
@@Parent:
        MOV     EDX,[EDX].vmtParent
        TEST    EDX,EDX
        JNE     @@OuterLoop
        MOV     EAX, 0
        JMP     @@Exit
@@Found:
        POP     EAX
        MOV     EAX, 1
@@Exit:
        POP     EDI
end;

//------------------------------------------------------------------------------

function GetDynamicMethod(AClass: TClass; Index: Integer): Pointer; assembler;
asm
        CALL    System.@FindDynaClass
end;

//==============================================================================
// Interface Table
//==============================================================================

function GetInitTable(AClass: TClass): PTypeInfo; assembler;
asm
        MOV     EAX, [EAX].vmtInitTable
end;

//------------------------------------------------------------------------------

function GetFieldTable(AClass: TClass): PFieldTable; assembler;
asm
        MOV     EAX, [EAX].vmtFieldTable
end;

//------------------------------------------------------------------------------

function GetMethodTable(AClass: TClass): PMethodTable; assembler;
asm
        MOV     EAX, [EAX].vmtMethodTable
end;

//------------------------------------------------------------------------------

function GetMethodEntry(MethodTable: PMethodTable; Index: Integer): PMethodEntry;
begin
  Result := Pointer(Cardinal(MethodTable) + 2);
  for Index := Index downto 1 do
    Inc(Cardinal(Result), Result^.EntrySize);
end;

//==============================================================================
// Class Parent methods
//==============================================================================

{$IFDEF WIN32}

procedure SetClassParent(AClass: TClass; NewClassParent: TClass);
var
  WrittenBytes: DWORD;
  PatchAddress: Pointer;
begin
  PatchAddress := PPointer(Integer(AClass) + vmtParent)^;
  //! StH: WriteProcessMemory IMO is not exactly the politically correct approach;
  // better VirtualProtect, direct patch, VirtualProtect
  if not WriteProcessMemory(GetCurrentProcess, PatchAddress, @NewClassParent,
    SizeOf(Pointer), WrittenBytes) then
    raise EJclVMTError.CreateResRecFmt(@RsVMTMemoryWriteError, [SysErrorMessage(GetLastError)]);
  if WrittenBytes <> SizeOf(Pointer) then
    raise EJclVMTError.CreateResRecFmt(@RsVMTMemoryWriteError, [IntToStr(WrittenBytes)]);
  // make sure that everything keeps working in a dual processor setting
  FlushInstructionCache(GetCurrentProcess, PatchAddress, SizeOf(Pointer));
end;

{$ENDIF WIN32}

//------------------------------------------------------------------------------

function GetClassParent(AClass: TClass): TClass; assembler;
asm
        MOV     EAX, [AClass].vmtParent
        TEST    Result, EAX
        JE      @@Exit
        MOV     EAX, [EAX]
@@Exit:
end;

//------------------------------------------------------------------------------

function IsClass(Address: Pointer): Boolean; assembler;
asm
        CMP     Address, Address.vmtSelfPtr
        JNZ     @False
        MOV     Result, True
        JMP     @Exit
@False:
        MOV     Result, False
@Exit:
end;

//------------------------------------------------------------------------------

function IsObject(Address: Pointer): Boolean; assembler;
asm
// or IsClass(Pointer(Address^));
        MOV     EAX, [Address]
        CMP     EAX, EAX.vmtSelfPtr
        JNZ     @False
        MOV     Result, True
        JMP     @Exit
@False:
        MOV     Result, False
@Exit:
end;

//==============================================================================
// Numeric formatting routines
//==============================================================================

function IntToStrZeroPad(Value, Count: Integer): AnsiString;
begin
  Result := IntToStr(Value);
  if Length(Result) < Count then
    Result := StrFillChar('0', Count - Length(Result)) + Result;
end;

//==============================================================================
// Loading of modules (DLLs)
//==============================================================================

{$IFDEF WIN32}

function LoadModule(var Module: TModuleHandle; FileName: string): Boolean;
begin
  if Module = INVALID_MODULEHANDLE_VALUE then
    Module := LoadLibrary(PChar(FileName));
  Result := Module <> INVALID_MODULEHANDLE_VALUE;
end;

//------------------------------------------------------------------------------

function LoadModuleEx(var Module: TModuleHandle; FileName: string; Flags: Cardinal): Boolean;
begin
  if Module = INVALID_MODULEHANDLE_VALUE then
    Module := LoadLibraryEx(PChar(FileName), 0, Flags);
  Result := Module <> INVALID_MODULEHANDLE_VALUE;
end;

//------------------------------------------------------------------------------

procedure UnloadModule(var Module: TModuleHandle);
begin
  if Module <> INVALID_MODULEHANDLE_VALUE then
    FreeLibrary(Module);
  Module := INVALID_MODULEHANDLE_VALUE;
end;

//------------------------------------------------------------------------------

function GetModuleSymbol(Module: TModuleHandle; SymbolName: string): Pointer;
begin
  Result := nil;
  if Module <> INVALID_MODULEHANDLE_VALUE then
    Result := GetProcAddress(Module, PChar(SymbolName));
end;

//------------------------------------------------------------------------------

function GetModuleSymbolEx(Module: TModuleHandle; SymbolName: string; var Accu: Boolean): Pointer;
begin
  Result := nil;
  if Module <> INVALID_MODULEHANDLE_VALUE then
    Result := GetProcAddress(Module, PChar(SymbolName));
  Accu := Accu and (Result <> nil);
end;

//------------------------------------------------------------------------------

function ReadModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
var
  Sym: Pointer;
begin
  Result := True;
  Sym := GetModuleSymbolEx(Module, SymbolName, Result);
  if Result then
    Move(Sym^, Buffer, Size);
end;

//------------------------------------------------------------------------------

function WriteModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
var
  Sym: Pointer;
begin
  Result := True;
  Sym := GetModuleSymbolEx(Module, SymbolName, Result);
  if Result then
    Move(Buffer, Sym^, Size);
end;

{$ENDIF WIN32}

end.
