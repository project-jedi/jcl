{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFinalize.pas, released on 2004-02-27.

The Initial Developers of the Original Code is: Andreas Hausladen
Copyright (c) 2004 Andreas Hausladen
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JCL home page,
located at http://jcl.sourceforge.net

The purpose of this unit is to reduce code inclusion for functions that are not
used by the program and which are only included because they are used in the
finalization section.

Known Issues:
-----------------------------------------------------------------------------}

unit JclFinalize;

{$I jcl.inc}

interface

type
  TFinalizeProc = procedure;

/// <summary>
/// AddFinalizeProc adds a TFinalizeProc to the finalize section.
/// The procedure is called on finalization.
/// </summary>
procedure AddFinalizeProc(const UnitName: string; FinalizeProc: TFinalizeProc);

/// <summary>
/// AddFinalizeObject adds an TObject derived class to the finalize section.
/// The object is destroyed on finalization.
/// </summary>
function AddFinalizeObject(const UnitName: string; Instance: TObject): TObject;

/// <summary>
/// AddFinalizeObjectNil adds an TObject derived class to the finalize section.
/// The object is destroyed and the reference is set to nil on finalization.
/// The object is freed by "Obj.Free; Obj := nil;"
/// </summary>
/// <limitation>
/// Only global variables are allowed to be specified.
/// </limitation>
function AddFinalizeObjectNil(const UnitName: string; var Reference{: TObject}): TObject;

/// <summary>
/// AddFinalizeFreeAndNil adds an TObject derived class to the finalize section.
/// The reference is set to nil before the object is destroyed on finalization.
/// </summary>
/// <limitation>
/// Only global variables are allowed to be specified.
/// </limitation>
function AddFinalizeFreeAndNil(const UnitName: string; var Reference{: TObject}): TObject;

/// <summary>
/// AddFinalizeMemory adds an memory allocation to the finalize section.
/// The memory is released on finalization.
/// </summary>
function AddFinalizeMemory(const UnitName: string; Ptr: Pointer): Pointer;

/// <summary>
/// AddFinalizeMemory adds an memory allocation to the finalize section.
/// The memory is released and the Ptr is set to nil on finalization.
/// </summary>
/// <limitation>
/// Only global variables are allowed to be specified.
/// </limitation>
function AddFinalizeMemoryNil(const UnitName: string; var Ptr{: Pointer}): Pointer;

/// <summary>
/// FinalizeUnit finalizes all items from the unit UnitName. The UnitName is
/// case sensitive. If you add any finalization item you must call this function
/// to free the items. Otherwise the items will be destroyed when the package
/// that contains the JclFinalize unit is unloaded.
/// </summary>
procedure FinalizeUnit(const UnitName: string);

implementation

type
  TFinalizeItem = class(TObject)
  public
    Next: TFinalizeItem;
    constructor Create(const AUnitName: string);
  end;

  TFinalizeUnitItem = class(TObject)
  public
    UnitName: string;
    Items: TFinalizeItem;
    Next: TFinalizeUnitItem;
    constructor Create(AUnitName: string; ANext: TFinalizeUnitItem);
    destructor Destroy; override;
  end;

var
  FinalizeUnitList: TFinalizeUnitItem = nil;

//=== { TFinalizeItem } ======================================================

constructor TFinalizeItem.Create(const AUnitName: string);
var
  P: TFinalizeUnitItem;
begin
  inherited Create;
  P := FinalizeUnitList;
  while P <> nil do
  begin
    if P.UnitName = AUnitName then
      Break;
    P := P.Next;
  end;
  if P = nil then
  begin
    FinalizeUnitList := TFinalizeUnitItem.Create(AUnitName, FinalizeUnitList);
    P := FinalizeUnitList;
  end;
  Next := P.Items;
  P.Items := Self;
end;


//=== { TFinalizeUnitItem } ==================================================

constructor TFinalizeUnitItem.Create(AUnitName: string; ANext: TFinalizeUnitItem);
begin
  inherited Create;
  UnitName := AUnitName;
  Next := ANext;
end;

destructor TFinalizeUnitItem.Destroy;
var
  P: TFinalizeItem;
begin
  while Items <> nil do
  begin
    P := Items;
    Items := P.Next;
    P.Free;
  end;
  inherited Destroy;
end;

/// <summary>
/// FinalizeUnits destroys all remaining finalization items.
/// </summary>
procedure FinalizeUnits;
var
  P: TFinalizeUnitItem;
begin
 // Normally FinalizeUnitList should be nil because the units should call
 // FinalizeUnit() in their finalization section.
  while FinalizeUnitList <> nil do
    try
      while FinalizeUnitList <> nil do
      begin
        P := FinalizeUnitList;
        FinalizeUnitList := P.Next;
        P.Free;
      end;
    except
      // ignore, we are in the finalization section
    end;
end;

procedure FinalizeUnit(const UnitName: string);
var
  N, P: TFinalizeUnitItem;
begin
  N := nil;
  P := FinalizeUnitList;
  if P = nil then
    Exit;
  while P <> nil do
  begin
    if P.UnitName = UnitName then
    begin
      if N = nil then
        FinalizeUnitList := P.Next
      else
        N.Next := P.Next;
      P.Free;
      Break;
    end;
    N := P;
    P := P.Next;
  end;
end;

//============================================================================

type
  PObject = ^TObject;
  PPointer = ^Pointer;

  TFinalizeProcItem = class(TFinalizeItem)
  private
    FFinalizeProc: TFinalizeProc;
  public
    constructor Create(const AUnitName: string; AFinalizeProc: TFinalizeProc);
    destructor Destroy; override;
  end;

  TFinalizeObjectItem = class(TFinalizeItem)
  private
    FInstance: TObject;
  public
    constructor Create(const AUnitName: string; AInstance: TObject);
    destructor Destroy; override;
  end;

  TFinalizeObjectNilItem = class(TFinalizeItem)
  private
    FReference: PObject;
  public
    constructor Create(const AUnitName: string; var AReference: TObject);
    destructor Destroy; override;
  end;

  TFinalizeFreeAndNilItem = class(TFinalizeItem)
  private
    FReference: PObject;
  public
    constructor Create(const AUnitName: string; var AReference: TObject);
    destructor Destroy; override;
  end;

  TFinalizeMemoryItem = class(TFinalizeItem)
  private
    FPtr: Pointer;
  public
    constructor Create(const AUnitName: string; APtr: Pointer);
    destructor Destroy; override;
  end;

  TFinalizeMemoryNilItem = class(TFinalizeItem)
  private
    FPtr: PPointer;
  public
    constructor Create(const AUnitName: string; var APtr: Pointer);
    destructor Destroy; override;
  end;

//=== { TFinalizeProcItem } ==================================================

constructor TFinalizeProcItem.Create(const AUnitName: string;
  AFinalizeProc: TFinalizeProc);
begin
  inherited Create(AUnitName);
  FFinalizeProc := AFinalizeProc;
end;

destructor TFinalizeProcItem.Destroy;
begin
  FFinalizeProc;
  inherited Destroy;
end;

//=== { TFinalizeObjectItem } ================================================

constructor TFinalizeObjectItem.Create(const AUnitName: string;
  AInstance: TObject);
begin
  inherited Create(AUnitName);
  FInstance := AInstance;
end;

destructor TFinalizeObjectItem.Destroy;
begin
  FInstance.Free;
  inherited Destroy;
end;

//=== { TFinalizeObjectNilItem } =============================================

constructor TFinalizeObjectNilItem.Create(const AUnitName: string;
  var AReference: TObject);
begin
  inherited Create(AUnitName);
  FReference := @AReference;
end;

destructor TFinalizeObjectNilItem.Destroy;
begin
  FReference^.Free;
  FReference^ := nil;
  inherited Destroy;
end;

//=== { TFinalizeFreeAndNilItem } ============================================

constructor TFinalizeFreeAndNilItem.Create(const AUnitName: string;
  var AReference: TObject);
begin
  inherited Create(AUnitName);
  FReference := @AReference;
end;

destructor TFinalizeFreeAndNilItem.Destroy;
var
  Obj: TObject;
begin
  Obj := FReference^;
  FReference^ := nil;
  Obj.Free;
  inherited Destroy;
end;

//=== { TFinalizeMemoryItem } ================================================

constructor TFinalizeMemoryItem.Create(const AUnitName: string; APtr: Pointer);
begin
  inherited Create(AUnitName);
  FPtr := APtr;
end;

destructor TFinalizeMemoryItem.Destroy;
begin
  if FPtr <> nil then
    FreeMem(FPtr);
  inherited Destroy;
end;

//=== { TFinalizeMemoryNilItem } =============================================

constructor TFinalizeMemoryNilItem.Create(const AUnitName: string;
  var APtr: Pointer);
begin
  inherited Create(AUnitName);
  FPtr := @APtr;
end;

destructor TFinalizeMemoryNilItem.Destroy;
begin
  if FPtr^ <> nil then
  begin
    FreeMem(FPtr^);
    FPtr^ := nil;
  end;
  inherited Destroy;
end;

//============================================================================

procedure AddFinalizeProc(const UnitName: string; FinalizeProc: TFinalizeProc);
begin
  TFinalizeProcItem.Create(UnitName, FinalizeProc);
end;

function AddFinalizeObject(const UnitName: string; Instance: TObject): TObject;
begin
  TFinalizeObjectItem.Create(UnitName, Instance);
  Result := Instance;
end;

function AddFinalizeObjectNil(const UnitName: string; var Reference{: TObject}): TObject;
begin
  TFinalizeObjectNilItem.Create(UnitName, TObject(Reference));
  Result := TObject(Reference);
end;

function AddFinalizeFreeAndNil(const UnitName: string; var Reference{: TObject}): TObject;
begin
  TFinalizeFreeAndNilItem.Create(UnitName, TObject(Reference));
  Result := TObject(Reference);
end;

function AddFinalizeMemory(const UnitName: string; Ptr: Pointer): Pointer;
begin
  TFinalizeMemoryItem.Create(UnitName, Ptr);
  Result := Ptr;
end;

function AddFinalizeMemoryNil(const UnitName: string; var Ptr{: Pointer}): Pointer;
begin
  TFinalizeMemoryNilItem.Create(UnitName, Pointer(Ptr));
  Result := Pointer(Ptr);
end;

{$IFDEF MSWINDOWS}
initialization

finalization
// asn: causes exceptions on linux 
  FinalizeUnits;
{$ENDIF MSWINDOWS}

end.

