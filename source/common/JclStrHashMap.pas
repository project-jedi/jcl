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
{ The Original Code is JclStrHashMap.pas.                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is Barry Kelly.                                       }
{ Portions created by Barry Kelly are Copyright (C) Barry Kelly. All rights reserved.              }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Barry Kelly, Robert Rossmair, Matthias Thoma, Petr Vones                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains a string-pointer associative map. It works by hashing the added strings using }
{ a passed-in traits object.                                                                       }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclStrHashMap;

{$I jcl.inc}

interface

uses
  SysUtils,
  JclBase, JclResources;

type
  EJclStringHashMapError = class(EJclError);
  THashValue = Cardinal;

type
  TStringHashMapTraits = class(TObject)
  public
    function Hash(const s: string): Cardinal; virtual; abstract;
    function Compare(const l, r: string): Integer; virtual; abstract;
  end;

function CaseSensitiveTraits: TStringHashMapTraits;
function CaseInsensitiveTraits: TStringHashMapTraits;

type
  TIterateFunc = function(AUserData: Pointer; const AStr: string; var APtr: Pointer): Boolean;

  TIterateMethod = function(AUserData: Pointer; const AStr: string; var APtr: Pointer): Boolean of object;

  PPHashNode = ^PHashNode;
  PHashNode = ^THashNode;
  THashNode = record
    Str: string;
    Ptr: Pointer;
    Left: PHashNode;
    Right: PHashNode;
  end;

  { Internal iterate function pointer type used by the protected
    TStringHashMap.NodeIterate method. }
  TNodeIterateFunc = procedure(AUserData: Pointer; ANode: PPHashNode);

  PHashArray = ^THashArray;
  THashArray = array [0..MaxInt div SizeOf(PHashNode) - 1] of PHashNode;

  TStringHashMap = class(TObject)
  private
    FHashSize: Cardinal;
    FCount: Cardinal;
    FList: PHashArray;
    FLeftDelete: Boolean;
    FTraits: TStringHashMapTraits;
    function IterateNode(ANode: PHashNode; AUserData: Pointer; AIterateFunc: TIterateFunc): Boolean;
    function IterateMethodNode(ANode: PHashNode; AUserData: Pointer; AIterateMethod: TIterateMethod): Boolean;
    procedure NodeIterate(ANode: PPHashNode; AUserData: Pointer; AIterateFunc: TNodeIterateFunc);
    procedure SetHashSize(AHashSize: Cardinal);
    procedure DeleteNodes(var q: PHashNode);
    procedure DeleteNode(var q: PHashNode);
  protected
    function FindNode(const s: string): PPHashNode;
    function AllocNode: PHashNode; virtual;
    procedure FreeNode(ANode: PHashNode); virtual;
    function GetData(const s: string): Pointer;
    procedure SetData(const s: string; p: Pointer);
  public
    constructor Create(ATraits: TStringHashMapTraits; AHashSize: Cardinal);
    destructor Destroy; override;
    procedure Add(const s: string; const p{: Pointer});
    function Remove(const s: string): Pointer;
    procedure RemoveData(const p{: Pointer});
    procedure Iterate(AUserData: Pointer; AIterateFunc: TIterateFunc);
    procedure IterateMethod(AUserData: Pointer; AIterateMethod: TIterateMethod);
    function Has(const s: string): Boolean;
    function Find(const s: string; var p{: Pointer}): Boolean;
    function FindData(const p{: Pointer}; var s: string): Boolean;
    procedure Clear;
    property Count: Cardinal read FCount;
    property Data[const s: string]: Pointer read GetData write SetData; default;
    property Traits: TStringHashMapTraits read FTraits;
    property HashSize: Cardinal read FHashSize write SetHashSize;
  end;

{ str=case sensitive, text=case insensitive }

function StrHash(const s: string): THashValue;
function TextHash(const s: string): THashValue;
function DataHash(var AValue; ASize: Cardinal): THashValue;
function Iterate_FreeObjects(AUserData: Pointer; const AStr: string; var AData: Pointer): Boolean;
function Iterate_Dispose(AUserData: Pointer; const AStr: string; var AData: Pointer): Boolean;
function Iterate_FreeMem(AUserData: Pointer; const AStr: string; var AData: Pointer): Boolean;

type
  TCaseSensitiveTraits = class(TStringHashMapTraits)
  public
    function Hash(const s: string): Cardinal; override;
    function Compare(const l, r: string): Integer; override;
  end;

  TCaseInsensitiveTraits = class(TStringHashMapTraits)
  public
    function Hash(const s: string): Cardinal; override;
    function Compare(const l, r: string): Integer; override;
  end;

implementation

//==================================================================================================
// Case Sensitive & Insensitive Traits
//==================================================================================================

function TCaseSensitiveTraits.Compare(const l, r: string): Integer;
begin
  Result := CompareStr(l, r);
end;

//--------------------------------------------------------------------------------------------------

function TCaseSensitiveTraits.Hash(const s: string): Cardinal;
begin
  Result := StrHash(s);
end;

//--------------------------------------------------------------------------------------------------

function TCaseInsensitiveTraits.Compare(const l, r: string): Integer;
begin
  Result := CompareText(l, r);
end;

//--------------------------------------------------------------------------------------------------

function TCaseInsensitiveTraits.Hash(const s: string): Cardinal;
begin
  Result := TextHash(s);
end;

//--------------------------------------------------------------------------------------------------

var
  _CaseSensitiveTraits: TCaseSensitiveTraits;

function CaseSensitiveTraits: TStringHashMapTraits;
begin
  if _CaseSensitiveTraits = nil then
    _CaseSensitiveTraits := TCaseSensitiveTraits.Create;
  Result := _CaseSensitiveTraits;
end;

//--------------------------------------------------------------------------------------------------

var
  _CaseInsensitiveTraits: TCaseInsensitiveTraits;

function CaseInsensitiveTraits: TStringHashMapTraits;
begin
  if _CaseInsensitiveTraits = nil then
    _CaseInsensitiveTraits := TCaseInsensitiveTraits.Create;
  Result := _CaseInsensitiveTraits;
end;

//--------------------------------------------------------------------------------------------------

function Iterate_FreeObjects(AUserData: Pointer; const AStr: string; var AData: Pointer): Boolean;
begin
  TObject(AData).Free;
  AData := nil;
  Result := True;
end;

//--------------------------------------------------------------------------------------------------

function Iterate_Dispose(AUserData: Pointer; const AStr: string; var AData: Pointer): Boolean;
begin
  Dispose(AData);
  AData := nil;
  Result := True;
end;

//--------------------------------------------------------------------------------------------------

function Iterate_FreeMem(AUserData: Pointer; const AStr: string; var AData: Pointer): Boolean;
begin
  FreeMem(AData);
  AData := nil;
  Result := True;
end;

//--------------------------------------------------------------------------------------------------

function StrHash(const s: string): Cardinal;
var
  i: Integer;
  p: PChar;
const
  C_LongBits = 32;
  C_OneEight = 4;
  C_ThreeFourths = 24;
  C_HighBits = $F0000000;
var
  temp: Cardinal;
begin
  { TODO : I should really be processing 4 bytes at once... }
  Result := 0;
  p := PChar(s);

  i := Length(s);
  while i > 0 do
  begin
    Result := (Result shl C_OneEight) + Ord(p^);
    temp := Result and C_HighBits;
    if temp <> 0 then
      Result := (Result xor (temp shr C_ThreeFourths)) and (not C_HighBits);
    Dec(i);
    Inc(p);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TextHash(const s: string): Cardinal;
var
  i: Integer;
  p: PChar;
const
  C_LongBits = 32;
  C_OneEight = 4;
  C_ThreeFourths = 24;
  C_HighBits = $F0000000;
var
  temp: Cardinal;
begin
  { TODO : I should really be processing 4 bytes at once... }
  Result := 0;
  p := PChar(s);

  i := Length(s);
  while i > 0 do
  begin
    Result := (Result shl C_OneEight) + Ord(UpCase(p^));
    temp := Result and C_HighBits;
    if temp <> 0 then
      Result := (Result xor (temp shr C_ThreeFourths)) and (not C_HighBits);
    Dec(i);
    Inc(p);
  end;
end;

//--------------------------------------------------------------------------------------------------

function DataHash(var AValue; ASize: Cardinal): THashValue;
var
  p: PChar;
const
  C_LongBits = 32;
  C_OneEight = 4;
  C_ThreeFourths = 24;
  C_HighBits = $F0000000;
var
  temp: Cardinal;
begin
  { TODO : I should really be processing 4 bytes at once... }
  Result := 0;
  p := @AValue;

  while ASize > 0 do
  begin
    Result := (Result shl C_OneEight) + Ord(p^);
    temp := Result and C_HighBits;
    if temp <> 0 then
      Result := (Result xor (temp shr C_ThreeFourths)) and (not C_HighBits);
    Dec(ASize);
    Inc(p);
  end;
end;

//==================================================================================================
// TStringHashMap
//==================================================================================================

constructor TStringHashMap.Create(ATraits: TStringHashMapTraits; AHashSize: Cardinal);
begin
  Assert(ATraits <> nil, 'HashList must have traits');
  SetHashSize(AHashSize);
  FTraits := ATraits;
end;

//--------------------------------------------------------------------------------------------------

destructor TStringHashMap.Destroy;
begin
  Clear;
  SetHashSize(0);
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

type
  PPCollectNodeNode = ^PCollectNodeNode;
  PCollectNodeNode = ^TCollectNodeNode;
  TCollectNodeNode = record
    next: PCollectNodeNode;
    str: string;
    ptr: Pointer;
  end;

procedure NodeIterate_CollectNodes(AUserData: Pointer; ANode: PPHashNode);
var
  ppcnn: PPCollectNodeNode;
  pcnn: PCollectNodeNode;
begin
  ppcnn := PPCollectNodeNode(AUserData);
  New(pcnn);
  pcnn^.next := ppcnn^;
  ppcnn^ := pcnn;

  pcnn^.str := ANode^^.Str;
  pcnn^.ptr := ANode^^.Ptr;
end;

//--------------------------------------------------------------------------------------------------

procedure TStringHashMap.SetHashSize(AHashSize: Cardinal);
var
  collect_list: PCollectNodeNode;

  procedure CollectNodes;
  var
    i: Integer;
  begin
    collect_list := nil;
    for i := 0 to FHashSize - 1 do
      NodeIterate(@FList^[i], @collect_list, NodeIterate_CollectNodes);
  end;

  procedure InsertNodes;
  var
    pcnn, tmp: PCollectNodeNode;
  begin
    pcnn := collect_list;
    while pcnn <> nil do
    begin
      tmp := pcnn^.next;
      Add(pcnn^.str, pcnn^.ptr);
      Dispose(pcnn);
      pcnn := tmp;
    end;
  end;
begin
  { 4 cases:
    we are empty, and AHashSize = 0 --> nothing to do
    we are full, and AHashSize = 0 --> straight empty
    we are empty, and AHashSize > 0 --> straight allocation
    we are full, and AHashSize > 0 --> rehash }

  if FHashSize = 0 then
    if AHashSize > 0 then
    begin
      GetMem(FList, AHashSize * SizeOf(FList^[0]));
      FillChar(FList^, AHashSize * SizeOf(FList^[0]), 0);
      FHashSize := AHashSize;
    end
    else
      { nothing to do }
  else
  begin
    if AHashSize > 0 then
    begin
      { must rehash table }
      CollectNodes;
      Clear;
      ReallocMem(FList, AHashSize * SizeOf(FList^[0]));
      FillChar(FList^, AHashSize * SizeOf(FList^[0]), 0);
      FHashSize := AHashSize;
      InsertNodes;
    end
    else
    begin
      { we are clearing the table - need hash to be empty }
      if FCount > 0 then
        raise EJclStringHashMapError.CreateResRec(@RsStringHashMapMustBeEmpty);
      FreeMem(FList);
      FList := nil;
      FHashSize := 0;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TStringHashMap.FindNode(const s: string): PPHashNode;
var
  i: Cardinal;
  r: Integer;
  ppn: PPHashNode;
begin
  { we start at the node offset by s in the hash list }
  i := FTraits.Hash(s) mod FHashSize;

  ppn := @FList^[i];

  if ppn^ <> nil then
    while True do
    begin
      r := FTraits.Compare(s, ppn^^.Str);

      { left, then right, then match }
      if r < 0 then
        ppn := @ppn^^.Left
      else
      if r > 0 then
        ppn := @ppn^^.Right
      else
        Break;

      { check for empty position after drilling left or right }
      if ppn^ = nil then
        Break;
    end;

  Result := ppn;
end;

//--------------------------------------------------------------------------------------------------

function TStringHashMap.IterateNode(ANode: PHashNode; AUserData: Pointer;
  AIterateFunc: TIterateFunc): Boolean;
begin
  if ANode <> nil then
  begin
    Result := AIterateFunc(AUserData, ANode^.Str, ANode^.Ptr);
    if not Result then
      Exit;

    Result := IterateNode(ANode^.Left, AUserData, AIterateFunc);
    if not Result then
      Exit;

    Result := IterateNode(ANode^.Right, AUserData, AIterateFunc);
    if not Result then
      Exit;
  end else
    Result := True;
end;

//--------------------------------------------------------------------------------------------------

function TStringHashMap.IterateMethodNode(ANode: PHashNode; AUserData: Pointer;
  AIterateMethod: TIterateMethod): Boolean;
begin
  if ANode <> nil then
  begin
    Result := AIterateMethod(AUserData, ANode^.Str, ANode^.Ptr);
    if not Result then
      Exit;

    Result := IterateMethodNode(ANode^.Left, AUserData, AIterateMethod);
    if not Result then
      Exit;

    Result := IterateMethodNode(ANode^.Right, AUserData, AIterateMethod);
    if not Result then
      Exit;
  end else
    Result := True;
end;

//--------------------------------------------------------------------------------------------------

procedure TStringHashMap.NodeIterate(ANode: PPHashNode; AUserData: Pointer;
  AIterateFunc: TNodeIterateFunc);
begin
  if ANode^ <> nil then
  begin
    AIterateFunc(AUserData, ANode);
    NodeIterate(@ANode^.Left, AUserData, AIterateFunc);
    NodeIterate(@ANode^.Right, AUserData, AIterateFunc);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TStringHashMap.DeleteNode(var q: PHashNode);
var
  t, r, s: PHashNode;
begin
  { we must delete node q without destroying binary tree }
  { Knuth 6.2.2 D (pg 432 Vol 3 2nd ed) }

  { alternating between left / right delete to preserve decent
    performance over multiple insertion / deletion }
  FLeftDelete := not FLeftDelete;

  { t will be the node we delete }
  t := q;

  if FLeftDelete then
  begin
    if t^.Right = nil then
      q := t^.Left
    else
    begin
      r := t^.Right;
      if r^.Left = nil then
      begin
        r^.Left := t^.Left;
        q := r;
      end else
      begin
        s := r^.Left;
        if s^.Left <> nil then
          repeat
            r := s;
            s := r^.Left;
          until s^.Left = nil;
        { now, s = symmetric successor of q }
        s^.Left := t^.Left;
        r^.Left :=  s^.Right;
        s^.Right := t^.Right;
        q := s;
      end;
    end;
  end else
  begin
    if t^.Left = nil then
      q := t^.Right
    else
    begin
      r := t^.Left;
      if r^.Right = nil then
      begin
        r^.Right := t^.Right;
        q := r;
      end else
      begin
        s := r^.Right;
        if s^.Right <> nil then
          repeat
            r := s;
            s := r^.Right;
          until s^.Right = nil;
        { now, s = symmetric predecessor of q }
        s^.Right := t^.Right;
        r^.Right := s^.Left;
        s^.Left := t^.Left;
        q := s;
      end;
    end;
  end;

  { we decrement before because the tree is already adjusted
    => any exception in FreeNode MUST be ignored.

    It's unlikely that FreeNode would raise an exception anyway. }
  Dec(FCount);
  FreeNode(t);
end;

//--------------------------------------------------------------------------------------------------

procedure TStringHashMap.DeleteNodes(var q: PHashNode);
begin
  if q^.Left <> nil then
    DeleteNodes(q^.Left);
  if q^.Right <> nil then
    DeleteNodes(q^.Right);
  FreeNode(q);
  q := nil;
end;

//--------------------------------------------------------------------------------------------------

function TStringHashMap.AllocNode: PHashNode;
begin
  New(Result);
  Result^.Left := nil;
  Result^.Right := nil;
end;

//--------------------------------------------------------------------------------------------------

procedure TStringHashMap.FreeNode(ANode: PHashNode);
begin
  Dispose(ANode);
end;

//--------------------------------------------------------------------------------------------------

function TStringHashMap.GetData(const s: string): Pointer;
var
  ppn: PPHashNode;
begin
  ppn := FindNode(s);

  if ppn^ <> nil then
    Result := ppn^^.Ptr
  else
    Result := nil;
end;

//--------------------------------------------------------------------------------------------------

procedure TStringHashMap.SetData(const s: string; p: Pointer);
var
  ppn: PPHashNode;
begin
  ppn := FindNode(s);

  if ppn^ <> nil then
    ppn^^.Ptr := p
  else
  begin
    { add }
    ppn^ := AllocNode;
    { we increment after in case of exception }
    Inc(FCount);
    ppn^^.Str := s;
    ppn^^.Ptr := p;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TStringHashMap.Add(const s: string; const p{: Pointer});
var
  ppn: PPHashNode;
begin
  ppn := FindNode(s);

  { if reordered from SetData because ppn^ = nil is more common for Add }
  if ppn^ = nil then
  begin
    { add }
    ppn^ := AllocNode;
    { we increment after in case of exception }
    Inc(FCount);
    ppn^^.Str := s;
    ppn^^.Ptr := Pointer(p);
  end else
    raise EJclStringHashMapError.CreateResRecFmt(@RsStringHashMapDuplicate, [s]);
end;

//--------------------------------------------------------------------------------------------------

type
  PListNode = ^TListNode;
  TListNode = record
    Next: PListNode;
    NodeLoc: PPHashNode;
  end;

  PDataParam = ^TDataParam;
  TDataParam = record
    Head: PListNode;
    Data: Pointer;
  end;

procedure NodeIterate_BuildDataList(AUserData: Pointer; ANode: PPHashNode);
var
  dp: PDataParam;
  t: PListNode;
begin
  dp := PDataParam(AUserData);
  if dp.Data = ANode^^.Ptr then
  begin
    New(t);
    t^.Next := dp.Head;
    t^.NodeLoc := ANode;
    dp.Head := t;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TStringHashMap.RemoveData(const p{: Pointer});
var
  dp: TDataParam;
  i: Integer;
  n, t: PListNode;
begin
  dp.Data := Pointer(p);
  dp.Head := nil;

  for i := 0 to FHashSize - 1 do
    NodeIterate(@FList^[i], @dp, NodeIterate_BuildDataList);

  n := dp.Head;
  while n <> nil do
  begin
    DeleteNode(n^.NodeLoc^);
    t := n;
    n := n^.Next;
    Dispose(t);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TStringHashMap.Remove(const s: string): Pointer;
var
  ppn: PPHashNode;
begin
  ppn := FindNode(s);

  if ppn^ <> nil then
  begin
    Result := ppn^^.Ptr;
    DeleteNode(ppn^);
  end
  else
    raise EJclStringHashMapError.CreateResRecFmt(@RsStringHashMapInvalidNode, [s]);
end;

//--------------------------------------------------------------------------------------------------

procedure TStringHashMap.IterateMethod(AUserData: Pointer;
  AIterateMethod: TIterateMethod);
var
  i: Integer;
begin
  for i := 0 to FHashSize - 1 do
    if not IterateMethodNode(FList^[i], AUserData, AIterateMethod) then
      Break;
end;

//--------------------------------------------------------------------------------------------------

procedure TStringHashMap.Iterate(AUserData: Pointer; AIterateFunc: TIterateFunc);
var
  i: Integer;
begin
  for i := 0 to FHashSize - 1 do
    if not IterateNode(FList^[i], AUserData, AIterateFunc) then
      Break;
end;

//--------------------------------------------------------------------------------------------------

function TStringHashMap.Has(const s: string): Boolean;
var
  ppn: PPHashNode;
begin
  ppn := FindNode(s);
  Result := ppn^ <> nil;
end;

//--------------------------------------------------------------------------------------------------

function TStringHashMap.Find(const s: string; var p{: Pointer}): Boolean;
var
  ppn: PPHashNode;
begin
  ppn := FindNode(s);
  Result := ppn^ <> nil;
  if Result then
    Pointer(p) := ppn^^.Ptr;
end;

//--------------------------------------------------------------------------------------------------

type
  PFindDataResult = ^TFindDataResult;
  TFindDataResult = record
    Found: Boolean;
    ValueToFind: Pointer;
    Key: string;
  end;

function Iterate_FindData(AUserData: Pointer; const AStr: string;
  var APtr: Pointer): Boolean;
var
  pfdr: PFindDataResult;
begin
  pfdr := PFindDataResult(AUserData);
  pfdr^.Found := (APtr = pfdr^.ValueToFind);
  Result := not pfdr^.Found;
  if pfdr^.Found then
    pfdr^.Key := AStr;
end;

//--------------------------------------------------------------------------------------------------

function TStringHashMap.FindData(const p{: Pointer}; var s: string): Boolean;
var
  pfdr: PFindDataResult;
begin
  New(pfdr);
  try
    pfdr^.Found := False;
    pfdr^.ValueToFind := Pointer(p);
    Iterate(pfdr, Iterate_FindData);
    Result := pfdr^.Found;
    if Result then
      s := pfdr^.Key;
  finally
    Dispose(pfdr);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TStringHashMap.Clear;
var
  i: Integer;
  ppn: PPHashNode;
begin
  for i := 0 to FHashSize - 1 do
  begin
    ppn := @FList^[i];
    if ppn^ <> nil then
      DeleteNodes(ppn^);
  end;
  FCount := 0;
end;

//--------------------------------------------------------------------------------------------------

initialization
finalization
  if @_CaseInsensitiveTraits <> nil then
    _CaseInsensitiveTraits.Free;

  if @_CaseSensitiveTraits <> nil then
    _CaseSensitiveTraits.Free;

// History:

// $Log$
// Revision 1.6  2004/07/28 18:00:51  marquardt
// various style cleanings, some minor fixes
//
// Revision 1.5  2004/05/18 18:58:04  rrossmair
// documentation extracted to StrHashMap.dtx
//
// Revision 1.4  2004/05/05 00:11:24  mthoma
// Updated headers: Added donors as contributors, adjusted the initial authors, added cvs names when they were not obvious. Changed $data to $date where necessary,
//
// Revision 1.3  2004/04/06 04:53:18  peterjhaas
// adapt compiler conditions, add log entry
//

end.

