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
{ The Original Code is JclStrHashMap.pas.                                      }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ 2001 of these individuals.                                                   }
{                                                                              }
{ Description: String-pointer associative map.                                 }
{ Unit Owner: Barry Kelly.                                                     }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This unit contains a string-pointer associative map. It works by hashing     }
{ the added strings using a passed-in traits object.                           }
{                                                                              }
{ Unit owner: Barry Kelly                                                      }
{ Last modified: June 6, 2001                                                  }
{                                                                              }
{******************************************************************************}

{ This unit contains the implementation of a string / pointer associative
  map. }
unit JclStrHashMap;

{$INCLUDE JCL.INC}

interface

uses SysUtils, JclBase, JclResources;

type
  { Brief: The error exception class used by TStringhashMap. }
  EJclStringHashMapError = class(EJclError);
  { Brief: The type of the return value for hashing functions. }
  THashValue = Cardinal;

type
  { Brief: An abstract class describing the interface for traits classes.
    Description:
      The desired behaviour of TStringHashMap for case sensitive and
      case insensitive maps is different. This class describes the
      interface through which user code can communicate to TStringHashMap
      how string keys added should be treated. Concrete instances are
      returned by the CaseSensitiveTraits and CaseInsensitiveTraits
      functions.

      See also: TCaseSensitiveTraits, TCaseInsensitiveTraits }
  TStringHashMapTraits = class
  public
    { Brief: Returns the hash value of the string s. }
    function Hash(const s: string): Cardinal; virtual; abstract;
    { Brief: Returns a negative integer if l is before r, a positive
      integer if r is before l, and zero if they are equal; all comparisons
      being lexicographical, i.e. not necessarily based on the ordinal
      values of the characters, but may be case insensitive, for example. }
    function Compare(const l, r: string): Integer; virtual; abstract;
  end;

{ Brief: A utility function returning an instance of TCaseSensitiveTraits. }
function CaseSensitiveTraits: TStringHashMapTraits;
{ Brief: A utility function returning an instance of TCaseInsensitiveTraits. }
function CaseInsensitiveTraits: TStringHashMapTraits;

type
  { Brief: The type of the parameter to TStringHashMap.Iterate.
    Description:
      * AUserData:
        The same parameter as was passed to the Iterate method.
      * AStr:
        The value of the string part of the current key.
      * APtr:
        The value of the pointer part of the current key.
    Should return True to continue iterating, or False to
    stop iterating and return to whatever called the Iterate method. }
  TIterateFunc = function(AUserData: Pointer; const AStr: string;
    var APtr: Pointer): Boolean;

  { Brief: The type of the parameter to TStringHashMap.IterateMethod.
    Description:
      These are the meanings of the arguments:<p>
      AUserData:
        The same parameter as was passed to the IterateMethod method.
      AStr:
        The value of the string part of the current key.
      APtr:
        The value of the pointer part of the current key.

      <p>Return value: Should return True to continue iterating, or False to
        stop iterating and return to whatever called the IterateMethod method. }
  TIterateMethod = function(AUserData: Pointer; const AStr: string;
    var APtr: Pointer): Boolean of object;

  { Brief: Pointer to a pointer to a THashNode record. }
  PPHashNode = ^PHashNode;
  { Brief: Pointer to a THashNode record. }
  PHashNode = ^THashNode;
  { Brief: The internal storage record used by TStringHashMap. }
  THashNode = record
    Str: string;
    Ptr: Pointer;
    Left: PHashNode;
    Right: PHashNode;
  end;

  { Internal iterate function pointer type used by the protected
    TStringHashMap.NodeIterate method. }
  TNodeIterateFunc = procedure(AUserData: Pointer; ANode: PPHashNode);

  { Brief: Pointer to a THashArray. }
  PHashArray = ^THashArray;
  { Brief: THashArray is the type of the array used for internal storage in
    TStringHashMap. }
  THashArray = array[0..MaxInt div SizeOf(PHashNode) - 1] of PHashNode;

  { Brief: TStringHashMap is a string-pointer associative array.
    Description:
      TStringHashMap is a string-pointer associative array. It uses a
      hash table for its implementation, which has several ramifications
      for its behaviour:
        * Lookup by string takes a constant time, i.e. if the map is big
          enough it takes just as long to find an item from 10 as from
          10 million (ignoring cache and paging effects).
        * Items are unordered. An iteration of all the items using the
          Iterate or IterateMethod methods won't go through all the items
          in any order that has meaning, i.e. the ordering is essentially
          random. }
  TStringHashMap = class
  private
    FHashSize: Cardinal;
    FCount: Cardinal;
    FList: PHashArray;
    FLeftDelete: Boolean;
    FTraits: TStringHashMapTraits;

    function IterateNode(ANode: PHashNode; AUserData: Pointer;
      AIterateFunc: TIterateFunc): Boolean;
    function IterateMethodNode(ANode: PHashNode; AUserData: Pointer;
      AIterateMethod: TIterateMethod): Boolean;
    procedure NodeIterate(ANode: PPHashNode; AUserData: Pointer;
      AIterateFunc: TNodeIterateFunc);
    procedure SetHashSize(AHashSize: Cardinal);
    procedure DeleteNodes(var q: PHashNode);
    procedure DeleteNode(var q: PHashNode);
  protected
    { Brief: A helper method used by the public methods.

      Parameters:
        s: The string key of the node to find.

      Returns:
        The location of the pointer to the node corresponding to s, if
        s is in the hash table. If s isn't in the table, then it will
        return the location of the pointer (= nil) which must be set
        in order to add s correctly to the table.

      Description:
        Returns, using double indirection, the node corresponding to the
        parameter. The double indirection allows internal implementation
        methods to both search and add using only one search through the
        table. }
    function FindNode(const s: string): PPHashNode;

    {@@$TStringHashMap_Alloc
      <TITLE Customizing TStringHashMap's node allocation strategy.>
      You can customize the way TStringHashMap allocates nodes, to avoid the
      overhead of millions of calls to New, by overriding the AllocNode
      and FreeNode methods. }

    { <GROUP $TStringHashMap_Alloc>
      <TITLE Allocating a node>
      Brief: Allocates a node.
      Returns: A pointer to a newly allocated node, with the Left and Right
        members set to nil. }
    function AllocNode: PHashNode; virtual;
    { <GROUP $TStringHashMap_Alloc>
      <TITLE Freeing a node>
      Brief: Frees a node.
      Parameters:
        ANode: The node to free. }
    procedure FreeNode(ANode: PHashNode); virtual;

    { Brief: Data property getter.
      Parameters:
        s: The key of the node to find.
      Returns:
        The value of the pointer if the node exists, or nil otherwise. }
    function GetData(const s: string): Pointer;
    { Brief: Data property setter.
      Parameters:
        s: The key of the node to set.
        p: The data to set the node's data to.
      Description:
        SetData will create a new node if none exists with key s. }
    procedure SetData(const s: string; p: Pointer);
  public
    { Brief: Constructs a new TStringHashMap instance.
      Parameters:
        ATraits:
          An instance of a descendant of TStringHashMapTraits. This object
          describes how the hash map will hash and compare string values.
        AHashSize:
          An initial size for the internal hash lookup table. This doesn't
          limit the amount of items the hash can hold; it just affects the
          speed of operations on the hash map. See the HashSize property for
          more info.
      Note: AHashSize should <b>not</b> be a power of 2. Use a power of 2
        minus 1, or a prime number. }
    constructor Create(ATraits: TStringHashMapTraits; AHashSize: Cardinal);

    { Brief: Destroys the instance. Call Free instead of Destroy. }
    destructor Destroy; override;

    { public methods }

    { Brief: Adds an association to the map.
      Parameters:
        s: The key to add.
        p: The data to add. It is an untyped constant to avoid typecasting
          issues, and to make code using the map more readable. It should
          be a 4-byte data type, like Integer or Pointer, or some object
          reference. }
    procedure Add(const s: string; const p{: Pointer});

    { Brief: Removes an association from the map.
      Parameters:
        s: The string value of the association to remove.
      Returns:
        The data field of the assocation.
      Description:
        The Remove method isn't as fast at removing items as Clear is; that
        is, calling Remove for every key in the hash will be quite a bit
        slower than calling Clear. The method returns the data field so that
        compact code can be written. See the example for sample usage.

        EJclStringHashMapError will be raised if the item isn't found.
      Example:
        This example demonstrates how the Remove method's return value can
        be used.
        <code>
        var
          myMap: TStringHashMap;
          // ...
          TObject(myMap.Remove('SomeItem')).Free;
        </code> }
    function Remove(const s: string): Pointer;

    { Brief: Removes <b>every</b> instance of p as a data field from the map.
      Parameters:
        p: The data to item. }
    procedure RemoveData(const p{: Pointer});

    { Brief: Iterates through associations in the map.
      Parameters:
        AUserData: A pointer parameter passed through untouched to the
          iterator function.
        AIterateFunc: A function pointer called for every assocation in
          the map; it should return False if it wants to prematurely stop
          the iteration.
      See Also: IterateMethod }
    procedure Iterate(AUserData: Pointer; AIterateFunc: TIterateFunc);

    { Brief: Iterates through associations in the map.
      Parameters:
        AUserData: A pointer parameter passed through untouched to the
          iterator method.
        AIterateMethod: A method pointer called for every assocation in
          the map; it should return False if it wants to prematurely stop
          the iteration.
      See Also: Iterate }
    procedure IterateMethod(AUserData: Pointer; AIterateMethod: TIterateMethod);

    { Brief: Checks if the map has an association with a string key.
      Parameters:
        s: The key to search for.
      Returns:
        True if found, False if there is no assocation with key s in the
          map. }
    function Has(const s: string): Boolean;

    { Brief: Finds the data part of an association corresponding to a string
        key.
      Parameters:
        s: The string key to find.
        p: A reference to a pointer which will be updated if the assocation
          is found. If the assocation isn't found, the pointer isn't touched.
          That is, it won't automatically be set to nil.
      Returns:
        True if the key was found, False otherwise. }
    function Find(const s: string; var p{: Pointer}): Boolean;

    { Brief: Finds the string key for a data value.
      Parameters:
        p: The data value to find.
        s: A reference to the string key to set if found.
      Returns:
        True if found, false otherwise.
      Description:
        FindData will find the first association in the map with a data
        field equal to p. However, the order of the search has no meaning;
        if there are multiple instances of p in the node, the key returned
        is not in any particular order. }
    function FindData(const p{: Pointer}; var s: string): Boolean;

    { Brief: Clears the map of all associations. }
    procedure Clear;

    { Brief: The number of assocations stored in the map. }
    property Count: Cardinal read FCount;

    { Brief: Allows access to the map like an array looked up by string.
      Description:
        This is the default property, so it needn't be specified directly.
        If the key corresponding to the string isn't found, an assocation
        is added if setting, but isn't added if getting. Reading from the
        map with a key not in the map will return nil.
      Example:
        Demonstrates use of the TStringHashMap.Data property.
        <code>
        var
          myMap: TStringHashMap;
          // ...
          myMap['this'] := Self;
          if myMap['something'] = nil then
            // 'something' not found
        </code> }
    property Data[const s: string]: Pointer read GetData write SetData; default;
    { Brief: Returns the traits object being used by the map. }
    property Traits: TStringHashMapTraits read FTraits;
    { Brief: The internal hash table size.
      Description:
        The hash table size affects all operations on the hash map. For maps
        designed to work with large numbers of assocations, the table should
        be correspondingly larger.
      Note: Don't change the value of this property too often, as it will
        force all items in the table to be rehashed, and this can take
        some time. }
    property HashSize: Cardinal read FHashSize write SetHashSize;
  end;

{ str=case sensitive, text=case insensitive }

{ Brief: Case sensitive hash of string s. }
function StrHash(const s: string): THashValue;
{ Brief: Case insensitive hash of string s. }
function TextHash(const s: string): THashValue;
{ Brief: A utility function which computes a hash a buffer.
  Parameters:
    AValue: A reference to the buffer to compute a hash value for.
    ASize: The size of the buffer. Use SizeOf where possible. }
function DataHash(var AValue; ASize: Cardinal): THashValue;

{ Brief: A utility iterating function that calls TObject.Free for all
  data items in a map.
  Example:
    This frees all objects in the map and clears the map.
    <code>
    var
      myMap: TStringHashMap;
      // ...
      myMap.Iterate(nil, Iterate_FreeObjects);
      myMap.Clear;
    </code> }
function Iterate_FreeObjects(AUserData: Pointer; const AStr: string;
  var AData: Pointer): Boolean;
{ Brief: A utility iterating function that calls Dispose for all data
  items in a map.
  See Also: Iterate_FreeObjects }
function Iterate_Dispose(AUserData: Pointer; const AStr: string;
  var AData: Pointer): Boolean;
{ Brief: A utility iterating function that calls FreeMem for all data
  items in a map.
  See Also: Iterate_FreeMem }
function Iterate_FreeMem(AUserData: Pointer; const AStr: string;
  var AData: Pointer): Boolean;

type
  { Brief: A useful concrete descendant of TStringHashMapTraits which
    implements case sensitive traits, with order based on ordinal value.
    See Also: TCaseInsensitiveTraits }
  TCaseSensitiveTraits = class(TStringHashMapTraits)
  public
    function Hash(const s: string): Cardinal; override;
    function Compare(const l, r: string): Integer; override;
  end;

type
  { Brief: A useful concrete descendant of TStringHashMapTraits which
    implements case insensitive traits, with order based on ordinal value.
    See Also: TCaseSensitiveTraits }
  TCaseInsensitiveTraits = class(TStringHashMapTraits)
  public
    function Hash(const s: string): Cardinal; override;
    function Compare(const l, r: string): Integer; override;
  end;

implementation

{
  ======================================================================
  Case Sensitive & Insensitive Traits
  ======================================================================
}

function TCaseSensitiveTraits.Compare(const l, r: string): Integer;
begin
  Result := CompareStr(l, r);
end;

function TCaseSensitiveTraits.Hash(const s: string): Cardinal;
begin
  Result := StrHash(s);
end;

function TCaseInsensitiveTraits.Compare(const l, r: string): Integer;
begin
  Result := CompareText(l, r);
end;

function TCaseInsensitiveTraits.Hash(const s: string): Cardinal;
begin
  Result := TextHash(s);
end;

var
  _CaseSensitiveTraits: TCaseSensitiveTraits;

function CaseSensitiveTraits: TStringHashMapTraits;
begin
  if _CaseSensitiveTraits = nil then
    _CaseSensitiveTraits := TCaseSensitiveTraits.Create;
  Result := _CaseSensitiveTraits;
end;

var
  _CaseInsensitiveTraits: TCaseInsensitiveTraits;

function CaseInsensitiveTraits: TStringHashMapTraits;
begin
  if _CaseInsensitiveTraits = nil then
    _CaseInsensitiveTraits := TCaseInsensitiveTraits.Create;
  Result := _CaseInsensitiveTraits;
end;

function Iterate_FreeObjects(AUserData: Pointer; const AStr: string;
  var AData: Pointer): Boolean;
begin
  TObject(AData).Free;
  AData := nil;
  Result := True;
end;

function Iterate_Dispose(AUserData: Pointer; const AStr: string;
  var AData: Pointer): Boolean;
begin
  Dispose(AData);
  AData := nil;
  Result := True;
end;

function Iterate_FreeMem(AUserData: Pointer; const AStr: string;
  var AData: Pointer): Boolean;
begin
  FreeMem(AData);
  AData := nil;
  Result := True;
end;

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
  {TODO I should really be processing 4 bytes at once... }
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
  {TODO I should really be processing 4 bytes at once... }
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
  {TODO I should really be processing 4 bytes at once... }
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

{
  ======================================================================
  TStringHashMap
  ======================================================================
}
constructor TStringHashMap.Create(ATraits: TStringHashMapTraits; AHashSize: Cardinal);
begin
  Assert(ATraits <> nil, 'HashList must have traits');
  SetHashSize(AHashSize);
  FTraits := ATraits;
end;

destructor TStringHashMap.Destroy;
begin
  Clear;
  SetHashSize(0);
  inherited Destroy;
end;

{
  protected methods
}
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
      else if r > 0 then
        ppn := @ppn^^.Right
      else
        Break;

      { check for empty position after drilling left or right }
      if ppn^ = nil then
        Break;
    end;

  Result := ppn;
end;

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

procedure TStringHashMap.DeleteNodes(var q: PHashNode);
begin
  if q^.Left <> nil then
    DeleteNodes(q^.Left);
  if q^.Right <> nil then
    DeleteNodes(q^.Right);
  FreeNode(q);
  q := nil;
end;

function TStringHashMap.AllocNode: PHashNode;
begin
  New(Result);
  Result^.Left := nil;
  Result^.Right := nil;
end;

procedure TStringHashMap.FreeNode(ANode: PHashNode);
begin
  Dispose(ANode);
end;

{
  property access
}
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

{ public methods }

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

procedure TStringHashMap.IterateMethod(AUserData: Pointer;
  AIterateMethod: TIterateMethod);
var
  i: Integer;
begin
  for i := 0 to FHashSize - 1 do
    if not IterateMethodNode(FList^[i], AUserData, AIterateMethod) then
      Break;
end;

procedure TStringHashMap.Iterate(AUserData: Pointer; AIterateFunc: TIterateFunc);
var
  i: Integer;
begin
  for i := 0 to FHashSize - 1 do
    if not IterateNode(FList^[i], AUserData, AIterateFunc) then
      Break;
end;

function TStringHashMap.Has(const s: string): Boolean;
var
  ppn: PPHashNode;
begin
  ppn := FindNode(s);
  Result := ppn^ <> nil;
end;

function TStringHashMap.Find(const s: string; var p{: Pointer}): Boolean;
var
  ppn: PPHashNode;
begin
  ppn := FindNode(s);
  Result := ppn^ <> nil;
  if Result then
    Pointer(p) := ppn^^.Ptr;
end;

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

end.

