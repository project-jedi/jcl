//------------------------------------------------------------------------------
// The Delphi Container Library
// Jean-Philippe BEMPEL aka RDM
// rdm_30@yahoo.com
//------------------------------------------------------------------------------
unit ArraySet;

{$I dcl.inc}

interface

uses DCL_intf, DCLUtil, AbstractContainer, ArrayList;

type
  TIntfArraySet = class(TIntfArrayList, IIntfCollection, IIntfSet, IIntfCloneable)
  protected
    function Add(AObject: IInterface): Boolean;
    function AddAll(ACollection: IIntfCollection): Boolean;
  protected
  { IIntfSet }
    procedure Intersect(ACollection: IIntfCollection);
    procedure Subtract(ACollection: IIntfCollection);
    procedure Union(ACollection: IIntfCollection);
  end;

  TStrArraySet = class(TStrArrayList, IStrCollection, IStrSet, ICloneable)
  protected
    function Add(const AString: string): Boolean;
    function AddAll(ACollection: IStrCollection): Boolean;
  protected
  { IStrSet }
    procedure Intersect(ACollection: IStrCollection);
    procedure Subtract(ACollection: IStrCollection);
    procedure Union(ACollection: IStrCollection);
  end;

  TArraySet = class(TArrayList, ICollection, ISet, ICloneable)
  protected
    function Add(AObject: TObject): Boolean;
    function AddAll(ACollection: ICollection): Boolean; 
  protected
  { ISet }
    procedure Intersect(ACollection: ICollection);
    procedure Subtract(ACollection: ICollection);
    procedure Union(ACollection: ICollection);
  end;

implementation

{ TIntfArraySet }

function TIntfArraySet.Add(AObject: IInterface): Boolean;
begin
  Result := False;
  if Contains(AObject) then
    Exit;
  inherited Add(AObject);
  Result := True;
end;

function TIntfArraySet.AddAll(ACollection: IIntfCollection): Boolean;
var
  It: IIntfIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Add(It.Next);
  Result := True;
end;

procedure TIntfArraySet.Intersect(ACollection: IIntfCollection);
begin
  RetainAll(ACollection)
end;

procedure TIntfArraySet.Subtract(ACollection: IIntfCollection);
begin
  RemoveAll(ACollection);
end;

procedure TIntfArraySet.Union(ACollection: IIntfCollection);
begin
  AddAll(ACollection);
end;

{ TStrArraySet }

function TStrArraySet.Add(const AString: string): Boolean;
begin
  Result := False;
  if Contains(AString) then
    Exit;
  inherited Add(AString);
  Result := True;
end;

function TStrArraySet.AddAll(ACollection: IStrCollection): Boolean;
var
  It: IStrIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Add(It.Next);
  Result := True;
end;

procedure TStrArraySet.Intersect(ACollection: IStrCollection);
begin
  RetainAll(ACollection);
end;

procedure TStrArraySet.Subtract(ACollection: IStrCollection);
begin
  RemoveAll(ACollection);
end;

procedure TStrArraySet.Union(ACollection: IStrCollection);
begin
  AddAll(ACollection);
end;

{ TArraySet }

function TArraySet.Add(AObject: TObject): Boolean;
begin
  Result := False;
  if Contains(AObject) then
    Exit;
  inherited Add(AObject);
  Result := True;
end;

function TArraySet.AddAll(ACollection: ICollection): Boolean;
var
  It: IIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Add(It.Next);
  Result := True;
end;

procedure TArraySet.Intersect(ACollection: ICollection);
begin
  RetainAll(ACollection);
end;

procedure TArraySet.Subtract(ACollection: ICollection);
begin
  RemoveAll(ACollection);
end;

procedure TArraySet.Union(ACollection: ICollection);
begin
  AddAll(ACollection);
end;

end.
