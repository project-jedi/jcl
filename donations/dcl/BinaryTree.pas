//------------------------------------------------------------------------------
// The Delphi Container Library
// Jean-Philippe BEMPEL aka RDM
// rdm_30@yahoo.com
//------------------------------------------------------------------------------
unit BinaryTree;

{$I dcl.inc}
{ $DEFINE RECURSIVE}
interface

uses DCL_intf, DCLUtil, Algorithms, AbstractContainer;

type
	TTreeColor = (tcBlack, tcRed);

  PIntfBinaryNode = ^TIntfBinaryNode;
	TIntfBinaryNode = record
  	Obj: IInterface;
		Left: PIntfBinaryNode;
    Right: PIntfBinaryNode;
    Parent: PIntfBinaryNode;
    Color: TTreeColor;
  end;

  PStrBinaryNode = ^TStrBinaryNode;
	TStrBinaryNode = record
  	Str: string;
		Left: PStrBinaryNode;
    Right: PStrBinaryNode;
    Parent: PStrBinaryNode;
    Color: TTreeColor;
  end;

  PBinaryNode = ^TBinaryNode;
	TBinaryNode = record
  	Obj: TObject;
		Left: PBinaryNode;
    Right: PBinaryNode;
    Parent: PBinaryNode;
    Color: TTreeColor;
  end;

	TIntfBinaryTree = class(TAbstractContainer, IIntfCollection, IIntfTree,
  	IIntfCloneable)
  private
  	FComparator: TIntfCompare;
    FCount: Integer;
  	FRoot: PIntfBinaryNode;
    FTraverseOrder: TTraverseOrder;
    procedure RotateLeft(Node: PIntfBinaryNode);
    procedure RotateRight(Node: PIntfBinaryNode);
  protected
  { IIntfCollection }
    function Add(AObject: IInterface): Boolean;
    function AddAll(ACollection: IIntfCollection): Boolean;
    procedure Clear;
    function Contains(AObject: IInterface): Boolean;
    function ContainsAll(ACollection: IIntfCollection): Boolean;
    function Equals(ACollection: IIntfCollection): Boolean;
    function First: IIntfIterator;
    function IsEmpty: Boolean;
    function Last: IIntfIterator;
    function Remove(AObject: IInterface): Boolean;
    function RemoveAll(ACollection: IIntfCollection): Boolean;
    function RetainAll(ACollection: IIntfCollection): Boolean;
    function Size: Integer;
  protected
  { IIntfTree }
    function GetTraverseOrder: TTraverseOrder;
    procedure SetTraverseOrder(Value: TTraverseOrder);
  protected
  { IIntfCloneable }
    function Clone: IInterface;
  public
  	constructor Create; overload;
    constructor Create(Comparator: TIntfCompare); overload;
    destructor Destroy; override;
  end;

	TStrBinaryTree = class(TAbstractContainer, IStrCollection, IStrTree, ICloneable)
  private
  	FComparator: TStrCompare;
    FCount: Integer;
  	FRoot: PStrBinaryNode;
    FTraverseOrder: TTraverseOrder;
    procedure RotateLeft(Node: PStrBinaryNode);
    procedure RotateRight(Node: PStrBinaryNode);
  protected
  { ICollection }
    function Add(const AString: string): Boolean;
    function AddAll(ACollection: IStrCollection): Boolean;
    procedure Clear;
    function Contains(const AString: string): Boolean;
    function ContainsAll(ACollection: IStrCollection): Boolean;
    function Equals(ACollection: IStrCollection): Boolean;
    function First: IStrIterator;
    function IsEmpty: Boolean;
    function Last: IStrIterator;
    function Remove(const AString: string): Boolean;
    function RemoveAll(ACollection: IStrCollection): Boolean;
    function RetainAll(ACollection: IStrCollection): Boolean;
    function Size: Integer;
  protected
  { IStrTree }
    function GetTraverseOrder: TTraverseOrder;
    procedure SetTraverseOrder(Value: TTraverseOrder);
  protected
  { ICloneable }
    function Clone: TObject;
  public
  	constructor Create; overload;
    constructor Create(Comparator: TStrCompare); overload;
    destructor Destroy; override;
  end;

	TBinaryTree = class(TAbstractContainer, ICollection, ITree,	ICloneable)
  private
  	FComparator: TCompare;
    FCount: Integer;
  	FRoot: PBinaryNode;
    FTraverseOrder: TTraverseOrder;
    procedure RotateLeft(Node: PBinaryNode);
    procedure RotateRight(Node: PBinaryNode);
  protected
  { ICollection }
    function Add(AObject: TObject): Boolean;
    function AddAll(ACollection: ICollection): Boolean;
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function ContainsAll(ACollection: ICollection): Boolean;
    function Equals(ACollection: ICollection): Boolean;
    function First: IIterator;
    function IsEmpty: Boolean;
    function Last: IIterator;
    function Remove(AObject: TObject): Boolean;
    function RemoveAll(ACollection: ICollection): Boolean;
    function RetainAll(ACollection: ICollection): Boolean;
    function Size: Integer;
  protected
  { ITree }
    function GetTraverseOrder: TTraverseOrder;
    procedure SetTraverseOrder(Value: TTraverseOrder);
  protected
  { ICloneable }
    function Clone: TObject;
  public
  	constructor Create; overload;
    constructor Create(Comparator: TCompare); overload;
    destructor Destroy; override;
  end;

implementation

type
  TIntfItr = class(TAbstractContainer, IIntfIterator)
  protected
    FCursor: PIntfBinaryNode;
    FOwnList: TIntfBinaryTree;
    FLastRet: PIntfBinaryNode;
  protected
  { IIntfIterator }
    procedure Add(AObject: IInterface);
    function GetObject: IInterface;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Next: IInterface; virtual;
    function NextIndex: Integer;
    function Previous: IInterface; virtual;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetObject(AObject: IInterface);
  public
    constructor Create(OwnList: TIntfBinaryTree; Start: PIntfBinaryNode);
    destructor Destroy; override;
  end;

  TPreOrderIntfItr = class(TIntfItr, IIntfIterator)
  protected
  { IIntfIterator }
    function Next: IInterface; override;
    function Previous: IInterface; override;
  end;

  TOrderIntfItr = class(TIntfItr, IIntfIterator)
  protected
  { IIntfIterator }
    function Next: IInterface; override;
    function Previous: IInterface; override;
  end;

  TPostOrderIntfItr = class(TIntfItr, IIntfIterator)
  protected
  { IIntfIterator }
    function Next: IInterface; override;
    function Previous: IInterface; override;
  end;

  TStrItr = class(TAbstractContainer, IStrIterator)
  protected
    FCursor: PStrBinaryNode;
    FOwnList: TStrBinaryTree;
    FLastRet: PStrBinaryNode;
  protected
  { IStrIterator }
    procedure Add(const AString: string);
    function GetString: string;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Next: string; virtual; 
    function NextIndex: Integer;
    function Previous: string; virtual;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetString(const AString: string);
  public
    constructor Create(OwnList: TStrBinaryTree; Start: PStrBinaryNode);
    destructor Destroy; override;
  end;

  TPreOrderStrItr = class(TStrItr, IStrIterator)
  protected
  { IStrIterator }
    function Next: string; override;
    function Previous: string; override;
  end;

  TOrderStrItr = class(TStrItr, IStrIterator)
  protected
  { IStrIterator }
    function Next: string; override;
    function Previous: string; override;
  end;

  TPostOrderStrItr = class(TStrItr, IStrIterator)
  protected
  { IStrIterator }
    function Next: string; override;
    function Previous: string; override;
  end;

  TItr = class(TAbstractContainer, IIterator)
  protected
    FCursor: PBinaryNode;
    FOwnList: TBinaryTree;
    FLastRet: PBinaryNode;
  protected
  { IIntfIterator }
    procedure Add(AObject: TObject);
    function GetObject: TObject;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Next: TObject; virtual;
    function NextIndex: Integer;
    function Previous: TObject; virtual;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetObject(AObject: TObject);
  public
    constructor Create(OwnList: TBinaryTree; Start: PBinaryNode);
    destructor Destroy; override;
  end;

  TPreOrderItr = class(TItr, IIterator)
  protected
  { IIterator }
    function Next: TObject; override;
    function Previous: TObject; override;
  end;

  TOrderItr = class(TItr, IIterator)
  protected
  { IIterator }
    function Next: TObject; override;
    function Previous: TObject; override;
  end;

  TPostOrderItr = class(TItr, IIterator)
  protected
  { IIterator }
    function Next: TObject; override;
    function Previous: TObject; override;
  end;

{ TIntfItr }

procedure TIntfItr.Add(AObject: IInterface);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  FOwnList.Add(AObject);
end;

constructor TIntfItr.Create(OwnList: TIntfBinaryTree; Start: PIntfBinaryNode);
begin
  inherited Create;
  FCursor := Start;
  FOwnList := OwnList;
  FOwnList._AddRef; // Add a ref because FOwnList is not an interface !
  FLastRet := nil;
end;

destructor TIntfItr.Destroy;
begin
  FOwnList._Release;
  inherited;
end;

function TIntfItr.GetObject: IInterface;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := FCursor.Obj;
end;

function TIntfItr.HasNext: Boolean;
begin
  Result := FCursor <> nil;
end;                           

function TIntfItr.HasPrevious: Boolean;
begin
  Result := FCursor <> nil;
end;

function TIntfItr.Next: IInterface;
begin
end;

function TIntfItr.NextIndex: Integer;
begin
  // No index
  raise EDCLOperationNotSupported.Create(SOperationNotSupported);
end;

function TIntfItr.Previous: IInterface;
begin
end;

function TIntfItr.PreviousIndex: Integer;
begin
  // No index
  raise EDCLOperationNotSupported.Create(SOperationNotSupported);
end;

procedure TIntfItr.Remove;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	FOwnList.Remove(Next);
end;

procedure TIntfItr.SetObject(AObject: IInterface);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  FCursor.Obj := AObject;
end;

{ TPreOrderIntfItr }

function TPreOrderIntfItr.Next: IInterface;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := FCursor.Obj;
  FLastRet := FCursor;
  if FCursor.Left <> nil then
  	FCursor := FCursor.Left
  else if FCursor.Right <> nil then
  	FCursor := FCursor.Right
  else
  begin
  	FCursor := FCursor.Parent;
    while (FCursor <> nil) and (FCursor.Left <> FLastRet) do // come from Right
    begin
    	FLastRet := FCursor;
			FCursor := FCursor.Parent;
    end;
    while (FCursor <> nil) and (FCursor.Right = nil) do
    begin
    	FLastRet := FCursor;
			FCursor := FCursor.Parent;
    end;
    if FCursor = nil then  // root
    	Exit;
    FCursor := FCursor.Right
  end;
end;

function TPreOrderIntfItr.Previous: IInterface;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := FCursor.Obj;
  FLastRet := FCursor;
  FCursor := FCursor.Parent;
  if (FCursor <> nil) and (FCursor.Left <> FLastRet) then // come from Right
    if FCursor.Left <> nil then
    begin
      FLastRet := FCursor;
      FCursor := FCursor.Left;
      while FCursor.Right <> nil do
      begin
        FLastRet := FCursor;
        FCursor := FCursor.Right;
      end;
    end;
end;

{ TOrderIntfItr }

function TOrderIntfItr.Next: IInterface;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if FCursor.Left <> FLastRet then
    while FCursor.Left <> nil do
	    FCursor := FCursor.Left;
  Result := FCursor.Obj;
  FLastRet := FCursor;
  if FCursor.Right <> nil then
    FCursor := FCursor.Right
  else
  begin
    FCursor := FCursor.Parent;
    while (FCursor <> nil) and (FCursor.Right = FLastRet) do
    begin
      FLastRet := FCursor;
      FCursor := FCursor.Parent;
    end;
  end;
end;

function TOrderIntfItr.Previous: IInterface;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := FCursor.Obj;
  FLastRet := FCursor;
  if (FCursor.Left <> nil) then
  begin
  	FCursor := FCursor.Left;
    while FCursor.Right <> nil do
    begin
      FLastRet := FCursor;
      FCursor := FCursor.Right;
    end;
  end
  else
  begin
    FCursor := FCursor.Parent;
    while (FCursor <> nil) and (FCursor.Right <> FLastRet) do // Come from Left
    begin
      FLastRet := FCursor;
      FCursor := FCursor.Parent;
    end;
  end;
end;

{ TPostOrderIntfItr }

function TPostOrderIntfItr.Next: IInterface;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if (FCursor.Left <> FLastRet) and (FCursor.Right <> FLastRet) then 
    while FCursor.Left <> nil do
	    FCursor := FCursor.Left;
	if (FCursor.Right <> nil) and (FCursor.Right <> FLastRet) then
  begin
  	FCursor := FCursor.Right;
    while FCursor.Left <> nil do
	    FCursor := FCursor.Left;
    if FCursor.Right <> nil then // particular worst case
	    FCursor := FCursor.Right;
  end;
  Result := FCursor.Obj;
  FLastRet := FCursor;
  FCursor := FCursor.Parent;
end;

function TPostOrderIntfItr.Previous: IInterface;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := FCursor.Obj;
  FLastRet := FCursor;
  if (FCursor.Right <> nil) and (FCursor.Right <> FLastRet) then
	  FCursor := FCursor.Right
  else
  begin
	  FCursor := FCursor.Parent;
    while (FCursor <> nil) and ((FCursor.Left = nil) or (FCursor.Left = FLastRet)) do
    begin
      FLastRet := FCursor;
      FCursor := FCursor.Parent;
    end;
    if FCursor = nil then // Root
    	Exit;
    FCursor := FCursor.Left;
  end;
end;

{ TStrItr }

procedure TStrItr.Add(const AString: string);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  FOwnList.Add(AString);
end;

constructor TStrItr.Create(OwnList: TStrBinaryTree; Start: PStrBinaryNode);
begin
  inherited Create;
  FCursor := Start;
  FOwnList := OwnList;
  FOwnList._AddRef; // Add a ref because FOwnList is not an interface !
  FLastRet := nil;
end;

destructor TStrItr.Destroy;
begin
  FOwnList._Release;
  inherited;
end;

function TStrItr.GetString: string;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := FCursor.Str;
end;

function TStrItr.HasNext: Boolean;
begin
  Result := FCursor <> nil;
end;

function TStrItr.HasPrevious: Boolean;
begin
  Result := FCursor <> nil;
end;

function TStrItr.Next: string;
begin
end;

function TStrItr.NextIndex: Integer;
begin
  // No index
  raise EDCLOperationNotSupported.Create(SOperationNotSupported);
end;

function TStrItr.Previous: string;
begin
end;

function TStrItr.PreviousIndex: Integer;
begin
  // No index
  raise EDCLOperationNotSupported.Create(SOperationNotSupported);
end;

procedure TStrItr.Remove;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	FOwnList.Remove(Next);
end;

procedure TStrItr.SetString(const AString: string);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  FCursor.Str := AString;
end;

{ TPreOrderStrItr }

function TPreOrderStrItr.Next: string;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := FCursor.Str;
  FLastRet := FCursor;
  if FCursor.Left <> nil then
  	FCursor := FCursor.Left
  else if FCursor.Right <> nil then
  	FCursor := FCursor.Right
  else
  begin
  	FCursor := FCursor.Parent;
    while (FCursor <> nil) and (FCursor.Left <> FLastRet) do // come from Right
    begin
    	FLastRet := FCursor;
			FCursor := FCursor.Parent;
    end;
    while (FCursor <> nil) and (FCursor.Right = nil) do
    begin
    	FLastRet := FCursor;
			FCursor := FCursor.Parent;
    end;
    if FCursor = nil then  // root
    	Exit;
    FCursor := FCursor.Right
  end;
end;

function TPreOrderStrItr.Previous: string;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := FCursor.Str;
  FLastRet := FCursor;
  FCursor := FCursor.Parent;
  if (FCursor <> nil) and (FCursor.Left <> FLastRet) then // come from Right
    if FCursor.Left <> nil then
    begin
      FLastRet := FCursor;
      FCursor := FCursor.Left;
      while FCursor.Right <> nil do
      begin
        FLastRet := FCursor;
        FCursor := FCursor.Right;
      end;
    end;
end;

{ TOrderStrItr }

function TOrderStrItr.Next: string;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if FCursor.Left <> FLastRet then
    while FCursor.Left <> nil do
	    FCursor := FCursor.Left;
  Result := FCursor.Str;
  FLastRet := FCursor;
  if FCursor.Right <> nil then
    FCursor := FCursor.Right
  else
  begin
    FCursor := FCursor.Parent;
    while (FCursor <> nil) and (FCursor.Right = FLastRet) do
    begin
      FLastRet := FCursor;
      FCursor := FCursor.Parent;
    end;
  end;
end;

function TOrderStrItr.Previous: string;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := FCursor.Str;
  FLastRet := FCursor;
  if (FCursor.Left <> nil) then
  begin
  	FCursor := FCursor.Left;
    while FCursor.Right <> nil do
    begin
      FLastRet := FCursor;
      FCursor := FCursor.Right;
    end;
  end
  else
  begin
    FCursor := FCursor.Parent;
    while (FCursor <> nil) and (FCursor.Right <> FLastRet) do // Come from Left
    begin
      FLastRet := FCursor;
      FCursor := FCursor.Parent;
    end;
  end;
end;

{ TPostOrderStrItr }

function TPostOrderStrItr.Next: string;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if (FCursor.Left <> FLastRet) and (FCursor.Right <> FLastRet) then 
    while FCursor.Left <> nil do
	    FCursor := FCursor.Left;
	if (FCursor.Right <> nil) and (FCursor.Right <> FLastRet) then
  begin
  	FCursor := FCursor.Right;
    while FCursor.Left <> nil do
	    FCursor := FCursor.Left;
    if FCursor.Right <> nil then // particular worst case
	    FCursor := FCursor.Right;
  end;
  Result := FCursor.Str;
  FLastRet := FCursor;
  FCursor := FCursor.Parent;
end;

function TPostOrderStrItr.Previous: string;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := FCursor.Str;
  FLastRet := FCursor;
  if (FCursor.Right <> nil) and (FCursor.Right <> FLastRet) then
	  FCursor := FCursor.Right
  else
  begin
	  FCursor := FCursor.Parent;
    while (FCursor <> nil) and ((FCursor.Left = nil) or (FCursor.Left = FLastRet)) do
    begin
      FLastRet := FCursor;
      FCursor := FCursor.Parent;
    end;
    if FCursor = nil then // Root
    	Exit;
    FCursor := FCursor.Left;
  end;
end;

{ TItr }

procedure TItr.Add(AObject: TObject);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  FOwnList.Add(AObject);
end;

constructor TItr.Create(OwnList: TBinaryTree; Start: PBinaryNode);
begin
  inherited Create;
  FCursor := Start;
  FOwnList := OwnList;
  FOwnList._AddRef; // Add a ref because FOwnList is not an interface !
  FLastRet := nil;
end;

destructor TItr.Destroy;
begin
  FOwnList._Release;
  inherited;
end;

function TItr.GetObject: TObject;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := FCursor.Obj;
end;

function TItr.HasNext: Boolean;
begin
	Result := FCursor <> nil;
end;

function TItr.HasPrevious: Boolean;
begin
	Result := FCursor <> nil;
end;

function TItr.Next: TObject;
begin
	Result := nil; // overriden in derived class
end;

function TItr.NextIndex: Integer;
begin
  // No index
  raise EDCLOperationNotSupported.Create(SOperationNotSupported);
end;

function TItr.Previous: TObject;
begin
	Result := nil; // overriden in derived class
end;

function TItr.PreviousIndex: Integer;
begin
  // No index
  raise EDCLOperationNotSupported.Create(SOperationNotSupported);
end;

procedure TItr.Remove;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	FOwnList.Remove(Next);
end;

procedure TItr.SetObject(AObject: TObject);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  FCursor.Obj := AObject;
end;

{ TPreOrderItr }

function TPreOrderItr.Next: TObject;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := FCursor.Obj;
  FLastRet := FCursor;
  if FCursor.Left <> nil then
  	FCursor := FCursor.Left
  else if FCursor.Right <> nil then
  	FCursor := FCursor.Right
  else
  begin
  	FCursor := FCursor.Parent;
    while (FCursor <> nil) and (FCursor.Left <> FLastRet) do // come from Right
    begin
    	FLastRet := FCursor;
			FCursor := FCursor.Parent;
    end;
    while (FCursor <> nil) and (FCursor.Right = nil) do
    begin
    	FLastRet := FCursor;
			FCursor := FCursor.Parent;
    end;
    if FCursor = nil then  // root
    	Exit;
    FCursor := FCursor.Right
  end;
end;

function TPreOrderItr.Previous: TObject;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := FCursor.Obj;
  FLastRet := FCursor;
  FCursor := FCursor.Parent;
  if (FCursor <> nil) and (FCursor.Left <> FLastRet) then // come from Right
    if FCursor.Left <> nil then
    begin
      FLastRet := FCursor;
      FCursor := FCursor.Left;
      while FCursor.Right <> nil do
      begin
        FLastRet := FCursor;
        FCursor := FCursor.Right;
      end;
    end;
end;

{ TOrderItr }

function TOrderItr.Next: TObject;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if FCursor.Left <> FLastRet then
    while FCursor.Left <> nil do
	    FCursor := FCursor.Left;
  Result := FCursor.Obj;
  FLastRet := FCursor;
  if FCursor.Right <> nil then
    FCursor := FCursor.Right
  else
  begin
    FCursor := FCursor.Parent;
    while (FCursor <> nil) and (FCursor.Right = FLastRet) do
    begin
      FLastRet := FCursor;
      FCursor := FCursor.Parent;
    end;
  end;
end;

function TOrderItr.Previous: TObject;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := FCursor.Obj;
  FLastRet := FCursor;
  if (FCursor.Left <> nil) then
  begin
  	FCursor := FCursor.Left;
    while FCursor.Right <> nil do
    begin
      FLastRet := FCursor;
      FCursor := FCursor.Right;
    end;
  end
  else
  begin
    FCursor := FCursor.Parent;
    while (FCursor <> nil) and (FCursor.Right <> FLastRet) do // Come from Left
    begin
      FLastRet := FCursor;
      FCursor := FCursor.Parent;
    end;
  end;
end;

{ TPostOrderItr }

function TPostOrderItr.Next: TObject;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if (FCursor.Left <> FLastRet) and (FCursor.Right <> FLastRet) then 
    while FCursor.Left <> nil do
	    FCursor := FCursor.Left;
	if (FCursor.Right <> nil) and (FCursor.Right <> FLastRet) then
  begin
  	FCursor := FCursor.Right;
    while FCursor.Left <> nil do
	    FCursor := FCursor.Left;
    if FCursor.Right <> nil then // particular worst case
	    FCursor := FCursor.Right;
  end;
  Result := FCursor.Obj;
  FLastRet := FCursor;
  FCursor := FCursor.Parent;
end;

function TPostOrderItr.Previous: TObject;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := FCursor.Obj;
  FLastRet := FCursor;
  if (FCursor.Right <> nil) and (FCursor.Right <> FLastRet) then
	  FCursor := FCursor.Right
  else
  begin
	  FCursor := FCursor.Parent;
    while (FCursor <> nil) and ((FCursor.Left = nil) or (FCursor.Left = FLastRet)) do
    begin
      FLastRet := FCursor;
      FCursor := FCursor.Parent;
    end;
    if FCursor = nil then // Root
    	Exit;
    FCursor := FCursor.Left;
  end;
end;

{ TIntfBinaryTree }

function TIntfBinaryTree.Add(AObject: IInterface): Boolean;
var
	NewNode: PIntfBinaryNode;
  Current, Save: PIntfBinaryNode;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Result := False;
	if AObject = nil then
  	Exit;
  New(NewNode);
  FillChar(NewNode^, SizeOf(NewNode^), 0); // Init to nil
  NewNode.Obj := AObject;
  // Insert into right place
  Save := nil;
	Current := FRoot;
  while Current <> nil do
  begin
  	Save := Current;
  	if FComparator(NewNode.Obj, Current.Obj) < 0 then
    	Current := Current.Left
    else
    	Current := Current.Right;
  end;
  NewNode.Parent := Save;
  if Save = nil then
  	FRoot := NewNode
  else if FComparator(NewNode.Obj, Save.Obj) < 0 then
  	Save.Left := NewNode
  else
  	Save.Right := NewNode;
  // RB balanced
  NewNode.Color := tcRed;
  while (NewNode <> FRoot) and (NewNode.Parent.Color = tcRed) do
  begin
  	if (NewNode.Parent.Parent <> nil) and (NewNode.Parent = NewNode.Parent.Parent.Left) then
    begin
    	Current := NewNode.Parent.Parent.Right;
      if Current.Color = tcRed then
      begin
      	NewNode.Parent.Color := tcBlack;
        Current.Color := tcBlack;
        NewNode.Parent.Parent.Color := tcRed;
        NewNode := NewNode.Parent.Parent;
      end
      else
      begin
      	if NewNode = NewNode.Parent.Right then
        begin
          NewNode := NewNode.Parent;
          RotateLeft(NewNode);
        end;
        NewNode.Parent.Color := tcBlack;
        NewNode.Parent.Parent.Color := tcRed;
        RotateRight(NewNode.Parent.Parent);
      end;
    end
    else
    begin
    	if NewNode.Parent.Parent = nil then
      	Current := nil
      else
	    	Current := NewNode.Parent.Parent.Left;
      if (Current <> nil) and (Current.Color = tcRed) then
      begin
      	NewNode.Parent.Color := tcBlack;
        Current.Color := tcBlack;
        NewNode.Parent.Parent.Color := tcRed;
        NewNode := NewNode.Parent.Parent;
      end
      else
      begin
      	if NewNode = NewNode.Parent.Left then
        begin
          NewNode := NewNode.Parent;
          RotateRight(NewNode);
        end;
        NewNode.Parent.Color := tcBlack;
        if NewNode.Parent.Parent <> nil then
	        NewNode.Parent.Parent.Color := tcRed;
        RotateLeft(NewNode.Parent.Parent);
      end;
    end;
  end; // while
  FRoot.Color := tcBlack;
  Inc(FCount);
	Result := True;
end;

function TIntfBinaryTree.AddAll(ACollection: IIntfCollection): Boolean;
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

procedure TIntfBinaryTree.Clear;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
{$IFDEF RECURSIVE}
	procedure DisposeChild(Node: PIntfBinaryNode);
  begin
  	if Node.Left <> nil then
    	DisposeChild(Node.Left);
    if Node.Right <> nil then
    	DisposeChild(Node.Right);
    Node.Obj := nil; // Force Release
    Dispose(Node);
  end;
{$ELSE}
var
	Current: PIntfBinaryNode;
  Save: PIntfBinaryNode;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
{$IFDEF RECURSIVE}
	// recursive version
  if FRoot <> nil then
  begin
  	DisposeChild(FRoot);
    FRoot := nil;
	end;
{$ELSE}
  // iterative version
	Current := FRoot;
  while Current <> nil do
  begin
  	if Current.Left <> nil then
    begin
    	Current := Current.Left;
    end
    else if Current.Right <> nil then
    begin
    	Current := Current.Right;
    end
    else
    begin
    	Current.Obj := nil; // Force Release
			if Current.Parent = nil then // Root
      begin
      	Dispose(Current);
        Current := nil;
			  FRoot := nil;
      end
      else
      begin
      	Save := Current;
      	Current := Current.Parent;
      	if Save = Current.Right then // True = from Right
        begin
        	Dispose(Save);
	      	Current.Right := nil;
        end
        else
        begin
        	Dispose(Save);
        	Current.Left := nil;
				end
      end;
    end;
  end; // while
{$ENDIF}
  FCount := 0;
end;

function TIntfBinaryTree.Clone: IInterface;

	function CloneNode(Node, Parent: PIntfBinaryNode): PIntfBinaryNode;
  begin
  	if Node = nil then
    begin
    	Result := nil;
    	Exit;
    end;
  	New(Result);
    Result.Obj := Node.Obj;
    Result.Color := Node.Color;
    Result.Parent := Parent;
    Result.Left := CloneNode(Node.Left, Result); // recursive call
    Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;

var
	NewTree: TIntfBinaryTree;
begin
	NewTree := TIntfBinaryTree.Create(FComparator);
  NewTree.FCount := FCount;
  NewTree.FRoot := CloneNode(FRoot, nil);
  Result := NewTree;
end;

function TIntfBinaryTree.Contains(AObject: IInterface): Boolean;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
{$IFDEF RECURSIVE}
	function ContainsChild(Node: PIntfBinaryNode): Boolean;
  begin
		Result := False;
  	if Node = nil then
    	Exit;
		if FComparator(Node.Obj, AObject) = 0 then
    begin
    	Result := True;
      Exit;
    end
    else if FComparator(Node.Obj, AObject) > 0 then
    	Result := ContainsChild(Node.Left)
    else if FComparator(Node.Obj, AObject) < 0 then
    	Result := ContainsChild(Node.Right)
  end;
{$ELSE}
var
	Current: PIntfBinaryNode;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Result := False;
  if AObject = nil then
		Exit;
{$IFDEF RECURSIVE}
  // recursive version
	Result := ContainsChild(FRoot);
{$ELSE}
  // iterative version
	Current := FRoot;
  while Current <> nil do
  begin
		if FComparator(Current.Obj, AObject) = 0 then
    begin
    	Result := True;
      Exit;
    end
    else if FComparator(Current.Obj, AObject) > 0 then
    	Current := Current.Left
    else if FComparator(Current.Obj, AObject) < 0 then
    	Current := Current.Right;
  end;
{$ENDIF}
end;

function TIntfBinaryTree.ContainsAll(ACollection: IIntfCollection): Boolean;
var
  It: IIntfIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
  begin
    if not Contains(It.Next) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

constructor TIntfBinaryTree.Create;
begin
	Create(IntfSimpleCompare);
end;

constructor TIntfBinaryTree.Create(Comparator: TIntfCompare);
begin
	FComparator := Comparator;
  FTraverseOrder := toPreOrder;
end;

destructor TIntfBinaryTree.Destroy;
begin
	Clear;
  inherited;
end;

function TIntfBinaryTree.Equals(ACollection: IIntfCollection): Boolean;
var
  It, ItSelf: IIntfIterator;
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
  if FCount <> ACollection.Size then
    Exit;
  It := ACollection.First;
  ItSelf := First;
  while ItSelf.HasNext do
		if FComparator(ItSelf.Next, It.Next) <> 0 then
      Exit;
  Result := True;
end;

function TIntfBinaryTree.First: IIntfIterator;
begin
	case GetTraverseOrder of
  	toPreOrder:	Result := TPreOrderIntfItr.Create(Self, FRoot);
  	toOrder: Result := TOrderIntfItr.Create(Self, FRoot);
  	toPostOrder: Result := TPostOrderIntfItr.Create(Self, FRoot);
  end;
end;

function TIntfBinaryTree.GetTraverseOrder: TTraverseOrder;
begin
	Result := FTraverseOrder;
end;

function TIntfBinaryTree.IsEmpty: Boolean;
begin
	Result := FCount = 0;
end;

function TIntfBinaryTree.Last: IIntfIterator;
var
	Start: PIntfBinaryNode;
begin
	Start := FRoot;
  case FTraverseOrder of
    toPreOrder:
      begin
        if Start <> nil then
          while Start.Right <> nil do
            Start := Start.Right;
        Result := TPreOrderIntfItr.Create(Self, Start); 
      end;
    toOrder:
      begin
        if Start <> nil then
          while Start.Right <> nil do
            Start := Start.Right;
        Result := TOrderIntfItr.Create(Self, Start); 
      end;
    toPostOrder:
      begin
        Result := TPostOrderIntfItr.Create(Self, Start);
      end;
  end;
end;

procedure TIntfBinaryTree.RotateLeft(Node: PIntfBinaryNode);
var
	TempNode: PIntfBinaryNode;
begin
	if Node = nil then
  	Exit;
	TempNode := Node.Right;
//  if TempNode = nil then	Exit;
  Node.Right := TempNode.Left;
  if TempNode.Left <> nil then
  	TempNode.Left.Parent := Node;
  TempNode.Parent := Node.Parent;
  if Node.Parent = nil then
  	FRoot := TempNode
  else if Node.Parent.Left = Node then
  	Node.Parent.Left := TempNode
  else
		Node.Parent.Right := TempNode;
  TempNode.Left := Node;
  Node.Parent := TempNode;
end;

procedure TIntfBinaryTree.RotateRight(Node: PIntfBinaryNode);
var
	TempNode: PIntfBinaryNode;
begin
	if Node = nil then
  	Exit;
	TempNode := Node.Left;
//  if TempNode = nil then 	Exit;
  Node.Left := TempNode.Right;
  if TempNode.Right <> nil then
  	TempNode.Right.Parent := Node;
  TempNode.Parent := Node.Parent;
  if Node.Parent = nil then
  	FRoot := TempNode
  else if Node.Parent.Right = Node then
  	Node.Parent.Right := TempNode
  else
		Node.Parent.Left := TempNode;
  TempNode.Right := Node;
  Node.Parent := TempNode;
end;

function TIntfBinaryTree.Remove(AObject: IInterface): Boolean;

	procedure Correction(Node: PIntfBinaryNode);
  var
  	TempNode: PIntfBinaryNode;
  begin
  	while (Node <> FRoot) and (Node.Color = tcBlack) do
    begin
    	if Node = Node.Parent.Left then
      begin
        TempNode := Node.Parent.Right;
        if TempNode = nil then
        begin
        	Node := Node.Parent;
          Continue; // while
        end;
        if (TempNode.Color = tcRed) then
        begin
        	TempNode.Color := tcBlack;
          Node.Parent.Color := tcRed;
          RotateLeft(Node.Parent);
          TempNode := Node.Parent.Right;
        end;
        if (TempNode.Left <> nil) and (TempNode.Left.Color = tcBlack) and  
        	 (TempNode.Right <> nil) and (TempNode.Right.Color = tcBlack) then
        begin
        	TempNode.Color := tcRed;
          Node := Node.Parent;
        end
        else
        begin
        	if (TempNode.Right <> nil) and  (TempNode.Right.Color = tcBlack) then
	        begin
          	TempNode.Left.Color := tcBlack;
            TempNode.Color := tcRed;
            RotateRight(TempNode);
            TempNode := Node.Parent.Right;
          end;
          TempNode.Color := Node.Parent.Color;
          Node.Parent.Color := tcBlack;
          if TempNode.Right <> nil then
	          TempNode.Right.Color := tcBlack;
          RotateLeft(Node.Parent);
          Node := FRoot;
        end;
      end else // if
      begin
        TempNode := Node.Parent.Left;
        if TempNode = nil then
        begin
        	Node := Node.Parent;
          Continue;
        end;
        if TempNode.Color = tcRed then
        begin
        	TempNode.Color := tcBlack;
          Node.Parent.Color := tcRed;
          RotateRight(Node.Parent);
          TempNode := Node.Parent.Left;
        end;
        if (TempNode.Left.Color = tcBlack) and (TempNode.Right.Color = tcBlack) then
        begin
        	TempNode.Color := tcRed;
          Node := Node.Parent;
        end
        else
        begin
        	if TempNode.Left.Color = tcBlack then
	        begin
          	TempNode.Right.Color := tcBlack;
            TempNode.Color := tcRed;
            RotateLeft(TempNode);
            TempNode := Node.Parent.Left;
          end;
          TempNode.Color := Node.Parent.Color;
          Node.Parent.Color := tcBlack;
          if TempNode.Left <> nil then
	          TempNode.Left.Color := tcBlack;
          RotateRight(Node.Parent);
          Node := FRoot;
        end;
      end // if
    end; // while
    Node.Color := tcBlack;
  end; // correction
var
	Current: PIntfBinaryNode;
  Node: PIntfBinaryNode;
	Save: PIntfBinaryNode;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Result := False;
  if AObject = nil then
  	Exit;
	// locate AObject in the tree
	Current := FRoot;
  while Current <> nil do
  begin
  	if FComparator(AObject, Current.Obj) = 0 then
    	Break;
  	if FComparator(AObject, Current.Obj) < 0 then
    	Current := Current.Left
    else
    	Current := Current.Right;
  end;
  if Current = nil then
  	Exit;
  // Remove
  if (Current.Left = nil) or (Current.Right = nil) then
  	Save := Current
  else
  begin // Successor in Save
  	if Current.Right <> nil then
    begin
    	Save := Current.Right;
      while Save.Left <> nil do // Minimum
      	Save := Save.Left;
    end else
    begin
    	Save := Current.Parent;
      while (Save <> nil) and (Current = Save.Right) do
      begin
      	Current := Save;
        Save := Save.Parent;
      end;
    end;
  end; // end Successor
  if Save.Left <> nil then
		Node := Save.Left
  else
  	Node := Save.Right;
  if Node <> nil then
  begin
	  Node.Parent := Save.Parent;
    if Save.Parent = nil then
      FRoot := Node
    else if Save = Save.Parent.Left then
      Save.Parent.Left := Node
    else
      Save.Parent.Right := Node;
		if Save.Color = tcBlack then // Correction
  		Correction(Node);
  end
  else if Save.Parent = nil then
  	FRoot := nil
  else
  begin
		if Save.Color = tcBlack then // Correction
  		Correction(Save);
    if Save.Parent <> nil then
    	if Save = Save.Parent.Left then
	    	Save.Parent.Left := nil
      else if Save = Save.Parent.Right then
      	Save.Parent.Right := nil
  end;
  Dispose(Save);
  Dec(FCount);
end;

function TIntfBinaryTree.RemoveAll(ACollection: IIntfCollection): Boolean;
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
    Remove(It.Next);
end;

function TIntfBinaryTree.RetainAll(ACollection: IIntfCollection): Boolean;
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
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
end;

procedure TIntfBinaryTree.SetTraverseOrder(Value: TTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TIntfBinaryTree.Size: Integer;
begin
	Result := FCount;
end;

{ TStrBinaryTree }

function TStrBinaryTree.Add(const AString: string): Boolean;
var
	NewNode: PStrBinaryNode;
  Current, Save: PStrBinaryNode;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Result := False;
	if AString = '' then
  	Exit;
  New(NewNode);
  FillChar(NewNode^, SizeOf(NewNode^), 0); // Init to nil
  NewNode.Str := AString;
  // Insert into right place
  Save := nil;
	Current := FRoot;
  while Current <> nil do
  begin
  	Save := Current;
  	if FComparator(NewNode.Str, Current.Str) < 0 then
    	Current := Current.Left
    else
    	Current := Current.Right;
  end;
  NewNode.Parent := Save;
  if Save = nil then
  	FRoot := NewNode
  else if FComparator(NewNode.Str, Save.Str) < 0 then
  	Save.Left := NewNode
  else
  	Save.Right := NewNode;
  // RB balanced
  NewNode.Color := tcRed;
  while (NewNode <> FRoot) and (NewNode.Parent.Color = tcRed) do
  begin
  	if (NewNode.Parent.Parent <> nil) and (NewNode.Parent = NewNode.Parent.Parent.Left) then
    begin
    	Current := NewNode.Parent.Parent.Right;
      if (Current <> nil) and (Current.Color = tcRed) then
      begin
      	NewNode.Parent.Color := tcBlack;
        Current.Color := tcBlack;
        NewNode.Parent.Parent.Color := tcRed;
        NewNode := NewNode.Parent.Parent;
      end
      else
      begin
      	if NewNode = NewNode.Parent.Right then
        begin
          NewNode := NewNode.Parent;
          RotateLeft(NewNode);
        end;
        NewNode.Parent.Color := tcBlack;
        NewNode.Parent.Parent.Color := tcRed;
        RotateRight(NewNode.Parent.Parent);
      end;
    end
    else
    begin
    	if NewNode.Parent.Parent = nil then
      	Current := nil
      else
	    	Current := NewNode.Parent.Parent.Left;
      if (Current <> nil) and (Current.Color = tcRed) then
      begin
      	NewNode.Parent.Color := tcBlack;
        Current.Color := tcBlack;
        NewNode.Parent.Parent.Color := tcRed;
        NewNode := NewNode.Parent.Parent;
      end
      else
      begin
      	if NewNode = NewNode.Parent.Left then
        begin
          NewNode := NewNode.Parent;
          RotateRight(NewNode);
        end;
        NewNode.Parent.Color := tcBlack;
        if NewNode.Parent.Parent <> nil then
	        NewNode.Parent.Parent.Color := tcRed;
        RotateLeft(NewNode.Parent.Parent);
      end;
    end;
  end; // while
  FRoot.Color := tcBlack;
  Inc(FCount);
	Result := True;
end;

function TStrBinaryTree.AddAll(ACollection: IStrCollection): Boolean;
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

procedure TStrBinaryTree.Clear;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
{$IFDEF RECURSIVE}
	procedure DisposeChild(Node: PIntfBinaryNode);
  begin
  	if Node.Left <> nil then
    	DisposeChild(Node.Left);
    if Node.Right <> nil then
    	DisposeChild(Node.Right);
    Node.Obj := nil; // Force Release
    Dispose(Node);
  end;
{$ELSE}
var
	Current: PStrBinaryNode;
  Save: PStrBinaryNode;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
{$IFDEF RECURSIVE}
	// recursive version
  if FRoot <> nil then
  begin
  	DisposeChild(FRoot);
    FRoot := nil;
	end;
{$ELSE}
  // iterative version
	Current := FRoot;
  while Current <> nil do
  begin
  	if Current.Left <> nil then
    begin
    	Current := Current.Left;
    end
    else if Current.Right <> nil then
    begin
    	Current := Current.Right;
    end
    else
    begin
    	Current.Str := ''; // Force Release
			if Current.Parent = nil then // Root
      begin
      	Dispose(Current);
        Current := nil;
			  FRoot := nil;
      end
      else
      begin
      	Save := Current;
      	Current := Current.Parent;
      	if Save = Current.Right then // True = from Right
        begin
        	Dispose(Save);
	      	Current.Right := nil;
        end
        else
        begin
        	Dispose(Save);
        	Current.Left := nil;
				end
      end;
    end;
  end; // while
{$ENDIF}
  FCount := 0;
end;

function TStrBinaryTree.Clone: TObject;

	function CloneNode(Node, Parent: PStrBinaryNode): PStrBinaryNode;
  begin
  	if Node = nil then
    begin
    	Result := nil;
    	Exit;
    end;
  	New(Result);
    Result.Str := Node.Str;
    Result.Color := Node.Color;
    Result.Parent := Parent;
    Result.Left := CloneNode(Node.Left, Result); // recursive call
    Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;

var
	NewTree: TStrBinaryTree;
begin
	NewTree := TStrBinaryTree.Create(FComparator);   
  NewTree.FCount := FCount;
  NewTree.FRoot := CloneNode(FRoot, nil);
  Result := NewTree;
end;

function TStrBinaryTree.Contains(const AString: string): Boolean;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
{$IFDEF RECURSIVE}
	function ContainsChild(Node: PIntfBinaryNode): Boolean;
  begin
		Result := False;
  	if Node = nil then
    	Exit;
		if FComparator(Node.Obj, AObject) = 0 then
    begin
    	Result := True;
      Exit;
    end
    else if FComparator(Node.Obj, AObject) > 0 then
    	Result := ContainsChild(Node.Left)
    else if FComparator(Node.Obj, AObject) < 0 then
    	Result := ContainsChild(Node.Right)
  end;
{$ELSE}
var
	Current: PStrBinaryNode;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Result := False;
  if AString = '' then
		Exit;
{$IFDEF RECURSIVE}
  // recursive version
	Result := ContainsChild(FRoot);
{$ELSE}
  // iterative version
	Current := FRoot;
  while Current <> nil do
  begin
		if FComparator(Current.Str, AString) = 0 then
    begin
    	Result := True;
      Exit;
    end
    else if FComparator(Current.Str, AString) > 0 then
    	Current := Current.Left
    else if FComparator(Current.Str, AString) < 0 then
    	Current := Current.Right;
  end;
{$ENDIF}
end;

function TStrBinaryTree.ContainsAll(ACollection: IStrCollection): Boolean;
var
  It: IStrIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
  begin
    if not Contains(It.Next) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

constructor TStrBinaryTree.Create;
begin
	Create(StrSimpleCompare);
end;

constructor TStrBinaryTree.Create(Comparator: TStrCompare);
begin
	FComparator := Comparator;
  FTraverseOrder := toPreOrder;
end;

destructor TStrBinaryTree.Destroy;
begin
	Clear;
  inherited;
end;

function TStrBinaryTree.Equals(ACollection: IStrCollection): Boolean;
var
  It, ItSelf: IStrIterator;
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
  if FCount <> ACollection.Size then
    Exit;
  It := ACollection.First;
  ItSelf := First;
  while ItSelf.HasNext do
		if FComparator(ItSelf.Next, It.Next) <> 0 then
      Exit;
  Result := True;
end;

function TStrBinaryTree.First: IStrIterator;
begin
	case GetTraverseOrder of
  	toPreOrder:	Result := TPreOrderStrItr.Create(Self, FRoot);
  	toOrder: Result := TOrderStrItr.Create(Self, FRoot);
  	toPostOrder: Result := TPostOrderStrItr.Create(Self, FRoot);
  end;
end;

function TStrBinaryTree.GetTraverseOrder: TTraverseOrder;
begin
	Result := FTraverseOrder;
end;

function TStrBinaryTree.IsEmpty: Boolean;
begin
	Result := FCount = 0;
end;

function TStrBinaryTree.Last: IStrIterator;
var
	Start: PStrBinaryNode;
begin
	Start := FRoot;
  case FTraverseOrder of
    toPreOrder:
      begin
        if Start <> nil then
          while Start.Right <> nil do
            Start := Start.Right;
        Result := TPreOrderStrItr.Create(Self, Start);
      end;
    toOrder:
      begin
        if Start <> nil then
          while Start.Right <> nil do
            Start := Start.Right;
        Result := TOrderStrItr.Create(Self, Start);
      end;
    toPostOrder:
      begin
        Result := TPostOrderStrItr.Create(Self, Start);
      end;
  end;
end;

function TStrBinaryTree.Remove(const AString: string): Boolean;

	procedure Correction(Node: PStrBinaryNode);
  var
  	TempNode: PStrBinaryNode;
  begin
  	while (Node <> FRoot) and (Node.Color = tcBlack) do
    begin
    	if Node = Node.Parent.Left then
      begin
        TempNode := Node.Parent.Right;
        if TempNode = nil then
        begin
        	Node := Node.Parent;
          Continue; // while
        end;
        if (TempNode.Color = tcRed) then
        begin
        	TempNode.Color := tcBlack;
          Node.Parent.Color := tcRed;
          RotateLeft(Node.Parent);
          TempNode := Node.Parent.Right;
        end;
        if (TempNode.Left <> nil) and (TempNode.Left.Color = tcBlack) and  
        	 (TempNode.Right <> nil) and (TempNode.Right.Color = tcBlack) then
        begin
        	TempNode.Color := tcRed;
          Node := Node.Parent;
        end
        else
        begin
        	if (TempNode.Right <> nil) and  (TempNode.Right.Color = tcBlack) then
	        begin
          	TempNode.Left.Color := tcBlack;
            TempNode.Color := tcRed;
            RotateRight(TempNode);
            TempNode := Node.Parent.Right;
          end;
          TempNode.Color := Node.Parent.Color;
          Node.Parent.Color := tcBlack;
          if TempNode.Right <> nil then
	          TempNode.Right.Color := tcBlack;
          RotateLeft(Node.Parent);
          Node := FRoot;
        end;
      end else // if
      begin
        TempNode := Node.Parent.Left;
        if TempNode = nil then
        begin
        	Node := Node.Parent;
          Continue;
        end;
        if TempNode.Color = tcRed then
        begin
        	TempNode.Color := tcBlack;
          Node.Parent.Color := tcRed;
          RotateRight(Node.Parent);
          TempNode := Node.Parent.Left;
        end;
        if (TempNode.Left.Color = tcBlack) and (TempNode.Right.Color = tcBlack) then
        begin
        	TempNode.Color := tcRed;
          Node := Node.Parent;
        end
        else
        begin
        	if TempNode.Left.Color = tcBlack then
	        begin
          	TempNode.Right.Color := tcBlack;
            TempNode.Color := tcRed;
            RotateLeft(TempNode);
            TempNode := Node.Parent.Left;
          end;
          TempNode.Color := Node.Parent.Color;
          Node.Parent.Color := tcBlack;
          if TempNode.Left <> nil then
	          TempNode.Left.Color := tcBlack;
          RotateRight(Node.Parent);
          Node := FRoot;
        end;
      end // if
    end; // while
    Node.Color := tcBlack;
  end; // correction
var
	Current: PStrBinaryNode;
  Node: PStrBinaryNode;
	Save: PStrBinaryNode;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Result := False;
  if AString = '' then
  	Exit;
	// locate AObject in the tree
	Current := FRoot;
  while Current <> nil do
  begin
  	if FComparator(AString, Current.Str) = 0 then
    	Break;
  	if FComparator(AString, Current.Str) < 0 then
    	Current := Current.Left
    else
    	Current := Current.Right;
  end;
  if Current = nil then
  	Exit;
  // Remove
  if (Current.Left = nil) or (Current.Right = nil) then
  	Save := Current
  else
  begin // Successor in Save
  	if Current.Right <> nil then
    begin
    	Save := Current.Right;
      while Save.Left <> nil do // Minimum
      	Save := Save.Left;
    end else
    begin
    	Save := Current.Parent;
      while (Save <> nil) and (Current = Save.Right) do
      begin
      	Current := Save;
        Save := Save.Parent;
      end;
    end;
  end; // end Successor
  if Save.Left <> nil then
		Node := Save.Left
  else
  	Node := Save.Right;
  if Node <> nil then
  begin
	  Node.Parent := Save.Parent;
    if Save.Parent = nil then
      FRoot := Node
    else if Save = Save.Parent.Left then
      Save.Parent.Left := Node
    else
      Save.Parent.Right := Node;
		if Save.Color = tcBlack then // Correction
  		Correction(Node);
  end
  else if Save.Parent = nil then
  	FRoot := nil
  else
  begin
		if Save.Color = tcBlack then // Correction
  		Correction(Save);
    if Save.Parent <> nil then
    	if Save = Save.Parent.Left then
	    	Save.Parent.Left := nil
      else if Save = Save.Parent.Right then
      	Save.Parent.Right := nil
  end;
  Dispose(Save);
  Dec(FCount);
end;

function TStrBinaryTree.RemoveAll(ACollection: IStrCollection): Boolean;
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
    Remove(It.Next);
end;

function TStrBinaryTree.RetainAll(ACollection: IStrCollection): Boolean;
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
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
end;

procedure TStrBinaryTree.RotateLeft(Node: PStrBinaryNode);
var
	TempNode: PStrBinaryNode;
begin
	if Node = nil then
  	Exit;
	TempNode := Node.Right;
//  if TempNode = nil then	Exit;
  Node.Right := TempNode.Left;
  if TempNode.Left <> nil then
  	TempNode.Left.Parent := Node;
  TempNode.Parent := Node.Parent;
  if Node.Parent = nil then
  	FRoot := TempNode
  else if Node.Parent.Left = Node then
  	Node.Parent.Left := TempNode
  else
		Node.Parent.Right := TempNode;
  TempNode.Left := Node;
  Node.Parent := TempNode;
end;

procedure TStrBinaryTree.RotateRight(Node: PStrBinaryNode);
var
	TempNode: PStrBinaryNode;
begin
	if Node = nil then
  	Exit;
	TempNode := Node.Left;
//  if TempNode = nil then 	Exit;
  Node.Left := TempNode.Right;
  if TempNode.Right <> nil then
  	TempNode.Right.Parent := Node;
  TempNode.Parent := Node.Parent;
  if Node.Parent = nil then
  	FRoot := TempNode
  else if Node.Parent.Right = Node then
  	Node.Parent.Right := TempNode
  else
		Node.Parent.Left := TempNode;
  TempNode.Right := Node;
  Node.Parent := TempNode;
end;

procedure TStrBinaryTree.SetTraverseOrder(Value: TTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TStrBinaryTree.Size: Integer;
begin
	Result := FCount;
end;

{ TBinaryTree }

function TBinaryTree.Add(AObject: TObject): Boolean;
var
	NewNode: PBinaryNode;
  Current, Save: PBinaryNode;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Result := False;
	if AObject = nil then
  	Exit;
  New(NewNode);
  FillChar(NewNode^, SizeOf(NewNode^), 0); // Init to nil
  NewNode.Obj := AObject;
  // Insert into right place
  Save := nil;
	Current := FRoot;
  while Current <> nil do
  begin
  	Save := Current;
  	if FComparator(NewNode.Obj, Current.Obj) < 0 then
    	Current := Current.Left
    else
    	Current := Current.Right;
  end;
  NewNode.Parent := Save;
  if Save = nil then
  	FRoot := NewNode
  else if FComparator(NewNode.Obj, Save.Obj) < 0 then
  	Save.Left := NewNode
  else
  	Save.Right := NewNode;
  // RB balanced
  NewNode.Color := tcRed;
  while (NewNode <> FRoot) and (NewNode.Parent.Color = tcRed) do
  begin
  	if (NewNode.Parent.Parent <> nil) and (NewNode.Parent = NewNode.Parent.Parent.Left) then
    begin
    	Current := NewNode.Parent.Parent.Right;
      if Current.Color = tcRed then
      begin
      	NewNode.Parent.Color := tcBlack;
        Current.Color := tcBlack;
        NewNode.Parent.Parent.Color := tcRed;
        NewNode := NewNode.Parent.Parent;
      end
      else
      begin
      	if NewNode = NewNode.Parent.Right then
        begin
          NewNode := NewNode.Parent;
          RotateLeft(NewNode);
        end;
        NewNode.Parent.Color := tcBlack;
        NewNode.Parent.Parent.Color := tcRed;
        RotateRight(NewNode.Parent.Parent);
      end;
    end
    else
    begin
    	if NewNode.Parent.Parent = nil then
      	Current := nil
      else
	    	Current := NewNode.Parent.Parent.Left;
      if (Current <> nil) and (Current.Color = tcRed) then
      begin
      	NewNode.Parent.Color := tcBlack;
        Current.Color := tcBlack;
        NewNode.Parent.Parent.Color := tcRed;
        NewNode := NewNode.Parent.Parent;
      end
      else
      begin
      	if NewNode = NewNode.Parent.Left then
        begin
          NewNode := NewNode.Parent;
          RotateRight(NewNode);
        end;
        NewNode.Parent.Color := tcBlack;
        if NewNode.Parent.Parent <> nil then
	        NewNode.Parent.Parent.Color := tcRed;
        RotateLeft(NewNode.Parent.Parent);
      end;
    end;
  end; // while
  FRoot.Color := tcBlack;
  Inc(FCount);
	Result := True;
end;

function TBinaryTree.AddAll(ACollection: ICollection): Boolean;
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

procedure TBinaryTree.Clear;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
{$IFDEF RECURSIVE}
	procedure DisposeChild(Node: PIntfBinaryNode);
  begin
  	if Node.Left <> nil then
    	DisposeChild(Node.Left);
    if Node.Right <> nil then
    	DisposeChild(Node.Right);
    Node.Obj := nil; // Force Release
    Dispose(Node);
  end;
{$ELSE}
var
	Current: PBinaryNode;
  Save: PBinaryNode;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
{$IFDEF RECURSIVE}
	// recursive version
  if FRoot <> nil then
  begin
  	DisposeChild(FRoot);
    FRoot := nil;
	end;
{$ELSE}
  // iterative version
	Current := FRoot;
  while Current <> nil do
  begin
  	if Current.Left <> nil then
    begin
    	Current := Current.Left;
    end
    else if Current.Right <> nil then
    begin
    	Current := Current.Right;
    end
    else
    begin
    	Current.Obj := nil; // Force Release
			if Current.Parent = nil then // Root
      begin
      	Dispose(Current);
        Current := nil;
			  FRoot := nil;
      end
      else
      begin
      	Save := Current;
      	Current := Current.Parent;
      	if Save = Current.Right then // True = from Right
        begin
        	Dispose(Save);
	      	Current.Right := nil;
        end
        else
        begin
        	Dispose(Save);
        	Current.Left := nil;
				end
      end;
    end;
  end; // while
{$ENDIF}
  FCount := 0;
end;

function TBinaryTree.Clone: TObject;

	function CloneNode(Node, Parent: PBinaryNode): PBinaryNode;
  begin
  	if Node = nil then
    begin
    	Result := nil;
    	Exit;
    end;
  	New(Result);
    Result.Obj := Node.Obj;
    Result.Color := Node.Color;
    Result.Parent := Parent;
    Result.Left := CloneNode(Node.Left, Result); // recursive call
    Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;

var
	NewTree: TBinaryTree;
begin
	NewTree := TBinaryTree.Create(FComparator);
  NewTree.FCount := FCount;
  NewTree.FRoot := CloneNode(FRoot, nil);
  Result := NewTree;
end;

function TBinaryTree.Contains(AObject: TObject): Boolean;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
{$IFDEF RECURSIVE}
	function ContainsChild(Node: PIntfBinaryNode): Boolean;
  begin
		Result := False;
  	if Node = nil then
    	Exit;
		if FComparator(Node.Obj, AObject) = 0 then
    begin
    	Result := True;
      Exit;
    end
    else if FComparator(Node.Obj, AObject) > 0 then
    	Result := ContainsChild(Node.Left)
    else if FComparator(Node.Obj, AObject) < 0 then
    	Result := ContainsChild(Node.Right)
  end;
{$ELSE}
var
	Current: PBinaryNode;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Result := False;
  if AObject = nil then
		Exit;
{$IFDEF RECURSIVE}
  // recursive version
	Result := ContainsChild(FRoot);
{$ELSE}
  // iterative version
	Current := FRoot;
  while Current <> nil do
  begin
		if FComparator(Current.Obj, AObject) = 0 then
    begin
    	Result := True;
      Exit;
    end
    else if FComparator(Current.Obj, AObject) > 0 then
    	Current := Current.Left
    else if FComparator(Current.Obj, AObject) < 0 then
    	Current := Current.Right;
  end;
{$ENDIF}
end;

function TBinaryTree.ContainsAll(ACollection: ICollection): Boolean;
var
  It: IIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
  begin
    if not Contains(It.Next) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

constructor TBinaryTree.Create;
begin
	Create(SimpleCompare);
end;

constructor TBinaryTree.Create(Comparator: TCompare);
begin
	FComparator := Comparator;
  FTraverseOrder := toPreOrder;
end;

destructor TBinaryTree.Destroy;
begin
	Clear;
  inherited;
end;

function TBinaryTree.Equals(ACollection: ICollection): Boolean;
var
  It, ItSelf: IIterator;
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
  if FCount <> ACollection.Size then
    Exit;
  It := ACollection.First;
  ItSelf := First;
  while ItSelf.HasNext do
		if FComparator(ItSelf.Next, It.Next) <> 0 then
      Exit;
  Result := True;
end;

function TBinaryTree.First: IIterator;
begin
	case GetTraverseOrder of
  	toPreOrder:	Result := TPreOrderItr.Create(Self, FRoot);
  	toOrder: Result := TOrderItr.Create(Self, FRoot);
  	toPostOrder: Result := TPostOrderItr.Create(Self, FRoot);
  end;
end;

function TBinaryTree.GetTraverseOrder: TTraverseOrder;
begin
	Result := FTraverseOrder;
end;

function TBinaryTree.IsEmpty: Boolean;
begin
	Result := FCount = 0;
end;

function TBinaryTree.Last: IIterator;
var
	Start: PBinaryNode;
begin
	Start := FRoot;
  case FTraverseOrder of
    toPreOrder:
      begin
        if Start <> nil then
          while Start.Right <> nil do
            Start := Start.Right;
        Result := TPreOrderItr.Create(Self, Start);
      end;
    toOrder:
      begin
        if Start <> nil then
          while Start.Right <> nil do
            Start := Start.Right;
        Result := TOrderItr.Create(Self, Start);
      end;
    toPostOrder:
      begin
        Result := TPostOrderItr.Create(Self, Start);
      end;
  end;
end;

function TBinaryTree.Remove(AObject: TObject): Boolean;

	procedure Correction(Node: PBinaryNode);
  var
  	TempNode: PBinaryNode;
  begin
  	while (Node <> FRoot) and (Node.Color = tcBlack) do
    begin
    	if Node = Node.Parent.Left then
      begin
        TempNode := Node.Parent.Right;
        if TempNode = nil then
        begin
        	Node := Node.Parent;
          Continue; // while
        end;
        if (TempNode.Color = tcRed) then
        begin
        	TempNode.Color := tcBlack;
          Node.Parent.Color := tcRed;
          RotateLeft(Node.Parent);
          TempNode := Node.Parent.Right;
        end;
        if (TempNode.Left <> nil) and (TempNode.Left.Color = tcBlack) and  
        	 (TempNode.Right <> nil) and (TempNode.Right.Color = tcBlack) then
        begin
        	TempNode.Color := tcRed;
          Node := Node.Parent;
        end
        else
        begin
        	if (TempNode.Right <> nil) and  (TempNode.Right.Color = tcBlack) then
	        begin
          	TempNode.Left.Color := tcBlack;
            TempNode.Color := tcRed;
            RotateRight(TempNode);
            TempNode := Node.Parent.Right;
          end;
          TempNode.Color := Node.Parent.Color;
          Node.Parent.Color := tcBlack;
          if TempNode.Right <> nil then
	          TempNode.Right.Color := tcBlack;
          RotateLeft(Node.Parent);
          Node := FRoot;
        end;
      end else // if
      begin
        TempNode := Node.Parent.Left;
        if TempNode = nil then
        begin
        	Node := Node.Parent;
          Continue;
        end;
        if TempNode.Color = tcRed then
        begin
        	TempNode.Color := tcBlack;
          Node.Parent.Color := tcRed;
          RotateRight(Node.Parent);
          TempNode := Node.Parent.Left;
        end;
        if (TempNode.Left.Color = tcBlack) and (TempNode.Right.Color = tcBlack) then
        begin
        	TempNode.Color := tcRed;
          Node := Node.Parent;
        end
        else
        begin
        	if TempNode.Left.Color = tcBlack then
	        begin
          	TempNode.Right.Color := tcBlack;
            TempNode.Color := tcRed;
            RotateLeft(TempNode);
            TempNode := Node.Parent.Left;
          end;
          TempNode.Color := Node.Parent.Color;
          Node.Parent.Color := tcBlack;
          if TempNode.Left <> nil then
	          TempNode.Left.Color := tcBlack;
          RotateRight(Node.Parent);
          Node := FRoot;
        end;
      end // if
    end; // while
    Node.Color := tcBlack;
  end; // correction
var
	Current: PBinaryNode;
  Node: PBinaryNode;
	Save: PBinaryNode;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Result := False;
  if AObject = nil then
  	Exit;
	// locate AObject in the tree
	Current := FRoot;
  while Current <> nil do
  begin
  	if FComparator(AObject, Current.Obj) = 0 then
    	Break;
  	if FComparator(AObject, Current.Obj) < 0 then
    	Current := Current.Left
    else
    	Current := Current.Right;
  end;
  if Current = nil then
  	Exit;
  // Remove
  if (Current.Left = nil) or (Current.Right = nil) then
  	Save := Current
  else
  begin // Successor in Save
  	if Current.Right <> nil then
    begin
    	Save := Current.Right;
      while Save.Left <> nil do // Minimum
      	Save := Save.Left;
    end else
    begin
    	Save := Current.Parent;
      while (Save <> nil) and (Current = Save.Right) do
      begin
      	Current := Save;
        Save := Save.Parent;
      end;
    end;
  end; // end Successor
  if Save.Left <> nil then
		Node := Save.Left
  else
  	Node := Save.Right;
  if Node <> nil then
  begin
	  Node.Parent := Save.Parent;
    if Save.Parent = nil then
      FRoot := Node
    else if Save = Save.Parent.Left then
      Save.Parent.Left := Node
    else
      Save.Parent.Right := Node;
		if Save.Color = tcBlack then // Correction
  		Correction(Node);
  end
  else if Save.Parent = nil then
  	FRoot := nil
  else
  begin
		if Save.Color = tcBlack then // Correction
  		Correction(Save);
    if Save.Parent <> nil then
    	if Save = Save.Parent.Left then
	    	Save.Parent.Left := nil
      else if Save = Save.Parent.Right then
      	Save.Parent.Right := nil
  end;
  Dispose(Save);
  Dec(FCount);
end;

function TBinaryTree.RemoveAll(ACollection: ICollection): Boolean;
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
    Remove(It.Next);
end;

function TBinaryTree.RetainAll(ACollection: ICollection): Boolean;
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
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
end;

procedure TBinaryTree.RotateLeft(Node: PBinaryNode);
var
	TempNode: PBinaryNode;
begin
	if Node = nil then
  	Exit;
	TempNode := Node.Right;
//  if TempNode = nil then	Exit;
  Node.Right := TempNode.Left;
  if TempNode.Left <> nil then
  	TempNode.Left.Parent := Node;
  TempNode.Parent := Node.Parent;
  if Node.Parent = nil then
  	FRoot := TempNode
  else if Node.Parent.Left = Node then
  	Node.Parent.Left := TempNode
  else
		Node.Parent.Right := TempNode;
  TempNode.Left := Node;
  Node.Parent := TempNode;
end;

procedure TBinaryTree.RotateRight(Node: PBinaryNode);
var
	TempNode: PBinaryNode;
begin
	if Node = nil then
  	Exit;
	TempNode := Node.Left;
//  if TempNode = nil then 	Exit;
  Node.Left := TempNode.Right;
  if TempNode.Right <> nil then
  	TempNode.Right.Parent := Node;
  TempNode.Parent := Node.Parent;
  if Node.Parent = nil then
  	FRoot := TempNode
  else if Node.Parent.Right = Node then
  	Node.Parent.Right := TempNode
  else
		Node.Parent.Left := TempNode;
  TempNode.Right := Node;
  Node.Parent := TempNode;
end;

procedure TBinaryTree.SetTraverseOrder(Value: TTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TBinaryTree.Size: Integer;
begin
	Result := FCount;
end;

end.
