unit TreeExampleMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JclBase;

type
  TMainForm = class(TForm)
    btnIntfArrayTree: TButton;
    memoResult: TMemo;
    btnArrayTree: TButton;
    btnStrBinaryTree: TButton;
    procedure btnIntfArrayTreeClick(Sender: TObject);
    procedure btnArrayTreeClick(Sender: TObject);
    procedure btnStrBinaryTreeClick(Sender: TObject);
  public
  end;

  IIntfInteger = interface
    ['{0E32C3C9-5940-4373-B3BA-644473E3F3C2}']
    function GetValue: Integer;
    procedure SetValue(AValue: Integer);
    property Value: Integer read GetValue write SetValue;
  end;

  TIntfInteger = class(TInterfacedObject, IIntfInteger)
  private
    FValue: Integer;
    function GetValue: Integer;
    procedure SetValue(AValue: Integer);
  public
    constructor Create(AValue: Integer);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  JclContainerIntf, JclAlgorithms, JclBinaryTrees;

{ TIntfInteger }

constructor TIntfInteger.Create(AValue: Integer);
begin
  inherited Create;
  FValue := AValue;
end;

function TIntfInteger.GetValue: Integer;
begin
  Result := FValue;
end;

procedure TIntfInteger.SetValue(AValue: Integer);
begin
  FValue := AValue;
end;

function IntfIntegerComparator(AIntf1, AIntf2: IInterface): Integer;
begin
  Result := (AIntf1 as IIntfInteger).Value - (AIntf2 as IIntfInteger).Value;
end;

procedure TMainForm.btnIntfArrayTreeClick(Sender: TObject);
var
  Tree: IJclIntfTree;
  I: Integer;
  Obj: IIntfInteger;
  It: IJclIntfIterator;
begin
  memoResult.Lines.Clear;
  Tree := TJclIntfBinaryTree.Create(IntfIntegerComparator);
  for I := 0 to 17 do
  begin
    Obj := TIntfInteger.Create(I);
    Tree.Add(Obj);
  end;

  if Tree.Contains(TIntfInteger.Create(15)) then
    memoResult.Lines.Add('contains 15');

  Tree.TraverseOrder := toPostOrder;
  It := Tree.Last;
  while It.HasPrevious do
  begin
    Obj := It.Previous as IIntfInteger;
    memoResult.Lines.Add(IntToStr(Obj.Value));
  end;

  It := Tree.First;
  while It.HasNext do
    It.Remove;
end;

procedure TMainForm.btnArrayTreeClick(Sender: TObject);
var
  Tree: IJclTree;
  I: Integer;
  It: IJclIterator;
begin
  memoResult.Lines.Clear;
  Tree := TJclBinaryTree.Create(JclAlgorithms.IntegerCompare);
  for I := 0 to 17 do
    Tree.Add(TObject(I));

  if Tree.Contains(TObject(15)) then
    memoResult.Lines.Add('contains 15');

  Tree.TraverseOrder := toOrder;
  It := Tree.First;
  while It.HasNext do
    memoResult.Lines.Add(IntToStr(Integer(It.Next)));
end;

procedure TMainForm.btnStrBinaryTreeClick(Sender: TObject);
var
  Tree: IJclStrTree;
  I: Integer;
  It: IJclStrIterator;
begin
  memoResult.Lines.Clear;
  Tree := TJclStrBinaryTree.Create(JclAlgorithms.StrSimpleCompare);
  for I := 0 to 17 do
    Tree.Add(Format('%.2d', [I]));

  if Tree.Contains('15') then
    memoResult.Lines.Add('contains 15');

  Tree.TraverseOrder := toOrder;
  It := Tree.First;
  while It.HasNext do
    memoResult.Lines.Add(It.Next);
end;

end.

