unit ListExampleMain;

interface

uses
  {$IFDEF WIN32}
  Windows, Messages, Graphics, Controls, Forms, Dialogs, StdCtrls,
  {$ENDIF}
  {$IFDEF LINUX}
  QForms, QControls, QStdCtrls,
  {$ENDIF}
  SysUtils, Classes;

type
  TMainForm = class(TForm)
    btnIntfArrayList: TButton;
    btnIntfLinkedList: TButton;
    btnIntfVector: TButton;
    btnArrayList: TButton;
    btnLinkedList: TButton;
    btnVector: TButton;
    memResult: TMemo;
    btnMyObjectList: TButton;
    btnStrArrayList: TButton;
    btnStrLinkedList: TButton;
    btnStrVector: TButton;
    procedure btnIntfArrayListClick(Sender: TObject);
    procedure btnIntfLinkedListClick(Sender: TObject);
    procedure btnIntfVectorClick(Sender: TObject);
    procedure btnArrayListClick(Sender: TObject);
    procedure btnLinkedListClick(Sender: TObject);
    procedure btnVectorClick(Sender: TObject);
    procedure btnMyObjectListClick(Sender: TObject);
    procedure btnStrArrayListClick(Sender: TObject);
    procedure btnStrLinkedListClick(Sender: TObject);
    procedure btnStrVectorClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  IIntfMyObject = interface
    ['{BA33CBCC-9CB2-4672-BF54-F52C2A0BEFFE}']
    function GetInt: Integer;
    function GetStr: string;
    procedure SetInt(Value: Integer);
    procedure SetStr(const Value: string);
    property Int: Integer read GetInt write SetInt;
    property Str: string read GetStr write SetStr;
  end;

  TIntfMyObject = class(TInterfacedObject, IIntfMyObject)
  private
    FInt: Integer;
    FStr: string;
  protected
  { IIntfMyObject }
    function GetInt: Integer;
    function GetStr: string;
    procedure SetInt(Value: Integer);
    procedure SetStr(const Value: string);
  end;

  IPerson = interface
    ['{755C857B-A9E2-4D9D-8418-541CAEA79679}']
    function GetAge: Integer;
    function GetMarried: Boolean;
    function GetName: string;
    procedure SetAge(Value: Integer);
    procedure SetMarried(Value: Boolean);
    procedure SetName(const Value: string);
    property Age: Integer read GetAge write SetAge;
    property Married: Boolean read GetMarried write SetMarried;
    property Name: string read GetName write SetName;
  end;

  TPerson = class(TInterfacedObject, IPerson)
  private
    FName: string;
    FAge: Integer;
    FMarried: Boolean;
  protected
  { IPerson }
    function GetAge: Integer;
    function GetMarried: Boolean;
    function GetName: string;
    procedure SetAge(Value: Integer);
    procedure SetMarried(Value: Boolean);
    procedure SetName(const Value: string);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses JclContainerIntf, JclArrayLists, JclLinkedLists, JclVectors, MyObjectList;

{ TIntfMyObject }

function TIntfMyObject.GetInt: Integer;
begin
  Result := FInt;
end;

function TIntfMyObject.GetStr: string;
begin
  Result := FStr;
end;

procedure TIntfMyObject.SetInt(Value: Integer);
begin
  FInt := Value;
end;

procedure TIntfMyObject.SetStr(const Value: string);
begin
  FStr := Value;
end;

procedure TMainForm.btnIntfArrayListClick(Sender: TObject);
var
  List, Sub: IJclIntfList;
  MyArray: IJclIntfArray;
  MyObject: IIntfMyObject;
  It: IJclIntfIterator;
  I: Integer;
begin
  memResult.Lines.Clear;
  List := TJclIntfArrayList.Create;
  MyObject := TIntfMyObject.Create;
  MyObject.Int := 42;
  MyObject.Str := 'MyString';
  List.Add(MyObject);

  MyObject := IIntfMyObject(List.GetObject(0));
  //memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);

  MyObject := TIntfMyObject.Create;
  MyObject.Int := 41;
  MyObject.Str := 'AnotherString';
  List.Add(MyObject);

  Sub := List.SubList(0, 10);

  // Iteration
  It := Sub.First;
  while It.HasNext do
  begin
    MyObject := IIntfMyObject(It.Next);
    memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
  end;
  // use [] default of Items[]
  MyArray := List as IJclIntfArray;
  for I := 0 to MyArray.Size - 1 do
  begin
    MyObject := IIntfMyObject(MyArray[I]);
    memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
  end;
end;

procedure TMainForm.btnIntfLinkedListClick(Sender: TObject);
var
  List, Sub: IJclIntfList;
  MyObject: IIntfMyObject;
  It: IJclIntfIterator;
begin
  memResult.Lines.Clear;
  List := TJclIntfLinkedList.Create;
  MyObject := TIntfMyObject.Create;
  MyObject.Int := 42;
  MyObject.Str := 'MyString';
  List.Add(MyObject);
  MyObject := IIntfMyObject(List.GetObject(0));
  memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);

  MyObject := TIntfMyObject.Create;
  MyObject.Int := 41;
  MyObject.Str := 'AnotherString';
  List.Add(MyObject);

  Sub := List.SubList(1, 10);

  It := Sub.First;
  while It.HasNext do
  begin
    MyObject := IIntfMyObject(It.Next);
    memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
  end;
end;

procedure TMainForm.btnIntfVectorClick(Sender: TObject);
var
  List: TJclIntfVector;
  MyObject: IIntfMyObject;
  It: IJclIntfIterator;
  I: Integer;
begin
  memResult.Lines.Clear;
  List := TJclIntfVector.Create;
  try
    MyObject := TIntfMyObject.Create;
    MyObject.Int := 42;
    MyObject.Str := 'MyString';
    List.Add(MyObject);
    MyObject := IIntfMyObject(List.GetObject(0));
    memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);

    MyObject := TIntfMyObject.Create;
    MyObject.Int := 41;
    MyObject.Str := 'AnotherString';
    List.Add(MyObject);
    It := List.First;
    while It.HasNext do
    begin
      MyObject := IIntfMyObject(It.Next);
      memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
    end;
    // Fastest way
    for I := 0 to List.Size - 1 do
    begin
      MyObject := IIntfMyObject(List.Items[I]);
      memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
    end;
    List.Clear;
  finally
    It := nil; // Force release Iterator before free list !
    List.Free; // No Ref Count
  end;
end;

procedure TMainForm.btnArrayListClick(Sender: TObject);
var
  List: IJclList;
  MyObject: TMyObject;
  It: IJclIterator;
begin
  memResult.Lines.Clear;
  List := TJclArrayList.Create;
  MyObject := TMyObject.Create;
  MyObject.Int := 42;
  MyObject.Str := 'MyString';
  List.Add(MyObject);
  MyObject := TMyObject(List.GetObject(0));
  memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);

  MyObject := TMyObject.Create;
  MyObject.Int := 41;
  MyObject.Str := 'AnotherString';
  List.Add(MyObject);

  It := List.First;
  while It.HasNext do
  begin
    MyObject := TMyObject(It.Next);
    memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
  end;
  It := List.First;
  while It.HasNext do
    It.Remove;
end;

procedure TMainForm.btnLinkedListClick(Sender: TObject);
var
  List: IJclList;
  MyObject: TMyObject;
  It: IJclIterator;
begin
  memResult.Lines.Clear;
  List := TJclLinkedList.Create;
  MyObject := TMyObject.Create;
  MyObject.Int := 42;
  MyObject.Str := 'MyString';
  List.Add(MyObject);
  MyObject := TMyObject(List.GetObject(0));
  memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);

  MyObject := TMyObject.Create;
  MyObject.Int := 41;
  MyObject.Str := 'AnotherString';
  List.Add(MyObject);

  It := List.First;
  while It.HasNext do
  begin
    MyObject := TMyObject(It.Next);
    memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
  end;
end;

procedure TMainForm.btnVectorClick(Sender: TObject);
var
  List: TJclVector;
  MyObject: TMyObject;
  It: IJclIterator;
  I: Integer;
begin
  memResult.Lines.Clear;
  List := TJclVector.Create;
  try
    MyObject := TMyObject.Create;
    MyObject.Int := 42;
    MyObject.Str := 'MyString';
    List.Add(MyObject);
    MyObject := TMyObject(List.GetObject(0));
    memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);

    MyObject := TMyObject.Create;
    MyObject.Int := 41;
    MyObject.Str := 'AnotherString';
    List.Add(MyObject);

    It := List.First;
    while It.HasNext do
    begin
      MyObject := TMyObject(It.Next);
      memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
    end;
    // Fastest way
    for I := 0 to List.Size - 1 do
    begin
      MyObject := TMyObject(List.Items[I]);
      memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
    end;
    List.Clear;
  finally
    It := nil; // Force release Iterator before free list !
    List.Free; // No ref count
  end;
end;

procedure TMainForm.btnMyObjectListClick(Sender: TObject);
var
  List: IMyObjectList;
  MyObject: TMyObject;
begin
  memResult.Lines.Clear;
  List := TMyObjectList.Create;
  MyObject := TMyObject.Create;
  MyObject.Int := 42;
  MyObject.Str := 'MyString';
  List.Add(MyObject);
  memResult.Lines.Add(IntToStr(List.GetObject(0).Int));
  memResult.Lines.Add(List.GetObject(0).Str);
end;

procedure TMainForm.btnStrArrayListClick(Sender: TObject);
var
  List, Sub: IJclStrList;
  MyArray: IJclStrArray;
  It: IJclStrIterator;
  I: Integer;
  S: string;
begin
  memResult.Lines.Clear;
  List := TJclStrArrayList.Create;
  List.Add('MyString');

  S := List.GetString(0);
  //memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);

  List.Add('AnotherString');

  Sub := List.SubList(0, 10);
  // Iteration
  It := Sub.First;
  while It.HasNext do
  begin
    S := It.Next;
    memResult.Lines.Add(S);
  end;
  // use [] default of Items[]
  MyArray := List as IJclStrArray;
  for I := 0 to MyArray.Size - 1 do
  begin
    S := MyArray[I];
    memResult.Lines.Add(S);
  end;
end;

{ TPerson }

function TPerson.GetAge: Integer;
begin
  Result := FAge;
end;

function TPerson.GetMarried: Boolean;
begin
  Result := FMarried;
end;

function TPerson.GetName: string;
begin
  Result := FName;
end;

procedure TPerson.SetAge(Value: Integer);
begin
  FAge := Value;
end;

procedure TPerson.SetMarried(Value: Boolean);
begin
  FMarried := Value;
end;

procedure TPerson.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TMainForm.btnStrLinkedListClick(Sender: TObject);
var
  List, Sub: IJclStrList;
  S: string;
  It: IJclStrIterator;
begin
  memResult.Lines.Clear;
  List := TJclStrLinkedList.Create;
  List.Add('MyString');
  memResult.Lines.Add(List.GetString(0));

  List.Add('AnotherString');

  Sub := List.SubList(1, 10);

  It := Sub.First;
  while It.HasNext do
  begin
    S := It.Next;
    memResult.Lines.Add(S);
  end;
end;

procedure TMainForm.btnStrVectorClick(Sender: TObject);
var
  List: TJclStrVector;
  S: string;
  It: IJclStrIterator;
  I: Integer;
begin
  memResult.Lines.Clear;
  List := TJclStrVector.Create;
  try
    List.Add('MyString');
    S := List.GetString(0);
    memResult.Lines.Add(S);

    List.Add('AnotherString');

    It := List.First;
    while It.HasNext do
    begin
      S := It.Next;
      memResult.Lines.Add(S);
    end;
    // Fastest way
    for I := 0 to List.Size - 1 do
    begin
      S := List.Items[I];
      memResult.Lines.Add(S);
    end;
    List.Clear;
  finally
    It := nil; // Force release Iterator before free list !
    List.Free; // No Ref Count
  end;
end;

end.

