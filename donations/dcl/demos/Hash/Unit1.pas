unit Unit1;

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
  TfrmHash = class(TForm)
    btnIntfIntfHashMap: TButton;
    btnIntfHashSet: TButton;
    btnHashMap: TButton;
    btnHashSet: TButton;
    memResult: TMemo;
    btnStrIntfHashMap: TButton;
    btnIntfArraySet: TButton;
    btnArraySet: TButton;
    btnStrStrHashMap: TButton;
    btnStrHashMap: TButton;
    btnStrHashSet: TButton;
    btnStrArraySet: TButton;
    procedure btnIntfIntfHashMapClick(Sender: TObject);
    procedure btnStrIntfHashMapClick(Sender: TObject);
    procedure btnHashMapClick(Sender: TObject);
    procedure btnIntfHashSetClick(Sender: TObject);
    procedure btnHashSetClick(Sender: TObject);
    procedure btnIntfArraySetClick(Sender: TObject);
    procedure btnArraySetClick(Sender: TObject);
    procedure btnStrStrHashMapClick(Sender: TObject);
    procedure btnStrHashMapClick(Sender: TObject);
    procedure btnStrHashSetClick(Sender: TObject);
    procedure btnStrArraySetClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  IIntfMyObject = interface
    ['{B2CB604F-4F5F-44D8-A86F-6138CD329B42}']
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

  TMyObject = class(TObject)
  private
    FInt: Integer;
    FStr: string;
  public
    property Int: Integer read FInt write FInt;
    property Str: string read FStr write FStr;
  end;

var
  frmHash: TfrmHash;

implementation

{$R *.dfm}

uses JclDCL_intf, JclHashMap, JclHashSet, JclArraySet;

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

procedure TfrmHash.btnIntfIntfHashMapClick(Sender: TObject);
var
  Map: IIntfIntfMap;
  MyObject: IIntfMyObject;
  KeyObject: TInterfacedObject;
  It: IIntfIterator;
begin
  Map := TJclIntfIntfHashMap.Create;
  MyObject := TIntfMyObject.Create;
  MyObject.Int := 42;
  MyObject.Str := 'MyString';
  KeyObject := TInterfacedObject.Create;
  Map.PutValue(KeyObject, MyObject);
  MyObject := IIntfMyObject(Map.GetValue(KeyObject));
  memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);

  It := Map.Values.First;
  while It.HasNext do
  begin
    memResult.Lines.Add(IIntfMyObject(It.Next).Str);
  end;
  memResult.Lines.Add('--------------------------------------------------------');
end;

procedure TfrmHash.btnStrIntfHashMapClick(Sender: TObject);
var
  Map: IStrIntfMap;
  MyObject: IIntfMyObject;
begin
  Map := TJclStrIntfHashMap.Create;
  MyObject := TIntfMyObject.Create;
  MyObject.Int := 42;
  MyObject.Str := 'MyString';
  Map.PutValue('MyKey', MyObject);
  MyObject := TIntfMyObject.Create;
  MyObject.Int := 43;
  MyObject.Str := 'AnotherString';
  Map.PutValue('MyKey2', MyObject);
  MyObject := IIntfMyObject(Map.GetValue('MyKey2'));
  memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
  memResult.Lines.Add('--------------------------------------------------------');
end;

procedure TfrmHash.btnHashMapClick(Sender: TObject);
var
  Map: IMap;
  MyObject: TMyObject;
  KeyObject: TObject;
  It: IIterator;
begin
  Map := TJclHashMap.Create;
  MyObject := TMyObject.Create;
  KeyObject := TObject.Create;
  try
    MyObject.Int := 42;
    MyObject.Str := 'MyString';
    Map.PutValue(KeyObject, MyObject);
    MyObject := TMyObject(Map.GetValue(KeyObject));
    memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
    It := Map.Values.First;
    while It.HasNext do
      memResult.Lines.Add(TMyObject(It.Next).Str);
    memResult.Lines.Add('--------------------------------------------------------');
  finally
    // MyObject.Free;  // Free in the map (Default: OwnsObject = True)
    // KeyObject.Free;
  end;
end;

procedure TfrmHash.btnIntfHashSetClick(Sender: TObject);
var
  MySet: IIntfSet;
  MyObject: IIntfMyObject;
  It: IIntfIterator;
begin
  MySet := TJclIntfHashSet.Create;
  MyObject := TIntfMyObject.Create;
  MyObject.Int := 42;
  MyObject.Str := 'MyString';
  MySet.Add(MyObject);
  MySet.Add(MyObject);
  It := MySet.First;
  while It.HasNext do
    memResult.Lines.Add(IIntfMyObject(It.Next).Str);
  memResult.Lines.Add(IntToStr(MySet.Size));
  memResult.Lines.Add('--------------------------------------------------------');
end;

procedure TfrmHash.btnHashSetClick(Sender: TObject);
var
  MySet: ISet;
  MyObject: TMyObject;
  It: IIterator;
begin
  MySet := TJclHashSet.Create;
  MyObject := TMyObject.Create;
  MyObject.Int := 42;
  MyObject.Str := 'MyString';
  MySet.Add(MyObject);
  MySet.Add(MyObject);
  It := MySet.First;
  while It.HasNext do
    memResult.Lines.Add(TMyObject(It.Next).Str);
  memResult.Lines.Add(IntToStr(MySet.Size));
  memResult.Lines.Add('--------------------------------------------------------');
end;

procedure TfrmHash.btnIntfArraySetClick(Sender: TObject);
var
  MySet: IIntfSet;
  MyObject: IIntfMyObject;
  It: IIntfIterator;
begin
  MySet := TJclIntfArraySet.Create;
  MyObject := TIntfMyObject.Create;
  MyObject.Int := 42;
  MyObject.Str := 'MyString';
  MySet.Add(MyObject);
  MySet.Add(MyObject);
  It := MySet.First;
  while It.HasNext do
    memResult.Lines.Add(IIntfMyObject(It.Next).Str);
  memResult.Lines.Add(IntToStr(MySet.Size));
  memResult.Lines.Add('--------------------------------------------------------');
end;

procedure TfrmHash.btnArraySetClick(Sender: TObject);
var
  MySet: ISet;
  MyObject: TMyObject;
  It: IIterator;
begin
  MySet := TJclArraySet.Create;
  MyObject := TMyObject.Create;
  MyObject.Int := 42;
  MyObject.Str := 'MyString';
  MySet.Add(MyObject);
  MySet.Add(MyObject);
  It := MySet.First;
  while It.HasNext do
    memResult.Lines.Add(TMyObject(It.Next).Str);
  memResult.Lines.Add(IntToStr(MySet.Size));
  memResult.Lines.Add('--------------------------------------------------------');
end;

procedure TfrmHash.btnStrStrHashMapClick(Sender: TObject);
var
  Map: IStrStrMap;
  It: IStrIterator;
begin
  Map := TJclStrStrHashMap.Create;
  Map.PutValue('MyKey1', 'MyString1');
  Map.PutValue('MyKey2', 'MyString2');
  Map.PutValue('MyKey3', 'MyString3');
  It := Map.KeySet.First;
  while It.HasNext do
    memResult.Lines.Add(It.Next);
  It := Map.Values.First;
  while It.HasNext do
    memResult.Lines.Add(It.Next);
  Map.PutValue('MyKey2', 'AnotherString2');
  memResult.Lines.Add(Map.GetValue('MyKey2'));
  memResult.Lines.Add('--------------------------------------------------------');
end;

type
  TLinks = class(TJclStrHashMap);

procedure TfrmHash.btnStrHashMapClick(Sender: TObject);
var
  Map: IStrMap;
  MyObject: TMyObject;
  //It: IStrIterator;
  Links: TLinks;
begin
  Map := TJclStrHashMap.Create;
  MyObject := TMyObject.Create;
  MyObject.Int := 42;
  MyObject.Str := 'MyString';

{  Map.PutValue('MyKey1', MyObject);
  MyObject := TMyObject(Map.GetValue('MyKey1'));
  memResult.Lines.Add(IntToStr(MyObject.Int) + ' ' + MyObject.Str);
  It := Map.KeySet.First;
  while It.HasNext do
    memResult.Lines.Add(It.Next);
  memResult.Lines.Add('--------------------------------------------------------');
  }
  Links := TLinks.Create;
  Links.PutValue('MyKey1', MyObject);
  Links.Remove('MyKey1');
  Links.PutValue('MyKey1', MyObject);
end;

procedure TfrmHash.btnStrHashSetClick(Sender: TObject);
var
  MySet: IStrSet;
  It: IStrIterator;
begin
  MySet := TJclStrHashSet.Create;
  MySet.Add('MyString');
  MySet.Add('MyString');
  It := MySet.First;
  while It.HasNext do
    memResult.Lines.Add(It.Next);
  memResult.Lines.Add(IntToStr(MySet.Size));
  memResult.Lines.Add('--------------------------------------------------------');
end;

procedure TfrmHash.btnStrArraySetClick(Sender: TObject);
var
  MySet: IStrSet;
  It: IStrIterator;
begin
  MySet := TJclStrArraySet.Create;
  MySet.Add('MyString');
  MySet.Add('MyString');
  It := MySet.First;
  while It.HasNext do
    memResult.Lines.Add(It.Next);
  memResult.Lines.Add(IntToStr(MySet.Size));
  memResult.Lines.Add('--------------------------------------------------------');
end;

end.

