unit JclDFMTestMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JclDFM, JclDFMUtils, StdCtrls, ComCtrls, ExtCtrls;

type
  TfmJclDFMTest = class(TForm)
    Panel1: TPanel;
    btnDFM2Tree: TButton;
    btnDFMGetComps: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    btnCleanDFM: TButton;
    PageControl1: TPageControl;
    tsTV: TTabSheet;
    tsComponents: TTabSheet;
    tsSkipList: TTabSheet;
    Memo1: TMemo;
    tvDFMTree: TTreeView;
    memSkipPropertys: TMemo;
    btnExtractImageLists: TButton;
    tsImage: TTabSheet;
    Image: TImage;
    tsLayout: TTabSheet;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    btnLoadLayout: TButton;
    tsTVItems: TTabSheet;
    btnLoadTreeViewItems: TButton;
    tvItems: TTreeView;
    procedure btnDFM2TreeClick(Sender: TObject);
    procedure btnDFMGetCompsClick(Sender: TObject);
    procedure btnCleanDFMClick(Sender: TObject);
    procedure btnExtractImageListsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tsImageResize(Sender: TObject);
    procedure btnLoadLayoutClick(Sender: TObject);
    procedure btnLoadTreeViewItemsClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  fmJclDFMTest: TfmJclDFMTest;

implementation

{$R *.DFM}

procedure AddDFMObject2Tree(ATreeView: TTreeView; ANode: TTreeNode; ADFMObj: TDFMComponent);
var
  i: Integer;
  an,an2: TTreeNode;
  s: string;
  PropertyValueString: string;
begin
  an := ATreeView.Items.AddChild(ANode, ADFMObj.ComponentName + ':' + ADFMObj.ComponentClassName);
  if ADFMObj.Propertys.Count > 0 then
  begin
    an2 := ATreeView.Items.AddChild(an, 'Propertys');
    for i := 0 to Pred(ADFMObj.Propertys.Count) do
    begin
      PropertyValueString := ADFMObj.Propertys[i].AsString;
      if Length(PropertyValueString) > 100 then
        PropertyValueString := Copy(PropertyValueString, 1, 100) + '...';
      s := Format('=%s', [PropertyValueString]);
      ATreeView.Items.AddChild(an2, ADFMObj.Propertys[i].Name + ' ' +
        ValueType2String(ADFMObj.Propertys[i].Typ) + s);
    end;
  end;
  if ADFMObj.SubComponents.Count > 0 then
  begin
    an2 := ATreeView.Items.AddChild(an, 'SubComponents');
    for i := 0 to Pred(ADFMObj.SubComponents.Count) do
      AddDFMObject2Tree(ATreeView, an2, ADFMObj.SubComponents[i]);
  end;
end;

procedure DrawImageList2CanvasFromDFMComponent(ADFMComponent: TDFMComponent;
  ACanvas: TCanvas; Ax, Ay: Integer);
var
  BitmapList: TList;
  i: Integer;
  Bitmap: TBitmap;
  cx: Integer;
begin
  BitmapList := TList.Create;
  ExtractImageList2BitmapsFromDFMComponent(BitmapList, ADFMComponent);
  if BitmapList.Count > 0 then
  begin
    cx := 0;
    for i := 0 to Pred(BitmapList.Count) do
    begin
      Bitmap := BitmapList[i];
      if Assigned(Bitmap) then
      begin
        ACanvas.Draw(Ax + cx, Ay, Bitmap);
        Inc(cx, Bitmap.Width);
        Bitmap.Free;
      end;
    end;
  end;
  BitmapList.Free;
end;

procedure TfmJclDFMTest.btnDFM2TreeClick(Sender: TObject);
var
  RCOMP: TDFMRootComponent;
begin
  if OpenDialog.Execute then
  begin
    RCOMP := TDFMRootComponent.Create;
    RCOMP.LoadFromFile(OpenDialog.FileName);
    AddDFMObject2Tree(tvDFMTree, nil, RCOMP);
    RCOMP.Free;
    PageControl1.ActivePage := tsTV;
  end;
end;

procedure TfmJclDFMTest.btnDFMGetCompsClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    DFMGetAllComponentTypes(OpenDialog.FileName, Memo1.Lines);
    PageControl1.ActivePage := tsComponents;
  end;
end;

procedure TfmJclDFMTest.btnCleanDFMClick(Sender: TObject);
var
  RCOMP: TDFMRootComponent;
begin
  if OpenDialog.Execute then
  begin
    RCOMP := TDFMRootComponent.Create;
    RCOMP.LoadFromFile(OpenDialog.FileName);
    DFMRemoveUnwantedComponentsAndProps(RCOMP, nil, memSkipPropertys.Lines);
    SaveDialog.FileName := ChangeFileExt(OpenDialog.FileName, '.cleaned_dfm');
    if SaveDialog.Execute then
      RCOMP.SaveToFile(SaveDialog.FileName);
    RCOMP.Free;
  end;
end;

procedure TfmJclDFMTest.btnExtractImageListsClick(Sender: TObject);
var
  RCOMP: TDFMRootComponent;
  ComponentList: TList;
  i,j, imgcy, tw, imgh: Integer;
  s: string;
begin
  if OpenDialog.Execute then
  begin
    Image.Canvas.FillRect(Rect(0,0,Image.Width, Image.Height));
    RCOMP := TDFMRootComponent.Create;
    RCOMP.LoadFromFile(OpenDialog.FileName);
    ComponentList := TList.Create;
    if RCOMP.FindComponentsByClass('TImageList', ComponentList) > 0 then
    begin
      imgcy := 2;
      for i := 0 to Pred(ComponentList.Count) do
      with TDFMComponent(ComponentList[i]) do
      begin
        s := Format('%s.%s', [RCOMP.ComponentClassName, ComponentName]);
        with Image.Canvas do
        begin
          tw := TextWidth(s);
          TextOut(2, imgcy, s);
        end;
        DrawImageList2CanvasFromDFMComponent(ComponentList[i],
          Image.Canvas, tw + 7, imgcy);
        imgh := 16;
        for j := 0 to Pred(Propertys.Count) do
          if SameText(Propertys[j].Name, 'Height') then
          begin
            imgh := Propertys[j].AsInteger;
            Break;
          end;
        Inc(imgcy, imgh + 4);
      end;
    end;
    ComponentList.Free;
    RCOMP.Free;
    PageControl1.ActivePage := tsImage;
  end;
end;

procedure TfmJclDFMTest.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage := tsTV;
end;

procedure TfmJclDFMTest.tsImageResize(Sender: TObject);
begin
  Image.Picture.Bitmap.Width := Image.Width;
  Image.Picture.Bitmap.Height := Image.Height;
end;

procedure TfmJclDFMTest.btnLoadLayoutClick(Sender: TObject);
begin
  PageControl1.ActivePage := tsLayout;
  ShowMessage('Old Layout');
  LoadLayout(tsLayout, ExtractFilePath(Application.ExeName)
    + 'LoadLayout.partial_dfm');
  ShowMessage('New Layout');
end;

procedure TfmJclDFMTest.btnLoadTreeViewItemsClick(Sender: TObject);
var
  RCOMP: TDFMRootComponent;
  ComponentList: TList;
begin
  if OpenDialog.Execute then
  begin
    RCOMP := TDFMRootComponent.Create;
    RCOMP.LoadFromFile(OpenDialog.FileName);
    ComponentList := TList.Create;
    if RCOMP.FindComponentsByClass('TTreeView', ComponentList) > 0 then
      ReadTreeViewItemsFromDFMComponent(tvItems, ComponentList[0]);
    ComponentList.Free;
    RCOMP.Free;
    PageControl1.ActivePage := tsTVItems;
  end;
end;

end.
