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
    memSkipProperties: TMemo;
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

{$R *.dfm}

procedure AddDFMObject2Tree(ATreeView: TTreeView; ANode: TTreeNode; ADFMObj: TJclDFMComponent);
var
  I: Integer;
  an, an2: TTreeNode;
  s: string;
  PropertyValueString: string;
begin
  an := ATreeView.Items.AddChild(ANode, ADFMObj.ComponentName + ':' +
    ADFMObj.ComponentClassName);
  if ADFMObj.Properties.Count > 0 then
  begin
    an2 := ATreeView.Items.AddChild(an, 'Properties');
    for I := 0 to ADFMObj.Properties.Count - 1 do
    begin
      PropertyValueString := ADFMObj.Properties[I].AsString;
      if Length(PropertyValueString) > 100 then
        PropertyValueString := Copy(PropertyValueString, 1, 100) + '...';
      S := Format('=%s', [PropertyValueString]);
      ATreeView.Items.AddChild(an2, ADFMObj.Properties[I].Name + ' ' +
        ValueTypeToString(ADFMObj.Properties[I].Typ) + S);
    end;
  end;
  if ADFMObj.SubComponents.Count > 0 then
  begin
    an2 := ATreeView.Items.AddChild(an, 'SubComponents');
    for I := 0 to ADFMObj.SubComponents.Count - 1 do
      AddDFMObject2Tree(ATreeView, an2, ADFMObj.SubComponents[I]);
  end;
end;

procedure DrawImageList2CanvasFromDFMComponent(ADFMComponent: TJclDFMComponent;
  ACanvas: TCanvas; Ax, Ay: Integer);
var
  BitmapList: TList;
  I: Integer;
  Bitmap: TBitmap;
  cx: Integer;
begin
  BitmapList := TList.Create;
  try
    ExtractImageList2BitmapsFromDFMComponent(BitmapList, ADFMComponent);
    if BitmapList.Count > 0 then
    begin
      cx := 0;
      for I := 0 to BitmapList.Count - 1 do
      begin
        Bitmap := BitmapList[I];
        if Assigned(Bitmap) then
        begin
          ACanvas.Draw(Ax + cx, Ay, Bitmap);
          Inc(cx, Bitmap.Width);
          Bitmap.Free;
        end;
      end;
    end;
  finally
    BitmapList.Free;
  end;
end;

procedure TfmJclDFMTest.btnDFM2TreeClick(Sender: TObject);
var
  RComp: TJclDFMRootComponent;
begin
  if OpenDialog.Execute then
  begin
    RComp := TJclDFMRootComponent.Create;
    try
      RComp.LoadFromFile(OpenDialog.FileName);
      AddDFMObject2Tree(tvDFMTree, nil, RComp);
    finally
      RComp.Free;
    end;
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
  RComp: TJclDFMRootComponent;
begin
  if OpenDialog.Execute then
  begin
    RComp := TJclDFMRootComponent.Create;
    try
      RComp.LoadFromFile(OpenDialog.FileName);
      DFMRemoveUnwantedComponentsAndProps(RComp, nil, memSkipProperties.Lines);
      SaveDialog.FileName := ChangeFileExt(OpenDialog.FileName, '.cleaned_dfm');
      if SaveDialog.Execute then
        RComp.SaveToFile(SaveDialog.FileName);
    finally
      RComp.Free;
    end;
  end;
end;

procedure TfmJclDFMTest.btnExtractImageListsClick(Sender: TObject);
var
  RComp: TJclDFMRootComponent;
  ComponentList: TList;
  I, J, imgcy, tw, imgh: Integer;
  S: string;
begin
  if OpenDialog.Execute then
  begin
    Image.Canvas.FillRect(Rect(0,0,Image.Width, Image.Height));
    RComp := TJclDFMRootComponent.Create;
    try
      RComp.LoadFromFile(OpenDialog.FileName);
      ComponentList := TList.Create;
      try
        if RComp.FindComponentsByClass('TImageList', ComponentList) > 0 then
        begin
          imgcy := 2;
          for I := 0 to ComponentList.Count - 1 do
            with TJclDFMComponent(ComponentList[I]) do
            begin
              S := Format('%s.%s', [RComp.ComponentClassName, ComponentName]);
              with Image.Canvas do
              begin
                tw := TextWidth(S);
                TextOut(2, imgcy, S);
              end;
              DrawImageList2CanvasFromDFMComponent(ComponentList[I],
                Image.Canvas, tw + 7, imgcy);
              imgh := 16;
              for J := 0 to Properties.Count - 1 do
                if SameText(Properties[J].Name, 'Height') then
                begin
                  imgh := Properties[J].AsInteger;
                  Break;
                end;
              Inc(imgcy, imgh + 4);
            end;
        end;
      finally
        ComponentList.Free;
      end;
    finally
      RComp.Free;
    end;
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
  RComp: TJclDFMRootComponent;
  ComponentList: TList;
begin
  if OpenDialog.Execute then
  begin
    RComp := TJclDFMRootComponent.Create;
    try
      RComp.LoadFromFile(OpenDialog.FileName);
      ComponentList := TList.Create;
      try
        if RComp.FindComponentsByClass('TTreeView', ComponentList) > 0 then
          ReadTreeViewItemsFromDFMComponent(tvItems, ComponentList[0]);
      finally
        ComponentList.Free;
      end;
    finally
      RComp.Free;
    end;
    PageControl1.ActivePage := tsTVItems;
  end;
end;

end.
