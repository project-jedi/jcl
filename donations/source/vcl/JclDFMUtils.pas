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
{ The Original Code is JclDFMUtils.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Contains util routines for DFM reading and writing...                                            }
{                                                                                                  }
{ Known Issues:                                                                                    }
{   This is a preview - class and functionnames might be changed                                   }
{                                                                                                  }
{ Unit owner: Uwe Schuster                                                                         }
{ Last modified: October 27, 2003                                                                  }
{                                                                                                  }
{**************************************************************************************************}

unit JclDFMUtils;

{$I jcl.inc}

interface

uses
  Windows, Classes, SysUtils, Controls, ImgList, JclDFM, Graphics, Forms,
  ComCtrls;

procedure ReadImageListFromDFMComponent(AImageList: TCustomImageList;
  ADFMComponent: TDFMComponent);
procedure ExtractImageList2BitmapsFromDFMComponent(ABitmapList: TList;
  ADFMComponent: TDFMComponent);
procedure LoadLayout(AControl: TControl; ADFMComponent: TDFMComponent); overload;
procedure LoadLayout(AControl: TControl; AStream: TStream); overload;
procedure LoadLayout(AControl: TControl; AFileName: string); overload;
procedure ReadTreeViewItemsFromDFMComponent(ATreeView: TCustomTreeView;
  ADFMComponent: TDFMComponent);

implementation

type
  TDFMImagelist = class(TCustomImageList);
  TDFMTreeView = class(TCustomTreeView);
  TDFMTreeNodes = class(TTreeNodes);

procedure ReadImageListFromDFMComponent(AImageList: TCustomImageList;
  ADFMComponent: TDFMComponent);
var
  DFMFiler: TDFMFiler;
  FilerStream, BitmapStream: TMemoryStream;
  i: Integer;
  BitmapReadProc: TStreamProc;
begin
  if Assigned(AImageList) and Assigned(ADFMComponent) then
  begin
    BitmapStream := nil;
    for i := 0 to Pred(ADFMComponent.Propertys.Count) do
      if (ADFMComponent.Propertys[i].Typ = vaBinary) and
        SameText(ADFMComponent.Propertys[i].Name, 'Bitmap')
      then
      begin
        BitmapStream := ADFMComponent.Propertys[i].AsStream;
        Break;
      end;
    if Assigned(BitmapStream) then
    begin
      FilerStream := nil;
      DFMFiler := nil;
      try
        FilerStream := TMemoryStream.Create;
        DFMFiler := TDFMFiler.Create(FilerStream, 0);
        TDFMImagelist(AImageList).DefineProperties(DFMFiler);
        if BitmapStream.Size > 0 then
        begin
          BitmapStream.Position := 0;
          BitmapReadProc := DFMFiler.GetBinaryReadProcByName('Bitmap');
          if Assigned(BitmapReadProc) then
            BitmapReadProc(BitmapStream);
        end;
      finally
        DFMFiler.Free;
        FilerStream.free;
      end;
    end;
  end;
end;

procedure ExtractImageList2BitmapsFromDFMComponent(ABitmapList: TList;
  ADFMComponent: TDFMComponent);
var
  ImageList: TImageList;
  i: Integer;
  Bitmap: TBitmap;
begin
  if Assigned(ABitmapList) and Assigned(ADFMComponent) then
  begin
    ImageList := nil;
    try
      ImageList := TImageList.Create(nil);
      ReadImageListFromDFMComponent(ImageList, ADFMComponent);
      if ImageList.Count > 0 then
      begin
        for i := 0 to Pred(ImageList.Count) do
        begin
          Bitmap := TBitmap.Create;
          Bitmap.Width := ImageList.Width;
          Bitmap.Height := ImageList.Height;
          ImageList.Draw(Bitmap.Canvas, 0, 0, i);
          ABitmapList.Add(Bitmap);
        end;
      end;
    finally
      ImageList.Free;
    end;
  end;
end;

procedure Controls2List(AList: TList; AControl: TControl);
var
  i: Integer;
begin
  if Assigned(AList) and Assigned(AControl) then
  begin
    if AList.IndexOf(AControl) = -1 then
      AList.Add(AControl);
    if (AControl is TWinControl) and
      (csAcceptsControls in AControl.ControlStyle)
    then
      for i := 0 to Pred(TWinControl(AControl).ControlCount) do
        Controls2List(AList, TWinControl(AControl).Controls[i]);
  end;
end;

procedure LoadControlFromDFMComponent(ADFMComponent: TDFMComponent;
  AControl: TControl);
var
  ObjectStream: TMemoryStream;
begin
  ObjectStream := nil;
  try
    ObjectStream := TMemoryStream.Create;
    ADFMComponent.GetObjectBinary(ObjectStream, False);
    ObjectStream.Position := 0;
    ObjectStream.ReadComponent(AControl);
  finally
    ObjectStream.Free;
  end;
end;

procedure LoadControlsFromDFMComponent(ADFMComponent: TDFMComponent;
  AParentControl: TControl; AControlList: TList);
var
  i: Integer;
  CControl: TControl;
begin
  CControl := nil;
  for i := 0 to AControlList.Count - 1 do
    if TControl(AControlList[i]).Name = ADFMComponent.ComponentName then
    begin
      CControl := AControlList[i];
      Break;
    end;
  if Assigned(CControl) then
  begin
    if AParentControl is TWinControl then
      CControl.Parent := TWinControl(AParentControl);
    CControl.Visible := True; 
    LoadControlFromDFMComponent(ADFMComponent, CControl);
    if ADFMComponent.SubComponents.Count > 0 then
      for i := 0 to ADFMComponent.SubComponents.Count - 1 do
        LoadControlsFromDFMComponent(ADFMComponent.SubComponents[i], CControl,
          AControlList);
  end;
end;

procedure LoadLayout(AControl: TControl; ADFMComponent: TDFMComponent); overload;
var
  ControlList: TList;
  ControlComponent: TDFMComponent;
  i: Integer;
begin
  if Assigned(AControl) and Assigned(ADFMComponent) then
  begin
    ControlComponent := ADFMComponent.FindComponent(AControl.Name);
    if Assigned(ControlComponent) then
    begin
      ControlList := nil;
      try
        ControlList := TList.Create;
        Controls2List(ControlList, AControl);
        for i := 0 to ControlList.Count - 1 do
          if ControlList[i] <> AControl then
            with TControl(ControlList[i]) do
            begin
              if AControl is TWinControl then
                Parent := TWinControl(AControl);
              Visible := False;
            end;
        LoadControlsFromDFMComponent(ControlComponent, nil, ControlList);
      finally
        ControlList.Free;
      end;
    end;
  end;
end;

procedure LoadLayout(AControl: TControl; AStream: TStream); overload;
var
  RCOMP: TDFMRootComponent;
begin
  if Assigned(AControl) and Assigned(AStream) then
  begin
    RCOMP := nil;
    try
      RCOMP := TDFMRootComponent.Create;
      RCOMP.LoadFromStream(AStream);
      LoadLayout(AControl, RCOMP);
    finally
      RCOMP.Free;
    end;
  end;
end;

procedure LoadLayout(AControl: TControl; AFileName: string);
var
  RCOMP: TDFMRootComponent;
begin
  if Assigned(AControl) and FileExists(AFileName) then
  begin
    RCOMP := nil;
    try
      RCOMP := TDFMRootComponent.Create;
      RCOMP.LoadFromFile(AFileName);
      LoadLayout(AControl, RCOMP);
    finally
      RCOMP.Free;
    end;
  end;
end;

procedure ReadTreeViewItemsFromDFMComponent(ATreeView: TCustomTreeView;
  ADFMComponent: TDFMComponent);
var
  DFMFiler: TDFMFiler;
  FilerStream, ItemsStream: TMemoryStream;
  i: Integer;
  ItemsReadProc: TStreamProc;
begin
  if Assigned(ATreeView) and Assigned(ADFMComponent) then
  begin
    TDFMTreeView(ATreeView).Items.Clear;
    ItemsStream := nil;
    for i := 0 to Pred(ADFMComponent.Propertys.Count) do
      if (ADFMComponent.Propertys[i].Typ = vaBinary) and
        SameText(ADFMComponent.Propertys[i].Name, 'Items.Data')
      then
      begin
        ItemsStream := ADFMComponent.Propertys[i].AsStream;
        Break;
      end;
    if Assigned(ItemsStream) then
    begin
      FilerStream := nil;
      DFMFiler := nil;
      try
        FilerStream := TMemoryStream.Create;
        DFMFiler := TDFMFiler.Create(FilerStream, 0);
        TDFMTreeNodes(TDFMTreeView(ATreeView).Items).DefineProperties(DFMFiler);
        if ItemsStream.Size > 0 then
        begin
          ItemsStream.Position := 0;
          ItemsReadProc := DFMFiler.GetBinaryReadProcByName('Data');
          if Assigned(ItemsReadProc) then
            ItemsReadProc(ItemsStream);
        end;
      finally
        DFMFiler.Free;
        FilerStream.free;
      end;
    end;
  end;
end;

end.
