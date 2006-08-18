{==============================================================================|
| Project : ToolHelp Viewer                                      | 000.005.000 |
|==============================================================================|
| Content:  Global images and functions                                        |
|==============================================================================|
| The contents of this file are subject to the Mozilla Public License Ver. 1.0 |
| (the "License"); you may not use this file except in compliance with the     |
| License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ |
|                                                                              |
| Software distributed under the License is distributed on an "AS IS" basis,   |
| WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for |
| the specific language governing rights and limitations under the License.    |
|==============================================================================|
| The Original Code is Petr Vones.                                             |
|==============================================================================|
| The Initial Developer of the Original Code is Petr Vones, petr.v@mujmail.cz  |
| Portions created by Petr Vones are Copyright (C) 1999.                       |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================}

unit Global;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ImgList;

type
  TGlobalModule = class(TDataModule)
    ToolbarImagesList: TImageList;
    SaveDialog: TSaveDialog;
    procedure DataModuleCreate(Sender: TObject);
  private
    FPeViewer: Variant;
    FPeViewerRegistred: Boolean;
  public
    function ExecuteSaveDialog(var FileName: TFileName): Boolean;
    procedure ListViewToClipboard(ListView: TListView);
    procedure ListViewToFile(ListView: TListView; const FileName: TFileName);
    procedure ViewPE(const FileName: TFileName);
    property PeViewerRegistred: Boolean read FPeViewerRegistred;
  end;

var
  GlobalModule: TGlobalModule;

implementation

{$R *.DFM}

uses
  ClipBrd, ToolsUtils, JclSysInfo;

resourcestring
  sWrongWindowsVersion = 'This application is intended for Windows 95/98/2000 only';

procedure CheckWindowsVersion;
begin
  if IsWinNT4 then
  begin
    MessageBox(0, PChar(sWrongWindowsVersion), nil, MB_OK or MB_ICONERROR);
    Halt(0);
  end;
end;

{ TGlobalModule }

procedure TGlobalModule.ListViewToClipboard(ListView: TListView);
var
  S: TStringList;
begin
  S := TStringList.Create;
  Screen.Cursor := crHourGlass;
  try
    ListViewToStrings(ListView, S, ListView.MultiSelect);
    Clipboard.AsText := S.Text;
  finally
    S.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TGlobalModule.ListViewToFile(ListView: TListView; const FileName: TFileName);
var
  S: TStringList;
begin
  SaveDialog.FileName := ChangeFileExt(FileName, '');
  if SaveDialog.Execute then
  begin
    S := TStringList.Create;
    Screen.Cursor := crHourGlass;
    try
      ListViewToStrings(ListView, S, ListView.MultiSelect);
      S.SaveToFile(SaveDialog.FileName);
    finally
      S.Free;
      Screen.Cursor := crDefault;
    end;
  end;
end;

function TGlobalModule.ExecuteSaveDialog(var FileName: TFileName): Boolean;
begin
  SaveDialog.FileName := ChangeFileExt(FileName, '');
  Result := SaveDialog.Execute;
  if Result then FileName := SaveDialog.FileName;
end;

procedure TGlobalModule.DataModuleCreate(Sender: TObject);
begin
  FPeViewerRegistred := IsPeViewerRegistred;
end;

procedure TGlobalModule.ViewPE(const FileName: TFileName);
begin
  FPeViewer := CreateOrGetOleObject(PeViewerClassName);
  FPeViewer.OpenFile(FileName);
  FPeViewer.BringToFront;
end;

initialization
  CheckWindowsVersion;

end.
