{******************************************************************************}
{                                                       	               }
{ JEDI HIT - HtmlHelp Inclusion Tool                                             }
{ 								               }
{ 								               }
{ Portions created by Matthias Thoma are                                       }
{ Copyright (C) 2001 Matthias Thoma                                            }
{ 								               }
{ Obtained through:                               	                       }
{ Joint Endeavour of Delphi Innovators (Project JEDI)                          }
{								               }
{ You may retrieve the latest version of this file at the Project              }
{ JEDI home page, located at http://delphi-jedi.org                            }
{								               }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html	                               }
{                                                                              }
{ Software distributed under the License is distributed on an 	               }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License. 			               }
{ 								               }
{******************************************************************************}

unit UnHelpTool;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ValEdit, ExtCtrls, StdCtrls, ComCtrls, Menus, Buttons;
  
type
  TMainForm = class(TForm)
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    lbRegistry: TListBox;
    Panel2: TPanel;
    Panel3: TPanel;
    MainMenu: TMainMenu;
    OpenDialog: TOpenDialog;
    GroupBox1: TGroupBox;
    edPath: TEdit;
    edIndexFile: TEdit;
    cbIndexFile: TCheckBox;
    lbNo1: TLabel;
    btBrowseFile: TButton;
    btBrowseIndex: TButton;
    PopupMenu1: TPopupMenu;
    Delete1: TMenuItem;
    Modify1: TMenuItem;
    N1: TMenuItem;
    Addhelpfile1: TMenuItem;
    sbAddHelpfile: TSpeedButton;
    Help1: TMenuItem;
    N2: TMenuItem;
    About1: TMenuItem;
    btSaveExit: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure btSaveAndExitClick(Sender: TObject);
    procedure btAddHelpfileClick(Sender: TObject);
    procedure vlRegistrySelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure lbRegistryEnter(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure sbAddHelpfileClick(Sender: TObject);
    procedure cbIndexFileClick(Sender: TObject);
    procedure edPathExit(Sender: TObject);
    procedure ReadWriteCheck;
    procedure btSaveExitClick(Sender: TObject);
    procedure edIndexFileExit(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btBrowseFileClick(Sender: TObject);
    procedure btBrowseIndexClick(Sender: TObject);

  private
    FHelpfiles: TStringList;
    FHelpfileNames: TStringList;
    FIndexFileNames: TStringList;
    FRow: Integer;

    procedure LoadFromRegistry;
    procedure SaveToRegistry;


  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation
uses
  Registry;

{$R *.dfm}


//------------------------------------------------------------------------------

procedure TMainForm.ReadWriteCheck;
var
  i: Integer;

begin
  // Check if all Counts are equal
  if (FHelpFiles.Count <> FIndexFileNames.Count) or
  (FHelpFiles.Count <> FHelpFileNames.Count) then
  begin
    ShowMessage('Serious Failure #1');
  end;

  i := 0;

  while i <= (FHelpFiles.Count - 1) do
  begin

  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SaveToRegistry;
var
  i: Integer;

begin
  with TRegistry.Create do
  begin
    DeleteKey('Software\JEDI\JHIT\');
    if OpenKey('Software\JEDI\JHIT\', True) then
    begin
      CloseKey;
      for i := 0 to lbRegistry.Items.Count - 1 do
      begin
        if OpenKey('Software\JEDI\JHIT\'+FHelpFiles.Strings[i] ,True) then
        begin
          WriteString('Path', FHelpFileNames.Strings[i]);
          WriteString('Index File', FIndexFileNames.Strings[i]);
          CloseKey;
        end;
      end;
    end;

    Free;
  end;

end;

//------------------------------------------------------------------------------

procedure TMainForm.LoadFromRegistry;
var
  i: Integer;

begin
  // Read the Registry Entries

  with TRegistry.Create do
  begin
    if OpenKey('Software\JEDI\JHIT\',false) then
    begin
    if HasSubkeys then
      begin
        GetKeyNames(FHelpFiles);
        CloseKey;

        for i := 0 to FHelpFiles.Count-1 do
        begin
           if OpenKey('Software\JEDI\JHIT\'+FHelpFiles.Strings[i],False) then
          begin
            FHelpFileNames.Add(ReadString('Path'));
            FIndexFileNames.Add(ReadString('Index File'));
            CloseKey;
          end
          else
          begin
            FIndexFileNames.Add('');
            FHelpFileNames.Add('');
          end;
         end;
      end;
    end;

    Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
  CS: Boolean;

begin
  FHelpFiles := TStringList.Create;
  FHelpFileNames := TStringList.Create;
  FIndexFileNames := TStringList.Create;

  LoadFromRegistry;
  i := 1;
  FRow := -10;

  while i <= FHelpFileNames.Count do
  begin
    lbRegistry.Items.Add(FHelpFiles.Strings[i-1]);
    Inc(i);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.btSaveAndExitClick(Sender: TObject);
begin
  SaveToRegistry;
  Application.Terminate;
end;

//------------------------------------------------------------------------------

procedure TMainForm.btAddHelpfileClick(Sender: TObject);
var
  Key: string;
  i: Integer;
  FileName: String;

begin
(*  if OpenDialog.Execute then
  begin
    for i := 0 to OpenDialog.Files.Count - 1 do
    begin
      FileName := OpenDialog.Files.Strings[i];
      Key := ExtractFileName(FileName);
      if UpperCase(ExtractFileExt(Key)) = '.CHM' then
        Delete(Key,Length(Key)-3,4);

      vlRegistry.RowCount := vlRegistry.RowCount + 1;
      vlRegistry.Cells[0,vlRegistry.RowCount-1] := Key;
      vlRegistry.Cells[1,vlRegistry.RowCount-1] := FileName;

      if FileExists(ExtractFilePath(FileName)+'\'+Key+'.chi') then
      begin
        Key := Key + '.chi';
        Key := ExtractFilePath(FileName)+Key;
      end
      else
        Key := FileName;

      FIndexFileNames.Add(Key);
    end;
  end;  *)
end;

//------------------------------------------------------------------------------

procedure TMainForm.vlRegistrySelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
end;

//------------------------------------------------------------------------------

procedure TMainForm.lbRegistryEnter(Sender: TObject);
begin
  if lbRegistry.ItemIndex >= 0 then
  begin
    edPath.Text := FHelpFileNames.Strings[lbRegistry.ItemIndex];
    edIndexFile.Text := FIndexFileNames.Strings[lbRegistry.ItemIndex];
    cbIndexFile.Checked := not(edPath.Text = edIndexFile.Text);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.Delete1Click(Sender: TObject);
begin
  if lbRegistry.ItemIndex >= 0 then
  begin
    FIndexFileNames.Delete(lbRegistry.ItemIndex);
    FHelpFileNames.Delete(lbRegistry.ItemIndex);
    FHelpFiles.Delete(lbRegistry.ItemIndex);
    lbRegistry.Items.Delete(lbRegistry.ItemIndex);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.sbAddHelpfileClick(Sender: TObject);
var
  Key: string;
  i,t: Integer;
  FileName: String;
  KeyO: string;


begin
  if OpenDialog.Execute then
  begin
    for i := 0 to OpenDialog.Files.Count - 1 do
    begin
      FileName := OpenDialog.Files.Strings[i];
      Key := ExtractFileName(FileName);

      if UpperCase(ExtractFileExt(Key)) = '.CHM' then
        Delete(Key,Length(Key)-3,4);

      KeyO := Key;

      if FHelpFiles.IndexOf(KeyO) > -1 then
      begin
        t := 1;
        KeyO := Key + '['+inttostr(t)+']';

        while FHelpFiles.IndexOf(KeyO) > -1 do
        begin
          inc(t);
          KeyO := Key + '['+inttostr(t)+']';
        end;
      end;

      FHelpfiles.Add(KeyO);
      lbRegistry.Items.Add(KeyO);
      FHelpFileNames.Add(FileName);

      if FileExists(ExtractFilePath(FileName)+'\'+Key+'.chi') then
      begin
        Key := Key + '.chi';
        Key := ExtractFilePath(FileName)+Key;
      end
      else
        Key := FileName;

      FIndexFileNames.Add(Key);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.cbIndexFileClick(Sender: TObject);
begin
  if lbRegistry.ItemIndex >= 0 then
  begin
    edIndexFile.Enabled := cbIndexFile.Checked;
    if not (cbIndexFile.Checked) then
    begin
      FIndexFileNames.Strings[lbRegistry.ItemIndex] :=
        FHelpFileNames.Strings[lbRegistry.ItemIndex];

    end;
    edIndexFile.Text := FIndexFileNames.Strings[lbRegistry.ItemIndex];
    end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.edPathExit(Sender: TObject);
begin
  if lbRegistry.ItemIndex >= 0 then
  begin
    FHelpFileNames.Strings[lbRegistry.ItemIndex] := edPath.Text;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.btSaveExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

//------------------------------------------------------------------------------

procedure TMainForm.edIndexFileExit(Sender: TObject);
begin
  if lbRegistry.ItemIndex >= 0 then
  begin
    FIndexFileNames.Strings[lbRegistry.ItemIndex] := edindexFile.Text;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
 SaveToRegistry;
end;

//------------------------------------------------------------------------------

procedure TMainForm.btBrowseFileClick(Sender: TObject);
begin
  if lbRegistry.ItemIndex >= 0 then
    if OpenDialog.Execute then
    begin
      edPath.Text := OpenDialog.FileName;
    end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.btBrowseIndexClick(Sender: TObject);
begin
  if (lbRegistry.ItemIndex >= 0) and (edIndexFile.Enabled)then
    if OpenDialog.Execute then
    begin
      edIndexFile.Text := OpenDialog.FileName;
    end;
end;

end.
