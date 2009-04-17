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
{ The Original Code is JclStackTraceViewerStackFrame.pas.                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is Uwe Schuster.                                      }
{ Portions created by Uwe Schuster are Copyright (C) 2009 Uwe Schuster. All rights reserved.       }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Uwe Schuster (uschuster)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                              $ }
{ Revision:      $Rev::                                                                      $ }
{ Author:        $Author::                                                                 $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclStackTraceViewerStackFrame;

{$I jcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, IniFiles,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclDebug, StackViewUnit, StackCodeUtils;

type
  TfrmStack = class(TFrame)
    lv: TListView;
    procedure lvDblClick(Sender: TObject);
    procedure lvChange(Sender: TObject; Item: TListItem; Change: TItemChange);
  private
    { Private declarations }
    FStackList: TStackViewItemsList;
    FOnSelectStackLine: TNotifyEvent;
    procedure DoSelectStackLine;
    procedure SetStackList(const Value: TStackViewItemsList);
    function GetSelected: TStackViewItem;
  public
    { Public declarations }
    procedure LoadState(AIni: TCustomIniFile; const ASection, APrefix: string);
    procedure SaveState(AIni: TCustomIniFile; const ASection, APrefix: string);
    property StackList: TStackViewItemsList read FStackList write SetStackList;
    property Selected: TStackViewItem read GetSelected;
    property OnSelectStackLine: TNotifyEvent read FOnSelectStackLine write FOnSelectStackLine;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: $';
    Revision: '$Revision: $';
    Date: '$Date: $';
    LogPath: ''
    );
{$ENDIF UNITVERSIONING}

implementation

{$R *.dfm}

{ TfrmStack }

procedure TfrmStack.DoSelectStackLine;
begin
  if Assigned(FOnSelectStackLine) then
    FOnSelectStackLine(Self);
end;

function TfrmStack.GetSelected: TStackViewItem;
begin
  if Assigned(lv.Selected) and Assigned(lv.Selected.Data) and (TObject(lv.Selected.Data) is TStackViewItem) then
    Result := TStackViewItem(lv.Selected.Data)
  else
    Result := nil;
end;

procedure TfrmStack.LoadState(AIni: TCustomIniFile; const ASection, APrefix: string);
var
  I: Integer;
begin
  for I := 0 to lv.Columns.Count - 1 do
    lv.Columns.Items[I].Width := AIni.ReadInteger(ASection,
      Format(APrefix + 'ColumnWidth%d', [I]), lv.Columns.Items[I].Width);
end;

procedure TfrmStack.lvChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  DoSelectStackLine;
end;

procedure TfrmStack.lvDblClick(Sender: TObject);
begin
  JumpToCode(Selected);
end;

procedure TfrmStack.SaveState(AIni: TCustomIniFile; const ASection, APrefix: string);
var
  I: Integer;
begin
  for I := 0 to lv.Columns.Count - 1 do
    AIni.WriteInteger(ASection, Format(APrefix + 'ColumnWidth%d', [I]), lv.Columns.Items[I].Width);
end;

procedure TfrmStack.SetStackList(const Value: TStackViewItemsList);
var
  I: Integer;
  ListItem: TListItem;
  S: string;
begin
  FStackList := Value;

  lv.Items.BeginUpdate;
  try
    lv.Items.Clear;
    if Assigned(FStackList) then
      for I := 0 to FStackList.Count - 1 do
      begin
        ListItem := lv.Items.Add;
        ListItem.Caption := FStackList[I].ModuleName;
        ListItem.SubItems.Add(FStackList[I].SourceUnitName);
        ListItem.SubItems.Add(FStackList[I].ProcedureName);
        ListItem.SubItems.Add(FStackList[I].SourceName);
        if FStackList[I].LineNumber > 0 then
          S := IntToStr(FStackList[I].LineNumber)
        else
          S := '';
        ListItem.SubItems.Add(S);
        if lievProcedureStartLocationInfo in FStackList[I].Values then
          S := IntToStr(FStackList[I].LineNumberOffsetFromProcedureStart)
        else
          S := '';
        ListItem.SubItems.Add(S);
        ListItem.SubItems.Add(FStackList[I].Revision);
        if FStackList[I].ProjectName <> '' then
          S := ExtractFileName(FStackList[I].ProjectName)
        else
          S := ExtractFileName(FStackList[I].FileName);
        ListItem.SubItems.Add(S);
        if FStackList[I].TranslatedLineNumber > 0 then
          S := IntToStr(FStackList[I].TranslatedLineNumber)
        else
          S := '';
        ListItem.SubItems.Add(S);
        ListItem.Data := FStackList[I];
      end;
  finally
    lv.Items.EndUpdate;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
