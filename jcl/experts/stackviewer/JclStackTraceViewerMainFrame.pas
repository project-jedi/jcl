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
{ The Original Code is JclStackTraceViewerMainFrame.pas.                                           }
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

unit JclStackTraceViewerMainFrame;

{$I jcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Docktoolform, StdCtrls, ComCtrls, Menus,
  ActnList, ToolWin, ExtCtrls, IniFiles, ToolsAPI,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclDebug, JclDebugSerialization, Contnrs, JclStackTraceViewerStackFrame, JclStackTraceViewerModuleFrame,
  JclStackTraceViewerClasses, StackCodeUtils, JclStackTraceViewerExceptInfoFrame, JclStackTraceViewerThreadFrame,
  JclStackTraceViewerOptions,
  JclStackTraceViewerAPIImpl, JclOtaUtils
  , JclStrings, JclDebugXMLDeserializer, JclStackTraceViewerStackUtils
  ;

type
  TfrmMain = class(TFrame)
    ActionList1: TActionList;
    acJumpToCodeLine: TAction;
    acLoadStack: TAction;
    OpenDialog1: TOpenDialog;
    tv: TTreeView;
    acOptions: TAction;
    acUpdateLocalInfo: TAction;
    Splitter2: TSplitter;
    StatusBar: TStatusBar;
    PB: TProgressBar;
    procedure acJumpToCodeLineExecute(Sender: TObject);
    procedure acLoadStackExecute(Sender: TObject);
    procedure tvChange(Sender: TObject; Node: TTreeNode);
    procedure acOptionsExecute(Sender: TObject);
    procedure acUpdateLocalInfoExecute(Sender: TObject);
  private
    { Private declarations }
    FTreeViewLinkList: TObjectList;
    FThreadInfoList: TJclStackTraceViewerThreadInfoList;
    FExceptionInfo: TJclStackTraceViewerExceptionInfo;
    FStackFrame: TfrmStack;
    FModuleFrame: TfrmModule;
    FExceptionFrame: TfrmException;
    FThreadFrame: TfrmThread;
    FLastControl: TControl;
    FOptions: TExceptionViewerOption;
    FRootDir: string;
    procedure DoProgress(APos, AMax: Integer; const AText: string);
    procedure PrepareStack(AStack: TJclStackTraceViewerLocationInfoList; AForce: Boolean = False);
    procedure SetOptions(const Value: TExceptionViewerOption);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadWindowState(ADesktop: TCustomIniFile);
    procedure SaveWindowState(ADesktop: TCustomIniFile; AIsProject: Boolean);
    property Options: TExceptionViewerOption read FOptions write SetOptions;
    property RootDir: string read FRootDir write FRootDir;
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

uses
  JclOtaConsts, JclOtaResources,
  JclStackTraceViewerImpl;

{$R *.dfm}

type
  TTreeViewLinkKind = (tvlkException, tvlkModuleList, tvlkThread, tvlkThreadStack, tvlkThreadCreationStack);

  TTreeViewLink = class(TObject)
  private
    FData: TObject;
    FKind: TTreeViewLinkKind;
  public
    property Data: TObject read FData write FData;
    property Kind: TTreeViewLinkKind read FKind write FKind;
  end;

procedure TfrmMain.LoadWindowState(ADesktop: TCustomIniFile);
begin
  if Assigned(ADesktop) then
  begin
    FStackFrame.LoadState(ADesktop, JclStackTraceViewerDesktopIniSection, 'StackFrameSingle');
    FModuleFrame.LoadState(ADesktop, JclStackTraceViewerDesktopIniSection);
    FThreadFrame.LoadState(ADesktop, JclStackTraceViewerDesktopIniSection);
  end;
end;

procedure TfrmMain.PrepareStack(AStack: TJclStackTraceViewerLocationInfoList; AForce: Boolean = False);
var
  LocationInfoProcessor: TJclLocationInfoProcessor;
begin
  LocationInfoProcessor := TJclLocationInfoProcessor.Create;
  try
    LocationInfoProcessor.ModuleList := FExceptionInfo.Modules;
    LocationInfoProcessor.OnProgress := DoProgress;
    LocationInfoProcessor.Options := FOptions;
    LocationInfoProcessor.RootDir := RootDir;
    PB.Max := 100;
    PB.Position := 0;
    PB.Visible := True;
    try
      LocationInfoProcessor.PrepareLocationInfoList(AStack, AForce);
    finally
      PB.Visible := False;
    end;
  finally
    LocationInfoProcessor.Free;
  end;
end;

procedure TfrmMain.SaveWindowState(ADesktop: TCustomIniFile; AIsProject: Boolean);
begin
  if Assigned(ADesktop) then
  begin
    FStackFrame.SaveState(ADesktop, JclStackTraceViewerDesktopIniSection, 'StackFrameSingle');
    FModuleFrame.SaveState(ADesktop, JclStackTraceViewerDesktopIniSection);
    FThreadFrame.SaveState(ADesktop, JclStackTraceViewerDesktopIniSection);
  end;
end;

procedure TfrmMain.SetOptions(const Value: TExceptionViewerOption);
var
  OldOptions: TExceptionViewerOption;
begin
  OldOptions := TExceptionViewerOption.Create;
  try
    OldOptions.Assign(FOptions);
    FOptions.Assign(Value);
    if FOptions.ModuleVersionAsRevision <> OldOptions.ModuleVersionAsRevision then
    begin
      { TODO -oUSc : Update stack views }
    end;
  finally
    OldOptions.Free;
  end;
end;

procedure TfrmMain.tvChange(Sender: TObject; Node: TTreeNode);
var
  TreeViewLink: TTreeViewLink;
  NewControl: TControl;
  ThreadInfo: TJclStackTraceViewerThreadInfo;
  ForceStackUpdate: Boolean;
begin
  inherited;
  NewControl := nil;
  if Assigned(tv.Selected) and Assigned(tv.Selected.Data) and
    (TObject(tv.Selected.Data) is TTreeViewLink) then
  begin
    ForceStackUpdate := Sender = acUpdateLocalInfo;
    TreeViewLink := TTreeViewLink(tv.Selected.Data);
    if (TreeViewLink.Kind = tvlkModuleList) and (TreeViewLink.Data is TModuleList) then
    begin
      NewControl := FModuleFrame;
      FModuleFrame.ModuleList := TModuleList(TreeViewLink.Data);
    end
    else
    if (TreeViewLink.Kind = tvlkThread) and (TreeViewLink.Data is TJclStackTraceViewerThreadInfo) then
    begin
      ThreadInfo := TJclStackTraceViewerThreadInfo(TreeViewLink.Data);
      NewControl := FThreadFrame;
      PrepareStack(ThreadInfo.CreationStack, ForceStackUpdate);
      if tioCreationStack in ThreadInfo.Values then
        FThreadFrame.CreationStackList := ThreadInfo.CreationStack
      else
        FThreadFrame.CreationStackList := nil;
      if TreeViewLink.Data = FThreadInfoList[0] then
        FThreadFrame.Exception := FExceptionInfo.Exception
      else
        FThreadFrame.Exception := nil;
      PrepareStack(ThreadInfo.Stack, ForceStackUpdate);
      if tioStack in ThreadInfo.Values then
        FThreadFrame.StackList := ThreadInfo.Stack
      else
        FThreadFrame.StackList := nil;
    end
    else
    if (TreeViewLink.Kind = tvlkException) and (TreeViewLink.Data is TException) then
    begin
      NewControl := FExceptionFrame;
      FExceptionFrame.Exception := TException(TreeViewLink.Data);
    end
    else
    if (TreeViewLink.Kind in [tvlkThreadStack, tvlkThreadCreationStack]) and (TreeViewLink.Data is TJclStackTraceViewerLocationInfoList) then
    begin
      PrepareStack(TJclStackTraceViewerLocationInfoList(TreeViewLink.Data), ForceStackUpdate);
      FStackFrame.StackList := TJclStackTraceViewerLocationInfoList(TreeViewLink.Data);
      NewControl := FStackFrame;
    end;
  end;
  if Assigned(NewControl) then
    NewControl.Show;
  if Assigned(FLastControl) and (FLastControl <> NewControl) then
    FLastControl.Hide;
  if FLastControl <> NewControl then
    FLastControl := NewControl;
end;

procedure TfrmMain.acJumpToCodeLineExecute(Sender: TObject);
begin
  if Assigned(FThreadFrame) and FThreadFrame.Visible and Assigned(FThreadFrame.Selected) then
    JumpToCode(FThreadFrame.Selected)
  else
  if Assigned(FStackFrame) and FStackFrame.Visible and Assigned(FStackFrame.Selected) then
    JumpToCode(FStackFrame.Selected);
end;

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExceptionInfo := TJclStackTraceViewerExceptionInfo.Create;
  FThreadInfoList := FExceptionInfo.ThreadInfoList;
  FTreeViewLinkList := TObjectList.Create;
  FStackFrame := TfrmStack.Create(Self);
  FStackFrame.Name := 'StackFrameSingle';
  FStackFrame.Parent := Self;
  FStackFrame.Align := alClient;
  FStackFrame.Visible := False;

  FModuleFrame := TfrmModule.Create(Self);
  FModuleFrame.Parent := Self;
  FModuleFrame.Align := alClient;
  FModuleFrame.Visible := False;

  FExceptionFrame := TfrmException.Create(Self);
  FExceptionFrame.Name := 'ExceptionFrameSingle';
  FExceptionFrame.Parent := Self;
  FExceptionFrame.Align := alClient;
  FExceptionFrame.Visible := False;

  FThreadFrame := TfrmThread.Create(Self);
  FThreadFrame.Parent := Self;
  FThreadFrame.Align := alClient;
  FThreadFrame.Visible := False;

  PB.Parent := StatusBar;
  PB.SetBounds(2, 3, 96, 14);

  FOptions := TExceptionViewerOption.Create;
  if Assigned(StackTraceViewerExpert) then
  begin
    Options := StackTraceViewerExpert.Options;
    RootDir := StackTraceViewerExpert.RootDir;
  end;

  FLastControl := nil;
end;

destructor TfrmMain.Destroy;
begin
  FOptions.Free;
  FTreeViewLinkList.Free;
  FExceptionInfo.Free;
  inherited Destroy;
end;

procedure TfrmMain.acLoadStackExecute(Sender: TObject);
var
  SS: TStringStream;
  {$IFNDEF COMPILER12_UP}
  FS: TFileStream;
  {$ENDIF ~COMPILER12_UP}
  I: Integer;
  S: string;
  tn, tns: TTreeNode;
  TreeViewLink: TTreeViewLink;
  XMLDeserializer: TJclXMLDeserializer;
  SerializeExceptionInfo: TExceptionInfo;
begin
  inherited;
  if OpenDialog1.Execute then
  begin
    FStackFrame.StackList := nil;
    tv.Selected := nil;
    tv.Items.Clear;
    FTreeViewLinkList.Clear;
    SS := TStringStream.Create('');
    try
      {$IFDEF COMPILER12_UP}
      SS.LoadFromFile(OpenDialog1.FileName);
      {$ELSE ~COMPILER12_UP}
      FS := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
      try
        SS.CopyFrom(FS, 0);
      finally
        FS.Free;
      end;
      {$ENDIF ~COMPILER12_UP}
      SerializeExceptionInfo := TExceptionInfo.Create;
      try
        XMLDeserializer := TJclXMLDeserializer.Create('ExceptInfo');
        try
          XMLDeserializer.LoadFromString(SS.DataString);
          SerializeExceptionInfo.Deserialize(XMLDeserializer);
        finally
          XMLDeserializer.Free;
        end;
        FExceptionInfo.AssignExceptionInfo(SerializeExceptionInfo);
      finally
        SerializeExceptionInfo.Free;
      end;

      FTreeViewLinkList.Add(TTreeViewLink.Create);
      TreeViewLink := TTreeViewLink(FTreeViewLinkList.Last);
      TreeViewLink.Kind := tvlkModuleList;
      TreeViewLink.Data := FExceptionInfo.Modules;
      tn := tv.Items.Add(nil, Format('Module List [%d]', [FExceptionInfo.Modules.Count]));
      tn.Data := TreeViewLink;

      if FThreadInfoList.Count > 0 then
      begin
        for I := 0 to FThreadInfoList.Count - 1 do
        begin
          if tioIsMainThread in FThreadInfoList[I].Values then
            S := '[MainThread]'
          else
            S := '';
          S := Format('ID: %d %s', [FThreadInfoList[I].ThreadID, S]);

          FTreeViewLinkList.Add(TTreeViewLink.Create);
          TreeViewLink := TTreeViewLink(FTreeViewLinkList.Last);
          TreeViewLink.Kind := tvlkThread;
          TreeViewLink.Data := FThreadInfoList[I];
          tn := tv.Items.Add(nil, S);
          tn.Data := TreeViewLink;

          if I = 0 then
          begin
            FTreeViewLinkList.Add(TTreeViewLink.Create);
            TreeViewLink := TTreeViewLink(FTreeViewLinkList.Last);
            TreeViewLink.Kind := tvlkException;
            TreeViewLink.Data := FExceptionInfo.Exception;
            tns := tv.Items.AddChild(tn, 'Exception');
            tns.Data := TreeViewLink;
          end;

          if tioStack in FThreadInfoList[I].Values then
          begin
            FTreeViewLinkList.Add(TTreeViewLink.Create);
            TreeViewLink := TTreeViewLink(FTreeViewLinkList.Last);
            TreeViewLink.Kind := tvlkThreadStack;
            TreeViewLink.Data := FThreadInfoList[I].Stack;
            tns := tv.Items.AddChild(tn, Format('Stack [%d]', [FThreadInfoList[I].Stack.Count]));
            tns.Data := TreeViewLink;
          end;

          if tioCreationStack  in FThreadInfoList[I].Values then
          begin
            FTreeViewLinkList.Add(TTreeViewLink.Create);
            TreeViewLink := TTreeViewLink(FTreeViewLinkList.Last);
            TreeViewLink.Kind := tvlkThreadCreationStack;
            TreeViewLink.Data := FThreadInfoList[I].CreationStack;
            tns := tv.Items.AddChild(tn, Format('CreationStack [%d]', [FThreadInfoList[I].CreationStack.Count]));
            tns.Data := TreeViewLink;
          end;
          if FOptions.ExpandTreeView then
            tn.Expanded := True;
        end;
      end;
    finally
      SS.Free;
    end;
  end;
end;

procedure TfrmMain.acOptionsExecute(Sender: TObject);
begin
  inherited;
  TJclOTAExpertBase.ConfigurationDialog(rsStackTraceViewerOptionsPageName);
end;

procedure TfrmMain.acUpdateLocalInfoExecute(Sender: TObject);
begin
  inherited;
  tvChange(Sender, nil);
end;

procedure TfrmMain.DoProgress(APos, AMax: Integer; const AText: string);
begin
  PB.Max := AMax;
  PB.Position := APos;
  StatusBar.Panels[1].Text := AText;
  StatusBar.Update;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
