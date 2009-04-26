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
  {PlatformDefaultStyleActnCtrls,} ActnPopup, ActnList, ToolWin, ExtCtrls, IniFiles, ToolsAPI,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclDebug, JclDebugSerialization, Contnrs, JclStackTraceViewerStackFrame, JclStackTraceViewerModuleFrame,
  StackViewUnit, StackFrame2, StackCodeUtils, JclStackTraceViewerExceptInfoFrame, JclStackTraceViewerThreadFrame,
  JclStackTraceViewerOptions,
  StackLineNumberTranslator, JclOtaUtils
  , ActiveX
  , FileSearcherUnit, JclStrings, JclDebugXMLDeserializer
  ;

type
  TfrmMain = class(TFrame)
    ActionList1: TActionList;
    acJumpToCodeLine: TAction;
    acLoadStack: TAction;
    OpenDialog1: TOpenDialog;
    cboxThread: TComboBox;
    tv: TTreeView;
    acOptions: TAction;
    acUpdateLocalInfo: TAction;
    Splitter2: TSplitter;
    procedure acJumpToCodeLineExecute(Sender: TObject);
    procedure acLoadStackExecute(Sender: TObject);
    procedure cboxThreadChange(Sender: TObject);
    procedure tvChange(Sender: TObject; Node: TTreeNode);
    procedure acOptionsExecute(Sender: TObject);
    procedure acUpdateLocalInfoExecute(Sender: TObject);
  private
    { Private declarations }
    FStackItemList: TStackViewItemsList;
    FCreationStackItemList: TStackViewItemsList;
    FTreeViewLinkList: TObjectList;
    FThreadInfoList: TJclSerializableThreadInfoList;
    FExceptionInfo: TExceptionInfo;
    FStackFrame: TfrmStack;
    FModuleFrame: TfrmModule;
    FExceptionFrame: TfrmException;
    FThreadFrame: TfrmThread;
    FLastControl: TControl;
    FOptions: TExceptionViewerOption;
    FRootDir: string;
    procedure PrepareStack(AStack: TJclSerializableLocationInfoList; AStackItemList: TStackViewItemsList);
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

const
  IDEDesktopIniSection = 'TStackViewAddIn';//todo - move

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
    FStackFrame.LoadState(ADesktop, IDEDesktopIniSection, 'StackFrameSingle');
    FModuleFrame.LoadState(ADesktop, IDEDesktopIniSection);
    FThreadFrame.LoadState(ADesktop, IDEDesktopIniSection);
  end;
end;

type
  TFindMapping = class(TObject)
  private
    FItems: TList;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TStackViewItem;
  public
    FoundFile: Boolean;
    FileName: string;
    ProjectName: string;
    constructor Create;
    destructor Destroy; override;
    procedure Add(AStackViewItem: TStackViewItem);
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TStackViewItem read GetItems; default;
  end;

constructor TFindMapping.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

destructor TFindMapping.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TFindMapping.Add(AStackViewItem: TStackViewItem);
begin
  FItems.Add(AStackViewItem);
end;

function TFindMapping.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TFindMapping.GetItems(AIndex: Integer): TStackViewItem;
begin
  Result := FItems[AIndex];
end;

procedure TfrmMain.PrepareStack(AStack: TJclSerializableLocationInfoList; AStackItemList: TStackViewItemsList);
var
  I, J, K, Idx, NewLineNumber: Integer;
  StackViewItem: TStackViewItem;
  FindFileList: TStringList;
  FindMapping: TFindMapping;
  FileName, ProjectName: string;
  RevisionMS: TMemoryStream;
  RevisionStream, Stream: IStream;
  FS: TFileStream;

  S: string;
  EV: IOTAEnvironmentOptions;
  FileSearcher: TFileSearcher;
  BrowsingPaths: TStringList;

  Found: Boolean;
  RevisionLineNumbers, CurrentLineNumbers: TList;
begin
  AStackItemList.Clear;
  if AStack.Count > 0 then
  begin
    FindFileList := TStringList.Create;
    try
      FindFileList.Sorted := True;
      //check if the files can be found in a project in the current project group
      for I := 0 to AStack.Count - 1 do
      begin
        StackViewItem := AStackItemList.Add;
        StackViewItem.Assign(AStack[I]);
        StackViewItem.Revision := AStack[I].UnitVersionRevision;
        Idx := FindFileList.IndexOf(AStack[I].SourceName);
        if Idx <> -1 then
        begin
          FindMapping := TFindMapping(FindFileList.Objects[Idx]);
          FindMapping.Add(StackViewItem);
          StackViewItem.FoundFile := FindMapping.FoundFile;
          StackViewItem.FileName := FindMapping.FileName;
          StackViewItem.ProjectName := FindMapping.ProjectName;
        end
        else
        begin
          if AStack[I].SourceName <> '' then
            FileName := FindModuleAndProject(AStack[I].SourceName, ProjectName)
          else
          begin
            FileName := '';
            ProjectName := '';
          end;
          FindMapping := TFindMapping.Create;
          FindMapping.Add(StackViewItem);
          FindFileList.AddObject(AStack[I].SourceName, FindMapping);
          FindMapping.FoundFile := FileName <> '';
          FindMapping.FileName := FileName;
          FindMapping.ProjectName := ProjectName;

          StackViewItem.FoundFile := FileName <> '';
          StackViewItem.FileName := FileName;
          StackViewItem.ProjectName := ProjectName;
        end;
      end;

      //use the build number from the version number as revision number if the revision number is empty
      if FOptions.ModuleVersionAsRevision then
      begin
        for I := 0 to FindFileList.Count - 1 do
        begin
          FindMapping := TFindMapping(FindFileList.Objects[I]);
          if (FindMapping.Count > 0) and (FindMapping[0].Revision = '') and (FindMapping[0].ModuleName <> '') then
          begin
            Idx := -1;
            { TODO -oUSc : Compare full filename when the filename in the stack contains also the path

  Why full filenames?

  It is possible to load
  <Path 1>\TestDLL.DLL
  <Path 2>\TestDLL.DLL}
            for J := 0 to FExceptionInfo.Modules.Count - 1 do
              if CompareText(ExtractFileName(FExceptionInfo.Modules[J].ModuleName), ExtractFileName(FindMapping[0].ModuleName)) = 0 then
              begin
                Idx := J;
                Break;
              end;
            if Idx <> -1 then
            begin
              S := FExceptionInfo.Modules[Idx].BinFileVersion;
              K := Pos('.', S);
              if K > 0 then
                Delete(S, 1, K);
              K := Pos('.', S);
              if K > 0 then
                Delete(S, 1, K);
              K := Pos('.', S);
              if K > 0 then
              begin
                Delete(S, 1, K);
                for J := 0 to FindMapping.Count - 1 do
                  FindMapping[J].Revision := S;
              end;
            end;
          end;
        end;
      end;

      //check if the other files can be found in BrowsingPath
      Found := False;
      for I := 0 to FindFileList.Count - 1 do
      begin
        FindMapping := TFindMapping(FindFileList.Objects[I]);
        if (FindFileList[I] <> '') and (not FindMapping.FoundFile) then
        begin
          Found := True;
          Break;
        end;
      end;
      if Found then
      begin
        FileSearcher := TFileSearcher.Create;
        try
          BrowsingPaths := TStringList.Create;
          try
            EV := (BorlandIDEServices as IOTAServices).GetEnvironmentOptions;
            StrTokenToStrings(EV.Values['BrowsingPath'], ';', BrowsingPaths);
            for I := 0 to BrowsingPaths.Count - 1 do
            begin
              S := BrowsingPaths[I];
              if Pos('$(BDS)', S) > 0 then
                S := StringReplace(S, '$(BDS)', RootDir, []);
              FileSearcher.SearchPaths.Add(S);
            end;
          finally
            BrowsingPaths.Free;
          end;
          if FileSearcher.SearchPaths.Count > 0 then
          begin
            for I := 0 to FindFileList.Count - 1 do
            begin
              FindMapping := TFindMapping(FindFileList.Objects[I]);
              if (FindFileList[I] <> '') and (not FindMapping.FoundFile) and (FileSearcher.IndexOf(FindFileList[I]) = -1) then
                FileSearcher.Add(FindFileList[I]);
            end;
            if FileSearcher.Count > 0 then
            begin
              FileSearcher.Search;
              for I := 0 to FindFileList.Count - 1 do
              begin
                FindMapping := TFindMapping(FindFileList.Objects[I]);
                if not FindMapping.FoundFile then
                begin
                  Idx := FileSearcher.IndexOf(FindFileList[I]);
                  if (Idx <> -1) and (FileSearcher[Idx].Results.Count > 0) then
                  begin
                    FindMapping.FoundFile := True;
                    FindMapping.FileName := FileSearcher[Idx].Results[0];
                    FindMapping.ProjectName := '';
                    for J := 0 to FindMapping.Count - 1 do
                    begin
                      FindMapping[J].FoundFile := FindMapping.FoundFile;
                      FindMapping[J].FileName := FindMapping.FileName;
                      FindMapping[J].ProjectName := FindMapping.ProjectName;
                    end;
                  end;
                end;
              end;
            end;
          end;
        finally
          FileSearcher.Free;
        end;
      end;
      for I := 0 to FindFileList.Count - 1 do
      begin
        FindMapping := TFindMapping(FindFileList.Objects[I]);
        if (FindMapping.FoundFile) and (FindMapping.Count > 0) {and (FindMapping[0].Revision <> '')} then//todo - check revision
        begin
          Found := False;
          for J := 0 to FindMapping.Count - 1 do
            if FindMapping[J].LineNumber > 0 then
            begin
              Found := True;
              Break;
            end;
          if Found then
          begin
            Stream := GetFileEditorContent(FindMapping.FileName);
            if not Assigned(Stream) then
            begin
              if FileExists(FindMapping.FileName) then
              begin
(BorlandIDEServices as IOTAMessageServices).AddTitleMessage(Format('Using %s', [FindMapping.FileName]));//todo - remove
                FS := TFileStream.Create(FindMapping.FileName, fmOpenRead);
                Stream := TStreamAdapter.Create(FS);
              end;
            end
            else
              FS := nil;
            try
              if Assigned(Stream) and (FS = nil) then//todo - remove FS = nil
              begin
                RevisionLineNumbers := TList.Create;
                CurrentLineNumbers := TList.Create;
                try
                  for J := 0 to FindMapping.Count - 1 do
                    if FindMapping[J].LineNumber > 0 then
                      RevisionLineNumbers.Add(Pointer(FindMapping[J].LineNumber));
                  RevisionMS := TMemoryStream.Create;
                  try
                    RevisionStream := TStreamAdapter.Create(RevisionMS);
(BorlandIDEServices as IOTAMessageServices).AddTitleMessage(Format('F1 %s', [FindMapping.FileName]));//todo - remove
                    if GetRevisionContent(FindMapping.FileName, FindMapping[0].Revision, RevisionStream) then
                    begin
(BorlandIDEServices as IOTAMessageServices).AddTitleMessage(Format('F2 %s', [FindMapping.FileName]));//todo - remove
                      if TranslateLineNumbers(RevisionStream, Stream, RevisionLineNumbers, CurrentLineNumbers) > 0 then
                      begin
(BorlandIDEServices as IOTAMessageServices).AddTitleMessage(Format('F3 %s', [FindMapping.FileName]));//todo - remove
                        if RevisionLineNumbers.Count = CurrentLineNumbers.Count then
                        begin
                          for J := 0 to FindMapping.Count - 1 do
                            if FindMapping[J].LineNumber > 0 then
                            begin
                              FindMapping[J].TranslatedLineNumber := -1;
                              for K := 0 to RevisionLineNumbers.Count - 1 do
                                if Integer(RevisionLineNumbers[K]) = FindMapping[J].LineNumber then
                                begin
                                  FindMapping[J].TranslatedLineNumber := Integer(CurrentLineNumbers[K]);
                                  Break;
                                end;
                            end;
                        end;
                      end;
                    end;
                  finally
                    RevisionMS.Free;
                  end;
                finally
                  RevisionLineNumbers.Free;
                  CurrentLineNumbers.Free;
                end;
              end;
            finally
              FS.Free;
            end;
            StackViewItem.TranslatedLineNumber := NewLineNumber;
          end;
        end;
      end;
    finally
      for I := 0 to FindFileList.Count - 1 do
        FindFileList.Objects[I].Free;
      FindFileList.Free;
    end;
  end;
end;

procedure TfrmMain.SaveWindowState(ADesktop: TCustomIniFile; AIsProject: Boolean);
begin
  if Assigned(ADesktop) then
  begin
    FStackFrame.SaveState(ADesktop, IDEDesktopIniSection, 'StackFrameSingle');
    FModuleFrame.SaveState(ADesktop, IDEDesktopIniSection);
    FThreadFrame.SaveState(ADesktop, IDEDesktopIniSection);
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
  ThreadInfo: TJclSerializableThreadInfo;
begin
  inherited;
  NewControl := nil;
  if Assigned(tv.Selected) and Assigned(tv.Selected.Data) and
    (TObject(tv.Selected.Data) is TTreeViewLink) then
  begin
    TreeViewLink := TTreeViewLink(tv.Selected.Data);
    if (TreeViewLink.Kind = tvlkModuleList) and (TreeViewLink.Data is TModuleList) then
    begin
      NewControl := FModuleFrame;
      FModuleFrame.ModuleList := TModuleList(TreeViewLink.Data);
    end
    else
    if (TreeViewLink.Kind = tvlkThread) and (TreeViewLink.Data is TJclSerializableThreadInfo) then
    begin
      ThreadInfo := TJclSerializableThreadInfo(TreeViewLink.Data);
      NewControl := FThreadFrame;
      PrepareStack(ThreadInfo.CreationStack, FCreationStackItemList);
      if tioCreationStack in ThreadInfo.Values then
        FThreadFrame.CreationStackList := FCreationStackItemList
      else
        FThreadFrame.CreationStackList := nil;
      if TreeViewLink.Data = FThreadInfoList[0] then
        FThreadFrame.Exception := FExceptionInfo.Exception
      else
        FThreadFrame.Exception := nil;
      PrepareStack(ThreadInfo.Stack, FStackItemList);
      if tioStack in ThreadInfo.Values then
        FThreadFrame.StackList := FStackItemList
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
    if (TreeViewLink.Kind in [tvlkThreadStack, tvlkThreadCreationStack]) and (TreeViewLink.Data is TJclSerializableLocationInfoList) then
    begin
      PrepareStack(TJclSerializableLocationInfoList(TreeViewLink.Data), FStackItemList);
      FStackFrame.StackList := FStackItemList;
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
//  FThreadInfoList := TThreadInfoList.Create;
  FExceptionInfo := TExceptionInfo.Create;
  FThreadInfoList := FExceptionInfo.ThreadInfoList;
  FStackItemList := TStackViewItemsList.Create;
  FCreationStackItemList := TStackViewItemsList.Create;
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
  FStackItemList.Free;
  FCreationStackItemList.Free;
//  FThreadInfoList.Free;
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
begin
  inherited;
  if OpenDialog1.Execute then
  begin
    FStackFrame.StackList := nil;
    FStackItemList.Clear;
    FCreationStackItemList.Clear;
    cboxThread.Items.Clear;
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
      //FExceptionInfo.LoadFromString(SS.DataString);
      XMLDeserializer := TJclXMLDeserializer.Create('ExceptInfo');
      try
        XMLDeserializer.LoadFromString(SS.DataString);
        FExceptionInfo.Deserialize(XMLDeserializer);
      finally
        XMLDeserializer.Free;
      end;

      FTreeViewLinkList.Add(TTreeViewLink.Create);
      TreeViewLink := TTreeViewLink(FTreeViewLinkList.Last);
      TreeViewLink.Kind := tvlkModuleList;
      TreeViewLink.Data := FExceptionInfo.Modules;
      tn := tv.Items.Add(nil, Format('Module List [%d]', [FExceptionInfo.Modules.Count]));
      tn.Data := TreeViewLink;

      if FThreadInfoList.Count > 0 then
      begin
        {
        for I := 0 to FThreadInfoList.Count - 1 do
          cboxThread.Items.AddObject(Format('[%d/%d] ThreadID: %d [%d]', [I + 1, FThreadInfoList.Count,
            FThreadInfoList[I].ThreadID, FThreadInfoList[I].Stack.Count]), FThreadInfoList[I]);
        }
        for I := 0 to FThreadInfoList.Count - 1 do
        begin
          cboxThread.Items.AddObject(Format('[%d/%d] %s', [I + 1, FThreadInfoList.Count, ''{FThreadInfoList[I].AsString}]), FThreadInfoList[I]);
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

        cboxThread.ItemIndex := 0;
        cboxThreadChange(nil);
      end;
    finally
      SS.Free;
    end;
  end;
end;

procedure TfrmMain.acOptionsExecute(Sender: TObject);
begin
  inherited;
  TJclOTAExpertBase.ConfigurationDialog('Stack Trace Viewer');
end;

procedure TfrmMain.acUpdateLocalInfoExecute(Sender: TObject);
begin
  inherited;
  tvChange(nil, nil);
end;

procedure TfrmMain.cboxThreadChange(Sender: TObject);
begin
  inherited;
  {//todo
  if (cboxThread.ItemIndex <> -1) and (cboxThread.Items.Objects[cboxThread.ItemIndex] is TJclThreadInfo) then
    StackListToListBox(TJclThreadInfo(cboxThread.Items.Objects[cboxThread.ItemIndex]).Stack)
  else
  begin
    lbStack.Items.Clear;
    FStackItemList.Clear;
  end;
  }
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
