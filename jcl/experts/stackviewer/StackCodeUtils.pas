unit StackCodeUtils;

{$I jcl.inc}

interface

uses
  SysUtils,
  {$IFNDEF BDS}
  Classes,
  {$ENDIF !BDS}
  ActiveX, ToolsAPI,
  JclOtaUtils,
  JclStackTraceViewerClasses;

function FindModule(const AFileName: string): string;
function FindModuleAndProject(const AFileName: string; var AProjectName: string): string;
function GetFileEditorContent(const AFileName: string): IStream;
procedure JumpToCode(AStackViewItem: TJclStackTraceViewerLocationInfo);

implementation

function FindModuleInfoInProject(AProject: IOTAProject; const AFileName: string): IOTAModuleInfo;
var
  I, P: Integer;
  ModuleInfo: IOTAModuleInfo;
  S, S2: string;
begin
  Result := nil;
  if AProject.GetModuleCount > 0 then
  begin
    S := UpperCase(AFileName);
    for I := 0 to Pred(AProject.GetModuleCount) do
    begin
      ModuleInfo := AProject.GetModule(I);
      if Assigned(ModuleInfo) then
      begin
        S2 := UpperCase(ModuleInfo.FileName);
        P := Pos(S, S2);
        if (P > 0) and (P = Length(S2) - Length(S) + 1) then
        begin
          Result := ModuleInfo;
          Break;
        end;
      end;
    end;
  end;
end;

function FindModule(const AFileName: string): string;
var
  Dummy: string;
begin
  Result := FindModuleAndProject(AFilename, Dummy);
end;

function FindModuleInfoAndProject(const AFileName: string; var AProjectName: string): IOTAModuleInfo;
var
  I: Integer;
  ProjectGroup: IOTAProjectGroup;
begin
  Result := nil;
  AProjectName := '';
  {$IFDEF BDS}
  ProjectGroup := (BorlandIDEServices as IOTAModuleServices).MainProjectGroup;
  {$ELSE !BDS}
  ProjectGroup := TJclOTAExpertBase.GetProjectGroup;
  {$ENDIF !BDS}
  if Assigned(ProjectGroup) then
    for I := 0 to ProjectGroup.ProjectCount - 1 do
    begin
      Result := FindModuleInfoInProject(ProjectGroup.Projects[I], AFileName);
      if Assigned(Result) then
      begin
        AProjectName := ProjectGroup.Projects[I].FileName;
        Break;
      end;
    end;
end;

function FindModuleAndProject(const AFileName: string; var AProjectName: string): string;
var
  ModuleInfo: IOTAModuleInfo;
begin
  ModuleInfo := FindModuleInfoAndProject(AFileName, AProjectName);
  if Assigned(ModuleInfo) then
    Result := ModuleInfo.FileName
  else
    Result := '';
end;

function GetFileEditorContent(const AFileName: string): IStream;
var
  I: Integer;
  Module: IOTAModule;
  {$IFDEF BDS}
  EditorContent: IOTAEditorContent;
  {$ELSE !BDS}
  ContentPos, ReadCount, BufferSize: Integer;
  Buffer: Pointer;
  ModuleSourceEditor: IOTASourceEditor;
  ModuleReader: IOTAEditReader;
  S: TStream;
  SA: TStreamAdapter;
  {$ENDIF !BDS}
begin
  Result := nil;
  Module := (BorlandIDEServices as IOTAModuleServices).FindModule(AFileName);
  if Assigned(Module) then
  begin
    {$IFDEF BDS}
    for I := 0 to Module.ModuleFileCount - 1 do
      if Supports(Module.ModuleFileEditors[I], IOTAEditorContent, EditorContent) then
      begin
        Result := EditorContent.Content;
        Break;
      end;
    {$ELSE !BDS}
    for I := 0 to Module.GetModuleFileCount - 1 do
      if Supports(Module.GetModuleFileEditor(I), IOTASourceEditor, ModuleSourceEditor) then
      begin
        ModuleReader := ModuleSourceEditor.CreateReader;
        if Assigned(ModuleReader) then
        begin
          ContentPos := 0;
          BufferSize := 4096;
          SA := TStreamAdapter.Create(TMemoryStream.Create, soOwned);
          S := SA.Stream;
          Result := SA;
          GetMem(Buffer, BufferSize);
          try
            ReadCount := BufferSize;
            while ReadCount = BufferSize do
            begin
              ReadCount := ModuleReader.GetText(ContentPos, Buffer, ReadCount);
              if ReadCount > 0 then
              begin
                Inc(ContentPos, BufferSize);
                S.Write(Buffer^, ReadCount);
              end;
            end;
          finally
            FreeMem(Buffer);
          end;
        end;
        Break;
      end;
    {$ENDIF !BDS}
  end;
end;

procedure JumpToCode(AStackViewItem: TJclStackTraceViewerLocationInfo);
var
  S, FileName, ProjectName: string;
  Module: IOTAModule;
  ModuleInfo: IOTAModuleInfo;  

  SourceEditor: IOTASourceEditor;
  I, LineNumber: Integer;
  EditPos: TOTAEditPos;
begin
  if Assigned(AStackViewItem) then
  begin
    FileName := AStackViewItem.SourceName;
    ModuleInfo := FindModuleInfoAndProject(FileName, ProjectName);
    if Assigned(ModuleInfo) then
    begin
      S := ModuleInfo.FileName;
      if (S <> '') and Assigned(BorlandIDEServices) then
      begin
        {$IFDEF BDS}
        Module := (BorlandIDEServices as IOTAModuleServices).OpenModule(S);
        {$ELSE !BDS}
        Module := ModuleInfo.OpenModule;
        {$ENDIF !BDS}
      end;
    end
    else
    if AStackViewItem.FoundFile then
    begin
      {$IFDEF BDS}
      Module := (BorlandIDEServices as IOTAModuleServices).OpenModule(AStackViewItem.FileName);
      {$ELSE !BDS}
      (BorlandIDEServices as IOTAActionServices).OpenFile(AStackViewItem.FileName);
      Module := (BorlandIDEServices as IOTAModuleServices).FindModule(AStackViewItem.FileName);
      {$ENDIF !BDS}
    end;
    if Assigned(Module) then
    begin
      {$IFDEF BDS}
      Module.Show;
      {$ENDIF BDS}
      for I := 0 to Module.GetModuleFileCount - 1 do
        if Supports(Module.GetModuleFileEditor(I), IOTASourceEditor, SourceEditor) then
        begin
          SourceEditor.Show;
          if SourceEditor.EditViewCount > 0 then
          begin
            if AStackViewItem.TranslatedLineNumber > 0 then
              LineNumber := AStackViewItem.TranslatedLineNumber
            else
              LineNumber := AStackViewItem.LineNumber;
            if LineNumber > 0 then
            begin
              SourceEditor.EditViews[0].Center(LineNumber, 1);
              EditPos.Line := LineNumber;
              EditPos.Col := 1;
              SourceEditor.EditViews[0].CursorPos := EditPos;
            end;
          end;
          Break;
        end;
    end;
  end;
end;

end.
