unit StackCodeUtils;

interface

uses
  SysUtils, ToolsAPI, StackViewUnit;

function FindModule(const AFileName: string): string;
function FindModuleAndProject(const AFileName: string; var AProjectName: string): string;
procedure JumpToCode(AStackViewItem: TStackViewItem);

implementation

function FindModuleInProject(AProject: IOTAProject; const AFileName: string): string;
var
  I, P: Integer;
  ModuleInfo: IOTAModuleInfo;
  S, S2: string;
begin
  Result := '';
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
          Result := ModuleInfo.FileName;
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

function FindModuleAndProject(const AFileName: string; var AProjectName: string): string;
var
  I: Integer;
  ProjectGroup: IOTAProjectGroup;
begin
  Result := '';
  AProjectName := '';
  ProjectGroup := (BorlandIDEServices as IOTAModuleServices).MainProjectGroup;
  if Assigned(ProjectGroup) then
    for I := 0 to ProjectGroup.ProjectCount - 1 do
    begin
      Result := FindModuleInProject(ProjectGroup.Projects[I], AFileName);
      if Result <> '' then
      begin
        AProjectName := ProjectGroup.Projects[I].FileName;
        Break;
      end;
    end;
end;

procedure JumpToCode(AStackViewItem: TStackViewItem);
var
  S, FileName: string;
  Module: IOTAModule;

  SourceEditor: IOTASourceEditor;
  I, LineNumber: Integer;
  EditPos: TOTAEditPos;
begin
  if Assigned(AStackViewItem) then
  begin
    FileName := AStackViewItem.SourceName;
    S := FindModule(FileName);
    if (S <> '') and Assigned(BorlandIDEServices) then
    begin
      Module := (BorlandIDEServices as IOTAModuleServices).OpenModule(S);
      if Assigned(Module) then
      begin
        Module.Show;
        for I := 0 to Module.ModuleFileCount - 1 do
          if Supports(Module.ModuleFileEditors[I], IOTASourceEditor, SourceEditor) then
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
end;

end.
