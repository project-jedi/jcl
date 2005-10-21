unit JclUsesWizard;

{$I jcl.inc}

interface

uses
  Classes, SysUtils, Windows, Registry;

type
  TWizardAction = (waSkip, waAddToImpl, waAddToIntf, waMoveToIntf);
  PErrorInfo = ^TErrorInfo;
  TErrorInfo = record
    // parsed from compiler message
    UnitName: array [0..MAX_PATH - 1] of Char;
    LineNumber: Integer;
    Identifier: array [0..MAX_PATH - 1] of Char;
    // resolved by wizard
    UsesName: array [0..MAX_PATH - 1] of Char; // unit name to be added to uses clause
  end;

const
  SJCLRegSubkey = 'Jedi\JCL\IDE\JclUsesExpert';
  SIniIdentifierLists = 'IdentifierLists';
  SRegDebugLibPath = 'Debug Library';
  SRegLibPath = 'Library';
  SRegWizardActive = 'Uses Wizard Active';
  SRegWizardCofirm = 'Uses Wizard Confirm';
  SRegWizardIniFile = 'Configuration File';

resourcestring
  SIntfUsesNotFound = 'JEDI Uses wizard: Uses clause not found in the interface section';
  SImplUsesNotFound = 'JEDI Uses wizard: Uses clause not found in the implementation section';
  SIntfUsesInvalid = 'JEDI Uses wizard: Uses clause invalid in the interface section';
  SImplUsesInvalid = 'JEDI Uses wizard: Uses clause invalid in the implementation section';

procedure Register;
procedure SettingsChanged;

implementation

uses
  ToolsAPI, Messages, Forms, Controls, Dialogs, ActnList, StdCtrls, ExtCtrls, ComCtrls, IniFiles,
  JclOptionsFrame, JclParseUses, JclUsesDialog, JclFileUtils;

const
  SJCLUsesWizardID = 'JEDI.JCLUsesWizard'; // wizard ID
  SJCLUsesWizardName = 'JCL Uses Wizard'; // wizard name
  SEnvOptionsDlgClassName = 'TPasEnvironmentDialog';

type
  TJCLUsesWizardNotifier = class(TNotifierObject, IOTANotifier, IOTAIDENotifier, IOTAIDENotifier50)
  private
    { IOTAIDENotifier }
    procedure AfterCompile(Succeeded: Boolean); overload;
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
    { IOTAIDENotifier50 }
    procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); overload;
    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean); overload;
  end;

  TJCLUsesWizard = class(TNotifierObject, IOTANotifier, IOTAWizard)
  private
    FActive: Boolean;
    FApplicationIdle: TIdleEvent;
    FConfirmChanges: Boolean;
    FErrors: TList;
    FIdentifierLists: TStrings;
    FIniFile: string;
    FNotifierIndex: Integer;
    procedure AppIdle(Sender: TObject; var Done: Boolean);
    function AppWindowHook(var Msg: TMessage): Boolean;
    procedure ClearErrors;
    function DoConfirmChanges(ChangeList: TStrings): TModalResult;
    procedure InitializeIdentifierLists;
    procedure ProcessCompilerMessages(Messages: TStrings);
    procedure ProcessUses;
    procedure ResolveUsesName(Error: PErrorInfo);
    procedure SetActive(Value: Boolean);
    procedure SetConfirmChanges(Value: Boolean);
    { IOTAWizard }
    procedure Execute;
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
  public
    Value: Integer;
    constructor Create;
    destructor Destroy; override;
    function LoadFromRegistry: Boolean;
    property Active: Boolean read FActive write SetActive;
    property ConfirmChanges: Boolean read FConfirmChanges write SetConfirmChanges;
    property IniFile: string read FIniFile;
  end;

var
  Wizard: TJCLUsesWizard = nil;

function FindClassForm(const AClassName: string): TForm;
var
  I: Integer;
begin
  Result := nil;
  with Screen do
    for I := 0 to FormCount - 1 do
      if Forms[I].ClassNameIs(AClassName) then
      begin
        Result := Forms[I];
        Break;
      end;
end;

function GetActiveProject: IOTAProject;
var
  ProjectGroup: IOTAProjectGroup;
  I: Integer;
begin
  Result := nil;

  with BorlandIDEServices as IOTAModuleServices do
  begin
    ProjectGroup := nil;
    for I := 0 to ModuleCount - 1 do
      if Supports(Modules[I], IOTAProjectGroup, ProjectGroup) then
        Break;

    if Assigned(ProjectGroup) then
      Result := ProjectGroup.ActiveProject
    else
      for I := 0 to ModuleCount - 1 do
        if Supports(Modules[I], IOTAProject, Result) then
          Break;
  end;
end;

function GetLineNumber(S1, S2: PChar): Integer;
var
  P: PChar;
begin
  if S2 < S1 then
    Result := 0
  else
  begin
    Result := 1;
    P := StrPos(S1, #13#10);
    while (P <> nil) and (P <= S2) do
    begin
      Inc(Result);

      P := StrPos(P + 2, #13#10);
    end;
  end;
end;

//=== { TLine } ==============================================================

// TLine 'guessed' from coreide60.bpl

type
  TLine = class(TObject)
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function GetLineText: string; virtual;
  end;

{ TLine stubs }

constructor TLine.Create;
begin
end;

destructor TLine.Destroy;
begin
  inherited Destroy;
end;

function TLine.GetLineText: string;
begin
  Result := '';
end;

// the message treeview is custom drawn; hence this hack

procedure GetCompilerMessages(List: TStrings);
var
  MessageViewForm: TForm;
  I: Integer;
  TreeView: TTreeView;
  Node: TTreeNode;
  Line: TLine;
begin
  // if TMsgWindow exists all messages are sent to it
  MessageViewForm := FindClassForm('TMsgWindow');
  if MessageViewForm = nil then // otherwise TMessageViewForm is used
    MessageViewForm := FindClassForm('TMessageViewForm');

  if Assigned(MessageViewForm) then
  begin
    TreeView := nil;
    with MessageViewForm do
      for I := 0 to ControlCount - 1 do
        if Controls[I].ClassNameIs('TTreeMessageView') then
        begin
          TreeView := Controls[I] as TTreeView;
          Break;
        end;

    if Assigned(TreeView) then
    begin
      with TreeView do
      begin
        Node := Items.GetFirstNode;
        while Node <> nil do
        begin
          Line := TLine(Node.Data);

          if Assigned(Line) then
            List.Add(Line.GetLineText);

          Node := Node.GetNext;
        end;
      end;
    end;
  end;
end;

function ReadEditorBuffer(Buffer: IOTAEditBuffer): string;
const
  BufSize = 1024;
var
  Reader: IOTAEditReader;
  Stream: TStringStream;
  ReaderPos, Read: Integer;
  Buf: array [0..BufSize] of Char;
begin
  Result := '';
  if Buffer = nil then
    Exit;

  Reader := Buffer.CreateReader;
  Stream := TStringStream.Create('');
  try
    ReaderPos := 0;
    repeat
      Read := Reader.GetText(ReaderPos, @Buf, BufSize);
      Inc(ReaderPos, Read);
      if (Read < 0) or (Read > BufSize) then
        raise Exception.Create('Error reading from edit buffer');
      Buf[Read] := #0;
      Stream.WriteString(Buf);
    until Read < BufSize;
    
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

function ReadString(S: PChar; Len: Integer): string;
begin
  SetString(Result, S, Len);
end;

//=== { TJCLUsesWizardNotifier } =============================================

// TJCLUsesWizardNotifier private: IOTAIDENotifier

procedure TJCLUsesWizardNotifier.AfterCompile(Succeeded: Boolean);
begin
  // do nothing
end;

procedure TJCLUsesWizardNotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
  // do nothing
end;

procedure TJCLUsesWizardNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  // do nothing
end;

//=== { TJCLUsesWizardNotifier } =============================================

// TJCLUsesWizardNotifier private: IOTAIDENotifier50

procedure TJCLUsesWizardNotifier.AfterCompile(Succeeded, IsCodeInsight: Boolean);
var
  Messages: TStrings;
begin
  if IsCodeInsight or Succeeded then
    Exit;

  Messages := TStringList.Create;
  try
    GetCompilerMessages(Messages);
    if Assigned(Wizard) then
      Wizard.ProcessCompilerMessages(Messages);
  finally
    Messages.Free;
  end;
end;

procedure TJCLUsesWizardNotifier.BeforeCompile(const Project: IOTAProject;
  IsCodeInsight: Boolean; var Cancel: Boolean);
begin
  // do nothing
end;

//=== { TJCLUsesWizard } =====================================================

// private

procedure TJCLUsesWizard.AppIdle(Sender: TObject; var Done: Boolean);
begin
  Application.OnIdle := FApplicationIdle;
  FApplicationIdle := nil;

  if FErrors.Count = 0 then
    Exit;

  ProcessUses;
end;

function TJCLUsesWizard.AppWindowHook(var Msg: TMessage): Boolean;
var
  Form: TForm;
  Panel2: TPanel;
  PropSheetControl1: TPageControl;
  JCLOptionsTab: TTabSheet;
begin
  Result := False;

  // ShowModal of any form always calls DisableTaskWindows
  if (Msg.Msg = WM_ENABLE) and not TWMEnable(Msg).Enabled then
  begin
    Form := FindClassForm(SEnvOptionsDlgClassName);
    if not Assigned(Form) then
      Exit;

    Panel2 := Form.FindChildControl('Panel2') as TPanel;
    if not Assigned(Panel2) then
      Exit;

    PropSheetControl1 := Panel2.FindChildControl('PropertySheetControl1') as TPageControl;
    if not Assigned(PropSheetControl1) then
      Exit;

    JCLOptionsTab := TTabSheet.Create(Form);
    try
      JCLOptionsTab.PageControl := PropSheetControl1;
      JCLOptionsTab.Caption := 'JEDI Options';

      TFrameJclOptions.Create(JCLOptionsTab);
    except
      JCLOptionsTab.Free;
      raise;
    end;
  end;
end;

procedure TJCLUsesWizard.ClearErrors;
var
  I: Integer;
  P: PErrorInfo;
begin
  for I := 0 to FErrors.Count - 1 do
  begin
    P := FErrors[I];
    FreeMem(P);
  end;
  FErrors.Clear;
end;

function TJCLUsesWizard.DoConfirmChanges(ChangeList: TStrings): TModalResult;
var
  Dialog: TFormUsesConfirm;
begin
  Dialog := TFormUsesConfirm.Create(nil, ChangeList, FErrors);
  try
    Result := Dialog.ShowModal;
  finally
    Dialog.Free;
  end;
end;

// load identifier lists
// each line represents one JCL unit in the following format:
// <unit_name>=<identifier0>,<identifier1>,...

procedure TJCLUsesWizard.InitializeIdentifierLists;
var
  IniFile: TIniFile;
  I: Integer;
  IdentListFileName: string;
  IdentList: TStrings;
begin
  FIdentifierLists.Clear;

  IniFile := TIniFile.Create(FIniFile);
  try
    IdentList := TStringList.Create;
    try
      IniFile.ReadSection(SIniIdentifierLists, FIdentifierLists);
      for I := 0 to FIdentifierLists.Count - 1 do
      begin
        IdentListFileName := IniFile.ReadString(SIniIdentifierLists, FIdentifierLists[I],
          ChangeFileExt(FIdentifierLists[I], '.txt'));
        if ExtractFilePath(IdentListFileName) = '' then
          IdentListFileName := ExtractFilePath(FIniFile) + IdentListFileName;

          IdentList.LoadFromFile(IdentListFileName);
          FIdentifierLists[I] := FIdentifierLists[I] + '=' + IdentList.CommaText;
      end;
    finally
      IdentList.Free;
    end;
  finally
    IniFile.Free;
  end;
end;

// load localized strings for the undeclared identifier error

procedure TJCLUsesWizard.ProcessCompilerMessages(Messages: TStrings);
const
  SIdentFormatSpec = '%s';
var
  I: Integer;
  Error: PErrorInfo;
  SError: string;
  SUndeclaredIdent: string;

  procedure LoadDcc32Strings;
  const
    {$IFDEF COMPILER6}
    SErrorID = 4147; // 'Error'
    SUndeclaredIdentID = 47; // 'Undeclared identifier: ''%s'''
    {$ELSE}
    SErrorID = 4200;
    SUndeclaredIdentID = 2;
    {$ENDIF COMPILER6}
  var
    Registry: TRegistry;
    Dcc32FileName: string;
    Dcc32: HMODULE;
    ResString: TResStringRec;
  begin
    SError := '';
    SUndeclaredIdent := '';
    
    Dcc32FileName := 'dcc32.exe';

    // try to retrieve and prepend Delphi bin path
    Registry := TRegistry.Create(KEY_READ);
    try
      {$IFDEF COMPILER6_UP}
      Registry.RootKey := HKEY_CURRENT_USER;
      if Registry.OpenKeyReadOnly((BorlandIDEServices as IOTAServices).GetBaseRegistryKey) then
        Dcc32FileName := PathAddSeparator(Registry.ReadString('RootDir')) + 'Bin\' + Dcc32FileName
      else
      {$ENDIF COMPILER6_UP}
      begin
        Registry.RootKey := HKEY_LOCAL_MACHINE;
        if Registry.OpenKeyReadOnly((BorlandIDEServices as IOTAServices).GetBaseRegistryKey) then
          Dcc32FileName := PathAddSeparator(Registry.ReadString('RootDir')) + 'Bin\' + Dcc32FileName;
      end;
    finally
      Registry.Free;
    end;

    // try to load localized resources first
    Dcc32 := LoadResourceModule(PChar(Dcc32FileName));
    if Dcc32 = 0 then // if not found try the executable
      Dcc32 := LoadLibraryEx(PChar(Dcc32FileName), 0, LOAD_LIBRARY_AS_DATAFILE);
    if Dcc32 = 0 then
      Exit;

    try
      ResString.Module := @Dcc32;
      ResString.Identifier := SErrorID;
      SError := LoadResString(@ResString);

      ResString.Identifier := SUndeclaredIdentID;
      SUndeclaredIdent := LoadResString(@ResString);
    finally
      FreeLibrary(Dcc32);
    end;
  end;

  // example error message: [Error] Unit1.pas(37): Undeclared identifier: 'GetWindowsFolder'

  function ParseMessage(const Msg: string; var Error: PErrorInfo): Boolean;
  var
    P, P1, P2: PChar;
    UnitName: string;
    LineNumber: Integer;
    Identifier: string;
  begin
    Result := False;
    Error := nil;
    P := PChar(Msg);

    // check opening bracket
    if P^ <> '[' then
      Exit;
    Inc(P);

    // check severity
    if StrLComp(P, PChar(SError), Length(SError)) <> 0 then
      Exit;
    Inc(P, Length(SError));

    // check closing bracket
    if P^ <> ']' then
      Exit;
    Inc(P);

    // check space
    if P^ <> ' ' then
      Exit;
    Inc(P);

    // read unit name
    UnitName := '';
    while P^ <> '(' do
    begin
      if P^ = #0 then
        Break;

      UnitName := UnitName + P^;

      Inc(P);
    end;
    if UnitName = '' then
      Exit;
    if P^ <> '(' then
      Exit;
    Inc(P);

    // read line number
    LineNumber := 0;
    while P^ <> ')' do
    begin
      if P^ = #0 then
        Break;

      LineNumber := LineNumber * 10 + Ord(P^) - Ord('0');

      Inc(P);
    end;
    if LineNumber = 0 then
      Exit;
    if P^ <> ')' then
      Exit;
    Inc(P);

    // check colon
    if P^ <> ':' then
      Exit;
    Inc(P);

    // check space
    if P^ <> ' ' then
      Exit;
    Inc(P);

    // check text
    Identifier := '';
    P1 := PChar(SUndeclaredIdent);

    // check text up to '%s'
    P2 := StrPos(P1, SIdentFormatSpec);
    if P2 = nil then
      Exit;
    if StrLComp(P, P1, P2 - P1) <> 0 then
      Exit;

    P1 := P + (P2 - P1);

    // check text after '%s'
    Inc(P2, Length(SIdentFormatSpec));
    P := StrEnd(P);
    Dec(P, StrLen(P2));

    if StrComp(P, P2) <> 0 then
      Exit;

    // copy identifier
    while P1 < P do
    begin
      Identifier := Identifier + P1^;
      Inc(P1);
    end;
    if Identifier = '' then
      Exit;

    // match
    Error := AllocMem(SizeOf(TErrorInfo));
    try
      StrLCopy(Error^.UnitName, PChar(UnitName), Length(Error^.UnitName));
      Error^.LineNumber := LineNumber;
      StrLCopy(Error^.Identifier, PChar(Identifier), Length(Error^.Identifier));

      Result := True;
    except
      FreeMem(Error);
      raise;
    end;
  end;

begin
  ClearErrors;
  if not Assigned(Messages) then
    Exit;

  LoadDcc32Strings;
  for I := 0 to Messages.Count - 1 do
    if ParseMessage(Messages[I], Error) then
      FErrors.Add(Error);

  for I := 0 to FErrors.Count - 1 do
    ResolveUsesName(FErrors[I]);

  for I := FErrors.Count - 1 downto 0 do
  begin
    Error := FErrors[I];
    if Error^.UsesName = '' then
    begin
      FreeMem(Error);
      FErrors.Delete(I);
    end;
  end;

  Application.ProcessMessages;

  FApplicationIdle := Application.OnIdle;
  Application.OnIdle := AppIdle;
end;

procedure TJCLUsesWizard.ProcessUses;
var
  GoalSource: string;
  Goal: TCustomGoal;
  I: Integer;
  ChangeList: TStrings;
  IntfLength, ImplLength: Integer;
  Writer: IOTAEditWriter;
  Project: IOTAProject;
begin
  GoalSource := '';
  with BorlandIDEServices as IOTAEditorServices do
    if Assigned(TopBuffer) then
      GoalSource := ReadEditorBuffer(TopBuffer)
    else
      Exit;

  Goal := CreateGoal(PChar(GoalSource));
  if not Assigned(Goal) then
    Exit;

  try
    if Goal is TProgramGoal then
      with TProgramGoal(Goal) do
      begin
        IntfLength := Length(UsesList.Text);
        ChangeList := TStringList.Create;
        try
          for I := 0 to FErrors.Count - 1 do
            with PErrorInfo(FErrors[I])^ do
              if (UsesName <> '') and (ChangeList.IndexOf(UsesName) = -1) then
                ChangeList.AddObject(UsesName, TObject(waAddToIntf));

          if not FConfirmChanges or (DoConfirmChanges(ChangeList) = mrOK) then
          begin
            for I := ChangeList.Count - 1 downto 0 do
              case TWizardAction(ChangeList.Objects[I]) of
                waAddToImpl, waAddToIntf:
                  if UsesList.Count = 0 then
                    UsesList.Add(ChangeList[I])
                  else
                    UsesList.Insert(0, ChangeList[I]);
              end;

            with BorlandIDEServices as IOTAEditorServices do
              if Assigned(TopBuffer) then
              begin
                Writer := TopBuffer.CreateUndoableWriter;
                try
                  Writer.CopyTo(Length(TextBeforeUses));
                  Writer.DeleteTo(Length(TextBeforeUses) + IntfLength);
                  Writer.Insert(PChar(UsesList.Text));
                  Writer.CopyTo(Length(GoalSource));
                finally
                  Writer := nil;
                end;
              end;

            // attempt to recompile
            Project := GetActiveProject;
            if Assigned(Project) and Assigned(Project.ProjectBuilder) then
              Project.ProjectBuilder.BuildProject(cmOTAMake, True, True);
          end;
        finally
          ChangeList.Free;
        end;
      end
    else
    if Goal is TLibraryGoal then
      with TLibraryGoal(Goal) do
      begin
        IntfLength := Length(UsesList.Text);
        ChangeList := TStringList.Create;
        try
          for I := 0 to FErrors.Count - 1 do
            with PErrorInfo(FErrors[I])^ do
              if (UsesName <> '') and (ChangeList.IndexOf(UsesName) = -1) then
                ChangeList.AddObject(UsesName, TObject(waAddToIntf));

          if not FConfirmChanges or (DoConfirmChanges(ChangeList) = mrOK) then
          begin
            for I := ChangeList.Count - 1 downto 0 do
              case TWizardAction(ChangeList.Objects[I]) of
                waAddToImpl, waAddToIntf:
                  if UsesList.Count = 0 then
                    UsesList.Add(ChangeList[I])
                  else
                    UsesList.Insert(0, ChangeList[I]);
              end;

            with BorlandIDEServices as IOTAEditorServices do
              if Assigned(TopBuffer) then
              begin
                Writer := TopBuffer.CreateUndoableWriter;
                try
                  Writer.CopyTo(Length(TextBeforeUses));
                  Writer.DeleteTo(Length(TextBeforeUses) + IntfLength);
                  Writer.Insert(PChar(UsesList.Text));
                  Writer.CopyTo(Length(GoalSource));
                finally
                  Writer := nil;
                end;
              end;

            // attempt to recompile
            Project := GetActiveProject;
            if Assigned(Project) and Assigned(Project.ProjectBuilder) then
              Project.ProjectBuilder.BuildProject(cmOTAMake, True, True);
          end;
        finally
          ChangeList.Free;
        end;
      end
    else
    if Goal is TUnitGoal then
      with TUnitGoal(Goal) do
      begin
        IntfLength := Length(UsesIntf.Text);
        ImplLength := Length(UsesImpl.Text);
        ChangeList := TStringList.Create;
        try
          for I := 0 to FErrors.Count - 1 do
            with PErrorInfo(FErrors[I])^ do
              if (UsesName <> '') and (ChangeList.IndexOf(UsesName) = -1) then
              begin
                if LineNumber < GetLineNumber(PChar(GoalSource), PChar(GoalSource) + Length(TextBeforeIntf) +
                  IntfLength + Length(TextAfterIntf)) then // error in interface section
                begin
                  if UsesImpl.IndexOf(UsesName) = -1 then
                    ChangeList.AddObject(UsesName, TObject(waAddToIntf))
                  else
                    ChangeList.AddObject(UsesName, TObject(waMoveToIntf));
                end
                else // error in implementation section
                  ChangeList.AddObject(UsesName, TObject(waAddToImpl));
              end;

          if not FConfirmChanges or (DoConfirmChanges(ChangeList) = mrOK) then
          begin
            for I := ChangeList.Count - 1 downto 0 do
              case TWizardAction(ChangeList.Objects[I]) of
                waAddToImpl:
                  if UsesImpl.Count = 0 then
                    UsesImpl.Add(ChangeList[I])
                  else
                    UsesImpl.Insert(0, ChangeList[I]);
                waAddToIntf:
                  if UsesIntf.Count = 0 then
                    UsesIntf.Add(ChangeList[I])
                  else
                    UsesIntf.Insert(0, ChangeList[I]);
                waMoveToIntf:
                  begin
                    if UsesIntf.Count = 0 then
                      UsesIntf.Add(ChangeList[I])
                    else
                      UsesIntf.Insert(0, ChangeList[I]);
                    UsesImpl.Remove(UsesImpl.IndexOf(ChangeList[I]));
                  end;
                else
                  ChangeList.Delete(I);
              end;

            if ChangeList.Count = 0 then
              Exit;

            with BorlandIDEServices as IOTAEditorServices do
              if Assigned(TopBuffer) then
              begin
                Writer := TopBuffer.CreateUndoableWriter;
                try
                  Writer.CopyTo(Length(TextBeforeIntf));
                  Writer.DeleteTo(Length(TextBeforeIntf) + IntfLength);
                  Writer.Insert(PChar(UsesIntf.Text));
                  Writer.CopyTo(Length(TextBeforeIntf) + IntfLength + Length(TextAfterIntf));
                  Writer.DeleteTo(Length(TextBeforeIntf) + IntfLength + Length(TextAfterIntf) + ImplLength);
                  Writer.Insert(PChar(UsesImpl.Text));
                  Writer.CopyTo(Length(GoalSource));
                finally
                  Writer := nil;
                end;
              end;

            // attempt to recompile
            Project := GetActiveProject;
            if Assigned(Project) and Assigned(Project.ProjectBuilder) then
              Project.ProjectBuilder.BuildProject(cmOTAMake, True, True);
          end;
        finally
          ChangeList.Free;
        end;
      end;
  finally
    Goal.Free;
  end;
end;

procedure TJCLUsesWizard.ResolveUsesName(Error: PErrorInfo);
var
  I: Integer;
  Identifiers: TStrings;
  IdentifierIndex: Integer;
begin
  if FIdentifierLists.Count = 0 then
    InitializeIdentifierLists;

  Identifiers := TStringList.Create;
  try
    with FIdentifierLists do
      for I := 0 to Count - 1 do
      begin
        Identifiers.CommaText := Values[Names[I]];
        with Error^ do
        begin
          IdentifierIndex := Identifiers.IndexOf(Identifier);
          if IdentifierIndex <> -1 then
          begin
            StrLCopy(UsesName, PChar(Names[I]), Length(UsesName));
            Break;
          end;
        end;
      end;
  finally
    Identifiers.Free;
  end;
end;

procedure TJCLUsesWizard.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    if Value then
    begin
      with BorlandIDEServices as IOTAServices do
        FNotifierIndex := AddNotifier(TJCLUsesWizardNotifier.Create);

      FActive := FNotifierIndex <> -1;
    end
    else
    begin
      if FNotifierIndex <> -1 then
        with BorlandIDEServices as IOTAServices do
          RemoveNotifier(FNotifierIndex);

      FNotifierIndex := -1;
      FActive := False;
    end;
  end;
end;

procedure TJCLUsesWizard.SetConfirmChanges(Value: Boolean);
begin
  if Value <> FConfirmChanges then
  begin
    FConfirmChanges := Value;
  end;
end;

//=== { TJCLUsesWizard } =====================================================

// TJCLUsesWizard private: IOTAWizard

procedure TJCLUsesWizard.Execute;
begin
  // do nothing
end;

function TJCLUsesWizard.GetIDString: string;
begin
  Result := SJCLUsesWizardID;
end;

function TJCLUsesWizard.GetName: string;
begin
  Result := SJCLUsesWizardName;
end;

function TJCLUsesWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

//=== { TJCLUsesWizard } =====================================================

// public

constructor TJCLUsesWizard.Create;
begin
  inherited Create;
  FIdentifierLists := TStringList.Create;
  FErrors := TList.Create;
  FActive := False;
  FConfirmChanges := False;
  FNotifierIndex := -1;

  Application.HookMainWindow(AppWindowHook);
  LoadFromRegistry;
end;

destructor TJCLUsesWizard.Destroy;
begin
  Application.UnhookMainWindow(AppWindowHook);
  SetActive(False);
  Wizard := nil;
  ClearErrors;
  FErrors.Free;
  FIdentifierLists.Free;
  inherited Destroy;
end;

function TJCLUsesWizard.LoadFromRegistry: Boolean;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create(KEY_READ);
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    with BorlandIDEServices as IOTAServices do
    begin
      Result := Registry.OpenKey(GetBaseRegistryKey + '\' + SJCLRegSubkey, False);
      if not Result then
      begin
        Registry.RootKey := HKEY_LOCAL_MACHINE;
        Result := Registry.OpenKey(GetBaseRegistryKey + '\' + SJCLRegSubkey, False);
      end;
    end;

    if Result then
    begin
      SetActive(Registry.ValueExists(SRegWizardActive) and Registry.ReadBool(SRegWizardActive));
      FConfirmChanges := not Registry.ValueExists(SRegWizardCofirm) or Registry.ReadBool(SRegWizardCofirm);
      FIniFile := Registry.ReadString(SRegWizardIniFile);
    end;
  finally
    Registry.Free;
  end;
end;

// create and register wizard instance

procedure Register;
begin
  Wizard := TJCLUsesWizard.Create;
  RegisterPackageWizard(Wizard);
end;

procedure SettingsChanged;
begin
  if Assigned(Wizard) then
    Wizard.LoadFromRegistry;
end;

end.
