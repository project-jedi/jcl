{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) extension                                                        }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JediInstallIntf.pas.                                                        }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) of Petr Vones. All Rights Reserved.                                                }
{                                                                                                  }
{ Contributor(s): Robert Rossmair (crossplatform & BCB support)                                    }
{                 Florent Ouchet (new core for more than one target)                               }
{                                                                                                  }
{ Last modified: $Date$                          }
{                                                                                                  }
{**************************************************************************************************}

unit JediInstall;

{$I jcl.inc}
{$I crossplatform.inc}

interface

uses
  SysUtils, Classes,
  JclContainerIntf;

type
  TJediInstallGUIOption =
    (
      goExpandable,
      goRadioButton,
      goNoAutoCheck,            // do not auto-check when the parent node gets checked
      goStandaloneParent,       // do not auto-uncheck when all child nodes are unchecked
      goChecked
    );
  TJediInstallGUIOptions = set of TJediInstallGUIOption;

type
  TDialogType = (dtWarning, dtError, dtInformation, dtConfirmation);
  TDialogTypes = set of TDialogType;
  TDialogResponse = (drYes, drNo, drOK, drCancel);
  TDialogResponses = set of TDialogResponse;

  EJediInstallInitFailure = class(Exception);

  IJediPage = interface
    ['{5669B427-F46D-4737-9D1D-680C52CDE3DF}']
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function GetHintAtPos(ScreenX, ScreenY: Integer): string;
    procedure Show;

    property Caption: string read GetCaption write SetCaption;
  end;

  IJediReadmePage = interface(IJediPage)
    ['{5DA5C5C9-649F-47CF-B64A-55E983CA88EC}']
    procedure SetReadmeFileName(const Value: string);
    function GetReadmeFileName: string;

    property ReadmeFileName: string read GetReadmeFileName write SetReadmeFileName;
  end;

  IJediInstallPage = interface(IJediPage)
    ['{91C3A26F-0258-410A-9EAF-06F86C5748CF}']
    procedure AddInstallOption(Id: Integer; Options: TJediInstallGUIOptions;
      const Caption: string = ''; const Hint: string = ''; Parent: Integer = -1);
    procedure InitDisplay;
    function GetOptionChecked(Id: Integer): Boolean;
    procedure SetOptionChecked(Id: Integer; Value: Boolean);
    function GetDirectoryCount: Integer;
    function GetDirectory(Index: Integer): string;
    procedure SetDirectory(Index: Integer; const Value: string);
    function AddDirectory(Caption: string): Integer;
    function GetProgress: Integer;
    procedure SetProgress(Value: Integer);
    procedure BeginInstall;
    procedure MarkOptionBegin(Id: Integer);
    procedure MarkOptionEnd(Id: Integer; Failed: Boolean);
    procedure EndInstall;
    procedure CompilationStart(const ProjectName: string);
    procedure AddHint(const Line: string);
    procedure AddWarning(const Line: string);
    procedure AddError(const Line: string);
    procedure AddFatal(const Line: string);
    procedure AddText(const Line: string);
    procedure CompilationProgress(const FileName: string; LineNumber: Integer);
    procedure SetIcon(const FileName: string);

    property OptionChecked[Id: Integer]: Boolean read GetOptionChecked write SetOptionChecked;
    property DirectoryCount: Integer read GetDirectoryCount;
    property Directories[Index: Integer]: string read GetDirectory write SetDirectory;
    property Progress: Integer read GetProgress write SetProgress;
  end;

  TOptionRec = record
    Name: string;
    Value: string;
  end;

  TOptionArray = array of TOptionRec;

  TStringArray = array of string;

  IJediConfiguration = interface
    ['{4E96C8E8-ABA7-475D-BDF9-88B158F2CED3}']
    function GetSections: TStringArray;
    function GetOptions(const Section: string): TOptionArray;
    function GetOptionAsBool(const Section: string; Id: Integer): Boolean;
    procedure SetOptionAsBool(const Section: string; Id: Integer; Value: Boolean);
    function GetOptionAsBoolByName(const Section: string; const Name: string): Boolean;
    procedure SetOptionAsBoolByName(const Section: string; const Name: string; Value: Boolean);
    function GetOptionAsString(const Section: string; Id: Integer): string;
    procedure SetOptionAsString(const Section: string; Id: Integer; const Value: string);
    function GetOptionAsStringByName(const Section: string; const Name: string): string;
    procedure SetOptionAsStringByName(const Section: string; const Name: string; const Value: string);

    procedure Clear;
    procedure DeleteSection(const Section: string);
    procedure DeleteOption(const Section: string; Id: Integer);
    function SectionExists(const Section: string): Boolean;
    function ValueExists(const Section: string; Id: Integer): Boolean; overload;
    function ValueExists(const Section: string; const Name: string): Boolean; overload;

    property Sections: TStringArray read GetSections;
    property Options[const Section: string]: TOptionArray read GetOptions;
    property OptionAsBool[const Section: string; Id: Integer]: Boolean read GetOptionAsBool
      write SetOptionAsBool;
    property OptionAsBoolByName[const Section: string; const Name: string]: Boolean
      read GetOptionAsBoolByName write SetOptionAsBoolByName;
    property OptionAsString[const Section: string; Id: Integer]: string read GetOptionAsString
      write SetOptionAsString;
    property OptionAsStringByName[const Section: string; const Name: string]: string
      read GetOptionAsStringByName write SetOptionAsStringByName;
  end;

  IJediDistribution = interface
    ['{90E201C9-EA6B-446A-9251-D2516867874D}']
  end;

  TInstallEvent = procedure of Object;

  // GUI abstraction layer
  IJediInstallGUI = interface
    ['{3471A535-51D7-4FBB-B6AE-20D136E38E34}']
    function Dialog(const Text: string; DialogType: TDialogType = dtInformation;
      Options: TDialogResponses = [drOK]): TDialogResponse;
    function CreateReadmePage: IJediReadmePage;
    function CreateInstallPage: IJediInstallPage;
    function GetPageCount: Integer;
    function GetPage(Index: Integer): IJediPage;
    function GetStatus: string;
    procedure SetStatus(const Value: string);
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function GetProgress: Integer;
    procedure SetProgress(Value: Integer);
    function GetAutoAcceptDialogs: TDialogTypes;
    procedure SetAutoAcceptDialogs(Value: TDialogTypes);
    function GetAutoCloseOnFailure: Boolean;
    procedure SetAutoCloseOnFailure(Value: Boolean);
    function GetAutoCloseOnSuccess: Boolean;
    procedure SetAutoCloseOnSuccess(Value: Boolean);
    function GetAutoInstall: Boolean;
    procedure SetAutoInstall(Value: Boolean);
    function GetAutoUninstall: Boolean;
    procedure SetAutoUninstall(Value: Boolean);
    procedure Execute;

    property AutoAcceptDialogs: TDialogTypes read GetAutoAcceptDialogs write SetAutoAcceptDialogs;
    property AutoCloseOnFailure: Boolean read GetAutoCloseOnFailure write SetAutoCloseOnFailure;
    property AutoCloseOnSuccess: Boolean read GetAutoCloseOnSuccess write SetAutoCloseOnSuccess;
    property AutoInstall: Boolean read GetAutoInstall write SetAutoInstall;
    property AutoUninstall: Boolean read GetAutoUninstall write SetAutoUninstall;
    property PageCount: Integer read GetPageCount;
    property Pages[Index: Integer]: IJediPage read GetPage;
    property Status: string read GetStatus write SetStatus;
    property Caption: string read GetCaption write SetCaption;
    property Progress: Integer read GetProgress write SetProgress;
  end;

  IJediProduct = interface
    ['{CF5BE67A-4A49-43FB-8F6E-217A51023DA4}']
    procedure Init;
    function Install: Boolean;
    function Uninstall: Boolean;
    procedure Close;
  end;

  TJediInstallGUICreator = function: IJediInstallGUI;
  TJediConfigurationCreator = function: IJediConfiguration;

  TCompileLineType = (clText, clFileProgress, clHint, clWarning, clError, clFatal);

  TJediInstallCore = class(TComponent)
  private
    FInstallGUI: IJediInstallGUI;
    {$IFDEF VisualCLX}
    FGUIComponent: TComponent;
    {$ENDIF VisualCLX}
    FProducts: IJclIntfList;
    FClosing: Boolean;
    FOptions: TStrings;
    FInstallGUICreator: TJediInstallGUICreator;
    FConfiguration: IJediConfiguration;
    FConfigurationCreator: TJediConfigurationCreator;
    function GetProductCount: Integer;
    function GetProduct(Index: Integer): IJediProduct;
    function GetInstallGUI: IJediInstallGUI;
    function GetConfiguration: IJediConfiguration;
  {$IFDEF VisualCLX}
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;    
  {$ENDIF VisualCLX}
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function AddProduct(AProduct: IJediProduct): Integer;
    procedure Execute;
    function Install: Boolean;
    function Uninstall: Boolean;
    procedure Close;
    function AddInstallOption(const Name: string): Integer;
    function GetInstallOptionName(Id: Integer): string;
    function GetOptionCount: Integer;
    function ProcessLogLine(const Line: string; var LineType: TCompileLineType;
      Page: IJediInstallPage): string;

    property ProductCount: Integer read GetProductCount;
    property Products[Index: Integer]: IJediProduct read GetProduct;
    property Closing: Boolean read FClosing;
    property InstallOptionName[Id: Integer]: string read GetInstallOptionName;
    property OptionCount: Integer read GetOptionCount;
    property InstallGUI: IJediInstallGUI read GetInstallGUI;
    property InstallGUICreator: TJediInstallGUICreator read FInstallGUICreator
      write FInstallGUICreator;
    property Configuration: IJediConfiguration read GetConfiguration;
    property ConfigurationCreator: TJediConfigurationCreator read FConfigurationCreator
      write FConfigurationCreator;
  end;

var
  JediTargetOption: Integer = -1;

function InstallCore: TJediInstallCore;

resourcestring
  RsCantFindFiles    = 'Can not find installation files, check your installation.';
  RsCloseRADTool     = 'Please close all running instances of Delphi/C++Builder IDE before the installation.';
  RsConfirmInstall   = 'Are you sure to install all selected features?';
  RsConfirmUninstall = 'Do you really want to uninstall the JCL?';
  RsInstallSuccess   = 'Installation finished';
  RsInstallFailure   = 'Installation failed.'#10'Check compiler output for details.';
  RsNoInstall        = 'There is no Delphi/C++Builder installation on this machine. Installer will close.';
  RsUpdateNeeded     = 'You should install latest Update Pack #%d for %s.'#13#10 +
                       'Would you like to open Borland support web page?';
  RsHintTarget       = 'Installation target';

implementation

uses
  JclArrayLists, JclFileUtils;

var
  InternalInstallCore: TJediInstallCore = nil;

function InstallCore: TJediInstallCore;
begin
  if not Assigned(InternalInstallCore) then
    InternalInstallCore := TJediInstallCore.Create;
  Result := InternalInstallCore;
end;

//=== { TJediInstallCore } ===================================================

function TJediInstallCore.AddInstallOption(const Name: string): Integer;
begin
  Result := FOptions.IndexOf(Name);
  if Result = -1 then
    Result := FOptions.Add(Name);
end;

function TJediInstallCore.AddProduct(AProduct: IJediProduct): Integer;
begin
  Result := FProducts.Size;
  FProducts.Add(AProduct);
end;

procedure TJediInstallCore.Close;
var
  Index: Integer;
begin
  if Closing then
    Exit;
  FClosing := True;
  
  for Index := FProducts.Size - 1 downto 0 do
    (FProducts.GetObject(Index) as IJediProduct).Close;
  FProducts.Clear;
  FProducts := nil;
  FInstallGUI := nil;
  FConfiguration := nil;
end;

constructor TJediInstallCore.Create;
begin
  inherited Create(nil);
  
  FOptions := TStringList.Create;
  FProducts := TJclIntfArrayList.Create;
  FClosing := False;
  JediTargetOption := AddInstallOption('joTarget');
end;

destructor TJediInstallCore.Destroy;
begin
  Close;
  FConfigurationCreator := nil;
  FInstallGUICreator := nil;
  FProducts := nil;
  FInstallGUI := nil;
  FConfiguration := nil;
  FOptions.Free;

  inherited Destroy;
end;

procedure TJediInstallCore.Execute;
var
  Index: Integer;
  AInstallGUI: IJediInstallGUI;
begin
  AInstallGUI := InstallGUI;

  for Index := FProducts.Size - 1 downto 0 do
    (FProducts.GetObject(Index) as IJediProduct).Init;

  if Assigned(AInstallGUI) then
    AInstallGUI.Execute;
end;

function TJediInstallCore.GetConfiguration: IJediConfiguration;
begin
  if Assigned(FConfigurationCreator) and not Assigned(FConfiguration) then
    FConfiguration := ConfigurationCreator;
  Result := FConfiguration;
end;

function TJediInstallCore.GetInstallGUI: IJediInstallGUI;
var
{$IFDEF VisualCLX}
  CompRef: IInterfaceComponentReference;
{$ENDIF VisualCLX}
  AutoAcceptDialogs: TDialogTypes;
begin
  if Assigned(FInstallGUICreator) and not Assigned(FInstallGUI) then
  begin
    FInstallGUI := InstallGUICreator;
    AutoAcceptDialogs := [];
    if ParamPos('AcceptInformations') >= 1 then
      Include(AutoAcceptDialogs, dtInformation);
    if ParamPos('AcceptConfirmations') >= 1 then
      Include(AutoAcceptDialogs, dtConfirmation);
    if ParamPos('AcceptWarnings') >= 1 then
      Include(AutoAcceptDialogs, dtWarning);
    if ParamPos('AcceptErrors') >= 1 then
      Include(AutoAcceptDialogs, dtError);
    FInstallGUI.AutoAcceptDialogs := AutoAcceptDialogs;
    FInstallGUI.AutoCloseOnFailure := ParamPos('CloseOnFailure') >= 1;
    FInstallGUI.AutoCloseOnSuccess := ParamPos('CloseOnSuccess') >= 1;
    FInstallGUI.AutoInstall := ParamPos('Install') >= 1;
    FInstallGUI.AutoUninstall := ParamPos('Uninstall') >= 1;
  end;
  Result := FInstallGUI;
{$IFDEF VisualCLX}
  Result.QueryInterface(IInterfaceComponentReference, CompRef);
  if Assigned(CompRef) then
  begin
    FGUIComponent := CompRef.GetComponent;
    FGuiComponent.FreeNotification(Self);
  end;
{$ENDIF VisualCLX}
end;

function TJediInstallCore.GetInstallOptionName(Id: Integer): string;
begin
  Result := FOptions.Strings[Id];
end;

function TJediInstallCore.GetOptionCount: Integer;
begin
  Result := FOptions.Count;
end;

function TJediInstallCore.GetProduct(Index: Integer): IJediProduct;
begin
  Result := FProducts.GetObject(Index) as IJediProduct;
end;

function TJediInstallCore.GetProductCount: Integer;
begin
  Result := FProducts.Size;
end;

function TJediInstallCore.Install: Boolean;
var
  Index: Integer;
begin
  Result := True;
  for Index := FProducts.Size - 1 downto 0 do
  begin
    Result := (FProducts.GetObject(Index) as IJediProduct).Install;
    if not Result then
      Break;
  end;
end;

{$IFDEF VisualCLX}
procedure TJediInstallCore.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FGUIComponent) then
  begin
    FGUIComponent := nil;
    FInstallGUI := nil;
  end;
end;
{$ENDIF VisualCLX}

function TJediInstallCore.ProcessLogLine(const Line: string;
  var LineType: TCompileLineType; Page: IJediInstallPage): string;

  function HasText(Text: string; const Values: array of string): Boolean;
  var
    i: Integer;
  begin
    Result := True;
    Text := AnsiLowerCase(Text);
    for i := Low(Values) to High(Values) do
      if Pos(Values[i], Text) > 0 then
        Exit;
    Result := False;
  end;

  function IsCompileFileLine(const Line: string): Boolean;

    function PosLast(Ch: Char; const S: string): Integer;
    begin
      for Result := Length(S) downto 1 do
        if S[Result] = Ch then
          Exit;
      Result := 0;
    end;

  var
    ps, psEnd, LineNum, Err: Integer;
    Filename: string;
  begin
    Result := False;
    ps := PosLast('(', Line);
    if (ps > 0) and (Pos(': ', Line) = 0) and (Pos('.', Line) > 0) then
    begin
      psEnd := PosLast(')', Line);
      if psEnd < ps then
        Exit;

      Filename := Copy(Line, 1, ps - 1);
      if (Filename <> '') and (Filename[Length(Filename)] > #32) then
      begin
        Val(Copy(Line, ps + 1, psEnd - ps - 1), LineNum, Err);
        if Err = 0 then
        begin
          if Assigned(Page) then
            Page.CompilationProgress(FileName, LineNum);
          Result := True;
        end;
      end;
    end;
  end;

begin
  LineType := clText;
  Result := Line;
  if Line = '' then
    Exit;

  if IsCompileFileLine(Line) then
  begin
    LineType:= clFileProgress;
    Result := '';
  end
  else if HasText(Line, ['hint: ', 'hinweis: ', 'suggestion: ', 'conseil: ']) then // do not localize
  begin
    // hide hint about getter/setter names
    if (Pos(' H2369 ', Line) = 0) then
    begin
      LineType := clHint;
      if Assigned(Page) then
        Page.AddHint(Line);
    end
    else
      Result := '';
  end
  else if HasText(Line, ['warning: ', 'warnung: ', 'avertissement: ']) then // do not localize
  begin
    // hide platform warnings
    if (Pos(' W1002 ', Line) = 0) then
    begin
      LineType := clWarning;
      if Assigned(Page) then
        Page.AddWarning(Line);
    end
    else
      Result := '';
  end
  else if HasText(Line, ['error: ', 'fehler: ', 'erreur: ']) then // do not localize
  begin
    LineType := clError;
    if Assigned(Page) then
      Page.AddError(Line);
  end
  else if HasText(Line, ['fatal: ', 'schwerwiegend: ', 'fatale: ']) then // do not localize
  begin
    LineType := clFatal;
    if Assigned(Page) then
      Page.AddFatal(Line);
  end
  else if Assigned(Page) then
    Page.AddText(Line);
end;

function TJediInstallCore.Uninstall: Boolean;
var
  Index: Integer;
begin
  Result := True;
  for Index := FProducts.Size - 1 downto 0 do
    Result := (FProducts.GetObject(Index) as IJediProduct).Uninstall and Result;
end;

initialization

finalization

InternalInstallCore.Free;

end.
