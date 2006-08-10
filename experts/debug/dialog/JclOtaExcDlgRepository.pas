{****************************************************************************}
{                                                                            }
{ Project JEDI Code Library (JCL)                                            }
{                                                                            }
{ The contents of this file are subject to the Mozilla Public License        }
{ Version 1.1 (the "License");                                               }
{ you may not use this file except in compliance with the License. You may   }
{ obtain a copy of the License at http://www.mozilla.org/MPL/                }
{                                                                            }
{ Software distributed under the License is distributed on an "AS IS" basis, }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   }
{ for the specific language governing rights and limitations under the       }
{ License.                                                                   }
{                                                                            }
{ The Original Code is JclOtaExcDlgRepository.pas.                           }
{                                                                            }
{ The Initial Developer of the Original Code is Florent Ouchet               }
{         <outchy att users dott sourceforge dott net>                       }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet.    }
{                                                                            }
{ Contributors:                                                              }
{                                                                            }
{****************************************************************************}
{                                                                            }
{ Last modified: $Date: $                                                    }
{                                                                            }
{****************************************************************************}

unit JclOtaExcDlgRepository;

interface

{$I jcl.inc}

uses
  SysUtils,
  ToolsAPI,
  JclBorlandTools,
  JclOtaUtils, JclOtaTemplates, JclOtaRepositoryUtils;

type
  TJclOtaExcDlgParams = class(TJclOtaTemplateParams)
  private
    FHookDll: Boolean;
    FFileName: string;
    FCodeDetails: Boolean;
    FModuleName: Boolean;
    FModuleOffset: Boolean;
    FDelayedTrace: Boolean;
    FFormName: string;
    FLogFile: Boolean;
    FLogFileName: string;
    FAddressOffset: Boolean;
    FVirtualAddress: Boolean;
    FActivePersonality: TJclBorPersonality;
    FLanguages: TJclBorPersonalities;
    FRawData: Boolean;
    FSendEMail: Boolean;
    FEMailAddress: string;
    FFormAncestor: string;
    FModalDialog: Boolean;
    FSizeableDialog: Boolean;
    FEMailSubject: string;
    FDesigner: TJclBorDesigner;
    FModuleList: Boolean;
    FOSInfo: Boolean;
    FActiveControls: Boolean;
    FStackList: Boolean;
    FAutoScrollBars: Boolean;
  public
    constructor Create; reintroduce;
  published
    // file options
    property Languages: TJclBorPersonalities read FLanguages write FLanguages;
    property ActivePersonality: TJclBorPersonality read FActivePersonality
      write FActivePersonality;
    property FileName: string read FFileName write FFileName;
    property FormName: string read FFormName write FFormName;
    property FormAncestor: string read FFormAncestor write FFormAncestor;
    property Designer: TJclBorDesigner read FDesigner write FDesigner;
    // form options
    property ModalDialog: Boolean read FModalDialog write FModalDialog;
    property SendEMail: Boolean read FSendEMail write FSendEMail;
    property EMailAddress: string read FEMailAddress write FEMailAddress;
    property EMailSubject: string read FEMailSubject write FEMailSubject;
    property SizeableDialog: Boolean read FSizeableDialog write FSizeableDialog;
    property AutoScrollBars: Boolean read FAutoScrollBars write FAutoScrollBars;
    // system options
    property DelayedTrace: Boolean read FDelayedTrace write FDelayedTrace;
    property HookDll: Boolean read FHookDll write FHookDll;
    property LogFile: Boolean read FLogFile write FLogFile;
    property LogFileName: string read FLogFileName write FLogFileName;
    property OSInfo: Boolean read FOSInfo write FOSInfo;
    property ModuleList: Boolean read FModuleList write FModuleList;
    property ActiveControls: Boolean read FActiveControls write FActiveControls;
    // trace options
    property StackList: Boolean read FStackList write FStackList;
    property RawData: Boolean read FRawData write FRawData;
    property ModuleName: Boolean read FModuleName write FModuleName;
    property ModuleOffset: Boolean read FModuleOffset write FModuleOffset;
    //property AddressOffset: Boolean read FAddressOffset write FAddressOffset;
    property CodeDetails: Boolean read FCodeDetails write FCodeDetails;
    property VirtualAddress: Boolean read FVirtualAddress write FVirtualAddress;
  end;
  
  TJclExcDlgExpert = class(TJclOtaRepositoryExpert)
  public
    procedure CreateExceptionDialog(const Params: TJclOtaExcDlgParams);
  end;

  TJclExcDlgDelphiExpert = class(TJclExcDlgExpert)
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure DoExecute(const Personality: TJclBorPersonality); override;
    function IsVisible(const Personality: TJclBorPersonality): Boolean; override;
  end;

  TJclExcDlgCBuilderExpert = class(TJclExcDlgExpert)
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure DoExecute(const Personality: TJclBorPersonality); override;
    function IsVisible(const Personality: TJclBorPersonality): Boolean; override;
  end;

implementation

{$R JclOtaExcDlgIcons.res}

uses
  Windows, Classes, Forms,
  JclStrings, JclFileUtils, JclSysInfo, JclResources, JclRegistry,
  JclOtaConsts, JclOtaResources, JclOtaExcDlgWizard;

//=== { TJclOtaExcDlgParams } ================================================

constructor TJclOtaExcDlgParams.Create;
begin
  inherited Create;
  
  FHookDll := True;
  FLanguage := bpUnknown;
  FLanguages := [bpUnknown];
  FFileName := '';
  FCodeDetails := True;
  FModuleName := True;
  FModuleOffset := False;
  FDelayedTrace := True;
  FFormName := 'ExceptionDialog';
  FFormAncestor := TForm.ClassName;
  FLogFile := False;
  FLogFileName := '';
  FAddressOffset := True;
  FVirtualAddress := False;
  FActivePersonality := bpUnknown;
  FRawData := False;
  FSendEMail := False;
  FEMailAddress := '';
  FEMailSubject := '';
  FModalDialog := True;
  FSizeableDialog := False;
  FDesigner := bdVCL;
  FModuleList := True;
  FOSInfo := True;
  FActiveControls := True;
  FStackList := True;
  FAutoScrollBars := True;
end;

//=== { TJclExcDlgExpert } ===================================================

procedure TJclExcDlgExpert.CreateExceptionDialog(
  const Params: TJclOtaExcDlgParams);
  function LoadTemplate(const FileName: string): string;
  var
    AFileStream: TFileStream;
    StreamLength: Int64;
  begin
    Result := '';
    if FileName <> '' then
    begin
      AFileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
      try
        StreamLength := AFileStream.Size;
        SetLength(Result, StreamLength);
        AFileStream.ReadBuffer(Result[1], StreamLength);
      finally
        AFileStream.Free;
      end;
    end;
  end;
const
  TemplateSubDir = 'experts\debug\dialog\';
  DelphiTemplate = 'ExceptDlg.Delphi32';
  BCBTemplate = 'ExceptDlg.CBuilder32';
var
  JclSettingsKeyName, TemplatePath,
  FormExtension, FormTemplate, FormContent, FormFileName,
  HeaderExtension, HeaderTemplate, HeaderContent, HeaderFileName,
  SourceExtension, SourceTemplate, SourceContent, SourceFileName: string;
  OTAServices: IOTAServices;
begin
  Supports(BorlandIDEServices,IOTAServices,OTAServices);
  if not Assigned(OTAServices) then
    raise EJclExpertException.CreateTrace(RsENoIDEServices);

  JclSettingsKeyName := StrEnsureSuffix('\', OTAServices.GetBaseRegistryKey) + RegJclKey;
  TemplatePath := PathAddSeparator(RegReadString(HKCU, JclSettingsKeyName, 'RootDir')) + TemplateSubDir;

  case Params.Language of
    bpDelphi32:
      begin
        FormExtension := JclBorDesignerFormExtension[Params.Designer];
        FormTemplate := TemplatePath + DelphiTemplate + FormExtension;
        HeaderExtension := '';
        HeaderTemplate := '';
        SourceExtension := SourceExtensionPAS;
        SourceTemplate := TemplatePath + DelphiTemplate + SourceExtension;
      end;
    bpBCBuilder32:
      begin
        FormExtension := JclBorDesignerFormExtension[Params.Designer];
        FormTemplate := TemplatePath + BCBTemplate + FormExtension;
        HeaderExtension := SourceExtensionH;
        HeaderTemplate := TemplatePath + BCBTemplate + HeaderExtension;
        SourceExtension := SourceExtensionCPP;
        SourceTemplate := TemplatePath + BCBTemplate + SourceExtension;
      end;
  else
      begin
        FormExtension := '';
        FormTemplate := '';
        HeaderExtension := '';
        HeaderTemplate := '';
        SourceExtension := '';
        SourceTemplate := '';
      end;
  end;

  FormTemplate := LoadTemplate(FormTemplate);
  HeaderTemplate := LoadTemplate(HeaderTemplate);
  SourceTemplate := LoadTemplate(SourceTemplate);

  FormContent := ApplyTemplate(FormTemplate, Params);
  HeaderContent := ApplyTemplate(HeaderTemplate, Params);
  SourceContent := ApplyTemplate(SourceTemplate, Params);

  if Params.FileName <> '' then
  begin
    FormFileName := ChangeFileExt(Params.FileName, FormExtension);
    HeaderFileName := ChangeFileExt(Params.FileName, HeaderExtension);
    SourceFileName := ChangeFileExt(Params.FileName, SourceExtension);
  end
  else
  begin
    FormFileName := '';
    HeaderFileName := '';
    SourceFileName := '';
  end;

  CreateForm(Params.FormAncestor, Params.FormName, FormFileName, FormContent, SourceFileName,
    SourceContent, HeaderFileName, HeaderContent);
end;

//=== { TJclRepositoryExpert } ===============================================

constructor TJclExcDlgDelphiExpert.Create;
begin
  inherited Create(RsRepositoryExcDlgDelphiName, RsRepositoryExcDlgDelphiDescription,
    RsAboutDialogTitle, RsRepositoryExcDlgPage, JclRepositoryCategoryDelphiFiles,
    JclDesignerVcl, JclDelphiPersonality, LoadIcon(FindResourceHInstance(HInstance), 'JclExcDlg'), ritForm);
end;

destructor TJclExcDlgDelphiExpert.Destroy;
begin
  inherited Destroy;
end;

procedure TJclExcDlgDelphiExpert.DoExecute(const Personality: TJclBorPersonality);
var
  AParams: TJclOtaExcDlgParams;
begin
  AParams := TJclOtaExcDlgParams.Create;
  try
    AParams.Languages := [bpDelphi32];
    AParams.Language := bpDelphi32;
    AParams.ActivePersonality := bpDelphi32;
    if ExcDlgWizard(AParams) and (AParams.Language <> bpUnknown) then
      CreateExceptionDialog(AParams);
  finally
    AParams.Free;
  end;
end;

function TJclExcDlgDelphiExpert.IsVisible(
  const Personality: TJclBorPersonality): Boolean;
begin
  Result := Personality = bpDelphi32;
end;

//=== { TJclExcDlgCBuilderExpert } ===========================================

constructor TJclExcDlgCBuilderExpert.Create;
begin
  inherited Create(RsRepositoryExcDlgCBuilderName, RsRepositoryExcDlgCBuilderDescription,
    RsAboutDialogTitle, RsRepositoryExcDlgPage, JclRepositoryCategoryCBuilderFiles,
    JclDesignerVcl, JclCBuilderPersonality, LoadIcon(FindResourceHInstance(HInstance), 'JclExcDlgCPP'), ritForm);
end;

destructor TJclExcDlgCBuilderExpert.Destroy;
begin
  inherited Destroy;
end;

procedure TJclExcDlgCBuilderExpert.DoExecute(
  const Personality: TJclBorPersonality);
var
  AParams: TJclOtaExcDlgParams;
begin
  AParams := TJclOtaExcDlgParams.Create;
  try
    AParams.Languages := [bpDelphi32];
    AParams.Language := bpDelphi32;
    AParams.ActivePersonality := bpBCBuilder32;
    if ExcDlgWizard(AParams) and (AParams.Language <> bpUnknown) then
      CreateExceptionDialog(AParams);
  finally
    AParams.Free;
  end;
end;

function TJclExcDlgCBuilderExpert.IsVisible(
  const Personality: TJclBorPersonality): Boolean;
begin
  Result := Personality = bpBCBuilder32;
end;

end.
