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
{ The Original Code is JclOtaRepositoryReg.pas.                                                    }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet                                     }
{         <outchy att users dott sourceforge dott net>                                             }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet. All rights reserved.     }
{                                                                                                  }
{ Contributors:                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclOtaRepositoryReg;

interface

{$I jcl.inc}

{$IFDEF DELPHI6_UP}
{$DEFINE DELPHIEXCDLG}
{$ENDIF DELPHI6_UP}

{$IFDEF BCB6_UP}
{$DEFINE CBUILDEREXCDLG}
{$ENDIF BCB6_UP}

{$IFDEF COMPILER10_UP}
{$DEFINE DELPHIEXCDLG}
{$DEFINE CBUILDEREXCDLG}
{$ENDIF COMPILER10_UP}

uses
  SysUtils, Classes,
  ToolsAPI,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBorlandTools,
  JclOtaUtils, JclOtaRepositoryUtils, JclOtaExcDlgRepository;

type
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

// design package entry point
procedure Register;

// expert DLL entry point
function JCLWizardInit(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var TerminateProc: TWizardTerminateProc): Boolean; stdcall;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\repository';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  Windows,
  JclStrings, JclFileUtils, JclRegistry,
  JclOtaResources, JclOtaConsts, JclOtaTemplates, JclOtaExcDlgWizard;

procedure Register;
begin
  try
    {$IFDEF DELPHIEXCDLG}
    if TJclOTAExpertBase.IsPersonalityLoaded(JclDelphiPersonality) then
      RegisterPackageWizard(TJclExcDlgDelphiExpert.Create);
    {$ENDIF DELPHIEXCDLG}
    {$IFDEF CBUILDEREXCDLG}
    if TJclOTAExpertBase.IsPersonalityLoaded(JclCBuilderPersonality) then
      RegisterPackageWizard(TJclExcDlgCBuilderExpert.Create);
    {$ENDIF CBUILDEREXCDLG}
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

var
  {$IFDEF DELPHIEXCDLG}
  JCLDelphiWizardIndex: Integer = -1;
  {$ENDIF DELPHIEXCDLG}
  {$IFDEF CBUILDEREXCDLG}
  JclCBuilderWizardIndex: Integer = -1;
  {$ENDIF CBUILDEREXCDLG}

procedure JclWizardTerminate;
var
  OTAWizardServices: IOTAWizardServices;
begin
  try
    OTAWizardServices := TJclOTAExpertBase.GetOTAWizardServices;
    
    {$IFDEF DELPHIEXCDLG}
    if JCLDelphiWizardIndex <> -1 then
      OTAWizardServices.RemoveWizard(JCLDelphiWizardIndex);
    {$ENDIF DELPHIEXCDLG}

    {$IFDEF CBUILDEREXCDLG}
    if JclCBuilderWizardIndex <> -1 then
      OTAWizardServices.RemoveWizard(JclCBuilderWizardIndex);
    {$ENDIF CBUILDEREXCDLG}
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

function JCLWizardInit(const BorlandIDEServices: IBorlandIDEServices;
    RegisterProc: TWizardRegisterProc;
    var TerminateProc: TWizardTerminateProc): Boolean stdcall;
var
  OTAWizardServices: IOTAWizardServices;
begin
  try
    TerminateProc := JclWizardTerminate;

    OTAWizardServices := TJclOTAExpertBase.GetOTAWizardServices;

    {$IFDEF DELPHIEXCDLG}
    //if IsPersonalityLoaded(BorlandIDEServices, JclDelphiPersonality) then
    //  JCLDelphiWizardIndex := OTAWizardServices.AddWizard(TJclExcDlgDelphiExpert.Create);
    {$ENDIF DELPHIEXCDLG}
    {$IFDEF CBUILDEREXCDLG}
    //if IsPersonalityLoaded(BorlandIDEServices, JclCBuilderPersonality) then
    //  JclCBuilderWizardIndex := OTAWizardServices.AddWizard(TJclExcDlgCBuilderExpert.Create);
    {$ENDIF CBUILDEREXCDLG}
    Result := True;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      Result := False;
    end;
  end;
end;

//=== { TJclExcDlgExpert } ===================================================

procedure TJclExcDlgExpert.CreateExceptionDialog(
  const Params: TJclOtaExcDlgParams);
  function LoadTemplate(const FileName: string): string;
  var
    AFileStream: TFileStream;
    StreamLength: Int64;
    AnsiResult: AnsiString;
  begin
    AnsiResult := '';
    if FileName <> '' then
    begin
      AFileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
      try
        StreamLength := AFileStream.Size;
        SetLength(AnsiResult, StreamLength);
        AFileStream.ReadBuffer(AnsiResult[1], StreamLength);
      finally
        AFileStream.Free;
      end;
    end;
    Result := string(AnsiResult);
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
  OTAServices := GetOTAServices;
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
  inherited Create(LoadResString(@RsRepositoryExcDlgDelphiName), LoadResString(@RsRepositoryExcDlgDelphiDescription),
    LoadResString(@RsAboutDialogTitle), LoadResString(@RsRepositoryExcDlgPage), JclRepositoryCategoryDelphiFiles,
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
  inherited Create(LoadResString(@RsRepositoryExcDlgCBuilderName), LoadResString(@RsRepositoryExcDlgCBuilderDescription),
    LoadResString(@RsAboutDialogTitle), LoadResString(@RsRepositoryExcDlgPage), JclRepositoryCategoryCBuilderFiles,
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

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
