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

{$IFDEF DELPHI}
{$DEFINE DELPHIEXCDLG}
{$ENDIF DELPHI}

{$IFDEF BCB}
{$DEFINE CBUILDEREXCDLG}
{$ENDIF BCB}

{$IFDEF COMPILER10_UP}
{$DEFINE CBUILDEREXCDLG}
{$ENDIF COMPILER10_UP}

uses
  SysUtils, Classes,
  ToolsAPI,
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

implementation

uses
  Windows,
  JclStrings, JclFileUtils, JclRegistry,
  JclOtaResources, JclOtaConsts, JclOtaTemplates, JclOtaExcDlgWizard;


function IsPersonalityLoaded(const BorlandIDEServices: IBorlandIDEServices;
  const PersonalityName: string): Boolean;
{$IFDEF BDS}
var
  PersonalityServices: IOTAPersonalityServices;
  Index: Integer;
begin
  Supports(BorlandIDEServices, IOTAPersonalityServices, PersonalityServices);
  if not Assigned(PersonalityServices) then
    raise EJclExpertException.CreateTrace(RsENoPersonalityServices);

  Result := False;

  for Index := 0 to PersonalityServices.PersonalityCount - 1 do
    if SameText(PersonalityServices.Personalities[Index], PersonalityName) then
    begin
      Result := True;
      Break;
    end;
end;
{$ELSE BDS}
begin
  Result := True;
end;
{$ENDIF BDS}


procedure Register;
begin
  try
    {$IFDEF DELPHI}
    if IsPersonalityLoaded(BorlandIDEServices, JclDelphiPersonality) then
      RegisterPackageWizard(TJclExcDlgDelphiExpert.Create);
    {$ENDIF DELPHI}
    {$IFDEF BCB}
    if IsPersonalityLoaded(BorlandIDEServices, JclCBuilderPersonality) then
      RegisterPackageWizard(TJclExcDlgCBuilderExpert.Create);
    {$ENDIF BCB}
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

var
  {$IFDEF DELPHI}
  JCLDelphiWizardIndex: Integer = -1;
  {$ENDIF DELPHI}
  {$IFDEF BCB}
  JclCBuilderWizardIndex: Integer = -1;
  {$ENDIF BCB}

procedure JclWizardTerminate;
var
  OTAWizardServices: IOTAWizardServices;
begin
  try
    Supports(BorlandIDEServices, IOTAWizardServices, OTAWizardServices);
    if not Assigned(OTAWizardServices) then
      raise EJclExpertException.CreateTrace(RsENoWizardServices);

    {$IFDEF DELPHI}
    if JCLDelphiWizardIndex <> -1 then
      OTAWizardServices.RemoveWizard(JCLDelphiWizardIndex);
    {$ENDIF DELPHI}

    {$IFDEF BCB}
    if JclCBuilderWizardIndex <> -1 then
      OTAWizardServices.RemoveWizard(JclCBuilderWizardIndex);
    {$ENDIF BCB}
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

    Supports(BorlandIDEServices, IOTAWizardServices, OTAWizardServices);
    if not Assigned(OTAWizardServices) then
      raise EJclExpertException.CreateTrace(RsENoWizardServices);

    {$IFDEF DELPHI}
    //if IsPersonalityLoaded(BorlandIDEServices, JclDelphiPersonality) then
    //  JCLDelphiWizardIndex := OTAWizardServices.AddWizard(TJclExcDlgDelphiExpert.Create);
    {$ENDIF DELPHI}
    {$IFDEF BCB}
    //if IsPersonalityLoaded(BorlandIDEServices, JclCBuilderPersonality) then
    //  JclCBuilderWizardIndex := OTAWizardServices.AddWizard(TJclExcDlgCBuilderExpert.Create);
    {$ENDIF BCB}
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
  BCBTemplate    = 'ExceptDlg.CBuilder32';
var
  JclSettingsKeyName, TemplatePath,
  FormExtension, FormTemplate, FormContent, FormFileName,
  HeaderExtension, HeaderTemplate, HeaderContent, HeaderFileName,
  SourceExtension, SourceTemplate, SourceContent, SourceFileName: string;
  OTAServices: IOTAServices;
begin
  Supports(BorlandIDEServices, IOTAServices, OTAServices);
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
