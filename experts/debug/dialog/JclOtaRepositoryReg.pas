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
{ The Original Code is JclOtaRepositoryReg.pas.                              }
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
  ToolsAPI;

// design package entry point
procedure Register;

// expert DLL entry point
function JCLWizardInit(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var TerminateProc: TWizardTerminateProc): Boolean; stdcall;

implementation

uses
  SysUtils,
  JclOtaUtils, JclOtaResources, JclOtaConsts,
  JclOtaExcDlgRepository;


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

end.
