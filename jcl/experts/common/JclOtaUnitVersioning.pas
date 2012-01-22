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
{ The Original Code is JclOtaUtils.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones.                                        }
{ Portions created by Petr Vones are Copyright (C) of Petr Vones.                                  }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclOtaUnitVersioning;

interface

{$I jcl.inc}
{$I crossplatform.inc}

uses
  SysUtils, Classes, Windows,
  Controls,
  JclBase,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclOTAUtils;

type
  TJclOTAUnitVersioningExpert = class(TJclOTAExpert)
  private
    FUnitVersioningSheet: TControl;
  public
    constructor Create; reintroduce;
    { IJclOTAOptionsCallback }
    procedure AddConfigurationPages(AddPageFunc: TJclOTAAddPageFunc); override;
    procedure ConfigurationClosed(AControl: TControl; SaveChanges: Boolean); override;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  Forms,
  JclOtaConsts, JclOtaResources,
  JclOtaUnitVersioningSheet;

//=== { TJclOTAUnitVersioningExpert } ========================================

constructor TJclOTAUnitVersioningExpert.Create;
begin
  inherited Create(JclUnitVersioningExpertName);
end;

procedure TJclOTAUnitVersioningExpert.AddConfigurationPages(
  AddPageFunc: TJclOTAAddPageFunc);
begin
  // AddPageFunc uses '\' as a separator in PageName to build a tree
  if not Assigned(FUnitVersioningSheet) then
  begin
    FUnitVersioningSheet := TJclOtaUnitVersioningFrame.Create(Application);
    AddPageFunc(FUnitVersioningSheet, LoadResString(@RsUnitVersioningSheet), Self);
  end;
  // override to customize
end;

procedure TJclOTAUnitVersioningExpert.ConfigurationClosed(AControl: TControl;
  SaveChanges: Boolean);
begin
  if Assigned(AControl) and (AControl = FUnitVersioningSheet) then
    FreeAndNil(FUnitVersioningSheet)
  else
    inherited ConfigurationClosed(AControl, SaveChanges);
  // override to customize
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
