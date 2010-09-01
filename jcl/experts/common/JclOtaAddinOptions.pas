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
{ The Original Code is JclOtaAddinOptions.pas.                                                     }
{                                                                                                  }
{ The Initial Developer of the Original Code is Uwe Schuster.                                      }
{ Portions created by Uwe Schuster are Copyright (C) 2010 Uwe Schuster. All rights reserved.       }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Uwe Schuster (uschuster)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclOtaAddinOptions;

{$I jcl.inc}

interface

uses
  Forms, ToolsAPI,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclOtaActionConfigureSheet, JclOtaUnitVersioningSheet, JclOtaEmptyAddinOptionsFrame;

function JclGetAddinOptionsCaption(const ACaption: string): string;
procedure JclRegisterCommonAddinOptions;
procedure JclUnregisterCommonAddinOptions;

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
  JclStrings, JclOtaResources;

function JclGetAddinOptionsCaption(const ACaption: string): string;
begin
  Result := RsProjectJEDIAddinOptionsCaptionPrefix + StrReplaceChar(ACaption, '\', '.');
end;

type
  TJclActionAddinOptions = class(TInterfacedObject, INTAAddinOptions)
  private
    FFrame: TJclOtaActionConfigureFrame;
  public
    procedure DialogClosed(Accepted: Boolean);
    procedure FrameCreated(AFrame: TCustomFrame);
    function GetArea: string;
    function GetCaption: string;
    function GetFrameClass: TCustomFrameClass;
    function ValidateContents: Boolean;
    function GetHelpContext: Integer;
    function IncludeInIDEInsight: Boolean;
  end;

  TJclUnitVersioningAddinOptions = class(TInterfacedObject, INTAAddinOptions)
  public
    procedure DialogClosed(Accepted: Boolean);
    procedure FrameCreated(AFrame: TCustomFrame);
    function GetArea: string;
    function GetCaption: string;
    function GetFrameClass: TCustomFrameClass;
    function ValidateContents: Boolean;
    function GetHelpContext: Integer;
    function IncludeInIDEInsight: Boolean;
  end;

  TJclEmptyPageAddinOptions = class(TInterfacedObject, INTAAddinOptions)
  private
    FCaption: string;
    FTitle: string;
  public
    constructor Create(const ACaption, ATitle: string);
    procedure DialogClosed(Accepted: Boolean);
    procedure FrameCreated(AFrame: TCustomFrame);
    function GetArea: string;
    function GetCaption: string;
    function GetFrameClass: TCustomFrameClass;
    function ValidateContents: Boolean;
    function GetHelpContext: Integer;
    function IncludeInIDEInsight: Boolean;
  end;

//=== { TJclActionAddinOptions } =============================================

procedure TJclActionAddinOptions.DialogClosed(Accepted: Boolean);
begin
  if Accepted then
    FFrame.SaveChanges;
end;

procedure TJclActionAddinOptions.FrameCreated(AFrame: TCustomFrame);
begin
  FFrame := TJclOtaActionConfigureFrame(AFrame);
end;

function TJclActionAddinOptions.GetArea: string;
begin
  Result := '';
end;

function TJclActionAddinOptions.GetCaption: string;
begin
  Result := JclGetAddinOptionsCaption(RsActionSheet);
end;

function TJclActionAddinOptions.GetFrameClass: TCustomFrameClass;
begin
  Result := TJclOtaActionConfigureFrame;
end;

function TJclActionAddinOptions.GetHelpContext: Integer;
begin
  Result := 0;
end;

function TJclActionAddinOptions.IncludeInIDEInsight: Boolean;
begin
  Result := True;
end;

function TJclActionAddinOptions.ValidateContents: Boolean;
begin
  Result := True;
end;

//=== { TJclUnitVersioningAddinOptions } =====================================

procedure TJclUnitVersioningAddinOptions.DialogClosed(Accepted: Boolean);
begin
  //
end;

procedure TJclUnitVersioningAddinOptions.FrameCreated(AFrame: TCustomFrame);
begin
  //
end;

function TJclUnitVersioningAddinOptions.GetArea: string;
begin
  Result := '';
end;

function TJclUnitVersioningAddinOptions.GetCaption: string;
begin
  Result := JclGetAddinOptionsCaption(RsUnitVersioningSheet);
end;

function TJclUnitVersioningAddinOptions.GetFrameClass: TCustomFrameClass;
begin
  Result := TJclOtaUnitVersioningFrame;
end;

function TJclUnitVersioningAddinOptions.GetHelpContext: Integer;
begin
  Result := 0;
end;

function TJclUnitVersioningAddinOptions.IncludeInIDEInsight: Boolean;
begin
  Result := True;
end;

function TJclUnitVersioningAddinOptions.ValidateContents: Boolean;
begin
  Result := True;
end;

//=== { TJclEmptyPageAddinOptions } ==========================================

constructor TJclEmptyPageAddinOptions.Create(const ACaption, ATitle: string);
begin
  inherited Create;
  FCaption := ACaption;
  FTitle := ATitle;
end;

procedure TJclEmptyPageAddinOptions.DialogClosed(Accepted: Boolean);
begin
//
end;

procedure TJclEmptyPageAddinOptions.FrameCreated(AFrame: TCustomFrame);
begin
  TJclOtaEmptyAddinOptionsFrm(AFrame).lbTitle.Caption := FTitle;
end;

function TJclEmptyPageAddinOptions.GetArea: string;
begin
  Result := '';
end;

function TJclEmptyPageAddinOptions.GetCaption: string;
begin
  Result := FCaption;
end;

function TJclEmptyPageAddinOptions.GetFrameClass: TCustomFrameClass;
begin
  Result := TJclOtaEmptyAddinOptionsFrm;
end;

function TJclEmptyPageAddinOptions.GetHelpContext: Integer;
begin
  Result := 0;
end;

function TJclEmptyPageAddinOptions.IncludeInIDEInsight: Boolean;
begin
  Result := False;
end;

function TJclEmptyPageAddinOptions.ValidateContents: Boolean;
begin
  Result := True;
end;

var
  ActionAddinOptions: TJclActionAddinOptions = nil;
  UnitVersioningAddinOptions: TJclUnitVersioningAddinOptions = nil;
  ProjectJEDIEmptyAddinOptions: TJclEmptyPageAddinOptions = nil;
  ProjectJEDIJclEmptyAddinOptions: TJclEmptyPageAddinOptions = nil;
  ProjectJEDIJclCommonEmptyAddinOptions: TJclEmptyPageAddinOptions = nil;

procedure JclRegisterCommonAddinOptions;
begin
  if not Assigned(ActionAddinOptions) then
  begin
    ActionAddinOptions := TJclActionAddinOptions.Create;
    (BorlandIDEServices as INTAEnvironmentOptionsServices).RegisterAddInOptions(ActionAddinOptions);
  end;
  if not Assigned(UnitVersioningAddinOptions) then
  begin
    UnitVersioningAddinOptions := TJclUnitVersioningAddinOptions.Create;
    (BorlandIDEServices as INTAEnvironmentOptionsServices).RegisterAddInOptions(UnitVersioningAddinOptions);
  end;
  if not Assigned(ProjectJEDIEmptyAddinOptions) then
  begin
    ProjectJEDIEmptyAddinOptions := TJclEmptyPageAddinOptions.Create(RsProjectJEDIAddinOptionsCaption,
      RsProjectJEDIAddinOptionsTitle);
    (BorlandIDEServices as INTAEnvironmentOptionsServices).RegisterAddInOptions(ProjectJEDIEmptyAddinOptions);
  end;
  if not Assigned(ProjectJEDIJclEmptyAddinOptions) then
  begin
    ProjectJEDIJclEmptyAddinOptions := TJclEmptyPageAddinOptions.Create(RsProjectJEDIJclAddinOptionsCaption,
       RsProjectJEDIJclAddinOptionsTitle);
    (BorlandIDEServices as INTAEnvironmentOptionsServices).RegisterAddInOptions(ProjectJEDIJclEmptyAddinOptions);
  end;
  if not Assigned(ProjectJEDIJclCommonEmptyAddinOptions) then
  begin
    ProjectJEDIJclCommonEmptyAddinOptions := TJclEmptyPageAddinOptions.Create(RsProjectJEDIJclCommonAddinOptionsCaption,
      RsProjectJEDIJclCommonAddinOptionsTitle);
    (BorlandIDEServices as INTAEnvironmentOptionsServices).RegisterAddInOptions(ProjectJEDIJclCommonEmptyAddinOptions);
  end;
end;

procedure JclUnregisterCommonAddinOptions;
begin
  if Assigned(ActionAddinOptions) then
  begin
    (BorlandIDEServices as INTAEnvironmentOptionsServices).UnregisterAddInOptions(ActionAddinOptions);
    ActionAddinOptions := nil;
  end;
  if Assigned(UnitVersioningAddinOptions) then
  begin
    (BorlandIDEServices as INTAEnvironmentOptionsServices).UnregisterAddInOptions(UnitVersioningAddinOptions);
    UnitVersioningAddinOptions := nil;
  end;
  if Assigned(ProjectJEDIEmptyAddinOptions) then
  begin
    (BorlandIDEServices as INTAEnvironmentOptionsServices).UnregisterAddInOptions(ProjectJEDIEmptyAddinOptions);
    ProjectJEDIEmptyAddinOptions := nil;
  end;
  if Assigned(ProjectJEDIJclEmptyAddinOptions) then
  begin
    (BorlandIDEServices as INTAEnvironmentOptionsServices).UnregisterAddInOptions(ProjectJEDIJclEmptyAddinOptions);
    ProjectJEDIJclEmptyAddinOptions := nil;
  end;
  if Assigned(ProjectJEDIJclCommonEmptyAddinOptions) then
  begin
    (BorlandIDEServices as INTAEnvironmentOptionsServices).UnregisterAddInOptions(ProjectJEDIJclCommonEmptyAddinOptions);
    ProjectJEDIJclCommonEmptyAddinOptions := nil;
  end;
end;

end.
