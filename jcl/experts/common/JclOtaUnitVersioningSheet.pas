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
{ The Original Code is JclOtaUnitVersioningSheet.pas.                                              }
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

unit JclOtaUnitVersioningSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls;

type
  TJclOtaUnitVersioningFrame = class(TFrame)
    MemoUnitVersioning: TMemo;
  private
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  ActnList, Menus,
  ToolsApi,
  JclFileUtils, JclUnitVersioning, JclUnitVersioningProviders,
  JclOtaConsts, JclOtaResources, JclOtaUtils;

constructor TJclOtaUnitVersioningFrame.Create(AOwner: TComponent);
var
  UnitVersioning: TUnitVersioning;
  UnitVersioningModule: TUnitVersioningModule;
  UnitVersion: TUnitVersion;
  I, J: Integer;
  LongFileName: string;
begin
  inherited Create(AOwner);
  UnitVersioning := GetUnitVersioning;
  UnitVersioning.RegisterProvider(TJclDefaultUnitVersioningProvider);
  for I := 0 to Pred(UnitVersioning.ModuleCount) do
    UnitVersioning.LoadModuleUnitVersioningInfo(UnitVersioning.Modules[I].Instance);
  MemoUnitVersioning.Lines.BeginUpdate;
  try
    MemoUnitVersioning.Lines.Clear;
    for I := 0 to Pred(UnitVersioning.ModuleCount) do
    begin
      UnitVersioningModule := UnitVersioning.Modules[I];
      MemoUnitVersioning.Lines.Add(Format('%s [%d units]', [GetModulePath(UnitVersioningModule.Instance), UnitVersioningModule.Count]));
      for J := 0 to Pred(UnitVersioningModule.Count) do
      begin
        UnitVersion := UnitVersioningModule.Items[J];
        LongFileName := UnitVersion.LogPath;
        if LongFileName <> '' then
          LongFileName := PathAddSeparator(LongFileName);
        LongFileName := LongFileName + UnitVersion.RCSfile;
        MemoUnitVersioning.Lines.Add(Format('%s  %s  %s', [LongFileName, UnitVersion.Revision, UnitVersion.Date]));
      end;
    end;
  finally
    MemoUnitVersioning.Lines.EndUpdate;
  end;
end;

end.
