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
{                                                                                                  }
{**************************************************************************************************}

// $Id$

{$IFNDEF Develop}unit {$IFDEF VisualCLX}QJediInstallIntf{$ELSE}JediInstallIntf{$ENDIF};{$ENDIF}

interface

uses
  {$IFDEF VisualCLX}QComCtrls, QDialogs,{$ELSE}ComCtrls, Dialogs,{$ENDIF}
  BorRADToolInstall;

const
  // Feature masks
  FID_Product              = $1F000000;
  FID_IsProduct            = $00FFFFFF;
  FID_Category             = $00FF0000;
  FID_IsCategory           = $0000FFFF;
  FID_Level2               = $0000FF00;
  FID_IsLevel2             = $000000FF;
  FID_Level3               = $000000FF;
  FID_Expandable           = $20000000;
  FID_StandaloneParent     = $40000000; // do not auto-uncheck when all child nodes are unchecked
  FID_Checked              = $80000000;
  FID_NumberMask           = $7FFFFFFF;

  // Icon indexes
  IcoProduct               = 0;
  IcoLevel1                = 1;
  IcoChecked               = 2;
  IcoUnchecked             = 3;

type
  IJediInstallTool = interface
    ['{CB8A2F3A-9E7C-4646-9E1F-60102A8F957D}']
    function BPLPath(Installation: TJclBorRADToolInstallation): string;
    function DCPPath(Installation: TJclBorRADToolInstallation): string;
    function FeatureChecked(FeatureID: Cardinal; Installation: TJclBorRADToolInstallation): Boolean;
    function GetBorRADToolInstallations: TJclBorRADToolInstallations;
    function MessageBox(const Text: string; DlgType: TMsgDlgType = mtInformation;
      Buttons: TMsgDlgButtons = [mbOK]{$IFDEF VisualCLX}; DefaultBtn: TMsgDlgBtn = mbNone{$ENDIF}): Integer;
    procedure UpdateInfo(Installation: TJclBorRADToolInstallation; const InfoText: string);
    procedure UpdateStatus(const Text: string);
    procedure WriteInstallLog(const Text: string);
    property BorRADToolInstallations: TJclBorRADToolInstallations read GetBorRADToolInstallations;
  end;

  IJediInstall = interface
    ['{BE0A7968-9003-40DD-99F0-250CAC8B2D85}']
    function InitInformation(const ApplicationFileName: string): Boolean;
    function Install: Boolean;
    function InstallFor(Installation: TJclBorRADToolInstallation): Boolean;
    function PopulateTreeView(Installation: TJclBorRADToolInstallation; Nodes: TTreeNodes): Boolean;
    function SelectedNodeCollapsing(Node: TTreeNode): Boolean;
    procedure SelectedNodeChanged(Node: TTreeNode);
    procedure SetTool(const Value: IJediInstallTool);
  end;

implementation

end.
