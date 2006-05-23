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
{ Last modified: $Date$                                                      }
{                                                                                                  }
{**************************************************************************************************}

unit JediInstall;

interface

uses
  SysUtils, Classes,
  JclSysUtils, JclBorlandTools;

type
  TJediInstallOption =
    (
      ioTarget,
      ioJCL,
      ioJclDefThreadSafe,
      ioJclDefDropObsoleteCode,
      ioJclDefMathPrecSingle,
      ioJclDefMathPrecDouble,
      ioJclDefMathPrecExtended,
      ioJclMapCreate,
      ioJclMapLink,
      ioJclMapDelete,
      ioJclEnv,
      ioJclEnvLibPath,
      ioJclEnvBrowsingPath,
      ioJclEnvDebugDCUPath,
      ioJclMake,
      ioJclMakeRelease,
      ioJclMakeReleaseVClx,
      ioJclMakeDebug,
      ioJclMakeDebugVClx,
      ioJclCopyHppFiles,
      ioJclDualPackages,         // must be before ioJclPackages
      ioJclPackages,
      ioJclExpertsDesignPackages,
      ioJclExpertsDLL,
      ioJclExperts,
      ioJclExpertDebug,
      ioJclExpertAnalyzer,
      ioJclExpertFavorite,
      ioJclExpertRepository,
      ioJclExpertThreadNames,
      ioJclExpertUses,
      ioJclExpertSimdView,
      ioJclExpertVersionControl,
      ioJclCopyPackagesHppFiles,
      ioJclExcDialog,
      ioJclExcDialogVCL,
      ioJclExcDialogVCLSnd,
      ioJclExcDialogCLX,
      ioJclHelp,
      ioJclHelpHlp,
      ioJclHelpChm,
      ioJclHelpHxS,
      ioJclHelpHxSPlugin,
      ioJclMakeDemos              // = ioJclLast, see below.
    );

  TJediInstallGUIOption =
    (
      goExpandable,
      goRadioButton,
      goNoAutoCheck,            // do not auto-check when the parent node gets checked
      goStandaloneParent,       // do not auto-uncheck when all child nodes are unchecked
      goChecked
    );
  TJediInstallGUIOptions = set of TJediInstallGUIOption;

  TInstallOptionData = record
    Parent: TJediInstallOption;
    Caption: string;
    Hint: string;
  end;

const
  ioJclLast = ioJclMakeDemos;
//  Significand: array[TJclBorRadToolKind] of Char = ('D', 'C', 'D');
  {$IFDEF KEEP_DEPRECATED}
  Prefixes: array[brDelphi..brCppBuilder] of Char = ('D', 'C');
  {$ENDIF KEEP_DEPRECATED}

type
  TDialogType = (dtWarning, dtError, dtInformation, dtConfirmation);
  TDialogResponse = (drYes, drNo, drOK, drCancel);
  TDialogResponses = set of TDialogResponse;

  TInstallationEvent = procedure (Installation: TJclBorRADToolInstallation) of object;
  TInstallationProgressEvent = procedure (Percent: Cardinal) of object;

  EJediInstallInitFailure = class(Exception);

  IJediInstallTool = interface
    ['{85408C67-92B5-42D0-84E0-D30201C0400D}']
    function Dialog(const Text: string; DialogType: TDialogType = dtInformation;
      Options: TDialogResponses = [drOK]): TDialogResponse;
    function GetBPLPath(Installation: TJclBorRADToolInstallation): string;
    function GetDCPPath(Installation: TJclBorRADToolInstallation): string;
    function FeatureChecked(FeatureID: Cardinal; Installation: TJclBorRADToolInstallation): Boolean;
    function GetBorRADToolInstallations: TJclBorRADToolInstallations;
    function OptionGUI(Installation: TJclBorRADToolInstallation): TObject;
    function GUIAddOption(GUI, Parent: TObject; Option: TJediInstallOption; const Text: string;
      GUIOptions: TJediInstallGUIOptions): TObject;
    procedure SetBPLPath(Installation: TJclBorRADToolInstallation; const Value: string);
    procedure SetDCPPath(Installation: TJclBorRADToolInstallation; const Value: string);
    procedure SetReadme(const FileName: string);
    procedure UpdateInfo(Installation: TJclBorRADToolInstallation; const InfoText: string);
    procedure UpdateStatus(const Text: string);
    procedure WriteInstallLog(Installation: TJclBorRADToolInstallation; const Text: string);
    property BorRADToolInstallations: TJclBorRADToolInstallations read GetBorRADToolInstallations;
    property BPLPath[Installation: TJclBorRADToolInstallation]: string read GetBPLPath write SetBPLPath;
    property DCPPath[Installation: TJclBorRADToolInstallation]: string read GetDCPPath write SetDCPPath;
    property Readme: string write SetReadme;
  end;

  IJediInstall = interface
    ['{2C4A8C85-18BB-4A67-B37F-806C60632569}']
    function FeatureInfoFileName(FeatureID: Cardinal): string;
    function GetHint(Option: TJediInstallOption): string;
    function InitInformation(const ApplicationFileName: string): Boolean;
    function Install: Boolean;
    function Uninstall: Boolean;
    function ReadmeFileName: string;
    procedure SetTool(const Value: IJediInstallTool);
    procedure SetOnProgress(Value: TInstallationProgressEvent);
    function Supports(Installation: TJclBorRADToolInstallation): Boolean;
    procedure SetOnWriteLog(Installation: TJclBorRADToolInstallation; Value: TTextHandler);
    procedure SetOnStarting(Value: TInstallationEvent);
    procedure SetOnEnding(Value: TInstallationEvent); // OnEnding called on success only
    function GetVersion: string;
  end;

function OptionToStr(const Option: TJediInstallOption): string;

resourcestring
  RsCantFindFiles   = 'Can not find installation files, check your installation.';
  RsCloseRADTool    = 'Please close all running instances of Delphi/C++Builder IDE before the installation.';
  RsConfirmInstall  = 'Are you sure to install all selected features?';
  RsInstallSuccess  = 'Installation finished';
  RsInstallFailure  = 'Installation failed.'#10'Check compiler output for details.';
  RsNoInstall       = 'There is no Delphi/C++Builder installation on this machine. Installer will close.';
  RsUpdateNeeded    = 'You should install latest Update Pack #%d for %s.'#13#10 +
                      'Would you like to open Borland support web page?';

implementation

uses
  TypInfo;

function OptionToStr(const Option: TJediInstallOption): string;
begin
  Result := GetEnumName(TypeInfo(TJediInstallOption), Ord(Option));
end;

// History:

// $Log$
// Revision 1.21  2006/02/28 16:30:20  ahuser
// Jedi Registry Information record
//
// Revision 1.20  2006/02/26 18:31:42  outchy
// Chm help can now be removed
// Alpha version for the help 2.0
//
// Revision 1.19  2006/01/15 00:51:22  outchy
// cvs support in version control expert
// version control expert integration in the installer
//
// Revision 1.18  2005/12/26 18:03:41  outchy
// Enhanced bds support (including C#1 and D8)
// Introduction of dll experts
// Project types in templates
//
// Revision 1.17  2005/12/04 10:10:57  obones
// Borland Developer Studio 2006 support
//
// Revision 1.16  2005/11/10 22:16:31  outchy
// Added creation/link/deletion of MAP files for packages.
//
// Revision 1.15  2005/10/28 04:38:53  rrossmair
// - fixes related to package uninstallation, and more
//
// Revision 1.14  2005/08/01 04:52:03  rrossmair
// - (basic) support for compilation of examples
//
// Revision 1.13  2005/07/28 21:57:49  outchy
// JEDI Installer can now install design-time packages for C++Builder 5 and 6
//
// Revision 1.12  2005/03/05 06:33:17  rrossmair
// - support for some conditional defines added.
//
// Revision 1.11  2005/02/28 20:19:07  uschuster
// changes for Uses wizard
//
// Revision 1.10  2005/02/03 05:22:17  rrossmair
// - more uninstall support (still unfinished)
//
// Revision 1.9  2004/11/14 05:55:55  rrossmair
// - installer refactoring (continued)
//
// Revision 1.8  2004/11/09 07:51:37  rrossmair
// - installer refactoring (incomplete)
//

end.
