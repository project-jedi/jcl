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
{ The Original Code is JclOtaConsts.pas.                                                           }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Unit owner: Florent Ouchet                                                                       }
{ Last modified: October 19, 2005                                                                  }
{                                                                                                  }
{**************************************************************************************************}

unit JclOtaConsts;

interface

{$I jcl.inc}

const
  DelphiRootDirKeyValue  = 'RootDir';
  JediIDESubKey          = 'Jedi\JCL\IDE\';
  DelphiEnvironmentVar   = 'DELPHI';
  {$IFDEF COMPILER6_UP}
  EnvironmentVarsKey     = 'Environment Variables';
  {$ENDIF COMPÏLER6_UP}

  MapFileOptionName      = 'MapFile';
  OutputDirOptionName    = 'OutputDir';
  RuntimeOnlyOptionName  = 'RuntimeOnly';
  PkgDllDirOptionName    = 'PkgDllDir';
  BPLOutputDirOptionName = 'PackageDPLOutput';
  LIBPREFIXOptionName    = 'SOPrefix';
  LIBSUFFIXOptionName    = 'SOSuffix';

  JclDebugExpertRegKey = 'JclDebugExpert';
  JclDebugEnabledRegValue = 'JclDebugEnabled';

  FavoritesSectionName     = 'JclFavoriteFoldersExpert';
  PictDialogFolderItemName = 'PictureDialogPath';

  BorlandImagesPath        = 'Borland Shared\Images';

  FavDialogTemplateName      = 'FAVDLGTEMPLATE';
  OpenPictDialogTemplateName = 'DLGTEMPLATE';

  BPLExtension           = '.bpl';
  DPKExtension           = '.dpk';
  MAPExtension           = '.map';
  DRCExtension           = '.drc';
  DPRExtention           = '.dpr';

  // Jcl Uses Expert
  SUsesExpertSubkey = 'JclUsesExpert';
  SIniIdentifierLists = 'IdentifierLists';
  SRegDebugLibPath = 'Debug Library';
  SRegLibPath = 'Library';
  SRegWizardActive = 'Uses Wizard Active';
  SRegWizardConfirm = 'Uses Wizard Confirm';
  SRegWizardIniFile = 'Configuration File';

  SJCLUsesWizardID = 'JEDI.JCLUsesWizard'; // wizard ID
  SJCLUsesWizardName = 'JCL Uses Wizard'; // wizard name

implementation

// History:

// $Log$
// Revision 1.2  2005/10/22 14:24:18  marquardt
// more expert integration and cleanup
//
// Revision 1.1  2005/10/21 12:24:41  marquardt
// experts reorganized with new directory common
//

end.
