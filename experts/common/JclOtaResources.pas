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
{ The Original Code is JclOtaResources.pas.                                                        }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones.                                        }
{ Portions created by Petr Vones are Copyright (C) of Petr Vones.                                  }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Unit owner: Florent Ouchet                                                                       }
{ Last modified: $Date$                                                      }
{                                                                                                  }
{**************************************************************************************************}

unit JclOtaResources;

interface

{$I jcl.inc}

uses JclBase;

//=== JclOtaUtils.pas ========================================================
resourcestring
  RsENoIDEServices = 'Unable to get Borland IDE Services';
  RsENoNTAServices = 'Unable to get Borland NTA Services';
  RsENoSplashServices = 'Unable to get Borland Splash Services';
  RsENoAboutServices = 'Unable to get Borland About Services';
  RsENoModuleServices = 'Unable to get Borland Module Services';
  RsENoWizardServices = 'Unable to get Borland Wizard Services';
  RsENoPackageServices = 'Unable to get Borland Package Services';
  RsENoModule = 'Unable to get Module';
  RsBadModuleHInstance = 'Unable to get module HInstance';
  RsENoRootDir = 'RootDir is empty';
  RsENoIDEMenu = 'Unable to get IDE menu';
  RsENoToolsMenu = 'Unable to get Tools menu';

  RsAboutDialogTitle = 'JEDI Code Library';
  RsAboutCopyright = 'Copyright the JCL development team';
  RsAboutTitle = 'JEDI Code Library';
  RsAboutDescription = 'JEDI Code Library http://jcl.sf.net' + AnsiLineBreak +
                       'The JCL is a member of the JEDI Project http://www.delphi-jedi.org' + AnsiLineBreak +
                       'Covered under the Mozilla Public License v1.1 (MPL 1.1)' + AnsiLineBreak +
                       'License available at http://www.mozilla.org/MPL/MPL-1.1.html';
  RsAboutLicenceStatus = 'MPL 1.1';
  RsJCLOptions = 'JCL Options...';
  RsActionSheet = 'Common\Actions';
  RsENoBitmapResources = 'Unable to load bitmap resource';
  RsENoEnvironmentOptions = 'Environment options are not available';

//=== JclExceptionForm.pas ===================================================
resourcestring
  RsReportFormCaption = 'Exception in an expert of the JCL';
  RsExceptionDetails = 'An exception was raised in an expert of the JCL.' + AnsiLineBreak +
                       'The JCL development team expects quality and performance for the library.' +
                       'That''s why we highly encourage you to report this exception by quoting ' +
                       'your version of Delphi/BCB/BDS (including patch numbers), by explaining ' +
                       'steps to reproduce and by copying the call stack displayed in the box below.' + AnsiLineBreak +
                       'There are several ways to report bugs in the JCL:' + AnsiLineBreak +
                       ' - issue tracker (recommended),' + AnsiLineBreak +
                       ' - jedi newsgroups,' + AnsiLineBreak +
                       ' - mailing list.' + AnsiLineBreak +
                       'Details and guidelines for these tools are available at:';
  RsReportURL = 'http://homepages.borland.com/jedi/jcl/page24.html';
  RsReportCaption = 'JCL - Feedback&&Support - Report a bug page';
  RsDetailsExceptionName = 'Exception class name: ';
  RsDetailsExceptionMessage = 'Exception message: ';
  RsErrorWhileFormatting = 'An exception was raised while formatting details for the report';
  RsReportClose = '&Close';

//=== JclOtaActionConfigureSheet.pas =========================================
resourcestring
  RsActions = '&Actions :';
  RsCaption = 'Caption';
  RsShortcut = 'Shortcut';
  RsRestore = '&Restore';

//=== JclExpertConfigurationForm.pas =========================================
resourcestring
  RsConfigurationCaption = 'JCL Options';
  RsOk = '&Ok';
  RsCancel = '&Cancel';
  RsSelectPage = 'Select a page';
  RsHomePage = '&JCL Home page';

//=== OpenDlgFavAdapter.pas ==================================================
resourcestring
  RsAdd          = '<- Add';
  RsDelete       = '&Delete';
  RsFavorites    = '&Favorites';
  RsConfirmation = 'Confirmation';
  RsDelConfirm   = 'Are you sure to delete "%s" from favorite folders?';

//=== JclUsesDialog.pas ======================================================
resourcestring
  RsActionSkip = 'Skip';
  RsActionAdd = 'Add';
  RsActionMove = 'Move';
  RsSectionImpl = 'to implementation uses';
  RsSectionIntf = 'to interface uses';
  RsUndeclIdent = '[Error] %s(%d) Undeclared identifier: ''%s''';
  RsConfirmChanges = '%s: Confirm changes';

//=== JclParseUses.pas =======================================================
resourcestring
  RsEDuplicateUnit = 'Duplicate unit ''%s''';
  RsEInvalidLibrary = 'Invalid library';
  RsEInvalidProgram = 'Invalid program';
  RsEInvalidUnit = 'Invalid unit';
  RsEInvalidUses = 'Invalid uses clause';

//=== ThreadExpertSharedNames.pas ============================================
resourcestring
  RsEnterMutexTimeout = 'JCL Thread Name IDE Expert Mutex Timeout';

//=== ProjAnalyserImpl.pas ===================================================
resourcestring
  RsAnalyzeActionCaption = 'Analyze project %s';
  RsAnalyzeActionName = 'ProjectAnalyseCommand';
  RsProjectNone = '[none]';
  RsCantFindFiles = 'Can''t find MAP or executable file';
  RsBuildingProject = 'Building project %s ...';
  RsAnalyseMenuItemNotInserted = 'Can''t insert the analyse menu item';

//=== ProjAnalyzerFrm.pas ====================================================
resourcestring
  RsFormCaption = 'Project Analyzer - %s';
  RsStatusText = 'Units: %d, Forms: %d, Code: %d, Data: %d, Bss: %d, Resources: %d';
  RsCodeData = '(CODE+DATA)';

//=== JclUsesWizard.pas ======================================================
resourcestring
  RsJediOptionsCaption = 'JEDI Options';
  RsEErrorReadingBuffer = 'Error reading from edit buffer';
  RsUsesSheet = 'Uses wizard';

//=== JclOptionsFrame.pas ====================================================
resourcestring
  RsUsesConfigurationFile = '&Configuration file:';
  RsUsesActive = '&Active';
  RsUsesConfirm = '&Prompt to confirm changes';
  RsUsesOpenTitle = 'Select JEDI Uses wizard configuration file';
  RsUsesOpenFilters = 'Configuration files (*.ini)|*.ini|All files (*.*)|*.*';

//=== JclDebugIdeImpl.pas ====================================================
resourcestring
  RsENoProjectOptions = 'Project options are not available';
  RsBuildActionCaption = 'Build JCL Debug %s';
  RsBuildAllCaption = 'Build JCL Debug All Projects';
  RsBuildActionName = 'ProjectJCLBuildCommand';
  RsBuildAllActionName = 'ProjectJCLBuildAllCommand';
  RsCantInsertToInstalledPackage = 'JCL Debug IDE Expert: Can not insert debug information to installed package' +
    #13#10'%s'#13#10#10'Would you like to disable inserting JCL Debug data ?';
  RsInsertDataCaption = 'Insert JCL Debug data';
  RsInsertDataActionName = 'ProjectJCLInsertDataCommand';
  RsEExecutableNotFound = 'Executable file for project "%s" not found.' +
    'JCL debug data can''t be added to the project.';
  RsENoActiveProject = 'No active project';
  RsENoProjectMenuItem = 'Project menu item not found';
  RsENoBuildMenuItem = 'Build menu item not found';
  RsEBuildMenuItemNotInserted = 'Can''t insert the build menu item';
  RsEInsertDataMenuItemNotInserted = 'Can''t insert the insert data menu item';
  RsENoBuildAction = 'Build action not found';
  RsENoBuildAllAction = 'Build All action not found';
  RsENoProjectGroup = 'No project group';

//=== JclSIMDView.pas ========================================================
resourcestring
  RsENoDebuggerServices = 'Unable to get Borland Debugger Services';
  RsENoViewMenuItem = 'View menu item not found';
  RsENoDebugWindowsMenuItem = 'Debug windows menu item not found';

//=== JclSIMDUtils.pas =======================================================
resourcestring
  RsSIMD = 'SIMD';
  RsMMX = 'MMX';
  RsExMMX = 'Ex MMX';
  Rs3DNow = '3DNow!';
  RsEx3DNow = 'Ex 3DNow!';
  RsSSE1 = 'SSE1';
  RsSSE2 = 'SSE2';
  RsSSE3 = 'SSE3';
  RsLong = '64-bit Core';

  RsTrademarks =
    'MMX is a trademark of Intel Corporation.' + #13#10 +
    '3DNow! is a registered trademark of Advanced Micro Devices.';

  RsNoSIMD = 'No SIMD registers found';
  RsNoSSE = 'SSE are not supported on this processor';
  RsNo128SIMD = 'No 128-bit-register SIMD';
  RsNo64SIMD = 'No 64-bit-register SIMD';
  RsNotSupportedFormat = '<Unsupported format>';
  RsNoPackedData = '<No packed data>';
  RsFormCreateError = 'An exception was triggered while creating the debug window : ';
  RsModifyMM = 'Modification of MM%d';
  RsModifyXMM1 = 'Modification of XMM%d';
  RsModifyXMM2 = 'Modification of XMM%.2d';

  RsVectorIE = 'IE  ';
  RsVectorDE = 'DE  ';
  RsVectorZE = 'ZE  ';
  RsVectorOE = 'OE  ';
  RsVectorUE = 'UE  ';
  RsVectorPE = 'PE  ';
  RsVectorDAZ = 'DAZ '; //  (Only in Intel P4, Intel Xeon and AMD)
  RsVectorIM = 'IM  ';
  RsVectorDM = 'DM  ';
  RsVectorZM = 'ZM  ';
  RsVectorOM = 'OM  ';
  RsVectorUM = 'UM  ';
  RsVectorPM = 'PM  ';
  RsVectorRC = 'RC  ';
  RsVectorFZ = 'FZ  ';

  RsVectorIEText = 'Invalid-operation exception';
  RsVectorDEText = 'Denormal-operand exception';
  RsVectorZEText = 'Zero-divide exception';
  RsVectorOEText = 'Overflow exception';
  RsVectorUEText = 'Underflow exception';
  RsVectorPEText = 'Precision exception';
  RsVectorDAZText = 'Denormal are zeros'; //  (Only in Intel P4, Intel Xeon and AMD)
  RsVectorIMText = 'Invalid-operation mask';
  RsVectorDMText = 'Denormal-operand mask';
  RsVectorZMText = 'Zero-divide mask';
  RsVectorOMText = 'Overflow mask';
  RsVectorUMText = 'Underflow mask';
  RsVectorPMText = 'Precision mask';
  RsVectorRCText = 'Rounding control';
  RsVectorFZText = 'Flush to zero';

  RsRoundToNearest = 'Round to nearest';
  RsRoundDown = 'Round down';
  RsRoundUp = 'Round up';
  RsRoundTowardZero = 'Round toward zero';

  RsEBadRegisterDisplay = 'Bad register display';

//=== JclSIMDViewForm.pas ====================================================
resourcestring
  RsECantUpdateThreadContext = 'Unable to update the thread context';

implementation

// History:

// $Log$
// Revision 1.11  2006/03/13 22:05:31  outchy
// Fixed error message to display project name
//
// Revision 1.10  2006/01/08 17:16:56  outchy
// Settings reworked.
// Common window for expert configurations
//
// Revision 1.9  2005/12/26 18:03:39  outchy
// Enhanced bds support (including C#1 and D8)
// Introduction of dll experts
// Project types in templates
//
// Revision 1.8  2005/12/16 23:46:25  outchy
// Added expert stack form.
// Added code to display call stack on expert exception.
// Fixed package extension for D2006.
//
// Revision 1.7  2005/10/27 13:50:39  rrossmair
// - cleaned up mistakenly expanded check-in comments
//
// Revision 1.6  2005/10/27 08:31:08  outchy
// Items add in the splash screen and in the about box of Delphi (requires at least D2005)
//
// Revision 1.5  2005/10/26 03:29:44  rrossmair
// - improved header information, added Date and Log CVS tags.
//
// Revision 1.4  2005/10/24 12:05:51  marquardt
// further cleanup
//
// Revision 1.3  2005/10/23 12:53:36  marquardt
// further expert cleanup and integration, use of JclRegistry
//
// Revision 1.2  2005/10/22 14:24:18  marquardt
// more expert integration and cleanup
//
// Revision 1.1  2005/10/21 12:24:41  marquardt
// experts reorganized with new directory common
//

end.
