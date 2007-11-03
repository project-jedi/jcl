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
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
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
  RsENoPersonalityServices = 'Unable to get Borland Personality Services';
  RsENoMessageServices = 'Unable to get Borland Message Services';
  RsENoGalleryCategoryManager = 'Unable to get Borland Gallery Category Manager';
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

//=== JclOtaWizardForm.pas ===================================================
resourcestring
  RsNext = '&Next';
  RsPrevious = '&Previous';
  RsFinish = '&Finish';
  RsWizardProgression = 'Page %d of %d: %s';

//=== JclOtaExcDlgWizard.pas =================================================
resourcestring
  RsExceptionDialogConfigure = 'New exception dialog...';

//=== JclOtaExcDlgFileFrame.pas ==============================================
resourcestring
  RsExcDlgFileOptions = 'file options';
  RsLanguage = '&Language:';
  RsFileName = '&File name:';
  RsFormName = 'Form &name:';
  RsFormAncestor = 'Form &ancestor:';
  RsFileNameDialog = '&Save new file as...';

//=== JclOtaExcDlgFormFrame.pas ==============================================
resourcestring
  RsExcDlgFormOptions = 'form options';
  RsDialogWithMailButton = '&Button to send stack trace by mail';
  RsEMail = '&EMail:';
  RsSubject = '&Subject:';
  RsModalDialog = '&Modal dialog';
  RsSizeableDialog = 'S&izeable dialog';
  RsAutoScrollBars = '&Automatic scroll bars';

//=== JclOtaExcDlgSystemFrame.pas ============================================
resourcestring
  RsExcDlgSystemOptions = 'system options';
  RsDelayedStackTrace = '&Delayed stack traces (faster)';
  RsLogTrace = '&Add crash data to log file';
  RsHookDll = '&Hook DLL';
  RsModuleList = '&Module list';
  RsOSInfo = '&Operating system informations';
  RsActiveControls = '&List of active controls';
  RsMainThreadOnly = '&Catch only exceptions of main thread';

//=== JclOtaExcDlgTraceFrame.pas =============================================
resourcestring
  RsExcDlgTraceOptions = 'trace options';
  RsStackList = '&Stack list';
  RsRawData = '&Raw analysis of the stack';
  RsModuleName = '&Module name';
  //RsAddressOffset = 'Address offset';
  RsCodeDetails = '&Code details';
  RsVirtualAddress = '&Virtual address';
  RsModuleOffset = 'Module &offset';
  RsPreview = '&Preview:';
  RsAllThreads = 'Include traces for registered &threads';

//=== JclOtaExcDlgIgnoreFrame.pas ============================================
resourcestring
  RsExcDlgIgnoreOptions = 'ignored exceptions';
  RsTraceAllExceptions = '&Trace all exceptions';
  RsTraceEAbort = 'Trace &EAbort and its descendants';
  RsIgnoredExceptions = '&Ancestor exception classes to ignore (one per line)';

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

//=== ProjAnalyserImpl.pas ===================================================
resourcestring
  RsAnalyzeActionCaption = 'Analyze project %s';
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
  RsCantInsertToInstalledPackage = 'JCL Debug IDE Expert: Can not insert debug information to installed package' +
    AnsiLineBreak + '%s' + AnsiLineBreak + 'Would you like to disable inserting JCL Debug data ?';
  RsCompilationAborted = 'JCL Debug data cannot be inserted to installed package' + AnsiLineBreak + 'Compilation aborted';
  RsInsertDataCaption = 'Insert JCL Debug data';
  RsEExecutableNotFound = 'Executable file for project "%s" not found.' +
    'JCL debug data can''t be added to the binary.';
  RsEMapFileNotFound = 'Map file "%s" for project "%s" not found.' +
    'No conversions of debug information were made';
  RsConvertedMapToJdbg = 'Converted MAP file "%s" (%d bytes) to .jdbg (%d bytes)';
  RsInsertedJdbg = 'Converted MAP file "%s" (%d bytes) and inserted debug information (%d bytes) into the binary';
  RsEMapConversion = 'Failed to convert MAP file "%s"';
  RsENoActiveProject = 'No active project';
  RsENoProjectMenuItem = 'Project menu item not found';
  RsENoBuildMenuItem = 'Build menu item not found';
  RsEBuildMenuItemNotInserted = 'Can''t insert the build menu item';
  RsEInsertDataMenuItemNotInserted = 'Can''t insert the insert data menu item';
  RsENoBuildAction = 'Build action not found';
  RsENoBuildAllAction = 'Build All action not found';
  RsENoProjectGroup = 'No project group';
  RsDebugConfigPageCaption = 'Debug info converter';

//=== JclDebugIdeConfigFrame.pas =============================================
resourcestring
  RsDebugEnableExpert = 'Enable debug expert';
  RsDebugGenerateJdbg = 'Generate .jdbg files';
  RsDebugInsertJdbg = 'Insert data into the binary';

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
  RsLong = '64-bit Core';

  RsTrademarks =
    'MMX is a trademark of Intel Corporation.' + AnsiLineBreak +
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

//=== JclOtaExcDlgRepository.pas =============================================
resourcestring
  RsRepositoryExcDlgPage = 'Exception dialog';
  
  RsRepositoryExcDlgDelphiName = 'Jcl Exception dialog for Delphi';
  RsRepositoryExcDlgDelphiDescription = 'Create an exception dialog for your Delphi project';

  RsRepositoryExcDlgCBuilderName = 'Jcl Exception dialog for C++Builder';
  RsRepositoryExcDlgCBuilderDescription = 'Create an exception dialog for your C++Builder';

implementation

end.
