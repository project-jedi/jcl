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
{ The Original Code is JclResources.pas.                                                           }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright (C) Marcel van Brakel. All rights reserved.  }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Alexei Koudinov                                                                                }
{   Barry Kelly                                                                                    }
{   Flier Lu (flier)                                                                               }
{   Marcel Bestebroer                                                                              }
{   Marcel van Brakel                                                                              }
{   Matthias Thoma (mthoma)                                                                        }
{   Peter Friese                                                                                   }
{   Petr Vones                                                                                     }
{   Raymond Alexander (rayspostbox3)                                                               }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Scott Price (scottprice)                                                                       }                                                                       
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Unit which provides a central place for all resource strings used in the JCL                     }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclResources;

{$I jcl.inc}

interface

//--------------------------------------------------------------------------------------------------
// JclBase
//--------------------------------------------------------------------------------------------------

resourcestring
  RsWin32Prefix        = 'Win32: %s (%u)';
  RsDynArrayError      = 'DynArrayInitialize: ElementSize out of bounds';
  RsSysErrorMessageFmt = 'Win32 Error %d (%x)';

//--------------------------------------------------------------------------------------------------
// JclBorlandTools
//--------------------------------------------------------------------------------------------------

resourcestring
  RsIndexOufOfRange = 'Index out of range';
  RsNeedUpdate      = 'You should install latest Update Pack #%d for %s';
  RsUpdatePackName  = 'Update Pack #%d';
  {$IFDEF KYLIX}
  RsDelphiName      = 'Kylix %d for Delphi';
  RsBCBName         = 'Kylix %d for C++Builder';
  RsOpenEdition     = 'Open Edition';
  RsServerDeveloper = 'Server Developer';
  RsVclIncludeDir   = '/include/vcl/';
  {$ENDIF KYLIX}
  {$IFDEF MSWINDOWS}
  RsDelphiName      = 'Delphi %d';
  RsBCBName         = 'C++Builder %d';
  RsClientServer    = 'Client/Server';
  RsStandard        = 'Standard';
  RsVclIncludeDir   = '\Include\Vcl\';
  {$ENDIF MSWINDOWS}
  RsArchitect       = 'Architect';
  RsEnterprise      = 'Enterprise';
  RsPersonal        = 'Personal';
  RsProfessional    = 'Professional';

//--------------------------------------------------------------------------------------------------
// JclCIL
//--------------------------------------------------------------------------------------------------

resourcestring
  RsInstructionStreamInvalid = 'Invalid IL instruction stream';

//--------------------------------------------------------------------------------------------------
// JclClasses
//--------------------------------------------------------------------------------------------------

resourcestring
  RsVMTMemoryWriteError = 'Error writing VMT memory (%s)';

//--------------------------------------------------------------------------------------------------
// JclClr
//--------------------------------------------------------------------------------------------------

resourcestring
  RsClrCopyright = '// Delphi-JEDI .NET Framework IL Disassembler.  Version 0.1' +  #13#10 +
    '// Project JEDI Code Library (JCL) Team. All rights reserved.' +  #13#10;

//--------------------------------------------------------------------------------------------------
// JclCOM
//--------------------------------------------------------------------------------------------------

resourcestring
  RsComInvalidParam      = 'An invalid parameter was passed to the routine. If a parameter was' +
    ' expected, it might be an unassigned item or nil pointer';
  RsComFailedStreamRead  = 'Failed to read all of the data from the specified stream';
  RsComFailedStreamWrite = 'Failed to write all of the data into the specified stream';

//--------------------------------------------------------------------------------------------------
// JclComplex
//--------------------------------------------------------------------------------------------------

resourcestring
  RsComplexInvalidString = 'Failed to create a complex number from the string provided';

//--------------------------------------------------------------------------------------------------
// JclConsole
//--------------------------------------------------------------------------------------------------

resourcestring
  RsCannotRaiseSignal = 'Cannot raise %s signal.';

//--------------------------------------------------------------------------------------------------
// JclCounter
//--------------------------------------------------------------------------------------------------

resourcestring
  RsNoCounter = 'No high performance counters supported';

//--------------------------------------------------------------------------------------------------
// JclDateTime
//--------------------------------------------------------------------------------------------------

resourcestring
  RsMakeUTCTime    = 'Error converting to UTC time. Time zone could not be determined';
  RsDateConversion = 'Error illegal date or time format';

//--------------------------------------------------------------------------------------------------
// JclDebug
//--------------------------------------------------------------------------------------------------

resourcestring
  // Diagnostics
  RsDebugAssertValidPointer = 'Invalid Pointer passed to AssertValid';
  RsDebugAssertValidString  = 'Invalid string passed to AssertValid';

  // TMapFiles
  RsDebugMapFileExtension = '.map'; // do not localize
  RsDebugNoProcessInfo    = 'Unable to obtain process information';
  RsDebugSnapshot         = 'Failure creating toolhelp32 snapshot';

//--------------------------------------------------------------------------------------------------
// JclEDI
//--------------------------------------------------------------------------------------------------

resourcestring
  RsEDIError001 = 'Could not open edi file.  File not specified.';
  RsEDIError002 = 'Could not save edi file.  File name and path not specified.';
  RsEDIError003 = 'Could not get data object from %s at index [%s],';
  RsEDIError004 = 'Could not get data object from %s at index [%s], Index too low.';
  RsEDIError005 = 'Could not get data object from %s at index [%s], Index too high.';
  RsEDIError006 = 'Could not get data object from %s at index [%s], ' +
    'There was no data object assigned.';
  RsEDIError007 = 'Could not set data object from %s at index [%s].';
  RsEDIError008 = 'Could not set data object from %s at index [%s], Index too low.';
  RsEDIError009 = 'Could not set data object from %s at index [%s], Index too high.';
  RsEDIError010 = 'Could not delete data object from %s at index [%s]';
  RsEDIError011 = 'Could not delete data objects from %s at index [%s]';
  RsEDIError012 = 'Delimiters have not been assigned to interchange.  Dissassemble cancelled.';
  RsEDIError013 = 'Delimiters have not been assigned to interchange.  Assemble cancelled.';
  RsEDIError014 = 'Could not find interchange control header segment terminator.';
  RsEDIError015 = 'Could not find interchange control header.';
  RsEDIError016 = 'Could not find interchange control trailer segment terminator.';
  RsEDIError017 = 'Could not find interchange control trailer.';
  RsEDIError018 = 'Could not find interchange control trailer or garbage at end of file.';
  RsEDIError019 = 'Could not assign delimiters to functional group.  Dissassemble cancelled.';
  RsEDIError020 = 'Could not assign delimiters to functional group.  Assemble cancelled.';
  RsEDIError021 = 'Could not find functional group header segment terminator.';
  RsEDIError022 = 'Could not find functional group header.'; //conditional for UN/EDIFACT
  RsEDIError023 = 'Could not find functional group trailer segment terminator.';
  RsEDIError024 = 'Could not find functional group trailer.';
  RsEDIError025 = 'Could not assign delimiters to transaction set.  Dissassemble cancelled.';
  RsEDIError026 = 'Could not assign delimiters to transaction set.  Assemble cancelled.';
  RsEDIError027 = 'Could not find transaction set header.';
  RsEDIError028 = 'Could not find transaction set trailer segment terminator.';
  RsEDIError029 = 'Could not find transaction set trailer.';
  RsEDIError030 = 'Could not assign delimiters to message.  Dissassemble cancelled.';
  RsEDIError031 = 'Could not assign delimiters to message.  Assemble cancelled.';
  RsEDIError032 = 'Could not find message header.';
  RsEDIError033 = 'Could not find message trailer segment terminator.';
  RsEDIError034 = 'Could not find message trailer.';
  RsEDIError035 = 'Could not assign delimiters to segment.  Dissassemble cancelled.';
  RsEDIError036 = 'Could not assign delimiters to segment.  Assemble cancelled.';
  RsEDIError037 = 'Could not assign delimiters to composite element.  Dissassemble cancelled.';
  RsEDIError038 = 'Could not assign delimiters to composite element.  Assemble cancelled.';
  RsEDIError039 = 'Could not get data object in transaction set loop at index [%s], ' +
    'Data object does not exist.';
  RsEDIError040 = 'Could not get data object in transaction set loop at index [%s], ' +
    'Index too high.';
  RsEDIError041 = 'Could not get data object in transaction set loop at index [%s], Index too low.';
  RsEDIError042 = 'Could not get data object in transaction set loop at index [%s].';
  RsEDIError043 = 'Could not set data object in transaction set loop at index [%s], ' +
    'Index too high.';
  RsEDIError044 = 'Could not set data object in transaction set loop at index [%s], Index too low.';
  RsEDIError045 = 'Could not set data object in transaction set loop at index [%s].';
  RsEDIError046 = 'Could not get data object in message loop at index [%s], ' +
    'Data object does not exist.';
  RsEDIError047 = 'Could not get data object in message loop at index [%s], Index too high.';
  RsEDIError048 = 'Could not get data object in message loop at index [%s], Index too low.';
  RsEDIError049 = 'Could not get data object in message loop at index [%s].';
  RsEDIError050 = 'Could not set data object in message loop at index [%s], Index too high.';
  RsEDIError051 = 'Could not set data object in message loop at index [%s], Index too low.';
  RsEDIError052 = 'Could not set data object in message loop at index [%s].';
  RsEDIError053 = 'Loop in loop stack record at index [%s] does not exist.';
  RsEDIError054 = 'Could not get loop stack record at index [%s], Index too high.';
  RsEDIError055 = 'Could not get loop stack record at index [%s], Index too low.';
  RsEDIError056 = 'Could not get loop stack record at index [%s].';
  RsEDIError057 = 'Could not get safe loop stack index [%s].';
  RsEDIError058 = 'Could not assign element specification to element at index [%s] ' +
    'in segment [%s] at index [%s] in transaction set.';

  RsUnknownAttribute = 'Unknown Attribute';

//--------------------------------------------------------------------------------------------------
// JclEDISEF
//--------------------------------------------------------------------------------------------------

resourcestring
  // Transaction Set:850
  SEFTextSetsCode_Set0_Desc = 'Transaction Set or message title.';
  SEFTextSetsCode_Set1_Desc = 'Transaction Set functional group (X12).';
  SEFTextSetsCode_Set2_Desc = 'Transaction Set or message purpose.';
  SEFTextSetsCode_Set3_Desc = 'Level 1 note on transaction set or message.';
  SEFTextSetsCode_Set4_Desc = 'Level 2 note on transaction set or message.';
  SEFTextSetsCode_Set5_Desc = 'Level 3 note on transaction set or message.';
  // Transaction Set~segment ordinal number: 850~1
  SEFTextSetsCode_Seg0_Desc = 'Segment reference notes that are part of the transaction set in X12.';
  SEFTextSetsCode_Seg1_Desc = 'Segment reference notes documented with the segment (like in VICS/UCS).';
  SEFTextSetsCode_Seg2_Desc = 'Segment reference comment documented with the transaction set.';
  SEFTextSetsCode_Seg3_Desc = 'Segment name.';
  SEFTextSetsCode_Seg4_Desc = 'Level 1 note on segment.';
  SEFTextSetsCode_Seg5_Desc = 'Level 2 note on segment.';
  SEFTextSetsCode_Seg6_Desc = 'Segment purpose.';
  SEFTextSetsCode_Seg7_Desc = 'Level 3 note on segment. See * below for other levels of notes.';
  // Transaction Set~segment ordinal number~element or composite ordinal number: 850~1~4
  SEFTextSetsCode_Elm0_Desc = 'Level 1 note on element or composite.';
  SEFTextSetsCode_Elm1_Desc = 'Level 2 note on element or composite.';
  SEFTextSetsCode_Elm2_Desc = 'Name of element or composite.';
  SEFTextSetsCode_Elm4_Desc = 'Level 3 note on element or composite.';

//--------------------------------------------------------------------------------------------------
// JclEDIXML
//--------------------------------------------------------------------------------------------------

resourcestring
  EDIXMLError001 = 'Could not open edi file.  File not specified.';
  EDIXMLError002 = 'Could not save edi file.  File name and path not specified.';
  EDIXMLError003 = 'Could not assign delimiters to edi file.  Disassemble cancelled.';
  EDIXMLError004 = 'Could not assign delimiters to edi file.  Assemble cancelled.';
  EDIXMLError005 = 'Could not assign delimiters to interchange control. Disassemble cancelled.';
  EDIXMLError006 = 'Could not assign delimiters to interchange control. Assemble cancelled.';
  EDIXMLError007 = 'Could not find interchange control end tag.';
  EDIXMLError008 = 'Could not find interchange control end tag delimiter.';
  EDIXMLError009 = 'Could not find interchange control header.';
  EDIXMLError010 = 'Could not find interchange control header end tag.';
  EDIXMLError011 = 'Could not find interchange control header end tag delimiter.';
  EDIXMLError012 = 'Could not find interchange control trailer.';
  EDIXMLError013 = 'Could not find interchange control trailer end tag.';
  EDIXMLError014 = 'Could not find interchange control trailer end tag delimiter.';
  EDIXMLError015 = 'Could not assign delimiters to functional group. Disassemble cancelled.';
  EDIXMLError016 = 'Could not assign delimiters to functional group. Assemble cancelled.';
  EDIXMLError017 = 'Could not find functional group end tag.';
  EDIXMLError018 = 'Could not find functional group end tag delimiter.';
  EDIXMLError019 = 'Could not find functional group header.';
  EDIXMLError020 = 'Could not find functional group header end tag.';
  EDIXMLError021 = 'Could not find functional group header end tag delimiter.';
  EDIXMLError022 = 'Could not find functional group trailer.';
  EDIXMLError023 = 'Could not find functional group trailer end tag.';
  EDIXMLError024 = 'Could not find functional group trailer end tag delimiter.';
  EDIXMLError025 = 'Could not assign delimiters to transactoin set. Disassemble cancelled.';
  EDIXMLError026 = 'Could not assign delimiters to transactoin set. Assemble cancelled.';
  EDIXMLError027 = 'Could not find transaction set end tag.';
  EDIXMLError028 = 'Could not find transaction set end tag delimiter.';
  EDIXMLError029 = 'Could not assign delimiters to transactoin set loop. Disassemble cancelled.';
  EDIXMLError030 = 'Could not assign delimiters to transactoin set loop. Assemble cancelled.';
  EDIXMLError031 = 'Could not find loop end tag';
  EDIXMLError032 = 'Could not find loop end tag delimiter';
  EDIXMLError033 = 'Could not set data object at index [%s].';
  EDIXMLError034 = 'Could not set data object at index [%s], Index too low.';
  EDIXMLError035 = 'Could not set data object at index [%s], Index too high.';
  EDIXMLError036 = 'Could not get data object at index [%s], There was no data object to get.';
  EDIXMLError037 = 'Could not get data object at index [%s], Index too low.';
  EDIXMLError038 = 'Could not get data object at index [%s], Index too high.';
  EDIXMLError039 = 'Could not get data object at index [%s], Data object does not exist.';
  EDIXMLError040 = 'Could not delete EDI data object';
  EDIXMLError041 = 'Could not assign delimiters to segment. Disassemble cancelled.';
  EDIXMLError042 = 'Could not assign delimiters to segment. Assemble cancelled.';
  EDIXMLError043 = 'Could not find segment begin tag';
  EDIXMLError044 = 'Could not find segment end tag';
  EDIXMLError045 = 'Could not find segment end tag delimiter';
  EDIXMLError046 = 'Could not assign delimiters to element. Disassemble cancelled.';
  EDIXMLError047 = 'Could not assign delimiters to element. Assemble cancelled.';
  EDIXMLError048 = 'Could not find element tag';
  EDIXMLError049 = 'Could not find element end tag';
  EDIXMLError050 = 'Could not find element end tag delimiter';
  EDIXMLError051 = 'Could not set element at index [%s].';
  EDIXMLError052 = 'Could not set element at index [%s], Index too low.';
  EDIXMLError053 = 'Could not set element at index [%s], Index too high.';
  EDIXMLError054 = 'Could not get element at index [%s], There was no element to get.';
  EDIXMLError055 = 'Could not get element at index [%s], Index too low.';
  EDIXMLError056 = 'Could not get element at index [%s], Index too high.';
  EDIXMLError057 = 'Could not get element at index [%s], Element does not exist.';
  EDIXMLError058 = 'Could not delete element at index [%s].';
  EDIXMLError059 = 'Could not find transaction set header.';
  EDIXMLError060 = 'Could not find transaction set trailer.';
  EDIXMLError061 = 'Could not find transaction set header and trailer.';
  EDIXMLError062 = 'TEDIXMLANSIX12FormatTranslator: Unexpected object [%s] found.';

//--------------------------------------------------------------------------------------------------
// JclExprEval
//--------------------------------------------------------------------------------------------------

resourcestring
  RsExprEvalRParenExpected = 'Parse error: '')'' expected';
  RsExprEvalFactorExpected = 'Parse error: Factor expected';
  RsExprEvalUnknownSymbol = 'Parse error: Unknown symbol: ''%s''';

  RsExprEvalFirstArg = 'Parse error: ''('' and function''s first parameter expected';
  RsExprEvalNextArg = 'Parse error: '','' and another parameter expected';
  RsExprEvalEndArgs = 'Parse error: '')'' to close function''s parameters expected';

  RsExprEvalExprNotFound = 'Expression compiler error: Expression ''%s'' not found';
  RsExprEvalExprPtrNotFound = 'Expression compiler error: Expression pointer not found';
  RsExprEvalExprRefCountAssertion = 'Expression compiler error: expression refcount < 0';

//--------------------------------------------------------------------------------------------------
// JclFileUtils
//--------------------------------------------------------------------------------------------------

resourcestring
  // Path manipulation
  RsPathInvalidDrive = '%s is not a valid drive';

  // Files and directories
  RsFileUtilsAttrUnavailable = 'Unable to retrieve attributes of %s';

  RsCannotCreateDir = 'Unable to create directory';
  RsDelTreePathIsEmpty = 'DelTree: Path is empty';
  RsFileSearchAttrInconsistency = 'Some file search attributes are required AND rejected!';

  // TJclFileVersionInfo
  RsFileUtilsNoVersionInfo = 'File contains no version information';
  RsFileUtilsLanguageIndex = 'Illegal language index';

  // Strings returned from OSIdentTOString()
  RsVosUnknown      = 'Unknown';
  RsVosDos          = 'MS-DOS';
  RsVosOS216        = '16-bit OS/2';
  RsVosOS232        = '32-bit OS/2';
  RsVosNT           = 'Windows NT';
  RsVosWindows16    = '16-bit Windows';
  RsVosPM16         = '16-bit PM';
  RsVosPM32         = '32-bit PM';
  RsVosWindows32    = '32-bit Windows';
  RsVosDosWindows16 = '16-bit Windows, running on MS-DOS';
  RsVosDosWindows32 = 'Win32 API, running on MS-DOS';
  RsVosOS216PM16    = '16-bit PM, running on 16-bit OS/2';
  RsVosOS232PM32    = '32-bit PM, running on 32-bit OS/2';
  RsVosNTWindows32  = 'Win32 API, running on Windows/NT';
  RsVosDesignedFor  = 'Designed for ';

  // Strings returned from OSFileTypeToString()
  RsVftUnknown         = 'Unknown';
  RsVftApp             = 'Application';
  RsVftDll             = 'Library';
  RsVftDrv             = 'Driver';
  RsVftFont            = 'Font';
  RsVftVxd             = 'Virtual device';
  RsVftStaticLib       = 'Static-link library';
  RsVft2DrvPRINTER     = 'Printer';
  RsVft2DrvKEYBOARD    = 'Keyboard';
  RsVft2DrvLANGUAGE    = 'Language';
  RsVft2DrvDISPLAY     = 'Display';
  RsVft2DrvMOUSE       = 'Mouse';
  RsVft2DrvNETWORK     = 'Network';
  RsVft2DrvSYSTEM      = 'System';
  RsVft2DrvINSTALLABLE = 'Installable';
  RsVft2DrvSOUND       = 'Sound';
  RsVft2DrvCOMM        = 'Communications';
  RsVft2FontRASTER     = 'Raster';
  RsVft2FontVECTOR     = 'Vector';
  RsVft2FontTRUETYPE   = 'TrueType';

  // TJclFileStream
  RsFileStreamCreate         = 'Unable to create temporary file stream';

  // TJclFileMapping
  RsCreateFileMapping        = 'Failed to create FileMapping';
  RsCreateFileMappingView    = 'Failed to create FileMappingView';
  RsLoadFromStreamSize       = 'Not enough space in View in procedure LoadFromStream';
  RsFileMappingInvalidHandle = 'Invalid file handle';
  RsViewNeedsMapping         = 'FileMap argument of TJclFileMappingView constructor cannot be nil';
  RsFailedToObtainSize       = 'Failed to obtain size of file';

  // GetDriveTypeStr()
  RsUnknownDrive   = 'Unknown drive type';
  RsRemovableDrive = 'Removable Drive';
  RsHardDisk       = 'Hard Disk';
  RsRemoteDrive    = 'Remote Drive';
  RsCDRomDrive     = 'CD-ROM';
  RsRamDisk        = 'RAM-Disk';

  // GetFileAttributeList()
  RsAttrDirectory  = 'Directory';
  RsAttrReadOnly   = 'ReadOnly';
  RsAttrSystemFile = 'SystemFile';
  RsAttrVolumeID   = 'Volume ID';
  RsAttrArchive    = 'Archive';
  RsAttrAnyFile    = 'AnyFile';
  RsAttrHidden     = 'Hidden';

  // GetFileAttributeListEx()
  RsAttrNormal       = 'Normal';
  RsAttrTemporary    = 'Temporary';
  RsAttrCompressed   = 'Compressed';
  RsAttrOffline      = 'Offline';
  RsAttrEncrypted    = 'Encrypted';
  RsAttrReparsePoint = 'Reparse Point';
  RsAttrSparseFile   = 'Sparse';

  // TJclFileMapping.Create
  RsFileMappingOpenFile = 'Unable to open the file';

  // TJclMappedTextReader
  RsFileIndexOutOfRange = 'Index of out range';

  // FileGetTypeName()
  RsDefaultFileTypeName = ' File';

//--------------------------------------------------------------------------------------------------
// JclGraphics, JclGraphUtils
//--------------------------------------------------------------------------------------------------

resourcestring
  RsBitsPerSampleNotSupported = '%d bits per sample not supported in color space conversion';
  RsAssertUnpairedEndUpdate   = 'Unpaired BeginUpdate EndUpdate';
  RsCreateCompatibleDc        = 'Could not create compatible DC';
  RsDestinationBitmapEmpty    = 'Destination bitmap is empty';
  RsDibHandleAllocation       = 'Could not allocate handle for DIB';
  RsMapSizeFmt                = 'Could not set size on class "%s"';
  RsSelectObjectInDc          = 'Could not select object in DC';
  RsSourceBitmapEmpty         = 'Source bitmap is empty';
  RsSourceBitmapInvalid       = 'Source bitmap is invalid';
  RsNoBitmapForRegion         = 'No bitmap for region';
  RsNoDeviceContextForWindow  = 'Cannot get device context of the window';
  RsInvalidRegion             = 'Invalid Region defined for RegionInfo';
  RsRegionDataOutOfBound      = 'Out of bound index on RegionData';
  RsRegionCouldNotCreated     = 'Region could not be created';
  RsInvalidHandleForRegion    = 'Invalid handle for region';
  RsInvalidRegionInfo         = 'Invalid RegionInfo';

  RsBitmapExtension           = '.bmp';
  RsJpegExtension             = '.jpg';

//--------------------------------------------------------------------------------------------------
// JclMapi
//--------------------------------------------------------------------------------------------------

resourcestring
  RsMapiError         = 'MAPI Error: (%d) "%s"';
  RsMapiMissingExport = 'Function "%s" is not exported by client';
  RsMapiInvalidIndex  = 'Index is out ot range';
  RsMapiMailNoClient  = 'No Simple MAPI client installed, cannot send the message';

  RsMapiErrUSER_ABORT               = 'User abort';
  RsMapiErrFAILURE                  = 'General MAPI failure';
  RsMapiErrLOGIN_FAILURE            = 'MAPI login failure';
  RsMapiErrDISK_FULL                = 'Disk full';
  RsMapiErrINSUFFICIENT_MEMORY      = 'Insufficient memory';
  RsMapiErrACCESS_DENIED            = 'Access denied';
  RsMapiErrTOO_MANY_SESSIONS        = 'Too many sessions';
  RsMapiErrTOO_MANY_FILES           = 'Too many files were specified';
  RsMapiErrTOO_MANY_RECIPIENTS      = 'Too many recipients were specified';
  RsMapiErrATTACHMENT_NOT_FOUND     = 'A specified attachment was not found';
  RsMapiErrATTACHMENT_OPEN_FAILURE  = 'Attachment open failure';
  RsMapiErrATTACHMENT_WRITE_FAILURE = 'Attachment write failure';
  RsMapiErrUNKNOWN_RECIPIENT        = 'Unknown recipient';
  RsMapiErrBAD_RECIPTYPE            = 'Bad recipient type';
  RsMapiErrNO_MESSAGES              = 'No messages';
  RsMapiErrINVALID_MESSAGE          = 'Invalid message';
  RsMapiErrTEXT_TOO_LARGE           = 'Text too large';
  RsMapiErrINVALID_SESSION          = 'Invalid session';
  RsMapiErrTYPE_NOT_SUPPORTED       = 'Type not supported';
  RsMapiErrAMBIGUOUS_RECIPIENT      = 'A recipient was specified ambiguously';
  RsMapiErrMESSAGE_IN_USE           = 'Message in use';
  RsMapiErrNETWORK_FAILURE          = 'Network failure';
  RsMapiErrINVALID_EDITFIELDS       = 'Invalid edit fields';
  RsMapiErrINVALID_RECIPS           = 'Invalid recipients';
  RsMapiErrNOT_SUPPORTED            = 'Not supported';

  RsMapiMailORIG        = 'From';
  RsMapiMailTO          = 'To';
  RsMapiMailCC          = 'Cc';
  RsMapiMailBCC         = 'Bcc';
  RsMapiMailSubject     = 'Subject';
  RsMapiMailBody        = 'Body';

//--------------------------------------------------------------------------------------------------
// JclMath
//--------------------------------------------------------------------------------------------------

resourcestring
  RsMathDomainError    = 'Domain check failure in JclMath';
  RsEmptyArray         = 'Empty array is not allowed as input parameter';
  RsNonPositiveArray   = 'Input array contains non-positive or zero values';
  RsUnexpectedDataType = 'Unexpected data type';
  RsUnexpectedValue    = 'Unexpected data value';
  RsRangeError         = 'Cannot merge range';
  RsInvalidRational    = 'Invalid rational number';
  RsDivByZero          = 'Division by zero';
  RsRationalDivByZero  = 'Rational division by zero';
  RsNoNaN              = 'NaN expected';
  RsNaNTagError        = 'NaN Tag value %d out of range';
  RsNaNSignal          = 'NaN signaling %d';
  RsPowerInfinite      = 'Power function: Result is infinite';
  RsPowerComplex       = 'Power function: Result is complex';

//--------------------------------------------------------------------------------------------------
// JclMetadata
//--------------------------------------------------------------------------------------------------

resourcestring
  RsUnknownClassLayout      = 'Unknown class layout - $%.8x';
  RsUnknownStringFormatting = 'Unknown string formatting - $%.8x';
  RsInvalidSignatureData    = 'Invalid compressed signature data - %.2x %.2x %.2x %.2x';
  RsUnknownManifestResource = 'Unknown manifest resource visibility - %d';
  RsNoLocalVarSig           = 'Signature %s is not LocalVarSig';
  RsLocalVarSigOutOfRange   = 'LocalVarSig count %d is out of range [1..$$FFFE]';

//--------------------------------------------------------------------------------------------------
// JclMIDI
//--------------------------------------------------------------------------------------------------

resourcestring
  RsOctaveC      = 'C';
  RsOctaveCSharp = 'C#';
  RsOctaveD      = 'D';
  RsOctaveDSharp = 'D#';
  RsOctaveE      = 'E';
  RsOctaveF      = 'F';
  RsOctaveFSharp = 'F#';
  RsOctaveG      = 'G';
  RsOctaveGSharp = 'G#';
  RsOctaveA      = 'A';
  RsOctaveASharp = 'A#';
  RsOctaveB      = 'B';

  RsMidiInvalidChannelNum = 'Invalid MIDI channel number (%d)';
  {$IFDEF UNIX}
  RsMidiNotImplemented    = 'JclMidi: MIDI I/O for Unix not (yet) implemented';
  {$ENDIF UNIX}

//--------------------------------------------------------------------------------------------------
// JclMiscel
//--------------------------------------------------------------------------------------------------

resourcestring
  // CreateProcAsUser
  RsCreateProcOSVersionError          = 'Unable to determine OS version';
  RsCreateProcNTRequiredError         = 'Windows NT required';
  RsCreateProcBuild1057Error          = 'NT version 3.51 build 1057 or later required';

  RsCreateProcPrivilegeMissing        = 'This account does not have the privilege "%s" (%s)';
  RsCreateProcLogonUserError          = 'LogonUser failed';
  RsCreateProcAccessDenied            = 'Access denied';
  RsCreateProcLogonFailed             = 'Unable to logon';
  RsCreateProcSetStationSecurityError = 'Cannot set WindowStation "%s" security.';
  RsCreateProcSetDesktopSecurityError = 'Cannot set Desktop "%s" security.';
  RsCreateProcPrivilegesMissing       = 'This account does not have one (or more) of ' +
    'the following privileges: ' + '"%s"(%s)' + #13 + '"%s"(%s)' + #13;
  RsCreateProcCommandNotFound         = 'Command or filename not found: "%s"';
  RsCreateProcFailed                  = 'CreateProcessAsUser failed';

//--------------------------------------------------------------------------------------------------
// JclMultimedia
//--------------------------------------------------------------------------------------------------

resourcestring
  // Multimedia timer
  RsMmTimerGetCaps     = 'Error retrieving multimedia timer device capabilities';
  RsMmTimerBeginPeriod = 'The supplied timer period value is out of range';
  RsMmSetEvent         = 'Error setting multimedia event timer';
  RsMmInconsistentId   = 'Multimedia timer callback was called with inconsistent Id';
  RsMmTimerActive      = 'This operation cannot be performed while the timer is active';

  // Audio Mixer
  RsMmMixerSource      = 'Source';
  RsMmMixerDestination = 'Destination';
  RsMmMixerUndefined   = 'Undefined';
  RsMmMixerDigital     = 'Digital';
  RsMmMixerLine        = 'Line';
  RsMmMixerMonitor     = 'Monitor';
  RsMmMixerSpeakers    = 'Speakers';
  RsMmMixerHeadphones  = 'Headphones';
  RsMmMixerTelephone   = 'Telephone';
  RsMmMixerWaveIn      = 'Waveform-audio input';
  RsMmMixerVoiceIn     = 'Voice input';
  RsMmMixerMicrophone  = 'Microphone';
  RsMmMixerSynthesizer = 'Synthesizer';
  RsMmMixerCompactDisc = 'Compact disc';
  RsMmMixerPcSpeaker   = 'PC speaker';
  RsMmMixerWaveOut     = 'Waveform-audio output';
  RsMmMixerAuxiliary   = 'Auxiliary audio line';
  RsMmMixerAnalog      = 'Analog';
  RsMmMixerNoDevices   = 'No mixer device found';
  RsMmMixerCtlNotFound = 'Line control (%s, %.8x) not found';

  // EJclMciError
  RsMmUnknownError     = 'Unknown MCI error No. %d';
  RsMmMciErrorPrefix   = 'MCI-Error: ';

  // CD audio routines
  RsMmNoCdAudio        = 'Cannot open CDAUDIO-Device';
  RsMmCdTrackNo        = 'Track: %.2u';
  RsMMCdTimeFormat     = '%2u:%.2u';
  RsMMTrackAudio       = 'Audio';
  RsMMTrackOther       = 'Other';

//--------------------------------------------------------------------------------------------------
// JclNTFS
//--------------------------------------------------------------------------------------------------

resourcestring
  RsInvalidArgument = '%s: Invalid argument <%s>';
  RsNtfsUnableToDeleteSymbolicLink = 'Unable to delete temporary symbolic link';

//--------------------------------------------------------------------------------------------------
// JclPCRE
//--------------------------------------------------------------------------------------------------

resourcestring
  SErrNoMatch = 'No match';
  SErrNull = 'Required value is null';
  SErrBadOption = 'Bad option';
  SErrBadMagic = 'Bad magic';
  SErrUnknownNode = 'Unknown node';
  SErrNoMemory = 'Out of memory';
  SErrNoSubString = 'No substring';

//--------------------------------------------------------------------------------------------------
// JclPeImage
//--------------------------------------------------------------------------------------------------

resourcestring
  RsPeReadOnlyStream          = 'Stream is read-only';

  // TJclPeImage
  RsPeCantOpen                = 'Cannot open file "%s"';
  RsPeNotPE                   = 'This is not a PE format';
  RsPeNotResDir               = 'Not a resource directory';
  RsPeNotAvailableForAttached = 'Feature is not available for attached images';
  RsPeSectionNotFound         = 'Section "%s" not found';

  // PE directory names
  RsPeImg_00 = 'Exports';
  RsPeImg_01 = 'Imports';
  RsPeImg_02 = 'Resources';
  RsPeImg_03 = 'Exceptions';
  RsPeImg_04 = 'Security';
  RsPeImg_05 = 'Base Relocations';
  RsPeImg_06 = 'Debug';
  RsPeImg_07 = 'Description';
  RsPeImg_08 = 'Machine Value';
  RsPeImg_09 = 'TLS';
  RsPeImg_10 = 'Load configuration';
  RsPeImg_11 = 'Bound Import';
  RsPeImg_12 = 'IAT';
  RsPeImg_13 = 'Delay load import';
  RsPeImg_14 = 'COM run-time';

  // NT Header names
  RsPeSignature               = 'Signature';
  RsPeMachine                 = 'Machine';
  RsPeNumberOfSections        = 'Number of Sections';
  RsPeTimeDateStamp           = 'Time Date Stamp';
  RsPePointerToSymbolTable    = 'Symbols Pointer';
  RsPeNumberOfSymbols         = 'Number of Symbols';
  RsPeSizeOfOptionalHeader    = 'Size of Optional Header';
  RsPeCharacteristics         = 'Characteristics';
  RsPeMagic                   = 'Magic';
  RsPeLinkerVersion           = 'Linker Version';
  RsPeSizeOfCode              = 'Size of Code';
  RsPeSizeOfInitializedData   = 'Size of Initialized Data';
  RsPeSizeOfUninitializedData = 'Size of Uninitialized Data';
  RsPeAddressOfEntryPoint     = 'Address of Entry Point';
  RsPeBaseOfCode              = 'Base of Code';
  RsPeBaseOfData              = 'Base of Data';
  RsPeImageBase               = 'Image Base';
  RsPeSectionAlignment        = 'Section Alignment';
  RsPeFileAlignment           = 'File Alignment';
  RsPeOperatingSystemVersion  = 'Operating System Version';
  RsPeImageVersion            = 'Image Version';
  RsPeSubsystemVersion        = 'Subsystem Version';
  RsPeWin32VersionValue       = 'Win32 Version';
  RsPeSizeOfImage             = 'Size of Image';
  RsPeSizeOfHeaders           = 'Size of Headers';
  RsPeCheckSum                = 'CheckSum';
  RsPeSubsystem               = 'Subsystem';
  RsPeDllCharacteristics      = 'Dll Characteristics';
  RsPeSizeOfStackReserve      = 'Size of Stack Reserve';
  RsPeSizeOfStackCommit       = 'Size of Stack Commit';
  RsPeSizeOfHeapReserve       = 'Size of Heap Reserve';
  RsPeSizeOfHeapCommit        = 'Size of Heap Commit';
  RsPeLoaderFlags             = 'Loader Flags';
  RsPeNumberOfRvaAndSizes     = 'Number of RVA';

  // Load config names
  RsPeVersion                       = 'Version';
  RsPeGlobalFlagsClear              = 'GlobalFlagsClear';
  RsPeGlobalFlagsSet                = 'GlobalFlagsSet';
  RsPeCriticalSectionDefaultTimeout = 'CriticalSectionDefaultTimeout';
  RsPeDeCommitFreeBlockThreshold    = 'DeCommitFreeBlockThreshold';
  RsPeDeCommitTotalFreeThreshold    = 'DeCommitTotalFreeThreshold';
  RsPeLockPrefixTable               = 'LockPrefixTable';
  RsPeMaximumAllocationSize         = 'MaximumAllocationSize';
  RsPeVirtualMemoryThreshold        = 'VirtualMemoryThreshold';
  RsPeProcessHeapFlags              = 'ProcessHeapFlags';
  RsPeProcessAffinityMask           = 'ProcessAffinityMask';
  RsPeCSDVersion                    = 'CSDVersion';
  RsPeReserved                      = 'Reserved';
  RsPeEditList                      = 'EditList';

  // Machine names
  RsPeMACHINE_UNKNOWN = 'Unknown';
  RsPeMACHINE_I386    = 'Intel 386';
  RsPeMACHINE_R3000   = 'MIPS little-endian R3000';
  RsPeMACHINE_R4000   = 'MIPS little-endian R4000';
  RsPeMACHINE_R10000  = 'MIPS little-endian R10000';
  RsPeMACHINE_ALPHA   = 'Alpha_AXP';
  RsPeMACHINE_POWERPC = 'IBM PowerPC Little-Endian';

  // Subsystem names
  RsPeSUBSYSTEM_UNKNOWN     = 'Unknown';
  RsPeSUBSYSTEM_NATIVE      = 'Native';
  RsPeSUBSYSTEM_WINDOWS_GUI = 'GUI';
  RsPeSUBSYSTEM_WINDOWS_CUI = 'Console';
  RsPeSUBSYSTEM_OS2_CUI     = 'OS/2';
  RsPeSUBSYSTEM_POSIX_CUI   = 'Posix';
  RsPeSUBSYSTEM_RESERVED8   = 'Reserved 8';

  // Debug symbol type names
  RsPeDEBUG_UNKNOWN       = 'UNKNOWN';
  RsPeDEBUG_COFF          = 'COFF';
  RsPeDEBUG_CODEVIEW      = 'CODEVIEW';
  RsPeDEBUG_FPO           = 'FPO';
  RsPeDEBUG_MISC          = 'MISC';
  RsPeDEBUG_EXCEPTION     = 'EXCEPTION';
  RsPeDEBUG_FIXUP         = 'FIXUP';
  RsPeDEBUG_OMAP_TO_SRC   = 'OMAP_TO_SRC';
  RsPeDEBUG_OMAP_FROM_SRC = 'OMAP_FROM_SRC';
  RsPeDEBUG_BORLAND       = 'BORLAND';

  // TJclPePackageInfo.PackageModuleTypeToString
  RsPePkgExecutable = 'Executable';
  RsPePkgPackage    = 'Package';
  PsPePkgLibrary    = 'Library';

  // TJclPePackageInfo.PackageOptionsToString
  RsPePkgNeverBuild     = 'NeverBuild';
  RsPePkgDesignOnly     = 'DesignOnly';
  RsPePkgRunOnly        = 'RunOnly';
  RsPePkgIgnoreDupUnits = 'IgnoreDupUnits';

  // TJclPePackageInfo.ProducerToString
  RsPePkgV3Produced        = 'Delphi 3 or C++ Builder 3';
  RsPePkgProducerUndefined = 'Undefined';
  RsPePkgBCB4Produced      = 'C++ Builder 4 or later';
  RsPePkgDelphi4Produced   = 'Delphi 4 or later';

  // TJclPePackageInfo.UnitInfoFlagsToString
  RsPePkgMain     = 'Main';
  RsPePkgWeak     = 'Weak';
  RsPePkgOrgWeak  = 'OrgWeak';
  RsPePkgImplicit = 'Implicit';

//--------------------------------------------------------------------------------------------------
// JclPrint
//--------------------------------------------------------------------------------------------------

resourcestring
  RsSpoolerDocName = 'My Document';

  RsInvalidPrinter        = 'Invalid printer';
  RsNAStartDocument       = 'Unable to "Start document"';
  RsNASendData            = 'Unable to send data to printer';
  RsNAStartPage           = 'Unable to "Start page"';
  RsNAEndPage             = 'Unable to "End page"';
  RsNAEndDocument         = 'Unable to "End document"';
  RsNATransmission        = 'Not all chars have been sent correctly to printer';
  RsDeviceMode            = 'Error retrieving DeviceMode';
  RsUpdatingPrinter       = 'Error updating printer driver';
  RsIndexOutOfRange       = 'Index out of range setting bin';
  RsRetrievingSource      = 'Error retrieving Bin Source Info';
  RsRetrievingPaperSource = 'Error retrieving Paper Source Info';
  RsIndexOutOfRangePaper  = 'Index out of range setting paper';

  // Paper Styles (PS)
  RsPSLetter      = 'Letter 8 1/2 x 11 in';
  RsPSLetterSmall = 'Letter Small 8 1/2 x 11 in';
  RsPSTabloid     = 'Tabloid 11 x 17 in';
  RsPSLedger      = 'Ledger 17 x 11 in';
  RsPSLegal       = 'Legal 8 1/2 x 14 in';
  RsPSStatement   = 'Statement 5 1/2 x 8 1/2 in';
  RsPSExecutive   = 'Executive 7 1/2 x 10 in';
  RsPSA3          = 'A3 297 x 420 mm';
  RsPSA4          = 'A4 210 x 297 mm';
  RsPSA4Small     = 'A4 Small 210 x 297 mm';
  RsPSA5          = 'A5 148 x 210 mm';
  RsPSB4          = 'B4 250 x 354';
  RsPSB5          = 'B5 182 x 257 mm';
  RsPSFolio       = 'Folio 8 1/2 x 13 in';
  RsPSQuarto      = 'Quarto 215 x 275 mm';
  RsPS10X14       = '10 x 14 in';
  RsPS11X17       = '11 x 17 in';
  RsPSNote        = 'Note 8 1/2 x 11 in';
  RsPSEnv9        = 'Envelope #9 3 7/8 x 8 7/8 in';
  RsPSEnv10       = 'Envelope #10 4 1/8 x 9 1/2 in';
  RsPSEnv11       = 'Envelope #11 4 1/2 x 10 3/8 in';
  RsPSEnv12       = 'Envelope #12 4 \276 x 11 in';
  RsPSEnv14       = 'Envelope #14 5 x 11 1/2 in';
  RsPSCSheet      = 'C size sheet';
  RsPSDSheet      = 'D size sheet';
  RsPSESheet      = 'E size sheet';
  RsPSUser        = 'User Defined Size';
  RsPSUnknown     = 'Unknown Paper Size';

//--------------------------------------------------------------------------------------------------
// JclRegistry
//--------------------------------------------------------------------------------------------------

resourcestring
  RsUnableToOpenKeyRead  = 'Unable to open key "%s" for read';
  RsUnableToOpenKeyWrite = 'Unable to open key "%s" for write';
  RsUnableToAccessValue  = 'Unable to open key "%s" and access value "%s"';

//--------------------------------------------------------------------------------------------------
// JclRTTI
//--------------------------------------------------------------------------------------------------

resourcestring
  RsRTTIValueOutOfRange   = 'Value out of range (%s).';
  RsRTTIUnknownIdentifier = 'Unknown identifier ''%s''.';
  RsRTTIInvalidGUIDString = 'Invalid conversion from string to GUID (%s).';
  RsRTTIInvalidBaseType   = 'Invalid base type (%s is of type %s).';

  RsRTTIVar               = 'var ';
  RsRTTIConst             = 'const ';
  RsRTTIArrayOf           = 'array of ';
  RsRTTIOut               = 'out ';
  RsRTTIBits              = 'bits';
  RsRTTIOrdinal           = 'ordinal=';
  RsRTTITrue              = 'True';
  RsRTTIFalse             = 'False';
  RsRTTITypeError         = '???';
  RsRTTITypeInfoAt        = 'Type info: %p';

  RsRTTIPropRead          = 'read';
  RsRTTIPropWrite         = 'write';
  RsRTTIPropStored        = 'stored';

  RsRTTIField             = 'field';
  RsRTTIStaticMethod      = 'static method';
  RsRTTIVirtualMethod     = 'virtual method';

  RsRTTIIndex =             'index';
  RsRTTIDefault =           'default';

  RsRTTIName              = 'Name: ';
  RsRTTIType              = 'Type: ';
  RsRTTIFlags             = 'Flags: ';
  RsRTTIGUID              = 'GUID: ';
  RsRTTITypeKind          = 'Type kind: ';
  RsRTTIOrdinalType       = 'Ordinal type: ';
  RsRTTIMinValue          = 'Min value: ';
  RsRTTIMaxValue          = 'Max value: ';
  RsRTTINameList          = 'Names: ';
  RsRTTIClassName         = 'Class name: ';
  RsRTTIParent            = 'Parent: ';
  RsRTTIPropCount         = 'Property count: ';
  RsRTTIUnitName          = 'Unit name: ';
  RsRTTIBasedOn           = 'Based on: ';
  RsRTTIFloatType         = 'Float type: ';
  RsRTTIMethodKind        = 'Method kind: ';
  RsRTTIParamCount        = 'Parameter count: ';
  RsRTTIReturnType        = 'Return type: ';
  RsRTTIMaxLen            = 'Max length: ';
  RsRTTIElSize            = 'Element size: ';
  RsRTTIElType            = 'Element type: ';
  RsRTTIElNeedCleanup     = 'Elements need clean up: ';
  RsRTTIVarType           = 'Variant type: ';

  RsDeclarationFormat     = '// Declaration for ''%s'' not supported.';

//--------------------------------------------------------------------------------------------------
// JclSchedule
//--------------------------------------------------------------------------------------------------

resourcestring
  RsScheduleInvalidTime     = 'Invalid time specification';
  RsScheduleEndBeforeStart  = 'End time can not be before start time';
  RsScheduleIntervalZero    = 'Interval should be larger than 0';
  RsScheduleNoDaySpecified  = 'At least one day of the week should be specified';
  RsScheduleIndexValueSup   = 'Property IndexValue not supported for current IndexKind';
  RsScheduleIndexValueZero  = 'IndexValue can not be 0';
  RsScheduleDayNotSupported = 'Property Day not supported for current IndexKind';
  RsScheduleDayInRange      = 'Day values should fall in the range 1 .. 31';
  RsScheduleMonthInRange    = 'Month values should fall in the range 1 .. 12';

//--------------------------------------------------------------------------------------------------
// JclStatistics
//--------------------------------------------------------------------------------------------------

resourcestring
  RsInvalidSampleSize = 'Invalid sample size (%d)';

//--------------------------------------------------------------------------------------------------
// JclStrHashMap
//--------------------------------------------------------------------------------------------------

resourcestring
  RsStringHashMapMustBeEmpty = 'HashList: must be empty to set size to zero';
  RsStringHashMapDuplicate   = 'Duplicate hash list entry: %s';
  RsStringHashMapInvalidNode = 'Tried to remove invalid node: %s';
  RsStringHashMapNoTraits    = 'HashList must have traits';

//--------------------------------------------------------------------------------------------------
// JclStrings
//--------------------------------------------------------------------------------------------------

resourcestring
  RsBlankSearchString       = 'Search string cannot be blank';
  RsInvalidEmptyStringItem  = 'String list passed to StringsToMultiSz cannot contain empty strings.';
  RsNumericConstantTooLarge = 'Numeric constant too large.';

  RsStringsToMultiStringAssertion     = 'StringsToMultiString with empty item';
  RsStringsToWideMultiStringAssertion = 'StringsToWideMultiString with empty item';

//--------------------------------------------------------------------------------------------------
// JclSynch
//--------------------------------------------------------------------------------------------------

resourcestring
  RsSynchAttachWin32Handle    = 'Invalid handle to TJclWin32HandleObject.Attach';
  RsSynchDuplicateWin32Handle = 'Invalid handle to TJclWin32HandleObject.Duplicate';
  RsSynchInitCriticalSection  = 'Failed to initalize critical section';
  RsSynchAttachDispatcher     = 'Invalid handle to TJclDispatcherObject.Attach';
  RsSynchCreateEvent          = 'Failed to create event';
  RsSynchOpenEvent            = 'Failed to open event';
  RsSynchCreateWaitableTimer  = 'Failed to create waitable timer';
  RsSynchOpenWaitableTimer    = 'Failed to open waitable timer';
  RsSynchCreateSemaphore      = 'Failed to create semaphore';
  RsSynchOpenSemaphore        = 'Failed to open semaphore';
  RsSynchCreateMutex          = 'Failed to create mutex';
  RsSynchOpenMutex            = 'Failed to open mutex';
  RsMetSectInvalidParameter   = 'An invalid parameter was passed to the constructor.';
  RsMetSectInitialize         = 'Failed to initialize the metered section.';
  RsMetSectNameEmpty          = 'Name cannot be empty when using the Open constructor.';

//--------------------------------------------------------------------------------------------------
// JclSysInfo
//--------------------------------------------------------------------------------------------------

resourcestring
  RsSystemProcess     = 'System Process';
  RsSystemIdleProcess = 'System Idle Process';

  RsIntelCacheDescr01 = 'Instruction TLB, 4Kb pages, 4-way set associative, 32 entries';
  RsIntelCacheDescr02 = 'Instruction TLB, 4Mb pages, fully associative, 2 entries';
  RsIntelCacheDescr03 = 'Data TLB, 4Kb pages, 4-way set associative, 64 entries';
  RsIntelCacheDescr04 = 'Data TLB, 4Mb pages, 4-way set associative, 8 entries';
  RsIntelCacheDescr06 = '8KB instruction cache, 4-way set associative, 32 byte line size';
  RsIntelCacheDescr08 = '16KB instruction cache, 4-way set associative, 32 byte line size';
  RsIntelCacheDescr0A = '8KB data cache 2-way set associative, 32 byte line size';
  RsIntelCacheDescr0C = '16KB data cache, 4-way set associative, 32 byte line size';
  RsIntelCacheDescr40 = 'No L2 cache';
  RsIntelCacheDescr41 = 'Unified cache, 32 byte cache line, 4-way set associative, 128Kb';
  RsIntelCacheDescr42 = 'Unified cache, 32 byte cache line, 4-way set associative, 256Kb';
  RsIntelCacheDescr43 = 'Unified cache, 32 byte cache line, 4-way set associative, 512Kb';
  RsIntelCacheDescr44 = 'Unified cache, 32 byte cache line, 4-way set associative, 1Mb';
  RsIntelCacheDescr45 = 'Unified cache, 32 byte cache line, 4-way set associative, 2Mb';

  RsOSVersionWin95     = 'Windows 95';
  RsOSVersionWin95OSR2 = 'Windows 95 OSR2';
  RsOSVersionWin98     = 'Windows 98';
  RsOSVersionWin98SE   = 'Windows 98 SE';
  RsOSVersionWinME     = 'Windows ME';
  RsOSVersionWinNT3    = 'Windows NT 3.%u';
  RsOSVersionWinNT4    = 'Windows NT 4.%u';
  RsOSVersionWin2000   = 'Windows 2000';
  RsOSVersionWinXP     = 'Windows XP';
  RsOSVersionWin2003   = 'Windows Server 2003';

  RsProductTypeWorkStation      = 'Workstation';
  RsProductTypeServer           = 'Server';
  RsProductTypeAdvancedServer   = 'Advanced Server';
  RsProductTypePersonal         = 'Home Edition';
  RsProductTypeProfessional     = 'Professional';
  RsProductTypeDatacenterServer = 'Datacenter Server';

  RsOpenGLInfoError = 'Err';
  
//--------------------------------------------------------------------------------------------------
// JclSysUtils
//--------------------------------------------------------------------------------------------------

resourcestring
  RsCannotWriteRefStream = 'Can not write to a read-only memory stream';
  RsStringToBoolean      = 'Unable to convert the string "%s" to a boolean';
  RsInvalidDigit         = 'Invalid base %d digit ''%s'' encountered.';
  RsInvalidDigitValue    = 'There is no valid base %d digit for decimal value %d';

//--------------------------------------------------------------------------------------------------
// JclTD32
//--------------------------------------------------------------------------------------------------

resourcestring
  RsHasNotTD32Info = 'File [%s] has not TD32 debug information!';

//--------------------------------------------------------------------------------------------------
// JclUnicode
//--------------------------------------------------------------------------------------------------

resourcestring
  RsUREBaseString          = 'Error in regular expression: %s' + #13;
  RsUREUnexpectedEOS       = 'Unexpected end of pattern.';
  RsURECharacterClassOpen  = 'Character class not closed, '']'' is missing.';
  RsUREUnbalancedGroup     = 'Unbalanced group expression, '')'' is missing.';
  RsUREInvalidCharProperty = 'A character property is invalid';
  RsUREInvalidRepeatRange  = 'Invalid repetition range.';
  RsURERepeatRangeOpen     = 'Repetition range not closed, ''}'' is missing.';
  RsUREExpressionEmpty     = 'Expression is empty.';

//--------------------------------------------------------------------------------------------------
// JclUnitConv
//--------------------------------------------------------------------------------------------------

resourcestring
  RsTempConvTypeError = 'An invalid type has been provided for the %s parameter';
  RsConvTempBelowAbsoluteZero = 'Temperature can not be below Absolute Zero!';

//--------------------------------------------------------------------------------------------------
// JclWinMidi
//--------------------------------------------------------------------------------------------------

resourcestring
  RsMidiInUnknownError    = 'Unknown MIDI-In error No. %d';
  RsMidiOutUnknownError   = 'Unknown MIDI-Out error No. %d';

//--------------------------------------------------------------------------------------------------
// JclZlib
//--------------------------------------------------------------------------------------------------

resourcestring
  // zlib
  RsZLibNeedDict     = 'need dictionary';
  RsZLibStreamEnd    = 'stream end';
  RsZLibOK           = '';
  RsZLibErrNo        = 'file error';
  RsZLibStreamError  = 'stream error';
  RsZLibDataError    = 'data error';
  RsZLibMemError     = 'insufficient memory';
  RsZLibBufError     = 'buffer error';
  RsZLibVersionError = 'incompatible version';
  RsZLibUnknownError = 'unknown zlib error';
  RsZLibNoSetSize    = 'TZLibStream cannot perform set size';
  RsZLibNoSeek       = 'TZLibStream cannot perform seek';
  RsZLibNoWrite      = 'TZLibReader cannot write';
  RsZLibNoRead       = 'TZLibWriter cannot read';

  // gzip
  RsGzipNoSetSize = 'gzip stream cannot perform set size';
  RsGzipNoSeek    = 'gzip stream cannot perform seek';
  RsGzipNoWrite   = 'gzip reader cannot write';
  RsGzipNoRead    = 'gzip writer cannot read';

  RsGzipNoGZipStream          = 'no gzip stream';
  RsGzipNoDeflate             = 'no deflate compression';
  RsGzipMultipartNotSupported = 'multipart gzip files are not supported';
  RsGzipEncryptedNotSupported = 'encrypted gzip files are not supported';
  RsGzipUnknownFlags          = 'unknown flags';
  RsGzipCRCError              = 'checksum error';
  RsGzipSizeError             = 'uncompressed size error';

  // Tar
  RsTarOctalToIntInvalidCharacters = 'OctalToInt invalid characters: "%s"';
  RsTarOctalToIntOutOfRange        = 'OctalToInt out of range: "%s"';
  RsTarChecksumError               = 'TAR Checksum Error';
  RsTarSetOctalOutOfRange          = 'SetOctal: out of range';

implementation

// History:

// $Log$
// Revision 1.15  2004/09/30 13:11:27  marquardt
// remove PH contributions
//
// Revision 1.14  2004/09/30 08:09:07  marquardt
// remove JclDITs remains
//
// Revision 1.13  2004/08/23 10:13:58  scottprice
// Modified temperature routines, and added support for Rankine and Reaumur.  Added some string constants to this unit related to that change.
//
// Revision 1.12  2004/08/18 17:10:27  rrossmair
// added RsInvalidSampleSize for JclStatistics
//
// Revision 1.11  2004/08/03 07:22:37  marquardt
// resourcestring cleanup
//
// Revision 1.10  2004/08/02 15:30:16  marquardt
// hunting down (rom) comments
//
// Revision 1.9  2004/07/28 18:00:51  marquardt
// various style cleanings, some minor fixes
//
// Revision 1.8  2004/06/14 11:05:51  marquardt
// symbols added to all ENDIFs and some other minor style changes like removing IFOPT
//
// Revision 1.7  2004/06/02 03:23:46  rrossmair
// cosmetic changes in several units (code formatting, help TODOs processed etc.)
//
// Revision 1.6  2004/05/14 15:26:34  rrossmair
// header updated according to new policy: initial developers & contributors listed
//
// Revision 1.5  2004/04/18 00:41:04  peterjhaas
// remove unneeded OpenGL error messages
//
// Revision 1.4  2004/04/06 04:38:57  peterjhaas
// Add resources for DIT and ZLib
//

end.
