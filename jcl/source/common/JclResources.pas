{******************************************************************************}
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.0 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JclResources.pas.                                       }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ 2000 of these individuals.                                                   }
{                                                                              }
{ Last modified: December 13, 2000                                             }
{                                                                              }
{******************************************************************************}

unit JclResources;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

//------------------------------------------------------------------------------
// JclBase
//------------------------------------------------------------------------------

resourcestring
  RsWin32Prefix   = 'Win32: %s (%u)';
  RsDynArrayError = 'DynArrayInitialize: ElementSize out of bounds';

//------------------------------------------------------------------------------
// JclUnicode
//------------------------------------------------------------------------------

resourcestring
  RsUREBaseString = 'Error in regular expression: %s' + #13;
  RsUREUnexpectedEOS = 'Unexpected end of pattern.';
  RsURECharacterClassOpen = 'Character class not closed, '']'' is missing.';
  RsUREUnbalancedGroup = 'Unbalanced group expression, '')'' is missing.';
  RsUREInvalidCharProperty = 'A character property is invalid';
  RsUREInvalidRepeatRange = 'Invalid repeation range.';
  RsURERepeatRangeOpen = 'Repeation range not closed, ''}'' is missing.';
  RsUREExpressionEmpty = 'Expression is empty.';

//------------------------------------------------------------------------------
// JclClasses
//------------------------------------------------------------------------------

  // VMT

resourcestring
  RsVMTMemoryWriteError = 'Error writing VMT memory (%s)';

//------------------------------------------------------------------------------
// JclCounter
//------------------------------------------------------------------------------

resourcestring
  RsNoCounter = 'No high performance counters supported';

//------------------------------------------------------------------------------
// JclDateTime
//------------------------------------------------------------------------------

resourcestring
  RsMakeUTCTime    = 'Error converting to UTC time. Time zone could not be determined';
  RsDateConversion = 'Error illegal date or time format';

//------------------------------------------------------------------------------
// JclDebug
//------------------------------------------------------------------------------

  // Diagnostics

resourcestring
  RsDebugAssertValidPointer = 'Invalid Pointer passed to AssertValid';
  RsDebugAssertValidString = 'Invalid string passed to AssertValid';

  // TMapFiles

  RsDebugMapFileExtension = '.map'; // do not localize
  RsDebugNoProcessInfo    = 'Unable to obtain process information';
  RsDebugSnapshot         = 'Failure creating toolhelp32 snapshot';

//------------------------------------------------------------------------------
// JclFileUtils
//------------------------------------------------------------------------------

resourcestring

  // Path manipulation

  RsPathInvalidDrive = '%s is not a valid drive';

  // Files and directories

  RsFileUtilsAttrUnavailable = 'Unable to retrieve attributes of %s';

  RsCannotCreateDir = 'Unable to create directory';

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

  // TJclFileStream

  RsFileStreamCreate         = 'Unable to create temporary file stream';

  // TJclFileMapping

  RsCreateFileMapping        = 'Failed to create FileMapping';
  RsCreateFileMappingView    = 'Failed to create FileMappingView';
  RsLoadFromStreamSize       = 'Not enough space in View in procedure LoadFromStream';
  RsFileMappingInvalidHandle = 'Invalid filehandle';
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

//------------------------------------------------------------------------------
// JclGraphics, JclGraphUtils
//------------------------------------------------------------------------------

resourcestring
  RsAssertUnpairedEndUpdate  = 'Unpaired beginupdate endupdate';
  RsCreateCompatibleDc       = 'Could not create compatible DC';
  RsDestinationBitmapEmpty   = 'Destination bitmap is empty';
  RsDibHandleAllocation      = 'Could not allocate handle for DIB';
  RsMapSizeFmt               = 'Could not set size on class "%s"';
  RsSelectObjectInDc         = 'Could not select object in DC';
  RsSourceBitmapEmpty        = 'Source bitmap is empty';
  RsSourceBitmapInvalid      = 'Source bitmap is invalid';
  RsNoBitmapForRegion        = 'No bitmap for region';
  RsNoDeviceContextForWindow = 'Cannot get device context of the window';
  RsInvalidRegion            = 'Invalid Region defined for RegionInfo';
  RsRegionDataOutOfBound     = 'Out of bound index on regiondata';
  RsRegionCouldNotCreated    = 'Region could not be created';
  RsInvalidHandleForRegion   = 'Invalid handle for region';
  RsInvalidRegionInfo        = 'Invalid RegionInfo';

//------------------------------------------------------------------------------
// JclMapi
//------------------------------------------------------------------------------

resourcestring
  RsMapiError         = 'MAPI Error: (%d) "%s"';
  RsMapiMissingExport = 'Function "%s" is not exported by client';

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

  RsMapiMailNoClient = 'No Simple MAPI client installed, can''t send the message';

  RsMapiMailTO          = 'To';
  RsMapiMailCC          = 'Cc';
  RsMapiMailBCC         = 'Bcc';
  RsMapiMailSubject     = 'Subject';
  RsMapiMailBody        = 'Body';
  RsMapiMailAttachments = 'Attachments';

//------------------------------------------------------------------------------
// JclMath
//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------
// JclMime
//------------------------------------------------------------------------------

resourcestring
  RsInputBufferNil  = 'MIME (Base64) Conversion: Input Buffer must not be NIL';
  RsOutputBufferNil = 'MIME (Base64) Conversion: Output Buffer must not be NIL';

//------------------------------------------------------------------------------
// JclMiscel
//------------------------------------------------------------------------------

resourcestring
  // StrLstSaveLoad
  RsErrNotExist = 'Specified list ["%s"] does not exist';

  // CreateProcAsUser
  RsCreateProcOSVersionError          = 'Unable to determine OS version';
  RsCreateProcNTRequiredError         = 'Windows NT required';
  RsCreateProcBuild1057Error          = 'NT version 3.51 build 1057 or later required';

  RsCreateProcPrivilegeMissing        = 'This account does not have the privilege "%s"(%s)';
  RsCreateProcLogonUserError          = 'LogonUser failed';
  RsCreateProcAccessDenied            = 'Access denied';
  RsCreateProcLogonFailed             = 'Unable to logon';
  RsCreateProcSetStationSecurityError = 'Can''t set WindowStation "%s" security.';
  RsCreateProcSetDesktopSecurityError = 'Can''t set Desktop "%s" security.';
  RsCreateProcPrivilegesMissing       = 'This account does not have on (or more) of ' +
    'the following privileges: ' + '"%s"(%s)' + #13 + '"%s"(%s)' + #13;
  RsCreateProcCommandNotFound         = 'Command or filename not found: "%s"';
  RsCreateProcFailed                  = 'CreateProcessAsUser failed';                                  

//------------------------------------------------------------------------------
// JclMultimedia
//------------------------------------------------------------------------------

resourcestring
  RsMmTimerGetCaps     = 'Error retrieving multimedia timer device capabilities';
  RsMmTimerBeginPeriod = 'The supplied timer period value is out of range';
  RsMmSetEvent         = 'Error setting multimedia event timer';
  RsMmInconsistentId   = 'Multimedia timer callback was called with inconsistent id';
  RsMmTimerActive      = 'This operation cannot be performed while the timer is active';

  RsMmNoCdAudio        = 'Cannot open CDAUDIO-Device!';

  RsMmUnknownError     = 'Unknown MCI error Nr. %d';
  RsMmMciErrorPrefix   = 'MCI-Error: ';

//------------------------------------------------------------------------------
// JclNTFS
//------------------------------------------------------------------------------

resourcestring
  RsNtfsUnableToDeleteSymbolicLink = 'Unable to delete temporary symbolic link';

//------------------------------------------------------------------------------
// JclPeImage
//------------------------------------------------------------------------------

  // TJclPeImage

resourcestring
  RsPeCantOpen = 'Can''t open file "%s"';
  RsPeNotPE = 'This is not a PE format';
  RsPeUnexpected = 'Unexpected error';
  RsPeNotResDir = 'Not a resource directory';
  RsPeNotAvailableForAttached = 'Feature is not available for attached images';
  RsPeSectionNotFound = 'Section "%s" not found';

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

  RsPeSignature = 'Signature';
  RsPeMachine = 'Machine';
  RsPeNumberOfSections = 'Number of Sections';
  RsPeTimeDateStamp = 'Time Date Stamp';
  RsPePointerToSymbolTable = 'Symbols Pointer';
  RsPeNumberOfSymbols = 'Number of Symbols';
  RsPeSizeOfOptionalHeader = 'Size of Optional Header';
  RsPeCharacteristics = 'Characteristics';
  RsPeMagic = 'Magic';
  RsPeLinkerVersion = 'Linker Version';
  RsPeSizeOfCode = 'Size of Code';
  RsPeSizeOfInitializedData = 'Size of Initialized Data';
  RsPeSizeOfUninitializedData = 'Size of Uninitialized Data';
  RsPeAddressOfEntryPoint = 'Address of Entry Point';
  RsPeBaseOfCode = 'Base of Code';
  RsPeBaseOfData = 'Base of Data';
  RsPeImageBase = 'Image Base';
  RsPeSectionAlignment = 'Section Alignment';
  RsPeFileAlignment = 'File Alignment';
  RsPeOperatingSystemVersion = 'Operating System Version';
  RsPeImageVersion = 'Image Version';
  RsPeSubsystemVersion = 'Subsystem Version';
  RsPeWin32VersionValue = 'Win32 Version';
  RsPeSizeOfImage = 'Size of Image';
  RsPeSizeOfHeaders = 'Size of Headers';
  RsPeCheckSum = 'CheckSum';
  RsPeSubsystem = 'Subsystem';
  RsPeDllCharacteristics = 'Dll Characteristics';
  RsPeSizeOfStackReserve = 'Size of Stack Reserve';
  RsPeSizeOfStackCommit = 'Size of Stack Commit';
  RsPeSizeOfHeapReserve = 'Size of Heap Reserve';
  RsPeSizeOfHeapCommit = 'Size of Heap Commit';
  RsPeLoaderFlags = 'Loader Flags';
  RsPeNumberOfRvaAndSizes = 'Number of RVA';

  // Load config names

  RsPeVersion = 'Version';
  RsPeGlobalFlagsClear = 'GlobalFlagsClear';
  RsPeGlobalFlagsSet = 'GlobalFlagsSet';
  RsPeCriticalSectionDefaultTimeout = 'CriticalSectionDefaultTimeout';
  RsPeDeCommitFreeBlockThreshold = 'DeCommitFreeBlockThreshold';
  RsPeDeCommitTotalFreeThreshold = 'DeCommitTotalFreeThreshold';
  RsPeLockPrefixTable = 'LockPrefixTable';
  RsPeMaximumAllocationSize = 'MaximumAllocationSize';
  RsPeVirtualMemoryThreshold = 'VirtualMemoryThreshold';
  RsPeProcessHeapFlags = 'ProcessHeapFlags';
  RsPeProcessAffinityMask = 'ProcessAffinityMask';
  RsPeCSDVersion = 'CSDVersion';
  RsPeReserved = 'Reserved';
  RsPeEditList = 'EditList';

  // Machine names

  RsPeMACHINE_UNKNOWN = 'Unknown';
  RsPeMACHINE_I386 = 'Intel 386';
  RsPeMACHINE_R3000 = 'MIPS little-endian R3000';
  RsPeMACHINE_R4000 = 'MIPS little-endian R4000';
  RsPeMACHINE_R10000 = 'MIPS little-endian R10000';
  RsPeMACHINE_ALPHA = 'Alpha_AXP';
  RsPeMACHINE_POWERPC = 'IBM PowerPC Little-Endian';

  // Subsystem names

  RsPeSUBSYSTEM_UNKNOWN = 'Unknown';
  RsPeSUBSYSTEM_NATIVE = 'Native';
  RsPeSUBSYSTEM_WINDOWS_GUI = 'GUI';
  RsPeSUBSYSTEM_WINDOWS_CUI = 'Console';
  RsPeSUBSYSTEM_OS2_CUI = 'OS/2';
  RsPeSUBSYSTEM_POSIX_CUI = 'Posix';
  RsPeSUBSYSTEM_RESERVED8 = 'Reserved 8';

  // Debug symbol type names

  RsPeDEBUG_UNKNOWN = 'UNKNOWN';
  RsPeDEBUG_COFF = 'COFF';
  RsPeDEBUG_CODEVIEW = 'CODEVIEW';
  RsPeDEBUG_FPO = 'FPO';
  RsPeDEBUG_MISC = 'MISC';
  RsPeDEBUG_EXCEPTION = 'EXCEPTION';
  RsPeDEBUG_FIXUP = 'FIXUP';
  RsPeDEBUG_OMAP_TO_SRC = 'OMAP_TO_SRC';
  RsPeDEBUG_OMAP_FROM_SRC = 'OMAP_FROM_SRC';
  RsPeDEBUG_BORLAND = 'BORLAND';

//------------------------------------------------------------------------------
// JclPrint
//------------------------------------------------------------------------------

resourcestring
  RsInvalidPrinter  = 'Invalid printer';
  RsNAStartDocument = 'Unable to "Start document"';
  RsNASendData      = 'Unable to send data to printer';
  RsNAStartPage     = 'Unable to "Start page"';
  RsNAEndPage       = 'Unable to "End page"';
  RsNAEndDocument   = 'Unable to "End document"';
  RsNATransmission  = 'Not all chars have been sent correctly to printer';
  RsDeviceMode      = 'Error retrieving DeviceMode';
  RsUpdatingPrinter = 'Error updating printer driver';
  RsIndexOutOfRange = 'Index out of range setting bin';
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
  RsPS10X14       = '10x14 in';
  RsPS11X17       = '11x17 in';
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

//------------------------------------------------------------------------------
// JclRegistry
//------------------------------------------------------------------------------

resourcestring
  RsUnableToOpenKeyRead  = 'Unable to open the "%s" key for read';
  RsUnableToOpenKeyWrite = 'Unable to open the "%s" key for write';

//------------------------------------------------------------------------------
// JclSscanf
//------------------------------------------------------------------------------

resourcestring
  RsSscanfBadSet       = 'Bad set: ';
  RsSscanfBadFormat    = 'Bad format string';
  RsSscanfInsufficient = 'Insufficent pointers for format specifiers';

//------------------------------------------------------------------------------
// JclSynch
//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------
// JclComplex
//------------------------------------------------------------------------------

resourcestring
  RsComplexInvalidString = 'Failed to create a complex number from the string provided';

implementation

end.
