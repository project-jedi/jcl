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
{ The Original Code is JclNTFS.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel. Portions created by Marcel van  }
{ Brakel are Copyright (C) Marcel van Brakel. All Rights Reserved.                                 }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel van Brakel                                                                              }
{   Peter J. Haas (peterjhaas)                                                                     }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Petr Vones (pvones)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Contains routines to perform filesystem related tasks available only with NTFS. These are mostly }
{ relatively straightforward wrappers for various IOCTs related to compression, sparse files,      }
{ reparse points, volume mount points and so forth. Note that some functions require NTFS 5 or     }
{ higher!                                                                                          }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file


// Comments on Win9x compatibility of the functions used in this unit

// The following functions exist at least since Win95C, but invariably returns
// ERROR_CALL_NOT_IMPLEMENTED:
//   BackupSeek, BackupRead, BackupWrite

unit JclNTFS;

{$I jcl.inc}
{$I windowsonly.inc}

interface

uses
  Windows, Classes,
  JclBase, JclWin32;

//--------------------------------------------------------------------------------------------------
// NTFS Exception
//--------------------------------------------------------------------------------------------------

type
  EJclNtfsError = class(EJclWin32Error);

//--------------------------------------------------------------------------------------------------
// NTFS - Compression
//--------------------------------------------------------------------------------------------------

type
  TFileCompressionState = (fcNoCompression, fcDefaultCompression, fcLZNT1Compression);

function NtfsGetCompression(const FileName: string; var State: Short): Boolean; overload;
function NtfsGetCompression(const FileName: string): TFileCompressionState; overload;
function NtfsSetCompression(const FileName: string; const State: Short): Boolean;
procedure NtfsSetFileCompression(const FileName: string; const State: TFileCompressionState);
procedure NtfsSetDirectoryTreeCompression(const Directory: string; const State: TFileCompressionState);
procedure NtfsSetDefaultFileCompression(const Directory: string; const State: TFileCompressionState);
procedure NtfsSetPathCompression(const Path: string; const State: TFileCompressionState; Recursive: Boolean);

//--------------------------------------------------------------------------------------------------
// NTFS - Sparse Files
//--------------------------------------------------------------------------------------------------

type
  TNtfsAllocRanges = record
    Entries: Integer;
    Data: PFileAllocatedRangeBuffer;
    MoreData: Boolean;
  end;

function NtfsSetSparse(const FileName: string): Boolean;
function NtfsZeroDataByHandle(const Handle: THandle; const First, Last: Int64): Boolean;
function NtfsZeroDataByName(const FileName: string; const First, Last: Int64): Boolean;
function NtfsQueryAllocRanges(const FileName: string; Offset, Count: Int64; var Ranges: TNtfsAllocRanges): Boolean;
function NtfsGetAllocRangeEntry(const Ranges: TNtfsAllocRanges; Index: Integer): TFileAllocatedRangeBuffer;
function NtfsSparseStreamsSupported(const Volume: string): Boolean;
function NtfsGetSparse(const FileName: string): Boolean;

//--------------------------------------------------------------------------------------------------
// NTFS - Reparse Points
//--------------------------------------------------------------------------------------------------

function NtfsDeleteReparsePoint(const FileName: string; ReparseTag: DWORD): Boolean;
function NtfsSetReparsePoint(const FileName: string; var ReparseData; Size: Longword): Boolean;
function NtfsGetReparsePoint(const FileName: string; var ReparseData: TReparseGuidDataBuffer): Boolean;
function NtfsGetReparseTag(const Path: string; var Tag: DWORD): Boolean;
function NtfsReparsePointsSupported(const Volume: string): Boolean;
function NtfsFileHasReparsePoint(const Path: string): Boolean;

//--------------------------------------------------------------------------------------------------
// NTFS - Volume Mount Points
//--------------------------------------------------------------------------------------------------

function NtfsIsFolderMountPoint(const Path: string): Boolean;
function NtfsMountDeviceAsDrive(const Device: string; Drive: Char): Boolean;
function NtfsMountVolume(const Volume: Char; const MountPoint: string): Boolean;

//--------------------------------------------------------------------------------------------------
// NTFS - Change Journal
//--------------------------------------------------------------------------------------------------

//--------------------------------------------------------------------------------------------------
// NTFS - Opportunistic Locks
//--------------------------------------------------------------------------------------------------

type
  TOpLock = (olExclusive, olReadOnly, olBatch, olFilter);

function NtfsOpLockAckClosePending(Handle: THandle; Overlapped: TOverlapped): Boolean;
function NtfsOpLockBreakAckNo2(Handle: THandle; Overlapped: TOverlapped): Boolean;
function NtfsOpLockBreakAcknowledge(Handle: THandle; Overlapped: TOverlapped): Boolean;
function NtfsOpLockBreakNotify(Handle: THandle; Overlapped: TOverlapped): Boolean;
function NtfsRequestOpLock(Handle: THandle; Kind: TOpLock; Overlapped: TOverlapped): Boolean;

//--------------------------------------------------------------------------------------------------
// Junction Points
//--------------------------------------------------------------------------------------------------

function NtfsCreateJunctionPoint(const Source, Destination: string): Boolean;
function NtfsDeleteJunctionPoint(const Source: string): Boolean;
function NtfsGetJunctionPointDestination(const Source: string; var Destination: string): Boolean;

//--------------------------------------------------------------------------------------------------
// Streams
//--------------------------------------------------------------------------------------------------

type
  TStreamId = (siInvalid, siStandard, siExtendedAttribute, siSecurity, siAlternate,
    siHardLink, siProperty, siObjectIdentifier, siReparsePoints, siSparseFile);
  TStreamIds = set of TStreamId;

  TInternalFindStreamData = record
    FileHandle: THandle;
    Context: Pointer;
    StreamIds: TStreamIds;
  end;

  TFindStreamData = record
    Internal: TInternalFindStreamData;
    Attributes: DWORD;
    StreamID: TStreamId;
    Name: WideString;
    Size: Int64;
  end;

function NtfsFindFirstStream(const FileName: string; StreamIds: TStreamIds; var Data: TFindStreamData): Boolean;
function NtfsFindNextStream(var Data: TFindStreamData): Boolean;
function NtfsFindStreamClose(var Data: TFindStreamData): Boolean;

//--------------------------------------------------------------------------------------------------
// Hard links
//--------------------------------------------------------------------------------------------------

function NtfsCreateHardLink(const LinkFileName, ExistingFileName: string): Boolean;

type
  TNtfsHardLinkInfo = record
    LinkCount: Cardinal;
    case Integer of
    0: (
      FileIndexHigh: Cardinal;
      FileIndexLow: Cardinal);
    1: (
      FileIndex: Int64);
  end;

function NtfsGetHardLinkInfo(const FileName: string; var Info: TNtfsHardLinkInfo): Boolean;

function NtfsFindHardLinks(const Path: string; const FileIndexHigh, FileIndexLow: Cardinal; const List: TStrings): Boolean;
function NtfsDeleteHardLinks(const FileName: string): Boolean;

implementation

uses
  {$IFDEF FPC}
  WinSysUt,
  {$ENDIF FPC}
  SysUtils,
  JclFileUtils, JclResources, JclSecurity;

//==================================================================================================
// NTFS - Compression
//==================================================================================================

// Helper consts, helper types, helper routines

const
  CompressionFormat: array [TFileCompressionState] of Short =
  (
    COMPRESSION_FORMAT_NONE,
    COMPRESSION_FORMAT_DEFAULT,
    COMPRESSION_FORMAT_LZNT1
  );

  // use IsDirectory(FileName) as array index
  FileFlag: array [Boolean] of DWORD = (0, FILE_FLAG_BACKUP_SEMANTICS);

type
  TStackFrame = packed record
    CallersEBP: DWord;
    CallerAddress: DWord;
  end;

  EJclInvalidArgument = class(EJclError);

//--------------------------------------------------------------------------------------------------

{$STACKFRAMES OFF}

function CallersCallerAddress: Pointer;
asm
        MOV     EAX, [EBP]
        MOV     EAX, TStackFrame([EAX]).CallerAddress
end;

//--------------------------------------------------------------------------------------------------

{$STACKFRAMES ON}

procedure ValidateArgument(Condition: Boolean; const Routine: string;
  const Argument: string);
begin
  if not Condition then
    raise EJclInvalidArgument.CreateResRecFmt(@RsInvalidArgument, [Routine, Argument])
      at CallersCallerAddress;
end;

{$IFNDEF STACKFRAMES_ON}
{$STACKFRAMES OFF}
{$ENDIF ~STACKFRAMES_ON}

//--------------------------------------------------------------------------------------------------

function SetCompression(const FileName: string; const State: Short; FileFlag: DWORD): Boolean;
var
  Handle: THandle;
  BytesReturned: DWORD;
  Buffer: Short;
begin
  Result := False;
  Handle := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ, nil, OPEN_EXISTING, FileFlag, 0);
  if Handle <> INVALID_HANDLE_VALUE then
  try
    Buffer := State;
    Result := DeviceIoControl(Handle, FSCTL_SET_COMPRESSION, @Buffer,
      SizeOf(Short), nil, 0, BytesReturned, nil);
  finally
    CloseHandle(Handle);
  end
end;

//--------------------------------------------------------------------------------------------------

function SetPathCompression(Dir: string; const Mask: string; const State: Short;
  const SetDefault, Recursive: Boolean): Boolean;
var
  FileName: string;
  SearchRec: TSearchRec;
  R: Integer;
begin
  if SetDefault then
    Result := SetCompression(Dir, State, FILE_FLAG_BACKUP_SEMANTICS)
  else
    Result := True;
  if Result then
  begin
    Dir := PathAddSeparator(Dir);
    if FindFirst(Dir + Mask, faAnyFile, SearchRec) = 0 then
    try
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          FileName := Dir + SearchRec.Name;
          if (SearchRec.Attr and faDirectory) = 0 then
            Result := SetCompression(FileName, State, 0)
          else
            if Recursive then
              Result := SetPathCompression(FileName, Mask, State, SetDefault, True);
          if not Result then
            Exit;
        end;
        R := FindNext(SearchRec);
      until R <> 0;
      Result := (R = ERROR_NO_MORE_FILES);
    finally
      SysUtils.FindClose(SearchRec);
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function NtfsGetCompression(const FileName: string; var State: Short): Boolean;
var
  Handle: THandle;
  BytesReturned: DWORD;
begin
  Result := False;
  Handle := CreateFile(PChar(FileName), 0, 0, nil, OPEN_EXISTING,
    FileFlag[IsDirectory(FileName)], 0);
  if Handle <> INVALID_HANDLE_VALUE then
    try
      Result := DeviceIoControl(Handle, FSCTL_GET_COMPRESSION, nil, 0, @State,
        SizeOf(Short), BytesReturned, nil);
    finally
      CloseHandle(Handle);
    end;
end;

//--------------------------------------------------------------------------------------------------

function NtfsGetCompression(const FileName: string): TFileCompressionState;
var
  State: Short;
begin
  if not NtfsGetCompression(FileName, State) then
    RaiseLastOSError;
  case State of
    COMPRESSION_FORMAT_NONE:
      Result := fcNoCompression;
    COMPRESSION_FORMAT_LZNT1:
      Result := fcLZNT1Compression;
  else
    Assert(False, 'TFileCompressionState requires expansion');
    Result := TFileCompressionState(State);
  end;
end;

//--------------------------------------------------------------------------------------------------

function NtfsSetCompression(const FileName: string; const State: Short): Boolean;
begin
  Result := SetCompression(FileName, State, FileFlag[IsDirectory(FileName)]);
end;

//--------------------------------------------------------------------------------------------------

{$STACKFRAMES ON}

procedure NtfsSetFileCompression(const FileName: string; const State: TFileCompressionState);
begin
  ValidateArgument(not IsDirectory(FileName), 'NtfsSetFileCompression', 'FileName');
  if not SetCompression(FileName, CompressionFormat[State], 0) then
    RaiseLastOSError;
end;

//--------------------------------------------------------------------------------------------------

procedure NtfsSetDefaultFileCompression(const Directory: string; const State: TFileCompressionState);
begin
  ValidateArgument(IsDirectory(Directory), 'NtfsSetDefaultFileCompression', 'Directory');
  if not SetCompression(Directory, CompressionFormat[State], FILE_FLAG_BACKUP_SEMANTICS) then
    RaiseLastOSError;
end;

//--------------------------------------------------------------------------------------------------

procedure NtfsSetDirectoryTreeCompression(const Directory: string; const State: TFileCompressionState);
begin
  ValidateArgument(IsDirectory(Directory), 'NtfsSetDirectoryTreeCompression', 'Directory');
  if not SetPathCompression(Directory, '*', CompressionFormat[State], True, True) then
    RaiseLastOSError;
end;

{$IFNDEF STACKFRAMES_ON}
{$STACKFRAMES OFF}
{$ENDIF ~STACKFRAMES_ON}

//--------------------------------------------------------------------------------------------------

procedure NtfsSetPathCompression(const Path: string;
  const State: TFileCompressionState; Recursive: Boolean);
var
  Dir, Mask: string;
  SetDefault: Boolean;
begin
  SetDefault := IsDirectory(Path);
  if SetDefault then
  begin
    Dir := Path;
    Mask := '*';
  end
  else
  begin
    Dir := ExtractFilePath(Path);
    Mask := ExtractFileName(Path);
    if Mask = '' then
      Mask := '*';
  end;
  if not SetPathCompression(Dir, Mask, CompressionFormat[State], SetDefault, Recursive) then
    RaiseLastOSError;
end;

//==================================================================================================
// NTFS - Sparse Files
//==================================================================================================

function NtfsSetSparse(const FileName: string): Boolean;
var
  Handle: THandle;
  BytesReturned: DWORD;
begin
  Result := False;
  Handle := CreateFile(PChar(FileName), GENERIC_WRITE, 0, nil, OPEN_EXISTING, 0, 0);
  if Handle <> INVALID_HANDLE_VALUE then
    try
      Result := DeviceIoControl(Handle, FSCTL_SET_SPARSE, nil, 0, nil, 0, BytesReturned, nil);
    finally
      CloseHandle(Handle);
    end;
end;

//--------------------------------------------------------------------------------------------------

function NtfsZeroDataByHandle(const Handle: THandle; const First, Last: Int64): Boolean;
var
  BytesReturned: DWORD;
  ZeroDataInfo: TFileZeroDataInformation;
  Info: TByHandleFileInformation;
begin
  Result := False;
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    // Continue only if the file is a sparse file, this avoids the overhead
    // associated with an IOCTL when the file isn't even a sparse file.
    GetFileInformationByHandle(Handle, Info);
    Result := (Info.dwFileAttributes and FILE_ATTRIBUTE_SPARSE_FILE) <> 0;
    if Result then
    begin
      ZeroDataInfo.FileOffset.QuadPart := First;
      ZeroDataInfo.BeyondFinalZero.QuadPart := Last;
      Result := DeviceIoControl(Handle, FSCTL_SET_ZERO_DATA, @ZeroDataInfo,
        SizeOf(ZeroDataInfo), nil, 0, BytesReturned, nil);
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function NtfsZeroDataByName(const FileName: string; const First, Last: Int64): Boolean;
var
  Handle: THandle;
begin
  Result := False;
  Handle := CreateFile(PChar(FileName), GENERIC_WRITE, 0, nil, OPEN_EXISTING, 0, 0);
  if Handle <> INVALID_HANDLE_VALUE then
    try
      Result := NtfsZeroDataByHandle(Handle, First, Last);
    finally
      CloseHandle(Handle);
    end;
end;

//--------------------------------------------------------------------------------------------------

function NtfsGetAllocRangeEntry(const Ranges: TNtfsAllocRanges;
  Index: Integer): TFileAllocatedRangeBuffer;
var
  Offset: Longint;
begin
  Assert((Index >= 0) and (Index < Ranges.Entries));
  Offset := Longint(Ranges.Data) + Index * SizeOf(TFileAllocatedRangeBuffer);
  Result := PFileAllocatedRangeBuffer(Offset)^;
end;

//--------------------------------------------------------------------------------------------------

function __QueryAllocRanges(const Handle: THandle; const Offset, Count: Int64;
  var Ranges: PFileAllocatedRangeBuffer; var MoreData: Boolean; var Size: Cardinal): Boolean;
var
  BytesReturned: DWORD;
  SearchRange: TFileAllocatedRangeBuffer;
  BufferSize: Cardinal;
begin
  SearchRange.FileOffset.QuadPart := Offset;
  SearchRange.Length.QuadPart := Count;
  BufferSize := 4 * 64 * SizeOf(TFileAllocatedRangeBuffer);
  Ranges := AllocMem(BufferSize);
  Result := DeviceIoControl(Handle, FSCTL_QUERY_ALLOCATED_RANGES, @SearchRange,
    SizeOf(SearchRange), Ranges, BufferSize, BytesReturned, nil);
  MoreData := GetLastError = ERROR_MORE_DATA;
  if MoreData then
    Result := True;
  Size := BytesReturned;
  if BytesReturned = 0 then
  begin
    FreeMem(Ranges);
    Ranges := nil;
  end;
end;

function NtfsQueryAllocRanges(const FileName: string; Offset, Count: Int64;
  var Ranges: TNtfsAllocRanges): Boolean;
var
  Handle: THandle;
  CurrRanges: PFileAllocatedRangeBuffer;
  R, MoreData: Boolean;
  Size: Cardinal;
begin
  Result := False;
  Handle := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  if Handle <> INVALID_HANDLE_VALUE then
  try
    R := __QueryAllocRanges(Handle, Offset, Count, CurrRanges, MoreData, Size);
    Ranges.MoreData := MoreData;
    Result := R;
    if R then
    begin
      Ranges.Entries := Size div SizeOf(TFileAllocatedRangeBuffer);
      Ranges.Data := CurrRanges;
    end
    else
    begin
      Ranges.Entries := 0;
      Ranges.Data := nil;
    end;
  finally
    CloseHandle(Handle);
  end;
end;

//--------------------------------------------------------------------------------------------------

function NtfsSparseStreamsSupported(const Volume: string): Boolean;
var
  MCL, Flags: Cardinal;
begin
  Result := GetVolumeInformation(PChar(Volume), nil, 0, nil, MCL, Flags, nil, 0);
  if Result then
    Result := (Flags and FILE_SUPPORTS_SPARSE_FILES) <> 0;
end;

//--------------------------------------------------------------------------------------------------

function NtfsGetSparse(const FileName: string): Boolean;
var
  Handle: THandle;
  Info: TByHandleFileInformation;
begin
  Result := False;
  Handle := CreateFile(PChar(FileName), 0, FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil, OPEN_EXISTING, 0, 0);
  if Handle <> INVALID_HANDLE_VALUE then
    try
      GetFileInformationByHandle(Handle, Info);
      Result := (Info.dwFileAttributes and FILE_ATTRIBUTE_SPARSE_FILE) <> 0;
    finally
      CloseHandle(Handle);
    end;
end;

//==================================================================================================
// NTFS - Reparse Points
//==================================================================================================

function NtfsGetReparseTag(const Path: string; var Tag: DWORD): Boolean;
var
  SearchRec: TSearchRec;
begin
  Result := NtfsFileHasReparsePoint(Path);
  if Result then
  begin
    Result := FindFirst(Path, faAnyFile, SearchRec) = 0;
    if Result then
    begin
      // Check if file has a reparse point
      Result := ((SearchRec.Attr and FILE_ATTRIBUTE_REPARSE_POINT) <> 0);
      // If so the dwReserved0 field contains the reparse tag
      if Result then
        Tag := SearchRec.FindData.dwReserved0;
      FindClose(SearchRec);
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function NtfsReparsePointsSupported(const Volume: string): Boolean;
var
  MCL, Flags: Cardinal;
begin
  Result := GetVolumeInformation(PChar(Volume), nil, 0, nil, MCL, Flags, nil, 0);
  if Result then
    Result := (Flags and FILE_SUPPORTS_REPARSE_POINTS) <> 0;
end;

//--------------------------------------------------------------------------------------------------

function NtfsFileHasReparsePoint(const Path: string): Boolean;
var
  Attr: DWORD;
begin
  Result := False;
  Attr := GetFileAttributes(PChar(Path));
  if Attr <> DWORD(-1) then
    Result := (Attr and FILE_ATTRIBUTE_REPARSE_POINT) <> 0;
end;

//--------------------------------------------------------------------------------------------------

function NtfsDeleteReparsePoint(const FileName: string; ReparseTag: DWORD): Boolean;
var
  Handle: THandle;
  BytesReturned: DWORD;
  ReparseData: TReparseGuidDataBuffer;
begin
  Result := False;
  Handle := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, 0, nil,
    OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OPEN_REPARSE_POINT, 0);
  if Handle <> INVALID_HANDLE_VALUE then
    try
      FillChar(ReparseData, SizeOf(ReparseData), #0);
      ReparseData.ReparseTag := ReparseTag;
      Result := DeviceIoControl(Handle, FSCTL_DELETE_REPARSE_POINT, @ReparseData,
        REPARSE_GUID_DATA_BUFFER_HEADER_SIZE, nil, 0, BytesReturned, nil);
    finally
      CloseHandle(Handle);
    end;
end;

//--------------------------------------------------------------------------------------------------

function NtfsSetReparsePoint(const FileName: string; var ReparseData; Size: Longword): Boolean;
var
  Handle: THandle;
  BytesReturned: DWORD;
begin
  Result := False;
  Handle := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, 0, nil,
    OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OPEN_REPARSE_POINT, 0);
  if Handle <> INVALID_HANDLE_VALUE then
    try
      Result := DeviceIoControl(Handle, FSCTL_SET_REPARSE_POINT, @ReparseData,
        Size, nil, 0, BytesReturned, nil);
    finally
      CloseHandle(Handle);
    end;
end;

//--------------------------------------------------------------------------------------------------

function NtfsGetReparsePoint(const FileName: string; var ReparseData: TReparseGuidDataBuffer): Boolean;
var
  Handle: THandle;
  BytesReturned: DWORD;
  LastError: DWORD;
begin
  Result := False;
  Handle := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ, nil,
    OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OPEN_REPARSE_POINT, 0);
  LastError := GetLastError;
  if Handle <> INVALID_HANDLE_VALUE then
    try
      Result := DeviceIoControl(Handle, FSCTL_GET_REPARSE_POINT, nil, 0, @ReparseData,
        ReparseData.ReparseDataLength + SizeOf(ReparseData), BytesReturned, nil);
      if not Result then
      begin
        ReparseData.ReparseDataLength := BytesReturned;
        LastError := GetLastError;
      end;
    finally
      CloseHandle(Handle);
      SetLastError(LastError);
    end;
end;

//==================================================================================================
// NTFS - Volume Mount Points
//==================================================================================================

function NtfsIsFolderMountPoint(const Path: string): Boolean;
var
  Tag: DWORD;
begin
  Result := NtfsGetReparseTag(Path, Tag);
  if Result then
    Result := (Tag = IO_REPARSE_TAG_MOUNT_POINT);
end;

//--------------------------------------------------------------------------------------------------

function NtfsMountDeviceAsDrive(const Device: string; Drive: Char): Boolean;
const
  DDD_FLAGS = DDD_RAW_TARGET_PATH or DDD_REMOVE_DEFINITION or DDD_EXACT_MATCH_ON_REMOVE;
var
  DriveStr: string;
  VolumeName: string;
begin
  // To create a mount point we must obtain a unique volume name first. To obtain
  // a unique volume name the drive must exist. Therefore we must temporarily
  // create a symbolic link for the drive using DefineDosDevice.
  DriveStr := Drive + ':';
  Result := DefineDosDevice(DDD_RAW_TARGET_PATH, PChar(DriveStr), PChar(Device));
  if Result then
  begin
    SetLength(VolumeName, 1024);
    Result := RtdlGetVolumeNameForVolumeMountPoint(PChar(DriveStr + '\'),
      PChar(VolumeName), 1024);
    // Attempt to delete the symbolic link, if it fails then don't attempt to
    // set the mountpoint either but raise an exception instead, there's something
    // seriously wrong so let's try to control the damage done already :)
    if not DefineDosDevice(DDD_FLAGS, PChar(DriveStr), PChar(Device)) then
      raise EJclNtfsError.CreateResRec(@RsNtfsUnableToDeleteSymbolicLink);
    if Result then
      Result := RtdlSetVolumeMountPoint(PChar(DriveStr + '\'), PChar(VolumeName));
  end;
end;

//--------------------------------------------------------------------------------------------------

function NtfsMountVolume(const Volume: Char; const MountPoint: string): Boolean;
var
  VolumeName: string;
  VolumeStr: string;
begin
  SetLength(VolumeName, 1024);
  VolumeStr := Volume + ':\';
  Result := RtdlGetVolumeNameForVolumeMountPoint(PChar(VolumeStr), PChar(VolumeName), 1024);
  if Result then
  begin
    if not JclFileUtils.DirectoryExists(MountPoint) then
      JclFileUtils.ForceDirectories(MountPoint);
    Result := RtdlSetVolumeMountPoint(PChar(MountPoint), PChar(VolumeName));
  end;
end;

//==================================================================================================
// NTFS - Change Journal
//==================================================================================================

//==================================================================================================
// NTFS - Opportunistic Locks
//==================================================================================================

function NtfsOpLockAckClosePending(Handle: THandle; Overlapped: TOverlapped): Boolean;
var
  BytesReturned: Cardinal;
begin
  Result := DeviceIoControl(Handle, FSCTL_OPBATCH_ACK_CLOSE_PENDING, nil, 0, nil,
    0, BytesReturned, @Overlapped);
end;

//--------------------------------------------------------------------------------------------------

function NtfsOpLockBreakAckNo2(Handle: THandle; Overlapped: TOverlapped): Boolean;
var
  BytesReturned: Cardinal;
begin
  Result := DeviceIoControl(Handle, FSCTL_OPLOCK_BREAK_ACK_NO_2, nil, 0, nil, 0,
    BytesReturned, @Overlapped);
end;

//--------------------------------------------------------------------------------------------------

function NtfsOpLockBreakAcknowledge(Handle: THandle; Overlapped: TOverlapped): Boolean;
var
  BytesReturned: Cardinal;
begin
  Result := DeviceIoControl(Handle, FSCTL_OPLOCK_BREAK_ACKNOWLEDGE, nil, 0, nil,
    0, BytesReturned, @Overlapped);
  Result := Result or (GetLastError = ERROR_IO_PENDING);
end;

//--------------------------------------------------------------------------------------------------

function NtfsOpLockBreakNotify(Handle: THandle; Overlapped: TOverlapped): Boolean;
var
  BytesReturned: Cardinal;
begin
  Result := DeviceIoControl(Handle, FSCTL_OPLOCK_BREAK_NOTIFY, nil, 0, nil, 0,
    BytesReturned, @Overlapped);
end;

//--------------------------------------------------------------------------------------------------

function NtfsRequestOpLock(Handle: THandle; Kind: TOpLock; Overlapped: TOverlapped): Boolean;
const
  IoCodes: array [TOpLock] of Cardinal = (
    FSCTL_REQUEST_OPLOCK_LEVEL_1, FSCTL_REQUEST_OPLOCK_LEVEL_2,
    FSCTL_REQUEST_BATCH_OPLOCK, FSCTL_REQUEST_FILTER_OPLOCK);
var
  BytesReturned: Cardinal;
begin
  Result := DeviceIoControl(Handle, IoCodes[Kind], nil, 0, nil, 0, BytesReturned, @Overlapped);
  Result := Result or (GetLastError = ERROR_IO_PENDING);
end;

//==================================================================================================
// Junction Points
//==================================================================================================

type
  TReparseDataBufferOverlay = record
  case Boolean of
    False:
      (Reparse: TReparseDataBuffer;);
    True:
      (Buffer: array [0..MAXIMUM_REPARSE_DATA_BUFFER_SIZE] of Char;);
  end;
  
//--------------------------------------------------------------------------------------------------

function IsReparseTagValid(Tag: DWORD): Boolean;
begin
  Result := (Tag and (not IO_REPARSE_TAG_VALID_VALUES) = 0) and
    (Tag > IO_REPARSE_TAG_RESERVED_RANGE);
end;

//--------------------------------------------------------------------------------------------------

function NtfsCreateJunctionPoint(const Source, Destination: string): Boolean;
var
  Dest: array [0..1024] of Char; // Writable copy of Destination
  DestW: WideString;             // Unicode version of Dest
  FullDir: array [0..1024] of Char;
  FilePart: PChar;
  ReparseData: TReparseDataBufferOverlay;
  NameLength: Longword;
begin
  Result := False;
  // For some reason the destination string must be prefixed with \??\ otherwise
  // the IOCTL will fail, ensure it's there.
  if Copy(Destination, 1, 3) = '\??' then
    StrPCopy(Dest, Destination)
  else
  begin
    // Make sure Destination is a directory or again, the IOCTL will fail.
    if (GetFullPathName(PChar(Destination), 1024, FullDir, FilePart) = 0) or
      (GetFileAttributes(FullDir) = DWORD(-1)) then
    begin
      SetLastError(ERROR_PATH_NOT_FOUND);
      Exit;
    end;
    StrPCopy(Dest, '\??\' + Destination);
  end;
  FillChar(ReparseData, SizeOf(ReparseData), #0);
  NameLength := StrLen(Dest) * SizeOf(WideChar);
  ReparseData.Reparse.ReparseTag := IO_REPARSE_TAG_MOUNT_POINT;
  ReparseData.Reparse.ReparseDataLength := NameLength + 12;
  ReparseData.Reparse.SubstituteNameLength := NameLength;
  ReparseData.Reparse.PrintNameOffset := NameLength + 2;
  // Not the most elegant way to copy an AnsiString into an Unicode buffer but
  // let's avoid dependencies on JclUnicode.pas (adds significant resources).
  DestW := WideString(Dest);
  Move(DestW[1], ReparseData.Reparse.PathBuffer, Length(DestW) * SizeOf(WideChar));
  Result := NtfsSetReparsePoint(Source, ReparseData.Reparse,
    ReparseData.Reparse.ReparseDataLength + REPARSE_DATA_BUFFER_HEADER_SIZE);
end;

//--------------------------------------------------------------------------------------------------

function NtfsDeleteJunctionPoint(const Source: string): Boolean;
begin
  Result := NtfsDeleteReparsePoint(Source, IO_REPARSE_TAG_MOUNT_POINT);
end;

//--------------------------------------------------------------------------------------------------

function NtfsGetJunctionPointDestination(const Source: string; var Destination: string): Boolean;
var
  Handle: THandle;
  ReparseData: TReparseDataBufferOverlay;
  BytesReturned: DWORD;
begin
  Result := False;
  if NtfsFileHasReparsePoint(Source) then
  begin
    Handle := CreateFile(PChar(Source), GENERIC_READ, 0, nil,
      OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OPEN_REPARSE_POINT, 0);
    if Handle <> INVALID_HANDLE_VALUE then
    try
      if DeviceIoControl(Handle, FSCTL_GET_REPARSE_POINT, nil, 0, @ReparseData,
        MAXIMUM_REPARSE_DATA_BUFFER_SIZE, BytesReturned, nil) {and
        IsReparseTagValid(ReparseData.Reparse.ReparseTag) then}
        then
      begin
        if BytesReturned >= ReparseData.Reparse.SubstituteNameLength + SizeOf(WideChar) then
        begin
          SetLength(Destination, (ReparseData.Reparse.SubstituteNameLength div SizeOf(WideChar)) + 1);
          WideCharToMultiByte(CP_THREAD_ACP, 0, ReparseData.Reparse.PathBuffer,
            (ReparseData.Reparse.SubstituteNameLength div SizeOf(WCHAR)) + 1,
            PChar(Destination), Length(Destination), nil, nil);
          Result := True;
        end;
      end;
    finally
      CloseHandle(Handle);
    end
  end;
end;

//==================================================================================================
// Streams
//==================================================================================================

// FindStream is an internal helper routine for NtfsFindFirstStream and
// NtfsFindNextStream. It uses the backup API to enumerate the streams in an
// NTFS file and returns when it either finds a stream that matches the filter
// specified in the Data parameter or hits EOF. Details are returned through
// the Data parameter and success/failure as the Boolean result value.

function FindStream(var Data: TFindStreamData): Boolean;
var
  Header: TWin32StreamId;
  BytesToRead, BytesRead: DWORD;
  BytesToSeek: TULargeInteger;
  Hi, Lo: DWORD;
  FoundStream: Boolean;
  StreamName: PWideChar;
begin
  Result := False;
  FoundStream := False;
  // We loop until we either found a stream or an error occurs.
  while not FoundStream do
  begin
    // Read stream header
    BytesToRead := DWORD(@Header.cStreamName[0]) - DWORD(@Header.dwStreamId);
    if not Windows.BackupRead(Data.Internal.FileHandle, (@Header), BytesToRead, BytesRead,
      False, True, Data.Internal.Context) then
    begin
      SetLastError(ERROR_READ_FAULT);
      Exit;
    end;
    if BytesRead = 0 then // EOF
    begin
      SetLastError(ERROR_NO_MORE_FILES);
      Exit;
    end;
    // If stream has a name then read it
    if Header.dwStreamNameSize > 0 then
    begin
      StreamName := HeapAlloc(GetProcessHeap, 0, Header.dwStreamNameSize + SizeOf(WCHAR));
      if StreamName = nil then
      begin
        SetLastError(ERROR_OUTOFMEMORY);
        Exit;
      end;
      if not Windows.BackupRead(Data.Internal.FileHandle, Pointer(StreamName),
        Header.dwStreamNameSize, BytesRead, False, True, Data.Internal.Context) then
      begin
        HeapFree(GetProcessHeap, 0, StreamName);
        SetLastError(ERROR_READ_FAULT);
        Exit;
      end;
      StreamName[Header.dwStreamNameSize div SizeOf(WCHAR)] := WideChar(#0);
    end
    else
      StreamName := nil;
    // Did we find any of the specified streams ([] means any stream)?
    if (Data.Internal.StreamIds = []) or
      (TStreamId(Header.dwStreamId) in Data.Internal.StreamIds) then
    begin
      FoundStream := True;
      {$IFDEF FPC}
      Data.Size := Header.Size.QuadPart;
      {$ELSE}
      Data.Size := Header.Size;
      {$ENDIF FPC}
      Data.Name := StreamName;
      Data.Attributes := Header.dwStreamAttributes;
      Data.StreamId := TStreamId(Header.dwStreamId);
    end;
    // Release stream name memory if necessary
    if Header.dwStreamNameSize > 0 then
      HeapFree(GetProcessHeap, 0, StreamName);
    // Move past data part to beginning of next stream (or EOF)
    {$IFDEF FPC}
    BytesToSeek.QuadPart := Header.Size.QuadPart;
    if (Header.Size.QuadPart <> 0) and (not JclWin32.BackupSeek(Data.Internal.FileHandle, BytesToSeek.LowPart,
         BytesToSeek.HighPart, Lo, Hi, Data.Internal.Context)) then
    {$ELSE}
    BytesToSeek.QuadPart := Header.Size;
    if (Header.Size <> 0) and (not JclWin32.BackupSeek(Data.Internal.FileHandle, BytesToSeek.LowPart,
      BytesToSeek.HighPart, Lo, Hi, Data.Internal.Context)) then
    {$ENDIF FPC}
    begin
      SetLastError(ERROR_READ_FAULT);
      Exit;
    end;
  end;
  // Due to the usage of Exit, we only get here if everything succeeded
  Result := True;
end;

//--------------------------------------------------------------------------------------------------

function NtfsFindFirstStream(const FileName: string; StreamIds: TStreamIds;
  var Data: TFindStreamData): Boolean;
begin
  Result := False;
  // Open file for reading, note that the FILE_FLAG_BACKUP_SEMANTICS requires
  // the SE_BACKUP_NAME and SE_RESTORE_NAME privileges.
  Data.Internal.FileHandle := CreateFile(PChar(FileName), GENERIC_READ,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING,
    FILE_FLAG_BACKUP_SEMANTICS, 0);
  if Data.Internal.FileHandle <> INVALID_HANDLE_VALUE then
  begin
    // Initialize private context
    Data.Internal.StreamIds := StreamIds;
    Data.Internal.Context := nil;
    // Call upon the Borg worker to find the next (first) stream
    Result := FindStream(Data);
    if not Result then
    begin
      // Failure, cleanup relieving the caller of having to call FindStreamClose
      CloseHandle(Data.Internal.FileHandle);
      Data.Internal.FileHandle := INVALID_HANDLE_VALUE;
      Data.Internal.Context := nil;
      if GetLastError = ERROR_NO_MORE_FILES then
        SetLastError(ERROR_FILE_NOT_FOUND);
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function NtfsFindNextStream(var Data: TFindStreamData): Boolean;
begin
  Result := False;
  if Data.Internal.FileHandle <> INVALID_HANDLE_VALUE then
    Result := FindStream(Data)
  else
    SetLastError(ERROR_INVALID_HANDLE);
end;

//--------------------------------------------------------------------------------------------------

function NtfsFindStreamClose(var Data: TFindStreamData): Boolean;
var
  BytesRead: DWORD;
  LastError: DWORD;
begin
  Result := Data.Internal.FileHandle <> INVALID_HANDLE_VALUE;
  if Result then
  begin
    // Call BackupRead one last time to signal that we're done with it
    Result := Windows.BackupRead(0, nil, 0, BytesRead, True, False, Data.Internal.Context);
    if Result then
      LastError := ERROR_SUCCESS
    else
      LastError := GetLastError;
    CloseHandle(Data.Internal.FileHandle);
    Data.Internal.FileHandle := INVALID_HANDLE_VALUE;
    Data.Internal.Context := nil;
  end
  else
    LastError := ERROR_INVALID_HANDLE;
  SetLastError(LastError);
end;

//==================================================================================================
// Hard links
//==================================================================================================
//
// CreateHardLinkNT
//
// Creates a hard link on NT 4. Both LinkFileName and ExistingFileName must reside on the same, NTFS formatted volume.
//
// LinkName: Name of the hard link to create
// ExistingFileName: Fully qualified path of the file for which to create a hard link
// Result: True if successfull, False if failed. In the latter case use GetLastError to obtain the reason of failure.
//
// Remarks: On Windows 2000 and up you should favor the usage of CreateHardLinkNT5.
//          You must be a member of the Administrators or Backup Operators group.
// Requirements: Windows NT 3.51, 4.0, 2000 or XP
//
function CreateHardLinkNT(const LinkFileName, ExistingFileName: string): BOOL;
var
  BackupPriv, RestorePriv: Boolean;      // we're these privileges enabled on entry?
  HardLink: THandle;                     // handle for hard link
  HardLinkName: WideString;              // full path name of the hard link in unicode
  FullPath: string;                      // full path name of the hard link in ansi
  FilePart: PChar;                       // we need this to call GetFullPathName but don't use it
  StreamId: TWin32StreamId;              // the stream header record
  Context: Pointer;                      // context pointer for BackupWrite
  BytesToWrite, BytesWritten: Cardinal;  // number of bytes to write, that got written
begin
  // always the pessimist...
  Result := False;
  // test if the required privileges are enabled
  BackupPriv := IsPrivilegeEnabled(SE_BACKUP_NAME);
  RestorePriv := IsPrivilegeEnabled(SE_RESTORE_NAME);
  try
    // if not enable them now, just in case we're not an administrator
    if not BackupPriv then
      Win32Check(EnableThreadPrivilege(True, SE_BACKUP_NAME));
    if not RestorePriv then
      Win32Check(EnableThreadPrivilege(True, SE_RESTORE_NAME));
    // test if the hard link already exists, if so bail out
    HardLink := CreateFile(PChar(LinkFileName), 0, 0, nil, OPEN_EXISTING, 0, 0);
    if HardLink <> INVALID_HANDLE_VALUE then
    begin
      CloseHandle(HardLink);
      SetLastError(ERROR_ALREADY_EXISTS);
      Exit;
    end;
    // open the _existing_ file. this may seem counter intuitive but creating a hard link consists of writing a
    // backup_link stream to the existing file containing the path of the hard link. in response NTFS will add the
    // hard link name as an alternate filename and create a directory entry for the hard link.
    HardLink := CreateFile(PChar(ExistingFileName), GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_POSIX_SEMANTICS, 0);
    if HardLink <> INVALID_HANDLE_VALUE then
    try
      // get the full unicode path name for the hard link
      SetLength(FullPath, MAX_PATH);
      GetFullPathName(PChar(LinkFileName), MAX_PATH, PChar(FullPath), FilePart);
      SetLength(FullPath, StrLen(PChar(FullPath)));
      HardLinkName := FullPath;
      // initialize and write the stream header
      FillChar(StreamId, SizeOf(StreamId), 0);
      StreamId.dwStreamId := BACKUP_LINK;
      {$IFDEF FPC}
      StreamId.Size.QuadPart := (Length(HardLinkName) + 1) * SizeOf(WideChar);
      {$ELSE}
      StreamId.Size := (Length(HardLinkName) + 1) * SizeOf(WideChar);
      {$ENDIF FPC}
      BytesToWrite := DWORD(@StreamId.cStreamName[0]) - DWORD(@StreamId.dwStreamId);
      Context := nil;
      Win32Check(Windows.BackupWrite(HardLink, @StreamId, BytesToWrite, BytesWritten, False, False, Context));
      if BytesToWrite <> BytesWritten then
        RaiseLastOSError;
      // now write the hard link name
      {$IFDEF FPC}
      Win32Check(Windows.BackupWrite(HardLink, @HardLinkName[1], StreamId.Size.QuadPart, BytesWritten, False, False, Context));
      if BytesWritten <> StreamId.Size.QuadPart then
      {$ELSE}
      Win32Check(Windows.BackupWrite(HardLink, @HardLinkName[1], StreamId.Size, BytesWritten, False, False, Context));
      if BytesWritten <> StreamId.Size then
      {$ENDIF FPC}
        RaiseLastOSError;
      // and finally release the context
      Windows.BackupWrite(HardLink, nil, 0, BytesWritten, True, False, Context);
      Result := True;
    finally
      CloseHandle(HardLink);
    end;
  finally
    // if we enabled them we should disable as well
    if not BackupPriv then
      Win32Check(EnableThreadPrivilege(False, SE_BACKUP_NAME));
    if not RestorePriv then
      Win32Check(EnableThreadPrivilege(False, SE_RESTORE_NAME));
  end;
end;

//--------------------------------------------------------------------------------------------------
//
// CreateHardLink2000
//
// Creates a hard link on NT 5. Simple wrapper around CreateHardLink API function. See PSDK docs for more details.
//
// LinkName: Name of the hard link to create
// ExistingFileName: Fully qualified path of the file for which to create a hard link
// Result: True if successfull, False if failed. In the latter case use GetLastError to obtain the reason of failure.
//
// Remarks: On Windows NT 4 and earlier you can use CreateHardLinkNT.
// Requirements: Windows 2000 or XP
//
function CreateHardLink2000(const LinkFileName, ExistingFileName: string): BOOL;
begin
  Result := RtdlCreateHardLink(PChar(LinkFileName), PChar(ExistingFileName), nil);
end;

//--------------------------------------------------------------------------------------------------

function NtfsCreateHardLink(const LinkFileName, ExistingFileName: string): Boolean;
begin
  // first try the official CreateHardLink Windows API function, which only exists for Windows 2000 and up
  Result := CreateHardLink2000(LinkFileName, ExistingFileName);
  // if that fails try the akward NT method using the Tapi Backup API
  if not Result then
    Result := CreateHardLinkNT(LinkFileName, ExistingFileName);
end;

//--------------------------------------------------------------------------------------------------

function NtfsGetHardLinkInfo(const FileName: string; var Info: TNtfsHardLinkInfo): Boolean;
var
  F: THandle;
  FileInfo: TByHandleFileInformation;
begin
  Result := False;
  F := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  if F <> INVALID_HANDLE_VALUE then
  try
    if GetFileInformationByHandle(F, FileInfo) then
    begin
      Info.LinkCount := FileInfo.nNumberOfLinks;
      Info.FileIndexHigh := FileInfo.nFileIndexHigh;
      Info.FileIndexLow := FileInfo.nFileIndexLow;
      Result := True;
    end;
  finally
    CloseHandle(F);
  end
end;

//--------------------------------------------------------------------------------------------------

function NtfsFindHardLinks(const Path: string; const FileIndexHigh, FileIndexLow: Cardinal; const List: TStrings): Boolean;
var
  SearchRec: TSearchRec;
  R: Integer;
  Info: TNtfsHardLinkInfo;
begin
  // start the search
  R := FindFirst(Path + '\*.*', faAnyFile, SearchRec);
  Result := (R = 0);
  if Result then
  try
    while R = 0 do
    begin
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        if (SearchRec.Attr and faDirectory) = faDirectory then
        begin
          // recurse into subdirectory
          Result := NtfsFindHardLinks(Path + '\' + SearchRec.Name, FileIndexHigh, FileIndexLow, List);
          if not Result then
            Break;
        end
        else
        begin
          // found a file, is it a hard link?
          if NtfsGetHardLinkInfo(Path + '\' + SearchRec.Name, Info) then
          begin
            if (Info.FileIndexHigh = FileIndexHigh) and (Info.FileIndexLow = FileIndexLow) then
              List.Add(Path + '\' + SearchRec.Name);
          end;
        end;
      end;
      R := FindNext(SearchRec);
    end;
    Result := R = ERROR_NO_MORE_FILES;
  finally
    SysUtils.FindClose(SearchRec);
  end;
  if R = ERROR_ACCESS_DENIED then
    Result := True;
end;

//--------------------------------------------------------------------------------------------------

function NtfsDeleteHardLinks(const FileName: string): Boolean;
var
  FullPathName: string;
  FilePart: PChar;
  Files: TStrings;
  I: Integer;
  Info: TNtfsHardLinkInfo;
begin
  Result := False;
  // get the full pathname of the specified file
  SetLength(FullPathName, MAX_PATH);
  GetFullPathName(PChar(FileName), MAX_PATH, PChar(FullPathName), FilePart);
  SetLength(FullPathName, StrLen(PChar(FullPathName)));
  // get hard link information
  if NtfsGetHardLinkInfo(FullPathName, Info) then
  begin
    Files := TStringList.Create;
    try
      if Info.LinkCount > 1 then
      begin
        // find all hard links for this file
        if not NtfsFindHardLinks(FullPathName[1] + ':', Info.FileIndexHigh, Info.FileIndexLow, Files) then
          Exit;
        // first delete the originally specified file from the list, we don't delete that one until all hard links
        // are succesfully deleted so we can use it to restore them if anything goes wrong. Theoretically one could
        // use any of the hard links but in case the restore goes wrong, at least the specified file still exists...
        for I := 0 to Files.Count - 1 do
        begin
          if CompareStr(FullPathName, Files[I]) = 0 then
          begin
            Files.Delete(I);
            Break;
          end;
        end;
        // delete all found hard links
        I := 0;
        while I < Files.Count do
        begin
          if not DeleteFile(Files[I]) then
            Break;
          Inc(I);
        end;
        if I = Files.Count then
        begin
          // all hard links succesfully deleted, now delete the originally specified file. if this fails we set
          // I to Files.Count - 1 so that the next code block will restore all hard links we just deleted.
          Result := DeleteFile(FullPathName);
          if not Result then
            I := Files.Count - 1;
        end;
        if I < Files.Count then
        begin
          // not all hard links could be deleted, attempt to restore the ones that were
          while I >= 0 do
          begin
            // ignore result, just attempt to restore...
            NtfsCreateHardLink(Files[I], FullPathName);
            Dec(I);
          end;
        end;
      end
      else
      begin
        // there are no hard links, just delete the file
        Result := DeleteFile(FullPathName);
      end;
    finally
      Files.Free;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

// History:

// $Log$
// Revision 1.13  2004/07/29 07:58:21  marquardt
// inc files updated
//
// Revision 1.12  2004/07/28 18:00:53  marquardt
// various style cleanings, some minor fixes
//
// Revision 1.11  2004/07/14 03:00:34  rrossmair
// fixed bug #1962 ( NtfsCreateJunctionPoint fails if a \\??\\ path is used)
//
// Revision 1.10  2004/06/16 07:30:31  marquardt
// added tilde to all IFNDEF ENDIFs, inherited qualified
//
// Revision 1.9  2004/06/14 11:05:53  marquardt
// symbols added to all ENDIFs and some other minor style changes like removing IFOPT
//
// Revision 1.8  2004/05/31 00:30:45  rrossmair
// Processed documentation TODOs
//
// Revision 1.7  2004/05/13 07:46:06  rrossmair
// changes for FPC 1.9.3+ compatibility
//
// Revision 1.6  2004/05/05 07:33:49  rrossmair
// header updated according to new policy: initial developers & contributors listed
//
// Revision 1.5  2004/04/06 04:55:18  peterjhaas
// adapt compiler conditions, add log entry
//

end.
