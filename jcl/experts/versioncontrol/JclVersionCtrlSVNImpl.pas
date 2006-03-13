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
{ The Original Code is JclVersionCtrlSVNImpl.pas                                                   }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet.                                    }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet.                          }
{ Portions created by Elahn Ientile are Copyright (C) of Elahn Ientile.                            }
{                                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Unit owner: Florent Ouchet                                                                       }
{ Last modified: $Date$                                                      }
{ Revision: $Revision$                                                                       }
{                                                                                                  }
{**************************************************************************************************}

unit JclVersionCtrlSVNImpl;

{$I jcl.inc}

interface

uses
  SysUtils, Classes, Windows, Graphics,
  VersionControlImpl;

type
  TJclVersionControlSVN = class (TJclVersionControlPlugin)
  private
    FTortoiseSVNProc: string;
  protected
    function GetSupportedActions: TJclVersionControlActions; override;
    function GetFileActions(const FileName: string): TJclVersionControlActions; override;
    function GetSandboxActions(const SdBxName: string): TJclVersionControlActions; override;
    function GetIcon(const Action: TJclVersionControlAction): Integer; override;
    function GetEnabled: Boolean; override;
    function GetName: string; override;
  public
    constructor Create(const AExpert: TJclVersionControlExpert); override;
    destructor Destroy; override;
    function GetSandboxNames(const FileName: string; SdBxNames: TStrings): Boolean; override;
    function ExecuteAction(const FileName: string;
      const Action: TJclVersionControlAction): Boolean; override;
  end;

implementation

uses
  JclFileUtils, JclRegistry,
  JclOtaUtils, JclOtaResources, JclOtaConsts;

const
  JclVersionCtrlRegKeyName = 'SOFTWARE\TortoiseSVN';
  JclVersionCtrlRegValueName = 'ProcPath';
  JclVersionCtrlSVNAddVerb = 'add';
  JclVersionCtrlSVNBlameVerb = 'blame';
  JclVersionCtrlSVNBranchVerb = 'copy';
  JclVersionCtrlSVNCheckOutVerb = 'checkout';
  JclVersionCtrlSVNCommitVerb = 'commit';
  JclVersionCtrlSVNDiffVerb = 'diff';
  JclVersionCtrlSVNGraphVerb = 'revisiongraph';
  JclVersionCtrlSVNLogVerb = 'log';
  JclVersionCtrlSVNLockVerb = 'lock';
  JclVersionCtrlSVNMergeVerb = 'merge';
  JclVersionCtrlSVNRenameVerb = 'rename';
  JclVersionCtrlSVNRepoBrowserVerb = 'repobrowser';
  JclVersionCtrlSVNRevertVerb = 'revert';
  JclVersionCtrlSVNStatusVerb = 'repostatus';
  JclVersionCtrlSVNTagVerb = 'copy';
  JclVersionCtrlSVNUpdateVerb = 'update';
  JclVersionCtrlSVNUpdateToParam = '/rev';
  JclVersionCtrlSVNUnlockVerb = 'unlock';
  JclVersionCtrlSVNTortoiseDLL = 'TortoiseSVN.dll';
  JclVersionCtrlSVNDirectory1 = '.svn\';
  JclVersionCtrlSVNDirectory2 = '_svn\';  
  JclVersionCtrlSVNEntryFile = 'entries';

  JclVersionCtrlSVNDirectories: array [0..1] of string =
   ( JclVersionCtrlSVNDirectory1, JclVersionCtrlSVNDirectory2 ); 

resourcestring
  RsVersionCtrlSVNName = 'subversion';
  RsEEmptyFileName = 'Error: empty file name';
  RSENoTortoiseSVN = 'TortoiseSVN is not detected on the system';

//=== TJclVersionControlSVN ==================================================

constructor TJclVersionControlSVN.Create(const AExpert: TJclVersionControlExpert);
begin
  inherited Create(AExpert);
  FTortoiseSVNProc := RegReadStringDef(HKLM, JclVersionCtrlRegKeyName, JclVersionCtrlRegValueName, '');
end;

destructor TJclVersionControlSVN.Destroy;
begin
  inherited Destroy;
end;

function TJclVersionControlSVN.ExecuteAction(const FileName: string;
  const Action: TJclVersionControlAction): Boolean;
  function CallTortoiseSVNProc(const ActionName: string;
    const Param: string = ''): Boolean;
  var
    StartupInfo: TStartupInfo;
    ProcessInfo: TProcessInformation;
    CurrentDir, CommandLine: string;
  begin
    FillChar(StartupInfo,SizeOf(TStartupInfo),#0);
    FillChar(ProcessInfo,SizeOf(TProcessInformation),#0);
    startupInfo.cb := SizeOf(TStartupInfo);
    startupInfo.dwFlags := STARTF_USESHOWWINDOW;
    startupInfo.wShowWindow := SW_SHOW;

    if FileName = '' then
      raise EJclExpertException.CreateTrace(RsEEmptyFileName);
    if not Enabled then
      raise EJClExpertException.CreateTrace(RsENoTortoiseSVN);

    if FileName[Length(FileName)] = DirDelimiter then
      CurrentDir := FileName
    else
      CurrentDir := ExtractFilePath(FileName);
    CommandLine := Format('%s /command:%s /path:"%s" %s /notempfile', [FTortoiseSVNProc, ActionName, FileName, Param]);

    Result := CreateProcess(nil, PChar(CommandLine), nil,
      nil, False, 0, nil, PChar(CurrentDir), StartupInfo, ProcessInfo);

    if Result then
    begin
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
    end;
  end;
begin
  case Action of
    vcaAdd,
    vcaAddSandbox:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNAddVerb);
    vcaBlame :
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNBlameVerb);
    vcaBranch,
    vcaBranchSandbox:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNBranchVerb);
    vcaCheckOutSandbox:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNCheckOutVerb);
    vcaCommit,
    vcaCommitSandbox:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNCommitVerb);
    vcaDiff:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNDiffVerb);
    vcaGraph:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNGraphVerb);
    vcaLog,
    vcaLogSandbox:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNLogVerb);
    vcaLock,
    vcaLockSandbox:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNLockVerb);
    vcaMerge,
    vcaMergeSandbox:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNMergeVerb);
    vcaRename:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNRenameVerb);
    vcaRepoBrowser:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNRepoBrowserVerb);
    vcaRevert,
    vcaRevertSandbox:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNRevertVerb);
    vcaStatus,
    vcaStatusSandbox:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNStatusVerb);
    vcaTag,
    vcaTagSandBox:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNTagVerb);
    vcaUpdate,
    vcaUpdateSandbox:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNUpdateVerb);
    vcaUpdateTo,
    vcaUpdateSandboxTo:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNUpdateVerb, JclVersionCtrlSVNUpdateToParam);
    vcaUnlock,
    vcaUnlockSandbox:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNUnlockVerb);
    else
      Result := inherited ExecuteAction(FileName, Action);
  end;
end;

function TJclVersionControlSVN.GetEnabled: Boolean;
begin
  Result := FTortoiseSVNProc <> '';
end;

function TJclVersionControlSVN.GetFileActions(
  const FileName: string): TJclVersionControlActions;
var
  EntryFile: string;
  Entries: TStrings;
  IndexDir, IndexEntry: Integer;
  FileNameValue: string;
begin
  Result := inherited GetFileActions(FileName);

  if Enabled then
  begin
    Entries := TStringList.Create;
    try
      FileNameValue := Format('NAME="%s"', [ExtractFileName(AnsiUpperCaseFileName(FileName))]);

      for IndexDir := Low(JclVersionCtrlSVNDirectories) to High(JclVersionCtrlSVNDirectories) do
      begin
        EntryFile := PathAddSeparator(ExtractFilePath(FileName))
          + JclVersionCtrlSVNDirectories[IndexDir] + JclVersionCtrlSVNEntryFile;

        if FileExists(EntryFile) then
        begin
          Entries.LoadFromFile(EntryFile);

          for IndexEntry := 0 to Entries.Count - 1 do
            if Pos(FileNameValue, AnsiUpperCase(Entries.Strings[IndexEntry])) > 0 then
          begin
            // TODO: check modifications
            Result := Result + [vcaBlame, vcaBranch, vcaCommit, vcaDiff, vcaGraph,
              vcaLog, vcaLock, vcaMerge, vcaRename, vcaRevert, vcaRepoBrowser,
              vcaStatus, vcaTag, vcaUpdate, vcaUpdateTo, vcaUnlock];
            FreeAndNil(Entries);
            Exit;
          end;
        end;
      end;
    finally
      Entries.Free;
    end;
    Result := Result + [vcaAdd];
  end;
end;

function TJclVersionControlSVN.GetSupportedActions: TJclVersionControlActions;
begin
  Result := inherited GetSupportedActions;
  if Enabled then
    Result := Result + [vcaAdd, vcaAddSandbox, vcaBlame, vcaBranch,
    vcaBranchSandbox, vcaCheckOutSandbox, vcaCommit, vcaCommitSandbox, vcaDiff,
    vcaGraph, vcaLog, vcaLogSandbox, vcaLock, vcaLockSandbox, vcaMerge,
    vcaMergeSandbox, vcaRename, vcaRepoBrowser, vcaRevert, vcaRevertSandbox,
    vcaStatus, vcaStatusSandbox, vcaTag, vcaTagSandBox, vcaUpdate,
    vcaUpdateSandbox, vcaUpdateTo, vcaUpdateSandboxTo, vcaUnlock, vcaUnlockSandbox];
end;

function TJclVersionControlSVN.GetIcon(
  const Action: TJclVersionControlAction): Integer;
var
  LibraryName: string;
begin
  LibraryName := PathAddSeparator(ExtractFilePath(FTortoiseSVNProc)) + JclVersionCtrlSVNTortoiseDLL;
  
  case Action of
    vcaAdd,
    vcaAddSandbox:
      Result := Expert.CacheResourceIcon(LibraryName, 124);
    vcaBlame:
      Result := Expert.CacheResourceIcon(LibraryName, 5146);
    vcaBranch,
    vcaBranchSandbox:
      Result := Expert.CacheResourceIcon(LibraryName, 132);
    vcaCheckOutSandbox:
      Result := Expert.CacheResourceIcon(LibraryName, 121);
    vcaCommit,
    vcaCommitSandbox:
      Result := Expert.CacheResourceIcon(LibraryName, 123);
    vcaDiff:
      Result := Expert.CacheResourceIcon(LibraryName, 135);
    vcaGraph:
      Result := Expert.CacheResourceIcon(LibraryName, 5151);
    vcaLog,
    vcaLogSandbox:
      Result := Expert.CacheResourceIcon(LibraryName, 136);
    vcaLock,
    vcaLockSandbox:
      Result := Expert.CacheResourceIcon(LibraryName, 5152);
    vcaMerge,
    vcaMergeSandbox:
      Result := Expert.CacheResourceIcon(LibraryName, 129);
    vcaRename:
      Result := Expert.CacheResourceIcon(LibraryName, 134);
    vcaRepoBrowser:
      Result := Expert.CacheResourceIcon(LibraryName, 5145);
    vcaRevert,
    vcaRevertSandbox:
      Result := Expert.CacheResourceIcon(LibraryName, 125);
    vcaStatus,
    vcaStatusSandbox:
      Result := Expert.CacheResourceIcon(LibraryName, 143);
    vcaTag,
    vcaTagSandBox:
      Result := Expert.CacheResourceIcon(LibraryName, 132);
    vcaUpdate,
    vcaUpdateSandbox,
    vcaUpdateTo,
    vcaUpdateSandboxTo:
      Result := Expert.CacheResourceIcon(LibraryName, 122);
    vcaUnlock,
    vcaUnlockSandbox:
      Result := Expert.CacheResourceIcon(LibraryName, 5153);
    else
      Result := inherited GetIcon(Action);
  end;

end;

function TJclVersionControlSVN.GetName: string;
begin
  Result := RsVersionCtrlSVNName;
end;

function TJclVersionControlSVN.GetSandboxActions(
  const SdBxName: string): TJclVersionControlActions;
var
  SvnDirectory: string;
  IndexDir: Integer;
begin
  Result := inherited GetSandboxActions(SdBxName);

  if Enabled then
  begin
    for IndexDir := Low(JclVersionCtrlSVNDirectories) to High(JclVersionCtrlSVNDirectories) do
    begin
      SvnDirectory := sdBxName + JclVersionCtrlSVNDirectories[IndexDir];

      if DirectoryExists(SvnDirectory) then
      begin
        Result := Result + [vcaAddSandbox, vcaBranchSandbox, vcaCommitSandbox,
          vcaLogSandbox, vcaLockSandbox, vcaMergeSandbox, vcaRevertSandbox,
          vcaStatusSandbox, vcaTagSandBox, vcaUpdateSandbox, vcaUpdateSandboxTo,
          vcaUnlockSandbox];
        Exit;
      end;
    end;
    // not in a sandbox
    Result := Result + [vcaCheckOutSandbox];
  end;
end;

function TJclVersionControlSVN.GetSandboxNames(const FileName: string;
  SdBxNames: TStrings): Boolean;
var
  DirectoryName: string;
  IndexDir, IndexFileName: Integer;
begin
  Result := True;

  SdBxNames.BeginUpdate;
  try
    SdBxNames.Clear;

    if Enabled then
      for IndexFileName := Length(FileName) downto 1 do
        if FileName[IndexFileName] = DirDelimiter then
    begin
      for IndexDir := Low(JclVersionCtrlSVNDirectories) to High(JclVersionCtrlSVNDirectories) do
      begin
        DirectoryName := Copy(FileName, 1, IndexFileName) + JclVersionCtrlSVNDirectories[IndexDir];
        if DirectoryExists(DirectoryName) then
          SdBxNames.Add(DirectoryName);
      end;
    end;
  finally
    SdBxNames.EndUpdate;
  end;
  
  if SdBxNames.Count = 0 then
    Result := inherited GetSandboxNames(FileName, SdBxNames);
end;

initialization

try
  TJclVersionControlExpert.RegisterPluginClass(TJclVersionControlSVN);
except
  on ExceptionObj: TObject do
  begin
    JclExpertShowExceptionDialog(ExceptionObj);
    raise;
  end;
end;

finalization

try
  TJclVersionControlExpert.UnregisterPluginClass(TJclVersionControlSVN);
except
  on ExceptionObj: TObject do
  begin
    JclExpertShowExceptionDialog(ExceptionObj);
    raise;
  end;
end;

// History:

// $Log$
// Revision 1.4  2006/03/13 22:14:59  outchy
// PathSeparator renamed to DirDelimiter
// Installer checks paths
//
// Revision 1.3  2006/01/26 06:15:17  outchy
// Repository browser now works
//
// Revision 1.2  2006/01/25 20:33:27  outchy
// Added _svn as a valid subdirectory
//
// Revision 1.1  2006/01/15 00:51:22  outchy
// cvs support in version control expert
// version control expert integration in the installer
//

end.
