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
{ The Original Code is JclVersionCtrlCVSImpl.pas                                                   }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet.                                    }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet.                          }
{                                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Unit owner: Florent Ouchet                                                                       }
{ Last modified: $Date$                                                      }
{ Revision: $Revision$                                                                       }
{                                                                                                  }
{**************************************************************************************************}

unit JclVersionCtrlCVSImpl;

{$I jcl.inc}

interface

uses
  SysUtils, Windows, Classes, Graphics,
  VersionControlImpl;

type
  TJclVersionControlCVS = class (TJclVersionControlPlugin)
  private
    FTortoiseCVSAct: string;
  protected
    function GetSupportedActions: TJclVersionControlActions; override;
    function GetFileActions(const FileName: string): TJclVersionControlActions; override;
    function GetSandboxActions(const SdBxName: string): TJclVersionControlActions; override;
    function GetIcon(const Action: TJclVersionControlAction): Integer; override;
    function GetEnabled: Boolean; override;
    function GetName: string; override;
  public
    constructor Create(const AExpert: TJclVersionControlExpert); override;
    function GetSandboxNames(const FileName: string; SdBxNames: TStrings): Boolean; override;
    function ExecuteAction(const FileName: string;
      const Action: TJclVersionControlAction): Boolean; override;
  end;

implementation

uses
  JclFileUtils, JclRegistry,
  JclOtaUtils, JclOtaResources, JclOtaConsts;

const
  JclVersionCtrlCVSTrtseShlDLL = 'TrtseShl.dll';
  JclVersionCtrlCVSRegKeyName = 'SOFTWARE\TortoiseCVS';
  JclVersionCtrlCVSRegValueName = 'RootDir';
  JclVersionCtrlCVSTortoiseAct = 'TortoiseAct.exe';
  JclVersionCtrlCVSDirectory = 'CVS\';
  JclVersionCtrlCVSEntriesFile = 'Entries';

  JclVersionControlCVSAddVerb = 'CVSAdd';
  JclVersionControlCVSAddRecurseVerb = 'CVSAddRecursive';
  JclVersionControlCVSAnnotateVerb = 'CVSAnnotate';
  JclVersionControlCVSBranchVerb = 'CVSBranch';
  JclVersionControlCVSCheckOutVerb = 'CVSCheckOut';
  JclVersionControlCVSCommitVerb = 'CVSCommitDialog';
  JclVersionControlCVSDiffVerb = 'CVSDiff';
  JclVersionControlCVSGraphVerb = 'CVSRevisionGraph';
  JclVersionControlCVSLogVerb = 'CVSLog';
  JclVersionControlCVSEditVerb = 'CVSEdit';
  JclVersionControlCVSListEditorsVerb = 'CVSListEditors';
  JclVersionControlCVSTagVerb = 'CVSTag';
  JclVersionControlCVSUpdateVerb = 'CVSUpdate';
  JclVersionControlCVSUpdateDialogVerb = 'CVSUpdateDialog';
  JclVersionControlCVSUnEditVerb = 'CVSUnedit';

resourcestring
  RsVersionCtrlCVSName = 'cvs';
  RsEEmptyFileName = 'Error: empty file name';
  RSENoTortoiseCVS = 'TortoiseCVS is not detected on the system';

//=== TJclVersionControlCVS ==================================================

constructor TJclVersionControlCVS.Create(const AExpert: TJclVersionControlExpert);
begin
  inherited Create(AExpert);
  FTortoiseCVSAct := RegReadStringDef(HKLM, JclVersionCtrlCVSRegKeyName,
    JclVersionCtrlCVSRegValueName, '');

  if FTortoiseCVSAct <> '' then
    FTortoiseCVSAct := PathAddSeparator(FTortoiseCVSAct) + JclVersionCtrlCVSTortoiseAct;
end;

function TJclVersionControlCVS.ExecuteAction(const FileName: string;
  const Action: TJclVersionControlAction): Boolean;
  function CallTortoiseCVSAct(const ActionName: string): Boolean;
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
      raise EJClExpertException.CreateTrace(RsENoTortoiseCVS);

    if FileName[Length(FileName)] = PathSeparator then
      CurrentDir := FileName
    else
      CurrentDir := ExtractFilePath(FileName);
      
    CommandLine := Format('%s %s -l "%s"', [FTortoiseCVSAct, ActionName, PathRemoveSeparator(FileName)]);

    Result := CreateProcess(nil, PChar(CommandLine), nil,
      nil, False, 0, nil, PChar(CurrentDir), StartupInfo, ProcessInfo);

    if Result then
    begin
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
    end;
    Result := False;
  end;
begin
  case Action of
    vcaAdd:
      Result := CallTortoiseCVSAct(JclVersionControlCVSAddVerb);
    vcaAddSandbox:
      Result := CallTortoiseCVSAct(JclVersionControlCVSAddRecurseVerb);
    vcaBlame:
      Result := CallTortoiseCVSAct(JclVersionControlCVSAnnotateVerb);
    vcaBranch,
    vcaBranchSandbox:
      Result := CallTortoiseCVSAct(JclVersionControlCVSBranchVerb);
    vcaCheckOutSandbox:
      Result := CallTortoiseCVSAct(JclVersionControlCVSCheckOutVerb);
    vcaCommit,
    vcaCommitSandbox:
      Result := CallTortoiseCVSAct(JclVersionControlCVSCommitVerb);
    vcaDiff:
      Result := CallTortoiseCVSAct(JclVersionControlCVSDiffVerb);
    vcaGraph:
      Result := CallTortoiseCVSAct(JclVersionControlCVSGraphVerb);
    vcaLog,
    vcaLogSandbox:
      Result := CallTortoiseCVSAct(JclVersionControlCVSLogVerb);
    vcaLock,
    vcaLockSandbox:
      Result := CallTortoiseCVSAct(JclVersionControlCVSEditVerb);
    vcaStatus,
    vcaStatusSandbox:
      Result := CallTortoiseCVSAct(JclVersionControlCVSListEditorsVerb);
    vcaTag,
    vcaTagSandBox:
      Result := CallTortoiseCVSAct(JclVersionControlCVSTagVerb);
    vcaUpdate,
    vcaUpdateSandbox:
      Result := CallTortoiseCVSAct(JclVersionControlCVSUpdateVerb);
    vcaUpdateTo,
    vcaUpdateSandboxTo:
      Result := CallTortoiseCVSAct(JclVersionControlCVSUpdateDialogVerb);
    vcaUnlock,
    vcaUnlockSandbox:
      Result := CallTortoiseCVSAct(JclVersionControlCVSUnEditVerb);
    else
      Result := inherited ExecuteAction(FileName, Action);
  end;
end;

function TJclVersionControlCVS.GetEnabled: Boolean;
begin
  Result := FTortoiseCVSAct <> '';
end;

function TJclVersionControlCVS.GetFileActions(
  const FileName: string): TJclVersionControlActions;
var
  CvsDirectory: string;
  Entries: TStrings;
  Index: Integer;
  FileNameLine: string;
  Added: Boolean;
begin
  Result := inherited GetFileActions(FileName);

  CvsDirectory := PathAddSeparator(ExtractFilePath(FileName)) + JclVersionCtrlCVSDirectory;
  FileNameLine := Format('/%s/', [ExtractFileName(AnsiUpperCaseFileName(FileName))]);

  if DirectoryExists(CvsDirectory) and Enabled then
  begin
    Entries := TStringList.Create;
    try
      Entries.LoadFromFile(CvsDirectory + JclVersionCtrlCVSEntriesFile);
      Added := False;
      for Index := 0 to Entries.Count - 1 do
        if Pos(FileNameLine, AnsiUpperCase(Entries.Strings[Index])) = 1 then
      begin
        Added := True;
        Break;
      end;

      if Added then
      // TODO: check modifications
        Result := Result + [vcaBlame, vcaBranch, vcaCommit, vcaDiff, vcaGraph,
          vcaLog, vcaLock, vcaStatus, vcaTag, vcaUpdate, vcaUpdateTo, vcaUnlock]
      else
        Result := Result + [vcaAdd];
    finally
      Entries.Free;
    end;
  end;
end;

function TJclVersionControlCVS.GetSupportedActions: TJclVersionControlActions;
begin
  Result := inherited GetSupportedActions;
  if Enabled then
    Result := Result + [vcaAdd, vcaAddSandbox, vcaBlame, vcaBranch,
    vcaBranchSandbox, vcaCheckOutSandbox, vcaCommit, vcaCommitSandbox,
    vcaDiff, vcaGraph, vcaLog, vcaLogSandbox, vcaLock, vcaLockSandbox,
    vcaStatus, vcaStatusSandbox, vcaTag, vcaTagSandBox, vcaUpdate,
    vcaUpdateSandbox, vcaUpdateTo, vcaUpdateSandboxTo, vcaUnlock, vcaUnlockSandbox];
end;

function TJclVersionControlCVS.GetIcon(
  const Action: TJclVersionControlAction): Integer;
var
  LibraryName: string;
begin
  LibraryName := PathAddSeparator(ExtractFilePath(FTortoiseCVSAct)) + JclVersionCtrlCVSTrtseShlDLL;

  case Action of
    vcaAdd,
    vcaAddSandbox:
      Result := Expert.CacheResourceIcon(LibraryName, 'IDI_ADD');
    vcaGraph,
    vcaLog,
    vcaLogSandbox,
    vcaBlame:
      Result := Expert.CacheResourceIcon(LibraryName, 'IDI_LOG');
    vcaBranch,
    vcaBranchSandbox:
      Result := Expert.CacheResourceIcon(LibraryName, 'IDI_BRANCH');
    vcaCheckOutSandbox:
      Result := Expert.CacheResourceIcon(LibraryName, 'IDI_CHECKOUT');
    vcaCommit,
    vcaCommitSandbox:
      Result := Expert.CacheResourceIcon(LibraryName, 'IDI_COMMIT');
    vcaDiff:
      Result := Expert.CacheResourceIcon(LibraryName, 'IDI_COMPARE');
    vcaLock,
    vcaLockSandbox,
    vcaStatus,
    vcaStatusSandbox,
    vcaTag,
    vcaTagSandBox:
      Result := -1;
    vcaUpdate,
    vcaUpdateSandbox,
    vcaUpdateTo,
    vcaUpdateSandboxTo:
      Result := Expert.CacheResourceIcon(LibraryName, 'IDI_UPDATE');
    vcaUnlock,
    vcaUnlockSandbox:
      Result := Expert.CacheResourceIcon(LibraryName, 'IDI_REVERT');
    else
      Result := inherited GetIcon(Action);
  end;
end;

function TJclVersionControlCVS.GetName: string;
begin
  Result := RsVersionCtrlCVSName;
end;

function TJclVersionControlCVS.GetSandboxActions(
  const SdBxName: string): TJclVersionControlActions;
var
  CvsDirectory: string;
begin
  Result := inherited GetSandboxActions(SdBxName) + [vcaAddSandbox];

  CvsDirectory := sdBxName + JclVersionCtrlCvsDirectory;

  if Enabled then
  begin
    if DirectoryExists(CvsDirectory) then
      Result := Result + [vcaAddSandbox, vcaBranchSandbox, vcaCommitSandbox,
        vcaLogSandbox, vcaLockSandbox, vcaStatusSandbox, vcaTagSandBox,
        vcaUpdateSandbox, vcaUpdateSandboxTo, vcaUnlockSandbox]
    else
      Result := Result + [vcaCheckOutSandbox];
  end;
end;

function TJclVersionControlCVS.GetSandboxNames(const FileName: string;
  SdBxNames: TStrings): Boolean;
var
  DirectoryName: string;
  Index: Integer;
begin
  Result := True;

  SdBxNames.BeginUpdate;
  try
    SdBxNames.Clear;

    if Enabled then
      for Index := Length(FileName) downto 1 do
        if FileName[Index] = PathSeparator then
    begin
      DirectoryName := Copy(FileName, 1, Index);
      if DirectoryExists(DirectoryName + JclVersionCtrlCVSDirectory) then
        SdBxNames.Add(DirectoryName);
    end;

    if SdBxNames.Count = 0 then
      Result := inherited GetSandboxNames(FileName, SdBxNames);
  finally
    SdBxNames.EndUpdate;
  end;
end;

initialization

try
  TJclVersionControlExpert.RegisterPluginClass(TJclVersionControlCVS);
except
  on ExceptionObj: TObject do
  begin
    JclExpertShowExceptionDialog(ExceptionObj);
    raise;
  end;
end;

finalization

try
  TJclVersionControlExpert.UnregisterPluginClass(TJclVersionControlCVS);
except
  on ExceptionObj: TObject do
  begin
    JclExpertShowExceptionDialog(ExceptionObj);
    raise;
  end;
end;

// History:

// $Log$
// Revision 1.1  2006/01/15 00:51:22  outchy
// cvs support in version control expert
// version control expert integration in the installer
//

end.
