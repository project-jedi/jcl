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
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclVersionCtrlSVNImpl;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Windows, Graphics,
  JclVersionControl;

type
  TJclVersionControlSVN = class (TJclVersionControlPlugin)
  private
    FTortoiseSVNProc: string;
  protected
    function GetSupportedActionTypes: TJclVersionControlActionTypes; override;
    function GetFileActions(const FileName: TFileName): TJclVersionControlActionTypes; override;
    function GetSandboxActions(const SdBxName: TFileName): TJclVersionControlActionTypes; override;
    function GetEnabled: Boolean; override;
    function GetName: string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetSandboxNames(const FileName: TFileName; SdBxNames: TStrings): Boolean; override;
    function ExecuteAction(const FileName: TFileName;
      const Action: TJclVersionControlActionType): Boolean; override;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\vcl';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclVclResources,
  JclFileUtils, JclSysInfo, JclSysUtils, JclRegistry, JclStrings;

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
//  JclVersionCtrlSVNTortoiseDLL = 'TortoiseSVN.dll';
  JclVersionCtrlSVNDirectory1 = '.svn\';
  JclVersionCtrlSVNDirectory2 = '_svn\';
  JclVersionCtrlSVNEntryFile = 'entries';

  JclVersionCtrlSVNDirectories: array [0..1] of string =
   ( JclVersionCtrlSVNDirectory1, JclVersionCtrlSVNDirectory2 );

//=== TJclVersionControlSVN ==================================================

constructor TJclVersionControlSVN.Create;
var
  SaveAcc: TJclRegWOW64Access;
begin
  inherited Create;

  if IsWindows64 then
  begin
    // on 64 bit machines look in the 64bit section of registy for tortoise SVN (64bit) registry stuff
    SaveAcc := RegGetWOW64AccessMode;
    try
      RegSetWOW64AccessMode(ra64Key);
      FTortoiseSVNProc := RegReadStringDef(HKLM, JclVersionCtrlRegKeyName, JclVersionCtrlRegValueName, '');
    finally
      RegSetWOW64AccessMode(SaveAcc);
    end;
    if FTortoiseSVNProc = '' then // when the 64bit Version is not found try to find the 32bit version
      FTortoiseSVNProc := RegReadStringDef(HKLM, JclVersionCtrlRegKeyName, JclVersionCtrlRegValueName, '');
  end
  else
    FTortoiseSVNProc := RegReadStringDef(HKLM, JclVersionCtrlRegKeyName, JclVersionCtrlRegValueName, '');
end;

destructor TJclVersionControlSVN.Destroy;
begin
  inherited Destroy;
end;

function TJclVersionControlSVN.ExecuteAction(const FileName: TFileName;
  const Action: TJclVersionControlActionType): Boolean;

  function CallTortoiseSVNProc(const ActionName: string;
    const Param: string = ''): Boolean;
  var
    StartupInfo: TStartupInfo;
    ProcessInfo: TProcessInformation;
    CurrentDir, CommandLine: string;
  begin
    ResetMemory(StartupInfo, SizeOf(TStartupInfo));
    ResetMemory(ProcessInfo, SizeOf(TProcessInformation));
    startupInfo.cb := SizeOf(TStartupInfo);
    startupInfo.dwFlags := STARTF_USESHOWWINDOW;
    startupInfo.wShowWindow := SW_SHOW;

    if FileName = '' then
      raise EJclVersionControlError.Create(RsEEmptyFileName);
    if not Enabled then
      raise EJclVersionControlError.Create(RsENoTortoiseSVN);

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
  const FileName: TFileName): TJclVersionControlActionTypes;
var
  EntryLine: string;
  EntryFileName, UpperCaseFileName, XmlFileNameValue: TFileName;
  Entries: TJclAnsiMappedTextReader;
  IndexDir: Integer;
begin
  Result := inherited GetFileActions(FileName);

  if Enabled then
  begin
    UpperCaseFileName := StrUpper(ExtractFileName(FileName));
    XmlFileNameValue := Format('NAME="%s"', [UpperCaseFileName]);

    for IndexDir := Low(JclVersionCtrlSVNDirectories) to High(JclVersionCtrlSVNDirectories) do
    begin
      EntryFileName := PathAddSeparator(ExtractFilePath(FileName))
        + JclVersionCtrlSVNDirectories[IndexDir] + JclVersionCtrlSVNEntryFile;

      if FileExists(EntryFileName) then
      begin
        Entries := TJclAnsiMappedTextReader.Create(EntryFileName);
        try
          while not Entries.Eof do
          begin
            EntryLine := string(Entries.ReadLn);
            // old SVN entries file (xml-like)
            if Pos(XmlFileNameValue, StrUpper(EntryLine)) > 0 then
            begin
              // TODO: check modifications
              Result := Result + [vcaBlame, vcaBranch, vcaCommit, vcaDiff, vcaGraph,
                vcaLog, vcaLock, vcaMerge, vcaRename, vcaRevert, vcaRepoBrowser,
                vcaStatus, vcaTag, vcaUpdate, vcaUpdateTo, vcaUnlock];
              FreeAndNil(Entries);
              Exit;
            end;
            // new SVN entries file (flat-style)
            if EntryLine = NativeFormFeed then
            begin
              EntryLine := string(Entries.ReadLn);
              if StrSame(UpperCaseFileName, StrUpper(EntryLine)) then
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
  end;
end;

function TJclVersionControlSVN.GetSupportedActionTypes: TJclVersionControlActionTypes;
begin
  Result := inherited GetSupportedActionTypes;
  if Enabled then
    Result := Result + [vcaAdd, vcaAddSandbox, vcaBlame, vcaBranch,
    vcaBranchSandbox, vcaCheckOutSandbox, vcaCommit, vcaCommitSandbox, vcaDiff,
    vcaGraph, vcaLog, vcaLogSandbox, vcaLock, vcaLockSandbox, vcaMerge,
    vcaMergeSandbox, vcaRename, vcaRepoBrowser, vcaRevert, vcaRevertSandbox,
    vcaStatus, vcaStatusSandbox, vcaTag, vcaTagSandBox, vcaUpdate,
    vcaUpdateSandbox, vcaUpdateTo, vcaUpdateSandboxTo, vcaUnlock, vcaUnlockSandbox];
end;

function TJclVersionControlSVN.GetName: string;
begin
  Result := LoadResString(@RsVersionCtrlSVNName);
end;

function TJclVersionControlSVN.GetSandboxActions(
  const SdBxName: TFileName): TJclVersionControlActionTypes;
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

function TJclVersionControlSVN.GetSandboxNames(const FileName: TFileName;
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
      DirectoryName := Copy(FileName, 1, IndexFileName);
      for IndexDir := Low(JclVersionCtrlSVNDirectories) to High(JclVersionCtrlSVNDirectories) do
      begin
        if DirectoryExists(DirectoryName + JclVersionCtrlSVNDirectories[IndexDir]) then
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

  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  RegisterVersionControlPluginClass(TJclVersionControlSVN);

finalization

  UnregisterVersionControlPluginClass(TJclVersionControlSVN);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
