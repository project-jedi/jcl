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
{ The Original Code is MakeJclDbg.dpr.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Command line tool for inserting JCL debug data created from MAP files into executable files      }
{                                                                                                  }
{ Last modified: March 13, 2002                                                                    }
{                                                                                                  }
{**************************************************************************************************}

program MakeJclDbg;

{$I jcl.inc}

{$APPTYPE CONSOLE}

uses
  Windows, Classes, SysUtils,
  JclDebug, JclFileUtils, JclPeImage, JclStrings;

var
  JdbgFlag, InsertToExeFlag: Boolean;

function MakeDebugData(const FileNames: string): Boolean;
var
  FilesList: TStringList;
  I: Integer;
  MapFileSize, BinDataSize: Integer;
  FileName, ExecutableFileName: TFileName;
  LinkerBugUnit: string;

  procedure FindExecutableFileName(const MapFileName: TFileName);
  var
    ExecFilesList: TStringList;
    I, ValidCnt: Integer;
  begin
    ExecutableFileName := '';
    ValidCnt := 0;
    ExecFilesList := TStringList.Create;
    try
      if AdvBuildFileList(ChangeFileExt(MapFileName, '.*'), faArchive, ExecFilesList, amSubSetOf, [flFullNames]) then
        with ExecFilesList do
        begin
          for I := 0 to Count - 1 do
            if IsValidPeFile(Strings[I]) then
            begin
              Objects[I] := Pointer(True);
              Inc(ValidCnt);
              if ExecutableFileName = '' then
                ExecutableFileName := Strings[I];
            end;
          case ValidCnt of
            0: WriteLn(#13#10'Can not find any executable file for the MAP file.');
            1: Write(' -> ' + ExtractFileName(ExecutableFileName));
          else
            ExecutableFileName := '';
            WriteLn(#13#10'Ambiguous executable file names:');
            for I := 0 to Count - 1 do
              if Boolean(Objects[I]) then
                WriteLn(Strings[I]);
          end;
        end;
    finally
      ExecFilesList.Free;
    end;
  end;

begin
  Result := True;
  FilesList := TStringList.Create;
  try
    if AdvBuildFileList(FileNames, faArchive, FilesList, amSubSetOf, [flFullNames]) then
      for I := 0 to FilesList.Count - 1 do
      begin
        FileName := FilesList[I];
        if ExtractFileExt(FileName) <> '.map' then
          Continue;
        Write(#13#10, FilesList[I]);
        Result := False;
        if JdbgFlag then
          Result := ConvertMapFileToJdbgFile(FileName);
        if InsertToExeFlag then
        begin
          FindExecutableFileName(FileName);
          Result := (ExecutableFileName <> '');
          if Result then
            Result := InsertDebugDataIntoExecutableFile(ExecutableFileName,
              FileName, LinkerBugUnit, MapFileSize, BinDataSize);
        end;
        if Result then
          WriteLn(' ... OK')
        else
        begin
          WriteLn(' ... ERROR!');
          Break;
        end;
      end;
  finally
    FilesList.Free;
  end;
end;

begin
  WriteLn('Make JCL debug data command line utility. (c) 2002 Project JEDI');
  JdbgFlag := AnsiSameText(ParamStr(1), '-J');
  InsertToExeFlag := AnsiSameText(ParamStr(1), '-E');
  if (ParamCount <> 2) or not (JdbgFlag xor InsertToExeFlag) then
  begin
    WriteLn('Usage: MAKEJCLDBG -<J|E> <map filenames>');
    WriteLn('       J - Create .JDBG files');
    WriteLn('       E - Insert debug data into executable files');
    WriteLn('Executable files must be in the same directory like MAP files');
  end
  else
  if not MakeDebugData(ParamStr(2)) then
    Halt(1);
end.
