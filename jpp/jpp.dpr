{ **************************************************************************** }
{                                                                              }
{    Pascal PreProcessor                                                       }
{    Copyright (c) 2001 Barry Kelly.                                           }
{    barry_j_kelly@hotmail.com                                                 }
{                                                                              }
{    The contents of this file are subject to the Mozilla Public License       }
{    Version 1.1 (the "License"); you may not use this file except in          }
{    compliance with the License. You may obtain a copy of the License at      }
{    http://www.mozilla.org/MPL/                                               }
{                                                                              }
{    Software distributed under the License is distributed on an "AS IS"       }
{    basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   }
{    License for the specific language governing rights and limitations        }
{    under the License.                                                        }
{                                                                              }
{    The Original Code is ppp.dpr                                              }
{                                                                              }
{    The Initial Developer of the Original Code is Barry Kelly.                }
{    Portions created by Barry Kelly are Copyright (C) 2001                    }
{    Barry Kelly. All Rights Reserved.                                         }
{                                                                              }
{    Contributors: Robert Rossmair, Peter Thörnqvist                           }
{                                                                              }
{    Alternatively, the contents of this file may be used under the terms      }
{    of the Lesser GNU Public License (the  "LGPL License"), in which case     }
{    the provisions of LGPL License are applicable instead of those            }
{    above.  If you wish to allow use of your version of this file only        }
{    under the terms of the LPGL License and not to allow others to use        }
{    your version of this file under the MPL, indicate your decision by        }
{    deleting  the provisions above and replace  them with the notice and      }
{    other provisions required by the LGPL License.  If you do not delete      }
{    the provisions above, a recipient may use your version of this file       }
{    under either the MPL or the LPGL License.                                 }
{                                                                              }
{ **************************************************************************** }

// Last modified: $Date$
// For history, see end of file

{$APPTYPE CONSOLE}
program jpp;

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  Libc,
  {$ENDIF}
  {$IFDEF HAS_UNIT_TYPES}
  Types,
  {$ENDIF HAS_UNIT_TYPES}
  SysUtils,
  Classes,
  TypInfo,
  JclFileUtils,
  JppState in 'JppState.pas',
  JppParser in 'JppParser.pas',
  FindFileIter in 'FindFileIter.pas',
  JppLexer in 'JppLexer.pas',
  PCharUtils in 'PCharUtils.pas';

const
  SubstChar = '_';
  ProcessedExtension = '.jpp';
  SWarningJppGenerated =
    '{**************************************************************************************************}'#13#10 +
    '{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }'#13#10 +
    '{**************************************************************************************************}'#13#10;

procedure Syntax;
begin
  Writeln(
    'JEDI PreProcessor v. 2004-12-03'#10,
    'Copyright (C) 2001 Barry Kelly'#10,
    #10,
    'Syntax:'#10,
    '  ' + ParamStr(0) + ' [options] <input files>...'#10,
    #10,
    'Options:'#10,
    '  -c'#9#9'Process conditional directives'#10,
    '  -C'#9#9'Strip comments'#10,
    '  -fxxx'#9#9'Prefix xxx to filename'#10,
    '  -h, -?'#9'This help'#10,
    '  -i[x[,y...]]'#9'Process includes, except files x, y, ...'#10,
    '  -pxxx'#9#9'Add xxx to include path'#10,
    '  -dxxx'#9#9'Define xxx as a preprocessor conditional symbol'#10,
    '  -uxxx'#9#9'Assume preprocessor conditional symbol xxx as not defined'#10,
    '  -rx[,y...]'#9'Comma-separated list of strings to replace underscores'#10,
    #9#9'in input file names with'#10,
    //'  -x[n:]yyy'#9'Strip first n characters from file name; precede filename by prefix yyy'#10,
    #10,
    'When required to prevent the original file from being overwritten, '#10 +
    'the processed file''s extension will be changed to ', ProcessedExtension, #10,
    'If you have any suggestions or bug-reports, contact me at'#10,
    'barry_j_kelly@hotmail.com'
  );
  Halt(2);
end;

procedure Process(AState: TJppState; const AOld, ANew: string);
var
  parse: TJppParser;
  fsIn, fsOut: TStream;
  answer: string;
begin
  fsOut := nil;
  parse := nil;
  fsIn := nil;
  AState.PushState;
  try
    fsIn := TFileStream.Create(AOld, fmOpenRead);
    parse := TJppParser.Create(fsIn, AState);
    answer := Format('%s'#13#10'%s', [SWarningJppGenerated, parse.Parse]);
    fsOut := TFileStream.Create(ANew, fmCreate);
    fsOut.WriteBuffer(Pointer(answer)^, Length(answer));
  finally
    AState.PopState;
    fsOut.Free;
    parse.Free;
    fsIn.Free;
  end;
end;

procedure Substitute(var S: string; SubstChar: Char; SubstStrings: TStrings);
var
  I, J, K, N, Count: Integer;
  Result, SubstString: string;
  SubstDone: Boolean;
begin
  if SubstStrings = nil then
    Exit;

  Count := SubstStrings.Count;

  if Count = 0 then
    Exit;

  SetLength(Result, Length(S) + Length(SubstStrings.Text)); // sufficient length
  J := 1;
  N := 0;
  SubstDone := False;
  for I := 1 to Length(S) do
    if (S[I] = SubstChar) and not SubstDone then
    begin
      SubstString := SubstStrings[N];
      for K := 1 to Length(SubstString) do
      begin
        Result[J] := SubstString[K];
        Inc(J);
      end;
      Inc(N);
      SubstDone := N = SubstStrings.Count;
    end
    else
    begin
      Result[J] := S[I];
      Inc(J);
    end;
  SetLength(Result, J - 1);
  S := Result;
end;

procedure Params(ACommandLine: PChar);
var
  pppState: TJppState;
  StripLength: Integer; // RR
  Prefix, ReplaceString: string; // RR
  N: Integer;
  ReplaceStrings: TStringList;

  function HandleOptions(cp: PChar): PChar;

    function CheckOpt(cp: PChar; AOpt: TPppOption): PChar;
    begin
      case cp^ of
        '+':
          pppState.Options := pppState.Options + [AOpt];
        '-':
          pppState.Options := pppState.Options - [AOpt];
      else
        pppState.Options := pppState.Options + [AOpt];
      end;
      if cp^ in ['+', '-'] then
        Result := cp + 1
      else
        Result := cp;
    end;

  var
    tmp: string;
  begin
    cp := SkipWhite(cp);

    while cp^ = '-' do
    begin
      Inc(cp);

      case cp^ of
        'f', 'F': // RR
          begin
            Inc(cp);
            cp := ReadStringDoubleQuotedMaybe(cp, Prefix);
            Prefix := ExpandUNCFilename(Prefix);
          end;

        'h', 'H', '?':
          Syntax;

        'i', 'I':
          begin
            cp := ReadStringDoubleQuotedMaybe(CheckOpt(cp + 1, poProcessIncludes), tmp);
            pppState.ExcludedIncludes.CommaText := tmp;
          end;

        'c':
          cp := CheckOpt(cp + 1, poProcessDefines);

        'C':
          cp := CheckOpt(cp + 1, poStripComments);

        'p', 'P':
          begin
            Inc(cp);
            cp := ReadStringDoubleQuotedMaybe(cp, tmp);
            pppState.SearchPath.Add(ExpandUNCFileName(tmp));
          end;

        'd':
          begin
            Inc(cp);
            cp := ReadIdent(cp, tmp);
            pppState.Define(tmp);
          end;

        'r':
          begin
            Inc(cp);
            cp := ReadStringDoubleQuotedMaybe(cp, ReplaceString);
            ReplaceStrings.CommaText := ReplaceString;
          end;

        'u': // RR
          begin
            Inc(cp);
            cp := ReadIdent(cp, tmp);
            pppState.Undef(tmp);
          end;

        'x', 'X': // RR
          begin
            Inc(cp);
            cp := ReadStringDoubleQuotedMaybe(cp, Prefix);
            Val(Prefix, StripLength, N);
            if N > 1 then
              Prefix := Copy(Prefix, N + 1, Length(Prefix));
            Prefix := ExpandUNCFilename(Prefix);
          end;

      else
        Syntax;
      end;

      cp := SkipWhite(cp);
    end;
    Result := cp;
  end;

  function HandleFiles(cp: PChar): PChar;
  var
    NewName, tmp: string;
    iter: IFindFileIterator;
  begin
    while not (cp^ in ['-', #0]) do
    begin
      cp := SkipWhite(ReadStringDoubleQuotedMaybe(cp, tmp));

      if CreateFindFile(ExpandUNCFileName(tmp), faAnyFile and not faDirectory, iter) then
        repeat
          try
            if StripLength > 0 then
              NewName := Copy(ExtractFileName(iter.Name), StripLength + 1, Length(iter.Name))
            else
              NewName := ExtractFileName(iter.Name);

            Substitute(NewName, SubstChar, ReplaceStrings);

            NewName := ExpandUNCFileName(Prefix + NewName);

            if iter.Name = NewName then
              ChangeFileExt(NewName, ProcessedExtension);
            Process(pppState, iter.Name, NewName);
          except
            on e: Exception do
              Writeln(Format('Error: %s %s', [e.Message, iter.Name]));
          end;
        until not iter.Next
      else
        Writeln('Could not find ', tmp);
    end;
    Result := cp;
  end;

var
  cp: PChar;
begin
  cp := ACommandLine;
  StripLength := 0;
  pppState := nil;
  ReplaceStrings := nil;
  try
    pppState := TJppState.Create;
    ReplaceStrings := TStringList.Create;
    repeat
      cp := HandleOptions(cp);
      cp := HandleFiles(cp);
    until cp^ = #0;
  finally
    pppState.Free;
    ReplaceStrings.Free;
  end;
end;

var
  CommandLine: string;
  i: Integer;
begin
  try
    i := 1;
    if ParamCount = 0 then
      Syntax
    else
    begin
      while i <= ParamCount do
      begin
        CommandLine := CommandLine + ' ' + ParamStr(i);
        Inc(i);
      end;
      Params(PChar(CommandLine));
    end;
  except
    on e: Exception do
      Writeln(e.Message);
  end;

// History:

// Modifications by Robert Rossmair:  Added options "-u", "-x" and related code
// $Log$
// Revision 1.9  2004/12/03 04:17:19  rrossmair
// - "i" option changed to allow for excluding specified files from processing
//
// Revision 1.8  2004/08/23 16:42:35  rrossmair
// added -r option
//
// Revision 1.7  2004/06/20 03:24:48  rrossmair
// - orphaned line breaks problem fixed.
//
// Revision 1.6  2004/06/04 02:49:24  rrossmair
// - bug fix: StripLength was eventually not initialized
// - better error message formatting
//
// Revision 1.5  2004/04/18 06:25:07  rrossmair
// extension change for processed file only when necessary
//

end.
