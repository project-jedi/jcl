{ **************************************************************************** }
{                                                                              }
{    Pascal PreProcessor Parser                                                }
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
{    The Original Code is PppParser.pas                                        }
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

unit JppParser;

{$I jedi.inc}

interface

uses
  SysUtils, Classes, JppState, JppLexer;

type
  EPppParserError = class(Exception);

  TJppParser = class
  private
    FLexer: TJppLexer;
    FState: TJppState;
    FTriState: TTriState;
    FResult: string;
    FResultLen: Integer;
    FLineBreakPos: Integer;
    FSkipLevel: Integer;
    FAllWhiteSpaceIn: Boolean;
    FAllWhiteSpaceOut: Boolean;
    procedure RemoveOrphanedLineBreaks;
  protected
    procedure AddResult(const S: string);
    procedure Emit(const AText: string);
    function IsExcludedInclude(const FileName: string): Boolean;

    procedure NextToken;

    procedure ParseText;
    procedure ParseCondition(Token: TJppToken);
    function ParseInclude: string;

    procedure ParseDefine;
    procedure ParseUndef;

    // same as ParseText, but throws result away
    procedure Skip;

    property Lexer: TJppLexer read FLexer;
    property State: TJppState read FState;
  public
    constructor Create(AStream: TStream; APppState: TJppState);
    destructor Destroy; override;
    function Parse: string;
  end;

implementation

{$IFDEF MSWINDOWS}
const
  LineBreak = #13#10;

type
  T2Char = array[0..1] of Char;
  PLineBreak = ^T2Char;
{$ENDIF MSWINDOWS}

{$IFDEF UNIX}
const
  LineBreak = #10;

type
  PLineBreak = PChar;
{$ENDIF UNIX}

function AllWhiteSpace(P: PChar): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 1 to Length(P) do
    if P^ in [#9, #10, #13, ' '] then
      Inc(P)
    else
    begin
      Result := False;
      Break;
    end;
end;

{ TJppParser }

constructor TJppParser.Create(AStream: TStream; APppState: TJppState);
begin
  Assert(AStream <> nil);
  Assert(APppState <> nil);

  FLexer := TJppLexer.Create(AStream);
  FState := APppState;
  FTriState := ttDefined;
  FState.Undef('PROTOTYPE');
end;

destructor TJppParser.Destroy;
begin
  FLexer.Free;
  inherited;
end;

procedure TJppParser.AddResult(const S: string);
begin
  if FSkipLevel > 0 then
    Exit;
  while FResultLen + Length(S) > Length(FResult) do
    SetLength(FResult, Length(FResult) * 2);
  Move(S[1], FResult[FResultLen + 1], Length(S));
  if FAllWhiteSpaceOut then
    FAllWhiteSpaceOut := AllWhiteSpace(@FResult[FLineBreakPos]);
  Inc(FResultLen, Length(S));
end;

procedure TJppParser.Emit(const AText: string);
begin
  FResult := FResult + AText;
end;

function TJppParser.IsExcludedInclude(const FileName: string): Boolean;
begin
  Result := State.ExcludedIncludes.IndexOf(FileName) >= 0;
end;

procedure TJppParser.NextToken;
begin
  Lexer.NextTok;

  if FSkipLevel = 0 then
    RemoveOrphanedLineBreaks;
end;

procedure TJppParser.RemoveOrphanedLineBreaks;

  procedure DeleteCurrentLineIfOrphaned;
  begin
    if not FAllWhiteSpaceIn and FAllWhiteSpaceOut then
      if FLineBreakPos <= FResultLen then
      begin
        FResultLen := FLineBreakPos - 1;
        FResult[FResultLen + 1] := #0;
      end;
  end;

begin
  case Lexer.CurrTok of
    ptEof:
      DeleteCurrentLineIfOrphaned;
    ptComment:
      FAllWhiteSpaceIn := False;
    ptText:
      if FAllWhiteSpaceIn then
        FAllWhiteSpaceIn := AllWhiteSpace(PChar(Lexer.TokenAsString));
    ptEol:
      begin
        DeleteCurrentLineIfOrphaned;
        FLineBreakPos := FResultLen + 1;
        FAllWhiteSpaceIn := True;
        FAllWhiteSpaceOut := True;
      end;
    ptDefine,
    ptUndef,
    ptIfdef,
    ptIfndef,
    ptIfopt,
    ptElse,
    ptEndif:
      FAllWhiteSpaceIn := False;
    ptInclude:
      FAllWhiteSpaceIn := IsExcludedInclude(Lexer.TokenAsString);
  else
    // Error
  end;
end;

function TJppParser.Parse: string;
begin
  FLexer.Reset;
  FResult := '';
  FResultLen := 0;
  SetLength(FResult, 64 * 1024);

  ParseText;
  SetLength(FResult, FResultLen);
  Result := FResult;
end;

procedure TJppParser.ParseCondition(Token: TJppToken);
var
  SavedTriState: TTriState;
begin
  SavedTriState := FTriState;
  FTriState := State.TriState[Lexer.TokenAsString];
  try
    if FTriState = ttUnknown then
    begin
      AddResult(Lexer.RawComment);
      NextToken;
      ParseText;
      if Lexer.CurrTok = ptElse then
      begin
        AddResult(Lexer.RawComment);
        NextToken;
        ParseText;
      end;
      AddResult(Lexer.RawComment);
    end
    else
      if ((Token = ptIfdef) and (FTriState = ttDefined))
      or ((Token = ptIfndef) and (FTriState = ttUndef)) then
      begin
        NextToken;
        ParseText;
        if Lexer.CurrTok = ptElse then
        begin
          NextToken;
          Skip;
        end;
      end
      else
      begin
        NextToken;
        Skip;
        if Lexer.CurrTok = ptElse then
        begin
          NextToken;
          ParseText;
        end
        else
          ;
      end;
    if Lexer.CurrTok <> ptEndif then
      Lexer.Error('$ENDIF expected');
    NextToken;
  finally
    FTriState := SavedTriState;
  end;
end;

procedure TJppParser.ParseDefine;
begin
  case FTriState of
    ttUnknown:
      begin
        State.TriState[Lexer.TokenAsString] := ttUnknown;
        AddResult(Lexer.RawComment);
      end;
    ttDefined: State.Define(Lexer.TokenAsString);
  end;
  NextToken;
end;

procedure TJppParser.ParseUndef;
begin
  case FTriState of
    ttUnknown:
      begin
        State.TriState[Lexer.TokenAsString] := ttUnknown;
        AddResult(Lexer.RawComment);
      end;
    ttDefined: State.Undef(Lexer.TokenAsString);
  end;
  NextToken;
end;

function TJppParser.ParseInclude: string;
var
  oldLexer, newLexer: TJppLexer;
  fsIn: TStream;
begin
  Result := '';
  Assert(Lexer.TokenAsString <> '');
  { we must prevent case of $I- & $I+ becoming file names }
  if (Lexer.TokenAsString[1] in ['-', '+'])
  or IsExcludedInclude(Lexer.TokenAsString) then
    Result := Lexer.RawComment
  else
  begin
    fsIn := nil;
    newLexer := nil;

    oldLexer := Lexer;
    try
      try
        fsIn := FState.FindFile(Lexer.TokenAsString);
      except
        on e: Exception do
          Lexer.Error(e.Message);
      end;
      newLexer := TJppLexer.Create(fsIn);
      FLexer := newLexer;
      ParseText;
    finally
      FLexer := oldLexer;
      fsIn.Free;
      newLexer.Free;
    end;
  end;
  NextToken;
end;

procedure TJppParser.ParseText;

  procedure AddRawComment;
  begin
    AddResult(Lexer.RawComment);
    NextToken;
  end;

begin
  while True do
    case Lexer.CurrTok of
      ptComment:
      begin
        if not (poStripComments in State.Options) then
          AddResult(Lexer.TokenAsString);
        NextToken;
      end;

      ptText, ptEol:
      begin
        AddResult(Lexer.TokenAsString);
        NextToken;
      end;

      ptDefine, ptUndef, ptIfdef, ptIfndef, ptIfopt:
        if poProcessDefines in State.Options then
          case Lexer.CurrTok of
            ptDefine:
              ParseDefine;
            ptUndef:
              ParseUndef;
            ptIfdef:
              ParseCondition(ptIfdef);
            ptIfndef:
              ParseCondition(ptIfndef);
            ptIfopt:
              ParseCondition(ptIfopt);
          end
        else
          AddRawComment;

      ptElse, ptEndif:
        if poProcessDefines in State.Options then
          Break
        else
          AddRawComment;

      ptInclude:
        if poProcessIncludes in State.Options then
          AddResult(ParseInclude)
        else
          AddRawComment;
    else
      Break;
    end;
end;

procedure TJppParser.Skip;
begin
  Inc(FSkipLevel);
  try
    ParseText;
  finally;
    Dec(FSkipLevel);
  end;
end;

// History:

// $Log$
// Revision 1.10  2004/12/22 14:31:01  rrossmair
// - fixed TJppParser.ParseInclude: missing Result initialization caused garbage output
//
// Revision 1.9  2004/12/03 04:17:19  rrossmair
// - "i" option changed to allow for excluding specified files from processing
//
// Revision 1.8  2004/10/30 13:30:46  rrossmair
// - fixed TJppParser.ParseUndef bug
//
// Revision 1.7  2004/06/21 00:14:14  rrossmair
// - fixed ParseInclude
// - eventually match $ELSE, $ENDIF to $IFOPT (otherwise not handled)
// - renamed identifiers from JppLexer
// - AllWhiteSpace handles tabulators now
//
// Revision 1.6  2004/06/20 03:24:48  rrossmair
// - orphaned line breaks problem fixed.
//
// Revision 1.5  2004/06/05 19:42:08  rrossmair
// - fixed problems with nested compiler conditions
//
// Revision 1.4  2004/04/18 06:19:06  rrossmair
// introduced pre-undefined symbol "PROTOTYPE"
//

end.

