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
{    $Id$              }
{                                                                              }
{ **************************************************************************** }

unit JppParser;

interface

uses
  SysUtils, Classes, JppState, PppLexer;

type
  EPppParserError = class(Exception);

  TJppParser = class
  private
    FLexer: TPppLexer;
    FState: TPppState;
    FResult: string;
    FTriState: TTriState;
  protected
    procedure Emit(const AText: string);

    function ParseText: string;
    function ParseCondition(Token: TPppToken): string;
    function ParseIfdef: string;
    function ParseIfndef: string;
    function ParseInclude: string;

    procedure ParseDefine;
    procedure ParseUndef;

    property Lexer: TPppLexer read FLexer;
    property State: TPppState read FState;
  public
    constructor Create(AStream: TStream; APppState: TPppState);
    destructor Destroy; override;
    function Parse: string;
  end;

implementation

{ TJppParser }

constructor TJppParser.Create(AStream: TStream; APppState: TPppState);
begin
  Assert(AStream <> nil);
  Assert(APppState <> nil);

  FLexer := TPppLexer.Create(AStream);
  FState := APppState;
  FTriState := ttDefined;
end;

destructor TJppParser.Destroy;
begin
  FLexer.Free;
  inherited;
end;

procedure TJppParser.Emit(const AText: string);
begin
  FResult := FResult + AText;
end;

function TJppParser.Parse: string;
begin
  FLexer.Reset;
  FResult := '';

  Result := ParseText;

//  Result := FResult;
end;

procedure TJppParser.ParseDefine;
begin
  State.Define(Lexer.TokenAsString);
  Lexer.NextTok;
end;

function TJppParser.ParseCondition(Token: TPppToken): string;
var
  SavedTriState: TTriState;
begin
  SavedTriState := FTriState;
  FTriState := State.TriState(Lexer.TokenAsString);
  try
    if FTriState = ttUnknown then
    begin
      Result := Lexer.RawComment;
      Lexer.NextTok;
      Result := Result + ParseText;
      if Lexer.CurrTok = ptElse then
      begin
        Result := Result + Lexer.RawComment;
        Lexer.NextTok;
        Result := Result + ParseText;
      end;
      Result := Result + Lexer.RawComment;
    end
    else
      if ((Token = ptIfdef) and (FTriState = ttDefined))
      or ((Token = ptIfndef) and (FTriState = ttUndef)) then
      begin
        Lexer.NextTok;
        Result := ParseText;
        if Lexer.CurrTok = ptElse then
        begin
          Lexer.NextTok;
          ParseText;
        end;
      end
      else
      begin
        Lexer.NextTok;
        ParseText;
        if Lexer.CurrTok = ptElse then
        begin
          Lexer.NextTok;
          Result := ParseText;
        end
        else
          Result := '';
      end;
    if Lexer.CurrTok <> ptEndif then
      Lexer.Error('$ENDIF expected');
    Lexer.NextTok;
  finally
    FTriState := SavedTriState;
  end;
end;

function TJppParser.ParseIfdef: string;
begin
  if State.IsDefined(Lexer.TokenAsString) then
  begin
    Lexer.NextTok;
    Result := ParseText;
    if Lexer.CurrTok = ptElse then
    begin
      Lexer.NextTok;
      ParseText;
    end;
  end
  else
  begin
    Lexer.NextTok;
    ParseText;
    if Lexer.CurrTok = ptElse then
    begin
      Lexer.NextTok;
      Result := ParseText;
    end
    else
      Result := '';
  end;
  if Lexer.CurrTok <> ptEndif then
    Lexer.Error('$ENDIF expected');
  Lexer.NextTok;
end;

function TJppParser.ParseIfndef: string;
begin
  if not State.IsDefined(Lexer.TokenAsString) then
  begin
    Lexer.NextTok;
    Result := ParseText;
    if Lexer.CurrTok = ptElse then
    begin
      Lexer.NextTok;
      ParseText;
    end;
  end
  else
  begin
    Lexer.NextTok;
    ParseText;
    if Lexer.CurrTok = ptElse then
    begin
      Lexer.NextTok;
      Result := ParseText;
    end
    else
      Result := '';
  end;
  if Lexer.CurrTok <> ptEndif then
    Lexer.Error('$ENDIF expected');
  Lexer.NextTok;
end;

function TJppParser.ParseInclude: string;
var
  oldLexer, newLexer: TPppLexer;
  fsIn: TStream;
begin
  Assert(Lexer.TokenAsString <> '');
  { we must prevent case of $I- & $I+ becoming file names }
  if Lexer.TokenAsString[1] in ['-', '+'] then
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
      newLexer := TPppLexer.Create(fsIn);
      FLexer := newLexer;
      Result := Parse;
    finally
      FLexer := oldLexer;
      fsIn.Free;
      newLexer.Free;
    end;
  end;
  Lexer.NextTok;
end;

function TJppParser.ParseText: string;
var
  strBuilder: TStrings;

  function BuildResult: string;
  var
    i, total: Integer;
    cp: PChar;
  begin
    total := 0;
    for i := 0 to strBuilder.Count - 1 do
      total := total + Length(strBuilder[i]);
    SetLength(Result, total);
    cp := Pointer(Result);
    for i := 0 to strBuilder.Count - 1 do
    begin
      Move(strBuilder[i][1], cp^, Length(strBuilder[i]));
      cp := cp + Length(strBuilder[i]);
    end;
  end;

  procedure AddRawComment;
  begin
    strBuilder.Add(Lexer.RawComment);
    Lexer.NextTok;
  end;

begin
  strBuilder := TStringList.Create;
  try
    while True do
      case Lexer.CurrTok of
        ptComment:
        begin
          if not (poStripComments in State.Options) then
            strBuilder.Add(Lexer.TokenAsString);
          Lexer.NextTok;
        end;

        ptText:
        begin
          strBuilder.Add(Lexer.TokenAsString);
          Lexer.NextTok;
        end;

        ptDefine, ptUndef, ptIfdef, ptIfndef:
          if poProcessDefines in State.Options then
            case Lexer.CurrTok of
              ptDefine:
                if FTriState <> ttDefined then
                  AddRawComment
                else
                  ParseDefine;
              ptUndef:
                if FTriState <> ttDefined then
                  AddRawComment
                else
                  ParseUndef;
              ptIfdef:
                strBuilder.Add(ParseCondition(ptIfdef));
              ptIfndef:
                strBuilder.Add(ParseCondition(ptIfndef));
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
            strBuilder.Add(ParseInclude)
          else
            AddRawComment;
      else
        Break;
      end;

    Result := BuildResult;
  finally
    strBuilder.Free;
  end;
end;

procedure TJppParser.ParseUndef;
begin
  State.Undef(Lexer.TokenAsString);
  Lexer.NextTok;
end;

end.

