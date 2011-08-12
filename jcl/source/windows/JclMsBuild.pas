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
{ The Original Code is JclMsBuild.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet.                                    }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet. All Rights Reserved.     }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Routines around MsBuild project files.                                                           }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclMsBuild;

{$I jcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes,
  JclBase,
  JclSimpleXml;

type
  EJclMsBuildError = class(EJclError);

// this function parses MsBuild condition as described at:
// http://msdn.microsoft.com/en-us/library/7szfhaft.aspx
function ParseCondition(const Condition: string; Defines: TStrings): Boolean; overload;
function ParseCondition(const Condition: string; var Position: Integer; Len: Integer;
  Defines: TStrings): Boolean; overload;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\windows';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils,
  JclStrings,
  JclDevToolsResources;

function EvaluateString(const Condition: string; Defines: TStrings): string;
var
  Start, Position, Len: Integer;
  PropertyName: string;
begin
  Result := Condition;
  if Result <> '' then
  begin
    // evaluate variables
    repeat
      // start with the last match in order to convert $(some$(other))
      Start := StrLastPos('$(', Result);
      if Start > 0 then
      begin
        Len := Length(Result);
        Position := Start;
        while (Position <= Len) and (Result[Position] <> ')') do
          Inc(Position);
        if Position > Len then
          raise EJclMsBuildError.CreateRes(@RsEEndOfString);
        PropertyName := Copy(Result, Start + 2, Position - Start - 2);
        StrReplace(Result,
                   Copy(Result, Start, Position - Start + 1), // $(some)
                   Defines.Values[PropertyName], // some
                   [rfReplaceAll]);
      end;
    until Start = 0;
    // convert hexa to decimal
    if Copy(Result, 1, 2) = '0x' then
      Result := IntToStr(StrToInt('$' + Copy(Result, 3, Length(Result) - 2)));
  end;
end;

function ParseString(const Condition: string; var Position: Integer; Len: Integer;
  Defines: TStrings): string;
var
  StartPos, EndPos: Integer;
  HasQuote: Boolean;
  FileOrDirectory: string;
begin
  // skip heading spaces
  while (Position <= Len) and CharIsWhiteSpace(Condition[Position]) do
    Inc(Position);
  StartPos := Position;
  HasQuote := Condition[Position] = NativeSingleQuote;
  if HasQuote then
  begin
    // skip heading quote
    Inc(StartPos);
    Inc(Position);
    // quoted string
    while (Position <= Len) and (Condition[Position] <> NativeSingleQuote) do
      Inc(Position);
    if Position > Len then
      raise EJclMsBuildError.CreateRes(@RsEEndOfString);
    EndPos := Position;
    // skip closing quote
    Inc(Position);
  end
  else
  begin
    // alphanumeric strings do not need to be quoted
    while (Position <= Len) and CharIsValidIdentifierLetter(Condition[Position]) do
      Inc(Position);
    EndPos := Position;
  end;
  Result := Copy(Condition, StartPos, EndPos - StartPos);

  // evaluate builtin operators and functions
  if (Result = '') and (Condition[StartPos] = '!') and not HasQuote then
  begin
    Inc(Position);
    Result := BoolToStr(not StrToBool(ParseString(Condition, Position, Len, Defines)), True);
  end
  else
  if (CompareText(Result, 'Exists') = 0) and not HasQuote then
  begin
    while (Position <= Len) and CharIsWhiteSpace(Condition[Position]) do
      Inc(Position);
    // skip opening parenthesis
    if Condition[Position] <> '(' then
      raise EJclMsBuildError.CreateRes(@RsEMissingParenthesis);
    Inc(Position);
    FileOrDirectory := ParseString(Condition, Position, Len, Defines);
    Result := BoolToStr(FileExists(FileOrDirectory) or DirectoryExists(FileOrDirectory), True);
    while (Position <= Len) and CharIsWhiteSpace(Condition[Position]) do
      Inc(Position);
    // skip closing parenthesis
    if Condition[Position] <> ')' then
      raise EJclMsBuildError.CreateRes(@RsEMissingParenthesis);
    Inc(Position);
  end
  else
  if (CompareText(Result, 'HasTrailingSlash') = 0) and not HasQuote then
  begin
    while (Position <= Len) and CharIsWhiteSpace(Condition[Position]) do
      Inc(Position);
    // skip opening parenthesis
    if Condition[Position] <> '(' then
      raise EJclMsBuildError.CreateRes(@RsEMissingParenthesis);
    Inc(Position);
    FileOrDirectory := ParseString(Condition, Position, Len, Defines);
    Result := BoolToStr((FileOrDirectory <> '') and (FileOrDirectory[Length(FileOrDirectory)] = PathDelim), True);
    while (Position <= Len) and CharIsWhiteSpace(Condition[Position]) do
      Inc(Position);
    // skip closing parenthesis
    if Condition[Position] <> ')' then
      raise EJclMsBuildError.CreateRes(@RsEMissingParenthesis);
    Inc(Position);
  end
  else
    Result := EvaluateString(Result, Defines);
  // skip tailing spaces
  while (Position <= Len) and CharIsWhiteSpace(Condition[Position]) do
    Inc(Position);
end;

function ParseCondition(const Condition: string; var Position: Integer; Len: Integer;
  Defines: TStrings): Boolean;
type
  TOperator = (opUnknown, opEqual, opNotEqual, opLess, opLessOrEqual, opGreater, OpGreaterOrEqual, opAnd, opOr);
var
  LeftOperand, RightOperand: string;
  MiddleOperator: TOperator;
begin
  Result := True;
  if Condition <> '' then
  begin
    // read first word
    LeftOperand := ParseString(Condition, Position, Len, Defines);
    if (LeftOperand = '') and (Position <= Len) and (Condition[Position] = '(') then
    begin
      // skip opening parenthesis
      Inc(Position);
      LeftOperand := BoolToStr(ParseCondition(Condition, Position, Len, Defines), True);
      while (Position <= Len) and CharIsWhiteSpace(Condition[Position]) do
        Inc(Position);
      if Condition[Position] <> ')' then
        raise EJclMsBuildError.CreateRes(@RsEMissingParenthesis);
      // skip closing parenthesis
      Inc(Position);
    end;
    while (Position <= Len) and CharIsWhiteSpace(Condition[Position]) do
      Inc(Position);
    // read second word if any
    if Position <= Len then
    begin
      // read infix operator
      MiddleOperator := opUnknown;
      if (Position <= (Len + 2)) and (MiddleOperator = opUnknown) then
      begin
        if ((Condition[Position] = 'A') or (Condition[Position] = 'a')) and
           ((Condition[Position + 1] = 'N') or (Condition[Position + 1] = 'n')) and
           ((Condition[Position + 2] = 'D') or (Condition[Position + 2] = 'd'))  then
          MiddleOperator := opAnd;
        if MiddleOperator <> opUnknown then
          Inc(Position, 3);
      end;
      if (Position <= (Len + 1)) and (MiddleOperator = opUnknown) then
      begin
        if (Condition[Position] = '=') and (Condition[Position + 1] = '=') then
          MiddleOperator := opEqual
        else
        if (Condition[Position] = '!') and (Condition[Position + 1] = '=') then
          MiddleOperator := opNotEqual
        else
        if (Condition[Position] = '<') and (Condition[Position + 1] = '=') then
          MiddleOperator := opLessOrEqual
        else
        if (Condition[Position] = '>') and (Condition[Position + 1] = '=') then
          MiddleOperator := OpGreaterOrEqual
        else
        if ((Condition[Position] = 'O') or (Condition[Position] = 'o')) and
           ((Condition[Position + 1] = 'R') or (Condition[Position + 1] = 'r')) then
          MiddleOperator := opOr;
        if MiddleOperator <> opUnknown then
          Inc(Position, 2);
      end;
      if (Position <= Len) and (MiddleOperator = opUnknown) then
      begin
        if Condition[Position] = '<' then
          MiddleOperator := opLess
        else
        if Condition[Position] = '>' then
          MiddleOperator := opGreater;
        if MiddleOperator <> opUnknown then
          Inc(Position);
      end;
      // read right operand
      RightOperand := ParseString(Condition, Position, Len, Defines);
      if (RightOperand = '') and (Position <= Len) and (Condition[Position] = '(') then
      begin
        // skip opening parenthesis
        Inc(Position);
        RightOperand := BoolToStr(ParseCondition(Condition, Position, Len, Defines), True);
        while (Position <= Len) and CharIsWhiteSpace(Condition[Position]) do
          Inc(Position);
        if Condition[Position] <> ')' then
          raise EJclMsBuildError.CreateRes(@RsEMissingParenthesis);
        // skip closing parenthesis
        Inc(Position);
      end;
      while (Position <= Len) and CharIsWhiteSpace(Condition[Position]) do
        Inc(Position);

      if RightOperand = '' then
        raise EJclMsBuildError.CreateRes(@RsEEmptyIdentifier);
      case MiddleOperator of
        opUnknown:
          raise EJclMsBuildError.CreateRes(@RsEUnknownOperator);
        opEqual:
          Result := LeftOperand = RightOperand;
        opNotEqual:
          Result := LeftOperand <> RightOperand;
        opLess:
          Result := StrToInt(LeftOperand) < StrToInt(RightOperand);
        opLessOrEqual:
          Result := StrToInt(LeftOperand) <= StrToInt(RightOperand);
        opGreater:
          Result := StrToInt(LeftOperand) > StrToInt(RightOperand);
        OpGreaterOrEqual:
          Result := StrToInt(LeftOperand) >= StrToInt(RightOperand);
        opAnd:
          Result := StrToBool(LeftOperand) and StrToBool(RightOperand);
        opOr:
          Result := StrToBool(LeftOperand) or StrToBool(RightOperand);
      end;
    end
    else
    if LeftOperand = '' then
      raise EJclMsBuildError.CreateRes(@RsEEmptyIdentifier)
    else
      // no second word
      Result := StrToBool(LeftOperand)
  end;
end;

function ParseCondition(const Condition: string; Defines: TStrings): Boolean;
var
  Position, Len: Integer;
begin
  Len := Length(Condition);
  Position := 1;
  Result := ParseCondition(Condition, Position, Len, Defines);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
