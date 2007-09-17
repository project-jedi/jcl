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
{ The Original Code is JclOtaTemplates.pas.                                                        }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet                                     }
{         <outchy att users dott sourceforge dott net>                                             }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet. All rights reserved.     }
{                                                                                                  }
{ Contributors:                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclOtaTemplates;

interface

{$I jcl.inc}

uses
  Classes, JclBorlandTools;

type
  TJclOtaTemplateParams = class(TPersistent)
  protected
    FLanguage: TJclBorPersonality;
  public
    function GetBoolValue(const Name: string): Boolean; virtual;
    function IsDefined(const Name: string): Boolean; virtual;
    function GetStrValue(const Name: string): string; virtual;
    function GetIntValue(const Name: string): Integer; virtual;
    function GetStringsValue(const Name: string): TStrings; virtual;

    property Language: TJclBorPersonality read FLanguage write FLanguage;
  end;

const
  ModulePattern = '%MODULENAME%';
  FormPattern = '%FORMNAME%';
  AncestorPattern = '%ANCESTORNAME%';

function GetFinalFormContent(const Content, FormIdent,
  AncestorIdent: string): string;
function GetFinalHeaderContent(const Content, ModuleIdent, FormIdent,
  AncestorIdent: string): string;
function GetFinalSourceContent(const Content, ModuleIdent, FormIdent,
  AncestorIdent: string): string;

function ApplyTemplate(const Template: string;
  const Params: TJclOtaTemplateParams): string;

implementation

uses
  SysUtils,
  {$IFDEF HAS_UNIT_VARIANTS}
//  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  TypInfo,
  JclStrings, JclSysUtils;

function GetFinalFormContent(const Content, FormIdent,
  AncestorIdent: string): string;
begin
  Result := StringReplace(Content, FormPattern, FormIdent, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, AncestorPattern, AncestorIdent, [rfReplaceAll, rfIgnoreCase]);
end;

function GetFinalHeaderContent(const Content, ModuleIdent, FormIdent,
  AncestorIdent: string): string;
begin
  Result := StringReplace(Content, FormPattern, FormIdent, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, AncestorPattern, AncestorIdent, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, ModulePattern, ModuleIdent, [rfReplaceAll, rfIgnoreCase]);
end;

function GetFinalSourceContent(const Content, ModuleIdent, FormIdent,
  AncestorIdent: string): string;
begin
  Result := StringReplace(Content, FormPattern, FormIdent, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, AncestorPattern, AncestorIdent, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, ModulePattern, ModuleIdent, [rfReplaceAll, rfIgnoreCase]);
end;

function ApplyTemplate(const Template: string;
  const Params: TJclOtaTemplateParams): string;
  procedure CopyStr(var Dest: string; var IndexDest: Integer;
    var DestCharCount: Integer; const Src: string; IndexSrc: Integer;
    CharCount: Integer);
  begin
    if (Length(Src) - IndexSrc + 1) < CharCount then
      CharCount := Length(Src) - IndexSrc + 1;
    while (DestCharCount - IndexDest + 1) < CharCount do
    begin
      DestCharCount := 2 * DestCharCount;
      SetLength(Dest, DestCharCount);
    end;

    if CharCount > 0 then
    begin
      Move(Src[IndexSrc], Dest[IndexDest], CharCount);
      Inc(IndexDest, CharCount);
    end;
  end;
  function SkipBlanks(const Str: string; const Index: Integer;
    Count: Integer): Integer;
  begin
    Result := Index;
    while (Result <= Count) and (Str[Result] in AnsiWhiteSpace) do
      Inc(Result);
  end;
  function GetIdentifier(const Str: string; var Index: Integer;
    Count: Integer): string;
  var
    IndexStart: Integer;
  begin
    IndexStart := Index;
    while (Index <= Count) and (Str[Index] in ['0'..'9', 'A'..'Z', 'a'..'z', '_', '%']) do
      Inc(Index);
    Result := Copy(Str, IndexStart, Index - IndexStart);
  end;
var
  IndexInput, IndexOutput, TokenPos, CharCountIn, CharCountOut,
  IfCount, StrIndex, RepeatCount: Integer;
  Identifier, Command, Symbol, StrValue, RepeatPattern, RepeatValue: string;
  StrList: TStrings;
begin
  CharCountIn := Length(Template);
  CharCountOut := 2*CharCountIn;
  SetLength(Result, CharCountOut);
  IndexInput := 1;
  IndexOutput := 1;
  IfCount := 0;
  while IndexInput < CharCountIn do
  begin
    TokenPos := CharPos(Template, '%', IndexInput);

    if TokenPos = 0 then
    begin
      CopyStr(Result, IndexOutput, CharCountOut, Template, IndexInput, CharCountIn - IndexInput + 1);
      SetLength(Result, IndexOutput - 1);
      Exit;
    end
    else
    begin
      if IfCount = 0 then
        CopyStr(Result, IndexOutput, CharCountOut, Template, IndexInput, TokenPos - IndexInput);

      Identifier := GetIdentifier(Template, TokenPos, CharCountIn);
      Command := StrUpper(Identifier);

      if Command = '%IF' then
      begin
        TokenPos := SkipBlanks(Template, TokenPos, CharCountIn);
        Symbol := GetIdentifier(Template, TokenPos, CharCountIn);
        if (IfCount > 0) or not Params.IsDefined(Symbol) then
        begin
          Inc(IfCount);
        end;
      end
      else if Command = '%IFNOT' then
      begin
        TokenPos := SkipBlanks(Template, TokenPos, CharCountIn);
        Symbol := GetIdentifier(Template, TokenPos, CharCountIn);
        if (IfCount > 0) or Params.IsDefined(Symbol) then
          Inc(IfCount);
      end
      else if Command = '%ELSE' then
      begin
        if IfCount = 1 then
          IfCount := 0
        else if IfCount = 0 then
          IfCount := 1;
      end
      else if Command = '%ENDIF' then
      begin
        if IfCount > 0 then
          Dec(IfCount);
      end
      else if Command = '%STRVALUE' then
      begin
        TokenPos := SkipBlanks(Template, TokenPos, CharCountIn);
        Symbol := GetIdentifier(Template, TokenPos, CharCountIn);
        if IfCount = 0 then
        begin
          StrValue := Params.GetStrValue(Symbol);
          case Params.Language of
            bpDelphi32:
              begin
                StrValue := StringReplace(StrValue, AnsiSingleQuote, AnsiSingleQuote + AnsiSingleQuote, [rfReplaceAll]);
                StrValue := AnsiSingleQuote + StrValue + AnsiSingleQuote;
              end;
            bpBCBuilder32:
              begin
                StrValue := StringReplace(StrValue, AnsiDoubleQuote, AnsiBackslash + AnsiDoubleQuote, [rfReplaceAll]);
                StrValue := AnsiDoubleQuote + StrValue + AnsiDoubleQuote;
              end;
          end;
          CopyStr(Result, IndexOutput, CharCountOut, StrValue, 1, Length(StrValue));
        end;
      end
      else if Command = '%INTVALUE' then
      begin
        TokenPos := SkipBlanks(Template, TokenPos, CharCountIn);
        Symbol := GetIdentifier(Template, TokenPos, CharCountIn);
        if IfCount = 0 then
        begin
          StrValue := IntToStr(Params.GetIntValue(Symbol));
          CopyStr(Result, IndexOutput, CharCountOut, StrValue, 1, Length(StrValue));
        end;
      end
      else if Command = '%BOOLVALUE' then
      begin
        TokenPos := SkipBlanks(Template, TokenPos, CharCountIn);
        Symbol := GetIdentifier(Template, TokenPos, CharCountIn);
        if IfCount = 0 then
        begin
          StrValue := BooleanToStr(Params.GetBoolValue(Symbol));
          CopyStr(Result, IndexOutput, CharCountOut, StrValue, 1, Length(StrValue));
        end;
      end
      else if Command = '%REPEATLINE' then
      begin
        TokenPos := SkipBlanks(Template, TokenPos, CharCountIn);
        Symbol := GetIdentifier(Template, TokenPos, CharCountIn);
        if IfCount = 0 then
        begin
          RepeatCount := Params.GetIntValue(Symbol);
          StrIndex := TokenPos;
          while (StrIndex <= CharCountIn) and not (Template[StrIndex] in [AnsiLineFeed, AnsiCarriageReturn]) do
            Inc(StrIndex);
          RepeatPattern := Copy(Template, TokenPos, StrIndex - TokenPos);
          TokenPos := StrIndex;

          while RepeatCount > 0 do
          begin
            StrValue := RepeatPattern;
            StrIndex := Pos('%', StrValue);
            while StrIndex > 0 do
            begin
              Inc(StrIndex);
              Symbol := GetIdentifier(StrValue, StrIndex, Length(StrValue));
              StrList := Params.GetStringsValue(Symbol);
              if Assigned(StrList) then
                RepeatValue := StrList.Strings[RepeatCount - 1]
              else
                RepeatValue := '';
              StrReplace(StrValue, '%' + Symbol, RepeatValue, [rfReplaceAll, rfIgnoreCase]);
              StrIndex := Pos('%', StrValue);
            end;
            CopyStr(Result, IndexOutput, CharCountOut, StrValue, 1, Length(StrValue));
            CopyStr(Result, IndexOutput, CharCountOut, AnsiLineBreak, 1, Length(AnsiLineBreak));
            Dec(RepeatCount);
          end;
        end;
      end
      else if IfCount = 0 then
        CopyStr(Result, IndexOutput, CharCountOut, Identifier, 1, Length(Identifier));
        
      IndexInput := TokenPos;
    end;
  end;
end;

//=== { TJclOtaTemplateParams } ==============================================

function TJclOtaTemplateParams.GetBoolValue(const Name: string): Boolean;
var
  VariantValue: Variant;
begin
  VariantValue := GetPropValue(Self, Name);
  Result := Boolean(VariantValue);
end;

function TJclOtaTemplateParams.GetIntValue(const Name: string): Integer;
var
  VariantValue: Variant;
begin
  VariantValue := GetPropValue(Self, Name);
  Result := Integer(VariantValue);
end;

function TJclOtaTemplateParams.GetStringsValue(const Name: string): TStrings;
var
  Instance: TObject;
begin
  Instance := TObject(GetOrdProp(Self, Name));
  if Instance is TStrings then
    Result := TStrings(Instance)
  else
    Result := nil;
end;

function TJclOtaTemplateParams.GetStrValue(const Name: string): string;
var
  VariantValue: Variant;
begin
  VariantValue := GetPropValue(Self, Name, True);
  Result := string(VariantValue);
end;

function TJclOtaTemplateParams.IsDefined(const Name: string): Boolean;
begin
  Result := GetBoolValue(Name);
end;

end.
