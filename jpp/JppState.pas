{ **************************************************************************** }
{                                                                              }
{    PppState - Pascal PreProcessor State                                      }
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
{    The Original Code is PppState.pas                                         }
{                                                                              }
{    The Initial Developer of the Original Code is Barry Kelly.                }
{    Portions created by Barry Kelly are Copyright (C) 2001                    }
{    Barry Kelly. All Rights Reserved.                                         }
{                                                                              }
{    Contributors:                                                             }
{      Florent Ouchet                                                          }
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
{ $Id$ }

{ Brief: The state of the preprocessor; options (e.g. remove comments),
    defines, include search path, etc. }

// Modifications by Robert Rossmair:  Addition of TTriState type, TriState methods
unit JppState;

interface

uses
  SysUtils, Classes, JclBase, JclContainerIntf;

type
  EPppState = class(Exception);

  TPppOption = (poProcessIncludes, poProcessDefines, poStripComments, poProcessMacros);
  TPppOptions = set of TPppOption;

  TTriState = (ttUnknown, ttUndef, ttDefined);

  TPppState = class
  protected
    function GetOptions: TPppOptions; virtual; abstract;
    function GetDefineTriState(const ASymbol: string): TTriState; virtual; abstract;
    procedure SetDefineTriState(const ASymbol: string; const Value: TTriState); virtual; abstract;
  public
    { PushState is called at the start of every unit, and PopState at the
      end. This means that any declarations like $DEFINE will be file-local
      in scope. }
    procedure PushState; virtual; abstract;
    procedure PopState; virtual; abstract;

    function IsDefined(const ASymbol: string): Boolean; virtual; abstract;
    procedure Define(const ASymbol: string); virtual; abstract;
    procedure Undef(const ASymbol: string); virtual; abstract;

    function FindFile(const AName: string): TStream; virtual; abstract;
    procedure AddToSearchPath(const AName: string); virtual; abstract;

    procedure AddFileToExclusionList(const AName: string); virtual; abstract;
    function IsFileExcluded(const AName: string): Boolean; virtual; abstract;

    function ExpandMacro(const AName: string; const ParamValues: TDynStringArray): string; virtual; abstract;
    procedure DefineMacro(const AName: string; const ParamNames: TDynStringArray;
      const Value: string); virtual; abstract;
    procedure UndefMacro(const AName: string; const ParamNames: TDynStringArray); virtual; abstract;

    property Options: TPppOptions read GetOptions;
    property DefineTriState[const ASymbol: string]: TTriState read GetDefineTriState write SetDefineTriState;
  end;

  TSimplePppState = class(TPppState)
  private
    FStateStack: IJclStack;
    FOptions: TPppOptions;
    procedure InternalPushState(const ExcludedFiles, SearchPath: IJclStrList;
      const Macros: IJclStrStrMap; const Defines: IJclStrMap);
    function InternalPeekDefines: IJclStrMap;
    function InternalPeekExcludedFiles: IJclStrList;
    function InternalPeekMacros: IJclStrStrMap;
    function InternalPeekSearchPath: IJclStrList;
  protected
    function GetOptions: TPppOptions; override;
    procedure SetOptions(AOptions: TPppOptions);

    function GetDefineTriState(const ASymbol: string): TTriState; override;
    procedure SetDefineTriState(const ASymbol: string; const Value: TTriState); override;
  public
    constructor Create;
    destructor Destroy; override;

    { PushState is called at the start of every unit, and PopState at the
      end. This means that any declarations like $DEFINE will be file-local
      in scope. }
    procedure PushState; override;
    procedure PopState; override;

    function IsDefined(const ASymbol: string): Boolean; override;
    procedure Define(const ASymbol: string); override;
    procedure Undef(const ASymbol: string); override;

    function FindFile(const AName: string): TStream; override;
    procedure AddToSearchPath(const AName: string); override;

    procedure AddFileToExclusionList(const AName: string); override;
    function IsFileExcluded(const AName: string): Boolean; override;

    function ExpandMacro(const AName: string; const ParamValues: TDynStringArray): string; override;
    procedure DefineMacro(const AName: string; const ParamNames: TDynStringArray;
      const Value: string); override;
    procedure UndefMacro(const AName: string; const ParamNames: TDynStringArray); override;

    property Options: TPppOptions read GetOptions write SetOptions;

    { new stuff }
    //property SearchPath: TStringList read FSearchPath;
    //property ExcludedIncludes: TStrings read FExcludedIncludes;
  end;

implementation

uses
  JclAnsiStrings, JclArrayLists, JclHashMaps, JclStacks;

type
  TSimplePppStateItem = class
  public
    DefinedKeywords: IJclStrMap;
    ExcludedFiles: IJclStrList;
    Macros: IJclStrStrMap;
    SearchPath: IJclStrList;
  end;

{ TSimplePppState }

constructor TSimplePppState.Create;
begin
  FStateStack := TJclStack.Create(16, True);
  InternalPushState(TJclStrArrayList.Create(16), TJclStrArrayList.Create(16),
    TJclStrStrHashMap.Create(16), TJclStrHashMap.Create(16, False));
end;

destructor TSimplePppState.Destroy;
begin
  FStateStack := nil;
  inherited Destroy;
end;

procedure TSimplePppState.AddFileToExclusionList(const AName: string);
begin
  InternalPeekExcludedFiles.Add(AName);
end;

procedure TSimplePppState.AddToSearchPath(const AName: string);
begin
  InternalPeekSearchPath.Add(AName);
end;

function TSimplePppState.ExpandMacro(const AName: string;
  const ParamValues: TDynStringArray): string;
var
  AMacros: IJclStrStrMap;
  AMacroNames: IJclStrIterator;
  AMacroName: string;
  Index: Integer;
  Params: array of TVarRec;
begin
  AMacros := InternalPeekMacros;
  AMacroName := Format('%s`%d', [AName, Length(ParamValues)]);
  AMacroNames := AMacros.KeySet.First;
  while AMacroNames.HasNext do
  begin
    if JclAnsiStrings.StrSame(AMacroNames.Next, AMacroName) then
    begin
      SetLength(Params, Length(ParamValues));
      for Index := Low(ParamValues) to High(ParamValues) do
      begin
        Params[Index].VType := vtPChar;
        Params[Index].VPChar := PAnsiChar(ParamValues[Index]);
      end;
      Result := Format(AMacros.Items[AMacroNames.GetString], Params);
      Exit;
    end;
  end;
  raise EPppState.CreateFmt('unknown macro "%s"', [AMacroName]);
end;

procedure TSimplePppState.Define(const ASymbol: string);
begin
  SetDefineTriState(ASymbol, ttDefined);
end;

procedure TSimplePppState.DefineMacro(const AName: string;
  const ParamNames: TDynStringArray; const Value: string);
var
  AMacros: IJclStrStrMap;
  AMacroNames: IJclStrIterator;
  AMacroName, AMacroFormat: string;
  Index: Integer;
begin
  AMacros := InternalPeekMacros;
  AMacroName := Format('%s`%d', [AName, Length(ParamNames)]);
  AMacroNames := AMacros.KeySet.First;
  while AMacroNames.HasNext do
    if JclAnsiStrings.StrSame(AMacroNames.Next, AMacroName) then
      raise EPppState.CreateFmt('macro "%s" is already defined', [AName]);
  AMacroFormat := Value;
  for Index := Low(ParamNames) to High(ParamNames) do
    StrReplace(AMacroFormat, ParamNames[Index], '%' + IntToStr(Index) + ':s', [rfReplaceAll, rfIgnoreCase]);
  AMacros.Items[AMacroName] := AMacroFormat;
end;

function TSimplePppState.FindFile(const AName: string): TStream;
var
  i: Integer;
  fn: string;
  Found: Boolean;
  ASearchPath: IJclStrList;
begin
  ASearchPath := InternalPeekSearchPath;
  fn := AName;
  Found := FileExists(fn);
  if not Found then
    for i := 0 to ASearchPath.Size - 1 do
    begin
      fn := ASearchPath.Items[i] + PathDelim + AName;
      if FileExists(fn) then
      begin
        Found := True;
        Break;
      end;
    end;
  if not Found then
    raise EPppState.CreateFmt('File not found: %s', [AName]);
  Result := TFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
end;

function TSimplePppState.GetOptions: TPppOptions;
begin
  Result := FOptions;
end;

function TSimplePppState.GetDefineTriState(const ASymbol: string): TTriState;
var
  ADefines: IJclStrMap;
  ASymbolNames: IJclStrIterator;
begin
  Result := ttUnknown;
  ADefines := InternalPeekDefines;
  ASymbolNames := ADefines.KeySet.First;
  while ASymbolNames.HasNext do
  begin
    if JclAnsiStrings.StrSame(ASymbolNames.Next, ASymbol) then
    begin
      Result := TTriState(ADefines.Items[ASymbolNames.GetString]);
      Break;
    end;
  end;
end;

function TSimplePppState.InternalPeekDefines: IJclStrMap;
begin
  if FStateStack.Empty then
    raise EPppState.Create('Internal error: PPP State stack is empty');
  Result := (FStateStack.Peek as TSimplePppStateItem).DefinedKeywords;
end;

function TSimplePppState.InternalPeekExcludedFiles: IJclStrList;
begin
  if FStateStack.Empty then
    raise EPppState.Create('Internal error: PPP State stack is empty');
  Result := (FStateStack.Peek as TSimplePppStateItem).ExcludedFiles;
end;

function TSimplePppState.InternalPeekMacros: IJclStrStrMap;
begin
  if FStateStack.Empty then
    raise EPppState.Create('Internal error: PPP State stack is empty');
  Result := (FStateStack.Peek as TSimplePppStateItem).Macros;
end;

function TSimplePppState.InternalPeekSearchPath: IJclStrList;
begin
  if FStateStack.Empty then
    raise EPppState.Create('Internal error: PPP State stack is empty');
  Result := (FStateStack.Peek as TSimplePppStateItem).SearchPath;
end;

procedure TSimplePppState.InternalPushState(const ExcludedFiles,
  SearchPath: IJclStrList; const Macros: IJclStrStrMap; const Defines: IJclStrMap);
var
  AStateItem: TSimplePppStateItem;
begin
  AStateItem := TSimplePppStateItem.Create;
  AStateItem.ExcludedFiles := ExcludedFiles;
  AStateItem.DefinedKeywords := Defines;
  AStateItem.Macros := Macros;
  AStateItem.SearchPath := SearchPath;
  FStateStack.Push(AStateItem);
end;

function TSimplePppState.IsDefined(const ASymbol: string): Boolean;
begin
  Result := DefineTriState[ASymbol] = ttDefined;
end;

function TSimplePppState.IsFileExcluded(const AName: string): Boolean;
var
  AExcludedFiles: IJclStrList;
  AFileNames: IJclStrIterator;
begin
  AExcludedFiles := InternalPeekExcludedFiles;
  AFileNames := AExcludedFiles.First;
  Result := False;
  while AFileNames.HasNext do
  begin
    if JclAnsiStrings.StrSame(AFileNames.Next, AName) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TSimplePppState.PopState;
begin
  if FStateStack.Size <= 1 then
    raise EPppState.Create('Internal error: PPP State stack underflow');
  FStateStack.Pop.Free;
end;

procedure TSimplePppState.PushState;
var
  AExcludedFiles, ASearchPath: IJclStrList;
  ADefines: IJclStrMap;
  AMacros: IJclStrStrMap;
begin
  ADefines := (InternalPeekDefines as IJclIntfCloneable).Clone as IJclStrMap;
  AExcludedFiles := (InternalPeekExcludedFiles as IJclIntfCloneable).Clone as IJclStrList;
  ASearchPath := (InternalPeekSearchPath as IJclIntfCloneable).Clone as IJclStrList;
  AMacros := (InternalPeekMacros as IJclIntfCloneable).Clone as IJclStrStrMap;

  InternalPushState(AExcludedFiles, ASearchPath, AMacros, ADefines);
end;

procedure TSimplePppState.SetOptions(AOptions: TPppOptions);
begin
  FOptions := AOptions;
end;

procedure TSimplePppState.SetDefineTriState(const ASymbol: string;
  const Value: TTriState);
var
  ADefines: IJclStrMap;
  ASymbolNames: IJclStrIterator;
  Found: Boolean;
begin
  Found := False;
  ADefines := InternalPeekDefines;
  ASymbolNames := ADefines.KeySet.First;
  while ASymbolNames.HasNext do
  begin
    if JclAnsiStrings.StrSame(ASymbolNames.Next, ASymbol) then
    begin
      ADefines.Items[ASymbolNames.GetString] := TObject(Value);
      Found := True;
      Break;
    end;
  end;
  if not Found then
    ADefines.Items[ASymbol] := TObject(Value);
end;

procedure TSimplePppState.Undef(const ASymbol: string);
begin
  SetDefineTriState(ASymbol, ttUndef);
end;

procedure TSimplePppState.UndefMacro(const AName: string; const ParamNames: TDynStringArray);
var
  AMacros: IJclStrStrMap;
  AMacroNames: IJclStrIterator;
  AMacroName: string;
begin
  AMacros := InternalPeekMacros;
  AMacroName := Format('%s`%d', [AName, Length(ParamNames)]);
  AMacroNames := AMacros.KeySet.First;
  while AMacroNames.HasNext do
    if JclAnsiStrings.StrSame(AMacroNames.Next, AMacroName) then
      AMacros.Remove(AMacroNames.GetString);
end;

end.
