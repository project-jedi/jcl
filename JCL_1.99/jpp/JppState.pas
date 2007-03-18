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
  SysUtils, Classes, JclStrHashMap;

type
  EPppState = class(Exception);

  TPppOption = (poProcessIncludes, poProcessDefines, poStripComments);
  TPppOptions = set of TPppOption;

  TTriState = (ttUnknown, ttUndef, ttDefined);

  TPppState = class
  protected
    function GetOptions: TPppOptions; virtual; abstract;
    function GetTriState(const ASymbol: string): TTriState; virtual; abstract;
    procedure SetTriState(const ASymbol: string; const Value: TTriState); virtual; abstract;
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

    property Options: TPppOptions read GetOptions;
    property TriState[const ASymbol: string]: TTriState read GetTriState write SetTriState;
  end;

  TSimplePppState = class(TPppState)
  private
    FSearchPath: TStringList;
    FDefineHashStack: TList;
    FOptions: TPppOptions;
    FHashSize: Cardinal;
    function StackTop: TStringHashMap;
  protected
    function GetOptions: TPppOptions; override;
    procedure SetOptions(AOptions: TPppOptions);
  public
    constructor Create(AHashSize: Cardinal = 1033);
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

    property Options: TPppOptions read GetOptions write SetOptions;

    { new stuff }
    property SearchPath: TStringList read FSearchPath;
  end;

  TJppState = class(TSimplePppState)
  private
    FExcludedIncludes: TStrings;
  protected
    function GetTriState(const ASymbol: string): TTriState; override;
    procedure SetTriState(const ASymbol: string; const Value: TTriState); override;
  public
    constructor Create;
    destructor Destroy; override;
    property ExcludedIncludes: TStrings read FExcludedIncludes;
  end;

implementation

{ TSimplePppState }

const
// D5 compatibility
{$IFDEF LINUX}
  PathDelim = '/';
{$ELSE}
  PathDelim = '\';
{$ENDIF}

{ TSimplePppState }

constructor TSimplePppState.Create(AHashSize: Cardinal);
begin
  FHashSize := AHashSize;
  FSearchPath := TStringList.Create;
  FDefineHashStack := TList.Create;
  FDefineHashStack.Add(TStringHashMap.Create(CaseInsensitiveTraits, AHashSize));
end;

destructor TSimplePppState.Destroy;
var
  i: Integer;
begin
  if FDefineHashStack <> nil then
    for i := 0 to FDefineHashStack.Count - 1 do
      TObject(FDefineHashStack[i]).Free;
  FDefineHashStack.Free;
  FSearchPath.Free;
  inherited;
end;

function TSimplePppState.FindFile(const AName: string): TStream;
var
  i: Integer;
  fn: string;
  Found: Boolean;
begin
  fn := AName;
  Found := FileExists(fn);
  if not Found then
    for i := 0 to FSearchPath.Count - 1 do
    begin
      fn := FSearchPath[i] + PathDelim + AName;
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

procedure TSimplePppState.SetOptions(AOptions: TPppOptions);
begin
  FOptions := AOptions;
end;

procedure TSimplePppState.PushState;
var
  hash: TStringHashMap;
begin
  hash := TStringHashMap.Create(CaseInsensitiveTraits, FHashSize);
  try
    FDefineHashStack.Add(hash);
  except
    hash.Free;
    raise;
  end;
end;

procedure TSimplePppState.PopState;
begin
  if FDefineHashStack.Count <= 1 then
    raise EPppState.Create('Internal error: PPP State stack underflow');
  StackTop.Free;
  FDefineHashStack.Delete(FDefineHashStack.Count - 1);
end;

function TSimplePppState.IsDefined(const ASymbol: string): Boolean;
begin
  Result := TriState[ASymbol] = ttDefined;
end;

procedure TSimplePppState.Define(const ASymbol: string);
begin
  SetTriState(ASymbol, ttDefined);
end;

procedure TSimplePppState.Undef(const ASymbol: string);
begin
  SetTriState(ASymbol, ttUndef);
end;

function TSimplePppState.StackTop: TStringHashMap;
begin
  Assert(FDefineHashStack.Count > 0, 'Define stack underflow in TSimplePppState');
  Result := TStringHashMap(FDefineHashStack[FDefineHashStack.Count - 1]);
end;

{ TJppState }

constructor TJppState.Create;
begin
  inherited Create;
  FExcludedIncludes := TStringList.Create;
end;

destructor TJppState.Destroy;
begin
  FExcludedIncludes.Free;
  inherited Destroy;
end;

function TJppState.GetTriState(const ASymbol: string): TTriState;
var
  i, value: Integer;
begin
  { Because of stack semantics, this function is slightly complex.
    We keep seaching from the top down, until we find the symbol;
    then we check its value, to see if it is a local override of
    a globally defined symbol. }
  Result := ttUnknown;
  for i := FDefineHashStack.Count - 1 downto 0 do
    if TStringHashMap(FDefineHashStack[i]).Find(ASymbol, value) then
    begin
      Result := TTriState(value);
      Break;
    end;
end;

procedure TJppState.SetTriState(const ASymbol: string;
  const Value: TTriState);
begin
  StackTop[ASymbol] := Pointer(Value);
end;

end.
