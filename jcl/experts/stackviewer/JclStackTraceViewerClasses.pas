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
{ The Original Code is JclStackTraceViewerClasses.pas.                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Uwe Schuster.                                      }
{ Portions created by Uwe Schuster are Copyright (C) 2009 Uwe Schuster. All rights reserved.       }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Uwe Schuster (uschuster)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                              $ }
{ Revision:      $Rev::                                                                      $ }
{ Author:        $Author::                                                                 $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclStackTraceViewerClasses;

{$I jcl.inc}

interface

uses
  Contnrs,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclDebug, JclDebugSerialization;

type
  TJclStackTraceViewerLocationInfo = class(TJclLocationInfoEx)
  private
    FFoundFile: Boolean;
    FFileName: string;
    FProjectName: string;
    FRevision: string;
    FTranslatedLineNumber: Integer;
  public
    property FileName: string read FFileName write FFileName;
    property FoundFile: Boolean read FFoundFile write FFoundFile;
    property ProjectName: string read FProjectName write FProjectName;
    property Revision: string read FRevision write FRevision;
    property TranslatedLineNumber: Integer read FTranslatedLineNumber write FTranslatedLineNumber;
  end;

  TJclStackTraceViewerLocationInfoList = class(TJclCustomLocationInfoList)
  private
    FPrepared: Boolean;
    function GetItems(AIndex: Integer): TJclStackTraceViewerLocationInfo;
  public
    constructor Create; override;
    function Add(Addr: Pointer): TJclStackTraceViewerLocationInfo;
    property Items[AIndex: Integer]: TJclStackTraceViewerLocationInfo read GetItems; default;
    property Prepared: Boolean read FPrepared write FPrepared;
  end;

  TJclStackTraceViewerThreadInfo = class(TJclCustomThreadInfo)
  private
    function GetStack(const AIndex: Integer): TJclStackTraceViewerLocationInfoList;
  protected
    function GetStackClass: TJclCustomLocationInfoListClass; override;
  public
    property CreationStack: TJclStackTraceViewerLocationInfoList index 1 read GetStack;
    property Stack: TJclStackTraceViewerLocationInfoList index 2 read GetStack;
  end;

  TJclStackTraceViewerThreadInfoList = class(TObject)
  private
    FItems: TObjectList;
    function GetItems(AIndex: Integer): TJclStackTraceViewerThreadInfo;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TJclStackTraceViewerThreadInfo;
    procedure Clear;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TJclStackTraceViewerThreadInfo read GetItems; default;
  end;

  TJclStackTraceViewerExceptionInfo = class(TObject)
  private
    FException: TException;
    FThreadInfoList: TJclStackTraceViewerThreadInfoList;
    FModules: TModuleList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AssignExceptionInfo(AExceptionInfo: TExceptionInfo);
    property ThreadInfoList: TJclStackTraceViewerThreadInfoList read FThreadInfoList;
    property Exception: TException read FException;
    property Modules: TModuleList read FModules;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: $';
    Revision: '$Revision: $';
    Date: '$Date: $';
    LogPath: ''
    );
{$ENDIF UNITVERSIONING}

implementation

//=== { TJclStackTraceViewerLocationInfoList } ===============================

function TJclStackTraceViewerLocationInfoList.Add(Addr: Pointer): TJclStackTraceViewerLocationInfo;
begin
  Result := TJclStackTraceViewerLocationInfo(InternalAdd(Addr));
end;

constructor TJclStackTraceViewerLocationInfoList.Create;
begin
  inherited Create;
  FItemClass := TJclStackTraceViewerLocationInfo;
  FOptions := [];
  FPrepared := False;
end;

function TJclStackTraceViewerLocationInfoList.GetItems(AIndex: Integer): TJclStackTraceViewerLocationInfo;
begin
  Result := TJclStackTraceViewerLocationInfo(FItems[AIndex]);
end;

//=== { TJclStackTraceViewerThreadInfo } =====================================

function TJclStackTraceViewerThreadInfo.GetStack(const AIndex: Integer): TJclStackTraceViewerLocationInfoList;
begin
  case AIndex of
    1: Result := TJclStackTraceViewerLocationInfoList(FCreationStack);
    2: Result := TJclStackTraceViewerLocationInfoList(FStack);
    else
      Result := nil;
  end;
end;

function TJclStackTraceViewerThreadInfo.GetStackClass: TJclCustomLocationInfoListClass;
begin
  Result := TJclStackTraceViewerLocationInfoList;
end;

//=== { TJclStackTraceViewerThreadInfoList } =================================

constructor TJclStackTraceViewerThreadInfoList.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TJclStackTraceViewerThreadInfoList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TJclStackTraceViewerThreadInfoList.Add: TJclStackTraceViewerThreadInfo;
begin
  FItems.Add(TJclStackTraceViewerThreadInfo.Create);
  Result := TJclStackTraceViewerThreadInfo(FItems.Last);
end;

procedure TJclStackTraceViewerThreadInfoList.Clear;
begin
  FItems.Clear;
end;

function TJclStackTraceViewerThreadInfoList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJclStackTraceViewerThreadInfoList.GetItems(AIndex: Integer): TJclStackTraceViewerThreadInfo;
begin
  Result := TJclStackTraceViewerThreadInfo(FItems[AIndex]);
end;

//=== { TJclStackTraceViewerExceptionInfo } ==================================

constructor TJclStackTraceViewerExceptionInfo.Create;
begin
  inherited Create;
  FException := TException.Create;
  FThreadInfoList := TJclStackTraceViewerThreadInfoList.Create;
  FModules := TModuleList.Create;
end;

destructor TJclStackTraceViewerExceptionInfo.Destroy;
begin
  FModules.Free;
  FException.Free;
  FThreadInfoList.Free;
  inherited Destroy;
end;

procedure TJclStackTraceViewerExceptionInfo.AssignExceptionInfo(AExceptionInfo: TExceptionInfo);
var
  I: Integer;
begin
  FException.Assign(AExceptionInfo.Exception);
  FThreadInfoList.Clear;
  for I := 0 to AExceptionInfo.ThreadInfoList.Count - 1 do
    FThreadInfoList.Add.Assign(AExceptionInfo.ThreadInfoList[I]);
  FModules.Assign(AExceptionInfo.Modules);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
