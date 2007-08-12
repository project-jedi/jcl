{****************************************************************************}
{                                                                            }
{ Project JEDI Code Library (JCL)                                            }
{                                                                            }
{ The contents of this file are subject to the Mozilla Public License        }
{ Version 1.1 (the "License");                                               }
{ you may not use this file except in compliance with the License. You may   }
{ obtain a copy of the License at http://www.mozilla.org/MPL/                }
{                                                                            }
{ Software distributed under the License is distributed on an "AS IS" basis, }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   }
{ for the specific language governing rights and limitations under the       }
{ License.                                                                   }
{                                                                            }
{ The Original Code is JclOtaExcDlgRepository.pas.                           }
{                                                                            }
{ The Initial Developer of the Original Code is Florent Ouchet               }
{         <outchy att users dott sourceforge dott net>                       }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet.    }
{                                                                            }
{ Contributors:                                                              }
{                                                                            }
{****************************************************************************}
{                                                                            }
{ Last modified: $Date: $                                                    }
{                                                                            }
{****************************************************************************}

unit JclOtaExcDlgRepository;

interface

{$I jcl.inc}

uses
  Classes, Forms,
  JclBorlandTools,
  JclOtaTemplates;

type
  TJclOtaExcDlgParams = class(TJclOtaTemplateParams)
  private
    FHookDll: Boolean;
    FFileName: string;
    FCodeDetails: Boolean;
    FModuleName: Boolean;
    FModuleOffset: Boolean;
    FDelayedTrace: Boolean;
    FFormName: string;
    FLogFile: Boolean;
    FLogFileName: string;
    FAddressOffset: Boolean;
    FVirtualAddress: Boolean;
    FActivePersonality: TJclBorPersonality;
    FLanguages: TJclBorPersonalities;
    FRawData: Boolean;
    FSendEMail: Boolean;
    FEMailAddress: string;
    FFormAncestor: string;
    FModalDialog: Boolean;
    FSizeableDialog: Boolean;
    FEMailSubject: string;
    FDesigner: TJclBorDesigner;
    FModuleList: Boolean;
    FOSInfo: Boolean;
    FActiveControls: Boolean;
    FStackList: Boolean;
    FAutoScrollBars: Boolean;
    FMainThreadOnly: Boolean;
    FAllThreads: Boolean;
    FTraceEAbort: Boolean;
    FIgnoredExceptions: TStrings;
    FTraceAllExceptions: Boolean;
    function GetIgnoredExceptionsCount: Integer;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    // file options
    property Languages: TJclBorPersonalities read FLanguages write FLanguages;
    property ActivePersonality: TJclBorPersonality read FActivePersonality
      write FActivePersonality;
    property FileName: string read FFileName write FFileName;
    property FormName: string read FFormName write FFormName;
    property FormAncestor: string read FFormAncestor write FFormAncestor;
    property Designer: TJclBorDesigner read FDesigner write FDesigner;
    // form options
    property ModalDialog: Boolean read FModalDialog write FModalDialog;
    property SendEMail: Boolean read FSendEMail write FSendEMail;
    property EMailAddress: string read FEMailAddress write FEMailAddress;
    property EMailSubject: string read FEMailSubject write FEMailSubject;
    property SizeableDialog: Boolean read FSizeableDialog write FSizeableDialog;
    property AutoScrollBars: Boolean read FAutoScrollBars write FAutoScrollBars;
    // system options
    property DelayedTrace: Boolean read FDelayedTrace write FDelayedTrace;
    property HookDll: Boolean read FHookDll write FHookDll;
    property LogFile: Boolean read FLogFile write FLogFile;
    property LogFileName: string read FLogFileName write FLogFileName;
    property OSInfo: Boolean read FOSInfo write FOSInfo;
    property ModuleList: Boolean read FModuleList write FModuleList;
    property ActiveControls: Boolean read FActiveControls write FActiveControls;
    property MainThreadOnly: Boolean read FMainThreadOnly write FMainThreadOnly;
    // ignored exceptions
    property TraceAllExceptions: Boolean read FTraceAllExceptions
      write FTraceAllExceptions;
    property TraceEAbort: Boolean read FTraceEAbort write FTraceEAbort;
    property IgnoredExceptions: TStrings read FIgnoredExceptions write FIgnoredExceptions;
    property IgnoredExceptionsCount: Integer read GetIgnoredExceptionsCount;
    // trace options
    property StackList: Boolean read FStackList write FStackList;
    property RawData: Boolean read FRawData write FRawData;
    property ModuleName: Boolean read FModuleName write FModuleName;
    property ModuleOffset: Boolean read FModuleOffset write FModuleOffset;
    property AllThreads: Boolean read FAllThreads write FAllThreads;
    //property AddressOffset: Boolean read FAddressOffset write FAddressOffset;
    property CodeDetails: Boolean read FCodeDetails write FCodeDetails;
    property VirtualAddress: Boolean read FVirtualAddress write FVirtualAddress;
  end;

implementation

{$R JclOtaExcDlgIcons.res}

//=== { TJclOtaExcDlgParams } ================================================

constructor TJclOtaExcDlgParams.Create;
begin
  inherited Create;

  FHookDll := True;
  FLanguage := bpUnknown;
  FLanguages := [bpUnknown];
  FFileName := '';
  FCodeDetails := True;
  FModuleName := True;
  FModuleOffset := False;
  FDelayedTrace := True;
  FFormName := 'ExceptionDialog';
  FFormAncestor := TForm.ClassName;
  FLogFile := False;
  FLogFileName := '';
  FAddressOffset := True;
  FVirtualAddress := False;
  FActivePersonality := bpUnknown;
  FRawData := False;
  FSendEMail := False;
  FEMailAddress := '';
  FEMailSubject := '';
  FModalDialog := True;
  FSizeableDialog := False;
  FDesigner := bdVCL;
  FModuleList := True;
  FOSInfo := True;
  FActiveControls := True;
  FStackList := True;
  FAutoScrollBars := True;
  FMainThreadOnly := False;
  FTraceEAbort := False;
  FTraceAllExceptions := False;
  FIgnoredExceptions := TStringList.Create;
end;

destructor TJclOtaExcDlgParams.Destroy;
begin
  FIgnoredExceptions.Free;
  inherited Destroy;
end;

function TJclOtaExcDlgParams.GetIgnoredExceptionsCount: Integer;
begin
  Result := FIgnoredExceptions.Count;
end;

end.
