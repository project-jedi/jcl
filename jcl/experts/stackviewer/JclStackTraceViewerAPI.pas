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
{ The Original Code is JclStackTraceViewerAPI.pas.                                                 }
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

unit JclStackTraceViewerAPI;

interface

uses
  Classes, ActiveX;

type
  IJclLineNumberTranslator = interface
  ['{01E06940-49AE-464B-AC47-D65DFBC41396}']
    function GetIDString: string;
    function GetName: string;
    function TranslateLineNumbers(ARevisionContent, ACurrentContent: IStream;
      ARevisionLineNumbers: TList; ACurrentLineNumbers: TList): Integer;

    property Name: string read GetName;
    property IDString: string read GetIDString;
  end;

  IJclRevisionProvider = interface
  ['{8127FF3C-083D-47FD-855D-6C68EC7CBFB9}']
    function GetIDString: string;
    function GetName: string;
    function GetRevisionContent(const AFileName, ARevision: string; AContent: IStream): Boolean;

    property Name: string read GetName;
    property IDString: string read GetIDString;
  end;

var
  RegisterLineNumberTranslatorProc: function(const ATranslator: IJclLineNumberTranslator): Integer = nil;
  UnregisterLineNumberTranslatorProc: procedure(AIndex: Integer) = nil;
  RegisterRevisionProviderProc: function(const ATranslator: IJclRevisionProvider): Integer = nil;
  UnregisterRevisionProviderProc: procedure(AIndex: Integer) = nil;

function RegisterLineNumberTranslator(const ATranslator: IJclLineNumberTranslator): Integer;
procedure UnregisterLineNumberTranslator(AIndex: Integer);

function RegisterRevisionProvider(const ATranslator: IJclRevisionProvider): Integer;
procedure UnregisterRevisionProvider(AIndex: Integer);

implementation

function RegisterLineNumberTranslator(const ATranslator: IJclLineNumberTranslator): Integer;
begin
  Result := RegisterLineNumberTranslatorProc(ATranslator);
end;

procedure UnregisterLineNumberTranslator(AIndex: Integer);
begin
  UnregisterLineNumberTranslatorProc(AIndex);
end;

function RegisterRevisionProvider(const ATranslator: IJclRevisionProvider): Integer;
begin
  Result := RegisterRevisionProviderProc(ATranslator);
end;

procedure UnregisterRevisionProvider(AIndex: Integer);
begin
  UnregisterRevisionProviderProc(AIndex);
end;

end.
