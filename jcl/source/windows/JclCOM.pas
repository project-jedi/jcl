{******************************************************************************}
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JclCOM.pas.                                             }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ 2000 of these individuals.                                                   }
{                                                                              }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This unit contains Various COM (Component Object Model) utility routines.    }
{                                                                              }
{ Unit owner: Marcel van Brakel                                                }
{ Last modified: January 30, 2000                                              }
{                                                                              }
{******************************************************************************}

unit JclCOM;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

function IsDCOMEnabled: Boolean;
function GetDCOMVersion: string;
function GetMDACVersion: string;

implementation

uses
  Windows, ActiveX,
  SysUtils,
  JclFileUtils, JclRegistry, JclSysUtils, JclWin32;

function IsDCOMEnabled: Boolean;
var
  RegValue: string;
begin
  RegValue := RegReadString(HKEY_LOCAL_MACHINE, 'Software\Microsoft\OLE', 'EnableDCOM');
  Result := (RegValue = 'y') or (RegValue = 'Y');
end;

//------------------------------------------------------------------------------

function GetDCOMVersion: string;
const
  DCOMVersionKey = 'CLSID\{bdc67890-4fc0-11d0-a805-00aa006d2ea4}\InstalledVersion';
begin
  Result := '';
  if IsDCOMEnabled then
    Result := RegReadString(HKEY_CLASSES_ROOT, DCOMVersionKey, '');
end;

//------------------------------------------------------------------------------

function GetMDACVersion: string;
var
  Key: string;
  DLL: string;
  Version: TJclFileVersionInfo;
begin
  Result := '' ;
  Key := RegReadString(HKEY_CLASSES_ROOT, 'ADODB.Connection\CLSID', '');
  DLL := RegReadString(HKEY_CLASSES_ROOT, 'CLSID\' + Key + '\InprocServer32', '');
  if VersionResourceAvailable(DLL) then
  begin
    Version := TJclFileVersionInfo.Create(DLL);
    try
      Result := Version.ProductVersion;
    finally
      FreeAndNil(Version);
    end;
  end;
end;


end.
