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
{ Last modified: April 29, 2001                                                }
{                                                                              }
{******************************************************************************}

unit JclCOM;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

function IsDCOMInstalled: Boolean;
function IsDCOMEnabled: Boolean;
function GetDCOMVersion: string;
function GetMDACVersion: string;

implementation

uses
  Windows, ActiveX,
  SysUtils,
  JclFileUtils, JclRegistry, JclSysInfo, JclSysUtils, JclWin32;

function IsDCOMInstalled: Boolean;
var
  OLE32: HModule;
begin
  // DCOM is installed by default on all but Windows 95
  Result := not (GetWindowsVersion in [wvUnknown, wvWin95, wvWin95OSR2]);
  if not Result then
  begin
    OLE32 := LoadLibrary(PChar('OLE32.dll'));
    if OLE32 > HINSTANCE_ERROR then
    try
      Result := GetProcAddress(OLE32, PChar('CoCreateInstanceEx')) <> nil;
    finally
      FreeLibrary(OLE32);
    end;
  end;
end;

//------------------------------------------------------------------------------

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
  // note: this does not work on Windows NT/2000!
  // for a list of DCOM versions: http://support.microsoft.com/support/kb/articles/Q235/6/38.ASP
  Result := '';
  if IsDCOMEnabled then Result := RegReadString(HKEY_CLASSES_ROOT, DCOMVersionKey, '');
end;

//------------------------------------------------------------------------------

// Note: checking whether MDAC is installed at all can be done by querying the
// Software\Microsoft\DataAccess key for the FullInstallVer or Fill32InstallVer
// values. Windows 2000 always installs MDAC?

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
