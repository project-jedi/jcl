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
{ The Original Code is JclCOM.pas.                                                                 }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains Various COM (Component Object Model) utility routines.                        }
{                                                                                                  }
{ Unit owner: Marcel van Brakel                                                                    }
{ Last modified: February 19, 2002                                                                 }
{                                                                                                  }
{**************************************************************************************************}

unit JclCOM;

{$I jcl.inc}
{$DEFINE DEBUGGING}

{$WEAKPACKAGEUNIT ON}

interface

uses ActiveX;


const
  { Class ID's that may be reused }
  CLSID_StdComponentCategoriesMgr: TGUID = '{0002E005-0000-0000-C000-000000000046}';

  CATID_SafeForInitializing: TGuid = '{7DD95802-9882-11CF-9FA9-00AA006C42C4}';
  CATID_SafeForScripting: TGuid = '{7DD95801-9882-11CF-9FA9-00AA006C42C4}';


type
  { For use with the Internet Explorer Component Categories Routines.  May be Reused. }
  TArrayCatID = array[0..0] of TGuid;

  { This provides an easier alternative for types that need to be backward compatible }
  TLargeNumeric = {$IFDEF DELPHI5_UP} Int64 {$ELSE} Comp { Delphi 3 & 4 Support ?? } {$ENDIF};


  { DCOM and MDAC Related Tests and Utility Routines }

function IsDCOMInstalled: Boolean;
function IsDCOMEnabled: Boolean;
function GetDCOMVersion: string;
function GetMDACVersion: string;

  { Stream Related Routines }

function ResetIStreamToStart(Stream: IStream): Boolean;
function SizeOfIStreamContents(Stream: IStream): TLargeNumeric;

  { Other Marshalling Routines to complement "CoMarshalInterThreadInterfaceInStream" }
  { These routines will provide the ability to Marshal an Interface for a Seperate
    Process or even for access by a Seperate Machine.  However, to make things
    familiar to users of the existing CoMarshal... routine, I have kept the required
    Parameters the same, apart from the "stm" type now being a Var rather than just
    an Out - to allow a little flexibility if the developer wants the destination
    to be a specific stream, otherwise it creates one into the passed variable! }

function MarshalInterProcessInterfaceInStream(const iid: TIID;
  unk: IUnknown; var stm: IStream): HResult;
function MarshalInterMachineInterfaceInStream(const iid: TIID;
  unk: IUnknown; var stm: IStream): HResult;

  { Internet Explorer Component Categories Routines }
  { These routines help with the registration of:
      - Safe-Initialization &
      - Safe-for-Scripting
    of ActiveX controls or COM Automation Servers intended to be used in
    HTML pages displayed in Internet Explorer }
  { Conversion of an examples found in Microsoft Development Network document:
    MSDN Home >  MSDN Library >  ActiveX Controls >  Overviews/Tutorials
    Safe Initialization and Scripting for ActiveX Controls }

function CreateComponentCategory(catid: TGuid; sDescription: String): HResult;
function RegisterCLSIDInCategory(clsid: TGuid; catid: TGuid): HResult;
function UnRegisterCLSIDInCategory(clsid: TGuid; catid: TGuid): HResult;


implementation


uses
  Windows, SysUtils, Classes,
  JclFileUtils, JclRegistry, JclSysInfo, JclSysUtils, JclWin32;


{ Implementation Constants - may be reused by more than one routine }
const
  pcOLE32: PChar = 'OLE32.dll';


  { TODO:  Utility routine here might need to be re-vamped with the
           use of JclUnicode unit in mind. }

function StringToWideString(const Str: String): WideString;
var
  iLen: Integer;
begin
  iLen:= (Length(Str) + 1);
  SetLength(Result, (iLen - 1));
  StringToWideChar(Str, PWideChar(Result), iLen);
end;


  { DCOM and MDAC Related Tests and Utility Routines }


function IsDCOMInstalled: Boolean;
var
  OLE32: HModule;
begin
  { DCOM is installed by default on all but Windows 95 }
  Result := not (GetWindowsVersion in [wvUnknown, wvWin95, wvWin95OSR2]);
  if not Result then
  begin
    OLE32 := LoadLibrary(pcOLE32);
    if OLE32 > HINSTANCE_ERROR then
    try
      Result := GetProcAddress(OLE32, PChar('CoCreateInstanceEx')) <> nil;
    finally
      FreeLibrary(OLE32);
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function IsDCOMEnabled: Boolean;
var
  RegValue: string;
begin
  RegValue := RegReadString(HKEY_LOCAL_MACHINE, 'Software\Microsoft\OLE', 'EnableDCOM');
  Result := (RegValue = 'y') or (RegValue = 'Y');
end;

//--------------------------------------------------------------------------------------------------

function GetDCOMVersion: string;
const
  DCOMVersionKey: PChar = 'CLSID\{bdc67890-4fc0-11d0-a805-00aa006d2ea4}\InstalledVersion';
begin
  { NOTE:  This does not work on Windows NT/2000! For a list of DCOM versions:
      http://support.microsoft.com/support/kb/articles/Q235/6/38.ASP }
  Result := '';
  if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) AND IsDCOMEnabled then
    Result := RegReadString(HKEY_CLASSES_ROOT, DCOMVersionKey, '')
  else
    { Possibly from DComExt.dll ‘Product Version’ }
    Result := 'DCOM Version Unknown';
end;

//--------------------------------------------------------------------------------------------------

{ NOTE:  Checking whether MDAC is installed at all can be done by querying the
         Software\Microsoft\DataAccess key for the FullInstallVer or
         Fill32InstallVer values. Windows 2000 always installs MDAC 2.5 }

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


  { Stream Related Routines }


function ResetIStreamToStart(Stream: IStream): Boolean;
var
  i64Pos: TLargeNumeric;
  hrSeek: HResult;
begin
  { TODO:  Test this routine.
    TOTEST:  D4 (CBx ??) }
  { Try to get the current stream position, and reset to start if not already there }
  if Succeeded(Stream.Seek(0, STREAM_SEEK_CUR, i64Pos)) then
  begin
    if i64Pos = 0 then
      hrSeek := S_OK
    else
      hrSeek := Stream.Seek(0, STREAM_SEEK_SET, i64Pos);
      { Another possible option was seen as:
        - Stream.Seek(0, STREAM_SEEK_SET, NULL); }

    Result := (hrSeek = S_OK);
  end else
    Result := False;
end;

function SizeOfIStreamContents(Stream: IStream): TLargeNumeric;
var
  stat: TStatStg;
begin
  { TODO:  Test this routine.
    TOTEST:  D4 (CBx ??) }
  { If we can't determine the size of the Stream, then return -1 for Unattainable }
  if Succeeded(Stream.Stat(stat, STATFLAG_NONAME)) then
    Result := stat.cbSize
  else Result := -1;
end;


  { Other Marshalling Routines to complement "CoMarshalInterThreadInterfaceInStream" }


function MarshalInterProcessInterfaceInStream(const iid: TIID; unk: IUnknown;
  var stm: IStream): HResult;
var
  msData: TMemoryStream;
begin
  { TODO:  Test this routine.
    TOTEST:  D4 (CBx ??) }
  try
    { If passed a variable which doesn't contain a valid stream, create and return }
    if (stm = Nil) then
    begin
      msData := TMemoryStream.Create;

      stm := (TStreamAdapter.Create(msData, soOwned) as IStream);

      { Probably would never get here in such a condition, but just in case }
      if (stm = Nil) then
      begin
        Result := E_OUTOFMEMORY;
        Exit;
      end;
    end;

    if (stm <> Nil) then
      { Same Machine, Different Process}
      Result := CoMarshalInterface(stm, iid, unk, MSHCTX_LOCAL, Nil, MSHLFLAGS_NORMAL)
    else
      { TODO:  Most likely out of memory, though to add to }
      Result := E_POINTER;
  except
    Result := E_UNEXPECTED;
  end;
end;

function MarshalInterMachineInterfaceInStream(const iid: TIID; unk: IUnknown;
  var stm: IStream): HResult;
var
  msData: TMemoryStream;
begin
  { TODO:  Test this routine.  Have no need for it myself at present.
    TOTEST:  D4, D5, D6 (CBx ??) }
  try
    { If passed a variable which doesn't contain a valid stream, create and return }
    if (stm = Nil) then
    begin
      msData := TMemoryStream.Create;

      stm := (TStreamAdapter.Create(msData, soOwned) as IStream);

      { Probably would never get here in such a condition, but just in case }
      if (stm = Nil) then
      begin
        Result := E_OUTOFMEMORY;
        Exit;
      end;
    end;

    if (stm <> Nil) then
      { Different Machine }
      Result := CoMarshalInterface(stm, iid, unk, MSHCTX_DIFFERENTMACHINE, Nil, MSHLFLAGS_NORMAL)
    else
      { TODO:  Most likely out of memory, though to add to }
      Result := E_POINTER;
  except
    Result := E_UNEXPECTED;
  end;
end;


  { Internet Explorer Component Categories Routines }


function CreateComponentCategory(catid: TGuid; sDescription: String): HResult;
var
  CatRegister: ICatRegister;
  hr: HResult;
  CatInfo: TCATEGORYINFO;
  iLen: Integer;
  sTemp: String;
  wsTemp: WideString;
begin
  CatRegister := Nil;

  hr := CoCreateInstance(CLSID_StdComponentCategoriesMgr,
          Nil, CLSCTX_INPROC_SERVER, ICatRegister, CatRegister);

  if Succeeded(hr) then
    try
      (* Make sure the:
           HKCR\Component Categories\{..catid...}
         key is registered *)
      CatInfo.catid := catid;
      CatInfo.lcid := $0409 ; // english

      { Make sure the provided description is not too long.
        Only copy the first 127 characters if it is. }
      iLen := Length(sDescription);
      if (iLen > 128) then
         iLen := 128;

      sTemp := Copy(sDescription, 1, iLen);
      wsTemp := StringToWideString(sTemp);

      Move(Pointer(wsTemp)^, CatInfo.szDescription, (iLen * SizeOf(WideChar)));

      hr := CatRegister.RegisterCategories(1, @catinfo);
    finally
      CatRegister := Nil;
    end;

  { Return the Appropriate Result }
  Result := hr;
end;

function RegisterCLSIDInCategory(clsid: TGuid; catid: TGuid): HResult;
var
  CatRegister: ICatRegister;
  hr: HRESULT;
  arCatID: TArrayCatID;
begin
  { Register your component categories information }
  CatRegister := Nil;
  hr := CoCreateInstance(CLSID_StdComponentCategoriesMgr,
          Nil, CLSCTX_INPROC_SERVER, ICatRegister, CatRegister);

  if Succeeded(hr) then
    try
      { Register this category as being "implemented" by the class }
      arCatID[0] := catid;
      hr := CatRegister.RegisterClassImplCategories(clsid, 1, @arCatID);
    finally
      CatRegister := Nil;
    end;

  { Return the Appropriate Result }
  Result := hr;
end;

function UnRegisterCLSIDInCategory(clsid: TGuid; catid: TGuid): HResult;
var
  CatRegister: ICatRegister;
  hr: HRESULT;
  arCatID: TArrayCatID;
begin
  CatRegister := Nil;

  hr := CoCreateInstance(CLSID_StdComponentCategoriesMgr,
          Nil, CLSCTX_INPROC_SERVER, ICatRegister, CatRegister);

  if Succeeded(hr) then
    try
      { Unregister this category as being "implemented" by the class }
      arCatID[0] := catid;
      hr := CatRegister.UnRegisterClassImplCategories(clsid, 1, @arCatID);
    finally
      CatRegister := Nil;
    end;

  { Return the Appropriate Result }
  Result := hr;
end;

end.
