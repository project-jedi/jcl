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
{ The Original Code is JclDotNet.pas.                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Flier Lu (<flier_lu att yahoo dott com dott cn>).  }
{ Portions created by Flier Lu are Copyright (C) Flier Lu. All Rights Reserved.                    }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Flier Lu (flier)                                                                               }
{   Robert Marquardt (marquardt)                                                                   }
{   Olivier Sannier (obones)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Microsoft .Net framework support routines and classes.                                           }
{                                                                                                  }
{ Unit owner: Flier Lu                                                                             }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclDotNet;

{**************************************************************************************************}
{ Read this before compile!                                                                         }
{**************************************************************************************************}
{ 1. This unit is developed in Delphi6 with MS.Net v1.0.3705,                                      }
{    you maybe need to modify it for your environment.                                             }
{ 2. Delphi's TLibImp.exe would generate error *_TLB.pas files                                     }
{    when you import mscorlib.tlb, you should modify it by hand                                    }
{    for example, change Pointer to _Pointer...                                                    }
{    or use my modified edition of mscorlib_TLB.pas (mscor.zip)                                    }
{**************************************************************************************************}

interface

{$I jcl.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows, ActiveX,
  {$ENDIF MSWINDOWS}
  Classes, SysUtils,
  {$IFDEF RTL130_UP}
  Contnrs,
  {$ENDIF RTL130_UP}
  JclBase,
  mscoree_TLB, mscorlib_TLB;

{ TODO -cDOC : Original code: "Flier Lu" <flier_lu att yahoo dott com dott cn> }

type
  TJclClrBase = TInterfacedObject;

type
  IJclClrAppDomain = mscorlib_TLB._AppDomain;
  IJclClrEvidence  = mscorlib_TLB._Evidence;
  IJclClrAssembly  = mscorlib_TLB._Assembly;
  IJclClrMethod    = mscorlib_TLB._MethodInfo;

type
  TJclClrHostFlavor = (hfServer, hfWorkStation);

  TJclClrHostLoaderFlag =
   (hlOptSingleDomain,
    hlOptMultiDomain,
    hlOptMultiDomainHost,
    hlSafeMode,
    hlSetPreference);
  TJclClrHostLoaderFlags = set of TJclClrHostLoaderFlag;

type
  TJclClrAppDomain = class;
  TJclClrAppDomainSetup = class;
  TJclClrAssembly = class;

  TJclClrHost = class(TJclClrBase, ICorRuntimeHost)
  private
    FDefaultInterface: ICorRuntimeHost;
    FAppDomains: TObjectList;
    procedure EnumAppDomains;
    function GetAppDomain(const Idx: Integer): TJclClrAppDomain;
    function GetAppDomainCount: Integer;
    function GetDefaultAppDomain: IJclClrAppDomain;
    function GetCurrentAppDomain: IJclClrAppDomain;
  protected
    function AddAppDomain(const AppDomain: TJclClrAppDomain): Integer;
    function RemoveAppDomain(const AppDomain: TJclClrAppDomain): Integer; 
  public
    constructor Create(const ClrVer: WideString = '';
      const Flavor: TJclClrHostFlavor = hfWorkStation;
      const ConcurrentGC: Boolean = True;
      const LoaderFlags: TJclClrHostLoaderFlags = [hlOptSingleDomain]);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure Refresh;
    function CreateDomainSetup: TJclClrAppDomainSetup;
    function CreateAppDomain(const Name: WideString;
      const Setup: TJclClrAppDomainSetup = nil;
      const Evidence: IJclClrEvidence = nil): TJclClrAppDomain;
    function FindAppDomain(const Intf: IJclClrAppDomain; var Ret: TJclClrAppDomain): Boolean; overload;
    function FindAppDomain(const Name: WideString; var Ret: TJclClrAppDomain): Boolean; overload;
    class function CorSystemDirectory: WideString;
    class function CorVersion: WideString;
    class function CorRequiredVersion: WideString;
    property DefaultInterface: ICorRuntimeHost read FDefaultInterface implements ICorRuntimeHost;
    property AppDomains[const Idx: Integer]: TJclClrAppDomain read GetAppDomain; default;
    property AppDomainCount: Integer read GetAppDomainCount;
    property DefaultAppDomain: IJclClrAppDomain read GetDefaultAppDomain;
    property CurrentAppDomain: IJclClrAppDomain read GetCurrentAppDomain;
  end;

  TJclClrAssemblyArguments = array of WideString;

  TJclClrAppDomain = class(TJclClrBase, IJclClrAppDomain)
  private
    FHost: TJclClrHost;
    FDefaultInterface: IJclClrAppDomain;
  protected
    constructor Create(const AHost: TJclClrHost; const spAppDomain: IJclClrAppDomain);
  public
    function Load(const AssemblyString: WideString;
      const AssemblySecurity: IJclClrEvidence = nil): TJclClrAssembly; overload;
    function Load(const RawAssemblyStream: TStream;
      const RawSymbolStoreStream: TStream = nil;
      const AssemblySecurity: IJclClrEvidence = nil): TJclClrAssembly; overload;
    function Execute(const AssemblyFile: TFileName;
      const AssemblySecurity: IJclClrEvidence = nil): Integer; overload;
    function Execute(const AssemblyFile: TFileName;
      const Arguments: TJclClrAssemblyArguments;
      const AssemblySecurity: IJclClrEvidence = nil): Integer; overload;
    function Execute(const AssemblyFile: TFileName;
      const Arguments: TStrings;
      const AssemblySecurity: IJclClrEvidence = nil): Integer; overload;
    procedure Unload;
    property Host: TJclClrHost read FHost;
    property DefaultInterface: IJclClrAppDomain read FDefaultInterface implements IJclClrAppDomain;
  end;

  TJclClrAppDomainSetup = class(TJclClrBase, IAppDomainSetup)
  private
    FDefaultInterface: IAppDomainSetup;
    function GetApplicationBase: WideString;
    function GetApplicationName: WideString;
    function GetCachePath: WideString;
    function GetConfigurationFile: WideString;
    function GetDynamicBase: WideString;
    function GetLicenseFile: WideString;
    function GetPrivateBinPath: WideString;
    function GetPrivateBinPathProbe: WideString;
    function GetShadowCopyDirectories: WideString;
    function GetShadowCopyFiles: WideString;
    procedure SetApplicationBase(const Value: WideString);
    procedure SetApplicationName(const Value: WideString);
    procedure SetCachePath(const Value: WideString);
    procedure SetConfigurationFile(const Value: WideString);
    procedure SetDynamicBase(const Value: WideString);
    procedure SetLicenseFile(const Value: WideString);
    procedure SetPrivateBinPath(const Value: WideString);
    procedure SetPrivateBinPathProbe(const Value: WideString);
    procedure SetShadowCopyDirectories(const Value: WideString);
    procedure SetShadowCopyFiles(const Value: WideString);
  protected
    constructor Create(Intf: IAppDomainSetup);
  public
    property DefaultInterface: IAppDomainSetup read FDefaultInterface implements IAppDomainSetup;
    property ApplicationBase: WideString read GetApplicationBase write SetApplicationBase;
    property ApplicationName: WideString read GetApplicationName write SetApplicationName;
    property CachePath: WideString read GetCachePath write SetCachePath;
    property ConfigurationFile: WideString read GetConfigurationFile write SetConfigurationFile;
    property DynamicBase: WideString read GetDynamicBase write SetDynamicBase;
    property LicenseFile: WideString read GetLicenseFile write SetLicenseFile;
    property PrivateBinPath: WideString read GetPrivateBinPath write SetPrivateBinPath;
    property PrivateBinPathProbe: WideString read GetPrivateBinPathProbe write SetPrivateBinPathProbe;
    property ShadowCopyDirectories: WideString read GetShadowCopyDirectories write SetShadowCopyDirectories;
    property ShadowCopyFiles: WideString read GetShadowCopyFiles write SetShadowCopyFiles;
  end;

  TJclClrAssembly = class(TJclClrBase, IJclClrAssembly)
  private
    FDefaultInterface: IJclClrAssembly;
  protected
    constructor Create(Intf: IJclClrAssembly);
  public
    property DefaultInterface: IJclClrAssembly read FDefaultInterface implements IJclClrAssembly;
  end;

type
  TJclClrField = class(TObject)
  end;

  TJclClrProperty = class(TObject)
  end;

  TJclClrMethod = class(TJclClrBase, IJclClrMethod)
  private
    FDefaultInterface: IJclClrMethod;
  public
    property DefaultInterface: IJclClrMethod read FDefaultInterface implements IJclClrMethod;
  end;

  TJclClrObject = class(TObject)
  private
    function GetMethod(const Name: WideString): TJclClrMethod;
    function GetField(const Name: WideString): TJclClrField;
    function GetProperty(const Name: WideString): TJclClrProperty;
  protected
    constructor Create(const AssemblyName, NamespaceName, ClassName: WideString;
      const Parameters: array of const); overload;
    constructor Create(const AssemblyName, NamespaceName, ClassName: WideString;
      const NewInstance: Boolean = False); overload;
  public
    property Fields[const Name: WideString]: TJclClrField read GetField;
    property Properties[const Name: WideString]: TJclClrProperty read GetProperty;
    property Methods[const Name: WideString]: TJclClrMethod read GetMethod;
  end;

type
  HDOMAINENUM = Pointer;

function GetCORSystemDirectory(pbuffer: PWideChar; const cchBuffer: DWORD;
  var dwLength: DWORD): HRESULT; stdcall;
function GetCORVersion(pbuffer: PWideChar; const cchBuffer: DWORD;
  var dwLength: DWORD): HRESULT; stdcall;
function GetCORRequiredVersion(pbuffer: PWideChar; const cchBuffer: DWORD;
  var dwLength: DWORD): HRESULT; stdcall;
function CorBindToRuntimeHost(pwszVersion, pwszBuildFlavor, pwszHostConfigFile: PWideChar;
  const pReserved: Pointer; const startupFlags: DWORD;
  const rclsid: TCLSID; const riid: TIID; out pv): HRESULT; stdcall;
function CorBindToRuntimeEx(pwszVersion, pwszBuildFlavor: PWideChar; startupFlags: DWORD;
  const rclsid: TCLSID; const riid: TIID; out pv): HRESULT; stdcall;
function CorBindToRuntimeByCfg(const pCfgStream: IStream; const reserved, startupFlags: DWORD;
  const rclsid: TCLSID; const riid: TIID; out pv): HRESULT; stdcall;
function CorBindToRuntime(pwszVersion, pwszBuildFlavor: PWideChar;
  const rclsid: TCLSID; const riid: TIID; out pv): HRESULT; stdcall;
function CorBindToCurrentRuntime(pwszFileName: PWideChar;
  const rclsid: TCLSID; const riid: TIID; out pv): HRESULT; stdcall;
function ClrCreateManagedInstance(pTypeName: PWideChar;
  const riid: TIID; out pv): HRESULT; stdcall;
procedure CorMarkThreadInThreadPool; stdcall;
function RunDll32ShimW(const hwnd: HWND; const hinst: HMODULE;
  lpszCmdLine: PWideChar; const nCmdShow: Integer): HRESULT; stdcall;
function LoadLibraryShim(szDllName, szVersion: PWideChar;
  const pvReserved: Pointer; out phModDll: HMODULE): HRESULT; stdcall;
function CallFunctionShim(szDllName: PWideChar; const szFunctionName: PChar;
  const lpvArgument1, lpvArgument2: Pointer; szVersion: PWideChar;
  const pvReserved: Pointer): HRESULT; stdcall;
function GetRealProcAddress(const pwszProcName: PChar; out ppv: Pointer): HRESULT; stdcall;
procedure CorExitProcess(const exitCode: Integer); stdcall;

implementation

uses
  ComObj, Variants, Provider,
  JclSysUtils;

const
  mscoree_dll = 'mscoree.dll';

function GetCORSystemDirectory; external mscoree_dll;
function GetCORVersion; external mscoree_dll;
function GetCORRequiredVersion; external mscoree_dll;
function CorBindToRuntimeHost; external mscoree_dll;
function CorBindToRuntimeEx; external mscoree_dll;
function CorBindToRuntimeByCfg; external mscoree_dll;
function CorBindToRuntime; external mscoree_dll;
function CorBindToCurrentRuntime; external mscoree_dll;
function ClrCreateManagedInstance; external mscoree_dll;
procedure CorMarkThreadInThreadPool; external mscoree_dll;
function RunDll32ShimW; external mscoree_dll;
function LoadLibraryShim; external mscoree_dll;
function CallFunctionShim; external mscoree_dll;
function GetRealProcAddress; external mscoree_dll;
procedure CorExitProcess; external mscoree_dll;

//=== { TJclClrHost } ========================================================

const
  CLR_MAJOR_VERSION = 1;
  CLR_MINOR_VERSION = 0;
  CLR_BUILD_VERSION = 3705;

  STARTUP_CONCURRENT_GC                         = $1;
  STARTUP_LOADER_OPTIMIZATION_MASK              = $3 shl 1;
  STARTUP_LOADER_OPTIMIZATION_SINGLE_DOMAIN     = $1 shl 1;
  STARTUP_LOADER_OPTIMIZATION_MULTI_DOMAIN      = $2 shl 1;
  STARTUP_LOADER_OPTIMIZATION_MULTI_DOMAIN_HOST = $3 shl 1;
  STARTUP_LOADER_SAFEMODE                       = $10;
  STARTUP_LOADER_SETPREFERENCE                  = $100;

constructor TJclClrHost.Create(const ClrVer: WideString; const Flavor: TJclClrHostFlavor;
  const ConcurrentGC: Boolean; const LoaderFlags: TJclClrHostLoaderFlags);
const
  ClrHostFlavorNames: array [TJclClrHostFlavor] of WideString = ('srv', 'wks');
  ClrHostLoaderFlagValues: array [TJclClrHostLoaderFlag] of DWORD =
   (STARTUP_LOADER_OPTIMIZATION_SINGLE_DOMAIN,
    STARTUP_LOADER_OPTIMIZATION_MULTI_DOMAIN,
    STARTUP_LOADER_OPTIMIZATION_MULTI_DOMAIN_HOST,
    STARTUP_LOADER_SAFEMODE,
    STARTUP_LOADER_SETPREFERENCE);
var
  Flags: DWORD;
  ALoaderFlag: TJclClrHostLoaderFlag;
begin
  inherited Create;
  Flags := 0;
  if ConcurrentGC then
    Flags := Flags or STARTUP_CONCURRENT_GC;
  for ALoaderFlag := Low(TJclClrHostLoaderFlag) to High(TJclClrHostLoaderFlag) do
    if ALoaderFlag in LoaderFlags then
      Flags := Flags or ClrHostLoaderFlagValues[ALoaderFlag];
  OleCheck(CorBindToRuntimeEx(PWideCharOrNil(ClrVer),
    PWideChar(ClrHostFlavorNames[Flavor]), Flags,
    CLASS_CorRuntimeHost, IID_ICorRuntimeHost, FDefaultInterface));
end;

destructor TJclClrHost.Destroy;
begin
  FreeAndNil(FAppDomains);
  inherited Destroy;
end;

procedure TJclClrHost.EnumAppDomains;
var
  hEnum: Pointer;
  spUnk: IUnknown;
begin
  if Assigned(FAppDomains) then
    FAppDomains.Clear
  else
    FAppDomains := TObjectList.Create;

  OleCheck(FDefaultInterface.EnumDomains(hEnum));
  try
    while FDefaultInterface.NextDomain(hEnum, spUnk) <> S_FALSE do
      TJclClrAppDomain.Create(Self, spUnk as IJclClrAppDomain);
  finally
    OleCheck(FDefaultInterface.CloseEnum(hEnum));
  end;
end;

function TJclClrHost.FindAppDomain(const Intf: IJclClrAppDomain;
  var Ret: TJclClrAppDomain): Boolean;
var
  I: Integer;
begin
  for I := 0 to AppDomainCount-1 do
  begin
    Ret := AppDomains[I];
    if Ret.DefaultInterface = Intf then
    begin
      Result := True;
      Exit;
    end;
  end;
  Ret := nil;
  Result := False;
end;

function TJclClrHost.FindAppDomain(const Name: WideString;
  var Ret: TJclClrAppDomain): Boolean;
var
  I: Integer;
begin
  for I := 0 to AppDomainCount-1 do
  begin
    Ret := AppDomains[I];
    if Ret.DefaultInterface.FriendlyName = Name then
    begin
      Result := True;
      Exit;
    end;
  end;
  Ret := nil;
  Result := False;
end;

function TJclClrHost.GetAppDomain(const Idx: Integer): TJclClrAppDomain;
begin
  Result := TJclClrAppDomain(FAppDomains.Items[Idx]);
end;

function TJclClrHost.GetAppDomainCount: Integer;
begin
  Result := FAppDomains.Count;
end;

function TJclClrHost.GetDefaultAppDomain: IJclClrAppDomain;
var
  spUnk: IUnknown;
begin
  OleCheck(FDefaultInterface.GetDefaultDomain(spUnk));
  Result := spUnk as IJclClrAppDomain;
end;

function TJclClrHost.GetCurrentAppDomain: IJclClrAppDomain;
var
  spUnk: IUnknown;
begin
  OleCheck(FDefaultInterface.CurrentDomain(spUnk));
  Result := spUnk as IJclClrAppDomain;
end;

function TJclClrHost.AddAppDomain(const AppDomain: TJclClrAppDomain): Integer;
begin
  Result := FAppDomains.Add(AppDomain);
end;

function TJclClrHost.RemoveAppDomain(const AppDomain: TJclClrAppDomain): Integer;
begin
  Result := FAppDomains.Remove(AppDomain);
end;

class function TJclClrHost.CorSystemDirectory: WideString;
var
  Len: DWORD;
begin
  SetLength(Result, MAX_PATH);
  OleCheck(GetCORSystemDirectory(PWideChar(Result), Length(Result), Len));
  SetLength(Result, Len);
end;

class function TJclClrHost.CorVersion: WideString;
var
  Len: DWORD;
begin
  SetLength(Result, 64);
  OleCheck(GetCORVersion(PWideChar(Result), Length(Result), Len));
  SetLength(Result, Len);
end;

class function TJclClrHost.CorRequiredVersion: WideString;
var
  Len: DWORD;
begin
  SetLength(Result, 64);
  OleCheck(GetCORRequiredVersion(PWideChar(Result), Length(Result), Len));
  SetLength(Result, Len);
end;

function TJclClrHost.CreateDomainSetup: TJclClrAppDomainSetup;
var
  pUnk: IUnknown;
begin
  OleCheck(FDefaultInterface.CreateDomainSetup(pUnk));
  Result := TJclClrAppDomainSetup.Create(pUnk as IAppDomainSetup);
end;

function TJclClrHost.CreateAppDomain(const Name: WideString;
  const Setup: TJclClrAppDomainSetup;
  const Evidence: IJclClrEvidence): TJclClrAppDomain;
var
  pUnk: IUnknown;
begin
  OleCheck(FDefaultInterface.CreateDomainEx(PWideChar(Name), Setup as IAppDomainSetup, Evidence, pUnk));
  Result := TJclClrAppDomain.Create(Self, pUnk as IJclClrAppDomain);
end;

procedure TJclClrHost.Start;
begin
  OleCheck(DefaultInterface.Start);
  Refresh;
end;

procedure TJclClrHost.Stop;
begin
  OleCheck(DefaultInterface.Stop);
end;

procedure TJclClrHost.Refresh;
begin
  EnumAppDomains;
end;

//=== { TJclClrAppDomain } ===================================================

constructor TJclClrAppDomain.Create(const AHost: TJclClrHost;
  const spAppDomain: IJclClrAppDomain);
begin
  Assert(Assigned(AHost));
  Assert(Assigned(spAppDomain));
  inherited Create;
  FHost := AHost;
  FDefaultInterface := spAppDomain;
  FHost.AddAppDomain(Self);
end;

function TJclClrAppDomain.Execute(const AssemblyFile: TFileName;
  const Arguments: TJclClrAssemblyArguments;
  const AssemblySecurity: IJclClrEvidence): Integer;
var
  Args: Variant;
begin
  Assert(FileExists(AssemblyFile));
  if Length(Arguments) = 0 then
    Result := Execute(AssemblyFile, AssemblySecurity)
  else
  begin
    DynArrayToVariant(Args, @Arguments[0], TypeInfo(TJclClrAssemblyArguments));
    Result := DefaultInterface.ExecuteAssembly_3(AssemblyFile, AssemblySecurity, PSafeArray(TVarData(Args).VArray));
  end;
end;

function TJclClrAppDomain.Execute(const AssemblyFile: TFileName;
  const AssemblySecurity: IJclClrEvidence): Integer;
begin
  Assert(FileExists(AssemblyFile));
  if Assigned(AssemblySecurity) then
    Result := DefaultInterface.ExecuteAssembly(AssemblyFile, AssemblySecurity)
  else
    Result := DefaultInterface.ExecuteAssembly_2(AssemblyFile);
end;

function TJclClrAppDomain.Execute(const AssemblyFile: TFileName;
  const Arguments: TStrings; const AssemblySecurity: IJclClrEvidence): Integer;
var
  Args: Variant;
begin
  Assert(FileExists(AssemblyFile));
  if Arguments.Count = 0 then
    Result := Execute(AssemblyFile, AssemblySecurity)
  else
  begin
    Args := VarArrayFromStrings(Arguments);
    Result := DefaultInterface.ExecuteAssembly_3(AssemblyFile, AssemblySecurity, PSafeArray(TVarData(Args).VArray));
  end;
end;

function TJclClrAppDomain.Load(const AssemblyString: WideString;
  const AssemblySecurity: IJclClrEvidence): TJclClrAssembly;
begin
  if Assigned(AssemblySecurity) then
    Result := TJclClrAssembly.Create(DefaultInterface.Load_7(AssemblyString, AssemblySecurity))
  else
    Result := TJclClrAssembly.Create(DefaultInterface.Load_2(AssemblyString));
end;

function TJclClrAppDomain.Load(const RawAssemblyStream,
  RawSymbolStoreStream: TStream;
  const AssemblySecurity: IJclClrEvidence): TJclClrAssembly;
var
  RawAssembly, RawSymbolStore: Variant;
begin
  Assert(Assigned(RawAssemblyStream));
  RawAssembly := VarArrayCreate([0, RawAssemblyStream.Size-1], varByte);
  try
    try
      RawAssemblyStream.Read(VarArrayLock(RawAssembly)^, RawAssemblyStream.Size);
    finally
      VarArrayUnlock(RawAssembly);
    end;

    if not Assigned(RawSymbolStoreStream) then
      Result := TJclClrAssembly.Create(DefaultInterface.Load_3(PSafeArray(TVarData(RawAssembly).VArray)))
    else
    begin
      RawSymbolStore := VarArrayCreate([0, RawSymbolStoreStream.Size-1], varByte);
      try
        try
          RawSymbolStoreStream.Read(VarArrayLock(RawSymbolStore)^, RawSymbolStoreStream.Size);
        finally
          VarArrayUnlock(RawSymbolStore);
        end;

        if Assigned(AssemblySecurity) then
          Result := TJclClrAssembly.Create(DefaultInterface.Load_5(
            PSafeArray(TVarData(RawAssembly).VArray),
            PSafeArray(TVarData(RawSymbolStore).VArray),
            AssemblySecurity))
        else
          Result := TJclClrAssembly.Create(DefaultInterface.Load_4(
            PSafeArray(TVarData(RawAssembly).VArray),
            PSafeArray(TVarData(RawSymbolStore).VArray)));
      finally
        VarClear(RawSymbolStore);
      end;
    end;
  finally
    VarClear(RawAssembly);
  end;
end;

procedure TJclClrAppDomain.Unload;
var
  AppDomain: TJclClrAppDomain;
begin
  OleCheck(FHost.DefaultInterface.UnloadDomain(DefaultInterface));
  if FHost.FindAppDomain(DefaultInterface, AppDomain) and (AppDomain = Self) then
    FHost.RemoveAppDomain(Self);
end;

//=== { TJclClrObject } ======================================================

constructor TJclClrObject.Create(const AssemblyName, NamespaceName, ClassName: WideString;
  const Parameters: array of const);
begin
  inherited Create;
end;

constructor TJclClrObject.Create(const AssemblyName, NamespaceName, ClassName: WideString;
  const NewInstance: Boolean);
begin
  Create(AssemblyName, NamespaceName, ClassName, []);
end;

function TJclClrObject.GetField(const Name: WideString): TJclClrField;
begin
  // (rom) added to suppress warning until implementation
  Result := nil;
end;

function TJclClrObject.GetProperty(const Name: WideString): TJclClrProperty;
begin
  // (rom) added to suppress warning until implementation
  Result := nil;
end;

function TJclClrObject.GetMethod(const Name: WideString): TJclClrMethod;
begin
  // (rom) added to suppress warning until implementation
  Result := nil;
end;

//=== { TJclClrAppDomainSetup } ==============================================

constructor TJclClrAppDomainSetup.Create(Intf: IAppDomainSetup);
begin
  Assert(Assigned(Intf));
  inherited Create;
  FDefaultInterface := Intf;
end;

function TJclClrAppDomainSetup.GetApplicationBase: WideString;
begin
  OleCheck(FDefaultInterface.Get_ApplicationBase(Result));
end;

function TJclClrAppDomainSetup.GetApplicationName: WideString;
begin
  OleCheck(FDefaultInterface.Get_ApplicationName(Result));
end;

function TJclClrAppDomainSetup.GetCachePath: WideString;
begin
  OleCheck(FDefaultInterface.Get_CachePath(Result));
end;

function TJclClrAppDomainSetup.GetConfigurationFile: WideString;
begin
  OleCheck(FDefaultInterface.Get_ConfigurationFile(Result));
end;

function TJclClrAppDomainSetup.GetDynamicBase: WideString;
begin
  OleCheck(FDefaultInterface.Get_DynamicBase(Result));
end;

function TJclClrAppDomainSetup.GetLicenseFile: WideString;
begin
  OleCheck(FDefaultInterface.Get_LicenseFile(Result));
end;

function TJclClrAppDomainSetup.GetPrivateBinPath: WideString;
begin
  OleCheck(FDefaultInterface.Get_PrivateBinPath(Result));
end;

function TJclClrAppDomainSetup.GetPrivateBinPathProbe: WideString;
begin
  OleCheck(FDefaultInterface.Get_PrivateBinPathProbe(Result));
end;

function TJclClrAppDomainSetup.GetShadowCopyDirectories: WideString;
begin
  OleCheck(FDefaultInterface.Get_ShadowCopyDirectories(Result));
end;

function TJclClrAppDomainSetup.GetShadowCopyFiles: WideString;
begin
  OleCheck(FDefaultInterface.Get_ShadowCopyFiles(Result));
end;

procedure TJclClrAppDomainSetup.SetApplicationBase(const Value: WideString);
begin
  OleCheck(FDefaultInterface.Set_ApplicationBase(Value));
end;

procedure TJclClrAppDomainSetup.SetApplicationName(const Value: WideString);
begin
  OleCheck(FDefaultInterface.Set_ApplicationName(Value));
end;

procedure TJclClrAppDomainSetup.SetCachePath(const Value: WideString);
begin
  OleCheck(FDefaultInterface.Set_CachePath(Value));
end;

procedure TJclClrAppDomainSetup.SetConfigurationFile(const Value: WideString);
begin
  OleCheck(FDefaultInterface.Set_ConfigurationFile(Value));
end;

procedure TJclClrAppDomainSetup.SetDynamicBase(const Value: WideString);
begin
  OleCheck(FDefaultInterface.Set_DynamicBase(Value));
end;

procedure TJclClrAppDomainSetup.SetLicenseFile(const Value: WideString);
begin
  OleCheck(FDefaultInterface.Set_LicenseFile(Value));
end;

procedure TJclClrAppDomainSetup.SetPrivateBinPath(const Value: WideString);
begin
  OleCheck(FDefaultInterface.Set_PrivateBinPath(Value));
end;

procedure TJclClrAppDomainSetup.SetPrivateBinPathProbe(const Value: WideString);
begin
  OleCheck(FDefaultInterface.Set_PrivateBinPathProbe(Value));
end;

procedure TJclClrAppDomainSetup.SetShadowCopyDirectories(const Value: WideString);
begin
  OleCheck(FDefaultInterface.Set_ShadowCopyDirectories(Value));
end;

procedure TJclClrAppDomainSetup.SetShadowCopyFiles(const Value: WideString);
begin
  OleCheck(FDefaultInterface.Set_ShadowCopyFiles(Value));
end;

//=== { TJclClrAssembly } ====================================================

constructor TJclClrAssembly.Create(Intf: IJclClrAssembly);
begin
  Assert(Assigned(Intf));
  inherited Create;
  FDefaultInterface := Intf;
end;

// History:

// $Log$
// Revision 1.12  2005/02/25 07:20:15  marquardt
// add section lines
//
// Revision 1.11  2005/02/24 16:34:52  marquardt
// remove divider lines, add section lines (unfinished)
//
// Revision 1.10  2004/10/17 21:00:14  mthoma
// cleaning
//
// Revision 1.9  2004/08/09 06:38:08  marquardt
// add JvWStrUtils.pas as JclWideStrings.pas
//
// Revision 1.8  2004/08/01 11:40:23  marquardt
// move constructors/destructors
//
// Revision 1.7  2004/06/14 13:05:21  marquardt
// style cleaning ENDIF, Tabs
//
// Revision 1.6  2004/05/05 07:33:49  rrossmair
// header updated according to new policy: initial developers & contributors listed
//
// Revision 1.5  2004/04/06 04:55:17  
// adapt compiler conditions, add log entry
//

end.
