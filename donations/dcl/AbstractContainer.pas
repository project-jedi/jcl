//------------------------------------------------------------------------------
// The Delphi Container Library
// Jean-Philippe BEMPEL aka RDM
// rdm_30@yahoo.com
//------------------------------------------------------------------------------
unit AbstractContainer;

{$I dcl.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  Libc,
  {$ENDIF UNIX}
  DCL_Intf, DCLUtil;

type
  TIntfCriticalSection = class(TObject, IInterface)
  private
    FCriticalSection: TRTLCriticalSection;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TAbstractContainer = class(TInterfacedObject)
  {$IFDEF THREADSAFE}
  private
    FCriticalSection: TIntfCriticalSection;
  protected
    function EnterCriticalSection: IInterface;
  public
    constructor Create;
    destructor Destroy; override;
  {$ENDIF THREADSAFE}
  end;

implementation

//=== { TIntfCriticalSection } ===============================================

constructor TIntfCriticalSection.Create;
begin
  inherited Create;
  InitializeCriticalSection(FCriticalSection);
end;

destructor TIntfCriticalSection.Destroy;
begin
  DeleteCriticalSection(FCriticalSection);
  inherited Destroy;
end;

function TIntfCriticalSection._AddRef: Integer;
begin
  EnterCriticalSection(FCriticalSection);
  Result := 0;
end;

function TIntfCriticalSection._Release: Integer;
begin
  LeaveCriticalSection(FCriticalSection);
  Result := 0;
end;

function TIntfCriticalSection.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

//=== { TAbstractContainer } =================================================

{$IFDEF THREADSAFE}

constructor TAbstractContainer.Create;
begin
  FCriticalSection := TIntfCriticalSection.Create;
end;

destructor TAbstractContainer.Destroy;
begin
  FCriticalSection.Free;
  inherited Destroy;
end;

function TAbstractContainer.EnterCriticalSection: IInterface;
begin
  Result := FCriticalSection as IInterface;
end;

{$ENDIF THREADSAFE}

end.

