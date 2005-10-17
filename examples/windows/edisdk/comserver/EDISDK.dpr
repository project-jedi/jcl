library EDISDK;

uses
  ComServ,
  EDISDK_TLB in 'EDISDK_TLB.pas',
  JclEDICOM_ANSIX12 in 'JclEDICOM_ANSIX12.pas';

{$R *.TLB}

{$E dll}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.RES}

begin
end.
