unit PeViewerControl;

{$I JCL.INC}

interface

uses
  ComObj, ActiveX, PeViewer_TLB, Forms, Windows, StdVcl;

type
  TPeViewerControl = class(TAutoObject, IPeViewerControl)
  private
    FROTHandle: Integer;
  protected
    procedure OpenFile(const FileName: WideString); safecall;
    procedure BringToFront; safecall;
    { Protected declarations }
  public
    destructor Destroy; override;
    procedure Initialize; override;
  end;

implementation

uses ComServ, PeViewerMain;

procedure TPeViewerControl.OpenFile(const FileName: WideString);
begin
  if Length(FileName) > 0 then MainForm.OpenFile(FileName, True);
end;

procedure TPeViewerControl.BringToFront;
begin
  Application.Restore;
  SetForegroundWindow(Application.Handle);
end;

procedure TPeViewerControl.Initialize;
begin
  inherited;
  OleCheck(RegisterActiveObject(Self as IUnknown, Class_PeViewerControl,
    ACTIVEOBJECT_WEAK, FROTHandle));
  {$IFDEF DELPHI5_UP}
  ComServer.UIInteractive := False;
  {$ENDIF}
end;

destructor TPeViewerControl.Destroy;
begin
  OleCheck(RevokeActiveObject(FROTHandle, nil));
  inherited;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TPeViewerControl, Class_PeViewerControl,
    ciMultiInstance, tmApartment);
end.
