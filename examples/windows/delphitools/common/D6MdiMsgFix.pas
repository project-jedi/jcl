unit D6MdiMsgFix;

interface

{$I jcl.inc}

implementation

{$IFDEF DELPHI6}

uses
  Windows, Classes, SysUtils, Forms, AppEvnts;

type
  TFixApplicationEvents = class(TCustomApplicationEvents)
  protected
    procedure ApplicationEventsMessage(var Msg: TMsg; var Handled: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TApplicationAccess = class(TApplication);

var
  FixApplicationEvents: TFixApplicationEvents;

{ TFixApplicationEvents }

procedure TFixApplicationEvents.ApplicationEventsMessage(var Msg: TMsg; var Handled: Boolean);
begin
  with Application do
    if Assigned(MainForm) and (MainForm.FormStyle = fsMDIForm) and
      Assigned(Screen.ActiveForm) and (Screen.ActiveForm.FormStyle <> fsMdiChild) then
      begin
        Handled := True;
        with TApplicationAccess(Application) do
          if not IsKeyMsg(Msg) and not IsDlgMsg(Msg) then
          begin
            // prevent to call buggy TApplication.IsMDIMsg method, handle message here
            TranslateMessage(Msg);
            DispatchMessage(Msg);
          end;
      end;
end;

constructor TFixApplicationEvents.Create(AOwner: TComponent);
begin
  inherited;
  OnMessage := ApplicationEventsMessage;
end;

initialization
  FixApplicationEvents := TFixApplicationEvents.Create(nil);

finalization
  FreeAndNil(FixApplicationEvents);

{$ENDIF DELPHI6}

end.
