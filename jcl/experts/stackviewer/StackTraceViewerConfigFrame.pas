unit StackTraceViewerConfigFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Dialogs, StdCtrls, ExtCtrls, ExceptionViewerOptionsUnit;

type
  TJclStackTraceViewerConfigFrame = class(TFrame)
    cbExpandTreeView: TCheckBox;
  private
    FOptions: TExceptionViewerOption;
    function GetOptions: TExceptionViewerOption;
    procedure SetOptions(const Value: TExceptionViewerOption);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Options: TExceptionViewerOption read GetOptions write SetOptions;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL:  $';
    Revision: '$Revision:  $';
    Date: '$Date:  $';
    LogPath: ''
    );
{$ENDIF UNITVERSIONING}

implementation

{$R *.dfm}

//=== { TJclStackTraceViewerConfigFrame } ====================================

constructor TJclStackTraceViewerConfigFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := TExceptionViewerOption.Create;
end;

destructor TJclStackTraceViewerConfigFrame.Destroy;
begin
  FOptions.Free;
  inherited Destroy;
end;

function TJclStackTraceViewerConfigFrame.GetOptions: TExceptionViewerOption;
begin
  Result := FOptions;
  FOptions.ExpandTreeView := cbExpandTreeView.Checked;
end;

procedure TJclStackTraceViewerConfigFrame.SetOptions(const Value: TExceptionViewerOption);
begin
  FOptions.Assign(Value);
  cbExpandTreeView.Checked := FOptions.ExpandTreeView;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
