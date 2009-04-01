unit ExceptionViewerOptionsUnit;

interface

uses
  Classes;

type
  TExceptionViewerOption = class(TPersistent)
  private
    FExpandTreeView: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    property ExpandTreeView: Boolean read FExpandTreeView write FExpandTreeView;
  end;

implementation

{ TExceptionViewerOption }

constructor TExceptionViewerOption.Create;
begin
  inherited Create;
  FExpandTreeView := False;
end;

procedure TExceptionViewerOption.AssignTo(Dest: TPersistent);
begin
  if Dest is TExceptionViewerOption then
  begin
    TExceptionViewerOption(Dest).FExpandTreeView := ExpandTreeView;
  end
  else
    inherited AssignTo(Dest);
end;

end.
