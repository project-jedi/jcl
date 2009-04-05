unit ExceptionViewerOptionsUnit;

interface

uses
  Classes;

type
  TExceptionViewerOption = class(TPersistent)
  private
    FExpandTreeView: Boolean;
    FModuleVersionAsRevision: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    property ExpandTreeView: Boolean read FExpandTreeView write FExpandTreeView;
    property ModuleVersionAsRevision: Boolean read FModuleVersionAsRevision write FModuleVersionAsRevision;
  end;

implementation

{ TExceptionViewerOption }

constructor TExceptionViewerOption.Create;
begin
  inherited Create;
  FExpandTreeView := False;
  FModuleVersionAsRevision := False;
end;

procedure TExceptionViewerOption.AssignTo(Dest: TPersistent);
begin
  if Dest is TExceptionViewerOption then
  begin
    TExceptionViewerOption(Dest).FExpandTreeView := ExpandTreeView;
    TExceptionViewerOption(Dest).FModuleVersionAsRevision := ModuleVersionAsRevision;
  end
  else
    inherited AssignTo(Dest);
end;

end.
