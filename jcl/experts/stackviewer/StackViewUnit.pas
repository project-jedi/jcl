unit StackViewUnit;

interface

uses
  Contnrs, JclDebug;

type
  TStackViewItem = class(TJclLocationInfoEx)
  private
    FFoundFile: Boolean;
    FFileName: string;
    FProjectName: string;
    FRevision: string;
    FTranslatedLineNumber: Integer;
  public
    property FileName: string read FFileName write FFileName;
    property FoundFile: Boolean read FFoundFile write FFoundFile;
    property ProjectName: string read FProjectName write FProjectName;
    property Revision: string read FRevision write FRevision;
    property TranslatedLineNumber: Integer read FTranslatedLineNumber write FTranslatedLineNumber;
  end;

  TStackViewItemsList = class(TObject)
  private
    FItems: TObjectList;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TStackViewItem;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TStackViewItem;
    procedure Clear;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TStackViewItem read GetItems; default;
  end;

implementation

{ TStackViewItemsList }

constructor TStackViewItemsList.Create;
begin
  FItems := TObjectList.Create;
end;

destructor TStackViewItemsList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TStackViewItemsList.Add: TStackViewItem;
begin
  FItems.Add(TStackViewItem.Create(nil, nil));
  Result := TStackViewItem(FItems.Last);
end;

procedure TStackViewItemsList.Clear;
begin
  FItems.Clear;
end;

function TStackViewItemsList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TStackViewItemsList.GetItems(AIndex: Integer): TStackViewItem;
begin
  Result := TStackViewItem(FItems[AIndex]);
end;

end.
