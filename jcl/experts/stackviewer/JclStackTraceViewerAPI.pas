unit StackLineNumberTranslator;

interface

uses
  Classes, ActiveX;

type
  IJclLineNumberTranslator = interface
  ['{01E06940-49AE-464B-AC47-D65DFBC41396}']
    function GetIDString: string;
    function GetName: string;
    function TranslateLineNumbers(ARevisionContent, ACurrentContent: IStream;
      ARevisionLineNumbers: TList; ACurrentLineNumbers: TList): Integer;

    property Name: string read GetName;
    property IDString: string read GetIDString;
  end;

  IJclRevisionProvider = interface
  ['{8127FF3C-083D-47FD-855D-6C68EC7CBFB9}']
    function GetIDString: string;
    function GetName: string;
    function GetRevisionContent(const AFileName, ARevision: string; AContent: IStream): Boolean;

    property Name: string read GetName;
    property IDString: string read GetIDString;
  end;

  TJclLineNumberTranslators = class(TObject)
  private
    FIndexList: TList;
    FNextIndex: Integer;
    FTranslators: TInterfaceList;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): IJclLineNumberTranslator;
  public
    constructor Create;
    destructor Destroy; override;
    function RegisterTranslator(const ATranslator: IJclLineNumberTranslator): Integer;
    function TranslateLineNumbers(ARevisionContent, ACurrentContent: IStream;
      ARevisionLineNumbers: TList; ACurrentLineNumbers: TList): Integer;
    procedure UnregisterTranslator(AIndex: Integer);
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: IJclLineNumberTranslator read GetItems; default;
  end;

  TJclRevisionProviders = class(TObject)
  private
    FIndexList: TList;
    FNextIndex: Integer;
    FTranslators: TInterfaceList;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): IJclRevisionProvider;
  public
    constructor Create;
    destructor Destroy; override;
    function RegisterProvider(const ATranslator: IJclRevisionProvider): Integer;
    function GetRevisionContent(const AFileName, ARevision: string; AContent: IStream): Boolean;
    procedure UnregisterProvider(AIndex: Integer);
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: IJclRevisionProvider read GetItems; default;
  end;

var
  LineNumberTranslators: TJclLineNumberTranslators;
  RevisionProviders: TJclRevisionProviders;

function TranslateLineNumbers(ARevisionContent, ACurrentContent: IStream; ARevisionLineNumbers: TList; ACurrentLineNumbers: TList): Integer;

function RegisterLineNumberTranslator(const ATranslator: IJclLineNumberTranslator): Integer;
procedure UnregisterLineNumberTranslator(AIndex: Integer);

function GetRevisionContent(const AFileName, ARevision: string; AContent: IStream): Boolean;

function RegisterRevisionProvider(const ATranslator: IJclRevisionProvider): Integer;
procedure UnregisterRevisionProvider(AIndex: Integer);

implementation

{ TJclLineNumberTranslators }

constructor TJclLineNumberTranslators.Create;
begin
  inherited Create;
  FNextIndex := 1;
  FIndexList := TList.Create;
  FTranslators := TInterfaceList.Create;
end;

destructor TJclLineNumberTranslators.Destroy;
begin
  FTranslators.Free;
  FIndexList.Free;
  inherited Destroy;
end;

function TJclLineNumberTranslators.GetCount: Integer;
begin
  Result := FTranslators.Count;
end;

function TJclLineNumberTranslators.GetItems(AIndex: Integer): IJclLineNumberTranslator;
begin
  Result := IJclLineNumberTranslator(FTranslators[AIndex]);
end;

function TJclLineNumberTranslators.RegisterTranslator(const ATranslator: IJclLineNumberTranslator): Integer;
begin
  if Assigned(ATranslator) then
  begin
    Result := FNextIndex;
    Inc(FNextIndex);
    FTranslators.Add(ATranslator);
    FIndexList.Add(Pointer(Result));
  end
  else
    Result := -1;
end;

function TJclLineNumberTranslators.TranslateLineNumbers(ARevisionContent, ACurrentContent: IStream;
  ARevisionLineNumbers: TList; ACurrentLineNumbers: TList): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].TranslateLineNumbers(ARevisionContent, ACurrentContent, ARevisionLineNumbers, ACurrentLineNumbers);
    if Result > 0 then
      Break;
  end;
end;

procedure TJclLineNumberTranslators.UnregisterTranslator(AIndex: Integer);
var
  Idx: Integer;
begin
  Idx := FIndexList.IndexOf(Pointer(AIndex));
  if Idx <> -1 then
  begin
    FTranslators.Delete(Idx);
    FIndexList.Delete(Idx);
  end;
end;

{ TJclRevisionProviders }

constructor TJclRevisionProviders.Create;
begin
  inherited Create;
  FNextIndex := 1;
  FIndexList := TList.Create;
  FTranslators := TInterfaceList.Create;
end;

destructor TJclRevisionProviders.Destroy;
begin
  FTranslators.Free;
  FIndexList.Free;
  inherited Destroy;
end;

function TJclRevisionProviders.GetCount: Integer;
begin
  Result := FTranslators.Count;
end;

function TJclRevisionProviders.GetItems(AIndex: Integer): IJclRevisionProvider;
begin
  Result := IJclRevisionProvider(FTranslators[AIndex]);
end;

function TJclRevisionProviders.RegisterProvider(const ATranslator: IJclRevisionProvider): Integer;
begin
  if Assigned(ATranslator) then
  begin
    Result := FNextIndex;
    Inc(FNextIndex);
    FTranslators.Add(ATranslator);
    FIndexList.Add(Pointer(Result));
  end
  else
    Result := -1;
end;

function TJclRevisionProviders.GetRevisionContent(const AFileName, ARevision: string; AContent: IStream): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
    if Items[I].GetRevisionContent(AFileName, ARevision, AContent) then
    begin
      Result := True;
      Break;
    end;
end;

procedure TJclRevisionProviders.UnregisterProvider(AIndex: Integer);
var
  Idx: Integer;
begin
  Idx := FIndexList.IndexOf(Pointer(AIndex));
  if Idx <> -1 then
  begin
    FTranslators.Delete(Idx);
    FIndexList.Delete(Idx);
  end;
end;

function RegisterLineNumberTranslator(const ATranslator: IJclLineNumberTranslator): Integer;
begin
  Result := LineNumberTranslators.RegisterTranslator(ATranslator);
end;

function TranslateLineNumbers(ARevisionContent, ACurrentContent: IStream; ARevisionLineNumbers: TList; ACurrentLineNumbers: TList): Integer;
begin
  Result := LineNumberTranslators.TranslateLineNumbers(ARevisionContent, ACurrentContent, ARevisionLineNumbers, ACurrentLineNumbers);
end;

procedure UnregisterLineNumberTranslator(AIndex: Integer);
begin
  LineNumberTranslators.UnregisterTranslator(AIndex);
end;

function GetRevisionContent(const AFileName, ARevision: string; AContent: IStream): Boolean;
begin
  Result := RevisionProviders.GetRevisionContent(AFileName, ARevision, AContent);
end;

function RegisterRevisionProvider(const ATranslator: IJclRevisionProvider): Integer;
begin
  Result := RevisionProviders.RegisterProvider(ATranslator);
end;

procedure UnregisterRevisionProvider(AIndex: Integer);
begin
  RevisionProviders.UnregisterProvider(AIndex);
end;

initialization
  LineNumberTranslators := TJclLineNumberTranslators.Create;
  RevisionProviders := TJclRevisionProviders.Create;

finalization
  LineNumberTranslators.Free;
  RevisionProviders.Free;

end.
