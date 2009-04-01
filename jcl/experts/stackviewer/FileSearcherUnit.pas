unit FileSearcherUnit;

interface

uses
  SysUtils, Classes, Contnrs, JclFileUtils;

type
  TFileSearchItem = class(TObject)
  private
    FName: string;
    FResults: TStringList;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;
    property Name: string read FName;
    property Results: TStringList read FResults;
  end;

  TFileSearcher = class(TObject)
  private
    FFiles: TObjectList;
    FSearchPaths: TStringList;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TFileSearchItem;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const AName: string): TFileSearchItem;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    function IndexOf(const AName: string): Integer;
    procedure Search;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TFileSearchItem read GetItems; default;
    property SearchPaths: TStringList read FSearchPaths;
  end;

implementation

{ TFileSearcher }

constructor TFileSearcher.Create;
begin
  inherited Create;
  FFiles := TObjectList.Create;
  FSearchPaths := TStringList.Create;
end;

destructor TFileSearcher.Destroy;
begin
  FSearchPaths.Free;
  FFiles.Free;
  inherited Destroy;
end;

function TFileSearcher.Add(const AName: string): TFileSearchItem;
begin
  FFiles.Add(TFileSearchItem.Create(AName));
  Result := TFileSearchItem(FFiles.Last);
end;

procedure TFileSearcher.Clear;
begin
  FFiles.Clear;
end;

procedure TFileSearcher.Delete(AIndex: Integer);
begin
  FFiles.Delete(AIndex);
end;

function TFileSearcher.GetCount: Integer;
begin
  Result := FFiles.Count;
end;

function TFileSearcher.GetItems(AIndex: Integer): TFileSearchItem;
begin
  Result := TFileSearchItem(FFiles[AIndex]);
end;

function TFileSearcher.IndexOf(const AName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Items[I].Name = AName then
    begin
      Result := I;
      Break;
    end;
end;

procedure TFileSearcher.Search;
var
  I, Idx: Integer;
  FastIndexList: TStringList;
  FS: TFileSearchItem;
  Path: string;
  sr: TSearchRec;
  re: Integer;
begin
  if (Count > 0) then
  begin
    for I := 0 to Count - 1 do
      Items[I].Results.Clear;
    if FSearchPaths.Count > 0 then
    begin
      FastIndexList := TStringList.Create;
      try
        FastIndexList.Sorted := True;
        for I := 0 to Count - 1 do
          FastIndexList.AddObject(Items[I].Name, Items[I]);
        for I := 0 to FSearchPaths.Count - 1 do
        begin
          Path := PathAddSeparator(FSearchPaths[I]);
          re := FindFirst(Path + '*.*', faAnyFile - faDirectory, sr);
          while re = 0 do
          begin
            Idx := FastIndexList.IndexOf(sr.Name);
            if Idx <> -1 then
            begin
              FS := TFileSearchItem(FastIndexList.Objects[Idx]);
              FS.Results.Add(Path + sr.Name);
            end;
            re := FindNext(sr);
          end;
          FindClose(sr);
        end;
      finally
        FastIndexList.Free;
      end;
    end;
  end;
end;

{ TFileSearchItem }

constructor TFileSearchItem.Create(const AName: string);
begin
  inherited Create;
  FResults := TStringList.Create;
  FName := AName;
end;

destructor TFileSearchItem.Destroy;
begin
  FResults.Free;
  inherited Destroy;
end;

end.
