unit JclDebugStackUtils;

interface

uses
  Windows, SysUtils, Classes, Contnrs, JclDebug;

type
  TStackItem = class(TPersistent)
  private
    FSourceUnitName: string;
    FSourceName: string;
    FLineNumber: Integer;
    FProcedureName: string;
    FModuleName: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property ModuleName: string read FModuleName write FModuleName;
    property ProcedureName: string read FProcedureName write FProcedureName;
    property SourceUnitName: string read FSourceUnitName write FSourceUnitName;
    property SourceName: string read FSourceName write FSourceName;
    property LineNumber: Integer read FLineNumber write FLineNumber;
  end;

  TThreadInfoStack = class(TObject)
  private
    FItems: TObjectList;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TStackItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromString(AInString: string);
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TStackItem read GetItems; default;
  end;

  TThreadInfo = class(TObject)
  private
    FThreadID: DWORD;
    FStack: TThreadInfoStack;
  public
    constructor Create;
    destructor Destroy; override;
    property ThreadID: DWORD read FThreadID write FThreadID;
    property Stack: TThreadInfoStack read FStack;
  end;

  {
  TThreadInfoList = class(TObject)
  private
    FItems: TObjectList;
    FCount: Integer;
    function GetItems(AIndex: Integer): TThreadInfo;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromString(AInString: string);
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TThreadInfo read GetItems; default;
  end;
  }

  TThreadInfoList = class(TJclThreadInfoList)
  private
    procedure LoadStackFromString(AStack: TJclLocationInfoList; AInString: string);
  public
    procedure LoadFromString(AInString: string);
  end;

  TException = class(TObject)
  private
    FExceptionClassName: string;
    FExceptionMessage: string;
  public
    procedure Clear;
    procedure LoadFromString(AInString: string);
    property ExceptionClassName: string read FExceptionClassName write FExceptionClassName;
    property ExceptionMessage: string read FExceptionMessage write FExceptionMessage;
  end;

  TModule = class(TObject)
  private
    FHandleStr: string;
    FModuleName: string;
  public
    property HandleStr: string read FHandleStr write FHandleStr;
    property ModuleName: string read FModuleName write FModuleName;
  end;

  TModuleList = class(TObject)
  private
    FItems: TObjectList;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TModule;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromString(AInString: string);
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TModule read GetItems; default;
  end;

  TExceptionInfo = class(TObject)
  private
    FException: TException;
    FThreadInfoList: TThreadInfoList;
    FModules: TModuleList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromString(AInString: string);
    property ThreadInfoList: TThreadInfoList read FThreadInfoList;
    property Exception: TException read FException;
    property Modules: TModuleList read FModules;
  end;

implementation

type
  TCSVValue = class(TObject)
    Value: string;
  end;

  TCSVRecord = class(TObject)
  private
    FItems: TObjectList;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TCSVValue;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TCSVValue;
    procedure Clear;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TCSVValue read GetItems; default;
  end;

  TCSVFile = class(TObject)
  private
    FItems: TObjectList;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TCSVRecord;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TCSVRecord;
    procedure LoadFromString(AInString: string);
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TCSVRecord read GetItems; default;
  end;

{ TCSVRecord }

function TCSVRecord.Add: TCSVValue;
begin
  FItems.Add(TCSVValue.Create);
  Result := TCSVValue(FItems.Last);
end;

procedure TCSVRecord.Clear;
begin
  FItems.Clear;
end;

constructor TCSVRecord.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TCSVRecord.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TCSVRecord.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TCSVRecord.GetItems(AIndex: Integer): TCSVValue;
begin
  Result := TCSVValue(FItems[AIndex]);
end;

{ TCSVFile }

constructor TCSVFile.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TCSVFile.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TCSVFile.Add: TCSVRecord;
begin
  FItems.Add(TCSVRecord.Create);
  Result := TCSVRecord(FItems.Last);
end;

function TCSVFile.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TCSVFile.GetItems(AIndex: Integer): TCSVRecord;
begin
  Result := TCSVRecord(FItems[AIndex]);
end;

function JvAnsiStrSplitStrings2(var InString: AnsiString; const SplitChar, QuoteChar: AnsiChar; CSVItems: TCSVRecord): Integer;
var
  I, Len, SplitCounter: Integer;
  Ch: AnsiChar;
  InQuotes: Boolean;
  OutString: AnsiString;
begin
  InQuotes := False;
  Len := Length(InString);
  CSVItems.Clear;
  SplitCounter := 0; // ALWAYS ASSUME THAT ZERO IS VALID IN THE OUTGOING ARRAY.

  for I := 1 to Len do
  begin
    Ch := InString[I];
    if (Ch in [#10]) and not InQuotes then
    begin
      Delete(InString, 1, I);
      Break;
    end
    else
    if (Ch = SplitChar) and not InQuotes then
    begin
      CSVItems.Add.Value := AnsiDequotedStr(string(OutString), Char(QuoteChar));
      OutString := '';
      Inc(SplitCounter);
    end
    else
    begin
      OutString := OutString + Ch;
      if Ch = QuoteChar then
        InQuotes := not InQuotes;
    end;
    if I = Len then
      InString := '';
  end;
  I := Length(OutString);
  if (I > 0) and (OutString[I] = #13) then
    Delete(OutString, I, 1);
  CSVItems.Add.Value := AnsiDequotedStr(string(OutString), Char(QuoteChar));
  Inc(SplitCounter);
  Result := SplitCounter;
end;

procedure TCSVFile.LoadFromString(AInString: string);
var
  S: AnsiString;
  P: Integer;
begin
  FItems.Clear;
  S := AInString;
  P := Pos(#10, S);
  if P > 0 then
    Delete(S, 1, P);
  while S <> '' do
    JvAnsiStrSplitStrings2(S, ';', '"', Add);
end;


{ TStackInfo }
{

constructor TThreadInfoList.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TThreadInfoList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TThreadInfoList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TThreadInfoList.GetItems(AIndex: Integer): TThreadInfo;
begin
  Result := TThreadInfo(FItems[AIndex]);
end;

procedure TThreadInfoList.LoadFromString(AInString: string);
var
  CSVFile: TCSVFile;
  I: Integer;
  ThreadInfo: TThreadInfo;
begin
  FItems.Clear;
  CSVFile := TCSVFile.Create;
  try
    CSVFile.LoadFromString(AInString);
    for I := 0 to CSVFile.Count - 1 do
    begin
      FItems.Add(TThreadInfo.Create);
      ThreadInfo := TThreadInfo(FItems.Last);
      if CSVFile[I].Count > 0 then
        ThreadInfo.ThreadID := StrToIntDef(CSVFile[I][0].Value, 0);
      if CSVFile[I].Count > 6 then
        ThreadInfo.Stack.LoadFromString(CSVFile[I][6].Value);
    end;
  finally
    CSVFile.Free;
  end;
end;
}
procedure TThreadInfoList.LoadFromString(AInString: string);
var
  CSVFile: TCSVFile;
  CSVRecord: TCSVRecord;
  I: Integer;
  ThreadInfo: TJclThreadInfo;
begin
  Clear;
  CSVFile := TCSVFile.Create;
  try
    CSVFile.LoadFromString(AInString);
    for I := 0 to CSVFile.Count - 1 do
    begin
      CSVRecord := CSVFile[I];
      ThreadInfo := Add;
      ThreadInfo.Values := [];
      if CSVRecord.Count > 0 then
        ThreadInfo.ThreadID := StrToIntDef(CSVRecord[0].Value, 0);
      if (CSVRecord.Count > 1) and (CSVRecord[1].Value = '1') then
        ThreadInfo.Values := ThreadInfo.Values + [tioIsMainThread];
      if (CSVRecord.Count > 2) and (CSVRecord[2].Value <> '') then
      begin
        ThreadInfo.Name := CSVRecord[2].Value;
        ThreadInfo.Values := ThreadInfo.Values + [tioName];
      end;
      if (CSVRecord.Count > 3) and (CSVRecord[3].Value <> '') then
      begin
        ThreadInfo.CreationTime := StrToDateTime(CSVRecord[3].Value);
        ThreadInfo.Values := ThreadInfo.Values + [tioCreationTime];
      end;
      if (CSVRecord.Count > 4) and (CSVRecord[4].Value <> '') then
      begin
        ThreadInfo.ParentThreadID := StrToIntDef(CSVRecord[4].Value, 0);
        if ThreadInfo.ParentThreadID <> 0 then
          ThreadInfo.Values := ThreadInfo.Values + [tioParentThreadID];
      end;
      if (CSVRecord.Count > 5) and (CSVRecord[5].Value <> '') then
      begin
        LoadStackFromString(ThreadInfo.Stack, CSVRecord[5].Value);
        ThreadInfo.Values := ThreadInfo.Values + [tioStack];
      end;
      if (CSVRecord.Count > 6) and (CSVRecord[6].Value <> '') then
      begin
        LoadStackFromString(ThreadInfo.CreationStack, CSVRecord[6].Value);
        ThreadInfo.Values := ThreadInfo.Values + [tioCreationStack];
      end;
    end;
  finally
    CSVFile.Free;
  end;
end;

procedure TThreadInfoList.LoadStackFromString(AStack: TJclLocationInfoList; AInString: string);
var
  CSVFile: TCSVFile;
  CSVRecord: TCSVRecord;
  I: Integer;
  LocationInfoEx: TJclLocationInfoEx;
begin
  AStack.Clear;
  CSVFile := TCSVFile.Create;
  try
    CSVFile.LoadFromString(AInString);
    for I := 0 to CSVFile.Count - 1 do
    begin
      CSVRecord := CSVFile[I];
      LocationInfoEx := AStack.Add(nil);
      LocationInfoEx.Values := [];
      if (CSVRecord.Count > 3) and (CSVRecord[3].Value <> '') then
        LocationInfoEx.Values := LocationInfoEx.Values + [lievLocationInfo];
      if (CSVRecord.Count > 9) and (CSVRecord[9].Value <> '') then
        LocationInfoEx.Values := LocationInfoEx.Values + [lievProcedureStartLocationInfo];
      if CSVRecord.Count > 0 then
        LocationInfoEx.VAddress := Pointer(StrToIntDef('$' + CSVRecord[0].Value, 0));
      if CSVRecord.Count > 1 then
        LocationInfoEx.ModuleName := CSVRecord[1].Value;
      if CSVRecord.Count > 2 then
        LocationInfoEx.Address := Pointer(StrToIntDef('$' + CSVRecord[2].Value, 0));
      if CSVRecord.Count > 3 then
        LocationInfoEx.OffsetFromProcName := StrToIntDef('$' + CSVRecord[3].Value, 0);
      if CSVRecord.Count > 4 then
        LocationInfoEx.SourceUnitName := CSVRecord[4].Value;
      if CSVRecord.Count > 5 then
        LocationInfoEx.ProcedureName := CSVRecord[5].Value;
      if CSVRecord.Count > 6 then
        LocationInfoEx.SourceName := CSVRecord[6].Value;
      if CSVRecord.Count > 7 then
        LocationInfoEx.LineNumber := StrToIntDef(CSVRecord[7].Value, -1);
      if CSVRecord.Count > 8 then
        LocationInfoEx.OffsetFromLineNumber := StrToIntDef(CSVRecord[8].Value, -1);
      if CSVRecord.Count > 9 then
        LocationInfoEx.LineNumberOffsetFromProcedureStart := StrToIntDef(CSVRecord[9].Value, -1);
    end;
  finally
    CSVFile.Free;
  end;
end;


{ TStack }

constructor TThreadInfoStack.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TThreadInfoStack.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TThreadInfoStack.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TThreadInfoStack.GetItems(AIndex: Integer): TStackItem;
begin
  Result := TStackItem(FItems[AIndex]);
end;

procedure TThreadInfoStack.LoadFromString(AInString: string);
var
  CSVFile: TCSVFile;
  I: Integer;
  Stack: TStackItem;
begin
  FItems.Clear;
  CSVFile := TCSVFile.Create;
  try
    CSVFile.LoadFromString(AInString);
    for I := 0 to CSVFile.Count - 1 do
    begin
      FItems.Add(TStackItem.Create);
      Stack := TStackItem(FItems.Last);
      if CSVFile[I].Count > 1 then
        Stack.ModuleName := CSVFile[I][1].Value;
      if CSVFile[I].Count > 4 then
        Stack.SourceUnitName := CSVFile[I][4].Value;
      if CSVFile[I].Count > 5 then
        Stack.ProcedureName := CSVFile[I][5].Value;
      if CSVFile[I].Count > 6 then
        Stack.SourceName := CSVFile[I][6].Value;
      if CSVFile[I].Count > 7 then
        Stack.LineNumber := StrToIntDef(CSVFile[I][7].Value, -1);
    end;
  finally
    CSVFile.Free;
  end;
end;

{ TStackItem }

procedure TStackItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TStackItem then
  begin
    TStackItem(Dest).ModuleName := ModuleName;
    TStackItem(Dest).ProcedureName := ProcedureName;
    TStackItem(Dest).SourceUnitName := SourceUnitName;
    TStackItem(Dest).SourceName := SourceName;
    TStackItem(Dest).LineNumber := LineNumber;
  end
  else
    inherited AssignTo(Dest);
end;

{ TThreadInfo }

constructor TThreadInfo.Create;
begin
  inherited Create;
  FStack := TThreadInfoStack.Create;
end;

destructor TThreadInfo.Destroy;
begin
  FStack.Free;
  inherited Destroy;
end;

{ TExceptionInfo }

constructor TExceptionInfo.Create;
begin
  inherited Create;
  FException := TException.Create;
  FThreadInfoList := TThreadInfoList.Create;
  FModules := TModuleList.Create;
end;

destructor TExceptionInfo.Destroy;
begin
  FModules.Free;
  FException.Free;
  FThreadInfoList.Free;
  inherited Destroy;
end;

procedure TExceptionInfo.LoadFromString(AInString: string);
var
  CSVFile: TCSVFile;
  CSVRecord: TCSVRecord;
  I: Integer;
  S: string;
begin
  FThreadInfoList.Clear;
  FException.Clear;
  FModules.Clear;
  CSVFile := TCSVFile.Create;
  try
    CSVFile.LoadFromString(AInString);
    for I := 0 to CSVFile.Count - 1 do
    begin
      CSVRecord := CSVFile[I];
      if CSVRecord.Count > 1 then
      begin
        S := CSVRecord[0].Value;
        if S = 'ThreadInfo' then
          FThreadInfoList.LoadFromString(CSVRecord[1].Value)
        else
        if S = 'Exception' then
          FException.LoadFromString(CSVRecord[1].Value)
        else
        if S = 'Modules' then
          FModules.LoadFromString(CSVRecord[1].Value);
      end;
    end;
  finally
    CSVFile.Free;
  end;
end;

{ TException }

procedure TException.Clear;
begin
  FExceptionClassName := '';
  FExceptionMessage := '';
end;

procedure TException.LoadFromString(AInString: string);
var
  CSVFile: TCSVFile;
  CSVRecord: TCSVRecord;
begin
  Clear;
  CSVFile := TCSVFile.Create;
  try
    CSVFile.LoadFromString(AInString);
    if CSVFile.Count > 0 then
    begin
      CSVRecord := CSVFile[0];
      if CSVRecord.Count > 0 then
        FExceptionClassName := CSVRecord[0].Value;
      if CSVRecord.Count > 1 then
        FExceptionMessage := CSVRecord[1].Value;
    end;
  finally
    CSVFile.Free;
  end;
end;

{ TModuleList }

constructor TModuleList.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TModuleList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TModuleList.Clear;
begin
  FItems.Clear;
end;

function TModuleList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TModuleList.GetItems(AIndex: Integer): TModule;
begin
  Result := TModule(FItems[AIndex]);
end;

procedure TModuleList.LoadFromString(AInString: string);
var
  CSVFile: TCSVFile;
  CSVRecord: TCSVRecord;
  I: Integer;
  Module: TModule;
begin
  Clear;
  CSVFile := TCSVFile.Create;
  try
    CSVFile.LoadFromString(AInString);
    for I := 0 to CSVFile.Count - 1 do
    begin
      CSVRecord := CSVFile[I];
      if CSVRecord.Count > 0 then
      begin
        FItems.Add(TModule.Create);
        Module := TModule(FItems.Last);
        Module.HandleStr := CSVRecord[0].Value;
        if CSVRecord.Count > 1 then
          Module.ModuleName := CSVRecord[1].Value;
      end;
    end;
  finally
    CSVFile.Free;
  end;
end;

end.
