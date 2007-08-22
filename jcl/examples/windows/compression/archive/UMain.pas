unit UMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ActnList, ComCtrls, ImgList, JclCompression;

type
  TForm1 = class(TForm)
    ActionList1: TActionList;
    ActionOpen: TAction;
    ActionExtractSelected: TAction;
    ActionExtractAll: TAction;
    ActionNew: TAction;
    ActionAddFile: TAction;
    ActionAddDirectory: TAction;
    ActionSave: TAction;
    ListView1: TListView;
    OpenDialogArchive: TOpenDialog;
    SaveDialogArchive: TSaveDialog;
    OpenDialogFile: TOpenDialog;
    ProgressBar1: TProgressBar;
    PageControl1: TPageControl;
    TabSheetReadOnly: TTabSheet;
    TabSheetWriteOnly: TTabSheet;
    TabSheetReadWrite: TTabSheet;
    ButtonOpen: TButton;
    ButtonExtractSelected: TButton;
    ButtonExtractAll: TButton;
    ButtonNew: TButton;
    ButtonAddFile: TButton;
    ButtonAddDirectory: TButton;
    ButtonSave: TButton;
    ActionDelete: TAction;
    ActionNewRW: TAction;
    ActionOpenRW: TAction;
    ButtonNewRW: TButton;
    ButtonOpenRW: TButton;
    ButtonDeleteRW: TButton;
    ButtonAddFileRW: TButton;
    ButtonAddDirectoryRW: TButton;
    ButtonExtractSelectedRW: TButton;
    ButtonExtractAllRW: TButton;
    ButtonSaveRW: TButton;
    OpenDialogArchiveRW: TOpenDialog;
    procedure ActionAlwaysEnabled(Sender: TObject);
    procedure ActionExtractSelectedUpdate(Sender: TObject);
    procedure ActionExtractAllUpdate(Sender: TObject);
    procedure ActionAddFileUpdate(Sender: TObject);
    procedure ActionAddDirectoryUpdate(Sender: TObject);
    procedure ActionSaveUpdate(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionAddFileExecute(Sender: TObject);
    procedure ActionAddDirectoryExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure ActionExtractAllExecute(Sender: TObject);
    procedure ActionExtractSelectedExecute(Sender: TObject);
    procedure ActionDeleteUpdate(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionNewRWExecute(Sender: TObject);
    procedure ActionOpenRWExecute(Sender: TObject);
  private
    FArchive: TJclCompressionArchive;
    procedure CloseArchive;
    procedure ArchiveProgress(Sender: TObject; const Value, MaxValue: Int64);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  JclAnsiStrings, Sevenzip, FileCtrl;

function FileTimeToString(const FileTime: TFileTime): string;
var
  LocalFileTime: TFileTime;
  SystemTime: TSystemTime;
begin
  if FileTimeToLocalFileTime(FileTime, LocalFileTime)
    and FileTimeToSystemTime(LocalFileTime, SystemTime) then
    Result := DateTimeToStr(EncodeDate(SystemTime.wYear, SystemTime.wMonth, SystemTime.wDay)
      + EncodeTime(SystemTime.wHour, SystemTime.wMinute, SystemTime.wSecond, SystemTime.wMilliseconds))
  else
    Result := '';
end;

procedure TForm1.ActionAddDirectoryExecute(Sender: TObject);
var
  Directory: string;
begin
  if FileCtrl.SelectDirectory('Select directory', '', Directory, [sdNewUI], Self) then
  begin
    (FArchive as TJclCompressArchive).AddDirectory(ExtractFileName(Directory), Directory, True, True);
    ListView1.Items.BeginUpdate;
    try
      while ListView1.Items.Count < FArchive.ItemCount do
        ListView1.Items.Add;
    finally
      ListView1.Items.EndUpdate;
    end;
  end;
end;

procedure TForm1.ActionAddDirectoryUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FArchive is TJclCompressArchive) and FArchive.MultipleItemContainer;
end;

procedure TForm1.ActionAddFileExecute(Sender: TObject);
begin
  if OpenDialogFile.Execute then
  begin
    (FArchive as TJclCompressArchive).AddFile(ExtractFileName(OpenDialogFile.FileName), OpenDialogFile.FileName);
    ListView1.Items.Add;
  end;
end;

procedure TForm1.ActionAddFileUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FArchive is TJclCompressArchive)
    and (FArchive.MultipleItemContainer or (ListView1.Items.Count = 0));
end;

procedure TForm1.ActionAlwaysEnabled(Sender: TObject);
begin
  (Sender as TAction).Enabled := True;
end;

procedure TForm1.ActionDeleteExecute(Sender: TObject);
var
  Index: Integer;
begin
  for Index := ListView1.Items.Count - 1 downto 0 do
    if ListView1.Items[Index].Selected then
  begin
    (FArchive as TJclUpdateArchive).DeleteItem(Index);
    Break;
  end;

  ListView1.Items.Count := FArchive.ItemCount;
end;

procedure TForm1.ActionDeleteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FArchive is TJclUpdateArchive) and (ListView1.SelCount = 1);
end;

procedure TForm1.ActionExtractAllExecute(Sender: TObject);
var
  Directory: string;
begin
  if FileCtrl.SelectDirectory('Target directory', '', Directory, [sdNewUI], Self) then
  begin
    if FArchive is TJclDecompressArchive then
      TJclDecompressArchive(FArchive).ExtractAll(Directory, True)
    else
    if FArchive is TJclUpdateArchive then
      TJclUpdateArchive(FArchive).ExtractAll(Directory, True);
  end;
end;

procedure TForm1.ActionExtractAllUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FArchive is TJclDecompressArchive) or (FArchive is TJclUpdateArchive);
end;

procedure TForm1.ActionExtractSelectedExecute(Sender: TObject);
var
  Directory: string;
  Index: Integer;
begin
  if FileCtrl.SelectDirectory('Target directory', '', Directory, [sdNewUI], Self) then
  begin
    for Index := 0 to ListView1.Items.Count - 1 do
      FArchive.Items[Index].Selected := ListView1.Items.Item[Index].Selected;

    if FArchive is TJclDecompressArchive then
      TJclDecompressArchive(FArchive).ExtractSelected(Directory, True)
    else
    if FArchive is TJclUpdateArchive then
      TJclUpdateArchive(FArchive).ExtractSelected(Directory, True);
  end;
end;

procedure TForm1.ActionExtractSelectedUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ((FArchive is TJclDecompressArchive) or (FArchive is TJclUpdateArchive))
    and (ListView1.SelCount > 0);
end;

procedure TForm1.ActionNewExecute(Sender: TObject);
var
  ArchiveFileName, ArchiveExt, VolumeSizeStr, Password: string;
  VolumeSize: Int64;
  Code: Integer;
begin
  if SaveDialogArchive.Execute then
  begin
    CloseArchive;

    ArchiveFileName := SaveDialogArchive.FileName;
    VolumeSizeStr := '0';
    repeat
      if InputQuery('Split archive?', 'Volume size in byte:', VolumeSizeStr) then
        Val(VolumeSizeStr, VolumeSize, Code)
      else
      begin
        VolumeSize := 0;
        Code := 0;
      end;
    until Code = 0;

    InputQuery('Archive password', 'Value', Password);

    ArchiveExt := ExtractFileExt(ArchiveFileName);
    if VolumeSize <> 0 then
      ArchiveFileName := ArchiveFileName + '.%d';

    if AnsiSameText(ArchiveExt, '.zip') then
      FArchive := TJclZipCompressArchive.Create(ArchiveFileName, VolumeSize, VolumeSize <> 0)
    else
    if AnsiSameText(ArchiveExt, '.tar') then
      FArchive := TJclTarCompressArchive.Create(ArchiveFileName, VolumeSize, VolumeSize <> 0)
    else
    if AnsiSameText(ArchiveExt, '.7z') then
      FArchive := TJcl7zCompressArchive.Create(ArchiveFileName, VolumeSize, VolumeSize <> 0)
    else
    if AnsiSameText(ArchiveExt, '.bz2') then
      FArchive := TJclBZ2CompressArchive.Create(ArchiveFileName, VolumeSize, VolumeSize <> 0)
    else
    if AnsiSameText(ArchiveExt, '.gz') then
      FArchive := TJclGZipCompressArchive.Create(ArchiveFileName, VolumeSize, VolumeSize <> 0);

    if Assigned(FArchive) then
    begin
      FArchive.Password := Password;
      FArchive.OnProgress := ArchiveProgress;
    end;
  end;
end;

procedure TForm1.ActionNewRWExecute(Sender: TObject);
var
  ArchiveFileName, ArchiveExt, VolumeSizeStr, Password: string;
  VolumeSize: Int64;
  Code: Integer;
begin
  if SaveDialogArchive.Execute then
  begin
    CloseArchive;

    ArchiveFileName := SaveDialogArchive.FileName;
    VolumeSizeStr := '0';
    repeat
      if InputQuery('Split archive?', 'Volume size in byte:', VolumeSizeStr) then
        Val(VolumeSizeStr, VolumeSize, Code)
      else
      begin
        VolumeSize := 0;
        Code := 0;
      end;
    until Code = 0;

    InputQuery('Archive password', 'Value', Password);

    ArchiveExt := ExtractFileExt(ArchiveFileName);
    if VolumeSize <> 0 then
      ArchiveFileName := ArchiveFileName + '.%d';

    if AnsiSameText(ArchiveExt, '.zip') then
      FArchive := TJclZipUpdateArchive.Create(ArchiveFileName, VolumeSize, VolumeSize <> 0)
    else
    if AnsiSameText(ArchiveExt, '.tar') then
      FArchive := TJclTarUpdateArchive.Create(ArchiveFileName, VolumeSize, VolumeSize <> 0)
    else
    if AnsiSameText(ArchiveExt, '.7z') then
      FArchive := TJcl7zUpdateArchive.Create(ArchiveFileName, VolumeSize, VolumeSize <> 0)
    else
    if AnsiSameText(ArchiveExt, '.bz2') then
      FArchive := TJclBZ2UpdateArchive.Create(ArchiveFileName, VolumeSize, VolumeSize <> 0)
    else
    if AnsiSameText(ArchiveExt, '.gz') then
      FArchive := TJclGZipUpdateArchive.Create(ArchiveFileName, VolumeSize, VolumeSize <> 0);

    if Assigned(FArchive) then
    begin
      FArchive.Password := Password;
      FArchive.OnProgress := ArchiveProgress;
    end;
  end;
end;

procedure TForm1.ActionOpenExecute(Sender: TObject);
var
  ArchiveFileName, ArchiveFileExt, Password: string;
  SplitArchive: Boolean;
begin
  if OpenDialogArchive.Execute then
  begin
    CloseArchive;

    ArchiveFileName := OpenDialogArchive.FileName;
    ArchiveFileExt := ExtractFileExt(ArchiveFileName);
    SplitArchive := AnsiSameText(ArchiveFileExt, '.001');
    if SplitArchive then
    begin
      ArchiveFileName := ChangeFileExt(ArchiveFileName, '');
      ArchiveFileExt := ExtractFileExt(ArchiveFileName);
      ArchiveFileName := ArchiveFileName + '.%d';
    end;

    InputQuery('Archive password', 'Value', Password);

    if AnsiSameText(ArchiveFileExt, '.zip') then
      FArchive := TJclZipDecompressArchive.Create(ArchiveFileName, 0, SplitArchive)
    else
    if AnsiSameText(ArchiveFileExt, '.bz2') then
      FArchive := TJclBZ2DecompressArchive.Create(ArchiveFileName, 0, SplitArchive)
    else
    if AnsiSameText(ArchiveFileExt, '.rar') then
      FArchive := TJclRarDecompressArchive.Create(ArchiveFileName, 0, SplitArchive)
    else
    if AnsiSameText(ArchiveFileExt, '.arj') then
      FArchive := TJclArjDecompressArchive.Create(ArchiveFileName, 0, SplitArchive)
    else
    if AnsiSameText(ArchiveFileExt, '.z') then
      FArchive := TJclZDecompressArchive.Create(ArchiveFileName, 0, SplitArchive)
    else
    if AnsiSameText(ArchiveFileExt, '.lzh') then
      FArchive := TJclLzhDecompressArchive.Create(ArchiveFileName, 0, SplitArchive)
    else
    if AnsiSameText(ArchiveFileExt, '.7z') then
      FArchive := TJcl7zDecompressArchive.Create(ArchiveFileName, 0, SplitArchive)
    else
    if AnsiSameText(ArchiveFileExt, '.nsis') then
      FArchive := TJclNsisDecompressArchive.Create(ArchiveFileName, 0, SplitArchive)
    else
    if AnsiSameText(ArchiveFileExt, '.iso') then
      FArchive := TJclIsoDecompressArchive.Create(ArchiveFileName, 0, SplitArchive)
    else
    if AnsiSameText(ArchiveFileExt, '.cab') then
      FArchive := TJclCabDecompressArchive.Create(ArchiveFileName, 0, SplitArchive)
    else
    if AnsiSameText(ArchiveFileExt, '.chm') then
      FArchive := TJclChmDecompressArchive.Create(ArchiveFileName, 0, SplitArchive)
    else
    if AnsiSameText(ArchiveFileExt, '.rpm') then
      FArchive := TJclRpmDecompressArchive.Create(ArchiveFileName, 0, SplitArchive)
    else
    if AnsiSameText(ArchiveFileExt, '.deb') then
      FArchive := TJclDebDecompressArchive.Create(ArchiveFileName, 0, SplitArchive)
    else
    if AnsiSameText(ArchiveFileExt, '.cpio') then
      FArchive := TJclCpioDecompressArchive.Create(ArchiveFileName, 0, SplitArchive)
    else
    if AnsiSameText(ArchiveFileExt, '.tar') then
      FArchive := TJclTarDecompressArchive.Create(ArchiveFileName, 0, SplitArchive)
    else
    if AnsiSameText(ArchiveFileExt, '.gz') then
      FArchive := TJclGZipDecompressArchive.Create(ArchiveFileName, 0, SplitArchive);

    if Assigned(FArchive) then
    begin
      FArchive.Password := Password;
      FArchive.OnProgress := ArchiveProgress;

      if FArchive is TJclDecompressArchive then
        TJclDecompressArchive(FArchive).ListFiles
      else
      if FArchive is TJclUpdateArchive then
        TJclUpdateArchive(FArchive).ListFiles;

      ListView1.Items.BeginUpdate;
      try
        while ListView1.Items.Count < FArchive.ItemCount do
          ListView1.Items.Add;
      finally
        ListView1.Items.EndUpdate;
      end;
    end;
  end;
end;

procedure TForm1.ActionOpenRWExecute(Sender: TObject);
var
  ArchiveFileName, ArchiveFileExt, Password: string;
  SplitArchive: Boolean;
begin
  if OpenDialogArchiveRW.Execute then
  begin
    CloseArchive;

    ArchiveFileName := OpenDialogArchiveRW.FileName;
    ArchiveFileExt := ExtractFileExt(ArchiveFileName);
    SplitArchive := AnsiSameText(ArchiveFileExt, '.001');
    if SplitArchive then
    begin
      ArchiveFileName := ChangeFileExt(ArchiveFileName, '');
      ArchiveFileExt := ExtractFileExt(ArchiveFileName);
      ArchiveFileName := ArchiveFileName + '.%d';
    end;

    InputQuery('Archive password', 'Value', Password);

    if AnsiSameText(ArchiveFileExt, '.zip') then
      FArchive := TJclZipUpdateArchive.Create(ArchiveFileName, 0, SplitArchive)
    else
    if AnsiSameText(ArchiveFileExt, '.bz2') then
      FArchive := TJclBZ2UpdateArchive.Create(ArchiveFileName, 0, SplitArchive)
    else
    if AnsiSameText(ArchiveFileExt, '.7z') then
      FArchive := TJcl7zUpdateArchive.Create(ArchiveFileName, 0, SplitArchive)
    else
    if AnsiSameText(ArchiveFileExt, '.tar') then
      FArchive := TJclTarUpdateArchive.Create(ArchiveFileName, 0, SplitArchive)
    else
    if AnsiSameText(ArchiveFileExt, '.gz') then
      FArchive := TJclGZipUpdateArchive.Create(ArchiveFileName, 0, SplitArchive);

    if Assigned(FArchive) then
    begin
      FArchive.Password := Password;
      FArchive.OnProgress := ArchiveProgress;

      if FArchive is TJclDecompressArchive then
        TJclDecompressArchive(FArchive).ListFiles
      else
      if FArchive is TJclUpdateArchive then
        TJclUpdateArchive(FArchive).ListFiles;

      ListView1.Items.BeginUpdate;
      try
        while ListView1.Items.Count < FArchive.ItemCount do
          ListView1.Items.Add;
      finally
        ListView1.Items.EndUpdate;
      end;
    end;
  end;
end;

procedure TForm1.ActionSaveExecute(Sender: TObject);
begin
  (FArchive as TJclCompressArchive).Compress;
  CloseArchive;
end;

procedure TForm1.ActionSaveUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FArchive is TJclCompressArchive) and (ListView1.Items.Count > 0);
end;

procedure TForm1.ArchiveProgress(Sender: TObject; const Value, MaxValue: Int64);
var
  MyValue, MyMaxValue: Int64;
begin
  MyValue := Value;
  MyMaxValue := MaxValue;

  while MyMaxValue > High(Word) do
  begin
    MyMaxValue := MyMaxValue shr 8;
    MyValue := MyValue shr 8;
  end;
  ProgressBar1.Max := MyMaxValue;
  ProgressBar1.Position := MyValue;
end;

procedure TForm1.CloseArchive;
begin
  FreeAndNil(FArchive);
  ListView1.Items.Clear;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CloseArchive;
end;

procedure TForm1.ListView1Data(Sender: TObject; Item: TListItem);
var
  CompressionItem: TJclCompressionItem;
begin
  if not Assigned(FArchive) then
  begin
    Item.Caption := '';
    Item.SubItems.Clear;
    Exit;
  end;

  CompressionItem := FArchive.Items[Item.Index];

  Item.Caption := CompressionItem.FileName;
  Item.SubItems.Clear;
  Item.SubItems.Add(CompressionItem.PackedName);
  Item.SubItems.Add(IntToStr(CompressionItem.FileSize));
  Item.SubItems.Add(IntToStr(CompressionItem.PackedSize));
  Item.SubItems.Add(FileTimeToString(CompressionItem.CreationTime));
  Item.SubItems.Add(FileTimeToString(CompressionItem.LastAccessTime));
  Item.SubItems.Add(FileTimeToString(CompressionItem.LastWriteTime));
  Item.SubItems.Add(CompressionItem.Comment);
  Item.SubItems.Add(CompressionItem.HostOS);
  Item.SubItems.Add(CompressionItem.HostFS);
  Item.SubItems.Add(CompressionItem.User);
  Item.SubItems.Add(CompressionItem.Group);
  Item.SubItems.Add(IntToHex(CompressionItem.CRC, 8)); 
end;

initialization

  if not Load7Zip then
    raise EJclCompressionError.Create('Cannot load sevenzip library');

end.
