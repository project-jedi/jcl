[Files]
Source: isxdl.dll; DestDir: {tmp}; Flags: dontcopy

[Code]
procedure istool_AddFile(URL, Filename: PChar);
  external 'isxdl_AddFile@files:isxdl.dll stdcall';
function istool_DownloadFiles(hWnd: Integer): Integer;
   external 'isxdl_DownloadFiles@files:isxdl.dll stdcall';
procedure istool_ClearFiles;
  external 'isxdl_ClearFiles@files:isxdl.dll stdcall';

type
  ISXDL = record
    Source: string;
    DestDir: string;
    DestName: string;
    Tasks: string;
    Components: string;
    Languages: string;
    Flags: Cardinal;
  end;

var
  istool_files: array of ISXDL;

procedure istool_download_init();
begin
  SetArrayLength(istool_files, 1);
  // Delphi 5
  istool_files[0].Source     := 'http://jvcl.sourceforge.net/daily/JVCL3-Latest.zip';
  istool_files[0].DestDir    := ExpandConstant('{tmp}');
  istool_files[0].DestName   := 'JVCL3-Latest.zip';
  istool_files[0].Tasks      := '';
  istool_files[0].Components := 'IDE\Delphi5';
  istool_files[0].Languages  := '';
  istool_files[0].Flags      := 0;
end;

function istool_checklanguages(const Languages: String): Boolean;
begin
  Result := (Languages = '') or (Pos(ActiveLanguage, Languages) > 0);
end;

function istool_download(CurPage: Integer): boolean;
var
  i, NumFiles: Integer;
  DestFile, DestDir: string;
begin
  Result := True;
  if CurPage <> wpReady then
    Exit;
  istool_ClearFiles();
  istool_download_init();
  NumFiles := GetArrayLength(istool_files);
  for i := 0 to NumFiles - 1 do
  begin
    if IsComponentSelected(istool_files[i].Components) and
       IsTaskSelected(istool_files[i].Tasks) and
       istool_checklanguages(istool_files[i].Languages) then
    begin
      DestDir := AddBackslash(istool_files[i].DestDir);
      if istool_files[i].DestName = '' then
        DestFile := DestDir + 'dlfile.' + IntToStr(i)
      else
        DestFile := DestDir + istool_files[i].DestName;
      istool_AddFile(istool_files[i].Source, DestFile);
    end;
  end;
  //Result := 0 <> istool_DownloadFiles(StrToInt(ExpandConstant('{wizardhwnd}')));
  Result := 0 <> istool_DownloadFiles(0);
end;
