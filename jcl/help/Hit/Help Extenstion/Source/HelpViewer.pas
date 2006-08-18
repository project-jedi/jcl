{******************************************************************************}
{                                                       	               }
{ JEDI HIT - HtmlHelp Inclusion Tool                                             }
{                                                       	               }
{ 								               }
{ Portions created by Matthias Thoma are                                       }
{ Copyright (C) 2001 Matthias Thoma                                            }
{ 								               }
{ Portions created by Marcel van Brakel                                        }
{ Copyright (C) 2001 Marcel van Brakel                                         }
{ 								               }
{ Portions created by The Helpware Group                                       }
{ Copyright (C) 2001 The Helpware Group                                        }
{								               }
{ The Helpware Group consists of  Rob Chandler, Kurt Senfer                    }
{ and Marcel van Brakel.                                                       }
{ http://helpware.net                                                          }
{								               }
{ 								               }
{ Obtained through:                               	                       }
{ Joint Endeavour of Delphi Innovators (Project JEDI)                          }
{								               }
{ You may retrieve the latest version of this file at the Project              }
{ JEDI home page, located at http://delphi-jedi.org                            }
{								               }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html	                               }
{                                                                              }
{ Software distributed under the License is distributed on an 	               }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License. 			               }
{ 								               }
{******************************************************************************}
{ History:                                                                     }
{                                                                              }
{                                                                              }
{   07/15/01: Release Alpha 2                                                                            }
{   07/14/01: Added new compare Function                                       }
{   07/04/01: Code review, minor changes                                       }
{   06/30/01: Initial Release Alpha 1                                          }
{                                                                              }
{******************************************************************************}



unit HelpViewer;

interface

implementation
uses
  HelpIntfs, SysUtils, Classes, HtmlHlp, Registry,
  ActiveX, ComObj, Windows, Dialogs;


{ THHelpExt }

type
  THHelpExt = class(TInterfacedObject, ICustomHelpViewer)
  private
    FNotificationID: Integer;      // HelpManager Notification ID
    FHelpFiles: TStringList;       // Help Files Identifiers
    FHelpFileNames: TStringList;   // Help Filenames
    FIndexFileNames: TStringList;
    FHelpString: TStringList;
    FGetHelpString: Boolean;
    FHelpFile: String;
    FLink: THHAKLink;
    FNumKeywords: Integer;

  public
    FHelpManager: IHelpManager;
    function GetViewerName: string;
    procedure NotifyID(const ViewerID: Integer);
    procedure ShutDown;
    procedure SoftShutDown;
    function UnderstandsKeyword(const HelpString: String): Integer;
    function GetHelpStrings(const HelpString: String): TStringList;
    function KeywordSupported(const FileName, Keyword: String): Boolean;
    procedure ShowHelp(const HelpString: String);
    function CanShowTableOfContents: Boolean;
    procedure ShowTableOfContents;

    property NumKeywords: Integer read FNumKeywords write FNumKeywords;
    property HelpManager: IHelpManager read FHelpManager write FHelpManager;

    constructor Create;
    destructor Destroy; override;
  end;

var
  JEDICHMViewerExtension: THHelpExt;


{ TViewHelper }

{ TViewHelper is needed because the HelpManager shows no selection dialog      }
{ when UnderstandsKeyword > 2, but no other Helpviewer supports that particular}
{ keyword. In such cases TViewHelp forces a selection dialog.                  }

type
  TViewHelper = class(TInterfacedObject, ICustomHelpViewer)
  private
    FNotificationID: Integer;      // HelpManager Notification ID
    FHelpManager: IHelpManager;

    function GetViewerName: string;
    procedure NotifyID(const ViewerID: Integer);
    procedure ShutDown;
    procedure SoftShutDown;
    function UnderstandsKeyword(const HelpString: String): Integer;
    function GetHelpStrings(const HelpString: String): TStringList;
    procedure ShowHelp(const HelpString: String);
    function CanShowTableOfContents: Boolean;
    procedure ShowTableOfContents;
  end;

var
  JEDITViewHelper: TViewHelper;


{ IT Storage }

const
  CLSID_ITStorage: TGUID = (D1:$5d02926a; D2:$212e; D3:$11d0; D4:($9d,$f9,$00,$a0,$c9,$22,$e6,$ec));
  IID_ITStorage: TGUID = (D1:$88cc31de; D2:$27ab; D3:$11d0; D4:($9d,$f9,$00,$a0,$c9,$22,$e6,$ec));

type
  SNB = PWideChar; // from objidl.h

  TCompactionLev = (COMPACT_DATA, COMPACT_DATA_AND_PATH);

  PItsControlData = ^TItsControlData;
    _ITS_Control_Data = record
    cdwControlData: UINT;
    adwControlData: array [0..0] of UINT;
  end;
  TItsControlData = _ITS_Control_Data;

  IItsStorage = interface (IUnknown)
    function StgCreateDocFile(const pwcsName: PWideChar; grfMode: DWORD;
      reserved: DWORD; var ppstgOpen: IStorage): HRESULT; stdcall;
    function StgCreateDocFileOnILockBytes(plkbyt: ILockBytes; grfMode: DWORD;
      reserved: DWORD; var ppstgOpen: IStorage): HRESULT; stdcall;
    function StgIsStorageFile(const pwcsName: PWideChar): HRESULT; stdcall;
    function StgIsStorageILockBytes(plkbyt: ILockBytes): HRESULT; stdcall;
    function StgOpenStorage(const pwcsName: PWideChar; pstgPriority: IStorage;
      grfMode: DWORD; snbExclude: SNB; reserved: DWORD; var ppstgOpen: IStorage): HRESULT; stdcall;
    function StgOpenStorageOnILockBytes(plkbyt: ILockBytes; pStgPriority: IStorage;
      grfMode: DWORD; snbExclude: SNB; reserved: DWORD; var ppstgOpen: IStorage): HRESULT; stdcall;
    function StgSetTimes(const lpszName: PWideChar; const pctime, patime,
      pmtime: TFileTime): HRESULT; stdcall;
    function SetControlData(pControlData: PItsControlData): HRESULT; stdcall;
    function DefaultControlData(var ppControlData: PItsControlData): HRESULT; stdcall;
    function Compact(const pwcsName: PWideChar; iLev: TCompactionLev): HRESULT; stdcall;
  end;


{ Keyword Finder }


type
  TChmKeywordFinder = class (TObject)
  private
    FUrls: TStrings;
    FKeywords: TList;
    FKeywordStorage: IStorage;

  public
    constructor Create(const Path: string);
    destructor Destroy; override;
    function FindKeyword(const Keyword: string; var Topics: TStrings): Boolean;
    function FindFastKeyword(const Keyword: string; var Topics: TStrings): Boolean;
  end;

//------------------------------------------------------------------------------
// Small Comparisation Helper
//------------------------------------------------------------------------------

function HHCompareStr(s1, s2: string): Integer;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE or NORM_IGNORESYMBOLS,
    PChar(S1), Length(S1), PChar(S2), Length(S2))-2;
end;

//------------------------------------------------------------------------------
// JEDI HTMLHelp Viewer Extension
//------------------------------------------------------------------------------

function THHelpExt.GetViewerName: string;
begin
  Result := 'JHIT - JEDI HHTMLHelp Include Tool';
end;

//------------------------------------------------------------------------------

procedure THHelpExt.NotifyID(const ViewerID: Integer);
begin
  FNotificationID := ViewerID;
end;

//------------------------------------------------------------------------------

procedure THHelpExt.ShutDown;
begin
  if Assigned(FHelpFiles) then
    FHelpFiles.Free;

  if Assigned(FHelpFileNames) then
    FHelpFileNames.Free;

  if Assigned(FIndexFileNames) then
    FIndexFileNames.Free;

  if not(FGetHelpString) then
    FHelpString.Free;
end;

//------------------------------------------------------------------------------

procedure THHelpExt.SoftShutDown;
begin
  HtmlHelp(0, nil, HH_CLOSE_ALL, 0);
end;

//------------------------------------------------------------------------------

function THHelpExt.UnderstandsKeyword(const HelpString: String): Integer;
var
  i: Integer;

begin
  Result := 0;
  if not(FGetHelpString) then
    FHelpString.Free;  // Free when previously unused.

  FHelpString := TStringList.Create;
  FGetHelpString := false;

  for i := 0 to FHelpFileNames.Count - 1 do
  begin
     if KeywordSupported(FIndexFileNames.Strings[i], HelpString) then
     begin
       inc(Result);
       FHelpFile := FHelpFiles.Strings[i];
       FHelpString.Add(Format('(%s) %s',[FHelpFile,HelpString]));
     end;
  end;
  FNumkeywords := Result;
end;

//------------------------------------------------------------------------------

function THHelpExt.GetHelpStrings(const HelpString: String): TStringList;
begin
  FGetHelpString := True;
  Result := FHelpString;
end;

//------------------------------------------------------------------------------

procedure THHelpExt.ShowHelp(const HelpString: String);
var
  HelpFile: string;
  HelpStr: string;

  p1: Integer;
  p2: Integer;

begin
  HelpStr := HelpString;

  if FGetHelpString then
  begin
     P1 := Pos('(',HelpString);
     P2 := Pos(')',HelpString);

     if P2-P1 > 1 then
       FHelpFile := copy(HelpString,P1+1,P2-P1-1);
     HelpStr := Trim(copy(HelpString,P2+1,Length(HelpString)));
  end;

  HelpFile := FHelpFileNames.Strings[FHelpFiles.IndexOf(FHelpFile)];
  FLink.pszKeywords := PChar(HelpStr);

  HtmlHelp(FHelpManager.GetHandle,PChar(HelpFile),HH_KEYWORD_LOOKUP,
  Cardinal(@FLink));

  FNumkeywords := 0;
end;

//------------------------------------------------------------------------------

function THHelpExt.CanShowTableOfContents: Boolean;
begin
  Result := False;
end;

//------------------------------------------------------------------------------

procedure THHelpExt.ShowTableOfContents;
begin
  // Not supported yet
end;

//------------------------------------------------------------------------------

function THHelpExt.KeywordSupported(const FileName, Keyword: String): Boolean;
var
  fTopics: TStrings;
  FKeywords: TChmKeywordFinder;

begin
  FKeywords := TCHMKeywordFinder.Create(FileName);
  try
    fTopics := TStringList.Create;

    try
      if not FKeywords.FindFastKeyword(Keyword, fTopics) then
        Result := False
      else
        Result := True;
    finally
      fTopics.Free;
    end;
  finally
    FKeywords.Free;
  end;
end;

//------------------------------------------------------------------------------

constructor THHelpExt.Create;
var
  i: Integer;

begin
  inherited Create;

  FHelpFiles      := TStringList.Create;
  FHelpFileNames  := TStringList.Create;
  FIndexFileNames := TStringList.Create;
  FHelpString     := TStringList.Create;

  FillChar(FLink,sizeof(THHAKLink),#0);
  FLink.cbStruct := sizeof(THHAKLink);

  // Read the Registry Entries
  // Everything but not Failsafe at the moment!!!

  with TRegistry.Create do
  begin
    if OpenKey('Software\JEDI\JHIT\', False) then
    begin
    if HasSubkeys then
      begin
        GetKeyNames(FHelpFiles);
        CloseKey;

        for i := 0 to FHelpFiles.Count-1 do
        begin
          if OpenKey('Software\JEDI\JHIT\'+FHelpFiles.Strings[i],False) then
          begin
            FHelpFileNames.Add(ReadString('Path'));
            FIndexFileNames.Add(ReadString('Index File'));
            CloseKey;
          end
          else
          begin
            FIndexFileNames.Add('');
            FHelpFileNames.Add('');  // We have a problem
          end;
         end;
      end;
    end;

    Free;
  end;
end;

//------------------------------------------------------------------------------

destructor THHelpExt.Destroy;
begin
  inherited;
end;


//------------------------------------------------------------------------------
// JEDI HelpView
//------------------------------------------------------------------------------

function TViewHelper.GetViewerName: string;
begin
  Result := 'JEDI Viewer Helper';
end;

//------------------------------------------------------------------------------

procedure TViewHelper.NotifyID(const ViewerID: Integer);
begin
  FNotificationID := ViewerID;
end;

//------------------------------------------------------------------------------

procedure TViewHelper.ShutDown;
begin
end;

//------------------------------------------------------------------------------

procedure TViewHelper.SoftShutDown;
begin
end;

//------------------------------------------------------------------------------

function TViewHelper.UnderstandsKeyword(const HelpString: String): Integer;
begin
  If Assigned(JEDICHMViewerExtension) then
    Result := Integer(JEDICHMViewerExtension.NumKeywords > 1)
  else
    Result := 0;
end;

//------------------------------------------------------------------------------

function TViewHelper.GetHelpStrings(const HelpString: String): TStringList;
begin
  Result := TStringList.Create;
end;

//------------------------------------------------------------------------------

procedure TViewHelper.ShowHelp(const HelpString: String);
begin
  // Should never be called
end;

//------------------------------------------------------------------------------

function TViewHelper.CanShowTableOfContents: Boolean;
begin
  Result := False;
end;

//------------------------------------------------------------------------------

procedure TViewHelper.ShowTableOfContents;
begin
  // Empty
end;

//==============================================================================
// Utitity routines for reading and parsing ITS streams
//==============================================================================

function ItsReadStream(const Storage: IStorage; const Name: WideString; var Buffer: string): Integer;
var
  Stream: IStream;
  BytesRead: Longint;
  Stat: TStatStg;
begin
  OleCheck(Storage.OpenStream(PWideChar(@Name[1]), nil, STGM_READ or STGM_SHARE_DENY_WRITE, 0, Stream));
  try
    OleCheck(Stream.Stat(Stat, STATFLAG_NONAME));
    Result := Stat.cbSize;
    SetLength(Buffer, Result);
    OleCheck(Stream.Read(@Buffer[1], Result, @BytesRead));
    if BytesRead <> Result then raise EOleSysError.Create('', 0, 0);
  except
    on EOleSysError do
    begin
      Result := 0;
      repeat
        SetLength(Buffer, Result + 64);
        OleCheck(Stream.Read(@Buffer[Result + 1], 64, @BytesRead));
        Result := Result + BytesRead;
      until BytesRead <> 64;
      SetLength(Buffer, Result);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure ReadUrlStr(const Storage: IStorage; var UrlStr: TStrings);
var
  S, Buffer: string;
  P: PChar;
  I, L, TotalRead: Integer;
begin
  TotalRead := ItsReadStream(Storage, '#URLSTR', Buffer);
  P := @Buffer[10];
  I := 10;
  while I < TotalRead do
  begin
    S := P;
    UrlStr.Add(S);
    L := Length(S) + 9;
    Inc(P, L);
    Inc(I, L);
    while P^ = #0 do
    begin
      Inc(P);
      Inc(I);
    end;
  end;
end;

//------------------------------------------------------------------------------

type
  TIntegerArray  = array [0..536870908] of Integer;
  PIntegerArray  = ^TIntegerArray;

  PKeywordInfo = ^TKeywordInfo;
  TKeywordInfo = record
    Keyword: string;
    TopicCount: Integer;
    TopicIndices: PIntegerArray;
  end;

//------------------------------------------------------------------------------

procedure ReadKeywords(const Storage: IStorage; var Keywords: TList);
var
  Buffer, Page: string;
  P: PChar;
  PageIndex, PageCount: Integer;
  Keyword: WideString;
  KeywordCount: Integer;
  I, J, L: Integer;
  KeywordInfo: PKeywordInfo;
begin
  ItsReadStream(Storage, 'BTree', Buffer);
  PageCount := Byte(Pointer(Buffer[$1F]));

  for PageIndex := 0 to PageCount - 1 do
  begin
    Page := Copy(Buffer, $4D + (PageIndex * 2048), 2048);
    J := 0;
    KeywordCount := Byte(Pointer(Page[3]));
    P := @Page[13];
    I := 13;
    while (J < KeywordCount) and (I < 2048) do
    begin
      KeywordInfo := AllocMem(SizeOf(TKeywordInfo));
      Keywords.Add(KeywordInfo);
      Keyword := PWideChar(P);
      KeywordInfo^.Keyword := Keyword;
      L := (Length(Keyword) + 1) * 2;
      Inc(P, L + (3 * SizeOf(DWORD)));
      Inc(I, L + (3 * SizeOf(DWORD)));
      KeywordInfo^.TopicCount := DWORD(P^);
      Inc(P, 4);
      GetMem(KeywordInfo^.TopicIndices, KeywordInfo^.TopicCount * SizeOf(Integer));
      KeywordInfo^.TopicIndices^[0] := DWORD(P^);
      Inc(P, 2 * SizeOf(DWORD));
      Move(P^, KeywordInfo^.TopicIndices^[1], (KeywordInfo^.TopicCount - 1) * SizeOf(Integer));
      Inc(P, KeywordInfo^.TopicCount * SizeOf(DWORD));
      Inc(I, 4 + 2 * KeywordInfo^.TopicCount * SizeOf(DWORD));
      Inc(J);
    end;

    if Byte(Pointer(Page[9])) = 255 then
      Break;
  end;
end;

//------------------------------------------------------------------------------

function FFindKeyword(const Storage: IStorage; const SKeyword: String): Boolean;
var
  Buffer, Page, MidPage: string;
  P: PChar;
  PageCount: Integer;
  Keyword: WideString;
  KeywordCount: Integer;
  I, J, L: Integer;
  TopicCount: Integer;
  C: Integer;
  Min, Max, Mid: Integer;

begin
  Result := False;

  ItsReadStream(Storage, 'BTree', Buffer);
  PageCount := Byte(Pointer(Buffer[$1F]));
  Min := Max;

  if PageCount = 0 then
     Page := Copy(Buffer, $4D, 2048)
  else
  begin
    Min := 1;
    Max := PageCount - 1;

    while Min <= Max do
    begin
      Mid := (Min + Max) div 2;

      MidPage := Copy(Buffer, $4D + (Mid * 2048), 2048);
      P := @MidPage[13];
      Keyword := PWideChar(P);

      C := HHCompareStr(Keyword,SKeyword);

      if C = 0 then
      begin
        Result := True;
        Exit;
      end;

      if (C > 0) then
      begin
        Page := Copy(Buffer, $4D + ((Mid - 1) * 2048), 2048);
        P := @Page[13];
        Keyword := PWideChar(P);

        if HHCompareStr(Keyword,SKeyword) <= 0 then
          Break;

        Max := Mid - 1;
      end
      else
      begin
        if Mid + 1 < PageCount then
        begin
          Page := Copy(Buffer, $4D + ((Mid + 1) * 2048), 2048);
          P := @Page[13];
          Keyword := PWideChar(P);

          if HHCompareStr(Keyword,SKeyword) >= 0 then
          begin
            Page := MidPage;
            Break;
          end;
       end
       else
       begin
         Page := MidPage;
         Break;
       end;

        Min := Mid + 1;
      end;

      if Byte(Pointer(Page[9])) = 255 then
        Break;
    end;
  end;

  if Min > Max then
    Exit;

  J := 0;
  KeywordCount := Byte(Pointer(Page[3]));
  P := @Page[13];
  I := 13;

  while (J < KeywordCount) and (i<2048) do
  begin
    Keyword := PWideChar(P);

    C := HHCompareStr(Keyword,SKeyword);

    if C = 0 then
    begin
      Result := True;
      Exit;
    end;

    if (C > 0) then
      Exit;

    L := (Length(Keyword) + 1) * 2 + (3 * SizeOf(DWORD));
    Inc(P, L);
    Inc(I, L);
    TopicCount := 4 + 2 * SizeOf(DWORD) + DWORD(P^) * SizeOf(DWORD);
    Inc(P, TopicCount);
    Inc(I, TopicCount);
    Inc(J);
  end;
end;

//------------------------------------------------------------------------------

procedure ReleaseKeywords(var Keywords: TList);
var
  I: Integer;
  Keyword: PKeywordInfo;
begin
  if Keywords <> nil then
  begin
    for I := 0 to Keywords.Count - 1 do
    begin
      Keyword := PKeywordInfo(Keywords[I]);
      FreeMem(Keyword.TopicIndices);
      FreeMem(Keyword);
    end;
    Keywords.Clear;
  end;
end;

//==============================================================================
// TChmKeywordFinder
//==============================================================================

constructor TChmKeywordFinder.Create(const Path: string);
var
  ItsStorage: IItsStorage;
  Storage: IStorage;
  FileName: WideString;

begin
  inherited Create;

  FUrls := TStringList.Create;
  FKeywords := TList.Create;

  FileName := Path;
  OleCheck(CoCreateInstance(CLSID_ITStorage, nil, CLSCTX_INPROC_SERVER, IID_ITStorage, ItsStorage));
  OleCheck(ItsStorage.StgOpenStorage(PWideChar(@Filename[1]), nil, STGM_READ or STGM_SHARE_DENY_WRITE, nil, 0, Storage));
  ReadUrlStr(Storage, FUrls);
  FileName := '$WWKeywordLinks';
  OleCheck(Storage.OpenStorage(@FileName[1], nil, STGM_READ or STGM_SHARE_DENY_WRITE, nil, 0, FKeywordStorage));
end;

//------------------------------------------------------------------------------

destructor TChmKeywordFinder.Destroy;
begin
  FreeAndNil(FUrls);
  ReleaseKeywords(FKeywords);
  FreeAndNil(FKeywords);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TChmKeywordFinder.FindKeyword(const Keyword: string; var Topics: TStrings): Boolean;
var
  KeywordInfo: PKeywordInfo;
  Comp, Min, Max, Mid: Integer;
begin
  Min := 0;
  Max := FKeywords.Count - 1;
  Result := False;

  while Min <= Max do
  begin
    Mid := (Max + Min) div 2;
    KeywordInfo := PKeywordInfo(FKeywords[Mid]);
    Comp := StrIComp(PChar(Keyword), PChar(KeywordInfo^.Keyword));

    if Comp = 0 then
    begin
      Result := True;
      Break;
    end;

    if Comp < 0 then
      Max := Mid - 1
    else
      Min := Mid + 1;
  end;
end;

//------------------------------------------------------------------------------

function TChmKeywordFinder.FindFastKeyword(const Keyword: string; var Topics: TStrings): Boolean;
begin
  Result := FFindKeyword(FKeywordStorage,Keyword);
end;


initialization
  { Install the Main Viewer Extension }
  JEDICHMViewerExtension := THHelpExt.Create;
  HelpIntfs.RegisterViewer(JEDICHMViewerExtension, JEDICHMViewerExtension.FHelpManager);

  { Install the Helper }
  JEDITViewHelper := TViewHelper.Create;
  HelpIntfs.RegisterViewer(JEDITViewHelper, JEDITViewHelper.FHelpManager);


finalization
  JEDITViewHelper.SoftShutDown;
  JEDICHMViewerExtension.SoftShutDown;

  if Assigned(JEDITViewHelper.FHelpManager) then
  begin
    JEDITViewHelper.FHelpManager.Release(JEDITViewHelper.FNotificationID);
    JEDITViewHelper.FHelpManager := nil;
  end;

  if Assigned(JEDICHMViewerExtension.FHelpManager) then
  begin
    JEDICHMViewerExtension.FHelpManager.Release(JEDICHMViewerExtension.FNotificationID);
    JEDICHMViewerExtension.FHelpManager := nil;
  end;
end.


