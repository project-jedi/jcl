{******************************************************************************}
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.0 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JclLocales.pas.                                         }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{ Last modified: December 12, 2000                                             }
{                                                                              }
{******************************************************************************}

unit JclLocales;

{$I JCL.INC}

interface

uses
  Windows, Classes,
  {$IFDEF DELPHI5_UP}
  Contnrs,
  {$ENDIF DELPHI5_UP}
  JclBase;

//------------------------------------------------------------------------------
// Missing translations
//------------------------------------------------------------------------------

function LANGIDFROMLCID(const lcid: LCID): Word;
function MAKELANGID(const usPrimaryLanguage, usSubLanguage: Byte): Word;
function PRIMARYLANGID(const lgid: Word): Word;
function SUBLANGID(const lgid: Word): Word;
function MAKELCID(const wLanguageID, wSortID: Word): LCID;
function SORTIDFROMLCID(const lcid: LCID): Word;

//------------------------------------------------------------------------------
// System locales
//------------------------------------------------------------------------------

type
  TJclLocalesDays = 1..7;
  TJclLocalesMonths = 1..13;
  TJclLocaleDateFormats = (ldShort, ldLong);

  TJclLocaleInfo = class (TObject)
  private
    FDateFormats: TStringList;
    FLastDateFormat: TJclLocaleDateFormats;
    FLocaleID: LCID;
    FTimeFormats: TStringList;
    FUseSystemACP: Boolean;
    function GetIntegerInfo(InfoType: Integer): Integer;
    function GetStringInfo(InfoType: Integer): string;
    function GetLangID: LANGID;
    function GetSortID: Word;
    function GetLangIDPrimary: Word;
    function GetLangIDSub: Word;
    function GetLongMonthNames(Month: TJclLocalesMonths): string;
    function GetAbbreviatedMonthNames(Month: TJclLocalesMonths): string;
    function GetLongDayNames(Day: TJclLocalesDays): string;
    function GetAbbreviatedDayNames(Day: TJclLocalesDays): string;
    function GetCharInfo(InfoType: Integer): Char;
    function GetTimeFormats: TStrings;
    function GetDateFormats(Format: TJclLocaleDateFormats): TStrings;
    function GetFontCharset: Byte;
    procedure SetUseSystemACP(const Value: Boolean);
  protected
    class procedure ClearOrCreateStringList(var Strings: TStringList);
  public
    constructor Create(ALocaleID: LCID {$IFDEF SUPPORTS_DEFAULTPARAMS} = LOCALE_SYSTEM_DEFAULT {$ENDIF});
    destructor Destroy; override;
    property CharInfo[InfoType: Integer]: Char read GetCharInfo;
    property IntegerInfo[InfoType: Integer]: Integer read GetIntegerInfo;
    property StringInfo[InfoType: Integer]: string read GetStringInfo; default;
    property FontCharset: Byte read GetFontCharset;
    property LangID: LANGID read GetLangID;
    property LocaleID: LCID read FLocaleID;
    property LangIDPrimary: Word read GetLangIDPrimary;
    property LangIDSub: Word read GetLangIDSub;
    property SortID: Word read GetSortID;
    property UseSystemACP: Boolean read FUseSystemACP write SetUseSystemACP;
    property AbbreviatedCountryName: string index LOCALE_SABBREVCTRYNAME read GetStringInfo;
    property AbbreviatedDayNames[Day: TJclLocalesDays]: string read GetAbbreviatedDayNames;
    property AbbreviatedLangName: string index LOCALE_SABBREVLANGNAME read GetStringInfo;
    property AbbreviatedMonthNames[Month: TJclLocalesMonths]: string read GetAbbreviatedMonthNames;
    property CodePageANSI: Integer index LOCALE_IDEFAULTANSICODEPAGE read GetIntegerInfo;
    property CodePageMAC: Integer index LOCALE_IDEFAULTMACCODEPAGE read GetIntegerInfo;
    property CodePageOEM: Integer index LOCALE_IDEFAULTCODEPAGE read GetIntegerInfo;
    property CountryCode: Integer index LOCALE_IDEFAULTCOUNTRY read GetIntegerInfo;
    property DateFormats[Format: TJclLocaleDateFormats]: TStrings read GetDateFormats;
    property DecimalSeparator: Char index LOCALE_SDECIMAL read GetCharInfo;
    property EnglishCountryName: string index LOCALE_SENGCOUNTRY read GetStringInfo;
    property EnglishLangName: string index LOCALE_SENGLANGUAGE read GetStringInfo;
    property LangIdStr: string index LOCALE_ILANGUAGE read GetStringInfo;
    property ListItemSeparator: Char index LOCALE_SLIST read GetCharInfo;
    property LocalizedCountryName: string index LOCALE_SCOUNTRY read GetStringInfo;
    property LocalizedLangName: string index LOCALE_SLANGUAGE read GetStringInfo;
    property LongDayNames[Day: TJclLocalesDays]: string read GetLongDayNames;
    property LongMonthNames[Month: TJclLocalesMonths]: string read GetLongMonthNames;
    property MonetarySymbolIntl: string index LOCALE_SINTLSYMBOL read GetStringInfo;
    property MonetarySymbolLocal: string index LOCALE_SCURRENCY read GetStringInfo;
    property NativeCountryName: string index LOCALE_SNATIVECTRYNAME read GetStringInfo;
    property NegativeNumber: Char index LOCALE_INEGNUMBER read GetCharInfo;
    property TimeFormats: TStrings read GetTimeFormats;
    property ThousandSeparator: Char index LOCALE_STHOUSAND read GetCharInfo;
  end;

  TJclLocalesKind = (lkInstalled, lkSupported);

  TJclLocalesList = class (TObjectList)
  private
    FCodePages: TStrings;
    FKind: TJclLocalesKind;
    function GetItemFromLangID(LangID: LANGID): TJclLocaleInfo;
    function GetItemFromLangIDPrimary(LangIDPrimary: Word): TJclLocaleInfo;
    function GetItemFromLocaleID(LocaleID: LCID): TJclLocaleInfo;
    function GetItems(Index: Integer): TJclLocaleInfo;
  protected
    procedure CreateList;
  public
    constructor Create(AKind: TJclLocalesKind {$IFDEF SUPPORTS_DEFAULTPARAMS} = lkSupported {$ENDIF});
    destructor Destroy; override;
    property CodePages: TStrings read FCodePages;
    property ItemFromLangID[LangID: LANGID]: TJclLocaleInfo read GetItemFromLangID;
    property ItemFromLangIDPrimary[LangIDPrimary: Word]: TJclLocaleInfo read GetItemFromLangIDPrimary;
    property ItemFromLocaleID[LocaleID: LCID]: TJclLocaleInfo read GetItemFromLocaleID;
    property Items[Index: Integer]: TJclLocaleInfo read GetItems; default;
    property Kind: TJclLocalesKind read FKind;
  end;

//------------------------------------------------------------------------------
// Keyboard layouts
//------------------------------------------------------------------------------

  TJclKeybLayoutFlags = set of (klReorder, klUnloadPrevious, klSetForProcess,
    klActivate, klNotEllShell, klReplaceLang, klSubstituteOK);

  TJclKeyboardLayoutList = class;

  TJclKeyboardLayout = class (TObject)
  private
    FLayout: HKL;
    FLocaleInfo: TJclLocaleInfo;
    FOwner: TJclKeyboardLayoutList;
    function GetDeviceHandle: Word;
    function GetLocaleID: Word;
  public
    constructor Create(AOwner: TJclKeyboardLayoutList; ALayout: HKL);
    destructor Destroy; override;
    function Activate(ActivateFlags: TJclKeybLayoutFlags {$IFDEF SUPPORTS_DEFAULTPARAMS} = [] {$ENDIF}): Boolean;
    function Unload: Boolean;
    property DeviceHandle: Word read GetDeviceHandle;
    property Layout: HKL read FLayout;
    property LocaleID: Word read GetLocaleID;
    property LocaleInfo: TJclLocaleInfo read FLocaleInfo;
  end;

  TJclKeyboardLayoutList = class (TObject)
  private
    FList: TObjectList;
    FOnRefresh: TNotifyEvent;
    function GetCount: Integer;
    function GetItems(Index: Integer): TJclKeyboardLayout;
    function GetActiveLayout: TJclKeyboardLayout;
    function GetItemFromHKL(Layout: HKL): TJclKeyboardLayout;
    function GetLayoutFromLocaleID(LocaleID: Word): TJclKeyboardLayout;
  protected
    procedure DoRefresh; dynamic;
  public
    constructor Create;
    destructor Destroy; override;
    function ActivatePrevLayout(ActivateFlags: TJclKeybLayoutFlags {$IFDEF SUPPORTS_DEFAULTPARAMS} = [] {$ENDIF}): Boolean;
    function ActivateNextLayout(ActivateFlags: TJclKeybLayoutFlags {$IFDEF SUPPORTS_DEFAULTPARAMS} = [] {$ENDIF}): Boolean;
    function LoadLayout(const LayoutName: string; LoadFlags: TJclKeybLayoutFlags): Boolean;
    procedure Refresh;
    property ActiveLayout: TJclKeyboardLayout read GetActiveLayout;
    property Count: Integer read GetCount;
    property ItemFromHKL[Layout: HKL]: TJclKeyboardLayout read GetItemFromHKL;
    property Items[Index: Integer]: TJclKeyboardLayout read GetItems; default;
    property LayoutFromLocaleID[LocaleID: Word]: TJclKeyboardLayout read GetLayoutFromLocaleID;
    property OnRefresh: TNotifyEvent read FOnRefresh write FOnRefresh;
  end;

function JclKeyboardLayoutList: TJclKeyboardLayoutList;
function JclLocalesList: TJclLocalesList;

implementation

uses
  SysUtils,
  JclStrings, JclSysInfo, JclSysUtils;

const
  JclMaxKeyboardLayouts = 16;
  LocaleUseAcp: array [Boolean] of DWORD = (0, LOCALE_USE_CP_ACP);
{ TODO -cMOVE : Move the constant to JclWin32 }
  KLF_SETFORPROCESS = $00000100;

var
  GlobalKeyboardLayoutList: TJclKeyboardLayoutList;
  GlobalLocalesList: TJclLocalesList;

//------------------------------------------------------------------------------

function LANGIDFROMLCID(const lcid: LCID): Word;
begin
  Result := Word(lcid);
end;

//------------------------------------------------------------------------------

function MAKELANGID(const usPrimaryLanguage, usSubLanguage: Byte): Word;
begin
  Result := usPrimaryLanguage or (usSubLanguage shl 10);
end;

//------------------------------------------------------------------------------

function PRIMARYLANGID(const lgid: Word): Word;
begin
  Result := (lgid and $03FF);
end;

//------------------------------------------------------------------------------

function SUBLANGID(const lgid: Word): Word;
begin
  Result := (lgid shr 10);
end;

//------------------------------------------------------------------------------

function MAKELCID(const wLanguageID, wSortID: Word): LCID;
begin
  Result := wLanguageID or (wSortID shl 16);
end;

//------------------------------------------------------------------------------

function SORTIDFROMLCID(const lcid: LCID): Word;
begin
  Result := (lcid shl 16) and $0F;
end;

//------------------------------------------------------------------------------

function KeybLayoutFlagsToDWORD(const ActivateFlags: TJclKeybLayoutFlags;
  const LoadMode: Boolean): DWORD;
begin
  Result := 0;
  if klReorder in ActivateFlags then
    Inc(Result, KLF_REORDER);
  if (klUnloadPrevious in ActivateFlags) and IsWinNT then
    Inc(Result, KLF_UNLOADPREVIOUS);
  if (klSetForProcess in ActivateFlags) and IsWin2K then
    Inc(Result, KLF_SETFORPROCESS);
  if LoadMode then
  begin
    if klActivate in ActivateFlags then
      Inc(Result, KLF_ACTIVATE);
    if klNotEllShell in ActivateFlags then
      Inc(Result, KLF_NOTELLSHELL);
    if (klReplaceLang in ActivateFlags) and not IsWinNT3 then
      Inc(Result, KLF_REPLACELANG);
    if klSubstituteOK in ActivateFlags then
      Inc(Result, KLF_SUBSTITUTE_OK);
  end;
end;

//------------------------------------------------------------------------------

function JclKeyboardLayoutList: TJclKeyboardLayoutList;
begin
  if GlobalKeyboardLayoutList = nil then
    GlobalKeyboardLayoutList := TJclKeyboardLayoutList.Create;
  Result := GlobalKeyboardLayoutList;
end;

//------------------------------------------------------------------------------

function JclLocalesList: TJclLocalesList;
begin
  if GlobalLocalesList = nil then
    GlobalLocalesList := TJclLocalesList.Create(lkSupported);
  Result := GlobalLocalesList;
end;

//==============================================================================
// EnumXXX functions helper thread variables
//==============================================================================

threadvar
  ProcessedLocaleInfo: TJclLocaleInfo;
  ProcessedLocalesList: TJclLocalesList;

//==============================================================================
// TJclLocaleInfo
//==============================================================================

class procedure TJclLocaleInfo.ClearOrCreateStringList(var Strings: TStringList);
begin
  if Strings <> nil then
    Strings.Clear
  else
    Strings := TStringList.Create;
end;

//------------------------------------------------------------------------------

constructor TJclLocaleInfo.Create(ALocaleID: LCID);
begin
  inherited Create;
  FLocaleID := ALocaleID;
  FUseSystemACP := True;
end;

//------------------------------------------------------------------------------

destructor TJclLocaleInfo.Destroy;
begin
  FreeAndNil(FDateFormats);
  FreeAndNil(FTimeFormats);
  inherited;
end;

//------------------------------------------------------------------------------

function TJclLocaleInfo.GetAbbreviatedDayNames(Day: TJclLocalesDays): string;
begin
  Result := GetStringInfo(LOCALE_SABBREVDAYNAME1 + Day - 1);
end;

//------------------------------------------------------------------------------

function TJclLocaleInfo.GetAbbreviatedMonthNames(Month: TJclLocalesMonths): string;
var
  Param: DWORD;
begin
  if Month = 13 then
    Param := LOCALE_SABBREVMONTHNAME13
  else
    Param := LOCALE_SABBREVMONTHNAME1 + Month - 1;
  Result := GetStringInfo(Param);
end;

//------------------------------------------------------------------------------

function TJclLocaleInfo.GetCharInfo(InfoType: Integer): Char;
var
  S: string;
begin
  S := GetStringInfo(InfoType);
  if Length(S) >= 1 then
    Result := S[1]
  else
    Result := ' ';
end;

//------------------------------------------------------------------------------

function TJclLocaleInfo.GetDateFormats(Format: TJclLocaleDateFormats): TStrings;
const
  DateFormats: array [TJclLocaleDateFormats] of DWORD =
    (DATE_SHORTDATE, DATE_LONGDATE);

  function EnumDateFormatsProc(lpDateFormatString: LPSTR): BOOL; stdcall;
  begin
    ProcessedLocaleInfo.FDateFormats.Add(lpDateFormatString);
    DWORD(Result) := 1;
  end;

begin
  if (FDateFormats = nil) or (Format <> FLastDateFormat) then
  begin
    ClearOrCreateStringList(FDateFormats);
    ProcessedLocaleInfo := Self;
    try
      EnumDateFormats(@EnumDateFormatsProc, FLocaleID, DateFormats[Format] or
        LocaleUseAcp[FUseSystemACP]);
      FLastDateFormat := Format;
    finally
      ProcessedLocaleInfo := nil;
    end;
  end;
  Result := FDateFormats;
end;

//------------------------------------------------------------------------------

// TODO : Is there any better way how to get font charset for particular locale ?

function TJclLocaleInfo.GetFontCharset: Byte;
const
  CharsetTable: array [1..10] of record
    CodePage: Word;
    Charset: Byte;
  end = (
   (CodePage: 1252; Charset: ANSI_CHARSET),
   (CodePage: 1250; Charset: EASTEUROPE_CHARSET),
   (CodePage: 1251; Charset: RUSSIAN_CHARSET),
   (CodePage: 1253; Charset: GREEK_CHARSET),
   (CodePage: 1254; Charset: TURKISH_CHARSET),
   (CodePage: 1255; Charset: HEBREW_CHARSET),
   (CodePage: 1256; Charset: ARABIC_CHARSET),
   (CodePage: 1257; Charset: BALTIC_CHARSET),
   (CodePage: 874; Charset: THAI_CHARSET),
   (CodePage: 932; Charset: SHIFTJIS_CHARSET)
  );
var
  I, CpANSI: Integer;
begin
  Result := DEFAULT_CHARSET;
  CpANSI := CodePageANSI;
  for I := Low(CharsetTable) to High(CharsetTable) do
    if CharsetTable[I].CodePage = CpANSI then
    begin
      Result := CharsetTable[I].Charset;
      Break;
    end;
end;

//------------------------------------------------------------------------------

function TJclLocaleInfo.GetIntegerInfo(InfoType: Integer): Integer;
begin
  Result := StrToIntDef(GetStringInfo(InfoType), 0);
end;

//------------------------------------------------------------------------------

function TJclLocaleInfo.GetLangID: LANGID;
begin
  Result := LANGIDFROMLCID(FLocaleID);
end;

//------------------------------------------------------------------------------

function TJclLocaleInfo.GetLangIDPrimary: Word;
begin
  Result := PRIMARYLANGID(LangID);
end;

//------------------------------------------------------------------------------

function TJclLocaleInfo.GetLangIDSub: Word;
begin
  Result := SUBLANGID(LangID);
end;

//------------------------------------------------------------------------------

function TJclLocaleInfo.GetLongDayNames(Day: TJclLocalesDays): string;
begin
  Result := GetStringInfo(LOCALE_SDAYNAME1 + Day - 1);
end;

//------------------------------------------------------------------------------

function TJclLocaleInfo.GetLongMonthNames(Month: TJclLocalesMonths): string;
var
  Param: DWORD;
begin
  if Month = 13 then
    Param := LOCALE_SMONTHNAME13
  else
    Param := LOCALE_SMONTHNAME1 + Month - 1;
  Result := GetStringInfo(Param);
end;

//------------------------------------------------------------------------------

function TJclLocaleInfo.GetSortID: Word;
begin
  Result := SORTIDFROMLCID(FLocaleID);
end;

//------------------------------------------------------------------------------

function TJclLocaleInfo.GetStringInfo(InfoType: Integer): string;
var
  Res: Integer;
  W: PWideChar;
begin
  InfoType := InfoType or Integer(LocaleUseAcp[FUseSystemACP]);
  if IsWinNT then // TODO : Needs to be tested on NT !
  begin
    Res := GetLocaleInfoW(FLocaleID, InfoType, nil, 0);
    if Res > 0 then
    begin
      GetMem(W, Res * SizeOf(WideChar));
      Res := GetLocaleInfoW(FLocaleID, InfoType, W, Res);
      Result := WideCharToString(W);
      FreeMem(W);
    end;
  end
  else
  begin
    Res := GetLocaleInfoA(FLocaleID, InfoType, nil, 0);
    if Res > 0 then
    begin
      SetString(Result, nil, Res);
      Res := GetLocaleInfoA(FLocaleID, InfoType, PChar(Result), Res);
      StrResetLength(Result);
      // Note: GetLocaleInfo returns sometimes incorrect length of string on
      // Win95 (usually plus 1), that's why StrResetLength is called.
    end;
  end;
  if Res = 0 then
    Result := '';
end;

//------------------------------------------------------------------------------

function TJclLocaleInfo.GetTimeFormats: TStrings;

  function EnumTimeFormatsProc(lpTimeFormatString: LPSTR): BOOL; stdcall;
  begin
    ProcessedLocaleInfo.FTimeFormats.Add(lpTimeFormatString);
    DWORD(Result) := 1;
  end;

begin
  if FTimeFormats = nil then
  begin
    FTimeFormats := TStringList.Create;
    ProcessedLocaleInfo := Self;
    try
      EnumTimeFormats(@EnumTimeFormatsProc, FLocaleID, LocaleUseAcp[FUseSystemACP]);
    finally
      ProcessedLocaleInfo := nil;
    end;
  end;
  Result := FTimeFormats;
end;

//------------------------------------------------------------------------------

procedure TJclLocaleInfo.SetUseSystemACP(const Value: Boolean);
begin
  if FUseSystemACP <> Value then
  begin
    FUseSystemACP := Value;
    FreeAndNil(FDateFormats);
    FreeAndNil(FTimeFormats);
  end;
end;


//==============================================================================
// TJclLocalesList
//==============================================================================

constructor TJclLocalesList.Create(AKind: TJclLocalesKind);
begin
  inherited Create(True);
  FCodePages := TStringList.Create;
  FKind := AKind;
  CreateList;
end;

//------------------------------------------------------------------------------

procedure TJclLocalesList.CreateList;
const
  Flags: array [TJclLocalesKind] of DWORD = (LCID_INSTALLED, LCID_SUPPORTED);

  function EnumLocalesProc(lpLocaleString: LPSTR): BOOL; stdcall;
  var
    LocaleID: LCID;
  begin
    LocaleID := StrToIntDef('$' + Copy(lpLocaleString, 5, 4), 0);
    if LocaleID > 0 then
      ProcessedLocalesList.Add(TJclLocaleInfo.Create(LocaleID));
    DWORD(Result) := 1;
  end;

  function EnumCodePagesProc(lpCodePageString: LPSTR): BOOL; stdcall;
  begin
    ProcessedLocalesList.FCodePages.AddObject(lpCodePageString, Pointer(StrToIntDef(lpCodePageString, 0)));
    DWORD(Result) := 1;
  end;

begin
  ProcessedLocalesList := Self;
  try
    Win32Check(EnumSystemLocales(@EnumLocalesProc, Flags[FKind]));
    Win32Check(EnumSystemCodePages(@EnumCodePagesProc, Flags[FKind]));
  finally
    ProcessedLocalesList := nil;
  end;
end;

//------------------------------------------------------------------------------

destructor TJclLocalesList.Destroy;
begin
  FreeAndNil(FCodePages);
  inherited;
end;

//------------------------------------------------------------------------------

function TJclLocalesList.GetItemFromLangID(LangID: LANGID): TJclLocaleInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].LangID = LangID then
    begin
      Result := Items[I];
      Break;
    end;
end;

//------------------------------------------------------------------------------

function TJclLocalesList.GetItemFromLangIDPrimary(LangIDPrimary: Word): TJclLocaleInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].LangIDPrimary = LangIDPrimary then
    begin
      Result := Items[I];
      Break;
    end;
end;

//------------------------------------------------------------------------------

function TJclLocalesList.GetItemFromLocaleID(LocaleID: LCID): TJclLocaleInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].LocaleID = LocaleID then
    begin
      Result := Items[I];
      Break;
    end;
end;

//------------------------------------------------------------------------------

function TJclLocalesList.GetItems(Index: Integer): TJclLocaleInfo;
begin
  Result := TJclLocaleInfo(inherited Items[Index]);
end;

//==============================================================================
// TJclKeyboardLayout
//==============================================================================

function TJclKeyboardLayout.Activate(ActivateFlags: TJclKeybLayoutFlags): Boolean;
begin
  Result := ActivateKeyboardLayout(FLayout, KeybLayoutFlagsToDWORD(ActivateFlags, False)) <> 0;
end;

//------------------------------------------------------------------------------

constructor TJclKeyboardLayout.Create(AOwner: TJclKeyboardLayoutList; ALayout: HKL);
begin
  inherited Create;
  FLayout := ALayout;
  FOwner := AOwner;
  FLocaleInfo := TJclLocaleInfo.Create(MAKELCID(GetLocaleID, SORT_DEFAULT));
end;

//------------------------------------------------------------------------------

destructor TJclKeyboardLayout.Destroy;
begin
  FreeAndNil(FLocaleInfo);
  inherited;
end;

//------------------------------------------------------------------------------

function TJclKeyboardLayout.GetDeviceHandle: Word;
begin
  Result := HiWord(FLayout);
end;

//------------------------------------------------------------------------------

function TJclKeyboardLayout.GetLocaleID: Word;
begin
  Result := LoWord(FLayout);
end;

//------------------------------------------------------------------------------

function TJclKeyboardLayout.Unload: Boolean;
begin
  Result := UnloadKeyboardLayout(FLayout);
  if Result then
    FOwner.Refresh;
end;

//==============================================================================
// TJclKeyboardLayoutList
//==============================================================================

function TJclKeyboardLayoutList.ActivateNextLayout(
  ActivateFlags: TJclKeybLayoutFlags): Boolean;
begin
  Result := ActivateKeyboardLayout(HKL_NEXT, KeybLayoutFlagsToDWORD(ActivateFlags, False)) <> 0;
end;

//------------------------------------------------------------------------------

function TJclKeyboardLayoutList.ActivatePrevLayout(
  ActivateFlags: TJclKeybLayoutFlags): Boolean;
begin
  Result := ActivateKeyboardLayout(HKL_PREV, KeybLayoutFlagsToDWORD(ActivateFlags, False)) <> 0;
end;

//------------------------------------------------------------------------------

constructor TJclKeyboardLayoutList.Create;
begin
  inherited Create;
  FList := TObjectList.Create(True);
  Refresh;
end;

//------------------------------------------------------------------------------

destructor TJclKeyboardLayoutList.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

//------------------------------------------------------------------------------

procedure TJclKeyboardLayoutList.DoRefresh;
begin
  if Assigned(FOnRefresh) then
    FOnRefresh(Self);
end;

//------------------------------------------------------------------------------

function TJclKeyboardLayoutList.GetActiveLayout: TJclKeyboardLayout;
begin
  Result := ItemFromHKL[GetKeyboardLayout(0)];
end;

//------------------------------------------------------------------------------

function TJclKeyboardLayoutList.GetCount: Integer;
begin
  Result := FList.Count;
end;

//------------------------------------------------------------------------------

function TJclKeyboardLayoutList.GetItemFromHKL(Layout: HKL): TJclKeyboardLayout;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].Layout = Layout then
    begin
      Result := Items[I];
      Break;
    end;
end;

//------------------------------------------------------------------------------

function TJclKeyboardLayoutList.GetItems(Index: Integer): TJclKeyboardLayout;
begin
  Result := TJclKeyboardLayout(FList[Index]);
end;

//------------------------------------------------------------------------------

function TJclKeyboardLayoutList.GetLayoutFromLocaleID(LocaleID: Word): TJclKeyboardLayout;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].LocaleID = LocaleID then
    begin
      Result := Items[I];
      Break;
    end;
end;

//------------------------------------------------------------------------------

function TJclKeyboardLayoutList.LoadLayout(const LayoutName: string;
  LoadFlags: TJclKeybLayoutFlags): Boolean;
begin
  Result := LoadKeyboardLayout(PChar(LayoutName),
    KeybLayoutFlagsToDWORD(LoadFlags, True)) <> 0;
  if Result then
    Refresh;
end;

//------------------------------------------------------------------------------

procedure TJclKeyboardLayoutList.Refresh;
var
  Cnt, I: Integer;
  Layouts: array [1..JclMaxKeyboardLayouts] of HKL;
begin
  Cnt := GetKeyboardLayoutList(JclMaxKeyboardLayouts, Layouts);
  // Note: GetKeyboardLayoutList doesn't work as expected, when pass 0 to nBuff
  // it always returns 0 on Win95 (not tested on NT).
  FList.Clear;
  for I := 1 to Cnt do
    FList.Add(TJclKeyboardLayout.Create(Self, Layouts[I]));
  DoRefresh;
end;

//------------------------------------------------------------------------------

initialization

finalization
  FreeAndNil(GlobalKeyboardLayoutList);
  FreeAndNil(GlobalLocalesList);
end.
