{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclPrint.pas.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Unit owner: Marcel van Brakel                                                                    }
{ Last modified: January 30, 2001                                                                  }
{                                                                                                  }
{**************************************************************************************************}

unit JclPrint;

{$I jcl.inc}

{$WEAKPACKAGEUNIT ON}

interface

uses
  Windows, Classes, StdCtrls, SysUtils,
  JclBase;

const
  CCHBinName = 24;
  CCHPaperName = 64;
  CBinMax = 256;
  CPaperNames = 256;

type
  PWordArray = ^TWordArray;
  TWordArray = array [0..255] of Word;

type
  EJclPrinterError = class (EJclError);

  TJclPrintSet = class (TObject)
  private
    FDevice: PChar;
    FDriver: PChar;
    FPort: PChar;
    FHandle: THandle;
    FDeviceMode: PDeviceModeA;
    FPrinter: Integer;
    FBinArray: PWordArray;
    FNumBins: Byte;
    FPaperArray: PWordArray;
    FNumPapers: Byte;
    FDpiX: Integer;
    FiDpiY: Integer;
    procedure CheckPrinter;
    procedure SetBinArray;
    procedure SetPaperArray;
    function DefaultPaperName(const PaperID: Word): string;
  protected
    procedure SetOrientation(Orientation: Integer);
    function GetOrientation: Integer;
    procedure SetPaperSize(Size: Integer);
    function GetPaperSize: Integer;
    procedure SetPaperLength(Length: Integer);
    function GetPaperLength: Integer;
    procedure SetPaperWidth(Width: Integer);
    function GetPaperWidth: Integer;
    procedure SetScale(Scale: Integer);
    function GetScale: Integer;
    procedure SetCopies(Copies: Integer);
    function GetCopies: Integer;
    procedure SetBin(Bin: Integer);
    function GetBin: Integer;
    procedure SetPrintQuality(Quality: Integer);
    function GetPrintQuality: Integer;
    procedure SetColor(Color: Integer);
    function GetColor: Integer;
    procedure SetDuplex(Duplex: Integer);
    function GetDuplex: Integer;
    procedure SetYResolution(YRes: Integer);
    function GetYResolution: Integer;
    procedure SetTrueTypeOption(Option: Integer);
    function GetTrueTypeOption: Integer;
    function GetPrinterName: string;
    function GetPrinterPort: string;
    function GetPrinterDriver: string;
    procedure SetBinFromList(BinNum: Byte);
    function GetBinIndex: Byte;
    procedure SetPaperFromList(PaperNum: Byte);
    function GetPaperIndex: Byte;
    procedure SetPort(Port: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function GetBinSourceList: TStringList;
    function GetPaperList: TStringList;
    procedure SetDeviceMode(Creating: Boolean);
    procedure UpdateDeviceMode;
    procedure SaveToDefaults;
    procedure SavePrinterAsDefault;
    procedure ResetPrinterDialogs;
    function XInchToDot(const Inches: Double): Integer;
    function YInchToDot(const Inches: Double): Integer;
    function XCmToDot(const Cm: Double): Integer;
    function YCmToDot(const Cm: Double): Integer;
    function CpiToDot(const Cpi, Chars: Double): Integer;
    function LpiToDot(const Lpi, Lines: Double): Integer;
    procedure TextOutInch(const X, Y: Double; const Text: string);
    procedure TextOutCm(const X, Y: Double; const Text: string);
    procedure TextOutCpiLpi(const Cpi, Chars, Lpi, Lines: Double; const Text: string);
    procedure CustomPageSetup(const Width, Height: Double);
    procedure SaveToIniFile(const IniFileName, Section: string);
    function ReadFromIniFile(const IniFileName, Section: string): Boolean;
    property Orientation: Integer read GetOrientation write SetOrientation;
    property PaperSize: Integer read GetPaperSize write SetPaperSize;
    property PaperLength: Integer read GetPaperLength write SetPaperLength;
    property PaperWidth: Integer read GetPaperWidth write SetPaperWidth;
    property Scale: Integer read GetScale write SetScale;
    property Copies: Integer read GetCopies write SetCopies;
    property DefaultSource: Integer read GetBin write SetBin;
    property PrintQuality: Integer read GetPrintQuality write SetPrintQuality;
    property Color: Integer read GetColor write SetColor;
    property Duplex: Integer read GetDuplex write SetDuplex;
    property YResolution: Integer read GetYResolution write SetYResolution;
    property TrueTypeOption: Integer read GetTrueTypeOption write SetTrueTypeOption;
    property PrinterName: string read GetPrinterName;
    property PrinterPort: string read GetPrinterPort write SetPort;
    property PrinterDriver: string read GetPrinterDriver;
    property BinIndex: Byte read GetBinIndex write SetBinFromList;
    property PaperIndex: Byte read GetPaperIndex write SetPaperFromList;
    property DpiX: Integer read FDpiX write FDpiX;
    property DpiY: Integer read FiDpiY write FiDpiY;
  end;

procedure DirectPrint(const Printer, Data: string);
procedure SetPrinterPixelsPerInch;
function GetPrinterResolution: TPoint;
function CharFitsWithinDots(const Text: string; const Dots: Integer): Integer;
//procedure PrintTextRotation(X, Y: Integer; Rotation: Word; Text: string);
procedure PrintMemo(const Memo: TMemo; const Rect: TRect);

implementation

uses
  Graphics, IniFiles, Messages, Printers, WinSpool,
  JclResources;

//==================================================================================================
// Misc. functions
//==================================================================================================

procedure DirectPrint(const Printer, Data: string);
const
  cRaw = 'RAW';
type
  TDoc_Info_1 = record
    DocName: PChar;
    OutputFile: PChar;
    Datatype: PChar;
  end;
var
  PrinterHandle: THandle;
  DocInfo: TDoc_Info_1;
  BytesWritten: Cardinal;
  Count: Cardinal;
  Defaults: TPrinterDefaults;
begin
  // Defaults added for network printers. Supposedly the last member is ignored
  // by Windows 9x but is necessary for Windows NT. Code was copied from a msg
  // by Alberto Toledo to the C++ Builder techlist and fwd by Theo Bebekis.
  Defaults.pDatatype := cRaw;
  Defaults.pDevMode := nil;
  Defaults.DesiredAccess := PRINTER_ACCESS_USE;
  Count := Length(Data);
  if not OpenPrinter(PChar(Printer), PrinterHandle, @Defaults) then
    raise EJclPrinterError.CreateResRec(@RsInvalidPrinter);
  // Fill in the structure with info about this "document"
  DocInfo.DocName := 'My Document';
  DocInfo.OutputFile := nil;
  DocInfo.Datatype := cRaw;
  try
    // Inform the spooler the document is beginning
    if StartDocPrinter(PrinterHandle, 1, @DocInfo) = 0 then
      EJclPrinterError.CreateResRec(@RsNAStartDocument);
    try
      // Start a page
      if not StartPagePrinter(PrinterHandle) then
        EJclPrinterError.CreateResRec(@RsNAStartPage);
      try
        // Send the data to the printer
        if not WritePrinter(PrinterHandle, @Data, Count, BytesWritten) then
          EJclPrinterError.CreateResRec(@RsNASendData);
      finally
        // End the page
        if not EndPagePrinter(PrinterHandle) then
          EJclPrinterError.CreateResRec(@RsNAEndPage);
      end;
    finally
      // Inform the spooler that the document is ending
      if not EndDocPrinter(PrinterHandle) then
        EJclPrinterError.CreateResRec(@RsNAEndDocument);
    end;
  finally
    // Tidy up the printer handle
    ClosePrinter(PrinterHandle);
  end;
  // Check to see if correct number of bytes written
  if BytesWritten <> Count then
    EJclPrinterError.CreateResRec(@RsNATransmission);
end;

//--------------------------------------------------------------------------------------------------

procedure SetPrinterPixelsPerInch;
var
  FontSize: Integer;
begin
  FontSize := Printer.Canvas.Font.Size;
  Printer.Canvas.Font.PixelsPerInch := GetDeviceCaps(Printer.Handle, LogPixelsY);
  Printer.Canvas.Font.Size := FontSize;
end;

//--------------------------------------------------------------------------------------------------

function GetPrinterResolution: TPoint;
begin
  Result.X := GetDeviceCaps(Printer.Handle, LogPixelsX);
  Result.Y := GetDeviceCaps(Printer.Handle, LogPixelsY);
end;

//--------------------------------------------------------------------------------------------------

function CharFitsWithinDots(const Text: string; const Dots: Integer): Integer;
var
  Idx: Integer;
begin
  Idx := Length(Text);
  //  Result := iIdx; //WIMDC
  while (Idx > 0) and (Printer.Canvas.TextWidth(Copy(Text, 1, Idx)) > Dots) do
    Dec(Idx);
  Result := Idx;
end;

//--------------------------------------------------------------------------------------------------
//WIMDC: The function CanvasTextOutRotation contains a bug in DxGraphics so no need to
//       implement it right now here
(*
procedure PrintTextRotation(X, Y: Integer; Rotation: Word; Text: string);
begin
  CanvasTextOutRotation(Printer.Canvas, X, Y, Rotation, Text);
end;
*)

//--------------------------------------------------------------------------------------------------
//WIMDC took the function from DXGraphics and replaced some lines to work with the TStrings class
//      of the memo.

procedure CanvasMemoOut(Canvas: TCanvas; Memo: TMemo; Rect: TRect);
var
  MemoText: PChar;
begin
  MemoText := Memo.Lines.GetText;
  if MemoText = nil then
    Exit;
  try
    DrawText(Canvas.Handle, MemoText, StrLen(MemoText), Rect,
      DT_Left or DT_ExpandTabs or DT_WordBreak);
  finally
    FreeMem(MemoText);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure PrintMemo(const Memo: TMemo; const Rect: TRect);
begin
  CanvasMemoOut(Printer.Canvas, Memo, Rect);
end;

//==================================================================================================
// TJclPrintSet
//==================================================================================================

constructor TJclPrintSet.Create;
begin
  inherited Create;
  FBinArray := nil;
  FPaperArray := nil;
  FPrinter := -99;
  GetMem(FDevice, 255);
  GetMem(FDriver, 255);
  GetMem(FPort, 255);
end;

//--------------------------------------------------------------------------------------------------

destructor TJclPrintSet.Destroy;
begin
  if FBinArray <> nil then
    FreeMem(FBinArray, FNumBins * SizeOf(Word));
  if FPaperArray <> nil then
    FreeMem(FPaperArray, FNumPapers * SizeOf(Word));
  if FDevice <> nil then
    FreeMem(FDevice, 255);
  if FDriver <> nil then
    FreeMem(FDriver, 255);
  if FPort <> nil then
    FreeMem(FPort, 255);
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.CheckPrinter;
begin
  if FPrinter <> Printer.PrinterIndex then
  begin
    Printer.GetPrinter(FDevice, FDriver, FPort, FHandle);
    Printer.SetPrinter(FDevice, FDriver, FPort, FHandle);
    SetDeviceMode(False);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.SetBinArray;
var
  NumBinsRec: Integer;
begin
  if FBinArray <> nil then
    FreeMem(FBinArray, FNumBins * SizeOf(Word));
  FBinArray := nil;
  FNumBins := DeviceCapabilities(FDevice, FPort, DC_Bins, nil, FDeviceMode);
  if FNumBins > 0 then
  begin
    GetMem(FBinArray, FNumBins * SizeOf(Word));
    NumBinsRec := DeviceCapabilities(FDevice, FPort, DC_Bins,
      PChar(FBinArray), FDeviceMode);
    if NumBinsRec <> FNumBins then
      raise EJclPrinterError.CreateResRec(@RsRetrievingSource);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.SetPaperArray;
var
  NumPapersRec: Integer;
begin
  if FPaperArray <> nil then
    FreeMem(FPaperArray, FNumPapers * SizeOf(Word));
  FNumPapers := DeviceCapabilities(FDevice, FPort, DC_Papers, nil, FDeviceMode);
  if FNumPapers > 0 then
  begin
    GetMem(FPaperArray, FNumPapers * SizeOf(Word));
    NumPapersRec := DeviceCapabilities(FDevice, FPort, DC_Papers,
      PChar(FPaperArray), FDeviceMode);
    if NumPapersRec <> FNumPapers then
      raise EJclPrinterError.CreateResRec(@RsRetrievingPaperSource);
  end
  else
    FPaperArray := nil;
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.DefaultPaperName(const PaperID: Word): string;
begin
  case PaperID of
    dmpaper_Letter:
      Result := RsPSLetter;
    dmpaper_LetterSmall:
      Result := RsPSLetter;
    dmpaper_Tabloid:
      Result := RsPSTabloid;
    dmpaper_Ledger:
      Result := RsPSLedger;
    dmpaper_Legal:
      Result := RsPSLegal;
    dmpaper_Statement:
      Result := RsPSStatement;
    dmpaper_Executive:
      Result := RsPSExecutive;
    dmpaper_A3:
      Result := RsPSA3;
    dmpaper_A4:
      Result := RsPSA4;
    dmpaper_A4Small:
      Result := RsPSA4;
    dmpaper_A5:
      Result := RsPSA5;
    dmpaper_B4:
      Result := RsPSB4;
    dmpaper_B5:
      Result := RsPSB5;
    dmpaper_Folio:
      Result := RsPSFolio;
    dmpaper_Quarto:
      Result := RsPSQuarto;
    dmpaper_10X14:
      Result := RsPS10x14;
    dmpaper_11X17:
      Result := RsPS11x17;
    dmpaper_Note:
      Result := RsPSNote;
    dmpaper_Env_9:
      Result := RsPSEnv9;
    dmpaper_Env_10:
      Result := RsPSEnv10;
    dmpaper_Env_11:
      Result := RsPSEnv11;
    dmpaper_Env_12:
      Result := RsPSEnv12;
    dmpaper_Env_14:
      Result := RsPSEnv14;
    dmpaper_CSheet:
      Result := RsPSCSheet;
    dmpaper_DSheet:
      Result := RsPSDSheet;
    dmpaper_ESheet:
      Result := RsPSESheet;
    dmpaper_User:
      Result := RsPSUser;
  else
    Result := RsPSUnknown;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.GetBinSourceList: TStringList;
type
  TBinName = array [0..CCHBinName - 1] of Char;
  TBinArray = array [1..cBinMax] of TBinName;
  PBinArray = ^TBinArray;
var
  NumBinsRec: Integer;
  BinArray: PBinArray;
  BinList: TStringList;
  BinStr: string;
  Idx: Integer;
begin
  CheckPrinter;
  Result := nil;
  BinArray := nil;
  if FNumBins = 0 then
    Exit;
  try
    GetMem(BinArray, FNumBins * SizeOf(TBinName));
    NumBinsRec := DeviceCapabilities(FDevice, FPort, DC_BinNames,
      PChar(BinArray), FDeviceMode);
    if NumBinsRec <> FNumBins then
      raise EJclPrinterError.CreateResRec(@RsRetrievingSource);
    BinList := TStringList.Create;
    for Idx := 1 to NumBinsRec do
    begin
      BinStr := StrPas(BinArray^[Idx]);
      BinList.Add(BinStr);
    end;
    Result := BinList;
  finally
    if BinArray <> nil then
      FreeMem(BinArray, FNumBins * SizeOf(TBinName));
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.GetPaperList: TStringList;
type
  TPaperName = array [0..CCHPaperName - 1] of Char;
  TPaperArray = array [1..cPaperNames] of TPaperName;
  PPaperArray = ^TPaperArray;
var
  NumPaperRec: Integer;
  PaperArray: PPaperArray;
  PaperList: TStringList;
  PaperStr: string;
  Idx: Integer;
begin
  CheckPrinter;
  Result := nil;
  PaperArray := nil;
  if FNumPapers = 0 then
    Exit;
  try
    GetMem(PaperArray, FNumPapers * SizeOf(TPaperName));
    NumPaperRec := DeviceCapabilities(FDevice, FPort, DC_PaperNames,
      PChar(PaperArray), FDeviceMode);
    if NumPaperRec <> FNumPapers then
    begin
      PaperList := TStringList.Create;
      for Idx := 1 to FNumPapers do
      begin
        PaperStr := DefaultPaperName(FPaperArray^[Idx - 1]);
        PaperList.Add(PaperStr);
      end;
    end
    else
    begin
      PaperList := TStringList.Create;
      for Idx := 1 to NumPaperRec do
      begin
        PaperStr := StrPas(PaperArray^[Idx]);
        PaperList.Add(PaperStr);
      end;
    end;
    Result := PaperList;
  finally
    if PaperArray <> nil then
      FreeMem(PaperArray, FNumPapers * SizeOf(TPaperName));
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.SetDeviceMode(Creating: Boolean);
var
  Res: TPoint;
begin
  Printer.GetPrinter(FDevice, FDriver, FPort, FHandle);
  if FHandle = 0 then
  begin
    Printer.PrinterIndex := Printer.PrinterIndex;
    Printer.GetPrinter(FDevice, FDriver, FPort, FHandle);
  end;
  if FHandle <> 0 then
  begin
    FDeviceMode := GlobalLock(FHandle);
    FPrinter := Printer.PrinterIndex;
    FDeviceMode^.dmFields := dm_Orientation or dm_PaperSize or
      dm_PaperLength or dm_PaperWidth or
      dm_Scale or dm_Copies or
      dm_DefaultSource or dm_PrintQuality or
      dm_Color or dm_Duplex or
      dm_YResolution or dm_TTOption;
    UpdateDeviceMode;
    FDeviceMode^.dmFields := 0;
    SetBinArray;
    SetPaperArray;
  end
  else
  begin
    FDeviceMode := nil;
    if not Creating then
      raise EJclPrinterError.CreateResRec(@RsDeviceMode);
    FPrinter := -99;
  end;
  Res := GetPrinterResolution;
  dpiX := Res.X;
  dpiY := Res.Y;
  if FHandle <> 0 then
    GlobalUnLock(FHandle);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.UpdateDeviceMode;
var
  DrvHandle: THandle;
  ExtDevCode: Integer;
begin
  CheckPrinter;
  OpenPrinter(FDevice, DrvHandle, nil);
  FDeviceMode^.dmFields := dm_Orientation or dm_PaperSize or
    dm_PaperLength or dm_PaperWidth or
    dm_Scale or dm_Copies or
    dm_DefaultSource or dm_PrintQuality or
    dm_Color or dm_Duplex or
    dm_YResolution or dm_TTOption;
  ExtDevCode := DocumentProperties(0, DrvHandle, FDevice,
    FDeviceMode^, FDeviceMode^,
    DM_IN_BUFFER or DM_OUT_BUFFER);
  if ExtDevCode <> IDOK then
    raise EJclPrinterError.CreateResRec(@RsUpdatingPrinter);
  ClosePrinter(DrvHandle);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.SaveToDefaults;
var
  DrvHandle: THandle;
  ExtDevCode: Integer;
begin
  CheckPrinter;
  OpenPrinter(FDevice, DrvHandle, nil);
  ExtDevCode := DocumentProperties(0, DrvHandle, FDevice,
    FDeviceMode^, FDeviceMode^,
    DM_IN_BUFFER or DM_UPDATE);
  if ExtDevCode <> IDOK then
    raise EJclPrinterError.CreateResRec(@RsUpdatingPrinter)
  else
    // SendMessage($FFFF, WM_WININICHANGE, 0, 0);
    SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, 0);
  ClosePrinter(DrvHandle);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.SavePrinterAsDefault;
var
  DeviceStr: string;
begin
  CheckPrinter;
  DeviceStr := StrPas(FDevice) + ',' +
    StrPas(FDriver) + ',' +
    StrPas(FPort) + #0;
  WriteProfileString('windows', 'device', @DeviceStr[1]);
  WriteProfileString(nil, nil, nil);
  // SendMessage($FFFF, WM_WININICHANGE, 0, 0);
  SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, 0);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.ResetPrinterDialogs;
begin
  Printer.GetPrinter(FDevice, FDriver, FPort, FHandle);
  Printer.SetPrinter(FDevice, FDriver, FPort, FHandle);
  SetDeviceMode(False);
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.XInchToDot(const Inches: Double): Integer;
begin
  Result := Trunc(DpiX * Inches);
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.YInchToDot(const Inches: Double): Integer;
begin
  Result := Trunc(DpiY * Inches);
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.XCmToDot(const Cm: Double): Integer;
begin
  Result := Trunc(DpiX * (Cm * 2.54));
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.YCmToDot(const Cm: Double): Integer;
begin
  Result := Trunc(DpiY * (Cm * 2.54));
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.CpiToDot(const Cpi, Chars: Double): Integer;
begin
  Result := Trunc((DpiX * Chars) / Cpi);
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.LpiToDot(const Lpi, Lines: Double): Integer;
begin
  Result := Trunc((DpiY * Lpi) / Lines);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.TextOutInch(const X, Y: Double; const Text: string);
begin
  Printer.Canvas.TextOut(XInchToDot(X), YInchToDot(Y), Text);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.TextOutCm(const X, Y: Double; const Text: string);
begin
  Printer.Canvas.TextOut(XCmToDot(X), YCmToDot(Y), Text);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.TextOutCpiLpi(const Cpi, Chars, Lpi, Lines: Double; const Text: string);
begin
  Printer.Canvas.TextOut(CpiToDot(Cpi, Chars), LpiToDot(Lpi, Lines), Text);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.CustomPageSetup(const Width, Height: Double);
begin
  PaperSize := dmPaper_User;
  PaperLength := Trunc(254 * Height);
  YResolution := Trunc(DpiY * Height);
  PaperWidth := Trunc(254 * Width);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.SaveToIniFile(const IniFileName, Section: string);
var
  PrIniFile: TIniFile;
  CurrentName: string;
begin
  PrIniFile := TIniFile.Create(IniFileName);
  CurrentName := Printer.Printers[Printer.PrinterIndex];
  PrIniFile.WriteString(Section, RsPrintIniPrinterName, CurrentName);
  PrIniFile.WriteString(Section, RsPrintIniPrinterPort, PrinterPort);
  PrIniFile.WriteInteger(Section, RsPrintIniOrientation, Orientation);
  PrIniFile.WriteInteger(Section, RsPrintIniPaperSize, PaperSize);
  PrIniFile.WriteInteger(Section, RsPrintIniPaperLength, PaperLength);
  PrIniFile.WriteInteger(Section, RsPrintIniPaperWidth, PaperWidth);
  PrIniFile.WriteInteger(Section, RsPrintIniScale, Scale);
  PrIniFile.WriteInteger(Section, RsPrintIniCopies, Copies);
  PrIniFile.WriteInteger(Section, RsPrintIniDefaultSource, DefaultSource);
  PrIniFile.WriteInteger(Section, RsPrintIniPrintQuality, PrintQuality);
  PrIniFile.WriteInteger(Section, RsPrintIniColor, Color);
  PrIniFile.WriteInteger(Section, RsPrintIniDuplex, Duplex);
  PrIniFile.WriteInteger(Section, RsPrintIniYResolution, YResolution);
  PrIniFile.WriteInteger(Section, RsPrintIniTTOption, TrueTypeOption);
  PrIniFile.Free;
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.ReadFromIniFile(const IniFileName, Section: string): Boolean;
var
  PrIniFile: TIniFile;
  SavedName: string;
  NewIndex: Integer;
begin
  Result := False;
  PrIniFile := TIniFile.Create(IniFileName);
  SavedName := PrIniFile.ReadString(Section, RsPrintIniPrinterName, PrinterName);
  if PrinterName <> SavedName then
  begin
    NewIndex := Printer.Printers.IndexOf(SavedName);
    if NewIndex <> -1 then
    begin
      Result := True;
      Printer.PrinterIndex := NewIndex;
      PrinterPort := PrIniFile.ReadString(Section, RsPrintIniPrinterPort, PrinterPort);
      Orientation := PrIniFile.ReadInteger(Section, RsPrintIniOrientation, Orientation);
      PaperSize := PrIniFile.ReadInteger(Section, RsPrintIniPaperSize, PaperSize);
      PaperLength := PrIniFile.ReadInteger(Section, RsPrintIniPaperLength, PaperLength);
      PaperWidth := PrIniFile.ReadInteger(Section, RsPrintIniPaperWidth, PaperWidth);
      Scale := PrIniFile.ReadInteger(Section, RsPrintIniScale, Scale);
      Copies := PrIniFile.ReadInteger(Section, RsPrintIniCopies, Copies);
      DefaultSource := PrIniFile.ReadInteger(Section, RsPrintIniDefaultSource, DefaultSource);
      PrintQuality := PrIniFile.ReadInteger(Section, RsPrintIniPrintQuality, PrintQuality);
      Color := PrIniFile.ReadInteger(Section, RsPrintIniColor, Color);
      Duplex := PrIniFile.ReadInteger(Section, RsPrintIniDuplex, Duplex);
      YResolution := PrIniFile.ReadInteger(Section, RsPrintIniYResolution, YResolution);
      TrueTypeOption := PrIniFile.ReadInteger(Section, RsPrintIniTTOption, TrueTypeOption);
    end
    else
      Result := False;
  end;
  PrIniFile.Free;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.SetOrientation(Orientation: Integer);
begin
  CheckPrinter;
  FDeviceMode^.dmOrientation := Orientation;
  Printer.Orientation := TPrinterOrientation(Orientation - 1);
  FDeviceMode^.dmFields := FDeviceMode^.dmFields or DM_ORIENTATION;
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.GetOrientation: Integer;
begin
  CheckPrinter;
  Result := FDeviceMode^.dmOrientation;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.SetPaperSize(Size: Integer);
begin
  CheckPrinter;
  FDeviceMode^.dmPaperSize := Size;
  FDeviceMode^.dmFields := FDeviceMode^.dmFields or DM_PAPERSIZE;
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.GetPaperSize: Integer;
begin
  CheckPrinter;
  Result := FDeviceMode^.dmPaperSize;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.SetPaperLength(Length: Integer);
begin
  CheckPrinter;
  FDeviceMode^.dmPaperLength := Length;
  FDeviceMode^.dmFields := FDeviceMode^.dmFields or DM_PAPERLENGTH;
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.GetPaperLength: Integer;
begin
  CheckPrinter;
  Result := FDeviceMode^.dmPaperLength;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.SetPaperWidth(Width: Integer);
begin
  CheckPrinter;
  FDeviceMode^.dmPaperWidth := Width;
  FDeviceMode^.dmFields := FDeviceMode^.dmFields or DM_PAPERWIDTH;
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.GetPaperWidth: Integer;
begin
  CheckPrinter;
  Result := FDeviceMode^.dmPaperWidth;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.SetScale(Scale: Integer);
begin
  CheckPrinter;
  FDeviceMode^.dmScale := Scale;
  FDeviceMode^.dmFields := FDeviceMode^.dmFields or DM_SCALE;
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.GetScale: Integer;
begin
  CheckPrinter;
  Result := FDeviceMode^.dmScale;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.SetCopies(Copies: Integer);
begin
  CheckPrinter;
  FDeviceMode^.dmCopies := Copies;
  FDeviceMode^.dmFields := FDeviceMode^.dmFields or DM_COPIES;
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.GetCopies: Integer;
begin
  CheckPrinter;
  Result := FDeviceMode^.dmCopies;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.SetBin(Bin: Integer);
begin
  CheckPrinter;
  FDeviceMode^.dmDefaultSource := Bin;
  FDeviceMode^.dmFields := FDeviceMode^.dmFields or DM_DEFAULTSOURCE;
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.GetBin: Integer;
begin
  CheckPrinter;
  Result := FDeviceMode^.dmDefaultSource;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.SetPrintQuality(Quality: Integer);
begin
  CheckPrinter;
  FDeviceMode^.dmPrintQuality := Quality;
  FDeviceMode^.dmFields := FDeviceMode^.dmFields or DM_PRINTQUALITY;
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.GetPrintQuality: Integer;
begin
  CheckPrinter;
  Result := FDeviceMode^.dmPrintQuality;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.SetColor(Color: Integer);
begin
  CheckPrinter;
  FDeviceMode^.dmColor := Color;
  FDeviceMode^.dmFields := FDeviceMode^.dmFields or DM_ORIENTATION;
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.GetColor: Integer;
begin
  CheckPrinter;
  Result := FDeviceMode^.dmColor;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.SetDuplex(Duplex: Integer);
begin
  CheckPrinter;
  FDeviceMode^.dmDuplex := Duplex;
  FDeviceMode^.dmFields := FDeviceMode^.dmFields or DM_DUPLEX;
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.GetDuplex: Integer;
begin
  CheckPrinter;
  Result := FDeviceMode^.dmDuplex;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.SetYResolution(YRes: Integer);
var
  PrintDevMode: PDeviceModeA;
begin
  CheckPrinter;
  PrintDevMode := @FDeviceMode^;
  PrintDevMode^.dmYResolution := YRes;
  FDeviceMode^.dmFields := FDeviceMode^.dmFields or DM_YRESOLUTION;
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.GetYResolution: Integer;
var
  PrintDevMode: PDeviceModeA;
begin
  CheckPrinter;
  PrintDevMode := @FDeviceMode^;
  Result := PrintDevMode^.dmYResolution;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.SetTrueTypeOption(Option: Integer);
var
  PrintDevMode: PDeviceModeA;
begin
  CheckPrinter;
  PrintDevMode := @FDeviceMode^;
  PrintDevMode^.dmTTOption := Option;
  FDeviceMode^.dmFields := FDeviceMode^.dmFields or DM_TTOPTION;
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.GetTrueTypeOption: Integer;
var
  PrintDevMode: PDeviceModeA;
begin
  CheckPrinter;
  PrintDevMode := @FDeviceMode^;
  Result := PrintDevMode^.dmTTOption;
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.GetPrinterName: string;
begin
  CheckPrinter;
  Result := StrPas(FDevice);
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.GetPrinterPort: string;
begin
  CheckPrinter;
  Result := StrPas(FPort);
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.GetPrinterDriver: string;
begin
  CheckPrinter;
  Result := StrPas(FDriver);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.SetBinFromList(BinNum: Byte);
begin
  CheckPrinter;
  if FNumBins = 0 then
    Exit;
  if BinNum > FNumBins then
    raise EJclPrinterError.CreateResRec(@RsIndexOutOfRange)
  else
    DefaultSource := FBinArray^[BinNum];
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.GetBinIndex: Byte;
var
  Idx: Byte;
begin
  Result := 0;
  for Idx := 0 to FNumBins do
  begin
    if FBinArray^[Idx] = Word(FDeviceMode^.dmDefaultSource) then
    begin
      Result := Idx;
      Break;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.SetPaperFromList(PaperNum: Byte);
begin
  CheckPrinter;
  if FNumPapers = 0 then
    Exit;
  if PaperNum > FNumPapers then
    raise EJclPrinterError.CreateResRec(@RsIndexOutOfRangePaper)
  else
    PaperSize := FPaperArray^[PaperNum];
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPrintSet.SetPort(Port: string);
begin
  CheckPrinter;
  Port := Port + #0;
  Move(Port[1], FPort^, Length(Port));
  Printer.SetPrinter(FDevice, FDriver, FPort, FHandle);
end;

//--------------------------------------------------------------------------------------------------

function TJclPrintSet.GetPaperIndex: Byte;
var
  Idx: Byte;
begin
  Result := 0;
  for Idx := 0 to FNumPapers do
  begin
    if FPaperArray^[Idx] = Word(FDeviceMode^.dmPaperSize) then
    begin
      Result := Idx;
      Break;
    end;
  end;
end;

end.
