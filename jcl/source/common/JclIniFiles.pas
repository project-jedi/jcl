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
{ The Original Code is JclIniFiles.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is John C Molyneux.                                   }
{ Portions created by John C Molyneux are Copyright (C) John C Molyneux.                           }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Eric S. Fisher                                                                                 }
{   John C Molyneux                                                                                }
{   Peter J. Haas (peterjhaas)                                                                     }
{   Petr Vones (pvones)                                                                            }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclIniFiles;

{$I jcl.inc}

interface
uses
  Classes, IniFiles, SysUtils;
  
//--------------------------------------------------------------------------------------------------
// Initialization (ini) Files
//--------------------------------------------------------------------------------------------------

function IniReadBool(const FileName, Section, Line: string): Boolean;              // John C Molyneux
function IniReadInteger(const FileName, Section, Line: string): Integer;           // John C Molyneux
function IniReadString(const FileName, Section, Line: string): string;             // John C Molyneux
procedure IniWriteBool(const FileName, Section, Line: string; Value: Boolean);     // John C Molyneux
procedure IniWriteInteger(const FileName, Section, Line: string; Value: Integer);  // John C Molyneux
procedure IniWriteString(const FileName, Section, Line, Value: string);            // John C Molyneux

//--------------------------------------------------------------------------------------------------
// Initialization (ini) Files helper routines
//--------------------------------------------------------------------------------------------------

procedure IniReadStrings(IniFile: TCustomIniFile; const Section: string; Strings: TStrings);
procedure IniWriteStrings(IniFile: TCustomIniFile; const Section: string; Strings: TStrings);

//--------------------------------------------------------------------------------------------------
// IniFile interface without localized texts
//--------------------------------------------------------------------------------------------------

type
  TJclISOMemIniFile = class(TMemIniFile)
  public
    function ReadDate(const Section, Name: string; Default: TDateTime): TDateTime; override;
    function ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime; override;
    function ReadFloat(const Section, Name: string; Default: Double): Double; override;
    function ReadTime(const Section, Name: string; Default: TDateTime): TDateTime; override;
    procedure WriteDate(const Section, Name: string; Value: TDateTime); override;
    procedure WriteDateTime(const Section, Name: string; Value: TDateTime); override;
    procedure WriteFloat(const Section, Name: string; Value: Double); override;
    procedure WriteTime(const Section, Name: string; Value: TDateTime); override;
  end;

  TJclISOIniFile = class(TIniFile)
  public
    function ReadDate(const Section, Name: string; Default: TDateTime): TDateTime; override;
    function ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime; override;
    function ReadFloat(const Section, Name: string; Default: Double): Double; override;
    function ReadTime(const Section, Name: string; Default: TDateTime): TDateTime; override;
    procedure WriteDate(const Section, Name: string; Value: TDateTime); override;
    procedure WriteDateTime(const Section, Name: string; Value: TDateTime); override;
    procedure WriteFloat(const Section, Name: string; Value: Double); override;
    procedure WriteTime(const Section, Name: string; Value: TDateTime); override;
  end;

implementation
uses
  JclDITs;

//==================================================================================================
// Initialization Files
//==================================================================================================

function IniReadBool(const FileName, Section, Line: string): Boolean;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Result := Ini.ReadBool(Section, Line, False);
  finally
    Ini.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

function IniReadInteger(const FileName, Section, Line: string): Integer;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Result := Ini.ReadInteger(Section, Line, 0);
  finally
    Ini.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

function IniReadString(const FileName, Section, Line: string): string;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Result := Ini.ReadString(Section, Line, '');
  finally
    Ini.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure IniWriteBool(const FileName, Section, Line: string; Value: Boolean);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Ini.WriteBool(Section, Line, Value);
  finally
    Ini.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure IniWriteInteger(const FileName, Section, Line: string; Value: Integer);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Ini.WriteInteger(Section, Line, Value);
  finally
    Ini.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure IniWriteString(const FileName, Section, Line, Value: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Ini.WriteString(Section, Line, Value);
  finally
    Ini.Free;
  end;
end;

//==================================================================================================
// Initialization (ini) Files helper routines
//==================================================================================================

const
  ItemCountName = 'Count';

//--------------------------------------------------------------------------------------------------

procedure IniReadStrings(IniFile: TCustomIniFile; const Section: string; Strings: TStrings);
var
  Count, I: Integer;
begin
  with IniFile do
  begin
    Strings.BeginUpdate;
    try
      Strings.Clear;
      Count := ReadInteger(Section, ItemCountName, 0);
      for I := 0 to Count - 1 do
        Strings.Add(ReadString(Section, IntToStr(I), ''));
    finally
      Strings.EndUpdate;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure IniWriteStrings(IniFile: TCustomIniFile; const Section: string; Strings: TStrings);
var
  I: Integer;
begin
  with IniFile do
  begin
    EraseSection(Section);
    WriteInteger(Section, ItemCountName, Strings.Count);
    for I := 0 to Strings.Count - 1 do
      WriteString(Section, IntToStr(I), Strings[I]);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclISOMemIniFile.ReadDate(const Section, Name: string; Default: TDateTime): TDateTime;
begin
  Result := ISOStrToDateDef(ReadString(Section, Name, ''), Default);
end;

function TJclISOMemIniFile.ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime;
begin
  Result := ISOStrToDateTimeDef(ReadString(Section, Name, ''), Default);
end;

function TJclISOMemIniFile.ReadFloat(const Section, Name: string; Default: Double): Double;
begin
  Result := ISOStrToFloatDef(ReadString(Section, Name, ''), Default);
end;

function TJclISOMemIniFile.ReadTime(const Section, Name: string; Default: TDateTime): TDateTime;
begin
  Result := ISOStrToTimeDef(ReadString(Section, Name, ''), Default);
end;

procedure TJclISOMemIniFile.WriteDate(const Section, Name: string; Value: TDateTime);
begin
  WriteString(Section, Name, ISODateToStr(Value));
end;

procedure TJclISOMemIniFile.WriteDateTime(const Section, Name: string; Value: TDateTime);
begin
  WriteString(Section, Name, ISODateTimeToStr(Value));
end;

procedure TJclISOMemIniFile.WriteFloat(const Section, Name: string; Value: Double);
begin
  WriteString(Section, Name, ISOFloatToStr(Value));
end;

procedure TJclISOMemIniFile.WriteTime(const Section, Name: string; Value: TDateTime);
begin
  WriteString(Section, Name, ISOTimeToStr(Value));
end;

//--------------------------------------------------------------------------------------------------

function TJclISOIniFile.ReadDate(const Section, Name: string; Default: TDateTime): TDateTime;
begin
  Result := ISOStrToDateDef(ReadString(Section, Name, ''), Default);
end;

function TJclISOIniFile.ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime;
begin
  Result := ISOStrToDateTimeDef(ReadString(Section, Name, ''), Default);
end;

function TJclISOIniFile.ReadFloat(const Section, Name: string; Default: Double): Double;
begin
  Result := ISOStrToFloatDef(ReadString(Section, Name, ''), Default);
end;

function TJclISOIniFile.ReadTime(const Section, Name: string; Default: TDateTime): TDateTime;
begin
  Result := ISOStrToTimeDef(ReadString(Section, Name, ''), Default);
end;

procedure TJclISOIniFile.WriteDate(const Section, Name: string; Value: TDateTime);
begin
  WriteString(Section, Name, ISODateToStr(Value));
end;

procedure TJclISOIniFile.WriteDateTime(const Section, Name: string; Value: TDateTime);
begin
  WriteString(Section, Name, ISODateTimeToStr(Value));
end;

procedure TJclISOIniFile.WriteFloat(const Section, Name: string; Value: Double);
begin
  WriteString(Section, Name, ISOFloatToStr(Value));
end;

procedure TJclISOIniFile.WriteTime(const Section, Name: string; Value: TDateTime);
begin
  WriteString(Section, Name, ISOTimeToStr(Value));
end;

// History:

// 2004-03-23, Peter J. Haas
//  - add TJclISOMemIniFile, TJclISOIniFile

// $Log$
// Revision 1.7  2004/07/30 07:20:25  marquardt
// fixing TStringLists, adding BeginUpdate/EndUpdate
//
// Revision 1.6  2004/06/02 03:23:46  rrossmair
// cosmetic changes in several units (code formatting, help TODOs processed etc.)
//
// Revision 1.5  2004/05/14 15:24:46  rrossmair
// fixed header
//
// Revision 1.4  2004/05/05 00:04:11  mthoma
// Updated headers: Added donors as contributors, adjusted the initial authors, added cvs names when they were not obvious. Changed $data to $date where necessary,
//
// Revision 1.3  2004/04/06 04:32:43  peterjhaas
// Add TJclISOIniFile, TJclISOMemIniFile
//

end.
