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
{ The Original Code is JclWinMidi.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ MIDI functions for MS Windows platform                                                           }
{                                                                                                  }
{ Unit owner: Robert Rossmair                                                                      }
{ Last modified: April 8, 2002                                                                     }
{                                                                                                  }
{**************************************************************************************************}

unit JclWinMidi;

{$I jcl.inc}

interface

uses
  SysUtils, Classes, Windows, MMSystem, JclBase, JclMidi;

type
  TStereoChannel = (scLeft, scRight);
  
//--------------------------------------------------------------------------------------------------
// MIDI Out
//--------------------------------------------------------------------------------------------------

  IJclWinMidiOut = interface (IJclMidiOut)
    ['{F3FCE71C-B924-462C-BA0D-8C2DC118DADB}']
    // property access methods
    function GetChannelVolume(Channel: TStereoChannel): Word;
    procedure SetChannelVolume(Channel: TStereoChannel; const Value: Word);
    function GetVolume: Word;
    procedure SetVolume(const Value: Word);
    // properties
    property ChannelVolume[Channel: TStereoChannel]: Word read GetChannelVolume write SetChannelVolume;
    property Volume: Word read GetVolume write SetVolume;
  end;

function MidiOut(DeviceID: UINT): IJclWinMidiOut;
function MidiOutputs: TStrings;
procedure MidiOutCheck(Code: MMResult);

//--------------------------------------------------------------------------------------------------
// MIDI In
//--------------------------------------------------------------------------------------------------

procedure MidiInCheck(Code: MMResult);

implementation

uses JclStrings, JclResources;

resourcestring
  RsMidiInUnknownError = 'Unknown MIDI-In error No. %d';
  RsMidiOutUnknownError = 'Unknown MIDI-Out error No. %d';

//--------------------------------------------------------------------------------------------------

var
  FMidiOutputs: TStringList = nil;

function MidiOutputs: TStrings;
var
  I: Integer;
  Caps: TMidiOutCaps;
begin
  if FMidiOutputs = nil then
  begin
    FMidiOutputs := TStringList.Create;
    for I := 0 to midiOutGetNumDevs - 1 do
    begin
      if (midiOutGetDevCaps(I, @Caps, SizeOf(Caps)) = MMSYSERR_NOERROR) then
        FMidiOutputs.Add(Caps.szPName);
    end;
  end;
  Result := FMidiOutputs;
end;

//--------------------------------------------------------------------------------------------------

function GetMidiInErrorMessage(const ErrorCode: MMRESULT): string;
begin
  SetLength(Result, MAXERRORLENGTH-1);
  if midiInGetErrorText(ErrorCode, @Result[1], MAXERRORLENGTH) = MMSYSERR_NOERROR then
    StrResetLength(Result)
  else
    Result := Format(RsMidiInUnknownError, [ErrorCode]);
end;

//--------------------------------------------------------------------------------------------------

function GetMidiOutErrorMessage(const ErrorCode: MMRESULT): string;
begin
  SetLength(Result, MAXERRORLENGTH-1);
  if midiOutGetErrorText(ErrorCode, @Result[1], MAXERRORLENGTH) = MMSYSERR_NOERROR then
    StrResetLength(Result)
  else
    Result := Format(RsMidiOutUnknownError, [ErrorCode]);
end;

//--------------------------------------------------------------------------------------------------

procedure MidiInCheck(Code: MMResult);
begin
  if Code <> MMSYSERR_NOERROR then
    raise EJclMidiError.Create(GetMidiInErrorMessage(Code));
end;

//--------------------------------------------------------------------------------------------------

procedure MidiOutCheck(Code: MMResult);
begin
  if Code <> MMSYSERR_NOERROR then
    raise EJclMidiError.Create(GetMidiOutErrorMessage(Code));
end;

//==================================================================================================
// MidiOut implementation
//==================================================================================================

type
  TMidiOut = class(TJclMidiOut, IJclWinMidiOut)
  private
    FHandle: HMIDIOUT;
    FDeviceID: UINT;
    FDeviceCaps: TMidiOutCaps;
    FVolume: DWord;
    FRunningStatus: TMidiStatusByte;
    FRunningStatusEnabled: Boolean;
    function GetChannelVolume(Channel: TStereoChannel): Word;
    procedure SetChannelVolume(Channel: TStereoChannel; const Value: Word);
    function GetVolume: Word;
    procedure SetVolume(const Value: Word);
    procedure SetLRVolume(const LeftValue, RightValue: Word);
  protected
    function GetName: string; override;
    procedure ShortMessage(Status: TMidiStatusByte; Data1, Data2: TMidiDataByte); override;
    procedure LongMessage(Data: array of Byte); override;
  public
    constructor Create(ADeviceID: UINT);
    destructor Destroy; override;
    property Name: string read GetName;
    property ChannelVolume[Channel: TStereoChannel]: Word read GetChannelVolume write SetChannelVolume;
    property Volume: Word read GetVolume write SetVolume;
  end;

//--------------------------------------------------------------------------------------------------

function MidiOut(DeviceID: UINT): IJclWinMidiOut;
begin
  if DeviceID < midiOutGetNumDevs then
  begin
    if MidiOutputs.Objects[DeviceID] = nil then
      MidiOutputs.Objects[DeviceID] := TMidiOut.Create(DeviceID);
    Result := TMidiOut(MidiOutputs.Objects[DeviceID]);
  end;
end;

//--------------------------------------------------------------------------------------------------

constructor TMidiOut.Create(ADeviceID: UINT);
begin
  inherited Create;
  FVolume := $FFFFFFFF; // max. volume, in case Get/SetChannelVolume not supported
  MidiOutCheck(midiOutGetDevCaps(ADeviceID, @FDeviceCaps, SizeOf(FDeviceCaps)));
  MidiOutCheck(midiOutOpen(@FHandle, ADeviceID, 0, 0, 0));
  FDeviceID := ADeviceID;
end;

//--------------------------------------------------------------------------------------------------

destructor TMidiOut.Destroy;
begin
  inherited;
  midiOutClose(FHandle);
  MidiOutputs.Objects[FDeviceID] := nil;
end;

//--------------------------------------------------------------------------------------------------

function TMidiOut.GetName: string;
begin
  Result := FDeviceCaps.szPName;
end;

//--------------------------------------------------------------------------------------------------

procedure TMidiOut.ShortMessage(Status: TMidiStatusByte; Data1,
  Data2: TMidiDataByte);
type
  TMidiShortMsg = packed record
  case Integer of
    0: (DWord: DWORD);
    1: (StatusByte: TMidiStatusByte;
        DataByte1,
        DataByte2: TMidiDataByte);
    2: { Running status }
       (RDataByte1,
        RDataByte2: TMidiDataByte);
  end;
var
  Msg: TMidiShortMsg;
begin
  with Msg do
  begin
    StatusByte := Status;
    if FRunningStatusEnabled and (Status = FRunningStatus) then
    begin
      RDataByte1 := Data1 and MidiDataMask;
      RDataByte2 := Data2 and MidiDataMask;
    end else
    begin
      FRunningStatus := Status;
      DataByte1 := Data1 and MidiDataMask;
      DataByte2 := Data2 and MidiDataMask;
    end;
    MidiOutCheck(midiOutShortMsg(FHandle, DWord));
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TMidiOut.LongMessage(Data: array of Byte);
var
  Hdr: TMidiHdr;
begin
  FillChar(Hdr, SizeOf(Hdr), 0);
  Hdr.dwBufferLength := High(Data)-Low(Data)+1;;
  Hdr.dwBytesRecorded := Hdr.dwBufferLength;
  Hdr.lpData := @Data;
  Hdr.dwFlags := 0;
  MidiOutCheck(midiOutPrepareHeader(FHandle, @Hdr, SizeOf(Hdr)));
  MidiOutCheck(midiOutLongMsg(FHandle, @Hdr, SizeOf(Hdr)));
  repeat until (Hdr.dwFlags and MHDR_DONE) <> 0;
end;

//--------------------------------------------------------------------------------------------------

function TMidiOut.GetChannelVolume(Channel: TStereoChannel): Word;
begin
  midiOutGetVolume(FHandle, @FVolume);
  Result := FVolume;
end;

//--------------------------------------------------------------------------------------------------

procedure TMidiOut.SetChannelVolume(Channel: TStereoChannel; const Value: Word);
begin
  if Channel = scLeft then
    SetLRVolume(Value, ChannelVolume[scRight])
  else
    SetLRVolume(ChannelVolume[scLeft], Value);
end;

//--------------------------------------------------------------------------------------------------

function TMidiOut.GetVolume: Word;
begin
  Result := GetChannelVolume(scLeft);
end;

//--------------------------------------------------------------------------------------------------

procedure TMidiOut.SetVolume(const Value: Word);
begin
  SetLRVolume(Value, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TMidiOut.SetLRVolume(const LeftValue, RightValue: Word);
var
  Value: DWord;
begin
  with LongRec(Value) do
  begin
    Lo := LeftValue;
    Hi := RightValue;
  end;
  if Value <> FVolume then
  begin
    if (MIDICAPS_VOLUME and FDeviceCaps.dwSupport) <> 0 then
      MidiOutCheck(midiOutSetVolume(FHandle, Value));
    FVolume := Value;
  end;
end;

//--------------------------------------------------------------------------------------------------

initialization
finalization
  FMidiOutputs.Free;
end.
