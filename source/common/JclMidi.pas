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
{ The Original Code is JclMIDI.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Platform-independent MIDI declarations                                                           }
{                                                                                                  }
{ Unit owner: Robert Rossmair                                                                      }
{ Last modified: April 8, 2002                                                                     }
{                                                                                                  }
{**************************************************************************************************}

unit JclMIDI;

interface

{$I jcl.inc}

uses JclBase;

// manifest constants for MIDI message protocol
const
  // MIDI Status Bytes for Channel Voice Messages
  MIDIMsgNoteOff             = $80;
  MIDIMsgNoteOn              = $90;
  MIDIMsgPolyKeyPressure     = $A0;
  MIDIMsgControlChange       = $B0;
  MIDIMsgProgramChange       = $C0;
  MIDIMsgChannelKeyPressure  = $D0;
  MIDIMsgAftertouch          = MIDIMsgChannelKeyPressure; // Synonym
  MIDIMsgPitchWheelChange    = $E0;
  // MIDI Status Bytes for System Common Messages
  MIDIMsgSysEx               = $F0;
  MIDIMsgMTCQtrFrame         = $F1; // MIDI Time Code Qtr. Frame
  MIDIMsgSongPositionPtr     = $F2;
  MIDIMsgSongSelect          = $F3;
  MIDIMsgTuneRequest         = $F6;
  MIDIMsgEOX                 = $F7; // marks end of system exclusive message

  // MIDI Status Bytes for System Real-Time Messages
  MIDIMsgTimingClock         = $F8;
  MIDIMsgStartSequence       = $FA;
  MIDIMsgContinueSequence    = $FB;
  MIDIMsgStopSequence        = $FC;
  MIDIMsgActiveSensing       = $FE;
  MIDIMsgSystemReset         = $FF;

  // MIDICC...: MIDI Control Change Messages

  // Continuous Controllers MSB
  MIDICCBankSelect         = $00;
  MIDICCModulationWheel    = $01;
  MIDICCBreathControl      = $02;
  MIDICCFootController     = $04;
  MIDICCPortamentoTime     = $05;
  MIDICCDataEntry          = $06;
  MIDICCChannelVolume      = $07;
  MIDICCMainVolume = MIDICCChannelVolume;
  MIDICCBalance            = $08;
  MIDICCPan                = $0A;
  MIDICCExpression         = $0B;
  MIDICCEffectControl      = $0C;
  MIDICCEffectControl2     = $0D;
  MIDICCGeneralPurpose1    = $10;
  MIDICCGeneralPurpose2    = $11;
  MIDICCGeneralPurpose3    = $12;
  MIDICCGeneralPurpose4    = $13;
  // Continuous Controllers LSB
  MIDICCBankSelectLSB      = $20;
  MIDICCModulationWheelLSB = $21;
  MIDICCBreathControlLSB   = $22;
  MIDICCFootControllerLSB  = $24;
  MIDICCPortamentoTimeLSB  = $25;
  MIDICCDataEntryLSB       = $26;
  MIDICCChannelVolumeLSB   = $27;
  MIDICCMainVolumeLSB = MIDICCChannelVolumeLSB;
  MIDICCBalanceLSB         = $28;
  MIDICCPanLSB             = $2A;
  MIDICCExpressionLSB      = $2B;
  MIDICCEffectControlLSB   = $2C;
  MIDICCEffectControl2LSB  = $2D;
  MIDICCGeneralPurpose1LSB = $30;
  MIDICCGeneralPurpose2LSB = $31;
  MIDICCGeneralPurpose3LSB = $32;
  MIDICCGeneralPurpose4LSB = $33;
  // Switches
  MIDICCSustain            = $40;
  MIDICCPortamento         = $41;
  MIDICCSustenuto          = $42;
  MIDICCSoftPedal          = $43;
  MIDICCLegato             = $44;
  MIDICCHold2              = $45;

  MIDICCSound1             = $46; // (Sound Variation)
  MIDICCSound2             = $47; // (Timbre/Harmonic Intens.)
  MIDICCSound3             = $48; // (Release Time)
  MIDICCSound4             = $49; // (Attack Time)
  MIDICCSound5             = $4A; // (Brightness)
  MIDICCSound6             = $4B; // (Decay Time)
  MIDICCSound7             = $4C; // (Vibrato Rate)
  MIDICCSound8             = $4D; // (Vibrato Depth)
  MIDICCSound9             = $4E; // (Vibrato Delay)
  MIDICCSound10            = $4F; //

  MIDICCGeneralPurpose5    = $50;
  MIDICCGeneralPurpose6    = $51;
  MIDICCGeneralPurpose7    = $52;
  MIDICCGeneralPurpose8    = $53;
  MIDICCPortamentoControl  = $54;

  MIDICCReverbSendLevel    = $5B;
  MIDICCEffects2Depth      = $5C;
  MIDICCTremoloDepth = MIDICCEffects2Depth;
  MIDICCChorusSendLevel    = $5D;
  MIDICCEffects4Depth      = $5E;
  MIDICCCelesteDepth = MIDICCEffects4Depth;
  MIDICCEffects5Depth      = $5F;
  MIDICCPhaserDepth = MIDICCEffects5Depth;

  MIDICCDataEntryInc       = $60;
  MIDICCDataEntryDec       = $61;
  MIDICCNonRegParamNumLSB  = $62;
  MIDICCNonRegParamNumMSB  = $63;
  MIDICCRegParamNumLSB     = $64;
  MIDICCRegParamNumMSB     = $65;

//  Registered Parameter Numbers [CC# 65H,64H]
// ------------------------------------------------------------
//  CC#65 (MSB) | CC#64 (LSB) | Function
//  Hex|Dec|    |  Hex|Dec|   |
//  - - - - - - | - - - - - - |- - - - - - - - - - - - - - - -
//   00 = 0     |  00 = 0     | Pitch Bend Sensitivity
//   00 = 0     |  01 = 1     | Channel Fine Tuning
//   00 = 0     |  02 = 2     | Channel Coarse Tuning
//   00 = 0     |  03 = 3     | Tuning Program Change
//   00 = 0     |  04 = 4     | Tuning Bank Select

  MIDICCAllSoundOff        = $78;
  MIDICCResetAllControllers = $79;
  MIDICCLocalControl       = $7A;
  MIDICCAllNotesOff        = $7B;

  MIDICCOmniModeOff        = $7C;
  MIDICCOmniModeOn         = $7D;
  MIDICCMonoModeOn         = $7E;
  MIDICCPolyModeOn         = $7F;

type
  TMIDIChannel          = 1..16;
  TMIDIDataByte         = 0..$7F;
  TMIDIStatusByte       = $80..$FF;
  TMIDIVelocity         = TMIDIDataByte;
  TMIDIKey              = TMIDIDataByte;
  TMIDINote             = TMIDIKey;
  TMIDIPitchBendValue   = 0..1 shl 14-1;

const
  // Helper definitions
  MIDIDataMask = $7F;
  MIDIChannelMsgMask = $F0;
  MIDIInvalidStatus = TMIDIStatusByte(0);
  MIDIPitchWheelCenter = 1 shl 13;

type
  TMIDINotes = set of TMIDINote;

  TSingleNoteTuningData = packed record
  case Integer of
    0:
      (Key: TMIDINote; Frequency: array[0..2] of TMIDIDataByte);
    1:
      (DWord: LongWord);
  end;

  EJclMIDIError = class (EJclError);

//--------------------------------------------------------------------------------------------------
// MIDI Out
//--------------------------------------------------------------------------------------------------

  IJclMIDIOut = interface
    ['{A29C3EBD-EB70-4C72-BEC5-700AF57FD4C8}']
    // property access methods
    function GetActiveNotes(Channel: TMIDIChannel): TMIDINotes;
    function GetName: string;
    function GetRunningStatusEnabled: Boolean;
    procedure SetRunningStatusEnabled(const Value: Boolean);
    // Channel Voice Messages
    procedure NoteOff(Channel: TMIDIChannel; Key: TMIDINote; Velocity: TMIDIDataByte);
    procedure NoteOn(Channel: TMIDIChannel; Key: TMIDINote; Velocity: TMIDIDataByte);
    procedure PolyphonicKeyPressure(Channel: TMIDIChannel; Key: TMIDINote; Value: TMIDIDataByte);
    procedure ControlChange(Channel: TMIDIChannel; ControllerNum, Value: TMIDIDataByte);
    procedure ProgramChange(Channel: TMIDIChannel; ProgramNum: TMIDIDataByte);
    procedure ChannelKeyPressure(Channel: TMIDIChannel; Value: TMIDIDataByte);
    procedure PitchWheelChange(Channel: TMIDIChannel; Value: TMIDIPitchBendValue);
    //
    procedure PitchBend(Channel: TMIDIChannel; Value: Single);
    procedure AllChannelNotesOff(Channel: TMIDIChannel);
    procedure AllNotesOff;
    procedure SingleNoteTuningChange(TuningData: array of TSingleNoteTuningData);
    function NoteIsOn(Channel: TMIDIChannel; Key: TMIDINote): Boolean;
    // Properties
    property ActiveNotes[Channel: TMIDIChannel]: TMIDINotes read GetActiveNotes;
    property Name: string read GetName;
      // Tribute to some braindead devices which cannot handle running status (e.g. ESS Solo 1 Win2k driver)
    property RunningStatusEnabled: Boolean read GetRunningStatusEnabled write SetRunningStatusEnabled;
  end;

  // Abstract MIDI Out device class
  TJclMIDIOut = class(TInterfacedObject, IJclMIDIOut)
  private
    //FDeviceID: Cardinal;
    FRunningStatus: TMIDIStatusByte;
    FRunningStatusEnabled: Boolean;
    FActiveNotes: array[TMIDIChannel] of TMIDINotes;
  protected
    function GetActiveNotes(Channel: TMIDIChannel): TMIDINotes;
    function GetName: string; virtual; abstract;
    function GetRunningStatusEnabled: Boolean;
    procedure SetRunningStatusEnabled(const Value: Boolean);
    procedure ChannelMessage(Msg: TMIDIStatusByte; Channel: TMIDIChannel;
      Data1, Data2: TMIDIDataByte);
    procedure ShortMessage(Status: TMIDIStatusByte; Data1, Data2: TMIDIDataByte); virtual; abstract;
    procedure LongMessage(Data: array of Byte); virtual; abstract;
  public
    destructor Destroy; override;
    procedure AllChannelNotesOff(Channel: TMIDIChannel);
    procedure AllNotesOff;
    // Channel Voice Messages
    procedure NoteOff(Channel: TMIDIChannel; Key: TMIDINote; Velocity: TMIDIDataByte);
    procedure NoteOn(Channel: TMIDIChannel; Key: TMIDINote; Velocity: TMIDIDataByte);
    procedure PolyphonicKeyPressure(Channel: TMIDIChannel; Key: TMIDINote; Value: TMIDIDataByte);
    procedure ControlChange(Channel: TMIDIChannel; ControllerNum, Value: TMIDIDataByte);
    procedure ProgramChange(Channel: TMIDIChannel; ProgramNum: TMIDIDataByte);
    procedure ChannelKeyPressure(Channel: TMIDIChannel; Value: TMIDIDataByte);
    procedure PitchWheelChange(Channel: TMIDIChannel; Value: TMIDIPitchBendValue);
    //
    procedure PitchBend(Channel: TMIDIChannel; Value: Single);
    procedure SingleNoteTuningChange(TuningData: array of TSingleNoteTuningData);
    function NoteIsOn(Channel: TMIDIChannel; Key: TMIDINote): Boolean;
    property ActiveNotes[Channel: TMIDIChannel]: TMIDINotes read GetActiveNotes;
    property Name: string read GetName;
    property RunningStatusEnabled: Boolean read GetRunningStatusEnabled write SetRunningStatusEnabled;
  end;

function MIDIOut(DeviceID: Cardinal = 0): IJclMIDIOut;
function MIDISingleNoteTuningData(Key: TMIDINote; Frequency: Single): TSingleNoteTuningData;
function MIDINoteToStr(Note: TMIDINote): string;

implementation

uses
{$IFDEF MSWINDOWS}
  JclWinMIDI,
{$ENDIF MSWINDOWS}
  SysUtils;

//--------------------------------------------------------------------------------------------------

function MIDIOut(DeviceID: Cardinal = 0): IJclMIDIOut;
begin
  Result := nil;
{$IFDEF MSWINDOWS}
  Result := JclWinMIDI.MIDIOut(DeviceID);
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  { TODO : Linux MIDIOut }
{$ENDIF LINUX}
end;

//--------------------------------------------------------------------------------------------------

function MIDISingleNoteTuningData(
  Key: TMIDINote;
  Frequency: Single): TSingleNoteTuningData;
var
  F: Cardinal;
begin
  Result.Key := Key;
  F := Trunc(Frequency * (1 shl 14));
  Result.Frequency[0] := (F shr 14) and MIDIDataMask;
  Result.Frequency[1] := (F shr 7) and MIDIDataMask;
  Result.Frequency[2] := F and MIDIDataMask;
end;

//--------------------------------------------------------------------------------------------------

procedure CheckMIDIChannelNum(Channel: TMIDIChannel);
begin
  if (Channel < Low(TMIDIChannel)) or (Channel > High(TMIDIChannel)) then
    raise EJclMIDIError.CreateFmt('Invalid MIDI channel number (%d)', [Channel]);
end;

//--------------------------------------------------------------------------------------------------

const
  HalftonesPerOctave = 12;
  OctaveNotes: array[0..HalftonesPerOctave-1] of string[2] =
    ('C','C#','D','D#','E','F','F#','G','G#','A','A#','B');

function MIDINoteToStr(Note: TMIDINote): string;
begin
  Result := Format('%s%d', [OctaveNotes[Note mod HalftonesPerOctave], Note div HalftonesPerOctave -2]);
end;

//==================================================================================================
// TJclMIDIOut
//==================================================================================================

destructor TJclMIDIOut.Destroy;
begin
  AllNotesOff;
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TJclMIDIOut.GetActiveNotes(Channel: TMIDIChannel): TMIDINotes;
begin
  CheckMIDIChannelNum(Channel);
  Result := FActiveNotes[Channel];
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.AllChannelNotesOff(Channel: TMIDIChannel);
begin
  CheckMIDIChannelNum(Channel);
  ControlChange(Channel, MIDICCAllNotesOff, 0);
  FActiveNotes[Channel] := [];
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.AllNotesOff;
var
  Channel: TMIDIChannel;
begin
  for Channel := Low(FActiveNotes) to High(FActiveNotes) do
    if FActiveNotes[Channel] <> [] then
      AllChannelNotesOff(Channel);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.ChannelMessage(Msg: TMIDIStatusByte;
  Channel: TMIDIChannel; Data1, Data2: TMIDIDataByte);
begin
  ShortMessage(Msg or (Channel - Low(Channel)), Data1, Data2);
end;

//--------------------------------------------------------------------------------------------------

function TJclMIDIOut.GetRunningStatusEnabled: Boolean;
begin
  Result := FRunningStatusEnabled;
end;

//--------------------------------------------------------------------------------------------------

function TJclMIDIOut.NoteIsOn(Channel: TMIDIChannel; Key: TMIDINote): Boolean;
begin
  Result := Key in FActiveNotes[Channel];
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.NoteOff(Channel: TMIDIChannel; Key: TMIDINote; Velocity: TMIDIDataByte);
begin
  ChannelMessage(MIDIMsgNoteOff, Channel, Key, Velocity);
  Exclude(FActiveNotes[Channel], Key);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.NoteOn(Channel: TMIDIChannel; Key: TMIDINote; Velocity: TMIDIDataByte);
begin
  ChannelMessage(MIDIMsgNoteOn, Channel, Key, Velocity);
  Include(FActiveNotes[Channel], Key);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.PolyphonicKeyPressure(Channel: TMIDIChannel;
  Key: TMIDINote; Value: TMIDIDataByte);
begin
  ChannelMessage(MIDIMsgPolyKeyPressure, Channel, Key, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.ControlChange(Channel: TMIDIChannel; ControllerNum, Value: TMIDIDataByte);
begin
  ChannelMessage(MIDIMsgControlChange, Channel, ControllerNum, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.ProgramChange(Channel: TMIDIChannel; ProgramNum: TMIDIDataByte);
begin
  ChannelMessage(MIDIMsgProgramChange, Channel, ProgramNum, 0);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.ChannelKeyPressure(Channel: TMIDIChannel;
  Value: TMIDIDataByte);
begin
  ChannelMessage(MIDIMsgChannelKeyPressure, Channel, Value, 0);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.PitchWheelChange(Channel: TMIDIChannel; Value: TMIDIPitchBendValue);
begin
  ChannelMessage(MIDIMsgPitchWheelChange, Channel, Value, Value shr 7);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.PitchBend(Channel: TMIDIChannel; Value: Single);
var
  Temp: Word;
begin
  if Value < 0 then
    Temp := Round(Value * (1 shl 13))
  else
    Temp := Round(Value * (1 shl 13 - 1));
  PitchWheelChange(Channel, Temp);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.SetRunningStatusEnabled(const Value: Boolean);
begin
  FRunningStatus := MIDIInvalidStatus;
  FRunningStatusEnabled := Value;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.SingleNoteTuningChange(TuningData: array of TSingleNoteTuningData);
var
  Count: Integer;
  Buf: PByteArray;
  BufSize: Integer;
begin
  Count := High(TuningData)-Low(TuningData)+1;
  BufSize := 8 + Count * SizeOf(TSingleNoteTuningData);
  GetMem(Buf, BufSize);
  try
    Buf[0] := MIDIMsgSysEx;  // Universal Real Time SysEx header, first byte
    Buf[1] := $7F;	// second byte
    Buf[2] := 0;	// ID of target device (?)
    Buf[3] := 8;	// sub-ID#1 (MIDI Tuning)
    Buf[4] := 2;	// sub-ID#2 (note change)
    Buf[5] := 0;	// tuning program number (0 – 127)
    Buf[6] := Count;
    Move(TuningData, Buf[7], Count * SizeOf(TSingleNoteTuningData));
    Buf[BufSize-1] := MIDIMsgEOX;
    LongMessage(Slice(Buf^, BufSize));
  finally
    FreeMem(Buf);
  end;
end;

end.
