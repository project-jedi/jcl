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
{ Last modified: June 5, 2002                                                                     }
{                                                                                                  }
{**************************************************************************************************}

unit JclMIDI;

interface

{$I jcl.inc}

uses JclBase, Classes;

// manifest constants for MIDI message protocol
const
  // MIDI Status Bytes for Channel Voice Messages
  MIDIMsgNoteOff             = $80;
  MIDIMsgNoteOn              = $90;
  MIDIMsgPolyKeyPressure     = $A0;
  MIDIMsgControlChange       = $B0;
  MIDIMsgProgramChange       = $C0;
  MIDIMsgChannelKeyPressure  = $D0;
  MIDIMsgAftertouch = MIDIMsgChannelKeyPressure; // Synonym
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

  // Channel Mode Messages (Control Change >= $78)
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
  TMIDIDataByte         = 0..$7F;           //  7 bits
  TMIDIDataWord         = 0..$3FFF;         // 14 bits
  TMIDIStatusByte       = $80..$FF;
  TMIDIVelocity         = TMIDIDataByte;
  TMIDIKey              = TMIDIDataByte;
  TMIDINote             = TMIDIKey;

const
  // Helper definitions
  MIDIDataMask          = $7F;
  MIDIDataWordMask      = $3FFF;
  MIDIChannelMsgMask    = $F0;
  MIDIInvalidStatus     = TMIDIStatusByte(0);
  BitsPerMIDIDataByte   = 7;
  BitsPerMIDIDataWord   = BitsPerMIDIDataByte * 2;
  MIDIPitchWheelCenter  = 1 shl (BitsPerMIDIDataWord - 1);

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
    function GetMIDIStatus: TMIDIStatusByte;
    function GetRunningStatusEnabled: Boolean;
    procedure SetRunningStatusEnabled(const Value: Boolean);
    // Channel Voice Messages
    procedure NoteOff(Channel: TMIDIChannel; Key: TMIDINote; Velocity: TMIDIDataByte);
    procedure NoteOn(Channel: TMIDIChannel; Key: TMIDINote; Velocity: TMIDIDataByte);
    procedure PolyphonicKeyPressure(Channel: TMIDIChannel; Key: TMIDINote; Value: TMIDIDataByte);
    procedure ControlChange(Channel: TMIDIChannel; ControllerNum, Value: TMIDIDataByte);
    // High Resolution "macro" for controller numbers <= $13, sends upper 7 bits first,
    //   lower 7 bits per additional <controller name>LSB message afterwards
    procedure ControlChangeHR(Channel: TMIDIChannel; ControllerNum: TMIDIDataByte; Value: TMIDIDataWord);
    procedure SetSwitch(Channel: TMIDIChannel; ControllerNum: TMIDIDataByte; Value: Boolean);
    procedure ProgramChange(Channel: TMIDIChannel; ProgramNum: TMIDIDataByte);
    procedure ChannelPressure(Channel: TMIDIChannel; Value: TMIDIDataByte);
    procedure PitchWheelChange(Channel: TMIDIChannel; Value: TMIDIDataWord);
    // Control Change Messages
    procedure SelectProgram(Channel: TMIDIChannel; BankNum: TMIDIDataWord; ProgramNum: TMIDIDataByte);
    procedure ModulationWheel(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure BreathControl(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure FootController(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure PortamentoTime(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure DataEntry(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure MainVolume(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure Balance(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure Pan(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure Expression(Channel: TMIDIChannel; Value: TMidiDataByte);
    // "high resolution" variants
    procedure ModulationWheelHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure BreathControlHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure FootControllerHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure PortamentoTimeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure DataEntryHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure MainVolumeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure BalanceHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure PanHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure ExpressionHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    // Control Change Messages: Switches
    procedure Sustain(Channel: TMIDIChannel; Value: Boolean);
    procedure Portamento(Channel: TMIDIChannel; Value: Boolean);
    procedure Sustenuto(Channel: TMIDIChannel; Value: Boolean);
    procedure SoftPedal(Channel: TMIDIChannel; Value: Boolean);
    procedure Legato(Channel: TMIDIChannel; Value: Boolean);
    procedure Hold2(Channel: TMIDIChannel; Value: Boolean);
    // Channel Mode Messages
    procedure AllSoundOff(Channel: TMIDIChannel);
    procedure ResetAllControllers(Channel: TMIDIChannel);
    procedure SetLocalControl(Channel: TMIDIChannel; Value: Boolean);
    procedure AllNotesOff(Channel: TMIDIChannel);
    procedure OmniModeOff(Channel: TMIDIChannel);
    procedure OmniModeOn(Channel: TMIDIChannel);
    procedure MonoModeOn(Channel: TMIDIChannel; ChannelCount: Integer);
    procedure PolyModeOn(Channel: TMIDIChannel);
    //
    procedure PitchBend(Channel: TMIDIChannel; Value: Single);
    procedure SingleNoteTuningChange(TuningData: array of TSingleNoteTuningData);
    function NoteIsOn(Channel: TMIDIChannel; Key: TMIDINote): Boolean;
    procedure ActiveNotesOff(Channel: TMIDIChannel); overload;
    procedure ActiveNotesOff; overload;
    // Properties
    property ActiveNotes[Channel: TMIDIChannel]: TMIDINotes read GetActiveNotes;
    property Name: string read GetName;
    property LocalControl[Channel: TMIDIChannel]: Boolean write SetLocalControl;
    property MIDIStatus: TMIDIStatusByte read GetMIDIStatus;
      // Tribute to some braindead devices which cannot handle running status (e.g. ESS Solo 1 Win2k driver)
    property RunningStatusEnabled: Boolean read GetRunningStatusEnabled write SetRunningStatusEnabled;
  end;

  // Abstract MIDI Out device class
  TJclMIDIOut = class (TInterfacedObject, IJclMIDIOut)
  private
    FMIDIStatus: TMIDIStatusByte;
    FRunningStatusEnabled: Boolean;
    FActiveNotes: array[TMIDIChannel] of TMIDINotes;
  protected
    function GetActiveNotes(Channel: TMIDIChannel): TMIDINotes;
    function GetName: string; virtual; abstract;
    function GetMIDIStatus: TMIDIStatusByte;
    function IsRunningStatus(StatusByte: TMIDIStatusByte): Boolean;
    function GetRunningStatusEnabled: Boolean;
    procedure SetRunningStatusEnabled(const Value: Boolean);
    procedure ChannelMessage(Msg: TMIDIStatusByte; Channel: TMIDIChannel;
      Data1, Data2: TMIDIDataByte);
    procedure DoSendMessage(const Data: array of Byte); virtual; abstract;
    procedure SendMessage(const Data: array of Byte);
  public
    destructor Destroy; override;
    // Channel Voice Messages
    procedure NoteOff(Channel: TMIDIChannel; Key: TMIDINote; Velocity: TMIDIDataByte);
    procedure NoteOn(Channel: TMIDIChannel; Key: TMIDINote; Velocity: TMIDIDataByte);
    procedure PolyphonicKeyPressure(Channel: TMIDIChannel; Key: TMIDINote; Value: TMIDIDataByte);
    procedure ControlChange(Channel: TMIDIChannel; ControllerNum, Value: TMIDIDataByte);
    procedure ControlChangeHR(Channel: TMIDIChannel; ControllerNum: TMIDIDataByte; Value: TMIDIDataWord);
    procedure SetSwitch(Channel: TMIDIChannel; ControllerNum: TMIDIDataByte; Value: Boolean);
    procedure ProgramChange(Channel: TMIDIChannel; ProgramNum: TMIDIDataByte);
    procedure ChannelPressure(Channel: TMIDIChannel; Value: TMIDIDataByte);
    procedure PitchWheelChange(Channel: TMIDIChannel; Value: TMIDIDataWord);
    // Control Change Messages
    procedure SelectProgram(Channel: TMIDIChannel; BankNum: TMIDIDataWord; ProgramNum: TMIDIDataByte);
    procedure ModulationWheel(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure BreathControl(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure FootController(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure PortamentoTime(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure DataEntry(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure MainVolume(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure Balance(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure Pan(Channel: TMIDIChannel; Value: TMidiDataByte);
    procedure Expression(Channel: TMIDIChannel; Value: TMidiDataByte);
    // ...high Resolution
    procedure ModulationWheelHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure BreathControlHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure FootControllerHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure PortamentoTimeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure DataEntryHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure MainVolumeHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure BalanceHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure PanHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    procedure ExpressionHR(Channel: TMIDIChannel; Value: TMidiDataWord);
    // Control Change Messages: Switches
    procedure Sustain(Channel: TMIDIChannel; Value: Boolean);
    procedure Portamento(Channel: TMIDIChannel; Value: Boolean);
    procedure Sustenuto(Channel: TMIDIChannel; Value: Boolean);
    procedure SoftPedal(Channel: TMIDIChannel; Value: Boolean);
    procedure Legato(Channel: TMIDIChannel; Value: Boolean);
    procedure Hold2(Channel: TMIDIChannel; Value: Boolean);
    // Channel Mode Messages
    procedure AllSoundOff(Channel: TMIDIChannel);
    procedure ResetAllControllers(Channel: TMIDIChannel);
    procedure SetLocalControl(Channel: TMIDIChannel; Value: Boolean);
    procedure AllNotesOff(Channel: TMIDIChannel);
    procedure OmniModeOff(Channel: TMIDIChannel);
    procedure OmniModeOn(Channel: TMIDIChannel);
    procedure MonoModeOn(Channel: TMIDIChannel; ChannelCount: Integer);
    procedure PolyModeOn(Channel: TMIDIChannel);
    //
    procedure PitchBend(Channel: TMIDIChannel; Value: Single);
    procedure SingleNoteTuningChange(TuningData: array of TSingleNoteTuningData);
    function NoteIsOn(Channel: TMIDIChannel; Key: TMIDINote): Boolean;
    procedure ActiveNotesOff(Channel: TMIDIChannel); overload;
    procedure ActiveNotesOff; overload;
    property ActiveNotes[Channel: TMIDIChannel]: TMIDINotes read GetActiveNotes;
    property Name: string read GetName;
    property RunningStatusEnabled: Boolean read GetRunningStatusEnabled write SetRunningStatusEnabled;
  end;

function MIDIOut(DeviceID: Cardinal = 0): IJclMIDIOut;
procedure GetMidiOutputs(const List: TStrings);
function MIDISingleNoteTuningData(Key: TMIDINote; Frequency: Single): TSingleNoteTuningData;
function MIDINoteToStr(Note: TMIDINote): string;

implementation

uses
  JclResources,
{$IFDEF MSWINDOWS}
  JclWinMIDI,
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  JclUnixMIDI,
{$ENDIF LINUX}
  SysUtils;

//--------------------------------------------------------------------------------------------------

function MIDIOut(DeviceID: Cardinal = 0): IJclMIDIOut;
begin
  Result := nil;
{$IFDEF MSWINDOWS}
  Result := JclWinMIDI.MIDIOut(DeviceID);
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{ TODO -oRobert Rossmair : Linux MIDI Out }
  //Result := JclUnixMIDI.MidiOut(DeviceID);
{$ENDIF LINUX}
end;

//--------------------------------------------------------------------------------------------------

procedure GetMidiOutputs(const List: TStrings);
begin
{$IFDEF MSWINDOWS}
  JclWinMIDI.GetMidiOutputs(List);
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{ TODO -oRobert Rossmair : Linux GetMIDIOutputs }
  //JclUnixMIDI.GetMidiOutputs(List);
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
  F := Trunc(Frequency * (1 shl BitsPerMIDIDataWord));
  Result.Frequency[0] := (F shr BitsPerMIDIDataWord) and MIDIDataMask;
  Result.Frequency[1] := (F shr BitsPerMIDIDataByte) and MIDIDataMask;
  Result.Frequency[2] := F and MIDIDataMask;
end;

//--------------------------------------------------------------------------------------------------

procedure CheckMIDIChannelNum(Channel: TMIDIChannel);
begin
  if (Channel < Low(TMIDIChannel)) or (Channel > High(TMIDIChannel)) then
    raise EJclMIDIError.CreateFmt(RsInvalidMidiChannelNum, [Channel]);
end;

//--------------------------------------------------------------------------------------------------

function MIDINoteToStr(Note: TMIDINote): string;
const
  HalftonesPerOctave = 12;
begin
  case Note mod HalftonesPerOctave of
     0: Result := RsOctaveC;
     1: Result := RsOctaveCSharp;
     2: Result := RsOctaveD;
     3: Result := RsOctaveDSharp;
     4: Result := RsOctaveE;
     5: Result := RsOctaveF;
     6: Result := RsOctaveFSharp;
     7: Result := RsOctaveG;
     8: Result := RsOctaveGSharp;
     9: Result := RsOctaveA;
    10: Result := RsOctaveASharp;
    11: Result := RsOctaveB;
  end;
  Result := Format('%s%d', [Result, Note div HalftonesPerOctave -2]);
end;

//==================================================================================================
// TJclMIDIOut
//==================================================================================================

destructor TJclMIDIOut.Destroy;
begin
  ActiveNotesOff;
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TJclMIDIOut.GetActiveNotes(Channel: TMIDIChannel): TMIDINotes;
begin
  CheckMIDIChannelNum(Channel);
  Result := FActiveNotes[Channel];
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.ChannelMessage(Msg: TMIDIStatusByte;
  Channel: TMIDIChannel; Data1, Data2: TMIDIDataByte);
begin
  SendMessage([Msg or (Channel - Low(Channel)), Data1, Data2]);
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

procedure TJclMIDIOut.ControlChangeHR(Channel: TMIDIChannel; ControllerNum: TMIDIDataByte;
  Value: TMIDIDataWord);
begin
  ControlChange(Channel, ControllerNum, Value shr BitsPerMIDIDataByte and MIDIDataMask);
  if ControllerNum <= $13 then
    ControlChange(Channel, ControllerNum or $20, Value and MIDIDataMask);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.SetSwitch(Channel: TMIDIChannel; ControllerNum: TMIDIDataByte; Value: Boolean);
const
  DataByte: array [Boolean] of Byte = ($00, $7F);
begin
  ChannelMessage(MIDIMsgControlChange, Channel, ControllerNum, DataByte[Value]);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.ProgramChange(Channel: TMIDIChannel; ProgramNum: TMIDIDataByte);
begin
  ChannelMessage(MIDIMsgProgramChange, Channel, ProgramNum, 0);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.ChannelPressure(Channel: TMIDIChannel;
  Value: TMIDIDataByte);
begin
  ChannelMessage(MIDIMsgChannelKeyPressure, Channel, Value, 0);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.PitchWheelChange(Channel: TMIDIChannel; Value: TMIDIDataWord);
begin
  ChannelMessage(MIDIMsgPitchWheelChange, Channel, Value and MidiDataMask, Value shr BitsPerMIDIDataByte);
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

procedure TJclMIDIOut.AllSoundOff(Channel: TMIDIChannel);
begin
  ControlChange(Channel, MIDICCAllSoundOff, 0);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.SetLocalControl(Channel: TMIDIChannel; Value: Boolean);
begin
  SetSwitch(Channel, MIDICCLocalControl, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.ResetAllControllers(Channel: TMIDIChannel);
begin
  ControlChange(Channel, MIDICCResetAllControllers, 0);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.AllNotesOff(Channel: TMIDIChannel);
begin
  CheckMIDIChannelNum(Channel);
  ControlChange(Channel, MIDICCAllNotesOff, 0);
  FActiveNotes[Channel] := [];
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.SetRunningStatusEnabled(const Value: Boolean);
begin
  FMIDIStatus := MIDIInvalidStatus;
  FRunningStatusEnabled := Value;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.SingleNoteTuningChange(TuningData: array of TSingleNoteTuningData);
var
  Count: Integer;
  Buf: PByteArray;
  BufSize: Integer;
begin
  Count := High(TuningData) - Low(TuningData) + 1;
  BufSize := 8 + Count * SizeOf(TSingleNoteTuningData);
  GetMem(Buf, BufSize);
  try
    Buf[0] := MIDIMsgSysEx;  // Universal Real Time SysEx header, first byte
    Buf[1] := $7F;           // second byte
    Buf[2] := 0;             // ID of target device (?)
    Buf[3] := 8;             // sub-ID#1 (MIDI Tuning)
    Buf[4] := 2;             // sub-ID#2 (note change)
    Buf[5] := 0;             // tuning program number (0 – 127)
    Buf[6] := Count;
    Move(TuningData, Buf[7], Count * SizeOf(TSingleNoteTuningData));
    Buf[BufSize - 1] := MIDIMsgEOX;
    SendMessage(Slice(Buf^, BufSize));
  finally
    FreeMem(Buf);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.ActiveNotesOff(Channel: TMIDIChannel);
var
  Note: TMIDINote;
begin
  CheckMIDIChannelNum(Channel);
  if FActiveNotes[Channel] <> [] then
    for Note := Low(Note) to High(Note) do
      if Note in FActiveNotes[Channel] then
        NoteOff(Channel, Note, $7F);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.ActiveNotesOff;
var
  Channel: TMIDIChannel;
begin
  for Channel := Low(Channel) to High(Channel) do
    ActiveNotesOff(Channel);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.SelectProgram(Channel: TMIDIChannel;
  BankNum: TMIDIDataWord; ProgramNum: TMIDIDataByte);
begin
  ControlChangeHR(Channel, MIDICCBankSelect, BankNum);
  ProgramChange(Channel, ProgramNum);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.SendMessage(const Data: array of Byte);
begin
  if IsRunningStatus(Data[0]) then
    DoSendMessage(Slice(Data, 1))
  else
    DoSendMessage(Data);
end;

//--------------------------------------------------------------------------------------------------

function TJclMIDIOut.GetMIDIStatus: TMIDIStatusByte;
begin
  Result := FMIDIStatus;
end;

//--------------------------------------------------------------------------------------------------

function TJclMIDIOut.IsRunningStatus(StatusByte: TMIDIStatusByte): Boolean;
begin
  Result := (StatusByte = FMIDIStatus)
    and ((StatusByte and $F0) <> $F0)       // is channel message
    and RunningStatusEnabled;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.Balance(Channel: TMIDIChannel; Value: TMidiDataByte);
begin
  ControlChange(Channel, MIDICCBalance, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.BalanceHR(Channel: TMIDIChannel; Value: TMidiDataWord);
begin
  ControlChangeHR(Channel, MIDICCBalance, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.BreathControl(Channel: TMIDIChannel; Value: TMidiDataByte);
begin
  ControlChange(Channel, MIDICCBreathControl, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.BreathControlHR(Channel: TMIDIChannel; Value: TMidiDataWord);
begin
  ControlChangeHR(Channel, MIDICCBreathControl, Value);
end;
//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.DataEntry(Channel: TMIDIChannel; Value: TMidiDataByte);
begin
  ControlChange(Channel, MIDICCDataEntry, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.DataEntryHR(Channel: TMIDIChannel; Value: TMidiDataWord);
begin
  ControlChangeHR(Channel, MIDICCDataEntry, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.Expression(Channel: TMIDIChannel; Value: TMidiDataByte);
begin
  ControlChange(Channel, MIDICCExpression, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.ExpressionHR(Channel: TMIDIChannel; Value: TMidiDataWord);
begin
  ControlChangeHR(Channel, MIDICCExpression, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.FootController(Channel: TMIDIChannel; Value: TMidiDataByte);
begin
  ControlChange(Channel, MIDICCFootController, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.FootControllerHR(Channel: TMIDIChannel; Value: TMidiDataWord);
begin
  ControlChangeHR(Channel, MIDICCFootController, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.Hold2(Channel: TMIDIChannel; Value: Boolean);
begin
  SetSwitch(Channel, MIDICCHold2, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.Legato(Channel: TMIDIChannel; Value: Boolean);
begin
  SetSwitch(Channel, MIDICCLegato, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.MainVolume(Channel: TMIDIChannel;
  Value: TMidiDataByte);
begin
  ControlChange(Channel, MIDICCMainVolume, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.MainVolumeHR(Channel: TMIDIChannel;
  Value: TMidiDataWord);
begin
  ControlChangeHR(Channel, MIDICCMainVolume, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.ModulationWheel(Channel: TMIDIChannel;
  Value: TMidiDataByte);
begin
  ControlChange(Channel, MIDICCModulationWheel, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.ModulationWheelHR(Channel: TMIDIChannel; Value: TMidiDataWord);
begin
  ControlChangeHR(Channel, MIDICCModulationWheel, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.Pan(Channel: TMIDIChannel; Value: TMidiDataByte);
begin
  ControlChange(Channel, MIDICCPan, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.PanHR(Channel: TMIDIChannel; Value: TMidiDataWord);
begin
  ControlChangeHR(Channel, MIDICCPan, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.Portamento(Channel: TMIDIChannel; Value: Boolean);
begin
  SetSwitch(Channel, MIDICCPortamento, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.PortamentoTime(Channel: TMIDIChannel;
  Value: TMidiDataByte);
begin
  ControlChange(Channel, MIDICCPortamentoTime, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.PortamentoTimeHR(Channel: TMIDIChannel;
  Value: TMidiDataWord);
begin
  ControlChangeHR(Channel, MIDICCPortamentoTime, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.SoftPedal(Channel: TMIDIChannel; Value: Boolean);
begin
  SetSwitch(Channel, MIDICCSoftPedal, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.Sustain(Channel: TMIDIChannel; Value: Boolean);
begin
  SetSwitch(Channel, MIDICCSustain, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.Sustenuto(Channel: TMIDIChannel; Value: Boolean);
begin
  SetSwitch(Channel, MIDICCSustenuto, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.OmniModeOff(Channel: TMIDIChannel);
begin
  ControlChange(Channel, MIDICCOmniModeOff, 0);
  FActiveNotes[Channel] := []; // implicite All Notes Off
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.OmniModeOn(Channel: TMIDIChannel);
begin
  ControlChange(Channel, MIDICCOmniModeOn, 0);
  FActiveNotes[Channel] := []; // implicite All Notes Off
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.MonoModeOn(Channel: TMIDIChannel; ChannelCount: Integer);
begin
  ControlChange(Channel, MIDICCMonoModeOn, ChannelCount);
  FActiveNotes[Channel] := []; // implicite All Notes Off
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMIDIOut.PolyModeOn(Channel: TMIDIChannel);
begin
  ControlChange(Channel, MIDICCPolyModeOn, 0);
  FActiveNotes[Channel] := []; // implicite All Notes Off
end;

end.
