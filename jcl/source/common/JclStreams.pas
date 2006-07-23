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
{ The Original Code is JclStreams.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Robert Marquardt. Portions created by              }
{ Robert Marquardt are Copyright (C) Robert Marquardt (robert_marquardt att gmx dott de)           }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Florent Ouchet (outchy)                                                                        }
{   Heinz Zastrau                                                                                  }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$

unit JclStreams;

{$I jcl.inc}

interface

uses
{$IFDEF UNITVERSIONING}
  JclUnitVersioning,
{$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Libc,
  {$ENDIF LINUX}
  SysUtils, Classes;

type
  {$IFDEF COMPILER5}
  TSeekOrigin = (soBeginning, soCurrent, soEnd);
  {$ENDIF COMPILER5}

  EJclStreamError = class(Exception);
  
  // abstraction layer to support Delphi 5 and C++Builder 5 streams
  // 64 bit version of overloaded functions are introduced
  TJclStream = class(TStream)
  protected
    procedure SetSize(NewSize: Longint); overload; override;
    procedure SetSize(const NewSize: Int64);
      {$IFDEF COMPILER5} reintroduce; overload; virtual; {$ELSE} overload; override; {$ENDIF}
  public
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
      {$IFDEF COMPILER5} reintroduce; overload; virtual; {$ELSE} overload; override; {$ENDIF}
  end;

  //=== VCL stream replacements ===

  TJclHandleStream = class(TJclStream)
  private
    FHandle: THandle;
  protected
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(AHandle: THandle); virtual;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property Handle: THandle read FHandle;
  end;

  TJclFileStream = class(TJclHandleStream)
  public
    constructor Create(const FileName: string; Mode: Word; Rights: Cardinal = 0); reintroduce; virtual;
    destructor Destroy; override;
  end;

  {
  TJclCustomMemoryStream = class(TJclStream)
  end;

  TJclMemoryStream = class(TJclCustomMemoryStream)
  end;

  TJclStringStream = class(TJclStream)
  end;

  TJclResourceStream = class(TJclCustomMemoryStream)
  end;
  }

  //=== new stream ideas ===

  TJclEmptyStream = class(TJclStream)
  protected
    procedure SetSize(NewSize: Longint); overload; override;
    procedure SetSize(const NewSize: Int64); overload; override;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  TJclNullStream = class(TJclStream)
  private
    FPosition: Int64;
    FSize: Int64;
  protected
    procedure SetSize(const NewSize: Int64); override;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  TJclRandomStream = class(TJclNullStream)
  protected
    function GetRandSeed: Longint; virtual;
    procedure SetRandSeed(Seed: Longint); virtual;
  public
    function RandomData: Byte; virtual;
    procedure Randomize; dynamic;
    function Read(var Buffer; Count: Longint): Longint; override;
    property RandSeed: Longint read GetRandSeed write SetRandSeed;
  end;

  TJclMultiplexStream = class(TJclStream)
  private
    FStreams: TList;
    FReadStreamIndex: Integer;
    function GetStream(Index: Integer): TStream;
    function GetCount: Integer;
    procedure SetStream(Index: Integer; const Value: TStream);
    function GetReadStream: TStream;
    procedure SetReadStream(const Value: TStream);
    procedure SetReadStreamIndex(const Value: Integer);
  protected
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    function Add(NewStream: TStream): Integer;
    procedure Clear;
    function Remove(AStream: TStream): Integer;
    procedure Delete(const Index: Integer);

    property Streams[Index: Integer]: TStream read GetStream write SetStream;
    property ReadStreamIndex: Integer read FReadStreamIndex write SetReadStreamIndex;
    property ReadStream: TStream read GetReadStream write SetReadStream;
    property Count: Integer read GetCount;
  end;

  TJclStreamDecorator = class(TJclStream)
  private
    FAfterStreamChange: TNotifyEvent;
    FBeforeStreamChange: TNotifyEvent;
    FOwnsStream: Boolean;
    FStream: TStream;
    procedure SetStream(Value: TStream);
  protected
    procedure DoAfterStreamChange; virtual;
    procedure DoBeforeStreamChange; virtual;
    procedure SetSize(NewSize: Longint); overload; override;
    procedure SetSize(const NewSize: Int64); overload; override;
  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean = False); virtual;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property AfterStreamChange: TNotifyEvent read FAfterStreamChange write FAfterStreamChange;
    property BeforeStreamChange: TNotifyEvent read FBeforeStreamChange write FBeforeStreamChange;
    property OwnsStream: Boolean read FOwnsStream write FOwnsStream;
    property Stream: TStream read FStream write SetStream;
  end;

  TJclBufferedStream = class(TJclStreamDecorator)
  private
    FBuffer: array of Byte;
    FBufferCurrentSize: Longint;
    FBufferMaxModifiedPos: Longint;
    FBufferSize: Longint;
    FBufferStart: Longint;
    FPosition: Longint;
    FSize: Longint;
    function BufferHit: Boolean;
    function GetCalcedSize: Longint;
    function LoadBuffer: Boolean;
    procedure SetBufferSize(Value: Longint);
    function ReadFromBuffer(var Buffer; Count, Start: Longint): Longint;
    function WriteToBuffer(const Buffer; Count, Start: Longint): Longint;
  protected
    procedure DoAfterStreamChange; override;
    procedure DoBeforeStreamChange; override;
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean = False); override;
    destructor Destroy; override;
    procedure Flush;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property BufferSize: Longint read FBufferSize write SetBufferSize;
  end;

  TStreamNotifyEvent = procedure(Sender: TObject; Position: Int64; Size: Int64) of object;

  TJclEventStream = class(TJclStreamDecorator)
  private
    FNotification: TStreamNotifyEvent;
    procedure DoNotification;
  protected
    procedure DoBeforeStreamChange; override;
    procedure DoAfterStreamChange; override;
    procedure SetSize(NewSize: Longint); overload; override;
    procedure SetSize(const NewSize: Int64); overload; override;
  public
    constructor Create(AStream: TStream; ANotification: TStreamNotifyEvent = nil;
      AOwnsStream: Boolean = False); reintroduce; virtual;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property OnNotification: TStreamNotifyEvent read FNotification write FNotification;
  end;

  TJclEasyStream = class(TJclStreamDecorator)
  public
    function IsEqual(Stream: TStream): Boolean;
    function ReadBoolean: Boolean;
    function ReadChar: Char;
    {$IFNDEF BCB}
    function ReadComp: Comp;
    {$ENDIF !BCB}
    function ReadCurrency: Currency;
    function ReadDateTime: TDateTime;
    function ReadDouble: Double;
    function ReadExtended: Extended;
    function ReadInt64: Int64;
    function ReadInteger: Integer;
    function ReadCString: string;
    function ReadShortString: string;
    function ReadSingle: Single;
    function ReadSizedString: string;
    procedure WriteBoolean(Value: Boolean);
    procedure WriteChar(Value: Char);
    {$IFNDEF BCB}
    procedure WriteComp(const Value: Comp);
    {$ENDIF !BCB}
    procedure WriteCurrency(const Value: Currency);
    procedure WriteDateTime(const Value: TDateTime);
    procedure WriteDouble(const Value: Double);
    procedure WriteExtended(const Value: Extended);
    procedure WriteInt64(Value: Int64); overload;
    procedure WriteInteger(Value: Integer); overload;
    procedure WriteStringDlimitedByNull(const Value: string);
    procedure WriteShortString(const Value: ShortString);
    procedure WriteSingle(const Value: Single);
    procedure WriteSizedString(const Value: string);
  end;


{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation


uses
  JclBase, JclResources;

//=== { TJclStream } =========================================================

function TJclStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  Result64: Int64;
begin
  case Origin of
    soFromBeginning:
      Result64 := Seek(Int64(Offset), soBeginning);
    soFromCurrent:
      Result64 := Seek(Int64(Offset), soCurrent);
    soFromEnd:
      Result64 := Seek(Int64(Offset), soEnd);
  else
    Result64 := -1;
  end;
  if (Result64 < 0) or (Result64 > High(Longint)) then
    Result64 := -1;
  Result := Result64;
end;

function TJclStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  // override to customize
  Result := -1;
end;

procedure TJclStream.SetSize(NewSize: Longint);
begin
  SetSize(Int64(NewSize));
end;

procedure TJclStream.SetSize(const NewSize: Int64);
begin
  // override to customize
end;

//=== { TJclHandleStream } ===================================================

constructor TJclHandleStream.Create(AHandle: THandle);
begin
  inherited Create;
  FHandle := AHandle;
end;

function TJclHandleStream.Read(var Buffer; Count: Longint): Longint;
begin
  {$IFDEF MSWINDOWS}
  if (Count <= 0) or not ReadFile(Handle, Buffer, DWORD(Count), DWORD(Result), nil) then
    Result := 0;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Result := __read(Handle, Buffer, Count);
  {$ENDIF LINUX}
end;

function TJclHandleStream.Write(const Buffer; Count: Longint): Longint;
begin
  {$IFDEF MSWINDOWS}
  if (Count <= 0) or not WriteFile(Handle, Buffer, DWORD(Count), DWORD(Result), nil) then
    Result := 0;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Result := __write(Handle, Buffer, Count);
  {$ENDIF LINUX}
end;

{$IFDEF MSWINDOWS}
function TJclHandleStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
const
  INVALID_SET_FILE_POINTER = -1;
type
  TLarge = record
    case Boolean of
    False:
     (OffsetLo: Longint;
      OffsetHi: Longint);
    True:
      (Offset64: Int64);
  end;
var
  Offs: TLarge;
begin
  Offs.Offset64 := Offset;
  Offs.OffsetLo := SetFilePointer(Handle, Offs.OffsetLo, @Offs.OffsetHi, Ord(Origin));
  if (Offs.OffsetLo = INVALID_SET_FILE_POINTER) and (GetLastError <> NO_ERROR) then
    Result := -1
  else
    Result := Offs.Offset64;
end;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
function TJclHandleStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := __lseek(Handle, Offset, Origin);
end;
{$ENDIF LINUX}

procedure TJclHandleStream.SetSize(NewSize: Longint);
begin
  SetSize(Int64(NewSize));
end;

procedure TJclHandleStream.SetSize(const NewSize: Int64);
begin
  Seek(NewSize, soBeginning);
  {$IFDEF MSWINDOWS}
  if not SetEndOfFile(Handle) then
    RaiseLastOSError;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  if ftruncate(Handle, Position) = -1 then
    raise EJclStreamError.CreateRes(@RsStreamsSetSizeError);
  {$ENDIF LINUX}
end;

//=== { TJclFileStream } =====================================================

constructor TJclFileStream.Create(const FileName: string; Mode: Word; Rights: Cardinal);
var
  H: THandle;
begin
  if Mode = fmCreate then
  begin
    H := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE,
      0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
    inherited Create(H);
    if Handle = INVALID_HANDLE_VALUE then
      {$IFDEF CLR}
      raise EJclStreamError.CreateFmt(RsStreamsCreateError, [FileName]);
      {$ELSE}
      raise EJclStreamError.CreateResFmt(@RsStreamsCreateError, [FileName]);
      {$ENDIF CLR}
  end
  else
  begin
    H := THandle(FileOpen(FileName, Mode));
    inherited Create(H);
    if Handle = INVALID_HANDLE_VALUE then
      {$IFDEF CLR}
      raise EJclStreamError.CreateFmt(RsStreamsOpenError, [FileName]);
      {$ELSE}
      raise EJclStreamError.CreateResFmt(@RsStreamsOpenError, [FileName]);
      {$ENDIF CLR}
  end;
end;

destructor TJclFileStream.Destroy;
begin
  {$IFDEF MSWINDOWS}
  if Handle <> INVALID_HANDLE_VALUE then
    CloseHandle(Handle);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  __close(Handle);
  {$ENDIF LINUX}
  inherited Destroy;
end;

//=== { TJclEmptyStream } ====================================================

// a stream which stays empty no matter what you do
// so it is a Unix /dev/null equivalent

procedure TJclEmptyStream.SetSize(NewSize: Longint); 
begin
  // nothing
end;

procedure TJclEmptyStream.SetSize(const NewSize: Int64);
begin
  // nothing
end;

function TJclEmptyStream.Read(var Buffer; Count: Longint): Longint;
begin
  // you cannot read anything
  Result := 0;
end;

function TJclEmptyStream.Write(const Buffer; Count: Longint): Longint;
begin
  // you cannot write anything
  Result := 0;
end;

function TJclEmptyStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if Offset <> 0 then
    // seeking to anywhere except the position 0 is an error
    Result := -1
  else
    Result := 0;
end;

//=== { TJclNullStream } =====================================================

// a stream which only keeps position and size, but no data
// so it is a Unix /dev/zero equivalent (?)

procedure TJclNullStream.SetSize(const NewSize: Int64);
begin
  if NewSize > 0 then
    FSize := NewSize
  else
    FSize := 0;
  if FPosition > FSize then
    FPosition := FSize;
end;

function TJclNullStream.Read(var Buffer; Count: Longint): Longint;
begin
  if Count < 0 then
    Count := 0;
  // FPosition > FSize is possible!
  if FSize - FPosition < Count then
    Count := FSize - FPosition;
  // does not read if beyond EOF
  if Count > 0 then
  begin
    FillChar(Buffer, Count, 0);
    FPosition := FPosition + Count;
  end;
  Result := Count;
end;

function TJclNullStream.Write(const Buffer; Count: Longint): Longint;
begin
  if Count < 0 then
    Count := 0;
  FPosition := FPosition + Count;
  // writing when FPosition > FSize is possible!
  if FPosition > FSize then
    FSize := FPosition;
  Result := Count;
end;

function TJclNullStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
  Rel: Int64;
begin
  case Origin of
    soBeginning:
      Rel := 0;
    soCurrent:
      Rel := FPosition;
    soEnd:
      Rel := FSize;
  else
    // force Rel + Offset = -1 (code is never reached)
    Rel := Offset - 1;
  end;
  if Rel + Offset >= 0 then
  begin
    // all non-negative destination positions including beyond EOF are valid
    FPosition := Rel + Offset;
    Result := FPosition;
  end
  else
    Result := -1;
end;

//=== { TJclRandomStream } ===================================================

// A TJclNullStream decendant which returns random data when read
// so it is a Unix /dev/random equivalent

function TJclRandomStream.GetRandSeed: Longint;
begin
  Result := System.RandSeed;
end;

procedure TJclRandomStream.SetRandSeed(Seed: Longint);
begin
  System.RandSeed := Seed;
end;

function TJclRandomStream.RandomData: Byte;
begin
  Result := Byte(System.Random(256));
end;

procedure TJclRandomStream.Randomize;
begin
  System.Randomize;
end;

function TJclRandomStream.Read(var Buffer; Count: Longint): Longint;
var
  I: Longint;
  BufferPtr: PByte;
begin
  // this handles all necessary checks
  Count := inherited Read(Buffer, Count);
  BufferPtr := @Buffer;
  for I := 0 to Count - 1 do
  begin
    BufferPtr^ := RandomData;
    Inc(BufferPtr);
  end;
  Result := Count;
end;

//=== { TJclMultiplexStream } ================================================

constructor TJclMultiplexStream.Create;
begin
  inherited Create;
  FStreams := TList.Create;
  FReadStreamIndex := -1;
end;

destructor TJclMultiplexStream.Destroy;
begin
  FStreams.Free;
  inherited Destroy;
end;

function TJclMultiplexStream.Add(NewStream: TStream): Integer;
begin
  Result := FStreams.Add(Pointer(NewStream));
end;

procedure TJclMultiplexStream.Clear;
begin
  FStreams.Clear;
  FReadStreamIndex := -1;
end;

procedure TJclMultiplexStream.Delete(const Index: Integer);
begin
  FStreams.Delete(Index);
  if ReadStreamIndex = Index then
    FReadStreamIndex := -1
  else
  if ReadStreamIndex > Index then
    Dec(FReadStreamIndex);
end;

function TJclMultiplexStream.GetReadStream: TStream;
begin
  if FReadStreamIndex >= 0 then
    Result := TStream(FStreams.Items[FReadStreamIndex])
  else
    Result := nil;
end;

function TJclMultiplexStream.GetStream(Index: Integer): TStream;
begin
  Result := TStream(FStreams.Items[Index]);
end;

function TJclMultiplexStream.GetCount: Integer;
begin
  Result := FStreams.Count;
end;

function TJclMultiplexStream.Read(var Buffer; Count: Longint): Longint;
var
  Stream: TStream;
begin
  Stream := ReadStream;
  if Assigned(Stream) then
    Result := Stream.Read(Buffer, Count)
  else
    Result := 0;
end;

function TJclMultiplexStream.Remove(AStream: TStream): Integer;
begin
  Result := FStreams.Remove(Pointer(AStream));
  if FReadStreamIndex = Result then
    FReadStreamIndex := -1
  else
  if FReadStreamIndex > Result then
    Dec(FReadStreamIndex);
end;

function TJclMultiplexStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  // what should this function do?
  Result := -1;
end;

procedure TJclMultiplexStream.SetReadStream(const Value: TStream);
begin
  FReadStreamIndex := FStreams.IndexOf(Pointer(Value));
end;

procedure TJclMultiplexStream.SetReadStreamIndex(const Value: Integer);
begin
  FReadStreamIndex := Value;
end;

procedure TJclMultiplexStream.SetSize(const NewSize: Int64);
begin
  // what should this function do?
end;

procedure TJclMultiplexStream.SetStream(Index: Integer; const Value: TStream);
begin
  FStreams.Items[Index] := Pointer(Value);
end;

function TJclMultiplexStream.Write(const Buffer; Count: Longint): Longint;
var
  Index: Integer;
  ByteWritten, MinByteWritten: Longint;
begin
  MinByteWritten := Count;
  for Index := 0 to Self.Count - 1 do
  begin
    ByteWritten := TStream(FStreams.Items[Index]).Write(Buffer, Count);
    if ByteWritten < MinByteWritten then
      MinByteWritten := ByteWritten;
  end;
  Result := MinByteWritten;
end;

//=== { TJclStreamDecorator } ================================================

constructor TJclStreamDecorator.Create(AStream: TStream; AOwnsStream: Boolean = False);
begin
  inherited Create;
  FStream := AStream;
  FOwnsStream := AOwnsStream;
end;

destructor TJclStreamDecorator.Destroy;
begin
  if OwnsStream then
    FStream.Free;
  inherited Destroy;
end;

procedure TJclStreamDecorator.DoAfterStreamChange;
begin
  if Assigned(FAfterStreamChange) then
    FAfterStreamChange(Self);
end;

procedure TJclStreamDecorator.DoBeforeStreamChange;
begin
  if Assigned(FBeforeStreamChange) then
    FBeforeStreamChange(Self);
end;

function TJclStreamDecorator.Read(var Buffer; Count: Longint): Longint;
begin
  if Assigned(FStream) then
    Result := Stream.Read(Buffer, Count)
  else
    Result := 0;
end;

function TJclStreamDecorator.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if Assigned(FStream) then
  begin
    {$IFDEF COMPILER5}
    if Stream is TJclStream then
      Result := TJclStream(Stream).Seek(Offset, Origin)
    else
    if (Offset <= MaxLongint) or (Offset > -MaxLongint) then
      Result := Stream.Seek(Longint(Offset), Ord(Origin))
    else
      Result := -1;
    {$ELSE}
    Result := Stream.Seek(Offset, Origin);
    {$ENDIF COMPILER5}
  end
  else
    Result := -1;
end;

function TJclStreamDecorator.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if Assigned(FStream) then
    Result := Stream.Seek(Offset, Origin)
  else
    Result := -1;
end;

procedure TJclStreamDecorator.SetSize(NewSize: Longint);
begin
  if Assigned(FStream) then
    Stream.Size := NewSize;
end;

procedure TJclStreamDecorator.SetSize(const NewSize: Int64);
begin
  if Assigned(FStream) then
    Stream.Size := NewSize;
end;

procedure TJclStreamDecorator.SetStream(Value: TStream);
begin
  if Value <> FStream then
    try
      DoBeforeStreamChange;
    finally
      if OwnsStream then
        FStream.Free;
      FStream := Value;
      DoAfterStreamChange;
    end;
end;

function TJclStreamDecorator.Write(const Buffer; Count: Longint): Longint;
begin
  if Assigned(FStream) then
    Result := Stream.Write(Buffer, Count)
  else
    Result := 0;
end;

//=== { TJclBufferedStream } =================================================

constructor TJclBufferedStream.Create(AStream: TStream; AOwnsStream: Boolean = False);
begin
  inherited Create(AStream, AOwnsStream);
  FSize := -1;
  FPosition := Stream.Position;
  BufferSize := 4096;
end;

destructor TJclBufferedStream.Destroy;
begin
  Flush;
  inherited Destroy;
end;

function TJclBufferedStream.BufferHit: Boolean;
begin
  Result := (FBufferStart <= FPosition) and (FPosition < FBufferCurrentSize);
end;

procedure TJclBufferedStream.DoAfterStreamChange;
begin
  inherited DoAfterStreamChange;
  if Stream <> nil then
    FPosition := Stream.Position;
end;

procedure TJclBufferedStream.DoBeforeStreamChange;
begin
  inherited DoBeforeStreamChange;
  Flush;
end;

procedure TJclBufferedStream.Flush;
begin
  if Stream = nil then
    Exit;
  if FBufferMaxModifiedPos > 0 then
  begin
    Stream.Position := FBufferStart;
    Stream.WriteBuffer(FBuffer[0], FBufferMaxModifiedPos);
    FSize := Stream.Size;
    FBufferMaxModifiedPos := 0;
  end;
end;

function TJclBufferedStream.GetCalcedSize: Longint;
begin
  if FSize < 0 then
    FSize := Stream.Size;
  if FSize < FBufferMaxModifiedPos + FBufferStart then
    FSize := FBufferMaxModifiedPos + FBufferStart;
  Result := FSize;
end;

function TJclBufferedStream.LoadBuffer: Boolean;
begin
  Flush;
  if Length(FBuffer) <> FBufferSize then
    SetLength(FBuffer, FBufferSize);
  FStream.Position := FPosition;
  FBufferCurrentSize := FStream.Read(FBuffer[0], FBufferSize);
  FBufferStart := FPosition;
  Result := (FBufferCurrentSize > 0);
end;

function TJclBufferedStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := Count;
  while Count > 0 do
  begin
    if not BufferHit then
      if not LoadBuffer then
        Break;
    Dec(Count, ReadFromBuffer(Buffer, Count, Result - Count));
  end;
  Result := Result - Count;
end;

function TJclBufferedStream.ReadFromBuffer(var Buffer; Count, Start: Longint): Longint;
var
  BufPos: Longint;
  P: PChar;
begin
  Result := Count;
  BufPos := FPosition - FBufferStart;
  if Result > FBufferCurrentSize - BufPos then
    Result := FBufferCurrentSize - BufPos;
  P := @Buffer;
  Move(FBuffer[BufPos], P[Start], Result);
  Inc(FPosition, Result);
end;

function TJclBufferedStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  NewPos: Longint;
begin
  NewPos := FPosition;
  case Origin of
    soFromBeginning:
      NewPos := Offset;
    soFromCurrent:
      Inc(NewPos, Offset);
    soFromEnd:
      NewPos := GetCalcedSize + Offset;
  else
    NewPos := -1;
  end;
  if NewPos < 0 then
    NewPos := -1
  else
    FPosition := NewPos;
  Result := NewPos;
end;

procedure TJclBufferedStream.SetBufferSize(Value: Longint);
begin
  if FBufferSize <> Value then
    FBufferSize := Value;
end;

procedure TJclBufferedStream.SetSize(NewSize: Longint);
begin
  inherited SetSize(NewSize);
end;

function TJclBufferedStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := Count;
  while Count > 0 do
  begin
    if not BufferHit then
      LoadBuffer;
    Dec(Count, WriteToBuffer(Buffer, Count, Result - Count));
  end;
  Result := Result - Count;
end;

function TJclBufferedStream.WriteToBuffer(const Buffer; Count, Start: Longint): Longint;
var
  BufPos: Longint;
  P: PChar;
begin
  Result := Count;
  BufPos := FPosition - FBufferStart;
  if Result > Length(FBuffer) - BufPos then
    Result := Length(FBuffer) - BufPos;
  if FBufferCurrentSize < BufPos + Result then
    FBufferCurrentSize := BufPos + Result;
  P := @Buffer;
  Move(P[Start], FBuffer[BufPos], Result);
  FBufferMaxModifiedPos := BufPos + Result;
  Inc(FPosition, Result);
end;

//=== { TJclEventStream } ====================================================

constructor TJclEventStream.Create(AStream: TStream; ANotification:
  TStreamNotifyEvent = nil; AOwnsStream: Boolean = False);
begin
  inherited Create(AStream, AOwnsStream);
  FNotification := ANotification;
end;

procedure TJclEventStream.DoAfterStreamChange;
begin
  inherited DoAfterStreamChange;
  if Stream <> nil then
    DoNotification;
end;

procedure TJclEventStream.DoBeforeStreamChange;
begin
  inherited DoBeforeStreamChange;
  if Stream <> nil then
    DoNotification;
end;

procedure TJclEventStream.DoNotification;
begin
  if Assigned(FNotification) then
    FNotification(Self, Stream.Position, Stream.Size);
end;

function TJclEventStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := inherited Read(Buffer, Count);
  DoNotification;
end;

function TJclEventStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := inherited Seek(Offset, Origin);
  DoNotification;
end;

function TJclEventStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := inherited Seek(Offset, Origin);
  DoNotification;
end;

procedure TJclEventStream.SetSize(NewSize: Longint);
begin
  inherited SetSize(NewSize);
  DoNotification;
end;

procedure TJclEventStream.SetSize(const NewSize: Int64);
begin
  inherited SetSize(NewSize);
  DoNotification;
end;

function TJclEventStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := inherited Write(Buffer, Count);
  DoNotification;
end;

//=== { TJclEasyStream } =====================================================

function TJclEasyStream.IsEqual(Stream: TStream): Boolean;
const
  BUFSIZE = 65536;
var
  SavePos, StreamSavePos: Integer;
  ReadCount, StreamReadCount: Integer;
  Buffer, StreamBuffer: PChar;
  TestSize: Integer;
begin
  Result := False;
  SavePos := Position;
  StreamSavePos := Stream.Position;
  if Size <> Stream.Size then
    Exit;
  Buffer := nil;
  try
    GetMem(Buffer, 2*BUFSIZE);
    StreamBuffer := Buffer + BUFSIZE;
    Position := 0;
    Stream.Position := 0;
    TestSize := Size;
    while Position < TestSize do
    begin
      ReadCount := Read(Buffer^, BUFSIZE);
      StreamReadCount := Stream.Read(StreamBuffer^, BUFSIZE);
      if ReadCount <> StreamReadCount then
        Exit;
      if not CompareMem(Buffer, StreamBuffer, ReadCount) then
        Exit;
    end;
  finally
    Position := SavePos;
    Stream.Position := StreamSavePos;
    if Buffer <> nil then
      FreeMem(Buffer);
  end;
  Result := True;
end;

function TJclEasyStream.ReadBoolean: Boolean;
begin
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadChar: Char;
begin
  ReadBuffer(Result, SizeOf(Result));
end;

{$IFNDEF BCB}
function TJclEasyStream.ReadComp: Comp;
begin
  ReadBuffer(Result, SizeOf(Result));
end;
{$ENDIF !BCB}

function TJclEasyStream.ReadCurrency: Currency;
begin
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadDateTime: TDateTime;
begin
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadDouble: Double;
begin
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadExtended: Extended;
begin
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadInt64: Int64;
begin
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadInteger: Integer;
begin
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadCString: string;
var
  CurrPos: Integer;
  StrSize: Integer;
begin
  CurrPos := Position;
  repeat
  until ReadChar = #0;
  StrSize := Position - CurrPos - 1;
  SetString(Result, PChar(nil), StrSize);
  Position := CurrPos;
  ReadBuffer(Pointer(Result)^, StrSize);
  Position := Position + 1;
end;

function TJclEasyStream.ReadShortString: string;
var
  StrSize: Integer;
begin
  StrSize := Ord(ReadChar);
  SetString(Result, PChar(nil), StrSize);
  ReadBuffer(Pointer(Result)^, StrSize);
end;

function TJclEasyStream.ReadSingle: Single;
begin
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadSizedString: string;
var
  StrSize: Integer;
begin
  StrSize := ReadInteger;
  SetString(Result, PChar(nil), StrSize);
  ReadBuffer(Pointer(Result)^, StrSize);
end;

procedure TJclEasyStream.WriteBoolean(Value: Boolean);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteChar(Value: Char);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

{$IFNDEF BCB}
procedure TJclEasyStream.WriteComp(const Value: Comp);
begin
  WriteBuffer(Value, SizeOf(Value));
end;
{$ENDIF !BCB}

procedure TJclEasyStream.WriteCurrency(const Value: Currency);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteDateTime(const Value: TDateTime);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteDouble(const Value: Double);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteExtended(const Value: Extended);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteInt64(Value: Int64);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteInteger(Value: Integer);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteStringDlimitedByNull(const Value: string);
begin
  WriteBuffer(PChar(Value)^, Length(Value) + 1);
end;

procedure TJclEasyStream.WriteShortString(const Value: ShortString);
begin
  WriteBuffer(Value[0], Length(Value) + 1);
end;

procedure TJclEasyStream.WriteSingle(const Value: Single);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteSizedString(const Value: string);
var
  StrSize: Integer;
begin
  StrSize := Length(Value);
  WriteInteger(StrSize);
  WriteBuffer(Pointer(Value)^, StrSize);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
