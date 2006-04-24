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
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclStreams;

{$I jcl.inc}

interface

uses
  SysUtils, Classes;

type
  {$IFDEF COMPILER5}
  TSeekOrigin = (soBeginning, soCurrent, soEnd);
  {$ENDIF COMPILER5}

  EJclStreamException = class(Exception);
  
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

  {  classes that inherit from TJclStream should override these methods:
  TMyJclStream = class(TJclStream)
  protected
    procedure SetSize(const NewSize: Int64); override;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;
  }

  TJclEmptyStream = class(TJclStream)
  protected
    procedure SetSize(const NewSize: Int64); override;
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
    Result64 := 0;
  end;
  if (Result64 < Low(Longint)) or (Result64 > High(Longint)) then
    raise EJclStreamException.CreateRes(@RsStreamsRangeError);
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

//=== { TJclEmptyStream } ====================================================

// a stream which stays empty no matter what you do
// so it is a Unix /dev/null equivalent

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
  if FSize - FPosition < Count then
    Count := FSize - FPosition;
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
  if FPosition > FSize then
    FSize := FPosition;
  Result := Count;
end;

function TJclNullStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning:
      begin
        if Offset >= 0 then
          FPosition := Offset
        else
          FPosition := 0;
        if FPosition > FSize then
          FPosition := FSize;
        Result := FPosition;
      end;
    soCurrent:
      begin
        FPosition := FPosition + Offset;
        if FPosition > FSize then
          FPosition := FSize;
        if FPosition < 0 then
          FPosition := 0;
        Result := FPosition;
      end;
    soEnd:
      begin
        if Offset <= 0 then
          FPosition := FSize + Offset // offset is negative
        else
          FPosition := FSize;
        if FPosition < 0 then
          FPosition := 0;
        Result := FPosition;
      end;
  else
    Result := -1;
  end;
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
  if Count < 0 then
    Count := 0;
  if Size - Position < Count then
    Count := Size - Position;
  BufferPtr := @Buffer;
  for I := 0 to Count - 1 do
  begin
    BufferPtr^ := RandomData;
    Inc(BufferPtr);
  end;
  Position := Position + Count;
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

// History:

// $Log$

end.
