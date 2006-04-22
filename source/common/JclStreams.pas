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
    procedure SetSize(const NewSize: Int64); {$IFDEF COMPILER5} reintroduce; overload; virtual; {$ELSE COMPILER5} overload; override; {$ENDIF COMPILER5}
  public
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; {$IFDEF COMPILER5} reintroduce; overload; virtual; {$ELSE COMPILER5} overload; override; {$ENDIF COMPILER5}
  end;

  // classes that inherit from TJclStream should override these methods:
  //TMyJclStream = class(TJclStream)
  //protected
  //  procedure SetSize(const NewSize: Int64); override;
  //public
  //  function Read(var Buffer; Count: Longint): Longint; override;
  //  function Write(const Buffer; Count: Longint): Longint; override;
  //  function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  //end;

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
  public
    function Read(var Buffer; Count: Longint): Longint; override;
  end;

  TJclMultiplexStream = class(TJclStream)
  private
    FStreams: TList;
    FReadStreamIndex: Integer;
    function GetStream(Index: Integer): TStream;
    function GetStreamCount: Integer;
    procedure SetStream(Index: Integer; const Value: TStream);
    function GetReadStream: TStream;
    procedure SetReadStream(const Value: TStream);
    procedure SetReadStreamIndex(const Value: Integer);
  protected
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    function AddStream(NewStream: TStream): Integer;
    procedure ClearStream;
    function RemoveStream(AStream: TStream): Integer;
    procedure DeleteStream(const Index: Integer);

    property Streams[Index: Integer]: TStream read GetStream write SetStream;
    property ReadStreamIndex: Integer read FReadStreamIndex write SetReadStreamIndex;
    property ReadStream: TStream read GetReadStream write SetReadStream;
    property StreamCount: Integer read GetStreamCount;
  end;

implementation

uses
  JclResources, JclBase;

//=== { TJclStream } =========================================================

function TJclStream.Seek(Offset: Integer; Origin: Word): Longint;
var
  Result64: Int64;
begin
  case Origin of
    soFromBeginning :
      Result64 := Seek(Int64(Offset), soBeginning);
    soFromCurrent :
      Result64 := Seek(Int64(Offset), soCurrent);
    soFromEnd :
      Result64 := Seek(Int64(Offset), soEnd);
  else
    Result64 := 0;
  end;
  if (Result64 < Low(LongInt)) or (Result64 > High(LongInt)) then
    raise EJclStreamException.CreateRes(@RsStreamsRangeError);
  Result := Result64;
end;

function TJclStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  // override to customize
  Result := -1;
end;

procedure TJclStream.SetSize(NewSize: Integer);
begin
  SetSize(Int64(NewSize));
end;

procedure TJclStream.SetSize(const NewSize: Int64);
begin
  // override to customize
end;

//=== { TJclEmptyStream } ====================================================

procedure TJclEmptyStream.SetSize(const NewSize: Int64);
begin
  // nothing
end;

function TJclEmptyStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;

function TJclEmptyStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;

function TJclEmptyStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := 0;
end;

//=== { TJclNullStream } =====================================================

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

function TJclRandomStream.Read(var Buffer; Count: Integer): Longint;
{$IFDEF COMPILER5}
type
  PWord = ^Word;
{$ENDIF COMPILER5}
var
  BufferPtr: PByte;
begin
  if Count < 0 then
    Count := 0;
  if FSize - FPosition < Count then
    Count := FSize - FPosition;
  if Count > 0 then
  begin
    BufferPtr := @Buffer;
    while Count > 1 do
    begin
      PWord(BufferPtr)^ := Random($10000);
      Inc(BufferPtr, 2);
    end;
    if Count <> 0 then
      BufferPtr^ := Random($100);
    FPosition := FPosition + Count;
  end;
  Result := Count;
end;

//=== { TJclMultiplexStream } ================================================

function TJclMultiplexStream.AddStream(NewStream: TStream): Integer;
begin
  Result := FStreams.Add(Pointer(NewStream));
end;

procedure TJclMultiplexStream.ClearStream;
begin
  FStreams.Clear;
  FReadStreamIndex := -1;
end;

constructor TJclMultiplexStream.Create;
begin
  inherited Create;
  FStreams := TList.Create;
  FReadStreamIndex := -1;
end;

procedure TJclMultiplexStream.DeleteStream(const Index: Integer);
begin
  FStreams.Delete(Index);
  if ReadStreamIndex = Index then
    FReadStreamIndex := -1
  else if ReadStreamIndex > Index then
    Dec(FReadStreamIndex);
end;

destructor TJclMultiplexStream.Destroy;
begin
  FStreams.Free;
  inherited Destroy;
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

function TJclMultiplexStream.GetStreamCount: Integer;
begin
  Result := FStreams.Count;
end;

function TJclMultiplexStream.Read(var Buffer; Count: Integer): Longint;
var
  AReadStream: TStream;
begin
  AReadStream := ReadStream;
  if Assigned(AReadStream) then
    Result := AReadStream.Read(Buffer, Count)
  else
    Result := 0;
end;

function TJclMultiplexStream.RemoveStream(AStream: TStream): Integer;
begin
  Result := FStreams.Remove(Pointer(AStream));
  if FReadStreamIndex = Result then
    FReadStreamIndex := -1
  else if FReadStreamIndex > Result then
    Dec(FReadStreamIndex);
end;

function TJclMultiplexStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
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

function TJclMultiplexStream.Write(const Buffer; Count: Integer): Longint;
var
  Index: Integer;
  ByteWritten, MinByteWritten: Longint;
begin
  MinByteWritten := Count;
  for Index := 0 to StreamCount - 1 do
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
