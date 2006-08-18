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
{ The Original Code is JclMime.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Lightening fast Mime (Base64) Encoding and Decoding routines. Coded by Ralf Junker               }
{ (ralfjunker@gmx.de).                                                                             }
{                                                                                                  }
{ Unit owner: Marcel van Brakel                                                                    }
{ Last modified: January 29, 2001                                                                  }
{                                                                                                  }
{**************************************************************************************************}

unit JclMime;

{$I jcl.inc}

{$WEAKPACKAGEUNIT ON}

interface

uses
  Classes, SysUtils;

function MimeEncodeString(const S: AnsiString): AnsiString;
function MimeDecodeString(const S: AnsiString): AnsiString;
procedure MimeEncodeStream(const InputStream: TStream; const OutputStream: TStream);
procedure MimeDecodeStream(const InputStream: TStream; const OutputStream: TStream);
function MimeEncodedSize(const I: Cardinal): Cardinal;
function MimeDecodedSize(const I: Cardinal): Cardinal;
procedure MimeEncode(var InputBuffer; const InputByteCount: Cardinal; var OutputBuffer);
function MimeDecode(var InputBuffer; const InputBytesCount: Cardinal; var OutputBuffer): Cardinal;
function MimeDecodePartial(var InputBuffer; const InputBytesCount: Cardinal;
  var OutputBuffer; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Cardinal;
function MimeDecodePartialEnd(var OutputBuffer; const ByteBuffer: Cardinal;
  const ByteBufferSpace: Cardinal): Cardinal;

implementation

// Caution: For MimeEncodeStream and all other kinds of multi-buffered
// Mime encodings (i.e. Files etc.), BufferSize must be set to a multiple of 3.
// Even though the implementation of the Mime decoding routines below
// do not require a particular buffer size, they work fastest with sizes of
// multiples of four. The chosen size is a multiple of 3 and of 4 as well.
// The following numbers are, in addition, also divisible by 1024:
// $2400, $3000, $3C00, $4800, $5400, $6000, $6C00.

const
  BUFFER_SIZE = $3000;
  EqualSign = Byte('=');

  MIME_ENCODE_TABLE: array [0..63] of Byte = (
     65,  66,  67,  68,  69,  70,  71,  72,  // 00 - 07
     73,  74,  75,  76,  77,  78,  79,  80,  // 08 - 15
     81,  82,  83,  84,  85,  86,  87,  88,  // 16 - 23
     89,  90,  97,  98,  99, 100, 101, 102,  // 24 - 31
    103, 104, 105, 106, 107, 108, 109, 110,  // 32 - 39
    111, 112, 113, 114, 115, 116, 117, 118,  // 40 - 47
    119, 120, 121, 122,  48,  49,  50,  51,  // 48 - 55
     52,  53,  54,  55,  56,  57,  43,  47); // 56 - 63

  MIME_DECODE_TABLE: array [Byte] of Cardinal = (
    255, 255, 255, 255, 255, 255, 255, 255, //  00 -  07
    255, 255, 255, 255, 255, 255, 255, 255, //  08 -  15
    255, 255, 255, 255, 255, 255, 255, 255, //  16 -  23
    255, 255, 255, 255, 255, 255, 255, 255, //  24 -  31
    255, 255, 255, 255, 255, 255, 255, 255, //  32 -  39
    255, 255, 255,  62, 255, 255, 255,  63, //  40 -  47
     52,  53,  54,  55,  56,  57,  58,  59, //  48 -  55
     60,  61, 255, 255, 255, 255, 255, 255, //  56 -  63
    255,   0,   1,   2,   3,   4,   5,   6, //  64 -  71
      7,   8,   9,  10,  11,  12,  13,  14, //  72 -  79
     15,  16,  17,  18,  19,  20,  21,  22, //  80 -  87
     23,  24,  25, 255, 255, 255, 255, 255, //  88 -  95
    255,  26,  27,  28,  29,  30,  31,  32, //  96 - 103
     33,  34,  35,  36,  37,  38,  39,  40, // 104 - 111
     41,  42,  43,  44,  45,  46,  47,  48, // 112 - 119
     49,  50,  51, 255, 255, 255, 255, 255, // 120 - 127
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255);

type
  PByte4 = ^TByte4;
  TByte4 = packed record
    B1: Byte;
    B2: Byte;
    B3: Byte;
    B4: Byte;
  end;

  PByte3 = ^TByte3;
  TByte3 = packed record
    B1: Byte;
    B2: Byte;
    B3: Byte;
  end;

//--------------------------------------------------------------------------------------------------
// Wrapper functions & procedures
//--------------------------------------------------------------------------------------------------

function MimeEncodeString(const S: AnsiString): AnsiString;
var
  L: Cardinal;
begin
  L := Length(S);
  if L > 0 then
  begin
    SetLength(Result, MimeEncodedSize(L));
    MimeEncode(PChar(S)^, L, PChar(Result)^);
  end
  else
    Result := '';
end;

//--------------------------------------------------------------------------------------------------

function MimeDecodeString(const S: AnsiString): AnsiString;
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  L: Cardinal;
begin
  L := Length(S);
  if L > 0 then
  begin
    SetLength(Result, MimeDecodedSize(L));
    ByteBuffer := 0;
    ByteBufferSpace := 4;
    L := MimeDecodePartial(PChar(S)^, L, PChar(Result)^, ByteBuffer, ByteBufferSpace);
    Inc(L, MimeDecodePartialEnd(PChar(Cardinal(Result) + L)^, ByteBuffer, ByteBufferSpace));
    SetLength(Result, L);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure MimeEncodeStream(const InputStream: TStream; const OutputStream: TStream);
var
  InputBuffer: array [0..BUFFER_SIZE - 1] of Byte;
  OutputBuffer: array [0..((BUFFER_SIZE + 2) div 3) * 4 - 1] of Byte;
  BytesRead: Integer;
begin
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  while BytesRead > 0 do
  begin
    MimeEncode(InputBuffer, BytesRead, OutputBuffer);
    OutputStream.Write(OutputBuffer, MimeEncodedSize(BytesRead));
    BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure MimeDecodeStream(const InputStream: TStream; const OutputStream: TStream);
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  InputBuffer: array [0..(BUFFER_SIZE + 3) div 4 * 3 - 1] of Byte;
  OutputBuffer: array [0..BUFFER_SIZE - 1] of Byte;
  BytesRead: Integer;
begin
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  while BytesRead > 0 do
  begin
    OutputStream.Write(OutputBuffer, MimeDecodePartial(InputBuffer, BytesRead, OutputBuffer, ByteBuffer, ByteBufferSpace));
    BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  end;
  OutputStream.Write(OutputBuffer, MimeDecodePartialEnd(OutputBuffer, ByteBuffer, ByteBufferSpace));
end;

//--------------------------------------------------------------------------------------------------
// Helper functions
//--------------------------------------------------------------------------------------------------

function MimeEncodedSize(const I: Cardinal): Cardinal;
begin
  Result := (I + 2) div 3 * 4;
end;

//--------------------------------------------------------------------------------------------------

function MimeDecodedSize(const I: Cardinal): Cardinal;
begin
  Result := (I + 3) div 4 * 3;
end;

//--------------------------------------------------------------------------------------------------
// Primary functions & procedures
//--------------------------------------------------------------------------------------------------

procedure MimeEncode(var InputBuffer; const InputByteCount: Cardinal; var OutputBuffer);
var
  B: Cardinal;
  InMax3: Cardinal;
  InPtr, InLimitPtr: ^Byte;
  OutPtr: PByte4;
begin
  if InputByteCount <= 0 then
    Exit;

  InPtr := @InputBuffer;
  InMax3 := InputByteCount div 3 * 3;
  OutPTr := @OutputBuffer;
  Cardinal(InLimitPtr) := Cardinal(InPtr) + InMax3;

  while InPtr <> InLimitPtr do
  begin
    B := InPtr^;
    B := B shl 8;
    Inc(InPtr);
    B := B or InPtr^;
    B := B shl 8;
    Inc(InPtr);
    B := B or InPtr^;
    Inc(InPtr);
    // Write 4 bytes to OutputBuffer (in reverse order).
    OutPtr.B4 := MIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr.B3 := MIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr.B2 := MIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr.B1 := MIME_ENCODE_TABLE[B];
    Inc(OutPtr);
  end;

  case InputByteCount - InMax3 of
    1:
      begin
        B := InPtr^;
        B := B shl 4;
        OutPtr.B2 := MIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.B1 := MIME_ENCODE_TABLE[B];
        OutPtr.B3 := EqualSign; // Fill remaining 2 bytes.
        OutPtr.B4 := EqualSign;
      end;
    2:
      begin
        B := InPtr^;
        Inc(InPtr);
        B := B shl 8;
        B := B or InPtr^;
        B := B shl 2;
        OutPtr.B3 := MIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPTr.b2 := MIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.B1 := MIME_ENCODE_TABLE[B];
        OutPtr.B4 := EqualSign; // Fill remaining byte.
      end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function MimeDecode(var InputBuffer; const InputBytesCount: Cardinal; var OutputBuffer): Cardinal;
var
  ByteBuffer, ByteBufferSpace: Cardinal;
begin
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  Result := MimeDecodePartial(InputBuffer, InputBytesCount, OutputBuffer, ByteBuffer, ByteBufferSpace);
  Inc(Result, MimeDecodePartialEnd(PChar(Cardinal(OutputBuffer) + Result)^, ByteBuffer, ByteBufferSpace));
end;

//--------------------------------------------------------------------------------------------------

function MimeDecodePartial(var InputBuffer; const InputBytesCount: Cardinal;
  var OutputBuffer; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Cardinal;
var
  lByteBuffer, lByteBufferSpace, C: Cardinal;
  InPtr, InLimitPtr: ^Byte;
  OutPtr: PByte3;
begin
  if InputBytesCount > 0 then
  begin
    InPtr := @InputBuffer;
    Cardinal(InLimitPtr) := Cardinal(InPtr) + InputBytesCount;
    OutPtr := @OutputBuffer;
    lByteBuffer := ByteBuffer;
    lByteBufferSpace := ByteBufferSpace;
    while InPtr <> InLimitPtr do
    begin
      C := MIME_DECODE_TABLE[InPtr^]; // Read from InputBuffer.
      Inc(InPtr);
      if C = $FF then
        Continue;

      lByteBuffer := lByteBuffer shl 6;
      lByteBuffer := lByteBuffer or C;
      Dec(lByteBufferSpace);
      if lByteBufferSpace <> 0 then
        Continue; // Read 4 bytes from InputBuffer?

      OutPtr.B3 := Byte(lByteBuffer); // Write 3 bytes to OutputBuffer (in reverse order).
      lByteBuffer := lByteBuffer shr 8;
      OutPtr.B2 := Byte(lByteBuffer);
      lByteBuffer := lByteBuffer shr 8;
      OutPtr.B1 := Byte(lByteBuffer);
      lByteBuffer := 0;
      Inc(OutPtr);
      lByteBufferSpace := 4;
    end;
    ByteBuffer := lByteBuffer;
    ByteBufferSpace := lByteBufferSpace;
    Result := Cardinal(OutPtr) - Cardinal(@OutputBuffer);
  end
  else
    Result := 0;
end;

//--------------------------------------------------------------------------------------------------

function MimeDecodePartialEnd(var OutputBuffer; const ByteBuffer: Cardinal;
  const ByteBufferSpace: Cardinal): Cardinal;
var
  lByteBuffer: Cardinal;
begin
  case ByteBufferSpace of
    1:
      begin
        lByteBuffer := ByteBuffer shr 2;
        PByte3(@OutputBuffer).B2 := Byte(lByteBuffer);
        lByteBuffer := lByteBuffer shr 8;
        PByte3(@OutputBuffer).B1 := Byte(lByteBuffer);
        Result := 2;
      end;
    2:
      begin
        lByteBuffer := ByteBuffer shr 4;
        PByte3(@OutputBuffer).B1 := Byte(lByteBuffer);
        Result := 1;
      end;
  else
    Result := 0;
  end;
end;

end.
