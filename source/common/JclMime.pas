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
{ The Original Code is JclMime.pas.                                            }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ 2000 of these individuals.                                                   }
{                                                                              }
{ Last modified: June 29, 2000                                                 }
{                                                                              }
{******************************************************************************}

//------------------------------------------------------------------------------
// Author:         Ralf Junker <ralfjunker@gmx.de>
//
// Description:    Ligtening fast Mime (Base64) Encoding and Decoding routines.
//                 More detailed descriptions follow the declarations of the
//                 functions and procedures below.
//------------------------------------------------------------------------------

// he requires to be mentioned in the help with name and email address

unit JclMime;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

uses
  Classes, SysUtils,
  JclBase;

// MimeEncodeString takes a string, encodes it, and returns the result as a string.
// To decode the result string, use MimeDecodeString.

function MimeEncodeString(const s: AnsiString): AnsiString;

// MimeDecodeString takes a a string, decodes it, and returns the result as a string.
// Use MimeDecodeString to decode a string previously encoded with MimeEncodeString.

function MimeDecodeString(const s: AnsiString): AnsiString;

// MimeEncodeStream encodes InputStream starting at the current position
// up to the end and writes the result to OutputStream, again starting at
// the current position. When done, it will not reset either stream's positions,
// but leave InputStream at the last read position (i.e. the end) and
// OutputStream at the last write position (which can, but most not be the end).
// To encode the entire InputStream from beginning to end, make sure
// that its offset is positioned at the beginning of the stream. You can
// force this by issuing Seek (0, soFromBeginning) before calling this function.

procedure MimeEncodeStream(const InputStream: TStream; const OutputStream: TStream);

// MimeDecodeStream decodes InputStream starting at the current position
// up to the end and writes the result to OutputStream, again starting at
// the current position. When done, it will not reset either stream's positions,
// but leave InputStream at the last read position (i.e. the end) and
// OutputStream at the last write position (which can, but most not be the end).
// To decode the entire InputStream from beginning to end, make sure
// that its offset is positioned at the beginning of the stream. You can
// force this by issuing Seek (0, soFromBeginning) before calling this function.

procedure MimeDecodeStream(const InputStream: TStream; const OutputStream: TStream);

// Calculates the output size of i MimeEncoded bytes. Use for MimeEncode only.

function MimeEncodedSize(const i: Integer): Integer;

// Calculates the maximum output size of i MimeDecoded bytes.
// You may use it for MimeDecode to calculate the maximum amount of memory
// required for decoding in one single pass.

function MimeDecodedSize(const i: Integer): Integer;

// The primary Mime encoding routine.
//
// CAUTION: OutputBuffer must have enough memory allocated to take all encoded output.
// MimeEncodedSize (InputBytesCount) calculates this amount in bytes. MimeEncode will
// then fill the entire OutputBuffer, so there is no OutputBytesCount result for
// this procedure. Preallocating all memory at once (as required by MimeEncode)
// avoids the time-cosuming process of reallocation.
//
// If not all data fits into memory at once, you can use MimeEncode multiple times,
// but you must be very careful about the size of the InputBuffer.
// See comments on BUFFER_SIZE below for details.

procedure MimeEncode(const InputBuffer: Pointer; const InputByteCount: Integer; const OutputBuffer: Pointer);

// The primary Mime decoding routines.
//
// CAUTION: OutputBuffer must have enough memory allocated to take all output.
// MimeDecodedSize (InputBytesCount) calculates this amount in bytes. There is
// no guarantee that all output will be filled after decoding. All decoding
// functions therefore return the acutal number of bytes written to OutputBuffer.
// Preallocating all memory at once (as is required by MimeDecode)
// avoids the time-cosuming process of reallocation. After calling
// MimeDecode, simply cut the allocated memory down to OutputBytesCount,
// i.e. SetLength(OutString, OutputBytesCount).

function MimeDecode(const InputBuffer: Pointer; const InputBytesCount: Integer; const OutputBuffer: Pointer): Integer;

// The MimeDecodePartial_ functions are mostly for internal use.
// They serve the purpose of decoding very large data in multiple parts of
// smaller chunks, as used in MimeDecodeStream.

function MimeDecodePartial(const InputBuffer: Pointer; const InputBytesCount: Integer; const OutputBuffer: Pointer; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Integer;
function MimeDecodePartialEnd(const OutputBuffer: Pointer; const ByteBuffer: Cardinal; const ByteBufferSpace: Cardinal): Integer;

type
  EJclMimeError = class (EJclError);

implementation

uses
  JclResources;

// Caution: For MimeEncodeStream and all other kinds of multi-buffered
// Mime encodings (i.e. Files etc.), BufferSize must be set to a multiple of 3.
// Even though the implementation of the Mime decoding routines below
// do not require a particular buffer size, they work fastest with sizes of
// multiples of four. The chosen size is a multiple of 3 and of 4 as well.
// The following numbers are, in addition, also divisible by 1024:
// $2400, $3000, $3C00, $4800, $5400, $6000, $6C00.

const
  BUFFER_SIZE = $3000;
  EqualSign   = Byte('=');

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
    b1: Byte;
    b2: Byte;
    b3: Byte;
    b4: Byte;
  end;

  PByte3 = ^TByte3;
  TByte3 = packed record
    b1: Byte;
    b2: Byte;
    b3: Byte;
  end;

//------------------------------------------------------------------------------
// Wrapper functions & procedures
//------------------------------------------------------------------------------

function MimeEncodeString(const s: AnsiString): AnsiString;
var
  l: Integer;
begin
  l := Length(s);
  if l > 0 then
  begin
    SetLength(Result, MimeEncodedSize(l));
    MimeEncode(Pointer(s), l, Pointer(Result));
  end
  else
    Result := '';
end;

//------------------------------------------------------------------------------

function MimeDecodeString(const s: AnsiString): AnsiString;
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  l: Integer;
begin
  l := Length(s);
  if l > 0 then
  begin
    SetLength(Result, MimeDecodedSize(l));

    ByteBuffer := 0;
    ByteBufferSpace := 4;

    l := MimeDecodePartial(Pointer(s), l, Pointer(Result), ByteBuffer, ByteBufferSpace);
    MimeDecodePartialEnd(Pointer(Integer(Result) + l), ByteBuffer, ByteBufferSpace);
  end;
end;

//------------------------------------------------------------------------------

procedure MimeEncodeStream(const InputStream: TStream; const OutputStream: TStream);
var
  InputBuffer: array [0..BUFFER_SIZE - 1] of Byte;
  OutputBuffer: array [0..((BUFFER_SIZE + 2) div 3) * 4 - 1] of Byte;
  BytesRead: Integer;
begin
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  while BytesRead = SizeOf(InputBuffer) do
  begin
    MimeEncode(@InputBuffer, SizeOf(InputBuffer), @OutputBuffer);
    OutputStream.Write(OutputBuffer, SizeOf(OutputBuffer));
    BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  end;
  if BytesRead > 0 then
  begin
    MimeEncode(@InputBuffer, BytesRead, @OutputBuffer);
    OutputStream.Write(OutputBuffer, MimeEncodedSize(BytesRead));
  end;
end;

//------------------------------------------------------------------------------

procedure MimeDecodeStream(const InputStream: TStream; const OutputStream: TStream);
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  InputBuffer: array [0..BUFFER_SIZE - 1] of Byte;
  OutputBuffer: array [0..(BUFFER_SIZE + 3) div 4 * 3 - 1] of Byte;
  BytesRead: Integer;
begin
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  while BytesRead > 0 do
  begin
    OutputStream.Write(OutputBuffer, MimeDecodePartial(@InputBuffer, BytesRead, @OutputBuffer, ByteBuffer, ByteBufferSpace));
    BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  end;
  OutputStream.Write(OutputBuffer, MimeDecodePartialEnd(@OutputBuffer, ByteBuffer, ByteBufferSpace));
end;

//------------------------------------------------------------------------------
// Helper functions
//------------------------------------------------------------------------------

function MimeEncodedSize(const i: Integer): Integer;
begin
  Result := (i + 2) div 3 * 4;
end;

function MimeDecodedSize(const i: Integer): Integer;
begin
  Result := (i + 3) div 4 * 3;
end;

//------------------------------------------------------------------------------
// Primary functions & procedures
//------------------------------------------------------------------------------

procedure MimeEncode(const InputBuffer: Pointer; const InputByteCount: Integer; const OutputBuffer: Pointer);
var
  b: Cardinal;
  InMax3: Integer;
  pIn, PInLimit: ^Byte;
  POut: PByte4;
begin
  if InputBuffer = nil then
    raise EJclMimeError.CreateResRec(@RsInputBufferNil);
  if OutputBuffer = nil then
    raise EJclMimeError.CreateResRec(@RsOutputBufferNil);
  if InputByteCount <= 0 then
    Exit;

  pIn := InputBuffer;
  InMax3 := InputByteCount div 3 * 3;
  POut := OutputBuffer;

  Integer(PInLimit) := Integer(pIn) + InMax3;

  while pIn <> PInLimit do
  begin
    b := pIn^;
    b := b shl 8;
    Inc(pIn);
    b := b or pIn^;
    b := b shl 8;
    Inc(pIn);
    b := b or pIn^;
    Inc(pIn);

    // Write 4 bytes to OutputBuffer (in reverse order).
    POut.b4 := MIME_ENCODE_TABLE[b and $3F];
    b := b shr 6;
    POut.b3 := MIME_ENCODE_TABLE[b and $3F];
    b := b shr 6;
    POut.b2 := MIME_ENCODE_TABLE[b and $3F];
    b := b shr 6;
    POut.b1 := MIME_ENCODE_TABLE[b];

    Inc(POut);
  end;

  case InputByteCount - InMax3 of
    1:
      begin
        b := pIn^;

        b := b shl 4;

        POut.b2 := MIME_ENCODE_TABLE[b and $3F];
        b := b shr 6;
        POut.b1 := MIME_ENCODE_TABLE[b];

        POut.b3 := EqualSign; // Fill remaining 2 bytes.
        POut.b4 := EqualSign;
      end;
    2:
      begin
        b := pIn^;
        Inc(pIn);
        b := b shl 8;
        b := b or pIn^;

        b := b shl 2;

        POut.b3 := MIME_ENCODE_TABLE[b and $3F];
        b := b shr 6;
        POut.b2 := MIME_ENCODE_TABLE[b and $3F];
        b := b shr 6;
        POut.b1 := MIME_ENCODE_TABLE[b];

        POut.b4 := EqualSign; // Fill remaining byte.
      end;
  end;
end;

//------------------------------------------------------------------------------

function MimeDecode(const InputBuffer: Pointer; const InputBytesCount: Integer; const OutputBuffer: Pointer): Integer;
var
  ByteBuffer, ByteBufferSpace: Cardinal;
begin
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  Result := MimeDecodePartial(InputBuffer, InputBytesCount, OutputBuffer, ByteBuffer, ByteBufferSpace);
  Inc(Result, MimeDecodePartialEnd(Pointer(Integer(OutputBuffer) + Result), ByteBuffer, ByteBufferSpace));
end;

//------------------------------------------------------------------------------

function MimeDecodePartial(const InputBuffer: Pointer; const InputBytesCount: Integer; const OutputBuffer: Pointer; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Integer;
var
  lByteBuffer, lByteBufferSpace, c: Cardinal;
  pIn, PInLimit: ^Byte;
  POut: PByte3;
begin
  if InputBuffer = nil then
    raise EJclMimeError.CreateResRec(@RsInputBufferNil);
  if OutputBuffer = nil then
    raise EJclMimeError.CreateResRec(@RsOutputBufferNil);

  if InputBytesCount > 0 then
  begin
    pIn := InputBuffer;
    Integer(PInLimit) := Integer(pIn) + InputBytesCount;

    POut := OutputBuffer;

    lByteBuffer := ByteBuffer;
    lByteBufferSpace := ByteBufferSpace;

    while pIn <> PInLimit do
    begin
      c := MIME_DECODE_TABLE[pIn^]; // Read from InputBuffer.
      Inc(pIn);

      if c = $FF then
        Continue;

      lByteBuffer := lByteBuffer shl 6;
      lByteBuffer := lByteBuffer or c;
      Dec(lByteBufferSpace);

      if lByteBufferSpace <> 0 then
        Continue; // Read 4 bytes from InputBuffer?

      POut.b3 := Byte(lByteBuffer); // Write 3 bytes to OutputBuffer (in reverse order).
      lByteBuffer := lByteBuffer shr 8;
      POut.b2 := Byte(lByteBuffer);
      lByteBuffer := lByteBuffer shr 8;
      POut.b1 := Byte(lByteBuffer);
      lByteBuffer := 0;
      Inc(POut);

      lByteBufferSpace := 4;
    end;

    ByteBuffer := lByteBuffer;
    ByteBufferSpace := lByteBufferSpace;

    Result := Cardinal(POut) - Cardinal(OutputBuffer);
  end
  else
    Result := 0;
end;

//------------------------------------------------------------------------------

function MimeDecodePartialEnd(const OutputBuffer: Pointer; const ByteBuffer: Cardinal; const ByteBufferSpace: Cardinal): Integer;
var
  lByteBuffer: Cardinal;
begin
  if OutputBuffer = nil then
    raise EJclMimeError.CreateResRec(@RsOutputBufferNil);

  case ByteBufferSpace of
    1:
      begin
        lByteBuffer := ByteBuffer shr 2;
        PByte3(OutputBuffer).b2 := Byte(lByteBuffer);
        lByteBuffer := lByteBuffer shr 8;
        PByte3(OutputBuffer).b1 := Byte(lByteBuffer);
        Result := 2;
      end;
    2:
      begin
        lByteBuffer := ByteBuffer shr 4;
        PByte3(OutputBuffer).b1 := Byte(lByteBuffer);
        Result := 1;
      end;
  else
    Result := 0;
  end;
end;

end.

