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
{ The Initial Developer of the Original Code is Ralf Junker.                                       }
{ Portions created by Ralf Junker are Copyright (C) Ralf Junker. All rights reserved.              }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel van Brakel, Ralf Junker, Robert Marquardt, Robert Rossmair, Matthias Thoma, Petr Vones  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Lightning fast Mime (Base64) Encoding and Decoding routines. Coded by Ralf Junker                }
{ (ralfjunker att gmx dott de).                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{ Migration Guide from JCL 1.90 and older:                                                         }
{                                                                                                  }
{ These new functions now support line breaks (CRLF) as required by RFC 2045.                      }
{ Inserting line breaks is the default behaviour in RFC 2045 therefor the encoding functions now   }
{ encode with line breaks.                                                                         }
{                                                                                                  }
{ This may require changes to your code:                                                           }
{ Encoding without inserting line breaks is possible using the corresponding NoCRLF procedures:    }
{                                                                                                  }
{ MimeEncode => MimeEncodeNoCRLF                                                                   }
{ MimeEncodeString => MimeEncodeStringNoCRLF                                                       }
{ MimeEncodeStream => MimeEncodeStreamNoCRLF                                                       }
{ MimeEncodedSize => MimeEncodedSizeNoCRLF                                                         }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclMime;

{$I jcl.inc}

interface

uses
  Classes;

function MimeEncodeString(const S: AnsiString): AnsiString;
function MimeEncodeStringNoCRLF(const S: AnsiString): AnsiString;
function MimeDecodeString(const S: AnsiString): AnsiString;
function MimeEncodedSize(const InputSize: Cardinal): Cardinal;
function MimeEncodedSizeNoCRLF(const InputSize: Cardinal): Cardinal;
function MimeDecodedSize(const InputSize: Cardinal): Cardinal;
procedure DecodeHttpBasicAuthentication(const BasicCredentials: AnsiString;
  out UserId, PassWord: AnsiString);
procedure MimeEncode(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
procedure MimeEncodeNoCRLF(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
procedure MimeEncodeFullLines(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
function MimeDecode(const InputBuffer; const InputBytesCount: Cardinal; out OutputBuffer): Cardinal;
function MimeDecodePartial(const InputBuffer; const InputBytesCount: Cardinal;
  out OutputBuffer; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Cardinal;
function MimeDecodePartialEnd(out OutputBuffer; const ByteBuffer: Cardinal;
  const ByteBufferSpace: Cardinal): Cardinal;
procedure MimeEncodeFile(const InputFileName, OutputFileName: AnsiString);
procedure MimeEncodeFileNoCRLF(const InputFileName, OutputFileName: AnsiString);
procedure MimeDecodeFile(const InputFileName, OutputFileName: AnsiString);
procedure MimeEncodeStream(const InputStream: TStream; const OutputStream: TStream);
procedure MimeEncodeStreamNoCRLF(const InputStream: TStream; const OutputStream: TStream);
procedure MimeDecodeStream(const InputStream: TStream; const OutputStream: TStream);


const
  MIME_ENCODED_LINE_BREAK = 76;
  MIME_DECODED_LINE_BREAK = MIME_ENCODED_LINE_BREAK div 4 * 3;
  MIME_BUFFER_SIZE = MIME_DECODED_LINE_BREAK * 3 * 4 * 4;

implementation
uses
  SysUtils;

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

const
  { The mime encoding table. Do not alter. }
  MIME_ENCODE_TABLE: array[0..63] of Byte = (
    065, 066, 067, 068, 069, 070, 071, 072, //  00 - 07
    073, 074, 075, 076, 077, 078, 079, 080, //  08 - 15
    081, 082, 083, 084, 085, 086, 087, 088, //  16 - 23
    089, 090, 097, 098, 099, 100, 101, 102, //  24 - 31
    103, 104, 105, 106, 107, 108, 109, 110, //  32 - 39
    111, 112, 113, 114, 115, 116, 117, 118, //  40 - 47
    119, 120, 121, 122, 048, 049, 050, 051, //  48 - 55
    052, 053, 054, 055, 056, 057, 043, 047); // 56 - 63

  MIME_PAD_CHAR = Byte('=');

  MIME_DECODE_TABLE: array[Byte] of Cardinal = (
    255, 255, 255, 255, 255, 255, 255, 255, //   0 -   7
    255, 255, 255, 255, 255, 255, 255, 255, //   8 -  15
    255, 255, 255, 255, 255, 255, 255, 255, //  16 -  23
    255, 255, 255, 255, 255, 255, 255, 255, //  24 -  31
    255, 255, 255, 255, 255, 255, 255, 255, //  32 -  39
    255, 255, 255, 062, 255, 255, 255, 063, //  40 -  47
    052, 053, 054, 055, 056, 057, 058, 059, //  48 -  55
    060, 061, 255, 255, 255, 255, 255, 255, //  56 -  63
    255, 000, 001, 002, 003, 004, 005, 006, //  64 -  71
    007, 008, 009, 010, 011, 012, 013, 014, //  72 -  79
    015, 016, 017, 018, 019, 020, 021, 022, //  80 -  87
    023, 024, 025, 255, 255, 255, 255, 255, //  88 -  95
    255, 026, 027, 028, 029, 030, 031, 032, //  96 - 103
    033, 034, 035, 036, 037, 038, 039, 040, // 104 - 111
    041, 042, 043, 044, 045, 046, 047, 048, // 112 - 119
    049, 050, 051, 255, 255, 255, 255, 255, // 120 - 127
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

  PCardinal = ^Cardinal;

//--------------------------------------------------------------------------------------------------
// Wrapper functions & procedures
//--------------------------------------------------------------------------------------------------

function MimeEncodeString(const S: AnsiString): AnsiString;
var
  L: Cardinal;
begin
  if Pointer(S) <> nil then
    begin
      L := PCardinal(Cardinal(S) - 4)^;
      SetLength(Result, MimeEncodedSize(L));
      MimeEncode(Pointer(S)^, L, Pointer(Result)^);
    end
  else
    Result := '';
end;

//--------------------------------------------------------------------------------------------------

function MimeEncodeStringNoCRLF(const S: AnsiString): AnsiString;
var
  L: Cardinal;
begin
  if Pointer(S) <> nil then
    begin
      L := PCardinal(Cardinal(S) - 4)^;
      SetLength(Result, MimeEncodedSizeNoCRLF(L));
      MimeEncodeNoCRLF(Pointer(S)^, L, Pointer(Result)^);
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
  if Pointer(S) <> nil then
    begin
      L := PCardinal(Cardinal(S) - 4)^;
      SetLength(Result, MimeDecodedSize(L));
      ByteBuffer := 0;
      ByteBufferSpace := 4;
      L := MimeDecodePartial(Pointer(S)^, L, Pointer(Result)^, ByteBuffer, ByteBufferSpace);
      Inc(L, MimeDecodePartialEnd(Pointer(Cardinal(Result) + L)^, ByteBuffer, ByteBufferSpace));
      SetLength(Result, L);
    end
  else
    Result := '';
end;

//--------------------------------------------------------------------------------------------------

procedure DecodeHttpBasicAuthentication(const BasicCredentials: AnsiString; out UserId, PassWord: AnsiString);
label
  Fail;
const
  LBasic = 6; { Length ('Basic ') }
var
  DecodedPtr, P: PAnsiChar;
  I, L: Cardinal;
begin
  P := Pointer(BasicCredentials);
  if P = nil then
    goto Fail;

  L := Cardinal(Pointer(P - 4)^);
  if L <= LBasic then
    goto Fail;

  Dec(L, LBasic);
  Inc(P, LBasic);

  GetMem(DecodedPtr, MimeDecodedSize(L));
  L := MimeDecode(P^, L, DecodedPtr^);

  { Look for colon (':'). }
  I := 0;
  P := DecodedPtr;
  while (L > 0) and (P[I] <> ':') do
  begin
    Inc(I);
    Dec(L);
  end;

  { Store UserId and Password. }
  SetString(UserId, DecodedPtr, I);
  if L > 1 then
    SetString(PassWord, DecodedPtr + I + 1, L - 1)
  else
    PassWord := '';

  FreeMem(DecodedPtr);
  Exit;

  Fail:
  UserId := '';
  PassWord := '';
end;

//--------------------------------------------------------------------------------------------------
// Helper functions
//--------------------------------------------------------------------------------------------------

function MimeEncodedSize(const InputSize: Cardinal): Cardinal;
begin
  if InputSize > 0 then
    Result := (InputSize + 2) div 3 * 4 + (InputSize - 1) div MIME_DECODED_LINE_BREAK * 2
  else
    Result := InputSize;
end;

//--------------------------------------------------------------------------------------------------

function MimeEncodedSizeNoCRLF(const InputSize: Cardinal): Cardinal;
begin
  Result := (InputSize + 2) div 3 * 4;
end;

//--------------------------------------------------------------------------------------------------

function MimeDecodedSize(const InputSize: Cardinal): Cardinal;
begin
  Result := (InputSize + 3) div 4 * 3;
end;

//--------------------------------------------------------------------------------------------------
// Primary functions & procedures
//--------------------------------------------------------------------------------------------------

procedure MimeEncode(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
var
  IDelta, ODelta: Cardinal;
begin
  MimeEncodeFullLines(InputBuffer, InputByteCount, OutputBuffer);
  IDelta := InputByteCount div MIME_DECODED_LINE_BREAK; // Number of lines processed so far.
  ODelta := IDelta * (MIME_ENCODED_LINE_BREAK + 2);
  IDelta := IDelta * MIME_DECODED_LINE_BREAK;
  MimeEncodeNoCRLF(Pointer(Cardinal(@InputBuffer) + IDelta)^, InputByteCount - IDelta, Pointer(Cardinal(@OutputBuffer) + ODelta)^);
end;

//--------------------------------------------------------------------------------------------------

procedure MimeEncodeFullLines(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
var
  B, InnerLimit, OuterLimit: Cardinal;
  InPtr: PByte3;
  OutPtr: PByte4;
begin
  { Do we have enough input to encode a full line? }
  if InputByteCount < MIME_DECODED_LINE_BREAK then
    Exit;

  InPtr := @InputBuffer;
  OutPtr := @OutputBuffer;

  InnerLimit := Cardinal(InPtr);
  Inc(InnerLimit, MIME_DECODED_LINE_BREAK);

  OuterLimit := Cardinal(InPtr);
  Inc(OuterLimit, InputByteCount);

  { Multiple line loop. }
  repeat

    { Single line loop. }
    repeat
      { Read 3 bytes from InputBuffer. }
      B := InPtr^.b1;
      B := B shl 8;
      B := B or InPtr^.b2;
      B := B shl 8;
      B := B or InPtr^.b3;
      Inc(InPtr);
      { Write 4 bytes to OutputBuffer (in reverse order). }
      OutPtr^.b4 := MIME_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutPtr^.b3 := MIME_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutPtr^.b2 := MIME_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutPtr^.b1 := MIME_ENCODE_TABLE[B];
      Inc(OutPtr);
    until Cardinal(InPtr) >= InnerLimit;

    { Write line break (CRLF). }
    OutPtr^.b1 := 13;
    OutPtr^.b2 := 10;
    Inc(Cardinal(OutPtr), 2);

    Inc(InnerLimit, MIME_DECODED_LINE_BREAK);
  until InnerLimit > OuterLimit;
end;

//--------------------------------------------------------------------------------------------------

procedure MimeEncodeNoCRLF(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
var
  B, InnerLimit, OuterLimit: Cardinal;
  InPtr: PByte3;
  OutPtr: PByte4;
begin
  if InputByteCount = 0 then Exit;

  InPtr := @InputBuffer;
  OutPtr := @OutputBuffer;

  OuterLimit := InputByteCount div 3 * 3;

  InnerLimit := Cardinal(InPtr);
  Inc(InnerLimit, OuterLimit);

  { Last line loop. }
  while Cardinal(InPtr) < InnerLimit do
  begin
    { Read 3 bytes from InputBuffer. }
    B := InPtr^.b1;
    B := B shl 8;
    B := B or InPtr^.b2;
    B := B shl 8;
    B := B or InPtr^.b3;
    Inc(InPtr);
    { Write 4 bytes to OutputBuffer (in reverse order). }
    OutPtr^.b4 := MIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr^.b3 := MIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr^.b2 := MIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr^.b1 := MIME_ENCODE_TABLE[B];
    Inc(OutPtr);
  end;

  { End of data & padding. }
  case InputByteCount - OuterLimit of
    1:
      begin
        B := InPtr^.b1;
        B := B shl 4;
        OutPtr.b2 := MIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.b1 := MIME_ENCODE_TABLE[B];
        OutPtr.b3 := MIME_PAD_CHAR; { Pad remaining 2 bytes. }
        OutPtr.b4 := MIME_PAD_CHAR;
      end;
    2:
      begin
        B := InPtr^.b1;
        B := B shl 8;
        B := B or InPtr^.b2;
        B := B shl 2;
        OutPtr.b3 := MIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.b2 := MIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.b1 := MIME_ENCODE_TABLE[B];
        OutPtr.b4 := MIME_PAD_CHAR; { Pad remaining byte. }
      end;
  end;
end;

//--------------------------------------------------------------------------------------------------
// Decoding Core
//--------------------------------------------------------------------------------------------------

function MimeDecode(const InputBuffer; const InputBytesCount: Cardinal; out OutputBuffer): Cardinal;
var
  ByteBuffer, ByteBufferSpace: Cardinal;
begin
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  Result := MimeDecodePartial(InputBuffer, InputBytesCount, OutputBuffer, ByteBuffer, ByteBufferSpace);
  Inc(Result, MimeDecodePartialEnd(Pointer(Cardinal(@OutputBuffer) + Result)^, ByteBuffer, ByteBufferSpace));
end;

//--------------------------------------------------------------------------------------------------

function MimeDecodePartial(const InputBuffer; const InputBytesCount: Cardinal; out OutputBuffer; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Cardinal;
var
  lByteBuffer, lByteBufferSpace, C: Cardinal;
  InPtr, OuterLimit: ^Byte;
  OutPtr: PByte3;
begin
  if InputBytesCount > 0 then
    begin
      InPtr := @InputBuffer;
      Cardinal(OuterLimit) := Cardinal(InPtr) + InputBytesCount;
      OutPtr := @OutputBuffer;
      lByteBuffer := ByteBuffer;
      lByteBufferSpace := ByteBufferSpace;
      while InPtr <> OuterLimit do
        begin
          { Read from InputBuffer. }
          C := MIME_DECODE_TABLE[InPtr^];
          Inc(InPtr);
          if C = $FF then Continue;
          lByteBuffer := lByteBuffer shl 6;
          lByteBuffer := lByteBuffer or C;
          Dec(lByteBufferSpace);
          { Have we read 4 bytes from InputBuffer? }
          if lByteBufferSpace <> 0 then Continue;

          { Write 3 bytes to OutputBuffer (in reverse order). }
          OutPtr^.b3 := Byte(lByteBuffer);
          lByteBuffer := lByteBuffer shr 8;
          OutPtr^.b2 := Byte(lByteBuffer);
          lByteBuffer := lByteBuffer shr 8;
          OutPtr^.b1 := Byte(lByteBuffer);
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

function MimeDecodePartialEnd(out OutputBuffer; const ByteBuffer: Cardinal; const ByteBufferSpace: Cardinal): Cardinal;
var
  lByteBuffer: Cardinal;
begin
  case ByteBufferSpace of
    1:
      begin
        lByteBuffer := ByteBuffer shr 2;
        PByte3(@OutputBuffer)^.b2 := Byte(lByteBuffer);
        lByteBuffer := lByteBuffer shr 8;
        PByte3(@OutputBuffer)^.b1 := Byte(lByteBuffer);
        Result := 2;
      end;
    2:
      begin
        lByteBuffer := ByteBuffer shr 4;
        PByte3(@OutputBuffer)^.b1 := Byte(lByteBuffer);
        Result := 1;
      end;
  else
    Result := 0;
  end;
end;

//--------------------------------------------------------------------------------------------------
// File Encoding & Decoding
//--------------------------------------------------------------------------------------------------

procedure MimeEncodeFile(const InputFileName, OutputFileName: AnsiString);
var
  InputStream, OutputStream: TFileStream;
begin
  InputStream := TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutputStream := TFileStream.Create(OutputFileName, fmCreate);
    try
      MimeEncodeStream(InputStream, OutputStream);
    finally
      OutputStream.Free;
    end;
  finally
    InputStream.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure MimeEncodeFileNoCRLF(const InputFileName, OutputFileName: AnsiString);
var
  InputStream, OutputStream: TFileStream;
begin
  InputStream := TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutputStream := TFileStream.Create(OutputFileName, fmCreate);
    try
      MimeEncodeStreamNoCRLF(InputStream, OutputStream);
    finally
      OutputStream.Free;
    end;
  finally
    InputStream.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure MimeDecodeFile(const InputFileName, OutputFileName: AnsiString);
var
  InputStream, OutputStream: TFileStream;
begin
  InputStream := TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutputStream := TFileStream.Create(OutputFileName, fmCreate);
    try
      MimeDecodeStream(InputStream, OutputStream);
    finally
      OutputStream.Free;
    end;
  finally
    InputStream.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------
// Stream Encoding & Decoding
//--------------------------------------------------------------------------------------------------

procedure MimeEncodeStream(const InputStream: TStream; const OutputStream: TStream);
var
  InputBuffer: array[0..MIME_BUFFER_SIZE - 1] of Byte;
  OutputBuffer: array[0..(MIME_BUFFER_SIZE + 2) div 3 * 4 + MIME_BUFFER_SIZE div MIME_DECODED_LINE_BREAK * 2 - 1] of Byte;
  BytesRead: Cardinal;
  IDelta, ODelta: Cardinal;
begin
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));

  while BytesRead = SizeOf(InputBuffer) do
    begin
      MimeEncodeFullLines(InputBuffer, SizeOf(InputBuffer), OutputBuffer);
      OutputStream.Write(OutputBuffer, SizeOf(OutputBuffer));
      BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
    end;

  MimeEncodeFullLines(InputBuffer, BytesRead, OutputBuffer);

  IDelta := BytesRead div MIME_DECODED_LINE_BREAK; // Number of lines processed.
  ODelta := IDelta * (MIME_ENCODED_LINE_BREAK + 2);
  IDelta := IDelta * MIME_DECODED_LINE_BREAK;
  MimeEncodeNoCRLF(Pointer(Cardinal(@InputBuffer) + IDelta)^, BytesRead - IDelta, Pointer(Cardinal(@OutputBuffer) + ODelta)^);

  OutputStream.Write(OutputBuffer, MimeEncodedSize(BytesRead));
end;

//--------------------------------------------------------------------------------------------------

procedure MimeEncodeStreamNoCRLF(const InputStream: TStream; const OutputStream: TStream);
var
  InputBuffer: array[0..MIME_BUFFER_SIZE - 1] of Byte;
  OutputBuffer: array[0..((MIME_BUFFER_SIZE + 2) div 3) * 4 - 1] of Byte;
  BytesRead: Cardinal;
begin
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  while BytesRead = SizeOf(InputBuffer) do
    begin
      MimeEncodeNoCRLF(InputBuffer, SizeOf(InputBuffer), OutputBuffer);
      OutputStream.Write(OutputBuffer, SizeOf(OutputBuffer));
      BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
    end;

  MimeEncodeNoCRLF(InputBuffer, BytesRead, OutputBuffer);
  OutputStream.Write(OutputBuffer, MimeEncodedSizeNoCRLF(BytesRead));
end;

//--------------------------------------------------------------------------------------------------

procedure MimeDecodeStream(const InputStream: TStream; const OutputStream: TStream);
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  InputBuffer: array[0..MIME_BUFFER_SIZE - 1] of Byte;
  OutputBuffer: array[0..(MIME_BUFFER_SIZE + 3) div 4 * 3 - 1] of Byte;
  BytesRead: Cardinal;
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

// History:

// $Log$
// Revision 1.8  2004/07/07 22:34:25  mthoma
// Added Ralf MimeStreams and MimeFile utilities...
//
// Revision 1.7  2004/06/14 13:05:18  marquardt
// style cleaning ENDIF, Tabs
//
// Revision 1.6  2004/05/05 00:09:59  mthoma
// Updated headers: Added donors as contributors, adjusted the initial authors, added cvs names when they were not obvious. Changed $data to $date where necessary,
//
// Revision 1.5  2004/04/09 20:21:11  mthoma
// Updated to Ralfs latest release. That also fixed 0000406.
//
// Revision 1.4  2004/04/06 04:53:18  peterjhaas
// adapt compiler conditions, add log entry
//

end.
