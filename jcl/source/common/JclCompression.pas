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
{ The Original Code is JclCompression.pas.                                                         }
{                                                                                                  }
{ The Initial Developer of the Original Code is Matthias Thoma.                                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Alternatively, the contents of this file may be used under the terms of  the GNU Lesser General  }
{ Public License (the  "LGPL License"), in which case  the provisions of the LGPL License are      }
{ applicable instead of those above. If you wish to allow use of your version of this file only    }
{ under the terms of the LGPL License and not to allow others to use your version of this file     }
{ under the MPL, indicate your decision by deleting the provisions above and replace them with the }
{ notice and other provisions required by the LGPL License. If you do not delete the provisions    }
{ above, a recipient may use your version of this file under either the MPL or the LGPL License.   }
{                                                                                                  }
{ For more information about the LGPL:                                                             }
{ http://www.gnu.org/copyleft/lesser.html                                                          }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit is still in alpha state. It is likely that it will change a lot. Suggestions are       }
{ welcome.                                                                                         }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$

unit JclCompression;

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  Types,
  {$ENDIF UNIX}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  SysUtils, Classes,
  JclBase,
  zlibh;

{**************************************************************************************************}
{
                                       TJclCompressionStream
                                               -  -
                         -----------------------  --------------------------
                         -                                                 -
                   TJclCompressStream                            TJclDecompressStream
                         -                                                 -
            ---------------------------------                ---------------------------------
            -               -               -                -                 -             -
            -               -               -                -                 -             -
    TJclZLibCompressStream  -   TBZIP2CompressStram   TJclZLibDecompressStream -   TBZIP2DeCompressStream
                            -                                                  -
                            -                                          TGZDecompressStream
                      TGZCompressStream

                                                                               }
{**************************************************************************************************}

type
  TJclCompressionStream = class(TStream)
  private
    FOnProgress: TNotifyEvent;
    FBuffer: Pointer;
    FBufferSize: Cardinal;
    FStream: TStream;
  protected
    function SetBufferSize(Size: Cardinal): Cardinal; virtual;
    procedure Progress(Sender: TObject); dynamic;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure Reset; virtual;
  end;

  TJclCompressStream = class(TJclCompressionStream)
  public
    function Flush: Integer; dynamic; abstract;
    constructor Create(Destination: TStream);
  end;

  TJclDecompressStream = class(TJclCompressionStream)
  public
    constructor Create(Source: TStream);
  end;

  // ZIP Support
  TJclCompressionLevel = Integer;

  TJclZLibCompressStream = class(TJclCompressStream)
  private
    FWindowBits: Integer;
    FMemLevel: Integer;
    FMethod: Integer;
    FStrategy: Integer;
    FDeflateInitialized: Boolean;
    FCompressionLevel: Integer;
  protected
    ZLibRecord: TZStreamRec;
    procedure SetCompressionLevel(Value: Integer);
    procedure SetStrategy(Value: Integer);
    procedure SetMemLevel(Value: Integer);
    procedure SetMethod(Value: Integer);
    procedure SetWindowBits(Value: Integer);
  public
    constructor Create(Destination: TStream; CompressionLevel: TJclCompressionLevel = -1);
    destructor Destroy; override;
    function Flush: Integer; override;
    procedure Reset; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property WindowBits: Integer read FWindowBits write SetWindowBits;
    property MemLevel: Integer read FMemLevel write SetMemLevel;
    property Method: Integer read FMethod write SetMethod;
    property Strategy: Integer read FStrategy write SetStrategy;
    property CompressionLevel: Integer read FCompressionLevel write SetCompressionLevel;
  end;

  TJclZLibDecompressStream = class(TJclDecompressStream)
  private
    FWindowBits: Integer;
    FInflateInitialized: Boolean;
  protected
    ZLibRecord: TZStreamRec;
    procedure SetWindowBits(Value: Integer);
  public
    constructor Create(Source: TStream; WindowBits: Integer = DEF_WBITS);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property WindowBits: Integer read FWindowBits write SetWindowBits;
  end;

  // GZIP Support
  TJclGZIPCompressionStream = class(TJclCompressionStream)
  end;

  TJclGZIPDecompressionStream = class(TJclDecompressStream)
  end;

  // RAR Support
  TJclRARCompressionStream = class(TJclCompressionStream)
  end;

  TJclRARDecompressionStream = class(TJclDecompressStream)
  end;

  // TAR Support
  TJclTARCompressionStream = class(TJclCompressionStream)
  end;

  TJclTARDecompressionStream = class(TJclDecompressStream)
  end;

  // BZIP2 Support
(*
  TJclBZIP2CompressStream = class(TJclCompressStream)
 private
    FDeflateInitialized: Boolean;

  protected
    BZLibRecord: TBZStream;
  public
    function Flush: Integer; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;

    constructor Create(Destination: TStream; CompressionLevel: TJclCompressionLevel = -1);
    destructor Destroy; override;
  end;

  TJclBZIP2DecompressStream = class(TJclDecompressStream)
  private
    FInflateInitialized: Boolean;

  protected
    BZLibRecord: TBZStream;

  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;

    constructor Create(Source: TStream); overload;
    destructor Destroy; override;
  end;
*)

  EJclCompressionError = class(EJclError);

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
  JclResources;

const
  JclDefaultBufferSize = 131072; // 128k

//=== { TJclCompressionStream } ==============================================

constructor TJclCompressionStream.Create(Stream: TStream);
begin
  inherited Create;
  FBuffer := nil;
  SetBufferSize(JclDefaultBufferSize);
end;

destructor TJclCompressionStream.Destroy;
begin
  SetBufferSize(0);
  inherited Destroy;
end;

function TJclCompressionStream.Read(var Buffer; Count: Longint): Longint;
begin
  raise EJclCompressionError.CreateRes(@RsCompressionReadNotSupported);
end;

function TJclCompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EJclCompressionError.CreateRes(@RsCompressionWriteNotSupported);
end;

function TJclCompressionStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  raise EJclCompressionError.CreateRes(@RsCompressionSeekNotSupported);
end;

procedure TJclCompressionStream.Reset;
begin
  raise EJclCompressionError.CreateRes(@RsCompressionResetNotSupported);
end;

function TJclCompressionStream.SetBufferSize(Size: Cardinal): Cardinal;
begin
  if FBuffer <> nil then
    FreeMem(FBuffer, FBufferSize);

  FBufferSize := Size;

  if FBufferSize > 0 then
    GetMem(FBuffer, FBufferSize)
  else
    FBuffer := nil;

  Result := FBufferSize;
end;

procedure TJclCompressionStream.Progress(Sender: TObject);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender);
end;

//=== { TJclCompressStream } =================================================

constructor TJclCompressStream.Create(Destination: TStream);
begin
  inherited Create(Destination);
  FStream := Destination;
end;

//=== { TJclDecompressStream } ===============================================

constructor TJclDecompressStream.Create(Source: TStream);
begin
  inherited Create(Source);
  FStream := Source;
end;


//=== { TJclZLibCompressionStream } ==========================================

{ Error checking helper }

function ZLibCheck(const ErrCode: Integer): Integer;
begin
  Result := ErrCode;
  if ErrCode < 0 then
    case ErrCode of
      Z_ERRNO:
        raise EJclCompressionError.CreateRes(@RsCompressionZLibZErrNo);
      Z_STREAM_ERROR:
        raise EJclCompressionError.CreateRes(@RsCompressionZLibZStreamError);
      Z_DATA_ERROR:
        raise EJclCompressionError.CreateRes(@RsCompressionZLibZDataError);
      Z_MEM_ERROR:
        raise EJclCompressionError.CreateRes(@RsCompressionZLibZMemError);
      Z_BUF_ERROR:
        raise EJclCompressionError.CreateRes(@RsCompressionZLibZBufError);
      Z_VERSION_ERROR:
        raise EJclCompressionError.CreateRes(@RsCompressionZLibZVersionError);
    else
      raise EJclCompressionError.CreateRes(@RsCompressionZLibError);
    end;
end;

constructor TJclZLibCompressStream.Create(Destination: TStream; CompressionLevel: TJclCompressionLevel);
begin
  inherited Create(Destination);

  Assert(FBuffer <> nil);
  Assert(FBufferSize > 0);

  // Initialize ZLib StreamRecord
  with ZLibRecord do
  begin
    zalloc := nil; // Use build-in memory allocation functionality
    zfree := nil;
    next_in := nil;
    avail_in := 0;
    next_out := FBuffer;
    avail_out := FBufferSize;
  end;

  FWindowBits := DEF_WBITS;
  FMemLevel := DEF_MEM_LEVEL;
  FMethod := Z_DEFLATED;
  FStrategy := Z_DEFAULT_STRATEGY;
  FCompressionLevel := CompressionLevel;
  FDeflateInitialized := False;
end;

destructor TJclZLibCompressStream.Destroy;
begin
  Flush;
  if FDeflateInitialized then
  begin
    ZLibRecord.next_in := nil;
    ZLibRecord.avail_in := 0;
    ZLibRecord.avail_out := 0;
    ZLibRecord.next_out := nil;

    ZLibCheck(deflateEnd(ZLibRecord));
  end;

  inherited Destroy;
end;

function TJclZLibCompressStream.Write(const Buffer; Count: Longint): Longint;
begin
  if not FDeflateInitialized then
  begin
    ZLibCheck(deflateInit2(ZLibRecord, FCompressionLevel, FMethod, FWindowBits, FMemLevel, FStrategy));
    FDeflateInitialized := True;
  end;

  ZLibRecord.next_in := @Buffer;
  ZLibRecord.avail_in := Count;

  while ZLibRecord.avail_in > 0 do
  begin
    ZLibCheck(deflate(ZLibRecord, Z_NO_FLUSH));

    if ZLibRecord.avail_out = 0 then   // Output buffer empty. Write to stream and go on...
    begin
      FStream.WriteBuffer(FBuffer^, FBufferSize);
      Progress(Self);
      ZLibRecord.next_out := FBuffer;
      ZLibRecord.avail_out := FBufferSize;
    end;
  end;

  Result := Count;
end;

function TJclZLibCompressStream.Flush: Integer;
begin
  Result := 0;

  if FDeflateInitialized then
  begin
    ZLibRecord.next_in := nil;
    ZLibRecord.avail_in := 0;

    while (ZLibCheck(deflate(ZLibRecord, Z_FINISH)) <> Z_STREAM_END) and
      (ZLibRecord.avail_out = 0) do
    begin
      FStream.WriteBuffer(FBuffer^, FBufferSize);
      Progress(Self);

      ZLibRecord.next_out := FBuffer;
      ZLibRecord.avail_out := FBufferSize;
      Inc(Result, FBufferSize);
    end;

    if ZLibRecord.avail_out < FBufferSize then
    begin
      FStream.WriteBuffer(FBuffer^, FBufferSize-ZLibRecord.avail_out);
      Progress(Self);
      Inc(Result, FBufferSize - ZLibRecord.avail_out);
      ZLibRecord.next_out := FBuffer;
      ZLibRecord.avail_out := FBufferSize;
    end;
  end;
end;

function TJclZLibCompressStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if (Offset = 0) and (Origin = soFromCurrent) then
   Result := ZLibRecord.total_in
  else
  if (Offset = 0) and (Origin = soFromBeginning) and (ZLibRecord.total_in = 0) then
    Result := 0
  else
    Result := inherited Seek(Offset, Origin);
end;

procedure TJclZLibCompressStream.SetWindowBits(Value: Integer);
begin
  FWindowBits := Value;
end;

procedure TJclZLibCompressStream.SetMethod(Value: Integer);
begin
  FMethod := Value;
end;

procedure TJclZLibCompressStream.SetStrategy(Value: Integer);
begin
  FStrategy := Value;
  if FDeflateInitialized then
    ZLibCheck(deflateParams(ZLibRecord, FCompressionLevel, FStrategy));
end;

procedure TJclZLibCompressStream.SetMemLevel(Value: Integer);
begin
  FMemLevel := Value;
end;

procedure TJclZLibCompressStream.SetCompressionLevel(Value: Integer);
begin
  FCompressionLevel := Value;
  if FDeflateInitialized then
    ZLibCheck(deflateParams(ZLibRecord, FCompressionLevel, FStrategy));
end;

procedure TJclZLibCompressStream.Reset;
begin
  if FDeflateInitialized then
  begin
    Flush;
    ZLibCheck(deflateReset(ZLibRecord));
  end;
end;


//=== {  TJclZLibDecompressionStream } =======================================

constructor TJclZLibDecompressStream.Create(Source: TStream; WindowBits: Integer = DEF_WBITS);
begin
  inherited Create(Source);

  // Initialize ZLib StreamRecord
  with ZLibRecord do
  begin
    zalloc := nil; // Use build-in memory allocation functionality
    zfree := nil;
    next_in := nil;
    avail_in := 0;
    next_out := FBuffer;
    avail_out := FBufferSize;
  end;

  FInflateInitialized := False;
  FWindowBits := WindowBits;
end;

destructor TJclZLibDecompressStream.Destroy;
begin
  if FInflateInitialized then
  begin
    FStream.Seek(-ZLibRecord.avail_in, soFromCurrent);
    ZLibCheck(inflateEnd(ZLibRecord));
  end;

  inherited Destroy;
end;

function TJclZLibDecompressStream.Read(var Buffer; Count: Longint): Longint;
begin
  if not FInflateInitialized then
  begin
    ZLibCheck(InflateInit2(ZLibRecord, FWindowBits));
    FInflateInitialized := True;
  end;

  ZLibRecord.next_out := @Buffer;
  ZLibRecord.avail_out := Count;

  while ZLibRecord.avail_out > 0 do     // as long as we have data
  begin
    if ZLibRecord.avail_in = 0 then
    begin
      ZLibRecord.avail_in := FStream.Read(FBuffer^, FBufferSize);

      if ZLibRecord.avail_in = 0 then
      begin
        Result := Count - Longint(ZLibRecord.avail_out);
        Exit;
      end;

      ZLibRecord.next_in := FBuffer;
    end;

    if ZLibRecord.avail_in > 0 then
    begin
      ZLibCheck(inflate(ZLibRecord, Z_NO_FLUSH));
      Progress(Self);
    end;
  end;

  Result := Count;
end;

function TJclZLibDecompressStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
   if (Offset = 0) and (Origin = soFromCurrent) then
    Result := ZLibRecord.total_out
   else
     Result := inherited Seek(Offset, Origin);
end;

procedure TJclZLibDecompressStream.SetWindowBits(Value: Integer);
begin
  FWindowBits := Value;
end;

//=== { TJclBZLibCompressionStream } =========================================
(*
{ Error checking helper }

function BZIP2LibCheck(const ErrCode: Integer): Integer;
begin
  Result := ErrCode;

  if ErrCode < 0 then
  begin
    case ErrCode of
      Z_ERRNO:         raise EJclCompressionError.CreateRes(@RsCompressionZLibZErrNo);
      Z_STREAM_ERROR:  raise EJclCompressionError.CreateRes(@RsCompressionZLibZStreamError);
      Z_DATA_ERROR:    raise EJclCompressionError.CreateRes(@RsCompressionZLibZDataError);
      Z_MEM_ERROR:     raise EJclCompressionError.CreateRes(@RsCompressionZLibZMemError);
      Z_BUF_ERROR:     raise EJclCompressionError.CreateRes(@RsCompressionZLibZBufError);
      Z_VERSION_ERROR: raise EJclCompressionError.CreateRes(@RsCompressionZLibZVersionError);
    else
      raise EJclCompressionError.CreateRes(@RsCompressionZLibError);
    end;
  end;
end;

constructor TJclBZIP2CompressStream.Create(Destination: TStream; CompressionLevel: TJclCompressionLevel);
begin
  inherited Create(Destination);

  Assert(FBuffer <> nil);
  Assert(FBufferSize > 0);

  // Initialize ZLib StreamRecord
  with BZLibRecord do
  begin
    bzalloc   := nil; // Use build-in memory allocation functionality
    bzfree    := nil;
    next_in   := nil;
    avail_in  := 0;
    next_out  := FBuffer;
    avail_out := FBufferSize;

  end;

  FDeflateInitialized := False;
end;

destructor TJclBZIP2CompressStream.Destroy;
begin
  Flush;
  if FDeflateInitialized then
    BZIP2LibCheck(BZ2_bzCompressEnd(@BZLibRecord));

  inherited Destroy;
end;

function TJclBZIP2CompressStream.Write(const Buffer; Count: Longint): Longint;
begin
  if not FDeflateInitialized then
  begin
    BZIP2LibCheck(BZ2_bzCompressInit(@BZLibRecord,9,0,0));
    FDeflateInitialized := True;
  end;

  BZLibRecord.next_in := @Buffer;
  BZLibRecord.avail_in := Count;

  while BZLibRecord.avail_in > 0 do
  begin
    BZIP2LibCheck(BZ2_bzCompress(@BZLibRecord, BZ_RUN));

    if BZLibRecord.avail_out = 0 then   // Output buffer empty. Write to stream and go on...
    begin
      FStream.WriteBuffer(FBuffer^, FBufferSize);
      Progress(Self);
      BZLibRecord.next_out := FBuffer;
      BZLibRecord.avail_out := FBufferSize;
    end;
  end;

  Result := Count;
end;

function TJclBZIP2CompressStream.Flush: Integer;
begin
    Result := 0;

    if FDeflateInitialized then
    begin
      BZLibRecord.next_in := nil;
      BZLibRecord.avail_in := 0;

      while (BZIP2LibCheck(BZ2_bzCompress(@BZLibRecord, BZ_FLUSH)) <> Z_STREAM_END) and (BZLibRecord.avail_out = 0) do
      begin
        FStream.WriteBuffer(FBuffer^, FBufferSize);
        Progress(Self);
        
        BZLibRecord.next_out := FBuffer;
        BZLibRecord.avail_out := FBufferSize;
        Result := Result + FBufferSize;
      end;

      if BZLibRecord.avail_out < FBufferSize then
      begin
        FStream.WriteBuffer(FBuffer^, FBufferSize-BZLibRecord.avail_out);
        Progress(Self);
        Result := Result + FBufferSize-BZLibRecord.avail_out;
        BZLibRecord.next_out := FBuffer;
        BZLibRecord.avail_out := FBufferSize;
      end;
    end;
end;

function TJclBZIP2CompressStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
   if (Offset = 0) and (Origin = soFromCurrent) then
    Result := BZLibRecord.total_in_lo32
   else
   if (Offset = 0) and (Origin = soFromBeginning) and (BZLibRecord.total_in_lo32 = 0) then
       Result := 0
   else
     Result := inherited Seek(Offset, Origin);
end;

//=== { TJclZLibDecompressionStream } ========================================

constructor TJclBZIP2DecompressStream.Create(Source: TStream);
begin
  inherited Create(Source);

  // Initialize ZLib StreamRecord
  with BZLibRecord do
  begin
    bzalloc   := nil; // Use build-in memory allocation functionality
    bzfree    := nil;
    opaque    := nil;
    next_in   := nil;
    state     := nil;
    avail_in  := 0;
    next_out  := FBuffer;
    avail_out := FBufferSize;
  end;

  FInflateInitialized := False;
end;

destructor TJclBZIP2DecompressStream.Destroy;
begin
  if FInflateInitialized then
  begin
    FStream.Seek(-BZLibRecord.avail_in, soFromCurrent);
    BZIP2LibCheck(BZ2_bzDecompressEnd(@BZLibRecord));
  end;

  inherited Destroy;
end;

function TJclBZIP2DecompressStream.Read(var Buffer; Count: Longint): Longint;
var
  avail_out_ctr: Integer;

begin
  if not FInflateInitialized then
  begin
    BZIP2LibCheck(BZ2_bzDecompressInit(@BZLibRecord,0,0));
    FInflateInitialized := True;
  end;

  BZLibRecord.next_out := @Buffer;
  BZLibRecord.avail_out := Count;
  avail_out_ctr := Count;

  while avail_out_ctr > 0 do     // as long as we have data
  begin
    if BZLibRecord.avail_in = 0 then
    begin
      BZLibRecord.avail_in := FStream.Read(FBuffer^, FBufferSize);
      if BZLibRecord.avail_in = 0 then
      begin
        Result := Count - avail_out_ctr;
        Exit;
      end;

      BZLibRecord.next_in := FBuffer;
    end;


    if BZLibRecord.avail_in > 0 then
    begin
      BZIP2LibCheck(BZ2_bzDecompress(@BZLibRecord));
      avail_out_ctr := Count - BZLibRecord.avail_out;
    end
  end;

  Result := Count;
end;

function TJclBZIP2DecompressStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
   if (Offset = 0) and (Origin = soFromCurrent) then
    Result := BZLibRecord.total_out_lo32
   else
     Result := inherited Seek(Offset, Origin);
end;
*)

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

