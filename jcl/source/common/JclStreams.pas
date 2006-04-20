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
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclStreams;

{$I jcl.inc}

interface

uses
  Classes;

type
  TJclEmptyStream = class(TStream)
  protected
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  TJclNullStream = class(TStream)
  private
    FPosition: Int64;
    FSize: Int64;
  protected
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

implementation

//=== { TJclEmptyStream } ====================================================

procedure TJclEmptyStream.SetSize(NewSize: Longint);
begin
end;

procedure TJclEmptyStream.SetSize(const NewSize: Int64);
begin
end;

function TJclEmptyStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;

function TJclEmptyStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;

function TJclEmptyStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := 0;
end;

function TJclEmptyStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := 0;
end;

//=== { TJclNullStream } =====================================================

procedure TJclNullStream.SetSize(NewSize: Longint);
begin
  SetSize(Int64(NewSize));
end;

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
    FillChar(Buffer, Count, 0);
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

function TJclNullStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning:
      Result := Seek(Int64(Offset), soBeginning);
    soFromCurrent:
      Result := Seek(Int64(Offset), soCurrent);
    soFromEnd:
      Result := Seek(Int64(Offset), soEnd);
  else
    Result := -1;
  end;
end;

function TJclNullStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning:
      begin
        if Offset >= 0 then
          FPosition := Offset;
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
          FPosition := FSize - Offset;
        if FPosition < 0 then
          FPosition := 0;
        Result := FPosition;
      end;
  else
    Result := -1;
  end;
end;

// History:

// $Log$

end.
