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
{ The Original Code is AbstractContainer.pas and DCL_Util.pas.                                     }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Daniele Teti (dade2004)                                                                        }
{   Robert Marquardt (marquardt)                                                                   }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ The Delphi Container Library                                                                     }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclAbstractContainers;

{$I jcl.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  SysUtils, Classes, JclBase, JclContainerIntf;

type
  TJclIntfCriticalSection = class(TObject, IInterface)
  private
    FCriticalSection: TRTLCriticalSection;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TJclAbstractContainer = class(TInterfacedObject)
  {$IFDEF THREADSAFE}
  private
    FCriticalSection: TJclIntfCriticalSection;
  protected
    function EnterCriticalSection: IInterface;
  public
    constructor Create;
    destructor Destroy; override;
  {$ENDIF THREADSAFE}
  end;

  TJclStrCollection = class(TJclAbstractContainer, IJclStrCollection)
  protected
    { IJclStrCollection }
    function Add(const AString: string): Boolean; virtual; abstract;
    function AddAll(ACollection: IJclStrCollection): Boolean; virtual; abstract;
    procedure Clear; virtual; abstract;
    function Contains(const AString: string): Boolean; virtual; abstract;
    function ContainsAll(ACollection: IJclStrCollection): Boolean; virtual; abstract;
    function Equals(ACollection: IJclStrCollection): Boolean; virtual; abstract;
    function First: IJclStrIterator; virtual; abstract;
    function IsEmpty: Boolean; virtual; abstract;
    function Last: IJclStrIterator; virtual; abstract;
    function Remove(const AString: string): Boolean; overload; virtual; abstract;
    function RemoveAll(ACollection: IJclStrCollection): Boolean; virtual; abstract;
    function RetainAll(ACollection: IJclStrCollection): Boolean; virtual; abstract;
    function Size: Integer; virtual; abstract;
    procedure LoadFromStrings(Strings: TStrings);
    procedure SaveToStrings(Strings: TStrings);
    procedure AppendToStrings(Strings: TStrings);
    procedure AppendFromStrings(Strings: TStrings);
    function GetAsStrings: TStrings;
    function GetAsDelimited(Separator: string = AnsiLineBreak): string;
    procedure AppendDelimited(AString: string; Separator: string = AnsiLineBreak);
    procedure LoadDelimited(AString: string; Separator: string = AnsiLineBreak);
  end;

implementation

//=== { TJclIntfCriticalSection } ============================================

constructor TJclIntfCriticalSection.Create;
begin
  inherited Create;
  InitializeCriticalSection(FCriticalSection);
end;

destructor TJclIntfCriticalSection.Destroy;
begin
  DeleteCriticalSection(FCriticalSection);
  inherited Destroy;
end;

function TJclIntfCriticalSection._AddRef: Integer;
begin
  EnterCriticalSection(FCriticalSection);
  Result := 0;
end;

function TJclIntfCriticalSection._Release: Integer;
begin
  LeaveCriticalSection(FCriticalSection);
  Result := 0;
end;

function TJclIntfCriticalSection.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

//=== { TJclAbstractContainer } ==============================================

{$IFDEF THREADSAFE}

constructor TJclAbstractContainer.Create;
begin
  FCriticalSection := TJclIntfCriticalSection.Create;
end;

destructor TJclAbstractContainer.Destroy;
begin
  FCriticalSection.Free;
  inherited Destroy;
end;

function TJclAbstractContainer.EnterCriticalSection: IInterface;
begin
  Result := FCriticalSection as IInterface;
end;

{$ENDIF THREADSAFE}

//=== { TJclStrCollection } ==================================================

procedure TJclStrCollection.AppendDelimited(AString, Separator: string);
var
  Item: string;
  SepLen: Integer;
  PString, PSep, PPos: PChar;
begin
  PString := PChar(AString);
  PSep := PChar(Separator);
  PPos := StrPos(PString, PSep);
  if PPos <> nil then
  begin
    SepLen := StrLen(PSep);
    repeat
      //SetLength(Item, PPos - PString + 1);
      SetLength(Item, PPos - PString);
      Move(PString^, Item[1], PPos - PString);
      //Item[PPos - PString + 1] := #0;
      Add(Item);
      PString := PPos + SepLen;
      PPos := StrPos(PString, PSep);
    until PPos = nil;
    if StrLen(PString) > 0 then //ex. hello#world
      Add(PString);
  end
  else //There isnt a Separator in AString
    Add(AString);
end;

procedure TJclStrCollection.AppendFromStrings(Strings: TStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    Add(Strings[I]);
end;

procedure TJclStrCollection.AppendToStrings(Strings: TStrings);
var
  It: IJclStrIterator;
begin
  It := First;
  Strings.BeginUpdate;
  try
    while It.HasNext do
      Strings.Add(It.Next);
  finally
    Strings.EndUpdate;
  end;
end;

function TJclStrCollection.GetAsDelimited(Separator: string): string;
var
  It: IJclStrIterator;
begin
  It := First;
  Result := '';
  if It.HasNext then
    Result := It.Next;
  while It.HasNext do
    Result := Result + Separator + It.Next;
end;

function TJclStrCollection.GetAsStrings: TStrings;
begin
  Result := TStringList.Create;
  try
    AppendToStrings(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TJclStrCollection.LoadDelimited(AString, Separator: string);
begin
  Clear;
  AppendDelimited(AString, Separator);
end;

procedure TJclStrCollection.LoadFromStrings(Strings: TStrings);
begin
  Clear;
  AppendFromStrings(Strings);
end;

procedure TJclStrCollection.SaveToStrings(Strings: TStrings);
begin
  Strings.Clear;
  AppendToStrings(Strings);
end;

// History:

// $Log$
// Revision 1.5  2005/03/14 08:46:53  rrossmair
// - check-in in preparation for release 1.95
//
// Revision 1.4  2005/03/03 08:02:56  marquardt
// various style cleanings, bugfixes and improvements
//
// Revision 1.3  2005/03/02 17:48:54  rrossmair
// - replaced $IFDEF UNIX by $IFDEF HAS_UNIT_LIBC, fixed header
//
// Revision 1.2  2005/03/02 09:59:30  dade2004
// - added TJclStrCollection, which now serves as a common ancestor to all classes implementing IJclStrCollection.
// - replaced and bug-fixed JclAlgorithms.DCLAppendDelimited() by TJclStrCollection.AppendDelimited
//
// Revision 1.1  2005/02/24 03:57:10  rrossmair
// - donated DCL code, initial check-in
//

end.

