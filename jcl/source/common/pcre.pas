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
{ The Original Code is JclPRCE.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Peter Thornqvist.                                  }
{ Portions created by Peter Thornqvist are Copyright (C) of Peter Thornqvist. All rights reserved. }
{ Portions created by University of Cambridge are                                                  }
{ Copyright (C) 1997-2001 by University of Cambridge.                                              }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Robert Rossmair (rrossmair)                                                                    }
{                                                                                                  }
{ The latest release of PCRE is always available from                                              }
{ ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre-xxx.tar.gz                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Header conversion of pcre.h                                                                      }
{                                                                                                  }
{ Unit owner: Peter Thornqvist                                                                     }
{ Last modified: $Date$                                                      }
{                                                                                                  }
{**************************************************************************************************}

unit pcre;

{$I jedi.inc}

interface

(*************************************************
*       Perl-Compatible Regular Expressions      *
*************************************************)

{$WEAKPACKAGEUNIT ON}

// (p3) this is the switch to change between static and dynamic linking.
// It is set to dynamic by default. To disable simply insert a '.' before the '$'
//
// NOTE: if you enable static linking of DLL, this means that the pcre.dll *must*
// be in the users path or an AV will occur at startup

{$DEFINE PCRE_LINKONREQUEST}
(*$HPPEMIT '#include "pcre.h"'*)

const
  MAX_PATTERN_LENGTH = $10003;
  {$EXTERNALSYM MAX_PATTERN_LENGTH}
  MAX_QUANTIFY_REPEAT = $10000;
  {$EXTERNALSYM MAX_QUANTIFY_REPEAT}
  MAX_CAPTURE_COUNT = $FFFF;
  {$EXTERNALSYM MAX_CAPTURE_COUNT}
  MAX_NESTING_DEPTH = 200;
  {$EXTERNALSYM MAX_NESTING_DEPTH}

const
  (* Options *)
  PCRE_CASELESS = $0001;
  {$EXTERNALSYM PCRE_CASELESS}
  PCRE_MULTILINE = $0002;
  {$EXTERNALSYM PCRE_MULTILINE}
  PCRE_DOTALL = $0004;
  {$EXTERNALSYM PCRE_DOTALL}
  PCRE_EXTENDED = $0008;
  {$EXTERNALSYM PCRE_EXTENDED}
  PCRE_ANCHORED = $0010;
  {$EXTERNALSYM PCRE_ANCHORED}
  PCRE_DOLLAR_ENDONLY = $0020;
  {$EXTERNALSYM PCRE_DOLLAR_ENDONLY}
  PCRE_EXTRA = $0040;
  {$EXTERNALSYM PCRE_EXTRA}
  PCRE_NOTBOL = $0080;
  {$EXTERNALSYM PCRE_NOTBOL}
  PCRE_NOTEOL = $0100;
  {$EXTERNALSYM PCRE_NOTEOL}
  PCRE_UNGREEDY = $0200;
  {$EXTERNALSYM PCRE_UNGREEDY}
  PCRE_NOTEMPTY = $0400;
  {$EXTERNALSYM PCRE_NOTEMPTY}
  PCRE_UTF8 = $0800;
  {$EXTERNALSYM PCRE_UTF8}

  (* Exec-time and get-time error codes *)

  PCRE_ERROR_NOMATCH = -1;
  {$EXTERNALSYM PCRE_ERROR_NOMATCH}
  PCRE_ERROR_NULL = -2;
  {$EXTERNALSYM PCRE_ERROR_NULL}
  PCRE_ERROR_BADOPTION = -3;
  {$EXTERNALSYM PCRE_ERROR_BADOPTION}
  PCRE_ERROR_BADMAGIC = -4;
  {$EXTERNALSYM PCRE_ERROR_BADMAGIC}
  PCRE_ERROR_UNKNOWN_NODE = -5;
  {$EXTERNALSYM PCRE_ERROR_UNKNOWN_NODE}
  PCRE_ERROR_NOMEMORY = -6;
  {$EXTERNALSYM PCRE_ERROR_NOMEMORY}
  PCRE_ERROR_NOSUBSTRING = -7;
  {$EXTERNALSYM PCRE_ERROR_NOSUBSTRING}

  (* Request types for pcre_fullinfo() *)

  PCRE_INFO_OPTIONS = 0;
  {$EXTERNALSYM PCRE_INFO_OPTIONS}
  PCRE_INFO_SIZE = 1;
  {$EXTERNALSYM PCRE_INFO_SIZE}
  PCRE_INFO_CAPTURECOUNT = 2;
  {$EXTERNALSYM PCRE_INFO_CAPTURECOUNT}
  PCRE_INFO_BACKREFMAX = 3;
  {$EXTERNALSYM PCRE_INFO_BACKREFMAX}
  PCRE_INFO_FIRSTCHAR = 4;
  {$EXTERNALSYM PCRE_INFO_FIRSTCHAR}
  PCRE_INFO_FIRSTTABLE = 5;
  {$EXTERNALSYM PCRE_INFO_FIRSTTABLE}
  PCRE_INFO_LASTLITERAL = 6;
  {$EXTERNALSYM PCRE_INFO_LASTLITERAL}

type
  (* Types *)
  PPChar = ^PChar;
  PPPChar = ^PPChar;
  PInteger = ^Integer;
  PPointer = ^Pointer;

  real_pcre = record
    magic_number: Longword;
    size: Integer;
    tables: PChar;
    options: Longword;
    top_bracket: Word;
    top_backref: word;
    first_char: PChar;
    req_char: PChar;
    code: array [0..0] of Char;
  end;
  {$EXTERNALSYM real_pcre}
  TPCRE = real_pcre;
  PPCRE = ^TPCRE;

  real_pcre_extra = record
    options: PChar;
    start_bits: array [0..31] of Char;
  end;
{$EXTERNALSYM real_pcre_extra}
  TPCREExtra = real_pcre_extra;
  PPCREExtra = ^TPCREExtra;

(* Functions *)
{$IFNDEF PCRE_LINKONREQUEST}

function pcre_compile(const pattern: PChar; options: Integer;
  const errptr: PPChar; erroffset: PInteger; const tableptr: PChar): PPCRE; cdecl;
{$EXTERNALSYM pcre_compile}
function pcre_copy_substring(const subject: PChar; ovector: PInteger; stringcount, stringnumber: Integer;
  buffer: PChar; buffersize: Integer): Integer; cdecl;
{$EXTERNALSYM pcre_copy_substring}
function pcre_exec(const code: PPCRE; const extra: PPCREExtra; const subject: PChar;
{$EXTERNALSYM pcre_exec}
  length, startoffset, options: Integer; ovector: PInteger; ovecsize: Integer): Integer; cdecl;
function pcre_study(const code: PPCRE; options: Integer; const errptr: PPChar): PPCREExtra; cdecl;
{$EXTERNALSYM pcre_study}
function pcre_get_substring(const subject: PChar; ovector: PInteger;
{$EXTERNALSYM pcre_get_substring}
  stringcount, stringnumber: Integer; const stringptr: PPChar): Integer; cdecl;
function pcre_get_substring_list(const subject: PChar; ovector: PInteger;
  stringcount: Integer; listptr: PPPChar): Integer; cdecl;
{$EXTERNALSYM pcre_get_substring_list}
procedure pcre_free_substring(var stringptr: PChar); cdecl;
{$EXTERNALSYM pcre_free_substring}
procedure pcre_free_substring_list(var stringptr: PChar); cdecl;
{$EXTERNALSYM pcre_free_substring_list}
function pcre_maketables: PChar; cdecl;
{$EXTERNALSYM pcre_maketables}
function pcre_fullinfo(const code: PPCRE; const extra: PPCREExtra;
  what: Integer; where: Pointer): Integer; cdecl;
{$EXTERNALSYM pcre_fullinfo}
function pcre_info(const code: PPCRE; optptr, firstcharptr: PInteger): Integer; cdecl;
{$EXTERNALSYM pcre_info}
function pcre_version: PChar; cdecl;
{$EXTERNALSYM pcre_version}

// Don't use! These do *not* work!!!
function pcre_malloc(Size: Integer): Pointer; cdecl;
{$EXTERNALSYM pcre_malloc}
procedure pcre_free(P: Pointer); cdecl;
{$EXTERNALSYM pcre_free}

{$ELSE}
  // dynamic linking
type
  pcre_compile_func = function(const pattern: PChar; options: Integer;
    const errptr: PPChar; erroffset: PInteger; const tableptr: PChar): PPCRE; cdecl;
  pcre_copy_substring_func = function(const subject: PChar; ovector: PInteger; stringcount, stringnumber: Integer;
    buffer: PChar; buffersize: Integer): Integer; cdecl;
  pcre_exec_func = function(const code: PPCRE; const extra: PPCREExtra; const subject: PChar;
    length, startoffset, options: Integer; ovector: PInteger; ovecsize: Integer): Integer; cdecl;
  pcre_study_func = function(const code: PPCRE; options: Integer; const errptr: PPChar): PPCREExtra; cdecl;
  pcre_get_substring_func = function(const subject: PChar; ovector: PInteger;
    stringcount, stringnumber: Integer; const stringptr: PPChar): Integer; cdecl;
  pcre_get_substring_list_func = function(const subject: PChar; ovector: PInteger;
    stringcount: Integer; listptr: PPPChar): Integer; cdecl;
  pcre_free_substring_func = procedure(var stringptr: PChar); cdecl;
  pcre_free_substring_list_func = procedure(var stringptr: PChar); cdecl;
  pcre_maketables_func = function: PChar; cdecl;
  pcre_fullinfo_func = function(const code: PPCRE; const extra: PPCREExtra;
    what: Integer; where: Pointer): Integer; cdecl;
  pcre_info_func = function(const code: PPCRE; optptr, firstcharptr: PInteger): Integer; cdecl;
  pcre_version_func = function: PChar; cdecl;

  pcre_malloc_func = function(Size: Integer): Pointer; cdecl;
  pcre_free_func = procedure(P: Pointer); cdecl;
var
  pcre_compile: pcre_compile_func = nil;
  {$EXTERNALSYM pcre_compile}
  pcre_copy_substring: pcre_copy_substring_func = nil;
  {$EXTERNALSYM pcre_copy_substring}
  pcre_exec: pcre_exec_func = nil;
  {$EXTERNALSYM pcre_exec}
  pcre_study: pcre_study_func = nil;
  {$EXTERNALSYM pcre_study}
  pcre_get_substring: pcre_get_substring_func = nil;
  {$EXTERNALSYM pcre_get_substring}
  pcre_get_substring_list: pcre_get_substring_list_func = nil;
  {$EXTERNALSYM pcre_get_substring_list}
  pcre_free_substring: pcre_free_substring_func = nil;
  {$EXTERNALSYM pcre_free_substring}
  pcre_free_substring_list: pcre_free_substring_list_func = nil;
  {$EXTERNALSYM pcre_free_substring_list}
  pcre_maketables: pcre_maketables_func = nil;
  {$EXTERNALSYM pcre_maketables}
  pcre_fullinfo: pcre_fullinfo_func = nil;
  {$EXTERNALSYM pcre_fullinfo}
  pcre_info: pcre_info_func = nil;
  {$EXTERNALSYM pcre_info}
  pcre_version: pcre_version_func = nil;
  {$EXTERNALSYM pcre_version}

  // Don't use! These don't work!!!
  pcre_malloc: pcre_malloc_func = nil;
  {$EXTERNALSYM pcre_malloc}
  pcre_free: pcre_free_func = nil;
  {$EXTERNALSYM pcre_free}

{$ENDIF ~PCRE_LINKONREQUEST}

function IsPCRELoaded: Boolean;
function LoadPCRE: Boolean;
procedure UnloadPCRE;

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  {$IFDEF HAS_UNIT_TYPES}
  Types,
  {$ENDIF HAS_UNIT_TYPES}
  {$IFDEF HAS_UNIT_LIBC}
  Libc;
  {$ELSE ~HAS_UNIT_LIBC}
  dl;
  {$ENDIF ~HAS_UNIT_LIBC}
  {$ENDIF UNIX}

type
  {$IFDEF MSWINDOWS}
  TModuleHandle = HINST;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  TModuleHandle = Pointer;
  {$ENDIF LINUX}

const
  {$IFDEF MSWINDOWS}
  libpcremodulename = 'pcre.dll';
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  libpcremodulename = 'libpcre.so.0';
  INVALID_HANDLE_VALUE = TModuleHandle(0);
  {$ENDIF UNIX}

{$IFDEF PCRE_LINKONREQUEST}
var
  PCRELib: TModuleHandle = INVALID_HANDLE_VALUE;
{$ENDIF PCRE_LINKONREQUEST}

function IsPCRELoaded: Boolean;
begin
  {$IFDEF PCRE_LINKONREQUEST}
  Result := PCRELib <> INVALID_HANDLE_VALUE;
  {$ELSE}
  Result := True;
  {$ENDIF PCRE_LINKONREQUEST}
end;

function LoadPCRE: Boolean;

  function GetSymbol(SymbolName: PChar): Pointer;
  begin
    {$IFDEF MSWINDOWS}
    Result := GetProcAddress(PCRELib, PChar(SymbolName));
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    Result := dlsym(PCRELib, PChar(SymbolName));
    {$ENDIF UNIX}
  end;

begin
  {$IFDEF PCRE_LINKONREQUEST}
  if PCRELib = INVALID_HANDLE_VALUE then
    {$IFDEF MSWINDOWS}
    PCRELib := LoadLibrary(libpcremodulename);
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    PCRELib := dlopen(PChar(libpcremodulename), RTLD_NOW);
    {$ENDIF UNIX}
  Result := PCRELib <> INVALID_HANDLE_VALUE;
  if Result then
  begin
    @pcre_compile := GetSymbol('pcre_compile');
    @pcre_copy_substring := GetSymbol('pcre_copy_substring');
    @pcre_exec := GetSymbol('pcre_exec');
    @pcre_study := GetSymbol('pcre_study');
    @pcre_get_substring := GetSymbol('pcre_get_substring');
    @pcre_get_substring_list := GetSymbol('pcre_get_substring_list');
    @pcre_free_substring := GetSymbol('pcre_free_substring');
    @pcre_free_substring_list := GetSymbol('pcre_free_substring_list');
    @pcre_maketables := GetSymbol('pcre_maketables');
    @pcre_fullinfo := GetSymbol('pcre_fullinfo');
    @pcre_info := GetSymbol('pcre_info');
    @pcre_version := GetSymbol('pcre_version');

    @pcre_malloc := GetSymbol('pcre_malloc');
    @pcre_free := GetSymbol('pcre_free');
  end
  else
    UnloadPCRE;
  {$ELSE}
  Result := True;
  {$ENDIF PCRE_LINKONREQUEST}
end;

procedure UnloadPCRE;
begin
  {$IFDEF PCRE_LINKONREQUEST}
  if PCRELib <> INVALID_HANDLE_VALUE then
    {$IFDEF MSWINDOWS}
    FreeLibrary(PCRELib);
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    dlclose(Pointer(PCRELib));
    {$ENDIF UNIX}
  PCRELib := INVALID_HANDLE_VALUE;
  @pcre_compile := nil;
  @pcre_copy_substring := nil;
  @pcre_exec := nil;
  @pcre_study := nil;
  @pcre_get_substring := nil;
  @pcre_get_substring_list := nil;
  @pcre_free_substring := nil;
  @pcre_free_substring_list := nil;
  @pcre_maketables := nil;
  @pcre_fullinfo := nil;
  @pcre_info := nil;
  @pcre_version := nil;

  @pcre_malloc := nil;
  @pcre_free := nil;
  {$ENDIF PCRE_LINKONREQUEST}
end;

{$IFNDEF PCRE_LINKONREQUEST}
function pcre_compile; external libpcremodulename name 'pcre_compile';
function pcre_copy_substring; external libpcremodulename name 'pcre_copy_substring';
function pcre_exec; external libpcremodulename name 'pcre_exec';
function pcre_study; external libpcremodulename name 'pcre_study';
function pcre_get_substring; external libpcremodulename name 'pcre_get_substring';
function pcre_get_substring_list; external libpcremodulename name 'pcre_get_substring_list';
procedure pcre_free_substring; external libpcremodulename name 'pcre_free_substring';
procedure pcre_free_substring_list; external libpcremodulename name 'pcre_free_substring_list';
function pcre_maketables; external libpcremodulename name 'pcre_maketables';
function pcre_fullinfo; external libpcremodulename name 'pcre_fullinfo';
function pcre_info; external libpcremodulename name 'pcre_info';
function pcre_version; external libpcremodulename name 'pcre_version';
function pcre_malloc; external libpcremodulename name 'pcre_malloc';
procedure pcre_free; external libpcremodulename name 'pcre_free';
{$ENDIF ~PCRE_LINKONREQUEST}

// History

// $Log$
// Revision 1.5  2004/10/02 05:47:28  marquardt
// added check for incompatible jedi.inc
// replaced jedi.inc with jvcl.inc
//
// Revision 1.4  2004/07/27 06:42:23  marquardt
// style cleaning of pcre files
//
// Revision 1.3  2004/07/26 06:01:39  rrossmair
// *** empty log message ***
//
// Revision 1.2  2004/07/26 05:13:52  rrossmair
// made it compile under Kylix (no functional tests performed yet)
//

end.

