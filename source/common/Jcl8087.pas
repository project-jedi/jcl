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
{ The Original Code is Jcl8087.pas.                                            }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{ Last modified: August 16, 2000                                               }
{                                                                              }
{******************************************************************************}

unit Jcl8087;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

type
  T8087Precision = (pcSingle, pcReserved, pcDouble, pcExtended);
  T8087Rounding = (rcNearestOrEven, rcDownInfinity, rcUpInfinity, rcChopOrTruncate);
  T8087Infinity = (icProjective, icAffine);

function Get8087ControlWord: Word;
function Get8087Infinity: T8087Infinity;
function Get8087Precision: T8087Precision;
function Get8087Rounding: T8087Rounding;
function Get8087StatusWord(ClearExceptions: Boolean): Word;
function Set8087Infinity(const Infinity: T8087Infinity): T8087Infinity;
function Set8087Precision(const Precision: T8087Precision): T8087Precision;
function Set8087Rounding(const Rounding: T8087Rounding): T8087Rounding;
function Set8087ControlWord(const Control: Word): Word;

implementation

function Get8087ControlWord: Word; assembler;
var
  CW: Word;
asm
        FSTCW   [CW]
        MOV     AX, [CW]
end;

//------------------------------------------------------------------------------

function Get8087Infinity: T8087Infinity;
begin
  Result := T8087Infinity((Get8087ControlWord and $1000) shr 12);
end;

//------------------------------------------------------------------------------

function Get8087Precision: T8087Precision;
begin
  Result := T8087Precision((Get8087ControlWord and $0300) shr 8);
end;

//------------------------------------------------------------------------------

function Get8087Rounding: T8087Rounding;
begin
  Result := T8087Rounding((Get8087ControlWord and $0C00) shr 10);
end;

//------------------------------------------------------------------------------

function Get8087StatusWord(ClearExceptions: Boolean): Word; assembler;
asm
        TEST    AX, AX                // if ClearExceptions then
        JE      @@NOCLEAREXCEPTIONS
        FSTSW   AX                    //   get status word (clears exceptions)
        RET
@@NOCLEAREXCEPTIONS:                  // else
        FNSTSW  AX                    //   get status word (without clearing exceptions)
end;

//------------------------------------------------------------------------------

function Set8087Infinity(const Infinity: T8087Infinity): T8087Infinity;
var
  CW: Word;
begin
  CW := Get8087ControlWord;
  Result := T8087Infinity((CW and $1000) shr 12);
  Set8087ControlWord((CW and $EFFF) or (Word(Infinity) shl 12));
end;

//------------------------------------------------------------------------------

function Set8087Precision(const Precision: T8087Precision): T8087Precision;
var
  CW: Word;
begin
  CW := Get8087ControlWord;
  Result := T8087Precision((CW and $0300) shr 8);
  Set8087ControlWord((CW and $FCFF) or (Word(Precision) shl 8));
end;

//------------------------------------------------------------------------------

function Set8087Rounding(const Rounding: T8087Rounding): T8087Rounding;
var
  CW: Word;
begin
  CW := Get8087ControlWord;
  Result := T8087Rounding((CW and $0C00) shr 10);
  Set8087ControlWord((CW and $F3FF) or (Word(Rounding) shl 10));
end;

//------------------------------------------------------------------------------

function Set8087ControlWord(const Control: Word): Word; assembler;
var
  CW: Word;
asm
        FSTCW   [CW]         // get current control word
        MOV     CX, [CW]
        MOV     [CW], AX
        FNCLEX               // supress exceptions due to change in flags
        FLDCW   [CW]         // set control word
        MOV     AX, CX       // return old control word
end;

end.
