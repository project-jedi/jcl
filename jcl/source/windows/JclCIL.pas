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
{ The Original Code is JclClr.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Microsoft .Net CIL Instruction Set information support routines and classes.                     }
{                                                                                                  }
{ Unit owner: Flier Lu                                                                             }
{ Last modified: December 8, 2002                                                                  }
{                                                                                                  }
{**************************************************************************************************}

unit JclCIL;

interface

{$I jcl.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Classes, SysUtils,
  {$IFDEF COMPILER5_UP}
  Contnrs,
  {$ENDIF COMPILER5_UP}
  JclBase, JclSysUtils, JclClr, JclMetadata;

type
  TJclOpCode = ( opNop, opBreak,
                 opLdArg_0, opLdArg_1, opLdArg_2, opLdArg_3,
                 opLdLoc_0, opLdLoc_1, opLdLoc_2, opLdLoc_3,
                 opStLoc_0, opStLoc_1, opStLoc_2, opStLoc_3,
                 opldArg_s, opLdArga_s, opStArg_s,
                 opLdLoc_s, opLdLoca_s, opStLoc_s,
                 opLdNull, opLdc_I4_M1,
                 opLdc_I4_0, opLdc_I4_1, opLdc_I4_2, opLdc_I4_3, opLdc_I4_4,
                 opLdc_I4_5, opLdc_I4_6, opLdc_I4_7, opLdc_I4_8, opLdc_I4_s,
                 opLdc_i4, opLdc_i8, opLdc_r4, opLdc_r8,
                 opUnused49,
                 opDup, opPop, opJmp, opCall, opCalli, opRet,
                 opBr_s, opBrFalse_s, opBrTrue_s,
                 opBeq_s, opBge_s, opBgt_s, opBle_s, opBlt_s,
                 opBne_un_s, opBge_un_s, opBgt_un_s, opBle_un_s, opBlt_un_s,
                 opBr, opBrFalse, opBrTrue,
                 opBeq, opBge, opBgt, opBle, opBlt,
                 opBne_un, opBge_un, opBgt_un, opBle_un, opBlt_un,
                 opSwitch,
                 opLdInd_i1, opLdInd_i2, opLdInd_u1, opLdInd_u2,
                 opLdInd_i4, opLdInd_u4, opLdInd_i8, opLdInd_i,
                 opLdInd_r4, opLdInd_r8, opLdInd_ref, opStInd_ref,
                 opStInd_i1, opStInd_i2, opStInd_i4, opStInd_i8,
                 opStInd_r4, opStInd_r8,
                 opAdd, opSub, opMul, opDiv, opDiv_un, opRem, opRem_un,
                 opAnd, opOr, opXor, opShl, opShr, opShr_un, opNeg, opNot,
                 opConv_i1, opConv_i2, opConv_i4, opConv_i8,
                 opConv_r4, opConv_r8, opConv_u4, opConv_u8,
                 opCallVirt, opCpObj, opLdObj, opLdStr, opNewObj,
                 opCastClass, opIsInst, opConv_r_un,
                 opUnused58, opUnused1,
                 opUnbox, opThrow,
                 opLdFld, opLdFlda, opStFld, opLdsFld, opLdsFlda, opStsFld, opStObj,
                 opConv_ovf_i1_un, opConv_ovf_i2_un, opConv_ovf_i4_un, opConv_ovf_i8_un,
                 opConv_ovf_u1_un, opConv_ovf_u2_un, opConv_ovf_u4_un, opConv_ovf_u8_un,
                 opConv_ovf_i_un, opConv_ovf_u_un,
                 opBox, opNewArr, opLdLen,
                 opLdElema, opLdElem_i1, opLdElem_u1, opLdElem_i2, opLdElem_u2,
                 opLdElem_i4, opLdElem_u4, opLdElem_i8, opLdElem_i,
                 opLdElem_r4, opLdElem_r8, opLdElem_ref,
                 opStElem_i, opStElem_i1, opStElem_i2, opStElem_i4, opStElem_i8,
                 opStElem_r4, opStElem_r8, opStElem_ref,
                 opUnused2, opUnused3, opUnused4, opUnused5,
                 opUnused6, opUnused7, opUnused8, opUnused9,
                 opUnused10, opUnused11, opUnused12, opUnused13,
                 opUnused14, opUnused15, opUnused16, opUnused17,
                 opConv_ovf_i1, opConv_ovf_u1, opConv_ovf_i2, opConv_ovf_u2,
                 opConv_ovf_i4, opConv_ovf_u4, opConv_ovf_i8, opConv_ovf_u8,
                 opUnused50, opUnused18, opUnused19, opUnused20,
                 opUnused21, opUnused22, opUnused23,
                 opRefAnyVal, opCkFinite,
                 opUnused24, opUnused25,
                 opMkRefAny,
                 opUnused59, opUnused60, opUnused61, opUnused62, opUnused63,
                 opUnused64, opUnused65, opUnused66, opUnused67,
                 opLdToken,
                 opConv_u2, opConv_u1, opConv_i, opConv_ovf_i, opConv_ovf_u,
                 opAdd_ovf, opAdd_ovf_un, opMul_ovf, opMul_ovf_un, opSub_ovf, opSub_ovf_un,
                 opEndFinally, opLeave, opLeave_s, opStInd_i, opConv_u,
                 opUnused26, opUnused27, opUnused28, opUnused29, opUnused30,
                 opUnused31, opUnused32, opUnused33, opUnused34, opUnused35,
                 opUnused36, opUnused37, opUnused38, opUnused39, opUnused40,
                 opUnused41, opUnused42, opUnused43, opUnused44, opUnused45,
                 opUnused46, opUnused47, opUnused48,
                 opPrefix7, opPrefix6, opPrefix5, opPrefix4,
                 opPrefix3, opPrefix2, opPrefix1, opPrefixRef,

                 opArgLlist, opCeq, opCgt, opCgt_un, opClt, opClt_un,
                 opLdFtn, opLdVirtFtn, optUnused56,
                 opLdArg, opLdArga, opStArg, opLdLoc, opLdLoca, opStLoc,
                 opLocalLoc, opUnused57, opEndFilter, opUnaligned, opVolatile,
                 opTail, opInitObj, opUnused68, opCpBlk, opInitBlk, opUnused69,
                 opRethrow, opUnused51, opSizeOf, opRefAnyType,
                 opUnused52, opUnused53, opUnused54, opUnused55, opUnused70);

  TJclInstructionDumpILOption = (doLineNo, doRawBytes, doIL, doTokenValue, doComment);
  TJclInstructionDumpILOptions = set of TJclInstructionDumpILOption;

  TJclInstructionParamType = (ptVoid, ptI1, ptI2, ptI4, ptI8, ptU1, ptU2, ptU4, ptU8, ptR4, ptR8,
                              ptToken, ptSOff, ptLOff, ptArray);

const
  InstructionDumpILAllOption = [doLineNo, doRawBytes, doIL, doTokenValue, doComment];

type
  TJclClrILGenerator = class;

  TJclInstruction = class
  private
    FOpCode: TJclOpCode;
    FOffset: DWord;
    FParam: Variant;
    FOwner: TJclClrILGenerator;

    function GetWideOpCode: Boolean;
    function GetRealOpCode: Byte;

    function GetName: string;
    function GetFullName: string;
    function GetDescription: string;
    function GetParamType: TJclInstructionParamType;

    function FormatLabel(Offset: Integer): string;
  protected
    function GetSize: DWord; virtual;

    function DumpILOption(Option: TJclInstructionDumpILOption): string; virtual;
  public
    constructor Create(AOwner :TJclClrILGenerator; AOpCode: TJclOpCode);

    procedure Load(Stream: TStream); virtual;
    procedure Save(Stream: TStream); virtual;

    function DumpIL(Options: TJclInstructionDumpILOptions = [doIL]): string;

    property Owner :TJclClrILGenerator read FOwner;

    property OpCode: TJclOpCode read FOpCode;
    property WideOpCode: Boolean read GetWideOpCode;
    property RealOpCode: Byte read GetRealOpCode;

    property Param: Variant read FParam write FParam;
    property ParamType: TJclInstructionParamType read GetParamType;

    property Name: string read GetName;
    property FullName: string read GetFullName;
    property Description: string read GetDescription;

    property Size: DWord read GetSize;

    property Offset: DWord read FOffset;
  end;

  TJclUnaryInstruction = class(TJclInstruction)
  end;

  TJclBinaryInstruction = class(TJclInstruction)
  end;

  TJclClrILGenerator = class
  private
    FMethod: TJclClrMethodBody;
    FInstructions: TObjectList;

    function GetInstructionCount: Integer;
    function GetInstruction(const Idx: Integer): TJclInstruction;
  public
    constructor Create; overload;
    constructor Create(AMethod :TJclClrMethodBody); overload;
    destructor Destroy; override;

    function DumpIL(Options: TJclInstructionDumpILOptions): string;

    property Method: TJclClrMethodBody read FMethod;

    property Instructions[const Idx: Integer]: TJclInstruction read GetInstruction;
    property InstructionCount: Integer read GetInstructionCount;
  end;

  EJclCliInstructionError = class(EJclError);
  EJclCliInstructionStreamInvalid = class(EJclCliInstructionError);

implementation

uses
  Variants, StrUtils;

{ TODO : Move resourcestring to JclResources }
resourcestring
  RsInstructionStreamInvalid = 'Invalid IL instruction stream';

type
  TJclOpCodeInfoType = (itName, itFullName, itDescription);

const
  STP1 = $FE;

  OpCodeInfos: array[TJclOpCode, TJclOpCodeInfoType] of string = (
  ('nop',            'no operation', 'Do nothing'),
  ('break',          'breakpoint instruction', 'inform a debugger that a breakpoint has been reached.'),
  ('ldarg.0',        'load argument onto the stack', 'Load argument 0 onto stack'),
  ('ldarg.1',        'load argument onto the stack', 'Load argument 1 onto stack'),
  ('ldarg.2',        'load argument onto the stack', 'Load argument 2 onto stack'),
  ('ldarg.3',        'load argument onto the stack', 'Load argument 3 onto stack'),
  ('ldloc.0',        'load local variable onto the stack', 'Load local variable 0 onto stack.'),
  ('ldloc.1',        'load local variable onto the stack', 'Load local variable 1 onto stack.'),
  ('ldloc.2',        'load local variable onto the stack', 'Load local variable 2 onto stack.'),
  ('ldloc.3',        'load local variable onto the stack', 'Load local variable 3 onto stack.'),
  ('stloc.0',        'pop value from stack to local variable', 'Pop value from stack into local variable 0.'),
  ('stloc.1',        'pop value from stack to local variable', 'Pop value from stack into local variable 1.'),
  ('stloc.2',        'pop value from stack to local variable', 'Pop value from stack into local variable 2.'),
  ('stloc.3',        'pop value from stack to local variable', 'Pop value from stack into local variable 3.'),
  ('ldarg.s',        'load argument onto the stack', 'Load argument numbered num onto stack, short form.'),
  ('ldarga.s',       'load an argument address', 'fetch the address of argument argNum, short form'),
  ('starg.s',        'store a value in an argument slot', 'Store a value to the argument numbered num, short form'),
  ('ldloc.s',        'load local variable onto the stack', 'Load local variable of index indx onto stack, short form.'),
  ('ldloca.s',       'load local variable address', 'Load address of local variable with index indx, short form'),
  ('stloc.s',        'pop value from stack to local variable', 'Pop value from stack into local variable indx, short form.'),
  ('ldnull',         'load a null pointer', 'Push null reference on the stack'),
  ('ldc.i4.m1',      'load numeric constant', 'Push -1 onto the stack as int32.'),
  ('ldc.i4.0',       'load numeric constant', 'Push 0 onto the stack as int32.'),
  ('ldc.i4.1',       'load numeric constant', 'Push 1 onto the stack as int32.'),
  ('ldc.i4.2',       'load numeric constant', 'Push 2 onto the stack as int32.'),
  ('ldc.i4.3',       'load numeric constant', 'Push 3 onto the stack as int32.'),
  ('ldc.i4.4',       'load numeric constant', 'Push 4 onto the stack as int32.'),
  ('ldc.i4.5',       'load numeric constant', 'Push 5 onto the stack as int32.'),
  ('ldc.i4.6',       'load numeric constant', 'Push 6 onto the stack as int32.'),
  ('ldc.i4.7',       'load numeric constant', 'Push 7 onto the stack as int32.'),
  ('ldc.i4.8',       'load numeric constant', 'Push 8 onto the stack as int32.'),
  ('ldc.i4.s',       'load numeric constant', 'Push num onto the stack as int32, short form.'),
  ('ldc.i4',         'load numeric constant', 'Push num of type int32 onto the stack as int32.'),
  ('ldc.i8',         'load numeric constant', 'Push num of type int64 onto the stack as int64.'),
  ('ldc.r4',         'load numeric constant', 'Push num of type float32 onto the stack as F.'),
  ('ldc.r8',         'load numeric constant', 'Push num of type float64 onto the stack as F.'),
  ('unused',         '', ''),
  ('dup',            'duplicate the top value of the stack', 'duplicate value on the top of the stack'),
  ('pop',            'remove the top element of the stack', 'pop a value from the stack'),
  ('jmp',            'jump to method', 'Exit current method and jump to specified method'),
  ('call',           'call a method', 'Call method described by method'),
  ('calli',          'indirect method call', 'Call method indicated on the stack with arguments described by callsitedescr.'),
  ('ret',            'return from method', 'Return from method, possibly returning a value'),
  ('br.s',           'unconditional branch', 'branch to target, short form'),
  ('brfalse.s',      'branch on false, null, or zero', 'branch to target if value is zero (false), short form'),
  ('brtrue.s',       'branch on non-false or non-null', 'branch to target if value is non-zero (true), short form'),
  ('beq.s',          'branch on equal', 'branch to target if equal, short form'),
  ('bge.s',          'branch on greater than or equal to', 'branch to target if greater than or equal to, short form'),
  ('bgt.s',          'branch on greater than', 'branch to target if greater than, short form'),
  ('ble.s',          'branch on less than or equal to', 'branch to target if less than or equal to, short form'),
  ('blt.s',          'branch on less than', 'branch to target if less than'),
  ('bne.un.s',       'branch on not equal or unordered', 'branch to target if unequal or unordered, short form'),
  ('bge.un.s',       'branch on greater than or equal to, unsigned or unordered', 'branch to target if greater than or equal to (unsigned or unordered), short form'),
  ('bgt.un.s',       'branch on greater than, unsigned or unordered', 'branch to target if greater than (unsigned or unordered), short form'),
  ('ble.un.s',       'branch on less than or equal to, unsigned or unordered', 'branch to target if less than or equal to (unsigned or unordered), short form'),
  ('blt.un.s',       'branch on less than, unsigned or unordered', 'Branch to target if less than (unsigned or unordered), short form'),
  ('br',             'unconditional branch', 'branch to target '),
  ('brfalse',        'branch on false, null, or zero', 'branch to target if value is zero (false)'),
  ('brtrue',         'branch on non-false or non-null', 'branch to target if value is non-zero (true)'),
  ('beq',            'branch on equal', 'branch to target if equal'),
  ('bge',            'branch on greater than or equal to', 'branch to target if greater than or equal to'),
  ('bgt',            'branch on greater than', 'branch to target if greater than'),
  ('ble',            'branch on less than or equal to', 'branch to target if less than or equal to'),
  ('blt',            'branch on less than', 'branch to target if less than'),
  ('bne.un',         'branch on not equal or unordered', 'branch to target if unequal or unordered'),
  ('bge.un',         'branch on greater than or equal to, unsigned or unordered', 'branch to target if greater than or equal to (unsigned or unordered)'),
  ('bgt.un',         'branch on greater than, unsigned or unordered', 'branch to target if greater than (unsigned or unordered)'),
  ('ble.un',         'branch on less than or equal to, unsigned or unordered', 'branch to target if less than or equal to (unsigned or unordered)'),
  ('blt.un',         'branch on less than, unsigned or unordered', 'Branch to target if less than (unsigned or unordered) '),
  ('switch',         'table switch on value', 'jump to one of n values'),
  ('ldind.i1',       'load value indirect onto the stack', 'Indirect load value of type int8 as int32 on the stack.'),
  ('ldind.u1',       'load value indirect onto the stack', 'Indirect load value of type unsigned int8 as int32 on the stack.'),
  ('ldind.i2',       'load value indirect onto the stack', 'Indirect load value of type int16 as int32 on the stack.'),
  ('ldind.u2',       'load value indirect onto the stack', 'Indirect load value of type unsigned int16 as int32 on the stack.'),
  ('ldind.i4',       'load value indirect onto the stack', 'Indirect load value of type int32 as int32 on the stack.'),
  ('ldind.u4',       'load value indirect onto the stack', 'Indirect load value of type unsigned int32 as int32 on the stack.'),
  ('ldind.i8',       'load value indirect onto the stack', 'Indirect load value of type int64 as int64 on the stack.'),
  ('ldind.i',        'load value indirect onto the stack', 'Indirect load value of type native int as native int on the stack'),
  ('ldind.r4',       'load value indirect onto the stack', 'Indirect load value of type float32 as F on the stack.'),
  ('ldind.r8',       'load value indirect onto the stack', 'Indirect load value of type float64 as F on the stack.'),
  ('ldind.ref',      'load value indirect onto the stack', 'Indirect load value of type object ref as O on the stack.'),
  ('stind.ref',      'store value indirect from stack', 'Store value of type object ref (type O) into memory at address'),
  ('stind.i1',       'store value indirect from stack', 'Store value of type int8 into memory at address'),
  ('stind.i2',       'store value indirect from stack', 'Store value of type int16 into memory at address'),
  ('stind.i4',       'store value indirect from stack', 'Store value of type int32 into memory at address'),
  ('stind.i8',       'store value indirect from stack', 'Store value of type int64 into memory at address'),
  ('stind.r4',       'store value indirect from stack', 'Store value of type float32 into memory at address'),
  ('stind.r8',       'store value indirect from stack', 'Store value of type float64 into memory at address'),
  ('add',            'add numeric values', 'Add two values, returning a new value'),
  ('sub',            'subtract numeric values', 'Subtract value2 from value1, returning a new value'),
  ('mul',            'multiply values', 'Multiply values'),
  ('div',            'divide values', 'Divide two values to return a quotient or floating-point result'),
  ('div.un',         'divide integer values, unsigned', 'Divide two values, unsigned, returning a quotient'),
  ('rem',            'compute remainder', 'Remainder of dividing value1 by value2'),
  ('rem.un',         'compute integer remainder, unsigned', 'Remainder of unsigned dividing value1 by value2'),
  ('and',            'bitwise AND', 'Bitwise AND of two integral values, returns an integral value'),
  ('or',             'bitwise OR', 'Bitwise OR of two integer values, returns an integer.'),
  ('xor',            'bitwise XOR', 'Bitwise XOR of integer values, returns an integer'),
  ('shl',            'shift integer left', 'Shift an integer to the left (shifting in zeros)'),
  ('shr',            'shift integer right', 'Shift an integer right, (shift in sign), return an integer'),
  ('shr.un',         'shift integer right, unsigned', 'Shift an integer right, (shift in zero), return an integer'),
  ('neg',            'negate', 'Negate value'),
  ('not',            'bitwise complement', 'Bitwise complement'),
  ('conv.i1',        'data conversion', 'Convert to int8, pushing int32 on stack'),
  ('conv.i2',        'data conversion', 'Convert to int16, pushing int32 on stack'),
  ('conv.i4',        'data conversion', 'Convert to int32, pushing int32 on stack'),
  ('conv.i8',        'data conversion', 'Convert to int64, pushing int64 on stack'),
  ('conv.r4',        'data conversion', 'Convert to float32, pushing F on stack'),
  ('conv.r8',        'data conversion', 'Convert to float64, pushing F on stack'),
  ('conv.u4',        'data conversion', 'Convert to unsigned int32, pushing int32 on stack'),
  ('conv.u8',        'data conversion', 'Convert to unsigned int64, pushing int64 on stack'),
  ('callvirt',       'call a method associated, at runtime, with an object', 'Call a method associated with obj'),
  ('cpobj',          'copy a value type', 'Copy a value type from srcValObj to destValObj'),
  ('ldobj',          'copy value type to the stack', 'Copy instance of value type classTok to the stack.'),
  ('ldstr',          'load a literal string', 'push a string object for the literal string '),
  ('newobj',         'create a new object', 'allocate an uninitialized object or value type and call ctor '),
  ('castclass',      'cast an object to a class', 'Cast obj to class'),
  ('isinst',         'test if an object is an instance of a class or interface', 'test if obj is an instance of class, returning NULL or an instance of that class or interface'),
  ('conv.r.un',      'data conversion', 'Convert unsigned integer to floating-point, pushing F on stack'),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unbox',          'Convert boxed value type to its raw form', 'Extract the value type data from obj, its boxed representation'),
  ('throw',          'throw an exception', 'Throw an exception'),
  ('ldfld',          'load field of an object', 'Push the value of field of object, or value type, obj, onto the stack'),
  ('ldflda',         'load field address', 'Push the address of field of object obj on the stack'),
  ('stfld',          'store into a field of an object', 'Replace the value of field of the object obj with val'),
  ('ldsfld',         'load static field of a class', 'Push the value of field on the stack'),
  ('ldsflda',        'load static field address', 'Push the address of the static field, field, on the stack'),
  ('stsfld',         'store a static field of a class', 'Replace the value of field with val'),
  ('stobj',          'store a value type from the stack into memory', 'Store a value of type classTok from the stack into memory'),
  ('conv.ovf.i1.un', 'unsigned data conversion with overflow detection', 'Convert unsigned to an int8 (on the stack as int32) and throw an exception on overflow'),
  ('conv.ovf.i2.un', 'unsigned data conversion with overflow detection', 'Convert unsigned to an int16 (on the stack as int32) and throw an exception on overflow'),
  ('conv.ovf.i4.un', 'unsigned data conversion with overflow detection', 'Convert unsigned to an int32 (on the stack as int32) and throw an exception on overflow'),
  ('conv.ovf.i8.un', 'unsigned data conversion with overflow detection', 'Convert unsigned to an int64 (on the stack as int64) and throw an exception on overflow'),
  ('conv.ovf.u1.un', 'unsigned data conversion with overflow detection', 'Convert unsigned to an unsigned int8 (on the stack as int32) and throw an exception on overflow'),
  ('conv.ovf.u2.un', 'unsigned data conversion with overflow detection', 'Convert unsigned to an unsigned int16 (on the stack as int32) and throw an exception on overflow'),
  ('conv.ovf.u4.un', 'unsigned data conversion with overflow detection', 'Convert unsigned to an unsigned int32 (on the stack as int32) and throw an exception on overflow'),
  ('conv.ovf.u8.un', 'unsigned data conversion with overflow detection', 'Convert unsigned to an unsigned int64 (on the stack as int64) and throw an exception on overflow'),
  ('conv.ovf.i.un',  'unsigned data conversion with overflow detection', 'Convert unsigned to a native int (on the stack as native int) and throw an exception on overflow'),
  ('conv.ovf.u.un',  'unsigned data conversion with overflow detection', 'Convert unsigned to a native unsigned  int (on the stack as native int) and throw an exception on overflow'),
  ('box',            'convert value type to object reference', 'Convert valueType to a true object reference'),
  ('newarr',         'create a zero-based, one-dimensional array', 'create a new array with elements of type etype'),
  ('ldlen',          'load the length of an array', 'push the length (of type native unsigned int) of array on the stack'),
  ('ldelema',        'load address of an element of an array', 'Load the address of element at index onto the top of the stack'),
  ('ldelem.i1',      'load an element of an array', 'Load the element with type int8 at index onto the top of the stack as an int32'),
  ('ldelem.u1',      'load an element of an array', 'Load the element with type unsigned int8 at index onto the top of the stack as an int32'),
  ('ldelem.i2',      'load an element of an array', 'Load the element with type int16 at index onto the top of the stack as an int32'),
  ('ldelem.u2',      'load an element of an array', 'Load the element with type unsigned int16 at index onto the top of the stack as an int32'),
  ('ldelem.i4',      'load an element of an array', 'Load the element with type int32 at index onto the top of the stack as an int32'),
  ('ldelem.u4',      'load an element of an array', 'Load the element with type unsigned int32 at index onto the top of the stack as an int32 (alias for ldelem.i4)'),
  ('ldelem.i8',      'load an element of an array', 'Load the element with type int64 at index onto the top of the stack as an int64'),
  ('ldelem.i',       'load an element of an array', 'Load the element with type native int at index onto the top of the stack as an native int'),
  ('ldelem.r4',      'load an element of an array', 'Load the element with type float32 at index onto the top of the stack as an F'),
  ('ldelem.r8',      'load an element of an array', 'Load the element with type float64 at index onto the top of the stack as an F'),
  ('ldelem.ref',     'load an element of an array', 'Load the element of type object, at index onto the top of the stack as an O'),
  ('stelem.i',       'store an element of an array', 'Replace array element at index with the i value on the stack'),
  ('stelem.i1',      'store an element of an array', 'Replace array element at index with the int8 value on the stack'),
  ('stelem.i2',      'store an element of an array', 'Replace array element at index with the int16 value on the stack'),
  ('stelem.i4',      'store an element of an array', 'Replace array element at index with the int32 value on the stack'),
  ('stelem.i8',      'store an element of an array', 'Replace array element at index with the int64 value on the stack'),
  ('stelem.r4',      'store an element of an array', 'Replace array element at index with the float32 value on the stack'),
  ('stelem.r8',      'store an element of an array', 'Replace array element at index with the float64 value on the stack'),
  ('stelem.ref',     'store an element of an array', 'Replace array element at index with the ref value on the stack'),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('conv.ovf.i1',    'data conversion with overflow detection', 'Convert to an int8 (on the stack as int32) and throw an exception on overflow '),
  ('conv.ovf.u1',    'data conversion with overflow detection', 'Convert to a unsigned int8 (on the stack as int32) and throw an exception on overflow '),
  ('conv.ovf.i2',    'data conversion with overflow detection', 'Convert to an int16 (on the stack as int32) and throw an exception on overflow '),
  ('conv.ovf.u2',    'data conversion with overflow detection', 'Convert to a unsigned int16 (on the stack as int32) and throw an exception on overflow '),
  ('conv.ovf.i4',    'data conversion with overflow detection', 'Convert to an int32 (on the stack as int32) and throw an exception on overflow '),
  ('conv.ovf.u4',    'data conversion with overflow detection', 'Convert to a unsigned int32 (on the stack as int32) and throw an exception on overflow '),
  ('conv.ovf.i8',    'data conversion with overflow detection', 'Convert to an int64 (on the stack as int64) and throw an exception on overflow '),
  ('conv.ovf.u8',    'data conversion with overflow detection', 'Convert to a unsigned int64 (on the stack as int64) and throw an exception on overflow '),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('refanyval',      'load the address out of a typed reference', 'Push the address stored in a typed reference'),
  ('ckfinite',       'check for a finite real number', 'throw ArithmeticException if value is not a finite number'),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('mkrefany',       'push a typed reference on the stack', 'push a typed reference to ptr of type class onto the stack'),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('ldtoken',        'load the runtime representation of a metadata token', 'Convert metadata token to its runtime representation'),
  ('conv.u2',        'data conversion', 'Convert to unsigned int16, pushing int32 on stack'),
  ('conv.u1',        'data conversion', 'Convert to unsigned int8, pushing int32 on stack'),
  ('conv.i',         'data conversion', 'Convert to native int, pushing native int on stack'),
  ('conv.ovf.i',     'data conversion with overflow detection', 'Convert to an native int (on the stack as native int) and throw an exception on overflow'),
  ('conv.ovf.u',     'data conversion with overflow detection', 'Convert to a native unsigned  int (on the stack as native int) and throw an exception on overflow'),
  ('add.ovf',        'add integer values with overflow check', 'Add signed integer values with overflow check. '),
  ('add.ovf.un',     'add integer values with overflow check', 'Add unsigned integer values with overflow check.'),
  ('mul.ovf',        'multiply integer values with overflow check', 'Multiply signed integer values. Signed result must fit in same size'),
  ('mul.ovf.un',     'multiply integer values with overflow check', 'Multiply unsigned integer values. Unsigned result must fit in same size'),
  ('sub.ovf',        'subtract integer values, checking for overflow', 'Subtract native int from an native int. Signed result must fit in same size'),
  ('sub.ovf.un',     'subtract integer values, checking for overflow', 'Subtract native unsigned int from a native unsigned int. Unsigned result must fit in same size'),
  ('endfinally',     'end the finally or fault clause of an exception block', 'End finally clause of an exception block'),
  ('leave',          'exit a protected region of code', 'Exit a protected region of code.'),
  ('leave.s',        'exit a protected region of code', 'Exit a protected region of code, short form'),
  ('stind.i',        'store value indirect from stack', 'Store value of type native int into memory at address'),
  ('conv.u',         'data conversion', 'Convert to native unsigned int, pushing native int on stack'),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('prefix7',        '', ''),
  ('prefix6',        '', ''),
  ('prefix5',        '', ''),
  ('prefix4',        '', ''),
  ('prefix3',        '', ''),
  ('prefix2',        '', ''),
  ('prefix1',        '', ''),
  ('prefixref',      '', ''),

  ('arglist',        'get argument list', 'return argument list handle for the current method '),
  ('ceq',            'compare equal', 'push 1 (of type int32) if value1 equals value2, else 0'),
  ('cgt',            'compare greater than', 'push 1 (of type int32) if value1 > value2, else 0'),
  ('cgt.un',         'compare greater than, unsigned or unordered', 'push 1 (of type int32) if value1 > value2, unsigned or unordered, else 0'),
  ('clt',            'compare less than', 'push 1 (of type int32) if value1 < value2, else 0'),
  ('clt.un',         'compare less than, unsigned or unordered', 'push 1 (of type int32) if value1 < value2, unsigned or unordered, else 0'),
  ('ldftn',          'load method pointer', 'Push a pointer to a method referenced by method on the stack'),
  ('ldvirtftn',      'load a virtual method pointer', 'Push address of virtual method mthd on the stack'),
  ('unused',         '', ''),
  ('ldarg',          'load argument onto the stack', 'Load argument numbered num onto stack.'),
  ('ldarga',         'load an argument address', 'fetch the address of argument argNum.'),
  ('starg',          'store a value in an argument slot', 'Store a value to the argument numbered num'),
  ('ldloc',          'load local variable onto the stack', 'Load local variable of index indx onto stack.'),
  ('ldloca',         'load local variable address', 'Load address of local variable with index indx'),
  ('stloc',          'pop value from stack to local variable', 'Pop value from stack into local variable indx.'),
  ('localloc',       'allocate space in the local dynamic memory pool', 'Allocate space from the local memory pool.'),
  ('unused',         '', ''),
  ('endfilter',      'end filter clause of SEH', 'End filter clause of SEH exception handling'),
  ('unaligned.',     'pointer instruction may be unaligned', 'Subsequent pointer instruction may be unaligned'),
  ('volatile.',      'pointer reference is volatile', 'Subsequent pointer reference is volatile'),
  ('tail.',          'call terminates current method', 'Subsequent call terminates current method'),
  ('initobj',        'initialize a value type', 'Initialize a value type'),
  ('unused',         '', ''),
  ('cpblk',          'copy data from memory to memory', 'Copy data from memory to memory'),
  ('initblk',        'initialize a block of memory to a value', 'Set a block of memory to a given byte'),
  ('unused',         '', ''),
  ('rethrow',        'rethrow the current exception', 'Rethrow the current exception'),
  ('unused',         '', ''),
  ('sizeof',         'load the size in bytes of a value type', 'Push the size, in bytes, of a value type as a unsigned int32'),
  ('refanytype',     'load the type out of a typed reference', 'Push the type token stored in a typed reference'),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''),
  ('unused',         '', ''));

  OpCodeParamTypes: array[TJclOpCode] of TJclInstructionParamType =
  {00}(ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,
  {08} ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptU1,     ptU1,
  {10} ptU1,     ptU1,     ptU1,     ptU1,     ptVoid,   ptVoid,   ptVoid,   ptVoid,
  {18} ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptI1,
  {20} ptI4,     ptI8,     ptR4,     ptR8,     ptVoid,   ptVoid,   ptVoid,   ptToken,
  {28} ptToken,  ptVoid,   ptVoid,   ptSOff,   ptSOff,   ptSOff,   ptSOff,   ptSOff,
  {30} ptSOff,   ptSOff,   ptSOff,   ptSOff,   ptSOff,   ptSOff,   ptSOff,   ptSOff,
  {38} ptLOff,   ptLOff,   ptLOff,   ptLOff,   ptLOff,   ptLOff,   ptLOff,   ptLOff,
  {40} ptLOff,   ptLOff,   ptLOff,   ptLOff,   ptLOff,   ptVoid,   ptVoid,   ptVoid,
  {48} ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,
  {50} ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,
  {58} ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,
  {60} ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,
  {68} ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptToken,
  {70} ptToken,  ptToken,  ptToken,  ptToken,  ptToken,  ptToken,  ptVoid,   ptVoid,
  {78} ptVoid,   ptToken,  ptToken,  ptToken,  ptToken,  ptToken,  ptToken,  ptToken,
  {80} ptToken,  ptToken,  ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,
  {88} ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptToken,  ptToken,  ptVoid,   ptToken,
  {90} ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,
  {98} ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,
  {A0} ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,
  {A8} ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,
  {B0} ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,
  {B8} ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,
  {C0} ptVoid,   ptVoid,   ptToken,  ptVoid,   ptVoid,   ptVoid,   ptToken,   ptVoid,
  {C8} ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,
  {D0} ptToken,  ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,
  {D8} ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptI4,     ptI1,     ptVoid,
  {E0} ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,
  {E8} ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,
  {F0} ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,
  {F8} ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,
  {00} ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptToken,  ptToken,
  {08} ptVoid,   ptU2,     ptU2,     ptU2,     ptU2,     ptU2,     ptU2,     ptVoid,
  {10} ptVoid,   ptVoid,   ptI1,     ptVoid,   ptVoid,   ptToken,  ptVoid,   ptVoid,
  {18} ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptToken,  ptVoid,   ptVoid,   ptVoid,
  {20} ptVoid,   ptVoid,   ptVoid);

{ TJclClrILGenerator }

constructor TJclClrILGenerator.Create;
begin
  inherited;

  FMethod       := nil;
  FInstructions := TObjectList.Create;
end;

constructor TJclClrILGenerator.Create(AMethod: TJclClrMethodBody);
var
  OpCode: Byte;
  Stream: TMemoryStream;
  Instruction: TJclInstruction;
begin
  inherited Create;

  FMethod       := AMethod;
  FInstructions := TObjectList.Create;

  if Assigned(Method) then
  begin
    Stream      := TMemoryStream.Create;
    try
      Stream.Write(Method.Code^, Method.Size);
      Stream.Seek(0, soFromBeginning);
    
      while Stream.Position < Stream.Size do
      begin
        OpCode := PByte(DWORD(Stream.Memory) + Stream.Position)^;
        if OpCode = STP1 then
        begin
          OpCode := PByte(DWORD(Stream.Memory) + Stream.Position + 1)^;
          Instruction := TJclInstruction.Create(Self, TJclOpCode(MaxByte + 1 + OpCode));
        end
        else
          Instruction := TJclInstruction.Create(Self, TJclOpCode(OpCode));

        if Assigned(Instruction) then
        begin
          FInstructions.Add(Instruction);

          Instruction.Load(Stream);
        end;
      end;
    finally
      FreeAndNil(Stream);
    end;
  end;
end;

destructor TJclClrILGenerator.Destroy;
begin
  FreeAndNil(FInstructions);

  inherited;
end;

function TJclClrILGenerator.DumpIL(Options: TJclInstructionDumpILOptions): string;
  function FlagsToName(Flags: TJclClrExceptionClauseFlags): string;
  begin
    if cfFinally in Flags then
      Result := 'finally'
    else if cfFilter in Flags then
      Result := 'filter'
    else if cfFault in Flags then
      Result := 'fault'
    else
      Result := 'catch';
  end;
var
  I, J, Indent: Integer;
  IndentStr: string;
begin
  Indent := 0;

  with TStringList.Create do
  try
    IndentStr := DupeString('  ', Indent);

    for I:=0 to InstructionCount-1 do
    begin
      for J:=0 to Method.ExceptionHandlerCount-1 do
      with Method.ExceptionHandlers[J] do
      begin
        if Instructions[I].Offset = TryBlock.Offset then
        begin
          Add(IndentStr + '.try');
          Add(IndentStr + '{');
          Inc(Indent);
        end;
        if Instructions[I].Offset = (TryBlock.Offset + TryBlock.Length) then
        begin
          Add(IndentStr + '}  // end .try');
          Dec(Indent);
        end;
        if Instructions[I].Offset = HandlerBlock.Offset then
        begin
          Add(IndentStr + FlagsToName(Flags));
          Add(IndentStr + '{');
          Inc(Indent);
        end;
        if Instructions[I].Offset = (HandlerBlock.Offset + HandlerBlock.Length) then
        begin
          Add(IndentStr + '}  // end ' + FlagsToName(Flags));
          Dec(Indent);
        end;
      end;
      Add(DupeString('  ', Indent) + Instructions[I].DumpIL(Options));
    end;

    Result := Text;
  finally
    Free;
  end;
end;

function TJclClrILGenerator.GetInstructionCount: Integer;
begin
  Result := FInstructions.Count;
end;

function TJclClrILGenerator.GetInstruction(const Idx: Integer): TJclInstruction;
begin
  Result := TJclInstruction(FInstructions[Idx]);
end;

{ TJclInstruction }

constructor TJclInstruction.Create(AOwner :TJclClrILGenerator; AOpCode: TJclOpCode);
begin
  inherited Create;

  FOwner  := AOwner;
  FOpCode := AOpCode;
end;

function TJclInstruction.GetWideOpCode: Boolean;
begin
  Result := Integer(OpCode) > MaxByte;
end;

function TJclInstruction.GetRealOpCode: Byte;
begin
  if WideOpCode then
    Result := Integer(OpCode) mod (MaxByte + 1)
  else
    Result := Integer(OpCode);
end;

function TJclInstruction.GetParamType: TJclInstructionParamType;
begin
  Result := OpCodeParamTypes[OpCode];
end;

function TJclInstruction.GetName: string;
begin
  Result := OpCodeInfos[OpCode, itName];
end;

function TJclInstruction.GetFullName: string;
begin
  Result := OpCodeInfos[OpCode, itFullName];
end;

function TJclInstruction.GetDescription: string;
begin
  Result := OpCodeInfos[OpCode, itDescription]
end;

function TJclInstruction.GetSize: DWord;
const
  OpCodeSize: array[Boolean] of DWord = (1, 2);
begin
  case ParamType of
    ptSOff,
    ptI1,
    ptU1: Result := SizeOf(Byte);
    ptI2,
    ptU2: Result := SizeOf(Word);
    ptLOff,
    ptI4,
    ptToken,
    ptU4,
    ptR4: Result := SizeOf(DWORD);
    ptI8,
    ptU8,
    ptR8: Result := SizeOf(Int64);
    ptArray: Result := (VarArrayHighBound(FParam, 1) - VarArrayLowBound(FParam, 1) + 1 + 1) * SizeOf(Integer);
  else
    Result := 0;
  end;
  Result := OpCodeSize[OpCode in [opNop..opPrefixRef]] + Result;
end;

procedure TJclInstruction.Load(Stream: TStream);
var
  Code: Byte;
  I, ArraySize, Value: DWord;
begin
  FOffset := Stream.Position;

  try
    Stream.read(Code, SizeOf(Code));

    if WideOpCode then
    begin
      if Code <> STP1 then
        raise EJclCliInstructionStreamInvalid.CreateResRec(@RsInstructionStreamInvalid);

      Stream.read(Code, SizeOf(Code));
    end;

    if Code <> RealOpCode then
      raise EJclCliInstructionStreamInvalid.CreateResRec(@RsInstructionStreamInvalid);

    with TVarData(FParam) do
    case ParamType of
      ptSOff,
      ptI1: begin Stream.read(VShortInt, SizeOf(ShortInt)); VType := varShortInt; end;
      ptU1: begin Stream.read(VByte,     SizeOf(Byte));     VType := varByte; end;
      ptI2: begin Stream.read(VSmallInt, SizeOf(SmallInt)); VType := varSmallInt; end;
      ptU2: begin Stream.read(VWord,     SizeOf(Word));     VType := varWord; end;
      ptLOff,
      ptI4: begin Stream.read(VInteger,  SizeOf(Integer));  VType := varInteger; end;
      ptToken,
      ptU4: begin Stream.read(VLongWord, SizeOf(LongWord)); VType := varLongWord; end;
      ptI8,
      ptU8: begin Stream.read(VInt64,    SizeOf(Int64));    VType := varInt64; end;
      ptR4: begin Stream.read(VSingle,   SizeOf(Single));   VType := varSingle; end;
      ptR8: begin Stream.read(VDouble,   SizeOf(Double));   VType := varDouble; end;
      ptArray:
      begin
        Stream.read(ArraySize, SizeOf(ArraySize));

        FParam := VarArrayCreate([0, ArraySize-1], varInteger);
        for I:=0 to ArraySize-1 do
        begin
          Stream.read(Value, SizeOf(Value));
          FParam[I] := Value;
        end;
      end;
    end;
  except
    Stream.Position := FOffset;
    raise;
  end;
end;

procedure TJclInstruction.Save(Stream: TStream);
var
  Code: Byte;
  ArraySize: DWord;
  I, Value: Integer;
begin
  if WideOpCode then
  begin
    Code := STP1;
    Stream.Write(Code, SizeOf(Code));
  end;

  Code := RealOpCode;;
  Stream.Write(Code, SizeOf(Code));

  case ParamType of
    ptSOff,
    ptI1: Stream.Write(TVarData(FParam).VShortInt, SizeOf(ShortInt));
    ptU1: Stream.Write(TVarData(FParam).VByte,     SizeOf(Byte));
    ptI2: Stream.Write(TVarData(FParam).VSmallInt, SizeOf(SmallInt));
    ptU2: Stream.Write(TVarData(FParam).VWord,     SizeOf(Word));
    ptLOff,
    ptI4: Stream.Write(TVarData(FParam).VInteger,  SizeOf(Integer));
    ptToken,
    ptU4: Stream.Write(TVarData(FParam).VLongWord, SizeOf(LongWord));
    ptI8,
    ptU8: Stream.Write(TVarData(FParam).VInt64,    SizeOf(Int64));
    ptR4: Stream.Write(TVarData(FParam).VSingle,   SizeOf(Single));
    ptR8: Stream.Write(TVarData(FParam).VDouble,   SizeOf(Double));
    ptArray:
    begin
      ArraySize := VarArrayHighBound(FParam, 1) - VarArrayLowBound(FParam, 1) + 1;
      Stream.Write(ArraySize, SizeOf(ArraySize));

      for I:= VarArrayHighBound(FParam, 1) to VarArrayLowBound(FParam, 1) do
      begin
        Value := VarArrayGet(FParam, [I]);
        Stream.Write(Value, SizeOf(Value));
      end;
    end;
  end;
end;

function TJclInstruction.DumpIL(Options: TJclInstructionDumpILOptions): string;
var
  Opt: TJclInstructionDumpILOption;
begin
  if doLineNo in Options then
    Result := DumpILOption(doLineNo) + ': ';

  if doRawBytes in Options then
    Result := Result + Format(' /* %.24s */ ', [DumpILOption(doRawBytes)]);

  for Opt:=doIL to doTokenValue do
    Result := Result + DumpILOption(Opt) + ' ';

  if (doComment in Options) and ((FullName <> '') or (Description <> '')) then
  begin
    Result := Result + ' // ' + DumpILOption(doComment);
  end;
end;

function TJclInstruction.FormatLabel(Offset: Integer): string;
begin
  Result := 'IL_' + IntToHex(Offset, 4);
end;

function TJclInstruction.DumpILOption(Option: TJclInstructionDumpILOption): string;
  function TokenToString(Token: DWORD): string;
  begin
    Result := '(' + IntToHex(Token shr 24, 2) + ')' + IntToHex(Token mod (1 shl 24), 6);
  end;
var
  I: Integer;
  CodeStr, ParamStr: string;
  Row: TJclClrTableRow;
begin
  case Option of
    doLineNo:
      Result := 'IL_' + IntToHex(Offset, 4);
    doRawBytes:
    begin
      if WideOpCode then
        CodeStr := IntToHex(STP1, 2);

      CodeStr := CodeStr + IntToHex(RealOpCode, 2);
      CodeStr := CodeStr + DupeString(' ', 4 - Length(CodeStr));

      case ParamType of
        ptSOff,
        ptI1,
        ptU1:     ParamStr := IntToHex(TVarData(FParam).VByte, 2);
        ptI2,
        ptU2:     ParamStr := IntToHex(TVarData(FParam).VWord, 4);
        ptLOff,
        ptI4,
        ptU4,
        ptR4:     ParamStr := IntToHex(TVarData(FParam).VLongWord, 8);
        ptI8,
        ptU8,
        ptR8:     ParamStr := IntToHex(TVarData(FParam).VInt64, 16);
        ptArray:  ParamStr := 'Array';
        ptToken:  ParamStr := TokenToString(TVarData(FParam).VLongWord);
      else
        ParamStr := '';
      end;
      ParamStr := ParamStr + DupeString(' ', 10 - Length(ParamStr));
      Result := CodeStr + ' | ' + ParamStr;
    end;
    doIL:
    begin
      case ParamType of
      ptVoid: ; // do nothing
      ptToken:
      begin
        Row := Owner.Method.Method.Table.Stream.Metadata.Tokens[TVarData(Param).VLongWord];
        if Row is TJclClrTableTypeDefRow then
          Result := TJclClrTableTypeDefRow(Row).FullName
        else if Row is TJclClrTableTypeRefRow then
        with TJclClrTableTypeRefRow(Row) do
          Result := FullName
        else if Row is TJclClrTableMethodDefRow then
        with TJclClrTableMethodDefRow(Row) do
          Result := ParentToken.FullName + '.' + Name
        else if Row is TJclClrTableFieldDefRow then
        with TJclClrTableFieldDefRow(Row) do
          Result := ParentToken.FullName + '.' + Name
        else
          Result := Row.DumpIL;

        Result := Result + ' /* ' + IntToHex(TVarData(FParam).VLongWord, 4) + ' */';
      end;
      ptSOff:  Result := FormatLabel(Offset + Size + TVarData(Param).VByte - 1);
      ptLOff:  Result := FormatLabel(Offset + Size + TVarData(Param).VLongWord - 1);
      ptArray:
      begin
        for I:= VarArrayHighBound(FParam, 1) to VarArrayLowBound(FParam, 1) do
        begin
          Result := Result + FormatLabel(Offset + Size + VarArrayGet(FParam, [I]));
          if I <> VarArrayLowBound(FParam, 1) then
            Result := Result + ', ';
        end;
        Result := ' (' + Result + ')';
      end;
      else
        Result := VarToStr(Param);
      end;

      Result := GetName + DupeString(' ', 10 - Length(GetName)) + ' ' + Result;
      Result := Result + DupeString(' ', 20 - Length(Result));
    end;
    doTokenValue:
      Result := ''; // do nothing
    doComment:
      if FullName = '' then
        Result := Description
      else if Description = '' then
        Result := FullName
      else
        Result := FullName + ' - ' + Description;
  end;
end;

end.
