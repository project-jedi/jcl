{******************************************************************************}
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JclExprEval.pas.                                        }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ Mathematical expression evaluators. Includes an infix and postfix evaluator, }
{ supports variables, operator precedence, right associative operators,        }
{ ability to add new operators and more.                                       }
{                                                                              }
{ Unit owner: Marcel Bestebroer                                                }
{ Last modified: April 17, 2001                                                }
{                                                                              }
{******************************************************************************}

unit JclExprEval;

interface

uses
  Classes, SysUtils, Contnrs, JclBase;

type
//------------------------------------------------------------------------------
// Forward declarations
//------------------------------------------------------------------------------

  TJclExprList = class;
  TJclExprOperandStack = class;
  TJclExprOperatorStack = class;
  TJclAbstractExprEval = class;
  TJclExprOperator = class;
  TJclExprOperators = class;
  TJclExprVars = class;
  TJclExprVar = class;
  TJclExprFunctions = class;
  TJclExprFunction = class;

//------------------------------------------------------------------------------
// Exceptions
//------------------------------------------------------------------------------

  EJclExpression = class(EJclError);
  EJclExprList = class(EJclExpression);
  EJclExprOperandStack = class(EJclExpression);
  EJclExprOperatorStack = class(EJclExpression);
  EJclExprEval = class(EJclExpression);
  EJclExprVars = class(EJclExpression);
  EJclExprFuncs = class(EJclExpression);

//------------------------------------------------------------------------------
// Type definitions
//------------------------------------------------------------------------------

  TJclExprItemKind = (eikUnknown, eikOperand, eikOperator, eikVariable,
    eikExpression, eikFunction);
  TJclExprArgKind = (eakOperand, eakExpression, eakOperandArray,
    eakExpressionArray);
  TJclExprEvaluatorClass = class of TJclAbstractExprEval;
  TJclExprOperatorClass = class of TJclExprOperator;
  TJclExprFunctionClass = class of TJclExprFunction;

  PJclExprItem = ^TJclExprItem;
  TJclExprItem = record
    case Kind: TJclExprItemKind of
      eikOperator:    (Operator: TJclExprOperatorClass);
      eikOperand:     (Operand: Extended);
      eikVariable:    (Variable: TJclExprVar);
      eikExpression:  (Expression: TJclExprList);
      eikFunction:    (Func: TJclExprFunctionClass;
                       Args: TJclExprList);
  end;

//------------------------------------------------------------------------------
// Expression list
//------------------------------------------------------------------------------

  TJclExprList = class(TObject)
  private
    FList: array of TJclExprItem;
    FVariableList: TJclExprVars;
    FIsChild: Boolean;
  protected
    function CheckOperand(const S: string; const SP: Integer;
      var EP: Integer): Boolean;
    function CheckOperator(const S: string; const SP: Integer;
      var EP: Integer): Boolean;
    function CheckFunction(const S: string; const SP: Integer;
      var EP: Integer): Boolean;
    function CheckVariable(const S: string; const SP: Integer;
      var EP: Integer): Boolean;
    function GetAsString: string;
    function GetCount: Integer;
    function GetItems(I: Integer): TJclExprItem;
    function GetParameters(const I: Integer): string;
    function HandleSubExpression(const Value: string; const StartPos: Integer;
      var EndPos: Integer): Boolean;
    procedure SetAsString(const Value: string);
    procedure SetVariableList(const Value: TJclExprVars);
    function New(const Index: Integer): PJclExprItem;
    procedure ParseArguments(const Func: TJclExprFunctionClass; const S: string;
      SP: Integer; var EP: Integer);
    procedure ParseSubString(const Value: string; StartPos: Integer;
      var EndPos: Integer);

    property IsChild: Boolean read FIsChild write FIsChild;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AOperand: Extended); overload;
    procedure Add(const AOperator: TJclExprOperatorClass); overload;
    procedure Add(const AVariable: TJclExprVar); overload;
    procedure Add(const AExpression: TJclExprList); overload;
    procedure Add(const AFunc: TJclExprFunctionClass); overload;
    procedure Add(const AFunc: TJclExprFunctionClass;
      const AArguments: TJclExprList); overload;
    procedure Clear;
    procedure CopyFrom(const Source: TJclExprList);
    procedure Insert(const Index: Integer; const AOperand: Extended); overload;
    procedure Insert(const Index: Integer;
      const AOperator: TJclExprOperatorClass); overload;
    procedure Insert(const Index: Integer; const AVariable: TJclExprVar);
      overload;
    procedure Insert(const Index: Integer; const AExpression: TJclExprList);
      overload;
    procedure Insert(const Index: Integer; const AFunc: TJclExprFunctionClass);
      overload;
    procedure Insert(const Index: Integer; const AFunc: TJclExprFunctionClass;
      const AArguments: TJclExprList); overload;

    property AsString: string read GetAsString write SetAsString;
    property Count: Integer read GetCount;
    property Items[I: Integer]: TJclExprItem read GetItems; default;
    property VariableList: TJclExprVars read FVariableList;
  end;

//------------------------------------------------------------------------------
// Operand stack
//------------------------------------------------------------------------------

  TJclExprOperandStack = class(TObject)
  private
    FItems: array of TJclExprItem;
    FTop: Cardinal;
  protected
    function GetCapacity: Cardinal;
    procedure ReAlloc(const HasGrown: Boolean);
    procedure Push(const AItem: TJclExprItem); overload;
  public
    procedure Push(const AOperand: Extended); overload;
    procedure Push(const AOperator: TJclExprOperatorClass); overload;
    procedure Push(const AVariable: TJclExprVar); overload;
    procedure Push(const AExpression: TJclExprList); overload;
    function Peek: TJclExprItem;
    function PeekExpr: TJclExprList;
    function PeekOperand: Extended;
    function Pop: TJclExprItem;
    function PopExpr: TJclExprList;
    function PopOperand: Extended;
    procedure Clear;

    property Top: Cardinal read FTop;
    property Capacity: Cardinal read GetCapacity;
  end;

//------------------------------------------------------------------------------
// Operator stack
//------------------------------------------------------------------------------

  TJclExprOperatorStack = class(TObject)
  private
    FItems: array of TJclExprOperatorClass;
    FTop: Cardinal;
    FTargetList: TJclExprList;
  protected
    function GetCapacity: Cardinal;
    procedure ReAlloc(const HasGrown: Boolean);
    procedure SetTargetList(const Value: TJclExprList);
  public
    procedure Push(const AValue: TJclExprOperatorClass);
    function Peek: TJclExprOperatorClass;
    function Pop: TJclExprOperatorClass;
    procedure Clear;

    property Top: Cardinal read FTop;
    property Capacity: Cardinal read GetCapacity;
    property TargetList: TJclExprList read FTargetList;
  end;

//------------------------------------------------------------------------------
// Expression evaluators
//------------------------------------------------------------------------------

  TJclAbstractExprEval = class(TObject)
  private
    FExprList: TJclExprList;
    FOwnsList: Boolean;
  protected
    function GetExpression: string;
    function GetVars: TJclExprVars;
    procedure SetExpression(const AExpression: string);
    procedure SetExpressionList(const List: TJclExprList); 

    property Expression: string read GetExpression write SetExpression;
    property ExpressionList: TJclExprList read FExprList;
    property Vars: TJclExprVars read GetVars;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Evaluate: Extended; virtual; abstract;
  end;

  TJclPostFixExprEval = class(TJclAbstractExprEval)
  private
  protected
    procedure EvaluateOperator(const Operator: TJclExprOperatorClass;
      const Operands: TJclExprOperandStack);
    function UpdateLeftPrecedence(const Index: Integer;
      const AExprList: TJclExprList): Boolean;
    function UpdateRightPrecedence(const Index: Integer;
      const AExprList: TJclExprList): Boolean;
  public
    function Evaluate: Extended; override;
    procedure InFixList(const AExprList: TJclExprList);
    function AsInFix: string;

    property Expression;
    property Vars;
  end;

  TJclInFixExprEval = class(TJclAbstractExprEval)
  private
    FRank: Integer;
    FOperandStack: TJclExprOperandStack;
    FOperatorStack: TJclExprOperatorStack;
  protected
    procedure HandleFunction(const ExprItem: TJclExprItem);
    procedure UpdateAndCheckRank(const ExprItem: TJclExprItem);
    property OperandStack: TJclExprOperandStack read FOperandStack;
    property OperatorStack: TJclExprOperatorStack read FOperatorStack;
    property Rank: Integer read FRank write FRank;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Evaluate: Extended; override;
    procedure PostFixList(const AExprList: TJclExprList);
    function AsPostFix: string;

    property Expression;
    property Vars;
  end;

//------------------------------------------------------------------------------
// Operators
//------------------------------------------------------------------------------

  TJclExprOperators = class(TList)
  protected
    function GetItems(Index: Integer): TJclExprOperatorClass;
    procedure PutItems(Index: Integer; Item: TJclExprOperatorClass);
  public
    function Add(Item: TJclExprOperatorClass): Integer;
    function IndexOf(Item: TJclExprOperatorClass): Integer;
    function Remove(Item: TJclExprOperatorClass): Integer;

    property Items[Index: Integer]: TJclExprOperatorClass read GetItems
      write PutItems; default;
  end;

  TJclExprOperator = class(TObject)
  public
    class function Symbol: string; virtual; abstract;
    class function Evaluate(const Op1, Op2: Extended): Extended; overload;
      virtual; abstract;
    class procedure Evaluate(const OpStack: TJclExprOperandStack;
      const IgnoreCalcErrors: Boolean = False); overload; virtual; abstract;
    class procedure Input(const Operands: TJclExprOperandStack;
      const Operators: TJclExprOperatorStack;
      const IgnoreCalcErrors: Boolean = False); virtual; abstract;
    class function InputPrecedence: Smallint; virtual; abstract;
    class function StackPrecedence: Smallint; virtual; abstract;
    class function Rank: Smallint; virtual; abstract;
  end;

  TJclExprOperatorBase = class(TJclExprOperator)
  public
    class procedure Evaluate(const OpStack: TJclExprOperandStack;
      const IgnoreCalcErrors: Boolean = False); override;
    class procedure Input(const Operands: TJclExprOperandStack;
      const Operators: TJclExprOperatorStack;
      const IgnoreCalcErrors: Boolean = False); override;
    class function StackPrecedence: Smallint; override;
    class function Rank: Smallint; override;
  end;

  TJclExprOperatorBasePrec1 = class(TJclExprOperatorBase)
  public
    class function InputPrecedence: Smallint; override;
  end;

  TJclExprOperatorBasePrec2 = class(TJclExprOperatorBase)
  public
    class function InputPrecedence: Smallint; override;
  end;

  TJclExprOperatorBasePrec3 = class(TJclExprOperatorBase)
  public
    class function InputPrecedence: Smallint; override;
  end;

  TJclExprOperatorBasePrec4 = class(TJclExprOperatorBase)
  public
    class function InputPrecedence: Smallint; override;
  end;

  TJclExprOperatorAdd = class(TJclExprOperatorBasePrec2)
  public
    class function Symbol: string; override;
    class function Evaluate(const Op1, Op2: Extended): Extended; override;
  end;

  TJclExprOperatorSub = class(TJclExprOperatorBasePrec2)
  public
    class function Symbol: string; override;
    class function Evaluate(const Op1, Op2: Extended): Extended; override;
  end;

  TJclExprOperatorMult = class(TJclExprOperatorBasePrec3)
  public
    class function Symbol: string; override;
    class function Evaluate(const Op1, Op2: Extended): Extended; override;
  end;

  TJclExprOperatorDiv = class(TJclExprOperatorBasePrec3)
  public
    class function Symbol: string; override;
    class function Evaluate(const Op1, Op2: Extended): Extended; override;
  end;

  TJclExprOperatorIDiv = class(TJclExprOperatorBasePrec3)
  public
    class function Symbol: string; override;
    class function Evaluate(const Op1, Op2: Extended): Extended; override;
  end;

  TJclExprOperatorIMod = class(TJclExprOperatorBasePrec3)
  public
    class function Symbol: string; override;
    class function Evaluate(const Op1, Op2: Extended): Extended; override;
  end;

  TJclExprOperatorExp = class(TJclExprOperatorBase)
  public
    class function Symbol: string; override;
    class function Evaluate(const Op1, Op2: Extended): Extended; override;
    class function InputPrecedence: Smallint; override;
    class function StackPrecedence: Smallint; override;
  end;

  TJclExprOperatorAnd = class(TJclExprOperatorBasePrec3)
  public
    class function Symbol: string; override;
    class function Evaluate(const Op1, Op2: Extended): Extended; override;
  end;

  TJclExprOperatorOr = class(TJclExprOperatorBasePrec2)
  public
    class function Symbol: string; override;
    class function Evaluate(const Op1, Op2: Extended): Extended; override;
  end;

  TJclExprOperatorXor = class(TJclExprOperatorBasePrec2)
  public
    class function Symbol: string; override;
    class function Evaluate(const Op1, Op2: Extended): Extended; override;
  end;

  TJclExprOperatorShl = class(TJclExprOperatorBasePrec3)
  public
    class function Symbol: string; override;
    class function Evaluate(const Op1, Op2: Extended): Extended; override;
  end;
  
  TJclExprOperatorShr = class(TJclExprOperatorBasePrec3)
  public
    class function Symbol: string; override;
    class function Evaluate(const Op1, Op2: Extended): Extended; override;
  end;

  TJclExprOperatorLT = class(TJclExprOperatorBasePrec1)
  public
    class function Symbol: string; override;
    class function Evaluate(const Op1, Op2: Extended): Extended; override;
  end;

  TJclExprOperatorLE = class(TJclExprOperatorBasePrec1)
  public
    class function Symbol: string; override;
    class function Evaluate(const Op1, Op2: Extended): Extended; override;
  end;

  TJclExprOperatorEQ = class(TJclExprOperatorBasePrec1)
  public
    class function Symbol: string; override;
    class function Evaluate(const Op1, Op2: Extended): Extended; override;
  end;

  TJclExprOperatorGE = class(TJclExprOperatorBasePrec1)
  public
    class function Symbol: string; override;
    class function Evaluate(const Op1, Op2: Extended): Extended; override;
  end;

  TJclExprOperatorGT = class(TJclExprOperatorBasePrec1)
  public
    class function Symbol: string; override;
    class function Evaluate(const Op1, Op2: Extended): Extended; override;
  end;

  TJclExprOperatorNE = class(TJclExprOperatorBasePrec1)
  public
    class function Symbol: string; override;
    class function Evaluate(const Op1, Op2: Extended): Extended; override;
  end;

  TJclExprOperatorLParen = class(TJclExprOperatorBase)
  public
    class function Symbol: string; override;
    class function Evaluate(const Op1, Op2: Extended): Extended; override;
    class procedure Evaluate(const OpStack: TJclExprOperandStack;
      const IgnoreCalcErrors: Boolean = False); override;
    class function InputPrecedence: Smallint; override;
    class function StackPrecedence: Smallint; override;
    class function Rank: Smallint; override;
  end;

  TJclExprOperatorRParen = class(TJclExprOperatorBase)
  public
    class function Symbol: string; override;
    class function Evaluate(const Op1, Op2: Extended): Extended; override;
    class procedure Evaluate(const OpStack: TJclExprOperandStack;
      const IgnoreCalcErrors: Boolean = False); override;
    class procedure Input(const Operands: TJclExprOperandStack;
      const Operators: TJclExprOperatorStack;
      const IgnoreCalcErrors: Boolean = False); override;
    class function InputPrecedence: Smallint; override;
    class function StackPrecedence: Smallint; override;
    class function Rank: Smallint; override;
  end;

//------------------------------------------------------------------------------
// Expression variables
//------------------------------------------------------------------------------

  TJclExprVars = class(TObject)
  private
    FItems: TObjectList;
    FAutoAdd: Boolean;
  protected
    function GetCount: Integer;
    function GetItems(Index: Integer): TJclExprVar;
    function GetValues(Name: string): Extended;
    procedure PutItems(Index: Integer; AVar: TJclExprVar);
    procedure SetValues(Name: string; Value: Extended);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const AName: string): TJclExprVar; overload;
    procedure Add(const AVar: TJclExprVar); overload;
    procedure Clear;
    procedure Delete(const AIndex: Integer); overload;
    procedure Delete(const AName: string); overload;
    procedure Delete(const AVar: TJclExprVar); overload;
    function IndexOf(const AName: string): Integer; overload;
    function IndexOf(const AVar: TJclExprVar): Integer; overload;
    function Insert(const AIndex: Integer; const AName: string): TJclExprVar;
      overload;
    procedure Insert(const AIndex: Integer; const AVar: TJclExprVar); overload;

    property AutoAdd: Boolean read FAutoAdd write FAutoAdd;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TJclExprVar read GetItems write PutItems;
    property Values[Name: string]: Extended read GetValues write SetValues;
      default;
  end;

  TJclExprVar = class(TPersistent)
  private
    FName: string;
    FValue: Extended;
    FOwner: TJclExprVars;
  protected
    property Owner: TJclExprVars read FOwner;
  public
    constructor Create(const AOwner: TJclExprVars; const AName: string);
    destructor Destroy; override;

    property Name: string read FName;
    property Value: Extended read FValue write FValue;
  end;

//------------------------------------------------------------------------------
// Expression functions
//------------------------------------------------------------------------------

  TJclExprFunctions = class(TList)
  protected
    function GetFunctions(const Name: string): TJclExprFunctionClass;
    function GetItems(const Index: Integer): TJclExprFunctionClass;
    procedure PutItems(const Index: Integer; const Item: TJclExprFunctionClass);
  public
    function Add(const Item: TJclExprFunctionClass): Integer;
    function IndexOf(const Item: TJclExprFunctionClass): Integer; overload;
    function IndexOf(const Name: string): Integer; overload;
    function Remove(const Item: TJclExprFunctionClass): Integer;

    property Items[const Index: Integer]: TJclExprFunctionClass read GetItems
      write PutItems; default;
    property Functions[const Name: string]: TJclExprFunctionClass
      read GetFunctions;
  end;

  TJclExprFunction = class(TObject)
  public
    class function ArgumentCount: Integer; virtual;
    class function Arguments(const Index: Integer): TJclExprArgKind; virtual;
    class function Result: TJclExprArgKind; virtual;
    class function Name: string; virtual;
    class procedure Evaluate(const OperandStack: TJclExprOperandStack); virtual;
      abstract;
  end;

  TJclEFAbs = class(TJclExprFunction)
  public
    class procedure Evaluate(const OperandStack: TJclExprOperandStack);
      override;
  end;

  TJclEFMin = class(TJclExprFunction)
  public
    class function Arguments(const Index: Integer): TJclExprArgKind; override;
    class procedure Evaluate(const OperandStack: TJclExprOperandStack);
      override;
  end;

  TJclEFMax = class(TJclEFMin)
  public
    class procedure Evaluate(const OperandStack: TJclExprOperandStack);
      override;
  end;

  TJclEFIf = class(TJclExprFunction)
  public
    class function ArgumentCount: Integer; override;
    class function Arguments(const Index: Integer): TJclExprArgKind; override;
    class function Result: TJclExprArgKind; override;
    class procedure Evaluate(const OperandStack: TJclExprOperandStack);
      override;
  end;

  TJclEFSum = class(TJclExprFunction)
  public
    class function Arguments(const Index: Integer): TJclExprArgKind; override;
    class procedure Evaluate(const OperandStack: TJclExprOperandStack);
      override;
  end;

  TJclEFAvg = class(TJclExprFunction)
  public
    class procedure Evaluate(const OperandStack: TJclExprOperandStack);
      override;
  end;

//------------------------------------------------------------------------------
// Operator registration list
//------------------------------------------------------------------------------

function Operators: TJclExprOperators;

//------------------------------------------------------------------------------
// Function registration list
//------------------------------------------------------------------------------

function Functions: TJclExprFunctions;

implementation

uses
  Consts, JclLogic, Math, JclResources;

//------------------------------------------------------------------------------
// TJclExprList
//------------------------------------------------------------------------------

function TJclExprList.CheckOperand(const S: string; const SP: Integer;
  var EP: Integer): Boolean;
var
  HaveDec: Boolean;
  HaveExp: Boolean;
  DecPos: Integer;
  TmpS: string;

begin
  EP := SP;
  Result := True;
  HaveDec := False;
  HaveExp := False;
  DecPos := 0;
  while Result and (EP <= Length(S)) do
  begin
    Result := S[EP] in ['0' .. '9', 'E', 'e', '.', ',', '-', '+'];
    if Result and (S[EP] in ['+', '-']) then
      Result := (EP = SP) or (S[Pred(EP)] in ['e', 'E']);
    if Result and (S[EP] in ['.', ',']) then
    begin
      Result := not HaveDec;
      if not Result and (Copy(S, EP, 2) <> ', ') then
        raise EJclExprList.CreateResRecFmt(@RsExprEvalInvalidChar, [EP]);
      if not HaveDec then
        DecPos := Succ(EP - SP);
      HaveDec := True;
    end;
    if Result and (S[EP] in ['E', 'e']) then
    begin
      Result := not HaveExp and (EP > SP);
      HaveExp := True;
    end;
    if Result then
      Inc(EP);
  end;
  Result := (not Result and (EP > SP)) or (EP > Length(S));
  if Result then
  begin
    Dec(EP);
    TmpS := Copy(S, SP, Succ(EP - SP));
    if DecPos > 0 then
      TmpS[DecPos] := DecimalSeparator;
    Add(StrToFloat(TmpS));
  end;
end;

//------------------------------------------------------------------------------

function TJclExprList.CheckOperator(const S: string; const SP: Integer;
  var EP: Integer): Boolean;
var
  I: Integer;

begin
  EP := SP;
  Result := False;
  I := Pred(Operators.Count);
  while not Result and (I >= 0) do
  begin
    Result := CompareText(Copy(S, SP, Length(Operators[I].Symbol)),
      Operators[I].Symbol) = 0;
    EP := SP + Pred(Length(Operators[I].Symbol));
    if Result and
        (Operators[I].Symbol[1] in ['A' .. 'Z', 'a' .. 'z']) then
    begin
      if SP > 1 then
        Result := S[Pred(SP)] in [#0 .. ' '];
      if Result and (EP < Length(S)) then
        Result := (S[Succ(EP)] in [#0 .. ' ']);
    end;
    if Result then
      Add(Operators[I])
    else
      Dec(I);
  end;
end;

//------------------------------------------------------------------------------

function TJclExprList.CheckFunction(const S: string; const SP: Integer;
  var EP: Integer): Boolean;
var
  I: Integer;
  Tmp: Integer;

begin
  EP := SP;
  Result := False;
  I := Pred(Functions.Count);
  while not Result and (I >= 0) do
  begin
    Result := SameText(Copy(S, SP, Length(Functions[I].Name)),
      Functions[I].Name);
    EP := SP + Pred(Length(Functions[I].Name));
    if Result then
    begin
      Tmp := Succ(EP);
      while (Tmp <= Length(S)) and (S[Tmp] in [#0 .. ' ']) do
        Inc(Tmp);
      if (Tmp <= Length(S)) and (S[Tmp] = '(') then
        ParseArguments(Functions[I], S, Tmp, EP)
      else
        Add(Functions[I]);
    end
    else
      Dec(I);
  end;
end;

function TJclExprList.CheckVariable(const S: string; const SP: Integer;
  var EP: Integer): Boolean;
var
  I: Integer;

begin
  EP := SP;
  Result := False;
  if VariableList <> nil then
  begin
    while (EP <= Length(S)) and
        (S[EP] in ['A' .. 'Z', '0' .. '9', 'a' .. 'z']) do
      Inc(EP);
    I := VariableList.IndexOf(Copy(S, SP, EP - SP));
    if (I = -1) and VariableList.AutoAdd then
    begin
      VariableList.Add(Copy(S, SP, EP - SP));
      I := VariableList.IndexOf(Copy(S, SP, EP - SP));
    end;
    if I > -1 then
    begin
      Add(VariableList.Items[I]);
      Result := True;
    end;
    Dec(EP);
  end;
end;

//------------------------------------------------------------------------------

function TJclExprList.GetAsString: string;
var
  I: Integer;
  NeedSpaces: Boolean;

begin
  Result := '';
  NeedSpaces := (Count > 1) and (Items[1].Kind <> eikOperator);
  for I := 0 to Pred(Count) do
  begin
    case Items[I].Kind of
      eikOperand:
        begin
          if NeedSpaces and (I > 0) then
            Result := Result + ' '; 
          Result := Result + FloatToStr(Items[I].Operand);
        end;
      eikOperator:
        begin
          if NeedSpaces or
            (UpCase(Items[I].Operator.Symbol[1]) in ['A' .. 'Z']) then
            Result := Result + ' ';
          Result := Result + Items[I].Operator.Symbol;
          if not NeedSpaces and
            (UpCase(Items[I].Operator.Symbol[1]) in ['A' .. 'Z']) then
            Result := Result + ' ';
        end;
      eikVariable:
        begin
          if NeedSpaces and (I > 0) then
            Result := Result + ' '; 
          Result := Result + Items[I].Variable.Name;
        end;
      eikExpression:
        begin
          if NeedSpaces and (I > 0) then
            Result := Result + ' ';
          Result := Result + '[' + Items[I].Expression.AsString + ']';
        end;
      eikFunction:
        begin
          if NeedSpaces and (I > 0) then
            Result := Result + ' ';
          Result := Result + Items[I].Func.Name;
          if Items[I].Args <> nil then
            Result := Result + '(' + GetParameters(I) + ')';
        end;
    end;
  end;
  Result := Trim(Result);
end;

//------------------------------------------------------------------------------

function TJclExprList.GetCount: Integer;
begin
  Result := Length(FList);
end;

//------------------------------------------------------------------------------

function TJclExprList.GetItems(I: Integer): TJclExprItem;
begin
  if (I < 0) or (I >= Count) then
    raise EListError.CreateFmt(SListIndexError, [I]);
  Result := FList[I];
end;

//------------------------------------------------------------------------------

function TJclExprList.GetParameters(const I: Integer): string;
var
  EI: TJclExprItem;
  Func: TJclExprFunctionClass;
  ArgNum: Integer;
  ArrayNum: Integer;
  ArrayCount: Integer;
  ArgExpr: TJclExprList;

begin
  Result := '';
  EI := Items[I];
  Func := EI.Func;
  for ArgNum := 0 to Pred(EI.Args.Count) do
  begin
    if ArgNum > 0 then
      Result := Result + ', ';
    case Func.Arguments(ArgNum) of
      eakOperand,
      eakExpression:
        Result := Result + EI.Args[ArgNum].Expression.AsString;
      eakOperandArray,
      eakExpressionArray:
        begin
          ArgExpr := EI.Args[ArgNum].Expression;
          ArrayCount := ArgExpr.Count;
          for ArrayNum := 0 to Pred(ArrayCount) do
          begin
            if (ArgNum > 0) or (ArrayNum > 0) then
              Result := Result + ', ';
            Result := Result + ArgExpr[ArrayNum].Expression.AsString;
          end;
        end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TJclExprList.HandleSubExpression(const Value: string;
  const StartPos: Integer; var EndPos: Integer): Boolean;
var
  TmpExpr: TJclExprList;

begin
  Result := True;
  TmpExpr := TJclExprList.Create;
  try
    TmpExpr.SetVariableList(VariableList);
    EndPos := Length(Value);
    TmpExpr.ParseSubString(Value, StartPos, EndPos);
    Add(TmpExpr);
  except
    TmpExpr.Free;
    raise;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclExprList.SetAsString(const Value: string);
var
  EndPos: Integer;

begin
  EndPos := Length(Value);
  ParseSubString(Value, 1, EndPos);
end;

//------------------------------------------------------------------------------

procedure TJclExprList.SetVariableList(const Value: TJclExprVars);
begin
  if Value <> VariableList then
  begin
    if not IsChild then
      FVariableList.Free;
    FVariableList := Value;
    IsChild := True;
  end;
end;

//------------------------------------------------------------------------------

function TJclExprList.New(const Index: Integer): PJclExprItem;
begin
  if (Index < 0) or (Index > Count) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  SetLength(FList, Succ(Count));
  if Index < Pred(Count) then
    Move(FList[Index], FList[Index + 1], (Pred(Count) - Index) *
      SizeOf(TJclExprItem));
  Result := @FList[Index];
end;

//------------------------------------------------------------------------------

procedure TJclExprList.ParseArguments(const Func: TJclExprFunctionClass;
  const S: string; SP: Integer; var EP: Integer);
var
  ArgNum: Integer;
  ArgCount: Integer;
  ArgList: TJclExprList;
  CurArg: TJclExprList;
  ParCount: Integer;
  BracketCount: Integer;

begin
  ArgList := TJclExprList.Create;
  try
    ArgList.SetVariableList(VariableList);
    Inc(SP);
    while (SP <= Length(S)) and (S[SP] in [#0 .. ' ']) do
      Inc(SP);

    EP := SP;
    ArgNum := 0;
    ArgCount := 0;
    while (SP <= Length(S)) and (S[SP] <> ')') do
    begin
      if ArgCount = ArgNum then
      begin
        ArgList.Add(TJclExprList.Create);
        ArgList[ArgNum].Expression.SetVariableList(VariableList);
      end;
      CurArg := ArgList[ArgNum].Expression;
      if (ArgCount >= ArgNum) and (
        Func.Arguments(ArgNum) in [eakExpressionArray, eakOperandArray]) then
      begin
        CurArg := TJclExprList.Create;
        try
          CurArg.SetVariableList(VariableList);
          ArgList[ArgNum].Expression.Add(CurArg);
        except
          CurArg.Free;
          raise;
        end;
      end;
      Inc(ArgCount);
      if (ArgNum >= Func.ArgumentCount) or
          ((ArgNum = Pred(Func.ArgumentCount)) and
          (ArgCount > Func.ArgumentCount) and
          not (Func.Arguments(ArgNum) in [eakOperandArray,
            eakExpressionArray])) then
        raise EJclExprFuncs.CreateResRecFmt(@RsExprEvalWrongArgumentCount,
          [Func.Name]);
      EP := SP;
      ParCount := 0;
      BracketCount := 0;
      while (EP <= Length(S)) and (((ParCount > 0) and (BracketCount > 0)) or
        ((Copy(S, EP, 2) <> ', ') and (S[EP] <> ')'))) do
      begin
        if S[EP] = '(' then
          Inc(ParCount)
        else if S[EP]=')' then
          Dec(ParCount);
        if S[EP] = '[' then
          Inc(BracketCount)
        else if S[EP]=']' then
          Dec(BracketCount);
        Inc(EP);
      end;
      if BracketCount <> 0 then
        raise EJclExprList.CreateResRec(@RsExprEvalMissingRightBracket);
      if (ParCount <> 0) or (EP > Length(S)) then
        raise EJclExprList.CreateResRec(@RsExprEvalMissingRightParenthesis);
      ParCount := Pred(EP);
      CurArg.ParseSubString(S, SP, ParCount);
      if Copy(S, EP, 2) = ', ' then
        Inc(EP, 2);
      SP := EP;
      if ArgNum < Pred(Func.ArgumentCount) then
        Inc(ArgNum);
    end;
    Add(Func, ArgList);
  except
    ArgList.Free;
    raise;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclExprList.ParseSubString(const Value: string; StartPos: Integer;
  var EndPos: Integer);
var
 IsSubExpression: Boolean;
 AbsEndPos: Integer;

begin
  Clear;
  if EndPos <0 then
    AbsEndPos := Length(Value)
  else
    AbsEndPos := EndPos;
  while (StartPos <= AbsEndPos) and (Value[StartPos] in [#0 .. ' ']) do
    Inc(StartPos);
  IsSubExpression := (StartPos <= AbsEndPos) and (Value[StartPos] = '[');
  if IsSubExpression then
    Inc(StartPos);
  while (StartPos <= AbsEndPos) and (not IsSubExpression or
    (Value[StartPos] <> ']')) do
  begin
    while (StartPos <= AbsEndPos) and
        (Value[StartPos] in [#0 .. ' ']) do
      Inc(StartPos);
    EndPos := StartPos;
    if (StartPos <= AbsEndPos) and (not IsSubExpression or
      (Value[StartPos] <> ']')) then
    begin
      if ((Value[StartPos] = '[') and
          not HandleSubExpression(Value, StartPos, EndPos)) or (
          (Value[StartPos] <> '[') and
          not CheckOperator(Value, StartPos, EndPos) and
          not CheckOperand(Value, StartPos, EndPos) and
          not CheckFunction(Value, StartPos, EndPos) and
          not CheckVariable(Value, StartPos, EndPos)) then
        raise EJclExpression.CreateResRecFmt(@RsExprEvalInvalidChar, [StartPos]);
    end;
    StartPos := Succ(EndPos);
  end;
  if IsSubExpression and (Value[StartPos] <> ']') then
    raise EJclExpression.CreateResRec(@RsExprEvalMissingRightBracket)
  else if IsSubExpression then
    EndPos := StartPos;
end;

//------------------------------------------------------------------------------

constructor TJclExprList.Create;
begin
  inherited Create;
  FVariableList := TJclExprVars.Create;
end;

//------------------------------------------------------------------------------

destructor TJclExprList.Destroy;
begin
  Clear;
  if not IsChild then
    VariableList.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TJclExprList.Add(const AOperand: Extended);
begin
  with New(Count)^ do
  begin
    Kind := eikOperand;
    Operand := AOperand;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclExprList.Add(const AOperator: TJclExprOperatorClass);
begin
  with New(Count)^ do
  begin
    Kind := eikOperator;
    Operator := AOperator;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclExprList.Add(const AVariable: TJclExprVar);
begin
  with New(Count)^ do
  begin
    Kind := eikVariable;
    Variable := AVariable;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclExprList.Add(const AExpression: TJclExprList);
begin
  with New(Count)^ do
  begin
    Kind := eikExpression;
    Expression := AExpression;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclExprList.Add(const AFunc: TJclExprFunctionClass);
begin
  with New(Count)^ do
  begin
    Kind := eikFunction;
    Func := AFunc;
    Args := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclExprList.Add(const AFunc: TJclExprFunctionClass;
  const AArguments: TJclExprList);
begin
  with New(Count)^ do
  begin
    Kind := eikFunction;
    Func := AFunc;
    Args := AArguments;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclExprList.Clear;
var
  I: Integer;

begin
  for I := 0 to Pred(Count) do
    if Items[I].Kind = eikExpression then
      Items[I].Expression.Free;
  SetLength(FList, 0);
end;

//------------------------------------------------------------------------------

procedure TJclExprList.CopyFrom(const Source: TJclExprList);
var
  I: Integer;
  EI: TJclExprItem;
  VarIdx: Integer;
  SubExpr: TJclExprList;

begin
  for I := 0 to Pred(Source.Count) do
  begin
    EI := Source[I];
    case EI.Kind of
      eikOperand:
        Add(EI.Operand);
      eikOperator:
        Add(EI.Operator);
      eikVariable:
        begin
          VarIdx := VariableList.IndexOf(EI.Variable.Name);
          if VarIdx <0 then
          begin
            VariableList.Add(EI.Variable.Name);
            VarIdx := VariableList.IndexOf(EI.Variable.Name);
          end;
          Add(VariableList.Items[VarIdx]);
        end;
      eikExpression:
        begin
          SubExpr := TJclExprList.Create;
          try
            SubExpr.SetVariableList(VariableList);
            SubExpr.CopyFrom(EI.Expression);
            Add(SubExpr);
          except
            SubExpr.Free;
            raise;
          end;
        end;
      eikFunction:
        begin
          SubExpr := TJclExprList.Create;
          try
            SubExpr.SetVariableList(VariableList);
            SubExpr.CopyFrom(EI.Args);
            Add(EI.Func, SubExpr);
          except
            SubExpr.Free;
            raise;
          end;
        end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclExprList.Insert(const Index: Integer; const AOperand: Extended);
begin
  with New(Index)^ do
  begin
    Kind := eikOperand;
    Operand := AOperand;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclExprList.Insert(const Index: Integer;
  const AOperator: TJclExprOperatorClass);
begin
  with New(Index)^ do
  begin
    Kind := eikOperator;
    Operator := AOperator;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclExprList.Insert(const Index: Integer;
  const AVariable: TJclExprVar);
begin
  with New(Index)^ do
  begin
    Kind := eikVariable;
    Variable := AVariable;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclExprList.Insert(const Index: Integer;
  const AExpression: TJclExprList);
begin
  with New(Index)^ do
  begin
    Kind := eikExpression;
    Expression := AExpression;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclExprList.Insert(const Index: Integer;
  const AFunc: TJclExprFunctionClass);
begin
  with New(Index)^ do
  begin
    Kind := eikFunction;
    Func := AFunc;
    Args := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclExprList.Insert(const Index: Integer;
  const AFunc: TJclExprFunctionClass; const AArguments: TJclExprList);
begin
  with New(Index)^ do
  begin
    Kind := eikFunction;
    Func := AFunc;
    Args := AArguments;
  end;
end;

//------------------------------------------------------------------------------
// TJclExprOperandStack
//------------------------------------------------------------------------------

function TJclExprOperandStack.GetCapacity: Cardinal;
begin
  Result := Length(FItems);
end;

//------------------------------------------------------------------------------

procedure TJclExprOperandStack.ReAlloc(const HasGrown: Boolean);
var
  CurCapacity: Cardinal;
  MinCapacity: Cardinal;
  
begin
  CurCapacity := Capacity;
  if Top = 0 then
    MinCapacity := 0
  else if Top < 256 then
    MinCapacity := 1 shl (BitsNeeded(Top))
  else
    MinCapacity := 128 * Succ((Top div 128));
  if (MinCapacity > 0) and (MinCapacity < 4) then
    MinCapacity := 4;
  if (MinCapacity <> CurCapacity) and (HasGrown or
      ((MinCapacity - Top) > 2) or (Top = 0)) then
    SetLength(FItems, MinCapacity)
end;

//------------------------------------------------------------------------------

procedure TJclExprOperandStack.Push(const AItem: TJclExprItem);
begin
  Inc(FTop);
  ReAlloc(True);
  FItems[Pred(FTop)] := AItem;
end;

//------------------------------------------------------------------------------

procedure TJclExprOperandStack.Push(const AOperand: Extended);
var
  Item: TJclExprItem;
  
begin
  Item.Kind := eikOperand;
  Item.Operand := AOperand;
  Push(Item);
end;

//------------------------------------------------------------------------------

procedure TJclExprOperandStack.Push(const AOperator: TJclExprOperatorClass);
var
  Item: TJclExprItem;
  
begin
  Item.Kind := eikOperator;
  Item.Operator := AOperator;
  Push(Item);
end;

//------------------------------------------------------------------------------

procedure TJclExprOperandStack.Push(const AVariable: TJclExprVar);
var
  Item: TJclExprItem;

begin
  Item.Kind := eikVariable;
  Item.Variable := AVariable;
  Push(Item);
end;

//------------------------------------------------------------------------------

procedure TJclExprOperandStack.Push(const AExpression: TJclExprList);
var
  Item: TJclExprItem;

begin
  Item.Kind := eikExpression;
  Item.Expression := AExpression;
  Push(Item);
end;

//------------------------------------------------------------------------------

function TJclExprOperandStack.Peek: TJclExprItem;
begin
  if Top = 0 then
    raise EJclExprOperandStack.CreateResRec(@RsExprEvalStackEmpty);
  Result := FItems[Pred(Top)];
end;

//------------------------------------------------------------------------------

function TJclExprOperandStack.PeekExpr: TJclExprList;
begin
  with Peek do
    if Kind = eikExpression then
      Result := Expression
    else
      raise EJclExprList.CreateResRec(@RsExprEvalStackTopNotAnExpression);
end;

//------------------------------------------------------------------------------

function TJclExprOperandStack.PeekOperand: Extended;
begin
  with Peek do
    case Kind of
      eikOperand:
        Result := Operand;
      eikVariable:
        Result := Variable.Value;
      else
        raise EJclExprList.CreateResRec(@RsExprEvalStackTopNotAnOpOrVar);
    end;
end;

//------------------------------------------------------------------------------

function TJclExprOperandStack.Pop: TJclExprItem;
begin
  Result := Peek;
  Dec(FTop);
  ReAlloc(False);
end;

//------------------------------------------------------------------------------

function TJclExprOperandStack.PopExpr: TJclExprList;
begin
  with Pop do
    if Kind = eikExpression then
      Result := Expression
    else
      raise EJclExprList.CreateResRec(@RsExprEvalStackTopNotAnExpression);
end;

//------------------------------------------------------------------------------

function TJclExprOperandStack.PopOperand: Extended;
begin
  with Pop do
    case Kind of
      eikOperand:   Result := Operand;
      eikVariable:  Result := Variable.Value;
      else          raise EJclExprList.CreateResRec(
        @RsExprEvalStackTopNotAnOpOrVar);
    end;
end;

//------------------------------------------------------------------------------

procedure TJclExprOperandStack.Clear;
begin
  FTop := 0;
  ReAlloc(False);
end;

//------------------------------------------------------------------------------
// TJclExprOperatorStack
//------------------------------------------------------------------------------

function TJclExprOperatorStack.GetCapacity: Cardinal;
begin
  Result := Length(FItems);
end;

//------------------------------------------------------------------------------

procedure TJclExprOperatorStack.ReAlloc(const HasGrown: Boolean);
var
  CurCapacity: Cardinal;
  MinCapacity: Cardinal;

begin
  CurCapacity := Capacity;
  if Top = 0 then
    MinCapacity := 0
  else if Top < 256 then
    MinCapacity := 1 shl (BitsNeeded(Top))
  else
    MinCapacity := 128 * Succ((Top div 128));
  if (MinCapacity > 0) and (MinCapacity < 4) then
    MinCapacity := 4;
  if (MinCapacity <> CurCapacity) and (HasGrown or
      ((MinCapacity - Top) > 2) or (Top = 0)) then
    SetLength(FItems, MinCapacity)
end;

//------------------------------------------------------------------------------

procedure TJclExprOperatorStack.SetTargetList(const Value: TJclExprList);
begin
  FTargetList := Value;
end;

//------------------------------------------------------------------------------

procedure TJclExprOperatorStack.Push(const AValue: TJclExprOperatorClass);
begin
  Inc(FTop);
  ReAlloc(True);
  FItems[Pred(FTop)] := AValue;
end;

//------------------------------------------------------------------------------

function TJclExprOperatorStack.Peek: TJclExprOperatorClass;
begin
  if Top = 0 then
    raise EJclExprOperatorStack.CreateResRec(@RsExprEvalStackEmpty);
  Result := FItems[Pred(Top)];
end;

//------------------------------------------------------------------------------

function TJclExprOperatorStack.Pop: TJclExprOperatorClass;
begin
  Result := Peek;
  Dec(FTop);
  ReAlloc(False);
  if (TargetList <> nil) and (Result <> TJclExprOperatorLParen) and
      (Result <> TJclExprOperatorRParen) then
    TargetList.Add(Result);
end;

procedure TJclExprOperatorStack.Clear;
begin
  FTop := 0;
  ReAlloc(False);
end;

//------------------------------------------------------------------------------
// TJclAbstractExprEval
//------------------------------------------------------------------------------

function TJclAbstractExprEval.GetExpression: string;
begin
  Result := ExpressionList.AsString;
end;

//------------------------------------------------------------------------------

function TJclAbstractExprEval.GetVars: TJclExprVars;
begin
  Result := ExpressionList.VariableList;
end;

//------------------------------------------------------------------------------

procedure TJclAbstractExprEval.SetExpression(const AExpression: string);
begin
  ExpressionList.AsString := AExpression;
end;

//------------------------------------------------------------------------------

procedure TJclAbstractExprEval.SetExpressionList(const List: TJclExprList);
begin
  if List <> ExpressionList then
  begin
    if FOwnsList then
      ExpressionList.Free;
    FOwnsList := False;
    FExprList := List;
  end;
end;

//------------------------------------------------------------------------------

constructor TJclAbstractExprEval.Create;
begin
  inherited Create;
  FExprList := TJclExprList.Create;
  FOwnsList := True;
end;

//------------------------------------------------------------------------------

destructor TJclAbstractExprEval.Destroy;
begin
  if FOwnsList then
    FExprList.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// TJclPostFixExprEval
//------------------------------------------------------------------------------

procedure TJclPostFixExprEval.EvaluateOperator(
  const Operator: TJclExprOperatorClass; const Operands: TJclExprOperandStack);
var
  Op1: Extended;
  Op2: Extended;
  TmpItem: TJclExprItem;

begin
  if Operands.Top < 2 then
    raise EJclExprEval.CreateResRec(@RsExprEvalOperandExpected);
  TmpItem := Operands.Pop;
  case TmpItem.Kind of
    eikOperand:     Op2 := TmpItem.Operand;
    eikVariable:    Op2 := TmpItem.Variable.Value;
    eikExpression:  begin
                      with TJclPostFixExprEval.Create do
                      try
                        SetExpressionList(TmpItem.Expression);
                        Op2 := Evaluate;
                      finally
                        Free;
                      end;
                    end;
    else            raise EJclExprEval.CreateResRec(@RsExprEvalOperatorExpected);
  end;
  TmpItem := Operands.Pop;
  case TmpItem.Kind of
    eikOperand:     Op1 := TmpItem.Operand;
    eikVariable:    Op1 := TmpItem.Variable.Value;
    eikExpression:  begin
                      with TJclPostFixExprEval.Create do
                      try
                        SetExpressionList(TmpItem.Expression);
                        Op1 := Evaluate;
                      finally
                        Free;
                      end;
                    end;
    else            raise EJclExprEval.CreateResRec(@RsExprEvalOperatorExpected);
  end;
  Operands.Push(Operator.Evaluate(Op1, Op2));
end;

//------------------------------------------------------------------------------

function TJclPostFixExprEval.UpdateLeftPrecedence(const Index: Integer;
  const AExprList: TJclExprList): Boolean;
var
  CurOperator: TJclExprOperatorClass;

begin
  CurOperator := AExprList[Index].Operator;
  Result := (Index > 2) and (AExprList[Index - 2].Kind = eikOperator) and
    (AExprList[Index - 2].Operator.StackPrecedence <
      CurOperator.InputPrecedence);
  if Result then
  begin
    AExprList.Insert(Index, TJclExprOperatorRParen);
    AExprList.Insert(0, TJclExprOperatorLParen);
  end;
end;

//------------------------------------------------------------------------------

function TJclPostFixExprEval.UpdateRightPrecedence(const Index: Integer;
  const AExprList: TJclExprList): Boolean;
var
  CurOperator: TJclExprOperatorClass;

begin
  CurOperator := AExprList[Index].Operator;
  Result := (Index < (AExprList.Count - 2)) and
    (AExprList[Index + 2].Kind = eikOperator) and
    (AExprList[Index + 2].Operator.InputPrecedence <
      CurOperator.StackPrecedence);
  if Result then
  begin
    AExprList.Add(TJclExprOperatorRParen);
    AExprList.Insert(Succ(Index), TJclExprOperatorLParen);
  end;
end;

//------------------------------------------------------------------------------

function TJclPostFixExprEval.Evaluate: Extended;
var
  OperandStack: TJclExprOperandStack;
  I: Integer;
  EI: TJclExprItem;

begin
  Result := 0;
  OperandStack := TJclExprOperandStack.Create;
  try
    for I := 0 to Pred(ExpressionList.Count) do
    begin
      case ExpressionList[I].Kind of
        eikOperand:     OperandStack.Push(ExpressionList[I].Operand);
        eikVariable:    OperandStack.Push(ExpressionList[I].Variable.Value);
        eikOperator:    EvaluateOperator(ExpressionList[I].Operator,
                          OperandStack);
        eikExpression:  OperandStack.Push(ExpressionList[I].Expression);
        eikFunction:    ExpressionList[I].Func.Evaluate(OperandStack);
      end;
    end;
    if OperandStack.Top > 1 then
      raise EJclExprEval.CreateResRec(@RsExprEvalToomanyOperands);
    if OperandStack.Top < 1 then
      raise EJclExprEval.CreateResRec(@RsExprEvalOperandExpected);
    EI := OperandStack.Pop;
    case EI.Kind of
      eikOperand:
        Result := EI.Operand;
      eikExpression:
        begin
          with TJclInFixExprEval.Create do
          try
            SetExpressionList(EI.Expression);
            Result := Evaluate;
          finally
            Free;
          end;
        end;
      else
        raise EJclExprEval.CreateResRec(@RsExprEvalExprUnexpectedItem);
    end;
  finally
    OperandStack.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclPostFixExprEval.InFixList(const AExprList: TJclExprList);
var
  I: Integer;
  EI: TJclExprItem;
  Indices: array of Integer;
  VarIdx: Integer;
  OpIdx: Integer;

  procedure PushIndex(const Index: Integer);
  begin
    SetLength(Indices, Succ(Length(Indices)));
    Indices[High(Indices)] := Index;
  end;

  function PopIndex: Integer;
  begin
    Result := Indices[High(Indices)];
    SetLength(Indices, Pred(Length(Indices)));
  end;

  procedure UpdateIndices;
  var
    Idx: Integer;

  begin
    for Idx := 0 to High(Indices) do
      Indices[Idx] := Indices[Idx] + 1;
  end;

begin
  for I := 0 to Pred(ExpressionList.Count) do
  begin
    EI := ExpressionList[I];
    case EI.Kind of
      eikOperand:
        begin
          AExprList.Add(EI.Operand);
          PushIndex(Pred(AExprList.Count));
        end;
      eikVariable:
        begin
          VarIdx := AExprList.VariableList.IndexOf(EI.Variable.Name);
          if VarIdx = -1 then
          begin
            AExprList.VariableList.Add(EI.Variable.Name);
            VarIdx := AExprList.VariableList.IndexOf(EI.Variable.Name);
          end;
          AExprList.Add(AExprList.VariableList.Items[VarIdx]);
          PushIndex(Pred(AExprList.Count));
        end;
      eikOperator:
        begin
          if Length(Indices) < 2 then
            raise EJclExpression.CreateResRec(@RsExprEvalOperandExpected);
          OpIdx := PopIndex;
          AExprList.Insert(OpIdx, EI.Operator);
          if UpdateLeftPrecedence(OpIdx, AExprList) then
          begin
            UpdateIndices;
            Inc(OpIdx, 2);
          end;
          UpdateRightPrecedence(OpIdx, AExprList);
        end;
    end;
  end;  
end;

//------------------------------------------------------------------------------

function TJclPostFixExprEval.AsInFix: string;
var
  ExprList: TJclExprList;

begin
  ExprList := TJclExprList.Create;
  try
    InFixList(ExprList);
    Result := ExprList.AsString;
  finally
    ExprList.Free;
  end;
end;

//------------------------------------------------------------------------------
// TJclInFixExprEval
//------------------------------------------------------------------------------

procedure TJclInFixExprEval.HandleFunction(const ExprItem: TJclExprItem);
var
  ArgExpr: TJclExprList;
  ArgNum: Integer;
  ArrayNum: Integer;
  ArrayCount: Integer;
  Func: TJclExprFunctionClass;
  TmpEval: TJclInFixExprEval;
  ArrayItem: TJclExprList;
  ExprArray: Boolean;
  TargetList: TJclExprList;
  ConvertItem: TJclExprList;

begin
  TargetList := OperatorStack.TargetList;
  Func := ExprItem.Func;
  for ArgNum := 0 to Pred(ExprItem.Args.Count) do
  begin
    ArgExpr := ExprItem.Args[ArgNum].Expression;
    case Func.Arguments(ArgNum) of
      eakOperand:
        begin
          TmpEval := TJclInFixExprEval.Create;
          try
            TmpEval.SetExpressionList(ArgExpr);
            if TargetList <> nil then
              TmpEval.PostFixList(TargetList)
            else
              OperandStack.Push(TmpEval.Evaluate);
          finally
            TmpEval.Free;
          end;
        end;
      eakExpression:
        begin
          if TargetList <> nil then
          begin
            ConvertItem := TJclExprList.Create;
            try
              ConvertItem.SetVariableList(TargetList.VariableList);
              TmpEval := TJclInFixExprEval.Create;
              try
                TmpEval.SetExpressionList(ArgExpr);
                if TargetList <> nil then
                  TmpEval.PostFixList(ConvertItem)
                else
                  OperandStack.Push(TmpEval.Evaluate);
              finally
                TmpEval.Free;
              end;
              TargetList.Add(ConvertItem);
            except
              ConvertItem.Free;
              raise;
            end;
          end
          else
            OperandStack.Push(ArgExpr);
        end;
      eakOperandArray,
      eakExpressionArray:
        begin
          ExprArray := Func.Arguments(ArgNum) = eakExpressionArray;
          ArrayCount := ArgExpr.Count;
          for ArrayNum := 0 to Pred(ArrayCount) do
          begin
            ArrayItem := ArgExpr[ArrayNum].Expression;
            if ExprArray then
            begin
              if TargetList <> nil then
              begin
                ConvertItem := TJclExprList.Create;
                try
                  ConvertItem.SetVariableList(TargetList.VariableList);
                  TmpEval := TJclInFixExprEval.Create;
                  try
                    TmpEval.SetExpressionList(ArrayItem);
                    if TargetList <> nil then
                      TmpEval.PostFixList(ConvertItem)
                    else
                      OperandStack.Push(TmpEval.Evaluate);
                  finally
                    TmpEval.Free;
                  end;
                  TargetList.Add(ConvertItem);
                except
                  ConvertItem.Free;
                  raise;
                end;
              end
              else
                OperandStack.Push(ArrayItem);
            end
            else
            begin
              TmpEval := TJclInFixExprEval.Create;
              try
                TmpEval.SetExpressionList(ArrayItem);
                if TargetList <> nil then
                  TmpEval.PostFixList(TargetList)
                else
                  OperandStack.Push(TmpEval.Evaluate);
              finally
                TmpEval.Free;
              end;
            end;
          end;
          if TargetList <> nil then
            TargetList.Add(ArrayCount)
          else
            OperandStack.Push(ArrayCount);
        end;
    end;
  end;
  if TargetList <> nil then
  begin
    OperandStack.Push(0);
    TargetList.Add(Func);
  end
  else
    Func.Evaluate(OperandStack);
end;

//------------------------------------------------------------------------------

procedure TJclInFixExprEval.UpdateAndCheckRank(const ExprItem: TJclExprItem);
begin
  if ExprItem.Kind in [eikOperand, eikVariable, eikExpression, eikFunction] then
    Rank := Rank + 1
  else if ExprItem.Kind = eikOperator then
    Rank := Rank + ExprItem.Operator.Rank;
  if Rank > 1 then
    raise EJclExprEval.CreateResRec(@RsExprEvalOperatorExpected)
  else if Rank < 0 then
    raise EJclExprEval.CreateResRec(@RsExprEvalOperandExpected);
end;

//------------------------------------------------------------------------------

constructor TJclInFixExprEval.Create;
begin
  inherited Create;
  FOperandStack := TJclExprOperandStack.Create;
  FOperatorStack := TJclExprOperatorStack.Create;
end;

//------------------------------------------------------------------------------

destructor TJclInFixExprEval.Destroy;
begin
  FOperandStack.Free;
  FOperatorStack.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TJclInFixExprEval.Evaluate: Extended;
var
  I: Integer;
  EI: TJclExprItem;
  VarIdx: Integer;
  TargetList: TJclExprList;

begin
  OperandStack.Clear;
  OperatorStack.Clear;
  Rank := 0;
  TargetList := OperatorStack.TargetList;
  for I := 0 to Pred(ExpressionList.Count) do
  begin
    EI := ExpressionList[I];
    UpdateAndCheckRank(EI);
    case EI.Kind of
      eikOperand:
        begin
          OperandStack.Push(EI.Operand);
          if TargetList <> nil then
            TargetList.Add(EI.Operand);
        end;
      eikVariable:
        begin
          OperandStack.Push(EI.Variable.Value);
          if TargetList <> nil then
          begin
            VarIdx := TargetList.VariableList.IndexOf(EI.Variable.Name);
            if VarIdx <0 then
            begin
              TargetList.VariableList.Add(EI.Variable.Name);
              VarIdx := TargetList.VariableList.IndexOf(EI.Variable.Name);
            end;
            TargetList.Add(TargetList.VariableList.Items[VarIdx]);
          end;
        end;
      eikOperator:
        begin
          EI.Operator.Input(OperandStack, OperatorStack, TargetList <> nil);
        end;
      eikExpression:
        begin
          OperandStack.Push(EI.Expression);
          if TargetList <> nil then
            TargetList.Add(EI.Expression);
        end;
      eikFunction:
        HandleFunction(EI);
      else
        raise EJclExprEval.CreateResRec(@RsExprEvalExprUnexpectedItem);
    end;
  end;
  if Rank <> 1 then
    raise EJclExprEval.CreateResRec(@RsExprEvalOperandExpected);
  while (OperatorStack.Top > 0) do
    OperatorStack.Pop.Evaluate(OperandStack, TargetList <> nil);
  EI := OperandStack.Pop;
  case EI.Kind of
    eikOperand:
      Result := EI.Operand;
    eikExpression:
      begin
        with TJclInFixExprEval.Create do
        try
          SetExpressionList(EI.Expression);
          Result := Evaluate;
        finally
          Free;
        end;
      end;
    else
      raise EJclExprEval.CreateResRec(@RsExprEvalExprUnexpectedItem);
  end;
end;

//------------------------------------------------------------------------------

procedure TJclInFixExprEval.PostFixList(const AExprList: TJclExprList);
begin
  OperatorStack.SetTargetList(AExprList);
  try
    Evaluate;
  finally
    OperatorStack.SetTargetList(nil);
  end;
end;

//------------------------------------------------------------------------------

function TJclInFixExprEval.AsPostFix: string;
var
  ExprList: TJclExprList;

begin
  ExprList := TJclExprList.Create;
  try
    PostFixList(ExprList);
    Result := ExprList.AsString;
  finally
    ExprList.Free;
  end;
end;

//------------------------------------------------------------------------------
// TJclExprOperators
//------------------------------------------------------------------------------

function TJclExprOperators.GetItems(Index: Integer): TJclExprOperatorClass;
begin
  Result := TJclExprOperatorClass(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

procedure TJclExprOperators.PutItems(Index: Integer;
  Item: TJclExprOperatorClass);
begin
  inherited Items[Index] := Item;
end;

//------------------------------------------------------------------------------

function TJclExprOperators.Add(Item: TJclExprOperatorClass): Integer;
var
  I : Integer;

begin
  { Item should be inserted in descending order }
  I := Pred(Count);
  while (I > -1) and (Items[I].Symbol >= Item.Symbol) do
    Dec(I);
  Result := Succ(I);
  Insert(Result, Item);
end;

//------------------------------------------------------------------------------

function TJclExprOperators.IndexOf(Item: TJclExprOperatorClass): Integer;
begin
  Result := inherited IndexOf(Item);
end;

//------------------------------------------------------------------------------

function TJclExprOperators.Remove(Item: TJclExprOperatorClass): Integer;
begin
  Result := inherited Remove(Item);
end;

//------------------------------------------------------------------------------
// TJclExprOperatorBase
//------------------------------------------------------------------------------

class procedure TJclExprOperatorBase.Evaluate(
  const OpStack: TJclExprOperandStack; const IgnoreCalcErrors: Boolean = False);
var
  Op1: Extended;
  Op2: Extended;
  TmpItem: TJclExprItem;

begin
  if OpStack.Top < 2 then
    raise EJclExprEval.CreateResRec(@RsExprEvalOperandExpected);
  TmpItem := OpStack.Pop;
  case TmpItem.Kind of
    eikOperand:
      Op2 := TmpItem.Operand;
    eikVariable:
      Op2 := TmpItem.Variable.Value;
    eikExpression:
      begin
        with TJclInFixExprEval.Create do
        try
          SetExpressionList(TmpItem.Expression);
          Op2 := Evaluate;
        finally
          Free;
        end;
      end;
    else
      raise EJclExprEval.CreateResRec(@RsExprEvalOperandExpected);
  end;
  TmpItem := OpStack.Pop;
  case TmpItem.Kind of
    eikOperand:
      Op1 := TmpItem.Operand;
    eikVariable:
      Op1 := TmpItem.Variable.Value;
    eikExpression:
      begin
        with TJclInFixExprEval.Create do
        try
          SetExpressionList(TmpItem.Expression);
          Op1 := Evaluate;
        finally
          Free;
        end;
      end;
    else
      raise EJclExprEval.CreateResRec(@RsExprEvalOperandExpected);
  end;
  try
    Op1 := Evaluate(Op1, Op2);
  except
    if not IgnoreCalcErrors or (ExceptObject is EJclExpression) then
      raise;
  end;
  OpStack.Push(Op1);
end;

//------------------------------------------------------------------------------

class procedure TJclExprOperatorBase.Input(const Operands: TJclExprOperandStack;
  const Operators: TJclExprOperatorStack;
  const IgnoreCalcErrors: Boolean = False);
begin
  with Operators do
  begin
    while (Top > 0) and (Peek.StackPrecedence >= InputPrecedence) do
      Pop.Evaluate(Operands, IgnoreCalcErrors);
    Push(Self);
  end
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorBase.StackPrecedence: Smallint;
begin
  Result := InputPrecedence;
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorBase.Rank: Smallint;
begin
  Result := -1;
end;

//------------------------------------------------------------------------------
// TJclExprOperatorBasePrec1
//------------------------------------------------------------------------------

class function TJclExprOperatorBasePrec1.InputPrecedence: Smallint;
begin
  Result := 1;
end;

//------------------------------------------------------------------------------
// TJclExprOperatorBasePrec2
//------------------------------------------------------------------------------

class function TJclExprOperatorBasePrec2.InputPrecedence: Smallint;
begin
  Result := 2;
end;

//------------------------------------------------------------------------------
// TJclExprOperatorBasePrec3
//------------------------------------------------------------------------------

class function TJclExprOperatorBasePrec3.InputPrecedence: Smallint;
begin
  Result := 3;
end;

//------------------------------------------------------------------------------
// TJclExprOperatorBasePrec4
//------------------------------------------------------------------------------

class function TJclExprOperatorBasePrec4.InputPrecedence: Smallint;
begin
  Result := 4;
end;

//------------------------------------------------------------------------------
// Operators: TJclExprOperatorAdd
//------------------------------------------------------------------------------

class function TJclExprOperatorAdd.Symbol: string;
begin
  Result := '+';
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorAdd.Evaluate(const Op1, Op2: Extended): Extended;
begin
  Result := Op1 + Op2;
end;

//------------------------------------------------------------------------------
// Operators: TJclExprOperatorSub
//------------------------------------------------------------------------------

class function TJclExprOperatorSub.Symbol: string;
begin
  Result := '-';
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorSub.Evaluate(const Op1, Op2: Extended): Extended;
begin
  Result := Op1 - Op2;
end;

//------------------------------------------------------------------------------
// Operators: TJclExprOperatorMult
//------------------------------------------------------------------------------

class function TJclExprOperatorMult.Symbol: string;
begin
  Result := '*';
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorMult.Evaluate(const Op1,
  Op2: Extended): Extended;
begin
  Result := Op1 * Op2;
end;

//------------------------------------------------------------------------------
// Operators: TJclExprOperatorDiv
//------------------------------------------------------------------------------

class function TJclExprOperatorDiv.Symbol: string;
begin
  Result := '/';
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorDiv.Evaluate(const Op1, Op2: Extended): Extended;
begin
  Result := Op1 / Op2;
end;

//------------------------------------------------------------------------------
// Operators: TJclExprOperatorIDiv
//------------------------------------------------------------------------------

class function TJclExprOperatorIDiv.Symbol: string;
begin
  Result := 'div';
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorIDiv.Evaluate(const Op1,
  Op2: Extended): Extended;
begin
  Result := Trunc(Op1) div Trunc(Op2);
end;

//------------------------------------------------------------------------------
// Operators: TJclExprOperatorIMod
//------------------------------------------------------------------------------

class function TJclExprOperatorIMod.Symbol: string;
begin
  Result := 'mod';
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorIMod.Evaluate(const Op1,
  Op2: Extended): Extended;
begin
  Result := Trunc(Op1) mod Trunc(Op2);
end;

//------------------------------------------------------------------------------
// Operators: TJclExprOperatorExp
//------------------------------------------------------------------------------

class function TJclExprOperatorExp.Symbol: string;
begin
  Result := '^';
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorExp.Evaluate(const Op1,
  Op2: Extended): Extended;
begin
  Result := Power(Op1, Op2);
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorExp.InputPrecedence: Smallint;
begin
  Result := 5;
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorExp.StackPrecedence: Smallint;
begin
  Result := 4;
end;

//------------------------------------------------------------------------------
// Operators: TJclExprOperatorAnd
//------------------------------------------------------------------------------

class function TJclExprOperatorAnd.Symbol: string;
begin
  Result := 'and';
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorAnd.Evaluate(const Op1,
  Op2: Extended): Extended;
begin
  Result := Trunc(Op1) and Trunc(Op2);
end;

//------------------------------------------------------------------------------
// Operators: TJclExprOperatorOr
//------------------------------------------------------------------------------

class function TJclExprOperatorOr.Symbol: string;
begin
  Result := 'or';
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorOr.Evaluate(const Op1,
  Op2: Extended): Extended;
begin
  Result := Trunc(Op1) or Trunc(Op2);
end;

//------------------------------------------------------------------------------
// Operators: TJclExprOperatorXor
//------------------------------------------------------------------------------

class function TJclExprOperatorXor.Symbol: string;
begin
  Result := 'xor';
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorXor.Evaluate(const Op1,
  Op2: Extended): Extended;
begin
  Result := Trunc(Op1) xor Trunc(Op2);
end;

//------------------------------------------------------------------------------
// Operators: TJclExprOperatorShl
//------------------------------------------------------------------------------

class function TJclExprOperatorShl.Symbol: string;
begin
  Result := 'shl';
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorShl.Evaluate(const Op1,
  Op2: Extended): Extended;
begin
  Result := Trunc(Op1) shl Trunc(Op2);
end;

//------------------------------------------------------------------------------
// Operators: TJclExprOperatorShr
//------------------------------------------------------------------------------

class function TJclExprOperatorShr.Symbol: string;
begin
  Result := 'shr';
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorShr.Evaluate(const Op1,
  Op2: Extended): Extended;
begin
  Result := Trunc(Op1) shr Trunc(Op2);
end;

//------------------------------------------------------------------------------
// Operators: TJclExprOperatorLT
//------------------------------------------------------------------------------

class function TJclExprOperatorLT.Symbol: string;
begin
  Result := '<';
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorLT.Evaluate(const Op1,
  Op2: Extended): Extended;
begin
  if Op1 < Op2 then
    Result := 1
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
// Operators: TJclExprOperatorLE
//------------------------------------------------------------------------------

class function TJclExprOperatorLE.Symbol: string;
begin
  Result := '<=';
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorLE.Evaluate(const Op1,
  Op2: Extended): Extended;
begin
  if Op1 <= Op2 then
    Result := 1
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
// Operators: TJclExprOperatorEQ
//------------------------------------------------------------------------------

class function TJclExprOperatorEQ.Symbol: string;
begin
  Result := '=';
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorEQ.Evaluate(const Op1,
  Op2: Extended): Extended;
begin
  if Op1 = Op2 then
    Result := 1
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
// Operators: TJclExprOperatorGE
//------------------------------------------------------------------------------

class function TJclExprOperatorGE.Symbol: string;
begin
  Result := '>=';
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorGE.Evaluate(const Op1,
  Op2: Extended): Extended;
begin
  if Op1 >= Op2 then
    Result := 1
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
// Operators: TJclExprOperatorGT
//------------------------------------------------------------------------------

class function TJclExprOperatorGT.Symbol: string;
begin
  Result := '>';
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorGT.Evaluate(const Op1,
  Op2: Extended): Extended;
begin
  if Op1 > Op2 then
    Result := 1
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
// Operators: TJclExprOperatorNE
//------------------------------------------------------------------------------

class function TJclExprOperatorNE.Symbol: string;
begin
  Result := '<>';
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorNE.Evaluate(const Op1,
  Op2: Extended): Extended;
begin
  if Op1 <> Op2 then
    Result := 1
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
// Operators: TJclExprOperatorLParen
//------------------------------------------------------------------------------

class function TJclExprOperatorLParen.Symbol: string;
begin
  Result := '(';
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorLParen.Evaluate(const Op1,
  Op2: Extended): Extended;
begin
  raise EJclExprEval.CreateResRec(@RsExprEvalLeftParenthesis);
end;

//------------------------------------------------------------------------------

class procedure TJclExprOperatorLParen.Evaluate(
  const OpStack: TJclExprOperandStack; const IgnoreCalcErrors: Boolean = False);
begin
  raise EJclExprEval.CreateResRec(@RsExprEvalMissingRightParenthesis);
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorLParen.InputPrecedence: Smallint;
begin
  Result := 99;
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorLParen.StackPrecedence: Smallint;
begin
  Result := -99;
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorLParen.Rank: Smallint;
begin
  Result := 0;
end;

//------------------------------------------------------------------------------
// Operators: TJclExprOperatorRParen
//------------------------------------------------------------------------------

class function TJclExprOperatorRParen.Symbol: string;
begin
  Result := ')';
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorRParen.Evaluate(const Op1,
  Op2: Extended): Extended;
begin
  raise EJclExprEval.CreateResRec(@RsExprEvalRightParenthesis);
end;

//------------------------------------------------------------------------------

class procedure TJclExprOperatorRParen.Evaluate(
  const OpStack: TJclExprOperandStack; const IgnoreCalcErrors: Boolean = False);
begin
  raise EJclExprEval.CreateResRec(@RsExprEvalRightParenthesis);
end;

//------------------------------------------------------------------------------

class procedure TJclExprOperatorRParen.Input(
  const Operands: TJclExprOperandStack; const Operators: TJclExprOperatorStack;
  const IgnoreCalcErrors: Boolean = False);
begin
  inherited Input(Operands, Operators, IgnoreCalcErrors);
  if Operators.Top <= 1 then
     raise EJclExprEval.CreateResRec(@RsExprEvalMissingLeftParenthesis);
  Operators.Pop; // Remove right parenthesis
  Operators.Pop; // Remove left parenthesis
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorRParen.InputPrecedence: Smallint;
begin
  Result := -98;
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorRParen.StackPrecedence: Smallint;
begin
  Result := -98;
end;

//------------------------------------------------------------------------------

class function TJclExprOperatorRParen.Rank: Smallint;
begin
  Result := 0;
end;

//------------------------------------------------------------------------------
// TJclExprVars
//------------------------------------------------------------------------------

function TJclExprVars.GetCount: Integer;
begin
  Result := FItems.Count;
end;

//------------------------------------------------------------------------------

function TJclExprVars.GetItems(Index: Integer): TJclExprVar;
begin
  Result := TJclExprVar(FItems[Index]);
end;

//------------------------------------------------------------------------------

function TJclExprVars.GetValues(Name: string): Extended;
var
  I: Integer;

begin
  I := IndexOf(Name);
  if I > -1 then
    Result := Items[I].Value
  else
    raise EJclExprVars.CreateResRecFmt(@RsExprEvalUnknownVariable, [Name]);
end;

//------------------------------------------------------------------------------

procedure TJclExprVars.PutItems(Index: Integer; AVar: TJclExprVar);
begin
  FItems[Index] := AVar;
end;

//------------------------------------------------------------------------------

procedure TJclExprVars.SetValues(Name: string; Value: Extended);
var
  I: Integer;

begin
  I := IndexOf(Name);
  if I > -1 then
    Items[I].Value := Value
  else
    raise EJclExprVars.CreateResRecFmt(@RsExprEvalUnknownVariable, [Name]);
end;

//------------------------------------------------------------------------------

constructor TJclExprVars.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

//------------------------------------------------------------------------------

destructor TJclExprVars.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TJclExprVars.Add(const AName: string): TJclExprVar;
begin
  if IndexOf(AName) = -1 then
  begin
    Result := TJclExprVar.Create(Self, AName);
    try
      FItems.Add(Result);
    except
      Result.Free;
      raise;
    end;
  end
  else
    raise EJclExprVars.CreateResRecFmt(@RsExprEvalDuplicateVariable, [AName]);
end;

//------------------------------------------------------------------------------

procedure TJclExprVars.Add(const AVar: TJclExprVar);
begin
  if (IndexOf(AVar) = -1) and (IndexOf(AVar.Name) = -1) then
    FItems.Add(AVar)
  else
    raise EJclExprVars.CreateResRecFmt(@RsExprEvalDuplicateVariable, [AVar.Name]);
end;

//------------------------------------------------------------------------------

procedure TJclExprVars.Clear;
begin
  FItems.Clear;
end;

//------------------------------------------------------------------------------

procedure TJclExprVars.Delete(const AIndex: Integer);
begin
  FItems.Delete(AIndex);
end;

//------------------------------------------------------------------------------

procedure TJclExprVars.Delete(const AName: string);
var
  I: Integer;

begin
  I := IndexOf(AName);
  if I > -1 then
    FItems.Delete(I);
end;

//------------------------------------------------------------------------------

procedure TJclExprVars.Delete(const AVar: TJclExprVar);
var
  I: Integer;

begin
  I := IndexOf(AVar);
  if I > -1 then
    FItems.Delete(I);
end;

//------------------------------------------------------------------------------

function TJclExprVars.IndexOf(const AName: string): Integer;
begin
  Result := Pred(Count);
  while (Result >= 0) and not SameText(AName, Items[Result].Name) do
    Dec(Result);
end;

//------------------------------------------------------------------------------

function TJclExprVars.IndexOf(const AVar: TJclExprVar): Integer;
begin
  Result := FItems.IndexOf(AVar);
end;

//------------------------------------------------------------------------------

function TJclExprVars.Insert(const AIndex: Integer;
  const AName: string): TJclExprVar;
begin
  if IndexOf(AName) = -1 then
  begin
    Result := TJclExprVar.Create(Self, AName);
    try
      FItems.Insert(AIndex, Result);
    except
      Result.Free;
      raise;
    end;
  end
  else
    raise EJclExprVars.CreateResRecFmt(@RsExprEvalDuplicateVariable, [AName]);
end;

//------------------------------------------------------------------------------

procedure TJclExprVars.Insert(const AIndex: Integer; const AVar: TJclExprVar);
begin
  if (IndexOf(AVar) = -1) and (IndexOf(AVar.Name) = -1) then
    FItems.Insert(AIndex, AVar)
  else
    raise EJclExprVars.CreateResRecFmt(@RsExprEvalDuplicateVariable, [AVar.Name]);
end;

//------------------------------------------------------------------------------
// TJclExprVar
//------------------------------------------------------------------------------

constructor TJclExprVar.Create(const AOwner: TJclExprVars; const AName: string);
begin
  inherited Create;
  FOwner := AOwner;
  FName := AName;
  FValue := 0;
end;

//------------------------------------------------------------------------------

destructor TJclExprVar.Destroy;
begin
  if Owner <> nil then
    Owner.Delete(Self);
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// TJclExprFunctions
//------------------------------------------------------------------------------

function TJclExprFunctions.GetFunctions(
  const Name: string): TJclExprFunctionClass;
var
  I: Integer;
  
begin
  I := IndexOf(Name);
  if I > -1 then
    Result := Items[I]
  else
    raise EJclExprVars.CreateResRecFmt(@RsExprEvalUnknownFunction, [Name]);
end;

//------------------------------------------------------------------------------

function TJclExprFunctions.GetItems(
  const Index: Integer): TJclExprFunctionClass;
begin
  Result := TJclExprFunctionClass(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

procedure TJclExprFunctions.PutItems(const Index: Integer;
  const Item: TJclExprFunctionClass);
begin
  inherited Items[Index] := Item;
end;

//------------------------------------------------------------------------------

function TJclExprFunctions.Add(const Item: TJclExprFunctionClass): Integer;
begin
  Result := IndexOf(Item);
  if Result = -1 then
    Result := IndexOf(Item.Name);
  if Result = -1 then
    Result := inherited Add(Item);
end;

//------------------------------------------------------------------------------

function TJclExprFunctions.IndexOf(const Item: TJclExprFunctionClass): Integer;
begin
  Result := inherited IndexOf(Item);
end;

//------------------------------------------------------------------------------

function TJclExprFunctions.IndexOf(const Name: string): Integer;
begin
  Result := Pred(Count);
  while (Result >= 0) and not SameText(Name, Items[Result].Name) do
    Dec(Result);
end;

//------------------------------------------------------------------------------

function TJclExprFunctions.Remove(const Item: TJclExprFunctionClass): Integer;
begin
  Result := IndexOf(Item);
  if Result > -1 then
    Delete(Result); 
end;

//------------------------------------------------------------------------------
// TJclExprFunction
//------------------------------------------------------------------------------

class function TJclExprFunction.ArgumentCount: Integer;
begin
  Result := 1;
end;

//------------------------------------------------------------------------------

class function TJclExprFunction.Arguments(
  const Index: Integer): TJclExprArgKind;
begin
  if Index = 0 then
    Result := eakOperand
  else
    raise EListError.CreateResFmt(@SListIndexError, [Index]);
end;

//------------------------------------------------------------------------------

class function TJclExprFunction.Result: TJclExprArgKind;
begin
  Result := eakOperand;
end;

//------------------------------------------------------------------------------

class function TJclExprFunction.Name: string;
begin
  Result := ClassName;
  if SameText(Copy(Result, 1, Length('TJclExprFunc')), 'TJclExprFunc') then
    System.Delete(Result, 1, Length('TJclExprFunc'))
  else if SameText(Copy(Result, 1, Length('TJclEF')), 'TJclEF') then
    System.Delete(Result, 1, Length('TJclEF'))
  else if SameText(Copy(Result, 1, Length('TEF')), 'TEF') then
    System.Delete(Result, 1, Length('TEF'))
  else if SameText(Copy(Result, 1, Length('T')), 'T') then
    System.Delete(Result, 1, Length('T'));
end;

//------------------------------------------------------------------------------
// TJclEFAbs
//------------------------------------------------------------------------------

class procedure TJclEFAbs.Evaluate(const OperandStack: TJclExprOperandStack);
var
  OrgItem: TJclExprItem;

begin
  OrgItem := OperandStack.Pop;
  case OrgItem.Kind of
    eikOperand:
      OperandStack.Push(Abs(OrgItem.Operand));
    eikVariable:
      OperandStack.Push(Abs(OrgItem.Variable.Value));
    else
      raise EJclExprFuncs.CreateResRecFmt(@RsExprEvalInvalidArgument, [Name]);
  end;
end;

//------------------------------------------------------------------------------
// TJclEFMin
//------------------------------------------------------------------------------

class function TJclEFMin.Arguments(const Index: Integer): TJclExprArgKind;
begin
  if Index = 0 then
    Result := eakOperandArray
  else
    raise EListError.CreateResFmt(@SListIndexError, [Index]);
end;

//------------------------------------------------------------------------------

class procedure TJclEFMin.Evaluate(const OperandStack: TJclExprOperandStack);
var
  Cnt: Integer;
  Result: Extended;

begin
  Cnt := Trunc(OperandStack.PopOperand);
  if Cnt > 0 then
  begin
    Result := OperandStack.PopOperand;
    Dec(Cnt);
    while Cnt > 0 do
    begin
      if Result > OperandStack.PeekOperand then
        Result := OperandStack.PopOperand
      else
        OperandStack.PopOperand;
      Dec(Cnt);
    end;
  end
  else
    raise EJclExprFuncs.CreateResRecFmt(@RsExprEvalInvalidArrayCount, [Cnt, Name]);
  OperandStack.Push(Result)
end;

//------------------------------------------------------------------------------
// TJclEFMax
//------------------------------------------------------------------------------

class procedure TJclEFMax.Evaluate(const OperandStack: TJclExprOperandStack);
var
  Cnt: Integer;
  Result: Extended;

begin
  Cnt := Trunc(OperandStack.PopOperand);
  if Cnt > 0 then
  begin
    Result := OperandStack.PopOperand;
    Dec(Cnt);
    while Cnt > 0 do
    begin
      if Result < OperandStack.PeekOperand then
        Result := OperandStack.PopOperand
      else
        OperandStack.PopOperand;
      Dec(Cnt);
    end;
  end
  else
    raise EJclExprFuncs.CreateResRecFmt(@RsExprEvalInvalidArrayCount, [Cnt, Name]);
  OperandStack.Push(Result)
end;

//------------------------------------------------------------------------------
// TJclEFIf
//------------------------------------------------------------------------------

class function TJclEFIf.ArgumentCount: Integer;
begin
  Result := 3;
end;

//------------------------------------------------------------------------------

class function TJclEFIf.Arguments(const Index: Integer): TJclExprArgKind;
begin
  case Index of
    0:    Result := eakOperand;
    1, 2: Result := eakExpression;
    else  raise EListError.CreateResFmt(@SListIndexError, [Index]);
  end;
end;

//------------------------------------------------------------------------------

class function TJclEFIf.Result: TJclExprArgKind;
begin
  Result := eakExpression;
end;

//------------------------------------------------------------------------------

class procedure TJclEFIf.Evaluate(const OperandStack: TJclExprOperandStack);
var
  TrueExpr: TJclExprList;
  FalseExpr: TJclExprList;
  Condition: Extended;

begin
  FalseExpr := OperandStack.PopExpr;
  TrueExpr := OperandStack.PopExpr;
  Condition := OperandStack.PopOperand;
  if Condition = 0 then
    OperandStack.Push(FalseExpr)
  else
    OperandStack.Push(TrueExpr);
end;

//------------------------------------------------------------------------------
// TJclEFSum
//------------------------------------------------------------------------------

class function TJclEFSum.Arguments(const Index: Integer): TJclExprArgKind;
begin
  if Index = 0 then
    Result := eakOperandArray
  else
    raise EListError.CreateResFmt(@SListIndexError, [Index]);
end;

//------------------------------------------------------------------------------

class procedure TJclEFSum.Evaluate(const OperandStack: TJclExprOperandStack);
var
  Cnt: Integer;
  Result: Extended;
  
begin
  Cnt := Trunc(OperandStack.PopOperand);
  if Cnt > 0 then
  begin
    Result := OperandStack.PopOperand;
    Dec(Cnt);
    while Cnt > 0 do
    begin
      Result := Result + OperandStack.PopOperand;
      Dec(Cnt);
    end;
    OperandStack.Push(Result);
  end
  else
    raise EJclExprFuncs.CreateResRecFmt(@RsExprEvalInvalidArrayCount, [Cnt, Name]);
end;

class procedure TJclEFAvg.Evaluate(const OperandStack: TJclExprOperandStack);
var
  Cnt: Integer;
  Result: Extended;

begin
  Cnt := Trunc(OperandStack.PopOperand);
  OperandStack.Push(Cnt);
  inherited Evaluate(OperandStack);
  Result := OperandStack.PopOperand;
  Result := Result / Cnt;
  OperandStack.Push(Result);
end;

//------------------------------------------------------------------------------
// Operator registration list
//------------------------------------------------------------------------------

var
  OpList: TJclExprOperators;

function Operators: TJclExprOperators;
begin
  Result := OpList;
end;

//------------------------------------------------------------------------------
// Function registration list
//------------------------------------------------------------------------------

var
  FnList: TJclExprFunctions;

function Functions: TJclExprFunctions;
begin
  Result := FnList;
end;

//------------------------------------------------------------------------------
// register default operators
//------------------------------------------------------------------------------

procedure InitializeOperators;
begin
  OpList.Add(TJclExprOperatorAdd);
  OpList.Add(TJclExprOperatorSub);
  OpList.Add(TJclExprOperatorMult);
  OpList.Add(TJclExprOperatorDiv);
  OpList.Add(TJclExprOperatorIDiv);
  OpList.Add(TJclExprOperatorIMod);
  OpList.Add(TJclExprOperatorExp);
  OpList.Add(TJclExprOperatorAnd);
  OpList.Add(TJclExprOperatorOr);
  OpList.Add(TJclExprOperatorXor);
  OpList.Add(TJclExprOperatorShl);
  OpList.Add(TJclExprOperatorShr);
  OpList.Add(TJclExprOperatorLT);
  OpList.Add(TJclExprOperatorLE);
  OpList.Add(TJclExprOperatorEQ);
  OpList.Add(TJclExprOperatorGE);
  OpList.Add(TJclExprOperatorGT);
  OpList.Add(TJclExprOperatorNE);
  OpList.Add(TJclExprOperatorLParen);
  OpList.Add(TJclExprOperatorRParen);
end;

//------------------------------------------------------------------------------
// register default functions
//------------------------------------------------------------------------------

procedure InitializeFunctions;
begin
  FnList.Add(TJclEFAbs);
  FnList.Add(TJclEFMin);
  FnList.Add(TJclEFMax);
  FnList.Add(TJclEFIf);
  FnList.Add(TJclEFSum);
  FnList.Add(TJclEFAvg);
end;

//------------------------------------------------------------------------------
// initialize registration lists
//------------------------------------------------------------------------------

initialization
  OpList := TJclExprOperators.Create;
  InitializeOperators;
  FnList := TJclExprFunctions.Create;
  InitializeFunctions;

finalization
  FnList.Free;
  OpList.Free;
end.
