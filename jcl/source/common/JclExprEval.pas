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
{ The Original Code is JclExprEval.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains three expression evaluators, each tailored for different usage patterns. It   }
{ also contains the component objects, so that a customized expression evaluator can be assembled  }
{ relatively easily.                                                                               }
{                                                                                                  }
{ Unit owner: Barry Kelly                                                                          }
{ Last modified: June 14, 2001                                                                     }
{                                                                                                  }
{**************************************************************************************************}

{ Brief: This unit contains the expression evaluator.
  Description:
    The key classes are TEvaluator, TCompiledEvaluator and
    TExpressionCompiler.
    <p>
    * For single evaluations of multiple expressions, use TEvaluator.
    * For many evaluations of the same expression, use TCompiledEvaluator.
    * For many evaluations of many expressions, use TExpressionCompiler.
    <p>
    Customized evaluators can be put together from constituent parts. }

unit JclExprEval;

{$I jcl.inc}

{$WEAKPACKAGEUNIT ON}

interface

uses
  SysUtils, Classes,
  JclBase, JclSysUtils, JclStrHashMap, JclResources;

const
  { Brief: Initial size of internal hash lists created by TEasyEvaluator
      descendants, and also TExpressionCompiler. }
  C_ExprEval_HashSize = 127;

type
  { Brief: Exception class used by the expression evaluator. }
  EJclExprEvalError = class(EJclError);

const
  { Brief: Set of characters that will be skipped before a token read
    commences.
    See Also: TExprLexer }
  ExprWhiteSpace = [#1..#32];

type
  { Brief: Floating-point type used by TLexer and TParser.Evaluate. }
  TFloat = Double;
  { Brief: Pointer to TFloat. }
  PFloat = ^TFloat;

  { Brief: 32-bit IEEE standard floating-point value. }
  TFloat32 = Single;
  { <COMBINE TFloat32> }
  PFloat32 = ^TFloat32;

  { Brief: 64-bit IEEE standard floating-point value. }
  TFloat64 = Double;
  { <COMBINE TFloat64> }
  PFloat64 = ^TFloat64;

  { Brief: 80-bit Intel extended precision floating-point value. }
  TFloat80 = Extended;
  { <COMBINE TFloat80> }
  PFloat80 = ^TFloat80;

  { Brief: A function type taking no parameters. }
  TFloatFunc = function: TFloat;
  { <COMBINE TFloatFunc> }
  TFloat32Func = function: TFloat32;
  { <COMBINE TFloatFunc> }
  TFloat64Func = function: TFloat64;
  { <COMBINE TFloatFunc> }
  TFloat80Func = function: TFloat80;

  { Brief: A function type taking a single parameter. }
  TUnaryFunc = function(x: TFloat): TFloat;
  { <COMBINE TUnaryFunc> }
  TUnary32Func = function(x: TFloat32): TFloat32;
  { <COMBINE TUnaryFunc> }
  TUnary64Func = function(x: TFloat64): TFloat64;
  { <COMBINE TUnaryFunc> }
  TUnary80Func = function(x: TFloat80): TFloat80;

  { Brief: A function type taking two parameters. }
  TBinaryFunc = function(x, y: TFloat): TFloat;
  { <COMBINE TBinaryFunc> }
  TBinary32Func = function(x, y: TFloat32): TFloat32;
  { <COMBINE TBinaryFunc> }
  TBinary64Func = function(x, y: TFloat64): TFloat64;
  { <COMBINE TBinaryFunc> }
  TBinary80Func = function(x, y: TFloat80): TFloat80;

  { Brief: A function type taking three parameters. }
  TTernaryFunc = function(x, y, z: TFloat): TFloat;
  { <COMBINE TTernaryFunc> }
  TTernary32Func = function(x, y, z: TFloat32): TFloat32;
  { <COMBINE TTernaryFunc> }
  TTernary64Func = function(x, y, z: TFloat64): TFloat64;
  { <COMBINE TTernaryFunc> }
  TTernary80Func = function(x, y, z: TFloat80): TFloat80;

type
{ Forward Declarations }
  TExprLexer = class;
  TExprCompileParser = class;
  TExprEvalParser = class;
  TExprSym = class;
  TExprNode = class;
  TExprNodeFactory = class;

  { Brief: Finds a symbol corresponding to an identifier.
    Description:
      Expressions composed solely of numbers and operators may be
      evaluated quite easily, but to make an expression evaluator really
      useful requires that things like named constants, variables and
      functions be added as well.
      <p>
      To allow a defined set of constants, variables and functions
      to be used in multiple evaluators (and mixed and matched according
      to need), the task of handling symbol resolution is devolved to
      an object known as a <i>context</i>.
      <p>
      Contexts are not required to be flat; indeed, they are expected to
      be compound objects, which devolve to other, more specialized contexts
      to do actual resolution.
      <p>
      As an example, consider the way names are resolved in Object Pascal.
      The set of valid symbols is initially defined by the System unit, and
      then added to with each unit named in the uses clause. When a method
      is being compiled, private, protected and public names come into the
      namespace; they aren't in effect with methods of other classes. In
      a similar way, contexts can be built up according to requirements.
    Note: Don't construct instances of TExprContext directly, since it is
      abstract; instead, construct a concrete descendant.
    See Also:
      TExprHashContext, TExprSetContext }
  TExprContext = class
  public
    { Finds a symbol corresponding to an identifier.
      Parameters: s: The identifier of the symbol to find.
      Returns: The symbol object, or nil if not found. }
    function Find(const AName: string): TExprSym; virtual; abstract;
  end;

  { Brief: A context class that uses a hash map for its implementation.
    Description:
      This is a concrete context class that uses a hash map for symbol
      lookup. The fact that it uses a hash for its implementation means that
      symbol lookup takes a constant time, i.e. it is independant of the
      actual number of symbols stored in the context; however, this
      guarantee depends on the internal hash map's internal table being
      large enough to minimize collisions.
      <p>
    Note: Case sensitivity is also a concern; see the Create method. }
  TExprHashContext = class(TExprContext)
  private
    FHashMap: TStringHashMap;
  public
    { Brief: Creates an instance of THashContext.
      Parameters:
        ACaseSensitive: Whether this context should be case sensitive or not.
        AHashSize: The hash size to pass on to the internal
          hash map structure.
      Description:
        This constructs an instance of THashContext. The first
        parameter indicates case sensitivity; the meaning of the second
        parameter is slightly more subtle. It tells the context what
        value to pass on to the internal hash map's constructor. A large
        value will mean that the context will take up quite a bit of memory
        even when it is empty (4 bytes are added for every increase of 1
        in the hash size). For good performance, the hash should roughly
        be the expected average amount of items to be held in the hash.
        The performance degradation for too small hash tables is logarithmic,
        however, so only in pathalogical cases should this be a concern.
      Note: It is fairly important that the hash size not be an even
        number; it's best if it is a prime number, although typically
        a power of 2 minus one does fairly well (e.g. 127, 2047, etc.).
      See Also: TStringHashMap }
    constructor Create(
      ACaseSensitive: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF};
      AHashSize: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = 127{$ENDIF});

    { Brief: Destroys this instance. Use Free instead. }
    destructor Destroy; override;

    { Brief: Adds a symbol to this context.
      Parameters:
        ASymbol: The symbol object to add.
      Description:
        Once a symbol has been added, the context takes over ownership
        of it, and will free it when it is itself destroyed. }
    procedure Add(ASymbol: TExprSym);

    { Brief: Removes a symbol from this context.
      Parameters:
        AName: symbol to remove and free.
      Description:
        The symbol object refered to by AName will be destroyed. }
    procedure Remove(const AName: string);

    function Find(const AName: string): TExprSym; override;
  end;

  { Brief: A compound context object for combining multiple contexts.
    Description:
      A context class that contains a set of other contexts, which it
      searches in order, starting with the most recently added. }
  TExprSetContext = class(TExprContext)
  private
    FList: TList;
    FOwnsContexts: Boolean;
    function GetContexts(AIndex: Integer): TExprContext;
    function GetCount: Integer;
  public
    { Brief: Constructs an instance.
      Parameters:
        AOwnsContexts: Determines whether this context object
          should free contexts when they are deleted. You can use
          Extract to remove a context while keeping it intact. }
    constructor Create(AOwnsContexts: Boolean);

    { Brief: Destroys the TExprSetContext instance. Use Free instead. }
    destructor Destroy; override;

    { Brief: Adds a context to the set.
      Parameters:
        AContext: Context to add to the set of contexts. }
    procedure Add(AContext: TExprContext);
    { Brief: Removes a context.
      Parameters:
        AContext: Context to remove. }
    procedure Remove(AContext: TExprContext);
    { Brief: Removes a context by index.
      Parameters:
        AIndex: Index of context to remove. }
    procedure Delete(AIndex: Integer);
    { Brief: Removes a context without freeing it (if AOwnsContexts was
        passed as True in the constructor Create).
      Parameters:
        AContext: Context to remove.
      Returns:
        The context passed in (AContext). }
    function Extract(AContext: TExprContext): TExprContext;

    { Brief: Returns the number of contexts held by this set. }
    property Count: Integer read GetCount;
    { Brief: Accesses an internal context by index. }
    property Contexts[AIndex: Integer]: TExprContext read GetContexts;

    { Brief: Access to the internal list of contexts for advanced
        operations. }
    property InternalList: TList read FList;

    function Find(const AName: string): TExprSym; override;
  end;

  { Brief: Represents a symbol that may be found in an input stream.
    Description:
      An instance of this class is responsible for turning input lexemes
      (from the lexer) into either a concrete result (Evaluate) or
      expression nodes (Compile).
      <p>
      Because this class has state (properties like Lexer, EvalParser,
      CompileParser, NodeFactory), <b>a symbol should never be used by
      two threads simultaneously</b>. This design decision was taken to
      maximize ease of creating new symbols.
      <p>
      It is, however, safe to implement recursive symbols (i.e. a symbol
      that calls itself, even though it might be through another evaluator
      with different Lexer etc.) because the state is saved before Evaluate
      or Compile is called, and restored afterwards. }
  TExprSym = class
  private
    FIdent: string;
    FLexer: TExprLexer;
    FEvalParser: TExprEvalParser;
    FCompileParser: TExprCompileParser;
    FNodeFactory: TExprNodeFactory;
  public
    { Brief: Constructs a new instance.
      Parameters:
        AIdent: Identifier token that should trigger this symbol. }
    constructor Create(const AIdent: string);

    { Brief: Evaluates this symbol.
      Description:
        This method is called by an evaluating Parser when it finds this
        symbol in its stream. It should call methods of the lexer and parser
        and ultimately return a value that represents the value of this
        symbol. The Lexer is positioned at the first token following the
        symbol.
      See Also:
        TExprSym.Compile, TExprSym.Lexer, TExprSym.EvalParser }
    function Evaluate: TFloat; virtual; abstract;

    { Brief: Compiles this symbol.
      Description:
        This method is called by a compiling Parser when it finds this symbol
        in its stream. It should call methods of the lexer and parser
        and ultimately return an expression node that contains all the
        necessary state to evaluate this symbol at expression evaluation
        time. The Lexer is positioned at the first token following the
        symbol.
      See Also:
        TExprNode, TExprSym.Evaluate, TExprSym.Lexer, TExprSym.CompileParser }
    function Compile: TExprNode; virtual; abstract;

    { Brief: Identifier token that should trigger this symbol. }
    property Ident: string read FIdent;

    { Brief: The lexical analyser that found this symbol in its stream.
      Description:
        This property is set by the parser when it finds this symbol in the
        input stream. This is done so that the Evaluate and Compile methods
        can perform things like reading parameters etc., when they work
        out their context.
      See Also: TExprSym.EvalParser, TExprSym.CompileParser,
        TExprSym.NodeFactory, TExprLexer }
    property Lexer: TExprLexer read FLexer write FLexer;

    { Brief: The compile parser that found this symbol in its stream.
      Description:
        This property is set by a compiling parser when it finds this
        symbol in its input stream. This is done so that the Compile method
        can perform things like reading parameters etc., when it gathers
        sufficient information for compilation.
      See Also: TExprSym.EvalParser, TExprSym.Lexer, TExprSym.NodeFactory,
        TExprCompileParser }
    property CompileParser: TExprCompileParser read FCompileParser
      write FCompileParser;

    { Brief: The evaluation parser that found this symbol in its stream.
      Description:
        This property is set by an evaluating parser when it finds this
        symbol in its input stream. This is done so that the Evaluate method
        can perform things like reading parameters etc., when it gathers
        sufficient information for evaluation.
      See Also: TExprSym.CompileParser, TExprSym.Lexer, TExprSym.NodeFactory,
        TExprEvalParser }
    property EvalParser: TExprEvalParser read FEvalParser write FEvalParser;

    { Brief: The expression node factory object for creating expression
        node instances.
      Description:
        This property is set by a compiling parser when it finds this symbol
        in its input stream. It is set to nil for an evaluating stream, so
        it is only valid to use this property in the Compile method. It is
        should be used to construct expression nodes with sufficient state
        to calculate the value of this symbol at expression evaluation time.
      See Also: TExprSym.Lexer, TExprSym.CompileParser, TExprSym.Compile,
        TExprNodeFactory }
    property NodeFactory: TExprNodeFactory read FNodeFactory write FNodeFactory;
  end;

  { Brief: The type of token found by TExprLexer. }
  TExprToken = (

    // specials
    etEof,
    etNumber,
    etIdentifier,

    // user extension tokens
    etUser0, etUser1, etUser2, etUser3, etUser4, etUser5, etUser6, etUser7,
    etUser8, etUser9, etUser10, etUser11, etUser12, etUser13, etUser14, etUser15,
    etUser16, etUser17, etUser18, etUser19, etUser20, etUser21, etUser22, etUser23,
    etUser24, etUser25, etUser26, etUser27, etUser28, etUser29, etUser30, etUser31,

    // compound tokens
    etNotEqual, // <>
    etLessEqual, // <=
    etGreaterEqual, // >=

    // ASCII normal & ordinals

    etBang, // '!' #$21 33
    etDoubleQuote, // '"' #$22 34
    etHash, // '#' #$23 35
    etDollar, // '$' #$24 36
    etPercent, // '%' #$25 37
    etAmpersand, // '&' #$26 38
    etSingleQuote, // '''' #$27 39
    etLParen, // '(' #$28 40
    etRParen, // ')' #$29 41
    etAsterisk, // '*' #$2A 42
    etPlus, // '+' #$2B 43
    etComma, // ',' #$2C 44
    etMinus, // '-' #$2D 45
    etDot, // '.' #$2E 46
    etForwardSlash, // '/' #$2F 47

    // 48..57 - numbers...

    etColon, // ':' #$3A 58
    etSemiColon, // ';' #$3B 59
    etLessThan, // '<' #$3C 60
    etEqualTo, // '=' #$3D 61
    etGreaterThan, // '>' #$3E 62
    etQuestion, // '?' #$3F 63
    etAt, // '@' #$40 64

    // 65..90 - capital letters...

    etLBracket, // '[' #$5B 91
    etBackSlash, // '\' #$5C 92
    etRBracket, // ']' #$5D 93
    etArrow, // '^' #$5E 94
    // 95 - underscore
    etBackTick, // '`' #$60 96

    // 97..122 - small letters...

    etLBrace, // '{' #$7B 123
    etPipe, // '|' #$7C 124
    etRBrace, // '}' #$7D 125
    etTilde, // '~' #$7E 126
    et127, // '' #$7F 127
    etEuro, // 'Ä' #$80 128
    et129, // 'Å' #$81 129
    et130, // 'Ç' #$82 130
    et131, // 'É' #$83 131
    et132, // 'Ñ' #$84 132
    et133, // 'Ö' #$85 133
    et134, // 'Ü' #$86 134
    et135, // 'á' #$87 135
    et136, // 'à' #$88 136
    et137, // 'â' #$89 137
    et138, // 'ä' #$8A 138
    et139, // 'ã' #$8B 139
    et140, // 'å' #$8C 140
    et141, // 'ç' #$8D 141
    et142, // 'é' #$8E 142
    et143, // 'è' #$8F 143
    et144, // 'ê' #$90 144
    et145, // 'ë' #$91 145
    et146, // 'í' #$92 146
    et147, // 'ì' #$93 147
    et148, // 'î' #$94 148
    et149, // 'ï' #$95 149
    et150, // 'ñ' #$96 150
    et151, // 'ó' #$97 151
    et152, // 'ò' #$98 152
    et153, // 'ô' #$99 153
    et154, // 'ö' #$9A 154
    et155, // 'õ' #$9B 155
    et156, // 'ú' #$9C 156
    et157, // 'ù' #$9D 157
    et158, // 'û' #$9E 158
    et159, // 'ü' #$9F 159
    et160, // '†' #$A0 160
    et161, // '°' #$A1 161
    et162, // '¢' #$A2 162
    et163, // '£' #$A3 163
    et164, // '§' #$A4 164
    et165, // '•' #$A5 165
    et166, // '¶' #$A6 166
    et167, // 'ß' #$A7 167
    et168, // '®' #$A8 168
    et169, // '©' #$A9 169
    et170, // '™' #$AA 170
    et171, // '´' #$AB 171
    et172, // '¨' #$AC 172
    et173, // '≠' #$AD 173
    et174, // 'Æ' #$AE 174
    et175, // 'Ø' #$AF 175
    et176, // '∞' #$B0 176
    et177, // '±' #$B1 177
    et178, // '≤' #$B2 178
    et179, // '≥' #$B3 179
    et180, // '¥' #$B4 180
    et181, // 'µ' #$B5 181
    et182, // '∂' #$B6 182
    et183, // '∑' #$B7 183
    et184, // '∏' #$B8 184
    et185, // 'π' #$B9 185
    et186, // '∫' #$BA 186
    et187, // 'ª' #$BB 187
    et188, // 'º' #$BC 188
    et189, // 'Ω' #$BD 189
    et190, // 'æ' #$BE 190
    et191, // 'ø' #$BF 191
    et192, // '¿' #$C0 192
    et193, // '¡' #$C1 193
    et194, // '¬' #$C2 194
    et195, // '√' #$C3 195
    et196, // 'ƒ' #$C4 196
    et197, // '≈' #$C5 197
    et198, // '∆' #$C6 198
    et199, // '«' #$C7 199
    et200, // '»' #$C8 200
    et201, // '…' #$C9 201
    et202, // ' ' #$CA 202
    et203, // 'À' #$CB 203
    et204, // 'Ã' #$CC 204
    et205, // 'Õ' #$CD 205
    et206, // 'Œ' #$CE 206
    et207, // 'œ' #$CF 207
    et208, // '–' #$D0 208
    et209, // '—' #$D1 209
    et210, // '“' #$D2 210
    et211, // '”' #$D3 211
    et212, // '‘' #$D4 212
    et213, // '’' #$D5 213
    et214, // '÷' #$D6 214
    et215, // '◊' #$D7 215
    et216, // 'ÿ' #$D8 216
    et217, // 'Ÿ' #$D9 217
    et218, // '⁄' #$DA 218
    et219, // '€' #$DB 219
    et220, // '‹' #$DC 220
    et221, // '›' #$DD 221
    et222, // 'ﬁ' #$DE 222
    et223, // 'ﬂ' #$DF 223
    et224, // '‡' #$E0 224
    et225, // '·' #$E1 225
    et226, // '‚' #$E2 226
    et227, // '„' #$E3 227
    et228, // '‰' #$E4 228
    et229, // 'Â' #$E5 229
    et230, // 'Ê' #$E6 230
    et231, // 'Á' #$E7 231
    et232, // 'Ë' #$E8 232
    et233, // 'È' #$E9 233
    et234, // 'Í' #$EA 234
    et235, // 'Î' #$EB 235
    et236, // 'Ï' #$EC 236
    et237, // 'Ì' #$ED 237
    et238, // 'Ó' #$EE 238
    et239, // 'Ô' #$EF 239
    et240, // '' #$F0 240
    et241, // 'Ò' #$F1 241
    et242, // 'Ú' #$F2 242
    et243, // 'Û' #$F3 243
    et244, // 'Ù' #$F4 244
    et245, // 'ı' #$F5 245
    et246, // 'ˆ' #$F6 246
    et247, // '˜' #$F7 247
    et248, // '¯' #$F8 248
    et249, // '˘' #$F9 249
    et250, // '˙' #$FA 250
    et251, // '˚' #$FB 251
    et252, // '¸' #$FC 252
    et253, // '˝' #$FD 253
    et254, // '˛' #$FE 254
    et255, // 'ˇ' #$FF 255
    etInvalid // invalid token type
  );

  { Brief: A lexical analyser.
    Description:
      An object of this class breaks up an input stream into lexemes -
      that is, it breaks down the input stream into tokens with two
      properties: type and content. The type describes what sort of
      token the current token is. The content gives further information
      about some tokens.

      <p>For example, if the incoming stream is 'a + 03.60', then the lexical
      analyser will break it down into 3 tokens:
        * type (CurrTok): etIdentifier; content: TokenAsString = 'a'
        * type: etPlus; content: n/a
        * type: etNumber; content: TokenAsNumber = 3.6,
          also TokenAsString = '03.60'

      The token type is given by the CurrTok property, and the token
        content is given by the TokenAsString and TokenAsNumber properties.

      <p>The current token is skipped and the next token loaded when the
        NextTok method is called. When the end of the input stream is
        found, CurrTok will be equal to etEof, and repeated calls of
        NextTok won't do anything (i.e. CurrTok will remain equal to etEof). }
  TExprLexer = class
  protected
    { Brief: NextTok should set as appropriate. }
    FCurrTok: TExprToken;
    { Brief: NextTok should set as appropriate. }
    FTokenAsNumber: TFloat;
    { Brief: NextTok should set as appropriate. }
    FTokenAsString: string;
  public
    { Brief: Constructs an instance and calls Reset. }
    constructor Create;

    { Brief: Skips the current token and gets the next token.
      Description:
        This method is called by Reset (and thus implicitly by Create), so
        it doesn't need to be called to get the first token in the stream.

        <p>This method does the following jobs:
          * Skips whitespace
          * Determines token type from the first character after
            whitespace has been skipped
          * Reads in the rest of that token, based on the first character,
            possibly refining the token type based on further characters.
            For example, if '<' is read on the input stream, the token could
            be etLessThan, etNotEqual ('<>') or etLessEqual ('<=').
        After a call to this method, CurrTok will give the current token
        type, and TokenAsString and TokenAsNumber will give extra
        information as appropriate for the token type. }
    procedure NextTok; virtual; abstract;

    { Brief: Resets the position of the lexer to the start of its
        input stream.
      Description:
        Overridden implementations should call this at the <b>end</b> of
        their implementations, because it calls NextTok by default. }
    procedure Reset; virtual;

    { Brief: String information about the current token if that is
        appropriate.
      Description:
        This property is usually valid for tokens that don't have a fixed
        length, like etIdentifier and etNumber. It contains the text as
        found in the source stream, possibly after a little preprocessing
        (for instance, if the lexer supported strings, then this could
        return the string with control characters expanded). It is set by
        NextTok.
      See Also: NextTok, TokenAsNumber, CurrTok }
    property TokenAsString: string read FTokenAsString;

    { Brief: Number information about the current token if that is
        appropriate.
      Description:
        This property is usually only valid for well-formed integer or
        floating-point numbers found in the source text. It is set by
        NextTok.
      See Also: NextTok, TokenAsString, CurrTok }
    property TokenAsNumber: TFloat read FTokenAsNumber;

    { Brief: The current token type.
      Description:
        This contains the type of the token just read by NextTok.
      See Also: NextTok, TokenAsString, TokenAsNumber }
    property CurrTok: TExprToken read FCurrTok;
  end;

  { Brief: A node in an expression DAG (directed acyclic graph).
    Description:
      This is the abstract object from which all expression DAG nodes
      descend. TExprNodeFactory has responsibility for constructing
      the correct class and acts as a container for TExprNode descendant
      instances.
    Note: Don't construct TExprNode objects directly; call the methods
      of a TExprNodeFactory instance. }
  TExprNode = class
  private
    FDepList: TList;
    function GetDepCount: Integer;
    function GetDeps(AIndex: Integer): TExprNode;
  public
    { Brief: Constructs an instance.
      Parameters:
        ADepList: A list of dependancies, nodes this expression node
          depends on.
      Description:
        The dependancy list passed into this constructor should be the
        <b>direct</b> dependancies of this node. }
    constructor Create(const ADepList: array of TExprNode);
    { Brief: Destroys an instance. Use Free instead. }
    destructor Destroy; override;
    { Brief: Adds a dependancy.
      Parameters:
        ADep: Dependancy to add.
      Description:
        This method adds a dependancy to this node. }
    procedure AddDep(ADep: TExprNode);
    { Brief: Number of dependancies this node has. }
    property DepCount: Integer read GetDepCount;
    { Brief: Accesses a dependancy based on index. }
    property Deps[AIndex: Integer]: TExprNode read GetDeps; default;
    { Brief: Access to the internal dependancy list for advanced
      optimization strategies. }
    property DepList: TList read FDepList;
  end;

  { Brief: A factory class for TExprNode objects.
    Description:
      When compiling an expression, the expression must be broken down
      into 'atomic' components, like add, subtract, load constant,
      load variable, call function etc. Because different compilation
      strategies may involve different atomic node classes, the task
      of atomic node construction is given to a separate factory object.
      This object should keep a list of all the nodes it has constructed,
      and act as an interface for the construction of new nodes.
      <p>
      Concrete descendants of this factory should have a GenCode method
      that is specific to the implementation strategy. That GenCode method
      is a prime candidate for expression optimizations like common
      sub-expression elimination, constant sub-expression evaluation, etc. }
  TExprNodeFactory = class
  public
    { Brief: Loads the variable pointed to by ALoc.
      Description:
        Generates a node that will load a variable from a pointer. The
        type is important, because variables of different types are of
        different sizes and have different formats.
      Parameters:
        ALoc: Location of the variable to load. }
    function LoadVar32(ALoc: PFloat32): TExprNode; virtual; abstract;
    { <COMBINE LoadVar32> }
    function LoadVar64(ALoc: PFloat64): TExprNode; virtual; abstract;
    { <COMBINE LoadVar32> }
    function LoadVar80(ALoc: PFloat80): TExprNode; virtual; abstract;

    { Brief: Loads a constant value.
      Description:
        Generates a node that will load a constant value. The type is
        important because less precision will mean faster evaluation.
      Parameters:
        AValue: The value to load. }
    function LoadConst32(AValue: TFloat32): TExprNode; virtual; abstract;
    { <COMBINE LoadConst32> }
    function LoadConst64(AValue: TFloat64): TExprNode; virtual; abstract;
    { <COMBINE LoadConst32> }
    function LoadConst80(AValue: TFloat80): TExprNode; virtual; abstract;

    { Brief: Calls a function.
      Description:
        Generates a node that will call a function, possibly with
        parameters. There are 4 basic types of functions directly supported
        here: no parameters (Float*), 1 parameter (Unary*), 2 parameters
        (Binary*) and 3 parameters (Ternary*). }
    function CallFloatFunc(AFunc: TFloatFunc): TExprNode; virtual; abstract;
    { <COMBINE CallFloatFunc> }
    function CallFloat32Func(AFunc: TFloat32Func): TExprNode; virtual; abstract;
    { <COMBINE CallFloatFunc> }
    function CallFloat64Func(AFunc: TFloat64Func): TExprNode; virtual; abstract;
    { <COMBINE CallFloatFunc> }
    function CallFloat80Func(AFunc: TFloat80Func): TExprNode; virtual; abstract;
    { <COMBINE CallFloatFunc> }
    function CallUnaryFunc(AFunc: TUnaryFunc; x: TExprNode): TExprNode; virtual; abstract;
    { <COMBINE CallFloatFunc> }
    function CallUnary32Func(AFunc: TUnary32Func; x: TExprNode): TExprNode; virtual; abstract;
    { <COMBINE CallFloatFunc> }
    function CallUnary64Func(AFunc: TUnary64Func; x: TExprNode): TExprNode; virtual; abstract;
    { <COMBINE CallFloatFunc> }
    function CallUnary80Func(AFunc: TUnary80Func; x: TExprNode): TExprNode; virtual; abstract;
    { <COMBINE CallFloatFunc> }
    function CallBinaryFunc(AFunc: TBinaryFunc; x, y: TExprNode): TExprNode; virtual; abstract;
    { <COMBINE CallFloatFunc> }
    function CallBinary32Func(AFunc: TBinary32Func; x, y: TExprNode): TExprNode; virtual; abstract;
    { <COMBINE CallFloatFunc> }
    function CallBinary64Func(AFunc: TBinary64Func; x, y: TExprNode): TExprNode; virtual; abstract;
    { <COMBINE CallFloatFunc> }
    function CallBinary80Func(AFunc: TBinary80Func; x, y: TExprNode): TExprNode; virtual; abstract;
    { <COMBINE CallFloatFunc> }
    function CallTernaryFunc(AFunc: TTernaryFunc; x, y, z: TExprNode): TExprNode; virtual; abstract;
    { <COMBINE CallFloatFunc> }
    function CallTernary32Func(AFunc: TTernary32Func; x, y, z: TExprNode): TExprNode; virtual; abstract;
    { <COMBINE CallFloatFunc> }
    function CallTernary64Func(AFunc: TTernary64Func; x, y, z: TExprNode): TExprNode; virtual; abstract;
    { <COMBINE CallFloatFunc> }
    function CallTernary80Func(AFunc: TTernary80Func; x, y, z: TExprNode): TExprNode; virtual; abstract;

    { Brief: Performs an arithmetic operation.
      Description:
        These functions generate nodes that perform an arithmetic operation
        on their operands. }
    function Add(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;
    { <COMBINE Add> }
    function Subtract(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;
    { <COMBINE Add> }
    function Multiply(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;
    { <COMBINE Add> }
    function Divide(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;
    { <COMBINE Add> }
    function Negate(AValue: TExprNode): TExprNode; virtual; abstract;

    { Brief: Performs a comparison.
      Parameters:
        ALeft: Left side of comparison.
        ARight: Right side of comparison.
      Returns:
        A node that represents this comparison.
      Description:
        Compare returns a node that evaluates to -1, 0, or 1 depending on
        whether ALeft is less than, equal to, or greater than ARight,
        respectively. }
    function Compare(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;

    { Brief: Overloaded declarations for auto-selection of correct
        function in code.
      Description:
        These functions should auto-select the correct function for
        the input variable in code, making it more maintainable. }
    function LoadVar(ALoc: PFloat32): TExprNode; overload;
    { <COMBINE TExprNodeFactory.LoadVar@PFloat32> }
    function LoadVar(ALoc: PFloat64): TExprNode; overload;
    { <COMBINE TExprNodeFactory.LoadVar@PFloat32> }
    function LoadVar(ALoc: PFloat80): TExprNode; overload;
    { <COMBINE TExprNodeFactory.LoadVar@PFloat32> }
    function LoadConst(AValue: TFloat32): TExprNode; overload;
    { <COMBINE TExprNodeFactory.LoadVar@PFloat32> }
    function LoadConst(AValue: TFloat64): TExprNode; overload;
    { <COMBINE TExprNodeFactory.LoadVar@PFloat32> }
    function LoadConst(AValue: TFloat80): TExprNode; overload;
  end;

  { Brief: A compiling parser.
    Description:
      This is a compiling parser. It analyses the input stream of tokens
      from its lexer using a grammar and builds a graph of nodes that
      contains enough information to be converted into a high-speed
      evaluation structure, or possibly even machine code.

      <p>The key methods are Create and Compile.
    See Also: TExprEvalParser, TExprLexer, TExprSym }
  TExprCompileParser = class
  private
    FContext: TExprContext;
    FLexer: TExprLexer;
    FNodeFactory: TExprNodeFactory;
  public
    { Brief: Constructs an instance.
      Parameters:
        ALexer: The source of tokens to use. It doesn't take ownership
          of the lexer.
        ANodeFactory: The factory object to use for creating expression
          nodes. It doesn't take ownership of the factory. }
    constructor Create(ALexer: TExprLexer; ANodeFactory: TExprNodeFactory);

    { Brief: Compiles an expression from the lexical source.
      Description:
        This method compiles the expression by descending through its
        grammatical methods, starting with compile_expr.
      Returns: The top-level expression node. }
    function Compile: TExprNode; virtual;

    { Brief: The source of tokens for this parser. }
    property Lexer: TExprLexer read FLexer;

    { Brief: The node factory object that constructs concrete node types. }
    property NodeFactory: TExprNodeFactory read FNodeFactory;

    { Brief: The context object used for symbol lookup.
      Description:
        This property gives the context object that will be used for
        symbol lookup. Whenever an identifier is found in the input
        stream, the context will be searched (with TContext.Find), and the
        symbol found will have its <LINK TExprSym.Compile, Compile>
        method called. If no symbol is found or the Context property is nil,
        then an exception will be raised.  }
    property Context: TExprContext read FContext write FContext;

    // grammar starts here

    { Brief: Compiles relational operators and uses compile_simple_expr. }
    function compile_expr(ASkip: Boolean): TExprNode; virtual;
    { Brief: Compiles +, -, etc and uses compile_term. }
    function compile_simple_expr(ASkip: Boolean): TExprNode;
    { Brief: Compiles *, /, etc and uses compile_signed_factor. }
    function compile_term(ASkip: Boolean): TExprNode;
    { Brief: Compiles unary negate etc, and uses compile_factor. }
    function compile_signed_factor(ASkip: Boolean): TExprNode;
    { Brief: Compiles subexpressions (i.e. '(' & ')'), numbers, but defers
        identifiers to compile_ident_factor. }
    function compile_factor: TExprNode;
    { Brief: Looks up the symbol corresponding to an identifier and
        returns its compilation. }
    function compile_ident_factor: TExprNode;
  end;

  { Brief: An evaluating parser.
    Description:
      This is an evaluating parser. It evaluates the result of an expression
      as it grammatically analyses the input stream of tokens from its lexer.
      It returns a floating-point value.
    See Also: TExprCompileParser, TExprLexer, TExprSym }
  TExprEvalParser = class
  private
    FContext: TExprContext;
    FLexer: TExprLexer;
  public
    { Brief: Constructs an instance.
      Parameters:
        ALexer: The source of tokens to use. It doesn't take ownership
          of the lexer. }
    constructor Create(ALexer: TExprLexer);

    { Brief: Evaluates an expression from the lexical source.
      Description:
        This method evaluates the expression by descending through its
        grammatical methods, starting with eval_expr.
      Returns: The result of the evaluation. }
    function Evaluate: TFloat; virtual;

    { Brief: The source of tokens for this parser. }
    property Lexer: TExprLexer read FLexer;

    { Brief: The context object used for symbol lookup.
      Description:
        This property gives the context object that will be used for
        symbol lookup. Whenever an identifier is found in the input
        stream, the context will be searched (with TContext.Find), and
        the symbol found will have its <LINK TExprSym.Evaluate, Evaluate>
        method called. If no symbol is found or the Context property is nil,
        then an exception will be raised.  }
    property Context: TExprContext read FContext write FContext;

    // grammar starts here

    { Brief: Evaluates relational operators and uses eval_simple_expr. }
    function eval_expr(ASkip: Boolean): TFloat; virtual;
    { Brief: Evaluates +, -, etc and uses eval_term. }
    function eval_simple_expr(ASkip: Boolean): TFloat;
    { Brief: Evaluates *, /, etc and uses eval_signed_factor. }
    function eval_term(ASkip: Boolean): TFloat;
    { Brief: Evaluates unary negate etc, and uses eval_factor. }
    function eval_signed_factor(ASkip: Boolean): TFloat;
    { Brief: Evaluates subexpressions (i.e. '(' & ')'), numbers, but defers
        identifiers to eval_ident_factor. }
    function eval_factor: TFloat;
    { Brief: Looks up the symbol corresponding to an identifier and
        returns its evaluation. }
    function eval_ident_factor: TFloat;
  end;

{ some concrete class descendants follow... }

type
  { Brief: A simple expression lexical analyser. }
  TExprSimpleLexer = class(TExprLexer)
  protected
    { Brief: Current position in buffer. }
    FCurrPos: PChar;
    { Brief: Buffer containing expression. }
    FBuf: string;
    { Brief: Sets a new buffer and calls Reset. }
    procedure SetBuf(const ABuf: string);
  public
    { Brief: Constructs an instance with a buffer ABuf.
      Parameters:
        ABuf: A string containing an expression. }
    constructor Create(const ABuf: string);

    procedure NextTok; override;
    procedure Reset; override;

    { Brief: Buffer to read expression from.
      Description:
        Set this to change the source text the lexer extracts its tokens
        from. When it is set, the property setter calls the Reset method,
        so the lexer will be in a valid state to serve tokens. }
    property Buf: string read FBuf write SetBuf;
  end;

type
  { Brief: An operation that can be executed by a TExprVirtMach instance.
    Description:
      TExprVirtMachOp is the kernel of TExprVirtMach's operation. The
      containing class (TExprVirtMach) is just that - a container, and
      it just executes the instructions in order to do work.
      <p>
      Each instruction has a virtual Execute method, which should read
      input from somewhere and write output to somewhere else. Typically,
      the input is a series of pointers to floating-point variables, and
      the output is to one or more member variables. The output acts as
      input for instructions further on in the execution stream; for this
      mechanism to work, the inputs of downstream instructions must be
      'wired' to the outputs of upstream instructions.
    See Also: TExprVirtMach }
  TExprVirtMachOp = class
  private
    function GetOutputLoc: PFloat;
  protected
    { Brief: The actual variable this operation will write its output to.
      Description:
        This is the internal storage variable this operation will write
        its output to. Operations that use this operation for input should
        take the address of this variable (through the OutputLoc property)
        to get the result of evaluating this operation.
        <p>
        It is protected to allow easy (and fast) access for descendants.
      See Also: OutputLoc }
    FOutput: TFloat;
  public
    { Brief: Executes this instruction.
      Description:
        This method executes this instruction, reading from its inputs
        and writing to its output location. It returns False to terminate
        the execution sequence early; usually it returns True. }
    procedure Execute; virtual; abstract;

    { Brief: The address to which this operation will write its output to.
      See Also: FOutput }
    property OutputLoc: PFloat read GetOutputLoc;
  end;

  { Brief: A virtual machine for evaluating expressions relatively quickly.
    See Also: TExprVirtMachNodeFactory, TExprVirtMachOp }
  TExprVirtMach = class
  private
    FCodeList: TList;
    FConstList: TList;
  public
    { Brief: Constructs an instance. }
    constructor Create;
    { Brief: Destroys an instance. Use Free instead. }
    destructor Destroy; override;

    { Brief: Adds an operation to the end of the list of operations.
      Parameters:
        AOp: The operation to add. }
    procedure Add(AOp: TExprVirtMachOp);

    { Brief: Adds a constant to the constant list.
      Parameters:
        AOp: The constant to add. }
    procedure AddConst(AOp: TExprVirtMachOp);

    { Brief: Clears any stored code. }
    procedure Clear;

    { Brief: Executes the stored code and returns the result.
      Returns:
        The value output by the last instruction executed (the result).
      Description:
        This method executes the stored instructions in order starting
        at the beginning until it either runs out of instructions or it
        encounters a halt instruction. }
    function Execute: TFloat;
  end;

  { Brief: A node factory for virtual machine instructions.
    Description:
      This is a factory class for the default virtual machine.
    See Also: TExprVirtMach }
  TExprVirtMachNodeFactory = class(TExprNodeFactory)
  private
    FNodeList: TList;

    function AddNode(ANode: TExprNode): TExprNode;
    procedure DoClean(AVirtMach: TExprVirtMach);
    procedure DoConsts(AVirtMach: TExprVirtMach);
    procedure DoCode(AVirtMach: TExprVirtMach);
  public
    { Brief: Constructs an instance. }
    constructor Create;

    { Brief: Destroys an instance. Use Free instead. }
    destructor Destroy; override;

    { Brief: Generates code for a virtual machine.
      Parameters:
        AVirtMach: The virtual machine to generate code for.
      Description:
        This method converts the internal node DAG into instructions for
        the virtual machine passed in. }
    procedure GenCode(AVirtMach: TExprVirtMach);

    function LoadVar32(ALoc: PFloat32): TExprNode; override;
    function LoadVar64(ALoc: PFloat64): TExprNode; override;
    function LoadVar80(ALoc: PFloat80): TExprNode; override;
    function LoadConst32(AValue: TFloat32): TExprNode; override;
    function LoadConst64(AValue: TFloat64): TExprNode; override;
    function LoadConst80(AValue: TFloat80): TExprNode; override;

    function CallFloatFunc(AFunc: TFloatFunc): TExprNode; override;
    function CallFloat32Func(AFunc: TFloat32Func): TExprNode; override;
    function CallFloat64Func(AFunc: TFloat64Func): TExprNode; override;
    function CallFloat80Func(AFunc: TFloat80Func): TExprNode; override;
    function CallUnaryFunc(AFunc: TUnaryFunc; x: TExprNode): TExprNode; override;
    function CallUnary32Func(AFunc: TUnary32Func; x: TExprNode): TExprNode; override;
    function CallUnary64Func(AFunc: TUnary64Func; x: TExprNode): TExprNode; override;
    function CallUnary80Func(AFunc: TUnary80Func; x: TExprNode): TExprNode; override;
    function CallBinaryFunc(AFunc: TBinaryFunc; x, y: TExprNode): TExprNode; override;
    function CallBinary32Func(AFunc: TBinary32Func; x, y: TExprNode): TExprNode; override;
    function CallBinary64Func(AFunc: TBinary64Func; x, y: TExprNode): TExprNode; override;
    function CallBinary80Func(AFunc: TBinary80Func; x, y: TExprNode): TExprNode; override;
    function CallTernaryFunc(AFunc: TTernaryFunc; x, y, z: TExprNode): TExprNode; override;
    function CallTernary32Func(AFunc: TTernary32Func; x, y, z: TExprNode): TExprNode; override;
    function CallTernary64Func(AFunc: TTernary64Func; x, y, z: TExprNode): TExprNode; override;
    function CallTernary80Func(AFunc: TTernary80Func; x, y, z: TExprNode): TExprNode; override;

    function Add(ALeft, ARight: TExprNode): TExprNode; override;
    function Subtract(ALeft, ARight: TExprNode): TExprNode; override;
    function Multiply(ALeft, ARight: TExprNode): TExprNode; override;
    function Divide(ALeft, ARight: TExprNode): TExprNode; override;
    function Negate(AValue: TExprNode): TExprNode; override;
    function Compare(ALeft, ARight: TExprNode): TExprNode; override;
  end;

{ some concrete symbols }

type
  { Brief: Symbol for a constant of type TFloat. }
  TExprConstSym = class(TExprSym)
  private
    FValue: TFloat;
  public
    { Brief: Constructs an instance.
      Parameters:
        AIdent: Identifier for this constant.
        AValue: Value this identifier should evaluate to. }
    constructor Create(const AIdent: string; AValue: TFloat);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  { Brief: Symbol for a constant of type TFloat32. }
  TExprConst32Sym = class(TExprSym)
  private
    FValue: TFloat32;
  public
    { Brief: Constructs an instance.
      Parameters:
        AIdent: Identifier for this constant.
        AValue: Value this identifier should evaluate to. }
    constructor Create(const AIdent: string; AValue: TFloat32);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  { Brief: Symbol for a constant of type TFloat64. }
  TExprConst64Sym = class(TExprSym)
  private
    FValue: TFloat64;
  public
    { Brief: Constructs an instance.
      Parameters:
        AIdent: Identifier for this constant.
        AValue: Value this identifier should evaluate to. }
    constructor Create(const AIdent: string; AValue: TFloat64);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  { Brief: Symbol for a constant of type TFloat80. }
  TExprConst80Sym = class(TExprSym)
  private
    FValue: TFloat80;
  public
    { Brief: Constructs an instance.
      Parameters:
        AIdent: Identifier for this constant.
        AValue: Value this identifier should evaluate to. }
    constructor Create(const AIdent: string; AValue: TFloat80);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  { Brief: This class evaluates and / or compiles code for a 32-bit
    FP variable. }
  TExprVar32Sym = class(TExprSym)
  private
    FLoc: PFloat32;
  public
    { Brief: Constructs a symbol representing a 32-bit FP variable.
      Parameters:
        AIdent: Name of the variable.
        ALoc: Address of the variable. }
    constructor Create(const AIdent: string; ALoc: PFloat32);

    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  { Brief: This class evaluates and / or compiles code for a 64-bit
    FP variable. }
  TExprVar64Sym = class(TExprSym)
  private
    FLoc: PFloat64;
  public
    { Brief: Constructs a symbol representing a 64-bit FP variable.
      Parameters:
        AIdent: Name of the variable.
        ALoc: Address of the variable. }
    constructor Create(const AIdent: string; ALoc: PFloat64);

    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  { Brief: This class evaluates and / or compiles code for an 80-bit
    FP variable. }
  TExprVar80Sym = class(TExprSym)
  private
    FLoc: PFloat80;
  public
    { Brief: Constructs a symbol representing an 80-bit FP variable.
      Parameters:
        AIdent: Name of the variable.
        ALoc: Address of the variable. }
    constructor Create(const AIdent: string; ALoc: PFloat80);

    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  { Brief: A helper ancestor for function symbols.
    Description:
      This is a useful class to use as an ancestor for function symbols
      because it has protected methods to read parameters. }
  TExprAbstractFuncSym = class(TExprSym)
  protected
    { Brief: Evaluates the first argument using the EvalParser.
      Returns:
        The evaluation of the first argument.
      Description:
        This method will raise an exception if there is a missing '(' or
        missing first argument. }
    function EvalFirstArg: TFloat;
    { Brief: Evaluates a second or subsequent argument using EvalParser.
      Returns:
        The evaluation of the next argument.
      Description:
        This method will raise an exception if there is a missing ',' or
        missing argument after the comma. }
    function EvalNextArg: TFloat;
    { Brief: Compiles the first argument using the CompileParser.
      Returns:
        The compiled node for the first argument.
      Description:
        This method will raise an exception if there is a missing '(' or
        missing first argument. }
    function CompileFirstArg: TExprNode;
    { Brief: Compiles a second or subsequent argument using CompileParser.
      Returns:
        The compiled node for the next argument.
      Description:
        This method will raise an exception if there is a missing ',' or
        missing argument after the comma. }
    function CompileNextArg: TExprNode;
    { Brief: Reads in the end of an argument list.
      Description:
        This method will raise an exception if the current token isn't ')'.
        After checking, it skips the right parenthesis. }
    procedure EndArgs;
  end;

  { Brief: Function symbol for TFloatFunc. }
  TExprFuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TFloatFunc;
  public
    { Brief: Constructs an instance.
      Parameters:
        AIdent: Identifier for this function.
        AFunc: Function that evaluates this symbol. }
    constructor Create(const AIdent: string; AFunc: TFloatFunc);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  { Brief: Function symbol for TFloat32Func. }
  TExprFloat32FuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TFloat32Func;
  public
    { Brief: Constructs an instance.
      Parameters:
        AIdent: Identifier for this function.
        AFunc: Function that evaluates this symbol. }
    constructor Create(const AIdent: string; AFunc: TFloat32Func);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  { Brief: Function symbol for TFloat64Func. }
  TExprFloat64FuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TFloat64Func;
  public
    { Brief: Constructs an instance.
      Parameters:
        AIdent: Identifier for this function.
        AFunc: Function that evaluates this symbol. }
    constructor Create(const AIdent: string; AFunc: TFloat64Func);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  { Brief: Function symbol for TFloat80Func. }
  TExprFloat80FuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TFloat80Func;
  public
    { Brief: Constructs an instance.
      Parameters:
        AIdent: Identifier for this function.
        AFunc: Function that evaluates this symbol. }
    constructor Create(const AIdent: string; AFunc: TFloat80Func);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  { Brief: Function symbol for TUnaryFunc. }
  TExprUnaryFuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TUnaryFunc;
  public
    { Brief: Constructs an instance.
      Parameters:
        AIdent: Identifier for this function.
        AFunc: Function that evaluates this symbol. }
    constructor Create(const AIdent: string; AFunc: TUnaryFunc);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  { Brief: Function symbol for TUnary32Func. }
  TExprUnary32FuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TUnary32Func;
  public
    { Brief: Constructs an instance.
      Parameters:
        AIdent: Identifier for this function.
        AFunc: Function that evaluates this symbol. }
    constructor Create(const AIdent: string; AFunc: TUnary32Func);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  { Brief: Function symbol for TUnary64Func. }
  TExprUnary64FuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TUnary64Func;
  public
    { Brief: Constructs an instance.
      Parameters:
        AIdent: Identifier for this function.
        AFunc: Function that evaluates this symbol. }
    constructor Create(const AIdent: string; AFunc: TUnary64Func);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  { Brief: Function symbol for TUnary80Func. }
  TExprUnary80FuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TUnary80Func;
  public
    { Brief: Constructs an instance.
      Parameters:
        AIdent: Identifier for this function.
        AFunc: Function that evaluates this symbol. }
    constructor Create(const AIdent: string; AFunc: TUnary80Func);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  { Brief: Function symbol for TBinaryFunc. }
  TExprBinaryFuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TBinaryFunc;
  public
    { Brief: Constructs an instance.
      Parameters:
        AIdent: Identifier for this function.
        AFunc: Function that evaluates this symbol. }
    constructor Create(const AIdent: string; AFunc: TBinaryFunc);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  { Brief: Function symbol for TBinary32Func. }
  TExprBinary32FuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TBinary32Func;
  public
    { Brief: Constructs an instance.
      Parameters:
        AIdent: Identifier for this function.
        AFunc: Function that evaluates this symbol. }
    constructor Create(const AIdent: string; AFunc: TBinary32Func);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  { Brief: Function symbol for TBinary64Func. }
  TExprBinary64FuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TBinary64Func;
  public
    { Brief: Constructs an instance.
      Parameters:
        AIdent: Identifier for this function.
        AFunc: Function that evaluates this symbol. }
    constructor Create(const AIdent: string; AFunc: TBinary64Func);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  { Brief: Function symbol for TBinary80Func. }
  TExprBinary80FuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TBinary80Func;
  public
    { Brief: Constructs an instance.
      Parameters:
        AIdent: Identifier for this function.
        AFunc: Function that evaluates this symbol. }
    constructor Create(const AIdent: string; AFunc: TBinary80Func);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  { Brief: Function symbol for TTernaryFunc. }
  TExprTernaryFuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TTernaryFunc;
  public
    { Brief: Constructs an instance.
      Parameters:
        AIdent: Identifier for this function.
        AFunc: Function that evaluates this symbol. }
    constructor Create(const AIdent: string; AFunc: TTernaryFunc);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  { Brief: Function symbol for TTernary32Func. }
  TExprTernary32FuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TTernary32Func;
  public
    { Brief: Constructs an instance.
      Parameters:
        AIdent: Identifier for this function.
        AFunc: Function that evaluates this symbol. }
    constructor Create(const AIdent: string; AFunc: TTernary32Func);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  { Brief: Function symbol for TTernary64Func. }
  TExprTernary64FuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TTernary64Func;
  public
    { Brief: Constructs an instance.
      Parameters:
        AIdent: Identifier for this function.
        AFunc: Function that evaluates this symbol. }
    constructor Create(const AIdent: string; AFunc: TTernary64Func);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  { Brief: Function symbol for TTernary80Func. }
  TExprTernary80FuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TTernary80Func;
  public
    { Brief: Constructs an instance.
      Parameters:
        AIdent: Identifier for this function.
        AFunc: Function that evaluates this symbol. }
    constructor Create(const AIdent: string; AFunc: TTernary80Func);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

type
  { Brief: This is an abstract class that provides friendly methods for
      adding constanst, variables, functions and external contexts.
    Description:
      This is an abstract class provided so that symbols can be added
      to descendants' internal contexts with minimum hassle. Use a concrete
      descendant, like TEvaluator or TCompiledEvaluator instead. }
  TEasyEvaluator = class
  private
    FOwnContext: TExprHashContext;
    FExtContextSet: TExprSetContext;
    FInternalContextSet: TExprSetContext;
  protected
    { Brief: Provides protected access to the internal context set.
      Description:
        This is provided so that descendants can set their parser's Context
        property. }
    property InternalContextSet: TExprSetContext read FInternalContextSet;
  public
    { Brief: Creates an instance. }
    constructor Create;
    { Brief: Destroys an instance. Use Free instead. }
    destructor Destroy; override;

    { Brief: Adds a variable.
      Parameters:
        AName: Identifier of variable to add.
        AVar: Location of variable to add.
      Description:
        Adds a variable to the internal context. Whenever the variable
        is found in an expression, its current value will be inserted.
      Note: An assumption that may be made by optimizing compilers is that
        functions don't modify variables, and that functions may be called
        in any order.
      Note: Any variables added using these methods will override
        identifiers of the same name in external contexts added through
        ExtContextSet. }
    procedure AddVar(const AName: string; var AVar: TFloat32); overload;
    { <COMBINE TEasyEvaluator.AddVar@string@TFloat32> }
    procedure AddVar(const AName: string; var AVar: TFloat64); overload;
    { <COMBINE TEasyEvaluator.AddVar@string@TFloat32> }
    procedure AddVar(const AName: string; var AVar: TFloat80); overload;

    { Brief: Adds a constant.
      Parameters:
        AName: Identifier for the constant.
        AConst: Value of constant.
      Description:
        Adds a constant to the internal context. Constants are different
        from variables because sub-expressions made entirely from
        constants may be evaluated only once (at compile time), and that
        value used for all subsequent evaluations.
      Note: Any constants added using these methods will override
        identifiers of the same name in external contexts added through
        ExtContextSet. }
    procedure AddConst(const AName: string; AConst: TFloat32); overload;
    { <COMBINE TEasyEvaluator.AddConst@string@TFloat32> }
    procedure AddConst(const AName: string; AConst: TFloat64); overload;
    { <COMBINE TEasyEvaluator.AddConst@string@TFloat32> }
    procedure AddConst(const AName: string; AConst: TFloat80); overload;

    { Brief: Adds a function.
      Parameters:
        AName: Identifier for the function.
        AFunc: Function pointer that evaluates the function.
      Description:
        Adds a function to the internal context. Multiple calls to the
        same function with the same parameters <b>might</b> be resolved to
        a single call during common sub-expression elimination (CSE)
        optimization. A possible workaround would be to add a fake extra
        parameter and pass in different constant for each distinct call.
      Note: Any functions added using these methods will override
        identifiers of the same name in external contexts added through
        ExtContextSet. }
    procedure AddFunc(const AName: string; AFunc: TFloat32Func); overload;
    { <COMBINE TEasyEvaluator.AddFunc@string@TFloat32Func> }
    procedure AddFunc(const AName: string; AFunc: TFloat64Func); overload;
    { <COMBINE TEasyEvaluator.AddFunc@string@TFloat32Func> }
    procedure AddFunc(const AName: string; AFunc: TFloat80Func); overload;
    { <COMBINE TEasyEvaluator.AddFunc@string@TFloat32Func> }
    procedure AddFunc(const AName: string; AFunc: TUnary32Func); overload;
    { <COMBINE TEasyEvaluator.AddFunc@string@TFloat32Func> }
    procedure AddFunc(const AName: string; AFunc: TUnary64Func); overload;
    { <COMBINE TEasyEvaluator.AddFunc@string@TFloat32Func> }
    procedure AddFunc(const AName: string; AFunc: TUnary80Func); overload;
    { <COMBINE TEasyEvaluator.AddFunc@string@TFloat32Func> }
    procedure AddFunc(const AName: string; AFunc: TBinary32Func); overload;
    { <COMBINE TEasyEvaluator.AddFunc@string@TFloat32Func> }
    procedure AddFunc(const AName: string; AFunc: TBinary64Func); overload;
    { <COMBINE TEasyEvaluator.AddFunc@string@TFloat32Func> }
    procedure AddFunc(const AName: string; AFunc: TBinary80Func); overload;
    { <COMBINE TEasyEvaluator.AddFunc@string@TFloat32Func> }
    procedure AddFunc(const AName: string; AFunc: TTernary32Func); overload;
    { <COMBINE TEasyEvaluator.AddFunc@string@TFloat32Func> }
    procedure AddFunc(const AName: string; AFunc: TTernary64Func); overload;
    { <COMBINE TEasyEvaluator.AddFunc@string@TFloat32Func> }
    procedure AddFunc(const AName: string; AFunc: TTernary80Func); overload;

    { Brief: Removes an identifier from the internal context.
      Parameters:
        AName: Identifier to remove.
      Description:
        This method removes an identifier from the internal context and
        frees its associated symbol.
      Note: This is the only way to remove a single identifier from the
        internal context (call Clear to remove all identifiers). }
    procedure Remove(const AName: string);
    { Brief: Clears all identifiers from the internal context.
      Description:
        This method clears the internal context of symbols. It doesn't
        affect any contexts added through ExtContextSet. }
    procedure Clear;

    { Brief: A set of external contexts that are looked up after
        the internal context.
      Description:
        This property allows the addition of multiple utility contexts
        to this expression evaluator. Things like function libraries,
        variable sets, constant libraries etc. may be added using methods
        of this property. }
    property ExtContextSet: TExprSetContext read FExtContextSet;
  end;

  { Brief: Quick evaluator shell object.
    Description:
      This is an encapsulation of a simple lexer and an evaluating
      parser. It evaluates while parsing, and it doesn't store any
      compiled expression. This means it evaluates quite quickly, but
      isn't very fast for repeated evaluations of the same expression.

    Example:
      Create a new project, remove the default form and replace the contents
      of the project file with this:
      <code>
        uses SysUtils, JclExprEval, Dialogs;

        function MyAdder(x, y: Double): Double;
        begin
          Result := x + y + 0.12;
        end;

        var
          evaluator: TEvaluator;
          x: Double;
          y: Extended;
        begin
          evaluator := TEvaluator.Create;
          try
            evaluator.AddVar('x', x);
            evaluator.AddVar('y', y);
            evaluator.AddFunc('MyAdder', MyAdder);

            x := 3.5;
            y := 0.7;
            ShowMessage(Format('Delphi says: %.4g',
              [MyAdder(x, y)]));
            ShowMessage(Format('TEvaluator says: %.4g',
              [evaluator.Evaluate('MyAdder(x, y)')]));
          finally
            evaluator.Free;
          end;
        end.
      </code> }
  TEvaluator = class(TEasyEvaluator)
  private
    FLexer: TExprSimpleLexer;
    FParser: TExprEvalParser;
  public
    { Brief: Constructs an instance. }
    constructor Create;
    { Brief: Destroys an instance. Use Free instead. }
    destructor Destroy; override;

    { Brief: Evaluates an expression.
      Parameters:
        AExpr: The expression to evaluate.
      Returns:
        The result of the evaluation.
      Description:
        This sets the lexer source to AExpr, and calls the parser's
        <link TExprEvalParser.Evaluate, Evaluate> method. }
    function Evaluate(const AExpr: string): TFloat;
  end;

  { Brief: An evaluator that first compiles an expression into an
      intermediate form, then evaluates it on demand.
    Description:
      This evaluator is suitable for applications like graphing, where
      there is just one expression which is constantly evaluated with
      variables and/or functions changing value. It takes longer to
      compile than TEvaluator does to evaluate, but once compiled is
      much faster. }
  TCompiledEvaluator = class(TEasyEvaluator)
  private
    FExpr: string;
    FVirtMach: TExprVirtMach;
  public
    { Brief: Constructs an instance. }
    constructor Create;
    destructor Destroy; override;

    { Brief: Compiles an expression.
      Parameters:
        AExpr: The expression to compile.
      Description:
        Compiles the expression given by AExpr, and stores its compiled
        state internally, so that it can be evaluated quickly. }
    procedure Compile(const AExpr: string);
    { Brief: Evaluates the internal compiled expression.
      Returns: The result of the evaluation.
      Description:
        Executes the internal compiled state, and returns the evaluation.
        If there was an error while compiling, or the object is just after
        being created, it returns zero. }
    function Evaluate: TFloat;
  end;

  {TODO: change this definition to be just a normal function pointer, not
    a closure; will require a small executable memory allocater, and a
    couple of injected instructions. Similar concept to
    Forms.MakeObjectInstance.

    This will allow compiled expressions to be used as functions in
    contexts. Parameters won't be supported, though; I'll think about
    this. }

  { Brief: A compiled expression, which may be called directly to evaluate. }
  TCompiledExpression = function: TFloat of object;

  { Brief: An expression compiler, for multiple expressions.
    Description:
      This is a multiple expression compiler. It compiles expressions into
      function pointers, so that the function pointer can be called as if
      it were an ordinary Delphi function. It takes longer to compile an
      expression than TEvaluator does to evaluate, but once compiled it is
      much faster at evaluating.
      <p>
      It is suitable for spreadsheet-like applications, where there may
      be thousands of functions, all of which have to be evaluated
      quickly and repeatedly. }
  TExpressionCompiler = class(TEasyEvaluator)
  private
    FExprHash: TStringHashMap;
  public
    { Brief: Constructs an instance. }
    constructor Create;
    destructor Destroy; override;

    { Brief: Compiles an expression into a function pointer.
      Parameters:
        AExpr: Expression to compile.
      Returns:
        A function pointer which, when called, will evaluate the result of
        the expression and return it.
      Description:
        This method compiles the given expression into an internal
        representation (a reference to which is kept internally), and
        returns a function pointer that evaluates the expression
        whenever called.
        <p>
        Because a reference is kept internally, to free the expression
        (and thus release its resources), either the Remove or Delete
        methods must be called. Calling Clear will free all expressions,
        as will freeing this compiler. }
    function Compile(const AExpr: string): TCompiledExpression;
    { Brief: Frees a compiled expression.
      Parameters:
        AExpr: Expression to remove.
      Description:
        Remove frees the expression into which AExpr was compiled. AExpr is
        used as a string to look up a hash map, so it should be identical
        to the string passed in to Compile. }
    procedure Remove(const AExpr: string);
    { Brief: Frees a compiled expression.
      Parameters:
        ACompiledExpression: Expression to remove.
      Description:
        Delete frees the expression referenced by ACompiledExpression. }
    procedure Delete(ACompiledExpression: TCompiledExpression);
    { Brief: Clears all compiled expressions.
      Description:
        This method frees all internal compiled expressions; this will
        invalidate any remaining compiled expression function pointers,
        and subsequntly calling one of these remaining function pointers
        will result in undefined behaviour (probably an access violation). }
    procedure Clear;
  end;

implementation

//==================================================================================================
// TExprHashContext
//==================================================================================================

constructor TExprHashContext.Create(ACaseSensitive: Boolean; AHashSize: Integer);
begin
  if ACaseSensitive then
    FHashMap := TStringHashMap.Create(CaseSensitiveTraits, AHashSize)
  else
    FHashMap := TStringHashMap.Create(CaseInsensitiveTraits, AHashSize);
end;

//--------------------------------------------------------------------------------------------------

destructor TExprHashContext.Destroy;
begin
  FHashMap.Iterate(nil, Iterate_FreeObjects);
  FHashMap.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprHashContext.Add(ASymbol: TExprSym);
begin
  FHashMap.Add(ASymbol.Ident, ASymbol);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprHashContext.Remove(const AName: string);
begin
  TObject(FHashMap.Remove(AName)).Free;
end;

//--------------------------------------------------------------------------------------------------

function TExprHashContext.Find(const AName: string): TExprSym;
begin
  if not FHashMap.Find(AName, Result) then
    Result := nil;
end;

//==================================================================================================
// TExprSetContext
//==================================================================================================

procedure TExprSetContext.Add(AContext: TExprContext);
begin
  FList.Add(AContext);
end;

//--------------------------------------------------------------------------------------------------

constructor TExprSetContext.Create(AOwnsContexts: Boolean);
begin
  FOwnsContexts := AOwnsContexts;
  FList := TList.Create;
end;

procedure TExprSetContext.Delete(AIndex: Integer);
begin
  if FOwnsContexts then
    TObject(FList[AIndex]).Free;
  FList.Delete(AIndex);
end;

//--------------------------------------------------------------------------------------------------

destructor TExprSetContext.Destroy;
begin
  if FOwnsContexts then
    ClearObjectList(FList);
  FList.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TExprSetContext.Extract(AContext: TExprContext): TExprContext;
begin
  Result := AContext;
  FList.Remove(AContext);
end;

//--------------------------------------------------------------------------------------------------

function TExprSetContext.Find(const AName: string): TExprSym;
var
  i: Integer;
begin
  Result := nil;
  for i := Count - 1 downto 0 do
  begin
    Result := Contexts[i].Find(AName);
    if Result <> nil then
      Exit;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TExprSetContext.GetContexts(AIndex: Integer): TExprContext;
begin
  Result := TExprContext(FList[AIndex]);
end;

//--------------------------------------------------------------------------------------------------

function TExprSetContext.GetCount: Integer;
begin
  Result := FList.Count;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprSetContext.Remove(AContext: TExprContext);
begin
  FList.Remove(AContext);
  if FOwnsContexts then
    AContext.Free;
end;

//==================================================================================================
// TExprSym
//==================================================================================================

constructor TExprSym.Create(const AIdent: string);
begin
  FIdent := AIdent;
end;

//==================================================================================================
// TExprLexer
//==================================================================================================

constructor TExprLexer.Create;
begin
  Reset;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprLexer.Reset;
begin
  NextTok;
end;

//==================================================================================================
// TExprCompileParser
//==================================================================================================

constructor TExprCompileParser.Create(ALexer: TExprLexer; ANodeFactory: TExprNodeFactory);
begin
  FLexer := ALexer;
  FNodeFactory := ANodeFactory;
end;

//--------------------------------------------------------------------------------------------------

function TExprCompileParser.Compile: TExprNode;
begin
  Result := compile_expr(False);
end;

//--------------------------------------------------------------------------------------------------

function TExprCompileParser.compile_expr(ASkip: Boolean): TExprNode;
begin
  Result := compile_simple_expr(ASkip);

  { Utilize some of these compound instructions to test DAG optimization
    techniques later on.

    Playing a few games after much hard work, too.
    Functional programming is fun! :-> BJK }
  while True do
    case Lexer.CurrTok of
      etEqualTo: // =
      begin
        // need to return 1 if true, 0 if false
        // compare will return 0 if true, -1 / +1 if false
        // squaring will force a positive or zero value
        // subtract value from 1 to get answer
        // IOW: 1 - Sqr(Compare(x, y))

        // first, get comparison
        Result := NodeFactory.Compare(Result, compile_simple_expr(True));

        // next, square comparison - note that this
        // forces a common sub-expression; parse tree will no longer
        // be a tree, but a DAG
        Result := NodeFactory.Multiply(Result, Result);

        // finally, subtract from one
        Result := NodeFactory.Subtract(
          NodeFactory.LoadConst32(1),
          Result
        );
      end;

      etNotEqual: // <>
      begin
        // same as above, but without the subtract
        Result := NodeFactory.Compare(Result, compile_simple_expr(True));
        Result := NodeFactory.Multiply(Result, Result);
      end;

      etLessThan: // <
      begin
        // have 1 for less than, 0 for equal, 0 for greater than too
        // c = compare(x, y)
        // d = c * c
        // if less than, d = 1, c = -1; d - c = 2
        // if greater than, d = c = 1;  d - c = 0
        // if equal, d = c = 0;         d - c = 0
        // IOW: (Sqr(compare(x, y)) - compare(x, y)) / 2

        // get comparison
        Result := NodeFactory.Compare(Result, compile_simple_expr(True));
        // subtract from square
        Result := NodeFactory.Subtract(
          NodeFactory.Multiply(
            Result,
            Result
          ),
          Result
        );
        // divide by two
        Result := NodeFactory.Divide(Result, NodeFactory.LoadConst32(2));
      end;

      etLessEqual: // <=
      begin
        // less than or equal to return 1, greater than returns 0
        // c = compare(x, y)
        // d = c * c
        // <  c = -1, d = 1, c + d = 0
        // =  c = 0,  d = 0, c + d = 0
        // >  c = +1, d = 1, c + d = 2
        // then divide by two, take away from 1
        // IOW: 1 - (compare(x, y) + Sqr(compare(x, y))) / 2
        Result := NodeFactory.Compare(Result, compile_simple_expr(True));
        // now, for some fun!
        Result := NodeFactory.Subtract(
          NodeFactory.LoadConst32(1),
          NodeFactory.Divide(
            NodeFactory.Add(
              Result,
              NodeFactory.Multiply(
                Result,
                Result
              )
            ),
            NodeFactory.LoadConst32(2)
          )
        );
      end;

      etGreaterThan: // >
      begin
        // same as <=, without the taking away from 1 bit
        Result := NodeFactory.Compare(Result, compile_simple_expr(True));
        Result := NodeFactory.Divide(
          NodeFactory.Add(
            Result,
            NodeFactory.Multiply(
              Result,
              Result
            )
          ),
          NodeFactory.LoadConst32(2)
        );
      end;

      etGreaterEqual: // >=
      begin
        // same as less than, but subtract from one
        Result := NodeFactory.Compare(Result, compile_simple_expr(True));
        Result := NodeFactory.Subtract(
          NodeFactory.Multiply(
            Result,
            Result
          ),
          Result
        );
        Result := NodeFactory.Divide(Result, NodeFactory.LoadConst32(2));
        Result := NodeFactory.Subtract(NodeFactory.LoadConst32(1), Result);
      end;
    else
      Break;
    end;
end;

//--------------------------------------------------------------------------------------------------

function TExprCompileParser.compile_simple_expr(ASkip: Boolean): TExprNode;
begin
  Result := compile_term(ASkip);

  while True do
    case Lexer.CurrTok of
      etPlus:
        Result := NodeFactory.Add(Result, compile_term(True));
      etMinus:
        Result := NodeFactory.Subtract(Result, compile_term(True));
    else
      Break;
    end;
end;

//--------------------------------------------------------------------------------------------------

function TExprCompileParser.compile_term(ASkip: Boolean): TExprNode;
begin
  Result := compile_signed_factor(ASkip);

  while True do
    case Lexer.CurrTok of
      etAsterisk:
        Result := NodeFactory.Multiply(Result, compile_signed_factor(True));
      etForwardSlash:
        Result := NodeFactory.Divide(Result, compile_signed_factor(True));
    else
      Break;
    end;
end;

//--------------------------------------------------------------------------------------------------

function TExprCompileParser.compile_signed_factor(ASkip: Boolean): TExprNode;
var
  neg: Boolean;
begin
  if ASkip then
    Lexer.NextTok;

  neg := False;
  while True do
  begin
    case Lexer.CurrTok of
      etPlus:
        { do nothing };
      etMinus:
        neg := not neg;
    else
      Break;
    end;
    Lexer.NextTok;
  end;

  Result := compile_factor;
  if neg then
    Result := NodeFactory.Negate(Result);
end;

//--------------------------------------------------------------------------------------------------

function TExprCompileParser.compile_factor: TExprNode;
begin
  case Lexer.CurrTok of
    etNumber:
    begin
      Result := NodeFactory.LoadConst64(Lexer.TokenAsNumber);
      Lexer.NextTok;
    end;

    etIdentifier:
      Result := compile_ident_factor;

    etLParen:
    begin
      Result := compile_expr(True);
      if Lexer.CurrTok <> etRParen then
        raise EJclExprEvalError.CreateResRec(@RsExprEvalRParenExpected);
      Lexer.NextTok;
    end;
  else
    raise EJclExprEvalError.CreateResRec(@RsExprEvalFactorExpected);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TExprCompileParser.compile_ident_factor: TExprNode;
var
  sym: TExprSym;
  oldCompileParser: TExprCompileParser;
  oldLexer: TExprLexer;
  oldNodeFactory: TExprNodeFactory;
begin
  { find symbol }
  if FContext = nil then
    raise EJclExprEvalError.CreateResRecFmt(@RsExprEvalUnknownSymbol,
      [Lexer.TokenAsString]);
  sym := FContext.Find(Lexer.TokenAsString);
  if sym = nil then
    raise EJclExprEvalError.CreateResRecFmt(@RsExprEvalUnknownSymbol,
      [Lexer.TokenAsString]);

  Lexer.NextTok;

  { set symbol properties }
  oldCompileParser := sym.CompileParser;
  oldLexer := sym.Lexer;
  oldNodeFactory := sym.NodeFactory;
  sym.FLexer := Lexer;
  sym.FCompileParser := Self;
  sym.FNodeFactory := NodeFactory;
  try
    { compile symbol }
    Result := sym.Compile;
  finally
    sym.FLexer := oldLexer;
    sym.FCompileParser := oldCompileParser;
    sym.FNodeFactory := oldNodeFactory;
  end;
end;

//==================================================================================================
// TExprEvalParser
//==================================================================================================

constructor TExprEvalParser.Create(ALexer: TExprLexer);
begin
  FLexer := ALexer;
end;

//--------------------------------------------------------------------------------------------------

function TExprEvalParser.Evaluate: TFloat;
begin
  Result := eval_expr(False);
end;

//--------------------------------------------------------------------------------------------------

function TExprEvalParser.eval_expr(ASkip: Boolean): TFloat;
begin
  Result := eval_simple_expr(ASkip);

  while True do
    case Lexer.CurrTok of
      etEqualTo: // =
        if Result = eval_simple_expr(True) then
          Result := 1
        else
          Result := 0;
      etNotEqual: // <>
        if Result <> eval_simple_expr(True) then
          Result := 1
        else
          Result := 0;
      etLessThan: // <
        if Result < eval_simple_expr(True) then
          Result := 1
        else
          Result := 0;
      etLessEqual: // <=
        if Result <= eval_simple_expr(True) then
          Result := 1
        else
          Result := 0;
      etGreaterThan: // >
        if Result > eval_simple_expr(True) then
          Result := 1
        else
          Result := 0;
      etGreaterEqual: // >=
        if Result >= eval_simple_expr(True) then
          Result := 1
        else
          Result := 0;
    else
      Break;
    end;
end;

//--------------------------------------------------------------------------------------------------

function TExprEvalParser.eval_simple_expr(ASkip: Boolean): TFloat;
begin
  Result := eval_term(ASkip);

  while True do
    case Lexer.CurrTok of
      etPlus:
        Result := Result + eval_term(True);
      etMinus:
        Result := Result - eval_term(True);
    else
      Exit;
    end;
end;

//--------------------------------------------------------------------------------------------------

function TExprEvalParser.eval_term(ASkip: Boolean): TFloat;
begin
  Result := eval_signed_factor(ASkip);

  while True do
    case Lexer.CurrTok of
      etAsterisk:
        Result := Result * eval_signed_factor(True);
      etForwardSlash:
        Result := Result / eval_signed_factor(True);
    else
      Exit;
    end;
end;

//--------------------------------------------------------------------------------------------------

function TExprEvalParser.eval_signed_factor(ASkip: Boolean): TFloat;
var
  neg: Boolean;
begin
  if ASkip then
    Lexer.NextTok;

  neg := False;
  while True do
  begin
    case Lexer.CurrTok of
      etPlus:
        { do nothing };
      etMinus:
        neg := not neg;
    else
      Break;
    end;
    Lexer.NextTok;
  end;

  Result := eval_factor;
  if neg then
    Result := - Result;
end;

//--------------------------------------------------------------------------------------------------

function TExprEvalParser.eval_factor: TFloat;
begin
  case Lexer.CurrTok of
    etIdentifier:
      Result := eval_ident_factor;

    etLParen:
    begin
      Result := eval_expr(True);
      if Lexer.CurrTok <> etRParen then
        raise EJclExprEvalError.CreateResRec(@RsExprEvalRParenExpected);
      Lexer.NextTok;
    end;

    etNumber:
    begin
      Result := Lexer.TokenAsNumber;
      Lexer.NextTok;
    end;
  else
    raise EJclExprEvalError.CreateResRec(@RsExprEvalFactorExpected);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TExprEvalParser.eval_ident_factor: TFloat;
var
  sym: TExprSym;
  oldEvalParser: TExprEvalParser;
  oldLexer: TExprLexer;
begin
  { find symbol }
  if FContext = nil then
    raise EJclExprEvalError.CreateResRecFmt(@RsExprEvalUnknownSymbol,
      [Lexer.TokenAsString]);
  sym := FContext.Find(Lexer.TokenAsString);
  if sym = nil then
    raise EJclExprEvalError.CreateResRecFmt(@RsExprEvalUnknownSymbol,
      [Lexer.TokenAsString]);

  Lexer.NextTok;

  { set symbol properties }
  oldEvalParser := sym.FEvalParser;
  oldLexer := sym.Lexer;
  sym.FLexer := Lexer;
  sym.FEvalParser := Self;
  try
    { evaluate symbol }
    Result := sym.Evaluate;
  finally
    sym.FLexer := oldLexer;
    sym.FEvalParser := oldEvalParser;
  end;
end;

//==================================================================================================
// TExprSimpleLexer
//==================================================================================================

constructor TExprSimpleLexer.Create(const ABuf: string);
begin
  FBuf := ABuf;
  inherited Create;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprSimpleLexer.NextTok;
var
  { register variable optimization }
  cp: PChar;
  start: PChar;
const
  CharToTokenMap: array[Char] of TExprToken =
  (
    {#0..#31}
    etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid,
    {#32} etInvalid,
    {#33} etBang, {#34} etDoubleQuote, {#35} etHash, {#36} etDollar,
    {#37} etPercent, {#38} etAmpersand, {#39} etSingleQuote, {#40} etLParen,
    {#41} etRParen, {#42} etAsterisk, {#43} etPlus, {#44} etComma,
    {#45} etMinus, {#46} etDot, {#47} etForwardSlash,
    // 48..57 - numbers...
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid,
    {#58} etColon, {#59} etSemiColon, {#60} etLessThan, {#61} etEqualTo,
    {#62} etGreaterThan, {#63} etQuestion, {#64} etAt,
    // 65..90 - capital letters...
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid,
    {#91} etLBracket, {#92} etBackSlash, {#93} etRBracket, {#94} etArrow,
    etInvalid, // 95 - underscore
    {#96} etBackTick,
    // 97..122 - small letters...
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid,
    {#123} etLBrace,
    {#124} etPipe, {#125} etRBrace, {#126} etTilde, {#127} et127,
    {#128} etEuro, {#129} et129, {#130} et130, {#131} et131,
    {#132} et132, {#133} et133, {#134} et134, {#135} et135,
    {#136} et136, {#137} et137, {#138} et138, {#139} et139,
    {#140} et140, {#141} et141, {#142} et142, {#143} et143,
    {#144} et144, {#145} et145, {#146} et146, {#147} et147,
    {#148} et148, {#149} et149, {#150} et150, {#151} et151,
    {#152} et152, {#153} et153, {#154} et154, {#155} et155,
    {#156} et156, {#157} et157, {#158} et158, {#159} et159,
    {#160} et160, {#161} et161, {#162} et162, {#163} et163,
    {#164} et164, {#165} et165, {#166} et166, {#167} et167,
    {#168} et168, {#169} et169, {#170} et170, {#171} et171,
    {#172} et172, {#173} et173, {#174} et174, {#175} et175,
    {#176} et176, {#177} et177, {#178} et178, {#179} et179,
    {#180} et180, {#181} et181, {#182} et182, {#183} et183,
    {#184} et184, {#185} et185, {#186} et186, {#187} et187,
    {#188} et188, {#189} et189, {#190} et190, {#191} et191,
    {#192} et192, {#193} et193, {#194} et194, {#195} et195,
    {#196} et196, {#197} et197, {#198} et198, {#199} et199,
    {#200} et200, {#201} et201, {#202} et202, {#203} et203,
    {#204} et204, {#205} et205, {#206} et206, {#207} et207,
    {#208} et208, {#209} et209, {#210} et210, {#211} et211,
    {#212} et212, {#213} et213, {#214} et214, {#215} et215,
    {#216} et216, {#217} et217, {#218} et218, {#219} et219,
    {#220} et220, {#221} et221, {#222} et222, {#223} et223,
    {#224} et224, {#225} et225, {#226} et226, {#227} et227,
    {#228} et228, {#229} et229, {#230} et230, {#231} et231,
    {#232} et232, {#233} et233, {#234} et234, {#235} et235,
    {#236} et236, {#237} et237, {#238} et238, {#239} et239,
    {#240} et240, {#241} et241, {#242} et242, {#243} et243,
    {#244} et244, {#245} et245, {#246} et246, {#247} et247,
    {#248} et248, {#249} et249, {#250} et250, {#251} et251,
    {#252} et252, {#253} et253, {#254} et254, {#255} et255
  );
begin
  cp := FCurrPos;

  { skip whitespace }
  while cp^ in ExprWhiteSpace do
    Inc(cp);

  { determine token type }
  case cp^ of
    #0:
      FCurrTok := etEof;

    'a'..'z', 'A'..'Z', '_':
    begin
      start := cp;
      Inc(cp);
      while cp^ in ['0'..'9', 'a'..'z', 'A'..'Z', '_'] do
        Inc(cp);
      SetString(FTokenAsString, start, cp - start);
      FCurrTok := etIdentifier;
    end;

    '0'..'9':
    begin
      start := cp;

      { read in integer part of mantissa }
      while cp^ in ['0'..'9'] do
        Inc(cp);

      { check for and read in fraction part of mantissa }
      if cp^ = '.' then
      begin
        Inc(cp);
        while cp^ in ['0'..'9'] do
          Inc(cp);
      end;

      { check for and read in exponent }
      if cp^ in ['e', 'E'] then
      begin
        Inc(cp);
        if cp^ in ['+', '-'] then
          Inc(cp);
        while cp^ in ['0'..'9'] do
          Inc(cp);
      end;

      { evaluate number }
      SetString(FTokenAsString, start, cp - start);
      FTokenAsNumber := StrToFloat(FTokenAsString);

      FCurrTok := etNumber;
    end;

    '<':
    begin
      Inc(cp);
      case cp^ of
        '=':
        begin
          FCurrTok := etLessEqual;
          Inc(cp);
        end;

        '>':
        begin
          FCurrTok := etNotEqual;
          Inc(cp);
        end;
      else
        FCurrTok := etLessThan;
      end;
    end;

    '>':
    begin
      Inc(cp);
      if cp^ = '=' then
      begin
        FCurrTok := etGreaterEqual;
        Inc(cp);
      end else
        FCurrTok := etGreaterThan;
    end;

  else
    { map character to token }
    FCurrTok := CharToTokenMap[cp^];
    Inc(cp);
  end;

  FCurrPos := cp;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprSimpleLexer.Reset;
begin
  FCurrPos := PChar(FBuf);
  inherited Reset;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprSimpleLexer.SetBuf(const ABuf: string);
begin
  FBuf := ABuf;
  Reset;
end;

//==================================================================================================
// TExprNode
//==================================================================================================

constructor TExprNode.Create(const ADepList: array of TExprNode);
var
  i: Integer;
begin
  FDepList := TList.Create;

  for i := 0 to High(ADepList) do
    AddDep(ADepList[i]);
end;

//--------------------------------------------------------------------------------------------------

destructor TExprNode.Destroy;
begin
  FDepList.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprNode.AddDep(ADep: TExprNode);
begin
  FDepList.Add(ADep);
end;

//--------------------------------------------------------------------------------------------------

function TExprNode.GetDepCount: Integer;
begin
  Result := FDepList.Count;
end;

//--------------------------------------------------------------------------------------------------

function TExprNode.GetDeps(AIndex: Integer): TExprNode;
begin
  Result := TExprNode(FDepList[AIndex]);
end;

//==================================================================================================
// TExprNodeFactory
//==================================================================================================

function TExprNodeFactory.LoadVar(ALoc: PFloat32): TExprNode;
begin
  Result := LoadVar32(ALoc);
end;

//--------------------------------------------------------------------------------------------------

function TExprNodeFactory.LoadVar(ALoc: PFloat64): TExprNode;
begin
  Result := LoadVar64(ALoc);
end;

//--------------------------------------------------------------------------------------------------

function TExprNodeFactory.LoadVar(ALoc: PFloat80): TExprNode;
begin
  Result := LoadVar80(ALoc);
end;

//--------------------------------------------------------------------------------------------------

function TExprNodeFactory.LoadConst(AValue: TFloat32): TExprNode;
begin
  Result := LoadConst32(AValue);
end;

//--------------------------------------------------------------------------------------------------

function TExprNodeFactory.LoadConst(AValue: TFloat64): TExprNode;
begin
  Result := LoadConst64(AValue);
end;

//--------------------------------------------------------------------------------------------------

function TExprNodeFactory.LoadConst(AValue: TFloat80): TExprNode;
begin
  Result := LoadConst80(AValue);
end;

//==================================================================================================
// TEvaluator
//==================================================================================================

constructor TEvaluator.Create;
begin
  inherited Create;

  FLexer := TExprSimpleLexer.Create('');
  FParser := TExprEvalParser.Create(FLexer);

  FParser.Context := InternalContextSet;
end;

//--------------------------------------------------------------------------------------------------

destructor TEvaluator.Destroy;
begin
  FParser.Free;
  FLexer.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TEvaluator.Evaluate(const AExpr: string): TFloat;
begin
  FLexer.Buf := AExpr;
  Result := FParser.Evaluate;
end;

//==================================================================================================
// TExprVirtMachOp
//==================================================================================================

function TExprVirtMachOp.GetOutputLoc: PFloat;
begin
  Result := @FOutput;
end;

//==================================================================================================
// Virtual machine operators follow
//==================================================================================================

type
  { abstract base for var readers }
  TExprVarVmOp = class(TExprVirtMachOp)
  private
    FVarLoc: Pointer;
  public
    constructor Create(AVarLoc: Pointer);
  end;

  TExprVarVmOpClass = class of TExprVarVmOp;

  { the var readers }

  TExprVar32VmOp = class(TExprVarVmOp)
  public
    procedure Execute; override;
  end;

  TExprVar64VmOp = class(TExprVarVmOp)
  public
    procedure Execute; override;
  end;

  TExprVar80VmOp = class(TExprVarVmOp)
  public
    procedure Execute; override;
  end;

  { the const holder }
  TExprConstVmOp = class(TExprVirtMachOp)
  public
    constructor Create(AValue: TFloat);
    { null function }
    procedure Execute; override;
  end;

  { abstract unary operator }
  TExprUnaryVmOp = class(TExprVirtMachOp)
  protected
    FInput: PFloat;
  public
    constructor Create(AInput: PFloat);
    property Input: PFloat read FInput write FInput;
  end;

  TExprUnaryVmOpClass = class of TExprUnaryVmOp;

  { abstract binary operator }
  TExprBinaryVmOp = class(TExprVirtMachOp)
  protected
    FLeft: PFloat;
    FRight: PFloat;
  public
    constructor Create(ALeft, ARight: PFloat);
    property Left: PFloat read FLeft write FLeft;
    property Right: PFloat read FRight write FRight;
  end;

  TExprBinaryVmOpClass = class of TExprBinaryVmOp;

  { the 4 basic binary operators }

  TExprAddVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprSubtractVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprMultiplyVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprDivideVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprCompareVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  { the unary operators }

  TExprNegateVmOp = class(TExprUnaryVmOp)
  public
    procedure Execute; override;
  end;

  { function calls }

  TExprCallFloatVmOp = class(TExprVirtMachOp)
  private
    FFunc: TFloatFunc;
  public
    constructor Create(AFunc: TFloatFunc);
    procedure Execute; override;
  end;

  TExprCallFloat32VmOp = class(TExprVirtMachOp)
  private
    FFunc: TFloat32Func;
  public
    constructor Create(AFunc: TFloat32Func);
    procedure Execute; override;
  end;

  TExprCallFloat64VmOp = class(TExprVirtMachOp)
  private
    FFunc: TFloat64Func;
  public
    constructor Create(AFunc: TFloat64Func);
    procedure Execute; override;
  end;

  TExprCallFloat80VmOp = class(TExprVirtMachOp)
  private
    FFunc: TFloat80Func;
  public
    constructor Create(AFunc: TFloat80Func);
    procedure Execute; override;
  end;

  TExprCallUnaryVmOp = class(TExprVirtMachOp)
  private
    FFunc: TUnaryFunc;
    FX: PFloat;
  public
    constructor Create(AFunc: TUnaryFunc; x: PFloat);
    procedure Execute; override;
  end;

  TExprCallUnary32VmOp = class(TExprVirtMachOp)
  private
    FFunc: TUnary32Func;
    FX: PFloat;
  public
    constructor Create(AFunc: TUnary32Func; x: PFloat);
    procedure Execute; override;
  end;

  TExprCallUnary64VmOp = class(TExprVirtMachOp)
  private
    FFunc: TUnary64Func;
    FX: PFloat;
  public
    constructor Create(AFunc: TUnary64Func; x: PFloat);
    procedure Execute; override;
  end;

  TExprCallUnary80VmOp = class(TExprVirtMachOp)
  private
    FFunc: TUnary80Func;
    FX: PFloat;
  public
    constructor Create(AFunc: TUnary80Func; x: PFloat);
    procedure Execute; override;
  end;

  TExprCallBinaryVmOp = class(TExprVirtMachOp)
  private
    FFunc: TBinaryFunc;
    FX, FY: PFloat;
  public
    constructor Create(AFunc: TBinaryFunc; x, y: PFloat);
    procedure Execute; override;
  end;

  TExprCallBinary32VmOp = class(TExprVirtMachOp)
  private
    FFunc: TBinary32Func;
    FX, FY: PFloat;
  public
    constructor Create(AFunc: TBinary32Func; x, y: PFloat);
    procedure Execute; override;
  end;

  TExprCallBinary64VmOp = class(TExprVirtMachOp)
  private
    FFunc: TBinary64Func;
    FX, FY: PFloat;
  public
    constructor Create(AFunc: TBinary64Func; x, y: PFloat);
    procedure Execute; override;
  end;

  TExprCallBinary80VmOp = class(TExprVirtMachOp)
  private
    FFunc: TBinary80Func;
    FX, FY: PFloat;
  public
    constructor Create(AFunc: TBinary80Func; x, y: PFloat);
    procedure Execute; override;
  end;

  TExprCallTernaryVmOp = class(TExprVirtMachOp)
  private
    FFunc: TTernaryFunc;
    FX, FY, FZ: PFloat;
  public
    constructor Create(AFunc: TTernaryFunc; x, y, z: PFloat);
    procedure Execute; override;
  end;

  TExprCallTernary32VmOp = class(TExprVirtMachOp)
  private
    FFunc: TTernary32Func;
    FX, FY, FZ: PFloat;
  public
    constructor Create(AFunc: TTernary32Func; x, y, z: PFloat);
    procedure Execute; override;
  end;

  TExprCallTernary64VmOp = class(TExprVirtMachOp)
  private
    FFunc: TTernary64Func;
    FX, FY, FZ: PFloat;
  public
    constructor Create(AFunc: TTernary64Func; x, y, z: PFloat);
    procedure Execute; override;
  end;

  TExprCallTernary80VmOp = class(TExprVirtMachOp)
  private
    FFunc: TTernary80Func;
    FX, FY, FZ: PFloat;
  public
    constructor Create(AFunc: TTernary80Func; x, y, z: PFloat);
    procedure Execute; override;
  end;

//==================================================================================================
// TExprVar32VmOp
//==================================================================================================

procedure TExprVar32VmOp.Execute;
begin
  FOutput := PFloat32(FVarLoc)^;
end;

//==================================================================================================
// TExprVar64VmOp
//==================================================================================================

procedure TExprVar64VmOp.Execute;
begin
  FOutput := PFloat64(FVarLoc)^;
end;

//==================================================================================================
// TExprVar80VmOp
//==================================================================================================

procedure TExprVar80VmOp.Execute;
begin
  FOutput := PFloat80(FVarLoc)^;
end;

//==================================================================================================
// TExprConstVmOp
//==================================================================================================

constructor TExprConstVmOp.Create(AValue: TFloat);
begin
  FOutput := AValue;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprConstVmOp.Execute;
begin
end;

//==================================================================================================
// TExprUnaryVmOp
//==================================================================================================

constructor TExprUnaryVmOp.Create(AInput: PFloat);
begin
  FInput := AInput;
end;

//==================================================================================================
// TExprBinaryVmOp
//==================================================================================================

constructor TExprBinaryVmOp.Create(ALeft, ARight: PFloat);
begin
  FLeft := ALeft;
  FRight := ARight;
end;

//==================================================================================================
// TExprAddVmOp
//==================================================================================================

procedure TExprAddVmOp.Execute;
begin
  FOutput := FLeft^ + FRight^;
end;

//==================================================================================================
// TExprSubtractVmOp
//==================================================================================================

procedure TExprSubtractVmOp.Execute;
begin
  FOutput := FLeft^ - FRight^;
end;

//==================================================================================================
// TExprMultiplyVmOp
//==================================================================================================

procedure TExprMultiplyVmOp.Execute;
begin
  FOutput := FLeft^ * FRight^;
end;

//==================================================================================================
// TExprDivideVmOp
//==================================================================================================

procedure TExprDivideVmOp.Execute;
begin
  FOutput := FLeft^ / FRight^;
end;

//==================================================================================================
// TExprCompareVmOp
//==================================================================================================

procedure TExprCompareVmOp.Execute;
begin
  if FLeft^ < FRight^ then
    FOutput := -1
  else if FLeft^ > FRight^ then
    FOutput := 1
  else
    FOutput := 0;
end;

//==================================================================================================
// TExprNegateVmOp
//==================================================================================================

procedure TExprNegateVmOp.Execute;
begin
  FOutput := - FInput^;
end;

//==================================================================================================
// TExprVarVmOp
//==================================================================================================

constructor TExprVarVmOp.Create(AVarLoc: Pointer);
begin
  FVarLoc := AVarLoc;
end;

//==================================================================================================
// TExprCallFloatVmOp
//==================================================================================================

constructor TExprCallFloatVmOp.Create(AFunc: TFloatFunc);
begin
  FFunc := AFunc;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallFloatVmOp.Execute;
begin
  FOutput := FFunc;
end;

//==================================================================================================
// TExprCallFloat32VmOp
//==================================================================================================

constructor TExprCallFloat32VmOp.Create(AFunc: TFloat32Func);
begin
  FFunc := AFunc;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallFloat32VmOp.Execute;
begin
  FOutput := FFunc;
end;

//==================================================================================================
// TExprCallFloat64VmOp
//==================================================================================================

constructor TExprCallFloat64VmOp.Create(AFunc: TFloat64Func);
begin
  FFunc := AFunc;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallFloat64VmOp.Execute;
begin
  FOutput := FFunc;
end;

//==================================================================================================
// TExprCallFloat80VmOp
//==================================================================================================

constructor TExprCallFloat80VmOp.Create(AFunc: TFloat80Func);
begin
  FFunc := AFunc;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallFloat80VmOp.Execute;
begin
  FOutput := FFunc;
end;

//==================================================================================================
// TExprCallUnaryVmOp
//==================================================================================================

constructor TExprCallUnaryVmOp.Create(AFunc: TUnaryFunc; x: PFloat);
begin
  FFunc := AFunc;
  FX := x;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallUnaryVmOp.Execute;
begin
  FOutput := FFunc(FX^);
end;

//==================================================================================================
// TExprCallUnary32VmOp
//==================================================================================================

constructor TExprCallUnary32VmOp.Create(AFunc: TUnary32Func; x: PFloat);
begin
  FFunc := AFunc;
  FX := x;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallUnary32VmOp.Execute;
begin
  FOutput := FFunc(FX^);
end;

//==================================================================================================
// TExprCallUnary64VmOp
//==================================================================================================

constructor TExprCallUnary64VmOp.Create(AFunc: TUnary64Func; x: PFloat);
begin
  FFunc := AFunc;
  FX := x;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallUnary64VmOp.Execute;
begin
  FOutput := FFunc(FX^);
end;

//==================================================================================================
// TExprCallUnary80VmOp
//==================================================================================================

constructor TExprCallUnary80VmOp.Create(AFunc: TUnary80Func; x: PFloat);
begin
  FFunc := AFunc;
  FX := x;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallUnary80VmOp.Execute;
begin
  FOutput := FFunc(FX^);
end;

//==================================================================================================
// TExprCallBinaryVmOp
//==================================================================================================

constructor TExprCallBinaryVmOp.Create(AFunc: TBinaryFunc; x, y: PFloat);
begin
  FFunc := AFunc;
  FX := x;
  FY := y;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallBinaryVmOp.Execute;
begin
  FOutput := FFunc(FX^, FY^);
end;

//==================================================================================================
// TExprCallBinary32VmOp
//==================================================================================================

constructor TExprCallBinary32VmOp.Create(AFunc: TBinary32Func; x, y: PFloat);
begin
  FFunc := AFunc;
  FX := x;
  FY := y;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallBinary32VmOp.Execute;
begin
  FOutput := FFunc(FX^, FY^);
end;

//==================================================================================================
// TExprCallBinary64VmOp
//==================================================================================================

constructor TExprCallBinary64VmOp.Create(AFunc: TBinary64Func; x, y: PFloat);
begin
  FFunc := AFunc;
  FX := x;
  FY := y;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallBinary64VmOp.Execute;
begin
  FOutput := FFunc(FX^, FY^);
end;

//==================================================================================================
// TExprCallBinary80VmOp
//==================================================================================================

constructor TExprCallBinary80VmOp.Create(AFunc: TBinary80Func; x, y: PFloat);
begin
  FFunc := AFunc;
  FX := x;
  FY := y;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallBinary80VmOp.Execute;
begin
  FOutput := FFunc(FX^, FY^);
end;

//==================================================================================================
// TExprCallTernaryVmOp
//==================================================================================================

constructor TExprCallTernaryVmOp.Create(AFunc: TTernaryFunc; x, y, z: PFloat);
begin
  FFunc := AFunc;
  FX := x;
  FY := y;
  FZ := z;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallTernaryVmOp.Execute;
begin
  FOutput := FFunc(FX^, FY^, FZ^);
end;

//==================================================================================================
// TExprCallTernary32VmOp
//==================================================================================================

constructor TExprCallTernary32VmOp.Create(AFunc: TTernary32Func; x, y, z: PFloat);
begin
  FFunc := AFunc;
  FX := x;
  FY := y;
  FZ := z;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallTernary32VmOp.Execute;
begin
  FOutput := FFunc(FX^, FY^, FZ^);
end;

//==================================================================================================
// TExprCallTernary64VmOp
//==================================================================================================

constructor TExprCallTernary64VmOp.Create(AFunc: TTernary64Func; x, y, z: PFloat);
begin
  FFunc := AFunc;
  FX := x;
  FY := y;
  FZ := z;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallTernary64VmOp.Execute;
begin
  FOutput := FFunc(FX^, FY^, FZ^);
end;

//==================================================================================================
// TExprCallTernary80VmOp
//==================================================================================================

constructor TExprCallTernary80VmOp.Create(AFunc: TTernary80Func; x, y, z: PFloat);
begin
  FFunc := AFunc;
  FX := x;
  FY := y;
  FZ := z;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallTernary80VmOp.Execute;
begin
  FOutput := FFunc(FX^, FY^, FZ^);
end;

{ End of virtual machine operators }

//==================================================================================================
// TExprVirtMach
//==================================================================================================

constructor TExprVirtMach.Create;
begin
  FCodeList := TList.Create;
  FConstList := TList.Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TExprVirtMach.Destroy;
begin
  FreeObjectList(FCodeList);
  FreeObjectList(FConstList);
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMach.Execute: TFloat;
type
  PExprVirtMachOp = ^TExprVirtMachOp;
var
  i: Integer;
  pop: PExprVirtMachOp;
begin
  if FCodeList.Count <> 0 then
  begin
    { The code that follows is the same as this, but a lot faster
    for i := 0 to FCodeList.Count - 1 do
      TExprVirtMachOp(FCodeList[i]).Execute; }
    i := FCodeList.Count;
    pop := @FCodeList.List^[0];
    while i > 0 do
    begin
      pop^.Execute;
      Inc(pop);
      Dec(i);
    end;
    Result := TExprVirtMachOp(FCodeList[FCodeList.Count - 1]).FOutput;
  end
  else
    Result := 0;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprVirtMach.Add(AOp: TExprVirtMachOp);
begin
  FCodeList.Add(AOp);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprVirtMach.AddConst(AOp: TExprVirtMachOp);
begin
  FConstList.Add(AOp);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprVirtMach.Clear;
begin
  ClearObjectList(FCodeList);
  ClearObjectList(FConstList);
end;

//==================================================================================================
// TExprVirtMachNode
//==================================================================================================

type
  TExprVirtMachNode = class(TExprNode)
  private
    FExprVmCode: TExprVirtMachOp;
    function GetVmDeps(AIndex: Integer): TExprVirtMachNode;
  public
    procedure GenCode(AVirtMach: TExprVirtMach); virtual; abstract;

    property ExprVmCode: TExprVirtMachOp read FExprVmCode;

    { this property saves typecasting to access ExprVmCode }
    property VmDeps[AIndex: Integer]: TExprVirtMachNode read GetVmDeps; default;
  end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNode.GetVmDeps(AIndex: Integer): TExprVirtMachNode;
begin
  Result := TExprVirtMachNode(FDepList[AIndex]);
end;

//==================================================================================================
// Concrete expression nodes for virtual machine
//==================================================================================================

type
  TExprUnaryVmNode = class(TExprVirtMachNode)
  private
    FUnaryClass: TExprUnaryVmOpClass;
  public
    constructor Create(AUnaryClass: TExprUnaryVmOpClass;
      const ADeps: array of TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprBinaryVmNode = class(TExprVirtMachNode)
  private
    FBinaryClass: TExprBinaryVmOpClass;
  public
    constructor Create(ABinaryClass: TExprBinaryVmOpClass;
      const ADeps: array of TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprConstVmNode = class(TExprVirtMachNode)
  private
    FValue: TFloat;
  public
    constructor Create(AValue: TFloat);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprVar32VmNode = class(TExprVirtMachNode)
  private
    FValue: PFloat32;
  public
    constructor Create(AValue: PFloat32);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprVar64VmNode = class(TExprVirtMachNode)
  private
    FValue: PFloat64;
  public
    constructor Create(AValue: PFloat64);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprVar80VmNode = class(TExprVirtMachNode)
  private
    FValue: PFloat80;
  public
    constructor Create(AValue: PFloat80);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallFloatVmNode = class(TExprVirtMachNode)
  private
    FFunc: TFloatFunc;
  public
    constructor Create(AFunc: TFloatFunc);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallFloat32VmNode = class(TExprVirtMachNode)
  private
    FFunc: TFloat32Func;
  public
    constructor Create(AFunc: TFloat32Func);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallFloat64VmNode = class(TExprVirtMachNode)
  private
    FFunc: TFloat64Func;
  public
    constructor Create(AFunc: TFloat64Func);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallFloat80VmNode = class(TExprVirtMachNode)
  private
    FFunc: TFloat80Func;
  public
    constructor Create(AFunc: TFloat80Func);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallUnaryVmNode = class(TExprVirtMachNode)
  private
    FFunc: TUnaryFunc;
  public
    constructor Create(AFunc: TUnaryFunc; x: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallUnary32VmNode = class(TExprVirtMachNode)
  private
    FFunc: TUnary32Func;
  public
    constructor Create(AFunc: TUnary32Func; x: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallUnary64VmNode = class(TExprVirtMachNode)
  private
    FFunc: TUnary64Func;
  public
    constructor Create(AFunc: TUnary64Func; x: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallUnary80VmNode = class(TExprVirtMachNode)
  private
    FFunc: TUnary80Func;
  public
    constructor Create(AFunc: TUnary80Func; x: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallBinaryVmNode = class(TExprVirtMachNode)
  private
    FFunc: TBinaryFunc;
  public
    constructor Create(AFunc: TBinaryFunc; x, y: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallBinary32VmNode = class(TExprVirtMachNode)
  private
    FFunc: TBinary32Func;
  public
    constructor Create(AFunc: TBinary32Func; x, y: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallBinary64VmNode = class(TExprVirtMachNode)
  private
    FFunc: TBinary64Func;
  public
    constructor Create(AFunc: TBinary64Func; x, y: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallBinary80VmNode = class(TExprVirtMachNode)
  private
    FFunc: TBinary80Func;
  public
    constructor Create(AFunc: TBinary80Func; x, y: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallTernaryVmNode = class(TExprVirtMachNode)
  private
    FFunc: TTernaryFunc;
  public
    constructor Create(AFunc: TTernaryFunc; x, y, z: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallTernary32VmNode = class(TExprVirtMachNode)
  private
    FFunc: TTernary32Func;
  public
    constructor Create(AFunc: TTernary32Func; x, y, z: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallTernary64VmNode = class(TExprVirtMachNode)
  private
    FFunc: TTernary64Func;
  public
    constructor Create(AFunc: TTernary64Func; x, y, z: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallTernary80VmNode = class(TExprVirtMachNode)
  private
    FFunc: TTernary80Func;
  public
    constructor Create(AFunc: TTernary80Func; x, y, z: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCompareVmNode = class(TExprVirtMachNode)
  public
    constructor Create(ALeft, ARight: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

//==================================================================================================
// TExprUnaryVmNode
//==================================================================================================

constructor TExprUnaryVmNode.Create(AUnaryClass: TExprUnaryVmOpClass; const ADeps: array of TExprNode);
begin
  FUnaryClass := AUnaryClass;
  inherited Create(ADeps);
  Assert(FDepList.Count = 1);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprUnaryVmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := FUnaryClass.Create(VmDeps[0].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

//==================================================================================================
// TExprBinaryVmNode
//==================================================================================================

constructor TExprBinaryVmNode.Create(ABinaryClass: TExprBinaryVmOpClass; const ADeps: array of TExprNode);
begin
  FBinaryClass := ABinaryClass;
  inherited Create(ADeps);
  Assert(FDepList.Count = 2);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprBinaryVmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := FBinaryClass.Create(
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc
  );
  AVirtMach.Add(FExprVmCode);
end;

//==================================================================================================
// TExprConstVmNode
//==================================================================================================

constructor TExprConstVmNode.Create(AValue: TFloat);
begin
  FValue := AValue;
  inherited Create([]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprConstVmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprConstVmOp.Create(FValue);
  AVirtMach.AddConst(FExprVmCode);
end;

//==================================================================================================
// TExprVar32VmNode
//==================================================================================================

constructor TExprVar32VmNode.Create(AValue: PFloat32);
begin
  FValue := AValue;
  inherited Create([]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprVar32VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprVar32VmOp.Create(FValue);
  AVirtMach.Add(FExprVmCode);
end;

//==================================================================================================
// TExprVar64VmNode
//==================================================================================================

constructor TExprVar64VmNode.Create(AValue: PFloat64);
begin
  FValue := AValue;
  inherited Create([]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprVar64VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprVar64VmOp.Create(FValue);
  AVirtMach.Add(FExprVmCode);
end;

//==================================================================================================
// TExprVar80VmNode
//==================================================================================================

constructor TExprVar80VmNode.Create(AValue: PFloat80);
begin
  FValue := AValue;
  inherited Create([]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprVar80VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprVar80VmOp.Create(FValue);
  AVirtMach.Add(FExprVmCode);
end;

{ End of expression nodes for virtual machine }

//==================================================================================================
// TExprVirtMachNodeFactory
//==================================================================================================

constructor TExprVirtMachNodeFactory.Create;
begin
  FNodeList := TList.Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TExprVirtMachNodeFactory.Destroy;
begin
  FreeObjectList(FNodeList);
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.AddNode(ANode: TExprNode): TExprNode;
begin
  Result := ANode;
  FNodeList.Add(ANode);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprVirtMachNodeFactory.GenCode(AVirtMach: TExprVirtMach);
begin
  {TODO: optimize the expression tree into a DAG (i.e. find CSEs) and
    evaluate constant subexpressions, implement strength reduction, etc. }

  {TODO: move optimization logic (as far as possible) into ancestor classes
    once tested and interfaces are solid, so that other evaluation strategies
    can take advantage of these optimizations. }

  DoClean(AVirtMach);
  DoConsts(AVirtMach);
  DoCode(AVirtMach);
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.LoadVar32(ALoc: PFloat32): TExprNode;
begin
  Result := AddNode(TExprVar32VmNode.Create(ALoc));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.LoadVar64(ALoc: PFloat64): TExprNode;
begin
  Result := AddNode(TExprVar64VmNode.Create(ALoc));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.LoadVar80(ALoc: PFloat80): TExprNode;
begin
  Result := AddNode(TExprVar80VmNode.Create(ALoc));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.LoadConst32(AValue: TFloat32): TExprNode;
begin
  Result := AddNode(TExprConstVmNode.Create(AValue));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.LoadConst64(AValue: TFloat64): TExprNode;
begin
  Result := AddNode(TExprConstVmNode.Create(AValue));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.LoadConst80(AValue: TFloat80): TExprNode;
begin
  Result := AddNode(TExprConstVmNode.Create(AValue));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.Add(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprAddVmOp, [ALeft, ARight]));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.Subtract(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprSubtractVmOp, [ALeft, ARight]));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.Multiply(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprMultiplyVmOp, [ALeft, ARight]));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.Divide(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprDivideVmOp, [ALeft, ARight]));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.Negate(AValue: TExprNode): TExprNode;
begin
  Result := AddNode(TExprUnaryVmNode.Create(TExprNegateVmOp, [AValue]));
end;

//--------------------------------------------------------------------------------------------------

procedure TExprVirtMachNodeFactory.DoClean(AVirtMach: TExprVirtMach);
var
  i: Integer;
begin
  { clean up in preparation for code generation }
  AVirtMach.Clear;
  for i := 0 to FNodeList.Count - 1 do
    TExprVirtMachNode(FNodeList[i]).FExprVmCode := nil;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprVirtMachNodeFactory.DoConsts(AVirtMach: TExprVirtMach);
var
  i: Integer;
  node: TExprVirtMachNode;
begin
  { process consts }
  for i := 0 to FNodeList.Count - 1 do
  begin
    node := TExprVirtMachNode(FNodeList[i]);
    if (node is TExprConstVmNode) and (node.ExprVmCode = nil) then
      node.GenCode(AVirtMach);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TExprVirtMachNodeFactory.DoCode(AVirtMach: TExprVirtMach);
var
  i: Integer;
  node: TExprVirtMachNode;
begin
  { process code }
  for i := 0 to FNodeList.Count - 1 do
  begin
    node := TExprVirtMachNode(FNodeList[i]);
    if node.ExprVmCode = nil then
      node.GenCode(AVirtMach);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.CallFloatFunc(AFunc: TFloatFunc): TExprNode;
begin
  Result := AddNode(TExprCallFloatVmNode.Create(AFunc));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.CallFloat32Func(AFunc: TFloat32Func): TExprNode;
begin
  Result := AddNode(TExprCallFloat32VmNode.Create(AFunc));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.CallFloat64Func(AFunc: TFloat64Func): TExprNode;
begin
  Result := AddNode(TExprCallFloat64VmNode.Create(AFunc));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.CallFloat80Func(AFunc: TFloat80Func): TExprNode;
begin
  Result := AddNode(TExprCallFloat80VmNode.Create(AFunc));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.CallUnaryFunc(AFunc: TUnaryFunc; x: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCallUnaryVmNode.Create(AFunc, x));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.CallUnary32Func(AFunc: TUnary32Func; x: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCallUnary32VmNode.Create(AFunc, x));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.CallUnary64Func(AFunc: TUnary64Func; x: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCallUnary64VmNode.Create(AFunc, x));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.CallUnary80Func(AFunc: TUnary80Func; x: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCallUnary80VmNode.Create(AFunc, x));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.CallBinaryFunc(AFunc: TBinaryFunc; x, y: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCallBinaryVmNode.Create(AFunc, x, y));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.CallBinary32Func(AFunc: TBinary32Func; x, y: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCallBinary32VmNode.Create(AFunc, x, y));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.CallBinary64Func(AFunc: TBinary64Func; x, y: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCallBinary64VmNode.Create(AFunc, x, y));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.CallBinary80Func(AFunc: TBinary80Func; x, y: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCallBinary80VmNode.Create(AFunc, x, y));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.CallTernaryFunc(AFunc: TTernaryFunc; x, y, z: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCallTernaryVmNode.Create(AFunc, x, y, z));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.CallTernary32Func(AFunc: TTernary32Func; x, y, z: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCallTernary32VmNode.Create(AFunc, x, y, z));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.CallTernary64Func(AFunc: TTernary64Func; x, y, z: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCallTernary64VmNode.Create(AFunc, x, y, z));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.CallTernary80Func(AFunc: TTernary80Func; x, y, z: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCallTernary80VmNode.Create(AFunc, x, y, z));
end;

//--------------------------------------------------------------------------------------------------

function TExprVirtMachNodeFactory.Compare(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCompareVmNode.Create(ALeft, ARight));
end;

//==================================================================================================
// TCompiledEvaluator
//==================================================================================================

constructor TCompiledEvaluator.Create;
begin
  inherited Create;
  FVirtMach := TExprVirtMach.Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TCompiledEvaluator.Destroy;
begin
  FVirtMach.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TCompiledEvaluator.Compile(const AExpr: string);
var
  lex: TExprSimpleLexer;
  parse: TExprCompileParser;
  nodeFactory: TExprVirtMachNodeFactory;
begin
  if AExpr <> FExpr then
  begin
    FExpr := AExpr;
    FVirtMach.Clear;

    parse := nil;
    nodeFactory := nil;
    lex := TExprSimpleLexer.Create(FExpr);
    try
      nodeFactory := TExprVirtMachNodeFactory.Create;
      parse := TExprCompileParser.Create(lex, nodeFactory);
      parse.Context := InternalContextSet;
      parse.Compile;
      nodeFactory.GenCode(FVirtMach);
    finally
      parse.Free;
      nodeFactory.Free;
      lex.Free;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TCompiledEvaluator.Evaluate: TFloat;
begin
  Result := FVirtMach.Execute;
end;

//==================================================================================================
// TExprVar32Sym
//==================================================================================================

constructor TExprVar32Sym.Create(const AIdent: string; ALoc: PFloat32);
begin
  Assert(ALoc <> nil);
  FLoc := ALoc;
  inherited Create(AIdent);
end;

//--------------------------------------------------------------------------------------------------

function TExprVar32Sym.Compile: TExprNode;
begin
  Result := NodeFactory.LoadVar32(FLoc);
end;

//--------------------------------------------------------------------------------------------------

function TExprVar32Sym.Evaluate: TFloat;
begin
  Result := FLoc^;
end;

//==================================================================================================
// TExprVar64Sym
//==================================================================================================

constructor TExprVar64Sym.Create(const AIdent: string; ALoc: PFloat64);
begin
  Assert(ALoc <> nil);
  FLoc := ALoc;
  inherited Create(AIdent);
end;

//--------------------------------------------------------------------------------------------------

function TExprVar64Sym.Compile: TExprNode;
begin
  Result := NodeFactory.LoadVar64(FLoc);
end;

//--------------------------------------------------------------------------------------------------

function TExprVar64Sym.Evaluate: TFloat;
begin
  Result := FLoc^;
end;

//==================================================================================================
// TExprVar80Sym
//==================================================================================================

constructor TExprVar80Sym.Create(const AIdent: string; ALoc: PFloat80);
begin
  Assert(ALoc <> nil);
  FLoc := ALoc;
  inherited Create(AIdent);
end;

//--------------------------------------------------------------------------------------------------

function TExprVar80Sym.Compile: TExprNode;
begin
  Result := NodeFactory.LoadVar80(FLoc);
end;

//--------------------------------------------------------------------------------------------------

function TExprVar80Sym.Evaluate: TFloat;
begin
  Result := FLoc^;
end;

//==================================================================================================
// TExprCallFloatVmNode
//==================================================================================================

constructor TExprCallFloatVmNode.Create(AFunc: TFloatFunc);
begin
  FFunc := AFunc;
  inherited Create([]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallFloatVmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallFloatVmOp.Create(FFunc);
  AVirtMach.Add(FExprVmCode);
end;

//==================================================================================================
// TExprCallFloat32VmNode
//==================================================================================================

constructor TExprCallFloat32VmNode.Create(AFunc: TFloat32Func);
begin
  FFunc := AFunc;
  inherited Create([]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallFloat32VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallFloat32VmOp.Create(FFunc);
  AVirtMach.Add(FExprVmCode);
end;

//==================================================================================================
// TExprCallFloat64VmNode
//==================================================================================================

constructor TExprCallFloat64VmNode.Create(AFunc: TFloat64Func);
begin
  FFunc := AFunc;
  inherited Create([]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallFloat64VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallFloat64VmOp.Create(FFunc);
  AVirtMach.Add(FExprVmCode);
end;

//==================================================================================================
// TExprCallFloat80VmNode
//==================================================================================================

constructor TExprCallFloat80VmNode.Create(AFunc: TFloat80Func);
begin
  FFunc := AFunc;
  inherited Create([]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallFloat80VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallFloat80VmOp.Create(FFunc);
  AVirtMach.Add(FExprVmCode);
end;

//==================================================================================================
// TExprCallUnaryVmNode
//==================================================================================================

constructor TExprCallUnaryVmNode.Create(AFunc: TUnaryFunc; x: TExprNode);
begin
  FFunc := AFunc;
  inherited Create([x]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallUnaryVmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallUnaryVmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc
  );
  AVirtMach.Add(FExprVmCode);
end;

//==================================================================================================
// TExprCallUnary32VmNode
//==================================================================================================

constructor TExprCallUnary32VmNode.Create(AFunc: TUnary32Func; x: TExprNode);
begin
  FFunc := AFunc;
  inherited Create([x]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallUnary32VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallUnary32VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc
  );
  AVirtMach.Add(FExprVmCode);
end;

//==================================================================================================
// TExprCallUnary64VmNode
//==================================================================================================

constructor TExprCallUnary64VmNode.Create(AFunc: TUnary64Func; x: TExprNode);
begin
  FFunc := AFunc;
  inherited Create([x]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallUnary64VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallUnary64VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc
  );
  AVirtMach.Add(FExprVmCode);
end;

//==================================================================================================
// TExprCallUnary80VmNode
//==================================================================================================

constructor TExprCallUnary80VmNode.Create(AFunc: TUnary80Func; x: TExprNode);
begin
  FFunc := AFunc;
  inherited Create([x]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallUnary80VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallUnary80VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc
  );
  AVirtMach.Add(FExprVmCode);
end;

//==================================================================================================
// TExprCallBinaryVmNode
//==================================================================================================

constructor TExprCallBinaryVmNode.Create(AFunc: TBinaryFunc; x, y: TExprNode);
begin
  FFunc := AFunc;
  inherited Create([x, y]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallBinaryVmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallBinaryVmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc
  );
  AVirtMach.Add(FExprVmCode);
end;

//==================================================================================================
// TExprCallBinary32VmNode
//==================================================================================================

constructor TExprCallBinary32VmNode.Create(AFunc: TBinary32Func; x, y: TExprNode);
begin
  FFunc := AFunc;
  inherited Create([x, y]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallBinary32VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallBinary32VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc
  );
  AVirtMach.Add(FExprVmCode);
end;

//==================================================================================================
// TExprCallBinary64VmNode
//==================================================================================================

constructor TExprCallBinary64VmNode.Create(AFunc: TBinary64Func; x, y: TExprNode);
begin
  FFunc := AFunc;
  inherited Create([x, y]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallBinary64VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallBinary64VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc
  );
  AVirtMach.Add(FExprVmCode);
end;

//==================================================================================================
// TExprCallBinary80VmNode
//==================================================================================================

constructor TExprCallBinary80VmNode.Create(AFunc: TBinary80Func; x, y: TExprNode);
begin
  FFunc := AFunc;
  inherited Create([x, y]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallBinary80VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallBinary80VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc
  );
  AVirtMach.Add(FExprVmCode);
end;

//==================================================================================================
// TExprCallTernaryVmNode
//==================================================================================================

constructor TExprCallTernaryVmNode.Create(AFunc: TTernaryFunc; x, y, z: TExprNode);
begin
  FFunc := AFunc;
  inherited Create([x, y, z]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallTernaryVmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallTernaryVmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc,
    VmDeps[2].ExprVmCode.OutputLoc
  );
  AVirtMach.Add(FExprVmCode);
end;

//==================================================================================================
// TExprCallTernary32VmNode
//==================================================================================================

constructor TExprCallTernary32VmNode.Create(AFunc: TTernary32Func; x, y, z: TExprNode);
begin
  FFunc := AFunc;
  inherited Create([x, y, z]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallTernary32VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallTernary32VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc,
    VmDeps[2].ExprVmCode.OutputLoc
  );
  AVirtMach.Add(FExprVmCode);
end;

//==================================================================================================
// TExprCallTernary64VmNode
//==================================================================================================

constructor TExprCallTernary64VmNode.Create(AFunc: TTernary64Func; x, y, z: TExprNode);
begin
  FFunc := AFunc;
  inherited Create([x, y, z]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallTernary64VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallTernary64VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc,
    VmDeps[2].ExprVmCode.OutputLoc
  );
  AVirtMach.Add(FExprVmCode);
end;

//==================================================================================================
// TExprCallTernary80VmNode
//==================================================================================================

constructor TExprCallTernary80VmNode.Create(AFunc: TTernary80Func; x, y, z: TExprNode);
begin
  FFunc := AFunc;
  inherited Create([x, y, z]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCallTernary80VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallTernary80VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc,
    VmDeps[2].ExprVmCode.OutputLoc
  );
  AVirtMach.Add(FExprVmCode);
end;

//==================================================================================================
// TExprCompareVmNode
//==================================================================================================

constructor TExprCompareVmNode.Create(ALeft, ARight: TExprNode);
begin
  inherited Create([ALeft, ARight]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprCompareVmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCompareVmOp.Create(
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc
  );
  AVirtMach.Add(FExprVmCode);
end;

//==================================================================================================
// TExprAbstractFuncSym
//==================================================================================================

function TExprAbstractFuncSym.CompileFirstArg: TExprNode;
begin
  if Lexer.CurrTok <> etLParen then
    raise EJclExprEvalError.CreateResRec(@RsExprEvalFirstArg);
  Result := CompileParser.compile_expr(True);
end;

//--------------------------------------------------------------------------------------------------

function TExprAbstractFuncSym.CompileNextArg: TExprNode;
begin
  if Lexer.CurrTok <> etComma then
    raise EJclExprEvalError.CreateResRec(@RsExprEvalNextArg);
  Result := CompileParser.compile_expr(True);
end;

//--------------------------------------------------------------------------------------------------

function TExprAbstractFuncSym.EvalFirstArg: TFloat;
begin
  if Lexer.CurrTok <> etLParen then
    raise EJclExprEvalError.CreateResRec(@RsExprEvalFirstArg);
  Result := EvalParser.eval_expr(True);
end;

//--------------------------------------------------------------------------------------------------

function TExprAbstractFuncSym.EvalNextArg: TFloat;
begin
  if Lexer.CurrTok <> etComma then
    raise EJclExprEvalError.CreateResRec(@RsExprEvalNextArg);
  Result := EvalParser.eval_expr(True);
end;

//--------------------------------------------------------------------------------------------------

procedure TExprAbstractFuncSym.EndArgs;
begin
  if Lexer.CurrTok <> etRParen then
    raise EJclExprEvalError.CreateResRec(@RsExprEvalEndArgs);
  Lexer.NextTok;
end;

//==================================================================================================
// TExprFuncSym
//==================================================================================================

constructor TExprFuncSym.Create(const AIdent: string; AFunc: TFloatFunc);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

//--------------------------------------------------------------------------------------------------

function TExprFuncSym.Compile: TExprNode;
begin
  Result := NodeFactory.CallFloatFunc(FFunc);
end;

//--------------------------------------------------------------------------------------------------

function TExprFuncSym.Evaluate: TFloat;
begin
  Result := FFunc;
end;

//==================================================================================================
// TExprFloat32FuncSym
//==================================================================================================

constructor TExprFloat32FuncSym.Create(const AIdent: string; AFunc: TFloat32Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

//--------------------------------------------------------------------------------------------------

function TExprFloat32FuncSym.Compile: TExprNode;
begin
  Result := NodeFactory.CallFloat32Func(FFunc);
end;

//--------------------------------------------------------------------------------------------------

function TExprFloat32FuncSym.Evaluate: TFloat;
begin
  Result := FFunc;
end;

//==================================================================================================
// TExprFloat64FuncSym
//==================================================================================================

constructor TExprFloat64FuncSym.Create(const AIdent: string; AFunc: TFloat64Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

//--------------------------------------------------------------------------------------------------

function TExprFloat64FuncSym.Compile: TExprNode;
begin
  Result := NodeFactory.CallFloat64Func(FFunc);
end;

//--------------------------------------------------------------------------------------------------

function TExprFloat64FuncSym.Evaluate: TFloat;
begin
  Result := FFunc;
end;

//==================================================================================================
// TExprFloat80FuncSym
//==================================================================================================

constructor TExprFloat80FuncSym.Create(const AIdent: string; AFunc: TFloat80Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

//--------------------------------------------------------------------------------------------------

function TExprFloat80FuncSym.Compile: TExprNode;
begin
  Result := NodeFactory.CallFloat80Func(FFunc);
end;

//--------------------------------------------------------------------------------------------------

function TExprFloat80FuncSym.Evaluate: TFloat;
begin
  Result := FFunc;
end;

//==================================================================================================
// TExprUnaryFuncSym
//==================================================================================================

constructor TExprUnaryFuncSym.Create(const AIdent: string; AFunc: TUnaryFunc);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

//--------------------------------------------------------------------------------------------------

function TExprUnaryFuncSym.Compile: TExprNode;
begin
  Result := NodeFactory.CallUnaryFunc(FFunc, CompileFirstArg);
  EndArgs;
end;

//--------------------------------------------------------------------------------------------------

function TExprUnaryFuncSym.Evaluate: TFloat;
begin
  Result := FFunc(EvalFirstArg);
  EndArgs;
end;

//==================================================================================================
// TExprUnary32FuncSym
//==================================================================================================

constructor TExprUnary32FuncSym.Create(const AIdent: string; AFunc: TUnary32Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

//--------------------------------------------------------------------------------------------------

function TExprUnary32FuncSym.Compile: TExprNode;
begin
  Result := NodeFactory.CallUnary32Func(FFunc, CompileFirstArg);
  EndArgs;
end;

//--------------------------------------------------------------------------------------------------

function TExprUnary32FuncSym.Evaluate: TFloat;
begin
  Result := FFunc(EvalFirstArg);
  EndArgs;
end;

//==================================================================================================
// TExprUnary64FuncSym
//==================================================================================================

constructor TExprUnary64FuncSym.Create(const AIdent: string; AFunc: TUnary64Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

//--------------------------------------------------------------------------------------------------

function TExprUnary64FuncSym.Compile: TExprNode;
begin
  Result := NodeFactory.CallUnary64Func(FFunc, CompileFirstArg);
  EndArgs;
end;

//--------------------------------------------------------------------------------------------------

function TExprUnary64FuncSym.Evaluate: TFloat;
begin
  Result := FFunc(EvalFirstArg);
  EndArgs;
end;

//==================================================================================================
// TExprUnary80FuncSym
//==================================================================================================

constructor TExprUnary80FuncSym.Create(const AIdent: string; AFunc: TUnary80Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

//--------------------------------------------------------------------------------------------------

function TExprUnary80FuncSym.Compile: TExprNode;
begin
  Result := NodeFactory.CallUnary80Func(FFunc, CompileFirstArg);
  EndArgs;
end;

//--------------------------------------------------------------------------------------------------

function TExprUnary80FuncSym.Evaluate: TFloat;
begin
  Result := FFunc(EvalFirstArg);
  EndArgs;
end;

//==================================================================================================
// TExprBinaryFuncSym
//==================================================================================================

constructor TExprBinaryFuncSym.Create(const AIdent: string; AFunc: TBinaryFunc);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

//--------------------------------------------------------------------------------------------------

function TExprBinaryFuncSym.Compile: TExprNode;
var
  x, y: TExprNode;
begin
  // must be called this way, because evaluation order of function
  // parameters is not defined; we need CompileFirstArg to be called
  // first.
  x := CompileFirstArg;
  y := CompileNextArg;
  EndArgs;
  Result := NodeFactory.CallBinaryFunc(FFunc, x, y);
end;

//--------------------------------------------------------------------------------------------------

function TExprBinaryFuncSym.Evaluate: TFloat;
var
  x, y: TFloat;
begin
  x := EvalFirstArg;
  y := EvalNextArg;
  Result := FFunc(x, y);
  EndArgs;
end;

//==================================================================================================
// TExprBinary32FuncSym
//==================================================================================================

constructor TExprBinary32FuncSym.Create(const AIdent: string; AFunc: TBinary32Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

//--------------------------------------------------------------------------------------------------

function TExprBinary32FuncSym.Compile: TExprNode;
var
  x, y: TExprNode;
begin
  x := CompileFirstArg;
  y := CompileNextArg;
  EndArgs;
  Result := NodeFactory.CallBinary32Func(FFunc, x, y);
end;

//--------------------------------------------------------------------------------------------------

function TExprBinary32FuncSym.Evaluate: TFloat;
var
  x, y: TFloat;
begin
  x := EvalFirstArg;
  y := EvalNextArg;
  EndArgs;
  Result := FFunc(x, y);
end;

//==================================================================================================
// TExprBinary64FuncSym
//==================================================================================================

constructor TExprBinary64FuncSym.Create(const AIdent: string; AFunc: TBinary64Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

//--------------------------------------------------------------------------------------------------

function TExprBinary64FuncSym.Compile: TExprNode;
var
  x, y: TExprNode;
begin
  x := CompileFirstArg;
  y := CompileNextArg;
  EndArgs;
  Result := NodeFactory.CallBinary64Func(FFunc, x, y);
end;

//--------------------------------------------------------------------------------------------------

function TExprBinary64FuncSym.Evaluate: TFloat;
var
  x, y: TFloat;
begin
  x := EvalFirstArg;
  y := EvalNextArg;
  EndArgs;
  Result := FFunc(x, y);
end;

//==================================================================================================
// TExprBinary80FuncSym
//==================================================================================================

constructor TExprBinary80FuncSym.Create(const AIdent: string; AFunc: TBinary80Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

//--------------------------------------------------------------------------------------------------

function TExprBinary80FuncSym.Compile: TExprNode;
var
  x, y: TExprNode;
begin
  x := CompileFirstArg;
  y := CompileNextArg;
  EndArgs;
  Result := NodeFactory.CallBinary80Func(FFunc, x, y);
end;

//--------------------------------------------------------------------------------------------------

function TExprBinary80FuncSym.Evaluate: TFloat;
var
  x, y: TFloat;
begin
  x := EvalFirstArg;
  y := EvalNextArg;
  EndArgs;
  Result := FFunc(x, y);
end;

//==================================================================================================
// TExprTernaryFuncSym
//==================================================================================================

constructor TExprTernaryFuncSym.Create(const AIdent: string; AFunc: TTernaryFunc);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

//--------------------------------------------------------------------------------------------------

function TExprTernaryFuncSym.Compile: TExprNode;
var
  x, y, z: TExprNode;
begin
  x := CompileFirstArg;
  y := CompileNextArg;
  z := CompileNextArg;
  EndArgs;
  Result := NodeFactory.CallTernaryFunc(FFunc, x, y, z);
end;

//--------------------------------------------------------------------------------------------------

function TExprTernaryFuncSym.Evaluate: TFloat;
var
  x, y, z: TFloat;
begin
  x := EvalFirstArg;
  y := EvalNextArg;
  z := EvalNextArg;
  EndArgs;
  Result := FFunc(x, y, z);
end;

//==================================================================================================
// TExprTernary32FuncSym
//==================================================================================================

constructor TExprTernary32FuncSym.Create(const AIdent: string; AFunc: TTernary32Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

//--------------------------------------------------------------------------------------------------

function TExprTernary32FuncSym.Compile: TExprNode;
var
  x, y, z: TExprNode;
begin
  x := CompileFirstArg;
  y := CompileNextArg;
  z := CompileNextArg;
  EndArgs;
  Result := NodeFactory.CallTernary32Func(FFunc, x, y, z);
end;

//--------------------------------------------------------------------------------------------------

function TExprTernary32FuncSym.Evaluate: TFloat;
var
  x, y, z: TFloat;
begin
  x := EvalFirstArg;
  y := EvalNextArg;
  z := EvalNextArg;
  EndArgs;
  Result := FFunc(x, y, z);
end;

//==================================================================================================
// TExprTernary64FuncSym
//==================================================================================================

constructor TExprTernary64FuncSym.Create(const AIdent: string; AFunc: TTernary64Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

//--------------------------------------------------------------------------------------------------

function TExprTernary64FuncSym.Compile: TExprNode;
var
  x, y, z: TExprNode;
begin
  x := CompileFirstArg;
  y := CompileNextArg;
  z := CompileNextArg;
  EndArgs;
  Result := NodeFactory.CallTernary64Func(FFunc, x, y, z);
end;

//--------------------------------------------------------------------------------------------------

function TExprTernary64FuncSym.Evaluate: TFloat;
var
  x, y, z: TFloat;
begin
  x := EvalFirstArg;
  y := EvalNextArg;
  z := EvalNextArg;
  EndArgs;
  Result := FFunc(x, y, z);
end;

//==================================================================================================
// TExprTernary80FuncSym
//==================================================================================================

constructor TExprTernary80FuncSym.Create(const AIdent: string; AFunc: TTernary80Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

//--------------------------------------------------------------------------------------------------

function TExprTernary80FuncSym.Compile: TExprNode;
begin
  Result := NodeFactory.CallTernary80Func(FFunc, CompileFirstArg,
    CompileNextArg, CompileNextArg);
  EndArgs;
end;

//--------------------------------------------------------------------------------------------------

function TExprTernary80FuncSym.Evaluate: TFloat;
var
  x, y, z: TFloat;
begin
  x := EvalFirstArg;
  y := EvalNextArg;
  z := EvalNextArg;
  EndArgs;
  Result := FFunc(x, y, z);
end;

//==================================================================================================
// TExprConstSym
//==================================================================================================

function TExprConstSym.Compile: TExprNode;
begin
  Result := NodeFactory.LoadConst(FValue);
end;

//--------------------------------------------------------------------------------------------------

constructor TExprConstSym.Create(const AIdent: string; AValue: TFloat);
begin
  inherited Create(AIdent);
  FValue := AValue;
end;

//--------------------------------------------------------------------------------------------------

function TExprConstSym.Evaluate: TFloat;
begin
  Result := FValue;
end;

//==================================================================================================
// TExprConst32Sym
//==================================================================================================

constructor TExprConst32Sym.Create(const AIdent: string; AValue: TFloat32);
begin
  inherited Create(AIdent);
  FValue := AValue;
end;

//--------------------------------------------------------------------------------------------------

function TExprConst32Sym.Compile: TExprNode;
begin
  Result := NodeFactory.LoadConst(FValue);
end;

//--------------------------------------------------------------------------------------------------

function TExprConst32Sym.Evaluate: TFloat;
begin
  Result := FValue;
end;

//==================================================================================================
// TExprConst64Sym
//==================================================================================================

constructor TExprConst64Sym.Create(const AIdent: string; AValue: TFloat64);
begin
  inherited Create(AIdent);
  FValue := AValue;
end;

//--------------------------------------------------------------------------------------------------

function TExprConst64Sym.Compile: TExprNode;
begin
  Result := NodeFactory.LoadConst(FValue);
end;

//--------------------------------------------------------------------------------------------------

function TExprConst64Sym.Evaluate: TFloat;
begin
  Result := FValue;
end;

//==================================================================================================
// TExprConst80Sym
//==================================================================================================

constructor TExprConst80Sym.Create(const AIdent: string; AValue: TFloat80);
begin
  inherited Create(AIdent);
  FValue := AValue;
end;

//--------------------------------------------------------------------------------------------------

function TExprConst80Sym.Compile: TExprNode;
begin
  Result := NodeFactory.LoadConst(FValue);
end;

//--------------------------------------------------------------------------------------------------

function TExprConst80Sym.Evaluate: TFloat;
begin
  Result := FValue;
end;

//==================================================================================================
// TEasyEvaluator
//==================================================================================================

constructor TEasyEvaluator.Create;
begin
  FOwnContext := TExprHashContext.Create(False, C_ExprEval_HashSize);
  FExtContextSet := TExprSetContext.Create(False);
  FInternalContextSet := TExprSetContext.Create(False);

  // user added names get precedence over external context's names
  FInternalContextSet.Add(FExtContextSet);
  FInternalContextSet.Add(FOwnContext);
end;

//--------------------------------------------------------------------------------------------------

destructor TEasyEvaluator.Destroy;
begin
  FInternalContextSet.Free;
  FOwnContext.Free;
  FExtContextSet.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TEasyEvaluator.AddConst(const AName: string; AConst: TFloat80);
begin
  FOwnContext.Add(TExprConst80Sym.Create(AName, AConst));
end;

//--------------------------------------------------------------------------------------------------

procedure TEasyEvaluator.AddConst(const AName: string; AConst: TFloat64);
begin
  FOwnContext.Add(TExprConst64Sym.Create(AName, AConst));
end;

//--------------------------------------------------------------------------------------------------

procedure TEasyEvaluator.AddConst(const AName: string; AConst: TFloat32);
begin
  FOwnContext.Add(TExprConst32Sym.Create(AName, AConst));
end;

//--------------------------------------------------------------------------------------------------

procedure TEasyEvaluator.AddVar(const AName: string; var AVar: TFloat32);
begin
  FOwnContext.Add(TExprVar32Sym.Create(AName, @AVar));
end;

//--------------------------------------------------------------------------------------------------

procedure TEasyEvaluator.AddVar(const AName: string; var AVar: TFloat64);
begin
  FOwnContext.Add(TExprVar64Sym.Create(AName, @AVar));
end;

//--------------------------------------------------------------------------------------------------

procedure TEasyEvaluator.AddVar(const AName: string; var AVar: TFloat80);
begin
  FOwnContext.Add(TExprVar80Sym.Create(AName, @AVar));
end;

//--------------------------------------------------------------------------------------------------

procedure TEasyEvaluator.AddFunc(const AName: string; AFunc: TFloat32Func);
begin
  FOwnContext.Add(TExprFloat32FuncSym.Create(AName, AFunc));
end;

//--------------------------------------------------------------------------------------------------

procedure TEasyEvaluator.AddFunc(const AName: string; AFunc: TFloat64Func);
begin
  FOwnContext.Add(TExprFloat64FuncSym.Create(AName, AFunc));
end;

//--------------------------------------------------------------------------------------------------

procedure TEasyEvaluator.AddFunc(const AName: string; AFunc: TFloat80Func);
begin
  FOwnContext.Add(TExprFloat80FuncSym.Create(AName, AFunc));
end;

//--------------------------------------------------------------------------------------------------

procedure TEasyEvaluator.AddFunc(const AName: string; AFunc: TUnary32Func);
begin
  FOwnContext.Add(TExprUnary32FuncSym.Create(AName, AFunc));
end;

//--------------------------------------------------------------------------------------------------

procedure TEasyEvaluator.AddFunc(const AName: string; AFunc: TUnary64Func);
begin
  FOwnContext.Add(TExprUnary64FuncSym.Create(AName, AFunc));
end;

//--------------------------------------------------------------------------------------------------

procedure TEasyEvaluator.AddFunc(const AName: string; AFunc: TUnary80Func);
begin
  FOwnContext.Add(TExprUnary80FuncSym.Create(AName, AFunc));
end;

//--------------------------------------------------------------------------------------------------

procedure TEasyEvaluator.AddFunc(const AName: string; AFunc: TBinary32Func);
begin
  FOwnContext.Add(TExprBinary32FuncSym.Create(AName, AFunc));
end;

//--------------------------------------------------------------------------------------------------

procedure TEasyEvaluator.AddFunc(const AName: string; AFunc: TBinary64Func);
begin
  FOwnContext.Add(TExprBinary64FuncSym.Create(AName, AFunc));
end;

//--------------------------------------------------------------------------------------------------

procedure TEasyEvaluator.AddFunc(const AName: string; AFunc: TBinary80Func);
begin
  FOwnContext.Add(TExprBinary80FuncSym.Create(AName, AFunc));
end;

//--------------------------------------------------------------------------------------------------

procedure TEasyEvaluator.AddFunc(const AName: string; AFunc: TTernary32Func);
begin
  FOwnContext.Add(TExprTernary32FuncSym.Create(AName, AFunc));
end;

//--------------------------------------------------------------------------------------------------

procedure TEasyEvaluator.AddFunc(const AName: string; AFunc: TTernary64Func);
begin
  FOwnContext.Add(TExprTernary64FuncSym.Create(AName, AFunc));
end;

//--------------------------------------------------------------------------------------------------

procedure TEasyEvaluator.AddFunc(const AName: string; AFunc: TTernary80Func);
begin
  FOwnContext.Add(TExprTernary80FuncSym.Create(AName, AFunc));
end;

//--------------------------------------------------------------------------------------------------

procedure TEasyEvaluator.Clear;
begin
  FOwnContext.FHashMap.Iterate(nil, Iterate_FreeObjects);
  FOwnContext.FHashMap.Clear;
end;

//--------------------------------------------------------------------------------------------------

procedure TEasyEvaluator.Remove(const AName: string);
begin
  FOwnContext.Remove(AName);
end;

//--------------------------------------------------------------------------------------------------

type
  TInternalCompiledExpression = class
  private
    FVirtMach: TExprVirtMach;
    FRefCount: Integer;
  public
    constructor Create(AVirtMach: TExprVirtMach);
    destructor Destroy; override;
    property VirtMach: TExprVirtMach read FVirtMach;
    property RefCount: Integer read FRefCount write FRefCount;
  end;

//==================================================================================================
// TExpressionCompiler
//==================================================================================================

constructor TExpressionCompiler.Create;
begin
  FExprHash := TStringHashMap.Create(CaseInsensitiveTraits,
    C_ExprEval_HashSize);
  inherited Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TExpressionCompiler.Destroy;
begin
  FExprHash.Iterate(nil, Iterate_FreeObjects);
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TExpressionCompiler.Compile(const AExpr: string): TCompiledExpression;
var
  ice: TInternalCompiledExpression;
  vm: TExprVirtMach;
  parser: TExprCompileParser;
  lexer: TExprSimpleLexer;
  nodeFactory: TExprVirtMachNodeFactory;
begin
  if FExprHash.Find(AExpr, ice) then
  begin
    // expression already exists, add reference
    Result := ice.VirtMach.Execute;
    ice.RefCount := ice.RefCount + 1;
  end
  else
  begin
    // compile fresh expression
    parser := nil;
    nodeFactory := nil;
    lexer := TExprSimpleLexer.Create(AExpr);
    try
      nodeFactory := TExprVirtMachNodeFactory.Create;
      parser := TExprCompileParser.Create(lexer, nodeFactory);
      parser.Context := InternalContextSet;
      parser.Compile;

      ice := nil;
      vm := TExprVirtMach.Create;
      try
        nodeFactory.GenCode(vm);
        ice := TInternalCompiledExpression.Create(vm);
        ice.RefCount := 1;
        FExprHash.Add(AExpr, ice);
      except
        ice.Free;
        vm.Free;
        raise;
      end;
    finally
      nodeFactory.Free;
      parser.Free;
      lexer.Free;
    end;

    Result := ice.VirtMach.Execute;
  end;
end;

//--------------------------------------------------------------------------------------------------

type
  TIceFindResult = record
    Found: Boolean;
    Ce: TCompiledExpression;
    Ice: TInternalCompiledExpression;
    Expr: string;
  end;
  PIceFindResult = ^TIceFindResult;

//--------------------------------------------------------------------------------------------------

function Iterate_FindIce(AUserData: Pointer; const AStr: string; var APtr: Pointer): Boolean;
var
  pifr: PIceFindResult;
  ice: TInternalCompiledExpression;
  ce: TCompiledExpression;
begin
  pifr := AUserData;
  ice := APtr;
  ce := ice.VirtMach.Execute;

  if (TMethod(pifr^.Ce).Code = TMethod(ce).Code) and
    (TMethod(pifr^.Ce).Data = TMethod(ce).Data) then
  begin
    pifr^.Found := True;
    pifr^.Ice := ice;
    pifr^.Expr := AStr;
    Result := False;
  end else
    Result := True;
end;

//--------------------------------------------------------------------------------------------------

procedure TExpressionCompiler.Delete(ACompiledExpression: TCompiledExpression);
var
  ifr: TIceFindResult;
begin
  ifr.Found := False;
  ifr.Ce := ACompiledExpression;
  ifr.Ice := nil;
  ifr.Expr := '';
  FExprHash.Iterate(@ifr, Iterate_FindIce);
  if not ifr.Found then
    raise EJclExprEvalError.CreateResRec(@RsExprEvalExprPtrNotFound);
  Remove(ifr.Expr);
end;

//--------------------------------------------------------------------------------------------------

procedure TExpressionCompiler.Remove(const AExpr: string);
var
  ice: TInternalCompiledExpression;
begin
  if not FExprHash.Find(AExpr, ice) then
    raise EJclExprEvalError.CreateResRecFmt(@RsExprEvalExprNotFound,
      [AExpr]);

  ice.RefCount := ice.RefCount - 1;
  if ice.RefCount = 0 then
  begin
    ice.Free;
    FExprHash.Remove(AExpr);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TExpressionCompiler.Clear;
begin
  FExprHash.Iterate(nil, Iterate_FreeObjects);
end;

//==================================================================================================
// TInternalCompiledExpression
//==================================================================================================

constructor TInternalCompiledExpression.Create(AVirtMach: TExprVirtMach);
begin
  FVirtMach := AVirtMach;
end;

//--------------------------------------------------------------------------------------------------

destructor TInternalCompiledExpression.Destroy;
begin
  FVirtMach.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

end.

