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
{ The Original Code is JclUnicode.pas.                                         }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ 2000 of these individuals.                                                   }
{                                                                              }
{ Last modified: May 15, 2000                                                  }
{                                                                              }
{******************************************************************************}

unit JclUnicode;

// Copyright (c) 1999, 2000 Mike Lischke (public@lischke-online.de)
// Portions Copyright (c) 1999, 2000 Azret Botash (az)
//
// 12-JUN-2000 ml:
//  adjustments to make the unit JCL compliant
// 02-APR-2000 ml:
//   additional widestring conversion routines
// 01-APR-2000 ml:
//   preparation for public release
// FEB-MAR 2000 version 2.0 beta
//   - Unicode regular expressions (URE) search class (TURESearch)
//   - generic search engine base class for both the Boyer-Moore and the RE search class
//   - whole word only search in UTBM, bug fixes in UTBM
//   - string decompositon (including hangul)
// OCT/99 - JAN/2000 ml: version 1.0
//   - basic Unicode implementation, more than 100 WideString/UCS2 and UCS4 core functions
//   - TWideStrings and TWideStringList classes
//   - Unicode Tuned Boyer-Moore search class (TUTBMSearch)
//   - low and high level Unicode/Wide* functions
//   - low level Unicode UCS4 data import and functions
//   - helper functions
//
//  Version 2.2 beta
//------------------------------------------------------------------------------
// This unit contains routines and classes to manage and work with Unicode/WideString strings.
// You need Delphi 4 or higher to compile this code.
//
// Unicode encodings and wide strings:
// Currently there are several encoding schemes defined which describe (among others)
// the code size and (resulting from this) the usable value pool. Delphi supports the
// wide character data type for Unicode which corresponds to UCS2 (UTF-16 coding scheme).
// This scheme uses 2 bytes to store character values and can thus handle up to
// 65536 characters. Another scheme is UCS4 (UTF-32 coding scheme) which uses 4 bytes
// per character. The first 65536 code points correspond directly to those of UCS2.
// Other code points are mainly used for character surrogates. To provide support
// for UCS2 (WideChar in Delphi) as well as UCS4 the library is splitted into two
// parts. The low level part accepts and returns UCS4 characters while the high level
// part deals directly with WideChar/WideString data types. Additionally, UCS2 is
// defined as being WideChar to retain maximum compatibility.
//
// Publicly available low level functions are all preceded by "Unicode..." (e.g.
// in UnicodeToUpper) while the high level functions use the Str... or Wide...
// naming scheme (e.g. WideUpCase and WideUpperCase).
//
//------------------------------------------------------------------------------
// Open issues:
//   - Keep in mind that this unit is still in beta state. In particular the URE
//     class does not yet work for all cases.
//   - Yet to do things in the URE class are:
//     - check all character classes if they match correctly
//     - optimize rebuild of DFA (build only when pattern changes)
//     - set flag parameter of ExecuteURE
//     - add \d     any decimal digit
//           \D     any character that is not a decimal digit
//           \s     any whitespace character
//           \S     any character that is not a whitespace character
//           \w     any "word" character
//           \W     any "non-word" character
//   - For a perfect text search both the text to be searched through as well as
//     the pattern must be normalized to allow to match, say, accented and unaccented
//     characters or the ligature fi with the letter combination fi etc. Normalization
//     is usually done by decomposing the string and optionally compose it again,
//     but I had not yet the opportunity to go through the composition stuff.
//   - The wide string classes still compare text with functions provided by the
//     particular system. This works usually fine under WinNT/W2K (although also
//     there are limitations like maximum text lengths). Under Win9x conversions
//     from and to MBCS are necessary which are bound to a particular locale and
//     so very limited in general use. These comparisons should be changed so that
//     the code in this unit is used. This requires, though, a working composition
//     implementation.

interface

uses
  {$IFDEF LINUX}
  Libc, Types,
  {$ENDIF}
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  Classes;

const
  // definitions of often used characters:
  // Note: Use them only for tests of a certain character not to determine character
  //       classes like white spaces as in Unicode are often many code points defined
  //       being in a certain class. Hence your best option is to use the various
  //       UnicodeIs* functions.

  WideNull = WideChar(#0);
  WideTabulator = WideChar(#9);
  WideSpace = WideChar(#32);

  // logical line breaks
  WideLF = WideChar($A);
  WideLineFeed = WideChar($A);
  WideVerticalTab = WideChar($B);
  WideFormFeed = WideChar($C);
  WideCR = WideChar($D);
  WideCarriageReturn = WideChar($D);
  WideCRLF: WideString = #$D#$A;
  WideLineSeparator = WideChar($2028);
  WideParagraphSeparator = WideChar($2029);

  // byte order marks for strings
  // Unicode text files should contain $FFFE as first character to identify such
  // a file clearly. Depending on the system where the file was created on this
  // appears either in big endian or little endian style.

  BOM_LSB_FIRST = WideChar($FEFF); // this is how the BOM appears on x86 systems
                                   // when written by a x86 system
  BOM_MSB_FIRST = WideChar($FFFE);

type

  // Unicode transformation formats (UTF) data types

  PUTF7 = ^UTF7;
  UTF7 = Char;
  PUTF8 = ^PUTF8;
  UTF8 = Char;
  PUTF16 = ^UTF16;
  UTF16 = WideChar;
  PUTF32 = ^UTF32;
  UTF32 = Cardinal;

  // UTF conversion schemes (UCS) data types

  PUCS4 = ^UCS4;
  UCS4 = Cardinal;
  PUCS2 = PWideChar;
  UCS2 = WideChar;

const
  ReplacementCharacter: UCS4 = $0000FFFD;
  MaximumUCS2: UCS4 = $0000FFFF;
  MaximumUTF16: UCS4 = $0010FFFF;
  MaximumUCS4: UCS4 = $7FFFFFFF;

  SurrogateHighStart: UCS4 = $D800;
  SurrogateHighEnd: UCS4 = $DBFF;
  SurrogateLowStart: UCS4 = $DC00;
  SurrogateLowEnd: UCS4 = $DFFF;

type
  TWideStrings = class;

  TSearchFlags = set of (
    sfCaseSensitive,    // match letter case
    sfIgnoreNonSpacing, // ignore non-spacing characters in search
    sfSpaceCompress,    // handle several consecutive white spaces as one white space
                        // (this applies to the pattern as well as the search text)
    sfWholeWordOnly);   // match only text at end/start and/or surrounded by white spaces

  // a generic search class defininition used for tuned Boyer-Moore and Unicode
  // regular expression searches

  TSearchEngine = class (TObject)
  private
    FResults: TList;      // 2 entries for each result (start and stop position)
    FOwner: TWideStrings; // at the moment unused, perhaps later to access strings faster
  protected
    function GetCount: Integer; virtual;
  public
    constructor Create(AOwner: TWideStrings); virtual;
    destructor Destroy; override;

    procedure AddResult(Start, Stop: Cardinal); virtual;
    procedure Clear; virtual;
    procedure ClearResults; virtual;
    procedure DeleteResult(Index: Cardinal); virtual;
    procedure FindPrepare(const Pattern: WideString; Options: TSearchFlags); overload; virtual; abstract;
    procedure FindPrepare(const Pattern: PWideChar; PatternLength: Cardinal; Options: TSearchFlags); overload; virtual; abstract;
    function FindFirst(const Text: WideString; var Start, Stop: Cardinal): Boolean; overload; virtual; abstract;
    function FindFirst(const Text: PWideChar; TextLen: Cardinal; var Start, Stop: Cardinal): Boolean; overload; virtual; abstract;
    function FindAll(const Text: WideString): Boolean; overload; virtual; abstract;
    function FindAll(const Text: PWideChar; TextLen: Cardinal): Boolean; overload; virtual; abstract;
    procedure GetResult(Index: Cardinal; var Start, Stop: Integer); virtual;

    property Count: Integer read GetCount;
  end;

  // The Unicode Tuned Boyer-Moore (UTBM) search implementation is an extended
  // translation created from a free package written by Mark Leisher (mleisher@crl.nmsu.edu).
  //
  // The code handles high and low surrogates as well as case (in)dependency,
  // can ignore non-spacing characters and allows optionally to return whole
  // words only.

  // single pattern character

  PUTBMChar = ^TUTBMChar;
  TUTBMChar = record
    LoCase,
    UpCase,
    TitleCase: UCS4;
  end;

  PUTBMSkip = ^TUTBMSkip;
  TUTBMSkip = record
    BMChar: PUTBMChar;
    SkipValues: Integer;
  end;

  TUTBMSearch = class (TSearchEngine)
  private
    FFlags: TSearchFlags;
    FPattern: PUTBMChar;
    FPatternUsed,
    FPatternSize,
    FPatternLength: Cardinal;
    FSkipValues: PUTBMSkip;
    FSkipsUsed: Integer;
    FMD4: Cardinal;
  protected
    procedure ClearPattern;
    procedure Compile(Pattern: PUCS2; PatternLength: Integer; Flags: TSearchFlags);
    function Find(Text: PUCS2; TextLen: Cardinal; var MatchStart, MatchEnd: Cardinal): Boolean;
    function GetSkipValue(TextStart, TextEnd: PUCS2): Cardinal;
    function Match(Text, Start, Stop: PUCS2; var MatchStart, MatchEnd: Cardinal): Boolean;
  public
    constructor Create(AOwner: TWideStrings); override;
    destructor Destroy; override;

    procedure Clear; override;
    procedure FindPrepare(const Pattern: WideString; Options: TSearchFlags); overload; override;
    procedure FindPrepare(const Pattern: PWideChar; PatternLength: Cardinal; Options: TSearchFlags); overload; override;
    function FindFirst(const Text: WideString; var Start, Stop: Cardinal): Boolean; overload; override;
    function FindFirst(const Text: PWideChar; TextLen: Cardinal; var Start, Stop: Cardinal): Boolean; overload; override;
    function FindAll(const Text: WideString): Boolean; overload; override;
    function FindAll(const Text: PWideChar; TextLen: Cardinal): Boolean; overload; override;
  end;

  // Regular expression search engine for text in UCS2 form taking surrogates
  // into account. This implementation is an improved translation from the URE
  // package written by Mark Leisher (mleisher@crl.nmsu.edu) who used a variation
  // of the RE->DFA algorithm done by Mark Hopkins (markh@csd4.csd.uwm.edu).
  // Assumptions:
  //   o  Regular expression and text already normalized.
  //   o  Conversion to lower case assumes a 1-1 mapping.
  //
  // Definitions:
  //   Separator - any one of U+2028, U+2029, NL, CR.
  //
  // Operators:
  //   .      - match any character
  //   *      - match zero or more of the last subexpression
  //   +      - match one or more of the last subexpression
  //   ?      - match zero or one of the last subexpression
  //   ()     - subexpression grouping
  //   {m, n} - match at least m occurences and up to n occurences
  //            Note: both values can be 0 or ommitted which denotes then a unlimiting bound
  //            {,} and {0,} and {0, 0} correspond to *
  //            {, 1} and {0, 1} correspond to ?
  //            {1,} and {1, 0} correspond to +
  //   {m}    - match exactly m occurences
  //
  //   Notes:
  //     o  The "." operator normally does not match separators, but a flag is
  //        available that will allow this operator to match a separator.
  //
  // Literals and Constants:
  //   c       - literal UCS2 character
  //   \x....  - hexadecimal number of up to 4 digits
  //   \X....  - hexadecimal number of up to 4 digits
  //   \u....  - hexadecimal number of up to 4 digits
  //   \U....  - hexadecimal number of up to 4 digits
  //
  // Character classes:
  //   [...]           - Character class
  //   [^...]          - Negated character class
  //   \pN1,N2,...,Nn  - Character properties class
  //   \PN1,N2,...,Nn  - Negated character properties class
  //
  //   POSIX character classes recognized:
  //     :alnum:
  //     :alpha:
  //     :cntrl:
  //     :digit:
  //     :graph:
  //     :lower:
  //     :print:
  //     :punct:
  //     :space:
  //     :upper:
  //     :xdigit:
  //
  //   Notes:
  //     o  Character property classes are \p or \P followed by a comma separated
  //        list of integers between 1 and 32.  These integers are references to
  //        the following character properties:
  //
  //         N  Character Property
  //         --------------------------
  //         1  _URE_NONSPACING
  //         2  _URE_COMBINING
  //         3  _URE_NUMDIGIT
  //         4  _URE_NUMOTHER
  //         5  _URE_SPACESEP
  //         6  _URE_LINESEP
  //         7  _URE_PARASEP
  //         8  _URE_CNTRL
  //         9  _URE_PRIVATE
  //         10 _URE_UPPER   (note: upper, lower and titel case classes need to have case
  //         11 _URE_LOWER          sensitive search be enabled to match correctly!)
  //         12 _URE_TITLE
  //         13 _URE_MODIFIER
  //         14 _URE_OTHERLETTER
  //         15 _URE_DASHPUNCT
  //         16 _URE_OPENPUNCT
  //         17 _URE_CLOSEPUNCT
  //         18 _URE_OTHERPUNCT
  //         19 _URE_MATHSYM
  //         20 _URE_CURRENCYSYM
  //         21 _URE_OTHERSYM
  //         22 _URE_LTR
  //         23 _URE_RTL
  //         24 _URE_EURONUM
  //         25 _URE_EURONUMSEP
  //         26 _URE_EURONUMTERM
  //         27 _URE_ARABNUM
  //         28 _URE_COMMONSEP
  //         29 _URE_BLOCKSEP
  //         30 _URE_SEGMENTSEP
  //         31 _URE_WHITESPACE
  //         32 _URE_OTHERNEUT
  //
  //     o  Character classes can contain literals, constants, and character
  //        property classes. Example:
  //
  //        [abc\U10A\p1,3,4]

  // structure used to handle a compacted range of characters

  PUcRange = ^TUcRange;
  TUcRange = record
    MinCode,
    MaxCode: UCS4;
  end;

  TUcCClass = record
    Ranges: array of TUcRange;
    RangesUsed: Integer;
  end;

  // either a single character or a list of character classes

  TUcSymbol = record
    Chr: UCS4;
    CCL: TUcCClass;
  end;

  // this is a general element structure used for expressions and stack elements

  TUcElement = record
    OnStack: Boolean;
    AType,
    LHS,
    RHS: Cardinal;
  end;

  // this is a structure used to track a list or a stack of states

  PUcStateList = ^TUcStateList;
  TUcStateList = record
    List: array of Cardinal;
    ListUsed: Integer;
  end;

  // structure to track the list of unique states for a symbol during reduction

  PUcSymbolTableEntry = ^TUcSymbolTableEntry;
  TUcSymbolTableEntry = record
    ID,
    AType: Cardinal;
    Mods,
    Props: Cardinal;
    Symbol: TUcSymbol;
    States: TUcStateList;
  end;

  // structure to hold a single State

  PUcState = ^TUcState;
  TUcState = record
    ID: Cardinal;
    Accepting: Boolean;
    StateList: TUcStateList;
    Transitions: array of TUcElement;
    TransitionsUsed: Integer;
  end;

  // structure used for keeping lists of states

  TUcStateTable = record
    States: array of TUcState;
    StatesUsed: Integer;
  end;

  // structure to track pairs of DFA states when equivalent states are merged

  TUcEquivalent = record
    Left, Right: Cardinal;
  end;

  TUcExpressionList = record
    Expressions: array of TUcElement;
    ExpressionsUsed: Integer;
  end;

  TUcSymbolTable = record
    Symbols: array of TUcSymbolTableEntry;
    SymbolsUsed: Integer;
  end;

  TUcEquivalentList = record
    Equivalents: array of TUcEquivalent;
    EquivalentsUsed: Integer;
  end;

  // structure used for constructing the NFA and reducing to a minimal DFA

  PUREBuffer = ^TUREBuffer;
  TUREBuffer = record
    Reducing: Boolean;
    Error: Integer;
    Flags: Cardinal;
    Stack: TUcStateList;
    SymbolTable: TUcSymbolTable;       // table of unique symbols encountered
    ExpressionList: TUcExpressionList; // tracks the unique expressions generated
                                       // for the NFA and when the NFA is reduced
    States: TUcStateTable;             // the reduced table of unique groups of NFA states
    EquivalentList: TUcEquivalentList; // tracks states when equivalent states are merged
  end;

  TUcTransition = record
    Symbol,
    NextState: Cardinal;
  end;

  PDFAState = ^TDFAState;
  TDFAState = record
    Accepting: Boolean;
    NumberTransitions: Integer;
    StartTransition: Integer;
  end;

  TDFAStates = record
    States: array of TDFAState;
    StatesUsed: Integer;
  end;

  TUcTransitions = record
    Transitions: array of TUcTransition;
    TransitionsUsed: Integer;
  end;

  TDFA = record
    Flags: Cardinal;
    SymbolTable: TUcSymbolTable;
    StateList: TDFAStates;
    TransitionList: TUcTransitions;
  end;

  TURESearch = class (TSearchEngine)
  private
    FUREBuffer: TUREBuffer;
    FDFA: TDFA;
  protected
    procedure AddEquivalentPair(L, R: Cardinal);
    procedure AddRange(var CCL: TUcCClass; Range: TUcRange);
    function AddState(NewStates: array of Cardinal): Cardinal;
    procedure AddSymbolState(Symbol, State: Cardinal);
    function BuildCharacterClass(CP: PUCS2; Limit: Cardinal; Symbol: PUcSymbolTableEntry): Cardinal;
    procedure CCLSetup(Symbol: PUcSymbolTableEntry; Mask: Cardinal);
    procedure ClearUREBuffer;
    function CompileSymbol(S: PUCS2; Limit: Cardinal; Symbol: PUcSymbolTableEntry): Cardinal;
    procedure CompileURE(RE: PWideChar; RELength: Cardinal; Casefold: Boolean);
    procedure CollectPendingOperations(var State: Cardinal);
    function ConvertRegExpToNFA(RE: PWideChar; RELength: Cardinal): Cardinal;
    function ExecuteURE(Flags: Cardinal; Text: PUCS2; TextLen: Cardinal; var MatchStart, MatchEnd: Cardinal): Boolean;
    procedure ClearDFA;
    procedure HexDigitSetup(Symbol: PUcSymbolTableEntry; Mask: Cardinal);
    function MakeExpression(AType, LHS, RHS: Cardinal): Cardinal;
    function MakeHexNumber(NP: PUCS2; Limit: Cardinal; var Number: Cardinal): Cardinal;
    function MakeSymbol(S: PUCS2; Limit: Cardinal; var Consumed: Cardinal): Cardinal;
    function MatchesProperties(Props, C: Cardinal): Boolean;
    procedure MergeEquivalents;
    function ParsePropertyList(Properties: PUCS2; Limit: Cardinal; var Mask: Cardinal): Cardinal;
    function Peek: Cardinal;
    function Pop: Cardinal;
    function PosixCCL(CP: PUCS2; Limit: Cardinal; Symbol: PUcSymbolTableEntry): Cardinal;
    function ProbeLowSurrogate(LeftState: PUCS2; Limit: Cardinal; var Code: UCS4): Cardinal;
    procedure Push(V: Cardinal);
    procedure Reduce(Start: Cardinal);
    procedure SpaceSetup(Symbol: PUcSymbolTableEntry; Mask: Cardinal);
    function SymbolsAreDifferent(A, B: PUcSymbolTableEntry): Boolean;
  public
    constructor Create(AOwner: TWideStrings); override;
    destructor Destroy; override;

    procedure Clear; override;
    procedure FindPrepare(const Pattern: WideString; Options: TSearchFlags); overload; override;
    procedure FindPrepare(const Pattern: PWideChar; PatternLength: Cardinal; Options: TSearchFlags); overload; override;
    function FindFirst(const Text: WideString; var Start, Stop: Cardinal): Boolean; overload; override;
    function FindFirst(const Text: PWideChar; TextLen: Cardinal; var Start, Stop: Cardinal): Boolean; overload; override;
    function FindAll(const Text: WideString): Boolean; overload; override;
    function FindAll(const Text: PWideChar; TextLen: Cardinal): Boolean; overload; override;
  end;

  // Event used to give the application a chance to switch the way of how to save
  // the text in TWideStrings if the text contains characters not only from the
  // ANSI block but the save type is ANSI. On triggering the event the application
  // can change the property SaveUnicode as needed. This property is again checked
  // after the callback returns.

  TConfirmConversionEvent = procedure (Sender: TWideStrings; var Allowed: Boolean) of object;

  TWideStrings = class (TPersistent)
  private
    FUpdateCount: Integer;
    FLanguage: LCID;        // language can usually left alone, the system's default is used
    FSaved,                 // set in SaveToStream, True in case saving was successfull otherwise False
    FSaveUnicode: Boolean;  // flag set on loading to keep track in which format to save
                            // (can be set explicitely, but expect losses if there's true Unicode content
                            // and this flag is set to False)
    FOnConfirmConversion: TConfirmConversionEvent;
    function GetCommaText: WideString;
    function GetName(Index: Integer): WideString;
    function GetValue(const Name: WideString): WideString;
    procedure ReadData(Reader: TReader);
    procedure SetCommaText(const Value: WideString);
    procedure SetValue(const Name, Value: WideString);
    procedure WriteData(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoConfirmConversion(var Allowed: Boolean); virtual;
    procedure Error(const Msg: string; Data: Integer);
    function Get(Index: Integer): WideString; virtual; abstract;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetText: WideString; virtual;
    procedure Put(Index: Integer; const S: WideString); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
    procedure SetLanguage(Value: LCID); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const S: WideString): Integer; virtual;
    function AddObject(const S: WideString; AObject: TObject): Integer; virtual;
    procedure Append(const S: WideString);
    procedure AddStrings(Strings: TStrings); overload; virtual;
    procedure AddStrings(Strings: TWideStrings); overload; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(Strings: TWideStrings): Boolean;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetSeparatedText(Separators: WideString): WideString; virtual;
    function IndexOf(const S: WideString): Integer; virtual;
    function IndexOfName(const Name: WideString): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    procedure Insert(Index: Integer; const S: WideString); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: WideString; AObject: TObject);
    procedure LoadFromFile(const FileName: string); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: string); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SetText(const Value: WideString); virtual;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: WideString read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Language: LCID read FLanguage write SetLanguage;
    property Names[Index: Integer]: WideString read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Values[const Name: WideString]: WideString read GetValue write SetValue;
    property Saved: Boolean read FSaved;
    property SaveUnicode: Boolean read FSaveUnicode write FSaveUnicode;
    property Strings[Index: Integer]: WideString read Get write Put; default;
    property Text: WideString read GetText write SetText;

    property OnConfirmConversion: TConfirmConversionEvent read FOnConfirmConversion write FOnConfirmConversion;
  end;

  // TWideStringList class

  TWideStringItem = record
    FString: WideString;
    FObject: TObject;
  end;

  TWideStringItemList = array of TWideStringItem;

  TWideStringList = class (TWideStrings)
  private
    FList: TWideStringItemList;
    FCount: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer);
    procedure InsertItem(Index: Integer; const S: WideString);
    procedure SetSorted(Value: Boolean);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): WideString; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: WideString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    procedure SetLanguage(Value: LCID); override;
  public
    destructor Destroy; override;

    function Add(const S: WideString): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: WideString; var Index: Integer): Boolean; virtual;
    function IndexOf(const S: WideString): Integer; override;
    procedure Insert(Index: Integer; const S: WideString); override;
    procedure Sort; virtual;

    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

  // result type for number retrieval functions

  TUNumber = record
    Numerator,
    Denominator: Integer;
  end;

  TFontCharSet = 0..255;

// Functions involving Null-terminated strings
// NOTE: PWideChars as well as WideStrings are NOT managed by reference counting under Win32.
//       In Kylix this is different. WideStrings are reference counted there, just like ANSI strings.

function StrLenW(Str: PWideChar): Cardinal;
function StrEndW(Str: PWideChar): PWideChar;
function StrMoveW(Dest, Source: PWideChar; Count: Cardinal): PWideChar;
function StrCopyW(Dest, Source: PWideChar): PWideChar;
function StrECopyW(Dest, Source: PWideChar): PWideChar;
function StrLCopyW(Dest, Source: PWideChar; MaxLen: Cardinal): PWideChar;
function StrPCopyW(Dest: PWideChar; const Source: string): PWideChar;
function StrPLCopyW(Dest: PWideChar; const Source: string; MaxLen: Cardinal): PWideChar;
function StrCatW(Dest, Source: PWideChar): PWideChar;
function StrLCatW(Dest, Source: PWideChar; MaxLen: Cardinal): PWideChar;
function StrCompW(Str1, Str2: PWideChar): Integer;
function StrICompW(Str1, Str2: PWideChar): Integer;
function StrLCompW(Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
function StrLICompW(Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
function StrNScanW(S1, S2: PWideChar): Integer;
function StrRNScanW(S1, S2: PWideChar): Integer;
function StrScanW(Str: PWideChar; Chr: WideChar): PWideChar; overload;
function StrScanW(Str: PWideChar; Chr: WideChar; StrLen: Cardinal): PWideChar; overload;
function StrRScanW(Str: PWideChar; Chr: WideChar): PWideChar;
function StrPosW(Str, SubStr: PWideChar): PWideChar;
function StrUpperW(Str: PWideChar): PWideChar;
function StrLowerW(Str: PWideChar): PWideChar;
function StrTitleW(Str: PWideChar): PWideChar;
function StrAllocW(Size: Cardinal): PWideChar;
function StrBufSizeW(Str: PWideChar): Cardinal;
function StrNewW(Str: PWideChar): PWideChar;
procedure StrDisposeW(Str: PWideChar);
procedure StrSwapByteOrder(Str: PWideChar);

// Functions involving Delphi wide strings

function WideAdjustLineBreaks(const S: WideString): WideString;
function WideCharPos(const S: WideString; const Ch: WideChar; const Index: Integer): Integer;  //az
function WideCompose(const S: WideString): WideString;
function WideComposeHangul(Source: WideString): WideString;
function WideDecompose(const S: WideString): WideString;
function WideLoCase(C: WideChar): WideChar;
function WideLowerCase(const S: WideString): WideString;
function WideExtractQuotedStr(var Src: PWideChar; Quote: WideChar): WideString;
function WideQuotedStr(const S: WideString; Quote: WideChar): WideString;
function WideStringOfChar(C: WideChar; Count: Cardinal): WideString;
function WideTitleCaseChar(C: WideChar): WideChar;
function WideTitleCaseString(const S: WideString): WideString;
function WideTrim(const S: WideString): WideString;
function WideTrimLeft(const S: WideString): WideString;
function WideTrimRight(const S: WideString): WideString;
function WideUpCase(C: WideChar): WideChar;
function WideUpperCase(const S: WideString): WideString;

// Low level character routines

function UnicodeGetDigit(Code: UCS4): Integer;
function UnicodeGetNumber(Code: UCS4): TUNumber;
function UnicodeToUpper(Code: UCS4): UCS4;
function UnicodeToLower(Code: UCS4): UCS4;
function UnicodeToTitle(Code: UCS4): UCS4;

// Character test routines

function UnicodeIsAlpha(C: UCS4): Boolean;
function UnicodeIsDigit(C: UCS4): Boolean;
function UnicodeIsAlphaNum(C: UCS4): Boolean;
function UnicodeIsControl(C: UCS4): Boolean;
function UnicodeIsSpace(C: UCS4): Boolean;
function UnicodeIsWhiteSpace(C: UCS4): Boolean;
function UnicodeIsBlank(C: UCS4): Boolean;
function UnicodeIsPunctuation(C: UCS4): Boolean;
function UnicodeIsGraph(C: UCS4): Boolean;
function UnicodeIsPrintable(C: UCS4): Boolean;
function UnicodeIsUpper(C: UCS4): Boolean;
function UnicodeIsLower(C: UCS4): Boolean;
function UnicodeIsTitle(C: UCS4): Boolean;
function UnicodeIsHexDigit(C: UCS4): Boolean;

function UnicodeIsIsoControl(C: UCS4): Boolean;
function UnicodeIsFormatControl(C: UCS4): Boolean;

function UnicodeIsSymbol(C: UCS4): Boolean;
function UnicodeIsNumber(C: UCS4): Boolean;
function UnicodeIsNonSpacing(C: UCS4): Boolean;
function UnicodeIsOpenPunctuation(C: UCS4): Boolean;
function UnicodeIsClosePunctuation(C: UCS4): Boolean;
function UnicodeIsInitialPunctuation(C: UCS4): Boolean;
function UnicodeIsFinalPunctuation(C: UCS4): Boolean;

function UnicodeIsComposite(C: UCS4): Boolean;
function UnicodeIsQuotationMark(C: UCS4): Boolean;
function UnicodeIsSymmetric(C: UCS4): Boolean;
function UnicodeIsMirroring(C: UCS4): Boolean;
function UnicodeIsNonBreaking(C: UCS4): Boolean;

// Directionality functions

function UnicodeIsRightToLeft(C: UCS4): Boolean;
function UnicodeIsLeftToRight(C: UCS4): Boolean;
function UnicodeIsStrong(C: UCS4): Boolean;
function UnicodeIsWeak(C: UCS4): Boolean;
function UnicodeIsNeutral(C: UCS4): Boolean;
function UnicodeIsSeparator(C: UCS4): Boolean;

// Other character test functions

function UnicodeIsMark(C: UCS4): Boolean;
function UnicodeIsModifier(C: UCS4): Boolean;
function UnicodeIsLetterNumber(C: UCS4): Boolean;
function UnicodeIsConnectionPunctuation(C: UCS4): Boolean;
function UnicodeIsDash(C: UCS4): Boolean;
function UnicodeIsMath(C: UCS4): Boolean;
function UnicodeIsCurrency(C: UCS4): Boolean;
function UnicodeIsModifierSymbol(C: UCS4): Boolean;
function UnicodeIsNonSpacingMark(C: UCS4): Boolean;
function UnicodeIsSpacingMark(C: UCS4): Boolean;
function UnicodeIsEnclosing(C: UCS4): Boolean;
function UnicodeIsPrivate(C: UCS4): Boolean;
function UnicodeIsSurrogate(C: UCS4): Boolean;
function UnicodeIsLineSeparator(C: UCS4): Boolean;
function UnicodeIsParagraphSeparator(C: UCS4): Boolean;

function UnicodeIsIdentifierStart(C: UCS4): Boolean;
function UnicodeIsIdentifierPart(C: UCS4): Boolean;

function UnicodeIsDefined(C: UCS4): Boolean;
function UnicodeIsUndefined(C: UCS4): Boolean;

function UnicodeIsHan(C: UCS4): Boolean;
function UnicodeIsHangul(C: UCS4): Boolean;

// Utility functions

function CharSetFromLocale(Language: LCID): TFontCharSet;
function CodePageFromLocale(Language: LCID): Integer;
function CodeBlockFromChar(const C: WideChar): Cardinal;
function KeyboardCodePage: Word;
function KeyUnicode(C: Char): WideChar;
function StringToWideStringEx(const S: string; CodePage: Word): WideString;
function TranslateString(const S: string; CP1, CP2: Word): string;
function WideStringToStringEx(const WS: WideString; CodePage: Word): string;

// WideString conversion routines

function WideStringToUTF8(S: WideString): AnsiString;
function UTF8ToWideString(S: AnsiString): WideString;

//------------------------------------------------------------------------------

implementation

// ~67K Unicode data for case mapping, decomposition, numbers etc. This data is
// loaded on demand which means only those parts will be put in memory which are
// needed by one of the lookup functions.

{$R JclUnicode.res}

uses
  Consts, SyncObjs, SysUtils;

resourcestring
  RsUREBaseString = 'Error in regular expression: %s' + #13;
  RsUREUnexpectedEOS = 'Unexpected end of pattern.';
  RsURECharacterClassOpen = 'Character class not closed, '']'' is missing.';
  RsUREUnbalancedGroup = 'Unbalanced group expression, '')'' is missing.';
  RsUREInvalidCharProperty = 'A character property is invalid';
  RsUREInvalidRepeatRange = 'Invalid repeation range.';
  RsURERepeatRangeOpen = 'Repeation range not closed, ''}'' is missing.';
  RsUREExpressionEmpty = 'Expression is empty.';

type
  TCompareFunc = function (W1, W2: WideString; Locale: LCID): Integer;

var
  WideCompareText: TCompareFunc;

const

  // Values that can appear in the Mask1 parameter of the IsProperty function.

  UC_MN = $00000001; // Mark, Non-Spacing
  UC_MC = $00000002; // Mark, Spacing Combining
  UC_ME = $00000004; // Mark, Enclosing
  UC_ND = $00000008; // Number, Decimal Digit
  UC_NL = $00000010; // Number, Letter
  UC_NO = $00000020; // Number, Other
  UC_ZS = $00000040; // Separator, Space
  UC_ZL = $00000080; // Separator, Line
  UC_ZP = $00000100; // Separator, Paragraph
  UC_CC = $00000200; // Other, Control
  UC_CF = $00000400; // Other, Format
  UC_OS = $00000800; // Other, Surrogate
  UC_CO = $00001000; // Other, private use
  UC_CN = $00002000; // Other, not assigned
  UC_LU = $00004000; // Letter, Uppercase
  UC_LL = $00008000; // Letter, Lowercase
  UC_LT = $00010000; // Letter, Titlecase
  UC_LM = $00020000; // Letter, Modifier
  UC_LO = $00040000; // Letter, Other
  UC_PC = $00080000; // Punctuation, Connector
  UC_PD = $00100000; // Punctuation, Dash
  UC_PS = $00200000; // Punctuation, Open
  UC_PE = $00400000; // Punctuation, Close
  UC_PO = $00800000; // Punctuation, Other
  UC_SM = $01000000; // Symbol, Math
  UC_SC = $02000000; // Symbol, Currency
  UC_SK = $04000000; // Symbol, Modifier
  UC_SO = $08000000; // Symbol, Other
  UC_L  = $10000000; // Left-To-Right
  UC_R  = $20000000; // Right-To-Left
  UC_EN = $40000000; // European Number
  UC_ES = $80000000; // European Number Separator

  // Values that can appear in the Mask2 parameter of the IsProperty function

  UC_ET = $00000001; // European Number Terminator
  UC_AN = $00000002; // Arabic Number
  UC_CS = $00000004; // Common Number Separator
  UC_B  = $00000008; // Block Separator
  UC_S  = $00000010; // Segment (unit) Separator (this includes tab and vertical tab)
  UC_WS = $00000020; // Whitespace
  UC_ON = $00000040; // Other Neutrals

  // Implementation specific character properties.

  UC_CM = $00000080; // Composite
  UC_NB = $00000100; // Non-Breaking
  UC_SY = $00000200; // Symmetric
  UC_HD = $00000400; // Hex Digit
  UC_QM = $00000800; // Quote Mark
  UC_MR = $00001000; // Mirroring
  UC_SS = $00002000; // Space, other

  UC_CP = $00004000; // Defined

  // Added for UnicodeData-2.1.3.

  UC_PI = $00008000; // Punctuation, Initial
  UC_PF = $00010000; // Punctuation, Final

//==============================================================================
// Loader routines and structure definitions for resource data
//==============================================================================

type
  TUHeader = record
    BOM: WideChar;
    Count: Word;
    case Boolean of
      True: (
        Bytes: Cardinal);
      False: (
        Len: array [0..1] of Word);
  end;

  TWordArray = array of Word;
  TCardinalArray = array of Cardinal;

var
  // As the global data can be accessed by several threads it should be guarded
  // while the data is loaded.
  LoadInProgress: TCriticalSection;

//==============================================================================
// Internal support routines
//==============================================================================

function SwapCardinal(C: Cardinal): Cardinal;
// swaps all bytes in C from MSB to LSB order
// EAX contains both parameter as well as result
asm
        BSWAP   EAX
end;

//==============================================================================
// Support for character properties
//==============================================================================

var
  PropertyOffsets: TWordArray;
  PropertyRanges: TCardinalArray;

procedure LoadUnicodeTypeData;
// loads the character property data (as saved by the Unicode database extractor
// into the ctype.dat file)
var
  I, Size: Integer;
  Header: TUHeader;
  Stream: TResourceStream;
begin
  // make sure no other code is currently modifying the global data area
  if LoadInProgress = nil then
    LoadInProgress := TCriticalSection.Create;
  LoadInProgress.Enter;

  // Data already loaded?
  if PropertyOffsets = nil then
  begin
    Stream := TResourceStream.Create(HInstance, 'TYPE', 'UNICODE');
    Stream.Read(Header, SizeOf(Header));

    if Header.BOM = BOM_MSB_FIRST then
    begin
      Header.Count := Swap(Header.Count);
      Header.Bytes := SwapCardinal(Header.Bytes);
    end;

    // Calculate the offset into the storage for the ranges.  The offsets
    // array is on a 4-byte boundary and one larger than the value provided in
    // the header count field. This means the offset to the ranges must be
    // calculated after aligning the count to a 4-byte boundary.
    Size := (Header.Count + 1) * SizeOf(Word);
    if (Size and 3) <> 0 then
      Inc(Size, 4 - (Size and 3));

    // fill offsets array
    SetLength(PropertyOffsets, Size div SizeOf(Word));
    Stream.Read(PropertyOffsets[0], Size);

    // Do an endian swap if necessary. Don't forget there is an extra node on
    // the end with the final index.
    if Header.BOM = BOM_MSB_FIRST then
      for I := 0 to Header.Count do
        PropertyOffsets[I] := Swap(PropertyOffsets[I]);

    // Load the ranges.
    // The number of elements is in the last array position of the offsets.
    SetLength(PropertyRanges, PropertyOffsets[Header.Count]);
    Stream.Read(PropertyRanges[0], PropertyOffsets[Header.Count] * SizeOf(Cardinal));

    // Do an endian swap if necessary.
    if Header.BOM = BOM_MSB_FIRST then
    begin
      for I := 0 to PropertyOffsets[Header.Count] - 1 do
        PropertyRanges[I] := SwapCardinal(PropertyRanges[I]);
    end;
    Stream.Free;
  end;
  LoadInProgress.Leave;
end;

//------------------------------------------------------------------------------

function PropertyLookup(Code, N: Cardinal): Boolean;
var
  L, R, M: Integer;
begin
  // load property data if not already done
  if PropertyOffsets = nil then
    LoadUnicodeTypeData;

  Result := False;
  // There is an extra node on the end of the offsets to allow this routine
  // to work right. If the index is 0xffff, then there are no nodes for the
  // property.
  L := PropertyOffsets[N];
  if L <> $FFFF then
  begin
    // Locate the next offset that is not 0xffff.  The sentinel at the end of
    // the array is the max index value.
    M := 1;
    while ((Integer(N) + M) < High(PropertyOffsets)) and
      (PropertyOffsets[Integer(N) + M] = $FFFF) do
      Inc(M);

    R := PropertyOffsets[Integer(N) + M] - 1;

    while L <= R do
    begin
      // Determine a "mid" point and adjust to make sure the mid point is at
      // the beginning of a range pair.
      M := (L + R) shr 1;
      Dec(M, M and 1);
      if Code > PropertyRanges[M + 1] then
        L := M + 2
      else
      begin
        if Code < PropertyRanges[M] then
          R := M - 2
        else
        begin
          if (Code >= PropertyRanges[M]) and (Code <= PropertyRanges[M + 1]) then
          begin
            Result := True;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function IsProperty(Code, Mask1, Mask2: Cardinal): Boolean;
var
  I: Cardinal;
  Mask: Cardinal;
begin
  Result := False;
  if Mask1 <> 0 then
  begin
    Mask := 1;
    for I := 0 to 31 do
    begin
      if ((Mask1 and Mask) <> 0) and PropertyLookup(Code, I) then
      begin
        Result := True;
        Exit;
      end;
      Mask := Mask shl 1;
    end;
  end;

  if Mask2 <> 0 then
  begin
    I := 32;
    Mask := 1;
    while I < Cardinal(High(PropertyOffsets)) do
    begin
      if ((Mask2 and Mask) <> 0) and PropertyLookup(Code, I) then
      begin
        Result := True;
        Exit;
      end;
      Inc(I);
      Mask := Mask shl 1;
    end;
  end;
end;

//==============================================================================
// Support for case mapping
//==============================================================================

var
  CaseMapSize: Cardinal;
  CaseLengths: array [0..1] of Word;
  CaseMap: TCardinalArray;

procedure LoadUnicodeCaseData;
var
  Stream: TResourceStream;
  I: Cardinal;
  Header: TUHeader;
begin
  // make sure no other code is currently modifying the global data area
  if LoadInProgress = nil then
    LoadInProgress := TCriticalSection.Create;
  LoadInProgress.Enter;

  if CaseMap = nil then
  begin
    Stream := TResourceStream.Create(HInstance, 'CASE', 'UNICODE');
    Stream.Read(Header, SizeOf(Header));

    if Header.BOM = BOM_MSB_FIRST then
    begin
      Header.Count := Swap(Header.Count);
      Header.Len[0] := Swap(Header.Len[0]);
      Header.Len[1] := Swap(Header.Len[1]);
    end;

    // Set the node count and lengths of the upper and lower case mapping tables.
    CaseMapSize := Header.Count * 3;
    CaseLengths[0] := Header.Len[0] * 3;
    CaseLengths[1] := Header.Len[1] * 3;

    SetLength(CaseMap, CaseMapSize);

    // Load the case mapping table.
    Stream.Read(CaseMap[0], CaseMapSize * SizeOf(Cardinal));

    // Do an endian swap if necessary.
    if Header.BOM = BOM_MSB_FIRST then
    begin
      for I := 0 to CaseMapSize -1 do
        CaseMap[I] := SwapCardinal(CaseMap[I]);
    end;
    Stream.Free;
  end;
  LoadInProgress.Leave;
end;

//------------------------------------------------------------------------------

function CaseLookup(Code: Cardinal; L, R, Field: Integer): Cardinal;
var
  M: Integer;
begin
  // load case mapping data if not already done
  if CaseMap = nil then
    LoadUnicodeCaseData;

  // Do the binary search.
  while L <= R do
  begin
    // Determine a "mid" point and adjust to make sure the mid point is at
    // the beginning of a case mapping triple.
    M := (L + R) shr 1;
    Dec(M, M mod 3);
    if Code > CaseMap[M] then
      L := M + 3
    else
    begin
      if Code < CaseMap[M] then
        R := M - 3
      else
      begin
        if Code = CaseMap[M] then
        begin
          Result := CaseMap[M + Field];
          Exit;
        end;
      end;
    end;
  end;

  Result := Code;
end;

//------------------------------------------------------------------------------

function UnicodeToUpper(Code: UCS4): UCS4;
var
  Field,
  L, R: Integer;
begin
  // load case mapping data if not already done
  if CaseMap = nil then
    LoadUnicodeCaseData;

  if UnicodeIsUpper(Code) then
    Result := Code
  else
  begin
    if UnicodeIsLower(Code) then
    begin
      Field := 2;
      L := CaseLengths[0];
      R := (L + CaseLengths[1]) - 3;
    end
    else
    begin
      Field := 1;
      L := CaseLengths[0] + CaseLengths[1];
      R := CaseMapSize - 3;
    end;
    Result := CaseLookup(Code, L, R, Field);
  end;
end;

//------------------------------------------------------------------------------

function UnicodeToLower(Code: UCS4): UCS4;
var
  Field,
  L, R: Integer;
begin
  // load case mapping data if not already done
  if CaseMap = nil then
    LoadUnicodeCaseData;

  if UnicodeIsLower(Code) then
    Result := Code
  else
  begin
    if UnicodeIsUpper(Code) then
    begin
      Field := 1;
      L := 0;
      R := CaseLengths[0] - 3;
    end
    else
    begin
      Field := 2;
      L := CaseLengths[0] + CaseLengths[1];
      R := CaseMapSize - 3;
    end;
    Result := CaseLookup(Code, L, R, Field);
  end;
end;

//------------------------------------------------------------------------------

function UnicodeToTitle(Code: UCS4): UCS4;
var
  Field,
  L, R: Integer;
begin
  // load case mapping data if not already done
  if CaseMap = nil then
    LoadUnicodeCaseData;

  if UnicodeIsTitle(Code) then
    Result := Code
  else
  begin
    // The offset will always be the same for converting to title case.
    Field := 2;

    if UnicodeIsUpper(Code) then
    begin
      L := 0;
      R := CaseLengths[0] - 3;
    end
    else
    begin
      L := CaseLengths[0];
      R := (L + CaseLengths[1]) - 3;
    end;
    Result := CaseLookup(Code, L, R, Field);
  end;
end;

//==============================================================================
//  Support for decomposition
//==============================================================================

const

// Constants for hangul composition and decomposition (this is done
// algorithmically saving so significant memory)

  SBase = $AC00;
  LBase = $1100;
  VBase = $1161;
  TBase = $11A7;
  LCount = 19;
  VCount = 21;
  TCount = 28;
  NCount = VCount * TCount;   // 588
  SCount = LCount * NCount;   // 11172

var
  DecompositionSize: Cardinal;
  DecompositionNodes, Decompositions: TCardinalArray;

//------------------------------------------------------------------------------

procedure LoadUnicodeDecompositionData;
var
  Stream: TResourceStream;
  I: Cardinal;
  Header: TUHeader;
begin
  // make sure no other code is currently modifying the global data area
  if LoadInProgress = nil then
    LoadInProgress := TCriticalSection.Create;
  LoadInProgress.Enter;

  if Decompositions = nil then
  begin
    Stream := TResourceStream.Create(HInstance, 'DECOMPOSE', 'UNICODE');
    Stream.Read(Header, SizeOf(Header));

    if Header.BOM = BOM_MSB_FIRST then
    begin
      Header.Count := Swap(Header.Count);
      Header.Bytes := SwapCardinal(Header.Bytes);
    end;

    DecompositionSize := Header.Count shl 1; // two values per node
    SetLength(DecompositionNodes, DecompositionSize + 1); // one entry more (the sentinel)
    Stream.Read(DecompositionNodes[0], (DecompositionSize + 1) * SizeOf(Cardinal));
    SetLength(Decompositions, (Header.Bytes div SizeOf(Cardinal)) - DecompositionSize - 1);
    Stream.Read(Decompositions[0], Length(Decompositions) * SizeOf(Cardinal));

    // Do an endian swap if necessary.
    if Header.BOM = BOM_MSB_FIRST then
    begin
      for I := 0 to High(DecompositionNodes) do
        DecompositionNodes[I] := SwapCardinal(DecompositionNodes[I]);
      for I := 0 to High(Decompositions) do
        Decompositions[I] := SwapCardinal(Decompositions[I]);
    end;
    Stream.Free;
  end;

  LoadInProgress.Leave;
end;

//------------------------------------------------------------------------------

function UnicodeDecomposeHangul(Code: UCS4): TCardinalArray;
// algorithmically decompose hangul character using some predefined contstants
var
  Rest: Integer;
begin
  if not UnicodeIsHangul(Code) then
    Result := nil
  else
  begin
    Dec(Code, SBase);
    Rest := Code mod TCount;
    if Rest = 0 then
      SetLength(Result, 2)
    else
      SetLength(Result, 3);
    Result[0] := LBase + (Code div NCount);
    Result[1] := VBase + ((Code mod NCount) div TCount);
    if Rest <> 0 then
      Result[2] := TBase + Rest;
  end;
end;

//------------------------------------------------------------------------------

function UnicodeDecompose(Code: UCS4): TCardinalArray;
var
  L, R, M: Integer;
begin
  // load decomposition data if not already done
  if Decompositions = nil then
    LoadUnicodeDecompositionData;

  if not UnicodeIsComposite(Code) then
  begin
    // return the code itself if it is not a composite
    SetLength(Result, 1);
    Result[0] := Code;
  end
  else
  begin
    // if the code is hangul then decomposition is algorithmically
    Result := UnicodeDecomposeHangul(Code);
    if Result = nil then
    begin
      L := 0;
      R := DecompositionNodes[DecompositionSize] - 1;

      while L <= R do
      begin
        // Determine a "mid" point and adjust to make sure the mid point is at
        // the beginning of a code + offset pair.
        M := (L + R) shr 1;
        Dec(M, M and 1);
        if Code > DecompositionNodes[M] then
          L := M + 2
        else
          if Code < DecompositionNodes[M] then
            R := M - 2
          else
            if Code = DecompositionNodes[M] then
            begin
              // found a decomposition, return the codes
              SetLength(Result, DecompositionNodes[M + 3] - DecompositionNodes[M + 1] - 1);
              Move(Decompositions[DecompositionNodes[M + 1]], Result[0],
                Length(Result) * SizeOf(Cardinal));
              Break;
            end;
      end;
    end;
  end;
end;

//==============================================================================
// Support for combining classes
//==============================================================================

var
  CCLSize: Cardinal;
  CCLNodes: TCardinalArray;

//------------------------------------------------------------------------------

procedure LoadUnicodeCombiningData;
var
  Stream: TResourceStream;
  I: Cardinal;
  Header: TUHeader;
begin
  // make sure no other code is currently modifying the global data area
  if LoadInProgress = nil then
    LoadInProgress := TCriticalSection.Create;
  LoadInProgress.Enter;

  if CCLNodes = nil then
  begin
    Stream := TResourceStream.Create(HInstance, 'COMBINE', 'UNICODE');
    Stream.Read(Header, SizeOf(Header));

    if Header.BOM = BOM_MSB_FIRST then
    begin
      Header.Count := Swap(Header.Count);
      Header.Bytes := SwapCardinal(Header.Bytes);
    end;

    CCLSize := Header.Count * 3;
    SetLength(CCLNodes, CCLSize);
    Stream.Read(CCLNodes[0], CCLSize * SizeOf(Cardinal));

    if Header.BOM = BOM_MSB_FIRST then
      for I := 0 to CCLSize - 1 do
        CCLNodes[I] := SwapCardinal(CCLNodes[I]);

    Stream.Free;
  end;
  LoadInProgress.Leave;
end;

//------------------------------------------------------------------------------

function UnicodeCanonicalClass(Code: Cardinal): Cardinal;
var
  L, R, M: Integer;
begin
  // load combination data if not already done
  if CCLNodes = nil then
    LoadUnicodeCombiningData;

  Result := 0;
  L := 0;
  R := CCLSize - 1;

  while L <= R do
  begin
    M := (L + R) shr 1;
    Dec(M, M mod 3);
    if Code > CCLNodes[M + 1] then
      L := M + 3
    else
    begin
      if Code < CCLNodes[M] then
        R := M - 3
      else
      begin
        if (Code >= CCLNodes[M]) and (Code <= CCLNodes[M + 1]) then
        begin
          Result := CCLNodes[M + 2];
          Break;
        end;
      end;
    end;
  end;
end;

//----------------- Support for numeric values ---------------------------------

var
  NumberSize: Cardinal;
  NumberNodes: TCardinalArray;
  NumberValues: TWordArray;

//------------------------------------------------------------------------------

procedure LoadUnicodeNumberData;
var
  Stream: TResourceStream;
  I: Cardinal;
  Header: TUHeader;
begin
  // make sure no other code is currently modifying the global data area
  if LoadInProgress = nil then
    LoadInProgress := TCriticalSection.Create;
  LoadInProgress.Enter;

  if NumberNodes = nil then
  begin
    Stream := TResourceStream.Create(HInstance, 'NUMBERS', 'UNICODE');
    Stream.Read(Header, SizeOf(Header));

    if Header.BOM = BOM_MSB_FIRST then
    begin
      Header.Count := Swap(Header.Count);
      Header.Bytes := SwapCardinal(Header.Bytes);
    end;

    NumberSize := Header.Count;
    SetLength(NumberNodes, NumberSize);
    Stream.Read(NumberNodes[0], NumberSize * SizeOf(Cardinal));
    SetLength(NumberValues, (Header.Bytes - NumberSize * SizeOf(Cardinal)) div SizeOf(Word));
    Stream.Read(NumberValues[0], Length(NumberValues) * SizeOf(Word));

    if Header.BOM = BOM_MSB_FIRST then
    begin
      for I := 0 to High(NumberNodes) do
        NumberNodes[I] := SwapCardinal(NumberNodes[I]);
      for I := 0 to High(NumberValues) do
        NumberValues[I] := Swap(NumberValues[I]);
    end;
    Stream.Free;
  end;
  LoadInProgress.Leave;
end;

//------------------------------------------------------------------------------

function UnicodeNumberLookup(Code: UCS4; var num: TUNumber): Boolean;
var
  L, R, M: Integer;
  VP: PWord;
begin
  // load number data if not already done
  if NumberNodes = nil then
    LoadUnicodeNumberData;

  Result := False;
  L := 0;
  R := NumberSize - 1;
  while L <= R do
  begin
    // Determine a "mid" point and adjust to make sure the mid point is at
    // the beginning of a code+offset pair.
    M := (L + R) shr 1;
    Dec(M, M and 1);
    if Code > NumberNodes[M] then
      L := M + 2
    else
    begin
      if Code < NumberNodes[M] then
        R := M - 2
      else
      begin
        VP := Pointer(Cardinal(@NumberValues[0]) + NumberNodes[M + 1]);
        num.numerator := VP^;
        Inc(VP);
        num.denominator := VP^;
        Result := True;
        Break;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function UnicodeDigitLookup(Code: UCS4; var Digit: Integer): Boolean;
var
  L, R, M: Integer;
  VP: PWord;
begin
  // load number data if not already done
  if NumberNodes = nil then
    LoadUnicodeNumberData;

  Result := False;
  L := 0;
  R := NumberSize - 1;
  while L <= R do
  begin
    // Determine a "mid" point and adjust to make sure the mid point is at
    // the beginning of a code+offset pair.
    M := (L + R) shr 1;
    Dec(M, M and 1);
    if Code > NumberNodes[M] then
      L := M + 2
    else
    begin
      if Code < NumberNodes[M] then
        R := M - 2
      else
      begin
        VP := Pointer(Cardinal(@NumberValues[0]) + NumberNodes[M + 1]);
        M := VP^;
        Inc(VP);
        if M = VP^ then
        begin
          Digit := M;
          Result := True;
        end;
        Break;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function UnicodeGetNumber(Code: UCS4): TUNumber;
begin
  // Initialize with some arbitrary value, because the caller simply cannot
  // tell for sure if the code is a number without calling the ucisnumber()
  // macro before calling this function.
  Result.Numerator := -4711;
  Result.Denominator := -4711;

  UnicodeNumberLookup(Code, Result);
end;

//------------------------------------------------------------------------------

function UnicodeGetDigit(Code: UCS4): Integer;
begin
  // Initialize with some arbitrary value, because the caller simply cannot
  // tell for sure if the code is a number without calling the ucisdigit()
  //  macro before calling this function.
  Result := -4711;
  UnicodeDigitLookup(Code, Result);
end;

//==============================================================================
// TSearchEngine
//==============================================================================

constructor TSearchEngine.Create(AOwner: TWideStrings);
begin
  FOwner := AOwner;
  FResults := TList.Create;
end;

//------------------------------------------------------------------------------

destructor TSearchEngine.Destroy;
begin
  Clear;
  FResults.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TSearchEngine.AddResult(Start, Stop: Cardinal);
begin
  FResults.Add(Pointer(Start));
  FResults.Add(Pointer(Stop));
end;

//------------------------------------------------------------------------------

procedure TSearchEngine.Clear;
begin
  ClearResults;
end;

//------------------------------------------------------------------------------

procedure TSearchEngine.ClearResults;
begin
  FResults.Clear;
end;

//------------------------------------------------------------------------------

procedure TSearchEngine.DeleteResult(Index: Cardinal);
// explicitly deletes a search result
begin
  with FResults do
  begin
    // start index
    Delete(2 * Index);
    // stop index
    Delete(2 * Index);
  end;
end;

//------------------------------------------------------------------------------

function TSearchEngine.GetCount: Integer;
// returns the number of matches found
begin
  Result := FResults.Count div 2;
end;

//------------------------------------------------------------------------------

procedure TSearchEngine.GetResult(Index: Cardinal; var Start, Stop: Integer);
// returns the start position of a match (end position can be determined by
// adding the length of the pattern to the start position)
begin
  Start := Cardinal(FResults[2 * Index]);
  Stop := Cardinal(FResults[2 * Index + 1]);
end;

//==============================================================================
//  TUTBMSearch
//==============================================================================

constructor TUTBMSearch.Create(AOwner: TWideStrings);
begin
  inherited;
end;

//------------------------------------------------------------------------------

destructor TUTBMSearch.Destroy;
begin
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TUTBMSearch.ClearPattern;
begin
  FreeMem(FPattern);
  FPattern := nil;
  FFlags := [];
  FPatternUsed := 0;
  FPatternSize := 0;
  FPatternLength := 0;
  FreeMem(FSkipValues);
  FSkipValues := nil;
  FSkipsUsed := 0;
  FMD4 := 0;
end;

//------------------------------------------------------------------------------

function TUTBMSearch.GetSkipValue(TextStart, TextEnd: PUCS2): Cardinal;
// looks up the SkipValues value for a character
var
 I: Integer;
 C1, C2: UCS4;
 Sp: PUTBMSkip;
begin
  Result := 0;
  if Cardinal(TextStart) < Cardinal(TextEnd) then
  begin
    C1 := Word(TextStart^);
    if (TextStart + 1) < TextEnd then
      C2 := Word((TextStart + 1)^)
    else
      C2 := $FFFFFFFF;
    if (SurrogateHighStart <= C1) and (C1 <= SurrogateHighEnd) and
       (SurrogateLowStart <= C2) and (C2 <= $DDDD) then
      C1 := $10000 + (((C1 and $03FF) shl 10) or (C2 and $03FF));

    Sp := FSkipValues;
    for I := 0 to FSkipsUsed - 1 do
    begin
      if not (Boolean(C1 xor Sp.BMChar.UpCase) and
              Boolean(C1 xor Sp.BMChar.LoCase) and
              Boolean(C1 xor Sp.BMChar.TitleCase)) then
      begin
        if (TextEnd - TextStart) < Sp.SkipValues then
          Result := TextEnd - TextStart
        else
          Result := Sp.SkipValues;
        Exit;
      end;
      Inc(Sp);
    end;
    Result := FPatternLength;
  end;
end;

//------------------------------------------------------------------------------

function TUTBMSearch.Match(Text, Start, Stop: PUCS2; var MatchStart, MatchEnd: Cardinal): Boolean;
// Checks once whether the text at position Start (which points to the end of the
// current text part to be matched) matches.
// Note: If whole words only are allowed then the left and right border tests are
//       done here too. The keypoint for the right border is that the next character
//       after the search string is either the text end or a space character.
//       For the left side this is similar, but there is nothing like a string
//       start marker (like the string end marker #0).
//
//       It seems not obvious, but we still can use the passed Text pointer to do
//       the left check. Although this pointer might not point to the real string
//       start (e.g. in TUTBMSearch.FindAll Text is incremented as needed) it is
//       still a valid check mark. The reason is that Text either points to the
//       real string start or a previous match(happend already, keep in mind the
//       search options do not change in the FindAll loop) and the character just
//       before Text is a space character.
//       This fact implies, though, that strings passed to Find (or FindFirst,
//       FindAll in TUTBMSearch) always really start at the given address. Although
//       this might not be the case in some circumstances (e.g. if you pass only
//       the selection from an editor) it is still assumed that a pattern matching
//       from the first position on (from the search string start) also matches
//       when whole words only are allowed.
var
  CheckSpace: Boolean;
  C1, C2: UCS4;
  Count: Integer;
  Cp: PUTBMChar;
begin
  // be pessimistic
  Result := False;

  // set the potential match endpoint first
  MatchEnd := (Start - Text) + 1;

  C1 := Word(Start^);
  if (Start + 1) < Stop then
    C2 := Word((Start + 1)^)
  else
    C2 := $FFFFFFFF;
  if (SurrogateHighStart <= C1) and (C1 <= SurrogateHighEnd) and
     (SurrogateLowStart <= C2) and (C2 <= SurrogateLowEnd) then
  begin
    C1 := $10000 + (((C1 and $03FF) shl 10) or (C2 and $03FF));
    // Adjust the match end point to occur after the UTF-16 character.
    Inc(MatchEnd);
  end;

  // check special cases
  if FPatternUsed = 1 then
  begin
    MatchStart := Start - Text;
    Result := True;
    Exit;
  end;

  // Early out if entire words need to be matched and the next character
  // in the search string is neither the string end nor a space character.
  if (sfWholeWordOnly in FFlags) and
     not ((Start + 1)^ = WideNull) and
     not UnicodeIsWhiteSpace(Word((Start + 1)^)) then
    Exit;

  // compare backward
  Cp := FPattern;
  Inc(Cp, FPatternUsed - 1);

  Count := FPatternLength;
  while (Start >= Text) and (Count > 0) do
  begin
    // ignore non-spacing characters if indicated
    if sfIgnoreNonSpacing in FFlags then
    begin
      while (Start > Text) and UnicodeIsNonSpacing(C1) do
      begin
        Dec(Start);
        C2 := Word(Start^);
        if (Start - 1) > Text then
          C1 := Word((Start - 1)^)
        else
          C1 := $FFFFFFFF;
        if (SurrogateLowStart <= C2) and (C2 <= SurrogateLowEnd) and
           (SurrogateHighStart <= C1) and (C1 <= SurrogateHighEnd) then
        begin
          C1 := $10000 + (((C1 and $03FF) shl 10) or (C2 and $03FF));
          Dec(Start);
        end
        else
          C1 := C2;
      end;
    end;

    // handle space compression if indicated
    if sfSpaceCompress in FFlags then
    begin
      CheckSpace := False;
      while (Start > Text) and
            (UnicodeIsWhiteSpace(C1) or UnicodeIsControl(C1)) do
      begin
        CheckSpace := UnicodeIsWhiteSpace(C1);
        Dec(Start);
        C2 := Word(Start^);
        if (Start - 1) > Text then
          C1 := Word((Start - 1)^)
        else
          C1 := $FFFFFFFF;
        if (SurrogateLowStart <= C2) and (C2 <= SurrogateLowEnd) and
           (SurrogateHighStart <= C1) and (C1 <= SurrogateHighEnd) then
        begin
          C1 := $10000 + (((C1 and $03FF) shl 10) or (C2 and $03FF));
          Dec(Start);
        end
        else
          C1 := C2;
      end;
      // Handle things if space compression was indicated and one or
      // more member characters were found.
      if CheckSpace then
      begin
        if Cp.UpCase <> $20 then
          Exit;
        Dec(Cp);
        Dec(Count);
        // If Count is 0 at this place then the space character(s) was the first
        // in the pattern and we need to correct the start position.
        if Count = 0 then
          Inc(Start);
      end;
    end;

    // handle the normal comparison cases
    if (Count > 0) and
       (Boolean(C1 xor Cp.UpCase) and
        Boolean(C1 xor Cp.LoCase) and
        Boolean(C1 xor Cp.TitleCase)) then
      Exit;

    if C1 >= $10000 then
      Dec(Count, 2)
    else
      Dec(Count, 1);
    if Count > 0 then
    begin
      Dec(Cp);
      // get the next preceding character
      if Start > Text then
      begin
        Dec(Start);
        C2 := Word(Start^);
        if (Start - 1) > Text then
          C1 := Word((Start - 1)^)
        else
          C1 := $FFFFFFFF;
        if (SurrogateLowStart <= C2) and (C2 <= SurrogateLowEnd) and
           (SurrogateHighStart <= C1) and (C1 <= SurrogateHighEnd) then
        begin
          C1 := $10000 + (((C1 and $03FF) shl 10) or (C2 and $03FF));
          Dec(Start);
        end
        else
          C1 := C2;
      end;
    end;
  end;

  // So far the string matched. Now check its left border for a space character
  // if whole word only are allowed.
  if not (sfWholeWordOnly in FFlags) or
     (Start <= Text) or
     UnicodeIsWhiteSpace(Word((Start - 1)^)) then
  begin
    // set the match start position
    MatchStart := Start - Text;
    Result := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TUTBMSearch.Compile(Pattern: PUCS2; PatternLength: Integer; Flags: TSearchFlags);
var
  HaveSpace: Boolean;
  I, J, K,
  SLen: Integer;
  Cp: PUTBMChar;
  Sp: PUTBMSkip;
  C1, C2,
  Sentinel: UCS4;
begin
  if (Pattern <> nil) and (Pattern^ <> #0) and (PatternLength > 0) then
  begin
    // do some initialization
    FFlags := Flags;
    // extra skip flag
    FMD4 := 1;

    Sentinel := 0;

    // allocate more storage if necessary
    FPattern := AllocMem(SizeOf(TUTBMChar) * PatternLength);
    FSkipValues := AllocMem(SizeOf(TUTBMSkip) * PatternLength);
    FPatternSize := PatternLength;

    // Preprocess the pattern to remove controls (if specified) and determine case.
    Cp := FPattern;
    I := 0;
    HaveSpace := False;
    while I < PatternLength do
    begin
      C1 := Word(Pattern[I]);
      if (I + 1) < PatternLength then
        C2 := Word(Pattern[I + 1])
      else
        C2 := $FFFFFFFF;
      if (SurrogateHighStart <= C1) and (C1 <= SurrogateHighEnd) and
         (SurrogateLowStart <= C2) and (C2 <= SurrogateLowEnd) then
        C1 := $10000 + (((C1 and $03FF) shl 10) or (C2 and $03FF));

      // Make sure the HaveSpace flag is turned off if the character is not an
      // appropriate one.
      if not UnicodeIsWhiteSpace(C1) then
        HaveSpace := False;

      // If non-spacing characters should be ignored, do it here.
      if (sfIgnoreNonSpacing in Flags) and UnicodeIsNonSpacing(C1) then
      begin
        Inc(I);
        Continue;
      end;

      // check if spaces and controls need to be compressed
      if sfSpaceCompress in Flags then
      begin
        if UnicodeIsWhiteSpace(C1) then
        begin
          if not HaveSpace then
          begin
            // Add a space and set the flag.
            Cp.UpCase := $20;
            Cp.LoCase := $20;
            Cp.TitleCase := $20;
            Inc(Cp);

            // increase the real pattern length
            Inc(FPatternLength);
            Sentinel := $20;
            HaveSpace := True;
          end;
          Inc(I);
          Continue;
        end;

        // ignore all control characters
        if UnicodeIsControl(C1) then
        begin
          Inc(I);
          Continue;
        end;
      end;

      // add the character
      if not (sfCaseSensitive in Flags) then
      begin
        Cp.UpCase := UnicodeToUpper(C1);
        Cp.LoCase := UnicodeToLower(C1);
        Cp.TitleCase := UnicodeToTitle(C1);
      end
      else
      begin
        Cp.UpCase := C1;
        Cp.LoCase := C1;
        Cp.TitleCase := C1;
      end;

      Sentinel := Cp.UpCase;

      // move to the next character
      Inc(Cp);

      // increase the real pattern length appropriately
      if C1 >= $10000 then
        Inc(FPatternLength, 2)
      else
        Inc(FPatternLength);

      // increment the loop index for UTF-16 characters
      if C1 > $10000 then
        Inc(I, 2)
      else
        Inc(I);
    end;

    // set the number of characters actually used
    FPatternUsed := (PChar(Cp) - PChar(FPattern)) div SizeOf(TUTBMChar);

    // Go through and construct the skip array and determine the actual length
    // of the pattern in UCS2 terms.
    SLen := FPatternLength - 1;
    Cp := FPattern;
    K := 0;
    for I := 0 to FPatternUsed - 1 do
    begin
      // locate the character in the FSkipValues array
      Sp := FSkipValues;
      J := 0;
      while (J < FSkipsUsed) and (Sp.BMChar.UpCase <> Cp.UpCase) do
      begin
        Inc(J);
        Inc(Sp);
      end;

      // If the character is not found, set the new FSkipValues element and
      // increase the number of FSkipValues elements.
      if J = FSkipsUsed then
      begin
        Sp.BMChar := Cp;
        Inc(FSkipsUsed);
      end;

      // Set the updated FSkipValues value.  If the character is UTF-16 and is
      // not the last one in the pattern, add one to its FSkipValues value.
      Sp.SkipValues := SLen - K;
      if (Cp.UpCase >= $10000) and ((K + 2) < SLen) then
        Inc(Sp.SkipValues);

      // set the new extra FSkipValues for the sentinel character
      if ((Cp.UpCase >= $10000) and
          ((K + 2) <= SLen) or ((K + 1) <= SLen) and
          (Cp.UpCase = Sentinel)) then
        FMD4 := SLen - K;

      // increase the actual index
      if Cp.UpCase >= $10000 then
        Inc(K, 2)
      else
        Inc(K);
      Inc(Cp);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TUTBMSearch.Find(Text: PUCS2; TextLen: Cardinal; var MatchStart,
  MatchEnd: Cardinal): Boolean;
// this is the main matching routine using a tuned Boyer-Moore algorithm
var
  K: Cardinal;
  Start,
  Stop: PUCS2;
begin
  Result := False;
  if (FPattern <> nil) and (FPatternUsed > 0) and (Text <> nil) and
     (TextLen > 0) and (TextLen >= FPatternLength) then
  begin
    Start := Text + FPatternLength - 1;
    Stop := Text + TextLen;

    // adjust the start point if it points to a low surrogate
    if (SurrogateLowStart <= UCS4(Start^)) and
       (UCS4(Start^) <= SurrogateLowEnd) and
       (SurrogateHighStart <= UCS4((Start - 1)^)) and
       (UCS4((Start - 1)^) <= SurrogateHighEnd) then
      Dec(Start);

    while Start < Stop do
    begin
      repeat
        K := GetSkipValue(Start, Stop);
        if K = 0 then
          Break;
        Inc(Start, K);
        if (Start < Stop) and
           (SurrogateLowStart <= UCS4(Start^)) and
           (UCS4(Start^) <= SurrogateLowEnd) and
           (SurrogateHighStart <= UCS4((Start - 1)^)) and
           (UCS4((Start - 1)^) <= SurrogateHighEnd) then
          Dec(Start);
      until False;

      if (Start < Stop) and Match(Text, Start, Stop, MatchStart, MatchEnd) then
      begin
        Result := True;
        Break;
      end;
      Inc(Start, FMD4);
      if (Start < Stop) and
         (SurrogateLowStart <= UCS4(Start^)) and
         (UCS4(Start^) <= SurrogateLowEnd) and
         (SurrogateHighStart <= UCS4((Start - 1)^)) and
         (UCS4((Start - 1)^) <= SurrogateHighEnd) then
        Dec(Start);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TUTBMSearch.Clear;
begin
  ClearPattern;
  inherited;
end;

//------------------------------------------------------------------------------

function TUTBMSearch.FindAll(const Text: WideString): Boolean;
begin
  Result := FindAll(PWideChar(Text), Length(Text));
end;

//------------------------------------------------------------------------------

function TUTBMSearch.FindAll(const Text: PWideChar; TextLen: Cardinal): Boolean;
// Looks for all occurences of the pattern passed to FindPrepare and creates an
// internal list of their positions.
var
  Start, Stop: Cardinal;
  Run: PWideChar;
  RunLen: Cardinal;
begin
  ClearResults;
  Run := Text;
  RunLen := TextLen;
  // repeat to find all occurences of the pattern
  while Find(Run, RunLen, Start, Stop) do
  begin
    // store this result (consider text pointer movement)...
    AddResult(Start + Run - Text, Stop + Run - Text);
    // ... and advance text position and length
    Inc(Run, Stop);
    Dec(RunLen, Stop);
  end;
  Result := Count > 0;
end;

//------------------------------------------------------------------------------

function TUTBMSearch.FindFirst(const Text: WideString; var Start, Stop: Cardinal): Boolean;
// Looks for the first occurence of the pattern passed to FindPrepare in Text and
// returns True if one could be found (in which case Start and Stop are set to
// the according indices) otherwise False. This function is in particular of
// interest if only one occurence needs to be found.
begin
  ClearResults;
  Result := Find(PWideChar(Text), Length(Text), Start, Stop);
  if Result then
    AddResult(Start, Stop);
end;

//------------------------------------------------------------------------------

function TUTBMSearch.FindFirst(const Text: PWideChar; TextLen: Cardinal;
  var Start, Stop: Cardinal): Boolean;
// Same as the WideString version of this method.
begin
  ClearResults;
  Result := Find(Text, TextLen, Start, Stop);
  if Result then
    AddResult(Start, Stop);
end;

//------------------------------------------------------------------------------

procedure TUTBMSearch.FindPrepare(const Pattern: WideString; Options: TSearchFlags);
begin
  FindPrepare(PWideChar(Pattern), Length(Pattern), Options);
end;

//------------------------------------------------------------------------------

procedure TUTBMSearch.FindPrepare(const Pattern: PWideChar; PatternLength: Cardinal;
  Options: TSearchFlags);
// prepare following search by compiling the given pattern into an internal structure
begin
  Compile(Pattern, PatternLength, Options);
end;

//==============================================================================
// Unicode RE search core
//==============================================================================

const
  _URE_NONSPACING = $00000001;
  _URE_COMBINING = $00000002;
  _URE_NUMDIGIT = $00000004;
  _URE_NUMOTHER = $00000008;
  _URE_SPACESEP = $00000010;
  _URE_LINESEP = $00000020;
  _URE_PARASEP = $00000040;
  _URE_CNTRL = $00000080;
  _URE_PRIVATE = $00000100;

  _URE_UPPER = $00000200;
  _URE_LOWER = $00000400;
  _URE_TITLE = $00000800;
  _URE_MODIFIER = $00001000;
  _URE_OTHERLETTER = $00002000;
  _URE_DASHPUNCT = $00004000;
  _URE_OPENPUNCT = $00008000;
  _URE_CLOSEPUNCT = $00010000;
  _URE_OTHERPUNCT = $00020000;
  _URE_MATHSYM = $00040000;
  _URE_CURRENCYSYM = $00080000;
  _URE_OTHERSYM = $00100000;

  _URE_LTR = $00200000;
  _URE_RTL = $00400000;

  _URE_EURONUM = $00800000;
  _URE_EURONUMSEP = $01000000;
  _URE_EURONUMTERM = $02000000;
  _URE_ARABNUM = $04000000;
  _URE_COMMONSEP = $08000000;

  _URE_BLOCKSEP = $10000000;
  _URE_SEGMENTSEP = $20000000;

  _URE_WHITESPACE = $40000000;
  _URE_OTHERNEUT = $80000000;

  // Error codes

  _URE_OK = 0;
  _URE_UNEXPECTED_EOS = -1;
  _URE_CCLASS_OPEN = -2;
  _URE_UNBALANCED_GROUP = -3;
  _URE_INVALID_PROPERTY = -4;
  _URE_INVALID_RANGE = -5;
  _URE_RANGE_OPEN = -6;

  // options that can be combined for searching

  URE_IGNORE_NONSPACING = $01;
  URE_DONT_MATCHES_SEPARATORS = $02;

const

  // Flags used internally in the DFA

  _URE_DFA_CASEFOLD = $01;
  _URE_DFA_BLANKLINE = $02;

  CClassFlags: array [0..32] of Cardinal = (
    0,
    _URE_NONSPACING,
    _URE_COMBINING,
    _URE_NUMDIGIT,
    _URE_NUMOTHER,
    _URE_SPACESEP,
    _URE_LINESEP,
    _URE_PARASEP,
    _URE_CNTRL,
    _URE_PRIVATE,
    _URE_UPPER,
    _URE_LOWER,
    _URE_TITLE,
    _URE_MODIFIER,
    _URE_OTHERLETTER,
    _URE_DASHPUNCT,
    _URE_OPENPUNCT,
    _URE_CLOSEPUNCT,
    _URE_OTHERPUNCT,
    _URE_MATHSYM,
    _URE_CURRENCYSYM,
    _URE_OTHERSYM,
    _URE_LTR,
    _URE_RTL,
    _URE_EURONUM,
    _URE_EURONUMSEP,
    _URE_EURONUMTERM,
    _URE_ARABNUM,
    _URE_COMMONSEP,
    _URE_BLOCKSEP,
    _URE_SEGMENTSEP,
    _URE_WHITESPACE,
    _URE_OTHERNEUT
  );

const

  // symbol types for the DFA

  _URE_ANY_CHAR = 1;
  _URE_CHAR = 2;
  _URE_CCLASS = 3;
  _URE_NCCLASS = 4;
  _URE_BOL_ANCHOR = 5;
  _URE_EOL_ANCHOR = 6;

  // op codes for converting the NFA to a DFA

  _URE_SYMBOL = 10;
  _URE_PAREN = 11;
  _URE_QUEST = 12;
  _URE_STAR = 13;
  _URE_PLUS = 14;
  _URE_ONE = 15;
  _URE_AND = 16;
  _URE_OR = 17;

  _URE_NOOP = $FFFF;

  _URE_REGSTART = $8000;
  _URE_REGEND = $4000;

//==============================================================================
// TURESearch
//==============================================================================

constructor TURESearch.Create(AOwner: TWideStrings);
begin
  inherited;
end;

//------------------------------------------------------------------------------

destructor TURESearch.Destroy;
begin
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TURESearch.Clear;
begin
  inherited;
  ClearUREBuffer;
  ClearDFA;
end;

//------------------------------------------------------------------------------

procedure TURESearch.Push(V: Cardinal);
begin
  with FUREBuffer do
  begin
    // If the 'Reducing' parameter is True, check to see if the value passed is
    // already on the stack.
    if Reducing and ExpressionList.Expressions[Word(V)].OnStack then
      Exit;

    if Stack.ListUsed = Length(Stack.List) then
      SetLength(Stack.List, Length(Stack.List) + 8);
    Stack.List[Stack.ListUsed] := V;
    Inc(Stack.ListUsed);

    // If the 'reducing' parameter is True, flag the element as being on the Stack.
    if Reducing then
      ExpressionList.Expressions[Word(V)].OnStack := True;
  end;
end;

//------------------------------------------------------------------------------

function TURESearch.Peek: Cardinal;
begin
  if FUREBuffer.Stack.ListUsed = 0 then
    Result := _URE_NOOP
  else
    Result := FUREBuffer.Stack.List[FUREBuffer.Stack.ListUsed - 1];
end;

//------------------------------------------------------------------------------

function TURESearch.Pop: Cardinal;
begin
  if FUREBuffer.Stack.ListUsed = 0 then
    Result := _URE_NOOP
  else
  begin
    Dec(FUREBuffer.Stack.ListUsed);
    Result := FUREBuffer.Stack.List[FUREBuffer.Stack.ListUsed];
    if FUREBuffer.Reducing then
      FUREBuffer.ExpressionList.Expressions[Word(Result)].OnStack := False;
  end;
end;

//------------------------------------------------------------------------------

function TURESearch.ParsePropertyList(Properties: PUCS2; Limit: Cardinal;
  var Mask: Cardinal): Cardinal;
// Parse a comma-separated list of integers that represent character properties.
// Combine them into a mask that is returned in the 'mask' variable, and return
// the number of characters consumed.
var
  M, N: Cardinal;
  Run,
  ListEnd: PUCS2;
begin
  Run := Properties;
  ListEnd := Run + Limit;

  M := 0;
  N := 0;
  while (FUREBuffer.Error = _URE_OK) and (Run < ListEnd) do
  begin
    if Run^ = ',' then
    begin
      // Encountered a comma, so select the next character property flag and
      // reset the number.
      M := M or CClassFlags[N];
      N := 0;
    end
    else
    begin
      if (Run^ >= '0') and (Run^ <= '9') then
      begin
        // Encountered a digit, so start or Continue building the cardinal that
        // represents the character property flag.
        N := (N * 10) + Cardinal(Word(Run^) - Ord('0'));
      end
      else
      begin
        // Encountered something that is not part of the property list.
        // Indicate that we are done.
        Break;
      end;
    end;

    // If a property number greater than 32 occurs, then there is a problem.
    // Most likely a missing comma separator.
    if N > 32 then
      FUREBuffer.Error := _URE_INVALID_PROPERTY;
    Inc(Run);
  end;

  if N in [1..32] then
    M := M or CClassFlags[N];

  // Set the mask that represents the group of character properties.
  Mask := M;

  // Return the number of characters consumed.
  Result := Run - Properties;
end;

//------------------------------------------------------------------------------

function TURESearch.MakeHexNumber(NP: PUCS2; Limit: Cardinal; var Number: Cardinal): Cardinal;
// Collect a hex number with 1 to 4 digits and return the number of characters used.
var
  I: Integer;
  Run,
  ListEnd: PUCS2;
begin
  Run := np;
  ListEnd := Run + Limit;

  Number := 0;
  I := 0;
  while (I < 4) and (Run < ListEnd) do
  begin
    if (Run^ >= '0') and (Run^ <= '9') then
      Number := (Number shl 4) + Cardinal(Word(Run^) - Ord('0'))
    else
    begin
      if (Run^ >= 'A') and (Run^ <= 'F') then
        Number := (Number shl 4) + Cardinal(Word(Run^) - Ord('A')) + 10
      else
      begin
        if (Run^ >= 'a') and (Run^ <= 'f') then
          Number := (Number shl 4) + Cardinal(Word(Run^) - Ord('a')) + 10
        else
          Break;
      end;
    end;
    Inc(I);
    Inc(Run);
  end;

  Result := Run - NP;
end;

//------------------------------------------------------------------------------

procedure TURESearch.AddRange(var CCL: TUcCClass; Range: TUcRange);
// Insert a Range into a character class, removing duplicates and ordering them
// in increasing Range-start order.
var
  I: Integer;
  Temp: UCS4;
begin
  // If the `Casefold' flag is set, then make sure both endpoints of the Range
  // are converted to lower.
  if (FUREBuffer.Flags and _URE_DFA_CASEFOLD) <> 0 then
  begin
    Range.MinCode := UnicodeToLower(Range.MinCode);
    Range.MaxCode := UnicodeToLower(Range.MaxCode);
  end;

  // Swap the Range endpoints if they are not in increasing order.
  if Range.MinCode > Range.MaxCode then
  begin
    Temp := Range.MinCode;
    Range.MinCode := Range.MaxCode;
    Range.MaxCode := Temp;
  end;

  I := 0;
  while (I < CCL.RangesUsed) and (Range.MinCode < CCL.Ranges[I].MinCode) do
    Inc(I);

  // check for a duplicate
  if (I < CCL.RangesUsed) and (Range.MinCode = CCL.Ranges[I].MinCode) and
    (Range.MaxCode = CCL.Ranges[I].MaxCode) then
    Exit;

  if CCL.RangesUsed = Length(CCL.Ranges) then
    SetLength(CCL.Ranges, Length(CCL.Ranges) + 8);

  if I < CCL.RangesUsed then
    Move(CCL.Ranges[I], CCL.Ranges[I + 1], SizeOf(TUcRange) * (CCL.RangesUsed - I));

  CCL.Ranges[I].MinCode := Range.MinCode;
  CCL.Ranges[I].MaxCode := Range.MaxCode;
  Inc(CCL.RangesUsed);
end;

//------------------------------------------------------------------------------

const
  _URE_ALPHA_MASK = _URE_UPPER or _URE_LOWER or _URE_OTHERLETTER or _URE_MODIFIER or
                    _URE_TITLE or _URE_NONSPACING or _URE_COMBINING;
  _URE_ALNUM_MASK = _URE_ALPHA_MASK or _URE_NUMDIGIT;
  _URE_PUNCT_MASK = _URE_DASHPUNCT or _URE_OPENPUNCT or _URE_CLOSEPUNCT or _URE_OTHERPUNCT;
  _URE_GRAPH_MASK = _URE_NUMDIGIT or _URE_NUMOTHER or _URE_ALPHA_MASK or _URE_MATHSYM or
                    _URE_CURRENCYSYM or _URE_OTHERSYM;
  _URE_PRINT_MASK = _URE_GRAPH_MASK or _URE_SPACESEP;
  _URE_SPACE_MASK = _URE_SPACESEP or _URE_LINESEP or _URE_PARASEP;

type
  PTrie = ^TTrie;
  TTrie = record
    Key: UCS2;
    Len,
    Next: Cardinal;
    Setup: Integer;
    Mask: Cardinal;
  end;

//------------------------------------------------------------------------------

procedure TURESearch.CCLSetup(Symbol: PUcSymbolTableEntry; Mask: Cardinal);
begin
  Symbol.Props := Symbol.Props or Mask;
end;

//------------------------------------------------------------------------------

procedure TURESearch.SpaceSetup(Symbol: PUcSymbolTableEntry; Mask: Cardinal);
var
  Range: TUcRange;
begin
  Symbol.Props := Symbol.Props or Mask;

  Range.MinCode := Word(WideTabulator);
  Range.MaxCode := Word(WideTabulator);
  AddRange(Symbol.Symbol.CCL, Range);
  Range.MinCode := Word(WideCarriageReturn);
  Range.MaxCode := Word(WideCarriageReturn);
  AddRange(Symbol.Symbol.CCL, Range);
  Range.MinCode := Word(WideLineFeed);
  Range.MaxCode := Word(WideLineFeed);
  AddRange(Symbol.Symbol.CCL, Range);
  Range.MinCode := Word(WideFormFeed);
  Range.MaxCode := Word(WideFormFeed);
  AddRange(Symbol.Symbol.CCL, Range);
  Range.MinCode := $FEFF;
  Range.MaxCode := $FEFF;
  AddRange(Symbol.Symbol.CCL, Range);
end;

//------------------------------------------------------------------------------

procedure TURESearch.HexDigitSetup(Symbol: PUcSymbolTableEntry; Mask: Cardinal);
var
  Range: TUcRange;
begin
  Range.MinCode := Word('0');
  Range.MaxCode := Word('9');
  AddRange(Symbol.Symbol.CCL, Range);
  Range.MinCode := Word('A');
  Range.MaxCode := Word('F');
  AddRange(Symbol.Symbol.CCL, Range);
  Range.MinCode := Word('a');
  Range.MaxCode := Word('f');
  AddRange(Symbol.Symbol.CCL, Range);
end;

//------------------------------------------------------------------------------

const
  CClassTrie: array [0..64] of TTrie = (
    (Key: #$003A; Len: 1; Next:  1; Setup: 0; Mask: 0),
    (Key: #$0061; Len: 9; Next: 10; Setup: 0; Mask: 0),
    (Key: #$0063; Len: 8; Next: 19; Setup: 0; Mask: 0),
    (Key: #$0064; Len: 7; Next: 24; Setup: 0; Mask: 0),
    (Key: #$0067; Len: 6; Next: 29; Setup: 0; Mask: 0),
    (Key: #$006C; Len: 5; Next: 34; Setup: 0; Mask: 0),
    (Key: #$0070; Len: 4; Next: 39; Setup: 0; Mask: 0),
    (Key: #$0073; Len: 3; Next: 49; Setup: 0; Mask: 0),
    (Key: #$0075; Len: 2; Next: 54; Setup: 0; Mask: 0),
    (Key: #$0078; Len: 1; Next: 59; Setup: 0; Mask: 0),
    (Key: #$006C; Len: 1; Next: 11; Setup: 0; Mask: 0),
    (Key: #$006E; Len: 2; Next: 13; Setup: 0; Mask: 0),
    (Key: #$0070; Len: 1; Next: 16; Setup: 0; Mask: 0),
    (Key: #$0075; Len: 1; Next: 14; Setup: 0; Mask: 0),
    (Key: #$006D; Len: 1; Next: 15; Setup: 0; Mask: 0),
    (Key: #$003A; Len: 1; Next: 16; Setup: 1; Mask: _URE_ALNUM_MASK),
    (Key: #$0068; Len: 1; Next: 17; Setup: 0; Mask: 0),
    (Key: #$0061; Len: 1; Next: 18; Setup: 0; Mask: 0),
    (Key: #$003A; Len: 1; Next: 19; Setup: 1; Mask: _URE_ALPHA_MASK),
    (Key: #$006E; Len: 1; Next: 20; Setup: 0; Mask: 0),
    (Key: #$0074; Len: 1; Next: 21; Setup: 0; Mask: 0),
    (Key: #$0072; Len: 1; Next: 22; Setup: 0; Mask: 0),
    (Key: #$006C; Len: 1; Next: 23; Setup: 0; Mask: 0),
    (Key: #$003A; Len: 1; Next: 24; Setup: 1; Mask: _URE_CNTRL),
    (Key: #$0069; Len: 1; Next: 25; Setup: 0; Mask: 0),
    (Key: #$0067; Len: 1; Next: 26; Setup: 0; Mask: 0),
    (Key: #$0069; Len: 1; Next: 27; Setup: 0; Mask: 0),
    (Key: #$0074; Len: 1; Next: 28; Setup: 0; Mask: 0),
    (Key: #$003A; Len: 1; Next: 29; Setup: 1; Mask: _URE_NUMDIGIT),
    (Key: #$0072; Len: 1; Next: 30; Setup: 0; Mask: 0),
    (Key: #$0061; Len: 1; Next: 31; Setup: 0; Mask: 0),
    (Key: #$0070; Len: 1; Next: 32; Setup: 0; Mask: 0),
    (Key: #$0068; Len: 1; Next: 33; Setup: 0; Mask: 0),
    (Key: #$003A; Len: 1; Next: 34; Setup: 1; Mask: _URE_GRAPH_MASK),
    (Key: #$006F; Len: 1; Next: 35; Setup: 0; Mask: 0),
    (Key: #$0077; Len: 1; Next: 36; Setup: 0; Mask: 0),
    (Key: #$0065; Len: 1; Next: 37; Setup: 0; Mask: 0),
    (Key: #$0072; Len: 1; Next: 38; Setup: 0; Mask: 0),
    (Key: #$003A; Len: 1; Next: 39; Setup: 1; Mask: _URE_LOWER),
    (Key: #$0072; Len: 2; Next: 41; Setup: 0; Mask: 0),
    (Key: #$0075; Len: 1; Next: 45; Setup: 0; Mask: 0),
    (Key: #$0069; Len: 1; Next: 42; Setup: 0; Mask: 0),
    (Key: #$006E; Len: 1; Next: 43; Setup: 0; Mask: 0),
    (Key: #$0074; Len: 1; Next: 44; Setup: 0; Mask: 0),
    (Key: #$003A; Len: 1; Next: 45; Setup: 1; Mask: _URE_PRINT_MASK),
    (Key: #$006E; Len: 1; Next: 46; Setup: 0; Mask: 0),
    (Key: #$0063; Len: 1; Next: 47; Setup: 0; Mask: 0),
    (Key: #$0074; Len: 1; Next: 48; Setup: 0; Mask: 0),
    (Key: #$003A; Len: 1; Next: 49; Setup: 1; Mask: _URE_PUNCT_MASK),
    (Key: #$0070; Len: 1; Next: 50; Setup: 0; Mask: 0),
    (Key: #$0061; Len: 1; Next: 51; Setup: 0; Mask: 0),
    (Key: #$0063; Len: 1; Next: 52; Setup: 0; Mask: 0),
    (Key: #$0065; Len: 1; Next: 53; Setup: 0; Mask: 0),
    (Key: #$003A; Len: 1; Next: 54; Setup: 2; Mask: _URE_SPACE_MASK),
    (Key: #$0070; Len: 1; Next: 55; Setup: 0; Mask: 0),
    (Key: #$0070; Len: 1; Next: 56; Setup: 0; Mask: 0),
    (Key: #$0065; Len: 1; Next: 57; Setup: 0; Mask: 0),
    (Key: #$0072; Len: 1; Next: 58; Setup: 0; Mask: 0),
    (Key: #$003A; Len: 1; Next: 59; Setup: 1; Mask: _URE_UPPER),
    (Key: #$0064; Len: 1; Next: 60; Setup: 0; Mask: 0),
    (Key: #$0069; Len: 1; Next: 61; Setup: 0; Mask: 0),
    (Key: #$0067; Len: 1; Next: 62; Setup: 0; Mask: 0),
    (Key: #$0069; Len: 1; Next: 63; Setup: 0; Mask: 0),
    (Key: #$0074; Len: 1; Next: 64; Setup: 0; Mask: 0),
    (Key: #$003A; Len: 1; Next: 65; Setup: 3; Mask: 0)
  );

//------------------------------------------------------------------------------

function TURESearch.PosixCCL(CP: PUCS2; Limit: Cardinal; Symbol: PUcSymbolTableEntry): Cardinal;
// Probe for one of the POSIX colon delimited character classes in the static trie.
var
  I: Integer;
  N: Cardinal;
  TP: PTrie;
  Run,
  ListEnd: PUCS2;
begin
  Result := 0;
  // If the number of characters left is less than 7,
  // then this cannot be interpreted as one of the colon delimited classes.
  if Limit >= 7 then
  begin
    Run := cp;
    ListEnd := Run + Limit;
    TP := @CClassTrie[0];
    I := 0;
    while (Run < ListEnd) and (I < 8) do
    begin
      N := TP.Len;
      while (N > 0) and (TP.Key <> Run^) do
      begin
        Inc(TP);
        Dec(N);
      end;

      if N = 0 then
      begin
        Result := 0;
        Exit;
      end;

      if (Run^ = ':') and ((I = 6) or (I = 7)) then
      begin
        Inc(Run);
        Break;
      end;
      if (Run + 1) < ListEnd then
        TP := @CClassTrie[TP.Next];
      Inc(I);
      Inc(Run);
    end;

    Result := Run - CP;
    case TP.Setup of
      1:
        CCLSetup(Symbol, TP.Mask);
      2:
        SpaceSetup(Symbol, TP.Mask);
      3:
        HexDigitSetup(Symbol, TP.Mask);
    else
      Result := 0;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TURESearch.BuildCharacterClass(CP: PUCS2; Limit: Cardinal;
  Symbol: PUcSymbolTableEntry): Cardinal;
// Construct a list of ranges and return the number of characters consumed.
var
  RangeEnd: Integer;
  N: Cardinal;
  Run,
  ListEnd: PUCS2;
  C, Last: UCS4;
  Range: TUcRange;
begin
  Run := cp;
  ListEnd := Run + Limit;

  if Run^ = '^' then
  begin
    Symbol.AType := _URE_NCCLASS;
    Inc(Run);
  end
  else
    Symbol.AType := _URE_CCLASS;

  Last := 0;
  RangeEnd := 0;
  while (FUREBuffer.Error = _URE_OK) and (Run < ListEnd) do
  begin
    // Allow for the special case []abc], where the first closing bracket would end an empty
    // character class, which makes no sense. Hence this bracket is treaded literally.
    if (Run^ = ']') and (Symbol.Symbol.CCL.RangesUsed > 0) then
      Break;

    C := UCS4(Run^);
    Inc(Run);

    // escape character
    if C = Ord('\') then
    begin
      if Run = ListEnd then
      begin
        // The EOS was encountered when expecting the reverse solidus to be followed by the character it is escaping.
        // Set an Error code and return the number of characters consumed up to this point.
        FUREBuffer.Error := _URE_UNEXPECTED_EOS;
        Result := Run - CP;
        Exit;
      end;

      C := UCS4(Run^);
      Inc(Run);
      case UCS2(C) of
        'a':
          C := $07;
        'b':
          C := $08;
        'f':
          C := $0c;
        'n':
          C := $0a;
        'R':
          C := $0d;
        't':
          C := $09;
        'v':
          C := $0b;
        'p', 'P':
          begin
            Inc(Run, ParsePropertyList(Run, ListEnd - Run, Symbol.Props));
            // Invert the bit mask of the properties if this is a negated character class or if 'P' is used to specify
            // a list of character properties that should *not* match in a character class.
            if C = Ord('P') then
              Symbol.Props := not Symbol.Props;
            Continue;
          end;
        'x', 'X', 'u', 'U':
          begin
            if (Run < ListEnd) and
               ((Run^ >= '0') and (Run^ <= '9') or
                (Run^ >= 'A') and (Run^ <= 'F') or
                (Run^ >= 'a') and (Run^ <= 'f')) then
              Inc(Run, MakeHexNumber(Run, ListEnd - Run, C));
          end;
      end;
    end
    else
    begin
      if C = Ord(':') then
      begin
        // Probe for a POSIX colon delimited character class.
        Dec(Run);
        N := PosixCCL(Run, ListEnd - Run, Symbol);
        if N = 0 then
          Inc(Run)
        else
        begin
          Inc(Run, N);
          Continue;
        end;
      end;
    end;

    // Check to see if the current character is a low surrogate that needs
    // to be combined with a preceding high surrogate.
    if Last <> 0 then
    begin
      if (C >= SurrogateLowStart) and (C <= SurrogateLowEnd) then
      begin
        // Construct the UTF16 character code.
        C := $10000 + (((Last and $03FF) shl 10) or (C and $03FF))
      end
      else
      begin
        // Add the isolated high surrogate to the range.
        if RangeEnd = 1 then
          Range.MaxCode := Last and $FFFF
        else
        begin
          Range.MinCode := Last and $FFFF;
          Range.MaxCode := Last and $FFFF;
        end;

        AddRange(Symbol.Symbol.CCL, Range);
        RangeEnd := 0;
      end;
    end;

    // Clear the Last character code.
    Last := 0;

    // This slightly awkward code handles the different cases needed to construct a range.
    if (C >= SurrogateHighStart) and (C <= SurrogateHighEnd) then
    begin
      // If the high surrogate is followed by a Range indicator, simply add it as the Range start.  Otherwise,
      // save it in  the next character is a low surrogate.
      if Run^ = '-' then
      begin
        Inc(Run);
        Range.MinCode := C;
        RangeEnd := 1;
      end
      else
        Last := C;
    end
    else
    begin
      if RangeEnd = 1 then
      begin
        Range.MaxCode := C;
        AddRange(Symbol.Symbol.CCL, Range);
        RangeEnd := 0;
      end
      else
      begin
        Range.MinCode := C;
        Range.MaxCode := C;
        if Run^ = '-' then
        begin
          Inc(Run);
          RangeEnd := 1;
        end
        else
          AddRange(Symbol.Symbol.CCL, Range);
      end;
    end;
  end;

  if (Run < ListEnd) and (Run^ = ']') then
    Inc(Run)
  else
  begin
    // The parse was not terminated by the character class close symbol (']'), so set an error code.
    FUREBuffer.Error := _URE_CCLASS_OPEN;
  end;
  Result := Run - CP;
end;

//------------------------------------------------------------------------------

function TURESearch.ProbeLowSurrogate(LeftState: PUCS2; Limit: Cardinal;
  var Code: UCS4): Cardinal;
// Probe for a low surrogate hex code.
var
  I: Integer;
  Run,
  ListEnd: PUCS2;
begin
  I := 0;
  Code := 0;
  Run := LeftState;
  ListEnd := Run + Limit;

  while (I < 4) and (Run < ListEnd) do
  begin
    if (Run^ >= '0') and (Run^ <= '9') then
      Code := (Code shl 4) + Cardinal(Word(Run^) - Ord('0'))
    else
    begin
      if (Run^ >= 'A') and (Run^ <= 'F') then
        Code := (Code shl 4) + Cardinal(Word(Run^) - Ord('A')) + 10
      else
      begin
        if (Run^ >= 'a') and (Run^ <= 'f') then
          Code := (Code shl 4) + Cardinal(Word(Run^) - Ord('a')) + 10
        else
          Break;
      end;
    end;
    Inc(Run);
  end;

  if (SurrogateLowStart <= Code) and (Code <= SurrogateLowEnd) then
    Result :=  Run - LeftState
  else
    Result := 0;
end;

//------------------------------------------------------------------------------

function TURESearch.CompileSymbol(S: PUCS2; Limit: Cardinal; Symbol: PUcSymbolTableEntry): Cardinal;
var
  C: UCS4;
  Run,
  ListEnd: PUCS2;
begin
  Run := S;
  ListEnd := S + Limit;

  C := Word(Run^);
  Inc(Run);
  if C = Ord('\') then
  begin
    if Run = ListEnd then
    begin
      // The EOS was encountered when expecting the reverse solidus to be followed
      // by the character it is escaping. Set an Error code and return the number
      // of characters consumed up to this point.
      FUREBuffer.Error := _URE_UNEXPECTED_EOS;
      Result := Run - S;
      Exit;
    end;

    C := Word(Run^);
    Inc(Run);
    case UCS2(C) of
      'p', 'P':
        begin
          if UCS2(C) = 'p' then
            Symbol.AType :=_URE_CCLASS
          else
            Symbol.AType :=_URE_NCCLASS;
          Inc(Run, ParsePropertyList(Run, ListEnd - Run, Symbol.Props));
        end;
      'a':
        begin
          Symbol.AType := _URE_CHAR;
          Symbol.Symbol.Chr := $07;
        end;
      'b':
        begin
          Symbol.AType := _URE_CHAR;
          Symbol.Symbol.Chr := $08;
        end;
      'f':
        begin
          Symbol.AType := _URE_CHAR;
          Symbol.Symbol.Chr := $0C;
        end;
      'n':
        begin
          Symbol.AType := _URE_CHAR;
          Symbol.Symbol.Chr := $0A;
        end;
      'r':
        begin
          Symbol.AType := _URE_CHAR;
          Symbol.Symbol.Chr := $0D;
        end;
      't':
        begin
          Symbol.AType := _URE_CHAR;
          Symbol.Symbol.Chr := $09;
        end;
      'v':
        begin
          Symbol.AType := _URE_CHAR;
          Symbol.Symbol.Chr := $0B;
        end;
    else
      case UCS2(C) of
        'x', 'X', 'u', 'U':
          begin
            // Collect between 1 and 4 digits representing an UCS2 code.
            if (Run < ListEnd) and
              ((Run^ >= '0') and (Run^ <= '9') or
               (Run^ >= 'A') and (Run^ <= 'F') or
               (Run^ >= 'a') and (Run^ <= 'f')) then
              Inc(Run, MakeHexNumber(Run, ListEnd - Run, C));
          end;
      end;

      // Simply add an escaped character here.
      Symbol.AType := _URE_CHAR;
      Symbol.Symbol.Chr := C;
    end;
  end
  else
  begin
    if (UCS2(C) = '^') or (UCS2(C) = '$') then
    begin
      // Handle the BOL and EOL anchors. This actually consists simply of setting
      // a flag that indicates that the user supplied anchor match function should
      // be called. This needs to be done instead of simply matching line/paragraph
      // separators because beginning-of-text and end-of-text tests are needed as well.
      if UCS2(C) = '^' then
        Symbol.AType := _URE_BOL_ANCHOR
      else
        Symbol.AType := _URE_EOL_ANCHOR;
    end
    else
    begin
      if UCS2(C) = '[' then
      begin
        // construct a character class
        Inc(Run, BuildCharacterClass(Run, ListEnd - Run, Symbol));
      end
      else
      begin
        if UCS2(C) = '.' then
          Symbol.AType := _URE_ANY_CHAR
        else
        begin
          Symbol.AType := _URE_CHAR;
          Symbol.Symbol.Chr := C;
        end;
      end;
    end;
  end;

  // If the symbol type happens to be a character and is a high surrogate, then
  // probe forward to see if it is followed by a low surrogate that needs to be added.
  if (Run < ListEnd) and
     (Symbol.AType = _URE_CHAR) and
     (SurrogateHighStart <= Symbol.Symbol.Chr) and
     (Symbol.Symbol.Chr <= SurrogateHighEnd) then
  begin
    if (SurrogateLowStart <= UCS4(Run^)) and
       (UCS4(Run^) <= SurrogateLowEnd) then
    begin
      Symbol.Symbol.Chr := $10000 + (((Symbol.Symbol.Chr and $03FF) shl 10) or (Word(Run^) and $03FF));
      Inc(Run);
    end
    else
    begin
      if (Run^ = '\') and (((Run + 1)^ = 'x') or ((Run + 1)^ = 'X') or
         ((Run + 1)^ = 'u') or ((Run + 1)^ = 'U')) then
      begin
        Inc(Run, ProbeLowSurrogate(Run + 2, ListEnd - (Run + 2), C));
        if (SurrogateLowStart <= C) and (C <= SurrogateLowEnd) then
        begin
          // Take into account the \[xu] in front of the hex code.
          Inc(Run, 2);
          Symbol.Symbol.Chr := $10000 + (((Symbol.Symbol.Chr and $03FF) shl 10) or (C and $03FF));
        end;
      end;
    end;
  end;

  // Last, make sure any _URE_CHAR type symbols are changed to lower if the
  // 'Casefold' flag is set.
  if ((FUREBuffer.Flags and _URE_DFA_CASEFOLD) <> 0) and (Symbol.AType = _URE_CHAR) then
    Symbol.Symbol.Chr := UnicodeToLower(Symbol.Symbol.Chr);

  // If the symbol constructed is anything other than one of the anchors,
  // make sure the _URE_DFA_BLANKLINE flag is removed.
  if (Symbol.AType <> _URE_BOL_ANCHOR) and (Symbol.AType <> _URE_EOL_ANCHOR) then
    FUREBuffer.Flags := FUREBuffer.Flags and not _URE_DFA_BLANKLINE;

  // Return the number of characters consumed.
  Result := Run - S;
end;

//------------------------------------------------------------------------------

function TURESearch.SymbolsAreDifferent(A, B: PUcSymbolTableEntry): Boolean;
begin
  Result := False;
  if (A.AType <> B.AType) or (A.Mods <> B.Mods) or (A.Props <> B.Props) then
    Result := True
  else
  begin
    if (A.AType = _URE_CCLASS) or (A.AType = _URE_NCCLASS) then
    begin
      if A.Symbol.CCL.RangesUsed <> B.Symbol.CCL.RangesUsed then
        Result := True
      else
      begin
        if (A.Symbol.CCL.RangesUsed > 0) and
            not CompareMem(@A.Symbol.CCL.Ranges[0], @B.Symbol.CCL.Ranges[0],
                           SizeOf(TUcRange) * A.Symbol.CCL.RangesUsed) then
          Result := True;;
      end;
    end
    else
    begin
      if (A.AType = _URE_CHAR) and (A.Symbol.Chr <> B.Symbol.Chr) then
        Result := True;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TURESearch.MakeSymbol(S: PUCS2; Limit: Cardinal; var Consumed: Cardinal): Cardinal;
// Construct a symbol, but only keep unique symbols.
var
  I: Integer;
  Start: PUcSymbolTableEntry;
  Symbol: TUcSymbolTableEntry;
begin
  // Build the next symbol so we can test to see if it is already in the symbol table.
  FillChar(Symbol, SizeOf(TUcSymbolTableEntry), 0);
  Consumed := CompileSymbol(S, Limit, @Symbol);

  // Check to see if the symbol exists.
  I := 0;
  Start := @FUREBuffer.SymbolTable.Symbols[0];
  while (I < FUREBuffer.SymbolTable.SymbolsUsed) and SymbolsAreDifferent(@Symbol, Start) do
  begin
    Inc(I);
    Inc(Start);
  end;

  if I < FUREBuffer.SymbolTable.SymbolsUsed then
  begin
    // Free up any ranges used for the symbol.
    if (Symbol.AType = _URE_CCLASS) or (Symbol.AType = _URE_NCCLASS) then
      Symbol.Symbol.CCL.Ranges := nil;
    Result := FUREBuffer.SymbolTable.Symbols[I].ID;
    Exit;
  end;

  // Need to add the new symbol.
  if FUREBuffer.SymbolTable.SymbolsUsed = Length(FUREBuffer.SymbolTable.Symbols) then
  begin
    SetLength(FUREBuffer.SymbolTable.Symbols, Length(FUREBuffer.SymbolTable.Symbols) + 8);
  end;

  Symbol.ID := FUREBuffer.SymbolTable.SymbolsUsed;
  Inc(FUREBuffer.SymbolTable.SymbolsUsed);
  FUREBuffer.SymbolTable.Symbols[Symbol.ID] := Symbol;
  Result := Symbol.ID;
end;

//------------------------------------------------------------------------------

function TURESearch.MakeExpression(AType, LHS, RHS: Cardinal): Cardinal;
var
  I: Integer;
begin
  // Determine if the expression already exists or not.
  with FUREBuffer.ExpressionList do
  begin
    for I := 0 to ExpressionsUsed - 1 do
    begin
      if (Expressions[I].AType = AType) and
         (Expressions[I].LHS = LHS) and
         (Expressions[I].RHS = RHS) then
      begin
        Result := I;
        Exit;
      end;
    end;

    // Need to add a new expression.
    if ExpressionsUsed = Length(Expressions) then
      SetLength(Expressions, Length(Expressions) + 8);

    Expressions[ExpressionsUsed].OnStack := False;
    Expressions[ExpressionsUsed].AType := AType;
    Expressions[ExpressionsUsed].LHS := LHS;
    Expressions[ExpressionsUsed].RHS := RHS;

    Result := ExpressionsUsed;
    Inc(ExpressionsUsed);
  end;
end;

//------------------------------------------------------------------------------

function IsSpecial(C: Word): Boolean;
begin
  Result := C in [Word('+'), Word('*'), Word('?'), Word('{'), Word('|'), Word(')')];
end;

//------------------------------------------------------------------------------

procedure TURESearch.CollectPendingOperations(var State: Cardinal);
// collect all pending AND and OR operations and make corresponding expressions
var
  Operation: Cardinal;
begin
  repeat
    Operation := Peek;
    if (Operation <> _URE_AND) and (Operation <> _URE_OR) then
      Break;
    // make an expression with the AND or OR operator and its right hand side
    Operation := Pop;
    State := MakeExpression(Operation, Pop, State);
  until False;
end;

//------------------------------------------------------------------------------

function TURESearch.ConvertRegExpToNFA(RE: PWideChar; RELength: Cardinal): Cardinal;
// Convert the regular expression into an NFA in a form that will be easy to
// reduce to a DFA. The starting state for the reduction will be returned.
var
  C: UCS2;
  Head, Tail: PUCS2;
  S: WideString;
  Symbol,
  State,
  LastState,
  Used,
  M, N: Cardinal;
  I: Integer;
begin
  State := _URE_NOOP;

  Head := RE;
  Tail := Head + RELength;
  while (FUREBuffer.Error = _URE_OK) and (Head < Tail) do
  begin
    C := Head^;
    Inc(Head);
    case C of
      '(':
        Push(_URE_PAREN);
      ')': // check for the case of too many close parentheses
        begin
          if Peek = _URE_NOOP then
          begin
            FUREBuffer.Error := _URE_UNBALANCED_GROUP;
            Break;
          end;
          CollectPendingOperations(State);
          // remove the _URE_PAREN off the stack
          Pop;
        end;
      '*':
        State := MakeExpression(_URE_STAR, State, _URE_NOOP);
      '+':
        State := MakeExpression(_URE_PLUS, State, _URE_NOOP);
      '?':
        State := MakeExpression(_URE_QUEST, State, _URE_NOOP);
      '|':
        begin
          CollectPendingOperations(State);
          Push(State);
          Push(_URE_OR);
        end;
      '{': // expressions of the form {m, n}
        begin
          C := #0;
          M := 0;
          N := 0;
          // get first number
          while UnicodeIsWhiteSpace(Word(Head^)) do
            Inc(Head);
          S := '';
          while Head^ in [WideChar('0')..WideChar('9')] do
          begin
            S := S + Head^;
            Inc(Head);
          end;
          if S <> '' then
            M := StrToInt(S);

          while UnicodeIsWhiteSpace(Word(Head^)) do
            Inc(Head);
          if (Head^ <> ',') and (Head^ <> '}') then
          begin
            FUREBuffer.Error := _URE_INVALID_RANGE;
            Break;
          end;

          // check for an upper limit
          if Head^ <> '}' then
          begin
            Inc(Head);
            // get second number
            while UnicodeIsWhiteSpace(Word(Head^)) do
              Inc(Head);
            S := '';
            while Head^ in [WideChar('0')..WideChar('9')] do
            begin
              S := S + Head^;
              Inc(Head);
            end;
            if S <> '' then
              N := StrToInt(S);
          end
          else
            N := M;

          if Head^ <> '}' then
          begin
            FUREBuffer.Error := _URE_RANGE_OPEN;
            Break;
          end
          else
            Inc(Head);

          // N = 0 means unlimited number of occurences
          if N = 0 then
          begin
            case M of
              0: // {,} {0,}  {0, 0} mean the same as the star operator
                State := MakeExpression(_URE_STAR, State, _URE_NOOP);
              1: // {1,} {1, 0} mean the same as the plus operator
                State := MakeExpression(_URE_PLUS, State, _URE_NOOP);
            else
              begin
                // encapsulate the expanded branches as would they be in parenthesis
                // in order to avoid unwanted concatenation with pending operations/symbols
                Push(_URE_PAREN);
                // {m,} {m, 0} mean M fixed occurences plus star operator
                // make E^m...
                for I := 1 to M - 1 do
                begin
                  Push(State);
                  Push(_URE_AND);
                end;
                // ...and repeat the last symbol one or more times
                State := MakeExpression(_URE_PLUS, State, _URE_NOOP);
                CollectPendingOperations(State);
                Pop;
              end;
            end;
          end
          else
          begin
            // check proper range limits
            if M > N then
            begin
              FUREBuffer.Error := _URE_INVALID_RANGE;
              Break;
            end;

            // check special case {0, 1} (which corresponds to the ? operator)
            if (M = 0) and (N = 1) then
              State := MakeExpression(_URE_QUEST, State, _URE_NOOP)
            else
            begin
              // handle the general case by expanding {m, n} into the equivalent
              // expression E^m | E^(m + 1) | ... | E^n

              // encapsulate the expanded branches as would they be in parenthesis
              // in order to avoid unwanted concatenation with pending operations/symbols
              Push(_URE_PAREN);
              // keep initial state as this is the one all alternatives start from
              LastState := State;

              // Consider the special case M = 0 first. Because there's no construct
              // to enter a pure epsilon-transition into the expression array I
              // work around with the question mark operator to describe the first
              // and second branch alternative.
              if M = 0 then
              begin
                State := MakeExpression(_URE_QUEST, State, _URE_NOOP);
                Inc(M, 2);
                // Mark the pending OR operation (there must always follow at
                // least on more alternative because the special case {0, 1} has
                // already been handled).
                Push(State);
                Push(_URE_OR);
              end;

              while M <= N do
              begin
                State := LastState;
                // create E^M
                for I := 1 to Integer(M) - 1 do
                begin
                  Push(State);
                  Push(_URE_AND);
                end;
                // finish the branch and mark it as pending OR operation if it
                // isn't the last one
                CollectPendingOperations(State);
                if M < N then
                begin
                  Push(State);
                  Push(_URE_OR);
                end;
                Inc(M);
              end;
              // remove the _URE_PAREN off the stack
              Pop;
            end;
          end;
        end;
    else
      Dec(Head);
      Symbol := MakeSymbol(Head, Tail - Head, Used);
      Inc(Head, Used);
      State := MakeExpression(_URE_SYMBOL, Symbol, _URE_NOOP);
    end;

    if (C <> '(') and (C <> '|') and (C <> '{') and (Head < Tail) and
       (not IsSpecial(Word(Head^)) or (Head^ = '(')) then
    begin
      Push(State);
      Push(_URE_AND);
    end;
  end;

  CollectPendingOperations(State);
  if FUREBuffer.Stack.ListUsed > 0 then
    FUREBuffer.Error := _URE_UNBALANCED_GROUP;

  if FUREBuffer.Error = _URE_OK then
    Result := State
  else
    Result := _URE_NOOP;
end;

//------------------------------------------------------------------------------

procedure TURESearch.AddSymbolState(Symbol, State: Cardinal);
var
  I, J: Integer;
  Found: Boolean;
begin
  // Locate the symbol in the symbol table so the state can be added.
  // If the symbol doesn't exist, then we are in serious trouble.
  with FUREBuffer.SymbolTable do
  begin
    I := 0;
    while (I < SymbolsUsed) and (Symbol <> Symbols[I].ID) do
      Inc(I);

    Assert(I < SymbolsUsed);
  end;

  // Now find out if the state exists in the symbol's state list.
  with FUREBuffer.SymbolTable.Symbols[I].States do
  begin
    Found := False;
    for J := 0 to ListUsed - 1 do
    begin
      if State <= List[J] then
      begin
        Found := True;
        Break;
      end;
    end;

    if not Found then
      J := ListUsed;
    if not Found or (State < List[J]) then
    begin
      // Need to add the state in order.
      if ListUsed = Length(List) then
        SetLength(List, Length(List) + 8);
      if J < ListUsed then
        Move(List[J], List[J + 1], SizeOf(Cardinal) * (ListUsed - J));
      List[J] := State;
      Inc(ListUsed);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TURESearch.AddState(NewStates: array of Cardinal): Cardinal;
var
  I: Integer;
  Found: Boolean;
begin
  Found := False;
  for I := 0 to FUREBuffer.States.StatesUsed - 1 do
  begin
    if (FUREBuffer.States.States[I].StateList.ListUsed = Length(NewStates)) and
       CompareMem(@NewStates[0], @FUREBuffer.States.States[I].StateList.List[0],
         SizeOf(Cardinal) * Length(NewStates)) then
    begin
      Found := True;
      Break;
    end;
  end;

  if not Found then
  begin
    // Need to add a new DFA State (set of NFA states).
    if FUREBuffer.States.StatesUsed = Length(FUREBuffer.States.States) then
      SetLength(FUREBuffer.States.States, Length(FUREBuffer.States.States) + 8);

    with FUREBuffer.States.States[FUREBuffer.States.StatesUsed] do
    begin
      ID := FUREBuffer.States.StatesUsed;
      if (StateList.ListUsed + Length(NewStates)) >= Length(StateList.List) then
        SetLength(StateList.List, Length(StateList.List) + Length(NewStates) + 8);
      Move(NewStates[0], StateList.List[StateList.ListUsed], SizeOf(Cardinal) * Length(NewStates));
      Inc(StateList.ListUsed, Length(NewStates));
    end;
    Inc(FUREBuffer.States.StatesUsed);
  end;

  // Return the ID of the DFA state representing a group of NFA States.
  if Found then
    Result := I
  else
    Result := FUREBuffer.States.StatesUsed - 1;
end;

//------------------------------------------------------------------------------

procedure TURESearch.Reduce(Start: Cardinal);
var
  I, J,
  Symbols: Integer;
  State,
  RHS,
  s1, s2,
  ns1, ns2: Cardinal;
  Evaluating: Boolean;
begin
  FUREBuffer.Reducing := True;

  // Add the starting state for the reduction.
  AddState([Start]);

  // Process each set of NFA states that get created.
  I := 0;
  // further states are added in the loop
  while I < FUREBuffer.States.StatesUsed do
  begin
    with FUREBuffer, States.States[I], ExpressionList do
    begin
      // Push the current states on the stack.
      for J := 0 to StateList.ListUsed - 1 do
        Push(StateList.List[J]);

      // Reduce the NFA states.
      Accepting := False;
      Symbols := 0;
      J := 0;
      // need a while loop here as the stack will be modified within the loop and
      // so also its usage count used to terminate the loop
      while J < FUREBuffer.Stack.ListUsed do
      begin
        State := FUREBuffer.Stack.List[J];
        Evaluating := True;

        // This inner loop is the iterative equivalent of recursively
        // reducing subexpressions generated as a result of a reduction.
        while Evaluating do
        begin
          case Expressions[State].AType of
            _URE_SYMBOL:
              begin
                ns1 := MakeExpression(_URE_ONE, _URE_NOOP, _URE_NOOP);
                AddSymbolState(Expressions[State].LHS, ns1);
                Inc(Symbols);
                Evaluating := False;
              end;
            _URE_ONE:
              begin
                Accepting := True;
                Evaluating := False;
              end;
            _URE_QUEST:
              begin
                s1 := Expressions[State].LHS;
                ns1 := MakeExpression(_URE_ONE, _URE_NOOP, _URE_NOOP);
                State := MakeExpression(_URE_OR, ns1, s1);
              end;
            _URE_PLUS:
              begin
                s1 := Expressions[State].LHS;
                ns1 := MakeExpression(_URE_STAR, s1, _URE_NOOP);
                State := MakeExpression(_URE_AND, s1, ns1);
              end;
            _URE_STAR:
              begin
                s1 := Expressions[State].LHS;
                ns1 := MakeExpression(_URE_ONE, _URE_NOOP, _URE_NOOP);
                ns2 := MakeExpression(_URE_PLUS, s1, _URE_NOOP);
                State := MakeExpression(_URE_OR, ns1, ns2);
              end;
            _URE_OR:
              begin
                s1 := Expressions[State].LHS;
                s2 := Expressions[State].RHS;
                Push(s1);
                Push(s2);
                Evaluating := False;
              end;
            _URE_AND:
              begin
                s1 := Expressions[State].LHS;
                s2 := Expressions[State].RHS;
                case Expressions[s1].AType of
                  _URE_SYMBOL:
                    begin
                      AddSymbolState(Expressions[s1].LHS, s2);
                      Inc(Symbols);
                      Evaluating := False;
                    end;
                  _URE_ONE:
                    State := s2;
                  _URE_QUEST:
                    begin
                      ns1 := Expressions[s1].LHS;
                      ns2 := MakeExpression(_URE_AND, ns1, s2);
                      State := MakeExpression(_URE_OR, s2, ns2);
                    end;
                  _URE_PLUS:
                    begin
                      ns1 := Expressions[s1].LHS;
                      ns2 := MakeExpression(_URE_OR, s2, State);
                      State := MakeExpression(_URE_AND, ns1, ns2);
                    end;
                  _URE_STAR:
                    begin
                      ns1 := Expressions[s1].LHS;
                      ns2 := MakeExpression(_URE_AND, ns1, State);
                      State := MakeExpression(_URE_OR, s2, ns2);
                    end;
                  _URE_OR:
                    begin
                      ns1 := Expressions[s1].LHS;
                      ns2 := Expressions[s1].RHS;
                      ns1 := MakeExpression(_URE_AND, ns1, s2);
                      ns2 := MakeExpression(_URE_AND, ns2, s2);
                      State := MakeExpression(_URE_OR, ns1, ns2);
                    end;
                  _URE_AND:
                    begin
                      ns1 := Expressions[s1].LHS;
                      ns2 := Expressions[s1].RHS;
                      ns2 := MakeExpression(_URE_AND, ns2, s2);
                      State := MakeExpression(_URE_AND, ns1, ns2);
                    end;
                end;
              end;
          end;
        end;
        Inc(J);
      end;

      // clear the state stack
      while Pop <> _URE_NOOP do
        { nothing };

      // generate the DFA states for the symbols collected during the current reduction
      if (TransitionsUsed + Symbols) > Length(Transitions) then
        SetLength(Transitions, Length(Transitions) + Symbols);

      // go through the symbol table and generate the DFA state transitions for
      // each symbol that has collected NFA states
      Symbols := 0;
      J := 0;
      while J < FUREBuffer.SymbolTable.SymbolsUsed do
      begin
        begin
          if FUREBuffer.SymbolTable.Symbols[J].States.ListUsed > 0 then
          begin
            Transitions[Symbols].LHS := FUREBuffer.SymbolTable.Symbols[J].ID;
            with FUREBuffer.SymbolTable.Symbols[J] do
            begin
              RHS := AddState(Copy(States.List, 0, States.ListUsed));
              States.ListUsed := 0;
            end;
            Transitions[Symbols].RHS := RHS;
            Inc(Symbols);
          end;
        end;
        Inc(J);
      end;

      // set the number of transitions actually used
      // Note: we need again to qualify a part of the TransistionsUsed path since the
      //       state array could be reallocated in the AddState call above and the
      //       with ... do will then be invalid.
      States.States[I].TransitionsUsed := Symbols;
    end;
    Inc(I);
  end;
  FUREBuffer.Reducing := False;
end;

//------------------------------------------------------------------------------

procedure TURESearch.AddEquivalentPair(L, R: Cardinal);
var
  I: Integer;
begin
  L := FUREBuffer.States.States[L].ID;
  R := FUREBuffer.States.States[R].ID;

  if L <> R then
  begin
    if L > R then
    begin
      I := L;
      L := R;
      R := I;
    end;

    // Check to see if the equivalence pair already exists.
    I := 0;
    with FUREBuffer.EquivalentList do
    begin
      while (I < EquivalentsUsed) and
            ((Equivalents[I].Left <> L) or (Equivalents[I].Right <> R)) do
        Inc(I);

      if I >= EquivalentsUsed then
      begin
        if EquivalentsUsed = Length(Equivalents) then
          SetLength(Equivalents, Length(Equivalents) + 8);

        Equivalents[EquivalentsUsed].Left := L;
        Equivalents[EquivalentsUsed].Right := R;
        Inc(EquivalentsUsed);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TURESearch.MergeEquivalents;
// merges the DFA states that are equivalent
var
  I, J, K,
  Equal: Integer;
  Done: Boolean;
  State1, State2,
  LeftState, RightState: PUcState;
begin
  for I := 0 to FUREBuffer.States.StatesUsed - 1 do
  begin
    State1 := @FUREBuffer.States.States[I];
    if State1.ID = Cardinal(I) then
    begin
      J := 0;
      while J < I do
      begin
        State2 := @FUREBuffer.States.States[J];
        if State2.ID = Cardinal(J) then
        begin
          FUREBuffer.EquivalentList.EquivalentsUsed := 0;
          AddEquivalentPair(I, J);

          Done := False;
          Equal := 0;
          while Equal < FUREBuffer.EquivalentList.EquivalentsUsed do
          begin
            LeftState := @FUREBuffer.States.States[FUREBuffer.EquivalentList.Equivalents[Equal].Left];
            RightState := @FUREBuffer.States.States[FUREBuffer.EquivalentList.Equivalents[Equal].Right];

            if (LeftState.Accepting <> RightState.Accepting) or
               (LeftState.TransitionsUsed <> RightState.TransitionsUsed) then
            begin
              Done := True;
              Break;
            end;

            K := 0;
            while (K < LeftState.TransitionsUsed) and
                  (LeftState.Transitions[K].LHS = RightState.Transitions[K].LHS) do
              Inc(K);

            if K < LeftState.TransitionsUsed then
            begin
              Done := True;
              Break;
            end;

            for K := 0 to LeftState.TransitionsUsed - 1 do
              AddEquivalentPair(LeftState.Transitions[K].RHS, RightState.Transitions[K].RHS);

            Inc(Equal);
          end;

          if not Done then
            Break;
        end;
        Inc(J);
      end;

      if J < I then
      begin
        with FUREBuffer do
        begin
          for Equal := 0 to EquivalentList.EquivalentsUsed - 1 do
          begin
            States.States[EquivalentList.Equivalents[Equal].Right].ID :=
              States.States[EquivalentList.Equivalents[Equal].Left].ID;
          end;
        end;
      end;

    end;
  end;

  // Renumber the states appropriately
  State1 := @FUREBuffer.States.States[0];
  Equal := 0;
  for I := 0 to FUREBuffer.States.StatesUsed - 1 do
  begin
    if State1.ID = Cardinal(I) then
    begin
      State1.ID := Equal;
      Inc(Equal);
    end
    else
      State1.ID := FUREBuffer.States.States[State1.ID].ID;
    Inc(State1);
  end;
end;

//------------------------------------------------------------------------------

procedure TURESearch.ClearUREBuffer;
var
  I: Integer;
begin
  with FUREBuffer do
  begin
    // quite a few dynamic arrays to free
    Stack.List := nil;
    ExpressionList.Expressions := nil;

    // the symbol table has been handed over to the DFA and will be freed on
    // release of the DFA
    SymbolTable.SymbolsUsed := 0;

    for I := 0 to States.StatesUsed - 1 do
    begin
      States.States[I].Transitions := nil;
      States.States[I].StateList.List := nil;
      States.States[I].StateList.ListUsed := 0;
      States.States[I].TransitionsUsed := 0;
    end;

    States.StatesUsed := 0;
    States.States := nil;
    EquivalentList.Equivalents := nil;
  end;
  FillChar(FUREBuffer, SizeOf(FUREBuffer), 0);
end;

//------------------------------------------------------------------------------

procedure TURESearch.CompileURE(RE: PWideChar; RELength: Cardinal; Casefold: Boolean);
var
  I, J: Integer;
  State: Cardinal;
  Run: PUcState;
  TP: Integer;
begin
  // be paranoid
  if (RE <> nil) and (RE^ <> WideNull) and (RELength > 0) then
  begin
    // Reset the various fields of the compilation buffer. Default the Flags
    // to indicate the presense of the "^$" pattern.  If any other pattern
    // occurs, then this flag will be removed.  This is done to catch this
    // special pattern and handle it specially when matching.
    ClearUREBuffer;
    ClearDFA;
    FUREBuffer.Flags := _URE_DFA_BLANKLINE;
    if Casefold then
      FUREBuffer.Flags := FUREBuffer.Flags or _URE_DFA_CASEFOLD;

    // Construct the NFA. If this stage returns a 0, then an error occured or an
    // empty expression was passed.
    State := ConvertRegExpToNFA(RE, RELength);
    if State <> _URE_NOOP then
    begin
      // Do the expression reduction to get the initial DFA.
      Reduce(State);

      // Merge all the equivalent DFA States.
      MergeEquivalents;

      // Construct the minimal DFA.
      FDFA.Flags := FUREBuffer.Flags and (_URE_DFA_CASEFOLD or _URE_DFA_BLANKLINE);

      // Free up the NFA state groups and transfer the symbols from the buffer
      // to the DFA.
      FDFA.SymbolTable := FUREBuffer.SymbolTable;
      FUREBuffer.SymbolTable.Symbols := nil;

      // Collect the total number of states and transitions needed for the DFA.
      State := 0;
      for I := 0 to FUREBuffer.States.StatesUsed - 1 do
      begin
        if FUREBuffer.States.States[I].ID = State then
        begin
          Inc(FDFA.StateList.StatesUsed);
          Inc(FDFA.TransitionList.TransitionsUsed, FUREBuffer.States.States[I].TransitionsUsed);
          Inc(State);
        end;
      end;

      // Allocate enough space for the states and transitions.
      SetLength(FDFA.StateList.States, FDFA.StateList.StatesUsed);
      SetLength(FDFA.TransitionList.Transitions, FDFA.TransitionList.TransitionsUsed);

      // Actually transfer the DFA States from the buffer.
      State := 0;
      TP := 0;
      Run := @FUREBuffer.States.States[0];
      for I := 0 to FUREBuffer.States.StatesUsed - 1 do
      begin
        if Run.ID = State then
        begin
          FDFA.StateList.States[I].StartTransition := TP;
          FDFA.StateList.States[I].NumberTransitions := Run.TransitionsUsed;
          FDFA.StateList.States[I].Accepting := Run.Accepting;

          // Add the transitions for the state
          for J := 0 to FDFA.StateList.States[I].NumberTransitions - 1 do
          begin
            FDFA.TransitionList.Transitions[TP].Symbol := Run.Transitions[J].LHS;
            FDFA.TransitionList.Transitions[TP].NextState :=
              FUREBuffer.States.States[Run.Transitions[J].RHS].ID;
            Inc(TP);
          end;

          Inc(State);
        end;
        Inc(Run);
      end;
    end
    else
    begin
      // there might be an error while parsing the pattern, show it if so
      case FUREBuffer.Error of
        _URE_UNEXPECTED_EOS:
          raise Exception.CreateFmt(RsUREBaseString + RsUREUnexpectedEOS, [RE]);
        _URE_CCLASS_OPEN:
          raise Exception.CreateFmt(RsUREBaseString + RsURECharacterClassOpen, [RE]);
        _URE_UNBALANCED_GROUP:
          raise Exception.CreateFmt(RsUREBaseString + RsUREUnbalancedGroup, [RE]);
        _URE_INVALID_PROPERTY:
          raise Exception.CreateFmt(RsUREBaseString + RsUREInvalidCharProperty, [RE]);
        _URE_INVALID_RANGE:
          raise Exception.CreateFmt(RsUREBaseString + RsUREInvalidRepeatRange, [RE]);
        _URE_RANGE_OPEN:
          raise Exception.CreateFmt(RsUREBaseString + RsURERepeatRangeOpen, [RE]);
      else
        // expression was empty
        raise Exception.Create(RsUREExpressionEmpty);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TURESearch.ClearDFA;
var
  I: Integer;
begin
  with FDFA do
  begin
    for I := 0 to SymbolTable.SymbolsUsed - 1 do
    begin
      if (SymbolTable.Symbols[I].AType = _URE_CCLASS) or
         (SymbolTable.Symbols[I].AType = _URE_NCCLASS) then
        SymbolTable.Symbols[I].Symbol.CCL.Ranges := nil;
    end;

    for I := 0 to SymbolTable.SymbolsUsed - 1 do
    begin
      FDFA.SymbolTable.Symbols[I].States.List := nil;
      FDFA.SymbolTable.Symbols[I].States.ListUsed := 0;
    end;
    SymbolTable.SymbolsUsed := 0;

    SymbolTable.Symbols := nil;
    StateList.States := nil;
    TransitionList.Transitions := nil;
  end;
  FillChar(FDFA, SizeOf(FDFA), 0);
end;

//------------------------------------------------------------------------------

function IsSeparator(C: UCS4): Boolean;
begin
  Result := (C = $D) or (C = $A) or (C = $2028) or (C = $2029);
end;

//------------------------------------------------------------------------------

const
  PropertyMap: array [0..31] of Cardinal = (
    0, // class ID 1, corresponds to UC_MN
    1, // class ID 2, UC_MC
    3, // 3, UC_ND
    5, // 4, UC_NO
    6, // 5, UC_ZS
    7, // 6, UC_ZL
    8, // 7, UC_ZP
    9, // 8, UC_CC
    12, // 9, UC_CO
    14, // 10, UC_LU
    15, // 11, UC_LL
    16, // 12, UC_LT
    17, // 13, UC_LM
    18, // 14, UC_LO
    20, // 15, UC_PD
    21, // 16, UC_PS
    22, // 17, UC_PE
    23, // 18, UC_PO
    24, // 19, UC_SM
    25, // 20, UC_SC
    26, // 21, UC_SO
    27, // 22, UC_L
    28, // 23, UC_R
    29, // 24, UC_EN
    30, // 25, UC_ES
    32, // 26, UC_ET
    33, // 27, UC_AN
    34, // 28, UC_CS
    35, // 29, UC_B
    36, // 30, UC_S
    37, // 31, UC_WS
    38  // 32, UC_ON
    );

function TURESearch.MatchesProperties(Props, C: Cardinal): Boolean;
// tries to match any of the given properties
var
  I: Integer;
  Mask: Cardinal;
begin
  Result := False;
  // We need only one match in order to tell the caller success,
  // but unfortunately we cannot directly map the URE flags to the
  // Unicode property flags. Hence we need to loop and explicitly remap them.
  Mask := 1;
  for I := 0 to 31 do
  begin
    if ((Props and Mask) <> 0) and PropertyLookup(C, PropertyMap[I]) then
    begin
      Result := True;
      Exit;
    end;
    Mask := Mask shl 1;
  end;
end;

//------------------------------------------------------------------------------

function TURESearch.ExecuteURE(Flags: Cardinal; Text: PUCS2; TextLen: Cardinal;
  var MatchStart, MatchEnd: Cardinal): Boolean;
var
  I, J: Integer;
  Matched,
  Found: Boolean;
  Start, Stop: Integer;
  C: UCS4;
  Run, Tail, lp: PUCS2;
  LastState: PDFAState;
  Symbol: PUcSymbolTableEntry;
  Rp: PUcRange;
begin
  Result := False;
  if Text <> nil then
  begin
    // Handle the special case of an empty string matching the "^$" pattern.
    if (Textlen = 0) and ((FDFA.Flags and _URE_DFA_BLANKLINE) <> 0) then
    begin
      MatchStart := 0;
      MatchEnd := 0;
      Result := True;
      Exit;
    end;

    Run := Text;
    Tail := Run + TextLen;
    Start := -1;
    Stop := -1;
    LastState := @FDFA.StateList.States[0];

    Found := False;
    while not Found and (Run < Tail) do
    begin
      lp := Run;
      C := UCS4(Run^);
      Inc(Run);

      // Check to see if this is a high surrogate that should be combined with a
      // following low surrogate.
      if (Run < Tail) and
         (SurrogateHighStart <= C) and (C <= SurrogateHighEnd) and
         (SurrogateLowStart <= UCS4(Run^)) and (UCS4(Run^) <= SurrogateLowEnd) then
      begin
        C := $10000 + (((C and $03FF) shl 10) or (UCS4(Run^) and $03FF));
        Inc(Run);
      end;

      // Determine if the character is non-spacing and should be skipped.
      if ((Flags and URE_IGNORE_NONSPACING) <> 0) and UnicodeIsNonSpacingMark(C) then
      begin
        Inc(Run);
        Continue;
      end;

      if (FDFA.Flags and _URE_DFA_CASEFOLD) <> 0 then
        C := UnicodeToLower(C);

      // See if one of the transitions matches.
      I := LastState.NumberTransitions - 1;
      Matched := False;

      while not Matched and (I >= 0) do
      begin
        Symbol := @FDFA.SymbolTable.Symbols[FDFA.TransitionList.Transitions[LastState.StartTransition + I].Symbol];
        case Symbol.AType of
          _URE_ANY_CHAR:
            if ((Flags and URE_DONT_MATCHES_SEPARATORS) <> 0) or
               not IsSeparator(C) then
              Matched := True;
          _URE_CHAR:
            if C = Symbol.Symbol.Chr then
              Matched := True;
          _URE_BOL_ANCHOR:
            if Lp = Text then
            begin
              Run := lp;
              Matched := True;
            end
            else
            begin
              if IsSeparator(C) then
              begin
                if (C = $D) and (Run < Tail) and (Run^ = #$A) then
                  Inc(Run);
                Lp := Run;
                Matched := True;
              end;
            end;
          _URE_EOL_ANCHOR:
            if IsSeparator(C) then
            begin
              // Put the pointer back before the separator so the match end
              // position will be correct. This  will also cause the `Run'
              // pointer to be advanced over the current separator once the
              // match end point has been recorded.
              Run := Lp;
              Matched := True;
            end;
          _URE_CCLASS,
          _URE_NCCLASS:
            with Symbol^ do
            begin
              if Props <> 0 then
                Matched := MatchesProperties(Props, C);
              if Symbol.CCL.RangesUsed > 0 then
              begin
                Rp := @Symbol.CCL.Ranges[0];
                for J := 0 to Symbol.CCL.RangesUsed - 1 do
                begin
                  if (Rp.MinCode <= C) and (C <= Rp.MaxCode) then
                  begin
                    Matched := True;
                    Break;
                  end;
                  Inc(Rp);
                end;
              end;

              if AType = _URE_NCCLASS then
                Matched := not Matched;
            end;
        end;

        if Matched then
        begin
          if Start = -1 then
            Start := Lp - Text
          else
            Stop := Run - Text;

          LastState := @FDFA.StateList.States[FDFA.TransitionList.Transitions[LastState.StartTransition + I].NextState];

          // If the match was an EOL anchor, adjust the pointer past the separator
          // that caused the match. The correct match position has been recorded
          // already.
          if Symbol.AType = _URE_EOL_ANCHOR then
          begin
            // skip the character that caused the match.
            Inc(Run);
            // Handle the infamous CRLF situation.
            if (Run < Tail) and (C = $D) and (Run^ = #$A) then
              Inc(Run);
          end;
        end;
        Dec(I);
      end;

      if not Matched then
      begin
        Found := LastState.Accepting;
        if not Found then
        begin
          // If the last state was not accepting, then reset and start over.
          LastState := @FDFA.StateList.States[0];
          Start := -1;
          Stop := -1;
        end
        else
        begin
          // set start and stop pointer if not yet done
          if Start = -1 then
          begin
            Start := Lp - Text;
            Stop := Run - Text;
          end
          else
          begin
            if Stop = -1 then
              Stop := Lp - Text;
          end;
        end;
      end
      else
      begin
        if Run = Tail then
        begin
          if not LastState.Accepting then
          begin
            // This ugly hack is to make sure the end-of-line anchors match
            // when the source text hits the end. This is only done if the last
            // subexpression matches.
            for I := 0 to LastState.NumberTransitions - 1 do
            begin
              if Found then
                Break;
              Symbol := @FDFA.SymbolTable.Symbols[FDFA.TransitionList.Transitions[LastState.StartTransition + I].Symbol];
              if Symbol.AType =_URE_EOL_ANCHOR then
              begin
                LastState := @FDFA.StateList.States[FDFA.TransitionList.Transitions[LastState.StartTransition + I].NextState];
                if LastState.Accepting then
                begin
                  Stop := Run - Text;
                  Found := True;
                end
                else
                  Break;
              end;
            end;
          end
          else
          begin
            // Make sure any conditions that match all the way to the end of
            // the string match.
            Found := True;
            Stop := Run - Text;
          end;
        end;
      end;
    end;

    if Found then
    begin
      MatchStart := Start;
      MatchEnd := Stop;
    end;
    Result := Found;
  end;
end;

//------------------------------------------------------------------------------

function TURESearch.FindAll(const Text: WideString): Boolean;
begin
  Result := FindAll(PWideChar(Text), Length(Text));
end;

//------------------------------------------------------------------------------

function TURESearch.FindAll(const Text: PWideChar; TextLen: Cardinal): Boolean;
// Looks for all occurences of the pattern passed to FindPrepare and creates an
// internal list of their positions.
var
  Start, Stop: Cardinal;
  Run: PWideChar;
  RunLen: Cardinal;
begin
  ClearResults;
  Run := Text;
  RunLen := TextLen;
  // repeat to find all occurences of the pattern
  while ExecuteURE(0, Run, RunLen, Start, Stop) do
  begin
    // store this result (consider text pointer movement)...
    AddResult(Start + Run - Text, Stop + Run - Text);
    // ... and advance text position and length
    Inc(Run, Stop);
    Dec(RunLen, Stop);
  end;
  Result := FResults.Count > 0;
end;

//------------------------------------------------------------------------------

function TURESearch.FindFirst(const Text: WideString; var Start, Stop: Cardinal): Boolean;
begin
  Result := FindFirst(PWideChar(Text), Length(Text), Start, Stop);
end;

//------------------------------------------------------------------------------

function TURESearch.FindFirst(const Text: PWideChar; TextLen: Cardinal;
  var Start, Stop: Cardinal): Boolean;
// Looks for the first occurence of the pattern passed to FindPrepare in Text and
// returns True if one could be found (in which case Start and Stop are set to
// the according indices) otherwise False. This function is in particular of
// interest if only one occurence needs to be found.
begin
  ClearResults;
  Result := ExecuteURE(0, PWideChar(Text), Length(Text), Start, Stop);
  if Result then
    AddResult(Start, Stop);
end;

//------------------------------------------------------------------------------

procedure TURESearch.FindPrepare(const Pattern: PWideChar; PatternLength: Cardinal;
  Options: TSearchFlags);
begin
  CompileURE(Pattern, PatternLength, not (sfCaseSensitive in Options));
end;

//------------------------------------------------------------------------------

procedure TURESearch.FindPrepare(const Pattern: WideString; Options: TSearchFlags);
begin
  CompileURE(PWideChar(Pattern), Length(Pattern), not (sfCaseSensitive in Options));
end;

//==============================================================================
// TWideStrings
//==============================================================================

constructor TWideStrings.Create;
begin
  inherited;
  // there should seldom be the need to use a language other than the one of the
  // system
  FLanguage := GetUserDefaultLCID;
end;

//------------------------------------------------------------------------------

destructor TWideStrings.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TWideStrings.SetLanguage(Value: LCID);
begin
  FLanguage := Value;
end;

//------------------------------------------------------------------------------

function TWideStrings.Add(const S: WideString): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

//------------------------------------------------------------------------------

function TWideStrings.AddObject(const S: WideString; AObject: TObject): Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

//------------------------------------------------------------------------------

procedure TWideStrings.Append(const S: WideString);
begin
  Add(S);
end;

//------------------------------------------------------------------------------

procedure TWideStrings.AddStrings(Strings: TStrings);
var
  I: Integer;
  S: WideString;
  CP: Integer;
begin
  BeginUpdate;
  try
    CP := CodePageFromLocale(FLanguage);
    for I := 0 to Strings.Count - 1 do
    begin
      S := StringToWideStringEx(Strings[I], CP);
      AddObject(S, Strings.Objects[I]);
    end;
  finally
    EndUpdate;
  end;
end;

//------------------------------------------------------------------------------

procedure TWideStrings.AddStrings(Strings: TWideStrings);
var
  I: Integer;
  SourceCP,
  TargetCP: Integer;
  S: WideString;
begin
  BeginUpdate;
  try
    if Strings.FLanguage <> FLanguage then
    begin
      SourceCP := CodePageFromLocale(Strings.FLanguage);
      TargetCP := CodePageFromLocale(FLanguage);
      for I := 0 to Strings.Count - 1 do
      begin
        S := TranslateString(Strings[I], SourceCP, TargetCP);
        AddObject(S, Strings.Objects[I]);
      end;
    end
    else
    begin
      for I := 0 to Strings.Count - 1 do
        AddObject(Strings[I], Strings.Objects[I]);
    end;
  finally
    EndUpdate;
  end;
end;

//------------------------------------------------------------------------------

procedure TWideStrings.Assign(Source: TPersistent);
// usual assignment routine, but able to assign wide and small strings
begin
  if Source is TWideStrings then
  begin
    BeginUpdate;
    try
      Clear;
      AddStrings(TWideStrings(Source));
    finally
      EndUpdate;
    end;
  end
  else
  begin
    if Source is TStrings then
    begin
      BeginUpdate;
      try
        Clear;
        AddStrings(TStrings(Source));
      finally
        EndUpdate;
      end;
    end
    else
      inherited Assign(Source);
  end;
end;

//------------------------------------------------------------------------------

procedure TWideStrings.AssignTo(Dest: TPersistent);
// need to do also assignment to old style TStrings, but this class doesn't know
// TWideStrings, so we need to do it from here
var
  I: Integer;
  S: string;
  CP: Integer;
begin
  if Dest is TStrings then
  begin
    with Dest as TStrings do
    begin
      BeginUpdate;
      try
        CP := CodePageFromLocale(FLanguage);
        Clear;
        for I := 0 to Self.Count - 1 do
        begin
          S := WideStringToStringEx(Self[I], CP);
          AddObject(S, Self.Objects[I]);
        end;
      finally
        EndUpdate;
      end;
    end;
  end
  else
  begin
    if Dest is TWideStrings then
    begin
      with Dest as TWideStrings do
      begin
        BeginUpdate;
        try
          Clear;
          AddStrings(Self);
        finally
          EndUpdate;
        end;
      end;
    end
    else
      inherited;
  end;
end;

//------------------------------------------------------------------------------

procedure TWideStrings.BeginUpdate;
begin
  if FUpdateCount = 0 then
    SetUpdateState(True);
  Inc(FUpdateCount);
end;

//------------------------------------------------------------------------------

procedure TWideStrings.DefineProperties(Filer: TFiler);
// Defines a private property for the content of the list.
// There's a bug in the handling of text DFMs in Classes.pas which prevents
// WideStrings from loading under some circumstances. Zbysek Hlinka
// (zhlinka@login.cz) brought this to my attention and supplied also a solution.
// See ReadData and WriteData methods for implementation details.

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TWideStrings then
        Result := not Equals(TWideStrings(Filer.Ancestor))
    end
    else
      Result := Count > 0;
  end;

begin
  Filer.DefineProperty('WideStrings', ReadData, WriteData, DoWrite);
end;

//------------------------------------------------------------------------------

procedure TWideStrings.DoConfirmConversion(var Allowed: Boolean);
begin
  if Assigned(FOnConfirmConversion) then
    FOnConfirmConversion(Self, Allowed);
end;

//------------------------------------------------------------------------------

procedure TWideStrings.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    SetUpdateState(False);
end;

//------------------------------------------------------------------------------

function TWideStrings.Equals(Strings: TWideStrings): Boolean;
var
  I, Count: Integer;
begin
  Result := False;
  Count := GetCount;
  if Count <> Strings.GetCount then
    Exit;
  // TODO use internal comparation routine as soon as composition is implemented
  for I := 0 to Count - 1 do
    if Get(I) <> Strings.Get(I) then
      Exit;
  Result := True;
end;

//------------------------------------------------------------------------------

procedure TWideStrings.Error(const Msg: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
          MOV     EAX, [EBP + 4]
  end;

begin
  raise EStringListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

//------------------------------------------------------------------------------

procedure TWideStrings.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: WideString;
begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;

//------------------------------------------------------------------------------

function TWideStrings.GetCapacity: Integer;
// descendants may optionally override/replace this default implementation
begin
  Result := Count;
end;

//------------------------------------------------------------------------------

function TWideStrings.GetCommaText: WideString;
var
  S: WideString;
  P: PWideChar;
  I, Count: Integer;
begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = '') then
    Result := '""'
  else
  begin
    Result := '';
    for I := 0 to Count - 1 do
    begin
      S := Get(I);
      P := PWideChar(S);
      while not (P^ in [WideNull..WideSpace, WideChar('"'), WideChar(',')]) do
        Inc(P);
      if (P^ <> WideNull) then
        S := WideQuotedStr(S, '"');
      Result := Result + S + ', ';
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;

//------------------------------------------------------------------------------

function TWideStrings.GetName(Index: Integer): WideString;
var
  P: Integer;
begin
  Result := Get(Index);
  P := Pos('=', Result);
  if P > 0 then
    SetLength(Result, P - 1)
  else
    Result := '';
end;

//------------------------------------------------------------------------------

function TWideStrings.GetObject(Index: Integer): TObject;
begin
  Result := nil;
end;

//------------------------------------------------------------------------------

function TWideStrings.GetSeparatedText(Separators: WideString): WideString;
// same as GetText but with customizable separator characters
var
  I, L,
  Size,
  Count,
  SepSize: Integer;
  P: PWideChar;
  S: WideString;
begin
  Count := GetCount;
  SepSize := Length(Separators);
  Size := 0;
  for I := 0 to Count - 1 do
    Inc(Size, Length(Get(I)) + SepSize);
  SetLength(Result, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      // add current string
      System.Move(Pointer(S)^, P^, 2 * L);
      Inc(P, L);
    end;
    // add separators
    System.Move(Pointer(Separators)^, P^, 2 * SepSize);
    Inc(P, SepSize);
  end;
end;

//------------------------------------------------------------------------------

function TWideStrings.GetText: WideString;
var
  I, L, Size, Count: Integer;
  P: PWideChar;
  S: WideString;
begin
  Count := GetCount;
  Size := 0;
  for I := 0 to Count - 1 do
    Inc(Size, Length(Get(I)) + 2);
  SetLength(Result, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, 2 * L);
      Inc(P, L);
    end;
    P^ := WideCarriageReturn;
    Inc(P);
    P^ := WideLineFeed;
    Inc(P);
  end;
end;

//------------------------------------------------------------------------------

function TWideStrings.GetValue(const Name: WideString): WideString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := Copy(Get(I), Length(Name) + 2, MaxInt)
  else
    Result := '';
end;

//------------------------------------------------------------------------------

function TWideStrings.IndexOf(const S: WideString): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if WideCompareText(Get(Result), S, FLanguage) = 0 then
      Exit;
  Result := -1;
end;

//------------------------------------------------------------------------------

function TWideStrings.IndexOfName(const Name: WideString): Integer;
var
  P: Integer;
  S: WideString;
begin
  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := Pos('=', S);
    if (P > 0) and (WideCompareText(Copy(S, 1, P - 1), Name, FLanguage) = 0) then
      Exit;
  end;
  Result := -1;
end;

//------------------------------------------------------------------------------

function TWideStrings.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then
      Exit;
  Result := -1;
end;

//------------------------------------------------------------------------------

procedure TWideStrings.InsertObject(Index: Integer; const S: WideString; AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

//------------------------------------------------------------------------------

procedure TWideStrings.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  except
    RaiseLastWin32Error;
  end;
end;

//------------------------------------------------------------------------------

procedure TWideStrings.LoadFromStream(Stream: TStream);
// usual loader routine, but enhanced to handle byte order marks in stream
var
  Size,
  BytesRead: Integer;
  Order: WideChar;
  SW: WideString;
  SA: string;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    BytesRead := Stream.Read(Order, 2);
    if (Order = BOM_LSB_FIRST) or (Order = BOM_MSB_FIRST) then
    begin
      FSaveUnicode := True;
      SetLength(SW, (Size - 2) div 2);
      Stream.Read(PWideChar(SW)^, Size - 2);
      if Order = BOM_MSB_FIRST then
        StrSwapByteOrder(PWideChar(SW));
      SetText(SW);
    end
    else
    begin
      // without byte order mark it is assumed that we are loading ANSI text
      FSaveUnicode := False;
      Stream.Seek(-BytesRead, soFromCurrent);
      SetLength(SA, Size);
      Stream.Read(PChar(SA)^, Size);
      // TODO IsTextUnicode - ml: does not work, this function is only available on WinNT
      SetText(SA);
    end;
  finally
    EndUpdate;
  end;
end;

//------------------------------------------------------------------------------

procedure TWideStrings.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: WideString;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TWideStrings.Put(Index: Integer; const S: WideString);
var
  TempObject: TObject;
begin
  TempObject := GetObject(Index);
  Delete(Index);
  InsertObject(Index, S, TempObject);
end;

//------------------------------------------------------------------------------

procedure TWideStrings.PutObject(Index: Integer; AObject: TObject);
begin
end;

//------------------------------------------------------------------------------

procedure TWideStrings.ReadData(Reader: TReader);
var
  W: WideString;
  Len: Integer;
  Value: TValueType;
begin
  Reader.Read(Value, SizeOf(Value));
  if Value <> vaWString then
    raise Exception.Create(SInvalidProperty);
  Reader.Read(Len, SizeOf(Len));
  if Len > 0 then
  begin
    SetLength(W, Len);
    Reader.Read(PWideChar(W)^, 2 * Len);
    SetText(W);
  end;
end;

//------------------------------------------------------------------------------

procedure TWideStrings.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TWideStrings.SaveToStream(Stream: TStream);
var
  SW, BOM: WideString;
  SA: string;
  Allowed: Boolean;
  Run: PWideChar;
begin
  // The application can decide in which format to save the content.
  // If FSaveUnicode is False then all strings are saved in standard ANSI format
  // which is also loadable by TStrings but you should be aware that all Unicode
  // strings are then converted to ANSI based on the current system locale.
  // An extra event is supplied to ask the user about the potential loss of
  // information when converting Unicode to ANSI strings.
  SW := GetText;
  Allowed := True;
  FSaved := False; // be pessimistic
  // check for potential information loss makes only sense if the application has
  // set an event to be used as call back to ask about the conversion
  if not FSaveUnicode and Assigned(FOnConfirmConversion) then
  begin
    // application requests to save only ANSI characters, so check the text and
    // call back in case information could be lost
    Run := PWideChar(SW);
    // only ask if there's at least one Unicode character in the text
    while Run^ in [WideChar(#1)..WideChar(#255)] do
      Inc(Run);
    // Note: The application can still set FSaveUnicode to True in the callback.
    if Run^ <> WideNull then
      DoConfirmConversion(Allowed);
  end;

  if Allowed then
  begin
    // only save if allowed
    if FSaveUnicode then
    begin
      BOM := BOM_LSB_FIRST;
      Stream.WriteBuffer(PWideChar(BOM)^, 2);
      // SW has already been filled
      Stream.WriteBuffer(PWideChar(SW)^, 2 * Length(SW));
    end
    else
    begin
      SA := WideStringToStringEx(SW, CodePageFromLocale(FLanguage));
      if Allowed then
        Stream.WriteBuffer(PWideChar(SA)^, Length(SA));
    end;
    FSaved := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TWideStrings.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendants may optionally implement this method
end;

//------------------------------------------------------------------------------

procedure TWideStrings.SetCommaText(const Value: WideString);
var
  P, P1: PWideChar;
  S: WideString;
begin
  BeginUpdate;
  try
    Clear;
    P := PWideChar(Value);
    while P^ in [WideChar(#1)..WideSpace] do
      Inc(P);
    while P^ <> WideNull do
    begin
      if P^ = '"' then
        S := WideExtractQuotedStr(P, '"')
      else
      begin
        P1 := P;
        while (P^ > WideSpace) and (P^ <> ', ') do
          Inc(P);
        SetString(S, P1, P - P1);
      end;
      Add(S);

      while P^ in [WideChar(#1)..WideSpace] do
        Inc(P);
      if P^ = ', ' then
      begin
        repeat
          Inc(P);
        until not (P^ in [WideChar(#1)..WideSpace]);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

//------------------------------------------------------------------------------

procedure TWideStrings.SetText(const Value: WideString);
var
  Head,
  Tail: PWideChar;
  S: WideString;
begin
  BeginUpdate;
  try
    Clear;
    Head := PWideChar(Value);
    while Head^ <> WideNull do
    begin
      Tail := Head;
      while not (Tail^ in [WideNull, WideLineFeed, WideCarriageReturn, WideVerticalTab, WideFormFeed]) and
            (Tail^ <> WideLineSeparator) and
            (Tail^ <> WideParagraphSeparator) do
        Inc(Tail);
      SetString(S, Head, Tail - Head);
      Add(S);
      Head := Tail;
      if Head^ <> WideNull then
      begin
        Inc(Head);
        if (Tail^ = WideCarriageReturn) and (Head^ = WideLineFeed) then
          Inc(Head);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

//------------------------------------------------------------------------------

procedure TWideStrings.SetUpdateState(Updating: Boolean);
begin
end;

//------------------------------------------------------------------------------

procedure TWideStrings.SetValue(const Name, Value: WideString);
var
  I : Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then
      I := Add('');
    Put(I, Name + '=' + Value);
  end
  else
  begin
    if I >= 0 then
      Delete(I);
  end;
end;

//------------------------------------------------------------------------------

procedure TWideStrings.WriteData(Writer: TWriter);
var
  Len: Integer;
  W: WideString;
  Value: TValueType;
begin
  Value := vaWString;
  W := GetText;
  Len := Length(W);
  Writer.Write(Value, SizeOf(Value));
  Writer.Write(Len, SizeOf(Len));
  Writer.Write(PWideChar(W)^, 2 * Len);
end;

//==============================================================================
// TWideStringList
//==============================================================================

destructor TWideStringList.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  FCount := 0;
  FList := nil;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TWideStringList.Add(const S: WideString): Integer;
begin
  if not Sorted then
    Result := FCount
  else
  begin
    if Find(S, Result) then
    begin
      case Duplicates of
        dupIgnore:
          Exit;
        dupError:
          Error(SDuplicateString, 0);
      end;
    end;
  end;
  InsertItem(Result, S);
end;

//------------------------------------------------------------------------------

procedure TWideStringList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TWideStringList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

//------------------------------------------------------------------------------

procedure TWideStringList.Clear;
begin
  if FCount <> 0 then
  begin
    Changing;
    // this will automatically finalize the array
    FList := nil;
    FCount := 0;
    SetCapacity(0);
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWideStringList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  Changing;
  FList[Index].FString := '';
  Dec(FCount);
  if Index < FCount then
    System.Move(FList[Index + 1], FList[Index], (FCount - Index) * SizeOf(TWideStringItem));
  Changed;
end;

//------------------------------------------------------------------------------

procedure TWideStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TWideStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: TWideStringItem;
begin
  Temp := FList[Index1];
  FList[Index1] := FList[Index2];
  FList[Index2] := Temp;
end;

//------------------------------------------------------------------------------

function TWideStringList.Find(const S: WideString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := WideCompareText(FList[I].FString, S, FLanguage);
    if C < 0 then
      L := I+1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then
          L := I;
      end;
    end;
  end;
  Index := L;
end;

//------------------------------------------------------------------------------

function TWideStringList.Get(Index: Integer): WideString;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  Result := FList[Index].FString;
end;

//------------------------------------------------------------------------------

function TWideStringList.GetCapacity: Integer;
begin
  Result := Length(FList);
end;

//------------------------------------------------------------------------------

function TWideStringList.GetCount: Integer;
begin
  Result := FCount;
end;

//------------------------------------------------------------------------------

function TWideStringList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  Result := FList[Index].FObject;
end;

//------------------------------------------------------------------------------

procedure TWideStringList.Grow;
var
  Delta,
  Len: Integer;
begin
  Len := Length(FList);
  if Len > 64 then
    Delta := Len div 4
  else
  begin
    if Len > 8 then
      Delta := 16
    else
      Delta := 4;
  end;
  SetCapacity(Len + Delta);
end;

//------------------------------------------------------------------------------

function TWideStringList.IndexOf(const S: WideString): Integer;
begin
  if not Sorted then
    Result := inherited IndexOf(S)
  else
  if not Find(S, Result) then
    Result := -1;
end;

//------------------------------------------------------------------------------

procedure TWideStringList.Insert(Index: Integer; const S: WideString);
begin
  if Sorted then
    Error(SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then
    Error(SListIndexError, Index);
  InsertItem(Index, S);
end;

//------------------------------------------------------------------------------

procedure TWideStringList.InsertItem(Index: Integer; const S: WideString);
begin
  Changing;
  if FCount = Length(FList) then
    Grow;
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1], (FCount - Index) * SizeOf(TWideStringItem));
  with FList[Index] do
  begin
    Pointer(FString) := nil;
    FObject := nil;
    FString := S;
  end;
  Inc(FCount);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TWideStringList.Put(Index: Integer; const S: WideString);
begin
  if Sorted then
    Error(SSortedListError, 0);
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  Changing;
  FList[Index].FString := S;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TWideStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  Changing;
  FList[Index].FObject := AObject;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TWideStringList.QuickSort(L, R: Integer);
var
  I, J: Integer;
  P: WideString;
begin
  repeat
    I := L;
    J := R;
    P := FList[(L + R) shr 1].FString;
    repeat
      while WideCompareText(FList[I].FString, P, FLanguage) < 0 do
        Inc(I);
      while WideCompareText(FList[J].FString, P, FLanguage) > 0 do
        Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J);
    L := I;
  until I >= R;
end;

//------------------------------------------------------------------------------

procedure TWideStringList.SetCapacity(NewCapacity: Integer);
begin
  SetLength(FList, NewCapacity);
  if NewCapacity < FCount then
    FCount := NewCapacity;
end;

//------------------------------------------------------------------------------

procedure TWideStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then
      Sort;
    FSorted := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TWideStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then
    Changing
  else
    Changed;
end;

//------------------------------------------------------------------------------

procedure TWideStringList.Sort;
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1);
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWideStringList.SetLanguage(Value: LCID);
begin
  inherited;
  if Sorted then
    Sort;
end;

//==============================================================================
// Functions for null terminated wide strings
//==============================================================================

function StrLenW(Str: PWideChar): Cardinal;
// returns number of characters in a string excluding the null terminator
asm
        MOV     EDX, EDI
        MOV     EDI, EAX
        MOV     ECX, 0FFFFFFFFH
        XOR     AX, AX
        REPNE   SCASW
        MOV     EAX, 0FFFFFFFEH
        SUB     EAX, ECX
        MOV     EDI, EDX
end;

//------------------------------------------------------------------------------

function StrEndW(Str: PWideChar): PWideChar;
// returns a pointer to the end of a null terminated string
asm
        MOV     EDX, EDI
        MOV     EDI, EAX
        MOV     ECX, 0FFFFFFFFH
        XOR     AX, AX
        REPNE   SCASW
        LEA     EAX, [EDI - 2]
        MOV     EDI, EDX
end;

//------------------------------------------------------------------------------

function StrMoveW(Dest, Source: PWideChar; Count: Cardinal): PWideChar;
// Copies the specified number of characters to the destination string and returns Dest
// also as result. Dest must have enough room to store at least Count characters.
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI, EDX
        MOV     EDI, EAX
        MOV     EDX, ECX
        CMP     EDI, ESI
        JG      @@1
        JE      @@2
        SHR     ECX, 1
        REP     MOVSD
        MOV     ECX, EDX
        AND     ECX, 1
        REP     MOVSW
        JMP     @@2
@@1:
        LEA     ESI, [ESI + 2 * ECX - 2]
        LEA     EDI, [EDI + 2 * ECX - 2]
        STD
        AND     ECX, 1
        REP     MOVSW
        SUB     EDI, 2
        SUB     ESI, 2
        MOV     ECX, EDX
        SHR     ECX, 1
        REP     MOVSD
        CLD
@@2:
        POP     EDI
        POP     ESI
end;

//------------------------------------------------------------------------------

function StrCopyW(Dest, Source: PWideChar): PWideChar;
// copies Source to Dest and returns Dest
asm
        PUSH    EDI
        PUSH    ESI
        MOV     ESI, EAX
        MOV     EDI, EDX
        MOV     ECX, 0FFFFFFFFH
        XOR     AX, AX
        REPNE   SCASW
        NOT     ECX
        MOV     EDI, ESI
        MOV     ESI, EDX
        MOV     EDX, ECX
        MOV     EAX, EDI
        SHR     ECX, 1
        REP     MOVSD
        MOV     ECX, EDX
        AND     ECX, 1
        REP     MOVSW
        POP     ESI
        POP     EDI
end;

//------------------------------------------------------------------------------

function StrECopyW(Dest, Source: PWideChar): PWideChar;
// copies Source to Dest and returns a pointer to the null character ending the string
asm
        PUSH    EDI
        PUSH    ESI
        MOV     ESI, EAX
        MOV     EDI, EDX
        MOV     ECX, 0FFFFFFFFH
        XOR     AX, AX
        REPNE   SCASW
        NOT     ECX
        MOV     EDI, ESI
        MOV     ESI, EDX
        MOV     EDX, ECX
        SHR     ECX, 1
        REP     MOVSD
        MOV     ECX, EDX
        AND     ECX, 1
        REP     MOVSW
        LEA     EAX, [EDI - 2]
        POP     ESI
        POP     EDI
end;

//------------------------------------------------------------------------------

function StrLCopyW(Dest, Source: PWideChar; MaxLen: Cardinal): PWideChar;
// copies a specified maximum number of characters from Source to Dest
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     ESI, EAX
        MOV     EDI, EDX
        MOV     EBX, ECX
        XOR     AX, AX
        TEST    ECX, ECX
        JZ      @@1
        REPNE   SCASW
        JNE     @@1
        INC     ECX
@@1:
        SUB     EBX, ECX
        MOV     EDI, ESI
        MOV     ESI, EDX
        MOV     EDX, EDI
        MOV     ECX, EBX
        SHR     ECX, 1
        REP     MOVSD
        MOV     ECX, EBX
        AND     ECX, 1
        REP     MOVSW
        STOSW
        MOV     EAX, EDX
        POP     EBX
        POP     ESI
        POP     EDI
end;

//------------------------------------------------------------------------------

function StrPCopyW(Dest: PWideChar; const Source: string): PWideChar;
// copies a Pascal-style string to a null-terminated wide string
begin
  Result := StrPLCopyW(Dest, Source, Length(Source));
  Result[Length(Source)] := WideNull;
end;

//------------------------------------------------------------------------------

function StrPLCopyW(Dest: PWideChar; const Source: string; MaxLen: Cardinal): PWideChar;
// copies characters from a Pascal-style string into a null-terminated wide string
asm
        PUSH    EDI
        PUSH    ESI
        MOV     EDI, EAX
        MOV     ESI, EDX
        MOV     EDX, EAX
        XOR     AX, AX
@@1:
        LODSB
        STOSW
        DEC     ECX
        JNZ     @@1
        MOV     EAX, EDX
        POP     ESI
        POP     EDI
end;

//------------------------------------------------------------------------------

function StrCatW(Dest, Source: PWideChar): PWideChar;
// appends a copy of Source to the end of Dest and returns the concatenated string
begin
  StrCopyW(StrEndW(Dest), Source);
  Result := Dest;
end;

//------------------------------------------------------------------------------

function StrLCatW(Dest, Source: PWideChar; MaxLen: Cardinal): PWideChar;
// appends a specified maximum number of WideCharacters to string
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     EDI, Dest
        MOV     ESI, Source
        MOV     EBX, MaxLen
        SHL     EBX, 1
        CALL    StrEndW
        MOV     ECX, EDI
        ADD     ECX, EBX
        SUB     ECX, EAX
        JBE     @@1
        MOV     EDX, ESI
        SHR     ECX, 1
        CALL    StrLCopyW
@@1:
        MOV     EAX, EDI
        POP     EBX
        POP     ESI
        POP     EDI
end;

//------------------------------------------------------------------------------

function StrCompW(Str1, Str2: PWideChar): Integer;
// compares Str1 to Str2 (binary comparation)
// Note: There's also an extended comparation function which uses a given language
//       to compare unicode strings.
asm
        PUSH    EDI
        PUSH    ESI
        MOV     EDI, EDX
        MOV     ESI, EAX
        MOV     ECX, 0FFFFFFFFH
        XOR     EAX, EAX
        REPNE   SCASW
        NOT     ECX
        MOV     EDI, EDX
        XOR     EDX, EDX
        REPE    CMPSW
        MOV     AX, [ESI - 2]
        MOV     DX, [EDI - 2]
        SUB     EAX, EDX
        POP     ESI
        POP     EDI
end;

//------------------------------------------------------------------------------

function StrICompW(Str1, Str2: PWideChar): Integer;
// compares Str1 to Str2 without case sensitivity (binary comparation),
// Note: only ANSI characters are compared case insensitively
asm
        PUSH    EDI
        PUSH    ESI
        MOV     EDI, EDX
        MOV     ESI, EAX
        MOV     ECX, 0FFFFFFFFH
        XOR     EAX, EAX
        REPNE   SCASW
        NOT     ECX
        MOV     EDI, EDX
        XOR     EDX, EDX
@@1:
        REPE    CMPSW
        JE      @@4
        MOV     AX, [ESI - 2]
        CMP     AX, 'a'
        JB      @@2
        CMP     AX, 'z'
        JA      @@2
        SUB     AL, 20H
@@2:
        MOV     DX, [EDI - 2]
        CMP     DX, 'a'
        JB      @@3
        CMP     DX, 'z'
        JA      @@3
        SUB     DX, 20H
@@3:
        SUB     EAX, EDX
        JE      @@1
@@4:
        POP     ESI
        POP     EDI
end;

//------------------------------------------------------------------------------

function StrLCompW(Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
// compares a specified maximum number of charaters in two strings
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     EDI, EDX
        MOV     ESI, EAX
        MOV     EBX, ECX
        XOR     EAX, EAX
        OR      ECX, ECX
        JE      @@1
        REPNE   SCASW
        SUB     EBX, ECX
        MOV     ECX, EBX
        MOV     EDI, EDX
        XOR     EDX, EDX
        REPE    CMPSW
        MOV     AX, [ESI - 2]
        MOV     DX, [EDI - 2]
        SUB     EAX, EDX
@@1:
        POP     EBX
        POP     ESI
        POP     EDI
end;

//------------------------------------------------------------------------------

function StrLICompW(Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
// compares strings up to a specified maximum number of characters, not case sensitive
// Note: only ANSI characters are compared case insensitively
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     EDI, EDX
        MOV     ESI, EAX
        MOV     EBX, ECX
        XOR     EAX, EAX
        OR      ECX, ECX
        JE      @@4
        REPNE   SCASW
        SUB     EBX, ECX
        MOV     ECX, EBX
        MOV     EDI, EDX
        XOR     EDX, EDX
@@1:
        REPE    CMPSW
        JE      @@4
        MOV     AX, [ESI - 2]
        CMP     AX, 'a'
        JB      @@2
        CMP     AX, 'z'
        JA      @@2
        SUB     AX, 20H
@@2:
        MOV     DX, [EDI - 2]
        CMP     DX, 'a'
        JB      @@3
        CMP     DX, 'z'
        JA      @@3
        SUB     DX, 20H
@@3:
        SUB     EAX, EDX
        JE      @@1
@@4:
        POP     EBX
        POP     ESI
        POP     EDI
end;

//------------------------------------------------------------------------------

function StrNScanW(S1, S2: PWideChar): Integer;
// determines where (in S1) the first time one of the characters of S2 appear.
// The result is the length of a string part of S1 where none of the characters of
// S2 do appear (not counting the trailing #0 and starting with position 0 in S1).
var
  Run: PWideChar;
begin
  Result := -1;
  if (S1 <> nil) and (S2 <> nil) then
  begin
    Run := S1;
    while (Run^ <> #0) do
    begin
      if StrScanW(S2, Run^) <> nil then
        Break;
      Inc(Run);
    end;
    Result := Run - S1;
  end;
end;

//------------------------------------------------------------------------------

function StrRNScanW(S1, S2: PWideChar): Integer;
// This function does the same as StrRNScanW but uses S1 in reverse order. This
// means S1 points to the last character of a string, is traversed reversely
// and terminates with a starting #0. This is useful for parsing strings stored
// in reversed macro buffers etc.
var
  Run: PWideChar;
begin
  Result := -1;
  if (S1 <> nil) and (S2 <> nil) then
  begin
    Run := S1;
    while (Run^ <> #0) do
    begin
      if StrScanW(S2, Run^) <> nil then
        Break;
      Dec(Run);
    end;
    Result := S1 - Run;
  end;
end;

//------------------------------------------------------------------------------

function StrScanW(Str: PWideChar; Chr: WideChar): PWideChar;
// returns a pointer to first occurrence of a specified character in a string
asm
        PUSH    EDI
        PUSH    EAX
        MOV     EDI, Str
        MOV     ECX, 0FFFFFFFFH
        XOR     AX, AX
        REPNE   SCASW
        NOT     ECX
        POP     EDI
        MOV     AX, Chr
        REPNE   SCASW
        MOV     EAX, 0
        JNE     @@1
        MOV     EAX, EDI
        SUB     EAX, 2
@@1:
        POP     EDI
end;

//------------------------------------------------------------------------------

function StrScanW(Str: PWideChar; Chr: WideChar; StrLen: Cardinal): PWideChar;
// returns a pointer to first occurrence of a specified character in a string
// or nil if not found
// Note: this is just a binary search for the specified character and there's no
//       check for a terminating null. Instead at most StrLen characters are
//       searched. This makes this function extremly fast.
//
// on enter EAX contains Str, EDX contains Chr and ECX StrLen
// on exit EAX contains result pointer or nil
asm
        TEST EAX, EAX
        JZ @@Exit                 // get out if the string is nil or StrLen is 0
        JCXZ @@Exit
@@Loop:
        CMP [EAX], DX             // this unrolled loop is actually faster on modern processors
        JE @@Exit                 // than REP SCASW
        ADD EAX, 2
        DEC ECX
        JNZ @@Loop
        XOR EAX, EAX
@@Exit:
end;

//------------------------------------------------------------------------------

function StrRScanW(Str: PWideChar; Chr: WideChar): PWideChar;
// returns a pointer to the last occurance of Chr in Str
asm
        PUSH    EDI
        MOV     EDI, Str
        MOV     ECX, 0FFFFFFFFH
        XOR     AX, AX
        REPNE   SCASW
        NOT     ECX
        STD
        SUB     EDI, 2
        MOV     AX, Chr
        REPNE   SCASW
        MOV     EAX, 0
        JNE     @@1
        MOV     EAX, EDI
        ADD     EAX, 2
@@1:
        CLD
        POP     EDI
end;

//------------------------------------------------------------------------------

function StrPosW(Str, SubStr: PWideChar): PWideChar;
// returns a pointer to the first occurance of SubStr in Str
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        OR      EAX, EAX
        JZ      @@2
        OR      EDX, EDX
        JZ      @@2
        MOV     EBX, EAX
        MOV     EDI, EDX
        XOR     AX, AX
        MOV     ECX, 0FFFFFFFFH
        REPNE   SCASW
        NOT     ECX
        DEC     ECX
        JZ      @@2
        MOV     ESI, ECX
        MOV     EDI, EBX
        MOV     ECX, 0FFFFFFFFH
        REPNE   SCASW
        NOT     ECX
        SUB     ECX, ESI
        JBE     @@2
        MOV     EDI, EBX
        LEA     EBX, [ESI - 1] // Note: 2 would be wrong here, we are dealing
                               // with numbers not an address
@@1:
        MOV     ESI, EDX
        LODSW
        REPNE   SCASW
        JNE     @@2
        MOV     EAX, ECX
        PUSH    EDI
        MOV     ECX, EBX
        REPE    CMPSW
        POP     EDI
        MOV     ECX, EAX
        JNE     @@1
        LEA     EAX, [EDI - 2]
        JMP     @@3
@@2:
        XOR     EAX, EAX
@@3:
        POP     EBX
        POP     ESI
        POP     EDI
end;

//------------------------------------------------------------------------------

function StrUpperW(Str: PWideChar): PWideChar;
// converts Str to upper case and returns it
begin
  Result := Str;
  while Str^ <> WideNull do
  begin
    Str^ := WideChar(UnicodeToUpper(Word(Str^)));
    Inc(Str);
  end;
end;

//------------------------------------------------------------------------------

function StrLowerW(Str: PWideChar): PWideChar;
// converts Str to lower case and returns it
begin
  Result := Str;
  while Str^ <> WideNull do
  begin
    Str^ := WideChar(UnicodeToLower(Word(Str^)));
    Inc(Str);
  end;
end;

//------------------------------------------------------------------------------

function StrTitleW(Str: PWideChar): PWideChar;
// converts Str to title case and returns it
begin
  Result := Str;
  while Str^ <> WideNull do
  begin
    Str^ := WideChar(UnicodeToTitle(Word(Str^)));
    Inc(Str);
  end;
end;

//------------------------------------------------------------------------------

function StrAllocW(Size: Cardinal): PWideChar;
// Allocates a buffer for a null-terminated wide string and returns a pointer
// to the first character of the string.
begin
  Size := SizeOf(WideChar) * Size + SizeOf(Cardinal);
  GetMem(Result, Size);
  FillChar(Result^, Size, 0);
  Cardinal(Pointer(Result)^) := Size;
  Inc(Result, SizeOf(Cardinal) div SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

function StrBufSizeW(Str: PWideChar): Cardinal;
// Returns max number of wide characters that can be stored in a buffer
// allocated by StrAllocW.
begin
  Dec(Str, SizeOf(Cardinal) div SizeOf(WideChar));
  Result := (Cardinal(Pointer(Str)^) - SizeOf(Cardinal)) div 2;
end;

//------------------------------------------------------------------------------

function StrNewW(Str: PWideChar): PWideChar;
// Duplicates the given string (if not nil) and returns the address of the new string.
var
  Size: Cardinal;
begin
  if Str = nil then
    Result := nil
  else
  begin
    Size := StrLenW(Str) + 1;
    Result := StrMoveW(StrAllocW(Size), Str, Size);
  end;
end;

//------------------------------------------------------------------------------

procedure StrDisposeW(Str: PWideChar);
// releases a string allocated with StrNew.
begin
  if Str <> nil then
  begin
    Dec(Str, SizeOf(Cardinal) div SizeOf(WideChar));
    FreeMem(Str, Cardinal(Pointer(Str)^));
  end;
end;

//------------------------------------------------------------------------------

procedure StrSwapByteOrder(Str: PWideChar);
// exchanges in each character of the given string the low order and high order
// byte to go from LSB to MSB and vice versa.
// EAX contains address of string
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI, EAX
        MOV     EDI, ESI
        XOR     EAX, EAX  // clear high order byte to be able to use 32bit operand below
@@1:
        LODSW
        OR      EAX, EAX
        JZ      @@2
        XCHG    AL, AH
        STOSW
        JMP     @@1
@@2:
        POP     EDI
        POP     ESI
end;

//------------------------------------------------------------------------------

function WideAdjustLineBreaks(const S: WideString): WideString;
var
  Source,
  SourceEnd,
  Dest: PWideChar;
  Extra: Integer;
begin
  Source := Pointer(S);
  SourceEnd := Source + Length(S);
  Extra := 0;
  while Source < SourceEnd do
  begin
    case Source^ of
      WideLF:
        Inc(Extra);
      WideCR:
        if Source[1] = WideLineFeed then
          Inc(Source)
        else
          Inc(Extra);
    end;
    Inc(Source);
  end;

  Source := Pointer(S);
  SetString(Result, nil, SourceEnd - Source + Extra);
  Dest := Pointer(Result);
  while Source < SourceEnd do
  begin
    case Source^ of
      WideLineFeed:
        begin
          Dest^ := WideLineSeparator;
          Inc(Dest);
          Inc(Source);
        end;
      WideCarriageReturn:
        begin
          Dest^ := WideLineSeparator;
          Inc(Dest);
          Inc(Source);
          if Source^ = WideLineFeed then
            Inc(Source);
        end;
    else
      Dest^ := Source^;
      Inc(Dest);
      Inc(Source);
    end;
  end;
end;

//------------------------------------------------------------------------------

function WideQuotedStr(const S: WideString; Quote: WideChar): WideString;
// works like QuotedStr from SysUtils.pas but can insert any quotation character
var
  P, Src,
  Dest: PWideChar;
  AddCount: Integer;
begin
  AddCount := 0;
  P := StrScanW(PWideChar(S), Quote);
  while (P <> nil) do
  begin
    Inc(P);
    Inc(AddCount);
    P := StrScanW(P, Quote);
  end;

  if AddCount = 0 then
    Result := Quote + S + Quote
  else
  begin
    SetLength(Result, Length(S) + AddCount + 2);
    Dest := PWideChar(Result);
    Dest^ := Quote;
    Inc(Dest);
    Src := PWideChar(S);
    P := StrScanW(Src, Quote);
    repeat
      Inc(P);
      Move(Src^, Dest^, P - Src);
      Inc(Dest, P - Src);
      Dest^ := Quote;
      Inc(Dest);
      Src := P;
      P := StrScanW(Src, Quote);
    until P = nil;
    P := StrEndW(Src);
    Move(Src^, Dest^, P - Src);
    Inc(Dest, P - Src);
    Dest^ := Quote;
  end;
end;

//------------------------------------------------------------------------------

function WideExtractQuotedStr(var Src: PWideChar; Quote: WideChar): WideString;
// extracts a string enclosed in quote characters given by Quote
var
  P, Dest: PWideChar;
  DropCount: Integer;
begin
  Result := '';
  if (Src = nil) or (Src^ <> Quote) then
    Exit;

  Inc(Src);
  DropCount := 1;
  P := Src;
  Src := StrScanW(Src, Quote);

  while Src <> nil do   // count adjacent pairs of quote chars
  begin
    Inc(Src);
    if Src^ <> Quote then
      Break;
    Inc(Src);
    Inc(DropCount);
    Src := StrScanW(Src, Quote);
  end;

  if Src = nil then
    Src := StrEndW(P);
  if (Src - P) <= 1 then
    Exit;

  if DropCount = 1 then
    SetString(Result, P, Src - P - 1)
  else
  begin
    SetLength(Result, Src - P - DropCount);
    Dest := PWideChar(Result);
    Src := StrScanW(P, Quote);
    while Src <> nil do
    begin
      Inc(Src);
      if Src^ <> Quote then
        Break;
      Move(P^, Dest^, Src - P);
      Inc(Dest, Src - P);
      Inc(Src);
      P := Src;
      Src := StrScanW(Src, Quote);
    end;
    if Src = nil then
      Src := StrEndW(P);
    Move(P^, Dest^, Src - P - 1);
  end;
end;

//------------------------------------------------------------------------------

function WideStringOfChar(C: WideChar; Count: Cardinal): WideString;
// returns a string of Count characters filled with C
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 1 to Count do
    Result[I] := C;
end;

//------------------------------------------------------------------------------

function WideTrim(const S: WideString): WideString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and
        (UnicodeIsWhiteSpace(Word(S[I])) or UnicodeIsControl(Word(S[I]))) do
    Inc(I);
  if I > L then
    Result := ''
  else
  begin
    while UnicodeIsWhiteSpace(Word(S[L])) or UnicodeIsControl(Word(S[L])) do
      Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

//------------------------------------------------------------------------------

function WideTrimLeft(const S: WideString): WideString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and
        (UnicodeIsWhiteSpace(Word(S[I])) or UnicodeIsControl(Word(S[I]))) do
    Inc(I);
  Result := Copy(S, I, Maxint);
end;

//------------------------------------------------------------------------------

function WideTrimRight(const S: WideString): WideString;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and
        (UnicodeIsWhiteSpace(Word(S[I])) or UnicodeIsControl(Word(S[I]))) do
    Dec(I);
  Result := Copy(S, 1, I);
end;

//------------------------------------------------------------------------------

function WideCharPos(const S: WideString; const Ch: WideChar; const Index: Integer): Integer;
// returns the index of character Ch in S, starts searching at index Index
// Note: This is a quick memory search. No attempt is made to interpret either
// the given charcter nor the string (ligatures, modifiers, surrogates etc.)
asm
              TEST    EAX,EAX           // make sure we are not null
              JZ      @StrIsNil
              DEC     ECX               // make index zero based
              JL      @IdxIsSmall
              PUSH    EBX
              PUSH    EDI
              MOV     EDI,EAX           // EDI := S
              XOR     EAX,EAX
              MOV     AX, DX            // AX := Ch
              MOV     EDX,[EDI-4]       // EDX := Length(S) * 2
              SHR     EDX,1             // EDX := EDX div 2
              MOV     EBX,EDX           // save the length to calc. result
              SUB     EDX,ECX           // EDX = EDX - Index = # of chars to scan
              JLE     @IdxIsBig
              SHL ECX, 1                // two bytes per char
              ADD     EDI,ECX           // point to index'th char
              MOV     ECX,EDX           // loop counter
              REPNE   SCASW
              JNE     @NoMatch
              MOV     EAX,EBX           // result := saved length -
              SUB     EAX,ECX           // loop counter value
              POP     EDI
              POP     EBX
              RET

@IdxIsBig:
@NoMatch:
              XOR     EAX,EAX
              POP     EDI
              POP     EBX
              RET

@IdxIsSmall:
              XOR     EAX,EAX
@StrIsNil:
end;

//------------------------------------------------------------------------------

function WideCompose(const S: WideString): WideString;
// returns a string with all characters of S but if there is a possibility to
// combine characters then they are composed
var
  I: Integer;
begin
  for I := 1 to Length(S) do
  begin
    //UnicodeCompose TODO implementation
  end;
end;

//------------------------------------------------------------------------------

function WideComposeHangul(Source: WideString): WideString;
var
  Len: Integer;
  Ch, Last: WideChar;
  I, J: Integer;
  LINdex, VIndex, SIndex, TIndex: Integer;
begin
  // copy first char
  Len := Length(Source);
  if Len > 0 then
  begin
    // allocate memory only once and shorten the result when done
    SetLength(Result, Len);
    J := 1;
    Last := Source[J];
    Result := Last;

    for I := 2 to Len do
    begin
      Ch := Source[I];

      // 1. check to see if two current characters are L and V
      LIndex := Word(Last) - LBase;
      if (0 <= LIndex) and (LIndex < LCount) then
      begin
        VIndex := Word(Ch) - VBase;
        if (0 <= VIndex) and (VIndex < VCount) then
        begin
          // make syllable of form LV
          Last := WideChar((SBase + (LIndex * VCount + VIndex) * TCount));
          Result[J] := Last; // reset last
          Continue; // discard Ch
        end;
      end;

      // 2. check to see if two current characters are LV and T
      SIndex := Word(Last) - SBase;
      if (0 <= SIndex) and (SIndex < SCount) and ((SIndex mod TCount) = 0) then
      begin
        TIndex := Word(Ch) - TBase;
        if (0 <= TIndex) and (TIndex <= TCount) then
        begin
          // make syllable of form LVT
          Inc(Word(Last), TIndex);
          Result[J] := Last; // reset last
          Continue; // discard ch
        end;
      end;

      // if neither case was true, just add the character
      Last := Ch;
      Inc(J);
      Result[J] := Ch;
    end;
    // shorten the result to real length
    SetLength(Result, J);
  end
  else
    Result := '';
end;

//------------------------------------------------------------------------------

function WideDecompose(const S: WideString): WideString;
// returns a string with all characters of S but decomposed, e.g. Ê is returned
// as E^ etc.
var
  I, J, K: Integer;
  CClass: Cardinal;
  Decomp: TCardinalArray;
begin
  Result := '';
  Decomp := nil;
  for I := 1 to Length(S) do
  begin
    // no need to dive iteratively into decompositions as this is already done
    // on creation of the data used to lookup the decomposition
    Decomp := UnicodeDecompose(Word(S[I]));
    // We need to sort the returned values according to their canonical class.
    for J := 0 to High(Decomp) do
    begin
      CClass := UnicodeCanonicalClass(Decomp[J]);
      if CClass = 0 then
        Result := Result + WideChar(Decomp[J])
      else
      begin
        K := Length(Result);
        // bubble-sort combining marks as necessary
        while K > 1 do
        begin
          if UnicodeCanonicalClass(Word(Result[K])) <= CClass then
            Break;
          Dec(K);
        end;
        Insert(WideChar(Decomp[J]), Result, K + 1);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function WideLoCase(C: WideChar): WideChar;
begin
  Result := WideChar(UnicodeToLower(Word(C)));
end;

//------------------------------------------------------------------------------

function WideLowerCase(const S: WideString): WideString;
var
  I: Integer;
begin
  Result := S;
  for I := 1 to Length(S) do
    Result[I] := WideChar(UnicodeToLower(Word(Result[I])));
end;

//------------------------------------------------------------------------------

function WideTitleCaseChar(C: WideChar): WideChar;
begin
  Result := WideChar(UnicodeToTitle(Word(C)));
end;

//------------------------------------------------------------------------------

function WideTitleCaseString(const S: WideString): WideString;
var
  I: Integer;
begin
  Result := S;
  for I := 1 to Length(S) do
    Result[I] := WideChar(UnicodeToTitle(Word(Result[I])));
end;

//------------------------------------------------------------------------------

function WideUpCase(C: WideChar): WideChar;
begin
  Result := WideChar(UnicodeToUpper(Word(C)));
end;

//------------------------------------------------------------------------------

function WideUpperCase(const S: WideString): WideString;
var
  I: Integer;
begin
  Result := S;
  for I := 1 to Length(S) do
    Result[I] := WideChar(UnicodeToUpper(Word(Result[I])));
end;

//==============================================================================
// Character test routines
//==============================================================================

// Is the character alphabetic?
function UnicodeIsAlpha(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_LU or UC_LL or UC_LM or UC_LO or UC_LT, 0);
end;

//------------------------------------------------------------------------------

// Is the character a digit?
function UnicodeIsDigit(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_ND, 0);
end;

//------------------------------------------------------------------------------

// Is the character alphabetic or a number?
function UnicodeIsAlphaNum(C: UCS4): Boolean;
begin
Result := IsProperty(C, UC_LU or UC_LL or UC_LM or UC_LO or UC_LT or UC_ND, 0);
end;

//------------------------------------------------------------------------------

// Is the character a control character?
function UnicodeIsControl(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_CC or UC_CF, 0);
end;

//------------------------------------------------------------------------------

// Is the character a spacing character?
function UnicodeIsSpace(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_ZS or UC_SS, 0);
end;

//------------------------------------------------------------------------------

// Is the character a white space character (same as UnicodeIsSpace plus
// tabulator, new line etc.)?
function UnicodeIsWhiteSpace(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_ZS or UC_SS, UC_WS or UC_S);
end;

//------------------------------------------------------------------------------

// Is the character a space separator?
function UnicodeIsBlank(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_ZS, 0);
end;

//------------------------------------------------------------------------------

// Is the character a punctuation mark?
function UnicodeIsPunctuation(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_PD or UC_PS or UC_PE or UC_PO, UC_PI or UC_PF);
end;

//------------------------------------------------------------------------------

// Is the character graphical?
function UnicodeIsGraph(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_MN or UC_MC or UC_ME or UC_ND or UC_NL or UC_NO or
                       UC_LU or UC_LL or UC_LT or UC_LM or UC_LO or UC_PC or UC_PD or
                       UC_PS or UC_PE or UC_PO or UC_SM or UC_SM or UC_SC or UC_SK or
                       UC_SO, UC_PI or UC_PF);
end;

//------------------------------------------------------------------------------

// Is the character printable?
function UnicodeIsPrintable(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_MN or UC_MC or UC_ME or UC_ND or UC_NL or UC_NO or
                       UC_LU or UC_LL or UC_LT or UC_LM or UC_LO or UC_PC or UC_PD or
                       UC_PS or UC_PE or UC_PO or UC_SM or UC_SM or UC_SC or UC_SK or
                       UC_SO or UC_ZS, UC_PI or UC_PF);
end;

//------------------------------------------------------------------------------

// Is the character already upper case?
function UnicodeIsUpper(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_LU, 0);
end;

//------------------------------------------------------------------------------

// Is the character already lower case?
function UnicodeIsLower(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_LL, 0);
end;

//------------------------------------------------------------------------------

// Is the character already title case?
function UnicodeIsTitle(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_LT, 0);
end;

//------------------------------------------------------------------------------

// Is the character a hex digit?
function UnicodeIsHexDigit(C: UCS4): Boolean;
begin
  Result := IsProperty(C, 0, UC_HD);
end;

//------------------------------------------------------------------------------

// Is the character a C0 control character (< 32)?
function UnicodeIsIsoControl(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_CC, 0);
end;

//------------------------------------------------------------------------------

// Is the character a format control character?
function UnicodeIsFormatControl(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_CF, 0);
end;

//------------------------------------------------------------------------------

// Is the character a symbol?
function UnicodeIsSymbol(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_SM or UC_SC or UC_SO or UC_SK, 0);
end;

//------------------------------------------------------------------------------

// Is the character a number or digit?
function UnicodeIsNumber(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_ND or UC_NO or UC_NL, 0);
end;

//------------------------------------------------------------------------------

// Is the character non-spacing?
function UnicodeIsNonSpacing(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_MN, 0);
end;

//------------------------------------------------------------------------------

// Is the character an open/left punctuation (i.e. '[')?
function UnicodeIsOpenPunctuation(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_PS, 0);
end;

//------------------------------------------------------------------------------

// Is the character an close/right punctuation (i.e. ']')?
function UnicodeIsClosePunctuation(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_PE, 0);
end;

//------------------------------------------------------------------------------

// Is the character an initial punctuation (i.e. U+2018 LEFT SINGLE QUOTATION MARK)?
function UnicodeIsInitialPunctuation(C: UCS4): Boolean;
begin
  Result := IsProperty(C, 0, UC_PI);
end;

//------------------------------------------------------------------------------

// Is the character a final punctuation (i.e. U+2019 RIGHT SINGLE QUOTATION MARK)?
function UnicodeIsFinalPunctuation(C: UCS4): Boolean;
begin
  Result := IsProperty(C, 0, UC_PF);
end;

//------------------------------------------------------------------------------

// Can the character be decomposed into a set of other characters?
function UnicodeIsComposite(C: UCS4): Boolean;
begin
  Result := IsProperty(C, 0, UC_CM);
end;

//------------------------------------------------------------------------------

// Is the character one of the many quotation marks?
function UnicodeIsQuotationMark(C: UCS4): Boolean;
begin
  Result := IsProperty(C, 0, UC_QM);
end;

//------------------------------------------------------------------------------

// Is the character one that has an opposite form (i.e. <>)?
function UnicodeIsSymmetric(C: UCS4): Boolean;
begin
  Result := IsProperty(C, 0, UC_SY);
end;

//------------------------------------------------------------------------------

// Is the character mirroring (superset of symmetric)?
function UnicodeIsMirroring(C: UCS4): Boolean;
begin
  Result := IsProperty(C, 0, UC_MR);
end;

//------------------------------------------------------------------------------

// Is the character non-breaking (i.e. non-breaking space)?
function UnicodeIsNonBreaking(C: UCS4): Boolean;
begin
  Result := IsProperty(C, 0, UC_NB);
end;

//==============================================================================
// Directionality functions
//==============================================================================

// Does the character have strong right-to-left directionality (i.e. Arabic letters)?
function UnicodeIsRightToLeft(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_R, 0);
end;

//------------------------------------------------------------------------------

// Does the character have strong left-to-right directionality (i.e. Latin letters)?
function UnicodeIsLeftToRight(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_L, 0);
end;

//------------------------------------------------------------------------------

// Does the character have strong directionality?
function UnicodeIsStrong(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_L or UC_R, 0);
end;

//------------------------------------------------------------------------------

// Does the character have weak directionality (i.e. numbers)?
function UnicodeIsWeak(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_EN or UC_ES, UC_ET or UC_AN or UC_CS);
end;

//------------------------------------------------------------------------------

// Does the character have neutral directionality (i.e. whitespace)?
function UnicodeIsNeutral(C: UCS4): Boolean;
begin
  Result := IsProperty(C, 0, UC_B or UC_S or UC_WS or UC_ON);
end;

//------------------------------------------------------------------------------

// Is the character a block or segment separator?
function UnicodeIsSeparator(C: UCS4): Boolean;
begin
  Result := IsProperty(C, 0, UC_B or UC_S);
end;

//------------------------------------------------------------------------------

// Other functions inspired by John Cowan.
// Is the character a mark of some kind?
function UnicodeIsMark(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_MN or UC_MC or UC_ME, 0);
end;

//------------------------------------------------------------------------------

// Is the character a modifier letter?
function UnicodeIsModifier(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_LM, 0);
end;

//------------------------------------------------------------------------------

// Is the character a number represented by a letter?
function UnicodeIsLetterNumber(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_NL, 0);
end;

//------------------------------------------------------------------------------

// Is the character connecting punctuation?
function UnicodeIsConnectionPunctuation(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_PC, 0);
end;

//------------------------------------------------------------------------------

// Is the character a dash punctuation?
function UnicodeIsDash(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_PD, 0);
end;

//------------------------------------------------------------------------------

// Is the character a math character?
function UnicodeIsMath(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_SM, 0);
end;

//------------------------------------------------------------------------------

// Is the character a currency character?
function UnicodeIsCurrency(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_SC, 0);
end;

//------------------------------------------------------------------------------

// Is the character a modifier symbol?
function UnicodeIsModifierSymbol(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_SK, 0);
end;

//------------------------------------------------------------------------------

// Is the character a non-spacing mark?
function UnicodeIsNonSpacingMark(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_MN, 0);
end;

//------------------------------------------------------------------------------

// Is the character a spacing mark?
function UnicodeIsSpacingMark(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_MC, 0);
end;

//------------------------------------------------------------------------------

// Is the character enclosing (i.e. enclosing box)?
function UnicodeIsEnclosing(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_ME, 0);
end;

//------------------------------------------------------------------------------

// Is the character from the Private Use Area?
function UnicodeIsPrivate(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_CO, 0);
end;

//------------------------------------------------------------------------------

// Is the character one of the surrogate codes?
function UnicodeIsSurrogate(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_OS, 0);
end;

//------------------------------------------------------------------------------

// Is the character a line separator?
function UnicodeIsLineSeparator(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_ZL, 0);
end;

//------------------------------------------------------------------------------

// Is th character a paragraph separator;
function UnicodeIsParagraphSeparator(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_ZP, 0);
end;

//------------------------------------------------------------------------------

// Can the character begin an identifier?
function UnicodeIsIdentifierStart(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_LU or UC_LL or UC_LT or UC_LO or UC_NL, 0);
end;

//------------------------------------------------------------------------------

// Can the character appear in an identifier?
function UnicodeIsIdentifierPart(C: UCS4): Boolean;
begin
  Result := IsProperty(C, UC_LU or UC_LL or UC_LT or UC_LO or UC_NL or UC_MN or
                       UC_MC or UC_ND or UC_PC or UC_CF, 0);
end;

//------------------------------------------------------------------------------

// Is the character defined (appears in one of the data files)?
function UnicodeIsDefined(C: UCS4): Boolean;
begin
  Result := IsProperty(C, 0, UC_CP);
end;

//------------------------------------------------------------------------------

// Is the character not defined (non-Unicode)?
function UnicodeIsUndefined(C: UCS4): Boolean;
begin
  Result := not IsProperty(C, 0, UC_CP);
end;

//==============================================================================
// Other miscellaneous character property functions.
//==============================================================================

// Is the character a Han ideograph?
function UnicodeIsHan(C: UCS4): Boolean;
begin
  Result := ((C >= $4E00) and (C <= $9FFF))  or ((C >= $F900) and (C <= $FAFF));
end;

//------------------------------------------------------------------------------

// Is the character a pre-composed Hangul syllable?
function UnicodeIsHangul(C: UCS4): Boolean;
begin
  Result := (C >= $AC00) and (C <= $D7FF);
end;

//------------------------------------------------------------------------------

function CharSetFromLocale(Language: LCID): TFontCharSet;
var
  CP: Cardinal;
  CSI: TCharsetInfo;
begin
  CP:= CodePageFromLocale(Language);
  TranslateCharsetInfo(CP, CSI, TCI_SRCCODEPAGE);
  Result:= CSI.ciCharset;
end;

//------------------------------------------------------------------------------

function CodePageFromLocale(Language: LCID): Integer;
// determines the code page for a given locale
var
  Buf: array [0..6] of Char;
begin
  GetLocaleInfo(Language, LOCALE_IDefaultAnsiCodePage, Buf, 6);
  Result := StrToIntDef(Buf, GetACP);
end;

//------------------------------------------------------------------------------

function KeyboardCodePage: Word;
begin
  Result := CodePageFromLocale(GetKeyboardLayout(0) and $FFFF);
end;

//------------------------------------------------------------------------------

function KeyUnicode(C: Char): WideChar;
// converts the given character (as it comes with a WM_CHAR message) into its
// corresponding Unicode character depending on the active keyboard layout
begin
  MultiByteToWideChar(KeyboardCodePage, MB_USEGLYPHCHARS, @C, 1, @Result, 1);
end;

//------------------------------------------------------------------------------

function CodeBlockFromChar(const C: WideChar): Cardinal;
// returns the Unicode code block to which C belongs
begin
  case C of
    #$0000..#$007F: // Basic Latin
      Result := 0;
    #$0080..#$00FF: // Latin-1 Supplement
      Result := 1;
    #$0100..#$017F: // Latin Extended-A
      Result := 2;
    #$0180..#$024F: // Latin Extended-B
      Result := 3;
    #$0250..#$02AF: // IPA Extensions
      Result := 4;
    #$02B0..#$02FF: // Spacing Modifier Letters
      Result := 5;
    #$0300..#$036F: // Combining Diacritical Marks
      Result := 6;
    #$0370..#$03FF: // Greek
      Result := 7;
    #$0400..#$04FF: // Cyrillic
      Result := 8;
    #$0530..#$058F: // Armenian
      Result := 9;
    #$0590..#$05FF: // Hebrew
      Result := 10;
    #$0600..#$06FF: // Arabic
      Result := 11;
    #$0900..#$097F: // Devanagari
      Result := 12;
    #$0980..#$09FF: // Bengali
      Result := 13;
    #$0A00..#$0A7F: // Gurmukhi
      Result := 14;
    #$0A80..#$0AFF: // Gujarati
      Result := 15;
    #$0B00..#$0B7F: // Oriya
      Result := 16;
    #$0B80..#$0BFF: // Tamil
      Result := 17;
    #$0C00..#$0C7F: // Telugu
      Result := 18;
    #$0C80..#$0CFF: // Kannada
      Result := 19;
    #$0D00..#$0D7F: // Malayalam
      Result := 20;
    #$0E00..#$0E7F: // Thai
      Result := 21;
    #$0E80..#$0EFF: // Lao
      Result := 22;
    #$0F00..#$0FBF: // Tibetan
      Result := 23;
    #$10A0..#$10FF: // Georgian
      Result := 24;
    #$1100..#$11FF: // Hangul Jamo
      Result := 25;
    #$1E00..#$1EFF: // Latin Extended Additional
      Result := 26;
    #$1F00..#$1FFF: // Greek Extended
      Result := 27;
    #$2000..#$206F: // General Punctuation
      Result := 28;
    #$2070..#$209F: // Superscripts and Subscripts
      Result := 29;
    #$20A0..#$20CF: // Currency Symbols
      Result := 30;
    #$20D0..#$20FF: // Combining Marks for Symbols
      Result := 31;
    #$2100..#$214F: // Letterlike Symbols
      Result := 32;
    #$2150..#$218F: // Number Forms
      Result := 33;
    #$2190..#$21FF: // Arrows
      Result := 34;
    #$2200..#$22FF: // Mathematical Operators
      Result := 35;
    #$2300..#$23FF: // Miscellaneous Technical
      Result := 36;
    #$2400..#$243F: // Control Pictures
      Result := 37;
    #$2440..#$245F: // Optical Character Recognition
      Result := 38;
    #$2460..#$24FF: // Enclosed Alphanumerics
      Result := 39;
    #$2500..#$257F: // Box Drawing
      Result := 40;
    #$2580..#$259F: // Block Elements
      Result := 41;
    #$25A0..#$25FF: // Geometric Shapes
      Result := 42;
    #$2600..#$26FF: // Miscellaneous Symbols
      Result := 43;
    #$2700..#$27BF: // Dingbats
      Result := 44;
    #$3000..#$303F: // CJK Symbols and Punctuation
      Result := 45;
    #$3040..#$309F: // Hiragana
      Result := 46;
    #$30A0..#$30FF: // Katakana
      Result := 47;
    #$3100..#$312F: // Bopomofo
      Result := 48;
    #$3130..#$318F: // Hangul Compatibility Jamo
      Result := 49;
    #$3190..#$319F: // Kanbun
      Result := 50;
    #$3200..#$32FF: // Enclosed CJK Letters and Months
      Result := 51;
    #$3300..#$33FF: // CJK Compatibility
      Result := 52;
    #$4E00..#$9FFF: // CJK Unified Ideographs
      Result := 53;
    #$AC00..#$D7A3: // Hangul Syllables
      Result := 54;
    #$D800..#$DB7F: // High Surrogates
      Result := 55;
    #$DB80..#$DBFF: // High Private Use Surrogates
      Result := 56;
    #$DC00..#$DFFF: // Low Surrogates
      Result := 57;
    #$E000..#$F8FF: // Private Use
      Result := 58;
    #$F900..#$FAFF: // CJK Compatibility Ideographs
      Result := 59;
    #$FB00..#$FB4F: // Alphabetic Presentation Forms
      Result := 60;
    #$FB50..#$FDFF: // Arabic Presentation Forms-A
      Result := 61;
    #$FE20..#$FE2F: // Combining Half Marks
      Result := 62;
    #$FE30..#$FE4F: // CJK Compatibility Forms
      Result := 63;
    #$FE50..#$FE6F: // Small Form Variants
      Result := 64;
    #$FE70..#$FEFF: // Arabic Presentation Forms-B
      Result := 65;
    #$FF00..#$FFEF: // Halfwidth and Fullwidth Forms
      Result := 66;
  else
    // #$FFF0..#$FFFF Specials
    Result := 67;
  end;
end;

//------------------------------------------------------------------------------

function CompareTextWin95(W1, W2: WideString; Locale: LCID): Integer;
// special comparation function for Win9x since there's no system defined
// comparation function, returns -1 if W1 < W2, 0 if W1 = W2 or 1 if W1 > W2
var
  S1, S2: string;
  CP: Integer;
  L1, L2: Integer;
begin
  L1 := Length(W1);
  L2 := Length(W2);
  SetLength(S1, L1);
  SetLength(S2, L2);
  CP := CodePageFromLocale(Locale);
  WideCharToMultiByte(CP, 0, PWideChar(W1), L1, PChar(S1), L1, nil, nil);
  WideCharToMultiByte(CP, 0, PWideChar(W2), L2, PChar(S2), L2, nil, nil);
  Result := CompareStringA(Locale, NORM_IGNORECASE, PChar(S1), Length(S1),
    PChar(S2), Length(S2)) - 2;
end;

//------------------------------------------------------------------------------

function CompareTextWinNT(W1, W2: WideString; Locale: LCID): Integer;
// Wrapper function for WinNT since there's no system defined comparation function
// in Win9x and we need a central comparation function for TWideStringList.
// Returns -1 if W1 < W2, 0 if W1 = W2 or 1 if W1 > W2
begin
  Result := CompareStringW(Locale, NORM_IGNORECASE, PWideChar(W1), Length(W1),
    PWideChar(W2), Length(W2)) - 2;
end;

//------------------------------------------------------------------------------

function StringToWideStringEx(const S: string; CodePage: Word): WideString;
var
  L: Integer;
begin
  L:= MultiByteToWideChar(CodePage, 0, PChar(S), -1, nil, 0);
  SetLength(Result, L - 1);
  MultiByteToWideChar(CodePage, 0, PChar(S), -1, PWideChar(Result), L - 1);
end;

//------------------------------------------------------------------------------

function WideStringToStringEx(const WS: WideString; CodePage: Word): string;
var
  L: Integer;
begin
  L := WideCharToMultiByte(CodePage, 0, PWideChar(WS), -1, nil, 0, nil, nil);
  SetLength(Result, L - 1);
  WideCharToMultiByte(CodePage, 0, PWideChar(WS), -1, PChar(Result), L - 1, nil, nil);
end;

//------------------------------------------------------------------------------

function TranslateString(const S: string; CP1, CP2: Word): string;
begin
  Result:= WideStringToStringEx(StringToWideStringEx(S, CP1), CP2);
end;

//==============================================================================
// Conversion routines
//==============================================================================

const
  halfShift: Integer = 10;

  halfBase: UCS4 = $0010000;
  halfMask: UCS4 = $3FF;

  offsetsFromUTF8: array [0..5] of UCS4 = ($00000000, $00003080, $000E2080,
                                           $03C82080, $FA082080, $82082080);

  bytesFromUTF8: array [0..255] of Byte = (
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5);

  firstByteMark: array [0..6] of Byte = ($00, $00, $C0, $E0, $F0, $F8, $FC);

//------------------------------------------------------------------------------

function WideStringToUTF8(S: WideString): AnsiString;
var
  ch: UCS4;
  L, J, T,
  bytesToWrite: Word;
  byteMask: UCS4;
  byteMark: UCS4;
begin
  if Length(S) = 0 then
  begin
    Result := '';
    Exit;
  end;

  SetLength(Result, Length(S) * 6); // assume worst case
  T := 1;
  for J := 1 to Length(S) do
  begin
    byteMask := $BF;
    byteMark := $80;

    ch := UCS4(S[J]);

    if ch < $80 then
      bytesToWrite := 1
    else
    if ch < $800 then
      bytesToWrite := 2
    else
    if ch < $10000 then
      bytesToWrite := 3
    else
    if ch < $200000 then
      bytesToWrite := 4
    else
    if ch < $4000000 then
      bytesToWrite := 5
    else
    if ch <= MaximumUCS4 then
      bytesToWrite := 6
    else
    begin
      bytesToWrite := 2;
      ch := ReplacementCharacter;
    end;

    for L := bytesToWrite downto 2 do
    begin
      Result[T + L - 1] := Char((ch or byteMark) and byteMask);
      ch := ch shr 6;
    end;
    Result[T] := Char(ch or firstByteMark[bytesToWrite]);
    Inc(T, bytesToWrite);
  end;
  SetLength(Result, T - 1); // assume worst case
end;

//------------------------------------------------------------------------------

function UTF8ToWideString(S: AnsiString): WideString;
var
  L, J, T: Cardinal;
  ch: UCS4;
  extraBytesToWrite: Word;
begin
  if Length(S) = 0 then
  begin
    Result := '';
    Exit;
  end;

  SetLength(Result, Length(S)); // create enough room

  L := 1;
  T := 1;
  while L <= Cardinal(Length(S)) do
  begin
    ch := 0;
    extraBytesToWrite := bytesFromUTF8[Ord(S[L])];

    for J := extraBytesToWrite downto 1 do
    begin
      ch := ch + Ord(S[L]);
      Inc(L);
      ch := ch shl 6;
    end;
    ch := ch + Ord(S[L]);
    Inc(L);
    ch := ch - offsetsFromUTF8[extraBytesToWrite];

    if ch <= MaximumUCS2 then
    begin
      Result[T] := WideChar(ch);
      Inc(T);
    end
    else
    if ch > MaximumUCS4 then
    begin
      Result[T] := WideChar(ReplacementCharacter);
      Inc(T);
    end
    else
    begin
      ch := ch - halfBase;
      Result[T] := WideChar((ch shr halfShift) + SurrogateHighStart);
      Inc(T);
      Result[T] := WideChar((ch and halfMask) + SurrogateLowStart);
      Inc(T);
    end;
  end;
  SetLength(Result, T - 1); // now fix up length
end;

//------------------------------------------------------------------------------

initialization
  if (Win32Platform and VER_PLATFORM_WIN32_NT) <> 0 then
    @WideCompareText := @CompareTextWinNT
  else
    @WideCompareText := @CompareTextWin95;
finalization
  LoadInProgress.Free;
end.
