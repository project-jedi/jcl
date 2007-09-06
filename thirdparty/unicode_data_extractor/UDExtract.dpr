program UDExtract;

{$APPTYPE CONSOLE}

// Application to convert a Unicode database file into a resource script compilable
// to a resource file. For usage see procedure PrintUsage.

uses
  Classes, SysUtils, JclUnicode, JclSysUtils;

type
  TDecomposition = record
    Code: Cardinal;
    Decompositions: array of Cardinal;
  end;

  // collect of case mappings for each code point which is cased
  TCase = record
    Code: Cardinal;
    Fold,               // normalized case for case independent string comparison (e.g. for "ß" this is "ss")
    Lower,              // lower case (e.g. for "ß" this is "ß")
    Title,              // tile case (used mainly for compatiblity, ligatures etc., e.g. for "ß" this is "Ss")
    Upper: TUCS4Array;  // upper cae (e.g. for "ß" this is "SS")
  end;

  // structures for handling numbers
  TCodeIndex = record
    Code,
    Index: Cardinal;
  end;

  TNumber = record
    Numerator,
    Denominator: Integer;
  end;

  // start and stop of a range of code points
  TRange = record
    Start,
    Stop: Cardinal;
  end;

  TRangeArray = array of TRange;

const
  // List of categories expected to be found in the Unicode Character Database
  // including some implementation specific properties.
  // Note: there are multiple definitions which describe the same property (because they are used in the general
  //       categories as well as bidirectional categories (while we store both types as one).
  //       These are:
  //       - Mn, NSM for non-spacing mark
  //       - Zp, B for paragraph separator
  CategoriesStrings: array[TCharacterCategory] of string = (
    // normative categories
    'Lu', // letter, upper case
    'Ll', // letter, lower case
    'Lt', // letter, title case
    'Mn', // mark, non spacing
    'Mc', // mark, spacing combining
    'Me', // mark, enclosing
    'Nd', // number, decimal digit
    'Nl', // number, letter
    'No', // number, other
    'Zs', // separator, space
    'Zl', // separator, line
    'Zp', // separator, paragraph
    'Cc', // other, control
    'Cf', // other, format
    'Cs', // other, surrogate
    'Co', // other, private use
    'Cn', // other, not assigned
    // informative categories
    'Lm', // letter, modifier
    'Lo', // letter, other
    'Pc', // punctuation, connector
    'Pd', // punctuation, dash
    'Ps', // punctuation, open
    'Pe', // punctuation, close
    'Pi', // punctuation, initial quote
    'Pf', // punctuation, final quote
    'Po', // punctuation, other
    'Sm', // symbol, math
    'Sc', // symbol, currency
    'Sk', // symbol, modifier
    'So', // symbol, other
    // bidirectional categories
    'L',   // left-to-right
    'LRE', // left-to-right embedding
    'LRO', // left-to-right override
    'R',   // right-to-left
    'AL',  // right-to-left arabic
    'RLE', // right-to-left embedding
    'RLO', // right-to-left override
    'PDF', // pop directional format
    'EN',  // european number
    'ES',  // european number separator
    'ET',  // european number terminator
    'AN',  // arabic number
    'CS',  // common number separator
      // 'NSM' non-spacing (see comment above)
    'BN',  // boundary neutral
      // 'B' paragraph separator  (see comment above)
    'S',   // segment separator
    'WS',  // white space
    'ON',  // other neutrals
    // self defined categories, they do not appear in the Unicode data file
    'Cm', // composed (can be decomposed)
    'Nb', // non-breaking
    'Sy', // symmetric (has left and right forms)
    'Hd', // hex digit
    'Qm', // quote marks
    'Mr', // mirroring
    'Ss', // space, other
    'Cp' // assigned character (there is a definition in the Unicode standard)
    //'Luu' // letter unique upper case
    );

var
  SourceFileName,
  SpecialCasingFileName,
  CaseFoldingFileName,
  DerivedNormalizationPropsFileName,
  TargetFileName: string;
  Verbose: Boolean;

  // array used to collect a decomposition before adding it to the decomposition table
  DecompTemp: array[0..63] of Cardinal;
  DecompTempSize: Integer;

  // character category ranges
  Categories: array[TCharacterCategory] of TRangeArray;
  // canonical combining classes
  CCCs: array[Byte] of TRangeArray;
  // list of decomposition
  Decompositions: array of TDecomposition;
  // array to hold the number equivalents for specific codes (sorted by code)
  NumberCodes: array of TCodeIndex;
  // array of numbers used in NumberCodes
  Numbers: array of TNumber;
  // array for all case mappings (including 1 to many casing if a special casing source file was given)
  CaseMapping: array of TCase;
  // array of compositions (somehow the same as Decompositions except sorted by decompositions and removed elements)
  Compositions: array of TDecomposition;
  // array of composition exception ranges
  CompositionExceptions: array of TRange;

//----------------------------------------------------------------------------------------------------------------------

procedure FatalError(const S: string);

begin
  if not Verbose then
  begin
    Writeln;
    Writeln('[Fatal error] ' + S);
  end;
  ExitCode := 4;
  Abort;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Warning(const S: string);

begin
  if not Verbose then
  begin
    Writeln;
    Writeln('[Warning] ' + S);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function IsHexDigit(C: Char): Boolean;

begin
  Result := C in ['0'..'9', 'A'..'Z', 'a'..'z'];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddRangeToCategories(Start, Stop: Cardinal; Category: TCharacterCategory); overload;

var
  I, J: Integer;

begin
  // Is this the first entry for this category?
  if Categories[Category] = nil then
  begin
    // first entry, just add it
    SetLength(Categories[Category], 1);
    Categories[Category][0].Start := Start;
    Categories[Category][0].Stop := Stop;
  end
  else
  begin
    // there are already entries for this category

    // optimize the case of adding the range to the end
    I := High(Categories[Category]);
    if Start > Categories[Category][I].Stop then
    begin
      Inc(I);
      SetLength(Categories[Category], I + 1);
      Categories[Category][I].Start := Start;
      Categories[Category][I].Stop := Stop;
    end
    else
    begin
      // need to locate the insertion point
      I := 0;
      while (I < Length(Categories[Category])) and (Start > Categories[Category][I].Start) do
        Inc(I);

      // If the start value lies in the current range, then simply set the
      // new end point of the range to the end value passed as a parameter.
      if (Categories[Category][I].Start <= Start) and (Start <= Categories[Category][I].Stop + 1) then
        Categories[Category][I].Stop := Stop
      else
      begin
        // shift following values up
        J := Length(Categories[Category]);
        SetLength(Categories[Category], J + 1);
        Move(Categories[Category][I], Categories[Category][I + 1], (J - I) * SizeOf(TRange));

        // Add the new range at the insertion point.
        Categories[Category][I].Start := Start;
        Categories[Category][I].Stop := Stop;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddRangeToCategories(Start, Stop: Cardinal; CategoryID: string); overload;

// Adds a range of code points to the categories structure.

var
  Category: TCharacterCategory;

begin
  // find category
  for Category := Low(TCharacterCategory) to High(TCharacterCategory) do
    if CategoriesStrings[Category] = CategoryID then
    begin
      AddRangeToCategories(Start, Stop, Category);
      Break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddToCategories(Code: Cardinal; Category: TCharacterCategory); overload;

var
  I, J: Integer;
  S, E: Cardinal;
  
begin
  // Is this the first entry for this category?
  if Categories[Category] = nil then
  begin
    // first entry, just add it
    SetLength(Categories[Category], 1);
    Categories[Category][0].Start := Code;
    Categories[Category][0].Stop := Code;
  end
  else
  begin
    // there are already entries for this category

    // Optimize the cases of extending the last range and adding new ranges to the end.
    I := High(Categories[Category]);
    E := Categories[Category][I].Stop;
    S := Categories[Category][I].Start;

    if Code = E + 1 then
      // extend the last range
      Categories[Category][I].Stop := Code
    else
    begin
      if Code > E + 1 then
      begin
        // start another range on the end
        Inc(I);
        SetLength(Categories[Category], I + 1);
        Categories[Category][I].Start := Code;
        Categories[Category][I].Stop := Code;
      end
      else
      begin
        // continue only if the given code is not already in the last range
        if Code < S then
        begin
          // The Code should be inserted somewhere before the last range in the
          // list, locate the insertion point.
          I := 0;
          while (I < Length(Categories[Category])) and (Code > Categories[Category][I].Stop + 1) do
            Inc(I);
          E := Categories[Category][I].Stop;
          S := Categories[Category][I].Start;

          if Code = E + 1 then
            Categories[Category][I].Stop := Code // simply extend the current range
          else
            if Code < S then
            begin
              // Add a new entry before the current location.  Shift all entries
              // before the current one up by one to make room.
              J := Length(Categories[Category]);
              SetLength(Categories[Category], J + 1);
              Move(Categories[Category][I], Categories[Category][I + 1], (J - I) * SizeOf(TRange));

              // add the new range at the insertion point
              Categories[Category][I].Start := Code;
              Categories[Category][I].Stop := Code;
          end;
        end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddToCategories(Code: Cardinal; CategoryID: string); overload;

// Adds a range of code points to the categories structure.

var
  Category: TCharacterCategory;

begin
  // find category
  for Category := Low(TCharacterCategory) to High(TCharacterCategory) do
    if CategoriesStrings[Category] = CategoryID then
    begin
      AddToCategories(Code, Category);
      Break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddCanonicalCombiningClass(Code, CCClass: Cardinal);

var
  I, J: Integer;
  E: Cardinal;

begin
  // most of the code points have a combining class of 0 (so to speak the default class)
  // hence we don't need to store them
  if CCClass > 0 then
  begin
    // optimize adding the first item
    if CCCs[CCClass] = nil then
    begin
      SetLength(CCCs[CCClass], 1);
      CCCs[CCClass][0].Start := Code;
      CCCs[CCClass][0].Stop := Code;
    end
    else
    begin
      // Handle the special case of extending the range on the end.
      I := High(CCCs[CCClass]);
      E := CCCs[CCClass][I].Stop;
      if Code = E + 1 then
        CCCs[CCClass][I].Stop := Code
      else
      begin
        // Handle the special case of adding another range on the end.
        if Code > E + 1 then
        begin
          Inc(I);
          SetLength(CCCs[CCClass], I + 1);
          CCCs[CCClass][I].Start := Code;
          CCCs[CCClass][I].Stop := Code;
        end
        else
        begin
          // Locate either the insertion point or range for the Code.
          I := 0;
          while (I < Length(CCCs[CCClass])) and (Code > CCCs[CCClass][I].Stop + 1) do
            Inc(I);

          if Code = CCCs[CCClass][I].Stop + 1 then
            // extend an existing range
            CCCs[CCClass][I].Stop := Code
          else
          begin
            if Code < CCCs[CCClass][I].Start then
            begin
              // start a new range before the current location
              J := Length(CCCs[CCClass]);
              SetLength(CCCs[CCClass], J + 1);
              Move(CCCs[CCClass][I], CCCs[CCClass][I + 1], (J - I) * SizeOf(TRange));

              // add the new range at the insertion point
              CCCs[CCClass][I].Start := Code;
              CCCs[CCClass][I].Stop := Code;
            end;
          end;
        end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function MakeNumber(Num, Denom: Integer): Integer;

// adds a number if it does not already exist and returns its index value

var
  I: Integer;

begin
  Result := -1;
  // determine if the number already exists
  for I := 0 to  High(Numbers) do
    if (Numbers[I].Numerator = Num) and (Numbers[I].Denominator = Denom) then
    begin
      Result := I;
      Break;
    end;

  if Result = -1 then
  begin
    Result := Length(Numbers);
    SetLength(Numbers, Result + 1);

    Numbers[Result].Numerator := Num;
    Numbers[Result].Denominator := Denom;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddNumber(Code: Cardinal; Num, Denom: Integer);

var
  I, J: Integer;

begin
  // Insert the Code in order.
  I := 0;
  J := Length(NumberCodes);
  while (I < J) and (Code > NumberCodes[I].Code) do
    Inc(I);

  // Handle the case of the codes matching and simply replace the number that was there before.
  if (I < J) and (Code = NumberCodes[I].Code) then
    NumberCodes[I].Index := MakeNumber(Num, Denom)
  else
  begin
    // Resize the array if necessary.
    SetLength(NumberCodes, J + 1);

    // Shift things around to insert the Code if necessary.
    if I < J then
    begin
      Move(NumberCodes[I], NumberCodes[I + 1], (J - I) * SizeOf(TCodeIndex));
      FillChar(NumberCodes[I], SizeOf(TCodeIndex), 0);
    end;
    NumberCodes[I].Code := Code;
    NumberCodes[I].Index := MakeNumber(Num, Denom);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddDecomposition(Code: Cardinal);

var
  I, J: Integer;
  
begin
  AddToCategories(Code, ccComposed);

  // locate the insertion point for the code
  I := 0;
  J := Length(Decompositions);
  while (I < J) and (Code > Decompositions[I].Code) do
    Inc(I);

  if (I = J) or (Decompositions[I].Code <> Code) then
  begin
    // allocate space for a new decomposition
    SetLength(Decompositions, J + 1);

    if I < J then
    begin
      // shift the Decompositions up by one if the codes don't match
      Move(Decompositions[I], Decompositions[I + 1], (J - I) * SizeOf(TDecomposition));
      FillChar(Decompositions[I], SizeOf(TDecomposition), 0);
    end;
  end;
  
  // insert or replace a decomposition
  if Length(Decompositions[I].Decompositions) <> DecompTempSize then
    SetLength(Decompositions[I].Decompositions, DecompTempSize);

  Decompositions[I].Code := Code;
  Move(DecompTemp[0], Decompositions[I].Decompositions[0], DecompTempSize * SizeOf(Cardinal));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddRangeToCompositionExclusions(Start, Stop: Cardinal);
begin
  SetLength(CompositionExceptions, Length(CompositionExceptions) + 1);
  CompositionExceptions[High(CompositionExceptions)].Start := Start;
  CompositionExceptions[High(CompositionExceptions)].Stop := Stop;
end;

//----------------------------------------------------------------------------------------------------------------------

function FindOrAddCaseEntry(Code: Cardinal): Integer;

// Used to look up the given code in the case mapping array. If no entry with the given code
// exists then it is added implicitely.

var
  J: Integer;

begin
  Result := 0;
  J := Length(CaseMapping);
  while (Result < J) and (CaseMapping[Result].Code < Code) do
    Inc(Result);

  // this code is not yet in the case mapping table
  if (Result = J) or (CaseMapping[Result].Code <> Code) then
  begin
    SetLength(CaseMapping, J + 1);

    // locate the insertion point
    Result := 0;
    while (Result < J) and (Code > CaseMapping[Result].Code) do
      Inc(Result);
    if Result < J then
    begin
      // shift the array up by one
      Move(CaseMapping[Result], CaseMapping[Result + 1], (J - Result) * SizeOf(TCase));
      FillChar(CaseMapping[Result], SizeOf(TCase), 0);
    end;
    Casemapping[Result].Code := Code;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddFoldCase(Code: Cardinal; FoldMapping: TUCS4Array);

var
  I: Integer;

begin
  I := FindOrAddCaseEntry(Code);
  if Length(CaseMapping[I].Fold) = 0 then
    CaseMapping[I].Fold := Copy(FoldMapping, 0, Length(FoldMapping))
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddLowerCase(Code: Cardinal; Lower: TUCS4Array);

var
  I: Integer;

begin
  I := FindOrAddCaseEntry(Code);
  if Length(CaseMapping[I].Lower) = 0 then
    CaseMapping[I].Lower := Copy(Lower, 0, Length(Lower))
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddUpperCase(Code: Cardinal; Upper: TUCS4Array);

var
  I: Integer;

begin
  I := FindOrAddCaseEntry(Code);
  if Length(CaseMapping[I].Upper) = 0 then
    CaseMapping[I].Upper := Copy(Upper, 0, Length(Upper))
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddTitleCase(Code: Cardinal; Title: TUCS4Array);

var
  I: Integer;

begin
  I := FindOrAddCaseEntry(Code);
  if Length(CaseMapping[I].Title) = 0 then
    CaseMapping[I].Title := Copy(Title, 0, Length(Title))
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SplitLine(const Line: string; Elements: TStringList);

// splits the given string into parts which are separated by semicolon and fills Elements
// with the partial strings

var
  Head,
  Tail: PChar;
  S: string;
  
begin
  Elements.Clear;
  Head := PChar(Line);
  while Head^ <> #0 do
  begin
    Tail := Head;
    // look for next semicolon or string end (or comment identifier)
    while not (Tail^ in [';', '#', #0]) do
      Inc(Tail);
    SetString(S, Head, Tail - Head);
    Elements.Add(Trim(S));
    // ignore all characters in a comment 
    if Tail^ in ['#', #0] then
      Break;
    Head := Tail + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SplitCodes(const Line: string; var Elements: TUCS4Array);

// splits S, which may contain space delimited hex strings, into single parts
// and fills Elements

var
  Head,
  Tail: PChar;
  S: string;
  I: Integer;

begin
  Elements := nil;
  Head := PChar(Line);
  while Head^ <> #0 do
  begin
    Tail := Head;
    while IsHexDigit(Tail^) do
      Inc(Tail);
    SetString(S, Head, Tail - Head);
    if Length(S) > 0 then
    begin
      I := Length(Elements);
      SetLength(Elements, I + 1);
      Elements[I] := StrToInt('$' + S);
    end;
    // skip spaces
    while Tail^ = ' ' do
      Inc(Tail);
    if Tail^ = #0 then
     Break;
    Head := Tail;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ParseData;

// ParseData takes the source file and extracts all relevant data into internal structures to be
// used when creating the resource script.

var
  Lines,
  Line: TStringList;
  I, J: Integer;
  RangePending: Boolean;
  StartCode,
  EndCode: Cardinal;

  // decomposition parsing
  S,
  Buffer: string;
  Head,
  Tail: PChar;

  // number representation
  Nominator,
  Denominator: Integer;

  // case mapping
  AMapping: TUCS4Array;
  
begin
  Lines := TStringList.Create;
  try
    // Unicode data files are about 600K in size, so don't hesitate and load them in one rush.
    Lines.LoadFromFile(SourceFileName);
    Line := TStringList.Create;
    try
      // Go for each line, organization is one line for a code point or two consecutive lines
      // for a range of code points.
      RangePending := False;
      StartCode := 0;
      for I := 0 to Lines.Count - 1 do
      begin
        SplitLine(Lines[I], Line);
        // continue only if the line is not empty
        if Line.Count > 0 then
        begin
          // Line contains now up to 15 entries, starting with the code point value
          if RangePending then
          begin
            // last line was a range start, so this one must be the range end
            if Pos(', LAST>', UpperCase(Line[1])) = 0 then
              FatalError(Format('Range end expected in line %d.', [I + 1]));
            EndCode := StrToInt('$' + Line[0]);

            // register general category
            AddRangeToCategories(StartCode, EndCode, Line[2]);

            // register bidirectional category
            AddRangeToCategories(StartCode, EndCode, Line[4]);

            // mark the range as containing assigned code points
            AddRangeToCategories(StartCode, EndCode, ccAssigned);
            RangePending := False;
          end
          else
          begin
            StartCode := StrToInt('$' + Line[0]);
            // check for the start of a range
            if Pos(', FIRST>', UpperCase(Line[1])) > 0 then
              RangePending := True
            else
            begin
              // normal case, one code point must be parsed

              // 1) categorize code point as being assinged
              AddToCategories(StartCode, ccAssigned);

              if Line.Count < 3 then
                Continue;
              // 2) categorize the general character class
              AddToCategories(StartCode, Line[2]);

              if Line.Count < 4 then
                Continue;
              // 3) register canonical combining class
              AddCanonicalCombiningClass(StartCode, StrToInt(Line[3]));

              if Line.Count < 5 then
                Continue;
              // 4) categorize the bidirectional character class
              AddToCategories(StartCode, Line[4]);

              if Line.Count < 6 then
                Continue;
              // 5) if the character can be decomposed then keep its decomposed parts
              //    and add it to the can-be-decomposed category
              S := Line[5];
              // consider only canonical decomposition mappings
              J := Pos('<', S);
              if (J = 0) and (Length(S) > 0) then
              begin
                DecompTempSize := 0;
                Head := PChar(S);
                while Head^ <> #0 do
                begin
                  Tail := Head;
                  while IsHexDigit(Tail^) do
                    Inc(Tail);
                  SetString(Buffer, Head, Tail - Head);
                  if Length(Buffer) > 1 then
                    DecompTemp[DecompTempSize] := StrToInt('$' + Buffer)
                  else
                    DecompTemp[DecompTempSize] := 0;
                  Inc(DecompTempSize);

                  if Tail^ = #0 then
                    Break;
                  Head := Tail + 1;
                end;

                // If there is more than one code in the temporary decomposition
                // array then add the character with its decomposition.
                // (outchy) latest unicode data have aliases to link items having the same decompositions
                //if DecompTempSize > 1 then
                  AddDecomposition(StartCode);
              end;

              if Line.Count < 9 then
                Break;
              // 6) examine if there is a numeric representation of this code
              if Length(Line[8]) > 0 then
              begin
                Head := PChar(Line[8]);
                Tail := Head;
                while Tail^ in ['+', '-', '0'..'9'] do
                  Inc(Tail);
                SetString(S, Head, Tail - Head);
                Nominator := StrToInt(S);
                Denominator := 1;
                if Tail^ = '/' then
                begin
                  Inc(Tail);
                  Head := Tail;
                  while Tail^ in ['+', '-', '0'..'9'] do
                    Inc(Tail);
                  SetString(S, Head, Tail - Head);
                  Denominator := StrToInt(S);
                end;
                AddNumber(StartCode, Nominator, Denominator);
              end;

              if Line.Count < 13 then
                Continue;
              SetLength(AMapping, 1);
              // 7) read simple upper case mapping (only 1 to 1 mappings)
              if Length(Line[12]) > 0 then
              begin
                AMapping[0] := StrToInt('$' + Line[12]);
                AddUpperCase(StartCode, AMapping);
              end;

              if Line.Count < 14 then
                Continue;
              // 8) read simple lower case mapping
              if Length(Line[13]) > 0 then
              begin
                AMapping[0] := StrToInt('$' + Line[13]);
                AddLowerCase(StartCode, AMapping);
              end;

              if Line.Count < 15 then
                Continue;
              // 9) read title case mapping
              if Length(Line[14]) > 0 then
              begin
                AMapping[0] := StrToInt('$' + Line[14]);
                AddTitleCase(StartCode, AMapping);
              end;
            end;
          end;
        end;
        if not Verbose then
          Write(Format(#13'  %d%% done', [Round(100 * I / Lines.Count)]));
      end;
    finally
      Line.Free;
    end;
  finally
    Lines.Free;
  end;
  if not Verbose then
    Writeln;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ParseSpecialCasing;

// One-to-many case mappings are given by a separate file which is in a different format
// than the Unicode data file. This procedure parses this file and adds those extended mappings
// to the internal array.

var
  Lines,
  Line: TStringList;
  I: Integer;
  Code: Cardinal;

  AMapping: TUCS4Array;

begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(SpecialCasingFileName);
    Line := TStringList.Create;
    try
      for I := 0 to Lines.Count - 1 do
      begin
        SplitLine(Lines[I], Line);
        // continue only if the line is not empty
        if (Line.Count > 0) and (Length(Line[0]) > 0) then
        begin
          Code := StrToInt('$' + Line[0]);
          // extract lower case
          if Length(Line[1]) > 0 then
          begin
            SplitCodes(Line[1], AMapping);
            AddLowerCase(Code, AMapping);
          end;
          // extract title case
          if Length(Line[2]) > 0 then
          begin
            SplitCodes(Line[2], AMapping);
            AddTitleCase(Code, AMapping);
          end;
          // extract upper case
          if Length(Line[3]) > 0 then
          begin
            SplitCodes(Line[3], AMapping);
            AddUpperCase(Code, AMapping);
          end;
        end;
        if not Verbose then
          Write(Format(#13'  %d%% done', [Round(100 * I / Lines.Count)]));
      end;
    finally
      Line.Free;
    end;
  finally
    Lines.Free;
  end;
  if not Verbose then
    Writeln;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ParseCaseFolding;

// Casefolding data is given by yet another optional file. Usually case insensitive string comparisons
// are done by converting the strings to lower case and compare them, but in some cases
// this is not enough. We only add those special cases to our internal casing array.

var
 Lines,
 Line: TStringList;
 I: Integer;
 Code: Cardinal;

 AMapping: TUCS4Array;

begin
 Lines := TStringList.Create;
 try
   Lines.LoadFromFile(CaseFoldingFileName);
   Line := TStringList.Create;
   try
     for I := 0 to Lines.Count - 1 do
     begin
       // Layout of one line is:
       // <code>; <status>; <mapping>; # <name>
       // where status is either "L" describing a normal lowered letter
       // and "E" for exceptions (only the second is read)
       SplitLine(Lines[I], Line);
       // continue only if the line is not empty
       if (Line.Count > 0) and (Length(Line[0]) > 0) then
       begin
         // the code currently being under consideration
         Code := StrToInt('$' + Line[0]);
         // type of mapping
         if ((Line[1] = 'C') or (Line[1] = 'F')) and (Length(Line[2]) > 0) then
         begin
           SplitCodes(Line[2], AMapping);
           AddFoldCase(Code, AMapping);
         end;
       end;
       if not Verbose then
         Write(Format(#13'  %d%% done', [Round(100 * I / Lines.Count)]));
     end;
   finally
     Line.Free;
   end;
 finally
   Lines.Free;
 end;
 if not Verbose then
   Writeln;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ParseDerivedNormalizationProps;

// parse DerivedNormalizationProps looking for composition exclusions

var
 Lines,
 Line: TStringList;
 I, SeparatorPos: Integer;
 Start, Stop: Cardinal;

begin
  Lines := TStringList.Create;
 try
   Lines.LoadFromFile(DerivedNormalizationPropsFileName);
   Line := TStringList.Create;
   try
     for I := 0 to Lines.Count - 1 do
     begin
       // Layout of one line is:
       // <range>; <options> [;...] ; # <name>
       SplitLine(Lines[I], Line);
       // continue only if the line is not empty
       if (Line.Count > 0) and (Length(Line[0]) > 1) then
       begin
         // the range currently being under consideration
         SeparatorPos := Pos('..', Line[0]);
         if SeparatorPos > 0 then
         begin
           Start := StrToInt('$' + Copy(Line[0], 1, SeparatorPos - 1));
           Stop := StrToInt('$' + Copy(Line[0], SeparatorPos + 2, MaxInt));
         end
         else
         begin
           Start := StrToInt('$' + Line[0]);
           Stop := Start;
         end;
         // first option is considered
         if SameText(Line[1], 'Full_Composition_Exclusion') then
           AddRangeToCompositionExclusions(Start, Stop);
       end;
       if not Verbose then
         Write(Format(#13'  %d%% done', [Round(100 * I / Lines.Count)]));
     end;
   finally
     Line.Free;
   end;
 finally
   Lines.Free;
 end;
 if not Verbose then
   Writeln;
end;

//----------------------------------------------------------------------------------------------------------------------

function FindDecomposition(Code: Cardinal): Integer;

var
  L, R, M: Integer;

begin
  Result := -1;
  L := 0;
  R := High(Decompositions);
  while L <= R do
  begin
    M := (L + R) shr 1;
    if Code > Decompositions[M].Code then
      L := M + 1
    else
      if Code < Decompositions[M].Code then
        R := M - 1
      else
      begin
        Result := M;
        Break;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure DecomposeIt(const D: TDecomposition);

var
  I, J: Integer;
  
begin
  for I := 0 to High(D.Decompositions) do
  begin
    J := FindDecomposition(D.Decompositions[I]);
    if J > -1 then
      DecomposeIt(Decompositions[J])
    else
    begin
      DecompTemp[DecompTempSize] := D.Decompositions[I];
      Inc(DecompTempSize);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ExpandDecompositions;

// Expand all decompositions by recursively decomposing each character in the decomposition.

var
  I: Integer;
  
begin
  for I := 0 to High(Decompositions) do
  begin
    DecompTempSize := 0;
    DecomposeIt(Decompositions[I]);
    if DecompTempSize > 0 then
      AddDecomposition(Decompositions[I].Code);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function IsCompositionExcluded(Code: Cardinal): Boolean;

// checks if composition is excluded to this code (decomposition cannot be recomposed)

var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(CompositionExceptions) do
    with CompositionExceptions[I] do
      if (Start <= Code) and (Code <= Stop) then
      begin
        Result := True;
        Break;
      end;
end;

//----------------------------------------------------------------------------------------------------------------------

function SortCompositions(Item1, Item2: Pointer): Integer;
type
  PDecomposition = ^TDecomposition;
var
  Decomposition1, Decomposition2: PDecomposition;
  I, Len1, Len2, MinLen: Integer;
begin
  Decomposition1 := Item1;
  Decomposition2 := Item2;
  Len1 := Length(Decomposition1^.Decompositions);
  Len2 := Length(Decomposition2^.Decompositions);
  MinLen := Len1;
  if MinLen > Len2 then
    MinLen := Len2;

  for I := 0 to MinLen - 1 do
  begin
    if Decomposition1^.Decompositions[I] > Decomposition2^.Decompositions[I] then
    begin
      Result := 1;
      Exit;
    end
    else
    if Decomposition1^.Decompositions[I] < Decomposition2^.Decompositions[I] then
    begin
      Result := -1;
      Exit;
    end;
  end;
  // if starts of two arrays are identical, sorting from longer to shorter (gives more
  // chances to longer combinations at runtime
  if Len1 < Len2 then
    Result := 1
  else
  if Len1 > Len2 then
    Result := -1
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure CreateCompositions;

// create composition list from decomposition list

var
  I, J: Integer;
begin
  // reduce reallocations
  SetLength(Compositions, Length(Decompositions));

  // eliminate exceptions
  I := 0;
  for J := 0 to High(Decompositions) do
    if not IsCompositionExcluded(Decompositions[J].Code) then
  begin
    Compositions[I] := Decompositions[J];
    Inc(I);
  end;

  // fix overhead
  SetLength(Compositions, I);

  SortDynArray(Pointer(Compositions), SizeOf(Compositions[0]), SortCompositions);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure CreateResourceScript;

// creates the target file using the collected data

var
  Stream: TFileStream;
  CurrentLine: string;
  
  //--------------- local functions -------------------------------------------

  procedure WriteLine(S: string = '');

  // writes the given string as line into the resource script

  begin
    S := S + #13#10;
    with Stream do
      WriteBuffer(PChar(S)^, Length(S));
  end;

  //---------------------------------------------------------------------------

  procedure WriteByte(Value: Byte);

  // Buffers one byte of data (conversion to two-digit hex string is performed first)
  // and flushs out the current line if there are 32 values collected.

  begin
    CurrentLine := CurrentLine + Format('%.2x ', [Value]);
    if Length(CurrentLine) = 32 * 3 then
    begin
      WriteLine('  ''' + Trim(CurrentLine) + '''');
      CurrentLine := '';
    end;
  end;

  //---------------------------------------------------------------------------

  procedure WriteLong(Value: Cardinal);

  // records four bytes of data by splitting the given value

  var
    Buffer: array[0..3] of Byte absolute Value;

  begin
    // Buffer is actually not a variable but a different access method for Value
    // in order to avoid shifts or ugly type casts.
    WriteByte(Buffer[0]);
    WriteByte(Buffer[1]);
    WriteByte(Buffer[2]);
    WriteByte(Buffer[3]);
  end;

  //---------------------------------------------------------------------------

  procedure WriteArray(Values: array of Cardinal);

  // loops through Values and writes them into the target file

  var
    I: Integer;

  begin
    for I := 0 to High(Values) do
      WriteLong(Values[I]);
  end;

  //---------------------------------------------------------------------------

  procedure FlushLine;

  begin
    if Length(CurrentLine) > 0 then
    begin
      WriteLine('  ''' + Trim(CurrentLine) + '''');
      CurrentLine := '';
    end;
  end;

  //--------------- end local functions ---------------------------------------

var
  I, J: Integer;
  Category: TCharacterCategory;

begin
  CurrentLine := '';
  Stream := TFileStream.Create(TargetFileName, fmCreate);
  try
    // 1) template header
    WriteLine('/' + StringOfChar('*', 100));
    WriteLine;
    WriteLine;
    WriteLine('  ' + TargetFileName);
    WriteLine;
    WriteLine;
    WriteLine('  Produced by UDExtract written by Dipl. Ing. Mike Lischke, public@lischke-online.de');
    WriteLine;
    WriteLine;
    WriteLine(StringOfChar('*', 100) + '/');
    WriteLine;
    WriteLine;

    // 2) category data
    WriteLine('CATEGORIES UNICODEDATA LOADONCALL MOVEABLE DISCARDABLE');
    WriteLine('{');
    // write out only used categories  
    for Category := Low(TCharacterCategory) to High(TCharacterCategory) do
      if Assigned(Categories[Category]) then
      begin
        // a) record what category it is actually (the cast assumes there will never
        //    be more than 256 categories)
        WriteByte(Ord(Category));
        // b) tell how many ranges are assigned
        WriteLong(Length(Categories[Category]));
        // c) write start and stop code of each range
        for I := 0 to High(Categories[Category]) do
        begin
          WriteLong(Categories[Category][I].Start);
          WriteLong(Categories[Category][I].Stop);
        end;
      end;
      
    FlushLine;
    WriteLine('}');
    WriteLine;
    WriteLine;

    // 3) case mapping data
    WriteLine('CASE UNICODEDATA LOADONCALL MOVEABLE DISCARDABLE');
    WriteLine('{');
    // record how many case mapping entries we have
    WriteLong(Length(CaseMapping));
    for I := 0 to High(CaseMapping) do
      with CaseMapping[I] do
      begin
        // store every available case mapping, consider one-to-many mappings
        // a) write actual code point
        WriteLong(Code);
        // b) write lower case
        WriteLong(Length(Fold));
        WriteArray(Fold);
        // c) write lower case
        WriteLong(Length(Lower));
        WriteArray(Lower);
        // d) write title case
        WriteLong(Length(Title));
        WriteArray(Title);
        // e) write upper case
        WriteLong(Length(Upper));
        WriteArray(Upper);
      end;
    FlushLine;
    WriteLine('}');
    WriteLine;
    WriteLine;

    // 4) decomposition data
    // fully expand all decompositions before generating the output
    ExpandDecompositions;
    WriteLine('DECOMPOSITION UNICODEDATA LOADONCALL MOVEABLE DISCARDABLE');
    WriteLine('{');
    // record how many decomposition entries we have
    WriteLong(Length(Decompositions));
    for I := 0 to High(Decompositions) do
      with Decompositions[I] do
      begin
        WriteLong(Code);
        WriteLong(Length(Decompositions));
        WriteArray(Decompositions);
      end;
    FlushLine;
    WriteLine('}');
    WriteLine;
    WriteLine;

    // 5) canonical combining class data
    WriteLine('COMBINING UNICODEDATA LOADONCALL MOVEABLE DISCARDABLE');
    WriteLine('{');
    for I := 0 to 255 do
      if Assigned(CCCs[I]) then
      begin
        // a) record which class is stored here
        WriteLong(I);
        // b) tell how many ranges are assigned
        WriteLong(Length(CCCs[I]));
        // c) write start and stop code of each range
        for J := 0 to High(CCCs[I]) do
        begin
          WriteLong(CCCs[I][J].Start);
          WriteLong(CCCs[I][J].Stop);
        end;
      end;
      
    FlushLine;
    WriteLine('}');
    WriteLine;
    WriteLine;

    // 6) number data, this is actually two arrays, one which contains the numbers
    //    and the second containing the mapping between a code and a number 
    WriteLine('NUMBERS UNICODEDATA LOADONCALL MOVEABLE DISCARDABLE');
    WriteLine('{');
    // first, write the number definitions (size, values)
    WriteLong(Length(Numbers));
    for I := 0 to High(Numbers) do
    begin
      WriteLong(Cardinal(Numbers[I].Numerator));
      WriteLong(Cardinal(Numbers[I].Denominator));
    end;
    // second, write the number mappings (size, values)
    WriteLong(Length(NumberCodes));
    for I := 0 to High(NumberCodes) do
    begin
      WriteLong(NumberCodes[I].Code);
      WriteLong(NumberCodes[I].Index);
    end;
    FlushLine;
    WriteLine('}');
    WriteLine;
    WriteLine;

    // 7 ) composition data
    // create composition data from decomposition data and exclusion list before generating the output
    CreateCompositions;
    WriteLine('COMPOSITION UNICODEDATA LOADONCALL MOVEABLE DISCARDABLE');
    WriteLine('{');
    // first, write the number of compositions
    WriteLong(Length(Compositions));
    for I := 0 to High(Compositions) do
      with Compositions[I] do
    begin
      WriteLong(Code);
      WriteLong(Length(Decompositions));
      WriteArray(Decompositions);
    end;
    FlushLine;
    WriteLine('}');
  finally
    Stream.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
procedure PrintUsage;

begin
  Writeln('Usage: UDExtract Source[.txt] Target[.rc] options');
  Writeln('  Path and extension are optional. Default extension for all source files');
  Writeln('  (including optional files) is ".txt".');
  Writeln('  Source must be a Unicode data file (e.g. UnicodeData-5.1.0.txt)');
  Writeln('  and Target is a resource script.');
  Writeln;
  Writeln('  Options might have the following values (not case sensitive):');
  Writeln('    /?'#9#9'shows this screen');
  Writeln('    /c=filename'#9'specifies an optional file containing special casing');
  Writeln('    '#9#9'properties (e.g. SpecialCasing-5.1.0.txt)');
  Writeln('    /f=filename'#9'specifies an optional file containing case fold');
  Writeln('    '#9#9'mappings (e.g. CaseFolding-5.1.0.txt)');
  WriteLn('    /d=filename'#9'specifies an optional file containing derived normalization');
  WriteLn('    '#9#9'props (e.g. DerivedNormalizationProps-5.1.0.txt)');
  Writeln('    /v'#9#9'verbose mode; no warnings, errors etc. are shown, no user input is required');
  Writeln;
  Writeln('Press <enter> to continue...');
  Readln;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure CheckExtension(var FileName: String; const Ext: String);

// Checks whether the given file name string contains an extension. If not then Ext is added to FileName.

begin
  if ExtractFileExt(FileName) = '' then
    FileName := FileName + Ext;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ParseOptions;

var
  I: Integer;
  S: string;
  Run: PChar;

begin
  for I := 3 to ParamCount do
  begin
    S := Trim(ParamStr(I));
    if (Length(S) = 0) or (S[1] <> '/') then
    begin
      Halt(2);
    end
    else
    if SameText(S, '/v') then
      Verbose := True
    else
    if SameText(Copy(S, 1, 3), '/c=') then
    begin
      SpecialCasingFileName := Trim(Copy(S, 4, MaxInt));
      if SpecialCasingFileName[1] in ['''', '"'] then
      begin
        Run := PChar(SpecialCasingFileName);
        SpecialCasingFileName := Trim(AnsiExtractQuotedStr(Run, SpecialCasingFileName[1]));
      end;
      CheckExtension(SpecialCasingFileName, '.txt');
    end
    else
    if SameText(Copy(S, 1, 3), '/f=') then
    begin
      CaseFoldingFileName := Trim(Copy(S, 4, MaxInt));
      if CaseFoldingFileName[1] in ['''', '"'] then
      begin
        Run := PChar(CaseFoldingFileName);
        CaseFoldingFileName := Trim(AnsiExtractQuotedStr(Run, CaseFoldingFileName[1]));
      end;
      CheckExtension(CaseFoldingFileName, '.txt');
    end
    else
    if SameText(Copy(S, 1, 3), '/d=') then
    begin
      DerivedNormalizationPropsFileName := Trim(Copy(S, 4, MaxInt));
      if DerivedNormalizationPropsFileName[1] in ['''', '"'] then
      begin
        Run := PChar(DerivedNormalizationPropsFileName);
        DerivedNormalizationPropsFileName := Trim(AnsiExtractQuotedStr(Run, DerivedNormalizationPropsFileName[1]));
      end;
      CheckExtension(DerivedNormalizationPropsFileName, '.txt');
    end
    else
    begin
      PrintUsage;
      if SameText(S, '/?') then
        Halt(0)
      else
        Halt(2);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

begin
  Writeln('Unicode database conversion tool');
  Writeln(#$B8' 2000, written by Dipl. Ing. Mike Lischke [public@lischke-online.de]');
  Writeln;

  if ParamCount = 0 then
    PrintUsage
  else
  try
    ParseOptions;

    SourceFileName := Trim(ParamStr(1));
    CheckExtension(SourceFileName, '.txt');
    TargetFileName := Trim(ParamStr(2));
    CheckExtension(TargetFileName, '.rc');

    if not FileExists(SourceFileName) then
    begin
      if not Verbose then
        Writeln(Format('[Fatal error] ''%s'' not found', [SourceFileName]));
      Halt(1);
    end
    else
    begin
      if not Verbose then
      begin
        Writeln;
        Writeln('Reading data from ' + SourceFileName + ':');
      end;
      ParseData;

      // optional parsing parts
      if Length(SpecialCasingFileName) > 0 then
      begin
        if not FileExists(SpecialCasingFileName) then
        begin
          Writeln;
          Warning(SpecialCasingFileName + ' not found, ignoring special casing');
        end
        else
        begin
          if not Verbose then
          begin
            Writeln;
            Writeln('Reading special casing data from ' + SpecialCasingFileName + ':');
          end;
          ParseSpecialCasing;
        end;
      end;

      if Length(CaseFoldingFileName) > 0 then
      begin
        if not FileExists(CaseFoldingFileName) then
        begin
          Writeln;
          Warning(CaseFoldingFileName + ' not found, ignoring case folding');
        end
        else
        begin
          if not Verbose then
          begin
            Writeln;
            Writeln('Reading case folding data from ' + CaseFoldingFileName + ':');
          end;
          ParseCaseFolding;
        end;
      end;

      if Length(DerivedNormalizationPropsFileName) > 0 then
      begin
        if not FileExists(DerivedNormalizationPropsFileName) then
        begin
          WriteLn;
          Warning(DerivedNormalizationPropsFileName + ' not found, ignoring derived normalization');
        end
        else
        begin
          if not Verbose then
          begin
            WriteLn;
            WriteLn('Reading derived normalization props from ' + DerivedNormalizationPropsFileName + ':');
          end;
          ParseDerivedNormalizationProps;
        end;
      end;

      // finally write the collected data
      if not Verbose then
      begin
        Writeln;
        Writeln;
        Writeln('Writing resource script ' + TargetFileName + '  ');
        CreateResourceScript;
      end;
    end;

  finally
    if not Verbose then
    begin
      Writeln;
      Writeln('Program finished. Press <enter> to continue...');
      ReadLn;
    end;
  end;
end.

