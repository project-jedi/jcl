
type
  Pimaxdiv_t = ^imaxdiv_t;
  imaxdiv_t = record
    quot : int64;
    rem : int64;
  end;

function imaxabs (__n : intmax_t) : intmax_t; cdecl; external clib name 'imaxabs';
function imaxdiv (__numer: intmax_t; __denom: intmax_t): imaxdiv_t; cdecl;external clib name 'imaxdiv';
function strtoimax(__nptr: PChar; __endptr: PPChar; __base: Integer): intmax_t;cdecl;external clib name 'strtoimax';
function strtoumax(__nptr: PChar; __endptr: PPChar; __base: Integer): uintmax_t; cdecl; external clib name 'strtoumax';
function wcstoimax(__nptr: Pwchar_t; __endptr: PPwchar_t; __base: Integer): intmax_t; cdecl;external clib name 'wcstoimax';
function wcstoumax(__nptr: Pwchar_t; __endptr: PPwchar_t; __base: Integer): uintmax_t; cdecl;external clib name 'wcstoumax';

