
const
  __LC_CTYPE    = 0;
  __LC_NUMERIC  = 1;
  __LC_TIME     = 2;
  __LC_COLLATE  = 3;
  __LC_MONETARY = 4;
  __LC_MESSAGES = 5;
  __LC_ALL      = 6;
  __LC_PAPER    = 7;
  __LC_NAME     = 8;
  __LC_ADDRESS  = 9;
  __LC_TELEPHONE = 10;
  __LC_MEASUREMENT = 11;
  __LC_IDENTIFICATION = 12;

  LC_CTYPE = __LC_CTYPE;
  LC_NUMERIC = __LC_NUMERIC;
  LC_TIME = __LC_TIME;
  LC_COLLATE = __LC_COLLATE;
  LC_MONETARY = __LC_MONETARY;
  LC_MESSAGES = __LC_MESSAGES;
  LC_ALL = __LC_ALL;
  LC_PAPER = __LC_PAPER;
  LC_NAME = __LC_NAME;
  LC_ADDRESS = __LC_ADDRESS;
  LC_TELEPHONE = __LC_TELEPHONE;
  LC_MEASUREMENT = __LC_MEASUREMENT;
  LC_IDENTIFICATION = __LC_IDENTIFICATION;

type
   Plconv = ^lconv;
   lconv = record
        decimal_point : Pchar;
        thousands_sep : Pchar;
        grouping : Pchar;
        int_curr_symbol : Pchar;
        currency_symbol : Pchar;
        mon_decimal_point : Pchar;
        mon_thousands_sep : Pchar;
        mon_grouping : Pchar;
        positive_sign : Pchar;
        negative_sign : Pchar;
        int_frac_digits : char;
        frac_digits : char;
        p_cs_precedes : char;
        p_sep_by_space : char;
        n_cs_precedes : char;
        n_sep_by_space : char;
        p_sign_posn : char;
        n_sign_posn : char;
        int_p_cs_precedes : char;
        int_p_sep_by_space : char;
        int_n_cs_precedes : char;
        int_n_sep_by_space : char;
        int_p_sign_posn : char;
        int_n_sign_posn : char;
        __int_p_cs_precedes : char;
        __int_p_sep_by_space : char;
        __int_n_cs_precedes : char;
        __int_n_sep_by_space : char;
        __int_p_sign_posn : char;
        __int_n_sign_posn : char;
     end;


function setlocale(__category:longint; __locale:Pchar):Pchar;cdecl;external clib name 'setlocale';
function localeconv:Plconv;cdecl;external clib name 'localeconv';
function __newlocale(__category_mask:longint; __locale:Pchar; __base:__locale_t):__locale_t;cdecl;external clib name '__newlocale';
function __duplocale(__dataset:__locale_t):__locale_t;cdecl;external clib name '__duplocale';
procedure __freelocale(__dataset:__locale_t);cdecl;external clib name '__freelocale';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TLConv = lconv;
  _lconv = lconv;
