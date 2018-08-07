

const
   WEOF = $ffffffff;

type

   Pwctype_t = ^wctype_t;
   wctype_t = dword;
   Pwctrans_t = ^wctrans_t;
   wctrans_t = ^int32_t;

function iswalpha(__wc:wint_t):longint;cdecl;external clib name 'iswalpha';
function iswcntrl(__wc:wint_t):longint;cdecl;external clib name 'iswcntrl';
function iswdigit(__wc:wint_t):longint;cdecl;external clib name 'iswdigit';
function iswgraph(__wc:wint_t):longint;cdecl;external clib name 'iswgraph';
function iswlower(__wc:wint_t):longint;cdecl;external clib name 'iswlower';
function iswprint(__wc:wint_t):longint;cdecl;external clib name 'iswprint';
function iswpunct(__wc:wint_t):longint;cdecl;external clib name 'iswpunct';
function iswspace(__wc:wint_t):longint;cdecl;external clib name 'iswspace';
function iswupper(__wc:wint_t):longint;cdecl;external clib name 'iswupper';
function iswxdigit(__wc:wint_t):longint;cdecl;external clib name 'iswxdigit';
function iswblank(__wc:wint_t):longint;cdecl;external clib name 'iswblank';
function wctype(__property:Pchar):wctype_t;cdecl;external clib name 'wctype';
function iswctype(__wc:wint_t; __desc:wctype_t):longint;cdecl;external clib name 'iswctype';
function towlower(__wc:wint_t):wint_t;cdecl;external clib name 'towlower';
function towupper(__wc:wint_t):wint_t;cdecl;external clib name 'towupper';
function towctrans(__wc:wint_t; __desc:wctrans_t):wint_t;cdecl;external clib name 'towctrans';

function __iswalnum_l(__wc:wint_t; __locale:__locale_t):longint;cdecl;external clib name '__iswalnum_l';
function __iswalpha_l(__wc:wint_t; __locale:__locale_t):longint;cdecl;external clib name '__iswalpha_l';
function __iswcntrl_l(__wc:wint_t; __locale:__locale_t):longint;cdecl;external clib name '__iswcntrl_l';
function __iswdigit_l(__wc:wint_t; __locale:__locale_t):longint;cdecl;external clib name '__iswdigit_l';
function __iswgraph_l(__wc:wint_t; __locale:__locale_t):longint;cdecl;external clib name '__iswgraph_l';
function __iswlower_l(__wc:wint_t; __locale:__locale_t):longint;cdecl;external clib name '__iswlower_l';
function __iswprint_l(__wc:wint_t; __locale:__locale_t):longint;cdecl;external clib name '__iswprint_l';
function __iswpunct_l(__wc:wint_t; __locale:__locale_t):longint;cdecl;external clib name '__iswpunct_l';
function __iswspace_l(__wc:wint_t; __locale:__locale_t):longint;cdecl;external clib name '__iswspace_l';
function __iswupper_l(__wc:wint_t; __locale:__locale_t):longint;cdecl;external clib name '__iswupper_l';
function __iswxdigit_l(__wc:wint_t; __locale:__locale_t):longint;cdecl;external clib name '__iswxdigit_l';
function __iswblank_l(__wc:wint_t; __locale:__locale_t):longint;cdecl;external clib name '__iswblank_l';
function __wctype_l(__property:Pchar; __locale:__locale_t):wctype_t;cdecl;external clib name '__wctype_l';
function __iswctype_l(__wc:wint_t; __desc:wctype_t; __locale:__locale_t):longint;cdecl;external clib name '__iswctype_l';
function __towlower_l(__wc:wint_t; __locale:__locale_t):wint_t;cdecl;external clib name '__towlower_l';
function __towupper_l(__wc:wint_t; __locale:__locale_t):wint_t;cdecl;external clib name '__towupper_l';
function __wctrans_l(__property:Pchar; __locale:__locale_t):wctrans_t;cdecl;external clib name '__wctrans_l';
function __towctrans_l(__wc:wint_t; __desc:wctrans_t; __locale:__locale_t):wint_t;cdecl;external clib name '__towctrans_l';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
