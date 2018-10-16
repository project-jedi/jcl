

function gettext(__msgid:Pchar):Pchar;cdecl;external clib name 'gettext';
function dgettext(__domainname:Pchar; __msgid:Pchar):Pchar;cdecl;external clib name 'dgettext';
function __dgettext(__domainname:Pchar; __msgid:Pchar):Pchar;cdecl;external clib name '__dgettext';
function dcgettext(__domainname:Pchar; __msgid:Pchar; __category:longint):Pchar;cdecl;external clib name 'dcgettext';
function __dcgettext(__domainname:Pchar; __msgid:Pchar; __category:longint):Pchar;cdecl;external clib name '__dcgettext';
function ngettext(__msgid1:Pchar; __msgid2:Pchar; __n:dword):Pchar;cdecl;external clib name 'ngettext';
function dngettext(__domainname:Pchar; __msgid1:Pchar; __msgid2:Pchar; __n:dword):Pchar;cdecl;external clib name 'dngettext';
function dcngettext(__domainname:Pchar; __msgid1:Pchar; __msgid2:Pchar; __n:dword; __category:longint):Pchar;cdecl;external clib name 'dcngettext';
function textdomain(__domainname:Pchar):Pchar;cdecl;external clib name 'textdomain';
function bindtextdomain(__domainname:Pchar; __dirname:Pchar):Pchar;cdecl;external clib name 'bindtextdomain';
function bind_textdomain_codeset(__domainname:Pchar; __codeset:Pchar):Pchar;cdecl;external clib name 'bind_textdomain_codeset';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
