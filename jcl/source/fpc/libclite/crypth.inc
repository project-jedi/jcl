

{ defined earlier in unistdh.inc...
function crypt(__key:Pchar; __salt:Pchar):Pchar;cdecl;external cryptlib name 'crypt';
procedure setkey(__key:Pchar);cdecl;external cryptlib name 'setkey';
procedure encrypt(__block:Pchar; __edflag:longint);cdecl;external cryptlib name 'encrypt';
}
type
   Pcrypt_data = ^crypt_data;
   crypt_data = record
        keysched : array[0..(16 * 8)-1] of char;
        sb0 : array[0..32767] of char;
        sb1 : array[0..32767] of char;
        sb2 : array[0..32767] of char;
        sb3 : array[0..32767] of char;
        crypt_3_buf : array[0..13] of char;
        current_salt : array[0..1] of char;
        current_saltbits : longint;
        direction : longint;
        initialized : longint;
     end;

function crypt_r(__key:Pchar; __salt:Pchar; __data:Pcrypt_data):Pchar;cdecl;external cryptlib name 'crypt_r';
procedure setkey_r(__key:Pchar; __data:Pcrypt_data);cdecl;external cryptlib name 'setkey_r';
procedure encrypt_r(__block:Pchar; __edflag:longint; __data:Pcrypt_data);cdecl;external cryptlib name 'encrypt_r';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TCryptData = crypt_data;
  PCryptData = ^TCryptData;


