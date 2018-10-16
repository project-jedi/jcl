
{$ifndef _REGEXP_H}

const
   _REGEXP_H = 1;
{$include <features.h>}
{$include <alloca.h>}
{$include <regex.h>}
{$include <stdlib.h>}
{$include <string.h>}
  var
     loc1 : Pchar;cvar;external;
     loc2 : Pchar;cvar;external;
     locs : Pchar;cvar;external;
{$ifndef __DO_NOT_DEFINE_COMPILE}
(* error
{
 in declarator_list *)
  var
 : char;
(* error
  size_t __input_size = 0;
 in declarator_list *)
(* error
  size_t __current_size = 0;
 in declarator_list *)
     __ch : longint;cvar;public;
     __error : longint;cvar;public;
(* error
  regex_t *__expr_ptr;
 in declarator_list *)
{$if defined __GNUC__ && __GNUC__ >= 2}
(* error
  const size_t __req = __alignof__ (regex_t *);
 in declarator_list *)
{$else}
(* error
  const size_t __req = 8;
 in declarator_list *)
{$endif}
(* error
  expbuf += __req;
in declaration at line 122 *)
(* error
  expbuf -= (expbuf - ((char *) 0)) % __req;
in declaration at line 123 *)
(* error
  if (endbuf < expbuf + sizeof (regex_t))
 in declarator_list *)
(* error
    }
in declaration at line 128 *)
(* error
  __expr_ptr->buffer = expbuf + sizeof (regex_t);
(* error
  __expr_ptr->buffer = expbuf + sizeof (regex_t);
 in declarator_list *)
(* error
  __expr_ptr->allocated = endbuf -  (char *) __expr_ptr->buffer;
(* error
  __expr_ptr->allocated = endbuf -  (char *) __expr_ptr->buffer;
 in declarator_list *)
(* error
  while ((__ch = (GETC ())) != eof)
 in declarator_list *)
(* error
          break;
in declaration at line 139 *)
(* error
        }
in declaration at line 144 *)
(* error
          char *__new_room = (char *) alloca (__new_size);
 in declarator_list *)
(* error
          if (__new_room + __new_size == __input_buffer)
 in declarator_list *)
(* error
              __input_buffer = (char *) memcpy (__new_room, __input_buffer,
in declaration at line 151 *)
(* error
            }
in declaration at line 154 *)
(* error
            {
in declaration at line 157 *)
(* error
              __input_buffer = (char *) memcpy (__new_room, __input_buffer,
in declaration at line 159 *)
(* error
            }
in declaration at line 162 *)
(* error
    }
in declaration at line 164 *)
(* error
  __error = regcomp (__expr_ptr, __input_buffer, REG_NEWLINE);
in declaration at line 167 *)
(* error
  if (__error != 0)
 in declarator_list *)
      var
         REG_ESUBREG : case;cvar;public;
         REG_EBRACK : case;cvar;public;
         REG_EPAREN : case;cvar;public;
         REG_EBRACE : case;cvar;public;
         REG_BADBR : case;cvar;public;
         REG_ERANGE : case;cvar;public;
(* error
      case REG_ESIZE:
 in declarator_list *)
(* error
      }
in declaration at line 200 *)
(* error
}
{$endif}
in declaration at line 210 *)

function advance(__string:Pchar; __expbuf:Pchar):longint;cdecl;external clib name 'advance';
{$endif}


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
