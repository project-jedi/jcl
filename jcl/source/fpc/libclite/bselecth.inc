
{$ifndef _SYS_SELECT_H}
{$error "Never use <bits/select.h> directly; include <sys/select.h> instead."}
{$endif}
{$if defined __GNUC__ && __GNUC__ >= 2}
(* error
# define __FD_ZERO(fdsp) \
in define line 26 *)
(* error
  do {                                                                        \
in declaration at line 28 *)
(* error
    int __d0, __d1;                                                           \
(* error
    __asm__ ____ ("cld; rep; stosl"                                   \
in declaration at line 34 *)
(* error
                          : "memory");                                        \
(* error
  } while (0)
in define line 37 *)
(* error
  __asm__ ____ ("btsl %1,%0"                                          \
(* error
                        : "cc","memory")
(* error
# define __FD_CLR(fd, fdsp) \
(* error
                        : "cc","memory")
(* error
# define __FD_ISSET(fd, fdsp) \
 in declarator_list *)
 in declarator_list *)
 in declarator_list *)
 in declarator_list *)
 in declarator_list *)
(* error
   ({register char __result;                                                  \
(* error
     __asm__ ____ ("btl %1,%2 ; setcb %b0"                            \
in declaration at line 54 *)
(* error
                           : "cc");                                           \
(* error
     __result; }))
in declaration at line 55 *)
(* error
     __result; }))
{$else}
in define line 61 *)
(* error
  do {                                                                        \
in declaration at line 63 *)
(* error
    unsigned int __i;                                                         \
(* error
    fd_set *__arr = (set);                                                    \
in declaration at line 64 *)
(* error
    fd_set *__arr = (set);                                                    \
(* error
    for (__i = 0; __i < sizeof (fd_set) / sizeof (__fd_mask); ++__i)          \
in declaration at line 65 *)
(* error
    for (__i = 0; __i < sizeof (fd_set) / sizeof (__fd_mask); ++__i)          \
in declaration at line 65 *)
(* error
    for (__i = 0; __i < sizeof (fd_set) / sizeof (__fd_mask); ++__i)          \
in declaration at line 66 *)
(* error
      __FDS_BITS (__arr)[__i] = 0;                                            \
(* error
  } while (0)
in define line 68 *)
(* error
# define __FD_CLR(d, set)    (__FDS_BITS (set)[__FDELT (d)] &= ~__FDMASK (d))
in define line 69 *)
(* error
# define __FD_ISSET(d, set)  (__FDS_BITS (set)[__FDELT (d)] & __FDMASK (d))
in define line 70 *)
{$endif}


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
