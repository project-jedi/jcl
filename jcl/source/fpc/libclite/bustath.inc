
type
   P_ustat = ^_ustat;
   _ustat = record
     f_tfree : __daddr_t;
     f_tinode : __ino_t;
     f_fname : array[0..5] of char;
     f_fpack : array[0..5] of char;
   end;



{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TUStat = _ustat;
  PUStat = ^TUStat;
