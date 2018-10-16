
type
   Pnetent = ^netent;
   netent = record
        n_name : Pchar;
        n_aliases : ^Pchar;
        n_addrtype : longint;
        n_net : uint32_t;
     end;
   PPNetEnt = ^Pnetent;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TNetEnt = netent;
