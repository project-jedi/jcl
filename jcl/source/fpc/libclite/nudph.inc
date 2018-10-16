
type
   Pudphdr = ^udphdr;
   udphdr = record
        source : u_int16_t;
        dest : u_int16_t;
        len : u_int16_t;
        check : u_int16_t;
     end;

const
   SOL_UDP = 17;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
