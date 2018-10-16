
type
   Pfddi_header = ^fddi_header;
   fddi_header = record
        fddi_fc : u_int8_t;
        fddi_dhost : array[0..(FDDI_K_ALEN)-1] of u_int8_t;
        fddi_shost : array[0..(FDDI_K_ALEN)-1] of u_int8_t;
     end;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
