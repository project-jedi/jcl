
type
   Ptrn_hdr = ^trn_hdr;
   trn_hdr = record
        trn_ac : u_int8_t;
        trn_fc : u_int8_t;
        trn_dhost : array[0..(TR_ALEN)-1] of u_int8_t;
        trn_shost : array[0..(TR_ALEN)-1] of u_int8_t;
        trn_rcf : u_int16_t;
        trn_rseg : array[0..7] of u_int16_t;
     end;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
