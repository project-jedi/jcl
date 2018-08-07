
type
   Psockaddr_pkt = ^sockaddr_pkt;
   sockaddr_pkt = record
        spkt_family: sa_family_t;
        spkt_device : array[0..13] of byte;
        spkt_protocol : word;
     end;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
