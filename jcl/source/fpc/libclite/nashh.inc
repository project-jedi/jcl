
type
   Psockaddr_ash = ^sockaddr_ash;
   sockaddr_ash = record
        sash_family : sa_family_t;
        sash_ifindex : longint;
        sash_channel : byte;
        sash_plen : dword;
        sash_prefix : array[0..15] of byte;
     end;


const
   ASH_CHANNEL_ANY = 0;
   ASH_CHANNEL_CONTROL = 1;
   ASH_CHANNEL_REALTIME = 2;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
