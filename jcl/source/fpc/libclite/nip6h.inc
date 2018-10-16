
type
   Pip6_hdr = ^ip6_hdr;
   ip6_hdr = record
        ip6_ctlun : record
            case longint of
               0 : ( ip6_un1 : record
                    ip6_un1_flow : uint32_t;
                    ip6_un1_plen : uint16_t;
                    ip6_un1_nxt : uint8_t;
                    ip6_un1_hlim : uint8_t;
                 end );
               1 : ( ip6_un2_vfc : uint8_t );
            end;
        ip6_src : in6_addr;
        ip6_dst : in6_addr;
     end;


   Pip6_hbh = ^ip6_hbh;
   ip6_hbh = record
        ip6h_nxt : uint8_t;
        ip6h_len : uint8_t;
     end;

   Pip6_dest = ^ip6_dest;
   ip6_dest = record
        ip6d_nxt : uint8_t;
        ip6d_len : uint8_t;
     end;

   Pip6_rthdr = ^ip6_rthdr;
   ip6_rthdr = record
        ip6r_nxt : uint8_t;
        ip6r_len : uint8_t;
        ip6r_type : uint8_t;
        ip6r_segleft : uint8_t;
     end;

   Pip6_rthdr0 = ^ip6_rthdr0;
   ip6_rthdr0 = record
        ip6r0_nxt : uint8_t;
        ip6r0_len : uint8_t;
        ip6r0_type : uint8_t;
        ip6r0_segleft : uint8_t;
        ip6r0_reserved : uint8_t;
        ip6r0_slmap : array[0..2] of uint8_t;
        ip6r0_addr : array[0..0] of in6_addr;
     end;

   Pip6_frag = ^ip6_frag;
   ip6_frag = record
        ip6f_nxt : uint8_t;
        ip6f_reserved : uint8_t;
        ip6f_offlg : uint16_t;
        ip6f_ident : uint32_t;
     end;

const
   IP6F_OFF_MASK = $fff8;
   IP6F_RESERVED_MASK = $0006;
   IP6F_MORE_FRAG = $0001;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
