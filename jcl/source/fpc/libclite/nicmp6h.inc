

const
   ICMP6_FILTER = 1;
   ICMP6_FILTER_BLOCK = 1;
   ICMP6_FILTER_PASS = 2;
   ICMP6_FILTER_BLOCKOTHERS = 3;
   ICMP6_FILTER_PASSONLY = 4;
type
   Picmp6_filter = ^_icmp6_filter;
   _icmp6_filter = record
        data : array[0..7] of uint32_t;
     end;
   TICMP6_Filter = _icmp6_filter;

   Picmp6_hdr = ^icmp6_hdr;
   icmp6_hdr = record
        icmp6_type : uint8_t;
        icmp6_code : uint8_t;
        icmp6_cksum : uint16_t;
        icmp6_dataun : record
            case longint of
               0 : ( icmp6_un_data32 : array[0..0] of uint32_t );
               1 : ( icmp6_un_data16 : array[0..1] of uint16_t );
               2 : ( icmp6_un_data8 : array[0..3] of uint8_t );
            end;
     end;



const
   ICMP6_DST_UNREACH = 1;
   ICMP6_PACKET_TOO_BIG = 2;
   ICMP6_TIME_EXCEEDED = 3;
   ICMP6_PARAM_PROB = 4;
   ICMP6_INFOMSG_MASK = $80;
   ICMP6_ECHO_REQUEST = 128;
   ICMP6_ECHO_REPLY = 129;
   ICMP6_MEMBERSHIP_QUERY = 130;
   ICMP6_MEMBERSHIP_REPORT = 131;
   ICMP6_MEMBERSHIP_REDUCTION = 132;
   ICMP6_DST_UNREACH_NOROUTE = 0;
   ICMP6_DST_UNREACH_ADMIN = 1;
   ICMP6_DST_UNREACH_NOTNEIGHBOR = 2;
   ICMP6_DST_UNREACH_ADDR = 3;
   ICMP6_DST_UNREACH_NOPORT = 4;
   ICMP6_TIME_EXCEED_TRANSIT = 0;
   ICMP6_TIME_EXCEED_REASSEMBLY = 1;
   ICMP6_PARAMPROB_HEADER = 0;
   ICMP6_PARAMPROB_NEXTHEADER = 1;
   ICMP6_PARAMPROB_OPTION = 2;

function ICMP6_FILTER_WILLPASS(__type: Integer; const filterp: TICMP6_Filter): Boolean;
function ICMP6_FILTER_WILLBLOCK(__type: Integer; const filterp: TICMP6_Filter): Boolean;
procedure ICMP6_FILTER_SETPASS(__type: Integer; var filterp: TICMP6_Filter);
procedure ICMP6_FILTER_SETBLOCK(__type: Integer; var filterp: TICMP6_Filter);
procedure ICMP6_FILTER_SETPASSALL(var filterp: TICMP6_Filter);
procedure ICMP6_FILTER_SETBLOCKALL(var filterp: TICMP6_Filter);

    const
       ND_ROUTER_SOLICIT = 133;
       ND_ROUTER_ADVERT = 134;
       ND_NEIGHBOR_SOLICIT = 135;
       ND_NEIGHBOR_ADVERT = 136;
       ND_REDIRECT = 137;

    type
       Pnd_router_solicit = ^_nd_router_solicit;
       _nd_router_solicit = record
            nd_rs_hdr : icmp6_hdr;
         end;

    type
       Pnd_router_advert = ^_nd_router_advert;
       _nd_router_advert = record
            nd_ra_hdr : icmp6_hdr;
            nd_ra_reachable : uint32_t;
            nd_ra_retransmit : uint32_t;
         end;


    const
       ND_RA_FLAG_MANAGED = $80;
       ND_RA_FLAG_OTHER = $40;
       ND_RA_FLAG_HOME_AGENT = $20;

    type
       Pnd_neighbor_solicit = ^_nd_neighbor_solicit;
       _nd_neighbor_solicit = record
            nd_ns_hdr : icmp6_hdr;
            nd_ns_target : in6_addr;
         end;


    type
       Pnd_neighbor_advert = ^_nd_neighbor_advert;
       _nd_neighbor_advert = record
            nd_na_hdr : icmp6_hdr;
            nd_na_target : in6_addr;
         end;


    const
       ND_NA_FLAG_ROUTER = $00000080;
       ND_NA_FLAG_SOLICITED = $00000040;
       ND_NA_FLAG_OVERRIDE = $00000020;

    type
       Pnd_redirect = ^_nd_redirect;
       _nd_redirect = record
            nd_rd_hdr : icmp6_hdr;
            nd_rd_target : in6_addr;
            nd_rd_dst : in6_addr;
         end;


    type
       Pnd_opt_hdr = ^nd_opt_hdr;
       nd_opt_hdr = record
            nd_opt_type : uint8_t;
            nd_opt_len : uint8_t;
         end;


    const
       ND_OPT_SOURCE_LINKADDR = 1;
       ND_OPT_TARGET_LINKADDR = 2;
       ND_OPT_PREFIX_INFORMATION = 3;
       ND_OPT_REDIRECTED_HEADER = 4;
       ND_OPT_MTU = 5;
       ND_OPT_RTR_ADV_INTERVAL = 7;
       ND_OPT_HOME_AGENT_INFO = 8;
    type
       Pnd_opt_prefix_info = ^nd_opt_prefix_info;
       nd_opt_prefix_info = record
            nd_opt_pi_type : uint8_t;
            nd_opt_pi_len : uint8_t;
            nd_opt_pi_prefix_len : uint8_t;
            nd_opt_pi_flags_reserved : uint8_t;
            nd_opt_pi_valid_time : uint32_t;
            nd_opt_pi_preferred_time : uint32_t;
            nd_opt_pi_reserved2 : uint32_t;
            nd_opt_pi_prefix : in6_addr;
         end;


    const
       ND_OPT_PI_FLAG_ONLINK = $80;
       ND_OPT_PI_FLAG_AUTO = $40;
       ND_OPT_PI_FLAG_RADDR = $20;
    type
       Pnd_opt_rd_hdr = ^nd_opt_rd_hdr;
       nd_opt_rd_hdr = record
            nd_opt_rh_type : uint8_t;
            nd_opt_rh_len : uint8_t;
            nd_opt_rh_reserved1 : uint16_t;
            nd_opt_rh_reserved2 : uint32_t;
         end;

       Pnd_opt_mtu = ^_nd_opt_mtu;
       _nd_opt_mtu = record
            nd_opt_mtu_type : uint8_t;
            nd_opt_mtu_len : uint8_t;
            nd_opt_mtu_reserved : uint16_t;
            nd_opt_mtu_mtu : uint32_t;
         end;

       Pnd_opt_adv_interval = ^_nd_opt_adv_interval;
       _nd_opt_adv_interval = record
            nd_opt_adv_interval_type : uint8_t;
            nd_opt_adv_interval_len : uint8_t;
            nd_opt_adv_interval_reserved : uint16_t;
            nd_opt_adv_interval_ival : uint32_t;
         end;

       Pnd_opt_home_agent_info = ^_nd_opt_home_agent_info;
       _nd_opt_home_agent_info = record
            nd_opt_home_agent_info_type : uint8_t;
            nd_opt_home_agent_info_len : uint8_t;
            nd_opt_home_agent_info_reserved : uint16_t;
            nd_opt_home_agent_info_preference : int16_t;
            nd_opt_home_agent_info_lifetime : uint16_t;
         end;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
