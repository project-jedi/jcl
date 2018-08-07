
type
   Picmphdr = ^icmphdr;
   icmphdr = record
        _type : u_int8_t;
        code : u_int8_t;
        checksum : u_int16_t;
        un : record
            case longint of
               0 : ( echo : record
                    id : u_int16_t;
                    sequence : u_int16_t;
                 end );
               1 : ( gateway : u_int32_t );
               2 : ( frag : record
                    __unused : u_int16_t;
                    mtu : u_int16_t;
                 end );
            end;
     end;


const
   ICMP_ECHOREPLY = 0;
   ICMP_DEST_UNREACH = 3;
   ICMP_SOURCE_QUENCH = 4;
   ICMP_REDIRECT = 5;
   ICMP_ECHO = 8;
   ICMP_TIME_EXCEEDED = 11;
   ICMP_PARAMETERPROB = 12;
   ICMP_TIMESTAMP = 13;
   ICMP_TIMESTAMPREPLY = 14;
   ICMP_INFO_REQUEST = 15;
   ICMP_INFO_REPLY = 16;
   ICMP_ADDRESS = 17;
   ICMP_ADDRESSREPLY = 18;
   NR_ICMP_TYPES = 18;
   ICMP_NET_UNREACH = 0;
   ICMP_HOST_UNREACH = 1;
   ICMP_PROT_UNREACH = 2;
   ICMP_PORT_UNREACH = 3;
   ICMP_FRAG_NEEDED = 4;
   ICMP_SR_FAILED = 5;
   ICMP_NET_UNKNOWN = 6;
   ICMP_HOST_UNKNOWN = 7;
   ICMP_HOST_ISOLATED = 8;
   ICMP_NET_ANO = 9;
   ICMP_HOST_ANO = 10;
   ICMP_NET_UNR_TOS = 11;
   ICMP_HOST_UNR_TOS = 12;
   ICMP_PKT_FILTERED = 13;
   ICMP_PREC_VIOLATION = 14;
   ICMP_PREC_CUTOFF = 15;
   NR_ICMP_UNREACH = 15;
   ICMP_REDIR_NET = 0;
   ICMP_REDIR_HOST = 1;
   ICMP_REDIR_NETTOS = 2;
   ICMP_REDIR_HOSTTOS = 3;
   ICMP_EXC_TTL = 0;
   ICMP_EXC_FRAGTIME = 1;

type
   Picmp_ra_addr = ^icmp_ra_addr;
   icmp_ra_addr = record
        ira_addr : u_int32_t;
        ira_preference : u_int32_t;
     end;

   Picmp = ^icmp;
   icmp = record
        icmp_type : u_int8_t;
        icmp_code : u_int8_t;
        icmp_cksum : u_int16_t;
        icmp_hun : record
            case longint of
               0 : ( ih_pptr : u_char );
               1 : ( ih_gwaddr : in_addr );
               2 : ( ih_idseq : record
                    icd_id : u_int16_t;
                    icd_seq : u_int16_t;
                 end );
               3 : ( ih_void : u_int32_t );
               4 : ( ih_pmtu : record
                    ipm_void : u_int16_t;
                    ipm_nextmtu : u_int16_t;
                 end );
               5 : ( ih_rtradv : record
                    irt_num_addrs : u_int8_t;
                    irt_wpa : u_int8_t;
                    irt_lifetime : u_int16_t;
                 end );
            end;
        icmp_dun : record
            case longint of
               0 : ( id_ts : record
                    its_otime : u_int32_t;
                    its_rtime : u_int32_t;
                    its_ttime : u_int32_t;
                 end );
               1 : ( id_ip : record
                    idi_ip : ip;
                 end );
               2 : ( id_radv : icmp_ra_addr );
               3 : ( id_mask : u_int32_t );
               4 : ( id_data : array[0..0] of u_int8_t );
            end;
     end;


const
   ICMP_MINLEN = 8;
   ICMP_TSLEN = 8 + (3 * (sizeof(n_time)));
   ICMP_MASKLEN = 12;
   ICMP_ADVLENMIN = (8 + SizeOf(ip)+8);

    const
       ICMP_UNREACH = 3;
       ICMP_SOURCEQUENCH = 4;
       ICMP_ROUTERADVERT = 9;
       ICMP_ROUTERSOLICIT = 10;
       ICMP_TIMXCEED = 11;
       ICMP_PARAMPROB = 12;
       ICMP_TSTAMP = 13;
       ICMP_TSTAMPREPLY = 14;
       ICMP_IREQ = 15;
       ICMP_IREQREPLY = 16;
       ICMP_MASKREQ = 17;
       ICMP_MASKREPLY = 18;
       ICMP_MAXTYPE = 18;
       ICMP_UNREACH_NET = 0;
       ICMP_UNREACH_HOST = 1;
       ICMP_UNREACH_PROTOCOL = 2;
       ICMP_UNREACH_PORT = 3;
       ICMP_UNREACH_NEEDFRAG = 4;
       ICMP_UNREACH_SRCFAIL = 5;
       ICMP_UNREACH_NET_UNKNOWN = 6;
       ICMP_UNREACH_HOST_UNKNOWN = 7;
       ICMP_UNREACH_ISOLATED = 8;
       ICMP_UNREACH_NET_PROHIB = 9;
       ICMP_UNREACH_HOST_PROHIB = 10;
       ICMP_UNREACH_TOSNET = 11;
       ICMP_UNREACH_TOSHOST = 12;
       ICMP_UNREACH_FILTER_PROHIB = 13;
       ICMP_UNREACH_HOST_PRECEDENCE = 14;
       ICMP_UNREACH_PRECEDENCE_CUTOFF = 15;
       ICMP_REDIRECT_NET = 0;
       ICMP_REDIRECT_HOST = 1;
       ICMP_REDIRECT_TOSNET = 2;
       ICMP_REDIRECT_TOSHOST = 3;
       ICMP_TIMXCEED_INTRANS = 0;
       ICMP_TIMXCEED_REASS = 1;
       ICMP_PARAMPROB_OPTABSENT = 1;

function ICMP_ADVLEN(const p: icmp): cardinal;
Function ICMP_INFOTYPE(_type: cardinal): boolean;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
