

const
   TCP_NODELAY = 1;
   TCP_MAXSEG = 2;
   TCP_CORK = 3;
   TCP_KEEPIDLE = 4;
   TCP_KEEPINTVL = 5;
   TCP_KEEPCNT = 6;
   TCP_SYNCNT = 7;
   TCP_LINGER2 = 8;
   TCP_DEFER_ACCEPT = 9;
   TCP_WINDOW_CLAMP = 10;
   TCP_INFO = 11;
   TCP_QUICKACK = 12;

const
   TH_FIN = $01;
   TH_SYN = $02;
   TH_RST = $04;
   TH_PUSH = $08;
   TH_ACK = $10;
   TH_URG = $20;

type
   Ptcphdr = ^tcphdr;
   tcphdr = record
        source : u_int16_t;
        dest : u_int16_t;
        seq : u_int32_t;
        ack_seq : u_int32_t;
        flag0 : u_int16_t;
        window : u_int16_t;
        check : u_int16_t;
        urg_ptr : u_int16_t;
     end;

   Const
     TCP_ESTABLISHED = 1;
     TCP_SYN_SENT = 2;
     TCP_SYN_RECV = 3;
     TCP_FIN_WAIT1 = 4;
     TCP_FIN_WAIT2 = 5;
     TCP_TIME_WAIT = 6;
     TCP_CLOSE = 7;
     TCP_CLOSE_WAIT = 8;
     TCP_LAST_ACK = 9;
     TCP_LISTEN = 10;
     TCP_CLOSING = 11;

   TCPOPT_EOL = 0;
   TCPOPT_NOP = 1;
   TCPOPT_MAXSEG = 2;
   TCPOLEN_MAXSEG = 4;
   TCPOPT_WINDOW = 3;
   TCPOLEN_WINDOW = 3;
   TCPOPT_SACK_PERMITTED = 4;
   TCPOLEN_SACK_PERMITTED = 2;
   TCPOPT_SACK = 5;
   TCPOPT_TIMESTAMP = 8;
   TCPOLEN_TIMESTAMP = 10;
   TCPOLEN_TSTAMP_APPA = TCPOLEN_TIMESTAMP + 2;
   TCPOPT_TSTAMP_HDR = (((TCPOPT_NOP shl 24) or (TCPOPT_NOP shl 16)) or (TCPOPT_TIMESTAMP shl 8)) or TCPOLEN_TIMESTAMP;
   TCP_MSS = 512;
   TCP_MAXWIN = 65535;
   TCP_MAX_WINSHIFT = 14;

   SOL_TCP = 6;

   TCPI_OPT_TIMESTAMPS = 1;
   TCPI_OPT_SACK = 2;
   TCPI_OPT_WSCALE = 4;
   TCPI_OPT_ECN = 8;
type
   tcp_ca_state = (
     TCP_CA_Open := 0,
     TCP_CA_Disorder := 1,
     TCP_CA_CWR := 2,
     TCP_CA_Recovery := 3,
     TCP_CA_Loss := 4);

type
   Ptcp_info = ^_tcp_info;
   _tcp_info = record // Renamed, conflicts with TCP_INFO
        tcpi_state : u_int8_t;
        tcpi_ca_state : u_int8_t;
        tcpi_retransmits : u_int8_t;
        tcpi_probes : u_int8_t;
        tcpi_backoff : u_int8_t;
        tcpi_options : u_int8_t;
        flag0 : u_int8_t;
        tcpi_rto : u_int32_t;
        tcpi_ato : u_int32_t;
        tcpi_snd_mss : u_int32_t;
        tcpi_rcv_mss : u_int32_t;
        tcpi_unacked : u_int32_t;
        tcpi_sacked : u_int32_t;
        tcpi_lost : u_int32_t;
        tcpi_retrans : u_int32_t;
        tcpi_fackets : u_int32_t;
        tcpi_last_data_sent : u_int32_t;
        tcpi_last_ack_sent : u_int32_t;
        tcpi_last_data_recv : u_int32_t;
        tcpi_last_ack_recv : u_int32_t;
        tcpi_pmtu : u_int32_t;
        tcpi_rcv_ssthresh : u_int32_t;
        tcpi_rtt : u_int32_t;
        tcpi_rttvar : u_int32_t;
        tcpi_snd_ssthresh : u_int32_t;
        tcpi_snd_cwnd : u_int32_t;
        tcpi_advmss : u_int32_t;
        tcpi_reordering : u_int32_t;
     end;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
