
type
   Pec_addr = ^ec_addr;
   ec_addr = record
        station : byte;
        net : byte;
     end;

   Psockaddr_ec = ^sockaddr_ec;
   sockaddr_ec = record
        sec_family : sa_family_t;
        port : byte;
        cb : byte;
        _type : byte;
        addr : ec_addr;
        cookie : dword;
     end;


const
   ECTYPE_PACKET_RECEIVED = 0;
   ECTYPE_TRANSMIT_STATUS = $10;
   ECTYPE_TRANSMIT_OK = 1;
   ECTYPE_TRANSMIT_NOT_LISTENING = 2;
   ECTYPE_TRANSMIT_NET_ERROR = 3;
   ECTYPE_TRANSMIT_NO_CLOCK = 4;
   ECTYPE_TRANSMIT_LINE_JAMMED = 5;
   ECTYPE_TRANSMIT_NOT_PRESENT = 6;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
