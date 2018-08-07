

type
   Pether_addr = ^ether_addr;
   ether_addr = record
        ether_addr_octet : array[0..(ETH_ALEN)-1] of u_int8_t;
     end;

   Pether_header = ^ether_header;
   ether_header = record
        ether_dhost : array[0..(ETH_ALEN)-1] of u_int8_t;
        ether_shost : array[0..(ETH_ALEN)-1] of u_int8_t;
        ether_type : u_int16_t;
     end;


const
   ETHERTYPE_PUP = $0200;
   ETHERTYPE_IP = $0800;
   ETHERTYPE_ARP = $0806;
   ETHERTYPE_REVARP = $8035;
   ETHER_ADDR_LEN = ETH_ALEN;
   ETHER_TYPE_LEN = 2;
   ETHER_CRC_LEN = 4;
   ETHER_HDR_LEN = ETH_HLEN;
   ETHER_MIN_LEN = ETH_ZLEN + ETHER_CRC_LEN;
   ETHER_MAX_LEN = ETH_FRAME_LEN + ETHER_CRC_LEN;

function ETHER_IS_VALID_LEN(foo : cardinal) : boolean;


const
   ETHERTYPE_TRAIL = $1000;
   ETHERTYPE_NTRAILER = 16;
   ETHERMTU = ETH_DATA_LEN;
   ETHERMIN = (ETHER_MIN_LEN - ETHER_HDR_LEN) - ETHER_CRC_LEN;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TEtherAddr = ether_addr;
  PEtherAddr = ^TEtherAddr;


