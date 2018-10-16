
type
   Pigmp = ^igmp;
   igmp = record
        igmp_type : u_int8_t;
        igmp_code : u_int8_t;
        igmp_cksum : u_int16_t;
        igmp_group : in_addr;
     end;


const
   IGMP_MEMBERSHIP_QUERY = $11;
   IGMP_V1_MEMBERSHIP_REPORT = $12;
   IGMP_V2_MEMBERSHIP_REPORT = $16;
   IGMP_V2_LEAVE_GROUP = $17;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
