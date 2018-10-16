
const
   SHAPER_QLEN = 10;
   SHAPER_LATENCY = 5 * HZ;
   SHAPER_MAXSLIP = 2;
   SHAPER_BURST = HZ / 50;
   SHAPER_SET_DEV = $0001;
   SHAPER_SET_SPEED = $0002;
   SHAPER_GET_DEV = $0003;
   SHAPER_GET_SPEED = $0004;

type
  Pshaperconf = ^shaperconf;
  shaperconf_u = record
    case Integer of
      0: (ssu_name: packed array[0..14-1] of Char);
      1: (ssu_speed: u_int32_t);
  end;

  shaperconf =  record
    ss_cmd: u_int16_t;
    ss_u: shaperconf_u;
  end;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
