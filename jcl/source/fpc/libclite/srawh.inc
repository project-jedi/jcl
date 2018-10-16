
const
   RAW_MAJOR = 162;

function RAW_SETBIND : cardinal;
function RAW_GETBIND : cardinal;

type
   Praw_config_request = ^raw_config_request;
   raw_config_request = record
        raw_minor : longint;
        block_major : uint64_t;
        block_minor : uint64_t;
     end;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TRawConfigRequest = raw_config_request;
  PRawConfigRequest = ^TRawConfigRequest;

