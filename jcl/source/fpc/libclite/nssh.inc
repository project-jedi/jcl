
type
  nss_status =
  (
    NSS_STATUS_TRYAGAIN := -2,
    NSS_STATUS_UNAVAIL := -1,
    NSS_STATUS_NOTFOUND := 0,
    NSS_STATUS_SUCCESS := 1,
    NSS_STATUS_RETURN := 2
  );

function __nss_configure_lookup(__dbname:Pchar; __string:Pchar):longint;cdecl;external clib name '__nss_configure_lookup';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
