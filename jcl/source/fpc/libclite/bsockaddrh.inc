
type
   Psa_family_t = ^sa_family_t;
   sa_family_t = word;

Const
   __SOCKADDR_COMMON_SIZE = sizeof(sa_family_t);

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
