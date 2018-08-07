
const
   RTLD_LAZY = $00001;
   RTLD_NOW = $00002;
   RTLD_BINDING_MASK = $3;
   RTLD_NOLOAD = $00004;
   RTLD_GLOBAL = $00100;
   RTLD_LOCAL = 0;
   RTLD_NODELETE = $01000;

procedure _dl_mcount_wrapper_check(__selfpc:pointer);cdecl;external clib name '_dl_mcount_wrapper_check';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
