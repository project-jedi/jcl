{ ---------------------------------------------------------------------
    'h2pas -S -c -D -l libc -i' from endian.h and bits/endian.h
  ---------------------------------------------------------------------}

const
  External_library='libc'; {Setup as you need}

{$PACKRECORDS C}

const
  __LITTLE_ENDIAN     = 1234;
  __BIG_ENDIAN        = 4321;
  __PDP_ENDIAN        = 3412;
  __BYTE_ORDER        = __LITTLE_ENDIAN;
  __FLOAT_WORD_ORDER  = __BYTE_ORDER;

  LITTLE_ENDIAN       = __LITTLE_ENDIAN;
  BIG_ENDIAN          = __BIG_ENDIAN;
  PDP_ENDIAN          = __PDP_ENDIAN;
  BYTE_ORDER          = __BYTE_ORDER;
