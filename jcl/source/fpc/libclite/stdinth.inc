
type
  int64_t = Int64;
  uint8_t = byte;

  uint16_t = word;
  uint32_t = dword;
  uint64_t = qword;
  int_least8_t = char;
  int_least16_t = smallint;
  int_least32_t = longint;
  int_least64_t = int64;
  uint_least8_t = byte;
  uint_least16_t = word;
  uint_least32_t = dword;
  uint_least64_t = qword;

  int_fast8_t = shortint;
  int_fast16_t = longint;
  int_fast32_t = longint;
  int_fast64_t = int64;
  uint_fast8_t = byte;

  uint_fast16_t = dword;
  uint_fast32_t = dword;
  uint_fast64_t = qword;

  intptr_t = longint;
  uintptr_t = dword;
  intmax_t = Int64;
  uintmax_t = QWord;

const
  INT8_MIN = -(128);
  INT16_MIN = (-(32767)) - 1;
  INT32_MIN = (-(2147483647)) - 1;
  INT64_MIN             = int64_t(-int64_t(9223372036854775807)-1);

  INT8_MAX = 127;
  INT16_MAX = 32767;
  INT32_MAX = 2147483647;
  INT64_MAX             = int64_t(9223372036854775807);

  UINT8_MAX = 255;
  UINT16_MAX = 65535;
  UINT32_MAX = 4294967295;
  UINT64_MAX            = uint64_t(-1);

  INT_LEAST8_MIN = -(128);
  INT_LEAST16_MIN = (-(32767)) - 1;
  INT_LEAST32_MIN = (-(2147483647)) - 1;
  INT_LEAST64_MIN = int_least64_t(-int64_t(9223372036854775807)-1);

  INT_LEAST8_MAX = 127;
  INT_LEAST16_MAX = 32767;
  INT_LEAST32_MAX = 2147483647;
  INT_LEAST64_MAX =  int_least64_t(9223372036854775807);

  UINT_LEAST8_MAX = 255;
  UINT_LEAST16_MAX = 65535;
  UINT_LEAST32_MAX = 4294967295;
  UINT_LEAST64_MAX      = uint_least64_t(-1);

  INT_FAST8_MIN = -(128);
  INT_FAST16_MIN = (-(2147483647)) - 1;
  INT_FAST32_MIN = (-(2147483647)) - 1;
  INT_FAST64_MIN        = int_fast64_t(-Int64(9223372036854775807)-1);

  INT_FAST8_MAX = 127;
  INT_FAST16_MAX = 2147483647;
  INT_FAST32_MAX = 2147483647;
  INT_FAST64_MAX        = int_fast64_t(9223372036854775807);

  UINT_FAST8_MAX = 255;
  UINT_FAST16_MAX = 4294967295;
  UINT_FAST32_MAX = 4294967295;
  UINT_FAST64_MAX       = uint_fast64_t(-1);

  INTPTR_MIN = (-(2147483647)) - 1;
  INTPTR_MAX = 2147483647;
  UINTPTR_MAX = 4294967295;

  INTMAX_MIN            = Int64(-Int64(9223372036854775807)-1);
  INTMAX_MAX            = Int64(9223372036854775807);
  UINTMAX_MAX           = QWord(-1);

  PTRDIFF_MIN = (-(2147483647)) - 1;
  PTRDIFF_MAX = 2147483647;

  SIG_ATOMIC_MIN = (-(2147483647)) - 1;
  SIG_ATOMIC_MAX = 2147483647;

  SIZE_MAX = 4294967295;

  WINT_MIN = 0;
  WINT_MAX = 4294967295;

