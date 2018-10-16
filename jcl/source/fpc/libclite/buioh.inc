
const
   UIO_MAXIOV = 1024;

type
   Piovec = ^iovec;
   iovec = record
        iov_base : pointer;
        iov_len : size_t;
     end;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TIoVector = iovec;
  PIoVector = ^TIoVector;


