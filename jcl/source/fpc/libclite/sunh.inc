
type
   Psockaddr_un = ^sockaddr_un;
   sockaddr_un = record
     sun_family : sa_family_t;
     sun_path : array[0..107] of char;
   end;


Function SUN_LEN(ptr: PSockAddr_un): Cardinal;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TSockAddr_un = sockaddr_un;
