function ETHER_IS_VALID_LEN(foo : Cardinal) : boolean;
begin
   ETHER_IS_VALID_LEN:=(foo>=ETHER_MIN_LEN) and (foo<=ETHER_MAX_LEN);
end;
