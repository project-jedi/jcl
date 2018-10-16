function CLK_TCK : __clock_t;
  begin
     CLK_TCK:=__clock_t(__sysconf(2));
  end;
