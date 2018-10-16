
Function dbtob(num: cardinal): cardinal;

begin
  Result:=num shl 10;
end;


Function btodb(num: cardinal): cardinal;

begin
  Result:=num shr 10;
end;


Function fs_to_dq_blocks(num, blksize: cardinal): quad_t;

begin
  Result:=(num*blksize) div BLOCK_SIZE;
end;


Function QCMD(cmd, _type: cardinal): cardinal;

begin
  Result:=(cmd shl SUBCMDSHIFT) or (_type and SUBCMDMASK);
end;


Function dqoff(UID: loff_t): quad_t;

begin
  Result:=UID*SizeOf(dqblk);
end;
