
type
   Puser_fpregs_struct = ^user_fpregs_struct;
   user_fpregs_struct = record
        cwd : longint;
        swd : longint;
        twd : longint;
        fip : longint;
        fcs : longint;
        foo : longint;
        fos : longint;
        st_space : array[0..19] of longint;
     end;

   Puser_fpxregs_struct = ^user_fpxregs_struct;
   user_fpxregs_struct = record
        cwd : word;
        swd : word;
        twd : word;
        fop : word;
        fip : longint;
        fcs : longint;
        foo : longint;
        fos : longint;
        mxcsr : longint;
        reserved : longint;
        st_space : array[0..31] of longint;
        xmm_space : array[0..31] of longint;
        padding : array[0..55] of longint;
     end;

   Puser_regs_struct = ^user_regs_struct;
   user_regs_struct = record
        ebx : longint;
        ecx : longint;
        edx : longint;
        esi : longint;
        edi : longint;
        ebp : longint;
        eax : longint;
        xds : longint;
        xes : longint;
        xfs : longint;
        xgs : longint;
        orig_eax : longint;
        eip : longint;
        xcs : longint;
        eflags : longint;
        esp : longint;
        xss : longint;
     end;

   Puser = ^user;
   user = record
        regs : user_regs_struct;
        u_fpvalid : longint;
        i387 : user_fpregs_struct;
        u_tsize : dword;
        u_dsize : dword;
        u_ssize : dword;
        start_code : dword;
        start_stack : dword;
        signal : longint;
        reserved : longint;
        u_ar0 : Puser_regs_struct;
        u_fpstate : Puser_fpregs_struct;
        magic : dword;
        u_comm : array[0..31] of char;
        u_debugreg : array[0..7] of longint;
     end;


const
   PAGE_SHIFT = 12;
   PAGE_SIZE = 1 shl PAGE_SHIFT;
   PAGE_MASK =  not (PAGE_SIZE - 1);
   NBPG = PAGE_SIZE;
   UPAGES = 1;
   HOST_TEXT_START_ADDR = u.start_code;
   HOST_STACK_END_ADDR = (u.start_stack) + ((u.u_ssize) * NBPG);


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
