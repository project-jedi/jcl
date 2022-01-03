unit libclite;

{$mode objfpc}
{$inline on}
{$h+}

Interface

uses unixtype;

Const
  clib = 'c';
  dllib = 'dl';
  cryptlib = 'crypt';
  threadslib = 'pthread';

{$i glue.inc}           // C to Pascal type mappings
{$i endianh.inc}        // endian.h bits/endian.h
{$i typesh.inc}         // types.h sys/types.h
{$i posixopth.inc}      // bits/posix_opt.h
{$i stdinth.inc}        // stdint.h
{$i wordsizeh.inc}      // bits/wordsize.h
{$i limitsh.inc}        // limits.h
{$i posix1_limh.inc}    // bits/posix1_lim.h
{$i posix2_limh.inc}    // bits/posix2_lim.h
{$i xopen_limh.inc}     // bits/xopen_lim.h
{$i local_limh.inc}     // bits/local_lim.h
{$i inttypesh.inc}      // inttypes.h
{$i errnoh.inc}         // errno.h asm/errno.h bits/errno.h
{$i pathsh.inc}         // paths.h
{$i lib_namesh.inc}     // gnu/lib-names.h
{$i xlocaleh.inc}       // xlocale.h
{$i sigcontexth.inc}    // asm/sigcontext.h (from kernel)
{$i sigseth.inc}        // bits/sigset.h
{$i signumh.inc}        // bits/signum.h
{$i siginfoh.inc}       // bits/siginfo.h
{$i sigstackh.inc}      // bits/sigstack.h
{$i sigactionh.inc}     // bits/sigaction.h
{$i signalh.inc}        // signal.h
{$i btimeh.inc}         // bits/time.h
{$i timeh.inc}          // time.h
{$i stimeh.inc}         // sys/time.h
{$i timexh.inc}         // sys/timex.h
{$i timesh.inc}         // sys/times.h
{$i bschedh.inc}        // bits/sched.h
{$i schedh.inc}         // sched.h
{$i pthreadtypesh.inc}  // bits/pthreadtypes.h
{$i pthreadh.inc}       // pthread.h
{$i sigthreadh.inc}     // bits/sigthread.h
{$i semaphoreh.inc}     // semaphore.h
{$i spawnh.inc}         // spawn.h
{$i bfcntlh.inc}        // bits/fcntl.h
{$i fcntlh.inc}         // fcntl.h
{$i fileh.inc}          // sys/file.h
{$i bdirenth.inc}       // bits/dirent.h
{$i direnth.inc}        // dirent.h
{$i bstath.inc}         // bits/stat.h
{$i sstath.inc}         // sys/stat.h
{$i fnmatchh.inc}       // fnmatch.h
{$i gconvh.inc}         // gconv.h
{$i gconfigh.inc}       // _G_config.h
{$i libioh.inc}         // libio.h
{$i stdioh.inc}         // stdio.h
{$i stdio_limh.inc}     // bits/stdio_lim.h
{$i stdio_exth.inc}     // stdio_ext.h
{$i bconfnameh.inc}     // bits/confname.h
{$i unistdh.inc}        // unistd.h
{$i fstabh.inc}         // fstab.h
{$i mntenth.inc}        // mntent.h
{$i ioctlsh.inc}        // bits/ioctls.h
{$i ioctl_typesh.inc}   // bits/ioctl-types.h
{$i btermiosh.inc}      // bits/termios.h
{$i termiosh.inc}       // termios.h
{$i sttydefaultsh.inc}  // sys/ttydefaults.h
{$i sioctlh.inc}        // sys/ioctl.h
{ $i srawh.inc}          // sys/raw.h
{$i ptyh.inc}           // pty.h
{$i smounth.inc}        // sys/mount.h
{$i ssysctlh.inc}       // sys/sysctl.h
{$i stringh.inc}        // string.h
{$i stdlibh.inc}        // stdlib.h
{$i malloch.inc}        // malloc.h
{$i ssysinfoh.inc}      // sys/sysinfo.h
{$i bdlfcnh.inc}        // bits/dlfcn.h
{$i dlfcnh.inc}         // dlfcn.h
{$i localeh.inc}        // locale.h
{$i nl_typesh.inc}      // nl_types.h
{$i langinfoh.inc}      // langinfo.h
{$i wordexph.inc}       // wordexp.h
{$i iconvh.inc}         // iconv.h
{$i bresourceh.inc}     // bits/resource.h
{$i sresourceh.inc}     // sys/resource.h
{$i argzh.inc}          // argz.h
{$i envzh.inc}          // envz.h
{$i ctypeh.inc}         // sys/ctype.h
{$i wctypeh.inc}        // wctype.h
{$i wcharh.inc}         // wchar.h
{$i bwaitflagsh.inc}    // bits/waitflags.h
{$i bwaitstatush.inc}   // bits/waitstatus.h
{$i swaith.inc}         // sys/wait.h
{$i butsnameh.inc}      // bits/utsname.h
{$i sutsnameh.inc}      // sys/utsname.h
{$i bmmanh.inc}         // bits/mman.h
{$i smmaph.inc}         // sys/mmap.h
{$i ssyslogh.inc}       // sys/syslog.h
{$i glibc_versionh.inc} // from gnu/libc-version.h
{$i buioh.inc}          // bits/uio.h
{$i suioh.inc}          // sys/uio.h
{$i asockiosh.inc}      // asm/sockios.h
{$i asocketh.inc}       // asm/socket.h
{$i bsockaddrh.inc}     // bits/sockaddr.h (inlined in bits/socket.h)
{$i bsocketh.inc}       // bits/socket.h
{$i ssocketh.inc}       // sys/socket.h
{$i sunh.inc}           // sys/un.h
{$i ninh.inc}           // netinet/in.h
{$i binh.inc}           // bits/in.h, inlined in netinet/in.h
{$i aineth.inc}         // arpa/inet.h
{$i bnetdbh.inc}        // bits/netdb.h
{$i netdbh.inc}         // netdb.h
// empty anyway {$i bselecth.inc}       // bits/select.h
{$i sselecth.inc}       // sys/select.h
{$i pwdh.inc}           // pwd.h
{$i grph.inc}           // grp.h
{$i sptraceh.inc}       // sys/ptrace.h
{$i ulimith.inc}        // ulimit.h
{$i bpollh.inc}         // bits/poll.h
{$i spollh.inc}         // sys/poll.h
{$i utimeh.inc}         // utime.h
{$i sysexitsh.inc}      // sysexits.h
{$i bustath.inc}        // bits/ustat.h
{$i sustath.inc}        // sys/ustat.h
{$i errh.inc}           // err.h
{$i errorh.inc}         // error.h
{$i bfenvh.inc}         // bits/fenv.h
{$i fenvh.inc}          // fenv.h
{$i bipch.inc}          // bits/ipc.h
{$i sipch.inc}          // sys/ipc.h
{$i bshmh.inc}          // bits/shm.h
{$i sshmh.inc}          // sys/shm.h
{$i bsemh.inc}          // bits/sem.h
{$i ssemh.inc}          // sys/sem.h
{$i libgenh.inc}        // libgen.h
{$i butmph.inc}         // bits/utmp.h
{$i utmph.inc}          // utmp.h
{$i butmpxh.inc}        // bits/utmpx.h
{$i utmpxh.inc}         // utmpx.h
{$i svtimesh.inc}       // sys/vtimes.h
{$i svlimith.inc}       // sys/vlimit.h
{$i sucontexth.inc}     // sys/ucontext.h
{$i ucontexth.inc}      // ucontext.h
{$i bmsqh.inc}          // bits/msq.h
{$i smsgh.inc}          // sys/msg.h
{$i bstatfsh.inc}       // bits/statfs.h
{$i sstatfsh.inc}       // sys/statfs.h
{$i bstatvfsh.inc}      // bits/statvfs.h
{$i sstatvfsh.inc}      // sys/statvfs.h
{$i monetaryh.inc}      // monetary.h
{$i mcheckh.inc}        // mcheck.h
{$i printfh.inc}        // printf.h
{$i libintlh.inc}       // libintl.h
{$i shadowh.inc}        // shadow.h
{$i fmtmsgh.inc}        // fmtmsg.h
{$i squotah.inc}        // sys/quota.h
{$i stimebh.inc}        // sys/timeb.h
{$i spermh.inc}         // sys/perm.h
{ $i suserh.inc}        // sys/user.h  // You're not supposed to use this...
{$i sswaph.inc}         // sys/swap.h
{$i ssendfileh.inc}     // sys/sendfile.h
{$i srebooth.inc}       // sys/reboot.h
{$i aioh.inc}           // aio.h
{$i aliasesh.inc}       // aliases.h
{$i globh.inc}          // glob.h
{$i crypth.inc}         // crypt.h
{$i sfsuidh.inc}        // sys/fsuid.h
{$i sklogh.inc}         // sys/klog.h
{$i skdaemonh.inc}      // sys/kdaemon.h
{$i saccth.inc}         // sys/acct.h
{$i bstroptsh.inc}      // bits/stropts.h
{$i stroptsh.inc}       // stropts.h
{$i allocah.inc}        // alloca.h
{$i getopth.inc}        // getopt.h
{$i argph.inc}          // argp.h
{$i nssh.inc}           // nss.h
{$i regexh.inc}         // regex.h
{ $i regexph.inc}       // regexp.h // You're not supposed to use this...
{ $i netherneth.inc}     // net/ethernet.h
{$i nifh.inc}           // net/if.h
{$i nif_arph.inc}       // net/if_arp.h
{$i nif_packeth.inc}    // net/if_packet.h
{ $i nppp_defsh.inc}    // net/ppp_defs.h is empty
{ $i nif_ppph.inc}       // net/if_ppp.h
{ $i nif_shaperh.inc}    // net/if_shaper.h
{ $i nif_slip.h}        // net/if_slip.h in kerneldefs
{ $i nppp_comp.h}       // net/ppp-comp.h in kerneldefs
{$i nrouteh.inc}        // net/route.h
{$i nashh.inc}          // netash/ash.h
{$i nath.inc}           // netatalk/at.h
{$i nax25h.inc}         // netax25/ax25.h
{$i nech.inc}           // neteconet/ec.h
{$i nipxh.inc}          // netipx/ipx.h
{$i npacketh.inc}       // netpacket/packet.h
{$i nnetromh.inc}       // netrom/netrom.h
{$i nroseh.inc}         // netrose/rose.h
{ $i nif_etherh.inc}     // netinet/if_ether.h
{ $i netherh.inc}        // netinet/ether.h
{$i nicmp6h.inc}        // netinet/icmp6.h
{ $i nif_fddih.inc}      // netinet/if_fddi.h
{ $i nif_trh.inc}        // netinet/if_tr.h
{$i nigmph.inc}         // netinet/igmp.h
{$i nin_systmh.inc}     // netinet/in_systm.h
{$i niph.inc}           // netinet/ip.h
{$i nip6h.inc}          // netinet/ip6.h
{$i nip_icmph.inc}      // netinet/ip_icmp.h
{$i ntcph.inc}          // netinet/tcp.h
{$i nudph.inc}          // netinet/udp.h
{$i proutedh.inc}       // protocols/routed.h
{$i prwhodh.inc}        // protocols/rwhod.h
{$i ptalkdh.inc}        // protocols/talkd.h
{ $i ptimedh.inc}        // protocols/timed.h
{$i sscsih.inc}         // scsi/scsi.h
{$i sscsi_ioctlh.inc}   // scsi/scsi_ioctl.h
{ $i ssgh.inc}           // scsi/sg.h
{$i ttyenth.inc}        // ttyent.h
{$i sgttyh.inc}         // sgtty.h
{$i searchh.inc}        // search.h

Implementation

{$i types.inc}         // types.h macros.
{$i cerrno.inc}       // errno.h asm/errno.h bits/errno.h macros.
{$i time.inc}         // bits/time.h macros.
{$i stime.inc}        // sys/time.h macros.
{$i dirent.inc}       // dirent.h macros.
{$i sstat.inc}        // sys/stat.h macros.
{$i libio.inc}        // libio.h macros.
{$i termios.inc}      // termios.h macros.
{$i sttydefaults.inc} // sys/ttydefaults.h macros.
{ $i sraw.inc}         // sys/raw.h macros.
{$i bwaitstatus.inc}  // bits/waitstatus.h macros.
{$i ssyslog.inc}      // sys/syslog.h macros.
{$i bsocket.inc}      // bits/socket.h macros.
{$i sun.inc}          // sys/un.h macros.
{$i nin.inc}          // netinet/in.h macros.
{$i sselect.inc}      // sys/select.h macros.
{$i squota.inc}       // sys/quota.h macros.
{ $i nethernet.inc}    // net/ethernet.h macros.
{ $i nif_ppp.inc}      // net/if_ppp.h macros.
{$i nroute.inc}       // net/route.h macros.
{$i nip.inc}          // netinet/ip.h macros.
{ $i nif_ether.inc}    // netinet/if_ether.h macros.
{$i nicmp6.inc}       // netinet/icmp6.h macros.
{$i nip_icmp.inc}     // netinet/ip_icmp.h macros.
{$i pthread.inc}      // pthread.h Kylix compatibility.

end.
