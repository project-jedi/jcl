JEDI Code Library
=================

The JEDI Code Library (JCL) consists of a set of thoroughly tested and fully documented
utility functions and non-visual classes which can be instantly reused in your Delphi
and C++ Builder projects.

The library is built upon code donated from the JEDI community. It is reformatted to achieve
a common look-and-feel, tested, documented and merged into the library. The library is
grouped into several categories such as Strings, Files and I/O, Security, Math and many, many
more. The library is released to the public under the terms of the Mozilla Public License (MPL)
and as such can be freely used in both freeware/shareware, opensource and commercial projects.
The entire JEDI Code Library is distributed under the terms of the Mozilla Public License (MPL).

This includes, but is not limited to, this document and all source code and ancillary files.
Source code files included in the JCL have a header which explicitly states this (as is required)
however, unless noted otherwise, all files including those without an MPL header, are subject
to the MPL license.

This fork is attempt to solve the issue of assembling JCL in the FPC/Lazarus.
The original repository can be found here: https://github.com/project-jedi/jcl

Download the ZIP file
---------------------
If you download the JCL as a ZIP file from GitHub, you also have to download the jedi.inc and
kylix.inc files from the <https://github.com/project-jedi/jedi> project and copy them to the
jcl\jcl\source\include\jedi directory.

Clone with GIT
--------------
```
> git clone https://github.com/Makhaon/jcl.git jcl
```
This will get you the JCL repository.
you have to move the jedi.inc and kylix.inc files from the jcl\jcl\source\include to the
jcl\jcl\source\include\jedi directory.

How to install
--------------
- For install on Delphi start the **jcl\install.bat**
- For install on FPC under Linux or Windows just unpack archive to a folder

FPC/Lazarus compatibility
---------------
Version are compatible with current stable (1.8.4+3.0.4) and trunk (1.9.0+3.1.1) versions 

Daily snapshots
---------------
You can download daily snapshots from <http://jcl.sourceforge.net/daily>

Report Bugs
-----------
While we try hard to release bug free software, bugs are a part of reality. So, if you have found any bugs,
please report them to us. To make the bug reporting as efficient as possible, please try to follow these rules:

- Make the report as detailed as possible so we have a fair chance to reproduce and fix it.
- If you have any code that reproduce the problem, attach it to the report (zip file, source only, no dependencies on third-party software).
- Detailed steps are mandatory for us to understand and solve your problem. 
- If you already have a solution you believe will work, include it in the bug report.
- Be prepared to monitor the report after submission since it is very common that we will need additional information.

[Login](http://issuetracker.delphi-jedi.org/my_view_page.php) to report a bug

If you do not have a login, please [register](http://issuetracker.delphi-jedi.org/signup_page.php), it's quick and will allow you
to receive email updates when the bug you posted gets through the resolution process.

Spicialized fpc bugs you can report here: https://github.com/Makhaon/jcl/issues

Contact
-------
The JCL is built by the community, but more importantly, for the community. Therefore we would like to hear from you. If you have
any problems, comments or suggestions we would appreciate it if you would drop us a note. There are several ways to get in contact with us.
Note that we also like to hear about you if everything is fine, as this will let us know that what we do is appreciated.

- Newsgroups
  The newsgroups are the most active forums for discussing the JCL (and other JEDI related topics) and is also the place where the
  JCL developers hang out. The JCL specific group is called **jedi.jcl**, but you are recommended to also subscribe to at least
  **jedi.general** and **jedi.jvcl**.

  Newsserver (<news://news.delphi-jedi.org>)
    - jedi.jcl (<news://news.delphi-jedi.org/jedi.jcl>)
    - jedi.general (<news://news.delphi-jedi.org/jedi.general>)
    - jedi.jvcl (<news://news.delphi-jedi.org/jedi.jvcl>)

- Web gateway
  If you can't access to the newsgroups directly (because of firewalls or company policy), there is also a [web gateway to the newsgroups](http://newsportal.delphi-jedi.org/).

Fixes are made
-------
List of main fixes are made in the jcl to be patially comitible with fpc/Windows/Linux.
- PathUncPrefix const added in JclFileUtils
- All Libc uses are changed to libclite
- JclSynch is surrounded with IFDEFs as totally incompatible with Linux
- Fixes for fpc mode:
- StrAddRef StrDecRef are removed from JclAnsiStrings
- PULARGE_INTEGER is redefined in JclBase
- HWND redefined as System.THandle
- GetMem removed as unnecessary and incompatible with current fpc versions
- Uses of zlibh, bzip2 and formats are removed as incompatible from JclCompression
- Uses of BaseUnix, dateutils are added in the Linux mode in JclDateTime unit
- LocalDateTimeToDateTime, DateTimeToLocalDateTime are fixed to work in Linux
- DeleteDirectory, CopyDirectory, MoveDirectory are surrounded with IFDEFs as worked under Windows (shell)
- FileExists is removed as already existed and intefered with rtl library
- FileGetTempName is fixed to work in Linux
- GetSizeOfFile is fixed to work in Linux
- $UNDEF SUPPORTS_EXTENDED added in fpc/CPUX64 mode in JclMath unit
- FastDegToRad, FastRadToDeg, FastGradToRad, FastRadToGrad, FastDegToGrad, FastGradToDeg are surrounded with IFDEFs as worked under Windows
- InitExceptObjProc is fixed to work in Windows
- TJclHandleStream is fixed to work in Linux
- Uses of FpWinAPICompatibility is added to JclStringConversions
- _AddRef and _Release are changed in TJclInterfacedStringList and TJclStringList 
- TTabSetData.AddRef and TTabSetData.ReleaseRef are fixed
- GetIPAddress, GetCurrentFolder, SetEnvironmentVar and DelEnvironmentVar are fixed to work in Linux
- GetAccessToHandleList is fixed
- SystemTObjectInstance and IsCompiledWithPackages are surrounded with IFDEFs as worked under Windows
- Fixes in the JclRTTI unit for compatibilty with trunk version
