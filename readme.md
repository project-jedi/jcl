JEDI Code Library for Freepascal
================================


The JEDI Code Library (JCL) consists of a set of thoroughly tested and fully documented
utility functions and non-visual classes which can be instantly reused in your Delphi
and C++ Builder projects.

This fork aims to keep JCL compatible with Freepascal/Lazarus.

The original repository can be found at <https://github.com/project-jedi/jcl>

Clone with GIT
--------------
```
> git clone git://github.com/blikblum/jcl.git jcl
> cd jcl
```

How to install
--------------
Open the **jcl/packages/fpc/Jcl.lpk** file in Lazarus and compile

Add Jcl as a dependency in your Lazarus project

For more info about Lazarus packages: <http://wiki.freepascal.org/Lazarus_Packages>

Report Bugs
-----------
To make the bug reporting as efficient as possible, please try to follow these rules:

- Make the report as detailed as possible so we have a fair chance to reproduce and fix it.
- If you have any code that reproduce the problem, attach it to the report (zip file, source only, no dependencies on third-party software).
- Detailed steps are mandatory for us to understand and solve your problem. 
- If you already have a solution you believe will work, include it in the bug report.
- Be prepared to monitor the report after submission since it is very common that we will need additional information.

Use the github [issues](https://github.com/blikblum/jcl/issues) to report a bug

Known Issues
------------

Only Jcl.lpk is ported. JclContainers.lpk, JclVcl.lpk, JclDeveloperTools.lpk and template.lpk were not tested and probably won't work
