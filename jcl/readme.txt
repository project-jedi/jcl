- JEDI Code Library

The JEDI Code Library (JCL) consists of a set of thoroughly tested and fully documented
utility functions and non-visual classes which can be instantly reused in your Delphi
and C++ Builder projects.

The library is built upon code donated from the JEDI community. It is reformatted to achieve
a common look-and-feel, tested, documented and merged into the library. The library is grouped
into several categories such as Strings, Files and I/O, Security, Math and many, many more. 
To get a quick impression of what's available see the Index.txt file.

The library is released to the public under the terms of the Mozilla Public License (MPL) 
and as such can be freely used in both freeware/shareware, opensource and commercial projects. 
The MPL does have a few restrictions and we urge you to read the MPL (license.txt) and MPL FAQ
documents available in the JCL-Help.chm file.


- Contents of the JEDI Code Library

\Source

This directory contains all the source code units for the JEDI Code Library.

\Help

Contains the JCL Helpfile (JCL-Help.chm) and the separate helpfile for the expression parser.
This helpfiles are in the HTML Help format so you will need to install HTML Help, if you 
haven't done so already, before you can view it. You can download HTML Help
from the microsoft website at 

htpp://msdn.microsoft.com/library/tools/htmlhelp/wkshp/download_main.htm


\Help\HIT

Contains a ALPHA package of the JEDI Help Inclusion Tool. It allows to add any HtmlHelp file
to your Delphi 6 IDE. Under no cirumstances install this package in any other Delphi version.

First you have to compile and run the necessary tool. Please go to \Help\HIT\Tool\Source\ and 
compile the file helptool.dpr. Add the HelpTool to your Tool Menu and start it. Add the JCL
helpfiles to the list. Close the tool.
  
To install the package go to \Help\HIT\HelpExtension\Package\ directory and compile and install
the JHIT package. 

If you have questions about JHIT please visit
www.egroups.com/group/jhit/ 


\Packages

Contains design time packages for Delphi 4, 5 and 6.


\Examples

Contains example projects demonstrating some of the functions/classes in the JCL. It also
contains the debug extension, project analyzer (see below) and tools by Petr Vones.


- Installation

1) Unzip to any directory of your liking (we'll refer to this as <JCL>)
2) Add the <JCL>\Source directory to Delphi's library path (Tools | Environment Options)
3) Start using the JCL by including it's units in your project's uses clause

To use the JCL Debug Extension (see the help for JclDebug) you will need to install both the
debug extension itself as well as the JCL design time package. To do so follow these steps:

1) Open the <JCL>\Packages\DJCL.dpk package and click the install button.
2) Open the <JCL>\Examples\DebugExtension\JclDebugIde.dpk package and click the install button.

The same steps are required to install the Project Analyzer:

1) Open the <JCL>\Packages\DJCL.dpk package and click the install button.
2) Open the <JCL>\Examples\ProjectAnalyzer\ProjectAnalyzer package and click the install button.


- What's new

Please have a look at whatsnew.txt file.


- Feedback and Support

JCL is build by the community, but more importantly, for the community. Therefore we would
like to hear from you. If you have any comments or suggestions we would appreciate it 
if you would drop us a note. There are several ways to get in contact with us. 

* You can always contact us directly by e-mail. 

* If you want to keep up to date about JCL then you can join the JCL mailing lists either 
  by going to JCL Yahoogroups page (http://yahoogroups.com/group/JEDI-JCL) or by sending an 
  email to Yahoogroups (JEDI-JCL-subscribe@egroups.com) You can also use this list to voice
  your opinion, comments or suggestions. 


- Bug Reporting

IF you find a bug please let us know about it immediately. There are two ways to contact us

* Send an e-mail to jcl@delphi-jedi.org
  
  Please include the following details to your bug report

  - Your name (to acknowledge your help in the documentation) 
  - E-mail (for follow-up questions)
  - The compiler you are using, for example Delphi 5, update pack 1
  - The Operating system you are using, for example Windows NT 4, service pack 6 
  - JCL Version. You can find the version in the JclBase unit. 
    We need the value of the JclVersion constant. Alternatively you may supply the last
    modification date of the unit in which the code that the bug applies to resides. 
  - Unit Name of the unit the code this bug-report applies to resides in. 
  - Routine The name of the routine the bug report applies to, optionally including the
    sourcecode linenumber(s) 
  - A description of the bug. 
  - A detailed description of the steps to reproduce the bug. 
  - Fix If possible a suggestion on how to fix the bug. 


* Use the bug report form at
  http://delphi-jedi.org/pgbugreport 
  
  See above which information we need from you.


- Known issues

* The StrMatch function does not handle the wild card "*" correctly. In most cases you can
  use the StrMatches function instead. 
* The function CreateDosRedirected doesn't work on NT4 SP6 (you get an empty output)
* The BIOS Information functionality in JclSysInfo does not work correctly on every system. We
  suggest to avoid the use that functions yet.
