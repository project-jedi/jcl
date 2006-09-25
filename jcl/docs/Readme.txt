--------------------------------------------------------------------------------

Jedi Code Library
Release 1.97
Build 2172
10-February-2006

--------------------------------------------------------------------------------

News
JCL release 1.97 provide an updated support for all targets (including Borland
Developer Studio 2006) and an installation of some experts in C#Builder 1 and
Delphi 8.

As always, multiple bugs have been fixed; for detailed change logs, use the
facilities of our CVS repository at SourceForge.net , see below.

Important: 

Note that the package naming has changed: the same package name is used by all
versions of the compiler supporting suffixes (C++Builder 6, Delphi 6, Delphi 7,
C#Builder 1, Delphi 8, Delphi 2005 and BDS 2006); a different suffix is added
for each target. The installer tries to remove old packages. 3rd party packages
requiring old DJcl* resp. CJcl* packages need to be changed to accomodate the
new naming scheme or they will cause conflicts in the IDE at load time.

DCP files are now created in the lib\target subdirectory of the JCL
installation. 3rd party packages requiring JCL packages need to have this path
in their "browse path" option to compile.

(Windows only) Installation options:

 - Packages compiled by the JCL installer don't contain any debug informations
to keep their size as small as possible.

 - The Jedi Code Library packages are required by some 3rd party packages
(including the Jedi Visual Component Library - JVCL), the installer generates
them if the "Packages" node is checked.

 - The installer can generate MAP informations for each package. These
informations can be linked into binaries to become JCL debug data. Once linked
MAP files could be deleted. These options are subnodes of the "Packages" node.

Experts:

 - For Delphi 5, Delphi 6, Delphi 7, C++Builder 5 and C++Builder 6, experts can
be installed as design time packages or dll experts. For C#Builder 1 and
Delphi 8, experts are installed as dll experts (those products don't load
design time packages). For Delphi 2005 and Borland Developer Studio 2006,
experts are installed as design time packages.

 - A new expert integrating version control systems in the IDE was added. It
provides an integration of TortoiseCVS and TortoiseSVN inside the IDE, items are
added in the IDE menu and buttons can be placed in IDE toolbars via the
customize dialog, see below.

 - A dialog-box provides configuration options for JCL experts in the Tools menu.

.net Framework support:

A subset of JCL units was worked over to support Delphi.Net (Delphi 2005 & BDS
2006). The packages belong to the Jedi.Jcl namespace.

--------------------------------------------------------------------------------

Supported Tools
Run time support:
 - Kylix 3

Design time support (only experts):
 - C#Builder 1 (refer to installation notes below).
 - Delphi 8.net (refer to installation notes below).

Both supports (run time and design time):
 - Delphi version 5, 6, 7
 - C++Builder version 5 & 6
 - Delphi 2005 (Delphi Win32 and Delphi.net personalities)
 - Borland Developer Studio 2006 (Delphi Win32, C++ Builder Win32, Delphi.net
   and C#Builder personalities)

--------------------------------------------------------------------------------

Notes
Not every unit supports all tools. Look out for *.exc files in the tool-specific
lib/subdirectories for a list of units excluded from compilation.
Kylix 3/C++ installation is broken; the installer will fail when it attempts to
build the packages. Since the dreaded file open/save dialog Kylix bug is
haunting us again (wasn't it considered to be defeated as of Kernel 2.4.21?),
we are at present not investigating this further.
Free Pascal (FP) support has not been updated for this release; most units from
source/common should work with FP 2.0, as tests with a 2.0 beta (1.9.8)
indicated, but this has not been verified. Note that there are no plans to
support FP versions from the 1.0 branch.
Installation on C#Builder 1 and Delphi 8:

These products cannot be used to build the JCL installer, you need an other
supported product to install JCL experts on these products.
These products are not able to use the JCL library as a runtime library. You
cannot write managed applications and managed packages based on the JCL.
These products are not shipped with their native compilers, you have to download
it from codecentral (http://cc.borland.com). The item (21333)  the native
compiler to be installed in Delphi 8. The item (21334)  the native compiler to
be installed in C#Builder 1. These zip files have to be extracted in the
products director using the standard pattern:
      Executable files (exe and dll)   - BDS\X.0\bin
      Compiler files (dcp and dcu)     - BDS\X.0\lib
      Toolsapi source files            - BDS\X.0\source\ToolsAPI.

--------------------------------------------------------------------------------

JCL Distribution content
Install.bat                   - Compile and run JCL Installer (Win32)
QInstall.bat                  - Compile and run CLX version of JCL Installer (Win32)
install.sh                    - Compile and run JCL Installer (Linux)
bin                           - Common place for sample application EXE files
lib                           - Common place for compiled units.
docs                          - Readme (this file) and other documents
examples                      - JCL example applications
experts                       - JCL IDE experts source code
experts\debug                 - JCL Debug IDE expert for using JclDebug unit
experts\debug\dialog          - Application exception dialog replacement
experts\debug\simdview        - Low-level debug window for XMM registers
experts\debug\threadnames     - IDE expert showing class names for debugged threads
experts\debug\tools           - Tools for creating files with JCL debug information
experts\favfolders            - Favorite folders combobox in IDE open/save file dialogs
experts\projectanalyzer       - Project Analyzer IDE expert
experts\useswizard            - JCL uses wizard
experts\versioncontrol        - Integration of TortoiseCVS and TortoiseSVN in the IDE
examples\common               - CLX and Win32 example applications in Delphi
examples\dotnet               - JCL example applications for Delphi.net
examples\windows              - JCL example applications for Delphi.Win32
examples\windows\delphitools  - Collection of system tools using JCL
help                          - Help file
install                       - Installer source code
packages                      - JCL package sources
source                        - JCL source code

--------------------------------------------------------------------------------

Feedback
If you have any comments or suggestions we would appreciate it if you drop us a
note. There are several ways to get in contact with us:

 - Newsgroup is the recommended way to contact other JCL users and the team
itself. They are hosted at news://forums.talkto.net/jedi.jcl.

 - Write to jcl@delphi-jedi.org  or to jcl-testing@delphi-jedi.org  This email
account should not be used for support requests. If you need support please use
either the newsgroups or the mailing list.

 - If you want to keep up to date about JCL then you can join the JCL mailing list
by going to http://www.egroups.com/group/JEDI-JCL You can also use this list to
voice your opinion, comments or suggestions.

--------------------------------------------------------------------------------

Issue Tracking
An issue tracking tool can be accessed via ('Code Library' category):
http://homepages.borland.com/jedi/issuetracker/

The general rule is: IF YOU WANT TO GET A BUG FIXED YOU NEED TO LOG IT!

The JEDI issue tracker is based up on the Mantis BugTracker Open Source project.
More background information about it is available on its homepage
http://mantisbt.sourceforge.net

Please be aware that you are allowed there to enter feature request and code
donations as well.

--------------------------------------------------------------------------------

Debug Extension for JclDebug unit
The experts\debug folder contains an IDE expert which assists to insert JCL
Debug information into executable files. This can be useful when use source
location routines from JclDebug unit. These routines need some kind of special
information to be able provide source location for given address in the process.
Currently there are four options to get it work:

Generate and deploy MAP file with your executable file. The file is generated
by the linker. It needs to be set in Project|Options dialog -> Linker page,
Detailed checkbox.
Generate and deploy JDBG file file with your executable file. This is binary
file based on MAP file but its size is typically about 12% of original MAP file.
You can generate it by MapToJdbg tool in jcl\examples\windows\tools folder.
The advantage over MAP file is smaller size and better security of the file
content because it is not a plain text file and it also contains a checksum.
Generate Borland TD32 debug symbols. These symbols are stored directly in the
executable file but usually adds several megabytes so the file is very large.
The advantage is you don't have to deploy any other file and it is easy to
generate it by checking Include TD32 debug info in Linker option page.
Insert JCL Debug info into executable file by the IDE expert. The size of added
data is similar to JDBG file but it will be inserted directly into the
executable file. This is probably best option because it combines small size of
included data and no requirement of deploying additional files. In case you use
this option you need install the JclDebugIde expert.
The IDE expert will add new item to IDE Project menu. For Delphi 5, 6 and 7 it
adds 'Insert JCL Debug data' check item at the end of the Project menu. When the
item is checked, everytime the project is compiled by one of following commands:
Compile, Build, Compile All Projects, Build All Projects or Run necessary JCL
debug data are automatically inserted into the executable. Moreover, for Build
and Build All commands dialog with detailed information of size of these data
will be displayed.

You can generate those debug data for packages and libraries as well using the
expert. Each executable file in the project can use different option from those
listed above. It is not necessary to generate any debug data for Borland runtime
packages because the source location code can use names of exported functions to
get procedure or method name. To get line number information for Borland RTL and
VCL/CLX units you have to check Use Debug DCUs checkbox in
Project|Options dialog -> Compiler tab. Unfortunately it is not possible to get
line number information for Borland runtime packages because Borland does not
provide detailed MAP files for them so you get procedure or method name only.

In case you have more than one data source for an executable file by an accident
the best one is chosen in following order:
 - JCL Debug data in the executable file
 - JDBG file
 - Borland TD32 symbols
 - MAP file

Library or Borland package exports 
It is also possible to insert JCL debug data programmatically to the executable
file by using MakeJclDbg command line tool in jcl\examples\windows\delphitools
folder. You can study included makefiles which uses this tool for building
delphitools examples.

To help using JclDebug exceptional stack tracking in application simple dialog
is provided in jcl\experts\debug\dialogfolder. The dialog replaces standard
dialog displayed by VCL or CLX application when an unhandled exception occurs.
It has additional Detailed button showing the stack, list of loaded modules and
other system information. By adding the dialog to the application exceptional
stack tracking code is automatically initialized so you don't have to care about
it. You can also turn on logging to text file by setting the Tag property of the
dialog to '1'. There is also version for CLX (ClxExceptDlg) but it works on
Windows only. These dialogs are intended to be added to Object Repository.

Short description of getting the JclDebug functionality in your project:

 - Close all running instances of Delphi
 - Install JCL and IDE experts by the JCL Installer
 - Run Delphi IDE and open your project
 - Remove any TApplication.OnException handlers from your project(if any).
 - Add new Exception Dialog by selecting File | New | Other ... | Dialogs tab,
 - Select 'Exception Dialog' or 'Exception Dialog with Send' icon, Click OK button,
 - Save the form (use ExceptionDialog.pas name, for example)
 - Check Project | Insert JCL Debug data menu item
 - Do Project | Build

--------------------------------------------------------------------------------

Version control expert

The JCL team is proud to release a new expert integrating version control
actions inside the Delphi/BCB/BDS IDE. It wraps TortoiseCVS  and TortoiseSVN
commands in actions that can be placed on IDE toolbars and in IDE menu.

This expert requires TortoiseCVS  or/and TortoiseSVN installed on the system to
work properly. Please refer to these products documentations for help about
using version control systems.

The structure of the "Jcl Version" menu can be customized in the JCL options
dialog (in the "Tools" menu).

--------------------------------------------------------------------------------

Downloads of stable sources

These sources are official JCL releases and file status can be considered as
stable for use in final applications. During the past years, there have been
around 2 or 3 releases per year.

Jedi Code Library: File List on SourceForge:
http://sourceforge.net/project/showfiles.php?group_id=47514

--------------------------------------------------------------------------------

Development sources

These files are under active development and may cause some incompatibilities
and some conflicts with existing code. You should not use these files in final
applications. The JCL development team provides these files for testing and
feedback from users.

You can download snapshots of the CVS repository updated every day in the  JCL
daily page

To always have access to the most recent changes in the JCL, you should install
a CVS client (we recommend TortoiseCVS and WinCVS) and download the CVS
repository files to your computer. With the CVS client, you can update your
local repository at any time. For more instructions on how to set up CVS and use
it with JCL, see the CVS instruction page. You can also access the CVS
repository via the web interface.

--------------------------------------------------------------------------------

Getting involved in JCL development

If you want to help out making JCL better or bigger or just plain cooler, there
are several ways in which you can help out. Here are some of the things we need
your help on:
 - Donate source code
 - Donate time writing help
 - Donate time writing demos
 - Donate time fixing bugs
 - Share your experience by helping users in newsgroups and mailing lists

JCL accepts donations from developers as long as the source fullfills the
requirements set up by the JEDI and JCL teams. To read more about these
requirements, visit the page http://homepages.borland.com/jedi/jcl

You can also donate your time by writing help for the source already in JCL. We
currently use Doc-o-Matic to create the finished help files but the actual help
sources are plain text files in a simple to understand format. We can provide
you with auto-generated templates with all classes, properties, types etc
already inserted. The "only" thing left to do is fill in the actual help text
for the help items. If you are interested in writing help, contact us.

If you want to help fix bugs in JCL, go to Mantis and check the bug report
there. You can post replies as well as fixes directly in the bug report. One of
the JCL developers will pick up the report/fix and update the CVS repository if
the fix is satisfactory. If you report and fix a lot of bugs, you might even get
developer access to CVS so you can update the JCL files directly.
