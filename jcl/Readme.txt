===============================================================================
Jedi Code Library
Release 1.22
Build 965                                                           22-Oct-2002
===============================================================================

It has been a long time since we released the last version of the Jedi Code
Library. Now, we are proud to present Version 1.22. It includes a real bunch of
bug fixes, some new features and even two completely new units.

Furthermore we are introducing an Issue Tracker to help you and us to solve JCL
related problems as fast as possible. The times in which you had to check the
VCS, or contact someone from the team individually to retain information about
the status of a specific bug are gone by now.

So far the good news, some less good news is that the team changed a little
bit. The former lead and main coordinator of the JCL - Marcel van Brakel - has
left the team and moved on to other areas. We hereby wish him good look and
want to say "thank you" for all what he has done. Please do not send any email
regarding JCL support to him - instead use the mailing list, the newsgroup or
one of our email accounts.

* New features

- Delphi 7 support
- JCL Installer allows to specify path to BPL and DCP files
- Exception Dialog with 'Send' button to send the report by Simple MAPI
- New Unit: JclSchedule  Contains scheduler classes.

* Bug fixes

* JclConsole:
  - Changed Alloc and Dealloc to class function

* JclDebug:
  - Bug in TJclMapScanner class address range calculating
  - InsertDebugDataIntoExecutableFile returns True if DebugData already present

* JclFileUtils:
  - GetSizeOfFile function does not work for files larger than 4GB

* JclGraphics:
  - Memory leak in IconToBitmap function

* JclMapi:
  - Bug in TJclEmail.GetParentWnd 

* JclMath:
  - ModFloat(-2, 2) gives 2

* JclStrings:
  - Bug in StrStripNonNumbersChars
  - StrReverse could use StrReverseInPlace

* JclSysInfo:
  - GetShellProcessName does not work on Win9x systems
  - RunningProcessesList, FullPath option does not work under Windows XP
  - Exception in GetBiosName and GetBiosExtendedInfo
  - Program crashes when tries to use function GetBIOSExtendedInfo and others under Windows 2000
  - GetSwapFileUsage gives an error if there is no Page File

* JclSysUtils:
  - StrToBoolean does not accept 'Y', 'N', 'T', 'F'

* JclUnicode:
  - Dialogs unit does not need to be in the uses clause

* JCL Installer:
  - Installerinoperable when using large fonts


* JCL Distribution content:

Install.bat               - Compile and run JCL Installer
makefile.mak              - Builds all JCL examples and tools
Readme.txt
Bin                       - Folder for compiled JCL binaries
Dcu                       - Folder for DCU files
Examples                  - JCL example applications
Examples\DebugExtension   - JCL Debug IDE expert for using JclDebug unit
             \Dialog      - Application exception dialog replacement
             \ThreadNames - IDE expert showing class names for debugged threads
             \Tools       - Tools for creating files with JCL debug information
Examples\DelphiTools      - Collection of system tools using JCL
Examples\InstallHelper    - JCL Installer
Examples\ProjectAnalyzer  - Project Analyzer IDE expert
Help                      - Help file
Packages                  - JCL runtime package and project group containing all
                            available design-time packages
Source                    - JCL source code


* Installation

Supported development tools versions:

- Delphi 4 Update Pack #3
- Delphi 5 Update Pack #1
- Delphi 6 Update Pack #2 (including Personal Edition)
- Delphi 7
- Free Pascal compiler
- C++ Builder 5

Please make sure you have installed latest update packs. You can download them
from Borland Support web page: http://info.borland.com/devsupport/delphi/

Important:
If you have installed any previous version of the JCL you have to delete it. It
is also necessary to remove all installed JCL packages from the IDE. Do not mix
files or compiled packages from older versions of the JCL with current version.

Because the JCL does NOT contain any components you don't have to install it to
the IDE. Just use desired unit(s) in your project (add them to uses clause) and
make sure IDE Library Path or Project Search Path contains JCL\Source folder,
i.e. C:\Delphi\JCL\Source (where C:\Delphi\JCL is folder when you have unzipped
the JCL archive).

- To set the path for all projects in IDE Library Path (recommended) go to:
  Tools|Environment Options -> Library tab, add the path to Library Path field
  or use JCL Installer described later in the document

- To set the path for a project only (optional) go to:
  Project|Options -> Directories/Conditionals page, add the path to Search path
  field

Although you might want to install additional experts or files shipped with the
JCL. Currently there are:

- Help file
- Improved VCL Exception Dialogs (see JclDebug notes below in the document)
- IDE experts

The installation process is described later in the document. You can use JCL
Installer to simplify the installation process.

Using JCL with Delphi 7 Trial Edition:
The compiler in the Trail Edition is not fully compatible with full editions
of Delphi. It is not able to compile units with $WEAKPACKAGEUNIT ON switch
used in the JCL units. You may temporarily comment out the directive in order
to compile the JCL units by this edition.


* JCL Installer

Helps you to integrate Jedi Code Library with Delphi IDE. Currently it assists
with:

- Compiling and installing runtime packages and IDE experts
- Adding JCL Debug extension dialogs to Object Repository
- Adding JCL\Source path to Library Path in Environment Options
- Integrating JCL help file to the IDE.

To start it double-click on Install.bat file in the JCL root folder.


* Feedback 

If you have any comments or suggestions we would appreciate it if you drop us a
note. There are several ways to get in contact with us:

  - Write to jcl@delphi-jedi.org or to jcl-testing@delphi-jedi.org
    This email account should not be used for support requests. If you need
    support please use either the newsgroups or the mailing list.

  - If you want to keep up to date about JCL then you can join the JCL mailing
    list by going to http://www.egroups.com/group/JEDI-JCL
    You can also use this list to voice your opinion, comments or suggestions.

  - If you prefer a newsgroup over a mailing list please join us at 
    news://forums.talkto.net/jedi.jcl.
    The newsgroup is the point where you can discuss the JCL with other users
    and with the team itself.


* Issue Tracking

An issue tracking tool can be accessed via ('Code Library' category):

http://projectjedi.sourceforge.net/mantis/

The general rule is: If you want to get a bug fixed you need to log it!

The JEDI issue tracker is based up on the Mantis BugTracker Open Source project.
More background information about it is available on its homepage
http://mantisbt.sourceforge.net

Please be aware that you are allowed there to enter feature request and code
donations as well.


* Packages

DJCL40.dpk - Runtime package for Delphi 4
DJCL50.dpk - Runtime package for Delphi 5
DJCL60.dpk - Runtime package for Delphi 6
DJCL70.dpk - Runtime package for Delphi 7
CJCL50.bpk - Runtime package for C++ Builder 5

JclDebugIde      - Expert for inserting JCL Debug information into executable
                   files. This is useful when use source location routines in
                   your application
ThreadNameExpert - Expert for displaying class names of TThread classes in
                   Thread Status window during debugging. See ThreadNamesExample
                   project in JCL\Examples folder. This expert isn't shipped for
                   Delphi 7 because the IDE already has the functionality.
ProjectAnalyzer  - Expert showing list of units compiled to a current project
                   including the information how much a unit contributes to the
                   executable file size. It will add 'Analyze Project <name>'
                   item to the Project menu


* Debug Extension for JclDebug unit

Examples\DebugExtension folder contains IDE expert which assists to insert JCL
Debug information into executable files. This can be useful when use source
location routines from JclDebug unit. These routines needs some kind of special
information to be able provide source location for given address in the process.
Currently there are four options to get it work:

1. Generate and deploy MAP file with your executable file. The file is generated
   by the linker. It needs to be set in Project|Options dialog -> Linker page,
   Detailed checkbox.

2. Generate and deploy JDBG file file with your executable file. This is binary
   file based on MAP file but its size is typically about 12% of original MAP
   file. You can generate it by MapToJdbg tool in JCL\Examples\Tools folder.
   The advantage over MAP file is smaller size and better security of the file
   content because it is not a plain text file and it also contains a checksum.

3. Generate Borland TD32 debug symbols. These symbols are stored directly in the
   executable file but usually adds several megabytes so the file is very large.
   The advantage is you don't have to deploy any other file and it is easy to
   generate it by checking Include TD32 debug info in Linker option page.

4. Insert JCL Debug info into executable file by the IDE expert. The size of
   added data is similar to JDBG file but it will be inserted directly into the
   executable file. This is probably best option because it combines small size
   of included data and no requirement of deploying additional files. In case
   you use this option you need install the JclDebugIde expert.

The IDE expert will add new item to IDE Project menu. Due some differences in
Open Tools API the use of the expert is slightly different in Delphi 4. For
Delphi 5, 6 and 7 it adds 'Insert JCL Debug data' check item at the end of the
Project menu. When the item is checked, everytime the project is compiled by one
of following commands: Compile, Build, Compile All Projects, Build All Projects
or Run necessary JCL debug data are automatically inserted into the executable.
Moreover, for Build and Build All commands dialog with detailed information of
size of these data will be displayed. For Delphi 4 there is 'Build JCL Debug'
command only in the Project menu which performs a build including these data.

You can generate those debug data for packages and libraries as well using the
expert. Each executable file in the project can use different option from those
listed above. It is not necessary to generate any debug data for Borland runtime
packages because the source location code can use names of exported functions
to get procedure or method name. To get line number information for Borland RTL
and VCL/CLX units you have to check Use Debug DCUs checkbox in Project|Options
dialog -> Compiler tab (not available in Delphi 4). Unfortunately it is not
possible to get line number information for Borland runtime packages because
Borland does not provide detailed MAP files for them so you get procedure or
method name only.

In case you have more than one data source for an executable file by an accident
the best one is chosen in following order:

1. JCL Debug data in the executable file
2. JDBG file
3. Borland TD32 symbols
4. MAP file
5. Library or Borland package exports

It is also possible to insert JCL debug data programmatically to the executable
file by using MakeJclDbg command line tool in JCL\Examples\Tools folder. You can
study included makefiles which uses this tool for building DelphiTools examples.

To help using JclDebug exceptional stack tracking in application simple dialog
is provided in JCL\Examples\DebugExtension\Dialog folder. The dialog replaces
standard dialog displayed by VCL or CLX application when an unhandled exception
occurs. It has additional Detailed button showing the stack, list of loaded
modules and other system information. By adding the dialog to the application
exceptional stack tracking code is automatically initialized so you don't have
to care about it. You can also turn on logging to text file by setting the Tag
property of the dialog to '1'. There is also version for CLX (ClxExceptDlg) but
it works on Windows only. These dialogs are intended to be added to Object
Repository.

Short description of getting the JclDebug functionality in your project:

1. Close all running instances of Delphi
2. Install JCL and IDE experts by the JCL Installer
3. Run Delphi IDE and open your project
4. Remove any TApplication.OnException handlers from your project (if any).
5. Add new Exception Dialog by selecting File | New | Other ...  | Dialogs tab,
   Select 'Exception Dialog' or 'Exception Dialog with Send' icon,
   Click OK button,
   Save the form (use ExceptionDialog.pas name, for example)
6. Check Project | Insert JCL Debug data menu item
7. Do Project | Build


* Makefiles

In order to compile all examples and tools by one command we provide makefiles.
To use them open Command Prompt, change current directory to root JCL directory
and run MAKE command. It should start to compile all project using most recent
version of compiler from installed Delphi versions. All executable files will be
created in JCL\Bin directory.


* Version Control

We are using FreeVCS as version control system. You can download it from
http://www.freevcs.de

To sychronize please use one of the following accounts:

server: demos.href.com
uid: jcluser1
pwd: jcluser

server: demos.href.com
uid: jcluser2
pwd: jcluser

server: demos.href.com
uid: jcluser3
pwd: jcluser


* Downloads

Jedi Code Library: File List on SourceForge:
http://sourceforge.net/project/showfiles.php?group_id=47514