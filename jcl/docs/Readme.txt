------------------------------------------------------------------------


  Jedi Code Library

Release 1.92
Build 1661
16-September-2004

------------------------------------------------------------------------

*Note: JCL release 1.92 is an interim release to support JVCL 3 Beta 2.
Due to lack of time, the text below has not been updated and does not
reflect the changes in JCL since release 1.91.*

It has been a long time since we released the last publicly promoted
version 1.22 of the Jedi Code Library (JCL). Now, about eight weeks
after release 1.90, we are proud to present Version 1.91. A lot of stuff
changed since release 1.22 - nonetheless we tried to keep backwards
compatability as far as possible. One of the most noticeable changes is
that we are now crossplatform <cps.html>! JCL 1.91 supports writing
platform independent VisualCLX applications for Windows and x86-Linux.

Regarding the leap in version numbers since 1.22: Consider 1.9x as
pre-*2.0*.


      Supported Tools

    * Delphi version 5, 6, 7
    * C++Builder version 5 & 6
    * Kylix 3 (Delphi & C++)

Notes:

    * Not every unit supports all tools. Look out for *.exc files in the
      tool-specific lib/ subdirectories for a list of units excluded
      from compilation.
    * LM.pas is not longer distributed with the JCL. Renamed to
      JwaLM.pas, it is available as part of Marcel van Brakel's
      distribution of Win32 API translations
      <http://members.chello.nl/m.vanbrakel2/win32api._zip>.
    * We didn't manage to persuade Kylix 3/C++ to build the CJclVClx
      package; this issue remains unresolved for the time being and will
      cause the installer to fail at the respective point of execution.
    * *Free Pascal* <http://www.freepascal.org/> (FP) support is in the
      pipeline; this relates to the FP code branch currently under
      development (1.9.x - we will not support FP versions 1.0.x).
      Expect a JCL release providing FP support soon after this branch
      has got stable.


      What's new since version 1.22

_New Units:_

    JclBorlandTools
    JclCIL
    JclCLR
    JclDITs
    JclDotNet
    JclEDI
    JclEDI_ANSIX12
    JclEDI_ANSIX12_Ext
    JclEDI_UNEDIFACT
    JclEDI_UNEDIFACT_Ext
    JclEDISEF
    JclEDITranslators
    JclEDIXML
    JclMetadata
    JclMsdosSys
    JclTask
    JclZLib 

_Changes to existing units:_

    JclDateTime

        function UnixTimeToDateTime(const UnixTime: TJclUnixTime32):
        TDateTime;
        function FileTimeToUnixTime(const FileTime: TFileTime):
        TJclUnixTime32;
        function UnixTimeToFileTime(const UnixTime: TJclUnixTime32):
        TFileTime;

    JclFileUtils

        function PathGetRelativePath(Origin, Destination: string): string;
        TFileHandler = procedure (const FileName: string) of object;
        TFileHandlerEx = procedure (const Directory: string; const
        FileInfo: TSearchRec) of object;
        function VerifyFileAttributeMask(var RejectedAttributes,
        RequiredAttributes: Integer): Boolean;
        function IsFileAttributeMatch(FileAttributes,
        RejectedAttributes, RequiredAttributes: Integer): Boolean;
        function FileAttributesStr(const FileInfo: TSearchRec): string;
        function IsFileNameMatch(FileName: string; const Mask: string;
        const CaseSensitive: Boolean = {$IFDEF MSWINDOWS} False {$ELSE}
        True {$ENDIF}): Boolean;
        function IsRootDirectory(const CanonicFileName: string): Boolean;
        function GetFileStatus(const FileName: string; out StatBuf:
        TStatBuf64; const ResolveSymLinks: Boolean): Integer;
        function Win32MoveFileReplaceExisting(const SrcFilename,
        DstFilename: string): Boolean;
        function CreateSymbolicLink(const Name, Target: string): Boolean;
        function SymbolicLinkTarget(const Name: string): string;
        function FormatVersionString(const FixedInfo: TVSFixedFileInfo;
        VersionFormat: TFileVersionFormat = vfFull): string; overload;
        procedure VersionExtractFileInfo(const FixedInfo:
        TVSFixedFileInfo; var Major, Minor, Build, Revision: Word);
        procedure VersionExtractProductInfo(const FixedInfo:
        TVSFixedFileInfo; var Major, Minor, Build, Revision: Word);
        function VersionFixedFileInfo(const FileName: string; var
        FixedInfo: TVSFixedFileInfo): Boolean;
        function VersionFixedFileInfoString(const FileName: string;
        VersionFormat: TFileVersionFormat = vfFull;
        const NotAvailableText: string = ''): string;
        procedure EnumFiles(const Path: string; HandleFile:
        TFileHandlerEx; RejectedAttributes: Integer =
        faRejectedByDefault; RequiredAttributes: Integer = 0; Abort:
        PBoolean = nil);
        procedure EnumDirectories(const Root: string; const
        HandleDirectory: TFileHandler;
        const IncludeHiddenDirectories: Boolean = False; const
        SubDirectoriesMask: string = '';
        Abort: PBoolean = nil {$IFDEF UNIX}; ResolveSymLinks: Boolean =
        True {$ENDIF});
        function FileSearch: IJclFileEnumerator;

        Classes:

            TJclFileAttributeMask
            TJclFileEnumerator

        Interfaces:

            IJclFileEnumerator

    JclIniFiles

        TJclISOMemIniFile
        TJclISOIniFile

    JclMath

        function CommercialRound(const X: Float): Int64;

    JclMapi

        function JclSimpleSendFax(const ARecipient, AName, ASubject,
        ABody: string; const AAttachment: TFileName = ''; ShowDialog:
        Boolean = True; AParentWND: HWND = 0): Boolean;
        function JclSimpleBringUpSendMailDialog(const ASubject, ABody:
        string; const AAttachment: TFileName = ''; AParentWND: HWND =
        0): Boolean;

    JclMiscel

        function WinExec32AndRedirectOutput(const Cmd: string; var
        Output: string; RawOutput: Boolean = False): Cardinal; 

    JclPeImage

        function PeUpdateLinkerTimeStamp(const FileName: string; const
        Time: TDateTime): Boolean;
        function PeReadLinkerTimeStamp(const FileName: string): TDateTime;

    JclRegistry

        function RegGetDataSize(const RootKey: DelphiHKEY; const Key,
        Name: string; out DataSize: Cardinal): Boolean;
        function RegGetDataType(const RootKey: DelphiHKEY; const Key,
        Name: string; out DataType: Cardinal): Boolean;
        function RegReadCardinal(const RootKey: DelphiHKEY; const Key,
        Name: string): Cardinal;
        function RegReadCardinalDef(const RootKey: DelphiHKEY; const
        Key, Name: string; Def: Cardinal): Cardinal;
        function RegReadInt64(const RootKey: DelphiHKEY; const Key,
        Name: string): Int64;
        function RegReadInt64Def(const RootKey: DelphiHKEY; const Key,
        Name: string; Def: Int64): Int64;
        function RegReadUInt64(const RootKey: DelphiHKEY; const Key,
        Name: string): UInt64;
        function RegReadUInt64Def(const RootKey: DelphiHKEY; const Key,
        Name: string; Def: UInt64): UInt64;
        function RegReadAnsiString(const RootKey: DelphiHKEY; const Key,
        Name: AnsiString): AnsiString;
        function RegReadAnsiStringDef(const RootKey: DelphiHKEY; const
        Key, Name, Def: AnsiString): AnsiString;
        function RegReadWideString(const RootKey: DelphiHKEY; const Key,
        Name: WideString): WideString;
        function RegReadWideStringDef(const RootKey: DelphiHKEY; const
        Key, Name, Def: WideString): WideString;
        function RegReadMultiString(const RootKey: DelphiHKEY; const
        Key, Name: string): string; overload;
        procedure RegReadMultiString(const RootKey: DelphiHKEY; const
        Key, Name: string; out Value: TDynStringArray); overload;
        procedure RegReadMultiString(const RootKey: DelphiHKEY; const
        Key, Name: string; Value: TStrings); overload;
        function RegReadMultiStringDef(const RootKey: DelphiHKEY; const
        Key, Name: string; const Def: string): string; overload;
        procedure RegReadMultiStringDef(const RootKey: DelphiHKEY; const
        Key, Name: string; out Value: TDynStringArray; const Def:
        TDynStringArray); overload;
        procedure RegReadMultiStringDef(const RootKey: DelphiHKEY; const
        Key, Name: string; Value, Def: TStrings); overload;
        function RegReadMultiAnsiString(const RootKey: DelphiHKEY; const
        Key, Name: AnsiString): AnsiString;
        function RegReadMultiAnsiStringDef(const RootKey: DelphiHKEY;
        const Key, Name: AnsiString; const Def: AnsiString): AnsiString;
        function RegReadMultiWideString(const RootKey: DelphiHKEY; const
        Key, Name: WideString): WideString;
        function RegReadMultiWideStringDef(const RootKey: DelphiHKEY;
        const Key, Name: WideString; const Def: WideString): WideString;
        procedure RegReadBinary(const RootKey: DelphiHKEY; const Key,
        Name: string; out Value: TDynByteArray); overload;
        function RegReadBinaryAsAnsiString(const RootKey: DelphiHKEY;
        const Key, Name: AnsiString): AnsiString;
        function RegReadBinaryAsAnsiStringDef(const RootKey: DelphiHKEY;
        const Key, Name: string; const Def: AnsiString): AnsiString;
        function RegReadBinaryAsWideString(const RootKey: DelphiHKEY;
        const Key, Name: WideString): WideString;
        function RegReadBinaryAsWideStringDef(const RootKey: DelphiHKEY;
        const Key, Name: string; const Def: WideString): WideString;
        procedure RegWriteCardinal(const RootKey: DelphiHKEY; const Key,
        Name: string; Value: Cardinal);
        procedure RegWriteInt64(const RootKey: DelphiHKEY; const Key,
        Name: string; Value: Int64);
        procedure RegWriteUInt64(const RootKey: DelphiHKEY; const Key,
        Name: string; Value: UInt64);
        procedure RegWriteAnsiString(const RootKey: DelphiHKEY; const
        Key, Name, Value: AnsiString);
        procedure RegWriteWideString(const RootKey: DelphiHKEY; const
        Key, Name, Value: WideString);
        procedure RegWriteMultiString(const RootKey: DelphiHKEY; const
        Key, Name: string; const Value: string); overload;
        procedure RegWriteMultiString(const RootKey: DelphiHKEY; const
        Key, Name: string; const Value: array of String); overload;
        procedure RegWriteMultiString(const RootKey: DelphiHKEY; const
        Key, Name: string; const Value: TDynStringArray); overload;
        procedure RegWriteMultiString(const RootKey: DelphiHKEY; const
        Key, Name: string; const Value: TStrings); overload;

    JclRTTI

        dropped: function JclHookIs(const IsProc: TJclIsFunc): Boolean;
        dropped: function JclIsHooked: Boolean;
        dropped: function JclUnhookIs: Boolean;

    JclShell

        dropped: function ShellLinkGetIcon(const Link: TShellLink; const
        Icon: TIcon): Boolean;
        function ShellLinkIcon(const Link: TShellLink): HICON; overload;
        function ShellLinkIcon(const FileName: string): HICON; overload;

    JclStatistics

        function MedianUnsorted(const X: TDynFloatArray): Float; 

    JclStrings

        dropped: function StrIsNumber(const S: AnsiString): Boolean;
        function StrConsistsOfNumberChars(const S: AnsiString): Boolean;
        /(was: StrIsNumber)/
        function StrEnsureNoPrefix(const Prefix, Text: AnsiString):
        AnsiString;
        function StrEnsureNoSuffix(const Suffix, Text: AnsiString):
        AnsiString;
        function CharLastPos(const S: AnsiString; const C: AnsiChar;
        const Index: Integer = 1): Integer;
        function StringsToMultiString(const Value: array of AnsiString):
        AnsiString; overload;
        function StringsToMultiString(const Value: TDynStringArray):
        AnsiString; overload;
        function StringsToMultiString(const Value: TStrings):
        AnsiString; overload;
        procedure MultiStringToStrings(out Dest: TDynStringArray; const
        Value: AnsiString); overload;
        procedure MultiStringToStrings(Dest: TStrings; const Value:
        AnsiString); overload;
        function StringsToMultiWideString(const Value: array of
        AnsiString): WideString; overload;
        function StringsToMultiWideString(const Value: TDynStringArray):
        WideString; overload;
        function StringsToMultiWideString(const Value: TStrings):
        WideString; overload;
        procedure MultiWideStringToStrings(out Dest: TDynStringArray;
        const Value: WideString); overload;
        procedure MultiWideStringToStrings(Dest: TStrings; const Value:
        WideString); overload;

    JclSvcCtrl

        function Install(const ServiceName, DisplayName, ImageName:
        string; const Description: string = ''; ServiceTypes:
        TJclServiceTypes = [stWin32OwnProcess]; StartType:
        TJclServiceStartType = sstDemand; ErrorControlType:
        TJclServiceErrorControlType = ectNormal; DesiredAccess: DWORD =
        DefaultSvcDesiredAccess; const LoadOrderGroup: TJclServiceGroup
        = nil; const Dependencies: PChar = nil; const Account: PChar =
        nil; const Password: PChar = nil): TJclNtService;
        function GetServiceStatus(ServiceHandle: SC_HANDLE): DWord;
        function GetServiceStatusWaitingIfPending(ServiceHandle:
        SC_HANDLE): DWord;


      JCL Distribution content

Install.bat                   - Compile and run JCL Installer (Win32)
QInstall.bat                  - Compile and run CLX version of JCL Installer (Win32)
install.sh                    - Compile and run JCL Installer (Linux)
bin                           - Common place for sample application EXE files
lib                           - Common place for compiled units.
docs                          - Readme (this file) and other documents
examples                      - JCL example applications
examples\make.bat             - Builds selected examples and tools
examples\vcl                  - JCL example applications
examples\vcl\debugextension   - JCL Debug IDE expert for using JclDebug unit
                 \dialog      - Application exception dialog replacement
                 \threadnames - IDE expert showing class names for debugged threads
                 \tools       - Tools for creating files with JCL debug information
examples\vcl\delphitools      - Collection of system tools using JCL
examples\vcl\projectanalyzer  - Project Analyzer IDE expert
examples\visclx               - JCL example applications
examples\windows              - JCL example applications
help                          - Help file
install                       - Installer source code
packages                      - JCL runtime packages and project groups
source                        - JCL source code


      Feedback

If you have any comments or suggestions we would appreciate it if you
drop us a note. There are several ways to get in contact with us:

    * Write to jcl@delphi-jedi.org <mailto:jcl@delphi-jedi.org> or to
      jcl-testing@delphi-jedi.org <mailto:jcl-testing@delphi-jedi.org>
      This email account should not be used for support requests. If you
      need support please use either the newsgroups or the mailing list.
    * If you want to keep up to date about JCL then you can join the JCL
      mailing list by going to http://www.egroups.com/group/JEDI-JCL You
      can also use this list to voice your opinion, comments or suggestions.
    * If you prefer a newsgroup over a mailing list please join us at
      news://forums.talkto.net/jedi.jcl. The newsgroup is the point
      where you can discuss the JCL with other users and with the team
      itself.


      Issue Tracking

An issue tracking tool can be accessed via ('Code Library' category):

http://homepages.borland.com/jedi/issuetracker/

The general rule is: *If you want to get a bug fixed you need to log it!*

The JEDI issue tracker is based up on the Mantis BugTracker Open Source
project. More background information about it is available on its
homepage http://mantisbt.sourceforge.net

Please be aware that you are allowed there to enter feature request and
code donations as well.


      Debug Extension for JclDebug unit

The examples\vcl\debugExtension folder contains IDE expert which assists
to insert JCL Debug information into executable files. This can be
useful when use source location routines from JclDebug unit. These
routines needs some kind of special information to be able provide
source location for given address in the process. Currently there are
four options to get it work:

   1. Generate and deploy MAP file with your executable file. The file
      is generated by the linker. It needs to be set in Project|Options
      dialog -> Linker page, Detailed checkbox.
   2. Generate and deploy JDBG file file with your executable file. This
      is binary file based on MAP file but its size is typically about
      12% of original MAP file. You can generate it by MapToJdbg tool in
      jcl\examples\vcl\tools folder. The advantage over MAP file is
      smaller size and better security of the file content because it is
      not a plain text file and it also contains a checksum.
   3. Generate Borland TD32 debug symbols. These symbols are stored
      directly in the executable file but usually adds several megabytes
      so the file is very large. The advantage is you don't have to
      deploy any other file and it is easy to generate it by checking
      Include TD32 debug info in Linker option page.
   4. Insert JCL Debug info into executable file by the IDE expert. The
      size of added data is similar to JDBG file but it will be inserted
      directly into the executable file. This is probably best option
      because it combines small size of included data and no requirement
      of deploying additional files. In case you use this option you
      need install the JclDebugIde expert.

The IDE expert will add new item to IDE Project menu. For Delphi 5, 6
and 7 it adds 'Insert JCL Debug data' check item at the end of the
Project menu. When the item is checked, everytime the project is
compiled by one of following commands: Compile, Build, Compile All
Projects, Build All Projects or Run necessary JCL debug data are
automatically inserted into the executable. Moreover, for Build and
Build All commands dialog with detailed information of size of these
data will be displayed.

You can generate those debug data for packages and libraries as well
using the expert. Each executable file in the project can use different
option from those listed above. It is not necessary to generate any
debug data for Borland runtime packages because the source location code
can use names of exported functions to get procedure or method name. To
get line number information for Borland RTL and VCL/CLX units you have
to check Use Debug DCUs checkbox in Project|Options dialog -> Compiler
tab. Unfortunately it is not possible to get line number information for
Borland runtime packages because Borland does not provide detailed MAP
files for them so you get procedure or method name only.

In case you have more than one data source for an executable file by an
accident the best one is chosen in following order:

   1. JCL Debug data in the executable file
   2. JDBG file
   3. Borland TD32 symbols
   4. MAP file
   5. Library or Borland package exports

It is also possible to insert JCL debug data programmatically to the
executable file by using MakeJclDbg command line tool in
jcl\examples\vcl\delphitools folder. You can study included makefiles
which uses this tool for building delphitools examples.

To help using JclDebug exceptional stack tracking in application simple
dialog is provided in jcl\examples\debugextension\dialog folder. The
dialog replaces standard dialog displayed by VCL or CLX application when
an unhandled exception occurs. It has additional Detailed button showing
the stack, list of loaded modules and other system information. By
adding the dialog to the application exceptional stack tracking code is
automatically initialized so you don't have to care about it. You can
also turn on logging to text file by setting the Tag property of the
dialog to '1'. There is also version for CLX (ClxExceptDlg) but it works
on Windows only. These dialogs are intended to be added to Object
Repository.

*Short description of getting the JclDebug functionality in your project:*

   1. Close all running instances of Delphi
   2. Install JCL and IDE experts by the JCL Installer
   3. Run Delphi IDE and open your project
   4. Remove any TApplication.OnException handlers from your project (if
      any).
   5. Add new Exception Dialog by selecting File | New | Other ... |
      Dialogs tab, Select 'Exception Dialog' or 'Exception Dialog with
      Send' icon, Click OK button, Save the form (use
      ExceptionDialog.pas name, for example)
   6. Check Project | Insert JCL Debug data menu item
   7. Do Project | Build


      Makefiles

In order to compile examples and tools by one command we provide makefiles.

To use them, cd into the jcl/examples sub directory and at the command
prompt, type

> make

It should start to compile the covered projects using most recent
version of compiler from installed Delphi versions. All executable files
will be created in jcl/bin directory.


      Version Control

To always have access to the most recent changes in the JCL, you should
install a CVS client (we recommend TortoiseCVS and WinCVS) and download
the CVS repository files to your computer. With the CVS client, you can
update your local repository at any time.

For more instructions on how to set up CVS and use it with JVCL, see the
CVS instruction page
<http://sourceforge.net/docman/display_doc.php?docid=14033&group_id=1>.
You can also access the CVS repository via the web
<http://cvs.sourceforge.net/viewcvs.py/jcl/>.


      Downloads

Jedi Code Library: File List on SourceForge:

http://sourceforge.net/project/showfiles.php?group_id=47514


      Getting involved in JCL development

If you want to help out making JCL better or bigger or just plain
cooler, there are several ways in which you can help out. Here are some
of the things we need your help on:

    * Donate source code
    * Donate time writing help
    * Donate time writing demos
    * Donate time fixing bugs

JCL accepts donations from developers as long as the source fullfills
the requirements set up by the JEDI and JCL teams. To read more about
these requirements, visit the page http://homepages.borland.com/jedi/jcl

You can also donate your time by writing help for the source already in
JCL. We currently use Doc-o-Matic to create the finished help files but
the actual help sources are plain text files in a simple to understand
format. We can provide you with auto-generated templates with all
classes, properties, types etc already inserted. The "only" thing left
to do is fill in the actual help text for the help items. If you are
interested in writing help, contact us.

If you want to help fix bugs in JCL, go to Mantis and check the bug
report there. You can post replies as well as fixes directly in the bug
report. One of the JCL developers will pick up the report/fix and update
the CVS repository if the fix is satisfactory. If you report and fix a
lot of bugs, you might even get developer access to CVS so you can
update the JCL files directly.


      Fixed bugs

The following bugs have been fixed since the JCL 1.90 release

Mantis entry # 	Category 	Description
#0000028	JclMath	Feature Request: Checksums xor
#0000033	JclRegistry	New Functions for Read/Write Widestrings to the
Registry
#0000080	JclHookExcept	Exception hooking may cause crash during
finalization of the app
#0000177	JclStrings	Feature Request: CharFirstPos and CharLastPos
#0000186	JclSysInfo	Exception in GetBiosName and GetBiosExtendedInfo
#0000203	Documentation	StrMatches function is case-sensitive
#0000236	JclDateTime	jclDateTime with Indiana time zone
#0000278	Documentation	TDllVersion: wrong unit
#0000297	Miscellanous	installation error in xp
#0000302	Examples	BLS not found
#0000322	Installation	JediInstaller.exe inoperable when using large fonts
#0000402	JclDateTime	DateTimeToLocalDateTime incorrectly converts during
non-DST
#0000403	JclDateTime	DateTimeToLocalDateTime and LocalDateTimeToDateTime
don't include Standard Time Bias
#0000406	JclMime	Potential Access Violation in MimeDecode
#0000413	Miscellanous	Installation of JCL/JVCL using JEDI Installer
failing with Delphi 6 Personal
#0000417	JclPrint	Get the default windows printer
#0000521	JclSvcCtrl	TJclSCManager raises exception when free'd
#0000547	JclSysInfo	GetAPMBatteryFlag should return a SET of TAPMBatteryFlag
#0000848	JclSvcCtrl	class/method TJclSCManager.Refresh causes range
check error
#0000898	JclLanMan	GetLocalGroups and GetGlobalGroups crashes
#0000923	JclShell	ShellRunControlPanel dies not work under Win98
#0000924	Documentation	'IsWinXP' missing in Help
#0000947	JclStrings	StrBetween not working as expected
#0001045	JclDateTime	DateTimeToLocalDateTime function conversion bug
#0001060	JclStrings	Problem with extraction string between two same chars.
#0001115	JclMultimedia	TJclMultimedia Timer Elapsed function bug
#0001119	JclStrings	StrIsNumber, CharIsNumber any good?
#0001239	Examples	Error in RTTIDemoMain
#0001240	Examples	Error in RTTIDemoMain
#0001241	Examples	Error in RTTIDemoMain
#0001268	JclStatistics	function Median is wrong
#0001317	Installation	JclSysInfo.pas(2083): Undefinierter Bezeichner:
'TAsnObjectIdentifier'
#0001487	JclMime	MimeDecodeString (undefined result of the function)
#0001513	JclGraphics	ApplyLUT reverses params to CheckParams
#0001603	Installation	Access violation in installer
#0001606	Installation	EAccessViolation during installation of JCL 1.90
#0001668	Installation	Error installing JCL1.90-Build1497 on D5
#0001711	Installation	QJediInstaller gets Access Violation

The following bugs have been fixed (among others) since the JCL 1.22 release

Mantis entry # 	Category 	Description
#0000041	JclStrings	Todo Item not yet done: StrContainsChars
#0000046		TJclPeMapImgHooks.ReplaceImport does not work for host process
#0000056		JCLDebug is not catching exceptions that are raised from DLLs
#0000196	JclDebug	Display Call stack on exception box
#0000197	Miscellanous	all files are readonly
#0000248	JclSysUtils	JclSysUtils.StrToBoolean does not accept 'Y', 'N',
'T', 'F'
#0000253		StrStripNonNumbersChars
#0000271		GetSizeOfFile(const FileName: string) does not work for size > 4GB
#0000278		TDllVersion: wrong unit
#0000303		RunningProcessesList: FullPath option does not work any more
#0000321		Use TStrings instead of TStringList in procedure
StringListCustomSort
#0000342		Memory leak
#0000349		ModFloat(-2, 2) gives 2, which is wrong
#0000365	JclSysInfo	GetShellProcessName does not work on Win9x systems
#0000401	JclSysInfo	ERangeError in GetCPUSpeed
#0000444	Documentation	ASCIIZ and Unicode
#0000445	Documentation	ASCIIZ and Unicode
#0000512	JclSysInfo	RunningProcessesList does not return full path on XP
#0000533		RunningProcessesList does not return full path on XP
#0000605	JclMAPI	AddressType reported as the addressname
#0000632	JclPEImage	TJclPeImage.StampToDateTime simplification
#0000632	JclPEImage	TJclPeImage.StampToDateTime simplification
#0000640	JclStrings	JclStrings.pas - Compilation under Linux.
#0000670	JclSysInfo	JCLSYSINFO - GetMacAddresses does not fall back on
GetMacAddressesSnmp
#0000683	JclSysInfo	GetBIOSDate does not work on Win9x/Me systems
#0000718	JclSecurity	Missing JclBase in uses gives error in D5
#0000744	JclRTTI	JclRTTI causes access violation locating IsClass
#0000785	JclDateTime	SystemTimeToFileTime and DosDateTimeToFileTime do
nothing.
#0000789	JclSynch	ThreadID defined as Integer - fails on win95/98/ME
#0000801	JclSysInfo	JclSysInfo.GetCPUSpeed - division by zero
#0000845	JclSysInfo	GetWindowsVersion - Errors!!!
#0000856	JclSysUtils	SetVirtualMethod causes Access Violation
#0000868	JclStrings	ERangeError raised in StrReplaceCS
#0000872	JclStrings	Passing empty string to strReplace causes exception
#0000920	JclMAPI	Add addrress type for telex, at the moment only types
for smtp and fax
#0000921		Add addrress type for telex, at the moment only types for smtp
and fax
#0000929	JclSysInfo	User home directory in Win 2000/XP
#0000938	JclIniFiles	Changes o IniFiles not written
#0000961	Examples	StackTrace, Exception Handler
#0000967	JclDebug	CreateStrackList function
#0000977	JclFileUtils	AdvBuildFileList does not return files
#0000981	JclSysInfo	Division by zero in JclSysInfo
#0001004	JclFileUtils	AdvBuildFileList fails to match
#0001006	JclSysInfo	Division by 0 and runtime error 216
#0001007		GetCPUSpeed() crashes on Hyperthreading CPUs
#0001013	JclDateTime	JclDateTime & TIME_ZONE_ID_UNKNOWN
#0001024	JclSysInfo	TerminateApp calls CloseHandle even if OpenProcess fails
#0001074	JclSysInfo	Dr. Watson when GetCPUInfo is called
#0001109	JclFileUtils	TJclFileVersionInfo: add property ItemByName or
function GetItemByName
#0001112	JclGraphUtils	Linking JclGraphUtils GPFs under XP Home Edition
#0001118		type incompatible pointer assignment
#0001123	JclStrings	StrReplaceCI and StrSmartCase ignore national chars
#0001133	JclFileUtils	DelTree('') deletes c:*:* !!!
#0001137	JclStrings	StrMatches with a search of '*' won't match empty string
#0001147	JclStrings	Call of CharPos with Index=1 returns 2 if first char
of string do match
#0001156	JclStrings	Error in conversion
#0001167	JclSysInfo	EZeroDivide is raised in GetCPUSpeed
#0001185	Miscellanous	BCB is not defined for VER110
#0001226	JclFileUtils	Files without any attributes aren't returned by
AdvBuildFileList
#0001282	JclSysInfo	JCLSysInfo GetWindowsVersion fails on Windows 2003
#0001294		SetVirtualMethod sets method to wrong location
#0001295	JclSysInfo	DevideByZero Error

