; Script created by Andreas Hausladen (Andreas.Hausladen@gmx.de) for the JCL
;
; CONDITIONAL COMPILATION
;    Include_Binaries    Create an installer that can install a precompiled JCL
;    Include_Examples    Add the Examples directory to the installer (user can then select the component)
;    DEBUGGING           Development. Uses fast compression (script debugging)
;    Include_DelphiX     Include the binaries for Delphi X (X in 6..16)

#ifndef CmdLineBuild
#define JclRoot "..\Jcl"
#define JclLib "setupbuild\lib"
#define JclBpl "setupbuild\bpl"
#define JclHpp "setupbuild\hpp"
#define DEBUGGING
#endif

#define Include_SingleIDE
#define Include_Binaries
#define Include_Examples

#include "Settings.iss"

#define MyAppName "Jedi Code Library"
#define MyAppVerName "Jedi Code Library " + JclVersionStr
#define MyAppPublisher "JCL Team"
#define MyAppURL "http://jcl.sourceforge.net/"

;---------------------------------------------------
; Setup the preprocessor defines for the binary files
#ifdef Include_SingleIDE
#define JclLib6     JclLib
#define   JclBpl6   JclBpl
#define JclLib7     JclLib
#define   JclBpl7   JclBpl
#define JclLib9     JclLib
#define   JclBpl9   JclBpl
#define JclLib10    JclLib
#define   JclBpl10  JclBpl
#define   JclHpp10  JclHpp
#define JclLib11    JclLib
#define   JclBpl11  JclBpl
#define   JclHpp11  JclHpp
#define JclLib12    JclLib
#define   JclBpl12  JclBpl
#define   JclHpp12  JclHpp
#define JclLib14    JclLib
#define   JclBpl14  JclBpl
#define   JclHpp14  JclHpp
#define JclLib15    JclLib
#define   JclBpl15  JclBpl
#define   JclHpp15  JclHpp
#define JclLib16    JclLib
#define   JclBpl16  JclBpl
#define   JclHpp16  JclHpp
#define JclLib17    JclLib
#define   JclBpl17  JclBpl
#define   JclHpp17  JclHpp
#endif

;---------------------------------------------------

[Setup]
AppName={#MyAppName}
AppVerName={#MyAppVerName}
AppVersion={#JclVersionStr}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName=C:\DelphiComponents\JCL
DefaultGroupName=DelphiComponents\JEDI Code Library
DisableProgramGroupPage=no
;LicenseFile={#JclRoot}\help\MPL-1.1.html
OutputBaseFilename=JCLSetup
PrivilegesRequired=none
#ifdef DEBUGGING
Compression=zip/1
#else
Compression=lzma/ultra64
#endif
SolidCompression=yes
ShowLanguageDialog=auto
OptimizedChecks=yes


; for skin
#define MyWizardBottomImageFile "Skin\images\wizardbottom.bmp"
#define MyWizardButtonImageFile "Skin\images\button.bmp"
#define MyWizardImageFile "wizard.bmp"
#define MyWizardSmallImageFile "wizardsmall.bmp"
WizardImageFile=Skin\images\{#MyWizardImageFile}
WizardSmallImageFile=Skin\images\{#MyWizardSmallImageFile}
#include "Skin\isxskin.iss"

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "french"; MessagesFile: "compiler:Languages\French.isl"
Name: "german"; MessagesFile: "compiler:Languages\German.isl"

#ifdef Include_Binaries
[Types]
Name: "full"; Description: "Full installation"
Name: "compact"; Description: "Source only installation"
Name: "custom"; Description: "Custom installation"; Flags: iscustom
Name: "prefered"; Description: "Prefered installation"
#endif

[Components]
#ifdef Include_Examples
Name: "Examples"; Description: "Example projects"; Types: full;
#endif

#ifdef Include_Binaries

#include "IdeComponents.iss"

[Components]
; Package selection
Name: "Experts"; Description: "Register IDE Experts"; Types: full prefered
Name: "Experts\JclDebugExpert"; Description: "Debug Expert"; Types: full prefered
Name: "Experts\JclFavoriteFoldersExpert"; Description: "Favorite Folder Expert"; Types: full prefered
Name: "Experts\JclProjectAnalysisExpert"; Description: "Project Analysis Expert"; Types: full prefered
Name: "Experts\JclRepositoryExpert"; Description: "Repository Expert"; Types: full prefered
Name: "Experts\JclSIMDViewExpert"; Description: "SIMD-View Expert"; Types: full prefered
Name: "Experts\JclThreadNameExpert"; Description: "Thread Name Expert"; Types: full prefered
Name: "Experts\JclUsesExpert"; Description: "Uses Expert"; Types: full prefered
Name: "Experts\JclVersionControlExpert"; Description: "Version Control Expert"

#endif

[Dirs]
Name: "{app}\bin"

[Files]
Source: {#JclRoot}\*.bat; DestDir: "{app}"; Flags: ignoreversion
Source: {#JclRoot}\*.sh; DestDir: "{app}"; Flags: ignoreversion
Source: {#JclRoot}\*.txt; DestDir: "{app}"; Flags: ignoreversion
Source: {#JclRoot}\*.proj; DestDir: "{app}"; Flags: ignoreversion
Source: {#JclRoot}\devtools\*; DestDir: "{app}\devtools"; Excludes: ".svn,__history"; Flags: ignoreversion sortfilesbyextension recursesubdirs
Source: {#JclRoot}\docs\*; DestDir: "{app}\docs"; Excludes: ".svn,__history"; Flags: ignoreversion sortfilesbyextension recursesubdirs
Source: {#JclRoot}\experts\*; DestDir: "{app}\experts"; Excludes: ".svn,__history,*.~*,*.txt"; Flags: ignoreversion sortfilesbyextension recursesubdirs
Source: {#JclRoot}\install\*; DestDir: "{app}\install"; Excludes: ".svn,__history,*.~*,ISS,dcc32.cfg,*.cmd,*.local,*.identcache"; Flags: ignoreversion recursesubdirs sortfilesbyextension
Source: {#JclRoot}\lib\*; DestDir: "{app}\lib"; Excludes: ".svn,__history,*.dcu,*.obj,*.dcp,*.lib,*.bpi,*.dfm,*.res,*.txt"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#JclRoot}\packages\*; DestDir: "{app}\packages"; Excludes: ".svn,__history,*.drc,*.txt,*.identcache,*.local,*.~*,*.dcu"; Flags: ignoreversion sortfilesbyextension recursesubdirs
Source: {#JclRoot}\source\*; DestDir: "{app}\source"; Excludes: ".svn,__history,*.~*,*.hpp,*.txt"; Flags: ignoreversion sortfilesbyextension recursesubdirs
#ifdef Include_Examples
; SolidBreak
Source: {#JclRoot}\examples\*; DestDir: "{app}\examples"; Excludes: ".svn,__history,*.dcu,*.obj,*.exe,*.map,*.bpl,*.dcp,*.~*,*.drc,*.local"; Components: "Examples"; Flags: ignoreversion recursesubdirs sortfilesbyextension solidbreak
#endif

#ifdef Include_Binaries
#ifdef Include_Delphi6
; SolidBreak;
Source: {#JclLib6}\*; DestDir: "{app}\lib\d6"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi6"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl6}\*; DestDir: "{code:GetDelphiBplDir|6}"; Components: "IDE\Delphi6"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
#endif
#ifdef Include_Delphi7
; SolidBreak;
Source: {#JclLib7}\*; DestDir: "{app}\lib\d7"; Excludes: ".svn,__history,*.txt"; Components: "IDE\Delphi7"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl7}\*; DestDir: "{code:GetDelphiBplDir|7}"; Components: "IDE\Delphi7"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
#endif
#ifdef Include_Delphi9
; SolidBreak;
Source: {#JclLib9}\*; DestDir: "{app}\lib\d9"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi9"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl9}\*; DestDir: "{code:GetDelphiBplDir|9}"; Components: "IDE\Delphi9"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
#endif
#ifdef Include_Delphi10
; SolidBreak;
Source: {#JclLib10}\*; DestDir: "{app}\lib\d10"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi10"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl10}\*; DestDir: "{code:GetDelphiBplDir|10}"; Components: "IDE\Delphi10"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#JclHpp10}\*; DestDir: "{code:GetHPPDir|10}"; Components: "IDE\Delphi10"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi11
; SolidBreak;
Source: {#JclLib11}\*; DestDir: "{app}\lib\d11"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi11"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl11}\*; DestDir: "{code:GetDelphiBplDir|11}"; Components: "IDE\Delphi11"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#JclHpp11}\*; DestDir: ""{app}\include\d11"; Components: "IDE\Delphi11"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi12
; SolidBreak;
Source: {#JclLib12}\*; DestDir: "{app}\lib\d12"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi12"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl12}\*; DestDir: "{code:GetDelphiBplDir|12}"; Components: "IDE\Delphi12"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#JclHpp12}\*; DestDir: ""{app}\include\d12"; Components: "IDE\Delphi12"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi14
; SolidBreak;
Source: {#JclJcl14}\*; DestDir: "{app}\lib\d14"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi14"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl14}\*; DestDir: "{code:GetDelphiBplDir|14}"; Components: "IDE\Delphi14"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#JclHpp14}\*; DestDir: ""{app}\include\d14"; Components: "IDE\Delphi14"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi15
; SolidBreak;
Source: {#JclLib15}\*; DestDir: "{app}\lib\d15"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi15"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl15}\*; DestDir: "{code:GetDelphiBplDir|15}"; Components: "IDE\Delphi15"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#JclHpp15}\*; DestDir: "{app}\include\d15"; Components: "IDE\Delphi15"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi16
; SolidBreak;
Source: {#JclLib16}\*; DestDir: "{app}\lib\d16"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi16"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl16}\*; DestDir: "{code:GetDelphiBplDir|16}"; Components: "IDE\Delphi16"; Flags: ignoreversion sortfilesbyextension
Source: {#JclBpl16}\Win64\*; DestDir: "{code:GetDelphiBplDir|16}\Win64"; Components: "IDE\Delphi16"; Flags: ignoreversion sortfilesbyextension
Source: {#JclHpp16}\*; DestDir: "{app}\include\d16"; Components: "IDE\Delphi16"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi17
; SolidBreak;
Source: {#JclLib17}\*; DestDir: "{app}\lib\d17"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi17"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl17}\*; DestDir: "{code:GetDelphiBplDir|17}"; Components: "IDE\Delphi17"; Flags: ignoreversion sortfilesbyextension
Source: {#JclBpl17}\Win64\*; DestDir: "{code:GetDelphiBplDir|17}\Win64"; Components: "IDE\Delphi17"; Flags: ignoreversion sortfilesbyextension
Source: {#JclHpp17}\*; DestDir: "{app}\include\d17"; Components: "IDE\Delphi17"; Flags: ignoreversion sortfilesbyextension
#endif

#endif

; only source code => execute Jedi Installer
[Run]
Filename: {app}\install.bat; Description: "Execute JEDI Installer"; Flags: postinstall shellexec; Check: IsSourceInstall;

#ifdef Include_Binaries
[Registry]
#ifdef Include_Delphi6
; Delphi 6
Root: HKCU; Subkey: "{code:GetDelphiRegKey|6}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|6}; Components: "IDE\Delphi6"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|6}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d6; Components: "IDE\Delphi6"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|6}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi6"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|6}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi6"; Flags: uninsdeletevalue;
#endif
#ifdef Include_Delphi7
; Delphi 7
Root: HKCU; Subkey: "{code:GetDelphiRegKey|7}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|7}; Components: "IDE\Delphi7"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|7}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d7; Components: "IDE\Delphi7"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|7}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi7"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|7}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi7"; Flags: uninsdeletevalue;
#endif
#ifdef Include_Delphi9
; Delphi 2005
Root: HKCU; Subkey: "{code:GetDelphiRegKey|9}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|9}; Components: "IDE\Delphi9"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|9}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d9; Components: "IDE\Delphi9"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|9}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi9"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|9}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi9"; Flags: uninsdeletevalue;
#endif
#ifdef Include_Delphi10
; Delphi 2006
Root: HKCU; Subkey: "{code:GetDelphiRegKey|10}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|10}; Components: "IDE\Delphi10"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|10}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d10; Components: "IDE\Delphi10"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|10}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi10"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|10}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi10"; Flags: uninsdeletevalue;
#endif
#ifdef Include_Delphi11
; Delphi 2007
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|11}; Components: "IDE\Delphi11"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d11; Components: "IDE\Delphi11"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi11"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi11"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi11";
#endif
#ifdef Include_Delphi12
; Delphi 2009
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|12}; Components: "IDE\Delphi12"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d12; Components: "IDE\Delphi12"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi12"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi12"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi12";
#endif
#ifdef Include_Delphi14
; Delphi 2010
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|14}; Components: "IDE\Delphi14"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d14; Components: "IDE\Delphi14"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi14"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi14"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi14";
#endif
#ifdef Include_Delphi15
; Delphi XE
Root: HKCU; Subkey: "{code:GetDelphiRegKey|15}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|15}; Components: "IDE\Delphi15"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|15}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d15; Components: "IDE\Delphi15"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|15}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi15"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|15}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi15"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|15}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi15";
#endif
#ifdef Include_Delphi16
; Delphi XE2
Root: HKCU; Subkey: "{code:GetDelphiRegKey|16}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|16}; Components: "IDE\Delphi16"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|16}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d16; Components: "IDE\Delphi16"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|16}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi16"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|16}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi16"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|16}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi16";
#endif
#ifdef Include_Delphi17
; Delphi XE3
Root: HKCU; Subkey: "{code:GetDelphiRegKey|17}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|17}; Components: "IDE\Delphi17"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|17}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d17; Components: "IDE\Delphi17"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|17}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi17"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|17}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi17"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|17}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi17";
#endif

#endif



[UninstallDelete]
Type: files; Name: "{app}\bin\JediInstaller.exe"
Type: files; Name: "{app}\bin\JCL-install.ini"
Type: files; Name: "{app}\bin\*.log"
Type: files; Name: "{app}\source\*.hpp"
Type: files; Name: "{app}\source\*.~*"
Type: files; Name: "{app}\source\common\*.hpp"
Type: files; Name: "{app}\source\vcl\*.hpp"
Type: files; Name: "{app}\source\windows\*.hpp"
Type: files; Name: "{app}\source\common\*.dcu"
Type: files; Name: "{app}\source\vcl\*.dcu"
Type: files; Name: "{app}\source\windows\*.dcu"
; lib\Delphi 6
Type: files; Name: "{app}\lib\d6\*"
Type: files; Name: "{app}\lib\d6\debug\*"
Type: files; Name: "{code:GetDelphiBplDir|6}\Jcl*.~bpl";
; lib\Delphi 7
Type: files; Name: "{app}\lib\d7\*"
Type: files; Name: "{app}\lib\d7\debug\*"
Type: files; Name: "{code:GetDelphiBplDir|7}\Jcl*.~bpl";
; lib\Delphi 2005
Type: files; Name: "{app}\lib\d9\*"
Type: files; Name: "{app}\lib\d9\debug\*"
Type: files; Name: "{code:GetDelphiBplDir|9}\Jcl*.~bpl";
; lib\Delphi/C++Builder 2006
Type: files; Name: "{app}\lib\d10\*"
Type: files; Name: "{app}\lib\d10\debug\*"
Type: files; Name: "{code:GetDelphiBplDir|10}\Jcl*.~bpl";
; lib\Delphi/C++Builder 2007
Type: files; Name: "{app}\lib\d11\*"
Type: files; Name: "{app}\lib\d11\debug\*"
Type: files; Name: "{app}\include\d11\*"
Type: files; Name: "{code:GetDelphiBplDir|11}\Jcl*.~bpl";
; lib\Delphi/C++Builder 2009
Type: files; Name: "{app}\lib\d12\*"
Type: files; Name: "{app}\lib\d12\debug\*"
Type: files; Name: "{app}\include\d12\*"
Type: files; Name: "{code:GetDelphiBplDir|12}\Jcl*.~bpl";
; lib\Delphi/C++Builder 2010
Type: files; Name: "{app}\lib\d14\*"
Type: files; Name: "{app}\lib\d14\debug\*"
Type: files; Name: "{app}\include\d14\*"
Type: files; Name: "{code:GetDelphiBplDir|14}\Jcl*.~bpl";
; lib\Delphi/C++Builder XE
Type: files; Name: "{app}\lib\d15\*"
Type: files; Name: "{app}\lib\d15\debug\*"
Type: files; Name: "{app}\include\d15\*"
Type: files; Name: "{code:GetDelphiBplDir|15}\Jcl*.~bpl";
; lib\Delphi/C++Builder XE2
Type: files; Name: "{app}\lib\d16\win32\*"
Type: files; Name: "{app}\lib\d16\win32\debug\*"
Type: files; Name: "{app}\lib\d16\win64\*"
Type: files; Name: "{app}\lib\d16\win64\debug\*"
Type: files; Name: "{app}\include\d16\*"
Type: files; Name: "{code:GetDelphiBplDir|16}\Jcl*.~bpl";
; lib\Delphi/C++Builder XE3
Type: files; Name: "{app}\lib\d17\win32\*"
Type: files; Name: "{app}\lib\d17\win32\debug\*"
Type: files; Name: "{app}\lib\d17\win64\*"
Type: files; Name: "{app}\lib\d17\win64\debug\*"
Type: files; Name: "{app}\include\d17\*"
Type: files; Name: "{code:GetDelphiBplDir|17}\Jcl*.~bpl";
[Icons]
Name: "{group}\{cm:ProgramOnTheWeb,{#MyAppName}}"; Filename: "{#MyAppURL}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"

#include "ComponentInstallerScript.iss"

[Code]
// callbacks for the ComponentInstaller

procedure UserRegisterComponents(Components: TStrings);
begin
end;

procedure UserUnregisterComponents(Components: TStrings);
// uninstall all JCL experts, not only the one that the user had selected
// during the installation. The user could have started the JediInstaller
// or have added additional designtime packages by hand.
var
  IdeList: TStrings;
  IdeIndex: Integer;
  IdeKind: TIdeKind;
  Version: Integer;
begin
{  // Uninstall from all IDEs ?
  for Version := 6 to LastInstalledIDEVersionNumber do
    UninstallExpertsPrefixed(ikDelphi, Version, 'Jcl');
  for Version := 6 to 6 do
    UninstallExpertsPrefixed(ikBCB, Version, 'Jcl');}

  IdeList := TStringList.Create;
  try
    GetSelectedList(IdeList, 'IDE', Components);
    // unregister per IDE
    for IdeIndex := 0 to IdeList.Count - 1 do
    begin
      ExtractIdeInfo(IdeList[IdeIndex], IdeKind, Version);
      UninstallExpertsPrefixed(IdeKind, Version, 'Jcl');
    end;
  finally
    IdeList.Free;
  end;
end;

function MapExpert(IdeKind: TIdeKind; Version: Integer; const ExpertName: string): string;
begin
  if StartsText('Jcl', ExpertName) then
  begin
    if Version < 9 then
    begin
      case IdeKind of
        ikDelphi:
          Result := GetDelphiBplDir(IntToStr(Version)) + '\' + ExpertName + 'DLLd' + IntToStr(Version) + '0.dll';
        ikBCB:
          Result := GetBCBBplDir(IntToStr(Version)) + '\' + ExpertName + 'DLLc' + IntToStr(Version) + '0.dll';
      end;
    end
    else
      Result := GetDelphiBplDir(IntToStr(Version)) + '\' + ExpertName  + IntToStr(Version) + '0.bpl';
  end;
end;

function MapDesignPackage(IdeKind: TIdeKind; Version: Integer; const PackageName: string): string;
begin
  Result := '';
end;

procedure GetSearchPaths(IdeKind: TIdeKind; Version: Integer; var SearchPaths, DebugPaths, BrowsePaths, IncludePaths: string);
var
  LibDir, AppDir: string;
begin
  AppDir := ExpandConstant('{app}');
  SearchPaths := '';
  DebugPaths := '';
  BrowsePaths := '';
  IncludePaths := '';
  case IdeKind of
    ikDelphi:
	  if Version >= 16 then // XE2+
        LibDir := AppDir + '\lib\d' + IntToStr(Version) + '\$(Platform)' // is replaced by Win32/Win64 in the CompInstall.dll
	  else
        LibDir := AppDir + '\lib\d' + IntToStr(Version);
    ikBCB:
      LibDir := AppDir + '\lib\c' + IntToStr(Version);
  else
    Exit;
  end;
  
  SearchPaths := LibDir + ';' + AppDir + '\source\Include';
  DebugPaths := LibDir + '\debug';
  BrowsePaths := AppDir + '\source\common;' + AppDir + '\source\vcl;' + AppDir + '\source\windows';
  if Version >= 11 then
    IncludePaths := ExpandConstant('{app}') + '\include\d' + IntToStr(Version)
  else if Version = 10 then
    IncludePaths := GetHPPDir(IntToStr(Version));
end;

// events

function InitializeSetup(): Boolean;
begin
  Result := InitComponentInstaller;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssPostInstall  then
    RegisterComponents;
end;

function InitializeUninstall(): Boolean;
begin
  Result := InitComponentUninstaller;
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
begin
  if CurUninstallStep = usUninstall then
    UnregisterComponents;
end;

// Skin

procedure CurPageChanged(CurPageID: Integer);
begin
  // update calls for skin
  UpdateButton(WizardForm.BackButton, bidBack);
  UpdateButton(WizardForm.NextButton, bidNext);
  UpdateButton(WizardForm.CancelButton, bidCancel);
end;

procedure InitializeWizard();
begin
  // initialize call for skin
  InitializeSkin;
end;

