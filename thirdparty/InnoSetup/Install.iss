; Script created by Andreas Hausladen (Andreas.Hausladen@gmx.de) for the JCL
;
; CONDITIONAL COMPILATION
;    Include_Binaries    Create an installer that can install a precompiled JCL
;    Include_Examples    Add the Examples directory to the installer (user can then select the component)
;    DEBUGGING           Development. Will only use Delphi 5 BPLs as files with a fast compression (script debugging)
;    Include_DelphiX     Include the binaries for Delphi X (X in 6..15)
;    Include_BCBX        Include the binaries for C++Builder 6

#define Include_SingleIDE
#define Include_Binaries
#define Include_Examples
;#define DEBUGGING

#include "Settings.iss"

#ifdef Include_SingleIDE
#endif
#define MyAppName "Jedi Code Library"
#define MyAppVerName "Jedi Code Library " + JclVersionStr
#define MyAppPublisher "JCL Team"
#define MyAppURL "http://jcl.sourceforge.net/"
#define downloadurl "http://jcl.sourceforge.net/websetup/jcl"

#ifdef DEBUGGING
 #define Include_SingleIDE
 #define Include_Delphi15
 #undef Include_Examples
#endif

#ifdef Include_Binaries
 #ifndef Include_SingleIDE
  #define Include_Delphi6
  #define Include_Delphi7
  #define Include_Delphi9
  #define Include_Delphi10
  #define Include_Delphi11
  #define Include_Delphi12
  #define Include_Delphi14
  #define Include_Delphi15
 #endif
#endif


;---------------------------------------------------
#ifndef SourceDirectoriesDefines
#include "SourceDirectories.iss"
#endif

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
;WebSetupUpdateURL={#downloadurl}


// for skin
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

[Packages]
Name: delphi6; Description: "Delphi 6 binary files"; Source: "{#downloadurl}/jcldelphi6.isz"; Flags: localcopy;
Name: delphi7; Description: "Delphi 7 binary files"; Source: "{#downloadurl}/jcldelphi7.isz"; Flags: localcopy;
Name: delphi9; Description: "Delphi 2005 binary files"; Source: "{#downloadurl}/jcldelphi9.isz"; Flags: localcopy;
Name: delphi10; Description: "Delphi/C++Builder 2006 binary files"; Source: "{#downloadurl}/jcldelphi10.isz"; Flags: localcopy;
Name: delphi11; Description: "Delphi/C++Builder 2007 binary files"; Source: "{#downloadurl}/jcldelphi11.isz"; Flags: localcopy;
Name: delphi12; Description: "Delphi/C++Builder 2009 binary files"; Source: "{#downloadurl}/jcldelphi12.isz"; Flags: localcopy;
Name: delphi14; Description: "Delphi/C++Builder 2010 binary files"; Source: "{#downloadurl}/jcldelphi14.isz"; Flags: localcopy;
Name: delphi15; Description: "Delphi/C++Builder XE binary files"; Source: "{#downloadurl}/jcldelphi15.isz"; Flags: localcopy;

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
Source: {#JclRoot}\examples\*; DestDir: "{app}\examples"; Excludes: ".svn,__history,*.dcu,*.obj,*.exe,*.bpl,*.dcp,*.~*"; Components: "Examples"; Flags: ignoreversion recursesubdirs sortfilesbyextension solidbreak
#endif

#ifdef Include_Binaries
#ifdef Include_Delphi6
; SolidBreak;
Source: {#JclRoot}\lib\d6\*; DestDir: "{app}\lib\d6"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi6"; Package: delphi6; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#Delphi6Bpl}\Jcl*.*; DestDir: "{code:GetDelphiBplDir|6}"; Components: "IDE\Delphi6"; Package: delphi6; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
#endif
#ifdef Include_Delphi7
; SolidBreak;
Source: {#JclRoot}\lib\d7\*; DestDir: "{app}\lib\d7"; Excludes: ".svn,__history,*.txt"; Components: "IDE\Delphi7"; Package: delphi7; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#Delphi7Bpl}\Jcl*.*; DestDir: "{code:GetDelphiBplDir|7}"; Components: "IDE\Delphi7"; Package: delphi7; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
#endif
#ifdef Include_Delphi9
; SolidBreak;
Source: {#JclRoot}\lib\d9\*; DestDir: "{app}\lib\d9"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi9"; Package: delphi9; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#Delphi9Bpl}\Jcl*9*.*; DestDir: "{code:GetDelphiBplDir|9}"; Components: "IDE\Delphi9"; Package: delphi9; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
#endif
#ifdef Include_Delphi10
; SolidBreak;
  #ifdef JclHpp
Source: {#JclLib}\*.*; DestDir: "{app}\lib\d10"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi10"; Package: delphi10; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl}\*.*; DestDir: "{code:GetDelphiBplDir|10}"; Components: "IDE\Delphi10"; Package: delphi10; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#JclHpp}\*.*; DestDir: "{code:GetHPPDir|10}"; Components: "IDE\Delphi10"; Package: delphi10; Flags: ignoreversion sortfilesbyextension
  #else
Source: {#JclRoot}\lib\d10\*; DestDir: "{app}\lib\d10"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi10"; Package: delphi10; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#Delphi10Bpl}\Jcl*10*.*; DestDir: "{code:GetDelphiBplDir|10}"; Components: "IDE\Delphi10"; Package: delphi10; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#Delphi10Hpp}\Jcl*.hpp; DestDir: "{code:GetHPPDir|10}"; Components: "IDE\Delphi10"; Package: delphi10; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
  #endif
#endif
#ifdef Include_Delphi11
; SolidBreak;
  #ifdef JclHpp
Source: {#JclLib}\*.*; DestDir: "{app}\lib\d11"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi11"; Package: delphi11; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl}\*.*; DestDir: "{code:GetDelphiBplDir|11}"; Components: "IDE\Delphi11"; Package: delphi11; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#JclHpp}\*.*; DestDir: "{code:GetHPPDir|11}"; Components: "IDE\Delphi11"; Package: delphi11; Flags: ignoreversion sortfilesbyextension
  #else
Source: {#JclRoot}\lib\d11\*; DestDir: "{app}\lib\d11"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi11"; Package: delphi11; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#Delphi11Bpl}\Jcl*.*; DestDir: "{code:GetDelphiBplDir|11}"; Components: "IDE\Delphi11"; Package: delphi11; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#Delphi11Hpp}\Jcl*.hpp; DestDir: "{code:GetHPPDir|11}"; Components: "IDE\Delphi11"; Package: delphi11; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
  #endif
#endif
#ifdef Include_Delphi12
; SolidBreak;
  #ifdef JclHpp
Source: {#JclLib}\*.*; DestDir: "{app}\lib\d12"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi12"; Package: delphi12; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl}\*.*; DestDir: "{code:GetDelphiBplDir|12}"; Components: "IDE\Delphi12"; Package: delphi12; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#JclHpp}\*.*; DestDir: "{code:GetHPPDir|12}"; Components: "IDE\Delphi12"; Package: delphi12; Flags: ignoreversion sortfilesbyextension
  #else
Source: {#JclRoot}\lib\d12\*; DestDir: "{app}\lib\d12"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi12"; Package: delphi12; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#Delphi12Bpl}\Jcl*.*; DestDir: "{code:GetDelphiBplDir|12}"; Components: "IDE\Delphi12"; Package: delphi12; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#Delphi12Hpp}\Jcl*.hpp; DestDir: "{code:GetHPPDir|12}"; Components: "IDE\Delphi12"; Package: delphi12; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
  #endif
#endif
#ifdef Include_Delphi14
; SolidBreak;
  #ifdef JclHpp
Source: {#JclJcl}\*.*; DestDir: "{app}\lib\d14"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi14"; Package: delphi14; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl}\*.*; DestDir: "{code:GetDelphiBplDir|14}"; Components: "IDE\Delphi14"; Package: delphi14; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#JclHpp}\*.*; DestDir: "{code:GetHPPDir|14}"; Components: "IDE\Delphi14"; Package: delphi14; Flags: ignoreversion sortfilesbyextension
  #else
Source: {#JclRoot}\lib\d14\*; DestDir: "{app}\lib\d14"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi14"; Package: delphi14; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#Delphi14Bpl}\Jcl*.*; DestDir: "{code:GetDelphiBplDir|14}"; Components: "IDE\Delphi14"; Package: delphi14; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#Delphi14Hpp}\Jcl*.hpp; DestDir: "{code:GetHPPDir|14}"; Components: "IDE\Delphi14"; Package: delphi14; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
  #endif
#endif
#ifdef Include_Delphi15
; SolidBreak;
  #ifdef JclHpp
Source: {#JclLib}\*.*; DestDir: "{app}\lib\d15"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi15"; Package: delphi15; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl}\*.*; DestDir: "{code:GetDelphiBplDir|15}"; Components: "IDE\Delphi15"; Package: delphi15; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#JclHpp}\*.*; DestDir: "{code:GetHPPDir|15}"; Components: "IDE\Delphi15"; Package: delphi15; Flags: ignoreversion sortfilesbyextension
  #else
Source: {#JclRoot}\lib\d15\*; DestDir: "{app}\lib\d15"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi15"; Package: delphi15; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#Delphi15Bpl}\Jcl*.*; DestDir: "{code:GetDelphiBplDir|15}"; Components: "IDE\Delphi15"; Package: delphi15; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#Delphi15Hpp}\Jcl*.hpp; DestDir: "{code:GetHPPDir|15}"; Components: "IDE\Delphi15"; Package: delphi15; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
  #endif
#endif

#endif

; only source code => execute Jedi Installer
[Run]
Filename: {app}\install.bat; Description: "Execute JEDI Installer"; Flags: postinstall shellexec; Check: IsSourceInstall;

#ifdef Include_Binaries
[Registry]
#ifdef Include_Delphi6
; Delphi 6
Root: HKCU; Subkey: "{code:GetDelphiRegKey|6}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|6}; Components: "IDE\Delphi6"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(6)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|6}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d6; Components: "IDE\Delphi6"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(6)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|6}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi6"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(6)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|6}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi6"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(6)
#endif
#ifdef Include_Delphi7
; Delphi 7
Root: HKCU; Subkey: "{code:GetDelphiRegKey|7}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|7}; Components: "IDE\Delphi7"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(7)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|7}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d7; Components: "IDE\Delphi7"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(7)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|7}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi7"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(7)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|7}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi7"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(7)
#endif
#ifdef Include_Delphi9
; Delphi 2005
Root: HKCU; Subkey: "{code:GetDelphiRegKey|9}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|9}; Components: "IDE\Delphi9"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(9)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|9}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d9; Components: "IDE\Delphi9"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(9)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|9}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi9"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(9)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|9}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi9"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(9)
#endif
#ifdef Include_Delphi10
; Delphi 2006
Root: HKCU; Subkey: "{code:GetDelphiRegKey|10}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|10}; Components: "IDE\Delphi10"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(10)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|10}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d10; Components: "IDE\Delphi10"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(10)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|10}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi10"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(10)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|10}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi10"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(10)
#endif
#ifdef Include_Delphi11
; Delphi 2007
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|11}; Components: "IDE\Delphi11"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(11)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d11; Components: "IDE\Delphi11"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(11)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi11"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(11)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi11"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(11)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi11"; Check: IsDelphiInstalled(11)
#endif
#ifdef Include_Delphi12
; Delphi 2009
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|12}; Components: "IDE\Delphi12"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(12)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d12; Components: "IDE\Delphi12"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(12)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi12"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(12)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi12"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(12)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi12"; Check: IsDelphiInstalled(12)
#endif
#ifdef Include_Delphi14
; Delphi 2010
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|14}; Components: "IDE\Delphi14"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(14)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d14; Components: "IDE\Delphi14"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(14)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi14"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(14)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi14"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(14)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi14"; Check: IsDelphiInstalled(14)
#endif
#ifdef Include_Delphi15
; Delphi XE
Root: HKCU; Subkey: "{code:GetDelphiRegKey|15}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|15}; Components: "IDE\Delphi15"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(15)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|15}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d15; Components: "IDE\Delphi15"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(15)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|15}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi15"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(15)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|15}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi15"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(15)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|15}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi15"; Check: IsDelphiInstalled(15)
#endif
#endif



[UninstallDelete]
Type: files; Name: "{app}\bin\JediInstaller.exe"
Type: files; Name: "{app}\bin\JCL-install.ini"
Type: files; Name: "{app}\bin\*.log"
Type: files; Name: "{app}\source\*.hpp"
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
Type: files; Name: "{code:GetDelphiBplDir|11}\Jcl*.~bpl";
; lib\Delphi/C++Builder 2009
Type: files; Name: "{app}\lib\d12\*"
Type: files; Name: "{app}\lib\d12\debug\*"
Type: files; Name: "{code:GetDelphiBplDir|12}\Jcl*.~bpl";
; lib\Delphi/C++Builder 2010
Type: files; Name: "{app}\lib\d14\*"
Type: files; Name: "{app}\lib\d14\debug\*"
Type: files; Name: "{code:GetDelphiBplDir|14}\Jcl*.~bpl";
; lib\Delphi/C++Builder XE
Type: files; Name: "{app}\lib\d15\*"
Type: files; Name: "{app}\lib\d15\debug\*"
Type: files; Name: "{code:GetDelphiBplDir|15}\Jcl*.~bpl";

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
  for Version := 6 to 14 do
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
      LibDir := AppDir + '\lib\d' + IntToStr(Version);
    ikBCB:
      LibDir := AppDir + '\lib\c' + IntToStr(Version);
  else
    Exit;
  end;
  
  SearchPaths := LibDir + ';' + AppDir + '\source\Include';
  DebugPaths := LibDir + '\debug';
  BrowsePaths := AppDir + '\source\Include;' + AppDir + '\source\common;' + AppDir + '\source\vcl;' + AppDir + '\source\windows';
  if Version >= 10 then
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

