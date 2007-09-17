%PROJECT% %NAME%;

uses
<%%% START REQUIRES %%%>
  %NAME%,
<%%% END REQUIRES %%%>
  ,
<%%% START FILES %%%>
  %UNITNAME% in '%FILENAME%' {%FORMNAMEANDTYPE%},
<%%% END FILES %%%>
  ;

{$LIBSUFFIX '11'}

[assembly: AssemblyTitle('JEDI Code Library')]
[assembly: AssemblyDescription('%DESCRIPTION%')]
[assembly: AssemblyConfiguration('')]
[assembly: AssemblyCompany('Project JEDI')]
[assembly: AssemblyProduct('JEDI Code Library')]
[assembly: AssemblyCopyright('Copyright (C) 1999, 2007 Project JEDI')]
[assembly: AssemblyTrademark('')]
[assembly: AssemblyCulture('')]

// MajorVersion.MinorVersion.BuildNumber.Revision
[assembly: AssemblyVersion('%VERSION_MAJOR_NUMBER%.%VERSION_MINOR_NUMBER%.%RELEASE_NUMBER%.%BUILD_NUMBER%')]

// Package signature
[assembly: AssemblyDelaySign(false)]
[assembly: AssemblyKeyFile('')]
[assembly: AssemblyKeyName('')]

// Com visibility of the assembly
[assembly: ComVisible(False)]
//[assembly: Guid('')]
//[assembly: TypeLibVersion(1, 0)]


begin
end.
