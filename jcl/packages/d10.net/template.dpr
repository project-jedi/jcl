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

{$LIBSUFFIX '10'}

[assembly: AssemblyTitle('JEDI Code Library')]
[assembly: AssemblyDescription('%DESCRIPTION%')]
[assembly: AssemblyConfiguration('')]
[assembly: AssemblyCompany('')]
[assembly: AssemblyProduct('JCL')]
[assembly: AssemblyCopyright('')]
[assembly: AssemblyTrademark('')]
[assembly: AssemblyCulture('')]

// MajorVersion.MinorVersion.BuildNumber.Revision
[assembly: AssemblyVersion('1.0.*')]

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
