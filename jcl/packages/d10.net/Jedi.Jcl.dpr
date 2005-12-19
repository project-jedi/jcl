library Jedi.Jcl;

uses
  System.Reflection,
  System.Runtime.InteropServices,
  JclAbstractContainers in '..\..\source\common\JclAbstractContainers.pas',
  JclAlgorithms in '..\..\source\common\JclAlgorithms.pas',
  JclAnsiStrings in '..\..\source\common\JclAnsiStrings.pas',
  JclArrayLists in '..\..\source\common\JclArrayLists.pas',
  JclArraySets in '..\..\source\common\JclArraySets.pas',
  JclBase in '..\..\source\common\JclBase.pas',
  JclBinaryTrees in '..\..\source\common\JclBinaryTrees.pas',
  JclComplex in '..\..\source\common\JclComplex.pas',
  JclContainerIntf in '..\..\source\common\JclContainerIntf.pas',
  JclDateTime in '..\..\source\common\JclDateTime.pas',
  JclFileUtils in '..\..\source\common\JclFileUtils.pas',
  JclHashSets in '..\..\source\common\JclHashSets.pas',
  JclIniFiles in '..\..\source\common\JclIniFiles.pas',
  JclLinkedLists in '..\..\source\common\JclLinkedLists.pas',
  JclLogic in '..\..\source\common\JclLogic.pas',
  JclMath in '..\..\source\common\JclMath.pas',
  JclMime in '..\..\source\common\JclMime.pas',
  JclQueues in '..\..\source\common\JclQueues.pas',
  JclResources in '..\..\source\common\JclResources.pas',
  JclRTTI in '..\..\source\common\JclRTTI.pas',
  JclStacks in '..\..\source\common\JclStacks.pas',
  JclStatistics in '..\..\source\common\JclStatistics.pas',
  JclStrings in '..\..\source\common\JclStrings.pas',
  JclSysInfo in '..\..\source\common\JclSysInfo.pas',
  JclSysUtils in '..\..\source\common\JclSysUtils.pas',
  JclUnitConv in '..\..\source\common\JclUnitConv.pas',
  JclValidation in '..\..\source\common\JclValidation.pas',
  JclVectors in '..\..\source\common\JclVectors.pas',
  JclHashMaps in '..\..\source\common\JclHashMaps.pas';

{$LIBSUFFIX '10'}

[assembly: AssemblyTitle('JEDI Code Library')]
[assembly: AssemblyDescription('Functions and classes')]
[assembly: AssemblyConfiguration('')]
[assembly: AssemblyCompany('')]
[assembly: AssemblyProduct('JCL')]
[assembly: AssemblyCopyright('')]
[assembly: AssemblyTrademark('')]
[assembly: AssemblyCulture('')]

//
// Die Versionsinformation einer Assemblierung enthält die folgenden vier Werte:
//
//      Hauptversion
//      Nebenversion
//      Build-Nummer
//      Revision
//
// Sie können alle vier Werte festlegen oder für Revision und Build-Nummer die
// Standardwerte mit '*' - wie nachfolgend gezeigt - verwenden:

[assembly: AssemblyVersion('1.0.*')]

//
// Zum Signieren einer Assemblierung müssen Sie einen Schlüssel angeben. Weitere Informationen
// über das Signieren von Assemblierungen finden Sie in der Microsoft .NET Framework-Dokumentation.
//
// Mit den folgenden Attributen steuern Sie, welcher Schlüssel für die Signatur verwendet wird.

// Hinweise:
//   (*) Wenn kein Schlüssel angegeben wird, ist die Assemblierung nicht signiert.
//   (*) KeyName verweist auf einen Schlüssel, der im Crypto Service Provider
//      (CSP) auf Ihrem Rechner installiert wurde. KeyFile verweist auf eine
//       Datei, die einen Schlüssel enthält.
//   (*) Wenn sowohl der KeyFile- als auch der KeyName-Wert angegeben ist, wird
//       die folgende Verarbeitung durchgeführt:
//       (1) Wenn KeyName in dem CSP gefunden wird, wird dieser Schlüssel verwendet.
//       (2) Wenn KeyName nicht, aber KeyFile vorhanden ist, wird der Schlüssel
//           in KeyFile im CSP installiert und verwendet.
//   (*) Ein KeyFile können Sie mit dem Utility sn.exe (Starker Name) erzeugen.
//       Der Speicherort von KeyFile sollte relativ zum Projektausgabeverzeichnis
//       angegeben werden. Wenn sich Ihr KeyFile im Projektverzeichnis befindet,
//       würden Sie das Attribut AssemblyKeyFile folgendermaßen festlegen:
//       [assembly: AssemblyKeyFile('mykey.snk')], vorausgesetzt, Ihr
//       Ausgabeverzeichnis ist das Projektverzeichnis (Vorgabe).
//   (*) Verzögerte Signatur ist eine erweiterte Option; nähere Informationen
//       dazu finden Sie in der Microsoft .NET Framework-Dokumentation.
//
[assembly: AssemblyDelaySign(false)]
[assembly: AssemblyKeyFile('')]
[assembly: AssemblyKeyName('')]

//
// Verwenden Sie die folgenden Attribute zur Steuerung der COM-Sichtbarkeit Ihrer Assemblierung.
// Standardmäßig ist die gesamte Assemblierung für COM sichtbar. Die Einstellung false für ComVisible
// ist die für Ihre Assemblierung empfohlene Vorgabe. Um dann eine Klasse und ein Interface für COM
// bereitzustellen, setzen Sie jeweils ComVisible auf true. Es wird auch empfohlen das Attribut
// Guid hinzuzufügen.
//

[assembly: ComVisible(False)]
//[assembly: Guid('')]
//[assembly: TypeLibVersion(1, 0)]


begin
end.
