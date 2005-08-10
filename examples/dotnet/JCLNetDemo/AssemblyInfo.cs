using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

//
// Die allgemeinen Assemblierungsinformationen werden durch die folgenden
// Attribute gesteuert. Ändern Sie die Attributwerte, um die zu einer
// Assemblierung gehörenden Informationen zu modifizieren.
//
[assembly: AssemblyTitle("")]
[assembly: AssemblyDescription("")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("")]
[assembly: AssemblyCopyright("")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]	
	
// Die Versionsinformation einer Assemblierung enthält die folgenden vier Werte:
//      Hauptversion
//      Nebenversion
//      Build-Nummer
//      Revision
// Sie können alle vier Werte festlegen oder für Revision und Build-Nummer die
// Standardwerte mit '*' - wie nachfolgend gezeigt - verwenden:

[assembly: AssemblyVersion("1.0.*")]

//
// Zum Signieren einer Assemblierung müssen Sie einen Schlüssel angeben. Weitere Informationen
// über das Signieren von Assemblierungen finden Sie in der Microsoft .NET Framework-Dokumentation.
// Mit den folgenden Attributen steuern Sie, welcher Schlüssel für die Signatur verwendet wird.
// Hinweise:
//   (*) Wenn kein Schlüssel angegeben wird, ist die Assemblierung nicht signiert.
//   (*) KeyName verweist auf einen Schlüssel, der im Crypto Service Provider
//      (CSP) auf Ihrem Rechner installiert wurde. KeyFile verweist auf eine
//       Datei, die einen Schlüssel enthält.
//   (*) Wenn sowohl der KeyFile- als auch der KeyName-Wert angegeben ist, wird
//      die folgende Verarbeitung durchgeführt:
//       (1) Wenn KeyName in dem CSP gefunden wird, wird dieser Schlüssel verwendet.
//       (2) Wenn KeyName nicht, aber KeyFile vorhanden ist, wird der Schlüssel
//           in KeyFile im CSP installiert und verwendet.
//   (*) Ein KeyFile können Sie mit dem Utility sn.exe (Starker Name) erzeugen.
//       Der Speicherort von KeyFile sollte relativ zum Projektausgabeverzeichnis
//       %Projektverzeichnis%\bin\<Konfiguration> angegeben werden. Wenn sich Ihr
//       KeyFile z.B. im Projektverzeichnis befindet, würden Sie das Attribut
//       AssemblyKeyFile folgendermaßen festlegen:
//       [assembly: AssemblyKeyFile("..\\..\\mykey.snk")]
//   (*) Verzögerte Signatur ist eine erweiterte Option; nähere Informationen
//       dazu finden Sie in der Microsoft .NET Framework-Dokumentation.
//
[assembly: AssemblyDelaySign(false)]
[assembly: AssemblyKeyFile("")]
[assembly: AssemblyKeyName("")]

//
// Verwenden Sie die folgenden Attribute zur Steuerung der COM-Sichtbarkeit Ihrer Assemblierung.
// Standardmäßig ist die gesamte Assemblierung für COM sichtbar. Die Einstellung false für ComVisible
// ist die für Ihre Assemblierung empfohlene Vorgabe. Um dann eine Klasse und ein Interface für COM
// bereitzustellen, setzen Sie jeweils ComVisible auf true. Es wird auch empfohlen das Attribut
// Guid hinzuzufügen.
//

[assembly: ComVisible(false)]
//[assembly: Guid("")]
//[assembly: TypeLibVersion(1, 0)]

