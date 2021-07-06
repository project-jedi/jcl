{$IFNDEF JEDI_INC}
{$DEFINE JEDI_INC}

{**************************************************************************************************}
{                                                                                                  }
{  The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");}
{  you may not use this file except in compliance with the License. You may obtain a copy of the   }
{  License at http://www.mozilla.org/MPL/                                                          }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing rights }
{  and limitations under the License.                                                              }
{                                                                                                  }
{  The Original Code is: jedi.inc.                                                                 }
{  The Initial Developer of the Original Code is Project JEDI http://www.delphi-jedi.org           }
{                                                                                                  }
{  Alternatively, the contents of this file may be used under the terms of the GNU Lesser General  }
{  Public License (the  "LGPL License"), in which case the provisions of the LGPL License are      }
{  applicable instead of those above. If you wish to allow use of your version of this file only   }
{  under the terms of the LGPL License and not to allow others to use your version of this file    }
{  under the MPL, indicate your decision by deleting the provisions above and replace them with    }
{  the notice and other provisions required by the LGPL License. If you do not delete the          }
{  provisions above, a recipient may use your version of this file under either the MPL or the     }
{  LGPL License.                                                                                   }
{                                                                                                  }
{  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{  This file defines various generic compiler directives used in different libraries, e.g. in the  }
{  JEDI Code Library (JCL) and JEDI Visual Component Library Library (JVCL). The directives in     }
{  this file are of generic nature and consist mostly of mappings from the VERXXX directives       }
{  defined by Delphi, C++Builder and FPC to friendly names such as DELPHI5 and                     }
{  SUPPORTS_WIDESTRING. These friendly names are subsequently used in the libraries to test for    }
{  compiler versions and/or whether the compiler supports certain features (such as widestrings or }
{  64 bit integers. The libraries provide an additional, library specific, include file. For the   }
{  JCL e.g. this is jcl.inc. These files should be included in source files instead of this file   }
{  (which is pulled in automatically).                                                             }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

(*

- Development environment directives

  This file defines two directives to indicate which development environment the
  library is being compiled with. Currently this can either be Delphi, Kylix,
  C++Builder or FPC.

  Directive           Description
  ------------------------------------------------------------------------------
  DELPHI              Defined if compiled with Delphi
  KYLIX               Defined if compiled with Kylix
  DELPHICOMPILER      Defined if compiled with Delphi or Kylix/Delphi
  BCB                 Defined if compiled with C++Builder
  CPPBUILDER          Defined if compiled with C++Builder (alias for BCB)
  BCBCOMPILER         Defined if compiled with C++Builder or Kylix/C++
  DELPHILANGUAGE      Defined if compiled with Delphi, Kylix or C++Builder
  BORLAND             Defined if compiled with Delphi, Kylix or C++Builder
  FPC                 Defined if compiled with FPC

- Platform Directives

  Platform directives are not all explicitly defined in this file, some are
  defined by the compiler itself. They are listed here only for completeness.

  Directive           Description
  ------------------------------------------------------------------------------
  WIN32               Defined when target platform is 32 bit Windows
  WIN64               Defined when target platform is 64 bit Windows
  MSWINDOWS           Defined when target platform is 32 bit Windows
  LINUX               Defined when target platform is Linux
  UNIX                Defined when target platform is Unix-like (including Linux)
  CLR                 Defined when target platform is .NET

- Architecture directives. These are auto-defined by FPC
  CPU32 and CPU64 are mostly for generic pointer size dependant differences rather
  than for a specific architecture.

  CPU386              Defined when target platform is native x86 (win32)
  CPUx86_64           Defined when target platform is native x86_64 (win64)
  CPU32               Defined when target is 32-bit
  CPU64	              Defined when target is 64-bit
  CPUASM              Defined when target assembler is available

- Visual library Directives

  The following directives indicate for a visual library. In a Delphi/BCB
  (Win32) application you need to define the VisualCLX symbol in the project
  options, if  you want to use the VisualCLX library. Alternatively you can use
  the IDE expert, which is distributed with the JCL to do this automatically.

  Directive           Description
  ------------------------------------------------------------------------------
  VCL                 Defined for Delphi/BCB (Win32) exactly if VisualCLX is not defined
  VisualCLX           Defined for Kylix; needs to be defined for Delphi/BCB to
                      use JCL with VisualCLX applications.


- Other cross-platform related defines

  These symbols are intended to help in writing portable code.

  Directive           Description
  ------------------------------------------------------------------------------
  PUREPASCAL          Code is machine-independent (as opposed to assembler code)
  Win32API            Code is specific for the Win32 API;
                      use instead of "{$IFNDEF CLR} {$IFDEF MSWINDOWS}" constructs


- Delphi Versions

  The following directives are direct mappings from the VERXXX directives to a
  friendly name of the associated compiler. These directives are only defined if
  the compiler is Delphi (ie DELPHI is defined).

  Directive           Description
  ------------------------------------------------------------------------------
  DELPHI1             Defined when compiling with Delphi 1 (Codename WASABI/MANGO)
  DELPHI2             Defined when compiling with Delphi 2 (Codename POLARIS)
  DELPHI3             Defined when compiling with Delphi 3 (Codename IVORY)
  DELPHI4             Defined when compiling with Delphi 4 (Codename ALLEGRO)
  DELPHI5             Defined when compiling with Delphi 5 (Codename ARGUS)
  DELPHI6             Defined when compiling with Delphi 6 (Codename ILLIAD)
  DELPHI7             Defined when compiling with Delphi 7 (Codename AURORA)
  DELPHI8             Defined when compiling with Delphi 8 (Codename OCTANE)
  DELPHI2005          Defined when compiling with Delphi 2005 (Codename DIAMONDBACK)
  DELPHI9             Alias for DELPHI2005
  DELPHI10            Defined when compiling with Delphi 2006 (Codename DEXTER)
  DELPHI2006          Alias for DELPHI10
  DELPHI11            Defined when compiling with Delphi 2007 for Win32 (Codename SPACELY)
  DELPHI2007          Alias for DELPHI11
  DELPHI12            Defined when compiling with Delphi 2009 for Win32 (Codename TIBURON)
  DELPHI2009          Alias for DELPHI12
  DELPHI14            Defined when compiling with Delphi 2010 for Win32 (Codename WEAVER)
  DELPHI2010          Alias for DELPHI14
  DELPHI15            Defined when compiling with Delphi XE for Win32 (Codename FULCRUM)
  DELPHIXE            Alias for DELPHI15
  DELPHI16            Defined when compiling with Delphi XE2 for Win32 (Codename PULSAR)
  DELPHIXE2           Alias for DELPHI16
  DELPHI17            Defined when compiling with Delphi XE3 for Win32 (Codename WATERDRAGON)
  DELPHIXE3           Alias for DELPHI17
  DELPHI18            Defined when compiling with Delphi XE4 for Win32 (Codename QUINTESSENCE)
  DELPHIXE4           Alias for DELPHI18
  DELPHI19            Defined when compiling with Delphi XE5 for Win32 (Codename ZEPHYR)
  DELPHIXE5           Alias for DELPHI19
  DELPHI20            Defined when compiling with Delphi XE6 for Win32 (Codename PROTEUS)
  DELPHIXE6           Alias for DELPHI20
  DELPHI21            Defined when compiling with Delphi XE7 for Win32 (Codename CARPATHIA)
  DELPHIXE7           Alias for DELPHI21
  DELPHI22            Defined when compiling with Delphi XE8 for Win32 (Codename ELBRUS)
  DELPHIXE8           Alias for DELPHI22
  DELPHI23            Defined when compiling with Delphi 10 for Win32 (Codename AITANA)
  DELPHIX_SEATTLE     Alias for DELPHI23
  DELPHI24            Defined when compiling with Delphi 10.1 for Win32 (Codename BIGBEN)
  DELPHIX_BERLIN      Alias for DELPHI24
  DELPHI25            Defined when compiling with Delphi 10.2 for Win32 (Codename GODZILLA)
  DELPHIX_TOKYO       Alias for DELPHI25
  DELPHI26            Defined when compiling with Delphi 10.3 for Win32 (Codename CARNIVAL)
  DELPHIX_RIO         Alias for DELPHI26
  DELPHI1_UP          Defined when compiling with Delphi 1 or higher
  DELPHI2_UP          Defined when compiling with Delphi 2 or higher
  DELPHI3_UP          Defined when compiling with Delphi 3 or higher
  DELPHI4_UP          Defined when compiling with Delphi 4 or higher
  DELPHI5_UP          Defined when compiling with Delphi 5 or higher
  DELPHI6_UP          Defined when compiling with Delphi 6 or higher
  DELPHI7_UP          Defined when compiling with Delphi 7 or higher
  DELPHI8_UP          Defined when compiling with Delphi 8 or higher
  DELPHI2005_UP       Defined when compiling with Delphi 2005 or higher
  DELPHI9_UP          Alias for DELPHI2005_UP
  DELPHI10_UP         Defined when compiling with Delphi 2006 or higher
  DELPHI2006_UP       Alias for DELPHI10_UP
  DELPHI11_UP         Defined when compiling with Delphi 2007 for Win32 or higher
  DELPHI2007_UP       Alias for DELPHI11_UP
  DELPHI12_UP         Defined when compiling with Delphi 2009 for Win32 or higher
  DELPHI2009_UP       Alias for DELPHI12_UP
  DELPHI14_UP         Defined when compiling with Delphi 2010 for Win32 or higher
  DELPHI2010_UP       Alias for DELPHI14_UP
  DELPHI15_UP         Defined when compiling with Delphi XE for Win32 or higher
  DELPHIXE_UP         Alias for DELPHI15_UP
  DELPHI16_UP         Defined when compiling with Delphi XE2 for Win32 or higher
  DELPHIXE2_UP        Alias for DELPHI16_UP
  DELPHI17_UP         Defined when compiling with Delphi XE3 for Win32 or higher
  DELPHIXE3_UP        Alias for DELPHI17_UP
  DELPHI18_UP         Defined when compiling with Delphi XE4 for Win32 or higher
  DELPHIXE4_UP        Alias for DELPHI18_UP
  DELPHI19_UP         Defined when compiling with Delphi XE5 for Win32 or higher
  DELPHIXE5_UP        Alias for DELPHI19_UP
  DELPHI20_UP         Defined when compiling with Delphi XE6 for Win32 or higher
  DELPHIXE6_UP        Alias for DELPHI20_UP
  DELPHI21_UP         Defined when compiling with Delphi XE7 for Win32 or higher
  DELPHIXE7_UP        Alias for DELPHI21_UP
  DELPHI22_UP         Defined when compiling with Delphi XE8 for Win32 or higher
  DELPHIXE8_UP        Alias for DELPHI22_UP
  DELPHI23_UP         Defined when compiling with Delphi 10 for Win32 or higher
  DELPHIX_SEATTLE_UP  Alias for DELPHI23_UP
  DELPHI24_UP         Defined when compiling with Delphi 10.1 for Win32 or higher
  DELPHIX_BERLIN_UP   Alias for DELPHI24_UP
  DELPHI25_UP         Defined when compiling with Delphi 10.2 for Win32 or higher
  DELPHIX_TOKYO_UP    Alias for DELPHI25_UP
  DELPHI26_UP         Defined when compiling with Delphi 10.3 for Win32 or higher
  DELPHIX_RIO_UP      Alias for DELPHI26_UP
  DELPHI27_UP         Defined when compiling with Delphi 10.4 for Win32 or higher


- Kylix Versions

  The following directives are direct mappings from the VERXXX directives to a
  friendly name of the associated compiler. These directives are only defined if
  the compiler is Kylix (ie KYLIX is defined).

  Directive           Description
  ------------------------------------------------------------------------------
  KYLIX1              Defined when compiling with Kylix 1
  KYLIX2              Defined when compiling with Kylix 2
  KYLIX3              Defined when compiling with Kylix 3 (Codename CORTEZ)
  KYLIX1_UP           Defined when compiling with Kylix 1 or higher
  KYLIX2_UP           Defined when compiling with Kylix 2 or higher
  KYLIX3_UP           Defined when compiling with Kylix 3 or higher


- Delphi Compiler Versions (Delphi / Kylix, not in BCB mode)

  Directive           Description
  ------------------------------------------------------------------------------
  DELPHICOMPILER1      Defined when compiling with Delphi 1
  DELPHICOMPILER2      Defined when compiling with Delphi 2
  DELPHICOMPILER3      Defined when compiling with Delphi 3
  DELPHICOMPILER4      Defined when compiling with Delphi 4
  DELPHICOMPILER5      Defined when compiling with Delphi 5
  DELPHICOMPILER6      Defined when compiling with Delphi 6 or Kylix 1, 2 or 3
  DELPHICOMPILER7      Defined when compiling with Delphi 7
  DELPHICOMPILER8      Defined when compiling with Delphi 8
  DELPHICOMPILER9      Defined when compiling with Delphi 2005
  DELPHICOMPILER10     Defined when compiling with Delphi Personality of BDS 4.0
  DELPHICOMPILER11     Defined when compiling with Delphi 2007 for Win32
  DELPHICOMPILER12     Defined when compiling with Delphi Personality of BDS 6.0
  DELPHICOMPILER14     Defined when compiling with Delphi Personality of BDS 7.0
  DELPHICOMPILER15     Defined when compiling with Delphi Personality of BDS 8.0
  DELPHICOMPILER16     Defined when compiling with Delphi Personality of BDS 9.0
  DELPHICOMPILER17     Defined when compiling with Delphi Personality of BDS 10.0
  DELPHICOMPILER18     Defined when compiling with Delphi Personality of BDS 11.0
  DELPHICOMPILER19     Defined when compiling with Delphi Personality of BDS 12.0
  DELPHICOMPILER20     Defined when compiling with Delphi Personality of BDS 14.0
  DELPHICOMPILER21     Defined when compiling with Delphi Personality of BDS 15.0
  DELPHICOMPILER22     Defined when compiling with Delphi Personality of BDS 16.0
  DELPHICOMPILER23     Defined when compiling with Delphi Personality of BDS 17.0
  DELPHICOMPILER24     Defined when compiling with Delphi Personality of BDS 18.0
  DELPHICOMPILER25     Defined when compiling with Delphi Personality of BDS 19.0
  DELPHICOMPILER26     Defined when compiling with Delphi Personality of BDS 20.0
  DELPHICOMPILER27     Defined when compiling with Delphi Personality of BDS 21.0
  DELPHICOMPILER1_UP   Defined when compiling with Delphi 1 or higher
  DELPHICOMPILER2_UP   Defined when compiling with Delphi 2 or higher
  DELPHICOMPILER3_UP   Defined when compiling with Delphi 3 or higher
  DELPHICOMPILER4_UP   Defined when compiling with Delphi 4 or higher
  DELPHICOMPILER5_UP   Defined when compiling with Delphi 5 or higher
  DELPHICOMPILER6_UP   Defined when compiling with Delphi 6 or Kylix 1, 2 or 3 or higher
  DELPHICOMPILER7_UP   Defined when compiling with Delphi 7 or higher
  DELPHICOMPILER8_UP   Defined when compiling with Delphi 8 or higher
  DELPHICOMPILER9_UP   Defined when compiling with Delphi 2005
  DELPHICOMPILER10_UP  Defined when compiling with Delphi 2006 or higher
  DELPHICOMPILER11_UP  Defined when compiling with Delphi 2007 for Win32 or higher
  DELPHICOMPILER12_UP  Defined when compiling with Delphi 2009 for Win32 or higher
  DELPHICOMPILER14_UP  Defined when compiling with Delphi 2010 for Win32 or higher
  DELPHICOMPILER15_UP  Defined when compiling with Delphi XE for Win32 or higher
  DELPHICOMPILER16_UP  Defined when compiling with Delphi XE2 for Win32 or higher
  DELPHICOMPILER17_UP  Defined when compiling with Delphi XE3 for Win32 or higher
  DELPHICOMPILER18_UP  Defined when compiling with Delphi XE4 for Win32 or higher
  DELPHICOMPILER19_UP  Defined when compiling with Delphi XE5 for Win32 or higher
  DELPHICOMPILER20_UP  Defined when compiling with Delphi XE6 for Win32 or higher
  DELPHICOMPILER21_UP  Defined when compiling with Delphi XE7 for Win32 or higher
  DELPHICOMPILER22_UP  Defined when compiling with Delphi XE8 for Win32 or higher
  DELPHICOMPILER23_UP  Defined when compiling with Delphi 10 for Win32 or higher
  DELPHICOMPILER24_UP  Defined when compiling with Delphi 10.1 for Win32 or higher
  DELPHICOMPILER25_UP  Defined when compiling with Delphi 10.2 for Win32 or higher
  DELPHICOMPILER26_UP  Defined when compiling with Delphi 10.3 for Win32 or higher
  DELPHICOMPILER27_UP  Defined when compiling with Delphi 10.4 for Win32 or higher


- C++Builder Versions

  The following directives are direct mappings from the VERXXX directives to a
  friendly name of the associated compiler. These directives are only defined if
  the compiler is C++Builder (ie BCB is defined).

  Directive    Description
  ------------------------------------------------------------------------------
  BCB1         Defined when compiling with C++Builder 1
  BCB3         Defined when compiling with C++Builder 3
  BCB4         Defined when compiling with C++Builder 4
  BCB5         Defined when compiling with C++Builder 5 (Codename RAMPAGE)
  BCB6         Defined when compiling with C++Builder 6 (Codename RIPTIDE)
  BCB10        Defined when compiling with C++Builder Personality of BDS 4.0 (also known as C++Builder 2006) (Codename DEXTER)
  BCB11        Defined when compiling with C++Builder Personality of RAD Studio 2007 (also known as C++Builder 2007) (Codename COGSWELL)
  BCB12        Defined when compiling with C++Builder Personality of RAD Studio 2009 (also known as C++Builder 2009) (Codename TIBURON)
  BCB14        Defined when compiling with C++Builder Personality of RAD Studio 2010 (also known as C++Builder 2010) (Codename WEAVER)
  BCB15        Defined when compiling with C++Builder Personality of RAD Studio XE (also known as C++Builder XE) (Codename FULCRUM)
  BCB16        Defined when compiling with C++Builder Personality of RAD Studio XE2 (also known as C++Builder XE2) (Codename PULSAR)
  BCB17        Defined when compiling with C++Builder Personality of RAD Studio XE3 (also known as C++Builder XE3) (Codename WATERDRAGON)
  BCB18        Defined when compiling with C++Builder Personality of RAD Studio XE4 (also known as C++Builder XE4) (Codename QUINTESSENCE)
  BCB19        Defined when compiling with C++Builder Personality of RAD Studio XE5 (also known as C++Builder XE5) (Codename ZEPHYR)
  BCB20        Defined when compiling with C++Builder Personality of RAD Studio XE6 (also known as C++Builder XE6) (Codename PROTEUS)
  BCB21        Defined when compiling with C++Builder Personality of RAD Studio XE7 (also known as C++Builder XE7) (Codename CARPATHIA)
  BCB22        Defined when compiling with C++Builder Personality of RAD Studio XE8 (also known as C++Builder XE8) (Codename ELBRUS)
  BCB23        Defined when compiling with C++Builder Personality of RAD Studio 10 Seattle (also known as C++Builder 10 Seattle) (Codename AITANA)
  BCB24        Defined when compiling with C++Builder Personality of RAD Studio 10.1 Berlin (also known as C++Builder 10.1 Berlin) (Codename BIGBEN)
  BCB25        Defined when compiling with C++Builder Personality of RAD Studio 10.2 Tokyo (also known as C++Builder 10.2 Tokyo) (Codename GODZILLA)
  BCB26        Defined when compiling with C++Builder Personality of RAD Studio 10.3 Rio (also known as C++Builder 10.3) (Codename CARNIVAL)
  BCB27        Defined when compiling with C++Builder Personality of RAD Studio 10.4 Rio (also known as C++Builder 10.4) (Codename DENALI)
  BCB1_UP      Defined when compiling with C++Builder 1 or higher
  BCB3_UP      Defined when compiling with C++Builder 3 or higher
  BCB4_UP      Defined when compiling with C++Builder 4 or higher
  BCB5_UP      Defined when compiling with C++Builder 5 or higher
  BCB6_UP      Defined when compiling with C++Builder 6 or higher
  BCB10_UP     Defined when compiling with C++Builder Personality of BDS 4.0 or higher
  BCB11_UP     Defined when compiling with C++Builder Personality of RAD Studio 2007 or higher
  BCB12_UP     Defined when compiling with C++Builder Personality of RAD Studio 2009 or higher
  BCB14_UP     Defined when compiling with C++Builder Personality of RAD Studio 2010 or higher
  BCB15_UP     Defined when compiling with C++Builder Personality of RAD Studio XE or higher
  BCB16_UP     Defined when compiling with C++Builder Personality of RAD Studio XE2 or higher
  BCB17_UP     Defined when compiling with C++Builder Personality of RAD Studio XE3 or higher
  BCB18_UP     Defined when compiling with C++Builder Personality of RAD Studio XE4 or higher
  BCB19_UP     Defined when compiling with C++Builder Personality of RAD Studio XE5 or higher
  BCB20_UP     Defined when compiling with C++Builder Personality of RAD Studio XE6 or higher
  BCB21_UP     Defined when compiling with C++Builder Personality of RAD Studio XE7 or higher
  BCB22_UP     Defined when compiling with C++Builder Personality of RAD Studio XE8 or higher
  BCB23_UP     Defined when compiling with C++Builder Personality of RAD Studio 10 or higher
  BCB24_UP     Defined when compiling with C++Builder Personality of RAD Studio 10.1 or higher
  BCB25_UP     Defined when compiling with C++Builder Personality of RAD Studio 10.2 or higher
  BCB26_UP     Defined when compiling with C++Builder Personality of RAD Studio 10.3 or higher
  BCB27_UP     Defined when compiling with C++Builder Personality of RAD Studio 10.4 or higher


- RAD Studio / Borland Developer Studio Versions

  The following directives are direct mappings from the VERXXX directives to a
  friendly name of the associated IDE. These directives are only defined if
  the IDE is Borland Developer Studio Version 2 or above.

  Note: Borland Developer Studio 2006 is marketed as Delphi 2006 or C++Builder 2006,
  but those provide only different labels for identical content.

  Directive    Description
  ------------------------------------------------------------------------------
  BDS          Defined when compiling with BDS version of dcc32.exe (Codename SIDEWINDER)
  BDS2         Defined when compiling with BDS 2.0 (Delphi 8) (Codename OCTANE)
  BDS3         Defined when compiling with BDS 3.0 (Delphi 2005) (Codename DIAMONDBACK)
  BDS4         Defined when compiling with BDS 4.0 (Borland Developer Studio 2006) (Codename DEXTER)
  BDS5         Defined when compiling with BDS 5.0 (CodeGear RAD Studio 2007) (Codename HIGHLANDER)
  BDS6         Defined when compiling with BDS 6.0 (CodeGear RAD Studio 2009) (Codename TIBURON)
  BDS7         Defined when compiling with BDS 7.0 (Embarcadero RAD Studio 2010) (Codename WEAVER)
  BDS8         Defined when compiling with BDS 8.0 (Embarcadero RAD Studio XE) (Codename FULCRUM)
  BDS9         Defined when compiling with BDS 9.0 (Embarcadero RAD Studio XE2) (Codename PULSAR)
  BDS10        Defined when compiling with BDS 10.0 (Embarcadero RAD Studio XE3) (Codename WATERDRAGON)
  BDS11        Defined when compiling with BDS 11.0 (Embarcadero RAD Studio XE4) (Codename QUINTESSENCE)
  BDS12        Defined when compiling with BDS 12.0 (Embarcadero RAD Studio XE5) (Codename ZEPHYR)
  BDS14        Defined when compiling with BDS 14.0 (Embarcadero RAD Studio XE6) (Codename PROTEUS)
  BDS15        Defined when compiling with BDS 15.0 (Embarcadero RAD Studio XE7) (Codename CARPATHIA)
  BDS16        Defined when compiling with BDS 16.0 (Embarcadero RAD Studio XE8) (Codename ELBRUS)
  BDS17        Defined when compiling with BDS 17.0 (Embarcadero RAD Studio 10) (Codename AITANA)
  BDS18        Defined when compiling with BDS 18.0 (Embarcadero RAD Studio 10.1) (Codename BIGBEN)
  BDS19        Defined when compiling with BDS 19.0 (Embarcadero RAD Studio 10.2) (Codename GODZILLA)
  BDS20        Defined when compiling with BDS 20.0 (Embarcadero RAD Studio 10.3) (Codename CARNIVAL)
  BDS21        Defined when compiling with BDS 21.0 (Embarcadero RAD Studio 10.4) (Codename DENALI)
  BDS2_UP      Defined when compiling with BDS 2.0 or higher
  BDS3_UP      Defined when compiling with BDS 3.0 or higher
  BDS4_UP      Defined when compiling with BDS 4.0 or higher
  BDS5_UP      Defined when compiling with BDS 5.0 or higher
  BDS6_UP      Defined when compiling with BDS 6.0 or higher
  BDS7_UP      Defined when compiling with BDS 7.0 or higher
  BDS8_UP      Defined when compiling with BDS 8.0 or higher
  BDS9_UP      Defined when compiling with BDS 9.0 or higher
  BDS10_UP     Defined when compiling with BDS 10.0 or higher
  BDS11_UP     Defined when compiling with BDS 11.0 or higher
  BDS12_UP     Defined when compiling with BDS 12.0 or higher
  BDS14_UP     Defined when compiling with BDS 14.0 or higher
  BDS15_UP     Defined when compiling with BDS 15.0 or higher
  BDS16_UP     Defined when compiling with BDS 16.0 or higher
  BDS17_UP     Defined when compiling with BDS 17.0 or higher
  BDS18_UP     Defined when compiling with BDS 18.0 or higher
  BDS19_UP     Defined when compiling with BDS 19.0 or higher
  BDS20_UP     Defined when compiling with BDS 20.0 or higher
  BDS21_UP     Defined when compiling with BDS 21.0 or higher

- Compiler Versions

  The following directives are direct mappings from the VERXXX directives to a
  friendly name of the associated compiler. Unlike the DELPHI_X and BCB_X
  directives, these directives are indepedent of the development environment.
  That is, they are defined regardless of whether compilation takes place using
  Delphi or C++Builder.

  Directive     Description
  ------------------------------------------------------------------------------
  COMPILER1      Defined when compiling with Delphi 1
  COMPILER2      Defined when compiling with Delphi 2 or C++Builder 1
  COMPILER3      Defined when compiling with Delphi 3
  COMPILER35     Defined when compiling with C++Builder 3
  COMPILER4      Defined when compiling with Delphi 4 or C++Builder 4
  COMPILER5      Defined when compiling with Delphi 5 or C++Builder 5
  COMPILER6      Defined when compiling with Delphi 6 or C++Builder 6
  COMPILER7      Defined when compiling with Delphi 7
  COMPILER8      Defined when compiling with Delphi 8
  COMPILER9      Defined when compiling with Delphi 9
  COMPILER10     Defined when compiling with Delphi or C++Builder Personalities of BDS 4.0
  COMPILER11     Defined when compiling with Delphi or C++Builder Personalities of BDS 5.0
  COMPILER12     Defined when compiling with Delphi or C++Builder Personalities of BDS 6.0
  COMPILER14     Defined when compiling with Delphi or C++Builder Personalities of BDS 7.0
  COMPILER15     Defined when compiling with Delphi or C++Builder Personalities of BDS 8.0
  COMPILER16     Defined when compiling with Delphi or C++Builder Personalities of BDS 9.0
  COMPILER17     Defined when compiling with Delphi or C++Builder Personalities of BDS 10.0
  COMPILER18     Defined when compiling with Delphi or C++Builder Personalities of BDS 11.0
  COMPILER19     Defined when compiling with Delphi or C++Builder Personalities of BDS 12.0
  COMPILER20     Defined when compiling with Delphi or C++Builder Personalities of BDS 14.0
  COMPILER21     Defined when compiling with Delphi or C++Builder Personalities of BDS 15.0
  COMPILER22     Defined when compiling with Delphi or C++Builder Personalities of BDS 16.0
  COMPILER23     Defined when compiling with Delphi or C++Builder Personalities of BDS 17.0
  COMPILER24     Defined when compiling with Delphi or C++Builder Personalities of BDS 18.0
  COMPILER25     Defined when compiling with Delphi or C++Builder Personalities of BDS 19.0
  COMPILER26     Defined when compiling with Delphi or C++Builder Personalities of BDS 20.0
  COMPILER27     Defined when compiling with Delphi or C++Builder Personalities of BDS 21.0
  COMPILER1_UP   Defined when compiling with Delphi 1 or higher
  COMPILER2_UP   Defined when compiling with Delphi 2 or C++Builder 1 or higher
  COMPILER3_UP   Defined when compiling with Delphi 3 or higher
  COMPILER35_UP  Defined when compiling with C++Builder 3 or higher
  COMPILER4_UP   Defined when compiling with Delphi 4 or C++Builder 4 or higher
  COMPILER5_UP   Defined when compiling with Delphi 5 or C++Builder 5 or higher
  COMPILER6_UP   Defined when compiling with Delphi 6 or C++Builder 6 or higher
  COMPILER7_UP   Defined when compiling with Delphi 7
  COMPILER8_UP   Defined when compiling with Delphi 8
  COMPILER9_UP   Defined when compiling with Delphi Personalities of BDS 3.0
  COMPILER10_UP  Defined when compiling with Delphi or C++Builder Personalities of BDS 4.0 or higher
  COMPILER11_UP  Defined when compiling with Delphi or C++Builder Personalities of BDS 5.0 or higher
  COMPILER12_UP  Defined when compiling with Delphi or C++Builder Personalities of BDS 6.0 or higher
  COMPILER14_UP  Defined when compiling with Delphi or C++Builder Personalities of BDS 7.0 or higher
  COMPILER15_UP  Defined when compiling with Delphi or C++Builder Personalities of BDS 8.0 or higher
  COMPILER16_UP  Defined when compiling with Delphi or C++Builder Personalities of BDS 9.0 or higher
  COMPILER17_UP  Defined when compiling with Delphi or C++Builder Personalities of BDS 10.0 or higher
  COMPILER18_UP  Defined when compiling with Delphi or C++Builder Personalities of BDS 11.0 or higher
  COMPILER19_UP  Defined when compiling with Delphi or C++Builder Personalities of BDS 12.0 or higher
  COMPILER20_UP  Defined when compiling with Delphi or C++Builder Personalities of BDS 14.0 or higher
  COMPILER21_UP  Defined when compiling with Delphi or C++Builder Personalities of BDS 15.0 or higher
  COMPILER22_UP  Defined when compiling with Delphi or C++Builder Personalities of BDS 16.0 or higher
  COMPILER23_UP  Defined when compiling with Delphi or C++Builder Personalities of BDS 17.0 or higher
  COMPILER24_UP  Defined when compiling with Delphi or C++Builder Personalities of BDS 18.0 or higher
  COMPILER25_UP  Defined when compiling with Delphi or C++Builder Personalities of BDS 19.0 or higher
  COMPILER26_UP  Defined when compiling with Delphi or C++Builder Personalities of BDS 20.0 or higher
  COMPILER27_UP  Defined when compiling with Delphi or C++Builder Personalities of BDS 21.0 or higher


- RTL Versions

  Use e.g. following to determine the exact RTL version since version 14.0:
    {$IFDEF CONDITIONALEXPRESSIONS}
      {$IF Declared(RTLVersion) and (RTLVersion >= 14.2)}
        // code for Delphi 6.02 or higher, Kylix 2 or higher, C++Builder 6 or higher
        ...
      {$IFEND}
    {$ENDIF}

  Directive     Description
  ------------------------------------------------------------------------------
  RTL80_UP      Defined when compiling with Delphi 1 or higher
  RTL90_UP      Defined when compiling with Delphi 2 or higher
  RTL93_UP      Defined when compiling with C++Builder 1 or higher
  RTL100_UP     Defined when compiling with Delphi 3 or higher
  RTL110_UP     Defined when compiling with C++Builder 3 or higher
  RTL120_UP     Defined when compiling with Delphi 4 or higher
  RTL125_UP     Defined when compiling with C++Builder 4 or higher
  RTL130_UP     Defined when compiling with Delphi 5 or C++Builder 5 or higher
  RTL140_UP     Defined when compiling with Delphi 6, Kylix 1, 2 or 3 or C++Builder 6 or higher
  RTL150_UP     Defined when compiling with Delphi 7 or higher
  RTL160_UP     Defined when compiling with Delphi 8 or higher
  RTL170_UP     Defined when compiling with Delphi Personalities of BDS 3.0 or higher
  RTL180_UP     Defined when compiling with Delphi or C++Builder Personalities of BDS 4.0 or higher
  RTL185_UP     Defined when compiling with Delphi or C++Builder Personalities of BDS 5.0 or higher
  RTL190_UP     Defined when compiling with Delphi.NET of BDS 5.0 or higher
  RTL200_UP     Defined when compiling with Delphi or C++Builder Personalities of BDS 6.0 or higher
  RTL210_UP     Defined when compiling with Delphi or C++Builder Personalities of BDS 7.0 or higher
  RTL220_UP     Defined when compiling with Delphi or C++Builder Personalities of BDS 8.0 or higher
  RTL230_UP     Defined when compiling with Delphi or C++Builder Personalities of BDS 9.0 or higher
  RTL240_UP     Defined when compiling with Delphi or C++Builder Personalities of BDS 10.0 or higher
  RTL250_UP     Defined when compiling with Delphi or C++Builder Personalities of BDS 11.0 or higher
  RTL260_UP     Defined when compiling with Delphi or C++Builder Personalities of BDS 12.0 or higher
  RTL270_UP     Defined when compiling with Delphi or C++Builder Personalities of BDS 14.0 or higher
  RTL280_UP     Defined when compiling with Delphi or C++Builder Personalities of BDS 15.0 or higher
  RTL290_UP     Defined when compiling with Delphi or C++Builder Personalities of BDS 16.0 or higher
  RTL300_UP     Defined when compiling with Delphi or C++Builder Personalities of BDS 17.0 or higher
  RTL310_UP     Defined when compiling with Delphi or C++Builder Personalities of BDS 18.0 or higher
  RTL320_UP     Defined when compiling with Delphi or C++Builder Personalities of BDS 19.0 or higher
  RTL330_UP     Defined when compiling with Delphi or C++Builder Personalities of BDS 20.0 or higher
  RTL340_UP     Defined when compiling with Delphi or C++Builder Personalities of BDS 21.0 or higher


- CLR Versions

  Directive     Description
  ------------------------------------------------------------------------------
  CLR            Defined when compiling for .NET
  CLR10          Defined when compiling for .NET 1.0 (may be overriden by FORCE_CLR10)
  CLR10_UP       Defined when compiling for .NET 1.0 or higher
  CLR11          Defined when compiling for .NET 1.1 (may be overriden by FORCE_CLR11)
  CLR11_UP       Defined when compiling for .NET 1.1 or higher
  CLR20          Defined when compiling for .NET 2.0 (may be overriden by FORCE_CLR20)
  CLR20_UP       Defined when compiling for .NET 2.0 or higher


- Feature Directives

  The features directives are used to test if the compiler supports specific
  features, such as method overloading, and adjust the sources accordingly. Use
  of these directives is preferred over the use of the DELPHI and COMPILER
  directives.

  Directive              Description
  ------------------------------------------------------------------------------
  SUPPORTS_CONSTPARAMS           Compiler supports const parameters (D1+)
  SUPPORTS_SINGLE                Compiler supports the Single type (D1+)
  SUPPORTS_DOUBLE                Compiler supports the Double type (D1+)
  SUPPORTS_EXTENDED              Compiler supports the Extended type (D1+)
  SUPPORTS_CURRENCY              Compiler supports the Currency type (D2+)
  SUPPORTS_THREADVAR             Compiler supports threadvar declarations (D2+)
  SUPPORTS_OUTPARAMS             Compiler supports out parameters (D3+)
  SUPPORTS_VARIANT               Compiler supports variant (D2+)
  SUPPORTS_WIDECHAR              Compiler supports the WideChar type (D2+)
  SUPPORTS_WIDESTRING            Compiler supports the WideString type (D3+/BCB3+)
  SUPPORTS_INTERFACE             Compiler supports interfaces (D3+/BCB3+)
  SUPPORTS_DISPINTERFACE         Compiler supports dispatch interfaces (D3+/BCB3+)
  SUPPORTS_DISPID                Compiler supports dispatch ids (D3+/BCB3+/FPC)
  SUPPORTS_EXTSYM                Compiler supports the $EXTERNALSYM directive (D4+/BCB3+)
  SUPPORTS_NODEFINE              Compiler supports the $NODEFINE directive (D4+/BCB3+)
  SUPPORTS_LONGWORD              Compiler supports the LongWord type (unsigned 32 bit) (D4+/BCB4+)
  SUPPORTS_INT64                 Compiler supports the Int64 type (D4+/BCB4+)
  SUPPORTS_UINT64                Compiler supports the UInt64 type (D7+)
  SUPPORTS_DYNAMICARRAYS         Compiler supports dynamic arrays (D4+/BCB4+)
  SUPPORTS_DEFAULTPARAMS         Compiler supports default parameters (D4+/BCB4+)
  SUPPORTS_OVERLOAD              Compiler supports overloading (D4+/BCB4+)
  SUPPORTS_IMPLEMENTS            Compiler supports implements (D4+/BCB4+)
  SUPPORTS_DEPRECATED            Compiler supports the deprecated directive (D6+/BCB6+)
  SUPPORTS_PLATFORM              Compiler supports the platform directive (D6+/BCB6+)
  SUPPORTS_LIBRARY               Compiler supports the library directive (D6+/BCB6+/FPC)
  SUPPORTS_LOCAL                 Compiler supports the local directive (D6+/BCB6+)
  SUPPORTS_SETPEFLAGS            Compiler supports the SetPEFlags directive (D6+/BCB6+)
  SUPPORTS_EXPERIMENTAL_WARNINGS Compiler supports the WARN SYMBOL_EXPERIMENTAL and WARN UNIT_EXPERIMENTAL directives (D6+/BCB6+)
  SUPPORTS_INLINE                Compiler supports the inline directive (D9+/FPC)
  SUPPORTS_FOR_IN                Compiler supports for in loops (D9+)
  SUPPORTS_NESTED_CONSTANTS      Compiler supports nested constants (D9+)
  SUPPORTS_NESTED_TYPES          Compiler supports nested types (D9+)
  SUPPORTS_REGION                Compiler supports the REGION and ENDREGION directives (D9+)
  SUPPORTS_ENHANCED_RECORDS      Compiler supports class [operator|function|procedure] for record types (D9.NET, D10+)
  SUPPORTS_CLASS_FIELDS          Compiler supports class fields (D9.NET, D10+)
  SUPPORTS_CLASS_HELPERS         Compiler supports class helpers (D9.NET, D10+)
  SUPPORTS_CLASS_OPERATORS       Compiler supports class operators (D9.NET, D10+)
  SUPPORTS_CLASS_CTORDTORS       Compiler supports class contructors/destructors (D14+)
  SUPPORTS_STRICT                Compiler supports strict keyword (D9.NET, D10+)
  SUPPORTS_STATIC                Compiler supports static keyword (D9.NET, D10+)
  SUPPORTS_FINAL                 Compiler supports final keyword (D9.NET, D10+)
  SUPPORTS_METHODINFO            Compiler supports the METHODINFO directives (D10+)
  SUPPORTS_GENERICS              Compiler supports generic implementations (D11.NET, D12+)
  SUPPORTS_GENERIC_TYPES         Compiler supports generic implementations of types (D11.NET, D12+, FPC)
  SUPPORTS_GENERIC_METHODS       Compiler supports generic implementations of methods (D11.NET, D12+, FPC)
  SUPPORTS_GENERIC_ROUTINES      Compiler supports generic implementations of global functions/procedures (FPC)
  SUPPORTS_DEPRECATED_DETAILS    Compiler supports additional text for the deprecated directive (D11.NET, D12+)
  ACCEPT_DEPRECATED              Compiler supports or ignores the deprecated directive (D6+/BCB6+/FPC)
  ACCEPT_PLATFORM                Compiler supports or ignores the platform directive (D6+/BCB6+/FPC)
  ACCEPT_LIBRARY                 Compiler supports or ignores the library directive (D6+/BCB6+)
  SUPPORTS_CUSTOMVARIANTS        Compiler supports custom variants (D6+/BCB6+)
  SUPPORTS_VARARGS               Compiler supports varargs (D6+/BCB6+)
  SUPPORTS_ENUMVALUE             Compiler supports assigning ordinalities to values of enums (D6+/BCB6+)
  SUPPORTS_DEPRECATED_WARNINGS   Compiler supports deprecated warnings (D6+/BCB6+)
  SUPPORTS_LIBRARY_WARNINGS      Compiler supports library warnings (D6+/BCB6+)
  SUPPORTS_PLATFORM_WARNINGS     Compiler supports platform warnings (D6+/BCB6+)
  SUPPORTS_UNSAFE_WARNINGS       Compiler supports unsafe warnings (D7)
  SUPPORTS_WEAKPACKAGEUNIT       Compiler supports the WEAKPACKAGEUNIT directive
  SUPPORTS_COMPILETIME_MESSAGES  Compiler supports the MESSAGE directive
  SUPPORTS_PACKAGES              Compiler supports Packages
  HAS_UNIT_LIBC                  Unit Libc exists (Kylix, FPC on Linux/x86)
  HAS_UNIT_RTLCONSTS             Unit RTLConsts exists (D6+/BCB6+/FPC)
  HAS_UNIT_TYPES                 Unit Types exists (D6+/BCB6+/FPC)
  HAS_UNIT_VARIANTS              Unit Variants exists (D6+/BCB6+/FPC)
  HAS_UNIT_STRUTILS              Unit StrUtils exists (D6+/BCB6+/FPC)
  HAS_UNIT_DATEUTILS             Unit DateUtils exists (D6+/BCB6+/FPC)
  HAS_UNIT_CONTNRS               Unit contnrs exists (D6+/BCB6+/FPC)
  HAS_UNIT_HTTPPROD              Unit HTTPProd exists (D9+)
  HAS_UNIT_GIFIMG                Unit GifImg exists (D11+)
  HAS_UNIT_ANSISTRINGS           Unit AnsiStrings exists (D12+)
  HAS_UNIT_PNGIMAGE              Unit PngImage exists (D12+)
  HAS_UNIT_CHARACTER             Unit Character exists (D12+)
  XPLATFORM_RTL                  The RTL supports crossplatform function names (e.g. RaiseLastOSError) (D6+/BCB6+/FPC)
  SUPPORTS_UNICODE               string type is aliased to an unicode string (WideString or UnicodeString) (DX.NET, D12+)
  SUPPORTS_UNICODE_STRING        Compiler supports UnicodeString (D12+)
  SUPPORTS_INT_ALIASES           Types Int8, Int16, Int32, UInt8, UInt16 and UInt32 are defined in the unit System (D12+)
  HAS_UNIT_RTTI                  Unit RTTI is available (D14+)
  SUPPORTS_CAST_INTERFACE_TO_OBJ The compiler supports casts from interfaces to objects (D14+)
  SUPPORTS_DELAYED_LOADING       The compiler generates stubs for delaying imported function loads (D14+)
  HAS_UNIT_REGULAREXPRESSIONSAPI Unit RegularExpressionsAPI is available (D15+)
  HAS_UNIT_SYSTEM_UITYPES        Unit System.UITypes is available (D16+)
  HAS_UNIT_SYSTEM_ACTIONS        Unit System.Actions is available (D17+)
  DEPRECATED_SYSUTILS_ANSISTRINGS  AnsiString functions from SysUtils are deprecated and moved to System.AnsiStrings (D18+)
  HAS_PROPERTY_STYLEELEMENTS     TControl has a StyleElements property (D17+)
  HAS_AUTOMATIC_DB_FIELDS        Database fields are automatically created/refreshed (D20+)
  HAS_EARGUMENTEXCEPTION         Exception class EArgumentException is available (D14+)
  HAS_ENOTIMPLEMENTED            Exception class ENotImplemented is available (D15+)
  HAS_UNIT_VCL_THEMES            Unit Vcl.Themes is available (D16+)
  HAS_UNIT_UXTHEME               Unit (Vcl.)UxTheme is available (D7+)
  HAS_EXCEPTION_STACKTRACE       Exception class has the StackTrace propery (D12+)
  SUPPORTS_LEGACYIFEND           Compiler supports the LEGACYIFEND directive (D17+)
  DEPRECATED_TCHARACTER          TCharacter is deprecated and replaced by a record helper on Char (D18+)


- Compiler Settings

  The compiler settings directives indicate whether a specific compiler setting
  is in effect. This facilitates changing compiler settings locally in a more
  compact and readible manner.

  Directive              Description
  ------------------------------------------------------------------------------
  ALIGN_ON               Compiling in the A+ state (no alignment)
  BOOLEVAL_ON            Compiling in the B+ state (complete boolean evaluation)
  ASSERTIONS_ON          Compiling in the C+ state (assertions on)
  DEBUGINFO_ON           Compiling in the D+ state (debug info generation on)
  IMPORTEDDATA_ON        Compiling in the G+ state (creation of imported data references)
  LONGSTRINGS_ON         Compiling in the H+ state (string defined as AnsiString)
  IOCHECKS_ON            Compiling in the I+ state (I/O checking enabled)
  WRITEABLECONST_ON      Compiling in the J+ state (typed constants can be modified)
  LOCALSYMBOLS           Compiling in the L+ state (local symbol generation)
  LOCALSYMBOLS_ON        Alias of LOCALSYMBOLS
  TYPEINFO_ON            Compiling in the M+ state (RTTI generation on)
  OPTIMIZATION_ON        Compiling in the O+ state (code optimization on)
  OPENSTRINGS_ON         Compiling in the P+ state (variable string parameters are openstrings)
  OVERFLOWCHECKS_ON      Compiling in the Q+ state (overflow checing on)
  RANGECHECKS_ON         Compiling in the R+ state (range checking on)
  TYPEDADDRESS_ON        Compiling in the T+ state (pointers obtained using the @ operator are typed)
  SAFEDIVIDE_ON          Compiling in the U+ state (save FDIV instruction through RTL emulation)
  VARSTRINGCHECKS_ON     Compiling in the V+ state (type checking of shortstrings)
  STACKFRAMES_ON         Compiling in the W+ state (generation of stack frames)
  EXTENDEDSYNTAX_ON      Compiling in the X+ state (Delphi extended syntax enabled)
*)

{$DEFINE BORLAND}

{ Set FreePascal to Delphi mode }
{$IFDEF FPC}
  {$MODE DELPHI}
  {$ASMMODE Intel}
  {$UNDEF BORLAND}
  {$DEFINE CPUASM}
   // FPC defines CPU32, CPU64 and Unix automatically
{$ENDIF}

{$IFDEF BORLAND}
  {$IFDEF LINUX}
    {$IFDEF VER140} // Only under Delphi 6, LINUX implies Kylix
    {$DEFINE KYLIX}
    {$ENDIF}
  {$ENDIF LINUX}
  {$IFNDEF CLR}
    {$IFNDEF CPUX86}
      // CPUX86 is not defined, which means it most likely is a 64 bits compiler.
      // However, this is only the case if either of two other symbols are defined:
      // http://docwiki.embarcadero.com/RADStudio/Seattle/en/Conditional_compilation_%28Delphi%29
      {$DEFINE CPU64}
      {$DEFINE DELPHI64_TEMPORARY}
      {$IFNDEF CPUX64}
        {$IFNDEF CPU64BITS}
          {$DEFINE CPU386}  // None of the two 64-bits symbols are defined, assume this is 32-bit
          {$DEFINE CPU32}   
          {$UNDEF CPU64}
          {$UNDEF DELPHI64_TEMPORARY}
        {$ENDIF ~CPU64BITS}
      {$ENDIF ~CPUX64}
    {$ELSE ~CPUX86}
      {$DEFINE CPU386}
      {$DEFINE CPU32}
    {$ENDIF ~CPUX86}

    // The ASSEMBLER symbol appeared with Delphi 7
    {$IFNDEF COMPILER7_UP}
      {$DEFINE CPUASM}
    {$ELSE}
      {$IFDEF ASSEMBLER}
        {$DEFINE CPUASM}
      {$ENDIF ASSEMBLER}
    {$ENDIF ~COMPILER7_UP}    
  {$ENDIF ~CLR}
{$ENDIF BORLAND}

{------------------------------------------------------------------------------}
{ VERXXX to COMPILERX, DELPHIX and BCBX mappings                               }
{------------------------------------------------------------------------------}

{$IFDEF BORLAND}
  {$IFDEF KYLIX}
    {$I kylix.inc} // FPC incompatible stuff
  {$ELSE ~KYLIX}

    {$DEFINE UNKNOWN_COMPILER_VERSION}

    {$IFDEF VER80}
      {$DEFINE COMPILER1}
      {$DEFINE DELPHI1}
      {$DEFINE DELPHICOMPILER1}
      {$DEFINE RTL80_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER90}
      {$DEFINE COMPILER2}
      {$DEFINE DELPHI2}
      {$DEFINE DELPHICOMPILER2}
      {$DEFINE RTL90_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER93}
      {$DEFINE COMPILER2}
      {$DEFINE BCB1}
      {$DEFINE BCB}
      {$DEFINE RTL93_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER100}
      {$DEFINE COMPILER3}
      {$DEFINE DELPHI3}
      {$DEFINE DELPHICOMPILER3}
      {$DEFINE RTL100_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER110}
      {$DEFINE COMPILER35}
      {$DEFINE BCB3}
      {$DEFINE BCB}
      {$DEFINE RTL110_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER120}
      {$DEFINE COMPILER4}
      {$DEFINE DELPHI4}
      {$DEFINE DELPHICOMPILER4}
      {$DEFINE RTL120_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER125}
      {$DEFINE COMPILER4}
      {$DEFINE BCB4}
      {$DEFINE BCB}
      {$DEFINE RTL125_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER130}
      {$DEFINE COMPILER5}
      {$IFDEF BCB}
        {$DEFINE BCB5}
      {$ELSE}
        {$DEFINE DELPHI5}
        {$DEFINE DELPHICOMPILER5}
      {$ENDIF}
      {$DEFINE RTL130_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER140}
      {$DEFINE COMPILER6}
      {$IFDEF BCB}
        {$DEFINE BCB6}
      {$ELSE}
        {$DEFINE DELPHI6}
        {$DEFINE DELPHICOMPILER6}
      {$ENDIF}
      {$DEFINE RTL140_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER150}
      {$DEFINE COMPILER7}
      {$DEFINE DELPHI7}
      {$DEFINE DELPHICOMPILER7}
      {$DEFINE RTL150_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER160}
      {$DEFINE BDS2}
      {$DEFINE BDS}
      {$IFDEF CLR}
        {$DEFINE CLR10}
      {$ENDIF CLR}
      {$DEFINE COMPILER8}
      {$DEFINE DELPHI8}
      {$DEFINE DELPHICOMPILER8}
      {$DEFINE RTL160_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER170}
      {$DEFINE BDS3}
      {$DEFINE BDS}
      {$IFDEF CLR}
        {$DEFINE CLR11}
      {$ENDIF CLR}
      {$DEFINE COMPILER9}
      {$DEFINE DELPHI9}
      {$DEFINE DELPHI2005} // synonym to DELPHI9
      {$DEFINE DELPHICOMPILER9}
      {$DEFINE RTL170_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER180}
      {$DEFINE BDS}
      {$IFDEF CLR}
        {$DEFINE CLR11}
      {$ENDIF CLR}
      {$IFDEF VER185}
        {$DEFINE BDS5}
        {$DEFINE COMPILER11}
        {$IFDEF BCB}
          {$DEFINE BCB11}
        {$ELSE}
          {$DEFINE DELPHI11}
          {$DEFINE DELPHI2007} // synonym to DELPHI11
          {$DEFINE DELPHICOMPILER11}
        {$ENDIF}
        {$DEFINE RTL185_UP}
      {$ELSE ~~VER185}
        {$DEFINE BDS4}
        {$DEFINE COMPILER10}
        {$IFDEF BCB}
          {$DEFINE BCB10}
        {$ELSE}
          {$DEFINE DELPHI10}
          {$DEFINE DELPHI2006} // synonym to DELPHI10
          {$DEFINE DELPHICOMPILER10}
        {$ENDIF}
        {$DEFINE RTL180_UP}
      {$ENDIF ~VER185}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER190} // Delphi 2007 for .NET
      {$DEFINE BDS}
      {$DEFINE BDS5}
      {$IFDEF CLR}
        {$DEFINE CLR20}
      {$ENDIF CLR}
      {$DEFINE COMPILER11}
      {$DEFINE DELPHI11}
      {$DEFINE DELPHI2007} // synonym to DELPHI11
      {$DEFINE DELPHICOMPILER11}
      {$DEFINE RTL190_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF VER190}

    {$IFDEF VER200} // RAD Studio 2009
      {$DEFINE BDS}
      {$DEFINE BDS6}
      {$IFDEF CLR}
        {$DEFINE CLR20}
      {$ENDIF CLR}
      {$DEFINE COMPILER12}
      {$IFDEF BCB}
        {$DEFINE BCB12}
      {$ELSE}
        {$DEFINE DELPHI12}
        {$DEFINE DELPHI2009} // synonym to DELPHI12
        {$DEFINE DELPHICOMPILER12}
      {$ENDIF BCB}
      {$IFDEF CLR}
        {$DEFINE RTL190_UP}
      {$ELSE}
        {$DEFINE RTL200_UP}
      {$ENDIF}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF VER200}

    {$IFDEF VER210} // RAD Studio 2010
      {$DEFINE BDS}
      {$DEFINE BDS7}
      {$DEFINE COMPILER14}
      {$IFDEF BCB}
        {$DEFINE BCB14}
      {$ELSE}
        {$DEFINE DELPHI14}
        {$DEFINE DELPHI2010} // synonym to DELPHI14
        {$DEFINE DELPHICOMPILER14}
      {$ENDIF BCB}
      {$DEFINE RTL210_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF VER210}

    {$IFDEF VER220} // RAD Studio XE
      {$DEFINE BDS}
      {$DEFINE BDS8}
      {$DEFINE COMPILER15}
      {$IFDEF BCB}
        {$DEFINE BCB15}
      {$ELSE}
        {$DEFINE DELPHI15}
        {$DEFINE DELPHIXE} // synonym to DELPHI15
        {$DEFINE DELPHICOMPILER15}
      {$ENDIF BCB}
      {$DEFINE RTL220_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF VER220}

    {$IFDEF VER230} // RAD Studio XE2
      {$DEFINE BDS}
      {$DEFINE BDS9}
      {$DEFINE COMPILER16}
      {$IFDEF BCB}
        {$DEFINE BCB16}
      {$ELSE}
        {$DEFINE DELPHI16}
        {$DEFINE DELPHIXE2} // synonym to DELPHI16
        {$DEFINE DELPHICOMPILER16}
      {$ENDIF BCB}
      {$DEFINE RTL230_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF VER230}

    {$IFDEF VER240} // RAD Studio XE3
      {$DEFINE BDS}
      {$DEFINE BDS10}
      {$DEFINE COMPILER17}
      {$IFDEF BCB}
        {$DEFINE BCB17}
      {$ELSE}
        {$DEFINE DELPHI17}
        {$DEFINE DELPHIXE3} // synonym to DELPHI17
        {$DEFINE DELPHICOMPILER17}
      {$ENDIF BCB}
      {$DEFINE RTL240_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF VER240}

    {$IFDEF VER250} // RAD Studio XE4
      {$DEFINE BDS}
      {$DEFINE BDS11}
      {$DEFINE COMPILER18}
      {$IFDEF BCB}
        {$DEFINE BCB18}
      {$ELSE}
        {$DEFINE DELPHI18}
        {$DEFINE DELPHIXE4} // synonym to DELPHI18
        {$DEFINE DELPHICOMPILER18}
      {$ENDIF BCB}
      {$DEFINE RTL250_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF VER250}

    {$IFDEF VER260} // RAD Studio XE5
      {$DEFINE BDS}
      {$DEFINE BDS12}
      {$DEFINE COMPILER19}
      {$IFDEF BCB}
        {$DEFINE BCB19}
      {$ELSE}
        {$DEFINE DELPHI19}
        {$DEFINE DELPHIXE5} // synonym to DELPHI19
        {$DEFINE DELPHICOMPILER19}
      {$ENDIF BCB}
      {$DEFINE RTL260_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF VER260}

    {$IFDEF VER270} // RAD Studio XE6
      {$DEFINE BDS}
      {$DEFINE BDS14}
      {$DEFINE COMPILER20}
      {$IFDEF BCB}
        {$DEFINE BCB20}
      {$ELSE}
        {$DEFINE DELPHI20}
        {$DEFINE DELPHIXE6} // synonym to DELPHI20
        {$DEFINE DELPHICOMPILER20}
      {$ENDIF BCB}
      {$DEFINE RTL270_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF VER270}

    {$IFDEF VER280} // RAD Studio XE7
      {$DEFINE BDS}
      {$DEFINE BDS15}
      {$DEFINE COMPILER21}
      {$IFDEF BCB}
        {$DEFINE BCB21}
      {$ELSE}
        {$DEFINE DELPHI21}
        {$DEFINE DELPHIXE7} // synonym to DELPHI21
        {$DEFINE DELPHICOMPILER21}
      {$ENDIF BCB}
      {$DEFINE RTL280_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF VER280}

    {$IFDEF VER290} // RAD Studio XE8
      {$DEFINE BDS}
      {$DEFINE BDS16}
      {$DEFINE COMPILER22}
      {$IFDEF BCB}
        {$DEFINE BCB22}
      {$ELSE}
        {$DEFINE DELPHI22}
        {$DEFINE DELPHIXE8} // synonym to DELPHI22
        {$DEFINE DELPHICOMPILER22}
      {$ENDIF BCB}
      {$DEFINE RTL290_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF VER290}

    {$IFDEF VER300} // RAD Studio 10
      {$DEFINE BDS}
      {$DEFINE BDS17}
      {$DEFINE COMPILER23}
      {$IFDEF BCB}
        {$DEFINE BCB23}
      {$ELSE}
        {$DEFINE DELPHI23}
        {$DEFINE DELPHIX_SEATTLE} // synonym to DELPHI23
        {$DEFINE DELPHICOMPILER23}
      {$ENDIF BCB}
      {$DEFINE RTL300_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF VER300}

    {$IFDEF VER310} // RAD Studio 10.1
      {$DEFINE BDS}
      {$DEFINE BDS18}
      {$DEFINE COMPILER24}
      {$IFDEF BCB}
        {$DEFINE BCB24}
      {$ELSE}
        {$DEFINE DELPHI24}
        {$DEFINE DELPHIX_BERLIN} // synonym to DELPHI24
        {$DEFINE DELPHICOMPILER24}
      {$ENDIF BCB}
      {$DEFINE RTL310_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF VER310}

    {$IFDEF VER320} // RAD Studio 10.2
      {$DEFINE BDS}
      {$DEFINE BDS19}
      {$DEFINE COMPILER25}
      {$IFDEF BCB}
        {$DEFINE BCB25}
      {$ELSE}
        {$DEFINE DELPHI25}
        {$DEFINE DELPHIX_TOKYO}  // synonym to DELPHI25
        {$DEFINE DELPHICOMPILER25}
      {$ENDIF BCB}
      {$DEFINE RTL320_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF VER320}

    {$IFDEF VER330} // RAD Studio 10.3
      {$DEFINE BDS}
      {$DEFINE BDS20}
      {$DEFINE COMPILER26}
      {$IFDEF BCB}
        {$DEFINE BCB26}
      {$ELSE}
        {$DEFINE DELPHI26}
        {$DEFINE DELPHIX_RIO}  // synonym to DELPHI26
        {$DEFINE DELPHICOMPILER26}
      {$ENDIF BCB}
      {$DEFINE RTL330_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF VER330}

    {$IFDEF VER340} // RAD Studio 10.4
      {$DEFINE BDS}
      {$DEFINE BDS21}
      {$DEFINE COMPILER27}
      {$IFDEF BCB}
        {$DEFINE BCB27}
      {$ELSE}
        {$DEFINE DELPHI27}
        {$DEFINE DELPHICOMPILER27}
      {$ENDIF BCB}
      {$DEFINE RTL340_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF VER340}

    {$IFDEF UNKNOWN_COMPILER_VERSION} // adjust for newer version (always use latest version)
      {$DEFINE BDS}
      {$DEFINE BDS21}
      {$DEFINE COMPILER27}
      {$IFDEF BCB}
        {$DEFINE BCB27}
      {$ELSE}
        {$DEFINE DELPHI27}
        {$DEFINE DELPHICOMPILER27}
      {$ENDIF BCB}
      {$DEFINE RTL340_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

  {$ENDIF ~KYLIX}

  {$IFDEF BCB}
    {$DEFINE CPPBUILDER}
    {$DEFINE BCBCOMPILER}
  {$ELSE ~BCB}
    {$DEFINE DELPHI}
    {$DEFINE DELPHICOMPILER}
  {$ENDIF ~BCB}

{$ENDIF BORLAND}

{------------------------------------------------------------------------------}
{ DELPHIX_UP from DELPHIX mappings                                             }
{------------------------------------------------------------------------------}

{$IFDEF DELPHI27} {$DEFINE DELPHI27_UP} {$ENDIF}
{$IFDEF DELPHI26} {$DEFINE DELPHI26_UP} {$ENDIF}
{$IFDEF DELPHI25} {$DEFINE DELPHI25_UP} {$ENDIF}
{$IFDEF DELPHI24} {$DEFINE DELPHI24_UP} {$ENDIF}
{$IFDEF DELPHI23} {$DEFINE DELPHI23_UP} {$ENDIF}
{$IFDEF DELPHI22} {$DEFINE DELPHI22_UP} {$ENDIF}
{$IFDEF DELPHI21} {$DEFINE DELPHI21_UP} {$ENDIF}
{$IFDEF DELPHI20} {$DEFINE DELPHI20_UP} {$ENDIF}
{$IFDEF DELPHI19} {$DEFINE DELPHI19_UP} {$ENDIF}
{$IFDEF DELPHI18} {$DEFINE DELPHI18_UP} {$ENDIF}
{$IFDEF DELPHI17} {$DEFINE DELPHI17_UP} {$ENDIF}
{$IFDEF DELPHI16} {$DEFINE DELPHI16_UP} {$ENDIF}
{$IFDEF DELPHI15} {$DEFINE DELPHI15_UP} {$ENDIF}
{$IFDEF DELPHI14} {$DEFINE DELPHI14_UP} {$ENDIF}
{$IFDEF DELPHI12} {$DEFINE DELPHI12_UP} {$ENDIF}
{$IFDEF DELPHI11} {$DEFINE DELPHI11_UP} {$ENDIF}
{$IFDEF DELPHI10} {$DEFINE DELPHI10_UP} {$ENDIF}
{$IFDEF DELPHI9}  {$DEFINE DELPHI9_UP}  {$ENDIF}
{$IFDEF DELPHI8}  {$DEFINE DELPHI8_UP}  {$ENDIF}
{$IFDEF DELPHI7}  {$DEFINE DELPHI7_UP}  {$ENDIF}
{$IFDEF DELPHI6}  {$DEFINE DELPHI6_UP}  {$ENDIF}
{$IFDEF DELPHI5}  {$DEFINE DELPHI5_UP}  {$ENDIF}
{$IFDEF DELPHI4}  {$DEFINE DELPHI4_UP}  {$ENDIF}
{$IFDEF DELPHI3}  {$DEFINE DELPHI3_UP}  {$ENDIF}
{$IFDEF DELPHI2}  {$DEFINE DELPHI2_UP}  {$ENDIF}
{$IFDEF DELPHI1}  {$DEFINE DELPHI1_UP}  {$ENDIF}

{------------------------------------------------------------------------------}
{ DELPHIX_UP from DELPHIX_UP mappings                                          }
{------------------------------------------------------------------------------}

{$IFDEF DELPHI27_UP}
  {$DEFINE DELPHI26_UP}
{$ENDIF}

{$IFDEF DELPHI26_UP}
  {$DEFINE DELPHIX_RIO_UP} // synonym to DELPHI26_UP
  {$DEFINE DELPHI25_UP}
{$ENDIF}

{$IFDEF DELPHI25_UP}
  {$DEFINE DELPHIX_TOKYO_UP} // synonym to DELPHI25_UP
  {$DEFINE DELPHI24_UP}
{$ENDIF}

{$IFDEF DELPHI24_UP}
  {$DEFINE DELPHIX_BERLIN_UP} // synonym to DELPHI24_UP
  {$DEFINE DELPHI23_UP}
{$ENDIF}

{$IFDEF DELPHI23_UP}
  {$DEFINE DELPHIX_SEATTLE_UP} // synonym to DELPHI23_UP
  {$DEFINE DELPHI22_UP}
{$ENDIF}

{$IFDEF DELPHI22_UP}
  {$DEFINE DELPHIXE8_UP} // synonym to DELPHI22_UP
  {$DEFINE DELPHI21_UP}
{$ENDIF}

{$IFDEF DELPHI21_UP}
  {$DEFINE DELPHIXE7_UP} // synonym to DELPHI21_UP
  {$DEFINE DELPHI20_UP}
{$ENDIF}

{$IFDEF DELPHI20_UP}
  {$DEFINE DELPHIXE6_UP} // synonym to DELPHI20_UP
  {$DEFINE DELPHI19_UP}
{$ENDIF}

{$IFDEF DELPHI19_UP}
  {$DEFINE DELPHIXE5_UP} // synonym to DELPHI19_UP
  {$DEFINE DELPHI18_UP}
{$ENDIF}

{$IFDEF DELPHI18_UP}
  {$DEFINE DELPHIXE4_UP} // synonym to DELPHI18_UP
  {$DEFINE DELPHI17_UP}
{$ENDIF}

{$IFDEF DELPHI17_UP}
  {$DEFINE DELPHIXE3_UP} // synonym to DELPHI17_UP
  {$DEFINE DELPHI16_UP}
{$ENDIF}

{$IFDEF DELPHI16_UP}
  {$DEFINE DELPHIXE2_UP} // synonym to DELPHI16_UP
  {$DEFINE DELPHI15_UP}
{$ENDIF}

{$IFDEF DELPHI15_UP}
  {$DEFINE DELPHIXE_UP} // synonym to DELPHI15_UP
  {$DEFINE DELPHI14_UP}
{$ENDIF}

{$IFDEF DELPHI14_UP}
  {$DEFINE DELPHI2010_UP} // synonym to DELPHI14_UP
  {$DEFINE DELPHI12_UP}
{$ENDIF}

{$IFDEF DELPHI12_UP}
  {$DEFINE DELPHI2009_UP} // synonym to DELPHI12_UP
  {$DEFINE DELPHI11_UP}
{$ENDIF}

{$IFDEF DELPHI11_UP}
  {$DEFINE DELPHI2007_UP} // synonym to DELPHI11_UP
  {$DEFINE DELPHI10_UP}
{$ENDIF}

{$IFDEF DELPHI10_UP}
  {$DEFINE DELPHI2006_UP} // synonym to DELPHI10_UP
  {$DEFINE DELPHI9_UP}
{$ENDIF}

{$IFDEF DELPHI9_UP}
  {$DEFINE DELPHI2005_UP} // synonym to DELPHI9_UP
  {$DEFINE DELPHI8_UP}
{$ENDIF}

{$IFDEF DELPHI8_UP} {$DEFINE DELPHI7_UP} {$ENDIF}
{$IFDEF DELPHI7_UP} {$DEFINE DELPHI6_UP} {$ENDIF}
{$IFDEF DELPHI6_UP} {$DEFINE DELPHI5_UP} {$ENDIF}
{$IFDEF DELPHI5_UP} {$DEFINE DELPHI4_UP} {$ENDIF}
{$IFDEF DELPHI4_UP} {$DEFINE DELPHI3_UP} {$ENDIF}
{$IFDEF DELPHI3_UP} {$DEFINE DELPHI2_UP} {$ENDIF}
{$IFDEF DELPHI2_UP} {$DEFINE DELPHI1_UP} {$ENDIF}

{------------------------------------------------------------------------------}
{ BCBX_UP from BCBX mappings                                                   }
{------------------------------------------------------------------------------}

{$IFDEF BCB27} {$DEFINE BCB27_UP} {$ENDIF}
{$IFDEF BCB26} {$DEFINE BCB26_UP} {$ENDIF}
{$IFDEF BCB25} {$DEFINE BCB25_UP} {$ENDIF}
{$IFDEF BCB24} {$DEFINE BCB24_UP} {$ENDIF}
{$IFDEF BCB23} {$DEFINE BCB23_UP} {$ENDIF}
{$IFDEF BCB22} {$DEFINE BCB22_UP} {$ENDIF}
{$IFDEF BCB21} {$DEFINE BCB21_UP} {$ENDIF}
{$IFDEF BCB20} {$DEFINE BCB20_UP} {$ENDIF}
{$IFDEF BCB19} {$DEFINE BCB19_UP} {$ENDIF}
{$IFDEF BCB18} {$DEFINE BCB18_UP} {$ENDIF}
{$IFDEF BCB17} {$DEFINE BCB17_UP} {$ENDIF}
{$IFDEF BCB16} {$DEFINE BCB16_UP} {$ENDIF}
{$IFDEF BCB15} {$DEFINE BCB15_UP} {$ENDIF}
{$IFDEF BCB14} {$DEFINE BCB14_UP} {$ENDIF}
{$IFDEF BCB12} {$DEFINE BCB12_UP} {$ENDIF}
{$IFDEF BCB11} {$DEFINE BCB11_UP} {$ENDIF}
{$IFDEF BCB10} {$DEFINE BCB10_UP} {$ENDIF}
{$IFDEF BCB6}  {$DEFINE BCB6_UP}  {$ENDIF}
{$IFDEF BCB5}  {$DEFINE BCB5_UP}  {$ENDIF}
{$IFDEF BCB4}  {$DEFINE BCB4_UP}  {$ENDIF}
{$IFDEF BCB3}  {$DEFINE BCB3_UP}  {$ENDIF}
{$IFDEF BCB1}  {$DEFINE BCB1_UP}  {$ENDIF}

{------------------------------------------------------------------------------}
{ BCBX_UP from BCBX_UP mappings                                                }
{------------------------------------------------------------------------------}

{$IFDEF BCB27_UP} {$DEFINE BCB26_UP} {$ENDIF}
{$IFDEF BCB26_UP} {$DEFINE BCB25_UP} {$ENDIF}
{$IFDEF BCB25_UP} {$DEFINE BCB24_UP} {$ENDIF}
{$IFDEF BCB24_UP} {$DEFINE BCB23_UP} {$ENDIF}
{$IFDEF BCB23_UP} {$DEFINE BCB22_UP} {$ENDIF}
{$IFDEF BCB22_UP} {$DEFINE BCB21_UP} {$ENDIF}
{$IFDEF BCB21_UP} {$DEFINE BCB20_UP} {$ENDIF}
{$IFDEF BCB20_UP} {$DEFINE BCB19_UP} {$ENDIF}
{$IFDEF BCB19_UP} {$DEFINE BCB18_UP} {$ENDIF}
{$IFDEF BCB18_UP} {$DEFINE BCB17_UP} {$ENDIF}
{$IFDEF BCB17_UP} {$DEFINE BCB16_UP} {$ENDIF}
{$IFDEF BCB16_UP} {$DEFINE BCB15_UP} {$ENDIF}
{$IFDEF BCB15_UP} {$DEFINE BCB14_UP} {$ENDIF}
{$IFDEF BCB14_UP} {$DEFINE BCB12_UP} {$ENDIF}
{$IFDEF BCB12_UP} {$DEFINE BCB11_UP} {$ENDIF}
{$IFDEF BCB11_UP} {$DEFINE BCB10_UP} {$ENDIF}
{$IFDEF BCB10_UP} {$DEFINE BCB6_UP}  {$ENDIF}
{$IFDEF BCB6_UP}  {$DEFINE BCB5_UP}  {$ENDIF}
{$IFDEF BCB5_UP}  {$DEFINE BCB4_UP}  {$ENDIF}
{$IFDEF BCB4_UP}  {$DEFINE BCB3_UP}  {$ENDIF}
{$IFDEF BCB3_UP}  {$DEFINE BCB1_UP}  {$ENDIF}

{------------------------------------------------------------------------------}
{ BDSX_UP from BDSX mappings                                                   }
{------------------------------------------------------------------------------}

{$IFDEF BDS21} {$DEFINE BDS21_UP} {$ENDIF}
{$IFDEF BDS20} {$DEFINE BDS20_UP} {$ENDIF}
{$IFDEF BDS19} {$DEFINE BDS19_UP} {$ENDIF}
{$IFDEF BDS18} {$DEFINE BDS18_UP} {$ENDIF}
{$IFDEF BDS17} {$DEFINE BDS17_UP} {$ENDIF}
{$IFDEF BDS16} {$DEFINE BDS16_UP} {$ENDIF}
{$IFDEF BDS15} {$DEFINE BDS15_UP} {$ENDIF}
{$IFDEF BDS14} {$DEFINE BDS14_UP} {$ENDIF}
{$IFDEF BDS12} {$DEFINE BDS12_UP} {$ENDIF}
{$IFDEF BDS11} {$DEFINE BDS11_UP} {$ENDIF}
{$IFDEF BDS10} {$DEFINE BDS10_UP} {$ENDIF}
{$IFDEF BDS9} {$DEFINE BDS9_UP} {$ENDIF}
{$IFDEF BDS8} {$DEFINE BDS8_UP} {$ENDIF}
{$IFDEF BDS7} {$DEFINE BDS7_UP} {$ENDIF}
{$IFDEF BDS6} {$DEFINE BDS6_UP} {$ENDIF}
{$IFDEF BDS5} {$DEFINE BDS5_UP} {$ENDIF}
{$IFDEF BDS4} {$DEFINE BDS4_UP} {$ENDIF}
{$IFDEF BDS3} {$DEFINE BDS3_UP} {$ENDIF}
{$IFDEF BDS2} {$DEFINE BDS2_UP} {$ENDIF}

{------------------------------------------------------------------------------}
{ BDSX_UP from BDSX_UP mappings                                                }
{------------------------------------------------------------------------------}

{$IFDEF BDS21_UP} {$DEFINE BDS20_UP} {$ENDIF}
{$IFDEF BDS20_UP} {$DEFINE BDS19_UP} {$ENDIF}
{$IFDEF BDS19_UP} {$DEFINE BDS18_UP} {$ENDIF}
{$IFDEF BDS18_UP} {$DEFINE BDS17_UP} {$ENDIF}
{$IFDEF BDS17_UP} {$DEFINE BDS16_UP} {$ENDIF}
{$IFDEF BDS16_UP} {$DEFINE BDS15_UP} {$ENDIF}
{$IFDEF BDS15_UP} {$DEFINE BDS14_UP} {$ENDIF}
{$IFDEF BDS14_UP} {$DEFINE BDS12_UP} {$ENDIF}
{$IFDEF BDS12_UP} {$DEFINE BDS11_UP} {$ENDIF}
{$IFDEF BDS11_UP} {$DEFINE BDS10_UP} {$ENDIF}
{$IFDEF BDS10_UP} {$DEFINE BDS9_UP} {$ENDIF}
{$IFDEF BDS9_UP} {$DEFINE BDS8_UP} {$ENDIF}
{$IFDEF BDS8_UP} {$DEFINE BDS7_UP} {$ENDIF}
{$IFDEF BDS7_UP} {$DEFINE BDS6_UP} {$ENDIF}
{$IFDEF BDS6_UP} {$DEFINE BDS5_UP} {$ENDIF}
{$IFDEF BDS5_UP} {$DEFINE BDS4_UP} {$ENDIF}
{$IFDEF BDS4_UP} {$DEFINE BDS3_UP} {$ENDIF}
{$IFDEF BDS3_UP} {$DEFINE BDS2_UP} {$ENDIF}

{------------------------------------------------------------------------------}
{ DELPHICOMPILERX_UP from DELPHICOMPILERX mappings                             }
{------------------------------------------------------------------------------}

{$IFDEF DELPHICOMPILER27} {$DEFINE DELPHICOMPILER27_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER26} {$DEFINE DELPHICOMPILER26_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER25} {$DEFINE DELPHICOMPILER25_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER24} {$DEFINE DELPHICOMPILER24_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER23} {$DEFINE DELPHICOMPILER23_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER22} {$DEFINE DELPHICOMPILER22_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER21} {$DEFINE DELPHICOMPILER21_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER20} {$DEFINE DELPHICOMPILER20_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER19} {$DEFINE DELPHICOMPILER19_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER18} {$DEFINE DELPHICOMPILER18_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER17} {$DEFINE DELPHICOMPILER17_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER16} {$DEFINE DELPHICOMPILER16_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER15} {$DEFINE DELPHICOMPILER15_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER14} {$DEFINE DELPHICOMPILER14_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER12} {$DEFINE DELPHICOMPILER12_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER11} {$DEFINE DELPHICOMPILER11_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER10} {$DEFINE DELPHICOMPILER10_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER9}  {$DEFINE DELPHICOMPILER9_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER8}  {$DEFINE DELPHICOMPILER8_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER7}  {$DEFINE DELPHICOMPILER7_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER6}  {$DEFINE DELPHICOMPILER6_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER5}  {$DEFINE DELPHICOMPILER5_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER4}  {$DEFINE DELPHICOMPILER4_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER3}  {$DEFINE DELPHICOMPILER3_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER2}  {$DEFINE DELPHICOMPILER2_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER1}  {$DEFINE DELPHICOMPILER1_UP}  {$ENDIF}

{------------------------------------------------------------------------------}
{ DELPHICOMPILERX_UP from DELPHICOMPILERX_UP mappings                          }
{------------------------------------------------------------------------------}

{$IFDEF DELPHICOMPILER27_UP} {$DEFINE DELPHICOMPILER26_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER26_UP} {$DEFINE DELPHICOMPILER25_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER25_UP} {$DEFINE DELPHICOMPILER24_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER24_UP} {$DEFINE DELPHICOMPILER23_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER23_UP} {$DEFINE DELPHICOMPILER22_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER22_UP} {$DEFINE DELPHICOMPILER21_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER21_UP} {$DEFINE DELPHICOMPILER20_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER20_UP} {$DEFINE DELPHICOMPILER19_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER19_UP} {$DEFINE DELPHICOMPILER18_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER18_UP} {$DEFINE DELPHICOMPILER17_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER17_UP} {$DEFINE DELPHICOMPILER16_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER16_UP} {$DEFINE DELPHICOMPILER15_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER15_UP} {$DEFINE DELPHICOMPILER14_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER14_UP} {$DEFINE DELPHICOMPILER12_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER12_UP} {$DEFINE DELPHICOMPILER11_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER11_UP} {$DEFINE DELPHICOMPILER10_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER10_UP} {$DEFINE DELPHICOMPILER9_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER9_UP}  {$DEFINE DELPHICOMPILER8_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER8_UP}  {$DEFINE DELPHICOMPILER7_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER8_UP}  {$DEFINE DELPHICOMPILER7_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER7_UP}  {$DEFINE DELPHICOMPILER6_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER6_UP}  {$DEFINE DELPHICOMPILER5_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER5_UP}  {$DEFINE DELPHICOMPILER4_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER4_UP}  {$DEFINE DELPHICOMPILER3_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER3_UP}  {$DEFINE DELPHICOMPILER2_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER2_UP}  {$DEFINE DELPHICOMPILER1_UP}  {$ENDIF}

{------------------------------------------------------------------------------}
{ COMPILERX_UP from COMPILERX mappings                                         }
{------------------------------------------------------------------------------}

{$IFDEF COMPILER27} {$DEFINE COMPILER27_UP} {$ENDIF}
{$IFDEF COMPILER26} {$DEFINE COMPILER26_UP} {$ENDIF}
{$IFDEF COMPILER25} {$DEFINE COMPILER25_UP} {$ENDIF}
{$IFDEF COMPILER24} {$DEFINE COMPILER24_UP} {$ENDIF}
{$IFDEF COMPILER23} {$DEFINE COMPILER23_UP} {$ENDIF}
{$IFDEF COMPILER22} {$DEFINE COMPILER22_UP} {$ENDIF}
{$IFDEF COMPILER21} {$DEFINE COMPILER21_UP} {$ENDIF}
{$IFDEF COMPILER20} {$DEFINE COMPILER20_UP} {$ENDIF}
{$IFDEF COMPILER19} {$DEFINE COMPILER19_UP} {$ENDIF}
{$IFDEF COMPILER18} {$DEFINE COMPILER18_UP} {$ENDIF}
{$IFDEF COMPILER17} {$DEFINE COMPILER17_UP} {$ENDIF}
{$IFDEF COMPILER16} {$DEFINE COMPILER16_UP} {$ENDIF}
{$IFDEF COMPILER15} {$DEFINE COMPILER15_UP} {$ENDIF}
{$IFDEF COMPILER14} {$DEFINE COMPILER14_UP} {$ENDIF}
{$IFDEF COMPILER12} {$DEFINE COMPILER12_UP} {$ENDIF}
{$IFDEF COMPILER11} {$DEFINE COMPILER11_UP} {$ENDIF}
{$IFDEF COMPILER10} {$DEFINE COMPILER10_UP} {$ENDIF}
{$IFDEF COMPILER9}  {$DEFINE COMPILER9_UP}  {$ENDIF}
{$IFDEF COMPILER8}  {$DEFINE COMPILER8_UP}  {$ENDIF}
{$IFDEF COMPILER7}  {$DEFINE COMPILER7_UP}  {$ENDIF}
{$IFDEF COMPILER6}  {$DEFINE COMPILER6_UP}  {$ENDIF}
{$IFDEF COMPILER5}  {$DEFINE COMPILER5_UP}  {$ENDIF}
{$IFDEF COMPILER4}  {$DEFINE COMPILER4_UP}  {$ENDIF}
{$IFDEF COMPILER35} {$DEFINE COMPILER35_UP} {$ENDIF}
{$IFDEF COMPILER3}  {$DEFINE COMPILER3_UP}  {$ENDIF}
{$IFDEF COMPILER2}  {$DEFINE COMPILER2_UP}  {$ENDIF}
{$IFDEF COMPILER1}  {$DEFINE COMPILER1_UP}  {$ENDIF}

{------------------------------------------------------------------------------}
{ COMPILERX_UP from COMPILERX_UP mappings                                      }
{------------------------------------------------------------------------------}

{$IFDEF COMPILER27_UP} {$DEFINE COMPILER26_UP} {$ENDIF}
{$IFDEF COMPILER26_UP} {$DEFINE COMPILER25_UP} {$ENDIF}
{$IFDEF COMPILER25_UP} {$DEFINE COMPILER24_UP} {$ENDIF}
{$IFDEF COMPILER24_UP} {$DEFINE COMPILER23_UP} {$ENDIF}
{$IFDEF COMPILER23_UP} {$DEFINE COMPILER22_UP} {$ENDIF}
{$IFDEF COMPILER22_UP} {$DEFINE COMPILER21_UP} {$ENDIF}
{$IFDEF COMPILER21_UP} {$DEFINE COMPILER20_UP} {$ENDIF}
{$IFDEF COMPILER20_UP} {$DEFINE COMPILER19_UP} {$ENDIF}
{$IFDEF COMPILER19_UP} {$DEFINE COMPILER18_UP} {$ENDIF}
{$IFDEF COMPILER18_UP} {$DEFINE COMPILER17_UP} {$ENDIF}
{$IFDEF COMPILER17_UP} {$DEFINE COMPILER16_UP} {$ENDIF}
{$IFDEF COMPILER16_UP} {$DEFINE COMPILER15_UP} {$ENDIF}
{$IFDEF COMPILER15_UP} {$DEFINE COMPILER14_UP} {$ENDIF}
{$IFDEF COMPILER14_UP} {$DEFINE COMPILER12_UP} {$ENDIF}
{$IFDEF COMPILER12_UP} {$DEFINE COMPILER11_UP} {$ENDIF}
{$IFDEF COMPILER11_UP} {$DEFINE COMPILER10_UP} {$ENDIF}
{$IFDEF COMPILER10_UP} {$DEFINE COMPILER9_UP}  {$ENDIF}
{$IFDEF COMPILER9_UP}  {$DEFINE COMPILER8_UP}  {$ENDIF}
{$IFDEF COMPILER8_UP}  {$DEFINE COMPILER7_UP}  {$ENDIF}
{$IFDEF COMPILER7_UP}  {$DEFINE COMPILER6_UP}  {$ENDIF}
{$IFDEF COMPILER6_UP}  {$DEFINE COMPILER5_UP}  {$ENDIF}
{$IFDEF COMPILER5_UP}  {$DEFINE COMPILER4_UP}  {$ENDIF}
{$IFDEF COMPILER4_UP}  {$DEFINE COMPILER35_UP} {$ENDIF}
{$IFDEF COMPILER35_UP} {$DEFINE COMPILER3_UP}  {$ENDIF}
{$IFDEF COMPILER3_UP}  {$DEFINE COMPILER2_UP}  {$ENDIF}
{$IFDEF COMPILER2_UP}  {$DEFINE COMPILER1_UP}  {$ENDIF}

{------------------------------------------------------------------------------}
{ RTLX_UP from RTLX_UP mappings                                                }
{------------------------------------------------------------------------------}

{$IFDEF RTL340_UP} {$DEFINE RTL330_UP} {$ENDIF}
{$IFDEF RTL330_UP} {$DEFINE RTL320_UP} {$ENDIF}
{$IFDEF RTL320_UP} {$DEFINE RTL310_UP} {$ENDIF}
{$IFDEF RTL310_UP} {$DEFINE RTL300_UP} {$ENDIF}
{$IFDEF RTL300_UP} {$DEFINE RTL290_UP} {$ENDIF}
{$IFDEF RTL290_UP} {$DEFINE RTL280_UP} {$ENDIF}
{$IFDEF RTL280_UP} {$DEFINE RTL270_UP} {$ENDIF}
{$IFDEF RTL270_UP} {$DEFINE RTL260_UP} {$ENDIF}
{$IFDEF RTL260_UP} {$DEFINE RTL250_UP} {$ENDIF}
{$IFDEF RTL250_UP} {$DEFINE RTL240_UP} {$ENDIF}
{$IFDEF RTL240_UP} {$DEFINE RTL230_UP} {$ENDIF}
{$IFDEF RTL230_UP} {$DEFINE RTL220_UP} {$ENDIF}
{$IFDEF RTL220_UP} {$DEFINE RTL210_UP} {$ENDIF}
{$IFDEF RTL210_UP} {$DEFINE RTL200_UP} {$ENDIF}
{$IFDEF RTL200_UP} {$DEFINE RTL190_UP} {$ENDIF}
{$IFDEF RTL190_UP} {$DEFINE RTL185_UP} {$ENDIF}
{$IFDEF RTL185_UP} {$DEFINE RTL180_UP} {$ENDIF}
{$IFDEF RTL180_UP} {$DEFINE RTL170_UP} {$ENDIF}
{$IFDEF RTL170_UP} {$DEFINE RTL160_UP} {$ENDIF}
{$IFDEF RTL160_UP} {$DEFINE RTL150_UP} {$ENDIF}
{$IFDEF RTL150_UP} {$DEFINE RTL145_UP} {$ENDIF}
{$IFDEF RTL145_UP} {$DEFINE RTL142_UP} {$ENDIF}
{$IFDEF RTL142_UP} {$DEFINE RTL140_UP} {$ENDIF}
{$IFDEF RTL140_UP} {$DEFINE RTL130_UP} {$ENDIF}
{$IFDEF RTL130_UP} {$DEFINE RTL125_UP} {$ENDIF}
{$IFDEF RTL125_UP} {$DEFINE RTL120_UP} {$ENDIF}
{$IFDEF RTL120_UP} {$DEFINE RTL110_UP} {$ENDIF}
{$IFDEF RTL110_UP} {$DEFINE RTL100_UP} {$ENDIF}
{$IFDEF RTL100_UP} {$DEFINE RTL93_UP}  {$ENDIF}
{$IFDEF RTL93_UP}  {$DEFINE RTL90_UP}  {$ENDIF}
{$IFDEF RTL90_UP}  {$DEFINE RTL80_UP}  {$ENDIF}

{------------------------------------------------------------------------------}
{ Check for CLR overrides of default detection                                 }
{------------------------------------------------------------------------------}

{$IFDEF CLR}
  {$IFDEF FORCE_CLR10}
    {$DEFINE CLR10}
    {$UNDEF CLR11}
    {$UNDEF CLR20}
  {$ENDIF FORCE_CLR10}

  {$IFDEF FORCE_CLR11}
    {$UNDEF CLR10}
    {$DEFINE CLR11}
    {$UNDEF CLR20}
  {$ENDIF FORCE_CLR11}

  {$IFDEF FORCE_CLR20}
    {$UNDEF CLR10}
    {$UNDEF CLR11}
    {$DEFINE CLR20}
  {$ENDIF FORCE_CLR20}
{$ENDIF CLR}

{------------------------------------------------------------------------------}
{ CLRX from CLRX_UP mappings                                                   }
{------------------------------------------------------------------------------}

{$IFDEF CLR10} {$DEFINE CLR10_UP} {$ENDIF}
{$IFDEF CLR11} {$DEFINE CLR11_UP} {$ENDIF}
{$IFDEF CLR20} {$DEFINE CLR20_UP} {$ENDIF}

{------------------------------------------------------------------------------}
{ CLRX_UP from CLRX_UP mappings                                                }
{------------------------------------------------------------------------------}

{$IFDEF CLR20_UP} {$DEFINE CLR11_UP} {$ENDIF}
{$IFDEF CLR11_UP} {$DEFINE CLR10_UP} {$ENDIF}

{------------------------------------------------------------------------------}

{$IFDEF DELPHICOMPILER}
  {$DEFINE DELPHILANGUAGE}
{$ENDIF}

{$IFDEF BCBCOMPILER}
  {$DEFINE DELPHILANGUAGE}
{$ENDIF}

{------------------------------------------------------------------------------}
{ KYLIXX_UP from KYLIXX mappings                                               }
{------------------------------------------------------------------------------}

{$IFDEF KYLIX3} {$DEFINE KYLIX3_UP} {$ENDIF}
{$IFDEF KYLIX2} {$DEFINE KYLIX2_UP} {$ENDIF}
{$IFDEF KYLIX1} {$DEFINE KYLIX1_UP} {$ENDIF}

{------------------------------------------------------------------------------}
{ KYLIXX_UP from KYLIXX_UP mappings                                            }
{------------------------------------------------------------------------------}

{$IFDEF KYLIX3_UP} {$DEFINE KYLIX2_UP} {$ENDIF}
{$IFDEF KYLIX2_UP} {$DEFINE KYLIX1_UP} {$ENDIF}

{------------------------------------------------------------------------------}
{ Map COMPILERX_UP to friendly feature names                                   }
{------------------------------------------------------------------------------}

{$IFDEF FPC}
  {$IFDEF  VER1_0}
     Please use FPC 2.0 or higher to compile this.
  {$ELSE}
    { FPC_FULLVERSION is available from 2.2.4 on }

    {$DEFINE SUPPORTS_OUTPARAMS}
    {$DEFINE SUPPORTS_WIDECHAR}
    {$DEFINE SUPPORTS_WIDESTRING}
    {$IF DEFINED(VER2_0) OR DEFINED(VER2_1)}
      {$IFDEF HASINTF}
        {$DEFINE SUPPORTS_INTERFACE}
      {$ENDIF}
      {$IFDEF HASVARIANT}
        {$DEFINE SUPPORTS_VARIANT}
      {$ENDIF}
      {$IFDEF HASCURRENCY}
        {$DEFINE SUPPORTS_CURRENCY}
      {$ENDIF}
    {$ELSE}
      {$DEFINE SUPPORTS_INTERFACE}
      {$DEFINE SUPPORTS_VARIANT}
      {$DEFINE SUPPORTS_CURRENCY}
    {$IFEND}
    {$IFDEF FPC_HAS_TYPE_SINGLE}
      {$DEFINE SUPPORTS_SINGLE}
    {$ENDIF}
    {$IFDEF FPC_HAS_TYPE_DOUBLE}
      {$DEFINE SUPPORTS_DOUBLE}
    {$ENDIF}
    {$IFDEF FPC_HAS_TYPE_EXTENDED}
      {$DEFINE SUPPORTS_EXTENDED}
    {$ENDIF}
    {$DEFINE SUPPORTS_THREADVAR}
    {$DEFINE SUPPORTS_CONSTPARAMS}
    {$DEFINE SUPPORTS_LONGWORD}
    {$DEFINE SUPPORTS_INT64}
    {$DEFINE SUPPORTS_DYNAMICARRAYS}
    {$DEFINE SUPPORTS_DEFAULTPARAMS}
    {$DEFINE SUPPORTS_OVERLOAD}
    {$DEFINE ACCEPT_DEPRECATED}  // 2.2 also gives warnings
    {$DEFINE ACCEPT_PLATFORM}    // 2.2 also gives warnings
    {$DEFINE ACCEPT_LIBRARY}
    {$DEFINE SUPPORTS_DEPRECATED}
    {$DEFINE SUPPORTS_PLATFORM}
    {$DEFINE SUPPORTS_LIBRARY}
    {$DEFINE SUPPORTS_DEPRECATED_WARNINGS}
    {$DEFINE SUPPORTS_PLATFORM_WARNINGS}
    {$DEFINE SUPPORTS_EXTSYM}
    {$DEFINE SUPPORTS_NODEFINE}
    {$DEFINE SUPPORTS_DISPINTERFACE}
    {$DEFINE SUPPORTS_IMPLEMENTS}
    {$DEFINE SUPPORTS_DISPID}
    {$DEFINE SUPPORTS_INLINE}
    {$DEFINE SUPPORTS_STATIC}
    {$DEFINE SUPPORTS_COMPILETIME_MESSAGES}

    {$DEFINE SUPPORTS_CUSTOMVARIANTS}
    {$DEFINE SUPPORTS_VARARGS}
    {$DEFINE SUPPORTS_ENUMVALUE}
    {$IF DEFINED(LINUX) AND DEFINED(CPU386)}
      {$DEFINE HAS_UNIT_LIBC}
    {$IFEND}
    {$DEFINE HAS_UNIT_CONTNRS}
    {$DEFINE HAS_UNIT_TYPES}
    {$DEFINE HAS_UNIT_VARIANTS}
    {$DEFINE HAS_UNIT_STRUTILS}
    {$DEFINE HAS_UNIT_DATEUTILS}
    {$DEFINE HAS_UNIT_RTLCONSTS}

    {$DEFINE XPLATFORM_RTL}

    {$IF DEFINED(FPC_FULLVERSION)}
      { 2.2.4 or newer }

      {$DEFINE SUPPORTS_SETPEFLAGS}
      {$DEFINE SUPPORTS_STRICT}

      {$IF defined(FPC_FULLVERSION) and (FPC_FULLVERSION >= 20400)}
        {$DEFINE SUPPORTS_UINT64}
        {$DEFINE SUPPORTS_EXPERIMENTAL_WARNINGS}
        {$DEFINE SUPPORTS_REGION}
        {$DEFINE SUPPORTS_UNICODE_STRING}
      {$IFEND}

      {$IF defined(FPC_FULLVERSION) and (FPC_FULLVERSION >= 20402)}
        {$DEFINE SUPPORTS_FOR_IN}
      {$IFEND}

      {$IF defined(FPC_FULLVERSION) and (FPC_FULLVERSION >= 20600)}
        {$DEFINE SUPPORTS_LIBRARY_WARNINGS}
        {$DEFINE SUPPORTS_DEPRECATED_DETAILS}
        {$DEFINE SUPPORTS_NESTED_TYPES}
        {$DEFINE SUPPORTS_NESTED_CONSTANTS}
        {$DEFINE SUPPORTS_ENHANCED_RECORDS} // called Advanced Records in FPC
        {$DEFINE SUPPORTS_CLASS_FIELDS}
        {$DEFINE SUPPORTS_CLASS_HELPERS}
        {$DEFINE SUPPORTS_CLASS_OPERATORS}
        {$DEFINE SUPPORTS_CLASS_CTORDTORS}
        {$DEFINE SUPPORTS_FINAL}
        {$DEFINE SUPPORTS_CAST_INTERFACE_TO_OBJ}

        {$DEFINE HAS_ENOTIMPLEMENTED}
      {$IFEND}

      {$IF defined(FPC_FULLVERSION) and (FPC_FULLVERSION >= 20602)}
        {$DEFINE SUPPORTS_INT_ALIASES}

        {$DEFINE HAS_EARGUMENTEXCEPTION}
      {$IFEND}

      {$IF defined(FPC_FULLVERSION) and (FPC_FULLVERSION >= 30000)}
        {$DEFINE SUPPORTS_GENERICS}
        {$DEFINE SUPPORTS_GENERIC_TYPES}

        {$DEFINE HAS_UNIT_CHARACTER}
      {$IFEND}

      {$IF defined(FPC_FULLVERSION) and (FPC_FULLVERSION >= 30200)}
        {$DEFINE SUPPORTS_GENERIC_METHODS}
        {$DEFINE SUPPORTS_GENERIC_ROUTINES}
        {$DEFINE SUPPORTS_WEAKPACKAGEUNIT}

        {$DEFINE HAS_UNIT_RTTI}
        {$DEFINE HAS_UNIT_SYSTEM_UITYPES}
      {$IFEND}
    {$ELSE}
      { older than 2.2.4 }

      {$IFDEF VER2_2}
        {$SUPPORTS_SETPEFLAGS}
        {$SUPPORTS_STRICT}
      {$ENDIF}
    {$IFEND}
  {$ENDIF}
{$ENDIF FPC}

{$IFDEF CLR}
  {$DEFINE SUPPORTS_UNICODE}
{$ENDIF CLR}

{$IFDEF COMPILER1_UP}
  {$DEFINE SUPPORTS_CONSTPARAMS}
  {$DEFINE SUPPORTS_SINGLE}
  {$DEFINE SUPPORTS_DOUBLE}
  {$DEFINE SUPPORTS_EXTENDED}
  {$DEFINE SUPPORTS_PACKAGES} 
{$ENDIF COMPILER1_UP}

{$IFDEF COMPILER2_UP}
  {$DEFINE SUPPORTS_CURRENCY}
  {$DEFINE SUPPORTS_THREADVAR}
  {$DEFINE SUPPORTS_VARIANT}
  {$DEFINE SUPPORTS_WIDECHAR}
{$ENDIF COMPILER2_UP}

{$IFDEF COMPILER3_UP}
  {$DEFINE SUPPORTS_OUTPARAMS}
  {$DEFINE SUPPORTS_WIDESTRING}
  {$DEFINE SUPPORTS_INTERFACE}
  {$DEFINE SUPPORTS_DISPINTERFACE}
  {$DEFINE SUPPORTS_DISPID}
  {$DEFINE SUPPORTS_WEAKPACKAGEUNIT}
{$ENDIF COMPILER3_UP}

{$IFDEF COMPILER35_UP}
  {$DEFINE SUPPORTS_EXTSYM}
  {$DEFINE SUPPORTS_NODEFINE}
{$ENDIF COMPILER35_UP}

{$IFDEF COMPILER4_UP}
  {$DEFINE SUPPORTS_LONGWORD}
  {$DEFINE SUPPORTS_INT64}
  {$DEFINE SUPPORTS_DYNAMICARRAYS}
  {$DEFINE SUPPORTS_DEFAULTPARAMS}
  {$DEFINE SUPPORTS_OVERLOAD}
  {$DEFINE SUPPORTS_IMPLEMENTS}
{$ENDIF COMPILER4_UP}

{$IFDEF COMPILER6_UP}
  {$DEFINE SUPPORTS_DEPRECATED}
  {$DEFINE SUPPORTS_LIBRARY}
  {$DEFINE SUPPORTS_PLATFORM}
  {$DEFINE SUPPORTS_LOCAL}
  {$DEFINE SUPPORTS_SETPEFLAGS}
  {$DEFINE SUPPORTS_EXPERIMENTAL_WARNINGS}
  {$DEFINE ACCEPT_DEPRECATED}
  {$DEFINE ACCEPT_PLATFORM}
  {$DEFINE ACCEPT_LIBRARY}
  {$DEFINE SUPPORTS_DEPRECATED_WARNINGS}
  {$DEFINE SUPPORTS_LIBRARY_WARNINGS}
  {$DEFINE SUPPORTS_PLATFORM_WARNINGS}
  {$DEFINE SUPPORTS_CUSTOMVARIANTS}
  {$DEFINE SUPPORTS_VARARGS}
  {$DEFINE SUPPORTS_ENUMVALUE}
  {$DEFINE SUPPORTS_COMPILETIME_MESSAGES}
{$ENDIF COMPILER6_UP}

{$IFDEF COMPILER7_UP}
  {$DEFINE SUPPORTS_UNSAFE_WARNINGS}
  {$DEFINE SUPPORTS_UINT64}
{$ENDIF COMPILER7_UP}

{$IFDEF COMPILER9_UP}
  {$DEFINE SUPPORTS_FOR_IN}
  {$DEFINE SUPPORTS_INLINE}
  {$DEFINE SUPPORTS_NESTED_CONSTANTS}
  {$DEFINE SUPPORTS_NESTED_TYPES}
  {$DEFINE SUPPORTS_REGION}
  {$IFDEF CLR}
    {$DEFINE SUPPORTS_ENHANCED_RECORDS}
    {$DEFINE SUPPORTS_CLASS_FIELDS}
    {$DEFINE SUPPORTS_CLASS_HELPERS}
    {$DEFINE SUPPORTS_CLASS_OPERATORS}
    {$DEFINE SUPPORTS_STRICT}
    {$DEFINE SUPPORTS_STATIC}
    {$DEFINE SUPPORTS_FINAL}
  {$ENDIF CLR}
{$ENDIF COMPILER9_UP}

{$IFDEF COMPILER10_UP}
  {$DEFINE SUPPORTS_ENHANCED_RECORDS}
  {$DEFINE SUPPORTS_CLASS_FIELDS}
  {$DEFINE SUPPORTS_CLASS_HELPERS}
  {$DEFINE SUPPORTS_CLASS_OPERATORS}
  {$DEFINE SUPPORTS_STRICT}
  {$DEFINE SUPPORTS_STATIC}
  {$DEFINE SUPPORTS_FINAL}
  {$DEFINE SUPPORTS_METHODINFO}
{$ENDIF COMPILER10_UP}

{$IFDEF COMPILER11_UP}
  {$IFDEF CLR}
    {$DEFINE SUPPORTS_GENERICS}
    {$DEFINE SUPPORTS_GENERIC_TYPES}
    {$DEFINE SUPPORTS_GENERIC_METHODS}
    {$DEFINE SUPPORTS_DEPRECATED_DETAILS}
  {$ENDIF CLR}
{$ENDIF COMPILER11_UP}

{$IFDEF COMPILER12_UP}
  {$DEFINE SUPPORTS_GENERICS}
  {$DEFINE SUPPORTS_GENERIC_TYPES}
  {$DEFINE SUPPORTS_GENERIC_METHODS}
  {$DEFINE SUPPORTS_DEPRECATED_DETAILS}
  {$DEFINE SUPPORTS_INT_ALIASES}
  {$IFNDEF CLR}
    {$DEFINE SUPPORTS_UNICODE}
    {$DEFINE SUPPORTS_UNICODE_STRING}
  {$ENDIF  CLR}
{$ENDIF COMPILER12_UP}

{$IFDEF COMPILER14_UP}
  {$DEFINE SUPPORTS_CLASS_CTORDTORS}
  {$DEFINE HAS_UNIT_RTTI}
  {$DEFINE SUPPORTS_CAST_INTERFACE_TO_OBJ}
  {$DEFINE SUPPORTS_DELAYED_LOADING}
{$ENDIF COMPILER14_UP}

{$IFDEF COMPILER16_UP}
  {$DEFINE USE_64BIT_TYPES}
{$ENDIF COMPILER16_UP}

{$IFDEF COMPILER17_UP}
  {$DEFINE SUPPORTS_LEGACYIFEND}
{$ENDIF COMPILER17_UP}

{$IFDEF RTL130_UP}
  {$DEFINE HAS_UNIT_CONTNRS}
{$ENDIF RTL130_UP}

{$IFDEF RTL140_UP}
  {$IFDEF LINUX}
    {$DEFINE HAS_UNIT_LIBC}
  {$ENDIF LINUX}
  {$DEFINE HAS_UNIT_RTLCONSTS}
  {$DEFINE HAS_UNIT_TYPES}
  {$DEFINE HAS_UNIT_VARIANTS}
  {$DEFINE HAS_UNIT_STRUTILS}
  {$DEFINE HAS_UNIT_DATEUTILS}
  {$DEFINE XPLATFORM_RTL}
{$ENDIF RTL140_UP}

{$IFDEF RTL150_UP}
  {$DEFINE HAS_UNIT_UXTHEME}
{$ENDIF RTL150_UP}

{$IFDEF RTL170_UP}
  {$DEFINE HAS_UNIT_HTTPPROD}
{$ENDIF RTL170_UP}

{$IFDEF RTL185_UP}
  {$DEFINE HAS_UNIT_GIFIMG}
{$ENDIF RTL185_UP}

{$IFDEF RTL200_UP}
  {$DEFINE HAS_UNIT_ANSISTRINGS}
  {$DEFINE HAS_UNIT_PNGIMAGE}
  {$DEFINE HAS_UNIT_CHARACTER}
  {$DEFINE HAS_EXCEPTION_STACKTRACE}
{$ENDIF RTL200_UP}

{$IFDEF RTL210_UP}
  {$DEFINE HAS_EARGUMENTEXCEPTION}
{$ENDIF RTL210_UP} 

{$IFDEF RTL220_UP}
  {$DEFINE HAS_UNIT_REGULAREXPRESSIONSAPI}
  {$DEFINE HAS_ENOTIMPLEMENTED}
{$ENDIF RTL220_UP}

{$IFDEF RTL230_UP}
  {$DEFINE HAS_UNITSCOPE}
  {$DEFINE HAS_UNIT_SYSTEM_UITYPES}
  {$DEFINE HAS_UNIT_VCL_THEMES}
{$ENDIF RTL230_UP}

{$IFDEF RTL240_UP}
  {$DEFINE HAS_UNIT_SYSTEM_ACTIONS}
  {$DEFINE HAS_PROPERTY_STYLEELEMENTS}
{$ENDIF RTL240_UP}

{$IFDEF RTL250_UP}
  {$DEFINE DEPRECATED_SYSUTILS_ANSISTRINGS}
  {$DEFINE DEPRECATED_TCHARACTER}
{$ENDIF RTL250_UP}

{$IFDEF RTL270_UP}
  {$DEFINE HAS_AUTOMATIC_DB_FIELDS}
{$ENDIF RTL270_UP}

{------------------------------------------------------------------------------}
{ Cross-platform related defines                                               }
{------------------------------------------------------------------------------}

{$IFNDEF CPUASM}
  {$DEFINE PUREPASCAL}
{$ENDIF ~CPUASM}

{$IFDEF WIN32}
  {$DEFINE MSWINDOWS} // predefined for D6+/BCB6+
  {$DEFINE Win32API}
{$ENDIF}

{$IFDEF DELPHILANGUAGE}
  {$IFDEF LINUX}
    {$DEFINE UNIX}
  {$ENDIF}

  {$IFNDEF CONSOLE}
    {$IFDEF LINUX}
      {$DEFINE VisualCLX}
    {$ENDIF}
    {$IFNDEF VisualCLX}
      {$DEFINE VCL}
    {$ENDIF}
  {$ENDIF ~CONSOLE}
{$ENDIF DELPHILANGUAGE}

{------------------------------------------------------------------------------}
{ Compiler settings                                                            }
{------------------------------------------------------------------------------}

{$IFOPT A+} {$DEFINE ALIGN_ON} {$ENDIF}
{$IFOPT B+} {$DEFINE BOOLEVAL_ON} {$ENDIF}
{$IFDEF COMPILER2_UP}
  {$IFOPT C+} {$DEFINE ASSERTIONS_ON} {$ENDIF}
{$ENDIF}
{$IFOPT D+} {$DEFINE DEBUGINFO_ON} {$ENDIF}
{$IFOPT G+} {$DEFINE IMPORTEDDATA_ON} {$ENDIF}
{$IFDEF COMPILER2_UP}
  {$IFOPT H+} {$DEFINE LONGSTRINGS_ON} {$ENDIF}
{$ENDIF}

// Hints
{$IFOPT I+} {$DEFINE IOCHECKS_ON} {$ENDIF}
{$IFDEF COMPILER2_UP}
  {$IFOPT J+} {$DEFINE WRITEABLECONST_ON} {$ENDIF}
{$ENDIF}
{$IFOPT L+} {$DEFINE LOCALSYMBOLS} {$DEFINE LOCALSYMBOLS_ON} {$ENDIF}
{$IFOPT M+} {$DEFINE TYPEINFO_ON} {$ENDIF}
{$IFOPT O+} {$DEFINE OPTIMIZATION_ON} {$ENDIF}
{$IFOPT P+} {$DEFINE OPENSTRINGS_ON} {$ENDIF}
{$IFOPT Q+} {$DEFINE OVERFLOWCHECKS_ON} {$ENDIF}
{$IFOPT R+} {$DEFINE RANGECHECKS_ON} {$ENDIF}

// Real compatibility
{$IFOPT T+} {$DEFINE TYPEDADDRESS_ON} {$ENDIF}
{$IFOPT U+} {$DEFINE SAFEDIVIDE_ON} {$ENDIF}
{$IFOPT V+} {$DEFINE VARSTRINGCHECKS_ON} {$ENDIF}
{$IFOPT W+} {$DEFINE STACKFRAMES_ON} {$ENDIF}

// Warnings
{$IFOPT X+} {$DEFINE EXTENDEDSYNTAX_ON} {$ENDIF}

// for Delphi/BCB trial versions remove the point from the line below
{.$UNDEF SUPPORTS_WEAKPACKAGEUNIT}

{$ENDIF ~JEDI_INC}
