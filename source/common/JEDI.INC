{******************************************************************************}
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JEDI.INC.                                               }
{                                                                              }
{ The Initial Developer of the Original Code is Project JEDI                   }
{ http://www.delphi-jedi.org                                                   }
{                                                                              }
{ Last modified: October 27 2000                                               }
{                                                                              }
{******************************************************************************}

{ This file defines various generic compiler directives used in the JEDI Code
  Library (JCL) and JEDI Visual Component Library Library (J-VCL). The directives
  in this file are of generic nature and consist mostly of mappings from the
  VERXXX directives defined by Delphi and C++ Builder to friendly names such as
  DELPHI5 and SUPPORTS_WIDESTRING. These friendly names or subsequently used in
  both libraries to test for compiler versions and/or whether the compiler
  supports certain features (such as widestring's or 64 bit integers. Both
  libraries provide an additional, library specific, include file. For the JCL
  this is JCL.INC. These files should be included in source files instead of
  this file, which is included through the library specific file.

- Development environment directives

  This file defines two driectives to indicate which development environment the
  library is being compiled with. Currently this can either be Delphi or
  C++ Builder (in the near future "Kylix" will be added).

  Directive    Description
  ------------------------------------------------------------------------------
  DELPHI       Defined if compiled with Delphi
  BCB          Defined if compiled with C++ Builder

- Platform Directives

  Platform directives are not explicitly defined in this file but are defined
  by the compiler itself. They are listed here only for completeness.

  Directive    Description
  ------------------------------------------------------------------------------
  WIN32        Defined when target platform is 32 bit Windows
  LINUX        Defined when target platform is Linux

- Delphi Versions

  The following directives are direct mappings from the VERXXX directives to a
  friendly name of the associated compiler. These directives are only defined if
  the compiler is Delphi (ie DELPHI is defined).

  Directive    Description
  ------------------------------------------------------------------------------
  DELPHI1      Defined when compiling with Delphi 1
  DELPHI2      Defined when compiling with Delphi 2
  DELPHI3      Defined when compiling with Delphi 3
  DELPHI4      Defined when compiling with Delphi 4
  DELPHI5      Defined when compiling with Delphi 5
  DELPHI6      Defined when compiling with Delphi 6
  DELPHI1_UP   Defined when compiling with Delphi 1 or higher
  DELPHI2_UP   Defined when compiling with Delphi 2 or higher
  DELPHI3_UP   Defined when compiling with Delphi 3 or higher
  DELPHI4_UP   Defined when compiling with Delphi 4 or higher
  DELPHI5_UP   Defined when compiling with Delphi 5 or higher
  DELPHI6_UP   Defined when compiling with Delphi 6 or higher

- C++ Builder Versions

  The following directives are direct mappings from the VERXXX directives to a
  friendly name of the associated compiler. These directives are only defined if
  the compiler is C++ Builder (ie BCB is defined).

  Directive    Description
  ------------------------------------------------------------------------------
  BCB1         Defined when compiling with C++ Builder 1
  BCB3         Defined when compiling with C++ Builder 3
  BCB4         Defined when compiling with C++ Builder 4
  BCB5         Defined when compiling with C++ Builder 5
  BCB6         Defined when compiling with C++ Builder 6
  BCB1_UP      Defined when compiling with C++ Builder 1 or higher
  BCB3_UP      Defined when compiling with C++ Builder 3 or higher
  BCB4_UP      Defined when compiling with C++ Builder 4 or higher
  BCB5_UP      Defined when compiling with C++ Builder 5 or higher
  BCB6_UP      Defined when compiling with C++ Builder 6 or higher

- Compiler Versions

  The following directives are direct mappings from the VERXXX directives to a
  friendly name of the associated compiler. Unlike the DELPHI_X and BCB_X
  directives, these directives are indepedent of the development environment.
  That is, they are defined regardless of whether compilation takes place using
  Delphi or C++ Builder.

  Directive    Description
  ------------------------------------------------------------------------------
  COMPILER1    Defined when compiling with Delphi 1
  COMPILER2    Defined when compiling with Delphi 2 or C++ Builder 1
  COMPILER3    Defined when compiling with Delphi 3
  COMPILER35   Defined when compiling with C++ Builder 3
  COMPILER4    Defined when compiling with Delphi 4 or C++ Builder 4
  COMPILER5    Defined when compiling with Delphi 5 or C++ Builder 5
  COMPILER6    Defined when compiling with Delphi 6 or C++ Builder 6
  COMPILER1_UP Defined when compiling with Delphi 1 or higher
  COMPILER2_UP Defined when compiling with Delphi 2 or C++ Builder 1 or higher
  COMPILER3_UP Defined when compiling with Delphi 3 or higher
  COMPILER35UP Defined when compiling with C++ Builder 3 or higher
  COMPILER4_UP Defined when compiling with Delphi 4 or C++ Builder 4 or higher
  COMPILER5_UP Defined when compiling with Delphi 5 or C++ Builder 5 or higher
  COMPILER6_UP Defined when compiling with Delphi 6 or C++ Builder 6 or higher

- Feature Directives

  The features directives are used to test if the compiler supports specific
  features, such as method overloading, and adjust the sources accordingly. Use
  of these directives is preferred over the use of the DELPHI and COMPILER
  directives.

  Directive              Description
  ------------------------------------------------------------------------------
  SUPPORTS_WIDESTRING    Compiler supports the WideString type (D3/BCB3 up)
  SUPPORTS_INTERFACE     Compiler supports interfaces (D3/BCB3)
  SUPPORTS_EXTSYM        Compiler supports the $EXTERNALSYM directive (D4/BCB3)
  SUPPORTS_NODEFINE      Compiler supports the $NODEFINE directive (D4/BCB3)
  SUPPORTS_INT64         Compiler supports the Int64 type (D4/BCB4)
  SUPPORTS_DYNAMICARRAYS Compiler supports dynamic arrays (D4/BCB4)
  SUPPORTS_DEFAULTPARAMS Compiler supports default parameters (D4/BCB4)
  SUPPORTS_OVERLOAD      Compiler supports overloading (D4/BCB4)
}

// VERXXX to COMPILERX, DELPHIX and BCBX mappings

{$IFDEF VER140}
  {$DEFINE COMPILER6}
  {$IFDEF BCB}
    {$DEFINE BCB6}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE COMPILER5}
  {$IFDEF BCB}
    {$DEFINE BCB5}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE COMPILER4}
  {$DEFINE BCB4}
  {$DEFINE BCB}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE COMPILER4}
  {$DEFINE DELPHI4}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE COMPILER35}
  {$DEFINE BCB3}
{$ENDIF}

{$IFDEF VER100}
  {$DEFINE COMPILER3}
  {$DEFINE DELPHI3}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER93}
  {$DEFINE COMPILER2}
  {$DEFINE BCB1}
  {$DEFINE BCB}
{$ENDIF}

{$IFDEF VER90}
  {$DEFINE COMPILER2}
  {$DEFINE DELPHI2}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER80}
  {$DEFINE COMPILER1}
  {$DEFINE DELPHI1}
  {$DEFINE DELPHI}
{$ENDIF}

// DELPHIX_UP from DELPHIX mappings

{$IFDEF DELPHI6}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI5}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI4}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI3}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI2}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI1}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

// BCBX_UP from BCBX mappings

{$IFDEF BCB6}
  {$DEFINE BCB6_UP}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB5}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB4}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB3}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB1}
  {$DEFINE BCB1_UP}
{$ENDIF}

// COMPILERX_UP from COMPILERX mappings

{$IFDEF COMPILER6}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER5}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER4}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER35}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER3}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER2}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER1}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

// Map COMPILERX_UP to friendly feature names

{$IFDEF COMPILER3_UP}
  {$DEFINE SUPPORTS_WIDESTRING}
  {$DEFINE SUPPORTS_INTERFACE}
{$ENDIF}

{$IFDEF COMPILER35_UP}
  {$DEFINE SUPPORTS_EXTSYM}
  {$DEFINE SUPPORTS_NODEFINE}
{$ENDIF}

{$IFDEF COMPILER4_UP}
  {$DEFINE SUPPORTS_INT64}
  {$DEFINE SUPPORTS_DYNAMICARRAYS}
  {$DEFINE SUPPORTS_DEFAULTPARAMS}
  {$DEFINE SUPPORTS_OVERLOAD}
{$ENDIF}
