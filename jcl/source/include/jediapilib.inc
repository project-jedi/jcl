{$IFNDEF JEDIAPILIB_INC}
{$DEFINE JEDIAPILIB_INC}

{******************************************************************************}
{                                                                              }
{ Windows Version Unit for Object Pascal                                       }
{                                                                              }
{ Portions created by Robert Marquardt are Copyright (C) 2004                  }
{ Robert Marquardt. All Rights Reserved.                                       }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project JEDI         }
{ APILIB home page, located at http://jedi-apilib.sourceforge.net              }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting the provisions above and   }
{ replace them with the notice and other provisions required by the LGPL       }
{ License. If you do not delete the provisions above, a recipient may use      }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{******************************************************************************}
{$A+,Z4}

// This file is intended for C header conversions.
// It defines several mutually exclusive IFDEFs which determine
// the Windows version the API conversion is usable with.

{$I jedi.inc}

{$IFNDEF JEDI_INC}
ALERT_jedi_inc_incompatible
// secure against old versions of jedi.inc.
{$ENDIF !JEDI_INC}

// disable items set in PACKAGE_CONDITIONS
{$DEFINE DYNAMIC_LINK}
{$UNDEF JWA_INCLUDE_SETUP_API}
{$UNDEF JWA_WINSOCK_1}
{$UNDEF NOVCL}
{$UNDEF JWA_INCLUDE_JWAADSTLB}
{$UNDEF JWA_INCLUDE_SHELLAPI}
{$UNDEF WINVISTA}

{$IFNDEF APILIBINSTALL}
  {----------------------------}
  { C++Builder                 }
  {----------------------------}
  {$IFDEF BCB5}
    {$I jediapilibc5.inc}
    {$DEFINE APILIB_CONFIGURED}
  {$ENDIF BCB5}
  {----------------------------}
  {$IFDEF BCB6}
    {$I jediapilibc6.inc}
    {$DEFINE APILIB_CONFIGURED}
  {$ENDIF BCB6}
  {----------------------------}
  { Delphi                     }
  {----------------------------}
  {$IFDEF DELPHI5}
    {$I jediapilibd5.inc}
    {$DEFINE APILIB_CONFIGURED}
  {$ENDIF DELPIH5}
  {----------------------------}
  {$IFDEF DELPHI6}
    {$I jediapilibd6.inc}
    {$DEFINE APILIB_CONFIGURED}
  {$ENDIF DELPIH6}
  {----------------------------}
  {$IFDEF DELPHI7}
    {$I jediapilibd7.inc}
    {$DEFINE APILIB_CONFIGURED}
  {$ENDIF DELPIH7}  
  {----------------------------}
  { BDS                        }
  {----------------------------}
  // BDS 1 and BDS 2 have the same version numbers for their native compilers
  // no compiler defines are used for BDS 1 and BDS 2
  {$IFDEF BDS1}
    //{$I jediapilibcs1.inc}
    {$DEFINE APILIB_CONFIGURED}
  {$ENDIF BDS1}
  {----------------------------}
  {$IFDEF BDS2}
    //{$I jediapilibd8.inc}
    {$DEFINE APILIB_CONFIGURED}
  {$ENDIF BDS2}
  {----------------------------}
  {$IFDEF BDS3}
    {$I jediapilibd9.inc}
    {$DEFINE APILIB_CONFIGURED}
  {$ENDIF BDS3}
  {----------------------------}
  {$IFDEF BDS4}
    {$I jediapilibd10.inc}
    {$DEFINE APILIB_CONFIGURED}
  {$ENDIF BDS4}
  {----------------------------}
  {$IFDEF BDS5}
    {$I jediapilibd11.inc}
    {$DEFINE APILIB_CONFIGURED}
  {$ENDIF BDS5}
  {----------------------------}
  {$IFDEF FPC}
    {$I jediapilibfpc.inc}
    {$DEFINE APILIB_CONFIGURED}
  {$ENDIF FPC}
  {----------------------------}

  // check configuration
  {$IFNDEF APILIB_CONFIGURED}
    {$IFDEF SUPPORTS_COMPILETIME_MESSAGES}
      {$MESSAGE FATAL 'Your Delphi/BCB version is not supported by this Jedi API Library version!'}
    {$ELSE}
      'Your Delphi/BCB version is not supported by this Jedi API Library version!'
    {$ENDIF SUPPORTS_COMPILETIME_MESSAGES}
  {$ENDIF !APILIB_CONFIGURED}

{$ENDIF ~APILIBINSTALL}

{WARNING:
Do not change state of
JWA_INCLUDEMODE
JWA_INTERFACESECTION
JWA_IMPLEMENTATIONSECTION
Its only set in JwaWindows.pas.
Standalone compiling would be useless!
}

// disable defines in JwaWindows.pas
{$DEFINE JWA_INCLUDE_JWAADSTLB}

// This global switch activates the using of long function variables
// for dynamic linking
// in old Delphi versions long var names are shorten and no more recognized.
{$UNDEF SUPPORT_LONG_VARNAMES}
{$IFDEF DELPHI6_UP}
  {$DEFINE SUPPORT_LONG_VARNAMES}
{$ENDIF DELPHI6_UP}

{$IFDEF FPC}
  {$DEFINE SUPPORT_LONG_VARNAMES}
{$ENDIF FPC}

// Default is WINXP
{$IFNDEF WIN95}
  {$IFNDEF WIN98}
    {$IFNDEF WIN98SE}
      {$IFNDEF WIN98ME}
        {$IFNDEF WINNT4}
          {$IFNDEF WIN2000}
            {$IFNDEF WINXP}
              {$IFNDEF WIN2003}
                {$IFNDEF WINVISTA}
                  {$IFNDEF WIN2008}
                    {$DEFINE WINXP}
                  {$ENDIF ~WIN2008}
                {$ENDIF ~WINVISTA}
              {$ENDIF ~WIN2003}
            {$ENDIF ~WINXP}
          {$ENDIF ~WIN2000}
        {$ENDIF ~WINNT4}
      {$ENDIF ~WIN98ME}
    {$ENDIF ~WIN98SE}
  {$ENDIF ~WIN98}
{$ENDIF ~WIN95}

// secondary IFDEFs for "_UP" which means also any later OS version

{$IFDEF WIN2008}
 {$DEFINE WIN95_UP}
 {$DEFINE WIN98_UP}
 {$DEFINE WIN98SE_UP}
 {$DEFINE WIN98ME_UP}
 {$DEFINE WINNT4_UP}
 {$DEFINE WIN2000_UP}
 {$DEFINE WINXP_UP}
 {$DEFINE WIN2003_UP}
 {$DEFINE WINVISTA_UP}
 {$DEFINE WIN2008_UP}
{$ENDIF WIN2008}

{$IFDEF WINVISTA}
 {$DEFINE WIN95_UP}
 {$DEFINE WIN98_UP}
 {$DEFINE WIN98SE_UP}
 {$DEFINE WIN98ME_UP}
 {$DEFINE WINNT4_UP}
 {$DEFINE WIN2000_UP}
 {$DEFINE WINXP_UP}
 {$DEFINE WIN2003_UP}
 {$DEFINE WINVISTA_UP}
{$ENDIF WINVISTA}

{$IFDEF WIN2003}
 {$DEFINE WIN95_UP}
 {$DEFINE WIN98_UP}
 {$DEFINE WIN98SE_UP}
 {$DEFINE WIN98ME_UP}
 {$DEFINE WINNT4_UP}
 {$DEFINE WIN2000_UP}
 {$DEFINE WINXP_UP}
 {$DEFINE WIN2003_UP}
{$ENDIF WIN2003}

{$IFDEF WINXP}
 {$DEFINE WIN95_UP}
 {$DEFINE WIN98_UP}
 {$DEFINE WIN98SE_UP}
 {$DEFINE WIN98ME_UP}
 {$DEFINE WINNT4_UP}
 {$DEFINE WIN2000_UP}
 {$DEFINE WINXP_UP}
{$ENDIF WINXP}

{$IFDEF WIN2000}
 {$DEFINE WIN95_UP}
 {$DEFINE WIN98_UP}
 {$DEFINE WIN98SE_UP}
 {$DEFINE WIN98ME_UP}
 {$DEFINE WINNT4_UP}
 {$DEFINE WIN2000_UP}
{$ENDIF WIN2000}

{$IFDEF WINNT4}
 {$DEFINE WIN95_UP}
 {$DEFINE WIN98_UP}
 {$DEFINE WIN98SE_UP}
 {$DEFINE WIN98ME_UP}
 {$DEFINE WINNT4_UP}
{$ENDIF WINNT4}

{$IFDEF WIN98ME}
 {$DEFINE WIN95_UP}
 {$DEFINE WIN98_UP}
 {$DEFINE WIN98SE_UP}
 {$DEFINE WIN98ME_UP}
{$ENDIF WIN98ME}

{$IFDEF WIN98SE}
 {$DEFINE WIN95_UP}
 {$DEFINE WIN98_UP}
 {$DEFINE WIN98SE_UP}
{$ENDIF WIN98SE}

{$IFDEF WIN98}
 {$DEFINE WIN95_UP}
 {$DEFINE WIN98_UP}
{$ENDIF WIN98}

{$IFDEF WIN95}
 {$DEFINE WIN95_UP}
{$ENDIF WIN95}

{$IFDEF SERVICEPACK_2}
 {$DEFINE SERVICEPACK_1_UP}
{$ENDIF SERVICEPACK_2}

{$IFDEF SERVICEPACK_3}
 {$DEFINE SERVICEPACK_1_UP}
 {$DEFINE SERVICEPACK_2_UP}
{$ENDIF SERVICEPACK_3}

{$IFDEF SERVICEPACK_4}
 {$DEFINE SERVICEPACK_1_UP}
 {$DEFINE SERVICEPACK_2_UP}
 {$DEFINE SERVICEPACK_3_UP}
{$ENDIF SERVICEPACK_4}

// Default is IE600
{$IFNDEF IE300}
  {$IFNDEF IE400}
    {$IFNDEF IE401}
      {$IFNDEF IE500}
        {$IFNDEF IE501}
          {$IFNDEF IE560}
            {$IFNDEF IE600}
              {$DEFINE IE600}
            {$ENDIF ~IE600}
          {$ENDIF ~IE560}
        {$ENDIF ~IE501}
      {$ENDIF ~IE500}
    {$ENDIF ~IE401}
  {$ENDIF ~IE400}
{$ENDIF ~IE300}

// secondary IFDEFs for "_UP" which means also any later IE version
{$IFDEF IE600}
 {$DEFINE IE300_UP}
 {$DEFINE IE400_UP}
 {$DEFINE IE401_UP}
 {$DEFINE IE500_UP}
 {$DEFINE IE501_UP}
 {$DEFINE IE560_UP}
 {$DEFINE IE600_UP}
{$ENDIF IE600}

{$IFDEF IE560}
 {$DEFINE IE300_UP}
 {$DEFINE IE400_UP}
 {$DEFINE IE401_UP}
 {$DEFINE IE500_UP}
 {$DEFINE IE501_UP}
 {$DEFINE IE560_UP}
{$ENDIF IE560}

{$IFDEF IE501}
 {$DEFINE IE300_UP}
 {$DEFINE IE400_UP}
 {$DEFINE IE401_UP}
 {$DEFINE IE500_UP}
 {$DEFINE IE501_UP}
{$ENDIF IE501}

{$IFDEF IE500}
 {$DEFINE IE300_UP}
 {$DEFINE IE400_UP}
 {$DEFINE IE401_UP}
 {$DEFINE IE500_UP}
{$ENDIF IE500}

{$IFDEF IE401}
 {$DEFINE IE300_UP}
 {$DEFINE IE400_UP}
 {$DEFINE IE401_UP}
{$ENDIF IE401}

{$IFDEF IE400}
 {$DEFINE IE300_UP}
 {$DEFINE IE400_UP}
{$ENDIF IE400}

{$IFDEF IE300}
 {$DEFINE IE300_UP}
{$ENDIF IE300}

// if none is set a default is determined from the Windows version IFDEFs
// IFDEF defaults for MSI (Microsoft Installer)
{$IFNDEF MSI200}
  {$IFNDEF MSI110}
    {$IFNDEF MSI100}
      {$IFDEF WINXP_UP}
        {$DEFINE MSI200}
      {$ELSE}
        {$IFDEF WIN2000_UP}
          {$DEFINE MSI110}
        {$ELSE}
          {$DEFINE MSI100}
        {$ENDIF WIN2000_UP}
      {$ENDIF WINXP_UP}
    {$ENDIF !MSI100}
  {$ENDIF !MSI110}
{$ENDIF !MSI200}

// secondary IFDEFs for MSI (Microsoft Installer)
{$IFDEF MSI200}
 {$DEFINE MSI200_UP}
 {$DEFINE MSI110_UP}
 {$DEFINE MSI100_UP}
{$ENDIF MSI200}

{$IFDEF MSI110}
 {$DEFINE MSI110_UP}
 {$DEFINE MSI100_UP}
{$ENDIF MSI110}

{$IFDEF MSI100}
 {$DEFINE MSI100_UP}
{$ENDIF MSI100}

// default is HTMLHELP12
{$IFNDEF HTMLHELP11}
  {$IFNDEF HTMLHELP12}
    {$DEFINE HTMLHELP12}
  {$ENDIF ~HTMLHELP12}
{$ENDIF ~HTMLHELP11}

// secondary IFDEFs for HtmlHelp
{$IFDEF HTMLHELP12}
 {$DEFINE HTMLHELP12_UP}
 {$DEFINE HTMLHELP11_UP}
{$ENDIF HTMLHELP12}

{$IFDEF HTMLHELP11}
 {$DEFINE HTMLHELP11_UP}
{$ENDIF HTMLHELP11}

{$ENDIF ~JEDIAPILIB_INC}