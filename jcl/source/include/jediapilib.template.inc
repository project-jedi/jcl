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

// This file is intended for C header conversions.
// It defines several mutually exclusive IFDEFs which determine
// the Windows version the API conversion is usable with.

{WARNING:
Do not change state of
JWA_INCLUDEMODE
JWA_INTERFACESECTION
JWA_IMPLEMENTATIONSECTION
Its only set in JwaWindows.pas.
Standalone compiling would be useless!
}

// Global switch to enable dynamic linking of some APIs
{.$DEFINE DYNAMIC_LINK}

// Global switch to make UNICODE versions of API functions default
{.$DEFINE UNICODE}

// Global switch to activate the use of the original basic types of
// Delphi Windows.pas. Either indirectly in JwaWinType.pas or
// directly through the use of Windows.pas instead of JwaWinType.pas.
{$DEFINE USE_DELPHI_TYPES}

//activate 64bit tweaks for some functions
//jwaWinBase::InterlockedCompareExchangePointer
//jwaWinBase::InterlockedExchangePointer
{.$DEFINE CPU64BIT}

// Global switch for the Windows version the files are compatible with
// ONLY A SINGLE ONE IS ALLOWED TO BE ACTIVATED BY CHANGING THE DOT TO A DOLLAR SIGN!
// Default is WINXP
{.$DEFINE WIN95}
{.$DEFINE WIN98}
{.$DEFINE WIN98SE}
{.$DEFINE WIN98ME}
{.$DEFINE WINNT4}
{.$DEFINE WIN2000}
{.$DEFINE WINXP}
{.$DEFINE WIN2003}
{.$DEFINE WINVISTA}
{.$DEFINE WIN2008}

{.$DEFINE SERVICEPACK_1}
{.$DEFINE SERVICEPACK_2}
{.$DEFINE SERVICEPACK_3}
{.$DEFINE SERVICEPACK_4}

// Global switch for the Windows Internet Explorer versions
// ONLY A SINGLE ONE IS ALLOWED TO BE ACTIVATED BY CHANGING THE DOT TO A DOLLAR SIGN!
// Default is IE600
{.$DEFINE IE300}
{.$DEFINE IE400}
{.$DEFINE IE401}
{.$DEFINE IE500}
{.$DEFINE IE501}
{.$DEFINE IE560}
{.$DEFINE IE600}

// Global switch for the MSI (Microsoft Installer) versions
// ONLY A SINGLE ONE IS ALLOWED TO BE ACTIVATED BY CHANGING THE DOT TO A DOLLAR SIGN!
// if none is set a default is determined from the Windows version IFDEFs
{.$DEFINE MSI100}
{.$DEFINE MSI110}
{.$DEFINE MSI200}

// Global switch for the HtmlHelp versions
// ONLY A SINGLE ONE IS ALLOWED TO BE ACTIVATED BY CHANGING THE DOT TO A DOLLAR SIGN!
// default is HTMLHELP12
{.$DEFINE HTMLHELP11}
{.$DEFINE HTMLHELP12}
