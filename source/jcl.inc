{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is jcl.inc                                                                     }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright (C) Marcel van Brakel.                       }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Marcel van Brakel                                                                              }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones                                                                                     }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This include file defines various JCL specific defines. The more generic defines are defined in  }
{ the jedi.inc file which is shared with the JEDI VCL.                                             }
{                                                                                                  }
{**************************************************************************************************}

{$B-} // Boolean shortcut evaluation
{$H+} // Long strings
{$J-} // Read-only typed constants
{$T-} // Type checked pointers off

{$I jedi.inc} // Pull in the JCL/J-VCL shared directives

{$IFNDEF JEDI_INC}
ALERT_jedi_inc_incompatible
// secure against old versions of jedi.inc.
{$ENDIF ~JEDI_INC}

// Math precision selection, mutually exclusive

{$DEFINE MATH_EXTENDED_PRECISION}
{.$DEFINE MATH_DOUBLE_PRECISION}
{.$DEFINE MATH_SINGLE_PRECISION}

{$IFDEF MATH_DOUBLE_PRECISION}
  {$UNDEF MATH_SINGLE_PRECISION}
  {$UNDEF MATH_EXTENDED_PRECISION}
{$ENDIF}
{$IFDEF MATH_SINGLE_PRECISION}
  {$UNDEF MATH_DOUBLE_PRECISION}
  {$UNDEF MATH_EXTENDED_PRECISION}
{$ENDIF}

{.$DEFINE MATH_EXT_EXTREMEVALUES}

// JclHookExcept support for hooking exceptions from DLLs

{.$DEFINE HOOK_DLL_EXCEPTIONS}

{$IFDEF SUPPORTS_UNSAFE_WARNINGS}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}


//Threadsafe directive (added for DCL support)
{.DEFINE THREADSAFE}

// To exclude obsolete code from compilation, remove the point from the line below
{.$DEFINE DROP_OBSOLETE_CODE}

{$IFNDEF DROP_OBSOLETE_CODE}
  {$DEFINE KEEP_DEPRECATED}
{$ENDIF}

//Support for JclUnitVersioning.pas)
{.$DEFINE UNITVERSIONING}


// debug sources

// defining these symbols will the debug source to be automatically registered
{.$DEFINE DEBUG_NO_BINARY}
{.$DEFINE DEBUG_NO_TD32}
{.$DEFINE DEBUG_NO_MAP}
{.$DEFINE DEBUG_NO_EXPORTS}
{.$DEFINE DEBUG_NO_SYMBOLS}

{$IFDEF CLR}
 {$WARN UNSAFE_TYPE ON}
 {$WARN UNSAFE_CODE ON}
 {$WARN UNSAFE_CAST ON}
 {$WARN UNIT_PLATFORM OFF}

 {$DEFINE MSWINDOWS}
 {$DEFINE PIC}
 {$DEFINE PUREPASCAL}
{$ENDIF CLR}

