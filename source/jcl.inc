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
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This include file defines various JCL specific defines. The more generic defines are defined in  }
{ the jedi.inc file which is shared with the JEDI VCL.                                             }
{                                                                                                  }
{ Unit owner: Marcel van Brakel                                                                    }
{ Last modified: July 5, 2002                                                                      }
{                                                                                                  }
{**************************************************************************************************}

{$B-} // Boolean shortcut evaluation
{$H+} // Long strings
{$J-} // Read-only typed constants
{$T-} // Type checked pointers off

{$INCLUDE jedi.inc} // Pull in the JCL/J-VCL shared directives

// Math precision selection, mutually exclusive

{$DEFINE MATH_EXTENDED_PRECISION}
{.$DEFINE MATH_DOUBLE_PRECISION}
{.$DEFINE MATH_SINGLE_PRECISION}

{$IFDEF MATH_DOUBLE_PRECISION}
  {$UNDEF MATH_EXTENDED_PRECISION}
{$ENDIF}
{$IFDEF MATH_SINGLE_PRECISION}
  {$UNDEF MATH_EXTENDED_PRECISION}
{$ENDIF}

{.$DEFINE MATH_EXT_EXTREMEVALUES}

// JclHookExcept support for hooking exceptions from DLLs

{.$DEFINE HOOK_DLL_EXCEPTIONS}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}  // Platform warnings off
{$ENDIF}

{$IFDEF DELPHI7_UP}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

