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

// $Log$
// Revision 1.12  2005/03/14 08:46:54  rrossmair
// - check-in in preparation for release 1.95
//
// Revision 1.11  2005/02/26 16:42:08  marquardt
// deactivated THREADSAFE and fixed bugs stemming from that
//
// Revision 1.10  2005/01/05 17:36:34  dade2004
// Added THREADSAFE directive for DCL support
//
// Revision 1.10  2005/01/05 06:55:51  dade2004
// Added THREADSAFE directive
//
// $Log$
// Revision 1.12  2005/03/14 08:46:54  rrossmair
// - check-in in preparation for release 1.95
//
// Revision 1.11  2005/02/26 16:42:08  marquardt
// deactivated THREADSAFE and fixed bugs stemming from that
//
// Revision 1.10  2005/01/05 17:36:34  dade2004
// Added THREADSAFE directive for DCL support
//
// Revision 1.9  2004/10/17 06:55:51  rrossmair
// cleanup
//
// Revision 1.8  2004/10/02 05:47:28  marquardt
// added check for incompatible jedi.inc
// replaced jedi.inc with jvcl.inc
//
// Revision 1.7  2004/07/29 07:58:21  marquardt
// inc files updated
//
// Revision 1.6  2004/06/02 03:17:02  rrossmair
// added DROP_OBSOLETE_CODE comment
//
// Revision 1.5  2004/05/31 22:28:39  rrossmair
// header updated according to new policy: initial developers & contributors listed
//

