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
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This include file defines various JCL specific defines for C++Builder 6.                         }
{ The more generic JCL defines are defined in jcl.inc and the generic defines in the jedi.inc file }
{ which is shared with the JEDI VCL.                                                               }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This file is filled by the JCL installer, all the changes made in its content will be lost the   }
{ next time the JCL is installed.                                                                  }
{                                                                                                  }
{**************************************************************************************************}

// Math precision selection, mutually exclusive
{$DEFINE MATH_EXTENDED_PRECISION}
{.$DEFINE MATH_DOUBLE_PRECISION}
{.$DEFINE MATH_SINGLE_PRECISION}


// Math functions takes care of infinites and NaN
{$DEFINE MATH_EXT_EXTREMEVALUES}


// JclHookExcept support for hooking exceptions from DLLs
{.$DEFINE HOOK_DLL_EXCEPTIONS}


//Threadsafe directive (added for DCL support)
{$DEFINE THREADSAFE}


// To exclude obsolete code from compilation, remove the point from the line below
{$DEFINE DROP_OBSOLETE_CODE}


//Support for JclUnitVersioning.pas)
{$DEFINE UNITVERSIONING}

// debug sources
// defining these symbols will the debug source to be automatically registered
{.$DEFINE DEBUG_NO_BINARY}
{.$DEFINE DEBUG_NO_TD32}
{.$DEFINE DEBUG_NO_MAP}
{.$DEFINE DEBUG_NO_EXPORTS}
{.$DEFINE DEBUG_NO_SYMBOLS}

// mark EDI units as weak package units (to avoid conflicts with the EDI package)
{.$DEFINE EDI_WEAK_PACKAGE_UNITS}
