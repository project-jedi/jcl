{$IFNDEF WINDOWSONLY_INC}
{$DEFINE WINDOWSONLY_INC}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: windowsonly.inc, released on 2002-07-04.

You may retrieve the latest version of this file at the JCL home page,
located at http://homepages.borland.com/jedi/jcl/

Known Issues:
-----------------------------------------------------------------------------}

// Last modified: $Date$
// For history see end of file

{$IFNDEF JEDI_INC}
ALERT_jedi_inc_missing
// This inc file depends on jedi.inc which has to
// be included first (usually indirectly through
// the inclusion of jcl.inc).
{$ENDIF ~JEDI_INC}

// Suppress platform warnings which are irrelevant
// because the including unit can only be compiled
// for the Windows platform anyway.

{$IFDEF SUPPORTS_PLATFORM_WARNINGS}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF SUPPORTS_PLATFORM_WARNINGS}

// Cause a compilation error for any platform except Windows.

{$IFNDEF MSWINDOWS}
  {$IFDEF SUPPORTS_COMPILETIME_MESSAGES}
    {$MESSAGE FATAL 'This unit is only supported on Windows!'}
  {$ELSE}
    'This unit is only supported on Windows!'
  {$ENDIF SUPPORTS_COMPILETIME_MESSAGES}
{$ENDIF ~MSWINDOWS}

// History:

// $Log$
// Revision 1.6  2004/08/12 17:22:30  marquardt
// removed XPLATFORM_RTL
//
// Revision 1.5  2004/08/10 00:52:06  rrossmair
// don't allow missing jedi.inc/wrong inclusion order to happen unnoticed.
//
// Revision 1.4  2004/07/30 07:16:47  marquardt
// added a tilde
//
// Revision 1.3  2004/07/29 07:58:21  marquardt
// inc files updated
//
// Revision 1.2  2004/06/21 01:17:51  rrossmair
// - $IFDEFed contents (to prevent from repeated inclusion)
// - use of feature friendly symbol names
// - header text adapted for JCL
//

{$ENDIF ~WINDOWSONLY_INC}

