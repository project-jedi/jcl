{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: LINUXONLY.INC, released on 2004-01-04

Last Modified: 2004-05-07

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

{$IFNDEF LINUX}
{$IFDEF COMPILER6_UP}
{$MESSAGE FATAL 'This unit is only supported on Linux!'}
{$ELSE}
This unit is only supported on Linux!
{$ENDIF COMPILER6_UP}
{$ENDIF LINUX}