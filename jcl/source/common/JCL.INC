{******************************************************************************}
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.0 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JCL.INC.                                                }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{ Last modified: May 19 2000                                                   }
{                                                                              }
{******************************************************************************}

{$B-} // Boolean shortcut evaluation
{$H+} // Long strings
{$J-} // Read-only typed constants

{$INCLUDE JEDI.INC} // Pull in the JCL/J-VCL shared directives

// Math precision selection, mutually exclusive

{$DEFINE MATH_EXTENDED_PRECISION}
{.$DEFINE MATH_DOUBLE_PRECISION}
{.$DEFINE MATH_SINGLE_PRECISION}

// Math angle selection, mutually exclusive

{.$DEFINE MATH_ANGLES_DEGREES}
{.$DEFINE MATH_ANGLES_GRADS}
{$DEFINE MATH_ANGLES_RADIANS}

// Free Pascal specific

{$DEFINE UseRegAsm}
{$DEFINE UseAsm}

{$IFDEF FPC}
  {$DEFINE SUPPORTS_INT64}
  {$DEFINE SUPPORTS_OVERLOAD}
  {$ASMMODE Intel}
  {$UNDEF UseRegAsm}
{$ENDIF}
